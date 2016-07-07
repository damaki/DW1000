-------------------------------------------------------------------------------
--  Copyright (c) 2016 Daniel King
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.
-------------------------------------------------------------------------------
with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with AUnit.Assertions;       use AUnit.Assertions;
with DW1000.Types;           use DW1000.Types;
with System.Multiprocessors; use System.Multiprocessors;

package body DW1000.System_Time.Tests
is

   --  The tests in this package perform exhaustive tests over the full 40-bit
   --  range in order to prove certain properties about the system time types.
   --
   --  In order to speed up the tests, the tests take advantage of
   --  multiprocessors to split the 2**40 test range into multiple sub-ranges,
   --  and each sub-range is delegated to a task. This provides a roughly
   --  linear reduction in execution time, based on the number of CPU threads.
   --
   --  However, even when taking advantage of multicore processors the tests
   --  take many hours to complete (about 10 hours per exhaustive test on my
   --  Intel Core i7-2630QM laptop).
   --
   --  The tests could be sped up even further by enabling compiler
   --  optimizations. However, when mixed with AUnit the optimization switches
   --  seem to cause the tests to crash at runtime with a Storage_Error fault,
   --  so optimizations should be disabled until a resolution to this problem
   --  is found.

   ----------------------------------------------------------------------------

   --  This protected type is used to synchronize with the tasks which are
   --  running the tests.
   --
   --  The environment thread calls Reset before starting its child tasks in
   --  order to reset the state between tests.
   --
   --  Each child task MUST call Notify when it has finished its portion of the
   --  test (successfully or not). When all N tasks have called notify the Wait
   --  entry will be unblocked.
   --
   --  The environment task waits for all child tasks to finish by calling the
   --  Wait entry. It will be unblocked once all child tasks have called
   --  Notify.
   --
   --  Each child task should periodically query the Test_Failed function to
   --  check if another child task has already encountered a test failure.
   --  If Test_Failed returns True then the child task should abort its tests
   --  ASAP (it must still call Notify, though). This permits the entire test
   --  to finish early when a failure is found.
   --
   --  The parameter N is the number of child tasks that are being used.
   --  It's recommended to set this to System.Multiprocessors.Number_Of_CPUs
   --  and take advantage of all available logical CPU cores.
   protected type Test_Result_PO_Type (N : CPU)
   is
      entry Wait (Test_Result  : out Boolean;
                  Fail_Message : out Unbounded_String);

      procedure Reset;

      procedure Notify (Test_Result  : in Boolean;
                        Fail_Message : in Unbounded_String);

      function Test_Failed return Boolean;

   private
      Count : CPU_Range := 0;

      Result  : Boolean := True;
      Message : Unbounded_String := Null_Unbounded_String;
   end Test_Result_PO_Type;


   protected body Test_Result_PO_Type
   is
      entry Wait (Test_Result  : out Boolean;
                  Fail_Message : out Unbounded_String)
        when Count = N
      is
      begin
         Test_Result  := Result;
         Fail_Message := Message;
      end Wait;

      procedure Reset
      is
      begin
         Count   := 0;
         Result  := True;
         Message := Null_Unbounded_String;
      end Reset;

      procedure Notify (Test_Result  : in Boolean;
                        Fail_Message : in Unbounded_String)
      is
      begin
         if Count < N then
            Count := Count + 1;
         end if;

         if Result and not Test_Result then
            Result  := False;
            Message := Fail_Message;
         end if;
      end Notify;

      function Test_Failed return Boolean
      is
      begin
         return not Result;
      end Test_Failed;
   end Test_Result_PO_Type;


   Test_Result_PO : Test_Result_PO_Type (Number_Of_CPUs);


   ----------------------------------------------------------------------------
   --  Test_Bits_40_Fine
   ----------------------------------------------------------------------------

   procedure Test_Bits_40_Fine (T : in out Test)
   is

      --  This task type runs a range of test values for this test.
      --  Multiple tasks are used to process different ranges to provide a
      --  significant speedup
      task type Runner_Task is
         entry Start (Test_First : in Bits_40;
                      Test_Last  : in Bits_40);
      end Runner_Task;

      task body Runner_Task
      is
         First : Bits_40;
         Last  : Bits_40;

      begin
         accept Start (Test_First : in Bits_40;
                       Test_Last  : in Bits_40) do
            First := Test_First;
            Last  := Test_Last;
         end Start;

         loop
            --  Don't check for test failure on every iteration, in order to
            --  prevent slowdown and lots of lock contention on the PO.
            for I in Bits_40 range 0 .. 1_000_000 loop
               pragma Assert
                 (First = To_Bits_40 (To_Fine_System_Time (First)),
                  "failed for X =" & Bits_40'Image (First) &
                    "actual =" &
                    Bits_40'Image (To_Bits_40 (To_Fine_System_Time (First))));

               exit when First = Last;

               First := First + 1;
            end loop;

            --  Stop early if the test has already failed
            --  (another task has noticed a test failure).
            exit when Test_Result_PO.Test_Failed or else First = Last;
         end loop;

         --  Test success.
         Test_Result_PO.Notify (True, Null_Unbounded_String);

      exception
         when Error : others =>
            Test_Result_PO.Notify
              (False, To_Unbounded_String (Exception_Message (Error)));
      end Runner_Task;


      Runners : array (CPU range 1 .. Number_Of_CPUs) of Runner_Task;

      Test_Result  : Boolean;
      Fail_Message : Unbounded_String;

      First        : Bits_40;
      Last         : Bits_40;
      Chunk_Length : constant Bits_40 := 2**40 / Runners'Length;

   begin

      Test_Result_PO.Reset;

      First := 0;
      Last  := Chunk_Length - 1;

      for I in Runners'Range loop
         if I < Number_Of_CPUs then
            Runners (I).Start (First, Last);
         else
            Runners (I).Start (First, Bits_40'Last);
         end if;

         First := Last + 1;
         Last  := Last + (Chunk_Length - 1);
      end loop;

      --  Wait for all tasks to terminate (successfully or not).
      --  Tasks will stop early when they notice that the test has failed.
      Test_Result_PO.Wait (Test_Result, Fail_Message);

      Assert (Test_Result, To_String (Fail_Message));

   end Test_Bits_40_Fine;


   -------------------------------------------------------------------------	---
   --  Test_Bits_40_Coarse
   ----------------------------------------------------------------------------

   procedure Test_Bits_40_Coarse (T : in out Test)
   is

      --  This task type runs a range of test values for this test.
      --  Multiple tasks are used to process different ranges to provide a
      --  significant speedup
      task type Runner_Task is
         entry Start (Test_First : in Bits_40;
                      Test_Last  : in Bits_40);
      end Runner_Task;

      task body Runner_Task
      is
         First : Bits_40;
         Last  : Bits_40;

      begin
         accept Start (Test_First : in Bits_40;
                       Test_Last  : in Bits_40) do
            First := Test_First;
            Last  := Test_Last;
         end Start;

         loop
            --  Don't check for test failure on every iteration, in order to
            --  prevent slowdown and lots of lock contention on the PO.
            for I in Bits_40 range 0 .. 1_000_000 loop
               pragma Assert
                 ((First and 16#FFFFFFFE00#) =
                      To_Bits_40 (To_Coarse_System_Time (First)),
                  "failed for Y =" & Bits_40'Image (First) &
                    ", actual =" &
                    Bits_40'Image (To_Bits_40 (To_Coarse_System_Time (First)))
                 );

               exit when First = Last;

               First := First + 1;
            end loop;

            --  Stop early if the test has already failed
            --  (another task has noticed a test failure).
            exit when Test_Result_PO.Test_Failed or else First = Last;
         end loop;

         --  Test success.
         Test_Result_PO.Notify (True, Null_Unbounded_String);

      exception
         when Error : others =>
            Test_Result_PO.Notify
              (False, To_Unbounded_String (Exception_Message (Error)));
      end Runner_Task;


      Runners : array (CPU range 1 .. Number_Of_CPUs) of Runner_Task;

      Test_Result  : Boolean;
      Fail_Message : Unbounded_String;

      First        : Bits_40;
      Last         : Bits_40;
      Chunk_Length : constant Bits_40 := 2**40 / Runners'Length;

   begin

      Test_Result_PO.Reset;

      First := 0;
      Last  := Chunk_Length - 1;

      for I in Runners'Range loop
         if I < Number_Of_CPUs then
            Runners (I).Start (First, Last);
         else
            Runners (I).Start (First, Bits_40'Last);
         end if;

         First := Last + 1;
         Last  := Last + (Chunk_Length - 1);
      end loop;

      --  Wait for all tasks to terminate (successfully or not).
      --  Tasks will stop early when they notice that the test has failed.
      Test_Result_PO.Wait (Test_Result, Fail_Message);

      Assert (Test_Result, To_String (Fail_Message));

   end Test_Bits_40_Coarse;


   ----------------------------------------------------------------------------
   --  Test_To_Coarse_System_Time
   ----------------------------------------------------------------------------

   procedure Test_To_Coarse_System_Time (T : in out Test)
   is

      --  This task type runs a range of test values for this test.
      --  Multiple tasks are used to process different ranges to provide a
      --  significant speedup
      task type Runner_Task is
         entry Start (Test_First : in Coarse_System_Time;
                      Test_Last  : in Coarse_System_Time);
      end Runner_Task;

      task body Runner_Task
      is
         First : Coarse_System_Time;
         Last  : Coarse_System_Time;

      begin
         accept Start (Test_First : in Coarse_System_Time;
                       Test_Last  : in Coarse_System_Time) do
            First := Test_First;
            Last  := Test_Last;
         end Start;

         loop
            --  Don't check for test failure on every iteration, in order to
            --  prevent slowdown and lots of lock contention on the PO.
            for I in Integer range 0 .. 1_000_000 loop
               pragma Assert
                 (First = To_Coarse_System_Time (To_Fine_System_Time (First)),
                  "failed for X =" & Coarse_System_Time'Image (First));

               exit when First = Last;

               First := Coarse_System_Time'Succ (First);
            end loop;

            --  Stop early if the test has already failed
            --  (another task has noticed a test failure).
            exit when Test_Result_PO.Test_Failed or else First = Last;
         end loop;

         --  Test success.
         Test_Result_PO.Notify (True, Null_Unbounded_String);

      exception
         when Error : others =>
            Test_Result_PO.Notify
              (False, To_Unbounded_String (Exception_Message (Error)));
      end Runner_Task;


      Runners : array (CPU range 1 .. Number_Of_CPUs) of Runner_Task;

      Test_Result  : Boolean;
      Fail_Message : Unbounded_String;

      First        : Coarse_System_Time;
      Last         : Coarse_System_Time;
      Chunk_Length : constant Coarse_System_Time :=
                       Coarse_System_Time'Last / Runners'Length;

   begin

      Test_Result_PO.Reset;

      First := 0.0;
      Last  := Chunk_Length;

      for I in Runners'Range loop
         if I < Number_Of_CPUs then
            Runners (I).Start (First, Last);
            First := Last;
            Last  := Last + Chunk_Length;
         else
            Runners (I).Start (First, Coarse_System_Time'Last);
         end if;
      end loop;

      --  Wait for all tasks to terminate (successfully or not).
      --  Tasks will stop early when they notice that the test has failed.
      Test_Result_PO.Wait (Test_Result, Fail_Message);

      Assert (Test_Result, To_String (Fail_Message));

   end Test_To_Coarse_System_Time;

end DW1000.System_Time.Tests;
