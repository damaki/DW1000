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
with AUnit.Assertions; use AUnit.Assertions;
with DW1000.Types;     use DW1000.Types;

package body DW1000.System_Time.Tests
is

   procedure Test_Bits_40_Fine (T : in out Test)
   is
   begin

      for X in Bits_40'Range loop
         Assert (X = To_Bits_40 (To_Fine_System_Time (X)),
                 "failed for X =" & Bits_40'Image (X));
      end loop;

   end Test_Bits_40_Fine;


   procedure Test_Bits_40_Coarse (T : in out Test)
   is
   begin

      for X in Bits_40'Range loop
         Assert ((X and 16#FFFFFFFE00#) = To_Bits_40 (To_Coarse_System_Time (X)),
                 "failed for Y =" & Bits_40'Image (X) &
                   ", actual =" &
                   Bits_40'Image (To_Bits_40 (To_Coarse_System_Time (X))));
      end loop;

   end Test_Bits_40_Coarse;


   procedure Test_To_Coarse_System_Time (T : in out Test)
   is
      X : Coarse_System_Time := Coarse_System_Time'First;
   begin

      loop
         Assert (X = To_Coarse_System_Time (To_Fine_System_Time (X)),
                 "failed for X =" & Coarse_System_Time'Image (X));

         exit when X = Coarse_System_Time'Last;

         X := Coarse_System_Time'Succ (X);
      end loop;

   end Test_To_Coarse_System_Time;

end DW1000.System_Time.Tests;
