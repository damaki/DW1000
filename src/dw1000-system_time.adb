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

package body DW1000.System_Time
with SPARK_Mode => On
is

   --------------------------
   --  System_Time_Offset  --
   --------------------------

   function System_Time_Offset (Time : in Fine_System_Time;
                                Span : in System_Time_Span)
                                return Fine_System_Time
   is
      Span_FST : constant Fine_System_Time := Fine_System_Time (Span);
   begin
      if Fine_System_Time'Last - Time >= Span_FST then
         return Time + Span_FST;
      else
         --  Wrap-around case
         return Fine_System_Time'Last - Span_FST;
      end if;
   end System_Time_Offset;

   ----------------------
   --  Calculate_Span  --
   ----------------------

   function Calculate_Span (Start_Time : in Fine_System_Time;
                            End_Time   : in Fine_System_Time)
                            return System_Time_Span
   is
   begin
      if Start_Time <= End_Time then
         return System_Time_Span (End_Time - Start_Time);

      else
         return System_Time_Span ((Fine_System_Time'Last - Start_Time) +
                                    End_Time);
      end if;
   end Calculate_Span;

end DW1000.System_Time;
