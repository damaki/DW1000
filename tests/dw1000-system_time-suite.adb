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
with AUnit.Test_Caller;
with DW1000.System_Time.Tests;

package body DW1000.System_Time.Suite
is

   function Suite return Access_Test_Suite
   is
      package Caller is new AUnit.Test_Caller (Tests.Test);

      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test(Caller.Create
                   ("Test conversion: Bits_40 -> Fine -> Bits_40",
                      Tests.Test_Bits_40_Fine'Access));
      Ret.Add_Test(Caller.Create
                   ("Test conversion: Bits_40 -> Coarse -> Bits_40",
                      Tests.Test_Bits_40_Coarse'Access));
      Ret.Add_Test(Caller.Create
                   ("Test conversion: Coarse -> Fine -> Coarse",
                      Tests.Test_To_Coarse_System_Time'Access));

      return Ret;
   end Suite;

end DW1000.System_Time.Suite;
