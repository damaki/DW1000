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
with AUnit.Test_Fixtures;

package DW1000.System_Time.Tests
is
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Bits_40_Fine (T : in out Test);
   --  Proves the following property:
   --     (for all X in Bits_40'Range =>
   --         X = To_Bits_40 (To_Fine_System_Time (X)))

   procedure Test_Bits_40_Coarse (T : in out Test);
   --  Proves the following property:
   --     (for all X in Bits_40'Range =>
   --         X = To_Bits_40 (To_Coarse_System_Time (X)))

   procedure Test_To_Coarse_System_Time (T : in out Test);
   --  Proves the following property:
   --     (for all X in Coarse_System_Time =>
   --         X = To_Coarse_System_Time (To_Fine_System_Time (X)))

end DW1000.System_Time.Tests;
