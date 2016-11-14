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

with DW1000.Driver;
with DW1000.System_Time; use DW1000.System_Time;

package DW1000.Ranging
with SPARK_Mode => On
is
   Air_Refractive_Index     : constant := 1.0003;

   Speed_Of_Light_In_Vacuum : constant := 299_792_458.0;

   Speed_Of_Light_In_Air    : constant
     := Speed_Of_Light_In_Vacuum / Air_Refractive_Index;

   type Distance is
   delta 0.01
   range 0.0 .. System_Time_Span'Last * Speed_Of_Light_In_Air
     with Small => 0.01;
   --  Distance in meters, with precision to 1 cm.
   --
   --  The maximum possible distance represented by this type is a little
   --  over 5 million kilometers. Although such a distance is not achievable
   --  in practice since the range of the DW1000 is limited to about 300 m,
   --  the range of the DW1000 timestamps (40-bit) do permit such a large value
   --  to be calculated if extreme values are used.

   type Biased_Distance is new Distance;
   --  Type for distance measurements which include the ranging bias.

   function Remove_Ranging_Bias
     (Measured_Distance : in Biased_Distance;
      Channel           : in DW1000.Driver.Channel_Number;
      PRF               : in DW1000.Driver.PRF_Type) return Distance
     with Global => null;
   --  Remove the ranging bias from a distance measurement.
   --
   --  The distance calculated using the DW1000 contains a bias based on the
   --  channel and PRF configuration used during the measurement. This bias
   --  must be removed to achieve a more accurate ranging measurement.
   --
   --  @param Measured_Distance The distance measurement which includes the
   --     ranging bias.
   --
   --  @param Channel The UWB channel which was used to measure the distance.
   --
   --  @param PRF The PRF which was used to measure the distance.
   --
   --  @return The corrected distance, with the bias removed.

end DW1000.Ranging;
