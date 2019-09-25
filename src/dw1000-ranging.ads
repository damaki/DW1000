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

   Speed_Of_Light_In_Vacuum : constant := 299_792_458.0; --  meters per second

   Speed_Of_Light_In_Air    : constant
     := Speed_Of_Light_In_Vacuum / Air_Refractive_Index; --  meters per second

   type Meters is delta 0.001 range 0.0 .. 30_000.0;
   --  Distance in meters with a resolution of 1 mm.
   --
   --  This type is constrained to a maximum range of 30 km, which is more than
   --  enough since the operational range of the DW1000 is limited to 300 m.

   type Biased_Distance is new Meters;
   --  Type for distance measurements (in meters) which includes a ranging bias.
   --
   --  The bias is influenced by the UWB channel and pulse repetition frequency
   --  (PRF) that was used to perform the measurement. This bias must be removed
   --  for a more accurate ranging measurement.

   function Remove_Ranging_Bias
     (Measured_Distance : in Biased_Distance;
      Channel           : in DW1000.Driver.Channel_Number;
      PRF               : in DW1000.Driver.PRF_Type) return Meters
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
