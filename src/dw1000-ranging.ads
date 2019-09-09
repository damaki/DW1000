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

   type Meters is new Float
     range 0.0 .. Float (Speed_Of_Light_In_Air * System_Time_Span'Last);
   --  Distance in meters.
   --
   --  The range of this type is constrained to the range of distance values
   --  possible based on the maximum possible value for the time of flight.
   --
   --  This is a Float type (rather than a fixed-point type) to avoid some
   --  difficulties when dealing with fixed-point types with fractional 'smalls,
   --  for example that 1.0 can't be represented exactly. If a fixed-point type
   --  without a fractional 'small is used then we would run into problems where
   --  conversions from System_Time_Span to Meters would not be provable in SPARK
   --  because the conversion would not be in the perfect result set as defined
   --  in Annex G.2.3 of the Ada 2012 LRM. GNATprove currently limits conversions
   --  between fixed-point types to always belong to the perfect result set.

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
