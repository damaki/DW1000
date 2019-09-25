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

with DW1000.Driver; use DW1000.Driver;

package body DW1000.Ranging
with SPARK_Mode => On
is

   type Short_Distance is
   delta 0.25
   range 0.0 .. 63.75
     with Size => 8;

   type Correction_Table is array (Natural range <>) of Short_Distance;

   subtype Correction_Distance is Meters range 0.0 .. 0.68;
   --  Subtype to constrain the maximum amount of correction that is applied.

   Offset_16MHz_Narrowband : constant Correction_Distance := 0.23;
   Offset_16MHz_Wideband   : constant Correction_Distance := 0.28;
   Offset_64MHz_Narrowband : constant Correction_Distance := 0.17;
   Offset_64MHz_Wideband   : constant Correction_Distance := 0.30;

   -------------------------
   --  Correction Tables  --
   -------------------------

   --  These lookup tables are used to determine the correction value needed
   --  to remove the ranging bias from the raw distance measurements.
   --
   --  Each entry in the table is a threshold value, in meters. The index of
   --  the array element is the correction value needed in centimeters for that
   --  threshold value.

   Correction_Table_Ch1_16MHz : constant Correction_Table
     := (0  =>  0.25,
         1  =>  0.75,
         2  =>  1.00,
         3  =>  1.25,
         4  =>  1.75,
         5  =>  2.25,
         6  =>  2.75,
         7  =>  3.00,
         8  =>  3.25,
         9  =>  3.75,
         10 =>  4.50,
         11 =>  5.00,
         12 =>  5.75,
         13 =>  6.25,
         14 =>  7.00,
         15 =>  7.50,
         16 =>  8.25,
         17 =>  9.00,
         18 => 10.00,
         19 => 10.75,
         20 => 11.75,
         21 => 12.50,
         22 => 13.50,
         23 => 14.50,
         24 => 15.75,
         25 => 16.50,
         26 => 17.75,
         27 => 19.00,
         28 => 20.50,
         29 => 22.25,
         30 => 24.50,
         31 => 27.25,
         32 => 31.75,
         33 => 38.75,
         34 => 55.50,
         35 => 63.75,
         36 => 63.75);

   Correction_Table_Ch2_16MHz : constant Correction_Table
     := (0 => 0.25,
         1 => 0.50,
         2 => 1.00,
         3 => 1.25,
         4 => 1.50,
         5 => 2.00,
         6 => 2.25,
         7 => 2.50,
         8 => 3.00,
         9 => 3.25,
         10 => 3.75,
         11 => 4.50,
         12 => 5.00,
         13 => 5.50,
         14 => 6.00,
         15 => 6.75,
         16 => 7.25,
         17 => 8.00,
         18 => 8.75,
         19 => 9.50,
         20 => 10.25,
         21 => 11.00,
         22 => 11.75,
         23 => 12.75,
         24 => 13.75,
         25 => 14.50,
         26 => 15.50,
         27 => 16.50,
         28 => 17.75,
         29 => 19.50,
         30 => 21.25,
         31 => 24.00,
         32 => 27.75,
         33 => 33.75,
         34 => 48.50,
         35 => 60.00,
         36 => 63.75);

   Correction_Table_Ch3_16MHz : constant Correction_Table
     := (0 => 0.25,
         1 => 0.50,
         2 => 0.75,
         3 => 1.00,
         4 => 1.25,
         5 => 1.75,
         6 => 2.00,
         7 => 2.25,
         8 => 2.50,
         9 => 3.00,
         10 => 3.50,
         11 => 4.00,
         12 => 4.50,
         13 => 5.00,
         14 => 5.50,
         15 => 6.00,
         16 => 6.50,
         17 => 7.00,
         18 => 7.75,
         19 => 8.25,
         20 => 9.00,
         21 => 9.75,
         22 => 10.50,
         23 => 11.25,
         24 => 12.25,
         25 => 13.00,
         26 => 13.75,
         27 => 14.75,
         28 => 15.75,
         29 => 17.25,
         30 => 19.00,
         31 => 21.25,
         32 => 24.50,
         33 => 30.00,
         34 => 43.25,
         35 => 53.25,
         36 => 63.75);

   Correction_Table_Ch4_16MHz : constant Correction_Table
     := (0 => 1.75,
         1 => 1.75,
         2 => 2.00,
         3 => 2.25,
         4 => 2.25,
         5 => 2.50,
         6 => 2.75,
         7 => 2.75,
         8 => 3.00,
         9 => 3.25,
         10 => 3.50,
         11 => 3.75,
         12 => 4.00,
         13 => 4.25,
         14 => 4.50,
         15 => 4.75,
         16 => 5.00,
         17 => 5.25,
         18 => 5.50,
         19 => 5.75,
         20 => 6.00,
         21 => 6.50,
         22 => 6.75,
         23 => 7.00,
         24 => 7.50,
         25 => 7.75,
         26 => 8.00,
         27 => 8.50,
         28 => 9.00,
         29 => 9.50,
         30 => 10.00,
         31 => 10.50,
         32 => 11.00,
         33 => 11.50,
         34 => 12.00,
         35 => 12.50,
         36 => 13.00,
         37 => 13.75,
         38 => 14.25,
         39 => 14.75,
         40 => 15.25,
         41 => 15.75,
         42 => 16.50,
         43 => 17.00,
         44 => 17.75,
         45 => 18.50,
         46 => 19.50,
         47 => 20.25,
         48 => 21.25,
         49 => 22.25,
         50 => 23.50,
         51 => 24.75,
         52 => 26.00,
         53 => 27.50,
         54 => 29.00,
         55 => 30.75,
         56 => 32.50,
         57 => 34.75,
         58 => 37.50,
         59 => 41.00,
         60 => 45.50,
         61 => 51.75,
         62 => 59.50,
         63 => 63.75,
         64 => 63.75,
         65 => 63.75,
         66 => 63.75,
         67 => 63.75);

   Correction_Table_Ch5_16MHz : constant Correction_Table
     := (0 => 0.25,
         1 => 0.25,
         2 => 0.50,
         3 => 0.75,
         4 => 1.00,
         5 => 1.25,
         6 => 1.50,
         7 => 1.50,
         8 => 1.75,
         9 => 2.00,
         10 => 2.25,
         11 => 2.75,
         12 => 3.00,
         13 => 3.50,
         14 => 3.75,
         15 => 4.00,
         16 => 4.50,
         17 => 5.00,
         18 => 5.25,
         19 => 5.75,
         20 => 6.25,
         21 => 6.75,
         22 => 7.25,
         23 => 7.75,
         24 => 8.50,
         25 => 9.00,
         26 => 9.50,
         27 => 10.25,
         28 => 11.00,
         29 => 12.00,
         30 => 13.25,
         31 => 14.75,
         32 => 17.00,
         33 => 20.75,
         34 => 30.00,
         35 => 37.00,
         36 => 63.75);

   Correction_Table_Ch7_16MHz : constant Correction_Table
     := (0 => 1.00,
         1 => 1.25,
         2 => 1.25,
         3 => 1.25,
         4 => 1.50,
         5 => 1.50,
         6 => 1.75,
         7 => 1.75,
         8 => 1.75,
         9 => 2.00,
         10 => 2.25,
         11 => 2.25,
         12 => 2.50,
         13 => 2.50,
         14 => 2.75,
         15 => 2.75,
         16 => 3.00,
         17 => 3.25,
         18 => 3.25,
         19 => 3.50,
         20 => 3.75,
         21 => 4.00,
         22 => 4.25,
         23 => 4.25,
         24 => 4.50,
         25 => 4.75,
         26 => 5.00,
         27 => 5.25,
         28 => 5.50,
         29 => 5.75,
         30 => 6.25,
         31 => 6.50,
         32 => 6.75,
         33 => 7.25,
         34 => 7.50,
         35 => 7.75,
         36 => 8.00,
         37 => 8.50,
         38 => 8.75,
         39 => 9.00,
         40 => 9.50,
         41 => 9.75,
         42 => 10.00,
         43 => 10.50,
         44 => 11.00,
         45 => 11.50,
         46 => 12.00,
         47 => 12.50,
         48 => 13.00,
         49 => 13.75,
         50 => 14.50,
         51 => 15.25,
         52 => 16.00,
         53 => 17.00,
         54 => 18.00,
         55 => 18.75,
         56 => 20.00,
         57 => 21.25,
         58 => 23.00,
         59 => 25.25,
         60 => 28.00,
         61 => 31.75,
         62 => 36.75,
         63 => 42.00,
         64 => 45.50,
         65 => 48.50,
         66 => 51.25,
         67 => 63.75);

   Correction_Table_Ch1_64MHz : constant Correction_Table
     := (0 => 0.25,
         1 => 0.50,
         2 => 0.50,
         3 => 0.75,
         4 => 1.00,
         5 => 1.25,
         6 => 1.75,
         7 => 2.50,
         8 => 3.25,
         9 => 4.00,
         10 => 4.75,
         11 => 5.50,
         12 => 6.00,
         13 => 6.75,
         14 => 7.50,
         15 => 8.00,
         16 => 8.75,
         17 => 9.50,
         18 => 10.75,
         19 => 12.00,
         20 => 14.00,
         21 => 19.50,
         22 => 25.25,
         23 => 30.00,
         24 => 39.25,
         25 => 63.75);

   Correction_Table_Ch2_64MHz : constant Correction_Table
     := (0 => 0.25,
         1 => 0.50,
         2 => 0.50,
         3 => 0.75,
         4 => 1.00,
         5 => 1.00,
         6 => 1.50,
         7 => 2.25,
         8 => 3.00,
         9 => 3.50,
         10 => 4.25,
         11 => 4.75,
         12 => 5.25,
         13 => 6.00,
         14 => 6.50,
         15 => 7.00,
         16 => 7.75,
         17 => 8.25,
         18 => 9.25,
         19 => 10.50,
         20 => 12.25,
         21 => 17.00,
         22 => 22.25,
         23 => 26.25,
         24 => 34.50,
         25 => 63.75);

   Correction_Table_Ch3_64MHz : constant Correction_Table
     := (0 => 0.25,
         1 => 0.25,
         2 => 0.50,
         3 => 0.75,
         4 => 0.75,
         5 => 1.00,
         6 => 1.25,
         7 => 2.00,
         8 => 2.50,
         9 => 3.25,
         10 => 3.75,
         11 => 4.25,
         12 => 4.75,
         13 => 5.25,
         14 => 5.75,
         15 => 6.25,
         16 => 6.75,
         17 => 7.50,
         18 => 8.25,
         19 => 9.25,
         20 => 11.00,
         21 => 15.00,
         22 => 19.75,
         23 => 23.25,
         24 => 30.50,
         25 => 63.75);

   Correction_Table_Ch4_64MHz : constant Correction_Table
     := (0 => 1.75,
         1 => 2.00,
         2 => 2.00,
         3 => 2.25,
         4 => 2.25,
         5 => 2.50,
         6 => 2.75,
         7 => 3.00,
         8 => 3.25,
         9 => 3.25,
         10 => 3.50,
         11 => 3.75,
         12 => 4.00,
         13 => 4.00,
         14 => 4.25,
         15 => 4.50,
         16 => 4.75,
         17 => 4.75,
         18 => 5.00,
         19 => 5.25,
         20 => 5.50,
         21 => 6.00,
         22 => 6.25,
         23 => 6.75,
         24 => 7.00,
         25 => 7.25,
         26 => 7.50,
         27 => 8.00,
         28 => 8.25,
         29 => 8.50,
         30 => 8.75,
         31 => 9.25,
         32 => 9.75,
         33 => 10.25,
         34 => 10.75,
         35 => 11.25,
         36 => 12.00,
         37 => 12.50,
         38 => 13.25,
         39 => 14.00,
         40 => 15.00,
         41 => 16.00,
         42 => 17.00,
         43 => 18.50,
         44 => 20.25,
         45 => 22.25,
         46 => 24.50,
         47 => 27.25,
         48 => 30.50,
         49 => 34.00,
         50 => 36.50,
         51 => 38.50,
         52 => 40.50,
         53 => 44.50,
         54 => 55.00,
         55 => 62.25,
         56 => 63.75,
         57 => 63.75,
         58 => 63.75);

   Correction_Table_Ch5_64MHz : constant Correction_Table
     := (0 => 0.25,
         1 => 0.25,
         2 => 0.25,
         3 => 0.50,
         4 => 0.50,
         5 => 0.75,
         6 => 1.00,
         7 => 1.50,
         8 => 1.75,
         9 => 2.25,
         10 => 2.50,
         11 => 3.00,
         12 => 3.25,
         13 => 3.75,
         14 => 4.00,
         15 => 4.25,
         16 => 4.75,
         17 => 5.25,
         18 => 5.75,
         19 => 6.50,
         20 => 7.50,
         21 => 10.50,
         22 => 13.75,
         23 => 16.25,
         24 => 21.25,
         25 => 63.75);

   Correction_Table_Ch7_64MHz : constant Correction_Table
     := (0 => 1.00,
         1 => 1.25,
         2 => 1.25,
         3 => 1.25,
         4 => 1.50,
         5 => 1.50,
         6 => 1.75,
         7 => 1.75,
         8 => 2.00,
         9 => 2.00,
         10 => 2.25,
         11 => 2.25,
         12 => 2.50,
         13 => 2.50,
         14 => 2.50,
         15 => 2.75,
         16 => 2.75,
         17 => 3.00,
         18 => 3.25,
         19 => 3.25,
         20 => 3.50,
         21 => 3.75,
         22 => 4.00,
         23 => 4.00,
         24 => 4.25,
         25 => 4.50,
         26 => 4.75,
         27 => 4.75,
         28 => 5.00,
         29 => 5.25,
         30 => 5.50,
         31 => 5.75,
         32 => 6.00,
         33 => 6.25,
         34 => 6.50,
         35 => 7.00,
         36 => 7.25,
         37 => 7.75,
         38 => 8.25,
         39 => 8.75,
         40 => 9.25,
         41 => 9.75,
         42 => 10.50,
         43 => 11.50,
         44 => 12.50,
         45 => 13.50,
         46 => 15.00,
         47 => 16.75,
         48 => 18.75,
         49 => 20.75,
         50 => 22.50,
         51 => 23.75,
         52 => 25.00,
         53 => 27.50,
         54 => 33.75,
         55 => 38.25,
         56 => 43.00,
         57 => 48.00,
         58 => 63.75);

   -------------------------
   --  Lookup_Correction  --
   -------------------------

   function Lookup_Correction (Measured_Distance : in Meters;
                               Table             : in Correction_Table)
                               return Correction_Distance
     with Global => null,
     Pre => Table'Length <= 68;

   function Lookup_Correction (Measured_Distance : in Meters;
                               Table             : in Correction_Table)
                               return Correction_Distance is

      Distance_25cm : Short_Distance;

      I : Natural;

   begin
      if Measured_Distance > Meters (Short_Distance'Last) then
         Distance_25cm := Short_Distance'Last;

      else
         --  Workaround since GNATprove does not yet support conversions
         --  between different fixed-point and floating-point types.
         --
         --  This is equivalent to:
         --     Distance_25cm := Short_Distance (Measured_Distance);
         Distance_25cm :=
           Short_Distance'Delta * Integer (Measured_Distance / Meters (Short_Distance'Delta));
      end if;

      --  Find the index of the table entry which matches the estimated distance.
      I := 0;
      while I < Table'Length loop
         pragma Loop_Variant (Increases => I);

         exit when Distance_25cm <= Table (Table'First + I);
         I := I + 1;
      end loop;

      --  The index is the correction needed in centimeters.
      return Correction_Distance (0.01) * Meters (I);

   end Lookup_Correction;

   ---------------------------
   --  Remove_Ranging_Bias  --
   ---------------------------

   function Remove_Ranging_Bias
     (Measured_Distance : in Biased_Distance;
      Channel           : in DW1000.Driver.Channel_Number;
      PRF               : in DW1000.Driver.PRF_Type) return Meters is
      Initial_Distance : constant Meters := Meters (Measured_Distance);

      Correction : Correction_Distance;
      PRF_Offset : Correction_Distance;

      Result     : Meters;

   begin

      if PRF = PRF_16MHz then
         case Channel is
            when 1 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch1_16MHz);
               PRF_Offset := Offset_16MHz_Narrowband;

            when 2 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch2_16MHz);
               PRF_Offset := Offset_16MHz_Narrowband;

            when 3 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch3_16MHz);
               PRF_Offset := Offset_16MHz_Narrowband;

            when 4 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch4_16MHz);
               PRF_Offset := Offset_16MHz_Wideband;

            when 5 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch5_16MHz);
               PRF_Offset := Offset_16MHz_Narrowband;

            when 7 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch7_16MHz);
               PRF_Offset := Offset_16MHz_Wideband;
         end case;

      else
         case Channel is
            when 1 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch1_64MHz);
               PRF_Offset := Offset_64MHz_Narrowband;

            when 2 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch2_64MHz);
               PRF_Offset := Offset_64MHz_Narrowband;

            when 3 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch3_64MHz);
               PRF_Offset := Offset_64MHz_Narrowband;

            when 4 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch4_64MHz);
               PRF_Offset := Offset_64MHz_Wideband;

            when 5 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch5_64MHz);
               PRF_Offset := Offset_64MHz_Narrowband;

            when 7 =>
               Correction := Lookup_Correction (Initial_Distance,
                                                Correction_Table_Ch7_64MHz);
               PRF_Offset := Offset_64MHz_Wideband;
         end case;
      end if;

      --  Compute the following, but be careful to avoid overflow.
      --     Result := Initial_Distance - (Correction - PRF_Offset)

      if Correction >= PRF_Offset then
         Correction := Correction - PRF_Offset;

         if Initial_Distance >= Correction then
            Result := Initial_Distance - Correction;
         else
            Result := 0.0;
         end if;

      else
         --  Positive correction
         Correction := PRF_Offset - Correction;

         Result := Initial_Distance + Correction;
      end if;

      return Result;
   end Remove_Ranging_Bias;

end DW1000.Ranging;
