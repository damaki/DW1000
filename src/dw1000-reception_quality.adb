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

with Ada.Numerics.Generic_Elementary_Functions;
with DW1000.Register_Types;                     use DW1000.Register_Types;
with Interfaces;                                use Interfaces;

package body DW1000.Reception_Quality
with SPARK_Mode => On
is


   function Adjust_RXPACC (RXPACC              : in RX_FINFO_RXPACC_Field;
                           RXPACC_NOSAT        : in RXPACC_NOSAT_Field;
                           RXBR                : in RX_FINFO_RXBR_Field;
                           SFD_LENGTH          : in Bits_8;
                           Non_Standard_SFD    : in Boolean) return RX_FINFO_RXPACC_Field
   is
      RXPACC_Adjustment : RX_FINFO_RXPACC_Field;

   begin
      if RXPACC_NOSAT_Field (RXPACC) = RXPACC_NOSAT then
         if Non_Standard_SFD then
            --  DecaWave-defined SFD sequence is used
            if RXBR = Data_Rate_110K then
               --  110 kbps data rate. SFD length is always 64 symbols
               RXPACC_Adjustment := 82;
            else
               --  850 kbps and 6.8 Mbps. SFD length is 8 or 16 symbols
               RXPACC_Adjustment := (if SFD_LENGTH = 8 then 10 else 18);
            end if;

         else
            --  Standard-defined SFD sequence is used
            if RXBR = Data_Rate_110K then
               -- 110 kbps data rate. SFD length is always 64 symbols
               RXPACC_Adjustment := 64;
            else
               -- 850 kbps and 6.8 Mbps. SFD length is always 8 symbols.
               RXPACC_Adjustment := 5;
            end if;
         end if;
      else
         --  No adjustment necessary
         RXPACC_Adjustment := 0;
      end if;

      if RXPACC_Adjustment > RXPACC then
         return 0;
      else
         return RXPACC - RXPACC_Adjustment;
      end if;
   end Adjust_RXPACC;


   function Log10 (X : in Long_Float) return Long_Float
     with Global => null,
     Pre => X > 0.0;
   --  Compute the base 10 logarithm.
   --
   --  This function is a wrapper around the Log function in the package
   --  Ada.Numerics.Generic_Elementary_Functions. This is necessary since
   --  it doesn't have the necessary SPARK annotations, so it is hidden from
   --  SPARK with this function.
   --
   --  The precondition of this function ensures that the Log function in
   --  Ada.Numerics.Generic_Elementary_Functions doesn't raise an
   --  Argument_Error function.


   function Log10 (X : in Long_Float) return Long_Float
     with SPARK_Mode => Off
   is
      package Long_Float_Math is
        new Ada.Numerics.Generic_Elementary_Functions (Long_Float);
   begin
      return Long_Float_Math.Log (X, 10.0);
   end Log10;


   function Receive_Signal_Power (Use_16MHz_PRF : in Boolean;
                                  RXPACC        : in RX_FINFO_RXPACC_Field;
                                  CIR_PWR       : in RX_FQUAL_CIR_PWR_Field)
                                  return Float
   is
      subtype Numerator_Range is Long_Float
      range 2.0**17 .. (2.0**16 - 1.0) * 2.0**17;

      subtype Denominator_Range is Long_Float
      range 1.0 .. (2.0**12 - 1.0)**2;

      type RXPACC_Sq_Range is range 0 .. RX_FINFO_RXPACC_Field'Last**2;

      N   : Numerator_Range;
      D   : Denominator_Range;
      A   : Long_Float;
      R   : Long_Float;
   begin
      --  Calculation from the DW1000 User Manual for the receive signal power
      --  is as follows:
      --              _         _
      --             | C * 2**17 |
      --  10 * log10 | --------- | - A
      --             |_  N**2   _|
      --
      --  Where:
      --    * C is the CIR_PWR value
      --    * N is the RXPACC value
      --    * A is 113.77 for a 16 MHz PRF or 121.74 for a 64 MHz PRF

      if CIR_PWR /= 0 then
         N := Long_Float (Bits_33 (CIR_PWR) * 2**17);

         pragma Assert (N in Numerator_Range);
      else
         --  Prevent value of 0 for the numerator, otherwise the input
         --  to the Log function would be 0, which is not permitted.
         N := 2.0**17;

         pragma Assert (N in Numerator_Range);
      end if;

      pragma Assert_And_Cut (N in Numerator_Range);

      if RXPACC /= 0 then
         D := Long_Float (RXPACC_Sq_Range (RXPACC) * RXPACC_Sq_Range (RXPACC));

         pragma Assert (D in Denominator_Range);
      else
         --  Prevent division by zero.
         --  In theory, this should never happen.
         D := 1.0;

         pragma Assert (D in Denominator_Range);
      end if;

      pragma Assert_And_Cut (N in Numerator_Range and D in Denominator_Range);

      R := Log10 (N / D);

      --  The values in this assumption are based on the mathemtical min/max
      --  values of Log10 based on the possible range of the input: N / D.
      pragma Assume
        (R in -2.10699788590519423954821296166977269784955865640566952845
         .. 9.933983230052930026417915579094972598428724237632091142242,
         "The possible output range of log10, for the possible input range");

      if Use_16MHz_PRF then
         A := 113.77; --  16 MHz PRF
      else
         A := 121.74; --  64 MHz PRF
      end if;

      return Float ((10.0 * R) - A);

   end Receive_Signal_Power;


   function First_Path_Signal_Power (Use_16MHz_PRF : in Boolean;
                                     F1            : in RX_TIME_FP_AMPL1_Field;
                                     F2            : in RX_FQUAL_FP_AMPL2_Field;
                                     F3            : in RX_FQUAL_FP_AMPL3_Field;
                                     RXPACC        : in RX_FINFO_RXPACC_Field)
                                     return Float
   is
      subtype F_Range is Long_Float range 0.0 .. (2.0**16 - 1.0)**2;

      subtype Numerator_Range  is Long_Float
      range 1.0 .. (F_Range'Last * 3.0);

      subtype Denominator_Range is Long_Float
      range 1.0 .. (2.0**12 - 1.0)**2;

      type AMPL_Sq_Range is range 0 .. (2**16 - 1)**2;

      type AMPL_Sum_Range is range 0 .. AMPL_Sq_Range'Last * 3;

      type RXPACC_Sq_Range is range 0 .. RX_FINFO_RXPACC_Field'Last**2;

      F_Sum : AMPL_Sum_Range;
      N     : Numerator_Range;
      D     : Denominator_Range;
      R     : Long_Float;
      A     : Long_Float;
   begin
      --  Calculation from the DW1000 User Manual for the receive signal power
      --  is as follows:
      --              _                     _
      --             | F1**2 + F2**2 + F3**3 |
      --  10 * log10 | --------------------- | - A
      --             |_        N**2         _|
      --
      --  Where:
      --    * F1 is the FP_AMPL1 field from the RX_TIME register
      --    * F2 is the FP_AMPL2 field from the RX_FQUAL register
      --    * F3 is the FP_AMPL3 field from the RX_FQUAL register
      --    * N is the RXPACC value
      --    * A is 113.77 for a 16 MHz PRF or 121.74 for a 64 MHz PRF

      if F1 /= 0 or F2 /= 0 or F3 /= 0 then
         F_Sum := (AMPL_Sum_Range (AMPL_Sq_Range (F1) * AMPL_Sq_Range (F1))
                   + AMPL_Sum_Range (AMPL_Sq_Range (F2) * AMPL_Sq_Range (F2))
                   + AMPL_Sum_Range (AMPL_Sq_Range (F3) * AMPL_Sq_Range (F3)));

         N := Numerator_Range (F_Sum);

         pragma Assert (N in Numerator_Range);
      else
         N := 1.0;

         pragma Assert (N in Numerator_Range);
      end if;

      pragma Assert_And_Cut (N in Numerator_Range);

      if RXPACC /= 0 then
         D := Denominator_Range (RXPACC_Sq_Range (RXPACC) * RXPACC_Sq_Range (RXPACC));

         pragma Assert (D in Denominator_Range);
      else
         --  Prevent division by zero.
         --  In theory, this should never happen.
         D := 1.0;

         pragma Assert (D in Denominator_Range);
      end if;

      pragma Assert_And_Cut (N in Numerator_Range and D in Denominator_Range);

      R := Log10 (N / D);

      --  The values in this assumption are based on the mathemtical min/max
      --  values of Log10 based on the possible range of the input: N / D.
      pragma Assume
        (R in -7.22450781219257524826183006241290354369191576604307340090
         .. 9.632946607530499415568708737557182286738992505552491879931,
         "The possible output range of log10, for the possible input range");

      if Use_16MHz_PRF then
         A := 113.77; --  16 MHz PRF
      else
         A := 121.74; --  64 MHz PRF
      end if;

      return Float ((10.0 * R) - A);

   end First_Path_Signal_Power;


   function Transmitter_Clock_Offset (RXTOFS  : in RX_TTCKO_RXTOFS_Field;
                                      RXTTCKI : in RX_TTCKI_RXTTCKI_Field)
                                      return Long_Float
   is
      Offset   : Long_Float;
      Interval : Long_Float;
   begin
      if RXTOFS > -2**18 then
         Offset := Long_Float (RXTOFS);
      else
         --  Special case for the most negative number
         --  (closest to negative infinity).
         --  Normally, this value would indicate -2.0**18, however, we must
         --  constrain the range of Offset to -(2.0**18 - 1.0) so that we can
         --  prove that Offset / Interval >= -1.0.
         --
         --  If Offset is allowed to have the value -2.0**18 then it is not
         --  possible for GNATprove to prove that Offset / Interval >= -1.0
         --  (presumably the rounding may cause the proof to fail for -1.0)

         Offset := -2.0**18 + 1.0;
      end if;

      pragma Assert_And_Cut (Offset in -2.0**18 + 1.0 .. 2.0**18 - 1.0);

      --  RXTTCKI takes one of two values (Section 7.2.21 of the User Manual):
      --     16#01F00000# for a 16 MHz PRF
      --     16#01FC0000# for a 64 MHz PRF
      --  Based on this assumption, we can constrain the potential range of
      --  Interval to prove absence of overflow and range errors.
      Interval := Long_Float (Bits_32 (RXTTCKI) and 16#01FC0000#);

      pragma Assert_And_Cut
        (Offset in -(2.0**18 - 1.0) .. 2.0**18 - 1.0
         and then Interval in 0.0 | 2.0**18 .. 16#01FC0000.0#
         and then
           (if Interval /= 0.0 then Offset > -Interval and Offset < Interval));

      if Interval /= 0.0 then
         pragma Assert (if Offset < 0.0  then Offset / Interval >= -1.0);
         pragma Assert (if Offset >= 0.0 then Offset / Interval <= 1.0);

         return Offset / Interval;
      else
         return 0.0;
      end if;
   end Transmitter_Clock_Offset;

end DW1000.Reception_Quality;
