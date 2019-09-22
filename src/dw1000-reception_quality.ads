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

with DW1000.Register_Types; use DW1000.Register_Types;
with DW1000.Types;          use DW1000.Types;

--  @summary
--  Utility functions for measuring the quality of received frames.
package DW1000.Reception_Quality
with SPARK_Mode => On
is

   function Adjust_RXPACC (RXPACC              : in RX_FINFO_RXPACC_Field;
                           RXPACC_NOSAT        : in Bits_16;
                           RXBR                : in RX_FINFO_RXBR_Field;
                           SFD_LENGTH          : in Bits_8;
                           Non_Standard_SFD    : in Boolean) return RX_FINFO_RXPACC_Field
     with Pre => (RXBR /= Reserved
                  and (if Non_Standard_SFD then SFD_LENGTH in 8 | 16));
   --  Apply the correction to the RXPACC value.
   --
   --  The preamble accumulation count (RXPACC) value may include SFD symbols
   --  in the count. This function removes SFD symbols from the preamble
   --  symbol count in RXPACC, and returns the adjusted RXPACC value.
   --
   --  Note: This function does not support user-defined SFD sequences. It only
   --  supports the standard and DecaWave defined SFD sequences. The specific
   --  SFD sequence used is determined from the RXBR, SFD_LENGTH, and
   --  Non_Standard_SFD parameters.
   --
   --  @param RXPACC The value of the RXPACC field from the RX_FINFO register.
   --     This is the value which is to be adjusted.
   --
   --  @param RXPACC_NOSAT The value of the RXPACC_NOSAT register.
   --
   --  @param RXBR The value of the RXBR field from the RX_FINFO register.
   --     This value determines the data rate of the received frame (110 kbps,
   --     850 kbps, or 6.8 Mbps).
   --
   --  @param SFD_LENGTH The value of the SFD_LENGTH field from the USR_SFD
   --     register. This value must be 8 or 16 symbols and is used only if
   --     Non_Standard_SFD is True. Otherwise, the value does not matter.
   --
   --  @param Non_Standard_SFD Determines whether or not the standards-defined
   --     SFD sequence is used, or the DecaWave defined sequence is used.


   function Receive_Signal_Power (Use_16MHz_PRF : in Boolean;
                                  RXPACC        : in RX_FINFO_RXPACC_Field;
                                  CIR_PWR       : in Bits_16) return Float
     with Post => Receive_Signal_Power'Result in -142.81 .. -14.43;
   --  Compute the estimated receive signal power in dBm.
   --
   --  @param Use_16MHz_PRF Set to True if a 16 MHz PRF is used, otherwise set
   --     to False to indicate a 64 MHz PRF.
   --
   --  @param RXPACC The value of the RXPACC field from the RX_FINFO register.
   --     Note that this value should be corrected before calling this function
   --     if it is equal to the RXPACC_NOSAT register. See the description for
   --     the RXPACC field in the DW1000 User Manual in Section 7.2.18 for more
   --     information on correcting the RXPACC.
   --
   --  @param CIR_PWR The value of the CIR_PWR field from the RX_FQUAL register
   --
   --  @return The estimated receive signal power in dBm. The theoretical range
   --     is -166.90 dBm to -14.43 dBm.


   function First_Path_Signal_Power (Use_16MHz_PRF : in Boolean;
                                     F1            : in Bits_16;
                                     F2            : in Bits_16;
                                     F3            : in Bits_16;
                                     RXPACC        : in RX_FINFO_RXPACC_Field)
                                     return Float
     with Post => First_Path_Signal_Power'Result in -193.99 .. -17.44;
   --  Compute the estimated first path power level in dBm.
   --
   --  @param Use_16MHz_PRF Set to True if a 16 MHz PRF is used, otherwise set
   --     to False to indicate a 64 MHz PRF.
   --
   --  @param F1 The value of the FP_AMPL1 field from the RX_TIME register.
   --
   --  @param F2 The value of the FP_AMPL2 field from the RX_FQUAL register.
   --
   --  @param F3 The value of the FP_AMPL3 field from the RX_FQUAL register.
   --
   --  @param RXPACC The value of the RXPACC field from the RX_FINFO register.
   --     Note that this value should be corrected before calling this function
   --     if it is equal to the RXPACC_NOSAT register. See the description for
   --     the RXPACC field in the DW1000 User Manual in Section 7.2.18 for more
   --     information on correcting the RXPACC.
   --
   --  @return The estimated first path power in dBm. The theoretical range
   --     is -218.07 dBm to -12.67 dBm.


   function Transmitter_Clock_Offset (RXTOFS  : in Bits_19;
                                      RXTTCKI : in Bits_32) return Long_Float
     with Post => Transmitter_Clock_Offset'Result in -1.0 .. 1.0;
   --  Calculate the clock offset between the receiver's and transmitter's
   --  clocks.
   --
   --  Since the transmitter and receiver radios are clocked by their own
   --  crystals, there can be a slight variation between the crystals'
   --  frequencies. This function provides a measure of the offset
   --  between this receiver and the remote transmitter clocks.
   --
   --  @param RXTOFS The value of the RXTOFS field from the RX_TTCKO register.
   --
   --  @param RXTTCKI The value of the RXTTCKI field from the RX_TTCKI
   --     register.
   --
   --  @return The computed clock offset. A positive value indicates that the
   --     transmitter's clock is running faster than the receiver's clock, and
   --     a negative value indicates that the transmitter's clock is running
   --     slower than the receiver's clock. For example, a value of 7.014E-06
   --     indicates that the transmitter is faster by 7 ppm. Likewise, a value
   --     of -5.045E-06 indicates that the transmitter's clock is slower by
   --     5 ppm.

end DW1000.Reception_Quality;
