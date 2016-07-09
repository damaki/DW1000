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

with DW1000.Types; use DW1000.Types;

--  @summary
--  Utility functions for measuring the quality of received frames.
package DW1000.Reception_Quality
with SPARK_Mode => On
is

   function Receive_Signal_Power (Use_16MHz_PRF : in Boolean;
                                  RXPACC        : in Bits_12;
                                  CIR_PWR       : in Bits_16) return Float
     with Post => Receive_Signal_Power'Result in -166.90 .. -14.43;
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
                                     RXPACC        : in Bits_12) return Float
     with Post => First_Path_Signal_Power'Result in -218.07 .. -12.66;
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

end DW1000.Reception_Quality;
