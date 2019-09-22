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

--  @summary
--  Reference transmit power gain tables for both smart transmit power and
--  manual transmit power modes for the EVB1000 hardware.
--
--  @description
--  These tables are targeted for the EVB1000 evaluation boards, and can serve
--  as a starting point for calibrating the transmit power for different
--  hardware targets and antennas.
--
--  Below is an example of using this package to configure the DW1000
--  transmitter to operate in the smart tx power mode, when using UWB channel
--  4 and a PRF of 64 MHz:
--
--     DecaDriver.Transmitter.Configure_Tx_Power
--       (Smart_Tx_Power_Table (4, PRF_64MHz));
--
--  Note that if the channel number has the type DW1000.Driver.Channel_Number
--  then it is necessary to cast the Channel_Number variable to the type
--  Positive when indexing these tables. An example of this is shown below:
--
--     DecaDriver.Transmitter.Configure_Tx_Power
--       (Smart_Tx_Power_Table (Positive (Channel), PRF_64MHz));
--
--  The values in this package were calculated from Section 7.2.31.4 in the
--  DW1000 User Manual.
package Tx_Power
with SPARK_Mode => On
is

   --  Values for the smart tx power mode
   Smart_Tx_Power_Table : constant Tx_Power_Config_Table :=
     (1 | 2 => (PRF_16MHz =>
                    (Smart_Tx_Power_Enabled => True,
                     Boost_Normal           => (Fine_Gain   => 10.5,
                                                Coarse_Gain => Gain_7_5_dB),
                     Boost_500us            => (Fine_Gain   => 10.5,
                                                Coarse_Gain => Gain_10_dB),
                     Boost_250us            => (Fine_Gain   => 10.5,
                                                Coarse_Gain => Gain_12_5_dB),
                     Boost_125us            => (Fine_Gain   => 10.5,
                                                Coarse_Gain => Gain_15_dB)),

                PRF_64MHz =>
                  (Smart_Tx_Power_Enabled => True,
                   Boost_Normal           => (Fine_Gain   => 3.5,
                                              Coarse_Gain => Gain_7_5_dB),
                   Boost_500us            => (Fine_Gain   => 3.5,
                                              Coarse_Gain => Gain_10_dB),
                   Boost_250us            => (Fine_Gain   => 3.5,
                                              Coarse_Gain => Gain_12_5_dB),
                   Boost_125us            => (Fine_Gain   => 3.5,
                                              Coarse_Gain => Gain_15_dB))),

      3 => (PRF_16MHz =>
                (Smart_Tx_Power_Enabled => True,
                 Boost_Normal           => (Fine_Gain   => 7.5,
                                            Coarse_Gain => Gain_7_5_dB),
                 Boost_500us            => (Fine_Gain   => 7.5,
                                            Coarse_Gain => Gain_10_dB),
                 Boost_250us            => (Fine_Gain   => 7.5,
                                            Coarse_Gain => Gain_12_5_dB),
                 Boost_125us            => (Fine_Gain   => 7.5,
                                            Coarse_Gain => Gain_15_dB)),

            PRF_64MHz =>
              (Smart_Tx_Power_Enabled => True,
               Boost_Normal           => (Fine_Gain   => 5.5,
                                          Coarse_Gain => Gain_5_dB),
               Boost_500us            => (Fine_Gain   => 5.5,
                                          Coarse_Gain => Gain_7_5_dB),
               Boost_250us            => (Fine_Gain   => 5.5,
                                          Coarse_Gain => Gain_10_dB),
               Boost_125us            => (Fine_Gain   => 5.5,
                                          Coarse_Gain => Gain_12_5_dB))),

      4 => (PRF_16MHz =>
                (Smart_Tx_Power_Enabled => True,
                 Boost_Normal           => (Fine_Gain   => 15.5,
                                            Coarse_Gain => Gain_10_dB),
                 Boost_500us            => (Fine_Gain   => 15.5,
                                            Coarse_Gain => Gain_12_5_dB),
                 Boost_250us            => (Fine_Gain   => 15.5,
                                            Coarse_Gain => Gain_15_dB),
                 Boost_125us            => (Fine_Gain   => 15.5,
                                            Coarse_Gain => Gain_15_dB)),

            PRF_64MHz =>
              (Smart_Tx_Power_Enabled => True,
               Boost_Normal           => (Fine_Gain   => 13.0,
                                          Coarse_Gain => Gain_5_dB),
               Boost_500us            => (Fine_Gain   => 13.0,
                                          Coarse_Gain => Gain_7_5_dB),
               Boost_250us            => (Fine_Gain   => 13.0,
                                          Coarse_Gain => Gain_10_dB),
               Boost_125us            => (Fine_Gain   => 13.0,
                                          Coarse_Gain => Gain_12_5_dB))),

      5 | 6 => (PRF_16MHz =>
                    (Smart_Tx_Power_Enabled => True,
                     Boost_Normal           => (Fine_Gain   => 4.0,
                                                Coarse_Gain => Gain_10_dB),
                     Boost_500us            => (Fine_Gain   => 4.0,
                                                Coarse_Gain => Gain_12_5_dB),
                     Boost_250us            => (Fine_Gain   => 4.0,
                                                Coarse_Gain => Gain_15_dB),
                     Boost_125us            => (Fine_Gain   => 7.0,
                                                Coarse_Gain => Gain_15_dB)),

                PRF_64MHz =>
                  (Smart_Tx_Power_Enabled => True,
                   Boost_Normal           => (Fine_Gain   => 2.5,
                                              Coarse_Gain => Gain_5_dB),
                   Boost_500us            => (Fine_Gain   => 2.5,
                                              Coarse_Gain => Gain_7_5_dB),
                   Boost_250us            => (Fine_Gain   => 2.5,
                                              Coarse_Gain => Gain_10_dB),
                   Boost_125us            => (Fine_Gain   => 2.5,
                                              Coarse_Gain => Gain_12_5_dB))),

      7 => (PRF_16MHz =>
                (Smart_Tx_Power_Enabled => True,
                 Boost_Normal           => (Fine_Gain   => 9.0,
                                            Coarse_Gain => Gain_5_dB),
                 Boost_500us            => (Fine_Gain   => 9.0,
                                            Coarse_Gain => Gain_7_5_dB),
                 Boost_250us            => (Fine_Gain   => 9.0,
                                            Coarse_Gain => Gain_10_dB),
                 Boost_125us            => (Fine_Gain   => 9.0,
                                            Coarse_Gain => Gain_12_5_dB)),

            PRF_64MHz =>
              (Smart_Tx_Power_Enabled => True,
               Boost_Normal           => (Fine_Gain   => 8.5,
                                          Coarse_Gain => Gain_0_dB),
               Boost_500us            => (Fine_Gain   => 8.5,
                                          Coarse_Gain => Gain_2_5_dB),
               Boost_250us            => (Fine_Gain   => 8.5,
                                          Coarse_Gain => Gain_7_5_dB),
               Boost_125us            => (Fine_Gain   => 8.5,
                                          Coarse_Gain => Gain_10_dB))));

   --  Values for the manual tx power mode
   Manual_Tx_Power_Table : constant Tx_Power_Config_Table :=
     (1 | 2 => (PRF_16MHz =>
                    (Smart_Tx_Power_Enabled => False,
                     Boost_SHR | Boost_PhR  => (Fine_Gain   => 10.5,
                                                Coarse_Gain => Gain_7_5_dB)),

                PRF_64MHz =>
                  (Smart_Tx_Power_Enabled => False,
                   Boost_SHR | Boost_PHR  => (Fine_Gain   => 3.5,
                                              Coarse_Gain => Gain_7_5_dB))),

      3 => (PRF_16MHz =>
                (Smart_Tx_Power_Enabled => False,
                 Boost_SHR | Boost_PHR  => (Fine_Gain   => 7.5,
                                            Coarse_Gain => Gain_7_5_dB)),

            PRF_64MHz =>
              (Smart_Tx_Power_Enabled => False,
               Boost_SHR | Boost_PHR  => (Fine_Gain   => 5.5,
                                          Coarse_Gain => Gain_5_dB))),

      4 => (PRF_16MHz =>
                (Smart_Tx_Power_Enabled => False,
                 Boost_SHR | Boost_PHR  => (Fine_Gain   => 15.5,
                                            Coarse_Gain => Gain_10_dB)),

            PRF_64MHz =>
              (Smart_Tx_Power_Enabled => False,
               Boost_SHR | Boost_PHR  => (Fine_Gain   => 13.0,
                                          Coarse_Gain => Gain_5_dB))),

      5 | 6 => (PRF_16MHz =>
                    (Smart_Tx_Power_Enabled => False,
                     Boost_SHR | Boost_PHR  => (Fine_Gain   => 4.0,
                                                Coarse_Gain => Gain_10_dB)),

                PRF_64MHz =>
                  (Smart_Tx_Power_Enabled => False,
                   Boost_SHR | Boost_PHR  => (Fine_Gain   => 2.5,
                                              Coarse_Gain => Gain_5_dB))),

      7 => (PRF_16MHz =>
                (Smart_Tx_Power_Enabled => False,
                 Boost_SHR | Boost_PHR  => (Fine_Gain   => 9.0,
                                            Coarse_Gain => Gain_5_dB)),

            PRF_64MHz =>
              (Smart_Tx_Power_Enabled => False,
               Boost_SHR | Boost_PHR  => (Fine_Gain   => 8.5,
                                          Coarse_Gain => Gain_0_dB))));

end Tx_Power;
