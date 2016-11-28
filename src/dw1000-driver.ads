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

pragma Profile (Ravenscar);
pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with DW1000.Constants;
with DW1000.BSP;
with DW1000.Register_Types; use DW1000.Register_Types;
with DW1000.System_Time;    use DW1000.System_Time;
with DW1000.Types;          use DW1000.Types;
with Interfaces;            use Interfaces;

--  This package contains high-level procedures for using the DW1000.
package DW1000.Driver
with SPARK_Mode => On
is
   type Result_Type is
     (Success,
      Error);

   type Clocks is
     (Enable_All_Seq,
      Force_Sys_XTI,
      Force_Sys_PLL,
      Read_Acc_On,
      Read_Acc_Off,
      Force_OTP_On,
      Force_OTP_Off,
      Force_Tx_PLL);

   type Data_Rates is
     (Data_Rate_110k,  -- 110 kbps
      Data_Rate_850k,  -- 850 kbps
      Data_Rate_6M8); -- 6.8 Mbps
   for Data_Rates use
     (Data_Rate_110k => 2#00#,
      Data_Rate_850k => 2#01#,
      Data_Rate_6M8  => 2#10#);

   type Channel_Number is range 1 .. 7
     with Static_Predicate => Channel_Number in 1..5 | 7;
   --  Channels 1 .. 5 and 7 are supported by the DW1000.

   type PRF_Type is (PRF_16MHz, PRF_64MHz);
   for PRF_Type use
     (PRF_16MHz => 2#01#,
      PRF_64MHz => 2#10#);

   type Preamble_Lengths is
     (PLEN_64,
      PLEN_128,
      PLEN_256,
      PLEN_512,
      PLEN_1024,
      PLEN_1536,
      PLEN_2048,
      PLEN_4096);

   type Preamble_Acq_Chunk_Length is
     (PAC_8,
      PAC_16,
      PAC_32,
      PAC_64);

   type Preamble_Code_Number is new Positive range 1 .. 24;

   type Physical_Header_Modes is
     (Standard_Frames,
      Extended_Frames);
   for Physical_Header_Modes use
     (Standard_Frames => 2#00#,
      Extended_Frames => 2#11#);

   type SFD_Timeout_Number is new Natural range 0 .. (2**16) - 1;

   type SFD_Length_Number is new Natural range 8 .. 64
     with Static_Predicate => SFD_Length_Number in 8 .. 16 | 64;

   type Rx_Modes is (Normal, Sniff);

   type Coarse_Tx_Power_Number is delta 3.0 range 0.0 .. 18.0
     with Small => 0.5;
   --  GNATprove requires a Small which is a negative power of 2 or 10.

   type Fine_Tx_Power_Number is delta 0.5 range 0.0 .. 15.5
     with Small => 0.5;

   type Tx_Power_Value (Coarse_Gain_Enabled : Boolean := True) is record
      Fine_Gain : Fine_Tx_Power_Number;

      case Coarse_Gain_Enabled is
         when True =>
            Coarse_Gain : Coarse_Tx_Power_Number;
         when False =>
            null;
      end case;
   end record;

   type Tx_Power_Config_Type (Smart_Tx_Power_Enabled : Boolean := True) is
      record
         case Smart_Tx_Power_Enabled is
         when True =>
            Boost_Normal : Tx_Power_Value;
            Boost_500us  : Tx_Power_Value;
            Boost_250us  : Tx_Power_Value;
            Boost_125us  : Tx_Power_Value;

         when False =>
            Boost_SHR    : Tx_Power_Value;
            Boost_PHR    : Tx_Power_Value;
         end case;
      end record;

   type Tx_Power_Config_Table is
     array (Positive range 1 .. 7, PRF_Type)
     of Tx_Power_Config_Type;

   function To_Positive (PAC : in Preamble_Acq_Chunk_Length) return Positive
   is (case PAC is
          when PAC_8  => 8,
          when PAC_16 => 16,
          when PAC_32 => 32,
          when PAC_64 => 64);


   function To_Positive (Preamble_Length : in Preamble_Lengths) return Positive
   is (case Preamble_Length is
          when PLEN_64   => 64,
          when PLEN_128  => 128,
          when PLEN_256  => 256,
          when PLEN_512  => 512,
          when PLEN_1024 => 1024,
          when PLEN_1536 => 1536,
          when PLEN_2048 => 2048,
          when PLEN_4096 => 4096);

   function Recommended_PAC (Preamble_Length : in Preamble_Lengths)
                             return Preamble_Acq_Chunk_Length
   is (case Preamble_Length is
          when PLEN_64   => PAC_8,
          when PLEN_128  => PAC_8,
          when PLEN_256  => PAC_16,
          when PLEN_512  => PAC_16,
          when PLEN_1024 => PAC_32,
          when PLEN_1536 => PAC_64,
          when PLEN_2048 => PAC_64,
          when PLEN_4096 => PAC_64);
   --  Get the recommended preamble acquisition chunk (PAC) length based
   --  on the preamble length.
   --
   --  These recommendations are from Section 4.1.1 of the DW1000 User Manual.


   function Recommended_SFD_Timeout
     (Preamble_Length : in Preamble_Lengths;
      SFD_Length      : in SFD_Length_Number;
      PAC             : in Preamble_Acq_Chunk_Length)
      return SFD_Timeout_Number
   is (SFD_Timeout_Number
       ((To_Positive (Preamble_Length) + Positive (SFD_Length) + 1) -
          To_Positive (PAC)));
   --  Compute the recommended SFD timeout for a given preamble length, SFD
   --  length, and preamble acquisition chunk length.
   --
   --  For example, with a preable length of 1024 symbols, an SFD length of
   --  64 symbols, and a PAC length of 32 symbols the recommended SFD timeout
   --  is 1024 + 64 + 1 - 32 = 1057 symbols.
   --
   --  @param Preamble_Length The length of the preamble in symbols.
   --
   --  @param SFD_Length The length of the SFD in symbols. The SFD length
   --     depends on whether or not a non-standard SFD is used, and the data
   --     rate. For a data rate of 110 kbps the SFD length is 64 symbols for
   --     the standard SFD and DecaWave-defined SFD. For a data rate of 850
   --     kbps and above the SFD length is 8 symbols for a standard SFD, and 8
   --     or 16 symbols for the DecaWave-defined SFD sequence.
   --
   --  @param PAC The preamble acquisition chunk length.

   procedure Load_LDE_From_ROM
     with Global => (In_Out => DW1000.BSP.Device_State,
                     Input  => Ada.Real_Time.Clock_Time),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 null                    => Ada.Real_Time.Clock_Time);
   --  Loads the leading edge detection (LDE) microcode from ROM.
   --
   --  The LDE code must be loaded in order to use the LDE algorithm. If the
   --  LDE code is not loaded then the LDERUNE bit in the PMSC_CTRL1 register
   --  must be set to 0.
   --
   --  Note: This procedure modifies the clocks setting in PMSC_CTRL0.

   procedure Enable_Clocks (Clock : in Clocks)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Clock);
   --  Enables and configures the specified clock.
   --
   --  This procedure configures the following registers:
   --    * PMSC_CTRL0

   procedure Read_OTP (Address : in     Bits_11;
                       Word    :    out Bits_32)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => ((DW1000.BSP.Device_State, Word) => (DW1000.BSP.Device_State,
                                                     Address));
   --  Reads a 32-bit word from the DW1000 one-time programmable (OTP) memory.
   --
   --  The package DW1000.Constants defines the addresses used to store the
   --  various data stored in the OTP memory.

   procedure Read_OTP_Tx_Power_Level (Channel  : in     Channel_Number;
                                      PRF      : in     PRF_Type;
                                      Tx_Power :    out TX_POWER_Type)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Channel,
                                             PRF),
                 Tx_Power                => (DW1000.BSP.Device_State,
                                             Channel,
                                             PRF));

   procedure Read_OTP_Antenna_Delay
     (Antenna_Delay_16_MHz : out Antenna_Delay_Time;
      Antenna_Delay_64_MHz : out Antenna_Delay_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 Antenna_Delay_16_MHz    => DW1000.BSP.Device_State,
                 Antenna_Delay_64_MHz    => DW1000.BSP.Device_State);


   function To_Bits_8 (Config : in Tx_Power_Value) return Bits_8;
   --  Convert a Tx power configuration to its Bits_8 representation for use
   --  when writing to the TX_POWER register.
   --
   --  The TX_POWER register is composed of four Bits_8 fields, each of which
   --  contains 3 bits for the coarse gain setting, and 5 bits for the fine
   --  gain setting. This function converts to this Bits_8 representation.

   function Fine_Gain (Tx_Power_Bits : in Bits_8) return Fine_Tx_Power_Number;
   --  Get the fine gain setting from one of the fields in the TX_POWER
   --  register.
   --
   --  The TX_POWER register is composed of four Bits_8 fields, each of which
   --  contains 3 bits for the coarse gain setting, and 5 bits for the fine
   --  gain setting. This function gets the fine gain parameter.

   function Is_Coarse_Gain_Enabled (Tx_Power_Bits : in Bits_8) return Boolean
   is ((Tx_Power_Bits and 2#111_00000#) /= 2#111_00000#);
   --  Check if the coarse gain output is enabled or disabled.
   --
   --  The TX_POWER register is composed of four Bits_8 fields, each of which
   --  contains 3 bits for the coarse gain setting, and 5 bits for the fine
   --  gain setting. This function gets the coarse gain parameter.
   --
   --  Note that the specical value of 2#111# for the coarse gain bits in the
   --  TX_POWER register's fields means that the coarse gain output is disabled
   --  (only the fine gain is used). If the coarse gain is disabled, then this
   --  function always returns False. Otherwise, it returns True.

   function Coarse_Gain (Tx_Power_Bits : in Bits_8)
                         return Coarse_Tx_Power_Number
     with Post => (if not Is_Coarse_Gain_Enabled (Tx_Power_Bits)
                   then Coarse_Gain'Result = 0.0);
   --  Get the coarse gain setting from one of the fields in the TX_POWER
   --  register.
   --
   --  The TX_POWER register is composed of four Bits_8 fields, each of which
   --  contains 3 bits for the coarse gain setting, and 5 bits for the fine
   --  gain setting. This function gets the coarse gain parameter.
   --
   --  Note that the specical value of 2#111# for the coarse gain bits in the
   --  TX_POWER register's fields means that the coarse gain output is disabled
   --  (only the fine gain is used). If the coarse gain is disabled, then this
   --  function always returns 0.0.

   procedure Configure_Tx_Power (Config : Tx_Power_Config_Type)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Config);
   --  Configure the transmit power of the DW1000 transmitter.
   --
   --  This procedure is used to configure both smart transmit power and
   --  manual transmit power. The @Smart_Tx_Power_Enabled@ field of the
   --  @Tx_Power_Config_Type@ record determines whether or not smart tx is
   --  enabled or disabled.
   --
   --  Depending on whether or not smart transmit power is enabled or disabled
   --  the @Tx_Power_Config_Type@ record contains different fields.
   --
   --  An example of configuring a specific smart transmit power configuration
   --  is demonstrated below:
   --
   --     Configure_Tx_Power (Tx_Power_Config_Type'
   --       (Smart_Tx_Power_Enabled => True,
   --        Boost_Normal           => (Coarse_Gain_Enabled => True,
   --                                   Fine_Gain           => 10.5,
   --                                   Coarse_Gain         => 9.0),
   --        Boost_500us            => (Coarse_Gain_Enabled => True,
   --                                   Fine_Gain           => 10.5,
   --                                   Coarse_Gain         => 12.0),
   --        Boost_250us            => (Coarse_Gain_Enabled => True,
   --                                   Fine_Gain           => 10.5,
   --                                   Coarse_Gain         => 15.0),
   --        Boost_125us            => (Coarse_Gain_Enabled => True,
   --                                   Fine_Gain           => 10.5,
   --                                   Coarse_Gain         => 18.0)));
   --
   --  An example manual transmit power configuration is shown below:
   --
   --     Configure_Tx_Power (Tx_Power_Config_Type'
   --       (Smart_Tx_Power_Enabled => False,
   --        Boost_SHR              => (Coarse_Gain_Enabled => True,
   --                                   Fine_Gain           => 3.5,
   --                                   Coarse_Gain         => 9.0),
   --        Boost_PHR              => (Coarse_Gain_Enabled => True,
   --                                   Fine_Gain           => 3.5,
   --                                   Coarse_Gain         => 9.0));
   --
   --  @param Config Record containing the transmit power configuration.

   procedure Read_EUID (EUID : out Bits_64)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => ((DW1000.BSP.Device_State, EUID) => DW1000.BSP.Device_State);
   --  Read the extended unique identifier (EUID).

   procedure Write_EUID (EUID : in Bits_64)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + EUID);
   --  Write the extended unique identifier (EUID).

   procedure Read_PAN_ID (PAN_ID : out Bits_16)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => ((DW1000.BSP.Device_State, PAN_ID) => DW1000.BSP.Device_State);

   procedure Write_PAN_ID (PAN_ID : in Bits_16)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + PAN_ID);

   procedure Read_Short_Address (Short_Address : out Bits_16)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => ((DW1000.BSP.Device_State,
                 Short_Address) => DW1000.BSP.Device_State);

   procedure Write_Short_Address (Short_Address : in Bits_16)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Short_Address);

   procedure Read_PAN_ID_And_Short_Address (PAN_ID        : out Bits_16;
                                            Short_Address : out Bits_16)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => ((DW1000.BSP.Device_State,
                 PAN_ID,
                 Short_Address) => DW1000.BSP.Device_State);

   procedure Write_PAN_ID_And_Short_Address (PAN_ID        : in Bits_16;
                                             Short_Address : in Bits_16)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + (PAN_ID, Short_Address));

   procedure Read_Tx_Antenna_Delay (Antenna_Delay : out Antenna_Delay_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => ((DW1000.BSP.Device_State,
                 Antenna_Delay) => DW1000.BSP.Device_State);
   --  Read the currently configured Tx antenna delay.
   --
   --  The antenna delay is a 16-bit value using the same unit as the system
   --  time and time stamps, i.e. 499.2 MHz * 128, so the least significant
   --  bit is approximately 15.65 picoseconds.

   procedure Write_Tx_Antenna_Delay (Antenna_Delay : in Antenna_Delay_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Antenna_Delay)),
     Pre => Antenna_Delay <= (2.0**16 - 1.0) * Fine_System_Time'Delta;
   --  Set the Tx antenna delay.
   --
   --  The antenna delay is a 16-bit value using the same unit as the system
   --  time and time stamps, i.e. 499.2 MHz * 128, so the least significant
   --  bit is approximately 15.65 picoseconds.
   --
   --  This procedure configures the following registers:
   --    * TX_ANTD
   --
   --  @param Antenna_Delay The antenna delay. The maximum allowed value is
   --     1025.625 nanoseconds.

   procedure Read_Rx_Antenna_Delay (Antenna_Delay : out Antenna_Delay_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => ((DW1000.BSP.Device_State,
                 Antenna_Delay) => DW1000.BSP.Device_State);
   --  Read the currently configured Rx antenna delay.
   --
   --  The antenna delay is a 16-bit value using the same unit as the system
   --  time and time stamps, i.e. 499.2 MHz * 128, so the least significant
   --  bit is approximately 15.65 picoseconds.

   procedure Write_Rx_Antenna_Delay (Antenna_Delay : in Antenna_Delay_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Antenna_Delay)),
     Pre => Antenna_Delay <= (2.0**16 - 1.0) * Fine_System_Time'Delta;
   --  Set the Rx antenna delay.
   --
   --  The antenna delay is a 16-bit value using the same unit as the system
   --  time and time stamps, i.e. 499.2 MHz * 128, so the least significant
   --  bit is approximately 15.65 picoseconds.
   --
   --  This procedure configures the following registers:
   --    * LDE_RXANTD
   --
   --  @param Antenna_Delay The antenna delay. The maximum allowed value is
   --     1025.625 nanoseconds.

   procedure Configure_LDE (PRF              : in PRF_Type;
                            Rx_Preamble_Code : in Preamble_Code_Number;
                            Data_Rate        : in Data_Rates)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             PRF,
                                             Rx_Preamble_Code,
                                             Data_Rate));
   --  Configures the LDE subsystem for the specified pulse repetition
   --  frequency (PRF), receiver preamble code, and data rate.
   --
   --  This procedure configures the following registers:
   --    * LDE_CFG1
   --    * LDE_CFG2
   --    * LDE_REPC

   procedure Configure_PLL (Channel : in Channel_Number)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Channel));
   --  Configures the PLL subsystem for the specified UWB channel.
   --
   --  This procedure configures the following registers:
   --    * FS_PLLCFG
   --    * FS_PLLTUNE
   --    * FS_XTALT

   procedure Configure_RF (Channel : in Channel_Number)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Channel));
   --  Configures the RF subsystem for the specified UWB channel.
   --
   --  This procedure configures the following registers:
   --    * RF_RXCTRLH
   --    * RF_TXCTRL

   procedure Configure_DRX (PRF                : in PRF_Type;
                            Data_Rate          : in Data_Rates;
                            Tx_Preamble_Length : in Preamble_Lengths;
                            PAC                : in Preamble_Acq_Chunk_Length;
                            SFD_Timeout        : in SFD_Timeout_Number;
                            Nonstandard_SFD    : in Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State
                 => (DW1000.BSP.Device_State,
                     PRF,
                     Data_Rate,
                     Tx_Preamble_Length,
                     PAC,
                     SFD_Timeout,
                     Nonstandard_SFD));
   --  Configures the DRX subsystem for the specified configuration.
   --
   --  This procedure configures the following registers:
   --    * DRX_TUNE0b
   --    * DRX_TUNE1a
   --    * DRX_TUNE1b
   --    * DRX_TUNE4H
   --    * DRX_TUNE2
   --    * DRX_SFDTOC

   procedure Configure_AGC (PRF : in PRF_Type)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + PRF);
   --  Configures the automatic gain control (AGC) subsystem.
   --
   --  This procedure configures the following registers:
   --    * AGC_TUNE2
   --    * AGC_TUNE1

   procedure Configure_TC (Channel : in Channel_Number)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Channel);
   --  Configure the transmit calibration (TC) block for the specified channel.

   procedure Configure_TX_FCTRL (Frame_Length        : in Natural;
                                 Tx_Data_Rate        : in Data_Rates;
                                 Tx_PRF              : in PRF_Type;
                                 Ranging             : in Boolean;
                                 Preamble_Length     : in Preamble_Lengths;
                                 Tx_Buffer_Offset    : in Natural;
                                 Inter_Frame_Spacing : in Natural)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Frame_Length,
                                             Tx_Data_Rate,
                                             Tx_PRF,
                                             Ranging,
                                             Preamble_Length,
                                             Tx_Buffer_Offset,
                                             Inter_Frame_Spacing)),
     Pre =>
       (Frame_Length < Constants.TX_BUFFER_Length
        and then Tx_Buffer_Offset < Constants.TX_BUFFER_Length
        and then Frame_Length + Tx_Buffer_Offset <= Constants.TX_BUFFER_Length
        and then Inter_Frame_Spacing < 256);


   procedure Configure_CHAN_CTRL
     (Tx_Channel              : in Channel_Number;
      Rx_Channel              : in Channel_Number;
      Use_DecaWave_SFD        : in Boolean;
      Use_Tx_User_Defined_SFD : in Boolean;
      Use_Rx_User_Defined_SFD : in Boolean;
      Rx_PRF                  : in PRF_Type;
      Tx_Preamble_Code        : in Preamble_Code_Number;
      Rx_Preamble_Code        : in Preamble_Code_Number)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Tx_Channel,
                                             Rx_Channel,
                                             Use_DecaWave_SFD,
                                             Use_Tx_User_Defined_SFD,
                                             Use_Rx_User_Defined_SFD,
                                             Rx_PRF,
                                             Tx_Preamble_Code,
                                             Rx_Preamble_Code)),
     Pre => ((if Use_Tx_User_Defined_SFD then not Use_DecaWave_SFD)
             and (if Use_Rx_User_Defined_SFD then not Use_DecaWave_SFD));


   procedure Configure_Nonstandard_SFD_Length (Data_Rate : in Data_Rates)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Data_Rate);
   --  Configures the length of the non-standard SFD for the specified
   --  data rate.
   --
   --  This procedure configures the following registers:
   --    * USR_SFD

   procedure Configure_Non_Standard_SFD (Rx_SFD : in String;
                                         Tx_SFD : in String)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + (Rx_SFD, Tx_SFD)),
     Pre => (Rx_SFD'Length in 8 .. 16 | 64
             and Tx_SFD'Length = Rx_SFD'Length
             and (for all I in Rx_SFD'Range => Rx_SFD (I) in '+' | '-' | '0')
             and (for all I in Tx_SFD'Range => Tx_SFD (I) in '+' | '-' | '0')
            );
   --  Configure a non-standard SFD sequence.
   --
   --  WARNING: Only experts should consider designing their own SFD sequence.
   --  Designing an SFD is a complicated task, and is outside the scope of this
   --  documentation. It is strongly recommended to use either the standard
   --  defined SFD sequence, or the DecaWave defined SFD sequence.
   --
   --  The Rx_SFD and Tx_SFD strings must be strings containing only '+', '-',
   --  and '0' characters. No other characters are permitted.
   --  Below is an example of calling this procedure, using the
   --  DecaWave defined 16-symbol SFD sequence as an example SFD sequence:
   --
   --     Configure_Non_Standard_SFD (Rx_SFD => "----+-+--++--+00",
   --                                 Tx_SFD => "----+-+--++--+00");
   --
   --  Note that the Tx and Rx SFD must have the same length.
   --
   --  @param Rx_SFD The SFD sequence to use in the receiver.
   --
   --  @param Tx_SFD The SFD sequence to use in the transmitter.

   procedure Set_Frame_Filtering_Enabled (Enabled : in Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Enabled);
   --  Enable or disable frame filtering.
   --
   --  Frame filtering allows the DW1000 to automatically reject frames
   --  according to certain criterea according to the IEEE 802.15.4-2011
   --  MAC layer.
   --
   --  To configure which frames are accepted or rejected by the DW1000 see the
   --  Configure_Frame_Filtering procedure.
   --
   --  @param Enabled When set to True frame filtering is enabled. Otherwise,
   --     it is disabled.

   procedure Set_FCS_Check_Enabled (Enabled : in Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Enabled);
   --  Enable or disable the automatic frame check sequence (FCS) on received
   --  frames.
   --
   --  By default, the DW1000 automatically checks the 16-bit CRC FCS on each
   --  received frame. The last two octets in the received frame are assumed
   --  as the 16-bit CRC, and is compared against the actual FCS computed
   --  against all but the last two octets in the received frame.
   --
   --  If the DW1000 detects that the actual FCS does not match the FCS in the
   --  received frame, then it generates an FCS error. If double-buffered mode
   --  is enabled then the received frame is discarded and the buffer re-used
   --  for the next received frame.
   --
   --  This procedure enables or disables the FCS check.
   --
   --  @param Enabled When True (default after DW1000 reset) the DW1000 will
   --     check the FCS of each received frame. Set this to false to disable
   --     the FCS check for each packet.

   procedure Configure_Frame_Filtering (Behave_As_Coordinator : in Boolean;
                                        Allow_Beacon_Frame    : in Boolean;
                                        Allow_Data_Frame      : in Boolean;
                                        Allow_Ack_Frame       : in Boolean;
                                        Allow_MAC_Cmd_Frame   : in Boolean;
                                        Allow_Reserved_Frame  : in Boolean;
                                        Allow_Frame_Type_4    : in Boolean;
                                        Allow_Frame_Type_5    : in Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Behave_As_Coordinator,
                                             Allow_Beacon_Frame,
                                             Allow_Data_Frame,
                                             Allow_Ack_Frame,
                                             Allow_MAC_Cmd_Frame,
                                             Allow_Reserved_Frame,
                                             Allow_Frame_Type_4,
                                             Allow_Frame_Type_5));
   --  Configure which MAC frame types are automatically filtered by the
   --  DW1000.
   --
   --  Note that the frame filtering configuration only takes effect when
   --  frame filtering is enabled (see Set_Frame_Filtering_Enabled).
   --
   --  @param Behave_As_Coordinator When set to True the DW1000 will accept
   --     a frame without a destination address if the source address has
   --     the PAN ID matching the coordinator's PAN ID. When set to False
   --     and when filtering is enabled the DW1000 will reject these frames.
   --
   --  @param Allow_Beacon_Frame When set to True the DW1000 will accept
   --     frames whose frame type is a beacon frame. When set to False
   --     and when filtering is enabled the DW1000 will reject these frames.
   --
   --  @param Allow_Data_Frame When set to True the DW1000 will accept
   --     frames whose frame type is a data frame. When set to False
   --     and when filtering is enabled the DW1000 will reject these frames.
   --
   --  @param Allow_Ack_Frame When set to True the DW1000 will accept frames
   --     whose frame type is an acknowledgement frame. When set to False
   --     and when filtering is enabled the DW1000 will reject these frames.
   --
   --  @param Allow_MAC_Cmd_Frame When set to True the DW1000 will accept
   --     frames whose frame type is a MAC command frame. When set to False
   --     and when filtering is enabled the DW1000 will reject these frames.
   --
   --  @param Allow_Reserved_Frame When set to True the DW1000 will accept
   --     frames whose frame type is set to a reserved value (values 2#100#
   --     to 2#111#) as defined by IEEE 802.15.4-2011. When set to False
   --     and when filtering is enabled the DW1000 will reject these frames.
   --
   --  @param Allow_Frame_Type_4 When set to True the DW1000 will accept
   --     frames whose frame type is set to the value 2#100#, i.e. 4.
   --     When set to False and when frame filtering is enabled the DW1000
   --     will reject these frames.
   --
   --  @param Allow_Frame_Type_5 When set to True the DW1000 will accept
   --     frames whose frame type is set to the value 2#101#, i.e. 5.
   --     When set to False and when frame filtering is enabled the DW1000
   --     will reject these frames.

   procedure Set_Smart_Tx_Power (Enabled : in Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Enabled);
   --  Enables or disables smart Tx power control.
   --
   --  Regulations for UWB typically specify a maximum transmit power limit of
   --  -41.3 dBm / MHz, typically measured with a dwell time of 1 ms. Short
   --  frames transmitted at a data rate of 6.8 Mbps and a short preamble
   --  length are transmitted in a fraction of a millisecond. If only a single
   --  short frame is transmitted with in 1 ms then the frame can be
   --  transmitted at a higher power than the -41.3 dBm / MHz regulatory limit.
   --
   --  When the smart tx power control is enabled then the DW1000 will
   --  boost the power for short transmissions. It is the user's responsibility
   --  to avoid sending multiple short frames within the same millisecond to
   --  remain within the regulatory limits.
   --
   --  This procedure configures the following registers:
   --    * SYS_CFG


   procedure Set_Tx_Data (Data   : in Types.Byte_Array;
                          Offset : in Natural)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State
                 => (DW1000.BSP.Device_State,
                     Data,
                     Offset)),
     Pre =>
       (Data'Length > 0
        and then Data'Length <= DW1000.Constants.TX_BUFFER_Length
        and then Offset < DW1000.Constants.TX_BUFFER_Length
        and then Data'Length + Offset <= DW1000.Constants.TX_BUFFER_Length);
   --  Write data to the DW1000 TX buffer.
   --
   --  Before starting the transmission, the frame length and offset must be
   --  programmed into the DW1000 separately using the Set_Tx_Frame_Length
   --  procedure.
   --
   --  The frame is not transmitted until the Start_Tx procedure is called.
   --
   --  This procedure configures the following registers:
   --    * TX_BUFFER

   procedure Set_Tx_Frame_Length (Length : in Natural;
                                  Offset : in Natural)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + (Length, Offset)),
     Pre => (Length < DW1000.Constants.TX_BUFFER_Length
             and then Offset < DW1000.Constants.TX_BUFFER_Length
             and then Length + Offset <= DW1000.Constants.TX_BUFFER_Length);
   --  Configures the frame length and offset within the transmit buffer
   --  (TX_BUFFER) to use when transmitting the next packet.
   --
   --  This procedure configures the following registers:
   --    * TX_FCTRL

   procedure Read_Rx_Data (Data   :    out Types.Byte_Array;
                           Offset : in     Natural)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + (Offset, Data),
                 Data                    => (DW1000.BSP.Device_State, Offset)),
     Pre =>
       (Data'Length > 0
        and then Data'Length <= DW1000.Constants.RX_BUFFER_Length
        and then Offset < DW1000.Constants.RX_BUFFER_Length
        and then Data'Length + Offset <= DW1000.Constants.RX_BUFFER_Length);
   --  Read the received frame from the Rx buffer.

   procedure Start_Tx_Immediate (Rx_After_Tx : in Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Rx_After_Tx);

   procedure Start_Tx_Delayed (Rx_After_Tx : in     Boolean;
                               Result      :    out Result_Type)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Rx_After_Tx,
                 Result                  => (DW1000.BSP.Device_State,
                                             Rx_After_Tx));
   --  Transmit the contents of the TX buffer with a delay.
   --
   --  The time at which the packet is to be transmitted must be set before
   --  calling this procedure by using the Set_Delayed_Tx_Rx_Time procedure.
   --
   --  When Rx_After_Tx is True then the receiver is automatically enabled
   --  after the transmission is completed.
   --
   --  This procedure configures the following registers:
   --    * SYS_CTRL

   procedure Set_Delayed_Tx_Rx_Time (Delay_Time : in Coarse_System_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Delay_Time);
   --  Set the receive and transmit delay.
   --
   --  Both Rx and Tx share the same delay. It is not possible for the receiver
   --  and transmitter to use different delays simultaneously.
   --
   --  The delay time is measured in units of 499.2 MHz * 128, i.e. the least
   --  significant bit of the delay time is approximately 15.65 ps.
   --
   --  Note that the 9 low order bits of the input value are ignored by the
   --  DW1000, as described in Section 7.2.12 of the DW1000 User Manual
   --  (DX_TIME register).
   --
   --  This procedure configures the following registers:
   --    * DX_TIME

   procedure Set_Sleep_After_Tx (Enabled : in Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Enabled);
   --  Configures the DW1000 to enter sleep more (or not) after transmitting a
   --  frame.
   --
   --  When Enable is True, the DW1000 will automatically enter sleep mode
   --  after each frame is sent. Otherwise, when Enable is False the DW1000
   --  will not enter sleep mode after each frame is sent.
   --
   --  This procedure configures the following registers:
   --    * PMSC_CTRL1

   procedure Read_Rx_Adjusted_Timestamp (Timestamp : out Fine_System_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 Timestamp               => DW1000.BSP.Device_State);
   --  Read the corrected timestamp associated with the last received packet.
   --
   --  This timestamp is the timestamp that has been fully corrected for the
   --  time of packet reception. The timestamp is in units of approximately
   --  15.65 picoseconds.

   procedure Read_Rx_Raw_Timestamp (Timestamp : out Coarse_System_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 Timestamp               => DW1000.BSP.Device_State);
   --  Read the raw timestamp associated with the last received packet.
   --
   --  This timestamp is the timestamp before the various corrections for the
   --  time of reception have been applied. The timestamp is in units of
   --  approximately 8.013 nanoseconds.

   procedure Read_Rx_Timestamps (Adjusted : out Fine_System_Time;
                                 Raw      : out Coarse_System_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 (Adjusted, Raw)         => DW1000.BSP.Device_State);
   --  Read both the raw and adjusted timestamps for the last received packet.

   procedure Read_Tx_Adjusted_Timestamp (Timestamp : out Fine_System_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 Timestamp               => DW1000.BSP.Device_State);
   --  Read the corrected timestamp associated with the last transmitted
   --  packet.
   --
   --  This timestamp is the timestamp that has been fully corrected for the
   --  time of packet transmission. The timestamp is in units of approximately
   --  15.65 picoseconds.

   procedure Read_Tx_Raw_Timestamp (Timestamp : out Coarse_System_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 Timestamp               => DW1000.BSP.Device_State);
   --  Read the raw timestamp associated with the last transmitted packet.
   --
   --  This timestamp is the timestamp before the various corrections for the
   --  time of transmission have been applied. The timestamp is in units of
   --  approximately 8.013 nanoseconds.

   procedure Read_Tx_Timestamps (Adjusted : out Fine_System_Time;
                                 Raw      : out Coarse_System_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 (Adjusted, Raw)         => DW1000.BSP.Device_State);

   procedure Read_System_Timestamp (Timestamp : out Coarse_System_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 Timestamp               => DW1000.BSP.Device_State);
   --  Read the current value of the DW1000's system timestamp.
   --
   --  The timestamp is measured in units of 499.2 MHz * 128, i.e. the least
   --  significant bit of the timestamp is approximately 15.65 ps.

   procedure Check_Overrun (Overrun : out Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 Overrun                 => DW1000.BSP.Device_State);
   --  Check if an overrun condition has occurred.
   --
   --  An overrun condition occurs if the DW1000 receives a new packet before
   --  the host processor has been able to read the previously received packet.
   --
   --  See Section 4.3.5 of the DW1000 User Manual for more information of the
   --  overrun condition.

   procedure Force_Tx_Rx_Off
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State);
   --  Force off the tranceiver.
   --
   --  This also clears the status registers.
   --
   --  Turning off the tranceiver will cancel any pending receive or
   --  transmit operation.

   procedure Reset_Rx
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State);
   --  Perform a soft reset of the receiver only.

   procedure Toggle_Host_Side_Rx_Buffer_Pointer
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State);
   --  Toggle the host side receive buffer pointer (HSRBP).
   --
   --  This procedure is only relevant when double-buffer mode is enabled.
   --  Calling this procedure signals to the DW1000 that the host IC is
   --  finished with the contents of the current double-buffered set.
   --
   --  It should be called after the host IC has finished reading the receive
   --  registers after a packet has been received.

   procedure Sync_Rx_Buffer_Pointers
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State);
   --  Synchronize the Rx buffer pointers for double-buffer operation.
   --
   --  This procedure synchronizes the ICRBP and HSRBP bits in the SYS_CTRL
   --  register so that they are the same. This is only relevant when the
   --  DW1000 is operating in double-buffer mode.
   --
   --  This procedure configures the following registers:
   --    * SYS_CTRL

   procedure Start_Rx_Immediate
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State);
   --  Enable the receiver immediately (without a delay).
   --
   --  This procedure configures the following registers:
   --    * SYS_CTRL

   procedure Start_Rx_Delayed (Result  : out Result_Type)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                 Result                  => DW1000.BSP.Device_State);
   --  Enable the receiver after a delay.
   --
   --  The receiver is enabled only at the time configured by calling the
   --  Set_Tx_Rx_Delay_Time procedure, which must be set before calling this
   --  procedure.
   --
   --  This procedure configures the following registers:
   --    * SYS_CTRL

   procedure Set_Rx_Mode (Mode        : in Rx_Modes;
                          Rx_On_Time  : in Types.Bits_4;
                          Rx_Off_Time : in Coarse_System_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + (Mode, Rx_On_Time, Rx_Off_Time)),
     Pre =>
       ((if Mode = Sniff then Rx_Off_Time > 0.0)
        and Rx_Off_Time < Coarse_System_Time'Delta * (128.0 * 2.0**8));
   --  Enables or disables the receiver sniff mode.
   --
   --  When Mode is set to Normal then when the receiver is turned on (see
   --  the Enable_Rx procedure) then it will operate until either a frame is
   --  received, or until the receiver timeout time is reached. In the Normal
   --  mode the Rx_On_Time and Rx_Off_Time parameters are not used.
   --
   --  When Mode is set to Sniff then the receiver will be activated for the
   --  duration of the Rx_On_Time, searching for a preamble. If a preamble is
   --  detected within this duration then the receiver continues operation to
   --  try to receive the packet. Otherwise, if no preamble is detected then
   --  the receiver is then disabled for the Rx_Off_Time, after which it is
   --  re-enabled to repeat the process.
   --
   --  The Rx_On_Time is measured in units of the preamble acquisition count
   --  (PAC) see Section 4.1.1 of the DW100 User Manual for more information.
   --
   --  The Rx_Off_Time is measured in units of the 128 system clock cycles, or
   --  approximately 1 us. If the Mode is set to Sniff then the Rx_Off_Time
   --  must be non-zero (a value of 0 would disable the sniff mode on the
   --  DW1000).
   --
   --  The Rx_Off_Time must be less than 15.385 microseconds.
   --
   --  This procedure configures the following registers:
   --    * RX_SNIFF

   procedure Set_Auto_Rx_Reenable (Enabled : in Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Enabled);
   --  Enable or disable the Rx auto re-enable feature.
   --
   --  This feature has different behaviour depending on whether or not the
   --  receiver is operating in double-buffer mode.
   --
   --  When Rx auto re-enable is disabled the receiver will stop receiving
   --  when any receive event happens (e.g. an error occurred, or a frame
   --  was received OK).
   --
   --  When Rx auto re-enable is enabled then the receiver behaviour
   --  depends on the double-buffer configuration:
   --    * In single-buffer mode the receiver is automatically re-enabled
   --      after a receive error occurs (e.g. physical header error),
   --      EXCEPT a frame wait timeout error.
   --    * In double-buffer mode the receiver is automatically re-enabled
   --      when a frame is received, or when an error occurs (e.g. physical
   --      header error), EXCEPT a frame wait timeout error.
   --
   --  This procedure configures the following registers:
   --    * SYS_CFG

   procedure Set_Rx_Double_Buffer (Enabled : in Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Enabled);
   --  Configures double-buffer mode.
   --
   --  By default the DW1000 operates in single-buffer mode. Double-buffer
   --  mode can be enabled to allow the host application to read the previously
   --  received frame at the same time as the DW1000 is receiving the next
   --  frame.
   --
   --  Also see the Sync_Rx_Buffer_Pointers procedure.
   --
   --  This procedure configures the following registers:
   --    * SYS_CFG

   procedure Set_Rx_Frame_Wait_Timeout (Timeout : in Frame_Wait_Timeout_Time)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Timeout);
   --  Configure the receive timeout.
   --
   --  When the receiver is enabled the receive timeout is started.
   --  If no complete frame is received within the configured Rx timeout
   --  then the receiver is automatically disabled.
   --
   --  The Rx timeout can be disabled by setting the Timeout to 0.0.
   --
   --  The Rx timeout is measured in units of 499.2 MHz / 512, i.e. in units
   --  of approximately 1.026 us. The maximum timeout is approximately
   --  67.215385 ms.
   --
   --  This procedure configures the following registers:
   --    * SYS_CFG
   --
   --  @param Timeout The maximum time (in seconds) to wait for a frame.
   --     E.g. a value of 0.001 is 1 millisecond. The maximum permitted value
   --     is 0.067_215_385, i.e. a little over 67 milliseconds.

   procedure Set_Preamble_Detect_Timeout (Timeout : in Types.Bits_16)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Timeout);
   --  Configure the preamble detection timeout.
   --
   --  When the receiver is enabled the preamble timeout is started.
   --  If no preamble is detected within the configured preamble detection
   --  timeout then the receiver is automatically disabled.
   --
   --  The preamble detect timeout can be disabled by setting the Timeout to 0.
   --
   --  The preamble detect timeout is measured in units of preamble acquisition
   --  chunk (PAC) size, which can be 8, 16, 32, or 64. See Section 7.2.40.9 of
   --  the DW1000 User Manual for more information.
   --
   --  This procedure configures the following registers:
   --    * DRX_PRETOC

   procedure Calibrate_Sleep_Count
     (Half_XTAL_Cycles_Per_LP_Osc_Cycle : out Types.Bits_16)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State           => DW1000.BSP.Device_State,
                 Half_XTAL_Cycles_Per_LP_Osc_Cycle => DW1000.BSP.Device_State);

   procedure Upload_AON_Config
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State);
   --  Upload the AON block configurations to the AON.
   --
   --  This uploads the configuration from the AON_CFG0 and AON_CFG1 registers
   --  into the AON block.

   procedure Save_Registers_To_AON
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State);
   --  Copy the user configurations from the host interface register set into
   --  the AON memory.
   --
   --  If enabled to do so, after exiting sleep mode the DW1000 will reload the
   --  user configuration from the AON memory into the host interface register
   --  set.
   --
   --  The behaviour of the AON subsystem when exiting sleep or deep-sleep
   --  states can be configured via the AON_WCFG register.

   procedure Restore_Registers_From_AON
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State);
   --  Load the user configuration from the AON memory into the host interface
   --  register set.

   procedure AON_Read_Byte (Address : in     Types.Bits_8;
                            Data    :    out Types.Bits_8)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends =>
       (DW1000.BSP.Device_State => (DW1000.BSP.Device_State, Address),
        Data                    => (DW1000.BSP.Device_State, Address));
   -- Reads a single byte from the Always-On block.

   procedure AON_Contiguous_Read (Start_Address : in     Types.Bits_8;
                                  Data          :    out Types.Byte_Array)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Start_Address,
                                             Data),
                 Data                    => + (DW1000.BSP.Device_State,
                                               Start_Address)),
     Pre => (Data'Length <= 256
             and then Natural (Start_Address) + Data'Length <= 256);
   -- Reads a contiguous sequence of bytes from the Always-On block.

   procedure AON_Scatter_Read (Addresses : in     Types.Byte_Array;
                               Data      :    out Types.Byte_Array)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Addresses,
                                             Data),
                 Data                    => + (DW1000.BSP.Device_State,
                                               Addresses)),
     Pre => Addresses'Length = Data'Length;
   --  Reads a non-contiguous set of bytes from the Always-on block.
   --
   --  This procedure reads bytes from the sequence of addresses in the
   --  Addresses array, and stores the byte that was read in the corresponding
   --  position in the Data array.

   procedure Configure_Sleep_Count (Sleep_Count : in Types.Bits_16)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Sleep_Count);

   procedure Set_XTAL_Trim (Trim : in Types.Bits_5)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => + Trim);

   procedure Configure_LEDs (Tx_LED_Enable    : in Boolean;
                             Rx_LED_Enable    : in Boolean;
                             Rx_OK_LED_Enable : in Boolean;
                             SFD_LED_Enable   : in Boolean;
                             Test_Flash       : in Boolean)
     with Global => (In_Out => DW1000.BSP.Device_State),
     Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                             Tx_LED_Enable,
                                             Rx_LED_Enable,
                                             Rx_OK_LED_Enable,
                                             SFD_LED_Enable,
                                             Test_Flash));
   --  Configure the behaviour of the LEDs.
   --
   --  @param Tx_LED_Enable When set to True the DW1000 will flash the Tx LED
   --     while the transmitter is on.
   --
   --  @param Rx_LED_Enable When set to True the DW1000 will flash the Rx LED
   --     while the receiver is on.
   --
   --  @param Rx_OK_LED_Enable When set to True the DW1000 will flash the LED
   --     when a packet is received without errors.
   --
   --  @param SFD_LED_Enable When set to True the DW1000 will flash the LED
   --     when an SFD is detected.
   --
   --  @param Test_Flash When set to True the DW1000 will flash the configured
   --     LEDs once, immediately after the LEDs are configured.

end DW1000.Driver;
