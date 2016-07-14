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

with Ada.Unchecked_Conversion;
with DW1000.Constants;       use DW1000.Constants;
with DW1000.Registers;       use DW1000.Registers;
with DW1000.Register_Driver;


package body DW1000.Driver
with SPARK_Mode => On
is

   -- These values for LDE_CFG1 are given by the user manual
   LDE_CFG1_Value  : constant LDE_CFG1_Type
     := (NTM   => 13,
         PMULT => 3);

   -- These values for LDE_CFG2 are given by the user manual
   LDE_CFG2_Values : constant array (PRF_Type) of Types.Bits_16
     := (PRF_16MHz => 16#1607#,
         PRF_64MHz => 16#0607#);

   -- These values for FS_PLLCFG are given by the user manual
   FS_PLLCFG_Values : constant array (Positive range 1 .. 7) of Types.Bits_32
     := (1   => 16#09000407#,
         2|4 => 16#08400508#,
         3   => 16#08401009#,
         5|7 => 16#0800041D#,
         -- Note that channel 6 is not a valid channel. However, Channel_Number
         -- cannot be used as the array index type since it has a predicate.
         6   => 0);

   -- These values for FS_PLLTUNE are given by the user manual
   FS_PLLTUNE_Values : constant array (Positive range 1 .. 7) of Types.Bits_8
     := (1   => 16#1E#,
         2|4 => 16#26#,
         3   => 16#5E#,
         5|7 => 16#BE#,
         -- Note that channel 6 is not a valid channel. However, Channel_Number
         -- cannot be used as the array index type since it has a predicate.
         6   => 0);

   -- These values for FS_PLLCFG are ported from the C decadriver
   FS_XTALT_Value : constant FS_XTALT_Type
     := (XTALT    => 16,
         Reserved => 2#011#);

   -- These values for RF_TXCTRL are given by the user manual
   RF_TXCTRL_Values : constant array (Positive range 1 .. 7) of Types.Bits_32
     := (1 => 16#00006C50#,
         2 => 16#00056CA0#,
         3 => 16#00086CC0#,
         4 => 16#00045C80#,
         5 => 16#001E3FE0#,
         7 => 16#001E7DE0#,
         -- Note that channel 6 is not a valid channel. However, Channel_Number
         -- cannot be used as the array index type since it has a predicate.
         6 => 0);

   -- These values for RF_RXCTRLH are given by the user manual
   RF_RXCTRLH_Values : constant array (Positive range 1 .. 7) of Types.Bits_8
     := (1..3|5 => 16#D8#,
         4|7    => 16#BC#,
         -- Note that channel 6 is not a valid channel. However, Channel_Number
         -- cannot be used as the array index type since it has a predicate.
         6      => 0);

   -- These values for DRX_TUNE0b are given by the user manual
   DRX_TUNE0b_Values : constant array (Data_Rates, Boolean) of Types.Bits_16
     := (Data_Rate_110k =>
           (False => 16#000A#, -- standard SFD
            True  => 16#0016#),-- non-standard SFD
         Data_Rate_850k =>
           (False => 16#0001#, -- standard SFD
            True  => 16#0006#),-- non-standard SFD
         Data_Rate_6M8  =>
           (False => 16#0001#, -- standard SFD
            True  => 16#0002#) -- non-standard SFD
        );

   -- These values for DRX_TUNE1a are given by the user manual
   DRX_TUNE1a_Values : constant array (PRF_Type) of Types.Bits_16
     := (PRF_16MHz => 16#0087#,
         PRF_64MHz => 16#008D#);

   -- These values for DRX_TUNE2 are given by the user manual
   DRX_TUNE2_Values : constant array (Preamble_Acq_Chunk_Length,
                                      PRF_Type) of Types.Bits_32
     := (PAC_8  =>
           (PRF_16MHz => 16#311A002D#,
            PRF_64MHz => 16#313B006B#),
         PAC_16 =>
           (PRF_16MHz => 16#331A0052#,
            PRF_64MHz => 16#333B00BE#),
         PAC_32 =>
           (PRF_16MHz => 16#351A009A#,
            PRF_64MHz => 16#353B015E#),
         PAC_64 =>
           (PRF_16MHz => 16#371A011D#,
            PRF_64MHz => 16#373B0296#)
        );

   -- These values for AGC_TUNE1 are given by the user manual
   AGC_TUNE1_Values : constant array (PRF_Type) of Types.Bits_16
     := (PRF_16MHz => 16#8870#,
         PRF_64MHz => 16#889B#);

   -- This value for AGC_TUNE2 is given by the user manual
   AGC_TUNE2_Value : constant Types.Bits_32 := 16#2502A907#;

   -- These values for TC_PGDELAY are given by the user manual
   TC_PGDELAY_Values : constant array (Positive range 1 .. 7) of Bits_8
     := (1 => 16#C9#,
         2 => 16#C2#,
         3 => 16#C5#,
         4 => 16#95#,
         5 => 16#C0#,
         6 => 0,      --  Channel 6 not in Channel_Number
         7 => 16#93#);

   -- This value for non-standard SFD lengths are given by the user manual
   Non_Standard_SFD_Lengths : constant array (Data_Rates) of Types.Bits_8
     := (Data_Rate_110k => 64,
         Data_Rate_850k => 16,
         Data_Rate_6M8  => 8);

   procedure Load_LDE_From_ROM
   is
      use type Ada.Real_Time.Time;

      Finish_Time : Ada.Real_Time.Time;
      Now         : Ada.Real_Time.Time;

      PMSC_CTRL0_Reg : Register_Types.PMSC_CTRL0_Type;

   begin
      --  Set up clocks
      PMSC_CTRL0.Read (PMSC_CTRL0_Reg);
      PMSC_CTRL0_Reg.SYSCLKS := 1;
      PMSC_CTRL0_Reg.RXCLKS  := 0;
      PMSC_CTRL0_Reg.TXCLKS  := 0;
      PMSC_CTRL0_Reg.FACE    := 0;
      PMSC_CTRL0_Reg.ADCCE   := 0;
      PMSC_CTRL0_Reg.Reserved_1 := 2#110#;
      --  Writing to these Reserved bits is undocumented in the User Manual,
      --  but the DecaWave C code sets these bits, so it serves some purpose.
      PMSC_CTRL0.Write (PMSC_CTRL0_Reg);

      --  Kick off the NV MEM load
      OTP_CTRL.Write (OTP_CTRL_Type'
                        (OTPRDEN    => 0,
                         OTPREAD    => 0,
                         OTPMRWR    => 0,
                         OTPPROG    => 0,
                         OTPMR      => 0,
                         LDELOAD    => 1,
                         Reserved_1 => 0,
                         Reserved_2 => 0,
                         Reserved_3 => 0));

      --  Code upload takes up to 150 us
      --  A busy wait is used here to prevent blocking the calling task.
      --  This permits calling this function from a protected object.

      pragma Warnings (GNATprove, Off, "unused assignment",
                       Reason => "Assignment used for non-blocking delay");
      Finish_Time := Ada.Real_Time.Clock;
      Finish_Time := Finish_Time + Ada.Real_Time.Microseconds (150);
      loop
         Now := Ada.Real_Time.Clock;

         pragma Warnings (GNATprove, Off, "statement has no effect",
                          Reason => "delay required for LDE code load");
         exit when Now >= Finish_Time;
      end loop;

      pragma Warnings (GNATprove, On, "unused assignment");
      pragma Warnings (GNATprove, On, "statement has no effect");

      --  Default clocks
      PMSC_CTRL0_Reg.SYSCLKS    := 0;
      PMSC_CTRL0_Reg.Reserved_1 := 2#100#;
      PMSC_CTRL0.Write (PMSC_CTRL0_Reg);
   end Load_LDE_From_ROM;

   procedure Enable_Clocks (Clock : in Clocks)
   is
      PMSC_CTRL0_Reg : PMSC_CTRL0_Type;

   begin

      PMSC_CTRL0.Read (PMSC_CTRL0_Reg);

      case Clock is
         when Enable_All_Seq =>
            PMSC_CTRL0_Reg.SYSCLKS := 2#00#;
            PMSC_CTRL0_Reg.RXCLKS  := 2#00#;
            PMSC_CTRL0_Reg.TXCLKS  := 2#00#;
            PMSC_CTRL0_Reg.FACE    := 0;

            -- Need to write the above changes before setting GPCE
            PMSC_CTRL0.Write (PMSC_CTRL0_Reg);

            PMSC_CTRL0_Reg.Reserved_1 := PMSC_CTRL0_Reg.Reserved_1 and 2#100#;
            --  Writing to the reserved bits is undocumented in the User Manual
            --  but the DecaWave C code masks these bits.

         when Force_Sys_XTI =>
            PMSC_CTRL0_Reg.SYSCLKS := 2#01#;

         when Force_Sys_PLL =>
            PMSC_CTRL0_Reg.SYSCLKS := 2#10#;

         when Read_Acc_On =>
            PMSC_CTRL0_Reg.RXCLKS    := 2#10#;
            PMSC_CTRL0_Reg.FACE      := 1;

            -- Need to write the above changes before setting SOFTRESET
            PMSC_CTRL0.Write (PMSC_CTRL0_Reg);

            PMSC_CTRL0_Reg.SOFTRESET := PMSC_CTRL0_Reg.SOFTRESET or 2#1000#;

         when Read_Acc_Off =>
            PMSC_CTRL0_Reg.RXCLKS    := 2#00#;
            PMSC_CTRL0_Reg.FACE      := 0;

            -- Need to write the above changes before clearing SOFTRESET
            PMSC_CTRL0.Write (PMSC_CTRL0_Reg);

            PMSC_CTRL0_Reg.SOFTRESET := PMSC_CTRL0_Reg.SOFTRESET and 2#0111#;

         when Force_OTP_On =>
            PMSC_CTRL0_Reg.Reserved_1 := PMSC_CTRL0_Reg.Reserved_1 or 2#100#;

         when Force_OTP_Off =>
            PMSC_CTRL0_Reg.Reserved_1 := PMSC_CTRL0_Reg.Reserved_1 and 2#011#;

         when Force_Tx_PLL =>
            PMSC_CTRL0_Reg.TXCLKS := 2#10#;

      end case;

      PMSC_CTRL0.Write (PMSC_CTRL0_Reg);

   end Enable_Clocks;


   procedure Read_OTP (Address : in     Bits_11;
                       Word    :    out Bits_32)
   is
      CTRL_Reg : OTP_CTRL_Type;
      RDAT_Reg : OTP_RDAT_Type;

   begin
      -- Set OTP address to read
      OTP_ADDR.Write (OTP_ADDR_Type'(OTP_ADDR => Address,
                                     Reserved => 0));

      -- Trigger OTP read
      CTRL_Reg := OTP_CTRL_Type'
        (OTPRDEN    => 1,
         OTPREAD    => 1,
         OTPMRWR    => 0,
         OTPPROG    => 0,
         OTPMR      => 0,
         LDELOAD    => 0,
         Reserved_1 => 0,
         Reserved_2 => 0,
         Reserved_3 => 0);

      OTP_CTRL.Write (CTRL_Reg);

      -- OTPRDEN is not self-clearing. Also clear OPTREAD
      CTRL_Reg.OTPRDEN := 0;
      CTRL_Reg.OTPREAD := 0;
      OTP_CTRL.Write (CTRL_Reg);

      -- Read back the OTP word
      OTP_RDAT.Read (RDAT_Reg);
      Word := RDAT_Reg.OTP_RDAT;

   end Read_OTP;


   procedure Read_EUID (EUID : out Bits_64)
   is
      EUI_Reg : EUI_Type;

   begin
      EUI.Read (EUI_Reg);

      EUID := EUI_Reg.EUI;
   end Read_EUID;

   procedure Write_EUID (EUID : in Bits_64)
   is
   begin
      EUI.Write ( (EUI => EUID) );
   end Write_EUID;

   procedure Read_Tx_Antenna_Delay (Antenna_Delay : out Antenna_Delay_Time)
   is
      TX_ANTD_Reg : TX_ANTD_Type;

   begin
      TX_ANTD.Read (TX_ANTD_Reg);

      Antenna_Delay := To_Antenna_Delay_Time (TX_ANTD_Reg.TX_ANTD);
   end Read_Tx_Antenna_Delay;



   procedure Write_Tx_Antenna_Delay (Antenna_Delay : in Antenna_Delay_Time)
   is
   begin
      TX_ANTD.Write
        ( (TX_ANTD => To_Bits_16 (Antenna_Delay)) );
   end Write_Tx_Antenna_Delay;



   procedure Read_Rx_Antenna_Delay (Antenna_Delay : out Antenna_Delay_Time)
   is
      LDE_RXANTD_Reg : LDE_RXANTD_Type;

   begin
      LDE_RXANTD.Read (LDE_RXANTD_Reg);

      Antenna_Delay := To_Antenna_Delay_Time (LDE_RXANTD_Reg.LDE_RXANTD);
   end Read_Rx_Antenna_Delay;



   procedure Write_Rx_Antenna_Delay (Antenna_Delay : in Antenna_Delay_Time)
   is
   begin
      LDE_RXANTD.Write
        ( (LDE_RXANTD => To_Bits_16 (Antenna_Delay)) );
   end Write_Rx_Antenna_Delay;


   procedure Configure_LDE (PRF : in PRF_Type)
   is
   begin
      LDE_CFG1.Write (LDE_CFG1_Value);
      LDE_CFG2.Write (LDE_CFG2_Type'(LDE_CFG2 => LDE_CFG2_Values (PRF)));

   end Configure_LDE;

   procedure Configure_PLL (Channel : in Channel_Number)
   is
   begin
      FS_PLLCFG.Write ( (FS_PLLCFG => FS_PLLCFG_Values (Positive (Channel))) );

      FS_PLLTUNE.Write
        ( (FS_PLLTUNE => FS_PLLTUNE_Values (Positive (Channel))) );

      FS_XTALT.Write (FS_XTALT_Value);
   end Configure_PLL;

   procedure Configure_RF (Channel : in Channel_Number)
   is
   begin
      RF_RXCTRLH.Write
        (RF_RXCTRLH_Type'
           (RF_RXCTRLH => RF_RXCTRLH_Values (Positive (Channel)))
        );

      RF_TXCTRL.Write
        (RF_TXCTRL_Type'
           (RF_TXCTRL => RF_TXCTRL_Values (Positive (Channel)))
        );

   end Configure_RF;

   procedure Configure_DRX (PRF                : in PRF_Type;
                            Data_Rate          : in Data_Rates;
                            Tx_Preamble_Length : in Preamble_Lengths;
                            PAC                : in Preamble_Acq_Chunk_Length;
                            SFD_Timeout        : in SFD_Timeout_Number;
                            Nonstandard_SFD    : in Boolean)
   is
   begin
      DRX_TUNE0b.Write
        (DRX_TUNE0b_Type'
           (DRX_TUNE0b => DRX_TUNE0b_Values (Data_Rate, Nonstandard_SFD))
        );

      DRX_TUNE1a.Write
        (DRX_TUNE1a_Type'
           (DRX_TUNE1a => DRX_TUNE1a_Values (PRF))
        );

      if Data_Rate = Data_Rate_110k then
         DRX_TUNE1b.Write
           (DRX_TUNE1b_Type'
              (DRX_TUNE1b => 16#0064#)
           );

      elsif Tx_Preamble_Length = PLEN_64 then
         DRX_TUNE1b.Write
           (DRX_TUNE1b_Type'
              (DRX_TUNE1b => 16#0010#)
           );

         DRX_TUNE4H.Write
           (DRX_TUNE4H_Type'
              (DRX_TUNE4H => 16#0010#)
           );
      else
         DRX_TUNE1b.Write
           (DRX_TUNE1b_Type'
              (DRX_TUNE1b => 16#0020#)
           );

         DRX_TUNE4H.Write
           (DRX_TUNE4H_Type'
              (DRX_TUNE4H => 16#0028#)
           );
      end if;

      DRX_TUNE2.Write
        (DRX_TUNE2_Type'
           (DRX_TUNE2 => DRX_TUNE2_Values (PAC, PRF))
        );

      DRX_SFDTOC.Write
        (DRX_SFDTOC_Type'
           (DRX_SFDTOC => Types.Bits_16 (SFD_Timeout))
        );
   end Configure_DRX;


   procedure Configure_AGC (PRF : in PRF_Type)
   is
   begin

      AGC_TUNE2.Write
        (AGC_TUNE2_Type'
           (AGC_TUNE2 => AGC_TUNE2_Value)
        );

      AGC_TUNE1.Write
        (AGC_TUNE1_Type'
           (AGC_TUNE1 => AGC_TUNE1_Values (PRF))
        );

   end Configure_AGC;

   procedure Configure_TC (Channel : in Channel_Number)
   is
   begin
      TC_PGDELAY.Write
        ((TC_PGDELAY => TC_PGDELAY_Values (Positive (Channel))));
   end Configure_TC;

   procedure Configure_Nonstandard_SFD_Length (Data_Rate : in Data_Rates)
   is
      USR_SFD_Reg : Register_Types.USR_SFD_Type;

   begin
      USR_SFD.Read (USR_SFD_Reg);
      USR_SFD_Reg.Sub_Registers (0) := Non_Standard_SFD_Lengths (Data_Rate);
      USR_SFD.Write (USR_SFD_Reg);
   end Configure_Nonstandard_SFD_Length;

   procedure Configure_Non_Standard_SFD (Rx_SFD : in String;
                                         Tx_SFD : in String)
   is

      --  Takes a string of up to 8 SFD symbols and compute the value of one
      --  of the USR_SFD register's magnitude sub-registers (8 bits).
      function Magnitude (SFD : in String) return Types.Bits_8
        with Pre =>
          (SFD'Length in 1 .. 8
           and (for all I in SFD'Range => SFD (I) in '+' | '-' | '0'))
      is
         Result : Bits_8 := 0;

      begin
         for I in Natural range 0 .. SFD'Length - 1 loop
            if SFD (SFD'First + I) /= '0' then
               Result := Result or Shift_Left (Bits_8 (1), I);
            end if;
         end loop;

         return Result;
      end Magnitude;

      function Polarity (SFD : in String) return Types.Bits_8
        with Pre =>
          (SFD'Length in 1 .. 8
           and (for all I in SFD'Range => SFD (I) in '+' | '-' | '0'))
      is
         Result : Bits_8 := 0;

      begin
         for I in Natural range 0 .. SFD'Length - 1 loop
            if SFD (SFD'First + I) = '-' then
               Result := Result or Shift_Left (Bits_8 (1), I);
            end if;
         end loop;

         return Result;
      end Polarity;

      USR_SFD_Reg : USR_SFD_Type;

   begin
      if Tx_SFD'Length = 8 then
         USR_SFD_Reg.Sub_Registers :=
           (0 => Bits_8 (Tx_SFD'Length),
            1 => Magnitude (Tx_SFD),
            3 => Polarity  (Tx_SFD),
            5 => Magnitude (Rx_SFD),
            7 => Polarity  (Rx_SFD),
            others => 0);

      elsif Tx_SFD'Length <= 16 then
         USR_SFD_Reg.Sub_Registers :=
           (0 => Bits_8 (Tx_SFD'Length),
            1 => Magnitude (Tx_SFD (Tx_SFD'First     .. Tx_SFD'First + 7)),
            2 => Magnitude (Tx_SFD (Tx_SFD'First + 8 .. Tx_SFD'Last     )),
            3 => Polarity  (Tx_SFD (Tx_SFD'First     .. Tx_SFD'First + 7)),
            4 => Polarity  (Tx_SFD (Tx_SFD'First + 8 .. Tx_SFD'Last     )),
            5 => Magnitude (Rx_SFD (Rx_SFD'First     .. Rx_SFD'First + 7)),
            6 => Magnitude (Rx_SFD (Rx_SFD'First + 8 .. Rx_SFD'Last     )),
            7 => Polarity  (Rx_SFD (Rx_SFD'First     .. Rx_SFD'First + 7)),
            8 => Polarity  (Rx_SFD (Rx_SFD'First + 8 .. Rx_SFD'Last     )),
            others => 0);

      else
         USR_SFD_Reg.Sub_Registers :=
           (0  => Bits_8 (Tx_SFD'Length),
            9  => Magnitude (Tx_SFD (Tx_SFD'First      .. Tx_SFD'First + 7)),
            10 => Magnitude (Tx_SFD (Tx_SFD'First + 8  .. Tx_SFD'First + 15)),
            11 => Magnitude (Tx_SFD (Tx_SFD'First + 16 .. Tx_SFD'First + 23)),
            12 => Magnitude (Tx_SFD (Tx_SFD'First + 24 .. Tx_SFD'First + 31)),
            13 => Magnitude (Tx_SFD (Tx_SFD'First + 32 .. Tx_SFD'First + 39)),
            14 => Magnitude (Tx_SFD (Tx_SFD'First + 40 .. Tx_SFD'First + 47)),
            15 => Magnitude (Tx_SFD (Tx_SFD'First + 48 .. Tx_SFD'First + 55)),
            16 => Magnitude (Tx_SFD (Tx_SFD'First + 56 .. Tx_SFD'Last)),
            17 => Polarity  (Tx_SFD (Tx_SFD'First      .. Tx_SFD'First + 7)),
            18 => Polarity  (Tx_SFD (Tx_SFD'First + 8  .. Tx_SFD'First + 15)),
            19 => Polarity  (Tx_SFD (Tx_SFD'First + 16 .. Tx_SFD'First + 23)),
            20 => Polarity  (Tx_SFD (Tx_SFD'First + 24 .. Tx_SFD'First + 31)),
            21 => Polarity  (Tx_SFD (Tx_SFD'First + 32 .. Tx_SFD'First + 39)),
            22 => Polarity  (Tx_SFD (Tx_SFD'First + 40 .. Tx_SFD'First + 47)),
            23 => Polarity  (Tx_SFD (Tx_SFD'First + 48 .. Tx_SFD'First + 55)),
            24 => Polarity  (Tx_SFD (Tx_SFD'First + 56 .. Tx_SFD'Last)),
            25 => Magnitude (Rx_SFD (Rx_SFD'First      .. Rx_SFD'First + 7)),
            26 => Magnitude (Rx_SFD (Rx_SFD'First + 8  .. Rx_SFD'First + 15)),
            27 => Magnitude (Rx_SFD (Rx_SFD'First + 16 .. Rx_SFD'First + 23)),
            28 => Magnitude (Rx_SFD (Rx_SFD'First + 24 .. Rx_SFD'First + 31)),
            29 => Magnitude (Rx_SFD (Rx_SFD'First + 32 .. Rx_SFD'First + 39)),
            30 => Magnitude (Rx_SFD (Rx_SFD'First + 40 .. Rx_SFD'First + 47)),
            31 => Magnitude (Rx_SFD (Rx_SFD'First + 48 .. Rx_SFD'First + 55)),
            32 => Magnitude (Rx_SFD (Rx_SFD'First + 56 .. Rx_SFD'Last)),
            33 => Polarity  (Rx_SFD (Rx_SFD'First      .. Rx_SFD'First + 7)),
            34 => Polarity  (Rx_SFD (Rx_SFD'First + 8  .. Rx_SFD'First + 15)),
            35 => Polarity  (Rx_SFD (Rx_SFD'First + 16 .. Rx_SFD'First + 23)),
            36 => Polarity  (Rx_SFD (Rx_SFD'First + 24 .. Rx_SFD'First + 31)),
            37 => Polarity  (Rx_SFD (Rx_SFD'First + 32 .. Rx_SFD'First + 39)),
            38 => Polarity  (Rx_SFD (Rx_SFD'First + 40 .. Rx_SFD'First + 47)),
            39 => Polarity  (Rx_SFD (Rx_SFD'First + 48 .. Rx_SFD'First + 55)),
            40 => Polarity  (Rx_SFD (Rx_SFD'First + 56 .. Rx_SFD'Last)),
            others => 0);
      end if;

      USR_SFD.Write (USR_SFD_Reg);

   end Configure_Non_Standard_SFD;

   procedure Set_Frame_Filtering_Enabled (Enabled : in Boolean)
   is
      SYS_CFG_Reg : SYS_CFG_Type;
   begin
      SYS_CFG.Read (SYS_CFG_Reg);
      SYS_CFG_Reg.FFEN := (if Enabled then 1 else 0);
      SYS_CFG.Write (SYS_CFG_Reg);
   end Set_Frame_Filtering_Enabled;

   procedure Configure_Frame_Filtering (Behave_As_Coordinator   : in Boolean;
                                        Allow_Beacon_Frame      : in Boolean;
                                        Allow_Data_Frame        : in Boolean;
                                        Allow_Ack_Frame         : in Boolean;
                                        Allow_MAC_Cmd_Frame     : in Boolean;
                                        Allow_Reserved_Frame    : in Boolean;
                                        Allow_Frame_Type_4      : in Boolean;
                                        Allow_Frame_Type_5      : in Boolean)
   is
      SYS_CFG_Reg : SYS_CFG_Type;
   begin
      SYS_CFG.Read (SYS_CFG_Reg);
      SYS_CFG_Reg.FFBC := (if Behave_As_Coordinator then 1 else 0);
      SYS_CFG_Reg.FFAB := (if Allow_Beacon_Frame    then 1 else 0);
      SYS_CFG_Reg.FFAD := (if Allow_Data_Frame      then 1 else 0);
      SYS_CFG_Reg.FFAA := (if Allow_Ack_Frame       then 1 else 0);
      SYS_CFG_Reg.FFAM := (if Allow_MAC_Cmd_Frame   then 1 else 0);
      SYS_CFG_Reg.FFAR := (if Allow_Reserved_Frame  then 1 else 0);
      SYS_CFG_Reg.FFA4 := (if Allow_Frame_Type_4    then 1 else 0);
      SYS_CFG_Reg.FFA5 := (if Allow_Frame_Type_5    then 1 else 0);
      SYS_CFG.Write (SYS_CFG_Reg);
   end Configure_Frame_Filtering;

   procedure Set_Smart_Tx_Power (Enabled : in Boolean)
   is
      SYS_CFG_Reg : SYS_CFG_Type;

   begin
      SYS_CFG.Read (SYS_CFG_Reg);
      SYS_CFG_Reg.DIS_STXP := (if Enabled then 0 else 1);
      SYS_CFG.Write (SYS_CFG_Reg);
   end Set_Smart_Tx_Power;

   procedure Read_OTP_Tx_Power_Level (Channel  : in     Channel_Number;
                                      PRF      : in     PRF_Type;
                                      Tx_Power :    out TX_POWER_Type)
   is
      Address : Bits_11;
      Word    : Bits_32;

      function Word_To_TX_POWER is new Ada.Unchecked_Conversion
        (Source => Bits_32,
         Target => TX_POWER_Type);

   begin
      case Channel is
         when 1 => Address := OTP_ADDR_CH1_TX_POWER_PRF_64;
         when 2 => Address := OTP_ADDR_CH2_TX_POWER_PRF_64;
         when 3 => Address := OTP_ADDR_CH3_TX_POWER_PRF_64;
         when 4 => Address := OTP_ADDR_CH4_TX_POWER_PRF_64;
         when 5 => Address := OTP_ADDR_CH5_TX_POWER_PRF_64;
         when 7 => Address := OTP_ADDR_CH7_TX_POWER_PRF_64;
      end case;

      if PRF = PRF_64MHz then
         Address := Address + 1;
      end if;

      Read_OTP (Address => Address,
                Word    => Word);

      Tx_Power := Word_To_TX_POWER (Word);
   end Read_OTP_Tx_Power_Level;

   procedure Read_OTP_Antenna_Delay
     (Antenna_Delay_16_MHz : out Antenna_Delay_Time;
      Antenna_Delay_64_MHz : out Antenna_Delay_Time)
   is
      Word : Bits_32;
      Lo   : Bits_16;
      Hi   : Bits_16;

   begin
      Read_OTP (Address => OTP_ADDR_ANTENNA_DELAY,
                Word    => Word);

      Lo := Bits_16 (Word and 16#FFFF#);
      Hi := Bits_16 (Shift_Right (Word, 16) and 16#FFFF#);

      Antenna_Delay_16_MHz := To_Antenna_Delay_Time (Lo);
      Antenna_Delay_64_MHz := To_Antenna_Delay_Time (Hi);
   end Read_OTP_Antenna_Delay;

   function To_Bits_8 (Config : in Tx_Power_Config_Type) return Bits_8
     with SPARK_Mode => Off
   --  SPARK mode is disabled as a workaround since GNATprove doesn't yet
   --  support conversions between different fixed-point types.
   is
      type Extended_Fine_Tx_Power_Number is
      delta Fine_Tx_Power_Number'Delta
      range 0.0 .. Fine_Tx_Power_Number'Last * 2.0
        with Small => Fine_Tx_Power_Number'Small;
      --  Need a temporary type with twice the range to hold an intermediate
      --  calculation.

      Two : constant Extended_Fine_Tx_Power_Number := 2.0;
      --  GNATprove doesn't support multiplications/divisions between
      --  fixed point types and universal real. So this constant is used as a
      --  workaround so that thed multiplication is between two fixed point
      --  types.

      Temp : Extended_Fine_Tx_Power_Number;

      Coarse_Bits : Bits_3;
      Fine_Bits   : Bits_5;

   begin
      if Config.Coarse_Gain_Enabled then
         Coarse_Bits := 2#110# - Bits_3 (Bits_8 (Config.Coarse_Gain) / 3);
      else
         Coarse_Bits := 2#111#;
      end if;

      Temp := Extended_Fine_Tx_Power_Number (Config.Fine_Gain);
      Temp := Temp * Two;
      Fine_Bits := Bits_5 (Temp);

      return (Bits_8 (Coarse_Bits) * 2**5) or Bits_8 (Fine_Bits);
   end To_Bits_8;

   function Fine_Gain (Tx_Power_Bits : in Bits_8) return Fine_Tx_Power_Number
     with SPARK_Mode => Off
   --  SPARK mode is disabled as a workaround since GNATprove doesn't yet
   --  support conversions between different fixed-point types.
   is
      type Extended_Fine_Tx_Power_Number is
      delta Fine_Tx_Power_Number'Delta
      range 0.0 .. Fine_Tx_Power_Number'Last * 2.0
        with Small => Fine_Tx_Power_Number'Small;
      --  Need a temporary type with twice the range to hold an intermediate
      --  calculation.

      Two : constant Extended_Fine_Tx_Power_Number := 2.0;
      --  GNATprove doesn't support multiplications/divisions between
      --  fixed point types and universal real. So this constant is used as a
      --  workaround so that thed multiplication is between two fixed point
      --  types.

      Temp : Extended_Fine_Tx_Power_Number;
   begin
      Temp := Extended_Fine_Tx_Power_Number (Tx_Power_Bits and 2#000_11111#);
      Temp := Temp / Two;

      return Fine_Tx_Power_Number (Temp);
   end FIne_Gain;

   function Coarse_Gain (Tx_Power_Bits : in Bits_8)
                         return Coarse_Tx_Power_Number
   is
      Temp : Bits_8;
   begin
      if Is_Coarse_Gain_Enabled (Tx_Power_Bits) then
         Temp := Tx_Power_Bits / 2**5;
         return Coarse_Tx_Power_Number ((2#110# - Temp) * 3);
      else
         return 0.0;
      end if;
   end Coarse_Gain;

   procedure Configure_Smart_Tx_Power (Boost_Normal : Tx_Power_Config_Type;
                                       Boost_500us  : Tx_Power_Config_Type;
                                       Boost_250us  : Tx_Power_Config_Type;
                                       Boost_125us  : Tx_Power_Config_Type)
   is
      SYS_CFG_Reg : SYS_CFG_Type;

   begin
      TX_POWER.Write (TX_POWER_Type'(BOOSTNORM => To_Bits_8 (Boost_Normal),
                                     BOOSTP500 => To_Bits_8 (Boost_500us),
                                     BOOSTP250 => To_Bits_8 (Boost_250us),
                                     BOOSTP125 => To_Bits_8 (Boost_125us)));

      SYS_CFG.Read (SYS_CFG_Reg);
      SYS_CFG_Reg.DIS_STXP := 0; --  Enable smart Tx power
      SYS_CFG.Write (SYS_CFG_Reg);
   end Configure_Smart_Tx_Power;

   procedure Configure_Manual_Tx_Power (Boost_SHR : Tx_Power_Config_Type;
                                        Boost_PHR : Tx_Power_Config_Type)
   is
      SYS_CFG_Reg : SYS_CFG_Type;

   begin
      TX_POWER.Write (TX_POWER_Type'(BOOSTNORM => 16#22#,
                                     BOOSTP500 => To_Bits_8 (Boost_PHR),
                                     BOOSTP250 => To_Bits_8 (Boost_SHR),
                                     BOOSTP125 => 16#0E#));

      SYS_CFG.Read (SYS_CFG_Reg);
      SYS_CFG_Reg.DIS_STXP := 1; --  Disable smart Tx power
      SYS_CFG.Write (SYS_CFG_Reg);
   end Configure_Manual_Tx_Power;

   procedure Set_Tx_Data (Data   : in Types.Byte_Array;
                          Offset : in Natural)
   is
   begin
      DW1000.Register_Driver.Write_Register
        (Register_ID => Registers.TX_BUFFER_Reg_ID,
         Sub_Address => Types.Bits_15 (Offset),
         Data        => Data);
   end Set_Tx_Data;

   procedure Set_Tx_Frame_Length (Length : in Natural;
                                  Offset : in Natural)
   is
      TX_FCTRL_Reg : TX_FCTRL_Type;

   begin
      TX_FCTRL.Read (TX_FCTRL_Reg);
      TX_FCTRL_Reg.TFLEN   := Types.Bits_7 (Length mod 2**7);
      TX_FCTRL_Reg.TFLE    := Types.Bits_3 (Length / 2**7);
      TX_FCTRL_Reg.TXBOFFS := Types.Bits_10 (Offset);
      TX_FCTRL.Write (TX_FCTRL_Reg);

   end Set_Tx_Frame_Length;

   procedure Start_Tx_Immediate (Rx_After_Tx : in Boolean)
   is
      SYS_CTRL_Reg   : SYS_CTRL_Type;

   begin
      SYS_CTRL_Reg := SYS_CTRL_Type'
        (SFCST      => 0,
         TXSTRT     => 1,
         TXDLYS     => 0, --  No delay
         CANSFCS    => 0,
         TRXOFF     => 0,
         WAIT4RESP  => (if Rx_After_Tx then 1 else 0),
         RXENAB     => 0,
         RXDLYE     => 0,
         HRBPT      => 0,
         Reserved_1 => 0,
         Reserved_2 => 0,
         Reserved_3 => 0);

      SYS_CTRL.Write (SYS_CTRL_Reg);
   end Start_Tx_Immediate;

   procedure Start_Tx_Delayed (Rx_After_Tx : in     Boolean;
                               Result      :    out Result_Type)
   is
      SYS_CTRL_Reg   : SYS_CTRL_Type;
      SYS_STATUS_Reg : SYS_STATUS_Type;

   begin
      SYS_CTRL_Reg := SYS_CTRL_Type'
        (SFCST      => 0,
         TXSTRT     => 1,
         TXDLYS     => 1, --  Delayed transmit
         CANSFCS    => 0,
         TRXOFF     => 0,
         WAIT4RESP  => (if Rx_After_Tx then 1 else 0),
         RXENAB     => 0,
         RXDLYE     => 0,
         HRBPT      => 0,
         Reserved_1 => 0,
         Reserved_2 => 0,
         Reserved_3 => 0);

      SYS_CTRL.Write (SYS_CTRL_Reg);

      SYS_STATUS.Read (SYS_STATUS_Reg);

      if SYS_STATUS_Reg.HPDWARN = 0 then
         Result := Success;

      else
         -- Cancel the transmit
         SYS_CTRL_Reg := SYS_CTRL_Type'(SFCST      => 0,
                                        TXSTRT     => 0,
                                        TXDLYS     => 0,
                                        CANSFCS    => 0,
                                        TRXOFF     => 1,
                                        WAIT4RESP  => 0,
                                        RXENAB     => 0,
                                        RXDLYE     => 0,
                                        HRBPT      => 0,
                                        Reserved_1 => 0,
                                        Reserved_2 => 0,
                                        Reserved_3 => 0);
         SYS_CTRL.Write (SYS_CTRL_Reg);

         Set_Sleep_After_Tx (Enabled => False);

         Result := Error;
      end if;

   end Start_Tx_Delayed;

   procedure Read_Rx_Data (Data   :    out Types.Byte_Array;
                           Offset : in     Natural)
   is
   begin
      DW1000.Register_Driver.Read_Register
        (Register_ID => Registers.RX_BUFFER_Reg_ID,
         Sub_Address => Types.Bits_15 (Offset),
         Data        => Data);
   end Read_Rx_Data;

   procedure Set_Delayed_Tx_Rx_Time (Delay_Time : in Coarse_System_Time)
   is
   begin
      DX_TIME.Write (DX_TIME_Type'(DX_TIME => To_Bits_40 (Delay_Time)));
   end Set_Delayed_Tx_Rx_Time;

   procedure Set_Sleep_After_Tx (Enabled : in Boolean)
   is
      PMSC_CTRL1_Reg : PMSC_CTRL1_Type;

   begin
      PMSC_CTRL1.Read (PMSC_CTRL1_Reg);
      PMSC_CTRL1_Reg.ATXSLP := (if Enabled then 1 else 0);
      PMSC_CTRL1.Write (PMSC_CTRL1_Reg);
   end Set_Sleep_After_Tx;

   procedure Read_Rx_Adjusted_Timestamp (Timestamp : out Fine_System_Time)
   is
      RX_TIME_Reg : RX_TIME_Type;

   begin
      RX_TIME.Read (RX_TIME_Reg);
      Timestamp := To_Fine_System_Time (RX_TIME_Reg.RX_STAMP);
   end Read_Rx_Adjusted_Timestamp;


   procedure Read_Rx_Raw_Timestamp (Timestamp : out Coarse_System_Time)
   is
      RX_Time_Reg : RX_Time_Type;

   begin
      RX_TIME.Read (RX_TIME_Reg);
      Timestamp := To_Coarse_System_Time (RX_TIME_Reg.RX_RAWST);
   end Read_Rx_Raw_Timestamp;


   procedure Read_Rx_Timestamps (Adjusted : out Fine_System_Time;
                                 Raw      : out Coarse_System_Time)
   is
      RX_Time_Reg : RX_Time_Type;

   begin
      RX_TIME.Read (RX_TIME_Reg);
      Adjusted := To_Fine_System_Time (RX_TIME_Reg.RX_STAMP);
      Raw      := To_Coarse_System_Time (RX_TIME_Reg.RX_RAWST);
   end Read_Rx_Timestamps;


   procedure Read_Tx_Adjusted_Timestamp (Timestamp : out Fine_System_Time)
   is
      TX_TIME_Reg : TX_TIME_Type;

   begin
      TX_TIME.Read (TX_TIME_Reg);
      Timestamp := To_Fine_System_Time (TX_TIME_Reg.TX_STAMP);
   end Read_Tx_Adjusted_Timestamp;


   procedure Read_Tx_Raw_Timestamp (Timestamp : out Coarse_System_Time)
   is
      TX_Time_Reg : TX_Time_Type;

   begin
      TX_TIME.Read (TX_TIME_Reg);
      Timestamp := To_Coarse_System_Time (TX_TIME_Reg.TX_RAWST);
   end Read_Tx_Raw_Timestamp;


   procedure Read_Tx_Timestamps (Adjusted : out Fine_System_Time;
                                 Raw      : out Coarse_System_Time)
   is
      TX_Time_Reg : TX_Time_Type;

   begin
      TX_TIME.Read (TX_TIME_Reg);
      Adjusted := To_Fine_System_Time (TX_TIME_Reg.TX_STAMP);
      Raw      := To_Coarse_System_Time (TX_TIME_Reg.TX_RAWST);
   end Read_Tx_Timestamps;


   procedure Read_System_Timestamp (Timestamp : out Coarse_System_Time)
   is
      SYS_TIME_Reg : SYS_TIME_Type;

   begin
      SYS_TIME.Read (SYS_TIME_Reg);
      Timestamp := To_Coarse_System_Time (SYS_TIME_Reg.SYS_TIME);
   end Read_System_Timestamp;

   procedure Check_Overrun (Overrun : out Boolean)
   is
      SYS_STATUS_Reg : SYS_STATUS_Type;

   begin
      SYS_STATUS.Read (SYS_STATUS_Reg);
      Overrun := SYS_STATUS_Reg.RXOVRR = 1;
   end Check_Overrun;

   procedure Force_Tx_Rx_Off
   is
      SYS_MASK_Reg : SYS_MASK_Type;

   begin
      -- Temporarily disable all interrupts
      SYS_MASK.Read (SYS_MASK_Reg);
      SYS_MASK.Write (SYS_MASK_Type'(Reserved_3 => 0,
                                     others     => 0));

      -- Disable Tx
      SYS_CTRL.Write (SYS_CTRL_Type'(SFCST      => 0,
                                     TXSTRT     => 0,
                                     TXDLYS     => 0,
                                     CANSFCS    => 0,
                                     TRXOFF     => 1,
                                     WAIT4RESP  => 0,
                                     RXENAB     => 0,
                                     RXDLYE     => 0,
                                     HRBPT      => 0,
                                     Reserved_1 => 0,
                                     Reserved_2 => 0,
                                     Reserved_3 => 0));

      -- Force transceiver off; don't want to see any new events.
      SYS_STATUS.Write (SYS_STATUS_Type'(IRQS       => 0,
                                         CPLOCK     => 0,
                                         ESYNCR     => 0,
                                         AAT        => 1,
                                         TXFRB      => 1,
                                         TXPRS      => 1,
                                         TXPHS      => 1,
                                         TXFRS      => 1,
                                         RXPRD      => 1,
                                         RXSFDD     => 1,
                                         LDEDONE    => 1,
                                         RXPHD      => 1,
                                         RXPHE      => 1,
                                         RXDFR      => 1,
                                         RXFCG      => 1,
                                         RXFCE      => 1,
                                         RXRFSL     => 1,
                                         RXRFTO     => 1,
                                         LDEERR     => 1,
                                         RXOVRR     => 0,
                                         RXPTO      => 1,
                                         GPIOIRQ    => 0,
                                         SLP2INIT   => 0,
                                         RFPLL_LL   => 0,
                                         CLKPLL_LL  => 0,
                                         RXSFDTO    => 1,
                                         HPDWARN    => 0,
                                         TXBERR     => 0,
                                         AFFREJ     => 1,
                                         HSRBP      => 0,
                                         ICRBP      => 0,
                                         RXRSCS     => 0,
                                         RXPREJ     => 0,
                                         TXPUTE     => 0,
                                         Reserved_1 => 0,
                                         Reserved_2 => 0));

      Sync_Rx_Buffer_Pointers;

      -- Restore previous interrupt settings.
      SYS_MASK.Write (SYS_MASK_Reg);

   end Force_Tx_Rx_Off;

   procedure Toggle_Host_Side_Rx_Buffer_Pointer
   is
      SYS_CTRL_Reg   : SYS_CTRL_Type;
   begin
      SYS_CTRL.Read (SYS_CTRL_Reg);
      SYS_CTRL_Reg.HRBPT := 1;
      SYS_CTRL.Write (SYS_CTRL_Reg);
   end Toggle_Host_Side_Rx_Buffer_Pointer;

   procedure Sync_Rx_Buffer_Pointers
   is
      SYS_STATUS_Reg : SYS_STATUS_Type;

   begin
      SYS_STATUS.Read (SYS_STATUS_Reg);

      -- Check if the IC side receive buffer pointer is the same
      -- as the host side receive buffer pointer.
      if SYS_STATUS_Reg.ICRBP /= SYS_STATUS_Reg.HSRBP then
         Toggle_Host_Side_Rx_Buffer_Pointer;
      end if;

   end Sync_Rx_Buffer_Pointers;


   procedure Start_Rx_Immediate
   is
      SYS_CTRL_Reg   : SYS_CTRL_Type;

   begin
      Sync_Rx_Buffer_Pointers;

      SYS_CTRL_Reg := SYS_CTRL_Type'
        (SFCST      => 0,
         TXSTRT     => 0,
         TXDLYS     => 0,
         CANSFCS    => 0,
         TRXOFF     => 0,
         WAIT4RESP  => 0,
         RXENAB     => 1,
         RXDLYE     => 0,
         HRBPT      => 0,
         Reserved_1 => 0,
         Reserved_2 => 0,
         Reserved_3 => 0);

      SYS_CTRL.Write (SYS_CTRL_Reg);

   end Start_Rx_Immediate;


   procedure Start_Rx_Delayed (Result  : out Result_Type)
   is
      SYS_CTRL_Reg   : SYS_CTRL_Type;
      SYS_STATUS_Reg : SYS_STATUS_Type;

   begin
      Sync_Rx_Buffer_Pointers;

      SYS_CTRL_Reg := SYS_CTRL_Type'
        (SFCST      => 0,
         TXSTRT     => 0,
         TXDLYS     => 0,
         CANSFCS    => 0,
         TRXOFF     => 0,
         WAIT4RESP  => 0,
         RXENAB     => 1,
         RXDLYE     => 1, --  Delayed RX
         HRBPT      => 0,
         Reserved_1 => 0,
         Reserved_2 => 0,
         Reserved_3 => 0);

      SYS_CTRL.Write (SYS_CTRL_Reg);

      -- Check for errors
      SYS_STATUS.Read (SYS_STATUS_Reg);

      if SYS_STATUS_Reg.HPDWARN = 1 then
         Force_Tx_Rx_Off;

         -- Clear the delay bit
         SYS_CTRL_Reg.RXDLYE := 0;
         SYS_CTRL.Write (SYS_CTRL_Reg);

         Result := Error;

      else
         Result := Success;
      end if;

   end Start_Rx_Delayed;

   procedure Set_Rx_Mode (Mode        : in Rx_Modes;
                          Rx_On_Time  : in Types.Bits_4;
                          Rx_Off_Time : in Coarse_System_Time)
   is
   begin
      if Mode = Normal then
         RX_SNIFF.Write (RX_SNIFF_Type'(SNIFF_ONT  => 0,
                                        SNIFF_OFFT => 0,
                                        Reserved_1 => 0,
                                        Reserved_2 => 0));

      else
         RX_SNIFF.Write
           (RX_SNIFF_Type'
              (SNIFF_ONT  => Rx_On_Time,
               SNIFF_OFFT => Bits_8 (To_Bits_40 (Rx_Off_Time) and 16#FF#),
               Reserved_1 => 0,
               Reserved_2 => 0));
      end if;

   end Set_Rx_Mode;

   procedure Set_Auto_Rx_Reenable (Enabled : in Boolean)
   is
      SYS_CFG_Reg : SYS_CFG_Type;

   begin
      SYS_CFG.Read (SYS_CFG_Reg);
      SYS_CFG_Reg.RXAUTR := (if Enabled then 1 else 0);
      SYS_CFG.Write (SYS_CFG_Reg);
   end Set_Auto_Rx_Reenable;

   procedure Set_Rx_Double_Buffer (Enabled : in Boolean)
   is
      SYS_CFG_Reg : SYS_CFG_Type;

   begin
      SYS_CFG.Read (SYS_CFG_Reg);
      SYS_CFG_Reg.DIS_DRXB := (if Enabled then 0 else 1);
      SYS_CFG.Write (SYS_CFG_Reg);
   end Set_Rx_Double_Buffer;

   procedure Set_Rx_Timeout (Timeout : in Types.Bits_16)
   is
      SYS_CFG_Reg : SYS_CFG_Type;

   begin
      SYS_CFG.Read (SYS_CFG_Reg);

      if Timeout > 0 then
         SYS_CFG_Reg.RXWTOE := 1;

         RX_FWTO.Write ( (RXFWTO => Timeout) );

      else
         SYS_CFG_Reg.RXWTOE := 0;

      end if;

      SYS_CFG.Write (SYS_CFG_Reg);
   end Set_Rx_Timeout;

   procedure Set_Preamble_Detect_Timeout (Timeout : in Types.Bits_16)
   is
   begin
      DRX_PRETOC.Write ( (DRX_PRETOC => Timeout) );
   end Set_Preamble_Detect_Timeout;

   procedure Calibrate_Sleep_Count
     (Half_XTAL_Cycles_Per_LP_Osc_Cycle : out Types.Bits_16)
   is
      PMSC_CTRL0_Reg : PMSC_CTRL0_Type;

      Data : Types.Byte_Array (1 .. 2);

   begin
      -- Enable calibration
      AON_CFG1.Write (AON_CFG1_Type'(SLEEP_CE => 0,
                                     SMXX     => 0,
                                     LPOSC_C  => 1,
                                     Reserved => 0));
      Upload_AON_Config;

      -- Disable calibration
      AON_CFG1.Write (AON_CFG1_Type'(SLEEP_CE => 0,
                                     SMXX     => 0,
                                     LPOSC_C  => 0,
                                     Reserved => 0));
      Upload_AON_Config;

      PMSC_CTRL0.Read (PMSC_CTRL0_Reg);
      PMSC_CTRL0_Reg.SYSCLKS := 2#01#;
      PMSC_CTRL0_Reg.RXCLKS  := 2#00#;
      PMSC_CTRL0.Write (PMSC_CTRL0_Reg);

      -- Read number of XTAL/2 cycles per LP osc cycle
      AON_Contiguous_Read (Start_Address => 117,
                           Data          => Data);

      Half_XTAL_Cycles_Per_LP_Osc_Cycle
        := (Types.Bits_16 (Data (1))
            or Shift_Left (Types.Bits_16 (Data (2)), 8));

      -- Reset PMSC_CTRL0
      PMSC_CTRL0_Reg.SYSCLKS := 2#00#;
      PMSC_CTRL0.Write (PMSC_CTRL0_Reg);

   end Calibrate_Sleep_Count;

   procedure Upload_AON_Config
   is
   begin
      AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                     SAVE     => 0,
                                     UPL_CFG  => 1,
                                     DCA_READ => 0,
                                     DCA_ENAB => 0,
                                     Reserved => 0));
      AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                     SAVE     => 0,
                                     UPL_CFG  => 0,
                                     DCA_READ => 0,
                                     DCA_ENAB => 0,
                                     Reserved => 0));
   end Upload_AON_Config;

   procedure Save_Registers_To_AON
   is
   begin
      AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                     SAVE     => 1, --  This bit auto-clears
                                     UPL_CFG  => 0,
                                     DCA_READ => 0,
                                     DCA_ENAB => 0,
                                     Reserved => 0));
   end Save_Registers_To_AON;

   procedure Restore_Registers_From_AON
   is
   begin
      AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 1, --  This bit auto-clears
                                     SAVE     => 0,
                                     UPL_CFG  => 0,
                                     DCA_READ => 0,
                                     DCA_ENAB => 0,
                                     Reserved => 0));
   end Restore_Registers_From_AON;

   procedure AON_Read_Byte (Address : in     Types.Bits_8;
                            Data    :    out Types.Bits_8)
   is
      AON_RDAT_Reg : AON_RDAT_Type;

   begin
      -- Load address
      AON_ADDR.Write (AON_ADDR_Type'(AON_ADDR => Address));

      -- Enable DCA_ENAB
      AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                     SAVE     => 0,
                                     UPL_CFG  => 0,
                                     DCA_READ => 0,
                                     DCA_ENAB => 1,
                                     Reserved => 0));

      -- Now also enable DCA_READ
      AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                     SAVE     => 0,
                                     UPL_CFG  => 0,
                                     DCA_READ => 1,
                                     DCA_ENAB => 1,
                                     Reserved => 0));

      -- Read the result
      AON_RDAT.Read (AON_RDAT_Reg);
      Data := AON_RDAT_Reg.AON_RDAT;

      -- Clear DCA_ENAB and DCA_READ
      AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                     SAVE     => 0,
                                     UPL_CFG  => 0,
                                     DCA_READ => 0,
                                     DCA_ENAB => 0,
                                     Reserved => 0));
   end AON_Read_Byte;

   procedure AON_Contiguous_Read (Start_Address : in     Types.Bits_8;
                                  Data          :    out Types.Byte_Array)
   is
      Address      : Types.Bits_8 := Start_Address;
      AON_RDAT_Reg : AON_RDAT_Type;

   begin
      Data := (others => 0); -- workaround for flow analysis.

      for I in Data'Range loop
         -- Load address
         AON_ADDR.Write (AON_ADDR_Type'(AON_ADDR => Address));

         -- Enable DCA_ENAB
         AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                        SAVE     => 0,
                                        UPL_CFG  => 0,
                                        DCA_READ => 0,
                                        DCA_ENAB => 1,
                                        Reserved => 0));

         -- Now also enable DCA_READ
         AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                        SAVE     => 0,
                                        UPL_CFG  => 0,
                                        DCA_READ => 1,
                                        DCA_ENAB => 1,
                                        Reserved => 0));

         -- Read the result
         AON_RDAT.Read (AON_RDAT_Reg);
         Data (I) := AON_RDAT_Reg.AON_RDAT;

         Address := Address + 1;
      end loop;

      -- Clear DCA_ENAB and DCA_READ
      AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                     SAVE     => 0,
                                     UPL_CFG  => 0,
                                     DCA_READ => 0,
                                     DCA_ENAB => 0,
                                     Reserved => 0));
   end AON_Contiguous_Read;

   procedure AON_Scatter_Read (Addresses : in     Types.Byte_Array;
                               Data      :    out Types.Byte_Array)
   is
      AON_RDAT_Reg : AON_RDAT_Type;

      A_First : constant Integer := Addresses'First;
      D_First : constant Integer := Data'First;

   begin
      Data := (others => 0); -- workaround for flow analysis.

      for I in Natural range 0 .. Data'Length - 1 loop
         -- Load address
         AON_ADDR.Write (AON_ADDR_Type'(AON_ADDR => Addresses (A_First + I)));

         -- Enable DCA_ENAB
         AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                        SAVE     => 0,
                                        UPL_CFG  => 0,
                                        DCA_READ => 0,
                                        DCA_ENAB => 1,
                                        Reserved => 0));

         -- Now also enable DCA_READ
         AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                        SAVE     => 0,
                                        UPL_CFG  => 0,
                                        DCA_READ => 1,
                                        DCA_ENAB => 1,
                                        Reserved => 0));

         -- Read the result
         AON_RDAT.Read (AON_RDAT_Reg);
         Data (D_First + I) := AON_RDAT_Reg.AON_RDAT;
      end loop;

      -- Clear DCA_ENAB and DCA_READ
      AON_CTRL.Write (AON_CTRL_Type'(RESTORE  => 0,
                                     SAVE     => 0,
                                     UPL_CFG  => 0,
                                     DCA_READ => 0,
                                     DCA_ENAB => 0,
                                     Reserved => 0));
   end AON_Scatter_Read;

   procedure Configure_Sleep_Count (Sleep_Count : in Types.Bits_16)
   is
      PMSC_CTRL0_Reg : PMSC_CTRL0_Type;

   begin
      PMSC_CTRL0.Read (PMSC_CTRL0_Reg);
      PMSC_CTRL0_Reg.SYSCLKS := 2#01#;
      PMSC_CTRL0_Reg.RXCLKS  := 2#00#;
      PMSC_CTRL0.Write (PMSC_CTRL0_Reg);

      -- Make sure we don't accidentally sleep
      AON_CFG0.Write (AON_CFG0_Type'(SLEEP_EN  => 0,
                                     WAKE_PIN  => 0,
                                     WAKE_SPI  => 0,
                                     WAKE_CNT  => 0,
                                     LPDIV_EN  => 0,
                                     LPCLKDIVA => 0,
                                     SLEEP_TIM => 0));

      AON_CFG1.Write (AON_CFG1_Type'(SLEEP_CE => 0,
                                     SMXX     => 0,
                                     LPOSC_C  => 0,
                                     Reserved => 0));

      -- Disable the sleep counter
      Upload_AON_Config;

      -- Set the new value
      AON_CFG0.Write (AON_CFG0_Type'(SLEEP_EN  => 0,
                                     WAKE_PIN  => 0,
                                     WAKE_SPI  => 0,
                                     WAKE_CNT  => 0,
                                     LPDIV_EN  => 0,
                                     LPCLKDIVA => 0,
                                     SLEEP_TIM => Sleep_Count));
      Upload_AON_Config;

      -- Enable the new value
      AON_CFG1.Write (AON_CFG1_Type'(SLEEP_CE => 1,
                                     SMXX     => 0,
                                     LPOSC_C  => 0,
                                     Reserved => 0));

      PMSC_CTRL0_Reg.SYSCLKS := 2#00#;
      PMSC_CTRL0.Write (PMSC_CTRL0_Reg);

   end Configure_Sleep_Count;


   procedure Set_XTAL_Trim (Trim : in Types.Bits_5)
   is
      FS_XTALT_Reg : FS_XTALT_Type;

   begin
      FS_XTALT.Read (FS_XTALT_Reg);
      FS_XTALT_Reg.XTALT := Trim;
      FS_XTALT.Write (FS_XTALT_Reg);
   end Set_XTAL_Trim;

   procedure Configure_LEDs (Tx_LED_Enable    : in Boolean;
                             Rx_LED_Enable    : in Boolean;
                             Rx_OK_LED_Enable : in Boolean;
                             SFD_LED_Enable   : in Boolean;
                             Test_Flash       : in Boolean)
   is
      GPIO_MODE_Reg  : Register_Types.GPIO_MODE_Type;
      PMSC_LEDC_Reg  : Register_Types.PMSC_LEDC_Type;
      PMSC_CTRL0_Reg : Register_Types.PMSC_CTRL0_Type;

      LED_Enabled    : constant Boolean := (Tx_LED_Enable or
                                            Rx_LED_Enable or
                                            Rx_OK_LED_Enable or
                                            SFD_LED_Enable);
   begin
      --  Configure LED GPIOs
      GPIO_MODE.Read (GPIO_MODE_Reg);
      GPIO_MODE_Reg.MSGP0 := (if Rx_OK_LED_Enable then 1 else 0);
      GPIO_MODE_Reg.MSGP1 := (if SFD_LED_Enable   then 1 else 0);
      GPIO_MODE_Reg.MSGP2 := (if Rx_LED_Enable    then 1 else 0);
      GPIO_MODE_Reg.MSGP3 := (if Tx_LED_Enable    then 1 else 0);
      GPIO_MODE.Write (GPIO_MODE_Reg);

      --  Enable LP oscillator to run from counter, turn on debounce clock
      PMSC_CTRL0.Read (PMSC_CTRL0_Reg);
      PMSC_CTRL0_Reg.GPDCE := 1;
      PMSC_CTRL0_Reg.KHZCLKEN := 1;
      PMSC_CTRL0.Write (PMSC_CTRL0_Reg);

      -- Enable LEDs
      PMSC_LEDC.Read (PMSC_LEDC_Reg);
      PMSC_LEDC_Reg.BLINK_TIM := 16; -- 16 * 14 ms = 224 ms
      PMSC_LEDC_Reg.BLNKEN    := (if LED_Enabled then 1 else 0);
      PMSC_LEDC.Write (PMSC_LEDC_Reg);

      if Test_Flash then
         --  Blink each LED
         PMSC_LEDC_Reg.BLNKNOW := 2#1111#;
         PMSC_LEDC.Write (PMSC_LEDC_Reg);

         --  Clear forced bits
         PMSC_LEDC_Reg.BLNKNOW := 2#0000#;
         PMSC_LEDC.Write (PMSC_LEDC_Reg);
      end if;
   end Configure_LEDs;

end DW1000.Driver;
