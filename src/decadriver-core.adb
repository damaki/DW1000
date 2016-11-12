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

with DW1000.Constants; use DW1000.Constants;
with DW1000.Registers; use DW1000.Registers;
with Interfaces;       use Interfaces;

package body DecaDriver.Core
with SPARK_Mode => On
is

   Default_SFD_Timeout : constant DW1000.Driver.SFD_Timeout_Number := 16#1041#;

   protected body Driver
   is

      procedure Initialize (Load_Antenna_Delay   : in Boolean;
                            Load_XTAL_Trim       : in Boolean;
                            Load_UCode_From_ROM  : in Boolean)
      is
         Word : Bits_32;

         PMSC_CTRL1_Reg : DW1000.Register_Types.PMSC_CTRL1_Type;
         SYS_MASK_Reg   : DW1000.Register_Types.SYS_MASK_Type;

      begin

         DW1000.Driver.Enable_Clocks (DW1000.Driver.Force_Sys_XTI);

         DW1000.Driver.Read_OTP (OTP_ADDR_CHIP_ID, Part_ID);
         DW1000.Driver.Read_OTP (OTP_ADDR_LOT_ID, Lot_ID);

         if Load_Antenna_Delay then
            DW1000.Driver.Read_OTP (OTP_ADDR_ANTENNA_DELAY,
                                    Word);

            -- High 16 bits are the antenna delay with a 64 MHz PRF.
            -- Low 16 bits are the antenna delay with a 16 MHz PRF.
            Antenna_Delay_PRF_16 :=
              To_Fine_System_Time (Bits_40 (Word and 16#FFFF#));

            Word := Shift_Right (Word, 16);

            Antenna_Delay_PRF_64 :=
              To_Fine_System_Time (Bits_40 (Word and 16#FFFF#));
         else
            Antenna_Delay_PRF_16 := 0.0;
            Antenna_Delay_PRF_64 := 0.0;
         end if;

         if Load_XTAL_Trim then
            DW1000.Driver.Read_OTP (OTP_ADDR_XTAL_TRIM, Word);
            XTAL_Trim := Bits_5 (Word and 2#1_1111#);
         else
            XTAL_Trim := 2#1_0000#; -- Set to midpoint
         end if;

         if Load_UCode_From_ROM then
            DW1000.Driver.Load_LDE_From_ROM;

         else
            -- Should disable LDERUN bit, since the LDE isn't loaded.
            DW1000.Registers.PMSC_CTRL1.Read (PMSC_CTRL1_Reg);
            PMSC_CTRL1_Reg.LDERUNE := 0;
            DW1000.Registers.PMSC_CTRL1.Write (PMSC_CTRL1_Reg);
         end if;

         DW1000.Driver.Enable_Clocks (DW1000.Driver.Force_Sys_PLL);
         DW1000.Driver.Enable_Clocks (DW1000.Driver.Enable_All_Seq);

         --  Store a local copy of the SYS_CFG register
         DW1000.Registers.SYS_CFG.Read (SYS_CFG_Reg);

         --  Configure IRQs
         DW1000.Registers.SYS_MASK.Read (SYS_MASK_Reg);
         SYS_MASK_Reg.MRXSFDTO := 1;
         SYS_MASK_Reg.MRXPHE   := 1;
         SYS_MASK_Reg.MRXRFSL  := 1;
         SYS_MASK_Reg.MRXFCE   := 1;
         SYS_MASK_Reg.MRXFCG   := 1; --  Always detect frame received
         SYS_MASK_Reg.MRXDFR   := 1;
         SYS_MASK_Reg.MTXFRS   := 1; --  Always detect frame sent
         DW1000.Registers.SYS_MASK.Write (SYS_MASK_Reg);

         Detect_Frame_Timeout := True;
         Detect_SFD_Timeout   := True;
         Detect_PHR_Error     := True;
         Detect_RS_Error      := True;
         Detect_FCS_Error     := True;

      end Initialize;



      procedure Configure (Config : in Configuration_Type)
      is
         SFD_Timeout : DW1000.Driver.SFD_Timeout_Number;

      begin

         --  110 kbps data rate has special handling
         if Config.Data_Rate = DW1000.Driver.Data_Rate_110k then
            SYS_CFG_Reg.RXM110K := 1;
         else
            SYS_CFG_Reg.RXM110K := 0;
         end if;

         Long_Frames := Config.PHR_Mode = DW1000.Driver.Extended_Frames;
         SYS_CFG_Reg.PHR_MODE :=
           Bits_2 (DW1000.Driver.Physical_Header_Modes'Pos (Config.PHR_Mode));

         DW1000.Registers.SYS_CFG.Write (SYS_CFG_Reg);

         DW1000.Driver.Configure_LDE (Config.PRF,
                                      Config.Rx_Preamble_Code,
                                      Config.Data_Rate);
         DW1000.Driver.Configure_PLL (Config.Channel);
         DW1000.Driver.Configure_RF (Config.Channel);

         --  Don't allow a zero SFD timeout
         SFD_Timeout := (if Config.SFD_Timeout = 0
                         then Default_SFD_Timeout
                         else Config.SFD_Timeout);

         DW1000.Driver.Configure_DRX
           (PRF                => Config.PRF,
            Data_Rate          => Config.Data_Rate,
            Tx_Preamble_Length => Config.Tx_Preamble_Length,
            PAC                => Config.Rx_PAC,
            SFD_Timeout        => SFD_Timeout,
            Nonstandard_SFD    => Config.Use_Nonstandard_SFD);

         DW1000.Driver.Configure_AGC (Config.PRF);

         DW1000.Driver.Configure_TC (Config.Channel);

         --  If a non-std SFD is used then the SFD length must be programmed
         --  for the DecaWave SFD, based on the data rate.
         if Config.Use_Nonstandard_SFD then
            Configure_Nonstandard_SFD_Length (Config.Data_Rate);
         end if;

         --  Configure the channel, Rx PRF, non-std SFD, and preamble codes
         DW1000.Registers.CHAN_CTRL.Write
           (DW1000.Register_Types.CHAN_CTRL_Type'
              (TX_CHAN  => Bits_4 (Config.Channel),
               RX_CHAN  => Bits_4 (Config.Channel),
               DWSFD    => (if Config.Use_Nonstandard_SFD then 1 else 0),
               RXPRF    => (if Config.PRF = PRF_16MHz then 2#01# else 2#10#),
               TNSSFD   => (if Config.Use_Nonstandard_SFD then 1 else 0),
               RNSSFD   => (if Config.Use_Nonstandard_SFD then 1 else 0),
               TX_PCODE => Bits_5 (Config.Tx_Preamble_Code),
               RX_PCODE => Bits_5 (Config.Rx_Preamble_Code),
               Reserved => 0));

         --  Set the Tx frame control (transmit data rate, PRF, ranging bit)
         DW1000.Registers.TX_FCTRL.Write
           (DW1000.Register_Types.TX_FCTRL_Type'
              (TFLEN    => 0,
               TFLE     => 0,
               R        => 0,
               TXBR     => Bits_2 (Data_Rates'Pos (Config.Data_Rate)),
               TR       => 1,
               TXPRF    => (if Config.PRF = PRF_16MHz then 2#01# else 2#10#),
               TXPSR    =>
                 (case Config.Tx_Preamble_Length is
                     when PLEN_64 | PLEN_128 | PLEN_256 | PLEN_512 => 2#01#,
                     when PLEN_1024 | PLEN_1536 | PLEN_2048        => 2#10#,
                     when others                                   => 2#11#),
               PE       =>
                 (case Config.Tx_Preamble_Length is
                     when PLEN_64 | PLEN_1024 | PLEN_4096 => 2#00#,
                     when PLEN_128 | PLEN_1536            => 2#01#,
                     when PLEN_256 | PLEN_2048            => 2#10#,
                     when others                          => 2#11#),
               TXBOFFS  => 0,
               IFSDELAY => 0));

         --  Load the crystal trim (if requested)
         if Use_OTP_XTAL_Trim then
            DW1000.Driver.Set_XTAL_Trim (XTAL_Trim);
         end if;

         --  Load the antenna delay (if requested)
         if Use_OTP_Antenna_Delay then
            if Config.PRF = PRF_16MHz then
               DW1000.Driver.Write_Tx_Antenna_Delay (Antenna_Delay_PRF_16);
               DW1000.Driver.Write_Rx_Antenna_Delay (Antenna_Delay_PRF_16);
            else
               DW1000.Driver.Write_Tx_Antenna_Delay (Antenna_Delay_PRF_64);
               DW1000.Driver.Write_Rx_Antenna_Delay (Antenna_Delay_PRF_64);
            end if;
         end if;

      end Configure;


      procedure Configure_LEDs (Tx_LED_Enable    : in Boolean;
                                Rx_LED_Enable    : in Boolean;
                                Rx_OK_LED_Enable : in Boolean;
                                SFD_LED_Enable   : in Boolean;
                                Test_Flash       : in Boolean)
      is
      begin
         DW1000.Driver.Configure_LEDs
           (Tx_LED_Enable    => Tx_LED_Enable,
            Rx_LED_Enable    => Rx_LED_Enable,
            Rx_OK_LED_Enable => Rx_OK_LED_Enable,
            SFD_LED_Enable   => SFD_LED_Enable,
            Test_Flash       => Test_Flash);
      end Configure_LEDs;


      procedure Configure_Errors (Frame_Timeout : in Boolean;
                                  SFD_Timeout   : in Boolean;
                                  PHR_Error     : in Boolean;
                                  RS_Error      : in Boolean;
                                  FCS_Error     : in Boolean)
      is
         SYS_MASK_Reg : DW1000.Register_Types.SYS_MASK_Type;

      begin
         --  Configure which interrupts are enabled
         DW1000.Registers.SYS_MASK.Read (SYS_MASK_Reg);
         SYS_MASK_Reg.MRXRFTO  := (if Frame_Timeout then 1 else 0);
         SYS_MASK_Reg.MRXSFDTO := (if SFD_Timeout   then 1 else 0);
         SYS_MASK_Reg.MRXPHE   := (if PHR_Error     then 1 else 0);
         SYS_MASK_Reg.MRXRFSL  := (if RS_Error      then 1 else 0);
         SYS_MASK_Reg.MRXFCE   := (if FCS_Error     then 1 else 0);
         DW1000.Registers.SYS_MASK.Write (SYS_MASK_Reg);

         Detect_Frame_Timeout := Frame_Timeout;
         Detect_SFD_Timeout   := SFD_Timeout;
         Detect_PHR_Error     := PHR_Error;
         Detect_RS_Error      := RS_Error;
         Detect_FCS_Error     := FCS_Error;
      end Configure_Errors;

      procedure Force_Tx_Rx_Off
      is
      begin
         DW1000.Driver.Force_Tx_Rx_Off;

         --  The transmitter is now idle.
         Tx.Transmitter.Notify_Tx_Complete;
      end Force_Tx_Rx_Off;

      procedure Set_PAN_ID (PAN_ID : in Bits_16)
      is
      begin
         DW1000.Driver.Write_PAN_ID (PAN_ID);
      end Set_PAN_ID;

      procedure Set_Short_Address (Short_Address : in Bits_16)
      is
      begin
         DW1000.Driver.Write_Short_Address (Short_Address);
      end Set_Short_Address;

      function Get_Part_ID return Bits_32
      is
      begin
         return Part_ID;
      end Get_Part_ID;

      function Get_Lot_ID  return Bits_32
      is
      begin
         return Lot_ID;
      end Get_Lot_ID;

      function PHR_Mode return DW1000.Driver.Physical_Header_Modes
      is
      begin
         if Long_Frames then
            return Extended_Frames;
         else
            return Standard_Frames;
         end if;
      end PHR_Mode;

      procedure DW1000_IRQ
      is
         SYS_STATUS_Reg : DW1000.Register_Types.SYS_STATUS_Type;

         SYS_STATUS_Clear : DW1000.Register_Types.SYS_STATUS_Type
           := (Reserved_1 => 0,
               Reserved_2 => 0,
               others     => 0);

      begin
         DW1000.BSP.Acknowledge_DW1000_IRQ;

         DW1000.Registers.SYS_STATUS.Read (SYS_STATUS_Reg);

         --  The DW1000 User Manual, Section 4.1.6, states that after certain
         --  types of errors the receiver should be reset to ensure that the
         --  next good frame has the correct timestamp. To handle this, we
         --  use the Reset_Rx procedure.

         if SYS_STATUS_Reg.RXRFTO = 1 then
            DW1000.Driver.Reset_Rx;

            if Detect_Frame_Timeout then
               Rx.Receiver.Notify_Receive_Error (Rx.Frame_Timeout);
            end if;
            SYS_STATUS_Clear.RXRFTO := 1;
         end if;

         if SYS_STATUS_Reg.RXSFDTO = 1 then
            if Detect_SFD_Timeout then
               Rx.Receiver.Notify_Receive_Error (Rx.SFD_Timeout);
            end if;
            SYS_STATUS_Clear.RXSFDTO := 1;
         end if;

         if SYS_STATUS_Reg.RXPHE = 1 then
            DW1000.Driver.Reset_Rx;

            if Detect_PHR_Error then
               Rx.Receiver.Notify_Receive_Error (Rx.PHR_Error);
            end if;
            SYS_STATUS_Clear.RXPHE := 1;
         end if;

         if SYS_STATUS_Reg.RXRFSL = 1 then
            DW1000.Driver.Reset_Rx;

            if Detect_RS_Error then
               Rx.Receiver.Notify_Receive_Error (Rx.RS_Error);
            end if;
            SYS_STATUS_Clear.RXRFSL := 1;
         end if;

         if SYS_STATUS_Reg.RXFCG = 1 or SYS_STATUS_Reg.RXDFR = 1 then
            Rx.Receiver.Notify_Frame_Received;
            SYS_STATUS_Clear.RXFCG := 1;

            --  Clear RX flags
            SYS_STATUS_Clear.RXDFR   := 1;
            SYS_STATUS_Clear.RXPRD   := 1;
            SYS_STATUS_Clear.RXSFDD  := 1;
            SYS_STATUS_Clear.LDEDONE := 1;
            SYS_STATUS_Clear.RXPHD   := 1;
         end if;

         if SYS_STATUS_Reg.RXFCE = 1 then
            if Detect_FCS_Error then
               Rx.Receiver.Notify_Receive_Error (Rx.FCS_Error);
            end if;
            SYS_STATUS_Clear.RXFCE := 1;

            --  Clear RX flags
            SYS_STATUS_Clear.RXDFR   := 1;
            SYS_STATUS_Clear.RXPRD   := 1;
            SYS_STATUS_Clear.RXSFDD  := 1;
            SYS_STATUS_Clear.LDEDONE := 1;
            SYS_STATUS_Clear.RXPHD   := 1;
         end if;

         if SYS_STATUS_Reg.TXFRS = 1 then
            --  Frame sent
            Tx.Transmitter.Notify_Tx_Complete;

            -- Clear all TX events
            SYS_STATUS_Clear.AAT   := 1;
            SYS_STATUS_Clear.TXFRS := 1;
            SYS_STATUS_Clear.TXFRB := 1;
            SYS_STATUS_Clear.TXPHS := 1;
            SYS_STATUS_Clear.TXPRS := 1;
         end if;

         SYS_STATUS_Clear.AFFREJ := 1;

         --  Clear events that we have seen.
         DW1000.Registers.SYS_STATUS.Write (SYS_STATUS_Clear);

      end DW1000_IRQ;

   end Driver;


end DecaDriver.Core;
