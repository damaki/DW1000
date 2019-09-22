-------------------------------------------------------------------------------
--  Copyright (c) 2019 Daniel King
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

with DW1000.Constants;         use DW1000.Constants;
with DW1000.Registers;         use DW1000.Registers;
with DW1000.Register_Driver;   use DW1000.Register_Driver;
with DW1000.Reception_Quality; use DW1000.Reception_Quality;
with Interfaces;               use Interfaces;

package body DecaDriver
with SPARK_Mode => On
is

   use Implementation;

   Default_SFD_Timeout : constant DW1000.Driver.SFD_Timeout_Number := 16#1041#;

   Null_Frame_Info : constant Frame_Info_Type
     := (RX_TIME_Reg      => (others   => <>),
         RX_FINFO_Reg     => (others   => <>),
         RX_FQUAL_Reg     => (STD_NOISE => 0,
                              FP_AMPL2  => 0,
                              FP_AMPL3  => 0,
                              CIR_PWR   => 0),
         RXPACC_NOSAT_Reg => (RXPACC_NOSAT => 0),
         RX_TTCKI_Reg     => (RXTTCKI => 0),
         RX_TTCKO_Reg     => (RXTOFS     => 0,
                              RSMPDEL    => 0,
                              RCPHASE    => 0.0,
                              Reserved_1 => 0,
                              Reserved_2 => 0),
         SFD_LENGTH       => 64,
         Non_Standard_SFD => False);

   -------------------------
   --  Receive_Timestamp  --
   -------------------------

   function Receive_Timestamp (Frame_Info : in Frame_Info_Type)
                               return Fine_System_Time
   is
   begin
      return Frame_Info.RX_TIME_Reg.RX_STAMP;
   end Receive_Timestamp;

   ----------------------------
   --  Receive_Signal_Power  --
   ----------------------------

   function Receive_Signal_Power (Frame_Info : in Frame_Info_Type)
                                  return Float
   is
      RXBR       : RX_FINFO_RXBR_Field;
      SFD_LENGTH : Bits_8;
      RXPACC     : RX_FINFO_RXPACC_Field;

   begin
      RXBR := Frame_Info.RX_FINFO_Reg.RXBR;
      if RXBR = Reserved then --  Detect reserved value
         RXBR := Data_Rate_6M8;    --  default to 6.8 Mbps
      end if;

      SFD_LENGTH := Frame_Info.SFD_LENGTH;
      if Frame_Info.Non_Standard_SFD and SFD_LENGTH not in 8 | 16 then
         SFD_LENGTH := 8; --  default to length 8
      end if;

      RXPACC := Adjust_RXPACC
        (RXPACC           => Frame_Info.RX_FINFO_Reg.RXPACC,
         RXPACC_NOSAT     => Frame_Info.RXPACC_NOSAT_Reg.RXPACC_NOSAT,
         RXBR             => RXBR,
         SFD_LENGTH       => SFD_LENGTH,
         Non_Standard_SFD => Frame_Info.Non_Standard_SFD);

      return Receive_Signal_Power
        (Use_16MHz_PRF => Frame_Info.RX_FINFO_Reg.RXPRF = PRF_16MHz,
         RXPACC        => RXPACC,
         CIR_PWR       => Frame_Info.RX_FQUAL_Reg.CIR_PWR);
   end Receive_Signal_Power;

   -------------------------------
   --  First_Path_Signal_Power  --
   -------------------------------

   function First_Path_Signal_Power (Frame_Info : in Frame_Info_Type)
                                     return Float
   is
      RXBR       : RX_FINFO_RXBR_Field;
      SFD_LENGTH : Bits_8;
      RXPACC     : RX_FINFO_RXPACC_Field;
   begin
      RXBR := Frame_Info.RX_FINFO_Reg.RXBR;
      if RXBR = Reserved then --  Detect reserved value
         RXBR := Data_Rate_6M8; --  default to 6.8 Mbps
      end if;

      SFD_LENGTH := Frame_Info.SFD_LENGTH;
      if Frame_Info.Non_Standard_SFD and SFD_LENGTH not in 8 | 16 then
         SFD_LENGTH := 8; --  default to length 8
      end if;

      RXPACC := Adjust_RXPACC
        (RXPACC           => Frame_Info.RX_FINFO_Reg.RXPACC,
         RXPACC_NOSAT     => Frame_Info.RXPACC_NOSAT_Reg.RXPACC_NOSAT,
         RXBR             => RXBR,
         SFD_LENGTH       => SFD_LENGTH,
         Non_Standard_SFD => Frame_Info.Non_Standard_SFD);

      return First_Path_Signal_Power
        (Use_16MHz_PRF => Frame_Info.RX_FINFO_Reg.RXPRF = PRF_16MHz,
         F1            => Frame_Info.RX_TIME_Reg.FP_AMPL1,
         F2            => Frame_Info.RX_FQUAL_Reg.FP_AMPL2,
         F3            => Frame_Info.RX_FQUAL_Reg.FP_AMPL3,
         RXPACC        => RXPACC);
   end First_Path_Signal_Power;

   --------------------------------
   --  Transmitter_Clock_Offset  --
   --------------------------------

   function Transmitter_Clock_Offset (Frame_Info : in Frame_Info_Type)
                                      return Long_Float
   is
   begin
      return Transmitter_Clock_Offset
        (RXTOFS  => Frame_Info.RX_TTCKO_Reg.RXTOFS,
         RXTTCKI => Frame_Info.RX_TTCKI_Reg.RXTTCKI);
   end Transmitter_Clock_Offset;

   --------------
   --  Driver  --
   --------------

   protected body Driver
   is

      ------------------
      --  Initialize  --
      ------------------

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

            --  High 16 bits are the antenna delay with a 64 MHz PRF.
            --  Low 16 bits are the antenna delay with a 16 MHz PRF.
            Antenna_Delay_PRF_16 := To_Antenna_Delay_Time (Bits_16 (Word and 16#FFFF#));

            Word := Shift_Right (Word, 16);
            Antenna_Delay_PRF_64 := To_Antenna_Delay_Time (Bits_16 (Word and 16#FFFF#));
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

         DW1000.Driver.Enable_Clocks (Force_Sys_PLL);
         DW1000.Driver.Enable_Clocks (Enable_All_Seq);

         --  Store a local copy of the SYS_CFG register
         DW1000.Registers.SYS_CFG.Read (SYS_CFG_Reg);

         --  Configure IRQs
         DW1000.Registers.SYS_MASK.Read (SYS_MASK_Reg);
         SYS_MASK_Reg.MRXRFTO  := Not_Masked;
         SYS_MASK_Reg.MRXSFDTO := Not_Masked;
         SYS_MASK_Reg.MRXPHE   := Not_Masked;
         SYS_MASK_Reg.MRXRFSL  := Not_Masked;
         SYS_MASK_Reg.MRXDFR   := Not_Masked; --  Always detect frame received
         SYS_MASK_Reg.MTXFRS   := Not_Masked; --  Always detect frame sent
         DW1000.Registers.SYS_MASK.Write (SYS_MASK_Reg);

         Detect_Frame_Timeout := True;
         Detect_SFD_Timeout   := True;
         Detect_PHR_Error     := True;
         Detect_RS_Error      := True;
         Detect_FCS_Error     := True;

      end Initialize;

      -----------------
      --  Configure  --
      -----------------

      procedure Configure (Config : in Configuration_Type)
      is
         SFD_Timeout : DW1000.Driver.SFD_Timeout_Number;

      begin

         --  110 kbps data rate has special handling
         if Config.Data_Rate = DW1000.Driver.Data_Rate_110k then
            SYS_CFG_Reg.RXM110K := SFD_110K;
         else
            SYS_CFG_Reg.RXM110K := SFD_850K_6M8;
         end if;

         --  Set physical header mode (standard or extended frames)
         Long_Frames := Config.PHR_Mode = Extended_Frames;
         SYS_CFG_Reg.PHR_MODE := SYS_CFG_PHR_MODE_Field'Val
           (Physical_Header_Modes'Pos (Config.PHR_Mode));

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
               TXBR     => (case Config.Data_Rate is
                               when Data_Rate_110k => Data_Rate_110K,
                               when Data_Rate_850k => Data_Rate_850K,
                               when Data_Rate_6M8 => Data_Rate_6M8),
               TR       => Enabled,
               TXPRF    => (if Config.PRF = PRF_16MHz then PRF_16MHz else PRF_64MHz),
               TXPSR    =>
                 (case Config.Tx_Preamble_Length is
                     when PLEN_64 | PLEN_128 | PLEN_256 | PLEN_512 => PLEN_64,
                     when PLEN_1024 | PLEN_1536 | PLEN_2048        => PLEN_1024,
                     when others                                   => PLEN_4096),
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

      ------------------------
      --  Configure_Errors  --
      ------------------------

      procedure Configure_Errors (Frame_Timeout : in Boolean;
                                  SFD_Timeout   : in Boolean;
                                  PHR_Error     : in Boolean;
                                  RS_Error      : in Boolean;
                                  FCS_Error     : in Boolean)
      is
      begin
         Detect_Frame_Timeout := Frame_Timeout;
         Detect_SFD_Timeout   := SFD_Timeout;
         Detect_PHR_Error     := PHR_Error;
         Detect_RS_Error      := RS_Error;
         Detect_FCS_Error     := FCS_Error;
      end Configure_Errors;

      -----------------------
      --  Force_Tx_Rx_Off  --
      -----------------------

      procedure Force_Tx_Rx_Off
      is
      begin
         DW1000.Driver.Force_Tx_Rx_Off;

         --  Set the Tx Complete flag to True to ensure any task waiting for
         --  the current transmission to complete.
         Ada.Synchronous_Task_Control.Set_True (Tx_Complete_Flag);
      end Force_Tx_Rx_Off;

      -------------------
      --  Get_Part_ID  --
      -------------------

      function Get_Part_ID return Bits_32
      is
      begin
         return Part_ID;
      end Get_Part_ID;

      ------------------
      --  Get_Lot_ID  --
      ------------------

      function Get_Lot_ID  return Bits_32
      is
      begin
         return Lot_ID;
      end Get_Lot_ID;

      ----------------
      --  PHR_Mode  --
      ----------------

      function PHR_Mode return DW1000.Driver.Physical_Header_Modes
      is
      begin
         if Long_Frames then
            return Extended_Frames;
         else
            return Standard_Frames;
         end if;
      end PHR_Mode;

      --------------------------
      --  Start_Tx_Immediate  --
      --------------------------

      procedure Start_Tx_Immediate (Rx_After_Tx     : in Boolean;
                                    Auto_Append_FCS : in Boolean)
      is
      begin
         DW1000.Driver.Start_Tx_Immediate (Rx_After_Tx, Auto_Append_FCS);

         Ada.Synchronous_Task_Control.Set_False (Tx_Complete_Flag);
      end Start_Tx_Immediate;

      ------------------------
      --  Start_Tx_Delayed  --
      ------------------------

      procedure Start_Tx_Delayed
        (Rx_After_Tx : in     Boolean;
         Result      :    out DW1000.Driver.Result_Type)
      is
      begin
         DW1000.Driver.Start_Tx_Delayed (Rx_After_Tx => Rx_After_Tx,
                                         Result      => Result);

         if Result = DW1000.Driver.Success then
            Ada.Synchronous_Task_Control.Set_False (Tx_Complete_Flag);
         else
            Ada.Synchronous_Task_Control.Set_True (Tx_Complete_Flag);
         end if;
      end Start_Tx_Delayed;

      ---------------
      --  Rx_Wait  --
      ---------------

      entry Rx_Wait (Frame      : in out DW1000.Types.Byte_Array;
                     Length     :    out Frame_Length_Number;
                     Frame_Info :    out Frame_Info_Type;
                     Status     :    out Rx_Status_Type;
                     Overrun    :    out Boolean)
        when Frame_Ready
      is
      begin
         pragma Assume (Frame_Ready,
                        "barrier condition is true on entry");

         pragma Assume ((if Frame_Ready then Rx_Count > 0),
                        "Invariant for the Receiver protected object");

         Length     := Frame_Queue (Queue_Head).Length;
         Frame_Info := Frame_Queue (Queue_Head).Frame_Info;
         Status     := Frame_Queue (Queue_Head).Status;
         Overrun    := Frame_Queue (Queue_Head).Overrun;

         if Status = No_Error then
            if Length > 0 then
               if Frame'Length >= Length then
                  Frame (Frame'First .. Frame'First + Integer (Length - 1))
                    := Frame_Queue (Queue_Head).Frame (1 .. Length);

               else
                  Frame := Frame_Queue (Queue_Head).Frame (1 .. Frame'Length);

               end if;
            end if;

         else
            Length := 0;
         end if;

         Queue_Head  := Queue_Head + 1;
         Rx_Count    := Rx_Count - 1;
         Frame_Ready := Rx_Count > 0;

      end Rx_Wait;

      ----------------------------
      --  Pending_Frames_Count  --
      ----------------------------

      function Pending_Frames_Count return Natural
      is
      begin
         return Rx_Count;
      end Pending_Frames_Count;

      ------------------------------
      --  Discard_Pending_Frames  --
      ------------------------------

      procedure Discard_Pending_Frames
      is
      begin
         Rx_Count := 0;
      end Discard_Pending_Frames;

      --------------------------
      --  Start_Rx_Immediate  --
      --------------------------

      procedure Start_Rx_Immediate
      is
      begin
         DW1000.Driver.Start_Rx_Immediate;
      end Start_Rx_Immediate;

      ------------------------
      --  Start_Rx_Delayed  --
      ------------------------

      procedure Start_Rx_Delayed (Result  : out Result_Type)
      is
      begin
         DW1000.Driver.Start_Rx_Delayed (Result => Result);
      end Start_Rx_Delayed;

      ----------------------
      --  Frame_Received  --
      ----------------------

      procedure Frame_Received
      is
         RX_FINFO_Reg : DW1000.Register_Types.RX_FINFO_Type;

         Frame_Length : Natural;
         Next_Idx     : Rx_Frame_Queue_Index;

      begin
         --  Read the frame length from the DW1000
         DW1000.Registers.RX_FINFO.Read (RX_FINFO_Reg);
         Frame_Length := Natural (RX_FINFO_Reg.RXFLEN) +
                         Natural (RX_FINFO_Reg.RXFLE) * 2**7;

         --  If a frame is received whose length is larger than the configured
         --  maximum frame size, then truncate the frame length.
         if Frame_Length > Frame_Length_Number'Last then
            Frame_Length := Frame_Length_Number'Last;
         end if;

         pragma Assert (Frame_Length in Frame_Length_Number);

         if Frame_Length > 0 then
            if Rx_Count = Frame_Queue'Length then
               Overrun_Occurred := True;

            else
               Next_Idx := Queue_Head + Rx_Frame_Queue_Index (Rx_Count);

               Rx_Count := Rx_Count + 1;

               Frame_Queue (Next_Idx).Status  := No_Error;
               Frame_Queue (Next_Idx).Length  := Frame_Length;
               Frame_Queue (Next_Idx).Overrun := Overrun_Occurred;

               DW1000.Register_Driver.Read_Register
                 (Register_ID => DW1000.Registers.RX_BUFFER_Reg_ID,
                  Sub_Address => 0,
                  Data        =>
                    Frame_Queue (Next_Idx).Frame (1 .. Frame_Length));

               Overrun_Occurred := False;

               DW1000.Registers.RX_FINFO.Read
                 (Frame_Queue (Next_Idx).Frame_Info.RX_FINFO_Reg);

               DW1000.Registers.RX_FQUAL.Read
                 (Frame_Queue (Next_Idx).Frame_Info.RX_FQUAL_Reg);

               DW1000.Registers.RX_TIME.Read
                 (Frame_Queue (Next_Idx).Frame_Info.RX_TIME_Reg);

               DW1000.Registers.RX_TTCKI.Read
                 (Frame_Queue (Next_Idx).Frame_Info.RX_TTCKI_Reg);

               DW1000.Registers.RX_TTCKO.Read
                 (Frame_Queue (Next_Idx).Frame_Info.RX_TTCKO_Reg);

               declare
                  Byte : Byte_Array (1 .. 1);
               begin
                  --  Don't read the entire USR_SFD register. We only need to
                  --  read the first byte (the SFD_LENGTH field).
                  DW1000.Register_Driver.Read_Register
                    (Register_ID => DW1000.Registers.USR_SFD_Reg_ID,
                     Sub_Address => 0,
                     Data        => Byte);
                  Frame_Queue (Next_Idx).Frame_Info.SFD_LENGTH := Byte (1);
               end;

               --  Check the CHAN_CTRL register to determine whether or not a
               --  non-standard SFD is being used.
               declare
                  CHAN_CTRL_Reg : CHAN_CTRL_Type;
               begin
                  DW1000.Registers.CHAN_CTRL.Read (CHAN_CTRL_Reg);
                  Frame_Queue (Next_Idx).Frame_Info.Non_Standard_SFD
                    := CHAN_CTRL_Reg.DWSFD = 1;
               end;
            end if;

            Frame_Ready := True;
         end if;

         DW1000.Driver.Toggle_Host_Side_Rx_Buffer_Pointer;
      end Frame_Received;

      ---------------------
      --  Receive_Error  --
      ---------------------

      procedure Receive_Error (Error : in Rx_Status_Type)
      is
         Next_Idx     : Rx_Frame_Queue_Index;

      begin
         if Rx_Count = Frame_Queue'Length then
            Overrun_Occurred := True;

         else
            Next_Idx := Queue_Head + Rx_Frame_Queue_Index (Rx_Count);

            Rx_Count := Rx_Count + 1;

            Frame_Queue (Next_Idx).Length     := 0;
            Frame_Queue (Next_Idx).Status     := Error;
            Frame_Queue (Next_Idx).Overrun    := Overrun_Occurred;
            Frame_Queue (Next_Idx).Frame_Info := Null_Frame_Info;
            Overrun_Occurred := False;
         end if;

         Frame_Ready := True;
      end Receive_Error;

      ------------------
      --  DW1000_IRQ  --
      ------------------

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

         --  Frame timeout?
         if SYS_STATUS_Reg.RXRFTO = 1 then
            DW1000.Driver.Reset_Rx;

            if Detect_Frame_Timeout then
               Receive_Error (Frame_Timeout);
            else
               DW1000.Driver.Start_Rx_Immediate;
            end if;
            SYS_STATUS_Clear.RXRFTO := 1;
         end if;

         --  SFD timeout?
         if SYS_STATUS_Reg.RXSFDTO = 1 then
            if Detect_SFD_Timeout then
               Receive_Error (SFD_Timeout);
            else
               DW1000.Driver.Start_Rx_Immediate;
            end if;
            SYS_STATUS_Clear.RXSFDTO := 1;
         end if;

         --  Physical header error?
         if SYS_STATUS_Reg.RXPHE = 1 then
            DW1000.Driver.Reset_Rx;

            if Detect_PHR_Error then
               Receive_Error (PHR_Error);
            else
               DW1000.Driver.Start_Rx_Immediate;
            end if;
            SYS_STATUS_Clear.RXPHE := 1;
         end if;

         --  Reed-Solomon error correction error?
         if SYS_STATUS_Reg.RXRFSL = 1 then
            DW1000.Driver.Reset_Rx;

            if Detect_RS_Error then
               Receive_Error (RS_Error);
            else
               DW1000.Driver.Start_Rx_Immediate;
            end if;
            SYS_STATUS_Clear.RXRFSL := 1;
         end if;

         --  Packet received?
         if SYS_STATUS_Reg.RXDFR = 1 then
            if SYS_STATUS_Reg.RXFCE = 1 then
               if Detect_FCS_Error then
                  Receive_Error (FCS_Error);
               else
                  DW1000.Driver.Start_Rx_Immediate;
               end if;
            else
               Frame_Received;
            end if;

            --  Clear RX flags
            SYS_STATUS_Clear.RXDFR   := 1;
            SYS_STATUS_Clear.RXFCG   := 1;
            SYS_STATUS_Clear.RXFCE   := 1;
            SYS_STATUS_Clear.RXPRD   := 1;
            SYS_STATUS_Clear.RXSFDD  := 1;
            SYS_STATUS_Clear.LDEDONE := 1;
            SYS_STATUS_Clear.RXPHD   := 1;
         end if;

         --  Transmit complete?
         if SYS_STATUS_Reg.TXFRS = 1 then
            --  Frame sent
            Ada.Synchronous_Task_Control.Set_True (Tx_Complete_Flag);

            -- Clear all TX events
            SYS_STATUS_Clear.AAT   := 1;
            SYS_STATUS_Clear.TXFRS := 1;
            SYS_STATUS_Clear.TXFRB := 1;
            SYS_STATUS_Clear.TXPHS := 1;
            SYS_STATUS_Clear.TXPRS := 1;
         end if;

         SYS_STATUS_Clear.AFFREJ := 1;

         --  Clear all events that we have seen.
         DW1000.Registers.SYS_STATUS.Write (SYS_STATUS_Clear);

      end DW1000_IRQ;

   end Driver;

   pragma Annotate
     (GNATprove, False_Positive,
      "call to potentially blocking subprogram ""dw1000.bsp.",
      "Procedures in DW1000.BSP are not blocking");

   pragma Annotate
     (GNATprove, False_Positive,
      "call to potentially blocking subprogram ""Acknowledge_DW1000_IRQ",
      "DW1000.BSP.Acknowledge_DW1000_IRQ is not blocking");

end DecaDriver;
