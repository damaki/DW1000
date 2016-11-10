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

with DW1000.Reception_Quality; use DW1000.Reception_Quality;
with DW1000.Registers;
with DW1000.Register_Driver;

package body DecaDriver.Rx
with SPARK_Mode => On
is


   Null_Frame_Info : constant Frame_Info_Type
     := (RX_TIME_Reg      => (RX_STAMP => 0,
                              FP_INDEX => 0,
                              FP_AMPL1 => 0,
                              RX_RAWST => 0),
         RX_FINFO_Reg     => (RXFLEN   => 0,
                              RXFLE    => 0,
                              RXNSPL   => 0,
                              RXBR     => 0,
                              RNG      => 0,
                              RXPRF    => 0,
                              RXPSR    => 0,
                              RXPACC   => 0,
                              Reserved => 0),
         RX_FQUAL_Reg     => (STD_NOISE => 0,
                              FP_AMPL2  => 0,
                              FP_AMPL3  => 0,
                              CIR_PWR   => 0),
         RXPACC_NOSAT_Reg => (RXPACC_NOSAT => 0),
         RX_TTCKI_Reg     => (RXTTCKI => 0),
         RX_TTCKO_Reg     => (RXTOFS     => 0,
                              RSMPDEL    => 0,
                              RCPHASE    => 0,
                              Reserved_1 => 0,
                              Reserved_2 => 0),
         SFD_LENGTH       => 64,
         Non_Standard_SFD => False);


   function Receive_Timestamp (Frame_Info : in Frame_Info_Type)
                               return Fine_System_Time
   is
   begin
      return To_Fine_System_Time (Frame_Info.RX_TIME_Reg.RX_STAMP);
   end Receive_Timestamp;


   function Receive_Signal_Power (Frame_Info : in Frame_Info_Type)
                                  return Float
   is
      RXBR       : Bits_2;
      SFD_LENGTH : Bits_8;
      RXPACC     : Bits_12;
   begin
      RXBR := Frame_Info.RX_FINFO_Reg.RXBR;
      if RXBR = 2#11# then --  Detect reserved value
         RXBR := 2#10#; --  default to 6.8 Mbps
      end if;

      SFD_LENGTH := Frame_Info.SFD_LENGTH;
      if Frame_Info.Non_Standard_SFD or SFD_LENGTH not in 8 | 16 then
         SFD_LENGTH := 8; --  default to length 8
      end if;

      RXPACC := Adjust_RXPACC
        (RXPACC           => Frame_Info.RX_FINFO_Reg.RXPACC,
         RXPACC_NOSAT     => Frame_Info.RXPACC_NOSAT_Reg.RXPACC_NOSAT,
         RXBR             => RXBR,
         SFD_LENGTH       => SFD_LENGTH,
         Non_Standard_SFD => Frame_Info.Non_Standard_SFD);

      return Receive_Signal_Power
        (Use_16MHz_PRF => Frame_Info.RX_FINFO_Reg.RXPRF = 2#10#,
         RXPACC        => RXPACC,
         CIR_PWR       => Frame_Info.RX_FQUAL_Reg.CIR_PWR);
   end Receive_Signal_Power;


   function First_Path_Signal_Power (Frame_Info : in Frame_Info_Type)
                                     return Float
   is
      RXBR       : Bits_2;
      SFD_LENGTH : Bits_8;
      RXPACC     : Bits_12;
   begin
      RXBR := Frame_Info.RX_FINFO_Reg.RXBR;
      if RXBR = 2#11# then --  Detect reserved value
         RXBR := 2#10#; --  default to 6.8 Mbps
      end if;

      SFD_LENGTH := Frame_Info.SFD_LENGTH;
      if not (SFD_LENGTH in 8 | 16) then
         SFD_LENGTH := 8; --  default to length 8
      end if;

      RXPACC := Adjust_RXPACC
        (RXPACC           => Frame_Info.RX_FINFO_Reg.RXPACC,
         RXPACC_NOSAT     => Frame_Info.RXPACC_NOSAT_Reg.RXPACC_NOSAT,
         RXBR             => RXBR,
         SFD_LENGTH       => SFD_LENGTH,
         Non_Standard_SFD => Frame_Info.Non_Standard_SFD);

      return First_Path_Signal_Power
        (Use_16MHz_PRF => Frame_Info.RX_FINFO_Reg.RXPRF = 2#10#,
         F1            => Frame_Info.RX_TIME_Reg.FP_AMPL1,
         F2            => Frame_Info.RX_FQUAL_Reg.FP_AMPL2,
         F3            => Frame_Info.RX_FQUAL_Reg.FP_AMPL3,
         RXPACC        => RXPACC);
   end First_Path_Signal_Power;


   function Transmitter_Clock_Offset (Frame_Info : in Frame_Info_Type)
                                      return Long_Float
   is
   begin
      return Transmitter_Clock_Offset
        (RXTOFS  => Frame_Info.RX_TTCKO_Reg.RXTOFS,
         RXTTCKI => Frame_Info.RX_TTCKI_Reg.RXTTCKI);
   end Transmitter_Clock_Offset;


   protected body Receiver
   is
      entry Wait (Frame      : in out DW1000.Types.Byte_Array;
                  Length     :    out Frame_Length_Number;
                  Frame_Info :    out Frame_Info_Type;
                  Status     :    out Rx_Status_Type;
                  Overrun    :    out Boolean)
        when Frame_Ready
      is
      begin
         Length     := Frame_Queue (Queue_Head).Length;
         Frame_Info := Frame_Queue (Queue_Head).Frame_Info;
         Status     := Frame_Queue (Queue_Head).Status;
         Overrun    := Frame_Queue (Queue_Head).Overrun;

         if Status = No_Error then
            if Frame'Length >= Length then
               Frame (Frame'First .. Frame'First + Integer (Length - 1))
                 := Frame_Queue (Queue_Head).Frame (1 .. Length);

            else
               Frame := Frame_Queue (Queue_Head).Frame (1 .. Frame'Length);

            end if;
         end if;

         Queue_Head  := Queue_Head + 1;
         Rx_Count    := Rx_Count - 1;
         Frame_Ready := Rx_Count > 0;

      end Wait;

      function Pending_Frames_Count return Natural
      is
      begin
         return Rx_Count;
      end Pending_Frames_Count;

      procedure Discard_Pending_Frames
      is
      begin
         Rx_Count := 0;
      end Discard_Pending_Frames;

      procedure Set_FCS_Check_Enabled (Enabled : in Boolean)
      is
      begin
         DW1000.Driver.Set_FCS_Check_Enabled (Enabled);
      end Set_FCS_Check_Enabled;

      procedure Set_Frame_Filtering_Enabled (Enabled : in Boolean)
      is
      begin
         DW1000.Driver.Set_Frame_Filtering_Enabled (Enabled);
      end Set_Frame_Filtering_Enabled;

      procedure Configure_Frame_Filtering (Behave_As_Coordinator : in Boolean;
                                           Allow_Beacon_Frame    : in Boolean;
                                           Allow_Data_Frame      : in Boolean;
                                           Allow_Ack_Frame       : in Boolean;
                                           Allow_MAC_Cmd_Frame   : in Boolean;
                                           Allow_Reserved_Frame  : in Boolean;
                                           Allow_Frame_Type_4    : in Boolean;
                                           Allow_Frame_Type_5    : in Boolean)
      is
      begin
         DW1000.Driver.Configure_Frame_Filtering
           (Behave_As_Coordinator => Behave_As_Coordinator,
            Allow_Beacon_Frame    => Allow_Beacon_Frame,
            Allow_Data_Frame      => Allow_Data_Frame,
            Allow_Ack_Frame       => Allow_Ack_Frame,
            Allow_MAC_Cmd_Frame   => Allow_MAC_Cmd_Frame,
            Allow_Reserved_Frame  => Allow_Reserved_Frame,
            Allow_Frame_Type_4    => Allow_Frame_Type_4,
            Allow_Frame_Type_5    => Allow_Frame_Type_5);
      end Configure_Frame_Filtering;

      procedure Set_Rx_Double_Buffer (Enabled : in Boolean)
      is
      begin
         DW1000.Driver.Set_Rx_Double_Buffer (Enabled);
      end Set_Rx_Double_Buffer;

      procedure Set_Rx_Auto_Reenable (Enabled : in Boolean)
      is
      begin
         DW1000.Driver.Set_Auto_Rx_Reenable (Enabled);
      end Set_Rx_Auto_Reenable;

      procedure Set_Delayed_Rx_Time(Time : in Coarse_System_Time)
      is
      begin
         DW1000.Driver.Set_Delayed_Tx_Rx_Time (Time);
      end Set_Delayed_Rx_Time;

      procedure Set_Frame_Wait_Timeout (Timeout : in Frame_Wait_Timeout_Time)
      is
      begin
         DW1000.Driver.Set_Rx_Frame_Wait_Timeout (Timeout);
      end Set_Frame_Wait_Timeout;

      procedure Start_Rx_Immediate
      is
      begin
         DW1000.Driver.Start_Rx_Immediate;
      end Start_Rx_Immediate;

      procedure Start_Rx_Delayed (Result  : out Result_Type)
      is
      begin
         DW1000.Driver.Start_Rx_Delayed (Result => Result);
      end Start_Rx_Delayed;

      procedure Notify_Frame_Received
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

               DW1000.Register_Driver.Read_Register
                 (Register_ID => DW1000.Registers.RX_BUFFER_Reg_ID,
                  Sub_Address => 0,
                  Data        =>
                    Frame_Queue (Next_Idx).Frame (1 .. Frame_Length));

               Frame_Queue (Next_Idx).Length  := Frame_Length;
               Frame_Queue (Next_Idx).Status  := No_Error;
               Frame_Queue (Next_Idx).Overrun := Overrun_Occurred;

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
      end Notify_Frame_Received;

      procedure Notify_Receive_Error (Error : in Rx_Status_Type)
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
      end Notify_Receive_Error;

   end Receiver;

end DecaDriver.Rx;
