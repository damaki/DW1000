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

with Ada.Real_Time;
with Ada.Interrupts;
with DecaDriver_Config;
with DW1000.Constants;
with DW1000.Driver;          use DW1000.Driver;
with DW1000.BSP;
with DW1000.Register_Types;
with DW1000.System_Time;     use DW1000.System_Time;
with Dw1000.Types;           use DW1000.Types;
with System;

--  @summary
--  High-level Ravenscar driver for typical usage of the DW1000.
--
--  @description
--  This driver provides a high-level API for configuring and using the DW1000.
--
--  This driver consists of three protected objects:
--    * Driver: Provides general configuration for the DW1000.
--    * Receiver: Provides functionality for receiving packets.
--    * Transmitter: Provides functionality for transmitting packets.
--
--  Before the DW1000 can be used, it must be initialized and configured.
--  This is done using the Driver.Initialize and Driver.Configure procedures.
--  The Initialize procedure initializes the DW1000 and, if specified by the
--  user, loads certain values from the DW1000 OTP memory, such as the antenna
--  delay. Below is an example of initializing the DW1000 and driver:
--
--     DecaDriver.Driver.Initialize (Load_Antenna_Delay   => True,
--                                   Load_XTAL_Trim       => True,
--                                   Load_Tx_Power_Levels => True,
--                                   Load_UCode_From_ROM  => True);
--
--  After the DW1000 has been initialized, it can be set to a specific
--  configuration (UWB channel, PRF, preamble code, etc...) using the Configure
--  procedure. Below is an example of configuring the DW1000:
--
--     DecaDriver.Driver.Configure (DecaDriver.Configuration_Type'
--        (Channel             => 1,
--         PRF                 => DW1000.Driver.PRF_64MHz,
--         Tx_Preamble_Length  => DW1000.Driver.PLEN_1024,
--         Tx_PAC              => DW1000.Driver.PAC_32,
--         Tx_Preamble_Code    => 9,
--         Rx_Preamble_Code    => 9,
--         Use_Nonstandard_SFD => False,
--         Data_Rate           => DW1000.Driver.Data_Rate_850k,
--         PHR_Mode            => DW1000.Driver.Standard_Frames,
--         Enable_Smart_Power  => True));
--
--  Note that some configuration parameters use values that are defined in the
--  package DW1000.Driver.
--
--  Once the driver is initialized and configured then packets can be sent
--  and received. Sending a packet is split into the following steps:
--    1. Write the data into the DW1000 transmit buffer.
--    2. Set the frame length information.
--    3. Start the transmission.
--
--  Below is an example of sending a zeroized packet of 10 bytes:
--
--     declare
--        Data : DW1000.Types.Byte_Array(1 .. 10) := (others => 0);
--     begin
--        DecaDriver.Transmitter.Set_Tx_Data (Data   => Data,
--                                            Offset => 0);
--        DecaDriver.Transmitter.Set_Tx_Frame_Length (Length => Data'Length,
--                                                    Offset => 0);
--        DecaDriver.Transmitter.Start_Tx_Immediate (Rx_After_Tx => False);
--     end;
--
--  Note that the receiver can be automatically enabled by the DW1000 after the
--  transmission has completed. This feature is enabled by setting Rx_After_Tx
--  to True when calling Start_Tx_Immediate.
--
--  To wait for the transmission to be completed, you can wait using the
--  Wait_For_Tx_Complete entry. This entry will block until the DW1000 signals
--  to the driver that the transmission has been completed. An example is shown
--  below:
--
--     DecaDriver.Transmitter.Wait_For_Tx_Complete;
--
--  Packets can only be received when the receiver is enabled. To enable the
--  receiver immediately (without delay), then use the
--  Receiver.Start_Rx_Immediate procedure. Then, to block until a packet is
--  received, use the Receiver.Wait entry. An example of waiting for a packet
--  is shown below:
--
--     declare
--        Frame_Data : DW1000.Types.Byte_Array (DecaDriver.Frame_Length_Number);
--        Frame_Size : DW1000.Types.Frame_Length_Number;
--        Timestamp  : DW1000.System_Time.Fine_System_Time;
--        Error      : DecaDriver.Rx_Errors;
--        Overrun    : Boolean;
--     begin
--        DecaDriver.Receiver.Start_Rx_Immediate;
--        DecaDriver.Receiver.Wait (Frame     => Frame_Data,
--                                  Size      => Frame_Size,
--                                  Timestamp => Timestamp,
--                                  Error     => Error,
--                                  Overrun   => Overrun);
--
--        if Error = DecaDriver.No_Error then
--           --  No error occurred
--        else
--           --  An error occurred during packet reception.
--        end if;
--
--        if Overrun then
--           -- A packet was received before this packet, but it was dropped
--           -- because there was no free space in the receive queue.
--        end if;
--     end;
--
--  During packet reception it is possible for several different types of error
--  to occur. The Receiver driver can be configured to ignore certain errors,
--  so that the Wait entry will only wait for valid frames to be received.
--  To configure which errors are filtered, use the Driver.Configure_Errors
--  procedure. An example of using this procedure to disable all receive error
--  notifications (only wait for valid packets) is shown below:
--
--     DecaDriver.Driver.Configure_Errors (Frame_Timeout => False,
--                                         SFD_Timeout   => False,
--                                         PHR_Error     => False,
--                                         RS_Error      => False,
--                                         FCS_Error     => False);
--
--  By default, all errors are suppressed so that the Receiver.Wait entry will
--  only capture frames that are received without errors.
--
--  It is possible to enable the transmitter or receiver at a specific time,
--  rather than starting the transmission/reception immediately. This is known
--  as the "delayed tx/rx" feature. Typically, this is used to enable the
--  transmitter or receiver after a delay relative to a previous transmission
--  or reception. Below is an example of waiting for a packet to be received,
--  and then transmitting the received packet 10 milliseconds after the packet
--  was received:
--
--     declare
--        Frame_Data : DW1000.Types.Byte_Array (DecaDriver.Frame_Length_Number);
--        Frame_Size : DW1000.Types.Frame_Length_Number;
--        Rx_Time    : DW1000.System_Time.Fine_System_Time;
--        Error      : DecaDriver.Rx_Errors;
--        Overrun    : Boolean;
--
--        Tx_Time    : DW1000.System_Time.Coarse_System_Time;
--        Tx_Result  : DW1000.Driver.Result_Codes;
--     begin
--        --  Wait for a packet
--        DecaDriver.Receiver.Start_Rx_Immediate;
--        DecaDriver.Receiver.Wait (Frame     => Frame_Data,
--                                  Size      => Frame_Size,
--                                  Timestamp => Rx_Time,
--                                  Error     => Error,
--                                  Overrun   => Overrun);
--
--        if Error = DecaDriver.No_Error then
--           --  Compute the transmit time (10 ms after the receive time)
--           Tx_Time :=
--              DW1000.System_Time.To_Coarse_System_Time (
--                 DW1000.System_Time.System_Time_Offset (Rx_Time, 0.01));
--
--           -- Configure the time at which the transmitter should be enabled
--           DecaDriver.Transmitter.Set_Delayed_Tx_Time (Time => Tx_Time);
--
--           -- Begin the delayed transmission
--           DecaDriver.Transmitter.Start_Tx_Delayed (Rx_After_Tx => False,
--                                                    Result      => Tx_Result);
--
--           if Result = DW1000.Driver.Success then
--              -- The delayed transmission was configured successfully.
--           else
--              -- Delayed transmit failed. The transmit time is already passed
--           end if;
--        end if;
--     end;
--
--  The delayed transmission can fail if the target transmit time has already
--  passed when Transmitter.Start_Tx_Delayed is called. E.g. in the above
--  example, the delayed transmit will fail if Start_Tx_Delayed is called after
--  the 10 ms delay has already passed. This can happen in cases such as the
--  task being blocked for too long by a higher-priority task.
package DecaDriver
with SPARK_Mode => On
is
   type Configuration_Type is record
      Channel             : DW1000.Driver.Channel_Number;
      PRF                 : DW1000.Driver.PRF_Type;
      Tx_Preamble_Length  : DW1000.Driver.Preamble_Lengths;
      Tx_PAC              : DW1000.Driver.Preamble_Acq_Chunk_Length;
      Tx_Preamble_Code    : DW1000.Driver.Preamble_Code_Number;
      Rx_Preamble_Code    : DW1000.Driver.Preamble_Code_Number;
      Use_Nonstandard_SFD : Boolean;
      Data_Rate           : DW1000.Driver.Data_Rates;
      PHR_Mode            : DW1000.Driver.Physical_Header_Modes;
      SFD_Timeout         : DW1000.Driver.SFD_Timeout_Number;
      Enable_Smart_Power  : Boolean;
   end record;

   subtype Frame_Length_Number is Natural range 0 .. 1024;

   type Rx_Errors is (No_Error,
                      Frame_Timeout,
                      Preamble_Timeout,
                      SFD_Timeout,
                      RS_Error,
                      FCS_Error);

   type Rx_Frame_Type is record
      Size      : Frame_Length_Number;
      Frame     : Byte_Array (Frame_Length_Number);
      Timestamp : Fine_System_Time;
      Error     : Rx_Errors;
      Overrun   : Boolean;
   end record
     with Dynamic_Predicate => (if Rx_Frame_Type.Error = No_Error
                                then Rx_Frame_Type.Size > 0
                                else (Rx_Frame_Type.Size = 0
                                      and Rx_Frame_Type.Timestamp = 0.0));

   type Rx_Frame_Queue_Index is mod DecaDriver_Config.Receiver_Queue_Size;

   type Rx_Frame_Queue_Type is
     array (Rx_Frame_Queue_Index)
     of Rx_Frame_Type;

   type Tx_Power_Array is array (Natural range 0 .. 11) of Bits_32;

   protected type Receiver_Type
     with Interrupt_Priority => DecaDriver_Config.Driver_Priority
   is
      entry Wait (Frame     : in out Byte_Array;
                  Size      :    out Frame_Length_Number;
                  Timestamp :    out Fine_System_Time;
                  Error     :    out Rx_Errors;
                  Overrun   :    out Boolean)
      with Depends => (Frame         => + Receiver_Type,
                       Timestamp     => Receiver_Type,
                       Size          => Receiver_Type,
                       Receiver_Type => Receiver_Type,
                       Error         => Receiver_Type,
                       Overrun       => Receiver_Type),
        Pre => Frame'Length > 0,
        Post => (if Error = No_Error
                 then Size in 1 .. DW1000.Constants.RX_BUFFER_Length
                 else Size = 0 and Timestamp = 0.0);
      --  Waits for a frame to be received, or an error. When a frame is
      --  received (or if one has been previously received and is waiting to be
      --  read) then the frame's content and size are copied to the Frame and
      --  Size arguments.
      --
      --  If any of the enabled errors occurs (e.g. an FCS error is detected)
      --  then the Error argument is set to specify the type of receive error
      --  that occurred, Size is set to 0, and the contents of the Frame array
      --  are unmodified.
      --
      --  If no error occurs (Error is set to No_Error) then the frame contents
      --  are copied to the Frame array, and Size is set to the length of the
      --  frame (in bytes). If the Frame array is too small to store the
      --  received frame then the frame's contents are truncated, but the Size
      --  argument still reflects the frame's true size.
      --
      --  If the Frame array is larger than the received frame then the extra
      --  bytes in the Frame array are unmodified.

      function Pending_Frames_Count return Natural
        with Post => Pending_Frames_Count'Result <= 2;
      --  Returns the number of received frames that are waiting to be read.

      procedure Set_Delayed_Rx_Time(Time : in Coarse_System_Time)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Time,
                    Receiver_Type           => Receiver_Type);
      --  Set the time at which the receiver is turned on and the frame is
      --  sent, when using the delayed receive feature.
      --
      --  This procedure should be called before calling Start_Tx to set the
      --  time for the packet. Furthermore, the Delayed_Tx parameter to
      --  Start_Tx must be set to True, otherwise the transmit time is ignored
      --  and the packet is sent immediately when Start_Tx is called.
      --
      --  Although the System_Time type represents a fine grained time (at
      --  least 15.65 picoseconds), the DW1000 ignores the low order bits
      --  of the delayed tx time so the actual precision is approximately
      --  8.013 nanoseconds.
      --
      --  WARNING: The receiver and transmitter both share the same tx time
      --  register. So calling this procedure will overwrite any delayed
      --  receive time.
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Set_Delayed_Rx_Time""",
         "Procedures in DW1000.BSP are not blocking");


      procedure Start_Rx_Immediate
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                    Receiver_Type           => Receiver_Type);
      --  Turn on the receiver immediately (without delay).
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Start_Rx_Immediate""",
         "Procedures in DW1000.BSP are not blocking");


      procedure Start_Rx_Delayed (Result  : out Result_Type)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                    Result                  => DW1000.BSP.Device_State,
                    Receiver_Type           => Receiver_Type);
      --  Turn on the receiver at the configured delay time.
      --
      --  The time at which the receiver should be enabled is programmed
      --  using the Set_Delayed_Rx_Time procedure, which must be called before
      --  calling this procedure.
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Start_Rx_Delayed""",
         "Procedures in DW1000.BSP are not blocking");


      procedure Notify_Frame_Received
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Receiver_Type,
                    Receiver_Type           => + DW1000.BSP.Device_State);
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Notify_Frame_Received""",
         "Procedures in DW1000.BSP are not blocking");


      --  Reads a received frame from the DW1000.
      --
      --  WARNING: This is intended to only be called by the DecaDriver IRQ
      --  when the DW1000 signals that a frame has been received. This procedure
      --  should not be called by the user.

      procedure Notify_Receive_Error (Error : in Rx_Errors)
        with Depends => (Receiver_Type => + Error),
        Pre => Error /= No_Error;

   private

      Frame_Queue : Rx_Frame_Queue_Type :=
                      (others => (Size      => 0,
                                  Timestamp => 0.0,
                                  Frame     => (others => 0),
                                  Error     => No_Error,
                                  Overrun   => False));
      --  Cyclic buffer for storing received frames, read from the DW1000.

      Queue_Head       : Rx_Frame_Queue_Index := Rx_Frame_Queue_Index'Last;
      Rx_Count         : Natural              := 0;
      Overrun_Occurred : Boolean              := False;
      Frame_Ready      : Boolean              := False;
   end Receiver_Type;


   protected type Transmitter_Type
     with Interrupt_Priority => DecaDriver_Config.Driver_Priority
   is

      entry Wait_For_Tx_Complete;
      --  Wait for an in in-progress transmission to finish.
      --
      --  This entry blocks whilst the transmitter is busy sending a packet.
      --  Otherwise, when the transmitter is idle this entry does not block.

      function Is_Tx_Complete return Boolean;
      --  Check if the transmitter is busy.
      --
      --  Returns True when the transmitter is idle, and False otherwise.

      procedure Set_Tx_Data (Data   : in Byte_Array;
                             Offset : in Natural)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Data,
                                                Offset),
                    Transmitter_Type        => Transmitter_Type),
        Pre => (Data'Length in 1 .. 1024 and then
                Offset < 1024            and then
                Data'Length + Offset <= 1024);
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Set_Tx_Data""",
         "Procedures in DW1000.BSP are not blocking");

      procedure Set_Tx_Frame_Length (Length : in Natural;
                                     Offset : in Natural)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Length,
                                                Offset),
                    Transmitter_Type        => Transmitter_Type),
        Pre => (Length in 1 .. DW1000.Constants.TX_BUFFER_Length and then
                Offset < DW1000.Constants.TX_BUFFER_Length       and then
                Length + Offset <= DW1000.Constants.TX_BUFFER_Length);
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Set_Tx_Frame_Length""",
         "Procedures in DW1000.BSP are not blocking");

      procedure Set_Delayed_Tx_Time(Time : in Coarse_System_Time)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Time,
                    Transmitter_Type        => Transmitter_Type);
      --  Set the time at which the transmitter is turned on and the frame is
      --  sent, when using the delayed transmit feature.
      --
      --  This procedure should be called before calling Start_Tx to set the
      --  time for the packet. Furthermore, the Delayed_Tx parameter to
      --  Start_Tx must be set to True, otherwise the transmit time is ignored
      --  and the packet is sent immediately when Start_Tx is called.
      --
      --  Although the System_Time type represents a fine grained time (at
      --  least 15.65 picoseconds), the DW1000 ignores the low order bits
      --  of the delayed tx time so the actual precision is approximately
      --  8.013 nanoseconds.
      --
      --  WARNING: The receiver and transmitter both share the same tx time
      --  register. So calling this procedure will overwrite any delayed
      --  receive time.
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Set_Delayed_Tx_Time""",
         "Procedures in DW1000.BSP are not blocking");


      procedure Start_Tx_Immediate (Rx_After_Tx : in     Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Rx_After_Tx),
                    Transmitter_Type        => Transmitter_Type);
      --  Start a transmission of a packet immediate (without a delay).
      --
      --  The packet data is set by calling Set_Tx_Data to program the data
      --  into the DW1000's transmit buffer.
      --
      --  The frame length information must be set before calling Start_Tx
      --  by calling Set_Tx_Frame_Length.
      --
      --  If Rx_After_Tx is set to True then the receiver is automatically
      --  enabled after the transmission is completed.
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Start_Tx_Immediate""",
         "Procedures in DW1000.BSP are not blocking");


      procedure Start_Tx_Delayed
        (Rx_After_Tx : in     Boolean;
         Result      :    out DW1000.Driver.Result_Type)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Rx_After_Tx),
                    Result                  => (DW1000.BSP.Device_State,
                                                Rx_After_Tx),
                    Transmitter_Type        => (Transmitter_Type,
                                                DW1000.BSP.Device_State,
                                                Rx_After_Tx));
      --  Start a delayed transmission of a packet.
      --
      --  The packet data is set by calling Set_Tx_Data to program the data
      --  into the DW1000's transmit buffer.
      --
      --  The frame length information must be set before calling Start_Tx
      --  by calling Set_Tx_Frame_Length.
      --
      --  If Rx_After_Tx is set to True then the receiver is automatically
      --  enabled after the transmission is completed.
      --
      --  Note that the time at which the packet should be transmitted must be
      --  set before calling Start_Tx_Delayed, by using the Set_Tx_Time
      --  procedure.
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Start_Tx_Delayed""",
         "Procedures in DW1000.BSP are not blocking");

      procedure Notify_Tx_Complete;
      --  Notify the driver that the transmit is complete.
      --
      --  WARNING: This procedure is intended to be called only by the DW1000
      --  IRQ. This procedure should not be called by the user.

   private
      Tx_Idle : Boolean := True;
      --  True when the transmitter is idle, False otherwise.

   end Transmitter_Type;


   Receiver    : Receiver_Type;
   Transmitter : Transmitter_Type;


   protected type Driver_Type
     with Interrupt_Priority => DecaDriver_Config.Driver_Priority
   is

      procedure Initialize (Load_Antenna_Delay   : in Boolean;
                            Load_XTAL_Trim       : in Boolean;
                            Load_Tx_Power_Levels : in Boolean;
                            Load_UCode_From_ROM  : in Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State,
                        Input  => Ada.Real_Time.Clock_Time),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Driver_Type,
                                                Load_Antenna_Delay,
                                                Load_XTAL_Trim,
                                                Load_Tx_Power_Levels,
                                                Load_UCode_From_ROM),
                    Driver_Type             => + (DW1000.BSP.Device_State,
                                                  Load_Antenna_Delay,
                                                  Load_XTAL_Trim,
                                                  Load_Tx_Power_Levels,
                                                  Load_UCode_From_ROM),
                    null                    => Ada.Real_Time.Clock_Time);
      --  Initialize the DecaDriver and DW1000.
      --
      --  If Load_Antenna_Delay is True then the antenna delay is read from the
      --  DW1000 OTP memory and is stored in this DecaDriver. The antenna delay
      --  is applied when the Configure procedure is called.
      --
      --  If Load_XTAL_Trim is True then the XTAL trim is read from the DW1000
      --  OTP memory and is stored in this DecaDriver. The XTAL trim is applied
      --  when the Configure procedure is called.
      --
      --  If Load_Tx_Power_Levels is True then the transmit power levels are
      --  read from the DW1000 OTP memory and is stored in this DecaDriver. The
      --  transmit power levels are applied when the Configure procedure is
      --  called, based on the specific configuration (channel & PRF).
      --
      --  If Load_UCode_From_ROM is True then the LDE microcode is loaded from
      --  the DW1000's ROM into the DW1000's RAM. This is necessary for the LDE
      --  algorithm to operate. If this is False then the LDE algorithm is
      --  disabled and is not run when packets are received.
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Initialize""",
         "Procedures in DW1000.BSP are not blocking");

      procedure Configure (Config : in Configuration_Type)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Config,
                                                Driver_Type),
                    Driver_Type             => + Config),
        Post => (PHR_Mode = Config.PHR_Mode);
      --  Configure the DW1000 for a specific channel, PRF, preamble, etc...
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Configure""",
         "Procedures in DW1000.BSP are not blocking");


      procedure Configure_Errors (Frame_Timeout : in Boolean;
                                  SFD_Timeout   : in Boolean;
                                  PHR_Error     : in Boolean;
                                  RS_Error      : in Boolean;
                                  FCS_Error     : in Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Frame_Timeout,
                                                SFD_Timeout,
                                                PHR_Error,
                                                RS_Error,
                                                FCS_Error),
                    Driver_Type             => (Driver_Type,
                                                Frame_Timeout,
                                                SFD_Timeout,
                                                PHR_Error,
                                                RS_Error,
                                                FCS_Error));
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""Configure_Errors""",
         "Procedures in DW1000.BSP are not blocking");

      function Get_Part_ID return Bits_32;
      function Get_Lot_ID  return Bits_32;

      function PHR_Mode return DW1000.Driver.Physical_Header_Modes
        With Depends => (PHR_Mode'Result => Driver_Type);

   private

      procedure DW1000_IRQ
        with Attach_Handler => DecaDriver_Config.DW1000_IRQ_Id,
        Global => (In_Out => (DW1000.BSP.Device_State,
                              Receiver,
                              Transmitter)),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Driver_Type,
                                                Receiver),
                    Driver_Type             => Driver_Type,
                    Transmitter             => (DW1000.BSP.Device_State,
                                                Transmitter,
                                                Driver_Type),
                    Receiver                => (DW1000.BSP.Device_State,
                                                Receiver,
                                                Driver_Type));
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation ""DW1000_IRQ""",
         "Procedures in DW1000.BSP are not blocking");
      --  DW1000 IRQ handler.
      --
      --  This performs functionality for packet reception and transmission.

      Part_ID : Bits_32 := 0;
      Lot_ID  : Bits_32 := 0;

      Antenna_Delay_PRF_64 : Bits_16 := 0;
      Antenna_Delay_PRF_16 : Bits_16 := 0;
      XTAL_Trim            : Bits_5  := 2#1_0000#;

      OTP_Tx_Power_Levels  : Tx_Power_Array := (others => 0);

      Long_Frames : Boolean := False;

      SYS_CFG_Reg : DW1000.Register_Types.SYS_CFG_Type :=
                      DW1000.Register_Types.SYS_CFG_Type'
                        (FFEN       => 0,
                         FFBC       => 0,
                         FFAB       => 0,
                         FFAD       => 0,
                         FFAA       => 0,
                         FFAM       => 0,
                         FFAR       => 0,
                         FFA4       => 0,
                         FFA5       => 0,
                         HIRQ_POL   => 0,
                         SPI_EDGE   => 0,
                         DIS_FCE    => 0,
                         DIS_DRXB   => 0,
                         DIS_PHE    => 0,
                         DIS_RSDE   => 0,
                         FCS_INT2F  => 0,
                         PHR_MODE   => 0,
                         DIS_STXP   => 0,
                         RXM110K    => 0,
                         RXWTOE     => 0,
                         RXAUTR     => 0,
                         AUTOACK    => 0,
                         AACKPEND   => 0,
                         Reserved_1 => 0,
                         Reserved_2 => 0);

      Use_OTP_XTAL_Trim     : Boolean := False;
      Use_OTP_Antenna_Delay : Boolean := False;

      Detect_Frame_Timeout  : Boolean := False;
      Detect_SFD_Timeout    : Boolean := False;
      Detect_PHR_Error      : Boolean := False;
      Detect_RS_Error       : Boolean := False;
      Detect_FCS_Error      : Boolean := False;

   end Driver_Type;

   Driver      : Driver_Type;

end DecaDriver;
