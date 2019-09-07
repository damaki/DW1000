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

with DW1000.Driver;          use DW1000.Driver;
with DW1000.BSP;
with DW1000.Register_Types;  use DW1000.Register_Types;
with DW1000.System_Time;     use DW1000.System_Time;
with Dw1000.Types;           use DW1000.Types;

package DecaDriver.Rx
with SPARK_Mode => On
is

   type Rx_Status_Type is (No_Error,
                           Frame_Timeout,
                           Preamble_Timeout,
                           SFD_Timeout,
                           PHR_Error,
                           RS_Error,
                           FCS_Error);

   type Frame_Info_Type is record
      RX_TIME_Reg      : RX_TIME_Type;
      RX_FINFO_Reg     : RX_FINFO_Type;
      RX_FQUAL_Reg     : RX_FQUAL_Type;
      RXPACC_NOSAT_Reg : RXPACC_NOSAT_Type;
      RX_TTCKI_Reg     : RX_TTCKI_Type;
      RX_TTCKO_Reg     : RX_TTCKO_Type;
      SFD_LENGTH       : Bits_8;
      Non_Standard_SFD : Boolean;
   end record;
   --  Stores information about received frames.

   function Receive_Timestamp (Frame_Info : in Frame_Info_Type)
                               return Fine_System_Time;
   --  Get the corrected timestamp for the time of packet reception.
   --
   --  This timestamp marks the time at which the start of frame delimiter
   --  (SFD) part of the physical frame was received by the DW1000. It is
   --  used for the time-of-flight ranging algorithms.

   function Receive_Signal_Power (Frame_Info : in Frame_Info_Type)
                                  return Float
     with Post => Receive_Signal_Power'Result in -166.90 .. -14.43;
   --  Get the estimated receive signal power in dBm.

   function First_Path_Signal_Power (Frame_Info : in Frame_Info_Type)
                                     return Float
     with Post => First_Path_Signal_Power'Result in -218.07 .. -12.66;
   --  Get the estimated first path power in dBm.

   function Transmitter_Clock_Offset (Frame_Info : in Frame_Info_Type)
                                      return Long_Float
     with Post => Transmitter_Clock_Offset'Result in -1.0 .. 1.0;
   --  Calculate the clock offset between the receiver's and transmitter's
   --  clocks.
   --
   --  Since the transmitter and receiver radios are clocked by their own
   --  crystals, there can be a slight variation between the crystals'
   --  frequencies. This function provides a measure of the offset
   --  between this receiver and the remote transmitter clocks.
   --
   --  @param Frame_Info The frame information record for the received frame.
   --
   --  @return The computed clock offset. A positive value indicates that the
   --     transmitter's clock is running faster than the receiver's clock, and
   --     a negative value indicates that the transmitter's clock is running
   --     slower than the receiver's clock. For example, a value of 7.014E-06
   --     indicates that the transmitter is faster by 7 ppm. Likewise, a value
   --     of -5.045E-06 indicates that the transmitter's clock is slower by
   --     5 ppm.

   type Rx_Frame_Type is record
      Length     : Frame_Length_Number;
      Frame      : Byte_Array (1 .. Frame_Length_Number'Last);
      Frame_Info : Frame_Info_Type;
      Status     : Rx_Status_Type;
      Overrun    : Boolean;
   end record
     with Dynamic_Predicate =>
       (if Rx_Frame_Type.Status /= No_Error then Rx_Frame_Type.Length = 0);

   type Rx_Frame_Queue_Index is mod DecaDriver_Config.Receiver_Queue_Length;

   subtype Rx_Frame_Queue_Count is Natural range 0 .. DecaDriver_Config.Receiver_Queue_Length;

   type Rx_Frame_Queue_Type is
     array (Rx_Frame_Queue_Index)
     of Rx_Frame_Type;

   type Tx_Power_Config_Type (Smart_Tx_Power_Enabled : Boolean := True) is record
      case Smart_Tx_Power_Enabled is
         when True =>
            Boost_Normal : DW1000.Driver.Tx_Power_Config_Type;
            Boost_500us  : DW1000.Driver.Tx_Power_Config_Type;
            Boost_250us  : DW1000.Driver.Tx_Power_Config_Type;
            Boost_125us  : DW1000.Driver.Tx_Power_Config_Type;

         when False =>
            Boost_SHR    : DW1000.Driver.Tx_Power_Config_Type;
            Boost_PHR    : DW1000.Driver.Tx_Power_Config_Type;
      end case;
   end record;

   ----------------------------------------------------------------------------
   -- Receiver
   ----------------------------------------------------------------------------

   protected Receiver
     with Interrupt_Priority => DecaDriver_Config.Driver_Priority
   is
      entry Wait (Frame      : in out Byte_Array;
                  Length     :    out Frame_Length_Number;
                  Frame_Info :    out Frame_Info_Type;
                  Status     :    out Rx_Status_Type;
                  Overrun    :    out Boolean)
      with Depends => (Frame         => + Receiver,
                       Frame_Info    => Receiver,
                       Length        => Receiver,
                       Receiver => Receiver,
                       Status        => Receiver,
                       Overrun       => Receiver),
        Pre => Frame'Length > 0,
        Post => (if Status /= No_Error then Length = 0);
      --  Waits for a frame to be received, or an error. When a frame is
      --  received (or if one has been previously received and is waiting to be
      --  read) then the frame's content and size are copied to the Frame and
      --  Length arguments.
      --
      --  If any of the enabled errors occurs (e.g. an FCS error is detected)
      --  then the Status argument is set to specify the type of receive error
      --  that occurred, Length is set to 0, and the contents of the Frame
      --  array are unmodified.
      --
      --  If no error occurs (Status is set to No_Error) then the frame
      --  contents are copied to the Frame array, and Length is set to the
      --  length of the frame (in bytes). If the Frame array is too small to
      --  store the received frame then the frame's contents are truncated, but
      --  the Length argument still reflects the frame's true size.
      --
      --  If the Frame array is larger than the received frame then the extra
      --  bytes in the Frame array are unmodified.
      --
      --  When a valid frame is successfully received information about the
      --  received frame is copied to the Frame_Info output parameter. This
      --  information can be used to calculate various values about the
      --  received frame, such as the estimated receive signal power and the
      --  clock offset between the transmitter and receiver.
      --
      --  @param Frame If a frame has been successfully received
      --     (Status = No_Error) then the contents of the frame are copied to
      --     this array. If this array is too small to store the entire
      --     frame then the frame is truncated.
      --
      --  @param Length The length of the received frame in bytes. If the frame
      --     was received successfully then this value is a natural number
      --     (a frame length of 0 is possible). Otherwise, if an error occurred
      --     then this value is set to 0 always.
      --
      --  @param Frame_Info When a frame is successfully received information
      --     about the received frame is stored in this output parameter, and
      --     can be used to calculate various information about the frame, such
      --     as the estimated receive signal power, and the clock offset
      --     between the transmitter and receiver.
      --
      --  @param Status Indicates whether or not a frame was successfully
      --     received. If a frame was successfully received then this parameter
      --     is set to No_Error. Otherwise, if an error occurred then Status
      --     indicates the cause of the error (e.g. an invalid FCS).
      --
      --  @param Overrun Indicates whether or not an overrun condition has
      --     occurred. This output parameter is set to True when one or more
      --     frames have been dropped before the reception of this frame, due
      --     to insufficient buffer space.

      function Pending_Frames_Count return Natural
        with Post => (Pending_Frames_Count'Result <=
                        DecaDriver_Config.Receiver_Queue_Length);
      --  Returns the number of received frames that are waiting to be read.

      procedure Discard_Pending_Frames
        with Depends => (Receiver => Receiver);
      --  Discard any pending frames that have been received, but have not yet
      --  been read.

      procedure Set_FCS_Check_Enabled (Enabled : in Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Enabled,
                    Receiver           => Receiver);
      --  Enable or disable the automatic frame check sequence (FCS) on
      --  received frames.
      --
      --  By default, the DW1000 automatically checks the 16-bit CRC FCS on
      --  each received frame. The last two octets in the received frame are
      --  assumed as the 16-bit CRC, and is compared against the actual FCS
      --  computed against all but the last two octets in the received frame.
      --
      --  If the DW1000 detects that the actual FCS does not match the FCS in
      --  the received frame, then it generates an FCS error. If
      --  double-buffered mode is enabled then the received frame is discarded
      --  and the buffer re-used for the next received frame.
      --
      --  This procedure enables or disables the FCS check.
      --
      --  @param Enabled When True (default after DW1000 reset) the DW1000 will
      --     check the FCS of each received frame. Set this to false to disable
      --     the FCS check for each packet.

      procedure Set_Frame_Filtering_Enabled (Enabled : in Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Enabled,
                    Receiver           => Receiver);
      --  Enable or disable frame filtering.
      --
      --  Frame filtering allows the DW1000 to automatically reject frames
      --  according to certain criterea according to the IEEE 802.15.4-2011
      --  MAC layer.
      --
      --  To configure which frames are accepted or rejected by the DW1000 see
      --  the Configure_Frame_Filtering procedure.
      --
      --  @param Enabled When set to True frame filtering is enabled.
      --     Otherwise, when False, it is disabled.

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
                                                Allow_Frame_Type_5),
                    Receiver           => Receiver);
      --  Configure which MAC frame types are automatically filtered by the
      --  DW1000.
      --
      --  Note that the frame filtering configuration only takes effect when
      --  frame filtering is enabled.
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

      procedure Set_Rx_Double_Buffer (Enabled : in Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Enabled,
                    Receiver           => Receiver);
      --  Enable or disable the double-buffer mode of the receiver.

      procedure Set_Rx_Auto_Reenable (Enabled : in Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Enabled,
                    Receiver           => Receiver);
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

      procedure Set_Delayed_Rx_Time(Time : in Coarse_System_Time)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Time,
                    Receiver           => Receiver);
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


      procedure Set_Frame_Wait_Timeout (Timeout : in Frame_Wait_Timeout_Time)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Timeout,
                    Receiver           => Receiver);


      procedure Start_Rx_Immediate
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                    Receiver           => Receiver);
      --  Turn on the receiver immediately (without delay).


      procedure Start_Rx_Delayed (Result  : out Result_Type)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                    Result                  => DW1000.BSP.Device_State,
                    Receiver           => Receiver);
      --  Turn on the receiver at the configured delay time.
      --
      --  The time at which the receiver should be enabled is programmed
      --  using the Set_Delayed_Rx_Time procedure, which must be called before
      --  calling this procedure.


      procedure Notify_Frame_Received
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Receiver,
                    Receiver           => + DW1000.BSP.Device_State);


      --  Reads a received frame from the DW1000.
      --
      --  WARNING: This is intended to only be called by the DecaDriver IRQ
      --  when the DW1000 signals that a frame has been received. This
      --  procedure should not be called by the user.

      procedure Notify_Receive_Error (Error : in Rx_Status_Type)
        with Depends => (Receiver => + Error),
        Pre => Error /= No_Error;

   private
      Frame_Queue : Rx_Frame_Queue_Type
        := (others => (Length     => 0,
                       Frame      => (others => 0),
                       Frame_Info => Frame_Info_Type'
                         (RX_TIME_Reg      => (RX_STAMP => 0,
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
                          SFD_LENGTH       => 0,
                          Non_Standard_SFD => False),
                       Status      => No_Error,
                       Overrun    => False));
      --  Cyclic buffer for storing received frames, read from the DW1000.

      Queue_Head       : Rx_Frame_Queue_Index := Rx_Frame_Queue_Index'Last;
      Rx_Count         : Rx_Frame_Queue_Count := 0;
      Overrun_Occurred : Boolean              := False;
      Frame_Ready      : Boolean              := False;
   end Receiver;

end DecaDriver.Rx;
