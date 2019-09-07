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

with DW1000.Constants;
with DW1000.Driver;
with DW1000.BSP;
with DW1000.System_Time; use DW1000.System_Time;
with Dw1000.Types;       use DW1000.Types;

package DecaDriver.Tx
with SPARK_Mode => On
is

   protected Transmitter
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

      procedure Configure_Tx_Power
        (Config : DW1000.Driver.Tx_Power_Config_Type)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Config),
                    Transmitter        => Transmitter);

      procedure Set_Tx_Data (Data   : in Byte_Array;
                             Offset : in Natural)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Data,
                                                Offset),
                    Transmitter        => Transmitter),
        Pre => (Data'Length in 1 .. 1024 and then
                Offset < 1024            and then
                Data'Length + Offset <= 1024);

      procedure Set_Tx_Frame_Length (Length : in Natural;
                                     Offset : in Natural)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Length,
                                                Offset),
                    Transmitter        => Transmitter),
        Pre => (Length < DW1000.Constants.TX_BUFFER_Length and then
                Offset < DW1000.Constants.TX_BUFFER_Length and then
                Length + Offset <= DW1000.Constants.TX_BUFFER_Length);

      procedure Set_Delayed_Tx_Time(Time : in Coarse_System_Time)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => + Time,
                    Transmitter        => Transmitter);
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


      procedure Start_Tx_Immediate (Rx_After_Tx     : in Boolean;
                                    Auto_Append_FCS : in Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Rx_After_Tx,
                                                Auto_Append_FCS),
                    Transmitter        => Transmitter);
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
      --
      --  If Auto_Append_FCS is set to True then the DW1000 will automatically
      --  calculate and append the 2-byte frame check sequence (FCS) to the
      --  transmitted frame.


      procedure Start_Tx_Delayed
        (Rx_After_Tx : in     Boolean;
         Result      :    out DW1000.Driver.Result_Type)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Rx_After_Tx),
                    Result                  => (DW1000.BSP.Device_State,
                                                Rx_After_Tx),
                    Transmitter        => (Transmitter,
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

      procedure Read_Tx_Adjusted_Timestamp (Timestamp : out Fine_System_Time)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                    Timestamp               => DW1000.BSP.Device_State,
                    Transmitter        => Transmitter);


      procedure Read_Tx_Raw_Timestamp (Timestamp : out Coarse_System_Time)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                    Timestamp               => DW1000.BSP.Device_State,
                    Transmitter        => Transmitter);

      procedure Notify_Tx_Complete;
      --  Notify the driver that the transmit is complete.
      --
      --  WARNING: This procedure is intended to be called only by the DW1000
      --  IRQ. This procedure should not be called by the user.

   private
      Tx_Idle : Boolean := True;
      --  True when the transmitter is idle, False otherwise.

   end Transmitter;

end DecaDriver.Tx;
