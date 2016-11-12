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

with Ada.Real_Time;      use Ada.Real_Time;
with DecaDriver;
with DecaDriver.Core;
with DecaDriver.Rx;      use DecaDriver.Rx;
with DecaDriver.Tx;
with DW1000.BSP;
with DW1000.Driver;      use DW1000.Driver;
with DW1000.System_Time; use DW1000.System_Time;
with DW1000.Types;
with EVB1000_Tx_Power;

--  This example continuously waits for a packet to be received, and then
--  re-transmits the received packet after a 500 ms delay.
procedure Echo_Example
  with SPARK_Mode,
  Global => (Input  => Ada.Real_Time.Clock_Time,
             In_Out => (DW1000.BSP.Device_State,
                        DecaDriver.Core.Driver,
                        DecaDriver.Rx.Receiver,
                        DecaDriver.Tx.Transmitter)),
  Depends => (DecaDriver.Core.Driver    => + DW1000.BSP.Device_State,
              DecaDriver.Rx.Receiver    => + (DW1000.BSP.Device_State,
                                              DecaDriver.Core.Driver),
              DecaDriver.Tx.Transmitter => + (DW1000.BSP.Device_State,
                                              DecaDriver.Core.Driver,
                                              DecaDriver.Rx.Receiver),
              DW1000.BSP.Device_State   => + (DecaDriver.Core.Driver,
                                              DecaDriver.Rx.Receiver),
              null                      => Ada.Real_Time.Clock_Time)
is
   Rx_Packet        : DW1000.Types.Byte_Array (1 .. 127) := (others => 0);
   Rx_Packet_Length : DecaDriver.Frame_Length_Number;
   Rx_Frame_Info    : DecaDriver.Rx.Frame_Info_Type;
   Rx_Status        : DecaDriver.Rx.Rx_Status_Type;
   Rx_Overrun       : Boolean;

   Rx_Timestamp     : DW1000.System_Time.Fine_System_Time;
   Tx_Timestamp     : DW1000.System_Time.Fine_System_Time;

   Tx_Result        : DW1000.Driver.Result_Type;

begin
   --  Driver must be initialized once before it is used.
   DecaDriver.Core.Driver.Initialize
     (Load_Antenna_Delay  => True,
      Load_XTAL_Trim      => True,
      Load_UCode_From_ROM => True);

   --  Configure the DW1000
   DecaDriver.Core.Driver.Configure
     (DecaDriver.Core.Configuration_Type'
        (Channel             => 1,
         PRF                 => PRF_64MHz,
         Tx_Preamble_Length  => PLEN_1024,
         Rx_PAC              => PAC_8,
         Tx_Preamble_Code    => 9,
         Rx_Preamble_Code    => 9,
         Use_Nonstandard_SFD => False,
         Data_Rate           => Data_Rate_110k,
         PHR_Mode            => Standard_Frames,
         SFD_Timeout         => 1024 + 64 + 1));

   --  Configure the transmit power for the PRF and channel chosen.
   --  We use the reference values for the EVB1000 in this example.
   DecaDriver.Tx.Transmitter.Configure_Tx_Power
     (EVB1000_Tx_Power.Manual_Tx_Power_Table (1, PRF_64MHz));

   --  Enable the LEDs controlled by the DW1000.
   DecaDriver.Core.Driver.Configure_LEDs
     (Tx_LED_Enable    => True,  --  Enable transmit LED
      Rx_LED_Enable    => True,  --  Enable receive LED
      Rx_OK_LED_Enable => False,
      SFD_LED_Enable   => False,
      Test_Flash       => True); --  Flash both LEDs once

   --  In this example we only want to receive valid packets without errors,
   --  so configure the DW1000 to automatically re-enable the receiver when
   --  errors occur. The driver will not be notified of receiver errors whilst
   --  this is enabled.
   DecaDriver.Rx.Receiver.Set_Rx_Auto_Reenable (Enabled => True);

   DecaDriver.Rx.Receiver.Start_Rx_Immediate;

   --  Continuously receive packets, and echo them back after a 500 ms delay.
   loop
      DecaDriver.Rx.Receiver.Wait
        (Frame      => Rx_Packet,
         Length     => Rx_Packet_Length,
         Frame_Info => Rx_Frame_Info,
         Status     => Rx_Status,
         Overrun    => Rx_Overrun);

      if Rx_Status = No_Error then
         --  Get the timestamp at which the packet was received.
         Rx_Timestamp := DecaDriver.Rx.Receive_Timestamp (Rx_Frame_Info);

         --  We want to send the packet 0.5 seconds after it was received.
         Tx_Timestamp := System_Time_Offset (Rx_Timestamp, 0.5);

         --  Configure the transmitter to transmit the packet
         --  at the delayed time.
         DecaDriver.Tx.Transmitter.Set_Delayed_Tx_Time
           (Time => To_Coarse_System_Time (Tx_Timestamp));

         --  Load the packet into the transmitter
         DecaDriver.Tx.Transmitter.Set_Tx_Data
           (Data   => Rx_Packet (1 .. Rx_Packet_Length),
            Offset => 0);

         --  Tell the driver the length of the packet and its position in the
         --  transmit buffer.
         DecaDriver.Tx.Transmitter.Set_Tx_Frame_Length
           (Length => Rx_Packet_Length,
            Offset => 0);

         --  Transmit the delayed packet, and enable the receiver after the
         --  packet is sent.
         DecaDriver.Tx.Transmitter.Start_Tx_Delayed
           (Rx_After_Tx => True,
            Result      => Tx_Result);

         --  If the target transmit time has already passed (e.g. because we
         --  took too long to configure the transmitter, etc...) then don't
         --  transmit and just wait for another packet
         if Tx_Result /= Success then
            DecaDriver.Rx.Receiver.Start_Rx_Immediate;
         end if;
      end if;
   end loop;
end Echo_Example;
