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

with Ada.Real_Time;   use Ada.Real_Time;
with DecaDriver;
with DW1000.BSP;
with DW1000.Driver;   use DW1000.Driver;
with DW1000.Types;

--  This simple example demonstrates using the DW1000 to receive packets.
procedure Receive_Example
  with SPARK_Mode,
  Global => (Input  => Ada.Real_Time.Clock_Time,
             In_Out => (DW1000.BSP.Device_State,
                        DecaDriver.Driver)),
  Depends => (DecaDriver.Driver         => + DW1000.BSP.Device_State,
              DW1000.BSP.Device_State   => + DecaDriver.Driver,
              null                      => Ada.Real_Time.Clock_Time)
is
   Rx_Packet        : DW1000.Types.Byte_Array (1 .. 127) := (others => 0);
   Rx_Packet_Length : DecaDriver.Frame_Length_Number;
   Rx_Frame_Info    : DecaDriver.Frame_Info_Type;
   Rx_Status        : DecaDriver.Rx_Status_Type;
   Rx_Overrun       : Boolean;

begin
   --  Driver must be initialized once before it is used.
   DecaDriver.Driver.Initialize
     (Load_Antenna_Delay  => True,
      Load_XTAL_Trim      => True,
      Load_UCode_From_ROM => True);

   --  Configure the DW1000
   DecaDriver.Driver.Configure
     (DecaDriver.Configuration_Type'
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

   --  We don't need to configure the transmit power in this example, because
   --  we don't transmit any frames!

   --  Enable the LEDs controlled by the DW1000.
   DW1000.Driver.Configure_LEDs
     (Tx_LED_Enable    => True,  --  Enable transmit LED
      Rx_LED_Enable    => True,  --  Enable receive LED
      Rx_OK_LED_Enable => False,
      SFD_LED_Enable   => False,
      Test_Flash       => True); --  Flash both LEDs once

   --  In this example we only want to receive valid packets without errors,
   --  so configure the DW1000 to automatically re-enable the receiver when
   --  errors occur. The driver will not be notified of receiver errors whilst
   --  this is enabled.
   DW1000.Driver.Set_Auto_Rx_Reenable (Enabled => True);

   --  Continuously receive packets
   loop
      --  Enable the receiver to listen for a packet
      DecaDriver.Driver.Start_Rx_Immediate;

      --  Wait for a packet
      DecaDriver.Driver.Rx_Wait
        (Frame      => Rx_Packet,
         Length     => Rx_Packet_Length,
         Frame_Info => Rx_Frame_Info,
         Status     => Rx_Status,
         Overrun    => Rx_Overrun);

      --  When execution has reached here then a packet has been received
      --  successfully.
   end loop;
end Receive_Example;
