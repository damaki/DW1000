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

with Ada.Real_Time;                use Ada.Real_Time;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with DecaDriver;
with DW1000.BSP;
with DW1000.Driver;                use DW1000.Driver;
with DW1000.Types;
with Tx_Power;

--  This simple example demonstrates how to transmit packets.
procedure Transmit_Example
  with SPARK_Mode,
  Global => (Input  => Ada.Real_Time.Clock_Time,
             In_Out => (DW1000.BSP.Device_State,
                        DecaDriver.Driver,
                        DecaDriver.Tx_Complete_Flag)),
  Depends => (DecaDriver.Driver           =>+ DW1000.BSP.Device_State,
              DW1000.BSP.Device_State     =>+ DecaDriver.Driver,
              DecaDriver.Tx_Complete_Flag =>+ null,
              null                        => Ada.Real_Time.Clock_Time)
is
   Packet : constant DW1000.Types.Byte_Array (1 .. 10) := (others => 16#AA#);

   Now    : Ada.Real_Time.Time;

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

   --  Configure the transmit power for the PRF and channel chosen.
   --  We use the reference values for the EVB1000 in this example.
   DW1000.Driver.Configure_Tx_Power
     (Tx_Power.Manual_Tx_Power_Table (1, PRF_64MHz));

   --  Enable the LEDs controlled by the DW1000.
   DW1000.Driver.Configure_LEDs
     (Tx_LED_Enable    => True,  --  Enable transmit LED
      Rx_LED_Enable    => True,  --  Enable receive LED
      Rx_OK_LED_Enable => False,
      SFD_LED_Enable   => False,
      Test_Flash       => True); --  Flash both LEDs once

   Now := Ada.Real_Time.Clock;

   --  Send packets at a rate of 1 packet per second
   loop
      --  Load the packet into the transmitter's buffer.
      DW1000.Driver.Set_Tx_Data
        (Data   => Packet,
         Offset => 0);

      --  Tell the driver the length of the packet and its position in the
      --  transmit buffer.
      DW1000.Driver.Set_Tx_Frame_Length
        (Length => Packet'Length,
         Offset => 0);

      --  Start transmitting the packet now.
      --  (don't turn on the receiver after transmitting).
      DecaDriver.Driver.Start_Tx_Immediate (Rx_After_Tx     => False,
                                            Auto_Append_FCS => False);

      --  Wait for the packet to finish sending.
      Suspend_Until_True (DecaDriver.Tx_Complete_Flag);

      --  Wait for the time to send the next packet (1 second delay)
      Now := Now + Seconds (1);
      delay until Now;
   end loop;
end Transmit_Example;
