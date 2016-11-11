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
with DecaDriver.Core;
with DecaDriver.Tx;
with DW1000.BSP;
with DW1000.Driver;   use DW1000.Driver;
with DW1000.Types;
with EVB1000_Tx_Power;

--  This simple example demonstrates how to transmit packets.
procedure Transmit_Example
  with SPARK_Mode,
  Global => (Input  => Ada.Real_Time.Clock_Time,
             In_Out => (DW1000.BSP.Device_State,
                        DecaDriver.Core.Driver,
                        DecaDriver.Tx.Transmitter)),
  Depends => (DecaDriver.Core.Driver    => + DW1000.BSP.Device_State,
              DecaDriver.Tx.Transmitter => + null,
              DW1000.BSP.Device_State   => + DecaDriver.Core.Driver,
              null                      => Ada.Real_Time.Clock_Time)
is
   Packet : constant DW1000.Types.Byte_Array (1 .. 10) := (others => 16#AA#);

   Now    : Ada.Real_Time.Time;

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
         SFD_Timeout         => 1024 + 8));

   --  Configure the transmit power for the PRF and channel chosen.
   --  We use the reference values for the EVB1000 in this example.
   DecaDriver.Tx.Transmitter.Configure_Tx_Power
     (EVB1000_Tx_Power.Manual_Tx_Power_Table (1, PRF_64MHz));

   Now := Ada.Real_Time.Clock;

   --  Send packets at a rate of 1 packet per second
   loop
      --  Load the packet into the transmitter's buffer.
      DecaDriver.Tx.Transmitter.Set_Tx_Data
        (Data   => Packet,
         Offset => 0);

      --  Tell the driver the length of the packet and its position in the
      --  transmit buffer.
      DecaDriver.Tx.Transmitter.Set_Tx_Frame_Length
        (Length => Packet'Length,
         Offset => 0);

      --  Start transmitting the packet now.
      --  (don't turn on the receiver after transmitting).
      DecaDriver.Tx.Transmitter.Start_Tx_Immediate (Rx_After_Tx => False);

      --  Wait for the packet to finish sending.
      DecaDriver.Tx.Transmitter.Wait_For_Tx_Complete;

      --  Wait for the time to send the next packet (1 second delay)
      Now := Now + Seconds (1);
      delay until Now;
   end loop;
end Transmit_Example;
