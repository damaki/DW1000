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

with Ada.Interrupts.Names;
with System;

--  @summary
--  Configuration options for the DecaDriver.
package DecaDriver_Config
is

   DW1000_IRQ_Id : constant Ada.Interrupts.Interrupt_ID
     := Ada.Interrupts.Names.GPIOTE_Interrupt;
   --  The interrupt ID for the DW1000 IRQ line.

   Driver_Priority : constant System.Interrupt_Priority
     := System.Interrupt_Priority'Last;
   --  The interrupt priority used for the DecaDriver's protected objects.

   Receiver_Queue_Length : constant Positive := 2;
   --  The length of the receiver queue used to hold received packets
   --  on the host MCU side.
   --
   --  The minimum value for this configuration parameter is 2.
   --
   --  Larger values for this configuration parameter make it less likely for
   --  a packet to be missed (overrun condition). However, larger values will
   --  use more memory.

   Maximum_Receive_Frame_Length : constant Positive := 1024;
   --  Controls the buffer size used for storing received frames in the
   --  receiver driver queue.
   --
   --  The minimum permitted value is 1.
   --  The maximum permitted value is 1024.
   --
   --  The DW1000 supports extended frame lengths of up to 1024 bytes. However,
   --  the IEEE 802.15.4-2011 standard restricts the physical frame size to
   --  127 bytes (see the constant aMaxPHYPacketSize in Table 70 in
   --  Section 9.2 of the IEEE 802.15.4-2011 standard).
   --
   --  If you are using the extended capabilities of the DW1000 then this
   --  constant should be set to 1024.
   --
   --  Otherwise, if you are only using frames up to the maximum size set in
   --  the standard (127 bytes) then you can set this constant to 127 to
   --  reduce the amount of memory used by the receiver driver's queue.
   --
   --  If a frame is received by the driver whose length is smaller than this
   --  constant then the received frame is truncated to this length. For
   --  example, if Maximum_Receive_Frame_Size is set to 50, and a frame is
   --  received with a length of 62 bytes, then the received frame is truncated
   --  to the first 50 bytes (the last 12 bytes are ignored by the driver).

end DecaDriver_Config;
