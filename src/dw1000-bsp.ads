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

with DW1000.Types;

--  This package defines low-level procedures for interfacing with the DW1000
--  at the physical layer.
--
--  The body for this package is not included in this project as it is very
--  specific to the target hardware. Therefore, the body for this package
--  must be implemented by the user for their target board.
package DW1000.BSP
with SPARK_Mode => On,
  Abstract_State => (Device_State with Synchronous, External),
  Initializes    => Device_State
is

   procedure Reset_DW1000
     with Global => (Output => Device_State),
     Depends => (Device_State => null);
   --  Resets the DW1000 via the RSTn line.

   procedure Get_Reset_State (State : out DW1000.Types.Bits_1)
     with Global => (Input => Device_State),
     Depends => (State => Device_State);
   --  Read the current state of the RSTn line.
   --
   --  The State is 0 when the RSTn line is asserted (low), and 1 when the
   --  RSTn line is de-asserted (high).
   --
   --  Reading the state of the RSTn is useful when waking up from the SLEEP
   --  or DEEPSLEEP states, as the DW1000 asserts the RSTn line whilst it is in
   --  the WAKEUP state. RSTn is de-asserted once it has entered the INIT
   --  state.

   procedure Acknowledge_DW1000_IRQ
     with Global => (In_Out => Device_State);
   --  Acknowledge the IRQ request from the DW1000.
   --
   --  This should be called from the interrupt handler of the DW1000 IRQ line
   --  to acknowledge the interrupt.
   --
   --- Failure to call this procedure from the interrupt handler may result in
   --  an infinite interrupt loop.

   procedure Disable_DW1000_IRQ
     with Global => (In_Out => Device_State);
   --  Disables the DW1000 IRQ to prevent the DW1000 interrupt from being
   --  triggered.
   --
   --  Any IRQs requested after calling this function should be held pending
   --  until Enable_DW1000_IRQ is called.

   procedure Enable_DW1000_IRQ
     with Global => (In_Out => Device_State);
   --  Enables the DW1000 IRQ.


   procedure Use_Slow_SPI_Clock
     with Global => (In_Out => Device_State);
   --  Switch the BSP to use a slow SPI clock speed (no faster than 3 MHz).
   --
   --  The slow SPI clock speed should be used when the DW1000 is in the INIT
   --  state.


   procedure Use_Fast_SPI_Clock
     with Global => (In_Out => Device_State);
   --  Switch the BSP to use a faster SPI clock speed (no faster than 20 MHz).
   --
   --  The fast SPI clock speed can be used when the DW1000 has left the INIT
   --  state.


   procedure Wakeup (Wait_For_INIT : in Boolean)
     with Global => (In_Out => Device_State);
   --  Perform the Wakeup sequence.
   --
   --  This asserts the WAKEUP condition for a minimum of 500 microseconds.
   --
   --  If Wait_For_INIT is True, then this function will then also wait for up
   --  to an additional 4 milliseconds for the DW1000 to enter the INIT state.
   --  This guarantees that the SPI interface is ready after the return of this
   --  procedure.
   --
   --  This is a non-blocking function (the Ada 'delay' statement is not used).
   --  Instead, a busy wait is be used for the delays.


   procedure Write_Transaction(Header : in DW1000.Types.Byte_Array;
                               Data   : in DW1000.Types.Byte_Array)
     with Global => (In_Out => Device_State),
     Depends => (Device_State => + (Header, Data)),
     Pre => (Header'Length in 1 .. 3
             and Data'Length > 0);
   --  Perform a "write" transaction to the DW1000.
   --
   --  This procedure executes a write transaction by performing the following
   --  steps:z
   --     1. Select the DW1000 on the SPI bus.
   --     2. Send the transaction header bytes (1 .. 3 bytes) via the SPI
   --        interface.
   --     3. Send the transaction data (variable length) to the DW1000 via the
   --        SPI interface.
   --     4. Deselect the DW1000 on the SPI bus.
   --
   --  Note: This procedure must not block. I.e. the procedure must not use
   --  the 'delay until' statement, nor call any protected entries.

   procedure Read_Transaction(Header : in     DW1000.Types.Byte_Array;
                              Data   :    out DW1000.Types.Byte_Array)
     with Global => (In_Out => Device_State),
     Depends => (Device_State => + (Header, Data),
                 Data         => (Header, Device_State)),
     Pre => (Header'Length in 1 .. 3
             and Data'Length > 0);
   --  Perform a "read" transaction from the DW1000.
   --
   --  This procedure executes a write transaction by performing the following
   --  steps:
   --     1. Select the DW1000 on the SPI bus.
   --     2. Send the transaction header bytes (1 .. 3 bytes) via the SPI
   --        interface.
   --     3. Read the transaction data (variable length) from the DW1000 via
   --        the SPI interface, and write the received bytes to the 'Data' byte
   --        array.
   --     4. Deselect the DW1000 on the SPI bus.
   --
   --  Note: This procedure must not block. I.e. the procedure must not use
   --  the 'delay until' statement, nor call any protected entries.

end DW1000.BSP;
