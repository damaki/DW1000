-------------------------------------------------------------------------------
-- Copyright (c) 2019 Daniel King
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
-- IN THE SOFTWARE.
-------------------------------------------------------------------------------

with Ada.Real_Time;           use Ada.Real_Time;
with NRF52;                   use NRF52;
with NRF52.GPIO;              use NRF52.GPIO;
with NRF52.SPI;               use NRF52.SPI;
with NRF52.GPIOTE;            use NRF52.GPIOTE;

with System.Storage_Elements; use System.Storage_Elements;

--  BSP implementation for the DWM1001 module.
--
--  This BSP is designed for a Ravenscar runtime, and makes use of a protected
--  object to satisfy the Synchronous requirement of the abstract Device_State.
--  The use of a protected object protects against races between tasks
--  and the DW1000 IRQ, which both may attempt to access the SPI bus.
--
--  To adapt this runtime for a ZFP profile (where protected objects are
--  prohibited) another mechanim must be used. For example, interrupts may
--  be disabled during SPI transactions.
package body DW1000.BSP
with SPARK_Mode => Off
is

   --  PO pin assignments.
   Reset_Pin    : constant := 24;
   SPI_MOSI_Pin : constant := 20;
   DW_IRQ_Pin   : constant := 19;
   SPI_MISO_Pin : constant := 18;
   SPI_CS_Pin   : constant := 17;
   SPI_CLK_Pin  : constant := 16;

   IRQ_Event    : constant := 0;

   Wakeup_Delay : constant Time_Span := Microseconds (510);

   procedure Select_Device
   is
   begin
      P0_Periph.OUTCLR := (As_Array => True,
                           Arr      => (SPI_CS_Pin => Clear,
                                        others     => <>));
   end Select_Device;

   procedure Deselect_Device
   is
   begin
      P0_Periph.OUTSET := (As_Array => True,
                           Arr      => (SPI_CS_Pin => Set,
                                        others     => <>));
   end Deselect_Device;

   procedure Reset_DW1000
   is
      End_Time : Ada.Real_Time.Time;
   begin

      --  Configure RSTn as output low
      P0_Periph.PIN_CNF (Reset_Pin) := (DIR    => Output,
                                        INPUT  => Disconnect,
                                        PULL   => Disabled,
                                        DRIVE  => S0S1,
                                        SENSE  => Disabled,
                                        others => <>);
      P0_Periph.OUTCLR := (As_Array => True,
                           Arr      => (Reset_Pin => Clear,
                                        others    => <>));

      --  Busy wait for 2 ms
      End_Time := Ada.Real_Time.Clock + Milliseconds (2);
      loop
         exit when Ada.Real_Time.Clock >= End_Time;
      end loop;

      --  Put the RSTn line back to hi-Z
      P0_Periph.PIN_CNF (Reset_Pin) := (DIR    => Input,
                                        INPUT  => Connect,
                                        PULL   => Disabled,
                                        DRIVE  => S0S1,
                                        SENSE  => Disabled,
                                        others => <>);
   end Reset_DW1000;

   procedure Get_Reset_State (State : out DW1000.Types.Bits_1)
   is
   begin
      if P0_Periph.IN_k.Arr (Reset_Pin) = Low then
         State := 0;
      else
         State := 1;
      end if;
   end Get_Reset_State;

   procedure Acknowledge_DW1000_IRQ
   is
   begin
      GPIOTE_Periph.EVENTS_IN (IRQ_Event) := (EVENTS_IN => 0, others => <>);
   end Acknowledge_DW1000_IRQ;

   procedure Disable_DW1000_IRQ
   is
   begin
      GPIOTE_Periph.INTENCLR.IN_k.Arr (IRQ_Event) := Clear;
   end Disable_DW1000_IRQ;


   procedure Enable_DW1000_IRQ
   is
   begin
      GPIOTE_Periph.INTENSET.IN_k.Arr (IRQ_Event) := Set;
   end Enable_DW1000_IRQ;


   procedure Use_Slow_SPI_Clock
   is
   begin
      SPI1_Periph.FREQUENCY := 16#2000_0000#; --  2 Mbps
   end Use_Slow_SPI_Clock;


   procedure Use_Fast_SPI_Clock
   is
   begin
      SPI1_Periph.FREQUENCY := 16#8000_0000#; --  8 Mbps (max speed)
   end Use_Fast_SPI_Clock;


   procedure Assert_WAKEUP
   is
   begin
      --  WAKEUP pin is not connected on the DWM1001, so use the alternate
      --  method of holding CS low to initiate wakeup.
      Select_Device;
   end Assert_WAKEUP;


   procedure Deassert_WAKEUP
   is
   begin
      Deselect_Device;
   end Deassert_WAKEUP;


   procedure Write_Transaction(Header : in DW1000.Types.Byte_Array;
                               Data   : in DW1000.Types.Byte_Array)
   is
      Dummy : Byte;

   begin
      Disable_DW1000_IRQ;

      Select_Device;

      SPI1_Periph.EVENTS_READY := (EVENTS_READY => 0, others => <>);

      --  Send the header
      for B of Header loop
         SPI1_Periph.TXD := (TXD => Byte (B), others => <>);

         loop
            exit when SPI1_Periph.EVENTS_READY.EVENTS_READY = 1;
         end loop;

         SPI1_Periph.EVENTS_READY := (EVENTS_READY => 0, others => <>);

         --  Read of RXD is needed after every byte transmission
         Dummy := SPI1_Periph.RXD.RXD;
      end loop;

      --  Send the data
      for B of Data loop
         SPI1_Periph.TXD := (TXD => Byte (B), others => <>);

         loop
            exit when SPI1_Periph.EVENTS_READY.EVENTS_READY = 1;
         end loop;

         SPI1_Periph.EVENTS_READY := (EVENTS_READY => 0, others => <>);

         --  Read of RXD is needed after every byte transmission
         Dummy := SPI1_Periph.RXD.RXD;
      end loop;

      Deselect_Device;

      Enable_DW1000_IRQ;

   end Write_Transaction;

   procedure Read_Transaction(Header : in     DW1000.Types.Byte_Array;
                              Data   :    out DW1000.Types.Byte_Array)
   is
      Dummy : Byte;

   begin
      Disable_DW1000_IRQ;

      Select_Device;

      SPI1_Periph.EVENTS_READY := (EVENTS_READY => 0, others => <>);

      --  Send the header
      for B of Header loop
         SPI1_Periph.TXD := (TXD => Byte (B), others => <>);

         loop
            exit when SPI1_Periph.EVENTS_READY.EVENTS_READY = 1;
         end loop;

         SPI1_Periph.EVENTS_READY := (EVENTS_READY => 0, others => <>);

         --  Read of RXD is needed after every byte transmission
         Dummy := SPI1_Periph.RXD.RXD;
      end loop;

      --  Receive the data
      for B of Data loop
         SPI1_Periph.TXD := (TXD => 16#FF#, others => <>);

         loop
            exit when SPI1_Periph.EVENTS_READY.EVENTS_READY = 1;
         end loop;

         SPI1_Periph.EVENTS_READY := (EVENTS_READY => 0, others => <>);

         B := DW1000.Types.Bits_8 (SPI1_Periph.RXD.RXD);
      end loop;

      Deselect_Device;

      Enable_DW1000_IRQ;

   end Read_Transaction;

begin
   --  Configure GPIOs

   Deselect_Device;

   --  Set reset pin to hi-z by default. See DW1000 Datasheet clause 5.6.3.1
   P0_Periph.PIN_CNF (Reset_Pin) := (DIR    => Input,
                                     INPUT  => Connect,
                                     PULL   => Disabled,
                                     DRIVE  => S0S1,
                                     SENSE  => Disabled,
                                     others => <>);
   P0_Periph.PIN_CNF (DW_IRQ_Pin) := (DIR    => Input,
                                      INPUT  => Connect,
                                      PULL   => Disabled,
                                      DRIVE  => S0S1,
                                      SENSE  => Disabled,
                                      others => <>);
   P0_Periph.PIN_CNF (SPI_MISO_Pin) := (DIR    => Input,
                                        INPUT  => Connect,
                                        PULL   => Pullup,
                                        DRIVE  => S0S1,
                                        SENSE  => Disabled,
                                        others => <>);
   P0_Periph.PIN_CNF (SPI_MOSI_Pin) := (DIR    => Output,
                                        INPUT  => Disconnect,
                                        PULL   => Disabled,
                                        DRIVE  => S0S1,
                                        SENSE  => Disabled,
                                        others => <>);
   P0_Periph.PIN_CNF (SPI_CLK_Pin) := (DIR    => Output,
                                       INPUT  => Connect,
                                       PULL   => Disabled,
                                       DRIVE  => S0S1,
                                       SENSE  => Disabled,
                                       others => <>);
   P0_Periph.PIN_CNF (SPI_CS_Pin) := (DIR    => Output,
                                      INPUT  => Disconnect,
                                      PULL   => Disabled,
                                      DRIVE  => S0S1,
                                      SENSE  => Disabled,
                                      others => <>);

   --  Configure SPI
   SPI1_Periph.CONFIG := (ORDER  => MsbFirst,
                          CPHA   => Leading,
                          CPOL   => Activehigh,
                          others => <>);
   SPI1_Periph.PSEL := (SCK  => SPI_CLK_Pin,
                        MOSI => SPI_MOSI_Pin,
                        MISO => SPI_MISO_Pin);
   SPI1_Periph.FREQUENCY := 16#2000_0000#; --  2 Mbps
   SPI1_Periph.ENABLE := (ENABLE => Enabled, others => <>);

   --  Configure IRQ
   GPIOTE_Periph.CONFIG (IRQ_Event) := (MODE     => Event,
                                        PSEL     => DW_IRQ_Pin,
                                        POLARITY => Lotohi,
                                        OUTINIT  => Low,
                                        others   => <>);
   GPIOTE_Periph.INTENSET.IN_k.Arr (0) := Set; --  Enable interrupt

   --  Device might be sleeping, so assert the WAKEUP pin to wake it.
   --  WAKEUP pin must be asserted for at least 500 microseconds.
   Assert_WAKEUP;
   declare
      use type DW1000.Types.Bits_1;

      Now        : Ada.Real_Time.Time;
      WAKEUP_End : Ada.Real_Time.Time;
      RSTn_State : DW1000.Types.Bits_1;
   begin
      Now        := Ada.Real_Time.Clock;
      WAKEUP_End := Now + Wakeup_Delay;

      delay until WAKEUP_End;

      Deassert_WAKEUP;

      --  Reset the device. This only has an effect if the device wasn't asleep.
      --  Since if the device was asleep then it is now in the WAKEUP state for
      --  approx. 4 ms, and during this state it keeps the RSTn line low anyway.
      Reset_DW1000;

      --  Delay for 4 ms to allow the DW1000 to transition into the INIT state.
      --  Otherwise, user code may not be able to communicate with the DW1000
      --  if elaboration finishes within 4 ms and the user immediately tries to
      --  use the DW1000, since it will still be in the WAKEUP state.
      Now        := Ada.Real_Time.Clock;
      WAKEUP_End := Now + Milliseconds (4);

      loop
         --  The DW1000 de-asserts the RSTn line when it exits the WAKEUP state
         --  which lets us exit early.
         Get_Reset_State (RSTn_State);
         exit when RSTn_State = 1;

         --  Otherwise, exit anyway after 4 ms.
         Now := Ada.Real_Time.Clock;
         exit when Now >= WAKEUP_End;
      end loop;
   end;

end DW1000.BSP;
