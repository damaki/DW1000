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
with DecaDriver_Config;
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

   SPI_Frequency_2_MHz : constant NRF52.UInt32 := 16#2000_0000#;
   SPI_Frequency_Max   : constant NRF52.UInt32 := 16#8000_0000#; --  8 MHz

   procedure Select_Device;
   --  Asserts the SPI chip select pin to the DW1000.

   procedure Deselect_Device;
   --  De-asserts the SPI chip select pin to the DW1000.

   ------------------------
   --  Protected Driver  --
   ------------------------

   --  A protected object is used to ensure that the 'Synchronous' specification
   --  of the Device_State is respected. This prevents race conditions with
   --  the SPI interface to the DW1000 by ensuring only one caller has access
   --  at any one time.
   --
   --  Note that the SPI interface also needs to be accessible from interrupt
   --  context e.g. the interrupt handler for the DW1000 IRQ. Therefore, we
   --  give this protected object an Interrupt_Priority.

   protected Driver
     with Interrupt_Priority => DecaDriver_Config.Driver_Priority
   is
      procedure PO_Reset_DW1000;

      procedure PO_Get_Reset_State (State : out DW1000.Types.Bits_1);

      procedure PO_Use_Slow_SPI_Clock;

      procedure PO_Use_Fast_SPI_Clock;

      procedure PO_Wakeup (Wait_For_INIT : in Boolean);

      procedure PO_Write_Transaction (Header : in DW1000.Types.Byte_Array;
                                      Data   : in DW1000.Types.Byte_Array)
        with Pre => (Header'Length in 1 .. 3 and Data'Length > 0);

      procedure PO_Read_Transaction (Header : in     DW1000.Types.Byte_Array;
                                     Data   :    out DW1000.Types.Byte_Array)
        with Pre => (Header'Length in 1 .. 3 and Data'Length > 0);

   private

      procedure SPI_Transfer_Byte (Tx_Byte : in     NRF52.Byte;
                                   Rx_Byte :    out NRF52.Byte)
        with Inline;

   end Driver;

   ---------------------
   --  Driver (body)  --
   ---------------------

   protected body Driver is

      -----------------------
      --  PO_Reset_DW1000  --
      -----------------------

      procedure PO_Reset_DW1000
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
      end PO_Reset_DW1000;

      --------------------------
      --  PO_Get_Reset_State  --
      --------------------------

      procedure PO_Get_Reset_State (State : out DW1000.Types.Bits_1)
      is
      begin
         if P0_Periph.IN_k.Arr (Reset_Pin) = Low then
            State := 0;
         else
            State := 1;
         end if;
      end PO_Get_Reset_State;

      -----------------------------
      --  PO_Use_Slow_SPI_Clock  --
      -----------------------------

      procedure PO_Use_Slow_SPI_Clock
      is
      begin
         --  Select fastest possible SPI speed that does not exceed 3 MHz.
         SPI1_Periph.ENABLE := (ENABLE => Disabled, others => <>);
         SPI1_Periph.FREQUENCY := SPI_Frequency_2_MHz;
         SPI1_Periph.ENABLE := (ENABLE => Enabled, others => <>);
      end PO_Use_Slow_SPI_Clock;

      -----------------------------
      --  PO_Use_Fast_SPI_Clock  --
      -----------------------------

      procedure PO_Use_Fast_SPI_Clock
      is
      begin
         SPI1_Periph.ENABLE := (ENABLE => Disabled, others => <>);
         SPI1_Periph.FREQUENCY := SPI_Frequency_Max;
         SPI1_Periph.ENABLE := (ENABLE => Enabled, others => <>);
      end PO_Use_Fast_SPI_Clock;

      -----------------
      --  PO_Wakeup  --
      -----------------

      procedure PO_Wakeup (Wait_For_INIT : in Boolean)
      is
         use type DW1000.Types.Bits_1;

         Now        : Ada.Real_Time.Time;
         WAKEUP_End : Ada.Real_Time.Time;
         RSTn_State : DW1000.Types.Bits_1;

      begin
         --  WAKEUP pin is not connected on the DWM1001, so use the alternate
         --  method of holding CS low to initiate wakeup.
         Select_Device;

         --  busy wait
         WAKEUP_End := Ada.Real_Time.Clock;
         WAKEUP_End := WAKEUP_End + Wakeup_Delay;
         loop
            Now := Ada.Real_Time.Clock;
            exit when Now >= WAKEUP_End;
         end loop;

         Deselect_Device;

         if Wait_For_INIT then
            WAKEUP_End := WAKEUP_End + Milliseconds (4);

            loop
               --  The DW1000 holds the RESET line low while it is waking up.
               Get_Reset_State (RSTn_State);
               exit when RSTn_State = 1;

               --  Exit anyway after 4 milliseconds. This avoids the possibility
               --  of an infinite loop.
               Now := Ada.Real_Time.Clock;
               exit when Now >= WAKEUP_End;

            end loop;
         end if;

      end PO_Wakeup;

      ----------------------------
      --  PO_Write_Transaction  --
      ----------------------------

      procedure PO_Write_Transaction (Header : in DW1000.Types.Byte_Array;
                                      Data   : in DW1000.Types.Byte_Array)
      is
         Dummy : Byte;

      begin
         Disable_DW1000_IRQ;

         Select_Device;

         SPI1_Periph.EVENTS_READY := (EVENTS_READY => 0, others => <>);

         --  Send the header
         for B of Header loop
            SPI_Transfer_Byte (Tx_Byte => Byte (B),
                               Rx_Byte => Dummy);
         end loop;

         --  Send the data
         for B of Data loop
            SPI_Transfer_Byte (Tx_Byte => Byte (B),
                               Rx_Byte => Dummy);
         end loop;

         Deselect_Device;

         Enable_DW1000_IRQ;

      end PO_Write_Transaction;

      ---------------------------
      --  PO_Read_Transaction  --
      ---------------------------

      procedure PO_Read_Transaction (Header : in     DW1000.Types.Byte_Array;
                                     Data   :    out DW1000.Types.Byte_Array)
      is
         Rx_Byte : Byte;

      begin
         Disable_DW1000_IRQ;

         Select_Device;

         SPI1_Periph.EVENTS_READY := (EVENTS_READY => 0, others => <>);

         --  Send the header
         for B of Header loop
            SPI_Transfer_Byte (Tx_Byte => Byte (B),
                               Rx_Byte => Rx_Byte);
         end loop;

         --  Receive the data
         for B of Data loop
            SPI_Transfer_Byte (Tx_Byte => Byte (B),
                               Rx_Byte => Rx_Byte);

            B := DW1000.Types.Bits_8 (Rx_Byte);
         end loop;

         Deselect_Device;

         Enable_DW1000_IRQ;

      end PO_Read_Transaction;

      -------------------------
      --  SPI_Transfer_Byte  --
      -------------------------

      procedure SPI_Transfer_Byte (Tx_Byte : in     NRF52.Byte;
                                   Rx_Byte :    out NRF52.Byte)
      is
      begin
         SPI1_Periph.TXD := (TXD => Tx_Byte, others => <>);

         loop
            exit when SPI1_Periph.EVENTS_READY.EVENTS_READY = 1;
         end loop;

         SPI1_Periph.EVENTS_READY := (EVENTS_READY => 0, others => <>);

         Rx_Byte := SPI1_Periph.RXD.RXD;
      end SPI_Transfer_Byte;

   end Driver;

   --------------------
   --  Reset_DW1000  --
   --------------------

   procedure Reset_DW1000
   is
   begin
      Driver.PO_Reset_DW1000;
   end Reset_DW1000;

   -----------------------
   --  Get_Reset_State  --
   -----------------------

   procedure Get_Reset_State (State : out DW1000.Types.Bits_1)
   is
   begin
      Driver.PO_Get_Reset_State (State);
   end Get_Reset_State;

   ------------------------------
   --  Acknowledge_DW1000_IRQ  --
   ------------------------------

   procedure Acknowledge_DW1000_IRQ
   is
   begin
      GPIOTE_Periph.EVENTS_IN (IRQ_Event) := (EVENTS_IN => 0, others => <>);
   end Acknowledge_DW1000_IRQ;

   --------------------------
   --  Disable_DW1000_IRQ  --
   --------------------------

   procedure Disable_DW1000_IRQ
   is
   begin
      GPIOTE_Periph.INTENCLR.IN_k.Arr (IRQ_Event) := Clear;
   end Disable_DW1000_IRQ;

   -------------------------
   --  Enable_DW1000_IRQ  --
   -------------------------

   procedure Enable_DW1000_IRQ
   is
   begin
      GPIOTE_Periph.INTENSET.IN_k.Arr (IRQ_Event) := Set;
   end Enable_DW1000_IRQ;

   --------------------------
   --  Use_Slow_SPI_Clock  --
   --------------------------

   procedure Use_Slow_SPI_Clock
   is
   begin
      Driver.PO_Use_Slow_SPI_Clock;
   end Use_Slow_SPI_Clock;

   --------------------------
   --  Use_Fast_SPI_Clock  --
   --------------------------

   procedure Use_Fast_SPI_Clock
   is
   begin
      Driver.PO_Use_Fast_SPI_Clock;
   end Use_Fast_SPI_Clock;

   --------------
   --  Wakeup  --
   --------------

   procedure Wakeup (Wait_For_INIT : in Boolean)
   is
   begin
      Driver.PO_Wakeup (Wait_For_INIT);
   end Wakeup;

   -------------------------
   --  Write_Transaction  --
   -------------------------

   procedure Write_Transaction (Header : in DW1000.Types.Byte_Array;
                                Data   : in DW1000.Types.Byte_Array)
   is
   begin
      Driver.PO_Write_Transaction (Header, Data);
   end Write_Transaction;

   ------------------------
   --  Read_Transaction  --
   ------------------------

   procedure Read_Transaction (Header : in     DW1000.Types.Byte_Array;
                               Data   :    out DW1000.Types.Byte_Array)
   is
   begin
      Driver.PO_Read_Transaction (Header, Data);
   end Read_Transaction;

   ---------------------
   --  Select_Device  --
   ---------------------

   procedure Select_Device
   is
   begin
      P0_Periph.OUTCLR := (As_Array => True,
                           Arr      => (SPI_CS_Pin => Clear,
                                        others     => <>));
   end Select_Device;

   -----------------------
   --  Deselect_Device  --
   -----------------------

   procedure Deselect_Device
   is
   begin
      P0_Periph.OUTSET := (As_Array => True,
                           Arr      => (SPI_CS_Pin => Set,
                                        others     => <>));
   end Deselect_Device;

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
   SPI1_Periph.CONFIG := (ORDER  => Msbfirst,
                          CPHA   => Leading,
                          CPOL   => Activehigh,
                          others => <>);
   SPI1_Periph.PSEL := (SCK  => SPI_CLK_Pin,
                        MOSI => SPI_MOSI_Pin,
                        MISO => SPI_MISO_Pin);
   SPI1_Periph.FREQUENCY := SPI_Frequency_2_MHz;
   SPI1_Periph.ENABLE := (ENABLE => Enabled, others => <>);

   --  Configure IRQ
   GPIOTE_Periph.CONFIG (IRQ_Event) := (MODE     => Event,
                                        PSEL     => DW_IRQ_Pin,
                                        POLARITY => Lotohi,
                                        OUTINIT  => Low,
                                        others   => <>);
   GPIOTE_Periph.INTENSET.IN_k.Arr (0) := Set; --  Enable interrupt

   --  Device might be sleeping, so assert the WAKEUP condition to wake it.
   Select_Device;
   declare
      use type DW1000.Types.Bits_1;

      Now        : Ada.Real_Time.Time;
      WAKEUP_End : Ada.Real_Time.Time;
      RSTn_State : DW1000.Types.Bits_1;
   begin
      Now        := Ada.Real_Time.Clock;
      WAKEUP_End := Now + Wakeup_Delay;

      delay until WAKEUP_End;

      Deselect_Device;

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
