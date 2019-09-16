[![Build Status](https://travis-ci.com/damaki/DW1000.svg?branch=master)](https://travis-ci.com/damaki/DW1000)

# 1. DW1000

This repository contains an Ada/SPARK driver for the
[DecaWave DW1000](http://www.decawave.com/products/dw1000)
ultra-wideband (UWB) radio chip.

The driver is based on the C code provided by DecaWave, but most parts are
quite different in order to take advantage of Ada's stronger type safety, and
to provide an easier-to-use API.

This DW1000 driver written and verified using the SPARK language. GNATprove is
used to prove the absence of runtime errors such as: numerical overflow,
infinite loops, division by zero, and out-of-bounds array accesses.

_Note: Due to a bug in GNATprove from SPARK GPL 2016 it is currently not possible to run GNATprove in proof mode on the_ ``DecaDriver`` _package, or packages which have visibility to the_ ``DecaDriver`` _package. However, it is still possible to perform flow analysis with GNATprove on all files, and the majority of the driver is able to be analyzed in proof mode with GNATprove. This bug has been fixed in the development version of SPARK Pro._

# 2. License

All files are licensed under the MIT license.

# 3. Using the driver

To use the driver, include the contents of the ``src`` directory into your
project, and provide an implementation of ``dw1000-bsp.adb`` for your target
hardware (more on this below in 3.2).

Check out the [Wiki](https://github.com/damaki/DW1000/wiki) for more information
on using the driver.

## 3.1 Prerequisites

Parts of the driver require certain runtime features that are not available on all
bare-board Ada runtimes. In particular, the following features are used:
  * Protected objects are used by the ``DecaDriver``. This requires
    an Ada runtime supporting tasking (e.g. a Ravenscar runtime).
  * The ``Ada.Real_Time.Clock`` function is used by the ``DW1000.Driver``
    package. This feature is also supported in Ravenscar runtimes.
  * The package ``Ada.Numerics.Generic_Elementary_Functions`` is used by the
    package ``DW1000.Reception_Quality``. Small footprint (SFP) Ravenscar
    runtimes don't support these math functions, but they are implemented in
    the full Ravenscar runtimes.

Typically, this means that a full Ravenscar runtime is required as a minimum,
such as: [ravenscar-full-evb1000](https://github.com/damaki/ravenscar-full-evb1000).

## 3.2 Porting the BSP

It is also necessary to provide an implementation of the package ``DW1000.BSP``
(the file ``dw1000-bsp.adb``) for your target hardware to provide the SPI
interface to the DW1000. The package spec is already defined in the file
``src/dw1000-bsp.ads``, and an example implementation of ``dw1000-bsp.adb`` can
be found in ``bsp-examples/evb1000/dw1000-bsp.adb``.

During elaboration, the package body should initialize the peripherals used
to communicate with the DW1000, i.e. the SPI and external interrupt (EXTI)
peripherals.

The package body must implement the following procedures:
  * The procedure ``Reset_DW1000`` resets the DW1000 by asserting low the nRST
    line to the DW1000, then deasserting high the nRST line.
  * The procedure ``Acknowledge_DW1000_IRQ`` is called by the DecaDriver's
    interrupt service routine (ISR) to acknowledge the EXTI generated by the
    DW1000. For an STM32 MCU, this procedure would acknowledge the interrupt
    in the EXTI peripheral.
  * The procedure ``Write_Transaction`` sends data to the DW1000 via the SPI.
    It performs the following sequence of actions:
    1. Assert low the DW1000's chip select line on the SPI.
    2. Write the bytes in the ``Header`` byte array to the DW1000 via the SPI.
    3. Write the bytes in the ``Data`` byte array to the DW1000 via the SPI.
    4. Deassert high the DW1000 chip select line on the SPI.
  * The procedure ``Read_Transaction`` reads data from the DW1000 via the SPI.
    It performs the following sequence of actions:
    1. Assert low the DW1000's chip select line on the SPI.
    2. Write the bytes in the ``Header`` byte array to the DW1000 via the SPI.
    3. Read ``Data'Length`` bytes from the DW1000 via the SPI, and store them
       in the ``Data`` byte array.
    4. Deassert high the DW1000 chip select line on the SPI.

## 3.3 See Also

If you are using the [DecaWave EVB1000](http://www.decawave.com/products/evk1000-evaluation-kit)
evaluation board then there are the following two SPARK and Ada projects on
GitHub which are useful:
  * [EVB1000](https://github.com/damaki/EVB1000): SPARK drivers for the LCD, LED,
    and switch peripherals on the EVB1000.
  * [Ravenscar-full-evb1000](https://github.com/damaki/ravenscar-full-evb1000):
    a full Ravenscar runtime for the STM32F105 ARM Cortex-M3 microcontroller,
    configured for the EVB1000.

# 4. Examples

Here's some examples of SPARK code using the driver.

The [examples](https://github.com/damaki/DW1000/tree/master/examples)
directory contains some buildable and runnable examples which are also
analyzable with GNATprove.

## 4.1 Transmitting frames

```Ada
with Ada.Real_Time;    use Ada.Real_Time;
with DecaDriver;
with DecaDriver.Core;
with DecaDriver.Rx;
with DecaDriver.Tx;
with DW1000.BSP;
with DW1000.Driver;    use DW1000.Driver;
with DW1000.Types;     use DW1000.Types;
with EVB1000_Tx_Power; use EVB1000_Tx_Power;

procedure Example
  with SPARK_Mode => On,
  Global => (Input  =>  Ada.Real_Time.Clock_Time,
             In_Out => (DW1000.BSP.Device_State,
                        DecaDriver.Core.Driver,
                        DecaDriver.Tx.Transmitter)),
  Depends => (DW1000.BSP.Device_State   => (DW1000.BSP.Device_State,
                                            DecaDriver.Core.Driver),
              DecaDriver.Tx.Transmitter => DecaDriver.Tx.Transmitter,
              DecaDriver.Core.Driver    => (DecaDriver.Core.Driver,
                                            DW1000.BSP.Device_State),
              null                      => Ada.Real_Time.Clock_Time)
is
   Frame_Data : constant Byte_Array(1 .. 127) := (others => 16#AA#);

   Config     : constant DecaDriver.Core.Configuration_Type
     := (Channel             => 1,
         PRF                 => PRF_64MHz,
         Tx_Preamble_Length  => PLEN_1024,
         Rx_PAC              => PAC_32,
         Tx_Preamble_Code    => 9,
         Rx_Preamble_Code    => 9,
         Use_Nonstandard_SFD => False,
         Data_Rate           => Data_Rate_110k,
         PHR_Mode            => Standard_Frames,
         SFD_Timeout         => 1025 + 64 - 32);

begin
   DecaDriver.Core.Driver.Initialize
     (Load_Antenna_Delay   => True,
      Load_XTAL_Trim       => True,
      Load_UCode_From_ROM  => True);

   DecaDriver.Core.Driver.Configure (Config);

   --  The reference transmit power values for the DecaWave EVB1000 evalulation
   --  board are used to configure the transmit power level.
   DecaDriver.Tx.Transmitter.Configure_Tx_Power
     (Smart_Tx_Power_Table (Positive (Config.Channel), Config.PRF));

   --  Continuously send packets
   loop
      DecaDriver.Tx.Transmitter.Set_Tx_Data
        (Data   => Frame_Data,
         Offset => 0);

      DecaDriver.Tx.Transmitter.Set_Tx_Frame_Length
        (Length => Frame_Data'Length,
         Offset => 0);

      DecaDriver.Tx.Transmitter.Start_Tx_Immediate (Rx_After_Tx => False);

      DecaDriver.Tx.Transmitter.Wait_For_Tx_Complete;
   end loop;
end Example;
```

## 4.2 Receiving frames

```Ada
with Ada.Real_Time;      use Ada.Real_Time;
with DecaDriver;
with DecaDriver.Core;
with DecaDriver.Rx;
with DecaDriver.Tx;
with DW1000.BSP;
with DW1000.Driver;      use DW1000.Driver;
with DW1000.System_Time;
with DW1000.Types;       use DW1000.Types;

procedure Example
  with SPARK_Mode => On,
  Global => (Input  =>  Ada.Real_Time.Clock_Time,
             In_Out => (DW1000.BSP.Device_State,
                        DecaDriver.Core.Driver,
                        DecaDriver.Rx.Receiver)),
  Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                          DecaDriver.Driver),
              DecaDriver.Rx.Receiver  => DecaDriver.Rx.Receiver,
              DecaDriver.Core.Driver  => (DecaDriver.Core.Driver,
                                          DW1000.BSP.Device_State),
              null                    => Ada.Real_Time.Clock_Time)
is
   Frame_Data    : Byte_Array(1 .. 127) := (others => 0);
   Frame_Length  : DecaDriver.Frame_Length_Number;
   Rx_Frame_Info : DecaDriver.Rx.Frame_Info_Type;
   Rx_Error      : DecaDriver.Rx.Rx_Errors;
   Rx_Overrun    : Boolean;

begin
   DecaDriver.Core.Driver.Initialize
     (Load_Antenna_Delay   => True,
      Load_XTAL_Trim       => True,
      Load_UCode_From_ROM  => True);

   DecaDriver.Core.Driver.Configure
     (DecaDriver.Core.Configuration_Type'
       (Channel             => 1,
        PRF                 => PRF_64MHz,
        Tx_Preamble_Length  => PLEN_1024,
        Rx_PAC              => PAC_32,
        Tx_Preamble_Code    => 9,
        Rx_Preamble_Code    => 9,
        Use_Nonstandard_SFD => False,
        Data_Rate           => Data_Rate_110k,
        PHR_Mode            => Standard_Frames,
        SFD_Timeout         => 1025 + 64 - 32,
        Enable_Smart_Power  => False));

   --  Continuously receive packets
   loop
      DecaDriver.Rx.Receiver.Start_Rx_Immediate;

      DecaDriver.Rx.Receiver.Wait
        (Frame      => Frame_Data,
         Length     => Frame_Length,
         Frame_Info => Rx_Frame_Info,
         Error      => Rx_Error,
         Overrun    => Rx_Overrun);
   end loop;
end Example;
```
