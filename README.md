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

_Note: Due to a bug in GNATprove from SPARK GPL 2016 it is currently not possible to run GNATprove in proof mode on the_ ``DecaDriver`` _package, or packages which have visibility to the_ ``DecaDriver`` _package. However, it is still possible to run perform flow analysis with GNATprove on all files, and the majority of the driver is able to be analyzed in proof mode with GNATprove._

# 2. License

All files are licensed under the MIT license.

# 3. Using the driver

To use the driver, include the contents of the ``src`` directory into your
project.

It is also necessary to provide an implementation of the package ``DW1000.BSP``
(the file ``dw1000-bsp.adb``) for your target hardware to provide the SPI
interface to the DW1000. The package spec is already defined in the file
``src/dw1000-bsp.ads``, and an example implementation of ``dw1000-bsp.adb`` can
be found in ``bsp-examples/evb1000/dw1000-bsp.adb``.

## 3.1 See Also

If you are using the [DecaWave EVB1000](http://www.decawave.com/products/evk1000-evaluation-kit) 
evaluation board then there are the following two SPARK and Ada projects on 
GitHub which are useful:
  * [EVB1000](https://github.com/damaki/EVB1000): SPARK drivers for the LCD, LED,
    and switch peripherals on the EVB1000. 
  * [Ravenscar-sfp-stm32f105](https://github.com/damaki/ravenscar-sfp-stm32f105):
    a Ravenscar runtime for the STM32F105 ARM Cortex-M3 microcontroller on the
    EVB1000.

# 4. Examples

Here's some examples of SPARK code using the driver:

## 4.1 Transmitting frames

```Ada
with Ada.Real_Time; use Ada.Real_Time;
with DecaDriver;
with DW1000.BSP;
with DW1000.Driver; use DW1000.Driver;
with DW1000.Types;  use DW1000.Types;

procedure Example
  with SPARK_Mode => On,
  Global => (Input  =>  Ada.Real_Time.Clock_Time,
             In_Out => (DW1000.BSP.Device_State,
                        DecaDriver.Driver,
                        DecaDriver.Transmitter)),
  Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                          DecaDriver.Driver),
              DecaDriver.Transmitter  => DecaDriver.Transmitter,
              DecaDriver.Driver       => (DecaDriver.Driver,
                                          DW1000.BSP.Device_State),
              null                    => Ada.Real_Time.Clock_Time)
is
   Frame_Data : constant Byte_Array(1 .. 127) := (others => 16#AA#);

begin
   DecaDriver.Driver.Initialize (Load_Antenna_Delay   => True,
                                 Load_XTAL_Trim       => True,
                                 Load_Tx_Power_Levels => True,
                                 Load_UCode_From_ROM  => True);

   DecaDriver.Driver.Configure (DecaDriver.Configuration_Type'
                                  (Channel             => 1,
                                   PRF                 => PRF_64MHz,
                                   Tx_Preamble_Length  => PLEN_1024,
                                   Tx_PAC              => PAC_32,
                                   Tx_Preamble_Code    => 9,
                                   Rx_Preamble_Code    => 9,
                                   Use_Nonstandard_SFD => False,
                                   Data_Rate           => Data_Rate_110k,
                                   PHR_Mode            => Standard_Frames,
                                   SFD_Timeout         => 1025 + 64 - 32,
                                   Enable_Smart_Power  => False));

   --  Continuously send packets
   loop
      DecaDriver.Transmitter.Set_Tx_Data (Data   => Frame_Data,
                                          Offset => 0);
      DecaDriver.Transmitter.Set_Tx_Frame_Length (Length => Frame_Data'Length,
                                                  Offset => 0);
      DecaDriver.Transmitter.Start_Tx_Immediate (Rx_After_Tx => False);
      
      DecaDriver.Transmitter.Wait_For_Tx_Complete;
   end loop;
end Example;
```

## 4.2 Receiving frames

```Ada
with Ada.Real_Time;      use Ada.Real_Time;
with DecaDriver;
with DW1000.BSP;
with DW1000.Driver;      use DW1000.Driver;
with DW1000.System_Time;
with DW1000.Types;       use DW1000.Types;

procedure Example
  with SPARK_Mode => On,
  Global => (Input  =>  Ada.Real_Time.Clock_Time,
             In_Out => (DW1000.BSP.Device_State,
                        DecaDriver.Driver,
                        DecaDriver.Receiver)),
  Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                          DecaDriver.Driver),
              DecaDriver.Receiver     => DecaDriver.Receiver,
              DecaDriver.Driver       => (DecaDriver.Driver,
                                          DW1000.BSP.Device_State),
              null                    => Ada.Real_Time.Clock_Time)
is
   Frame_Data   : Byte_Array(1 .. 127) := (others => 0);
   Frame_Size   : DecaDriver.Frame_Length_Number;
   Rx_Timestamp : DW1000.System_Time.Fine_System_Time;
   Rx_Error     : DecaDriver.Rx_Errors;
   Rx_Overrun   : Boolean;

begin
   DecaDriver.Driver.Initialize (Load_Antenna_Delay   => True,
                                 Load_XTAL_Trim       => True,
                                 Load_Tx_Power_Levels => True,
                                 Load_UCode_From_ROM  => True);

   DecaDriver.Driver.Configure (DecaDriver.Configuration_Type'
                                  (Channel             => 1,
                                   PRF                 => PRF_64MHz,
                                   Tx_Preamble_Length  => PLEN_1024,
                                   Tx_PAC              => PAC_32,
                                   Tx_Preamble_Code    => 9,
                                   Rx_Preamble_Code    => 9,
                                   Use_Nonstandard_SFD => False,
                                   Data_Rate           => Data_Rate_110k,
                                   PHR_Mode            => Standard_Frames,
                                   SFD_Timeout         => 1025 + 64 - 32,
                                   Enable_Smart_Power  => False));

   --  Continuously receive packets
   loop
      DecaDriver.Receiver.Start_Rx_Immediate;
      
      DecaDriver.Receiver.Wait (Frame     => Frame_Data,
                                Size      => Frame_Size,
                                Timestamp => Rx_Timestamp,
                                Error     => Rx_Error,
                                Overrun   => Rx_Overrun);
   end loop;
end Example;
```
