[![Build Status](https://travis-ci.com/damaki/DW1000.svg?branch=master)](https://travis-ci.com/damaki/DW1000)

# DW1000

This repository contains an Ada/SPARK driver for the
[DecaWave DW1000](http://www.decawave.com/products/dw1000)
ultra-wideband (UWB) radio chip.

The driver code written and verified using the SPARK language (a subset of Ada).
GNATprove is used to prove the absence of runtime errors such as: numerical overflow,
infinite loops, division by zero, and out-of-bounds array accesses.

The API is currently considered unstable. You may need to update your code to
to reflect API changes as the driver is developed.

## License

All files are licensed under the MIT license.

## Using the driver

To use the driver, include the contents of the ``src`` directory into your
project, as well as the appropriate BSP directory under `bsp-examples`
(or provide your own implementation for your own hardware).

Check out the [Wiki](https://github.com/damaki/DW1000/wiki) for more information
on using the driver.

### Prerequisites

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

Typically, this means that a full Ravenscar runtime is required as a minimum.
AdaCore's [bb-runtimes](https://github.com/AdaCore/bb-runtimes) repository has
runtimes available for a variety of bare-bones embedded targets. The exact
runtime needed depends on your target board.

If you don't use all of the features of this driver, then you can use a reduced
runtime. For example, if you don't use the `DecaDriver` then you don't need a
Ravenscar runtime (unless other parts of your code use Ravenscar features).

### Supported Boards

This driver currently supports the [DWM1001](https://www.decawave.com/product/dwm1001-module/)
module. This module is used in the [DWM1001-DEV](https://www.decawave.com/product/dwm1001-development-board/)
development board and the [MDEK1001](https://www.decawave.com/product/mdek1001-deployment-kit/)
development kit.

Previously, the [EVB1000](https://www.decawave.com/product/evk1000-evaluation-kit/)
was supported and an old port to this board is still present in the `bsp-examples`
directory. This port is currently obsolete/broken due to lack of EVB1000 hardware
available to develop with.

### Porting the BSP

Porting the driver to a new target consists of re-implementing the `dw1000-bsp.adb`
file, which implements the `DW1000.BSP` package. This package implements everything
necessary to actually communicate with the DW1000 including: sending and receiving
data via SPI, managing the DW1000 interrupt request (IRQ), resetting the
DW1000, and waking it up from its sleep mode.

The package spec is already defined in the package spec in the file
`src/dw1000-bsp.ads`. This stays the same for all targets; only the package body
needs to be re-implemented.

An reference implementation for the DWM1001 target can be found in
``bsp-examples/dwm1001/dw1000-bsp.adb``. This is a useful starting point when
porting to a new target.

## See Also

If you are using the [DecaWave EVB1000](http://www.decawave.com/products/evk1000-evaluation-kit)
evaluation board then there are the following two SPARK and Ada projects on
GitHub which are useful:
  * [EVB1000](https://github.com/damaki/EVB1000): SPARK drivers for the LCD, LED,
    and switch peripherals on the EVB1000.
  * [Ravenscar-full-evb1000](https://github.com/damaki/ravenscar-full-evb1000):
    a full Ravenscar runtime for the STM32F105 ARM Cortex-M3 microcontroller,
    configured for the EVB1000.

## Examples

See the [examples](https://github.com/damaki/DW1000/tree/master/examples)
directory for various example programs using the driver.