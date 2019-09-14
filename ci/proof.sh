#!/bin/sh

# Dependencies
apt update
apt upgrade -y
apt install -y --no-install-recommends python

# Install the ravenscar_full_nrf52832 BSP from bb-runtimes
git clone https://github.com/AdaCore/bb-runtimes
cd bb-runtimes
python ./build_rts.py --output=temp --bsps-only nrf52832
gprbuild -P temp/BSPs/ravenscar_full_nrf52832.gpr -j0 -f
gprinstall -P temp/BSPs/ravenscar_full_nrf52832.gpr -p -f
cd ..

# Prove all examples
gnatprove -P examples/transmit/transmit_example.gpr --mode=all --level=2 -j0 -- checks-as-errors
gnatprove -P examples/receive/receive_example.gpr --mode=all --level=2 -j0 -- checks-as-errors
gnatprove -P examples/echo/echo_example.gpr --mode=all --level=2 -j0 -- checks-as-errors