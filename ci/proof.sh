#!/bin/sh

# Dependencies
apt update
apt upgrade -y
apt install -y --no-install-recommends python

# Install the ravenscar_full_nrf52832 BSP from bb-runtimes
git clone https://github.com/AdaCore/bb-runtimes.git
cd bb-runtimes
python ./build_rts.py --output=temp --bsps-only nrf52832
gprbuild -P temp/BSPs/ravenscar_full_nrf52832.gpr -j0 -f
gprinstall -P temp/BSPs/ravenscar_full_nrf52832.gpr -p -f
cd ..

# Prove all .gpr files in the examples directory
find examples -regex ".*\.gpr$" -exec gnatprove -P {} --mode=all --level=3 -j0 --checks-as-errors -XBSP=DWM1001 \;