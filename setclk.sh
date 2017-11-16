#!/bin/sh

# Usage: setclk.sh <freq-in-MHz>

# Set the clock frequency for fclk0
# Not all frequencies are supported due to how the PLLs work, the actual
# set frequency will be displayed.

CLK_BASE=/sys/devices/soc0/amba/f8007000.devcfg
#CLK_BASE=/sys/devices/amba.1/f8007000.devcfg
CLK_NAME="fclk0"
#CLK_NAME="FPGA0"
FCLK0_BASE=$CLK_BASE/fclk/$CLK_NAME


if [ ! -f $FCLK0_BASE ]; then
  echo $CLK_NAME > $CLK_BASE/fclk_export
fi

PREV_FREQ=$(cat $FCLK0_BASE/set_rate)
echo "Prev frequency was $PREV_FREQ"

echo "$1""000000" > $FCLK0_BASE/round_rate
ROUND_RES=$(cat $FCLK0_BASE/round_rate)
set $ROUND_RES

echo "Setting frequency to $3"

echo $3 > $FCLK0_BASE/set_rate
