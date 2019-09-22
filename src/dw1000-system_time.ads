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

with DW1000.Types; use DW1000.Types;

--  @summary
--  Types and functions for dealing with time derived from the DW1000 system
--  time (clocked at 63.8976 GHz).
--
--  @description
--  The DW1000's system time is used for several 40-bit timestamps which are
--  measured in units of approx. 15.65 picoseconds. The timestamps which use
--  this 40-bit 63.8976 GHz system time are listed below:
--    * System Time Counter (SYS_TIME register)
--    * Delayed Send or Receive Time (DX_TIME register)
--    * Adjusted Receive Timestamp (RX_STAMP field in the RX_TIME register)
--    * Raw Receive Timestamp (RX_RAWST field in the RX_TIME register)
--    * Adjusted Transmit Timestamp (TX_STAMP field in the TX_TIME register)
--    * Raw Transmit Timestamp (TX_RAWST field in the TX_TIME register)
--
--  Out of the above registers, not all are capable of measuring to the full
--  40-bit granularity of 15.65 picoseconds. In some registers the 9 least
--  significant bits are ignored which gives 512x less precision, i.e. the
--  effective granularity is 8.013 nanoseconds. The registers that are capable
--  of the FULL precision of 15.65 picoseconds are listed below:
--    * Adjusted Receive Timestamp (RX_STAMP field in the RX_TIME register)
--    * Adjusted Transmit Timestamp (TX_STAMP field in the TX_TIME register)
--
--  The following registers ignore the 9 least significant bits, providing a
--  granularity of 8.013 nanoseconds:
--    * System Time Counter (SYS_TIME register)
--    * Delayed Send or Receive Time (DX_TIME register)
--    * Raw Receive Timestamp (RX_RAWST field in the RX_TIME register)
--    * Raw Transmit Timestamp (TX_RAWST field in the TX_TIME register)
--
--  To manage the two time stamp precisions, the System_Time package defines
--  two types:
--    * Fine_System_Time, capable of the full 15.65 picosecond precision.
--    * Coarse_System_Time, capable of the restricted 8.013 nanosecond
--      precision.
--
--  For both of these types, conversion functions are defined to convert to and
--  from the 40-bit representation type that is used by the DW1000 register
--  set. The To_Bits_40 function converts from either Fine_System_Time or
--  Coarse_System_Time to the Bits_40 type. The To_Fine_System_Time and
--  To_Coarse_System_Time functions convert from the Bits_40 representation to
--  the Fine_System_Time and Coarse_System_Time types respectively.
--
--  Furthermore, the antenna delay is also measured in units of
--  15.65 picoseconds, but only has a 16-bit range (compared to the 40-bit
--  timestamps with a much bigger range). The antenna delay is represented
--  as the type Antenna_Delay_Time, which is a subtype of Fine_System_Time
--  with a maximum value of about 1.0256 microseconds.
--
--  For the antenna delay there are two conversion functions: To_Bits_16 and
--  To_Antenna_Delay_Time. The To_Bits_16 function converts from the
--  Antenna_Delay_Time type to its 16-bit representation. The
--  To_Antenna_Delay_Time function converts the other way; from the 16-bit
--  representation to the Antenna_Delay_Time type.
package DW1000.System_Time
with SPARK_Mode => On
is

   Chipping_Rate_Hz : constant := 499_200_000.0;
   --  DW1000 system clocks are referenced to the 499.2 MHz chipping rate
   --  defined in the IEEE 802.15.4-2011 standard.

   System_Time_Clock_Hz : constant := Chipping_Rate_Hz * 128.0;
   --  System time and time stamps are based on time units which are at
   --  63.8976 GHz (499.2 MHz * 128). This yields a resolution of
   --  approximately 15.65 picoseconds.

   type Fine_System_Time is
   delta 1.0 / System_Time_Clock_Hz
   range 0.0 .. (2.0**40 - 1.0) / System_Time_Clock_Hz
     with Small => 1.0 / System_Time_Clock_Hz,
     Size => 40;
   --  Type for representing the DW1000 fine-grained system time in seconds,
   --  with a precision of at least 15.65 picoseconds.
   --
   --  The DW1000 uses Bits_40 to represent timestamp values (e.g. system time,
   --  and tx/rx timestamps), which is measured in units of 499.2 MHz * 128,
   --  i.e. it increments in units of about 15.65 picoseconds (when using the
   --  PLL clock).
   --
   --  The Fine_System_Time is used for the transmit and receive time stamps.
   --
   --  WARNING: This type represents the system time when the PLL clock is
   --  used. On power up, before the digital PLL is enabled, the System Time
   --  increments in units of about 52.08 nanoseconds (3328x slower than the
   --  PLL), and so this Fine_System_Time type is not appropriate in such
   --  circumstances.

   type Coarse_System_Time is
   delta 512.0 / System_Time_Clock_Hz
   range 0.0 .. (2.0**40 - 1.0) / System_Time_Clock_Hz
     with Small => 1.0 / System_Time_Clock_Hz,
     Size => 40;
   --  Type for representing the DW1000 coarsely-grained system time in seconds,
   --  with a precision of at least 8.013 nanoseconds.
   --
   --  The DW1000 uses Bits_40 to represent the system time counter and the
   --  delayed tx/rx times, but ignores the low 9 order bits giving an
   --  effective precision of 8.013 nanoseconds (512x less precise than the
   --  fine-grained system time used for the transmit and receive timestamps).
   --
   --  The Coarse_System_Time is used for the System Time Counter, and the
   --  delayed tx/rx times.
   --
   --  WARNING: This type represents the system time when the PLL clock is
   --  used. On power up, before the digital PLL is enabled, the System Time
   --  increments in units of about 26.67 microseconds (3328x slower than the
   --  PLL), and so this Coarse_System_Time type is not appropriate in such
   --  circumstances.

   type System_Time_Span is new Fine_System_Time;

   type Antenna_Delay_Time is
   delta Fine_System_Time'Delta
   range 0.0 .. Fine_System_Time'Delta * (2**16 - 1)
     with Small => Fine_System_Time'Small,
     Size => 16;
   --  Type to represent an antenna delay time.

   type Frame_Wait_Timeout_Time is
   delta 512.0 / Chipping_Rate_Hz
   range 0.0 .. ((2.0**16 - 1.0) * 512.0) / Chipping_Rate_Hz
     with Small => 512.0 / Chipping_Rate_Hz,
     Size => 16;
   --  Type to represent the frame wait timeout.
   --
   --  The range of this type is 0.0 .. 0.067215385, i.e. the maximum value
   --  is 67.215385 milliseconds.
   --
   --  The resolution of this type is 128 system block cycle
   --  (about 1 microsecond).

   type Response_Wait_Timeout_Time is
   delta 512.0 / Chipping_Rate_Hz
   range 0.0 .. ((2.0**20 - 1.0) * 512.0) / Chipping_Rate_Hz
     with Small => 512.0 / Chipping_Rate_Hz,
     Size => 20;
   --  Type to represent the wait-for-response turnaround time.
   --
   --  The range of this type is 0.0 .. 1.0754615 i.e. the maximum value is
   --  about 1.0754615 seconds.
   --
   --  The resolution of this type is 128 system block cycle
   --  (about 1 microsecond).

   type Sniff_Off_Time is
   delta 512.0 / Chipping_Rate_Hz
   range 0.0 .. ((2.0**8 - 1.0) * 512.0) / Chipping_Rate_Hz
     with Small => 512.0 / Chipping_Rate_Hz,
     Size => 8;
   --  Type to represent the SNIFF mode OFF time.
   --
   --  The range of this type is 0.0 .. 0.000_262_051 i.e. the maximum value is
   --  about 0.000_262_051 seconds (about 262 microseconds).
   --
   --  The resolution of this type is 128 system block cycle
   --  (about 1 microsecond).

   function To_Bits_40 (Time : in Fine_System_Time) return Bits_40 is
     (Bits_40 (Time / Fine_System_Time (Fine_System_Time'Delta)));
   --  Convert a Fine_System_Time value to its equivalent Bits_40
   --  representation.
   --
   --  Note that is a surjective function, i.e. some values of System_Time
   --  map to the same Bits_40 value. This is due System_Time supporting a
   --  smaller granularity (approx. 0.9 picoseconds) than Bits_40
   --  (approx. 15.65 picoseconds).

   function To_Bits_40 (Time : in Coarse_System_Time) return Bits_40 is
     (Bits_40 (Time / Coarse_System_Time (Coarse_System_Time'Delta)) * 512)
     with Post => (To_Bits_40'Result and 2#1_1111_1111#) = 0;
   --  Convert a Coarse_System_Time value to its 40-bit integer representation.
   --
   --  Note that for registers using a coarse time the 9 least significant bits
   --  are 0.

   function To_Bits_40 (Span : in System_Time_Span) return Bits_40
   is (To_Bits_40 (Fine_System_Time (Span)));
   --  Convert a System_Time_Span value to its equivalent Bits_40
   --  representation.
   --
   --  System_Time_Span has the same resolution as Fine_System_Time, so the
   --  LSB in the Bits_40 representation is approx. 15.65 picoseconds.

   function To_Bits_16 (Time : in Antenna_Delay_Time) return Bits_16 is
     (Bits_16 (Time / Antenna_Delay_Time (Antenna_Delay_Time'Delta)));
   --  Convert a Antenna_Delay_Time value to its 16-bit representation.

   function To_Bits_16 (Time : in Frame_Wait_Timeout_Time) return Bits_16 is
     (Bits_16 (Time / Frame_Wait_Timeout_Time (Frame_Wait_Timeout_Time'Delta)));
   --  Convert a Frame_Wait_Timeout_Time to its 16-bit representation.

   function To_Fine_System_Time (Bits : in Bits_40) return Fine_System_Time
     with Inline,
     Global => null;
   --  Convert a Bits_40 value to Fine_System_Time.
   --
   --  The DW1000 register set uses a 40-bit integer to represent the system
   --  time and rx/tx timestamp values. This function converts the 40-bit
   --  integer representation to the System_Time fixed-point representation.

   function To_Coarse_System_Time (Bits : in Bits_40)
                                   return Coarse_System_Time
     with Inline,
     Global => null;
   --  Convert a 40-bit register value to Coarse_System_Time.

   function To_System_Time_Span (Bits : in Bits_40) return System_Time_Span
   is (System_Time_Span (To_Fine_System_Time (Bits)))
     with Inline,
     Global => null;
   --  Convert a 40-bit time span to the equivalent System_Time_Span value.


   function To_Antenna_Delay_Time (Bits : in Bits_16)
                                   return Antenna_Delay_Time
     with Inline,
     Global => null;
   --  Convert a 16-bit antenna delay register value to Antenna_Delay_Time.


   function System_Time_Offset (Time : in Fine_System_Time;
                                Span : in System_Time_Span)
                                return Fine_System_Time;
   --  Calculate the offset of a system time, with wrap-around.
   --
   --  Since the system time on the DW1000 is a counter which wraps-around,
   --  it is also permitted to have System_Time wrap-around when calculating
   --  a specific System_Time offset set in the future. This function handles
   --  this wrap-around case correctly.
   --
   --  This function is particularly useful when calculating a specific time
   --  at which the next packet should be set, relative to the previously
   --  received packet's timestamp.

   function System_Time_Offset (Time : in Coarse_System_Time;
                                Span : in System_Time_Span)
                                return Fine_System_Time
   is (System_Time_Offset (Fine_System_Time (Time), Span));


   function Calculate_Span (Start_Time : in Fine_System_Time;
                            End_Time   : in Fine_System_Time)
                            return System_Time_Span;
   --  Calculate the time span between two points in time.
   --
   --  Note that since the DW1000 system time wraps-around after about every
   --  17 seconds, so it is possible for the End_Time to be less than the
   --  Start_Time. This function takes this wrap-around behavior into account.

   function Calculate_Span (Start_Time : in Coarse_System_Time;
                            End_Time   : in Coarse_System_Time)
                            return System_Time_Span
   is (Calculate_Span (Fine_System_Time (Start_Time),
                       Fine_System_Time (End_Time)));
   --  Calculate the time span between two points in time.
   --
   --  Note that since the DW1000 system time wraps-around after about every
   --  17 seconds, so it is possible for the End_Time to be less than the
   --  Start_Time. This function takes this wrap-around behavior into account.


   function Calculate_Span (Start_Time : in Fine_System_Time;
                            End_Time   : in Coarse_System_Time)
                            return System_Time_Span
   is (Calculate_Span (Start_Time, Fine_System_Time (End_Time)));
   --  Calculate the time span between two points in time.
   --
   --  Note that since the DW1000 system time wraps-around after about every
   --  17 seconds, so it is possible for the End_Time to be less than the
   --  Start_Time. This function takes this wrap-around behavior into account.


   function Calculate_Span (Start_Time : in Coarse_System_Time;
                            End_Time   : in Fine_System_Time)
                            return System_Time_Span
   is (Calculate_Span (Fine_System_Time (Start_Time), End_Time));
   --  Calculate the time span between two points in time.
   --
   --  Note that since the DW1000 system time wraps-around after about every
   --  17 seconds, so it is possible for the End_Time to be less than the
   --  Start_Time. This function takes this wrap-around behavior into account.

private

   type FP_40 is delta 1.0 range 0.0 .. 2.0**40 - 1.0
     with Small => 1.0;
   --  Helper fixed-point type to represent a 40-bit integer type.

   function To_Fine_System_Time (Bits : in Bits_40) return Fine_System_Time is
     (Fine_System_Time (Fine_System_Time'Delta) * FP_40 (Bits));

   function To_Coarse_System_Time (Bits : in Bits_40)
                                   return Coarse_System_Time is
     (Coarse_System_Time (Coarse_System_Time'Delta) * FP_40 (Bits / 512));

   function To_Antenna_Delay_Time (Bits : in Bits_16)
                                   return Antenna_Delay_Time is
     (Antenna_Delay_Time'Delta * Integer (Bits));

end DW1000.System_Time;
