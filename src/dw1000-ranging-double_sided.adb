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

package body DW1000.Ranging.Double_Sided
with SPARK_Mode => On
is
   function Compute_Distance
     (Tag_Tx_Poll_Timestamp     : in Fine_System_Time;
      Anchor_Rx_Poll_Timestamp  : in Fine_System_Time;
      Anchor_Tx_Resp_Timestamp  : in Fine_System_Time;
      Tag_Rx_Resp_Timestamp     : in Fine_System_Time;
      Tag_Tx_Final_Timestamp    : in Fine_System_Time;
      Anchor_Rx_Final_Timestamp : in Fine_System_Time) return Biased_Distance
   is
      type System_Time_Span_Div2 is
      delta System_Time_Span'Delta / 2.0
      range 0.0 .. System_Time_Span'Last --  need the same range as System_Time_Span
        with Small => System_Time_Span'Small / 2.0;

      type System_Time_Span_Div4 is
      delta System_Time_Span'Delta / 4.0
      range 0.0 .. System_Time_Span'Last / 2.0
        with Small => System_Time_Span'Small / 4.0;

      T_Roundtrip1 : constant System_Time_Span := Calculate_Span
        (Start_Time => Tag_Tx_Poll_Timestamp,
         End_Time   => Tag_Rx_Resp_Timestamp);

      T_Reply1 : constant System_Time_Span := Calculate_Span
        (Start_Time => Anchor_Rx_Poll_Timestamp,
         End_Time   => Anchor_Tx_Resp_Timestamp);

      T_Roundtrip2 : constant System_Time_Span := Calculate_Span
        (Start_Time => Anchor_Tx_Resp_Timestamp,
         End_Time   => Anchor_Rx_Final_Timestamp);

      T_Reply2 : constant System_Time_Span := Calculate_Span
        (Start_Time => Tag_Rx_Resp_Timestamp,
         End_Time   => Tag_Tx_Final_Timestamp);

      Time_Of_Flight_1 : System_Time_Span_Div2;
      Time_Of_Flight_2 : System_Time_Span_Div2;
      Time_Of_Flight   : System_Time_Span_Div4;

      Diff : System_Time_Span;
      Sum  : System_Time_Span_Div2;

      TOF_I41   : Bits_41;
      TOF_Float : Long_Float;

   begin
      if (T_Reply1 > T_Roundtrip1) or (T_Reply2 > T_Roundtrip2) then
         Time_Of_Flight := 0.0;

      else
         Diff := T_Roundtrip1 - T_Reply1;
         Time_Of_Flight_1 := System_Time_Span_Div2 (Diff / System_Time_Span (2.0));

         Diff := T_Roundtrip2 - T_Reply2;
         Time_Of_Flight_2 := System_Time_Span_Div2 (Diff / System_Time_Span (2.0));

         Sum := Time_Of_Flight_1 + Time_Of_Flight_2;
         Time_Of_Flight := System_Time_Span_Div4 (Sum / System_Time_Span_Div4 (2.0));
      end if;

      --  Convert to floating point
      TOF_I41   := Bits_41 (Time_Of_Flight / System_Time_Span_Div4 (System_Time_Span_Div4'Delta));
      TOF_Float := Long_Float (TOF_I41) * System_Time_Span_Div4'Delta;

      return Biased_Distance (TOF_Float * Speed_Of_Light_In_Air);
   end Compute_Distance;


   function Compute_Distance
     (Tag_Tx_Poll_Timestamp     : in Fine_System_Time;
      Anchor_Rx_Poll_Timestamp  : in Fine_System_Time;
      Anchor_Tx_Resp_Timestamp  : in Fine_System_Time;
      Tag_Rx_Resp_Timestamp     : in Fine_System_Time;
      Tag_Tx_Final_Timestamp    : in Fine_System_Time;
      Anchor_Rx_Final_Timestamp : in Fine_System_Time;
      Channel                   : in DW1000.Driver.Channel_Number;
      PRF                       : in DW1000.Driver.PRF_Type) return Meters
   is
      Distance_With_Bias : Biased_Distance;

   begin
      Distance_With_Bias := Compute_Distance
        (Tag_Tx_Poll_Timestamp     => Tag_Tx_Poll_Timestamp,
         Anchor_Rx_Poll_Timestamp  => Anchor_Rx_Poll_Timestamp,
         Anchor_Tx_Resp_Timestamp  => Anchor_Tx_Resp_Timestamp,
         Tag_Rx_Resp_Timestamp     => Tag_Rx_Resp_Timestamp,
         Tag_Tx_Final_Timestamp    => Tag_Tx_Final_Timestamp,
         Anchor_Rx_Final_Timestamp => Anchor_Rx_Final_Timestamp);

      return Remove_Ranging_Bias
        (Measured_Distance => Distance_With_Bias,
         Channel           => Channel,
         PRF               => PRF);
   end Compute_Distance;

end DW1000.Ranging.Double_Sided;
