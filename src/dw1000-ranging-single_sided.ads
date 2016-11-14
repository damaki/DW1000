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

package DW1000.Ranging.Single_Sided
with SPARK_Mode => On
is

   function Compute_Distance
     (Tag_Tx_Poll_Timestamp    : in Fine_System_Time;
      Anchor_Rx_Poll_Timestamp : in Fine_System_Time;
      Anchor_Tx_Resp_Timestamp : in Fine_System_Time;
      Tag_Rx_Resp_Timestamp    : in Fine_System_Time) return Biased_Distance
     with Global => null;
   --  Compute the distance based on a single-sided ranging exchange.
   --
   --  The distance measurement calculated by this function contains a bias
   --  which can be removed to obtain a more accurate distance measurement.
   --  The @Remove_Ranging_Bias@ function can be used to remove the bias.
   --
   --  @param Tag_Tx_Poll_Timestamp The timestamp of the Tag's local clock when
   --     the Tag sent the poll message to the anchor.
   --
   --  @param Anchor_Rx_Poll_Timestamp The timestamp of the Anchor's local
   --     clock when it received the poll message from the Tag.
   --
   --  @param Anchor_Tx_Poll_Timestamp The timstamp of the Anchor's local clock
   --     when it sent the response message back to the Tag.
   --
   --  @param Tag_Rx_Resp_Timestamp The timestamp of the Tag's local clock when
   --     it received the response from the Anchor.
   --
   --  @return The measured distance between the tag and anchor. This
   --     measurement contains a bias which must be removed to obtain a more
   --     accurate measurement.

end DW1000.Ranging.Single_Sided;
