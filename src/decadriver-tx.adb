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

with DW1000.Driver; use DW1000.Driver;

package body DecaDriver.Tx
with SPARK_Mode => On
is

   protected body Transmitter
   is

      entry Wait_For_Tx_Complete
      with SPARK_Mode => Off --  Workaround for "statement has no effect" below
        when Tx_Idle
      is
      begin
         null;
      end Wait_For_Tx_Complete;

      function Is_Tx_Complete return Boolean
      is
      begin
         return Tx_Idle;
      end Is_Tx_Complete;

      procedure Configure_Tx_Power
        (Config : DW1000.Driver.Tx_Power_Config_Type)
      is
      begin
         DW1000.Driver.Configure_Tx_Power (Config);
      end Configure_Tx_Power;

      procedure Set_Tx_Data (Data   : in DW1000.Types.Byte_Array;
                             Offset : in Natural)
      is
      begin
         DW1000.Driver.Set_Tx_Data (Data   => Data,
                                    Offset => Offset);
      end Set_Tx_Data;

      procedure Set_Tx_Frame_Length (Length : in Natural;
                                     Offset : in Natural)
      is
      begin
         DW1000.Driver.Set_Tx_Frame_Length (Length => Length,
                                            Offset => Offset);
      end Set_Tx_Frame_Length;

      procedure Set_Delayed_Tx_Time(Time : in Coarse_System_Time)
      is
      begin
         DW1000.Driver.Set_Delayed_Tx_Rx_Time (Time);
      end Set_Delayed_Tx_Time;

      procedure Start_Tx_Immediate (Rx_After_Tx : in     Boolean)
      is
      begin
         DW1000.Driver.Start_Tx_Immediate (Rx_After_Tx);
         Tx_Idle := False;
      end Start_Tx_Immediate;

      procedure Start_Tx_Delayed
        (Rx_After_Tx : in     Boolean;
         Result      :    out DW1000.Driver.Result_Type)
      is
      begin
         DW1000.Driver.Start_Tx_Delayed (Rx_After_Tx => Rx_After_Tx,
                                         Result      => Result);

         Tx_Idle := not (Result = DW1000.Driver.Success);
      end Start_Tx_Delayed;

      procedure Read_Tx_Adjusted_Timestamp (Timestamp : out Fine_System_Time)
      is
      begin
         DW1000.Driver.Read_Tx_Adjusted_Timestamp (Timestamp);
      end Read_Tx_Adjusted_Timestamp;

      procedure Read_Tx_Raw_Timestamp (Timestamp : out Coarse_System_Time)
      is
      begin
         DW1000.Driver.Read_Tx_Raw_Timestamp (Timestamp);
      end Read_Tx_Raw_Timestamp;

      procedure Notify_Tx_Complete
      is
      begin
         Tx_Idle := True;
      end Notify_Tx_Complete;

   end Transmitter;

end DecaDriver.Tx;
