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

with DW1000.System_Time;
with DW1000.Types;       use DW1000.Types;
with System;

--  This package defines types for each of the DW1000 registers.
--
--  Each register type has a representation clause to match the layout of the
--  register according to the DW1000 User Manual.
package DW1000.Register_Types
with SPARK_Mode => On
is

   ----------------------------------------------------------------------------
   -- DEV_ID register file

   type DEV_ID_Type is record
      REV    : Types.Bits_4  := 2#0000#;
      VER    : Types.Bits_4  := 2#0011#;
      MODEL  : Types.Bits_8  := 16#01#;
      RIDTAG : Types.Bits_16 := 16#DECA#;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for DEV_ID_Type use record
      REV    at 0 range  0 ..  3;
      VER    at 0 range  4 ..  7;
      MODEL  at 0 range  8 .. 15;
      RIDTAG at 0 range 16 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- EUI register file

   type EUI_Type is record
      EUI : Types.Bits_64;
   end record
     with Size => 64,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EUI_Type use record
      EUI at 0 range 0 .. 63;
   end record;

   ----------------------------------------------------------------------------
   -- PANDADR register file

   type PANADR_Type is record
      SHORT_ADDR : Types.Bits_16 := 16#FFFF#;
      PAN_ID     : Types.Bits_16 := 16#FFFF#;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for PANADR_Type use record
      SHORT_ADDR at 0 range  0 .. 15;
      PAN_ID     at 0 range 16 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- SYS_CFG register file

   type SYS_CFG_FFEN_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Frame Filtering Enable.

   type SYS_CFG_FFBC_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Frame Filtering Behave as a Coordinator.

   type SYS_CFG_FFAB_Field is
     (Not_Allowed,
      Allowed)
     with Size => 1;
   --  Frame Filtering Allow Beacon frame reception.

   type SYS_CFG_FFAD_Field is
     (Not_Allowed,
      Allowed)
     with Size => 1;
   --  Frame Filtering Allow Data frame reception.

   type SYS_CFG_FFAA_Field is
     (Not_Allowed,
      Allowed)
     with Size => 1;
   --  Frame Filtering Allow Acknowledgment frame reception.

   type SYS_CFG_FFAM_Field is
     (Not_Allowed,
      Allowed)
     with Size => 1;
   --  Frame Filtering Allow MAC command frame reception.

   type SYS_CFG_FFAR_Field is
     (Not_Allowed,
      Allowed)
     with Size => 1;
   --  Frame Filtering Allow Reserved frame types.

   type SYS_CFG_FFA4_Field is
     (Not_Allowed,
      Allowed)
     with Size => 1;
   --  Frame Filtering Allow frames with frame type field of 4, (binary 100).

   type SYS_CFG_FFA5_Field is
     (Not_Allowed,
      Allowed)
     with Size => 1;
   --  Frame Filtering Allow frames with frame type field of 5, (binary 101).

   type SYS_CFG_HIRQ_Pol_Field is
     (Active_Low,
      Active_High)
     with Size => 1;
   --  Host interrupt polarity.

   type SYS_CFG_SPI_EDGE_Field is
     (Sampling_Edge,
      Opposite_Edge)
     with Size => 1;
   --  SPI data launch edge.

   type SYS_CFG_DIS_FCE_Field is
     (Not_Disabled,
      Disabled)
     with Size => 1;
   --  Disable frame check error handling.

   type SYS_CFG_DIS_DRXB_Field is
     (Not_Disabled,
      Disabled)
     with Size => 1;
   --  Disable Double RX Buffer.

   type SYS_CFG_DIS_PHE_Field is
     (Not_Disabled,
      Disabled)
     with Size => 1;
   --  Disable receiver abort on PHR error.

   type SYS_CFG_DIS_RSDE_Field is
     (Not_Disabled,
      Disabled)
     with Size => 1;
   --  Disable Receiver Abort on RSD (Reed-Solomon Decoder) error.

   type SYS_CFG_FCS_INIT2F_Field is
     (All_Zeroes,
      All_Ones)
     with Size => 1;
   --  This bit allows selection of the initial seed value for the FCS
   --  generation and checking function that is set at the start of each frame
   --  transmission and reception.

   type SYS_CFG_PHR_MODE_Field is
     (Standard_Frames_Mode,
      Reserved_01,
      Reserved_10,
      Long_Frames_Mode)
     with Size => 2;
   --  Standard (max. 127 octets) or long (max. 1023 octets) frames.

   type SYS_CFG_DIS_STXP_Field is
     (Not_Disabled,
      Disabled)
     with Size => 1;
   --  Disable Smart TX Power control.

   type SYS_CFG_RXM110K_Field is
     (SFD_850K_6M8,
      SFD_110K)
     with Size => 1;
   --  Receiver Mode 110 kbps data rate.

   type SYS_CFG_RXWTOE_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Receive Wait Timeout Enable.

   type SYS_CFG_RXAUTR_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Receiver Auto-Re-enable.

   type SYS_CFG_AUTOACK_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Automatic Acknowledgement Enable.

   type SYS_CFG_AACKPEND_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Automatic Acknowledgement Pending bit control.

   type SYS_CFG_Type is record
      FFEN       : SYS_CFG_FFEN_Field       := Disabled;
      FFBC       : SYS_CFG_FFBC_Field       := Disabled;
      FFAB       : SYS_CFG_FFAB_Field       := Not_Allowed;
      FFAD       : SYS_CFG_FFAD_Field       := Not_Allowed;
      FFAA       : SYS_CFG_FFAA_Field       := Not_Allowed;
      FFAM       : SYS_CFG_FFAM_Field       := Not_Allowed;
      FFAR       : SYS_CFG_FFAR_Field       := Not_Allowed;
      FFA4       : SYS_CFG_FFA4_Field       := Not_Allowed;
      FFA5       : SYS_CFG_FFA5_Field       := Not_Allowed;
      HIRQ_POL   : SYS_CFG_HIRQ_Pol_Field   := Active_High;
      SPI_EDGE   : SYS_CFG_SPI_EDGE_Field   := Sampling_Edge;
      DIS_FCE    : SYS_CFG_DIS_FCE_Field    := Not_Disabled;
      DIS_DRXB   : SYS_CFG_DIS_DRXB_Field   := Disabled;
      DIS_PHE    : SYS_CFG_DIS_PHE_Field    := Not_Disabled;
      DIS_RSDE   : SYS_CFG_DIS_RSDE_Field   := Not_Disabled;
      FCS_INT2F  : SYS_CFG_FCS_INIT2F_Field := All_Zeroes;
      PHR_MODE   : SYS_CFG_PHR_MODE_Field   := Standard_Frames_Mode;
      DIS_STXP   : SYS_CFG_DIS_STXP_Field   := Not_Disabled;
      RXM110K    : SYS_CFG_RXM110K_Field    := SFD_850K_6M8;
      RXWTOE     : SYS_CFG_RXWTOE_Field     := Disabled;
      RXAUTR     : SYS_CFG_RXAUTR_Field     := Disabled;
      AUTOACK    : SYS_CFG_AUTOACK_Field    := Disabled;
      AACKPEND   : SYS_CFG_AACKPEND_Field   := Disabled;

      Reserved_1 : Types.Bits_3 := 0;
      Reserved_2 : Types.Bits_5 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for SYS_CFG_Type use record
      FFEN       at 0 range  0 ..  0;
      FFBC       at 0 range  1 ..  1;
      FFAB       at 0 range  2 ..  2;
      FFAD       at 0 range  3 ..  3;
      FFAA       at 0 range  4 ..  4;
      FFAM       at 0 range  5 ..  5;
      FFAR       at 0 range  6 ..  6;
      FFA4       at 0 range  7 ..  7;
      FFA5       at 0 range  8 ..  8;
      HIRQ_POL   at 0 range  9 ..  9;
      SPI_EDGE   at 0 range 10 .. 10;
      DIS_FCE    at 0 range 11 .. 11;
      DIS_DRXB   at 0 range 12 .. 12;
      DIS_PHE    at 0 range 13 .. 13;
      DIS_RSDE   at 0 range 14 .. 14;
      FCS_INT2F  at 0 range 15 .. 15;
      PHR_MODE   at 0 range 16 .. 17;
      DIS_STXP   at 0 range 18 .. 18;

      Reserved_1 at 0 range 19 .. 21;

      RXM110K    at 0 range 22 .. 22;

      Reserved_2 at 0 range 23 .. 27;

      RXWTOE     at 0 range 28 .. 28;
      RXAUTR     at 0 range 29 .. 29;
      AUTOACK    at 0 range 30 .. 30;
      AACKPEND   at 0 range 31 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- SYS_TIME register file

   type SYS_TIME_Type is record
      SYS_TIME : System_Time.Coarse_System_Time;
   end record
     with Pack, Size => 40,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   ----------------------------------------------------------------------------
   -- TX_FCTRL register file

   type TX_FCTRL_TFLEN_Field is range 0 .. 127
     with Size => 7;
   --  Transmit Frame Length.

   type TX_FCTRL_TFLE_Field is range 0 .. 7
     with Size => 3;
   --  Transmit Frame Length Extension.

   type TX_FCTRL_TXBR_Field is
     (Data_Rate_110K,
      Data_Rate_850K,
      Data_Rate_6M8,
      Reserved)
     with Size => 2;
   --  Transmit Bit Rate.

   type TX_FCTRL_TR_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Transmit Ranging enable.

   type TX_FCTRL_TXPRF_Field is
     (PRF_4MHz,
      PRF_16MHz,
      PRF_64MHz,
      Reserved)
     with Size => 2;
   --  Transmit Pulse Repetition Frequency.

   type TX_FCTRL_TXPSR_Field is
     (PLEN_16,
      PLEN_64,
      PLEN_1024,
      PLEN_4096)
     with Size => 2;
   --  Transmit Preamble Symbol Repetitions (PSR).

   type TX_FCTRL_PE_Field is range 0 .. 3
     with Size => 2;
   --  Preamble Extension.

   type TX_FCTRL_TXBOFFS_Field is range 0 .. 2**10 - 1
     with Size => 10;
   --  Transmit buffer index offset.

   type TX_FCTRL_IFSDELAY_Field is range 0 .. 255
     with Size => 8;
   --  Inter-Frame Spacing.

   type TX_FCTRL_Type is record
      TFLEN    : TX_FCTRL_TFLEN_Field    := 12;
      TFLE     : TX_FCTRL_TFLE_Field     := 0;
      R        : Types.Bits_3            := 0;
      TXBR     : TX_FCTRL_TXBR_Field     := Data_Rate_6M8;
      TR       : TX_FCTRL_TR_Field       := Disabled;
      TXPRF    : TX_FCTRL_TXPRF_Field    := PRF_16MHz;
      TXPSR    : TX_FCTRL_TXPSR_Field    := PLEN_64;
      PE       : TX_FCTRL_PE_Field       := 0;
      TXBOFFS  : TX_FCTRL_TXBOFFS_Field  := 0;
      IFSDELAY : TX_FCTRL_IFSDELAY_Field := 0;
   end record
     with Size => 40,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TX_FCTRL_Type use record
      TFLEN    at 0 range  0 ..  6;
      TFLE     at 0 range  7 ..  9;
      R        at 0 range 10 .. 12;
      TXBR     at 0 range 13 .. 14;
      TR       at 0 range 15 .. 15;
      TXPRF    at 0 range 16 .. 17;
      TXPSR    at 0 range 18 .. 19;
      PE       at 0 range 20 .. 21;
      TXBOFFS  at 0 range 22 .. 31;
      IFSDELAY at 0 range 32 .. 39;
   end record;

   ----------------------------------------------------------------------------
   -- TX_BUFFER register file

   type TX_BUFFER_Type is record
      TX_BUFFER : Types.Byte_Array(1 .. 1024) := (others => 0);
   end record
     with Size => 8192,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TX_BUFFER_Type use record
      TX_BUFFER at 0 range 0 .. 8191;
   end record;

   ----------------------------------------------------------------------------
   -- DX_TIME register file

   type DX_TIME_Type is record
      DX_TIME : System_Time.Coarse_System_Time := 0.0;
   end record
     with Pack, Size => 40,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   ----------------------------------------------------------------------------
   -- RX_FWTO register file

   type RX_FWTO_Type is record
      RXFWTO : System_Time.Frame_Wait_Timeout_Time := 0.0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RX_FWTO_Type use record
      RXFWTO at 0 range 0 .. 15;
   end record;

   ----------------------------------------------------------------------------
   -- SYS_CTRL register file

   type SYS_CTRL_SFCST_Field is
     (Not_Suppressed,
      Suppressed)
     with Size => 1;
   --  Suppress auto-FCS Transmission (on this next frame).

   type SYS_CTRL_TXSTRT_Field is
     (No_Action,
      Start_Tx)
     with Size => 1;
   --  Transmit Start.

   type SYS_CTRL_TXDLYS_Field is
     (Not_Delayed,
      Delayed)
     with Size => 1;
   --  Transmitter Delayed Sending.

   type SYS_CTRL_CANSFCS_Field is
     (Not_Cancelled,
      Cancelled)
     with Size => 1;
   --  Cancel Suppression of auto-FCS transmission (on the current frame).

   type SYS_CTRL_TRXOFF_Field is
     (No_Action,
      Transceiver_Off)
     with Size => 1;
   --  Transceiver Off.

   type SYS_CTRL_WAIT4RESP_Field is
     (No_Wait,
      Wait)
     with Size => 1;
   --  Wait for Response.

   type SYS_CTRL_RXENAB_Field is
     (No_Action,
      Start_Rx)
     with Size => 1;
   --  Enable Receiver.

   type SYS_CTRL_RXDLYE_Field is
     (Not_Delayed,
      Delayed)
     with Size => 1;
   --  Receiver Delayed Enable.

   type SYS_CTRL_HRBPT_Field is
     (No_Action,
      Toggle)
     with Size => 1;
   --  Host Side Receive Buffer Pointer Toggle.

   type SYS_CTRL_Type is record
      SFCST     : SYS_CTRL_SFCST_Field     := Not_Suppressed;
      TXSTRT    : SYS_CTRL_TXSTRT_Field    := No_Action;
      TXDLYS    : SYS_CTRL_TXDLYS_Field    := Not_Delayed;
      CANSFCS   : SYS_CTRL_CANSFCS_Field   := Not_Cancelled;
      TRXOFF    : SYS_CTRL_TRXOFF_Field    := No_Action;
      WAIT4RESP : SYS_CTRL_WAIT4RESP_Field := No_Wait;
      RXENAB    : SYS_CTRL_RXENAB_Field    := No_Action;
      RXDLYE    : SYS_CTRL_RXDLYE_Field    := Not_Delayed;
      HRBPT     : SYS_CTRL_HRBPT_Field     := No_Action;

      Reserved_1 : Types.Bits_2  := 0;
      Reserved_2 : Types.Bits_14 := 0;
      Reserved_3 : Types.Bits_7  := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for SYS_CTRL_Type use record
      SFCST      at 0 range  0 ..  0;
      TXSTRT     at 0 range  1 ..  1;
      TXDLYS     at 0 range  2 ..  2;
      CANSFCS    at 0 range  3 ..  3;

      Reserved_1 at 0 range  4 ..  5;

      TRXOFF     at 0 range  6 ..  6;
      WAIT4RESP  at 0 range  7 ..  7;
      RXENAB     at 0 range  8 ..  8;
      RXDLYE     at 0 range  9 ..  9;

      Reserved_2 at 0 range 10 .. 23;

      HRBPT      at 0 range 24 .. 24;

      Reserved_3 at 0 range 25 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- SYS_MASK register file

   type SYS_MASK_Mask_Field is
     (Masked,
      Not_Masked)
     with Size => 1;
   --  Mask event.

   type SYS_MASK_Type is record
      MCPLOCK   : SYS_MASK_Mask_Field := Masked;
      MESYNCR   : SYS_MASK_Mask_Field := Masked;
      MAAT      : SYS_MASK_Mask_Field := Masked;
      MTXFRB    : SYS_MASK_Mask_Field := Masked;
      MTXPRS    : SYS_MASK_Mask_Field := Masked;
      MTXPHS    : SYS_MASK_Mask_Field := Masked;
      MTXFRS    : SYS_MASK_Mask_Field := Masked;
      MRXPRD    : SYS_MASK_Mask_Field := Masked;
      MRXSFDD   : SYS_MASK_Mask_Field := Masked;
      MLDEDONE  : SYS_MASK_Mask_Field := Masked;
      MRXPHD    : SYS_MASK_Mask_Field := Masked;
      MRXPHE    : SYS_MASK_Mask_Field := Masked;
      MRXDFR    : SYS_MASK_Mask_Field := Masked;
      MRXFCG    : SYS_MASK_Mask_Field := Masked;
      MRXFCE    : SYS_MASK_Mask_Field := Masked;
      MRXRFSL   : SYS_MASK_Mask_Field := Masked;
      MRXRFTO   : SYS_MASK_Mask_Field := Masked;
      MLDEERR   : SYS_MASK_Mask_Field := Masked;
      MRXOVRR   : SYS_MASK_Mask_Field := Masked;
      MRXPTO    : SYS_MASK_Mask_Field := Masked;
      MGPIOIRQ  : SYS_MASK_Mask_Field := Masked;
      MSLP2INIT : SYS_MASK_Mask_Field := Masked;
      MRFPLLLL  : SYS_MASK_Mask_Field := Masked;
      MCPLLLL   : SYS_MASK_Mask_Field := Masked;
      MRXSFDTO  : SYS_MASK_Mask_Field := Masked;
      MHPDWARN  : SYS_MASK_Mask_Field := Masked;
      MTXBERR   : SYS_MASK_Mask_Field := Masked;
      MAFFREJ   : SYS_MASK_Mask_Field := Masked;

      Reserved_1 : Types.Bits_1 := 0;
      Reserved_2 : Types.Bits_1 := 0;
      Reserved_3 : Types.Bits_2 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for SYS_MASK_Type use record
      Reserved_1 at 0 range 0 .. 0;

      MCPLOCK    at 0 range  1 ..  1;
      MESYNCR    at 0 range  2 ..  2;
      MAAT       at 0 range  3 ..  3;
      MTXFRB     at 0 range  4 ..  4;
      MTXPRS     at 0 range  5 ..  5;
      MTXPHS     at 0 range  6 ..  6;
      MTXFRS     at 0 range  7 ..  7;
      MRXPRD     at 0 range  8 ..  8;
      MRXSFDD    at 0 range  9 ..  9;
      MLDEDONE   at 0 range 10 .. 10;
      MRXPHD     at 0 range 11 .. 11;
      MRXPHE     at 0 range 12 .. 12;
      MRXDFR     at 0 range 13 .. 13;
      MRXFCG     at 0 range 14 .. 14;
      MRXFCE     at 0 range 15 .. 15;
      MRXRFSL    at 0 range 16 .. 16;
      MRXRFTO    at 0 range 17 .. 17;
      MLDEERR    at 0 range 18 .. 18;

      Reserved_2 at 0 range 19 .. 19;

      MRXOVRR    at 0 range 20 .. 20;
      MRXPTO     at 0 range 21 .. 21;
      MGPIOIRQ   at 0 range 22 .. 22;
      MSLP2INIT  at 0 range 23 .. 23;
      MRFPLLLL   at 0 range 24 .. 24;
      MCPLLLL    at 0 range 25 .. 25;
      MRXSFDTO   at 0 range 26 .. 26;
      MHPDWARN   at 0 range 27 .. 27;
      MTXBERR    at 0 range 28 .. 28;
      MAFFREJ    at 0 range 29 .. 29;

      Reserved_3 at 0 range 30 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- SYS_STATUS register file

   type SYS_STATUS_Type is record
      IRQS      : Types.Bits_1  := 0;
      CPLOCK    : Types.Bits_1  := 0;
      ESYNCR    : Types.Bits_1  := 0;
      AAT       : Types.Bits_1  := 0;
      TXFRB     : Types.Bits_1  := 0;
      TXPRS     : Types.Bits_1  := 0;
      TXPHS     : Types.Bits_1  := 0;
      TXFRS     : Types.Bits_1  := 0;
      RXPRD     : Types.Bits_1  := 0;
      RXSFDD    : Types.Bits_1  := 0;
      LDEDONE   : Types.Bits_1  := 0;
      RXPHD     : Types.Bits_1  := 0;
      RXPHE     : Types.Bits_1  := 0;
      RXDFR     : Types.Bits_1  := 0;
      RXFCG     : Types.Bits_1  := 0;
      RXFCE     : Types.Bits_1  := 0;
      RXRFSL    : Types.Bits_1  := 0;
      RXRFTO    : Types.Bits_1  := 0;
      LDEERR    : Types.Bits_1  := 0;
      RXOVRR    : Types.Bits_1  := 0;
      RXPTO     : Types.Bits_1  := 0;
      GPIOIRQ   : Types.Bits_1  := 0;
      SLP2INIT  : Types.Bits_1  := 0;
      RFPLL_LL  : Types.Bits_1  := 0;
      CLKPLL_LL : Types.Bits_1  := 0;
      RXSFDTO   : Types.Bits_1  := 0;
      HPDWARN   : Types.Bits_1  := 0;
      TXBERR    : Types.Bits_1  := 0;
      AFFREJ    : Types.Bits_1  := 0;
      HSRBP     : Types.Bits_1  := 0;
      ICRBP     : Types.Bits_1  := 0;
      RXRSCS    : Types.Bits_1  := 0;
      RXPREJ    : Types.Bits_1  := 0;
      TXPUTE    : Types.Bits_1  := 0;

      Reserved_1 : Types.Bits_1 := 0;
      Reserved_2 : Types.Bits_5 := 0;
   end record
     with Size => 40,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for SYS_STATUS_Type use record
      IRQS       at 0 range  0 ..  0;
      CPLOCK     at 0 range  1 ..  1;
      ESYNCR     at 0 range  2 ..  2;
      AAT        at 0 range  3 ..  3;
      TXFRB      at 0 range  4 ..  4;
      TXPRS      at 0 range  5 ..  5;
      TXPHS      at 0 range  6 ..  6;
      TXFRS      at 0 range  7 ..  7;
      RXPRD      at 0 range  8 ..  8;
      RXSFDD     at 0 range  9 ..  9;
      LDEDONE    at 0 range 10 .. 10;
      RXPHD      at 0 range 11 .. 11;
      RXPHE      at 0 range 12 .. 12;
      RXDFR      at 0 range 13 .. 13;
      RXFCG      at 0 range 14 .. 14;
      RXFCE      at 0 range 15 .. 15;
      RXRFSL     at 0 range 16 .. 16;
      RXRFTO     at 0 range 17 .. 17;
      LDEERR     at 0 range 18 .. 18;

      Reserved_1 at 0 range 19 .. 19;

      RXOVRR     at 0 range 20 .. 20;
      RXPTO      at 0 range 21 .. 21;
      GPIOIRQ    at 0 range 22 .. 22;
      SLP2INIT   at 0 range 23 .. 23;
      RFPLL_LL   at 0 range 24 .. 24;
      CLKPLL_LL  at 0 range 25 .. 25;
      RXSFDTO    at 0 range 26 .. 26;
      HPDWARN    at 0 range 27 .. 27;
      TXBERR     at 0 range 28 .. 28;
      AFFREJ     at 0 range 29 .. 29;
      HSRBP      at 0 range 30 .. 30;
      ICRBP      at 0 range 31 .. 31;
      RXRSCS     at 4 range  0 ..  0;
      RXPREJ     at 4 range  1 ..  1;
      TXPUTE     at 4 range  2 ..  2;

      Reserved_2 at 4 range  3 ..  7;
   end record;

   ----------------------------------------------------------------------------
   -- RX_FINFO register file

   type RX_FINFO_RXFLEN_Field is range 0 .. 127
     with Size => 7;
   --  Receive Frame Length.

   type RX_FINFO_RXFLE_Field is range 0 .. 7
     with Size => 3;
   --  Receive Frame Length Extension.

   type RX_FINFO_RXNSPL_Field is range 0 .. 3
     with Size => 2;
   --  Receive non-standard preamble length.

   type RX_FINFO_RXBR_Field is
     (Data_Rate_110K,
      Data_Rate_850K,
      Data_Rate_6M8,
      Reserved)
     with Size => 2;
   --  Receive Bit Rate report.

   type RX_FINFO_RNG_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Receiver Ranging.

   type RX_FINFO_RXPRFR_Field is
     (Reserved_00,
      PRF_16MHz,
      PRF_64MHz,
      Reserved_11)
     with Size => 2;
   --  RX Pulse Repetition Rate report.

   type RX_FINFO_RXPSR_Field is
     (PLEN_16,
      PLEN_64,
      PLEN_1024,
      PLEN_4096)
     with Size => 2;
   --  RX Preamble Repetition.

   type RX_FINFO_RXPACC_Field is range 0 .. 2**12 - 1
     with Size => 12;

   type RX_FINFO_Type is record
      RXFLEN : RX_FINFO_RXFLEN_Field := 0;
      RXFLE  : RX_FINFO_RXFLE_Field  := 0;
      RXNSPL : RX_FINFO_RXNSPL_Field := 0;
      RXBR   : RX_FINFO_RXBR_Field   := Data_Rate_110K;
      RNG    : RX_FINFO_RNG_Field    := Disabled;
      RXPRF  : RX_FINFO_RXPRFR_Field := Reserved_00;
      RXPSR  : RX_FINFO_RXPSR_Field  := PLEN_16;
      RXPACC : RX_FINFO_RXPACC_Field := 0;

      Reserved : Types.Bits_1 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RX_FINFO_Type use record
      RXFLEN   at 0 range  0 ..  6;
      RXFLE    at 0 range  7 ..  9;

      Reserved at 0 range 10 .. 10;

      RXNSPL   at 0 range 11 .. 12;
      RXBR     at 0 range 13 .. 14;
      RNG      at 0 range 15 .. 15;
      RXPRF    at 0 range 16 .. 17;
      RXPSR    at 0 range 18 .. 19;
      RXPACC   at 0 range 20 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- RX_BUFFER register file

   type RX_BUFFER_Type is record
      RX_BUFFER : Types.Byte_Array(1 .. 1024) := (others => 0);
   end record
     with Size => 8192,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RX_BUFFER_Type use record
      RX_BUFFER at 0 range 0 .. 8191;
   end record;

   ----------------------------------------------------------------------------
   -- RX_FQUAL register file

   type RX_FQUAL_STD_NOISE_Field is range 0 .. 2**16 - 1
     with Size => 16;
   --  Standard Deviation of Noise.

   type RX_FQUAL_FP_AMPL2_Field is range 0 .. 2**16 - 1
     with Size => 16;
   --  First Path Amplitude point 2.

   type RX_FQUAL_FP_AMPL3_Field is range 0 .. 2**16 - 1
     with Size => 16;
   --  First Path Amplitude point 3.

   type RX_FQUAL_CIR_PWR_Field is range 0 .. 2**16 - 1
     with Size => 16;
   --  Channel Impulse Response Power.

   type RX_FQUAL_Type is record
      STD_NOISE : RX_FQUAL_STD_NOISE_Field := 0;
      FP_AMPL2  : RX_FQUAL_FP_AMPL2_Field  := 0;
      FP_AMPL3  : RX_FQUAL_FP_AMPL3_Field  := 0;
      CIR_PWR   : RX_FQUAL_CIR_PWR_Field   := 0;
   end record
     with Size => 64,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RX_FQUAL_Type use record
      STD_NOISE at 0 range  0 .. 15;
      FP_AMPL2  at 0 range 16 .. 31;
      FP_AMPL3  at 4 range  0 .. 15;
      CIR_PWR   at 4 range 16 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- RX_TTCKI register file

   type RX_TTCKI_RXTTCKI_Field is range 0 .. 2**32 - 1
     with Size => 32;
   --  RX time tracking interval.

   type RX_TTCKI_Type is record
      RXTTCKI : RX_TTCKI_RXTTCKI_Field := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RX_TTCKI_Type use record
      RXTTCKI at 0 range 0 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- RX_TTCKO register file

   type RX_TTCKO_RXTOFS_Field is range -2**18 .. 2**18 - 1
     with Size => 19;
   --  RX time tracking offset.

   type RX_TTCKO_RSMPDEL_Field is range 0 .. 255
     with Size => 8;
   --  This 8-bit field reports an internal re-sampler delay value.

   type RX_TTCKO_RCPHASE_Field is
   delta 360.0 / 127.0
   range 0.0 .. 360.0
     with Small => 360.0 / 127.0,
       Size => 7;
   --  This 7-bit field reports the receive carrier phase adjustment at time
   --  the ranging timestamp is made. This gives the phase (7 bits = 360 degrees)
   --  of the internal carrier tracking loop at the time that the RX timestamp
   --  is received.

   type RX_TTCKO_Type is record
      RXTOFS  : RX_TTCKO_RXTOFS_Field  := 0;
      RSMPDEL : RX_TTCKO_RSMPDEL_Field := 0;
      RCPHASE : RX_TTCKO_RCPHASE_Field := RX_TTCKO_RCPHASE_Field'First;

      Reserved_1 : Types.Bits_5 := 0;
      Reserved_2 : Types.Bits_1 := 0;
   end record
     with Size => 40,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RX_TTCKO_Type use record
      RXTOFS     at 0 range  0 .. 18;

      Reserved_1 at 0 range 19 .. 23;

      RSMPDEL    at 0 range 24 .. 31;
      RCPHASE    at 0 range 32 .. 38;

      Reserved_2 at 0 range 39 .. 39;
   end record;

   ----------------------------------------------------------------------------
   -- RX_TIME register file

   type RX_TIME_FP_INDEX_Field is range 0 .. 2**16 - 1
     with Size => 16;
   --  First path index.

   type RX_TIME_FP_AMPL1_Field is range 0 .. 2**16 - 1
     with Size => 16;
   --  First Path Amplitude point 1.

   type RX_TIME_Type is record
      RX_STAMP : System_Time.Fine_System_Time   := 0.0;
      FP_INDEX : RX_TIME_FP_INDEX_Field         := 0;
      FP_AMPL1 : RX_TIME_FP_AMPL1_Field         := 0;
      RX_RAWST : System_Time.Coarse_System_Time := 0.0;
   end record
     with Size => 8*14,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RX_TIME_Type use record
      RX_STAMP at 0 range  0 .. 39;
      FP_INDEX at 4 range  8 .. 23;
      FP_AMPL1 at 4 range 24 .. 39;
      RX_RAWST at 8 range  8 .. 47;
   end record;

   ----------------------------------------------------------------------------
   -- TX_TIME register file

   type TX_TIME_Type is record
      TX_STAMP : System_Time.Fine_System_Time   := 0.0;
      TX_RAWST : System_Time.Coarse_System_Time := 0.0;
   end record
     with Size => 80,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TX_TIME_Type use record
      TX_STAMP at 0 range 0 .. 39;
      TX_RAWST at 4 range 8 .. 47;
   end record;

   ----------------------------------------------------------------------------
   -- TX_ANTD register file

   type TX_ANTD_Type is record
      TX_ANTD : System_Time.Antenna_Delay_Time := 0.0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TX_ANTD_Type use record
      TX_ANTD at 0 range 0 .. 15;
   end record;

   ----------------------------------------------------------------------------
   --  SYS_STATE register file

   type SYS_STATE_TX_STATE_Field is
     (Idle,
      Preamble,
      SFD,
      PHR,
      SDE,
      DATA,
      Reserved_0110,
      Reserved_0111,
      Reserved_1000,
      Reserved_1001,
      Reserved_1010,
      Reserved_1011,
      Reserved_1100,
      Reserved_1101,
      Reserved_1110,
      Reserved_1111)
     with Size => 4;
   --  Current Transmit State Machine value

   type SYS_STATE_RX_STATE_Field is
     (Idle,
      Start_Analog,
      Reserved_00010,
      Reserved_00011,
      Rx_Ready,
      Preamble_Search,
      Preamble_Timeout,
      SFD_Search,
      Configure_PHR_Rx,
      PHY_Rx_Start,
      Data_Rate_Ready,
      Reserved_01011,
      Data_Rx_Seq,
      Configure_Data_Rx,
      PHR_Not_OK,
      Last_Symbol,
      Wait_RSD_Done,
      RSD_OK,
      RSD_Not_OK,
      Reconfigure_110K,
      Wait_110K_PHR,
      Reserved_10101,
      Reserved_10110,
      Reserved_10111,
      Reserved_11000,
      Reserved_11001,
      Reserved_11010,
      Reserved_11011,
      Reserved_11100,
      Reserved_11101,
      Reserved_11110,
      Reserved_11111)
     with Size => 5;
   --  Current Receive State Machine value

   type SYS_STATE_PMSC_STATE_Field is
     (Init,
      Idle,
      TX_Wait,
      RX_Wait,
      Tx,
      Rx,
      Reserved_0110,
      Reserved_0111,
      Reserved_1000,
      Reserved_1001,
      Reserved_1010,
      Reserved_1011,
      Reserved_1100,
      Reserved_1101,
      Reserved_1110,
      Reserved_1111)
     with Size => 4;

   type SYS_STATE_Type is record
      TX_STATE   : SYS_STATE_TX_STATE_Field   := Idle;
      RX_STATE   : SYS_STATE_RX_STATE_Field   := Idle;
      PMSC_STATE : SYS_STATE_PMSC_STATE_Field := Idle;

      Reserved_1 : Bits_4  := 0;
      Reserved_2 : Bits_3  := 0;
      Reserved_3 : Bits_12 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for SYS_STATE_Type use record
      TX_STATE   at 0 range 0 .. 3;

      Reserved_1 at 0 range 4 .. 7;

      RX_STATE   at 0 range 8 .. 12;

      Reserved_2 at 0 range 13 .. 15;

      PMSC_STATE at 0 range 16 .. 19;

      Reserved_3 at 0 range 20 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- ACK_RESP_T register file

   type ACK_RESP_T_ACK_TIM_Field is range 0 .. 255
     with Size => 8;
   --  Auto-Acknowledgement turn-around Time (in number of preamble symbols).

   type ACK_RESP_T_Type is record
      W4D_TIM : System_Time.Response_Wait_Timeout_Time := 0.0;
      ACK_TIM : ACK_RESP_T_ACK_TIM_Field               := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for ACK_RESP_T_Type use record
      W4D_TIM  at 0 range 0 .. 19;

      Reserved at 0 range 20 .. 23;

      ACK_TIM  at 0 range 24 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- RX_SNIFF register file

   type RX_SNIFF_SNIFF_ONT_Field is range 0 .. 15
     with Size => 4;
   --  SNIFF Mode ON time. This parameter is specified in units of PAC.

   type RX_SNIFF_Type is record
      SNIFF_ONT  : RX_SNIFF_SNIFF_ONT_Field   := 0;
      SNIFF_OFFT : System_Time.Sniff_Off_Time := 0.0;

      Reserved_1 : Types.Bits_4  := 0;
      Reserved_2 : Types.Bits_16 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RX_SNIFF_Type use record
      SNIFF_ONT  at 0 range  0 ..  3;

      Reserved_1 at 0 range  4 ..  7;

      SNIFF_OFFT at 0 range  8 .. 15;

      Reserved_2 at 0 range 16 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- TX_POWER register file

   type TX_POWER_COARSE_Field is
     (Gain_15_dB,
      Gain_12_5_dB,
      Gain_10_dB,
      Gain_7_5_dB,
      Gain_5_dB,
      Gain_2_5_dB,
      Gain_0_dB,
      Off) --  No output when used
     with Size => 3;
   --  Coarse (DA setting) gain in 2.5 dB steps.
   --
   --  Unfortunately we can't use a fixed-point type here (like the fine gain)
   --  due to the reversed HW representation of the coarse gain and the
   --  need for the special "Off" setting.

   type TX_POWER_FINE_Field is delta 0.5 range 0.0 .. 15.5
     with Size => 5;
   --  Fine (Mixer) gain in dB (Decibels) in 0.5 dB steps.

   type TX_POWER_Field is record
      Fine_Gain   : TX_POWER_FINE_Field   := 0.0;
      Coarse_Gain : TX_POWER_COARSE_Field := Gain_15_dB;
   end record
     with Size => 8;

   for TX_POWER_Field use record
      Fine_Gain   at 0 range 0 .. 4;
      Coarse_Gain at 0 range 5 .. 7;
   end record;

   type TX_POWER_Type is record
      BOOSTNORM : TX_POWER_Field := (Fine_Gain   => 1.0,
                                     Coarse_Gain => Gain_12_5_dB);
      BOOSTP500 : TX_POWER_Field := (Fine_Gain   => 1.0,
                                     Coarse_Gain => Gain_15_dB);
      BOOSTP250 : TX_POWER_Field := (Fine_Gain   => 4.0,
                                     Coarse_Gain => Gain_15_dB);
      BOOSTP125 : TX_POWER_Field := (Fine_Gain   => 7.0,
                                     Coarse_Gain => Gain_15_dB);
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TX_POWER_Type use record
      BOOSTNORM at 0 range  0 ..  7;
      BOOSTP500 at 0 range  8 .. 15;
      BOOSTP250 at 0 range 16 .. 23;
      BOOSTP125 at 0 range 24 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- CHAN_CTRL register file

   type CHAN_CTRL_Channel_Field is range 0 .. 15
     with Size => 4;
   --  This selects the transmit/receive channel.

   type CHAN_CTRL_DWSFD_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  This bit enables a non-standard Decawave proprietary SFD sequence.

   type CHAN_CTRL_RXPRF_Field is
     (Reserved_00,
      PRF_16MHz,
      PRF_64MHz,
      Reserved_11)
     with Size => 2;
   --  This two bit field selects the PRF used in the receiver.

   type CHAN_CTRL_TNSSFD_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  This bit enables the use of a user specified (non-standard) SFD in the
   --  transmitter.

   type CHAN_CTRL_RNSSFD_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  This bit enables the use of a user specified (non-standard) SFD in the
   --  receiver.

   type CHAN_CTRL_PCODE_Field is range 0 .. 31
     with Size => 5;
   --  This field selects the preamble code used in the transmitter/receiver.

   type CHAN_CTRL_Type is record
      TX_CHAN  : CHAN_CTRL_Channel_Field := 5;
      RX_CHAN  : CHAN_CTRL_Channel_Field := 5;
      DWSFD    : CHAN_CTRL_DWSFD_Field   := Disabled;
      RXPRF    : CHAN_CTRL_RXPRF_Field   := Reserved_00;
      TNSSFD   : CHAN_CTRL_TNSSFD_Field  := Disabled;
      RNSSFD   : CHAN_CTRL_RNSSFD_Field  := Disabled;
      TX_PCODE : CHAN_CTRL_PCODE_Field   := 0;
      RX_PCODE : CHAN_CTRL_PCODE_Field   := 0;

      Reserved : Types.Bits_9 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for CHAN_CTRL_Type use record
      TX_CHAN  at 0 range  0 ..  3;
      RX_CHAN  at 0 range  4 ..  7;

      Reserved at 0 range  8 .. 16;

      DWSFD    at 0 range 17 .. 17;
      RXPRF    at 0 range 18 .. 19;
      TNSSFD   at 0 range 20 .. 20;
      RNSSFD   at 0 range 21 .. 21;
      TX_PCODE at 0 range 22 .. 26;
      RX_PCODE at 0 range 27 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- USR_SFD register file

   type USR_SFD_Type is record
      Sub_Registers : Types.Byte_Array(0 .. 40);
   end record
     with Size => 41*8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for USR_SFD_Type use record
      Sub_Registers at 0 range 0 .. 41*8 - 1;
   end record;

   ----------------------------------------------------------------------------
   -- AGC_CTRL register file

   ------------------------------
   --  AGC_CTRL1 sub-register  --
   ------------------------------

   type AGC_CTRL1_DIS_AM_Field is
     (Not_Disabled,
      Disabled)
     with Size => 1;

   type AGC_CTRL1_Type is record
      DIS_AM : AGC_CTRL1_DIS_AM_Field := Disabled;

      Reserved : Types.Bits_15 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AGC_CTRL1_Type use record
      DIS_AM   at 0 range 0 .. 0;

      Reserved at 0 range 1 .. 15;
   end record;

   ------------------------------
   --  AGC_TUNE1 sub-register  --
   ------------------------------

   type AGC_TUNE1_Field is new Bits_16;

   AGC_TUNE1_PRF_16MHz : constant AGC_TUNE1_Field := 16#8870#;
   AGC_TUNE1_PRF_64MHz : constant AGC_TUNE1_Field := 16#889B#;

   type AGC_TUNE1_Type is record
      AGC_TUNE1 : AGC_TUNE1_Field;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AGC_TUNE1_Type use record
      AGC_TUNE1 at 0 range 0 .. 15;
   end record;

   ------------------------------
   --  AGC_TUNE2 sub-register  --
   ------------------------------

   type AGC_TUNE2_Field is new Bits_32;

   AGC_TUNE2_Value : constant AGC_TUNE2_Field := 16#2502A907#;

   type AGC_TUNE2_Type is record
      AGC_TUNE2 : AGC_TUNE2_Field;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AGC_TUNE2_Type use record
      AGC_TUNE2 at 0 range 0 .. 31;
   end record;

   ------------------------------
   --  AGC_TUNE3 sub-register  --
   ------------------------------

   type AGC_TUNE3_Field is new Bits_16;

   AGC_TUNE3_Value : constant AGC_TUNE3_Field := 16#0035#;

   type AGC_TUNE3_Type is record
      AGC_TUNE3 : AGC_TUNE3_Field;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AGC_TUNE3_Type use record
      AGC_TUNE3 at 0 range 0 .. 15;
   end record;

   ------------------------------
   --  AGC_STAT1 sub-register  --
   ------------------------------

   type AGC_STAT1_EDG1_Field is range 0 .. 2**5 - 1
     with Size => 5;
   --  This 5-bit gain value relates to input noise power measurement.

   type AGC_STAT1_EDV2_Field is range 0 .. 2**9 - 1
     with Size => 9;
   --  This 9-bit value relates to the input noise power measurement.

   type AGC_STAT1_Type is record
      EDG1 : AGC_STAT1_EDG1_Field;
      EDV2 : AGC_STAT1_EDV2_Field;

      Reserved_1 : Types.Bits_6;
      Reserved_2 : Types.Bits_4;
   end record
     with Size => 24,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AGC_STAT1_Type use record
      Reserved_1 at 0 range  0 ..  5;

      EDG1       at 0 range  6 .. 10;
      EDV2       at 0 range 11 .. 19;

      Reserved_2 at 0 range 20 .. 23;
   end record;

   ----------------------------------------------------------------------------
   -- EXT_SYNC register file

   ---------------------------
   -- EC_CTRL sub-register  --
   ---------------------------

   type EC_CTRL_OSTSM_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  External transmit synchronisation mode enable.

   type EC_CTRL_OSRSM_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  External receive synchronisation mode enable.

   type EC_CTRL_PLLLDT_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Clock PLL lock detect tune.

   type EC_CTRL_WAIT_Field is range 0 .. 255
     with Size => 8;
   --  Wait counter used for external transmit synchronisation and external
   --  timebase reset.
   --
   --  The wait time is the number of external clock cycles (@ 38.4 MHz).

   type EC_CTRL_OSTRM_Field is
     (Disabled,
      Enabled)
     with Size => 1;

   type EC_CTRL_Type is record
      OSTSM  : EC_CTRL_OSTSM_Field  := Disabled;
      OSRSM  : EC_CTRL_OSRSM_Field  := Disabled;
      PLLLDT : EC_CTRL_PLLLDT_Field := Enabled;
      WAIT   : EC_CTRL_WAIT_Field   := 0;
      OSTRM  : EC_CTRL_OSTRM_Field  := Disabled;

      Reserved : Types.Bits_20 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EC_CTRL_Type use record
      OSTSM    at 0 range  0 ..  0;
      OSRSM    at 0 range  1 ..  1;
      PLLLDT   at 0 range  2 ..  2;
      WAIT     at 0 range  3 .. 10;
      OSTRM    at 0 range 11 .. 11;

      Reserved at 0 range 12 .. 31;
   end record;

   ---------------------------
   -- EC_RXTC sub-register  --
   ---------------------------

   type EC_RCTC_RX_TS_EST_Field is range 0 .. 2**32 - 1
     with Size => 32;
   --  External clock synchronisation counter captured on RMARKER.

   type EC_RXTC_Type is record
      RX_TS_EST : EC_RCTC_RX_TS_EST_Field;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EC_RXTC_Type use record
      RX_TS_EST at 0 range 0 .. 31;
   end record;

   ---------------------------
   -- EC_GOLP sub-register  --
   ---------------------------

   type EC_GOLP_OFFSET_EXT_Field is range 0 .. 2**6 - 1
     with Size => 6;

   type EC_GOLP_Type is record
      OFFSET_EXT : EC_GOLP_OFFSET_EXT_Field := 0;

      Reserved   : Types.Bits_26 := 0;
   end record
   with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EC_GOLP_Type use record
      OFFSET_EXT at 0 range 0 .. 5;

      Reserved   at 0 range 6 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- ACC_MEM register file

   type ACC_MEM_Number_Type is range -32_768 .. 32_767
     with Size => 16;

   type ACC_MEM_Sample_Type is record
      Real : ACC_MEM_Number_Type;
      Imag : ACC_MEM_Number_Type;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for ACC_MEM_Sample_Type use record
      Real at 0 range 0 .. 15;
      Imag at 2 range 0 .. 15;
   end record;

   type ACC_MEM_CIR_Array is array(Types.Index range <>) of ACC_MEM_Sample_Type;

   type ACC_MEM_Type is record
      CIR : ACC_MEM_CIR_Array (0 .. 1015);
   end record
     with Size => 4064*8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for ACC_MEM_Type use record
      CIR at 0 range 0 .. (4064*8) - 1;
   end record;

   ----------------------------------------------------------------------------
   -- GPIO_CTRL register file

   -----------------------------
   -- GPIO_MODE sub-register  --
   -----------------------------

   type GPIO_MODE_MSGP0_Field is
     (GPIO0,
      RXOKLED,
      Reserved_10,
      Reserved_11)
     with Size => 2;
   --  Mode Selection for GPIO0/RXOKLED.

   type GPIO_MODE_MSGP1_Field is
     (GPIO1,
      SFDLED,
      Reserved_10,
      Reserved_11)
     with Size => 2;
   --  Mode Selection for GPIO1/SFDLED.

   type GPIO_MODE_MSGP2_Field is
     (GPIO2,
      RXLED,
      Reserved_10,
      Reserved_11)
     with Size => 2;
   --  Mode Selection for GPIO2/RXLED

   type GPIO_MODE_MSGP3_Field is
     (GPIO3,
      TXLED,
      Reserved_10,
      Reserved_11)
     with Size => 2;
   --  Mode Selection for GPIO3/TXLED

   type GPIO_MODE_MSGP4_Field is
     (GPIO4,
      EXTPA,
      Reserved_10,
      Reserved_11)
     with Size => 2;
   --  Mode Selection for GPIO4/EXTPA

   type GPIO_MODE_MSGP5_Field is
     (GPIO5,
      EXTTXE,
      Reserved_10,
      Reserved_11)
     with Size => 2;
   --  Mode Selection for GPIO5/EXTTXE

   type GPIO_MODE_MSGP6_Field is
     (GPIO6,
      EXTRXE,
      Reserved_10,
      Reserved_11)
     with Size => 2;
   --  Mode Selection for GPIO6/EXTRXE

   type GPIO_MODE_MSGP7_Field is
     (SYNC,
      GPIO7,
      Reserved_10,
      Reserved_11)
     with Size => 2;
   --  Mode Selection for SYNC/GPIO7

   type GPIO_MODE_MSGP8_Field is
     (IRQ,
      GPIO8,
      Reserved_10,
      Reserved_11)
     with Size => 2;
   --  Mode Selection for IRQ/GPIO8

   type GPIO_MODE_Type is record
      MSGP0 : GPIO_MODE_MSGP0_Field := GPIO0;
      MSGP1 : GPIO_MODE_MSGP1_Field := GPIO1;
      MSGP2 : GPIO_MODE_MSGP2_Field := GPIO2;
      MSGP3 : GPIO_MODE_MSGP3_Field := GPIO3;
      MSGP4 : GPIO_MODE_MSGP4_Field := GPIO4;
      MSGP5 : GPIO_MODE_MSGP5_Field := GPIO5;
      MSGP6 : GPIO_MODE_MSGP6_Field := GPIO6;
      MSGP7 : GPIO_MODE_MSGP7_Field := SYNC;
      MSGP8 : GPIO_MODE_MSGP8_Field := IRQ;

      Reserved_1 : Types.Bits_6 := 0;
      Reserved_2 : Types.Bits_8 := 0;
   end record
   with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for GPIO_MODE_Type use record
      Reserved_1 at 0 range  0 ..  5;

      MSGP0      at 0 range  6 ..  7;
      MSGP1      at 0 range  8 ..  9;
      MSGP2      at 0 range 10 .. 11;
      MSGP3      at 0 range 12 .. 13;
      MSGP4      at 0 range 14 .. 15;
      MSGP5      at 0 range 16 .. 17;
      MSGP6      at 0 range 18 .. 19;
      MSGP7      at 0 range 20 .. 21;
      MSGP8      at 0 range 22 .. 23;

      Reserved_2 at 0 range 24 .. 31;
   end record;

   ----------------------------
   -- GPIO_DIR sub-register  --
   ----------------------------

   type GPIO_DIR_GDP_Field is
     (Output,
      Input)
     with Size => 1;
   --  Direction Selection for GPIOx

   type GPIO_DIR_GDM_Field is
     (Clear,
      Set)
     with Size => 1;
   --  Mask for setting the direction of GPIOx.
   --
   --  When writing to GDP0 so select the I/O direction of GPIOx, the value of
   --  GDPx is only changed if this GDMx mask bit is Set for the write
   --  operation. GDMx will always read as 0.

   type GPIO_DIR_Type is record
      GDP0 : GPIO_DIR_GDP_Field := Input;
      GDP1 : GPIO_DIR_GDP_Field := Input;
      GDP2 : GPIO_DIR_GDP_Field := Input;
      GDP3 : GPIO_DIR_GDP_Field := Input;
      GDM0 : GPIO_DIR_GDM_Field := Clear;
      GDM1 : GPIO_DIR_GDM_Field := Clear;
      GDM2 : GPIO_DIR_GDM_Field := Clear;
      GDM3 : GPIO_DIR_GDM_Field := Clear;
      GDP4 : GPIO_DIR_GDP_Field := Input;
      GDP5 : GPIO_DIR_GDP_Field := Input;
      GDP6 : GPIO_DIR_GDP_Field := Input;
      GDP7 : GPIO_DIR_GDP_Field := Input;
      GDM4 : GPIO_DIR_GDM_Field := Clear;
      GDM5 : GPIO_DIR_GDM_Field := Clear;
      GDM6 : GPIO_DIR_GDM_Field := Clear;
      GDM7 : GPIO_DIR_GDM_Field := Clear;
      GDP8 : GPIO_DIR_GDP_Field := Input;
      GDM8 : GPIO_DIR_GDM_Field := Clear;

      Reserved_1 : Types.Bits_3  := 0;
      Reserved_2 : Types.Bits_11 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for GPIO_DIR_Type use record
      GDP0       at 0 range  0 ..  0;
      GDP1       at 0 range  1 ..  1;
      GDP2       at 0 range  2 ..  2;
      GDP3       at 0 range  3 ..  3;
      GDM0       at 0 range  4 ..  4;
      GDM1       at 0 range  5 ..  5;
      GDM2       at 0 range  6 ..  6;
      GDM3       at 0 range  7 ..  7;
      GDP4       at 0 range  8 ..  8;
      GDP5       at 0 range  9 ..  9;
      GDP6       at 0 range 10 .. 10;
      GDP7       at 0 range 11 .. 11;
      GDM4       at 0 range 12 .. 12;
      GDM5       at 0 range 13 .. 13;
      GDM6       at 0 range 14 .. 14;
      GDM7       at 0 range 15 .. 15;
      GDP8       at 0 range 16 .. 16;

      Reserved_1 at 0 range 17 .. 19;

      GDM8       at 0 range 20 .. 20;

      Reserved_2 at 0 range 21 .. 31;
   end record;

   -----------------------------
   -- GPIO_DOUT sub-register  --
   -----------------------------

   type GPIO_DOUT_GOM_Field is
     (Clear,
      Set)
     with Size => 1;

   type GPIO_DOUT_Type is record
      GOP0 : Types.Bits_1        := 0;
      GOP1 : Types.Bits_1        := 0;
      GOP2 : Types.Bits_1        := 0;
      GOP3 : Types.Bits_1        := 0;
      GOM0 : GPIO_DOUT_GOM_Field := Clear;
      GOM1 : GPIO_DOUT_GOM_Field := Clear;
      GOM2 : GPIO_DOUT_GOM_Field := Clear;
      GOM3 : GPIO_DOUT_GOM_Field := Clear;
      GOP4 : Types.Bits_1        := 0;
      GOP5 : Types.Bits_1        := 0;
      GOP6 : Types.Bits_1        := 0;
      GOP7 : Types.Bits_1        := 0;
      GOM4 : GPIO_DOUT_GOM_Field := Clear;
      GOM5 : GPIO_DOUT_GOM_Field := Clear;
      GOM6 : GPIO_DOUT_GOM_Field := Clear;
      GOM7 : GPIO_DOUT_GOM_Field := Clear;
      GOP8 : Types.Bits_1        := 0;
      GOM8 : GPIO_DOUT_GOM_Field := Clear;

      Reserved_1 : Types.Bits_3  := 0;
      Reserved_2 : Types.Bits_11 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for GPIO_DOUT_Type use record
      GOP0       at 0 range  0 ..  0;
      GOP1       at 0 range  1 ..  1;
      GOP2       at 0 range  2 ..  2;
      GOP3       at 0 range  3 ..  3;
      GOM0       at 0 range  4 ..  4;
      GOM1       at 0 range  5 ..  5;
      GOM2       at 0 range  6 ..  6;
      GOM3       at 0 range  7 ..  7;
      GOP4       at 0 range  8 ..  8;
      GOP5       at 0 range  9 ..  9;
      GOP6       at 0 range 10 .. 10;
      GOP7       at 0 range 11 .. 11;
      GOM4       at 0 range 12 .. 12;
      GOM5       at 0 range 13 .. 13;
      GOM6       at 0 range 14 .. 14;
      GOM7       at 0 range 15 .. 15;
      GOP8       at 0 range 16 .. 16;

      Reserved_1 at 0 range 17 .. 19;

      GOM8       at 0 range 20 .. 20;

      Reserved_2 at 0 range 21 .. 31;
   end record;

   -----------------------------
   -- GPIO_IRQE sub-register  --
   -----------------------------

   type GPIO_IREQ_GIRQE_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  GPIO IRQ Enable for GPIOx input.

   type GPIO_IRQE_Type is record
      GIRQE0 : GPIO_IREQ_GIRQE_Field := Disabled;
      GIRQE1 : GPIO_IREQ_GIRQE_Field := Disabled;
      GIRQE2 : GPIO_IREQ_GIRQE_Field := Disabled;
      GIRQE3 : GPIO_IREQ_GIRQE_Field := Disabled;
      GIRQE4 : GPIO_IREQ_GIRQE_Field := Disabled;
      GIRQE5 : GPIO_IREQ_GIRQE_Field := Disabled;
      GIRQE6 : GPIO_IREQ_GIRQE_Field := Disabled;
      GIRQE7 : GPIO_IREQ_GIRQE_Field := Disabled;
      GIRQE8 : GPIO_IREQ_GIRQE_Field := Disabled;

      Reserved : Types.Bits_23 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for GPIO_IRQE_Type use record
      GIRQE0   at 0 range  0 ..  0;
      GIRQE1   at 0 range  1 ..  1;
      GIRQE2   at 0 range  2 ..  2;
      GIRQE3   at 0 range  3 ..  3;
      GIRQE4   at 0 range  4 ..  4;
      GIRQE5   at 0 range  5 ..  5;
      GIRQE6   at 0 range  6 ..  6;
      GIRQE7   at 0 range  7 ..  7;
      GIRQE8   at 0 range  8 ..  8;

      Reserved at 0 range  9 .. 31;
   end record;

   -----------------------------
   -- GPIO_ISEN sub-register  --
   -----------------------------

   type GPIO_ISEN_GISEN_Field is
     (Active_High,
      Active_Low)
     with Size => 1;
   --  GPIO IRQ Sense selection GPIO0 input.

   type GPIO_ISEN_Type is record
      GISEN0 : GPIO_ISEN_GISEN_Field := Active_High;
      GISEN1 : GPIO_ISEN_GISEN_Field := Active_High;
      GISEN2 : GPIO_ISEN_GISEN_Field := Active_High;
      GISEN3 : GPIO_ISEN_GISEN_Field := Active_High;
      GISEN4 : GPIO_ISEN_GISEN_Field := Active_High;
      GISEN5 : GPIO_ISEN_GISEN_Field := Active_High;
      GISEN6 : GPIO_ISEN_GISEN_Field := Active_High;
      GISEN7 : GPIO_ISEN_GISEN_Field := Active_High;
      GISEN8 : GPIO_ISEN_GISEN_Field := Active_High;

      Reserved : Types.Bits_23 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for GPIO_ISEN_Type use record
      GISEN0   at 0 range  0 ..  0;
      GISEN1   at 0 range  1 ..  1;
      GISEN2   at 0 range  2 ..  2;
      GISEN3   at 0 range  3 ..  3;
      GISEN4   at 0 range  4 ..  4;
      GISEN5   at 0 range  5 ..  5;
      GISEN6   at 0 range  6 ..  6;
      GISEN7   at 0 range  7 ..  7;
      GISEN8   at 0 range  8 ..  8;

      Reserved at 0 range  9 .. 31;
   end record;

   ------------------------------
   -- GPIO_IMODE sub-register  --
   ------------------------------

   type GPIO_IMODE_GIMOD_Field is
     (Level,
      Edge)
     with Size => 1;
   --  GPIO IRQ Mode selection for GPIOx input.

   type GPIO_IMODE_Type is record
      GIMOD0 : GPIO_IMODE_GIMOD_Field := Level;
      GIMOD1 : GPIO_IMODE_GIMOD_Field := Level;
      GIMOD2 : GPIO_IMODE_GIMOD_Field := Level;
      GIMOD3 : GPIO_IMODE_GIMOD_Field := Level;
      GIMOD4 : GPIO_IMODE_GIMOD_Field := Level;
      GIMOD5 : GPIO_IMODE_GIMOD_Field := Level;
      GIMOD6 : GPIO_IMODE_GIMOD_Field := Level;
      GIMOD7 : GPIO_IMODE_GIMOD_Field := Level;
      GIMOD8 : GPIO_IMODE_GIMOD_Field := Level;

      Reserved : Types.Bits_23 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for GPIO_IMODE_Type use record
      GIMOD0   at 0 range  0 ..  0;
      GIMOD1   at 0 range  1 ..  1;
      GIMOD2   at 0 range  2 ..  2;
      GIMOD3   at 0 range  3 ..  3;
      GIMOD4   at 0 range  4 ..  4;
      GIMOD5   at 0 range  5 ..  5;
      GIMOD6   at 0 range  6 ..  6;
      GIMOD7   at 0 range  7 ..  7;
      GIMOD8   at 0 range  8 ..  8;

      Reserved at 0 range  9 .. 31;
   end record;

   -----------------------------
   -- GPIO_IBES sub-register  --
   -----------------------------

   type GPIO_IBES_GIBES_Field is
     (Use_GPIO_IMODE,
      Both_Edges)
     with Size => 1;
   --  GPIO IRQ "Both Edge" selection for GPIOx input.

   type GPIO_IBES_Type is record
      GIBES0 : GPIO_IBES_GIBES_Field := Use_GPIO_IMODE;
      GIBES1 : GPIO_IBES_GIBES_Field := Use_GPIO_IMODE;
      GIBES2 : GPIO_IBES_GIBES_Field := Use_GPIO_IMODE;
      GIBES3 : GPIO_IBES_GIBES_Field := Use_GPIO_IMODE;
      GIBES4 : GPIO_IBES_GIBES_Field := Use_GPIO_IMODE;
      GIBES5 : GPIO_IBES_GIBES_Field := Use_GPIO_IMODE;
      GIBES6 : GPIO_IBES_GIBES_Field := Use_GPIO_IMODE;
      GIBES7 : GPIO_IBES_GIBES_Field := Use_GPIO_IMODE;
      GIBES8 : GPIO_IBES_GIBES_Field := Use_GPIO_IMODE;

      Reserved : Types.Bits_23 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for GPIO_IBES_Type use record
      GIBES0   at 0 range  0 ..  0;
      GIBES1   at 0 range  1 ..  1;
      GIBES2   at 0 range  2 ..  2;
      GIBES3   at 0 range  3 ..  3;
      GIBES4   at 0 range  4 ..  4;
      GIBES5   at 0 range  5 ..  5;
      GIBES6   at 0 range  6 ..  6;
      GIBES7   at 0 range  7 ..  7;
      GIBES8   at 0 range  8 ..  8;

      Reserved at 0 range  9 .. 31;
   end record;

   -----------------------------
   -- GPIO_ICLR sub-register  --
   -----------------------------

   type GPIO_ICLR_GICLR_Field is
     (No_Action,
      Clear_IRQ_Latch)
     with Size => 1;
   --  GPIO IRQ latch clear for GPIOx input.

   type GPIO_ICLR_Type is record
      GICLR0 : GPIO_ICLR_GICLR_Field := No_Action;
      GICLR1 : GPIO_ICLR_GICLR_Field := No_Action;
      GICLR2 : GPIO_ICLR_GICLR_Field := No_Action;
      GICLR3 : GPIO_ICLR_GICLR_Field := No_Action;
      GICLR4 : GPIO_ICLR_GICLR_Field := No_Action;
      GICLR5 : GPIO_ICLR_GICLR_Field := No_Action;
      GICLR6 : GPIO_ICLR_GICLR_Field := No_Action;
      GICLR7 : GPIO_ICLR_GICLR_Field := No_Action;
      GICLR8 : GPIO_ICLR_GICLR_Field := No_Action;

      Reserved : Types.Bits_23 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for GPIO_ICLR_Type use record
      GICLR0   at 0 range  0 ..  0;
      GICLR1   at 0 range  1 ..  1;
      GICLR2   at 0 range  2 ..  2;
      GICLR3   at 0 range  3 ..  3;
      GICLR4   at 0 range  4 ..  4;
      GICLR5   at 0 range  5 ..  5;
      GICLR6   at 0 range  6 ..  6;
      GICLR7   at 0 range  7 ..  7;
      GICLR8   at 0 range  8 ..  8;

      Reserved at 0 range  9 .. 31;
   end record;

   -----------------------------
   -- GPIO_IDBE sub-register  --
   -----------------------------

   type GPIO_IDBE_GIDBE_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  GPIO IRQ de-bounce enable for GPIOx.

   type GPIO_IDBE_Type is record
      GIDBE0 : GPIO_IDBE_GIDBE_Field := Disabled;
      GIDBE1 : GPIO_IDBE_GIDBE_Field := Disabled;
      GIDBE2 : GPIO_IDBE_GIDBE_Field := Disabled;
      GIDBE3 : GPIO_IDBE_GIDBE_Field := Disabled;
      GIDBE4 : GPIO_IDBE_GIDBE_Field := Disabled;
      GIDBE5 : GPIO_IDBE_GIDBE_Field := Disabled;
      GIDBE6 : GPIO_IDBE_GIDBE_Field := Disabled;
      GIDBE7 : GPIO_IDBE_GIDBE_Field := Disabled;
      GIDBE8 : GPIO_IDBE_GIDBE_Field := Disabled;

      Reserved : Types.Bits_23 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for GPIO_IDBE_Type use record
      GIDBE0   at 0 range  0 ..  0;
      GIDBE1   at 0 range  1 ..  1;
      GIDBE2   at 0 range  2 ..  2;
      GIDBE3   at 0 range  3 ..  3;
      GIDBE4   at 0 range  4 ..  4;
      GIDBE5   at 0 range  5 ..  5;
      GIDBE6   at 0 range  6 ..  6;
      GIDBE7   at 0 range  7 ..  7;
      GIDBE8   at 0 range  8 ..  8;

      Reserved at 0 range  9 .. 31;
   end record;

   ----------------------------
   -- GPIO_RAW sub-register  --
   ----------------------------

   type GPIO_RAW_Type is record
      GRAWP0 : Types.Bits_1 := 0;
      GRAWP1 : Types.Bits_1 := 0;
      GRAWP2 : Types.Bits_1 := 0;
      GRAWP3 : Types.Bits_1 := 0;
      GRAWP4 : Types.Bits_1 := 0;
      GRAWP5 : Types.Bits_1 := 0;
      GRAWP6 : Types.Bits_1 := 0;
      GRAWP7 : Types.Bits_1 := 0;
      GRAWP8 : Types.Bits_1 := 0;

      Reserved : Types.Bits_23 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for GPIO_RAW_Type use record
      GRAWP0   at 0 range  0 ..  0;
      GRAWP1   at 0 range  1 ..  1;
      GRAWP2   at 0 range  2 ..  2;
      GRAWP3   at 0 range  3 ..  3;
      GRAWP4   at 0 range  4 ..  4;
      GRAWP5   at 0 range  5 ..  5;
      GRAWP6   at 0 range  6 ..  6;
      GRAWP7   at 0 range  7 ..  7;
      GRAWP8   at 0 range  8 ..  8;

      Reserved at 0 range  9 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- DRX_CONF register file

   ------------------------------
   -- DRX_TUNE0b sub-register  --
   ------------------------------

   type DRX_TUNE0b_Field is new Bits_16;

   DRX_TUNE0b_110K_STD     : constant DRX_TUNE0b_Field := 16#000A#;
   DRX_TUNE0b_110K_Non_STD : constant DRX_TUNE0b_Field := 16#0016#;
   DRX_TUNE0b_850K_STD     : constant DRX_TUNE0b_Field := 16#0001#;
   DRX_TUNE0b_850K_Non_STD : constant DRX_TUNE0b_Field := 16#0006#;
   DRX_TUNE0b_6M8_STD      : constant DRX_TUNE0b_Field := 16#0001#;
   DRX_TUNE0b_6M8_Non_STD  : constant DRX_TUNE0b_Field := 16#0002#;

   type DRX_TUNE0b_Type is record
      DRX_TUNE0b : DRX_TUNE0b_Field;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for DRX_TUNE0b_Type use record
      DRX_TUNE0b at 0 range 0 .. 15;
   end record;

   ------------------------------
   -- DRX_TUNE1a sub-register  --
   ------------------------------

   type DRX_TUNE1a_Field is new Bits_16;

   DRX_TUNE1a_16MHz : constant DRX_TUNE1a_Field := 16#0087#;
   DRX_TUNE1a_64MHz : constant DRX_TUNE1a_Field := 16#008D#;

   type DRX_TUNE1a_Type is record
      DRX_TUNE1a : DRX_TUNE1a_Field;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for DRX_TUNE1a_Type use record
      DRX_TUNE1a at 0 range 0 .. 15;
   end record;

   ------------------------------
   -- DRX_TUNE1b sub-register  --
   ------------------------------

   type DRX_TUNE1b_Field is new Bits_16;

   DRX_TUNE1b_110K     : constant DRX_TUNE1b_Field := 16#0064#;
   --  Preamble lengths > 1024 symbols, for 110 kbps operation

   DRX_TUNE1b_850K_6M8 : constant DRX_TUNE1b_Field := 16#0020#;
   -- Preamble lengths 128 to 1024 symbols, for 850 kbps and 6.8 Mbps operation

   DRX_TUNE1b_6M8      : constant DRX_TUNE1b_Field := 16#0010#;
   --  Preamble length = 64 symbols, for 6.8 Mbps operation

   type DRX_TUNE1b_Type is record
      DRX_TUNE1b : DRX_TUNE1b_Field;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for DRX_TUNE1b_Type use record
      DRX_TUNE1b at 0 range 0 .. 15;
   end record;

   ------------------------------
   -- DRX_TUNE2 sub-register  --
   ------------------------------

   type DRX_TUNE2_Field is new Bits_32;

   DRX_TUNE2_PAC8_16MHz  : constant DRX_TUNE2_Field := 16#311A002D#;
   DRX_TUNE2_PAC8_64MHz  : constant DRX_TUNE2_Field := 16#313B006B#;
   DRX_TUNE2_PAC16_16MHz : constant DRX_TUNE2_Field := 16#331A0052#;
   DRX_TUNE2_PAC16_64MHz : constant DRX_TUNE2_Field := 16#333B00BE#;
   DRX_TUNE2_PAC32_16MHz : constant DRX_TUNE2_Field := 16#351A009A#;
   DRX_TUNE2_PAC32_64MHz : constant DRX_TUNE2_Field := 16#353B015E#;
   DRX_TUNE2_PAC64_16MHz : constant DRX_TUNE2_Field := 16#371A011D#;
   DRX_TUNE2_PAC64_64MHz : constant DRX_TUNE2_Field := 16#373B0296#;

   type DRX_TUNE2_Type is record
      DRX_TUNE2 : DRX_TUNE2_Field;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for DRX_TUNE2_Type use record
      DRX_TUNE2 at 0 range 0 .. 31;
   end record;

   ------------------------------
   -- DRX_SFDTOC sub-register  --
   ------------------------------

   type DRX_SFDTOC_Field is range 0 .. 2**16 - 1
     with Size => 16;
   --  SFD detection timeout count (in units of preamble symbols)

   type DRX_SFDTOC_Type is record
      DRX_SFDTOC : DRX_SFDTOC_Field;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for DRX_SFDTOC_Type use record
      DRX_SFDTOC at 0 range 0 .. 15;
   end record;

   ------------------------------
   -- DRX_PRETOC sub-register  --
   ------------------------------

   type DRX_PRETOC_Field is range 0 .. 2**16 - 1
     with Size => 16;
   --  Preamble detection timeout count (in units of PAC size symbols)

   type DRX_PRETOC_Type is record
      DRX_PRETOC : DRX_PRETOC_Field;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for DRX_PRETOC_Type use record
      DRX_PRETOC at 0 range 0 .. 15;
   end record;

   ------------------------------
   -- DRX_TUNE4H sub-register  --
   ------------------------------

   type DRX_TUNE4H_Field is new Bits_16;

   DRX_TUNE4H_Preamble_64 : constant DRX_TUNE4H_Field := 16#0010#; --  64
   DRX_TUNE4H_Others      : constant DRX_TUNE4H_Field := 16#0028#; --  128 or greater

   type DRX_TUNE4H_Type is record
      DRX_TUNE4H : DRX_TUNE4H_Field;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for DRX_TUNE4H_Type use record
      DRX_TUNE4H at 0 range 0 .. 15;
   end record;

   --------------------------------
   -- RXPACC_NOSAT sub-register  --
   --------------------------------

   type RXPACC_NOSAT_Field is range 0 .. 2**16 - 1
     with Size => 16;

   type RXPACC_NOSAT_Type is record
      RXPACC_NOSAT : RXPACC_NOSAT_Field;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RXPACC_NOSAT_Type use record
      RXPACC_NOSAT at 0 range 0 .. 15;
   end record;

   ----------------------------------------------------------------------------
   -- RF_CONF register file

   ---------------------------
   -- RF_CONF sub-register  --
   ---------------------------

   type RF_CONF_TXFEN_Field is new Bits_5;
   --  Transmit block force enable.

   RF_CONF_TXFEN_Force_Enable : constant RF_CONF_TXFEN_Field := 16#1F#;

   type RF_CONF_PLLFEN_Field is new Bits_3;
   --  PLL block force enables.

   RF_CONF_PLLFEN_Force_Enable : constant RF_CONF_PLLFEN_Field := 16#5#;

   type RF_CONF_LDOFEN_Field is new Bits_5;
   --  Write 0x1F to force the enable to all LDOs.

   RF_CONF_LDOFEN_Force_Enable : constant RF_CONF_LDOFEN_Field := 16#1F#;

   type RF_CONF_TXRXSW_Field is new Bits_2;

   RF_CONF_TXRXSW_Force_Tx : constant RF_CONF_TXRXSW_Field := 16#2#;
   RF_CONF_TXRXSW_Force_Rx : constant RF_CONF_TXRXSW_Field := 16#1#;

   type RF_CONF_Type is record
      TXFEN  : RF_CONF_TXFEN_Field  := 0;
      PLLFEN : RF_CONF_PLLFEN_Field := 0;
      LDOFEN : RF_CONF_LDOFEN_Field := 0;
      TXRXSW : RF_CONF_TXRXSW_Field := 0;

      Reserved_1 : Types.Bits_8 := 0;
      Reserved_2 : Types.Bits_9 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RF_CONF_Type use record
      Reserved_1 at 0 range  0 ..  7;

      TXFEN      at 0 range  8 .. 12;
      PLLFEN     at 0 range 13 .. 15;
      LDOFEN     at 0 range 16 .. 20;
      TXRXSW     at 0 range 21 .. 22;

      Reserved_2 at 0 range 23 .. 31;
   end record;

   ------------------------------
   -- RF_RXCTRLH sub-register  --
   ------------------------------

   type RF_RXCTRLH_Field is new Bits_8;

   RF_RXCTRLH_500MHz : constant RF_RXCTRLH_Field := 16#D8#; --  Channels 1,2,3,5
   RF_RXCTRLH_900MHz : constant RF_RXCTRLH_Field := 16#BC#; --  Channels 4,7

   type RF_RXCTRLH_Type is record
      RF_RXCTRLH : RF_RXCTRLH_Field;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RF_RXCTRLH_Type use record
      RF_RXCTRLH at 0 range 0 .. 7;
   end record;

   -----------------------------
   -- RF_TXCTRL sub-register  --
   -----------------------------

   type RF_TXCTRL_Field is new Bits_32;

   RF_TXCTRL_Channel_1 : constant RF_TXCTRL_Field := 16#00005C40#;
   RF_TXCTRL_Channel_2 : constant RF_TXCTRL_Field := 16#00045CA0#;
   RF_TXCTRL_Channel_3 : constant RF_TXCTRL_Field := 16#00086CC0#;
   RF_TXCTRL_Channel_4 : constant RF_TXCTRL_Field := 16#00045C80#;
   RF_TXCTRL_Channel_5 : constant RF_TXCTRL_Field := 16#001E3FE3#;
   RF_TXCTRL_Channel_7 : constant RF_TXCTRL_Field := 16#001E7DE0#;

   type RF_TXCTRL_Type is record
      RF_TXCTRL : RF_TXCTRL_Field;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RF_TXCTRL_Type use record
      RF_TXCTRL at 0 range 0 .. 31;
   end record;

   -----------------------------
   -- RF_STATUS sub-register  --
   -----------------------------

   type RF_STATUS_CPLLLOCK_Field is
     (Not_Locked,
      Locked)
     with Size => 1;
   --  Clock PLL Lock status.

   type RF_STATUS_CPLLLOW_Field is
     (Not_Low,
      Low)
     with Size => 1;
   --  Clock PLL Low flag status.

   type RF_STATUS_CPLLHIGH_Field is
     (Not_High,
      High)
     with Size => 1;
   --  Clock PLL High flag status.

   type RF_STATUS_RFPLLLOCK_Field is
     (Not_Locked,
      Locked)
     with Size => 1;
   --  RF PLL Lock status.

   type RF_STATUS_Type is record
      CPLLLOCK  : RF_STATUS_CPLLLOCK_Field  := Not_Locked;
      CPLLLOW   : RF_STATUS_CPLLLOW_Field   := Not_Low;
      CPLLHIGH  : RF_STATUS_CPLLHIGH_Field  := Not_High;
      RFPLLLOCK : RF_STATUS_RFPLLLOCK_Field := Not_Locked;

      Reserved  : Types.Bits_4 := 0;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for RF_STATUS_Type use record
      CPLLLOCK  at 0 range 0 .. 0;
      CPLLLOW   at 0 range 1 .. 1;
      CPLLHIGH  at 0 range 2 .. 2;
      RFPLLLOCK at 0 range 3 .. 3;

      Reserved  at 0 range 4 .. 7;
   end record;

   ---------------------------
   -- LDOTUNE sub-register  --
   ---------------------------

   type LDOTUNE_Field is new Bits_40;

   type LDOTUNE_Type is Record
      LDOTUNE : LDOTUNE_Field := 16#88_8888_8888#;
   end record
     with Size => 40,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for LDOTUNE_Type use record
      LDOTUNE at 0 range 0 .. 39;
   end record;

   ----------------------------------------------------------------------------
   -- TX_CAL register file

   ---------------------------
   -- TC_SARC sub-register  --
   ---------------------------

   type TC_SARC_SAR_CTRL_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Enable or disable the SAR

   type TC_SARC_Type is record
      SAR_CTRL : TC_SARC_SAR_CTRL_Field := Disabled;

      Reserved : Types.Bits_15 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TC_SARC_Type use record
      SAR_CTRL at 0 range 0 .. 0;

      Reserved at 0 range 1 .. 15;
   end record;

   ---------------------------
   -- TC_SARL sub-register  --
   ---------------------------

   type TC_SARL_SAR_LVBAT_Field is range 0 .. 255
     with Size => 8;
   --  Latest SAR reading for Voltage level.

   type TC_SARL_SAR_LTEMP_Field is range 0 .. 255
     with Size => 8;
   --  Latest SAR reading for Temperature level.

   type TC_SARL_Type is record
      SAR_LVBAT : TC_SARL_SAR_LVBAT_Field := 0;
      SAR_LTEMP : TC_SARL_SAR_LTEMP_Field := 0;

      Reserved  : Types.Bits_8 := 0;
   end record
     with Size => 24,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TC_SARL_Type use record
      SAR_LVBAT at 0 range  0 ..  7;
      SAR_LTEMP at 0 range  8 .. 15;

      Reserved  at 0 range 16 .. 23;
   end record;

   ---------------------------
   -- TC_SARW sub-register  --
   ---------------------------

   type TC_SARW_WVBAT_Field is range 0 .. 255
     with Size => 8;
   --  SAR reading of Voltage level taken at last wakeup event.

   type TC_SARW_WTEMP_Field is range 0 .. 255
     with Size => 8;
   --  SAR reading of temperature level taken at last wakeup event.

   type TC_SARW_Type is record
      SAR_WVBAT : TC_SARW_WVBAT_Field := 0;
      SAR_WTEMP : TC_SARW_WTEMP_Field := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TC_SARW_Type use record
      SAR_WVBAT at 0 range 0 ..  7;
      SAR_WTEMP at 0 range 8 .. 15;
   end record;

   -------------------------------
   --  TC_PG_CTRL sub-register  --
   -------------------------------

   type TC_PG_CTRL_PG_START_Field is
     (No_Action,
      Start)
     with Size => 1;
   --  Start the pulse generator calibration.

   type TC_PG_CTRL_PG_TMEAS_Field is range 0 .. 15
     with Size => 4;
   --  Number of clock cycles over which to run the pulse generator cal counter.

   type TC_PG_CTRL_Type is record
      PG_START : TC_PG_CTRL_PG_START_Field := No_Action;
      PG_TMEAS : TC_PG_CTRL_PG_TMEAS_Field := 0;

      Reserved_1 : Bits_1;
      Reserved_2 : Bits_2;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TC_PG_CTRL_Type use record
      PG_START   at 0 range 0 .. 0;

      Reserved_1 at 0 range 1 .. 1;

      PG_TMEAS   at 0 range 2 .. 5;

      Reserved_2 at 0 range 6 .. 7;
   end record;

   ---------------------------------
   --  TC_PG_STATUS sub-register  --
   ---------------------------------

   type TC_PG_STATUS_DELAY_CNT_Field is range 0 .. 2**12 - 1
     with Size => 12;
   --  Reference value required for temperature bandwidth compensation

   type TC_PG_STATUS_Type is record
      DELAY_CNT : TC_PG_STATUS_DELAY_CNT_Field := 0;

      Reserved : Bits_4;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TC_PG_STATUS_Type use record
      DELAY_CNT at 0 range 0 .. 11;

      Reserved  at 0 range 12 .. 15;
   end record;

   ------------------------------
   -- TC_PGDELAY sub-register  --
   ------------------------------

   type TC_PGDELAY_Field is new Bits_8;
   --  8-bit configuration register for setting the Pulse Generator Delay value.

   TC_PGDELAY_Channel_1 : constant TC_PGDELAY_Field := 16#C9#;
   TC_PGDELAY_Channel_2 : constant TC_PGDELAY_Field := 16#C2#;
   TC_PGDELAY_Channel_3 : constant TC_PGDELAY_Field := 16#C5#;
   TC_PGDELAY_Channel_4 : constant TC_PGDELAY_Field := 16#95#;
   TC_PGDELAY_Channel_5 : constant TC_PGDELAY_Field := 16#B5#;
   TC_PGDELAY_Channel_7 : constant TC_PGDELAY_Field := 16#93#;

   type TC_PGDELAY_Type is record
      TC_PGDELAY : TC_PGDELAY_Field;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TC_PGDELAY_Type use record
      TC_PGDELAY at 0 range 0 .. 7;
   end record;

   -----------------------------
   -- TC_PGTEST sub-register  --
   -----------------------------

   type TC_PGTEST_Field is new Bits_8;
   --  8-bit configuration register for use in setting the transmitter into
   --  continuous wave (CW) mode.

   TC_PGTEST_Normal_Operation : constant TC_PGTEST_Field := 16#00#;
   TC_PGTEST_Continuous_Wave  : constant TC_PGTEST_Field := 16#13#;

   type TC_PGTEST_Type is record
      TC_PGTEST : TC_PGTEST_Field := TC_PGTEST_Normal_Operation;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for TC_PGTEST_Type use record
      TC_PGTEST at 0 range 0 .. 7;
   end record;

   ----------------------------------------------------------------------------
   -- FS_CTRL register file

   -----------------------------
   -- FS_PLLCFG sub-register  --
   -----------------------------

   type FS_PLLCFG_Field is new Bits_32;

   FS_PLLCFG_Channel_1 : constant FS_PLLCFG_Field := 16#09000407#;
   FS_PLLCFG_Channel_2 : constant FS_PLLCFG_Field := 16#08400508#;
   FS_PLLCFG_Channel_3 : constant FS_PLLCFG_Field := 16#08401009#;
   FS_PLLCFG_Channel_4 : constant FS_PLLCFG_Field := 16#08400508#;
   FS_PLLCFG_Channel_5 : constant FS_PLLCFG_Field := 16#0800041D#;
   FS_PLLCFG_Channel_7 : constant FS_PLLCFG_Field := 16#0800041D#;

   type FS_PLLCFG_Type is record
      FS_PLLCFG : FS_PLLCFG_Field;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for FS_PLLCFG_Type use record
      FS_PLLCFG at 0 range 0 .. 31;
   end record;

   ------------------------------
   -- FS_PLLTUNE sub-register  --
   ------------------------------

   type FS_PLLTUNE_Field is new Bits_8;

   FS_PLLTUNE_Channel_1 : constant FS_PLLTUNE_Field := 16#1E#;
   FS_PLLTUNE_Channel_2 : constant FS_PLLTUNE_Field := 16#26#;
   FS_PLLTUNE_Channel_3 : constant FS_PLLTUNE_Field := 16#56#;
   FS_PLLTUNE_Channel_4 : constant FS_PLLTUNE_Field := 16#26#;
   FS_PLLTUNE_Channel_5 : constant FS_PLLTUNE_Field := 16#BE#;
   FS_PLLTUNE_Channel_7 : constant FS_PLLTUNE_Field := 16#BE#;

   type FS_PLLTUNE_Type is record
      FS_PLLTUNE : FS_PLLTUNE_Field;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for FS_PLLTUNE_Type use record
      FS_PLLTUNE at 0 range 0 .. 7;
   end record;

   ----------------------------
   -- FS_XTALT sub-register  --
   ----------------------------

   type FS_XTALT_Field is range 0 .. 2**5 - 1
     with Size => 5;
   --  Crystal Trim.

   type FS_XTALT_Type is record
      XTALT    : FS_XTALT_Field := 0;

      Reserved : Types.Bits_3 := 2#011#;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for FS_XTALT_Type use record
      XTALT    at 0 range 0 .. 4;

      Reserved at 0 range 5 .. 7;
   end record;

   ----------------------------------------------------------------------------
   -- AON register file

   ----------------------------
   -- AON_WCFG sub-register  --
   ----------------------------

   type AON_WCFG_ONW_RADC_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  On Wake-up Run the (temperature and voltage) Analog-to-Digital Convertors.

   type AON_WCFG_ONW_RX_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  On Wake-up turn on the Receiver.

   type AON_WCFG_ONW_LEUI_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  On Wake-up load the EUI from OTP memory into the EUI register.

   type AON_WCFG_ONW_LDC_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  On Wake-upload configurations from the AON memory into the host
   --  interface register set.

   type AON_WCFG_ONW_L64_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  On Wake-up load the Length64 receiver operating parameter set.

   type AON_WCFG_PRES_SLEEP_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Preserve Sleep.

   type AON_WCFG_ONW_LLDE_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  On Wake-up load the LDE microcode.

   type AON_WCFG_ONW_LLD0_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  On Wake-up load the LDOTUNE value from OTP.

   type AON_WCFG_Type is record
      ONW_RADC   : AON_WCFG_ONW_RADC_Field   := Disabled;
      ONW_RX     : AON_WCFG_ONW_RX_Field     := Disabled;
      ONW_LEUI   : AON_WCFG_ONW_LEUI_Field   := Disabled;
      ONW_LDC    : AON_WCFG_ONW_LDC_Field    := Disabled;
      ONW_L64    : AON_WCFG_ONW_L64_Field    := Disabled;
      PRES_SLEEP : AON_WCFG_PRES_SLEEP_Field := Disabled;
      ONW_LLDE   : AON_WCFG_ONW_LLDE_Field   := Disabled;
      ONW_LLD0   : AON_WCFG_ONW_LLD0_Field   := Disabled;

      Reserved_1 : Types.Bits_1 := 0;
      Reserved_2 : Types.Bits_2 := 0;
      Reserved_3 : Types.Bits_2 := 0;
      Reserved_4 : Types.Bits_3 := 0;
   end Record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AON_WCFG_Type use record
      ONW_RADC   at 0 range  0 ..  0;
      ONW_RX     at 0 range  1 ..  1;

      Reserved_1 at 0 range  2 .. 2;

      ONW_LEUI   at 0 range  3 ..  3;

      Reserved_2 at 0 range  4 ..  5;

      ONW_LDC    at 0 range  6 ..  6;
      ONW_L64    at 0 range  7 ..  7;
      PRES_SLEEP at 0 range  8 ..  8;

      Reserved_3 at 0 range  9 .. 10;

      ONW_LLDE   at 0 range 11 .. 11;
      ONW_LLD0   at 0 range 12 .. 12;

      Reserved_4 at 0 range 13 .. 15;
   end record;

   ----------------------------
   -- AON_CTRL sub-register  --
   ----------------------------

   type AON_CTRL_RESTORE_Field is
     (No_Action,
      Restore)
     with Size => 1;
   --  When this bit is set the DW1000 will copy the user configurations from
   --  the AON memory to the host interface register set.

   type AON_CTRL_SAVE_Field is
     (No_Action,
      Save)
     with Size => 1;
   --  When this bit is set the DW1000 will copy the user configurations from
   --  the host interface register set into the AON memory.

   type AON_CTRL_UPL_CFG_Field is
     (No_Action,
      Upload)
     with Size => 1;
   --  Upload the AON block configurations to the AON.

   type AON_CTRL_DCA_READ_Field is
     (No_Action,
      Trigger_Read)
     with Size => 1;
   --  Direct AON memory access read.

   type AON_CTRL_DCA_ENAB_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Direct AON memory access enable bit.

   type AON_CTRL_Type is record
      RESTORE  : AON_CTRL_RESTORE_Field  := No_Action;
      SAVE     : AON_CTRL_SAVE_Field     := No_Action;
      UPL_CFG  : AON_CTRL_UPL_CFG_Field  := No_Action;
      DCA_READ : AON_CTRL_DCA_READ_Field := No_Action;
      DCA_ENAB : AON_CTRL_DCA_ENAB_Field := Disabled;

      Reserved : Types.Bits_3 := 0;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AON_CTRL_Type use record
      RESTORE  at 0 range 0 .. 0;
      SAVE     at 0 range 1 .. 1;
      UPL_CFG  at 0 range 2 .. 2;
      DCA_READ at 0 range 3 .. 3;

      Reserved at 0 range 4 .. 6;

      DCA_ENAB at 0 range 7 .. 7;
   end record;

   ----------------------------
   -- AON_RDAT sub-register  --
   ----------------------------

   type AON_RDAT_Type is record
      AON_RDAT : Types.Bits_8;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AON_RDAT_Type use record
      AON_RDAT at 0 range 0 .. 7;
   end record;

   ----------------------------
   -- AON_ADDR sub-register  --
   ----------------------------

   type AON_ADDR_Field is new Bits_8;

   type AON_ADDR_Type is record
      AON_ADDR : AON_ADDR_Field;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AON_ADDR_Type use record
      AON_ADDR at 0 range 0 .. 7;
   end record;

   ----------------------------
   -- AON_CFG0 sub-register  --
   ----------------------------

   type AON_CFG0_SLEEP_EN_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  This is the sleep enable configuration bit.

   type AON_CFG0_WAKE_PIN_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Wake using WAKEUP pin.

   type AON_CFG0_WAKE_SPI_Pin_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Wake using SPI access.

   type AON_CFG0_WAKE_CNT_Pin_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Wake when sleep counter elapses.

   type AON_CFG0_LPDIV_EN_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  Low power divider enable configuration.

   type AON_CFG0_LPCLKDIVA_Field is range 0 .. 2**11 - 1
     with Size => 11;
   --  This field specifies a divider count for dividing the raw DW1000 XTAL
   --  oscillator frequency to set an LP clock frequency.

   type AON_CFG0_SLEEP_TIM_Field is range 0 .. 2**16 - 1
     with Size => 16;
   --  Sleep time.

   type AON_CFG0_Type is record
      SLEEP_EN  : AON_CFG0_SLEEP_EN_Field     := Disabled;
      WAKE_PIN  : AON_CFG0_WAKE_PIN_Field     := Enabled;
      WAKE_SPI  : AON_CFG0_WAKE_SPI_Pin_Field := Enabled;
      WAKE_CNT  : AON_CFG0_WAKE_CNT_Pin_Field := Enabled;
      LPDIV_EN  : AON_CFG0_LPDIV_EN_Field     := Disabled;
      LPCLKDIVA : AON_CFG0_LPCLKDIVA_Field    := 255;
      SLEEP_TIM : AON_CFG0_SLEEP_TIM_Field    := 20735;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AON_CFG0_Type use record
      SLEEP_EN  at 0 range  0 ..  0;
      WAKE_PIN  at 0 range  1 ..  1;
      WAKE_SPI  at 0 range  2 ..  2;
      WAKE_CNT  at 0 range  3 ..  3;
      LPDIV_EN  at 0 range  4 ..  4;
      LPCLKDIVA at 0 range  5 .. 15;
      SLEEP_TIM at 0 range 16 .. 31;
   end record;

   ----------------------------
   -- AON_CFG1 sub-register  --
   ----------------------------

   type AON_CFG1_SLEEP_CE_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  This bit enables the sleep counter.

   type AON_CFG1_SMXX_Field is
     (Clear,
      Set)
     with Size => 1;
   --  This bit needs to be Cleared for correct operation in the SLEEP state
   --  within the DW1000. By default this bit is Set.

   type AON_CFG1_LPOSC_C_Field is
     (Disabled,
      Enabled)
     with Size => 1;
   --  This bit enables the calibration function that measures the period of the
   --  IC's internal low powered oscillator

   type AON_CFG1_Type is record
      SLEEP_CE : AON_CFG1_SLEEP_CE_Field := Enabled;
      SMXX     : AON_CFG1_SMXX_Field     := Set;
      LPOSC_C  : AON_CFG1_LPOSC_C_Field  := Enabled;

      Reserved : Types.Bits_13 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for AON_CFG1_Type use record
      SLEEP_CE at 0 range 0 ..  0;
      SMXX     at 0 range 1 ..  1;
      LPOSC_C  at 0 range 2 ..  2;

      Reserved at 0 range 3 .. 15;
   end record;

   ----------------------------------------------------------------------------
   -- OTP_IF register file

   -- OTP_WDAT sub-register
   type OTP_WDAT_Type is record
      OTP_WDAT : Types.Bits_32;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for OTP_WDAT_Type use record
      OTP_WDAT at 0 range 0 .. 31;
   end record;

   -- OTP_ADDR sub-register
   type OTP_ADDR_Type is record
      OTP_ADDR : Types.Bits_11 := 0;

      Reserved : Types.Bits_5  := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for OTP_ADDR_Type use record
      OTP_ADDR at 0 range 0 .. 10;

      Reserved at 0 range 11 .. 15;
   end record;

   -- OTP_CTRL sub-register
   type OTP_CTRL_Type is record
      OTPRDEN : Types.Bits_1 := 0;
      OTPREAD : Types.Bits_1 := 0;
      OTPMRWR : Types.Bits_1 := 0;
      OTPPROG : Types.Bits_1 := 0;
      OTPMR   : Types.Bits_4 := 0;
      LDELOAD : Types.Bits_1 := 0;

      Reserved_1 : Types.Bits_1 := 0;
      Reserved_2 : Types.Bits_2 := 0;
      Reserved_3 : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for OTP_CTRL_Type use record
      OTPRDEN    at 0 range  0 ..  0;
      OTPREAD    at 0 range  1 ..  1;

      Reserved_1 at 0 range  2 ..  2;

      OTPMRWR    at 0 range  3 ..  3;

      Reserved_2 at 0 range  4 ..  5;

      OTPPROG    at 0 range  6 ..  6;
      OTPMR      at 0 range  7 .. 10;

      Reserved_3 at 0 range 11 .. 14;

      LDELOAD    at 0 range 15 .. 15;
   end record;

   -- OTP_STAT sub-register
   type OTP_STAT_Type is record
      OTPPRGD  : Types.Bits_1 := 0;
      OTPVPOK  : Types.Bits_1 := 0;

      Reserved : Types.Bits_14 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for OTP_STAT_Type use record
      OTPPRGD  at 0 range 0 .. 0;
      OTPVPOK  at 0 range 1 .. 1;

      Reserved at 0 range 2 .. 15;
   end record;

   -- OTP_RDAT sub-register
   type OTP_RDAT_Type is record
      OTP_RDAT : Types.Bits_32;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for OTP_RDAT_Type use record
      OTP_RDAT at 0 range 0 .. 31;
   end record;

   -- OTP_SRDAT sub-register
   type OTP_SRDAT_Type is record
      OTP_SRDAT : Types.Bits_32;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for OTP_SRDAT_Type use record
      OTP_SRDAT at 0 range 0 .. 31;
   end record;

   -- OTP_SF sub-register
   type OTP_SF_Type is record
      OPS_KICK : Types.Bits_1 := 0;
      LDO_KICK : Types.Bits_1 := 0;
      OPS_SEL  : Types.Bits_1 := 0;

      Reserved_1 : Types.Bits_3 := 0;
      Reserved_2 : Types.Bits_2 := 0;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for OTP_SF_Type use record
      OPS_KICK   at 0 range 0 .. 0;
      LDO_KICK   at 0 range 1 .. 1;

      Reserved_1 at 0 range 2 .. 4;

      OPS_SEL    at 0 range 5 .. 5;

      Reserved_2 at 0 range 6 .. 7;
   end record;

   ----------------------------------------------------------------------------
   -- LDE_IF register file

   -- LDE_THRESH sub-register
   type LDE_THRESH_Type is record
      LDE_THRESH : Types.Bits_16;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for LDE_THRESH_Type use record
      LDE_THRESH at 0 range 0 .. 15;
   end record;

   -- LDE_CFG1 sub-register
   type LDE_CFG1_Type is record
      NTM   : Types.Bits_5 := 2#0_1100#;
      PMULT : Types.Bits_3 := 2#011#;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for LDE_CFG1_Type use record
      NTM   at 0 range 0 .. 4;
      PMULT at 0 range 5 .. 7;
   end record;

   -- LDE_PPINDX sub-register
   type LDE_PPINDX_Type is record
      LDE_PPINDX : Types.Bits_16;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for LDE_PPINDX_Type use record
      LDE_PPINDX at 0 range 0 .. 15;
   end record;

   -- LDE_PPAMPL sub-register
   type LDE_PPAMPL_Type is record
      LDE_PPAMPL : Types.Bits_16;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for LDE_PPAMPL_Type use record
      LDE_PPAMPL at 0 range 0 .. 15;
   end record;

   -- LDE_RXANTD sub-register
   type LDE_RXANTD_Type is record
      LDE_RXANTD : Types.Bits_16;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for LDE_RXANTD_Type use record
      LDE_RXANTD at 0 range 0 .. 15;
   end record;

   -- LDE_CFG2 sub-register
   type LDE_CFG2_Type is record
      LDE_CFG2 : Types.Bits_16;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for LDE_CFG2_Type use record
      LDE_CFG2 at 0 range 0 .. 15;
   end record;

   -- LDE_REPC sub-register
   type LDE_REPC_Type is record
      LDE_REPC : Types.Bits_16;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for LDE_REPC_Type use record
      LDE_REPC at 0 range 0 .. 15;
   end record;

   ----------------------------------------------------------------------------
   -- DIG_DIAG register file

   -- EVC_CTRL sub-register
   type EVC_CTRL_Type is record
      EVC_EN  : Types.Bits_1   := 0;
      EVC_CLR : Types.Bits_1   := 0;

      Reserved : Types.Bits_30 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_CTRL_Type use record
      EVC_EN   at 0 range 0 .. 0;
      EVC_CLR  at 0 range 1 .. 1;

      Reserved at 0 range 2 .. 31;
   end record;

   -- EVC_PHE sub-register
   type EVC_PHE_Type is record
      EVC_PHE : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_PHE_Type use record
      EVC_PHE  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_RSE sub-register
   type EVC_RSE_Type is record
      EVC_RSE : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_RSE_Type use record
      EVC_RSE  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_FCG sub-register
   type EVC_FCG_Type is record
      EVC_FCG : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_FCG_Type use record
      EVC_FCG  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_FCE sub-register
   type EVC_FCE_Type is record
      EVC_FCE : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_FCE_Type use record
      EVC_FCE  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_FFR sub-register
   type EVC_FFR_Type is record
      EVC_FFR : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_FFR_Type use record
      EVC_FFR  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_OVR sub-register
   type EVC_OVR_Type is record
      EVC_OVR : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_OVR_Type use record
      EVC_OVR  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_STO sub-register
   type EVC_STO_Type is record
      EVC_STO : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_STO_Type use record
      EVC_STO  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_PTO sub-register
   type EVC_PTO_Type is record
      EVC_PTO : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_PTO_Type use record
      EVC_PTO  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_FWTO sub-register
   type EVC_FWTO_Type is record
      EVC_FWTO : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_FWTO_Type use record
      EVC_FWTO  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_TXFS sub-register
   type EVC_TXFS_Type is record
      EVC_TXFS : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_TXFS_Type use record
      EVC_TXFS  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_HPW sub-register
   type EVC_HPW_Type is record
      EVC_HPW : Types.Bits_12 := 0;

      Reserved : Types.Bits_4 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_HPW_Type use record
      EVC_HPW  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- EVC_TPW sub-register
   type EVC_TPW_Type is record
      EVC_TPW : Types.Bits_12;

      Reserved : Types.Bits_4;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for EVC_TPW_Type use record
      EVC_TPW  at 0 range 0 .. 11;

      Reserved at 0 range 12 .. 15;
   end record;

   -- DIAG_TMC sub-register
   type DIAG_TMC_Type is record
      TX_PSTM : Types.Bits_1 := 0;

      Reserved_1 : Types.Bits_4  := 0;
      Reserved_2 : Types.Bits_11 := 0;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for DIAG_TMC_Type use record
      Reserved_1 at 0 range 0 ..  3;

      TX_PSTM    at 0 range 4 ..  4;

      Reserved_2 at 0 range 5 .. 15;
   end record;

   ----------------------------------------------------------------------------
   -- PMSC register file

   type PMSC_CTRL0_Type is record
      SYSCLKS   : Types.Bits_2 := 0;
      RXCLKS    : Types.Bits_2 := 0;
      TXCLKS    : Types.Bits_2 := 0;
      FACE      : Types.Bits_1 := 0;
      ADCCE     : Types.Bits_1 := 0;
      AMCE      : Types.Bits_1 := 0;
      GPCE      : Types.Bits_1 := 0;
      GPRN      : Types.Bits_1 := 0;
      GPDCE     : Types.Bits_1 := 0;
      GPDRN     : Types.Bits_1 := 0;
      KHZCLKEN  : Types.Bits_1 := 0;
      SOFTRESET : Types.Bits_4 := 2#1111#;

      Reserved_1 : Types.Bits_3 := 2#100#;
      Reserved_2 : Types.Bits_4 := 0;
      Reserved_3 : Types.Bits_3 := 2#011#;
      Reserved_4 : Types.Bits_4 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for PMSC_CTRL0_Type use record
      SYSCLKS    at 0 range  0 ..  1;
      RXCLKS     at 0 range  2 ..  3;
      TXCLKS     at 0 range  4 ..  5;
      FACE       at 0 range  6 ..  6;

      Reserved_1 at 0 range  7 ..  9;

      ADCCE      at 0 range 10 .. 10;

      Reserved_2 at 0 range 11 .. 14;

      AMCE       at 0 range 15 .. 15;
      GPCE       at 0 range 16 .. 16;
      GPRN       at 0 range 17 .. 17;
      GPDCE      at 0 range 18 .. 18;
      GPDRN      at 0 range 19 .. 19;

      Reserved_3 at 0 range 20 .. 22;

      KHZCLKEN   at 0 range 23 .. 23;

      Reserved_4 at 0 range 24 .. 27;

      SOFTRESET  at 0 range 28 .. 31;
   end record;

   -- PMSC_CTRL1 sub-register
   type PMSC_CTRL1_Type is record
      ARX2INIT  : Types.Bits_1 := 0;
      PKTSEQ    : Types.Bits_8 := 2#1110_0111#;
      ATXSLP    : Types.Bits_1 := 0;
      ARXSLP    : Types.Bits_1 := 0;
      SNOZE     : Types.Bits_1 := 0;
      SNOZR     : Types.Bits_1 := 0;
      PLLSYN    : Types.Bits_1 := 0;
      LDERUNE   : Types.Bits_1 := 1;
      KHZCLKDIV : Types.Bits_6 := 2#10_0000#;

      Reserved_1 : Types.Bits_1 := 0;
      Reserved_2 : Types.Bits_1 := 0;
      Reserved_3 : Types.Bits_1 := 0;
      Reserved_4 : Types.Bits_8 := 2#0100_0000#;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for PMSC_CTRL1_Type use record
      Reserved_1 at 0 range 0 .. 0;

      ARX2INIT   at 0 range 1 .. 1;

      Reserved_2 at 0 range 2 .. 2;

      PKTSEQ     at 0 range 3 .. 10;
      ATXSLP     at 0 range 11 .. 11;
      ARXSLP     at 0 range 12 .. 12;
      SNOZE      at 0 range 13 .. 13;
      SNOZR      at 0 range 14 .. 14;
      PLLSYN     at 0 range 15 .. 15;

      Reserved_3 at 0 range 16 .. 16;

      LDERUNE    at 0 range 17 .. 17;

      Reserved_4 at 0 range 18 .. 25;

      KHZCLKDIV  at 0 range 26 .. 31;
   end record;

   -- PMSC_SNOZT sub-register
   type PMSC_SNOZT_Type is record
      SNOZ_TIM : Types.Bits_8 := 2#0100_0000#;
   end record
     with Size => 8,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for PMSC_SNOZT_Type use record
      SNOZ_TIM at 0 range 0 .. 7;
   end record;

   -- PMSC_TXFSEQ sub-register
   type PMSC_TXFSEQ_Type is record
      TXFINESEQ : Types.Bits_16 := 2#0000_1011_0011_1100#;
   end record
     with Size => 16,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for PMSC_TXFSEQ_Type use record
      TXFINESEQ at 0 range 0 .. 15;
   end record;

   -- PMSC_LEDC sub-register
   type PMSC_LEDC_Type is record
      BLINK_TIM : Types.Bits_8 := 2#0010_0000#;
      BLNKEN    : Types.Bits_1 := 0;
      BLNKNOW   : Types.Bits_4 := 0;

      Reserved_1 : Types.Bits_7  := 0;
      Reserved_2 : Types.Bits_12 := 0;
   end record
     with Size => 32,
     Bit_Order => System.Low_Order_First,
     Scalar_Storage_Order => System.Low_Order_First;

   for PMSC_LEDC_Type use record
      BLINK_TIM  at 0 range 0 .. 7;
      BLNKEN     at 0 range 8 .. 8;

      Reserved_1 at 0 range 9 .. 15;

      BLNKNOW    at 0 range 16 .. 19;

      Reserved_2 at 0 range 20 .. 31;
   end record;

end DW1000.Register_Types;
