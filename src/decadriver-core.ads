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

with Ada.Real_Time;
with DecaDriver.Rx;
with DecaDriver.Tx;
with DW1000.BSP;
with DW1000.Driver;         use DW1000.Driver;
with DW1000.Register_Types; use DW1000.Register_Types;
with DW1000.System_Time;    use DW1000.System_Time;
with DW1000.Types;          use DW1000.Types;

--  @summary
--  High-level Ravenscar driver for typical usage of the DW1000.
--
--  @description
--  This driver provides a high-level API for configuring and using the DW1000.
--
--  This driver consists of three protected objects:
--    * Driver: Provides general configuration for the DW1000.
--    * Receiver: Provides functionality for receiving packets.
--    * Transmitter: Provides functionality for transmitting packets.
--
--  Before the DW1000 can be used, it must be initialized and configured.
--  This is done using the Driver.Initialize and Driver.Configure procedures.
--  The Initialize procedure initializes the DW1000 and, if specified by the
--  user, loads certain values from the DW1000 OTP memory, such as the antenna
--  delay. Below is an example of initializing the DW1000 and driver:
--
--     DecaDriver.Core.Driver.Initialize (Load_Antenna_Delay   => True,
--                                        Load_XTAL_Trim       => True,
--                                        Load_Tx_Power_Levels => True,
--                                        Load_UCode_From_ROM  => True);
--
--  After the DW1000 has been initialized, it can be set to a specific
--  configuration (UWB channel, PRF, preamble code, etc...) using the Configure
--  procedure. Below is an example of configuring the DW1000:
--
--     DecaDriver.Core.Driver.Configure (DecaDriver.Configuration_Type'
--        (Channel             => 1,
--         PRF                 => DW1000.Driver.PRF_64MHz,
--         Tx_Preamble_Length  => DW1000.Driver.PLEN_1024,
--         Tx_PAC              => DW1000.Driver.PAC_32,
--         Tx_Preamble_Code    => 9,
--         Rx_Preamble_Code    => 9,
--         Use_Nonstandard_SFD => False,
--         Data_Rate           => DW1000.Driver.Data_Rate_850k,
--         PHR_Mode            => DW1000.Driver.Standard_Frames,
--         Enable_Smart_Power  => True));
--
--  Note that some configuration parameters use values that are defined in the
--  package DW1000.Driver.
--
--  If the transmitter is to be used, then the transmit power must be
--  configured to ensure that the DW1000 transmits at a suitable power level
--  within the -41.3 dBm/MHz regulatory limit. The transmit power depends on
--  the UWB channel and PRF that has been configured, as well as the specific
--  RF circuitry and antenna that is being used.
--
--  Furthermore, the DW1000 supports two modes of transmit power: manual
--  transmit power and smart transmit power. In the manual transmit power mode
--  the power for the SHR and PHR portions of the physical frame are configured
--  independently. In the smart transmit power mode different levels of
--  transmit power can be configured for different physical frame lengths;
--  shorter frames can be configured to transmit at a higher power.
--
--  The power level is configured using a combination of a coarse gain and fine
--  gain values. The coarse gain permits the range 0.0 .. 18.0 dB in steps of
--  3 dB, and the fine gain permits the range 0.0 .. 15.5 dB in steps of
--- 0.5 dB. The coarse gain output can also be disabled.
--
--  Below is an example of configuring the DW1000 to operate in manual
--  transmit power mode, with the SHR and PHR portions of the frame configured
--  to transmit with a total gain of 19.5 dBm:
--
--     DecaDriver.Tx.Transmitter.Configure_Tx_Power
--       (Smart_Tx_Power_Enabled => False,
--        Boost_SHR => (Coarse_Gain_Enabled => True,
--                      Coarse_Gain         => 9.0,
--                      Fine_Gain           => 10.5),
--        Boost_PHR => (Coarse_Gain_Enabled => True,
--                      Coarse_Gain         => 9.0,
--                      Fine_Gain           => 10.5));
--
--  Since the transmit power is different for each hardware design this driver
--  cannot automatically configure the correct transmit power. The host
--  application must define the power to use for each UWB channel and PRF.
--  The package EVB1000_Tx_Power defines reference values, suitable for use
--  with the DecaWave EVB1000 evaluation boards using the 0 dBi antenna.
--
--  Once the driver is initialized and configured then packets can be sent
--  and received. Sending a packet is split into the following steps:
--    1. Write the data into the DW1000 transmit buffer.
--    2. Set the frame length information.
--    3. Start the transmission.
--
--  Below is an example of sending a zeroized packet of 10 bytes:
--
--     declare
--        Data : DW1000.Types.Byte_Array(1 .. 10) := (others => 0);
--     begin
--        DecaDriver.Tx.Transmitter.Set_Tx_Data (Data   => Data,
--                                               Offset => 0);
--        DecaDriver.Tx.Transmitter.Set_Tx_Frame_Length
--           (Length => Data'Length,
--            Offset => 0);
--        DecaDriver.Tx.Transmitter.Start_Tx_Immediate (Rx_After_Tx => False);
--     end;
--
--  Note that the receiver can be automatically enabled by the DW1000 after the
--  transmission has completed. This feature is enabled by setting Rx_After_Tx
--  to True when calling Start_Tx_Immediate.
--
--  To wait for the transmission to be completed, you can wait using the
--  Wait_For_Tx_Complete entry. This entry will block until the DW1000 signals
--  to the driver that the transmission has been completed. An example is shown
--  below:
--
--     DecaDriver.Tx.Transmitter.Wait_For_Tx_Complete;
--
--  Packets can only be received when the receiver is enabled. To enable the
--  receiver immediately (without delay), then use the
--  Receiver.Start_Rx_Immediate procedure. Then, to block until a packet is
--  received, use the Receiver.Wait entry. An example of waiting for a packet
--  is shown below:
--
--     declare
--        Frame_Data : DW1000.Types.Byte_Array (DecaDriver.Frame_Length_Number);
--        Length     : DW1000.Types.Frame_Length_Number;
--        Timestamp  : DW1000.System_Time.Fine_System_Time;
--        Error      : DecaDriver.Rx.Rx_Errors;
--        Overrun    : Boolean;
--     begin
--        DecaDriver.Rx.Receiver.Start_Rx_Immediate;
--        DecaDriver.Rx.Receiver.Wait (Frame     => Frame_Data,
--                                     Length    => Length,
--                                     Timestamp => Timestamp,
--                                     Error     => Error,
--                                     Overrun   => Overrun);
--
--        if Error = DecaDriver.Rx.No_Error then
--           --  No error occurred
--        else
--           --  An error occurred during packet reception.
--        end if;
--
--        if Overrun then
--           -- A packet was received before this packet, but it was dropped
--           -- because there was no free space in the receive queue.
--        end if;
--     end;
--
--  During packet reception it is possible for several different types of error
--  to occur. The Receiver driver can be configured to ignore certain errors,
--  so that the Wait entry will only wait for valid frames to be received.
--  To configure which errors are filtered, use the Driver.Configure_Errors
--  procedure. An example of using this procedure to disable all receive error
--  notifications (only wait for valid packets) is shown below:
--
--     DecaDriver.Core.Driver.Configure_Errors (Frame_Timeout => False,
--                                              SFD_Timeout   => False,
--                                              PHR_Error     => False,
--                                              RS_Error      => False,
--                                              FCS_Error     => False);
--
--  By default, all errors are suppressed so that the Receiver.Wait entry will
--  only capture frames that are received without errors.
--
--  It is possible to enable the transmitter or receiver at a specific time,
--  rather than starting the transmission/reception immediately. This is known
--  as the "delayed tx/rx" feature. Typically, this is used to enable the
--  transmitter or receiver after a delay relative to a previous transmission
--  or reception. Below is an example of waiting for a packet to be received,
--  and then transmitting the received packet 10 milliseconds after the packet
--  was received:
--
--     declare
--        Frame_Data : DW1000.Types.Byte_Array (DecaDriver.Frame_Length_Number);
--        Length     : DW1000.Types.Frame_Length_Number;
--        Rx_Time    : DW1000.System_Time.Fine_System_Time;
--        Error      : DecaDriver.Rx.Rx_Errors;
--        Overrun    : Boolean;
--
--        Tx_Time    : DW1000.System_Time.Coarse_System_Time;
--        Tx_Result  : DW1000.Driver.Result_Codes;
--     begin
--        --  Wait for a packet
--        DecaDriver.Rx.Receiver.Start_Rx_Immediate;
--        DecaDriver.Rx.Receiver.Wait (Frame     => Frame_Data,
--                                     Length    => Length,
--                                     Timestamp => Rx_Time,
--                                     Error     => Error,
--                                     Overrun   => Overrun);
--
--        if Error = DecaDriver.Rx.No_Error then
--           --  Compute the transmit time (10 ms after the receive time)
--           Tx_Time :=
--              DW1000.System_Time.To_Coarse_System_Time
--                 (DW1000.System_Time.System_Time_Offset (Rx_Time, 0.01));
--
--           -- Configure the time at which the transmitter should be enabled
--           DecaDriver.Tx.Transmitter.Set_Delayed_Tx_Time (Time => Tx_Time);
--
--           -- Begin the delayed transmission
--           DecaDriver.Tx.Transmitter.Start_Tx_Delayed
--              (Rx_After_Tx => False,
--               Result      => Tx_Result);
--
--           if Result = DW1000.Driver.Success then
--              -- The delayed transmission was configured successfully.
--           else
--              -- Delayed transmit failed. The transmit time is already passed
--           end if;
--        end if;
--     end;
--
--  The delayed transmission can fail if the target transmit time has already
--  passed when Transmitter.Start_Tx_Delayed is called. E.g. in the above
--  example, the delayed transmit will fail if Start_Tx_Delayed is called after
--  the 10 ms delay has already passed. This can happen in cases such as the
--  task being blocked for too long by a higher-priority task.
package DecaDriver.Core
with SPARK_Mode => On
is

   type Configuration_Type is record
      Channel             : DW1000.Driver.Channel_Number;
      PRF                 : DW1000.Driver.PRF_Type;
      Tx_Preamble_Length  : DW1000.Driver.Preamble_Lengths;
      Rx_PAC              : DW1000.Driver.Preamble_Acq_Chunk_Length;
      Tx_Preamble_Code    : DW1000.Driver.Preamble_Code_Number;
      Rx_Preamble_Code    : DW1000.Driver.Preamble_Code_Number;
      Use_Nonstandard_SFD : Boolean;
      Data_Rate           : DW1000.Driver.Data_Rates;
      PHR_Mode            : DW1000.Driver.Physical_Header_Modes;
      SFD_Timeout         : DW1000.Driver.SFD_Timeout_Number;
   end record;

   protected Driver
     with Interrupt_Priority => DecaDriver_Config.Driver_Priority
   is

      procedure Initialize (Load_Antenna_Delay   : in Boolean;
                            Load_XTAL_Trim       : in Boolean;
                            Load_UCode_From_ROM  : in Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State,
                        Input  => Ada.Real_Time.Clock_Time),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Driver,
                                                Load_Antenna_Delay,
                                                Load_XTAL_Trim,
                                                Load_UCode_From_ROM),
                    Driver             => + (DW1000.BSP.Device_State,
                                                  Load_Antenna_Delay,
                                                  Load_XTAL_Trim,
                                                  Load_UCode_From_ROM),
                    null                    => Ada.Real_Time.Clock_Time);
      --  Initialize the DecaDriver and DW1000.
      --
      --  If Load_Antenna_Delay is True then the antenna delay is read from the
      --  DW1000 OTP memory and is stored in this DecaDriver. The antenna delay
      --  is applied when the Configure procedure is called.
      --
      --  If Load_XTAL_Trim is True then the XTAL trim is read from the DW1000
      --  OTP memory and is stored in this DecaDriver. The XTAL trim is applied
      --  when the Configure procedure is called.
      --
      --  If Load_Tx_Power_Levels is True then the transmit power levels are
      --  read from the DW1000 OTP memory and is stored in this DecaDriver. The
      --  transmit power levels are applied when the Configure procedure is
      --  called, based on the specific configuration (channel & PRF).
      --
      --  If Load_UCode_From_ROM is True then the LDE microcode is loaded from
      --  the DW1000's ROM into the DW1000's RAM. This is necessary for the LDE
      --  algorithm to operate. If this is False then the LDE algorithm is
      --  disabled and is not run when packets are received.
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation",
         "Procedures in DW1000.BSP are not blocking");

      procedure Configure (Config : in Configuration_Type)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Config,
                                                Driver),
                    Driver             => + Config),
        Post => (PHR_Mode = Config.PHR_Mode);
      --  Configure the DW1000 for a specific channel, PRF, preamble, etc...
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation",
         "Procedures in DW1000.BSP are not blocking");


      procedure Configure_LEDs (Tx_LED_Enable    : in Boolean;
                                Rx_LED_Enable    : in Boolean;
                                Rx_OK_LED_Enable : in Boolean;
                                SFD_LED_Enable   : in Boolean;
                                Test_Flash       : in Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (Driver                  => + null,
                    DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Tx_LED_Enable,
                                                Rx_LED_Enable,
                                                Rx_OK_LED_Enable,
                                                SFD_LED_Enable,
                                                Test_Flash));
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation",
         "Procedures in DW1000.BSP are not blocking");


      procedure Configure_Errors (Frame_Timeout : in Boolean;
                                  SFD_Timeout   : in Boolean;
                                  PHR_Error     : in Boolean;
                                  RS_Error      : in Boolean;
                                  FCS_Error     : in Boolean)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Frame_Timeout,
                                                SFD_Timeout,
                                                PHR_Error,
                                                RS_Error,
                                                FCS_Error),
                    Driver             => (Driver,
                                                Frame_Timeout,
                                                SFD_Timeout,
                                                PHR_Error,
                                                RS_Error,
                                                FCS_Error));
      --  Configure which error notifications are enabled.
      --
      --  @param Frame_Timeout Set to True if error notifications should be
      --     given for frame timeout events. When False, the frame timeout
      --     event is ignored.
      --
      --  @param SFD_Timeout Set to True if error notifications should be
      --     given for SFD timeout events. When False, the SFD timeout
      --     event is ignored.
      --
      --  @param PHR_Error Set to True if error notifications should be
      --     given for physical header error events. When False, the physical
      --     header errors are ignored.
      --
      --  @param RS_Error Set to True if error notifications should be
      --     given when a frame could not be decoded because an uncorrectable
      --     error was detected in the Reed-Solomon decoder. When False, the
      --     Reed-Solomon decoding errors are ignored.
      --
      --  @param FCS_Error Set to True if error notifications should be
      --     given for packets with an invalid FCS. When False, then FCS errors
      --     are ignored.
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation",
         "Procedures in DW1000.BSP are not blocking");

      procedure Force_Tx_Rx_Off
        with Global => (In_Out => (DW1000.BSP.Device_State, Tx.Transmitter)),
        Depends => (DW1000.BSP.Device_State => DW1000.BSP.Device_State,
                    Driver                  => Driver,
                    Tx.Transmitter          => Tx.Transmitter);
      --  Switch off the transmitter and receiver.
      --
      --  This will abort any reception or transmission currently in progress.
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation",
         "Procedures in DW1000.BSP are not blocking");

      procedure Set_PAN_ID (PAN_ID : in Bits_16)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                PAN_ID),
                    Driver             => Driver);
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation",
         "Procedures in DW1000.BSP are not blocking");

      procedure Set_Short_Address (Short_Address : in Bits_16)
        with Global => (In_Out => DW1000.BSP.Device_State),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Short_Address),
                    Driver             => Driver);
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation",
         "Procedures in DW1000.BSP are not blocking");

      function Get_Part_ID return Bits_32;
      function Get_Lot_ID  return Bits_32;

      function PHR_Mode return DW1000.Driver.Physical_Header_Modes
        With Depends => (PHR_Mode'Result => Driver);

   private

      procedure DW1000_IRQ
        with Attach_Handler => DecaDriver_Config.DW1000_IRQ_Id,
        Global => (In_Out => (DW1000.BSP.Device_State,
                              Rx.Receiver,
                              Tx.Transmitter)),
        Depends => (DW1000.BSP.Device_State => (DW1000.BSP.Device_State,
                                                Driver,
                                                Rx.Receiver),
                    Driver                  => Driver,
                    Tx.Transmitter          => (DW1000.BSP.Device_State,
                                                Tx.Transmitter,
                                                Driver),
                    Rx.Receiver             => (DW1000.BSP.Device_State,
                                                Rx.Receiver,
                                                Driver));
      pragma Annotate
        (GNATprove, False_Positive,
         "potentially blocking operation in protected operation",
         "Procedures in DW1000.BSP are not blocking");
      --  DW1000 IRQ handler.
      --
      --  This performs functionality for packet reception and transmission.

      Part_ID : Bits_32 := 0;
      Lot_ID  : Bits_32 := 0;

      Antenna_Delay_PRF_64 : Fine_System_Time := 0.0;
      Antenna_Delay_PRF_16 : Fine_System_Time := 0.0;
      XTAL_Trim            : Bits_5  := 2#1_0000#;

      Long_Frames : Boolean := False;

      SYS_CFG_Reg : SYS_CFG_Type := SYS_CFG_Type'
        (Reserved_1 => 0,
         Reserved_2 => 0,
         PHR_MODE   => 0,
         others     => 0);

      Use_OTP_XTAL_Trim     : Boolean := False;
      Use_OTP_Antenna_Delay : Boolean := False;

      Detect_Frame_Timeout  : Boolean := True;
      Detect_SFD_Timeout    : Boolean := True;
      Detect_PHR_Error      : Boolean := True;
      Detect_RS_Error       : Boolean := True;
      Detect_FCS_Error      : Boolean := True;

   end Driver;

end DecaDriver.Core;
