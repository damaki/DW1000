--  This spec has been automatically generated from STM32F105xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with System;

package STM32.AFIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -------------------
   -- EVCR_Register --
   -------------------

   subtype EVCR_PIN_Field is STM32.UInt4;
   subtype EVCR_PORT_Field is STM32.UInt3;
   subtype EVCR_EVOE_Field is STM32.Bit;

   --  Event Control Register (AFIO_EVCR)
   type EVCR_Register is record
      --  Pin selection
      PIN           : EVCR_PIN_Field := 16#0#;
      --  Port selection
      PORT          : EVCR_PORT_Field := 16#0#;
      --  Event Output Enable
      EVOE          : EVCR_EVOE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVCR_Register use record
      PIN           at 0 range 0 .. 3;
      PORT          at 0 range 4 .. 6;
      EVOE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -------------------
   -- MAPR_Register --
   -------------------

   subtype MAPR_SPI1_REMAP_Field is STM32.Bit;
   subtype MAPR_I2C1_REMAP_Field is STM32.Bit;
   subtype MAPR_USART1_REMAP_Field is STM32.Bit;
   subtype MAPR_USART2_REMAP_Field is STM32.Bit;
   subtype MAPR_USART3_REMAP_Field is STM32.UInt2;
   subtype MAPR_TIM1_REMAP_Field is STM32.UInt2;
   subtype MAPR_TIM2_REMAP_Field is STM32.UInt2;
   subtype MAPR_TIM3_REMAP_Field is STM32.UInt2;
   subtype MAPR_TIM4_REMAP_Field is STM32.Bit;
   subtype MAPR_CAN1_REMAP_Field is STM32.UInt2;
   subtype MAPR_PD01_REMAP_Field is STM32.Bit;
   subtype MAPR_TIM5CH4_IREMAP_Field is STM32.Bit;
   subtype MAPR_ETH_REMAP_Field is STM32.Bit;
   subtype MAPR_CAN2_REMAP_Field is STM32.Bit;
   subtype MAPR_MII_RMII_SEL_Field is STM32.Bit;
   subtype MAPR_SWJ_CFG_Field is STM32.UInt3;
   subtype MAPR_SPI3_REMAP_Field is STM32.Bit;
   subtype MAPR_TIM2ITR1_IREMAP_Field is STM32.Bit;
   subtype MAPR_PTP_PPS_REMAP_Field is STM32.Bit;

   --  AF remap and debug I/O configuration register (AFIO_MAPR)
   type MAPR_Register is record
      --  SPI1 remapping
      SPI1_REMAP      : MAPR_SPI1_REMAP_Field := 16#0#;
      --  I2C1 remapping
      I2C1_REMAP      : MAPR_I2C1_REMAP_Field := 16#0#;
      --  USART1 remapping
      USART1_REMAP    : MAPR_USART1_REMAP_Field := 16#0#;
      --  USART2 remapping
      USART2_REMAP    : MAPR_USART2_REMAP_Field := 16#0#;
      --  USART3 remapping
      USART3_REMAP    : MAPR_USART3_REMAP_Field := 16#0#;
      --  TIM1 remapping
      TIM1_REMAP      : MAPR_TIM1_REMAP_Field := 16#0#;
      --  TIM2 remapping
      TIM2_REMAP      : MAPR_TIM2_REMAP_Field := 16#0#;
      --  TIM3 remapping
      TIM3_REMAP      : MAPR_TIM3_REMAP_Field := 16#0#;
      --  TIM4 remapping
      TIM4_REMAP      : MAPR_TIM4_REMAP_Field := 16#0#;
      --  CAN1 remapping
      CAN1_REMAP      : MAPR_CAN1_REMAP_Field := 16#0#;
      --  Port D0/Port D1 mapping on OSCIN/OSCOUT
      PD01_REMAP      : MAPR_PD01_REMAP_Field := 16#0#;
      --  Set and cleared by software
      TIM5CH4_IREMAP  : MAPR_TIM5CH4_IREMAP_Field := 16#0#;
      --  unspecified
      Reserved_17_20  : STM32.UInt4 := 16#0#;
      --  Ethernet MAC I/O remapping
      ETH_REMAP       : MAPR_ETH_REMAP_Field := 16#0#;
      --  CAN2 I/O remapping
      CAN2_REMAP      : MAPR_CAN2_REMAP_Field := 16#0#;
      --  MII or RMII selection
      MII_RMII_SEL    : MAPR_MII_RMII_SEL_Field := 16#0#;
      --  Write-only. Serial wire JTAG configuration
      SWJ_CFG         : MAPR_SWJ_CFG_Field := 16#0#;
      --  unspecified
      Reserved_27_27  : STM32.Bit := 16#0#;
      --  SPI3/I2S3 remapping
      SPI3_REMAP      : MAPR_SPI3_REMAP_Field := 16#0#;
      --  TIM2 internal trigger 1 remapping
      TIM2ITR1_IREMAP : MAPR_TIM2ITR1_IREMAP_Field := 16#0#;
      --  Ethernet PTP PPS remapping
      PTP_PPS_REMAP   : MAPR_PTP_PPS_REMAP_Field := 16#0#;
      --  unspecified
      Reserved_31_31  : STM32.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAPR_Register use record
      SPI1_REMAP      at 0 range 0 .. 0;
      I2C1_REMAP      at 0 range 1 .. 1;
      USART1_REMAP    at 0 range 2 .. 2;
      USART2_REMAP    at 0 range 3 .. 3;
      USART3_REMAP    at 0 range 4 .. 5;
      TIM1_REMAP      at 0 range 6 .. 7;
      TIM2_REMAP      at 0 range 8 .. 9;
      TIM3_REMAP      at 0 range 10 .. 11;
      TIM4_REMAP      at 0 range 12 .. 12;
      CAN1_REMAP      at 0 range 13 .. 14;
      PD01_REMAP      at 0 range 15 .. 15;
      TIM5CH4_IREMAP  at 0 range 16 .. 16;
      Reserved_17_20  at 0 range 17 .. 20;
      ETH_REMAP       at 0 range 21 .. 21;
      CAN2_REMAP      at 0 range 22 .. 22;
      MII_RMII_SEL    at 0 range 23 .. 23;
      SWJ_CFG         at 0 range 24 .. 26;
      Reserved_27_27  at 0 range 27 .. 27;
      SPI3_REMAP      at 0 range 28 .. 28;
      TIM2ITR1_IREMAP at 0 range 29 .. 29;
      PTP_PPS_REMAP   at 0 range 30 .. 30;
      Reserved_31_31  at 0 range 31 .. 31;
   end record;

   ----------------------
   -- EXTICR1_Register --
   ----------------------

   ------------------
   -- EXTICR1.EXTI --
   ------------------

   --  EXTICR1_EXTI array element
   subtype EXTICR1_EXTI_Element is STM32.UInt4;

   --  EXTICR1_EXTI array
   type EXTICR1_EXTI_Field_Array is array (0 .. 3) of EXTICR1_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR1_EXTI
   type EXTICR1_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : STM32.Short;
         when True =>
            --  EXTI as an array
            Arr : EXTICR1_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR1_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  External interrupt configuration register 1 (AFIO_EXTICR1)
   type EXTICR1_Register is record
      --  EXTI0 configuration
      EXTI           : EXTICR1_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR1_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ----------------------
   -- EXTICR2_Register --
   ----------------------

   ------------------
   -- EXTICR2.EXTI --
   ------------------

   --  EXTICR2_EXTI array element
   subtype EXTICR2_EXTI_Element is STM32.UInt4;

   --  EXTICR2_EXTI array
   type EXTICR2_EXTI_Field_Array is array (4 .. 7) of EXTICR2_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR2_EXTI
   type EXTICR2_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : STM32.Short;
         when True =>
            --  EXTI as an array
            Arr : EXTICR2_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR2_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  External interrupt configuration register 2 (AFIO_EXTICR2)
   type EXTICR2_Register is record
      --  EXTI4 configuration
      EXTI           : EXTICR2_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR2_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ----------------------
   -- EXTICR3_Register --
   ----------------------

   ------------------
   -- EXTICR3.EXTI --
   ------------------

   --  EXTICR3_EXTI array element
   subtype EXTICR3_EXTI_Element is STM32.UInt4;

   --  EXTICR3_EXTI array
   type EXTICR3_EXTI_Field_Array is array (8 .. 11) of EXTICR3_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR3_EXTI
   type EXTICR3_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : STM32.Short;
         when True =>
            --  EXTI as an array
            Arr : EXTICR3_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR3_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  External interrupt configuration register 3 (AFIO_EXTICR3)
   type EXTICR3_Register is record
      --  EXTI8 configuration
      EXTI           : EXTICR3_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR3_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ----------------------
   -- EXTICR4_Register --
   ----------------------

   ------------------
   -- EXTICR4.EXTI --
   ------------------

   --  EXTICR4_EXTI array element
   subtype EXTICR4_EXTI_Element is STM32.UInt4;

   --  EXTICR4_EXTI array
   type EXTICR4_EXTI_Field_Array is array (12 .. 15) of EXTICR4_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR4_EXTI
   type EXTICR4_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : STM32.Short;
         when True =>
            --  EXTI as an array
            Arr : EXTICR4_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR4_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  External interrupt configuration register 4 (AFIO_EXTICR4)
   type EXTICR4_Register is record
      --  EXTI12 configuration
      EXTI           : EXTICR4_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR4_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- MAPR2_Register --
   --------------------

   subtype MAPR2_TIM9_REMAP_Field is STM32.Bit;
   subtype MAPR2_TIM10_REMAP_Field is STM32.Bit;
   subtype MAPR2_TIM11_REMAP_Field is STM32.Bit;
   subtype MAPR2_TIM13_REMAP_Field is STM32.Bit;
   subtype MAPR2_TIM14_REMAP_Field is STM32.Bit;
   subtype MAPR2_FSMC_NADV_Field is STM32.Bit;

   --  AF remap and debug I/O configuration register
   type MAPR2_Register is record
      --  unspecified
      Reserved_0_4   : STM32.UInt5 := 16#0#;
      --  TIM9 remapping
      TIM9_REMAP     : MAPR2_TIM9_REMAP_Field := 16#0#;
      --  TIM10 remapping
      TIM10_REMAP    : MAPR2_TIM10_REMAP_Field := 16#0#;
      --  TIM11 remapping
      TIM11_REMAP    : MAPR2_TIM11_REMAP_Field := 16#0#;
      --  TIM13 remapping
      TIM13_REMAP    : MAPR2_TIM13_REMAP_Field := 16#0#;
      --  TIM14 remapping
      TIM14_REMAP    : MAPR2_TIM14_REMAP_Field := 16#0#;
      --  NADV connect/disconnect
      FSMC_NADV      : MAPR2_FSMC_NADV_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : STM32.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAPR2_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      TIM9_REMAP     at 0 range 5 .. 5;
      TIM10_REMAP    at 0 range 6 .. 6;
      TIM11_REMAP    at 0 range 7 .. 7;
      TIM13_REMAP    at 0 range 8 .. 8;
      TIM14_REMAP    at 0 range 9 .. 9;
      FSMC_NADV      at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Alternate function I/O
   type AFIO_Peripheral is record
      --  Event Control Register (AFIO_EVCR)
      EVCR    : EVCR_Register;
      --  AF remap and debug I/O configuration register (AFIO_MAPR)
      MAPR    : MAPR_Register;
      --  External interrupt configuration register 1 (AFIO_EXTICR1)
      EXTICR1 : EXTICR1_Register;
      --  External interrupt configuration register 2 (AFIO_EXTICR2)
      EXTICR2 : EXTICR2_Register;
      --  External interrupt configuration register 3 (AFIO_EXTICR3)
      EXTICR3 : EXTICR3_Register;
      --  External interrupt configuration register 4 (AFIO_EXTICR4)
      EXTICR4 : EXTICR4_Register;
      --  AF remap and debug I/O configuration register
      MAPR2   : MAPR2_Register;
   end record
     with Volatile;

   for AFIO_Peripheral use record
      EVCR    at 0 range 0 .. 31;
      MAPR    at 4 range 0 .. 31;
      EXTICR1 at 8 range 0 .. 31;
      EXTICR2 at 12 range 0 .. 31;
      EXTICR3 at 16 range 0 .. 31;
      EXTICR4 at 20 range 0 .. 31;
      MAPR2   at 28 range 0 .. 31;
   end record;

   --  Alternate function I/O
   AFIO_Periph : aliased AFIO_Peripheral
     with Import, Address => AFIO_Base;

end STM32.AFIO;
