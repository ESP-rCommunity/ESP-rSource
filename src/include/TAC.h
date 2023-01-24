C ********************************************************************************
C This file contains common blocks and variable declarations related to the
C thermally activated cooling (TAC) plant component and an associated water tank
C plant component. These were previously coded in F90 modules, but
C recast into the current format to make this code compatible with the
C g77 compiler.
C ********************************************************************************


C ********************************************************************************
C Data for the TAC component (formerly MODULE TAC_1).
C ********************************************************************************
      COMMON/TAC_1/
     &  TAC_Pw_gen_inlet,TAC_Tgenr,TAC_gen_eff,TAC_gen_limit,
     &  TAC_a,TAC_b1,TAC_b2,TAC_b3,TAC_c1,TAC_c2,TAC_c3,
     &  TAC_d1,TAC_d2,TAC_d3,TAC_Tgenref,TAC_Tcondref,
     &  TAC_Tevapref,TAC_pump,TAC_cond_mdot_air,TAC_cond_qfan,
     &  TAC_Triselocal,iIP_DG_controller


C---------------------------------------------------------------------------------
C Pressure of water in loop connecting outlet of FC-cogen exhaust-to-water
C heat exchanger to generator inlet. This is required to determine the properties
C of the water flowing into the generator's heat exchanger.
C---------------------------------------------------------------------------------
      REAL TAC_Pw_gen_inlet  ! (Pa).

C---------------------------------------------------------------------------------
C Generator characteristics.
C---------------------------------------------------------------------------------
      REAL TAC_Tgenr      ! Temperature of refrigerant at generator inlet (oC).
      REAL TAC_gen_eff    ! Effectiveness of generator's heat exchanger (-).
      REAL TAC_gen_limit  ! Maximum heat input to generator at std conditions (W).

C---------------------------------------------------------------------------------
C Parametric-empirical representation of COP.
C---------------------------------------------------------------------------------
      REAL TAC_a    ! Nominal COP at standard operating conditions (-).
      REAL TAC_b1   ! Parametric coeff for 1st order temp dependency (1/oC).
      REAL TAC_b2   ! Ditto.
      REAL TAC_b3   ! Ditto.
      REAL TAC_c1   ! Parametric coeff for 2nd order temp dependency (1/oC2).
      REAL TAC_c2   ! Ditto.
      REAL TAC_c3   ! Ditto.
      REAL TAC_d1   ! Ditto.
      REAL TAC_d2   ! Ditto.
      REAL TAC_d3   ! Ditto.

C---------------------------------------------------------------------------------
C Standard operating temperatures for COP calculation.
C---------------------------------------------------------------------------------
      REAL TAC_Tgenref   ! Generator (oC).
      REAL TAC_Tcondref  ! Condenser (oC).
      REAL TAC_Tevapref  ! Evaporator (oC).

C---------------------------------------------------------------------------------
C Power of pump that circulates refrigerant within TAC.
C---------------------------------------------------------------------------------
      REAL TAC_pump  ! (W).

C---------------------------------------------------------------------------------
C Condenser characteristics.
C---------------------------------------------------------------------------------
      REAL TAC_cond_mdot_air  ! Flow rate of air through condenser (kg/s).
      REAL TAC_cond_qfan      ! Power of fan that blows air through cond (W).
      REAL TAC_Triselocal     ! Temp rise of cond inlet air due to local (oC).

C---------------------------------------------------------------------------------
C Integer pointing to distributed generation controller, if present. (set to 
C zero if not present.
C---------------------------------------------------------------------------------
      INTEGER iIP_DG_controller



C ********************************************************************************
C Data for cold water storage tank for coupling to TAC
C (formerly MODULE TAC_coldtank).
C ********************************************************************************
      COMMON/TAC_coldtank/
     &  TAC_coldtank_Mass,TAC_coldtank_UA,TAC_coldtank_Cp

C---------------------------------------------------------------------------------
C Characteristics of cold tank
C---------------------------------------------------------------------------------
      REAL TAC_coldtank_Mass  ! Mass of water stored in cold tank (kg).
      REAL TAC_coldtank_UA    ! Tank heat gain coefficient (W/K)
      REAL TAC_coldtank_Cp    ! Specific heat of water in tank. (J/kgK)
