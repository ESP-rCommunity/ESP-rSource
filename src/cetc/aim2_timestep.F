C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C This file contains the following routines related to the AIM-2 infiltration model.
C These routines are invoked during bps' time-step simulation.

C   AIM2_CONTROL             Controls the time-step AIM-2 calculations.
C   AIM2_FLUELEAK            Calculates flue flow coefficient.
C   AIM2_C_R_X_Y             Calculates total leakage coefficient and leakage distribution parameters.
C   AIM2_stack_flowF         Calculates the stack flow factor.
C   AIM2_stack_pressure      Calculates the stack effect reference pressure.
C   AIM2_stack_infil         Calculates the infiltration air flow induced by the stack effect.
C   AIM2_wind_flowF          Calculates the wind flow factor.
C   AIM2_wind_shelter        Calculates the wind local wind shelter coefficient.
C   AIM2_wind_pressure       Calculates the wind effect reference pressure.
C   AIM2_wind_infil          Calculates the infiltration air flow induced by the wind effect.
C   AIM2_stack_plus_wind     Calculates the infiltration air flow induced by both stack and wind effects.
C   AIM2_mechanical          Calculates the unbalanced mechanical ventilation rate in the house
C   AIM2_comb_nat_mech       Calculates net air flow rate from outdoors caused by combined
C                            influence of natural infiltration and unbalanced mechanical ventilation.
C   AIM2_sumover_DT          Calculates average air flow rate from outdoors over time-step when
C                            separate calculations are performed for furnace on and off cycles.
C   AIM2_split2zones         Apportions infiltration to individual zones in model.
C   AIM2_inf2attic           Set the infiltration rate for the attic zone.
C   AIM2_airflow_to_conduct  Expresses infiltration air flow rate in terms of a conductance between
C                            indoor-outdoor temperature difference.

C *******************************************************************************
C References:
C
C    Beausoleil-Morrison I. (2000), AIM-2 Implementation into ESP-r:
C    Solving for R, X, and Y Leakage Distribution Parameters, NRCan internal report.
C
C    Bradley B. (1993), Implementation of the AIM-2 Infiltration
C    Model in HOT2000, Unies Contract Report to NRCan.
C
C    Bradley B. (2000), E-mail to Ian Beausoleil-Morrison dated October 13.
C
C    Incropera F.P. and Dewitt D.P. (1985), Fundamentals of Heat and Mass
C    Transfer, Second Edition, John Wiley & Sons.
C
C    Kiel D.E. and Wilson D.J. (1987), `Influence of Natural Infiltration on Total
C    Building Ventilation Dominated by Strong Fan Exhaust', ASHRAE Transactions 93(2).
C
C    Walker I.S. and Wilson D.J. (1990), The Alberta Air Infiltration
C    Model: AIM-2, University of Alberta Dept of Mech Eng, Report 71.
C
C    Wilson D.J. and Walker I.S. (1990), `Combining Air Infiltration and Exhaust
C    Ventilation', Indoor Air '90, Proc 5th Int Conf on Indoor Air Quality and Climate,
C    Toronto Canada, 467-472.
C
C    Wray C. (1997), Letter to Brian Bradley dated March 17.
C
C    Walker I.S. and Wilson D.J. (1998), "Field validation of algebraic equations for
C    stack and wind driven air infiltration calculations", HVAC & R Research, 4(2):119-139.
C
C *******************************************************************************


C *******************************************************************************
C ****************************** AIM2_CONTROL ***********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 2, 2000
C Copyright 2000: NRCan Buildings Group
C
C Modified by Ian Beausoleil-Morrison on July 13, 2001:
C   - Pass furnace part-load ratio to AIM-2 model.

C Modified by Stephanie Mombourquette on May 4, 2004:
C   - Added IMPLICIT NONE to each subroutine..

C This subroutine controls the time-step portion of the AIM-2 calculations.
C The AIM-2 model is used to determine the infiltration rate and load to
C each zone. This subroutine is called for each zone in the model each
C time-step of the simulation.

C INPUTS:
C    ICOMP  The zone under consideration
C    furnON_flue_diam  Diameter of furnace flue when furnace operating (mm).
C    furnOFF_flue_diam  Diameter of furnace flue when furnace off (mm).

C OUTPUTS:
C    ZONE_infil_cond  The zone's infiltration, expressed in terms of
C                     a conductance (W/K).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_CONTROL(ICOMP,ZONE_infil_cond)
C      USE AIM2_CALC_DATA
C      USE HVAC_CALC_DATA

      IMPLICIT NONE
#include "aim2_parameters.h"      
#include "aim2_common.h"



C HVAC_CALC_DATA common used to pass pass furnace part-load ratio to AIM-2 model
C on time-step basis.
      common/HVAC_CALC_DATA/HVAC_furnace_plr

c Declare real in HVAC_CALC_DATA common
      REAL HVAC_furnace_plr
C Declarations for variables passed in calling statement.
      INTEGER ICOMP
      REAL ZONE_infil_cond

C Declarations for local variables.
      INTEGER num_inf_regimes,inf_regime
      REAL frac_ON,frac_OFF
      REAL AIM2_Cflue,AIM2_Ctotal,AIM2_Y,AIM2_R,AIM2_X,AIM2_Fs,AIM2_Ps,
     &     AIM2_Qstack,AIM2_Fw,AIM2_Sw,AIM2_Pw,AIM2_Qwind,AIM2_natural,
     &     AIM2_unbal_mech,AIM2_total,AIM2_total_sum

C The AIM-2 model calculates the infiltration rate for the whole house but
C we are currently within the zone loop. Therefore, execute the AIM-2 algorithm
C only for the model's first zone. This will calculate the infiltration rate for
C the whole house. Following this, apportion the infiltration air flow to
C the individual zones.

      IF( ICOMP.eq.1 ) THEN  ! First zone, perform AIM-2 calcs.

C If the furnace's flue affects the house's infiltration, then the infiltration rate
C will be calculated twice: once for the case when the furnace is operating and once
C for the case when it is off. A weighted sum of these two calculations will be used
C to determine the infiltration rate for the current time-step. The furnace will
C cycle on and off during the simulation time-step. As such, the furnace's part-load
C ratio (as calculated by the furnace routine one time-step in arrears) is used to
C establish the weighting for the infiltration summation.
C In cases where the furnace's operation does not affect the infiltration (e.g. condensing
C and induced draft furnaces which are aerodynamically sealed from the house, electric
C furnaces, and old-style furnaces with no damper control) the infiltration calculation
C is performed only once for the time-step.

C First determine whether the infiltration rate needs to be calculated separately
C for furnace on and off cycles. If two calculations are necessary then establish
C the weightings for the infiltration summation that will be performed later.
        IF(furnON_flue_diam.eq.furnOFF_flue_diam)THEN
          num_inf_regimes = 1 ! Furnace operation does not affect infiltration.
        ELSE
          num_inf_regimes = 2             ! Infiltration affected by furnace operation.
          frac_ON = HVAC_furnace_plr      ! frac_ON is frac of time furnace ran previous
          frac_OFF = 1.0-HVAC_furnace_plr   ! sim time-step: comes from the furnace model.
        ENDIF
        HVAC_furnace_plr = 0.  ! Reset to zero (necessary in case furnace does not run this time-step).

C Now calculate the infiltration rate(s). First for the furnace on cycle and
C then (if necessary) for the furnace off cycle.
        DO inf_regime=1,num_inf_regimes

C---------Calculate leakage coefficients and parameters.
          CALL AIM2_FLUELEAK(inf_regime,AIM2_Cflue)           ! Calc flue leakage coeff.
          CALL AIM2_C_R_X_Y(AIM2_Cflue,AIM2_Ctotal,           ! Calc building total flow coeff and
     &                      AIM2_Y,AIM2_R,AIM2_X)             ! leakage distribution parameters.    

C---------Stack effect.
          CALL AIM2_stack_flowF(AIM2_Y,AIM2_R,AIM2_X,AIM2_Fs) ! Calc stack flow factor.
          CALL AIM2_stack_pressure(AIM2_Ps)                   ! Calc stack effect reference pressure.
          CALL AIM2_stack_infil(AIM2_Ctotal,AIM2_Fs,AIM2_Ps,  ! Calc infil induced by stack effect.
     &                          AIM2_Qstack)

C---------Wind effect.
          CALL AIM2_wind_flowF(AIM2_Y,AIM2_R,AIM2_X,AIM2_Fw)  ! Calc wind effect flow factor.
          CALL AIM2_wind_shelter(AIM2_Y,AIM2_Sw)              ! Calc local wind shelter coeff.
          CALL AIM2_wind_pressure(AIM2_Sw,AIM2_Pw)            ! Calc wind effect reference pressure.
          CALL AIM2_wind_infil(AIM2_Ctotal,AIM2_Fw,AIM2_Pw,   ! Calc infil induced by wind effect.
     &                         AIM2_Qwind)

C---------Interaction of stack and wind effects.
          CALL AIM2_stack_plus_wind(AIM2_Qstack,AIM2_Qwind,AIM2_natural)

C---------Interaction of natural and mechanical ventilation.
          CALL AIM2_mechanical(AIM2_unbal_mech)               ! Calc unbalanced mech vent rate.
          CALL AIM2_comb_nat_mech(AIM2_natural,               ! Calc net infiltration.
     &                            AIM2_unbal_mech,AIM2_total)

C---------If separate calculations are performed for furnace on and off cycles, then
C---------determine average infiltration rate over time step.
          IF(num_inf_regimes.eq.2)THEN
            CALL AIM2_sumover_DT(inf_regime,AIM2_total,frac_ON,  ! Weight air flow for ON/OFF state.
     &                           frac_OFF,AIM2_total_sum)
            if(inf_regime.eq.2) AIM2_total = AIM2_total_sum        ! Average air flow over time-step.  
          ENDIF

        END DO

C The building's total air flow from outdoors has now been determined. Note that this does
C not include the infiltration to the crawlspace or attic, only to the occupied portion
C of the building. The AIM-2 model calculates the infiltration rate for the whole house.
c It is called only for the model's first zone. Now this total infiltration rate must be
C apportioned to individual zones.
        CALL AIM2_split2zones(AIM2_total)

C The above calculated the infiltration to the occupied portion of the house. Now determine
C the infiltration to the attic zone (if it exists).
        CALL AIM2_inf2attic

C End of AIM-2 calculations for first zone in model. By this point the infiltration
C air flow to each zone of the model has been calculated for the current time-step.
      ENDIF

C For the zone called, assign the zone's calculated infiltration conductance
C to ESP-r's standard infiltration variable, CVIF. CVIF is incorporated into the
C zone's energy balance by ESP-r's normal approach, so no further
C interactions with the zone matrix processor and solver are required.
      CALL AIM2_airflow_to_conduct(ICOMP,ZONE_infil_cond)

C Record the infiltration rate for use in creating the results file that will be passed
C to the HOT3000 interface.
      CALL H3KSTORE_AIM2_DATA(AIM2_total)

C Completion of the infiltration calculations for this zone, this time-step.
      RETURN
      END


C ********************************************************************************
C **************************** AIM2_FLUELEAK *************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 13, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the flue leakage coefficient which is required
C by the AIM-2 algorithm. It is called each time-step since the flue leakage
C is a function of furnace operation. When the flue's effective opening varies
C between furnace on and off states this subroutine is called twice per time-step.

C INPUTS:
C    inf_regime  Flag indicating whether to calculate flue coefficient for case
C                where furnace operating (1) or off (0).
C    furnON_flue_diam  Effective diameter of furnace flue when furnace operating.
C    furnOFF_flue_diam  Effective diameter of furnace flue when furnace off.
C    TFA(i)  Air-point temperature of zone `i' at future time-row
C            (standard ESP-r solution variable).
C    AIM2_n  Flow coefficient of house.
C    AIM2_ref_zone  Number of zone whose temp used as reference for infil calcs. 
C    H2K_fireplace1  Flag indicating whether fireplace #1 exists (true/false).
C    H2K_fireplace2  Flag indicating whether fireplace #2 exists (true/false).
C    AIM2_fire_flue1  Diameter of #1 fireplace flue (mm).
C    AIM2_fire_flue2  Diameter of #2 fireplace flue (mm).
C    H2K_DHW1_exist  Flag indicating presence of primary DHW system (true/false).
C    H2K_DHW2_exist  Flag indicating presence of secondary DHW system (true/false).
C    H2K_DHW1_shareflue  Flag indicating whether primary DHW system shares
C                        flue with the furnace (true/false).
C    H2K_DHW2_shareflue  Flag indicating whether primary DHW system shares
C                        flue with the furnace (true/false).
C    AIM2_DHW_flue1  Diameter of primary DHW system flue (mm).
C    AIM2_DHW_flue2  Diameter of secondary DHW system flue (mm).
C    H2K_DHW1_inside  Flag indicating whether primary DHW system located
C                     indoors (true/false).
C    H2K_DHW2_inside  Flag indicating whether secondary DHW system located
C                     indoors (true/false).
C    H2K_DHW1_solar  Flag indicating whether primary DHW system is a solar collector
C                    (true/false).

C OUTPUTS:
C    AIM2_Cflue  The flue leakage coefficient (m^3/s/Pa^1/n).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_FLUELEAK(inf_regime,AIM2_Cflue)
C      USE AIM2_INPUT_DATA
C      USE AIM2_CALC_DATA
C      USE AIM2_H2K_DUMMY

      IMPLICIT NONE

#include "building.h"
#include "aim2_parameters.h"      
#include "aim2_common.h"

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      REAL TFA,QFA

C Type declarations for variables passed in calling statement.
      INTEGER inf_regime
      REAL AIM2_Cflue

      REAL AIRDEN

C Type declarations for local variables.
      REAL furn_flue_Dmm,Cd,dPref,Tref,rho,Aflue



C Determine the furnace flue's diameter. This is a function of whether the
C calculation is being performed for the furnace on or off state.
      IF(inf_regime.eq.1)THEN
        furn_flue_Dmm = furnON_flue_diam  ! Furnace on.
      ELSE
        furn_flue_Dmm = furnOFF_flue_diam ! Furnace off.
      ENDIF

C Calculate the area of the furnace flue's opening.
      Aflue = PI/4. * (furn_flue_Dmm/1000.)**2. ! `PI' from module; area in m2.

C If there are solid fuel burning appliances (e.g. fireplace) then add the
C effective areas of their flues. The user is meant to input the effective
C area of these flues to account for damper and door operation.
      IF(H2K_fireplace1)THEN  ! Fireplace #1 exists.
        Aflue = Aflue + PI/4. * (AIM2_fire_flue1/1000.)**2.
      ENDIF
      IF(H2K_fireplace2)THEN  ! Fireplace #2 exists.
        Aflue = Aflue + PI/4. * (AIM2_fire_flue2/1000.)**2.
      ENDIF

C The DHW systems (primary and secondary) may have dedicated flues or they may
C share the same flue as the furnace. If there are separate flues and if the
C DHW systems are located indoors the add these flues to the total flue area.
      IF(H2K_DHW1_exist)THEN             ! There is a primary DHW system.
        if(.not.H2K_DHW1_shareflue)then  ! DHW flue is separate from furnace flue.
          if(.not.H2K_DHW1_solar)then    ! No flue on solar DHW systems.
            if(H2K_DHW1_inside)then      ! Don't care about flue if system outdoors.
              Aflue = Aflue + PI/4. * (AIM2_DHW_flue1/1000.)**2.
            endif
          endif
        endif
      ENDIF
      IF(H2K_DHW2_exist)THEN             ! There is a secondary DHW system.
        if(.not.H2K_DHW2_shareflue)then  ! DHW flue is separate from furnace flue.
          if(H2K_DHW2_inside)then        ! Don't care about flue if system outdoors.
            Aflue = Aflue + PI/4. * (AIM2_DHW_flue2/1000.)**2.
          endif
        endif
      ENDIF

C Set the discharge coefficient (Cd) and the reference pressure difference (dPref).
C These are used to calculate the flue flow coefficient. According to Wray (1997)
C if Aflue represents the "effective" leakage then Cd should be 1 and dPref should
C be 4. Whereas if Aflue represents the "equivalent" leakage then Cd should be 0.611
C and dPref should be 10. In contrast, HOT2000 v8 sets Cd to 0.5 and dPref to 4.
C According to Bradley (2000) Iain Walker disagreed with Craig Wray on
C this point and agreed with the HOT2000 v8 implementation. Also according
C to Bradley (2000) the HOT2000 user-input flue diameter (and hence Aflue) is a
C "nominal" value, not "effective" or "equivalent". Therefore, the approach used
C in HOT2000 v8 is retained here.
      Cd = 0.5    ! Discharge coefficient (see annotations above).
      dPref = 4.0 ! Reference pressure used to calculate flue flow coefficient.

C Calculate the air density. This is evaluated using the ideal gas law (using
C a function from the CFD domain) evaluated at the temperature of the zone
C defined to be the reference. The indoor temperature is used here because the
C air is flowing from indoors to out through the flue.
      Tref = TFA(AIM2_ref_zone) ! Use `future' temperature.
      rho = AIRDEN(Tref)

C Now calculate the flue flow coefficient. This is equation E from Wray (1997).
C As discussed in Walker & Wilson (1990, p3), the flow coefficient is assumed
C to be the same for all leakage sites. As such, the AIM2_n previously
C established is used here. The units of the leakage coefficient are
C m^3/(s Pa^n) where `n' is the flow exponent (dimensionless).
      AIM2_Cflue = Cd * Aflue * sqrt(2./rho) * (1./dPref)**(AIM2_n-0.5)


      RETURN
      END


C ********************************************************************************
C ***************************** AIM2_C_R_X_Y *************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 17, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the building's total leakage coefficient, and the
C `ceiling-floor sum', `ceiling-floor difference', and `flue fraction' leakage
C distribution parameters. It is called each time-step since the flue leakage
C is a function of furnace operation. When the flue's effective opening varies
C between furnace on and off states this subroutine is called twice per time-step.

C INPUTS:
C    AIM2_ceil_frac  Ceiling leakage fraction (dimensionless).
C    AIM2_wall_frac  Wall leakage fraction (dimensionless).
C    AIM2_floor_frac  Floor leakage fraction (dimensionless).
C    AIM2_CO  The building's total fabric leakage coefficient (m^3/s/Pa^1/n).
C    AIM2_Cflue  The flue leakage coefficient (m^3/s/Pa^1/n).

C OUTPUTS:
C    AIM2_Ctotal  The building's total leakage coefficient (m^3/s/Pa^1/n).
C    AIM2_Y  The flue leakage fraction (dimensionless).
C    AIM2_R  The "ceiling-floor sum" leakage distribution parameter (dimensionless).
C    AIM2_X  The "ceiling-floor difference" leakage distribution parameter (dimensionless).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_C_R_X_Y(AIM2_Cflue,AIM2_Ctotal,AIM2_Y,AIM2_R,
     &                        AIM2_X)
C      USE AIM2_INPUT_DATA
C      USE AIM2_CALC_DATA

      IMPLICIT NONE
#include "aim2_parameters.h"      
#include "aim2_common.h"

C Type declarations for variables passed in calling statement.
      REAL AIM2_Cflue,AIM2_Ctotal,AIM2_Y,AIM2_R,AIM2_X

C Calculate the building's total leakage coefficient (Walker & Wilson 1990, eq 2).
C The units of the leakage coefficient are m^3/(s Pa^n) where `n' is the
C flow exponent (dimensionless).
      AIM2_Ctotal = AIM2_CO + AIM2_Cflue

C Calculate the flue fraction (Walker & Wilson 1990, eq 5).
      AIM2_Y = AIM2_Cflue / AIM2_Ctotal

C Calculate the "ceiling-floor sum" leakage distribution parameter
C (Beausoleil-Morrison 2000, eq 13).
      AIM2_R = (AIM2_ceil_frac+AIM2_floor_frac) * (1.0-AIM2_Y)

C Calculate the "ceiling-floor difference" leakage distribution parameter
C (Beausoleil-Morrison 2000, eq 14).
      AIM2_X = (AIM2_ceil_frac-AIM2_floor_frac) * (1.0-AIM2_Y)


      RETURN
      END


C ********************************************************************************
C *************************** AIM2_stack_flowF ***********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 18, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the stack flow factor required by the AIM-2 model.
C It is called each time-step since this is a function of furnace operation.
C When the flue's effective opening varies between furnace on and off states 
C this subroutine is called twice per time-step.

C INPUTS:
C    AIM2_Y  The flue leakage fraction
C    AIM2_R  The "ceiling-floor sum" leakage distribution parameter.
C    AIM2_X  The "ceiling-floor difference" leakage distribution parameter.
C    AIM2_n  The exponent of the flow-pressure relationship.
C    AIM2_Betaf  The normalized flue height.

C OUTPUTS:
C    AIM2_Fs  The stack flow factor (dimensionless).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_stack_flowF(AIM2_Y,AIM2_R,AIM2_X,AIM2_Fs)
C      USE AIM2_CALC_DATA

      IMPLICIT NONE

#include "aim2_parameters.h"      
#include "aim2_common.h"

C Type declarations for variables passed in calling statement.
      REAL AIM2_Y,AIM2_R,AIM2_X,AIM2_Fs

C Type declarations for local variables.
      REAL part1,part2,part3,Mlimit,Mfactor,Xc,Fadd


C Step 1: calculate `M' factor. This is eq 22-23 from Walker & Wilson (1990, p12).
      part1 = ( AIM2_X + AIM2_Y*(2.*AIM2_n+1.) )**2. ! Numerator of Mlimit.
      part2 = 2. - AIM2_R                            ! Denominator of Mlimit.
      Mlimit =  part1/part2                          ! Range limit on eq 22-23.
      IF( Mlimit.le.1.0 )THEN
        Mfactor = Mlimit                             ! Eq 22.
      ELSE
        Mfactor = 1.0                                ! Eq 23.
      ENDIF

C Step 2: calculate critical value of ceiling-floor difference fraction.
C This is `Xc' in eq 25 from Walker & Wilson (1990, p12).
      part1 = ( 2.*(1.-AIM2_R-AIM2_Y) ) / ( AIM2_n+1. ) ! 2nd term in eq 25.
      part2 = 2. * AIM2_Y * (AIM2_Betaf-1.)**AIM2_n     ! 3rd term in eq 25.
      Xc = AIM2_R + part1 - part2                       ! Eq 25.

C Step 3: calculate additive flue function. This is `F' in eq 24 from
C Walker & Wilson (1993, p12).
      part1 = AIM2_n * AIM2_Y * (AIM2_Betaf-1.)**( (3.*AIM2_n-1.)/3. ) ! 1st term in eq 24.
      part2 = 3. * (Xc-AIM2_X)**2. * AIM2_R**(1.-AIM2_n) ! Num 2nd term in bracks in eq 24.
      part3 = 2. * (AIM2_Betaf+1.)                       ! Den 2nd term in bracks in eq 24.
      Fadd = part1 * ( 1. - part2/part3 )                ! Eq 24.

C Step 4: calculate the stack flow factor using eq 21 from Walker & Wilson (1990, p11).
C Note that this reduces to Walker & Wilson eq 9 when there is no flue.
      part1 = ( 1. + AIM2_n*AIM2_R ) / ( AIM2_n + 1. )   ! 1st term, left brackets in eq 21.
      part2 = ( 0.5 - 0.5*Mfactor**1.25 )**(AIM2_n+1.)   ! 1st term, right bracks in eq 21.
      AIM2_Fs = part1*part2 + Fadd


      RETURN
      END


C ********************************************************************************
C ************************** AIM2_stack_pressure *********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 24, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the stack effect reference pressure required
C by the AIM-2 model. It is called each time-step since this is a function of
C the indoor and outdoor temperatures. When the flue's effective opening varies
C between furnace on and off states this subroutine is called twice per time-step.

C INPUTS:
C    TFA(i)  Air-point temperature (oC) of zone `i' at future time-row
C            (standard ESP-r solution variable).
C    TF  Future value of outdoor temperature in oC (standard ESP-r variable).
C    gravity  Gravitational acceleration (m/s^2).
C    AIM2_eaves_height  Height of building eaves (m).
C    AIM2_ref_zone  Number of zone whose temp used as reference for infil calcs. 

C OUTPUTS:
C    AIM2_Ps  The stack effect reference pressure (Pa).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_stack_pressure(AIM2_Ps)
C      USE AIM2_INPUT_DATA
C      USE AIM2_CALC_DATA

      IMPLICIT NONE

#include "building.h"
#include "aim2_parameters.h"      
#include "aim2_common.h"


      COMMON/FVALA/TFA(MCOM),QFA(MCOM)

      REAL TFA,QFA

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF, HP, HF

C Type declarations for variables passed in calling statement.
      REAL AIM2_Ps,AIRDEN

C Type declarations for local variables.
      REAL rho,TinK,ToutK

C Calculate the outdoor air density. This is evaluated using the ideal gas law
C (using a function from the CFD domain) evaluated at the `future' outdoor temperature.
      rho = AIRDEN(TF)

C Set the outdoor temperature in K.
      ToutK = TF + 273.15

C Determine the indoor temperature in K. This is taken as the `future' temperature of
C the zone defined to be the reference for the infiltration calculations. Consequently,
C it is the temperature of that zone one time-step in arrears.
      TinK = TFA(AIM2_ref_zone) + 273.15

C Now calculate the stack effect reference pressure (Pa) using eq 8 from
C Walker & Wilson (1990). The absolute value of the temperature difference
C is used here. Otherwise negative stack pressures result when it is warmer
C outdoors than indoors; this subsequently would causes the AIM-2 model to
C fail when the stack pressure is raised to the flow exponent. Although air
C flow is reversed when the outside is warmer, the logic of the AIM-2 model
C should still hold. Nothwithstanding, 1) this occurs quite seldomly and
C has minimal impact on heating loads; 2) sensitivity tests comparing this
C approach with one where the stack  pressure was set to zero when it was
C warmer outside produced nearly identical results.
      AIM2_Ps = rho * gravity * AIM2_eaves_height *
     &          abs(TinK-ToutK) / TinK


      RETURN
      END


C ********************************************************************************
C **************************** AIM2_stack_infil **********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 24, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the infiltration air flow induced by the stack effect
C using the AIM-2 model. It is called each time-step since this is a function of
C the indoor and outdoor temperatures. When the flue's effective opening varies
C between furnace on and off states this subroutine is called twice per time-step.

C INPUTS:
C    AIM2_Ctotal  The building's total leakage coefficient (m^3/s/Pa^1/n)
C    AIM2_Fs  The stack flow factor (dimensionless).
C    AIM2_Ps  The stack effect reference pressure (Pa).
C    AIM2_n  The exponent of the flow-pressure relationship (dimensionless).

C OUTPUTS:
C    AIM2_Qstack  The air flow induced by the stack effect (m^3/s).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_stack_infil(AIM2_Ctotal,AIM2_Fs,AIM2_Ps,
     &                            AIM2_Qstack)
C      USE AIM2_CALC_DATA

      IMPLICIT NONE
#include "aim2_parameters.h"      
#include "aim2_common.h"


C Declarations for variables passed in calling statement.
      REAL AIM2_Ctotal,AIM2_Fs,AIM2_Ps,AIM2_Qstack

C Calculate the air flow (m^3/s) induced by the stack effect using eq 7 from
C Walker & Wilson (1990, p7). This is actually what the air flow would be in
C the absence of wind and mechanical effects. It will be combined later with
C these other effects to calculate the actual infiltration to the house.
      AIM2_Qstack = AIM2_Ctotal * AIM2_Fs * AIM2_Ps**AIM2_n


      RETURN
      END


C ********************************************************************************
C *************************** AIM2_wind_flowF ***********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 24, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the wind flow factor required by the AIM-2 model.
C It is called each time-step since this is a function of furnace operation.
C When the flue's effective opening varies between furnace on and off states 
C this subroutine is called twice per time-step.

C INPUTS:
C    AIM2_Y  The flue leakage fraction
C    AIM2_R  The "ceiling-floor sum" leakage distribution parameter.
C    AIM2_X  The "ceiling-floor difference" leakage distribution parameter.
C    AIM2_n  The exponent of the flow-pressure relationship.

C OUTPUTS:
C    AIM2_Fw  The wind flow factor (dimensionless).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_wind_flowF(AIM2_Y,AIM2_R,AIM2_X,AIM2_Fw)
C      USE AIM2_CALC_DATA

      IMPLICIT NONE
#include "aim2_parameters.h"      
#include "aim2_common.h"

C Type declarations for variables passed in calling statement.
      REAL AIM2_Y,AIM2_R,AIM2_X,AIM2_Fw

C Type declarations for local variables.
      REAL     S,part1,part2,part3
      INTEGER  foundation_type, i, largest
      REAL     area_temp, X_S

C Determine predominant foundation type 
C If the user has input multiple foundations, then use the one with the largest area

      IF (H2K_found_num.eq.0) THEN        ! No foundation input by user.
        foundation_type = 3               ! Assume slab-on-grade for purposes of calculating infiltration.
      ELSEIF (H2K_found_num.eq.1) THEN    ! One foundation input by user.
        foundation_type= H2K_found_type(1)! Set `foundation_type'.
      ELSE                                ! User has input multiple foundations: find largest.
        area_temp = 0.
        do i=1,H2K_found_num
          if( H2K_found_area(i).gt.area_temp ) then
            largest = i
            area_temp = H2K_found_area(i)
          endif
        enddo
        foundation_type=H2K_found_type(largest)

      ENDIF


C  Calculate the wind flow factor depending on the foundation type

      IF ( foundation_type .ge.4 .and. foundation_type .le.6 ) THEN  ! Crawl spaces
      
C       Calculate X_S according to Equation 24 from Walker & Wilson (1998)
        X_S = (1-AIM2_R)/5. - 1.5*AIM2_Y
        
C       Calculate the wind flow factor using Equation 20 from Walker & Wilson (1998)
        part1 = 1. - ( (AIM2_X-X_S)/(2.-AIM2_R) )**1.5  ! Term X_star
        part2 = 1. - AIM2_R*(AIM2_n/2. +0.2)            ! Term R_star
        part3 = 1. - AIM2_Y/4.                          ! Term Y_star

        AIM2_Fw=0.19*(2.-AIM2_n)*part1*part2*part3
        
      ELSE    ! Basements or slab-on-grade 
C       Calculate the `S' factor. This is the bottom part of eq 26 from
C       Walker & Wilson (1990, p13).
        S = ( AIM2_X+AIM2_R+2.*AIM2_Y ) / 2.

C       Calculate the wind flow factor using eq 26 from Walker & Wilson (1990, p13).
C       Note that this reduces to Walker & Wilson eq 16 when there is no flue.
        part1 = 0.19 * (2.-AIM2_n)                        ! Term preceding large bracks in eq26.
        part2 = 1. - ( (AIM2_X+AIM2_R)/2. )**(1.5-AIM2_Y) ! Term in large bracks in eq26.
        part3 = AIM2_Y * ( S-2.*AIM2_Y*S**4. ) / 4.       ! Last term in eq26.
        AIM2_Fw = part1*part2 - part3

      ENDIF


      RETURN
      END


C ********************************************************************************
C *************************** AIM2_wind_shelter **********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 24, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the wind local wind shelter coefficient required
C by the AIM-2 model. It is called each time-step since this is a function of
C furnace operation. When the flue's effective opening varies between furnace
C on and off states this subroutine is called twice per time-step.

C INPUTS:
C    AIM2_Y  The flue leakage fraction
C    AIM2_Swo  The shelter coefficients for the walls.
C    AIM2_Swflue  The shelter coefficients for the flue.

C OUTPUTS:
C    AIM2_Sw  The local wind shelter coefficient (dimensionless).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_wind_shelter(AIM2_Y,AIM2_Sw)
C      USE AIM2_CALC_DATA

      IMPLICIT NONE
#include "aim2_parameters.h"      
#include "aim2_common.h"


C Type declarations for variables passed in calling statement.
      REAL AIM2_Y,AIM2_Sw

C Calculate the local wind shelter coefficient using eq 29 from
C Walker & Wilson (1990, p17).
      AIM2_Sw = AIM2_Swo*(1.-AIM2_Y) + AIM2_Swflue*1.5*AIM2_Y


      RETURN
      END


C ********************************************************************************
C ************************** AIM2_wind_pressure **********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 24, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the wind effect reference pressure required
C by the AIM-2 model. It is called each time-step since this is a function of
C the indoor and outdoor temperatures. When the flue's effective opening varies
C between furnace on and off states this subroutine is called twice per time-step.

C INPUTS:
C    TF  The future value of outdoor temperature in oC (standard ESP-r variable).
C    AIM2_Sw  The local wind shelter coefficient (dimensionless).
C    VF  The future wind speed magnitude in m/s (standard ESP-r variable).
C    AIM2_Ct  The wind speed correction factor (dimensionless).

C OUTPUTS:
C    AIM2_Pw  The wind effect reference pressure (Pa).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_wind_pressure(AIM2_Sw,AIM2_Pw)
C      USE AIM2_CALC_DATA

      IMPLICIT NONE

#include "aim2_parameters.h"      
#include "aim2_common.h"

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF, HP, HF

C Type declarations for variables passed in calling statement.
      REAL AIM2_Sw,AIM2_Pw,AIRDEN

C Type declarations for local variables.
      REAL rho,Ue

C Calculate the outdoor air density. This is evaluated using the ideal gas law
C (using a function from the CFD domain) evaluated at the `future' outdoor temperature.
      rho = AIRDEN(TF)

C Calculate the wind speed at the site using eq 27 from Bradley (1993, p8).
C This is the Wieringa (1986) model. This relates the wind speed at the site to
C the wind speed at the weather station (i.e. from the weather file). As AIM2_Ct
C is time-invariant it was calculate prior to commencing the time-step simulation.
      Ue = AIM2_Ct*VF

C Now calculate the wind reference pressure (Pa) using eq 15 from
C Walker & Wilson (1990, p9).
      AIM2_Pw = rho * 0.5 * (AIM2_Sw*Ue)**2.


      RETURN
      END


C ********************************************************************************
C ***************************** AIM2_wind_infil **********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 24, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the infiltration air flow induced by the wind effect
C using the AIM-2 model. It is called each time-step since this is a function of
C the indoor and outdoor temperatures. When the flue's effective opening varies
C between furnace on and off states this subroutine is called twice per time-step.

C INPUTS:
C    AIM2_Ctotal  The building's total leakage coefficient (m^3/s/Pa^1/n)
C    AIM2_Fw  The wind flow factor (dimensionless).
C    AIM2_Pw  The wind effect reference pressure (Pa).
C    AIM2_n  The exponent of the flow-pressure relationship.

C OUTPUTS:
C    AIM2_Qwind  The air flow induced by the wind effect (m^3/s).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_wind_infil(AIM2_Ctotal,AIM2_Fw,AIM2_Pw,AIM2_Qwind)
C      USE AIM2_CALC_DATA

      IMPLICIT NONE
#include "aim2_parameters.h"
#include "aim2_common.h"

C Declarations for variables passed in calling statement.
      REAL AIM2_Ctotal,AIM2_Fw,AIM2_Pw,AIM2_Qwind

C Calculate the air flow (m^3/s) induced by the wind effect using eq 14 from
C Walker & Wilson (1990, p9). This is actually what the air flow would be in
C the absence of stack and mechanical effects. It will be combined later with
C these other effects to calculate the actual infiltration to the house.
      AIM2_Qwind = AIM2_Ctotal * AIM2_Fw * AIM2_Pw**AIM2_n


      RETURN
      END


C ********************************************************************************
C *************************** AIM2_stack_plus_wind *******************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 24, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the infiltration air flow induced by both stack and
C wind effects using the AIM-2 model. It considers the interaction of the two
c factors. It is called each time-step since this is a function of
C the indoor and outdoor temperatures. When the flue's effective opening varies
C between furnace on and off states this subroutine is called twice per time-step.

C INPUTS:
C    AIM2_Qstack  The air flow induced by the stack effect (m^3/s).
C    AIM2_Qwind  The air flow induced by the wind effect (m^3/s).
C    AIM2_n  The exponent of the flow-pressure relationship.
C    AIM2_B1  Interaction coefficient between wind and stack effects
C             (constant and dimensionless).

C OUTPUTS:
C    AIM2_natural  The total natural infiltration rate induced by the
C                  interaction of stack and wind effects (m^3/s).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_stack_plus_wind(AIM2_Qstack,AIM2_Qwind,
     &                                AIM2_natural)
C      USE AIM2_INPUT_DATA
C      USE AIM2_CALC_DATA

      IMPLICIT NONE
#include "aim2_parameters.h"      
#include "aim2_common.h"


C Declarations for variables passed in calling statement.
      REAL AIM2_Qstack,AIM2_Qwind,AIM2_natural

C Declarations for local variables.
      REAL part1,part2,part3

C Calculate the infiltration air flow (m^3/s) induced by the interaction of
C stack and wind effects using eq 6 from Walker & Wilson (1990, p4). This is
C actually what the air flow would be in the absence of mechanical effects.
C It will be combined later with the mechanical ventilation to calculate the
C actual air flow from outdoors to the house.
      part1 = AIM2_Qstack**(1./AIM2_n)                         ! Stack portion.
      part2 = AIM2_Qwind**(1./AIM2_n)                          ! Wind portion.
      part3 = AIM2_B1 * (AIM2_Qstack*AIM2_Qwind)**(0.5/AIM2_n) ! Interaction portion.
      AIM2_natural = ( part1 + part2 + part3 )**AIM2_n         ! Superposition portions.


      RETURN
      END


C ********************************************************************************
C ****************************** AIM2_mechanical *********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 25, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the unbalanced mechanical ventilation rate in the house.
C Any unbalanced mechanical flow will interact with the natural infiltration.
C This subroutine is called each time-step. When the flue's effective opening varies
C between furnace on and off states this subroutine is called twice per time-step.

C INPUTS:
C    ????

C OUTPUTS:
C    AIM2_unbal_mech  The unbalanced mechanical ventilation rate (m^3/s).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_mechanical(AIM2_unbal_mech)

      IMPLICIT NONE

C Declarations for variables passed in calling statement.
      REAL AIM2_unbal_mech


C Calculate the unbalanced mechanical ventilation. This must be sensitive to the
C amount of combusion air drawn by the furnace, and the operating state of the HRV
C and any exhaust fans. This functionality will be implemented once these
C other models are incorporated into ESP-r. For now it is assumed that the ventilation
C is balanced. Refer to Bradley (1993, p8-9).
      AIM2_unbal_mech = 0.


      RETURN
      END


C ********************************************************************************
C ***************************** AIM2_comb_nat_mech *******************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 25, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the net air flow rate from outdoors caused by the
C combined influence of natural infiltration and the unbalanced portion of the
C mechanical ventilation. This subroutine is called each time-step. When the
C flue's effective opening varies between furnace on and off states this subroutine
C is called twice per time-step.

C INPUTS:
C    AIM2_unbal_mech  The unbalanced mechanical ventilation rate (m^3/s).
C    AIM2_natural  The total natural infiltration rate induced by the
C                  interaction of stack and wind effects (m^3/s).

C OUTPUTS:
C    AIM2_total  The net air flow rate from outdoors caused by the combined influence
C                of natural infiltration and the unbalanced portion of the mechanical
C                ventilation (m^3/s).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_comb_nat_mech(AIM2_natural,AIM2_unbal_mech,
     &                              AIM2_total)
C      USE AIM2_CALC_DATA

      IMPLICIT NONE
#include "aim2_parameters.h"      
#include "aim2_common.h"


C Declarations for variables passed in calling statement.
      REAL AIM2_natural,AIM2_unbal_mech,AIM2_total

C Declarations for local variables.
      REAL part1,part2,part3

C Calculate the net air flow rate from outdoors caused by the combined influence
C of natural infiltration and the unbalanced portion of the mechanical ventilation.
C This will subsequently be treated as `infiltration' within ESP-r's zonal energy balances.
C Note that the balanced portion of the mechanical ventilation is in addition to this
C and is calculated elsewhere.
C Calculate the net air flow using the Kiel-Wilson approach (m^3/s).
      part1 = AIM2_natural**(1./AIM2_n)                 ! Natural infiltration portion.
      part2 = ( 0.5*AIM2_unbal_mech )**(1./AIM2_n)      ! First part of fan.
      part3 = 0.5*AIM2_unbal_mech                       ! Second part of fan.
      AIM2_total = ( part1 + part2 )**AIM2_n + part3    ! Superposition portions.


      RETURN
      END


C ********************************************************************************
C ******************************* AIM2_sumover_DT ********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 25, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the average air flow rate from outdoors over the
C time-step when separate calculations are performed for furnace on and off cycles.
C This subroutine is called twice each time-step but only when the flue's
C effective opening varies between furnace on and off states.

C INPUTS:
C    AIM2_total  The net air flow rate from outdoors (m^3/s) for either the
C                furnace on or off state.
C    inf_regime  Incates whether `AIM2_total' for furnace on (=1) or off (=2) state.
C    frac_ON  The fraction of time the furnace ran the previous simulation time-step.
C    frac_OFF  The fraction of time the furnace did not run the previous simulation
C              time-step.

C OUTPUTS:
C    AIM2_total_sum  A running sum of the average air flow rate from outdoors
C                    over the time-step.
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_sumover_DT(inf_regime,AIM2_total,frac_ON,
     &                           frac_OFF,AIM2_total_sum)

      IMPLICIT NONE

C Declarations for variables passed in calling statement.
      REAL AIM2_total,frac_ON,frac_OFF,AIM2_total_sum
      INTEGER inf_regime

C Determine whether this subroutine was called for the furnace ON or OFF state.
C Then calculate the contribution to the average air flow rate over the time-step.
      if(inf_regime.eq.1) AIM2_total_sum = AIM2_total*frac_ON   ! Weighted air flow from ON cycle.
      if(inf_regime.eq.2) AIM2_total_sum =                      ! Add weighted air flow from OFF cycle.
     &                    AIM2_total_sum + AIM2_total*frac_OFF


      RETURN
      END


C ********************************************************************************
C ******************************* AIM2_split2zones *******************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 26, 2000
C Copyright 2000: NRCan Buildings Group

C The AIM-2 model calculates the infiltration rate for the whole house. As such, it
C is called only for the model's first zone. This subroutine takes the total leakage
C rate and apportions it to individual zones. This is necessary as infiltration
C appears as a term in each zone's matrix of energy balance equations. This subroutine
C is called each time-step.

C INPUTS:
C    AIM2_total  The net air flow rate from outdoors (m^3/s) averaged over the
C                time-step.
C    NCOMP  The number of zones in the building model.
C    AIM2_num_zones_infil  The number of zones that receive infiltration.
C    AIM2_zone_infil(i)  The zone index of the i'th zone that receives infiltration.
C    AIM2_basement_zone  The zone index of the basement zone.
C    VOL(i)  The volume (m^3) of i'th zone.
C    AIM2_house_vol  Total volume of zones that are internal to house (m^3).

C OUTPUTS:
C    AIM2_infil_to_zones(i)  The infiltration rate (m^3/s) apportioned to zone `i'.
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_split2zones(AIM2_total)
C      USE AIM2_INPUT_DATA
C      USE AIM2_CALC_DATA

      IMPLICIT NONE

#include "building.h"
#include "geometry.h"
#include "aim2_parameters.h"      
#include "aim2_common.h"

      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

C Declarations for variables passed in calling statement.
      REAL AIM2_total

C Declarations for local variables.
      INTEGER ICOMP
      INTEGER i

C Apportion infiltration to each zone. For now just distribute the air flow
C in proportion to each room's volume. The basement should be treated somewhat
C differently in the future.
      DO ICOMP=1,NCOMP                    ! Examine each zone in the model.
        AIM2_infil_to_zones(ICOMP) = 0.   ! Initialize infil to zone.

        do i=1,AIM2_num_zones_infil                             ! Check if there is infil to this zone.
c Note: Not including the crawl space BB
          if( AIM2_zone_infil(i) .eq. ICOMP .and. 
     &          ICOMP .ne. AIM2_CrawlSpace_zone )then
            AIM2_infil_to_zones(ICOMP) =                        ! There is infil to this zone:
     &                   VOL(ICOMP)/AIM2_house_vol * AIM2_total ! calc air flow rate.
            exit                                                ! Processing of this zone complete.
          endif
        end do

      END DO


      RETURN
      END


C ********************************************************************************
C ************************ AIM2_airflow_to_conduct *******************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 26, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine expresses the infiltration air flow rate to a zone in terms of a
C conductance (W/K) between the indoor-outdoor temperature difference. This is
C required to express the AIM-2 predicted infiltration in a form that can be
C introduced directly into ESP-r's matrix of zone heat balance equations.
C It is called once for each zone within each time-step.

C INPUTS:
C    ICOMP  The zone under consideration.
C    TF  The future value of outdoor temperature in oC (standard ESP-r variable).
C    AIM2_infil_to_zones(i)  The infiltration rate (m^3/s) to zone `i' (from MODULE AIM2_CALC_DATA).

C OUTPUTS:
C    ZONE_infil_cond  The zone's infiltration, expressed in terms of
C                     a conductance (W/K).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_airflow_to_conduct(ICOMP,ZONE_infil_cond)
C      USE AIM2_CALC_DATA

      IMPLICIT NONE
#include "aim2_parameters.h"      
#include "aim2_common.h"
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF, HP, HF

C Type declarations for variables passed in calling statement.
      REAL ZONE_infil_cond
      INTEGER ICOMP

C Type declarations for local variables.
      REAL rho,Cp,AIRDEN

C Calculate the outdoor air density. This is evaluated using the ideal gas law
C (using a function from the CFD domain) evaluated at the `future' outdoor temperature.
C The outdoor temperature is used because the air is flowing from outdoors to in.
      rho = AIRDEN(TF)

C Set the specific heat of air. This can be treated as a constant over a wide
C range of outdoor air temperatures (-30oC to +30oC).
      Cp = 1006. ! Incropera & Dewitt, Table A.4, p767.

C Express the infiltration air flow rate in terms of a conductance between the
C indoor-outdoor temperature difference.
      ZONE_infil_cond = AIM2_infil_to_zones(ICOMP) * rho * Cp


      RETURN
      END


C ********************************************************************************
C ******************************* AIM2_inf2attic *********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: December 3, 2001
C Copyright 2001: CETC/NRCan

C The AIM-2 model calculates the infiltration rate for the occupied portion of the house.
C The infiltration to the attic zone is treated separately in this subroutine.

C INPUTS:
C    AIM2_attic_zone   The zone number of the attic.

C OUTPUTS:
C    AIM2_infil_to_zones(i)  The infiltration rate (m^3/s) apportioned to zone `i'.
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_inf2attic
C      USE AIM2_INPUT_DATA
C      USE AIM2_CALC_DATA

      IMPLICIT NONE

#include "building.h"
#include "geometry.h"
#include "aim2_parameters.h"      
#include "aim2_common.h"

C Declarations for local variables.
      REAL attic_ach

C If user has specified that one of the zones in the model is an attic, then
C set the infiltration rate for the attic. IF an attic has not been specified,
C then `AIM2_attic_zone' will have defaulted to zero. In this case, no action
C is taken here.
      IF(AIM2_attic_zone.gt.0)THEN

C Assume the attic receives 0.5 ac/h of infiltration from the outdoors and no infiltration
C from the occupied portion of the house. This is the approach used in the HOT2000 bin
C model. Sensitivity tests showed that this has only a minor impact on the heating loads,
C although a significant impact on the zone air temperature in the attic. A more refined
C treatment should be used in the future.
        attic_ach = 0.5
        AIM2_infil_to_zones(AIM2_attic_zone) =
     &             attic_ach * VOL(AIM2_attic_zone) / 3600.


      END IF

      RETURN
      END
