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

C****************** PEM_static_template *******************************
C
C     CMP95S establishes for a plant component with plant db code 950 ie.
C     1 node (ISV>19) CHP PEM fuel cell model whether the correct number
C     of control variables have been specified, whethe the number of W
C     connections is correct and whether the connections are to the
C     correct fluid type.
      SUBROUTINE PEM_static_template(IPCOMP)
#include "plant.h"
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &     ICONDX(MPCOM,MNODEC,MPCONC)

C.....Flag to enable inter-domain iteration
      common / Plt_Elec_Net / bInter_domain_iteration
      logical bInter_domain_iteration

C     Reset variable indicating if fuel cell operating map exits - ensures that map
C     will be rebuilt during subsequent simulations.

C     Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) CHP PEM fuel cell component'
         WRITE(ITU,*) ' (db code 950)'
         NITMS=10
         WRITE (ITU,*)' ADATA ',(ADATA(IPCOMP,J), J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C     Check component has 1st connection to water.
C
      NCONS=1
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
         IPCON1=ICONDX(IPCOMP,1,IPCONC)
         IF(IPCONC.LE.NCONPN) THEN
            IF(IPCON1.EQ.0) THEN
               GOTO 990
            ELSE IF(ICONTP(IPCON1).NE.20) THEN
               GOTO 990
            END IF
         ELSE IF(IPCON1.NE.0) THEN
            GOTO 990
         END IF
 10   CONTINUE

C.....Enable inter-domain iteration
      bInter_domain_iteration = .true.

      RETURN


C     Error handling
 990  WRITE(IUOUT,*) ' CMP95S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie connection to water'
      WRITE(IUOUT,*) '             '
      STOP ' CMP95S: unresolvable error'
      END

C================= FUNCTION PEM_FCinit ====================================
C     Created by: Alex Ferguson
C     Created on: March 2001
C     Copyright:  Alex Ferguson 2001 / CETC 2003.
C----------------------------------------------------------------------
C     ABSTRACT:
C     This is a simple function that stores constant data
C     to be used in the PEFC model. It can be called at any point
C     to assign values to variables, and prevents data from being
C     specified twice.
C
C----------------------------------------------------------------------
C     REVISIONS:
C     SEPT 2003: Code was heavily revised to comply with CETC coding
C                standard and improve general readability.
C======================================================================
      REAL FUNCTION PEM_FCinit(Index)
      IMPLICIT NONE
      INTEGER INDEX             ! pointer to requested data
C---- Convert from oC to K
      IF ( INDEX .EQ.  0 ) PEM_FCinit = 273.      ! (K)
C---- Surrounding temperature
      IF ( INDEX .EQ.  1 ) PEM_FCinit = 298.      ! (K)
C---- Atmospheric pressure
      IF ( INDEX .EQ.  2 ) PEM_FCinit = 101.32    ! (kPa)
C---- Supply water temperature
      IF ( INDEX .EQ.  3 ) PEM_FCinit = 283.      ! (K)
C---- Supply Air temperature
      IF ( INDEX .EQ.  4 ) PEM_FCinit = 293.      ! (K)
C---- Supply NG temperature
      IF ( INDEX .EQ.  5 ) PEM_FCinit = 293.      ! (K)
C---- Supply Methanol temperature
      IF ( INDEX .EQ.  6 ) PEM_FCinit = 293.      ! (K)
C---- Maximum permitted water temperature
      IF ( INDEX .EQ.  7 ) PEM_FCinit = 368.      ! (K)
C---- Turn down ratio ( no longer used )
      IF ( INDEX .EQ.  8 ) PEM_FCinit = -99.      ! (-)
C---- Molar Mass of water
      IF ( INDEX .EQ.  9 ) PEM_FCinit = 18.016    ! (kg/mol)
C---- Density of water (l)
      IF ( INDEX .EQ. 10 ) PEM_FCinit = 1000.     ! (kg/m3)
C---- Molar Mass of Methanol
      IF ( INDEX .EQ. 11 ) PEM_FCinit = 32.042    ! (kg/mol)
C---- Density of water (l)
      IF ( INDEX .EQ. 12 ) PEM_FCinit = 787.      ! (kg/m3)
C---- Ratio O2/N2 in air
      IF ( INDEX .EQ. 13 ) PEM_FCinit = 79./21.   ! (kg/m3)
C---- Reference Temperature
      IF ( INDEX .EQ. 14 ) PEM_FCinit = 298.      ! (K)
C---- Reference pressure
      IF ( INDEX .EQ. 15 ) PEM_FCinit = 101.32    ! (kPa)
C---- Enthalpies of formation  kJ/kmol @ 1 atm, 298K
      IF ( INDEX .EQ. 16 ) PEM_FCinit =  -241820. ! (H2O v)
      IF ( INDEX .EQ. 17 ) PEM_FCinit =  -285830. ! (H2O l)
      IF ( INDEX .EQ. 18 ) PEM_FCinit =  -110530. ! (CO)
      IF ( INDEX .EQ. 19 ) PEM_FCinit =  -393520. ! (CO2)
      IF ( INDEX .EQ. 20 ) PEM_FCinit =        0. ! (H2)
      IF ( INDEX .EQ. 21 ) PEM_FCinit =        0. ! (O2)
      IF ( INDEX .EQ. 22 ) PEM_FCinit =        0. ! (N2)
      IF ( INDEX .EQ. 23 ) PEM_FCinit =   -74850. ! (CH4)
      IF ( INDEX .EQ. 24 ) PEM_FCinit =  -208450. ! (OCTANE)
      IF ( INDEX .EQ. 25 ) PEM_FCinit =  -200670. ! (METHANE)
C---- Entropies of formation  kJ/kmol K @ 1 atm, 298K
      IF ( INDEX .EQ. 26 ) PEM_FCinit =   188.83  ! (H2O v)
      IF ( INDEX .EQ. 27 ) PEM_FCinit =    69.62  ! (H2O l)
      IF ( INDEX .EQ. 28 ) PEM_FCinit =   197.65  ! (CO)
      IF ( INDEX .EQ. 29 ) PEM_FCinit =   213.80  ! (CO2)
      IF ( INDEX .EQ. 30 ) PEM_FCinit =   130.68  ! (H2)
      IF ( INDEX .EQ. 31 ) PEM_FCinit =   205.04  ! (O2)
      IF ( INDEX .EQ. 32 ) PEM_FCinit =   191.61  ! (N2)
      IF ( INDEX .EQ. 33 ) PEM_FCinit =   186.16  ! (CH4)
      IF ( INDEX .EQ. 34 ) PEM_FCinit =   466.73  ! (OCTANE)
      IF ( INDEX .EQ. 35 ) PEM_FCinit =   239.79  ! (METHANE)
C---- Universal gas constatn (kJ/kmol K)
      IF ( INDEX .EQ. 36 ) PEM_FCinit =    8.314  ! (kJ/kmol K)
C---- Molecular mass of Methane - used for reporting only (kg/kmol)
      IF ( INDEX .EQ. 37 ) PEM_FCinit =   16.043  ! (kg/kmol)
C---- Higher heating value of methane
      IF ( INDEX .EQ. 38 ) PEM_FCinit =    55496.  ! (kJ/kg)
C---- Lower heating value of methane
      IF ( INDEX .EQ. 39 ) PEM_FCinit =    50010.  ! (kJ/kg)
C---- Iteration tolerances
      IF ( INDEX .EQ. 40 ) PEM_FCinit =    10.0    ! temperature (K)
      IF ( INDEX .EQ. 41 ) PEM_FCinit =    0.0001 ! relative error (0->1)
C---- End function
      END

C===================== CMP95C =========================================
C     Created by: Alex Ferguson
C     Created on: March 2001
C     Copyright:  Alex Ferguson 2001 / CETC 2003.
C----------------------------------------------------------------------
C     ABSTRACT:
C     CMP95C is the ESP-r coefficient generator for the cogeneration
C     PEM fuel cell model written by Alex Ferguson.  It does the
C     following:
C
C     1. Builds a map describing the operation of the fuel cell system
C        using the subroutine POWER
C     2. Locates the current operating point in the operating map
C     3. Determines the amount of heat that can be recovered from
C        the fuel cell system using the subroutine PEM_HEAT_REC
C     4. Constructs the coefficient matrix required by ESP-r to
C        characterize the fuel cell operation.
C
C     Inputs/outputs/variable definitions follow.
C
C---------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C
C----------------------------------------------------------------------
C     REVISIONS:
C     SEPT 2003: Code was heavily revised to comply with CETC coding
C                standard and improve general readability.
C======================================================================
      SUBROUTINE PEM_coeff_gen(IPCOMP,COUT,ISTATS)
      use h3kmodule
      IMPLICIT NONE
C---- Header files req'd for ESP-r common blocks
#include "plant.h"
#include "building.h"
C=======================================================================
C     START ESP-r common blocks
C=======================================================================
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &     ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &     IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &     ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCEQU/IMPEXP,RATIMP
C--------------------------------------------------------------------------------
C     Common block assoiated with electrical network
C     (Addition by Alex Ferguson, Jan 8, 2004)
C--------------------------------------------------------------------------------
      common/elpcp/
     &     NPEL,                ! number of "electrified" plant components
     &     PFP(mpcom),          ! power factor of componet i (0->1)
     &     IPFP(mpcom),         ! integer indicating if power factor lags (-1) or leads (+1)
     &     PWRP(mpcom),         ! real power consumption of component (generation is -ive)
     &     BVOLTP(mpcom),       ! opertional voltage of component
     &     IPHP(mpcom)          ! phase component is connected to -> dummy variable

C-------------------------------------------------------------------------
C     Declare common variables (req'd for use of IMPLICIT NONE statement
C     w/FORTRAN 90 compilers
C-------------------------------------------------------------------------
      INTEGER MAXITP,ICSV,itrclp,NPCOMP,
     &     NCI,ICONTP,ICONDX,ITCF,iuout,iuin,
     &     ITRACE,IZNTRC,ITU,ITC,ICNT,NPCDAT,
     &     IPOFS1,IPOFS2,NPCON,IPC1,
     &     IPN1,IPCT,IPC2,IPN2,IHRP,IHRF,
     &     IDYP,IDYF,IDWP,IDWF,NSINC,ITS,IMPEXP,NPEL,
     &     IPFP,IPHP
      REAL PERREL,PERTMP,PERFLX,PERMFL
     &     ,CSVI,ADATA,BDATA,
     &     CDATA,CONVAR,
     &     CSVF,CSVP,PCONDR,
     &     PCONSD, TC, RATIMP,
     &     BVOLTP, PWRP, PFP
C-----------------------------------------------------------------------
C     Declare ESP-r function types
C-----------------------------------------------------------------------
      REAL SHTFLD, PEM_FCinit
C=======================================================================
C     END common blocks // START passed & local varaibes
C=======================================================================
C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------
      INTEGER IPCOMP            ! Number of FC in plant network
      INTEGER ISTATS            ! Flag indicating which plant matrix is being solved.
      REAL COUT(MPCOE)          ! Array containing matrix coefficients req'd
                                ! by ESP-r
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
C---- Environment variables
      Integer Icon1             ! pointer to incomming conn.
      Integer Inod1             ! pointer to fc model node
C---- Inputs
      integer Fuel_type         ! fuel type (1= methane, 3 = methanol)
      REAL P_Net_Max            ! max fc net power  output  (kWe)
      REAL cool_COP             ! cooling system COP
C     REAL cool_COP1                ! cooling system COP parametric coefficient 1
C     REAL cool_COP2                ! cooling system COP parametric coefficient 2
C     REAL cool_COP3                ! cooling system COP parametric coefficient 3
C     REAL cool_COP4                ! cooling system COP parametric coefficient 4
      REAL T_cell               ! Stack operating temperature (K)
      REAL dT_S_heat_rec        ! Minimum heat recovery approach temperature
      INTEGER CONTROL_TYPE      ! Flag indicating controller type
C---- Arrays
      REAL TH_cold_streams(2,2) ! Array containing temperature-enthalpy points
                                ! for cooling water (straight line w/ 2 points.
                                ! passed to subroutine PEM_heat_rec, which performs pinch
                                ! analysis
      REAL TH_hot_streams(2,12) ! Array containing temperature-enthalpy points
                                ! for cooling reformate. Passed to subrouitine
                                ! PEM_heat_rec, which performs pinch analysis
      REAL Burner_exhaust(2,7)  ! Array containing composition of the
                                ! burner exhaust. Calculated by subroutine POWER
      REAL TH_Surplus(12,2)     ! Array containing temperature-enthalpy points
                                ! for cooling reformate, calculated by subroutine POWER


      real OPERATING_MAP(500,12,12) ! Array used to hold fuel cell operational map
                                ! (results of time-invariant calcuations)
      LOGICAL MAP_BUILT         ! Flag indicating that map has been built

C---- Power demand (control data)
      REAL P_demand             ! Net power demand of fuel cell
C---- named constants
      REAL T_water_max          ! max temp of water (K)
      REAL Turn_Down            ! Turn down ratio
C---- Other variables
      logical fc_on             ! Flag for turning fuel cell on/off
      REAL MFR_Cool_Water       ! cooling water flow rate (kg/s)
      REAL T_Cool_Water         ! cooling water inlet temperature (K)
      REAL T_Stack_Outlet_Max   ! max temperature cooling water
                                ! can be heated to in fuel cell stack (K)
      REAL Cp_Cool_Water,cA     ! heat capacity of cooling water (kWt/K)
      REAL Q_Hsr_Total          ! "Heat sink resource":
                                !  Q_Hsr_Total: max amount of heat that
                                !         can be xferred into cooling
                                !         water while raising it from%
                                !         T_Cool_Water to T_water_max (kWt)
      logical Done              ! Flag used to indicate that
                                ! search through fuel cell operating
                                ! map has located the correct operating
                                ! point.
      integer ii,it,igg,iq      ! counters
      REAL P_gross              ! operating point (OP) gross power (kWe)
      REAL P_gross_1,P_gross_2  ! gross power for  interpolation bounds 1 & 2 (kWe)
      REAL P_Net                ! OP net power (kWe)
      REAL P_Net1,P_Net2        ! net-power for interpolation bounds 1 & 2 (kWe)
      REAL p_gross_max          ! maximum gross power (kWe)
      REAL W_Parasitic          ! parasitic energy use (kWe)
      REAL W_Parasitic1,W_Parasitic2 ! parasitic energy use for interpolation bounds 1 & 2 (kWe)
      REAL W_Cooling            ! cooling electricity use (kWe)
      REAL W_Cooling1,W_Cooling2 ! cooling electricity use for interpolation bounds 1 & 2
      REAL Q_Cooling            ! cooling requirement (kWt)
      REAL Q_Cooling1,Q_Cooling2 ! cooling requirement for interpolation bounds 1 & 2 (kWt)
      REAL Q_Cell               ! heat recovery from stack (kWt)
      REAL Q_Cell1,Q_Cell2      ! heat recovery from stack for interpolation bounds 1 & 2 (kWt)
      REAL Q_Space              ! total space heat recovery (kWt)
      REAL Q_Space1,Q_Space2          ! total space heat recovery for interpolation bounds 1 & 2(kWt)
      REAL fuel_flow            ! fuel flow (kmol/s)
      REAL fuel_flow1,fuel_flow2 ! fuel flow for interpolation bounds 1 & 2 (kmol/s)
      integer mode              ! flag used to indicate correct
                                ! calculation procedure for
                                ! subroutine POWER
      REAL coeff                ! temporary variable used in interpolation
      integer iseg              ! number of segments on cooling map
      integer Msize             ! size of fuel cell operating map (=<500)
      logical close             ! logical result of close-to-zero comparisons
      logical no_cooling_water            ! flag indicating that cooling water
                                ! flow is zero
      REAL SMALL                ! tolerence of close-to-zero comparisons

      LOGICAL DEBUG_ME          ! Flag for debugging features
      INTEGER iTemp             ! holding variable

      INTEGER iemodel           ! flag indicating calulation that should
                                ! be performed by EMACH function (=1 for all cases)
      REAL dummy_a, dummy_b     ! dummy variables used when calling EMACH function

C-----Save operating map, map-built flag
      SAVE operating_map, map_built

C======================================================================
C     END Declarations // START Named Constants
C======================================================================
C-----------------------------------------------------------------------
C     Max water temperature
C-----------------------------------------------------------------------
      T_water_max = PEM_FCinit(7)           ! (K)
C-----------------------------------------------------------------------
C     Size of fuel cell map (<=500!)
C-----------------------------------------------------------------------
      Msize = 500
C----------------------------------------------------------------------
C     Tolerence of close-to-zero comparisons
C----------------------------------------------------------------------
      SMALL = 1.0E-05
C======================================================================
C     END Named Constants // START Environment initialization
C======================================================================
C----------------------------------------------------------------
C     Setup connection and node pointers
C----------------------------------------------------------------
      Icon1 = ICONDX(IPCOMP,1,1) !pointer to incomming connection
      Inod1 = NPCDAT(IPCOMP,9)   !pointer to FC model node (there's
                                 !only one!)
C----------------------------------------------------------------
C     Mark node temperature and mass flow rates for iteration
C----------------------------------------------------------------
      icsv(inod1,1)=1            !flag indicating iteration on
                                 !first node temperature req'd
      csvi(inod1,1)=csvf(inod1,1)!set "initial" temperature value
                                 !"future" value calculated on last
                                 !timestep. ESP-r assumes that
                                 !iteration has converged when
                                 !these two variables are approx.
                                 !equal.
C----------------------------------------------------------------
C     Read data in from ADATA(IPCOMP, ?) - See variable declarations
C     above for units.
C----------------------------------------------------------------
C     Fuel Type
      Fuel_Type= int(ADATA(IPCOMP, 1))
C     Fuel Cell normal rated output (kWe) (NET)
      P_Net_Max =   ADATA(IPCOMP, 2)/1000.
C     Turn down ratio (-)
      Turn_Down =  ADATA(IPCOMP,3)
C     Evaproative cooling system performance (COP)
      cool_COP =   ADATA(IPCOMP, 18)
C     Cell stack operating temperature (K)
      T_cell = ADATA(IPCOMP, 36)+PEM_FCinit(0)
C     Space heat recovery ,inimum approach temperature (K)
      dT_S_heat_rec  = ADATA(IPCOMP, 38)


C======================================================================
C     END Environment initialization // START fuel cell modelling
C======================================================================
C----------------------------------------------------------------------
C     Read data from ESP-r control variable
C----------------------------------------------------------------------
      P_demand = CDATA(IPCOMP,1) * P_Net_Max ! kWe
C-----------------------------------------------------------------------
C     Check to see if demand/max power is greater than turn down ratio,
C     if so, turn fc off
C-----------------------------------------------------------------------
      IF (P_demand/P_Net_Max.LE.Turn_Down) THEN
C------- Check to see if power demand is greater than zero
         CALL eclose(P_demand, 0.0,0.1,close)
         IF ( close ) THEN
C---------- Power demand is zero. Turn FC off.
            FC_on = .FALSE.
         ELSE
C---------- Power demand is greater than zero, but less than
C---------- mimimum operating point. Turn FC off
            FC_on = .FALSE.
         ENDIF
      ELSE
C------- Fuel cell will operate
         FC_on = .TRUE.
      ENDIF
C-----------------------------------------------------------------------
C     Fuel cell is on. Get cooling water data:
C     CONVAR(conn#,1) = temperature (oC)
C     CONVAR(conn#,2) = 1st phase flow (kg/s)
C     SHTFLD(3,T) -> ESP-r function returining heat capacitance
C                   of water at temperature T.
C-----------------------------------------------------------------------
      MFR_Cool_Water = CONVAR(ICON1, 2) !cooling water mass flow rate, kg/s
      T_Cool_Water = CONVAR(ICON1,1) +PEM_FCinit(0) !cooling water temperature, oC
      Cp_Cool_Water = (SHTFLD(3,T_Cool_Water) +
     &     SHTFLD(3,T_water_max))/2000.*MFR_Cool_Water
                                !avg cooling water heat capacity
                                !between inlet and maximum water
                                !temperature (kW/oC)
C-----------------------------------------------------------------------
C     Check to see if there is cooling water flowing through
C     the fuel cell. Use ESP-r standard subroutine ECLOSE to
C     compare MFR_Cool_Water to zero, and set flag no_cooling_water
C
C     Synopsys:
C         ECLOSE( variable to be compared,
C                 value to compare to,
C                 tolerance of comparison,
C                 logical result of comparison)
C-----------------------------------------------------------------------
      CALL ECLOSE(MFR_Cool_Water,0.00,SMALL,no_cooling_water)
C-----------------------------------------------------------------------
C     Check to see which matrix is being solved.
C
C     ------------------------------------------------------------------
C     |     ISTATS     |                  Meaning
C     |----------------|------------------------------------------------
C     |       1        | Temperatre matrix is being solved. FC must be
C     |                | characterized, and first law of thermodynamics
C     |                | is applied to CV representing FC heat recovery
C     |                | facilities.
C     |----------------|------------------------------------------------
C     |       2        | First phase mass flow matrix is being solved.
C     |                | Apply conservation of mass, FC need not be
C     |                | characterized.
C     |----------------|------------------------------------------------
C     |       3        | Second phase mass flow matrix is being solved.
C     |                | Apply conservation of mass, FC need not be
C     |                | characterized.
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (ISTATS.EQ.1) THEN
C-----------------------------------------------------------------------
C     |  Temperature matrix is being solved. Begin FC characterization.
C     |
C     |  IS FC turned on?
C     |    YES -> charaterize FC
C     |    NO  -> set state variables to zero
C-----------------------------------------------------------------------
         IF ( FC_on ) then
            IF( .not. no_cooling_water) THEN
C-----------------------------------------------------------------------
C     |        COOLING water flow rate is greater than zero. Calculate
C     |        the max amount of heat that can be transferred to the
C     |        cooling water without heating it above the max permitted
C     |        water temperature (T_water_max):
C     |
C     |           T_Stack_Outlet_Max: temperature to which cooling water can be
C     |                   heated by FC stack
C     |           Cp_Cool_Water:  avg. heat capacity of water
C     |           Q_Hsr_Total:  "heat sink resource"-> max amount of heat that
C     |                   can be transferred to cooling water without
C     |                   raising its temperature above T_water_max
C-----------------------------------------------------------------------
C------------- Calculate max fc stack cooling water outlet temperature
               IF (T_cell-dT_S_heat_rec.GT.T_water_max) THEN
C---------------- Cooling water temp. rise limited by T_water_max
                  T_Stack_Outlet_Max = T_water_max
               ELSEIF (T_Cool_Water.GT.T_cell-dT_S_heat_rec)THEN
C---------------- Cooling water inlet temperature is above max outlet
C---------------- temperature. NO POTENTIAL for heat recovery in fuel cell.
                  T_Stack_Outlet_Max = T_Cool_Water
               ELSE
C---------------- Max cooling water outlet temp =
C----------------     cell temp. - min heat recovery temp diff.
                  T_Stack_Outlet_Max = T_cell-dT_S_heat_rec
               ENDIF
C-----------------------------------------------------------------------
C     |        Determine size of heat sink resources:
C     |          - Q_Hsr_Total = amount of heat req'd to raise cooling water
C     |                    from inlet temperature to maximum temperature
C-----------------------------------------------------------------------
               Q_Hsr_Total = Cp_Cool_Water * (T_Water_Max-T_Cool_Water) ! (kW)
               IF (Q_Hsr_Total.LE.0.) Q_Hsr_Total = 0.
            ENDIF
C---------------------------------------------------------------------
C     |        Start modelling of fuel cell plant. Check MAP_BUILT flag
C     |        to see if fuel cell operating map has been built. IF not,
C     |        build one on time step.
C---------------------------------------------------------------------
            If ( .NOT. MAP_BUILT )  THEN
C---------------------------------------------------------------------
C     |        Build fuel cell operating map.
C     |
C     |        For each operating point between P_gross = 0->pgroM,
C     |        the fuel cell operating map contains:
C     |           1. the gross electrical power
C     |           2. the parasitic electrical requirement (EXCLUDING
C     |              the cooling fan electrical requirement)
C     |           3. the fuel flow, and
C     |           4. a temperature-enthalpy profile describing the
C     |              hot process streams.
C     |
C     |        First, call PEMFC_POWER(...)
C     |        to determine the gross power required to produce the
C     |        rated net PEMFC_powersee note), assuming that no heat is
C     |        dumped to the space heating loop (worst-case scenario)
C     |
C     |        Note: The calculations performed by the POWER subroutine
C     |        are controlled using the mode flag:
C     |             mode = 1: maximum power calculation
C     |             mode = 2: part-load power calculation.
C     |
C     |        See sections 4.2, 4.2.2 and 4.2.3 in Ferguson (2003).
C---------------------------------------------------------------------
               mode = 1         ! <- flag for max-power calculation
C---------------------------------------------------------------------
C     |        Call POWER subroutine to determine gross power
C     |        required to deliver net power under zero space
C     |        heating load (ie aux. cooling system must deliver
C     |        all of the required heat extraction from fuel cell
C     |        system). When mode=1, POWER returns the gross power
C     |        (P_gross) required for the system to deliver a specified
C     |        net power (P_Net)
C---------------------------------------------------------------------
               CALL  PEM_Power(IPCOMP, ! component number (input)
     &              mode,       ! flag indicating which calculations to perform (input)
     &              P_gross,    ! gross power predicted by POWER (output,kW)
     &              W_Parasitic, ! Parasitic electrical use (dummy,kW)
     &              iseg,       ! # of segments in cooling map (dummy)
     &              TH_Surplus, ! cooling map array describing temperature-enthalpy (dummy)
     &                          !   profile of cooling reformate streams (dummy)
     &              Q_Cell,     ! waste heat available in fuel cell (dummy,kW)
     &              fuel_flow,  ! fuel flow (dummy, kmol/s)
     &              p_gross_max, ! maximum stack gross power output (dummy, kW)
     &              Burner_exhaust) ! array holding burner exhaust compositions (dummy)
C---------------------------------------------------------------------
C    |         Set max gross power parameter to gross power calculated
C    |         above, and set mode flag for part-load power
C    |         calculation
C---------------------------------------------------------------------
               p_gross_max = P_gross
               mode = 2
C-----------------------------------------------------------------------
C     |           Now for P_gross = 0 -> p_gross_max, call PEM_POWER(...) to determine
C     |           the parasitic electrical consumption (excluding cooling),
C     |           cell cooling load, reformate cooling load and system fuel
C     |           use. Write these values into the OPERATING_MAP array.
C     |           NOTE - when MODE=2, the inputs and outputs to POWER
C     |           are reversed. POWER returns the net power and various
C     |           operating parameters corresponding to the operating point
C     |           characterized by p_gross_max.
C-------------------------------------------------------------------------
               DO ii = 1, Msize
C---------------- Set P_gross as a ratio of ii/Msize
                  P_gross = p_gross_max*REAL(ii)/REAL(Msize)
C-----------------------------------------------------------------------
C     |           CALL POWER to characterize operating point described by P_gross
C-----------------------------------------------------------------------
                  CALL  PEM_Power(IPCOMP, ! component number (input)
     &                 mode,    ! flag indicating which calculations to perform (input)
     &                 P_gross,    ! gross power for operating (input,kW)
     &                 W_Parasitic,   ! Parasitic electrical use (output,kW)
     &                 iseg,    ! # of segments in cooling map (output)
     &                 TH_Surplus,  ! cooling map array describing temperature-enthalpy (output)
     &                          !   profile of cooling reformate streams (output)
     &                 Q_Cell,   ! waste heat available in fuel cell (output,kW)
     &                 fuel_flow,   ! fuel flow (output, kmol/s)
     &                 p_gross_max,   ! maximum stack gross power output (output, kW)
     &                 Burner_exhaust)   ! array holding burner exhaust compositions (output)
C-----------------------------------------------------------------------------------------
C     |           Use data calculated by POWER to fill fuel cell operating map at
C     |           operating location ii
C-----------------------------------------------------------------------------------------
                  OPERATING_MAP(ii,1,1)= P_gross
                  OPERATING_MAP(ii,2,1)= W_Parasitic
                  OPERATING_MAP(ii,3,1)= Q_Cell
                  OPERATING_MAP(ii,5,1)= fuel_flow
                  OPERATING_MAP(ii,8,1)= iseg
C---------------- Cooling map
                  DO igg = 1, iseg
                     OPERATING_MAP(ii,9,igg)=TH_Surplus(igg,1) !temperature
                     OPERATING_MAP(ii,10,igg)=TH_Surplus(igg,2) !enthalpy
                  ENDDO
C---------------- Burner exhaust compositions
                  DO igg = 1,7
                     OPERATING_MAP(ii,11,igg) = Burner_exhaust(1,igg) ! HELP
                     OPERATING_MAP(ii,12,igg) = Burner_exhaust(2,igg) ! aux fuel use
                  ENDDO
               ENDDO
C------------- Set flag to ensure operating map is not built again
               MAP_BUILT = .TRUE.
C----------------------------------------------------------------------
C              Fuel cell operating map has been built.
C----------------------------------------------------------------------
            ENDIF
C----------------------------------------------------------------------
C     |        Begin search for fuel cell operating point that:
C     |         - satisfies the electrical power demand (P_demand), if
C     |           possible.
C     |         - minimizes the gross electrical power produced (P_gross)
C     |
C     |        Overview:
C     |        1. Take two data points (ii, ii+1) from operating map OPERATING_MAP.
C     |        2. For each operating point, determine:
C     |              a. how much heat stack transfers to cooling water
C     |              b. how much heat auxiliary cooling system must extract
C     |              c. resulting fan power requirements
C     |              d. resulting net power production (P_Net)
C     |        3. If P_Net(ii) <= P_demand <= P_Net(ii+1), the actual operating
C     |           point lies between ii and ii+1. Interpolate for the
C     |           correct operating point. Otherwise, set ii=ii+1, and
C     |           perform steps 1-3 again.
C     |        4. if the end of the operating map is reached (ie ii=Msize)
C     |           and the operating point has not been found, the power demand
C     |           exceeds thes system's capacity. Use operating point ii=Msize.
C     |
C     |        This procedure is described extensively Section 4.3,
C     |        in Ferguson (2003).
C-------------------------------------------------------------------------
C---------- Flag used to determine if operating point has been determined
            Done = .FALSE.
C-------------------------------------------------------------------------
C     |     ii is a counter that determines where the search loop is in the
C     |     operating map ( OPERATING_MAP(ii,ATRIBUTE,location) )
C-------------------------------------------------------------------------
            ii = 1
C-------------------------------------------------------------------------
C     |     Loop through operating map as long as idone = 0.
C-------------------------------------------------------------------------
C               IF(DEBUG_ME) print*, ' - Initiating map search'
            DO WHILE (.NOT. DONE)
C-------------------------------------------------------------------------
C     |        Check to see if search has reached end of operaiting map
C-------------------------------------------------------------------------
               IF(ii.eq.Msize) THEN
C-------------------------------------------------------------------------
C     |           System will produce maximum power, & demand will not be met.
C     |           Get data for point ii.
C----------------------------------------------------------------------------------
                  P_gross = OPERATING_MAP(ii,1,1) !Gross power
                  W_Parasitic = OPERATING_MAP(ii,2,1) !Parasitic energy use
                  Q_Cell = OPERATING_MAP(ii,3,1) !Cell waste heat
                  fuel_flow = OPERATING_MAP(ii,5,1) ! fuel flow
                  iseg = OPERATING_MAP(ii,8,1) !# of segments on cooling map
C---------------- Fill reformate cooling array
                  DO iq = 1, iseg
                     TH_hot_streams(1,iq) = OPERATING_MAP(ii,9,iq) !temperature
                     TH_hot_streams(2,iq) = OPERATING_MAP(ii,10,iq) !enthalpy
                  ENDDO
C---------------- Fill burner exhaust array
                  DO iq = 1,7
                     Burner_exhaust(1,iq) = OPERATING_MAP(ii,11,iq) ! HELP
                     Burner_exhaust(2,iq) = OPERATING_MAP(ii,12,iq) ! AUX burner
                  ENDDO
C--------------------------------------------------------------------------------
C     |            Call subroutine PEM_HEAT_REC to determine the amount of heat
C     |            that can be transferred from the fuel cell system to the
C     |            space heating loop.
C     |            Summary:
C     |            Inputs:
C     |              T_cell - cell temperature
C     |              T_cool_water    - water inlet temperature
C     |              Cp_cool_water  - water heat capacitance (Cp*mfr)
C     |              iseg   - # segments in array TH_hot_streams
C     |              TH_cold_streams   - water temperature enthalpy curve
C     |              TH_hot_streams   - truncated hot side profile
C     |              dT_S_heat_rec  - minimum space heat recovery approach T
C     |              Burner_exhaust  - array containing exhaust gas compisitions
C     |              Fuel_type   - integer flag indicating fuel type
C     |            Outputs:
C     |              Q_Space1  - Heat delivered from the fuel cell system to the water
C     |              Q_Cooling1  - Fuel cell system cooling load.
C---------------------------------------------------------------------------------
                  CALL PEM_HEAT_REC(
     &                 T_water_max,
     &                 T_cell,
     &                 T_cool_water,
     &                 Cp_cool_water,
     &                 iseg,
     &                 TH_cold_streams,
     &                 TH_hot_streams,
     &                 dT_S_heat_rec,
     &                 Burner_exhaust,
     &                 Fuel_type,
     &                 Q_Space,
     &                 Q_Cooling,
     &                 DEBUG_ME
     &                 )
C---------------------------------------------------------------------------------
C     |           If recovered heat exceeds total heat sink resource,
C     |           set to resource size:
C---------------------------------------------------------------------------------
                  IF (Q_Space.GT.Q_Hsr_Total) Q_Space=Q_Hsr_Total
C---------------------------------------------------------------------------------
C     |           If space heat is less than zero, set to zero
C---------------------------------------------------------------------------------
                  IF (Q_Space.LT.0.) Q_Space = 0.
C---------------------------------------------------------------------------------
C     |           Calculate power required by cooling equipment
C     |           (Equation 4.3.4, Ferguson 2003.)
C---------------------------------------------------------------------------------
                  W_Cooling = (Q_Cooling)/cool_COP
C---------------------------------------------------------------------------------
C     |           Calculate net power
C     |           Net = gross - parasitic losses - fan power requiremments
C     |           (Equation 4.3.5, Ferguson 2003.)
C---------------------------------------------------------------------------------
                  P_Net = P_gross - W_Parasitic - W_Cooling
C---------------- Set iDone flag to indicate end of search
                  Done = .TRUE.
               ELSE
C----------------------------------------------------------------------
C     |           Search has not reached end of map. Check to see
C     |           if operating point might be between ii and ii+1:
C     |           If: P_gross(ii+1)-W_Parasitic(ii+1) is not greater than
C     |               demand, this can't be the operating point &
C     |               there's no point in going further
C----------------------------------------------------------------------
C                  IF(DEBUG_ME) THEN
C                     print*, ' - Bounds'
C                     print*, '     P_NetC=', P_demand
C                     print*, '     OPERATING_MAP(',ii,',1,1)=',
C     &                    OPERATING_MAP(ii,1,1)
C                     print*, '     OPERATING_MAP(',ii+1,',1,1)=',
C     &                    OPERATING_MAP(ii+1,1,1)
C                  ENDIF
                  IF ((OPERATING_MAP(ii+1,1,1)-OPERATING_MAP(ii+1,2,1))
     &                 .LT.P_demand) THEN
C------------------- The operating point isn't between ii and ii+1. Do nothing.
                  ELSE
C------------------- The operating point might be between ii and ii+1.
C----------------------------------------------------------------------------------
C     |              Get data for point ii.
C----------------------------------------------------------------------------------
                     P_gross_1 = OPERATING_MAP(ii,1,1) !Gross power
                     W_Parasitic1 = OPERATING_MAP(ii,2,1) !Parasitic energy use
                     Q_Cell1 = OPERATING_MAP(ii,3,1) !Cell waste heat
                     fuel_flow1 = OPERATING_MAP(ii,5,1) ! fuel flow
                     iseg = OPERATING_MAP(ii,8,1) !# of segments on cooling map
C------------------- Fill reformate cooling array
                     DO iq = 1, iseg
                        TH_hot_streams(1,iq) = OPERATING_MAP(ii,9,iq) !temperature
                        TH_hot_streams(2,iq) = OPERATING_MAP(ii,10,iq) !enthalpy
                     ENDDO
C------------------- Fill burner exhaust array
                     DO iq = 1,7
                        Burner_exhaust(1,iq) = OPERATING_MAP(ii,11,iq) ! HELP
                        Burner_exhaust(2,iq) = OPERATING_MAP(ii,12,iq) ! AUX burner
                     ENDDO
C--------------------------------------------------------------------------------
C     |              Call subroutine PEM_HEAT_REC to determine the amount of heat
C     |              that can be transferred from the fuel cell system to the
C     |              space heating loop.
C     |              Summary:
C     |              Inputs:
C     |                T_cell - cell temperature
C     |                T_cool_water    - water inlet temperature
C     |                Cp_cool_water  - water heat capacitance (Cp*mfr)
C     |                iseg   - # segments in array TH_hot_streams
C     |                TH_cold_streams   - water temperature enthalpy curve
C     |                TH_hot_streams   - truncated hot side profile
C     |                dT_S_heat_rec  - minimum space heat recovery approach T
C     |                Burner_exhaust  - array containing exhaust gas compisitions
C     |                Fuel_type   - integer flag indicating fuel type
C     |              Outputs:
C     |                Q_Space1  - Heat delivered from the fuel cell system to the water
C     |                Q_Cooling1  - Fuel cell system cooling load.
C---------------------------------------------------------------------------------
                     CALL PEM_HEAT_REC(
     &                    T_water_max,
     &                    T_cell,
     &                    T_cool_water,
     &                    Cp_cool_water,
     &                    iseg,
     &                    TH_cold_streams,TH_hot_streams,
     &                    dT_S_heat_rec,
     &                    Burner_exhaust,
     &                    Fuel_type,
     &                    Q_Space1,
     &                    Q_Cooling1,
     &                    DEBUG_ME)
C---------------------------------------------------------------------------------
C     |              If recovered heat exceeds total heat sink resource,
C     |              set to resource size:
C---------------------------------------------------------------------------------
                     IF (Q_Space1.GT.Q_Hsr_Total) Q_Space1=Q_Hsr_Total
C---------------------------------------------------------------------------------
C     |              If space heat is less than zero, set to zero
C---------------------------------------------------------------------------------
                     IF (Q_Space1.LT.0.) Q_Space1 = 0.
C---------------------------------------------------------------------------------
C     |              Calculate power required by cooling equipment
C     |              (Equation 4.3.4, Ferguson 2003.)
C---------------------------------------------------------------------------------
                     W_Cooling1 = (Q_Cooling1)/cool_COP ! (kW)
C---------------------------------------------------------------------------------
C     |              Calculate net power
C     |              Net = gross - parasitic losses - fan power requiremments
C     |              (Equation 4.3.5, Ferguson 2003.)
C---------------------------------------------------------------------------------
                     P_Net1 = P_gross_1 - W_Parasitic1 - W_Cooling1  ! (kW)
C----------------------------------------------------------------------------------
C     |              Get data for point ii+1.
C----------------------------------------------------------------------------------
                     P_gross_2 = OPERATING_MAP(ii+1,1,1) !Gross power
                     W_Parasitic2 = OPERATING_MAP(ii+1,2,1) !Parasitic energy use
                     Q_Cell2 = OPERATING_MAP(ii+1,3,1) !Cell waste heat
                     fuel_flow2 = OPERATING_MAP(ii+1,5,1) !fuel flow
                     iseg = OPERATING_MAP(ii+1,8,1) !# of segments on cooling map
C------------------- Fill reformate cooling array
                     DO iq = 1, iseg
                        TH_hot_streams(1,iq) = OPERATING_MAP(ii+1,9,iq) !temperature
                        TH_hot_streams(2,iq) = OPERATING_MAP(ii+1,10,iq) !enthalpy
                     ENDDO
C------------------- Fill burner exhaust array
                     DO iq = 1,7
                        Burner_exhaust(1,iq) = OPERATING_MAP(ii+1,11,iq) ! HELP
                        Burner_exhaust(2,iq) = OPERATING_MAP(ii+1,12,iq) ! AUX burner
                     ENDDO
C--------------------------------------------------------------------------------
C     |              Call subroutine PEM_HEAT_REC to determine the amount of heat
C     |              that can be transferred from the fuel cell system to the
C     |              space heating loop.
C     |              Summary:
C     |              Inputs:
C     |                T_cell - cell temperature
C     |                T_cool_water    - water inlet temperature
C     |                Cp_cool_water  - water heat capacitance (Cp*mfr)
C     |                iseg   - # segments in array TH_hot_streams
C     |                TH_cold_streams   - water temperature enthalpy curve
C     |                TH_hot_streams   - truncated hot side profile
C     |                dT_S_heat_rec  - minimum space heat recovery approach T
C     |                Burner_exhaust  - array containing exhaust gas compisitions
C     |                Fuel_type   - integer flag indicating fuel type
C     |              Outputs:
C     |                Q_Space2  - Heat delivered from the fuel cell system to the water
C     |                Q_Cooling2  - Fuel cell system cooling load.
C---------------------------------------------------------------------------------
                     CALL PEM_HEAT_REC(
     &                    T_water_max,
     &                    T_cell,
     &                    T_cool_water,
     &                    Cp_cool_water,
     &                    iseg,
     &                    TH_cold_streams,
     &                    TH_hot_streams,
     &                    dT_S_heat_rec,
     &                    Burner_exhaust,
     &                    Fuel_type,
     &                    Q_Space2,
     &                    Q_Cooling2,
     &                    DEBUG_ME)
C---------------------------------------------------------------------------------
C     |              If recovered heat exceeds total heat sink resource,
C     |              set to resource size:
C---------------------------------------------------------------------------------
                     IF (Q_Space2.GT.Q_Hsr_Total) Q_Space2=Q_Hsr_Total
C---------------------------------------------------------------------------------
C     |              If space heat is less than zero, set to zero
C---------------------------------------------------------------------------------
                     IF (Q_Space2.LT.0.) Q_Space2 = 0.
C---------------------------------------------------------------------------------
C     |              Calculate power required by cooling equipment
C     |              (Equation 4.3.4, Ferguson 2003.)
C---------------------------------------------------------------------------------
                     W_Cooling2 = (Q_Cooling2)/cool_COP  ! (kW)
C---------------------------------------------------------------------------------
C     |              Calculate net power
C     |              Net = gross - parasitic losses - fan power requiremments
C     |              (Equation 4.3.5, Ferguson 2003.)
C---------------------------------------------------------------------------------
                     P_Net2 = P_gross_2 - W_Parasitic2 - W_Cooling2 !(kW)
C---------------------------------------------------------------------------------
C     |              Check to see if P_Net1 <= P_demand <= P_Net2. If so, the
C     |              operating point has been found!
C---------------------------------------------------------------------------------
                     IF ( ( P_Net1 .LE. P_demand ) .AND.
     &                    ( P_demand .LE. P_Net2 ) ) THEN
C---------------------------------------------------------------------------------
C     |              OP has been found. Interpolate between parameters:
C     |
C     |                                       (P - P1)
C     |                  X = X1 + (X2 - X1) * --------
C     |                                       (P2 - P1)
C     |
C---------------------------------------------------------------------------------
C---------------------- temp. variable used for interpolation calculations
                        coeff = (P_demand-P_Net1)/(P_Net2-P_Net1)
C---------------------- Interoplation:
                        fuel_flow = fuel_flow1 +
     &                       ( fuel_flow2 - fuel_flow1 ) * coeff !fuel flow
                        Q_Space = Q_Space1 + (Q_Space2-Q_Space1)*coeff   !space heat
                        P_gross = P_gross_1 +
     &                       (P_gross_2-P_gross_1) * coeff !gross power
                        Q_Cell = Q_Cell1 +(Q_Cell2-Q_Cell1)*coeff  !cell waste heat
                        Q_Cooling = Q_Cooling1 +
     &                       ( Q_Cooling2 - Q_Cooling1 ) * coeff !aux cooling load
C---------------------- Set net power to demand
                        P_Net = P_demand
C---------------------- Set iDone flag
                        Done = .TRUE.

                     ELSE
C---------------------------------------------------------------------------------
C     |                 Operating point has not been found, do nothing
C---------------------------------------------------------------------------------
                     ENDIF
                     IF ( DEBUG_ME ) THEN
                        print*, '     -> iteration:'
                        print*, '        P_gross_1  =', P_gross_1
                        print*, '        W_Parasitic1 =', W_Parasitic1
                        print*, '        W_Cooling1 =', Q_Cooling1
                        print*, '        P_Net1  =', P_Net1
                        print*, '        - '
                        print*, '        P_gross_2  =', P_gross_2
                        print*, '        W_Parasitic2 =', W_Parasitic2
                        print*, '        W_Cooling2 =', Q_Cooling2
                        print*, '        P_Net2  =', P_Net2
                     ENDIF
                  ENDIF
               ENDIF
C------------- Increment map location
               ii = ii+1
            ENDDO
C--------------------------------------------------------------------------------
C     |     END fuel cell operational calculations
C--------------------------------------------------------------------------------
         ELSE
C-----------------------------------------------------------------------
C     |   Fuel cell is off. Set state variables to zero
C-----------------------------------------------------------------------
            fuel_flow  = 0.         !fuel flow
            Q_Space = 0.         !space heat
            P_gross   = 0.         !gross power
            P_Net   = 0.         !net power
            Q_Cell  = 0.         !cell waste heat
            Q_Cooling  = 0.         !aux cooling load
         ENDIF
C================================================================================
C        END fuel cell modelling / START ESP-r specific calculations
C================================================================================
C-------------------------------------------------------------------------------
C     |  Calculate coefficients for state-space matrix:
C     |
C     |  | -CPcwi   Cpcwi | | Ti |  = Q_Space
C     |                     | Th |
C     |
C     |  Where: - CPcwi is the heat capacity of the cooling water,
C     |         - Ti is the future temperature of node i (fuel cell control volume)
C     |         - Th is the future temperatuere of upstream node h to which the fuel
C     |           cell is connected to (and therefore the temperature of the
C     |           cooling water)
C     |         - Q_Space is the heat recovery predicted by the model
C     |
C     |  Th and Ti are evaluated by ESP-r. The coefficient generator needs to
C     |  provide the COUT() array populated with the remaining terms in the
C     |  state space matrix.
C     |
C     |  Note - since the model is steady state, there is no need to worry
C     |  about the implicit/explicit weighting factor (alpha)
C-------------------------------------------------------------------------------
         IF( no_cooling_water ) THEN
            cout(1)= 1.
            cout(2)= -1.
            cout(3)= 0.
         ELSE
            cout(1)= Cp_Cool_Water
            cout(2)= -Cp_Cool_Water
            cout(3)= Q_Space
         ENDIF
C---------------------------------------------------------------------------------
C     |  Save other interesting data in the PCAOUT array:
C     |    1. Fuel consumption
C     |    2. Electrical production (gross)
C     |    3. Electrical production (net)
C     |    4. Recovered space heat
C     |    5. Electrical efficiency
C     |    6. Thermal efficiency
C     |    7. Overall efficiency
C---------------------------------------------------------------------------------
         NAPDAT(IPCOMP)   = 7 ! number of additional outputs
C------- Fuel flow (kg/s) Natural gas only!
         PCAOUT(IPCOMP,1) = PEM_FCINIT(37) * fuel_flow ! kg/s
C------- Gross power
         PCAOUT(IPCOMP,2) = P_gross * 1000. ! W
C------- Net power
         PCAOUT(IPCOMP,3) = P_Net * 1000. ! W
C------- Recovered space heat
         PCAOUT(IPCOMP,4) = Q_Space * 1000 ! W
C------- Check if fuel flow is zero
         IF ( FC_ON ) THEN
C---------- Electrical efficiency (HHV)
            PCAOUT(IPCOMP,5) = P_Net/
     &           ( PEM_FCINIT(38) * fuel_flow * PEM_FCINIT(37) )
C---------- Thermal efficiency (HHV)
            PCAOUT(IPCOMP,6) = Q_Space /
     &           (PEM_FCINIT(38) * fuel_flow * PEM_FCINIT(37))
C---------- Overall efficiency (HHV)
            PCAOUT(IPCOMP,7) = ( P_Net + Q_Space ) /
     &           (PEM_FCINIT(38) * fuel_flow * PEM_FCINIT(37))
         ELSE
            PCAOUT(IPCOMP,5) = -1.
            PCAOUT(IPCOMP,6) = -1.
            PCAOUT(IPCOMP,7) = -1.
         ENDIF



C.......H3Kreports.(LOW_TEMP_FC).start......................
C.......The following code is used to trasnport data to
C.......the H3K reporting object
C.......
C.......Any changes to this code should be made within
C.......the H3Kreports demarcaration comments

C.......Calculations required for H3K reports:
C.......
C.......Fuel cell balance of plant electrical efficiency
C.......

C.......Net electrical output
         Call AddToReport(rvpltFCellQElecNet%Identifier,
     &      P_Net * 1000.)

C.......Parasitic electrical output
         Call AddToReport(rvpltFCellQElecPrstc%Identifier,
     &      (P_gross-P_net)*1000.)

C.......Net thermal output
         Call AddToReport(rvpltFCellQthrmNet%Identifier,
     &       Q_space * 1000.)

C.......Fuel HHV
         Call AddToReport(rvpltFCellFuelHHV%Identifier,
     &      (PEM_FCINIT(38) * fuel_flow * PEM_FCINIT(37))*1000.)

C.......Electrical Efficiency - fuel cell
        IF ( FC_ON ) then
         Call AddToReport(rvpltFCellElecEffBOP%Identifier,
     &      100.0 * P_Net/
     &         ( PEM_FCINIT(38) * fuel_flow * PEM_FCINIT(37) ))

C.......Thermal efficiency ~
         Call AddToReport(rvpltFCellCogenEff%Identifier,
     &      100.0 * ( P_Net + Q_Space ) /
     &             (PEM_FCINIT(38) * fuel_flow * PEM_FCINIT(37)))
        endif

C.......Fuel Flow
         Call AddToReport(rvpltFCellFuelFlw%Identifier,
     &      PEM_FCINIT(37) * fuel_flow)
C.......H3Kreports.(LOW_TEMP_FC).end.........................






C---------------------------------------------------------------------------------
C Electrical network related operations - for ISTATS == 1 only!
C 1. Save real power output in electrical network common block variable
C 2. Call subroutine EMACH to (i) calculate the reactive component of the power
C    produced by the network, and (ii) save this data in the electrical
C    network common block
C

C---------------------------------------------------------------------------------
         IF (ISTATS .EQ. 1) THEN
            pwrp(ipcomp) = P_net*1000. ! Real power output (W), generation is +ive...
            IEMODEL=1           ! Flag indicating which power calculation will be performed
            CALL EMACH(
     &           IPCOMP,        ! Component number (input)
     &           IEMODEL,       ! power calculation flag (input)
     &           pwrp(ipcomp),  ! Real power (input, W)
     &           dummy_a,       ! Complex power (output, W)
     &           dummy_b)       ! Overall power (output, W)
         ENDIF



      ELSEIF ((ISTATS.EQ.2)) THEN
C---------------------------------------------------------------------------------
C     |  First phase mass balance - what goes in must come out.
C---------------------------------------------------------------------------------
         cout(1)= 1.
         cout(2)= -PCONDR(Icon1)
         cout(3)= 0.
      ELSEIF ((ISTATS.EQ.3)) THEN
C---------------------------------------------------------------------------------
C     |  Second phase mass balance - set flow to zero
C---------------------------------------------------------------------------------
         cout(1)= 1.
         cout(2)= 0.
         cout(3)= 0.
      ELSE
C--------CAN'T HAPPEN - ISTATS is set by ESP-r!
      ENDIF

C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC .gt. 0 .AND. NSINC .ge. ITC .AND.NSINC .lt. ITCF
     & .AND.ITRACE(37) .ne. 0 )THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        WRITE(ITU,*) ' 1 node PEM fuel cell'
        WRITE(ITU,*) ' Matrix node    ',INOD1
        WRITE(ITU,*) ' Connection #   ',ICON1
        WRITE(ITU,*) '  - Temperature ',CONVAR(ICON1,1)
        WRITE(ITU,*) '  - Flow rate   ',CONVAR(ICON1,2)
        IF(ISTATS .eq. 1) THEN
           WRITE(ITU,*) ' Control       = ',CDATA(IPCOMP,1),' (-)'
           WRITE(ITU,*) ' W demand      = ',P_demand,' (kWe)'
           WRITE(ITU,*) ' P gross       = ',P_gross,' (kWe)'
           WRITE(ITU,*) ' P net         = ',P_net,' (kWe)'
           WRITE(ITU,*) ' Q space       = ',Q_space,' (kWt)'
           IF ( FC_ON ) THEN
C---------- Electrical efficiency (HHV)
              WRITE(ITU,*) ' N electric HHV= ', P_Net/
     &           ( PEM_FCINIT(38) * fuel_flow * PEM_FCINIT(37) )
C---------- Thermal efficiency (HHV)
              WRITE(ITU,*) ' N thermal HHV = ', Q_Space /
     &           (PEM_FCINIT(38) * fuel_flow * PEM_FCINIT(37))
C---------- Overall efficiency (HHV)
              WRITE(ITU,*) ' N overall HHV = ',( P_Net + Q_Space ) /
     &           (PEM_FCINIT(38) * fuel_flow * PEM_FCINIT(37))
         ENDIF
        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        WRITE(ITU,*) (COUT(ii),ii=1,4)
        IF(ITU .eq. IUOUT) THEN     ! trace output going to screen, not file
           itemp=(IPCOMP/4)*4
           IF(itemp .eq. IPCOMP .OR. IPCOMP .eq. NPCOMP) call epagew ! write 4 lines at a time.
        END IF
        WRITE(ITU,*) ' Leaving subroutine PEM_coeff_gen'
      END IF
      RETURN
      END


C===================== PEM_HEAT_REC =========================================
C     Created by: Alex Ferguson
C     Created on: March 2001
C     Copyright:  Alex Ferguson 2001 / CETC 2003.
C----------------------------------------------------------------------
C     ABSTRACT:
C
C---------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C
C----------------------------------------------------------------------
C     REVISIONS:
C     SEPT 2003: Code was heavily revised to comply with CETC coding
C                standard and improve general readability.
C======================================================================
      SUBROUTINE PEM_HEAT_REC(
     &     T_water_max,
     &     T_cell,
     &     T_cool_water,
     &     Cp_Cool_Water,
     &     iseg,
     &     TH_cold_streams,
     &     TH_hot_streams,
     &     dT_S_heat_rec,
     &     Burner_exhaust,
     &     Fuel_type,
     &     Q_Space,
     &     Q_Cooling,
     &     DEBUG
     &     )

      IMPLICIT NONE
C     Inputs
      REAL T_water_max          ! maximum temperature to which cooling water
                                ! can be heated (K)
      REAL T_cool_water         ! Cooling water inlet temperature (K)
      REAL Cp_Cool_Water        ! Cooling water heat capacity flow rate (kW/oC)
      REAL T_cell               ! Fuel cell operating temperature (K)
      REAL dT_S_heat_rec        ! Minimum heat recovery approach temperature (K)
      integer iseg              ! number of segments on cooling map
      integer Fuel_type         ! integer flag indicating fuel type
c     Arrays
      REAL TH_cold_streams(2,2) ! Array containg temperature-enthalpy points
                                ! for cooling water (straight line w/ 2 points.
                                ! passed to subroutine PEM_heat_rec, which performs pinch
                                ! analysis
                                !    TH_cold_streams(1,X) = temperaure @ node X
                                !    TH_cold_streams(2,X) = enthalpy @ node X
      REAL TH_hot_streams(2,12) ! Array containing temperature-enthalpy points
                                ! for cooling reformate. Passed to subrouitine
                                ! PEM_heat_rec, which performs pinch analysis
                                !    TH_hot_streams(1,X) = temperaure @ node X
                                !    TH_hot_streams(2,X) = enthalpy @ node X
      REAL Burner_exhaust(2,7)  ! Array containing composition of the
                                ! burner exhaust. Calculated by subroutine POWER
c     Outputs
      REAL Q_Space              ! Space heat recovery (kW)
      REAL Q_Cooling            ! System cooling requirement (kW)

C     Local variables
      REAL shift                ! temporary array containing the required
                                ! enthalpy shift.
      REAL H1,H2,T1,T2,T_int,rHIC,rHiH ! Temporary variables used for interpolation
      REAL Lowest_Enthalpy      ! Lowest enthalpy on the cold temperature-enthalpy profile
      REAL Lowest_temperature   ! Temporary variable used to compare the enthalpies
                                ! at the bottom of the shifted temperature-enthalpy curves (kW)
      REAL hot_stream_shift     ! Enthalpy shift required for heating stream (kW)
      REAL dH_exhaust                ! Enthalpy change occuring in exhaust streams as they are
                                ! heated or cooled.
      REAL Temp_comp(7)         ! Temporary array holding stream composition
      REAL Top_HR_Temperature               ! Temperature at which waste heat becomes available
                                ! for heat recovery (K)
      integer i,j,k,l           ! counters
      REAL HR_LB, HR_UB         ! bounds on the upper and lower limits of heat
                                ! recovery - describe the amount of overlap on
                                ! the shifted hot and cold temperature-enthalpy
                                ! profiles
      LOGICAL DEBUG             ! Flag for debugging

C--------------------------------------------------------------------------------
C     Fill cooling water (cold) temperature-enthalpy profile with 2 points
C     |  1. Cooling water inlet conditions
C     |  2. Conditions at cooling water maximum temperature
C--------------------------------------------------------------------------------
      TH_cold_streams(1,1) = T_Cool_Water ! Temperature
      TH_cold_streams(2,1) = 0.            ! Enthalpy
      IF (T_Cool_Water.GT.T_water_max) THEN
         TH_cold_streams(1,2) = T_Cool_Water ! Temperature
      ELSE
         TH_cold_streams(1,2) = T_water_max ! Temperature
      ENDIF
      TH_cold_streams(2,2) = Cp_Cool_Water *(T_water_max-T_cool_water) ! Enthalpy

      IF ( DEBUG ) THEN
         print*, '  Entering subroutine PEM_HEAT_REC'
         print*, '          -  PEM_HEAT_REC inputs'
         do i=1, 2
            print*, '              A ',TH_Cold_Streams(1,i),' - ',
     &           TH_Cold_Streams(2,i)
         enddo
         do i=1, iseg
            print*, '              B ',TH_Hot_Streams(1,i),' - ',
     &           TH_Hot_streams(2,i)
         enddo
      ENDIF

C----------------------------------------------------------------------------------
C     Check to see if cooling water inlet temperature is above maximum permitted
C     water temperature, or above the highest temperature in the cooling reformate
C     array.  If so, set heat recovery to zero, and the optimal exhast gas temp.
C     to the higest temperature in the cooling reformate array.
C----------------------------------------------------------------------------------
      IF ((T_cool_water.ge.TH_Hot_streams(1,1)-dT_S_heat_rec)
     &     .OR.(T_cool_water.GE.T_WATER_MAX)) THEN
C----------------------------------------------------------------------------------
C     |  Set recovered heat to zero
C----------------------------------------------------------------------------------
         Q_Space = 0.
C----------------------------------------------------------------------------------
C     |  Shift cooling water array to start at top of hot reformate array -
C     |  this will result in the correct cooling load being calculated.
C----------------------------------------------------------------------------------
         TH_cold_streams(2,1) = TH_cold_streams(2,1) +
     &        TH_hot_streams(2,1)
         TH_cold_streams(2,2) = TH_cold_streams(2,2) +
     &        TH_hot_streams(2,1)
C----------------------------------------------------------------------------------
C     |  Calculate cooling duty: cooling duty is equal to total enthalpy
C     |  change occuring in cooling streams.
C----------------------------------------------------------------------------------
         Top_HR_Temperature = TH_Hot_streams(1,1)
      ELSE

C----------------------------------------------------------------------------------
C     |  Calculate heat treansfer between heating and cooling streams. This
C     |  is accomolished by applying "Pinch Analysis" to determine the enthalpy
C     |  shift required to preserve the minimum temperature difference (dThr)
C     |  between the hot and cold temperature enthalpy profiles. This procedure
C     |  is described in detail in section B.2.2 in Appendix B, in Ferguson (2003).
C----------------------------------------------------------------------------------
C     |  Set Shift amount to zero
C----------------------------------------------------------------------------------
         hot_stream_shift = 0.            ! (kW)
C----------------------------------------------------------------------------------
C     |  Start cold temperature-enthalpy profile -
C     |  For each node X on the cold temperature-enthalpy profile:
C     |    1. Calculate the enthalpy H_Y on the hot temperature-enthalpy profile
C     |       that corresponds to the temperature T_X + dThr
C     |    2. The required enthalpy shift is determined:
C     |
C     |               Shift = H_Y - H_X
C     |
C     |       See equations B.2.1 - B.2.5 in Ferguson (2003)
C----------------------------------------------------------------------------------
         DO  i = 1, 2           !<- one segment, 2 nodes on cold temperature profile
C----------------------------------------------------------------------------------
C     |     Calculate intermediate temperature on hot temperature-enthalpy profile.
C     |
C     |                     T_Y = T_X + dThr
C     |
C----------------------------------------------------------------------------------
            T_int = TH_cold_streams(1, i) + dT_S_heat_rec ! (K)
C----------------------------------------------------------------------------------
C     |     Search through nodes on hot side temperature-enthalpy profile for two
C     |     nodes (1 & 2) such that  T_1 > T_Y > T_2
C----------------------------------------------------------------------------------
            DO  j = 1, iseg-1   !<- for all segments on the hotside curve
C----------------------------------------------------------------------------------
C     |        Assign two sequential nodes on the hot curve as lower and upper
C     |        bounds
C----------------------------------------------------------------------------------
               T1 = TH_hot_streams(1, j)  ! Upper bound
               T2 = TH_hot_streams(1, j + 1) ! Lower bound
C----------------------------------------------------------------------------------
C     |        If the intermediate temperature is between the lower and upper
C     |        bounds, proceed with the analysis. Otherwise, move on to the next
C     |        segment.
C----------------------------------------------------------------------------------
               IF ((T_int.LE.T1).AND.(T_int.GT.T2)) THEN
C----------------------------------------------------------------------------------
C     |           Assign lower and upper bound enthalpies to temp. variables H1
C     |           and H2. Assign enthalpy of the cold node to variable rHiC
C----------------------------------------------------------------------------------
                  H1 = TH_hot_streams(2, j) ! upper bound
                  H2 = TH_hot_streams(2, j + 1) ! lower bound
                  rHiC = TH_cold_streams(2, i) ! cold node enthalpy
C---------------------------------------------------------------------------------
C     |           Determine the enthalpy of the cooling stream at temperature T_X
C     |           through interpolation:
C     |
C     |                                          T_X - T_1
C     |               H_X  = H_1 + (H_2 - H_1) * ---------
C     |                                          T_2 - T_1
C     |
C---------------------------------------------------------------------------------
                  rHiH = H1 + (H2-H1)*  (T_int-T1)/(T2-T1)
C---------------------------------------------------------------------------------
C     |           Calculate enthalpy shift and store in temporary variable shift
C---------------------------------------------------------------------------------
                  shift = rHiH - rHiC   ! req'd shift (kW)
C---------------------------------------------------------------------------------
C     |           Check to see if this is the first time the required shift has
C     |           calculated:
C     |             - if so, set variable hot_stream_shift to calculated shift amount.
C     |             - otherwise, only update hot_stream_shift if the calculated shift
C     |               is larger than the current value of hot_stream_shift.
C---------------------------------------------------------------------------------
                  IF (hot_stream_shift.EQ.0.) THEN
                     hot_stream_shift = shift
                  ELSEIF (shift .GT. hot_stream_shift) THEN
                     hot_stream_shift = shift
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
C----------------------------------------------------------------------------------
C     |  Start hot temperature-enthalpy profile -
C     |  For each node Y on the hot temperature-enthalpy profile:
C     |    1. Calculate the enthalpy H_X on the hot temperature-enthalpy profile
C     |       that corresponds to the temperature T_Y - dThr
C     |    2. The required enthalpy shift is determined:
C     |
C     |               Shift = H_Y - H_X
C     |
C     |       See equations B.2.6 - B.2.10 in Ferguson (2003)
C----------------------------------------------------------------------------------
         DO  i = 1, iseg
C----------------------------------------------------------------------------------
C     |     Calculate intermediate temperature on hot temperature-enthalpy profile.
C     |
C     |                     T_X = T_Y - dThr
C     |
C----------------------------------------------------------------------------------
            T_int = TH_hot_streams(1, i) - dT_S_heat_rec
C----------------------------------------------------------------------------------
C     |     Check the two nodes on the cold side temperature-enthalpy profile to
C     |     see if T_1 < T_X < T2
C----------------------------------------------------------------------------------
            j = 1               ! Remember that the cooling water curve only has one
                                ! segment!
            T1 = TH_cold_streams(1, j)     ! Lower bound
            T2 = TH_cold_streams(1, j + 1) ! Upper bound
C----------------------------------------------------------------------------------
C     |     If the intermediate temperature is between the lower and upper
C     |     bounds, proceed with the analysis. Otherwise, move on to the next
C     |     segment on the hot-side temperature-enthalpy profile
C----------------------------------------------------------------------------------
            IF ((T_int .GT. T1).AND.(T_int .LE. T2)) THEN
C----------------------------------------------------------------------------------
C     |        Assign lower and upper bound enthalpies to temp. variables H1
C     |        and H2. Assign enthalpy of the hot node to variable rHiH
C----------------------------------------------------------------------------------
               H1 = TH_cold_streams(2, j)
               H2 = TH_cold_streams(2, j + 1)
               rHiH = TH_hot_streams(2, i)
C---------------------------------------------------------------------------------
C     |        Determine the enthalpy of the heating stream at temperature T_Y
C     |        through interpolation:
C     |
C     |                                          T_Y - T_1
C     |               H_Y  = H_1 + (H_2 - H_1) * ---------
C     |                                          T_2 - T_1
C     |
C---------------------------------------------------------------------------------
               rHiC = H1 + (H2-H1)*(T_int-T1)/(T2-T1)
C---------------------------------------------------------------------------------
C     |        Calculate enthalpy shift and store in temporary variable shift
C---------------------------------------------------------------------------------
               shift = rHiH - rHiC ! req'd shift (kW)
C---------------------------------------------------------------------------------
C     |        Update hot_stream_shift if calculated shift is larger than current value of
C     |        hot_stream_shift.
C---------------------------------------------------------------------------------
               IF ( shift .GT. hot_stream_shift ) THEN
                  hot_stream_shift = shift
               ENDIF
            ENDIF
         ENDDO
C---------------------------------------------------------------------------------
C     |  If the req'd enthalpy shift is less than zero, the lowest enthalpy on the
C     |  cold temperature-ethalpy curve may be below the lowest enthalpy on the
C     |  hot temperature-enthalpy curve. Therefore, check that rCA(2,1) will not be
C     |  less than rCB(2,iseg) once the shift has been applied, and adiust the
C     |  calculated enthalpy shift if necessary.
C     |
C     |  Lowest_temperature - Difference in enthalpy at bottom of temperature enthalpy profiles
C     |          after shift has been applied.
C---------------------------------------------------------------------------------
         Lowest_temperature = (TH_cold_streams(2,1)+hot_stream_shift) -
     &        TH_hot_streams(2,iseg)
         IF (Lowest_temperature.LT.0.) THEN
            hot_stream_shift = TH_hot_streams(2,iseg) -
     &           TH_cold_streams(2,1)
         ENDIF
C---------------------------------------------------------------------------------
C     |  Shift the cold compositie curve by hot_stream_shift.
C---------------------------------------------------------------------------------
         DO i = 1, 2            ! <- only 2 nodes on the cold curve...
            TH_cold_streams(2, i) = TH_cold_streams(2, i) +
     &           hot_stream_shift
         ENDDO
C---------------------------------------------------------------------------------
C     |  Determine the amount of overlap (ie, potential for heat transfer)
C     |  between the hot and cold temperature-enthalpy profiles
C     |
C     |
C     |    ^                                s
C     |  T |                                        s - Hot profile
C     |    |                            s           o - cold profile
C     |    |
C     |    |                       s
C     |    |   pinch
C     |    |      \           s
C     |    |       \                   o
C     |    |        \   s         o    |
C     |    |         -->     o         |
C     |    |            o              |
C     |    |       o    |              |
C     |    |            |              |
C     |    #--------------------------------------> H
C     |                 |<- overlap -->|
C     |
C     |                HR_LB         HR_UB
C     |
C     |  - HR_LB (heat recovery lower bound) is the greater of TH_cold_streams(2,1) and TH_hot_streams(2,iseg).
C     |  - HR_UB (heat recovery upper bound) is the lesser of TH_cold_streams(2,2) and TH_hot_streams(2,1).
C     |  - The difference between HR_LB and HR_UB represents the amount of heat
C     |    that can be transferred to the cooling water
C     |
C---------------------------------------------------------------------------------
         IF( TH_cold_streams(2,1) .GT. TH_hot_streams(2,iseg) ) THEN
            HR_LB = TH_cold_streams(2,1)    !(kW)
         ELSE
            HR_LB = TH_hot_streams(2,iseg) !(kW)
         ENDIF
         IF( TH_cold_streams(2,2) .LT. TH_hot_streams(2,1) ) THEN
            HR_UB = TH_cold_streams(2,2)    !(kW)
         ELSE
            HR_UB = TH_hot_streams(2,1)    !(kW)
         ENDIF
C---------------------------------------------------------------------------------
C     |  Calculate the amount of heat that can be transferred to the cooling
C     |  stream
C---------------------------------------------------------------------------------
         Q_Space = HR_UB - HR_LB   !(kW)
C---------------------------------------------------------------------------------
C     |  Determine the optimum echaust gas rejection temperature. This is the
C     |  temperature at which further cooling of the exhuast stream does not
C     |  result in additional heat transfer to the cooling water, and places
C     |  additional loads on the auxiliary cooling system.
C     |
C     |  See item 2b, Section 4.3.2 in Ferguson (2003).
C     |
C     |  1. Determine enthalpy corresponding to the lowest point on the cold
C     |     (TH_cold_streams) temperature-enthalpy profile (to the left of this point,
C     |     no heat recovery can occur and all enthalpy changes occuring
C     |     on the hot (TH_hot_streams) temperature-enthalpy profile represent surplus
C     |     heat.
C---------------------------------------------------------------------------------
         Lowest_Enthalpy = TH_cold_streams(2,1)
C---------------------------------------------------------------------------------
C     |  2. Loop through all segments on the hot temperature-enthalpy profile
C     |     to determine the temperature on the hot profile that
C     |     corresonds to enthalpy lowest_enthalpy:
C---------------------------------------------------------------------------------
         do k = 1, iseg         ! <- 'iseg' hot side segments; each segment is
                                !       defined by k and k+1
C---------------------------------------------------------------------------------
C     |     Is Lowest_Enthalpy between enthalpies at nodes k and k+1?
C---------------------------------------------------------------------------------
            IF (( TH_hot_streams(2,k).GE.Lowest_Enthalpy).AND.
     &           (TH_hot_streams(2,k+1).LT.lowest_enthalpy)) THEN
C---------------------------------------------------------------------------------
C     |        Lowest_Enthalpy is between enthalpies at nodes k and k+1. Interopolate to
C     |        determine temperature on hot temperature-enthalpy profile
C     |        that corresponds to enthalpy lowest_enthalpy:
C     |
C     |                                      lowest_enthalpy - H_k+1
C     |        T_X = T_k+1 + (T_k - T_k+1) * -------------
C     |                                       H_k - H_k+1
C     |
C---------------------------------------------------------------------------------
               Top_HR_Temperature = TH_hot_streams(1,k+1)
     &              +(Lowest_Enthalpy - TH_hot_streams(2,k+1))
     &              /(TH_hot_streams(2,k)-TH_hot_streams(2,k+1))
     &              *(TH_hot_streams(1,k)-TH_hot_streams(1,k+1))
            ENDIF
         ENDDO

      ENDIF


C---------------------------------------------------------------------------------
C     Check to see if T_cell is less than Top_HR_Temperature. If so, there exists
C     an optimum exhaust gas rejection temperature which will reduce
C     the overall cooling load.
C---------------------------------------------------------------------------------
      IF ( Top_HR_Temperature .GT. T_cell) THEN
C---------------------------------------------------------------------------------
C     |  Call subroutine del_H to determine the change in enthalpy (dH_exhaust the
C     |  exhaust gasses will undergo if cooled between Top_HR_Temperature and Tcell. (This
C     |  is equivlent to the reductuion in cooling load that occurs when the
C     |  exhaust gasses are rejected at Top_HR_Temperature, and not Tcell.
C     |
C     |  First, store burner exhaust compositions in temporary array
C     |  Temp_comp:
C     |  Burner_exhaust(1,i) - mol of species i in hydrogen burner exhaust
C     |  Burner_exhaust(2,i) - mol of species i in auxiliary burner exhaust
C---------------------------------------------------------------------------------
         DO l = 1, 7
            Temp_comp(l) =  Burner_exhaust(1,l)+Burner_exhaust(2,l)
         ENDDO
         CALL PEM_del_H(Temp_comp,      ! stream composition
     &        Top_HR_Temperature,           ! stream start temperature
     &        T_cell,           ! stream target temperature
     &        400.,             ! Dummy variable (water saturation temp)
     &        dH_exhaust,            ! Total change in enthalpy (output, kW)
     &        400.,             ! Dummy variable (fuel saturation temp)
     &        500.,             ! Dummy variable (fuel 1)
     &        Fuel_type,             ! fuel type
     &        .FALSE.)          ! flag indicating if vaporization will occur
      ELSE
C--------------------------------------------------------------------------------
C     |  Exhaust stream is being rejected at optimal temperature - no
C     |  cooling loads savings
C--------------------------------------------------------------------------------
         dH_exhaust = 0.
      ENDIF
C--------------------------------------------------------------------------------
C     |  Calculate cooling load:
C     |
C     |  Q_cool =   Lowest point on cold temperature-enthalpy curve
C     |           - Lowest point on hot temperature-enthalpy curve
C     |           + exhaust gas coolng load savings
C     |
C--------------------------------------------------------------------------------
      Q_Cooling = TH_cold_streams(2,1) - TH_hot_streams(2,iseg)
     &     +dH_exhaust

      IF ( DEBUG ) THEN
         print*, '            rDT          =', dT_S_heat_rec
         print*, '            Q_Space         =', Q_Space
         print*, '            TOP_HR_TEMPERATURE       =',
     &        TOP_HR_TEMPERATURE
         print*, '            T_cool_water          =', T_cool_water
         print*, '            T_WATER_MAX       =', T_WATER_MAX
         print*, '            Q_Cooling        =', Q_Cooling
         print*, '            TH_cold_streams(2,1)    =',
     &        TH_cold_streams(2,1)
         print*, '            TH_hot_streams(2,iseg) =',
     &        TH_hot_streams(2,iseg)
         print*, '            dH_exhaust        =', dH_exhaust

         print*, ' Exiting subroutine PEM_HEAT_REC'
      ENDIF



      RETURN
      END

C===================== POWER ==========================================
C     Created by: Alex Ferguson
C     Created on: March 2001
C     Copyright:  Alex Ferguson 2001 / CETC 2003.
C----------------------------------------------------------------------
C     ABSTRACT:
C
C----------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C
C----------------------------------------------------------------------
C     REVISIONS:
C     SEPT 2003: Code was heavily revised to comply with CETC coding
C                standard and improve general readability.
C======================================================================
      SUBROUTINE PEM_POWER(
     &     IPCOMP,
     &     mode,
     &     P_gross,
     &     W_Parasitic,
     &     iseg,
     &     TH_Surplus,
     &     Q_Cell,
     &     fuel_flow,
     &     p_gross_max,
     &     Burner_exhaust
     &     )
      IMPLICIT NONE
#include "plant.h"
C--------------------------------------------------------------------------------
C     Common block req'd for fuel cell description (contained in ADATA)
C--------------------------------------------------------------------------------
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
C-------------------------------------------------------------------------------
C     Declare common block variables
C-------------------------------------------------------------------------------
      REAL ADATA, BDATA
C--------------------------------------------------------------------------------
C     Declare initiaization functon
C--------------------------------------------------------------------------------
      REAL PEM_FCinit
C--------------------------------------------------------------------------------
C     Declare ESP-r local variables
C--------------------------------------------------------------------------------
C---- ENVIRONMENT VARIABLES
      INTEGER IPCOMP            ! pointer to component location in plant network
C---- PASSED VARIABLES
      INTEGER mode              ! integer indicating which calculation POWER
                                ! subroutine is used for (input):
                                !  mode = 1 -> deterime gross power that
                                !              corresponds to a given net power
                                !  mode = 2 -> characterize fuel cell at a
                                !              given net power
      REAL P_gross              ! gross power (kw, output if mode=1, input if
                                !     mode = 2)
      REAL W_Parasitic          ! parasitic losses, excluding cooling system
                                !     electrical requirement (kW, output)
      INTEGER iSeg              ! number of segments on the truncated cooling
                                !     temperature-enthalpy profile (output)
      REAL TH_Surplus(12,2)     ! array containing truncated cooling temperature
                                !     enthalpy profile (output)
      REAL Q_Cell               ! heat recovery from fuel cell (kW, output)
      REAL fuel_flow            ! fuel flow (kmol/s, output)
      REAL p_gross_max          ! Maximum gross power (kW, input if mode = 2)

C---- FUEL CELL DESCRIPTION (inputs)
      integer Fuel_type         ! fuel type (1= methane, 3 = methanol)
      REAL P_Net_Max            ! max fc net power  output  (kW)
      REAL elec_efficiency_at_full_power ! Net electrical efficiency at full load (0=>1)
      REAL activation_polarization ! activation polarization (dimensionless)
                                ! (see setion 3.1.3, Ferguson 2003)
      REAL PCU_efficiency       ! power condition unit efficiency (0->1)
      REAL fuel_cell_heat_loss  ! fuel cell thermal loss coefficent (kW/deg k)
      REAL AUX_burner_efficiency ! auxiliary burner efficiency  (0->1)
      REAL Air_compressor_eff   ! air compressor isentropic efficiency  (0->1)
      REAL NG_compressor_eff    ! mehtane compressor isentropic efficiency  (0->1)
      REAL Pump_efficiency      ! pump isentropic efficiency  (0->1)
      REAL Motor_efficiency     ! electric motor efficiency (0->1)
      REAL cool_COP                 ! cooling system COP parametric coefficient 1
C     REAL cool_COP2                ! cooling system COP parametric coefficient 2
C     REAL cool_COP3                ! cooling system COP parametric coefficient 3
C     REAL cool_COP4                ! cooling system COP parametric coefficient 4
      REAL Aux_Bur_util         ! auxiliary burner fuel utilization
      REAL Aux_Bur_EA           ! auxiliary burner excess air ratio
      REAL T_reformer           ! Reformer operating temperature (K)
      REAL T_cell               ! Stack operating temperature (K)
      REAL dT_P_heat_rec        ! minimum process-to-process approach temperature (K)
      REAL Pressure_oper        ! Compressor outlet pressures (kPa)
      REAL Pressure_anode       ! anode operating pressure (kPa)
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      INTEGER ii, jj, kk, ll    ! counters
      REAL P_net                ! net power, excluding cooling loads (kW,
                                !     input if mode = 1, output if node = 2)
      REAL pressures(3)         ! holding array used to pass pressure
                                !    data to subroutines
      REAL FC_inlet_data(10,2)  ! array contaning inlet conditions for
                                !    fuel cell anode and cathode
      REAL FC_outlet_data(10,2) ! array contaning outlet conditions for
                                !    fuel cell anode and cathode
      REAL Temp_comp(7)         ! holding arrays used to pass the
                                !    composition of a stream to
                                !    subroutines
      REAL Burner_exhaust(2,7)  ! Array containing composition of the
                                !    burner exhaust.
      REAL Composition_cold(10,30), Composition_hot(10,30)
                                ! Arrays containing stream compositions
                                !    on a per/mol H2 basis for all
                                !    cold (Composition_cold) and hot (Composition_hot)
                                !    streams. Populated by subroutine
                                !    recomp
      REAL TH_cold(10,30), TH_hot(10,30)
                                ! Arrays containing cold and hot
                                !    temperature-enthalpy profiles
                                !    for hot and cold streams
      REAL cathode_air(10,2)    ! Array containing air composition
                                !    at cathode inlet and outlet
      REAL burner_air(3)        ! Array containing conditions at
                                !    burner combustion air inlet
      REAL T_H2O_supply         ! inlet temperature of reaction water (K)
      REAL T_air_supply         ! supply temperatur of oxygen (K)
      REAL T_CH4_supply         ! supply temperature of methane (K)
      REAL T_air_compressor     ! inlet temperature of oxygen (at outlet of
                                !    compressor), (K)
      REAL T_CH4_compressor     ! inlet temperature of oxygen (at outlet of
                                !    compressor), (K)
      REAL T_surroundings       ! surrounding temperature  (K)
      REAL fuel_stioch          ! fuel flow required to deliver 1 mol/s
                                !    of hydrogen at anode inlet
      REAL max_efficiency       ! fuel cell stack theoretical maximum
                                !    electrochemical efficiency
      REAL  dH_stack            ! total enthalpy change occuring in
                                !    fuel cell per mol H2 reacted (kJ/mol H2)

      REAL Q_Cooling_Load       ! system thermal cooling load (kW)
      REAL W_Cooling            ! cooling system electrical cooling load (kW)
      REAL Wm_pump_water, Wm_pump_fuel ! pump mechanical work requirements
                                !    (Wm_pump_water: water, Wm_pump_fuel: methanol) (kW)
      REAL we_pump_total              ! Pump electrical work requirements (kW)
      REAL PCU_losses           ! losses in power conditioning unit (kW)
      LOGICAL DONE_1,DONE_2     ! flags used to halt iteration
      REAL P_grossL, P_grossU   ! bounds on gross power bisection root search (kW)
      REAL p_net_guess          ! Estimated net power (kW)
      REAL P_difference         ! error in gross power estimate
      REAL dH_exhaust           ! change in enthalpy associated with cooling
                                !     exhaust gasses (kW)
      REAL Peak_power_cap       ! Fuel cell peak power capacity (-)
      REAL Scaled_effeciency    ! Electrochemical efficiency @ full load
                                !     scaled by theoretical efficiency (-)
      REAL PtLoad_efficiency    ! fuel cell part load electrochemical efficiency (-)
      REAL H2_flow              ! hydrogen molar flow rate (kmol/s)
      REAL Q_FC_loss                ! fuel cell thermal losses (kW)

      REAL aux_fuel_L, aux_fuel_U, aux_fuel_M ! lower, upper and midpoints used for
                                !    fuel flow bisection root search
      REAL dH_reformer          ! total enthalpy change occuring within the
                                !    reformer (kW)
      REAL Q_aux_burner         ! heat delivered to fuel processor from auxiliary
                                !    burner
      REAL FP_Q_required        ! fuel processor auxiliary heat requirement (kW)
      REAL H_balance            ! net enthalpy deficit in fuel processor (kW)
      REAL H_cold_lowest        ! lowest enthalpy on cold composite curve (TH_cold)
      REAL AUX_inlet_data(7), AUX_outlet_data(7) ! Temporaray variables containing inlet and exhaust
                                !     compositions of auxiliary burner
      REAL Wm_comp_air          ! mechanical work requirement of air compressor (kW)
      REAL Wm_comp_fuel         ! mechanical work requirement of natural gas compressor (kW)
      REAL Top_HR_Temperature   ! optimal exhaust gas rejection temperature (K)
      REAL ATM_pressure                ! atmospheric pressure (kPa)
      REAL We_comp_total        ! total electrical work requirement (kW)
      REAL mm_water,mm_methanol ! water / methanol molar mass (kg/kmol)
      REAL rho_h2o,rho_methanol ! water / methanol density (kg/m3)
      REAL relative_error       ! tolerance for iterations (0->1)
C----------------------------------------------------------------------
C     Read fuel cell description from ADATA array
C----------------------------------------------------------------------
C     Fuel Type
      Fuel_type=   ADATA(IPCOMP, 1)
C     Fuel Cell normal rated output (kWe) (NET)
      P_Net_Max =  ADATA(IPCOMP, 2)/1000.
C     Cell electrochem efficiency at full load
      elec_efficiency_at_full_power =   ADATA(IPCOMP,4)
C     Activation Polarization
      activation_polarization =    ADATA(IPCOMP,5)
C     Power Conditioning efficiency
      PCU_efficiency =   ADATA(IPCOMP,6)
C     Fuel Cell thermal loss coefficient (kW/degC)
      fuel_cell_heat_loss =   ADATA(IPCOMP,8)/1000.
C     Auxiliary Burner Efficiency
      AUX_burner_efficiency =  ADATA(IPCOMP,13)
C     Air Compressor isothermal efficiency
      Air_compressor_eff = ADATA(IPCOMP,14)
C     NG Compressor isothermal efficiency
      NG_compressor_eff = ADATA(IPCOMP,15)
C     Pump efficiency
      Pump_efficiency = ADATA(IPCOMP,16)
C     Electric Motor Efficiency
      Motor_efficiency =   ADATA(IPCOMP,17)
C     Evaproative cooling system performance (COP)
      cool_COP =   ADATA(IPCOMP, 18)
C     Aux Burner Fuel Utilization
      Aux_Bur_util =  ADATA(IPCOMP, 21)
C     Auxiliary burner excess air amount
      Aux_Bur_EA = ADATA(IPCOMP, 26)
C     Reformer operating temperature (K)
      T_reformer =  ADATA(IPCOMP, 32)+PEM_FCinit(0)
C     Cell stack operating temperature (K)
      T_cell = ADATA(IPCOMP, 36)+PEM_FCinit(0)
C     Process heat recovery minimum approach temperature (K)
      dT_P_heat_rec = ADATA(IPCOMP, 37)
C     Ambient temperature (K)
      T_surroundings = ADATA(IPCOMP, 39) + PEM_FCinit(0)
C     Compressor outlet pressures (kPa)
      Pressure_oper = ADATA(IPCOMP,40)
C     Anode Operating pressure (kPa)
      Pressure_anode =  ADATA(IPCOMP,41)
C----------------------------------------------------------------------
C     Get relative error tolerance
C----------------------------------------------------------------------
      relative_error = PEM_FCinit(41)
C----------------------------------------------------------------------
C     Atmospheric pressure
C----------------------------------------------------------------------
      ATM_pressure = PEM_FCinit(2)         ! kPa
C----------------------------------------------------------------------
C     Specify supply temperature for fuel processor reactants -
C     The model might be improved later by allowing users to specify
C     these parameters in the input file
C----------------------------------------------------------------------
      T_H2O_supply = PEM_FCinit(3) ! Water temperature (K)
      T_air_supply = PEM_FCinit(4) ! Air temperature @ compressor inlet (K)
      T_CH4_supply = PEM_FCinit(5) ! Methane temperature @ compressor inlet (K)
      T_surroundings = PEM_FCinit(1) ! Ambient temperature (K)
C----------------------------------------------------------------------
C     Properties of water, methanol
C----------------------------------------------------------------------
      mm_water  = PEM_FCinit(9) ! water molar mass (kg/kmol)
      rho_h2o   = PEM_FCinit(10) ! water density (kg/m3)
      mm_methanol = PEM_FCinit(11) ! methanol molar mass (kg/kmol)
      rho_methanol  = PEM_FCinit(12) ! methanol density (kg/m3)
C----------------------------------------------------------------------
C     Lower and upper bounds, and initial guess for auxliary fuel
C     flow rate iteration. The selection of aux_fuel_U is somewhat arbitrary,
C     but its present value (0.0005) is sufficiently large to contain
C     the range of values encountered in residential fuel cell systems.
C     If the model were used to model significantly larger systems or
C     the model inputs diverged significantly from those described in
C     Ferguson (2003), the upper iteration bound might require
C     adjustment.
C----------------------------------------------------------------------
      aux_fuel_L = 0.                ! lower bound, kmol/s
      aux_fuel_U = 0.0005            ! upper bound, kmol/s
C----------------------------------------------------------------------
C     Store operating pressure parameters in holding array pressures,
C     which is used to pass pressure data to subroutines
C----------------------------------------------------------------------
      pressures(1) = Pressure_oper ! System operating pressure (pressure on cathode)
      pressures(2) = Pressure_anode ! Anode operatng pressure
C----------------------------------------------------------------------
C     Call subroutine PEM_Compositions to characterize mass flow rates through
C     the fuel cell system. PEM_Compositions returns five arrays that describe
C     the reactant flow theough the fuel cell  system on "per mol H2
C     produced basis".  This means that the flow rates are caculated by
C     assuming that 1 mol/s of hydrogen were entering the fuel cell.
C
C     The arrays returned by PEM_Compositions can be muliplied by the actual rate
C     of hydrogen flow to determine the composition of reactants/
C     products at any point in the system, for any operating point.
C----------------------------------------------------------------------
      CALL PEM_Compositions(Composition_cold, ! Array containing cold stream compositions (output)
     &     Composition_hot,     ! Array containing hot stream compositions  (output)
     &     cathode_air,         ! Array containing cathode air compositions (output)
     &     burner_air,               ! Array containing combustion air inlet conditions (output)
     &     fuel_stioch,                ! Fuel req'd to produce 1 mol H2 (output)
     &     IPCOMP,              ! Pointer identifying component in plant network (input)
     &     Burner_exhaust)      ! Array containing burner exhaust compositions (output)
C----------------------------------------------------------------------
C    Fill FC_inlet_data and FC_outlet_data arrays with stack inlet and
C    exit compositions (these are holding arrays that
C    describe the fuel cell stack reaction stochiomentry and
C    temperatures.
C
C    Note: at present, the fuel cell stack is assumed to behave
C    isothermally.
C----------------------------------------------------------------------
      FC_inlet_data(1, 1)= T_cell      ! Anode inlet temperature (K)
      FC_outlet_data(1, 1)= T_cell      ! Anode outlet temperature (K)
      FC_inlet_data(1, 2)= T_cell      ! Cathode inlet temperature (K)
      FC_outlet_data(1, 2)= T_cell      ! Cathode outlet temperature (K)
      DO  ii = 2, 8
         FC_inlet_data(ii, 1) = Composition_hot(ii, 8)  ! Anode inlet composition (species ii)
         FC_outlet_data(ii, 1) = Composition_hot(ii, 9)  ! Anode outlet composition (species ii)
         FC_inlet_data(ii, 2) = cathode_air(ii, 1)   ! Cathode inlet composition (species ii)
         FC_outlet_data(ii, 2) = cathode_air(ii, 2)   ! Cathode outlet composition (species ii)
      ENDDO
C-----------------------------------------------------------------------
C     Call subroutine PEM_FCther to characterize the fuel cell stack on a
C     "per-mol-H2-basis". PEM_FCther returns the total enthalpy change
C     occuring in the stack (on a per-mol-H2-basis) and the theoretical
C     maximum efficiency of the stack for a given set of operating
C     conditions.
C-----------------------------------------------------------------------
      CALL PEM_FCther(FC_inlet_data, ! array describing stack inlet conditions (input)
     &            FC_outlet_data, ! array describing stack outlet conditions (input)
     &            max_efficiency,  ! stack theoretical efficiency (output)
     &            dH_stack, ! stack enthalpy change (output)
     &            pressures, ! array containing anode/cathode operating pressure (input)
     &            Fuel_type)   ! fuel type flag (input)
C------------------------------------------------------------------------
C     Check calculation mode:
C       The variable mode is used to tell the subroutine POWER if
C       this is the variable PgorM has been previously determinbed
C          mode = 1: p_gross_max has not been determined, maximum power characteristic
C                    must be determined (see section 4.2.2, Ferguson 2003), or
C               = 2: p_gross_max has been determined, proceed to determine
C                    part load operation characteristics (see section 4.2.3,
C                    Ferguson 2003)
C------------------------------------------------------------------------
      IF (mode.eq.1) THEN
C------------------------------------------------------------------------
C     |  Determine gross power required to produce the system's  nominal
C     |  rated net power, assume all req'd cooling is accomplished with
C     |  water loop (ie. auxiliary cooling system electrical consumption
C     |  is zero)
C------------------------------------------------------------------------
         P_net = P_Net_Max          !Required power production (kW)
         P_grossL = P_net         !Lower bound for gross power iteration (kW)
         P_grossU = P_net * 2.    !Upper bound for gross power iteration (kW)
      ELSE
C------------------------------------------------------------------------
C     |  Characterization of fuel cell operating point described by gross
C     |  power output P_gross required. The variables P_net, P_grossL and
C     |  P_grossU are not used
C------------------------------------------------------------------------
      ENDIF
C------------------------------------------------------------------------
C     Initialize DONE flag, which is used to exit the iteration.
C------------------------------------------------------------------------
      DONE_1 = .FALSE.
C------------------------------------------------------------------------
C     Initiate bisection root search
C------------------------------------------------------------------------
      DO WHILE ( .NOT. DONE_1 )
C------------------------------------------------------------------------
C     |  If determination of gross power is required, set estimated
C     |  gross power to midpoint of bisection root seach bounds
C------------------------------------------------------------------------
         IF (mode .eq. 1) P_gross = ( P_grossU + P_grossL ) / 2. ! kW
C------------------------------------------------------------------------
C     |  The part-load nominal rated efficiency must be determined
C     |  by dividing the rated electrochemical efficiency by the maximum
C     |  theoretical efficiency. See equation 3.1.34, Ferguson 2003.
C------------------------------------------------------------------------
         Scaled_effeciency = elec_efficiency_at_full_power
     &        / max_efficiency  ! Dimensionless
C------------------------------------------------------------------------
C     |  The peak power capacity (PPC) is calculated. This is the
C     |  denominator in equation 3.1.35 (Ferguson 2003), and describes
C     |  the ratio between theoretical maximum power and the maximum
C     |  gross power.
C------------------------------------------------------------------------
         Peak_power_cap = 1.
     &        - (2.*Scaled_effeciency
     &        / (1.-activation_polarization)-1.)**2. ! Dimensionless
C------------------------------------------------------------------------
C     |  Calculate the fuel cell operating part-load efficiency. See
C     |  Equation 3.1.32, Ferguson 2003.
C     |
C     |  Note: during the gross power iteration (mode=1) the system is
C     |  assumed to be operating at its maximum gross power. Therefore,
C     |  the ratio between max gross power and theoretical maximum power
C     |  is equal to the peak power capacity (PPC)
C------------------------------------------------------------------------
         IF (mode .eq. 1 ) THEN
            PtLoad_efficiency = ((1. + SQRT(1. - Peak_power_cap))/2.)*
     &           (1.0-activation_polarization)
         ELSEIF ( mode .eq.2 ) THEN
            PtLoad_efficiency = ((1. + SQRT(1. - P_gross
     &           * Peak_power_cap/p_gross_max))/2.)
     &           * (1.0-activation_polarization)
         ENDIF
C------------------------------------------------------------------------
C     |  Calculate the rate at which hydrogen is required for the fuel
C     |  cell to deliver the specified gross power:
C     |
C     |                         Gross power
C     |  H2 flow = -----------------------------------------
C     |            (enthalpy change per mol H2 )*(efficiency)
C     |
C     |  Where: efficiency is the product of the theoretical and part-
C     |         load efficiencies above.
C     |
C     |  See Equation 4.2.8, Feguson 2003.
C------------------------------------------------------------------------
         H2_flow = P_gross
     &        / (abs(dH_stack) * PtLoad_efficiency * max_efficiency) ! (kmol/s)
C------------------------------------------------------------------------
C     |  The heat loss from the fuel cell stack are estimated using the
C     |  provided UA modulus (fuel_cell_heat_loss):
C     |
C     |         Heat loss = fuel_cell_heat_loss * (Tcell - T_surroundings)
C     |
C     |  SEE euation 3.2.31, Ferguson 2003.
C------------------------------------------------------------------------
         Q_FC_loss = fuel_cell_heat_loss * (T_cell - T_surroundings) ! (kW)
C------------------------------------------------------------------------
C     |  Calculate the surplus heat available in the fuel cell:
C     |
C     |  Q_Cell = (1 - efficiency)*(enthalpy change per mol H2)*(H2 flow)
C     |             - (Heat loss)
C     |
C------------------------------------------------------------------------
         Q_Cell = (1.- PtLoad_efficiency * max_efficiency)
     &        * abs(dH_stack) * H2_flow - Q_FC_loss ! (kW)
C------------------------------------------------------------------------
C     |  There's no need for the heat loss to be less than zero, but
C     |  just in case.
C------------------------------------------------------------------------
         IF (Q_Cell.LE.0) Q_Cell = 0.
C------------------------------------------------------------------------
C     |  Scale the array Burner_Exhaust to reflect actual flow rates in fuel cell
C     |  system: Multiply terms by calculated hydrogen flow rate
C------------------------------------------------------------------------
         do ii = 1,7
            Burner_Exhaust(1,ii) = Burner_Exhaust(1,ii)*H2_flow
         enddo
C------------------------------------------------------------------------
C     |  Initialize flag for auxiliary fuel iteration
C------------------------------------------------------------------------
         DONE_2 = .FALSE.
C------------------------------------------------------------------------
C     |  Initiate bisection root search for auxiliary fuel flow rate
C------------------------------------------------------------------------
         DO WHILE ( .NOT. DONE_2 )
C------------------------------------------------------------------------
C     |     Estimate auxiliary fuel flow requirement (guess half way
C     |     between upper and lower bounds
C------------------------------------------------------------------------
            aux_fuel_M = (aux_fuel_U + aux_fuel_L) / 2. ! (kmol/s)
C------------------------------------------------------------------------
C     |     Call subroutine PEM_Aux_Burner to characterize auxiliary burner
C------------------------------------------------------------------------
            CALL PEM_Aux_Burner(AUX_inlet_data, ! Inlet composition of aux burner (input)
     &                  AUX_outlet_data, ! Exhaust composition of aux. burner (input)
     &                  Aux_Bur_EA, ! Burner excess air ratio (input, dimensionless)
     &                  Aux_Bur_util,  ! Burner fuel utilization ratio
     &                  T_reformer,  ! Reformer operating temperature
     &                  Q_aux_burner,   ! Supplementary heat delivered by burner (output, kW)
     &                  aux_fuel_M,  ! burner fuel flow (input, kmol/s)
     &                  Fuel_type)   ! fuel type flag (input)
C------------------------------------------------------------------------
C     |     Call subroutine CURVES to build temperature-enthalpy profiles
C------------------------------------------------------------------------
            CALL PEM_CURVES(Composition_cold, ! Cold curve compositions (per-mol-H2 basis, input)
     &                  Composition_hot, ! Hot curve compositions (per-mol-H2 basis, input)
     &                  TH_cold, ! Cold curve temperature-enthalpy profile (output)
     &                  TH_hot,  ! Hot curve temperature-enthalpy profile (output)
     &                  AUX_inlet_data, ! Inlet composition of aux burner (input)
     &                  cathode_air,  ! Cathode inlet conditions (input)
     &                  Q_Cell,  ! Stack surplus heat (input, kW)
     &                  burner_air,  ! Burner combustion air compositions (input)
     &                  H2_flow, ! Stack hydrogen flow rate (input, kmol/s)
     &                  AUX_outlet_data, ! Auxiliary burner exhaust compositions (input)
     &                  dH_reformer, ! Enthalpy change in the reformer (kW, output)
     &                  IPCOMP) ! Pointer to component in plant network (input)
C------------------------------------------------------------------------
C     |     Call subroutine PEM_H_Shift to characterize the performance of the
C     |     the heat recovery equipment and determine how much heat
C     |     recovery can be accomplished. PEM_H_Shift is passed arrays TH_cold
C     |     and TH_hot, which it used to determine the req'd enthalpy shift
C     |     to maintain the minumum temperature difference. PEM_H_Shift also
C     |     adjusts the enthalpy data in TH_cold to reflect the required
C     |     enthalpy shift
C------------------------------------------------------------------------
            CALL PEM_H_Shift(TH_cold, ! Cold curve temperature-enthalpy profile (input/output)
     &           TH_hot,         ! Hot curve temperature-enthalpy profile (input)
     &           AUX_outlet_data,        ! Auxiliary exhaust burner composition (input)
     &           Pressure_oper,        ! operating pressure (kPa, input)
     &           dT_P_heat_rec,        ! minimum approach temperature (K,input)
     &           Fuel_type)          ! fuel type (input)
C------------------------------------------------------------------------
C     |     Calculate the fuel processor auxiliary heat requirement:
C     |
C     |     Q req'd =   Enthalpy @ top of cold profile
C     |               - Enthalpy @ top of hot profile
C     |
C     |     See equation 4.2.10 in Ferguson 2003.
C------------------------------------------------------------------------
            FP_Q_required = (TH_cold(10, 13)-TH_hot(10, 1)) /
     &           AUX_burner_efficiency
C------------------------------------------------------------------------
C     |     Check to see if the calculated heat requirement is less than
C     |     zero. If so, the auxiliary fuel flow can be set to zero!
C------------------------------------------------------------------------
            IF (FP_Q_required.LE.0.) THEN
C------------------------------------------------------------------------
C     |        No auxiliary fuel is required. Set fuel requirement and
C     |        root search bounds to zero. This will effectively end
C     |        the auxiliary fuel iteration.
C------------------------------------------------------------------------
               FP_Q_required = 0.
               aux_fuel_L = 0.
               aux_fuel_U = 0.
               aux_fuel_M = 0.
C------------------------------------------------------------------------
C     |        Subroutines PEM_Aux_Burner, CURVES and PEM_H_Shift must be called once
C     |        more to characterize the system at zero aux. fuel flow.
C------------------------------------------------------------------------
               CALL PEM_Aux_Burner(AUX_inlet_data, ! Inlet composition of aux burner (input)
     &              AUX_outlet_data,     ! Exhaust composition of aux. burner (input)
     &              Aux_Bur_EA,     ! Burner excess air ratio (input, dimensionless)
     &              Aux_Bur_util,      ! Burner fuel utilization ratio
     &              T_reformer,      ! Reformer operating temperature
     &              Q_aux_burner,       ! Supplementary heat delivered by burner (output, kW)
     &              aux_fuel_M,      ! burner fuel flow (input, kmol/s)
     &              Fuel_type)  ! fuel type flag (input)
               CALL PEM_CURVES(Composition_cold, ! Cold curve compositions (per-mol-H2 basis, input)
     &              Composition_hot,     ! Hot curve compositions (per-mol-H2 basis, input)
     &              TH_cold,     ! Cold curve temperature-enthalpy profile (output)
     &              TH_hot,      ! Hot curve temperature-enthalpy profile (output)
     &              AUX_inlet_data,     ! Inlet composition of aux burner (input)
     &              cathode_air,      ! Cathode inlet conditions (input)
     &              Q_Cell,     ! Stack surplus heat (input, kW)
     &              burner_air,      ! Burner combustion air compositions (input)
     &              H2_flow,     ! Stack hydrogen flow rate (input, kmol/s)
     &              AUX_outlet_data,     ! Auxiliary burner exhaust compositions (input)
     &              dH_reformer,     ! Enthalpy change in the reformer (kW, output)
     &              IPCOMP)     ! Pointer to component in plant network (input)
               CALL PEM_H_Shift(TH_cold, ! Cold curve temperature-enthalpy profile (input/output)
     &              TH_hot,      ! Hot curve temperature-enthalpy profile (input)
     &              AUX_outlet_data,     ! Auxiliary exhaust burner composition (input)
     &              Pressure_oper,     ! operating pressure (kPa, input)
     &              dT_P_heat_rec,     ! minimum approach temperature (K,input)
     &              Fuel_type)  ! fuel type (input)
            ELSE
C------------------------------------------------------------------------
C     |        Evaluate enthalpy conservation in reformer:
C     |
C     |            Net heat deficit = Req'd heat - Supplied heat
C     |
C     |       (At the correct auxiliary fuel flow rate, the amount of
C     |        heat required and supplied should be equal.)
C------------------------------------------------------------------------
               H_balance = FP_Q_required - Q_aux_burner ! kW
C------------------------------------------------------------------------
C     |        If H_balance is positive, set the lower bound equal to the midpoint
C     |        (aux_fuel_M).  If H_balance is negative,set the upper bound equal to the
C     |        mid point.
C------------------------------------------------------------------------
               IF (H_balance.GT.0.) aux_fuel_L = aux_fuel_M
               IF (H_balance.LE.0.) aux_fuel_U = aux_fuel_M
            ENDIF
C------------------------------------------------------------------------
C     |     Check for convergence, and set DONE_2 accordingly.
C------------------------------------------------------------------------
            IF ((aux_fuel_U - aux_fuel_L)
     &           .LE.(relative_error*aux_fuel_M))
     &           DONE_2 = .TRUE.
         ENDDO
C------------------------------------------------------------------------
C     |  Auxiliary fuel use & exhaust composition has been determined,
C     |  store in Burner_exhaust array. These data will be used to determine
C     |  the optimal exhaust gas rejection temperature.
C------------------------------------------------------------------------
         DO ii=1,7
            Burner_exhaust(2,ii)=AUX_outlet_data(ii)
         ENDDO
C---------------------------------------------------------------------------------
C     |  Build a "cooling map", or truncated temperature-enthalpy profile that
C     |  describes the temperature-enthalpy profile of the surplus heat available,
C     |  which will be used to calculate the heat transfer to the cooling water
C     |  in the subroutine PEM_HEAT_REC.
C     |
C     |  See item 6, Section 4.2.3 in Ferguson (2003).
C     |
C     |  1. Determine enthalpy corresponding to the lowest point on the cold
C     |     (TH_cold) temperature-enthalpy profile (to the left of this point,
C     |     no heat recovery can occur and all enthalpy changes occuring
C     |     on the hot (TH_hot) temperature-enthalpy profile represent surplus
C     |     heat.
C---------------------------------------------------------------------------------
         H_cold_lowest = TH_cold(10,1)   ! (kW)
C---------------------------------------------------------------------------------
C     |  2. Loop through all segments on the hot temperature-enthalpy profile
C     |     to determine the temperature on the hot profile that
C     |     corresonds to enthalpy lowest_enthalpy:
C---------------------------------------------------------------------------------
         do jj = 1, 12          ! <- 12 hot side segments, each segment is defined
                                !    by jj and jj+1
C---------------------------------------------------------------------------------
C     |     Is Lowest_Enthalpy between enthalpies at nodes jj and jj+1?
C---------------------------------------------------------------------------------
            IF ((TH_hot(10,jj).GE.H_cold_lowest).AND.
     &           (TH_hot(10,jj+1).LT.H_cold_lowest)) THEN
C---------------------------------------------------------------------------------
C     |        Lowest_Enthalpy is between enthalpies at nodes jj and jj+1. Interopolate to
C     |        determine temperature on hot temperature-enthalpy profile
C     |        that corresponds to enthalpy lowest_enthalpy:
C     |
C     |                                         lowest_enthalpy - H_jj+1
C     |        T_X = T_jj+1 + (T_jj - T_jj+1) * ------------------------
C     |                                               H_jj - H_jj+1
C     |
C---------------------------------------------------------------------------------
               Top_HR_Temperature = TH_hot(1,jj+1)
     &              +(H_cold_lowest-TH_hot(10,jj+1))
     &              /(TH_hot(10,jj)-TH_hot(10,jj+1))
     &              *(TH_hot(1,jj)-TH_hot(1,jj+1))    ! (K)
C---------------------------------------------------------------------------------
C     |        Save data in first point of cooling map in array TH_Surplus
C---------------------------------------------------------------------------------
               TH_Surplus(1,1)=Top_HR_Temperature
               TH_Surplus(1,2)=H_cold_lowest
C---------------------------------------------------------------------------------
C     |        Start counter kk @ second point in cooling map
C---------------------------------------------------------------------------------
               kk = 2
C---------------------------------------------------------------------------------
C     |        Populate cooling map. Each node on the hot temperature-enthalpy
C     |        profile with a temperature below Top_HR_Temperaturep is saved in TH_Surplus.
C     |
C     |        Note: only the temperature and enthalpy data are saved in TH_Surplus;
C     |        subroutine PEM_HEAT_REC uses linear interpolation to estimate the enthalpy
C     |        changes occuring in the cooling product gasses, and thus does not
C     |        require the stream compositions.
C---------------------------------------------------------------------------------
               DO ll=jj+1, 13   ! <- start at node after Top_HR_Temperaturep, stop at node 13
                  IF (TH_Surplus(kk-1,2).NE.TH_hot(2,ll)) THEN
                     TH_Surplus(kk, 1)=TH_hot(1,ll)  ! Temperature (K)
                     TH_Surplus(kk, 2)=TH_hot(10,ll) ! Enthalpy  (kW)
                     kk = kk + 1 ! <- increment cooling map counter
                  ENDIF
               ENDDO
C--------------------------------------------------------------------------------
C     |        Save the number of cooling curve segments in variable iseg.
C--------------------------------------------------------------------------------
               iseg = kk - 1
            ENDIF
         ENDDO
C--------------------------------------------------------------------------------
C     |  If the maximum gross power is being calculated, the parasitic losses of
C     |  the cooling system must be characterized. It is assumed that the fuel
C     |  cell system can deliver it's rated electrical output during periods
C     |  when there is no demand for heating (ie the auxiliary cooling system
C     |  can deliver all of the required cooling). In these circumstances, the
C     |  burner exhaust will be exhausted at temperature Top_HR_Temperature, and the
C     |  cooling savings associated with rejecting the exhaust at temperature
C     |  Top_HR_Temperature instead of the fuel cell stack temperature must be calculated.
C--------------------------------------------------------------------------------
         IF ( mode.EQ.1 ) THEN
C--------------------------------------------------------------------------------
C     |     Save H2 and aux burner exhaust compositions in temporary array
C--------------------------------------------------------------------------------
            DO ii = 1, 7
               Temp_comp(ii) =  (Burner_exhaust(1,ii)
     &              +Burner_exhaust(2,ii)) * H2_flow
            ENDDO
C--------------------------------------------------------------------------------
C     |     Call PEM_del_H to determine the enthalpy savings associated with rejecting
C     |     the exhaust at TomTemp instead of the cell temp. Since Tcell < Top_HR_Temperature,
C     |     the result (dH_exhaust) should be -ive.
C--------------------------------------------------------------------------------
            CALL PEM_del_H(Temp_comp,   ! stream composition
     &           Top_HR_Temperature, ! stream start temperature
     &           T_cell,        ! stream target temperature
     &           400.,          ! Dummy variable (water saturation temp)
     &           dH_exhaust,    ! Total change in enthalpy (output, kW)
     &           400.,          ! Dummy variable (fuel saturation temp)
     &           500.,          ! Dummy variable (fuel 1)
     &           Fuel_type,     ! fuel type
     &           .FALSE.)       ! flag indicating if vaporization will occur
C-------------------------------------------------------------------------------
C     |     Calculate cooling requirement.
C     |
C     |     Q_Cooling =   Coldest enthalpy on the cold temperature-enthalpy profile
C     |             - Coldest enthalpy on the hot temperature-enthalpy profile
C     |             - Savings associated with exhaust gasses
C     |
C     |     See Equation 4.2.15, Ferguson 2003. Remember that dH_exhaust is -ive!
C-------------------------------------------------------------------------------
            Q_Cooling_Load = TH_cold(10,1)-TH_hot(10,13) +  dH_exhaust ! kWt
C-------------------------------------------------------------------------------
C     |     Calculate auxiliary system electrical requirement. Divide cooling
C     |     requirement by coefficient of performance. See Equation 4.2.16,
C     |     Ferguson, 2003.
C-------------------------------------------------------------------------------
            W_Cooling =  (Q_Cooling_Load)/cool_COP ! kWe
         ELSE
C-------------------------------------------------------------------------------
C     |     mode = 2: and cooling requirement is dependent on space heating
C     |     water resource, and will be characterized in the top-level
C     |     subroutine on each timestep. Set aux. cooling system electrical
C     |     requirement to zero for now.
C-------------------------------------------------------------------------------
            W_Cooling = 0.
         ENDIF
C-------------------------------------------------------------------------------
C     |  Determine air and methane requirements, and estimate compressor
C     |  electrical requirements.
C     |
C     |  1. Save air flow amount in temporary array temp_comp for
C     |  h20 fuel co co2 h2
C     |  AUX_inlet_data(6) is o2
C     |  temp_comp(7) is n2
C-------------------------------------------------------------------------------
         temp_comp(1) = 0.
         temp_comp(2) = 0.
         temp_comp(3) = 0.
         temp_comp(4) = 0.
         temp_comp(5) = 0.
         temp_comp(6) = (Composition_cold(7,5)
     &        + cathode_air(7,1) + burner_air(2) )
     &        * H2_flow
     &        + AUX_inlet_data(6)
         temp_comp(7) = temp_comp(6) * PEM_FCinit(13)
C-------------------------------------------------------------------------------
C     |  2. Call subroutine PEM_compressor to characterize air compressor
C-------------------------------------------------------------------------------
         CALL PEM_COMPRESSOR(temp_comp, ! Stream composition (input)
     &               ATM_pressure,  ! Inlet pressure (kPa, input)
     &               Pressure_oper, ! Outlet pressure (kPa, input)
     &               T_air_supply,  ! Supply temperature @ compressor inlet (K, input)
     &               T_air_compressor,  ! Exhaust temperature @ compressor outlet (K, output)
     &               Air_compressor_eff, ! Compressor isentropic efficiency (input)
     &               Wm_comp_air)   ! Compressor work requirement (kWm, output)
C------------------------------------------------------------------------------
C     |  3. Save natural gas amount to temporary array
C------------------------------------------------------------------------------
         IF (Fuel_type.EQ.1) THEN

C Fuel is natural gas assign H2O, Fuel, CO, CO2, H2, O2, N2.
            temp_comp(1) = 0.
            temp_comp(2) = fuel_stioch * H2_flow + AUX_inlet_data(2)
            temp_comp(3) = 0.
            temp_comp(4) = 0.
            temp_comp(5) = 0.
            temp_comp(6) = 0.
            temp_comp(7) = 0.

C-------------------------------------------------------------------------------
C     |     3. Call subroutine compre to characterize NG compressor
C-------------------------------------------------------------------------------
            CALL PEM_COMPRESSOR(temp_comp, ! Stream composition (input)
     &           ATM_pressure,  ! Inlet pressure (kPa, input)
     &           Pressure_oper, ! Outlet pressure (kPa, input)
     &           T_CH4_supply,  ! Supply temperature @ compressor inlet (K, input)
     &           T_CH4_compressor, ! Exhaust temperature @ compressor outlet (K, output)
     &           NG_compressor_eff, ! Compressor isentropic efficiency (input)
     &           Wm_comp_fuel)  ! Compressor work requirement (kWm, output)
         ELSE
C---------- Fuel is not natural gas
            Wm_comp_fuel = 0.
         ENDIF
C------------------------------------------------------------------------------
C     |  4. Calculate compressor electrical work requirements See Equation 3.3.7,
C     |     Ferguson 2003.
C------------------------------------------------------------------------------
         We_comp_total = (Wm_comp_fuel + Wm_comp_air)/Motor_efficiency ! (kWe)
C------------------------------------------------------------------------------
C     |  Characterize water and mehanol pump behavior and electrical work
C     |  requirements. See Equations 3.3.2 and 3.3.7, in Ferguson 2003
C------------------------------------------------------------------------------
C------- Water pump
         Wm_pump_water = mm_water/rho_h2o *
     &        (Pressure_oper - 101.3)*Composition_cold(2, 1) * H2_flow
     &        /Pump_efficiency  !(kWm)
         IF(Fuel_type.EQ.3) THEN
C---------- fuel is mehanol
            Wm_pump_fuel = mm_methanol/rho_methanol
     &           * (Pressure_oper - 101.3)
     &           * Composition_cold(3, 4) * H2_flow
     &           /Pump_efficiency !(kWm)
         ELSE
C---------- Fuel is NATURAL GAS, so there is no need to pump it
            Wm_pump_fuel = 0.
         ENDIF
C-------------------------------------------------------------------------------
C     |  Calculate electrical requirement of pump(s)
C     |
C     |  we_pump_total = (water pump work + fuel pump work) / (elec. motor efficiency)
C     |
C-------------------------------------------------------------------------------
         we_pump_total = (Wm_pump_water + Wm_pump_fuel)/Motor_efficiency !(kWe)
C-------------------------------------------------------------------------------
C     |  Calculate losses in power conditioner
C     |
C     |      Loss = ( 1 - PCU efficiency ) * Gross power
C-------------------------------------------------------------------------------
         PCU_losses = (1-PCU_efficiency)*P_gross ! kWe
C-------------------------------------------------------------------------------
C     |  Determine state gross power iteration:
C     |    If mode = 1, the net power must be calculated and checked for
C     |       convergence with the rated maximum power parameter. If the
C     |       iteration has not converged, the root search bounds must be
C     |       updated, and the iteration repeated.
C     |    If mode = 2, the iteration can be terminated.
C-------------------------------------------------------------------------------
         IF(mode.eq.2)  THEN
            DONE_1 = .TRUE.
         ELSE
C-------------------------------------------------------------------------------
C     |  mode = 1, calculate the net power produced by the fuel cell:
C     |
C     |    Estimated net power =   Gross power
C     |                          - power conditioner losses
C     |                          - cooling system electrical requiremnt
C     |                          - compressor electrical requirements
C     |                          - pump requirements
C-------------------------------------------------------------------------------
            p_net_guess = P_gross - PCU_losses - We_comp_total
     &           - W_Cooling - we_pump_total ! (kWe)
C-------------------------------------------------------------------------------
C     |     Calcualte the error in the net power estimate, and update root
C     |     search bounds.
C     |
C     |        Difference =  (Required net power) - (estimated net power)
C     |
C-------------------------------------------------------------------------------
            P_difference = P_net - p_net_guess
            IF (P_difference .GT. 0.) P_grossL = P_gross
            IF (P_difference .LE. 0.) P_grossU = P_gross
C------------------------------------------------------------------------
C     |     test for convergence: if iteration has converged, set flag
C     |     DONE_1 to true.
C------------------------------------------------------------------------
            IF ((P_grossU-P_grossL).LE.(relative_error*P_Net_Max))
     &           DONE_1 = .TRUE.
         ENDIF
      ENDDO
C------------------------------------------------------------------------
C     System operating point has been fully characterized. Calculate
C     additional data required for fuel cell operating map.
C
C     Total parasitic loss:
C
C     |   W_Parasitic =   compressor electrical work
C     |           + pump electrical work
C     |           + PCU electircal losses
C
C     Fuel flow rate
C
C     |  fuel flow =   fuel flow req'd to produce necessary H2
C     |              + (aux. burner fuel flow per mol H2) * (H2 flow rate)
C------------------------------------------------------------------------
      W_Parasitic =  We_comp_total  + we_pump_total + PCU_losses ! parasitic power use (kWe)
      fuel_flow = fuel_stioch * H2_flow + AUX_inlet_data(2) ! fuel flow (kmol/s)

      RETURN
      END

C===================== PEM_COMPOSITIONS ==========================================
C     Created by: Alex Ferguson
C     Created on: March 2001
C     Copyright:  Alex Ferguson 2001 / CETC 2003.
C----------------------------------------------------------------------
C     ABSTRACT:
C
C----------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C
C----------------------------------------------------------------------
C     REVISIONS:
C     SEPT 2003: Code was heavily revised to comply with CETC coding
C                standard and improve general readability.
C======================================================================
      SUBROUTINE PEM_Compositions(
     &     Composition_cold,
     &     Composition_hot,
     &     cathode_air,
     &     burner_air,
     &     fuel_stioch,
     &     IPCOMP,
     &     Burner_exhaust)
      IMPLICIT NONE
#include "plant.h"

C-------------------------------------------------------------------------------
C     ESP-r common block required for fuel cell description (ADATA)
C-------------------------------------------------------------------------------
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
C-------------------------------------------------------------------------------
C     Declare initialization function
C-------------------------------------------------------------------------------
      REAL PEM_FCinit
C-------------------------------------------------------------------------------
C     Variable Definitions
C-------------------------------------------------------------------------------
C---- ESP-r Common blocks
      REAL ADATA,BDATA
C---- Passed Variables
      REAL Composition_cold(10,30), Composition_hot(10,30)
                                ! Arrays containing stream compositions
                                !    on a per/mol H2 basis for all
                                !    cold (Composition_cold) and hot (Composition_hot)
                                !    streams. Populated by subroutine
                                !    PEM_compositions
      REAL cathode_air(10,2)    ! Array containing air composition
                                !    at cathode inlet and outlet
      REAL burner_air(3)        ! Array containing conditions at
                                !    burner combustion air inlet
      REAL fuel_stioch          ! fuel flow required to deliver 1 mol/s
                                !    of hydrogen at anode inlet
      INTEGER IPCOMP            ! pointer to component in plant network
      REAL Burner_exhaust(2,7)  ! Array containing composition of the
                                !    burner exhaust.
C---- FUEL CELL DESCRIPTION (inputs)
      integer Fuel_type         ! fuel type (1= methane, 3 = methanol)
      REAL reformer_util        ! reformer fuel utilization (0->1)
      REAL Stack_util           ! stack fuel utilization (0->1)
      REAL H2_burner_util       ! H2 burner fuel utilization
      REAL Steam_to_carbon                 ! steam to carbon ratio
      REAL Stack_EA               ! stack excess air ratio
      REAL PROX_EA               ! prox excess air ratio
      REAL H2_burner_EA               ! H2 burner excess air ratio
C---- Local variables
      REAL atemp                ! fraction of carbon monoxide that remains unshifted
                                !    in the reformane after passing through the reformer
                                !    HTWS and LTWS reactors.
      REAL mol_C, mol_H, mol_O  ! variables describing the composition of the fuel
                                !    (CxHyOz) to be reacted in the fuel processor
      REAL H2O_Stioch               ! Fuel processor stiocheometric water flow
      REAL a_ref,a_htws,a_ltws  ! Fractions of water feed entering reformer, HTWS
                                !    and ltws reactors
      REAL S_refo,S_htws,S_ltws, S_prox ! carbon monoxide shift fractions for reformer
                                !    htws, ltws and prox reactors.
      REAL AP1, AP2, AP3        ! Coefficients used to simpifly fuel calculation
      INTEGER ii, jj               ! Counters
C-------------------------------------------------------------------------------
C     Read fuel cell description from ADATA array.
C-------------------------------------------------------------------------------
C     Fuel Type
      Fuel_type=int(ADATA(IPCOMP, 1))
C     Reformer fuel Utilization
      reformer_util =  ADATA(IPCOMP, 19)
C     Stack fuel Utilization
      Stack_util = ADATA(IPCOMP, 20)
C     Hydrogen Burner fuel Utilization
      H2_burner_util =  ADATA(IPCOMP, 22)
C     Steam-to-carbon ratio
      Steam_to_carbon =   ADATA(IPCOMP, 23)
C     Cell stack excess air amount (CATHODE EXCESS AIR)
      Stack_EA = ADATA(IPCOMP, 24)
C     PROX excess air amount
      PROX_EA = ADATA(IPCOMP, 25)
C     Hydrogen burner excess air amount
      H2_burner_EA = ADATA(IPCOMP, 27)
C     Reformer Shift Percentage
      S_refo =   ADATA(IPCOMP, 28)
C     HTWS Shift Percentage
      S_htws =   ADATA(IPCOMP, 29)
C     LTWS Shift Percentage
      S_ltws =   ADATA(IPCOMP, 30)
C     Prox Shift Percentage
      S_prox =   ADATA(IPCOMP, 31)
C---------------------------------------------------------------------
C     Set mol_C, mol_H, mol_O variables for fuel type CxHyOz
C---------------------------------------------------------------------
      IF (Fuel_type.EQ.1) THEN
C------- Methane
         mol_C = 1.
         mol_H = 4.
         mol_O = 0.
      ELSEIF (Fuel_type.EQ.2) THEN
C------- Octane (This fuel type has never been tested !!!)
         mol_C = 8.
         mol_H = 18.
         mol_O = 0.
      ELSEIF (Fuel_type.EQ.3) THEN
C------- Methanol
         mol_C = 1.
         mol_H = 4.
         mol_O = 1.
      ENDIF
C----------------------------------------------------------------------------
C     Calculate stiochiometric water requirement H2O_Stioch, on a
C     "per mol fuel basis" - ie: # mols required per mol fuel
C     reacted in the fuel processor. See Equation 3.2.18, Ferguson
C     2003; note that the reformer fuel utilization and and fuel
C     flow rate have been omitted from this equation.
C-----------------------------------------------------------------------------
      H2O_Stioch = (mol_C-mol_O) + mol_c *
     &     (1-(1-S_refo)*(1-s_HTWS)*(1-s_ltws)) ! (mol/mol fuel reacted)
C-----------------------------------------------------------------------------
C     Calculate the fraction of the water stream that must be diveted to each
C     of the reactors in the fuel processor. Each reactor's stiochiometric
C     water requirement is divided by the fuel processor total stiochiometric
C     water requirement (variable H2O_Stioch caclulated above). See Equations
C     3.2.19-3.2.24 in Ferguson 2003. Note that the reformer fuel utilization
C     and fuel flow rate terms have been omitted here; they're also ommited
C     from the calculation of the denominator above (H2O_Stioch).
C----------------------------------------------------------------------------
      a_ref = (mol_C-mol_O + mol_C*S_refo) / (H2O_Stioch) ! reformer water feed fraction
      a_htws = mol_C * S_htws * (1-S_refo) / (H2O_Stioch) ! HTWS water feed fraction
      a_ltws = mol_C * S_ltws * (1-S_htws) * (1-S_refo) / (H2O_Stioch) ! LTWS water feed fraction
C----------------------------------------------------------------------------
C     Calculate the fuel flow req'd to produce a single mol of hydrogen in
C     the fuel processor. See equations 3.2.13-3.2.16 in Ferguson 2003
C     variables aP1-aP3 represent coefficients used in the final equation
C----------------------------------------------------------------------------
      aP1 = 2.*mol_C + mol_H/2.-mol_O ! Dimensionless
      aP2 = mol_C*(1-S_refo)*(1-S_htws)*(1-S_ltws) ! Dimensionless
      aP3 = mol_C*(1-S_refo)*(1-S_htws)*(1-S_ltws)*(PROX_EA-1) ! Dimensionless
      fuel_stioch = 1./reformer_util/(aP1-aP2-aP3) ! (mol fuel req'd / mol H2 produced)
C-----------------------------------------------------------------------------
C     Calculate the fraction of carbon monoxide that remains unshifted after
C     passing through the reformer, htws and ltws reactors. Since each reactor
C     i shifts fraction Si from CO TO CO2, the amount of CO remaining in the
C     reformate is:
C
C        Frac Remaining = (1 - S_reformer) * (1 - S_HTWS) * (1 - S_LTWS)
C
C     This data will be used later, when the array Composition_cold is populated.
C
C-----------------------------------------------------------------------------
      atemp = (1. - S_refo) * (1. - S_htws) * (1. - S_ltws)
C-----------------------------------------------------------------------------
C     The array Composition_cold(species, location) is used to store the compositions of the
C     fuel processor species at all locations on a per-mol-H2 basis:
C                            Molar flow rate of species X at location I
C        Per mol H2 basis = ------------------------------------------
C                            Molar flow rate of H2 entering fuel cell
C
C     Each species in the fuel processor is assigned a certian location in the
C     first rank of the array in the array:
C
C       Location:
C           1. not used
C           2. water
C           3. fuel
C           4. CO
C           5. CO2
C           6. H2
C           7. O2
C           8. N2
C
C     Each location in the second rank of Composition_cold corresponds to the start or
C     end of a heating process, and the locations are sequential:
C
C       Location #:
C           1. Water stream starts (a_ref + a_htws + a_ltws)
C           2. Start of water vapourization
C           3. end of water vapourization
C           4. Fuel stream starts
C           5. Air stream starts - sent from compressor/blower
C           6. Air stream ends - air goes to PROX reactor
C           7. End of water stream a_ltws - goes to LTWS
C           8. End of water stream a_htws - goes to HTWS
C           9. End of water stream a_ref and fuel stream - go to reformer
C
C     The following code assigns the appropriate species composition to
C     locations in the array Composition_cold:
C
C     1. Add water feed to array locations rComp(2,1) -> rComp(2,9)
C-----------------------------------------------------------------------------
      DO jj = 1, 9
         Composition_cold(2, jj) = Steam_to_carbon * mol_C * fuel_stioch ! (mol/mol H2 produced)
      ENDDO
C-----------------------------------------------------------------------------
C     2. Add fuel feed to array locations rComp(3,4) -> rComp(3,9)
C-----------------------------------------------------------------------------
      DO  jj = 4, 9
         Composition_cold(3, jj) = fuel_stioch   ! (mol/mol H2 produced)
      ENDDO
C-----------------------------------------------------------------------------
C     3. Add O2/N2 feed to array locations rComp(7/8,5) -> rComp(7/8,9)
C-----------------------------------------------------------------------------
      DO  jj = 5, 9
         Composition_cold(7, jj) = mol_C
     &        * PROX_EA * fuel_stioch * atemp * reformer_util * 0.5
     &        + (1-reformer_util)*(mol_C+mol_H/4.-mol_C/2)* fuel_stioch ! (mol O2 /mol H2 produced)
         Composition_cold(8, jj) =
     &        PEM_FCinit(13) * Composition_cold(7, jj) ! (mol N2 /mol H2 produced)
      ENDDO
C-----------------------------------------------------------------------------
C     4. Subtract fraction of air feed that is transferred to PROX reactor
C        from array locations rComp(7/8,6) -> rComp(7/8,9)
C-----------------------------------------------------------------------------
      DO jj = 6, 9
         Composition_cold(7, jj) = Composition_cold(7, jj)
     &        - (mol_C * PROX_EA * fuel_stioch
     &        * atemp * reformer_util * 0.5  +
     &        (1-reformer_util)*(mol_C+mol_H/4-mol_C/2)* fuel_stioch) ! (mol O2 /mol H2 produced)
         Composition_cold(8, jj) = Composition_cold(8, jj)
     &        -PEM_FCinit(13)*(mol_C * PROX_EA * fuel_stioch
     &        * atemp * reformer_util * 0.5  +
     &        (1-reformer_util)*(mol_C+mol_H/4-mol_C/2)* fuel_stioch) ! (mol N2 /mol H2 produced)
      ENDDO
C-----------------------------------------------------------------------------
C     4. Subtract fraction of water feed that is transferred to LTWS reactor
C        from array locations rComp(2,7) -> rComp(2,9)
C-----------------------------------------------------------------------------
      DO jj = 7, 9
         Composition_cold(2, jj) = mol_C*(a_ref+a_htws)*Steam_to_carbon
     &        * fuel_stioch     ! (mol H2O/mol H2 produced)
      ENDDO
C-----------------------------------------------------------------------------
C     4. Subtract fraction of water feed that is transferred to HTWS reactor
C        from array locations rComp(2,8) -> rComp(2,9)
C-----------------------------------------------------------------------------
      DO jj = 8, 9
         Composition_cold(2, jj) = mol_C*a_ref
     &        * Steam_to_carbon * fuel_stioch ! (mol H2O/mol H2 produced)
      ENDDO
C-----------------------------------------------------------------------------
C     The array Composition_hot(species, location) is used to store the compositions of the
C     fuel processor species at all locations on a per-mol-H2 basis:
C
C                           Molar flow rate of species X at location I
C        Per mol H2 basis = ------------------------------------------
C                            Molar flow rate of H2 entering fuel cell
C
C     Each species in the fuel processor is assigned a location in the first
C     rank of Composition_hot
C
C       Slot:
C           1. not used
C           2. water
C           3. fuel
C           4. CO
C           5. CO2
C           6. H2
C           7. O2
C           8. N2
C
C     Each location in the second rank in Composition_hot corresponds to the start
C     or end of a COOLING process, and the locations are sequential:
C
C       Location #:
C           1. Reformer outlet
C           2. HTWS Inlet
C           3. HTWS Outlet
C           4. LTWS Inlet
C           5. LTWS Outlet
C           6. PROX Inlet
C           7. PROX outlet
C           8. FC Anode inlet
C           9. FC Anode outlet
C          10. Burner inlet (not including combustion air)
C          11. Burner outlet
C
C     The following code assigns the appropriate species composition to
C     locations in the array Composition_hot:
C
C     1. Composition of reformante at outlet of reformer
C-----------------------------------------------------------------------------
      Composition_hot(2, 1) = mol_C*a_ref*Steam_to_carbon*fuel_stioch -
     &     reformer_util*((mol_C-mol_O)+mol_C*S_refo)*fuel_stioch ! H2O
      Composition_hot(3, 1) = (1.-reformer_util)*fuel_stioch ! fuel
      Composition_hot(4, 1) = mol_C * reformer_util
     & * (1. - S_refo)*fuel_stioch ! CO
      Composition_hot(5, 1) = mol_C*reformer_util*S_refo*fuel_stioch ! CO2
      Composition_hot(6, 1) = (mol_C*(1. + S_refo)
     &     + mol_H/2. - mol_O)*reformer_util*fuel_stioch ! H2
      Composition_hot(7, 1) = 0.0 ! O2
      Composition_hot(8, 1) = 0.0 ! N2
C-----------------------------------------------------------------------------
C     2. Composition at inelt of HTWS reactor - reformer exhaust + additional
C        water to be reacted in HTWS
C     for h2o fuel co co2 h2 o2 n2
C-----------------------------------------------------------------------------
      Composition_hot(2, 2) = Composition_hot(2, 1)
     &     + mol_C*Steam_to_carbon*a_htws*fuel_stioch
      Composition_hot(3, 2) = Composition_hot(3, 1)
      Composition_hot(4, 2) = Composition_hot(4, 1)
      Composition_hot(5, 2) = Composition_hot(5, 1)
      Composition_hot(6, 2) = Composition_hot(6, 1)
      Composition_hot(7, 2) = Composition_hot(7, 1)
      Composition_hot(8, 2) = Composition_hot(8, 1)
C-----------------------------------------------------------------------------
C     3. Composition at outlet of HTWS reactor
C     for h2o fuel co co2 h2 o2 n2
C-----------------------------------------------------------------------------
      Composition_hot(2, 3) = Composition_hot(2, 2)
     &     - S_htws*Composition_hot(4, 2)
      Composition_hot(3, 3) = Composition_hot(3, 2)
      Composition_hot(4, 3) = (1. - S_htws)*Composition_hot(4, 2)
      Composition_hot(5, 3) = Composition_hot(5, 2)
     &     + S_htws*Composition_hot(4, 2)
      Composition_hot(6, 3) = Composition_hot(6, 2)
     &     + S_htws*Composition_hot(4, 2)
      Composition_hot(7, 3) = 0.
      Composition_hot(8, 3) = 0.
C-----------------------------------------------------------------------------
C     4. Composition at inelt of LTWS reactor - HTWS exhaust + additional
C        water to be reacted in LTWS
C-----------------------------------------------------------------------------
      Composition_hot(2, 4) = Composition_hot(2, 3)
     &     + mol_C *Steam_to_carbon *a_ltws*fuel_stioch ! H2O
      Composition_hot(3, 4) = Composition_hot(3, 3)                         ! FUEL
      Composition_hot(4, 4) = Composition_hot(4, 3)                         ! CO
      Composition_hot(5, 4) = Composition_hot(5, 3)                         ! CO2
      Composition_hot(6, 4) = Composition_hot(6, 3)                         ! H2
      Composition_hot(7, 4) = Composition_hot(7, 3)                         ! O2
      Composition_hot(8, 4) = Composition_hot(8, 3)                         ! N2
C-----------------------------------------------------------------------------
C     5. Composition at outlet of LTWS reactor
C-----------------------------------------------------------------------------
      Composition_hot(2, 5) = Composition_hot(2, 4)
     &     -S_ltws*Composition_hot(4,4) ! H2O
      Composition_hot(3, 5) = Composition_hot(3, 4) ! FUEL
      Composition_hot(4, 5) = Composition_hot(4, 4)
     &     *(1.-S_ltws)         ! CO
      Composition_hot(5, 5) = Composition_hot(5, 4)
     &     +  S_ltws*Composition_hot(4, 4) ! CO2
      Composition_hot(6, 5) = Composition_hot(6, 4)
     &     +  S_ltws*Composition_hot(4, 4) ! H2
      Composition_hot(7, 5) = 0.0 ! O2
      Composition_hot(8, 5) = 0.0 ! N2
C-----------------------------------------------------------------------------
C     6. Composition at inelt of PROX reactor - LTWS exhaust + additional
C        air to be reacted in PROX
C-----------------------------------------------------------------------------
      Composition_hot(2, 6) = Composition_hot(2, 5) ! H2O
      Composition_hot(3, 6) = Composition_hot(3, 5) ! FUEL
      Composition_hot(4, 6) = Composition_hot(4, 5) ! CO
      Composition_hot(5, 6) = Composition_hot(5, 5) ! CO2
      Composition_hot(6, 6) = Composition_hot(6, 5) ! H2
      Composition_hot(7, 6) = Composition_hot(7, 5)
     &     + PROX_EA*Composition_hot(4, 6)*0.5 !O2
      Composition_hot(8, 6) = Composition_hot(8, 5)
     &     + (PROX_EA*Composition_hot(4, 6)*0.5)* PEM_FCinit(13) !N2
C-----------------------------------------------------------------------------
C     7. Composition at outlet of PROX reactor
C-----------------------------------------------------------------------------
      Composition_hot(2, 7) = Composition_hot(2, 6)
     &     + 2*(PROX_EA-1)*Composition_hot(4, 6)*0.5  +
     &     mol_H/2.*(Composition_hot(3, 5))   ! H2O
      Composition_hot(3, 7) = 0.              ! FUEL
      Composition_hot(4, 7) = Composition_hot(4, 6)
     &     *(1. - S_prox)       ! CO
      Composition_hot(5, 7) = Composition_hot(5, 6)
     &     + Composition_hot(4, 6)*S_prox
     &     + mol_C*(Composition_hot(3, 5))    ! CO2
      Composition_hot(6, 7) = Composition_hot(6, 6)
     &     - 2*(PROX_EA-1)*Composition_hot(4, 6)*0.5 ! H2
      Composition_hot(7, 7) = 0.0 ! O2
      Composition_hot(8, 7) = Composition_hot(8, 6) ! N2
C-----------------------------------------------------------------------------
C     8. Composition at inlet of fuel cell stack anode - same as at outlet
C        of prox reactor
C-----------------------------------------------------------------------------
      Composition_hot(2, 8) = Composition_hot(2, 7) ! H2O
      Composition_hot(3, 8) = Composition_hot(3, 7) ! FUEL
      Composition_hot(4, 8) = Composition_hot(4, 7) ! CO
      Composition_hot(5, 8) = Composition_hot(5, 7) ! CO2
      Composition_hot(6, 8) = Composition_hot(6, 7) ! H2
      Composition_hot(7, 8) = Composition_hot(7, 7) ! O2
      Composition_hot(8, 8) = Composition_hot(8, 7) ! N2
C----------------------------------------------------------------------------
C     9. Composition at outlet of fuel cell anode
C----------------------------------------------------------------------------
      Composition_hot(2, 9) = Composition_hot(2, 8) ! H2O
      Composition_hot(3, 9) = Composition_hot(3, 8) ! FUEL
      Composition_hot(4, 9) = Composition_hot(4, 8) ! CO
      Composition_hot(5, 9) = Composition_hot(5, 8) ! CO2
      Composition_hot(6, 9) = (1. - Stack_util) * Composition_hot(6, 8) ! H2
      Composition_hot(7, 9) = Composition_hot(7, 8) ! O2
      Composition_hot(8, 9) = Composition_hot(8, 8) ! N2
C----------------------------------------------------------------------------
C     10. Hydrogen burner inlet composition - same as fuel cell anode  outlet
C         composition. Note: combustion air is stored separately.
C----------------------------------------------------------------------------
      Composition_hot(2, 10) = Composition_hot(2, 9) ! H2O
      Composition_hot(3, 10) = Composition_hot(3, 9) ! FUEL
      Composition_hot(4, 10) = Composition_hot(4, 9) ! CO
      Composition_hot(5, 10) = Composition_hot(5, 9) ! CO2
      Composition_hot(6, 10) = Composition_hot(6, 9) ! H2
      Composition_hot(7, 10) = Composition_hot(7, 9) ! O2
      Composition_hot(8, 10) = Composition_hot(8, 9) ! N2
C----------------------------------------------------------------------------
C     11. Hydrogen burner outlet composition
C----------------------------------------------------------------------------
      Composition_hot(2, 11) = Composition_hot(2, 10)
     &     + H2_burner_util * (Composition_hot(6, 10) + mol_H/2.
     &     * Composition_hot(3, 10)) ! H2O
      Composition_hot(3, 11) = Composition_hot(3, 10)
     &     * (1. - H2_burner_util) ! Fuel
      Composition_hot(4, 11) = Composition_hot(4, 10) ! CO
      Composition_hot(5, 11) = Composition_hot(5, 10)
     &     + H2_burner_util * mol_C
     &     * Composition_hot(3, 10) ! CO2
      Composition_hot(6, 11) = Composition_hot(6, 10)
     &     * (1. - H2_burner_util) ! H2
      Composition_hot(7, 11) = Composition_hot(7, 10)
     &     + burner_air(2) - H2_burner_util *
     &     (0.5 * Composition_hot(6, 10) + (mol_C + mol_H/4. - mol_O/2.) ! O2
     &     * Composition_hot(3, 10))
      Composition_hot(8, 11) = Composition_hot(8, 10) + burner_air(3) ! N2
C--------------------------------------------------------------------------------------
C     The composition of Air at the on the cathode inlet and outlet are contained
C     in a separate array cathode_air. Air composition on a per-mol-H2-basis follows:
C
C     1. Cathode inlet
C--------------------------------------------------------------------------------------
      cathode_air(2, 1) = 0.0    ! H2O
      cathode_air(3, 1) = 0.0    ! FUEL
      cathode_air(4, 1) = 0.0    ! CO
      cathode_air(5, 1) = 0.0    ! CO2
      cathode_air(6, 1) = 0.0    ! H2
      cathode_air(7, 1) = Stack_EA * 0.5 * Composition_hot(6, 8) ! O2
      cathode_air(8, 1) = PEM_FCinit(13) * cathode_air(7, 1) ! N2
C--------------------------------------------------------------------------------------
C     2. Cathode outlet
C--------------------------------------------------------------------------------------
      cathode_air(2, 2) = cathode_air(2, 1)
     &     + Stack_util * Composition_hot(6, 8) ! H2O
      cathode_air(3, 2) = cathode_air(3, 1) ! FUEL
      cathode_air(4, 2) = cathode_air(4, 1) ! CO
      cathode_air(5, 2) = cathode_air(5, 1) ! CO2
      cathode_air(6, 2) = cathode_air(6, 1) ! H2
      cathode_air(7, 2) = cathode_air(7, 1)
     &     - 0.5 * (Stack_util * Composition_hot(6, 8)) ! O2
      cathode_air(8, 2) = cathode_air(8, 1) ! N2
C---------------------------------------------------------------------------------------
C     Data describing the combistion air used in the burner is stored in
C     array burner_air(item)
C
C     Three items are stored in burner_air:
C          1. Air temperature
C          2. Oxygen concentration
C          3. Nitrogen concentration
C
C     At the present time, the oxygen and nitrogen composition data are calculated.
C     the temperature data is calculated in subroutine CURVES
C---------------------------------------------------------------------------------------
      burner_air(2) = H2_burner_EA * (0.5*Composition_hot(6, 10) +
     &     (mol_C + mol_H/4. - mol_O/2.)*Composition_hot(3, 10))
      burner_air(3) = burner_air(2) * PEM_FCinit(13)
C---------------------------------------------------------------------------------------
C     The hydrogen exhaust composition data is stored in array Burner_exhaust for use in
C     other subroutines
C---------------------------------------------------------------------------------------
      do ii = 1,7
         Burner_exhaust(1,ii) = Composition_hot(ii+1, 11)
      enddo

      RETURN
      END


C===================== PEM_FCther ===================================
C     Created by: Alex Ferguson
C     Created on: March 2001
C     Copyright:  Alex Ferguson 2001 / CETC 2003.
C----------------------------------------------------------------------
C     ABSTRACT:
C     This subroutine characterizes the theoretical (zero load) performance
C     of the fuel cell, and determines the total enthalpy change and total
C     entropy change that occurs within the fuel cell stack per mol H2 that
C     enters the stack.  This routine is passed two arrays FC_inlet_data and rCoFc0
C     that contain data on the fuel cell stack inlets and outlets, respectively:
C
C     FC_inlet_data:
C     FC_inlet_data(1,1)    = anode inlet temperature
C     FC_inlet_data(2->8,1) = anode inlet compositions
C     FC_inlet_data(1,2)    = cathode inlet temperature
C     FC_inlet_data(2->8,2) = cathode inlet compositions
C     FC_outlet_data:
C     FC_outlet_data(1,1)    = anode outlet temperature
C     FC_outlet_data(2->8,1) = anode outlet compositions
C     FC_outlet_data(1,2)    = cathode outlet temperature
C     FC_outlet_data(2->8,2) = cathode outlet compositions
C
C     Note:
C     The model currently assumes that the stack is isothermal, and
C     the inclusion of seperate temperature data for all inlets and
C     outlets is unnecessary in the current implementation.
C
C-----------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C
C----------------------------------------------------------------------
C     REVISIONS:
C     SEPT 2003: Code was heavily revised to comply with CETC coding
C                standard and improve general readability.
C======================================================================
      SUBROUTINE PEM_FCther(
     &     FC_inlet_data,       ! input
     &     FC_outlet_data,      ! input
     &     max_efficiency,      ! output
     &     dH_stack,            ! output
     &     pressures,           ! input
     &     Fuel_type            ! input
     &     )
      IMPLICIT NONE
C----------------------------------------------------------------------
C     ESP-r function delcarations
C----------------------------------------------------------------------
      REAL SHTFLD
C----------------------------------------------------------------------
C     Declare initialization function
C----------------------------------------------------------------------
      REAL PEM_FCinit
C----------------------------------------------------------------------
C     Fuel cell model function declarations
C----------------------------------------------------------------------
C---- Enthalpy functions
      REAL PEMf_hSTEAM, PEMf_hCH4, PEMf_hOCTg, PEMf_hMETHg,
     &     PEMf_hN2, PEMf_hH2, PEMf_hO2, PEMf_hCO, PEMf_hCO2
C---- Entropy Functions
      REAL PEMf_SSTEAM, PEMF_SCH4, PEMf_SOCTg,
     &     PEMf_SMETHg, PEMF_SN2, PEMF_SH2,
     &     PEMF_SO2, PEMF_SCO, PEMF_SCO2
C----------------------------------------------------------------------
C     VARIABLE DECLARATIONS
C----------------------------------------------------------------------
C---- Passed variables
      REAL FC_inlet_data(10,2)  ! Array describing stream compositions
                                ! at inlet of anode and cathode (input)
      REAL FC_outlet_data(10,2) ! Array describing stream compositions
                                ! at outlet of anode and cathode (input)
      REAL pressures(3)         ! Array describing pressure at anode
                                ! and cathode inlet (input)
      integer Fuel_type         ! integer flag indicating fuel type (input
      REAL dH_stack             ! enthalpy change in cell on a per-mol-
                                ! H2-basis (output)
      REAL max_efficiency       ! Maximum theoretical efficiency of cell
                                ! (output)
C---- Local Variables
      REAL hf_H2Ol              ! Enthalpy of formation of liquid water (kj/mol)
      REAL hf_H2Ov              ! Enthalpy of formation of vapor water (kj/mol)
      REAL hf_CH4               ! Enthalpy of formation of methane (kj/kmol)
      REAL hf_CO                ! Enthalpy of formation of carbon monoxide (kj/kmol)
      REAL hf_CO2               ! Enthalpy of formation of carbon dioxide (kj/kmol)
      REAL hf_H2                ! Enthalpy of formation of hydrogen (kj/kmol)
      REAL hf_N2                ! Enthalpy of formation of nitrogen (kj/kmol)
      REAL hf_O2                ! Enthalpy of formation of oxygen (kj/kmol)
      REAL hf_OCT               ! Enthalpy of formation of octane (kj/kmol)
      REAL hf_Meth              ! Enthalpy of formation of methane (kj/kmol)
      REAL T_cell               ! fuel cell operating temperature
      REAL Cp_Water_L           ! Specific heat of water (kj/kmol)
      REAL REF_pressure         ! reference pressure (kPa)
      REAL Anode_Inlet_Mols, Cathode_Inlet_Mols ! Total number of mols entering anode, cathode
      REAL Anode_Outlet_Mols, Cathode_Outlet_Mols ! Total number of mols exiting anode, cathode
      REAL Anode_inlet_pressure, Anode_outlet_pressure ! Anode inlet and outlet pressure (total, kPa)
      REAL rPCiA, rPCoA         ! Cathode inlet and outlet pressure (total, kPa)
      REAL rPAi, rPAo           ! Partial pressure on anode inlet/outlet (kPa)
      REAL rPCi, rPCo           ! Partial p%ressure on cathode inlet/outlet (kPa)
      REAL sf_H2Ol              ! Entropy of formation of liquid water (kj/mol)
      REAL sf_H2Ov              ! Entropy of formation of vapor water (kj/mol)
      REAL sf_CH4               ! Entropy of formation of methane (kj/kmol)
      REAL sf_CO                ! Entropy of formation of carbon monoxide (kj/kmol)
      REAL sf_CO2               ! Entropy of formation of carbon dioxide (kj/kmol)
      REAL sf_H2                ! Entropy of formation of hydrogen (kj/kmol)
      REAL sf_N2                ! Entropy of formation of nitrogen (kj/kmol)
      REAL sf_O2                ! Entropy of formation of oxygen (kj/kmol)
      REAL sf_OCT               ! Entropy of formation of octane (kj/kmol)
      REAL sf_Meth              ! Entropy of formation of methane (kj/kmol)
      REAL dS_stack             ! Change in entropy in fuel cell
      INTEGER ii                ! Counter
      REAL REF_temperature                  ! Reference temperature (K)
      REAL mm_water             ! Water molar mass (kg/kmol)
C---- Variables used to evaluate enthalpy difference in fuel cell stack
      REAL Hi_H2Oa, Hi_FUELa, Hi_COa, Hi_CO2a, Hi_H2a, Hi_O2a, Hi_N2a ! Enthalpy at inlet of anode
      REAL Ho_H2Oa, Ho_FUELa, Ho_COa, Ho_CO2a, Ho_H2a, Ho_O2a, Ho_N2a ! Enthalpy at outlet of anode
      REAL dHa_H2O, dHa_FUEL, dHa_CO, dHa_CO2, dHa_H2, dHa_O2, dHa_N2 ! Change in enthalpy in anode
      REAL Hi_H2Ob, Hi_FUELb, Hi_COb, Hi_CO2b, Hi_H2b, Hi_O2b, Hi_N2b ! Enthalpy at inlet of cathode
      REAL Ho_H2Ob, Ho_FUELb, Ho_COb, Ho_CO2b, Ho_H2b, Ho_O2b, Ho_N2b ! Enthalpy at outlet of cathode
      REAL dHb_H2O, dHb_FUEL, dHb_CO, dHb_CO2, dHb_H2, dHb_O2, dHb_N2 ! Change in enthalpy in cathode
C---- Variables used to evaluate entropy differences in fuel cell stack
      REAL Si_H2Oa, Si_FUELa, Si_COa, Si_CO2a, Si_H2a, Si_O2a, Si_N2a ! Enthalpy at inlet of anode
      REAL S2_H2Oa, S2_FUELa, S2_COa, S2_CO2a, S2_H2a, S2_O2a, S2_N2a ! Enthalpy at outlet of anode
      REAL dSa_H2O, dSa_FUEL, dSa_CO, dSa_CO2, dSa_H2, dSa_O2, dSa_N2 ! Change in enthalpy in anode
      REAL Si_H2Ob, Si_FUELb, Si_COb, Si_CO2b, Si_H2b, Si_O2b, Si_N2b ! Enthalpy at inlet of cathode
      REAL S2_H2Ob, S2_FUELb, S2_COb, S2_CO2b, S2_H2b, S2_O2b, S2_N2b ! Enthalpy at outlet of cathode
      REAL dSb_H2O, dSb_FUEL, dSb_CO, dSb_CO2, dSb_H2, dSb_O2, dSb_N2 ! Change in enthalpy in cathode
C---------------------------------------------------------------------
C     Declare enthalpies of formation at 25 oC - KJ/Kmol
C---------------------------------------------------------------------
      hf_H2Ov = PEM_FCinit(16)       ! Water vapor
      hf_H2Ol = PEM_FCinit(17)       ! liquid water
      hf_CO   = PEM_FCinit(18)       ! Carbon Monoxide
      hf_CO2  = PEM_FCinit(19)       ! Carbon Dioxide
      hf_H2   = PEM_FCinit(20)       ! Hydrogen
      hf_O2   = PEM_FCinit(21)       ! Oxygen
      hf_N2   = PEM_FCinit(22)       ! Nitrogen
      hf_CH4  = PEM_FCinit(23)       ! Methane
      hf_OCT  = PEM_FCinit(24)       ! Octane
      hf_Meth = PEM_FCinit(25)       ! Methanol
C---------------------------------------------------------------------
C     Declare absolute entropies at 25 oC / 101.3 kPa - KJ/Kmol
C---------------------------------------------------------------------
      sf_H2Ov = PEM_FCinit(26)       ! Water vapor
      sf_H2Ol = PEM_FCinit(27)       ! liquid water
      sf_CO   = PEM_FCinit(28)       ! Carbon Monoxide
      sf_CO2  = PEM_FCinit(29)       ! Carbon Dioxide
      sf_H2   = PEM_FCinit(30)       ! Hydrogen
      sf_O2   = PEM_FCinit(31)       ! Oxygen
      sf_N2   = PEM_FCinit(32)       ! Nitrogen
      sf_CH4  = PEM_FCinit(33)       ! Methane
      sf_OCT  = PEM_FCinit(34)       ! Octane
      sf_Meth = PEM_FCinit(35)       ! Methanol
C---------------------------------------------------------------------
C     Declare reference pressure / temperature for absolute entropies
C---------------------------------------------------------------------
      REF_temperature = PEM_FCinit(14) ! K
      REF_pressure = PEM_FCinit(15) ! kPa
C---------------------------------------------------------------------
C     Read cell operating temperature
C---------------------------------------------------------------------
      T_cell = FC_inlet_data(1,1)      !(K)
C---------------------------------------------------------------------
C     Water molar mass
C---------------------------------------------------------------------
      mm_water = PEM_FCinit(9)         !(kg/kmol)
C---------------------------------------------------------------------
C     Determine specific heat of water using SHTFLD function -
C     Remember to multiply by molar mass to convert to KJ/Kmol!
C---------------------------------------------------------------------
      Cp_Water_L= (SHTFLD(3,T_cell) + SHTFLD(3,REF_temperature))/2000.
     &     * mm_water            ! (kJ/Kmol)
C---------------------------------------------------------------------
C     Determine cell enthalpy change - REF: 298K, 101.32 kPa (per mol
C     H2 entering the cell)
C
C     THIS next chunk of code determines the enthalpy change that each
C     species undergoes as it moves through the cell and reacts. For both
C     the anode and cahtode, the following is completed for each species i:
C
C        H_inlet  = Composition_inlet*
C                   (Enthalpy_formation + Enthalpy(T_inlet) - Enthalpy(REF_temperature)
C
C        H_outlet = Composition_outlet*
C                   (Enthalpy_formation + Enthalpy(T_outletlet) - Enthalpy(REF_temperature)
C
C        H_change = (H_outlet - H_inlet)
C
C        WHERE:
C               Enthalpy(Temp) is the enthalpy function corresponding to species
C               i at the end of this file.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     1a. Anode Water reaction
C-----------------------------------------------------------------------
      Hi_H2OA = FC_inlet_data(2, 1)*(Hf_H2Ov +
     &     PEMf_HSTEAM(FC_inlet_data(1, 1))
     &     -PEMf_HSTEAM(REF_temperature))
      Ho_H2OA = FC_outlet_data(2, 1)*(Hf_H2Ov +
     &     PEMf_HSTEAM(FC_outlet_data(1, 1))
     &     -PEMf_HSTEAM(REF_temperature))
      dHa_H2O = Ho_H2OA - Hi_H2OA
C-----------------------------------------------------------------------
C     1b. Cathode Water reaction
C-----------------------------------------------------------------------
      Hi_H2OB = FC_inlet_data(2, 2)*(hf_H2Ol +
     &     Cp_Water_L*(T_cell - 298))

      Ho_H2OB = FC_outlet_data(2, 2)*(hf_H2Ol +
     &     Cp_Water_L*(T_cell - 298))

      dHb_H2O = Ho_H2OB - Hi_H2OB
C-----------------------------------------------------------------------
C     2. Anode/cathode fuel reaction
C-----------------------------------------------------------------------
      IF (Fuel_type.EQ.1) THEN
C------- fuel is methane /anode
         Hi_FUELA = FC_inlet_data(3, 1)*(hf_CH4 +
     &        PEMF_HCH4(FC_inlet_data(1, 1))
     &        -PEMF_HCH4(REF_temperature))
         Ho_FUELA = FC_outlet_data(3, 1)*(hf_CH4 +
     &        PEMF_HCH4(FC_outlet_data(1, 1))
     &        -PEMF_HCH4(REF_temperature))
         dHa_FUEL = Ho_FUELA - Hi_FUELA
C------- Cathode
         Hi_FUELB = FC_inlet_data(3, 2)*(Hf_CH4 +
     &        PEMF_HCH4(FC_inlet_data(1, 2))
     &        -PEMF_HCH4(REF_temperature))
         Ho_FUELB = FC_outlet_data(3, 2)*(Hf_CH4 +
     &        PEMF_HCH4(FC_outlet_data(1, 2))
     &        -PEMF_HCH4(REF_temperature))
         dHb_FUEL = Ho_FUELB - Hi_FUELB
      ELSEIF (Fuel_type.EQ.2) THEN
C------- fuel is octane / anode
         Hi_FUELA = FC_inlet_data(3, 1)*(hf_OCT +
     &        PEMf_HOCTg(FC_inlet_data(1, 1))
     &        -PEMf_HOCTg(REF_temperature))
         Ho_FUELA = FC_outlet_data(3, 1)*(hf_OCT +
     &        PEMf_HOCTg(FC_outlet_data(1, 1))
     &        -PEMf_HOCTg(REF_temperature))
         dHa_FUEL = Ho_FUELA - Hi_FUELA
C------- Cathode
         Hi_FUELB = FC_inlet_data(3, 2)*(hf_OCT +
     &        PEMf_HOCTg(FC_inlet_data(1, 2))
     &        -PEMf_HOCTg(REF_temperature))
         Ho_FUELB = FC_outlet_data(3, 2)*(hf_OCT +
     &        PEMf_HOCTg(FC_outlet_data(1, 2))
     &        -PEMf_HOCTg(REF_temperature))
         dHb_FUEL = Ho_FUELB - Hi_FUELB
      ELSEIF (Fuel_type.EQ.3) THEN
C------- Fuel is methanol / anode
         Hi_FUELA = FC_inlet_data(3, 1)*(hf_Meth +
     &        PEMf_HMETHg(FC_inlet_data(1, 1))
     &        -PEMf_HMETHg(REF_temperature))
         Ho_FUELA = FC_outlet_data(3, 1)*(hf_Meth +
     &        PEMf_HMETHg(FC_outlet_data(1, 1))
     &        -PEMf_HMETHg(REF_temperature))
         dHa_FUEL = Ho_FUELA - Hi_FUELA
C------- Cathode
         Hi_FUELB = FC_inlet_data(3, 2)*(hf_Meth +
     &        PEMf_HMETHg(FC_inlet_data(1, 2))
     &        -PEMf_HMETHg(REF_temperature))
         Ho_FUELB = FC_outlet_data(3, 2)*(hf_Meth +
     &        PEMf_HMETHg(FC_outlet_data(1, 2))
     &        -PEMf_HMETHg(REF_temperature))
         dHb_FUEL = Ho_FUELB - Hi_FUELB
      ENDIF
C-----------------------------------------------------------------------
C     3a. Anode CO reaction
C-----------------------------------------------------------------------
      Hi_coA = FC_inlet_data(4, 1)*(Hf_co +
     &     PEMf_HCO(FC_inlet_data(1, 1))-PEMf_HCO(REF_temperature))
      Ho_coA = FC_outlet_data(4, 1)*(Hf_co +
     &     PEMf_HCO(FC_outlet_data(1, 1))-PEMf_HCO(REF_temperature))
      dHa_co = Ho_coA - Hi_coA
C-----------------------------------------------------------------------
C     3b. Cathode CO reaction
C-----------------------------------------------------------------------
      Hi_coB = FC_inlet_data(4, 2)*(Hf_co +
     &     PEMf_HCO(FC_inlet_data(1, 2))-PEMf_HCO(REF_temperature))
      Ho_coB = FC_outlet_data(4, 2)*(Hf_co +
     &     PEMf_HCO(FC_outlet_data(1, 2))-PEMf_HCO(REF_temperature))
      dHb_co = Ho_coB - Hi_coB
C-----------------------------------------------------------------------
C     4a. Anode CO2 reaction
C-----------------------------------------------------------------------
      Hi_co2A = FC_inlet_data(5, 1)*(Hf_co2 +
     &     PEMf_HCO2(FC_inlet_data(1, 1))-PEMf_HCO2(REF_temperature))
      Ho_co2A = FC_outlet_data(5, 1)*(Hf_co2 +
     &     PEMf_HCO2(FC_outlet_data(1, 1))-PEMf_HCO2(REF_temperature))
      dHa_co2 = Ho_co2A - Hi_co2A
C-----------------------------------------------------------------------
C     4b. Cathode CO2 reaction
C-----------------------------------------------------------------------
      Hi_co2B = FC_inlet_data(5, 2)*(Hf_co2 +
     &     PEMf_HCO2(FC_inlet_data(1, 2))-PEMf_HCO2(REF_temperature))
      Ho_co2B = FC_outlet_data(5, 2)*(Hf_co2 +
     &     PEMf_HCO2(FC_outlet_data(1, 2))-PEMf_HCO2(REF_temperature))
      dHb_co2 = Ho_co2B - Hi_co2B
C-----------------------------------------------------------------------
C     5a. Anode H2 reaction
C-----------------------------------------------------------------------
      Hi_H2A = FC_inlet_data(6, 1)*(Hf_H2 +
     &     PEMF_HH2(FC_inlet_data(1, 1))-PEMF_HH2(REF_temperature))
      Ho_H2A = FC_outlet_data(6, 1)*(Hf_H2 +
     &     PEMF_HH2(FC_outlet_data(1, 1))-PEMF_HH2(REF_temperature))
      dHa_H2 = Ho_H2A - Hi_H2A
C-----------------------------------------------------------------------
C     5b. Cathode H2 reaction
C-----------------------------------------------------------------------
      Hi_H2B = FC_inlet_data(6, 2)*(Hf_H2 +
     &     PEMF_HH2(FC_inlet_data(1, 2))-PEMF_HH2(REF_temperature))
      Ho_H2B = FC_outlet_data(6, 2)*(Hf_H2 +
     &     PEMF_HH2(FC_outlet_data(1, 2))-PEMF_HH2(REF_temperature))
      dHb_H2 = Ho_H2B - Hi_H2B
C-----------------------------------------------------------------------
C     6a. Anode O2 reaction
C-----------------------------------------------------------------------
      Hi_o2A = FC_inlet_data(7, 1)*(Hf_o2 +
     &     PEMf_HO2(FC_inlet_data(1, 1))-PEMf_HO2(REF_temperature))
      Ho_o2A = FC_outlet_data(7, 1)*(Hf_o2 +
     &     PEMf_HO2(FC_outlet_data(1, 1))-PEMf_HO2(REF_temperature))
      dHa_o2 = Ho_o2A - Hi_o2A
C-----------------------------------------------------------------------
C     6b. Cathode O2 reaction
C-----------------------------------------------------------------------
      Hi_o2B = FC_inlet_data(7, 2)*(Hf_o2 +
     &     PEMf_HO2(FC_inlet_data(1, 2))-PEMf_HO2(REF_temperature))
      Ho_o2B = FC_outlet_data(7, 2)*(Hf_o2 +
     &     PEMf_HO2(FC_outlet_data(1, 2))-PEMf_HO2(REF_temperature))
      dHb_o2 = Ho_o2B - Hi_o2B
C-----------------------------------------------------------------------
C     7a. Anode N2 reaction
C-----------------------------------------------------------------------
      Hi_n2A = FC_inlet_data(8, 1)*(Hf_n2 +
     &     PEMf_hN2(FC_inlet_data(1, 1))-PEMf_hN2(REF_temperature))
      Ho_n2A = FC_outlet_data(8, 1)*(Hf_n2 +
     &     PEMf_hN2(FC_outlet_data(1, 1))-PEMf_hN2(REF_temperature))
      dHa_n2 = Ho_n2A - Hi_n2A
C-----------------------------------------------------------------------
C     7b. Cathode N2 reaction
C-----------------------------------------------------------------------
      Hi_n2B = FC_inlet_data(8, 2)*(Hf_n2 +
     &     PEMf_hN2(FC_inlet_data(1, 2))-PEMf_hN2(REF_temperature))
      Ho_n2B = FC_outlet_data(8, 2)*(Hf_n2 +
     &     PEMf_hN2(FC_outlet_data(1, 2))-PEMf_hN2(REF_temperature))
      dHb_n2 = Ho_n2B - Hi_n2B
C-----------------------------------------------------------------------
C     Calculate total enthalpy change per mol H2 entering cell:
C
C       dH_stack = SUM OF ENTHALPY CHANGES OF ALL SPECIES
C                  AT ANODE AND CATHODE
C-----------------------------------------------------------------------
      dH_stack = dHa_H2O + dHb_H2O + dHa_FUEL + dHb_FUEL + dHa_co
     &     + dHb_co + dHa_co2 + dHb_co2 + dHa_H2 + dHb_H2 + dHa_o2
     &     + dHb_o2 + dHa_n2 + dHb_n2  ! KJ / kmol H2
C-----------------------------------------------------------------------
C     Variables AmolXy are used to count the total # of mols at
C     the anode (A) and Cathode (C) inlets (i) and outlets (o).
C     This data is needed later in the partial pressure calculations.
C     Here, they're initialized to zero, which is probably not necessary,
C     but is good practice.
C-----------------------------------------------------------------------
      Anode_Inlet_Mols = 0.
      Cathode_Inlet_Mols = 0.
      Anode_Outlet_Mols = 0.
      Cathode_Outlet_Mols = 0.
C-----------------------------------------------------------------------
C     Count the total number of mols at anode and cathode inlet/outlet
C-----------------------------------------------------------------------
      DO ii = 2,8                !<- for each species 2->8
         Anode_Inlet_Mols =
     &        Anode_Inlet_Mols + FC_inlet_data(ii,1) ! anode inlet
         Cathode_Inlet_Mols =
     &        Cathode_Inlet_Mols + FC_inlet_data(ii,2) ! cathode inlet
         Anode_Outlet_Mols =
     &        Anode_Outlet_Mols + FC_outlet_data(ii,1) ! anode outlet
         Cathode_Outlet_Mols =
     &        Cathode_Outlet_Mols + FC_outlet_data(ii,2) ! cathode outlet
      ENDDO
C-----------------------------------------------------------------------
C     Set rPCxA variables to appropriate anode/cathode pressures
C-----------------------------------------------------------------------
      rPCiA = pressures(1)             ! Cathode inlet pressure
      rPCoA = pressures(1)             ! Cathode outlet pressure
      Anode_inlet_pressure = pressures(2)             ! Anode inlet pressure
      Anode_outlet_pressure = pressures(2)             ! Anode outlet pressure
C-----------------------------------------------------------------------
C     Determine cell entropy change - REF: 298K, 101.32 kPa (per mol
C     H2 entering the cell)
C
C     THIS next chunk of code determines the entropy change that each
C     species undergoes as it moves through the cell and reacts. For both
C     the anode and cahtode, the following is completed for each species i:
C
C       Parital Pressure (Pp):
C                                        # mols of species i @ inlet
C         Pp_inlet  =  Pressure_inlet  * ---------------------------
C                                           Total # mols @ inlet
C
C                                        # mols of species i @ outlet
C         Pp_outlet =  Pressure_outlet * -----------------------------
C                                           Total # mols @ outlet
C
C         S_inlet  = Composition_inlet*
C                    (Entropy_formation + Entropy(T_inlet,Pp_inlet)
C                                              - Entropy(REF_temperature,101.32 kPa))
C
C         S_outlet = Composition_outlet*
C                    (Entropy_formation + Entropy(T_outlet,Pp_outlet)
C                                              - Entropy(REF_temperature,101.32))
C
C         S_change = (H_outlet - H_inlet)
C
C         WHERE:
C               Entropy(Temp, Press) is the entropy function
C               corresponding to species i at the end of this file.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     1a. Anode Water reaction
C-----------------------------------------------------------------------
      rPAi = Anode_inlet_pressure
     &     * FC_inlet_data(2, 1)/Anode_Inlet_Mols
      rPAo = Anode_outlet_pressure
     &     * FC_outlet_data(2, 1)/Anode_Outlet_Mols
      Si_H2OA = FC_inlet_data(2, 1)*(sf_H2Ov +
     &     PEMf_sSTEAM(FC_inlet_data(1, 1),rPAi) -
     &     PEMf_sSTEAM(REF_temperature,REF_pressure))
      S2_H2OA = FC_outlet_data(2, 1)*(sf_H2Ov +
     &     PEMf_sSTEAM(FC_outlet_data(1, 1),rPAo) -
     &     PEMf_sSTEAM(REF_temperature,REF_pressure))
      dSa_H2O = S2_H2OA - Si_H2OA
C-----------------------------------------------------------------------
C     1b. Cathode Water reaction
C-----------------------------------------------------------------------
      rPCi = rPCiA * FC_inlet_data(2, 2)/Cathode_Inlet_Mols
      rPCo = rPCoA * FC_outlet_data(2, 2)/Cathode_Outlet_Mols
      Si_H2OB = FC_inlet_data(2, 2)*(sf_H2Ol +
     &     Cp_Water_L*log(T_cell/REF_temperature))
      S2_H2OB = FC_outlet_data(2, 2)*(sf_H2Ol +
     &     Cp_Water_L*log(T_cell/REF_temperature))
      dSb_H2O = S2_H2OB - Si_H2OB
C-----------------------------------------------------------------------
C     2. Anode/cathode fuel reaction
C-----------------------------------------------------------------------
      IF (Fuel_type.EQ.1) THEN
C------- fuel is methane /anode
         rPAi = Anode_inlet_pressure
     &        * FC_inlet_data(3, 1)/Anode_Inlet_Mols
         rPAo = Anode_outlet_pressure
     &        * FC_outlet_data(3, 1)/Anode_Outlet_Mols
         Si_FUELA = FC_inlet_data(3, 1)*(sf_cH4 +
     &        PEMf_sCH4(FC_inlet_data(1, 1),rPAi)-
     &        PEMf_sCH4(REF_temperature,REF_pressure))
         S2_FUELA = FC_outlet_data(3, 1)*(sf_cH4 +
     &        PEMf_sCH4(FC_outlet_data(1, 1),rPAo)-
     &        PEMf_sCH4(REF_temperature,REF_pressure))
         dSa_FUEL = S2_FUELA - Si_FUELA
C------- Cathode
         rPCi = rPCiA * FC_inlet_data(2, 2)/Cathode_Inlet_Mols
         rPCo = rPCoA * FC_outlet_data(2, 2)/Cathode_Outlet_Mols
         Si_FUELB = FC_inlet_data(3, 2)*(sf_ch4 +
     &        PEMf_sCH4(FC_inlet_data(1, 2),rPCi)-
     &        PEMf_sCH4(REF_temperature,REF_pressure))
         S2_FUELB = FC_outlet_data(3, 2)*(sf_ch4 +
     &        PEMf_sCH4(FC_outlet_data(1, 2),rPCo)-
     &        PEMf_sCH4(REF_temperature,REF_pressure))
         dSb_FUEL = S2_FUELB - Si_FUELB
      ELSEIF (Fuel_type.EQ.2) THEN
C------- Fuel is octane / anode
         rPAi = Anode_inlet_pressure
     &        * FC_inlet_data(3, 1)/Anode_Inlet_Mols
         rPAo = Anode_outlet_pressure
     &        * FC_outlet_data(3, 1)/Anode_Outlet_Mols
         Si_FUELA = FC_inlet_data(3, 1)*(sf_OCT +
     &        PEMf_sOCTg(FC_inlet_data(1, 1),rPAi)-
     &        PEMf_sOCTg(REF_temperature,REF_pressure))
         S2_FUELA = FC_outlet_data(3, 1)*(sf_Oct +
     &        PEMf_sOCTg(FC_outlet_data(1, 1),rPAo)-
     &        PEMf_sOCTg(REF_temperature,REF_pressure))
         dSa_FUEL = S2_FUELA - Si_FUELA
C------- Cathode
         rPCi = rPCiA * FC_inlet_data(3, 2)/Cathode_Inlet_Mols
         rPCo = rPCoA * FC_outlet_data(3, 2)/Cathode_Outlet_Mols
         Si_FUELB = FC_inlet_data(3, 2)*(sf_OCT +
     &        PEMf_sOCTg(FC_inlet_data(1, 2),rPCi)-
     &        PEMf_sOCTg(REF_temperature,REF_pressure))
         S2_FUELB = FC_outlet_data(3, 2)*(sf_Oct +
     &        PEMf_sOCTg(FC_outlet_data(1, 2),rPCo)-
     &        PEMf_sOCTg(REF_temperature,REF_pressure))
         dSb_FUEL = S2_FUELB - Si_FUELB
      ELSEIF (Fuel_type.EQ.3) THEN
C------- Fuel is Mehtanol / anode
         rPAi = Anode_inlet_pressure
     &        * FC_inlet_data(3, 1)/Anode_Inlet_Mols
         rPAo = Anode_outlet_pressure
     &        * FC_outlet_data(3, 1)/Anode_Outlet_Mols
         Si_FUELA = FC_inlet_data(3, 1)*(sf_Meth +
     &        PEMf_sMETHg(FC_inlet_data(1, 1),rPAi)-
     &        PEMf_sMETHg(REF_temperature,REF_pressure))
         S2_FUELA = FC_outlet_data(3, 1)*(sf_Meth +
     &        PEMf_sMETHg(FC_outlet_data(1, 1),rPAo)-
     &        PEMf_sMETHg(REF_temperature,REF_pressure))
         dSa_FUEL = S2_FUELA - Si_FUELA
C------- Cathode
         rPCi = rPCiA * FC_inlet_data(3, 2)/Cathode_Inlet_Mols
         rPCo = rPCoA * FC_outlet_data(3, 2)/Cathode_Outlet_Mols
         Si_FUELB = FC_inlet_data(3, 2)*(sf_Meth +
     &        PEMf_sMETHg(FC_inlet_data(1, 2),rPCi)-
     &        PEMf_sMETHg(REF_temperature,REF_pressure))
         S2_FUELB = FC_outlet_data(3, 2)*(sf_Meth +
     &        PEMf_sMETHg(FC_outlet_data(1, 2),rPCo)-
     &        PEMf_sMETHg(REF_temperature,REF_pressure))
         dSb_FUEL = S2_FUELB - Si_FUELB
      ENDIF
C-----------------------------------------------------------------------
C     3a. Anode CO reaction
C-----------------------------------------------------------------------
      rPAi = Anode_inlet_pressure * FC_inlet_data(4, 1)/Anode_Inlet_Mols
      rPAo = Anode_outlet_pressure
     &     * FC_outlet_data(4, 1)/Anode_Outlet_Mols
      Si_coA = FC_inlet_data(4, 1)*(sf_co +
     &     PEMf_sCO(FC_inlet_data(1, 1),rPAi)-
     &     PEMf_sCO(REF_temperature,REF_pressure))
      S2_coA = FC_outlet_data(4, 1)*(sf_co +
     &     PEMf_sCO(FC_outlet_data(1, 1),rPAo)-
     &     PEMf_sCO(REF_temperature,REF_pressure))
      dSa_co = S2_coA - Si_coA
C-----------------------------------------------------------------------
C     3b. Cathode CO reaction
C----------------------------------------------------------------------
      rPCi = rPCiA * FC_inlet_data(4, 2)/Cathode_Inlet_Mols
      rPCo = rPCoA * FC_outlet_data(4, 2)/Cathode_Outlet_Mols
      Si_coB = FC_inlet_data(4, 2)*(sf_co +
     &     PEMf_sCO(FC_inlet_data(1, 2),rPCi)-
     &     PEMf_sCO(REF_temperature,REF_pressure))
      S2_coB = FC_outlet_data(4, 2)*(sf_co +
     &     PEMf_sCO(FC_outlet_data(1, 2),rPCo)-
     &     PEMf_sCO(REF_temperature,REF_pressure))
      dSb_co = S2_coB - Si_coB
C-----------------------------------------------------------------------
C     4a. Anode CO2 reaction
C-----------------------------------------------------------------------
      rPAi = Anode_inlet_pressure * FC_inlet_data(5, 1)/Anode_Inlet_Mols
      rPAo = Anode_outlet_pressure
     &     * FC_outlet_data(5, 1)/Anode_Outlet_Mols
      Si_co2A = FC_inlet_data(5, 1)*(sf_co2 +
     &     PEMf_sCO2(FC_inlet_data(1, 1),rPAi)-
     &     PEMf_sCO2(REF_temperature,REF_pressure))
      S2_co2A = FC_outlet_data(5, 1)*(sf_co2 +
     &     PEMf_sCO2(FC_outlet_data(1, 1),rPAo)-
     &     PEMf_sCO2(REF_temperature,REF_pressure))
      dSa_co2 = S2_co2A - Si_co2A
C-----------------------------------------------------------------------
C     4b. Cathode CO2 reaction
C-----------------------------------------------------------------------
      rPCi = rPCiA * FC_inlet_data(5, 2)/Cathode_Inlet_Mols
      rPCo = rPCoA * FC_outlet_data(5, 2)/Cathode_Outlet_Mols
      Si_co2B = FC_inlet_data(5, 2)*(sf_co2 +
     &     PEMf_sCO2(FC_inlet_data(1, 2),rPCi)-
     &     PEMf_sCO2(REF_temperature,REF_pressure))
      S2_co2B = FC_outlet_data(5, 2)*(sf_co2 +
     &     PEMf_sCO2(FC_outlet_data(1, 2),rPCo)-
     &     PEMf_sCO2(REF_temperature,REF_pressure))
      dSb_co2 = S2_co2B - Si_co2B
C-----------------------------------------------------------------------
C     5a. Anode H2 reaction
C-----------------------------------------------------------------------
      rPAi = Anode_inlet_pressure * FC_inlet_data(6, 1)/Anode_Inlet_Mols
      rPAo = Anode_outlet_pressure
     &     * FC_outlet_data(6, 1)/Anode_Outlet_Mols
      Si_H2A = FC_inlet_data(6, 1)*(sf_H2 +
     &     PEMf_sH2(FC_inlet_data(1, 1),rPAi)-
     &     PEMf_sH2(REF_temperature,REF_pressure))
      S2_H2A = FC_outlet_data(6, 1)*(sf_H2 +
     &     PEMf_sH2(FC_outlet_data(1, 1),rPAo)-
     &     PEMf_sH2(REF_temperature,REF_pressure))
      dSa_H2 = S2_H2A - Si_H2A
C-----------------------------------------------------------------------
C     5b. Cathode H2 reaction
C-----------------------------------------------------------------------
      rPCi = rPCiA * FC_inlet_data(6, 2)/Cathode_Inlet_Mols
      rPCo = rPCoA * FC_outlet_data(6, 2)/Cathode_Outlet_Mols

      Si_H2B = FC_inlet_data(6, 2)*(sf_H2 +
     &     PEMf_sH2(FC_inlet_data(1, 2),rPCi)-
     &     PEMf_sH2(REF_temperature,REF_pressure))
      S2_H2B = FC_outlet_data(6, 2)*(sf_H2 +
     &     PEMf_sH2(FC_outlet_data(1, 2),rPCo)-
     &     PEMf_sH2(REF_temperature,REF_pressure))
      dSb_H2 = S2_H2B - Si_H2B
C-----------------------------------------------------------------------
C     6b. Cathode O2 reaction
C-----------------------------------------------------------------------
      rPAi = Anode_inlet_pressure * FC_inlet_data(7, 1)/Anode_Inlet_Mols
      rPAo = Anode_outlet_pressure
     &     * FC_outlet_data(7, 1)/Anode_Outlet_Mols
      Si_o2A = FC_inlet_data(7, 1)*(sf_o2 +
     &     PEMf_sO2(FC_inlet_data(1, 1),rPAi)-
     &     PEMf_sO2(REF_temperature,REF_pressure))
      S2_o2A = FC_outlet_data(7, 1)*(sf_o2 +
     &     PEMf_sO2(FC_outlet_data(1, 1),rPAo)-
     &     PEMf_sO2(REF_temperature,REF_pressure))
      dSa_o2 = S2_o2A - Si_o2A
C-----------------------------------------------------------------------
C     6b. Cathode O2 reaction
C-----------------------------------------------------------------------
      rPCi = rPCiA * FC_inlet_data(7, 2)/Cathode_Inlet_Mols
      rPCo = rPCoA * FC_outlet_data(7, 2)/Cathode_Outlet_Mols
      Si_o2B = FC_inlet_data(7, 2)*(sf_o2 +
     &     PEMf_sO2(FC_inlet_data(1, 2),rPCi)-
     &     PEMf_sO2(REF_temperature,REF_pressure))
      S2_o2B = FC_outlet_data(7, 2)*(sf_o2 +
     &     PEMf_sO2(FC_outlet_data(1, 2),rPCo)-
     &     PEMf_sO2(REF_temperature,REF_pressure))
      dSb_o2 = S2_o2B - Si_o2B
C-----------------------------------------------------------------------
C     7a. Anode N2 reaction
C-----------------------------------------------------------------------
      rPAi = Anode_inlet_pressure
     &     * FC_inlet_data(8, 1)/Anode_Inlet_Mols
      rPAo = Anode_outlet_pressure
     &     * FC_outlet_data(8, 1)/Anode_Outlet_Mols
      Si_n2A = FC_inlet_data(8, 1)*(sf_n2 +
     &     PEMf_sN2(FC_inlet_data(1, 1),rPAi)-
     &     PEMf_sN2(REF_temperature,REF_pressure))
      S2_n2A = FC_outlet_data(8, 1)*(sf_n2 +
     &     PEMf_sN2(FC_outlet_data(1, 1),rPAo)-
     &     PEMf_sN2(REF_temperature,REF_pressure))
      dSa_n2 = S2_n2A - Si_n2A
C-----------------------------------------------------------------------
C     7b. Cathode N2 reaction
C-----------------------------------------------------------------------
      rPCi = rPCiA * FC_inlet_data(8, 2) / Cathode_Inlet_Mols
      rPCo = rPCoA * FC_outlet_data(8, 2) / Cathode_Outlet_Mols
      Si_n2B = FC_inlet_data(8, 2)*(sf_n2 +
     &     PEMf_sN2(FC_inlet_data(1, 2),rPCi)-
     &     PEMf_sN2(REF_temperature,REF_pressure))
      S2_n2B = FC_outlet_data(8, 2)*(sf_n2 +
     &     PEMf_sN2(FC_outlet_data(1, 2),rPCo)-
     &     PEMf_sN2(REF_temperature,REF_pressure))
      dSb_n2 = S2_n2B - Si_n2B
C----------------------------------------------------------------------
C     Calculate total entropy change per mol H2 entering the cell:
C
C        dS_stack = SUM OF ENTROPY CHANGES OF ALL SPECIES
C     -               AT ANODE AND CATHODE
C----------------------------------------------------------------------
      dS_stack = dSa_H2O + dSb_H2O + dSa_FUEL + dSb_FUEL + dSa_co
     &     + dSb_co + dSa_co2 + dSb_co2 + dSa_H2 + dSb_H2 + dSa_o2
     &     + dSb_o2 + dSa_n2 + dSb_n2
C----------------------------------------------------------------------
C     Calculate the theoretical efficiency of the fuel cell:
C
C                          Tcell * dS_stack
C         efficiency = 1 - --------------
C                           dH_stack
C
C     See Equation 3.1.11, Ferguson 2003.
C
C----------------------------------------------------------------------
      max_efficiency = 1. - (T_cell * dS_stack) / dH_stack
      RETURN
      END


