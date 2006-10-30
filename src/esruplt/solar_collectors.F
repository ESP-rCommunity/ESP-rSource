C ====================================================================
C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre
C Natural Resources Canada, Government of Canada
C 2006. Please Contact Ian Beausoliel-Morrison for details
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

C===================== simple_solar_coll_static ========================
C     Created by: Didier Thevenard
C     Created on: January 2006
C     Copyright:  CETC 2006
C-----------------------------------------------------------------------
C     ABSTRACT:
C     Establishes for a simplified solar collector whether the correct 
C     number of control variables have been specified, whether the 
C     number of water connections is correct and whether the connections 
C     are to the correct fluid type.
C
C=======================================================================

      SUBROUTINE simple_solar_coll_static(IPCOMP)

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE
      
C---- Include statements

#include "plant.h"
#include "building.h"

C---- Arguments

      INTEGER IPCOMP
      
C---- Common blocks

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN
      
      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT
      
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA,BDATA

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      REAL PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP
      
C---- Local variables      

      INTEGER IX1,NITMS
      INTEGER J
      INTEGER NCITM,NCONS
      INTEGER IPCON,IPCONC
      LOGICAL ERROR_FLAG, ERROR_FLAG2, CLOSEA,CLOSEB
      INTEGER IAM_CORR_METHOD   ! Incidence angle correction method (=1 fit; =2 interpolation) 
      INTEGER ICOUNTER          ! Loop counter
      INTEGER IAM_CORR_NUM_DATA_PAIRS
                                ! Number of data pairs for incidence angle correction using
      REAL    IAM_CORR_DATA(20) ! Data pairs for angle and correction factor
      REAL    GLYCOL_MASS_FRACTION
                                ! Mass fraction of glycol in the collector fluid


C---- Set error flag to false
      ERROR_FLAG = .false.
      ERROR_FLAG2 = .false.

C Set collector local variables based on user-input values
C---- Flag for incidence angle correction method
C---- =1 use standard quadratic equation
C---- =2 use set of pairs of cutoff angles and correction factors        
      IAM_CORR_METHOD = INT(ADATA(IPCOMP,8))
C---- Number of data pairs for incidence angle correction data
      IAM_CORR_NUM_DATA_PAIRS = INT(ADATA(IPCOMP,11))
C---- Pairs of angles and incidence angle correction factors        
      DO ICOUNTER = 1,2*IAM_CORR_NUM_DATA_PAIRS
        IAM_CORR_DATA(ICOUNTER) = ADATA(IPCOMP,11+ICOUNTER)
      END DO
C---- Glycol mass fraction
      GLYCOL_MASS_FRACTION = ADATA(IPCOMP,34)

C---- Trace output

      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
        WRITE(ITU,*) ' 1 node (ISV=20) Simplified flat-plate solar '
        WRITE(ITU,*) ' collector'
        NITMS=11
        WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
        call epagew
      END IF ! Matches to IF(ITC.GT.0.AND.ITRACE(35).NE.0)

C---- Check that glycol mass fraction is either 0. for pure water or 
C     50% glycol and water. Correlations are currently implemented for
C     only 50% glycol mixtures
      CALL ECLOSE(GLYCOL_MASS_FRACTION,0.,1.,CLOSEA)
      CALL ECLOSE(GLYCOL_MASS_FRACTION,50.,1.,CLOSEB)
      IF((.NOT.CLOSEA).AND.(.NOT.CLOSEB)) THEN
      WRITE(IUOUT,*) ' simple_solar_coll_static: '
      WRITE(IUOUT,*) ' Glycol fraction through the solar collector '
      WRITE(IUOUT,*) ' component ', IPCOMP, ' is not set to 0. or '
      WRITE(IUOUT,*) ' 50%. Model is only currently able to deal '
      WRITE(IUOUT,*) ' with these concentrations of glycol'
      STOP ' simple_solar_coll_coeff_gen: unresolvable error'
      ENDIF

C---- Check that containment exists

      CALL ECLOSE(PCNTMF(IPCOMP),-99.00,0.001,CLOSEA)
      IF(CLOSEA) THEN
        WRITE(IUOUT,*) 'simple_solar_coll_coeff_gen : A containment '
        WRITE(IUOUT,*) 'must be specified for component ',IPCOMP
        WRITE(IUOUT,*) ' and all components of the same type'
        ERROR_FLAG2 = .true.
      ENDIF ! Matches to IF(CLOSEA) THEN

C---- Check user specified number of controlled variables

      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' simple_solar_coll_static warning: user ',
     &                ' specified wrong number of controlled ',
     &                ' variables '

C---- Check component has 1 connection

      NCONS=1
      DO 10 IPCONC=1,MPCONC
        IPCON=ICONDX(IPCOMP,1,IPCONC)
        IF(IPCONC.LE.NCONS) THEN
          IF(IPCON.EQ.0) THEN
            ERROR_FLAG = .true.
          END IF ! Matches IF(IPCONC.LE.NCONS)
        ELSE IF(IPCON.NE.0) THEN
          ERROR_FLAG = .true.
        END IF ! Matches IF(IPCONC.LE.NCONS)
   10 CONTINUE

C---- Check that total number of data pairs entered by the user is not greater
C---- than the max of 10

      IF((IAM_CORR_METHOD.EQ.2).AND.
     &   (IAM_CORR_NUM_DATA_PAIRS.GT.10)) THEN
      WRITE(IUOUT,*) ' simple_solar_coll_static: Number of data '
      WRITE(IUOUT,*) ' pairs for incidence angle modifiers for '
      WRITE(IUOUT,*) ' component ', IPCOMP, ' can not be more '
      WRITE(IUOUT,*) ' than 10 ' 
      STOP ' simple_solar_coll_coeff_gen: unresolvable error'
      ENDIF ! Matches to IF((IAM_CORR_METHOD.EQ.2) ...

C---- Check that pairs of data for incidence angle correction factors is
C---- given in accending order of incidence angle

      IF(IAM_CORR_METHOD.EQ.2) THEN
        DO ICOUNTER = 2,IAM_CORR_NUM_DATA_PAIRS
          IF(IAM_CORR_DATA(2*ICOUNTER-1).LT.
     &       IAM_CORR_DATA(2*ICOUNTER-3)) THEN
      WRITE(IUOUT,*) ' simple_solar_coll_static: incidence angle '
      WRITE(IUOUT,*) ' modifiers for component',IPCOMP, ' not entered '
      WRITE(IUOUT,*) ' in increasing order of incidence angles'
      STOP ' simple_solar_coll_coeff_gen: unresolvable error'
          ENDIF ! Matches to IF(IAM_CORR_DATA(2*ICOUNTER-1) ...
        END DO
      ENDIF ! Matches to IF(closea)

C---- Check that two adjacent data pairs for incidence angle correction do 
C     not have the same angle of incidence. This will help prevent division by 0.
C     in subroutine "simple_solar_coll_coeff_gen"

      IF(IAM_CORR_METHOD.EQ.2) THEN
        DO ICOUNTER = 2,IAM_CORR_NUM_DATA_PAIRS
          CALL ECLOSE(IAM_CORR_DATA(2*ICOUNTER-1),
     &         IAM_CORR_DATA(2*ICOUNTER-3),0.001,CLOSEA)
          IF(CLOSEA) THEN
      WRITE(IUOUT,*) ' simple_solar_coll_static: incidence angle '
      WRITE(IUOUT,*) ' modifiers for component',IPCOMP, ' have '
      WRITE(IUOUT,*) ' two consecutive data pairs with the same '
      WRITE(IUOUT,*) ' angle of incidence '
      STOP ' simple_solar_coll_coeff_gen: unresolvable error'
          ENDIF ! Matches to IF(CLOSEA)
       END DO
      ENDIF ! Matches to IF(IAM_CORR_METHOD.EQ.2) 

C---- Error handling

      IF(ERROR_FLAG) THEN
      WRITE(IUOUT,*) ' simple_solar_coll_static: connection error '
      WRITE(IUOUT,*) ' for component ',IPCOMP,' should be ',NCONS
      WRITE(IUOUT,*) ' connection(s)'
      STOP ' simple_solar_coll_static: unresolvable error'
      ENDIF ! Matches IF(ERROR_FLAG) 

      IF(ERROR_FLAG2) THEN
      STOP ' Fatal error for simple_solar_coll_static - specify a 
     &containment !'
      ENDIF ! Matches IF(ERROR_FLAG2) 

      RETURN
      END

C===================== simple_solar_coll_coeff_gen =====================
C     Created by: Didier Thevenard
C     Created on: January 2006
C     Copyright:  CETC 2006
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This is the ESP-r coefficient generator for the simplified solar
C     collector. The model is based on using collector efficiency equation
C     data to predict the performance of the collector for the time step.
C
C References:

C Hensen, J.L.M. (1991), On the Thermal Ineraction of Building Structure
C and Heating and Ventilating Systems, Ph.D. Thesis, Eindhoven University
C of Technology, The Netherlands.

C Thevenard, D., Haddad, K., and Purdy, J., Development of a New Solar 
C Collector Model in ESP-r. Proceedings of the 1st Canadian Solar Buildings
C Research Network.August 2006.
C=======================================================================

      SUBROUTINE simple_solar_coll_coeff_gen(IPCOMP,COUT,ISTATS)

C-----------------------------------------------------------------------
C     Declarations
C-----------------------------------------------------------------------

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Include statements

#include "building.h"
#include "plant.h"

C---- Arguments

      INTEGER  IPCOMP,ISTATS
      REAL COUT(MPCOE)

C---- ESP-r Common blocks

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/Pctime/TIMSEC
      REAL TIMSEC

      COMMON/PCTC/TC(MPCOM)
      REAL TC

      COMMON/PCEQU/IMPEXP,RATIMP
      INTEGER IMPEXP
      REAL RATIMP

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA,BDATA
      
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP

      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      REAL PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX
      
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),NAPDAT(MPCOM)
      REAL QDATA,PCAOUT
      INTEGER NAPDAT

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      INTEGER MAXITP,ITRCLP,ICSV
      REAL PERREL,PERTMP,PERFLX,PERMFL,CSVI

      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      REAL SAZI,SALT
      INTEGER ISUNUP

C---- Parameters 

      REAL SMALL
      PARAMETER (SMALL=1.0E-15)
      REAL PI
      PARAMETER (PI=3.14159265359)
      REAL DTOR
      PARAMETER (DTOR=PI/180.)

C---- Collector parameters

      REAL    COLLAREA        ! Collector area (m2)
      INTEGER EFFTYPE         ! Efficiency equation type (1..2)
      REAL    ETA0            ! Constant term of efficiency equ (-)
      REAL    ETA1            ! Linear coef of eff. equ. (W/m2/K)
      REAL    ETA2            ! Quadratic coef of eff. equ. (W/m2/K2)
      REAL    B0              ! Linear term of incidence angle mod. (-)
      REAL    B1              ! Quad. term of incidence angle mod. (-)
      REAL    MDOT_TST        ! Test flow rate (kg/s)
      REAL    CP_TST          ! Test fluid heat capacitance (kJ/kg/C)
      REAL    COLLSLOP        ! Collector slope (degree, hrz=0, vrt=90)
      REAL    COLLAZMT        ! Collector azimuth (degree, S=0, E=90)
      INTEGER    IAM_CORR_METHOD 
                              ! Incidence angle correction method (=1 fit; =2 interpolation)    
      INTEGER    IAM_CORR_NUM_DATA_PAIRS
                              ! Number of data pairs for incidence angle correction using
                              ! interpolation
      REAL    IAM_CORR_DATA(20)
                              ! Data pairs for angle and correction factor 
      REAL    GLYCOL_MASS_FRACTION
                              ! Mass fraction of glycol in mixture (%)
      REAL    COLL_MASS       ! Collector mass (kg)
      REAL    COLL_CP         ! Collector specific heat (J/kg-Deg C)

C---- Local variables

      REAL    AUX             ! Auxiliary variable
      REAL    BU              ! Auxiliary variable for calculation of R
      REAL    BT              ! Auxiliary variable for calculation of R
      REAL    CAI             ! Cosine of angle of incidence (-)
      LOGICAL CLOSEA          ! Auxiliary variable 
      REAL    DELTAT          ! Temperature difference (C)
      REAL    ETA0_LIN        ! Constant term of linearized eff. equ.
      REAL    ETA1_LIN        ! Linear coef of linearized eff. equ.
      REAL    ETA1_IN         ! Linear coef of eff. equ. based on Tin
      REAL    FPUL            ! Auxiliary variable for calculation of R
      REAL    G               ! Solar rad. in plane of collector (W/m2)
      REAL    HEATGAIN        ! Heat gain from collector (W)
      INTEGER ICON1           ! Pointer to interconnection
      INTEGER INOD1           ! Pointer to node
      REAL    MDOTCP_TST      ! Heat capacitance flowrate for test conds
      REAL    MDOTCP_USE      ! Heat capacitance flowrate for use conds
      REAL    MDOTCP_MOD      ! Modified heat capacitance flowrate for use conds
      REAL    QD              ! Direct normal irradiance (W/m2)
      REAL    QF              ! Diffuse horizontal irradiance (W/m2)
      REAL    R               ! Flowrate corrective coefficient (-)
      REAL    SRADD           ! Direct solar rad. on collector surface (W/m2)
      REAL    SRADF           ! Diffuse solar rad. on collector surface (W/m2)
      REAL    UA              ! Heat loss coefficient to environment (W/C)
      REAL    XIAM            ! Incidence angle modifier (-)
      REAL    HEATLOSS_ACTUAL ! Actual heat loss from collector (W)
      REAL    HEATGAIN_ACTUAL ! Actual heat gain to the collector (W)
      REAL    RADIANS_TO_DEGREES
                              ! Conversion factor from radians --> degrees
      INTEGER ICOUNTER        ! Loop counter
      REAL    INC_ANGLE       ! Incidence angle in radians  
      LOGICAL INC_ANGLE_INT_FOUND
                              ! Indicates whether proper incidence angle bounding interval found     
      INTEGER INC_ANGLE_INT_NUM
                              ! Incidence angle interval
      REAL    GLYCOL_VOL_FRACTION
                              ! Volume fraction of propylene glycol in mixture (%)
      REAL    ALPHA           ! Implicit/explicit weighting factor
      REAL    COLL_MASS_CP_PROD
                              ! Collector mass and specific heat product (J/Deg C)
      REAL    UA_ACTUAL       ! Actual UA to surrounding (W/m2-K)
      REAL    MDOT_USE        ! Total mass flow rate through collector (kg/s)
      REAL    TBOIL           ! Boiling temperature of glycol-water mixture (Deg C)
      
      REAL    TFREEZE         ! Freezing temperature of glycol-water mixture (Deg C)

C---- Declare ESP-r function types

      REAL    GLYCOL_WATER_TBOIL
                              ! Function for boiling temperature of glycol-water
                              ! mixtures
      REAL    GLYCOL_WATER_TFREEZE
                              ! Function for freezing temperature of glycol-water
                              ! mixtures
      REAL    GLYCOL_WATER_CP ! Function for specific heat of glycol-water mixture
      REAL    GLYCOL_VOL_FRACTION_FIND
                              ! Function for finding volume fraction of glycol based
                              ! on mass fraction value
C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine ',
     &   ' simple_solar_coll_coeff_gen '

C---- Initialize pointers to inter-connection(s) ICON, and node(s) INOD.

      ICON1 = ICONDX(IPCOMP,1,1)
      INOD1 = NPCDAT(IPCOMP,9)

C=======================================================================
C     Generate coefficients for temperature equation.
C=======================================================================

      IF(ISTATS.EQ.1) THEN

C---- Conversion factor from degrees to radians
      
        RADIANS_TO_DEGREES = 180. / 3.14159
      
C---- Initialise component parameters from the database information

C---- Collector area   
        COLLAREA = ADATA(IPCOMP,1)
C---- Collector efficiency equation type
C---- 1 indicates equation is based on inlet temperature
C---- 2 indicates equation is based on average temperature
        EFFTYPE = INT(ADATA(IPCOMP,2))
C---- Constant term in collector efficiency equation
        ETA0 = ADATA(IPCOMP,3)
C---- Linear term in collector efficiency equation
        ETA1 = ADATA(IPCOMP,4)
C---- Quadratic term in collector efficiency equation
        ETA2 = ADATA(IPCOMP,5)
C---- Mass flow rate at test conditions
        MDOT_TST = ADATA(IPCOMP,6)
C---- Specific heat of fluid during testing
        CP_TST = ADATA(IPCOMP,7)
C---- Flag for incidence angle correction method
C---- =1 use standard quadratic equation
C---- =2 use set of pairs of cutoff angles and correction factors        
        IAM_CORR_METHOD = INT(ADATA(IPCOMP,8))
C---- Linear term in incidence angle correction fit
        B0 = ADATA(IPCOMP,9)
C---- Quadratic term in incidence angle correction fit
        B1 = ADATA(IPCOMP,10)
C---- Number of data pairs for incidence angle correction data
        IAM_CORR_NUM_DATA_PAIRS = INT(ADATA(IPCOMP,11))
C---- Pairs of angles and incidence angle correction factors        
        DO ICOUNTER = 1,2*IAM_CORR_NUM_DATA_PAIRS
          IAM_CORR_DATA(ICOUNTER) = ADATA(IPCOMP,11+ICOUNTER)
        END DO
C---- Collector slope
        COLLSLOP = ADATA(IPCOMP,32)
C---- Collector azimuth angle
        COLLAZMT = ADATA(IPCOMP,33)
C---- Mass fraction of propylene glycol in working fluid
        GLYCOL_MASS_FRACTION = ADATA(IPCOMP,34)
C---- Collector mass
        COLL_MASS = ADATA(IPCOMP,35)
C---- Collector average capacitance
        COLL_CP = ADATA(IPCOMP,36)

C-----------------------------------------------------------------------
C     Calculate tilted solar radiation and incidence angle modifier
C-----------------------------------------------------------------------
        
C---- Calculate cosine of angle of incidence. Set to zero if 
C     surface facing away from the sun or sun is not up

        CAI = COS(SALT * DTOR) * SIN(COLLSLOP * DTOR) * 
     &        COS((COLLAZMT - SAZI) * DTOR) + 
     &        SIN(SALT * DTOR) * COS(COLLSLOP * DTOR)
        CAI = MAX(CAI,0.)
        IF (SALT.LE.0.) CAI = 0.

C---- Calculate diffuse and direct irradiance

        CALL MZSINT(1,QD,QF)

C---- Calculate tilted solar radiation
C     Do not attempt calculation if sun is not up

        G = 0.
        IF (QD.GT.0. .OR. QF.GT.0.) THEN
          IF (SALT.LE.0.) THEN
            G = QF
          ELSE 
            CALL MZSRAD0(COLLSLOP,QD,QF,CAI,SRADD,SRADF)
            G = SRADD + SRADF
          ENDIF ! Matches to IF (SALT.LE.0.)
        ENDIF ! Matches to IF (QD.GT.0. .OR. QF.GT.0.)

C---- Calculate incidence angle modifier

C---- Initialize correction factor to 0.
        XIAM = 0.
C---- Incidence angle for time step in degrees
        INC_ANGLE = acos(CAI) * RADIANS_TO_DEGREES
C---- Check if incidence angle correction method is close to 1 to use
C---- standard quadratic equation
        IF(IAM_CORR_METHOD.EQ.1) THEN
          IF (CAI.GT.0.) THEN
            AUX = 1. / CAI - 1.
            XIAM = 1. - B0 * AUX - B1 * AUX * AUX
          ENDIF ! Matches IF (CAI.GT.0.)
C---- Else use pairs of angles and correction factors
        ENDIF ! Matches to IF(closea)
C---- Check if incidence angle correction method is close to 2 to use
C---- data pairs and interpolation method
        IF(IAM_CORR_METHOD.EQ.2) THEN
          IF(CAI.GT.0.) THEN
C---- Logic variable to flag that proper interval where the actual
C---- incidence angle lies is found
            INC_ANGLE_INT_FOUND = .false.
C---- Loop through incidence angle values to find proper interval
C---- where actual incidence angle lies
            DO ICOUNTER = 1,IAM_CORR_NUM_DATA_PAIRS
              IF((.NOT.INC_ANGLE_INT_FOUND).AND.
     &          (INC_ANGLE.LT.IAM_CORR_DATA(2*ICOUNTER-1))) THEN
                INC_ANGLE_INT_FOUND = .TRUE.
                INC_ANGLE_INT_NUM = ICOUNTER
              ENDIF ! Matches IF((.NOT.INC_ANGLE_INT_FOUND) ...
            END DO
C---- In the case that the interval found is below the 1st angle, then
C---- use the correction factor for the 1st data pair
            IF((INC_ANGLE_INT_FOUND).AND.
     &                      (INC_ANGLE_INT_NUM.EQ.1)) THEN
              XIAM = IAM_CORR_DATA(2)
C---- If the interval number found is larger than 1, then interpolate 
C---- between two consecutive values
            ELSEIF((INC_ANGLE_INT_FOUND).AND.
     &                      (INC_ANGLE_INT_NUM.GT.1)) THEN
              XIAM = ((IAM_CORR_DATA(2*INC_ANGLE_INT_NUM) - 
     &          IAM_CORR_DATA(2*(INC_ANGLE_INT_NUM-1))) /
     &         (IAM_CORR_DATA(2*INC_ANGLE_INT_NUM-1) -
     &          IAM_CORR_DATA(2*(INC_ANGLE_INT_NUM-1)-1))) *
     &         (INC_ANGLE - 
     &          IAM_CORR_DATA(2*(INC_ANGLE_INT_NUM-1)-1)) +
     &          IAM_CORR_DATA(2*(INC_ANGLE_INT_NUM-1))
C---- If no interval is found, then incidence angle is higher than
C---- the highest angle of the data provided. In this case use the same
C---- correction  factor as that associated with the highest angle
            ELSEIF(.NOT.INC_ANGLE_INT_FOUND) THEN
              XIAM = IAM_CORR_DATA(2*IAM_CORR_NUM_DATA_PAIRS)
            ENDIF ! Matches IF((INC_ANGLE_INT_FOUND) ...
          ENDIF ! Matches IF(CAI.GT.0.)
        ENDIF ! Matches if IF(closea) 
        XIAM = MAX(XIAM,0.)

C-----------------------------------------------------------------------
C     Linearize efficiency equation. This is done using values of
C     temperatures at the future time step (and may therefore
C     require iterations on the part of ESP-r)
C     Note 1: the temperature used to linearize the equation is either
C     the inlet temperature or the average temperature. However when the
C     flowrate is zero, it is the collector temperature itself that is 
C     used since the purpose becomes the calculation of the collector
C     stagnation temperature
C-----------------------------------------------------------------------

        CALL eclose(CONVAR(ICON1,2),0.00,0.000001,closea)
        IF (closea) THEN
          DELTAT = CSVF(INOD1,1) - PCNTMF(IPCOMP)
        ELSE
C---- In the case that the efficiency equation is based on inlet temperature
          IF (EFFTYPE.EQ.1) THEN
            DELTAT = CONVAR(ICON1,1) - PCNTMF(IPCOMP)
C---- If the efficiency equation is based on the average temperature 
          ELSE
            DELTAT = (CSVF(INOD1,1) + 
     &               CONVAR(ICON1,1)) / 2. - PCNTMF(IPCOMP)
          ENDIF ! Matches IF (EFFTYPE.EQ.1)
        ENDIF ! Matches IF (closea)
C---- Following two modified coefficients are based on Equation 9 in 
C     paper by Thevenard et al. (2006)
        ETA0_LIN = ETA0
        ETA1_LIN = ETA1 + ETA2 * DELTAT

C-----------------------------------------------------------------------
C     Calculate heat capacity flow rates
C-----------------------------------------------------------------------

C---- Calculate heat capacity rate for test fluid (W/K), 
C     ie. SUM(mass flow * specific heat)

        MDOTCP_TST = MDOT_TST * CP_TST

C---- Total actual mass flow rate is sum of water mass flow and
C     glycol mass flow

        MDOT_USE = PCONDR(ICON1)*CONVAR(ICON1,2) +
     &             PCONDR(ICON1)*CONVAR(ICON1,3)

C---- Find glycol volume fraction in incoming flow

        GLYCOL_VOL_FRACTION = 
     &  GLYCOL_VOL_FRACTION_FIND(GLYCOL_MASS_FRACTION,
     &                           CONVAR(ICON1,1))

C---- Boiling temperature of glycol mixture

        TBOIL = GLYCOL_WATER_TBOIL(GLYCOL_VOL_FRACTION)

C---- Freezing temperature of glycol-water mixture

        TFREEZE = GLYCOL_WATER_TFREEZE(GLYCOL_VOL_FRACTION)

C---- Set incoming flow collector temperature using upper and lower bounds

        AUX = MAX(TFREEZE,MIN(TBOIL,CONVAR(ICON1,1)))

C---- Calculate heat capacity rate for the actual collector fluid (W/K), 
C     ie. SUM(mass flow * specific heat)

        MDOTCP_USE = MDOT_USE *
     &       GLYCOL_WATER_CP(GLYCOL_VOL_FRACTION,AUX)

C-----------------------------------------------------------------------
C     Calculate flowrate correction factor
C     The flow rate correction equation uses the efficiency
C     equation based on (Tin-Te). ETA1_IN is the linear 
C     parameters of the efficiency equation in that format
C     Note: the correction is done only if the flow rate is > 0
C-----------------------------------------------------------------------

        R=1.
        CALL eclose(CONVAR(ICON1,2),0.00,0.000001,closea)
        IF (.NOT.closea) THEN
          IF (EFFTYPE.EQ.1) THEN
            ETA1_IN = ETA1_LIN
          ELSE
C---- Equation 14 in Thevenrd et al. (2006)
            ETA1_IN = ETA1_LIN / 
     &                (1. + COLLAREA * ETA1_LIN / 2. / MDOTCP_TST)
          ENDIF ! Matches IF (EFFTYPE.EQ.1)
 
C---- Equation 13 in Thevenard et al. (2006)
          FPUL = -MDOTCP_TST / COLLAREA * 
     &            LOG(1. - ETA1_IN * COLLAREA / MDOTCP_TST)
C---- Equation 11 in Thevenard et al. (2006)
          BT = MDOTCP_TST / COLLAREA / FPUL
C---- Equation 12 in Thevenard et al. (2006)
          BU = MDOTCP_USE / COLLAREA / FPUL
C---- Equation 10 in Thevenard at al. (2006)
          R = BU / BT * (1. - EXP(-1. / BU)) / (1. - EXP(-1. / BT))
        ENDIF ! Matches IF (.NOT.closea)

C-----------------------------------------------------------------------
C     Modify the heat capacity rate to account for the fact that
C     the efficiency is expressed as a function of the inlet or 
C     average collector temperature
C-----------------------------------------------------------------------

        IF (EFFTYPE.EQ.1) THEN
C---- Equation 7 in Thevenard et al. (2006)
          MDOTCP_MOD = MDOTCP_USE - COLLAREA * ETA1_LIN
        ELSE
C---- Equation 8 in Thevenard et al. (2006)
          MDOTCP_MOD = MDOTCP_USE - COLLAREA * ETA1_LIN / 2.
        ENDIF ! Matches IF (EFFTYPE.EQ.1)

C-----------------------------------------------------------------------
C     Calculate gains and losses
C-----------------------------------------------------------------------

C---- Calculate heat gain from solar (not accounting for the 
C     heat losses to ambient)

C---- Euations 7 and 8 in Thevenard et al. (2006)
        HEATGAIN = COLLAREA * R * XIAM * ETA0_LIN * G

C---- Calculate coefficient of heat loss to containment

C---- Euations 7 and 8 in Thevenard et al. (2006)
        UA = COLLAREA * R * ETA1_LIN

C-----------------------------------------------------------------------
C     Setup equation coefficients
C-----------------------------------------------------------------------

C---- Mark fluid temp and flow rates for iteration.

        ICSV(INOD1,1) = 1
        CSVI(INOD1,1) = CSVF(INOD1,1)
        ICSV(INOD1,2) = 1
        CSVI(INOD1,2) = CSVF(INOD1,2)
        ICSV(INOD1,3) = 1
        CSVI(INOD1,3) = CSVF(INOD1,3)

C---- Collector mass x specific heat product

        COLL_MASS_CP_PROD = COLL_MASS * COLL_CP

C---- Actual UA to surrounding

        call eclose(MDOTCP_MOD,0.00,0.000001,closea)
        IF((.NOT.closea).AND.(MDOTCP_MOD.GT.0.)) THEN
          UA_ACTUAL = UA * MDOTCP_USE / MDOTCP_MOD
        ELSE
          UA_ACTUAL = UA
        ENDIF ! Matches IF((.NOT.closea)

C---- Collector time constant

        TC(IPCOMP) = 
     &       COLL_MASS_CP_PROD / (UA_ACTUAL + MDOTCP_USE + SMALL)

C ------Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
C-------Fully implicit.

        IF(IMPEXP.EQ.1) THEN
           ALPHA = 1.
C-------Implicit/explicit with user-specified (fixed) weighting.
        ELSEIF(IMPEXP.EQ.2) THEN
           ALPHA = RATIMP
C-------General case: implicit/explicit with calculated weighting.
        ELSEIF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63 * TC(IPCOMP)) THEN
             ALPHA = 1.
          ELSE
             ALPHA = RATIMP
          END IF
C-------Steady-state.
        ELSEIF(IMPEXP.EQ.4) THEN
            COLL_MASS_CP_PROD = 0.
            ALPHA = 1.
        ENDIF ! Matches IF(IMPEXP.EQ.1)

C--------------------------------------------------------------------- 
C     Establish matrix equation self- and cross-coupling coefficients
C     and then present-time coefficient
C     These coefficients appear in equation 5.4 of Hensen (1991)
C     COUT(1) is the coefficient of Theta_i
C     COUT(2) is the coefficient of Theta_j
C     COUT(3) is the right-hand side
C     Note that when the flowrate is zero, the equations will simply
C     calculate the collector plate stagnation temperature
C---------------------------------------------------------------------
        call eclose(MDOTCP_USE,0.00,0.000001,closea)
        IF (.NOT.closea) THEN
          COUT(1) = -ALPHA * (MDOTCP_MOD + UA) - 
     &             COLL_MASS_CP_PROD / TIMSEC
          COUT(2) = ALPHA * MDOTCP_MOD
          COUT(3) = ((1. - ALPHA) * (PCRP(ICON1) + PUAP(INOD1)) -
     &           COLL_MASS_CP_PROD / TIMSEC) * CSVP(INOD1,1) -
     &           (1. - ALPHA) * PCRP(ICON1) * PCTP(ICON1) -
     &            ALPHA * (UA * PCNTMF(IPCOMP) + HEATGAIN) -
     &           (1. - ALPHA) * (PUAP(INOD1) * PCNTMP(IPCOMP) + 
     &           PCQP(INOD1))
        ELSE
          COUT(1) = -ALPHA * UA - COLL_MASS_CP_PROD / TIMSEC
          COUT(2) = 0.
          COUT(3) = ((1. - ALPHA) * PUAP(INOD1) -
     &           COLL_MASS_CP_PROD / TIMSEC) * CSVP(INOD1,1) -
     &           ALPHA * (UA * PCNTMF(IPCOMP) + HEATGAIN) -
     &           (1. - ALPHA) * (PUAP(INOD1) * PCNTMP(IPCOMP) + 
     &           PCQP(INOD1))
        ENDIF ! Matches IF (.NOT.closea)

C---- Store "environment" variables future values

C---- UA to surrounding
        PUAF(INOD1) = UA
C---- Temperature of incoming flow to collector        
        PCTF(ICON1) = CONVAR(ICON1,1)
C---- Mass flow rate x capacitance of incoming flow
        PCRF(ICON1) = MDOTCP_MOD
C---- Heat gain by collector node
        PCQF(INOD1) = HEATGAIN

C---- Set values for actual heat loss from and gain to the collector for
C---- output

        call eclose(MDOTCP_MOD,0.00,0.000001,closea)
        IF((.NOT.closea).AND.(MDOTCP_MOD.GT.0.)) THEN
C---- Modify heat gain and loss by the heat capacity ratio
          HEATGAIN_ACTUAL = HEATGAIN * MDOTCP_USE / MDOTCP_MOD
          HEATLOSS_ACTUAL = 
     &                  UA * (CSVF(INOD1,1) - PCNTMF(IPCOMP)) *
     &                  MDOTCP_USE / MDOTCP_MOD
        ELSE
          HEATGAIN_ACTUAL = HEATGAIN
          HEATLOSS_ACTUAL = UA * (CSVF(INOD1,1) - PCNTMF(IPCOMP))
        ENDIF ! Matches IF((.NOT.closea).AND.(MDOTCP_MOD.GT.0.))

C---- Net collector heat gain is non-zero only when there is
C---- flow through the collector

C---- Establish additional output variables

        NAPDAT(IPCOMP) = 5
        PCAOUT(IPCOMP,1) = G
        PCAOUT(IPCOMP,2) = CAI
        PCAOUT(IPCOMP,3) = PCNTMF(IPCOMP)
        PCAOUT(IPCOMP,4) = HEATGAIN_ACTUAL
        PCAOUT(IPCOMP,5) = HEATLOSS_ACTUAL

C=======================================================================
C     Generate coefficients for 1st phase flow equation.
C=======================================================================

      ELSEIF(ISTATS.EQ.2) THEN

C---- Total actual mass flow rate is sum of water mass flow and
C     glycol mass flow

         MDOT_USE = PCONDR(ICON1)*CONVAR(ICON1,2) +
     &              PCONDR(ICON1)*CONVAR(ICON1,3)

C---- Mass fraction of propylene glycol in working fluid

         GLYCOL_MASS_FRACTION = ADATA(IPCOMP,34)
         COUT(1) = 1.
         COUT(2) = 0.
         COUT(3) = MDOT_USE * (1 - GLYCOL_MASS_FRACTION / 100.) 

C=======================================================================
C     Generate coefficients for 2nd phase flow equation.
C=======================================================================

      ELSEIF(ISTATS.EQ.3) THEN

C---- Total actual mass flow rate is sum of water mass flow and
C     glycol mass flow

         MDOT_USE = PCONDR(ICON1)*CONVAR(ICON1,2) +
     &              PCONDR(ICON1)*CONVAR(ICON1,3)

C---- Mass fraction of propylene glycol in working fluid
         GLYCOL_MASS_FRACTION = ADATA(IPCOMP,34)

         COUT(1) = 1.
         COUT(2) = 0.
         COUT(3) = MDOT_USE * GLYCOL_MASS_FRACTION / 100.

      ENDIF ! Matches IF(ISTATS.EQ.1)

C-----------------------------------------------------------------------
C     End of calculation
C-----------------------------------------------------------------------

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
        WRITE(ITU,*) ' Component     ',IPCOMP,':'
        WRITE(ITU,*) ' 1 node (ISV=20) Simplified solar collector'
        WRITE(ITU,*) ' Matrix node(s) ',INOD1
        WRITE(ITU,*) ' Connection(s)  ',ICON1
        IF(ISTATS.EQ.1) THEN
          WRITE(ITU,*) ' DAY        =', IDYF
          WRITE(ITU,*) ' HOUR       =', IHRF
          WRITE(ITU,*) ' '
          WRITE(ITU,*) ' SALT       =',SALT
          WRITE(ITU,*) ' SAZI       =',SAZI
          WRITE(ITU,*) ' CAI        =',CAI
          WRITE(ITU,*) ' QD         =',QD
          WRITE(ITU,*) ' QF         =',QF
          WRITE(ITU,*) ' G          =',G
          WRITE(ITU,*) ' '
          WRITE(ITU,*) ' CONT. TMP  =', PCNTMP(IPCOMP)
          WRITE(ITU,*) ' DELTAT     =', DELTAT
          WRITE(ITU,*) ' ETA0_LIN   =', ETA0_LIN
          WRITE(ITU,*) ' ETA1_LIN   =', ETA1_LIN
          WRITE(ITU,*) ' XIAM=      =', XIAM
          WRITE(ITU,*) ' '
          WRITE(ITU,*) ' MDOTCP_USE =', MDOTCP_USE
          WRITE(ITU,*) ' MDOTCP_MOD =', MDOTCP_MOD 
          WRITE(ITU,*) ' R          =', R
          WRITE(ITU,*) ' '
          WRITE(ITU,*) ' HEATGAIN   =', HEATGAIN
          WRITE(ITU,*) ' UA         = ',UA
        ENDIF ! Matches IF(ISTATS.EQ.1)
        WRITE(ITU,*) 'Exiting subroutine simple_solar_coll_coeff_gen'
      ENDIF ! Matches IF(ITC.GT.0.AND.NSINC.GE.ITC ...

C---- Return to the calling module

      RETURN

      END

C===================== GLYCOL_WATER_TBOIL =============================
C     Created by: Kamel Haddad
C     Created on: June 2006
C     Copyright:  CETC 2006
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function returns boiling temperature in Deg C of propylene
C     glycol water mixture for a certain glycol volume fraction
C
C References:
C Physical Properties of Secondary Coolants (Brines). Chapter 21 of the 
C 1997 ASHRAE Handbook of Fundamentals. 
C=======================================================================

      REAL FUNCTION GLYCOL_WATER_TBOIL(GLYCOL_VOL_FRACTION)

      IMPLICIT NONE

      REAL GLYCOL_VOL_FRACTION ! Glycol volume fraction in mixture (%)

C---- Set boiling temperature

      GLYCOL_WATER_TBOIL = 
     &                    0.0000000768 * GLYCOL_VOL_FRACTION**5 - 
     &                    0.0000134283 * GLYCOL_VOL_FRACTION**4 +
     &                    0.0008116883 * GLYCOL_VOL_FRACTION**3 -
     &                    0.0177068683 * GLYCOL_VOL_FRACTION**2 +
     &                    0.1700612687 * GLYCOL_VOL_FRACTION +
     &                    99.6798713492

      RETURN
      END

C===================== GLYCOL_WATER_TFREEZE ===========================
C     Created by: Kamel Haddad
C     Created on: June 2006
C     Copyright:  CETC 2006
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function returns freezing temperature in Deg C of propylene
C     glycol water mixture for a certain glycol volume fraction
C
C References:
C Physical Properties of Secondary Coolants (Brines). Chapter 21 of the 
C 1997 ASHRAE Handbook of Fundamentals. 
C=======================================================================

      REAL FUNCTION GLYCOL_WATER_TFREEZE(GLYCOL_VOL_FRACTION)

      IMPLICIT NONE

      REAL GLYCOL_VOL_FRACTION    ! Glycol volume fraction (%)

C---- Set freezing point temperature

      GLYCOL_WATER_TFREEZE = 
     &                   -0.00018486 * GLYCOL_VOL_FRACTION**3 + 
     &                    0.00290062 * GLYCOL_VOL_FRACTION**2 -
     &                    0.35822839 * GLYCOL_VOL_FRACTION +
     &                    0.04705087

      RETURN
      END

C===================== GLYCOL_WATER_CP ================================
C     Created by: Kamel Haddad
C     Created on: June 2006
C     Copyright:  CETC 2006
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function returns specific heat in J/kg-DegC of propylene
C     glycol water mixture for a certain glycol volume fraction and 
C     temperature
C     Note: Only glycol fractions of 0. and 50.% are currently supported
C
C References:
C Physical Properties of Secondary Coolants (Brines). Chapter 21 of the 
C 1997 ASHRAE Handbook of Fundamentals. 
C=======================================================================

      REAL FUNCTION GLYCOL_WATER_CP(GLYCOL_VOL_FRACTION,TEMP)

      IMPLICIT NONE

      REAL GLYCOL_VOL_FRACTION    ! Glycol volume fraction in mixture (%)
      REAL TEMP                   ! Temperature of mixture (Deg C)
      REAL SHTFLD                 ! Function for specific heat of water (J/kg-Deg C)

      LOGICAL close0,close50      

C---- Check whether glycol volume fraction is close to 0. or 50.%
      call eclose(GLYCOL_VOL_FRACTION,0.,1.,close0)
      call eclose(GLYCOL_VOL_FRACTION,50.,1.,close50)

      IF(close0) THEN

C---- Function for pure water
        GLYCOL_WATER_CP = SHTFLD(3,TEMP)

      ELSE

C---- The following correlation is used for all other volume fractions.
C---- This correlation though is intended to apply for 50% vol. fraction      
        GLYCOL_WATER_CP = 3.858548 * TEMP + 
     &                    3454.717742

      ENDIF ! Matches IF(close0)

      RETURN
      END

C===================== GLYCOL_WATER_DENSITY=============================
C     Created by: Kamel Haddad
C     Created on: June 2006
C     Copyright:  CETC 2006
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function returns density in kg/m3 of propylene
C     glycol water mixture for a certain glycol volume fraction and 
C     temperature
C     Note: Only glycol fractions of 0 and 50% are currently supported
C
C References:
C Physical Properties of Secondary Coolants (Brines). Chapter 21 of the 
C 1997 ASHRAE Handbook of Fundamentals. 
C=======================================================================

      REAL FUNCTION 
     &     GLYCOL_WATER_DENSITY(GLYCOL_VOL_FRACTION,TEMP)

      IMPLICIT NONE

      REAL GLYCOL_VOL_FRACTION       ! Volume fraction of glycol in mixture (%)
      REAL TEMP                      ! Temperature of mixture (Deg C)
      REAL RHOFLD                    ! Function for density of water (kg/m3)

      LOGICAL close0,close50,close90

C---- ESP-r common holding output console unit number
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C---- Check whether volume fraction is 0, 50, or 90%
      call eclose(GLYCOL_VOL_FRACTION,0.,1.,close0)
      call eclose(GLYCOL_VOL_FRACTION,50.,1.,close50)
      call eclose(GLYCOL_VOL_FRACTION,90.,1.,close90)

      IF(close0) THEN

C---- This is the function for pure water
        GLYCOL_WATER_DENSITY = RHOFLD(3,TEMP)

      ELSEIF(close90) THEN

C---- This is the correlation for 90% vol glycol
        GLYCOL_WATER_DENSITY = 
     &                   -0.00046217 * TEMP**2 -
     &                    0.75925000 * TEMP    +
     &                    1066.45671302

C---- The following correlation is used for all other volume fractions.
C---- This correlation though is intended to apply for 50% vol. fraction      
      ELSE
        
        GLYCOL_WATER_DENSITY = 
     &               -0.00256715 * TEMP**2 -
     &                0.44036616 * TEMP +
     &                1052.70468968

      ENDIF ! Matches IF(close0)

      RETURN
      END

C===================== GLYCOL_VOL_FRACTION_FIND=========================
C     Created by: Kamel Haddad
C     Created on: June 2006
C     Copyright:  CETC 2006
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function returns glycol volume fraction based on mass fraction in
C     water glycol-mixture
C
C References:
C Physical Properties of Secondary Coolants (Brines). Chapter 21 of the 
C 1997 ASHRAE Handbook of Fundamentals. 
C=======================================================================

      REAL FUNCTION GLYCOL_VOL_FRACTION_FIND(MASS_FRACTION,TEMP)

      IMPLICIT NONE

      REAL MASS_FRACTION     ! Mass fraction of glycol in the mixture (%)
      REAL TEMP              ! Temperature of the glycol-water mixture (Deg C)
      REAL ERROR             ! Iteration error
      REAL VOL_FRACTION_OLD  ! Old estimate of volume fraction during iteration (%)
      REAL VOL_FRACTION_NEW  ! New estimate of volume fraction during iteration (%)
      REAL TBOIL             ! Boiling temperature of glycol-water mixture (Deg C)
      REAL TFREEZE           ! Freezing temperature of glycol-water mixture (Deg C)
      REAL MIX_TEMP          ! Temperature of the glycol-water mixture (Deg C)
      REAL GLY_WATER_DEN     ! Density of glycol-water mixture (kg/m3)
      REAL GLY_DEN           ! Density of glycol (kg/m3)
      REAL GLYCOL_WATER_DENSITY 
                             ! Function for glycol-water mixture density (kg/m3)
      REAL GLYCOL_WATER_TBOIL
                             ! Function for glycol-water boiling temperature 
      REAL GLYCOL_WATER_TFREEZE
                             ! Function for glycol-water freezing point
      REAL SMALL             
      PARAMETER (SMALL=1.0E-15)

C---- Initially set volume fraction of glycol to mass fraction
      VOL_FRACTION_OLD = MASS_FRACTION

C---- High limit for iteration error
      ERROR = 1.

C---- Iterate to find the volume fraction      
      DO WHILE(ERROR.GT.0.01)

C---- Boiling point of glycol-water mixture
        TBOIL = GLYCOL_WATER_TBOIL(VOL_FRACTION_OLD)

C---- Freezing temperature of glycol-water mixture
        TFREEZE = GLYCOL_WATER_TFREEZE(VOL_FRACTION_OLD)

C---- Set temperature of glycol-water mixture using minimum and max
C---- bounds
        MIX_TEMP = MAX(TFREEZE,MIN(TBOIL,TEMP))

C---- Density of glycol. Use same density as that for 90% vol.
C---- glycol mixture
        GLY_DEN = GLYCOL_WATER_DENSITY(90.,MIX_TEMP)

C---- Density of glycol-water mixture
        GLY_WATER_DEN = 
     &     GLYCOL_WATER_DENSITY(VOL_FRACTION_OLD,MIX_TEMP)

C---- Glycol volume fraction in mixture
        VOL_FRACTION_NEW = MASS_FRACTION * 
     &               GLY_WATER_DEN / (GLY_DEN + SMALL)

C---- Update error
        ERROR = 
     &  abs((VOL_FRACTION_NEW - VOL_FRACTION_OLD) /
     &       VOL_FRACTION_OLD + SMALL)

C---- Update previous estimate for volume fraction
        VOL_FRACTION_OLD = VOL_FRACTION_NEW

      END DO

C---- Set glycol volume fraction to final estimate
      GLYCOL_VOL_FRACTION_FIND = VOL_FRACTION_NEW

      RETURN

      END

C===================== GLYCOL_WATER_VISCOSITY ==========================
C     Created by: Kamel Haddad
C     Created on: June 2006
C     Copyright:  CETC 2006
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function returns glycol-water mixture viscosity for a given 
C     volume fraction of glycol and mixture temperature
C     Note: Only glycol fractions of 0 and 50% are currently supported
C
C References:
C Physical Properties of Secondary Coolants (Brines). Chapter 21 of the 
C 1997 ASHRAE Handbook of Fundamentals. 
C=======================================================================

      REAL FUNCTION 
     &GLYCOL_WATER_VISCOSITY(GLYCOL_VOL_FRACTION,TEMP)

      REAL GLYCOL_VOL_FRACTION     ! Volume fraction in the mixture (%)
      REAL TEMP                    ! Temperature of mixture in (Deg C)
      REAL FUNVIS                  ! Function for determining dynamic viscosity of
                                   ! glycol-water mixture (N-s/m2)

      LOGICAL close0,close50

C---- Check whether volume fraction is 0 or 50%
      call eclose(GLYCOL_VOL_FRACTION,0.,1.,close0)
      call eclose(GLYCOL_VOL_FRACTION,50.,1.,close50)

      IF(close0) THEN

        IF(TEMP.LT.20) THEN

          GLYCOL_WATER_VISCOSITY = (-8.06667E-6 * TEMP**4 +
     &                 3.20667E-4 * TEMP**3 -
     &                 2.99833E-3 * TEMP**2 -
     &                 4.38167E-2 * TEMP +
     &                 1.794) / 1.E3

        ELSEIF((TEMP.GE.20).AND.(TEMP.LT.50)) THEN

          GLYCOL_WATER_VISCOSITY = (-6.44444E-6 * TEMP**3 +
     &                  9.21429E-4 * TEMP**2 -
     &                  5.39603E-2 * TEMP +
     &                  1.75483) / 1.E3

        ELSE

          GLYCOL_WATER_VISCOSITY = (-3.2381E-7 * TEMP**3 +
     &                  1.36857E-4 * TEMP**2 -
     &                  2.04219E-2 * TEMP +
     &                  1.27443) / 1.E3

        ENDIF ! Matches IF(TEMP.LT.20)

      ELSE

C---- Following correlations are for 50% vol. glycol in mixture
        IF(TEMP.LT.0.) THEN

          GLYCOL_WATER_VISCOSITY = (-6.06667E-3 * TEMP**3 - 
     &                 9.02857E-2 * TEMP**2 -
     &                 2.2869 * TEMP + 
     &                 15.018) / 1.E3

        ELSEIF((TEMP.GE.0.).AND.(TEMP.LT.60.)) THEN
       
          GLYCOL_WATER_VISCOSITY = (2.3901E-6 * TEMP**4 -
     &                 4.17092E-4 * TEMP**3 + 
     &                 2.87333E-2 * TEMP**2 -
     &                 1.01465 * TEMP + 
     &                 18.346) / 1.E3

        ELSE

          GLYCOL_WATER_VISCOSITY = (-2.84382E-6 * TEMP**3 + 
     &                 1.03287E-3 * TEMP**2 -
     &                 1.33042E-1 * TEMP + 
     &                 6.61086) / 1.E3

        ENDIF ! Matches IF(TEMP.LT.0.)

      ENDIF ! Matches IF(close0)

      RETURN

      END

C===================== GLYCOL_WATER_COND ======================
C     Created by: Kamel Haddad
C     Created on: June 2006
C     Copyright:  CETC 2006
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function returns glycol-water mixture thermal conductivity for 
C     a given volume fraction of glycol and mixture temperature
C     Note: Only glycol fractions of 0 and 50% are currently supported
C
C References:
C Physical Properties of Secondary Coolants (Brines). Chapter 21 of the 
C 1997 ASHRAE Handbook of Fundamentals. 
C=======================================================================

      REAL FUNCTION 
     &GLYCOL_WATER_COND(GLYCOL_VOL_FRACTION,TEMP,DENSITY)

      REAL GLYCOL_VOL_FRACTION     ! Volume fraction in the mixture (%)
      REAL TEMP                    ! Temperature of mixture (Deg C)
      REAL TEMP_DEGK               ! Temperature (Deg K)
      REAL DENSITY                 ! Density of glycol-water mixture (kg/m3)
      REAL FUNCOND                 ! Function for determining dynamic viscosity of
                                   ! glycol-water mixture (W/m-K)

      LOGICAL close0,close50

C---- Check whether volume fraction is 0 or 50%
      call eclose(GLYCOL_VOL_FRACTION,0.,1.,close0)
      call eclose(GLYCOL_VOL_FRACTION,50.,1.,close50)

      IF(close0) THEN

C---- This is the function for pure water      
        TEMP_DEGK = TEMP + 273.15
        GLYCOL_WATER_COND = FUNCOND(TEMP_DEGK,DENSITY)

      ELSE

C---- Following correlation is for 50% vol. glycol in mixture        
        GLYCOL_WATER_COND = -3.8581E-6 * TEMP**2 + 
     &                       7.107268E-4 * TEMP + 
     &                       3.494378982E-1  

      ENDIF ! Matches IF(close0)

      RETURN

      END
