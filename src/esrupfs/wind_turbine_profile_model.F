C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004/2005. Please Contact Ian
C Beausoliel-Morrison for details concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C--Subroutines and Functions in the unit.---

C SUBROUTINE POWOC_Wind_Prof_Supp_Elec(IPWC)
C     This routine is the main for the wind turbine profile model.
C
C REAL FUNCTION GetWindTurbinePower(WindSpeed)
C     Determines the Wind turbine power output based upon windspeed.
C
C REAL FUNCTION GetWTPower(IPWC)
C     Determines the Wind speed. Either using direct climate data or 
C     using Nick Kelly's probability distribution. Then calculated the 
C     resulting wind turbine power output using GetWindTurbinePower(WindSpeed).

C SUBROUTINE POWOC_Wind_Read_Data(IPWC)
C     This subroutine will read in all profile data as well as component data
C     required by the wind turbine profile model.
C     


C *********************************************************************
C Model: Wind_Turbine_Profile_model
C 
C SUBROUTINE POWOC_Wind_Prof_Supp_Elec(IPWC)
C
C Created by: Phylroy Lopez
C Initial Creation Date: Jan 2005
C  
C - This code will determine the power output of a wind turbine. 
C - A wind turbine pwr vs wind speed profile is used to determine the power output at any given time.  
C - The influence of geography and site layout on wind speed is handled through Nick Kelly's 
C   wind model. This is optional. Direct wind speed from the climate data can be used as well.
C -Note: In the case of Mulitple wind turbines it is assumed that the placement is as such that the turbines 
C     do not affect each other. 
C
C
C INPUTS:
C - IPWC: Power-only component index number
C
C *********************************************************************


      SUBROUTINE POWOC_Wind_Prof_Supp_Elec(IPWC)
      IMPLICIT NONE
#include "power.h"

C---------------------------------------------------------------------------------
C Common holding the power-only component description read from the electric
C network input file.
C---------------------------------------------------------------------------------
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)

C---------------------------------------------------------------------------------
C Common holding the name of the non-HVAC loads file specified in the electric
C network input file (other variables in this common not used here).
C---------------------------------------------------------------------------------
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C---------------------------------------------------------------------------------
C Common holding power generation and loads data for power-only components.
C---------------------------------------------------------------------------------
      COMMON/ENETPOWC/PPOWOC(MPOWCOM),QPOWOC(MPOWCOM),
     &VPOWC(MPOWCOM)

C---------------------------------------------------------------------------------
C Commons holding simulation time-clock variables.
C---------------------------------------------------------------------------------
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/btime/btimep,btimef
      common/ptime/ptimep,ptimef

C---------------------------------------------------------------------------------
C Common holding name of ASCII file containing wind-generated electricity
C time-step data.
C---------------------------------------------------------------------------------
      common/H2wind/LWndSupp,iWndSupp

C---------------------------------------------------------------------------------
C Common holding power entering/leaving components developed for wind-generated
C hydrogen-based system model.
C---------------------------------------------------------------------------------
       COMMON/WINDHYD/
     & QTotalWindSupply, 
     & QFarmG,
     & fWindspeedProfile,
     & fWindPowerProfile,
     & iDataPoints

C---------------------------------------------------------------------------------
C Common for passing error messages to calling routine (only used when running
C in dll mode).
C---------------------------------------------------------------------------------
      common/dllerr/dllsubr, dllmesg

C---------------------------------------------------------------------------------
C Declarations for common block variables.
C---------------------------------------------------------------------------------
      INTEGER      NPOWCOM, POWCOMNO, POWCOMID, POWCOMPHTYP, POWCONEN,
     &             NPOWCDAT, NPOWCDATS
      REAL         POWCDAT
      REAL         PPOWOC, QPOWOC
      COMPLEX      VPOWC
      INTEGER      IHRP, IHRF, IDYP, IDYF, IDWP, IDWF, NSINC, ITS
      REAL         btimep, btimef, ptimep, ptimef
      CHARACTER    LWndSupp*72
      INTEGER      iWndSupp
      character    dllsubr*12,dllmesg*124
      CHARACTER*12 ENODNAM, HYCOMTYPSTR, HYCOMNAM, POWCOMNAM,CONECOMNAM
      CHARACTER*72 POWCDATS
      REAL         QElecT, QTotalWindSupply, QFarmG
      REAL         GetWTPower

C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      LOGICAL   XST,dll
      CHARACTER OUTS*124
 
      REAL      fWindSpeedProfile(100),fWindPowerProfile(100)
      INTEGER   iDataPoints
C-------------------------------------------------------------------------------------------
C If this subroutine is being called for the first time then process the user-specified
C electrical supply data. Determine data type and convert from ASCII file to binary-format
C for direct access during simulation.
C-------------------------------------------------------------------------------------------
C Make sure profile is only read in once.
C      write(*,*) 'Wind Turbine Profile Active ',LWndSupp      

C Read the profile data and component data in.
      CALL POWOC_Wind_Read_Data(IPWC)

C-------------------------------------------------------------------------------------------
C Determine the electrical supply for the current time-step.
C-------------------------------------------------------------------------------------------

C  Take into account scalar multiplier applied to wind supply
C  POWCDAT(IPWC,1) holds the scalar multiplier (additional data item).

      QTotalWindSupply = GetWTPower(IPWC)*POWCDAT(IPWC,2)
C      write(*,*) 'Wind Turbine Power = ',QTotalWindSupply 
C Convert from kW to W and strore in PPOWOC
      PPOWOC(IPWC) = QTotalWindSupply * 1000.0 
      QPOWOC(IPWC) = 0.


      RETURN
      END


C *********************************************************************
C Model: Wind_Turbine_Profile_model
C 
C SUBROUTINE POWOC_Wind_Read_Data(IPWC)
C
C Created by: Phylroy Lopez
C Initial Creation Date: Jan 2005
C  
C - This subroutine will read in all the data required for the model. 
C - Data is to be entered via esp-r's component interface. 
C - The profile of the wind turbines performance is to be entered via an ascii 
C     input file. See examples for format.
C
C INPUTS:
C - IPWC: Power-only component index number
C
C *********************************************************************
C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_Wind_Read_Data(IPWC)
      IMPLICIT NONE
#include "power.h"

C External functions.     
      integer lnblnk

C---------------------------------------------------------------------------------
C Common holding the power-only component description read from the electric
C network input file.
C---------------------------------------------------------------------------------
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)

C---------------------------------------------------------------------------------
C Common holding the name of the non-HVAC loads file specified in the electric
C network input file (other variables in this common not used here).
C---------------------------------------------------------------------------------
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C---------------------------------------------------------------------------------
C Common holding power generation and loads data for power-only components.
C---------------------------------------------------------------------------------
      COMMON/ENETPOWC/PPOWOC(MPOWCOM),QPOWOC(MPOWCOM),
     &VPOWC(MPOWCOM)

C---------------------------------------------------------------------------------
C Commons holding simulation time-clock variables.
C---------------------------------------------------------------------------------
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/btime/btimep,btimef
      common/ptime/ptimep,ptimef

C---------------------------------------------------------------------------------
C Common holding name of ASCII file containing wind-turbine profile data.
C---------------------------------------------------------------------------------
      common/H2wind/LWndSupp,iWndSupp

C---------------------------------------------------------------------------------
C Common holding power entering/leaving components developed for wind-generated
C hydrogen-based system model.
C---------------------------------------------------------------------------------
      COMMON/WINDHYD/
     & QTotalWindSupply, 
     & QFarmG,
     & fWindspeedProfile,
     & fWindPowerProfile,
     & iDataPoints

C---------------------------------------------------------------------------------
C Common for passing error messages to calling routine (only used when running
C in dll mode).
C---------------------------------------------------------------------------------
      common/dllerr/dllsubr, dllmesg

      COMMON/FILEP/IFIL
      COMMON/ER/IER
      COMMON/OUTIN/IUOUT,IUIN
      
C---------------------------------------------------------------------------------
C Declarations for common block variables.
C---------------------------------------------------------------------------------
      INTEGER      NPOWCOM, POWCOMNO, POWCOMID, POWCOMPHTYP, POWCONEN,
     &             NPOWCDAT, NPOWCDATS
      REAL         POWCDAT
      REAL         PPOWOC, QPOWOC
      COMPLEX      VPOWC
      INTEGER      IHRP, IHRF, IDYP, IDYF, IDWP, IDWF, NSINC, ITS
      REAL         btimep, btimef, ptimep, ptimef
      CHARACTER    LWndSupp*72
      INTEGER      iWndSupp
      character    dllsubr*12,dllmesg*124
      CHARACTER*12 ENODNAM, HYCOMTYPSTR, HYCOMNAM, POWCOMNAM,CONECOMNAM
      CHARACTER*72 POWCDATS
      REAL         QElecT, QTotalWindSupply, QFarmG
      REAL         fWindSpeedProfile(100),fWindPowerProfile(100)

C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      LOGICAL   XST,dll
      CHARACTER OUTS*124
 
      CHARACTER OUTSTR*124,word*124
      INTEGER IUNIT,IFIL,k,IER,ND,ival,i,ISTAT,IUOUT, IUIN
      REAL val
      
      INTEGER iDatapoints
      REAL HDWTI
      REAL NTURB
      REAL ILOC
      REAL REFH
      REAL IWSTAT
      REAL IPROF
      REAL TURBI

      INTEGER IWndSupp_ascii
      LOGICAL wnderror
C-------------------------------------------------------------------------------------------
C If this subroutine is being called for the first time then process the user-specified
C electrical supply data. Determine data type and convert from ASCII file to binary-format
C for direct access during simulation.
C-------------------------------------------------------------------------------------------

C Read in the component data.
      HDWTI= POWCDAT(IPWC,1)
      NTURB= POWCDAT(IPWC,2)
      ILOC= POWCDAT(IPWC,3)
      REFH= POWCDAT(IPWC,4)
      IWSTAT= POWCDAT(IPWC,5)
      IPROF= POWCDAT(IPWC,6)
      TURBI= POWCDAT(IPWC,7)

C Make sure profile is only read in once.       
      IF(iWndSupp==0) THEN
C         write(outs,*) 'Wind Turbine Profile Active ',LWndSupp
C POWCDATS(IPWC,1) holds the profile file name (additional data string).
         LWndSupp = POWCDATS(IPWC,1)
C         write(outs,*),LWndSupp
         XST=.FALSE.
         call FINDFIL(LWndSupp,XST)
         if(XST)then
            iWndSupp=1
         else
           write(outs,'(3a)') 'Wind Turbine Profile File ',LWndSupp
     &       (1:lnblnk(LWndSupp)),' was referenced but not found.'
            call isadll(dll)  ! Move this to within loop below.
            if(dll)then
               dllsubr='ersys'
               dllmesg=outs
               ier=2
               return
            else
               call edisp(iuout,outs)
               STOP ' Need wind profile data: unresolvable error.'
            endif
         endif
C Get profile into array.
C Determine number of data points in profile.
C-----Notify user that wind-generated electric supply data are
C     being processed (this can take some time).

C      call usrmsg(' Processing wind-gen electric data...',' ','-')
C-----Open the .wnd file.
      wnderror = .false.
      IWndSupp_ascii = IFIL+16  ! Should be a safe unit number to use.
      CALL ERPFREE(IWndSupp_ascii,ISTAT)
      CALL EFOPSEQ(IWndSupp_ascii,LWndSupp,1,IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) ' Error opening windturbine profile  input file.'
        wnderror = .true.
      ENDIF
      K=0
C      write(*,*)'OPening file',LWndSupp
      CALL EFOPSEQ(IWndSupp_ascii, LWndSupp,1,IER)
      CALL STRIPC(IWndSupp_ascii,OUTSTR,99,ND,1,
     &'Wind Turbine Profile',IER)
      CALL EGETWI(OUTSTR,K,ival,1,100,'F',
     &'Wind Turbine Profile data: data points',IER)
      iDataPoints=ival
C      write(*,*)'Wind Turbine: Data Points = ',iDatapoints
C place profile into arrays. 
      do 10 i = 1,iDataPoints
      CALL STRIPC(IWndSupp_ascii,OUTSTR,99,ND,1,
     & 'Wind Turbine Profile:data value',IER)
       K=0
       val=0
      CALL EGETWR(OUTSTR,K,val,0.,1e6,'F',
     & 'WindProfile: wind speed',IER)
      fWindSpeedProfile(i)= val
C      write (*,*)'Wind speed data point read:',fWindSpeedProfile(i)
      val=0
      CALL EGETWR(OUTSTR,K,val,0.,1e6,'F',
     & 'WindProfile: wind power',IER)
      fWindPowerProfile(i)= val
C      write (*,*)'Wind power data point read:',fWindPowerProfile(i)
  10  end do
      END IF
      RETURN
      END

      
C *********************************************************************
C Model: Wind_Turbine_Profile_model
C 
C REAL FUNCTION GetWindTurbinePower(WindSpeed)
C
C Created by: Phylroy Lopez
C Initial Creation Date: Jan 2005
C  
C - This routine will determine the wind power generated via the profile given in 
C     the input file. It uses simple lineat interpolaton to determine the power.Any
C     wind speeds outside of the given power curves will return a zero.
C
C INPUTS:
C     - REAL WindSpeed   The windspeed in m/s
C
C *********************************************************************      
      REAL FUNCTION GetWindTurbinePower(WindSpeed)
      IMPLICIT NONE
      COMMON/WINDHYD/
     & QTotalWindSupply, 
     & QFarmG,
     & fWindspeedProfile,
     & fWindPowerProfile,
     & iDataPoints
      REAL         QElecT, QTotalWindSupply, QFarmG
      REAL         fWindSpeedProfile(100),fWindPowerProfile(100)    
      INTEGER      iDataPoints,i  
      REAL WindSpeed
      i=0.
      GetWindTurbinePower =0.
C      Determine if within turbine power curve range. 
      IF  ( WindSpeed .le. fWindSpeedProfile(iDataPoints) .and.
     &     WindSpeed .ge. fWindSpeedProfile(1)) THEN
C     Find range
         DO WHILE (GetWindTurbinePower .eq. 0. .and. i .lt.iDataPoints )
            i=i+1
            if ( WindSpeed .ge. fWindSpeedProfile(i) .and. 
     &           WindSpeed .lt. fWindSpeedProfile(i+1) ) then
               
C     use linear interpolation
               
               GetWindTurbinePower = fWindPowerProfile(i)
     &              +(  ( Windspeed - fWindSpeedProfile(i))
     &              /
     &              ( fWindSpeedProfile(i+1) - fWindSpeedProfile(i) ) 
     &              * (fWindPowerProfile(i+1) - fWindPowerProfile(i)))
            ENDIF
         END DO   
            
C     Use linear interpolation to determine power output.
      ELSE
         GetWindTurbinePower = 0.
      ENDIF
      RETURN
      END  ! of FUNCTION
      
      
 
C *********************************************************************
C Model: Wind_Turbine_Profile_model
C 
C REAL FUNCTION GetWindTurbinePower(WindSpeed)
C
C Created by: Phylroy Lopez
C Initial Creation Date: Jan 2005
C  
C - This routine will determine the wind power generated via the profile given in 
C     the input file. It uses simple linear interpolaton to determine the power.Any
C     wind speeds outside of the given power curve will return a zero. This will determine the 
C     actual wind speed and use it with the GetWindturbine Power 
C
C INPUTS:
C     - INTEGER IPWC    Component number.
C
C *********************************************************************      
       REAL FUNCTION GetWTPower(IPWC)
       IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

C---------------------------------------------------------------------------------
C Common holding the power-only component description read from the electric
C network input file.
C---------------------------------------------------------------------------------
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)

C---------------------------------------------------------------------------------
C Common holding the name of the non-HVAC loads file specified in the electric
C network input file (other variables in this common not used here).
C---------------------------------------------------------------------------------
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C---------------------------------------------------------------------------------
C Common holding power generation and loads data for power-only components.
C---------------------------------------------------------------------------------
      COMMON/ENETPOWC/PPOWOC(MPOWCOM),QPOWOC(MPOWCOM),
     &VPOWC(MPOWCOM)

C---------------------------------------------------------------------------------
C Commons holding simulation time-clock variables.
C---------------------------------------------------------------------------------
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/btime/btimep,btimef
      common/ptime/ptimep,ptimef

C---------------------------------------------------------------------------------
C Common holding name of ASCII file containing wind-turbine profile data.
C---------------------------------------------------------------------------------
      common/H2wind/LWndSupp,iWndSupp

C---------------------------------------------------------------------------------
C Common holding power entering/leaving components developed for wind-generated
C hydrogen-based system model.
C---------------------------------------------------------------------------------
      COMMON/WINDHYD/
     & QTotalWindSupply, 
     & QFarmG,
     & fWindspeedProfile,
     & fWindPowerProfile,
     & iDataPoints

C---------------------------------------------------------------------------------
C Common for passing error messages to calling routine (only used when running
C in dll mode).
C---------------------------------------------------------------------------------
      common/dllerr/dllsubr, dllmesg

      COMMON/FILEP/IFIL
      COMMON/ER/IER
      COMMON/OUTIN/IUOUT,IUIN
      
C---------------------------------------------------------------------------------
C Declarations for common block variables.
C---------------------------------------------------------------------------------
      INTEGER      NPOWCOM, POWCOMNO, POWCOMID, POWCOMPHTYP, POWCONEN,
     &             NPOWCDAT, NPOWCDATS
      REAL         POWCDAT
      REAL         PPOWOC, QPOWOC
      COMPLEX      VPOWC
      INTEGER      IHRP, IHRF, IDYP, IDYF, IDWP, IDWF, NSINC, ITS
      REAL         btimep, btimef, ptimep, ptimef
      CHARACTER    LWndSupp*72
      INTEGER      iWndSupp
      character    dllsubr*12,dllmesg*124
      CHARACTER*12 ENODNAM, HYCOMTYPSTR, HYCOMNAM, POWCOMNAM,CONECOMNAM
      CHARACTER*72 POWCDATS
      REAL         QElecT, QTotalWindSupply, QFarmG
      REAL         fWindSpeedProfile(100),fWindPowerProfile(100)

C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      LOGICAL   XST,dll
 
      CHARACTER OUTSTR*124,word*124
      INTEGER IUNIT,IFIL,k,IER,ND,ival,i,ISTAT,IUOUT, IUIN
      REAL val
      
      INTEGER iDatapoints
      

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      INTEGER IFLWN,ICFFS
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      REAL DRYB,QDIF,QDNR,WDIR,WSPD,WRED
      INTEGER IRVH
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMPLEX VSPM
      REAL MINU,MAXU,MINV,MAXV,INC


      character spmlabel*16, outs*124,spflnam*72,STRSPM*16
      character LAPROB*72
      logical close

      REAL HDWTI
      REAL NTURB
      REAL ILOC
      REAL REFH
      REAL IWSTAT
      REAL IPROF
      REAL TURBI
      REAL PI
      REAL WIDIR
      REAL WISPD
      REAL DRYBT
      REAL ROUGH
      REAL COMPU
      REAL COMPV
      REAL STDEV
      INTEGER ICOUNT
      REAL PWRTOT
      REAL VALU
      REAL TPRU
      REAL TPRC
      REAL PDU
      REAL TPRV
      REAL VALV
      INTEGER J
      REAL PDV
      REAL PRC
      REAL TSL 
      REAL TDUR
      REAL WISPDM
      REAL WIDIRM
      REAL PWR,POWER
C Subroutine
      REAL  GetWindTurbinePower     

      REAL TotalPRC


C Read in the component data.

      HDWTI = POWCDAT( IPWC , 1 )
      NTURB = NINT( POWCDAT ( IPWC , 2 ) )
      ILOC = NINT( POWCDAT( IPWC , 3 ) )
      REFH = POWCDAT( IPWC , 4 )
      IWSTAT = NINT(POWCDAT( IPWC , 5 ) )
      IPROF = NINT( POWCDAT( IPWC , 6 ) )
      TURBI = POWCDAT( IPWC , 7 )

C Set up pi
      PI = 22. / 7.

C Get the current climate data 
      WIDIR = DF
      WISPD = VF
      DRYBT = TF

      IF( WISPD .LT. 1E-3 ) WISPD = 1E-3

C Calculate the wind velocity at the height of the wind turbine adjust the free
C stream velocity to height. This data should be available if a flow network
C is active. I f profiles or speed reduction are not specified then default
C values are used.
      IF(WRED .GT. 1E-6 .AND. IPROF .LT. 1 ) THEN
        WISPD = WISPD * WRED
      ELSEIF(IPROF.GE.1)THEN
C      write(*,*)'Profile active',IPROF
C If profiles are on then fill in the appropriate parameters
C 1 urban, 2 suburban, 3 rural.
        IF(ILOC.EQ.1)THEN

C Urban location
          PWR = 0.32
          ROUGH = 5.
        ELSEIF(ILOC.EQ.2)THEN

C Suburban location
          PWR = 0.23
          ROUGH = 0.8
        ELSE

C Rural location
          PWR = 0.14
          ROUGH = 0.05
        ENDIF

C Set the new wind speed and turbulent intensity from profiles. If height
C is zero set to a small value to avoid a NaN.

C set height of wind turbine to a small number if zero.. 
        IF( HDWTI .LT. 1E-6 ) HDWTI = 1E-6

C set reference height to small number if zero.
        IF( REFH .LT. 1E-6 ) REFH = 1E-6

C The new wind speed corrected for height of tower and reference height (height of weather sation tower)
        WISPD = WISPD * ( HDWTI / REFH ) **PWR

C Calulation of Turbidity.
C Commented out until Nick Kelly gets back to me on this equation..PL
C        TURBI=(ROUGH*LOG(30./REFH))/LOG(HDWTI/REFH)

C        write(*,*) 'Profile Wind speed ',WISPD
C        write(*,*) 'Profile TURBI ',TURBI

      ENDIF

      

      IF(IWSTAT.GT.0)THEN

C Calculate wind velocity components
        COMPU = WISPD * SIN ( ( WIDIR * PI ) / 180. )
        COMPV = WISPD * COS ( ( WIDIR * PI ) / 180. )

C Calculate standard deviation, based on turbulent intensity.
        STDEV=TURBI*WISPD

C Calculate the range of wind component speeds
        MINU = COMPU - 3 * STDEV
        MAXU = COMPU + 3 * STDEV
        MINV = COMPV - 3 * STDEV
        MAXV = COMPV + 3 * STDEV


C Calculate the increment (divided into 10 of a range -3 to +3)
        INC=6.0*STDEV/10. 

C initialize counter. 
        ICOUNT=1

C initialize total for timestep.
        PWRTOT=0.

C Set up starting U values.
        VALU=MINU

        TotalPRC = 0.

C Set up loop to generate power output. 
        DO 66 I=1,10
C Calculate the current U-value probability density
          PDU = ( 1. / (STDEV * ( 2. * PI ) ** 0.5 ) ) *
     & EXP( - ( ( ( VALU - COMPU ) ** 2. ) / ( 2. * STDEV ** 2. ) ) )



C Set the starting value of V
          VALV=MINV
 
          DO 77 J=1,10

C Ditto for the V-value
            PDV=( 1. / ( STDEV * ( 2. * PI ) ** 0.5 ) ) *
     & EXP( - ( ( ( VALV - COMPV ) ** 2. ) / ( 2. * STDEV ** 2. ) ) )
           

C Calculate the combined probability.
            PRC=( INC * PDU ) * ( INC * PDV )
           
C Calculate the duration of this wind speed and direction
            TSL = 3600. / REAL( ntstep )
            TDUR = PRC * TSL

           TotalPRC = TotalPRC + PRC


C Re-calculate the new wind speed.
            WISPDM = ( VALU ** 2. + VALV ** 2. ) ** 0.5

C ...and wind direction...
            IF(ABS(VALV).LT.1E-6)VALV=1E-6
            WIDIRM=ATAN(VALU/VALV)*(180./PI)
            IF(VALV.GE.0)THEN
              IF(VALU.GE.0.)THEN
                WIDIRM=WIDIRM
              ELSE
                WIDIRM=360.+WIDIRM
              ENDIF
            ELSE
              IF(VALU.GE.0.)THEN
                WIDIRM=180.+WIDIRM
              ELSE
                WIDIRM=180.+WIDIRM
              ENDIF
            ENDIF


C Get wind power for given wind speed.
            
C            write(*,*)'Stat Windspeed',WISPDM
            PWR = GetWindTurbinePower( WISPDM )
C            write(*,*)'Stat Power',PWR

C Increment the V direction.
            VALV=VALV+INC
C Obtain the number of KJ for probability slice.
            PWRTOT=PWRTOT+PWR*TDUR

  77      CONTINUE
          VALU=VALU+INC
  66    CONTINUE

C Write results for probability based power output
C        Convert back to kW. 
        POWER= PWRTOT / TSL
C        write(*,*)'Stat Total Power',POWER
C         write(*,*)'TotalPRC',TotalPRC
      ELSE       
        POWER= GetWindTurbinePower( WISPD )
C        write(*,*)'Windspeed',WISPD
C        write(*,*)'Power',POWER
      ENDIF
        GetWTPower = POWER
      RETURN
      END

