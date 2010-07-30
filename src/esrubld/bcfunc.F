C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

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

C This file contains the following routines:

C      MZBCTL: control executive.
C      MZMIXT: calculate `weighted' zone temperature.
C      CFVAR: get sensed condition for single sensor control.
C      CFMVAR: get sensed conditions for multi-sensor control.
C      BCL00: `weighted' temperature control.
C      BCL01: basic control.
C      BCL02: free float.
C      BCL03: basic (exponential) pre-heat (or cool) control.
C      BCL04: fixed heat injection/extraction control.
C      BCL05: proportional+integral+derivative control action.
C      BCL06: plant to building linker.
C      BCL07: multi-stage controller with hysteresis.
C      BCL08: CAV HAVC variable supply temperature system.
C      BCL09: heat pipe.
C      BCL10: on/off control.
C      BCL11: temperature brought to multi-sensor value (ideal).
C      BCL12: temperature brought to multi-sensor value (on-off).
C      BCL13: time-proportioning on/off control.
C      BCL14: floating (`three-position') control.
C      BCL15: optimum start logic controller.
C      BCL16: optimum stop logic controller.
C      BCL17: fuzzy logic PI/PD Controller.
C      BCL18: null controller.
C      bcl19: multi-sensor basic controller.
C      BCL20: evaporative source (defined as a controller).
C      BCL21: master slave capacity inj/ext controller
C      BCL22: VAV with CAV with reheat air supply controller
C      BCL23: Match sensed values (ideal:2 setpoints)
C      BCL24: Adaptive human comfort model
C      BCL99: constructional thermo-physical property variation.
C      MZRCPL: mixed temperature iteration.
C      ZRHCTL: relative humidity controlling routine
C      ZRH01:  room relative humidity control to compliment BCL01
C      FZINIT:intialise fuzzy membership variables.

C ******************** MZBCTL

C The main building control function executive.

      SUBROUTINE MZBCTL(ier,icomp)
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      common/dllerr/dllsubr,dllmesg

      character dllsubr*12,dllmesg*124,outs*124
      logical dll

C Check if running in dll mode.
      call isadll(dll)



C If actuator is not located in zone then no control is imposed
C regardless of control function prevailing at this time.
      IF(IBAN(ICF,1).EQ.-1.OR.(IBAN(ICF,1).EQ.-2.AND.IBAN(ICF,2).NE.
     &IICOMP).OR.(IBAN(ICF,1).GT.0.AND.IBAN(ICF,1).NE.IICOMP))GOTO 2

C For the special case of current zone, mixed node control only.
      IF(IBSN(ICF,1).EQ.-2.AND.IBSN(ICF,2).EQ.IICOMP
     &.AND.IBCLAW(ICF,IDTYP,IPER).NE.24)GOTO 77
      GOTO 76
   77 CALL BCL00
      return

C 17 control functions active at present + thermo-physical
C property substitution (BCL99).  If control function index
C negated then property substitution active within zone
C associated with control function.
   76 IFUNC=ABS(IBCLAW(ICF,IDTYP,IPER))
      GOTO
     &(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
     & ,IFUNC
      GOTO 98

C CL1 is basic control with potential for a different heating and
C cooling set point.   Limiting capacities allow either heating or
C cooling to be disallowed. Humidity control may also be active.
    1 CALL BCL01(ier)
      if(ier.eq.2)then
        return
      endif
      GOTO 99

C CL2 is a free-floating controller.
    2 CALL BCL02
      GOTO 99

C CL3 is a pre-heating/pre-cooling condition.  Limiting
C heating and cooling capacities are supplied and within these
C constraints temperatures will evolve exponentially towards
C the set point temperature of the control period which follows.
    3 CALL BCL03
      GOTO 99

C CL4 is a fixed heat injection or extraction period.
    4 CALL BCL04
      GOTO 99

C CL5 is a three-term PID controller.
    5 CALL BCL05
      GOTO 99

C CL6 allows zone input or extract flux to be set
C as a function of the plant network status.
    6 CALL BCL06(icomp)
      GOTO 99

C CL7 allows a multi-stage energy supply to
C be defined.
    7 CALL BCL07
      GOTO 99

C CL8 is a variable supply temperature system
C with limit constraints.
    8 CALL BCL08
      GOTO 99

C CL9 is a heat pipe which allows heat to be extracted from one
C node and transferred to another.
    9 CALL BCL09
      GOTO 99

C CL10 allows separate flux ON and flux OFF set points to
C be specified for both heating and cooling.
   10 CALL BCL10
      GOTO 99

C CL11 is a multi-sensor controller which will bring the 
C temperature of the associated zone to a specified
C function of the sensed temperatures at other locations.
C Heating and cooling restrictions are allowed.    
   11 CALL BCL11
      GOTO 99

C CL12 is a multi-sensor on/off controller. It will bring
C the temperature of the associated zone to a specified
C function of the sensed temperature at other locations.
C Heating and cooling restrictions are allowed.
   12 CALL BCL12
      GOTO 99

C CL13 is a time-proportioning on/off controller.
C Separate ON and OFF set-points may be specified.      
   13 CALL BCL13
      GOTO 99

C CL14 is a floating ('three-position') controller.   
   14 CALL BCL14
      GOTO 99

C CL15 is an optimum start logic controller. 
   15 CALL BCL15
      GOTO 99

C CL16 is an optimum stop logic controller. 
   16 CALL BCL16
      GOTO 99

C CL17 is a Fuzzy Logic controller. 
   17 CALL BCL17
      GOTO 99

C CL18 is a null controller. 
   18 CALL BCL18
      GOTO 99

C CL19 is a multi-sensor basic controller
   19 CALL BCL19
      GOTO 99

C CL20 evaporative source (e.g. swimming pool).
   20 call BCL20
      goto 99

C CL21 slave capacity controller
   21 call BCL21
      goto 99

C CL22 VAV and CAV air based controller
   22 call BCL22
      goto 99
      
C CL23 sensed setpoint based controller
   23 call BCL23
      goto 99
      
C CL24 Adaptive human comfort model
   24 call BCL24(ier)
      goto 99
      
C New control laws should be inserted here.
   98 continue
      write(outs,'(a,i3,a)')' Zone control function',IFUNC,
     &  ' is not implemented in this release.'
      call edisp(iuout,outs)
      if(dll)then
        dllsubr='MZBCTL'
        dllmesg=outs
        ier=2
        return
      else
        call epwait
        call epagend
        STOP
      endif

C CL99 will modify the thermo-physical properties of
C the specified multi-layered constructions.
   99 ifunc=ibsn(icf,4)
      if(ifunc.gt.0)then
        if(ibsn(ifunc,1).ne.-99)then
          write(outs,'(2a)')' Error: Nested control functions must',
     &      ' refer to control function -99.'
          if(dll)then
            dllsubr='MZBCTL'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            call epwait
            call epagend
            stop
          endif
        else

C Set ICALL to zero to signal nested control property substitution.
          ICALL=0
          call bcl99(ICALL)
        endif
      endif
      RETURN
      END

C ******************** MZMIXT

C MZMIXT computes the zone weighted temperature
C prevailing at the 'future' time-row of
C the current time step.

C Tmix = CONV*Tair + RAD*Tmrt
C This subroutine can be called from outwith a controls scenario by
C setting ICMP to negative unity times zone number. In such a case CONV
C is set to 0.5

C  IMRTT   - Defines the position of the MRT sensor within the zone
C            IMRTT=1 : selected surfaces associated with sensor
C            IMRTT=2 : all surfaces associated with sensor

C  ITSC    - dictates whether or not transparent surfaces can influence
C            the sensor.
C            ITSC=0 : no
C            ITSC=1 : yes

C  NSAMRT  - corresponds to IMRTT=1 and is an index of the number of
C            surfaces associated with the MRT sensor

C  IMRTS   - identifies which surfaces are used in the MRT calculation

      SUBROUTINE MZMIXT(ICMP,TMRT,TMIX)
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)

      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,
     &QFUT,TFUT
      COMMON/CONTL/IMRTT(MCOM),ITSC(MCOM),NSAMRT(MCOM),IMRTS(MCOM,MS)

      character outs*124
      logical close

C If a negative number is passed as icmp then change icmp to positive.
      if(icmp.lt.0)then
        icmp=-1*icmp
        conv=0.5
        ic=1
      endif

C Test that ic is non-zero.
      if(icf.ne.0)then
        IC=ICF
        CONV=FLOAT(IBSN(IC,3))/100.0
      endif

      RAD=1.0-CONV

C Fix sensor type for now.
      IMRTT(IC)=2
      ITSC(IC)=1

C Summate product of internal surface temperatures (opaque
C and transparent), and corresponding areas.
      TAI=TFUT
      NC=NCONST(ICMP)
      TMRT=TAI
      SUM=0.
      AREA=0.
      call eclose(RAD,0.00,0.1,close)
      IF(close)GOTO 8

C Opaque internal surfaces.
      DO 20 J=1,NC
        IF(IMRTT(IC).EQ.2)GOTO 3
        JJ=J
        DO 50 K=1,NSAMRT(IC)
          IF(JJ.NE.IMRTS(IC,K))GOTO 50
          GOTO 3
   50   CONTINUE
        GOTO 20
    3   TO=TFS(ICMP,J)
        SUM=SUM+TO*SNA(ICMP,J)
        AREA=AREA+SNA(ICMP,J)
   20 CONTINUE

C Compute weighted temperature.
      IF(AREA.LE.0.)GOTO 1000
      TMRT=SUM/AREA
    8 TMIX=CONV*TAI+RAD*TMRT
      GOTO 7
 1000 call edisp(iuout,
     &  ' MZMIXT: zero area associated with the mrt sensor!')
      write(outs,'(A,2I3,2F8.2)')' Zone ',ICMP,' ic tmrt tmix ',
     &  ic,tmrt,tmix
      call edisp(iuout,outs)
    7 RETURN
      END

C ******************** CFVAR

C CFVAR determines the sensed condition associated
C with the sensor defined for the control function.

      SUBROUTINE CFVAR(TCTL,IER)
#include "building.h"
#include "tdf2.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/btime/btimep,btimef
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)

C TDF commons.
C Index for each potential temporal db entry type 0=not used are in
C ommon tdfflg2

      common/bfngr/cfgsd(MCF,6),ibsnv(mcf,3),bfngn
      common/dllerr/dllsubr,dllmesg

      character dllsubr*12,dllmesg*124,outs*124
      DIMENSION VAL(MTABC+2)
      
      logical bfngn,svcfgr,dll
      
C Check if running in dll mode.
      call isadll(dll)
      svcfgr=.false.

C If function genrator referenced and required function
C is `sensed variable function`, then re-set IBSN(ICF,?)
      IF(IBSN(ICF,1).EQ.-6.AND.IBSN(ICF,2).EQ.9)THEN
         IBSNV(ICF,1)=IBSN(ICF,1)
         IBSNV(ICF,2)=IBSN(ICF,2)
         IBSNV(ICF,3)=IBSN(ICF,3)
         IBSN(ICF,1)=int(cfgsd(icf,1))
         IBSN(ICF,2)=int(cfgsd(icf,2))
         IBSN(ICF,3)=int(cfgsd(icf,3))
         SVCFGR=.TRUE.
      ENDIF

      IF(IBSN(ICF,1).EQ.0.OR.IBSN(ICF,1).EQ.IICOMP)THEN

C Match flow controller type. If PID use TFA otherwise use TNP.
C << check for ON/OFF OPTIMUM START etc. >>
        ICL=IBCLAW(ICF,IDTYP,IPER)
        if(ICL.eq.5)then
         TCTL=TFA(IICOMP)
        else
         TCTL=TNP
        endif
      ELSEIF(IBSN(ICF,1).GT.0.AND.IBSN(ICF,1).NE.IICOMP)THEN
         IZ=IBSN(ICF,1)
         IS=IBSN(ICF,2)
         IN=IBSN(ICF,3)
         IF(IN.EQ.0.AND.IS.EQ.0)TCTL=TFA(IZ)
         IF(IN.EQ.0.AND.IS.GT.0)TCTL=TFS(IZ,IS)
         IF(IN.GT.0.AND.IS.GT.0)TCTL=TFC(IZ,IS,IN)

C Sensor measures plant node state-space variable.
      ELSE IF(IBSN(ICF,1).EQ.-1) THEN

C Which is obviously not allowed in case of bld.
        write(outs,'(a)')
     &   ' CFVAR: simulator does not support sensing plant node state.'
        if(dll)then
          dllsubr='CFVAR'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(iuout,outs)
          call epwait
          call epagend
          STOP
        endif
      ELSEIF(IBSN(ICF,1).EQ.-2)THEN
         IZ=IBSN(ICF,2)

C If zone is not specified then take the current zone
         IF(IZ.EQ.0)IZ=IICOMP
         CALL MZMIXT(IZ,TMRT,TCTL)

      ELSEIF(IBSN(ICF,1).EQ.-3)THEN
C Match flow controller type.
        ICTYP=IBCTYP(ICF,IDTYP,IPER)

C Controller type 0: sensor measures temperature.
        IF(ICTYP.EQ.0)THEN
           IF(IBSN(ICF,2).EQ.0)THEN
              TCTL=TF
           ELSEIF(IBSN(ICF,2).EQ.1)THEN
              TCTL=SOLAIR(TF,QFF,QDF)
           ELSE
              GOTO 998
           ENDIF

C Controller type 35: sensor measures wind speed.
         ELSE IF(ICTYP.EQ.35)THEN     
           IF(IBSN(ICF,2).EQ.2)THEN
              TCTL=VF
           ELSE
              GOTO 998
           ENDIF
             
C Controller type 36: sensor measures wind direction.
         ELSE IF(ICTYP.EQ.36)THEN     
           IF(IBSN(ICF,2).EQ.3)THEN
              TCTL=DF
           ELSE
              GOTO 998
           ENDIF

C Controller type 37: sensor measures diffuse horizontal solar rad.
         ELSE IF(ICTYP.EQ.37)THEN
           IF(IBSN(ICF,2).EQ.4)THEN
              TCTL=QFF
           ELSE
              GOTO 998
           ENDIF

C Controller type 38: sensor measures direct normal solar rad.
         ELSE IF(ICTYP.EQ.38)THEN
           IF(IBSN(ICF,2).EQ.5)THEN
              TCTL=QDF
           ELSE
              GOTO 998
           ENDIF

C Controller type 39: sensor measures relative humidity outdoor air.
         ELSE IF(ICTYP.EQ.39)THEN
           IF(IBSN(ICF,2).EQ.6)THEN
              TCTL=HF
           ELSE
              GOTO 998
           ENDIF

C Invalid controller type.
         ELSE
           WRITE(outs,'(a,i3,a,i3,a)') 'CFVAR: function',ICF,' type',
     &        ICTYP,'invalid controller type.'
           if(dll)then
             dllsubr='CFVAR'
             dllmesg=outs
             ier=2
             return
           else
             call edisp(iuout,outs)
             call epwait
             call epagend
             STOP
           endif
         ENDIF

C Sensing mass flow component variable.          
      ELSEIF(IBSN(ICF,1).EQ.-4)THEN
C Which is currently not active in the case of bld.
         write(outs,'(a)')
     &  'CFVAR: simulator does not support sensing mass flow variable.'
         if(dll)then
           dllsubr='CFVAR'
           dllmesg=outs
           ier=2
           return
         else
           call edisp(iuout,outs)
           call epwait
           call epagend
           STOP
         endif
      ELSEIF(IBSN(ICF,1).EQ.-5)THEN

C Setpoint to be read from temporal file.
         if(ISETPTT(ICF).ne.0)then
           itdi=ISETPTT(ICF)
           IFOC=itdi
           CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
           TCTL=VAL(ISD)
C Debug.
C           write(6,*)'tdf ctl data @',btimef,' is item',itdi,
C     &       ' column ',isd,' value ',tctl,' for ctl loop ',icf

         else
           write(outs,'(a)')
     &     'CFVAR: ISETPTT does not point to a valid temporal item'
           if(dll)then
             dllsubr='CFVAR'
             dllmesg=outs
             ier=2
             return
           else
             call edisp(iuout,outs)
             call epwait
             call epagend
             STOP
           endif
         endif
      ELSEIF(IBSN(ICF,1).EQ.-6)THEN
         IFNC=IBSN(ICF,2)
         CALL CFFNGR(IFNC,TCTL)
      ENDIF

      IF(SVCFGR)THEN
         IFUNC=9
         CALL CFFNGR(IFUNC,TCTL)
         IBSN(ICF,1)=IBSNV(ICF,1)
         IBSN(ICF,2)=IBSNV(ICF,2)
         IBSN(ICF,3)=IBSNV(ICF,3)
      ENDIF
      RETURN

 998  WRITE(outs,'(a,i3,a,i3,a)') 'CFVAR: function',ICF,' type',
     &  ICTYP,' incorrect match of sensor and controller type.'
      if(dll)then
        dllsubr='CFVAR'
        dllmesg=outs
        ier=2
        return
      else
        call edisp(iuout,outs)
        call epwait
        call epagend
        STOP
      endif
      END

C ******************** CFMVAR
C Recover control point data.
      SUBROUTINE CFMVAR(ier)

#include "building.h"
#include "tdf2.h"
#include "control.h"      

      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/CMSVAR/NSEN(MCF),ISMOD(MCF),IBMSN(MCF,MSEN,4),
     &TAUXSN(MCF,MSEN)
      common/btime/btimep,btimef

      common/dllerr/dllsubr,dllmesg
      DIMENSION VAL(MTABC+2)
      character dllsubr*12,dllmesg*124,outs
      logical dll

C Check if in dll mode.
      call isadll(dll)
      DO 28 J=1,NSEN(ICF)
        IF(IBMSN(ICF,J,1).EQ.0.OR.IBMSN(ICF,J,1).EQ.IICOMP)THEN
C i.e. if sensor located in current zone
           TAUXSN(ICF,J)=TNP
        ELSEIF(IBMSN(ICF,J,1).GT.0.AND.IBMSN(ICF,J,1).NE.IICOMP)THEN
           IZ=IBMSN(ICF,J,1)
           IS=IBMSN(ICF,J,2)
           IN=IBMSN(ICF,J,3)
           IF(IN.EQ.0.AND.IS.EQ.0)TAUXSN(ICF,J)=TFA(IZ)
           IF(IN.EQ.0.AND.IS.GT.0)TAUXSN(ICF,J)=TFS(IZ,IS)
           IF(IN.GT.0.AND.IS.GT.0)TAUXSN(ICF,J)=TFC(IZ,IS,IN)
        ELSE IF(IBMSN(ICF,J,1).EQ.-1) THEN

C Sensor measures plant node state-space variable.
C Which is obviously not allowed in case of bld.
          write(outs,'(a)')
     &   'CFMVAR: simulator does not support sensing plant node state.'
          if(dll)then
            dllsubr='CFMVAR'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            call epwait
            call epagend
            STOP
          endif
C Sensor measures a mix of zone air temp & MRT
        ELSEIF(IBMSN(ICF,J,1).EQ.-2)THEN
           IZ=IBMSN(ICF,J,2)
           CALL MZMIXT(IZ,TMRT,TCTL)
           TAUXSN(ICF,J)=TCTL
        ELSEIF(IBMSN(ICF,J,1).EQ.-3)THEN

C Sensor measures ambient conditions
C 0 - Dry bulb temp
           IF(IBMSN(ICF,J,2).EQ.0)THEN
              TAUXSN(ICF,J)=TF
C 1 = Sol-air temp
           ELSEIF(IBMSN(ICF,J,2).EQ.1)THEN
              TAUXSN(ICF,J)=SOLAIR(TF,QFF,QDF)   
C 2 = wind velocity 
           ELSEIF(IBMSN(ICF,J,2).EQ.2)THEN
              TAUXSN(ICF,J)=VF 
C 3 = wind direction
           ELSEIF(IBMSN(ICF,J,2).EQ.3)THEN
              TAUXSN(ICF,J)=DF
C 4 = diffuse horizontal radiation
           ELSEIF(IBMSN(ICF,J,2).EQ.4)THEN
              TAUXSN(ICF,J)=QFF
C 5 = direct normal radiation
           ELSEIF(IBMSN(ICF,J,2).EQ.5)THEN
              TAUXSN(ICF,J)=QDF
C 6 = relative humidity
           ELSEIF(IBMSN(ICF,J,2).EQ.6)THEN
              TAUXSN(ICF,J)=HF
           ENDIF
        ELSEIF(IBMSN(ICF,J,1).EQ.-4)THEN

C Sensing mass flow component variable.          
C Which is obviously not allowed in case of bld.
           write(outs,'(a)')
     & 'CFMVAR: simulator does not support sensing mass flow variable.'
          if(dll)then
            dllsubr='CFMVAR'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            call epwait
            call epagend
            STOP
          endif
        ELSEIF(IBMSN(ICF,J,1).EQ.-5)THEN

C Setpoint to be read from an external file or from temporal
C database.
          if(ISETPTT(ICF).ne.0)then
            itdi=ISETPTT(ICF)
            IFOC=itdi
            CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
            TCTL=VAL(ISD)
            TAUXSN(ICF,J)=TCTL 

C Debug.
C           write(6,*)'CFMVAR:tdf ctl data @',btimef,' is item',itdi,
C     &       ' column ',isd,' value ',tctl,' for ctl loop ',icf

          else
            write(outs,'(a)')
     &      'CFMVAR: ISETPTT does not point to a valid temporal item'
            if(dll)then
              dllsubr='CFMVAR'
              dllmesg=outs
              ier=2
              return
            else
              call edisp(iuout,outs)
              call epwait
              call epagend
              STOP
            endif
          endif
        ELSEIF(IBMSN(ICF,J,1).EQ.-6)THEN
          write(outs,'(a)')
     &    'CFMVAR: version does not support -6 type AUXILIARY sensor.'
          if(dll)then
            dllsubr='CFMVAR'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            call epwait
            call epagend
            STOP
          endif
        ENDIF
28    CONTINUE

      RETURN
      END

C ******************** BCL00

C This routine solves the air temperature/plant capacity
C (perhaps carried through) equation (B1*T+B2*Q=B3) in
C terms of the prevailing control function information
C and to obtain the desired mixed node temperature in the curent zone.
C To do this the mixed temperature is computed on the basis of
C the present time row surface and air point temperatures.

      SUBROUTINE BCL00
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)

      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/BTIME/BTIMEP,BTIMEF

      character outs*124

      B1=BB1
      B2=BB2
      B3=BB3
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=
     &IBAN(ICF,2)
      TFIN=TBCPS(ICF,IDTYP,IPER)
      IF(IPER.EQ.NBCDP(ICF,IDTYP))TFIN=24.
      CONV=FLOAT(IBSN(ICF,3))/100.

C Make decision based on imposed control philosophy.
      Q=0.
      TFUT=TNP
      QFUT=0.
      IF(NSINC.EQ.1)GOTO 6
      ITER1=0
      NDIV=2
      GOTO 7
    6 QFUT=0.
      CALL MZBACK(Q)
      B1=BB1
      B2=BB2
      B3=BB3

C Trace output ?
 9993 IF(ITC.LE.0.OR.NSINC.LT.ITC)GOTO 9999
      IF(ITRACE(30).EQ.0.OR.NSINC.GT.ITCF.OR.IZNTRC(IICOMP).NE.1)
     &GOTO 9999
      write(outs,'(A,I4)')' Subroutine BCL00     Trace output',ICNT
      call edisp(itu,outs)
      ICNT=ICNT+1
      CALL DAYCLK(IDYP,BTIMEF,ITU)

C Output control temperature and plant injection/extraction
C values.
      call edisp(itu,' ')
      write(outs,'(A,I4)')    ' Zone',IICOMP
      call edisp(itu,outs)
      call edisp(itu,' ')
      write(outs,'(A,F20.3)') ' TFUT = ',TFUT
      call edisp(itu,outs)
      write(outs,'(A,F20.3)') ' QFUT = ',QFUT
      call edisp(itu,outs)
 9999 RETURN

C  Compute future time-row 'mixed' temperature but
C  assuming present time-row surface temperatures.
    7 CALL MZMIXT(IICOMP,TMRT,TMIX)
      ITYPE=IBCLAW(ICF,IDTYP,IPER)
      GOTO(8,9,11,12),ITYPE
      GOTO 99

C Thermostatic control period: is temperature within
C range.

C Control data is established from the BMISCD array.
    8 TL=BMISCD(ICF,IDTYP,IPER,6)
      TU=BMISCD(ICF,IDTYP,IPER,7)
      QH=BMISCD(ICF,IDTYP,IPER,2)
      QHL=BMISCD(ICF,IDTYP,IPER,3)
      QC=-BMISCD(ICF,IDTYP,IPER,4)
      QCL=-BMISCD(ICF,IDTYP,IPER,5)
      IF(TMIX.LE.TU.AND.TMIX.GE.TL)GOTO 81

C Is 'mixed' temperature too high ?
      IF(TMIX.GT.TU)GOTO 13

C Temperature too low - heat to lower limit! .....
      IPLT=1
      QMX=QH
      QMN=QHL

      TMIX=TL
      T=(TMIX-TMRT*(1.-CONV))/CONV

C ..... and compute required capacity .....
   17 Q=(B3-B1*T)/B2

C CONVERT FROM /M2 IF NECESSARY.
      QQ=Q
      IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)

C ..... is this available ?
      IF(QQ.LE.QH.AND.QQ.GE.QC)GOTO 14
      IF(QQ.GT.QH)GOTO 15

C Required cooling capacity not available hence set to
C available amount.
      Q=QC
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)

C ..... and compute air temperature to result.
   16 T=(B3-B2*Q)/B1
      GOTO 14

C Required heating capacity not available hence set to
C available amount.
   15 Q=QH
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)

C ..... and compute air temperature to result.
      GOTO 16

C Temperature too high - cool to upper limit!
   13 IPLT=2
      QMX=QC
      QMN=QCL

      TMIX=TU
      T=(TMIX-TMRT*(1.-CONV))/CONV
      GOTO 17

C Establish actual future time-row
C 'mixed' temperature .....
   14 TFUT=T

C ..... by executing back substitution operation
C of reduced zone matrix.
   81 CALL MZBACK(Q)
      B1=BB1
      B2=BB2
      B3=BB3
      CALL MZMIXT(IICOMP,TMRT,TMIX)

C Is future time-row 'mixed' value acceptable ?
      TUP=TU+0.05
      TLOW=TL-0.05
      IF(ITYPE.NE.3)GOTO 211
      JII=IPER+1
      IF(IPER.EQ.NBCDP(ICF,IDTYP))JII=1
      TUP=BMISCD(ICF,IDTYP,JII,7)+0.05
      TLOW=BMISCD(ICF,IDTYP,JII,6)-0.05
      IF(TMIX.LE.TUP.AND.TMIX.GE.TLOW)GOTO 18

C  Determine target temperature.
      IF(TMIX.GT.TUP)TMP=BMISCD(ICF,IDTYP,JII,7)
      IF(TMIX.LT.TLOW)TMP=BMISCD(ICF,IDTYP,JII,6)
      TARG=TMIX+(TMP-TMIX)/(ABS(TFIN-BTIMEF)+1.)
      TUP=TARG+0.05
      TLOW=TARG-0.05
  211 IF(TMIX.GT.TUP)GOTO 21
      IF(TMIX.LT.TLOW)GOTO 22
      GOTO 18
   21 IF(Q.LE.QC)THEN
         QFUT=QC
         Q=0.
         IF(IBAN(ICF,1).NE.-2)Q=QC
         CALL MZBACK(Q)
         GOTO 18
      ENDIF
      TDIFF=ABS(TUP-TMIX)
      IF(TDIFF.LE.0.1)TDIFF=0.1
      T=T-(TDIFF/2.0)
      ITER1=ITER1+1
      IF(ITER1.EQ.1)INORP=1
      IF(INORP.LT.0)INORP=(INORP+1)*(-1)
      GOTO 23
   22 IF(Q.GE.QH)THEN
         QFUT=QH
         Q=0.
         IF(IBAN(ICF,1).NE.-2)Q=QH
         CALL MZBACK(Q)
         GOTO 18
      ENDIF
      TDIFF=ABS(TMIX-TLOW)
      IF(TDIFF.LE.0.1)TDIFF=0.1
      T=T+(TDIFF/2.0)
      ITER1=ITER1+1
      IF(ITER1.EQ.1)INORP=-1
      IF(INORP.GT.0)INORP=(INORP-1)*(-1)
   23 IF(ITER1.GT.200)GOTO 24
      IF(ABS(INORP).GT.10.AND.NDIV.EQ.4)GOTO 25
      IF(INORP.LT.10)GOTO 17
      INORP=INORP/10
      NDIV=NDIV+1
      GOTO 17
   24 call edisp(iuout,' BCL00 error: iteration limit exceeded.')
      write(outs,'(A,F7.2,A,I4)')' Time ',BTIMEF,' Year Day ',IDYP
      call edisp(iuout,outs)
      call epwait
      call epagend
      STOP
   25 call edisp(iuout,
     &  ' BCL00 error: +ve/-ve iteration limit exceeded.')
      write(outs,'(A,F7.2,A,I4)')' Time ',BTIMEF,' Year Day ',IDYP
      call edisp(iuout,outs)
      call epwait
      call epagend
      STOP

   18 TCONT=T
      QFUT=Q
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)
      GOTO 9993

C Floating temperature period.
    9 GOTO 6

C Pre-heating/cooling period: first determine if no-plant
C temperature is within range of subsequent period control temperatures.
   11 TU=BMISCD(ICF,IDTYP,IPER,7)
      TL=BMISCD(ICF,IDTYP,IPER,6)
      QH=BMISCD(ICF,IDTYP,IPER,2)
      QHL=BMISCD(ICF,IDTYP,IPER,3)
      QC=-BMISCD(ICF,IDTYP,IPER,4)
      QCL=-BMISCD(ICF,IDTYP,IPER,5)
      JJ=IPER+1
      IF(IPER.EQ.NBCDP(ICF,IDTYP))JJ=1
      IF(TMIX.LE.BMISCD(ICF,IDTYP,JJ,7).AND.TMIX.GE.
     &BMISCD(ICF,IDTYP,JJ,6))GOTO 81

C Determine target air temperature for current time step.
      IF(TMIX.LT.BMISCD(ICF,IDTYP,JJ,6))THEN
         TMIX=BMISCD(ICF,IDTYP,JJ,6)

         IPLT=1
         QMX=QH
         QMN=QHL
      ENDIF
      IF(TMIX.GT.BMISCD(ICF,IDTYP,JJ,7))THEN
         TMIX=BMISCD(ICF,IDTYP,JJ,7)

         IPLT=2
         QMX=QC
         QMN=QCL
      ENDIF
      TARG=(TMIX-TMRT*(1.-CONV))/CONV
      T=TFA(IICOMP)+(TARG-TFA(IICOMP))/(ABS(TFIN-BTIMEF)+1.)
      GOTO 17

C Fixed heat injection/extraction period: is temperature
C within control range ?
   12 TU=BMISCD(ICF,IDTYP,IPER,5)
      TL=BMISCD(ICF,IDTYP,IPER,4)
      QH=BMISCD(ICF,IDTYP,IPER,2)
      QC=-BMISCD(ICF,IDTYP,IPER,3)
      IF(TMIX.LE.TU.AND.TMIX.GE.TL)GOTO 81

C Is temperature too high ?
      IF(TMIX.GT.TU)GOTO 19

C Temperature too low: inject fixed heating.
      IPLT=1
      QMX=QH
      QMN=0.

      Q=QH
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      TFUT=(B3-B2*Q)/B1
      T=TFUT
      CALL MZBACK(Q)
      B1=BB1
      B2=BB2
      B3=BB3
      GOTO 18

C Temperature too high: extract fixed cooling.
   19 IPLT=2
      QMX=QC
      QMN=0.

      Q=QC
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      TFUT=(B3-B2*Q)/B1
      T=TFUT
      CALL MZBACK(Q)
      B1=BB1
      B2=BB2
      B3=BB3
      GOTO 18

   99 write(outs,98)ITYPE
   98 format(' BCL00: building control law',I3,' does not yet')
      call edisp(iuout,outs)
      call edisp(iuout,
     &  ' exist but has been referenced.  This is a fatal error.')
      call epwait
      call epagend
      STOP
      END

C ******************** BCL01
C A basic controller.
      SUBROUTINE BCL01(IER)

#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)

C Common for relative humidity control (ideal) and dew point relative humidity
C control.
      COMMON/IDRHCTL1/RHSPTU(MCOM),RHSPTL(MCOM),RHHC(MCOM),
     &RHDC(MCOM),IRHCT(MCOM)
      COMMON/ATPRES/PATMOS
      COMMON/FVALG/GFA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/SLAVE1/QHB(MCF),QCB(MCF),Qmst(MCF)

C Description of zone control action; these data are used
C in H3Kreports to determine heating, cooling loads and 
C to evaluate passive solar design performance.
      common/H3KReportsControl/bZoneHeated,   bZoneCooled,
     &                         fHeatSetpoint, fCoolSetpoint

C Flags indicating zone is heated, cooled.
      logical bZoneHeated(MCOM), bZoneCooled(MCOM)
C Heating and cooling setpoint (oC)
      real fHeatSetpoint(MCOM), fCoolSetpoint(MCOM)

      common/dllerr/dllsubr,dllmesg

      character dllsubr*12,dllmesg*124,outs*124
      logical dll,CLOSERH
      LOGICAL AreWindowsOpen

      COMMON/WindowControl/
     &     fWindowSetpoint,
     &     fACSetPoint, 
     &     bWindowControlFlag,
     &     iWindowControlZone,
     &     bWindowsAreOpen,
     &     fDeltaTInttoExt,
     &     fWindowLowSetpoint
          

C     Exterior Temperature at which and below the windows could open.  
      REAL fWindowSetpoint

C     The AC Temperature setpoint. 
      REAL fACSetPoint

C     The zone at which the temperature is controlled against.
      INTEGER iWindowControlZone

C     The flag which indicates the window feature is active.
      LOGICAL bWindowControlFlag

C     Common block variable to tell other routines that the windows are open. 
C     (This saves processor time)
      LOGICAL bWindowsAreOpen

C     The DeltaT that is used to detemine if cooling can be used from the outside.
C     Thus Windows could be open when DeltaT >= Zone Temp - ExteriorTemp
      REAL  fDeltaTInttoExt

C     The temperature at which and below that the windows remain closed.
C     This prevents the zone becoming too cold. 
      REAL fWindowLowSetpoint

C Check to see if running in dll mode.
      call isadll(dll)

C Fatal error test.
      IF(BMISCD(ICF,IDTYP,IPER,1).LT.6.)GOTO 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        if (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).eq.0) then
          write(outs,'(a,i2)') 
     &      'Entering subroutine BCL01 sensing in zone ',
     &      IBSN(ICF,1)
          call edisp(itu,outs)
        elseif (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).GT.0) then
          write(outs,'(a,i2,a,i2)') 
     &      'Entering subroutine BCL01 sensing in zone ',
     &      IBSN(ICF,1),' and actuating in zone ',IBAN(ICF,1)
          call edisp(itu,outs)
        else
          call edisp(itu,' Entering subroutine BCL01')
        endif
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

      QFUT=0.
      TFUT=TNP

C Added for slave controller...
      Qmst(ICF)=QFUT

C Determine sensed temperature.
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C QHM = max heating capacity (W)
C QHN = min heating capacity (W)
C QCM = max cooling capacity (W)
C QCN = min cooling capacity (W)
C HSP = heating set point temp. (deg.C)
C CSP = cooling set point temp. (deg.C)

C The variable keeping track of the cooling/heating capacities of the 
C hvac system for ideal hvac modeling is set to zero at the beginning 
C of the simulation. If this capacity is found to be greater than zero, 
C then the cooling/heating capacity of the associated control law for 
C the zone law needs to be updated. The variable cooling/heating capacity 
C is set at the approprite HVAC equipment simulation routine.

C Set maximum heating capacity:
      if(cont_fun_heat_cap(IICOMP).gt.0 ) then
        QHM=cont_fun_heat_cap(IICOMP)
      else
        QHM=BMISCD(ICF,IDTYP,IPER,2)       
      endif

C Set minimum heating capacity
      QHN=BMISCD(ICF,IDTYP,IPER,3)

C Set maximum cooling capacity:
      if(cont_fun_cool_cap(IICOMP).gt.0 ) then

C     if the windows are open
C     ensure that the AC is not used. Note: if window model not active the result of 
C     AreWindowsOpen()  will be false. 
C     Also the window control is only used in tandem with a cooling 
C     stratagy. 
         if ( AreWindowsOpen() 
     &        .and. (BMISCD(ICF,IDTYP,IPER,4) .gt. 0.)  ) then
            QCM = 0.
            QCN = 0.
         else
            QCM = - cont_fun_cool_cap(IICOMP)
C     Ensure windows are closed.
            bWindowsAreOpen = .false.
         endif
      else
         QCM = -BMISCD(ICF,IDTYP,IPER,4)
      endif

C Set minimum cooling capacity
      QCN=-BMISCD(ICF,IDTYP,IPER,5)

      HSP=BMISCD(ICF,IDTYP,IPER,6)
      CSP=BMISCD(ICF,IDTYP,IPER,7)

C Save heating/cooling setpoints
      fHeatSetpoint(iicomp) = HSP
      fCoolSetpoint(iicomp) = CSP

C Set heating flags to true...
      bZoneHeated(iicomp) = .true.
      bZoneCooled(iicomp) = .true.

C Setpoints must be between 10oC and 30oC for passive solar computations
      if ( fHeatSetpoint(iicomp).lt.10.0 .or.
     &     fHeatSetpoint(iicomp).gt.30.0 ) bZoneHeated(iicomp) = .false.
      if ( fCoolSetpoint(iicomp).lt.10.0 .or.
     &     fCoolSetpoint(iicomp).gt.30.0 ) bZoneCooled(iicomp) = .false.

C Capacity must be greater than 1 W for passive solar computations
       if (  QHM .lt. 1.0 )  bZoneHeated(iicomp) = .false.
       if (  ABS(QCM) .lt. 1.0 )  bZoneCooled(iicomp) = .false.

C Following lines added to set master zone values for use in slave controller.
C Each control of type 2 will have values set in case they are referenced
C as a master control.
      QHB(ICF)=QHM
      QCB(ICF)=QCM

C Determine if a RH is to be controlled. If this is the case store the set point
C for the zone and set the RH control flag. The control action takes place in 
C MZVAPC, where the zone humidity and moisture content is calculated. 
      IRHCT(IICOMP)=0
      IF(INT(BMISCD(ICF,IDTYP,IPER,1)).GT.7)THEN

C Upper relative humidity set point (%).
        RHSPTU(IICOMP)=BMISCD(ICF,IDTYP,IPER,8)

C Lower relative humidity set point (%).
        RHSPTL(IICOMP)=BMISCD(ICF,IDTYP,IPER,9)

C Humidification capacity limit (W)
        RHHC(IICOMP)=BMISCD(ICF,IDTYP,IPER,10)/1000.

C Dehumidification capacity limit (W)
        RHDC(IICOMP)=-BMISCD(ICF,IDTYP,IPER,11)/1000.

C Relative humidity control method 1-moisture injection/extraction 
C 2-heating and cooling. Control method 1 requires the setting of the
C IRHCT flag. 
        IRHCTT=INT(BMISCD(ICF,IDTYP,IPER,12))
        IF(IRHCTT.EQ.1) THEN
          IRHCT(IICOMP)=1
        ELSEIF(IRHCTT.EQ.2)THEN
c          WRITE(*,*)' '
c          WRITE(*,*)'Temperature control of RH'
C If temperature control for alteration of RH is to be used then
C check that the zone RH is in limits. If not then alter the
C heating and cooling set points appropriately. 
          RHC=PCRH2(TFA(IICOMP),GFA(IICOMP),PATMOS)
c          WRITE(*,*)'Current status ',TFA(IICOMP),GFA(IICOMP)
c          WRITE(*,*)'Current RH',RHC
C If moisture content above that for the target RH then implement
C cooling (change cooling and heating set points)
          IF(RHC.GT.RHSPTU(IICOMP))THEN
c            WRITE(*,*)'Target RH',RHSPTU(IICOMP)       
c            WRITE(*,*)'RH too high ... heating'
C Iteratively calculate the temperature needed to hit the target RH
C Initiate the iterative temperature
c            TITM=TFA(IICOMP)
             TITM=TCTL
        
C Set the target RH 
            RHT=RHSPTU(IICOMP)

C Calculate the RH at this temperature
            ITER=1
  404       RHI=PCRH2(TITM,GFA(IICOMP),PATMOS)
c            WRITE(*,*)'New Iteration RH ...',RHI             
            CLOSERH=.TRUE.      
            CALL ECLOSE(RHI,RHT,0.1,CLOSERH)
            IF(.NOT.CLOSERH)THEN
              TITM=TITM+((RHI-RHT)*0.005)
              ITER=ITER+1
c              WRITE(*,*)'New Iteration temperature ...',TITM  
            ENDIF
 
            IF(.NOT.CLOSERH.AND.RHI.GE.0.0.AND.RHI.LE.100.0.AND.
     &          ITER.LT.1000)GOTO 404
  
            IF(CLOSERH.AND.TITM.GT.0..AND.TITM.LT.100.)THEN
              CSP=100.
              HSP=TITM
c              WRITE(*,*)'New heating and cooling set point',HSP,CSP    
            ENDIF      
      
          ELSEIF(RHC.LT.RHSPTL(IICOMP))THEN
c            WRITE(*,*)'Target RH',RHSPTL(IICOMP)       
c            WRITE(*,*)'RH too low ... cooling'
C Iteratively calculate the temperature needed to hit the target RH
C Initialte the iterative temperature
            TITM=TFA(IICOMP)

C Set the target RH 
            RHT=RHSPTL(IICOMP)

C Calculate the RH at the iterative temperature
            ITER=1
  405       RHI=PCRH2(TITM,GFA(IICOMP),PATMOS)
c            WRITE(*,*)'New Iteration RH ...',RHI     
            CLOSERH=.TRUE.      
            CALL ECLOSE(RHI,RHT,0.1,CLOSERH)
            IF(.NOT.CLOSERH)THEN
              TITM=TITM-((RHT-RHI)*0.005)
              ITER=ITER+1
c              WRITE(*,*)'New Iteration temperature ...',TITM  
            ENDIF 
            IF(.NOT.CLOSERH.AND.RHI.GE.0..AND.RHI.LE.100..AND.
     &ITER.LT.1000)GOTO 405  
            IF(CLOSERH.AND.TITM.GT.0..AND.TITM.LT.100.)THEN
              CSP=TITM
              HSP=0.  
c              WRITE(*,*)'New heating and cooling set point',HSP,CSP         
            ENDIF  
          ENDIF
        ENDIF
      ELSE
        RHDC(IICOMP)=0.0
        RHHC(IICOMP)=0.0
        RHSPTU(IICOMP)=0.0
        RHSPTL(IICOMP)=0.0
        IRHCT(IICOMP)=0
      ENDIF

      IF(TCTL.LE.CSP.AND.TCTL.GE.HSP)RETURN
      
c      WRITE(*,*)'Controlling zone to...',HSP,CSP,TCTL,TNP    
C Since sensed temperature is not within range, attempt to control
C THIS zone's temperature.
      TCTL=TNP

C Inserted for the EU Fog Project: if heating or cooling set point is set to
C -1000., the controller will use the dew point temperature as the set point.
C This is an alternative to humidification in that the zone RH is  maintained
C at (approx.) 100% RH.
      IF(int(CSP).EQ.-1000.OR.int(HSP).EQ.-1000)THEN
        
C Set point is the dew point temperature of the zone. 
        PATMOS=1013.25
        HSP=DEWPT(GFA(IICOMP),PATMOS)
        CSP=DEWPT(GFA(IICOMP),PATMOS)
        IF(HSP.LT.0.0)HSP=0.0
        IF(CSP.LT.0.0)CSP=0.0
      ENDIF

C Is temp. too high ?
      IF(TCTL.GT.CSP)GOTO 1
      IPLT=1
      QMX=QHM
      QMN=QHN
      TCONT=HSP

C Too low, heat to HSP.
      Q=(BB3-BB1*HSP)/BB2

C Convert from (/m^2) if necessary.
      QQ=Q
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)

C Is this capacity available ?
      IF(QQ.LE.QHM.AND.QQ.GE.QHN)GOTO 2
      IF(QQ.GT.QHM)GOTO 3
      Q=QHN
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      GOTO 2
    3 Q=QHM
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      GOTO 2

C Temp. too high, cool to CSP.
    1 Q=(BB3-BB1*CSP)/BB2
      IPLT=2
      QMX=QCM
      QMN=QCN
      TCONT=CSP

C Convert from (/m^2) if necessary.
      QQ=Q
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)

C Is this available ?
      IF(QQ.GE.QCM.AND.QQ.LE.QCN)GOTO 2
      IF(QQ.LT.QCM)GOTO 5
      Q=QCN
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      GOTO 2
    5 Q=QCM
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)

    2 TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
c      WRITE(*,*)'Heating/cooling flux and set points',QFUT,HSP,CSP
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Set QFUT for use by slave controller
      Qmst(ICF)=QFUT
      
C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F10.3,A,F10.3,a)') ' Qfuture = ',QFUT,
     &     ' Tfuture = ',TFUT,' (Q master ',Qmst(ICF),')'
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL01')
      END IF

      RETURN
   99 write(outs,'(a)')' BCL01: data incomplete.'
      if(dll)then
        dllsubr='BCL01'
        dllmesg=outs
        ier=2
        return
      else
        call edisp(iuout,outs)
        call epwait
        call epagend
        STOP
      endif
      END

C ******************** BCL02

C A free-float controller.

      SUBROUTINE BCL02
#include "building.h"
#include "control.h"      

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/SLAVE1/QHB(MCF),QCB(MCF),Qmst(MCF)
      
      character outs*124

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        call edisp(itu,' Entering subroutine BCL02')
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

      QFUT=0.
      TFUT=TNP
      
C Inserted for slave controller
      Qmst(ICF)=QFUT

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F10.3)') ' Qfuture = ',QFUT,
     &                                   ' Tfuture = ',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL02')
      END IF

      RETURN
      END

C ******************** BCL03

C An exponential pre-heat (or cool) controller.

      SUBROUTINE BCL03
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/BTIME/BTIMEP,BTIMEF
      
      character outs*124

C Fatal error test.
      IF(BMISCD(ICF,IDTYP,IPER,1).LT.6.)GOTO 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        if (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).eq.0) then
          write(outs,'(a,i2)') 
     &      'Entering subroutine BCL03 sensing in zone ',
     &      IBSN(ICF,1)
          call edisp(itu,outs)
        elseif (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).GT.0) then
          write(outs,'(a,i2,a,i2)') 
     &      'Entering subroutine BCL03 sensing in zone ',
     &      IBSN(ICF,1),' and actuating in zone ',IBAN(ICF,1)
          call edisp(itu,outs)
        else
          call edisp(itu,' Entering subroutine BCL03')
        endif
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

      QFUT=0.
      TFUT=TNP

C Determine sensed temperature.
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C QHM = max heating capacity (W)
C QHN = min heating capacity (W)
C QCM = max cooling capacity (W)
C QCN = min cooling capacity (W)
C HSP = heating set point temp. (deg.C)
C CSP = cooling set point temp. (deg.C)

      QHM=BMISCD(ICF,IDTYP,IPER,2)
      QHN=BMISCD(ICF,IDTYP,IPER,3)
      QCM=-BMISCD(ICF,IDTYP,IPER,4)
      QCN=-BMISCD(ICF,IDTYP,IPER,5)
      HSP=BMISCD(ICF,IDTYP,IPER,6)
      CSP=BMISCD(ICF,IDTYP,IPER,7)

      JJ=IPER+1
      IF(IPER.EQ.NBCDP(ICF,IDTYP))JJ=1
      TNEXT=TNP

C Test to establish if pre-heating or pre-cooling required ?
      IF(TCTL.LE.CSP.AND.TCTL.GE.HSP)RETURN

C Since sensed temperature is not within range, attempt to control
C THIS zone's temperature.
      TCTL=TNP

C Establish desired temperature - assume heating required and correct
C if cooling required.
      TNEXT=HSP
      IF(TCTL.GT.CSP)TNEXT=CSP

C Determine target temperature for current time-step.
      TCTL=TFA(IICOMP)+(TNEXT-TFA(IICOMP))
     &/(ABS(TBCPS(ICF,IDTYP,JJ)-BTIMEF)+1.)

C Is temp. too high ?
      IF(TCTL.GT.CSP)GOTO 1

C Too low, heat to TCTL.
      Q=(BB3-BB1*TCTL)/BB2
      IPLT=1
      QMX=QHM
      QMN=QHN
      TCONT=TCTL

C Convert from (/m^2) if necessary.
    4 QQ=Q
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)

C Is this capacity available ?
      IF(QQ.LE.QHM.AND.QQ.GE.QHN)GOTO 2
      IF(QQ.GT.QHM)GOTO 3
      Q=QHN
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      GOTO 4
    3 Q=QHM
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      GOTO 4

C Temp. too high, cool to TCTL.
    1 Q=(BB3-BB1*TCTL)/BB2
      IPLT=2
      QMX=QCM
      QMN=QCN
      TCONT=TCTL

C Convert from (/m^2) if necessary.
    6 QQ=Q
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)

C Is this available ?
      IF(QQ.GE.QCM.AND.QQ.LE.QCN)GOTO 2
      IF(QQ.LT.QCM)GOTO 5
      Q=QCN
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      GOTO 6
    5 Q=QCM
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      GOTO 6
    2 TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F10.3)') ' Qfuture = ',QFUT,
     &                                   ' Tfuture = ',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL03')
      END IF

      RETURN
   99 call edisp(iuout,' BCL03: data incomplete.')
      call epwait
      call epagend
      STOP
      END

C ******************** BCL04
C A fixed heat injection/extraction controller.

      SUBROUTINE BCL04
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      CHARACTER OUTS*124

C Fatal error test.
      IF(BMISCD(ICF,IDTYP,IPER,1).LT.4.0)GOTO 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        if (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).eq.0) then
          write(outs,'(a,i2)') 
     &      'Entering subroutine BCL04 sensing in zone ',
     &      IBSN(ICF,1)
          call edisp(itu,outs)
        elseif (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).GT.0) then
          write(outs,'(a,i2,a,i2)') 
     &      'Entering subroutine BCL04 sensing in zone ',
     &      IBSN(ICF,1),' and actuating in zone ',IBAN(ICF,1)
          call edisp(itu,outs)
        else
          call edisp(itu,' Entering subroutine BCL04')
        endif
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

      QFUT=0.
      TFUT=TNP

C Determine sensed temperature.
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C QH  = heating capacity (W)
C QC  = cooling capacity (W)
C HSP = heating set point temp. (deg.C)
C CSP = cooling set point temp. (deg.C)

      QH=BMISCD(ICF,IDTYP,IPER,2)
      QC=-BMISCD(ICF,IDTYP,IPER,3)
      HSP=BMISCD(ICF,IDTYP,IPER,4)
      CSP=BMISCD(ICF,IDTYP,IPER,5)
      IF(TCTL.LE.CSP.AND.TCTL.GE.HSP)RETURN

C Is temp. too high ?
      IF(TCTL.GT.CSP)GOTO 1

C Too low, heat.
      Q=QH
      IPLT=1
      QMX=QH
      QMN=QH
      TCONT=HSP
      GOTO 2

C Temp. too high, cool.
    1 Q=QC
      IPLT=2
      QMX=QC
      QMN=QC
      TCONT=CSP

C Convert from (/m^2) if necessary.
    2 ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F10.3)') ' Qfuture = ',QFUT,
     &                                   ' Tfuture = ',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL04')
      END IF

      RETURN
   99 call edisp(iuout,' BCL04: data incomplete.')
      call epwait
      call epagend
      STOP
      END

C ******************** BCL05

C Proportional+intergral+derivative (P or P+I or P+D or PID) control action
C suitable for use with controller types 0,35,36,37,38,39.

      SUBROUTINE BCL05
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/BLGPID/BPID(MCF,6),NSINCR
      
      character outs*124

C Fatal error test:
      ICTYP=IBCTYP(ICF,IDTYP,IPER)
      IF(ICTYP.NE.0.AND.(ICTYP.LT.35.OR.ICTYP.GT.39))GOTO 97

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        if (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).eq.0) then
          write(outs,'(a,i2)') 
     &      'Entering subroutine BCL05 sensing in zone ',
     &      IBSN(ICF,1)
          call edisp(itu,outs)
        elseif (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).GT.0) then
          write(outs,'(a,i2,a,i2)') 
     &      'Entering subroutine BCL05 sensing in zone ',
     &      IBSN(ICF,1),' and actuating in zone ',IBAN(ICF,1)
          call edisp(itu,outs)
        else
          call edisp(itu,' Entering subroutine BCL05')
        endif
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

      QFUT=0.0
      TFUT=TNP

C IMO  =  1: proportional control mode
C  "   =  2: P+I control mode
C  "   =  3: P+D control mode
C  "   =  4: P+I+D control mode
c QHMX =  max heating capacity (W)
c QHMN =  min heating capacity (W),
c HSP  =  heating set point temp (deg.C),
c HTR  =  heating throttling range or proportional band  (deg.C),
c QCMX =  max cooling capacity (W),
c QCMN =  min cooling capacity (W),
c CSP  =  cooling set point temp. (deg.C),
c CTR  =  cooling throttling range or proportional band  (deg.C),
c TI   =  integral (reset) action time (secs),
c TD   =  derivative (rate) action time (secs).

c It is assumed that the set points are at the mid-points
c of the throttling ranges.

      IMO=INT(BMISCD(ICF,IDTYP,IPER,2))
      QHMX=BMISCD(ICF,IDTYP,IPER,3)
      QHMN=BMISCD(ICF,IDTYP,IPER,4)
      HSP=BMISCD(ICF,IDTYP,IPER,5)
      HTR=BMISCD(ICF,IDTYP,IPER,6)
      QCMX=-BMISCD(ICF,IDTYP,IPER,7)
      QCMN=-BMISCD(ICF,IDTYP,IPER,8)
      CSP=BMISCD(ICF,IDTYP,IPER,9)
      CTR=BMISCD(ICF,IDTYP,IPER,10)   
      IF(IMO.EQ.2)TI=INT(BMISCD(ICF,IDTYP,IPER,11))
      IF(IMO.EQ.3)TD=INT(BMISCD(ICF,IDTYP,IPER,11))
      IF(IMO.EQ.4)TI=INT(BMISCD(ICF,IDTYP,IPER,11))
      IF(IMO.EQ.4)TD=INT(BMISCD(ICF,IDTYP,IPER,12))
  
C Fatal error tests:      
      IF(INT(BMISCD(ICF,IDTYP,IPER,2)).LT.1.OR.
     &   INT(BMISCD(ICF,IDTYP,IPER,2)).GT.4)GOTO 99 
     
      IF(INT(BMISCD(ICF,IDTYP,IPER,2)).EQ.1.AND.
     &   INT(BMISCD(ICF,IDTYP,IPER,1)).NE.9)GOTO 981 

      IF(INT(BMISCD(ICF,IDTYP,IPER,2)).EQ.2.AND.
     &   INT(BMISCD(ICF,IDTYP,IPER,1)).NE.10)GOTO 982 
     
      IF(INT(BMISCD(ICF,IDTYP,IPER,2)).EQ.3.AND.
     &   INT(BMISCD(ICF,IDTYP,IPER,1)).NE.10)GOTO 983 

      IF(INT(BMISCD(ICF,IDTYP,IPER,2)).EQ.4.AND.
     &   INT(BMISCD(ICF,IDTYP,IPER,1)).NE.11)GOTO 984 
     
      IF((IMO.EQ.2.OR.IMO.EQ.4).AND.TI.LT.1.0)GOTO 96

C Set setpoint to be at the mid range of the proportional band
c HTU = upper heating temperature limit
c HTL = lower heating temperature limit
      HTU=HSP+(HTR/2.0)
      HTL=HSP-(HTR/2.0)

C Set setpoint to be at the mid range of the proportional band
c CTU = upper cooling temperature limit
c CTL = lower cooling temperature limit
      CTU=CSP+(CTR/2.0)
      CTL=CSP-(CTR/2.0)

C Set time-step increment required for integral and derivative action,
      TDELTA=3600/NTSTEP

C << consider in the future to revise code to only restart the
C << integral action when the new setpont is attained.
C << especially to reduce overshoot so the current code does
C << not match what happens in physical devices.

      IF(NSINC.EQ.(NSINCR+1))THEN

C If we are still within a control period (no setpoint changes) then
C use the following logic:
C Update variables used in recursive algorithms.
C Save values in common bpid(icf,?)
C bpid(icf,1)= heating error at present time-step,
C bpid(icf,2)= heating error at previous time-step,
C bpid(icf,3)= time-integral of the heating error,
C bpid(icf,4)= cooling error at present time-step,
C bpid(icf,5)= cooling error at previous time-step,
C bpid(icf,6)= time-integral of the cooling error,

C Take the average heating error at the two timesteps.
         BPID(ICF,3)=BPID(ICF,3)
     &+(0.5*(BPID(ICF,2)*TDELTA+BPID(ICF,1)*TDELTA))
         BPID(ICF,2)=BPID(ICF,1)

C Take the average cooling error at the two timesteps.
         BPID(ICF,6)=BPID(ICF,6)
     &+(0.5*(BPID(ICF,5)*TDELTA+BPID(ICF,4)*TDELTA))
         BPID(ICF,5)=BPID(ICF,4)
      ELSE

C If inside first time increment of a new control period re-set the integral error
C term.
         BPID(ICF,3)=0.
         BPID(ICF,2)=BPID(ICF,1)
         BPID(ICF,6)=0.        
         BPID(ICF,5)=BPID(ICF,4)
      ENDIF
      
C Determine sensed temperature.
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C Establish the errors at the present time-step;
C The convention is that a positive error happens when
C you are below the setpoint. 
C Changed from TCTL-HSP 
      BPID(ICF,1)=HSP-TCTL   ! heating error

C Changed from TCTL-CSP
      BPID(ICF,4)=CSP-TCTL   ! cooling error

C Heating cycle;      
      QH=0.0    ! Set flux to zero,
      
C Determine heating flux at set point assuming linear relationship.
      QSPH=(QHMX+QHMN)/2.0

C Determine the heating integral action (P+I or PID),
      IF(IMO.EQ.2.OR.IMO.EQ.4)THEN
        RINTH=(1.0/TI)*(BPID(ICF,3))
      ELSE
        RINTH=0.0
      ENDIF
                
      IF(TCTL.GT.HTU)THEN
        QH=QHMN        
      ELSEIF(TCTL.LT.HTL)THEN
        QH=QHMX    
      ELSEIF(TCTL.GE.HTL.AND.TCTL.LE.HTU)THEN        

C The temperature is within the throttling range (proportional band):
C Determine the gain for heating,
        RKPH=(QHMX-QHMN)/(HTU-HTL)

C Determine the derivative action for htg,
        IF(IMO.EQ.3.OR.IMO.EQ.4)THEN
          RDRVH=(TD/TDELTA)*(BPID(ICF,1)-BPID(ICF,2))
        ELSE
          RDRVH=0.0
        ENDIF

C The heating flux magnitude,
        QH=QSPH+(RKPH*(BPID(ICF,1)+RINTH+RDRVH))
        
      ENDIF
      
C Limit heating flux;
      IF(QH.GT.QHMX)THEN
        QH=QHMX
      ELSEIF(QH.LT.QHMN)THEN
        QH=QHMN
      ENDIF

C Cooling cycle;
C Set flux,
      QC=0.0
      
C Determine clg flux at set point assuming linear relationship.
      QSPC=(QCMX+QCMN)/2.0

C Determine the clg integral action.
C RINTC is the integral term () 
      IF(IMO.EQ.2.OR.IMO.EQ.4)THEN
        RINTC=(TDELTA/TI)*(BPID(ICF,6))
      ELSE
        RINTC=0.0
      ENDIF
                
      IF(TCTL.GT.CTU)THEN
        QC=QCMX
      ELSEIF(TCTL.LT.CTL)THEN
        QC=QCMN    
      ELSEIF(TCTL.GE.CTL.AND.TCTL.LE.CTU)THEN        

C The temperature is within the throttling range (proportional band):
C Determine the gain for clg,
        RKPC=(QCMN-QCMX)/(CTU-CTL)

C Determine the derivative action for clg,
        IF(IMO.EQ.3.OR.IMO.EQ.4)THEN
          RDRVC=(TD/TDELTA)*(BPID(ICF,4)-BPID(ICF,5))
        ELSE
          RDRVC=0.0
        ENDIF
C The cooling flux magnitude,
        QC=QSPC+(RKPC*(BPID(ICF,4)+RINTC+RDRVC))
      
      ENDIF

C Limit cooling flux;
      IF(QC.LT.QCMX)QC=QCMX
      IF(QC.GT.QCMN)QC=QCMN

C Determine net flux input
      Q=QH+QC       
        
C Remember building time-step
      NSINCR=NSINC
      
      IPLT=1
      QMX=QHMX
      QMN=QHMN
      
C Convert from (/m^2) if necessary.
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      TCONT=TFUT      
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F8.3,A,F8.3,A,F8.3,A,F10.3,A,F8.3)') 
     &    ' Qfuture=',QFUT,' Tfuture=',TFUT,' Tctl=',TCTL,
     &    ' TDelta=',TDELTA,' Q ht=',QH,' ht err=',BPID(ICF,1)
         call edisp(itu,outs)
         write(outs,'(A,F8.3,A,F8.3,A,F8.3,A,F10.3,A,F9.3,A,F9.3)') 
     &     ' ht spt=',HSP,' ht stp U=',HTU,' ht sp L= ',HTL,
     &     ' ht gain=',RKPH,' ht intgrl=',RINTH,' deriv=',RDRVH
         call edisp(itu,outs)
         write(outs,'(A,F10.3,3(A,F8.3),A,F10.3,A,F9.3,A,F9.3)') 
     &    ' Q cl=',QC,' cl spt=',CSP,' cl stp U=',CTU,' cl sp L=',CTL,
     &    ' cl gain=',RKPC,' cl intgrl=',RINTC,' deriv=',RDRVC
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL05')
      END IF
      
      RETURN

96    call edisp(iuout,'BCL05: fatal error Int Act Time must be > 1.s')
      call epwait
      call epagend
      STOP  

97    call edisp(iuout,'BCL05: fatal error on controller type.')
      call epwait
      call epagend
      STOP
981   call edisp(iuout,'BCL05: proportional should have 9 data items.')
      call epwait
      call epagend
      STOP
982   call edisp(iuout,'BCL05: prop + I should have 10 data items.')
      call epwait
      call epagend
      STOP
983   call edisp(iuout,'BCL05: prop + D should have 10 data items.')
      call epwait
      call epagend
      STOP
984   call edisp(iuout,'BCL05: prop +I+D should have 11 data items.')
      call epwait
      call epagend
      STOP
99    call edisp(iuout,'BCL05: BPID mode flag outwith 1 to 4 range.')
      call epwait
      call epagend
      STOP

      END

C ******************** BCL06 ********************
C
C This routine determines the heat exchange between a building zone
C and a plant component. It is used to link the building and plant
C systems.  Suitable for use with a controller type acting on heat
C flux.

C IPCMP : plant component supplying/extracting flux.
C IPNOD : node location within component for state variable
C         referencing.
C ISIND : index defining the type of calculation to be performed:
C         1 means m.c.(ts - ta)
C         2 means plant component model calculated heat exchange based
C           on prevailing building zone conditions. This heat flux is
C           'picked up' by this control law and transferred to the
C           building zone according to any actuator type
C         3 means h.A.(ts - ta) experimentally active.
C         4 means multiple component coupling.
C QHMAX : maximum heating flux allowed (W)
C QCMAX : maximum cooling flux allowed (W)

      SUBROUTINE BCL06(icomp)

#include "building.h"
#include "plant.h"
#include "control.h"

      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      COMMON/PCEMB/IPALLOC(MPCOM),TALLOC(MPCOM)
      COMMON/MBINFO/ZMBI(MCOM,4)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)

      DIMENSION ICPL(MPCOM,2),IXTC(MPCOM,2)

      character outs*124

      PARAMETER (SMALL=1.0E-15)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) call edisp(itu,' Entering subroutine BCL06')


C Determine the coupling type
      ISIND=IFIX(BMISCD(ICF,IDTYP,IPER,4)) 

C Fatal error tests  
      IF(ISIND.NE.4) THEN   
        IF(INT(BMISCD(ICF,IDTYP,IPER,1)).NE.5.AND.
     &  INT(BMISCD(ICF,IDTYP,IPER,1)).NE.7)then
           call edisp(itu,
     &       ' BCL06: fatal error on number of control data items.')
           call epwait
           call epagend
           STOP
        ENDIF
      ENDIF
      ICLCTP=IBCTYP(ICF,IDTYP,IPER)
      IF(ICLCTP.NE.0.AND.ICLCTP.NE.2.AND.ICLCTP.NE.4.AND.
     &   ICLCTP.NE.6.AND.ICLCTP.NE.8)then
         call edisp(itu,
     &     ' BCL06: fatal error on controller type control law.')
         call epwait
         call epagend
         STOP
      endif


      QFUT=0.
      TFUT=TNP
      INEX=0

C Determine coupled supply plant component and node.

      IF(ISIND.NE.4) THEN 
        IPCMP=IFIX(BMISCD(ICF,IDTYP,IPER,2))
        IPNOD=IFIX(BMISCD(ICF,IDTYP,IPER,3))
        IN=NPCDAT(IPCMP,9)+IPNOD-1
      ELSEIF(ISIND.EQ.4) THEN

C Set the embedded flag to zero after finding ipcmp.
        IPCMP=IFIX(BMISCD(ICF,IDTYP,IPER,2))
        if(ipcmp.ne.0) then
          IPALLOC(IPCMP)=0
        else
          call edisp(itu,
     &     ' BCL06: fatal error (zero index) of control data item.')
          call epwait
          call epagend
          STOP   
        endif

C Determine the number of supply and extracting components.
        NCPL=IFIX(BMISCD(ICF,IDTYP,IPER,2))
        NEXT=IFIX(BMISCD(ICF,IDTYP,IPER,3))
        IF(INT(BMISCD(ICF,IDTYP,IPER,1)).NE.5+2*(NCPL+NEXT)) THEN
         call edisp(itu,
     &     ' BCL06: fatal error on number of control data items.')
         call epwait
         call epagend
         STOP   
        ENDIF       
        DO 77 ICPL1=1,NCPL
           ICPL2=ICPL1*2
           ICPL(ICPL1,1)=IFIX(BMISCD(ICF,IDTYP,IPER,6+ICPL2-1))
           ICPL(ICPL1,2)=IFIX(BMISCD(ICF,IDTYP,IPER,6+ICPL2))
  77    CONTINUE
        
        DO 78 INXT1=1,NEXT
           INXT2=INXT1*2
           IXTC(INXT1,1)=IFIX(BMISCD(ICF,IDTYP,IPER,6+
     &     (NCPL*2)+INXT2-1))
           IXTC(INXT1,2)=IFIX(BMISCD(ICF,IDTYP,IPER,6+(NCPL*2)+INXT2))
  78    CONTINUE
      ENDIF

         
C Determine type of calculation and heating/cooling flux
C limits.
      QHMAX=BMISCD(ICF,IDTYP,IPER,5)
      QCMAX=-BMISCD(ICF,IDTYP,IPER,6)

C Determine the coupled extracting component and
C node therein. Trap for zero ipcpex & ipndex which
C might happen with, say an oil filled radiator or
C liquid plant component.
      IF(ISIND.NE.4) THEN 
        IPCPEX=IFIX(BMISCD(ICF,IDTYP,IPER,7))
        IPNDEX=IFIX(BMISCD(ICF,IDTYP,IPER,8))
        if(ipcpex.gt.0.and.ipndex.gt.0)then
          INEX=NPCDAT(IPCPEX,9)+IPNDEX-1
        endif
      ENDIF


C Jump depending on calculation type

C Air point heat addition/extraction; simultaneous solution of the
C [q = m.cp.dt] and [bb1.ta + bb2 q = bb3] equations gives q and ta
C Note that this treatment is only possible if building control
C function has air point actuator, and if the referenced
C plant component node's working fluid is air.
C
C Note: there are two types of "q=m.cp.dt" couplings:
C
C    - the sequential (ISIND=1) coupling substitutes the most recent
C      results from the plant domain into the current zone air-point energy 
C      balance equation. This can lead to a 'mismatch' of values
C      for heat transfer between the zone and plant domains
C
C    - the concurrent (INSID=5) coupling uses the zone and plant temperatures
C      from the previous time step to calculate the resulting heat injection.

      IF(ISIND.EQ.1 .OR. ISIND.EQ.5 ) THEN
         IF(IBAN(ICF,1).LT.0.OR.IBAN(ICF,2).GT.0.OR.
     &      MOD(ISV(IPCMP,IPNOD),10).NE.1)then
            call edisp(itu,
     &        'BCL06: fatal error on actuated node or fluid type.')
            call epwait
            call epagend
            stop
         endif

C Temperature, 1st and 2nd mass flow rate of plant injection node
         TMPN=CSVF(IN,1) 
         AMFR=CSVF(IN,2)
         VMFR=CSVF(IN,3)

C Heat capcity of dry air & vapor:
C FUNCTION SHTFLD(i,T): returns specific heat of 
C (i=1) dry air, (i=2) vapor, (i=3) water for 
C fluid @ temperature T.
C
C SHTFLD is not available in bld, but neither is the plant 
C domain, so bcl06 won't be called for bld-only simulations 
C anyway.

         Zone_temperature = TFA(icomp)

C T_average not currently used.
C         T_average     = ( TMPN + Zone_temperature ) / 2.0

         C_air_flow    = AMFR 
     &        * SHTFLD ( 1, ( TMPN + Zone_temperature ) / 2.0)

         C_vapor_flow  = VMFR 
     &        * SHTFLD ( 2, ( TMPN + Zone_temperature ) / 2.0 ) 
       
         IF(AMFR.LT.SMALL) THEN
            Q=0.
         ELSE
            IF (ISIND.EQ.1) THEN 

C Simultaneous solution of [q = m.cp.dt] and [bb1.ta + bb2 q = bb3]
C equations. Note: this methodology is really only valid for 'onion'
C type building<->plant domain iteration schemes (which are presently
C not supported by ESP-r. If used with a 'ping-pong' building->plant
C iteration scheme, the plant injection/extraction node temperatures
C will not be updated to reflect the new zone air temperature, and the 
C amount of HEAT INJECTED INTO THE ZONE WILL NOT EQUAL THE AMOUNT OF 
C HEAT EXTRACTED FROM THE PLANT! 
C
C The disparity in the zone-side and plant side simulation results 
C will increase with the time-step length. Differences of 3% or more
C have been noted for 5 minute time-steps.

               Q=( TMPN - ( BB3 /BB1 ) )
     &              / ((1./ ( C_air_flow + C_vapor_flow ))-(BB2/BB1))

            ELSE 
C ISIND = 5
C
C Use concurrent zone and plant boundary conditions from the previous 
C time-step. Note: this methodolgy will result in a building response
C that is lagged one-timestep behind the plant if 'ping-poing' type 
C domain couplings are used. The error introduced with this approach 
C should be negligable 
C
C           
C Zone heat balance:
C
C     Q = ( Injection temperature - Zone temperature ) * 
C 
C            ( Air flow heat capacity + Water flow heat capacity )

               Q=( TMPN - Zone_temperature ) 
     &              * ( C_air_flow + c_vapor_flow )

            ENDIF
         ENDIF

C Set moisture addition to zone due to plant connection.
         ZMBI(IICOMP,3)=CSVF(IN,3)

C Determine airflow out of zone assuming volume balance.
C Trap for possible inex of zero.
         if(inex.gt.0)then
           EXAIR=AMAX1(CSVF(IN,2),CSVF(INEX,2))
         else
           EXAIR=CSVF(IN,2)
         endif

C Adjust total airflow leaving zone.
         ZMBI(IICOMP,4)=ZMBI(IICOMP,4)+EXAIR

C Heat pick up from plant component by actuated building side node
      ELSE IF(ISIND.EQ.2.OR.ISIND.EQ.3) THEN

C Transfer heat from embedded components etc 
         Q=QDATA(IPCMP)   

C If component is embedded then record the actuator location, this
C defined where the heat transfer takes place on the building side.
         IF(ISIND.EQ.3) THEN
           IPALLOC(IPCMP)=1
           IZ=IBAN(ICF,1)
           IS=IBAN(ICF,2)
           IN=IBAN(ICF,3)
           IF(IN.EQ.0.AND.IS.EQ.0)TALLOC(IPCMP)=TFA(IZ)
           IF(IN.EQ.0.AND.IS.GT.0)TALLOC(IPCMP)=TFS(IZ,IS)
           IF(IN.GT.0.AND.IS.GT.0)TALLOC(IPCMP)=TFC(IZ,IS,IN)
         ENDIF

C Simultaneous solution of the [q = A.h.dt] and
C [bb1.ta + bb2 q = bb3] equations will give q and ta.
C Note that this treatment is only possible if building control
C function has air point sensor and actuator.

C New calculation types go here.

C In the case of multiple component connection multiple 
C supply and extract components can be connected to the 
C the energy input to the zone is determined by a weighted
C enthaltpy average;
C          tave = mct1 + mct2 + ....... mct(n-1) + mctn
C                 -------------------------------------
C                  mc1 + mc2 + .......... mc(n-1) + mcn
C
C  tave and the sum of the mass flow rates is then used in the
C  equation for q
C
C  Vapour input and extraction to the zone are also summed.

      ELSE IF(ISIND.EQ.4) THEN
C Loop through all connected components to get heat input
         TTOT=0.0
         AFTT=0.0
         VFTT=0.0
         DO 88 ICLP=1,NCPL
           IN=NPCDAT(ICPL(ICLP,1),9)+ICPL(ICLP,2)-1
           TTOT=TTOT+(CSVF(IN,1)*1006.*CSVF(IN,2))
           AFTT=AFTT+CSVF(IN,2)
           VFTT=VFTT+CSVF(IN,3)
  88     CONTINUE
         IF(AFTT.GT.SMALL) THEN
           TAVE=TTOT/(AFTT*1006.)
           Q=(TAVE-(BB3/BB1))/((1./(1006.*AFTT))-(BB2/BB1))
           VFEX=0.0
           DO 89 INEX=1,NEXT
             INX=NPCDAT(IXTC(INEX,1),9)+IXTC(INEX,2)-1
             VFEX=VFEX+CSVF(INX,2)
   89      CONTINUE                       
         ELSE   
           Q=0.0           
         ENDIF
C Set moisture addition to zone due to multiple plant connection.
         ZMBI(IICOMP,3)=ZMBI(IICOMP,3) + VFTT

           
C Determine airflow out of zone assuming volume balance.
         AFEX=AMAX1(VFEX,AFTT)

C Adjust total airflow leaving zone.
         ZMBI(IICOMP,4)=ZMBI(IICOMP,4)+AFEX
      ELSE
         call edisp(itu,' BCL06: fatal error on calculation type.')
         call epwait
         call epagend
         STOP
      END IF

C Limit Q if necessary.
      IF(Q.GT.QHMAX) Q=QHMAX
      IF(Q.LT.QCMAX) Q=QCMAX

C Convert Q to W/m^2 if actuator located at surface or
C in construction.
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0) ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0) Q=Q/SNA(IICOMP,ISUR)

C Assign future time row temperature and heat flux.
      TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q

C Check if actuator will input mix of radiant/convective flux
C Set IPLT to 1 in case of heating and to 2 in case of cooling.
      IF (Q.GE.0.) THEN
        IPLT=1
        QMX=QHMAX
        QMN=0.
      ELSE
        IPLT=2
        QMX=QCMAX
        QMN=0.
      END IF
      TCONT=TFUT

C Note flux max, min argument to this call set to QFUT (instead of QMX),
C and QFUT instead of QMN to prevent unessesary iteration, 
C which had previously led to plant/building flux mismatch.
C Reverted by JWH to use QMX and QMN pending investigation by Nick Kelly.
      IF(IBAN(ICF,1).EQ.-2) CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &                                  IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,2I4)')
     &   ' Heat from component & node ',IPCMP,IPNOD
         call edisp(itu,outs)
         write(outs,'(A,3I5)') ' to building side actuator  ',
     &                IBAN(ICF,1),IBAN(ICF,2),IBAN(ICF,3)
         call edisp(itu,outs)
         write(outs,'(A,F10.3,A,F10.3)') ' Qfuture = ',QFUT,
     &                                   ' Tfuture = ',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL06')
      END IF

      RETURN
      END

C ******************** BCL07

C A multi-stage controller with hysteresis.

      SUBROUTINE BCL07
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C Special common block for this controller. The variable
C stored retains the previous controller signal. ISTG07 records
C the previous stage the unit was set to as follows:
C ISTG07 = 0; unit off
C        = 1; heating mode, stage 1
C        = 2; heating mode, stage 2
C        = 3; heating mode, stage 3
C        =-1; cooling mode, stage 1
C        =-2; cooling mode, stage 2
C        =-3; cooling mode, stage 3
      COMMON/BCL07M/ISTG07(MCOM)
      
      character outs*124

      DIMENSION QH(4),QC(4)

C Fatal error test.
      IF(BMISCD(ICF,IDTYP,IPER,1).LT.12.)GOTO 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) call edisp(itu,' Entering subroutine BCL07')

C Initialise ISTG07 at first increment
      IF(NSINC.EQ.1)ISTG07(ICF)=0

      QFUT=0.
      TFUT=TNP

C Determine sensed temperature.
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C QH(1) = heating supplied when unit off - base load stage 0 (W)
C QH(2) = heating supplied at stage 1 - must be >or=to QH0 (W)
C QH(3) = heating supplied at stage 2 - must be >or=to QH1 (W)
C QH(4) = heating supplied at stage 3 - must be >or=to QH2 (W)
C QC(1) = cooling supplied when unit off - base load stage 0 (-W)
C QC(2) = cooling supplied at stage 1 - must be >or=to QC0 (-W)
C QC(3) = cooling supplied at stage 2 - must be >or=to QC1 (-W)
C QC(4) = cooling supplied at stage 3 - must be >or=to QC2 (-W)
C HSP = heating set point temp. (deg.C)
C HDB = heating dead band (K), (>or=to 0)
C CSP = cooling set point temp. (deg.C)
C CDB = cooling dead band (K), (>or=to 0)

      QH(1)=BMISCD(ICF,IDTYP,IPER,2)
      QH(2)=BMISCD(ICF,IDTYP,IPER,3)
      IF(QH(2).LT.QH(1))GOTO 97
      QH(3)=BMISCD(ICF,IDTYP,IPER,4)
      IF(QH(3).LT.QH(2))GOTO 97
      QH(4)=BMISCD(ICF,IDTYP,IPER,5)
      IF(QH(4).LT.QH(3))GOTO 97
      QC(1)=-BMISCD(ICF,IDTYP,IPER,6)
      QC(2)=-BMISCD(ICF,IDTYP,IPER,7)
      IF(QC(2).GT.QC(1))GOTO 97
      QC(3)=-BMISCD(ICF,IDTYP,IPER,8)
      IF(QC(3).GT.QC(2))GOTO 97
      QC(4)=-BMISCD(ICF,IDTYP,IPER,9)
      IF(QC(4).GT.QC(3))GOTO 97
      HSP=BMISCD(ICF,IDTYP,IPER,10)
      HDB=BMISCD(ICF,IDTYP,IPER,11)
      IF(HDB.LT.0.)GOTO 96
      CSP=BMISCD(ICF,IDTYP,IPER,12)
      CDB=BMISCD(ICF,IDTYP,IPER,13)
      IF(CDB.LT.0.)GOTO 96

C Upper heating and lower cooling values.
      HUP=HSP+HDB
      CLOW=CSP-CDB

C Establish whether or not ISTG07 alters (ie hysteresis)
C Let ISTG07 equal local variable ISTAGE.
      ISTAGE=ISTG07(ICF)
      IF(ISTAGE.EQ.0.AND.(HSP.LE.TCTL.AND.TCTL.LE.CSP))RETURN

C Prepare to switch equipment on. Identify if heating
C or cooling required.
      IF(ISTAGE.EQ.0)THEN
        T=TCTL
        IF(T.LT.HSP)GOTO 1
        IF(T.GT.CSP)GOTO 2
      ENDIF

C Equipment is already on!

C Base algorithm on the zone conditions if this level
C of energy supply were maintained throughout the time-increment.
C This assumes that the energy suppied will maintain
C the current temperature, within acceptable limits ie hysteresis.

      IF(ISTAGE.GT.0)THEN
        Q=QH(ISTAGE+1)
      ENDIF
      IF(ISTAGE.LT.0)THEN
        Q=QC(ISTAGE+1)
      ENDIF
      QQ=Q
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)

C T = zone temperature with current energy supply.
      T=(BB3-BB2*QQ)/BB1
      IF(ISTAGE.GT.0)THEN
        IF(HSP.LE.T.AND.T.LE.HUP)GOTO 3
      ENDIF
      IF(ISTAGE.LT.0)THEN
        IF(CLOW.LE.T.AND.T.LE.CSP)GOTO 4
      ENDIF

      IF(ISTAGE.LT.0)GOTO 2

C Check heating application for new ISTAGE.
C Potentially increase energy supplied.
    1 IF(T.LT.HSP)THEN
    7   ISTAGE=ISTAGE+1
        IF(ISTAGE.GT.3)GOTO 6
        Q=QH(ISTAGE+1)
        QQ=Q
        ISUR=0
        IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
        IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)
        T=(BB3-BB2*QQ)/BB1
        IF(T.LT.HSP)GOTO 7
    6   IF(ISTAGE.GT.3)ISTAGE=3
        GOTO 3
      ENDIF

C Potentially decrease energy supplied.
      IF(T.GT.HUP)THEN
    9   ISTAGE=ISTAGE-1
        IF(ISTAGE.LT.0)GOTO 8
        Q=QH(ISTAGE+1)
        QQ=Q
        ISUR=0
        IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
        IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)
        T=(BB3-BB2*QQ)/BB1
        IF(T.GT.HUP)GOTO 9
    8   IF(ISTAGE.LT.0)ISTAGE=0
        GOTO 3
      ENDIF

C Potentially increased cooling energy supplied.
    2 IF(T.GT.CSP)THEN
   12   ISTAGE=ISTAGE+1
        IF(ISTAGE.GT.3)GOTO 11
        Q=QC(ISTAGE+1)
        QQ=Q
        ISUR=0
        IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
        IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)
        T=(BB3-BB2*QQ)/BB1
        IF(T.GT.CSP)GOTO 12
   11   IF(ISTAGE.GT.3)ISTAGE=3
        GOTO 4
      ENDIF

C Potentially decreased cooling energy supplied.
      IF(T.GT.CLOW)THEN
   14   ISTAGE=ISTAGE-1
        IF(ISTAGE.LT.0)GOTO 13
        Q=QC(ISTAGE+1)
        QQ=Q
        ISUR=0
        IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
        IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)
        T=(BB3-BB2*QQ)/BB1
        IF(T.LT.CLOW)GOTO 14
   13   IF(ISTAGE.LT.0)ISTAGE=0
        GOTO 4
      ENDIF

    3 IPLT=1
      TCONT=HSP
      GOTO 5
    4 IPLT=2
      TCONT=CSP
    5 TFUT=T
      QFUT=QQ
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F10.3)') ' Qfuture = ',QFUT,
     &                                   ' Tfuture = ',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL07')
      END IF

      RETURN

C Fatal errors.
   99 call edisp(iuout,' BCL07: data incomplete.')
      GOTO 998
   97 call edisp(iuout,' BCL07: heating or cooling stages in error')
      call edisp(iuout,'        (see manual).')
      GOTO 998
   96 call edisp(iuout,' BCL07: dead-band cannot be less than zero.')
  998 call epwait
      call epagend
      STOP
      END

C ******************** BCL08

C 'BCL08' is a variable supply temperature system
C with limit constraints.

      SUBROUTINE BCL08
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/MBINFO/ZMBI(MCOM,4)
      COMMON/CLIMHG/HEXTP,HEXTF,GEXTP,GEXTF
      
      character outs*124

C Fatal error test if there are less than 6 misecel items.
      IF(BMISCD(ICF,IDTYP,IPER,1).LT.6.)GOTO 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        call edisp(itu,' Entering subroutine BCL08')
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

      QFUT=0.
      TFUT=TNP
      TCTL=TNP
      IF(IBSN(ICF,1).NE.-2)GOTO 101
      IZ=IBSN(ICF,2)
      CALL MZMIXT(IZ,TMRT,TCTL)
  101 IF(IBSN(ICF,1).EQ.-3.AND.IBSN(ICF,2).EQ.0)TCTL=TF
      IF(IBSN(ICF,1).EQ.-3.AND.IBSN(ICF,2).EQ.1)TCTL=SOLAIR(TF,QFF,QDF)

C TSM = max supply air temperature (deg.C)
C TSN = min supply air temperature (deg.C)
C AFR = air volume flow rate (m3/s)
C HSP = heating set point temp. (deg.C)
C CSP = cooling set point temp. (deg.C)
C IFC = cooling availability index (0; cooling, 1; no cooling)
      TSM=BMISCD(ICF,IDTYP,IPER,2)
      TSN=BMISCD(ICF,IDTYP,IPER,3)
      AFR=BMISCD(ICF,IDTYP,IPER,4)
      HSP=BMISCD(ICF,IDTYP,IPER,5)
      CSP=BMISCD(ICF,IDTYP,IPER,6)
      IFC=NINT(BMISCD(ICF,IDTYP,IPER,7))
      IF(TCTL.LE.CSP.AND.TCTL.GE.HSP)GOTO 3
      D=AFR*1.2*1006.0

c Is temperature above the cooling setpoint?
      IF(TCTL.GT.CSP)GOTO 1

c Too low, establish Tsupply to heat air to heating set point (HSP).
      TS=(BB3-(BB1-(BB2*D))*HSP)/(BB2*D)

c Is this supply temperature available ?
      IF(TS.LT.TF)TS=TF
      IF(TS.LT.TSN)TS=TSN
      IF(TS.LE.TSM)GOTO 2
      TS=TSM
      GOTO 2

c Zone temp. too high, establish Ts to cool to CSP.
    1 TS=(BB3-(BB1-(BB2*D))*CSP)/(BB2*D)

c Is this supply temperature available ?
      IF(IFC.EQ.0)THEN
        IF(TF.LT.TS)TS=TF
      ENDIF
      IF(IFC.EQ.1)THEN
        IF(TS.LT.TF)TS=TF
      ENDIF
      IF(TS.GE.TSN)GOTO 2
      TS=TSN

    2 TFUT=(BB3-BB2*D*TS)/(BB1-BB2*D)
      QFUT=D*(TS-TFUT)

C Enter gain due to mechanical system here
C mechanical system mass flow rate * moisture content.
C << what does GEXTF the future ambient humidity ratio 
C << have to do with this component ??
    3 ZMBI(IICOMP,3)=AFR*1.2*GEXTF

C Total mass flow rate lost from the zone (no moisture included).
      ZMBI(IICOMP,4)=ZMBI(IICOMP,4)+AFR*1.2

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F8.3,A,F8.3,A,F10.3)') 
     &     ' Qfuture = ',QFUT,
     &     ' Tfuture = ',TFUT,
     &     ' Tsupply = ',TS,
     &     ' Flow loss from zone = ',ZMBI(IICOMP,4)
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL08')
      END IF

      RETURN
   99 call edisp(iuout,' BCL08: data incomplete.')
      call epwait
      call epagend
      STOP
      END

C ******************** BCL09

C 'BCL09' is a heat pipe model (after Berthold Stanzel)
C which operates by transfering heat from
C some outermost construction node to some innermost construction
C node.  The algorithm iterates until the two nodes are within
C the stated temperature difference tolerance and the flux required
C to achieve this is within the flux difference tolerance
C relative to that flux calculated from a U $delta$ t model which
C represents the heat pipe physics.

      SUBROUTINE BCL09
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/BCL9/IHPZON,IHPCON,IHPNDO,QNOPF,QHPSV

      character outs*124

c Fatal error test.
      IF(INT(BMISCD(ICF,IDTYP,IPER,1)).NE.9)GOTO 99
      QFUT=0.
      TFUT=TNP

c ITER  = 0: no iterate; 1: iterate.
c ICON  = Construction containing heat pipe.
c INDO  = Outermost heat pipe node.
c INDI  = Innermost heat pipe node.
c TCRIT = INDI critical temperature (deg.C)
c MAXIT = Maximum number of iterations.
c TEMPC = Temperature difference tolerance.
c FLUXC = Flux difference tolerance.
c ITRAC = Trace flag.
      ITER=INT(BMISCD(ICF,IDTYP,IPER,2))
      ICON=INT(BMISCD(ICF,IDTYP,IPER,3))
      INDO=INT(BMISCD(ICF,IDTYP,IPER,4))
      INDI=INT(BMISCD(ICF,IDTYP,IPER,5))
      TCRIT=BMISCD(ICF,IDTYP,IPER,6)
      MAXIT=INT(BMISCD(ICF,IDTYP,IPER,7))
      TEMPC=BMISCD(ICF,IDTYP,IPER,8)
      FLUXC=BMISCD(ICF,IDTYP,IPER,9)
      ITRAC=INT(BMISCD(ICF,IDTYP,IPER,10))

c Heat Pipe parameter.
      SEFF=0.2

      ITERN=1
      IFLIP=2
      ICOUNT=0

      B1=BB1
      B2=BB2
      B3=BB3

c Determine flux exchange.
      IF(TFC(IICOMP,ICON,INDO).GT.TFC(IICOMP,ICON,INDI))THEN
         UEFF=500.
         IF(TFC(IICOMP,ICON,INDI).GT.TCRIT)UEFF=250.
         QFUT=UEFF*(TFC(IICOMP,ICON,INDO)-TFC(IICOMP,ICON,INDI))
         TFUT=(B3-B2*QFUT)/B1
      ELSEIF(TFC(IICOMP,ICON,INDO).LE.TFC(IICOMP,ICON,INDI))THEN
         EFFLAM=2.85
         XXX=EFFLAM/SEFF
         IF(TFC(IICOMP,ICON,INDI).GT.TCRIT)XXX=250.
         QFUT=XXX*(TFC(IICOMP,ICON,INDO)-TFC(IICOMP,ICON,INDI))
         TFUT=(B3-B2*QFUT)/B1
         RETURN
      ENDIF

      IF(ITER.EQ.0)THEN

c This will only work at small time-steps.
         QNOPF= QHPSV
         QHPSV=-QFUT
         QNOPF=QNOPF-QFUT
         RETURN
      ENDIF

c Apply and test effect by backward substitution
c of zone matrix ...
    1 Q=0.
      CALL MZBACK(Q)
      QFC(IICOMP)=QFUT*SNA(IICOMP,ICON)
      CALL MZCNB1(IICOMP,ICON)

c Iterate until ITERN > MAXIT OR
c {ABS(TFC(INDO)-TFC(INDI)) < TEMPC and ABS(QFUT-QX) < FLUXC}.
      UEFF=500.
      IF(TFC(IICOMP,ICON,INDI).GT.TCRIT)UEFF=250.
      QX=UEFF*(TFC(IICOMP,ICON,INDO)-TFC(IICOMP,ICON,INDI))

c Trace.
      IF(ITRAC.EQ.1)THEN
         IF(ITERN.EQ.1)write(outs,'(A,F4.1)')' TIME = ',BTIMEF
         call edisp(iuout,' ')
         call edisp(iuout,outs)
         write(outs,776)ITERN, TFC(IICOMP,ICON,INDO),
     &                TFC(IICOMP,ICON,INDI),QFUT, QX
  776    format(' Iteration = ',I4,' t_outer = ',F7.2,
     &          ' t_inner = ',F7.2,' QFUT = ',F10.1,' QX = ', F10.1)
         call edisp(iuout,outs)
      ENDIF

      ITERN=ITERN+1
      DELTA=0.1*QFUT
      IF(ABS(DELTA).LT.1.)THEN
         DELTA=1.
      ELSEIF(ABS(DELTA).LT.5.)THEN
         DELTA=5.
      ENDIF

      IF(ITERN.GT.MAXIT.OR.(ABS(TFC(IICOMP,ICON,INDO)-
     &             TFC(IICOMP,ICON,INDI)).LT.TEMPC.
     &             AND.ABS(QX-QFUT).LT.FLUXC))THEN

c Establish flux extract at outer node for use in MZSETU and save
c flux for use as present value at next time-step.
         QNOPF=QHPSV
         QHPSV=-QFUT
         QNOPF=QNOPF-QFUT
         RETURN

c Convergence device.
      ELSEIF(TFC(IICOMP,ICON,INDO).GT.TFC(IICOMP,ICON,INDI))THEN
         IF(IFLIP.EQ.1)THEN
            ICOUNT=ICOUNT+1
            QFUT=QFUT+DELTA/FLOAT(ICOUNT)
            IFLIP=0
         ELSE
            QFUT=QFUT+DELTA
            ICOUNT=0
            IFLIP=0
         ENDIF
      ELSEIF(TFC(IICOMP,ICON,INDO).LE.TFC(IICOMP,ICON,INDI))THEN
         IF(IFLIP.EQ.0)THEN
            ICOUNT=ICOUNT+1
            QFUT=QFUT-DELTA/FLOAT(ICOUNT)
            IFLIP=1
         ELSE
            QFUT=QFUT-DELTA
            ICOUNT=0
            IFLIP=1
         ENDIF
      ENDIF

      TFUT=(B3-B2*QFUT)/B1

      GOTO 1

   99 call edisp(iuout,' BCL09: data incomplete.')
      call epwait
      call epagend
      STOP
      END


C ******************** BCL10

C BCL10 allows separate flux ON and flux OFF set points to
C be specified for both heating and cooling.  During the
C ON periods the maximum specified flux is injected.  After
C the OFF condition the flux input is not reactivated until
C ON condition is reached.

      SUBROUTINE BCL10
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)

      COMMON/BCL10M/LASTH(MCOM),LASTC(MCOM)
      CHARACTER*3 LASTH,LASTC
      
      character outs*124

c Fatal error test.
      IF(BMISCD(ICF,IDTYP,IPER,1).LT.6.0)GOTO 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        call edisp(itu,' Entering subroutine BCL10')
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

      QFUT=0.
      TFUT=TNP

C Determine sensed temperature.
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

c QHM = fixed heating capacity (W)
c QCM = fixed cooling capacity (W)
c HSPON = heating ON set point temp. (deg.C)
c HSPOFF = heating OFF set point temp. (deg.C)
c CSPON = cooling ON set point temp. (deg.C)
c CSPOFF = cooling OFF set point temp. (deg.C)

C If the zone associated with this control function is served by an HVAC system
C simulated ideally, then use HVAC system capacity to update controller
C heating/cooling capacity for time step. 
      if(cont_fun_heat_cap(IICOMP).gt.0 ) then
        QHM=cont_fun_heat_cap(IICOMP)
      else
        QHM=BMISCD(ICF,IDTYP,IPER,2)       
      endif

      if(cont_fun_cool_cap(IICOMP).gt.0.) then
        QCM=-cont_fun_cool_cap(IICOMP)
      else        
        QCM=-BMISCD(ICF,IDTYP,IPER,3)
      endif

      HSPON=BMISCD(ICF,IDTYP,IPER,4)
      HSPOFF=BMISCD(ICF,IDTYP,IPER,5)
      CSPON=BMISCD(ICF,IDTYP,IPER,6)
      CSPOFF=BMISCD(ICF,IDTYP,IPER,7)

C Note that heating and cooling throttling ranges may overlap. In
C this case simultaneous heating and cooling may result.

C Heating cycle:
      IF(TCTL.LT.HSPON)THEN
         QH=QHM
         LASTH(IICOMP)='ON '
      ELSEIF(TCTL.GT.HSPOFF)THEN
         QH=0.
         LASTH(IICOMP)='OFF'
      ELSEIF(TCTL.GE.HSPON.AND.TCTL.LE.HSPOFF)THEN
         IF(LASTH(IICOMP).EQ.'ON ')QH=QHM
         IF(LASTH(IICOMP).EQ.'OFF')QH=0.
      ENDIF

C Cooling cycle:
      IF(TCTL.GT.CSPON)THEN
         QC=QCM
         LASTC(IICOMP)='ON '
      ELSEIF(TCTL.LT.CSPOFF)THEN
         QC=0.
         LASTC(IICOMP)='OFF'
      ELSEIF(TCTL.LE.CSPON.AND.TCTL.GE.CSPOFF)THEN
         IF(LASTC(IICOMP).EQ.'ON ')QC=QCM
         IF(LASTC(IICOMP).EQ.'OFF')QC=0.
      ENDIF

C Convert from (/m^2) if necessary.
      Q=QH+QC
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      IPLT=1
      QMX=Q
      QMN=Q
      TCONT=TFUT
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F10.3,A,F10.3,5a,F8.3)')
     &     ' Qfuture= ',QFUT,' (heating=',QH,' cooling=',QC,
     &     ') heating prior ',LASTH(IICOMP),
     &     ' cooling prior ',LASTC(IICOMP),
     &     ' Tfuture= ',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL10')
      END IF

      RETURN
   99 call edisp(iuout,' BCL10: data incomplete.')
      call epwait
      call epagend
      STOP
      END
      
C ******************** BCL11

C BCL11 is a multi-sensor temperature controller.
C This controller will bring the temperature of the
C associated zone to the greatest/lowest/mean/weighted value
C of the auxiliary sensor(s).
C It is suitable for use with a type 0 controller.
C It also is used to support temporal file setpoint sensing.

      SUBROUTINE BCL11
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/CMSVAR/NSEN(MCF),ISMOD(MCF),IBMSN(MCF,MSEN,4),
     &TAUXSN(MCF,MSEN)
     
      character outs*248
      
      DIMENSION SNWGHT(MSEN)  

C Fatal error tests:
      ICTYP=IBCTYP(ICF,IDTYP,IPER)
      IF(ICTYP.NE.0.AND.(ICTYP.LT.35.OR.ICTYP.GT.39))then
        call edisp(iuout,' BCL11: controller type error.')
        call epwait
        call epagend
        STOP
      endif

      IF(INT(BMISCD(ICF,IDTYP,IPER,1)).GT.16)then
        call edisp(iuout,' BCL11: incorrect no. of misc data items.')
        call epwait
        call epagend
        STOP
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) call edisp(itu,' Entering subroutine BCL11')

      QFUT=0.
      TFUT=TNP
      TEMPTO=0.0
      TEMPWT=0.0
      SNWTOT=0.0
      SCALEF=1.0
      OFFSET=0.0
        
C QHM = max heating capacity (W)
C QHN = min heating capacity (W)
C QCM = max cooling capacity (W)
C QCN = min cooling capacity (W)
      QHM=BMISCD(ICF,IDTYP,IPER,2)
      QHN=BMISCD(ICF,IDTYP,IPER,3)
      QCM=-BMISCD(ICF,IDTYP,IPER,4)
      QCN=-BMISCD(ICF,IDTYP,IPER,5)

C NSEN(ICF) = number of auxiliary sensors for control function.
      NSEN(ICF)=INT(BMISCD(ICF,IDTYP,IPER,6))

C ISM = mode of operation flag:-
C ISM = 1: TDIF = (ref sensor)-(greatest value of auxiliary sensors);
C ISM = 2: TDIF = (ref sensor)-(least value of auxiliary sensors);
C ISM = 3: TDIF = (ref sensor)-(mean value of auxiliary sensors).
C ISM = 4: TDIF = (ref sensor)-(weighting of auxiliary sensors).
      ISM=INT(BMISCD(ICF,IDTYP,IPER,7))

C SNWGHT() is the sensor weighting when ISM=4.
C If only one sensor read attributes and then get scaling and offset
C based on whether there is weighting or not. This follows the same
C logic used in econtrol.F subroutine EVCNTRLAW.
      if(NSEN(ICF).eq.1)then
        IBMSN(ICF,1,1)=INT(BMISCD(ICF,IDTYP,IPER,8))
        IBMSN(ICF,1,2)=INT(BMISCD(ICF,IDTYP,IPER,9))
        IBMSN(ICF,1,3)=INT(BMISCD(ICF,IDTYP,IPER,10))
        IBMSN(ICF,1,4)=INT(BMISCD(ICF,IDTYP,IPER,11))
        IF(ISM.EQ.4)THEN
          SNWGHT(1)=BMISCD(ICF,IDTYP,IPER,12)
          SNWTOT=SNWGHT(1)
          SCALEF=BMISCD(ICF,IDTYP,IPER,13)
          OFFSET=BMISCD(ICF,IDTYP,IPER,14)
        ELSE
          SCALEF=BMISCD(ICF,IDTYP,IPER,12)
          OFFSET=BMISCD(ICF,IDTYP,IPER,13)
C          SCALEF=1.0
C          OFFSET=0.0
        ENDIF
      elseif(NSEN(ICF).eq.2)then
        IBMSN(ICF,1,1)=INT(BMISCD(ICF,IDTYP,IPER,8))
        IBMSN(ICF,1,2)=INT(BMISCD(ICF,IDTYP,IPER,9))
        IBMSN(ICF,1,3)=INT(BMISCD(ICF,IDTYP,IPER,10))
        IBMSN(ICF,1,4)=INT(BMISCD(ICF,IDTYP,IPER,11))
        IBMSN(ICF,2,1)=INT(BMISCD(ICF,IDTYP,IPER,12))
        IBMSN(ICF,2,2)=INT(BMISCD(ICF,IDTYP,IPER,13))
        IBMSN(ICF,2,3)=INT(BMISCD(ICF,IDTYP,IPER,14))
        IBMSN(ICF,2,4)=INT(BMISCD(ICF,IDTYP,IPER,15))
        IF(ISM.EQ.4)THEN
          SNWGHT(1)=BMISCD(ICF,IDTYP,IPER,16)
          SNWGHT(2)=BMISCD(ICF,IDTYP,IPER,17)
          SNWTOT=SNWGHT(1)+SNWGHT(2)
          SCALEF=BMISCD(ICF,IDTYP,IPER,18)
          OFFSET=BMISCD(ICF,IDTYP,IPER,19)
        ELSE
          SCALEF=BMISCD(ICF,IDTYP,IPER,16)
          OFFSET=BMISCD(ICF,IDTYP,IPER,17)
        ENDIF
      else
        DO 10 L=1,NSEN(ICF)           
          IBMSN(ICF,L,1)=INT(BMISCD(ICF,IDTYP,IPER,8+4*(L-1)))
          IBMSN(ICF,L,2)=INT(BMISCD(ICF,IDTYP,IPER,9+4*(L-1)))
          IBMSN(ICF,L,3)=INT(BMISCD(ICF,IDTYP,IPER,10+4*(L-1)))
          IBMSN(ICF,L,4)=INT(BMISCD(ICF,IDTYP,IPER,11+4*(L-1)))

C << If using +4* logic, how does 12+ and 13+  fit? >>
          IF(ISM.EQ.4)THEN
            SNWGHT(L)=BMISCD(ICF,IDTYP,IPER,12+(L-1)+4*(NSEN(ICF)-1))
            SNWTOT=SNWTOT+SNWGHT(L)
          ENDIF
          IF(ISM.GE.1.AND.ISM.LT.4.AND.L.EQ.NSEN(ICF))THEN
            SCALEF=BMISCD(ICF,IDTYP,IPER,12+4*(L-1))
            OFFSET=BMISCD(ICF,IDTYP,IPER,13+4*(L-1))
          ELSEIF(ISM.EQ.4.AND.L.EQ.NSEN(ICF))THEN
            SCALEF=BMISCD(ICF,IDTYP,IPER,13+(L-1)+4*(NSEN(ICF)-1))
            OFFSET=BMISCD(ICF,IDTYP,IPER,14+(L-1)+4*(NSEN(ICF)-1))
          ENDIF    
10      CONTINUE
      endif
      
C Fatal error tests:      
      IF(ISM.LT.1.OR.ISM.GT.4)then
        call edisp(iuout,' BCL11: misc data - mode flag error.')
        call epwait
        call epagend
        STOP
      endif
      IF(NSEN(ICF).GT.2)then
        call edisp(iuout,' BCL11: Incorrect number of auxy sensors.')
        call epwait
        call epagend
        STOP
      endif
      IF(ISM.EQ.4.AND.INT(SNWTOT).NE.100)THEN
        call edisp(iuout,
     &   ' BCL11: sum of weighting factors does not equal 100.0.')
        call epwait
        call epagend
        STOP
      ENDIF
        
C Determine sensed temperatures.
      CALL CFMVAR(ier)
      if(ier.eq.2)then
        return
      endif

      DO 20 J=1,NSEN(ICF)
C Determine the greatest sensed temperature value;
         IF(J.EQ.1)TEMPHI=TAUXSN(ICF,J)
         IF(TAUXSN(ICF,J).GT.TEMPHI)TEMPHI=TAUXSN(ICF,J)
C Determine the lowest sensed temperature value;
         IF(J.EQ.1)TEMPLO=TAUXSN(ICF,J)
         IF(TAUXSN(ICF,J).LT.TEMPLO)TEMPLO=TAUXSN(ICF,J)
C Determine the total of the sensed values - required for mean;
         TEMPTO=TEMPTO+TAUXSN(ICF,J)
C Determine the weighted sensed temperature;
         TEMPWT=TEMPWT+(SNWGHT(J)*TAUXSN(ICF,J))*0.01
20    CONTINUE

C Determine the mean sensed temperature value;
      TEMPAV=TEMPTO/(NSEN(ICF))

C Apply scaling factor (SCALEF) and offset (OFFSET) for zonal
C pro rata temperature control.

      TEMPHI=(SCALEF*TEMPHI)+OFFSET
      TEMPLO=(SCALEF*TEMPLO)+OFFSET
      TEMPAV=(SCALEF*TEMPAV)+OFFSET
      TEMPWT=(SCALEF*TEMPWT)+OFFSET

C Debug.
C      write(6,*) '*OFFSET,SCALEF,TEMPHI,TEMPLO,TEMPAV,TEMPWT',OFFSET,
C     & SCALEF,TEMPHI,TEMPLO,TEMPAV,TEMPWT

      IF(ISM.EQ.1)THEN
        TDIF=ABS(TEMPHI-TNP)
        TSEN=TEMPHI
      ELSEIF(ISM.EQ.2)THEN
        TDIF=ABS(TEMPLO-TNP)
        TSEN=TEMPLO
      ELSEIF(ISM.EQ.3)THEN
        TDIF=ABS(TEMPAV-TNP)
        TSEN=TEMPAV
      ELSEIF(ISM.EQ.4)THEN 
        TDIF=ABS(TEMPWT-TNP)
        TSEN=TEMPWT
      ENDIF
 
      IF(TDIF.LT.0.001)RETURN

C Since the sensed temperature at the other location
C is different from the temperature of THIS zone 
C when no heating/ cooling is applied, it is 
C necessary to apply control.
      Q=(BB3-BB1*TSEN)/BB2

C Convert from (/m^2) if necessary.
      QQ=Q
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)

C Is this capacity available ?

      IF(TSEN.GE.TNP)THEN
        IF(QQ.LE.QHM.AND.QQ.GE.QHN)GOTO 1
        IF(QQ.GT.QHM)GOTO 2
        Q=QHN
        IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
        GOTO 1
    2   Q=QHM
        IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
        GOTO 1
      ELSEIF(TSEN.LT.TNP)THEN
        IF(QQ.GE.QCM.AND.QQ.LE.QCN)GOTO 1
        IF(QQ.LT.QCM)GOTO 3
        Q=QCN
        IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
        GOTO 1
    3   Q=QCM
        IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      ENDIF

    1 TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      IPLT=1
      QMX=Q
      QMN=0
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(6(A,F10.3),7(A,F5.2))') ' Qfuture=',QFUT,
     &     'W Tfuture=',TFUT,'C Max ht cp=',QHM,'W Min ht cp=',QHN,
     &     'W Max cl cp=',QCM,'W Min cl cp=',QCN,
     &     'W Scale=',SCALEF,' Offset=',OFFSET,
     &     'C T hi=',TEMPHI,'C T lo=',TEMPLO,'C T avg=',TEMPAV,
     &     'C T wtd=',TEMPWT,' T sensed=',TSEN
         call edisp248(itu,outs,110)
         call edisp(itu,' Leaving subroutine BCL11')
      END IF
     
      RETURN
      END

C ******************** BCL12

C BCL12 is a multi-sensor on-off controller. It acts to bring the
C associated zone temperature to some function of the secondary
C sensor(s). During the ON periods the maximum specified
C flux is injected.  After the OFF condition the flux input
C is not reactivated until the ON condition is reached.
C It is suitable for use with a type 0 controller.

      SUBROUTINE BCL12
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)

      COMMON/BCL10M/LASTH(MCOM),LASTC(MCOM)
      COMMON/CMSVAR/NSEN(MCF),ISMOD(MCF),IBMSN(MCF,MSEN,4),
     &TAUXSN(MCF,MSEN)
      
      CHARACTER*3 LASTH,LASTC
      character outs*124
      
      DIMENSION SNWGHT(MSEN)

C Fatal error tests:
      ICTYP=IBCTYP(ICF,IDTYP,IPER)
      IF(ICTYP.NE.0.AND.(ICTYP.LT.35.OR.ICTYP.GT.39))then
        call edisp(iuout,' BCL12: controller type error.')
        call epwait
        call epagend
        STOP
      endif
      IF(INT(BMISCD(ICF,IDTYP,IPER,1)).GT.16)then
        call edisp(iuout,' BCL12: incorrect no. of misc data items.')
        call epwait
        call epagend
        STOP
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) call edisp(itu,' Entering subroutine BCL12')
      
      QFUT=0.
      TFUT=TNP
      TEMPTO=0.0
      TEMPWT=0.0
      SNWTOT=0.0

C QHM = max heating flux capacity (W),
C QCM = max cooling flux capacity (W),
C HDIF = heating differential (deg.C),
C CDIF = cooling differential (deg.C),
      QHM=BMISCD(ICF,IDTYP,IPER,2)
      QCM=-BMISCD(ICF,IDTYP,IPER,3)
      HDIF=BMISCD(ICF,IDTYP,IPER,4)
      CDIF=BMISCD(ICF,IDTYP,IPER,5)
      
C NSEN(ICF) = number of auxiliary sensors for control function.
      NSEN(ICF)=INT(BMISCD(ICF,IDTYP,IPER,6))

C ISM = sensor 'mode of operation' flag:-
C ISM = 1: TDIF = (ref sensor)-(greatest value of auxiliary sensors).
C ISM = 2: TDIF = (ref sensor)-(least value of auxiliary sensors).
C ISM = 3: TDIF = (ref sensor)-(mean value of auxiliary sensors).
C ISM = 4: TDIF = (ref sensor)-(weighting of auxiliary sensors).
      ISM=INT(BMISCD(ICF,IDTYP,IPER,7))

C SNWGHT(?) is the sensor weighting when ISM=4     
      DO 10 L=1,NSEN(ICF)           
        IBMSN(ICF,L,1)=INT(BMISCD(ICF,IDTYP,IPER,8+4*(L-1)))
        IBMSN(ICF,L,2)=INT(BMISCD(ICF,IDTYP,IPER,9+4*(L-1)))
        IBMSN(ICF,L,3)=INT(BMISCD(ICF,IDTYP,IPER,10+4*(L-1)))
        IBMSN(ICF,L,4)=INT(BMISCD(ICF,IDTYP,IPER,11+4*(L-1)))
        IF(ISM.EQ.4)THEN
         SNWGHT(L)=BMISCD(ICF,IDTYP,IPER,12+(L-1)+4*(NSEN(ICF)-1))
         SNWTOT=SNWTOT+SNWGHT(L)
        ENDIF
10    CONTINUE

C Fatal error tests:
      IF(ISM.LT.1.OR.ISM.GT.4)then
        call edisp(iuout,' BCL12: misc data;sensor flag error.')
        call epwait
        call epagend
        STOP
      endif
      IF(NSEN(ICF).GT.2)then
        call edisp(iuout,' BCL12: Incorrect number of auxy sensors.')
        call epwait
        call epagend
        STOP
      endif
      IF(ISM.EQ.4.AND.INT(SNWTOT).NE.100)THEN
        call edisp(iuout,
     &    ' BCL12: sum of weighting factors does not equal 100.0.')
        call epwait
        call epagend
        STOP
      ENDIF
      
C Determine control point temperature,
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C Determine the auxiliary sensed temperatures,
      CALL CFMVAR(ier)
      if(ier.eq.2)then
        return
      endif

      DO 20 J=1,NSEN(ICF)
C Determine the greatest sensed temperature value;
         IF(J.EQ.1)TEMPHI=TAUXSN(ICF,J)
         IF(TAUXSN(ICF,J).GT.TEMPHI)TEMPHI=TAUXSN(ICF,J)
C Determine the lowest sensed temperature value;
         IF(J.EQ.1)TEMPLO=TAUXSN(ICF,J)
         IF(TAUXSN(ICF,J).LT.TEMPLO)TEMPLO=TAUXSN(ICF,J)
C Determine the total of the sensed values - required for mean;
         TEMPTO=TEMPTO+TAUXSN(ICF,J)
C Determine the weighted sensed temperature;
         TEMPWT=TEMPWT+(SNWGHT(J)*TAUXSN(ICF,J))*0.01
20    CONTINUE

C Determine the mean of the sensed temperature values;
      TEMPAV=TEMPTO/(NSEN(ICF))

      IF(ISM.EQ.1)THEN
         TMSEN=TEMPHI
      ELSEIF(ISM.EQ.2)THEN
         TMSEN=TEMPLO
      ELSEIF(ISM.EQ.3)THEN
         TMSEN=TEMPAV
      ELSEIF(ISM.EQ.4)THEN 
         TMSEN=TEMPWT
      ENDIF

C Heating cycle:
        IF(TCTL.LT.(TMSEN-0.5*HDIF))THEN
          QH=QHM
          LASTH(IICOMP)='ON '
        ELSEIF(TCTL.GT.(TMSEN+0.5*HDIF))THEN
          QH=0.
          LASTH(IICOMP)='OFF'
        ELSEIF(TCTL.GE.(TMSEN-0.5*HDIF).AND.
     &       TCTL.LE.(TMSEN+0.5*HDIF))THEN
          IF(LASTH(IICOMP).EQ.'ON ')QH=QHM
          IF(LASTH(IICOMP).EQ.'OFF')QH=0.
        ENDIF
        
C Cooling cycle:
        IF(TCTL.GT.(TMSEN+0.5*CDIF))THEN
           QC=QCM
           LASTC(IICOMP)='ON '
        ELSEIF(TCTL.LT.(TMSEN-0.5*CDIF))THEN
           QC=0.
           LASTC(IICOMP)='OFF'
        ELSEIF(TCTL.LE.(TMSEN+0.5*CDIF).AND.
     &       TCTL.GE.(TMSEN-0.5*CDIF))THEN
             IF(LASTC(IICOMP).EQ.'ON ')QC=QCM
         IF(LASTC(IICOMP).EQ.'OFF')QC=0.
        ENDIF
        
C Convert from (/m^2) if necessary.
      Q=QH+QC
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      IPLT=1
      QHMX=Q
      QHMN=0
      TCONT=TFUT
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QHMX,QHMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F10.3)') ' Qfuture = ',QFUT,
     &                                   ' Tfuture = ',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL12')
      END IF

      RETURN
      END

C ******************** BCL13
C BCL13 is time-proportioning on/off controller. In several ways
C it extends the functionality offered by BCL10.
C It is suitable for use with controllers types 0,35,36,37,38,39. 
C Heating and cooling restrictions are allowed.

      SUBROUTINE BCL13
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PREC7/ITCNST      
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C Special common for BCL13; icounh and icounc are
C heating cycle and cooling cycle period time-step counters.      
      COMMON/TIMPCT/ICOUNH,ICOUNC
      
      character outs*124
      
C Fatal error test:
      ICTYP=IBCTYP(ICF,IDTYP,IPER)
      IF(ICTYP.NE.0.AND.(ICTYP.LT.35.OR.ICTYP.GT.39))then
        call edisp(iuout,' BCL13: controller type error.')
        call epwait
        call epagend
        STOP
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        call edisp(itu,' Entering subroutine BCL13')
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

      QFUT=0.0
      TFUT=TNP

C Initialise heating and cooling cycle period time-step counters:    
      IF(NSINC.EQ.((ITCNST*24*NTSTEP)+1))THEN
         ICOUNH=0
         ICOUNC=0
      ENDIF

C QHM = max heating capacity (W)
C QCM = max cooling capacity (W)
C HSPON  = heating ON set point temp. (deg.C)
C HSPOFF = heating OFF set point temp. (deg.C)
C CSPON  = cooling ON set point temp. (deg.C)
C CSPOFF = cooling OFF set point temp. (deg.C)

      QHM=BMISCD(ICF,IDTYP,IPER,2)
      QCM=-BMISCD(ICF,IDTYP,IPER,3)
      HSPON=BMISCD(ICF,IDTYP,IPER,4)
      HSPOFF=BMISCD(ICF,IDTYP,IPER,5)
      CSPON=BMISCD(ICF,IDTYP,IPER,6)
      CSPOFF=BMISCD(ICF,IDTYP,IPER,7)

C CYTH   = total heating cycle period (minutes).     
C HONMIN = minimum heating ON cycle time (minutes).
C HOFMIN = minimum heating OFF cycle time (minutes).
C CYTC   = total cooling cycle period (minutes).
C CONMIN = minimum cooling ON cycle time (minutes).
C COFMIN = minimum cooling OFF cycle time (minutes).

      CYTH=60.0*BMISCD(ICF,IDTYP,IPER,8)
      HONMIN=60.0*BMISCD(ICF,IDTYP,IPER,9)
      HOFMIN=60.0*BMISCD(ICF,IDTYP,IPER,10)
      CYTC=60.0*BMISCD(ICF,IDTYP,IPER,11)
      CONMIN=60.0*BMISCD(ICF,IDTYP,IPER,12)
      COFMIN=60.0*BMISCD(ICF,IDTYP,IPER,13)

C Fatal error tests.
      IF(INT(BMISCD(ICF,IDTYP,IPER,1)).NE.12)GOTO 99
      
C Note that heating and cooling proportional bands may overlap. In
C this case simultaneous heating and cooling may result.
             
C Establish the total cycle period time in terms of time-steps:
C for heating;
      NTSCYH=NINT((CYTH*NTSTEP)/3600)
C for cooling;
      NTSCYC=NINT((CYTC*NTSTEP)/3600)
  
C Establish the minimum ON and OFF cycle times in terms of time-steps:
C for heating;
      MTSHON=NINT((HONMIN*NTSTEP)/3600)
      MTSHOF=NINT((HOFMIN*NTSTEP)/3600)
C for cooling;
      MTSCON=NINT((CONMIN*NTSTEP)/3600)
      MTSCOF=NINT((COFMIN*NTSTEP)/3600)
      
C Proportionality ratio for heating,
      RATIOH=(NTSCYH-(MTSHON+MTSHOF))/(HSPOFF-HSPON)
C Proportionality ratio for cooling,
      RATIOC=(NTSCYC-(MTSCON+MTSCOF))/(CSPON-CSPOFF)

C Determine the control point temperature,
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif
      
C Heating cycle:

      IF(ICOUNH.EQ.0.OR.ICOUNH.EQ.NTSCYH)THEN
C End of heating cycle period;
C Re-establish no. of ON time-steps for next cycle period, 
        NTSHON=NINT((NTSCYH-MTSHOF)+RATIOH*(TCTL-HSPON))
        IF(NTSHON.LT.MTSHON)NTSHON=MTSHON
        IF(NTSHON.GT.(NTSCYH-MTSHOF))NTSHON=(NTSCYH-MTSHOF)        
        IF(TCTL.GE.HSPOFF)NTSHON=0
        IF(TCTL.LT.HSPON)NTSHON=NTSCYH
        ICOUNH=1       
      ELSE
         ICOUNH=ICOUNH+1
      ENDIF
           
      IF(ICOUNH.LE.NTSHON)THEN
         QH=QHM   ! heating is ON
      ELSE
         QH=0.0   ! heating is OFF
      ENDIF
                  
C Cooling cycle:

      IF(ICOUNC.EQ.0.OR.ICOUNC.EQ.NTSCYC)THEN
C End of cooling cycle period;
C Re-establish no. of ON time-steps for next cycle period,
        NTSCON=NINT((NTSCYC-MTSCOF)-(RATIOC*(CSPON-TCTL)))
        IF(NTSCON.LT.MTSCON)NTSCON=MTSCON 
        IF(NTSCON.GT.(NTSCYC-MTSCOF))NTSCON=NTSCYC-MTSCOF
        IF(TCTL.LE.CSPOFF)NTSCON=0 
        IF(TCTL.GT.CSPON)NTSCON=NTSCYC           
        ICOUNC=1         
      ELSE
         ICOUNC=ICOUNC+1
      ENDIF

      IF(ICOUNC.LE.NTSCON)THEN
C cooling is ON
         QC=QCM
      ELSE
C cooling is OFF,
         QC=0.0
      ENDIF
      
C Convert from (/m^2) if necessary.
      Q=QH+QC
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      
      TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      IPLT=1
      QMX=Q
      QMN=0
      TCONT=TFUT
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F10.3,A,F10.3,a,F6.3,a,i3,a,F6.3,
     &                a,i3,a,F7.3)')
     &   ' Qfuture=',QFUT,' (Ht=',QH,' Cl=',QC,
     &   ') Ht prop=',RATIOH,' Ht cnt=',ICOUNH,
     &   ' Cl prop=',RATIOC,' Cl cnt=',ICOUNC,
     &   ' Tfuture=',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL13')
      END IF
      
      RETURN
   99 call edisp(iuout,' BCL13: incorrect number of data items.')
      call epwait
      call epagend
      STOP
      END

C ******************** BCL14
C BCL14 is a floating action ('three-position') controller.
C It is suitable for use with controller types 0,35,36,37,38,39. 
C Heating and cooling restrictions are allowed.

      SUBROUTINE BCL14
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/OUTIN/IUOUT,IUIN    
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/FLTCTR/QHLAST,QCLAST
      COMMON/FLTCLR/LASTH(MCOM),LASTC(MCOM)
      CHARACTER*3 LASTH,LASTC
      
      character outs*124

C Fatal error tests:
      ICTYP=IBCTYP(ICF,IDTYP,IPER)
      IF(ICTYP.NE.0.AND.(ICTYP.LT.35.OR.ICTYP.GT.39))then
        call edisp(iuout,' BCL14: controller type error.')
        call epwait
        call epagend
        STOP
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        call edisp(itu,' Entering subroutine BCL14')
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif
      
      QFUT=0.0
      TFUT=TNP

C HSP = heating set-point (deg.C),
C HDB = heating deadband (deg.C),
C HUPDIF = heating "shut switch" differential (deg.C),
C HLODIF = heating "open switch" differential (deg.C),
C CSP = cooling set-point (deg.C),
C CDB = cooling deadband (deg.C),
C CUPDIF = cooling "open switch" differential (deg.C),
C CLODIF = cooling "shut switch" differential (deg.C),
C QHM = maximum heating flux (W),
C QHN = minimum heating flux (W),
C HROC = rate of change of heater actuator (time-steps),
C QCM = maximum cooling flux (W),
C QCN = minimum cooling flux (W),
C CROC = rate of change of cooling actuator (time-steps),
     
      HSP=BMISCD(ICF,IDTYP,IPER,2)
      HDB=BMISCD(ICF,IDTYP,IPER,3)
      HUPDIF=BMISCD(ICF,IDTYP,IPER,4)
      HLODIF=BMISCD(ICF,IDTYP,IPER,5)
      CSP=BMISCD(ICF,IDTYP,IPER,6)
      CDB=BMISCD(ICF,IDTYP,IPER,7)
      CUPDIF=BMISCD(ICF,IDTYP,IPER,8)
      CLODIF=BMISCD(ICF,IDTYP,IPER,9)      
      QHM=BMISCD(ICF,IDTYP,IPER,10)
      QHN=BMISCD(ICF,IDTYP,IPER,11)
      HROC=BMISCD(ICF,IDTYP,IPER,12)
      QCM=-BMISCD(ICF,IDTYP,IPER,13)
      QCN=-BMISCD(ICF,IDTYP,IPER,14)
      CROC=BMISCD(ICF,IDTYP,IPER,15)

C Fatal error test.
      IF(INT(BMISCD(ICF,IDTYP,IPER,1)).NE.14)GOTO 99           
     
      QFUT=0.0
      TFUT=TNP

      CALL CFVAR(TCTL,IER)      
      if(ier.eq.2)then
        return
      endif
      T=TCTL
      
      DELTQH=(QHM-QHN)/HROC
      DELTQC=(QCM-QCN)/CROC

C Heating cycle;
      
      IF(T.GT.HSP-HDB*0.5.AND.T.LT.HSP+HDB*0.5)THEN
C T within deadband,
         QH=QHLAST
         LASTH(IICOMP)='OFF'

      ELSEIF(T.GE.HSP+HDB*0.5+HUPDIF)THEN
C T above top limit,
         QH=QHLAST-DELTQH
         LASTH(IICOMP)='ON'

      ELSEIF(T.LE.HSP-HDB*0.5-HLODIF)THEN
C T below bottom limit,
         QH=QHLAST+DELTQH
         LASTH(IICOMP)='ON'  

      ELSEIF(T.GE.HSP+HDB*0.5)THEN
C T in upper differential,
         IF(LASTH(IICOMP).EQ.'ON')THEN
            QH=QHLAST-DELTQH
            LASTH(IICOMP)='ON'
         ELSE
            QH=QHLAST
            LASTH(IICOMP)='OFF'
         ENDIF
         
      ELSEIF(T.LE.HSP-HDB*0.5)THEN
C  T in lower differential,
         IF(LASTH(IICOMP).EQ.'ON')THEN           
            QH=QHLAST+DELTQH
            LASTH(IICOMP)='ON'
         ELSE
            QH=QHLAST
            LASTH(IICOMP)='OFF'
         ENDIF       
         
      ENDIF
      
      IF(QH.GT.QHM)QH=QHM
      IF(QH.LT.QHN)QH=QHN
      QHLAST=QH

C Cooling cycle;
      
      IF(T.GT.CSP-CDB*0.5.AND.T.LT.CSP+CDB*0.5)THEN
C T within deadband,
         QC=QCLAST
         LASTC(IICOMP)='OFF'

      ELSEIF(T.GE.CSP+CDB*0.5+CUPDIF)THEN
C T above top limit,
         QC=QCLAST+DELTQC
         LASTC(IICOMP)='ON'

      ELSEIF(T.LE.CSP-CDB*0.5-CLODIF)THEN
C T below bottom limit,
         QC=QCLAST-DELTQC
         LASTC(IICOMP)='ON'

      ELSEIF(T.GE.CSP+CDB*0.5)THEN
C T in upper differential,
         IF(LASTC(IICOMP).EQ.'ON')THEN
            QC=QCLAST-DELTQC 
            LASTC(IICOMP)='ON'
         ELSE
            QC=QCLAST
            LASTC(IICOMP)='OFF'
         ENDIF
         
      ELSEIF(T.LE.CSP-CDB*0.5)THEN
C T in lower differential,
         IF(LASTC(IICOMP).EQ.'ON')THEN           
            QC=QCLAST-DELTQC
            LASTC(IICOMP)='ON'
         ELSE
            QC=QCLAST
            LASTC(IICOMP)='OFF'
         ENDIF
         
      ENDIF
      
      IF(QC.LT.QCM)QC=QCM
      IF(QC.GT.QCN)QC=QCN
      QCLAST=QC
      
C Convert from (/m^2) if necessary.
      Q=QH+QC
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      
      TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      IPLT=1
      QMX=Q
      QMN=0
      TCONT=TFUT
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F10.3,A,F10.3,5a,F8.3)')
     &     ' Qfuture= ',QFUT,' (heating=',QH,' cooling=',QC,
     &     ') heating prior ',LASTH(IICOMP),
     &     ' cooling prior ',LASTC(IICOMP),
     &     ' Tfuture= ',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL14')
      END IF
      
      RETURN
   99 call edisp(iuout,' BCL14: incorrect number of data items.')
      call epwait
      call epagend
      STOP
      END

C ******************** BCL15

C Logic Optimum Start (rewind) Controller.
C Suitable for use with controller types 0,35,36,37,38,39. 

      SUBROUTINE BCL15
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/OPTCTL/IRWIND,IRWLAS,IRWCNT,IRWCSV,IRWCHP,IRWCHN,IRWCHG,
     &LGCTIN,INITCT,STFUT,STPRES,STPAS1,STPAS2,IRWFNL,TCTLSV
      COMMON/TS6/idysv,ihrsv,irwcou,tsclaw,rewnd,nsncv1,nsncv2,pasone

      LOGICAL tsclaw,rewnd,pasone,close
      
      CHARACTER OUTS*124

C COMMON BLOCK VARIABLES FOR OPTCTL:-
C     IRWIND = FLAG INDICATING REWIND CALLED FOR,
C     IRWLAS = MEMORY FOR PREVIOUS REWIND OPERATION,
C     IRWCNT = NUMBER OF REWIND OPERATIONS,
C     IRWCSV=  MEMORY FOR REWIND COUNTER,
C     IRWCHP = FLAG INDICATING A REWIND AND FORWARD ADJUST OPERATION,
C     IRWCHN = FLAG INDICATING A REWIND AND BACKWARD ADJUST OPERATION,
C     IRWCHG = FLAG INDICATING BOTH IRWCHP & IRWCHN POSITIVE,
C     IRWFNL = FLAG INDICATING FINAL REWIND OPERATION,
C     LGCTIN = FLAG INDICATING INTIALISATION TIME,
C     INITCT = INITIALISATION COUNTER,
C     ST???  = FUTURE/PRESENT/START TRIAL START TIMES,

C MISC. DATA ITEMS:
C QH      = HEATING CAPACITY (W);
C DTMP    = DESIRED TEMPERATURE LEVEL (DEG.C);
C TR      = ACCEPTABLE TEMPERATURE BAND (DEG.C);
C DTOA    = DESIRED TIME OF ARRIVAL;
C TIMDIF  = MINIMUM TIME DIFFERENCE BETWEEN SUCCESSIVE TRIAL TIMES (HOURS);
C ITIMTR  = INITIAL TRIAL TEST TIME;
C         = 1: USER-DEFINED;
C         = 2: START-TIME = 04.00 HOURS;
C         = 3: BIRTLES & JOHN EQUATION.
C If ITIMTR = 3: 
C A0      =  HEATING CONSTANT FOR THE BIRTLES & JOHN EQUATION.
C A1      =  BUILDING  "       "   "     "        "     "    
C A2      =  EXT TEMP. "       "   "     "        "     "    

      qh=bmiscd(icf,idtyp,iper,2)
      dtmp=bmiscd(icf,idtyp,iper,3)
      tr=bmiscd(icf,idtyp,iper,4)
      dtoa=bmiscd(icf,idtyp,iper,5)
      timdif=bmiscd(icf,idtyp,iper,6)
      initim=int(bmiscd(icf,idtyp,iper,7))
      if(initim.eq.1)then
         stinit=4.    
      else if(initim.eq.2)then
         stinit=bmiscd(icf,idtyp,iper,8)
      else if(initim.eq.3)then
        a0=bmiscd(icf,idtyp,iper,8)
        a1=bmiscd(icf,idtyp,iper,9)
        a2=bmiscd(icf,idtyp,iper,10)
      endif

C FATAL ERROR TESTS,
      if(int(bmiscd(icf,idtyp,iper,1)).lt.6.or.int(bmiscd(icf,idtyp,iper
     &,1)).gt.9.)goto 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        call edisp(itu,' Entering subroutine BCL15')
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

C SET COMFORT TEMPERATURE UPPER AND LOWER LIMITS;
      thi=dtmp+(0.5*tr)
      tlo=dtmp-(0.5*tr)

C SET TIME INCREMENT,      
      deltat=1./float(ntstep)

      qfut=0.
      tfut=tnp

C DETERMINE SENSED VALUE,
      call cfvar(tctl,IER)
      if(ier.eq.2)then
        return
      endif
      
      if(initim.eq.3)then
C COMPUTE TIME, `DT', NECESSARY TO REACH DESIRED TEMPERATURE LEVEL
C SOURCE: BIRTLES, A.B., AND JOHN, R.W., 1985. " A NEW OPTIMUM START
C         CONTROL ALGORITHM," IN PROC. INT. SYMP. ON CONTROL AND OPERATION
C         OF BUILDING HVAC SYSTEMS, TRODHEIM, SINTEF.
        dt=exp(a0+a1*(tctl-dtmp)+a2*tf)
        stinit=dtoa-dt
      endif

C IF FIRST PASS, SET INITIAL TRIAL START TIME,
      if(lgctin.eq.1.and.irwcnt.eq.0)then
        stpres=stinit
      endif  

C SAVE PRESENT VALUES,
      if(lgctin.eq.1.and.irwcnt.gt.irwcsv)then
         stpas2=stpas1
         stpas1=stpres
         stpres=stfut
         irwcsv=irwcnt
      endif

C REWIND OR CONTINUE SIMULATION ?
C IRWIND = +1 FOR REWIND AND FORWARD ADJUST TRIAL TIME;
C IRWIND = -1 FOR REWIND AND BACKWARD ADJUST TRIAL TIME.
C IRWIND = 99 FOR CONTINUE UNTIL DTAO;
C IRWIND =  0 FOR CONTINUE AFTER DTAO. 
      if(btimef.ge.dtoa)then
         if(tctl.gt.thi)then
           irwind=1
         else if(tctl.lt.tlo)then
           irwind=-1
         else
           irwind=0
         endif
      if(irwfnl.eq.1)irwind=0

C ESTABLISH FUTURE TRIAL START TIME,
        if(irwind.eq.1)then
          if(irwlas.eq.-1)then
            stfut=stpres+0.5*(stpas1-stpres)
          else if(irwlas.ne.-1)then
            if(irwchg.eq.0)then
              stfut=stpres+deltat
            else
              stfut=stpres+0.5*(stpres-stpas1)
            endif
            if(irwlas.eq.1)then
              call eclose(tctl,tctlsv,0.001,close)
              if(close)then
                stfut=stpas1
                irwfnl=1
              endif
            endif
         endif
          
        else if(irwind.eq.-1)then
          if(irwlas.eq.1)then
             stfut=stpres-0.5*(stpres-stpas1)
          else if(irwlas.ne.1)then
            if(irwchg.eq.0)then
              stfut=stpres-deltat
            else
              stfut=stpres-0.5*(stpas1-stpres)
            endif
            if(irwlas.eq.-1)then
              call eclose(tctl,tctlsv,0.001,close)
              if(close)then
                stfut=stpas2
                irwfnl=1
              endif
            endif
          endif
        else if(irwind.eq.0)then

C ACCEPT CURRENT TRIAL TIME AS OPTIMUM START TIME,
          timopt=stpres

        endif
      endif
      
      call eclose(btimef,dtoa,0.001,close)
      if(close)q=0.

C POSSIBLY INJECT FLUX,      
      if(btimef.lt.dtoa)then
      irwind=99
        if(btimef.ge.stpres)then         
          q=bmiscd(icf,idtyp,iper,2)
        else
          q=0.
        endif
      endif

C CONTINUE WITH SIMULATION IF < MINIMUM TEMPERATURE DIFFERENCE
      if((irwind.eq.1.or.irwind.eq.-1).and.abs(stfut-stpas1).lt.
     &    timdif.and.irwlas.ne.0)then
        timopt=stpres
        irwind=0
      endif

C CONTINUE WITH SIMULATION IF `TIMOPT' < 0.0
      if(irwind.eq.-1.and.stfut.lt.0.and.stpres.le.0)then
        irwind=0
        timopt=stpres
      endif

C CONTINUE WITH SIMULATION IF `TIMOPT' > DTOA,
      if(irwind.eq.1.and.stfut.ge.dtoa)then
        irwind=0
        timopt=stpres
      endif

C SET REWIND POSITIVE (IRWCHP), NEGATIVE (IRWCHN), & CHANGE (IRWCHG) FLAGS,
      if(irwind.eq.1)irwchp=1
      if(irwind.eq.-1)irwchn=1
      if(irwchp.eq.1.and.irwchn.eq.1)irwchg=1

      if(btimef.ge.dtoa)then
C REMEMBER PRESENT REWIND FLAG,
        irwlas=irwind
      endif

C SET REWND FLAG
      if(btimef.ge.dtoa.and.(irwind.eq.1.or.irwind.eq.-1))then
        rewnd=.true.
C REMEMBER CURRENT SENSED VALUE,
        tctlsv=tctl
        irwcnt=irwcnt+1
      else
        rewnd=.false.
      endif
      
C RESET VARIABLES,
      if(irwind.eq.0)call mziboc

C CONVERT FROM (/M^2) IF NECESSARY.
      isur=0
      if(iban(icf,1).eq.iicomp.and.iban(icf,2).gt.0)isur=iban(icf,2)
      if(isur.gt.0)q=q/sna(iicomp,isur)
      tfut=(bb3-bb2*q)/bb1
      qfut=q

      iplt=1
      qmx=qh
      qmn=qh
      tcont=tfut
      if(iban(icf,1).eq.-2)call mzrcpl(iicomp,bb1,bb2,bb3,tcont,0.05,
     &iplt,qmx,qmn,tfut,qfut)  

C TRACE OUTPUT,
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(41).ne.0) then
         write(outs,'(A,F10.3,A,F8.3,A,F10.3,A,F10.3,A,F10.3)')
     & ' BCL15: Qfuture=',qfut,' Tfuture=',tfut,
     & ' STFUT=',stfut,' STPRES=',stpres,' TIMOPT=',timopt
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL15')
      endif

      return
   99 call edisp(iuout,' BCL15: DATA INCOMPLETE.')
      call epwait
      call epagend
      stop
      end

C ******************** BCL16
C Logic Optimum Stop Controller.
C Suitable for use with controller types 0,35,36,37,38,39.

      SUBROUTINE BCL16
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/TS6/idysv,ihrsv,irwcou,tsclaw,rewnd,nsncv1,nsncv2,pasone
      COMMON/OPTCTL/IRWIND,IRWLAS,IRWCNT,IRWCSV,IRWCHP,IRWCHN,IRWCHG,
     &LGCTIN,INITCT,STFUT,STPRES,STPAS1,STPAS2,IRWFNL,TCTLSV    
C Where:
C     IRWIND = flag indicating that a time rewind is required.
C     IRWLAS = memory for previous rewind operation,
C     IRWCNT = NUMBER OF REWIND OPERATIONS,
C     IRWCSV=  MEMORY FOR REWIND COUNTER,
C     IRWCHP = FLAG INDICATING A REWIND AND FORWARD ADJUST OPERATION,
C     IRWCHN = FLAG INDICATING A REWIND AND BACKWARD ADJUST OPERATION,
C     IRWCHG = FLAG INDICATING BOTH IRWCHP & IRWCHN POSITIVE,
C     IRWFNL = FLAG INDICATING FINAL REWIND OPERATION,
C     LGCTIN = FLAG INDICATING INTIALISATION TIME,
C     INITCT = INITIALISATION COUNTER,
C     ST???  = FUTURE/PRESENT/START TRIAL START TIMES,

      COMMON/BTIME/BTIMEP,BTIMEF

      LOGICAL tsclaw,rewnd,pasone,close
      
      CHARACTER OUTS*124


C MISC. DATA ITEMS:
C QH     = HEATING CAPACITY (W);
C QC     = COOLING CAPACITY (W);
C HSP    = HEATING SET POINT TEMP (DEG.C);
C CSP    = COOLING SET POINT TEMP (DEG.C);
C DTMP   = DESIRED TEMPERATURE LEVEL (DEG.C);
C TR     = ACCEPTABLE COMFORT TEMPERATURE BAND (DEG.C);
C OCDT   = OCCUPANCY DEPARTURE TIME;
C TIMDIF = MINIMUM DIFFERENCE BETWEEN SUCCESSIVE TRIAL TIMES (HOURS);
C STINIT = INITIAL TRIAL TEST TIME.

      qh=bmiscd(icf,idtyp,iper,2)
      qc=-bmiscd(icf,idtyp,iper,3)
      hsp=bmiscd(icf,idtyp,iper,4)
      csp=bmiscd(icf,idtyp,iper,5)
      dtmp=bmiscd(icf,idtyp,iper,6)
      tr=bmiscd(icf,idtyp,iper,7)
      ocdt=bmiscd(icf,idtyp,iper,8)
      timdif=bmiscd(icf,idtyp,iper,9)
      stinit=bmiscd(icf,idtyp,iper,10)

C FATAL ERROR TEST,
      if(int(bmiscd(icf,idtyp,iper,1)).ne.9)goto 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) call edisp(itu,' Entering subroutine BCL16')

C SET TIME INCREMENT,  
      deltat=1./float(ntstep)

C SET COMFORT TEMPERATURE UPPER AND LOWER LIMITS;
      thi=dtmp+(0.5*tr)
      tlo=dtmp-(0.5*tr)

      qfut=0.
      tfut=tnp

C DETERMINE SENSED TEMPERATURE;
      call cfvar(tctl,IER)
      if(ier.eq.2)then
        return
      endif

C IF FIRST PASS, SET INITIAL TRIAL STOP TIME;
      initct=initct+1
      if(initct.eq.1.and.irwcnt.eq.0)stpres=stinit

C SAVE PRESENT VALUES;
       if(irwcnt.gt.irwcsv)then
         stpas2=stpas1
         stpas1=stpres
         stpres=stfut
         irwcsv=irwcnt
      endif

C ESTABLISH PERIOD START TIME,
      if(initct.eq.1)pertms=btimef

C REWIND OR CONTINUE SIMULATION ?
C IRWIND = +1 FOR REWIND AND FORWARD ADJUST TRIAL TIME;
C IRWIND = -1 FOR REWIND AND BACKWARD ADJUST TRIAL TIME.
C IRWIND = 99 FOR CONTINUE UNTIL OCDT;
C IRWIND =  0 FOR CONTINUE AFTER OCDT. 

      if(btimef.ge.ocdt)then
         if(tctl.gt.thi)then
           irwind=-1
         else if(tctl.lt.tlo)then
           irwind=1
         else
           irwind=0
         endif
      if(irwfnl.eq.1)irwind=0
         
C ESTABLISH FUTURE TRIAL STOP TIME;
         if(irwind.eq.1)then
           if(irwlas.eq.-1)then
              stfut=stpres+0.5*(stpas1-stpres)
           else if(irwlas.ne.-1)then
              if(irwchg.eq.0)then
                stfut=stpres+deltat
              else
                 stfut=stpres+0.5*(stpres-stpas1)
              endif
              if(irwlas.eq.-1)then
                call eclose(tctl,tctlsv,0.001,close)
                if(close)then
                  stfut=stpas2
                  irwfnl=1
                endif
              endif    
           endif 
         else if(irwind.eq.-1)then
           if(irwlas.eq.1)then
              stfut=stpres-0.5*(stpres-stpas1)
           else if(irwlas.ne.1)then
              if(irwchg.eq.0)then
                stfut=stpres-deltat
              else
                stfut=stpres-0.5*(stpas1-stpres)
              endif
              if(irwlas.eq.1)then
                call eclose(tctl,tctlsv,0.001,close)
                if(close)then
                  stfut=stpas1
                  irwfnl=1
                endif
              endif
           endif
         else if(irwind.eq.0)then
C ACCEPT CURRENT TRIAL TIME AS OPTIMUM STOP TIME,
           timopt=stpres
         endif
      endif

C POSSIBLY INJECT FLUX;
      call eclose(btimef,ocdt,0.001,close)
      if(close)q=0.

      if(btimef.lt.ocdt)then
        irwind=99
        if(btimef.ge.stpres)then
C CEASE FLUX INJECTION
          q=0.
        else
C INJECT FLUX: (ALGORITHM IS BASED ON BCL04: `IDEAL FIXED FLUX INJECTION'.)
           if(tctl.lt.csp)then
C TEMP TOO LOW, HEAT;
             q=qh
             iplt=1
             qmx=qh
             qmn=qh
             tcont=hsp
           else if(tctl.gt.hsp)then
C TEMP TOO HIGH, COOL;
             q=qc
             iplt=2
             qmx=qc
             qmn=qc
             tcont=csp
           else
C TEMP WITHIN CONTROL RANGE;
             qfut=0.
             tfut=tnp
           endif
        endif
      endif
      
C CONTINUE WITH SIMULATION IF < MINIMUM TEMPERATURE DIFFERENCE
      if((irwind.eq.1.or.irwind.eq.-1).and.abs(stfut-stpas1).lt.
     &timdif.and.irwlas.ne.0)then
        irwind=0
        if(stfut.lt.stpres)then
          timopt=stpres
        else if(stfut.ge.stpres)then
          stfut=stpres
          irwind=-1
        endif        
      endif

C CONTINUE WITH SIMULATION IF `TIMOPT' < 0.0
      if(irwind.eq.-1.and.stfut.lt.pertms.and.stpres.le.pertms)then
        irwind=0
        timopt=stpres
      endif
            
      if(irwind.eq.1.and.stfut.ge.ocdt)then
C CONTINUE WITH SIMULATION IF `TIMOPT' > OCDT,
        irwind=0
        timopt=stpres
      endif

      if(irwind.eq.0)then
        irwfnl=0
        irwlas=0
        lgctin=0
      endif

      if(irwind.eq.1)irwchp=1
      if(irwind.eq.-1)irwchn=1
      if(irwchp.eq.1.and.irwchn.eq.1)irwchg=1
      if(btimef.ge.ocdt)then
        irwlas=irwind
      endif

C SET `REWIND TIME' FLAG,
      if(btimef.ge.ocdt.and.(irwind.eq.1.or.irwind.eq.-1))then
         rewnd=.true.
C REMEMBER CURRENT SENSED VALUE,
         tctlsv=tctl
         irwcnt=irwcnt+1
      else
         rewnd=.false.
      endif
      
C RESET VARIABLES,
      if(irwind.eq.0)call mziboc

C CONVERT FROM (/M^2) IF NECESSARY.
      isur=0
      if(iban(icf,1).eq.iicomp.and.iban(icf,2).gt.0)isur=iban(icf,2)
      if(isur.gt.0)q=q/sna(iicomp,isur)
      tfut=(bb3-bb2*q)/bb1
      qfut=q

      iplt=1
      qmx=qh
      qmn=qh
      tcont=tfut
      if(iban(icf,1).eq.-2)call mzrcpl(iicomp,bb1,bb2,bb3,tcont,0.05,
     &iplt,qmx,qmn,tfut,qfut)    

C TRACE OUTPUT,
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(41).ne.0) then
         write(outs,'(A,F10.3,A,F10.3)')
     & ' Subroutine BCL16:  Qfuture = ',qfut,' Tfuture = ',tfut
         call edisp(itu,outs)
         call edisp(itu,' ')  
     
         write(outs,'(A,F10.3,A,F10.3,A,F10.3)')
     & ' Subroutine BCL16: STFUT = ',stfut,' STPRES = ',stpres,
     & '  TIMOPT = ',timopt
         call edisp(itu,outs)
         call edisp(itu,' ') 
  
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL16')
         
      endif

      return
   99 call edisp(iuout,' BCL16: DATA INCOMPLETE.')
      call epwait
      call epagend
      stop
      end

C ******************** BCL17

C Fuzzy Logic PI/PD Controller.

      SUBROUTINE BCL17
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/FZCTL1/FZM(MFP,3,5,10),ERRSAV(MFP),QSAV(MFP),
     &ACUT(3,5),FZCTIP(5,5),FZCTOP(5),COA(5)
      COMMON/FZCTL2/NFZF(MFP,3),FZT(MFP,3),NONZRO
      COMMON/FZCTL3/FZLABL,FZLOOK

      PARAMETER (SMALL=1.0E-15)

      CHARACTER*8 FZLABL(MFP,3,5)
      CHARACTER*8 FZLOOK(MFP,5,5)

      CHARACTER OUTS*124

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) CALL EDISP(ITU,' Entering subroutine BCL17')

      QFUT=0.
      TFUT=TNP

C Fatal error tests.
      IF(INT(BMISCD(ICF,IDTYP,IPER,1)).NE.7)GOTO 99

      IF(INT(BMISCD(ICF,IDTYP,IPER,3)).NE.1.AND.
     &INT(BMISCD(ICF,IDTYP,IPER,3)).NE.2)GOTO 97

      IF(INT(BMISCD(ICF,IDTYP,IPER,5)).NE.1.AND.
     &INT(BMISCD(ICF,IDTYP,IPER,5)).NE.2)GOTO 98

C Control parameters are:
C     IFP:=    Fuzzy membership data set.
C     IMO:=    Flag indicating control mode: 1 for PI; 2 for PD.
C     SP:=     Set point (deg. C).
C     IDEFUZ:= Defuzzification mode: 1 for MOM; 2 for COA.
C     SCALER:= Scale factor for error.
C     SCALCE:= Scale factor for change of error.
C     SCALOP:= Scale factor for output.
      
C Assign control parameters.
      IFP=INT(BMISCD(ICF,IDTYP,IPER,2))
      IMO=INT(BMISCD(ICF,IDTYP,IPER,3))
      SP=BMISCD(ICF,IDTYP,IPER,4)
      IDEFUZ=INT(BMISCD(ICF,IDTYP,IPER,5))
      SCALER=BMISCD(ICF,IDTYP,IPER,6)
      SCALCE=BMISCD(ICF,IDTYP,IPER,7)
      SCALOP=BMISCD(ICF,IDTYP,IPER,8)

C Establish max and min flux outputs.
      QMIN=SCALOP*FZM(IFP,3,1,3)
      QMAX=SCALOP*FZM(IFP,3,NFZF(IFP,3),3)

C (Re)Initialise fuzzy variables.
       CALL FZINIT

C Determine sensed temperature.
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C Determine inputs: error and change of error.
      DO 30 I=1,2
        IF(I.EQ.1)THEN
          FZERR=TCTL-SP
          FZERR=SCALER*FZERR
        ELSE
          FZERR=SCALCE*((TCTL-SP)-ERRSAV(IFP))
        ENDIF

C Compute Alpha-Cut.
        DO 25 J=1,NFZF(IFP,I)
          IF((J.EQ.1.AND.FZERR.LT.FZM(IFP,I,J,3)).OR.
     &       (J.EQ.NFZF(IFP,I).AND.FZERR.GT.FZM(IFP,I,J,3)))THEN
             ACUT(I,J)=1.
          ELSEIF(FZERR.GE.FZM(IFP,I,J,1).AND.FZERR.LE.FZM(IFP,I,J,3))
     &    THEN
C Acceleration line cut.
             ACUT(I,J)=FZCUT(FZM(IFP,I,J,7),FZM(IFP,I,J,8),FZERR)
          ELSEIF(FZERR.GT.FZM(IFP,I,J,3).AND.FZERR.LE.FZM(IFP,I,J,5))
     &    THEN
C Deceleration line cut.
            ACUT(I,J)=FZCUT(FZM(IFP,I,J,9),FZM(IFP,I,J,10),FZERR)
          ELSE
C Rule not fired.
            ACUT(I,J)=0.
          ENDIF
  25    CONTINUE
30    CONTINUE

C Set up Cut Input array.
      DO 40 J=1,NFZF(IFP,1)
        DO 35 L=1,NFZF(IFP,2)
           FZCTIP(J,L)=FZMIN(ACUT(1,J),ACUT(2,L))
  35    CONTINUE
40    CONTINUE

C Set up Cut Output array.
      DO 50 J=1,NFZF(IFP,3)
        DO 44 L=1,NFZF(IFP,1)
          DO 42 K=1,NFZF(IFP,2)
           IF(FZLOOK(IFP,L,K).EQ.FZLABL(IFP,3,J).AND.FZCTIP(L,K).GT.0.)
     &      THEN
              IF(FZCTOP(J).LT.SMALL)THEN
                 FZCTOP(J)=FZCTIP(L,K)
              ELSE
                 FZCTOP(J)=FZMAX(FZCTOP(J),FZCTIP(L,K))
              ENDIF
           ENDIF
   42     CONTINUE
  44    CONTINUE
50    CONTINUE

C Determine De-fuzzified Output.

C MOM defuzzification method selected by user.
      IF(IDEFUZ.EQ.1)THEN
         PEAKMX=0.
         PEAKS=1.
         DO 60 J=1,NFZF(IFP,3)
            IF(FZCTOP(J).GT.PEAKMX)THEN
               PEAKMX=FZCTOP(J)
            ELSEIF(ABS(FZCTOP(J)-PEAKMX).LT.SMALL.AND.PEAKMX.GT.SMALL)
     &          THEN
               PEAKS=PEAKS+1.
            ENDIF
60       CONTINUE

         TOTAL=0.
         DO 65 J=1,NFZF(IFP,3)
            IF(FZCTOP(J).GT.0..AND.FZCTOP(J).GE.PEAKMX)THEN
               IF(J.EQ.1.OR.J.EQ.NFZF(IFP,3))THEN
                  IF(ABS(FZCTOP(J)-1.).LT.SMALL)THEN
                     TOTAL=TOTAL+FZM(IFP,3,J,3)
                  ELSE
                     IF(J.EQ.1)THEN
                        PT1=FZM(IFP,3,J,3)
                        PT2=XCROSS(FZCTOP(J),FZM(IFP,3,J,9),
     &                             FZM(IFP,3,J,10))
                      ELSEIF(J.EQ.NFZF(IFP,3))THEN
                        PT1=XCROSS(FZCTOP(J),FZM(IFP,3,J,7),
     &                             FZM(IFP,3,J,8))
                        PT2=FZM(IFP,3,J,3)
                      ENDIF
                      PT3=PT1+((PT2-PT1)*0.5)
                      TOTAL=TOTAL+((PT1+PT2)*0.5)
                  ENDIF
               ELSE
                  IF(ABS(FZCTOP(J)-1.).LT.SMALL)THEN
                     TOTAL=TOTAL+FZM(IFP,3,J,3)
                  ELSE
                     PT1=XCROSS(FZCTOP(J),FZM(IFP,3,J,7),
     &                                    FZM(IFP,3,J,8))
                     PT2=XCROSS(FZCTOP(J),FZM(IFP,3,J,9),
     &                                    FZM(IFP,3,J,10))
                     PT3=PT1+((PT2-PT1)*0.5)
                     TOTAL=TOTAL+((PT1+PT2+PT3)/3.)
                  ENDIF
               ENDIF
            ENDIF
65       CONTINUE

         IF(PEAKS.GT.0)THEN
            FZCTL=TOTAL/PEAKS
         ELSE
            FZCTL=0.
         ENDIF

C COA defuzzification method selected by user.
      ELSE

C Initialise 'Number of non-zero Ouput Set' counter.
         NONZRO=0

C First require centroids of all non-zero cut output sets.
         DO 70 J=1,NFZF(IFP,3)

C Alpha Cut = 0.
            IF(FZCTOP(J).LT.SMALL)GOTO 70
            NONZRO=NONZRO+1

C Alpha Cut = 1: 'Full triangle' COA.
            IF(ABS(FZCTOP(J)-1.).LT.SMALL)THEN
               IF(J.EQ.1)THEN
                   COA(J)=FZM(IFP,3,J,3)
               ELSEIF(J.EQ.NFZF(IFP,3))THEN
                   COA(J)=FZM(IFP,3,J,3)
               ELSE
                  A=FZM(IFP,3,J,5)-FZM(IFP,3,J,1)
                  B=FZM(IFP,3,J,3)
                  COA(J)=FZM(IFP,3,J,1)+((A+B)/3)
               ENDIF

C Alpha Cut < 1: 'Trapezoidal' COA.
            ELSEIF(FZCTOP(J).LT.1.)THEN
               IF(J.EQ.1)THEN
                  COAX1=FZM(IFP,3,J,3)
                  COAY1=FZCTOP(J)
                  COAX2=XCROSS(FZCTOP(J),FZM(IFP,3,J,9),FZM(IFP,3,J,10))
                  COAY2=FZCTOP(J)
C COAX3 = point half-way up slope.
                  COAX3=((FZM(IFP,3,J,5)-COAX2)/2.)+COAX2
                  COAY3=YCROSS(FZM(IFP,3,J,9),COAX3,FZM(IFP,3,J,10))
                  COA(J)=((COAX1*COAY1)+(COAX2*COAY2)+(COAX3*COAY3))/
     &                    (COAY1+COAY2+COAY3)
               ELSEIF(J.EQ.NFZF(IFP,3))THEN                        
                  COAX2=XCROSS(FZCTOP(J),FZM(IFP,3,J,7),FZM(IFP,3,J,8))
                  COAY2=FZCTOP(J)
C COAX1= point half-way up slope.
                  COAX1=((COAX2-FZM(IFP,3,J,1))/2.)+FZM(IFP,3,J,1)
                  COAY1=YCROSS(FZM(IFP,3,J,7),COAX1,FZM(IFP,3,J,9))
                  COAX3=FZM(IFP,3,J,3)
                  COAY3=FZCTOP(J)
                  COA(J)=((COAX1*COAY1)+(COAX2*COAY2)+(COAX3*COAY3))/
     &                    (COAY1+COAY2+COAY3)
               ELSE

                  COAX2=XCROSS(FZCTOP(J),FZM(IFP,3,J,7),FZM(IFP,3,J,8))
C COAX1= point half-way up slope.
                  COAX1=((COAX2-FZM(IFP,3,J,1))/2.)+FZM(IFP,3,J,1)
                  COAY1=YCROSS(FZM(IFP,3,J,7),COAX1,FZM(IFP,3,J,8))
                  COAY2=FZCTOP(J)

                  COAX3=XCROSS(FZCTOP(J),FZM(IFP,3,J,9),FZM(IFP,3,J,10))
                  COAY3=FZCTOP(J)
C COAX4= point half-way up slope.
                  COAX4=((FZM(IFP,3,J,5)-COAX3)/2.)+COAX3
                  COAY4=YCROSS(FZM(IFP,3,J,9),COAX4,FZM(IFP,3,J,10))

                  COA(J)=((COAX1*COAY1)+(COAX2*COAY2)+(COAX3*COAY3)+
     &                    (COAX4*COAY4))/(COAY1+COAY2+COAY3+COAY4)

               ENDIF
            ENDIF
70        CONTINUE
C Now find overall centroid == Fuzzy Logic Controller Output.
          COATOT=0.
          DO 80 J=1,NFZF(IFP,3)
             COATOT=COATOT+COA(J)
80        CONTINUE

          IF(NONZRO.GT.0)THEN
             FZCTL=COATOT/NONZRO
          ELSE
              FZCTL=0.
          ENDIF
      ENDIF

C Assign Control Data.
      Q=FZCTL
      IF(IMO.EQ.1)Q=FZCTL+QSAV(IFP)
      Q=Q*SCALOP
      IF(Q.LT.QMIN)Q=QMIN
      IF(Q.GT.QMAX)Q=QMAX

C Convert from (/m^2) if necessary.
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      TCONT=TFUT 
      iplt=1
      qmx=qmax
      qmn=qmin 

C Deal with mixed actuator.    
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Remember error.
      ERRSAV(IFP)=TCTL-SP

C Remember output.

      QSAV(IFP)=Q

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         WRITE(ITU,'(A,F10.3,A,F10.3)') ' Qfuture = ',QFUT,
     &                                   ' Tfuture = ',TFUT
         CALL EDISP(ITU,OUTS)
         CALL EDISP(ITU,' Leaving subroutine BCL17')
      END IF

      RETURN

97    CALL EDISP(iuout,' BCL17: Incorrect value for PI/PD flag.')
      CALL EPWAIT
      CALL EPAGEND
      STOP

98    CALL EDISP(iuout,' BCL17: Incorrect value for defuzzn flag.')
      CALL EPWAIT
      CALL EPAGEND
      STOP

99    CALL EDISP(iuout,' BCL17: incorrect no. of misc. data items.')
      CALL EPWAIT
      CALL EPAGEND
      STOP

      END

C ******************** BCL18

C 'BCL18' is a null controller. The controller output
C is identically the same as the sensor input.

      SUBROUTINE BCL18
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      
      character outs*124

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) call edisp(itu,' Entering subroutine BCL18')

      QFUT=0.
      TFUT=TNP

C Determine sensed condition.
      CALL CFVAR(SENVAL,IER)
      if(ier.eq.2)then
        return
      endif

C Set output flux.
      Q=SENVAL

      QMX=Q
      QMN=0
      IPLT=1
      TCONT=SENVAL

C Convert from (/m^2) if necessary.
      ISUR=0
      IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
      IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,F10.3)') ' Qfuture = ',QFUT,
     &                                   ' Tfuture = ',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL18')
      END IF

      RETURN
      END

C ******************** bcl19

C This controller will activate heating/cooling to achieve
C a desired temperature in the associated zone but it will
C only do so based on the output of multiple sensors;
C i.e. if any of the auxilliary sensors demands heating for
C the auxiliary zone then heating can be ON in current zone,
C if any of the auxiliary sensors demands cooling for the
C auxiliary zone then cooling can be ON in current zone.
C Each auxiliary sensor has it's own heating/cooling set-points.
C In the event of simultaneous heating and cooling in the
C associated zone due to the demands of the auxiliary
C sensors, no heating nor cooling will be applied, and a
C warning message will be generated.
C This control law is suitable for a type 0 controller.

      SUBROUTINE BCL19
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/CMSVAR/NSEN(MCF),ISMOD(MCF),IBMSN(MCF,MSEN,4),
     &TAUXSN(MCF,MSEN)
     
      character outs*124

      DIMENSION AUXHSP(MSEN),AUXCSP(MSEN)
      
C Fatal error tests:
      ICTYP=IBCTYP(ICF,IDTYP,IPER)
      IF(ICTYP.NE.0.AND.(ICTYP.LT.35.OR.ICTYP.GT.39))then
        call edisp(iuout,' BCL19: controller type error.')
        call epwait
        call epagend
        STOP
      endif

      IF(INT(BMISCD(ICF,IDTYP,IPER,1)).GT.MISC)then
        call edisp(iuout,' BCL19: too many misc data items.')
        call epwait
        call epagend
        STOP
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) call edisp(itu,' Entering subroutine BCL19')

      QFUT=0.
      TFUT=TNP

C Determine temperature in associated zone.
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C QHM = max heating capacity (W)
C QHN = min heating capacity (W)
C QCM = max cooling capacity (W)
C QCN = min cooling capacity (W)
C HSP = heating set point temp. (deg.C)
C CSP = cooling set point temp. (deg.C)

      QHM=BMISCD(ICF,IDTYP,IPER,2)
      QHN=BMISCD(ICF,IDTYP,IPER,3)
      QCM=-BMISCD(ICF,IDTYP,IPER,4)
      QCN=-BMISCD(ICF,IDTYP,IPER,5)
      HSP=BMISCD(ICF,IDTYP,IPER,6)
      CSP=BMISCD(ICF,IDTYP,IPER,7)
      IF(TCTL.LE.CSP.AND.TCTL.GE.HSP)goto 1000

C NSEN(ICF) = number of auxiliary sensors for control function.
      NSEN(ICF)=INT(BMISCD(ICF,IDTYP,IPER,8))

C Fatal error test:      
      IF(NSEN(ICF).GT.MSEN)then
        call edisp(iuout,' BCL19: too many auxiliary sensors.')
        call epwait
        call epagend
        STOP
      endif

      IF(INT(BMISCD(ICF,IDTYP,IPER,1)).NE.(7+6*NSEN(ICF)))then
        call edisp(iuout,' BCL19: incorrect no of misc data items.')
        call epwait
        call epagend
        STOP
      endif

      DO 10 L=1,NSEN(ICF)           
        IBMSN(ICF,L,1)=INT(BMISCD(ICF,IDTYP,IPER,9+6*(L-1)))
        IBMSN(ICF,L,2)=INT(BMISCD(ICF,IDTYP,IPER,10+6*(L-1)))
        IBMSN(ICF,L,3)=INT(BMISCD(ICF,IDTYP,IPER,11+6*(L-1)))
        IBMSN(ICF,L,4)=INT(BMISCD(ICF,IDTYP,IPER,12+6*(L-1)))
        AUXHSP(L)=BMISCD(ICF,IDTYP,IPER,13+6*(L-1))
        AUXCSP(L)=BMISCD(ICF,IDTYP,IPER,14+6*(L-1))
 10   CONTINUE
      
C Start with assuming that neither heating nor cooling
C of the current zone is needed, and then
C determine auxiliary sensor temperatures (or outdoor conditions).
      IHEATING=0
      ICOOLING=0
      CALL CFMVAR(ier)
      if(ier.eq.2)then
        return
      endif
 
C Determine for each auxiliary sensor's zone whether
C heating or cooling is demanded.
      DO 20 L=1,NSEN(ICF)
         AUXSEN=TAUXSN(ICF,L)
         IF(AUXSEN.LT.AUXHSP(L)) IHEATING=1
         IF(AUXSEN.GT.AUXCSP(L)) ICOOLING=1
 20   CONTINUE

C Check whether neither heating nor cooling required.
      IF(IHEATING.EQ.0.AND.ICOOLING.EQ.0)goto 1000

C For the case of simultaneous heating and cooling
C demand of auxiliary zones, do not control the
C current zone, and issue a warning.
      IF(IHEATING.EQ.1.AND.ICOOLING.EQ.1)THEN
         CALL EDISP(IUOUT,' BCL19: simultaneous heating and cooling')
         goto 1000
      ENDIF

C Since either heating or cooling is required by the auxiliary
C zones and the sensed temperature of the current zone is not
C within range, attempt to control this zone's temperature.
      TCTL=TNP

C If heating required an aux zone and this zone is below its setpoint
C then attempt to heat this zone to its heating setpoint.
      IF(IHEATING.EQ.1.and.TCTL.LT.HSP)then
        IPLT=1
        QMX=QHM
        QMN=QHN
        TCONT=HSP

C Too low, heat to HSP.
        Q=(BB3-BB1*HSP)/BB2

C Convert from (/m^2) if necessary.
        QQ=Q
        ISUR=0
        IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
        IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)

C Is this capacity available ?
        IF(QQ.LE.QHM.AND.QQ.GE.QHN)GOTO 2
        IF(QQ.GT.QHM)GOTO 3
        Q=QHN
        IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
        GOTO 2
    3   Q=QHM
        IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
        GOTO 2
      endif

C If auxiliary zones demand cooling and actuation zone is > cooling
C set point then Temp. too high, cool to CSP.
      if(ICOOLING.EQ.1.and.TCTL.GT.CSP)then
        Q=(BB3-BB1*CSP)/BB2
        IPLT=2
        QMX=QCM
        QMN=QCN
        TCONT=CSP

C Convert from (/m^2) if necessary.
        QQ=Q
        ISUR=0
        IF(IBAN(ICF,1).EQ.IICOMP.AND.IBAN(ICF,2).GT.0)ISUR=IBAN(ICF,2)
        IF(ISUR.GT.0)QQ=Q*SNA(IICOMP,ISUR)

C Is this available ?
        IF(QQ.GE.QCM.AND.QQ.LE.QCN)GOTO 2
        IF(QQ.LT.QCM)GOTO 5
        Q=QCN
        IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
        GOTO 2
    5   Q=QCM
        IF(ISUR.GT.0)Q=Q/SNA(IICOMP,ISUR)
      endif

    2 TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q
      IF(IBAN(ICF,1).EQ.-2)CALL MZRCPL(IICOMP,BB1,BB2,BB3,TCONT,0.05,
     &IPLT,QMX,QMN,TFUT,QFUT)

C Trace output.
 1000 IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         write(outs,'(A,F10.3,A,2F10.3)')' Local sensed temp = ',TCTL,
     &                                   ' Local setpoints = ',HSP,CSP
         call edisp(itu,outs)
         write(outs,'(A,F10.3)')' Aux sensed value = ',TAUXSN(ICF,1)
         call edisp(itu,outs)
         write(outs,'(A,I3,A,I3)') ' Aux heat yes/no = ',IHEATING,
     &                             ' Aux cool yes/no = ',ICOOLING
         call edisp(itu,outs)
         write(outs,'(A,F10.3,A,F10.3)') ' Qfuture = ',QFUT,
     &                                   ' Tfuture = ',TFUT
         call edisp(itu,outs)
         call edisp(itu,' Leaving subroutine BCL19')
      END IF
     
      RETURN
      END

C ******************** BCL20
C Encapsulation of an evaporative source (such as a swimming pool or
C a wetted surface. The sensor location is used to define the zone
C and surface. It has two operational modes:
C a) a wetted surface, no additional data required
C b) a swimming pool, 9 additional data items required.
c
C The driving equation for evaporation is:
C Ev=alpha.Abasin.(x` - xi)/cp (kg/s).
C
C  Where:
C  alpha is the surface convective heat transfer coefficient
C  Abasin is the surface area (m^2)
C  Tbas is the surface temperature (C).
C  cp the specific heat of air using SPHTC(GS,TD)
C  where GS is zone moisture content x` and zone temperature TD
C  x' is the maximum moisture content at the water surface temperature
C  xi is the indoor moisture content
C  x' is calculated using HUMRT1(TS,RH,PATMOS,IOPT) TS is the surface
C     temperature and RH is set to 100. PATMOS=1013.25.

C The operational modes are defined by BMISCD(ICF,IDTYP,IPER,2)
C if 0 for a wetted surface 1 for a swiming pool.

      SUBROUTINE BCL20
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2.
#include "geometry.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/FILEP/IFIL
C      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/ATPRES/PATMOS

C In support of ZRH calculations.
      common/concoe/hcip(mcom,ms),hcif(mcom,ms),hcop(mcom,ms),
     &              hcof(mcom,ms)
      common/fvalg/gfa(mcom)

C Evaporation common block for use in MZVAPC.
      common/evapsur/vapsur(mcom)

      CHARACTER OUTS*124
      logical unixok

C Fatal error test.
c      IF((BMISCD(ICF,IDTYP,IPER,1).NE.1.0).OR.
c     &(BMISCD(ICF,IDTYP,IPER,1).NE.10.0))GOTO 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) call edisp(itu,' Entering subroutine BCL20')

      QFUT=0.
      TFUT=TNP

C Setup variables
      icomp=IBSN(ICF,1)
      isur= IBSN(ICF,2)
      vapsur(icomp)=0.0
      TD = TFA(icomp)
      GZ= GFA(icomp)
      ZRH=PCRH2(TD,GZ,PATMOS)

C Currently assume that caual and solar gains to the pool are zero
      Qcasual=0.
      Qsolar=0.

C I mode = 0 then uses esp-r data if 1 then user specified.
      mode=int(BMISCD(ICF,IDTYP,IPER,2))
      if(mode.eq.0)then
        Abas = SNA(icomp,isur)
        Tbas = TFS(icomp,isur)
        alpha = hcif(icomp,isur)
C Wet opaque surface calculation for vapour released into the space
        wi=gfa(icomp)
        CP= SPHTC2(TD,wi)*1000
        hundred=100.0
        w=HUMRT1(Tbas,hundred,PATMOS,0)
        Ev= alpha * Abas * (w - wi)/cp
        vapsur(icomp)=Ev

C Calculate the information required for a swimming pool. 
C Use the poolbal calculation to calculate the pool energy requirements
      elseif(mode.eq.1)then
        Abas = BMISCD(ICF,IDTYP,IPER,3)

C Emissivity of the surface should have been set to zero for the model

        alpha= BMISCD(ICF,IDTYP,IPER,4)
        alphar= BMISCD(ICF,IDTYP,IPER,5)    
        tbas=  BMISCD(ICF,IDTYP,IPER,6)
        tbasex= BMISCD(ICF,IDTYP,IPER,7)
        fsupply= BMISCD(ICF,IDTYP,IPER,8)
        tsupply= BMISCD(ICF,IDTYP,IPER,9)  
        abasw= BMISCD(ICF,IDTYP,IPER,10)  
        ubasw= BMISCD(ICF,IDTYP,IPER,11) 
C Calculate the vapour into the space (kg/s)
C Use zone moisture content (xi), x at saturation to evaluate Ev.  
        wi=gfa(icomp)
        CP= SPHTC2(TD,wi)*1000
        hundred=100.0
        w=HUMRT1(Tbas,hundred,PATMOS,0)
        Ev= alpha * Abas * (w - wi)/cp
        vapsur(icomp)=Ev

        hvap=2550E3
        Hbas=113E3
        cw=4180.

C Read in surface details and calculate the area weighted radiant temperature. 
        call georead(ifil+1,lgeom(icomp),icomp,0,itu,ier)
        tsurf=0.
        do 20 isurf=1,nsur
          if(isurf.ne.isur)then
            tsurf=tsurf+SNA(icomp,isurf)*TFS(icomp,isurf)
            atot=atot+SNA(icomp,isurf)
          endif
  20    continue
        tsurf=tsurf/atot

C Pool basin energy balance:
        Qconv=alpha*Abas*(Tbas-TD)
        Qrad=alphar*Abas*(Tbas-Tsurf)
        Fevap=Ev
        Qevap=Fevap*(Hvap-Hbas)
        Qtrans=Ubasw*Abasw*(Tbas-Tbasex)
        Qsupply=Fsupply*cw*(Tbas-Tsupply)
        Qloss=Qconv+Qrad+Qevap+Qtrans+Qsupply
        Qbas=Qloss-Qcasual-Qsolar

C Transfer the convective and radiant heat transfers associated with the pool to the
C rest of the building model.
        QFUT=Qconv+Qrad
 
      endif

C Dump output
      call isunix(unixok)
      if(mode.eq.1.and.unixok)then
        write(6,*) ' Pool balance '
        write(6,*) ' t       ',IHRF,' (h)'
        write(6,*) ' Tair    ',TD,' (C)'
        write(6,*) ' g       ',gfa(icomp),' (kg/kg)'
        write(6,*) ' evapo   ', Ev,' (kg) '
        write(6,*) ' Qconv   ',Qconv,' (W)'
        write(6,*) ' Qrad    ',Qrad,' (W)'
        write(6,*) ' Qevap   ',Qevap,' (W)'
        write(6,*) ' Qtrans  ',Qtrans,' (W)'
        write(6,*) ' Qsupply ',Qsupply,' (W)'
        write(6,*) ' Qloss   ',Qloss,' (W)'
        write(6,*) ' Qbas    ',Qbas,' (W)'
        write(6,*) ' ----------------------'
      endif

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
        write(outs,'(a,f9.3,a,f9.4,a,f9.3,a,f5.1)') ' Zone db T(C):',
     &    TD,' moisure: ',wi,' cp:',cp,' RH: ',ZRH
        call edisp(itu,outs)
        write(outs,'(a,f9.3,a,f9.4,a,f9.4)') ' Surface hc: ',alpha,
     &    ' w` moisture ',w,' Evaporation (kg/s): ',Ev
        call edisp(itu,outs)
        call edisp(itu,' Leaving subroutine BCL20')
      endif

      return

C The fatal error test commented out so this block is as well.
C   99 call edisp(iuout,' BCL20: data incomplete.')
C      call epwait
C      call epagend
C      STOP
      END

C ******************** BCL21
C BCL21 - Slave Capacity Controller
C BCL21 - control acts to sense in master zone and to deliver capacity
C to slave zone (to be based on fraction of max capacity and to 
C deliver proportionally). 

      SUBROUTINE BCL21

#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/SLAVE1/QHB(MCF),QCB(MCF),Qmst(MCF)

C Commons for identifying wether a zone is heated or cooled.
      common/H3KReportsControl/bZoneHeated,   bZoneCooled,
     &                         fHeatSetpoint, fCoolSetpoint
      logical bZoneHeated(MCOM), bZoneCooled(MCOM)
      real fHeatSetpoint(MCOM), fCoolSetpoint(MCOM)
      integer iMasterZone
      character outs*124  
      logical close

C Determine sensed temperatures.
      call CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0)then
        if (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).eq.0) then
          write(outs,'(a,i2)') 
     &      'Entering subroutine BCL21 with master zone ',
     &      IBSN(ICF,1)
          call edisp(itu,outs)
        elseif (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).GT.0) then
          write(outs,'(a,i2,a,i2)') 
     &      'Entering subroutine BCL21 with master zone ',
     &      IBSN(ICF,1),' and slave zone ',IBAN(ICF,1)
          call edisp(itu,outs)
        else
          call edisp(itu,' Entering subroutine BCL21')
        endif
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

C A model might have several sets of master/slave zones, 
C Qmst() - delivered master capacity 
C Qslv - delivered slave capacity (if current function is a slave)
C QHB(),QCB() -master heating/cooling base capacities
       
C Current Q delivery to current master zone? What is this as % of max?
C Send same % of max to slave zone
            
C Initialise Qmst
      if (IBSN(ICF,1).GT.0) then

C sensor location = 'in another zone' i.e. Master Zone. Recover
C which control law is the master control for that zone.
        imasctlindex= int(BMISCD(ICF,IDTYP,IPER,2))
        iMasterZone = IBSN(imasctlindex, 1)
        QM=Qmst(imasctlindex)
      elseif (IBSN(ICF,1).EQ.0) then
        call edisp(itu, 
     &   ' BCL21: Sensor must be placed in master zone')
        call epwait
        call epagend
        STOP      
      endif

C Collect heating flags and setpoints from master zone
      bZoneHeated(iicomp)   = bZoneHeated(iMasterZone)
      bZoneCooled(iicomp)   = bZoneCooled(iMasterZone)
      fHeatSetpoint(iicomp) = fHeatSetpoint(iMasterZone)
      fHeatSetpoint(iicomp) = fHeatSetpoint(iMasterZone)

      if (QM.GT.0.0) then

C PBH - Percentage of Base Capacity delivered to master zone, heating:
        PBH=QM/QHB(imasctlindex)
      elseif (QM.LT.0.0) then

C PBC - Percentage of Base Capacity delivered to master zone, cooling:
        PBC=QM/QCB(imasctlindex)
      else
        PBC=0.0
      endif
      
C QHM = base (i.e max) heating capacity (W) for slave
C QCM = base (i.e max) cooling capacity (W) for slave

C If the zone associated with this control function is served by an HVAC system
C simulated ideally, then use HVAC system heating capacity to update controller
C heating capacity for time step 
      if(cont_fun_heat_cap(IICOMP).gt.0.) then
        QHM=cont_fun_heat_cap(IICOMP)
      else        
        QHM=BMISCD(ICF,IDTYP,IPER,3)
      endif
      
C If the zone associated with this control function is served by an HVAC system
C simulated ideally, then use HVAC system capacity to update controller
C cooling capacity for time step 
      if(cont_fun_cool_cap(IICOMP).gt.0.) then
        QCM=-cont_fun_cool_cap(IICOMP)
      else        
        QCM=-BMISCD(ICF,IDTYP,IPER,4)
      endif

C Using same % value, calculate delivered capacity to slave zone
      if (QM.GT.0.0) then
        Qslv=PBH*QHM
      elseif (QM.LT.0.0) then
        Qslv=PBC*QCM
      endif

C If close to zero then reset to zero.
      call eclose(QM,0.00,1.00,close)
      if(close)then
        Qslv=0.0
      endif

C Check Qslv not greater than specified base capacities
      if (Qslv.GT.QHM)then
        Qslv=QHM
      elseif (Qslv.LT.QCM)then
        Qslv=QCM
      endif
  
      QFUT=Qslv       
   
C Determine temperatures in slave zone
      Tslv=(BB3-BB2*QFUT)/BB1
      
C If zero capacity to zone, use TNP
      call eclose(QM,0.00,0.1,close)
      if(close)then
        TFUT=TNP
      else
        TFUT=Tslv
      endif

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
        write(outs,'(a,f9.2,a,f5.3,a,f5.3,a,f9.2,a,f9.2)')' Q master:',
     &   QM,' %heat cap: ',PBH,' %cool cap ',PBC,' Q slave:',Qslv,
     &   ' QFUT (slave): ',QFUT
        call edisp(itu,outs)
        call edisp(itu,'  ')
      endif   
            
      end

C ******************** BCL22
C 'BCL22' is a variable supply volume cooling control which
C reverts to a CAV system using reheat for heating mode.

C << the energy balance reported in esrures is off a bit >>
      SUBROUTINE BCL22
#include "building.h"
#include "control.h"
      
      integer lnblnk  ! function definition

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/MBINFO/ZMBI(MCOM,4)
      COMMON/CLIMHG/HEXTP,HEXTF,GEXTP,GEXTF

C Mode indicates if in VAV or CAV or reheat.
      character outs*144,mode*16
      real tswitchvavcav    ! The control temp for VAV/CAV switch
      real prefafr          ! The inital preference for flow rate
      logical vavok         ! Set true if in VAV mode

C Fatal error test if there are less than ?? misecel items.
      IF(BMISCD(ICF,IDTYP,IPER,1).LT.6.)GOTO 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        call edisp(itu,' Entering subroutine BCL22')
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

      mode = ' '
      QFUT=0.
      TFUT=TNP
      TCTL=TNP
      QReheat = 0.0
      tswitchvavcav = 0.0
      prefafr = 0.0
      IF(IBSN(ICF,1).NE.-2)GOTO 101
      IZ=IBSN(ICF,2)
      CALL MZMIXT(IZ,TMRT,TCTL)

  101 IF(IBSN(ICF,1).EQ.-3.AND.IBSN(ICF,2).EQ.0)TCTL=TF
      IF(IBSN(ICF,1).EQ.-3.AND.IBSN(ICF,2).EQ.1)TCTL=SOLAIR(TF,QFF,QDF)

C AFR is computed air volume flow rate (m3/s)
C AIRCAP is air heat capacity fixed at J/kg K.
      AIRCAP = 1006.0

C QRHMAX is reheat maximum capacity (W)
      QRHMAX=BMISCD(ICF,IDTYP,IPER,2)

C TSUPLY is air supply temperature (deg.C)
      TSUPLY=BMISCD(ICF,IDTYP,IPER,3)

C SP is desired set point temp. (deg.C)
      SP=BMISCD(ICF,IDTYP,IPER,4)

C AFRMAX is maximum air volume flow rate (m3/s)
C AFRMIN is minimum air volume flow rate (m3/s)
      AFRMAX=BMISCD(ICF,IDTYP,IPER,5)
      AFRMIN=BMISCD(ICF,IDTYP,IPER,6)

C Is temperature above the cooling setpoint? Remember what TCTL
C is for reporting in the trace.
      tswitchvavcav=TCTL
      IF(TCTL.LE.SP)then

C Too low, revert to CAV system at minimum flow rate.
C TS is the temp we need to have entering the room to get to
C the desired set point temperature.
        vavok = .false.        ! set toggle so trace correct
        AFR = AFRMIN           ! set to minimum flow rate
        prefafr = AFR
        D=AFR*1.2*AIRCAP
        TS=(BB3-(BB1-(BB2*D))*SP)/(BB2*D)  ! as in bcl08

C Calculate QReheat and test against QRHMAX.
        QReheat = AFR*1.2*AIRCAP * (TS - TSUPLY)
        if(QReheat.gt.QRHMAX)then

C If QReheat is greater than QRHMAX then constrain and calculate
C TS TFUT and QFUT.
          write(mode,'(a)') 'CAV reheat lim '
          QReheat = QRHMAX
          TS = (QReheat / (AFR*1.2*AIRCAP)) + TSUPLY
          TFUT=(BB3-BB2*D*TS)/(BB1-BB2*D)   ! as in bcl08
          QFUT= (D*(TS-TFUT))   ! as in bcl08
        else

C QReheat was allowable so calculate TS TFUT and QFUT
          write(mode,'(a)') 'CAV reheat std '
          TS = (QReheat / (AFR*1.2*AIRCAP)) + TSUPLY
          TFUT=(BB3-BB2*D*TS)/(BB1-BB2*D)   ! as in bcl08
          QFUT= (D*(TS-TFUT))   ! as in bcl08
        endif
      else

C Guess that the future temperature will be high if nothing done
C so establish air supply rate to cool air to set point (SP)
        vavok = .true.        ! set toggle so trace correct
        DT = TSUPLY - SP
        TS = TSUPLY       ! TS not used for VAV set to TSUPLY for trace.
        D = (BB3-(BB1*SP))/ (BB2*DT)   ! this is mcp for the air supplied to space
        AFR = D/(AIRCAP*1.2)    ! the volume flow rate needed m^3/s
        prefafr = AFR           ! remember this for trace

C Is this supply flow rate within the limits.
        write(mode,'(a)')   'VAV mode float '
        if(AFR.lt.AFRMIN)then
          AFR = AFRMIN
          write(mode,'(a)') 'VAV mode min f '
        elseif(AFR.gt.AFRMAX)then
          AFR = AFRMAX
          write(mode,'(a)') 'VAV mode max f '
        endif

        TFUT=(BB3-(BB2*1.2*AFR*AIRCAP*DT))/BB1
        QFUT=1.2*AFR*AIRCAP*(TSUPLY-TFUT)
      endif

C Enter gain due to mechanical system here
C mechanical system mass flow rate * moisture content.
C GEXTF the future ambient humidity ratio for the
C assumed outside air part of the supply.
      ZMBI(IICOMP,3)=AFR*1.2*GEXTF

C Total mass flow rate lost from the zone (no moisture included).
      ZMBI(IICOMP,4)=ZMBI(IICOMP,4)+AFR*1.2

C Trace output format depending on whether VAV or CAV active.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
         if(vavok)then
           write(outs,'(A,A,F9.1,4(A,F7.3),2(A,F10.3))') 
     &       mode(1:lnblnk(mode)),' Qfut=',QFUT,
     &       ' Tfut=',TFUT,' SetPt=',SP,' TS=',TS,
     &       ' Tsupply=',TSUPLY,' Guess VAV/CAV=',tswitchvavcav,
     &       ' mcp=',D
           call edisp(itu,outs)
           write(outs,'(8(A,F8.4))') ' Flow rate used =',AFR,
     &       ' Flow rate wanted =',prefafr,
     &       ' Flow gextf=',ZMBI(IICOMP,3),
     &       ' Flow loss=',ZMBI(IICOMP,4),
     &       ' BB1=',BB1,' BB2=',BB2,' BB3=',BB3,' DT=',DT
           call edisp(itu,outs)
         else
           write(outs,'(A,2(A,F9.1),4(A,F7.3))') 
     &       mode(1:lnblnk(mode)),' Qfut=',QFUT,' Reheat =',QReheat,
     &       ' Tfut=',TFUT,' SetPt=',SP,' Supply T after reheat=',TS,
     &       ' Guess VAV/CAV=',tswitchvavcav
           call edisp(itu,outs)
           write(outs,'(7(A,F8.4))') ' Flow rate used =',AFR,
     &       ' Flow rate wanted =',prefafr,
     &       ' Flow gextf=',ZMBI(IICOMP,3),
     &       ' Flow loss=',ZMBI(IICOMP,4),
     &       ' BB1=',BB1,' BB2=',BB2,' BB3=',BB3
           call edisp(itu,outs)
         endif
         call edisp(itu,' Leaving subroutine BCL22')
      END IF

      RETURN
   99 call edisp(iuout,' BCL22: data incomplete.')
      call epwait
      call epagend
      STOP
      END

C ******************** BCL23
C This subroutine dynamically allocates heating and cooling setpoint
C temperatures from a previously defined temporal file based on column
C numbers specifed in the contol law. It is a simplified implementation
C of control law 1 (basic heating and cooling). It can use two items in tdf:
C IACTIV (ACTIVITY of space) and IHTCLSETP (only heating and cooling setpoints)
      SUBROUTINE BCL23
#include "building.h"
#include "tdf2.h"
#include "control.h"
#include "sbem.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/btime/btimep,btimef
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      common/dllerr/dllsubr,dllmesg
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/CLIMIF/QFLWP,QFLWF,TFLWP,TFLWF,QDFLP,QDFLF,VFLP,VFLF,
     &             DFLP,DFLF,HFLP,HFLF

C These common TDFREL relate to setting cooling on if temperature exceeds
C the heating set point by 1degC for the UKNCM notional model (and
C associated DSM testing models)

      character dllsubr*12,dllmesg*124,outs*124,cmsg*12
      logical dll,near
      DIMENSION VAL(MTABC+2)
      integer COLUMNSETP ! column from control 23

C Check to see if running in dll mode.
      call isadll(dll)

C Fatal error test.
      IF(BMISCD(ICF,IDTYP,IPER,1).LT.2.)GOTO 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        call edisp(itu,' Entering subroutine BCL23')
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

C Determine sensed temperature.
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C QHM = max heating capacity (W)
C QHN = min heating capacity (W)
C QCM = max cooling capacity (W)
C QCN = min cooling capacity (W)
C HSP = heating set point temp. (deg.C)
C CSP = cooling set point temp. (deg.C)
      cmsg=' '
      QHM=999999.0
      QHN=0.0
      QCM=-999999.0
      QCN=0.0
      QFUT=0.0
      TFUT=TNP

C Turn cooling off if no UK NCM cooling generator is linked to this zone
C If there is no system associated with the zone then set QCM to zero.
C If this is the reference model then cooling will be present regardless
C of the type of space conditioning system. This is done later on 
      IHLZP=IHLZ(IiCOMP)  ! set up pointer
      NEAR=.TRUE.
      if(ihlzp.ne.0) CALL ECLOSE(CGEF(IHLZP),0.0,0.001,NEAR)
      IF(NEAR)QCM=0.0

C Read setpoints from tdf here (CSP and HSP)
C "COLUMNSETP" is an integer specifying the column within the
C returned temporal item data array VAL. 

C Each activity has 5 data items. Counting the timestamp as
C column 1 then the first activity will have heating setpoint
C at column 5 and the cooling setpoint at column 6 and the
c second activity will have its heating at column 10 and
C cooling setpoint at column 11.  RCTDFBALL gets all of the
C columns into VAL. But decrement COLUMNSETP by one to exclude
C the initial time column.
      COLUMNSETP=nint(BMISCD(ICF,IDTYP,IPER,2))
      if(IACTIV(ICF).ne.0)then
        itdi=IACTIV(ICF)
      elseif(IHTCLSETP(ICF).ne.0)then
        itdi=IHTCLSETP(ICF) 
      endif

C There is only a requirement to scan this timestep once to get
C both the heating and cooling setpoints.
      CALL RCTDFBALL(itrc,btimef,VAL,ISD,IER)
      if(IACTIV(ICF).ne.0)then
        HSP=VAL(COLUMNSETP-1)
      elseif(IHTCLSETP(ICF).ne.0)then
        HSP=VAL(COLUMNSETP)
      endif

C Now get the column for cooling.
      COLUMNSETP=nint(BMISCD(ICF,IDTYP,IPER,3))
      if(IACTIV(ICF).ne.0)then
        CSP=VAL(COLUMNSETP-1)
        cmsg=' via column'
      elseif(IHTCLSETP(ICF).ne.0)then
        CSP=VAL(COLUMNSETP)
        cmsg=' via column'
      endif
        
C If UK NCM reference building (or stripped with DSM testing flag equal to 2)
C then cooling set point is always 27degC
      if(INOTI.EQ.2.OR.(INOTI.eq.4.AND.iDsmTestingFlag.eq.2))then
        CSP=27.0
        cmsg=' via test'
        QCM=-999999.0
      endif

C If notional model or DSM testing then set cooling set point to 1degC
C more than static heating setpoint (found in the header of the temporal
C file) to compensate for overheating (5ach not implemented). The value
C in the header is based on information in SBEM.db1 which has been edited
C to reflect the highest heating setpoint in the activities timestep
C data. (the original static heating setpoint in SBEM.db1 was for use
C only in iSbem.

      if(INOTI.eq.1.or.iDsmTestingFlag.ge.1)then
        iitdi=IACTIV(IICOMP)
        if(iNatVentilatedFlag.eq.1)then

          HSPTest=TMAR(iitdi,IATDF(iitdi,15)) ! static high HSP
          CSP=HSPTest+1.0   ! the official NCM logic
          cmsg=' stat hsp+1'

C Test commented out, use highest heating setpoint for the activity.
C          if(HSPTest.ge.HSP)then
C            CSP=HSPTest+1.0
C            cmsg=' stat hsp+1'
C          else
C            CSP=HSP+1.0
C            cmsg=' step hsp+1'
C          endif

C Test commented out. 
C          if(CSP.lt.21.0)then
C            CSP=21.0
C            cmsg=' step hsp++'
C          endif

C Set up maximum cooling to equivalent of 5 air changes
          QCM=-1.*0.333*5.*vol(iicomp)*(tfa(iicomp)-TFLWF)

C If ambient is hotter than interior no cooling can be achieved
          if(QCM.gt.0.)QCM=0.
        endif
      endif

      IF(TCTL.LE.CSP.AND.TCTL.GE.HSP)then
        IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &     ITRACE(41).NE.0) THEN
          write(itu,'(a,f8.3,a,i3,a,f7.2,a,i3,a,f7.2,a,f7.2,2a,i3)')
     &      'tdf ctl data @',btimef,' is item',itdi,
     &      ' value ',tctl,' for ctl loop ',icf,
     &      ' hsp ',hsp,' csp ',csp,cmsg,' column of csp ',COLUMNSETP
          write(itu,*)'VAL is ',(val(i),i=1,24)
          write(itu,*)'tctl ',tctl,' is in the deadband.'
        ENDIF
        RETURN
      endif

C Since sensed temperature is not within range, attempt to control
C THIS zone's temperature.
C      TCTL=TNP

C Is temp. too high ?
      IF(TCTL.GT.CSP)GOTO 1

C May need these parameters in future
*      IPLT=1
*      QMX=QHM
*      QMN=QHN
*      TCONT=HSP

C Too low, heat to HSP.
      Q=(BB3-BB1*HSP)/BB2

C Is this capacity available ?
      IF(Q.LE.QHM.AND.Q.GE.QHN)GOTO 2
      IF(Q.GT.QHM)GOTO 3
      Q=QHN
      GOTO 2
    3 Q=QHM
      GOTO 2

C Temp. too high, cool to CSP.
    1 Q=(BB3-BB1*CSP)/BB2
*      IPLT=2
      QMX=QCM
      QMN=QCN
*      TCONT=CSP

C Is this available ?
      IF(Q.GE.QCM.AND.Q.LE.QCN)GOTO 2
      IF(Q.LT.QCM)GOTO 5
      Q=QCN
      GOTO 2
    5 Q=QCM
    2 TFUT=(BB3-BB2*Q)/BB1
      QFUT=Q

C Trace output format depending on whether VAV or CAV active.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) THEN
        write(itu,'(a,f8.3,a,i3,a,f7.2,a,i3,a,f6.2,a,f6.2,2a,i3,a,
     &    f8.0)')
     &    'tdf ctl data @',btimef,' is item',itdi,
     &    ' value ',tctl,' for ctl loop ',icf,
     &    ' hsp ',hsp,' csp ',csp,cmsg,' column of csp ',COLUMNSETP,
     &    ' cooling cap ',QCM
        write(itu,*)'VAL is ',(val(i),i=1,24)
        write(itu,*)'Qfut is ',QFUT,' TFUT is ',TFUT
        IF(INOTI.EQ.2)write(itu,*)'simulating UK NCM reference building'

        call edisp(itu,' Leaving subroutine BCL23')
      END IF

      return
   99 write(outs,'(a)')' BCL23: data incomplete.'
      if(dll)then
        dllsubr='BCL23'
        dllmesg=outs
        ier=2
        return
      else
        call edisp(iuout,outs)
        call epwait
        call epagend
        STOP
      endif
      END

C ******************** BCL24
C Adaptive human comfort model works just like the basic controller
C BCL01, but the comfort setpoints are continually changed based on a
C running mean comfort temperature calculated from the climate database
      SUBROUTINE BCL24(IER)

#include "building.h"
#include "control.h"

C climate file commons
      COMMON/FILEP/IFIL
      COMMON/CLIMIF/QFLWP,QFLWF,TFLWP,TFLWF,QDFLP,QDFLF,VFLP,VFLF,
     &             DFLP,DFLF,HFLP,HFLF

      common/pophelp/h(60)
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      common/dllerr/dllsubr,dllmesg

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT

      DIMENSION ICLM(24,6)
      PARAMETER (MMD=20)
      REAL PREVTEMP(MMD)
      character dllsubr*12,dllmesg*124,outs*124,H*72
      logical dll

C Check to see if running in dll mode.
      call isadll(dll)

C Fatal error test.
      IF(BMISCD(ICF,IDTYP,IPER,1).LT.6.)GOTO 99

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0) then
        if (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).eq.0) then
          write(outs,'(a,i2)') 
     &      'Entering subroutine BCL24 sensing in zone ',
     &      IBSN(ICF,1)
          call edisp(itu,outs)
        elseif (IBSN(ICF,1).GT.0.and.IBAN(ICF,1).GT.0) then
          write(outs,'(a,i2,a,i2)') 
     &      'Entering subroutine BCL01 sensing in zone ',
     &      IBSN(ICF,1),' and actuating in zone ',IBAN(ICF,1)
          call edisp(itu,outs)
        else
          call edisp(itu,' Entering subroutine BCL24')
        endif
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

C Determine sensed temperature.
      CALL CFVAR(TCTL,IER)
      if(ier.eq.2)then
        return
      endif

C calculate outside daily mean temperature TODM and
      IYDS=IDYP-MMD
      IYDF=IDYP-1
      DO 3157 II=1,MMD
        PREVTEMP(II)=0.0
 3157 CONTINUE
      IF(IYDS.GT.0.AND.IYDF.GT.0)THEN
        ID=0
        DO 557 I=IYDS,IYDF
          ID=ID+1
          II=I
          IREC=II
          READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)((ICLM(J,K),K=1,6),
     &           J=1,24)
          DO 656 J=1,24
            PREVTEMP(ID)=REAL(ICLM(J,2))/10.+PREVTEMP(ID)
  656     CONTINUE
          PREVTEMP(ID)=PREVTEMP(ID)/24.
  557   CONTINUE
      ELSEIF(IYDS.LE.0)THEN
        IYDS=365+IYDS
        ID=0
        DO 550 I=IYDS,365
          ID=ID+1
          II=I
          IREC=II
          READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)((ICLM(J,K),K=1,6),
     &           J=1,24)
          DO 657 J=1,24
            PREVTEMP(ID)=REAL(ICLM(J,2))/10.+PREVTEMP(ID)
  657     CONTINUE
          PREVTEMP(ID)=PREVTEMP(ID)/24.
  550   CONTINUE
        IF(IDYF.GT.0)THEN        
          DO 551 I=1,IDYF
            ID=ID+1
            II=I
            IREC=II
            READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)((ICLM(J,K),K=1,6),
     &             J=1,24)
            DO 658 J=1,24
              PREVTEMP(ID)=REAL(ICLM(J,2))/10.+PREVTEMP(ID)
  658       CONTINUE
            PREVTEMP(ID)=PREVTEMP(ID)/24.
  551     CONTINUE
        ENDIF
      ENDIF

C Get ambient temperature TOUT
      TOUT=TFLWF
C QHM = max heating capacity (W)
C QHN = min heating capacity (W)
C QCM = max cooling capacity (W)
C QCN = min cooling capacity (W)
C HSP = heating set point temp. (deg.C)
C CSP = cooling set point temp. (deg.C)
C ALPHA = running mean response parameter (-)
C CB = Comfort band
      QHM=BMISCD(ICF,IDTYP,IPER,2)
      QHN=BMISCD(ICF,IDTYP,IPER,3)
      QCM=-BMISCD(ICF,IDTYP,IPER,4)
      QCN=-BMISCD(ICF,IDTYP,IPER,5)
C      HSP=BMISCD(ICF,IDTYP,IPER,6)
C      CSP=BMISCD(ICF,IDTYP,IPER,7)
      ALPHA=BMISCD(ICF,IDTYP,IPER,8)
      CB=BMISCD(ICF,IDTYP,IPER,9)
      
C      TCTL=TNP
      IHEAT=0
      ICOOL=0
      QFUT=0.
      TFUT=TNP
      pw=0.

C Use setpoints as extreme limits at which controls become active
**      IF(TCTL.LT.HSP)THEN
**        IHEAT=1
**        IHEATI=1
**      ELSEIF(TCTL.GT.CSP)THEN
**        ICOOL=1
**        ICOOLI=1
**      ENDIF

C If temperature is within extreme limits then 
C Calculate heating/cooling based on adaptive comfort algorithm
C First calculate running mean temperature
        COEFF=1.
        TRM=0.
        DO 245 I=1,MMD
          ISUB=I-1
          TRM=TRM+COEFF*PREVTEMP(MMD-ISUB)
          COEFF=COEFF*ALPHA
 245    CONTINUE
        TRM=(1.-ALPHA)*TRM

C Calculate operative temperature of the zone (TOP). It is assumed that
C the user has selected to sense mean radiant temperature in the sensor
C details.
        TOP=TCTL

C Calculate comfort temperature TCOMF (based on the summer and winter behaviours)
        IF(TRM.GE.10.)TCOMF=0.33*TRM+18.8
        IF(TRM.LT.10.)TCOMF=0.09*TRM+22.6

C Get random number
        RN=RNOR()
        RN=ABS(RN)
        RN=1000.*RN
        IRN=INT(RN)
        RN=RN-REAL(IRN)

C Calculate probability that control will be activated.
        FUNC=0.171*TOP+0.166*TOUT-6.43
        Pw=EXP(FUNC)/(1.+EXP(FUNC))
        IF((TOP-TCOMF).GT.CB)THEN

C Hot, so set on cooling
          IF(Pw.GT.RN)ICOOL=1
        ELSEIF((TOP-TCOMF).LT.(-1.*CB))THEN

C Cold, so set on heating
          IF(Pw.LT.RN)IHEAT=1
        ELSE

C Within 2degC comfort band so do nothing
          IHEAT=0
          ICOOL=0
        ENDIF

C Set on heating to within comfort band of comfort temperature
C This is different from statically fixed setpoints!
      IF(IHEAT.EQ.1)THEN
**        Q=(BB3-BB1*HSP)/BB2
        Q=(BB3-BB1*(TCOMF-CB))/BB2
        IF(Q.LE.QHM.AND.Q.GE.QHN)GOTO 23
        IF(Q.GT.QHM)GOTO 33
        Q=QHN
        GOTO 23
   33   Q=QHM
   23   TFUT=(BB3-BB2*Q)/BB1
        QFUT=Q
      ELSEIF(ICOOL.EQ.1)THEN

C Set on cooling
**        Q=(BB3-BB1*CSP)/BB2
        Q=(BB3-BB1*(TCOMF+CB))/BB2
        IF(Q.GE.QCM.AND.Q.LE.QCN)GOTO 22
        IF(Q.LT.QCM)GOTO 55
        Q=QCN
        GOTO 22
   55   Q=QCM
   22   TFUT=(BB3-BB2*Q)/BB1
        QFUT=Q
      ENDIF

C Debug
C      write(6,*)trm,TOP,TCOMF,iheat,icool,QFUT,pw,rn

      RETURN
   99 write(outs,'(a)')' BCL24: data incomplete.'
      if(dll)then
        dllsubr='BCL24'
        dllmesg=outs
        ier=2
        return
      else
        call edisp(iuout,outs)
        call epwait
        call epagend
        STOP
      endif
  999 H(1) = 'A data i/o error has occured in the program at BCL24'
      H(2) = 'and it is probably best not to go on. '
      H(3) = 'Program is taking one step back.'
      CALL PHELPD('data i/o error',3,'i/o exception handler',0,0,IER)
      RETURN
      END

C ******************** BCL99
C BCL99 modifies the thermo-physical properties of up to three 
C multi-layered construction(s).
C ICALL = 0 signals call from nested zone control
C ICALL = 1 signals call from solar with no change in props.
C ICALL = 2 signals call from solar with request for alternative props.

      subroutine bcl99(ICALL)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0,G2 G5.
#include "geometry.h"

#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
#include "material.h"
#include "control.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/btime/btimep,btimef

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/GR1D02/IADP,IVAR,FMIN,FREF,FMAX,NSTP

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep

      common/fvala/tfa(mcom),qfa(mcom)
      common/pstsol/icf,idtyp,iper,bb1,bb2,bb3,iicomp,tnp,qfut,tfut

      common/t1/ne(ms),nairg(ms),ipairg(ms,mgp),rairg(ms,mgp)
      common/t2/con(ms,me),den(ms,me),sht(ms,me),thk(ms,me)

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      common/prec12/ei(mcom,ms),ee(mcom,ms),ai(mcom,ms),ae(mcom,ms)
      COMMON/PREC14/emarea(MCOM)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)

      common/trc/itrc
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/tpmod/itpmod(mcom)
      common/tpmodx/iperx,idtypx
      
      common/BSCTL/ITPREP(MCOM,MTMC),ICZN,IMDB(3),IGLZ(3)

C Material properties.
      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

C << for the case of bcl being called from solar is the limit >>
C << of 3 surface to change will have to be increased!        >>
      dimension mldbp(3),isupd(3)
      
      logical itpmod, close, usealt, CHANGED
      
      iunit=ifil+1
      
C Debug...
C      write(6,*) 'BCL 99 ',btimef
      
C If called from solar.F then use the data in common/BSCTL/ otherwise
C use data in normal ctl common blocks.
      if (ICALL.eq.1) then
        usealt=.false.
        ICMP=ICZN
      elseif (ICALL.eq.2) then
      
C Set usealt to true and remember zone number
        usealt=.true.
        ICMP=ICZN
      
C Set constructions to update.
        mldbp(1)=IMDB(1)
        mldbp(2)=IMDB(2)
        mldbp(3)=IMDB(3)
        
C Set surfaces to be updated.
        isupd(1)=IGLZ(1)
        isupd(2)=IGLZ(2)
        isupd(3)=IGLZ(3)
      else

C Establish which construction substitution control
C function relates to current zone.
        jcf=ibsn(icf,4)

C Fatal error test.
C Check that referenced ctl loop has been defined and is different 
C from the current loop.
        if(jcf.lt.1.or.jcf.gt.ncf.or.jcf.eq.icf)goto 99
      
C Check that thermophysical prop sub defined (-99) and that 
C sufficient data exists.
        if(ibsn(jcf,1).ne.-99.or.
     &     bmiscd(jcf,idtypx,iperx,1).lt.4.)goto 99

C Test if properties need to be reset. If not, return.
C Check that 2nd data item = zero (This allows time based control).
        call eclose(bmiscd(jcf,idtypx,iperx,2),0.00,0.001,close)
        ICMP=iicomp
        if(itpmod(ICMP))then 
          if(close)then
            usealt=.false.
          elseif(tfa(ICMP).lt.ibsn(jcf,2).and.
     &         tfa(ICMP).gt.ibsn(jcf,3))then
            usealt=.false.
          else
            usealt=.true.
          endif
        else
          if(close)then
            usealt=.false.
          elseif(tfa(ICMP).lt.ibsn(jcf,2).and.
     &         tfa(ICMP).gt.ibsn(jcf,3))then
            usealt=.false.
          else
            usealt=.true.
          endif
        endif
        
C Update up to three constructions.
        mldbp(1)=int(bmiscd(jcf,idtypx,iperx,3))
        mldbp(2)=int(bmiscd(jcf,idtypx,iperx,4))
        mldbp(3)=int(bmiscd(jcf,idtypx,iperx,5))
        
C Set surfaces to be updated.
        isupd(1)=iban(jcf,1)
        isupd(2)=iban(jcf,2)
        isupd(3)=iban(jcf,3)
      endif
      
C Update or reset properties based on above logic.
      CHANGED=.false.
      if (itpmod(ICMP).and.(.not.usealt)) then
        CHANGED=.true.

C Find version of materials database. If 1.1 then use material arrays.
        call eclose(matver,1.1,0.01,closemat1)
        call eclose(matver,1.2,0.01,closemat2)
        if(closemat1.or.closemat2)then
          continue
        else
          call edisp(iuout,'BCL99: Materials arrays are incomplete.')
        endif
	
C Reset properties by reading in zone descriptor files.

C Debug.
C        write(6,*)'Resetting tp properties at btimef ',btimef

        call georead(iunit,lgeom(ICMP),ICMP,0,itu,ier)
        call econst(lthrm(ICMP),iunit,ICMP,itrc,itu,ier)
        itpmod(ICMP)=.false.
      elseif ((.not.itpmod(ICMP)).and.usealt) then
        CHANGED=.true.
      
C Change to alternate properties (up to three constructions).
C Read in the thermophysical properties for this zone 
C (needed in subroutine mzcoe1).
        call georead(iunit,lgeom(ICMP),ICMP,0,itu,ier)
        call econst(lthrm(ICMP),iunit,ICMP,itrc,itu,ier)

C Get replacement properties for up to 3 constructions from 
C multi-layered database common block MLC.
C Determine construction for substitution. ii is the surface 
C number, ic is the composite construction reference.
C At present the replacement construction must have the
C same number of elements and airgaps as the original. 
C To generalise this, the variables 'ne', 'ngaps', and 'npgaps' 
C need to be modified in common 'prec9'. 
        do 999 icnsub=1,3
        
C ii represents the surface to update.
          ii = isupd(icnsub)
          if(ii.eq.0)goto 999
          nairg(ii)=0
          
C ic represents the mlc entry number of the new construction.
C ic is tested for zero in solar.F
          ic = mldbp(icnsub)

C Debug.
C        write(6,*) 'Updating thermophysical properties of surface ',ii
C        write(6,*) 'with construction ',DESC(ic)' at btimef ',btimef

C Put the composite db info into current surface attributes
C common.  Gather information about each layer. Read in the
C material db info. 
          ne(ii)=layers(ic)
          do 223 i=1,layers(ic)
            if(closemat1.or.closemat2)then
              legmat=ipr(ic,i)   ! which legacy index
              if(legmat.gt.0)then
                matarrayindex=mathash(legmat)  ! find matching array index
                dbcon=matdbcon(matarrayindex)
                dbden=matdbden(matarrayindex)
                dbsht=matdbsht(matarrayindex)
                E=matdbine(matarrayindex)
                A=matdbina(matarrayindex)
              endif
            else
              ier=1  ! incomplete materials arrays so pass back ier
            endif
            if(ier.eq.1)return
            if(ipr(ic,i).ne.0)then
              con(ii,i)=dbcon
              THRMLI(ICMP,II,I,1)=CON(II,I)
              den(ii,i)=dbden
              THRMLI(ICMP,II,I,2)=DEN(II,I)
              sht(ii,i)=dbsht
              THRMLI(ICMP,II,I,3)=SHT(II,I)
              thk(ii,i)=dthk(ic,i)
              THRMLI(ICMP,II,I,4)=THK(II,I)
            else
              con(ii,i) = 0.0
              den(ii,i) = 0.0
              sht(ii,i) = 0.0
              thk(ii,i)=dthk(ic,i)
              THRMLI(ICMP,II,I,4)=THK(II,I)

C Keep track of the air gaps in the composite. 
              nairg(ii)=nairg(ii)+1
              ipairg(ii,nairg(ii))=i
              if(svfc(ii).eq.'VERT')then
                rairg(ii,nairg(ii))=drair(ic,i,1)
              elseif(svfc(ii).eq.'FLOR')then
                rairg(ii,nairg(ii))=drair(ic,i,2)
              elseif(svfc(ii).eq.'CEIL')then
                rairg(ii,nairg(ii))=drair(ic,i,3)
              endif
              RGAPS(ICMP,II,nairg(ii))=rairg(ii,nairg(ii))
            endif

C Assign surface properties.
            if(i.eq.1)         ee(ICMP,ii) = e
            if(i.eq.layers(ic))ei(ICMP,ii) = e
            if(i.eq.1)         ae(ICMP,ii) = a
            if(i.eq.layers(ic))ai(ICMP,ii) = a
  223     continue

C Re-distribute nodes through new constructions, if the adaptive 
C gridding was on.
          IF(IADP.EQ.1)CALL NDCONS(ICMP,II)
  999   continue
        itpmod(ICMP)=.true.
      endif
      
C Keep track of the total surface area/emisivity for use
C in apportioning casual and plant radiation to surfaces.
      if (CHANGED) then
        emarea(ICMP)=0.0
        do 80 kk=1,nconst(ICMP)
          emarea(ICMP)=emarea(ICMP)+sna(ICMP,kk)*ei(ICMP,kk)
  80    continue
  
C Re-establish building-side matrix equation.
        call mzcoe1(ICMP)

C Modify if computational time-step is less than one hour.
        IF(NTSTEP.GT.1)THEN
          TSTEP=1./FLOAT(NTSTEP)
          CALL MZCOE2(TSTEP)
        ENDIF
      endif

      return
   99 call usrmsg(' ',' bcl99: data incomplete or in error.','W')
      call epwait
      call epagend
      stop
      end

c ******************** MZRCPL

c This routine is called by particular BCL?? routines
c when plant has been specified as having a radiant
c and convective component. Some iteration takes place.

      SUBROUTINE MZRCPL(IC,AA,BB,CC,TCTL,TR,IPLT,QMX,QMN,TE,Q)

#include "building.h"
#include "control.h"
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/SETUQ/QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)

      COMMON/ZONEQN/E(MEQ,MTR)
      COMMON/ZONEQS/ES(MEQ,MTR),ACAPS

      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)
      COMMON/PREC14/emarea(MCOM)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D04/GAM,RGAM

      logical atrace
      character outs*124

C If called from BCL06, any sensor will do
      IF(IBCLAW(ICF,IDTYP,IPER).EQ.6) GOTO 123

c Check that a valid control law is calling this routine !
      IF(IBSN(ICF,1).GE.0.AND.IBSN(ICF,2).EQ.0)GOTO 123
      IF(IBSN(ICF,1).EQ.-2)GOTO 123

c Inform user of problems!
      call edisp(iuout,
     &  ' Actuator and sensor not available with this')
      call edisp(iuout,' release. Simulation terminated !')
      call epwait
      call epagend
      STOP

C Trace output
 123  atrace = .false.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(41).NE.0)atrace = .true.
      if(atrace)then
        call edisp(ITU,' Entering subroutine MZRCPL')
        call edisp(ITU,
     &  ' Entry parameters: Zone,AA,BB,CC,TCTL,TR,IPLT,QMX,QMN,TE,Q')
        write(outs,*)IC,AA,BB,CC,TCTL,TR,IPLT,QMX,QMN,TE,Q
        call edisp(ITU,outs)
      endif

c Iteration counter.
      ITER=0

c Initialise QSUM the total plant supplied to the calculated
c value for a convective actuator only.
      QSUM=Q
      TOLD=TE

c N1 relates to the air node
c N2 relates to the plant term
c N3 relates to the present term
      NC=NCONST(IC)
      N1=NC+1
      N2=NC+2
      N3=NC+3

c At this stage the E array has been used in the forward
c reduction process and has been corrupted. Using array ES
c to re-assign the E array values due account can be taken
c of radiant plant and a new forward reduction conducted.
    1 DO 100 I=1,N1
        DO 110 J=1,N3+1
          E(I,J)=ES(I,J)
  110   CONTINUE
  100 CONTINUE

      DO 10 I=1,NC
        NN=NNDC(IC,I)

C Calculate the plant radiative and convective component in W/m2.
        PADDR=QSUM*(1.-CONV(IC))*ei(ic,i)/emarea(ic)

C Re-assign E variable.
        E(I,N3)=E(I,N3)+QC(IC,I,NN)*PADDR*GAM

C Trace...
      if(atrace)then
        write(outs,*)'Radiant: PADDR qsum qc() n(i,n3) n1 n3',
     &    PADDR,QSUM,QC(IC,I,NN),E(I,N3),i,n3
        call edisp(ITU,outs)
      endif
   10 CONTINUE
      PADDC=QSUM*CONV(IC)
      E(N1,N3)=E(N1,N3)+ACAPS*PADDC*GAM

C Trace...
      if(atrace)then
        write(outs,*)'Convec: PADDC QSUM acaps n(n1,n3) n1 n3 ',
     &    PADDC,QSUM,ACAPS,E(N1,N3),n1,n3
        call edisp(ITU,outs)
      endif

c Commence forward reduction.
      N=1
      DO 20 I=1,NC
        N=N+1
        M=N-1
        DO 30 J=N,N1
          X1=E(J,M)
          X2=E(I,M)
          X3=X1/X2
          DO 40 K=M,N3
            SUB=E(I,K)*X3
            E(J,K)=E(J,K)-SUB
   40     CONTINUE
   30   CONTINUE
   20 CONTINUE

c Air temperature coefficient.
      B1=E(N1,N1)

c Plant coefficient.
      B2=E(N1,N2)

c Present term coefficient.
      B3=E(N1,N3)

      TNEW=B3/B1
C Trace...
      if(atrace)then
        write(outs,*)'B1 B2 B3 TNEW TOLD',B1,B2,B3,TNEW,TOLD
        call edisp(ITU,outs)
      endif
      IF(ABS(TNEW-TOLD).LT.0.001)GOTO 2

      TDIFF=ABS(TCTL-TNEW)
      IF(TDIFF.GT.TR)THEN
        Q1=(B3-TCTL*B1)/B2
        QSUM=QSUM+Q1
        IF(IPLT.EQ.1)THEN
          IF(QSUM.GT.QMX)QSUM=QMX
          IF(QSUM.LT.QMN)QSUM=QMN
        ENDIF
        IF(IPLT.EQ.2)THEN
          IF(QSUM.LT.QMX)QSUM=QMX
          IF(QSUM.GT.QMN)QSUM=QMN
        ENDIF
        ITER=ITER+1
      ENDIF
      TOLD=TNEW
      IF(ITER.LE.25.AND.TDIFF.GT.TR)GOTO 1

c TDIFF acceptable. Conduct backward substitution.
    2 QQ=0.
      TE=TNEW
      DO 50 I=1,NC
        II=N1-I
        SUM=E(II,N3)-E(II,N2)*QQ
        DO 60 J=II,NC
          L=N1+II-J
          IF(L.EQ.N1)GOTO 61
          SUM=SUM-E(II,L)*TFS(IC,L)
          GOTO 62
   61     SUM=SUM-E(II,L)*TE
   62     IF(J.LT.NC)GOTO 60
          TFS(IC,II)=SUM/E(II,II)
   60   CONTINUE
   50 CONTINUE
      AA=B1
      BB=B2
      CC=B3
      Q=QSUM
      if(atrace)then
        write(outs,*)' Returning AA BB CC Q ',AA,BB,CC,Q
        call edisp(ITU,outs)
        call edisp(ITU,' Leaving subroutine MZRCPL')
        call edisp(ITU,' ')
      endif
      RETURN
      END

C ****************************** SUBROUTINE ZRHCTL *****************************
C This subroutine contains control algorithms for the control of relative humidity 
C in a zone. The algorithms are (or will be) the mirrors of the relevant thermal  
C control algorithms. The following algorithms are available:
C ZRHCTL- the controlling routine
C ZRH01 - basic control of zone relative humidity (capacity limited). 


      SUBROUTINE ZRHCTL(ICOMP,ITYP,GUC,GAXF)
      IF(ITYP.EQ.1) THEN
        CALL ZRH01(ICOMP,GUC,GAXF)
      ELSE
        RETURN
      ENDIF

      RETURN
      END

C ****************************** SUBROUTINE ZRH01 *****************************
C This routine performs control on the zone relative humidity
C Variables are as follows:
C 

      SUBROUTINE ZRH01(ICOMP,GUC,GAXF)

#include "building.h"
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TC/ITC,ICNT

      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

C Common for relative humidity control (ideal)
C      COMMON/IDRHCTL1/RHSPT(MCOM),IRHCT(MCOM)

      COMMON/IDRHCTL1/RHSPTU(MCOM),RHSPTL(MCOM),RHHC(MCOM),
     &RHDC(MCOM),IRHCT(MCOM)

      COMMON/ATPRES/PATMOS
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/VAPC2/ZMDAP(MCOM),ZMDAF(MCOM)

C Common for humidity control variables
      COMMON/RHCTLDAT/ZLLDF(MCOM),ZLLDP(MCOM)

      CHARACTER OUTS*124

C Initialise the variables
      IF(NSINC.GT.1) THEN
        ZLLDP(ICOMP)=ZLLDF(ICOMP)
      ELSE
        ZLLDP(ICOMP)=0.0
      ENDIF

      GCTU=0.0
      GCTL=0.0
      GCT=0.
      GAXF=0.0
      ENTHCT=0.0
      ENTHUC=0.0
      ZLLDF(ICOMP)=0.0
      ZRHUC=0.0

C Calculate zone moisture content at the upper RH set point 
      GCTU=HUMRT1(TFA(ICOMP),RHSPTU(ICOMP),PATMOS,0)

C Calculate zone moisture content at the lower RH set point 
      GCTL=HUMRT1(TFA(ICOMP),RHSPTL(ICOMP),PATMOS,0)

      IF(GUC.GT.GCTU.OR.GUC.LT.GCTL)THEN

C Calculate change in moisture content required (kg/kg da)
        IF(GUC.GT.GCTU) THEN
          GAXF=GCTU-GUC

C Check that the maximum dehumidification rate is not exceeded
          GAXR=(GAXF*ZMDAF(ICOMP))/(3600/NTSTEP)   
          IF(GAXR.LT.RHDC(ICOMP)) GAXF=
     &(RHDC(ICOMP)*3600/NTSTEP)/ZMDAF(ICOMP)
          ENTHCT=ENTHP2(TFA(ICOMP),GUC+GAXF)*1000.
        ENDIF
        IF(GUC.LT.GCTL)THEN 
          GAXF=GCTL-GUC

C Check that the maximum humidification rate is not exceeded
          GAXR=(GAXF*ZMDAF(ICOMP))/(3600/NTSTEP)   
          IF(GAXR.GT.RHHC(ICOMP)) GAXF=
     &(RHHC(ICOMP)*3600/NTSTEP)/ZMDAF(ICOMP)
          ENTHCT=ENTHP2(TFA(ICOMP),GUC+GAXF)*1000.
        ENDIF

C Calculate the total moisture addition or extraction over the timestep (kg) 
C        GAXTOT=GAXF*(NTSTEP/3600)*ZMDAF(ICOMP)
 
C Calculate the enthalpy at uncontrolled moisture content.
         ENTHUC=ENTHP2(TFA(ICOMP),GUC)*1000.

C Calculate the latent addition/extraction (W)
         ZLLDF(ICOMP)=(ENTHCT-ENTHUC)*ZMDAF(ICOMP)/(3600/NTSTEP)

C Reset the relative humidity control flag
        IRHCT(ICOMP)=0     
      ELSE
        GAXF=0.
        ZLLDF(ICOMP)=0.0 
      ENDIF

C Reset the relative humidity control flag
      IRHCT(ICOMP)=0

C If control is active then perform trace.
      write(outs,'(a)') 'Control active ....'

C Trace output ?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999

      IF(ITRACE(10).EQ.0.OR.NSINC.GT.ITCF.OR.
     &   IZNTRC(ICOMP).NE.1)goto 9999
      write(outs,'(A,I4)')' Subroutine ZRH01 Trace output',ICNT
      call edisp(itu,outs)
      write(outs,'(23X,A,I4)')' Zone',ICOMP
      ICNT=ICNT+1
      CALL DAYCLK(IDYP,BTIMEF,ITU)
      call edisp(itu,outs)

      write(outs,99980) RHSPTU(ICOMP)
99980 format(' RH set point upper: ',F14.6)
      call edisp(itu,outs)
      write(outs,99981) RHSPTL(ICOMP)
99981 format(' RH set point lower: ',F14.6)
      call edisp(itu,outs)
      write(outs,99982) RHHC(ICOMP)
99982 format(' Max humidification rate: ',F14.6)
      call edisp(itu,outs)
      write(outs,99983) RHDC(ICOMP)
99983 format(' Max dehumidification rate: ',F14.6)
      call edisp(itu,outs)
      write(outs,99984) GUC
99984 format(' Moisture cnt before control : ',F14.6)
      call edisp(itu,outs)
      write(outs,99985) GAXF
99985 format('Moisture addition extraction : ',F14.6)
      call edisp(itu,outs)
      write(outs,99987) ZLLDF(ICOMP)
99987 format('Room latent load = ',F14.6)
      call edisp(itu,outs)
      write(outs,99988) ENTHUC,ENTHCT
99988 format(' ENTHUC  = ',F14.6,' ENTHCT  = ',F14.6)
      call edisp(itu,outs)
      write(outs,99989) ZLLDF(ICOMP)
99989 format( 'Zone latent load = ',F14.6)
      call edisp(itu,outs)

 9999 RETURN
      END


C *********************** SUBROUTINE FZINIT
C This routine is called every time-step to zeroise common variables
C used in fuzzy logic control functions.

      SUBROUTINE FZINIT
#include "building.h"

      COMMON/FZCTL1/FZM(MFP,3,5,10),ERRSAV(MFP),QSAV(MFP),
     &ACUT(3,5),FZCTIP(5,5),FZCTOP(5),COA(5)     
      
      DO 84 I=1,5
         DO 80 J=1,5
            IF(J.EQ.1)THEN
               COA(I)=0.
               FZCTOP(I)=0.
            ENDIF
            FZCTIP(I,J)=0.
  80     CONTINUE
84    CONTINUE
      DO 88 L=1,3
         DO 86 N=1,5
            ACUT(L,N)=0.
  86     CONTINUE
88    CONTINUE

      RETURN
      END

      FUNCTION FZCUT(SLOPE,FINTCP,EROR)
      FZCUT=(SLOPE*EROR)+FINTCP
      RETURN
      END

      FUNCTION FZMIN(VALU1,VALU2)
      IF(VALU2.LT.VALU1)THEN
        FZMIN=VALU2
      ELSE
        FZMIN=VALU1
      ENDIF
      RETURN
      END

      FUNCTION FZMAX(VALU1,VALU2)
      IF(VALU2.GT.VALU1)THEN
        FZMAX=VALU2
      ELSE
        FZMAX=VALU1
      ENDIF
      RETURN
      END

      FUNCTION YCROSS(SLOPE,X,C)
      YCROSS=(SLOPE*X)+C
      RETURN
      END

      FUNCTION XCROSS(ACUT,SLOPE,C)
      PARAMETER (SMALL=1.0E-15)
      IF(C.LT.SMALL)C=SMALL
      XCROSS=(ACUT-C)/SLOPE
      RETURN
      END

