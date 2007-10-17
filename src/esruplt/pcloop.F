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
C      CONTRL: control executive.
C      CLVAR : get sensed condition for single sensor control.
C      CLMVAR: get sensed conditions for multi-sensor control.
C      CLAXSN:  "    "         "      "    "     "       "
C      PCBDAT: get sensed conditions from ascii file.
C      CLFNGR: function generator.
C      CLVARN: non-ideal sensor routine.
C      CTLROP: non-ideal actuator routine.
C      CLACDL: actuator delay routine.
C      PCL00:  component period switch-off control.
C      PCL01:  flux PID control.
C      PCL02:  flow PID control.
C      PCL03:  numerical proportional control.
C      PCL04:  optimum start controller.
C      PCL05:  basic proportional damper position controller.
C      PCL06:  null controller.
C      PCL07:  duty cycle controller.
C      PCL08:  single sensor two-position control.
C      PCL09:  multi-sensor two-position control.
C      PCL10:  zero-energy band controller.
C      PSYSPT: establishes set points for AHU systems.

C ******************** CONTRL ********************

C CONTRL determines plant control status based on most recent results,
C by invoking appropriate control routine (PCL??) for each active plant
C control loop to establish CDATA for current time-step
C
      SUBROUTINE CONTRL(iterp)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/Pctime/TIMSEC
      COMMON/CTLACTN/RINTGRL(MCL,2),DERIV(MCL)

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)
      common/pcfngr/clfgsd(MCL,6),pcfngn,IDAYPD

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      character outs*124
      character*10 wkd1, wkd2
      
      logical pcfngn

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Entering subroutine CONTRL'

C Return if no plant control loops active
      IF(NCL.EQ.0) GOTO 999

C Determine year day number of present day
      IF(IHRP.NE.24) THEN
         IDAY=IDYP
      ELSE
         IDAY=IDYF
      END IF

C Process each plant control loop to test if it is active
      DO 100 ICL=1,NCL
      ICFP=ICL

C Set up day type and period pointers
      NDAYT=NPCDT(ICL)

C If NDAYT=0 set data to weekday, saturday and sunday format
      IF(NDAYT.EQ.0) THEN
         IDTYPP=1
         IDW=IDWP
         IF(IHRP.EQ.24) IDW=IDWF
         IF(IDW.EQ.idwe1) IDTYPP=2
         IF(IDW.EQ.idwe2) IDTYPP=3
      ELSE
         DO 10 IDTYPP=1,NDAYT
         IDS=IPCDV(ICL,IDTYPP,1)
         IDF=IPCDV(ICL,IDTYPP,2)
         IF(IDAY.GE.IDS.AND.IDAY.LE.IDF) GOTO 20
   10    CONTINUE
         WRITE(outs,'(a,I3)') ' CONTRL: plant control loop ',ICL
         call edisp(iuout,outs)
         WRITE(outs,'(9x,a,I4)') 'no valid day type for year-day ',IDAY
         call edisp(iuout,outs)
         call edisp(iuout,' CONTRL: cannot locate appropriate day type')
         call epwait
         call epagend
         STOP
      END IF

C Check number of periods in each day and the start and finish times
   20 NDAYP=NPCDP(ICFP,IDTYPP)
      IF(NDAYP.EQ.0)then
        call edisp(iuout,' CONTRL: no day-periods defined')
        call epwait
        call epagend
        STOP
      endif
      DO 22 IDAYP=1,NDAYP
      IPERP=IDAYP
      IDAYPD=IDAYP
      TPS=TPCPS(ICFP,IDTYPP,IDAYP)
      IF(IDAYP.LT.NDAYP) THEN
         TPF=TPCPS(ICFP,IDTYPP,IDAYP+1)
      ELSE
         TPF=24.
      END IF

C If just starting control loop then initialise 
C integral and derivative action variables to zero.
      IF(PTIMEF.GT.TPS.AND.PTIMEF.LE.TPF) then
         ctltim=ptimef-timsec/3600.0
         if(abs(ctltim-tps).lt.0.000001) then 
            rintgrl(icfp,1)=0.0
            rintgrl(icfp,2)=0.0
            deriv(icfp)=0.0
         endif
         GOTO 30
      endif
   22 CONTINUE
      call edisp(iuout,' CONTRL: cannot locate appropriate day-period')
      call epwait
      call epagend
      STOP

   
C Valid period established; now invoke appropriate control law routine
C Plant control law 0: ie shut down the plant
   30 IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.0) THEN
         CALL PCL00

C Plant control law 1:  proportional, integral and
C derivative heating/cooling flux controller.
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.1) THEN
         CALL PCL01(iterp)

C Plant control law 2:  proportional, integral and
C derivative flow controller.
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.2) THEN
         CALL PCL02(iterp)

C Plant control law 3: basic proportional numerical value controller
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.3) THEN
         CALL PCL03

C Plant control law 4: optimum start controller
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.4) THEN
         CALL PCL04

C Plant control law 5: basic proportional damper position controller.
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.5) THEN
         CALL PCL05

C Plant control law 6: null controller.
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.6) THEN
         CALL PCL06

C Plant control law 7: duty cycle controller.
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.7) THEN
         CALL PCL07

C Plant control law 8: single sensor two-position control.
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.8) THEN
         CALL PCL08

C Plant control law 9: multi-sensor two-position control.
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.9) THEN
         CALL PCL09

C Plant control law 10: CCHP cogen system controller
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.10) THEN
         CALL DG_controller(iterp)

C Plant control law 11: Hydrogen demand controller
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.11) THEN
         CALL H2_demand_controller ( iterp )

C Plant cotrol law 12: Derive signal from boundary condition
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.12) THEN
         CALL Boundry_Condition_Control ()

C Plant control law 13: Adsorption storage unit controller
      ELSE IF(IPCLAW(ICFP,IDTYPP,IPERP).EQ.13) THEN
         CALL ADS_stg_controller(iterp)
         
C New controllers inserted here!
      ELSE
        call edisp(iuout,
     &    ' CONTRL: invalid plant control law has been referenced')
        call epwait
        call epagend
        STOP
      END IF

  100 CONTINUE
  


C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Leaving subroutine CONTRL'

  999 CONTINUE 
  
C MIMO Control: 
      call MIMO_Apply_Plt_Loops( )
      
      RETURN
      END

C ******************** CLVAR ********************

C CLVAR locates the sensor, checks if the sensor corresponds with
C prevailing controller type, determines sensed condition (SVCTL),
C actuated plant component (IPCMP), and number of this plant
C component type's control variable (IPCVR)

      SUBROUTINE CLVAR(SVCTL,IPCMP,IPCVR)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)

      common/pcfngr/clfgsd(MCL,6),pcfngn,IDAYPD
      COMMON/PTIME/PTIMEP,PTIMEF

      character outs*124
      
      logical pcfngn,svcfgr
      
      svcfgr=.false.

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Entering subroutine CLVAR'

C Locate the actuated plant component and node therein
      IF(IPAN(ICFP,1).NE.-1) THEN
         WRITE(outs,'(a,I5)') ' CLVAR: plant control loop ',ICFP
         call edisp(iuout,outs)
         call edisp(iuout,
     &    ' CLVAR: not yet active actuator location referenced')
         call epwait
         call epagend
         STOP
      END IF
      IPCMP=IPAN(ICFP,2)
      IPCVR=IPAN(ICFP,3)

C Establish sensor location
      IPS1=IPSN(ICFP,1)
      IPS2=IPSN(ICFP,2)
      IPS3=IPSN(ICFP,3)

      IF(IPS1.EQ.-6.AND.IPS2.EQ.9)THEN
        IPS1=int(clfgsd(icfp,1))
        IPS2=int(clfgsd(icfp,2))
        IPS3=int(clfgsd(icfp,3))
        SVCFGR=.TRUE.
      ENDIF

C Match sensor capability to plant controller type; ie. at present
C only IPS1=-1,-5, or -6 sensor allows a signal other than temperature
      ICLCTP=IPCTYP(ICFP,IDTYPP,IPERP)
      IF(IPS1.NE.-1.AND.IPS1.NE.-5.AND.IPS1.NE.-6
     &             .AND.(ICLCTP.NE.0.AND.ICLCTP.NE.1
     &             .AND.ICLCTP.NE.12.AND.ICLCTP.NE.18))then
         call edisp(iuout,
     &    ' CLVAR: invalid sensor for plant loop controller type ')
         call epwait
         call epagend
         STOP
      endif

C Determine sensed condition depending on the sensor location
C Sensor measures building zone related temperature
      IF(IPS1.GT.0) THEN
         SVCTL=TFA(IPS1)
         IF(IPS2.GT.0.AND.IPS3.EQ.0) SVCTL=TFS(IPS1,IPS2)
         IF(IPS2.GT.0.AND.IPS3.GT.0) SVCTL=TFC(IPS1,IPS2,IPS3)

C Sensor measures plant component containment loss
      ELSE IF(IPS1.EQ.-1.AND.IPS3.EQ.-1) THEN
         SVCTL=QDATA(IPS2)

C Sensor measures plant node state-space variable
      ELSE IF(IPS1.EQ.-1.AND.IPS3.GT.0) THEN
         IN=NPCDAT(IPS2,9)+IPS3-1

C Jump depending on sensed property for control loop controller type
C Controller type 0, 1, 12 or 18: senses temperature
         IF(ICLCTP.EQ.0.OR.ICLCTP.EQ.1.OR.ICLCTP.EQ.12.or.
     &      iclctp.eq.18) THEN
            SVCTL=CSVF(IN,1)

C Controller type 2, 3, 13 or 19: senses fluid enthalpy (kJ/kg) which is
C                         calculated from temp. and specific heat
C                         Note that ISV=10   indicates water
C                                   ISV=1,11 indicates dry air
C                                   ISV=21   indicates moist air
         ELSE IF(ICLCTP.EQ.2.OR.ICLCTP.EQ.3.OR.ICLCTP.EQ.13.or.
     &           iclctp.eq.19) THEN
            IF(ISV(IPS2,IPS3).EQ.10) THEN
               SVCTL=CSVF(IN,1)*SHTH2O(CSVF(IN,1))
            ELSE IF(ISV(IPS2,IPS3).EQ.1.OR.
     &              ISV(IPS2,IPS3).EQ.11) THEN
               SVCTL=ENTHP2(CSVF(IN,1),0.)
            ELSE IF(ISV(IPS2,IPS3).EQ.21) THEN
               SVCTL=ENTHP2(CSVF(IN,1),CSVF(IN,3)/CSVF(IN,2))
            ELSE
              WRITE(outs,'(a,I4,a,i4)') ' CLVAR: loop ',ICFP,
     &                                  ' type ',ICLCTP
              call edisp(iuout,outs)
              call edisp(iuout,
     &          ' CLVAR: invalid SSV for plant controller type ')
              call epwait
              call epagend
              STOP
            END IF

C Controller type 4, 5, 14 or 20: senses 1th phase mass flow rate (kg/s)
         ELSEIF(ICLCTP.EQ.4.OR.ICLCTP.EQ.5.OR.ICLCTP.EQ.14.or.
     &           iclctp.eq.20) THEN
            IF(ISV(IPS2,IPS3).LT.10)then
              call edisp(iuout,
     &          ' CLVAR: invalid SSV for plant controller type ')
              call epwait
              call epagend
              STOP
            endif
            SVCTL=CSVF(IN,2)

C Controller type 6, 7, 15 or 21: senses 2nd phase mass flow rate (kg/s)
         ELSE IF(ICLCTP.EQ.6.OR.ICLCTP.EQ.7.OR.ICLCTP.EQ.15.or.
     &           iclctp.eq.21) THEN
            IF(ISV(IPS2,IPS3).LT.21)then
              call edisp(iuout,
     &          ' CLVAR: invalid SSV for plant controller type ')
              call epwait
              call epagend
              STOP
            endif
            SVCTL=CSVF(IN,3)

C Controller type 8, 9, 16 or 22: senses some additional plant output
         ELSE IF(ICLCTP.EQ.8.OR.ICLCTP.EQ.9.OR.ICLCTP.EQ.16.or.
     &           iclctp.eq.22) THEN
            SVCTL=PCAOUT(IPS2,IPS3)

C Controller type 10, 11, 17 or 23: senses relative humidity (%).
         ELSE IF(ICLCTP.EQ.10.OR.ICLCTP.EQ.11.OR.ICLCTP.EQ.17.or.
     &           iclctp.eq.23) THEN
            PATMOS=1013.25
            SVCTL=PCRH2(CSVF(IN,1),CSVF(IN,3)/CSVF(IN,2),PATMOS)
         ELSE
            call edisp(iuout,' CLVAR: invalid plant controller type ')
            call epwait
            call epagend
            STOP
         END IF

C Sensor measures a mix of zone air and mean radiant temperature
      ELSE IF(IPS1.EQ.-2) THEN
         CALL MZMIXT(IPS2,TMRT,TMIX)
         CONV=FLOAT(IPS3)/100.
         SVCTL=TFA(IPS2)*CONV+TMRT*(1.-CONV)

C Sensor measures outside condition.
      ELSE IF(IPS1.EQ.-3) THEN
         IF(IPS2.EQ.0.AND.IPS3.EQ.0) THEN
            SVCTL=TFP
         ELSEIF(IPS2.EQ.-1.AND.IPS3.EQ.0) THEN
            SVCTL=SOLAIR(TFP,QFFP,QDFP)
         ELSEIF(IPS2.EQ.-2.AND.IPS3.EQ.0) THEN
            SVCTL=QFFP
         ELSEIF(IPS2.EQ.-3.AND.IPS3.EQ.0) THEN
            SVCTL=QDFP
         ELSEIF(IPS2.EQ.-4.AND.IPS3.EQ.0) THEN
            SVCTL=VFP
         ELSEIF(IPS2.EQ.-5.AND.IPS3.EQ.0) THEN
            SVCTL=DFP
         ELSEIF(IPS2.EQ.-6.AND.IPS3.EQ.0) THEN
            SVCTL=HFP
         ELSE
            WRITE(outs,'(a,I4,a)') ' CLVAR: loop ',ICFP,
     &                             '; outside conditions'
            call edisp(iuout,outs)
            call edisp(iuout,
     &          ' CLVAR: plant control sensor not yet active ')
            call epwait
            call epagend
            STOP
         END IF

C Sensor values are read from a boundry data file.
      ELSE IF(IPS1.EQ.-5) THEN
           IDTCOL=IPSN(ICFP,2)
           CALL PCBDAT(idtcol,svctl)

C Sensor values are provided by a function generator.
      ELSE IF(IPS1.EQ.-6) THEN
           IFUNCT=IPS2
           CALL CLFNGR(IFUNCT,SVCTL)

      ELSE
         call edisp(iuout,' CLVAR: illegal plant-side sensor rfrncd ')
         call epwait
         call epagend
         STOP
      END IF

      IF(svcfgr)THEN
         IFUNCT=9
         CALL CLFNGR(IFUNCT,SVCTL)
      ENDIF
      
      IPS4=IPSN(ICFP,4)
      if(ips4.eq.-1)then
         call CLVARN(SVCTL,SVCTLA)
         SVCTL=SVCTLA
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' Control variable              = ',SVCTL
         WRITE(ITU,*) ' Actuated component & variable = ',IPCMP,IPCVR
         WRITE(ITU,*) ' Leaving subroutine CLVAR'
      END IF

      RETURN
      END

C ******************** CLMVAR ********************

C CLMVAR locates the auxiliary sensor(s); checks for 
C correspondence with prevailing controller type; and
C determines sensed condition SVCTLM(?,?).

      SUBROUTINE CLMVAR
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/CPMVAR/NPSEN(MCL),ISMODP(MCL),IPMSN(MCL,MPSEN,5),
     &SVCTLM(MCL,MPSEN),SNWGTP(MPSEN)
      character outs*124

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Entering subroutine CLMVAR'

      DO 10 J=1,NPSEN(ICFP)

C Establish sensor location
        IPMS1=IPMSN(ICFP,J,1)
        IPMS2=IPMSN(ICFP,J,2)
        IPMS3=IPMSN(ICFP,J,3)

C Match sensor capability to plant controller type; ie. at present
C only IPMS1=-1 sensor allows a signal other than temperature
        ICLCTP=IPCTYP(ICFP,IDTYPP,IPERP)
        IF(IPMS1.NE.-1.AND.(ICLCTP.NE.0.AND.ICLCTP.NE.1
     &             .AND.ICLCTP.NE.12.AND.ICLCTP.NE.18))then
           call edisp(iuout,
     &      ' CLMVAR: invalid auxl`y sensor for plant loop controller 
     &     type ')
           call epwait
           call epagend
          STOP
        endif

C Determine sensed condition depending on the sensor location
C Sensor measures building zone related temperature
        IF(IPMS1.GT.0) THEN
           SVCTLM(ICFP,J)=TFA(IPMS1)
           IF(IPMS2.GT.0.AND.IPMS3.EQ.0) SVCTLM(ICFP,J)=TFS(IPMS1,IPMS2)
           IF(IPMS2.GT.0.AND.IPMS3.GT.0) SVCTLM(ICFP,J)=
     &                                          TFC(IPMS1,IPMS2,IPMS3)

C Sensor measures plant component containment loss
        ELSE IF(IPMS1.EQ.-1.AND.IPMS3.EQ.-1) THEN
           SVCTLM(ICFP,J)=QDATA(IPMS2)

C Sensor measures plant node state-space variable
        ELSE IF(IPMS1.EQ.-1.AND.IPMS3.GT.0) THEN
           IN=NPCDAT(IPMS2,9)+IPMS3-1

C Jump depending on sensed property for control loop controller type
C Controller type 0, 1, 12 or 18: senses temperature
           IF(ICLCTP.EQ.0.OR.ICLCTP.EQ.1.OR.ICLCTP.EQ.12.or.
     &        iclctp.eq.18) THEN
              SVCTLM(ICFP,J)=CSVF(IN,1)

C Controller type 2, 3, 13 or 19: senses fluid enthalpy (kJ/kg) which is
C                         calculated from temp. and specific heat
C                         Note that ISV=10   indicates water
C                                   ISV=1,11 indicates dry air
C                                   ISV=21   indicates moist air
           ELSE IF(ICLCTP.EQ.2.OR.ICLCTP.EQ.3.OR.ICLCTP.EQ.13.or.
     &             iclctp.eq.19) THEN
              IF(ISV(IPMS2,IPMS3).EQ.10) THEN
                 SVCTLM(ICFP,J)=CSVF(IN,1)*SHTH2O(CSVF(IN,1))
              ELSE IF(ISV(IPMS2,IPMS3).EQ.1.OR.
     &                ISV(IPMS2,IPMS3).EQ.11) THEN
                 SVCTLM(ICFP,J)=ENTHP2(CSVF(IN,1),0.)
              ELSE IF(ISV(IPMS2,IPMS3).EQ.21) THEN
                 SVCTLM(ICFP,J)=ENTHP2(CSVF(IN,1),CSVF(IN,3)/CSVF(IN,2))
              ELSE
                WRITE(outs,'(a,I4,a,i4)') ' CLMVAR: loop ',ICFP,
     &                                    ' type ',ICLCTP
                call edisp(iuout,outs)
                call edisp(iuout,
     &            ' CLMVAR: invalid SSV for plant controller type ')
                call epwait
                call epagend
                STOP
              END IF

C Controller type 4, 5, 14 or 20: senses 1th phase mass flow rate (kg/s)
           ELSE IF(ICLCTP.EQ.4.OR.ICLCTP.EQ.5.OR.ICLCTP.EQ.14.or.
     &             iclctp.eq.20) THEN
              IF(ISV(IPMS2,IPMS3).LT.10)then
                call edisp(iuout,
     &            ' CLMVAR: invalid SSV for plant controller type ')
                call epwait
                call epagend
                STOP
              endif
              SVCTLM(ICFP,J)=CSVF(IN,2)

C Controller type 6, 7, 15 or 21: senses 2nd phase mass flow rate (kg/s)
           ELSE IF(ICLCTP.EQ.6.OR.ICLCTP.EQ.7.OR.ICLCTP.EQ.15.or.
     &             iclctp.eq.21) THEN
              IF(ISV(IPMS2,IPMS3).LT.21)then
                call edisp(iuout,
     &            ' CLMVAR: invalid SSV for plant controller type ')
                call epwait
                call epagend
                STOP
              endif
              SVCTLM(ICFP,J)=CSVF(IN,3)

C Controller type 8, 9, 16 or 22: senses some additional plant output
           ELSE IF(ICLCTP.EQ.8.OR.ICLCTP.EQ.9.OR.ICLCTP.EQ.16.or.
     &             iclctp.eq.22) THEN
              SVCTLM(ICFP,J)=PCAOUT(IPMS2,IPMS3)

C Controller type 10, 11, 17 or 23: senses relative humidity (%).
           ELSE IF(ICLCTP.EQ.10.OR.ICLCTP.EQ.11.OR.ICLCTP.EQ.17.or.
     &             iclctp.eq.23) THEN
              PATMOS=1013.25
              SVCTLM(ICFP,J)=
     &                    PCRH2(CSVF(IN,1),CSVF(IN,3)/CSVF(IN,2),PATMOS)

           ELSE
              call edisp(iuout,' CLMVAR: invalid plant controller type')
              call epwait
              call epagend
              STOP
           END IF

C Sensor measures a mix of zone air and mean radiant temperature
        ELSE IF(IPMS1.EQ.-2) THEN
           CALL MZMIXT(IPMS2,TMRT,TMIX)
           CONV=FLOAT(IPMS3)/100.
           SVCTLM(ICFP,J)=TFA(IPMS2)*CONV+TMRT*(1.-CONV)

C Sensor measures outside condition.
        ELSE IF(IPMS1.EQ.-3) THEN
           IF(IPMS2.EQ.0.AND.IPMS3.EQ.0) THEN
              SVCTLM(ICFP,J)=TFP
           ELSEIF(IPMS2.EQ.-1.AND.IPMS3.EQ.0) THEN
              SVCTLM(ICFP,J)=SOLAIR(TFP,QFFP,QDFP)
           ELSEIF(IPMS2.EQ.-2.AND.IPMS3.EQ.0) THEN
              SVCTLM(ICFP,J)=QFFP
           ELSEIF(IPMS2.EQ.-3.AND.IPMS3.EQ.0) THEN
              SVCTLM(ICFP,J)=QDFP
           ELSEIF(IPMS2.EQ.-4.AND.IPMS3.EQ.0) THEN
              SVCTLM(ICFP,J)=VFP
           ELSEIF(IPMS2.EQ.-5.AND.IPMS3.EQ.0) THEN
              SVCTLM(ICFP,J)=DFP
           ELSEIF(IPMS2.EQ.-6.AND.IPMS3.EQ.0) THEN
              SVCTLM(ICFP,J)=HFP
           ELSE
              WRITE(outs,'(a,I4,a)') ' CLMVAR: loop ',ICFP,
     &                               '; outside conditions'
              call edisp(iuout,outs)
              call edisp(iuout,
     &            ' CLMVAR: plant control sensor not yet active ')
              call epwait
              call epagend
              STOP
           END IF

C Sensor values are read from a boundry data file.
        ELSE IF(IPMS1.EQ.-5) THEN
           IDTCOL=IPMSN(ICFP,J,2)
           CALL PCBDAT(idtcol,svctl)
           SVCTLM(ICFP,J)=SVCTL

C Sensor values are provided by a function generator.
      ELSE IF(IPMS1.EQ.-6) THEN
           call edisp(iuout,' CLMVAR: -6 type AUXILIARY sensor
     &          not available with this version.')
           call epwait
           call epagend
           STOP

        ELSE
           call edisp(iuout,' CLMVAR: illegal plant-side auxiliary
     &          sensor referenced ')
            call epwait
            call epagend
            STOP
        END IF
10    CONTINUE

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' NSINC = ',nsinc
         DO 20 J=1,NPSEN(ICFP)
           WRITE(ITU,*)' Auxil`y sensed variable     = ',SVCTLM(ICFP,J)
20       CONTINUE
         WRITE(ITU,*) ' Leaving subroutine CLMVAR'
      END IF

      RETURN
      END

C ******************** CLAXSN ********************
C CLAXSN evaluates and returns the (user-specified) 
C auxiliary sensor function in the case of 
C multi-sensor control.

      SUBROUTINE CLAXSN(IAUXFN,SVCTLX)
#include "plant.h"
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP

      COMMON/CPMVAR/NPSEN(MCL),ISMODP(MCL),IPMSN(MCL,MPSEN,5),
     &SVCTLM(MCL,MPSEN),SNWGTP(MPSEN)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Entering subroutine CLAXSN'

      SVCTOT=0.0
      SVCWGT=0.0

C IAUXFN is the auxiliary sensor function flag:-
C IAUXFN = 1: greatest value of auxiliary sensors;
C IAUXFN = 2: least value of auxiliary sensors;
C IAUXFN = 3: mean value of auxiliary sensors.
C IAUXFN = 4: weighting of auxiliary sensors.

      DO 10 J=1,NPSEN(ICFP)
C Determine the greatest auxiliary sensor value;
         IF(J.EQ.1)SVCTHI=SVCTLM(ICFP,J)
         IF(SVCTLM(ICFP,J).GT.SVCTHI)SVCTHI=SVCTLM(ICFP,J)
C Determine the lowest auxiliary sensor value;
         IF(J.EQ.1)SVCTLO=SVCTLM(ICFP,J)
         IF(SVCTLM(ICFP,J).LT.SVCTLO)SVCTLO=SVCTLM(ICFP,J)
C Determine the total of the auxiliary sensor values (required for mean)
         SVCTOT=SVCTOT+SVCTLM(ICFP,J)
C Determine the weighted auxiliary sensor value.
         SVCWGT=SVCWGT+(SNWGTP(J)*SVCTLM(ICFP,J))*0.01
10    CONTINUE

C Establish the mean sensed temperature value;
      SVCAVE=SVCTOT/(NPSEN(ICFP))

      IF(IAUXFN.EQ.1)THEN
         SVCTLX=SVCTHI
      ELSEIF(IAUXFN.EQ.2)THEN
         SVCTLX=SVCTLO
      ELSEIF(IAUXFN.EQ.3)THEN
         SVCTLX=SVCAVE
      ELSEIF(IAUXFN.EQ.4)THEN
         SVCTLX=SVCWGT
      ENDIF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' Resultant aux`ly sensed condition  =   ',SVCTLX
         WRITE(ITU,*) ' Leaving subroutine CLAXSN'
      END IF

      RETURN
      END

C ******************** PCBDAT

      SUBROUTINE PCBDAT(idtcol,svctl)
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PSTCTR/NSINCP,NSNCPR
       
      COMMON/FILEP/IFIL

      COMMON/C6/INDCFG
      COMMON/PBYDAT/PBYFIL,NSCVP,IPS
      COMMON/PTIME/PTIMEP,PTIMEF

      dimension valu(24)
      character outstr*124
      logical PBYFIL

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' ENTERING SUBROUTINE PCBDAT'
      ENDIF

      if(INDCFG.eq.2)then
         iunit=ifil+5
      else
         iunit=ifil+9
      endif

      if(nsincp.eq.1.and.ips.eq.1)then
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'bound file skip',IER)
        if(ier.ne.0)goto 888
      endif
      
      if(NSCVP.eq.nsincp)then
         backspace(iunit)
      else
         ips=ips+1
      endif

      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'bound file skip',IER)
      if(ier.ne.0)goto 888
      if(ND.eq.0)then
        call edisp(iuout,' Found a blank line... ')
        return
      endif
      K=0
      do 20 i=1,ND
        valu(i)=0.0
        CALL EGETWR(OUTSTR,K,valu(i),0.,0.,'-',
     &      'timestep boundry values',IER)
        if(ier.ne.0)goto 888
  20  continue
      svctl=valu(idtcol)

C Remember nsincp.
      NSCVP=nsincp

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0)THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' NSINC = ',nsinc,         
     &                ' SVCTL = ',svctl
         write(itu,*) ' Leaving subroutine PCBDAT'
      ENDIF

      return

888   write(iuout,*)' BOUDAT: EOF'
      CALL ERPFREE(IUNIT,ISTAT)
      return
      end

C ******************** CLFNGR ********************

C CLFNGR is a (user-defined) function generator.

      SUBROUTINE CLFNGR(IFNCT,SVCTL)
#include "building.h"
#include "plant.h"

      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      common/pclsol/icfp,idtypp,iperp

      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),npcdt(mcl),
     &            ipcdv(mcl,mpcdt,2),npcdp(mcl,mpcdt),
     &            tpcps(mcl,mpcdt,mpcdp),ipctyp(mcl,mpcdt,mpcdp),
     &            ipclaw(mcl,mpcdt,mpcdp),pmiscd(mcl,mpcdt,mpcdp,misc)

      common/pcfngr/clfgsd(MCL,6),pcfngn,IDAYPD
     
      logical pcfngn

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine CLFNGR'
      END IF

C Type 1: Step function generator.
      if(ifnct.eq.1) then

C Assign data as defined in table above.
         start =clfgsd(icfp,1)
         finish=clfgsd(icfp,2)
         vmax  =clfgsd(icfp,3)
         vmin  =clfgsd(icfp,4)

C Determine function shape.
         svctl=vmin
         if(ptimef.gt.start.and.ptimef.lt.finish) svctl=vmax

C Type 2: ramp function generator.
      elseif(ifnct.eq.2) then

C Assign data as defined in table above.
         start =clfgsd(icfp,1)
         finish=clfgsd(icfp,2)
         vmax  =clfgsd(icfp,3)
         vmin  =clfgsd(icfp,4)

C Determine function shape.
         svctl=vmin
         if(ptimef.gt.start.and.ptimef.lt.finish) then

C Calculate ratio for interpolation.
            ratio=(vmax-vmin)/(finish-start)

C Calculate value by interpolation.
            svctl=vmax-ratio*(finish-ptimef)
         endif

C Type 3: Square wave, sine series.
      elseif(ifnct.eq.3) then

C Assign data as defined in table above.
         vmax=clfgsd(icfp,1)
         vmin=clfgsd(icfp,2)
         freq=clfgsd(icfp,3)

C Find control period start and finish hour.
         NDAYP=NPCDP(ICFP,IDTYPP)
         TPS=TPCPS(ICFP,IDTYPP,IDAYPD)
         IF(IDAYPD.LT.NDAYP) THEN
            TPF=TPCPS(ICFP,IDTYPP,IDAYPD+1)
         ELSE
           TPF=24.
         END IF

C Find amplitude and mean value of wave.
         amp=(vmax-vmin)/2.
         vmean=(vmax+vmin)/2.

C Now evaluate sine series.
         pi=4.0*atan(1.0)
         sinsum=0.0
         half=(tps+tpf)/2.0
         f=1.0

C Approximate series with 100 terms.
         do 10 iterm=1, 100
            sinsum=1.0/f * sin((pi*ptimef*f*freq/half))+sinsum
            f=f+2.0
   10    continue
         svctl=sinsum*(4.0*amp)/pi+vmean

C Type 4: Square wave, cosine series.
      elseif(ifnct.eq.4) then

C Assign data as defined in table above.
         vmax=clfgsd(icfp,1)
         vmin=clfgsd(icfp,2)
         freq=clfgsd(icfp,3)

C Find control period start and finish hour.
         NDAYP=NPCDP(ICFP,IDTYPP)
         TPS=TPCPS(ICFP,IDTYPP,IDAYPD)
         IF(IDAYPD.LT.NDAYP) THEN
            TPF=TPCPS(ICFP,IDTYPP,IDAYPD+1)
         ELSE
           TPF=24.
         END IF

C Find amplitude and mean value of wave.
         amp=(vmax-vmin)/2.
         vmean=(vmax+vmin)/2.

C Now evaluate sine series.
         pi=4.0*atan(1.0)
         cossum=0.0
         half=(tps+tpf)/2.0
         f=1.0
         isign=1

C Approximate series with 100 terms.
         do 20 iterm=1, 100
            cossum=isign * 1.0/f * cos((pi*ptimef*f*freq/half))+cossum
            f=f+2.0
            isign=isign*(-1)
   20    continue
         svctl=cossum*(4.0*amp)/pi+vmean

C Type 5: Triangular wave.
      elseif(ifnct.eq.5) then

C Assign data as defined in table above.
         vmax=clfgsd(icfp,1)
         vmin=clfgsd(icfp,2)
         freq=clfgsd(icfp,3)

C Find control period start and finish hour.
         NDAYP=NPCDP(ICFP,IDTYPP)
         TPS=TPCPS(ICFP,IDTYPP,IDAYPD)
         IF(IDAYPD.LT.NDAYP) THEN
            TPF=TPCPS(ICFP,IDTYPP,IDAYPD+1)
         ELSE
           TPF=24.
         END IF

C Find amplitude and mean value of wave.
         amp=(vmax-vmin)/2.
         vmean=(vmax+vmin)/2.

C Now evaluate sine series.
         pi=4.0*atan(1.0)
         cossum=0.0
         half=(tps+tpf)/2.0
         f=1.0

C Approximate series with 100 terms.
         do 30 iterm=1, 100
            cossum=1.0/(f)**2 * cos((pi*ptimef*f*freq/half))+cossum
            f=f+2.0
   30    continue
         svctl=cossum*(8.0*amp)/(pi)**2 +vmean

C Type 6: Saw tooth wave.
      elseif(ifnct.eq.6) then

C Assign data as defined in table above.
         vmax=clfgsd(icfp,1)
         vmin=clfgsd(icfp,2)
         freq=clfgsd(icfp,3)

C Find control period start and finish hour.
         NDAYP=NPCDP(ICFP,IDTYPP)
         TPS=TPCPS(ICFP,IDTYPP,IDAYPD)
         IF(IDAYPD.LT.NDAYP) THEN
            TPF=TPCPS(ICFP,IDTYPP,IDAYPD+1)
         ELSE
           TPF=24.
         END IF

C Find amplitude and mean value of wave.
         amp=(vmax-vmin)/2.
         vmean=(vmax+vmin)/2.

C Now evaluate sine series.
         pi=4.0*atan(1.0)
         sinsum=0.0
         half=(tps+tpf)/2.0
         f=1.0
         isign=1

C Approximate series with 100 terms.
         do 40 iterm=1, 100
            sinsum=isign * 1.0/f * sin((pi*ptimef*f*freq/half))+sinsum
            f=f+1.0
            isign=isign*(-1)
   40    continue
         svctl=sinsum*(2.0*amp)/pi +vmean

C Type 7: Sine wave.
      elseif(ifnct.eq.7) then

C Assign data as defined in table above.
         vmax=clfgsd(icfp,1)
         vmin=clfgsd(icfp,2)
         freq=clfgsd(icfp,3)
         shift=clfgsd(icfp,4)

C Find control period start and finish hour.
         NDAYP=NPCDP(ICFP,IDTYPP)
         TPS=TPCPS(ICFP,IDTYPP,IDAYPD)
         IF(IDAYPD.LT.NDAYP) THEN
            TPF=TPCPS(ICFP,IDTYPP,IDAYPD+1)
         ELSE
           TPF=24.
         END IF

C Find amplitude and mean value of wave.
         amp=(vmax-vmin)/2.

C Now evaluate sine series.
         pi=4.0*atan(1.0)
         half=(tps+tpf)/2.0
         svctl=amp*(sin(pi*freq*(ptimef-shift)/half)+1.0) + vmin

C Type 8: Cosine wave.
      elseif(ifnct.eq.8) then

C Assign data as defined in table above.
         vmax=clfgsd(icfp,1)
         vmin=clfgsd(icfp,2)
         freq=clfgsd(icfp,3)
         shift=clfgsd(icfp,4)

C Find control period start and finish hour.
         NDAYP=NPCDP(ICFP,IDTYPP)
         TPS=TPCPS(ICFP,IDTYPP,IDAYPD)
         IF(IDAYPD.LT.NDAYP) THEN
            TPF=TPCPS(ICFP,IDTYPP,IDAYPD+1)
         ELSE
           TPF=24.
         END IF

C Find amplitude and mean value of wave.
         amp=(vmax-vmin)/2.

C Now evaluate sine series.
         pi=4.0*atan(1.0)
         half=(tps+tpf)/2.0
         svctl=amp*(cos(pi*freq*(ptimef-shift)/half)+1.0) + vmin

C Type 9: Use sensed property svctl.
      elseif(ifnct.eq.9) then
         sclfac=clfgsd(icfp,5)
         offset=clfgsd(icfp,6)
         svctl=(sclfac*svctl)+offset
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) then
         write(itu,'(I5)') ' Function type is',ifnct
         write(itu,'(F10.4)') ' PTIMEF=',ptimef
         write(itu,'(F10.4)') ' SVCTL =',svctl
         WRITE(ITU,*) ' Leaving subroutine CLFNGR'
      endif
      
      return
      end

C ******************** CLVARN ********************

C CLVARN imposes non-ideal sensor characteristics on 
C the sensed variable returned from CFVAR routine.

      SUBROUTINE CLVARN(SVCT,SVCTLN)
#include "plant.h"
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      COMMON/Pctime/TIMSEC

      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)

      common/senact/realsn(MNONID,3,7),realac(MNONID,5,7),svctlp,
     &       deltap,ctldrp
      
      PARAMETER (SMALL=1.0E-20)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Entering subroutine CLVARN'

C Initialise variables.
      svctln=svct
      offset=0.
      adjust=0.
      ifunct=0

      ipa2=ipan(icfp,2)
      ipa3=ipan(icfp,3)

      do 100 j=1,mnonid
        if(ipa2.eq.int(realsn(j,1,1)).and.ipa3.eq.int(realsn(j,1,2)))
     &     ifunct=j
100   continue
      if(ifunct.eq.0)return

      rnsinc=float(nsinc)
      tim=timsec*rnsinc

C Consider each non-ideal characteristic in turn.
      if(int(realsn(ifunct,1,3)).eq.1)then

C Offset non-ideal characteristic superimposed.
C co = fixed offset
C c1 = linear rate of increase
C amp = amplitude of periodic variation in offset
C per = period of periodic variation in offset

        c0=realsn(ifunct,1,4)
        c1=realsn(ifunct,1,5)
        amp=realsn(ifunct,1,6)
        per=realsn(ifunct,1,7)
        pi=3.14
         
        offset=c0+(c1*tim)+(amp*sin((2.*pi*tim)/per))

      endif

      if(int(realsn(ifunct,2,3)).eq.1)then
C First order time lag characteristic superimposed.
C rtau = user-defined time-constant
C svctlg = sensed condition with lag imposed
C svct = ideal sensed condition at present time-step
C svctlp = sensed condition saved from previous time-step
C deltat = time-step
C tim = simulation time

        rtau=realsn(ifunct,2,4)
        adjust=-1.*(svct-svctlp)*exp(-1.*(timsec/rtau))
        svctlg=svct+adjust
        svctlp=svctlg
      endif

C Now superimpose characteristics on ideal condition.
      svctln=svct+offset+adjust

      if(int(realsn(ifunct,3,3)).eq.1)then
C Fixed time delay characteristic superimposed.
C ndelay = user-defined time lag (time-steps)

        ndelay=int(realsn(ifunct,3,4))
        if(ndelay.gt.0)then
           call clsndl(ndelay,svctln,svctr,ifunct)        
           svctln=svctr
        endif
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' Ideal control variable              = ',SVCT
         WRITE(ITU,*) ' Adjusted control variable',SVCTLN
         WRITE(ITU,*) ' Leaving subroutine CLVARN'
      END IF

      RETURN
      END

C ******************** CTLROP ********************

C CTLROP imposes non-ideal actuator characteristics on 
C the controller output sent from the calling routine.

      SUBROUTINE CTLROP
#include "plant.h"
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      COMMON/Pctime/TIMSEC

      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)

      common/senact/realsn(MNONID,3,7),realac(MNONID,5,7),svctlp,
     &       deltap,ctldrp
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      
      PARAMETER (SMALL=1.0E-20)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Entering subroutine CTLROP'

C Initialise variables.
      ifunct=0
      offset=0.
      delta=0.
      prelod=0.
      preout=0.
      sticon=0.
      sticop=0.

      ipa2=ipan(icfp,2)
      ipa3=ipan(icfp,3)
      ctlrd=cdata(ipa2,ipa3)

      do 100 j=1,mnonid
        if(ipa2.eq.int(realac(j,1,1)).and.ipa3.eq.int(realac(j,1,2)))
     &     ifunct=j
100   continue
      if(ifunct.eq.0)return

      rnsinc=float(nsinc)
      tim=timsec*rnsinc

C Consider each non-ideal characteristic in turn.
      if(int(realac(ifunct,1,3)).eq.1)then

C Offset non-ideal characteristic superimposed.
C co = fixed offset
C c1 = linear rate of increase
C amp = amplitude of periodic variation in offset
C per = period of periodic variation in offset

        c0=realac(ifunct,1,4)
        c1=realac(ifunct,1,5)
        amp=realac(ifunct,1,6)
        per=realac(ifunct,1,7)
        pi=3.14
        offset=c0+(c1*tim)+(amp*sin((2.*pi*tim)/per))

      endif

      if(int(realac(ifunct,2,3)).eq.1)then
C Pre-load non-ideal characteristic superimposed.
C p = preload discontinuity when the input has zero value.
C Otherwise output varies according to linear relationship 
C with slope, pk.
        prel=realac(ifunct,2,4)
        pk=realac(ifunct,2,5)
        
        if(ctlrd.gt.0.)then
           preout=prel+(pk*ctlrd)
        else
           preout=-prel+(pk*ctlrd)
        endif
        prelod=preout-ctlrd
      endif      

      if(int(realac(ifunct,3,3)).eq.1)then
C Stiction non-ideal characteristic superimposed.
C The output is zero between specified input values. 
C Otherwise output varies according to linear relationship 
C with slope, sk.

        stic=realac(ifunct,3,4)
        sk=realac(ifunct,3,5)
        if(ctlrd.lt.stic.and.ctlrd.gt.(-1.*stic))then
           sticop=0.
        else
           sticop=ctlrd*sk
        endif        
        sticon=sticop-ctlrd
      endif

      if(int(realac(ifunct,4,3)).eq.1)then
C Hysteresis non-liner characteristic superimposed.
C h= hysteresis.

         h=realac(ifunct,4,4)
         if(ctlrd.gt.ctldrp)then
            delta=amax1(deltap+ctldrp-ctlrd,-1*h)
         else
            delta=amin1(deltap+ctldrp-ctlrd,h)
         endif
C Present values become previous values at next time-step.
         deltap=delta
         ctldrp=ctlrd
      endif

C Now superimpose characteristics on ideal condition.
      ctlrdn=ctlrd+prelod+sticon+delta

      if(int(realac(ifunct,5,3)).eq.1)then
C Fixed time delay characteristic superimposed.
C ndelay = user-defined time lag (time-steps)

        ndelay=int(realac(ifunct,5,4))
        if(ndelay.gt.0)then
           call clacdl(ipa2,ipa3,ctlrdn,ctlrdr,ifunct,ndelay)
           ctlrdn=ctlrdr
        endif
      endif
      
      cdata(ipa2,ipa3)=ctlrdn

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' Ideal controller output     = ',ctlrd
         WRITE(ITU,*) ' Adjusted controller output',ctlrdn
         WRITE(ITU,*) ' Leaving subroutine CTLROP'
      END IF

      RETURN
      END

C ******************** CLACDL ********************

C CLACDL handles actuator delay.

      SUBROUTINE CLACDL(IPCMPD,IPCVRD,CDLAY1,CDLAY2,ILOOPN,LAG)
#include "building.h"
#include "plant.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/PSTCTR/NSINCP,NSNCPR 

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      COMMON/PCACDL/CDTSV1(MPCOM,MMISCD,MNONID,MPDLAY),
     &              CDTSV2(MPCOM,MMISCD,MNONID,MPDLAY),NSCSV(MNONID)
      COMMON/PTIME/PTIMEP,PTIMEF

C Common block variables are:
C CDTSV1(?,?,?,?): holds past CDATA(?,?) values.
C CDTSV2(?,?,?,?): is a memory for CDTSV1(?,?,?,?) 
C                  when iteration in progress.

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' ENTERING SUBROUTINE CLACDL'
      ENDIF

C If iteration in operation, recover CDTSV1(?,?,?,?) values.
      IF(NSINCP.LE.NSCSV(ILOOPN))THEN
         DO 30 ICMP=1,NPCOMP
            DO 20 IPCT=1,NCI(ICMP)
               DO 10 L=1,MPDLAY
                  CDTSV1(ICMP,IPCT,ILOOPN,L)=CDTSV2(ICMP,IPCT,ILOOPN,L)
  10           CONTINUE
 20         CONTINUE
30       CONTINUE
      ENDIF      

C Save CDATA values - required in case of iteration.
      DO 60 ICMP=1,NPCOMP
         DO 50 IPCT=1,NCI(ICMP)
            DO 40 L=1,MPDLAY
               CDTSV2(ICMP,IPCT,ILOOPN,L)=CDTSV1(ICMP,IPCT,ILOOPN,L)
  40        CONTINUE
 50      CONTINUE
60    CONTINUE

C CDATA(?,?) in calling routine will be set to CDLAY2.        
      CDLAY2=CDTSV1(IPCMPD,IPCVRD,ILOOPN,LAG)

C Update saved CDATA(?,?) values.
      DO 70 J=MPDLAY,1,-1
         CDTSV1(IPCMPD,IPCVRD,ILOOPN,J)=CDTSV1(IPCMPD,IPCVRD,ILOOPN,J-1)
70    CONTINUE
      CDTSV1(IPCMPD,IPCVRD,ILOOPN,1)=CDLAY1

C Remember current plant time-step in simulation.
      NSCSV(ILOOPN)=NSINCP

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0)THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' NSINC = ',nsinc,
     &                ' CDLAY1= ',CDLAY1,        
     &                ' CDLAY2= ',CDLAY2
         write(itu,*) ' Leaving subroutine CLACDL'
      ENDIF

      RETURN
      END

C ******************** CLSNDL ********************

C CLSNDL handles sensor delay.

      SUBROUTINE CLSNDL(LAG,SV1,SV2,ILOOP)
#include "plant.h"
#include "building.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/PSTCTR/NSINCP,NSNCPR 

      COMMON/PCSNDL/SVSAV1(MNONID,MPDLAY),SVSAV2(MNONID,MPDLAY),
     &              NSPSV(MCL)
      COMMON/PTIME/PTIMEP,PTIMEF

C Common block variables are:
C SVSAV1(?,?): holds past SVCTL values.
C SVSAV2(?,?): is a memory for SVSAV1(?,?) 
C              when iteration in progress.

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' ENTERING SUBROUTINE CLSNDL'
      ENDIF

C If iteration in operation, recover SVSAV1(?,?) values.
      IF(NSINCP.LE.NSPSV(iloop))THEN
        DO 10 L=1,MPDLAY
           SVSAV1(iloop,L)=SVSAV2(iloop,L)
  10    CONTINUE
      ENDIF      

C Save SVCTL values - required in case of iteration.
      DO 40 L=1,MPDLAY
         SVSAV2(iloop,L)=SVSAV1(iloop,L)
  40  CONTINUE

C SVCTL in calling routine will be set to SV2.        
      SV2=SVSAV1(iloop,LAG)

C Update saved SVCTL values.
      DO 70 J=MPDLAY,1,-1
         SVSAV1(iloop,J)=SVSAV1(iloop,J-1)
70    CONTINUE
      SVSAV1(iloop,1)=SV1

C Remember current plant time-step in simulation.
      NSPSV(iloop)=NSINCP

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0)THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' NSINC = ',nsinc,
     &                ' SV1= ',SV1,        
     &                ' SV2= ',SV2
         write(itu,*) ' Leaving subroutine CLSNDL'         
      ENDIF

      RETURN
      END

C ******************** PCL00 ********************

C PCL00 forces a plant control loop actuator to the OFF state
C by setting all plant component control variables to zero

      SUBROUTINE PCL00
#include "plant.h"
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTL/NCL,IPSN(MCL,4),IPAN(MCL,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PCLSOL/ICFP,IDTYPP,IPERP

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine PCL00'
      END IF

C Establish the sensed condition and the 'actuated' component and node
      CALL CLVAR(SVCTL,IPCMP,IPCVR)

C Set CDATA to zero for this component
      DO 10 IPCVR=1,NCI(IPCMP)
         CDATA(IPCMP,IPCVR)=0.0
   10 CONTINUE

C Non-ideal actuator characteristics
      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Leaving subroutine PCL00'
      RETURN
      END

C ******************** PCL01 ********************
C PCL01 is a controller offering the following control modes:
C   1/ 'POSITIONAL'  PID.
C       - Proportional
C       - Proportional + Integral
C       - Proportional + Derivative
C       - Proportional + Integral + Derivative
C
C   2/ 'INCREMENTAL' or 'VELOCITY' PID.
C       - Proportional + Integral
C       - Proportional + Integral + Derivative.
C                   
C These act to control heating flux on the basis of the
C sensed condition. It is suitable for plant controller:
C       type  0: senses temperature
C    or type  2: senses enthalpy
C    or type  4: senses 1th phase mass flow rate
C    or type  6: senses 2nd phase mass flow rate
C    or type  8: senses additional plant output
C    or type 10: senses relative humidity


      SUBROUTINE PCL01(iterp)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/PSTCTR/NSINCP,NSNCPR   

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      COMMON/PCTL/NCL,IPSN(MCL,4),IPAN(MCL,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/Pctime/TIMSEC
      COMMON/CTLACTN/RINTGRL(MCL,2),DERIV(MCL)
      COMMON/DIGPID/PID(MCL,7)
      COMMON/PSTAT/ISTATP
      logical closea
      
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine PCL01'
      END IF

C Sources:
C Recursive Positional Eqn:
C           Ogata, K., Discrete-Time Control Systems
C           New Jersey: Prentice-Hall, 1987.
C Recursive Velocity PI Eqn:
C           Levermore, G,J.,
C           Building Energy Management Systems,
C           London: Spon, 1992.
C Recursive Velocity PID [rectangular integration]
C           Clarke,D.W., 1984.
C          'PID algorithms and their computer implementation',
C          Transactions Inst. Measurement and Control,6,(6),305-316.
C Recursive Velocity PID [trapezoidal integration]  
C          Isermann, R., Digital Control Systems.
C          New York: Springer-Verlag, 1981.
   
C Fatal error tests

      ICLCTP=IPCTYP(ICFP,IDTYPP,IPERP)
      IF(ICLCTP.NE.0.AND.ICLCTP.NE.2.AND.ICLCTP.NE.4.AND.
     &   ICLCTP.NE.6.AND.ICLCTP.NE.8.AND.ICLCTP.NE.10.AND.
     &   ICLCTP.NE.40)then
         call edisp(iuout,
     &    ' PCL01: fatal error on controller type control law ')
         call epwait
         call epagend
         STOP
      endif

      IF(INT(PMISCD(ICFP,IDTYPP,IPERP,2)).GT.5.OR.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,2)).LT.-5.OR.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,2)).EQ.0)then
         call edisp(iuout,' PCL01: fatal error on PID algorithm type.')
         call epwait
         call epagend
         STOP
      endif

      IF(INT(PMISCD(ICFP,IDTYPP,IPERP,2)).LE.2.AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,2)).GE.-2.AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,1)).LT.7)then
         call edisp(iuout,
     &     ' PCL01: fatal error on number of control data items.')
         call epwait
         call epagend
         STOP
      endif

      IF((INT(PMISCD(ICFP,IDTYPP,IPERP,2)).EQ.3.OR.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,2)).EQ.-3).AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.6.AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.14)then
         call edisp(iuout,
     &     ' PCL01: fatal error on number of control data items.')
         call epwait
         call epagend
         STOP
      endif

      IF((INT(PMISCD(ICFP,IDTYPP,IPERP,2)).GE.4.OR.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,2)).LE.-4).AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.7.AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.15)then
         call edisp(iuout,
     &     ' PCL01: fatal error on number of control data items.')
         call epwait
         call epagend
         STOP
      endif

C Set control law parameters:
C max heating/cooling capacity QX (W),
C min heating/cooling capacity QM (W),
C heating/cooling set point SPC (C, kJ/kg, kg/s or ?),
C heating/cooling throttling range TRC.
C integral action flag, IT,
C integral time TI (s),
C derivative action flag, ID,
C derivative time TD (s),
C mode of operation and algorithm selection flag IMO;
C  1 for heating - Non-recursive Positional P, PI, PD, or PID.
C  2 for heating - Recursive Positional P, PI, PD, or PID.
C  3 for heating - Recursive Velocity PI  [rectangular integration].
C  4 for heating - Recursive Velocity PID [rectangular integration].
C  5 for heating - Recursive Velocity PID [trapezoidal integration].
C -1 for cooling - Non-recursive Positional P, PI, PD, or PID.
C -2 for cooling - Recursive Positional P, PI, PD, or PID.
C -3 for cooling - Recursive Velocity PI  [rectangular integration].
C -4 for cooling - Recursive Velocity PID [rectangular integration].
C -5 for cooling - Recursive Velocity PID [trapezoidal integration].
C Note that the set point is at the
C mid-point of the throttling range.
      IMO=INT(PMISCD(ICFP,IDTYPP,IPERP,2))      
      QX=PMISCD(ICFP,IDTYPP,IPERP,3)
      QM=PMISCD(ICFP,IDTYPP,IPERP,4)
      SPC=PMISCD(ICFP,IDTYPP,IPERP,5)
      TRC=PMISCD(ICFP,IDTYPP,IPERP,6)
      IF(IMO.EQ.1.OR.IMO.EQ.2.OR.IMO.EQ.-1.OR.IMO.EQ.-2)THEN        
        IT=INT(PMISCD(ICFP,IDTYPP,IPERP,7))
        IF(IT.EQ.1) THEN
          TI=PMISCD(ICFP,IDTYPP,IPERP,8)
          ID=INT(PMISCD(ICFP,IDTYPP,IPERP,9))
          J=9
          IF(ID.EQ.1)THEN
             TD=PMISCD(ICFP,IDTYPP,IPERP,10)
             J=10
          ENDIF
        ELSE
          ID=INT(PMISCD(ICFP,IDTYPP,IPERP,8))
          J=8
          IF(ID.EQ.1)THEN
             TD=PMISCD(ICFP,IDTYPP,IPERP,9)
             J=9
          ENDIF
        ENDIF
      ELSEIF(IMO.EQ.3.OR.IMO.EQ.-3)THEN
        TI=PMISCD(ICFP,IDTYPP,IPERP,7)
        J=7
      ELSEIF(IMO.GE.4.OR.IMO.LE.-4)THEN
        TI=PMISCD(ICFP,IDTYPP,IPERP,7)
        TD=PMISCD(ICFP,IDTYPP,IPERP,8)
        J=8
      ENDIF

C Set point may be dynamically set by PSYSPT subroutine.
      IF(INT(SPC).eq.-1001)then
        IRE=INT(PMISCD(ICFP,IDTYPP,IPERP,J+1))
        MDFLAG=INT(PMISCD(ICFP,IDTYPP,IPERP,J+2))
        IMX=INT(PMISCD(ICFP,IDTYPP,IPERP,J+3))
        ISUPDC=INT(PMISCD(ICFP,IDTYPP,IPERP,J+4))
        IRETDC=INT(PMISCD(ICFP,IDTYPP,IPERP,J+5))
        ZNTPSP=PMISCD(ICFP,IDTYPP,IPERP,J+6)
        ZNRHSP=PMISCD(ICFP,IDTYPP,IPERP,J+7)
        call PSYSPT(ire,mdflag,imx,isupdc,iretdc,zntpsp,znrhsp,stpt)
        SPC=stpt
      ENDIF

C If first call for this control period, 
C reset integral action variables.
      DELTIM=TIMSEC/3600.
      CPRSTR=TPCPS(ICFP,IDTYPP,IPERP)
      IF((PTIMEF-CPRSTR).LE.DELTIM)THEN
        RINTGRL(ICFP,1)=0.
        RINTGRL(ICFP,2)=0.
      ENDIF 

C At every call, reset integral and derivative actions to 
C zero for non-recursive algorithm.
      RINT=0.0
      RDRV=0.0

C Establish sensed variable magnitude and actuated node location
      CALL CLVAR(SVCTL,IPCMP,IPCVR)

C Determine signal upper limit SGU, lower limit SGL.
C Test for 'realistic' values
      SGU=SPC+(TRC/2.0)
      SGL=SPC-(TRC/2.0)

      IF(SGU.LT.SGL)then
         call edisp(iuout,' PCL01: fatal error on signal set point(s).')
         call epwait
         call epagend
         STOP
      endif

C Determine heat flux at set point assuming linear relationship.
            QSP=(QX+QM)/2.0

C Common block variables for pid(icfp,?):
C  pid(icfp,1)= error at present time-step,
C  pid(icfp,2)= error at previous time-step,
C  pid(icfp,3)= error at 2nd last time-step,
C  pid(icfp,4)= time-integral of the error at present time-step,
C  pid(icfp,5)= time-integral of the error at last time-step,
C  pid(icfp,6)= control signal at present time-step,
C  pid(icfp,7)= control signal at previous time-step.
      
      IF(NSINCP.EQ.(NSNCPR+1))THEN
C since next plant time-step, update common variables,     
       IF(NSINCP.NE.2)PID(ICFP,3)=PID(ICFP,2)
       IF(NSINCP.NE.1)PID(ICFP,2)=PID(ICFP,1)
       PID(ICFP,5)=PID(ICFP,4)   
       PID(ICFP,7)=PID(ICFP,6)
      ENDIF
      PID(ICFP,1)=(SVCTL-SPC)
      PID(ICFP,4)=PID(ICFP,5)+(0.5*(PID(ICFP,2)+PID(ICFP,1)))

C Determine error.
      ER=(SVCTL-SPC)

C See if integral action is required.
      IF(IT.EQ.1.AND.ISTATP.EQ.2)THEN

C If iterating, reject previous error value.
               if(iterp.gt.1) 
     &             RINTGRL(ICFP,1)=RINTGRL(ICFP,1)-RINTGRL(ICFP,2)
               RINTGRL(ICFP,1)=RINTGRL(ICFP,1)+ER

C Remember error value for next iteration.
               RINTGRL(ICFP,2)=ER
      ENDIF

C Establish integral error term.
      IF((IMO.EQ.1.OR.IMO.EQ.-1).AND.IT.EQ.1)THEN
          RINT=RINTGRL(ICFP,1)*TIMSEC/TI
      ENDIF
      IF((IMO.EQ.2.OR.IMO.EQ.-2).AND.IT.EQ.1)THEN
          RINT=(TIMSEC/TI)*(PID(ICFP,5)+
     &                      0.5*(PID(ICFP,2)+PID(ICFP,1)))
      ENDIF
 
C Determine whether controlled heating or controlled cooling,
      IF(IMO.GT.0)THEN
C controlled heating.

C Determine the gain,
       RKP=(QM-QX)/(SGU-SGL)

C Determine heating magnitude,
       call eclose((SGU-SGL),0.00,0.0001,closea)
       IF(SVCTL.GE.SGU) THEN
C Control signal
        PID(ICFP,6)=QM
C Heating flux
        QH=QM
       ELSE IF(SVCTL.GT.SGL.AND.(.NOT.closea)) THEN
              
C Control signal depends on type of algorithm selected by user:
        IF(IMO.EQ.1) THEN

C See if derivative action is required.
            IF(ID.EQ.1) RDRV=(SVCTL-DERIV(ICFP))*TD/TIMSEC

C Now evaluate heat flux.
            QH=RKP*(ER+RINT+RDRV)+QSP

        ELSE IF(IMO.EQ.2)THEN
C See if derivative action is required.
           IF(ID.EQ.1) RDRV=(TD/TIMSEC)*(PID(ICFP,1)-PID(ICFP,2))
C Determine the control signal
            PID(ICFP,6)=RKP*(PID(ICFP,1)+RINT+RDRV)
C Now evaluate heat flux.
            QH=PID(ICFP,6)+QSP          

        ELSE IF(IMO.EQ.3)THEN
C Determine the control signal    
           PID(ICFP,6)=PID(ICFP,7)+RKP*(PID(ICFP,1)-PID(ICFP,2)
     &                            +((TIMSEC/TI)*PID(ICFP,2)))
C Now evaluate heat flux.
           QH=PID(ICFP,6)+QSP
     
        ELSE IF(IMO.EQ.4)THEN
            A=RKP*(1.+(TIMSEC/TI)+(TD/TIMSEC))
            B=-RKP*(1.+(2*TD/TIMSEC))
            C=RKP*TD/TIMSEC
C Determine the control signal
            PID(ICFP,6)=PID(ICFP,7)+A*PID(ICFP,1)+
     &                      B*PID(ICFP,2)+C*PID(ICFP,3)
C Now evaluate heat flux.
            QH=PID(ICFP,6)+QSP
    
        ELSE 
            A=RKP*(1.+(0.5*TIMSEC/TI)+(TD/TIMSEC))
            B=-RKP*(1.+(2*TD/TIMSEC)-(0.5*TIMSEC/TI))
            C=RKP*TD/TIMSEC
C Determine the control signal
            PID(ICFP,6)=PID(ICFP,7)+A*PID(ICFP,1)+
     &                      B*PID(ICFP,2)+C*PID(ICFP,3)
C Now evaluate heat flux.
            QH=PID(ICFP,6)+QSP
        ENDIF

C Limit flux.
           IF(QH.LT.QM) QH=QM
           IF(QH.GT.QX) QH=QX 

       ELSE
C Control signal
           PID(ICFP,6)=QX
C Heating flux
           QH=QX
       ENDIF

C Assign CDATA
          CDATA(IPCMP,IPCVR)=QH
 
      ELSEIF(IMO.LT.0) then
C controlled cooling.

C Determine the gain
       RKP=(QX-QM)/(SGU-SGL)

       call eclose((SGU-SGL),0.00,0.0001,closea)
       IF(SVCTL.GE.SGU) THEN
C Control signal
        PID(ICFP,6)=QX
C Cooling flux
        QC=QX
       ELSE IF(SVCTL.GT.SGL.AND.(.NOT.closea)) THEN

C Control signal depends on type of algorithm selected by user:

        IF(IMO.EQ.-1)THEN

C See if derivative action is required.
         IF(ID.EQ.1) RDRV=(SVCTL-DERIV(ICFP))*TD/TIMSEC

C Now evaluate cooling flux.
        QC=RKP*(ER+RINT+RDRV)+QSP

        ELSEIF(IMO.EQ.-2)THEN    
C See if derivative action is required.
         IF(ID.EQ.1)RDRV=(TD/TIMSEC)*(PID(ICFP,1)-PID(ICFP,2))
C Determine the control signal
         PID(ICFP,6)=RKP*(PID(ICFP,1)+RINT+RDRV)
C Now evaluate cooling flux.
         QC=PID(ICFP,6)+QSP
           
        ELSE IF(IMO.EQ.-3)THEN
C Determine the control signal
         PID(ICFP,6)=PID(ICFP,7)+RKP*(PID(ICFP,1)-PID(ICFP,2)
     &                           +((TIMSEC/TI)*PID(ICFP,2)))
C Now evaluate cooling flux.
         QC=PID(ICFP,6)+QSP

        ELSE IF(IMO.EQ.-4)THEN
         A=RKP*(1.+(TIMSEC/TI)+(TD/TIMSEC))
         B=-RKP*(1.+(2*TD/TIMSEC))
         C=RKP*TD/TIMSEC
C Determine the control signal
         PID(ICFP,6)=PID(ICFP,7)+A*PID(ICFP,1)+
     &                       B*PID(ICFP,2)+C*PID(ICFP,3)
C Now evaluate cooling flux.
         QC=PID(ICFP,6)+QSP
          
        ELSE
          A=RKP*(1.+(0.5*TIMSEC/TI)+(TD/TIMSEC))
          B=-RKP*(1.+(2*TD/TIMSEC)-(0.5*TIMSEC/TI))
          C=RKP*TD/TIMSEC
C Determine the control signal
          PID(ICFP,6)=PID(ICFP,7)+A*PID(ICFP,1)+
     &                       B*PID(ICFP,2)+C*PID(ICFP,3)
C Now evaluate cooling flux.
          QC=PID(ICFP,6)+QSP
          
        ENDIF
        IF(QC.LT.QM) QC=QM
        IF(QC.GT.QX) QC=QX 
       ELSE
C Control signal
        PID(ICFP,6)=QM
C Cooling flux
        QC=QM
                     
       ENDIF

C Assign CDATA
       CDATA(IPCMP,IPCVR)=-QC
      ENDIF

C Non-ideal actuator characteristics
      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

C Remember sensed value.
      DERIV(ICFP)=SVCTL

C Remember plant time-step
      NSNCPR=NSINCP
      
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         if(imo.eq.1) WRITE(ITU,*) ' QH = ',QH,
     &                ' CDATA = ',CDATA(IPCMP,IPCVR)
         if(imo.eq.-1) WRITE(ITU,*) ' QC = ',QC,
     &                ' CDATA = ',CDATA(IPCMP,IPCVR)
         WRITE(ITU,*) ' Leaving subroutine PCL01'
      END IF

      RETURN
      END

C ******************** PCL02 ********************

C PCL02 is a controller offering the following control modes:
C   1/ 'POSITIONAL'  PID.
C       - Proportional
C       - Proportional + Integral
C       - Proportional + Derivative
C       - Proportional + Integral + Derivative
C
C   3/ 'INCREMENTAL' (or 'VELOCITY') PID.
C       - Proportional + Integral
C       - Proportional + Integral + Derivative.

C These act to control flowrate on the basis of the
C sensed condition. It is suitable for plant controller:
C                    type  1: senses temperature
C                 or type  3: senses enthalpy
C                 or type  5: senses 1th phase mass flow rate
C                 or type  7: senses 2nd phase mass flow rate
C                 or type  9: senses additional plant output
C                 or type 11: senses relative humidity.

      SUBROUTINE PCL02(iterp)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/PSTCTR/NSINCP,NSNCPR   

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      COMMON/PCTL/NCL,IPSN(MCL,4),IPAN(MCL,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/Pctime/TIMSEC
      COMMON/DIGPID/PID(MCL,7)
      COMMON/CTLACTN/RINTGRL(MCL,2),DERIV(MCL)
      COMMON/PSTAT/ISTATP
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine PCL02'
      END IF 

C Sources:
C Recursive Positional Eqn:
C           Ogata, K., Discrete-Time Control Systems
C           New Jersey: Prentice-Hall, 1987.
C Recursive Velocity PI Eqn:
C           Levermore, G,J.,
C           Building Energy Management Systems,
C           London: Spon, 1992.
C Recursive Velocity PID [rectangular integration]
C           Clarke,D.W., 1984.
C          'PID algorithms and their computer implementation',
C          Transactions Inst. Measurement and Control,6,(6),305-316.
C Recursive Velocity PID [trapezoidal integration]  
C          Isermann, R., Digital Control Systems.
C          New York: Springer-Verlag, 1981.

C Fatal error tests

      ICLCTP=IPCTYP(ICFP,IDTYPP,IPERP)
      IF(ICLCTP.NE.1.AND.ICLCTP.NE.3.AND.ICLCTP.NE.5.AND.
     &   ICLCTP.NE.7.AND.ICLCTP.NE.9.AND.ICLCTP.NE.11.AND.
     &   ICLCTP.NE.40)then
        call edisp(iuout,
     &     ' PCL02: fatal error on controller type control law ')
        call epwait
        call epagend
        STOP
      endif

      IF(INT(PMISCD(ICFP,IDTYPP,IPERP,2)).GT.5.OR.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,2)).LT.-5.OR.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,2)).EQ.0)then
        call edisp(iuout,' PCL02: fatal error on PID algorithm type ')
        call epwait
        call epagend
        STOP
      endif

      IF(INT(PMISCD(ICFP,IDTYPP,IPERP,2)).LE.2.AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,2)).GE.-2.AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,1)).LT.7)then
        call edisp(iuout,
     &    ' PCL02: fatal error on number of control data items ')
        call epwait
        call epagend
        STOP
      endif

      IF((INT(PMISCD(ICFP,IDTYPP,IPERP,2)).EQ.3.OR.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,2)).EQ.-3).AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.6.AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.14)then
         call edisp(iuout,
     &     ' PCL02: fatal error on number of control data items.')
         call epwait
         call epagend
         STOP
      endif

      IF((INT(PMISCD(ICFP,IDTYPP,IPERP,2)).GE.4.OR.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,2)).LE.-4).AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.7.AND.
     &   INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.15)then
         call edisp(iuout,
     &     ' PCL02: fatal error on number of control data items.')
         call epwait
         call epagend
         STOP
      endif
       
C Set control law parameters: 
C Max flow rate FRX (kg/s or m3/s),
C Min flow rate FRM (kg/s or m3/s),
C Control set point SPC (C, kJ/kg, kg/s, or ?), 
C Control throttling range TRC(=mid-point of the throttling range.)
C Integral action flag, IT,
C Integral action  time, TI (s),
C Derivative action flag ID,
C Derivative action time, TD (s)
C Mode of operation, IMO;
C  1 For FRX < SPC; Non-recursive Positional P, PI, PD, or PID.
C  2 For FRX < SPC; Recursive Positional P, PI, PD, or PID.
C  3 For FRX < SPC; Recursive Velocity PI  [rectangular integration].
C  4 For FRX < SPC; Recursive Velocity PID [rectangular integration].
C  5 For FRX < SPC; Recursive Velocity PID [trapezoidal integration].
C -1 For FRX > SPC; Non-recursive Positional P, PI, PD, or PID.
C -2 For FRX > SPC; Recursive Positional P, PI, PD, or PID.
C -3 For FRX > SPC; Recursive Velocity PI  [rectangular integration].
C -4 For FRX > SPC; Recursive Velocity PID [rectangular integration].
C -5 For FRX > SPC; Recursive Velocity PID [trapezoidal integration].
C Note that the set point is at the
C mid-point of the throttling range.
      IMO=INT(PMISCD(ICFP,IDTYPP,IPERP,2))
      FRX=PMISCD(ICFP,IDTYPP,IPERP,3)
      FRM=PMISCD(ICFP,IDTYPP,IPERP,4)
      SPC=PMISCD(ICFP,IDTYPP,IPERP,5)
      TRC=PMISCD(ICFP,IDTYPP,IPERP,6)
      IF(IMO.EQ.1.OR.IMO.EQ.2.OR.IMO.EQ.-1.OR.IMO.EQ.-2)THEN        
        IT=INT(PMISCD(ICFP,IDTYPP,IPERP,7))
        IF(IT.EQ.1) THEN
          TI=PMISCD(ICFP,IDTYPP,IPERP,8)
          ID=INT(PMISCD(ICFP,IDTYPP,IPERP,9))
          J=9
          IF(ID.EQ.1)THEN
             TD=PMISCD(ICFP,IDTYPP,IPERP,10)
             J=10
          ENDIF
        ELSE
          ID=INT(PMISCD(ICFP,IDTYPP,IPERP,8))
          J=8
          IF(ID.EQ.1)THEN
             TD=PMISCD(ICFP,IDTYPP,IPERP,9)
             J=9
          ENDIF
        ENDIF
      ELSEIF(IMO.EQ.3.OR.IMO.EQ.-3)THEN
        TI=PMISCD(ICFP,IDTYPP,IPERP,7)
        J=7
      ELSEIF(IMO.GE.4.OR.IMO.LE.-4)THEN
        TI=PMISCD(ICFP,IDTYPP,IPERP,7)
        TD=PMISCD(ICFP,IDTYPP,IPERP,8)
        J=8
      ENDIF

C Set point may be dynamically set by PSYSPT subroutine.
      IF(INT(SPC).eq.-1001)then
        IRE=INT(PMISCD(ICFP,IDTYPP,IPERP,J+1))
        MDFLAG=INT(PMISCD(ICFP,IDTYPP,IPERP,J+2))
        IMX=INT(PMISCD(ICFP,IDTYPP,IPERP,J+3))
        ISUPDC=INT(PMISCD(ICFP,IDTYPP,IPERP,J+4))
        IRETDC=INT(PMISCD(ICFP,IDTYPP,IPERP,J+5))
        ZNTPSP=PMISCD(ICFP,IDTYPP,IPERP,J+6)
        ZNRHSP=PMISCD(ICFP,IDTYPP,IPERP,J+7)
        call PSYSPT(ire,mdflag,imx,isupdc,iretdc,zntpsp,znrhsp,stpt)
        SPC=stpt
      ENDIF

C If first call for this control period, 
C reset integral action variables.
      DELTIM=TIMSEC/3600.
      CPRSTR=TPCPS(ICFP,IDTYPP,IPERP)
      IF((PTIMEF-CPRSTR).LE.DELTIM)THEN
        RINTGRL(ICFP,1)=0.
        RINTGRL(ICFP,2)=0.
      ENDIF
         
C Set integral and derivative actions to zero for
C non-recursive algorithm.
      rint=0.0
      rdrv=0.0
 
C Determine signal upper limit SGU, lower limit SGL.
C Test for 'realistic' values
      SGU=SPC+(TRC/2.0)
      SGL=SPC-(TRC/2.0)
      IF(SGU.LT.SGL)then
        call edisp(iuout,
     &    ' PCL02: fatal error on signal set point(s) ')
        call epwait
        call epagend
        STOP
      endif

C Determine mass flow rate at set point assuming linear
C relationship.
      FRSP=(FRX+FRM)/2.0
      
C Establish sensed variable magnitude and actuated node location
      CALL CLVAR(SVCTL,IPCMP,IPCVR)

C Common block variables for pid(icfp,?):
C  pid(icfp,1)= error at present time-step,
C  pid(icfp,2)= error at previous time-step,
C  pid(icfp,3)= error at 2nd last time-step,
C  pid(icfp,4)= time-integral of the error at present time-step,
C  pid(icfp,5)= time-integral of the error at last time-step,
C  pid(icfp,6)= control signal at present time-step,
C  pid(icfp,7)= control signal at previous time-step.

      IF(NSINCP.EQ.(NSNCPR+1))THEN
C since next plant time-step, update common variables,     
       IF(NSINCP.NE.2)PID(ICFP,3)=PID(ICFP,2)
       IF(NSINCP.NE.1)PID(ICFP,2)=PID(ICFP,1)
       PID(ICFP,5)=PID(ICFP,4)   
       PID(ICFP,7)=PID(ICFP,6)
      ENDIF
      PID(ICFP,1)=(SVCTL-SPC)
      PID(ICFP,4)=PID(ICFP,5)+(0.5*(PID(ICFP,2)+PID(ICFP,1)))

C Determine error.
      ER=(SVCTL-SPC)

C See if integral action is required.
      IF(IT.EQ.1.AND.ISTATP.EQ.2)THEN

C If iterating, reject previous error value.
          if(iterp.gt.1)
     &       RINTGRL(ICFP,1)=RINTGRL(ICFP,1)-RINTGRL(ICFP,2)

          RINTGRL(ICFP,1)=RINTGRL(ICFP,1)+ER

C Remember error value for next iteration.
          RINTGRL(ICFP,2)=ER
      ENDIF

C Establish integral error term.
      IF((IMO.EQ.1.OR.IMO.EQ.-1).AND.IT.EQ.1)THEN
          RINT=RINTGRL(ICFP,1)*TIMSEC/TI
      ENDIF
      IF((IMO.EQ.2.OR.IMO.EQ.-2).AND.IT.EQ.1)THEN
            RINT=(TIMSEC/TI)*(PID(ICFP,5)+
     &                        0.5*(PID(ICFP,2)+PID(ICFP,1)))
      ENDIF   

C Determine whether max flow rate if value of 
C sensed variable below or above setpoint.
      IF(IMO.GT.0)THEN
C max flow rate if value of sensed variable below setpoint.
            
C Determine the gain 
            RKP=(FRM-FRX)/(SGU-SGL)
            
C Determine control signal and thus flowrate
            call eclose((SGU-SGL),0.00,0.0001,closea)
            IF(SVCTL.GE.SGU) THEN
C Control signal
            PID(ICFP,6)=FRM
C Flowrate
            FR=FRM
            
            ELSE IF(SVCTL.GT.SGL.AND.(.NOT.closea)) THEN

C Control signal depends on type of algorithm selected by user:
            IF(IMO.EQ.1)THEN

C See if derivative action is required.
            IF(ID.EQ.1) RDRV=(SVCTL-DERIV(ICFP))*TD/TIMSEC

C Now evaluate flow rate.
            FR=RKP*(ER+RINT+RDRV)+FRSP
            
            ELSE IF(IMO.EQ.2)THEN     
C See if derivative action is required.
            IF(ID.EQ.1) RDRV=(TD/TIMSEC)*(PID(ICFP,1)-PID(ICFP,2))
C Determine the control signal
            PID(ICFP,6)=RKP*(PID(ICFP,1)+RINT+RDRV)
C Thus flowrate
            FR=PID(ICFP,6)+FRSP
             
            ELSE IF(IMO.EQ.3)THEN
C Determine the control signal
            PID(ICFP,6)=PID(ICFP,7)+RKP*(PID(ICFP,1)-PID(ICFP,2)+
     &                      ((TIMSEC/TI)*PID(ICFP,2)))
C Thus flowrate
            FR=PID(ICFP,6)+FRSP

            ELSE IF(IMO.EQ.4)THEN
            A=RKP*(1.+(TIMSEC/TI)+(TD/TIMSEC))
            B=-RKP*(1.+(2*TD/TIMSEC))
            C=RKP*TD/TIMSEC
C Determine the control signal
            PID(ICFP,6)=PID(ICFP,7)+A*PID(ICFP,1)+
     &                       B*PID(ICFP,2)+C*PID(ICFP,3)
C Thus flowrate
            FR=PID(ICFP,6)+FRSP
             
            ELSE 
            A=RKP*(1.+(0.5*TIMSEC/TI)+(TD/TIMSEC))
            B=-RKP*(1.+(2*TD/TIMSEC)-(0.5*TIMSEC/TI))
            C=RKP*TD/TIMSEC
C Determine the control signal
            PID(ICFP,6)=PID(ICFP,7)+A*PID(ICFP,1)+
     &                       B*PID(ICFP,2)+C*PID(ICFP,3)
C Thus flowrate
            FR=PID(ICFP,6)+FRSP
             
            ENDIF

            IF(FR.LT.FRM) FR=FRM
            IF(FR.GT.FRX) FR=FRX

            ELSE
C Control signal
            PID(ICFP,6)=FRX
C Flowrate
            FR=FRX

         ENDIF
         
         ELSE IF(IMO.LT.0)THEN
C max flow rate if value of sensed variable above setpoint.

C Determine the gain
           RKP=(FRX-FRM)/(SGU-SGL)
C Determine control signal and thus flow
           call eclose((SGU-SGL),0.00,0.0001,closea)
           IF(SVCTL.GE.SGU) THEN
C Control signal
           PID(ICFP,6)=FRX
C Flowrate
           FR=FRX
            
           ELSE IF(SVCTL.GT.SGL.AND.(.NOT.closea)) THEN

C Control signal depends on type of algorithm selected by user:
           
           IF(IMO.EQ.-1) THEN       

C See if derivative action is required.
           IF(ID.EQ.1) RDRV=(SVCTL-DERIV(ICFP))*TD/TIMSEC
C Now evaluate flow rate.
           FR=RKP*(ER+RINT+RDRV)+FRSP
            
           ELSE IF(IMO.EQ.-2)THEN
C See if derivative action is required.
           IF(ID.EQ.1) RDRV=(TD/TIMSEC)*(PID(ICFP,1)-PID(ICFP,2))
C Determine the control signal
           PID(ICFP,6)=RKP*(PID(ICFP,1)+RINT+RDRV)
C Thus flowrate
           FR=PID(ICFP,6)+FRSP
             
           ELSE IF(IMO.EQ.-3)THEN
C Determine the control signal
           PID(ICFP,6)=PID(ICFP,7)+RKP*(PID(ICFP,1)-PID(ICFP,2)+
     &                      ((TIMSEC/TI)*PID(ICFP,2)))
C Thus flowrate
           FR=PID(ICFP,6)+FRSP

           ELSE IF(IMO.EQ.-4)THEN
           A=RKP*(1.+(TIMSEC/TI)+(TD/TIMSEC))
           B=-RKP*(1.+(2*TD/TIMSEC))
           C=RKP*TD/TIMSEC
C Determine the control signal
           PID(ICFP,6)=PID(ICFP,7)+A*PID(ICFP,1)+
     &                       B*PID(ICFP,2)+C*PID(ICFP,3)
C Thus flowrate
           FR=PID(ICFP,6)+FRSP
             
           ELSE
           A=RKP*(1.+(0.5*TIMSEC/TI)+(TD/TIMSEC))
           B=-RKP*(1.+(2*TD/TIMSEC)-(0.5*TIMSEC/TI))
           C=RKP*TD/TIMSEC
C Determine the control signal
           PID(ICFP,6)=PID(ICFP,7)+A*PID(ICFP,1)+
     &                       B*PID(ICFP,2)+C*PID(ICFP,3)
C Thus flowrate
           FR=PID(ICFP,6)+FRSP
             
           ENDIF

           IF(FR.LT.FRM) FR=FRM
           IF(FR.GT.FRX) FR=FRX

          ELSE

C Control signal
          PID(ICFP,6)=FRM

C Flowrate
          FR=FRM
          ENDIF
        ENDIF

C Assign CDATA
        CDATA(IPCMP,IPCVR)=FR

C Non-ideal actuator characteristics
      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

C Remember plant time-step
        NSNCPR=NSINCP

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' FR = ',FR,         
     &                ' CDATA = ',CDATA(IPCMP,IPCVR)
         WRITE(ITU,*) ' Leaving subroutine PCL02'
      END IF
      RETURN
      END

C ******************** PCL03 ********************

C PCL03 is a basic proportional controller which acts to control
C some numerical value on the basis of the sensed condition.
C CDATA will be expressed in ?'s depending on the
C definition of CDATA in the coefficient generator.
C This controller is suitable
C for plant controller type 12: senses temperature
C                   or type 13: senses enthalpy
C                   or type 14: senses 1th phase mass flow rate
C                   or type 15: senses 2nd phase mass flow rate
C                   or type 16: senses additional plant output
C                   or type 17: senses relative humidity

      SUBROUTINE PCL03
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      COMMON/HPCL03/HOUT(MPCOM),HSASC(MPCOM)
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine PCL03'
      END IF

C Fatal error tests
      IF(INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.5)then
        call edisp(iuout,
     &    ' PCL03: fatal error on number of control data items.')
        call epwait
        call epagend
        STOP
      endif
      ICLCTP=IPCTYP(ICFP,IDTYPP,IPERP)
      IF(ICLCTP.NE.12.AND.ICLCTP.NE.13.AND.ICLCTP.NE.14.AND.
     &   ICLCTP.NE.15.AND.ICLCTP.NE.16.AND.ICLCTP.NE.17.AND.
     &   ICLCTP.NE.40)then
         call edisp(iuout,
     &     ' PCL03: fatal error on controller type control law.')
         call epwait
         call epagend
         STOP
      endif

C Establish sensed variable magnitude and actuated node location
      CALL CLVAR(SVCTL,IPCMP,IPCVR)

C Set control law parameters: output OUTU when S>=Su, output OUTL
C when S<=Sl, upper set point for control variable SU, lower set
C point for control variable SL, and DS to overcome the controller's
C hysteresis
C HOUT is the previous time step output value, and HSASC is the
C "intersection" of HOUT and ascending path of H(S)
      OUTU=PMISCD(ICFP,IDTYPP,IPERP,2)
      OUTL=PMISCD(ICFP,IDTYPP,IPERP,3)
      SU=PMISCD(ICFP,IDTYPP,IPERP,4)
      SL=PMISCD(ICFP,IDTYPP,IPERP,5)
      DS=PMISCD(ICFP,IDTYPP,IPERP,6)

C Establish output signal OUT; incorporate hysteresis effect
      IF(SVCTL.GE.(HSASC(IPCMP)-DS).AND.SVCTL.LE.HSASC(IPCMP)) THEN
         OUT=HOUT(IPCMP)
      ELSE IF(SVCTL.LT.(HSASC(IPCMP)-DS)) THEN
         IF(SVCTL.LE.(SL-DS)) THEN
            OUT=OUTL
         ELSE IF(SVCTL.GE.(SU-DS)) THEN
            OUT=OUTU
         ELSE
            OUT=OUTL+(OUTU-OUTL)*(SVCTL-(SL-DS))/(SU-SL)
         END IF
C     ELSE IF(SVCTL.GT.HSASC(IPCMP)) THEN
      ELSE
         IF(SVCTL.LE.SL) THEN
            OUT=OUTL
         ELSE IF(SVCTL.GE.SU) THEN
            OUT=OUTU
         ELSE
            OUT=OUTL+(OUTU-OUTL)*(SVCTL-SL)/(SU-SL)
         END IF
      END IF
C Refresh history for H/H100 and in case of change recalculate HSASC
      call eclose(OUT,HOUT(IPCMP),0.00001,closea)
      IF(.NOT.closea) THEN
         HOUT(IPCMP)=OUT
         IF(OUT.GE.OUTU) THEN
            HSASC(IPCMP)=SU
         ELSE IF(OUT.LE.OUTL) THEN
            HSASC(IPCMP)=SL
         ELSE
            HSASC(IPCMP)=SL+(SU-SL)*(OUT-OUTL)/(OUTU-OUTL)
         END IF
      END IF

C Assign CDATA
      CDATA(IPCMP,IPCVR)=OUT

C Non-ideal actuator characteristics
      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' OUT = ',OUT
         WRITE(ITU,*) ' Leaving subroutine PCL03'
      END IF

      RETURN
      END

C ******************** PCL04 ********************

C PCL04 is an optimum start controller generating either OFF (=0)
C or ON signal (either 1 or some heating flux (W)) when time is
C past the optimum start time necessary to reach desired temperature
C level at some specified time.
C This controller is suitable for plant controllers acting on flux
C or on a variable expecting some numerical value

      SUBROUTINE PCL04
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine PCL04'
      END IF

C Fatal error tests
      IF(INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.6)then
        call edisp(iuout,
     &    ' PCL04: fatal error on number of control data items')
        call epwait
        call epagend
        STOP
       endif
      ICLCTP=IPCTYP(ICFP,IDTYPP,IPERP)
      IF(ICLCTP.NE. 0.AND.ICLCTP.NE. 2.AND.ICLCTP.NE. 4.AND.
     &   ICLCTP.NE. 6.AND.ICLCTP.NE. 8.AND.
     &   ICLCTP.NE.10.AND.ICLCTP.NE.12.AND.ICLCTP.NE.13.AND.
     &   ICLCTP.NE.14.AND.ICLCTP.NE.15.AND.
     &   ICLCTP.NE.16.AND.ICLCTP.NE.17.AND.ICLCTP.NE.40)then
         call edisp(iuout,
     &     ' PCL04: fatal error on controller type control law.')
         call epwait
         call epagend
         STOP
      endif

C Establish sensed variable magnitude and actuated node location
      CALL CLVAR(SVCTL,IPCMP,IPCVR)

C Set control law parameters: output OUTP when t>=t,start ,
C desired "time of arrival" DTOA, desired temperature level DTMP,
C and the coefficients A0 and A1
      OUTP=PMISCD(ICFP,IDTYPP,IPERP,2)
      DTOA=PMISCD(ICFP,IDTYPP,IPERP,3)
      DTMP=PMISCD(ICFP,IDTYPP,IPERP,4)
      A0=PMISCD(ICFP,IDTYPP,IPERP,5)
      A1=PMISCD(ICFP,IDTYPP,IPERP,6)
      A2=PMISCD(ICFP,IDTYPP,IPERP,7)

C Compute time DT necessary to reach desired temperature level
C based on the currently sensed temperature;
C source:   Levermore, G,J.,
C           Building Energy Management Systems,
C           London: Spon, 1992.

      DT=EXP(A0+A1*(SVCTL-DTMP)+A2*(TFP))

C Establish start time TSTART; find out if current time is past TSTART
      TSTART=DTOA-DT/3600.
      IF(TSTART.LT.0.) TSTART=24.0+TSTART
      IF(PTIMEF.GE.TSTART) THEN
         OUT=OUTP
      ELSE
         OUT=0.
      END IF

C Assign CDATA
      CDATA(IPCMP,IPCVR)=OUT

C Non-ideal actuator characteristics
      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' DT  = ',DT,'   TSTART = ',TSTART
         WRITE(ITU,*) ' OUT = ',OUT
         WRITE(ITU,*) ' Leaving subroutine PCL04'
      END IF

      RETURN
      END

C ******************** PCL05 ********************

C 'pcl05' is a basic proportional controller which acts to control
C AC damper position on the basis of the sensed condition. The
c damper position=0 when fully closed and 1 when fully open.
C CDATA will hold the fraction of mass flow allowed through the
C damper.  This controller is suitable
C for plant controller type 18: senses dry bulb temperature
C                   or type 19: senses enthalpy
C                   or type 20: senses 1th phase mass flow rate
C                   or type 21: senses 2nd phase mass flow rate
C                   or type 22: senses additional plant output
C                   or type 23: senses relative humidity
C                   or type 40: senses numerical value.

      subroutine pcl05
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/ptime/ptimep,ptimef

      common/pclsol/icfp,idtypp,iperp
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),npcdt(mcl),
     &            ipcdv(mcl,mpcdt,2),npcdp(mcl,mpcdt),
     &            tpcps(mcl,mpcdt,mpcdp),ipctyp(mcl,mpcdt,mpcdp),
     &            ipclaw(mcl,mpcdt,mpcdp),pmiscd(mcl,mpcdt,mpcdp,misc)
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)

c tmdr(?,1) holds sum of mass diversion ratios
c tmdr(?,2) indicates that (?,1) must be calculated
c           only once.
      dimension tmdr(mcl,2)
      logical closea

c Save contents of array 'tmdr'
      save tmdr

c 'ipass' is a flag used to indicate if first pass was made to this
c routine. << Kelly recommends removal >>
C      ipass = 0


c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(40).ne.0) then
         call dayclk(idyp,ptimef,itu)
         write(itu,*) ' Entering subroutine pcl03 '
      end if

c Fatal error tests
      if(int(pmiscd(icfp,idtypp,iperp,1)).ne.6)then
        call edisp(iuout,
     &    ' pcl05: fatal error on number of control data items.')
        call epwait
        call epagend
        STOP
      endif
      iclctp=ipctyp(icfp,idtypp,iperp)
      if(iclctp.ne.18.and.iclctp.ne.19.and.iclctp.ne.20.and.
     &  iclctp.ne.21.and.iclctp.ne.22.and.iclctp.ne.23.and.
     &  iclctp.ne.40)then
         call edisp(iuout,
     &     ' pcl05: fatal error on controller type control law.')
         call epwait
         call epagend
         STOP
      endif

c Establish sensed variable magnitude and actuated node location.
      call clvar(svctl,ipcmp,ipnod)

c Set control law parameters: max damper position,
c min damper position, value at max damper position (C,
c kJ/kg, or kg/s), value at min damper position (C,
c kJ/kg, or kg/s), first connection number and second
c connection number.
      dpx=pmiscd(icfp,idtypp,iperp,2)
      dpm=pmiscd(icfp,idtypp,iperp,3)
      sgu=pmiscd(icfp,idtypp,iperp,4)
      sgl=pmiscd(icfp,idtypp,iperp,5)
      icon1=int(pmiscd(icfp,idtypp,iperp,6))
      icon2=int(pmiscd(icfp,idtypp,iperp,7))

c If this is first pass, then initialise second 
c data item of array 'tmdr'.
      if(ipass.eq.0) then
         do 77 i=1,mcl
  77     tmdr(i,2)=0.0
         ipass=1
      endif
      
c If second item of array 'tmdr' for this control loop
c is zero, calculate sum of mass diversion ratios.
      if(int(tmdr(icfp,2)).eq.0) then
         if(icon1.gt.0.and.icon2.gt.0) then
            tmdr(icfp,1)=pcondr(icon1)+pcondr(icon2) 
         else
            tmdr(icfp,1)=0.0
         endif
      endif

c Test for 'realistic' values.
      if(sgu.lt.sgl)then
         call edisp(iuout, ' pcl05: fatal error on signal set point.')
         call epwait
         call epagend
         STOP
      endif

c Test to establish if control required.
      call eclose((SGU-SGL),0.00,0.0001,closea)
      if (svctl.ge.sgu) then
         dp=dpx
      else if(svctl.gt.sgl.and.(.NOT.closea)) then

c Evaluate damper position value.
         pgain=(dpx-dpm)/(sgu-sgl)
         dp=dpx-pgain*(sgu-svctl)
      else
         dp=dpm
      end if

c Assign 'cdata'.
      cdata(ipcmp,1)=dp

C Non-ideal actuator characteristics
      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

c and modify mass diversion ratio for other connection so that
c sum of mass diversion ratios for the two connections remains
c the same.
      if(icon1.gt.0.and.icon2.gt.0)
     &   pcondr(icon1)=tmdr(icfp,1)-(dp*pcondr(icon2))

c Set second data for this control loop so that
c total diversion ratio is not calculated again.
      tmdr(icfp,2)=1.0

c Trace output.
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(40).ne.0) then
         write(itu,*) ' dp = ',dp
         write(itu,*) ' Leaving subroutine pcl05'
      end if

c Return to calling module
      return
      end

C ******************** PCL06 ********************

C PCL06 is a `Null Controller'. The output from
C this controller is exactly equal to the  
C controller's input sensor value.

      SUBROUTINE PCL06
#include "plant.h"
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),npcdt(mcl),
     &            ipcdv(mcl,mpcdt,2),npcdp(mcl,mpcdt),
     &            tpcps(mcl,mpcdt,mpcdp),ipctyp(mcl,mpcdt,mpcdp),
     &            ipclaw(mcl,mpcdt,mpcdp),pmiscd(mcl,mpcdt,mpcdp,misc)
      COMMON/PCLSOL/ICFP,IDTYPP,IPERP

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine PCL06'
      END IF

C Establish the sensed condition and the 'actuated' component and node.
      CALL CLVAR(SVCTL,IPCMP,IPCVR)

C CDATA for this component is exactly equal to the sensed condition.
      CDATA(IPCMP,IPCVR)=SVCTL

C Non-ideal actuator characteristics
      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Leaving subroutine PCL06'

      RETURN
      END

C ******************** PCL07 ********************

C PCL07 is a 'Duty Cycle' controller which (on a time-cycled 
C basis) forces selected plant control loop actuators to the 
C OFF state by setting all plant component control variables 
C to zero.

      SUBROUTINE PCL07
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PSTCTR/NSINCP,NSNCPR

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)

      COMMON/NDUTY/NDUTYP(MCL),NTSOFF(MCL),NOFFCT(MCL),NDUTYS(MCL),
     &NDUTYC(MCL),NSCPAV(MCL),CDATON(MPCOM,MMISCD,MCL)
C COMMON BLOCK VARIABLES FOR 'NDUYTC' ARE:-
C NDUTYP  = duty cycle period (in plant time-steps);
C NTSOFF  = no. of plant time-steps `off' per duty cycle period;
C NOFFCT  = counter for `off' time-steps in duty cycle period;
C NDUTYS  = the plant time-step in period to commence `switch off';
C NDUTYC  = counter for logging current position in cycle period;
C NSCPAV  = memory for plant time-step.

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' ENTERING SUBROUTINE pcl07'
      ENDIF

C Fatal error test.
      IF(INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.3)THEN
         CALL EDISP(iuout,
     &    ' PCL07: fatal error -> no. of misc. data items')
         CALL EPWAIT
         CALL EPAGEND
         STOP
      ENDIF

C Set control parameters.
       NDUTYP(ICFP)=INT(PMISCD(ICFP,IDTYPP,IPERP,2))
       NTSOFF(ICFP)=INT(PMISCD(ICFP,IDTYPP,IPERP,3))
       NDUTYS(ICFP)=INT(PMISCD(ICFP,IDTYPP,IPERP,4))

C Establish the sensed condition and the `actuated' component.
       CALL CLVAR(SVCTL,IPCMP,IPCVR)

C Set 'cdaton' = cdata for `on' period of duty cycle.
       IF(NSCPAV(ICFP).EQ.0)THEN
          DO 10 IPCVR=1,NCI(IPCMP)   
            CDATON(IPCMP,IPCVR,ICFP)=CDATA(IPCMP,IPCVR)
   10     CONTINUE
       ENDIF

C At each time-step.
      IF(NSINCP.EQ.(NSCPAV(ICFP)+1))THEN
C Increment period time-step counter.
         ndutyc(icfp)=ndutyc(icfp)+1

C If at start of cycle period.
         IF(NDUTYC(ICFP).GT.NDUTYP(ICFP))THEN
C Reset duty cycle counter.
            NDUTYC(ICFP)=1
C Reset time-steps 'off' counter.
           NOFFCT(ICFP)=0
         ENDIF

C Possibly increment 'off' counter.
         IF(NDUTYC(ICFP).GE.NDUTYS(ICFP).AND.
     &      NOFFCT(ICFP).LE.NTSOFF(ICFP))THEN
            NOFFCT(ICFP)=NOFFCT(ICFP)+1
         ENDIF
      ENDIF

      IF(NDUTYC(ICFP).GE.NDUTYS(ICFP).AND.NOFFCT(ICFP).LE.NTSOFF(ICFP))
     &THEN
C 'Switch off' plant component.
         DO 20 IPCVR=1,NCI(IPCMP)
            CDATA(IPCMP,IPCVR)=0.
   20    CONTINUE
      ELSE
C 'Switch on' plant component.
            CDATA(IPCMP,IPCVR)=CDATON(IPCMP,IPCVR,ICFP)
      ENDIF

      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

C Remember current plant time-step.
      NSCPAV(ICFP)=NSINCP

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0)THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' NSINC = ',nsinc,         
     &                ' CDATA = ',cdata(ipcmp,ipcvr)
         write(itu,*) ' Leaving subroutine PCL07'
      ENDIF

      RETURN
      END

C ******************** PCL08 ********************

C PCL08 is a `Two-Position` controller.

      SUBROUTINE PCL08
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)
      COMMON/PCLOP8/LASTOU(MCL)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' ENTERING SUBROUTINE PCL08'
      ENDIF

C Fatal error test.
      IF(INT(PMISCD(ICFP,IDTYPP,IPERP,1)).LT.5)THEN
         CALL EDISP(iuout,
     &    ' PCL08: fatal error -> no. of misc. data items')
         CALL EPWAIT
         CALL EPAGEND
         STOP
      ENDIF

C Set control parameters.
      IMODE=INT(PMISCD(ICFP,IDTYPP,IPERP,2))
      SPTON=PMISCD(ICFP,IDTYPP,IPERP,3)
      SPTOFF=PMISCD(ICFP,IDTYPP,IPERP,4)
      OUTHI=PMISCD(ICFP,IDTYPP,IPERP,5)
      OUTLO=PMISCD(ICFP,IDTYPP,IPERP,6)

C Where IMODE indicates the following:

C                             OUTLO
C IMODE = +1        SPTOFF ----------
C                   SPTON  ----------
C                             OUTHI


C                             OUTHI
C IMODE = -1        SPTON  ----------
C                   SPTOFF ----------
C                             OUTLO

C Establish the sensed condition and the `actuated' component.
      CALL CLVAR(SVCTL,IPCMP,IPCVR)

      IF(IMODE.EQ.1)THEN
        IF(SVCTL.LT.SPTON)THEN
           CDATA(IPCMP,IPCVR)=OUTHI
           LASTOU(ICFP)=1
        ELSEIF(SVCTL.GT.SPTOFF)THEN
           CDATA(IPCMP,IPCVR)=OUTLO
           LASTOU(ICFP)=0
        ELSEIF(SVCTL.GE.SPTON.AND.SVCTL.LE.SPTOFF)THEN
           IF(LASTOU(ICFP).EQ.1)THEN
              CDATA(IPCMP,IPCVR)=OUTHI
           ELSEIF(LASTOU(ICFP).EQ.0)THEN
              CDATA(IPCMP,IPCVR)=OUTLO
           ENDIF
        ENDIF
      ELSEIF(IMODE.EQ.-1)THEN
        IF(SVCTL.LT.SPTON)THEN
           CDATA(IPCMP,IPCVR)=OUTLO
           LASTOU(ICFP)=0
        ELSEIF(SVCTL.GT.SPTOFF)THEN
           CDATA(IPCMP,IPCVR)=OUTHI
           LASTOU(ICFP)=1
        ELSEIF(SVCTL.GE.SPTON.AND.SVCTL.LE.SPTOFF)THEN
           IF(LASTOU(ICFP).EQ.1)THEN
              CDATA(IPCMP,IPCVR)=OUTHI
           ELSEIF(LASTOU(ICFP).EQ.0)THEN
              CDATA(IPCMP,IPCVR)=OUTLO
           ENDIF
        ENDIF
      ENDIF

      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0)THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' NSINC = ',nsinc,         
     &                ' CDATA = ',cdata(ipcmp,ipcvr)
         write(itu,*) ' Leaving subroutine PCL08'
      ENDIF

      RETURN
      END

C ******************** PCL09 ********************

C PCL09 is a multi-sensor `Two-Position` controller.

      SUBROUTINE PCL09
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)

      COMMON/PCLOP8/LASTOU(MCL)
      COMMON/CPMVAR/NPSEN(MCL),ISMODP(MCL),IPMSN(MCL,MPSEN,5),
     &SVCTLM(MCL,MPSEN),SNWGTP(MPSEN)

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' ENTERING SUBROUTINE PCL09'
      ENDIF

C Fatal error test.
      IF(INT(PMISCD(ICFP,IDTYPP,IPERP,1)).LT.10.OR.
     &INT(PMISCD(ICFP,IDTYPP,IPERP,1)).GT.18)THEN
         CALL EDISP(iuout,
     &    ' PCL09: fatal error -> no. of misc. data items')
         CALL EPWAIT
         CALL EPAGEND
         STOP
      ENDIF

      SNWTOT=0.0

C Control parameters:-
C OUTHI  = Output at `ON` state.
C OUTLO  = Output at `OFF` state.
C IOUMOD = Mode of operation:-
C         +1 for `ON` if control point is less than 
C            some function of auxiliary sensor(s).
C         -1 for `ON` if control point is greater than
C            some function of auxiliary sensor(s).
C NPSEN  = Number of auxiliary sensors.
C IAUXFN = Function for auxiliary sensors:-
C          1 = greatest value of auxiliary sensors;    
C          2 = least value of auxiliary sensors;
C          3 = mean value of auxiliary sensors;
C          4 = user-defined weighting of auxiliary sensors.
C IPMSN  = Sensor details.
C SNWGTP = Sensor weighting when IAUXFN=4.

C Set control parameters.
      OUTHI=PMISCD(ICFP,IDTYPP,IPERP,2)
      OUTLO=PMISCD(ICFP,IDTYPP,IPERP,3)
      IOUMOD=INT(PMISCD(ICFP,IDTYPP,IPERP,4))
      NPSEN(ICFP)=INT(PMISCD(ICFP,IDTYPP,IPERP,5))
      IAUXFN=INT(PMISCD(ICFP,IDTYPP,IPERP,6))
      DO 10 L=1,NPSEN(ICFP)
        IPMSN(ICFP,L,1)=INT(PMISCD(ICFP,IDTYPP,IPERP,7+4*(L-1)))
        IPMSN(ICFP,L,2)=INT(PMISCD(ICFP,IDTYPP,IPERP,8+4*(L-1)))
        IPMSN(ICFP,L,3)=INT(PMISCD(ICFP,IDTYPP,IPERP,9+4*(L-1)))
        IPMSN(ICFP,L,4)=INT(PMISCD(ICFP,IDTYPP,IPERP,10+4*(L-1)))
        IF(IAUXFN.EQ.4)THEN
          SNWGTP(L)=PMISCD(ICFP,IDTYPP,IPERP,13+(L-1)+4*(NPSEN(ICFP)-1))
          SNWTOT=SNWTOT+SNWGTP(L)
        ENDIF
10    CONTINUE      

C Fatal error tests.
      IF(IOUMOD.NE.1.AND.IOUMOD.NE.-1)THEN
         STOP ' PCL09: Incorrect output mode flag '
      ENDIF
      IF(IAUXFN.EQ.4.AND.INT(SNWTOT).NE.100)THEN
         STOP ' PCL09: sum of weighting factors does not equal 100.0'
      ENDIF

C Establish the sensed condition and the `actuated' component.
      CALL CLVAR(SVCTL,IPCMP,IPCVR)

C Establish the auxiliary sensed condition(s).
      CALL CLMVAR

C Determine the resultant auxiliary signal.
      CALL CLAXSN(IAUXFN,SVCTLX)

C Determine controller output.
      IF(IOUMOD.EQ.1)THEN
         IF(SVCTL.LE.SVCTLX)THEN
            CDATA(IPCMP,IPCVR)=OUTHI
            LASTOU(ICFP)=1
         ELSEIF(SVCTL.GT.SVCTLX)THEN
            CDATA(IPCMP,IPCVR)=OUTLO
            LASTOU(ICFP)=0         
         ENDIF
      ELSEIF(IOUMOD.EQ.-1)THEN
          IF(SVCTL.LE.SVCTLX)THEN
            CDATA(IPCMP,IPCVR)=OUTLO
            LASTOU(ICFP)=0
         ELSEIF(SVCTL.GT.SVCTLX)THEN
            CDATA(IPCMP,IPCVR)=OUTHI
            LASTOU(ICFP)=1
         ENDIF 
      ENDIF

C Non-ideal actuator characteristics
      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0)THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' NSINC = ',nsinc,         
     &                ' CDATA = ',cdata(ipcmp,ipcvr)
         write(itu,*) ' Leaving subroutine PCL09'
      ENDIF

      RETURN
      END

C ******************** PCL10 ********************

C PCL10 is a `Zero Energy Band' controller.

      SUBROUTINE PCL10
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      COMMON/PCTL/NCL,IPSN(MCL,4),IPAN(MCL,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine PCL10'
      END IF

C Fatal error tests
      IF(INT(PMISCD(ICFP,IDTYPP,IPERP,1)).NE.8)then
        call edisp(iuout,
     &    ' PCL10: fatal error on number of control data items.')
        call epwait
        call epagend
        STOP
      endif

C Establish sensed variable magnitude and actuated node location
      CALL CLVAR(SVCTL,IPCMP,IPCVR)

C Controller operation:
C If SVCTL < SPHHI then OUT=HTGHI
C If SPHHI < SVCTL < SPHLO then proportional output
C IF SPHLO < SVCTL < SPCLO then OUT=zero
C If SPCLO < SVCTL < SPCHI then proportional output
C If SVCTL > SPCHI then OUT=CLGHI

C              HTGHI ----                     ---- CLGHI
C                          \                /
C                           \              / 
C                            \            /  
C                       HTGLO \ _ ZEB__ _/ CLGLO
C                        
C                        ^    ^         ^    ^
C                        |    |         |    |
C                        |    |         |    |
C                     SPHHI SPHLO     SPCLO SPCHI

      HTGHI=PMISCD(ICFP,IDTYPP,IPERP,2)
      HTGLO=PMISCD(ICFP,IDTYPP,IPERP,3)
      CLGLO=PMISCD(ICFP,IDTYPP,IPERP,4)
      CLGHI=PMISCD(ICFP,IDTYPP,IPERP,5)
      SPHHI=PMISCD(ICFP,IDTYPP,IPERP,6)
      SPHLO=PMISCD(ICFP,IDTYPP,IPERP,7)
      SPCLO=PMISCD(ICFP,IDTYPP,IPERP,8)
      SPCHI=PMISCD(ICFP,IDTYPP,IPERP,9)

C Establish heating ratio.
      RH=(HTGHI-HTGLO)/(SPHLO-SPHHI)
C Extablish cooling ratio.
      RC=(CLGHI-CLGLO)/(SPCHI-SPCLO)

      IF(SVCTL.LT.SPHHI)THEN
         OUT=HTGHI
      ELSEIF(SVCTL.GE.SPHHI.AND.SVCTL.LE.SPHLO)THEN
         OUT=(RH*(SPHLO-SVCTL))+HTGLO
      ELSEIF(SVCTL.GT.SPHLO.AND.SVCTL.LT.SPCLO)THEN
         OUT=0.0
      ELSEIF(SVCTL.GE.SPCLO.AND.SVCTL.LE.SPCHI)THEN
         OUT=(RC*(SVCTL-SPCLO))+CLGLO
      ELSEIF(SVCTL.GT.SPCHI)THEN
         OUT=CLGHI
      ENDIF

C Assign CDATA
      CDATA(IPCMP,IPCVR)=OUT

C Non-ideal actuator characteristics
      ipa4=ipan(icfp,4)
      if(ipa4.eq.-1)call CTLROP

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' OUT = ',OUT
         WRITE(ITU,*) ' Leaving subroutine PCL10'
      END IF

      RETURN
      END

C ******************** PSYSPT ********************

C PSYSPT computes set points for air-handling plant systems.
C Refer ESP-r training folder:
C " ....... esp-r/training/plant/AHU'

      SUBROUTINE PSYSPT(ire,mdflag,imx,isupdc,iretdc,zntpsp,znrhsp,stpt)
#include "plant.h"
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      logical closea
      
      PARAMETER (SMALL=1.0E-15)

      PATMOS=1013.25

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' ENTERING SUBROUTINE PSYSPT'
      ENDIF

C Establish supply enthalpy.
      ETHSPC=ENTHP2(CSVF(isupdc,1),CSVF(isupdc,3)/CSVF(isupdc,2))

C Establish return enthalpy.
      ETHRTC=ENTHP2(CSVF(iretdc,1),CSVF(iretdc,3)/CSVF(iretdc,2))

C Establish zone conditions.
      wstpt=HUMR(zntpsp,znrhsp,1013.25)
      ETHSPT=ENTHP2(zntpsp,wstpt)

C Establish current supply and return conditions.
      tsupc=CSVF(isupdc,1)
      tretc=CSVF(iretdc,1)
      wsupc=CSVF(isupdc,3)/CSVF(isupdc,2)
      wretc=CSVF(iretdc,3)/CSVF(iretdc,2)

C RRL== "Moisture temperature gradient".
      if(ABS((tretc-tsupc)).LT.SMALL)THEN
         return
      else
         RRL=(wretc-wsupc)/(tretc-tsupc)
      endif

C Establish RRL 'equation intercept'.
      C=wstpt-(RRL*zntpsp)

C Establish future supply enthalpy, ETHSPF
C Assume: DELTAH=(ETHRTC-ETHSPC)=(ETHSPT-ETHSPF)
      ETHSPF=ETHSPT+ETHSPC-ETHRTC

C Establish future supply temperature.
      TSUPF=(ETHSPF-(C*2555.7))/(1.02+(RRL*2555.7))
      ACOEF=1.82*RRL
      BCOEF=(2501*RRL)+(1.82*C)+1.02
      CCOEF=(2501*C)-ETHSPF
      call eclose(ACOEF,0.00,0.0001,closea)
      IF(((BCOEF**2)-(4*ACOEF*CCOEF)).GE.0.AND.(.NOT.closea))THEN
        TSUPF=(-BCOEF+((BCOEF**2)-(4*ACOEF*CCOEF))**0.5)/(2*ACOEF)
        TSUPF2=(-BCOEF-((BCOEF**2)-(4*ACOEF*CCOEF))**0.5)/(2*ACOEF)
        IF(ABS(TSUPF).GT.100.)TSUPF=TSUPF2
      ENDIF

C Establish future supply moisture content.
      WSUPF=(ETHSPF-(1.02*TSUPF))/2555.7

      IF(WSUPF.LT.SMALL)WSUPF=SMALL

C Conditions at plant system inlet.
      IF(IRE.EQ.0)THEN         
         HUMINL=HUMR(TFP,HFP,PATMOS)
      ELSE     
         HUMINL=CSVF(imx,3)/CSVF(imx,2)
      ENDIF

C Conditions at dew point.
      TWTDEW=DEWPT(WSUPF,PATMOS)
      ENTDEW=ENTHP2(TWTDEW,WSUPF)

C Require conditions at pre-heat.
      ENTPRH=ENTDEW
      HUMPRH=HUMINL
      TPHEAT=TDB(ENTPRH,HUMPRH)
      RHPHT=100.0

C If pre-heat temperature is less than mixing box temp: 
      TMBOX=CSVF(imx,1)     
      IF(TPHEAT.LE.TMBOX)THEN        
        ENTPRH=ENTHP2(TMBOX,HUMINL)
        TDBDEW=TDB(ENTPRH,WSUPF)
        RHPHT=PCRH2(TDBDEW,CSVF(imx,3)/CSVF(imx,2),PATMOS)
      ENDIF
     
C Return required setpoint to calling routine:
C Pre-heat set point.
      if(mdflag.eq.1)stpt=TPHEAT
C Re-heat set point.
      if(mdflag.eq.2)stpt=tsupf
C Relative humidity set point.
      if(mdflag.eq.3)stpt=RHPHT

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0)THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' NSINC = ',nsinc,
     &                ' STPT = ',stpt
         write(itu,*) ' Leaving subroutine PSYSPT'
      ENDIF

      RETURN
      END
