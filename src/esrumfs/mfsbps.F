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

C *************** MFSBPS
C This file contains the following modules which are mfs's general
C counterparts to be used by bps, bld and plt: MFLWSU MFLWCA

C In addition to this there are the more specific mfs counterparts:
C  MFLW2B (in mfsbld.f) to be used by bps and bld
C  MFLW2P (in mfsplt.f) to be used by bps and plt
C  MFCNTL (in mfsplt.f) to be used by bps and plt

C ESRUmfs solves a network representing one-dimensional fluid mass
C flow in building and/or plant configurations.
C This involves the calculation of fluid flow through branches and nodes
C in a network which represents a building and/or plant configuration.
C The nodes and branches describe boundary pressure points, internal
C node characteristics and flow restrictions.
C ESRUmfs uses an iterative mass balance approach in which nodal
C pressures (of unknown-pressure nodes) are adjusted until the mass
C residual of each internal node satifies some criterion.

C All ESRUmfs COMMON block variables are described in emfnetw.F
C ESRUbps COMMON block variables associated with fluid mass flow are:
C     IAIRN  - building air flow simulation index (1 = text, 2= graphic)
C     ICAAS  - fluid (= air) flow network node associated with each
C              building zone. NB fluid flow network and building zones
C              need not be matched
C     IFLWN  - plant fluid flow simulation index (1 = on)
C     ICFFS  - fluid flow network node associated with each plant
C              component. NB fluid flow network and plant energy
C              network need not be matched
C     LAPROB - fluid flow problem description file
C     LAPRES - wind pressure coefficients distribution database
C     LAFRES - fluid flow network results file
C     NCOMP  - total number of building zones
C     NCON   - total number of building intra-zone connections
C     IER1   - input error index which ensures that ESRUbps menu picks
C              which are dependent on input data cannot be selected
C              if any error was detected during the input sequence
C     CVI?   - hourly ventilation conductance for infiltration air
C              for weekdays, Saturdays and Sundays respectively
C     CVV?   - hourly ventilation conductance for ventilation air
C              for weekdays, Saturdays and Sundays respectively
C     ICC?   - hourly convection coupling index for weekdays,
C              Saturdays and Sundays resp.: ICC?=0 ventilation air
C              at constant temperature (=TIA?) ICC?=N ventilation air
C              at zone N temperature
C     TIA?   - hourly temperature of incoming air during weekdays,
C              Saturdays and Sundays resp. and corresponding to ICC?=0
C              NB In case there is a leakage network, TIA? holds the
C                 normalised (to the external temperature) ventilation
C                 conductance
C     ZMBI   - zone air flow array; 1= infiltration mass flow rate;
C              2= product of zone coupling mass flow rate and moisture
C              content; 3= product of mechanical system mass flow rate
C              and moisture content; 4= total mass flow rate lost from
C              the zone (no moisture included)

C *** MFLWSU ***********************************************************
C This is the fluid mass flow network set up routine which:
C Opens and reads fluid flow related data files: wind pressure
C       coefficients file and flow network problem description file
C Checks fluid flow network problem description
C Assigns flow results file
C Sets iteration, Pstack model, matrix solver and trace default values
C Initialises flow, derivative and pressure history variables
C Sets known constant pressure nodes pressures and reset other nodes

      SUBROUTINE MFLWSU

#include "building.h"
#include "net_flow.h"
#include "plant.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/ER1/IER1

      COMMON/C1/NCOMP,NCON
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)

      COMMON/AFN/IAIRN,LAPROB,LAPRES,LAFRES,ICAAS(MCOM)
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/FFC/IMFRES

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      DOUBLE PRECISION FLW1,FLW2,HDP,HDV,HFL,PRES,PSTACK,RESID,SAFLW
      CHARACTER*72 LAPROB,LAPRES,LAFRES
      character outs*124

      PARAMETER (SMALL=1.0E-15)

      IER1=0
      IUNIT=IFIL+4

C Open WIND PRESSURE COEFFICIENTS file, read coefficient
C sets and close.
      CALL EFOPSEQ(IUNIT,LAPRES,1,IER)
      IF(ier.ne.0) GOTO 2
      READ(IUNIT,*,IOSTAT=ISTAT,ERR=3,END=4) NPRE
      DO 100 J=1,NPRE
      READ(IUNIT,*,IOSTAT=ISTAT,ERR=3,END=4) (FPRE(I,J), I=1,MPOS)
  100 CONTINUE
      CALL ERPFREE(IUNIT,ISTAT)

C Open fluid flow PROBLEM file and do a simple validity check.
      if(iairn.eq.0.and.iflwn.eq.1) then
         CALL EFOPSEQ(IUNIT,LAPROB,1,IER)
         IF(ier.ne.0) GOTO 6
         READ(IUNIT,*,IOSTAT=ISTAT,ERR=7,END=8) NNOD,NCMP,NCNN
         IF(NNOD.LT.2.OR.NNOD.GT.MNOD) GOTO 9
         IF(NCMP.LT.1.OR.NCMP.GT.MCMP) GOTO 11
         IF(NCNN.LT.2.OR.NCNN.GT.MCON) GOTO 11

C Now read contents, do further checks and then close
C the problem file.
         CALL MFLOAD(IUNIT,IER)
         IF(IER.NE.0) GOTO 1
         CALL ERPFREE(IUNIT,ISTAT)
      endif

C Check building zone/fluid flow node (must be air node)
C connectivities.  In the case of a building zone with
C no corresponding mass flow node do not check the fluid 
C type (this is an array index 0).
      IF(IAIRN.eq.0) GOTO 120
      DO 110 ICOMP=1,NCOMP
      IF(ICAAS(ICOMP).LT.0.OR.ICAAS(ICOMP).GT.NNOD) THEN
        WRITE(outs,'(A,I3,A,I3)') ' Building zone ',ICOMP,
     &         ' mapped to invalid mass flow node ',ICAAS(ICOMP)
        call edisp(iuout,outs)
        GOTO 14
      ELSE IF(ICAAS(ICOMP).NE.0) THEN
        IF(NDFLD(ICAAS(ICOMP)).NE.1) THEN
          WRITE(outs,'(A,I3,A,I3)') ' Building zone ',ICOMP,
     &          ' references non-air mass flow node ',ICAAS(ICOMP)
          call edisp(iuout,outs)
          GOTO 14
        END IF
      END IF
  110 CONTINUE

C Check plant component inter-connection/fluid flow
C connections mapping.
  120 IF(IFLWN.EQ.0) GOTO 130
      DO 122 IPCON=1,NPCON
      IF(ICFFS(IPCON).LT.0.OR.ICFFS(IPCON).GT.NCNN) THEN
         WRITE(IUOUT,'(A,I3,A,I3)') ' Plant connection ',IPCON,
     &        ' mapped to invalid mass flow connection ',ICFFS(IPCON)
         call edisp(iuout,outs)
         GOTO 14
      END IF

C Check on fluid type equality will be performed by MZPMXT.
  122 CONTINUE

C Assign results file.
  130 CONTINUE
      IMFRES=IUNIT

C Fill valid fluid flow component type arrays.
      CALL MFCDAT

C Sets iteration, Pstack model, matrix solver and trace
C default values.  Note that iteration parameter initial
C values should be set equal to corresponding plant
C iteration parameters MAXITP, PERREL, and PERMFL.
      MAXITF=100
      FERREL=.01
      FERMFL=.0005
      PMAX=50.
      STEFFR=-0.5
      IPSMOD=1
      MSLVTP=2
      MFTRAC=-1

C Initialise flow, derivative and pressure history variables.
      DO 140 ICNN=1,NCNN
        HDP(ICNN)=DBLE(SMALL)
        HDV(ICNN)=DBLE(1.0)
        HFL(ICNN)=DBLE(SMALL)
  140 CONTINUE

C Set known constant pressure nodes pressures and reset other nodes
      DO 150 INOD=1,NNOD
        IF(NDTYP(INOD).EQ.1.OR.NDTYP(INOD).EQ.2) THEN
          PRES(INOD)=SUPNOD(INOD,1)
        ELSE
          PRES(INOD)=0.0
        END IF
  150 CONTINUE

  200 RETURN

C Error messages.
    1 call edisp(iuout,' Error checking the mass flow network.')
      GOTO 51
    2 call edisp(iuout,' Wind pressure file cannot be assigned.')
      GOTO 50
    3 call edisp(iuout,' Wind pressure file read error.')
      GOTO 50
    4 call edisp(iuout,' End of wind pressure file detected.')
      GOTO 50
    6 call edisp(iuout,' Fluid flow file cannot be assigned.')
      GOTO 51
    7 call edisp(iuout,' Fluid flow file check error.')
      GOTO 51
    8 call edisp(iuout,' End of fluid flow file found.')
      GOTO 51
    9 call edisp(iuout,' Number of spaces out of range.')
      GOTO 51
   11 call edisp(iuout,' Number of connections out of range. ')
      GOTO 51
   14 call edisp(iuout,' Energy zones to fluid nodes mismatched. ')
      GOTO 99

   50 call edisp(iuout,' Fluid flow network pressure coef file error.')
      GOTO 99
   51 call edisp(iuout,' Fluid flow network file error. ')
      GOTO 99

   99 IER1=1
      CALL ERPFREE(IUNIT,ISTAT)
      CALL EPWAIT
      GOTO 200

      END

C *** MFLWCA ***********************************************************
C Controls the fluid flows calculation for each simulation time step:
C Set time and climate variables
C Set temperatures for nodes corresponding to building energy zones
C Set temperatures for nodes corresponding to plant energy components
C Set boundary nodes temperature (type 2 & 3) and wind pressure (type 3)
C Calculate node fluid densities
C Calculate connections stack pressure
C Solve the fluid flow network mass balances
C Transfer fluid flow simulation results to flow results file
C Establish building side air flows
C Establish plant side fluid flows
C
C ISCALL - indicates whether MFLWCA is called from the building
C           side (ISCALL = 1) or from the plant side (ISCALL = 2)
C STIME  - is used to transfer the simulation time to mfs

      SUBROUTINE MFLWCA(ISCALL,STIME)

#include "building.h"
#include "net_flow.h"
#include "plant.h"
#include "cfd.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/C1/NCOMP,NCON
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Time and weather data.
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

C Most recent 'future' time-row zone air
C temperatures saved in MZLS3.
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)

C Fluid flow network data.
      COMMON/AFN/IAIRN,LAPROB,LAPRES,LAFRES,ICAAS(MCOM)
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/FFC/IMFRES
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/MFCALC/IPSMOD,MSLVTP

C Simulation start-up period indicator.
      COMMON/SUS/ISSNY

C Time-step controller data.
      common/ts/itsflg,ntscpd,itssch(msch,5),itsmus
      common/outflg/itsplt

C--CFD
      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),ICNACT(MCFND,MNZ)
C Activation of connections

      DOUBLE PRECISION FLW1,FLW2,HDP,HDV,HFL,PRES,PSTACK,RESID,SAFLW
      LOGICAL MFSAVE,dotrace
      CHARACTER*72 LAPROB,LAPRES,LAFRES,LCFD
      character outs*124
      logical bInStartup

C If trace required set dotrace=.true.
      dotrace=.true.
      IF(ITC.LE.0.OR.NSINC.LT.ITC)dotrace=.false.
      IF(ITRACE(7).EQ.0.OR.NSINC.GT.ITCF)dotrace=.false.
      if(dotrace)then
        write(outs,'(A,I5)')' Subroutine MFLWCA Trace output ',ICNT
        call edisp(itu,outs)
        ICNT=ICNT+1
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

C Set time and climate variables.
      IRY=IYEAR
      IF(IHRP.EQ.24.AND.ITS.EQ.NTSTEP) THEN
        IYD=IDYF
        CALL EDAYR(IDYF,IRD,IRM)
      ELSE
        IYD=IDYP
        CALL EDAYR(IDYP,IRD,IRM)
      END IF
      FLWTIM=STIME
      DRYB=TF
      WDIR=DF

C Modify windspeed by the wind reduction factor.
      WSPD=VF*WRED

C Set temperatures for nodes corresponding to building energy zones.
      IF(IAIRN.NE.0) THEN
        DO 10 ICOMP=1,NCOMP
          IF(ICAAS(ICOMP).GT.0) TNOD(ICAAS(ICOMP))=TFA(ICOMP)
 10     CONTINUE
      END IF

C Set temperatures for nodes corresponding to a plant component.
C Note that plant - mass flow network matching is based on
C connections.  We assume that the flow will be in the positive
C direction if the previous flow was in the +ve direction and
C vice versa.
      IF(IFLWN.NE.0) THEN
        DO 20 IPCON=1,NPCON
          ICNN=ICFFS(IPCON)
          IF((FLW1(ICNN)+FLW2(ICNN)).GE.0.0) THEN
            TNOD(NODPS(ICNN))=CONVAR(IPCON,1)
          ELSE
            TNOD(NODNE(ICNN))=CONVAR(IPCON,1)
          END IF
   20   CONTINUE
      END IF

C Set boundary nodes temperature (type 2 & 3), 'temperature coupled'
C nodes and wind pressure (type 3).
      DO 30 INOD=1,NNOD
        IF((NDTYP(INOD).EQ.2.AND.NINT(SUPNOD(INOD,2)).EQ.1).OR.
     &      NDTYP(INOD).EQ.3) TNOD(INOD)=DRYB
        IF(ITND(INOD).GT.0) TNOD(INOD)=TNOD(ITND(INOD))
        IF(NDTYP(INOD).EQ.3) THEN

C Set wind direction index 1-16 corresponding to intervals
C with mid-points at 0, 22.5, ... , 337.5 deg, relative to
C surface azimuth (i.e. wind normal to surface = index 1).
          IWDR=NINT((WDIR-SUPNOD(INOD,2))/22.5)+1
          IF(IWDR.LT.1 ) IWDR=IWDR+16
          IF(IWDR.GT.16) IWDR=IWDR-16

C Calculate wind pressure and include stack 
C pressure (based on PRES(at height 0) = 0.0).
          CD=FPRE(IWDR,INT(SUPNOD(INOD,1)))
          RO=DENSIT(1,DRYB)
          PRES(INOD)=CD*.5*RO*WSPD*WSPD-9.81*RO*HNOD(INOD)
        END IF
   30 CONTINUE

C Calculate node fluid densities.
      DO 40 INOD=1,NNOD
        RHON(INOD)=DENSIT(NDFLD(INOD),TNOD(INOD))
   40 CONTINUE

C Calculate connections stack pressure based on previous
C flow direction.
      DO 50 ICNN=1,NCNN
        IDPS=NODPS(ICNN)
        IDNE=NODNE(ICNN)
        IF(IPSMOD.EQ.1) THEN
          IF(FLW1(ICNN).GE.0.0) THEN
            PSTACK(ICNN)=9.81*(RHON(IDPS)*(HNOD(IDPS)-HNOD(IDNE))
     &                         +HGTNE(ICNN)*(RHON(IDNE)-RHON(IDPS)))
          ELSE
            PSTACK(ICNN)=9.81*(RHON(IDNE)*(HNOD(IDPS)-HNOD(IDNE))
     &                         +HGTPS(ICNN)*(RHON(IDNE)-RHON(IDPS)))
          ENDIF
        ELSE IF(IPSMOD.EQ.2) THEN
          PSTACK(ICNN)=9.81*0.5*
     &                ((RHON(IDPS)+RHON(IDNE))*(HNOD(IDPS)-HNOD(IDNE))
     &               +(HGTNE(ICNN)+HGTPS(ICNN))*(RHON(IDNE)-RHON(IDPS)))
        ELSE
          STOP ' MFLWCA: unresolvable error !'
        END IF
   50 CONTINUE

      CALL MFSOLV

C Transfer mass flow simulation results to flow results file,
C unless we are still in the simulation start-up period.
C Note that in case of building AND plant, the
C results are only transferred when MFLWCA is called from the
C plant side (ie. 'inside' a plant time step) to prevent
C non-monotonity of the mass flow results time-series.
C Note that 'itsmus' ensures that result transfer occurs
C at user specified time-step.
      MFSAVE=.false.

C Check in simulation period (i.e. not in start up).
      if (bInStartup())then
        continue
C Debug.
C        write(6,*) 'bInstartup is true w/in flow'
      else
C Debug.
C        write(6,*) 'bInstartup is false w/in flow'
C      if ((IDYP.ge.ISDS).or.
C     &    (IDYF.eq.ISDS.and.IHRF.eq.1.and.FLWTIM.le.1.0)) then

C Check if results should be written (timestep controllers etc).
        if (itsmus.ne.0.or.itsplt.eq.1) then

C Finally check for coordination between plant and mass flow networks
C to avoid writing the same data twice.
          if (IAIRN.ge.1) then
            if (IFLWN.ne.1) then

C Mass flow and no plant therefore save.
              MFSAVE=.true.
            elseif (ISCALL.eq.2) then

C Mass flow and plant therefore only save if called from plant.
              MFSAVE=.true.
            endif
          else
            if (IFLWN.eq.1) then

C Plant only therefore save.
              MFSAVE=.true.
            endif
          endif
        endif
      endif

C Debug.
C     if (MFSAVE) then
C        write(6,*) 'std flow startup reached '
C      else
C        write(6,*) 'std flow startup not reached '
C      endif
C      write(6,*) 'flow:',IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,
C     &    BTIMEP,BTIMEF,ISSNY,ISDS,ISDF,MFSAVE,FLWTIM,IAIRN,
C     &    IFLWN,ISCALL,itsmus,itsplt

C Save results
      if (MFSAVE) then

C MFOUTP - should only write here for trace (equivalent to the
C ASCII version of flow results file).
        if(dotrace)call MFOUTP(ITU)
        call MFOUTPB(IMFRES)
      endif

C Establish building side air flows (ie. ventilation conductances)
C if MFLWCA is called from the building side.
      IF(IAIRN.ge.1.AND.ISCALL.EQ.1) CALL MFLW2B

C Establish plant side fluid flows
C if MFLWCA is called from the plant side.
      IF(IFLWN.EQ.1.AND.ISCALL.EQ.2) CALL MFLW2P

      RETURN
      END

