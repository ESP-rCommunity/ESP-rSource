C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001.

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

C *************** MFSBPS
C This file contains the following subroutines, which provide the
C interface to mfs when encapsulated in bps (or bld or plt):
C   MFLWSU
C   MFLWCA

C Note the other interface-related subroutines:
C  MFLW2B (in mfsbld.f) used by bps and bld
C  MFLW2P (in mfsplt.f) used by bps and plt
C  MFCNTL (in mfsplt.f) used by bps and plt

C The mfs algorithm solves for the flow of an incompressible fluid
C within a network established to represent a multi-zone building
C or plant systems in whole or part). This entailes the calculation
C of the mass flows through branches (flow restrictions) that are
C subject to a time varying pressure difference. Note that 'building'
C zones may be filled by a fluid other than air so that ESP-r's
C building-side modelling features may be employed to represent
C supply-side systems.

C The mfs-realted variables are described in emfnetw.F while the
C principal bps variables associated with fluid flow are as follows.
C     IAIRN  - building-side fluid flow index (0 = absent; 1 = present).
C     ICAAS  - maps zones to corresponding flow network nodes.
C     IFLWN  - plant-side fluid flow index (0 = absent; 1 = present)
C     ICFFS  - maps plant connections to corresponding flow network nodes.
C     LAPROB - flow network description file.
C     NCOMP  - total number of building zones.
C     NCON   - total number of inter-zone connections.
C     CVI?   - conductance for zone infiltration.
C     CVV?   - conductance for zone ventilation.
C     ICCZ?  - Ventilation source (0 = specified temperature (=TIA?);
C              n = zone 'n' temperature).
C     ZMBI   - zone air flow array:
C                 position 1 = infiltration mass flow rate;
C                          2 = product of zone-coupled mass flow rate
C                              and moisture content;
C                          3 = product of mechanical mass flow rate and
C                              moisture content;
C                          4 = mass flow rate lost from the zone

C *** MFLWSU ***********************************************************
C Flow network set up routine:
C    - open and read flow network description files
C    - get pressure coefficients from database
C    - assign flow results file
C    - set iteration control parameters
C    - initialise flows, derivatives and pressure history
C     -set known pressure nodes.

      SUBROUTINE MFLWSU

#include "building.h"
#include "net_flow.h"
#include "plant.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for wind pressure databases)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/ER1/IER1

C      integer ncomp,ncon
C      COMMON/C1/NCOMP,NCON
C      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
C     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
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
      CHARACTER*72 LAPROB
      character outs*124
      character lworking*144,fs*1
      logical concat,xst,unixok,close

      PARAMETER (SMALL=1.0E-15)

      IER1=0

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C If for some reason IAPRES is still zero set it to IFIL+4
      if(IAPRES.eq.0)IAPRES=IFIL+4
      IUNIT=IFIL+4

C Open WIND PRESSURE COEFFICIENTS file, read coefficient
C sets and close file. Adjust file name depending on location.
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        CALL EFOPSEQ(IAPRES,lapres,1,IER)
      elseif(ipathapres.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    lapres(1:lnblnk(lapres))

        CALL EFOPSEQ(IAPRES,lworking,1,IER)
      endif

      IF(ier.ne.0) GOTO 2
      READ(IAPRES,*,IOSTAT=ISTAT,ERR=3,END=4) NPRE
      DO 100 J=1,NPRE
      READ(IAPRES,*,IOSTAT=ISTAT,ERR=3,END=4) (FPRE(I,J), I=1,MPOS)
  100 CONTINUE
      CALL ERPFREE(IAPRES,ISTAT)

C Open fluid flow PROBLEM file and do a simple validity check.
      if(iairn.eq.0.and.iflwn.eq.1) then
         CALL EFOPSEQ(IUNIT,LAPROB,1,IER)
         IF(ier.ne.0) GOTO 6
         READ(IUNIT,*,IOSTAT=ISTAT,ERR=7,END=8) NNOD,NCMP,NCNN
         IF(NNOD.LT.2.OR.NNOD.GT.MNOD) GOTO 9
         IF(NCMP.LT.1.OR.NCMP.GT.MCMP) GOTO 11
         IF(NCNN.LT.2.OR.NCNN.GT.MCON) GOTO 12

C Read flow network description file, perform data checks and close
C the file.
         CALL MFLOAD(IUNIT,IER)
         IF(IER.NE.0) GOTO 1
         CALL ERPFREE(IUNIT,ISTAT)
      endif

C Assign results file channel.
      IMFRES=IUNIT

C Establish available flow component models.
      CALL MFCDAT

C Set iteration control variables (N.B. plant-related variables MAXITP,
C PERREL and PERMFL should be set equal to MAXITF, FERREL and FERMFL
C respectively.
      MAXITF=1500
      FERREL=1.0E-4
      FERMFL=1.0E-10
      PMAX=50.
      STEFFR=-0.25
      IPSMOD=1
      MSLVTP=2
      MFTRAC=-1

C Initialise flow, derivative and pressure history variables.
      DO 140 ICNN=1,NCNN
        HDP(ICNN)=DBLE(SMALL)
        HDV(ICNN)=DBLE(1.0)
        HFL(ICNN)=DBLE(SMALL)
  140 CONTINUE

C Assign pressure to fixed pressure nodes and initialise other nodes.
      DO 150 INOD=1,NNOD
        IF(NDTYP(INOD).EQ.1.OR.NDTYP(INOD).EQ.2) THEN
          PRES(INOD)=SUPNOD(INOD,1)
        ELSE
          PRES(INOD)=0.0
        END IF
  150 CONTINUE

      RETURN

C Error messages.
    1 write(outs,'(A)')
     & ' MFLWSU: error in mass flow network.'
      GOTO 50
    2 write(outs,'(A)')
     & ' MFLWSU: pressure coefficients file problem.'
      GOTO 50
    3 write(outs,'(A)')
     & ' MFLWSU: pressure coefficients file read error.'
      GOTO 50
    4 write(outs,'(A)')
     & ' MFLWSU: pressure coefficients file premature end.'
      GOTO 50
    6 write(outs,'(A)')
     & ' MFLWSU: Fluid flow results file problem.'
      GOTO 50
    7 write(outs,'(A)')
     & ' MFLWSU: Fluid flow results file read error.'
      GOTO 50
    8 write(outs,'(A)')
     & ' MFLWSU: Fluid flow results file premature end.'
      GOTO 50
    9 write(outs,'(A)')
     & ' MFLWSU: Number of network nodes out of range.'
      GOTO 50
   11 write(outs,'(A)')
     & ' MFLWSU: Number of network components out of range.'
      GOTO 50
   12 write(outs,'(A)')
     & ' MFLWSU: Number of network connections out of range.'

   50 call edisp(iuout,outs)
      write(outs,'(A)')' Terminating simulation!'
      call edisp(iuout,outs)
      IER1=1
      CALL ERPFREE(IUNIT,ISTAT)

      stop
      END

C *** MFLWCA ***********************************************************
C Controls the fluid flows calculation for each simulation time step:
C    - set time and climate variables;
C    - set temperatures for nodes corresponding to building energy zones;
C    - set temperatures for nodes corresponding to plant energy components;
C    - set boundary nodes temperature (type 2 & 3) and wind pressure (type 3);
C    - calculate node fluid densities;
C    - calculate connections stack pressure;
C    - solve the flow network mass balance;
C    - transfer fluid flow simulation results to results file;
C    - establish building side flows;
C    - establish plant side flows.
C
C ISCALL - indicates whether MFLWCA is called from the building
C          side (= 1) or plant side (= 2).
C STIME  - used to transfer the simulation time to mfs.

      SUBROUTINE MFLWCA(ISCALL,STIME)

#include "building.h"
#include "net_flow.h"
#include "plant.h"
#include "cfd.h"
#include "roam.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Present (P) and future (F) time-row time and other time-related data.
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

C Weather data.
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/TRANSO/LTROR,PAZIO(MCOM,MS)

C Most recent zone air temperatures as saved in MZLS3.
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)

C Flow network data.
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
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

C CFD-related data.
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

      COMMON/zfluid/znotair(mcom),zfldK,zfldD,zfldC,zfldA,
     &              zSWAp(mcom),zSWAf(mcom)

      DOUBLE PRECISION FLW1,FLW2,HDP,HDV,HFL,PRES,PSTACK,RESID,SAFLW
      LOGICAL MFSAVE,dotrace,znotair
      CHARACTER LAPROB*72,LCFD*72
      character outs*124
      logical bInStartup

C Is trace required?
      dotrace=.true.
      IF(ITC.LE.0.OR.NSINC.LT.ITC)dotrace=.false.
      IF(ITRACE(7).EQ.0.OR.NSINC.GT.ITCF)dotrace=.false.
      if(dotrace)then
        write(outs,'(A,I5)')' MFLWCA: Trace output ',ICNT
        call edisp(itu,outs)
        ICNT=ICNT+1
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

C Check that zones are legally mapped to network flow nodes.
      if(IAIRN.eq.0) goto 1
      do ICOMP=1,NCOMP
        if(ICAAS(ICOMP).lt.0.or.ICAAS(ICOMP).GT.NNOD) then
          write(outs,'(2(A,I3))') ' Building zone ',ICOMP,
     &           ' is mapped to invalid mass flow node.',ICAAS(ICOMP)
          call edisp(iuout,outs)
          goto 99
        else if(ICAAS(ICOMP).ne.0) then
          if(znotair(ICOMP)) then		! zone is not air filled
            if(NDFLD(ICAAS(ICOMP)).ne.2) then
              write(outs,'(2(A,I3))') ' Building zone ',ICOMP,
     &            ' incorrectly references an air flow node.',
     &            ICAAS(ICOMP)
              call edisp(iuout,outs)
              goto 99
            end if
          else					! zone is air filled
            if(NDFLD(ICAAS(ICOMP)).ne.1) then
              write(outs,'(2(A,I3))') ' Building zone ',ICOMP,
     &          ' incorrectly references a non-air flow node.',
     &          ICAAS(ICOMP)
              call edisp(iuout,outs)
              goto 99
            end if
          end if
        end if
      end do

C Check that plant network connections are legally mapped to
C network flow nodes.
    1 if(IFLWN.EQ.0) GOTO 2

      do IPCON=1,NPCON
        if(ICFFS(IPCON).LT.0.OR.ICFFS(IPCON).GT.NCNN) then
           write(outs,'(A,I3,A,I3)') ' Plant network connection ',
     &          IPCON,' references an invalid mass flow connection',
     &          ICFFS(IPCON)
           call edisp(iuout,outs)
           goto 99
        end if
      end do

C Set time and weather variables.
    2 IRY=IYEAR
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

C Set temperatures for nodes corresponding to a plant component (noting
C that network nodes are matched to plant network connections).
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

C Set boundary node temperature (for node types 2 & 3) and
C pressure (type 3).
      DO 30 INOD=1,NNOD
        IF((NDTYP(INOD).EQ.2.AND.NINT(SUPNOD(INOD,2)).EQ.1).OR.
     &      NDTYP(INOD).EQ.3) TNOD(INOD)=DRYB
        IF(ITND(INOD).GT.0) TNOD(INOD)=TNOD(ITND(INOD))
        IF(NDTYP(INOD).EQ.3) THEN

C Set wind direction index, 1-16, corresponding to intervals
C with mid-points at 0, 22.5, ... 337.5 deg relative to
C surface azimuth (i.e. wind normal to surface = index 1).
          IWDR=NINT((WDIR-SUPNOD(INOD,2))/22.5)+1

C If roaming activated then rotate the model.
          IF(IROAM.EQ.1)THEN
            ANGLE=SUPNOD(INOD,2)+REAL(LTROR)
            IF(ANGLE.GE.360.)ANGLE=ANGLE-360.
            IWDR=NINT((WDIR-ANGLE)/22.5)+1
          ENDIF
          IF(IWDR.LT.1 ) IWDR=IWDR+16
          IF(IWDR.GT.16) IWDR=IWDR-16

C Calculate wind pressure and include stack 
C pressure (based on pressure at height 0 = 0.0).
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
          write(outs,'(A)') ' MFLWCA: unresolvable error!'
          call edisp(iuout,outs)
          goto 99
        END IF
   50 CONTINUE

      CALL MFSOLV

C Transfer mass flow simulation results to results file,
C unless we are still in the simulation start-up period.
C Note that for a building AND plant simulation, the
C results are only transferred when MFLWCA is called from the
C plant side (i.e. 'inside' a plant time step) to prevent
C non-monotonity of the mass flow results time-series.
C Note that 'itsmus' ensures that result transfer occurs
C at the user specified time-step.
      MFSAVE=.false.

C Check if within simulation period (i.e. not in start up).
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

C Save results.
      if (MFSAVE) then

C MFOUTP - should only write here for trace (equivalent to the
C ASCII version of flow results file).
        if(dotrace)call MFOUTP(ITU)
        call MFOUTPB(IMFRES)
      endif

C Establish building side ventilation conductances if MFLWCA
C called from the building side.
      IF(IAIRN.ge.1.AND.ISCALL.EQ.1) CALL MFLW2B

C Establish plant side flows if MFLWCA is called from the
C plant side.
      IF(IFLWN.EQ.1.AND.ISCALL.EQ.2) CALL MFLW2P

      RETURN

   99 write(outs,'(A)')' MFLWCA: simulation terminating!'
      call edisp(iuout,outs)
      stop
      END

