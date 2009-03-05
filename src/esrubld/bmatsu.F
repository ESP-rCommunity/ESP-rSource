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

C MZCOE1
C MZCOE2
C MZCOE3
C MZSETU
C LUMPED creates the coefficients for lumped constructions.

c ******************** MZCOE1 ********************

c MZCOE1 sets up those coefficients (or partial coefficients)
c of the implicit difference equations which are constant with
c time - since they are computed from factors which are
c independent of time - and need therefore only be computed
c once for any given design hypothesis.

c Common block variables are:

c C     - Contains the positive coefficients (wholly or
c         partially established) of the temperature terms
c         relating to nodes in thermal contact with the
c         intra-construction node in question.   All other
c         coefficients are derivable from these.
c
c QC    - Contains the positive coefficients of the nodal
c         heat generation terms.

c In general terms any node within a multilayered construction
c is constrained to conform to the following form:

c A*TF1 + B*TF + C*TF2 + D*QF = E*TP1 + F*TP + G*TP2 + H*QP

c where:

c      A=-C(MCOM,MS,MN,1)
c      B=(2+C(MCOM,MS,MN,1)+C(MCOM,MS,MN,2))
c      C=-C(MCOM,MS,MN,2)
c      D=-QC(MCOM,MS,MN)
c      E=C(MCOM,MS,MN,1)
c      F=(2-C(MCOM,MS,MN,1)-C(MCOM,MS,MN,2))
c      G=C(MCOM,MS,MN,2)
c      H=QC(MCOM,MS,MN)

c      TF1 is the future temperature at 'outer' node in thermal
c      contact with node in question.

c      TF is the future temperature at node in question.

c      TF2 is the future temperature at 'inner' node in thermal
c      contact with node in question.

c      QF is the future heat injection at node in question.

c      and TP1, TP, TP2 & QP are the present temperatures
c      and heat generation at the above nodes.

c For nodes situated at surface layers (receiving radiant
c energy injections and convective exchanges) and zone air points
c (receiving convective energy exchanges) the coefficients
c of the governing simulation equation are established within
c each simulation time-step as detailed in the following
c reference:

c Clarke, J A (2001) Energy Simulation in Building Design
c (2nd Edn), Buterworth Heinemann, Oxford, ISBN 0 7506 5082 6

c The multilayered construction transient conduction model
c employs a 3 node per homogeneous element scheme.   This
c means that any multilayered construction is represented
c by (2*E + 1) nodes - including surface layer nodes -
c where E is the number of homogeneous elements in the
c multilayered construction.

      SUBROUTINE MZCOE1(ICOMP)
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      COMMON/COEFMOD/MODP

      REAL MODP

      DOUBLE PRECISION DELXL,DELXP,CAP

      character outs*124 

      DT=3600.

C Consider each multilayered construction within current
C zone.
      DO 10 I=1,NCONST(ICOMP)

C For lumped constructions.
         IF(ILUMP(ICOMP,I).GE.1)THEN
           CALL LUMPED(ICOMP,I)
           GOTO 10
         ENDIF

C For discretized constructions.
         NGP=NGAPS(ICOMP,I)
         M=1
         IL=1 

C Set the air gap flage to off.
         IFLG=0

c 'Outside' convection coefficient is omitted since its value
c is time-dependent.
         IN=1 
         DELXL=THRMLI(ICOMP,I,IL,4)/real(NNDL(ICOMP,I,IL))
         CAP=THRMLI(ICOMP,I,IL,2)*THRMLI(ICOMP,I,IL,3)
         C(ICOMP,I,IN,1)=real(DT/((DELXL/2.0)*CAP))
         C(ICOMP,I,IN,2)=real(2.0*THRMLI(ICOMP,I,IL,1)*DT/
     &                       ((DELXL*DELXL)*CAP))
         QC(ICOMP,I,IN)=real(DT/((DELXL/2.0)*CAP))

C Continue with other nodes (homogeneous) in the first layer.
         DO 20 ID=1,NNDL(ICOMP,I,IL)-1
            IN=IN+1
            C(ICOMP,I,IN,1)=real(THRMLI(ICOMP,I,IL,1)*DT/
     &                          ((DELXL*DELXL)*CAP))
            C(ICOMP,I,IN,2)=C(ICOMP,I,IN,1)
            QC(ICOMP,I,IN)=real(DT/(DELXL*CAP))
   20    CONTINUE

C Continue with other layers in the construction.
         DO 30 IL=2,NELTS(ICOMP,I)

C IL: current layer, IP: previous layer.
            IP=IL-1
            FNNDL=real(NNDL(ICOMP,I,IL))
            FNNDP=real(NNDL(ICOMP,I,IP))
            DELXL=THRMLI(ICOMP,I,IL,4)/FNNDL
            DELXP=THRMLI(ICOMP,I,IP,4)/FNNDP
            
C The current layer is an air gap.
            IF(M.LE.NGP.AND.NPGAP(ICOMP,I,M).EQ.IL)THEN

C If the previous layer was a solid.
              IF(IFLG.EQ.0)THEN
                IN=IN+1
                CAP=THRMLI(ICOMP,I,IP,2)*THRMLI(ICOMP,I,IP,3)*
     &              (DELXP/2.)+1.3*1005.5*(DELXL/2.0)
                C(ICOMP,I,IN,1)=real(THRMLI(ICOMP,I,IP,1)*DT/
     &              (DELXP*CAP))
                C(ICOMP,I,IN,2)=real(DT/((RGAPS(ICOMP,I,M)/FNNDL)*CAP))
                QC(ICOMP,I,IN)=real(DT/CAP)

C If the previous layer was an air gap
              ELSE
                IN=IN+1
                CAP=1.3*1005.5*(DELXL+DELXP)/2.0
                C(ICOMP,I,IN,1)=real(DT/
     &           ((RGAPS(ICOMP,I,M-1)/FNNDP)*CAP))
                C(ICOMP,I,IN,2)=real(DT/((RGAPS(ICOMP,I,M)/FNNDL)*CAP))
                QC(ICOMP,I,IN)=real(DT/CAP)
              ENDIF
              IFLG=1

C Continue with other nodes (homogeneous) in the layer.
              DO 40 ID=1,NNDL(ICOMP,I,IL)-1
                 IN=IN+1
                 C(ICOMP,I,IN,1)=real(DT/((RGAPS(ICOMP,I,M)/FNNDL)*
     &                           1.3*1005.5*DELXL))
                 C(ICOMP,I,IN,2)=C(ICOMP,I,IN,1)
                 QC(ICOMP,I,IN)=real(DT/(1.3*1005.5*DELXL))
   40         CONTINUE
              M=M+1

C The current layer is solid.
            ELSE
              FNNDL=real(NNDL(ICOMP,I,IL))
              FNNDP=real(NNDL(ICOMP,I,IP))
              DELXL=THRMLI(ICOMP,I,IL,4)/FNNDL
              DELXP=THRMLI(ICOMP,I,IP,4)/FNNDP

C If the previous layer was solid.
              IF(IFLG.EQ.0)THEN
                IN=IN+1
                CAP=THRMLI(ICOMP,I,IP,2)*THRMLI(ICOMP,I,IP,3)*
     &              (DELXP/2.)+THRMLI(ICOMP,I,IL,2)*
     &              THRMLI(ICOMP,I,IL,3)*(DELXL/2.0)
                C(ICOMP,I,IN,1)=real(THRMLI(ICOMP,I,IP,1)*DT/
     &           (DELXP*CAP))
                C(ICOMP,I,IN,2)=real(THRMLI(ICOMP,I,IL,1)*DT/
     &           (DELXL*CAP))
                QC(ICOMP,I,IN)=real(DT/CAP)

C If the previous layer was an air gap
              ELSE
                IN=IN+1
                CAP=1.3*1005.5*(DELXP/2.)+THRMLI(ICOMP,I,IL,2)*
     &              THRMLI(ICOMP,I,IL,3)*(DELXL/2.0)
                C(ICOMP,I,IN,1)=real(DT/
     &           ((RGAPS(ICOMP,I,M-1)/FNNDP)*CAP))
                C(ICOMP,I,IN,2)=real(THRMLI(ICOMP,I,IL,1)*DT/
     &           (DELXL*CAP))
                QC(ICOMP,I,IN)=real(DT/CAP)
              ENDIF
              IFLG=0

C Continue with other nodes (homogeneous) in the layer.
              DO 50 ID=1,NNDL(ICOMP,I,IL)-1
                 IN=IN+1
                 CAP=THRMLI(ICOMP,I,IL,2)*THRMLI(ICOMP,I,IL,3)
                 C(ICOMP,I,IN,1)=real(THRMLI(ICOMP,I,IL,1)*DT/
     &                           ((DELXL*DELXL)*CAP))
                 C(ICOMP,I,IN,2)=C(ICOMP,I,IN,1)
                 QC(ICOMP,I,IN)=real(DT/(DELXL*CAP))
   50         CONTINUE
            ENDIF
   30    CONTINUE

C Internal surface node
         IN=IN+1
         IL=NELTS(ICOMP,I)
         DELXL=THRMLI(ICOMP,I,IL,4)/real(NNDL(ICOMP,I,IL))
         CAP=THRMLI(ICOMP,I,IL,2)*THRMLI(ICOMP,I,IL,3)
         C(ICOMP,I,IN,1)=real(2.0*THRMLI(ICOMP,I,IL,1)*DT/
     &    ((DELXL*DELXL)*CAP))
         C(ICOMP,I,IN,2)=real(DT/((DELXL/2.0)*CAP))
         QC(ICOMP,I,IN)=real(DT/((DELXL/2.0)*CAP))
   10 CONTINUE

C Initialise coefficient modifier for use in MZCOE2
      MODP=1.0

c Trace output ?
      IF(ITRACE(15).EQ.1.AND.IZNTRC(ICOMP).EQ.1)goto  5555
      GOTO 9999
 5555 CONTINUE
      write(outs,'(A,I4,A,I4)')
     & ' Subroutine MZCOE1     Trace output',ICNT,'    Zone',ICOMP
      call edisp(itu,outs)
      call edisp(itu,' ')
      ICNT=ICNT+1

c For each multilayered construction in turn output the
c number of nodes and the coefficient values for each.
      call edisp(itu,
     &  ' Constr. No.of       C(1)          C(2)           QC')
      call edisp(itu,'         Nodes')
      call edisp(itu,' ')
      DO 9993 I=1,NCONST(ICOMP)
        NN=NNDC(ICOMP,I)
        write(outs,'(I6,I7,3F14.3)')I,NN,C(ICOMP,I,1,1),
     &    C(ICOMP,I,1,2),QC(ICOMP,I,1)
        call edisp(itu,outs)
        do 99922 j=2,NN
          write(outs,99921)C(ICOMP,I,J,1),C(ICOMP,I,J,2),QC(ICOMP,I,J)
99921     format(13X,3F14.3)
          call edisp(itu,outs)
99922   CONTINUE
 9993 CONTINUE
 9999 RETURN
      END

c ******************** MZCOE2 ********************

c MZCOE2 modifies the coefficients computed by MZCOE1
c if the imposed computational time-step is less than one
c hour.

      SUBROUTINE MZCOE2(TSTEP)
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      COMMON/COEFMOD/MODP

      REAL MODP

      character outs*124

C Modify C and QC coefficients using TSTEP. Division by MODP to prevent cumulative 
C modifications and ensure that the coefficient multiplier is consistent
C with the timestep.
      DO 10 I=1,NCOMP
      NC=NCONST(I)
      DO 20 J=1,NC
      NN=NNDC(I,J)
      DO 30 K=1,NN
      C(I,J,K,1)=TSTEP*C(I,J,K,1)/MODP
      C(I,J,K,2)=TSTEP*C(I,J,K,2)/MODP
      QC(I,J,K)=TSTEP*QC(I,J,K)/MODP
   30 CONTINUE
   20 CONTINUE
   10 CONTINUE

C Store value of modification
      MODP=TSTEP

C Trace output ?
      IF(ITC.EQ.0)goto 9999
      IF(ITRACE(15).EQ.0)GOTO 9999
      write(outs,'(A,I4)')' Subroutine MZCOE2     Trace output',ICNT
      call edisp(itu,outs)
      write(outs,'(23X,A)')'All zones'
      call edisp(itu,outs)

C For each multilayered construction in turn output the
C number of nodes and the coefficient values of each.
      DO 9994 I=1,NCOMP
      IF(IZNTRC(I).NE.1)GOTO 9994
      write(outs,'(A,I3)')' Zone',I
      call edisp(itu,' ')
      call edisp(itu,outs)
      call edisp(itu,'(when computational time-step < one hour)')
      call edisp(itu,' Constr. No.of C(1)          C(2)           QC')
      call edisp(itu,'         Nodes')
      call edisp(itu,' ')
      NC=NCONST(I)
      DO 9991 J=1,NC
        NN=NNDC(I,J)
        write(outs,9990)J,NN,C(I,J,1,1),C(I,J,1,2),QC(I,J,1)
 9990   format(I6,I7,3F14.3)
        call edisp(itu,outs)
        do 99901 k=2,NN
          write(outs,'(13X,3F14.3)')C(I,J,K,1),C(I,J,K,2),QC(I,J,K)
          call edisp(itu,outs)
99901   CONTINUE
 9991 CONTINUE
 9994 CONTINUE
 9999 RETURN
      END

C ******************** MZCOE3 ********************

C MZCOE3 initiates the computation of the internal grey
C body configuration factors, solar gains impinging on
C internal and external surfaces, the external longwave
C radiation exchanges, internal casual gains, window conduction,
C door conduction and ventilation exchanges.

C Difference equation coefficient set-up is then completed
C by MZSETU.

      SUBROUTINE MZCOE3(ICOMP)
      COMMON/GR3D150/INDX3,DTIME

      CALL MZSOLR(ICOMP)
      CALL MZELWE(ICOMP)
      CALL MZCASI(ICOMP)
      CALL MZWCON(ICOMP)
      CALL MZVENT(ICOMP)
      CALL SPMMOD(ICOMP)
      IF(INDX3.EQ.3)THEN
        CALL MZ3CFG(ICOMP)
        CALL MZ3SU1(ICOMP)
      ELSE
        CALL MZCFGG(ICOMP)
        CALL MZSETU(ICOMP)
      ENDIF
      RETURN
      END

c ******************** MZSETU ********************

c MZSETU completes the setting-up of the time-dependent
c coefficients of the difference equations commenced in
c MZCOE1.   The complete equation-set can then be
c solved simultaneously at the current simulation time-step.

      SUBROUTINE MZSETU(ICOMP)
#include "building.h"
#include "geometry.h"
#include "cfd.h"
#include "control.h"

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TC/ITC,ICNT

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PREC14/emarea(MCOM)

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/COE31/HRP(MS,MS),ZHRP(MS),HRF(MS,MS),ZHRF(MS)
      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/COE32J/QTMCA(MS,MN,2)
      COMMON/COE33/QELWP(MS),QELWF(MS)
      COMMON/COE34D/QCASR(2),QCASC(2)
      COMMON/COE35/ZAUGEP,ZAUGIP,ZAUGEF,ZAUGIF,QWCP,XX1,ZGIPM,ZGIFM
      COMMON/COE39/CVIP,CVVP,CVIF,CVVF,QVNP,XX3,CVVPM,CVVFM

      COMMON/PVALA/TPA(MCOM),QPA(MCOM)

      COMMON/CCTL/ICASCF(MCOM)
      COMMON/CONEQN/W(MS,MNM,5)
      COMMON/ZONEQN/E(MEQ,MTR)
      COMMON/ZONEQS/ES(MEQ,MTR),ACAPS

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &ARF(MCOM,MS)

      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &HCOF(MCOM,MS)

      COMMON/SETUQ/QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)

C Common saving coupled zones/surfaces
      common/Zone_Couplings/ bCoupled_Zone (MCOM,MS),
     &                       iCoupled_Zone (MCOM,MS),
     &                       iCoupled_Surf (MCOM,MS)

      logical bCoupled_Zone
      integer iCoupled_Surf, iCoupled_Zone
C Heat losses from plant components embedded in MLC nodes,
C as defined in the current zone
      real fPltHeatLoss_P, fPltHeatLoss_F
C Heat losses from plant components embedded in MLC nodes,
C as defined in adjacent zones
      real fAdjPltHeatLoss_P, fAdjPltHeatLoss_F
      
            
C Special for BCL09: Inject QNOPF at the outermost node IHPNDO of
C construction IHPCON of zone IHPZON as calculated from heat pipe model.
C QNOPF is the neagation of the summation of the present and future
C time-row flux injections as applied to the innermost node to
C which the heat pipe connects.
      COMMON/BCL9/IHPZON,IHPCON,IHPNDO,QNOPF,QHPSV

      COMMON/VTHP15/VCP(MS,MN,2),QCP(MS,MN)
      COMMON/VTHP16/VCF(MS,MN,2),QCF(MS,MN)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D04/GAM,RGAM
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/MOIST16/PATMpa
      COMMON/MOIST53/Ppair(MCOM),Fpair(MCOM),Spair(MCOM)
      COMMON/MOIST70/ACAP
      COMMON/MOIST81/TNC(MS,MN),QNcnst
      COMMON/MOIST82/TNS(MS),QNsrf
      COMMON/MOIST83/TNair,QNair

C      common/evapht/qevapf(mcom,ms,mn),qevapp(mcom,ms,mn)
C Generic nodal flux common - allows a flux to be added or subtracted 
C from a node.
      COMMON/GENFLUX/GENFLXF(MCOM,MS,MN),GENFLXP(MCOM,MS,MN)
      COMMON/AIRWAT/WATERZ(MCOM),WATCAP

C CFD
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/CFDCNV/QCFDF(MCOM,MS),QCFDP(MCOM,MS)

C Drylink variables:
      common/gan/qgan1,qgan2,wgan

C Thermal bridge common block is described in esruprj/edgeo.F
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      character outs*144,LCFD*72
      logical atrace,MSTROK,MSTRZN
      logical WATERZ

C.....Named constnats for timerows
      integer iPresent, iFuture
      parameter (iPresent=1, iFuture=2)

      real zoar  ! local for zonetotsurfacearea(MCOM)

C Check for trace request.
      atrace = .false.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(26).eq.1.AND.IZNTRC(ICOMP).eq.1)atrace = .true.
      if(atrace)then
        write(outs,'(A,I4)')' Subroutine MZSETU   Trace output',ICNT
        call edisp(itu,outs)
        write(outs,'(23X,A,I4)')'Zone',ICOMP
        call edisp(itu,outs)
        ICNT=ICNT+1
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

C For current component determine associated control function.
      IC=ICASCF(ICOMP)
      IF(NCF.NE.0.AND.IC.NE.0)then
        if(IBAN(IC,1).EQ.-2)THEN
          IEQSAV=1
        else
          IEQSAV=0
        endif
      ELSE
        IEQSAV=0
      ENDIF

C Consider each construction in turn.
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
         IN=1

C Node is situated at 'outside' surface.
C First set adjacent (internal or external) temperature.
         TT1=ATP(ICOMP,I)
         TT2=ATF(ICOMP,I)

C Special for bcl09.
         IF(ICOMP.EQ.IHPZON.AND.I.EQ.IHPCON)THEN
            QHPNOP=QNOPF-QHPSV
            QHPNOF=QHPSV
         ELSE
            QHPNOP=0.
            QHPNOF=0.
         ENDIF

C ADBMOD invokes the adiabatic case if IE(ICOMP,I) has been
C set to -5 in MZADJC.
         IF(IE(ICOMP,I).EQ.-5)THEN
           ADBMOD=1.
         ELSE
           ADBMOD=0.
         ENDIF

C Set-up the future coefficients for 'outside' surface node.
         W(I,IN,1)=0.
         W(I,IN,2)=1.0+VCF(I,IN,1)*GAM*HCOF(ICOMP,I)*(1.-ADBMOD)+
     &             VCF(I,IN,2)*GAM
         W(I,IN,3)=-VCF(I,IN,2)*GAM

C If 'outside' surface node is a plant interaction point
C set coefficient.
         IF(NCF.EQ.0.OR.IC.EQ.0)then
            W(I,IN,4)=0.
            Q=0.
         ELSEIF(IC.GT.0.AND.(IBAN(IC,1).NE.ICOMP.OR.
     &      IBAN(IC,3).LE.0.OR.IBAN(IC,2).NE.I.OR.
     &      IBAN(IC,3).NE.IN))THEN
            W(I,IN,4)=0.
            Q=0.
         ELSE
            W(I,IN,4)=-QCF(I,IN)*GAM

            Q=QNcnst/SNA(ICOMP,I)
         ENDIF

C........Heat loss from plant components embedded in node 
         fPltHeatLoss_P = fNode_embedded_gain (iComp,i,in,iPresent)
     &                     / sna(iComp,i) 
         fPltHeatLoss_F = fNode_embedded_gain (iComp,i,in,iFuture)
     &                     / sna(iComp,i)

C........Get heat loss from plant components defined in
C........adjacent zone 
         if ( bCoupled_Zone( iComp, i ) ) then
          
            iAdjacent_Zone = iCoupled_Zone ( iComp, i)
            iAdjacent_Surf = iCoupled_Surf ( iComp, i) 
            iAdjacent_Node = NNDC(iComp, i) - (in - 1)
            fAdjPltHeatLoss_P = fNode_embedded_gain (iAdjacent_Zone,
     &                                               iAdjacent_Surf,
     &                                               iAdjacent_Node,
     &                                               iPresent )
     &                          / sna(iComp,i) 

            fAdjPltHeatLoss_F = fNode_embedded_gain (iAdjacent_Zone,
     &                                               iAdjacent_Surf,
     &                                               iAdjacent_Node,
     &                                               iFuture)
     &                      / sna(iComp,i)                  
         else

            fAdjPltHeatLoss_P = 0.0
            fAdjPltHeatLoss_F = 0.0

         endif 
            
         
C Set up present coefficient (QHPNOP and QHPNOF relate to special 
C control function BCL09).
         QELWCP=QELWP(I)*(1.-ADBMOD)
         QELWCF=QELWF(I)*(1.-ADBMOD)
         W(I,IN,5)=VCP(I,IN,1)*RGAM*HCOP(ICOMP,I)*TT1+
     &            VCF(I,IN,1)*GAM*HCOF(ICOMP,I)*TT2*(1.-ADBMOD)+
     &            (1.-VCP(I,IN,1)*RGAM*HCOP(ICOMP,I)-VCP(I,IN,2)*
     &            RGAM)*TNC(I,IN)+VCP(I,IN,2)*RGAM*
     &            TNC(I,IN+1)+QCP(I,IN)*RGAM*(Q+QSOLE(I,1)+
     &            QELWCP+QTMCA(I,IN,1)+ARP(ICOMP,I)+QHPNOP
     &            +GENFLXP(ICOMP,I,IN) + fPltHeatLoss_P
     &            + fAdjPltHeatLoss_P )
     &            +QCF(I,IN)*GAM*(QSOLE(I,2)
     &            +QELWCF+QTMCA(I,IN,2)+ARF(ICOMP,I)+QHPNOF
     &            +GENFLXF(ICOMP,I,IN) + fPltHeatLoss_F
     &            + fAdjPltHeatLoss_F )

             
C Continue with remaining nodes within construction
         DO 20 IN=2,NNDC(ICOMP,I)-1

C Node is within multilayered construction: set present temperature
C of the two adjacent nodes in thermal contact with currently
C considered node.
            TT1=TNC(I,IN-1)
            IF(IN.EQ.NNDC(ICOMP,I)-1)THEN
              TT2=TNS(I)
            ELSE
              TT2=TNC(I,IN+1)
            ENDIF

C Complete the future coefficient set-up for this nodal equation.
            W(I,IN,1)=-VCF(I,IN,1)*GAM
            W(I,IN,2)=1.0+VCF(I,IN,1)*GAM+VCF(I,IN,2)*GAM
            W(I,IN,3)=-VCF(I,IN,2)*GAM

C If current node is a plant interaction point set
C coefficient.
            IF(NCF.EQ.0.OR.IC.EQ.0)then
               W(I,IN,4)=0.
               Q=0.
            ELSEIF(IC.GT.0.AND.(IBAN(IC,1).NE.ICOMP.OR.
     &         IBAN(IC,3).LE.0.OR.IBAN(IC,2).NE.I.OR.
     &         IBAN(IC,3).NE.IN))THEN
               W(I,IN,4)=0.
               Q=0.
            ELSE
               W(I,IN,4)=-QCF(I,IN)*GAM
               Q=QNcnst/SNA(ICOMP,I)
            ENDIF

C...........Get plant heat injection embedded into node            
            fPltHeatLoss_P = fNode_embedded_gain (iComp,i,in,iPresent)
     &                     / sna(iComp,i) 
            fPltHeatLoss_F = fNode_embedded_gain (iComp,i,in,iFuture)
     &                     / sna(iComp,i)
C........Get heat loss from plant components defined in
C........adjacent zone 
            if ( bCoupled_Zone( iComp, i ) ) then

               iAdjacent_Zone = iCoupled_Zone ( iComp, i)
               iAdjacent_Surf = iCoupled_Surf ( iComp, i) 
               iAdjacent_Node = NNDC(iComp, i) - (in - 1)
               
               fAdjPltHeatLoss_P = fNode_embedded_gain (iAdjacent_Zone,
     &                                                  iAdjacent_Surf,
     &                                                  iAdjacent_Node,
     &                                                  iPresent )
     &                             / sna(iComp,i) 

               fAdjPltHeatLoss_F = fNode_embedded_gain (iAdjacent_Zone,
     &                                                  iAdjacent_Surf,
     &                                                  iAdjacent_Node,
     &                                                  iFuture)
     &                         / sna(iComp,i)                  
            else
               fAdjPltHeatLoss_P = 0.0
               fAdjPltHeatLoss_F = 0.0
            endif             

C Set present coefficient.
            W(I,IN,5)=VCP(I,IN,1)*RGAM*TT1+(1.-VCP(I,IN,1)*
     &               RGAM-VCP(I,IN,2)*RGAM)*TNC(I,IN)+
     &               VCP(I,IN,2)*RGAM*TT2+QCP(I,IN)*RGAM*
     &               (Q+QTMCA(I,IN,1)+GENFLXP(ICOMP,I,IN)
     &                 + fPltHeatLoss_P + fAdjPltHeatLoss_P )
     &               +QCF(I,IN)*GAM*(QTMCA(I,IN,2)+GENFLXF(ICOMP,I,IN)
     &                 + fPltHeatLoss_F + fAdjPltHeatLoss_F )
   20    CONTINUE

C Node is situated at an inside surface: only partially
C set-up the 'W' coefficient set.
         IN=NNDC(ICOMP,I)
         W(I,IN,1)=-VCF(I,IN,1)*GAM
         W(I,IN,2)=0.
         W(I,IN,3)=0.
         W(I,IN,4)=0.
         W(I,IN,5)=0.

   10 CONTINUE

C Now set-up the coefficients of the internal surface nodal
C equations.
      N1=NC+1
      N2=NC+2
      N3=NC+3
      N4=NC+4

C E(I,1) to E(I,NC) excluding E(I,I) hold the radiation exchange
C coefficients; E(I,I) holds the surface node coefficient;
C E(I,N1) is the convection term; E(I,N2) is the plant term;
C E(I,N3) is the present term and E(I,N4) is the next-to surface
C node term.
      DO 30 I=1,NC
      NN=NNDC(ICOMP,I)
      SUM=0.
      DO 40 J=1,NC
      IF(J.EQ.I)THEN

C Surface node coefficient.
         E(I,J)=1.0+VCF(I,NN,1)*GAM+VCF(I,NN,2)*GAM*HCIF(ICOMP,I)+
     &   QCF(I,NN)*GAM*ZHRF(I)

C If CFD is present do not considere convection coefficients.
      IF((IFCFD(ICOMP).EQ.2.AND.NSINC.GT.1).OR.
     & (IFCFD(ICOMP).EQ.-2.AND.IDYP.GE.ISDS))E(I,J)=1.0+
     &                            VCF(I,NN,1)*GAM+QCF(I,NN)*GAM*ZHRF(I)
         W(I,NN,2)=E(I,J)
      ELSE

C Set-up radiation coefficient from J to I.
         SUM=SUM+HRP(J,I)*TNS(J)
         E(I,J)=-QCF(I,NN)*GAM*HRF(J,I)
      ENDIF
   40 CONTINUE
      E(I,N1)=-VCF(I,NN,2)*GAM*HCIF(ICOMP,I)

C if cfd model is considered do not take into account convection terms.
      IF((IFCFD(ICOMP).EQ.2.AND.NSINC.GT.1).OR.
     & (IFCFD(ICOMP).EQ.-2.AND.IDYP.GE.ISDS))E(I,N1)=-VCF(I,NN,2)*GAM

C If inside surface node is a plant interaction point
C set coefficient.
      IF(NCF.EQ.0.OR.IC.EQ.0)then
         Q=0.
         E(I,N2)=0.
      ELSEIF(IC.GT.0.AND.(IBAN(IC,1).NE.ICOMP.OR.
     &   IBAN(IC,2).LE.0.OR.IBAN(IC,3).GT.0.OR.
     &   IBAN(IC,2).NE.I))THEN
         Q=0.
         E(I,N2)=0.
      ELSE
         E(I,N2)=-QCF(I,NN)*GAM
         Q=QNsrf/SNA(ICOMP,I)
      ENDIF

C  For radiant casual gains to take surface emissivity into account.
C  Also append gains associated from plant containinment heat losses
C  ( use function fSurface_radiant_gain to obtain radiant gains at
C    surface assocaited with plant component containment heat losses
C   (in watts!) )
      zoar=zonetotsurfacearea(icomp)  ! assign local
      QCASRP=qcasr(1)*zoar*ei(icomp,i)/emarea(icomp)
     &          + fSurface_radiant_gain(iComp,i,iPresent) / sna(iComp,i)
      QCASRF=qcasr(2)*zoar*ei(icomp,i)/emarea(icomp)
     &          + fSurface_radiant_gain(iComp,i,iFuture) / sna(iComp,i)

C For radiant plant find __ in W/m2 .
      PADDR=QPLTP(ICOMP)*(1.-CONV(ICOMP))*ei(icomp,i)/emarea(icomp)
C Trace...
      if(atrace)then
        write(outs,*)'Surf casual rad: QCASRP qcasr(1) QCASRF qcasr(2)',
     &                I,QCASRP,qcasr(1),QCASRF,qcasr(2)
        call edisp(itu,outs)
        write(outs,*)'Surf plant rad: PADDR QPLTP() radiant_frac ',
     &                I,PADDR,QPLTP(ICOMP),1.-CONV(ICOMP)
        call edisp(itu,outs)
      endif


C.....Get heat loss from plant components embedded in nodes
      fPltHeatLoss_P = fNode_embedded_gain (iComp,i,nn,iPresent)
     &                     / sna(iComp,i) 
      fPltHeatLoss_F = fNode_embedded_gain (iComp,i,nn,iFuture)
     &                     / sna(iComp,i)
C.....Get heat loss from plant components defined in
C.....adjacent zone
      if ( bCoupled_Zone( iComp, i ) ) then

         iAdjacent_Zone = iCoupled_Zone ( iComp, i)
         iAdjacent_Surf = iCoupled_Surf ( iComp, i)
         iAdjacent_Node = NNDC(iComp, i) - (nn - 1)
         fAdjPltHeatLoss_P = fNode_embedded_gain (iAdjacent_Zone,
     &                                            iAdjacent_Surf,
     &                                            iAdjacent_Node,
     &                                            iPresent )
     &                       / sna(iComp,i)

         fAdjPltHeatLoss_F = fNode_embedded_gain (iAdjacent_Zone,
     &                                            iAdjacent_Surf,
     &                                            iAdjacent_Node,
     &                                            iFuture)
     &                   / sna(iComp,i)
      else

         fAdjPltHeatLoss_P = 0.0
         fAdjPltHeatLoss_F = 0.0

      endif
         
C Set-up present coefficient .
      E(I,N3)=VCP(I,NN,1)*RGAM*TNC(I,NN-1)+
     &(1.-VCP(I,NN,1)*RGAM-VCP(I,NN,2)*RGAM*HCIP(ICOMP,I)-
     &QCP(I,NN)*RGAM*ZHRP(I))*TNS(I)+VCP(I,NN,2)*RGAM*
     &HCIP(ICOMP,I)*TNair+QCP(I,NN)*RGAM*(Q+SUM+QSOLI(I,1)+
     &QTMCA(I,NN,1)+QCASRP+PADDR+GENFLXP(ICOMP,I,NN)
     &  + fPltHeatLoss_P + fAdjPltHeatLoss_F)+
     &QCF(I,NN)*GAM*(QSOLI(I,2)+QTMCA(I,NN,2)+QCASRF
     &+GENFLXF(ICOMP,I,NN)
     &  + fPltHeatLoss_F +fAdjPltHeatLoss_F)

C if cfd model is considered do not take into account convection terms
C and add CFD present values.
      IF((IFCFD(ICOMP).EQ.2.AND.NSINC.GT.1).OR.
     &    (IFCFD(ICOMP).EQ.-2.AND.IDYP.GE.ISDS))
     &                   E(I,N3)=VCP(I,NN,1)*RGAM*
     &                   TNC(I,NN-1)+(1.-VCP(I,NN,1)*RGAM-
     &                   QCP(I,NN)*RGAM*ZHRP(I))*TNS(I)+
     &                   QCP(I,NN)*RGAM*(Q+SUM+QSOLI(I,1)+
     &                   QTMCA(I,NN,1)+QCASRP+PADDR+QCFDP(ICOMP,I)+
     &                   GENFLXP(ICOMP,I,NN) 
     &                     + fPltHeatLoss_P +fAdjPltHeatLoss_P)
     &                   +QCF(I,NN)*GAM*(QSOLI(I,2)
     &                   +QTMCA(I,NN,2)+QCASRF+GENFLXF(ICOMP,I,NN)
     &                     + fPltHeatLoss_F +fAdjPltHeatLoss_F)
C Trace...
      if(atrace)then
        write(outs,*)'Resulting E(I,N3) I N3  coef: ',E(I,N3),I,N3
        call edisp(itu,outs)
      endif

C Next-to-surface future coefficient.
      E(I,N4)=-VCF(I,NN,1)*GAM
   30 CONTINUE

C Set-up air point equation coefficients.
      SUM1=0.
      SUM2=0.
      SUM3=0.

C E(N1,1) to E(N1,NC) hold the convection terms;
C E(N1,N1) holds the air node coefficient;
C E(N1,N2) is the plant term and E(N1,N3) is the
C present term.
      IF(MSTRZN(ICOMP))THEN
        TC=TPA(ICOMP)
        CPa=CPair01(TC)
        CPv=CPvpr01(TC)
        CPl=SHTH2O(TC)*1000.0
        RHp=Ppair(ICOMP)/Psat01(TC)
        Upair=X03(TC,RHp,PATMpa)
        RHOp=RHOdry01(TC)
        ACAPp=RHOp*VOL(ICOMP)*(CPa+CPv*Upair)+CPl*Fpair(ICOMP)
        ACAP=3600.0/(NTSTEP*ACAPp)
      ELSE
        ACAP=1.0/(NTSTEP*0.33*VOL(ICOMP))
      ENDIF

C Replace ACAP if it is a water-filled (or other fluid) zone.
      IF(WATERZ(ICOMP))ACAP=1.0/(NTSTEP*WATCAP*1000.0*VOL(ICOMP)/3600.0)

      DO 50 I=1,NC

C Convection terms.
      E(N1,I)=-HCIF(ICOMP,I)*GAM*SNA(ICOMP,I)*ACAP
      SUM1=SUM1-E(N1,I)
      XX=HCIP(ICOMP,I)*SNA(ICOMP,I)*ACAP
      SUM2=SUM2+XX*TNS(I)
      SUM3=SUM3+XX
   50 CONTINUE

C Air node term.
C First set thermal bridge term: tbrig (W/K).
      tbrig=totheatloss(icomp)      
      E(N1,N1)=1.0+SUM1+(ZAUGEF+ZAUGIF+CVIF+CVVF+tbrig)*GAM*ACAP

C If air node is plant interaction point set coefficient.
      IF(NCF.EQ.0.OR.IC.EQ.0)then
         E(N1,N2)=0.
      ELSEIF(IC.GT.0.AND.
     & ((IBAN(IC,1).NE.ICOMP.AND.IBAN(IC,1).GT.0).OR.
     &  (IBAN(IC,1).NE.-2.AND.IBAN(IC,2).GT.0).OR.
     &  (IBAN(IC,1).EQ.-2.AND.IBAN(IC,2).NE.ICOMP)))THEN
         E(N1,N2)=0.
      ELSE
         E(N1,N2)=-ACAP*GAM
      ENDIF

C Get convective plant input.
      PADDC=QPLTP(ICOMP)*CONV(ICOMP)

C Calculate thermal bridge term.
      qtbridge=ACAP*RGAM*tbrig*(TP-TFA(icomp))+ACAP*GAM*tbrig*TF

C Set-up present coefficient.
      E(N1,N3)=SUM2*RGAM+(1.-SUM3*RGAM)*TNair+
     &ACAP*RGAM*(QNair+QCASC(1)+QWCP+
     &QVNP+PADDC)+(XX1+XX3+QCASC(2))*ACAP*GAM
     &+qtbridge

      IGANg=0
      if(IGANg.eq.1)then
C Drylink mod start.
         if(icomp.lt.5)then
           call drylink(icomp,qganp1,qganf1,qganp2,qganf2,wganp,wganf)
         endif
         if(icomp.eq.1)then
           qgan=-(qganp1+qganf1)*0.5
           E(N1,N3)=E(N1,N3)+qgan*ACAP*GAM
         elseif(icomp.eq.2)then
           qgan=(qganp2+qganf2)*0.5
           E(N1,N3)=E(N1,N3)+qgan*ACAP*GAM
         elseif(icomp.eq.3)then
           qgan=-(qganp1+qganf1)*0.5
           E(N1,N3)=E(N1,N3)+qgan*ACAP*GAM
         elseif(icomp.eq.4)then
           qgan=(qganp2+qganf2)*0.5
           E(N1,N3)=E(N1,N3)+qgan*ACAP*GAM
         endif
C Drylink mod finish.
      endif

C Trace...
      if(atrace)then
        write(outs,*)'present coef E(N1,N3) PADDC',E(N1,N3),PADDC
        call edisp(itu,outs)
      endif

C *** NB ***
C Where:
C         XX1 = the sum of the products of window areas, U-values
C               and 'OUTSIDE' temperature at the future time-row.
C         XX3 = the sum of the product of ventilation conductances
C               and 'OUTSIDE' temperature at the future time-row.

      IF(IEQSAV.EQ.1)THEN
        ACAPS=ACAP
      ENDIF

C Trace output ?
      IF(atrace)then
        write(outs,*)' Air capacity coefficient = ',ACAP
        call edisp(itu,outs)

C Output difference equation coefficients.
        call edisp(itu,' ')
        call edisp(itu,' Construction node coefficients follow.')
        DO 9993 I=1,NC
          NN=NNDC(ICOMP,I)
          call edisp(itu,' ')
          write(outs,'(A,I3,A,I3)')' Construction',I,' No.of nodes',NN
          call edisp(itu,outs)
          call edisp(itu,
     &  ' Node Coupling 1 Coupling 2 Coupling 3 Coupling 4 Coupling 5')
          DO 9991 J=1,NN
            write(outs,'(I4,F10.3,4F11.3)')J,(W(I,J,K),K=1,5)
            call edisp(itu,outs)
 9991     CONTINUE
 9993   CONTINUE
        call edisp(itu,
     &  ' Zone surface and air node coefficients follow')
        DO 9988 I=1,N1
          write(outs,'(A,I3,A,I3,A,I3,A)')' Surface',I,' of',N1,
     &               ' equations and ',N4,' Terms:'
          call edisp(itu,outs)

C << MS sensitive >>
          if(N4.le.12)then
            write(outs,'(12F9.2)')(E(I,J),J=1,N4)
            call edisp(itu,outs)
          else
            write(outs,'(12F9.2)')(E(I,J),J=1,12)
            call edisp(itu,outs)
            write(outs,'(12F9.2)')(E(I,J),J=13,N4)
            call edisp(itu,outs)
          endif
 9988   CONTINUE
      endif
      RETURN
      END

C ****************************  LUMPED  *******************************
C LUMPED creates the coefficients for lumped constructions.
C *********************************************************************
      SUBROUTINE LUMPED(ICOMP,I)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)

      DIMENSION CAP(4),R(3),THKX(4)

      NGP=NGAPS(ICOMP,I)
      THKC=0.

C Calculate the total thickness for this construction.
      DO 10 J=1,NELTS(ICOMP,I)
         THKC=THKC+THRMLI(ICOMP,I,J,4)
   10 CONTINUE

C Define the three control volumes.
      THKX(1)=THKC/4.
      THKX(2)=THKC/2.
      THKX(3)=THKC-THKX(1)-THKX(2)
      THK=0.

C For the first control volume.
      L=1
      M=1

C Initialise the capacity of the first control volume.
      CAP(L)=0.

C Calculate the capacity of the three control volumes.
      DO 20 J=1,NELTS(ICOMP,I)
         THKD=THRMLI(ICOMP,I,J,4)
         THK=THK+THKD
   30    IF(ABS(THK-THKX(L)).LT.0.000001)THEN
            IF(M.LE.NGP.AND.NPGAP(ICOMP,I,M).EQ.J)THEN
               CAP(L)=CAP(L)+1.3*1005.5*THKD
               M=M+1
            ELSE
               CAP(L)=CAP(L)+THRMLI(ICOMP,I,J,2)*
     &                THRMLI(ICOMP,I,J,3)*THKD
            ENDIF
            L=L+1
            CAP(L)=0.
            THK=0.
         ELSEIF(THK.LT.THKX(L))THEN
            IF(M.LE.NGP.AND.NPGAP(ICOMP,I,M).EQ.J)THEN
               CAP(L)=CAP(L)+1.3*1005.5*THKD
               M=M+1
            ELSE
               CAP(L)=CAP(L)+THRMLI(ICOMP,I,J,2)*
     &                THRMLI(ICOMP,I,J,3)*THKD
            ENDIF
         ELSE
            THKR=THKD-(THK-THKX(L))
            IF(M.LE.NGP.AND.NPGAP(ICOMP,I,M).EQ.J)THEN
               CAP(L)=CAP(L)+1.3*1005.5*THKR
            ELSE
               CAP(L)=CAP(L)+THRMLI(ICOMP,I,J,2)*
     &                THRMLI(ICOMP,I,J,3)*THKR
            ENDIF
            THK=THK-THKX(L)
            THKD=THK
            L=L+1
            CAP(L)=0.
            IF(THK.GT.0.00001)GOTO 30
         ENDIF
   20 CONTINUE
      DO 40 IC=1,3
         CAP(IC)=CAP(IC)/THKX(IC)
   40 CONTINUE

C Define the distance between each two successive nodes.
      THKX(1)=THKC/2.
      THKX(2)=THKC-THKX(1)
      THK=0.
      M=1
      L=1

C Initialise the resistivity between the external and the middle nodes.
      R(L)=0.

C calculate the thermal resistivity between each two successive nodes.
      DO 50 J=1,NELTS(ICOMP,I)
         THKD=THRMLI(ICOMP,I,J,4)
         THK=THK+THKD
   60    IF(ABS(THK-THKX(L)).LT.0.000001)THEN
            IF(M.LE.NGP.AND.NPGAP(ICOMP,I,M).EQ.J)THEN
               R(L)=R(L)+RGAPS(ICOMP,I,M)*THKD/THRMLI(ICOMP,I,J,4)
               M=M+1
            ELSE
               R(L)=R(L)+THKD/THRMLI(ICOMP,I,J,1)
            ENDIF
            L=L+1   
            R(L)=0.
            THK=0.
         ELSEIF(THK.LT.THKX(L))THEN
            IF(M.LE.NGP.AND.NPGAP(ICOMP,I,M).EQ.J)THEN
               R(L)=R(L)+RGAPS(ICOMP,I,M)*THKD/THRMLI(ICOMP,I,J,4)
               M=M+1
            ELSE
               R(L)=R(L)+THKD/THRMLI(ICOMP,I,J,1)
            ENDIF
         ELSE
            THKR=THKD-(THK-THKX(L))
            IF(M.LE.NGP.AND.NPGAP(ICOMP,I,M).EQ.J)THEN
               R(L)=R(L)+RGAPS(ICOMP,I,M)*THKR/THRMLI(ICOMP,I,J,4)
            ELSE
               R(L)=R(L)+THKR/THRMLI(ICOMP,I,J,1)
            ENDIF
            THK=THK-THKX(L)
            THKD=THK
            L=L+1  
            R(L)=0.
            IF(THK.GT.0.00001)GOTO 60
         ENDIF
   50 CONTINUE

C Calculate the nodal coefficients for the three nodes representing the 
C lumped construction.
      DT=3600.
      IN=1
         C(ICOMP,I,IN,1)=DT/((THKC/4.)*CAP(1))
         C(ICOMP,I,IN,2)=DT/(R(1)*(THKC/4.)*CAP(1))
         QC(ICOMP,I,IN)=DT/((THKC/4.)*CAP(1))
      IN=2
         C(ICOMP,I,IN,1)=DT/(R(1)*(THKC/2.)*CAP(2))
         C(ICOMP,I,IN,2)=DT/(R(2)*(THKC/2.)*CAP(2))
         QC(ICOMP,I,IN)=DT/((THKC/2.)*CAP(2))
      IN=3
         C(ICOMP,I,IN,1)=DT/(R(2)*(THKC/4.)*CAP(3))
         C(ICOMP,I,IN,2)=DT/((THKC/4.)*CAP(3))
         QC(ICOMP,I,IN)=DT/((THKC/4.)*CAP(3))
      RETURN
      END
