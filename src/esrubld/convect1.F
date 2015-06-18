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

C This file contains the routines for calculating surface convection.

C     MZCONV  Controlling routine for calculating surface convection.
C     HTOVER  Overrides the calculated HC values with the user-prescribed values.
C     HTOVERCEN overrides the calculated HTC values with the user-prescribed values
C       specified in the .htc file which conform to CEN standards.
C     INTERR  Interrogates configuration and selects surface-convection approach.
C     SELCOR2 Selects convection approach for `type 2' convection control.
C     SELCOR3 Selects convection approach for `type 3' convection control.
C     OUTISIN Sets HCs for `outside' surfaces which are `interior' to building.
C     FSCALE  Calculates scaling factor to convert forced-flow coefficients.
C     SELFORC Selects from amongst forced-flow correlations.
C     QPLANT  Calculates total plant injection/extraction for current time step.
C     QZONE   Calculates heat injected/extracted to zone by air-handling system.


C ******************** MZCONV ********************
       SUBROUTINE MZCONV

#include "building.h"
#include "geometry.h"
#include "model.h"
#include "cfd.h"
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TC/ITC,ICNT
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &            ARF(MCOM,MS)
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      COMMON/CONVS/HCI(MCOM,MS),HCO(MCOM,MS)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      common/CORIND/ICORI(MCOM,MS),ICORE(MCOM,MS)
      COMMON/HCFIX2/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &              HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)
      common/HCTL/iCTLTP(MCOM,MBP),IcIuse(MCOM,MS,MBP),
     &            IcEuse(MCOM,MS,MBP),icnvKY(MCOM,MS,MBP),
     &            CVdat(MS*MBP,8)
      COMMON/PREC1I/APRAT(MCOM,MS),HEIGHT(MCOM,MS)
      common/ts4/itsitr(msch),itrpas
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT
      
C Indicies of simulation toggles for inside and outside hc coef.
      common/hcthry/ihct,icorexhct

      COMMON/calcdCFD/mICORcfd(MCOM),mHCcfd(MCOM)

      character outs*124,LCFD*72

C Iterate through each zone.
      DO 100 ICOMP=1,NCOMP

C Interrogate the configuration to decide which surface-convection approaches to
C apply for each `interior' building surface of the zone.
C This may have already been done if CFD was invoked, check this first.
        IF (mICORcfd(ICOMP).EQ.0) THEN
          CALL INTERR(ICOMP)
        ENDIF

C Iterate through each multi-layer construction.
        NC=NCONST(ICOMP)
        DO 101 ISUR=1,NC

C Set present time-row values.
          if(NSINC.EQ.1) then
            HCIP(ICOMP,ISUR)=1.0
            HCOP(ICOMP,ISUR)=1.0
          else

C If iterating at this time step then do not update present 
C coefficients after the first pass through this routine.
            if (ITRPAS.eq.0) then
              HCIP(ICOMP,ISUR)=HCI(ICOMP,ISUR)
              HCOP(ICOMP,ISUR)=HCO(ICOMP,ISUR)
            endif
          endif

C Calculate the HTC for `inside' surface of the mlc using empirical correlations.
C---------Set air & surface temperatures and temperature difference.
          TA=TFA(ICOMP)
          TS=TFS(ICOMP,ISUR)
          IF(NSINC.EQ.1)TS=0.0
          DT=ABS(TA-TS)
C---------Calculate the HTC using one of the empirical correlations.
          ICOR = ICORI(ICOMP,ISUR)
          HGT = HEIGHT(ICOMP,ISUR)
          ARHY = APRAT(ICOMP,ISUR)
          CALL HTBUOY(HC,ICOR,ICOMP,ISUR,DT,HGT,ARHY)
          HCIF(ICOMP,ISUR)=HC
C---------Decide whether to use the HTC calculated by the empirical correlation
C---------or the HTC calculated by CFD.
          IF(mHCcfd(ICOMP).EQ.1) CALL ASSESS(ICOMP,ISUR)

C Calculate the HTC for `outside' surface of the mlc using empirical correlations.
C The `outside' surface of the mlc may face the exterior environment, the ground,
C the air in an adjoining zone, or it may have an `internal/same',
C `internal/specified constant', or `adiabatic' boundary condition.

C `Outside' surface faces the exterior environment. If icorexhct is
C one then use the standard correlation, if icorexhct is two then
C use the MoWiTT correlation.
          IF( IE(ICOMP,ISUR).eq.0 ) THEN
            icor = icorexhct
            CALL HTCEXT(icor,HC,ICOMP,ISUR)
            HCOF(ICOMP,ISUR)=HC

C `Outside' surface is coupled to the ground.
          ELSEIF( IE(ICOMP,ISUR).EQ.-4 ) THEN
            HCOF(ICOMP,ISUR)=1000.

C `Outside' surface faces the air point of an adjacent zone.
C No calculation is performed in this case: in OUTISIN the convection
C coefficient calculated for the adjacent zone's `inside' surface will
C be assigned to this `outside' surface.
          ELSEIF( IE(ICOMP,ISUR).GT.0 ) THEN

C `Outside' surface is a BASESIMP foundation.
          ELSEIF( IE(ICOMP,ISUR).eq.-6 ) THEN
            call BSCONV(ICOMP,ISUR)

C `Outside' surface is `internal/same', `internal/specified constant',
C or `adiabatic'.
          ELSE
            TA=ATF(ICOMP,ISUR)
            TS=TFC(ICOMP,ISUR,1)
            IF(NSINC.EQ.1)TS=0.0
            DT=ABS(TA-TS)
            ICOR = ICORE(ICOMP,ISUR)
            HGT = HEIGHT(ICOMP,ISUR)
            ARHY = APRAT(ICOMP,ISUR)
            CALL HTBUOY(HC,ICOR,ICOMP,ISUR,DT,HGT,ARHY)
            HCOF(ICOMP,ISUR)=HC
          ENDIF

C Consider next mlc in zone.
  101   CONTINUE

C Override the calculated values if user has specified a convection file
C with fixed coefficients (ie. `type 1' control over convection calculations
C or type 4 CEN specified hc).
C-------Does a convection file exist?
        IF( IHC(ICOMP).EQ.1 ) THEN
C---------Has the user specified fixed coefficients in the convection file
C---------for any of the defined periods? 
          Ltype1 = 0
          DO 22 k=1,NHCFP(ICOMP)
            if( iCTLTP(ICOMP,k).eq.1 ) Ltype1=1
            if( iCTLTP(ICOMP,k).eq.4 ) Ltype1=4
   22     CONTINUE
          IF(Ltype1.eq.1) CALL HTOVER(ICOMP)
          IF(Ltype1.eq.4) CALL HTOVERCEN(ICOMP)
        ENDIF

C Consider next zone.
  100 CONTINUE

C Set the convection coefficients for the `outside' surfaces that are
C `interior' building surfaces.
      call OUTISIN

C Save future time-row values for use as present values at next time step.
      DO 20 ICOMP=1,NCOMP
        NC=NCONST(ICOMP)
        DO 21 ISUR=1,NC
          HCI(ICOMP,ISUR)=HCIF(ICOMP,ISUR)
          HCO(ICOMP,ISUR)=HCOF(ICOMP,ISUR)
   21   CONTINUE
   20 CONTINUE

C Trace output ?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999
      IF(ITRACE(6).EQ.0.OR.NSINC.GT.ITCF)goto  9999
      write(outs,'(A,I4)')' Subroutine MZCONV     Trace output',ICNT
      call edisp(itu,outs)
      ICNT=ICNT+1
      CALL DAYCLK(IDYP,BTIMEF,ITU)

C Output computed convection coefficients.
      DO 9994 ICOMP=1,NCOMP
        IF(IZNTRC(ICOMP).NE.1)goto 9994
        call edisp(itu,' ')
        write(outs,'(A,I3)')' Zone',ICOMP

C Vertical Bouyancy. IAORZ zero = outside temperature.
C CWIDTH is the width (m) of ventilated channel.
C CHEIGHT is the height of ventilated channel.
        call edisp(itu,outs)
        write(outs,'(A,I3,F5.2,F5.2)')'IAORZ,CWIDTH,CHEIGHT',
     &    IAORZ,CWIDTH,CHEIGHT
        call edisp(itu,outs)

        call edisp(itu,' Surface convection coefficients')
        call edisp(itu,
     &    ' Surface   Index ICOR Inside          `Outside`')
        call edisp(itu,
     &    '                      Present Future   Present Future')
        NC=NCONST(ICOMP)
        DO 9992 I=1,NC
          write(outs,9991)SNAME(ICOMP,I),IE(ICOMP,I),ICORI(ICOMP,I),
     &      HCIP(ICOMP,I),HCIF(ICOMP,I),HCOP(ICOMP,I),HCOF(ICOMP,I)
 9991     format(1x,a12,I3,I3,F8.2,F8.2,F8.2,F8.2)
          call edisp(itu,outs)
 9992   CONTINUE
 9994 CONTINUE
 9999 RETURN
      END

C ******************** HTOVER ********************
C HTOVER overrides the calculated HTC values with the user-prescribed values
C specified in the .htc file.

      SUBROUTINE HTOVER(ICOMP)

#include "building.h"
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/HCFIX2/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &              HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      common/HCTL/iCTLTP(MCOM,MBP),IcIuse(MCOM,MS,MBP),
     &            IcEuse(MCOM,MS,MBP),icnvKY(MCOM,MS,MBP),
     &            CVdat(MS*MBP,8)

C Check whether we are within one of the time intervals with type 1 control.
      TIME=BTIMEF
      NP=NHCFP(ICOMP)
      DO 30 I=1,NP
        IP=I
        IF( TIME.LE.HCFPFT(ICOMP,I) .AND. TIME.GT.HCFPST(ICOMP,I) )then
          if( iCTLTP(ICOMP,I).eq.1 ) goto 32
        ENDIF
   30 CONTINUE
      return

   32 NC=NCONST(ICOMP)
      DO 40 ISUR=1,NC

C Reset internal surface coefficents.
        IF(HCFVI(ICOMP,ISUR,IP).GE.0.0)THEN

C +ve, replace by given coefficient.
          HCIF(ICOMP,ISUR)=HCFVI(ICOMP,ISUR,IP)

C -1, do not adjust.
        ELSEIF(ABS(HCFVI(ICOMP,ISUR,IP)+1.).LT.0.00001)then
          goto 321

C =-99, add 5.7 to calculated value.
        ELSEIF(ABS(HCFVI(ICOMP,ISUR,IP)+99.).LT.0.00001)then
          HCIF(ICOMP,ISUR)=HCIF(ICOMP,ISUR)+5.7
        ENDIF

C Reset "outside" surface coefficents.
  321   IF(HCFVE(ICOMP,ISUR,IP).GE.0.0)THEN

C +ve, replace by given coefficient.
          HCOF(ICOMP,ISUR)=HCFVE(ICOMP,ISUR,IP)

C -1, do not adjust.
        ELSEIF(ABS(HCFVE(ICOMP,ISUR,IP)+1.).LT.0.00001)then
          goto 40

C =-99, add 5.7 to calculated value.
        ELSEIF(ABS(HCFVE(ICOMP,ISUR,IP)+99.).LT.0.00001)then
          HCOF(ICOMP,ISUR)=HCOF(ICOMP,ISUR)+5.7
        ENDIF

   40 CONTINUE

      RETURN
      END

C ******************** HTOVERCEN ********************
C HTOVERCEN overrides the calculated HTC values with the user-prescribed values
C specified in the .htc file which conform to CEN standards specified up and down
C hc coef for ceilings/floors and wall surfaces.

C The approach is based on defining hc regimes by topology rather than one for
C each surface. The interface typically writes standard values to this file
C but the file can hold alternative values.
      SUBROUTINE HTOVERCEN(ICOMP)

#include "building.h"
#include "geometry.h"
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TC/ITC,ICNT
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/HCFIX2/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &              HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &            ARF(MCOM,MS)
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      common/HCTL/iCTLTP(MCOM,MBP),IcIuse(MCOM,MS,MBP),
     &            IcEuse(MCOM,MS,MBP),icnvKY(MCOM,MS,MBP),
     &            CVdat(MS*MBP,8)

C Commons to support CEN upwards or downwards heat flow hc coef.
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)

C   CENLOC(i,k,jj) hc values for zone 'i', time interval 'k':
C       jj=1 index of a floor surface
C       jj=2 index of a ceiling surface
C       jj=3-5 floor upwd_inside, downwd_inside, external
C       jj=6-8 ceiling upwd_inside, downwd_inside, external
C       jj=9-11 sloped upwd_inside, downwd_inside, external
C       jj=12-14 wall inside, partition, external
      COMMON/HCCEN2/CENLOC(MCOM,MBP,14)

C Logical variables for close to floor, ceiling, wall, other for CEN tests.
      logical isfloor,isceiling,iswall,isother
      logical dotrace
      character outs*124

C Check whether we are within one of the time intervals with type 4 control.
      TIME=BTIMEF
      NP=NHCFP(ICOMP)
      DO 30 I=1,NP
        IP=I
        IF( TIME.LE.HCFPFT(ICOMP,I) .AND. TIME.GT.HCFPST(ICOMP,I) )then
          if( iCTLTP(ICOMP,I).eq.4 ) goto 32
        ENDIF
   30 CONTINUE
      return

C Trace output. If there is a zone htc file note that data may
C be superceeded by user specifications.
   32 dotrace=.false.
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999
      IF(ITRACE(6).EQ.0.OR.NSINC.GT.ITCF)goto  9999
      write(outs,'(A,I4,a,i2)')
     &  ' Subroutine HTOVERCEN  Trace output',ICNT,
     &  ' for zone ',icomp
      call edisp(itu,outs)
      CALL DAYCLK(IDYP,BTIMEF,ITU)
      dotrace=.true.
 9999 continue

C Check the direction of heat flow for the typical floor surface.
      ifloor = nint(CENLOC(ICOMP,IP,1))
      DTF=TFS(ICOMP,ifloor)-TFA(ICOMP)

C Check the direction of heat flow for the typical ceiling surface.
      iceil= nint(CENLOC(ICOMP,IP,2))
      DTC=TFS(ICOMP,iceil)-TFA(ICOMP)
      if(dotrace)then
        write(outs,'(a,i2,a,F7.3,a,i2,a,F7.3)') 
     &    ' CEN base surface is ',ifloor,' with DTF',DTF,
     &    ' and top surface is ',iceil,' with DTC',DTC
        call edisp(itu,outs)
        call edisp(itu,' Surface convection coefficients')
        call edisp(itu,' Surface    Inside      `Outside`')
        call edisp(itu,'            Future       Future')
      endif

      NC=NCONST(ICOMP)
      DO 40 ISUR=1,NC

C CEN standards are based on surface orientation as well
C as the direction of heat flow. First determine if the surface
C is a floor or ceiling. A 30 degree tollerence assumed e.g. a 
C sloped surface within 30deg of a ceiling is counted as a ceiling.
        isfloor=.false.
        isceiling=.false.
        iswall=.false.
        isother=.false.
        if(PELV(icomp,isur).GE.60.0.AND.
     &     PELV(icomp,isur).LE.90.0)then
          isceiling=.true.
        elseif(PELV(icomp,isur).GE.-90.0.AND.
     &         PELV(icomp,isur).LE.-60.0)then
          isfloor=.true.
        elseif(PELV(icomp,isur).GE.-30.0.AND.
     &         PELV(icomp,isur).LE.30.0) then
          iswall=.true.
        else
          isother=.true.
        endif

C Get the Delta T for the current surface.
        DT=TFS(ICOMP,isur)-TFA(ICOMP)
        if(dotrace)then
          write(itu,*) 'CEN isur,isfloor,isceiling,iswall,isother',
     &      isur,isfloor,isceiling,iswall,isother,' current DT is ',DT
        endif

        if(isfloor)then

C For CEN if a floor check and the floor surface temperature
C is warmer than the air temperature in the zone (DT>zero is upwards heat flow)
          if(DT.gt.0.0)then
            HCIF(ICOMP,ISUR)=CENLOC(ICOMP,IP,3)
          else
            HCIF(ICOMP,ISUR)=CENLOC(ICOMP,IP,4)
          endif
        elseif(isceiling)then

C In case a ceiling then DT > zero is downwards flow.
          if(DT.gt.0.0)then
            HCIF(ICOMP,ISUR)=CENLOC(ICOMP,IP,7)
          else
            HCIF(ICOMP,ISUR)=CENLOC(ICOMP,IP,6)
          endif
        elseif(iswall)then

C Typical wall is hc 2.5 for inside coefficient..
          HCIF(ICOMP,ISUR)=CENLOC(ICOMP,IP,12)
        elseif(isother)then

C In case sloped then DT > zero is downwards flow.
          if(DT.gt.0.0)then
            HCIF(ICOMP,ISUR)=CENLOC(ICOMP,IP,10)
          else
            HCIF(ICOMP,ISUR)=CENLOC(ICOMP,IP,9)
          endif
        endif

C Reset "outside" surface coefficents depending on the topology.
  321   if( IE(ICOMP,ISUR).eq.0 )then
          if(isfloor)then
            HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,5)
          elseif(isceiling)then
            HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,8)
          elseif(iswall)then
            HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,14)
          elseif(isother)then
            HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,11)
          endif
        else

C Does not face the outside so follow the CEN rules.
          if(isfloor)then

C The other side of a floor is assumed to have the same flow direction
C as the typical ceiling in the room so use DTC for this. DTC > zero is downwards flow
            if(DTC.gt.0.0)then
              HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,7)
            else
              HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,6)
            endif
          elseif(isceiling)then

C The other side of a ceiling is assumed to have the same flow direction
C as the typical floor in the room so use DTF for this. DTF>zero is upwards heat flow
            if(DTF.gt.0.0)then
              HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,3)
            else
              HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,4)
            endif
          elseif(isother)then

C The other side of a surface which is not classed as a floor or ceiling or
C a wall is treated as a ceiling.
            if(DTF.gt.0.0)then
              HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,3)
            else
              HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,4)
            endif
          elseif(iswall)then
            HCOF(ICOMP,ISUR)=CENLOC(ICOMP,IP,13)
          endif
        endif
        if(dotrace)then
          write(outs,9991)SNAME(ICOMP,ISUR),
     &      HCIF(ICOMP,ISUR),HCOF(ICOMP,ISUR)
 9991     format(1x,a12,F8.2,F8.2)
          call edisp(itu,outs)
        endif
   40 CONTINUE

      RETURN
      END

C ******************** INTERR ********************
C INTERR interrogates the configuration to decide which surface-convection
C approach to apply for each interior surface of the zone.
      SUBROUTINE INTERR(ICOMP)

#include "building.h"
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      common/CORIND/ICORI(MCOM,MS),ICORE(MCOM,MS)
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &            ARF(MCOM,MS)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C ICORI(ICOMP,ISUR) holds an index for each `inside' surface of the building
C                   indicating the surface convection approach to apply.
C ICORE(ICOMP,ISUR) holds an index for each `outside' surface of the building
C                   indicating the surface convection approach to apply.
C                   Note that an `outside' surface may `interior' to the building.
C                   ICORE(ICOMP,ISUR) is not used when the `outside' surface
C                   faces the exterior environment or the ground.

C Iterate through each mlc of the zone.
      NC=NCONST(ICOMP)
      DO 10 ISUR=1,NC

C Each mlc has an `inside' surface and an `outside' surface.  The `inside' surface
C faces the air node of the zone under consideration.  An empirical correlation
C must be assigned for each `inside' surface.  If the `outside' surface has a
C boundary condition of type `internal/same', `internal/specified constant', or
C `adiabatic', a correlation must be assigned to it as well.  If the `outside'
C surface faces the air node of an adjacent zone, then no correlation assignment
C is made: in coefficient calculated for the adjacent zone's `inside' surface will
C be assigned to this `outside' surface.

C Perform the correlation assignment operation twice for each mlc: first for the
C `inside' surface, then for the `outside' surface.  inRout=1 for `inside' and
C inRout=2 for `outside'.
      DO 20 inRout=1,2

C No need to assign correlation for `outside' surface if it faces the
C exterior environment or the ground.
        if( inRout.eq.2 .and.
     &      (IE(ICOMP,ISUR).eq.0 .or. IE(ICOMP,ISUR).eq.-4) ) goto 20

C No need to assign correlation for `outside' surface if it faces the
C air point of another zone.  This will be handled by subroutine OUTISIN.
        if( inRout.eq.2 .and. IE(ICOMP,ISUR).gt.0 ) goto 20

C Choose the calculation approach based on surface orientation, direction of
C heat flow, and user-specified simulation toggle.
C-------Set air and surface temperatures.
        if(inRout.eq.1)then
          TA=TFA(ICOMP)
          TS=TFS(ICOMP,ISUR)
        else
          if( IE(ICOMP,ISUR).GT.0 )then
C---------`Outside' surface faces the air point of an adjacent zone.
C---------This is now handled in OUTISIN.
          else
C---------`Outside' surface has a boundary condition of type `internal/same',
C---------`internal/specified constant', or `adiabatic'.
            TA=ATF(ICOMP,ISUR)
            TS=TFC(ICOMP,ISUR,1)
          endif
        endif
        IF(NSINC.EQ.1)TS=0.0
C-------Select the correlation via subroutine SELCOR1 in convect2.F.
        call SELCOR1(ICOMP,ISUR,inRout,TA,TS,ICOR)

C A specific approach may have been imposed for this surface through a convection
C file (.htc).  If so, this will override the ICOR value made in SELCOR1.
        call SELCOR2(ICOMP,ISUR,inRout,ICOR)

C `Adaptive' control may have been imposed on the convection calculations
C through a convection file (.htc).  If so, this will override the selection
C made above.
        call SELCOR3(ICOMP,ISUR,inRout,ICOR)

C Assign the correlation index to the appropriate array, depending on whether
C the surface under consideration is `inside' or `outside'.
        if(inRout.eq.1) ICORI(ICOMP,ISUR)=ICOR
        if(inRout.eq.2) ICORE(ICOMP,ISUR)=ICOR

   20 CONTINUE
   10 CONTINUE
      RETURN
      END

C ******************** SELCOR2 ********************
C SELCOR2 selects the convection-calculation approach to use for a given
C surface when `type 2' control over convection calculations is imposed.
C Note: the normal ICOR for this surface will have already been set via
C a call to SELCOR1. Only in the case that the user over-rides it is
C the ICOR value reset and returned as a different value.
      SUBROUTINE SELCOR2(ICOMP,ISUR,inRout,ICOR)

#include "building.h"
#include "model.h"
      COMMON/HCFIX2/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &              HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)
      common/HCTL/iCTLTP(MCOM,MBP),IcIuse(MCOM,MS,MBP),
     &            IcEuse(MCOM,MS,MBP),icnvKY(MCOM,MS,MBP),
     &            CVdat(MS*MBP,8)
      COMMON/BTIME/BTIMEP,BTIMEF

C `Type 2' control allows the user to specify the correlation to use for each
C surface (only applicable for surfaces interior to the building).  This
C could allow, for example, the use of the Alamdari & Hammond correlations
C for a wall, while using one of the Khalifa & Marshall correlations for
C a window.  Time scheduling can be used.  Specific correlations
C can be used for some surfaces while the standard selection (based on
C simulation toggle, surface orientation, and direction of heat flow)
C can be used for other surfaces.

C Has the the user has specified a convection (.htc) file for this zone?
      IF( IHC(ICOMP).EQ.1 ) THEN

C Check each control period for `type 2' control.
        DO 22 k=1,NHCFP(ICOMP)
          if( iCTLTP(ICOMP,k).eq.2 )then

C Are we within the time interval for `type 2'?
            if( BTIMEF.le.HCFPFT(ICOMP,k) .and.
     &          BTIMEF.gt.HCFPST(ICOMP,k)) then

C Use the correlation specified by the user.  If `-1', then do not override
C the choice made based on surface orientation, direction of heat flow, and
C simulation toggle.
              if(inRout.eq.1)then
                if(IcIuse(ICOMP,ISUR,k).ne.-1) ICOR=IcIuse(ICOMP,ISUR,k)
              else
                if(IcEuse(ICOMP,ISUR,k).ne.-1) ICOR=IcEuse(ICOMP,ISUR,k)
              endif
            endif
          endif
   22   CONTINUE
      ENDIF

      RETURN
      END

C ******************** SELCOR3 ********************
C SELCOR3 selects the convection-calculation approach to use for a given
C surface when `type 3' control over convection calculations is imposed.

      SUBROUTINE SELCOR3(ICOMP,ISUR,inRout,ICOR)

#include "building.h"
#include "model.h"
#include "geometry.h"
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/HCFIX2/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &              HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)
      common/HCTL/iCTLTP(MCOM,MBP),IcIuse(MCOM,MS,MBP),
     &            IcEuse(MCOM,MS,MBP),icnvKY(MCOM,MS,MBP),
     &            CVdat(MS*MBP,8)
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/Fisher/Vdot(MCOM),SAT(MCOM),ADJREF(MCOM,MS),DELTAR(MCOM)
      COMMON/adaptHC/HCctl5

      logical ONOFF

C `Type 3' allows adaptive control of the convection coefficient calculations
C (only applicable for surfaces interior to the building).  The user specifies
C a `control law' for each surface, which defines how the calculations will
C adapt.  For example, one correlation may be utilized for a given surface when
C there is no plant interaction in the zone, whereas another correlation will
C be used when there is heat injection.  Time scheduling can be used.  Adaptive
C control can be used for some surfaces while the standard selection (based on
C simulation toggle, surface orientation, and direction of heat flow)
C can be used for other surfaces.

C Firstly, determine whether adaptive control is to be applied...
C ...Has the the user has specified a convection (.htc) file for this zone?
      IF( IHC(ICOMP).EQ.1 ) THEN
C ...Check each control period for `type 3' control.
        DO 24 k=1,NHCFP(ICOMP)
          IF( iCTLTP(ICOMP,k).eq.3 )THEN
C ...Are we within the time interval for `type 3'?
            IF( BTIMEF.le.HCFPFT(ICOMP,k) .and.
     &                                  BTIMEF.gt.HCFPST(ICOMP,k)) THEN

C Adaptive control will be applied. Get index for zone, surface, and time
C interval; use data for `inside' or `outside' surface.
              II = icnvKY(ICOMP,ISUR,k)
              if(inRout.eq.1)jj=1
              if(inRout.eq.2)jj=5

C Now apply the appropriate control law...
              IF( ifix(CVdat(II,jj)).eq.1 ) THEN
C ...Control law 1:
C Toggle between correlations based on state of plant injection
C within zone. User has specified which correlations to use when zone is being
C heated (ON) or is unheated (OFF). If user has specified `-1' for the correlation,
C then accept the correlation chosen by SELCOR1.

C Any convective, radiation, or combined plant input?
                call QPLANT(ICOMP,Qheat)
                if( Qheat.gt.0. ) then
C ON state.
                  if(ifix(CVdat(II,jj+1)).ne.-1) then
                    ICOR = ifix(CVdat(II,jj+1))
                  endif
                else
C OFF state.
                  if(ifix(CVdat(II,jj+2)).ne.-1) then
                    ICOR = ifix(CVdat(II,jj+2))
                  endif
                endif
              ELSEIF( ifix(CVdat(II,jj)).eq.2 .and. inRout.eq.1 ) THEN

C ...Control law 2 (only applicable for `inside' surfaces):
C VAV system with constant-temperature, variable-volume cooling and with
C variable-temperature, CONSTANT-volume heating. When the system is providing
C heating or cooling (ON state), this control law utilizes a forced or mixed
C flow correlation but reverts to another correlation (user-specified, usually
C one for buoyancy-driven flow) when there is no heating or cooling (OFF state).

C Set air density & specific heat. Set zone air and surface temperatures.
                rhoair = 1.2
                CPair = 1006.
                TA=TFA(ICOMP)
                TS=TFS(ICOMP,ISUR)

C Determine whether zone is being heated, cooled, or whether system is off.
                call QPLANT(ICOMP,Qtotal)
                iOAflg = ifix(CVdat(II,jj+4))
                call QZONE(ICOMP,TA,Qtotal,iOAflg,Qznload)

                if( Qznload.gt.0. ) then

C ON state for HEATING. System supplies constant flow of air at minimum setting
C for VAV box (user-specified in ach). Zone coil heats air to meet demand. However,
C temperature of air at diffuser cannot exceed user-specified limit; if reheat
C cannot meet load then remaining heat is assumed to come from another source
C (eg. baseboards) which does not affect the flow regime.
C Calculate temperature of air entering zone.
                  ONOFF=.TRUE.
                  Vdot(ICOMP) = CVdat(II,jj+2)*VOL(ICOMP)/3600.
                  SAT(ICOMP) = TA + Qznload/( CPair*rhoair*Vdot(ICOMP) )
                  Tinmax = CVdat(II,jj+5)
                  if( SAT(ICOMP).gt.Tinmax ) SAT(ICOMP)=Tinmax

                elseif( Qznload.lt.0. ) then

C ON state for COOLING. System supplying air at constant temperature specified by
C user. Calculate volume of air required to meet cooling demand. Flow cannot be
C less than minimum level set by user (in ach). Flow cannot be greater than
C maximum level set by user (in ach).
                  ONOFF=.TRUE.
                  SAT(ICOMP) = CVdat(II,jj+1)
                  Vdot(ICOMP) = Qznload/( CPair*rhoair*(SAT(ICOMP)-TA) )
                  Vmin = CVdat(II,jj+2)*VOL(ICOMP)/3600.
                  if( Vdot(ICOMP).lt.Vmin ) Vdot(ICOMP)=Vmin
                  Vmax = CVdat(II,jj+3)*VOL(ICOMP)/3600.
                  if( Vdot(ICOMP).gt.Vmax ) Vdot(ICOMP)=Vmax
                else

C OFF state. If user specified `-1' for OFF correlation, then accept correlation
C chosen by SELCOR1. Set flow to zero so correct value is mapped to CFD domain
C (if active).
                  Vdot(ICOMP) = 0.
                  ONOFF=.FALSE.
                  if(ifix(CVdat(II,jj+7)).ne.-1) then
                    ICOR = ifix(CVdat(II,jj+7))
                  endif
                endif

                if(ONOFF) then             

C Select correlation equation (ICOR) using approach specified by user and
C calculate scaling factor for convection coefficient. Set temp diff between
C room inlet and outlet if Fisher free-horiz jet being used; ignore buoyancy
C of supply air when heating, since no correlation for this case.
                  iapp = ifix(CVdat(II,jj+6))
                  call SELFORC(ICOMP,ISUR,inRout,iapp,TS,TA,ICOR)
                  call FSCALE(SAT(ICOMP),TS,TA,ADJREF(ICOMP,ISUR))
                  if(iapp.eq.3) then
                    if( Qznload.lt.0. ) DELTAR(ICOMP)=TA-SAT(ICOMP)
                    if( Qznload.ge.0. ) DELTAR(ICOMP)=1.
                  endif
                endif
              ELSEIF( ifix(CVdat(II,jj)).eq.3 .and. inRout.eq.1 ) THEN

C ...Control law 3 (only applicable for `inside' surfaces):
C VAV system with constant-temperature, variable-volume cooling and with
C variable-temperature, VARIABLE-volume heating. When the system is providing
C heating or cooling (ON state), this control law utilizes a forced or mixed
C flow correlation but reverts to another correlation (user-specified, usually
C one for buoyancy-driven flow) when there is no heating or cooling (OFF state).

C Set air density & specific heat. Set zone air and surface temperatures.
                rhoair = 1.2
                CPair = 1006.
                TA=TFA(ICOMP)
                TS=TFS(ICOMP,ISUR)

C Determine whether zone is being heated, cooled, or whether system is off.
                call QPLANT(ICOMP,Qtotal)
                iOAflg = ifix(CVdat(II,jj+4))
                call QZONE(ICOMP,TA,Qtotal,iOAflg,Qznload)

                if( Qznload.gt.0. ) then

C ON state for HEATING. System attempts to meet heating demand at minimum
C user-specified flow rate (in ach). Zone coil heats air to meet demand but
C temperature of air at diffuser cannot exceed user-specified limit. If heating
C demand cannot be met at minimum flow rate, flow is increased, up to maximum
C level set by user (in ach), while temperature of air remains at maximum.
C If heating demand cannot be met at maximum flow and maximum temperature, then
C remaining heat is assumed to come from another source (eg. baseboards) which
C does not affect the flow regime. Calculate temperature of air entering zone.
                  ONOFF=.TRUE.
                  Vdot(ICOMP) = CVdat(II,jj+2)*VOL(ICOMP)/3600.
                  SAT(ICOMP) = TA + Qznload/( CPair*rhoair*Vdot(ICOMP) )
                  Tinmax = CVdat(II,jj+5)
                  if( SAT(ICOMP).gt.Tinmax )then
                    SAT(ICOMP) = Tinmax
                    Vdot(ICOMP) = Qznload/(CPair*rhoair*(SAT(ICOMP)-TA))
                    Vmax =  CVdat(II,jj+3)*VOL(ICOMP)/3600.
                    if( Vdot(ICOMP).gt.Vmax ) Vdot(ICOMP)=Vmax
                  endif
                elseif( Qznload.lt.0. ) then

C ON state for COOLING. System supplying air at constant temperature specified by
C user. Calculate volume of air required to meet cooling demand. Flow cannot be
C less than minimum level set by user (in ach). Flow cannot be greater than
C maximum level set by user (in ach).
                  ONOFF=.TRUE.
                  SAT(ICOMP) = CVdat(II,jj+1)
                  Vdot(ICOMP) = Qznload/( CPair*rhoair*(SAT(ICOMP)-TA) )
                  Vmin = CVdat(II,jj+2)*VOL(ICOMP)/3600.
                  if( Vdot(ICOMP).lt.Vmin ) Vdot(ICOMP)=Vmin
                  Vmax = CVdat(II,jj+3)*VOL(ICOMP)/3600.
                  if( Vdot(ICOMP).gt.Vmax ) Vdot(ICOMP)=Vmax
                else

C OFF state. If user specified `-1' for OFF correlation, then accept correlation
C chosen by SELCOR1. Set flow to zero so correct value is mapped to CFD domain
C (if active).
                  Vdot(ICOMP) = 0.
                  ONOFF=.FALSE.
                  if(ifix(CVdat(II,jj+7)).ne.-1) then
                    ICOR = ifix(CVdat(II,jj+7))
                  endif
                endif

                if(ONOFF) then             

C Select correlation equation (ICOR) using approach specified by user and
C calculate scaling factor for convection coefficient. Set temp diff between
C room inlet and outlet if Fisher free-horiz jet being used; ignore buoyancy of
C supply air when heating, since no correlation for this case.
                  iapp = ifix(CVdat(II,jj+6))
                  call SELFORC(ICOMP,ISUR,inRout,iapp,TS,TA,ICOR)
                  call FSCALE(SAT(ICOMP),TS,TA,ADJREF(ICOMP,ISUR))
                  if(iapp.eq.3) then
                    if( Qznload.lt.0. ) DELTAR(ICOMP)=TA-SAT(ICOMP)
                    if( Qznload.ge.0. ) DELTAR(ICOMP)=1.
                  endif
                endif
              ELSEIF( ifix(CVdat(II,jj)).eq.4 .and. inRout.eq.1 ) THEN

C ...Control law 4 (only applicable for `inside' surfaces):
C Variable-temperature constant-air-volume system (CVVT). When the system is
C providing heating or cooling (ON state), this control law utilizes a forced or
C mixed flow correlation but reverts to another correlation (user-specified,
C usually one for buoyancy-driven flow) when there is no heating or cooling
C (OFF state).

C Set air density & specific heat. Set zone air and surface temperatures.
                rhoair = 1.2
                CPair = 1006.
                TA=TFA(ICOMP)
                TS=TFS(ICOMP,ISUR)

C Determine whether zone is being heated, cooled, or whether system is off.
                call QPLANT(ICOMP,Qtotal)
                iOAflg = ifix(CVdat(II,jj+4))
                call QZONE(ICOMP,TA,Qtotal,iOAflg,Qznload)

                if( abs(Qznload).gt.0. ) then

C ON state. System supplying air at flow rate supplied by user (in ach).
C Calculate temperature of air required to meet zone heating/cooling demand.
C Temperature of air at diffuser cannot exceed user-specified limit; if load
C cannot be met by air-system, remaining heat is assumed to come from another
C source (eg. baseboards) which does not affect flow regime. Similarly, temperature
C of air at diffuser cannot be lower than user-specified limit.
                  Vdot(ICOMP) = CVdat(II,jj+1)*VOL(ICOMP)/3600.
                  SAT(ICOMP) = TA + Qznload/( CPair*rhoair*Vdot(ICOMP) )
                  Tinmax = CVdat(II,jj+2)
                  if( SAT(ICOMP).gt.Tinmax ) SAT(ICOMP)=Tinmax
                  Tinmin = CVdat(II,jj+3)
                  if( SAT(ICOMP).lt.Tinmin ) SAT(ICOMP)=Tinmin

C Select correlation equation (ICOR) using approach specified by user and
C calculate scaling factor for convection coefficient. Set temp diff between
C room inlet and outlet if Fisher free-horiz jet being used; ignore buoyancy of
C supply air when heating, since no correlation for this case.
                  iapp = ifix(CVdat(II,jj+6))
                  call SELFORC(ICOMP,ISUR,inRout,iapp,TS,TA,ICOR)
                  call FSCALE(SAT(ICOMP),TS,TA,ADJREF(ICOMP,ISUR))
                  if(iapp.eq.3) then
                    if( Qznload.lt.0. ) DELTAR(ICOMP)=TA-SAT(ICOMP)
                    if( Qznload.ge.0. ) DELTAR(ICOMP)=1.
                  endif
                else

C OFF state. If user specified `-1' for OFF correlation, then accept correlation
C chosen by SELCOR1. Set flow to zero so correct value is mapped to CFD domain
C (if active).
                  Vdot(ICOMP) = 0.
                  if(ifix(CVdat(II,jj+7)).ne.-1) then
                    ICOR = ifix(CVdat(II,jj+7))
                  endif
                endif

              ELSEIF( ifix(CVdat(II,jj)).eq.5 ) THEN
C ...Control law 5:
C Toggle between fixed convection coefficients based on state of plant injection
C within zone. User has specified values of convection coefficients when there is heating,
C cooling, or no plant interaction. This can be used for slab heating/cooling systems
C or ceiling heating/cooling systems. Can be used for `inside' or `outside' surfaces.
C If user has specified `-1' for the convection coefficient, then accept the correlation
C chosen by SELCOR1.

C Any convective, radiation, or combined plant input?
                call QPLANT(ICOMP,Qheat)
                if( Qheat.gt.0. ) then
C Heating to zone.
                  if(ifix(CVdat(II,jj+1)).ne.-1) then
                    ICOR = 99
                    HCctl5 = CVdat(II,jj+1)
                  endif
                elseif( Qheat.lt.0. ) then
C Cooling to zone.
                  if(ifix(CVdat(II,jj+2)).ne.-1) then
                    ICOR = 99
                    HCctl5 = CVdat(II,jj+2)
                  endif
                else
C No heating or cooling.
                  if(ifix(CVdat(II,jj+3)).ne.-1) then
                    ICOR = 99
                    HCctl5 = CVdat(II,jj+3)
                  endif
                endif

              ELSE
C ...Add additional control laws here.
              ENDIF
            ENDIF
          ENDIF
   24   CONTINUE
      ENDIF

      RETURN
      END


C ******************** FSCALE *********************
C FSCALE calculates the scaling factor to translate the Fisher `thesis'
C and Fisher & Pedersen `ASHRAE' convection coefficients to ESP-r's convection
C model.

      SUBROUTINE FSCALE(TIN,TS,TA,ADJ)

C Fisher's correlations use the HVAC systems' supply air temperature (TIN) as
C the reference temperature whereas ESP-r's energy balance utilizes the room air
C temperature (TA). Consequently, the convection coefficient calculated by Fisher's
C correlation must be scaled to the room air temperature. ADJREF is
C the scaling factor, which has a different value for each surface, and must be
C a positive value.

C ESP-r uses, Qc = HCesp*(TA-TS).
C Fisher & Pedersen use, Qc = HCfisher*(TIN-TS).
C Therefore, since HCesp*(TA-TS) = HCfisher*(TIN-TS)
C    ==> HCesp = HCfisher * (TIN-TS)/(TA-TS)

C Fisher's correlations were developed for a limited range of TIN. Testing has
C confirmed that extension to higher TIN is safe. However, the correlations
C should not be extended to lower TIN as this has not been examined.
      if( TIN.lt.10.) TIN=10.

C If the surface and air-point temperatures are the same, set the
C scaling factor to a large value (results are insensitive to magnitude
C as TS -> TA) to avoid division by zero.
      if( ABS(TS-TA).lt.0.001 ) then
        ADJ = 1000.
      else
        ADJ = ABS(( TS-TIN ) / ( TS-TA ))
      endif

      RETURN
      END


C ******************** SELFORC *********************
C SELFORC selects a forced or mixed flow correlation based on approach specified
C by user and based on surface orientation. Returns ICOR.

      SUBROUTINE SELFORC(ICOMP,ISUR,inRout,iapp,TS,TA,ICOR)
#include "building.h"

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/HCcomb/iAandH,iassist

C Get orientation of surface.
      ANGLE = PELV(ICOMP,ISUR)
      if(inRout.eq.2) ANGLE = -1.*ANGLE

C Select correlation for approach specified by user:
C   iapp=1 => Fisher & Pedersen `ASHRAE' correlations for radial ceiling diffusers.
C   iapp=2 => Fisher `thesis' correlations for radial ceiling diffusers.
C   iapp=3 => Fisher `thesis' correlations for free horizontal jet.
C   iapp=4 => Mixed convection model based on Fisher `thesis' correlations for
C             radial ceiling diffusers and Alamdari & Hammond's buoyancy eqns.

C In addition to ICOR, the mixed convection model requires the following
C variables to be set here:
C   iAandH :  Indicates whether to use Alamdari & Hammond equation for buoyant
C             conditions (iAandH=1) or for stably stratified conditions (iAandH=2).
C   iassist : Indicates whether buoyant forces assist (+1) or oppose (-1)
C             mechanical jet.

C Wall:
      IF(ANGLE.LE.45..AND.ANGLE.GE.-45.) THEN
        if(iapp.eq.1) ICOR=10
        if(iapp.eq.2) ICOR=11
        if(iapp.eq.3) ICOR=12
        if(iapp.eq.4)then
          ICOR=13
          if(TS.gt.TA) iassist = -1
          if(TS.le.TA) iassist = 1
        endif
C Floor:
      ELSEIF(ANGLE.LT.-45.) THEN
        if(iapp.eq.1) ICOR=80
        if(iapp.eq.2) ICOR=81
        if(iapp.eq.3) ICOR=82
        if(iapp.eq.4)then
          ICOR=83
          if(TS.gt.TA) iAandH = 1
          if(TS.le.TA) iAandH = 2
        endif
C Ceiling:
      ELSEIF(ANGLE.GT.45.) THEN
        if(iapp.eq.1) ICOR=90
        if(iapp.eq.2) ICOR=91
        if(iapp.eq.3) ICOR=92
        if(iapp.eq.4)then
          ICOR=93
          if(TS.gt.TA) iAandH = 2
          if(TS.le.TA) iAandH = 1
        endif
      ENDIF

      RETURN
      END


C ******************** OUTISIN ********************
C OUTISIN sets the convection coefficients for `outside' surfaces which are
C `interior' building surfaces.

      SUBROUTINE OUTISIN 

#include "building.h"
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &            ARF(MCOM,MS)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)

C Each physical surface of the building is modelled with two mlc's: one
C for each zone bounding the physical surface.  Consequently, the `outside'
C surface of one zone's mlc corresponds to the `inside' surface of the adjacent
C zone's mlc.  These `outside' surfaces are actually `interior' building
C surfaces.

C For each `outside' surface that is an `interior' building surface, this
C subroutine sets the convection coefficient to the value calculated in the
C neigbouring zone.  This ensures that each physical surface has a consistent
C convection coefficient in the zone equation sets.  In so doing, the logic
C utilized in adaptive convection calculations is applied to the `outside'
C surfaces that are `interior' building surfaces.

C Iterate through each zone.
      DO 100 ICOMP=1,NCOMP

C Iterate through each multi-layer construction.
        NC=NCONST(ICOMP)
        DO 101 ISUR=1,NC

C Does the `outside' surface of this mlc face the air point of an adjacent zone?
C In other words, is this `outside' surface an `interior' building surface?
          if( IE(ICOMP,ISUR).GT.0 ) then
C-----------Get the connection index.
            II=IE(ICOMP,ISUR)
C-----------Get the number of the adjacent zone.
            II1=IC2(II)
C-----------Get the number of the corresponding mlc.
            II2=IE2(II)
C-----------Set the `outside' convection coefficient equal to the `inside'
C-----------coefficient of the neighbouring zone.
            HCOF(ICOMP,ISUR) = HCIF(II1,II2)
          endif

  101   CONTINUE
  100 CONTINUE
      RETURN
      END

C ******************** QPLANT ********************
C QPLANT returns QM, the total plant injection (+) or extraction (-)
C to the zone.

      SUBROUTINE QPLANT(ICOMP,QM)
#include "building.h"

      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/SETUQ/QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)

C Compute mean energy injection/extraction at plant interaction node (PIN).
C Take average of present and future values.

c PIN is air point.
      QMA = (QPA(ICOMP)+QFA(ICOMP))*.5

C PIN is at surface.
      QMS = (QPS(ICOMP)+QFS(ICOMP))*.5

C PIN is a mix of radiant/convective flux (eg radiator).
      QMAS = (QPLTP(ICOMP)+QPLTF(ICOMP))*.5

C PIN is within construction.
      QMC = (QPC(ICOMP)+QFC(ICOMP))*.5

C Note that injection from explicit plant components is
C not considered here yet.

C Sum to get total plant injection/extraction.
      QM = QMA + QMS + QMAS + QMC

      RETURN
      END


C ******************** QZONE ********************
C QZONE returns Qznload, the convective heat injected (+) or extracted (-)
C to/from the zone by the air-handling system.

      SUBROUTINE QZONE(ICOMP,TA,Qtotal,iOAflg,Qznload)
#include "building.h"

      COMMON/COE39S/CIF(MCOM),CVF(MCOM),X3(MCOM),CVM(MCOM)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

C Qtotal is the total heating or cooling load, including the load of the
C outdoor air. Since the outdoor air is conditioned within the air-handling
C system, it does not impose a load on the zone. Only the `zone load' is
C considered in determining the temperature and flow rate of air required to
C heat/cool the zone. Therefore, the purpose of this subroutine is to determine
C the `zone load', the total load less the load imposed by the outdoor air.

C iOAflg is an integer flag indicating how the user has treated outdoor air:
C   0: Outdoor air not considered in model, ie. Qznload=Qtotal.
C   1: Outdoor air treated as infiltration.
C   2: Outdoor air treated as advection from another zone (eg. zone representing
C      air-handling unit.

      IF(iOAflg.eq.1) THEN
C Outdoor air treated as infiltration. Calculate future value of infiltration load.
C Infiltration conductance CIF from MZVENT. TF is future value of outdoor
C temperature.
        Qoa = CIF(ICOMP)*( TA-TF )
      ELSEIF(iOAflg.eq.2) THEN

C Outdoor air treated as advection from another zone. Calculate future value of
C advection load. Advection conductances are from MZVENT: CVF is future value
C of conductance, given in terms of Qvent=CVV*(Totherzone-TA); CVM is CVF
C normalized to outdoor air temperature, so Qvent=CVM*TF-CVF*TA.
        Qoa = CVF(ICOMP)*TA - CVM(ICOMP)*TF
      ELSE

C Outdoor air not considered.
        Qoa = 0.
      ENDIF

C `Zone load' is total load less outdoor-air load.
      Qznload = Qtotal - Qoa

      RETURN
      END
