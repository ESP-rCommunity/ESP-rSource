C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2006.

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
C  MZADJC - computes adjacent conditions for surfaces.
C
C ******************** MZADJC 

C MZADJC determines whether each construction in every Zone
C is internal or external and computes the assocoiated
C adjacent temperature and incident radiation values at the
C present and future time-rows.

C This routine also flags surfaces that are coupled to other
C zones, permitting MZSETU to properly append intra-construction
C and surface radiant heat injections to the energy balances
C for adjacent zones. 

C Common block variables are:

C IE     - surface defining index where:
C          IE=0 ; external
C          IE=-1 ; internal/same
C          IE=-2 ; internal/specified constant
C          IE> 0 ; internal/zone-coupled where IE identifies
c                  the connection.
C          IE=-4 ; ground coupled.
C          IE=-5 ; adiabatic.
C          IE=-6 ; BASESIMP foundation.
C          IE=-7 ; CEN 13791 partition.

C ATP    - present time adjacent temperature
C ATF    - future time adjacent temperature
C ARP    - present time adjacent incident radiation & casual etc.
C ARF    - future time adjacent incident radiation & casual etc.

      SUBROUTINE MZADJC
#include "building.h"
#include "site.h"
#include "geometry.h"
#include "control.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PREC14/emarea(MCOM)

C izsfloor is the index (within the zone) of a floor surface in each zone
C  (zero if no surface is close to horizontal facing up).
C izsceil is the index (within the zone) of a ceiling surface in each zone 
C  (zero if no surface is close to horizontal facing down).
      integer izsfloor,izsceil
      COMMON/PREC16/izsfloor(MCOM),izsceil(MCOM)

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &            ARF(MCOM,MS)

      COMMON/COE31S/HRS(MCOM,MS,MS),ZHRS(MCOM,MS)
      COMMON/COE32S/QSLIF(MCOM,MS),QSLEF(MCOM,MS),QTMCAF(MCOM,MS,MN)
      COMMON/COE34/QCASRT(MCOM),QCASCT

C Common containing zone casual gains
      COMMON/CasDat/Cas_Rad_dat(MCOM),Cas_Con_dat(MCOM),
     &     Cas_Lat_dat(MCOM)
      real Cas_Rad_dat, Cas_Con_dat, Cas_Lat_dat
C Common saving coupled zones/surfaces
      common/Zone_Couplings/ bCoupled_Zone (MCOM,MS),
     &                       iCoupled_Zone (MCOM,MS),
     &                       iCoupled_Surf (MCOM,MS)

      logical bCoupled_Zone
      integer iCoupled_Surf, iCoupled_Zone

C Control commons (for use with partition injections).
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/SETUQ/QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)
      COMMON/CCTL/ICASCF(MCOM)

C The following common saves the computed future time-row
C values for use as present values at the next time-step.
      COMMON/ADJCS/ATFS(MCOM,MS),ARFS(MCOM,MS)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      COMMON/GRND101/NNODTG,ITCNSTG,TEMP3G(MCOM),AREAGD(MCOM)

      COMMON/MOIST22/APp(MCOM,MS),APf(MCOM,MS)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)

      DIMENSION qlwsum(MS),qradcas(MS),qadjsol(MS),tinside(MS)
      DIMENSION tother(MS),qradcin(MS),qpltri(MS),qpltro(MS)
      DIMENSION qswsum(MS)
C.....Named constnats for timerows
      integer iPresent, iFuture
      parameter (iPresent=1, iFuture=2) 
      
C START SPECIAL mod for PASSYS
C      DIMENSION tsurm(3)
C      data init/1/
C FINISH SPECIAL mod for PASSYS

      CHARACTER outs*124
      LOGICAL ATRACE
      real zoar  ! local real for zonetotsurfacearea(MCOM)

C Flag trace info.
      ATRACE=.FALSE.
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9
      IF(ITRACE(32).NE.1.OR.NSINC.GT.ITCF)goto 9
      ATRACE=.TRUE.
      write(outs,'(A,I4)')' Subroutine MZADJC    Trace output',ICNT
      call edisp(itu,outs)
      ICNT=ICNT+1
      CALL DAYCLK(IDYP,BTIMEF,ITU)
    9 CONTINUE

C START SPECIAL mod for PASSYS
C      init=init+1
C      if(init.eq.25)init=1
C     if(init.eq.1)read(26,*)
C      read(26,*)idum,tdum,tdum,tsurm(1),tdum,tsurm(2),tdum,tsurm(3)
C END SPECIAL mod for PASSYS

C Zone-by-zone.
      DO 101 ICOMP=1,NCOMP
        NC=NCONST(ICOMP)
        if(ATRACE)then
          call edisp(itu,' ')
          write(outs,'(1x,A,A,I2,A,I2,A)') 
     &      zname(icomp)(1:lnzname(icomp)),' (',ICOMP,') with ',
     &      NC,' surfaces, units W/m^2 '
          call edisp(itu,outs)
          write(outs,'(8x,A,13x,A)')'inside face','|      other face'
          call edisp(itu,outs)
          write(outs,'(A,A)') ' SUR  INDX   Temp   Rcas   Rplt |  ATF',
     &                      '   ARF > ( lw    Rcas   Rplt  solar) Temp'
          call edisp(itu,outs)
        endif


C Surface-by-surface.
        IC=ICASCF(ICOMP)
        IF(IC.NE.0)THEN

C Debug.
C          write(6,'(A,4I3,4F8.2)')'ic,IBAN(1-3),qps,qfs,qpltp,qpltf=',
C     &      ic,IBAN(IC,1),IBAN(IC,2),IBAN(IC,3),QPS(ICOMP),QFS(ICOMP),
C     &      QPLTP(ICOMP),QPLTF(ICOMP)

        endif
        DO 10 I=1,NC
          ATP(ICOMP,I)=ATFS(ICOMP,I)
          ARP(ICOMP,I)=ARFS(ICOMP,I)
          qlwsum(I)=0.0
          qswsum(I)=0.0
          qradcas(I)=0.0
          qadjsol(I)=0.0
          tinside(I)=TFS(ICOMP,I)
          tother(I)=0.0
          APp(ICOMP,I)=APf(ICOMP,I)

C For reporting radiant casual at inside face.
          zoar=zonetotsurfacearea(icomp)
          qradcin(I)=(QCASRT(icomp)*zoar*0.5)*
     &                    ei(icomp,I)/emarea(icomp)

C For reporting plant radiant energy at inside face.
C Units below seems to be W/m^2.
          qpltri(I)=0.0
          qpltro(I)=0.0
          IC=ICASCF(ICOMP)
          IF(IC.NE.0)THEN
            IF(IBAN(IC,1).EQ.ICOMP.AND.IBAN(IC,2).EQ.I.AND.
     &         IBAN(IC,3).EQ.0)then
              qpltri(I)=(QPS(ICOMP)+QFS(ICOMP))*.5
            ELSEIF(IBAN(IC,1).EQ.-2.AND.IBAN(IC,2).EQ.ICOMP)then
              qpltri(I)=(QPLTP(ICOMP)+QPLTF(ICOMP))*0.5*
     &                  (1.-CONV(ICOMP))*ei(icomp,I)/emarea(icomp)
            ENDIF
          ENDIF

C Connection-by-connection.
          DO 20 J=1,NCON
            IF(IC1(J).NE.ICOMP.OR.IE1(J).NE.I)goto 20

C Connection established.
            IE(ICOMP,I)=-ICT(J)
            IF(ICT(J).EQ.3)IE(ICOMP,I)=J

C Connection: external.
            IF(ICT(J).EQ.0)THEN
              ATF(ICOMP,I)=TF
              ARF(ICOMP,I)=0.
              RH=HF/100.0
              APf(ICOMP,I)=Pvpr02(TF,RH)
              

C START SPECIAL mod for PASSYS
C              if(icomp.eq.2.and.(I.eq.1.or.I.eq.8.or.I.eq.10))then
C                kkk=1
C                 if(I.eq.8)kkk=2
C                 if(I.eq.10)kkk=3
C                 ATF(ICOMP,I)=tsurm(kkk)
C              endif
C END SPECIAL mod for PASSYS

C Connection: internal/same or relative (future time-row calculated
C at present time).  Assume casual radiant injection and long wave
C radiant exchange at outermost surface same as innermost.
            ELSEIF(ICT(J).EQ.1)THEN

C Plant radiant energy at other face same as inside face.
              qpltro(I)=qpltri(I)

              qlwsum(I)=0.
              DO 802 K=1,NCONST(ICOMP)
                IF(K.NE.I)qlwsum(I) = qlwsum(I) + (HRS(ICOMP,K,I)*
     &                  (TFS(ICOMP,K)-TFS(ICOMP,I)))
  802         CONTINUE
      
              ATF(ICOMP,I)=TFA(ICOMP)+FLOAT(IC2(J))
C This appears to be a bug:
C The timestep-averaged zone radiative casual gains
C are used to calculate the future gains at the outside surface.
              zoar=zonetotsurfacearea(icomp)
              qradcas(I)=(QCASRT(icomp)*zoar*0.5)*
     &                    ei(icomp,I)/emarea(icomp)
              
C Instead, use the future zone radiative casual gains, and append
C the gains resulting from plant containments on the outside
C surface. 
C
C              qradcas(I)=
C     &         Cas_Rad_dat(icomp) * ei(icomp,I)/emarea(icomp)
C

              ARF(ICOMP,I)=qlwsum(I)+qradcas(I)
              APf(ICOMP,I)=Pfair(ICOMP)

C Connection: internal/specified constant.
            ELSEIF(ICT(J).EQ.2)THEN
              ATF(ICOMP,I)=FLOAT(IC2(J))
              ARF(ICOMP,I)=FLOAT(IE2(J))
              APf(ICOMP,I)=0.6*Psat01(ATF(ICOMP,I))

C Connection: internal/zone-coupled (future time-row calculated at
c present time).
            ELSEIF(ICT(J).EQ.3)THEN
              ICPLE=IC2(J)
              ISCPLE=IE2(J)
              ATF(ICOMP,I)=TFA(ICPLE)

C Save coupled zone/surface to permit plant heat injection associated
C with adjacent zones to be tracked and inserted into this zone's
C constructions

              bCoupled_Zone ( iComp, i) = .true.
              iCoupled_Zone ( iComp, i) = iCPLE
              iCoupled_Surf ( iComp, i) = iSCPLE 

C ARF includes the solar radiation incident on the other face,
C the sum of the longwave radiation and casual radiant injection.
C Making use of the temperatures in the other zone, reconstruct the
C longwave radiation exchange.

C Plant radiant energy at other face.
              IC=ICASCF(ICPLE)
              qpltro(I)=0.0
              IF(IC.NE.0)THEN
                IF(IBAN(IC,1).EQ.ICPLE.AND.IBAN(IC,2).EQ.ISCPLE.AND.
     &             IBAN(IC,3).EQ.0)then
                  qpltro(I)=(QPS(ICPLE)+QFS(ICPLE))*.5
                ELSEIF(IBAN(IC,1).EQ.-2.AND.IBAN(IC,2).EQ.ICPLE)then
                  qpltro(I)=(ei(ICPLE,ISCPLE)/emarea(ICPLE))*0.5*
     &                      (QPLTP(ICPLE)+QPLTF(ICPLE))*(1.-CONV(ICPLE))
                ENDIF
              ENDIF

              qlwsum(I)=0.

              DO 702 K=1,NCONST(ICPLE)
                IF(K.NE.ISCPLE)qlwsum(I)=qlwsum(I)+
     &           (HRS(ICPLE,K,ISCPLE)*(TFS(ICPLE,K)-TFS(ICPLE,ISCPLE)))
  702         CONTINUE


C This appears to be a bug:
C The timestep-averaged zone radiative casual gains
C are used to calculate the future gains at the outside surface.
              zoar=zonetotsurfacearea(ICPLE)
              qradcas(I)=(QCASRT(ICPLE)*zoar*0.5)*
     &                 ei(ICPLE,ISCPLE)/emarea(ICPLE)

C Instead, use the future zone radiative casual gains, and append
C the gains resulting from plant containments on the outside
C surface. 
C
C              
C              qradcas(I)=
C     &         Cas_Rad_dat(ICPLE) * ei(ICPLE,ISCPLE)/emarea(ICPLE)
C     &         + fSurface_radiant_gain(iCPLE,iSCPLE,iFuture)
C     &         / sna(iCPLE,iSCPLE)


C Debug.
C      write(6,'(A,4I4,4F8.2)')'iz is oiz ois qr QCASRT zoa sna=',
C     &        icomp,i,ICPLE,ISCPLE,qradcas(I),QCASRT(ICPLE),
C     &        zonetotsurfacearea(ICPLE),sna(ICPLE,ISCPLE)

              ARF(ICOMP,I)=QSLIF(ICPLE,ISCPLE)+qlwsum(I)+qradcas(I)+
     &                     qpltro(I)
              qadjsol(I)=QSLIF(ICPLE,ISCPLE)
              tinside(I)=TFS(ICOMP,I)
              tother(I)=TFS(ICPLE,ISCPLE)
              APf(ICOMP,I)=Pfair(ICPLE)

c Connection 4: variable monthly temperature.
            ELSEIF(ICT(J).EQ.4)THEN
              CALL EDAYR(IDYF,IDYX,IMNTH)
              IF(IC2(J).GT.0)THEN
                ATF(ICOMP,I)=GRDTMP(IMNTH,IC2(J))
              ELSEIF(IC2(J).EQ.0)THEN
                ATF(ICOMP,I)=UGRDTP(IMNTH,IE2(J))

C Connection with the 3D ground model.
              ELSEIF(IC2(J).EQ.-3)THEN
                ATF(ICOMP,I)=TEMP3G(ICOMP)
              ENDIF
              ARF(ICOMP,I)=0.0
              APf(ICOMP,I)=0.6*Psat01(TF)

C Connection 5: Adiabatic case (adjacent zone air temperature set to
C external surface temperature). Note that this is an exact treatment.
C The following parameters assume a corresponding action in MZSETU.
            ELSEIF(ICT(J).EQ.5)THEN
              ATP(ICOMP,I)=TFC(IC1(J),IE1(J),1)
              ARP(ICOMP,I)=0.0

c Next line only needed for MZCONV.
              ATF(ICOMP,I)=TFC(IC1(J),IE1(J),1)
              ARF(ICOMP,I)=0.0
              APf(ICOMP,I)=0.6*Psat01(ATF(ICOMP,I))

C BASESIMP connection.
            ELSEIF(ICT(J).EQ.6)THEN

C Set adjacent temperature same as zone temperature.
              ATF(ICOMP,I)=TFA(ICOMP)

C Calculate the basement air temperature to use as the driving force
C for the BASESIMP-calculated heat loss to the soil and outdoor air.
              call BSTEMP(ICOMP,TBSAVG)

C Set radiation to adjacent surface equal to the negative of the heat
C flow from the foundation to the soil, as calculated by BASESIMP.
C This is multiplied by `surface weighting factor' (IE2) which indicates
C the percentage of the BASESIMP heat loss to attribute to the surface
C under consideration. Normally the sum of the `surface weighting factors'
C is 100%.
              call BSHEAT(ICOMP,J,TBSAVG,QBS)
              call H3KSTORE_FNDTN_LOSSES(ICOMP,idyp,QBS)
              ARF(ICOMP,I) = -0.01*FLOAT(IE2(J)) * QBS/SNA(ICOMP,I)

C APf is undocumented, but has something to do with moisture.
C Psat01 is a function that returns the saturation vapour pressure
C as a function of temperature. Treated the same here as for connection
C type 2.
              APf(ICOMP,I)=0.6*Psat01(ATF(ICOMP,I))

            ELSEIF(ICT(J).EQ.7)THEN

C Connection 7: CEN partition case - This is as the "similar" but
C there is the same shortwave solar radiation for both sides and
C the adjacent zone air temperature set to the temperature of the
C other face of the surface. Note that this is an exact treatment
C as specified in the CEN standard.
C The following parameters assume a corresponding action in MZSETU.

C Plant radiant energy at other face same as inside face.
              qpltro(I)=qpltri(I)

              qlwsum(I)=0.
              qswsum(I)=0.      
              DO 1802 K=1,NCONST(ICOMP)
                IF(K.NE.I)qlwsum(I) = qlwsum(I) + (HRS(ICOMP,K,I)*
     &                  (TFS(ICOMP,K)-TFS(ICOMP,I)))
 1802         CONTINUE

C If floor, then add the sw rad. of the other face of the ceiling
C If ceiling, then add the sw rad. of the other face of the floor
              if(PELV(icomp,i).GE.88.5.AND.
     &           PELV(icomp,i).LE.91.5)then

C Current surface is a ceiling so assign qswsum = of a typical floor.
C But if izsfloor is zero then assume surface 6 of the zone is a floor.
                if(izsfloor(icomp).ne.0)then
                  qswsum(I)=QSLIF(ICOMP,izsfloor(icomp))
                else
                  qswsum(I)=QSLIF(ICOMP,6)
                endif
              elseif(PELV(icomp,i).GE.-91.5.AND.
     &               PELV(icomp,i).LE.-88.5)then

C Current surface is a floor so assign qswsum = of a typical ceiling.
C But if izsceil is zero then assume surface 5 of the zone is a ceiling.
                if(izsceil(icomp).ne.0)then
                  qswsum(I)=QSLIF(ICOMP,izsceil(icomp))
                else
                  qswsum(I)=QSLIF(ICOMP,5)
                endif
              else

C Orientation is not a floor or ceiling so assume it is a wall
C in which case the solar on the other face is the same as on
C the inside of the wall (CEN is in a universe where this happens).
                qswsum(I)=QSLIF(ICOMP,I)
              endif

              ATF(ICOMP,I)=TFA(ICOMP)+FLOAT(IC2(J))

C This appears to be a bug:
C The timestep-averaged zone radiative casual gains
C are used to calculate the future gains at the outside surface.
              zoar=zonetotsurfacearea(icomp)
              qradcas(I)=(QCASRT(icomp)*zoar*0.5)*
     &                    ei(icomp,I)/emarea(icomp)
              
C Instead, use the future zone radiative casual gains, and append
C the gains resulting from plant containments on the outside
C surface. 
C
C              qradcas(I)=
C     &         Cas_Rad_dat(icomp) * ei(icomp,I)/emarea(icomp)
C

              ARF(ICOMP,I)=qlwsum(I)+qradcas(I)+qswsum(I)
              APf(ICOMP,I)=Pfair(ICOMP)
      
C Other connection types go here !
C           ELSEIF  ......    
        
            ENDIF

C No other connections need to be tested, jump to next construction.
            goto 12
   20     CONTINUE

C Trace.
   12     IF(ATRACE)then
            if(ie(icomp,i).ne.0)then
              WRITE(outs,9993)I,IE(ICOMP,I),tinside(I),qradcin(I),
     &              qpltri(I),ATF(ICOMP,I),ARF(ICOMP,I),qlwsum(I),
     &              qradcas(I),qpltro(I),qadjsol(I),tother(I)
 9993         FORMAT( I4,I5,2X,F6.2,8F7.2,F6.2)
              call edisp(itu,outs)
            endif
          endif
   10   CONTINUE
  101 CONTINUE

c Save future values.
      DO 201 ICOMP=1,NCOMP
        NC=NCONST(ICOMP)
        DO 30 I=1,NC
          ATFS(ICOMP,I)=ATF(ICOMP,I)
          ARFS(ICOMP,I)=ARF(ICOMP,I)
   30   CONTINUE
  201 CONTINUE

      RETURN
      END
