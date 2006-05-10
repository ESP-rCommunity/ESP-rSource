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
C      MZSOLR
C      MZSLGN
C      MZSANG
C      MZSINT
C      MZSCAI
C      MZSRAD
C      MZWINP
C      MZWINP2
C      MZTMCA
C      MZSFSH
C      MZSHDO

C ******************** MZSOLR ********************
C MZSOLR initiates the computation of the solar gains impinging
C on internal and external surfaces.

      SUBROUTINE MZSOLR(ICOMP)

#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      COMMON/COE32I/QLOSSD(MCOM,MS),QLOSSF(MCOM,MS),QLOSSA(MCOM,MS),
     &QDOOR(MCOM)
      COMMON/ADJC2/DIRT(MCOM,MS),DIFT(MCOM,MS),AIRT(MCOM,MS)
      COMMON/COE32J/QTMCA(MS,MN,2)
      COMMON/COE32S/QSLIF(MCOM,MS),QSLEF(MCOM,MS),QTMCAF(MCOM,MS,MN)

      COMMON/SOLOFF/ISOLAR
      COMMON/T3SAV/QTMCI(MCOM,MS,MN)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      character outs*124

      NC=NCONST(ICOMP)

C If ISOLAR=0 conduct solar analysis (default)
C If ISOLAR=1 then no solar analysis
      IF(ISOLAR.EQ.0)THEN
        CALL MZSLGN(ICOMP)
      ENDIF

C For current component (ICOMP) establish the first
C and last connections associated with surfaces.
      JCONF=0
      DO 10 J=1,NCON
        IF(JCONF.GT.0)goto 11
        IF(IC1(J).NE.ICOMP)goto 10
        IF(IE1(J).NE.1)goto 10
        JCONF=J
        JCONL=JCONF+NC-1
   10 CONTINUE

   11 ISURF=0
C Connection-by-connection.
      DO 20 J=JCONF,JCONL

C Surface number, ISURF.
      ISURF=ISURF+1

C Check for Type 3 connection.
      IF(ICT(J).EQ.3)THEN

C Type 3 connection established: internal/zone-coupled (future
C time-row calculated at present time). ICPLE and ISCPLE are the
C coupled zone and surface numbers.
        ICPLE=IC2(J)
        ISCPLE=IE2(J)

C Assign QLOSSD, QLOSSF and QLOSSA for current zone and surface
C to DIRT, DIFT and AIRT of coupled zone.
        DIRT(ICPLE,ISCPLE)=DIRT(ICPLE,ISCPLE)+QLOSSD(ICOMP,ISURF)
        DIFT(ICPLE,ISCPLE)=DIFT(ICPLE,ISCPLE)+QLOSSF(ICOMP,ISURF)
        AIRT(ICPLE,ISCPLE)=AIRT(ICPLE,ISCPLE)+QLOSSA(ICOMP,ISURF)

C If construction is a TMC then increment nodal absorptions due to
C any flux  absorptions within the contiguous TMC.
        ITMC1=ITMCFL(ICOMP,ISURF)
        ITMC2=ITMCFL(ICPLE,ISCPLE)
        IF(ITMC1.NE.0)THEN
          IF(ITMC2.EQ.0)THEN
            write(outs,1)ICPLE,ISCPLE
    1       format(' Zone',I3,' surface',I3,' should be a TMC')
            call edisp(iuout,outs)
            call edisp(iuout,
     &         ' but is not! The simulation will be stopped.')
            call epwait
            call epagend
            STOP
          ENDIF

           NN=NNDC(ICOMP,ISURF)
           DO 40 II=1,NN
              JJ=NN+1-II

C QTMCI is used to remember the nodal flux absorption
C for latter addition to the absorptions within the contiguous TMC.
              QTMCI(ICOMP,ISURF,II)=QTMCAF(ICOMP,ISURF,II)
              QTMCA(ISURF,II,2)=QTMCA(ISURF,II,2)+
     &                               QTMCI(ICPLE,ISCPLE,JJ)
              QTMCAF(ICOMP,ISURF,II)=QTMCAF(ICOMP,ISURF,II)+
     &                               QTMCI(ICPLE,ISCPLE,JJ)
   40      CONTINUE
        ENDIF

      ENDIF
   20 CONTINUE

      RETURN
      END

C ******************** MZSLGN ********************
C MZSLGN is an extended version of MZSOLG which allows
C multi-layered constructions to be transparent so that
C windows can be very accurately handled. It computes:

C 1. QSOLE - the solar energy absorbed by each external opaque surface
C      after adjustment by surface shading and allowing for solar
C      building geometry.

C 2. QSOLI - the solar energy absorbed by each internal opaque surface
C      after adjustment by window shading and allowing for
C      directional property of direct beam and multiple diffuse
C      reflections.

C 4. QLOSSD- the direct beam transmission through internal surface (type 3)
C      windows.

C 5. QLOSSF- the diffuse beam transmission through internal surface (type 3)
C      windows.

C 6. QLOSSA- the total re-transmitted (onabsorbed energy through internal
C      surface (type 3) windows.

C 7. QTMCA - the shortwave absorption at each node of a transparent multi-layered
C      construction (window by another name).

C These energy transfers are computed for both the present
C and future time rows. Units: W/m^2

      SUBROUTINE MZSLGN(ICOMP)
#include "building.h"

C ADD NEW PARAMETER STATEMENT
C MSTMC is number of different bidirectional datasets, limited
C to 1 for the present.
C MSGAL is number of optical sets per optics db item. At present
C set to 1: later increase to 2 for an alternative set that can be switched.
C MANH and MANV are the number of angles at which optical data is 
C held for horizontal/vertical grid data.
C At present this assumes 5deg bi-directional data from -90 to +90
C in elevation and azimuth.
      PARAMETER (MSTMC=1,MSGAL=1,MANH=37,MANV=37)

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/G4/NDP(MCOM),IDPN(MCOM,3)

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5,MBP),TMCA2(MCOM,MTMC,ME,5,MBP),
     &              TVTR2(MCOM,MTMC,MBP)

      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MBP,MTMC),
     &IBCFT(MCOM,MBP,MTMC),IBCSUR(MCOM,MTMC)
      COMMON/TMCB3/NBCTT(MCOM,MBP,MTMC),BACTPT(MCOM,MBP,MTMC)
      COMMON/TMCB4/IBOFOT(MCOM,MS),NBPONT(MCOM,MS),ECRAT(MCOM,MS)

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      COMMON/SHAD2/ISHD(MCOM),IGCS(MCOM,MS),PO(MCOM,MS,MT),
     & IGCI(MCOM,MS),insst(mcom,mgt,mt,misur),pinsst(mcom,mgt,mt,misur)

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),
     &ARP(MCOM,MS),ARF(MCOM,MS)
      COMMON/ADJC2/DIRT(MCOM,MS),DIFT(MCOM,MS),AIRT(MCOM,MS)

      COMMON/SUNPOS/SAZI,SALT,ISUNUP

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/COE32I/QLOSSD(MCOM,MS),QLOSSF(MCOM,MS),
     &QLOSSA(MCOM,MS),QDOOR(MCOM)
      COMMON/COE32J/QTMCA(MS,MN,2)

C The following common block saves the future time
C values (held in /COE32/) for use as the present values
C at the next time-step.
      COMMON/COE32S/QSLIF(MCOM,MS),QSLEF(MCOM,MS),QTMCAF(MCOM,MS,MN)
      COMMON/COE32SV/QSLIP(MCOM,MS),QSLEP(MCOM,MS),QTMCAP(MCOM,MS,MN)

      COMMON/SOLEXT/EXRAD(MS),EXDIR(MS),EXDIF(MS),EXSHAD(MS)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/VERSOL/ipkzon(mzs),ipksf(mzs),ivsolfil,vsol,VTSOL,vsnam

C Daylight coeff specific common block data definition:
      COMMON/DCOEF/NDCFP,NDCFS(MDCFP),DCOEF(MDCFP,MDCFS,MDCFV),
     & IDCFID(MCOM,MLCOM,MDF)

C Commons for summary of solar (for results library) and to save
C future for next timestep.
      common/solsum/q1adjz(2),q1outs(2),q2adjz(2),q2lost(2),q2tmc(2),
     &  q2wall(2),q2rem(2)
      common/solsmf/q1adjf(MCOM),q1outf(MCOM),q2adjf(MCOM),
     &  q2losf(MCOM),q2tmcf(MCOM),q2walf(MCOM),q2remf(MCOM)
      common/solsmp/q1adjp(MCOM),q1outp(MCOM),q2adjp(MCOM),
     &  q2losp(MCOM),q2tmcp(MCOM),q2walp(MCOM),q2remp(MCOM)
      common/ts4/itsitr(msch),itrpas
      
C SHOCC: new common ...
C iblnd - SHOCC blinds corresponding to ESP-r TMCs:
C      0, no; else, a f77 index [1,~] to access
C      SHOCC zone blind data structure
      common/shocc/iblnd(mcom,ms)

C Common for thermophysical property substitution.
      common/BSCTL/ITPREP(MCOM,MTMC),ICZN,IMDB(3),IGLZ(3)

C ADD IN NEW COMMON BLOCKS *****************************
C NSTMCFL flag for each model surface (connection). A non-zero
C   number indicates bi-directional information is available.
C   Limited to 0 or 1 at present.
C NSGALFL indicates which set of optical properties for the particular
C   bi-directional glazing (indicated by NSTMCFL). 
C   Limited to 0 or 1 at present.
C NGNTL number of layers in each TMC type.

C don't need following - set as parameter in MANH,MANV ??
C NGANGS number of angles in each TMC type. Set to 37 at present.

C TMTSOD(MSTMC,MSGAL,MANH,MANV) - outdoor side direct solar trans (direct to direct)

C TMTSOB(MSTMC,MSGAL,MANH,MANV) - outdoor side direct solar trans (direct to diff)

C TMABSO(MSTMC,MSGAL,ME,MANH,MANV) - outdoor side solar absorb for each alternative
C   and each layer and each angle.

C THTSOB(MSTMC,MSGAL) - outdoor side diffuse solar trans (diff to diff)

C TMABSDIF(MSTMC,MSGAL,ME) - outdoor side diffuse solar absorptance for each layer

C Add connection index common block because this is used to identify if
C a particular surface has bi-directional information.
C IZSTOCN - for each zone:surface a pointer to connection index.
      COMMON/C24/IZSTOCN(MCOM,MS)


C First, add a flag whether original (=0) or bi-directional (=1)
C data is available. Check on this flag***********
C IFLAGBI is set to 1 if the model contains bidirectional component; otherwise 0.
C Change IFLAGBI to be an array of ITMC - probably don't need IFLAGBI
C INTVALBI is the interval for the bidirectional data - 5deg at present.
C 
      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)

      COMMON/OPTDAT/NSGALFL(MSTMC),NGNTL(MSTMC),
     &  NGANGS(MSTMC),TMTSOD(MSTMC,MSGAL,MANH,MANV),
     &  TMTSOB(MSTMC,MSGAL,MANH,MANV),TMABSO(MSTMC,MSGAL,ME,MANH,MANV),
     &  THTSOB(MSTMC,MSGAL),TMABSDIF(MSTMC,MSGAL,ME)
     
      logical VTSOL,vsolfl,closea,neglum

      character outs*124,zname*12,zdesc*64,vsnam*72

      DIMENSION QIABS(MS),IANS(MS),IND1(MS),IND2(MS),ANGI(MS),
     &SRADDO(MS),SRADF(MS)
      DIMENSION PSAZI2(MS),PSELV2(MS),IANGBI1(MS),IANGBI2(MS)
      dimension inssur(misur),pinsur(misur)

C Daylight coeff specific
      dimension alt(8)

C Definitions for daylight coefficient patch altitude:
      data alt/6.,18.,30.,42.,54.,66.,78.,90./

      ZAREA=ZOA(ICOMP)
      
C      write (6,*) 'solar ',btimef

C ---------------- INITIALIZE ----------------
C                  ---------- 
C Now zero working variables and set present time-row
C results variables to previous time-step future time-row
C values, skip if iterating (and past first iteration).
      if (ITRPAS.eq.0) then
        q1adjz(1)=q1adjf(icomp)
        q1outs(1)=q1outf(icomp)
        q2adjz(1)=q2adjf(icomp)
        q2lost(1)=q2losf(icomp)
        q2tmc(1) =q2tmcf(icomp)
        q2wall(1)=q2walf(icomp)
        q2rem(1) =q2remf(icomp)
        q1adjp(icomp)=q1adjf(icomp)
        q1outp(icomp)=q1outf(icomp)
        q2adjp(icomp)=q2adjf(icomp)
        q2losp(icomp)=q2losf(icomp)
        q2tmcp(icomp)=q2tmcf(icomp)
        q2walp(icomp)=q2walf(icomp)
        q2remp(icomp)=q2remf(icomp)
      else
        q1adjz(1)=q1adjp(icomp)
        q1outs(1)=q1outp(icomp)
        q2adjz(1)=q2adjp(icomp)
        q2lost(1)=q2losp(icomp)
        q2tmc(1) =q2tmcp(icomp)
        q2wall(1)=q2walp(icomp)
        q2rem(1) =q2remp(icomp)
      endif
      q1adjz(2)=0.0
      q1outs(2)=0.0
      q2adjz(2)=0.0
      q2lost(2)=0.0
      q2tmc(2)=0.0
      q2wall(2)=0.0
      q2rem(2)=0.0
      NC=NCONST(ICOMP)
      DO 10 I=1,NC

C Internal opaque surface absorption
C and internal window loss variables.
        QIABS(I)=0.
        QLOSSD(ICOMP,I)=0.
        QLOSSF(ICOMP,I)=0.
        QLOSSA(ICOMP,I)=0.

C QSOLI and QSOLE, skip if iterating (and past first iteration).
        if (ITRPAS.eq.0) then
          QSOLE(I,1)=QSLEF(ICOMP,I)
          QSOLI(I,1)=QSLIF(ICOMP,I)
          QSLEP(ICOMP,I)=QSLEF(ICOMP,I)
          QSLIP(ICOMP,I)=QSLIF(ICOMP,I)
        else
          QSOLE(I,1)=QSLEP(ICOMP,I)
          QSOLI(I,1)=QSLIP(ICOMP,I)
        endif

        NN=NNDC(ICOMP,I)
        DO 20 II=1,NN

C QTMCA(,,1) relates to present time-row, (,,2) to future
C time-row.
          if (ITRPAS.eq.0) then
            QTMCA(I,II,1)=QTMCAF(ICOMP,I,II)
            QTMCAP(ICOMP,I,II)=QTMCAF(ICOMP,I,II)
          else
            QTMCA(I,II,1)=QTMCAP(ICOMP,I,II)
          endif
          QTMCA(I,II,2)=0.
   20   CONTINUE
   10 CONTINUE

C Initialise internal surface reflection, QIREF to zero.
      QIREF=0.0

C Initialise QD and QF.
      QD=0.0
      QF=0.0

C Initialise total flux entering zone
      QZNTOT=0.0

C Initialise external solar flux for PV materials.
      DO 98 I=1,MS
        EXRAD(I)=0.0
98    CONTINUE

C Initialise 'remaining flux' for diffuse allocation
      RDIFR=0.0

C Initialise window count for insolation file
      nw=0

c Initialise interval for bidirectional data
      INTVALBI=5

C Read in vertical solar from external file if set
C in simulation toggles. Only read for first zone.
      if(VTSOL.and.icomp.eq.1) read(ivsolfil,*) vsol

C ----------------- CALCULATE EXTERNAL INCIDENT SOLAR
C                   ---------------------------------
C Commence computation of the future time-row values:
C first determine the solar angles at this time.
      IF(ICOMP.EQ.1)CALL MZSANG

C---------------IF SUN NOT UP
C               -------------
C Set solar gain to zero for all nodes and jump to end of routine.
      if (ISUNUP.EQ.0) then
        DO 120 I=1,NC
          QSOLI(I,2)=0.
          QSOLE(I,2)=0.
          ITMC=ITMCFL(ICOMP,I)
          IF(ITMC.GE.1) THEN
            NN=NNDC(ICOMP,I)
            DO 130 J=1,NN
              QTMCA(I,J,2)=0.
 130        CONTINUE
          ENDIF
 120    CONTINUE
        goto 2
      endif

C Establish future row solar intensity values: QD & QF.
      CALL MZSINT(ICOMP,QD,QF)

C Solar absorption at external opaque surface + transparent
C multi-layered construction nodes. Loop for each surface.
      DO 3 I=1,NC
        ISRF=I

C If construction is internal then its external opaque solar gain 
C (from an adjacent zone) is handled by MZADJC and the total 
C surface radiation value held in variable ARF.
        IF(IE(ICOMP,ISRF).NE.0)THEN
          SRADDO(ISRF)=0.
          SRADF(ISRF)=0.
        ELSE

C Is surface facing away from the sun so that it can only
C receive diffuse component. IANS=1; yes, self-shaded.
          IANS(ISRF)=0
          CALL MZSFSH(ICOMP,ISRF,IANS(ISRF))

C Calculate the angle of incidence between the sun and surface ISRF,
C ANGI; COS of ANGI, CAI; and the window indices IND1 & IND2 for
C this external construction.
          IF(IANS(ISRF).NE.1)THEN
            CALL MZSCAI(ICOMP,ISRF,CAI,ANGI(ISRF),IND1(ISRF),IND2(ISRF),
     &            PSAZI2(ISRF),PSELV2(ISRF),IANGBI1(ISRF),IANGBI2(ISRF))
            IF(ANGI(ISRF).LT.0..OR.ANGI(ISRF).GT.90.)THEN
               write(outs,1000)ANGI(ISRF)
 1000          format(' MZSLGN error: incidence angle = ',F10.3,'.')
               call edisp(iuout,outs)
               goto 9999
            ENDIF
          ELSE

C Set default, otherwise undefined.
            CAI=0.
            ANGI(ISRF)=90.
            IND1(ISRF)=5
            IND2(ISRF)=6
          ENDIF

C SOLAR PROCESSING: incident comprises direct, sky diffuse
C and ground reflected components. Calculate direct (SRADDO)
C and diffuse (SRADF) on surface ISRF.
          CALL MZSRAD(ICOMP,ISRF,QD,QF,CAI,SRADDO(ISRF),SRADF(ISRF))

C Surface is self-shaded; no direct radiation.
          IF(IANS(ISRF).EQ.1)SRADDO(ISRF)=0.

C Replace calculated values by measured values from file for selected
C surfaces. Use the ratio of direct/diffuse as calculated to divide
C the measured values into direct and diffuse components.
          if(vtsol)then

C Check if it is a selected surface.
            vsolfl=.false.
            do 141 iiii=1,mzs
              if (ipkzon(iiii).eq.icomp.and.
     &            ipksf(iiii).eq.ISRF) vsolfl=.true.
  141       continue
            if(vsolfl)then

C First write out data into a temporary file for checking.
              CALL DAYCLK(IDYP,BTIMEF,42)
              write(42,*) 'zone number',icomp,' surface number',ISRF
              write(42,*)' Gh',QDF,' Dh',QFF
              write(42,*)' ESP-r calculated values on vertical plane:'
              write(42,*)' direct',SRADDO(ISRF),' diffuse',SRADF(ISRF)
              write(42,*)' Measured global vertical solar',vsol
              if(vsol.gt.sradf(ISRF))then
                xxx=sradf(ISRF)+sraddo(ISRF)
                if (xxx.lt.0.00001) xxx=1.0
                radrat=sraddo(ISRF)/xxx
                sraddo(ISRF)=vsol*radrat
                sradf(ISRF)=vsol*(1.-radrat)
              else
                sraddo(ISRF)=0.0
                sradf(ISRF)=vsol
              endif
              write(42,*)'meas. direct',SRADDO(ISRF),
     &                   ' meas. diffuse',SRADF(ISRF)
              write(42,*)'---------------------------------------------'
            endif
          endif
        ENDIF
   3  CONTINUE

C----------------TMC & WINDOW BLIND/SHUTTERS-------------------------
C                ---------------------------
C Now we have the external radiation on each surface.
      DO 4 I=1,NC
        IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.2)THEN
          POO=0.0
        ELSE
          POO=PO(ICOMP,I,IHRF)
        ENDIF

C----------------------BLIND CONTROL - EXTERNAL SURFACES
C                      ---------------------------------
C If external TMC, determine whether blind/shutter active.
        ITMC=ITMCFL(ICOMP,I)
        IBOFOT(ICOMP,I)=0
        ECRAT(ICOMP,I)=1.0
        NBPONT(ICOMP,I)=0

C****************************************************
C If bidirectional, no control possible for the moment.
C****************************************************
 
C Test here if blind/shutter control is on for this TMC.
        IF(ITMC.GT.0)then
          if(IBCMT(ICOMP,ITMC).EQ.1)THEN

C Window blind/shutter in place on this TMC. Test if it is active.
C NP is no. of control periods for this TMC type.
            NP=NBCTMC(ICOMP,ITMC)
            DO 5 K=1,NP
              IT1=IBCST(ICOMP,K,ITMC)
              IT2=IBCFT(ICOMP,K,ITMC)
              IF(IHRF.GT.IT1.AND.IHRF.LE.IT2) THEN
                IF(BACTPT(ICOMP,K,ITMC).LT.-98.0) THEN

C No activation level.
                  IBOFOT(ICOMP,I)=1
                  ECRAT(ICOMP,I)=0.0
                  NBPONT(ICOMP,I)=K
                ELSEIF(NBCTT(ICOMP,K,ITMC).EQ.1) THEN

C External air temperature activation.
                  IF(TF.GT.BACTPT(ICOMP,K,ITMC))THEN
                    IBOFOT(ICOMP,I)=1
                    ECRAT(ICOMP,I)=0.0
                    NBPONT(ICOMP,I)=K
                  ENDIF
                ELSEIF(NBCTT(ICOMP,K,ITMC).EQ.2) THEN

C Internal zone air temperature activation.
                  IF(TFA(ICOMP).GT.BACTPT(ICOMP,K,ITMC))THEN
                    IBOFOT(ICOMP,I)=1
                    ECRAT(ICOMP,I)=0.0
                    NBPONT(ICOMP,I)=K
                  ENDIF
                ELSEIF(NBCTT(ICOMP,K,ITMC).EQ.3) THEN

C Maintain illuminance set point - daylight coefficient method
C with linear interpolation of optical properties:
                  IBOFOT(ICOMP,I)=2
                  NBPONT(ICOMP,I)=K
          
C Calculate sensor illuminance - daylight coefficient method:
C Find which daylight coefficient set to use (use stage 1 i.e.
C maximal visible transmittance stage):
                  NDCP = IBCSUR(ICOMP,ITMC)

C Assume casual gains untouched and establish solar data for correct
C time-row.  For conversion from radians to degrees.
                  CALL MZSINT(ICOMP,QDIR,QDIF)

                  PI = 4.0 * ATAN(1.0)
                  RAD=PI/180.

C Check if any diffuse irradiance and if sun is up - if not do not 
C calculate and set to zero:
                  call eclose(QDIF,0.00,0.01,closea)
                  if(closea.OR.(SALT.LT.0.))then
                    esky  = 0.
                    esun  = 0. 
                    ELLUM = 0.     
                  else

C Direct normal and diffuse horizontal illuminance:
                    call LUMEFF(QDIF,QDIR,SALT,IDYP,skyeff,suneff)
                    esky=QDIF*skyeff
                    esun=QDIR*suneff

C Calculate reference horizontal illuminance:
                    call REFILL(QDIF,QDIR,SALT,SAZI,IDYP,refell)

C Define starting angle for test of sky element comprising sun:
                    ang0=4.0*PI

C Loop through all 145 sky patchs and calculate delta illuminance:
                    sill = 0.0
                    do 333 npatch = 1, 145
                      dill=0.0

C Calculate sky patch reference luminance:
                      call PATLUM(QDIF,QDIR,SALT,SAZI,IDYP,npatch,plv,
     &                  ang)

C Find sky element with minimum angle between sun position and element.
C This identifies number of sky element for calculation of direct
C illuminance contribution:
                      if(ang.lt.ang0)then
                        nelem = npatch
                        ang0 = ang
                      endif

C Calculate luminance (cd/m^2) for given sky patch:
                      plum=plv*esky/refell

C If inconsistency (i.e. error) in irradiance data - plum < 0 set it to 0:
                      if(plum.le.0.0)then
                        if((npatch.ge.1).and.(npatch.le.30))then
                          nzone = 1
                        elseif((npatch.ge.31).and.(npatch.le.60))then
                          nzone = 2
                        elseif((npatch.ge.61).and.(npatch.le.84))then
                          nzone = 3
                        elseif((npatch.ge.85).and.(npatch.le.108))then
                          nzone = 4
                        elseif((npatch.ge.109).and.(npatch.le.126))then
                          nzone = 5
                        elseif((npatch.ge.127).and.(npatch.le.138))then
                          nzone = 6
                        elseif((npatch.ge.139).and.(npatch.le.144))then
                          nzone = 7
                        elseif(npatch.eq.145)then
                          nzone = 8
                        endif                

C Use aproximate solution assuming uniform sky luminance distribution
C to estimate sky patch luminance:
                        plum = esky/(2.0*PI*sin(alt(nzone)*rad))
                        neglum = .true.
                      else
                        neglum = .false.
                      endif

C Calculate delta illuminance at sensor from given sky patch:
                      dill=DCOEF(NDCP,1,npatch)*plum*(2.0*PI/145.0)
                      sill = sill + dill
333                 continue

C Give warning about calculated negative luminance:
                    if(neglum)then
                      write (outs,'(a)')
     &     ' Sky patch luminance < = 0. Using uniform sky aproximation'
                      call edisp(iuout,outs)
                 write (outs,'(a,I3,a,f6.1,a,f6.1,a,f4.1)')' Day ',IDYP,
     &           ' Direct ',QDIR,' Diffuse ',QDIF,' Sun altitude ',SALT
                      call edisp(iuout,outs)
                    endif

C Check if any direct sun - if yes then calculate its contribution
C into sensor illuminance:
                    call eclose(QDIR,0.00,0.01,closea)
                    if(closea)then
                      dircon=0.0
                    else
                      dircon=DCOEF(NDCP,1,nelem)*esun
                    endif
                    ELLUM = sill + dircon
                  endif

C Calculate necessary visible transmittance to maintain lux set point.
C This works only for simple cases i.e. sensor illuminance proportional
C to the visible transmittance of TMC - case with only one TMC type in
C thermal zone:
                  CALL eclose(ELLUM,0.00,0.01,closea)
                  IF(closea)then
                    TVT = TVTR(ICOMP,ITMC)
                  ELSE
                    TVT = (BACTPT(ICOMP,K,ITMC)/ELLUM)*TVTR(ICOMP,ITMC)
                    IF(TVT.GE.TVTR(ICOMP,ITMC))THEN
                      TVT = TVTR(ICOMP,ITMC) 
                    ELSEIF(TVT.LE.TVTR2(ICOMP,ITMC,K))THEN
                      TVT = TVTR2(ICOMP,ITMC,K) 
                    ENDIF
                  ENDIF 
           
C Calculate linear property change ratio for varying other
C properties i.e. transmittance, absorptance and reflectance:
                  ECRAT(ICOMP,I) = (TVT - TVTR2(ICOMP,ITMC,K))/
     &                      (TVTR(ICOMP,ITMC)-TVTR2(ICOMP,ITMC,K))
                ELSEIF(NBCTT(ICOMP,K,ITMC).EQ.4
     &                 .AND.IBLND(ICOMP,I).GT.0) THEN
     
C LIGHTSWITCH manual blind control. First set the resulting direct
C transmission, td [W/m²],through TMC as if ibofot=0 (i.e. default optics),
                   xx1 = tmct(icomp,itmc,ind1(i))
                   if(ind2(i).eq.6) then
                      xx2 = 0.
                   else
                      xx2 = tmct(icomp,itmc,ind2(i))
                   endif
                   
                   zero=0.
                   call mzwinp(xx1,1.,xx2,zero,ind1(i),angi(i),xd,dummy)
                   td = sraddo(i)*(1.- poo)*xd

C We now have direct transmission, td, through TMC.
                   call updateblind(icomp,iblnd(icomp,i),angi(i),
     &                              td,istate,btimep,btimef)
                   
C If istate not equal to 0, then blinds are down.
                   if(istate.ne.0) then
                      ibofot(icomp,i)=1
                      ecrat(icomp,i)=0.
                      nbpont(icomp,i)=k
                   endif
                
                ELSE

C Radiation activation; check which surface the sensor is on;
C - if IBCSUR()=0, then current surface.
C POO/POO2 contains shading factor on corresponding opaque surface.
                  ISUR=IBCSUR(ICOMP,ITMC)
                  IF(ISUR.EQ.0)THEN
                    RAD=(SRADDO(I)*(1.-POO))+SRADF(I)
                  ELSE
                    IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.2)THEN
                      POO2=0.0
                    ELSE
                      POO2=PO(ICOMP,ISUR,IHRF)
                    ENDIF
                    RAD=(SRADDO(ISUR)*(1.-POO2))+SRADF(ISUR)
                  ENDIF
                  IF(RAD.GT.BACTPT(ICOMP,K,ITMC))THEN
                    IBOFOT(ICOMP,I)=1
                    NBPONT(ICOMP,I)=K
                  ENDIF
                ENDIF
              ENDIF
   5        CONTINUE
          endif
        ENDIF
    4 CONTINUE
              
    
C Update thermophysical properties here - we know the control state 
C so call bcl99 to update thermophysical props of controlled windows.
      ICALL=0
      DO 66 I=1,NC
C        write (6,'(a,2i4)') 'Blind state: ',IBOFOT(ICOMP,I),
C     &                                       NBPONT(ICOMP,I)
        ITMC=ITMCFL(ICOMP,I)
        if(ITMC.GT.0)then
          if(ITPREP(ICOMP,ITMC).gt.0)then
            if(IBCMT(ICOMP,ITMC).EQ.1) then

C Set ICALL to one because optical control is used with this TMC type
C and if IBOFOT is not set ICALL remains at one to signal that no
C change needs to be applied <<JWH guess at what the logic means>>.
              ICALL=1
              ICZN=ICOMP
              IWC=0
              if (IBOFOT(ICOMP,I).EQ.1) then
              
C Glazing in controlled state so switch to alternative property.
C << is the IWC limit of 3 a bottleneck for solar optical control >>
C << use? and what about the case of ITPREP being zero >>
                ICALL=2
                IWC=IWC+1
                if (IWC.lt.4) then
                  IMDB(IWC)=ITPREP(ICOMP,ITMC)
                  IGLZ(IWC)=I
                else
                  write (33,*) 
     &               'Too many controlled windows! Ignoring surface ',I
                endif
              endif
            endif
          endif
        endif
 66   continue
      if (ICALL.gt.0) call BCL99(ICALL)

C ---------------CALCULATE QSOLE FOR OPAQUE SURFACES
C                -----------------------------------
C QSOLE: external surface solar radiation absorbed by opaque surfaces.
      DO 6 I=1,NC
        IF(ITMCFL(ICOMP,I).GE.1) THEN

C TMC.
          QSOLE(I,2)=0.
        ELSEIF(IE(ICOMP,I).NE.0) THEN

C Not external construction.
          QSOLE(I,2)=0.
        ELSE
          IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.2)THEN
            QSOLE(I,2)=(SRADDO(I)+SRADF(I))*AE(ICOMP,I)
          ELSE
            QSOLE(I,2)=SRADDO(I)*AE(ICOMP,I)*(1.-PO(ICOMP,I,IHRF))+
     &      SRADF(I)*AE(ICOMP,I)
          ENDIF
          EXSHAD(I)=PO(ICOMP,I,IHRF)
          EXRAD(I)=SRADDO(I)*(1.-EXSHAD(I))+SRADF(I)
          EXDIR(I)=SRADDO(I)*(1.-EXSHAD(I))
          EXDIF(I)=SRADF(I)
        ENDIF
   6  CONTINUE

C ----------------CALCULATE QTMCA FOR TMC
C                 -----------------------
C QTMCA: external surface solar radiation absorbed by TMC's
      DO 7 I=1,NC

C Set a local flag if bidirectional data available for this surface.
        IBIDIR=NSTMCFL(IZSTOCN(ICOMP,I))

        if (IE(ICOMP,I).eq.0) then
          ITMC=ITMCFL(ICOMP,I)
          IF (ITMC.GE.1) THEN
            NE=NELTS(ICOMP,I)
            KP=NBPONT(ICOMP,I)
            INODE=1

            DO 40 J=1,NE
              NNOD=NNDL(ICOMP,I,J)
              FNNOD=FLOAT(NNOD)

C Interpolate on element absorptance; for direct (XX?)
C and diffuse (YY?) beams separately.
C IANS=0 implies NOT self-shaded, i.e. there is direct radiation.
C If IBOFOT()=1, then blind/shutter is ON - alternative values set.
C If IBOFOT()=2, then linear interpolation is ON - alternative values set.
C First, deal with direct radiation.
C**************************
C Again, control not dealt with at present for bidirectional case.
C************************
              IF(IANS(I).EQ.0)THEN
                IF(IBOFOT(ICOMP,I).EQ.2)THEN
                  AMIN=TMCA2(ICOMP,ITMC,J,IND1(I),KP)
                  AMAX=TMCA(ICOMP,ITMC,J,IND1(I))
                  XX1=AMIN + (AMAX - AMIN)*ECRAT(ICOMP,I)
                ELSEIF(IBOFOT(ICOMP,I).EQ.1)THEN
                  XX1=TMCA2(ICOMP,ITMC,J,IND1(I),KP)
                ELSE
                  IF(IBIDIR.EQ.0) THEN
                   XX1=TMCA(ICOMP,ITMC,J,IND1(I))
                  ELSE
C Bidirectional data available.
C XX1 is the absorptance for those available array elements that
C are less than the actual solar azimuth and elevation angles.
                   XX1=TMABSO(IBIDIR,1,J,IND1(I),IND2(I))
                  ENDIF
                ENDIF

                IF(IBIDIR.EQ.0.AND.IND2(I).EQ.6) THEN
                  XX2=0.0
                ELSE
                  IF(IBOFOT(ICOMP,I).EQ.2)THEN
                    AMIN=TMCA2(ICOMP,ITMC,J,IND2(I),KP)
                    AMAX=TMCA(ICOMP,ITMC,J,IND2(I))
                    XX2=AMIN + (AMAX - AMIN)*ECRAT(ICOMP,I)
                  ELSEIF(IBOFOT(ICOMP,I).EQ.1)THEN
                    XX2=TMCA2(ICOMP,ITMC,J,IND2(I),KP)
                  ELSEIF(IBIDIR.EQ.0)THEN
                    XX2=TMCA(ICOMP,ITMC,J,IND2(I))
                  ELSE
C Bidirectional case.
C Note: if angle is +90, set XX2=XX1 etc.
                    IF(IND1(I).eq.MANH)then
                     XX2=XX1
                     ELSE
                     XX2=TMABSO(ITMC,1,J,IND1(I)+1,IND2(I))
                    ENDIF
                    IF(IND2(I).eq.MANV)then
                     XX3=XX1
                    ELSE
                     XX3=TMABSO(ITMC,1,J,IND1(I),IND2(I)+1)
                    ENDIF
                    IF(IND1(I).eq.MANH)then
                     XX4=XX3
                    ELSEIF(IND2(I).eq.MANV)then
                     XX4=XX2
                    ELSE
                     XX4=TMABSO(ITMC,1,J,IND1(I)+1,IND2(I)+1)
                    ENDIF
                  ENDIF
                ENDIF
              ELSE
C Self-shaded.
                XX1=0.0
                XX2=0.0
                XX3=0.0
                XX4=0.0
              ENDIF

C Now deal with diffuse absorbed radiation- not required for bidirectional
C data as the diffuse transmittance is specified.
              IF(IBIDIR.EQ.0)THEN
                IF(IBOFOT(ICOMP,I).EQ.2)THEN
                  AMIN=TMCA2(ICOMP,ITMC,J,2,KP)
                  AMAX=TMCA(ICOMP,ITMC,J,2)
                  YY1=AMIN + (AMAX - AMIN)*ECRAT(ICOMP,I)
                  AMIN=TMCA2(ICOMP,ITMC,J,3,KP)
                  AMAX=TMCA(ICOMP,ITMC,J,3)
                  YY2=AMIN + (AMAX - AMIN)*ECRAT(ICOMP,I)        
                ELSEIF(IBOFOT(ICOMP,I).EQ.1)THEN
                  YY1=TMCA2(ICOMP,ITMC,J,2,KP)
                  YY2=TMCA2(ICOMP,ITMC,J,3,KP)
                ELSEIF(IBIDIR.EQ.0) THEN    
                  YY1=TMCA(ICOMP,ITMC,J,2)
                  YY2=TMCA(ICOMP,ITMC,J,3)
                ENDIF
              ENDIF
              ZERO=0.

C Now interpolate for the direct and diffuse absorbed radiation.
            IF(IBIDIR.EQ.0) THEN
C Call MZWINP to interpolate between two values.
              CALL MZWINP(XX1,1.,XX2,ZERO,IND1(I),ANGI(I),ABD,DUMMY)
              CALL MZWINP(YY1,1.,YY2,ZERO,2,51.0,ABF,DUMMY)
            ELSE
C Call MZWINP2 to interpolate in two dimensions for bidirectional data.
              CALL MZWINP2(XX1,XX2,XX3,XX4,IANGBI1(I),IANGBI2(I),
     &                     PSAZI2(I),PSELV2(I),ABD)
C Get diffuse absorptance.
              ABF=TMABSDIF(IBIDIR,1,J)
            ENDIF
              IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.2)THEN
                POO=0.
              ELSE
                POO=PO(ICOMP,I,IHRF)
              ENDIF
              DAB=SRADDO(I)*(1.-POO)*ABD
              FAB=SRADF(I)*ABF
C              write (6,'(a,i4,6f8.5)') 'Nodal data: ',IBOFOT(ICOMP,I),
C     &                                        XX1,XX2,ABD,YY1,YY2,ABF

              EXSHAD(I)=PO(ICOMP,I,IHRF)
              EXRAD(I)=SRADDO(I)*(1.-EXSHAD(I))+SRADF(I)
              EXDIR(I)=SRADDO(I)*(1.-EXSHAD(I))
              EXDIF(I)=SRADF(I)
              
              QTMCA(I,INODE,2)=QTMCA(I,INODE,2)+(DAB+FAB)/(FNNOD*2.)
              DO 45 JJ=2,NNOD
                INODE=INODE+1
                QTMCA(I,INODE,2)=QTMCA(I,INODE,2)+(DAB+FAB)/FNNOD
   45         CONTINUE
              INODE=INODE+1
              QTMCA(I,INODE,2)=QTMCA(I,INODE,2)+(DAB+FAB)/(FNNOD*2.)
   40       CONTINUE
          ENDIF
        endif
    7 CONTINUE

C -----------------------TMC
C                        ---
C Process transmission through transparent multi-layered
C constructions, but only if surface is of type 0 or 3.
C NB Actual transmissions need not be computed for a type 3
C surface since this has already been done when the adjacent
C zone was processed. Results held in common ADJC2.
      DO 30 I=1,NC

C Set a local flag if bidirectional data available for this surface.
        IBIDIR=NSTMCFL(IZSTOCN(ICOMP,I))

        ITMC=ITMCFL(ICOMP,I)
        if (IE(ICOMP,I).ge.0.and.ITMC.gt.0) then

C ---------------PROCESS FOR EXTERNAL TMC
C                ------------------------
C Transparent multi-layered construction: only direct transmittance
C required for direct (XX?) and diffuse (YY?) beams.
C IANS=0 implies NOT self-shaded, i.e. there is direct radiation.
C If IBOFOT()=1, then blind/shutter is ON - alternative values set.
C If IBOFOT()=2, then linear interpolation is ON - alternative values set.
          IF(IE(ICOMP,I).EQ.0) THEN
            KP=NBPONT(ICOMP,I)
            IF(IANS(I).EQ.0)THEN
              IF(IBOFOT(ICOMP,I).EQ.2)THEN
                TMIN=TMCT2(ICOMP,ITMC,IND1(I),KP)
                TMAX=TMCT(ICOMP,ITMC,IND1(I))
                XX1=TMIN + (TMAX - TMIN)*ECRAT(ICOMP,I)  
              ELSEIF(IBOFOT(ICOMP,I).EQ.1)THEN
                XX1=TMCT2(ICOMP,ITMC,IND1(I),KP)
              ELSE
                 IF(IBIDIR.EQ.0) THEN
                   XX1=TMCT(ICOMP,ITMC,IND1(I))
                 ELSE
C Bidirectional case for transmission - see similar treatment for absorption above.
C XX? is the direct-direct component; YY? is the direct-diffuse component
                   XX1=TMTSOD(IBIDIR,1,IND1(I),IND2(I))
                   YY1=TMTSOB(IBIDIR,1,IND1(I),IND2(I))
                 ENDIF
              ENDIF

              IF(IBIDIR.EQ.0.AND.IND2(I).EQ.6) THEN
                XX2=0
              ELSE
                IF(IBOFOT(ICOMP,I).EQ.2)THEN
                  TMIN=TMCT2(ICOMP,ITMC,IND2(I),KP)
                  TMAX=TMCT(ICOMP,ITMC,IND2(I))
                  XX2=TMIN + (TMAX - TMIN)*ECRAT(ICOMP,I)
                ELSEIF(IBOFOT(ICOMP,I).EQ.1)THEN
                  XX2=TMCT2(ICOMP,ITMC,IND2(I),KP)
                ELSEIF(IBIDIR.EQ.0)THEN
                  XX2=TMCT(ICOMP,ITMC,IND2(I))
                ELSE
C Bidirectional case.
                  IF(IND1(I).eq.MANH)then
                    XX2=XX1
                    YY2=YY1
                  ELSE
                    XX2=TMTSOD(IBIDIR,1,IND1(I)+1,IND2(I))
                    YY2=TMTSOB(IBIDIR,1,IND1(I)+1,IND2(I))
                  ENDIF
                  IF(IND2(I).eq.MANV)then
                    XX3=XX1
                    YY3=YY1
                  ELSE
                    XX3=TMTSOD(IBIDIR,1,IND1(I),IND2(I)+1)
                    YY3=TMTSOB(IBIDIR,1,IND1(I),IND2(I)+1)
                  ENDIF
                  IF(IND1(I).eq.MANH)then
                    XX4=XX3
                    YY4=YY3
                  ELSEIF(IND2(I).eq.MANV)then
                    XX4=XX2
                    YY4=YY2
                  ELSE
                    XX4=TMTSOD(IBIDIR,1,IND1(I)+1,IND2(I)+1)
                    YY4=TMTSOB(IBIDIR,1,IND1(I)+1,IND2(I)+1)
                  ENDIF
                ENDIF
              ENDIF
            ELSE
C Self-shaded.
              XX1=0.0
              XX2=0.0
              XX3=0.0
              XX4=0.0
              YY1=0.0
              YY2=0.0
              YY3=0.0
              YY4=0.0
            ENDIF
C Now for diffuse transmitted radiation - not required for bidirectional
C data as the diffuse transmittance is specified.
            IF(IBIDIR.EQ.0)THEN
              IF(IBOFOT(ICOMP,I).EQ.2)THEN
                TMIN=TMCT2(ICOMP,ITMC,2,KP)
                TMAX=TMCT(ICOMP,ITMC,2)
                YY1=TMIN + (TMAX - TMIN)*ECRAT(ICOMP,I)
                TMIN=TMCT2(ICOMP,ITMC,3,KP)
                TMAX=TMCT(ICOMP,ITMC,3)
                YY2=TMIN + (TMAX - TMIN)*ECRAT(ICOMP,I)
              ELSEIF(IBOFOT(ICOMP,I).EQ.1)THEN
                YY1=TMCT2(ICOMP,ITMC,2,KP)
                YY2=TMCT2(ICOMP,ITMC,3,KP)
              ELSEIF(IBIDIR.EQ.0) THEN
                YY1=TMCT(ICOMP,ITMC,2)
                YY2=TMCT(ICOMP,ITMC,3)
              ENDIF
            ENDIF
            ZERO=0.
C Now interpolate for the direct and diffuse transmitted radiation.
            IF(IBIDIR.EQ.0) THEN
C Call MZWINP to interpolate between two values.
              CALL MZWINP(XX1,1.,XX2,ZERO,IND1(I),ANGI(I),XD,DUMMY)
              CALL MZWINP(YY1,1.,YY2,ZERO,2,51.0,XF,DUMMY)
            ELSE
C Call MZWINP2 to interpolate in two dimensions for bidirectional data.
              CALL MZWINP2(XX1,XX2,XX3,XX4,IANGBI1(I),IANGBI2(I),
     &                     PSAZI2(I),PSELV2(I),XD)
              CALL MZWINP2(YY1,YY2,YY3,YY4,IANGBI1(I),IANGBI2(I),
     &                     PSAZI2(I),PSELV2(I),DTOD)
C Get diffuse transmission from common block.
              XF=THTSOB(1,1)
            ENDIF

            TFF=SRADF(I)*SNA(ICOMP,I)*XF
C For bidirectional data, add in the direct-diffuse component.
            IF(IBIDIR.NE.0)TFF=TFF+SRADF(I)*SNA(ICOMP,I)*DTOD 
  
            IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.2)THEN
              POO=0.
            ELSE
              POO=PO(ICOMP,I,IHRF)
            ENDIF
            TD=SRADDO(I)*SNA(ICOMP,I)*(1.-POO)*XD

            QZNTOT=QZNTOT+TD+TFF
            q1outs(2)=q1outs(2)+td+tff
          ENDIF

C-----------------PROCESS FOR TYPE 3 SURFACE TMC
C                 ------------------------------
C Set TD, TFF for a type 3 surface (internal surface with adjacent zone).
          IF(IE(ICOMP,I).GT.0) THEN
            TD=DIRT(ICOMP,I)*SNA(ICOMP,I)
            TFF=DIFT(ICOMP,I)*SNA(ICOMP,I)

            QZNTOT=QZNTOT+TD+TFF
            q1adjz(2)=q1adjz(2)+td+tff

            DIRT(ICOMP,I)=0.0
            DIFT(ICOMP,I)=0.0
            AIRT(ICOMP,I)=0.0
          ENDIF
         
C------------PROCESS INTERNAL SURFACES
C            -------------------------
C Now we know the solar radiation (direct (TD) and diffuse (TFF))
C entering the zone through TMC.

C Determine the solar energy absorbed at internal opaque
C surfaces and accumulate the corresponding flux reflections.
C Include effect of solar flux lost from system through
C external window re-transmission and calculate flux loss to
C adjacent zones through internal windows.
C Note different treatments depending on the
C availability of internal surface insolation time-series.

C JJ4=1 if insolation data is available in pre-calculated database.
          IF(IGCI(ICOMP,I).EQ.0)THEN
            JJ4=0
          ELSE
            JJ4=1
            nw=nw+1
          ENDIF

C IFLG1=0 if we use insolation data, =1 if use default insolation as
C specified in the geometry file.
C Set the number of insolated surfaces for use later.
          IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.1.OR.JJ4.EQ.0)THEN
            IFLG1=1
            ksurs=2
          ELSE
            IFLG1=0
            ksurs=misur
          ENDIF
          IF(IFLG1.EQ.1)THEN

C Default insolation distribution type 3 - treat direct as diffuse.
            IF(NDP(ICOMP).EQ.3)THEN
              inssur(1)=0
              inssur(2)=0
              QIREF=QIREF+TD
            ELSE

C Default insolation distribution types 1&2; determine insolated 
C surfaces and split.
              inssur(1)=IDPN(ICOMP,1)
              inssur(2)=IDPN(ICOMP,2)
              inssur(3)=IDPN(ICOMP,3)
              IF(inssur(2).EQ.0)THEN
                ZA=SNA(ICOMP,inssur(1))
              ELSE
                ZA=SNA(ICOMP,inssur(1))+SNA(ICOMP,inssur(2))
              ENDIF
              IF(inssur(2).GT.0)THEN

C There are two default insolated surfaces.
                pinsur(1)=SNA(ICOMP,inssur(1))/ZA
                pinsur(2)=SNA(ICOMP,inssur(2))/ZA
              ELSE

C There is one insolated surface.
                pinsur(1)=1.
                pinsur(2)=0.
              ENDIF
            ENDIF
          ELSE

C Insolation distribution data to be used, determine insolated 
C surfaces and split.
            do 51 kk=1,misur
              inssur(kk)=insst(icomp,nw,ihrf,kk)
              pinsur(kk)=pinsst(icomp,nw,ihrf,kk)
   51       continue
          ENDIF

C  ---------------------DIRECT TRANSMITTED COMPONENT
C                       ----------------------------
C Assign the direct component absorbed by opaque surface or
C transmitted through the TMC.
C NB for default insolation distribution type 3, inssur(1)=inssur(2)=0.
          do 60 k=1,ksurs
            ks=inssur(k)
            pro=pinsur(k)
            if (KS.gt.0.and.PRO.gt.0.0001) then

C Compute direct radiation incident on the surface 'KS'.
              PAREA=SNA(ICOMP,KS)
              if(TD.gt.0.001)then
                X1=TD*PRO/PAREA
              else
                X1=0.0
              endif

C Compute opaque surface absorption or transparent
C multi-layered construction nodal absorptions and
C transmissions as appropriate and total surface
C reflection for surface 'KS'.
              if (ITMCFL(ICOMP,KS).EQ.0) then

C A) OPAQUE SURFACE
                QIABS(KS)=QIABS(KS)+AI(ICOMP,KS)*X1
                QIREF=QIREF+(1.-AI(ICOMP,KS))*PAREA*X1
                q2wall(2)=q2wall(2)+x1*PAREA*AI(ICOMP,KS)
              else

C B) TMC - Call MZTMCA to get transmission and absorptions.
                CALL MZTMCA(ICOMP,KS,X1,XF,ABTOT)
                QIREF=QIREF+(1.-ABTOT-XF)*PAREA*X1
                if (IE(ICOMP,KS).GT.0) then
                  QLOSSD(ICOMP,KS)=QLOSSD(ICOMP,KS)+X1*XF
                  q2adjz(2)=q2adjz(2)+x1*PAREA*XF
                else
                  q2lost(2)=q2lost(2)+x1*PAREA*XF
                endif
                q2tmc(2)=q2tmc(2)+ABTOT*X1*PAREA
              endif
            endif
   60     continue

C-----------------DIFFUSE TRANSMITTED COMPONENT
C                 -----------------------------
C Now assign window transmitted diffuse radiation to all surfaces
C excluding the surface containing the window.

C X2 is the window transmitted diffuse radiation.
          X2=TFF/(ZAREA-SNA(ICOMP,I))
          DO 80 K=1,NC
            if (K.ne.I) then
              if (ITMCFL(ICOMP,K).EQ.0) then

C OPAQUE SURFACE
                QIABS(K)=QIABS(K)+AI(ICOMP,K)*X2
                QIREF=QIREF+(1.-AI(ICOMP,K))*SNA(ICOMP,K)*X2
                q2wall(2)=q2wall(2)+x2*SNA(ICOMP,K)*AI(ICOMP,K)
              else

C TRANSPARENT SURFACE - Assign absorptances and transmittance.
                CALL MZTMCA(ICOMP,K,X2,XF,ABTOT)
                QIREF=QIREF+(1.-ABTOT-XF)*SNA(ICOMP,K)*X2
                IF(IE(ICOMP,K).GT.0)THEN
                  QLOSSF(ICOMP,K)=QLOSSF(ICOMP,K)+(X2*XF)
                  q2adjz(2)=q2adjz(2)+x2*SNA(ICOMP,K)*XF
                ELSE
                  q2lost(2)=q2lost(2)+x2*SNA(ICOMP,K)*XF
                ENDIF
                q2tmc(2)=q2tmc(2)+ABTOT*X2*SNA(ICOMP,K)
              ENDIF
            ENDIF
   80     CONTINUE
        ENDIF
   30 CONTINUE

C  Compute the internal reflection diffuse radiation per m^2.
      RDIFR=QIREF/ZAREA

C --------------------REDISTRIBUTE REMAINING DIFFUSE
C                     ------------------------------
C Redistribute remaining diffuse RDIFR.
C Iterate on this next section; increase KKK if necessary.
      DO 102 KKK=1,15

C Do not loop if RDIFR is below a certain level.
C Level set at 1% of incoming flux, or 0.1 W/m^2.
        IF(RDIFR.LT.0.1.OR.RDIFR.LT.((QZNTOT/ZAREA)*0.01))goto 102
        RREF=0.0

        DO 101 I=1,NC
          IF(ITMCFL(ICOMP,I).EQ.0)THEN

C OPAQUE SURFACE
            QIABS(I)=QIABS(I)+AI(ICOMP,I)*RDIFR
            RREF=RREF+(1.-AI(ICOMP,I))*SNA(ICOMP,I)*RDIFR
            q2wall(2)=q2wall(2)+RDIFR*SNA(ICOMP,i)*AI(ICOMP,i)
          ELSE

C TRANSPARENT SURFACE - Assign absorptances and transmittance.
            CALL MZTMCA(ICOMP,I,RDIFR,XF,ABTOT)
            RREF=RREF+(1.-ABTOT-XF)*SNA(ICOMP,I)*RDIFR
            IF(IE(ICOMP,i).GT.0)THEN
              QLOSSF(ICOMP,I)=QLOSSF(ICOMP,I)+(RDIFR*XF)
              q2adjz(2)=q2adjz(2)+rdifr*SNA(ICOMP,i)*XF
            else
              q2lost(2)=q2lost(2)+rdifr*SNA(ICOMP,i)*XF
            ENDIF
            q2tmc(2)=q2tmc(2)+ABTOT*rdifr*SNA(ICOMP,i)
          ENDIF
  101   CONTINUE
        RDIFR=RREF/ZAREA
  102 CONTINUE

C For any diffuse remaining at this stage output a warning if it
C exceeds 2% of the incoming radiation to the zone, or a threshold
C of 1.0W/m^2, whichever is larger.
C Redistribute remaining flux to opaque surfaces on an
C area/absorptivity basis and continue.

C Compute area/absorptivity ratio.
      SUMA=0.
      QWARN=(QZNTOT/ZAREA)*0.02
      DO 109 I=1,NC
        IF(ITMCFL(ICOMP,I).LE.0)THEN
          SUMA=SUMA+AI(ICOMP,I)
        ENDIF
  109 CONTINUE
      AMEAN=SUMA/FLOAT(NC)
      IF(AMEAN.LE.0.0)AMEAN=0.1
      IF(RDIFR.GT.1.0.AND.RDIFR.GT.QWARN)THEN
        call edisp(iuout,' Routine mzslgn of solar.f')
        write(outs,'(A,I3,A,I4,A,I3)')' Zone', ICOMP,' Day',IDYF,
     &                                '  Hour (future)',IHRF
        call edisp(iuout,outs)
        write(outs,'(A,F5.2,A)')' WARNING! - Remaining diffuse (',
     &                RDIFR,' W/m^2) distributed on area/abs. basis'
        call edisp(iuout,outs)
      ENDIF

C----------------CALC INTERNAL SURFACE ABSORPTIONS
C                ---------------------------------
C Compute QSOLI for each internal opaque surface.
      DO 110 I=1,NC
        QSOLI(I,2)=0.
        IF(ITMCFL(ICOMP,I).LE.0)THEN
           QSOLI(I,2)=QIABS(I)+(RDIFR*AI(ICOMP,I))/AMEAN
        ENDIF
  110 CONTINUE

C---------------------PREPARE FOR NEXT TIMESTEP
C                     -------------------------
C Save future time values for use at next time step.
 2    DO 140 I=1,NC
        QSLEF(ICOMP,I)=QSOLE(I,2)
        QSLIF(ICOMP,I)=QSOLI(I,2)
        DIRT(ICOMP,I)=0.0
        DIFT(ICOMP,I)=0.0
        AIRT(ICOMP,I)=0.0

        NN=NNDC(ICOMP,I)
        DO 150 J=1,NN
          QTMCAF(ICOMP,I,J)=QTMCA(I,J,2)
  150   CONTINUE
  140 CONTINUE

C Energy fluxes of solar that has penetrated zone, then save these
C for possible averaging in MZLS3.
      q2rem(2)=rdifr*zarea
      q1tot=q1outs(2)+q1adjz(2)
      q2tot=q2tmc(2)+q2wall(2)+q2rem(2)
C      write(6,*) 'solsum solar ',BTIMEF,ICOMP,q1adjz(2),q1outs(2),
C     &   q2adjz(2),q2lost(2),q2tmc(2),q2wall(2),q2rem(2)
      q1adjf(icomp)=q1adjz(2)
      q1outf(icomp)=q1outs(2)
      q2adjf(icomp)=q2adjz(2)
      q2losf(icomp)=q2lost(2)
      q2tmcf(icomp)=q2tmc(2)
      q2walf(icomp)=q2wall(2)
      q2remf(icomp)=q2rem(2)

C-------------------------------TRACE
C                               -----
C Trace output ?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF.OR.
     &   IZNTRC(ICOMP).NE.1)goto 9999
      write(outs,'(A,I4,A,A)')' Subroutine MZSLGN Trace output ',ICNT,
     &  ' Zone ',zname(ICOMP)
      call edisp(itu,outs)
      ICNT=ICNT+1
      CALL DAYCLK(IDYP,BTIMEF,ITU)

      call edisp(itu,' All output W/m^2')
      call edisp(itu,' ')

C Output solar intensity details.
      write(outs,9993)SAZI,SALT
 9993 format( ' Azimuth (deg)=',F6.1,', Altitude (deg)=',F6.1)
      call edisp(itu,outs)
      write(outs,99931)QD,QF
99931 format( ' Direct normal intensity =',F6.1,
     &        ', Diffuse horizontal intensity =',F6.1)
      call edisp(itu,outs)
      call edisp(itu,' ')

C Output QSOLE and QSOLI for each construction if sun up.
      if(SALT.lt.-1.0)goto 9992
      call edisp(itu,' Surface External   Internal    TMC nodal  ')
      call edisp(itu,'         Solar      Solar       absorptions')
      call edisp(itu,'         Absorption Absorption  (outside>inside)')
      DO 9991 I=1,NC
        ITMC=ITMCFL(ICOMP,I)
        if(ITMC.LE.0)then
          write(outs,'(I8,2F11.2)')I,QSOLE(I,2),QSOLI(I,2)
          call edisp(itu,outs)
        else
          NN=NNDC(ICOMP,I)
          if(NN.le.10)then
            write(outs,'(I8,2F11.2,10F6.2)')I,QSOLE(I,2),QSOLI(I,2),
     &       (QTMCA(I,J,2),J=1,NN)
            call edisp(itu,outs)
          elseif(NN.gt.10)then
            write(outs,'(I8,2F11.2,10F6.2)')I,QSOLE(I,2),QSOLI(I,2),
     &       (QTMCA(I,J,2),J=1,10)
            call edisp(itu,outs)
            write(outs,'(30X,20F6.2)')(QTMCA(I,J,2),J=11,NN)
            call edisp(itu,outs)
          endif
        endif
 9991 CONTINUE
 9992 continue


C Output computed results.
      write(outs,9996)icomp
 9996 format(' Solar energy distribution for zone ',I2,' (Watts)')
      call edisp(itu,' ')
      call edisp(itu,outs)
      write(outs,'(8X,A)')
     &  '|    Sources    |    Losses     |   Absorbed via  |'
      call edisp(itu,outs)
      write(outs,99962)q1adjz(2),q2adjz(2),q2tmc(2)
99962 format(8X,'|intrn',F10.2,'|intrn',F10.2,'|Trn mlc',F10.2,'|')
      call edisp(itu,outs)
      write(outs,99963)q1outs(2),q2lost(2),q2wall(2)+q2rem(2)
99963 format(8X,'|extrn',F10.2,'|extrn',F10.2,'|Qpq mlc',F10.2,'|')
      call edisp(itu,outs)
      write(outs,99966)q1tot,q2adjz(2)+q2lost(2),q2tot
99966 format(' totals |',5X,F10.2,'|',5X,F10.2,'|',7X,F10.2,'|')
      call edisp(itu,outs)

 9999 RETURN
      END

c ******************** MZSANG ********************
c MZSANG computes the solar azimuth and altitude angles
c at the current time-step future time-row.   The 'ISUNUP'
c variable determines whether the sun is up (=1) or down
c (=0).   The solar angles are computed relative to local
c mean time (Greenwich is the reference time zone for
c Britain).

      SUBROUTINE MZSANG
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC8/SLAT,SLON

      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/SUNPOS/SAZI,SALT,ISUNUP

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      character outs*124,zname*12,zdesc*64
      logical close

      ISUNUP=1
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      DAY=FLOAT(IDYP)
      IF(IHRF.EQ.1)DAY=FLOAT(IDYF)
      HOUR=BTIMEF

C Calculate equation of time. In ESP-r before 21 June 2005.
C      A=1.978*DAY-160.22
C      B=0.989*DAY-80.11
C      EQT=0.1645*ESIND(A)-0.1255*ECOSD(B)-0.025*ESIND(B)

C Compute declination. In ESP-r before 21 June 2005; from Duffie and Beckmann).
C      A=280.1+0.9863*DAY
C      DEC=23.45*ESIND(A)

C Get month and day.
      IDAY=INT(DAY)
      CALL EDAYR(IDAY,IDY,IMT)

C Calculate declination (DEC) and equation of time (EQT).
C Use equations published in Muneer (Solar Radiation and Daylight Models
C for the Energy Efficient Design of Buildings, Architectural Press, 1997).
C Equations are those of Yallop 1992. The outputs for EQT and DEC have been
C confirmed against published example values tabulated in Muneer from 
C Astronomical Tables.
           
      IF(IMT.GT.2) THEN
        IYR1=IYEAR
        IMT1=IMT-3
      ELSE
        IYR1=IYEAR-1
        IMT1=IMT+9
      ENDIF
      INTT1=INT(30.6*IMT1+0.5)
      INTT2=INT(365.25*REAL(IYR1-1976))
      SMLT=((HOUR/24.0)+IDY+INTT1+INTT2-8707.5)/36525.0
      EPSILN=23.4393-0.013*SMLT
      CAPG=357.528+35999.050*SMLT
      IF(CAPG.GT.360.0) THEN
        G360=CAPG-INT(CAPG/360.0)*360.0
      ELSEIF(CAPG.LT.-360.0) THEN
        G360=CAPG-(INT(CAPG/360.0)-1)*360.0   
      ELSE
        G360=CAPG
      ENDIF
      CAPC=1.915*SIN(G360*R)+0.020*SIN(2.0*G360*R)
      CAPL=280.460+36000.770*SMLT+CAPC
      IF(CAPL.GT.360.0) THEN
        XL360=CAPL-INT(CAPL/360.0)*360.0
      ELSEIF(CAPL.LT.-360.0) THEN
        XL360=CAPL-(INT(CAPL/360.0)-1)*360.0
      ELSE
        XL360=CAPL
      ENDIF
      ALPHA=XL360-2.466*SIN(2.0*XL360*R)+0.053*SIN(4.0*XL360*R)

      EQT=(XL360-CAPC-ALPHA)/15.0
      DEC=ATAN(TAN(EPSILN*R)*SIN(ALPHA*R))/R

      SDEC=SIN(DEC*R)
      CDEC=COS(DEC*R)

C Compute solar altitude.
      TIME=HOUR+(EQT+SLON/15.)
      TIMCOE=15.*(12.-TIME)
      CDTIME=COS(TIMCOE*R)
      ABST=ABS(TIMCOE)
      SABST=SIN(ABST*R)
      SSLAT=SIN(SLAT*R)
      CSLAT=COS(SLAT*R)
      SALT=ASIN(SSLAT*SDEC+CSLAT*CDEC*CDTIME)/R
      IF(SALT.LT.0.)goto 1

C Compute solar azimuth.
      AZMUTH=(CDEC*SABST)/ECOSD(SALT)
      IF(AZMUTH.LT.-1.0)AZMUTH=-1.0
      IF(AZMUTH.GT.1.0)AZMUTH=1.0
      SAZI=ASIN(AZMUTH)/R

C Correct the azimuthal angle for time of day
C and hemisphere.
      XX=CDTIME
      call eclose(SLAT,0.00,0.1,close)
      if(close)goto 13
      call eclose(SLAT,90.00,0.1,close)
      if(close)goto 8
      YY=(CSLAT/SSLAT)*(SDEC/CDEC)
      goto 9
    8 YY=0.0
      goto 9
   13 YY=10.0*(SDEC/CDEC)
    9 IF(YY-XX)3,4,5
    3 IF(SLAT.GE.0.0)goto 6
      goto 7
    5 IF(SLAT.LT.0.0)goto 6
      goto 7
    4 IF(TIME.LE.12.0)SAZI=90.0
      IF(TIME.GT.12.0)SAZI=270.0
      goto 2
    6 IF(TIME.LE.12.0)SAZI=180.0-SAZI
      IF(TIME.GT.12.0)SAZI=180.0+SAZI
      goto 2
    7 IF(TIME.GT.12.0)SAZI=360.0-SAZI
      goto 2
    1 ISUNUP=0
    2 CONTINUE

C Trace output ?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF)goto 9999
      write(outs,'(A,I4)')' Subroutine MZSANG   Trace output',ICNT
      call edisp(itu,outs)
C      ICNT=ICNT+1

C Output time and solar position.
      write(outs,'(A,F4.0,A,F5.2,A,F5.2)')' Day ',DAY,' Mean time ',
     &  HOUR,' Solar time',TIME
      call edisp(itu,outs)
      IF(ISUNUP.EQ.0)goto 9993
      write(outs,'(A,F6.1,A,F6.1)')' Altitude =',SALT,' Azimuth =',SAZI
      call edisp(itu,outs)
 9999 RETURN

 9993 call edisp(itu,' Sun not up !')
      goto 9999
      END

C ******************** MZSINT ********************
C Establish future time-row solar intensity values: QD (augmented
C direct) & QF (background diffuse).
C The circumsolar component is established from a formula by
C Klucher.  This is then subtracted from the diffuse horizontal
C value and added to the direct normal value.

      SUBROUTINE MZSINT(ICOMP,QD,QF)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/DNORGH/IRTYPE
      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      character outs*124

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

      IF(QDF.LT.0.)QDF=0.
      IF(QFF.LT.0.)QFF=0.
      QD=QDF
      QF=QFF
      IF(IRTYPE.EQ.0)goto 1

C Solar radiation values are Global and Diffuse Horizontal.
      QD=(QDF-QFF)/SIN(SALT*R)
      IF(QD.GT.1353.0)goto 2
      IF(QD.LT.0.0)goto 3
      goto 1
    2 IF(SALT.GT.12.0)goto 4
      IF(ICOMP.GT.1)goto 5

C Warning : high direct normal.
      CALL DAYCLK(IDYP,BTIMEF,iuout)
      write(outs,'(a,I7,a,F6.1,a)')' MZSINT: increment',NSINC,
     &  ' direct n.',QD,' > solar constant '
      call edisp(iuout,outs)
      call edisp(iuout,
     & ' Default invoked; diffuse h.= global h.; direct n.=0.')
      goto 5

C Warning: diffuse > global.
    3 IF(ICOMP.GT.1)goto 5
      CALL DAYCLK(IDYP,BTIMEF,iuout)
      write(outs,7)NSINC
    7 format(' MZSINT: increment',I8,' diffuse h. > global h.')
      call edisp(iuout,outs)
      call edisp(iuout,
     & ' Default invoked; diffuse h.= global h.; direct n.= 0.')

C Default: zero direct normal, diffuse horizontal equals global 
C horizontal.
    5 QF=QDF
      QD=0.0
      goto 1

C FATAL ERROR: direct normal > solar constant & altitude > 10deg
    4 CALL DAYCLK(IDYP,BTIMEF,iuout)
      write(outs,'(a,I7,a,F6.1,a)')' MZSINT fatal: increment',NSINC,
     &  ' direct n.',QD,' > solar constant '
      call edisp(iuout,outs)
      write(outs,'(a,F6.1,a,F6.1)')' and solar altitude is ',SALT,
     &  ' and azimuth is ',SAZI
      call edisp(iuout,outs)
      write(outs,'(a,F6.1,a,F6.1)')' and global horizontal is ',QDF,
     &  ' and diffuse is ',QFF
      call edisp(iuout,outs)
      call epwait
      call epagend
      STOP

    1 RETURN
      END

C ******************** MZSCAI ********************
C MZSCAI computes the cosine of the solar angle of incidence.
C Also the elevation and azimuth angles of incidence for
C the bidirectional case.
      SUBROUTINE MZSCAI(ICOMP,II,CAI,ANGI,IND1,IND2,PSAZI2,
     &           PSELV2,IANGBI1,IANGBI2)
#include "building.h"
      PARAMETER (MSTMC=1,MSGAL=1,MANH=37,MANV=37)

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)

      COMMON/SUNPOS/SAZI,SALT,ISUNUP

      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      COMMON/OPTDAT/NSGALFL(MSTMC),NGNTL(MSTMC),
     &  NGANGS(MSTMC),TMTSOD(MSTMC,MSGAL,MANH,MANV),
     &  TMTSOB(MSTMC,MSGAL,MANH,MANV),TMABSO(MSTMC,MSGAL,ME,MANH,MANV),
     &  THTSOB(MSTMC,MSGAL),TMABSDIF(MSTMC,MSGAL,ME)
      COMMON/C24/IZSTOCN(MCOM,MS)

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C Surface-solar azimuth angle for this external construction.
      PSAZI=ABS(PAZI(ICOMP,II)-SAZI)

C Calculate angles for bi-directional information
C PSAZI2 is the solar minus surface normal azimuths.
C Example: if surface faces south, sun in east is -90deg;
C sun in west in +90deg.
C PSELV2 is the solar minus surface normal elevations.
C Example: for a vertical surface, sun vertically above
C is +90deg, horizontal sun is 0deg, with -ve values
C for the ground reflections.
      PSAZI2=SAZI-PAZI(ICOMP,II)
      PSELV2=SALT-PELV(ICOMP,II)

C Angle of incidence and its cosine.
C      CAI=COS(SALT*R)*SIN((90.-PELV(ICOMP,II))*R)*
C     &COS(PSAZI*R)+SIN(SALT*R)*COS((90.-PELV(ICOMP,II))*R)

C Angle of incidence and its cosine (Iain Macdonald Feb 2006).
      CAI=COS(SALT*R)*SIN((90.-PELV(ICOMP,II))*R)*COS(PSAZI*R)+
     &    SIN(SALT*R)*COS((90.-PELV(ICOMP,II))*R)
      IF(CAI.LT.0.)CAI=0.
      ANGI=ACOS(CAI)/R

C Establish angle of incidence index (IND1) for
C glazing property interpolation purposes; where

C IND1=1   for   0 <= ANGI <= 40
C IND1=2   for  40 <  ANGI <= 55
C IND1=3   for  55 <  ANGI <= 70
C IND1=4   for  70 <  ANGI <= 80
C IND1=5   for  80 <  ANGI <= 90

C Test if standard optical data or bidirectional data.
      IF(NSTMCFL(IZSTOCN(ICOMP,II)).EQ.0) THEN
        IF(ANGI.GT.-0.01.AND.ANGI.LT.0.0)ANGI=0.0
        IF(ANGI.GT.90.0.AND.ANGI.LT.90.01)ANGI=90.0
        IF(ANGI.LT.0..OR.ANGI.GT.90.)goto 9999
        IND1=1
        IF(ANGI.GT.40..AND.ANGI.LE.55.)IND1=2
        IF(ANGI.GT.55..AND.ANGI.LE.70.)IND1=3
        IF(ANGI.GT.70..AND.ANGI.LE.80.)IND1=4
        IF(ANGI.GT.80..AND.ANGI.LE.90.)IND1=5
        IND2=IND1+1
      ELSE

C For bidirectional data, IND1 is the index for azimuth, IND2 is the
C index for altitude. Assuming data goes from -90 to +90, in both
C cases the index points to the array value below.
C At present, we assume 5 deg intervals (37 array values in each direction)
C so -87 deg is IND1=1, 81 deg is IND1=2 etc.
C IANGBI1 and IANGBI2 are the angles corresponding to the IND1,IND2
C array elements.
        IND1=1+INT((PSAZI2+90.0)/REAL(INTVALBI))
        IND2=1+INT((PSELV2+90.0)/REAL(INTVALBI))
C IANGBI1 and IANGBI2 are the angles corresponding to the IND1,IND2 array element
        IANGBI1=INTVALBI*(IND1-1)-90
        IANGBI2=INTVALBI*(IND2-1)-90
      ENDIF
 9999 RETURN
      END

C ******************** MZSRAD ********************
C  SOLAR PROCESSING: incident comprises direct, sky diffuse
C  and ground reflected components.

      SUBROUTINE MZSRAD(ICOMP,II,QD,QF,CAI,SRADDO,SRADF)
#include "building.h"

      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER SNFNAM*72

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      COMMON/SKYFLG/ISKYFL
      COMMON/DNORGH/IRTYPE

C Start Perez definitions (1990 have suffix 2):
      DIMENSION  EPSINT(8),EPSINT2(8),
     &           F11ACC(8),F12ACC(8),F13ACC(8),
     &           F21ACC(8),F22ACC(8),F23ACC(8),
     &           F11ACC2(8),F12ACC2(8),F13ACC2(8),
     &           F21ACC2(8),F22ACC2(8),F23ACC2(8)

C Coefficients (Perez 1987):
      DATA EPSINT/1.056,1.253,1.586,2.134,3.230,5.980,10.080,999999./
      DATA F11ACC/-.011,-0.038, .166, .419, .710, .857, 0.734, 0.421/
      DATA F12ACC/ .748, 1.115, .909, .646, .025,-.370,-0.073,-0.661/
      DATA F13ACC/-.080,-0.109,-.179,-.262,-.290,-.279,-0.228, 0.097/
      DATA F21ACC/-.048,-0.023, .062, .140, .243, .267, 0.231, 0.119/
      DATA F22ACC/ .073, 0.106,-.021,-.167,-.511,-.792,-1.180,-2.125/
      DATA F23ACC/-.024,-0.037,-.050,-.042,-.004, .076, 0.199, 0.446/

C Coefficients (Perez 1990):
      DATA EPSINT2/1.065,1.230,1.500,1.950,2.800,4.500,6.200,999999./
      DATA F11ACC2/-.008, 0.130, .330, .568, .873, 1.132, 1.060, 0.678/
      DATA F12ACC2/ .588, 0.683, .487, .187,-.392,-1.237,-1.600,-0.327/
      DATA F13ACC2/-.062,-0.151,-.221,-.295,-.362,-0.412,-0.359,-0.250/
      DATA F21ACC2/-.060,-0.019, .055, .109, .226, 0.288, 0.264, 0.156/
      DATA F22ACC2/ .072, 0.066,-.064,-.152,-.462,-0.823,-1.127,-1.377/
      DATA F23ACC2/-.022,-0.029,-.026,-.014, .001, 0.056, 0.131, 0.251/

C END Perez definitions.

      PI = 4.0 * ATAN(1.0)
      R = PI / 180.
      BETA=90.0-PELV(ICOMP,II)

C Sky diffuse radiation is model dependent (Perez 1990 is the default).
      IF(ISKYFL.EQ.-1)THEN

C *** START Isotropic.
C Direct solar radiation incident normally on
C external construction prior to shading adjustment
C and absorption.
      SRADDO=QD*CAI
      IF( SRADDO .LT. 0.0 ) SRADDO = 0.0

C  The ground reflected component: assume isotropic
C  ground conditions.
      SRADT=QF+(QD*SIN(SALT*R))
      GRDDIF=0.5*GREF*(1.0-COS(BETA*R))*SRADT

C  The sky diffuse component.
      SKYDIF=QF*0.5*(1.0+COS(BETA*R))
      ENDIF
C *** END Isotropic.

      IF(ISKYFL.EQ.0)THEN

C *** START Klucher.
C At this stage QD and QF are the unmodified direct normal and
C diffuse horizontal values. We now modify for circumsolar activity.
C Note that the evaluation of CLEAR is approximate since the
C circumsolar has not yet been determined.
      SRADT=QF+(QD*SIN(SALT*R))
      IF(SRADT.LT.0.001)goto 1
      CLEAR=1.-((QF**2)/(SRADT**2))
      goto 2
    1 CLEAR=0.

    2 QFB=QF/(1.0+(CLEAR*(SIN(SALT*R)**2)*((SIN((90.0-SALT)*R))**3)))

C Use temporary variables (ADJD & ADJF) to avoid adjusting
C QD & QF each time this routine is called per surface.
      ADJD=QD+((QF-QFB)/SIN(SALT*R))
      ADJF=QFB

C Direct solar radiation incident normally on
C external construction prior to shading adjustment
C and absorption.
      SRADDO=ADJD*CAI
      IF( SRADDO .LT. 0.0 ) SRADDO = 0.0

C  The ground reflected component: assume isotropic
C  ground conditions.
      SRADT=ADJF+(ADJD*SIN(SALT*R))
      GRDDIF=0.5*GREF*(1.0-COS(BETA*R))*SRADT

C  The sky diffuse component.
      SKYDIF=ADJF*0.5*(1.0+COS(BETA*R))
      ENDIF
C *** END Klucher.

      IF(ISKYFL.EQ.1)THEN

C *** START Muneer.
         skydif=0.

C Direct solar radiation incident normally on
C external construction prior to shading adjustment
C and absorption.
         SRADDO=QD*CAI
         IF( SRADDO .LT. 0.0 ) SRADDO = 0.0

         s9=(90.-pelv(icomp,ii))*r
         e=1353.*(1.+0.033*cos(0.017203*idyp))*sin(salt*r)
         if(cai.le.0.0)goto 1005

C Add an extra test for solar altitude. For example, if there is a small error in the
C measurements (less than 1 W/m2 between GH and DH) and the solar altitude
C is <1 deg, the value of SRADDO is calculated to be >50W/m2. Therefore revert to cof2 
C if SRADDO is less than 5 W/m2 or if solar altitude <3deg.
         if(sraddo.lt.5.0.or.(irtype.ne.0.and.salt.lt.3.0))goto 998
         ff=sraddo/e
         cof1=0.224
         xx=cos(s9/2.)**2-cof1*(sin(s9)-s9*
     &      cos(s9)-PI*sin(s9/2.)**2)
         if(abs(sin(salt*r)).gt.1E-6)skydif=qf*
     &                              ((ff*cai/sin(salt*r))+xx*(1.-ff))
         goto 2500
  998    cof2=0.168
         xx=cos(s9/2.)**2+cof2*(sin(s9)-s9*
     &      cos(s9)-PI*sin(s9/2.)**2)
         skydif=qf*xx
         goto 2500
 1005    cof3=0.252
         xx=cos(s9/2.)**2+cof3*(sin(s9)-s9*
     &      cos(s9)-PI*sin(s9/2.)**2)
         skydif=qf*xx
 2500    continue

C  The ground reflected component : assume isotropic
C  ground conditions.
      SRADT=QF+(QD*SIN(SALT*R))
      GRDDIF=0.5*GREF*(1.0-COS(BETA*R))*SRADT

      ENDIF
C *** END Muneer.

      IF(ISKYFL.EQ.2)THEN

C *** START Perez 1987 model.
C Ref: Perez et al, Solar Energy Vol 39, No 3, pp221-231, 1987.

C alpha = the half-angle circumsolar region.
C hpmalf, hppalf are help variables.
C beta = the inclination angle of the surface (defined above).
C teta = incident angle on the tilted surface
C zet = solar zenith angle (pi/2 - solar altitude).
      HP    = 0.5 * PI
      ALPHA  = 25. * R
      HPMALF = HP - ALPHA
      HPPALF = HP + ALPHA

C NT sensitive to ACOS parameter range.
      if(ABS(CAI).le.1.0)then
        TETA   = ACOS(CAI)
      else
        TETA   = ACOS(1.0)
      endif
      ZET    = ACOS(SIN(SALT*R))

C Calculation of the relative air mass.
      AIRM  = 1. / SIN(SALT * R)
      IF (SALT .LT. 10.)
     &   AIRM = 1. / (SIN(SALT*R) + 0.15 * (SALT + 3.885)**(-1.253))

C Calculation of the extraterrestrial radiation.
      G0N = 1370. * (1. + 0.033 * COS(0.017214 * IDYP))

C The sky diffuse component:
      PSIC = ((HPPALF - TETA) / ALPHA) / 2.
      PSIH = 1.
      IF (ZET .GT. HPMALF) PSIH = (HPPALF - ZET)/(ALPHA*2.0)

      XIC = 0.0
      IF (TETA .GT. HPMALF .AND. TETA .LT. HPPALF)
     &    XIC = PSIH * PSIC * SIN(PSIC * ALPHA)
      IF (TETA .LT. HPMALF) XIC = PSIH * CAI
          XIH = PSIH * SIN(PSIH * ALPHA)
      IF (ZET .LT. HPMALF) XIH = COS(ZET)

C Approximation of A and C, the solid angles occupied by the
C circumsolar region, weighted by its average incidence on
C the slope and horizontal respectively. In the expression
C of SKYDIF the quotient of A/C is reduced to XIC/XIH.
C  A = 2. * (1. - COS(ALPHA)) * XIC
C  C = 2. * (1. - COS(ALPHA)) * XIH

C Determination of INTEPS with EPS ('sky clearness parameter').
      IF (QF.GT.0.) THEN
        EPS = (QF + QD) / QF
      ELSE
        EPS = 0.
      ENDIF
      DO 100 INT = 1,8
        IF (EPS .LE. EPSINT(INT)) THEN
        INTEPS = INT
        GO TO 200
        END IF
 100  CONTINUE
 200  CONTINUE

C DELTA is "the new sky brightness parameter".
      DELTA = QF * AIRM / G0N

C Determination of the "new circumsolar brightness coefficient
C (F1ACC) and horizon brightness coefficient (F2ACC)".
      F1ACC = F11ACC(INTEPS) + F12ACC(INTEPS) * DELTA
     &        + F13ACC(INTEPS) * ZET
      F2ACC = F21ACC(INTEPS) + F22ACC(INTEPS) * DELTA
     &        + F23ACC(INTEPS) * ZET

C Determination of the diffuse radiation on an inclined surface.
      SKYDIF = QF * ( 0.5 * (1. + COS(BETA*R)) * (1. - F1ACC)
     &         + F1ACC * XIC/XIH + F2ACC * SIN(BETA*R))

      IF (SKYDIF .LT. 0.) SKYDIF = 0.0

C Horizontal surfaces treated separately.
C  beta = 0   : surface facing up.
C  beta = 180 : surface facing down.
      IF (BETA.GT.-0.01.AND.BETA.LT.0.01)    SKYDIF = QF
      IF (BETA.GT.179.99.AND.BETA.LT.180.01) SKYDIF = 0

C Direct solar radiation incident normally on
C external construction prior to shading adjustment
C and absorption.
      SRADDO=QD*CAI
      IF( SRADDO .LT. 0.0 ) SRADDO = 0.0

C The ground reflected component: assume isotropic
C ground conditions.
      SRADT=QF+(QD*SIN(SALT*R))
      GRDDIF=0.5*GREF*(1.0-COS(BETA*R))*SRADT
      ENDIF
C *** END Perez 1987.

      IF(ISKYFL.EQ.3)THEN
 
C *** START Perez 1990 model.
C Ref: Perez et al, Solar Energy Vol 44, No 5, pp271-289, 1990.
C beta = the inclination angle of the surface (defined above).
C teta = incident angle on the tilted surface
C zet = solar zenith angle (pi/2 - solar altitude).
 
      TETA = ACOS(amin1(abs(CAI),1.))
      ZET  = (90.-SALT)*R
 
C Calculation of the relative air mass.
C (Muneer: Solar radiation and daylight models 1997)
      AIRM = 1. / (SIN(SALT*R) + 0.50572 * (SALT + 6.07995)**(-1.6364))
                                                                       
C Calculation of the extraterrestrial radiation (normal incidence).
C (Spencer 1971 plus solar constant from ASTM E-490, 2000)
      G0N = 1366.1 * (1. + 0.033 * COS(0.017214 * IDYP))
                                                                        
C The sky diffuse component:
      A0 = amax1(CAI,0.)
      A1 = amax1(sin(5.*R),sin(SALT*R))
                                                                        
C Determination of INTEPS with EPS ('sky clearness parameter').
      IF (QF.GT.0.) THEN
        EPS = (((QF + QD)/QF)+(1.041*ZET**3))/(1.0+(1.041*ZET**3))
      ELSE
        EPS = 0.
      ENDIF
      DO 101 INT = 1,8
        IF (EPS .LE. EPSINT2(INT)) THEN
        INTEPS = INT
        GO TO 201
        END IF
 101  CONTINUE
 201  CONTINUE

C DELTA is "the new sky brightness parameter".
      DELTA = QF * AIRM / G0N

C Determination of the "new circumsolar brightness coefficient
C (F1ACC) and horizon brightness coefficient (F2ACC)".
      F1ACC2 = F11ACC2(INTEPS) + F12ACC2(INTEPS) * DELTA
     &        + F13ACC2(INTEPS) * ZET
      F1ACC2 = amax1(F1ACC2,0.)

      F2ACC2 = F21ACC2(INTEPS) + F22ACC2(INTEPS) * DELTA
     &        + F23ACC2(INTEPS) * ZET

C Determination of the diffuse radiation on an inclined surface.
      SKYDIF = QF * ( (1. + COS(BETA*R)) * (1. - F1ACC2)/2.
     &         + F1ACC2 * A0/A1 + F2ACC2 * SIN(BETA*R))
      SKYDIF = amax1(SKYDIF,0.)

C Horizontal surfaces treated separately.
C  beta = 0   : surface facing up.
C  beta = 180 : surface facing down.
      IF (BETA.GT.-0.01.AND.BETA.LT.0.01)    SKYDIF = QF
      IF (BETA.GT.179.99.AND.BETA.LT.180.01) SKYDIF = 0

C Direct solar radiation incident normally on
C external construction prior to shading adjustment
C and absorption.
      SRADDO=QD*CAI
      IF( SRADDO .LT. 0.0 ) SRADDO = 0.0

C The ground reflected component: assume isotropic
C ground conditions.
      SRADT=QF+(QD*SIN(SALT*R))
      GRDDIF=GREF*(1.0-COS(BETA*R))*SRADT/2.
      ENDIF
C *** END Perez 1990 model.

C  Therefore total diffuse is:
      SRADF=SKYDIF+GRDDIF
      RETURN
      END

C ******************** MZWINP ********************
C MZWINP computes, from the 5 values read as input,
C the values of window direct solar transmission and
C total heat gain factor for any angle of incidence.
C Linear interpolation is assumed throughout.

      SUBROUTINE MZWINP(X1,Y1,X2,Y2,IND,ANG,TRN,HTG)
      IF(IND.EQ.1)THEN
        A=0.
        B=40.
      ENDIF
      IF(IND.EQ.2)THEN
        A=40.
        B=55.
      ENDIF
      IF(IND.EQ.3)THEN
        A=55.
        B=70.
      ENDIF
      IF(IND.EQ.4)THEN
        A=70.
        B=80.
      ENDIF
      IF(IND.EQ.5)THEN
        A=80.
        B=90.
        X2=0.
        Y2=0.
      ENDIF

      F=(ANG-A)/(B-A)
      TRN=X1+F*(X2-X1)
      HTG=Y1+F*(Y2-Y1)
      IF(TRN.LT.0.00005)TRN=0.
      IF(HTG.LT.0.00005)HTG=0.

      RETURN
      END

C ******************** MZWINP2 ********************
C MZWINP2 interpolates bidirectional data.
C Linear interpolation is assumed.

      SUBROUTINE MZWINP2(X1,X2,X3,X4,IANGBI1,IANGBI2,PSAZI2,PSELV2,Y)
#include "building.h"

      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      
C Interpolate azimuth first.
      F1=(PSAZI2-REAL(IANGBI1))/REAL(INTVALBI)
      Z1=X1+F1*(X2-X1)
      Z2=X3+F1*(X4-X3)

C Now interpolate on altitude.
      F2=(PSELV2-REAL(IANGBI2))/REAL(INTVALBI)
      Y=Z1+F2*(Z2-Z1)
      
      IF(Y.LT.0.00005)Y=0.

      RETURN
      END

C ****************************MZTMCA*********************************
C MZTMCA computes the absorptions and transmissions for a TMC
C It assumes that the radiation is incident from inside the
C room whereas TMC's are defined from the outside. The relative
C absorptivities are thus reversed by an approximate method which
C assumes that the refectivity is the same from each element of
C the TMC.

C RAD   is radiation incident on the TMC in W/m^2.
C XF    is the transmission factor assuming incident diffuse.
C ABTOT is the total absorbed.

      SUBROUTINE MZTMCA(ICOMP,ISUR,RAD,XF,ABTOT)
#include "building.h"

      COMMON/COE32J/QTMCA(MS,MN,2)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5,MBP),TMCA2(MCOM,MTMC,ME,5,MBP),
     &       TVTR2(MCOM,MTMC,MBP)
      COMMON/TMCB4/IBOFOT(MCOM,MS),NBPONT(MCOM,MS),ECRAT(MCOM,MS)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      DIMENSION ABF(ME),ABFABS(ME)

C Calculate relative absorptivities and transmission at 51 deg.
      NE=NELTS(ICOMP,ISUR)
      KP=NBPONT(ICOMP,ISUR)
      ABTOT=0.
      DO 1 L=1,NE
        LL=NE+1-L

C If IBOFOT()=1 the blind/shutter or If IBOFOT()=2 linear is on.
        IF (IBOFOT(ICOMP,ISUR).EQ.2) THEN
          AMIN=TMCA2(ICOMP,ITMCFL(ICOMP,ISUR),LL,2,KP)
          AMAX=TMCA(ICOMP,ITMCFL(ICOMP,ISUR),LL,2)
          XX1=AMIN + (AMAX - AMIN)*ECRAT(ICOMP,ISUR)
          AMIN=TMCA2(ICOMP,ITMCFL(ICOMP,ISUR),LL,3,KP)
          AMAX=TMCA(ICOMP,ITMCFL(ICOMP,ISUR),LL,3)
          XX2=AMIN + (AMAX - AMIN)*ECRAT(ICOMP,ISUR)
        ELSEIF(IBOFOT(ICOMP,ISUR).EQ.1) THEN
          XX1=TMCA2(ICOMP,ITMCFL(ICOMP,ISUR),LL,2,KP)
          XX2=TMCA2(ICOMP,ITMCFL(ICOMP,ISUR),LL,3,KP)
        ELSE
          XX1=TMCA(ICOMP,ITMCFL(ICOMP,ISUR),LL,2)
          XX2=TMCA(ICOMP,ITMCFL(ICOMP,ISUR),LL,3)
        ENDIF
        ABF(LL)=XX1+((51.-40.)/(55.-40.))*(XX2-XX1)
        ABTOT=ABTOT+ABF(LL)
    1 CONTINUE
      IF (IBOFOT(ICOMP,ISUR).EQ.2) THEN
        TMIN=TMCT2(ICOMP,ITMCFL(ICOMP,ISUR),2,KP)
        TMAX=TMCT(ICOMP,ITMCFL(ICOMP,ISUR),2)
        XX1=TMIN + (TMAX - TMIN)*ECRAT(ICOMP,ISUR)
        TMIN=TMCT2(ICOMP,ITMCFL(ICOMP,ISUR),3,KP)
        TMAX=TMCT(ICOMP,ITMCFL(ICOMP,ISUR),3)
        XX2=TMIN + (TMAX - TMIN)*ECRAT(ICOMP,ISUR)
      ELSEIF (IBOFOT(ICOMP,ISUR).EQ.1) THEN
        XX1=TMCT2(ICOMP,ITMCFL(ICOMP,ISUR),2,KP)
        XX2=TMCT2(ICOMP,ITMCFL(ICOMP,ISUR),3,KP)
      ELSE
        XX1=TMCT(ICOMP,ITMCFL(ICOMP,ISUR),2)
        XX2=TMCT(ICOMP,ITMCFL(ICOMP,ISUR),3)
      ENDIF
      XF=XX1+((51.-40.)/(55.-40.))*(XX2-XX1)

C Now calculate the absolute absorptivities (approximate method
C assumes that total reflectivity is the same in both directions
C for shortwave.
      ABFABS(1)=ABF(1)
      IF (NE.GT.1)THEN
        DO 2 L=2,NE
          SUM=0.
          PROD=1.
          do 3 K=1,L
            SUM=SUM+ABF(K)
    3     continue
          do 4 K=1,(L-1)
            PROD=PROD*(1.-ABFABS(K))
    4     continue
          ABFABS(L)=1.-(1.-SUM)/PROD
    2   CONTINUE

C Now calculate the relative absorptivities in the opposite
C direction.
        ABF(NE)=ABFABS(NE)
        DO 5 L=1,(NE-1)
          LL=NE-L
          SUM=0.
          PROD=1.
          do 6 K=(LL+1),NE
            SUM=SUM+ABF(K)
    6     continue
          do 7 K=LL,NE
            PROD=PROD*(1.-ABFABS(K))
    7     continue
          ABF(LL)=1.-SUM-PROD
    5   CONTINUE
      ENDIF
      INODE=NNDC(ICOMP,ISUR)
      DO 10 L=1,NE
        LL=NE+1-L
        NNOD=NNDL(ICOMP,ISUR,LL)
        FNNOD=FLOAT(NNOD)
        DFAB=RAD*ABF(LL)
        QTMCA(ISUR,INODE,2)=QTMCA(ISUR,INODE,2)+DFAB/(FNNOD*2.)
        DO 20 JJ=2,NNOD
          INODE=INODE-1
          QTMCA(ISUR,INODE,2)=QTMCA(ISUR,INODE,2)+DFAB/FNNOD
   20   CONTINUE
        INODE=INODE-1
        QTMCA(ISUR,INODE,2)=QTMCA(ISUR,INODE,2)+DFAB/(FNNOD*2.)
   10 CONTINUE
      RETURN
      END

C ******************** MZSFSH ********************
C MZSFSH determines if an external surface faces 'away' from
C the sun so causing self-shading.

C IANS=0 ; no self-shading
C IANS=1 ; self-shading occurs

      SUBROUTINE MZSFSH(ICOMP,IS,IANS)
#include "building.h"

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/SUNPOS/SAZI,SALT,ISUNUP

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C Determine the surface-solar azimuth angle.
      PSAZI=ABS(PAZI(ICOMP,IS)-SAZI)

C Cosine of angle of incidence.
      X1=COS(SALT*R)
      X2=SIN((90.-PELV(ICOMP,IS))*R)
      X3=COS(PSAZI*R)
      X4=SIN(SALT*R)
      X5=COS((90.-PELV(ICOMP,IS))*R)
      CAI=X1*X2*X3+X4*X5

C Surface is self-shaded if CAI is less than or
C equal to zero.
      IANS=0
      IF(CAI.LE.0.)IANS=1
      RETURN
      END

c ******************** MZSHDO ********************
c MZSHDO reads all shading/insolation information from the
c direct access shading file constructed via the input
c management facility prior to simulation.

c The shading file contains the shading of selected external
c surfaces (opaque and transparent), as well as the insolation
C of surfaces within the zone. This shading/insolation
c information - expressed as a proportion of 1 (completely
c shaded) - is held within the file for each of the 24 hours
c in a day which is deemed to represent a month.   Thus, for
c example, for a January simulation (or some subset of January)
c the file would contain one day of hourly shading/insolation
c values relating to selected surfaces.   If the subsequent
c simulation extended into the following month then an
c additional representative day of hourly values would be
c required.   For a full one year simulation then the file
c would contain 12 representative days of values.

c The shading/insolation file is therefore only re-accessed
c if the requested simulation period extends from one month
c to another  or if a series of simulations are being
c conducted over different periods which are situated within
c different months (that is only when the simulation month
c changes).

c Common block variables are:

c IML   - is the last month number for which the shading/
c         insolation information was obtained from the file.
c ISHD  - is the shading/insolation index which defines the
c         shading file contents where:
c         ISHD=0 means that no shading or insolation information
c                is available (the default case) and hence the
c                assumption is that all shading is zero and all
c                insolation sources take instructions from the
c                insolation distribution instructions in the zone
c                geometry file.   
c         ISHD=1 means that shading information is available for
c                selected surfaces but no insolation data is
c                available. Again, instructions from the zone
c                geometry file are followed.
c         ISHD=2 means that internal surface insolation information
c                is available for specified surfaces. Any unspecified
c                surface is treated as having no shading and, if
c                transparent, uses the geometry file instructions.
c         ISHD=3 means that both shading and insolation information is
c                available for selected surfaces. Any unspecified surface
c                is treated as having no shading and, if transparent
c                uses the geometry file specification.
c IGC   - is an array pointer which defines the address of
c         the first considered window in each surface.   A
c         value of zero means either that the corresponding
c         surface has no windows or that these windows are
c         not considered in the file.   In the latter case
c         zero shading is assumed when the window is sunlit.
c         The assumption is made that if the array address
c         of the first window considered in some surface
c         is given then information is available for each
c         and every window in that surface.
c PO    - gives the shading of each external exposed opaque
c         surface considered and for each hour in the
c         representative day.
c IRS   - defines the internal surfaces which potentially
c         would receive direct transmitted solar radiation
c         from each of the considered window/shutter
c         arrangements.   The maximum number of surfaces insolated
c         can be changed with parameter 'misur'.
c PI    - defines the split of the direct transmitted solar
c         radiation - relating to any window - between the
c         receiving internal surfaces.

      SUBROUTINE MZSHDO(IYRD)
#include "building.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      COMMON/SHAD1/IML

C Shading file record width found in mzshin.
      common/rsolmax/maxrec(mcom)

      COMMON/SHAD2/ISHD(MCOM),IGCS(MCOM,MS),PO(MCOM,MS,MT),
     & IGCI(MCOM,MS),insst(mcom,mgt,mt,misur),pinsst(mcom,mgt,mt,misur)

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LS,LUTIL
      DIMENSION ISS(12),ISADD(12)
      character outs*124,h*72
      logical OK,dok

c Determine current month number.
      CALL EDAYR(IYRD,ID,IM)

c If month has changed from previous call of this
c subroutine then read shading file.
      IF(IML.EQ.IM)RETURN

c Read information from file.
      IML=IM
      IUNIT=IFIL+1

c Handle shading file corresponding to each zone in
c turn.
      DO 10 I=1,NCOMP
        IF(ISI(I).EQ.0)goto 9

c Assign shading file for zone 'I'.
        LS=LSHAD(I)
        call EFOPRAN(IUNIT,LS,maxrec(i),1,IER)
        IF(ier.ne.0)goto 1000
        IREC=1
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(ISS(J),J=1,12),
     &      (ISADD(J),J=1,12)
        ISHD(I)=ISS(IM)
        IREC=ISADD(IM)
        IF(IREC.EQ.0)goto 1002

c Read opaque surface shading information.
        IISS=ISS(IM)
        NC=NCONST(I)
        goto (101,102,101),IISS
  101   DO 20 J=1,NC
          READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(PO(I,J,K),K=1,24)
          IREC=IREC+1
   20   CONTINUE

c Now read default window shading information for those
c arrangements considered << not used test doing without this read >>.
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(IGCS(I,J),J=1,NC)
        IRM=IREC

C 'ISUM' sums the number of default windows handled in total.
C Default windows are no longer considered in ish so skip related
C data structures here.
        ISUM=0
        IF(ISHD(I).EQ.1)goto 1

c Now read internal insolation information.
        IRM=IRM+ISUM+1
        goto 103
  102   IRM=ISADD(IM)
  103   ISUM=0
        IREC=IRM
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(IGCI(I,J),J=1,NC)
        irec=irec+1

C Calculate number of transparent surfaces that have insolation information.
        nw=0
        do 50 j=1,nc
          if(igci(i,j).eq.0)goto 50
          if(itmcfl(i,j).gt.0)then
            itmc=1
          else
            itmc=0
          endif
          nw=nw+itmc
   50   continue

C insst()=0, the whole surface is shaded; insst()=-1, the sun is not up,
C   otherwise the index of the surface being insolated.
C pinsst(i,j,k) proportion insolated from source k at hr m to insolated
C   surf n, for the receiving surface
        do 200 k=1,nw
          do 201 n=1,misur
            read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1001)
     &        (insst(i,k,m,n),m=1,24)
            IREC=IREC+1
  201     continue
          do 202 n=1,misur
            read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1001)
     &        (pinsst(i,k,m,n),m=1,24)
            IREC=IREC+1
  202     continue
          do 203 n=1,misur
C pinwst not used so skip past these records.
            IREC=IREC+1
  203     continue
  200   CONTINUE

c Free shading file.
    1   CALL ERPFREE(IUNIT,ISTAT)
        goto 10

c Error messages: if error is detected option is
c given to continue with default treatment (ISHD=0)
c applied to zone with which error is associated.
 1000   WRITE(outs,2)I
    2   FORMAT(' Zone',I3,' shading/insolation file cannot be opened.')
        call edisp(iuout,outs)
        goto 3

 1001   WRITE(outs,4)I
    4   FORMAT(' Zone',I3,' bad data in shading/insolation file.')
        call edisp(iuout,outs)
        goto 3

 1002   WRITE(outs,5)I,IM
    5   FORMAT(' Zone',I3,' no shading data for month',I3,'.')
        call edisp(iuout,outs)

    3   call edisp(iuout,' ')
        dok=.true.
        h(1)='Ignoring shading can alter performance predictions.'
        h(2)='Exercise care when continuing with the current model.'
        h(3)='SUGGEST - recalculate shading for whole year. '
        CALL ASKOK(' Continue the simulation and assume zero ',
     &           ' shading with default plane insolation?',OK,dok,3)
        IF(OK)goto  9

c Terminate simulation.
        CALL EPAGEND
        STOP

c Default Shading/Insolation treatment for this zone.
    9   ISHD(I)=0
        goto 1
   10 CONTINUE
      RETURN
      END

C ********* MZGREF *************
C Created by: Didier Thevenard
C Initial Creation Date: April 2005
C Copyright 2000: NRCan Buildings Group

C Subroutine MZGREF determines the ground reflectivity for the hour depending
C on the model specificed by the user: constant reflectivity, simple, and
C advanced
 
C references: "Development and Implementation of a Ground Reflectivity
C              for ESP-r" Prepared Levelton Consultants, Richmond B.C.,
C              for Natural Resources Canada, June 30th 2005.

C OUTPUTS:
C       GREF   actual snow reflectivity for the hour         
C *********************************************************************
      SUBROUTINE MZGREF

      IMPLICIT NONE

C-----------------------------------------------------------------------
C     DECLARATIONS
C-----------------------------------------------------------------------
      
C ESP-r Commons      
#include "building.h"
      INTEGER IXPOS
      REAL GREF         ! actual snow covered ground reflectivity for the hour
      INTEGER ITGREF    ! flag for ground reflectivity model used
      REAL GREF12       ! no snow ground reflectivity for each month
      REAL SREF         ! uncorrected snow covered ground reflectivity
      INTEGER NSNOW     ! monthly number of days with snow on the ground
      CHARACTER*72 SNFNAM ! snow depth file name 
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM

      REAL BTIMEP,BTIMEF ! Present and future building side time step
                         ! Number between 0. and 24.            
      COMMON/BTIME/BTIMEP,BTIMEF

C Building side time step parameters
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS 
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      INTEGER ITC,ICNT
      COMMON/TC/ITC,ICNT

      INTEGER ITCF,ITRACE,IZNTRC,ITU
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      INTEGER IFIL       ! Unit number used for processing input/output files
      COMMON/FILEP/IFIL

C Local variables   
      INTEGER MONTH(12)
      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
      CHARACTER OUTS*124
      INTEGER IDYAUX    ! day for which to determine ground reflectivity
      LOGICAL SNMLT     ! snow melt indicator
      INTEGER ID,IM     ! month day number and number of days in the month 
                        ! associated with certain day of the year
      REAL AUX          ! fraction of days of the month with snow on the ground
      INTEGER IBINSN    ! unit number for binary file holding snow depth
                        ! for each hour of the year
      REAL SNDEP        ! snow depth read from binary file
      REAL SREFP        ! updated ground reflectivity for the time step

C Saved variables for last snow albedo calculation
      INTEGER IDYLS     ! last day for which snow albedo calculated
      INTEGER IHRLS     ! last hour for which snow albedo calculated
      REAL SNDEPLS      ! last snow depth read from file
      REAL SREFLS       ! last calculated snow reflectivity 
      SAVE IDYLS,IHRLS,SNDEPLS,SREFLS
      DATA IDYLS,IHRLS,SNDEPLS/0,0,0./
      
C CASE 1: CONSTANT GROUND ALBEDO
C Nothing to do as the constant ground reflectivity is entered by the
C user and does not need to be modified

C CASE 2: SIMPLE MODEL 
C Ground reflectivity is a weighted average of the no-snow and
C snow reflectivities. The weighting factor is the fraction of
C the month with snow on the ground.

      IF (ITGREF.EQ.2) THEN
        IDYAUX=IDYP                    ! day of year
        IF(IHRF.EQ.1) IDYAUX=IDYF      ! end of day: use next day
        CALL EDAYR(IDYAUX,ID,IM)       ! calculate day and month
        AUX=MAX(MIN(FLOAT(NSNOW(IM))/FLOAT(MONTH(IM)),1.),0.)
        GREF=GREF12(IM)*(1.-AUX)+SREF*AUX

C CASE 3: ADVANCED MODEL
C If snow depth is 5 cm or more:
C   if snow depth increases from hour, reset reflectivity
C     to fresh snow reflectivity
C   else, if snow is not melting, decrease snow
C     reflectivity by 1% per day (or 0.99**(1/24) per hour)
C   else (i.e. if snow is melting), decrease snow 
C     reflectivity by 3% per day (or 0.97**(1/24) per hour)
C If snow depth is less than 5 cm, use a weighted average of the 
C no-snow and snow reflectivities (snow depth being used as the 
C weighting factor), and assume the snow is melting

      ELSEIF (ITGREF.EQ.3) THEN
 
C Get current day and hour and see if snow properties need to be reevaluated.
        IDYAUX=IDYP                    ! day of year
        IF(IHRF.EQ.1) IDYAUX=IDYF      ! end of day: use next day
        CALL EDAYR(IDYAUX,ID,IM)       ! calculate day and month
        IF (IDYAUX.NE.IDYLS.OR.IHRF.NE.IHRLS) THEN         ! need to refresh snow reflec.
        
C Read or calculate new snow depth.
          IBINSN=IFIL+41
          READ(IBINSN,REC=(IDYAUX-1)*24+IHRF,ERR=9000) SNDEP

C Determine whether there is snow melt.
          IF (SNDEP.GT.5.) THEN 
            CALL SNOWTEMP(SNDEP,SNMLT)
          ELSE
            SNMLT = .TRUE.
          ENDIF
          
C Recalculate snow reflectivity as if snow depth is greater than 5 cm.
          IF (SNDEP.GT.SNDEPLS) THEN
            SREFP=SREF
          ELSEIF (SNMLT) THEN
            SREFP=SREFLS*(.97**.04166666)
          ELSE
            SREFP=SREFLS*(.99**.04166666)
          ENDIF

C Calculate actual snow reflectivity, depending on snow depth.
          IF (SNDEP.GT.5.) THEN
            GREF = SREFP
          ELSE
            GREF=GREF12(IM)*(1.-SNDEP/5.)+SREFP*SNDEP/5.
          ENDIF
          
C Save snow reflectivity information.
          IDYLS=IDYAUX
          IHRLS=IHRF
          SNDEPLS=SNDEP
          SREFLS=SREFP
        ENDIF
      ENDIF

C Pass time step ground reflectivity for XML output.
      call add_to_xml_reporting(GREF,
     &  'Building/Ground_Reflectivity','units','dimensionless',
     &  'Reflectivity of the ground for solar radiation')

C Pass snow depth to XML output.
      call add_to_xml_reporting(SNDEP,
     &  'Climate/SnowDepth','units','cm',
     &  'Depth of the snow on the ground')

C-----------------------------------------------------------------------
C     TRACE
C-----------------------------------------------------------------------
      IF(ITC.LE.0.OR.ITRACE(19).EQ.0) GOTO 9999
      IF(NSINC.LT.ITC.OR.NSINC.GT.ITCF) GOTO 9999
      
C Subroutine and simulation time info      
      WRITE(OUTS,'(A,I4)')' Subroutine MZGREF   Trace output ',ICNT
      CALL EDISP(ITU,OUTS)
      ICNT=ICNT+1
      CALL DAYCLK(IDYP,BTIMEF,ITU)

C Print ground reflectivity and, if advanced model, snow depth ground. 
      IF (ITGREF.LE.2) THEN
        WRITE(OUTS,910) GREF
      ELSE
        WRITE(OUTS,920) GREF,SNDEP
      ENDIF
      CALL EDISP(ITU,OUTS)
      GOTO 9999
  910 FORMAT(' Ground reflectivity: ',F6.3)
  920 FORMAT(' Ground reflectivity: ',F6.3,'  Snow depth: ',F5.1,' cm')
      
C-----------------------------------------------------------------------
C     END OF FUNCTION
C-----------------------------------------------------------------------

 9000 CONTINUE
      call tstamp('>','SIM: Error reading binary snow depth file')
      call epagend
      STOP
 
 9999 CONTINUE
      RETURN
      END

C ***** PPGREF *********************************************************
C Created by: Didier Thevenard
C Initial Creation Date: April 2005
C Copyright 2000: NRCan Buildings Group

C Subroutine pre-processes the snow depth file when the 'advanced'
C ground reflectivity model is chosen
C The subroutine returns IER=0 if everything OK, 1 otherwise

C OUTPUTS:
C       IER  0 if everything OK 1 otherwise         
C *********************************************************************
      SUBROUTINE PPGREF(IER)

      IMPLICIT NONE

C-----------------------------------------------------------------------
C     DECLARATIONS
C-----------------------------------------------------------------------
      
C ESP-r Commons
      INTEGER IXPOS
      REAL GREF         ! actual snow covered ground reflectivity 
                        ! for the hour
      INTEGER ITGREF    ! flag for ground reflectivity model used
      REAL GREF12       ! no snow ground reflectivity for each month
      REAL SREF         ! uncorrected snow covered ground reflectivity
      INTEGER NSNOW     ! monthly number of days with snow on the ground
      CHARACTER*72 SNFNAM ! snow depth file name       
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM

      INTEGER IFIL      ! Unit number used for processing input/output files      
      COMMON/FILEP/IFIL

      common/pophelp/h(60)
      CHARACTER H*72
      
C Local variables      
      CHARACTER OUTS*124
      LOGICAL FOUND,OK,FEXIST
      REAL SNDEP        ! snow depth (cm)
      INTEGER ICOUNT    ! number of data items into snow depth file
      INTEGER IER       ! flag indicating if snow depth file was read successfully
      INTEGER IFILSN    ! unit number associated with snow depth ascii file
      INTEGER ISTAT
      INTEGER IBINSN    ! unit number associated with binary file

C Set default error code and return if not advanced albedo model
      IER=0
      IF (ITGREF.NE.3) RETURN

C CHECK EXISTENCE OF FILE, ASK FOR NEW FILE IF NOT FOUND, EXIT WITH
C ERROR IF CANNOT BE FOUND
      CALL FINDFIL(SNFNAM,FOUND)
      IF (.NOT.FOUND) THEN
        DO WHILE(.NOT.FOUND)
          WRITE(OUTS,'(A,A)') 'Could not find snow depth file: ',SNFNAM
          CALL EASKOK(OUTS, 
     &    ' Respecify? (simulator will exit if [no]!)',OK)
          IF (OK) THEN
            H(1)='The snow depth file contains hourly values '
            H(2)='of snow depth. Data should be in a one-column '
            H(3)='ASCII file, containing snow depth expressed in '
            H(4)='cm for all 8760 hours of the year. '
            CALL EASKS(SNFNAM,'Enter name of snow depth file',
     &        ' ',72,' ','Snow depth file',IER,4)
            CALL FINDFIL(SNFNAM,FOUND)
          ELSEIF(.NOT.OK) THEN
            call tstamp('>','SIM: snow depth file cannot be found')
            call epagend
            STOP
          ENDIF
        END DO
      ENDIF
      
C-----------------------------------------------------------------------
C     PREPROCESS SNOW DEPTH FILE: READ VALUES FROM ASCII FILE AND 
C     WRITE THEM TO BINARY FILE
C-----------------------------------------------------------------------

C Open snow depth ASCII file.
      IFILSN=IFIL+40
      CALL ERPFREE(IFILSN,ISTAT)
      CALL EFOPSEQ(IFILSN,SNFNAM,1,IER)
      IF (IER.NE.0) THEN
        call tstamp('>','SIM: snow depth file cannot be found')
        call epagend
        STOP
      ENDIF
      
C Open snow depth binary file.      
      IBINSN=IFIL+41
      CALL ERPFREE(IBINSN,ISTAT)
      INQUIRE(UNIT=IBINSN,EXIST=FEXIST)
      IF(FEXIST) CALL EFDELET(IBINSN,ISTAT)
      OPEN(IBINSN,STATUS='SCRATCH',FORM='UNFORMATTED',
     &  ACCESS='DIRECT',RECL=4,ERR=950)
      
C Loop on data from ascii file and read data and write into binary form.      
      ICOUNT=0
      DO WHILE(ICOUNT.LE.8760)
        READ(IFILSN,*,END=900,ERR=950) SNDEP
        ICOUNT=ICOUNT+1
        WRITE(IBINSN,REC=ICOUNT,ERR=950) SNDEP
      END DO
  900 CONTINUE
      IF (ICOUNT<8760) THEN
        call tstamp('>',
     &   'SIM: snow depth file has fewer than 8760 values')
        call epagend
        STOP
      ENDIF

C Close ASCII file and return.
      CALL ERPFREE(IFILSN,ISTAT)
      RETURN
      
C Error processing.
  950 CONTINUE
      call tstamp('>','SIM: Error processing snow depth file')
      call epagend
      STOP

      END

C ******* SNOWTEMP ****************************************************
C Created by: Didier Thevenard
C Initial Creation Date: April 2005
C Copyright 2000: NRCan Buildings Group

C Subroutine calculates whether snow melting takes place
C The subroutine requires SNDEP, the current snow depth, as input. It
C provides an indicator or whether snow melt is taking place, SNMLT, as
C ouput.
C Note that because of where the call to MZGREF is located, this 
C subroutine ends up using snow depth values, etc. from the previous
C time step. That's OK since we are just interested in approximate
C values of the snow temperature

C references: "Development and Implementation of a Ground Reflectivity
C              for ESP-r" Prepared Levelton Consultants, Richmond B.C.,
C              for Natural Resources Canada, June 30th 2005.

C INPUTS:   
C       SNDEP  snow depth for the time step (cm)

C OUTPUTS:
C       SNMLT  logical variable indicating whether there is snow melt 
C              (TRUE) and no snow melt (FALSE)         
C *********************************************************************
      SUBROUTINE SNOWTEMP(SNDEP,SNMLT)

      IMPLICIT NONE

C-----------------------------------------------------------------------
C     DECLARATIONS
C-----------------------------------------------------------------------
      
C ESP-r Commons      
      REAL GTP,GTF,QGLP,QGLF,QGGP,QGGF,GC,
     &TSKY,CLOUDC,CLOUD1,CLOUD2,CLOUD3,TBAVE,TBROOF,TBNRTH,
     &TBSUTH,TBEAST,TBWEST
      COMMON/ELWE3/GTP(6),GTF(6),QGLP,QGLF,QGGP,QGGF,GC(6,4),
     &TSKY,CLOUDC,CLOUD1,CLOUD2,CLOUD3,TBAVE,TBROOF,TBNRTH,
     &TBSUTH,TBEAST,TBWEST

      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      INTEGER IXPOS
      REAL GREF         ! actual snow covered ground reflectivity for the hour
      INTEGER ITGREF    ! flag for ground reflectivity model used
      REAL GREF12       ! no snow ground reflectivity for each month
      REAL SREF         ! uncorrected snow covered ground reflectivity
      INTEGER NSNOW     ! monthly number of days with snow on the ground
      CHARACTER*72 SNFNAM ! snow depth file name        
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM

      REAL SAZI,SALT    ! solar azimuth and altitude in degrees
      INTEGER ISUNUP    ! flag indicating whether sun is up or down
      COMMON/SUNPOS/SAZI,SALT,ISUNUP

      INTEGER IRTYPE
      COMMON/DNORGH/IRTYPE

C Arguments
      REAL SNDEP        ! snow depth (cm)
      LOGICAL SNMLT     ! flag for whether there is snow melt or not

C Local variables
      REAL QT           ! total solar radiation on snow surface
      REAL TSNOW        ! surface temperature of snow
      REAL ESIND        ! fortran sine function for variables in degrees

C Constants
      REAL ZEROK,SBC,GHTC,KSNOW,ESNOW,CMTOM
      DATA ZEROK/273.15/          ! 0°C in Kelvin 
      DATA SBC/5.6697E-08/        ! Stefan-Botlzmann constant, W/K4/m2
      DATA GHTC/10./              ! Convection coefficient, W/m2/K
      DATA KSNOW/0.12/            ! Thermal conductivity of snow, W/m/K
      DATA ESNOW/0.85/            ! Emissivity of snow
      DATA CMTOM/0.01/            ! Conversion constant from cm to m

C-----------------------------------------------------------------------
C     CALCULATE SNOW SURFACE TEMPERATURE THROUGH ENERGY BALANCE
C-----------------------------------------------------------------------

C Calculate incident solar radiation on the surface of the snow.
      QT=QFF+QDF*ESIND(SALT)      ! incident solar radiation
      IF(IRTYPE.EQ.1) QT=QDF

C Calculate hypothetical snow temperature and detect melting
C The snow surface temperature is deduced through an energy
C balance on the surface of the snow. This energy balance
C accounts for the solar radiation reaching the snow surface
C (direct and diffuse), convection heat transfer between the
C snow surface and the ambient air, longwave radiation exchange
C with the sky, conduction through the snow layer, and conduction
C to the ground surface.
      TSNOW=((1.-GREF)*QT+4*ESNOW*SBC*(TSKY**4)
     &       +GHTC*(TP+ZEROK)+(KSNOW/(SNDEP*CMTOM))*(GTF(1)+ZEROK))
     &      /(4*ESNOW*SBC*(TSKY**3)+GHTC+KSNOW/(SNDEP*CMTOM))
      TSNOW=TSNOW-ZEROK   
      IF (TSNOW.GT.0.) THEN
        SNMLT=.TRUE.
      ELSE
        SNMLT=.FALSE.
      ENDIF
      
      RETURN
      END      
