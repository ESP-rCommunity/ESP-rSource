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
C      MZGREF
C      PPGREF
C      MZSCTL

C ******************** MZSOLR ********************
C MZSOLR initiates the computation of the solar gains impinging
C on internal and external surfaces.

      SUBROUTINE MZSOLR(ICOMP)

#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

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
      JCONF=IZSTOCN(ICOMP,1)
      JCONL=IZSTOCN(ICOMP,NC)

      ISURF=0

C Connection-by-connection. Note: if JCONF was not reset
C in the loop above the zero start of loop then ict(J) will
C be out of range.

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
#include "geometry.h"
#include "CFC_common.h"
#include "net_flow.h"
#include "tdf2.h"
C ADD NEW PARAMETER STATEMENT
C MSTMC is number of different bidirectional datasets, limited
C to 20 for the present.
C MSGAL is number of optical sets per optics db item. At present
C set to 1: later increase to 2 for an alternative set that can be switched.
C MANH and MANV are the number of angles at which optical data is
C held for horizontal/vertical grid data.
C At present this assumes 5deg bi-directional data from -90 to +90
C in elevation and azimuth.
      PARAMETER (MSTMC=20,MSGAL=40,MANH=37,MANV=37)

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/PREC1/SNA(MCOM,MS)
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
     & POF(MCOM,MS,MT),IGCI(MCOM,MS),insst(mcom,mgt,mt,misur),
     & pinsst(mcom,mgt,mt,misur)

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

      COMMON/SOLEXT/EXRAD(MS),EXDIR(MS),EXDIF(MS),EXSHAD(MS),
     &                                                    EXSHADF(MS)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/VERSOL/ipkzon(mzs),ipksf(mzs),ivsolfil,vsol,VTSOL,vsnam

C Daylight coeff specific common block data definition:
      COMMON/DCOEF/NDCFP,NDCFS(MDCFP),DCOEF(MDCFP,MDCFS,MDCFV),
     & IDCFID(MCOM,MLCOM,MDF)

C Commons for summary of solar (for results library) and to save
C future for next timestep.
      common/solsum/q1adjz(2),q1outs(2),q2adjz(2),q2lost(2),q2tmc(2),
     &  q2wall(2),q2rem(2),q2cfc(2)
      common/solsmf/q1adjf(MCOM),q1outf(MCOM),q2adjf(MCOM),
     &  q2losf(MCOM),q2tmcf(MCOM),q2walf(MCOM),q2remf(MCOM),
     &  q2cfcf(MCOM)
      common/solsmp/q1adjp(MCOM),q1outp(MCOM),q2adjp(MCOM),
     &  q2losp(MCOM),q2tmcp(MCOM),q2walp(MCOM),q2remp(MCOM),
     &  q2cfcp(MCOM)
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
      integer IZSTOCN  ! for each zone:surface a pointer to connection index.
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
     &  THTSOB(MSTMC,MSGAL),TMABSDIF(MSTMC,MSGAL,ME),
     &  TMGVALUE(MSTMC,MSGAL,MANH,MANV),
     &  TMREFLECT(MSTMC,MSGAL,MANH,MANV),TUVALUE(MSTMC,MSGAL),
     &  TREXTERNAL(MSTMC,MSGAL),TRINTERNAL(MSTMC,MSGAL),
     &  TMVISUAL(MSTMC,MSGAL)

C Common block for treatment of diffuse radiation for bi-directional
C types 2 and 3.
      COMMON/OPTDAT2/DF_TX_SKY(MSTMC,MSGAL),DF_G_SKY(MSTMC,MSGAL),
     &  DF_RF_SKY(MSTMC,MSGAL),DF_TX_GND(MSTMC,MSGAL),
     &  DF_G_GND(MSTMC,MSGAL),DF_RF_GND(MSTMC,MSGAL),
     &  a_dfSKYinner(MSTMC,MSGAL),a_dfGNDinner(MSTMC,MSGAL),
     &  a_dfSKYouter(MSTMC,MSGAL),a_dfGNDouter(MSTMC,MSGAL)

C Required for WATSUN-PV model
      COMMON/PVANG/fInc_angle(MS)

      REAL fInc_angle          ! Angle of incidence for surface

C Local variables for CFCs
      real proang
      real TRANSBB_SYS,TRANSD_SYS,REFL_SYS,CFCabs
      real TRANSBB_SYSV,TRANSD_SYSV,REFL_SYSV,CFCabsV
      real TRANSD_SYS_BD,TRANSD_SYS_SKY,TRANSD_SYS_GRD
      real dummy,dummy2
      character ctmp*2

      integer iSolVis ! switch for CFC solar / visual data calculation
      integer i_direct ! switch for calling solar_multilayer

C ...
      logical VTSOL,vsolfl,closea,neglum

      logical bCloseToZero     ! Boolean used for comparison to zero

      character outs*124,vsnam*72

      DIMENSION QIABS(MS),IANS(MS),IND1(MS),IND2(MS),ANGI(MS),
     &SRADDO(MS),SRADF(MS),SKYDIF(MS),GRDDIF(MS)
      DIMENSION PSAZI2(MS),PSELV2(MS),IANGBI1(MS),IANGBI2(MS)
      dimension inssur(misur),pinsur(misur)

C Daylight coeff specific
      dimension alt(8)

C tdtrace is the direct transmission at the current timestep for each surface.
C tfftrace is the diffuse transmission at the current timestep for each surface.
      dimension tdtrace(MS),tfftrace(MS)

      dimension AbsSol(me),AbsVis(me)
      dimension AbsSolDir(me),AbsSolDifSky(me),AbsSolDifGrd(me)
      dimension EXDIFSKY(ms),EXDIFGRD(ms)

C Definitions for daylight coefficient patch altitude:
      data alt/6.,18.,30.,42.,54.,66.,78.,90./

C Bidirectional data - explicit definitions:
      COMMON/BITYPE/IBIDATATYPE
      integer IBIDATATYPE !this will probably have to be kept in a common block
                          !It will specify what type of data to read
                          !1: the existing transmittance and at different layers absorptance
                          !2: the transmittance, g value and reflectance
                          !3: the transmittance and g value only

      REAL EFF_TOT_ABS,XREFLECT_interp,XGVALUE_interp,qi_bidirectional
      REAL a_inner_layer,Rs_glazing,a_outer_layer, X_tr_interp
      REAL X_tr1_bitype2and3,X_tr2_bitype2and3,X_tr3_bitype2and3
      REAL X_tr4_bitype2and3 !Using it for the interpolation of transmittance in...
                             !cases of bidirectional data type 2 and 3. This is...
                             !a different variable than the one used for type 1
                             !(i.e. XX1, XX2, XX3, XX4) to avoid mixing up in complex
                             !situations.
      integer IBISET_timestep !the bidirectional dataset number to use.
                              !Use the number specified in tdf if exists. Otherwise use the 1st dataset.

      DIMENSION VAL(MBITS+2)
      real sumDirDiff !the sum of the direct and diffuse components for use in tdf (vertical solar)

C Fluid properties to use if a zone is not air filled (set via
C 'simulation toggles' menu in bps). zSWAp and zSWAf are the
C zone fluid solar absorptions at the present and future time
C rows respectively.
      COMMON/zfluid/znotair(mcom),zfldK,zfldD,zfldC,zfldA,
     &              zSWAp(mcom),zSWAf(mcom)
      logical znotair

C PCM state - switching of TMC properties
      common/spmfxst/ispmxist,spflnam 
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 

      character spflnam*72

      real fPcmStateFlag !temporarily converting to a real the flag for  
                         !the PCM state (solid, liquid, mixed) 
 
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)

      character strspm*16,unitspm*16,txtspm*72
    ! dataspm(??,1) contains the flag that indicates the
    ! state of PCM at each timestep 
    ! if iPcmStateFlag is 1: solid, if 2: mixed, if 3: liquid 

      ZAREA=zonetotsurfacearea(ICOMP)

C Debug.
C      write(6,*) 'solar ',btimef

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
        q2cfc(1) =q2cfcf(icomp)
        q2wall(1)=q2walf(icomp)
        q2rem(1) =q2remf(icomp)
        q1adjp(icomp)=q1adjf(icomp)
        q1outp(icomp)=q1outf(icomp)
        q2adjp(icomp)=q2adjf(icomp)
        q2losp(icomp)=q2losf(icomp)
        q2tmcp(icomp)=q2tmcf(icomp)
        q2walp(icomp)=q2walf(icomp)
        q2remp(icomp)=q2remf(icomp)
        zSWAp(icomp)=zSWAf(icomp)
      else
        q1adjz(1)=q1adjp(icomp)
        q1outs(1)=q1outp(icomp)
        q2adjz(1)=q2adjp(icomp)
        q2lost(1)=q2losp(icomp)
        q2tmc(1) =q2tmcp(icomp)
        q2cfc(1) =q2cfcp(icomp)
        q2wall(1)=q2walp(icomp)
        q2rem(1) =q2remp(icomp)
      endif
      q1adjz(2)=0.0
      q1outs(2)=0.0
      q2adjz(2)=0.0
      q2lost(2)=0.0
      q2tmc(2)=0.0
      q2cfc(2)=0.0
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

C Initialise zone fluid absorption to zero if not air.
      zSWAf(icomp)=0.0

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
        EXDIR(I)=0.0
        EXDIF(I)=0.0
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
          IF((ITMC.GE.1).or.(icfcfl(icomp,i).ge.1)) THEN
C           solar data
            transBB_S(icomp,i)=0.
            transD_S(icomp,i)=0.
            refl_S(icomp,i)=0.
C           visual data
            transBB_V(icomp,i)=0.
            transD_V(icomp,i)=0.
            refl_V(icomp,i)=0.

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

C Store angle of incidence in COMMON block variable; required by WATSUN-PV model
          fInc_angle(ISRF) = ANGI(ISRF)


C SOLAR PROCESSING: incident comprises direct, sky diffuse
C and ground reflected components. Calculate direct (SRADDO)
C and diffuse (SRADF) on surface ISRF.
          CALL MZSRAD(ICOMP,ISRF,QD,QF,CAI,SRADDO(ISRF),SRADF(ISRF),
     &         SKYDIF(ISRF),GRDDIF(ISRF))

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

C Replace calculated values by measured values from tdf file
C Use the ratio of direct/diffuse as calculated to divide
C the measured values into direct and diffuse components.
C Temporal (TDF) vertical solar radiation data
          if(IVERSOL(icomp,isrf).ne.0)then
            IFOC=IVERSOL(icomp,isrf)
            CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
            if(val(isd).gt.sradf(ISRF))then

C Sum direct and diffuse components
              sumDirDiff=sradf(ISRF)+sraddo(ISRF)
              call eclose(sumDirDiff,0.00,0.00001,closea)
              if (closea) then
                sumDirDiff=1.0
              endif
              radrat=sraddo(ISRF)/sumDirDiff
              sraddo(ISRF)=val(isd)*radrat
              sradf(ISRF)=val(isd)*(1.-radrat)
            else
              sraddo(ISRF)=0.0
              sradf(ISRF)=val(isd)
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
          POFF=0.0
        ELSE
          POO=PO(ICOMP,I,IHRF)
          POFF=POF(ICOMP,I,IHRF)
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
                ELSEIF(NBCTT(ICOMP,K,ITMC).EQ.-99) THEN

C Sensing time to swtich to alternative properties.
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
C transmission, td [W/m²],through TMC as if ibofot=0 (i.e. default
C optics).
                   xx1 = tmct(icomp,itmc,ind1(i))
                   if(ind2(i).eq.6) then
                      xx2 = 0.
                   else
                      xx2 = tmct(icomp,itmc,ind2(i))
                   endif

                   zero=0.
                   call mzwinp(xx1,1.,xx2,zero,ind1(i),angi(i),xd,
     &                                                          dummy)
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

                ELSEIF(NBCTT(ICOMP,K,ITMC).EQ.5) THEN 

C State of PCM activation. 
                  fPcmStateFlag=99.
                  if(ispmxist.gt.0)then ! special materials are defined
                    do ispm=1,nspmnod
                      ispmnod=ispm
                      ispmtype=ispmtyp(ispm,1)
                      if(ispmloc(ispm,1).eq.icomp) then
                        if (ispmloc(ispm,2).eq.I) then
                          if ((ispmtype.eq.53) .or.  ! legacy PCM model
c     &                        (ispmtype.eq.54) .or.  ! S. Hoffmann I  (obsolete, no corresponding code added)
c     &                        (ispmtype.eq.55) .or.  ! S. Hoffmann II (obsolete, no corresponding code added)
     &                        (ispmtype.eq.56)) then ! extended model with subcooling
C                             ! PCM is located in current zone & surface
                              fPcmStateFlag=dataspm(ispmnod,1) 
                          endif ! valid special material type
                        endif ! correct surface
                      endif ! correct zone
                    enddo ! loop registered special material nodes
                  else
                    ! should issue fatal error, here ...
                  endif
                  if (fPcmStateFlag .lt. 90.) then
                    write (6,*)'BACT 1     : ',BACTPT(ICOMP,K,ITMC)
                    write (6,*)'StateFlag 1: ',fPcmStateFlag   
                    call eclose(BACTPT(ICOMP,K,ITMC),
     &                          fPcmStateFlag,0.1,bCloseToZero)
                    IF (bCloseToZero) THEN 
                      write (6,*)'BACT     : ',BACTPT(ICOMP,K,ITMC)
                      write (6,*)'StateFlag: ',fPcmStateFlag   
                      IBOFOT(ICOMP,I)=1   ! "blinds are down", i.e. alternative properties active for 
                                          ! zone "ICOMP" and surface "I" ...
                                          ! BUT: we need 3 possible positions ... ?
                      ECRAT(ICOMP,I)=0.0  ! ??
                      NBPONT(ICOMP,I)=K   ! control period "K" ... ??
                    ELSE ! ??
C                      ... do nothing 
                    ENDIF                 
                  endif ! PCM exists

                ELSE ! NBCTT .ne. 1, 2, 3, 4 or 5

C Radiation activation; check which surface the sensor is on;
C - if IBCSUR()=0, then current surface.
C POO/POO2 contains shading factor on corresponding opaque surface.
                  ISUR=IBCSUR(ICOMP,ITMC)
                  IF(ISUR.EQ.0)THEN
                    RAD=SRADDO(I)*(1.-POO)+SRADF(I)*(1.-POFF)
                  ELSE
                    IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.2)THEN
                      POO2=0.0
                      POFF2=0.0
                    ELSE
                      POO2=PO(ICOMP,ISUR,IHRF)
                      POFF2=POF(ICOMP,ISUR,IHRF)
                    ENDIF
                    RAD=SRADDO(ISUR)*(1.-POO2)+SRADF(ISUR)*(1.-POFF2)
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

C Debug.
C        write(6,'(a,2i4)') 'Blind state: ',IBOFOT(ICOMP,I),
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
     &      SRADF(I)*AE(ICOMP,I)*(1.-POF(ICOMP,I,IHRF))
          ENDIF
          EXSHAD(I)=PO(ICOMP,I,IHRF)
          EXSHADF(I)=POF(ICOMP,I,IHRF)
          EXDIR(I)=SRADDO(I)*(1.-EXSHAD(I))
          EXDIF(I)=SRADF(I)*(1.-EXSHADF(I))
          EXRAD(I)=EXDIR(I)+EXDIF(I)
C This code ensures that EXDIR and EXDIF are zero if EXRAD is zero.
          call eclose(EXRAD(I), 0.0,0.001,bCloseToZero)
          if ( bCloseToZero ) then
             EXDIR(I) = 0.
             EXDIF(I) = 0.
          endif
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

C Bidirectional case
            if(IBIDIR.NE.0)then

C Call subroutine for controls here
              call mzsctl(ICOMP,I,SRADDO(I),SRADF(I),IBISET_timestep)

C Here it checks if during the control a dataset has been specified
C that is not included in the file with the data
C (for which there are no data). This is done by looking if there are
C data for internal surface resistance and also U value.
C If these values are 0 then it uses the 1st dataset and ignores the
C control.
              call eclose(TRINTERNAL(IBIDIR,IBISET_timestep),0.0,0.001,
     &                    bCloseToZero)
              if (bCloseToZero)then
                call eclose(TUVALUE(IBIDIR,IBISET_timestep),0.0,0.001,
     &                    bCloseToZero)
                if (bCloseToZero)then
                  write(outs,'(A,I2,A)')
     &             'Attempt is made to control dataset no: ',
     &             IBISET_timestep,
     &             ' while there are no data for this dataset.'
                  call edisp(iuout,outs)
                  write(outs,'(A)')
     &             'Dataset no: 1 will be used instead of this.'
                  call edisp(iuout,outs)
                  IBISET_timestep=1
                endif
              endif

C Interpolate transmittance and g values in case of bidirectional
C data type 2 and 3. Also interpolate reflectance in the case of
C data type 3. Do this outside the loop of NE (per layer).
              IF(IBIDATATYPE.EQ.2.OR.IBIDATATYPE.EQ.3)THEN
                X_tr1_bitype2and3=
     &               TMTSOD(IBIDIR,IBISET_timestep,IND1(I),IND2(I))
                XGVALUE1=
     &               TMGVALUE(IBIDIR,IBISET_timestep,IND1(I),IND2(I))
                IF(IBIDATATYPE.EQ.2)THEN

C Need to interpolate the reflectance for bidirectional data
                  XX1=TMREFLECT(IBIDIR,IBISET_timestep,IND1(I),IND2(I))
                ENDIF
                IF(IND1(I).eq.MANH)THEN
                  X_tr2_bitype2and3=X_tr1_bitype2and3
                  XGVALUE2=XGVALUE1
                  IF(IBIDATATYPE.EQ.2)THEN
                    XX2=XX1
                  ENDIF
                ELSE
                  X_tr2_bitype2and3=
     &             TMTSOD(IBIDIR,IBISET_timestep,IND1(I)+1,IND2(I))
                  XGVALUE2=
     &             TMGVALUE(IBIDIR,IBISET_timestep,IND1(I)+1,IND2(I))
                  IF(IBIDATATYPE.EQ.2)THEN
                    XX2=
     &               TMREFLECT(IBIDIR,IBISET_timestep,IND1(I)+1,IND2(I))
                  ENDIF
                ENDIF
                IF(IND2(I).eq.MANV)THEN
                  X_tr3_bitype2and3=X_tr1_bitype2and3
                  XGVALUE3=XGVALUE1
                  IF(IBIDATATYPE.EQ.2)THEN
                    XX3=XX1
                  ENDIF
                ELSE
                  X_tr3_bitype2and3=
     &             TMTSOD(IBIDIR,IBISET_timestep,IND1(I),IND2(I)+1)
                  XGVALUE3=
     &             TMGVALUE(IBIDIR,IBISET_timestep,IND1(I),IND2(I)+1)
                  IF(IBIDATATYPE.EQ.2)THEN
                    XX3=
     &               TMREFLECT(IBIDIR,IBISET_timestep,IND1(I),IND2(I)+1)
                  ENDIF
                ENDIF
                IF(IND1(I).eq.MANH)THEN
                  X_tr4_bitype2and3=X_tr3_bitype2and3
                  XGVALUE4=XGVALUE3
                  IF(IBIDATATYPE.EQ.2)THEN
                    XX4=XX3
                  ENDIF
                ELSEIF(IND2(I).eq.MANV)THEN
                  X_tr4_bitype2and3=X_tr2_bitype2and3
                  XGVALUE4=XGVALUE2
                  IF(IBIDATATYPE.EQ.2)THEN
                    XX4=XX2
                  ENDIF
                ELSE
                  X_tr4_bitype2and3=
     &             TMTSOD(IBIDIR,IBISET_timestep,IND1(I)+1,IND2(I)+1)
                  XGVALUE4=
     &             TMGVALUE(IBIDIR,IBISET_timestep,IND1(I)+1,IND2(I)+1)
                  IF(IBIDATATYPE.EQ.2)THEN
                     XX4=
     &             TMREFLECT(IBIDIR,IBISET_timestep,IND1(I)+1,IND2(I)+1)
                  ENDIF
                ENDIF

C Call MZWINP2 to interpolate in two dimensions for bidirectional data.
C X_tr_interp should be the same as XD that is used in the TMC loop afterwards.
                CALL MZWINP2(X_tr1_bitype2and3,X_tr2_bitype2and3,
     &                    X_tr3_bitype2and3,X_tr4_bitype2and3,
     &                    IANGBI1(I),IANGBI2(I),PSAZI2(I),PSELV2(I),
     &                    X_tr_interp)
                CALL MZWINP2(XGVALUE1,XGVALUE2,XGVALUE3,XGVALUE4,
     &                    IANGBI1(I),IANGBI2(I),PSAZI2(I),PSELV2(I),
     &                    XGVALUE_interp)
                IF(IBIDATATYPE.EQ.2)THEN
                  CALL MZWINP2(XX1,XX2,XX3,XX4,IANGBI1(I),IANGBI2(I),
     &                    PSAZI2(I),PSELV2(I),XREFLECT_interp)
                ENDIF
C End of interpolation for bidirectional data

C Get effective total absorptance from the interpolation of the solar
C transmittance according to a method defined by Fraunhofer.
C Do it only in cases of bidirectional datatypes 2 and 3 (g,te,rho
C or g,te only)
                IF(IBIDATATYPE.EQ.2)THEN
                  EFF_TOT_ABS=1.-X_tr_interp-XREFLECT_interp
                ELSEIF(IBIDATATYPE.EQ.3)THEN
                  EFF_TOT_ABS=0.5*(1.-X_tr_interp)
                ENDIF

C Solar flux for bidirectional data (experimental conditions)
C will be g value - transmittance
                qi_bidirectional=XGVALUE_interp-X_tr_interp

C Calculate the thermal resistance of the glazing based on the U values
C Re and Ri values defined in the bidirectional data file
C Trap also the zeros in case of corrupted data (will result to
C a non-realistic Rs_glazing)
                call eclose(TUVALUE(IBIDIR,IBISET_timestep),0.0,0.001,
     &                bCloseToZero)
                if(bCloseToZero)then
                  Rs_glazing=0.-TREXTERNAL(IBIDIR,IBISET_timestep)-
     &               TRINTERNAL(IBIDIR,IBISET_timestep)
                else
                  Rs_glazing=(1./TUVALUE(IBIDIR,IBISET_timestep))-
     &               TREXTERNAL(IBIDIR,IBISET_timestep)-
     &               TRINTERNAL(IBIDIR,IBISET_timestep)
                endif

C Calculate the effective absorptance of the inner layer (based on
C the experimental bidirectional data)
C abs_in=(qi*(Re+Rs+Ri)-abs_tot*Re)/Rs
C Trap also the zeros in case of corrupted data
                call eclose(Rs_glazing,0.0,0.001,bCloseToZero)
                if(bCloseToZero)then
                  a_inner_layer=0.
                else
                  a_inner_layer=
     &            (qi_bidirectional*(TREXTERNAL(IBIDIR,IBISET_timestep)+
     &            Rs_glazing+TRINTERNAL(IBIDIR,IBISET_timestep))-
     &            EFF_TOT_ABS*TREXTERNAL(IBIDIR,IBISET_timestep))/
     &            Rs_glazing
                endif

C Calculate the effective absorptance of the outer layer (based on
C the experimental bidirectional data)
C abs_out=abs_tot*(Re+Rs)-qi(Re+Rs+Ri)/Rs
C Trap also the zeros in case of corrupted data
                call eclose(Rs_glazing,0.0,0.001,bCloseToZero)
                if(bCloseToZero)then
                  a_outer_layer=0.
                else
                  a_outer_layer=
     &            (EFF_TOT_ABS*(TREXTERNAL(IBIDIR,IBISET_timestep)+
     &            Rs_glazing)-qi_bidirectional*
     &            (TREXTERNAL(IBIDIR,IBISET_timestep)+Rs_glazing+
     &            TRINTERNAL(IBIDIR,IBISET_timestep)))/Rs_glazing
                endif

C Debug.
C              write(6,*) Rs_glazing,'  ',qi_bidirectional
C              write(6,*) a_inner_layer,'  ', a_outer_layer
C End Debug

C End of Bidirectional case
              endif

            ENDIF
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
                    IF(IBIDATATYPE.EQ.1)THEN
C XX1 is the absorptance for those available array elements that
C are less than the actual solar azimuth and elevation angles.
                      XX1=
     &                 TMABSO(IBIDIR,IBISET_timestep,J,IND1(I),IND2(I))
                    ENDIF
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

C Bidirectional case - TYPE 1 DATA  (absorptances are available)
C Note: if angle is +90, set XX2=XX1 etc.
                    IF(IBIDATATYPE.EQ.1)THEN
                      IF(IND1(I).eq.MANH)then
                        XX2=XX1
                       ELSE
                        XX2=
     &                  TMABSO(ITMC,IBISET_timestep,J,IND1(I)+1,IND2(I))
                      ENDIF
                      IF(IND2(I).eq.MANV)then
                        XX3=XX1
                      ELSE
                        XX3=
     &                  TMABSO(ITMC,IBISET_timestep,J,IND1(I),IND2(I)+1)
                      ENDIF
                      IF(IND1(I).eq.MANH)then
                        XX4=XX3
                      ELSEIF(IND2(I).eq.MANV)then
                        XX4=XX2
                      ELSE
                        XX4=
     &                TMABSO(ITMC,IBISET_timestep,J,IND1(I)+1,IND2(I)+1)
                      ENDIF
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
              IF(IBIDATATYPE.EQ.1)THEN

C Absorptance is included in the bidirectional data file
C Call MZWINP2 to interpolate in two dimensions for bidirectional data.
                CALL MZWINP2(XX1,XX2,XX3,XX4,IANGBI1(I),IANGBI2(I),
     &                     PSAZI2(I),PSELV2(I),ABD)

C Get diffuse absorptance.
                ABF=TMABSDIF(IBIDIR,IBISET_timestep,J)

C Bidirectional data with transmittance and g values (plus reflectance
C in some cases)
              ELSEIF(IBIDATATYPE.EQ.2.OR.IBIDATATYPE.EQ.3)THEN

C Always applicable for a 3-layer glazing.
C For external layer assign the calculated absorptance on the outer layer.
                IF(J.EQ.1)THEN
                  ABD=a_outer_layer

C Use separate absorptivities for diffuse radiation from outside to inside.
                  ABFSKY=a_dfSKYouter(IBIDIR,IBISET_timestep)
                  ABFGND=a_dfGNDouter(IBIDIR,IBISET_timestep)

C Use average absorptivity for diffuse radiation from inside to outside.
                  ABF=(ABFSKY+ABFGND)/2.

C Assign 0 for middle gas layer.                
                ELSEIF(J.EQ.2)THEN
                  ABD=0.0
                  ABFSKY=0.0
                  ABFGND=0.0
                  ABF=0.0

C For internal layer, assign the calculated absorptance on the inner layer.
                ELSEIF(J.EQ.3)THEN
                  ABD=a_inner_layer
                  ABFSKY=a_dfSKYinner(IBIDIR,IBISET_timestep)
                  ABFGND=a_dfGNDinner(IBIDIR,IBISET_timestep)
                  ABF=(ABFSKY+ABFGND)/2.
                ENDIF
              ENDIF
            ENDIF
              IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.2)THEN
                POO=0.
                POFF=0.
              ELSE
                POO=PO(ICOMP,I,IHRF)
                POFF=POF(ICOMP,I,IHRF)
              ENDIF
              DAB=SRADDO(I)*(1.-POO)*ABD
              FAB=SRADF(I)*(1.-POFF)*ABF
              IF(IBIDATATYPE.EQ.2.OR.IBIDATATYPE.EQ.3)THEN
                FAB=(SKYDIF(I)*ABFSKY+GRDDIF(I)*ABFGND)*(1.-POFF)
              ENDIF

              EXSHAD(I)=PO(ICOMP,I,IHRF)
              EXSHADF(I)=POF(ICOMP,I,IHRF)
              EXDIR(I)=SRADDO(I)*(1.-EXSHAD(I))
              EXDIF(I)=SRADF(I)*(1.-EXSHADF(I))
              EXRAD(I)=EXDIR(I)+EXDIF(I)

C Ensures that EXDIR and EXDIF are zero if EXRAD is zero.
             call eclose(EXRAD(I), 0.0,0.001,bCloseToZero)
             if ( bCloseToZero ) then
                EXDIR(I) = 0.
                EXDIF(I) = 0.
             endif

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

        icfctp=icfcfl(icomp,i)              !CFC type

C Bidirectional case
        if(IBIDIR.NE.0)then

C Call subroutine for controls here
          call mzsctl(ICOMP,I,SRADDO(I),SRADF(I),IBISET_timestep)

C Here it checks if during the control a dataset has been specified
C that is not included in the file with the data
C (for which there are no data). This is done by looking if there are
C data for internal surface resistance and also U value.
C If these values are 0 then it uses the 1st dataset and ignores the
C control.
          call eclose(TRINTERNAL(IBIDIR,IBISET_timestep),0.0,0.001,
     &                bCloseToZero)
          if (bCloseToZero)then
            call eclose(TUVALUE(IBIDIR,IBISET_timestep),0.0,0.001,
     &                  bCloseToZero)
            if (bCloseToZero)then
              write(outs,'(A,I2,A)')
     &         'Attempt is made to control dataset no: ',
     &         IBISET_timestep,
     &         ' while there are no data for this dataset.'
              call edisp(iuout,outs)
              write(outs,'(A)')
     &         'Dataset no: 1 will be used instead of this.'
              call edisp(iuout,outs)
              IBISET_timestep=1
            endif
          endif

C End of bidirectional case
        endif

        ITMC=ITMCFL(ICOMP,I)

        if (IE(ICOMP,I).ge.0.and.(ITMC.gt.0.or.icfctp.gt.0)) then

         if(ITMC.gt.0)then


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
                STMIN=TMCT2(ICOMP,ITMC,IND1(I),KP)
                STMAX=TMCT(ICOMP,ITMC,IND1(I))
                XX1=STMIN + (STMAX - STMIN)*ECRAT(ICOMP,I)
              ELSEIF(IBOFOT(ICOMP,I).EQ.1)THEN
                XX1=TMCT2(ICOMP,ITMC,IND1(I),KP)
              ELSE
                 IF(IBIDIR.EQ.0) THEN
                   XX1=TMCT(ICOMP,ITMC,IND1(I))
                 ELSE
C Bidirectional case for transmission - see similar treatment for absorption above.
C XX? is the direct-direct component; YY? is the direct-diffuse component
                   XX1=TMTSOD(IBIDIR,IBISET_timestep,IND1(I),IND2(I))
                   YY1=TMTSOB(IBIDIR,IBISET_timestep,IND1(I),IND2(I))
                 ENDIF
              ENDIF

              IF(IBIDIR.EQ.0.AND.IND2(I).EQ.6) THEN
                XX2=0.
              ELSE
                IF(IBOFOT(ICOMP,I).EQ.2)THEN
                  STMIN=TMCT2(ICOMP,ITMC,IND2(I),KP)
                  STMAX=TMCT(ICOMP,ITMC,IND2(I))
                  XX2=STMIN + (STMAX - STMIN)*ECRAT(ICOMP,I)
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
                    XX2=TMTSOD(IBIDIR,IBISET_timestep,IND1(I)+1,IND2(I))
                    YY2=TMTSOB(IBIDIR,IBISET_timestep,IND1(I)+1,IND2(I))
                  ENDIF
                  IF(IND2(I).eq.MANV)then
                    XX3=XX1
                    YY3=YY1
                  ELSE
                    XX3=TMTSOD(IBIDIR,IBISET_timestep,IND1(I),IND2(I)+1)
                    YY3=TMTSOB(IBIDIR,IBISET_timestep,IND1(I),IND2(I)+1)
                  ENDIF
                  IF(IND1(I).eq.MANH)then
                    XX4=XX3
                    YY4=YY3
                  ELSEIF(IND2(I).eq.MANV)then
                    XX4=XX2
                    YY4=YY2
                  ELSE
                    XX4=
     &                TMTSOD(IBIDIR,IBISET_timestep,IND1(I)+1,IND2(I)+1)
                    YY4=
     &                TMTSOB(IBIDIR,IBISET_timestep,IND1(I)+1,IND2(I)+1)
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
                STMIN=TMCT2(ICOMP,ITMC,2,KP)
                STMAX=TMCT(ICOMP,ITMC,2)
                YY1=STMIN + (STMAX - STMIN)*ECRAT(ICOMP,I)
                STMIN=TMCT2(ICOMP,ITMC,3,KP)
                STMAX=TMCT(ICOMP,ITMC,3)
                YY2=STMIN + (STMAX - STMIN)*ECRAT(ICOMP,I)
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
     &                       PSAZI2(I),PSELV2(I),XD)
              IF(IBIDATATYPE.EQ.1)THEN
                CALL MZWINP2(YY1,YY2,YY3,YY4,IANGBI1(I),IANGBI2(I),
     &                       PSAZI2(I),PSELV2(I),DTOD)

C Get diffuse transmission from common block.
                XF=THTSOB(IBIDIR,IBISET_timestep)
              ELSEIF(IBIDATATYPE.EQ.2.OR.IBIDATATYPE.EQ.3)THEN
                DTOD=0.

C Use separate transmittances for diffuse radiation from outside to inside.
                XFSKY=DF_TX_SKY(IBIDIR,IBISET_timestep)
                XFGND=DF_TX_GND(IBIDIR,IBISET_timestep)

C Use XF for diffuse radiation travelling from inside to outside.
                XF=(XFSKY+XFGND)/2.0
              ENDIF
            ENDIF
            IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.2)THEN
              POO=0.
              POFF=0.
            ELSE
              POO=PO(ICOMP,I,IHRF)
              POFF=POF(ICOMP,I,IHRF)
            ENDIF

C Calculate diffuse transmission.
            TFF=SRADF(I)*SNA(ICOMP,I)*XF*(1.-POFF)
            IF(IBIDATATYPE.EQ.2.OR.IBIDATATYPE.EQ.3)THEN
            TFF=(SKYDIF(I)*XFSKY+GRDDIF(I)*XFGND)*SNA(ICOMP,I)*(1.-POFF)
            ENDIF
            tfftrace(i)=tff

C For bidirectional data, add in the direct-diffuse component to the
C diffuse transmission.
            if(IBIDIR.NE.0)then
              TFF=TFF+SRADDO(I)*(1.-POO)*SNA(ICOMP,I)*DTOD
              tfftrace(i)=tff
            endif

C Calculate direct transmission.
            TD=SRADDO(I)*SNA(ICOMP,I)*(1.-POO)*XD
            tdtrace(i)=td

            QZNTOT=QZNTOT+TD+TFF
            q1outs(2)=q1outs(2)+td+tff

          ENDIF

C-----------------PROCESS FOR TYPE 3 SURFACE TMC
C                 ------------------------------
C Set TD, TFF for a type 3 surface (internal surface with adjacent zone).
          IF(IE(ICOMP,I).GT.0) THEN
            TD=DIRT(ICOMP,I)*SNA(ICOMP,I)
            tdtrace(i)=td
            TFF=DIFT(ICOMP,I)*SNA(ICOMP,I)
            tfftrace(i)=tff

            QZNTOT=QZNTOT+TD+TFF
            q1adjz(2)=q1adjz(2)+td+tff

            DIRT(ICOMP,I)=0.0
            DIFT(ICOMP,I)=0.0
            AIRT(ICOMP,I)=0.0
          ENDIF

C-----------------PROCESS FOR EXTERNAL CFC
C                 ------------------------------
         elseif(icfctp.gt.0)then

          if(IE(icomp,i).eq.0)then  !only external CFCs supported

            TD=0.0
            TFF=0.0
            tmpIDF=0.0  !interior diffuse source
            tmpIBM=0.0  !interior beam source
            i_direct=0

C Get shading data if it exists
            IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.2)THEN
              POO=0.
              POFF=0.
            ELSE
              POO=PO(ICOMP,I,IHRF)
              POFF=POF(ICOMP,I,IHRF)
            ENDIF

C Apply shading factor to external incident radiation
            EXDIR(I)    = SRADDO(I)*(1.-POO)
            EXDIF(I)    = SRADF(I)*(1.-POFF)
            EXDIFSKY(I) = SKYDIF(I)*(1.-POFF)
            EXDIFGRD(I) = GRDDIF(I)*(1.-POFF)

            IF(ians(i).eq.0)then  !surface is not self shaded

C Determine profile angle
             call profile_angle(icomp,i,icfctp,proang)

             proang_s(icomp,icfctp,i)=proang    !save profile angle for CFC output

C............Determine beam-beam, beam-diffuse and diffuse-diffuse solar and visual 
C            properties for each CFC layer. 
C            Glazing layers have incidence angle dependence. Slat-blind layers 
C            have profile angle dependency
C............SOLAR PROPERTIES
             iSolVis=0
             call cfc_eff_opt_prop(icomp,i,icfctp,angi(i),proang,
     &                                                     iSolVis)

C            Check whether sky and ground diffuse calc. is requested
             if (i_ground_sky_diff_calc.eq.1) then

C              Calculate CFC system solar BEAM transmitted
               call solar_multilayer(i_direct,icomp,i,icfctp,
     &                   EXDIR(I),tmpIDF,
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   TRANSBB_SYS,TRANSD_SYS_BD,dummy,AbsSolDir)

C              Calculate CFC system solar SKY diffuse transmitted
               call solar_multilayer(i_sky,icomp,i,icfctp,
     &                   tmpIBM,EXDIFSKY(I),
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   dummy,TRANSD_SYS_SKY,dummy2,AbsSolDifSky)

C              Calculate CFC system solar GROUND diffuse transmitted
               call solar_multilayer(i_ground,icomp,i,icfctp,
     &                   tmpIBM,EXDIFGRD(I),
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   dummy,TRANSD_SYS_GRD,dummy2,AbsSolDifGrd)

C              Sum up diffuse transmitted contributions (sky + ground)
               TRANSD_SYS = TRANSD_SYS_BD +
     &                      TRANSD_SYS_SKY + TRANSD_SYS_GRD 

C              Sum up solar absorbed contributions (direct + sky diff + ground diff)
               do jj=1,ncfc_el(icomp,icfctp)
                    AbsSol(jj)=AbsSolDir(jj) + 
     &                         AbsSolDifSky(jj) +
     &                         AbsSolDifGrd(jj)
               end do

             else
C              Legacy model.
C              Calculate CFC system solar beam transmitted, diffuse transmitted, total reflected
C              and absorbed fluxes in each CFC layer
               call solar_multilayer(i_direct,icomp,i,icfctp,
     &           EXDIR(I),EXDIF(I),
     &           tmpIBM,tmpIDF,iSolVis,TRANSBB_SYS,TRANSD_SYS,REFL_SYS,
     &           AbsSol)
             endif


C............REPEAT for VISUAL
             iSolVis=1

             call cfc_eff_opt_prop(icomp,i,icfctp,angi(i),proang,
     &                                                     iSolVis)

C            Check whether sky and ground diffuse calc. is requested
             if (i_ground_sky_diff_calc.eq.1) then

C              Calculate CFC system visual BEAM transmitted
               call solar_multilayer(i_direct,icomp,i,icfctp,
     &                   EXDIR(I),tmpIDF,
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   TRANSBB_SYSV,TRANSD_SYS_BD,dummy,AbsSolDir)

C              Calculate CFC system solar SKY diffuse transmitted
               call solar_multilayer(i_sky,icomp,i,icfctp,
     &                   tmpIBM,EXDIFSKY(I),
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   dummy,TRANSD_SYS_SKY,dummy2,AbsSolDifSky)

C              Calculate CFC system solar GROUND diffuse transmitted
               call solar_multilayer(i_ground,icomp,i,icfctp,
     &                   tmpIBM,EXDIFGRD(I),
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   dummy,TRANSD_SYS_GRD,dummy2,AbsSolDifGrd)

C              Sum up diffuse transmitted contributions (sky + ground)
               TRANSD_SYSV = TRANSD_SYS_BD +
     &                       TRANSD_SYS_SKY + TRANSD_SYS_GRD 
C              Absorbed visual is of no interest

             else 
C             Legacy model.
C             Calculate CFC system visual beam transmitted, diffuse transmitted.
              call solar_multilayer(i_direct,icomp,i,icfctp,
     &           EXDIR(I),EXDIF(I),
     &           tmpIBM,tmpIDF,iSolVis,TRANSBB_SYSV,TRANSD_SYSV,
     &           REFL_SYSV,AbsVis)
             endif

C...........surface is self-shaded
            ELSE

             temp1=0.
             temp2=0.

             proang_s(icomp,icfctp,i)=-1.    !save profile angle for CFC output, -1 indicates self shaded

C            Determine diffuse-diffuse solar properties for
C            each CFC layer. Incidence and profile angles set to 0 since self-shaded.

C............SOLAR PROPERTIES
             iSolVis=0

             call cfc_eff_opt_prop(icomp,i,icfctp,temp1,temp2,
     &                                                    iSolVis)

C            Check whether sky and ground diffuse calc. is requested
             if(i_ground_sky_diff_calc.eq.1)then

C              Calculate CFC system solar BEAM transmitted
               call solar_multilayer(i_direct,icomp,i,icfctp,
     &                   EXDIR(I),tmpIDF,
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   TRANSBB_SYS,TRANSD_SYS_BD,dummy,AbsSolDir)

C              Calculate CFC system solar SKY diffuse transmitted
               call solar_multilayer(i_sky,icomp,i,icfctp,
     &                   tmpIBM,EXDIFSKY(I),
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   dummy,TRANSD_SYS_SKY,dummy2,AbsSolDifSky)

C              Calculate CFC system solar GROUND diffuse transmitted
               call solar_multilayer(i_ground,icomp,i,icfctp,
     &                   tmpIBM,EXDIFGRD(I),
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   dummy,TRANSD_SYS_GRD,dummy2,AbsSolDifGrd)

C              Sum up diffuse transmitted contributions (sky + ground)
               TRANSD_SYS = TRANSD_SYS_BD +
     &                      TRANSD_SYS_SKY + TRANSD_SYS_GRD 

C              Sum up solar absorbed contributions (direct + sky diff + ground diff)
               do jjj=1,ncfc_el(icomp,icfctp)
                    AbsSol(jjj)=AbsSolDir(jjj) + 
     &                         AbsSolDifSky(jjj) +
     &                         AbsSolDifGrd(jjj)
               end do

             else
C              Legacy model.
C              Calculate CFC system solar beam transmitted, diffuse transmitted, 
C              total reflected and absorbed fluxes in each CFC layer.
               call solar_multilayer(i_direct,icomp,i,icfctp,EXDIR(I),
     &             EXDIF(I),tmpIBM,tmpIDF,iSolVis,TRANSBB_SYS,
     &             TRANSD_SYS,REFL_SYS,AbsSol)
             endif ! sky/ground or legacy

C............REPEAT for VISUAL
             iSolVis=1

             call cfc_eff_opt_prop(icomp,i,icfctp,temp1,temp2,
     &                                                    iSolVis)

C            Check whether sky and ground diffuse calc. is requested
             if (i_ground_sky_diff_calc.eq.1) then

C              Calculate CFC system visual BEAM transmitted (TRANSBB_SYSV)
               call solar_multilayer(i_direct,icomp,i,icfctp,
     &                   EXDIR(I),tmpIDF,
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   TRANSBB_SYSV,TRANSD_SYS_BD,dummy,AbsSolDir)

C              Calculate CFC system visual SKY diffuse transmitted
               call solar_multilayer(i_sky,icomp,i,icfctp,
     &                   tmpIBM,EXDIFSKY(I),
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   dummy,TRANSD_SYS_SKY,dummy2,AbsSolDifSky)

C              Calculate CFC system visual GROUND diffuse transmitted
               call solar_multilayer(i_ground,icomp,i,icfctp,
     &                    tmpIBM,EXDIFGRD(I),
     &                    tmpIBM,tmpIDF,iSolVis,
     &                    dummy,TRANSD_SYS_GRD,dummy2,AbsSolDifGrd)

C              Sum up diffuse transmitted contributions (sky + ground)
               TRANSD_SYSV = TRANSD_SYS_BD +
     &                       TRANSD_SYS_SKY + TRANSD_SYS_GRD 

c               TRANSD_SYSV = TRANSD_SYSV
C              Absorbed visual is of no interest

             else 
C              Legacy model.
C              Calculate CFC system VISUAL beam transmitted, diffuse transmitted.
               call solar_multilayer(i_direct,icomp,i,icfctp,
     &                 EXDIR(I),EXDIF(I),
     &                 tmpIBM,tmpIDF,iSolVis,TRANSBB_SYSV,TRANSD_SYSV,
     &                 REFL_SYSV,AbsVis)
             endif ! sky/ground or legacy

            END IF ! self-shaded?

C save multilayer results for CFC output
            issur_self_shd(icomp,i)=real(ians(i))
C solar data
            transBB_S(icomp,i)=TRANSBB_SYS!*SNA(ICOMP,I)
            transD_S(icomp,i)=TRANSD_SYS!*SNA(ICOMP,I)
            ! 5.5.12: this does not seem to be set when ground/sky diffuse mode is on?
            refl_S(icomp,i)=REFL_SYS!*SNA(ICOMP,I)

C visual data, normalise to radiation available for output/storing of 
C fraction. These fractions are used in "casual.F/DWINVT()" as light 
C transmittance values (direct/diffuse) - the fraction is based on 
C solar radiation with visual properties for CFC layers. 
            if (EXDIR(I).gt.0.) then
              transBB_V(icomp,i)=TRANSBB_SYSV/EXDIR(I)
            else
              transBB_V(icomp,i)=0.
            endif
            if (i_ground_sky_diff_calc.eq.1) then
              if ((EXDIR(I)+EXDIFSKY(I)+EXDIFGRD(I)).gt.0.) then
                transD_V(icomp,i)=TRANSD_SYSV/(EXDIR(I)+EXDIFSKY(I)
     &                                     +EXDIFGRD(I))
              else
                 transD_V(icomp,i)=0.
              endif
            else ! legacy
              if ((EXDIR(I)+EXDIF(I)).gt.0.) then
                 transD_V(icomp,i)=TRANSD_SYSV/(EXDIR(I)+EXDIF(I))
              else
                 transD_V(icomp,i)=0.
              endif
             
            endif

            refl_V(icomp,i)=REFL_SYSV

C Calculate total solar absorbed in CFC
            CFCabs=0.
            do 999 ij=1,ncfc_el(icomp,icfctp)
                CFCabs=CFCabs+AbsSol(ij)
  999       continue
            q2cfc(2)=q2cfc(2)+CFCabs*SNA(ICOMP,I)

C Assign multilayer results to TD and TFF
            TD=TRANSBB_SYS*SNA(ICOMP,I)
            TFF=TRANSD_SYS*SNA(ICOMP,I)
            tfftrace(i)=tff
            tdtrace(i)=td

C Save total transmitted from ambient for output
            QZNTOT=QZNTOT+TD+TFF
            q1outs(2)=q1outs(2)+td+tff

C Assign nodal absorptions to CFC layer nodes
            ne=nelts(icomp,i)
            inode=1
            do 200 j=1,ne
              nnod=nndl(icomp,i,j)
              fnnod=float(nnod)
              qtmca(i,inode,2)=qtmca(i,inode,2)+
     &                                  AbsSol(j)/(fnnod*2.)
              qtmca_ext(icomp,i,inode)=qtmca(i,inode,2)    !save for CFC output
              do 205 jj=2,nnod
                inode=inode+1
                qtmca(i,inode,2)=qtmca(i,inode,2)+
     &                                  AbsSol(j)/fnnod
                qtmca_ext(icomp,i,inode)=qtmca(i,inode,2) !save for CFC output

  205         continue
              inode=inode+1
              qtmca(i,inode,2)=qtmca(i,inode,2)+
     &                                  AbsSol(j)/(fnnod*2.)
              qtmca_ext(icomp,i,inode)=qtmca(i,inode,2)    !save for CFC output

  200       continue

          end if

         end if

C------------PROCESS INTERNAL SURFACES
C            -------------------------
C The direct (TD) and diffuse (TFF) solar radiation (W) entering
C the zone through the current TMC (perhaps internal) is now known.

C Determine shortwave flux absorbed by zone fluid if not air
C and depreciate TD and TFF correspondingly.
      if(znotair(icomp))then
        zSWAf(icomp)=zSWAf(icomp)+(TD+TFF)*zfldA
        TD=TD*(1.0-zfldA)
        if(td.lt.0.0)td=0.0
        TFF=TFF*(1.0-zfldA)
        if(tff.lt.0.0)tff=0.0
      endif

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

C If KS<0, but TD>0 (possible if at a particular time near sunrise or sunset
C for the date of insolation analysis, the sun is down, but at the same time of day
C later or earlier in that month, the sun is above the horizon - i.e. resulting
C from the fact that insolation analysis is undertaken at the mid-month)
C If this occurs, reassign the direct transmittance to be diffuse transmittance,
C since the distribution is unknown.
            if (KS.LT.0.and.TD.GT.0.0) then
               TFF=TFF+TD
               TD=0.0
            endif

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
              if(ITMCFL(ICOMP,KS).EQ.0.and.icfcfl(icomp,ks).eq.0)then

C A) OPAQUE SURFACE
                QIABS(KS)=QIABS(KS)+AI(ICOMP,KS)*X1
                QIREF=QIREF+(1.-AI(ICOMP,KS))*PAREA*X1
                q2wall(2)=q2wall(2)+x1*PAREA*AI(ICOMP,KS)

C B) CFC SURFACE (external only) - run solar_multilayer with inside 
C    diffuse source and adjust abosrptances
              elseif(icfcfl(icomp,ks).gt.0.and.ie(icomp,ks).eq.0)then
                icfctp=icfcfl(icomp,ks)

                temp1=0.
                temp2=0.
                temp3=0.

C INSIDE SOURCE IS DIFFUSE RADIATION
C Calculate CFC system solar beam transmitted, diffuse transmitted, total reflected
C and absorbed fluxes in each CFC layer.Here, the transmitted component is actually
C the reflected component on the inside surface since the source is from the
C inside.
                iSolVis=0
                call solar_multilayer(0,icomp,ks,icfcfl(icomp,ks),
     &          temp1,temp2,temp3,X1,iSolVis,TRANSBB_SYS,TRANSD_SYS,
     &          REFL_SYS,AbsSol)

                QIREF=QIREF+TRANSD_SYS*PAREA    !reflected
                q2lost(2)=q2lost(2)+REFL_SYS*PAREA !transmitted back to ambient(reporting)

C Add additional absorbed for output reporting.
                CFCabs=0.
                do 888 ij=1,ncfc_el(icomp,icfctp)
                    CFCabs=CFCabs+AbsSol(ij)
  888           continue
                q2cfc(2)=q2cfc(2)+CFCabs*PAREA

C Assign additional nodal absorptions to CFC layer nodes
                ne=nelts(icomp,ks)
                inode=1
                do 201 j=1,ne
                nnod=nndl(icomp,ks,j)
                fnnod=float(nnod)
                qtmca(ks,inode,2)=qtmca(ks,inode,2)+
     &                                  AbsSol(j)/(fnnod*2.)
                do 206 jj=2,nnod
                  inode=inode+1
                  qtmca(ks,inode,2)=qtmca(ks,inode,2)+
     &                                  AbsSol(j)/fnnod

  206           continue
                inode=inode+1
                qtmca(ks,inode,2)=qtmca(ks,inode,2)+
     &                                  AbsSol(j)/(fnnod*2.)

  201           continue

              else

C C) TMC - Call MZTMCA to get transmission and absorptions.
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
              if (ITMCFL(ICOMP,K).EQ.0.and.icfcfl(icomp,k).eq.0) then

C OPAQUE SURFACE
                QIABS(K)=QIABS(K)+AI(ICOMP,K)*X2
                QIREF=QIREF+(1.-AI(ICOMP,K))*SNA(ICOMP,K)*X2
                q2wall(2)=q2wall(2)+x2*SNA(ICOMP,K)*AI(ICOMP,K)

C CFC SURFACE (external only) - run solar_multilayer with inside diffuse source and adjust absorbed fluxes
              elseif(icfcfl(icomp,k).gt.0.and.ie(icomp,k).eq.0)then
                icfctp=icfcfl(icomp,k)

                temp1=0.
                temp2=0.
                temp3=0.

C Calculate CFC system solar beam transmitted, diffuse transmitted, total reflected
C and absorbed fluxes in each CFC layer.Here, the transmitted component is actually
C the reflected component on the inside surface since the source is from the
C inside.
                iSolVis=0
                call solar_multilayer(0,icomp,k,icfcfl(icomp,k),
     &           temp1,temp2,temp3,X2,iSolVis,TRANSBB_SYS,TRANSD_SYS,
     &           REFL_SYS,AbsSol)

                QIREF=QIREF+TRANSD_SYS*SNA(ICOMP,K)         !reflected
                q2lost(2)=q2lost(2)+REFL_SYS*SNA(ICOMP,K)   !transmitted back to ambient(reporting)

C Add additional absorbed (reporting)
                CFCabs=0.
                do 777 ij=1,ncfc_el(icomp,icfctp)
                    CFCabs=CFCabs+AbsSol(ij)
  777           continue
                q2cfc(2)=q2cfc(2)+CFCabs*SNA(ICOMP,K)

C Assign additional absorbed to CFC layer nodes
                ne=nelts(icomp,k)
                inode=1
                do 202 j=1,ne
                nnod=nndl(icomp,k,j)
                fnnod=float(nnod)
                qtmca(k,inode,2)=qtmca(k,inode,2)+
     &                                  AbsSol(j)/(fnnod*2.)
                do 207 jj=2,nnod
                  inode=inode+1
                  qtmca(k,inode,2)=qtmca(k,inode,2)+
     &                                  AbsSol(j)/fnnod

  207           continue
                inode=inode+1
                qtmca(k,inode,2)=qtmca(k,inode,2)+
     &                                  AbsSol(j)/(fnnod*2.)

  202           continue

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
          IF(ITMCFL(ICOMP,I).EQ.0.and.icfcfl(icomp,i).eq.0)THEN

C OPAQUE SURFACE
            QIABS(I)=QIABS(I)+AI(ICOMP,I)*RDIFR
            RREF=RREF+(1.-AI(ICOMP,I))*SNA(ICOMP,I)*RDIFR
            q2wall(2)=q2wall(2)+RDIFR*SNA(ICOMP,i)*AI(ICOMP,i)

C CFC SURFACE (external only) - run solar_multilayer with inside diffuse source and adjust abosrptances
          ELSEIF(icfcfl(icomp,i).gt.0.and.ie(icomp,i).eq.0)then
             icfctp=icfcfl(icomp,i)

             temp1=0.
             temp2=0.
             temp3=0.

C Calculate CFC system solar beam transmitted, diffuse transmitted, total reflected
C and absorbed fluxes in each CFC layer.Here, the transmitted component is actually
C the reflected component on the inside surface since the source is from the
C inside.
             iSolVis=0
             call solar_multilayer(0,icomp,i,icfcfl(icomp,i),
     &        temp1,temp2,temp3,RDIFR,iSolVis,TRANSBB_SYS,TRANSD_SYS,
     &        REFL_SYS,AbsSol)

             RREF=RREF+TRANSD_SYS*SNA(ICOMP,i)              !reflected
             q2lost(2)=q2lost(2)+REFL_SYS*SNA(ICOMP,i)      !transmitted back to ambient(reporting)

C Add additional absorbed (reporting)
             CFCabs=0.
             do 555 ij=1,ncfc_el(icomp,icfctp)
                 CFCabs=CFCabs+AbsSol(ij)
  555        continue
             q2cfc(2)=q2cfc(2)+CFCabs*SNA(ICOMP,i)

             ne=nelts(icomp,i)

C Assign additional absorbed to CFC layer nodes
             inode=1
             do 203 j=1,ne
             nnod=nndl(icomp,i,j)
             fnnod=float(nnod)
             qtmca(i,inode,2)=qtmca(i,inode,2)+
     &                              AbsSol(j)/(fnnod*2.)
             do 208 jj=2,nnod
               inode=inode+1
               qtmca(i,inode,2)=qtmca(i,inode,2)+
     &                              AbsSol(j)/fnnod

  208       continue
             inode=inode+1
             qtmca(i,inode,2)=qtmca(i,inode,2)+
     &                               AbsSol(j)/(fnnod*2.)

  203       continue

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
C of 1.0 W/m^2, whichever is larger.
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
        IF(ITMCFL(ICOMP,I).LE.0.and.icfcfl(icomp,i).le.0)THEN
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

C Determine solar fluxes that have penetrated zone then save
C for possible averaging in MZLS3.
      q2rem(2)=rdifr*zarea
      q1tot=q1outs(2)+q1adjz(2)
      q2tot=q2tmc(2)+q2wall(2)+q2rem(2)+q2cfc(2)

      q1adjf(icomp)=q1adjz(2)
      q1outf(icomp)=q1outs(2)
      q2adjf(icomp)=q2adjz(2)
      q2losf(icomp)=q2lost(2)
      q2tmcf(icomp)=q2tmc(2)
      q2cfcf(icomp)=q2cfc(2)
      q2walf(icomp)=q2wall(2)
      q2remf(icomp)=q2rem(2)

C-------------------------------TRACE
C                               -----

      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF.OR.
     &   IZNTRC(ICOMP).NE.1)goto 9999
      call edisp(itu,' ')
      write(outs,'(A,I5,3A,I3,A)')' Subroutine MZSLGN   Trace output',
     &ICNT,' Zone ',zname(ICOMP),' (',ICOMP,').'
      call edisp(itu,outs)
      ICNT=ICNT+1
      CALL DAYCLK(IDYP,BTIMEF,ITU)

C Output solar intensity details.
      call edisp(itu,' ')
      write(outs,9993)SAZI,SALT
 9993 format( ' Azimuth = ',F6.1,', Altitude = ',F6.1)
      call edisp(itu,outs)
      write(outs,99931)QD,QF
99931 format( ' Irradiance (W/m^2): direct normal = ',F6.1,';',
     &        ' diffuse horizontal = ',F6.1)
      call edisp(itu,outs)

C Output QSOLE and QSOLI for each construction if sun up.
      if(SALT.lt.-1.0)goto 9992
      call edisp(itu,' ')
      write(outs,'(a)') ' Solar nodal absorptions (W/m^2):'
      call edisp(itu,outs)
      call edisp(itu,' ')
      call edisp(itu,' Surface        Dir.  Dif.  Ext.  Int.  TMC ')
      call edisp(itu,' name           shad. shad. solar solar node')
      call edisp(itu,'                            abs.  abs.  abs.')
      DO 9991 I=1,NC
        ITMC=ITMCFL(ICOMP,I)
        if(ITMC.LE.0)then
          write(outs,'(1X,a,5(1X,F5.1))')sname(icomp,I),EXSHAD(I),
     &                     EXSHADF(I),QSOLE(I,2),QSOLI(I,2)
          call edisp(itu,outs)
        else
          NN=NNDC(ICOMP,I)
          if(NN.le.12)then
            write(outs,'(1X,a,16(1X,F5.1))')sname(icomp,I),EXSHAD(I),
     &          EXSHADF(I),QSOLE(I,2),QSOLI(I,2),(QTMCA(I,J,2),J=1,NN)
            call edisp(itu,outs)
          elseif(NN.gt.12)then
            write(outs,'(1X,a,16(1X,F5.1))')sname(icomp,I),EXSHAD(I),
     &          EXSHADF(I),QSOLE(I,2),QSOLI(I,2),(QTMCA(I,J,2),J=1,12)
            call edisp(itu,outs)
            write(outs,'(31X,20(1X,F5.1))')(QTMCA(I,J,2),J=13,NN)
            call edisp(itu,outs)
          endif

C Output direct and diffuse solar passing glazing and then clear.
          write(outs,'(2a,F6.1,a,F6.1,a)') '  Transmissions (W):',
     &      ' direct = ',tdtrace(i),'; diffuse = ',tfftrace(i)
          call edisp(itu,outs)
          tdtrace(i)=0.
          tfftrace(i)=0.
        endif
 9991 CONTINUE
 9992 continue

C Output solar distribution.
      call edisp(itu,' ')
      write(outs,'(a)') ' Solar flux distribution (W):'
      call edisp(itu,outs)
      call edisp(itu,' ')
      write(outs,'(8X,A)')
     &  '|  Gains (from) |  Losses (to)  | Absorbed (into) |'
      call edisp(itu,outs)
      write(outs,99962)q1adjz(2),q2adjz(2),q2tmc(2)
99962 format(8X,'|Intrn',F10.2,'|Intrn',F10.2,'|Trn mlc',F10.2,'|')
      call edisp(itu,outs)
      write(outs,99963)q1outs(2),q2lost(2),q2wall(2)+q2rem(2)
99963 format(8X,'|Extrn',F10.2,'|Extrn',F10.2,'|Qpq mlc',F10.2,'|')
      call edisp(itu,outs)
      write(outs,99964)zSWAf(icomp)
99964 format(8X,'|',15X,'|',15X,'|Z.fluid',F10.2,'|')
      call edisp(itu,outs)
      write(outs,99965)q1tot,q2adjz(2)+q2lost(2),q2tot+zSWAf(icomp)
99965 format(' Totals |',5X,F10.2,'|',5X,F10.2,'|',7X,F10.2,'|')
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
#include "roam.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/PREC8/SLAT,SLON
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      character outs*124
      logical close
      INTEGER IPDR,IYDN,IRD,IRM

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

C If roaming is activated update latitude & longitude
C Roaming is used to describe location for movable bodies e.g. ships
      IF(IROAM.EQ.1)THEN

C Figure out if within roaming period
        DO 100 IPDR=1,NPDR

C Calculate roaming day of year
          IRD=LDYS(IPDR)
          IRM=LMTS(IPDR)
          CALL EDAY(IRD,IRM,IYDN)
          RTIME=REAL(IYDN)+XLHRS(IPDR)/24.0 ! Calculate roaming time
          STIME=REAL(IDAY)+HOUR/24.0        ! Calculate simulation time

          IF(STIME.LT.RTIME)THEN ! Roaming not started yet
            CONTINUE
          ELSEIF(STIME.GE.RTIME)THEN
            IF(LLOCT(IPDR).NE.0)THEN ! i.e. at a location (not moving)
              SLAT=XLOCLT(LLOCT(IPDR))
              SLON=XLOCLG(LLOCT(IPDR))

C Moving between locations so work out new location by interpolation
            ELSEIF(IPDR.LT.NPDR)THEN
              IRD=LDYS(IPDR+1)
              IRM=LMTS(IPDR+1)
              CALL EDAY(IRD,IRM,IYDN)
              RNTIME=REAL(IYDN)+XLHRS(IPDR+1)/24.0
              XINTPF=(STIME-RTIME)/(RNTIME-RTIME)
              SLAT=(XLOCLT(LLOCT(IPDR+1))-SLAT)*XINTPF+SLAT
              SLON=(XLOCLT(LLOCT(IPDR+1))-SLON)*XINTPF+SLON

C Else if last period of roaming is moving (i.e. no final destination
C is available) do not change location
            ELSE
              CONTINUE
            ENDIF
          ENDIF
 100    CONTINUE
      ENDIF

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
      INTT1=INT(30.6*real(IMT1)+0.5)
      INTT2=INT(365.25*REAL(IYR1-1976))
      SMLT=((HOUR/24.0)+real(IDY)+real(INTT1)
     &                 +real(INTT2)-8707.5)/36525.0
      EPSILN=23.4393-0.013*SMLT
      CAPG=357.528+35999.050*SMLT
      IF(CAPG.GT.360.0) THEN
        G360=CAPG-INT(real(CAPG)/360.0)*360.0
      ELSEIF(CAPG.LT.-360.0) THEN
        G360=CAPG-(INT(real(CAPG)/360.0)-1)*360.0   
      ELSE
        G360=CAPG
      ENDIF
      CAPC=1.915*SIN(G360*R)+0.020*SIN(2.0*G360*R)
      CAPL=280.460+36000.770*SMLT+CAPC
      IF(CAPL.GT.360.0) THEN
        XL360=CAPL-INT(real(CAPL)/360.0)*360.0
      ELSEIF(CAPL.LT.-360.0) THEN
        XL360=CAPL-(INT(real(CAPL)/360.0)-1)*360.0
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
      IF(ITC.LE.0.OR.NSINC.LT.ITC) return
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF) return
      call edisp(itu,' ')
      write(outs,'(A,I5)')' Subroutine MZSANG   Trace output',ICNT
      call edisp(itu,outs)
      ICNT=ICNT+1
      call DAYCLK(int(DAY),HOUR,itu)

C Output time and solar position.
      write(outs,'(A,F4.0,A,F5.2,A,F5.2)')' Day ',DAY,' Mean time ',
     &  HOUR,' Solar time ',TIME
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
      write(outs,'(a,I7,a,F7.0,2a)')' MZSINT: @',NSINC,
     &  ' dir n.',QD,' > solar constant so using',
     &  ' diff h.=glob h.; dir n.=0.'
      call edisp(iuout,outs)
      goto 5

C Warning: diffuse > global.
    3 IF(ICOMP.GT.1)goto 5
      CALL DAYCLK(IDYP,BTIMEF,iuout)
      write(outs,'(a,I7,2a)')' MZSINT: @',NSINC,
     &  ' diff h. > global h. so using',
     &  ' diff h.=glob h.; dir n.=0.'
      call edisp(iuout,outs)

C Default: zero direct normal, diffuse horizontal equals global
C horizontal.
    5 QF=QDF
      QD=0.0
      goto 1

C FATAL ERROR: direct normal > solar constant & altitude > 10deg
    4 CALL DAYCLK(IDYP,BTIMEF,iuout)
      write(outs,'(a,I7,a,F6.1,a)')' MZSINT fatal: @',NSINC,
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

C Next line and the related common block were existing here but not used anywhere
C      PARAMETER (MSTMC=20,MSGAL=40,MANH=37,MANV=37)

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)

      COMMON/SUNPOS/SAZI,SALT,ISUNUP

      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
C      COMMON/OPTDAT/NSGALFL(MSTMC),NGNTL(MSTMC),
C     &  NGANGS(MSTMC),TMTSOD(MSTMC,MSGAL,MANH,MANV),
C     &  TMTSOB(MSTMC,MSGAL,MANH,MANV),TMABSO(MSTMC,MSGAL,ME,MANH,MANV),
C     &  THTSOB(MSTMC,MSGAL),TMABSDIF(MSTMC,MSGAL,ME),
C     &  TMGVALUE(MSTMC,MSGAL,MANH,MANV),
C     &  TMREFLECT(MSTMC,MSGAL,MANH,MANV),TUVALUE(MSTMC,MSGAL),
C     &  TREXTERNAL(MSTMC,MSGAL),TRINTERNAL(MSTMC,MSGAL)
C     &  TMVISUAL(MSTMC,MSGAL)
      integer IZSTOCN
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

C ******************** MZSRAD *********************
C SOLAR PROCESSING: Subroutine that sets slope of
C surface then calls appropriate subroutine to determine
C direct and diffuse components of solar radiation. This
C subroutine was part of MZSRAD0. It was separated to
C accommodate call for MZSRAD0 from subroutine
C "simple_solar_coll_coeff_gen".

      SUBROUTINE MZSRAD(ICOMP,II,QD,QF,CAI,SRADDO,SRADF,SKYDIF,GRDDIF)
#include "building.h"

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)

C Slope of surface
      BETA=90.0-PELV(ICOMP,II)

C Call subroutine to determine direct and diffuse solar
C radiation
      CALL MZSRAD0(BETA,QD,QF,CAI,SRADDO,SRADF,SKYDIF,GRDDIF)
      RETURN
      END

C ******************** MZSRAD0 ********************
C  SOLAR PROCESSING: incident comprises direct, sky diffuse
C  and ground reflected components.

      SUBROUTINE MZSRAD0(SLOPE,QD,QF,CAI,SRADDO,SRADF,SKYDIF,GRDDIF)
#include "building.h"
#include "site.h"

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

C Set Beta equal to slope
      BETA=SLOPE

C Useful constants
      PI = 4.0 * ATAN(1.0)
      R = PI / 180.

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
      GRDDIF=0.5*groundrefl*(1.0-COS(BETA*R))*SRADT

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
      GRDDIF=0.5*groundrefl*(1.0-COS(BETA*R))*SRADT

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

         s9=beta*r
         e=1353.*(1.+0.033*cos(0.017203*real(idyp)))*sin(salt*r)
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
      GRDDIF=0.5*groundrefl*(1.0-COS(BETA*R))*SRADT

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
      G0N = 1370. * (1. + 0.033 * COS(0.017214 * real(IDYP)))

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
      IF (BETA.GT.179.99.AND.BETA.LT.180.01) SKYDIF = 0.

C Direct solar radiation incident normally on
C external construction prior to shading adjustment
C and absorption.
      SRADDO=QD*CAI
      IF( SRADDO .LT. 0.0 ) SRADDO = 0.0

C The ground reflected component: assume isotropic
C ground conditions.
      SRADT=QF+(QD*SIN(SALT*R))
      GRDDIF=0.5*groundrefl*(1.0-COS(BETA*R))*SRADT
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
      G0N = 1366.1 * (1. + 0.033 * COS(0.017214 * real(IDYP)))

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
      GRDDIF=groundrefl*(1.0-COS(BETA*R))*SRADT/2.
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
C assumes that the reflectivity is the same from each element of
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
        STMIN=TMCT2(ICOMP,ITMCFL(ICOMP,ISUR),2,KP)
        STMAX=TMCT(ICOMP,ITMCFL(ICOMP,ISUR),2)
        XX1=STMIN + (STMAX - STMIN)*ECRAT(ICOMP,ISUR)
        STMIN=TMCT2(ICOMP,ITMCFL(ICOMP,ISUR),3,KP)
        STMAX=TMCT(ICOMP,ITMCFL(ICOMP,ISUR),3)
        XX2=STMIN + (STMAX - STMIN)*ECRAT(ICOMP,ISUR)
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
C for shortwave).
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
c PO    - gives the direct shading of each external surface
c         for each hour in the representative day.
c POF   - gives the diffuse shading of each external surface
c         for each hour in the representative day.
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
#include "model.h"
#include "CFC_common.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      COMMON/SHAD1/IML

C Shading file record width found in mzshin.
      common/rsolmax/maxrec(mcom)

      COMMON/SHAD2/ISHD(MCOM),IGCS(MCOM,MS),PO(MCOM,MS,MT),
     & POF(MCOM,MS,MT),IGCI(MCOM,MS),insst(mcom,mgt,mt,misur),
     & pinsst(mcom,mgt,mt,misur)

      CHARACTER*72 LS
      DIMENSION ISS(12),ISADD(12)
      character outs*124
      logical OK,dok
      integer ier

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MZSHDO'  ! set for subroutine

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
        ier=0
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

C Shading only or shading/insolation case.
  101   DO 20 J=1,NC
          READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(PO(I,J,K),K=1,24)
          IREC=IREC+1
          READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(POF(I,J,K),K=1,24)
          IREC=IREC+1
   20   CONTINUE

        IF(ISHD(I).EQ.1)goto 1

        goto 103

C Insolation only case.
  102   IREC=IREC+2*NC
  103   READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(IGCI(I,J),J=1,NC)
        irec=irec+1

C Calculate number of transparent surfaces that have insolation information.
        nw=0
        do 50 j=1,nc
          if(igci(i,j).eq.0)goto 50
          if(itmcfl(i,j).gt.0)then
            itmc=1
            icfctp=0
          elseif(icfcfl(i,j).gt.0)then
            icfctp=1
            itmc=0
          else
            itmc=0
            icfctp=0
          endif
          nw=nw+itmc+icfctp
   50   continue

C insst()=0, the whole surface is shaded; insst()=-1, the sun is not up,
C   otherwise the index of the surface being insolated.
C pinsst() proportion insolated from source k at hr m to insolated
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
        helptopic='ignore_shading_warning'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' Continue the simulation and assume zero ',
     &    ' shading with default plane insolation?',OK,dok,nbhelp)
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
C       groundrefl   actual snow reflectivity for the hour
C *********************************************************************
      SUBROUTINE MZGREF
      use h3kmodule
      IMPLICIT NONE

C-----------------------------------------------------------------------
C     DECLARATIONS
C-----------------------------------------------------------------------

C ESP-r Commons
#include "building.h"
#include "site.h"
C site.h has common C5

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

      IF (groundreflmodel.EQ.2) THEN
        IDYAUX=IDYP                    ! day of year
        IF(IHRF.EQ.1) IDYAUX=IDYF      ! end of day: use next day
        CALL EDAYR(IDYAUX,ID,IM)       ! calculate day and month
        AUX=MAX(MIN(FLOAT(dayswithsnow(IM))/FLOAT(MONTH(IM)),1.),0.0)
        groundrefl=groundreflmonth(IM)*(1.-AUX)+snowgroundrefl*AUX

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

      ELSEIF (groundreflmodel.EQ.3) THEN

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
            SREFP=snowgroundrefl
          ELSEIF (SNMLT) THEN
            SREFP=SREFLS*(.97**.04166666)
          ELSE
            SREFP=SREFLS*(.99**.04166666)
          ENDIF

C Calculate actual snow reflectivity, depending on snow depth.
          IF (SNDEP.GT.5.) THEN
            groundrefl = SREFP
          ELSE
            groundrefl=groundreflmonth(IM)*(1.-SNDEP/5.)+SREFP*SNDEP/5.
          ENDIF

C Save snow reflectivity information.
          IDYLS=IDYAUX
          IHRLS=IHRF
          SNDEPLS=SNDEP
          SREFLS=SREFP
        ENDIF
      ENDIF

C Pass time step ground reflectivity for XML output.
      call AddToReport(
     &      rvBuildingGroundReflectivity%Identifier,
     &      groundrefl)

C Pass snow depth to XML output.
      call AddToReport(
     &      rvClimateSnownDepth%Identifier,
     &      SNDEP)

C-----------------------------------------------------------------------
C     TRACE
C-----------------------------------------------------------------------
      IF(ITC.LE.0.OR.ITRACE(19).EQ.0) GOTO 9999
      IF(NSINC.LT.ITC.OR.NSINC.GT.ITCF) GOTO 9999

C Subroutine and simulation time info
      WRITE(OUTS,'(A,I4)')' Subroutine MZGREF   Trace output ',ICNT
      call edisp(itu,' ')
      CALL EDISP(ITU,OUTS)
      ICNT=ICNT+1
      CALL DAYCLK(IDYP,BTIMEF,ITU)

C Print ground reflectivity and, if advanced model, snow depth ground.
      IF (groundreflmodel.LE.2) THEN
        WRITE(OUTS,910) groundrefl
      ELSE
        WRITE(OUTS,920) groundrefl,SNDEP
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
#include "building.h"
#include "site.h"
C site.h has common C5

      INTEGER IFIL      ! Unit number used for processing input/output files
      COMMON/FILEP/IFIL

C Local variables
      CHARACTER OUTS*124
      LOGICAL FOUND,OK,DOK,FEXIST
      REAL SNDEP        ! snow depth (cm)
      INTEGER ICOUNT    ! number of data items into snow depth file
      INTEGER IER       ! flag indicating if snow depth file was read successfully
      INTEGER IFILSN    ! unit number associated with snow depth ascii file
      INTEGER ISTAT
      INTEGER IBINSN    ! unit number associated with binary file

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='PPGREF'  ! set for subroutine

C Set default error code and return if not advanced albedo model
      IER=0
      IF (groundreflmodel.NE.3) RETURN

C CHECK EXISTENCE OF FILE, ASK FOR NEW FILE IF NOT FOUND, EXIT WITH
C ERROR IF CANNOT BE FOUND
      CALL FINDFIL(SNFNAM,FOUND)
      IF (.NOT.FOUND) THEN
        DO WHILE(.NOT.FOUND)
          dok=.true.
          WRITE(OUTS,'(A,A)') 'Could not find snow depth file: ',SNFNAM
          helptopic='snow_depth_file'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(OUTS,
     &      ' Respecify? (simulator will exit if [no]!)',
     &      OK,DOK,nbhelp)
          IF (OK) THEN
            CALL EASKS(SNFNAM,'Enter name of snow depth file',
     &        ' ',72,' ','Snow depth file',IER,nbhelp)
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
#include "building.h"
#include "site.h"
C site.h has common C5
      REAL GTP,GTF,QGLP,QGLF,QGGP,QGGF,GC,
     &TSKY,CLOUDC,CLOUD1,CLOUD2,CLOUD3,TBAVE,TBROOF,TBNRTH,
     &TBSUTH,TBEAST,TBWEST
      COMMON/ELWE3/GTP(6),GTF(6),QGLP,QGLF,QGGP,QGGF,GC(6,4),
     &TSKY,CLOUDC,CLOUD1,CLOUD2,CLOUD3,TBAVE,TBROOF,TBNRTH,
     &TBSUTH,TBEAST,TBWEST

      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

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
      TSNOW=((1.-groundrefl)*QT+4.*ESNOW*SBC*(TSKY**4)
     &       +GHTC*(TP+ZEROK)+(KSNOW/(SNDEP*CMTOM))*(GTF(1)+ZEROK))
     &      /(4.*ESNOW*SBC*(TSKY**3)+GHTC+KSNOW/(SNDEP*CMTOM))
      TSNOW=TSNOW-ZEROK
      IF (TSNOW.GT.0.) THEN
        SNMLT=.TRUE.
      ELSE
        SNMLT=.FALSE.
      ENDIF

      RETURN
      END

C ******************** MZSCTL ********************
C MZSCTL deals with solar related controls. It currently uses
C only controls related to bidirectional data. In the future, all
C existing and new controls should be added here.

      SUBROUTINE MZSCTL(ICOMP,ISURFNumber,SRADDO,SRADF,ISET)
#include "building.h"
#include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"
#include "control.h"
#include "esprdbfile.h"
#include "material.h"
      PARAMETER (MSTMC=20,MSGAL=40,MANH=37,MANV=37)
      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/trc/itrc ! needed to recover tdf data

      COMMON/OPTDAT/NSGALFL(MSTMC),NGNTL(MSTMC),
     &  NGANGS(MSTMC),TMTSOD(MSTMC,MSGAL,MANH,MANV),
     &  TMTSOB(MSTMC,MSGAL,MANH,MANV),TMABSO(MSTMC,MSGAL,ME,MANH,MANV),
     &  THTSOB(MSTMC,MSGAL),TMABSDIF(MSTMC,MSGAL,ME),
     &  TMGVALUE(MSTMC,MSGAL,MANH,MANV),
     &  TMREFLECT(MSTMC,MSGAL,MANH,MANV),TUVALUE(MSTMC,MSGAL),
     &  TREXTERNAL(MSTMC,MSGAL),TRINTERNAL(MSTMC,MSGAL),
     &  TMVISUAL(MSTMC,MSGAL)

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/SHAD2/ISHD(MCOM),IGCS(MCOM,MS),PO(MCOM,MS,MT),
     & POF(MCOM,MS,MT),IGCI(MCOM,MS),insst(mcom,mgt,mt,misur),
     & pinsst(mcom,mgt,mt,misur)

      integer ICF,IDTYP,IPER,IICOMP
      real BB1,BB2,BB3,TNP,QFUT,TFUT
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)

      COMMON/SUNPOS/SAZI,SALT,ISUNUP

C Daylight coeff specific common block data definition:
      COMMON/DCOEF/NDCFP,NDCFS(MDCFP),DCOEF(MDCFP,MDCFS,MDCFV),
     & IDCFID(MCOM,MLCOM,MDF)

C SHOCC: new common ...
C iblnd - SHOCC blinds corresponding to ESP-r TMCs:
C      0, no; else, a f77 index [1,~] to access
C      SHOCC zone blind data structure
      common/shocc/iblnd(mcom,ms)

      logical closea,neglum
      integer ISET !the bidirectional dataset number to use.
                  !Use the number specified in tdf if exists. Otherwise use the 1st dataset.
      integer ictl_constrdbindex_number !the number of the controlled construction will be assigned to it

      real shd_po_fctr_ctl,shd_pof_fctr_ctl,rad_on_surf

C Daylight coeff specific
      dimension alt(8)
      DIMENSION VAL(MBITS+2)

C Definitions for daylight coefficient patch altitude:
      data alt/6.,18.,30.,42.,54.,66.,78.,90./

      character outs*124
      character ctl_constrdb_name*72 !to match the name of the controlled construction (from db)
                                     !with the name of the construction of the study
      character name_constr_ofStudy*72 !to match the name of the construction of the study with
                                      !with the name of the construction picked to be controlled
                                      !from the constructions database (i.e. the one "actuated")

      real TVT,TVT2,TVT3,TVT4,TVT5,TVT6 !Local variables to calculate the visible transmittance
                                        !for the different daylight setpoints and controls
                                        !(i.e. one for each of the 6 datasets for bidirectional sets)
      integer i_ctlZone  !This is used when the sensor is located to another zone and indicates
                         !which zone. It will take the value of iosn(1,1).

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,NIN

C If there is a tdf item that defines which bidirectional dataset
C to use then use it. Otherwise use the first set.
      if(IBIDIRS(ICOMP,ISURFNumber).ne.0)then
        IFOC=IBIDIRS(ICOMP,ISURFNumber)
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        ISET=int(VAL(ISD))

C Check that there is not any tdf bidirectional item associated with it
C and that there are common optical controls
      elseif(IBIDIRS(ICOMP,ISURFNumber).eq.0.and.nof.gt.0)then
C Determine year day number of present day
        IF(IHRP.NE.24) THEN
          IDAY=IDYP
        ELSE
          IDAY=IDYF
        ENDIF

C Process each optical control loop to test if it is active
        DO 100 iol=1,nof

C<<<probably there is no need for this >>>>>
          IOFP=iol

C Set up day type and period pointers
          NDAYT=NOCDT(iol)

C If NDAYT=0 set data to all day types.
          NIN=0
          IF(NDAYT.EQ.0)THEN
            NIN=NBDAYTYPE
            NDAYT=0
          ENDIF
          DO 10 IDTYP=1,NDAYT
            IDS=IOCDV(iol,IDTYP,1)
            IDF=IOCDV(iol,IDTYP,2)
            IF(IDAY.GE.IDS.AND.IDAY.LE.IDF) GOTO 20
   10     CONTINUE
          WRITE(outs,'(a,I3)') ' CONTRL: optical control loop ',iol
          call edisp(iuout,outs)
          WRITE(outs,'(9x,a,I4)') 'no valid day type for year-day ',IDAY
          call edisp(iuout,outs)
          call edisp(iuout,
     &         ' CONTRL: cannot locate appropriate day type')
          call epwait
          call epagend
          STOP

C Check number of periods in each day and the start and finish times
   20     if(nin.le.-1.or.ndayt.lt.1)idtyp=icalender(iday)
          NDAYP=NOCDP(IOFP,IDTYP)
          IF(NDAYP.EQ.0)then
            call edisp(iuout,' CONTRL: no day-periods defined')
            call epwait
            call epagend
            STOP
          endif
          DO 22 IDAYP=1,NDAYP
            IPER=IDAYP
            TOS=TOCPS(IOFP,IDTYP,IDAYP)
            IF(IDAYP.LT.NDAYP) THEN
              TOF=TOCPS(IOFP,IDTYP,IDAYP+1)
            ELSE
              TOF=24.
            ENDIF
            if(BTIMEF.GT.TOS.AND.BTIMEF.LE.TOF)then

C Call in the future here different subroutines for different controls
              ictl_constrdbindex_number=ioan(1,2)

C **** IMPORTANT: TAKE ONLY THE 1ST 12 CHARS FROM "DESC" ****

C Debug.
C        write(6,*) 'desc', mlcname(ictl_constrdbindex_number)
C        write(6,*) 'ssmlcn', SSMLCN(izstocn(icomp,ISURFNumber))

              write(ctl_constrdb_name,'(a)')
     &             mlcname(ictl_constrdbindex_number)(1:12)
              write(name_constr_ofStudy,'(a)')
     &             SSMLCN(izstocn(icomp,ISURFNumber))(1:12)

C controls start here
C<<< Consider changing ioan(1,1) in the future to ioan(iol,1)>>>>
              if(ioan(1,1).eq.-5.and.ctl_constrdb_name(1:12).eq.
     &           name_constr_ofStudy(1:12))then
                if(ioclaw(1,IDTYP,IPER).eq.2)then

C Current zone temperature sensor
C<<<Check if instead if iosn(1,1) we should use iosn(iol,1) as in bcfunc.F>>>
                  if(iosn(1,1).eq.0)then

C Debug.
C                    write(6,*) 'TFA: ', TFA(ICOMP),'     ','time: ',btimef
C                    write(6,*) 'OMISCD: ', OMISCD(1,IDTYP,IPER,2)

C Allow max 6 setpoints and datasets to be used for the moment.
                    if(TFA(ICOMP).GT.OMISCD(1,IDTYP,IPER,2))then
                      ISET=int(OMISCD(1,IDTYP,IPER,3))
                    elseif(TFA(ICOMP).GT.OMISCD(1,IDTYP,IPER,4))then
                      ISET=int(OMISCD(1,IDTYP,IPER,5))
                    elseif(TFA(ICOMP).GT.OMISCD(1,IDTYP,IPER,6))then
                      ISET=int(OMISCD(1,IDTYP,IPER,7))
                    elseif(TFA(ICOMP).GT.OMISCD(1,IDTYP,IPER,8))then
                      ISET=int(OMISCD(1,IDTYP,IPER,9))
                    elseif(TFA(ICOMP).GT.OMISCD(1,IDTYP,IPER,10))then
                      ISET=int(OMISCD(1,IDTYP,IPER,11))
                    elseif(TFA(ICOMP).GT.OMISCD(1,IDTYP,IPER,12))then
                      ISET=int(OMISCD(1,IDTYP,IPER,13))
                    else
                      ISET=1
                    endif
                  elseif(iosn(1,1).eq.-3)then

C Senses outside db.
C Allow max 6 setpoints and datasets to be used for the moment.
                    if(TF.GT.OMISCD(1,IDTYP,IPER,2))then
                      ISET=int(OMISCD(1,IDTYP,IPER,3))
                    elseif(TF.GT.OMISCD(1,IDTYP,IPER,4))then
                      ISET=int(OMISCD(1,IDTYP,IPER,5))
                    elseif(TF.GT.OMISCD(1,IDTYP,IPER,6))then
                      ISET=int(OMISCD(1,IDTYP,IPER,7))
                    elseif(TF.GT.OMISCD(1,IDTYP,IPER,8))then
                      ISET=int(OMISCD(1,IDTYP,IPER,9))
                    elseif(TF.GT.OMISCD(1,IDTYP,IPER,10))then
                      ISET=int(OMISCD(1,IDTYP,IPER,11))
                    elseif(TF.GT.OMISCD(1,IDTYP,IPER,12))then
                      ISET=int(OMISCD(1,IDTYP,IPER,13))
                    else
                      ISET=1
                    endif

C Sense incident solar radiation on a surface
C Find out first which surface
                  elseif(iosn(1,1).eq.icomp.and.iosn(1,2).eq.
     &                   ISURFNumber)then

C Calculate now radiation on the surface (as in MZSLGN)
C First calculate the shading factor on the surface
C Using the same calculation for POO and POFF as in MZSLGN
                    if(ISHD(icomp).eq.0.or.ISHD(icomp).EQ.2)then
                      shd_po_fctr_ctl=0.0
                      shd_pof_fctr_ctl=0.0
                    else
                      shd_po_fctr_ctl=PO(ICOMP,ISURFNumber,IHRF)
                      shd_pof_fctr_ctl=POF(ICOMP,ISURFNumber,IHRF)
                    endif
                    rad_on_surf=SRADDO*(1.-shd_po_fctr_ctl)
     &                         +SRADF*(1.-shd_pof_fctr_ctl)

C Allow max 6 setpoints and datasets to be used for the moment.
                    if(rad_on_surf.GT.OMISCD(1,IDTYP,IPER,2))then
                      ISET=int(OMISCD(1,IDTYP,IPER,3))
                    elseif(rad_on_surf.GT.OMISCD(1,IDTYP,IPER,4))then
                      ISET=int(OMISCD(1,IDTYP,IPER,5))
                    elseif(rad_on_surf.GT.OMISCD(1,IDTYP,IPER,6))then
                      ISET=int(OMISCD(1,IDTYP,IPER,7))
                    elseif(rad_on_surf.GT.OMISCD(1,IDTYP,IPER,8))then
                      ISET=int(OMISCD(1,IDTYP,IPER,9))
                    elseif(rad_on_surf.GT.OMISCD(1,IDTYP,IPER,10))then
                      ISET=int(OMISCD(1,IDTYP,IPER,11))
                    elseif(rad_on_surf.GT.OMISCD(1,IDTYP,IPER,12))then
                      ISET=int(OMISCD(1,IDTYP,IPER,13))
                    else
                      ISET=1
                    endif

C ****ATTENTION:
C The implementation here for the daylight coeffiecient method
C is not a trivial task and is left for future work
C Sense lux via daylight coef method.
                  elseif(iosn(1,1).eq.-8.AND.
     &                IBLND(ICOMP,ISURFNumber).GT.0)then
                    continue
                  elseif(iosn(1,1).eq.-9)then

C Code taken from MZSLGN but not fully tested with bidirectional controls.
C Calculates visible transmittance based on lux setpoint value
C If above the original value (which may be 0.!!) then takes the visible
C transmittance from the set specified in the control file. It also returns the
C set back to use for all bidirectional calculations

C Find out first which surface
                    if(iosn(1,2).eq.icomp.and.iosn(1,3).eq.
     &                   ISURFNumber)then

C Calculate sensor illuminance - daylight coefficient method:
C Find which daylight coefficient set to use (use stage 1 i.e.
C maximal visible transmittance stage):

C****Attention: NDCP assignment needs further testing
C****is ISURFNumber for this assignment similar to the conventional TMC controls
                      NDCP = ISURFNumber
                    endif

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
     &                    ang)

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
                          elseif((npatch.ge.109).and.(npatch.le.126))
     &                      then
                            nzone = 5
                          elseif((npatch.ge.127).and.(npatch.le.138))
     &                      then
                            nzone = 6
                          elseif((npatch.ge.139).and.(npatch.le.144))
     &                      then
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
333                   continue

C Give warning about calculated negative luminance:
                      if(neglum)then
                        write (outs,'(a)')
     &     ' Sky patch luminance < = 0. Using uniform sky aproximation'
                        call edisp(iuout,outs)
                        write (outs,'(a,I3,a,f6.1,a,f6.1,a,f4.1)')
     &                  ' Day ',IDYP,' Direct ',QDIR,' Diffuse ',QDIF,
     &                  ' Sun altitude ',SALT
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
                      TVT=TMVISUAL(1,1)
                    else

C ****ATTENTION:
C The implementation here is not fully tested and has been put for experimental
C and "tidy-up" reasons
                      TVT=((OMISCD(1,IDTYP,IPER,2))/ELLUM)*TMVISUAL(1,1)
                      TVT2=
     &                 ((OMISCD(1,IDTYP,IPER,4))/ELLUM)*TMVISUAL(1,1)
                      TVT3=
     &                 ((OMISCD(1,IDTYP,IPER,6))/ELLUM)*TMVISUAL(1,1)
                      TVT4=
     &                 ((OMISCD(1,IDTYP,IPER,8))/ELLUM)*TMVISUAL(1,1)
                      TVT5=
     &                 ((OMISCD(1,IDTYP,IPER,10))/ELLUM)*TMVISUAL(1,1)
                      TVT6=
     &                 ((OMISCD(1,IDTYP,IPER,12))/ELLUM)*TMVISUAL(1,1)
                      if(TVT.GT.TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,3))))
     &                  then
                        TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,3)))=TVT
                        ISET=int(OMISCD(1,IDTYP,IPER,3))
                      elseif(TVT.LE.
     &                  TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,3))))then
                        if(TVT2.GT.
     &                    TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,5))))then
                          TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,5)))=TVT2
                          ISET=int(OMISCD(1,IDTYP,IPER,5))
                        elseif(TVT2.LE.
     &                    TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,5))))then
                          if(TVT3.GT.
     &                      TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,7))))then
                            TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,7)))=TVT3
                            ISET=int(OMISCD(1,IDTYP,IPER,7))
                          elseif(TVT3.LE.
     &                      TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,7))))then
                            if(TVT4.GT.
     &                        TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,9))))
     &                        then
                              TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,9)))=
     &                        TVT4
                              ISET=int(OMISCD(1,IDTYP,IPER,9))
                            elseif(TVT4.LE.
     &                        TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,9))))
     &                        then
                              if(TVT5.GT.
     &                         TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,11))))
     &                          then
                               TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,11)))=
     &                          TVT5
                                ISET=int(OMISCD(1,IDTYP,IPER,11))
                              elseif(TVT5.LE.
     &                         TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,11))))
     &                          then

C Identation has been slightly changed here
                                if(TVT6.GT.
     &                         TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,13))))
     &                            then
                               TMVISUAL(1,int(OMISCD(1,IDTYP,IPER,13)))=
     &                            TVT6
                                  ISET=int(OMISCD(1,IDTYP,IPER,13))
                                else
                                  continue
                                endif
                              endif
                            endif
                          endif
                        endif
                      endif
                    endif

C Sense temperature in another zone
                  elseif(iosn(1,1).gt.0.and.iosn(1,2).eq.0)then

C iosn(1,1) will specify which zone
                    i_ctlZone=iosn(1,1)

C Allow max 6 setpoints and datasets to be used for the moment.
                    if(TFA(i_ctlZone).GT.OMISCD(1,IDTYP,IPER,2))then
                      ISET=int(OMISCD(1,IDTYP,IPER,3))
                    elseif(TFA(i_ctlZone).GT.OMISCD(1,IDTYP,IPER,4))then
                      ISET=int(OMISCD(1,IDTYP,IPER,5))
                    elseif(TFA(i_ctlZone).GT.OMISCD(1,IDTYP,IPER,6))then
                      ISET=int(OMISCD(1,IDTYP,IPER,7))
                    elseif(TFA(i_ctlZone).GT.OMISCD(1,IDTYP,IPER,8))then
                      ISET=int(OMISCD(1,IDTYP,IPER,9))
                    elseif(TFA(i_ctlZone).GT.OMISCD(1,IDTYP,IPER,10))
     &                then
                      ISET=int(OMISCD(1,IDTYP,IPER,11))
                    elseif(TFA(i_ctlZone).GT.OMISCD(1,IDTYP,IPER,12))
     &                then
                      ISET=int(OMISCD(1,IDTYP,IPER,13))
                    else
                      ISET=1
                    endif

C End of all the bidirectional control options
                  endif
                endif
              endif
            endif
  22      CONTINUE
 100    CONTINUE
      else
        ISET=1
      endif
      return
      END
