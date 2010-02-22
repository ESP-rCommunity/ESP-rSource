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
C      MOTHKE
C      MOFMXN
C      MOFGET
C      MOFAXS
C      MOFHED
C      MOFDRW

C ******************** MOTHKE 

C MOTHKE determines the thickness of each element within
C the defined construction. These thicknesses are obtained
C from the relevant thermal properties file identified in
C the results database.

      SUBROUTINE MOTHKE(ICOMP,ISUR)
#include "building.h"
#include "model.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/ETHICK/ETHK(ME),THKMAX,THKMIN

      CHARACTER outs*124
      logical newgeo  ! to use for testing if new/old geometry file.

C Read (silently) in the zone geometry data.
      newgeo=.false.  ! assume older format geometry.
      IUNITF=IFIL+1
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(newgeo)then
        call georead(IUNITF,LGEOM(ICOMP),ICOMP,0,IUOUT,IER)
      else
        call egomin(IUNITF,LGEOM(ICOMP),ICOMP,0,0,IUOUT,IER)
      endif
      IF(IER.NE.0)THEN
        write(outs,'(A,A72)')' Could not find ',LGEOM(ICOMP)
        CALL USRMSG(' ',outs,'W')
        RETURN
      ENDIF

C If there is a mlc db scan it for properties of the surface.
      CALL GZPDBE(ICOMP,IER)
      IF(IER.NE.0)THEN
        call edisp(iuout,' Could not find all materials')
        RETURN
      ENDIF

C Read (silently) construction file.
      CALL ECONST(LTHRM(ICOMP),IUNITF,ICOMP,0,IUOUT,IER)
      IF(IER.NE.0)THEN
        write(outs,'(A,A72)')' Could not find ',LTHRM(ICOMP)
        CALL USRMSG(' ',outs,'W')
        RETURN
      ENDIF

C Read thicknesses and determine axis max. and min. values.
C Convert thickness into mm for display.
      THKMAX=0.
      THKMIN=0.
      IELT=NE(ISUR)
      DO 50 I=1,IELT
        ETHK(I)=THK(ISUR,I)*1000.0
        THKMAX=THKMAX+ETHK(I)
   50 CONTINUE
      DT=(THKMAX-THKMIN)

C Give 100mm clearance on both sides for boundary plotting.
      IF(DT.GT.2.0.AND.DT.LE.20.0)THEN
        THKMIN=-10.
        THKMAX=30.
      ELSEIF(DT.GT.20.0.AND.DT.LE.50.0)THEN
        THKMIN=-20.
        THKMAX=70.
      ELSEIF(DT.GT.50.0.AND.DT.LE.100.0)THEN
        THKMIN=-50.
        THKMAX=150.
      ELSEIF(DT.GT.100.0.AND.DT.LE.200.0)THEN
        THKMIN=-100.
        THKMAX=300.
      ELSEIF(DT.GT.200.0.AND.DT.LE.300.0)THEN
        THKMIN=-100.
        THKMAX=400.
      ELSEIF(DT.GT.300.0.AND.DT.LE.400.0)THEN
        THKMIN=-100.
        THKMAX=500.
      ELSEIF(DT.GT.400.0.AND.DT.LE.500.0)THEN
        THKMIN=-100.
        THKMAX=600.
      ELSEIF(DT.GT.500.0.AND.DT.LE.600.0)THEN
        THKMIN=-100.
        THKMAX=700.
      ELSEIF(DT.GT.600.0.AND.DT.LE.1000.0)THEN
        THKMIN=-100.
        THKMAX=1100.
      ENDIF

      RETURN

      END

C ******************** GZPDBE
C GZPDBE: Get material db ref for zone layers from mlc.
C Assumes that geometry file has been read and surface
C attributes are known. 

      SUBROUTINE GZPDBE(ICOMP,IER)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

C IPRZ is material db ref for each layer.
      common/T8/IPRZ(MS,ME)

      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

      CHARACTER outs*124,CSTR*25
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12

C Use material arrays if available. This also assumes that constructions will
C have been filled after the model cfg file was scanned.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
              if(closemat1.or.closemat2)then
        continue
              else
        call usrmsg('GZPDBE: The materials arrays are incomplete so',
     &    'this report might not work correctly.','W')
                return
              endif

C Clear IPRZ to 1 (not 0 since that is an air gap).
      do 45 ISUR=1,NZSUR(ICOMP)
        do 46 IL=1,ME
          IPRZ(ISUR,IL)=1
 46     continue
 45   continue

C If there is a mlc db scan it for properties of surfaces.
C Reset number of layers, if at end of loop it is still 0 then
C the construction was not found.
      do 44 ISUR=1,NZSUR(ICOMP)
        CALL SURADJ(ICOMP,ISUR,IE,T,IZC,ISC,ICN,CSTR)
        ICKNE=0
        DO 20 IC=1,NMLC
          IF(DESC(IC)(1:12).EQ.SSMLCN(ICN))THEN
            ICKNE=LAYERS(IC)

C Gather material index about each layer. IPRZ is used elsewhere.
            DO 23 IL=1,LAYERS(IC)
              IPRZ(ISUR,IL)=IPR(IC,IL)
   23       CONTINUE
          ENDIF
   20   CONTINUE
        if(ICKNE.eq.0)then
          write(outs,'(5A)') ' The construction ',SSMLCN(ICN)(1:12),
     &       ' of ',SSNAME(ICN),' was not found in the mlc db...'
          call edisp(iuout,outs)
          call edisp(iuout,' ')
          IER=1
        endif
  44  continue

      RETURN
      END

C ******************** MOFMXN
C MOFMXN determines the maximum and minimum values of the
C temperature profiles to be displayed.

      SUBROUTINE MOFMXN(ICOMP,ISUR)
#include "building.h"

      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/FGRAF1/TMAX,TMIN,xsc,ysc,Xadd,Yadd

      DIMENSION XVAL1(MZS)

C Compute max. and min.
      TMAX=-1.E+10
      TMIN=1.E+10
      DO 10 I=IODS,IODF
        ID=I

C For requested zone and construction and for every computational
C time-step, get temperature results for complete day.
        CALL CHKTIME(ID,ISTART,IEND)
        CALL MOFGET(ICOMP,ISUR,ID)

C Stepping at defined output interval.
        NN=2*NELTS(ICOMP,ISUR)
        DO 20 J=ISTART,IEND,NOUT

C Set all possible values, computing average if required.
C Copy averaged values into VAL2.
          CALL VALIAV(NN+3,J,XVAL1)

C Find max and min of all temperatures (From outside to inside).
          DO 80 L=1,NN+3
            TMAX=AMAX1(TMAX,XVAL1(L))
            TMIN=AMIN1(TMIN,XVAL1(L))
   80     CONTINUE
   20   CONTINUE
   10 CONTINUE

C Round values for display.
      TMIN1=TMIN
      TMIN=int(TMIN1-1.)

      RETURN
      END

C ******************** MOFGET ********************

C MOFGET recovers from the results library construction
C node and adjacent air temperature data for any specified
C construction and day.
C Place data in VAL1() as follows:
C VAL1(1,?) external conditions,
C VAL1(2,?)..VAL1(1+NN) node temperatures,
C VAL1(1+NN+1) inside surface temperatures,
C VAL1(1+NN+2) inside air temperature.

      SUBROUTINE MOFGET(ICOMP,ISUR,IDAY)
#include "building.h"
#include "site.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      CHARACTER*25 CSTR

      DIMENSION XD(MN)

      N=24*NTS
      CALL EDAYR(IDAY,IDAYN,IMTHN)

C Determine the 'other' zone and connection information. NN is
C number of nodes, NNT the inside surface position NNA the inside
C air temp position.
      NN=2*NELTS(ICOMP,ISUR)
      NNT=NN+1
      NNA=NNT+1
      CALL SURADJ(ICOMP,ISUR,IE,TMPO,IZC,ISC,ICN,CSTR)
      icc=izstocn(icomp,isur)

C Set 'outside' temperature. 1=same,2=constant,3=other zone,4=ground.
C Use VAL3(2,?) to hold either external air or other zone temp.
      ISET=ISIM
      if(ie.eq.0)then
        CALL MOCLIM(IDAY,2)
      elseif(ie.eq.3)then
        CALL GZAIRT(IDAY,IZC,ISET)
        CALL XTVAL3(GVAL,1,N,2)
      endif

C Zone air temperature into VAL3(1,?), inside surf temp intoVAL3(3,?).
      CALL GZAIRT(IDAY,ICOMP,ISET)
      CALL XTVAL3(GVAL,1,N,1)
      call GTMS(IDAY,ICOMP,ISUR,ISET)
      CALL XTVAL3(GVAL,1,N,3)

C For each timestep in the day extract the appropriate data from
C VAL1 and VAL3.
      do 40 ITIMO=1,N

C Depending on the other side conditions...
        if(ie.eq.0)then
          TADJ=VAL3(2,ITIMO)
        elseif(ie.eq.1)then
          TADJ=VAL3(1,ITIMO)
        elseif(ie.eq.2)then
          TADJ=TMPO
        elseif(ie.eq.3)then
          TADJ=VAL3(2,ITIMO)
        elseif(ie.eq.4)then

C If standard ground profile get it, if 3D ground set to 1.0C
C and if user defined get it.
          if(ic2(icc).gt.0)then
            TADJ=GRDTMP(IMTHN,ic2(icc))
          elseif(ic2(icc).eq.-3)then
            TADJ=1.0
          else
            TADJ=UGRDTP(IMTHN,ie2(icc))
          endif
        endif
        VAL1(1,ITIMO)=TADJ

C Get node temps at each timestep, place in VAL1() [shifted] and
C append inner surface temp.
        call GATMC(IDAY,icomp,isur,ITIMO,XD)
        do 50 i=1,NN
          VAL1(i+1,ITIMO)=XD(i)
  50    continue

C Add inside surface temperature.
        VAL1(1+NNT,ITIMO)=VAL3(3,ITIMO)

C Add inside zone temperature.
        VAL1(1+NNA,ITIMO)=VAL3(1,ITIMO)
   40 continue
      RETURN

      END

C ******************** MOFAXS
C MOFAXS draws the axis relating to construction
C temperature graphical output.

      SUBROUTINE MOFAXS(ICOMP,ISUR)
#include "building.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/ETHICK/ETHK(ME),THKMAX,THKMIN
      COMMON/FGRAF1/TMAX,TMIN,xsc,ysc,Xadd,Yadd
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
#else
      integer*8 igwid,igheight  ! for use with axiscale
#endif

      CALL MOFHED(ICOMP,ISUR,'i')
      XMAX=THKMAX
      XMIN=THKMIN

C Determine the scaling for both axis and place in static 
C memory for access by plotting code.
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,XMIN,XMAX,TMIN,TMAX,xsc,ysc,sca,
     &              Xadd,Yadd)
      call linescale(igl,Xadd,xsc,igb,Yadd,ysc)
      call horaxis(XMIN,XMAX,igl,igr,igb,Xadd,xsc,0,'Thickness (mm)')
      call vrtaxis(TMIN,TMAX,igl,igb,igt,Yadd,ysc,0,0,'Temp. Deg.C')
      RETURN
      END

C ******************** MOFHED
C MOFHED outputs the appropriate header information
C corresponding to graphical result display.
C If act = i then initial display, if act = s then single step display.
      SUBROUTINE MOFHED(ICOMP,ISUR,act)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/RECV5/SNA(MCOM,MS)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

      CHARACTER ETEXT*94,GTEXT*94,PDESCR*60,SDESCR*44,CSTR*25
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character TSTR*6,ZN*12,RSNAME*40,SN*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character act*1
      integer lml   ! string length for construction name.
#ifdef OSX
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps. 
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

      IF(MMOD.EQ.8)THEN
        if(act.eq.'i')then
          call startbuffer
          call win3d(menuchw,9,24,8,3,igl,igr,igt,igb,igw,igwh)
          igw=igr-igl
          igwh=igb-igt
        elseif(act.eq.'s')then
          call startbuffer
          call win3d(menuchw,9,24,8,3,igl,igr,igt,igb,igw,igwh)
          igw=igr-igl
          igwh=igb-igt
        endif
      ENDIF

      lsn1=MIN0(lnblnk(RFILE),22)
      if(NSIM.gt.1)then
        WRITE(ETEXT,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      else
        WRITE(ETEXT,'(4A)')'Lib: ',RFILE(1:lsn1),
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      endif
      if(MMOD.EQ.8)then
        iside=1
        isize=0
        ifont=1
        call viewtext(etext,iside,isize,ifont)
      else
        call edisp(iuout,ETEXT)
      endif

      WRITE(ETEXT,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &   SDESCR(11:lnblnk(SDESCR))
      GTEXT=ETEXT
      if(MMOD.EQ.8)then
        iside=2
        isize=0
        ifont=1
        call viewtext(gtext,iside,isize,ifont)
      else
        call edisp(iuout,ETEXT)
      endif

      CALL SURADJ(ICOMP,ISUR,IE,T,IZC,ISC,ICN,CSTR)
      SN=SSNAME(ICN)
      ZN=zname(ICOMP)
      WRITE(ETEXT,'(6A)')' Temperature profiles within ',
     &   SN(1:lnblnk(SN)),' of ',ZN(1:lnblnk(ZN)),' facing ',
     &   CSTR(1:lnblnk(CSTR))
      if(MMOD.EQ.8)then
        iside=3
        isize=0
        ifont=1
        call viewtext(etext,iside,isize,ifont)
      else
        call edisp(iuout,ETEXT)
      endif

C Determine if the surface is transparent or opaque.
      IF(SSOTF(ICN)(1:4).EQ.'OPAQ'.OR.
     &   SSOTF(ICN)(1:4).EQ.'UNKN')THEN
         TSTR='opaque'
      ELSEIF(SSOTF(ICN)(1:4).EQ.'CFC ')THEN
         TSTR='CFC'
      ELSE
         TSTR='trnsp.'
      ENDIF

C Print info on surface.
      lml=lnblnk(SSMLCN(ICN))
      WRITE(ETEXT,'(5a,f6.2,a)')' It is ',TSTR,', composed of ',
     &  SSMLCN(ICN)(1:lml),', with area ',SNA(ICOMP,ISUR),' m2.'
      GTEXT=ETEXT
      if(MMOD.EQ.8)then
        iside=4
        isize=0
        ifont=1
        call viewtext(etext,iside,isize,ifont)
      else
        call edisp(iuout,ETEXT)
      endif

      RETURN
      END

C ******************** MOFDRW
C MOFDRW displays the construction temperature profiles.

      SUBROUTINE MOFDRW(ICOMP,ISUR,act)
#include "building.h"
#include "geometry.h"

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/ETHICK/ETHK(ME),THKMAX,THKMIN
      COMMON/FGRAF1/TMAX,TMIN,xsc,ysc,Xadd,Yadd
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C IPRZ is material db ref for each layer.
      common/T8/IPRZ(MS,ME)

      DIMENSION XVAL1(MZS)
      character act*1,ETEXT*94,TDESCR*5,DS*7,DS1*10,CSTR*25,TCSTR*25
      character ZN*12
#ifdef OSX
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym,iid1,iid2    ! passed to etplot
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym,iid1,iid2    ! passed to etplot
#endif

C Re-establish available colours.
      call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
      if(nifgrey.lt.4)then
        call usrmsg(
     &    'Limited screen colours available so will usefrequent ',
     &    '(i.e. may flicker) refresh to animate graph.','W')
      endif

C Determine the 'other' zone and connection information.
      CALL SURADJ(ICOMP,ISUR,IE,T,IZC,ISC,ICN,CSTR)
      TCSTR=CSTR
      ZN=zname(ICOMP)

C Get and display results.
      icnt=0

C Draw construction section.
      iid1=igl; iid2=igt-10;
      call wstxpt(iid1,iid2,TCSTR)
      iid1=igr; iid2=igt-10;
      call wstxpt(iid1,iid2,ZN)
      igr=13
      X=0.
      YD=TMAX-TMIN
      NE=NELTS(ICOMP,ISUR)
      DO 101 L=1,NE
        XL=X
        XD=ETHK(L)
        X=X+ETHK(L)
        if(IPRZ(ISUR,L).ne.0)call egrbox(XL,TMIN,XD,YD,igr)
  101 CONTINUE

      DO 10 I=IODS,IODF
        ID=I

C For requested zone and construction and for every computational
C time-step, get temperature results for a complete day.
        CALL CHKTIME(ID,ISTART,IEND)
        CALL MOFGET(ICOMP,ISUR,ID)

        NN=2*NELTS(ICOMP,ISUR)
        NNT=NN+1
        DO 20 J=ISTART,IEND,NOUT

C Set all possible values from VAL1, computing average if required.
C Placing them in XVAL1 and then into VAL2.
          CALL VALIAV(NN+3,J,XVAL1)
          DO 140 K=1,NN+3
            VAL2(K,J)=XVAL1(K)
  140     CONTINUE

C Commence drawing. Remember that everything shifted over 1
C to allow for outside temp to be in VAL2(1,?)
          icnt=icnt+1
          X=THKMIN
          iupdown=0
          isym=0
          call etplot(X,VAL2(1,J),iupdown,isym)
          X=0.
          iupdown=1
          DO 90 L=1,NNT
            call etplot(X,VAL2(L+1,J),iupdown,isym)
            M=((L-1)/2)+1
            X=X+ETHK(M)/2.
   90     CONTINUE
          iupdown=1
          call etplot(THKMAX,VAL2(NNT+2,J),iupdown,isym)
          call forceflush
          if(act.eq.'a')then

C If single-stepping pause, every 20 timesteps refresh the axis.
C If there are limited interface colours then revert to refreshing at
C each timestep. Pause 200msec at each timestep.
            call pausems(200)
            if(nifgrey.lt.4)then
              call MOFHED(ICOMP,ISUR,'s')
              call MOFAXS(ICOMP,ISUR)
              icnt=0
            else
              if(icnt.gt.10)then
                call MOFAXS(ICOMP,ISUR)
                icnt=0
              endif
            endif
            call STIME(j,TDESCR)
            call STDATE(IYEAR,ID,DS,DS1)
            WRITE(ETEXT,'(4A)')' Time ',DS1,' at ',TDESCR
            iside=5
            isize=0
            ifont=1
            call viewtext(etext,iside,isize,ifont)

C If possible reset the foreground colour to white and redraw the line.
            if(nifgrey.gt.4)then
              call winscl('i',nifgrey-1)
              X=THKMIN
              iupdown=0
              isym=0
              call etplot(X,VAL2(1,J),iupdown,isym)
              X=0.
              iupdown=1
              DO 91 L=1,NNT
                call etplot(X,VAL2(L+1,J),iupdown,isym)
                M=((L-1)/2)+1
                X=X+ETHK(M)/2.
   91         CONTINUE
              iupdown=1
              call etplot(THKMAX,VAL2(NNT+2,J),iupdown,isym)
              call forceflush()
              call winscl('-',0)
            endif

C Re-Draw construction section.
            iid1=igl; iid2=igt-10;
            call wstxpt(iid1,iid2,TCSTR)
            iid1=igr; iid2=igt-10;
            call wstxpt(iid1,iid2,ZN)
            igr=13
            X=0.
            YD=TMAX-TMIN
            NE=NELTS(ICOMP,ISUR)
            DO 102 L=1,NE
              XL=X
              XD=ETHK(L)
              X=X+ETHK(L)
              if(IPRZ(ISUR,L).ne.0)call egrbox(XL,TMIN,XD,YD,igr)
  102       CONTINUE
            call forceflush
          else
            call forceflush
            call pausems(100)
          endif
   20   CONTINUE
   10 CONTINUE
      RETURN
      END
