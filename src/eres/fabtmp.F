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
C #include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/ETHICK/ETHK(ME),THKMAX,THKMIN

      CHARACTER outs*124

C Read (silently) in the zone geometry data.
      IUNITF=IFIL+1
      call georead(IUNITF,LGEOM(ICOMP),ICOMP,0,IUOUT,IER)
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
C GZPDBE: Get material reference for zone layers from mlc.
C Assumes that geometry file has been read and surface
C attributes are known. 

      SUBROUTINE GZPDBE(ICOMP,IER)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C IPRZ is materials array for each layer.
      common/T8/IPRZ(MS,ME)

      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

      CHARACTER outs*124,CSTR*25

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

C Find matching MLC. Gather materials array index for each 
C layer of each surface in the zone. IPRZ is used elsewhere.
        call matchmlcdesc(SMLCN(ICOMP,ISUR),ic)

        ICKNE=LAYERS(IC)
        DO 23 IL=1,LAYERS(IC)
          IPRZ(ISUR,IL)=IPRMAT(IC,IL)
   23   CONTINUE
        if(ICKNE.eq.0)then
          lml=lnblnk(SMLCN(ICOMP,ISUR))
          write(outs,'(5A)') ' The construction ',
     &      SMLCN(ICOMP,ISUR)(1:lml),' of ',SNAME(ICOMP,ISUR),
     &      ' was not found in the mlc db...'
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
      integer IZSTOCN
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
        CALL MOCLIM(IDAY,1)
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
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

      CALL MOFHED(ICOMP,ISUR,'i')
      XMAX=THKMAX
      XMIN=THKMIN

C Determine the scaling for both axis and place in static 
C memory for access by plotting code.
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,XMIN,XMAX,TMIN,TMAX,xsc,ysc,sca,
     &              Xadd,Yadd)
      call linescale(iigl,Xadd,xsc,iigb,Yadd,ysc)
      call dintervalf(Xmin,Xmax,ddy1,ny,0)
      call horaxisdd(XMIN,XMAX,iigl,iigr,iigb,Xadd,xsc,0,
     &  ddy1,ny,'Thickness (mm)')
      CALL dintervalf(TMIN,TMAX,ddy1,ny,0)
      call vrtaxisdd(TMIN,TMAX,iigl,iigb,iigt,Yadd,ysc,0,
     &  ddy1,ny,0,'Temp. Deg.C')
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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM

      CHARACTER ETEXT*94,GTEXT*94,PDESCR*64,SDESCR*44,CSTR*25
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character TSTR*6,ZN*12,RSNAME*40,SN*12
      character act*1
      integer lml   ! string length for construction name.
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps. 
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

      IF(MMOD.EQ.8)THEN
        if(act.eq.'i')then
          call startbuffer

C Setup and pass in parameters to win3d.
          iiw1=9; iiw2=24; iiw3=8; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
        elseif(act.eq.'s')then
          call startbuffer
          iiw1=9; iiw2=24; iiw3=8; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
        endif
      ENDIF

      lsn1=MIN0(lnblnk(RFILE),32)
      if(NSIM.gt.1)then
        WRITE(ETEXT,'(3A,I4,3A)')'Results library: ',
     &    RFILE(1:lsn1),'; results set:',ISIM,
     &    ' (',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
      else
        WRITE(ETEXT,'(5A)')'Results library: ',RFILE(1:lsn1),
     &    '; (',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
      endif
      if(MMOD.EQ.8)then
        iside=1; isize=0; ifont=1
        call viewtext(etext,iside,isize,ifont)
      else
        call edisp(iuout,ETEXT)
      endif

      WRITE(ETEXT,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &   SDESCR(12:lnblnk(SDESCR))
      GTEXT=ETEXT
      if(MMOD.EQ.8)then
        iside=2; isize=0; ifont=1
        call viewtext(gtext,iside,isize,ifont)
      else
        call edisp(iuout,ETEXT)
      endif

      CALL SURADJ(ICOMP,ISUR,IE,T,IZC,ISC,ICN,CSTR)
      SN=SNAME(ICOMP,ISUR)
      ZN=zname(ICOMP)
      WRITE(ETEXT,'(6A)')' Temperature profiles within ',
     &   SN(1:lnblnk(SN)),' of ',ZN(1:lnblnk(ZN)),' facing ',
     &   CSTR(1:lnblnk(CSTR))
      if(MMOD.EQ.8)then
        iside=3; isize=0; ifont=1
        call viewtext(etext,iside,isize,ifont)
      else
        call edisp(iuout,ETEXT)
      endif

C Determine if the surface is transparent or opaque. At this point
C CFC and CFC2 are still classed with opaque.
      IF(SOTF(ICOMP,ISUR)(1:4).EQ.'OPAQ'.OR.
     &   SOTF(ICOMP,ISUR)(1:4).EQ.'UNKN')THEN
         TSTR='opaque'
      ELSEIF(SOTF(ICOMP,ISUR)(1:4).EQ.'CFC ')THEN
         TSTR='CFC'
      ELSEIF(SOTF(ICOMP,ISUR)(1:4).EQ.'CFC2')THEN
         TSTR='CFC2'
      ELSE
         TSTR='trnsp.'
      ENDIF

C Print info on surface.
      lml=lnblnk(SMLCN(ICOMP,ISUR))
      WRITE(ETEXT,'(5a,f6.2,a)')' It is ',TSTR,', composed of ',
     &  SMLCN(ICOMP,ISUR)(1:lml),', with area ',SNA(ICOMP,ISUR),' m2.'
      GTEXT=ETEXT
      if(MMOD.EQ.8)then
        iside=4; isize=0; ifont=1
        call viewtext(etext,iside,isize,ifont)
      else
        call edisp(iuout,ETEXT)
      endif

      RETURN
      END

C ******************** MOFDRW
C MOFDRW displays the construction temperature profiles.

      SUBROUTINE MOFDRW(ICOMP,ISUR,act,ispeed)
#include "building.h"
#include "geometry.h"

C Passed parameters
      integer icomp  ! zone index
      integer isur   ! surface index
      integer ispeed ! 1 is 200ms 2 is 100ms 3 is 50ms 4 is 25ms

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

C IPRZ is materials array index for each layer.
      common/T8/IPRZ(MS,ME)

      DIMENSION XVAL1(MZS)
      character act*1,ETEXT*94,TDESCR*5,DS*7,DS1*10,DS2*8
      character CSTR*25,TCSTR*25
      character ZN*12
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym,iid1,iid2    ! passed to etplot
      integer iicol,ibsize
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym,iid1,iid2    ! passed to etplot
      integer*8 iicol,ibsize
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
      iicol=0; ibsize=0
      call textsizeatxy(iid1,iid2,TCSTR,ibsize,'-',iicol)
      iid1=igr; iid2=igt-10;
      call textsizeatxy(iid1,iid2,ZN,ibsize,'-',iicol)
      igr=13
      XX=0.
      YD=TMAX-TMIN
      NE=NELTS(ICOMP,ISUR)

C Draw a grey box for each layer that is not a gap.
      DO 101 L=1,NE
        XL=XX
        XD=ETHK(L)
        XX=XX+ETHK(L)
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
          XX=THKMIN
          iupdown=0
          isym=0
          call etplot(XX,VAL2(1,J),iupdown,isym)
          XX=0.
          iupdown=1
          DO 90 L=1,NNT
            call etplot(XX,VAL2(L+1,J),iupdown,isym)
            M=((L-1)/2)+1
            XX=XX+ETHK(M)/2.
   90     CONTINUE
          iupdown=1
          call etplot(THKMAX,VAL2(NNT+2,J),iupdown,isym)
          call forceflush
          if(act.eq.'a')then

C If single-stepping pause, every 20 timesteps refresh the axis.
C If there are limited interface colours then revert to refreshing at
C each timestep. Pause 200msec at each timestep.
            if(ispeed.eq.1)then
              call pausems(200)
            elseif(ispeed.eq.2)then
              call pausems(100)
            elseif(ispeed.eq.3)then
              call pausems(50)
            elseif(ispeed.eq.4)then
              call pausems(25)
            endif
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
            call STDATE(IYEAR,ID,DS,DS1,DS2)
            WRITE(ETEXT,'(4A)')' Time ',DS1,' at ',TDESCR
            iside=5; isize=0; ifont=1
            call viewtext(etext,iside,isize,ifont)

C If possible reset the foreground colour to white and redraw the line.
            if(nifgrey.gt.4)then
              iicol=nifgrey-1
              call winscl('i',iicol)
              XX=THKMIN
              iupdown=0
              isym=0
              call etplot(XX,VAL2(1,J),iupdown,isym)
              XX=0.
              iupdown=1
              DO 91 L=1,NNT
                call etplot(XX,VAL2(L+1,J),iupdown,isym)
                M=((L-1)/2)+1
                XX=XX+ETHK(M)/2.
   91         CONTINUE
              iupdown=1
              call etplot(THKMAX,VAL2(NNT+2,J),iupdown,isym)
              call forceflush()
              iicol=0
              call winscl('-',iicol)
            endif

C Re-Draw construction section.
            iid1=igl; iid2=igt-10;
            iicol=0; ibsize=0
            call textsizeatxy(iid1,iid2,TCSTR,ibsize,'-',iicol)
            iid1=igr; iid2=igt-10;
            call textsizeatxy(iid1,iid2,ZN,ibsize,'-',iicol)
            igr=13
            XX=0.
            YD=TMAX-TMIN
            NE=NELTS(ICOMP,ISUR)
            DO 102 L=1,NE
              XL=XX
              XD=ETHK(L)
              XX=XX+ETHK(L)
              if(IPRZ(ISUR,L).ne.0)call egrbox(XL,TMIN,XD,YD,igr)
  102       CONTINUE
            call forceflush
          else
            call STIME(j,TDESCR)
            call STDATE(IYEAR,ID,DS,DS1,DS2)
            WRITE(ETEXT,'(4A)')' Time ',DS1,' at ',TDESCR
            iside=5; isize=0; ifont=1
            call viewtext(etext,iside,isize,ifont)
            call forceflush
            if(ispeed.eq.1)then
              call pausems(100)
            elseif(ispeed.eq.2)then
              call pausems(60)
            elseif(ispeed.eq.3)then
              call pausems(40)
            elseif(ispeed.eq.4)then
              call pausems(35)
            endif
          endif
   20   CONTINUE
   10 CONTINUE
      RETURN
      END
