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

C fabcon.f
C This file contains the following routines:

C MOFCON: controlling routine for inter-fabric temperature and 
C         condensation.
C MOIHED
C MOSURC
C MOINTC
C MOPRES
C SVP
C MOIAXS
C MOIDRW

C ******************** MOFCON

C MOFCON is the main controlling routine for the output
C of inter-fabric temperature and condensation conditions.

      SUBROUTINE MOFCON

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/OUTIN/IUOUT,IUIN
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/pophelp/h(60)
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      CHARACTER*23 ITEM(10)
      character H*72,xfile*144,tg*1,delim*1,act*1

C Setup for menu.
   3  ITEM(1) ='2 select result set    '
      ITEM(2) ='3 define output period '
      ITEM(3) ='4 select zones         '
      ITEM(4) ='  ____________________ '
      ITEM(5) ='a Xsection temperatures'
      ITEM(6) ='b graph interstitial   '
      ITEM(7 )='  ____________________ '
      if(ixopen.eq.1)then
        ITEM(8)='> output >> file     '
      elseif(ixopen.eq.0)then
        ITEM(8)='> output >> screen   '
      endif
      ITEM(9)='? help                 '
      ITEM(10)='- exit                 '

C Instanciate help for this menu.
      H(1) ='This facility provides graphs on condensation '
      H(2) ='and intra-construction temperature patterns.  '
      H(3) ='  '
      H(4) ='Glazer plots through constructions and searches for  '
      H(5) ='condensation within one or more zones. '
      H(6) ='  '

      NITMS=10
      INO=-2
    4 CALL EMENU('Fabric',ITEM,NITMS,INO)

      IF(INO.EQ.1)then
        CALL MORESS
      elseif(INO.EQ.2)then
        CALL MOOPER
      elseif(INO.EQ.3)then
        CALL MOZDFN
      elseif(INO.EQ.5)then
        if(ISAVE.eq.3)then

C Display the variation in the temperature profile through
C any construction over any defined period of time.
C Request zone and surface for output and check if graph should be
C plotted over the whole period or one timestep at a time.
          CALL ASKZON(ICOMP)
          CALL ASKSUR(ICOMP,ISUR)
          h(1)='Animation shows each timestep in sequence.'
          CALL EASKABC('When plotting temperatures:',' ',
     &      'show all timesteps at once','animate data display',
     &      'continue',IW,1)
          if(IW.eq.1)act='-'
          if(IW.eq.2)act='a'

C Get element thicknesses for this construction.
          CALL MOTHKE(ICOMP,ISUR)

C Determine max. and min. temperatures for display.
          CALL MOFMXN(ICOMP,ISUR)

C Compute scale and draw axes.
          CALL MOFAXS(ICOMP,ISUR)

C Draw profiles.
          CALL MOFDRW(ICOMP,ISUR,act)
        else
          goto 5
        endif
      elseif(INO.EQ.6)then
        if(ISAVE.eq.3)then
          CALL ASKZON(ICOMP)
          CALL MOINTC(ICOMP)
        else
          goto 5
        endif
      elseif(INO.EQ.8)then
        call ctlexp(xfile,ixopen,ixunit,ixpunit,'T','Fabric',IER)
      elseif(INO.EQ.9)then
        CALL PHELPD('condens',6,'-',0,0,IER)
      elseif(INO.EQ.10)then
        return
      else
        INO=-1
        goto 3
      endif
      goto 3

    5 call edisp(iuout,' The save option associated with the')
      call edisp(iuout,' currently assigned result set is not 3; ')
      call edisp(iuout,' consequently this command is not available.')
      goto 1

C Illegal menu pick.
    1 INO=-1
      goto 4

      END


C ******************** MOIHED

C MOIHED outputs the appropriate header information corresponding
C to the fabric temperature or condensation information.

      SUBROUTINE MOIHED(ICOMP,ISUR)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM
      COMMON/RECV5/SNA(MCOM,MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/VAP1/IDO,IMO,TIMO,IDAYO,ITIMO
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      CHARACTER ETEXT*94,PDESCR*60,SDESCR*44,RSNAME*40,CSTR*25
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character TSTR*6,ZN*12,SN*12,DS*7,DS1*10
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      integer lml   ! string length for construction name.
#ifdef OSX
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps. Use opengdisp to get
C pixel references for calls to viewtext.
      CALL SURADJ(ICOMP,ISUR,IE,T,IZC,ISC,ICN,CSTR)
      SN=SSNAME(ICN)
      ZN=zname(ICOMP)
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

      IF(MMOD.EQ.8)THEN
        call startbuffer
        call win3d(menuchw,9,24,8,3,igl,igr,igt,igb,igw,igwh)
        igw=igr-igl
        igwh=igb-igt
      ENDIF

      lsn1=MIN0(lnblnk(RFILE),22)
      if(NSIM.gt.1)then
        WRITE(ETEXT,'(3A,I4,A,A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
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

      WRITE(ETEXT,'(6A)')' Condensation within ',
     &  SN(:lnblnk(SN)),' of ',ZN(:lnblnk(ZN)),' facing ',
     &  CSTR(1:lnblnk(CSTR))
      if(MMOD.EQ.8)then
        iside=2
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

C Display surface information.
      lml=lnblnk(SSMLCN(ICN))
      WRITE(ETEXT,'(5a,f6.2,a)')' It is ',TSTR,', composed of ',
     &  SSMLCN(ICN)(1:lml),', with area ',SNA(ICOMP,ISUR),' m2.'
      if(MMOD.EQ.8)then
        iside=3
        isize=0
        ifont=1
        call viewtext(etext,iside,isize,ifont)
      else
        call edisp(iuout,ETEXT)
      endif

      call STDATE(IYEAR,IDAYO,DS,DS1)
      WRITE(ETEXT,'(3A,f6.2)')' Time ',DS1,' at ',TIMO
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


C ******************** MOSURC
C MOSURC identifies which surfaces are below the air dew point temperature.
C If izrh=0 then use current zone RH, if izrh>0 then use izrh value for
C zone RH. If iverb=0 then report only summary, if iverb=1 report each
C timestep.
      SUBROUTINE MOSURC(IZONE,izrh,iverb)
#include "building.h"
#include "geometry.h"

      COMMON/OUTPCH/ICOUT
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      common/zcondhr/izctime(MCON)

      DIMENSION TSURF(MS),TSO(MS),ihit(ms)
      CHARACTER*1 A(MS)
      character outs*124,xfile*144,TDESCR*5,DESCR1*10,DESCR*7
      character tg*1,delim*1,CSTR*25
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      logical found

C Set chosen result set.
      ISET=ISIM

C Find the number of hours represented by an output timestep. Use
      TFACT=FLOAT(NOUT)/FLOAT(NTS)

C Set values as common block gets overwritten.
      patmos=1013.25
      iopt=2
      N=24*NTS

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
      endif

C For each day of the reporting period.
      do 11 i=1,MS
        ihit(i)=0
  11  continue
      NC=NZSUR(IZONE)
      if(iverb.eq.0)then
        write(outs,'(2a)')' Summary condensation report for: ',
     &    zname(IZONE)(1:lnzname(IZONE))
        call edisp(itru,outs)
      endif
      DO 30 IDO=IODS,IODF

C Heading.
        if(iverb.eq.1)then
          CALL STDATE(IYEAR,IDO,DESCR,DESCR1)
          call edisp(itru,' ')
          write(outs,'(a,a,a,a10)')' Summary condensation report for: ',
     &      zname(IZONE)(1:lnzname(IZONE)),' on ',DESCR1
          call edisp(itru,outs)
          call edisp(itru,' ')
          call edisp(itru,' Time                       Surface')
          write(outs,72)
   72     format(' (Hrs)   1  2  3  4  5  6  7  8  9 10 11 12 13',
     &      ' 14 15 16 17 18 19 20 21 22 23 24')
          call edisp(itru,outs)
        endif

C For each timestep in the day extract the appropriate data from
C VAL1, or return surface temperatures at timestep in TSURF.
        do 40 ITIMO=1,N
          if(izrh.eq.0)then
            call GZTMS(IDO,IZONE,ISET,ITIMO,TSURF,TSO,TI,TMCON,A1,A2,
     &                                                         A5,RHI)
          else
            RHI=float(izrh)
          endif

C Dew point temperature.
          GVALX=HUMRT1(TI,RHI,PATMOS,IOPT)
          TDP=DEWPT(GVALX,PATMOS)

          DO 70 L=1,MS
            A(L)=' '
   70     CONTINUE
          found=.false.
          DO 80 L=1,NC
            if(TSURF(L).LT.TDP)then
              A(L)='X'
              ihit(L)=ihit(L)+1
              found=.true.
            else
              A(L)='.'
            endif
   80     CONTINUE

C izctime is number of timesteps when some surface in zone has condensation.
          if(found)izctime(IZONE)=izctime(IZONE)+1

          CALL STIME(ITIMO,TDESCR)
          WRITE(outs,9)TDESCR,(A(L),L=1,NC)
    9     FORMAT(1X,A5,2X,A1,32(2X,A1))
          if(iverb.eq.1)call edisp(itru,outs)
   40   CONTINUE
   30 CONTINUE

      call edisp(itru,' Surface  occurances  hours')
      do 42 ij=1,NC
        CALL SURADJ(izone,ij,IE,T,IZC,ISC,ICN,CSTR)
        hrs=ihit(ij)*TFACT
        WRITE(outs,'(1x,a,i5,f8.2)') ssname(icn),ihit(ij),hrs
        call edisp(itru,outs)
  42  continue
      hrs=izctime(IZONE)*TFACT
      WRITE(outs,'(2a,i5,f8.2)') ' Total occurances & hours in ',
     &  zname(IZONE)(1:lnzname(IZONE)),izctime(IZONE),hrs
      call edisp(itru,outs)
      RETURN
      END

C ******************** MOINTC
C MOINTC displays the variation in the vapour pressure
C and saturated vapour pressure through any defined
C multilayered construction at any user-specified time.

      SUBROUTINE MOINTC(IZ)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
C esprdbfile.h supplies the following:
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/VAP1/IDO,IMO,TIMO,IDAYO,ITIMO
      COMMON/VAP2/THKN(MN),VRN(MN)

      COMMON/ETHICK/ETHK(ME),THKMAX,THKMIN
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)

C VDIF is diffusion res, IPRZ is primitive db ref for each layer.
      COMMON/T7/VDIF(ME)
      common/T8/IPRZ(MS,ME)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the index within matdatarray
      logical closemat1,closemat2

      character outs*124,H*72,NAM*72,TITL*72
      logical ok,dok

      IPICK=0

C Request surface for output.
    5 CALL ASKSUR(IZ,ISUR)

C Get element thicknesses for this construction.
      CALL MOTHKE(IZ,ISUR)

C Find version of materials database.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)

C Convert to node-related thicknesses.
      NN=2*NE(ISUR)+1
      THKN(1)=0.
      DO 10 I=2,NN
        J=I/2
        THKN(I)=THKN(I-1)+ETHK(J)/2000.
   10 CONTINUE

C Determine diffusion resistance factor for each element.
      call edisp(iuout,' Diffusion resistance factor for:')
      call edisp(iuout,' ')
      DO 20 I=1,NE(ISUR)
        if(closemat1.or.closemat2)then
          legmat=IPRZ(ISUR,I)   ! which legacy index
          if(legmat.ge.0)then
            matarrayindex=mathash(legmat)  ! find matching array index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
            if(matarrayindex.eq.0)then
              DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
              E=0.99; A=0.99; DRVAL=1.0
              TITL='GAPS'; NAM='AIR'
            else
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
              E=matdbine(matarrayindex)
              A=matdbina(matarrayindex)
              DRVAL=matdbdrv(matarrayindex)
              write(TITL,'(a)') 'placeholder'
              write(NAM,'(a)') matname(matarrayindex)(1:32)
            endif
          endif
        else
          CALL ERPCDB(IFMAT,IPRZ(ISUR,I),iuout,IER,DBCON,DBDEN,
     &      DBSHT,E,A,DRVAL,TITL,NAM)
        endif
        H(1)='Diffusion resistance factors are found in the'
        H(2)='materials database.'
        write(outs,'(A,I3,2x,A20)')' Element ',I,NAM(1:20)
        V=DRVAL
        CALL EASKR(V,outs,' Vapour res (MNs g^-1m^-1)? ',
     &    0.,'F',0.,'-',1.,'diffusion factor',IER,2)
        VDIF(I)=V
   20 CONTINUE

C Convert to node-related vapour resistance.
      VRN(1)=0.
      DO 30 I=2,NN
        J=I/2
        VRN(I)=(VDIF(J)*ETHK(J)/2000.)/19.2E-11
   30 CONTINUE

      IF(IPICK.ne.0)goto 42

C Ask for time.
      IDO=IOD1
      IMO=IOM1
   41 call askrtim(IFDAY,IMO,IDO,IJDAY,TIME,IT,IER)
      IDAYO=IJDAY
      TIMO=TIME
      ITIMO=IT

  42  CALL MOPRES(IZ,ISUR)

C Display header, draw axis and then profiles.
      CALL MOIHED(IZ,ISUR)
      CALL MOIAXS(IZ,ISUR)
      CALL MOIDRW(IZ,ISUR)

      dok=.false.
      h(1)='If you want to look at a different day, time or '
      h(2)='a different surface say yes. '
      h(3)='A no will return to the main menu. '
      call askok(' ',' Consider another condition ? ',OK,dok,3)
      if(.NOT.ok)then
        RETURN
      endif
      H(1)='If you want to alter both the surface and time then '
      H(2)='you will have the option of a second '
      CALL EASKABC('Consider:',' ','Different surface',
     &          'Different day or time','Both ? ',IW,1)
      if(IW.eq.3)then
        IPICK=0
        goto 5
      elseif(IW.eq.1)then
        IPICK=1
        goto 5
      elseif(IW.eq.2)then
        IPICK=0
        goto  41
      endif

      END

C ******************** MOPRES

C MOPRES recovers the nodal temperature information from
C the results database and computes the vapour pressure
C and saturated vapour pressure gradients.

      SUBROUTINE MOPRES(ICOMP,ISUR)
#include "building.h"
#include "site.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      COMMON/VAP1/IDO,IMO,TIMO,IDAYO,ITIMO
      COMMON/VAP2/THKN(MN),VRN(MN)
      COMMON/VAP3/PSS(MN),PS(MN),PSSI,PSSO,PSI,PSO,
     &            PMAX,PMIN,xsc,ysc,Xadd,Yadd

      DIMENSION TMP(MN)
      CHARACTER CSTR*25

C Set chosen result set.
      ISET=ISIM

C Set values as common block gets overwritten.
      PATMOS=1013.25

C Recover data from database.
C Get zone rh:
      CALL GZRH(IDAYO,ICOMP,ISET)
      CALL XTVAL1(GVAL,1,ITIMO,1)
      RHI=VAL1(1,ITIMO)

C Get zone air temp.
      CALL GZAIRT(IDAYO,ICOMP,ISET)
      CALL XTVAL1(GVAL,1,ITIMO,2)
      TI=VAL1(2,ITIMO)

C Get inside surface temperature.
      call GTMS(IDAYO,ICOMP,ISUR,ISET)
      CALL XTVAL1(GVAL,1,ITIMO,3)
      TIS=VAL1(3,ITIMO)

C Get node temps and add in internal surf temp.
      NN=2*NELTS(ICOMP,ISUR)
      NNM=NN+1
      NNT=NN+1
      call GATMC(IDAYO,icomp,isur,ITIMO,tmp)
      TMP(NNT)=TIS

C Change internal RH to percentage saturation.
      PSATI=PSATR(TI,RHI,PATMOS)

C Determine the 'other' zone and connection information.
C IE = 0 ; external, IE = 1 ; internal/same
C IE = 2 ; internal/TMP defines temperature
C IE = 3 ; internal/IZC defines coupled zone
C IE = 4 ; ground temperature
      CALL SURADJ(ICOMP,ISUR,IE,TMPO,IZC,ISC,ICN,CSTR)
      icc=izstocn(icomp,isur)
      if(ie.eq.0)then

C External temp and RH.
        CALL MOCLIM(IDAYO,2)
        TO=VAL3(2,ITIMO)
        CALL MOCLIM(IDAYO,28)
        RHO=VAL3(6,ITIMO)
      elseif(ie.eq.1)then
        TO=TI
        PSATO=PSATI
        goto 3
      elseif(ie.eq.2)then
        TO=TMPO
        PSATO=PSATI
        goto 3
      elseif(ie.eq.3)then
        CALL GZRH(IDAYO,IZC,ISET)
        CALL XTVAL1(GVAL,1,ITIMO,5)
        RHO=VAL1(5,ITIMO)
        CALL GZAIRT(IDAYO,IZC,ISET)
        CALL XTVAL1(GVAL,1,ITIMO,6)
        TO=VAL1(6,ITIMO)
      elseif(ie.eq.4)then

C Ground coupled get the current month and get ground temperature.
        CALL EDAYR(IDAYO,IDAYN,IMTHN)

C If standard ground profile get it, if 3D ground set to 1.0C
C and if user defined get it.
        if(ic2(icc).gt.0)then
          T0=GRDTMP(IMTHN,ic2(icc))
        elseif(ic2(icc).eq.-3)then
          T0=1.0
        else
          TO=UGRDTP(IMTHN,ie2(icc))
        endif
        CALL MOCLIM(IDAYO,28)
        RHO=VAL3(6,ITIMO)
      endif

C Change external RH to percentage saturation.
      PSATO=PSATR(TO,RHO,PATMOS)

C Compute saturated vapour pressure and sort.
    3 PSSI=SATVP(TI)
      DO 20 I=1,NNM
        PSS(I)=SATVP(TMP(I))
   20 CONTINUE
      PSSO=SATVP(TO)

C Compute vapour pressure.
      TVR=0.
      DO 30 I=2,NNT
        TVR=TVR+VRN(I)
   30 CONTINUE
      PSI=PSSI*PSATI/100.
      PS(NNM)=PSI
      PSO=PSSO*PSATO/100.
      PS(1)=PSO
      XM=(PSO-PSI)/TVR

      PLAST=PSO
      JJ=1
      DO 40 I=2,NN
        JJ=JJ+1
        PS(I)=PLAST-XM*VRN(JJ)
        PLAST=PS(I)
   40 CONTINUE

C Determine max. and min. values of pressure.
      PMAX=-1.E+10
      PMIN=1.E+10

      PMAX=AMAX1(PMAX,PSSI,PSSO,PSI,PSO)

      PMIN=AMIN1(PMIN,PSSI,PSSO,PSI,PSO)

      DO 50 I=1,NN
        PMAX=AMAX1(PMAX,PSS(I))
        PMIN=AMIN1(PMIN,PSS(I))
        PMAX=AMAX1(PMAX,PS(I))
        PMIN=AMIN1(PMIN,PS(I))
   50 CONTINUE

C Round values for display.
      PMIN1=PMIN
      PMIN=int(PMIN1-1.)

      RETURN
      END

C ******************** SVP
C Calculate saturated vapour pressure.

      FUNCTION SVP(TEMP)
      X=(10.)**(28.59051-8.2*ALOG10(TEMP+273.16)+
     &  2.4804*.001*(TEMP+273.16)-3142.31/(TEMP+273.16))
      X=X*100.
      SVP=X
      RETURN
      END

C ******************** MOIAXS

C MOIAXS draws the axes relating to interstitial
C condensation graphical output.

      SUBROUTINE MOIAXS(ICOMP,ISUR)
#include "building.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/ETHICK/ETHK(ME),THKMAX,THKMIN
      COMMON/OUTPCH/ICOUT
      COMMON/VAP1/IDO,IMO,TIMO,IDAYO,ITIMO
      COMMON/VAP3/PSS(MN),PS(MN),PSSI,PSSO,PSI,PSO,
     &            PMAX,PMIN,xsc,ysc,Xadd,Yadd

      character outs*124
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
#else
      integer*8 igwid,igheight  ! for use with axiscale
#endif

      XMAX=THKMAX
      XMIN=THKMIN

      call edisp(icout,' Interstitial condensation output')
      write(outs,'(A,I3,A,I3,A,I3,A,I3,A,F6.2)')' Zone',ICOMP,
     &  ' construction',ISUR,': on day',IDO,' of month',IMO,' at:',TIMO
      call edisp(icout,outs)

C Compute scaling factors and draw the axis.
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,XMIN,XMAX,PMIN,PMAX,xsc,ysc,sca,
     &              Xadd,Yadd)
      call linescale(igl,Xadd,xsc,igb,Yadd,ysc)
      call horaxis(XMIN,XMAX,igl,igr,igb,Xadd,xsc,0,'Thickness (mm)')
      call vrtaxis(PMIN,PMAX,igl,igb,igt,Yadd,ysc,0,0,'Pressure mbar')

      RETURN
      END

C ******************** MOIDRW ********************

C MOIDRW displays the saturated vapour pressure and
C and vapour pressure gradients.

      SUBROUTINE MOIDRW(ICOMP,ISUR)
#include "building.h"
#include "geometry.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C IPRZ is primitive db ref for each layer.
      common/T8/IPRZ(MS,ME)
      COMMON/ETHICK/ETHK(ME),THKMAX,THKMIN
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/VAP2/THKN(MN),VRN(MN)
      COMMON/VAP3/PSS(MN),PS(MN),PSSI,PSSO,PSI,PSO,
     &            PMAX,PMIN,xsc,ysc,Xadd,Yadd

      character CSTR*25,TCSTR*25,ZN*12
#ifdef OSX
      integer ipos,isize      ! for use with etlabel
      integer iupdown,isym    ! passed to etplot
      integer iid1,iid2
#else
      integer*8 ipos,isize      ! for use with etlabel
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iid1,iid2
#endif

C Determine the 'other' zone and connection information.
      CALL SURADJ(ICOMP,ISUR,IE,T,IZC,ISC,ICN,CSTR)
      TCSTR=CSTR
      ZN=zname(ICOMP)

C Draw construction section.
      iid1=igl; iid2=igt-10;
      call wstxpt(iid1,iid2,TCSTR)
      iid1=igr; iid2=igt-10;
      call wstxpt(iid1,iid2,ZN)
      igr=13
      X=0.
      YD=PMAX-PMIN
      LAY=NELTS(ICOMP,ISUR)
      DO 101 L=1,LAY
        XL=X
        XD=ETHK(L)
        X=X+ETHK(L)
        if(IPRZ(ISUR,L).ne.0)call egrbox(XL,PMIN,XD,YD,igr)
  101 CONTINUE

C Plot saturated vapour pressure.
      NN=2*NE(ISUR)+1
      iupdown=0
      isym=0
      call etplot(THKMIN,PSSO,iupdown,isym)
      iupdown=1
      DO 20 I=1,NN
        call etplot(THKN(I)*1000.,PSS(I),iupdown,isym)
   20 CONTINUE
      iupdown=1
      call etplot(THKMAX,PSSI,iupdown,isym)
      ipos=3
      isize=1
      call etlabel('Sat Vap Pres',THKMAX,PSSI,ipos,isize)

C Plot vapour pressure.
      iupdown=0
      isym=0
      call etplot(THKMIN,PSO,iupdown,isym)
      iupdown=-5
      isym=6
      DO 30 I=1,NN
        call etplot(THKN(I)*1000.,PS(I),iupdown,isym)
   30 CONTINUE
      iupdown=-5
      isym=0
      call etplot(THKMAX,PSI,iupdown,isym)
      ipos=3
      isize=1
      call etlabel('Vap Pres',THKMAX,PSI,ipos,isize)

      RETURN
      END

C ******************** FINDCND
C FINDCND reports on condensation within a zone over the reporting
C period.

      SUBROUTINE FINDCND(ICOMP)
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/FILEP/IFIL

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

C IPRZ is primitive db ref for each layer.
      COMMON/T7/VDIF(ME)
      common/T8/IPRZ(MS,ME)
      COMMON/ETHICK/ETHK(ME),THKMAX,THKMIN

      DIMENSION TMP(MN),PSS(MN),PS(MN),VRN(MN)
      CHARACTER CSTR*25,outs*124,TDESCR*5,xfile*144
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character ZN*12,SN*12,DS*7,DS1*10,TITL*72,NAM*72,tg*1,delim*1
      integer legmat  ! local variable for legacy index.
      logical closemat1,closemat2
      logical newgeo  ! to use for testing if new/old geometry file.

C Set chosen result set.
      newgeo=.false.  ! assume older format geometry.
      ISET=ISIM

C Set values as common block gets overwritten.
      PATMOS=1013.25

C If output to file alter the edisp unit number.
      IF(MMOD.NE.-6)ICOUT=IUOUT
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(iuout,' Output being directed to file... ')
      endif

      N=24*NTS
      ZN=zname(ICOMP)

C Read (silently) in the zone geometry data.
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
        call edisp(iuout,' Could not find all primitives')
        RETURN
      ENDIF

C Read (silently) construction file.
      call edisp(iuout,' ')
      CALL ECONST(LTHRM(ICOMP),IUNITF,ICOMP,0,IUOUT,IER)
      IF(IER.NE.0)THEN
        write(outs,'(A,A72)')' Could not find ',LTHRM(ICOMP)
        CALL USRMSG(' ',outs,'W')
        RETURN
      ENDIF

C Check which version of materials database.
      call eclose(matver,1.1,0.001,closemat1)
      call eclose(matver,1.2,0.001,closemat2)

      write(outs,'(A,A)') ' Condensation search in: ',ZN(1:lnblnk(ZN))
      call edisp(iuout,outs)

C For each surface.
      do 20 ISUR=1,NZSUR(ICOMP)
        CALL SURADJ(ICOMP,ISUR,IE,T,IZC,ISC,ICN,CSTR)
        SN=SSNAME(ICN)

C Determine diffusion resistance factor for each element
C from ascii or binary materials db and compute layer thickness.
        DO 24 I=1,NE(ISUR)
          ETHK(I)=THK(ISUR,I)*1000.0
          if(closemat1.or.closemat2)then
            legmat=IPRZ(ISUR,I)   ! which legacy index
            if(legmat.ge.0)then
              matarrayindex=mathash(legmat)  ! find matching array index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
              if(matarrayindex.eq.0)then
                DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
                E=0.99; A=0.99; DRVAL=1.0
                TITL='GAPS'; NAM='AIR'
              else
                DBCON=matdbcon(matarrayindex)
                DBDEN=matdbden(matarrayindex)
                DBSHT=matdbsht(matarrayindex)
                E=matdbine(matarrayindex)
                A=matdbina(matarrayindex)
                DRVAL=matdbdrv(matarrayindex)
                write(TITL,'(a)') 'placeholder'
                write(NAM,'(a)') matname(matarrayindex)(1:32)
              endif
            endif
          else
            CALL ERPCDB(IFMAT,IPRZ(ISUR,I),iuout,IER,DBCON,DBDEN,
     &         DBSHT,E,A,DRVAL,TITL,NAM)
          endif
          VDIF(I)=DRVAL
   24   CONTINUE

        NN=2*NE(ISUR)
        NNM=NN+1
        NNT=NN+1

C Convert to node-related vapour resistance.
        VRN(1)=0.
        DO 32 I=2,NNM
          J=I/2
          VRN(I)=(VDIF(J)*ETHK(J)/2000.)/19.2E-11
   32   CONTINUE

C For each day.
        do 30 IDAYO=IODS,IODF
          CALL EDAYR(IDAYO,IDAY,IMNTH)

C Get zone rh into VAL1(1,?), zone air temp into VAL1(2,?)
C and inside surface temperature into VAL1(3,?).
          CALL GZRH(IDAYO,ICOMP,ISET)
          CALL XTVAL1(GVAL,1,N,1)
          CALL GZAIRT(IDAYO,ICOMP,ISET)
          CALL XTVAL1(GVAL,1,N,2)
          call GTMS(IDAYO,ICOMP,ISUR,ISET)
          CALL XTVAL1(GVAL,1,N,3)

C Determine the 'other' zone and connection information.
          CALL SURADJ(ICOMP,ISUR,IE,TMPO,IZC,ISC,ICN,CSTR)
          icc=izstocn(icomp,isur)
          if(ie.eq.0)then
            CALL MOCLIM(IDAYO,2)
            CALL MOCLIM(IDAYO,28)
          elseif(ie.eq.3)then
            CALL GZAIRT(IDAYO,IZC,ISET)
            CALL XTVAL1(GVAL,1,N,4)
            CALL GZRH(IDAYO,IZC,ISET)
            CALL XTVAL1(GVAL,1,N,5)
          elseif(ie.eq.4)then
            CALL MOCLIM(IDAYO,28)
          endif

C For each timestep in the day extract the appropriate data from
C VAL1 and VAL3.
          do 40 ITIMO=1,N
            RHI=VAL1(1,ITIMO)
            TI=VAL1(2,ITIMO)
            TIS=VAL1(3,ITIMO)

C Get node temps at each timestep.
            call GATMC(IDAYO,icomp,isur,ITIMO,tmp)
            TMP(NN+1)=TIS

C Change internal RH to percentage saturation.
            PSATI=PSATR(TI,RHI,PATMOS)

C Depending on the other side conditions...
            if(ie.eq.0)then
              TO=VAL3(2,ITIMO)
              RHO=VAL3(6,ITIMO)
            elseif(ie.eq.1)then
              TO=TI
              PSATO=PSATI
              goto 3
            elseif(ie.eq.2)then
              TO=TMPO
              PSATO=PSATI
              goto 3
            elseif(ie.eq.3)then
              TO=VAL1(4,ITIMO)
              RHO=VAL1(5,ITIMO)
            elseif(ie.eq.4)then

C Ground coupled get the current month and get ground temperature.
              CALL EDAYR(IDAYO,IDAYN,IMTHN)

C If standard ground profile get it, if 3D ground set to 1.0C
C and if user defined get it.
              if(ic2(icc).gt.0)then
                T0=GRDTMP(IMTHN,ic2(icc))
              elseif(ic2(icc).eq.-3)then
                T0=1.0
              else
                TO=UGRDTP(IMTHN,ie2(icc))
              endif
C              TO=UGRDTP(IMTHN,ISC)
              RHO=VAL3(6,ITIMO)
            endif

C Change other side RH to percentage saturation.
            PSATO=PSATR(TO,RHO,PATMOS)

C Compute saturated vapour pressure and sort.
    3       PSSI=SATVP(TI)
            DO 201 I=1,NNM
              PSS(I)=SATVP(TMP(I))
  201       CONTINUE
            PSSO=SATVP(TO)

C Compute vapour pressure.
            TVR=0.
            DO 301 I=2,NNT
              TVR=TVR+VRN(I)
  301       CONTINUE
            PSI=PSSI*PSATI/100.
            PS(NNM)=PSI
            PSO=PSSO*PSATO/100.
            PS(1)=PSO
            XM=(PSO-PSI)/TVR

            PLAST=PSO
            JJ=1
            DO 401 I=2,NN
              JJ=JJ+1
              PS(I)=PLAST-XM*VRN(JJ)
              PLAST=PS(I)
  401       CONTINUE
            

C Is nodal vapour pressure greater than the saturation
C vapour pressure at any part of the construction.
            ITEST=0
            DO 150 IP=1,NN
              IF(PS(IP).LE.PSS(IP))goto 150
              IF(ITEST.EQ.1)goto 150
              ITEST=1
  150       CONTINUE
            IF(ITEST.EQ.1)then

C Condensation occurs.
              CALL STIME(ITIMO,TDESCR)
              CALL STDATE(IYEAR,IDAYO,DS,DS1)
              write(outs,'(8A)') ' Condensation: ',
     &          SN(1:lnblnk(SN)),' of ',ZN(1:lnblnk(ZN)),
     &          ' on ',DS1,' at ',TDESCR
              call edisp(itru,outs)
            endif
   40     continue
   30   continue
   20 continue
      call edisp(iuout,' ')
      call edisp(iuout,' End of condensation report. ')

      RETURN
      END
