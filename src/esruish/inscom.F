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

C File 'inscom.F' of ish.

C 'INSCOM' sets up the parameters relating to insolation computation.
C 'igeoms' reads appropriate info if a zone transitional shading file
C          is not used.
C  inwshd  reads the zone transitional shading file.
C  INSOLC  controls the insolation computation.
C  INCAI   computes areas and angles.
C  INSORT  sets up mesh points for transparent surfaces.
C  transp  transforms a point on a surface to 3D coordinate space.

C ************** INSCOM
C The main controller for the internal surface insolation calculation:
C        icomp is the zone index;
C        icstat is computation status (1 if data exists);
C        act if 's' is for silent mode, '-' allows interaction.

      SUBROUTINE INSCOM(ICOMP,ICSTAT,act)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL

      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/MTFILE/LTRNS,MULTC,MONS,MONF
      COMMON/SHDFIL/ISHADF
      COMMON/CONTR/MON,ISC(MS),IYD
      common/tmc/itmc(ms),itmcfg,nwins
      COMMON/SIF1/INSRTD,INSTRI
      common/ract/paction

C iaplic(1) toggle for shading;
C iaplic(2) toggle for insolation;
C iaplic(3) toggle for assessment where:
C     1 is 'all_applicable',
C     0 is manual selection of surfaces.
C nsurfcalc number of shaded surfaces, lstsfcalc() list of applicable
C surfaces, nsurfinso numberof insolation sources, isurfinso() list
C of insolation sources.
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),
     &       lstsfcalc(MCOM,MS),nsurfinso(MCOM),isurfinso(MCOM,MS)

      DIMENSION MTHNAM(12),IAVDEC(12),IVA(MS)
      CHARACTER zname*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character paction*16
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,H
      character MTHNAM*3,outs*124,TBASE*72,LTRNS*72,act*1,zdesc*64
      logical OK,DOK,XST,SOK,UDOK
      logical newgeo  ! to use for testing if new/old geometry file.

C Day of average solar declination in each month.
      DATA IAVDEC/17,15,16,15,15,11,17,16,16,16,15,11/
      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

      IUNIT=IFIL
      IUF=IFIL+1
      newgeo=.false.  ! Assume older format geometry.

C Read in the geometry file for the current zone to refresh /G1/.
C Note that if newgeo is true then the file can include values
C for common /ishdirec/.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(act.eq.'s')then
        if(newgeo)then
          call georead(IUF,LGEOM(ICOMP),ICOMP,1,0,IUOUT,IER)
        else
          call egomin(IUF,LGEOM(ICOMP),ICOMP,1,0,IUOUT,IER)
        endif
      else
        if(newgeo)then
          call georead(IUF,LGEOM(ICOMP),ICOMP,1,1,IUOUT,IER)
        else
          call egomin(IUF,LGEOM(ICOMP),ICOMP,1,1,IUOUT,IER)
        endif
      endif
      IF(IER.NE.0)THEN
        call edisp(iuout,'Problem detected while trying to read zone')
        call edisp(iuout,'geometry file - respecify in main menu.')
        return
      endif
      ISHADF=0

C Establish if any of the surfaces are TMCs.
C   - NTMC is the number of applicable source surfaces (external and
C     transparent);
C   - NWINS is the same as ntmc but is held in COMMON;
C   - ITMC is a toggle for each surface, if 1 then surface is a light
C     source.
C   - ITMCFG is a toggle, if 1 then there is at least one light source;
C   - ISC is the same as ITMC but is held in COMMON.
    4 NWINS=0
      ntmc=0

C If new format geometry and iaplic(icomp,2) alread equals 1 then
C all applicable surfaces were set in in the geometry file. Take
C nwins from nsurfinso and set iva from isurfinso. 
      if(newgeo)then
        if(iaplic(icomp,2).eq.1)then
          nwins=nsurfinso(icomp)
          do 41 j=1,nwins
            iva(j)=isurfinso(icomp,j)
  41      continue

C Set up itmc isc arrays and total number of sources in zone.
          DO 42 I=1,NZSUR(icomp)
            II=I
            itmc(ii)=0
            isc(ii)=0 
            DO 43  J=1,nwins
              if(II.EQ.IVA(J))then
                itmc(ii)=1
                isc(ii)=1
              endif
  43        CONTINUE
  42      CONTINUE

C If iaplic(icomp,2) is zero and nsurfinso is non-zero then user
C has manually selected surfaces so fill arrays. If zero then
C the user effectively said no insolation should be calculated
C so just clear itmc() and isc() arrays.
        elseif(iaplic(icomp,2).eq.0)then
          nwins=nsurfinso(icomp)
          if(nwins.eq.0)then
            do 44 I=1,NZSUR(icomp)
              II=I
              itmc(ii)=0
              isc(ii)=0 
  44        continue
          else
            do 45 j=1,nwins
              iva(j)=isurfinso(icomp,j)
  45        continue

C Set up itmc isc arrays and total number of sources in zone.
            DO 46 I=1,NZSUR(icomp)
              II=I
              itmc(ii)=0
              isc(ii)=0 
              DO 47  J=1,nwins
                if(II.EQ.IVA(J))then
                  itmc(ii)=1
                  isc(ii)=1
                endif
  47          CONTINUE
  46        CONTINUE
          endif
        endif

      else

C Older geometry file, user required to set preferences within ish
C or in the case of silent action just do all appliable.
        if(act.eq.'s')then

C Clear return array and select all applicable surfaces.
          do 14 i=1,NZSUR(icomp)
            IVA(I)=0
  14      continue
          DO 13 I=1,NZSUR(icomp)
            ICN=IZSTOCN(icomp,i)
            if(SSOTHER(ICN,1)(1:4).eq.'EXTE'.and.
     &         SSOTF(ICN)(1:4).ne.'OPAQ')then
              ntmc=ntmc+1
              IVA(ntmc)=I
            endif
  13      continue
          if(ntmc.eq.0)then
            CALL usrmsg('No applicable source surfaces found!',' ','W')
            return
          endif
          iaplic(icomp,2)=1
        else
          call edisp(iuout,' ')
          call edisp(iuout,'Select the external transparent surfaces')
          call edisp(iuout,'(i.e. light sources) that you wish to')
          call edisp(iuout,'include in the insolation calculation. If')
          call edisp(iuout,'none, exit the list.')
          CALL PICKSSUR(ICOMP,ntmc,'i',IVA,IER)
          if(ier.ne.0)goto 4
          if(ntmc.eq.0)goto 11
        endif

C Set up itmc isc arrays and total number of sources in zone.
        DO 110 I=1,NZSUR(icomp)
          II=I
          itmc(ii)=0
          isc(ii)=0 
          DO 120 J=1,ntmc
            if(II.EQ.IVA(J))then
              itmc(ii)=1
              isc(ii)=1
              nwins=nwins+1
            endif
  120     CONTINUE
  110   CONTINUE

C Fill ishdirec array.
        nsurfinso(icomp)=nwins
        do 121 j=1,nwins
          isurfinso(icomp,j)=iva(j)
  121   continue
      endif

C Display information on surfaces and check that at least one surface
C has been selected.
   11 ICONT=0
      DO 70 I=1,NZSUR(icomp)
        IF(ISC(I).EQ.1)ICONT=1
   70 CONTINUE
      IF(ICONT.EQ.0)then
        if(newgeo)then
          call edisp(iuout,' ')
          call edisp(iuout,
     &    'No insolation sources noted in geometry file - skipping!')
          RETURN
        else
          if(act.eq.'s')then
            CALL edisp(iuout,'No insolation sources - skipping!')
            RETURN
          else
            dok=.true.
            h(1)='Surfaces that are sources of sunlight must be'
            h(2)='identified.'
            CALL ASKOK('No surfaces specified!',
     &                 'Re-specify?',OK,dok,2)
            IF(OK)goto 4
            call edisp(iuout,'Insolation computation abandoned!')
            RETURN
          endif
        endif
      endif

C Ask for calculation month.
      if(act.eq.'s')then
        MONS=1
        MON=MONS
        MONF=12
        UDOK=.true.
        SOK=.false.
        IIC=2
      else
        h(1)='Choose from the supplied options.'
        CALL EASKABCD(' ','Insolation options:','one month',
     &          'sequence of months','whole year','cancel',IIC,1)
        if(IIC.eq.1)then
          CALL EASKI(MON,' ','Computation month number?',
     &     1,'F',12,'F',1,'ins comp month',IER,0)
        elseif(IIC.eq.2)then
          CALL EASKI(MONS,' ','Start month number?',
     &     1,'F',12,'F',1,'start month',IER,0)
          CALL EASKI(MONF,' ','End month number?',
     &     MONS,'F',12,'F',1,'end month',IER,0)
          MON=MONS
          dok=.false.
          h(1)='As the simulation progresses, the isolation'
          h(2)='distribution to internal zone surfaces is reported.'
          CALL ASKOK(' ','Insolation progress report?',SOK,dok,2)
          UDOK=.true.
        elseif(IIC.eq.3)then

C Set up for all months and then reset icc=2 for subsequent calcs.
          MONS=1
          MONF=12
          MON=MONS
          dok=.false.
          h(1)='As the simulation progresses, the isolation'
          h(2)='distribution to internal zone surfaces is reported.'
          CALL ASKOK(' ','Insolation progress report?',SOK,dok,2)
          UDOK=.true.
          IIC=2
        elseif(IIC.eq.4)then
          return
        endif
      endif

C Convert to day number.
  81  IDAY=IAVDEC(MON)
      CALL EDAY(IDAY,MON,IYD)

C Attempt to open the relevant zone transitional shading file 
C for this month. If unknown call IGEOMS.
      write(LTRNS,'(A)')zname(ICOMP)(1:lnblnk(zname(ICOMP)))
      write(TBASE,'(A,A3)')LTRNS(:LNBLNK(LTRNS)),MTHNAM(MON)
      CALL ERPFREE(IUNIT,ISTAT)
      call FINDFIL(TBASE,XST)

C Allow for number of surfaces within the transitional shading file.
      IRECW=MS+5
      IF(XST)THEN
        call EFOPRAN(IUNIT,TBASE,IRECW,1,IER)

C Since this file exists, there must have been a matching
C obstruction file and therefore the data in common /GS6/
C can be used.
        ISHADF=1
      ELSE
        ISHADF=0
        CALL IGEOMS(IIC)
      ENDIF

C Commence month computation and allow user to see results and
C then update the zone shading & insolation db.
      WRITE(outs,'(A3,A)')MTHNAM(MON),' calculation commenced.'
      if(paction(1:13).ne.'update_silent')then
        call usrmsg(outs,' ','-')
      endif
      CALL INSOLC(icomp)
      ICSTAT=1
      if(IIC.eq.1)then
        dok=.true.
        h(1)='This reports the distrubution of insolation to'
        h(2)='internal surfaces and indicates where window shading'
        h(3)='has reduced this insolation.'
        CALL ASKOK(' ','Insolation report?',OK,dok,3)
        if(OK) CALL INSYNP(ICOMP)
      elseif(IIC.eq.2.and.SOK)then
        CALL INSYNP(ICOMP)
      endif
      if(IIC.eq.1)then
        if(ICSTAT.eq.1)then
          dok=.true.
          h(1)='The calculated insolation data will be lost unless'
          h(2)='it is saved to the Zone Shading & Insolation file.'
          CALL ASKOK(' ',
     &         'Update Zone Shading/Insolation file?',OK,dok,2)
          if(OK)then
            INSTRI=0
            CALL ISAVE
          endif
        endif
        if(MON.lt.12)then
          dok=.true.
          h(1)='You may proceed to another month or terminate the'
          h(2)='insolation calculation.'
          CALL ASKOK(' ','Determine insolation for another month?',
     &      OK,dok,1)
          if(OK)then
            MON=MON+1
            CALL EASKI(MON,' ','Computation month number?',
     &       1,'F',12,'F',1,'ins comp month',IER,0)
            goto 81
          endif
        endif
      elseif(IIC.eq.2.and.UDOK)then
        INSTRI=0
        CALL ISAVE
        MON=MON+1
        if(MON.le.MONF)then
          goto 81
        else
          return
        endif
      endif
      RETURN

      END

C ****************** IGEOMS
C Reads appropriate information if a zone transitional shading file
C is not used.
      SUBROUTINE IGEOMS(IIC)
#include "building.h"

      common/pophelp/h(60)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      common/gs6/nox,noz,ngx,ngz,blkname(mb),BLKMAT(MB)
      common/tmc/itmc(ms),itmcfg,nwins
      character blkname*8,BLKMAT*12,H*72,outs*124

C Set initial grid values.
      ngx=10
      ngz=10
      nox=20
      noz=20
      if(IIC.eq.2)then
        return
      endif

C Prompt user for opaque grid size because not previously set.
      itmcfg=0
      do 9 i=1,nsur
        if(itmc(i).ne.0)itmcfg=1
    9 continue
      if(itmcfg.eq.0)goto 10

C Surface X grid density. 
      H(1)='In order to determine the insolation distribution'
      H(2)='a grid is superimposed on each surface. The grid'
      H(3)='resolution must lie between 4 and 20 cells in the'
      H(4)='the X and Z directions.'
      CALL EASKI(NOX,'Opaque grid:',
     &     'X direction divisions?',
     &     4,'F',20,'F',15,'surf X grid',IER,4)

C Surface Z grid density. 
      CALL EASKI(NOZ,'Opaque grid:',
     &     'Z direction divisions?',
     &     4,'F',20,'F',15,'surf Z grid',IER,4)

   10 RETURN
      END

C **************** INSOLC
C Controls the insolation computation.
C The calculated insolated surfaces and proportions are stored
C in common STINS. 
C insst()=0 - the whole surface is shaded; insst()=-1, the sun is not up,
C            otherwise the index of the surface being insolated.
C pinsst(i,j,k) - proportion insolated from source i at hr j to insolated
C               surf k, for the receiving surface
C pinwst(i,j,k) - proportion insolated from source i at hr j to insolated
C               surf k, for any default windows in receiving surface.
C               N.B. Default windows no longer used.
C Any remaining proportion not accounted for is due to shading on the
C insolation source.

C Variable igins(i,j) holds insolation information for each grid point. 
C It is defined as:
C     1<=k<=N   - hit is on internal opaque or TMC surface k
C     -1>=k>=-N - hit is on window in surface k (not used)
C     -100      - grid point is shaded
C     -200      - grid point is outside polygon of the external surface
C     -300      - grid point is in a window within a TMC (not used)
C        0      - undefined 

      SUBROUTINE INSOLC(icomp)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/SUNHR/ISUNHR
      common/gs6/nox,noz,ngx,ngz,blkname(mb),BLKMAT(MB)
      common/tshd/ioshd(mox,moz)
      COMMON/GRID11/XOP(MOX,MOZ),ZOP(MOX,MOZ)
      COMMON/SHAD1/ISUNUP
      COMMON/CAI/CAII(MS),CAIE(MS)
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/PROJI/XP,YP,ZP
      common/inswin/igins(mox,moz)
      common/pinsol/ins(ms),pins(ms),pinw(ms),pcshad
      common/stins/insst(mgt,24,misur),pinsst(mgt,24,misur),
     &             pinwst(mgt,24,misur),shadst(mgt,24)
      common/ract/paction
     
C ICAL is 0 if no insolation calculations have been done and is set
C to 1 when calculated in this subroutine. Note that ICAL is
C also set to 1 in COMPUTE if no shading surfaces are specified.
      COMMON/DATA2/ICAL
      COMMON/SANGL1/SAZI1,SALT1
      common/tmc/itmc(ms),itmcfg,nwins
      common/icflag/ic
      common/shdinf/noshad
      COMMON/PREC8/SLAT,SLON

      DIMENSION XF(MTV),YF(MTV),ZF(MTV),ydist(mox,moz)
      character blkname*8,BLKMAT*12,outs*124,paction*16
      CHARACTER*3 MTHNAM(12)

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C Set initial values.
      ICAL=0
      iwin=0
      ic=0

      ISUNHR=0
      INDCOM=0
      KIND=0
      KNT=0
      KSU=0

C Compute 24 hourly values of solar azimuth and altitude
C (relative to the original coordinate system).
      DO 620 I=1,24
        STIME=FLOAT(I)
        CALL EAZALTS(STIME,IYD,slat,slon,ISUNUP,SAZI,SALT)
        IF(ISUNUP.eq.1)KSU=KSU+1
  620 CONTINUE

C Set total number of sources in zone.
      DO 610 I=1,NZSUR(icomp)
        if(itmc(i).eq.1) knt=knt+1
  610 CONTINUE
      KT=KNT*KSU

C Loop through surfaces, for each light source perform calculations.
      DO 10 I=1,NZSUR(icomp)
        II=I
        MODES=1

C Flag 'noshad' is set to 1 if there is a zone transitional shading
C file and surface II has no shading information.
        noshad=0
        if(isc(ii).eq.0) goto 10
        NG=itmc(ii)
        IF(NG.LE.0) GOTO 99

C Grid the surfaces using insolation point of view. Note, the resulting
C transformed coordinates in /G1T/ are not written to file but used in
C the subsequent call to MESH.
        CALL TRNSF1I(II)
        CALL MESH(II)

C Calculate hour-by-hour.
        DO 20 J=1,24
          JJ=J
          STIME=FLOAT(JJ)
          CALL EAZALTS(STIME,IYD,slat,slon,ISUNUP,SAZI,SALT)
          SAZI1=SAZI
          SALT1=SALT
          IF(ISUNUP.EQ.1.AND.ISUNHR.EQ.0)ISUNHR=JJ
          CALL INCAI(JJ)

C Analyse each window (TMC) in turn.
          DO 30 K=1,NG
            KK=K
            kkk=iwin+kk
            if(isunup.eq.0)goto 2

C Set up number of mesh points for TMC (assume no default windows).
            ngridx=nox
            ngridz=noz

C Check whether the grid point falls within the surface.
            do 111 mmm=1,ngridx
              do 112 nnn=1,ngridz
                igins(mmm,nnn)=0
                ydist(mmm,nnn)=0.0
  112         continue
  111       continue

C Take zone transitional shading file information into account.
            call inwshd(ii,jj,modes)
            MODES=3

C If CAIE is positive then source surface can be considered.
            IF(CAIE(II).GT.0.0)GOTO 9
            do 200 mmm=1,ngridx
              do 210 nnn=1,ngridz
                if(igins(mmm,nnn).ne.-200.and.igins(mmm,nnn).ne.-300)
     &             igins(mmm,nnn)=-100
  210         continue
  200       continue
            GOTO 29

C Examine each internal surface.
    9       DO 40 L=1,NZSUR(icomp)
              LL=L
              IF(I.EQ.L)GOTO 40

C Exclude surfaces that cannot see each other.
              IF(CAII(LL).LE.0.0)GOTO 40

C Partially derive the transformation equations by computing the
C independent part of the equation coefficients; this is equivalent to
C translation plus three-dimensional rotation. Data placed in
C common /G1T/.
              CALL TRNSF1I(LL)

C Compute new solar angles relative to local face coordinate system.
              CALL TRNSF2I(JJ)

C Set up point-by-point analysis.
              do 50 m=1,ngridx
                mm=m
                do 60 n=1,ngridz
                  nn=n
                  if(ioshd(mm,nn).eq.1)goto 3

C For each undefined point.
                  DO 500 INV=1,NVER(II)
                    XF(INV)=X(JVN(II,INV))
                    YF(INV)=Y(JVN(II,INV))
                    ZF(INV)=Z(JVN(II,INV))
  500             CONTINUE

C Call transp with surface xop (xxw,yyw,zzw is 3d ray origin).
                  call transp(nver(ii),xf,yf,zf,xop(mm,nn),zop(mm,nn),
     &              xxw,yyw,zzw)
                  CALL TRNSF3I(XXW,YYW,ZZW)
                  IF(YP.LT.0.0)GOTO 60

C Call point containment test. IANS is 1 if point is within the
C receiving surface and striking the side opposite the surface normal.
                  CALL POINT1(LL,XP,ZP,IANS)
                  IF(IANS.EQ.1)GOTO 4
                  GOTO 60
    3             IGINS(MM,NN)=-100
                  GOTO 60
    4             continue

C Use crowxyz to check real distance between source point xxw,yyw,zzw
C and the intersection point xp,yp,zp. Use the nearest intersection
C for igins.
                  chkdis=crowxyz(xxw,yyw,zzw,xp,yp,zp)

C Distance is less so update ydist.
                  igins(mm,nn)=LL
                  ydist(mm,nn)=chkdis
   60           CONTINUE
   50         CONTINUE
   40       CONTINUE

C Sort the insolation passing through each insolation source surface.
   29       CALL INSORT(icomp,II,KK,JJ)

C Store for each hour and window.
            do 100 mmm=1,misur
              insst(kkk,jj,mmm)=ins(mmm)
              pinsst(kkk,jj,mmm)=pins(mmm)
              pinwst(kkk,jj,mmm)=pinw(mmm)
  100       continue
            shadst(kkk,jj)=pcshad

            WRITE(outs,'(A,A3,A)')'`',MTHNAM(MON),
     &                            '` insolation calculation commenced!'
            KIND=KIND+1
            CALCOM=FLOAT(KIND)/FLOAT(KT)
            IF(CALCOM.GT.0.95)THEN
              IF(INDCOM.EQ.4)goto 30
              INDCOM=INDCOM+1
              if(paction(1:13).ne.'update_silent')then
                CALL USRMSG(outs,'Almost complete.','P')
              endif
            ELSEIF(CALCOM.GT.0.75)THEN
              IF(INDCOM.EQ.3)goto 30
              INDCOM=INDCOM+1
              if(paction(1:13).ne.'update_silent')then
                CALL USRMSG(outs,'75% complete.','-')
              endif
            ELSEIF(CALCOM.GT.0.50)THEN
              IF(INDCOM.EQ.2)goto 30
              INDCOM=INDCOM+1
              if(paction(1:13).ne.'update_silent')then
                CALL USRMSG(outs,'50% complete.','-')
              endif
            ELSEIF(CALCOM.GT.0.25)THEN
              IF(INDCOM.EQ.1)goto 30
              INDCOM=INDCOM+1
              if(paction(1:13).ne.'update_silent')then
                CALL USRMSG(outs,'25% complete.','-')
              endif
            ENDIF
            goto 30

C Sun not up.
    2       do 101 mmm=1,misur
              insst(kkk,jj,mmm)=-1
              pinsst(kkk,jj,mmm)=0.0
              pinwst(kkk,jj,mmm)=0.0
  101       continue

   30     continue
          if(isunup.eq.1)modes=2
   20   continue
        iwin=iwin+ng
   10 CONTINUE
      ICAL=1
      RETURN

   99 call edisp(iuout,'Error in `INSOLC` window configuration!')
      CALL EPAGEND
      STOP
      END

C *********** inwshd
C Reads the transitional shading file if one exists and extracts
C surface shading information.

      SUBROUTINE INWSHD(ISUR,IHR,MODES)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/tshd/ioshd(mox,moz)
      COMMON/SHDFIL/ISHADF
      common/gs6/nox,noz,ngx,ngz,blkname(mb),BLKMAT(MB)
      COMMON/SUNHR/ISUNHR
      COMMON/RECSHD/IRECX,irecy
      common/shdinf/noshad

      DIMENSION IRECS(MS)
      character blkname*8,BLKMAT*12,outs*124

C Assume no default windows (i.e. only TMCs).
      ngldum=0
      IUSHD=IFIL

      IF(ISHADF.EQ.0.or.noshad.eq.1)GOTO 1
      goto (111,222,333)MODES

C Only transfer here for the first hour every day when sun is up.
  111 IREC=1
      READ(IUSHD,REC=IREC,IOSTAT=ISTAT,ERR=1000)(IRECS(I),I=1,ISUR)
      IRECN=IRECS(ISUR)
      IF(IRECN.EQ.0)then
          noshad=1
          GOTO 1
      endif
      IRECX=IRECN+2+ISUNHR-1+(IHR-(ISUNHR))*2

C Transfer here at all other sun-up hours.
  222 READ(IUSHD,REC=IRECX,IOSTAT=ISTAT,ERR=1000)ISUNUP
      IREC=IRECX+1
      IF(ISUNUP.EQ.0)GOTO 3
      READ(IUSHD,REC=IREC,IOSTAT=ISTAT,ERR=1000)IANS
      IREC=IREC+1
      irecy=irec
      IF(IANS.EQ.-2)GOTO 1
      IF(IANS.EQ.-1)GOTO 2

C Read surface shading information.
      do 41 j=1,nox
         read(iushd,rec=irec,iostat=istat,err=1000)(ioshd(j,k),k=1,noz)
         irec=irec+1
   41 continue
      NG=ngldum
      IF(NG.EQ.0)GOTO 3

  333 continue
 
    3 IRECX=IREC
      GOTO 1111

    1 do 12 i=1,nox
      do 13 j=1,noz
        ioshd(i,j)=0
   13 continue
   12 continue
      goto 3

    2 do 82 i=1,nox
      do 83 j=1,noz
        ioshd(i,j)=1
   83 continue
   82 continue
      goto 3

 1111 RETURN

 1000 WRITE(outs,17)IREC
   17 FORMAT('inwshd: Zone Transitional Shading file error, record',I6)
      call edisp(iuout,outs)
      GOTO 1111
      END

C ************ incai.f
      SUBROUTINE INCAI(IHOUR)
#include "building.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/OUTPCH/ICOUT
      COMMON/CAI/CAII(MS),CAIE(MS)
      COMMON/TC/ITC,ICNT
      COMMON/SANGL1/SAZI1,SALT1

      DIMENSION PAZI(MS),PELV(MS)
      DIMENSION XSUM(MS),YSUM(MS),ZSUM(MS)
      character outs*124
      logical OK

      r=atan(1.0)/45.0
      DO 10 I=1,NSUR
      XS=0.
      YS=0.
      ZS=0.
      NV=NVER(I)

C Gross surface area.
      DO 20 J=1,NV
      K=J+1
      IF(J.EQ.NV)K=1
      IP1=JVN(I,J)
      IP2=JVN(I,K)
      XS=XS+Y(IP1)*Z(IP2)-Z(IP1)*Y(IP2)
      YS=YS+Z(IP1)*X(IP2)-X(IP1)*Z(IP2)
      ZS=ZS+X(IP1)*Y(IP2)-Y(IP1)*X(IP2)
   20 CONTINUE
      XSUM(I)=XS
      YSUM(I)=YS
      ZSUM(I)=ZS
   10 CONTINUE

C Azimuth and elevation angles.
      DO 30 I=1,NSUR
      PAZI(I)=90.0
      if(abs(xsum(i)).lt.0.00001)pazi(i)=0.0
      IF(XSUM(I).LT.0.0)PAZI(I)=-90.0
      PELV(I)=90.0
      if(abs(zsum(i)).lt.0.00001)pelv(i)=0.0
      IF(ZSUM(I).LT.0.0)PELV(I)=-90.0
      if(abs(ysum(i)).lt.0.00001)goto 11
      PAZI(I)=ATAN2(XSUM(I),YSUM(I))
      PAZI(I)=PAZI(I)/R
   11 SRX2Y2=SQRT(XSUM(I)*XSUM(I)+YSUM(I)*YSUM(I))
      IF(PAZI(I).LT.0.0)PAZI(I)=PAZI(I)+360.0
      if(abs(srx2y2).lt.0.00001)goto 30
      PELV(I)=ATAN2(ZSUM(I),SRX2Y2)
      PELV(I)=PELV(I)/R
   30 CONTINUE

C  Cosine of the angle of incidence and surface-solar azimuth angle.
      DO 40 I=1,NSUR
      IF(SALT1.GT.0.0)GOTO 2
      CAII(I)=0.0
      CAIE(I)=0.0
      GOTO 40

    2 PSAZI=ABS(PAZI(I)-SAZI1)

      X1=COS(SALT1*R)
      X2=SIN((90.-PELV(I))*R)
      X3=COS(PSAZI*R)
      X4=SIN(SALT1*R)
      X5=COS((90.-PELV(I))*R)
      CAIE(I)=X1*X2*X3+X4*X5
      PAZ=PAZI(I)+180.0
      IF(PAZI(I).GT.180.0)PAZ=PAZI(I)-180.0
      PSAZI=ABS(PAZ-SAZI1)
      X3=COS(PSAZI*R)
      X1=COS(SALT1*R)
      X2=SIN((90.+PELV(I))*R)
      X3=COS(PSAZI*R)
      X4=SIN(SALT1*R)
      X5=COS((90.+PELV(I))*R)
      CAII(I)=X1*X2*X3+X4*X5
   40 CONTINUE

C  Trace output.
      IF(ITC.EQ.0)GOTO 1111
      OK=.false.
      IF(ITC.eq.2)OK=.true.
      IF(.NOT.OK)goto 1111

      call edisp(icout,' ')
      WRITE(outs,'(A,I3)')'*** INCAI: Hour',IHOUR
      call edisp(icout,outs)
      call edisp(icout,
     &' Surface    Internal    External    Azimuth    Elevation')

      DO 9990 I=1,NSUR
        WRITE(outs,9994)I,CAII(I),CAIE(I),PAZI(I),PELV(I)
 9994   FORMAT(I5,8X,F6.4,6X,F6.4,7X,F5.1,6X,F5.1)
        call edisp(icout,outs)
 9990 CONTINUE
 1111 RETURN
      END

C *********** insort
C Parameters passed are:
C   ii: external surface
C   kk: window
C   jj: hour

      subroutine insort(icomp,ii,kk,jj)
#include "building.h"

      common/inswin/igins(mox,moz)
      common/pinsol/ins(ms),pins(ms),pinw(ms),pcshad
      common/g1/x(mtv),y(mtv),z(mtv),nsur,jvn(ms,mv),nver(ms),ntv
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      common/gs6/nox,noz,ngx,ngz,blkname(mb),BLKMAT(MB)
      common/outin/iuout,iuin
      COMMON/C24/IZSTOCN(MCOM,MS)

      dimension icounts(ms+4),icountw(ms+4),icountt(ms+4)
      character blkname*8,BLKMAT*12,outs*124
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12

C Set up number of mesh points for TMC.
      ngridx=nox
      ngridz=noz

C Initialise.
      ihit=0
      pinrl=0.0
      pintot=0.0
      do 10 i=1,(ms+4)
        icounts(i)=0
        icountw(i)=0
        icountt(i)=0
   10 continue

C Count the number of grid points that project onto each surface.
      do 20 i=1,ngridx
      do 21 j=1,ngridz
      if(igins(i,j).eq.-300)then
         icountt(nsur+4)=icountt(nsur+4)+1
      elseif(igins(i,j).eq.-200)then
         icountt(nsur+3)=icountt(nsur+3)+1
      elseif(igins(i,j).eq.-100)then
         icountt(nsur+1)=icountt(nsur+1)+1
      elseif(igins(i,j).ge.1.and.igins(i,j).le.nsur)then
         icounts(igins(i,j))=icounts(igins(i,j))+1
         icountt(igins(i,j))=icountt(igins(i,j))+1
         ihit=ihit+1
      elseif(igins(i,j).le.-1.and.igins(i,j).ge.(-nsur))then
         icountw(abs(igins(i,j)))=icountw(abs(igins(i,j)))+1
         icountt(abs(igins(i,j)))=icountt(abs(igins(i,j)))+1
         ihit=ihit+1
      else
         icountt(nsur+2)=icountt(nsur+2)+1
      endif
   21 continue
   20 continue

C Initialise insolated surfaces.
      do 30 i=1,nsur
        ins(i)=i
   30 continue

C Bubble sort to give highest count to ins(1) etc.
C Sort based on total hits on surface (i.e. icountt).
      do 40 i=1,nsur-1
      do 41 j=1,nsur-1 
        if(icountt(j).lt.icountt(j+1))then
          itemp=icountt(j)
          icountt(j)=icountt(j+1)
          icountt(j+1)=itemp
          itemp=icounts(j)
          icounts(j)=icounts(j+1)
          icounts(j+1)=itemp
          itemp=icountw(j)
          icountw(j)=icountw(j+1)
          icountw(j+1)=itemp
          itemp=ins(j)
          ins(j)=ins(j+1)
          ins(j+1)=itemp
        endif
   41 continue
   40 continue

C Set insolated planes to zero if there are no hits, i.e. total shading.
      if(ihit.eq.0)then
        do 45 i=1,nsur
          ins(i)=0
          pins(i)=0.0
          pinw(i)=0.0
   45   continue
        pcshad=1.0
        goto 1
      endif   

C Calculate surface insolation proportion of total zone insolation
C (i.e. all surfaces add to 1).
      do 50 i=1,nsur
        pins(i)=real(icounts(i))/(real(ihit))
        pinw(i)=real(icountw(i))/(real(ihit))
   50 continue

C pcshad is the percentage of grid points shaded and is reported
C in the synopsis.
      pcshad=real(icountt(nsur+1))/(real(ihit+icountt(nsur+1)))

C Re-allocate proportions for surfaces greater than the maximum 
C number of insolated surfaces ('misur').
      do 60 i=(misur+1),nsur
        pinrl=pinrl+pins(i)
        pinrl=pinrl+pinw(i)
   60 continue
      if(pinrl.gt.0.0)then
        do 70 i=1,misur
          pins(i)=pins(i)+(pins(i)*pinrl/(1.0-pinrl))
          pinw(i)=pinw(i)+(pinw(i)*pinrl/(1.0-pinrl))
   70   continue
      endif

C Checks.
      do 80 i=1,misur
        pintot=pintot+pins(i)+pinw(i)
   80 continue
      if(pintot.lt.0.99.or.pintot.gt.1.01)then
        ICN=IZSTOCN(icomp,ii)
        write(outs,'(a,f5.3,3a,i2)') 'Warning: solar split is ',
     &     pintot,' in ',SSNAME(icn),' at hour ',jj
        call edisp(iuout,outs)
        goto 1
      endif
          
    1 return
      end

C ********** transp
C This routine transforms a point defined relative to a
C 3-D surface and gives the global X, Y and Z co-ordinates.
C It is used to return the transformed points of the mesh
C coordinates. Note that the routine is called for each grid
C point resulting in the transformation matrices being
C calculated repeatedly (this could be made more efficient by 
C arranging that EYEMAT is called only once).

C     STEP 1          -    Find equation of surface (EQN(4))
C     ROUTINE PLNEQN       and Centre of Gravity (VP(3))

C     STEP 2          -    Set up Eye Point normal to plane
C                          at C. of G. (EP(3))

C     STEP 3          -    Find matrix and reverse matrix that
C     ROUTINE EYEMAT       transform surface to normal view

C     STEP 4          -    Find co-ordinates of origin as transformed
C     ROUTINE ORTTRN       to normal view (origin is first surface vertex)

C     STEP 5          -    Derive displacement of Window/Door co-ordinates
C                          relative to origin

C     STEP 6          -    Apply reverse transformation to co-ordinates
C     ROUTINE ORTTRN       to give vertices in global co-ords.

      SUBROUTINE  TRANSP(N,X,Y,Z,DX,DZ,XXW,YYW,ZZW)
#include "building.h"

      DIMENSION  X(MTV),Y(MTV),Z(MTV)
      DIMENSION  TMAT(4,4),RMAT(4,4)
      DIMENSION  VP(3),EP(3),EQN(4)

C Get center of surface and surface equation.
      call PLEQN(X,Y,Z,N,VP,EQN,IERR)

C Find transformation matrices that normalise face.
      IF (IERR .LT. 0)  GOTO  100
      DO 250 J = 1,3
        EP(J) = VP(J) + EQN(J)
  250 CONTINUE
      CALL  EYEMAT(EP,VP,1.0,TMAT,RMAT)

C Transform first 2 surface vertices.
      CALL  ORTTRN(X(1),Y(1),Z(1),TMAT,X1,Y1,ZZZ,IERR)
      CALL  ORTTRN(X(2),Y(2),Z(2),TMAT,X2,Y2,ZZZ,IERR)
      ALPHA = ATAN2((Y2-Y1),(X2-X1))
      BETA = ATAN2(DZ,DX)
      D = SQRT(DX*DX+DZ*DZ)
      XXW = X1 + D * COS(ALPHA+BETA)
      YYW = Y1 + D * SIN(ALPHA+BETA)

C Take each grid point and apply transformation.
      CALL  ORTTRN(XXW,YYW,ZZZ,RMAT,XX,YY,ZZ,IERR)
      XXW = XX
      YYW = YY
      ZZW = ZZ

  100 CONTINUE

      RETURN
      END
