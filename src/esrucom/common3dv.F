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

C cmmon3dv.F contains the following Fortran subroutines:
C  ADJVIEW: Adjusts the viewing transforms & boundaries if necessary.
C  CHGAZI:  Called from C code with an increment or decrement of azimuth.
C  CHGELEV: Called from C code with an increment or decrement of elevation.
C  DRAWOBS: Checks for obstructions, loads, converts and draws them.
C  DRWSEN:  Loads, converts and draws MRT sensors (rec bodies).
C  EGRNDR:  Draws ground topology.
C  DSGRID:  Draws a dotted site grid with current scaling factors.
C  EMKVIEW: Constructs a 'viewer' format file.

C ************* ADJVIEW 
C ADJVIEW takes the current status of view and boundary mods
C and adjusts the viewing transforms & object boundaries if 
C necessary before displaying the zone.
C ITRC = trace level, IER=0 OK, IER=1 problem. If izgfoc is not 0
C then it represents the zone which is currently being edited.
      SUBROUTINE ADJVIEW(ITRC,IER)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      LOGICAL MODIFY,MODLEN,MODBND

      character ETEXT*82
#ifdef OSX
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

      SMALL=0.01

C If not in graphic mode return.
      if(MMOD.lt.8)return

C Tell wireframe control the current number of selected zones.
C If only drawing a ground topology set updwire manually.
      if(itdsp.eq.6)then
        numberofzones=1
        call updwire(numberofzones)  ! pass local integer
      else
        numberofzones=nzg
        call updwire(numberofzones)  ! pass local integer
      endif

C If altered then do a bound check on the current zone and update display.
      IF(MODIFY)THEN
        if(izgfoc.NE.0)CALL ESCZONE(izgfoc)
        IF(MODBND)CALL BNDOBJ(0,IER)

C If viewpoint or bounds different then initialise viewing paramters.
        IF(MODLEN)THEN
          HANG=ANG/2.0
          DIS = (VIEWM(1)-EYEM(1))**2 + (VIEWM(2)-EYEM(2))**2 +
     &          (VIEWM(3)-EYEM(3))**2
          IF(DIS.GE.SMALL)THEN
            HITH=1.0
            YON=1300.0
          ELSE
            CALL USRMSG(' ',
     &       ' The eye position and viewed position are too close!','W')
            RETURN
          ENDIF
          CALL LENS(IER)
        ENDIF

C Remind user of which model even when looking at one zone.
        WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
        iside=1
        isize=1
        ifont=1
        call viewtext(etext,iside,isize,ifont)

C Draw the selected bodies.
        CALL DRAWESP(ITRC,IER)

C Recover information begin edited and reset modify flag.
        if(izgfoc.NE.0)CALL ERCZONE(izgfoc)
        MODIFY=.FALSE.
        MODLEN=.FALSE.
        MODBND=.FALSE.
      ENDIF

      RETURN
      END

C ********* chgazi
C CHGAZI is called from C code in wwlib.c with an increment or decrement
C to the viewing azimuth.  If not needed then provide a dummy. 
      subroutine chgazi(icazi)
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      logical MODIFY,MODLEN,MODBND
      PI = 4.0 * ATAN(1.0)
      tdis= crow(eyem,viewm)
      call ln2az(VIEWM(1),VIEWM(2),VIEWM(3),eyem(1),eyem(2),eyem(3),
     &  az,el)
      aznew=az + float(icazi)

      RAD = PI/180.
      RYAZI = aznew*RAD
      RSALT = el*RAD
      Z = tdis*SIN(RSALT)
      XYDIS = tdis*COS(RSALT)
      IF (XYDIS .LT. 1E-6)THEN
        X = 0.
        Y = 0.
      ELSE
        X = XYDIS*SIN(RYAZI)
        Y = XYDIS*COS(RYAZI)
      ENDIF
      EYEM(1)=X+VIEWM(1)
      EYEM(2)=Y+VIEWM(2)
      EYEM(3)=Z+VIEWM(3)
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      itrc=0
      CALL ADJVIEW(itrc,IER)

      return
      end

C ********* chgelev
C CHGELEV is called from C code in wwlib.c with an increment or decrement
C to the viewing elevation.  If not needed then provide a dummy. 
      subroutine chgelev(icelev)
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      logical MODIFY,MODLEN,MODBND

      PI = 4.0 * ATAN(1.0)
      tdis= crow(eyem,viewm)
      call ln2az(VIEWM(1),VIEWM(2),VIEWM(3),eyem(1),eyem(2),eyem(3),
     &  az,el)
      elevnew=el + float(icelev)

      RAD = PI/180.
      RYAZI = az*RAD
      RSALT = elevnew*RAD
      Z = tdis*SIN(RSALT)
      XYDIS = tdis*COS(RSALT)
      IF (XYDIS .LT. 1E-6)THEN
        X = 0.
        Y = 0.
      ELSE
        X = XYDIS*SIN(RYAZI)
        Y = XYDIS*COS(RYAZI)
      ENDIF
      EYEM(1)=X+VIEWM(1)
      EYEM(2)=Y+VIEWM(2)
      EYEM(3)=Z+VIEWM(3)
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      itrc=0
      CALL ADJVIEW(itrc,IER)

      return
      end

C ******************** SVDSOPT ********************
C SVDSOPT - save graphics display options.
      subroutine SVDSOPT

      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY2SV/ITDSPsv,ITBNDsv,ITEPTsv,ITZNMsv,ITSNMsv,ITVNOsv,
     &      ITORGsv,ITSNRsv,ITOBSsv,ITHLSsv,ITHLZsv,ITGRDsv,GRDISsv,
     &      ITPPSWsv

      ITDSPsv=ITDSP
      ITBNDsv=ITBND
      ITEPTsv=ITEPT
      ITZNMsv=ITZNM
      ITSNMsv=ITSNM
      ITVNOsv=ITVNO
      ITORGsv=ITORG
      ITSNRsv=ITSNR
      ITOBSsv=ITOBS
      ITHLSsv=ITHLS
      ITHLZsv=ITHLZ
      ITGRDsv=ITGRD
      GRDISsv=GRDIS
      ITPPSWsv=ITPPSW

      return
      end

C ******************** RCDSOPT ********************
C RCDSOPT - recover graphics display options.
      subroutine RCDSOPT

      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY2SV/ITDSPsv,ITBNDsv,ITEPTsv,ITZNMsv,ITSNMsv,ITVNOsv,
     &      ITORGsv,ITSNRsv,ITOBSsv,ITHLSsv,ITHLZsv,ITGRDsv,GRDISsv,
     &      ITPPSWsv

      ITDSP=ITDSPsv
      ITBND=ITBNDsv
      ITEPT=ITEPTsv
      ITZNM=ITZNMsv
      ITSNM=ITSNMsv
      ITVNO=ITVNOsv
      ITORG=ITORGsv
      ITSNR=ITSNRsv
      ITOBS=ITOBSsv
      ITHLS=ITHLSsv
      ITHLZ=ITHLZsv
      ITGRD=ITGRDsv
      GRDIS=GRDISsv
      ITPPSW=ITPPSWsv

      return
      end

C **************DRAWOBS 
C DRAWOBS checks to see it obstructions are associated with a zone.
C If so it loads the geometric description within an obstruction file
C into common block BG1 and then converts into viewing coords, 
C clips the image and draws it.
      SUBROUTINE DRAWOBS(IFOC,ier)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

      DIMENSION  AX(MV),AY(MV),AZ(MV),IANXT(MV)
      DIMENSION  BX(MV),BY(MV),BZ(MV),IBNXT(MV)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3),COG3(3)

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER btemp*8,BLKNAME*8,BLKMAT*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      logical newgeo  ! to use for testing if new/old geometry file.

C If not in graphic mode return.
      if(MMOD.lt.8)return

C If newer geometry file it may include obstructions. If the value
C of iobs for this zone is 2 then obstructions were included.
      newgeo=.false.  ! assume older format geometry.
      call eclose(gversion(ifoc),1.1,0.01,newgeo)

C If no obstruction for this zone don't bother reading file.
C If iobs() is 1 then scan zone obstructions file. If iob()
C is 2 then re-read the version 1.1 zone geometry file.
      IUF=IFIL+1
      IF(IOBS(ifoc).EQ.0)THEN
        RETURN
      ELSEIF(IOBS(ifoc).EQ.1)THEN
        CALL EGOMST(IUF,ZOBS(ifoc),0,0,iuout,IER)
        IF(IER.NE.0)RETURN
      ELSEIF(IOBS(ifoc).EQ.2)THEN
        if(newgeo)then
          call georead(IUF,LGEOM(ifoc),ifoc,1,ITRC,iuout,ier)
          IF(IER.NE.0)RETURN
        endif
      ENDIF

      DO 301 IB=1,NB
        CALL CNVBLK(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),BANG(IB))

C If block names are to be displayed place near first vertex.
C Transform the location into eyepoint and then screen coords.
        IF(ITSNM.EQ.0)THEN
          COG1(1)=XB(1)
          COG1(2)=YB(1)
          COG1(3)=ZB(1)
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),ix,iy)
          btemp='        '
          write(btemp,'(a)') BLKNAME(IB)
          CALL wstxpt(ix,iy,btemp)
        ENDIF

C If editing an obstruction, highlight it.
        if(ITOBS.eq.IB)then
          IPEN= -305
        else
          IPEN= 1
        endif

C Continue on with surfaces in the block.
C Copy polygon for obstruction surface I into structure AX AY AZ
C where IANXT = Index of 'next' point. Clear IANXT & IBNXT arrays.
        do 1101 J = 1,MV
          IANXT(J) = 0
          IBNXT(J) = 0
 1101   continue

C There are 4 initial vertices per block side and 6 sides.
        NAP=4
        DO 1000 I=1,6
          DO 1100 J = 1,NAP
            K = JVNB(I,J)
            AX(J) = XB(K)
            AY(J) = YB(K)
            AZ(J) = ZB(K)
            IANXT(J) = J + 1
 1100     CONTINUE
          IANXT(4) = 1

C Transform block polygon to EYE co-ordinates where
C TEMAT = Model to Eye Matrix
          CALL MATPOLS(NAP,AX,AY,AZ,IANXT,TEMAT,SBBOX,
     &                 NBP,BX,BY,BZ,IBNXT,IERR)

C Generate clipping flags and clip geometry in eye coords.
C If ISTAT =  0 : totally inside frustrum
C If ISTAT =  1 : totally outside frustrum
C If ISTAT = -1 : straddles frustrum
          call CLIPSUR(NBP,BX,BY,BZ,ISTAT)
          IF (ISTAT .EQ. 1) THEN
            GOTO  1000
          ELSEIF (ISTAT .EQ. -1) THEN
            IBPNT=1
            CALL CUTSUR(NB1,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
          ENDIF
            
C If there are no edges to draw then jump.
          if(NB1.eq.0)goto 1000

C Multiply each vertex by ETSMAT (Eye to Screen Matrix) to transform
C to screen coords. First move to the beginning point (as passed from CUTSUR).
          IP = IABS(IBPNT)
          CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          ixlast=ix
          iylast=iy

C If IP1 is ever negative then jump to a later point.
          IP1 = IBNXT(IP)
          if(IP1.lt.1)IP1 = IBNXT(IP+1)
          if(IP1.lt.1)IP1 = IBNXT(IP+2)

 351      CONTINUE
          CALL ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          if(IPEN.eq.1)then
            call eswline(ixlast,iylast,ix,iy)
          elseif(IPEN.eq.-305)then
            call edwline(ixlast,iylast,ix,iy)
          else
            call eswline(ixlast,iylast,ix,iy)
          endif
          call forceflush()

C Repeat until next point is the first, remember position and skip
C past -1 values of IBNXT, save IP1 as IPT for testing.
          IPT = IP1
          IP1 = IBNXT(IP1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+2)
          ixlast=ix
          iylast=iy
          IF(IP1 .NE. IP) GOTO 351

C Complete the polygon.
          CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          if(IPEN.eq.1)then
            call eswline(ixlast,iylast,ix,iy)
          elseif(IPEN.eq.-305)then
            call edwline(ixlast,iylast,ix,iy)
          else
            call eswline(ixlast,iylast,ix,iy)
          endif
          call forceflush()
 1000   CONTINUE
  301 CONTINUE

      RETURN
      END

C **************** DRWSEN
C DRWSEN loads the geometric description within an MRT sensor file
C into common block BG1 and then converts into viewing coords,
C clips the image and draws it.
      SUBROUTINE DRWSEN(ier)
#include "building.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)

      DIMENSION  AX(MV),AY(MV),AZ(MV),IANXT(MV)
      DIMENSION  BX(MV),BY(MV),BZ(MV),IBNXT(MV)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3),COG3(3)

      CHARACTER temp*7,CUBN*6

C If not in graphic mode return.
      if(MMOD.lt.8)return

      if(NCUB.eq.0)return
      DO 301 IB=1,NCUB
        CALL CNVBLK(XOC(IB),YOC(IB),ZOC(IB),DXC(IB),DYC(IB),DZC(IB),
     &    CANG(IB))

C Place sensor names near first vertex.
C Transform the location into eyepoint and then screen coords.
        COG1(1)=XB(1)
        COG1(2)=YB(1)
        COG1(3)=ZB(1)
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),ix,iy)
        write(temp,'(A)')CUBN(IB)
        CALL wstxpt(ix,iy+8,temp)

C Continue on with surfaces in the sensor.
C Copy polygon for sensor surface I into structure AX AY AZ
C where IANXT = Index of 'next' point. Clear IANXT & IBNXT arrays.
        do 1101 J = 1,MV
          IANXT(J) = 0
          IBNXT(J) = 0
 1101   continue

C There are 4 initial vertices per block side and 6 sides.
        NAP=4
        DO 1000 I=1,6
          DO 1100 J = 1,NAP
            K = JVNB(I,J)
            AX(J) = XB(K)
            AY(J) = YB(K)
            AZ(J) = ZB(K)
            IANXT(J) = J + 1
 1100     CONTINUE
          IANXT(4) = 1

C Transform block polygon to EYE co-ordinates.
          CALL MATPOLS(NAP,AX,AY,AZ,IANXT,TEMAT,SBBOX,
     &                 NBP,BX,BY,BZ,IBNXT,IERR)

C Generate clipping flags and clip geometry in eye coords.
C If ISTAT =  0 : totally inside frustrum
C If ISTAT =  1 : totally outside frustrum
C If ISTAT = -1 : straddles frustrum
          call CLIPSUR(NBP,BX,BY,BZ,ISTAT)
          IF (ISTAT .EQ. 1) THEN
            GOTO  1000
          ELSEIF (ISTAT .EQ. -1) THEN
            IBPNT=1
            CALL CUTSUR(NB1,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
          ENDIF

C If there are no edges to draw then jump.
          if(NB1.eq.0)goto 1000

C Multiply each vertex by ETSMAT (Eye to Screen Matrix).
C First move to the beginning (as passed back from CUTSUR).
          IP = IABS(IBPNT)
          CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          ixlast=ix
          iylast=iy

C If IP1 is ever negative then jump to a later point.
          IP1 = IBNXT(IP)
          if(IP1.lt.1)IP1 = IBNXT(IP+1)
          if(IP1.lt.1)IP1 = IBNXT(IP+2)

 351      CONTINUE
          CALL ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          call eswline(ixlast,iylast,ix,iy)
          call forceflush()

C Repeat until next point is the first, remember position and skip
C past any -1 values of IBNXT, save IP1 as IPT for testing.
          IPT = IP1
          IP1 = IBNXT(IP1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+2)
          ixlast=ix
          iylast=iy
          IF(IP1 .NE. IP) GOTO 351

C Complete the polygon.
          CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          call eswline(ixlast,iylast,ix,iy)
          call forceflush()
 1000   CONTINUE
  301 CONTINUE

      RETURN
      END

C ****** Routine to draw ground topology.
      SUBROUTINE EGRNDR(IER)
#include "building.h"
      PARAMETER  (MPL = 200 )

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY6G/LINSTYG(MGRT)

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3),COG3(3)

      CHARACTER GMLCN*12, GSNAME*6,stemp*7

C Save the current font and switch to a smaller font for displaying
C surface names etc. Ixc,iyc keep track of surface/pixel COG.
      if(MMOD.lt.8)return
      NAMEFT=IFS-1
      LD=3
      if(ngtv.eq.0)return
      IF(ITVNO.EQ.0)then
        do 42 iv = 1,NGTV
          COG1(1)=XGT(IV)
          COG1(2)=YGT(IV)
          COG1(3)=ZGT(IV)
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),ix,iy)
          CALL ecirc(ix,iy,2,1)
          CALL VERTLBL(ix,iy,IV,ier)
  42    continue
      endif
      if(ngt.eq.0)return
      do 43 igs = 1, NGT
        ixc=0
        iyc=0

C Copy polygon for Surface I into structure A for compatibility with
C viewer format 'holes' in surfaces.
C NAP   = Number of vertex points on surface
C IANXT = Index of 'next' point
C IAPNT = Pointer to first vertex of polygon
        IAPNT = 1
        NAP = NGVER(igs)
        DO 1100 J = 1,NGVER(igs)
          K = JGVN(igs,J)
          AX(J) = XGT(K)
          AY(J) = YGT(K)
          AZ(J) = ZGT(K)
          IANXT(J) = J + 1
 1100   CONTINUE
        IANXT(NGVER(igs)) = IAPNT

C-----------------------------------------------------------------------
C Transform surface polygon as above.
        CALL MATPOL(NAP,IAPNT,AX,AY,AZ,IANXT,TEMAT,
     &             SBBOX,NBP,IBPNT,BX,BY,BZ,IBNXT, IERR)
        CALL CLIPFL(NBP,BX,BY,BZ,ISTAT)
        IF (ISTAT .EQ. 1) THEN
          GOTO  43
        ELSEIF (ISTAT .EQ.-1) THEN
          CALL  CUTPOL(NB,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
        ENDIF

        DO 302 JB = 1,NB

C Move to first point.
          IP = IABS(IBPNT)
          CALL  ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          ixlast=ix
          iylast=iy
          ixc=ixc+ix
          iyc=iyc+iy

C Draw succeeding points until last vertex.
          IP1 = IBNXT(IP)
 451      CONTINUE
          CALL  ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          if(linstyg(igs).eq.3)then
            call edline(ixlast,iylast,ix,iy,2)
          elseif(linstyg(igs).eq.2)then
            call edwline(ixlast,iylast,ix,iy)
          elseif(linstyg(igs).eq.1)then
            call eswline(ixlast,iylast,ix,iy)
          else
            call edline(ixlast,iylast,ix,iy,LD)
          endif
          ixc=ixc+ix
          iyc=iyc+iy

          IP1 = IBNXT(IP1)
          ixlast=ix
          iylast=iy
          IF(IP1 .NE. IP) GOTO 451

C Complete ground surface.
          CALL  ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          if(linstyg(igs).eq.3)then
            call edline(ixlast,iylast,ix,iy,2)
          elseif(linstyg(igs).eq.2)then
            call edwline(ixlast,iylast,ix,iy)
          elseif(linstyg(igs).eq.1)then
            call eswline(ixlast,iylast,ix,iy)
          else
            call edline(ixlast,iylast,ix,iy,LD)
          endif
          call forceflush()

C If ground surface names are to be displayed place near the centre of polygon.
C If the surface is not drawn (ixc=0 & iyc=0) do not label it.  For clarity drop to
C the next smaller font for the surface name and then restore font
C to normal size.
          IF(ITSNM.EQ.0)THEN
            IF(ixc.GT.0.AND.iyc.GT.0.AND.(NAP.EQ.NBP))THEN
              IF(IFS.GT.0)CALL winfnt(NAMEFT)
              write(stemp,'(A)')GSNAME(igs)
              ixc=ixc/NBP
              iyc=iyc/NBP
              CALL wstxpt(ixc,iyc+(3*(NAMEFT+1)),stemp)
              IF(IFS.GT.0)CALL winfnt(IFS)
            endif
          endif
  302   CONTINUE
  43  continue
      call forceflush()

      return
      end

C-----DSGRID -----------------------------------------------------------
C DSGRID draws a dotted grid at a given reference height RH, grid
C density GD, and dot density LD within
C the viewport with the current scaling factors etc.
      SUBROUTINE DSGRID(RH,GD,LD,IER)
#include "building.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      DIMENSION COG1(3),COG2(3),COG3(3)
      logical close,greyok

C If not in graphic mode return.
      if(MMOD.lt.8)return

      IER=0

C If libraries and monitor support greyscale then reset forground
C to 50% grey when drawing dots.
      greyok=.false.
      if(nifgrey.gt.4)then
        call winscl('i',nifgrey-3)
        greyok=.true.
      endif

      GRIDD=GD

C If GRIDD is zero compute default grid spacing.
C Use diagonal distance across the site.
      CALL ECLOSE(GRIDD,0.0,0.01,close)
      IF(close)THEN
        XD=(XMX-XMN)**2 + (YMX-YMN)**2
        DD=SQRT(XD)
        IF((DD.LE.5.))THEN
          GRIDD=0.5
        ELSEIF((DD.GT.5.).AND.(DD.LE.50.))THEN
          GRIDD=2.0
        ELSEIF((DD.GT.50.).AND.(DD.LE.100.))THEN
          GRIDD=5.0
        ELSEIF((DD.GT.100.).AND.(DD.LE.200.))THEN
          GRIDD=10.0
        ELSEIF((DD.GT.200.).AND.(DD.LE.500.))THEN
          GRIDD=25.0
        ELSEIF(DD.GT.500.)THEN
          GRIDD=50.0
        ENDIF
      ENDIF

C Make sure that the gridding will pass over the 0.0 point.
      GXMN=XMN
      IF(XMN-AINT(XMN).LT.GRIDD)GXMN=AINT(XMN)-1.

C Loop up in one direction.
      XINC=GXMN
  100 CONTINUE
      IF(XINC.LT.XMX+GRIDD)THEN

C Transform one end of line.
        COG1(1)=XINC
        COG1(2)=YMN-GRIDD
        COG1(3)=RH
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),ix,iy)

C Transform other end of line.
        COG1(1)=XINC
        COG1(2)=YMX+GRIDD
        COG1(3)=RH
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),ix1,iy1)

C Draw the dotted line.
        call edline(ix,iy,ix1,iy1,LD)

C Move to next grid position.
        XINC=XINC+GRIDD
        GOTO 100
      ENDIF
      call forceflush()

C Loop up in other direction.
C Make sure that the gridding will pass over the 0.0 point.
      GYMN=YMN
      IF(YMN-AINT(YMN).LT.GRIDD)GYMN=AINT(YMN)-1.
      YINC=GYMN
  110 CONTINUE
      IF(YINC.LT.YMX+GRIDD)THEN

C Transform one end of line.
        COG1(1)=XMN-GRIDD
        COG1(2)=YINC
        COG1(3)=RH
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),ix,iy)

C Transform other end of line.
        COG1(1)=XMX+GRIDD
        COG1(2)=YINC
        COG1(3)=RH
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),ix1,iy1)

C Draw the dotted line.
        call edline(ix,iy,ix1,iy1,LD)

C Move to next grid position.
        YINC=YINC+GRIDD
        GOTO 110
      ENDIF
      if(greyok)call winscl('-',0)
      call forceflush()

      RETURN
      END

C ******************* EMKVIEW 
C EMKVIEW constructs a 'viewer' format file. The trace level defines 
C whether information about the zones is displayed or not.
      SUBROUTINE EMKVIEW(ITRC,IUO,CFGOK,IER)

#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
C      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      LOGICAL  CFGOK,OK,dok
      logical newgeo  ! to use for testing if new/old geometry file.

C Use JGVNT for ground surfaces and JVNZ to hold inverted order for zone.
      DIMENSION IVALS(MCOM),JGVNT(MGRT,8),JVNZ(MS,MV)
      CHARACTER H*72
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER zname*12,zdesc*64,GMLCN*12,GSNAME*6,outs*124

      IER=0
      IUF=IFIL+1
      newgeo=.false.  ! assume older format geometry.

C Write to previously opened viewer file.

C Present a list of zone names to pick from if the configuration file
C has been read in, otherwise ask for zones one by one.
  106 IF(CFGOK)THEN
        H(1)=' Pick one, several or all zones for inclusion'
        INPIC=NCOMP
        CALL EPICKS(INPIC,IVALS,' ',' Which zones to include: ',
     &    12,NCOMP,zname,' zone list',IER,1)
      ELSE
        H(1)='More than one zone can be selected.'
        H(2)='A cancel request leaves the selection list unchanged.'
        CALL EASKI(INPIC,' ',' Number of zone to include ? ',
     &       1,'W',MCOM,'W',1,'no of zones',IERI,2)
        if(ieri.eq.-3) goto 105
        IF(INPIC.EQ.0)GOTO 105

        DO 79 IZZ=1,INPIC

C Pass ICOMP in as a positive so that 'ALL' is included in list.
          ICOMP=1
          CALL EASKGEOF('Pick zones to include in view',CFGOK,ICOMP,
     &      '-',IER)
          IVALS(IZZ)=ICOMP
   79   CONTINUE
      ENDIF

      IF(INPIC.EQ.0)GOTO 105
      DO 83 IZ=1,INPIC
        IF(IVALS(IZ).GT.0)THEN
          call eclose(gversion(IVALS(IZ)),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IUF,LGEOM(IVALS(IZ)),IVALS(IZ),1,0,iuout,IER)
          else
            call egomin(IUF,LGEOM(IVALS(IZ)),IVALS(IZ),1,0,iuout,IER)
          endif
          IF(IER.NE.0)THEN
            dok=.false.
            h(1)='While scanning the model`s zone geometry a problem'
            h(2)='was encountered. Perhaps a recent change corrupted'
            h(3)='the model? '
            CALL ASKOK(' ',
     &        'Problem while scanning geometry...try again?',OK,dok,3)
            if(ok)goto 106
            goto 83
          ENDIF

C Backup JVN to JVNZ
          do 121 J=1,nsur
            K=NVER(J)
            do 122 L=1,K
              JVNZ(J,L)=JVN(J,L)
  122       continue
  121     continue

C Write surface information to the viewer file. First invert edges.
          DO 120 J=1,NSUR
            K=NVER(J)
            DO 130 L=1,K/2
              ITEMP=JVNZ(J,L)
              JVNZ(J,L)=JVNZ(J,K+1-L)
              JVNZ(J,K+1-L)=ITEMP
  130       CONTINUE
  120     CONTINUE

C Write to output file.
          WRITE(IUO,18)zname(IVALS(IZ))
   18     FORMAT('GEN',1X,A12)
          WRITE(IUO,'(2I7)')NTV,NSUR
          DO 40 J=1,NTV
            WRITE(IUO,'(3F8.3)')X(J),Y(J),Z(J)
   40     CONTINUE
          DO 50 J=1,NSUR
            WRITE(IUO,'(2X,I3,1X,21I3)')NVER(J),(JVNZ(J,K),K=1,NVER(J))
   50     CONTINUE
        ENDIF

C If there is an obstructions file for this zone check and see if this
C should be included in image.
        if(IOBS(IZ).EQ.1)then
          write(outs,'(a,a)') ' Using: ',ZOBS(IZ)
          CALL USRMSG(' ',outs,'-')
          CALL EGOMST(IUF,ZOBS(IZ),0,0,iuout,IER)
          IF(IER.NE.0)RETURN
        elseif(IOBS(IZ).EQ.2)THEN
          continue  ! any obstructions are in the geometry file.
        endif
        if(NB.gt.0)then
          DO 301 IB=1,NB
C << conversion to using cnvblk and common GB1 to be done >>
C            CALL CNVBLK(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),
C     &        BANG(IB))
            CALL ERECC(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),
     &                 BANG(IB))

C Write obstruction information to the viewer file. First invert edges.
            DO 320 J=1,NSUR
              K=NVER(J)
              DO 330 L=1,K/2
                ITEMP=JVN(J,L)
                JVN(J,L)=JVN(J,K+1-L)
                JVN(J,K+1-L)=ITEMP
  330         CONTINUE
  320       CONTINUE

C Write obstructions to output file.
            WRITE(IUO,218)zname(IVALS(IZ))
  218       FORMAT('GEN',1X,A12,' obstructions')
            WRITE(IUO,'(2I7)')NTV,NSUR
            DO 240 J=1,NTV
              WRITE(IUO,'(3F8.3)')X(J),Y(J),Z(J)
  240       CONTINUE
            DO 250 J=1,NSUR
              WRITE(IUO,'(2X,I3,1X,21I3)')NVER(J),(JVN(J,K),K=1,NVER(J))
  250       CONTINUE
  301     CONTINUE
        ENDIF
   83 CONTINUE

C If there is a ground description ask if to be included and then
C add at the bottom of the file.
      if(NGTV.gt.0)then
        dok=.true.
        h(1)='The model includes a descrition of the surface of the'
        h(2)='ground at the site. If you say yes it will be included'
        h(3)='in the view. '
        CALL ASKOK(' ','Include the ground topography?',OK,dok,3)
        if(OK)then

C Write surface information to the viewer file. First invert edges.
C Use temporary array for this.
          DO 142 J=1,NGT
            DO 143 L=1,NGVER(J)
              JGVNT(J,L)=JGVN(J,L)
 143        continue
 142      continue
          DO 144 J=1,NGT
            K=NGVER(J)
            DO 145 L=1,K/2
              ITEMP=JGVNT(J,L)
              JGVNT(J,L)=JGVNT(J,K+1-L)
              JGVNT(J,K+1-L)=ITEMP
  145       CONTINUE
  144     CONTINUE

C Write to output file.
          do 242,JG=1,NGT
            WRITE(IUO,'(a,1x,a)')'PLA',GSNAME(JG)
            WRITE(IUO,'(I7)')NGVER(JG)
            DO 243 J=1,NGVER(JG)
              WRITE(IUO,'(3F8.3)')XGT(JGVNT(JG,J)),YGT(JGVNT(JG,J)),
     &          ZGT(JGVNT(JG,J))
  243       continue
  242     continue
        ENDIF

        endif

C Free the viewer file.
  105 CONTINUE
      CALL ERPFREE(IUO,ISTAT)

      RETURN
      END
