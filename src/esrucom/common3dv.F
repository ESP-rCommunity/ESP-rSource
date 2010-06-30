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
C IER=0 OK, IER=1 problem. If izgfoc is not 0
C then it represents the zone which is currently being edited.
      SUBROUTINE ADJVIEW(IER)
#include "building.h"
#include "model.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

C Passed parameters.
      integer ier
      
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      LOGICAL MODIFY,MODLEN,MODBND

      character ETEXT*82
#ifdef OSI
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
        WRITE(ETEXT,'(2A)')'Project: ',modeltitle(1:lnblnk(modeltitle))
        iside=1
        isize=1
        ifont=1
        call viewtext(etext,iside,isize,ifont)

C Draw the selected bodies.
        CALL DRAWESP(IER)

C Recover information begin edited and reset modify flag.
        if(izgfoc.NE.0)CALL ERCZONE(izgfoc)
        MODIFY=.FALSE.
        MODLEN=.FALSE.
        MODBND=.FALSE.
      ENDIF

      RETURN
      END

C ********* chgazi
C CHGAZI is called from C code in esp-r.c or esru_x.c with an increment or 
C decrement to the viewing azimuth (icazi). If not needed then provide a dummy. 
      subroutine chgazi(icazi)
#include "prj3dv.h"

C Passed paremter.
      integer icazi
      
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
      CALL ADJVIEW(IER)

      return
      end

C ********* chgelev
C CHGELEV is called from C code in esp-r.c or esru_x.c with an increment or 
C decrement to the viewing elevation (icelev). If not needed then provide a dummy. 
      subroutine chgelev(icelev)
#include "prj3dv.h"

C Passed paramter.
      integer icelev
      
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
      CALL ADJVIEW(IER)

      return
      end

C ******************** SVDSOPT ********************
C SVDSOPT - save graphics display options into common block RAY2SV.
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
C RCDSOPT - recover graphics display options from common block RAY2SV.
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
C DRAWOBS checks to see it obstructions are associated with zone (IFOC).
C If so it loads the geometric description within an obstruction file
C into common block BG1 and then converts into viewing coords, 
C clips the image and draws it.
C IER is returned as non-zero if there is a problem.
      SUBROUTINE DRAWOBS(IFOC,ier)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0, G2, GS5, GS6.
#include "geometry.h"
#include "prj3dv.h"

C Passed parameters.
      integer ifoc,ier
      
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW

C Local arrays.
      real AX,AY,AZ,BX,BY,BZ,SBBOX,COG1,COG2,COG3
      integer IANXT,IBNXT
      DIMENSION  AX(MV),AY(MV),AZ(MV),IANXT(MV)
      DIMENSION  BX(MV),BY(MV),BZ(MV),IBNXT(MV)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3),COG3(3)

      CHARACTER btemp*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      logical newgeo  ! to use for testing if new/old geometry file.
#ifdef OSI
      integer iix,iiy,iixlast,iiylast
#else
      integer*8 iix,iiy,iixlast,iiylast
#endif     

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
        CALL EGOMST(IUF,ifoc,ZOBS(ifoc),0,0,iuout,IER)
        IF(IER.NE.0)RETURN
      ELSEIF(IOBS(ifoc).EQ.2)THEN
        if(newgeo)then
          call georead(IUF,LGEOM(ifoc),ifoc,1,iuout,ier)
          IF(IER.NE.0)RETURN
        endif
      ENDIF

      DO 301 IB=1,nbobs(ifoc)
        if(BLOCKTYP(ifoc,IB)(1:4).eq.'obs ')then
          CALL CNVBLK(XOB(ifoc,IB),YOB(ifoc,IB),ZOB(ifoc,IB),
     &      DXOB(ifoc,IB),DYOB(ifoc,IB),DZOB(ifoc,IB),
     &      BANGOB(ifoc,IB,1))
        elseif(BLOCKTYP(ifoc,IB)(1:4).eq.'obs3')then
          CALL CNVBLK3A(XOB(ifoc,IB),YOB(ifoc,IB),ZOB(ifoc,IB),
     &      DXOB(ifoc,IB),DYOB(ifoc,IB),DZOB(ifoc,IB),
     &      BANGOB(ifoc,IB,1),BANGOB(ifoc,IB,2),BANGOB(ifoc,IB,3))
        elseif(BLOCKTYP(ifoc,IB)(1:4).eq.'obsp')then
          call CNVBLKP(ifoc,IB) ! convert obsp type.
        endif

C If block names are to be displayed place near first vertex.
C Transform the location into eyepoint and then screen coords.
        IF(ITSNM.EQ.0)THEN
          COG1(1)=XB(1)
          COG1(2)=YB(1)
          COG1(3)=ZB(1)
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),iix,iiy)
          btemp='        '
          write(btemp,'(a)') BLOCKNAME(IFOC,IB)
          CALL wstxpt(iix,iiy,btemp)
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
          call u2pixel(AAX,AAY,iix,iiy)
          iixlast=iix   ! cast for edline call
          iiylast=iiy

C If IP1 is ever negative then jump to a later point.
          IP1 = IBNXT(IP)
          if(IP1.lt.1)IP1 = IBNXT(IP+1)
          if(IP1.lt.1)IP1 = IBNXT(IP+2)

 351      CONTINUE
          CALL ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,iix,iiy)
          if(IPEN.eq.1)then
            call eswline(iixlast,iiylast,iix,iiy)
          elseif(IPEN.eq.-305)then
            call edwline(iixlast,iiylast,iix,iiy)
          else
            call eswline(iixlast,iiylast,iix,iiy)
          endif
          call forceflush()

C Repeat until next point is the first, remember position and skip
C past -1 values of IBNXT, save IP1 as IPT for testing.
          IPT = IP1
          IP1 = IBNXT(IP1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+2)
          iixlast=iix   ! cast for edline call
          iiylast=iiy
          IF(IP1 .NE. IP) GOTO 351

C Complete the polygon.
          CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,iix,iiy)
          if(IPEN.eq.1)then
            call eswline(iixlast,iiylast,iix,iiy)
          elseif(IPEN.eq.-305)then
            call edwline(iixlast,iiylast,iix,iiy)
          else
            call eswline(iixlast,iiylast,iix,iiy)
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
C IER is returned as non-zero if there is a problem.
      SUBROUTINE DRWSEN(ier)
#include "building.h"
#include "prj3dv.h"

C Passed parameter.
      integer ier
      
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)

C Local arrays.
      real AX,AY,AZ,BX,BY,BZ,SBBOX,COG1,COG2,COG3
      integer IANXT,IBNXT
      DIMENSION  AX(MV),AY(MV),AZ(MV),IANXT(MV)
      DIMENSION  BX(MV),BY(MV),BZ(MV),IBNXT(MV)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3),COG3(3)
#ifdef OSI
      integer iix,iiy,iixlast,iiylast,iid2
#else
      integer*8 iix,iiy,iixlast,iiylast,iid2
#endif     

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
        call u2pixel(COG3(1),COG3(2),iix,iiy)
        write(temp,'(A)')CUBN(IB)
        iid2=iiy+8
        CALL wstxpt(iix,iid2,temp)

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
          call u2pixel(AAX,AAY,iix,iiy)
          iixlast=iix   ! cast for edline call
          iiylast=iiy

C If IP1 is ever negative then jump to a later point.
          IP1 = IBNXT(IP)
          if(IP1.lt.1)IP1 = IBNXT(IP+1)
          if(IP1.lt.1)IP1 = IBNXT(IP+2)

 351      CONTINUE
          CALL ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,iix,iiy)
          call eswline(iixlast,iiylast,iix,iiy)
          call forceflush()

C Repeat until next point is the first, remember position and skip
C past any -1 values of IBNXT, save IP1 as IPT for testing.
          IPT = IP1
          IP1 = IBNXT(IP1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+2)
          iixlast=iix   ! cast for edline call
          iiylast=iiy
          IF(IP1 .NE. IP) GOTO 351

C Complete the polygon.
          CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,iix,iiy)
          call eswline(iixlast,iiylast,iix,iiy)
          call forceflush()
 1000   CONTINUE
  301 CONTINUE

      RETURN
      END

C ****** Routine to draw ground topology.
C EGRNDR  Draws ground topology held in GT5 and grndpl commons.
C IER is returned as non-zero if there has been a problem.
      SUBROUTINE EGRNDR(IER)
#include "building.h"
#include "prj3dv.h"

C Passed parameter.
      integer ier
      
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY6G/LINSTYG(MGRT)

C Local arrays.
      real AX,AY,AZ,BX,BY,BZ,SBBOX,COG1,COG2,COG3
      integer IANXT,IBNXT
      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3),COG3(3)

      CHARACTER GMLCN*12, GSNAME*6,stemp*7

C Local variables to pass to edline.
#ifdef OSI
      integer iix,iiy,iixlast,iiylast,iixc,iiyc,iid2
#else
      integer*8 iix,iiy,iixlast,iiylast,iixc,iiyc,iid2
#endif     

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
          call u2pixel(COG3(1),COG3(2),iix,iiy)
          CALL ecirc(iix,iiy,2,1)
          CALL VERTLBL(iix,iiy,IV,ier)
  42    continue
      endif
      if(ngt.eq.0)return
      do 43 igs = 1, NGT
        iixc=0
        iiyc=0

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
          call u2pixel(AAX,AAY,iix,iiy)
          iixlast=iix   ! cast for edline call
          iiylast=iiy
          iixc=iixc+iix
          iiyc=iiyc+iiy

C Draw succeeding points until last vertex.
          IP1 = IBNXT(IP)
 451      CONTINUE
          CALL  ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,iix,iiy)
          if(linstyg(igs).eq.3)then
            call edline(iixlast,iiylast,iix,iiy,2)
          elseif(linstyg(igs).eq.2)then
            call edwline(iixlast,iiylast,iix,iiy)
          elseif(linstyg(igs).eq.1)then
            call eswline(iixlast,iiylast,iix,iiy)
          else
            call edline(iixlast,iiylast,iix,iiy,LD)
          endif
          iixc=iixc+iix
          iiyc=iiyc+iiy

          IP1 = IBNXT(IP1)
          iixlast=iix   ! cast for call to edline
          iiylast=iiy
          IF(IP1 .NE. IP) GOTO 451

C Complete ground surface.
          CALL  ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,iix,iiy)
          if(linstyg(igs).eq.3)then
            call edline(iixlast,iiylast,iix,iiy,2)
          elseif(linstyg(igs).eq.2)then
            call edwline(iixlast,iiylast,iix,iiy)
          elseif(linstyg(igs).eq.1)then
            call eswline(iixlast,iiylast,iix,iiy)
          else
            call edline(iixlast,iiylast,iix,iiy,LD)
          endif
          call forceflush()

C If ground surface names are to be displayed place near the centre of polygon.
C If the surface is not drawn (iixc=0 & iyc=0) do not label it.  For clarity drop to
C the next smaller font for the surface name and then restore font
C to normal size.
          IF(ITSNM.EQ.0)THEN
            IF(iixc.GT.0.AND.iiyc.GT.0.AND.(NAP.EQ.NBP))THEN
              IF(IFS.GT.0)CALL winfnt(NAMEFT)
              write(stemp,'(A)')GSNAME(igs)
              iixc=iixc/NBP
              iiyc=iiyc/NBP
              iid2=iiyc+(3*(NAMEFT+1))
              CALL wstxpt(iixc,iid2,stemp)
              IF(IFS.GT.0)CALL winfnt(IFS)
            endif
          endif
  302   CONTINUE
  43  continue
      call forceflush()

      return
      end

C-----DSGRID -----------------------------------------------------------
C DSGRID draws a dotted grid at a given reference height (RH), grid
C density (GD), and dot density (LD) within the viewport
C with the current scaling factors etc.
C IER is returned as non-zero if there is a problem.
      SUBROUTINE DSGRID(RH,GD,LD,IER)
#include "building.h"
#include "prj3dv.h"

C Passed parameters.
      real RH,GD
      integer LD,IER
      
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C Explicit local variables
      real COG1,COG2,COG3
      dimension COG1(3),COG2(3),COG3(3)
      real GRIDD,XD,DD
      real GXMN,GYMN,XINC,YINC
      logical close,greyok

C Local variables to pass to edline.
#ifdef OSI
      integer iix,iiy,iix1,iiy1
#else
      integer*8 iix,iiy,iix1,iiy1
#endif       
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
        call u2pixel(COG3(1),COG3(2),iix,iiy)

C Transform other end of line.
        COG1(1)=XINC
        COG1(2)=YMX+GRIDD
        COG1(3)=RH
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),iix1,iiy1)

C Draw the dotted line, first take calculated pixels and
C cast to local variable to pass to edline. Only do this if
C the length of the line is more than one pixel.
        if(iix.eq.iix1.and.iiy.eq.iiy1)then
          continue
        else
          call edline(iix,iiy,iix1,iiy1,LD)
        endif

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
        call u2pixel(COG3(1),COG3(2),iix,iiy)

C Transform other end of line.
        COG1(1)=XMX+GRIDD
        COG1(2)=YINC
        COG1(3)=RH
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),iix1,iiy1)

C Draw the dotted line after casting to local variable.
        if(iix.eq.iix1.and.iiy.eq.iiy1)then
          continue
        else
          call edline(iix,iiy,iix1,iiy1,LD)
        endif

C Move to next grid position.
        YINC=YINC+GRIDD
        GOTO 110
      ENDIF
      if(greyok)call winscl('-',0)
      call forceflush()

      RETURN
      END

C ******************* EMKVIEW 
C EMKVIEW constructs a 'viewer' format file. IUO is the file unit number.
C CFGOK is passed in as .TRUE. if the user can select zones.
C IER is returned as non-zero if there is a problem.
      SUBROUTINE EMKVIEW(IUO,CFGOK,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"

C Passed parameters.
      integer IUO,IER
      logical CFGOK
      
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      LOGICAL OK,dok
      logical newgeo  ! to use for testing if new/old geometry file.

C Use JGVNT for ground surfaces and JVNZ to hold inverted order for zone.
      DIMENSION IVALS(MCOM),JGVNT(MGRT,8),JVNZ(MS,MV)
      CHARACTER H*72
      CHARACTER GMLCN*12,GSNAME*6,outs*124

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

C In the future it may not be necessary to re-scan the geometry
C files if we switch to the global coordinate data structures.
C << task to be done >>

      IF(INPIC.EQ.0)GOTO 105
      DO 83 IZ=1,INPIC
        IF(IVALS(IZ).GT.0)THEN
          call eclose(gversion(IVALS(IZ)),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IUF,LGEOM(IVALS(IZ)),IVALS(IZ),1,iuout,IER)
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
          CALL EGOMST(IUF,iz,ZOBS(IZ),0,0,iuout,IER)
          IF(IER.NE.0)RETURN
        elseif(IOBS(IZ).EQ.2)THEN
          continue  ! any obstructions are in the geometry file.
        endif
        if(nbobs(iz).gt.0)then
          DO 301 IB=1,nbobs(iz)

C Conversion using cnvblk or cvnblk3a and common GB1.
            if(BLOCKTYP(iz,IB)(1:4).eq.'obs ')then
              CALL CNVBLK(XOB(iz,IB),YOB(iz,IB),ZOB(iz,IB),
     &          DXOB(iz,IB),DYOB(iz,IB),DZOB(iz,IB),BANGOB(iz,IB,1))
            elseif(BLOCKTYP(iz,IB)(1:4).eq.'obs3')then
              CALL CNVBLK3A(XOB(iz,IB),YOB(iz,IB),ZOB(iz,IB),
     &          DXOB(iz,IB),DYOB(iz,IB),DZOB(iz,IB),BANGOB(iz,IB,1),
     &          BANGOB(iz,IB,2),BANGOB(iz,IB,3))
            elseif(BLOCKTYP(iz,IB)(1:4).eq.'obsp')then
              call CNVBLKP(iz,IB) ! convert obsp type.
            endif

C Write obstruction information to the viewer file. First invert edges.
            DO 320 J=1,6
              K=4
              DO 330 L=1,K/2
                ITEMP=JVNB(J,L)
                JVNB(J,L)=JVNB(J,K+1-L)
                JVNB(J,K+1-L)=ITEMP
  330         CONTINUE
  320       CONTINUE

C Write obstructions to output file.
            WRITE(IUO,218)zname(IVALS(IZ))
  218       FORMAT('GEN',1X,A12,' obstructions')
            WRITE(IUO,'(a)')'  8  6'
            DO 240 J=1,8
              WRITE(IUO,'(3F8.3)')XB(J),YB(J),ZB(J)
  240       CONTINUE
            DO 250 J=1,6
              WRITE(IUO,'(a,21I3)') '  4 ',(JVNB(J,K),K=1,4)
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
