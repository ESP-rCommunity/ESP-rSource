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

C cread3dv.F contains the following Fortran subroutines:

C  DRAWESP: Loads the geometric description(s) from common blocks,
C           converts into viewing coords, clips the image and draws it.

C cread3dv.F is an extended version of read3dv.F which makes use of
C connection based data i.e. G7 rather than G5 and can take advantage
C of G8 common block data for enhanced edge drawing. Assumes that other
C source code manages the G7 and G8 data.
 
C ******************** DRAWESP 
C DRAWESP loads the geometric description(s) from common blocks,
C converts into viewing coords, clips the image and draws it.
C ier is returned as non-zero if there has been a problem.
C This code is similar to that used in cfg3dv.F subroutine cdrawesp.
      SUBROUTINE DRAWESP(ier)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"

C Passed parameter.
      integer ier

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
C      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C szcoords : X,Y & Z coordinates in model (via geometry.h).
C iszjvn : topology of vertices (via geometry.h).

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL  ISTOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical greyok,found

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3)
      DIMENSION  CG(3),VN(3),ipoints(6,2)

      CHARACTER temp*16,DESCRC*25,stemp*13,temp20*21
      CHARACTER t13*13

C Local variables to pass to edline and axiscale.
#ifdef OSI
      integer iix,iiy,iixlast,iiylast,iixc,iiyc,iid1,iid2,iid3,iid4
      integer iicol,ipixw,ipixh
#else
      integer*8 iix,iiy,iixlast,iiylast,iixc,iiyc,iid1,iid2,iid3,iid4
      integer*8 iicol,ipixw,ipixh
#endif

C If not in graphic mode return.
      if(MMOD.lt.8)return

C If libraries and monitor support greyscale remember this.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Switch to a smaller font for displaying surface names etc.
C Iixc,iiyc keep track of surface/pixel COG.
      issize=4; izsize=5  ! for surfaces and zone name

C The right offset for the graphic box is the character width of the
C menu. Also allow 2 text lines at the bottom for future dialogue.
      IER=0

C Refresh the dialogue box if terminal type 8.
      IF(MMOD.EQ.8)CALL USRMSG(' ',' ','-')

C If site grid or ground is asked for draw now.
      IF(ITGRD.EQ.0)CALL DSGRID(0.0,GRDIS,5,IER)
      IF(ITDSP.EQ.5.or.ITDSP.EQ.6)call EGRNDR(IER)
      IF(ITDSP.EQ.6)return

C Beginning of loop to read in several ESP compatible geometry files.
C The first task is to set iz to zero and istok to true to force the
C code to draw the site origin symbol.
      IZ=0
      ISTOK=.TRUE.

  100 CONTINUE

C If the origin of the site is to be shown include this in the image
C and then go on to the normal objects. After this point ISTOK is .FALSE.
      if(ITORG.EQ.0.and.ISTOK)then
        CALL ORIGESP
        ISTOK=.FALSE.
      elseif(ITORG.EQ.1.and.ISTOK)then
        ISTOK=.FALSE.
      endif

      IZ=IZ+1
      IF(IZ.GT.nzg)RETURN

C If obstructions are to be drawn read the obstruction file, transform
C the obstruction description into a gen body and plot them out
C before going on to the surfaces in the zone itself.
      IF(ITDSP.EQ.0.or.ITDSP.eq.5.or.ITDSP.eq.7)then
        CALL DRAWOBS(nznog(IZ),ier)
      ENDIF

C Also draw visual entities if ITDSP set to zero or six.
      IF(ITDSP.EQ.0.or.ITDSP.eq.6)CALL DRAWVIS(nznog(IZ),ier)

C If zone names are to be displayed place near the centre of zone.
C For clarity use larger font and then restore font to normal size.
C Transform the site COG coords into eyepoint and then screen coords.
C Find the pixels required for the name and centre text.
C Note: temp character t13 is 1 char wider than zname().
      IF(ITZNM.EQ.0)THEN
        COG1(1)=ZCOG(nznog(IZ),1)
        COG1(2)=ZCOG(nznog(IZ),2)
        COG1(3)=ZCOG(nznog(IZ),3)
        CALL VECTRN(COG1,TSMAT,COG2,IER)
        call u2pixel(COG2(1),COG2(2),iix,iiy)

C Note call to textpixwidth corrupts t13 so need to
C re-create t13 after textpixwidth call.
        t13=' '
        write(t13,'(A)')zname(nznog(IZ))(1:lnzname(nznog(IZ)))
        CALL winfnt(izsize)
        call textpixwidth(t13,ipixw,ipixh)
        iixc = iix - (ipixw/2)
        write(t13,'(A)')zname(nznog(IZ))(1:lnzname(nznog(IZ)))
        call CLIPST(t13,iixc,iiy,COG2(3),iclp)
        if (iclp.eq.0) then
          iicol=0
          call textatxy(iixc,iiy,t13,'-',iicol)
          call forceflush()
        endif
        CALL winfnt(IFS)
      ENDIF
      IF(ier.NE.0)RETURN

C If there are no surfaces yet defined then draw the vertices with 
C label and loop back to the next body.
      IF(NZSUR(nznog(IZ)).EQ.0.AND.NZTV(nznog(IZ)).GT.1)THEN
        DO 202 IV=1,NZTV(nznog(IZ))
          COG1(1)=szcoords(nznog(IZ),IV,1)
          COG1(2)=szcoords(nznog(IZ),IV,2)
          COG1(3)=szcoords(nznog(IZ),IV,3)
          CALL VECTRN(COG1,TSMAT,COG2,IER)
          call CLIPPT(COG2(1),COG2(2),COG2(3),iclp)
          if (iclp.eq.0) then
            call u2pixel(COG2(1),COG2(2),iix,iiy)
            CALL ecirc(iix,iiy,3,1)
            CALL VERTLBL(iix,iiy,COG2(3),IV,ier)
            call forceflush()
          endif
  202   CONTINUE
        GOTO 100
      ELSEIF(NZSUR(nznog(IZ)).GE.1.AND.NZTV(nznog(IZ)).GT.1)THEN

C Check to see if there are any unlinked vertices, if so then draw them
C before proceeding to draw the surfaces. For each vertex, loop through
C each surface and its vertex list and see how many hits.
        found=.false.
        DO 204 IZV=1,NZTV(nznog(IZ))
          IHIT=0
          DO 8792 IS=1,NZSUR(nznog(IZ))
            icc=IZSTOCN(nznog(IZ),IS)
            DO 8794 IV=1,isznver(nznog(IZ),IS)
              IF(IZV.EQ.iszjvn(nznog(IZ),IS,IV))IHIT=IHIT+1
 8794       CONTINUE
 8792     CONTINUE

C If less than 2 hits draw the vertex as an open circle, if no hits
C draw it as a solid circle.
          IF(IHIT.LT.2)THEN
            COG1(1)=szcoords(nznog(IZ),IZV,1)
            COG1(2)=szcoords(nznog(IZ),IZV,2)
            COG1(3)=szcoords(nznog(IZ),IZV,3)
            CALL VECTRN(COG1,TSMAT,COG2,IER)
            call CLIPPT(COG2(1),COG2(2),COG2(3),iclp)
            if (iclp.eq.0) then
              found=.true.
              call u2pixel(COG2(1),COG2(2),iix,iiy)
              IF(IHIT.EQ.0)THEN
                CALL ecirc(iix,iiy,3,1)
              ELSEIF(IHIT.GT.0.AND.IHIT.LT.2)THEN
                CALL ecirc(iix,iiy,3,0)
              ENDIF
              CALL VERTLBL(iix,iiy,COG2(3),IZV,ier)
            endif
          ENDIF
  204   CONTINUE

C If any were drawn then draw a legend for the meaning
C of the vertex symbols in small proportional font.
        if (found) then
          CALL winfnt(issize)
          iid1=igl+10; iid2=igb+5;
          CALL ecirc(iid1,iid2,3,1)
          write(temp,'(A)')'Unlinked vertex'
          iid1=igl+20; iid2=igb+10;
          iicol=0
          call textatxy(iid1,iid2,temp,'-',iicol)
          iid1=igl+130; iid2=igb+5;
          CALL ecirc(iid1,iid2,3,0)
          write(temp20,'(A)')'Single-linked vertex'
          iid1=igl+140; iid2=igb+10;
          call textatxy(iid1,iid2,temp20,'-',iicol)
          CALL winfnt(IFS)
        endif
        call forceflush()
      ENDIF

C Continue on with any surfaces in the zone.
      DO 1000 I=1,NZSUR(nznog(IZ))

C icc is the connection related to the current surface.
        icc=IZSTOCN(nznog(IZ),I)

C If the configuration file has been read and there is a filter for
C which surfaces should be displayed test this here. Locate the
C connection (IC) which relates to this surface.
        IF(CFGOK)THEN
          CALL SURADJ(nznog(IZ),I,IE,TMP,IZC,ISC,IC,DESCRC)
          IF(ITDSP.EQ.2.AND.IE.NE.0)GOTO 1000
          IF(ITDSP.EQ.3.AND.IE.NE.3)GOTO 1000
          IF(ITDSP.EQ.4.AND.(IE.EQ.0.OR.IE.EQ.3))GOTO 1000
        ENDIF

C Copy polygon for Surface I into structure A for compatibility with
C viewer format 'holes' in surfaces.
C NAP   = Number of vertex points on surface
C IANXT = Index of 'next' point
C IAPNT = Pointer to first vertex of polygon
        IAPNT = 1
        NAP = isznver(nznog(IZ),I)
        DO 1100 J = 1,isznver(nznog(IZ),I)
          K = iszjvn(nznog(IZ),I,J)
          AX(J) = szcoords(nznog(IZ),K,1)
          AY(J) = szcoords(nznog(IZ),K,2)
          AZ(J) = szcoords(nznog(IZ),K,3)
          IANXT(J) = J + 1
 1100   CONTINUE

        IANXT(isznver(nznog(IZ),I)) = IAPNT

C-----------------------------------------------------------------------
C Transform surface polygon to EYE co-ordinates
C Take structure A multiply by TEMAT return structure B
C TEMAT = Model to Eye Matrix
        CALL MATPOL(NAP,IAPNT,AX,AY,AZ,IANXT,TSMAT,
     &              SBBOX,NBP,IBPNT,BX,BY,BZ,IBNXT,IERR)

C Generate clipping flags and clip geometry in eye coords.
C If ISTAT =  0 : totally inside frustrum
C If ISTAT =  1 : totally outside frustrum
C If ISTAT = -1 : straddles frustrum
        CALL CLIPFL(NBP,BX,BY,BZ,ISTAT)
        IF (ISTAT .EQ. 1) THEN
          GOTO  1000
        ELSEIF (ISTAT .EQ. -1) THEN
          CALL CUTPOL(NB,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
        else
          NB=1
        ENDIF

        iixc=0
        iiyc=0

C Set line style for this surface. linsty=2 is double width line. 
        if(ICC.ne.0)then
          if(LINSTY(ICC).eq.2)then
            IPEN= -305
          else
            IPEN= 1
          endif
        else
          IPEN= 1
        endif

C Check for hilights. ITHLS=1 is composition, ITHLS=2 trn:opq,
C ITHLS = 3 partial attributes
        if(ITHLS.eq.1.and.ITHLZ.gt.0)then
          lnssmlc=lnblnk(SMLCN(nznog(IZ),I))
          if(mlcname(ITHLZ)(1:lnmlcname(ITHLZ)).eq.
     &       SMLCN(nznog(IZ),I)(1:lnssmlc))then
            IPEN= -305
          else
            IPEN= 1
          endif
        elseif(ITHLS.eq.2)then
          if(ITHLZ.eq.1.and.SOTF(nznog(IZ),I)(1:4).eq.'OPAQ')then
            IPEN= -305
          else
            IPEN= 1
          endif
          if(ITHLZ.eq.2.and.SOTF(nznog(IZ),I)(1:4).ne.'OPAQ')then
            IPEN= -305
          else
            IPEN= 1
          endif
        elseif(ITHLS.eq.3)then
          if(SMLCN(nznog(IZ),I)(1:4).eq.'UNKN'.or.
     &       SOTHER(nznog(IZ),I,1)(1:4).eq.'UNKN')then
            IPEN= -305
          else
            IPEN= 1
          endif
        endif

C Debug.
C        write(6,*)'icc linsty ipen ithls ithlz',icc,linsty(icc),ipen,
C     &    ithls,ithlz,ISTOK

C Count the edges in case enhanced edge drawing required.
        iedge=0
        DO 300 J = 1,NB

C Move to the beginning.
          IP = IABS(IBPNT)
          call u2pixel(BX(IP),BY(IP),iix,iiy)

C If vertex are to be labeled do it in the smallest font, then
C move cursor to vertex and remember position.
          IF(ITVNO.EQ.0.AND.IP.LE.NAP)THEN
            IV=iszjvn(nznog(IZ),I,IP)
            CALL VERTLBL(iix,iiy,BZ(IP),IV,ier)
          ENDIF
          iixlast=iix   ! cast for edline call
          iiylast=iiy

          IP1 = IBNXT(IP)

 351      CONTINUE
          iedge=iedge+1
          call u2pixel(BX(IP1),BY(IP1),iix,iiy)
          if(IPEN.eq.1)then

C Normal (not highlighted) edge being drawn, if NAP = NBP then check
C common/G8 for hints as to alternate greys or dots.
            if(NAP.ne.NBP)then
              if(mmod.eq.8)then
                call eswline(iixlast,iiylast,iix,iiy)
              else
                call eswlinewwc(iixlast,iiylast,iix,iiy)
              endif
            else

C If there will be diagonal lines drawn (duplicate edges) check if the
C current edge is marked. If dotted line is drawn then don't bother
C with the co-planer.
              found=.false.
              if(nbedgdup(icc).ne.0)then
                if(iedgdup(icc,iedge).eq.0)then
                  found=.false.
                else
                  LD=4
                  if(mmod.eq.8)then
                    call edline(iixlast,iiylast,iix,iiy,LD)
                  else
                    call edlinewwc(iixlast,iiylast,iix,iiy,LD)
                  endif
                  found=.true.
                endif
              endif

C If this edge is shared with a co-planer surface with the same construction
C and greys can be done reset forground to 50% grey otherwise draw black,
C unless duplicate line has already been drawn.
              if(imatshr(icc,iedge).eq.0)then
                if(.NOT.found)then
                  if(mmod.eq.8)then
                    call eswline(iixlast,iiylast,iix,iiy)
                  else
                    call eswlinewwc(iixlast,iiylast,iix,iiy)
                  endif
                endif
              else
                if(greyok)then
                  iicol=nifgrey-3 
                  if(mmod.eq.8) call winscl('i',iicol)
                endif
                if(mmod.eq.8)then
                  call eswline(iixlast,iiylast,iix,iiy)
                else
                  call eswlinewwc(iixlast,iiylast,iix,iiy)
                endif
                if(greyok)then
                  iicol=0
                  if(mmod.eq.8) call winscl('-',iicol)
                endif
              endif
            endif
          elseif(IPEN.eq.-305)then
            if(mmod.eq.8)then
              call edwline(iixlast,iiylast,iix,iiy)
            else
              call edwlinewwc(iixlast,iiylast,iix,iiy)
            endif
          else
            if(mmod.eq.8)then
              call eswline(iixlast,iiylast,iix,iiy)
            else
              call eswlinewwc(iixlast,iiylast,iix,iiy)
            endif
          endif
          iixc=iixc+iix
          iiyc=iiyc+iiy
          call forceflush()

C If vertex are to be labeled do it in the smallest font and
C move cursor back to vertex and remember position.
          IF(ITVNO.EQ.0.AND.IP1.LE.NAP)THEN
            IV=iszjvn(nznog(IZ),I,IP1)
            CALL VERTLBL(iix,iiy,BZ(IP1),IV,ier)
            iixlast=iix   ! cast for edline call
            iiylast=iiy
          ENDIF

C Repeat until next point is the first, remember position.
          IP1 = IBNXT(IP1)
          iixlast=iix   ! cast for edline call
          iiylast=iiy
          IF(IP1 .NE. IP) GOTO 351

C Complete the polygon.
          iedge=iedge+1
          call u2pixel(BX(IP),BY(IP),iix,iiy)
          if(IPEN.eq.1)then

C Normal (not highlighted) edge being drawn, if NAP = NBP then check
C common/G8 for hints as to alternate greys or dots.
            if(NAP.ne.NBP)then
              if(mmod.eq.8)then
                call eswline(iixlast,iiylast,iix,iiy)
              else
                call eswlinewwc(iixlast,iiylast,iix,iiy)
              endif
            else

C If there will be diagonal lines drawn (duplicate edges) check if the
C current edge is marked. If dotted line is drawn then don't bother
C with the co-planer.
              found=.false.
              if(nbedgdup(icc).ne.0)then
                if(iedgdup(icc,iedge).eq.0)then
                  found=.false.
                else
                  LD=4
                  if(mmod.eq.8)then
                    call edline(iixlast,iiylast,iix,iiy,LD)
                  else
                    call edlinewwc(iixlast,iiylast,iix,iiy,LD)
                  endif
                  found=.true.
                endif
              endif

C If this edge is shared with a co-planer surface with the same construction
C and greys can be done reset forground to 50% grey otherwise draw black,
C unless duplicate line has already been drawn.
              if(imatshr(icc,iedge).eq.0)then
                if(.NOT.found)then
                  if(mmod.eq.8)then
                    call eswline(iixlast,iiylast,iix,iiy)
                  else
                    call eswlinewwc(iixlast,iiylast,iix,iiy)
                  endif
                endif
              else
                if(greyok)then
                  iicol=nifgrey-3
                  if(mmod.eq.8) call winscl('i',iicol)
                endif
                if(mmod.eq.8)then
                  call eswline(iixlast,iiylast,iix,iiy)
                else
                  call eswlinewwc(iixlast,iiylast,iix,iiy)
                endif
                if(greyok)then
                  iicol=0
                  if(mmod.eq.8) call winscl('-',iicol)
                endif
              endif
            endif
          elseif(IPEN.eq.-305)then
            if(mmod.eq.8)then
              call edwline(iixlast,iiylast,iix,iiy)
            else
              call edwlinewwc(iixlast,iiylast,iix,iiy)
            endif
          else
            if(mmod.eq.8)then
              call eswline(iixlast,iiylast,iix,iiy)
            else
              call eswlinewwc(iixlast,iiylast,iix,iiy)
            endif
          endif
          iixc=iixc+iix
          iiyc=iiyc+iiy
          call forceflush()
  300   CONTINUE

C If surface names are to be displayed place near the centre of polygon.
C If the surface is not drawn (iixc=0 & iiyc=0) do not label it. If the
C surface has been clipped then do not label it. For clarity drop to
C the next smaller font for the surface name and then restore font
C to normal size.
        IF(ITSNM.EQ.0)THEN
          IF(iixc.GT.0.AND.iiyc.GT.0.AND.(NAP.EQ.NBP))THEN
            if(icc.ne.0)then

C If surface normals to be displayed draw an arrow and
C offset the surface name depending on orientation.
              if(itsnr.eq.0)then
                CG(1)=SURCOG(nznog(IZ),I,1)
                CG(2)=SURCOG(nznog(IZ),I,2)
                CG(3)=SURCOG(nznog(IZ),I,3)
                VN(1)=SURVN(nznog(IZ),I,1)
                VN(2)=SURVN(nznog(IZ),I,2)
                VN(3)=SURVN(nznog(IZ),I,3)
                CALL VECTRN(VN,TSMAT,COG2,IER)
                call arrow(CG,VN,0.3,0.1,ipoints,'a',2)
                call u2pixel(COG2(1),COG2(2),iixc,iiyc)
                CALL winfnt(issize)
                write(stemp,'(A)')SNAME(nznog(IZ),I)
                iixc=iixc+5

C Check clipping.
                call CLIPST(stemp,iixc,iiyc,COG2(3),iclp)
                if (iclp.eq.0) then
                  iicol=0
                  call textatxy(iixc,iiyc,stemp,'-',iicol)
                endif
                CALL winfnt(IFS)
              else
                CG(1)=SURCOG(nznog(IZ),I,1)
                CG(2)=SURCOG(nznog(IZ),I,2)
                CG(3)=SURCOG(nznog(IZ),I,3)
                CALL VECTRN(CG,TSMAT,COG2,IER)
                call u2pixel(COG2(1),COG2(2),iixc,iiyc)
                IF(SVFC(nznog(IZ),I).EQ.'VERT')THEN

C Check for clipping.
                  call pixel2u(iixc,iiyc+3,xx,xy)
                  call CLIPPT(xx,xy,COG2(3),iclp1)
                  call pixel2u(iixc+ipixw+7,iiyc+3-ipixh,xx,xy)
                  call CLIPPT(xx,xy,COG2(3),iclp2)

C Draw arrow and horizontal line.
                  if (iclp1.eq.0 .and. iclp2.eq.0) then
                    iid1=iixc+3; iid2=iiyc-3
                    if(mmod.eq.8)then
                      call eswline(iixc,iiyc,iid1,iid2)
                      iid1=iixc+3; iid2=iiyc+3
                      call eswline(iixc,iiyc,iid1,iid2)
                      iid1=iixc+7
                      call eswline(iixc,iiyc,iid1,iiyc)
                      iixc=iixc+7
                    else
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                      iid1=iixc+3; iid2=iiyc+3
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                      iid1=iixc+7
                      call eswlinewwc(iixc,iiyc,iid1,iiyc)
                      iixc=iixc+7
                    endif
                    found=.true.
                  else
                    found=.false.
                  endif
                ELSEIF(SVFC(nznog(IZ),I).EQ.'CEIL')THEN

C Check for clipping.
                  call pixel2u(iixc-3,iiyc,xx,xy)
                  call CLIPPT(xx,xy,COG2(3),iclp1)
                  call pixel2u(iixc+ipixw+7,iiyc-ipixh,xx,xy)
                  call CLIPPT(xx,xy,COG2(3),iclp2)

C Draw arrow to surface then up and horizontal to the text.
                  if (iclp1.eq.0 .and. iclp2.eq.0) then
                    iid1=iixc+3; iid2=iiyc-3
                    if(mmod.eq.8)then
                      call eswline(iixc,iiyc,iid1,iid2)
                      iid1=iixc-3; iid2=iiyc-3
                      call eswline(iixc,iiyc,iid1,iid2)
                      iid1=iiyc-5
                      call eswline(iixc,iiyc,iixc,iid1)
                      iid1=iiyc-5; iid2=iixc+7; iid3=iiyc-5
                      call eswline(iixc,iid1,iid2,iid3)
                    else
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                      iid1=iixc-3; iid2=iiyc-3
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                      iid1=iiyc-5
                      call eswlinewwc(iixc,iiyc,iixc,iid1)
                      iid1=iiyc-5; iid2=iixc+7; iid3=iiyc-5
                      call eswlinewwc(iixc,iid1,iid2,iid3)
                    endif
                    iixc=iixc+7
                    found=.true.
                  else
                    found=.false.
                  endif
                ELSEIF(SVFC(nznog(IZ),I).EQ.'FLOR')THEN

C Check for clipping.
                  call pixel2u(iixc-3,iiyc+8,xx,xy)
                  call CLIPPT(xx,xy,COG2(3),iclp1)
                  call pixel2u(iixc+ipixw+7,iiyc+8-ipixh,xx,xy)
                  call CLIPPT(xx,xy,COG2(3),iclp2)

C Draw arrow to surface then down and horizontal to the text.
                  if (iclp1.eq.0 .and. iclp2.eq.0) then
                    iid1=iixc+3; iid2=iiyc+3
                    if(mmod.eq.8)then
                      call eswline(iixc,iiyc,iid1,iid2)
                      iid1=iixc-3; iid2=iiyc+3
                      call eswline(iixc,iiyc,iid1,iid2)
                      iid1=iiyc+5
                      call eswline(iixc,iiyc,iixc,iid1)
                      iid1=iiyc+5; iid2=iixc+7; iid3=iiyc+5
                      call eswline(iixc,iid1,iid2,iid3)
                    else
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                      iid1=iixc-3; iid2=iiyc+3
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                      iid1=iiyc+5
                      call eswline(iixc,iiyc,iixc,iid1)
                      iid1=iiyc+5; iid2=iixc+7; iid3=iiyc+5
                      call eswlinewwc(iixc,iid1,iid2,iid3)
                    endif
                    iixc=iixc+7; iiyc=iiyc+8
                    found=.true.
                  else
                    found=.false.
                  endif
                ENDIF
                CALL winfnt(issize)
                if (found) then
                  iicol=0
                  call textatxy(iixc,iiyc,stemp,'-',iicol)
                endif
                CALL winfnt(IFS)
              endif
            endif
          ENDIF
        ENDIF
 1000 CONTINUE
      call forceflush()

C Loop back again to see if there is more.
      GOTO 100

      END
