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

C Patched version of subroutines in read3dv.f (selective display of
C surface names).

C CADJVIEW: Adjusts the viewing transforms & boundaries if necessary.
C CDRAWESP: Loads the geometric description(s) from an ESP file,
C          converts into viewing coords, clips the image and draws it.

C ************* CADJVIEW 
C CADJVIEW takes the current status of view and boundary mods
C and adjusts the viewing transforms & object boundaries if 
C necessary before displaying the zone.
C IER=0 OK, IER=1 problem. 
C izgfoc greater than zero represents the zone which is currently being edited.
      SUBROUTINE CADJVIEW(IER)
#include "building.h"
#include "prj3dv.h"

C Passed parameter.
      integer ier

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      LOGICAL MODIFY,MODLEN,MODBND
      integer numberofzones ! to pass to updwire rather than ncomp

      SMALL=0.01

C If not in graphic mode return.
      if(MMOD.lt.8)return

C Tell wireframe control the current number of selected zones.
      numberofzones=nzg
      call updwire(numberofzones)  ! pass local integer

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

C Draw the selected bodies.
        CALL CDRAWESP(IER)

C Recover information begin edited and reset modify flag.
        if(izgfoc.NE.0)CALL ERCZONE(izgfoc)
        MODIFY=.FALSE.
        MODLEN=.FALSE.
        MODBND=.FALSE.
      ENDIF

      RETURN
      END

C ******************** CDRAWESP 
C CDRAWESP loads the geometric description(s) from common blocks,
C converts into viewing coords, clips the image and draws it.
C ier is returned as non-zero if there has been a problem.
C This code is similar to that used in cread3dv.F subroutine drawesp.
      SUBROUTINE CDRAWESP(ier)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"

C Passed parameter.
      integer ier

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
C      COMMON/OUTIN/IUOUT,IUIN
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/G8/nbedgdup(MCON),iedgdup(MCON,MV),nbedgshr(MCON),
     &          iedgshr(MCON,MV),imatshr(MCON,MV)

      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY6/LINSTY(MCON)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C VCOORD : X,Y & Z coordinates in model (via geometry.h).
C NZNVER : number of vertices associated with each connection (via geometry.h).
C NZJVN : topology of vertices associated with each connection (via geometry.h).

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL  ISTOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical greyok,found

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3),COG3(3)
      DIMENSION  CG(3),VN(3),ipoints(6,2)

      CHARACTER temp*16,DESCRC*25,stemp*13,temp20*21
      CHARACTER MLCN*12,t13*13

C Local variables to pass to edline and axiscale.
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iix,iiy,iixlast,iiylast,iixc,iiyc,iid1,iid2
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iix,iiy,iixlast,iiylast,iixc,iiyc,iid1,iid2
#endif

C If not in graphic mode return.
      if(MMOD.lt.8)return

C If libraries and monitor support greyscale remember this.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Save the current font and switch to a smaller font for displaying
C surface names etc. Iixc,iiyc keep track of surface/pixel COG.
      NAMEFT=IFS-1

C The right offset for the graphic box is the character width of the
C menu. Also allow 2 text lines at the bottom for future dialogue.
      IER=0

C Clear current viewing box and re-establish image bounds.
      CALL startbuffer()
      CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)
      igw=igr-igl
      igwh=igb-igt
      igwid=igw
      igheight=igwh

C Determine scaling ratios for the drawing and axis.  If static
C assume 1000 -1000 etc, if optimum then use SXMX,SYMX + 10% etc.
      IF(ITBND.EQ.0)THEN
        CALL axiscale(igwid,igheight,-1000.,1000.,-1000.,1000.,xsc,ysc,
     &                sca,Xadd,Yadd)
        call linescale(igl,Xadd,sca,igb,Yadd,sca)
      ELSEIF(ITBND.EQ.1)THEN

C Determine the 2D coords of the site extremes.
        CALL SITE2D(SXMX,SXMN,SYMX,SYMN,ier)
        CALL axiscale(igwid,igheight,SXMN,SXMX,SYMN,SYMX,xsc,ysc,sca,
     &                Xadd,Yadd)
        call linescale(igl,Xadd,sca,igb,Yadd,sca)
      ENDIF

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
      IF(ITDSP.EQ.0.or.ITDSP.eq.5)CALL DRAWOBS(nznog(IZ),ier)

C If zone names are to be displayed place near the centre of zone.
C For clarity use larger font and then restore font to normal size.
C Transform the site COG coords into eyepoint and then screen coords.
C Find the pixels required for the name and centre text.
C Note: temp character t13 is 1 char wider than zname().
      IF(ITZNM.EQ.0)THEN
        COG1(1)=ZCOG(nznog(IZ),1)
        COG1(2)=ZCOG(nznog(IZ),2)
        COG1(3)=ZCOG(nznog(IZ),3)
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),iix,iiy)
        write(t13,'(A)')zname(nznog(IZ))
        call textpixwidth(t13,ipixw)
        iixc = iix - (ipixw/2)
        CALL wstxpt(iixc,iiy,t13)
        call forceflush()
      ENDIF
      IF(ier.NE.0)RETURN

C If there are no surfaces yet defined then draw the vertices with 
C label and loop back to the next body.
      IF(NZSUR(nznog(IZ)).EQ.0.AND.NZTV(nznog(IZ)).GT.1)THEN
        DO 202 IV=1,NZTV(nznog(IZ))
          COG1(1)=VCOORD(nznog(IZ),IV,1)
          COG1(2)=VCOORD(nznog(IZ),IV,2)
          COG1(3)=VCOORD(nznog(IZ),IV,3)
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),iix,iiy)
          CALL ecirc(iix,iiy,3,1)
          CALL VERTLBL(iix,iiy,IV,ier)
          call forceflush()
  202   CONTINUE
        GOTO 100
      ELSEIF(NZSUR(nznog(IZ)).GE.1.AND.NZTV(nznog(IZ)).GT.1)THEN

C Check to see if there are any unlinked vertices, if so then draw them
C before proceeding to draw the surfaces. For each vertex, loop through
C each surface and its vertex list and see how many hits.
        DO 204 IZV=1,NZTV(nznog(IZ))
          IHIT=0
          DO 8792 IS=1,NZSUR(nznog(IZ))
            icc=IZSTOCN(nznog(IZ),IS)
            DO 8794 IV=1,NZNVER(icc)
              IF(IZV.EQ.NZJVN(icc,IV))IHIT=IHIT+1
 8794       CONTINUE
 8792     CONTINUE

C If less than 2 hits draw the vertex as an open circle, if no hits
C draw it as a solid circle.
          IF(IHIT.LT.2)THEN
            COG1(1)=VCOORD(nznog(IZ),IZV,1)
            COG1(2)=VCOORD(nznog(IZ),IZV,2)
            COG1(3)=VCOORD(nznog(IZ),IZV,3)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            IF(IHIT.EQ.0)THEN
              CALL ecirc(iix,iiy,3,1)
            ELSEIF(IHIT.GT.0.AND.IHIT.LT.2)THEN
              CALL ecirc(iix,iiy,3,0)
            ENDIF
            CALL VERTLBL(iix,iiy,IZV,ier)

C If less than 2 hits then draw a legend for the meaning
C of the vertex symbols.
            IF(IFS.GT.0)CALL winfnt(1)
            iid1=igl+20; iid2=igb+25;
            CALL ecirc(iid1,iid2,3,1)
            write(temp,'(A)')'Unlinked vertex'
            iid1=igl+30; iid2=igb+30;
            CALL wstxpt(iid1,iid2,temp)
            iid1=igl+140; iid2=igb+25;
            CALL ecirc(iid1,iid2,3,0)
            write(temp20,'(A)')'Single-linked vertex'
            iid1=igl+150; iid2=igb+30;
            CALL wstxpt(iid1,iid2,temp20)
            IF(IFS.GT.0)CALL winfnt(IFS)
          ENDIF
  204   CONTINUE
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
        NAP = NZNVER(icc)
        DO 1100 J = 1,NZNVER(icc)
          K = NZJVN(icc,J)
          AX(J) = VCOORD(nznog(IZ),K,1)
          AY(J) = VCOORD(nznog(IZ),K,2)
          AZ(J) = VCOORD(nznog(IZ),K,3)
          IANXT(J) = J + 1
 1100   CONTINUE

        IANXT(NZNVER(icc)) = IAPNT

C-----------------------------------------------------------------------
C Transform surface polygon to EYE co-ordinates
C Take structure A multiply by TEMAT return structure B
C TEMAT = Model to Eye Matrix
        CALL MATPOL(NAP,IAPNT,AX,AY,AZ,IANXT,TEMAT,
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
            WRITE(MLCN,'(A12)')DESC(ITHLZ)(1:12)
            if(MLCN(1:12).eq.SSMLCN(ICC)(1:12))then
              IPEN= -305
            else
              IPEN= 1
            endif
          elseif(ITHLS.eq.2)then
            if(ITHLZ.eq.1.and.SSOTF(ICC)(1:4).eq.'OPAQ')then
              IPEN= -305
            else
              IPEN= 1
            endif
          if(ITHLZ.eq.2.and.SSOTF(ICC)(1:4).ne.'OPAQ')then
              IPEN= -305
            else
              IPEN= 1
            endif
          elseif(ITHLS.eq.3)then
            if(SSMLCN(ICC)(1:4).eq.'UNKN'.or.
     &         SSOTHER(ICC,1)(1:4).eq.'UNKN')then
              IPEN= -305
            else
              IPEN= 1
            endif
          endif

C Debug.
C          write(6,*)'icc linsty ipen ithls ithlz',icc,linsty(icc),ipen,
C     &      ithls,ithlz,ISTOK

C Count the edges in case enhanced edge drawing required.
        iedge=0
        DO 300 J = 1,NB

C Multiply each vertex by ETSMAT to transform to screen coords
C ETSMAT = Eye to Screen Matrix. First move to the beginning.
          IP = IABS(IBPNT)
          CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,iix,iiy)

C If vertex are to be labeled do it in the smallest font, then
C move cursor to vertex and remember position.
          IF(ITVNO.EQ.0.AND.NAP.EQ.NBP)THEN
            IV=NZJVN(icc,IP)
            CALL VERTLBL(iix,iiy,IV,ier)
          ENDIF
          iixlast=iix   ! cast for edline call
          iiylast=iiy

          IP1 = IBNXT(IP)

 351      CONTINUE
          iedge=iedge+1
          CALL  ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,iix,iiy)
          if(IPEN.eq.1)then

C Normal (not highlighted) edge being drawn, if NAP = NBP then check
C common/G8 for hints as to alternate greys or dots.

C Debug.
C            write(6,*) 'nap nbp nbedgdup iedge j',nap,nbp,nbedgdup(icc),
C     &        iedge,j

            if(NAP.ne.NBP)then
              call eswline(iixlast,iiylast,iix,iiy)
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
                  call edline(iixlast,iiylast,iix,iiy,LD)
                  found=.true.
                endif
              endif

C If this edge is shared with a co-planer surface with the same construction
C and greys can be done reset forground to 50% grey otherwise draw black,
C unless duplicate line has already been drawn.
              if(imatshr(icc,iedge).eq.0)then
                if(.NOT.found)call eswline(iixlast,iiylast,iix,iiy)
              else
                if(greyok) call winscl('i',nifgrey-3)
                call eswline(iixlast,iiylast,iix,iiy)
                if(greyok)call winscl('-',0)
              endif
            endif
          elseif(IPEN.eq.-305)then
            call edwline(iixlast,iiylast,iix,iiy)
          else
            call eswline(iixlast,iiylast,iix,iiy)
          endif
          iixc=iixc+iix
          iiyc=iiyc+iiy
          call forceflush()

C If vertex are to be labeled do it in the smallest font and
C move cursor back to vertex and remember position.
          IF(ITVNO.EQ.0.AND.NAP.EQ.NBP)THEN
            IV=NZJVN(icc,IP1)
            CALL VERTLBL(iix,iiy,IV,ier)
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
          CALL  ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,iix,iiy)
          if(IPEN.eq.1)then

C Normal (not highlighted) edge being drawn, if NAP = NBP then check
C common/G8 for hints as to alternate greys or dots.
            if(NAP.ne.NBP)then
              call eswline(iixlast,iiylast,iix,iiy)
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
                  call edline(iixlast,iiylast,iix,iiy,LD)
                  found=.true.
                endif
              endif

C If this edge is shared with a co-planer surface with the same construction
C and greys can be done reset forground to 50% grey otherwise draw black,
C unless duplicate line has already been drawn.
              if(imatshr(icc,iedge).eq.0)then
                if(.NOT.found)call eswline(iixlast,iiylast,iix,iiy)
              else
                if(greyok)call winscl('i',nifgrey-3)
                call eswline(iixlast,iiylast,iix,iiy)
                if(greyok)call winscl('-',0)
              endif
            endif
          elseif(IPEN.eq.-305)then
            call edwline(iixlast,iiylast,iix,iiy)
          else
            call eswline(iixlast,iiylast,iix,iiy)
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
C Patch for cfg is to only label highlighted surfaces.
        IF(ITSNM.EQ.0)THEN
          IF(iixc.GT.0.AND.iiyc.GT.0.AND.(NAP.EQ.NBP))THEN
            if(icc.ne.0)then
              if(LINSTY(icc).eq.2)then
                CG(1)=SSURCOG(icc,1)
                CG(2)=SSURCOG(icc,2)
                CG(3)=SSURCOG(icc,3)
                VN(1)=SSURVN(icc,1)
                VN(2)=SSURVN(icc,2)
                VN(3)=SSURVN(icc,3)
                CALL VECTRN(VN,TEMAT,COG2,IER)
                CALL VECTRN(COG2,ETSMAT,COG3,IER)
                call u2pixel(COG3(1),COG3(2),iixc,iiyc)
                IF(IFS.GT.0)CALL winfnt(NAMEFT)
                write(stemp,'(A)')SSNAME(IC)
                call arrow(CG,VN,0.3,0.1,ipoints,'a',2)

                IF(IFS.GT.0)CALL winfnt(IFS)

C Offset the surface name depending on orientation.
                IF(SSVFC(icc).EQ.'VERT')THEN
                  iixc=iixc+5
                ELSEIF(SSVFC(icc).EQ.'CEIL')THEN
                  iixc=iixc+5
                ELSEIF(SSVFC(icc).EQ.'FLOR')THEN
                  iixc=iixc+5
                ELSE
                  iixc=iixc+5
                ENDIF

C Make some concession to the size of the font.
                CALL wstxpt(iixc,iiyc,stemp)
                IF(IFS.GT.0)CALL winfnt(IFS)
              endif
            endif
          ENDIF
        ENDIF
 1000 CONTINUE
      call forceflush()

C Loop back again to see if there is more.
      GOTO 100

      END
