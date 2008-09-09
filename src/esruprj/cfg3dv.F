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
C ITRC = trace level, IER=0 OK, IER=1 problem. If izgfoc is not 0
C then it represents the zone which is currently being edited.
      SUBROUTINE CADJVIEW(ITRC,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      LOGICAL MODIFY,MODLEN,MODBND

      SMALL=0.01

C If not in graphic mode return.
      if(MMOD.lt.8)return

C Tell wireframe control the current number of selected zones.
      call updwire(nzg)

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
        CALL CDRAWESP(ITRC,IER)

C Recover information begin edited and reset modify flag.
        if(izgfoc.NE.0)CALL ERCZONE(izgfoc)
        MODIFY=.FALSE.
        MODLEN=.FALSE.
        MODBND=.FALSE.
      ENDIF

      RETURN
      END

C ******************** CDRAWESP 
C CDRAWESP loads the geometric description(s) from an ESP file,
C converts into viewing coords, clips the image and draws it.
      SUBROUTINE CDRAWESP(ITRC,ier)
#include "building.h"
      PARAMETER  (MPL = 200 )

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      common/G8/nbedgdup(MCON),iedgdup(MCON,MV),nbedgshr(MCON),
     &          iedgshr(MCON,MV),imatshr(MCON,MV)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY6/LINSTY(MCON)
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C VCOORD : X,Y & Z coordinates of each vertice in zone.
C NZNVER : number of vertices associated with each connection.
C NZJVN : topology of vertices associated with each connection.
      COMMON/ZNDATA/VCOORD(MCOM,MTV,3),NZNVER(MCON),NZJVN(MCON,MV)

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL  ISTOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical close,greyok,found

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3),COG3(3)
      DIMENSION  CG(3),VN(3),ipoints(6,2)

      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER temp*16,DESCRC*25,stemp*13,temp20*21
      CHARACTER DESC*48,MLCN*12,zname*12,zdesc*64

C If not in graphic mode return.
      if(MMOD.lt.8)return

C If libraries and monitor support greyscale remember this.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Save the current font and switch to a smaller font for displaying
C surface names etc. Ixc,iyc keep track of surface/pixel COG.
      NAMEFT=IFS-1

C The right offset for the graphic box is the character width of the
C menu. Also allow 2 text lines at the bottom for future dialogue.
      IER=0

C Clear current viewing box and re-establish image bounds.
      CALL startbuffer()
      CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)

C Determine scaling ratios for the drawing and axis.  If static
C assume 1000 -1000 etc, if optimum then use SXMX,SYMX + 10% etc.
      IF(ITBND.EQ.0)THEN
        CALL axiscale(igw,igwh,-1000.,1000.,-1000.,1000.,xsc,ysc,sca,
     &                Xadd,Yadd)
        call linescale(igl,Xadd,sca,igb,Yadd,sca)
      ELSEIF(ITBND.EQ.1)THEN

C Determine the 2D coords of the site extremes.
        CALL SITE2D(SXMX,SXMN,SYMX,SYMN,ier)
        CALL axiscale(igw,igwh,SXMN,SXMX,SYMN,SYMX,xsc,ysc,sca,
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
      IZ=0
      ISTOK=.TRUE.

  100 CONTINUE

C If the origin of the site is to be shown include this in the image
C and then go on to the normal objects in subsequent passes.
      if(ITORG.EQ.0.and.ISTOK)then
        CALL ORIGESP
        goto 205
      elseif(ITORG.EQ.1.and.ISTOK)then
        ISTOK=.FALSE.
      endif

      IZ=IZ+1
      IF(IZ.GT.nzg)RETURN

C If obstructions are to be drawn read obstruction file, transform
C the obstruction description into a gen body and plot them out
C before going on to the surfaces in the zone itself.
      IF(ITDSP.EQ.0.or.ITDSP.eq.5)CALL DRAWOBS(nznog(IZ),ier)

C If zone names are to be displayed place near the centre of zone.
C For clarity use larger font and then restore font to normal size.
C Transform the site COG coords into eyepoint and then screen coords.
C Find the pixels required for the name and centre text.
      IF(ITZNM.EQ.0)THEN
        COG1(1)=ZCOG(nznog(IZ),1)
        COG1(2)=ZCOG(nznog(IZ),2)
        COG1(3)=ZCOG(nznog(IZ),3)
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),ix,iy)
        write(temp,'(A)')zname(nznog(IZ))
        call textpixwidth(temp,ipixw)
        ixc = ix - (ipixw/2)
        CALL wstxpt(ixc,iy,temp)
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
          call u2pixel(COG3(1),COG3(2),ix,iy)
          CALL ecirc(ix,iy,3,1)
          CALL VERTLBL(ix,iy,IV,ier)
          call forceflush()
  202   CONTINUE
        ISTOK=.FALSE.
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
            call u2pixel(COG3(1),COG3(2),ix,iy)
            IF(IHIT.EQ.0)THEN
              CALL ecirc(ix,iy,3,1)
            ELSEIF(IHIT.GT.0.AND.IHIT.LT.2)THEN
              CALL ecirc(ix,iy,3,0)
            ENDIF
            CALL VERTLBL(ix,iy,IZV,ier)

C If less than 2 hits then draw a legend for the meaning
C of the vertex symbols.
            IF(IFS.GT.0)CALL winfnt(1)
            CALL ecirc(igl+20,igb+25,3,1)
            write(temp,'(A)')'Unlinked vertex'
            CALL wstxpt(igl+30,igb+30,temp)
            CALL ecirc(igl+140,igb+25,3,0)
            write(temp20,'(A)')'Single-linked vertex'
            CALL wstxpt(igl+150,igb+30,temp20)
            IF(IFS.GT.0)CALL winfnt(IFS)
          ENDIF
  204   CONTINUE
        call forceflush()
      ENDIF

C Continue on with any surfaces in the zone.
  205 DO 1000 I=1,NZSUR(nznog(IZ))
        icc=IZSTOCN(nznog(IZ),I)

C If the configuration file has been read and there is a filter for
C which surfaces should be displayed test this here. Locate the
C connection (IC) which relates to this surface.
        IF(CFGOK.AND.(.NOT.ISTOK))THEN
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

        ixc=0
        iyc=0

C Set line style for this surface. 
        IF(.NOT.ISTOK)THEN
          if(ICC.ne.0)then
            if(LINSTY(IC).eq.2)then
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
            if(ITHLZ.eq.2.and.SSOTF(ICC)(1:4).eq.'TRAN')then
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

C Debug...
C          write(6,*)'icc linsty ipen ithls ithlz',icc,linsty(icc),ipen,
C     &      ithls,ithlz,ISTOK
        else
          IPEN= 1
        endif

C Count the edges in case enhanced edge drawing required.
        iedge=0
        DO 300 J = 1,NB

C Multiply each vertex by ETSMAT to transform to screen coords
C ETSMAT = Eye to Screen Matrix. First move to the beginning.
          IP = IABS(IBPNT)
          CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)

C If vertex are to be labeled do it in the smallest font, then
C move cursor to vertex and remember position.
          IF(ITVNO.EQ.0.AND.NAP.EQ.NBP.AND.(.NOT.ISTOK))THEN
            IV=NZJVN(icc,IP)
            CALL VERTLBL(ix,iy,IV,ier)
          ENDIF
          ixlast=ix
          iylast=iy

          IP1 = IBNXT(IP)

 351      CONTINUE
          iedge=iedge+1
          CALL  ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          if(IPEN.eq.1)then

C Normal (not highlighted) edge being drawn, if NAP = NBP then check
C common/G8 for hints as to alternate greys or dots.
C Debug..
C            write(6,*) 'nap nbp nbedgdup iedge j',nap,nbp,nbedgdup(icc),
C     &        iedge,j
            if(NAP.ne.NBP)then
              call eswline(ixlast,iylast,ix,iy)
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
                  call edline(ixlast,iylast,ix,iy,LD)
                  write(6,*) 'duplicate edge.'
                  found=.true.
                endif
              endif

C If this edge is shared with a co-planer surface with the same construction
C and greys can be done reset forground to 50% grey otherwise draw black,
C unless duplicate line has already been drawn.
              if(imatshr(icc,iedge).eq.0)then
                if(.NOT.found)call eswline(ixlast,iylast,ix,iy)
              else
                if(greyok)call winscl('i',nifgrey-3)
                call eswline(ixlast,iylast,ix,iy)
                if(greyok)call winscl('-',0)
              endif
            endif
          elseif(IPEN.eq.-305)then
            call edwline(ixlast,iylast,ix,iy)
          else
            call eswline(ixlast,iylast,ix,iy)
          endif
          ixc=ixc+ix
          iyc=iyc+iy

C If vertex are to be labeled do it in the smallest font and
C move cursor back to vertex and remember position.
          IF(ITVNO.EQ.0.AND.NAP.EQ.NBP.AND.(.NOT.ISTOK))THEN
            IV=NZJVN(icc,IP1)
            CALL VERTLBL(ix,iy,IV,ier)
            ixlast=ix
            iylast=iy
          ENDIF

C Repeat until next point is the first, remember position.
          IP1 = IBNXT(IP1)
          ixlast=ix
          iylast=iy
          IF(IP1 .NE. IP) GOTO 351

C Complete the polygon.
          iedge=iedge+1
          CALL  ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call u2pixel(AAX,AAY,ix,iy)
          if(IPEN.eq.1)then

C Normal (not highlighted) edge being drawn, if NAP = NBP then check
C common/G8 for hints as to alternate greys or dots.
            if(NAP.ne.NBP)then
              call eswline(ixlast,iylast,ix,iy)
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
                  call edline(ixlast,iylast,ix,iy,LD)
                  write(6,*) 'duplicate edge.'
                  found=.true.
                endif
              endif

C If this edge is shared with a co-planer surface with the same construction
C and greys can be done reset forground to 50% grey otherwise draw black,
C unless duplicate line has already been drawn.
              if(imatshr(icc,iedge).eq.0)then
                if(.NOT.found)call eswline(ixlast,iylast,ix,iy)
              else
                if(greyok)call winscl('i',nifgrey-3)
                call eswline(ixlast,iylast,ix,iy)
                if(greyok)call winscl('-',0)
              endif
            endif
          elseif(IPEN.eq.-305)then
            call edwline(ixlast,iylast,ix,iy)
          else
            call eswline(ixlast,iylast,ix,iy)
          endif
          ixc=ixc+ix
          iyc=iyc+iy
  300   CONTINUE
        call forceflush()

C If surface names are to be displayed place near the centre of polygon.
C If the surface is not drawn (ixc=0 & iyc=0) do not label it. If the
C surface has been clipped then do not label it. For clarity drop to
C the next smaller font for the surface name and then restore font
C to normal size.
C Patch for cfg is to only label highlighted surfaces.
        IF(ITSNM.EQ.0.AND.(.NOT.ISTOK))THEN
          IF(ixc.GT.0.AND.iyc.GT.0.AND.(NAP.EQ.NBP))THEN
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
                call u2pixel(COG3(1),COG3(2),ixc,iyc)
                IF(IFS.GT.0)CALL winfnt(NAMEFT)
                write(stemp,'(A)')SSNAME(IC)
                call arrow(CG,VN,0.3,0.1,ipoints,'a',2)

                IF(IFS.GT.0)CALL winfnt(IFS)

C Offset the surface name depending on orientation.
                IF(SSVFC(icc).EQ.'VERT')THEN
                  ixc=ixc+5
                ELSEIF(SSVFC(icc).EQ.'CEIL')THEN
                  ixc=ixc+5
                ELSEIF(SSVFC(icc).EQ.'FLOR')THEN
                  ixc=ixc+5
                ELSE
                  ixc=ixc+5
                ENDIF

C Make some concession to the size of the font.
                CALL wstxpt(ixc,iyc,stemp)
                IF(IFS.GT.0)CALL winfnt(IFS)
              endif
            endif
          ENDIF
        ENDIF
 1000 CONTINUE
      call forceflush()

C Loop back again to see if there is more after canceling site origin
C symbol display in subsequent zones.
      ISTOK=.FALSE.
      GOTO 100

      END
