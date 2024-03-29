C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C read3dv.f contains the following Fortran subroutines:
C  DRAWESP: Loads the geometric description(s) from an ESP file,
C           converts into viewing coords, clips the image and draws it.

C ******************** DRAWESP 
C DRAWESP loads the geometric description(s) from an ESP file,
C converts into viewing coords, clips the image and draws it.
C ier is returned as non-zero if there has been a problem.
      SUBROUTINE DRAWESP(ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Passed parameter.
      integer ier

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL  ISTOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical greyok,found
      integer ier2,ier3,ier4,ier5,ier6  ! traps

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3)
      DIMENSION  CG(3),VN(3),ipoints(6,2)

      CHARACTER ETEXT*82
      CHARACTER temp*16,DESCRC*25,stemp*13,temp20*21
      CHARACTER MLCN*32,t13*13
      integer izsize,issize
#ifdef OSI
      integer iside,isize,ifont       ! passed to viewtext
      integer iix,iiy,iixlast,iiylast,iixc,iiyc,iid1,iid2,iid3,iid4
      integer iicol,ipixw,ipixh       ! passed to textsizeatxy
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iix,iiy,iixlast,iiylast,iixc,iiyc,iid1,iid2,iid3,iid4
      integer*8 iicol,ipixw,ipixh     ! passed to textsizeatxy
#endif

C If libraries and monitor support greyscale remember this.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Set font for displaying zone & surface names etc.
C      write(6,'(a,3i2)') 'DRAWESP IFS,ITFS,IMFS',IFS,ITFS,IMFS
      issize=4; izsize=5  ! for surfaces and zone name

      IER=0; ier2=0; ier3=0; ier4=0; ier5=0; ier6=0; ier7=0; 

C Refresh the dialogue box if terminal type 8.
      IF(MMOD.EQ.8)CALL USRMSG(' ',' ','-')

C If site grid or ground is asked for draw now.
      if(ITGRD.EQ.0)then
        CALL DSGRID(0.0,GRDIS,5,IER2)
        if(ier2.ne.0)then
          call edisp(ieout,'DRAWESP: Error in drawing grid.')
          ier=-2
        endif
      endif
      if(ITDSP.EQ.5.or.ITDSP.EQ.6)then
        call EGRNDR(IER3)
        if(ier3.ne.0)then
          call edisp(ieout,'DRAWESP: Error in drawing ground.')
          ier=-3
        endif
      endif
      IF(ITDSP.EQ.6)return

C Remind user of which project.
      WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
      iside=1; isize=1; ifont=1
      if(mmod.eq.8)then
        call viewtext(etext,iside,isize,ifont)
      else
C        call viewtextwwc(etext,iside,isize,ifont)
      endif

C Beginning of loop to read in several ESP compatible geometry files.
      IZ=0
      ISTOK=.TRUE.

  100 CONTINUE

C If the origin of the site is to be shown include this in the image
C and then go on to the normal objects. After this point ISTOK is .FALSE.
      if(ITORG.EQ.0.and.ISTOK)then
        if(mmod.eq.8)CALL ORIGESP
        ISTOK=.FALSE.
      elseif(ITORG.EQ.1.and.ISTOK)then
        ISTOK=.FALSE.
      endif

C Read in one of the selected geometry files, however if this
C matches the one currently being edited take this from the
C saved common rather than from the zone file.
      IZ=IZ+1
      IF(IZ.GT.nzg)RETURN

C If obstructions are to be drawn read the obstruction file, transform
C the obstruction description into a gen body and plot them out
C before going on to the surfaces in the zone itself. Also draw any
C visual entities included in the model (e.g. to preview what will be
C passed to Radiance).
      if(ITDSP.EQ.0.or.ITDSP.eq.5.or.ITDSP.eq.7)then
        CALL DRAWOBS(nznog(IZ),ier4)
        if(ier4.ne.0)then
          call edisp(ieout,'DRAWESP: Error drawing obstructions.')
          ier=-4
        endif
      endif
      if(ITDSP.EQ.0.or.ITDSP.eq.8)then
        CALL DRAWVIS(nznog(IZ),ier4)
        if(ier4.ne.0)then
          call edisp(ieout,'DRAWESP: Error drawing visual entities.')
          ier=-4
        endif
      endif
      if(nznog(IZ).NE.izgfoc)then
        call georead(IFIL+1,LGEOM(nznog(IZ)),nznog(IZ),1,IUOUT,IER5)
        if(ier5.ne.0)then
          call edisp(ieout,'DRAWESP: Error recovering zone geometry.')
          ier=-5
        endif
      ELSEIF(nznog(IZ).EQ.izgfoc)THEN
        if(nznog(IZ).gt.0)then
          CALL ERCZONE(nznog(IZ))
        else
          call edisp(ieout,
     &    'DRAWESP: Error recovering zone geometry from memory.')
          ier=-5
        endif
      ENDIF

C If zone names are to be displayed place near the centre of zone.
C For clarity use larger font and then restore font to normal size.
C Transform the site COG coords into eyepoint and then screen coords.
C Find the pixels required for the name and centre text.
C Note: temp character t13 is 1 char wider than zname().
      IF(ITZNM.EQ.0)THEN
        COG1(1)=ZCOG(nznog(IZ),1)
        COG1(2)=ZCOG(nznog(IZ),2)
        COG1(3)=ZCOG(nznog(IZ),3)
        CALL VECTRN(COG1,TSMAT,COG2,IER6)
        if(ier6.ne.0)then
          call edisp(ieout,'DRAWESP: Error in name coord transforms.')
          ier=-6
        endif
        call u2pixel(COG2(1),COG2(2),iix,iiy)

C Note call to textpixwidth corrupts t13 so need to
C re-create t13 after textpixwidth call.
        t13=' '
        write(t13,'(A)')zname(nznog(IZ))(1:lnzname(nznog(IZ)))
        if(mmod.eq.8)then
          call winfnt(izsize)
          call textpixwidth(t13,ipixw,ipixh)
        else
          ipixw=lnblnk(t13)*7  ! assume 7 pixels wide
          ipixh=7              ! assume 7 pixels high
        endif
        write(t13,'(A)')zname(nznog(IZ))(1:lnzname(nznog(IZ)))
        iid4 = iix - (ipixw/2)

C Check for clipping.
        call pixel2u(iid4,iiy,xx,xy)
        call CLIPPT(xx,xy,COG2(3),iclp1)
        call pixel2u(iix+(ipixw/2),iiy-ipixh,xx,xy)
        call CLIPPT(xx,xy,COG2(3),iclp2)
        if (iclp1.eq.0 .and. iclp2.eq.0) then
          iicol=0
          if(mmod.eq.8)then
            call textatxy(iid4,iiy,t13,'-',iicol)
          else
            call textatxywwc(iid4,iiy,t13,'-',iicol)
          endif
        endif
        if (mmod.eq.8) CALL winfnt(IMFS)
      ENDIF

C << how to treat errors? >>
C      IF(ier.NE.0)RETURN
      if(mmod.eq.8) call forceflush()

C If there are no surfaces yet defined then draw the vertices with 
C label and loop back to the next body.
      IF(NSUR.EQ.0.AND.NTV.GT.1)THEN
        DO 202 IV=1,NTV
          COG1(1)=X(IV); COG1(2)=Y(IV); COG1(3)=Z(IV)
          CALL VECTRN(COG1,TSMAT,COG2,IER)
          CALL CLIPPT(COG2(1),COG2(2),COG2(3),iclp) ! check clipping
          if (iclp.gt.0) goto 202
          call u2pixel(COG2(1),COG2(2),iix,iiy)
          if(mmod.eq.8) call ecirc(iix,iiy,3,1)
          CALL VERTLBL(iix,iiy,COG2(3),IV,ier)
  202   CONTINUE
        if(mmod.eq.8) call forceflush()
        GOTO 100
      ELSEIF(NSUR.GE.1.AND.NTV.GT.1)THEN

C Check to see if there are any unlinked vertices, if so then draw them
C before proceeding to draw the surfaces. For each vertex, loop through
C each surface and its vertex list and see how many hits.
        found=.false.
        DO 204 IZV=1,NTV
          IHIT=0
          DO 8792 IS=1,NSUR
            DO 8794 IV=1,NVER(IS)
              IF(IZV.EQ.JVN(IS,IV))IHIT=IHIT+1
 8794       CONTINUE
 8792     CONTINUE

C If less than 2 hits draw the vertex as an open circle, if no hits
C draw it as a solid circle.
          IF(IHIT.LT.2)THEN
            COG1(1)=X(IZV); COG1(2)=Y(IZV); COG1(3)=Z(IZV)
            CALL VECTRN(COG1,TSMAT,COG2,IER)
            CALL CLIPPT(COG2(1),COG2(2),COG2(3),iclp) ! check clipping
            if (iclp.gt.0) goto 204
            found=.true.
            call u2pixel(COG2(1),COG2(2),iix,iiy)
            IF(IHIT.EQ.0)THEN
              if(mmod.eq.8) CALL ecirc(iix,iiy,3,1)
            ELSEIF(IHIT.GT.0.AND.IHIT.LT.2)THEN
              if(mmod.eq.8) CALL ecirc(iix,iiy,3,0)
            ENDIF
            CALL VERTLBL(iix,iiy,COG2(3),IZV,ier)
          ENDIF
  204   CONTINUE
  
C If any were found then draw a legend for the meaning of the vertex
C symbols if in graphic mode.
        if(found .and. mmod.eq.8)then
          CALL winfnt(issize)
          iid1=igl+10; iid2=igb+5;
          CALL ecirc(iid1,iid2,3,1)
          write(temp,'(A)')'Unlinked vertex'
          iid1=igl+20; iid2=igb+10;
          iicol=0
          call textatxy(iid1,iid2,temp,'-',iicol)
          iid1=igl+130; iid2=igb+5;
          if(mmod.eq.8) CALL ecirc(iid1,iid2,3,0)
          write(temp20,'(A)')'Single-linked vertex'
          iid1=igl+140; iid2=igb+10;
          call textatxy(iid1,iid2,temp20,'-',iicol)
          CALL winfnt(IMFS)
        endif
        if(mmod.eq.8) call forceflush()
      ENDIF

C Continue on with any surfaces in the zone. If zone is zero then
C we are drawing grid.
      DO 1000 I=1,NSUR
        isur=I
        if(iz.ne.0)then
          icc=IZSTOCN(nznog(IZ),isur)
        else
          icc=0
        endif

C If the configuration file has been read and there is a filter for
C which surfaces should be displayed test this here. Locate the
C connection (IC) which relates to this surface if a real zone.
        IF(CFGOK)THEN
          if(iz.ne.0)then
            CALL SURADJ(nznog(IZ),I,IE,TMP,IZC,ISC,IC,DESCRC)
            IF(ITDSP.EQ.2.AND.IE.NE.0)GOTO 1000
            IF(ITDSP.EQ.3.AND.IE.NE.3)GOTO 1000
            IF(ITDSP.EQ.4.AND.(IE.EQ.0.OR.IE.EQ.3))GOTO 1000
          endif
        ENDIF

C Copy polygon for Surface I into structure A for compatibility with
C viewer format 'holes' in surfaces.
C NAP   = Number of vertex points on surface
C IANXT = Index of 'next' point
C IAPNT = Pointer to first vertex of polygon
        IAPNT = 1
        NAP = NVER(I)
        DO 1100 J = 1,NVER(I)
          K = JVN(I,J)
          if(K.gt.0)then
            AX(J) = X(K)
            AY(J) = Y(K)
            AZ(J) = Z(K)
            IANXT(J) = J + 1
          else
            AX(J) = 0.0; AY(J) = 0.0; AZ(J) = 0.0 ! if JVN malformed
          endif
 1100   CONTINUE

        IANXT(NVER(I)) = IAPNT

C Transform surface polygon to screen co-ordinates
C Take structure A multiply by TSMAT return structure B
C TSMAT = Model to Screen Matrix
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
          WRITE(MLCN,'(A)') mlcname(ITHLZ)
          if(ICC.gt.0)then
            lnssmlc=lnblnk(SMLCN(nznog(IZ),isur))
            if(MLCN(1:lnmlcname(ITHLZ)).eq.
     &         SMLCN(nznog(IZ),isur)(1:lnssmlc))then
              IPEN= -305
            else
              IPEN= 1
            endif
          else
            IPEN=1
          endif
        elseif(ITHLS.eq.2)then
          if(ICC.gt.0)then
            if(ITHLZ.eq.1.and.SOTF(nznog(IZ),isur)(1:4).eq.'OPAQ')then
              IPEN= -305
            else
              IPEN= 1
            endif
            if(ITHLZ.eq.2.and.SOTF(nznog(IZ),isur)(1:4).eq.'TRAN')then
              IPEN= -305
            else
              IPEN= 1
            endif
          else
            IPEN=1
          endif
        elseif(ITHLS.eq.3)then
          if(ICC.gt.0)then
            if(SMLCN(nznog(IZ),isur)(1:3).eq.'UNK'.or.
     &         SOTHER(nznog(IZ),isur,1)(1:3).eq.'UNK')then
              IPEN= -305
            else
              IPEN= 1
            endif
          else
            IPEN=1
          endif
        endif

C Debug...
C        write(6,*)'icc linsty ipen ithls ithlz',icc,linsty(icc),ipen,
C     &    ithls,ithlz,ISTOK

C Count the edges in case enhanced edge drawing required.
        iedge=0
        DO 300 J = 1,NB

C Move to the beginning and transform screen to pixel coords.
          IP = IABS(IBPNT)
          call u2pixel(BX(IP),BY(IP),iix,iiy)

C If vertex are to be labeled do it in the smallest font, also place
C a 2pixel square dot at the vertex so if it is embedded in a line
C it can be seen. Then move cursor to vertex and remember position.
          IF(ITVNO.EQ.0.AND.IP.LE.NAP)THEN
            IV=JVN(I,IP)
            if(mmod.eq.8)then
              call esymbol(iix,iiy,8,1)
            else
              call esymbolwwc(iix,iiy,8,1)
            endif
            CALL VERTLBL(iix,iiy,BZ(IP),IV,ier)
          ENDIF
          iixlast=iix   ! cast for edline call
          iiylast=iiy

          IP1 = IBNXT(IP)

 351      CONTINUE
          iedge=iedge+1

C Ensure edge is less than MV.
          if(iedge.gt.MV) goto 300

          call u2pixel(BX(IP1),BY(IP1),iix,iiy)
          if(IPEN.eq.1)then

C Normal (not highlighted) edge being drawn, if NAP = NBP then check
C common/G8 for hints as to alternate greys or dots.

C Debug.
C            write(6,*) 'nap nbp nbedgdup iedge j',nap,nbp,nbedgdup(icc),
C     &        iedge,j

            if(NAP.ne.NBP)then
              if(mmod.eq.8)then
                call eswline(iixlast,iiylast,iix,iiy)
              else
                call eswlinewwc(iixlast,iiylast,iix,iiy)
              endif
            elseif(icc.eq.0)then
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
          if(mmod.eq.8) call forceflush()

C If vertex are to be labeled do it in the smallest font and
C move cursor back to vertex and remember position.
          IF(ITVNO.EQ.0.AND.IP1.LE.NAP)THEN
            IV=JVN(I,IP1)
            if(mmod.eq.8)then
              call esymbol(iix,iiy,8,1)
            else
              call esymbolwwc(iix,iiy,8,1)
            endif
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
            elseif(icc.eq.0)then
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
          if(mmod.eq.8) call forceflush()
  300   CONTINUE

C If surface names are to be displayed place near the centre of polygon.
C If the surface has been clipped then do not label it. For clarity drop to
C the next smaller font for the surface name and then restore font
C to normal size.
        IF(ITSNM.EQ.0)THEN
          IF(NAP.EQ.NBP)THEN
            if(icc.ne.0)then
              if(mmod.eq.8)CALL winfnt(issize)
              write(stemp,'(A)')SNAME(nznog(IZ),isur)
              call textpixwidth(stemp,ipixw,ipixh)    ! corrupts stemp
              write(stemp,'(A)')SNAME(nznog(IZ),isur)

C If surface normals to be displayed draw an arrow and
C offset the surface name depending on orientation.
              if(itsnr.eq.0)then
                CG(1)=SURCOG(nznog(IZ),isur,1)
                CG(2)=SURCOG(nznog(IZ),isur,2)
                CG(3)=SURCOG(nznog(IZ),isur,3)
                VN(1)=SURVN(nznog(IZ),isur,1)
                VN(2)=SURVN(nznog(IZ),isur,2)
                VN(3)=SURVN(nznog(IZ),isur,3)
                CALL VECTRN(VN,TSMAT,COG2,IER)
                call u2pixel(COG2(1),COG2(2),iixc,iiyc)

C Check for clipping.
                call CLIPPT(COG2(1),COG2(2),COG2(3),iclp1)
                call pixel2u(iixc+ipixw+5,iiyc-ipixh,xx,xy)
                call CLIPPT(xx,xy,COG2(3),iclp2)

                if (iclp1.eq.0 .and. iclp2.eq.0) then
                  call arrow(CG,VN,0.3,0.1,ipoints,'a',2)
                  IF(SVFC(nznog(IZ),isur).EQ.'VERT'.or.
     &              SVFC(nznog(IZ),isur).EQ.'CEIL'.or.
     &              SVFC(nznog(IZ),isur).EQ.'FLOR')THEN
                    iixc=iixc+5
                  ELSE
                    iixc=iixc+5
                  ENDIF
                  iicol=0
                  if(mmod.eq.8)then
                    call textatxy(iixc,iiyc,stemp,'-',iicol)
                  else
                    call textatxywwc(iixc,iiyc,stemp,'-',iicol)
                  endif
                endif
              else
                CG(1)=SURCOG(nznog(IZ),isur,1)
                CG(2)=SURCOG(nznog(IZ),isur,2)
                CG(3)=SURCOG(nznog(IZ),isur,3)
                CALL VECTRN(CG,TSMAT,COG2,IER)
                call u2pixel(COG2(1),COG2(2),iixc,iiyc)                
                IF(SVFC(nznog(IZ),isur).EQ.'VERT')THEN

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
                    else
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                    endif
                    iid1=iixc+3; iid2=iiyc+3
                    if(mmod.eq.8)then
                      call eswline(iixc,iiyc,iid1,iid2)
                    else
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                    endif
                    iid1=iixc+7
                    if(mmod.eq.8)then
                      call eswline(iixc,iiyc,iid1,iiyc)
                    else
                      call eswlinewwc(iixc,iiyc,iid1,iiyc)
                    endif
                    iixc=iixc+7; iiyc=iiyc+3
                    found=.true.
                  else
                    found=.false.
                  endif
                ELSEIF(SVFC(nznog(IZ),isur).EQ.'CEIL')THEN
                  
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
                    else
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                    endif
                    iid1=iixc-3; iid2=iiyc-3
                    if(mmod.eq.8)then
                      call eswline(iixc,iiyc,iid1,iid2)
                    else
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                    endif
                    iid1=iiyc-5
                    if(mmod.eq.8)then
                      call eswline(iixc,iiyc,iixc,iid1)
                    else
                      call eswlinewwc(iixc,iiyc,iixc,iid1)
                    endif
                    iid1=iiyc-5; iid2=iixc+7; iid3=iiyc-5
                    if(mmod.eq.8)then
                      call eswline(iixc,iid1,iid2,iid3)
                    else
                      call eswlinewwc(iixc,iid1,iid2,iid3)
                    endif
                    iixc=iixc+7
                    found=.true.
                  else
                    found=.false.
                  endif
                ELSEIF(SVFC(nznog(IZ),isur).EQ.'FLOR')THEN

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
                    else
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                    endif
                    iid1=iixc-3; iid2=iiyc+3
                    if(mmod.eq.8)then
                      call eswline(iixc,iiyc,iid1,iid2)
                    else
                      call eswlinewwc(iixc,iiyc,iid1,iid2)
                    endif
                    iid1=iiyc+5
                    if(mmod.eq.8)then
                      call eswline(iixc,iiyc,iixc,iid1)
                    else
                      call eswlinewwc(iixc,iiyc,iixc,iid1)
                    endif
                    iid1=iiyc+5; iid2=iixc+7; iid3=iiyc+5
                    if(mmod.eq.8)then
                      call eswline(iixc,iid1,iid2,iid3)
                    else
                      call eswlinewwc(iixc,iid1,iid2,iid3)
                    endif
                    iixc=iixc+7; iiyc=iiyc+8
                    found=.true.
                  else
                    found=.false.
                  endif
                ELSE

C Check for clipping.
                  call CLIPPT(COG2(1),COG2(2),COG2(3),iclp1)
                  call pixel2u(iixc+ipixw,iiyc-ipixh,xx,xy)
                  call CLIPPT(xx,xy,COG2(3),iclp2)
                  if (iclp1.eq.0 .and. iclp2.eq.0) then
                    found=.true.
                  else
                    found=.false.
                  endif
                ENDIF
                if (found) then
                  iicol=0
                  if(mmod.eq.8)then
                    call textatxy(iixc,iiyc,stemp,'-',iicol)
                  else
                    call textatxywwc(iixc,iiyc,stemp,'-',iicol)
                  endif
                endif
                if (mmod.eq.8) CALL winfnt(IMFS)
              endif
            endif
          ENDIF
        ENDIF
        if(mmod.eq.8) call forceflush()
 1000 CONTINUE

C Loop back again to see if there is more.
      GOTO 100

      END
