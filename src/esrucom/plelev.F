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

C PLELEV: Scales and draws a site plan/elev showing zone & obstructions.

C ************* PLELEV
C PLELEV Scales and draws a plan or elevation of a model
C and the surrounding obstruction blocks. direc is
C a character S, E, P,  ie. S gives a south elevation and
C P gives a plan.  Inpic is the number of zones, ivals is a
C array of selected zones to include.
C IUF is file unit for geometry, utility file units.
      SUBROUTINE PLELEV(IUF,direc,ier)
#include "building.h"

C geometry.h provides commons G0 and G2.
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      CHARACTER zname*12,zdesc*64,sstr*90,blabel*4,direc*1
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character tz*13
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym    ! passed to etplot
#endif
      logical newgeo  ! to use for testing if new/old geometry file.
      newgeo=.false.  ! assume older format geometry.

      IER=0
      IF(MMOD.LT.8)THEN
        CALL USRMSG('Not in graphic mode...',' ','W')
        RETURN
      ENDIF

C Define a viewing box, draw its border after clearing it and
C return the offsets which define it. Recalculate igw and igwh
C on fortran to address fault in 64 bit win3d code.
      call startbuffer()
      if(menuchw.ne.0)then
        CALL win3d(menuchw,7,4,3,3,igl,igr,igt,igb,igw,igwh)
      else
        CALL win3d(32,7,4,3,3,igl,igr,igt,igb,igw,igwh)
      endif
      igw=igr-igl
      igwh=igb-igt
      igwid=igw
      igheight=igwh
      
C Reopen the dialogue box.
      CALL USRMSG(' ',' ','-')

C Depending on plan or elevation views:
C Determine scaling ratios for the drawing and axis.
C Place scaling in static C variables for use by line drawing routines.
C Draw a scale horizontal axis, move to beginning point.
C Draw a scale vertical axis.
      if(direc.eq.'P')then
        CALL axiscale(igwid,igheight,XMN,XMX,YMN,YMX,xsc,ysc,sca,
     &    Xadd,Yadd)
        call linescale(igl,Xadd,sca,igb,Yadd,sca)
        CALL horaxis(XMN,XMX,igl,igr,igb,Xadd,sca,1,'East (m) ->')
        CALL vrtaxis(YMN,YMX,igl,igb,igt,Yadd,sca,1,0,'^ North')
      elseif(direc.eq.'S')then
        CALL axiscale(igwid,igheight,XMN,XMX,ZMN,ZMX,xsc,ysc,sca,
     &    Xadd,Yadd)
        call linescale(igl,Xadd,sca,igb,Yadd,sca)
        call horaxis(XMN,XMX,igl,igr,igb,Xadd,sca,1,'East (m) ->')
        call vrtaxis(ZMN,ZMX,igl,igb,igt,Yadd,sca,1,0,'^ Up')
      elseif(direc.eq.'E')then
        CALL axiscale(igwid,igheight,YMN,YMX,ZMN,ZMX,xsc,ysc,sca,
     &    Xadd,Yadd)
        call linescale(igl,Xadd,sca,igb,Yadd,sca)
        call horaxis(YMN,YMX,igl,igr,igb,Xadd,sca,1,'North (m) ->')
        call vrtaxis(ZMN,ZMX,igl,igb,igt,Yadd,sca,1,0,'^ Up')
      endif

C Loop through each of the selected zones, read geometry
C file and plot it. Move to first vertex and give the zone name.
      newpic=nzg
      do 54 mz=1,newpic
        newfoc=nznog(mz)
        call eclose(gversion(newfoc),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IUF,LGEOM(newfoc),newfoc,0,0,IUOUT,IER)
        else
          call egomin(IUF,LGEOM(newfoc),newfoc,0,0,IUOUT,IER)
        endif

C Check for a zone obstructions file.
        if(IOBS(newfoc).EQ.1)then
          CALL EGOMST(IUF,ZOBS(newfoc),0,0,iuout,IER)
        elseif(IOBS(newfoc).EQ.2)then
          continue
        endif

        if(direc.eq.'P')then
          call u2pixel(ZCOG(newfoc,1),ZCOG(newfoc,2),ix,iy)
          iupdown=0
          isym=0
          call etplot(ZCOG(newfoc,1),ZCOG(newfoc,2),iupdown,isym)
        elseif(direc.eq.'S')then
          call u2pixel(ZCOG(newfoc,1),ZCOG(newfoc,3),ix,iy)
          iupdown=0
          isym=0
          call etplot(ZCOG(newfoc,1),ZCOG(newfoc,3),iupdown,isym)
        elseif(direc.eq.'E')then
          call u2pixel(ZCOG(newfoc,2),ZCOG(newfoc,3),ix,iy)
          iupdown=0
          isym=0
          call etplot(ZCOG(newfoc,2),ZCOG(newfoc,3),iupdown,isym)
        endif
        IF(lnblnk(zname(newfoc)).GT.1)THEN
          write(tz,'(A)')zname(newfoc)
          call winfnt(ITFS)
          call wstxpt(ix+2,iy-2,tz)
        ELSE
          call winfnt(ITFS)
          call wstxpt(ix+2,iy-2,'Target')
        endif

        DO 400 I=1,NSUR
          J=JVN(I,1)
          iupdown=0
          isym=0
          if(direc.eq.'P')call etplot(X(J),Y(J),iupdown,isym)
          if(direc.eq.'S')call etplot(X(J),Z(J),iupdown,isym)
          if(direc.eq.'E')call etplot(Y(J),Z(J),iupdown,isym)

          K=NVER(I)
          DO 50 L=2,K
            M=JVN(I,L)
            iupdown=-302
            isym=0
            if(direc.eq.'P')call etplot(X(M),Y(M),iupdown,isym)
            if(direc.eq.'S')call etplot(X(M),Z(M),iupdown,isym)
            if(direc.eq.'E')call etplot(Y(M),Z(M),iupdown,isym)
   50     CONTINUE
          iupdown=-302
          isym=0
          if(direc.eq.'P')call etplot(X(J),Y(J),iupdown,isym)
          if(direc.eq.'S')call etplot(X(J),Z(J),iupdown,isym)
          if(direc.eq.'E')call etplot(Y(J),Z(J),iupdown,isym)
  400   CONTINUE
        call forceflush()

C Now draw each obstruction block by first converting to GEN (COMMON G1)
C and plotting.
        if(NB.eq.0)goto 54
        DO 420 IB= 1,NB
          CALL ERECC(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),BANG(IB))
          DO 422 I=1,NSUR
            J=JVN(I,1)
            iupdown=0
            isym=0
            if(direc.eq.'P')call etplot(X(J),Y(J),iupdown,isym)
            if(direc.eq.'S')call etplot(X(J),Z(J),iupdown,isym)
            if(direc.eq.'E')call etplot(Y(J),Z(J),iupdown,isym)
            K=NVER(I)
            DO 424 L=2,K
              M=JVN(I,L)
              iupdown=1
              isym=0
              if(direc.eq.'P')call etplot(X(M),Y(M),iupdown,isym)
              if(direc.eq.'S')call etplot(X(M),Z(M),iupdown,isym)
              if(direc.eq.'E')call etplot(Y(M),Z(M),iupdown,isym)
  424       CONTINUE
            iupdown=1
            isym=0
            if(direc.eq.'P')call etplot(X(J),Y(J),iupdown,isym)
            if(direc.eq.'S')call etplot(X(J),Z(J),iupdown,isym)
            if(direc.eq.'E')call etplot(Y(J),Z(J),iupdown,isym)
  422     CONTINUE
          if(direc.eq.'P')call u2pixel(X(J),Y(J),ix,iy)
          if(direc.eq.'S')call u2pixel(X(J),Z(J),ix,iy)
          if(direc.eq.'E')call u2pixel(Y(J),Z(J),ix,iy)
          WRITE(blabel,423)IB
  423     FORMAT('B',I2)
          call wstxpt(ix+4,iy-4,blabel)
  420   CONTINUE
  54  continue
      call forceflush()

C If there is a ground, ITDSP is flagged and plan view draw it.
      IF(ITDSP.EQ.5.and.NGT.gt.0)then
        if(direc.eq.'P')then
          DO 401 I=1,NGT
            J=JGVN(I,1)
            iupdown=0
            isym=0
            call etplot(XGT(J),YGT(J),iupdown,isym)
            K=NGVER(I)
            iupdown=-203
            isym=0
            DO 501 L=2,K
              M=JGVN(I,L)
              call etplot(X(M),Y(M),iupdown,isym)
  501       CONTINUE
            call etplot(X(J),Y(J),iupdown,isym)
  401     CONTINUE
        endif
      endif

C Print labels and return to default font.
      call winfnt(IFS)
      if(direc.eq.'P')then
        WRITE(sstr,'(a,a)')'Site Plan of: ',LSNAM(1:lnblnk(LSNAM))
      elseif(direc.eq.'S')then
        WRITE(sstr,'(a,a)')'South elev of: ',LSNAM(1:lnblnk(LSNAM))
      elseif(direc.eq.'E')then
        WRITE(sstr,'(a,a)')'East elev of: ',LSNAM(1:lnblnk(LSNAM))
      endif
      iy= igt-10
      ix = igl
      call wstxpt(ix,iy,sstr)
      call forceflush()

C If izgfoc is not zero and nzg is one and there is an obstructions
C file then reload current zone geometry file before returning.
C << this may not be necessary if newgeo is true >>
      if(nzg.eq.1.and.izgfoc.gt.0)then
        newfoc=izgfoc
        IF(IOBS(newfoc).EQ.1)THEN
          call eclose(gversion(newfoc),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IUF,LGEOM(newfoc),newfoc,0,0,IUOUT,IER)
          else
            call egomin(IUF,LGEOM(newfoc),newfoc,0,0,IUOUT,IER)
          endif
        endif
      endif
      RETURN
      END
