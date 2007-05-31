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

C GRAPH.F contains the following drawing routines for ESRUSHD:

C GRAPH:  Displays surface and superimposes the shaded areas.
C FACDRW: Draws the elevation of a surface.
C SHDDRW: Plots point on a surface to represent shaded portion.
C SITPLN: Scales and draws the site plan showing zone & obstructions.
C sunplt: draws a mercator sun path diagram.

C This code makes use of library facilities from esrulib.a .

C ************* GRAPH *************************
C GRAPH Displays selected body surface and superimposes the shaded
C areas.
      SUBROUTINE GRAPH(ICOMP)
      INCLUDE 'building.h'

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/HEADL/IRECS(MS)
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      CHARACTER BLKNAME*8,BLKMAT*12,outs*124,h*72
      logical OK,dok

      DIMENSION IOSHD(MOX,MOZ),IVA(MS)

C Assume there are no default windows.
      ngldum=0

      IUNIT=IFIL

   23 CALL PICKSSUR(ICOMP,NP,'L',IVA,IER)
      if(NP.eq.0)then
        goto 5
      else
        IS=IVA(1)
      endif

C Retrieve surface transformed coordinates.  Note XFT & ZFT could be 
C recalculated quite easily.
      NV=NVER(IS)
      IREC=IRECS(IS)
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(XFT(I),I=1,NV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(ZFT(I),I=1,NV)
      IREC=IREC+1

C Set up mesh centre points in case of first entry.
      CALL MESH(IS)

C The right offset for the graphic box is the character width of the menu.
      IER=0
      IF(MMOD.LT.8)THEN
        CALL USRMSG('Not in graphic mode...',' ','W')
        RETURN
      ENDIF

C Clear the vieweing box, draw its border and return the offsets
C which define it.  The win3d parameters match those in FACDRW
C and SITPLN.
      call startbuffer()
      CALL win3d(menuchw,7,4,3,3,igl,igr,igt,igb,igw,igwh)

C Update the equation of the plane, its transforms etc in COMMON/TRNSF
C and COMMON/G1T.
      CALL TRNSF1(IS)

C Find MAX & MIN surface coordinates.
      XMAX=-1.E+8
      ZMAX=-1.E+8
      ZMIN=1.E+8
      XMIN=1.E+8
      DO 10 I=1,NV
        XMAX=AMAX1(XMAX,XFT(I))
        ZMAX=AMAX1(ZMAX,ZFT(I))
        XMIN=AMIN1(XMIN,XFT(I))
        ZMIN=AMIN1(ZMIN,ZFT(I))
   10 CONTINUE

C Reopen the dialogue box.
      IF(MMOD.EQ.8)CALL usrmsg(' ',' ','-')

C Determine scaling ratios for the drawing and axis.
      CALL axiscale(igw,igwh,XMIN,XMAX,ZMIN,ZMAX,xsc,ysc,sca,
     &              Xadd,Zadd)

C Place scaling in static C variables for use by line drawing routines.
      call linescale(igl,Xadd,sca,igb,Zadd,sca)

C Output start hour.
      IH=1
      CALL EASKI(IH,' ',' Output start hour (1-24) ? ',
     &     1,'F',24,'F',1,'output start',IER,0)

C Hour-by-hour.
      DO 20 J=1,24
      JJ=J
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)ISUNUP
      IREC=IREC+1
      IF(ISUNUP.EQ.0)goto 20

C Is surface totally shaded, totally insolated or mixed?
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)INDS
      IREC=IREC+1
      IF(INDS.EQ.0)goto 12
      IF(INDS.EQ.-1)goto 13

C No shading case.
      IF(J.LT.IH)goto 20
      CALL FACDRW(JJ,ICOMP,IS,NV,XMIN,XMAX,ZMIN,ZMAX,Xadd,Zadd,SCA)
      goto 14

C Fully shaded case.
   13 IF(J.LT.IH)goto 20
      CALL FACDRW(JJ,ICOMP,IS,NV,XMIN,XMAX,ZMIN,ZMAX,Xadd,Zadd,SCA)
      CALL SHDDRW(IS,1,IOSHD)
      goto 14

C Mixed shading/insolation case.
   12 IF(J.LT.IH)goto 15
      goto 16
   15 IREC=IREC+NOX+NGLDUM*NGX/2
      IF((NGX/2)*2.NE.NGX.AND.NGLDUM.GT.0)IREC=IREC+1
      goto 20
   16 DO 30 K=1,NOX
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(IOSHD(K,L),L=1,NOZ)
      IREC=IREC+1
   30 CONTINUE
      IREC=IREC+NGLDUM*NGX/2
      IF((NGX/2)*2.NE.NGX.AND.NGLDUM.GT.0)IREC=IREC+1

      CALL FACDRW(JJ,ICOMP,IS,NV,XMIN,XMAX,ZMIN,ZMAX,Xadd,Zadd,SCA)
      CALL SHDDRW(IS,2,IOSHD)

   14 CONTINUE
      dok=.true.
      h(1)='Pause while you consider the data. '
      CALL ASKOK(' ',' Continue?',OK,dok,1)
      if(.NOT.OK)then
        dok=.true.
        h(1)='Option to look at additional surfaces. '
        h(2)='If you are done say no. '
        CALL ASKOK(' ',' Consider another surface?',OK,dok,2)
        IF(OK)goto  23
        goto 19
      endif
   20 CONTINUE
    5 CONTINUE

      dok=.true.
      h(1)='Option to look at additional surfaces. '
      h(2)='If you are done say no. '
      CALL ASKOK(' ',' Consider another surface ?',OK,dok,2)
      IF(OK)goto 23
   19 RETURN

 1001 WRITE(outs,25)IREC
   25 FORMAT(' Transitional shading file error in GRAPH - record',I4)
      call edisp(iuout,outs)
      goto 19
      END

C ************* FACDRW *****************
C FACDRW draws the elevation of the surface.
      SUBROUTINE FACDRW(IHOUR,ICOMP,ISUR,NV,XMIN,XMAX,ZMIN,ZMAX,
     &            Xadd,Zadd,SCA)
      INCLUDE 'building.h'

      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/G1T/XFT(MV),ZFT(MV)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS), SMLCN(MS), SVFC(MS),SOTHER(MS)
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      DIMENSION MTHNAM(12)

      CHARACTER*72 ETEXT
      CHARACTER*3 MTHNAM
      CHARACTER SNAME*12,SOTF*4,SVFC*4,SOTHER*15,SMLCN*12
      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C Display header information.
      CALL win3d(menuchw,7,4,3,3,igl,igr,igt,igb,igw,igwh)

      WRITE(ETEXT,1)MTHNAM(MON),IHOUR,ISUR,SNAME(ICOMP,ISUR)
    1 FORMAT(' Shading patterns: ',A3,' at hour',I3,' for surface',I3,
     &       ' ',a)
      CALL viewtext(ETEXT,1,1,2)

C Draw a scale horizontal axis, move to beginning point.
      CALL horaxis(XMIN,XMAX,igl,igr,igb,Xadd,sca,0,'X axis ->')

C Draw a scale vertical axis.
      CALL vrtaxis(ZMIN,ZMAX,igl,igb,igt,Zadd,sca,0,0,'^ Z axis')


C DRAW ZONE FACE.
      call etplot(XFT(1),ZFT(1),0,0)

      DO 10 I=2,NV
        call etplot(XFT(I),ZFT(I),1,0)
   10 CONTINUE
      call etplot(XFT(1),ZFT(1),1,0)
      call forceflush()

      RETURN
      END

C ************* SHDDRW *****************
C SHDDRW plots point on a surface to represent shaded portion.
C ISHAD=1 100 PERCENT SHADING. ISHAD=2 MIXED SHADING/INSOLATION CASE.
C << 100% case should test for point within surface >>.
      SUBROUTINE SHDDRW(IS,ISHAD,IOSHD)
      INCLUDE 'building.h'

      COMMON/GRID11/XOP(MOX,MOZ),ZOP(MOX,MOZ)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      DIMENSION IOSHD(MOX,MOZ)
      CHARACTER BLKNAME*8,BLKMAT*12

      DO 10 I=1,NOX
        DO 20 J=1,NOZ
          IF(ISHAD.EQ.1)then
            CALL POINT1(IS,XOP(I,J),ZOP(I,J),IANS)
            IF(IANS.EQ.0)goto 20
            goto 1
          endif
          IF(IOSHD(I,J).EQ.1)goto 1
          goto 20
    1     call u2pixel(XOP(I,J),ZOP(I,J),ix,iy)
          call edwline(ix-1,iy,ix+1,iy)
   20   CONTINUE
   10 CONTINUE
      call forceflush()

      RETURN
      END

C ************* SITPLN *****************
C SITPLN Scales and draws the site plan showing the object boundary
C and the surrounding obstruction blocks.
      SUBROUTINE SITPLN(ICOMP)
      INCLUDE 'building.h'

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),ANG(MB)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC8/SLAT,SLON
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GFONT/IFS,ITFS,IMFS

      CHARACTER zname*12,sstr*72,blabel*4,BLKNAME*8,BLKMAT*12
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character tz*13,zdesc*64

C The right offset for the graphic box is the character width
C of the menu. Also allow 2 text lines at the bottom for future
C dialogue. Axiscale is in wwlib.c.
      IER=0
      IF(MMOD.LT.8)THEN
        CALL USRMSG('Not in graphic mode...',' ','W')
        RETURN
      ENDIF

C Define a viewing box, draw its border after clearing it and
C return the offsets which define it.
      CALL win3d(menuchw,7,4,3,3,igl,igr,igt,igb,igw,igwh)
      call startbuffer()

C Reopen the dialogue box.
      CALL USRMSG(' ',' ','-')

C Check the max & min of the zone in question, first re-read
C geometry file.
      CALL EGOMIN(IFIL+1,LGEOM(ICOMP),ICOMP,0,0,IUOUT,IER)
      XMX=-1.E+8
      YMX=-1.E+8
      XMN=1.E+8
      YMN=1.E+8
      DO 302 I=1,NTV
        XMN=AMIN1(XMN,X(I))
        YMN=AMIN1(YMN,Y(I))
        XMX=AMAX1(XMX,X(I))
        YMX=AMAX1(YMX,Y(I))
  302 CONTINUE

C Check for max and min site coordinates. First convert obstruction
C block geometry into GEN (via common G1).
      DO 301 IB=1,MB
        CALL ERECC(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),ANG(IB))
        DO 300 I=1,NTV
          XMN=AMIN1(XMN,X(I))
          YMN=AMIN1(YMN,Y(I))
          XMX=AMAX1(XMX,X(I))
          YMX=AMAX1(YMX,Y(I))
  300   CONTINUE
  301 CONTINUE

C Determine scaling ratios for the drawing and axis.
      CALL axiscale(igw,igwh,XMN,XMX,YMN,YMX,xsc,ysc,sca,
     &              Xadd,Yadd)

C Place scaling in static C variables for use by line drawing routines.
      call linescale(igl,Xadd,sca,igb,Yadd,sca)

C Draw a scale horizontal axis, move to beginning point.
      CALL horaxis(XMN,XMX,igl,igr,igb,Xadd,sca,1,'East (m) ->')

C Draw a scale vertical axis.
      CALL vrtaxis(YMN,YMX,igl,igb,igt,Yadd,sca,1,0,'^ North')

C Re-read in the geometry file and plot it. Move to first vertex and
C give the zone name.
      CALL EGOMIN(IFIL+1,LGEOM(ICOMP),ICOMP,0,0,IUOUT,IER)

      call u2pixel(X(1),Y(1),ix,iy)
      call etplot(X(1),Y(1),0,0)
      IF(LNBLNK(zname(ICOMP)).GT.1)THEN
        write(tz,'(A)')zname(ICOMP)
        call winfnt(ITFS)
        call wstxpt(ix+2,iy-2,tz)
      ELSE
        call winfnt(ITFS)
        call wstxpt(ix+2,iy-2,'Target')
      ENDIF

      DO 400 I=1,NSUR
        J=JVN(I,1)
        call etplot(X(J),Y(J),0,0)

        K=NVER(I)
        DO 50 L=2,K
          M=JVN(I,L)
          call etplot(X(M),Y(M),-302,0)
   50   CONTINUE
          call etplot(X(J),Y(J),-302,0)
  400 CONTINUE

C Now draw each obstruction block by first converting to GEN (COMMON G1)
C and plotting.
      if(NB.eq.0)goto 421
      DO 420 IB= 1,NB
        CALL ERECC(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),ANG(IB))
        DO 422 I=1,NSUR
          J=JVN(I,1)
          call etplot(X(J),Y(J),0,0)
          K=NVER(I)
          DO 424 L=2,K
            M=JVN(I,L)
            call etplot(X(M),Y(M),1,0)
  424     CONTINUE
          call etplot(X(J),Y(J),1,0)
  422   CONTINUE

        call u2pixel(X(J),Y(J),ix,iy)
        WRITE(blabel,423)IB
  423   FORMAT('B',I2)
        call wstxpt(ix+2,iy-2,blabel)
  420 CONTINUE


C Print labels and return to default font.
  421 call winfnt(IFS)
      WRITE(sstr,425)SLAT,SLON
  425 FORMAT(' Site Plan @ latitude ',F5.1,' & longitude ',F4.1)
      iy= igt
      ix = igl
      call wstxpt(ix,iy,sstr)
      call forceflush()

      RETURN
      END

C ********** sunplt
C This routine draws a mercator sun path diagram
      SUBROUTINE SUNPLT(IJDAY,IER)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/PREC8/SLAT,SLON
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      DIMENSION MTHNAM(12)

      CHARACTER ETEXT*72,BLAB*3,MTHNAM*3
      logical ABOVE

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C Clear the vieweing box, draw its border and return the offsets
C which define it.
      CALL win3d(menuchw,8,8,4,3,igl,igr,igt,igb,igw,igwh)

C Reopen the dialogue box.
      IF(MMOD.EQ.8)CALL usrmsg(' ',' ','-')

C Determine scaling ratios for the drawing and axis.
      CALL axiscale(igw,igwh,0.,360.,0.,90.,xsc,ysc,sca,
     &              Xadd,Yadd)

C Place scaling in static C variables for use by line drawing routines.
      call linescale(igl,Xadd,xsc,igb,Yadd,ysc)

C Draw a scale horizontal axis.
      CALL horaxis(0.,360.,igl,igr,igb,Xadd,xsc,0,
     &            'Azimuth (0=North, 180=South)')

C Draw a scale vertical axis on left and right.
      CALL vrtaxis(0.,90.,igl,igb,igt,Yadd,ysc,0,0,'Altitude')
      CALL vrtaxis(0.,90.,igr,igb,igt,Yadd,ysc,0,1,':')

C Generate a heading for the view.
      CALL EDAYR(IJDAY,IDO,IMO)
      CALL viewtext('MERCATOR SUN PLOT',2,1,IFS)
      WRITE(ETEXT,'(A,I2,1X,A3,A,F6.1,A,F5.1)')
     &   ' Date: ',IDO,MTHNAM(IMO),' @ lat:',slat,' long:',slon
      CALL viewtext(ETEXT,3,1,IFS)

C Plot sun path, for each hour of the day get the azimuth and
C elevation and plot that position on the graph with a symbol and
C time indication.
      ABOVE=.FALSE.
      DO 2100 IHR=1,24
        STIME=FLOAT(IHR)
        SAZI=0.0
        SALT=0.0
        CALL EAZALTS(STIME,IJDAY,SLAT,SLON,ISUNUP,SAZI,SALT)
        IF(SALT.GT.0)then
          if(.NOT.ABOVE)then
            call etplot(SAZI,SALT,0,0)
            ABOVE=.TRUE.
          endif
          ICLKHR=IHR
          IF(IHR.GT.12)ICLKHR=IHR-12
          IF(ICLKHR.GT.9)then
            write(BLAB,'(I2)')ICLKHR
            call etlabel(BLAB,SAZI,SALT,4,ITFS)
          else
            write(BLAB,'(I1)')ICLKHR
            call etlabel(BLAB,SAZI,SALT,4,ITFS)
          endif

C Plot a symbol at the positon of the sun.
          call etplot(SAZI,SALT,1,6)
        endif
 2100 CONTINUE
      call forceflush()

      RETURN
      END

