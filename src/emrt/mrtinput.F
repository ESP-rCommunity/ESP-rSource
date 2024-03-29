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

C ************* EDMRTC 
C Control MRT sensor based calculations.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
      SUBROUTINE EDMRTC(ITRC,ITRU,ICOMP,IER)

#include "building.h"
#include "model.h"
#include "geometry.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/FILEP/IFIL

      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM
      COMMON/MRT2/GEOLOAD,MRTLOAD

      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL
      COMMON/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)
      COMMON /FORMF/ FF(MSM,MSM), FFS
      common /cube3/surarea(6)
      common/tmrt1/tmrt,temperr
      common/tmrt2/temp(ms),tempmrt
      common /cube5/fstgrp,fstsur,lstsur,strtgr
      integer fstgrp,fstsur,lstsur,strtgr

      DIMENSION VERT(35),radtemp(MCUB)
      LOGICAL GEOLOAD,MRTLOAD,tmrt,temperr
      CHARACTER VERT*29
      character outs*124
      integer MVERT,IVERT ! max items and current menu item

      helpinsub='mrtinput'  ! set for subroutine

C Initialise zone vertex menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      tmrt=.true.
      IUF=IFIL+1
      
C Check to see if the geometry file and viewfactor/MRT sensor 
C files have already been loaded, if so re-establish info.
      IF(GEOLOAD.AND.MRTLOAD)THEN
        CALL FILEIN(ITRC,ITRU,IER)
        strtgr = 1
        fstsur = 1
        lstsur = NSURM
      ENDIF

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      WRITE(VERT(1),'(A,A)') '  VWF : ',LVIEW(icomp)(1:21)
      WRITE(VERT(2),'(A,A)') '  geom: ',LGEOM(ICOMP)(1:21)
      WRITE(VERT(3),'(A,I2)')'  number of MRT sensors: ',NCUB(ICOMP)
      VERT(4)   ='  __________________________ '
      VERT(5)   ='b list/edit surface temps.   '
      VERT(6)   ='c list sensor details        '
      VERT(7)   ='  __________________________ '
      VERT(8)   ='d calc. sensor viewfactors   '
      VERT(9)   ='e calc. MRT using viewfactors'
      VERT(10)  ='f comfort calculations       '
      VERT(11)  ='  __________________________ '
      VERT(12)  ='> update file                '
      VERT(13)  ='? help                       '
      VERT(14)  ='- exit to main menu          '
      MVERT=14

C Help text for the menu.
      helptopic='position_dep_mrt_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
    3 CALL EMENU('  MRT sensor calculations',VERT,MVERT,IVERT)

      IF(IVERT.EQ.5)THEN

C Browse/Edit surface temperatures.
        CALL  EDSURT(ICOMP,IER)
      ELSEIF(IVERT.EQ.6)THEN

C Browse MRT sensors. 
        call edisp(iuout,' ')
        WRITE(outs,'(A,I2)') ' Number of MRT sensors: ',NCUB(ICOMP)
        call edisp(iuout,outs)
        call edisp(iuout,
     &  ' ________________________________________________________')
        call edisp(iuout,
     &  ' Sensor|     origin (m)     |     size (m)      |rotation')
        call edisp(iuout,
     &  ' name  |Xcord |Ycord |Zcord |length|width|height|degree')
        DO 10 L=1,NCUB(ICOMP)
          WRITE(outs,303)CUBN(L),XOC(L),YOC(L),ZOC(L),
     &                   DXC(L),DYC(L),DZC(L),CANG(L)
  303     FORMAT(1X,A6,3F7.2,1x,3F6.3,F6.1)
          call edisp(iuout,outs)
   10   CONTINUE
        call edisp(iuout,' ')
      ELSEIF(IVERT.EQ.8)THEN

C Perform MRT calculations. 
C Note sensors can be of mixed sizes, patch sizes are
C computed for each.
        tmrt=.true.
        IF(.NOT.MRTLOAD)THEN
          CALL USRMSG(' There is no MRT sensor definition yet.',
     &                ' Read MRT definition file first ','-')
          RETURN
        endif
        CALL CALMRT(ITRC,IUOUT,radtemp,ICOMP)
      ELSEIF(IVERT.EQ.9)THEN

C Use previous viewfactors to perform MRT calc.
        tmrt=.false.
        IF(.NOT.MRTLOAD)THEN
          CALL USRMSG(' There is no MRT sensor definition yet.',
     &      ' Read the TMRT definition file first ','W')
          RETURN
        endif

C (re)Calculate relative surfaces for rectangular body.
        do 990 icube=1,NCUB(ICOMP)
          if(IVFOK(icube).ne.1)then
            write(outs,'(3A)')' Sensor ',CUBN(icube),
     &        ' has no previous viewfactors so skipping... '
            call edisp(iuout,outs)
            goto 990
          endif
          surarea(1)=DXC(icube)*DZC(icube)
          surarea(2)=DYC(icube)*DZC(icube)
          surarea(3)=DXC(icube)*DZC(icube)
          surarea(4)=DYC(icube)*DZC(icube)
          surarea(5)=DXC(icube)*DYC(icube)
          surarea(6)=DXC(icube)*DYC(icube)
          totarea=(DXC(icube)*DYC(icube)+DXC(icube)*DZC(icube)+
     &             DYC(icube)*DZC(icube))*2.0
          do 1101 i=1,6
            surarea(i)=surarea(i)/totarea
1101      continue

C Read previous viewfactors from the sensor definition file.
C Use same subset of surfaces used in initial calculation to
C recover from FFL.
          fstsur=nsurmf-5
          lstsur=nsurmf-6
          DO 252 I = fstsur,nsurmf
            DO 250 J = 1,nsurmf
              ff(i,j)=FFL(icube,i-lstsur,j)
250         CONTINUE
252       CONTINUE

C Prior to call to temperature reset lstsur = nsur
          lstsur = nsur
          call temperature(ICOMP,icube)
          radtemp(icube)=tempmrt
990     continue
        tmrt=.true.
      ELSEIF(IVERT.EQ.10)THEN

C Comfort calculations.
        call edisp(iuout,' This facility not yet available. ')
      ELSEIF(IVERT.EQ.12)THEN

c Write the MRT definition values to MRT sensor defintion file, 
C overwrite existing info in any existing file.
        IF(.NOT.MRTLOAD)THEN
          CALL USRMSG(' ',' There is no information to save yet.','W')
          RETURN
        ENDIF
        CALL EMKMRT(LVIEW(icomp),LGEOM(icomp),NZSUR(icomp),IUF,
     &    ICOMP,'v',IER)
      ELSEIF(IVERT.EQ.13)THEN

C Produce help text for the  menu.
        CALL PHELPD('mrt control menu',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.14)THEN

C Exit to main routine but do a bit of processing if the definition
C has been completed.
        RETURN
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        GOTO 3
      ENDIF
      GOTO 92

      END 


C ************* EDSURT
C EDSURT display/edit surface temperatures.
C IER=0 OK, IER=1 problem.
      SUBROUTINE EDSURT(ICOMP,IER)
#include "building.h"
#include "geometry.h"
#include "epara.h"
#include "help.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/MRT2/GEOLOAD,MRTLOAD
      COMMON /SURPNT/ ISPNT(6,MSM), GAREA(MSM), EMISI(MS)
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL
      common/tmrt2/temp(ms),tempmrt
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      CHARACTER STMENU*30,KEY*1

      DIMENSION STMENU(30)
      LOGICAL GEOLOAD,MRTLOAD
      integer MVERT,IVERT ! max items and current menu item

      helpinsub='mrtinput'  ! set for subroutine

C Initial menu entry setup return if no file has been read in.
      IF(.NOT.GEOLOAD)RETURN
      MHEAD=1
      MCTL=3
      ILEN=nsurmf
      IPACT=CREATE
      CALL EKPAGE(IPACT)

   92 IER=0
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          ICN=IZSTOCN(ICOMP,L)
          WRITE(STMENU(M),14)KEY,L,SNAME(ICOMP,L),EMISI(L),temp(L)
   14     FORMAT(A1,I3,1X,A12,F5.2,F7.2)
        ENDIF
   10 CONTINUE
      MVERT=M+MCTL

C Set menu header text.
      STMENU(1) =   ' |No|Description |Emis |Temp '

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        STMENU(M+1)='  __________________________ '
      ELSE
        WRITE(STMENU(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      STMENU(M+2)  ='? Help                        '
      STMENU(M+3)  ='- Exit to MRT Menu            '
      IVERT=-3

C Help text for menu.
      helptopic='surf_temp_mrt_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU('  Surface temps',STMENU,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text for the  menu.
        CALL PHELPD('temp menu',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN
C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit item identified.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        helptopic='new_emissivity_dialog'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(EMISI(IFOC),' ',' New emisivity  ? ',
     &    0.01,'W',0.99,'W',0.9,'surface emis',IER,nbhelp)

        helptopic='new_temperature_dialog'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(temp(IFOC),' ',' New temperature (degC) ? ',
     &    -50.,'W',80.,'W',20.,'surface temp',IER,nbhelp)
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-2
      GOTO 3

      END 

C *********** CALMRT
      SUBROUTINE CALMRT(ITRC,ITRU,radtemp,ICOMP)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM
      COMMON /CELLS/ NCELL(MSM), NCPT(MSM), IGFACT
      COMMON /FORMF/ FF(MSM,MSM), FFS
      COMMON /VISIB/ IVIS(MSM,MSM), IARECT(MSM,MSM)
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL

      COMMON/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)
      common /cube3/surarea(6)
      common/tmrt2/temp(ms),tempmrt
      common /cube5/fstgrp,fstsur,lstsur,strtgr
      integer fstgrp,fstsur,lstsur,strtgr

C wrongrow   : Will contain the rownumber with largest error
C rowsum(i)  : will contain the sum over j  of  ff(i,j)
C radtemp    : MRT value for specific cube in geometry. [C]
C np         : number of patch-rings on bubble
      real drow,radtemp(MCUB),rowsum(MS+1)
      integer wrongrow
      CHARACTER outs*124

C     Cgfact  = Default value for griddiv. with CUBE
C     Cnpatch = Default value for patchdiv. with CUBE
C     Bgfact  = Default value for griddiv. with BODY
C     Bnpatch = Default value for patchdiv. with BODY
      INTEGER     Cgfact,  Cnpatch,    Bgfact,  Bnpatch
      PARAMETER  (Cgfact=1,Cnpatch=100,Bgfact=2,Bnpatch=50)

      do 290 icube=1,NCUB(ICOMP)

C Add a cube to the current geometry file description in common.
        call adcube(ITRC,ITRU,ICOMP,icube,IER)

C Determin surface areas for the current sensor.
        surarea(1)=DXC(icube)*DZC(icube)
        surarea(2)=DYC(icube)*DZC(icube)
        surarea(3)=DXC(icube)*DZC(icube)
        surarea(4)=DYC(icube)*DZC(icube)
        surarea(5)=DXC(icube)*DYC(icube)
        surarea(6)=DXC(icube)*DYC(icube)
        totarea=(DXC(icube)*DYC(icube)+DXC(icube)*DZC(icube)+
     &           DYC(icube)*DZC(icube))*2.0
        do 1100 i=1,6
          surarea(i)=surarea(i)/totarea
1100    continue

C Note because sensors can be of mixed sizes, patch sizes need to
C be recomputed for each. If total area is small ie. equivalent to
C a default size sensor less than 200 mm on a side then use one
C cube type patch, otherwise use body type patch.
        if (totarea.gt.0.3) then
          igfact=Bgfact
          npatch=Bnpatch
        else
          igfact=Cgfact
          npatch=Cnpatch
        endif

C Bgfact,Bnpatch Cgfact,Cnpatch are default values that are defined
C in ????? and can be changed there. (Recompile all files.)
C Remember that npatch is 4 times the number of rings on a bubble.
        write(outs,'(A,I4)')' Grid division is set to',igfact
        call edisp(iuout,outs)
        write(outs,'(A,I4)')' Patch division set to  ',npatch/4
        call edisp(iuout,outs)
        call edisp(iuout,' Reset via Calculation Parameters-option.')

c Initialize FFL(), FF() and IVIS().
        do 230 i=1,nsurmf
          do 220 j=1,nsurmf
            if(i.le.6)FFL(icube,i,j)=0.0
            ff(i,j)=0.0
            ivis(i,j)=0
220       continue
230     continue

C Setup grid within zone and Exit if error.
        CALL  GRID(ierror)
        if(ierror.ne.0)goto 1111

c With tmrt only gridpoints on the cube will be of interest
c Other gridpoints are not considdered.
        strtgr=fstgrp
        fstsur=nsurmf-5
        lstsur=nsurmf-6

        CALL  SURVIS
        call checkrec

        CALL USRMSG(' ',' Computation commences','-')

        CALL  BUBBLE

        PI = 4.0 * ATAN(1.0)
        DEN = PI* real(IGFACT*IGFACT)

        call edisp(iuout,' ')
        call edisp(iuout,' View factor information')
        WRITE(outs,2060)CUBN(icube),icube
2060    format('    From sensor ',A,' (',i3,') to the zone')
        call edisp(iuout,outs)

        rowsum(MS+1)=0.0
        DO 252 I = fstsur,nsurmf
          rowsum(i)=0.0
          DO 250 J = 1,nsurmf
            if (ncell(i).ne.0)then

C Calculate viewfactor by dividing by the number of grid cells in
C the polygon.
              ff(i,j)=ff(i,j)/(ncell(i)*PI)
            else
              ff(i,j)=ff(i,j)/den
            endif
            rowsum(i)=rowsum(i)+ff(i,j)

C Update the master list of sensor view factors.
            FFL(icube,i-lstsur,j)=ff(i,j)

250       CONTINUE
          drow=((rowsum(i)-1.0)**2)**0.5
          if ((drow).gt.rowsum(MS+1)) then
            rowsum(MS+1)=drow
            wrongrow=i
          endif
252     CONTINUE

C MRT calc reporting.
        write (outs,'(2a)')  'Surface       |',
     &     ' Cube face and MRT fraction from surface'
        call edisp(iuout,outs)
        write (outs,'(2a)') '(ID, Name)    | 1 (back),  2 (right),',
     &     '  3 (front),  4 (left),  5 (top),  6 (base),  MRT fraction'
        call edisp(iuout,outs)
        do 254 I=1,lstsur
        
C Calculate influence of each surface - total viewfactors*relative surface area(of sensor).
          total=0.
          do 2541 J=fstsur,nsurmf
            iface=J-fstsur+1
            total=total+FF(J,I)*surarea(iface)
 2541     continue
          write (outs,'(i3,1x,a,7(1X,F7.4))') I,SNAME(ICOMP,I),
     &                          (FF(J,I),J=fstsur,nsurmf),total
          call edisp(iuout,outs)  
 254    continue
        write (outs,'(4x,a,6(1X,F7.4))') ' Face total',
     &                    (rowsum(I),I=fstsur,nsurmf)
        call edisp(iuout,outs) 

C If summation is not close to unity then report.
        if (rowsum(MS+1).gt.0.01) then
          xxx=(1.0-rowsum(wrongrow))
          call edisp(iuout,' ')
          call edisp(iuout,' The sum over one row in the viewfactor ')
          call edisp(iuout,' matrix should be  equal to 1.00. ')
          write(outs,'(A,F8.4,A,I3)')' A deviation of ',xxx,
     &                            ' was encountered in ',wrongrow
          call edisp(iuout,outs)
        endif

C save MRT viewfactors....

        call temperature(ICOMP,icube)
        radtemp(icube)=tempmrt
        IVFOK(icube)=1

C Remove this sensor in preparation for next calculation.
        NTVM=NTVM-8
        nsurm=nsurm-6
        nsurmf=nsurm
290   continue

      RETURN

C Grid Error 
 1111 maxgrd=int(sqrt(real(mgc/nsurmf)))
      write(outs,11011) mgc
11011 format(' The maximum no of grid cells (',i4,') was exceeded.')
      call edisp(iuout,outs)
      write(outs,11012) maxgrd
11012 format(' The maximum value for this model is ',i3)
      call edisp(iuout,outs)
      RETURN

      END
     

C+++++++++++++++++++ ADCUBE
C adcube adds a MRT sensor (rectangular body) to a
C GEN type geometry, at a specified place. The
C surfaces of the sensor are defined with an opposite
C orientation to the other geometry-surfaces. Icube is the
C number of the cube for reporting purposes.

        SUBROUTINE  adcube(ITRC,ITRU,ICOMP,icube,IER)

#include "building.h"
#include "geometry.h"

      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM
      COMMON /PLNEC/ PEQN(4,MSM), PCG(3,MSM), BOX(3,2,MSM),
     &               STMAT(4,4,MSM), SRMAT(4,4,MSM)
      COMMON /SURPNT/ ISPNT(6,MSM), GAREA(MSM), EMISI(MS)
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL

      DIMENSION  IAPNT(100), APOL(1000)
      DIMENSION  CG(3),EQN(4)
      CHARACTER outs*124

C cubver: vertices of cube surfaces.

      IER=0
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      SA=SIN(CANG(ICUBE)*R)
      CA=COS(CANG(ICUBE)*R)

C Calculate the vertices of the cube and store in xm, ym, zm.
      ns=nsurm
      nv=NTVM
      NTVM=NTVM+8
      nsurm=nsurm+6
      if(NTVM.GT.MTV)then
        WRITE(outs,9005)  NTVM,MTV
 9005   FORMAT(' Number of vertices: ',I3,' > ',I3)
        call edisp(itru,outs)
        RETURN
      endif

      if(NSURM.GT.MSM)then
        WRITE(outs,9105)  nsurmf,MSM
 9105   FORMAT(' Number of surfaces: ',I3,' > ',I3)
        call edisp(itru,outs)
        return
      endif

C Assign bottom 4 points of sensor XM(NS+1) to XM(NS+4) etc.  
C anticlockwise and assign the top 4 points XM(NS+5) to XM(NS+8) 
C etc. again anticlockwise.  Point 1 is @  X0,Y0,Z0 with point 5 
C is directly above.
      XM(nv+1)=XOC(icube)
      XM(nv+2)=XM(nv+1)+(DXC(icube)*CA)
      XM(nv+3)=XM(nv+2)-(DYC(icube)*SA)
      XM(nv+4)=XM(nv+3)-(DXC(icube)*CA)
      YM(nv+1)=YOC(icube)
      YM(nv+2)=YM(nv+1)+(DXC(icube)*SA)
      YM(nv+3)=YM(nv+2)+(DYC(icube)*CA)
      YM(nv+4)=YM(nv+3)-(DXC(icube)*SA)
      DO 20 I=nv+5,nv+8
        XM(I)=XM(I-4)
        YM(I)=YM(I-4)
        ZM(I-4)=ZOC(icube)
        ZM(I)=ZOC(icube)+DZC(icube)
   20 CONTINUE

      do 240 i=nsurm-5,nsurm
        nverm(i)=4
240   continue

C Link up the surfaces of the cube such that they face the zone
C (clockwise order looking from the zone).
      jvnm((ns+1),1)=nv+2
      jvnm((ns+1),2)=nv+1
      jvnm((ns+1),3)=nv+5
      jvnm((ns+1),4)=nv+6
      jvnm((ns+2),1)=nv+3
      jvnm((ns+2),2)=nv+2
      jvnm((ns+2),3)=nv+6
      jvnm((ns+2),4)=nv+7
      jvnm((ns+3),1)=nv+4
      jvnm((ns+3),2)=nv+3
      jvnm((ns+3),3)=nv+7
      jvnm((ns+3),4)=nv+8
      jvnm((ns+4),1)=nv+1
      jvnm((ns+4),2)=nv+4
      jvnm((ns+4),3)=nv+8
      jvnm((ns+4),4)=nv+5
      jvnm((ns+5),1)=nv+6
      jvnm((ns+5),2)=nv+5
      jvnm((ns+5),3)=nv+8
      jvnm((ns+5),4)=nv+7
      jvnm((ns+6),1)=nv+4
      jvnm((ns+6),2)=nv+1
      jvnm((ns+6),3)=nv+2
      jvnm((ns+6),4)=nv+3

C These data will give the surfaces of the cube the right vertices,
C in a clockwise order looking from the outside !!!

C Rebuild the plane equations and master list of surfaces.
C as in filein.f.
      nsurmf=0
      DO 1000 I = 1,NSURM
       nsurmf = nsurmf + 1
       if(nsurmf .GT. MSM)then
         WRITE(outs,9305)  nsurmf,MS
 9305    FORMAT(' Number of surfaces: ',I3,' > ',I3)
         call edisp(itru,outs)
         RETURN
       endif

C For surface I derive values for NA NAP IAPNT APOL.
       CALL  ENCPOL(I,NA,NAP,IAPNT,APOL)

C Not used...
       CALL  SPAREA(NA,IAPNT,APOL,1,GAREA(I))

       INRP = 0
       CALL  PUTPOL(NA,NAP,IAPNT,APOL,INRP)
       ISPNT(1,nsurmf) = INRP
       CALL  MPLNEQN(ITRC,ITRU,1,1,APOL,CG,EQN,IERR)
       if(IERR .EQ. -1)then
         call edisp(itru,' Error in PLNEQN ')
         RETURN
       endif

       PEQN(1,nsurmf) = EQN(1)
       PEQN(2,nsurmf) = EQN(2)
       PEQN(3,nsurmf) = EQN(3)
       PEQN(4,nsurmf) = EQN(4)

       PCG(1,nsurmf) = CG(1)
       PCG(2,nsurmf) = CG(2)
       PCG(3,nsurmf) = CG(3)

1000  CONTINUE

      return
      END

