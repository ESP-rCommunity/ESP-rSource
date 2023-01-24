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

C This file contains subroutines to write and read property fields.
C Contains:
C  PRNPHI - Write PHI files (The format required by PHOTON - PHOENICS graphic analyser)
C  REAPRN - Read and write property fields in a binary sequential file.
C  SCAN3dI- scanns an (3D) array in planes of I=const 
C  SCANall- scans (3D) arrays AP, AE, AW, An, AS, AH, AL, SU, SP, AP
C  tecplotend- used for display plotting with 3rd party tools.
C  paraviewend- used for CFD postprocessing with ParaView

C ********************* PRNPHI *********************
C PRNPHI - Write the field values of varibles in ASCII format.
C  The format is the same as PHOENICS output files which can be read by
C  PHOTON (PHOENICS graphic analyser)

      SUBROUTINE PRNPHI(TITLE,ifl)
#include "building.h"
#include "cfd.h"

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz) 
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ

      DIMENSION IAUX(8),QMARK(1)

      CHARACTER*72 TITLE,FLAG2
      CHARACTER*5  FLAG1

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT
      LOGICAL IZEROT,IZanKE,IMITZ

      FLAG1=' TTFF'
      WRITE(ifl,130)TITLE
 130  FORMAT(' ',A72)
      WRITE(ifl,*)FLAG1
      NIM2=NI-2
      NJM2=NJ-2
      NKM2=NK-2
      IAUX(1)=50
      IAUX(2)=-1
      IAUX(3)=-2
      IAUX(4)=0
      IAUX(5)=0
      IAUX(6)=0
      IAUX(7)=0
      IAUX(8)=256
      WRITE(ifl,100)NIM2,NJM2,NKM2,(IAUX(I),I=1,8)
100   FORMAT(7X,I3,7X,I3,7X,I3,4(6X,I4))
      RINNER=0.0
      WRITE(ifl,110)RINNER
      WRITE(ifl,120)
120   FORMAT('** whatever **')
C120   FORMAT(' P1  P2  U1  U2  V1  V2  W1  W2  R1  R2  RS  KE  EP  H1
C     &  C1  C2  C3  C4 C5  C6  C7  C8  C9  C10 C11 C12 C13 C14 C15
C     & C16 C17 C18 C19 C20 C21 C22 C23 C24 C25 C26 C27 C28 C29 C30
C     & C31 C32 C33 C34 C35')

      WRITE(ifl,110)(XU(I),I=3,NI)
      WRITE(ifl,110)(YV(I),I=3,NJ)
      WRITE(ifl,110)(ZW(I),I=3,NK)
      DO 10 I=1,NKM2
10      QMARK(I)=0.0 
      WRITE(ifl,110)(QMARK(I),I=1,NKM2)
      FLAG2=' FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
      flag2(2:2)='T'
      if(incalu)flag2(4:4)='T'
      if(incalv)flag2(6:6)='T'
      if(incalw)flag2(8:8)='T'
      if(incalk)flag2(13:13)='T'
      if(incald)flag2(14:14)='T'
      if(incalt)flag2(15:15)='T'
      WRITE(ifl,*)FLAG2
      DO 20 K=2,NKM1
        WRITE(ifl,110)((P(I,J,K),J=2,NJM1),I=2,NIM1)
        if(incalu)WRITE(ifl,110)((Uf(I,J,K),J=2,NJM1),I=3,NI)
        if(incalv)WRITE(ifl,110)((Vf(i,j,K),J=3,NJ),I=2,NIM1)
        if(incalw)WRITE(ifl,110)((Wf(i,j,K+1),J=2,NJM1),I=2,NIM1)
        if(incalk)WRITE(ifl,110)((TEf(i,j,K),J=2,NJM1),I=2,NIM1)
        if(incald)WRITE(ifl,110)((EDf(i,j,K),J=2,NJM1),I=2,NIM1)
        if(incalt)WRITE(ifl,110)((Tf(i,j,K),J=2,NJM1),I=2,NIM1)
20    CONTINUE
110   FORMAT(6(1PE13.6))
      RETURN
      END

C ********************* REAOLD *********************
C REAOLD - Read variable fields in the previous time-step.
      subroutine REAOLD(ifl)
#include "building.h"
#include "cfd.h"

      COMMON/VARp/Up(ntcelx,ntcely,ntcelz),Vp(ntcelx,ntcely,ntcelz),
     1            Wp(ntcelx,ntcely,ntcelz),TEp(ntcelx,ntcely,ntcelz),
     2            EDp(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/FLUPRp/DENp(ntcelx,ntcely,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TEMPp/Tp(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN

      CHARACTER*6 FLAG2

      READ(ifl)NI,NJ,NK
      NIM1=NI-1
      NJM1=NJ-1
      NKM1=NK-1
      NIM2=NI-2
      NJM2=NJ-2
      NKM2=NK-2
      READ(ifl)FLAG2                   
      DO 20 K=1,NK
        READ(ifl)((P(I,J,K),J=1,NJ),I=1,NI)
        if(flag2(1:1).EQ.'T')then
          READ(ifl)((Up(I,J,K),J=1,NJ),I=1,NI)
        endif
        if(flag2(2:2).EQ.'T')then
          READ(ifl)((Vp(i,j,K),J=1,NJ),I=1,NI)
        endif
        if(flag2(3:3).EQ.'T')then
          READ(ifl)((Wp(i,j,K),J=1,NJ),I=1,NI)
        endif
        if(flag2(4:4).EQ.'T')then
          READ(ifl)((TEp(i,j,K),J=1,NJ),I=1,NI)
        endif
        if(flag2(5:5).EQ.'T')then
          READ(ifl)((EDp(i,j,K),J=1,NJ),I=1,NI)
        endif
        if(flag2(6:6).EQ.'T')then
          READ(ifl)((Tp(i,j,K),J=1,NJ),I=1,NI)
        endif
        READ(ifl)((DENp(i,j,K),J=1,NJ),I=1,NI)
20    CONTINUE               
      DO 200 I=1,NI
        DO 2001 J=1,NJ
          DO 2002 K=1,NK
            PP(I,J,K)=0.0
            if(flag2(1:1).EQ.'T') Uf(I,J,K) = Up(I,J,K)
            if(flag2(2:2).EQ.'T') Vf(i,j,K) = Vp(i,j,K)
            if(flag2(3:3).EQ.'T') Wf(i,j,K) = Wp(i,j,K)
            if(flag2(4:4).EQ.'T') TEf(i,j,K) = TEp(i,j,K)
            if(flag2(5:5).EQ.'T') EDf(i,j,K) = EDp(i,j,K)
            if(flag2(6:6).EQ.'T') Tf(i,j,K) = Tp(i,j,K)
            DENf(i,j,K)=DENp(i,j,K)
            VIS(I,J,K)=VISCOS
2002      CONTINUE
2001    CONTINUE
200   CONTINUE

      RETURN
      END

C ********************* PRNNEW *********************
C PRNNEW - Write variable fields in the present time-step.
      subroutine PRNNEW(ifl)
#include "building.h"
#include "cfd.h"

      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ

      CHARACTER*6 FLAG2

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT
      LOGICAL IZEROT,IZanKE,IMITZ

      WRITE(ifl)NI,NJ,NK
      FLAG2='FFFFFF'
      if(incalu)flag2(1:1)='T'
      if(incalv)flag2(2:2)='T' 
      if(incalw)flag2(3:3)='T'
      if(incalk)flag2(4:4)='T' 
      if(incald)flag2(5:5)='T'
      if(incalt)flag2(6:6)='T'                                           
      WRITE(ifl)FLAG2                 
      DO 30 K=1,NK
        WRITE(ifl)((P(I,J,K),J=1,NJ),I=1,NI)
        if(flag2(1:1).EQ.'T')WRITE(ifl)((Uf(I,J,K),J=1,NJ),I=1,NI)
        if(flag2(2:2).EQ.'T')WRITE(ifl)((Vf(i,j,K),J=1,NJ),I=1,NI)
        if(flag2(3:3).EQ.'T')WRITE(ifl)((Wf(i,j,K),J=1,NJ),I=1,NI)
        if(flag2(4:4).EQ.'T')WRITE(ifl)((TEf(i,j,K),J=1,NJ),I=1,NI)
        if(flag2(5:5).EQ.'T')WRITE(ifl)((EDf(i,j,K),J=1,NJ),I=1,NI)
        if(flag2(6:6).EQ.'T')WRITE(ifl)((Tf(i,j,K),J=1,NJ),I=1,NI)
        WRITE(ifl)((DENf(i,j,K),J=1,NJ),I=1,NI)
30    CONTINUE

      RETURN
      END

C ******** tecplotend - output of tecplot-readable text files
Cag@241106
C Change call to call with parameter "IFRAME" which is the time step!
      subroutine tecplotend(IFRAME)
#include "building.h"
#include "cfd.h"

      COMMON/FILEP/IFIL
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)

      integer IFRAME

      character lflname*72

Cag@241106
CC      write(*,*) 'TECPLOTEND:IFRAME = ',IFRAME
C Set filename to include current "frame number" aka. time step
      IF (IFRAME.le.9) THEN
        write(lflname,300) IFRAME
      ELSE IF (IFRAME.le.99) THEN
        write(lflname,301) IFRAME
      ELSE
        write(lflname,302) IFRAME
      ENDIF

C@ag241106
C removed EASKS to avoid filename query for time series:
      IUNIT=IFIL+1
      call EFOPSEQ(IUNIT,lflname,4,ier)

C Heading
      write(IUNIT,'(2a)') 'VARIABLES = ', 
     &      '"XP","YP","ZP","Uf","Vf","Wf","Tf","LMA","CTM1","CTM2"'
Cag@241106
C extended write format command
      write(IUNIT,'(5(a,i5))') 'ZONE T="Timestep ',IFRAME, 
     &      '", STRANDID=1, SOLUTIONTIME=',IFRAME, 
     &      ', I=',(ni-2),', J=',(nj-2),', K=',(nk-2)
Cag@241106 increased loop start to "2" and reduced loop runs by "-1". 
C          This is not checked extensively, however aleviates the 
C          problem with the output of "zero" cells to tecplot files.    
      do 100 k=2,nk-1
        do 100 j=2,nj-1
          do 100 i=2,ni-1
            write(IUNIT,200)XP(I),YP(J),ZP(K),
     &        Uf(I,J,K),Vf(I,J,K),Wf(I,J,K),Tf(I,J,K),AGEf(I,J,K),
     &        POLCONCf(1,I,J,K),POLCONCf(2,I,J,K)
 100  continue                      
      
 200  format(10E14.6)
Cag@241106
C Added formats "300" to "302"
 300  format('TECp_',I1,'.dat')
 301  format('TECp_',I2,'.dat')
 302  format('TECp_',I3,'.dat')

      call ERPFREE(IUNIT,ISTAT)
      
      return
      end

C ********************************************************************
C *   paraviewend - output of paraview-readable text files
C     ag@040108
C
C Output CFD results for ParaView post-processing. See www.paraview.org
C for details of the program ParaView.
C
C Call with parameter "IFRAME" which is the time step!
      subroutine paraviewend(IFRAME)
#include "building.h"
#include "geometry.h"
#include "cfd.h"

      COMMON/FILEP/IFIL
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      LOGICAL CALPOL,CALLMA
      CHARACTER POLNAM*12
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/GRDVRTS/iorg(MNZ),ixend(MNZ),iyend(MNZ),izend(MNZ),
     &  izende(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      common/rpath/path
      character*72 path,lpath

      integer IFRAME

      real Uftmp,Vftmp,Wftmp
      real Xtmp,Ytmp,Ztmp,Xorg,Yorg,Zorg

C 'lflname' is the name of the current output file
      character lflname*72

C If we have a version 3 results library, use domain origin vertex
C cooridinates to ensure that multiple domains do not overlap in
C paraview.
      if (ICFDSV.eq.3) then
        Xorg=szcoords(ICP,IORG(ICFD),1)
        Yorg=szcoords(ICP,IORG(ICFD),2)
        Zorg=szcoords(ICP,IORG(ICFD),3)
      else
        Xorg=0.
        Yorg=0.
        Zorg=0.
      endif

C Write time step data files.
C Set current filename to include current "frame number" (time step).
      write(lflname,300) ICFD,IFRAME

C Check and open data output file.
      IUNIT=IFIL+1
      lpath=path
      path='./'
      call EFOPSEQ(IUNIT,lflname,4,ier)
      path=lpath

C Write the '.vtk' header (total of 4 lines)
C It would be sexy to be able to enter the text for line 2
C from the ESP-r interface ...
      write(IUNIT,'(a)') '# vtk DataFile Version 3.0'
      write(IUNIT,'(a)') 'ESP-r CFD output'
      write(IUNIT,'(a)') 'ASCII'
      write(IUNIT,'(a)') 'DATASET RECTILINEAR_GRID'

C Increase loop starts to "2" and reduced loop runs by "-1".
C I.e., cut off one slice off each face of the CFD region
C (equivalent to  2 elements per dimension). This is not checked 
C extensively, however aleviates the problem with the output of 
C "zero" cells to paraview files.    

      write(IUNIT,'(3(a,i3))') 'DIMENSIONS ',(ni-2),' ',
     &          (nj-2),' ',(nk-2)

C Write the x coordinates
      write(IUNIT,'(a,i3,a)') 'X_COORDINATES ',(ni-2),' float'

      do 100 i=2,ni-1
        Xtmp=XP(I)+Xorg
        write(IUNIT,200)Xtmp
 100  continue

C Write the y coordinates
      write(IUNIT,'(a,i3,a)') 'Y_COORDINATES ',(nj-2),' float'

      do 101 j=2,nj-1
        Ytmp=YP(J)+Yorg
        write(IUNIT,200)Ytmp
 101  continue

C Write the z coordinates
      write(IUNIT,'(a,i3,a)') 'Z_COORDINATES ',(nk-2),' float'

      do 102 k=2,nk-1
        Ztmp=ZP(K)+Zorg
        write(IUNIT,200)Ztmp
 102  continue

C Write data header followed by data
C Begin with temperature
      write(IUNIT,'(a,i6)') 'Point_DATA ',(ni-2)*(nj-2)*(nk-2)
      write(IUNIT,'(a)') 'SCALARS temperature float'
      write(IUNIT,'(a)') 'LOOKUP_TABLE default'

      do 103 k=2,nk-1
        do 103 j=2,nj-1
          do 103 i=2,ni-1
            write(IUNIT,200)Tf(I,J,K)
 103  continue

C Continue with velocity
      write(IUNIT,'(a)') 'VECTORS velocity float'

      do 104 k=2,nk-1
        do 104 j=2,nj-1
          do 104 i=2,ni-1
C Velocity grid is offset from scalar grid (see GRIDGEO in cfgrid.F).
C Interpolate velocity vector components to take account of this.
            Uftmp=0.5*(Uf(i,j,k)+Uf(i+1,j,k))
            Vftmp=0.5*(Vf(i,j,k)+Vf(i,j+1,k))
            Wftmp=0.5*(Wf(i,j,k)+Wf(i,j,k+1))
            write(IUNIT,201)Uftmp,Vftmp,Wftmp
 104  continue

C Continue with contaminant concentration.
      if (NCTM(ICFD).gt.0) then
        do ictm=1,NCTM(ICFD)
          write(IUNIT,'(3a)') 'SCALARS ',
     &      POLNAM(ictm,ICFD)(1:lnblnk(POLNAM(ictm,ICFD))),'_conc float'
          write(IUNIT,'(a)') 'LOOKUP_TABLE default'

          do 105 k=2,nk-1
            do 105 j=2,nj-1
              do 105 i=2,ni-1
                write(IUNIT,201)POLCONCf(ictm,i,j,k)
  105     continue
        enddo
      endif

C Free the file handle
      call ERPFREE(IUNIT,ISTAT)

C Formats
 200  format(1E14.6)
 201  format(3E14.6)
 
 300  format('Domain',I2.2,'_',I3.3,'.vtk')

      return
      end

C ********************************************************************
C *   paraviewbc - output of paraview-readable files showing boundary 
C     conditions.
C
      subroutine paraviewbc
#include "building.h"
#include "geometry.h"
#include "cfd.h"

      COMMON/FILEP/IFIL
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/GRDVRTS/iorg(MNZ),ixend(MNZ),iyend(MNZ),izend(MNZ),
     &  izende(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)    
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/rpath/path
      character*72 path,lpath

      real Xorg,Yorg,Zorg

C 'lflname' is the name of the current output file
      character lflname*72

C Read CFD domain file to retrieve boundary condition information.
      call DFDREAD(ICP,0,0,IER)

C If we have a version 3 results library, use domain origin vertex
C cooridinates to ensure that multiple domains do not overlap in
C paraview.
      if (ICFDSV.eq.3) then
        Xorg=szcoords(ICP,IORG(ICFD),1)
        Yorg=szcoords(ICP,IORG(ICFD),2)
        Zorg=szcoords(ICP,IORG(ICFD),3)
      else
        Xorg=0.
        Yorg=0.
        Zorg=0.
      endif

C Write air flow openings file.
C Set current filename to include current "frame number" (time step).
      write(lflname,301) ICFD

C Check and open data output file.
      IUNIT=IFIL+1
      lpath=path
      path='./'
      call EFOPSEQ(IUNIT,lflname,4,ier)
      path=lpath

C Write the '.vtk' header (total of 4 lines)
      write(IUNIT,'(a)') '# vtk DataFile Version 3.0'
      write(IUNIT,'(a)') 'ESP-r CFD blockage'
      write(IUNIT,'(a)') 'ASCII'
      write(IUNIT,'(a)') 'DATASET UNSTRUCTURED_GRID'

C Find number of openings.
      nopn=0
      do iv=1,NVOL(ICFD)
        if(IVTYPE(iv,ICFD).ge.10 .and. IVTYPE(iv,ICFD).lt.20)nopn=nopn+1
      enddo

C Write opening vertices.
      npt=nopn*4
      write(IUNIT,'(a,I4,a)')'POINTS ',npt,' float'
      do iv=1,NVOL(ICFD)
        if (IVTYPE(iv,ICFD).ge.10 .and. IVTYPE(iv,ICFD).lt.20) then
          if (IVOLF(iv,ICFD).eq.1) then ! west
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &       (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &     (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &     (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &   (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
          elseif (IVOLF(iv,ICFD).eq.2) then ! east
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &       (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &     (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &     (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &   (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
          elseif (IVOLF(iv,ICFD).eq.3) then ! south
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &       (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &       (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &     (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &     (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
          elseif (IVOLF(iv,ICFD).eq.4) then ! north
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &     (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &     (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &   (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &   (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
          elseif (IVOLF(iv,ICFD).eq.5) then ! Low
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &       (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &       (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &     (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &     (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
          elseif (IVOLF(iv,ICFD).eq.6) then ! high
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &     (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &     (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,1))),
     &   (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
            write(IUNIT,'(3(E14.6,1X))')(Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &   (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
          endif
        endif
      enddo

C Assemble sources from vertices.
      nvt=npt+nopn
      write(IUNIT,'(a,I3,1X,I4)')'CELLS ',nopn,nvt
      i=0
      do iv=1,NVOL(ICFD)
        if (IVTYPE(iv,ICFD).ge.10 .and. IVTYPE(iv,ICFD).lt.20) then
          write(IUNIT,'(5(I4,1X))')
     &      4,(i),(i+1),(i+2),(i+3)
          i=i+4
        endif
      enddo

C Write cell types (pixels).
      write(IUNIT,'(a,I3)')'CELL_TYPES ',nopn
      do iv=1,NVOL(ICFD)
        if (IVTYPE(iv,ICFD).ge.10 .and. IVTYPE(iv,ICFD).lt.20) then
          write(IUNIT,'(a)')'8'
        endif
      enddo

C Write cell colours (slightly translucent red).
      write(IUNIT,'(a,I3)')'CELL_DATA ',nopn
      write(IUNIT,'(a)')'SCALARS colour float 1'
      write(IUNIT,'(a)')'LOOKUP_TABLE CellColors'
      do iv=1,NVOL(ICFD)
        if (IVTYPE(iv,ICFD).ge.10 .and. IVTYPE(iv,ICFD).lt.20) then
          write(IUNIT,'(a)')'0.0'
        endif
      enddo
      write(IUNIT,'(a)')'LOOKUP_TABLE CellColors 1'
      write(IUNIT,'(a)')'1.0 0.5 0.5 0.7'
      
C Free the file handle
      call ERPFREE(IUNIT,ISTAT)

C Write blockages file.
C Set current filename to include current "frame number" (time step).
      write(lflname,302) ICFD

C Check and open data output file.
      IUNIT=IFIL+1
      lpath=path
      path='./'
      call EFOPSEQ(IUNIT,lflname,4,ier)
      path=lpath

C Write the '.vtk' header (total of 4 lines)
      write(IUNIT,'(a)') '# vtk DataFile Version 3.0'
      write(IUNIT,'(a)') 'ESP-r CFD blockage'
      write(IUNIT,'(a)') 'ASCII'
      write(IUNIT,'(a)') 'DATASET UNSTRUCTURED_GRID'

C Write blockage vertices.
      npt=NBLK(ICFD)*8
      write(IUNIT,'(a,I4,a)')'POINTS ',npt,' float'
      do iblk=1,NBLK(ICFD)
        iv=INBLK(iblk,ICFD)
        write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,1))),
     &    (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
        write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &    (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
        write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,1))),
     &    (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
        write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &    (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
        write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,1))),
     &    (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
        write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &    (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
        write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,1))),
     &   (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
        write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &   (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
      enddo

C Assemble blockages from vertices.
      nvt=npt+NBLK(ICFD)
      write(IUNIT,'(a,I3,1X,I4)')'CELLS ',NBLK(ICFD),nvt
      i=0
      do iblk=1,NBLK(ICFD)
        write(IUNIT,'(9(I4,1X))')
     &    8,(i),(i+1),(i+2),(i+3),(i+4),(i+5),(i+6),(i+7)
        i=i+8
      enddo

C Write cell types (voxels).
      write(IUNIT,'(a,I3)')'CELL_TYPES ',NBLK(ICFD)
      do iblk=1,NBLK(ICFD)
        write(IUNIT,'(a)')'11'
      enddo

C Write cell colours (grey).
      write(IUNIT,'(a,I3)')'CELL_DATA ',NBLK(ICFD)
      write(IUNIT,'(a)')'SCALARS colour float 1'
      write(IUNIT,'(a)')'LOOKUP_TABLE CellColors'
      do iblk=1,NBLK(ICFD)
        write(IUNIT,'(a)')'0.0'
      enddo
      write(IUNIT,'(a)')'LOOKUP_TABLE CellColors 1'
      write(IUNIT,'(a)')'0.5 0.5 0.5 1'

C Free the file handle
      call ERPFREE(IUNIT,ISTAT)

C Write sources file.
C Set current filename to include current "frame number" (time step).
      write(lflname,303) ICFD

C Check and open data output file.
      IUNIT=IFIL+1
      lpath=path
      path='./'
      call EFOPSEQ(IUNIT,lflname,4,ier)
      path=lpath

C Write the '.vtk' header (total of 4 lines)
      write(IUNIT,'(a)') '# vtk DataFile Version 3.0'
      write(IUNIT,'(a)') 'ESP-r CFD blockage'
      write(IUNIT,'(a)') 'ASCII'
      write(IUNIT,'(a)') 'DATASET UNSTRUCTURED_GRID'

C Find number of sources.
      nsrc=0
      do iv=1,NVOL(ICFD)
        if (IVTYPE(iv,ICFD).eq.20) nsrc=nsrc+1
      enddo

C Write source vertices.
      npt=nsrc*8
      write(IUNIT,'(a,I4,a)')'POINTS ',npt,' float'
      do iv=1,NVOL(ICFD)
        if (IVTYPE(iv,ICFD).eq.20) then
          write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,1))),
     &      (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
          write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &      (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
          write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,1))),
     &     (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
          write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &     (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,1)))
          write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,1))),
     &     (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
          write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &     (Yorg+YV(JVCELLS(iv,ICFD,1))),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
          write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,1))),
     &   (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
          write(IUNIT,'(3(E14.6,1X))') (Xorg+XU(IVCELLS(iv,ICFD,2)+1)),
     &   (Yorg+YV(JVCELLS(iv,ICFD,2)+1)),(Zorg+ZW(KVCELLS(iv,ICFD,2)+1))
        endif
      enddo

C Assemble sources from vertices.
      nvt=npt+nsrc
      write(IUNIT,'(a,I3,1X,I4)')'CELLS ',nsrc,nvt
      i=0
      do iv=1,NVOL(ICFD)
        if (IVTYPE(iv,ICFD).eq.20) then
          write(IUNIT,'(9(I4,1X))')
     &      8,(i),(i+1),(i+2),(i+3),(i+4),(i+5),(i+6),(i+7)
          i=i+8
        endif
      enddo

C Write cell types (voxels).
      write(IUNIT,'(a,I3)')'CELL_TYPES ',nsrc
      do iv=1,NVOL(ICFD)
        if (IVTYPE(iv,ICFD).eq.20) then
          write(IUNIT,'(a)')'11'
        endif
      enddo

C Write cell colours (translucent blue).
      write(IUNIT,'(a,I3)')'CELL_DATA ',nsrc
      write(IUNIT,'(a)')'SCALARS colour float 1'
      write(IUNIT,'(a)')'LOOKUP_TABLE CellColors'
      do iv=1,NVOL(ICFD)
        if (IVTYPE(iv,ICFD).eq.20) then
          write(IUNIT,'(a)')'0.0'
        endif
      enddo
      write(IUNIT,'(a)')'LOOKUP_TABLE CellColors 1'
      write(IUNIT,'(a)')'0.5 0.5 1.0 0.4'
      
C Free the file handle
      call ERPFREE(IUNIT,ISTAT)

 301  format('Domain',I2.2,'_opn.vtk')
 302  format('Domain',I2.2,'_blk.vtk')
 303  format('Domain',I2.2,'_src.vtk')

      return
      end

