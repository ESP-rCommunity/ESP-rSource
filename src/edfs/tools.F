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

C CONTAINS TOOLS FOR CFD-DEVELOPMENT:
C  SCAN3dI- scanns an (3D) array in planes of I=const 
C  SCANall- scans (3D) arrays AP, AE, AW, An, AS, AH, AL, SU, SP, AP
C  IEX - flag for excel output=1 (i,j,k headings omited)
C  IEX = 0 write i,j,k headings
C       arr-the array, arrtit - its title up to 10 characters

C ********* scan3di
       subroutine scan3di(arr,arrtit,IEX)
#include "building.h"
#include "cfd.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      dimension arr(ntcelx,ntcely,ntcelz)
      character*(*) arrtit
      character lflname*72

      IUNIT=IFIL+1
      write (lflname,'(a,a)') arrtit(1:lnblnk(arrtit)),'.tst'
      call EFOPSEQ(IUNIT,lflname,4,ier)
      write(IUNIT,200) arrtit
           
      do 100 i=1,ni
        if (IEX.eq.0) write(IUNIT,210) i
        jstart=-11
 110    jstart=jstart+12
        jend=jstart+11
        jend=min0(jend,nj)
        if (IEX.eq.0) write(IUNIT,220) (j,j=jstart,jend)
        if (IEX.eq.0) write(IUNIT,'(a)') '  k'
        do 120 k=nk,1,-1
          write(IUNIT,230) k,(arr(i,j,k),j=jstart,jend)
 120    continue
        if(jend.lt.nj) goto 110
 100  continue       

      call ERPFREE(IUNIT,ISTAT)
       
 200  format(//,40x,a,/,39x,12('*'),/)
 210  format(/,2x,33('*-'),7x,' i =',i3,' ',7x,33('-*'))
 220  format(3x,'j = ',i3,11i12)
 230  format(1x,i3,12e12.4)

      return
      end

      subroutine scanall 
#include "building.h"
#include "cfd.h"

      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)

      IEXCEL=0 
      call scan3di(AP,'zz_AP',IEXCEL) 
      call scan3di(AE,'zz_AE',IEXCEL) 
      call scan3di(AW,'zz_AW',IEXCEL) 
      call scan3di(AN,'zz_AN',IEXCEL) 
      call scan3di(AS,'zz_AS',IEXCEL) 
      call scan3di(AH,'zz_AH',IEXCEL) 
      call scan3di(AL,'zz_AL',IEXCEL) 
      call scan3di(SU,'zz_SU',IEXCEL) 
      call scan3di(SP,'zz_SP',IEXCEL) 

      return
      end

c*********************************************************************
C      subroutine tecplotend_old
c*********************************************************************
#include "building.h"
#include "cfd.h"

C      COMMON/FILEP/IFIL
C      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
C     1            Wf(ntcelx,ntcely,ntcelz),
C     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
C     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
C      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
C     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
C      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
C      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
C     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
C     2            DZHP(ntcelz),DZPL(ntcelz),
C     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
C     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
C      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
C      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
C     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)

C      character lflname*72,tfile*72


C Ask for file name.
C      tfile='TECend.DAT'
C      lflname='TECend.DAT'
C      CALL EASKS(lflname,'TECPLOT file name?','  ',
C     &                           72,tfile,'TECPL file',IER,0)
C      IUNIT=IFIL+1
C      call EFOPSEQ(IUNIT,lflname,4,ier)

C Heading
C      write(IUNIT,'(2a)') 'VARIABLES = ', 
C     &      '"XP","YP","ZP","Uf","Vf","Wf","Tf","LMA","CTM1","CTM2"'
C      write(IUNIT,'(3(a,i5))')'ZONE F=POINT, I=',ni,', J=',nj,', K=',nk
     
C      do 100 k=1,nk
C        do 100 j=1,nj
C          do 100 i=1,ni
C            write(IUNIT,200)XP(I),YP(J),ZP(K),
C     &        Uf(I,J,K),Vf(I,J,K),Wf(I,J,K),Tf(I,J,K),AGEf(I,J,K),
C     &        POLCONCf(1,I,J,K),POLCONCf(2,I,J,K)
C 100  continue                      
      
C 200  format(10E14.6)
C      call ERPFREE(IUNIT,ISTAT)
      
C      return
C      end
