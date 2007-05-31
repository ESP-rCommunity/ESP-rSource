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

C This file contains subroutines to write and read property fields.
C Contains:
C  PRNPHI - Write PHI files (The format required by PHOTON - PHOENICS graphic analyser)
C  REAPRN - Read and write property fields in a binary sequential file.
C  SCAN3dI- scanns an (3D) array in planes of I=const 
C  SCANall- scans (3D) arrays AP, AE, AW, An, AS, AH, AL, SU, SP, AP
C  tecplotend- used for display plotting with 3rd party tools.

C ********************* PRNPHI *********************
C PRNPHI - Write the field values of varibles in ASCII format.
C  The format is the same as PHOENICS output files which can be read by
C  PHOTON (PHOENICS graphic analyser)

      SUBROUTINE PRNPHI(TITLE,ifl)
      INCLUDE 'cfd.h'

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
120   FORMAT(' P1  P2  U1  U2  V1  V2  W1  W2  R1  R2  RS  KE  EP  H1  H  
     &2  C1  C2  C3  C4',/,' C5  C6  C7  C8  C9  C10 C11 C12 C13 C14 C15 
     & C16 C17 C18 C19 C20 C21 C22 C23',/,' C24 C25 C26 C27 C28 C29 C30 
     &C31 C32 C33 C34 C35')
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
      INCLUDE 'cfd.h'

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

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT
      LOGICAL IZEROT,IZanKE,IMITZ

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
c          incalU=.true.
        endif
        if(flag2(2:2).EQ.'T')then
          READ(ifl)((Vp(i,j,K),J=1,NJ),I=1,NI)
c          incalV=.true.
        endif
        if(flag2(3:3).EQ.'T')then
          READ(ifl)((Wp(i,j,K),J=1,NJ),I=1,NI)
c          incalW=.true.
        endif
        if(flag2(4:4).EQ.'T')then
          READ(ifl)((TEp(i,j,K),J=1,NJ),I=1,NI)
c          incalK=.true.
        endif
        if(flag2(5:5).EQ.'T')then
          READ(ifl)((EDp(i,j,K),J=1,NJ),I=1,NI)
c          incalD=.true.
        endif
        if(flag2(6:6).EQ.'T')then
        READ(ifl)((Tp(i,j,K),J=1,NJ),I=1,NI)
c          incalT=.true.
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
      INCLUDE 'cfd.h'

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

C ********* SCAN3dI- scanns an (3D) array in planes of I=const 
C IEX - flag for excel output=1 (i,j,k headings omited)
C IEX = 0 write i,j,k headings
C      arr-the array, arrtit - its title up to 10 characters
       subroutine scan3di(arr,arrtit,IEX)
      INCLUDE 'cfd.h'

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
 230  format(1x,i3,1p12e12.4)

      return
      end

C ******** SCANall- scans (3D) arrays
      subroutine scanall 
      INCLUDE 'cfd.h'

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

C*****************
      subroutine tecplotend
      INCLUDE 'cfd.h'

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

      character lflname*72,tfile*72


C Ask for file name.
      tfile='TECend.DAT'
      lflname='TECend.DAT'
      CALL EASKS(lflname,'TECPLOT file name?','  ',
     &                           72,tfile,'TECPL file',IER,0)
      IUNIT=IFIL+1
      call EFOPSEQ(IUNIT,lflname,4,ier)

C Heading
      write(IUNIT,'(2a)') 'VARIABLES = ', 
     &      '"XP","YP","ZP","Uf","Vf","Wf","Tf","LMA","CTM1","CTM2"'
      write(IUNIT,'(3(a,i5))')'ZONE F=POINT, I=',ni,', J=',nj,', K=',nk
     
      do 100 k=1,nk
        do 100 j=1,nj
          do 100 i=1,ni
            write(IUNIT,200)XP(I),YP(J),ZP(K),
     &        Uf(I,J,K),Vf(I,J,K),Wf(I,J,K),Tf(I,J,K),AGEf(I,J,K),
     &        POLCONCf(1,I,J,K),POLCONCf(2,I,J,K)
 100  continue                      
      
 200  format(10E14.6)
      call ERPFREE(IUNIT,ISTAT)
      
      return
      end
