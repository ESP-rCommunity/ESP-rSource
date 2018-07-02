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

C This file contains network utility routines:
C   CINTSCT     - to determine the intersection point between
C                 a line joining two points and the boundary of the 
C                 network
C   ICNTRFM     - transform an icon
C   GETUTOKENS  - parses a character string via user defined separator.
C   removepad   - takes any initial blanks from string strin.
C   dupicname   - Check for duplicate icon names
C   copyicon    - copy icon data from icon iorigin to icon inew.
C   compacticon - compact icon data list (remove selected items).


C ******************* CINTSCT *********************
C Routine to determine the intersection point between
C a line joining two points and the boundary of the
C network
      SUBROUTINE CINTSCT(A,B,XM,XX,YM,YX,XI,YI,FOUND)

      DIMENSION A(2),B(2),DIST(3)
      LOGICAL VERT,HORIZ,FOUND,CLOSEA,CLOSEB

      HORIZ=.FALSE.
      VERT=.FALSE.
      FOUND=.FALSE.
      CLOSEA=.FALSE.
      CLOSEB=.FALSE.

      CALL ECLOSE(ABS(B(1)-A(1)),0.0,0.01,VERT)
      CALL ECLOSE(ABS(B(2)-A(2)),0.0,0.01,HORIZ)
      IF(.NOT.VERT.AND..NOT.HORIZ)THEN
        GR=(B(2)-A(2))/(B(1)-A(1))
        CN=A(2)-GR*A(1)     
      ENDIF

      DIST(3)=((A(1)-B(1))**2+(A(2)-B(2))**2)**0.5
      XI=XM
      YI=YM
      XT=0.0
      YT=0.0

C Loop through boundaries to find the intersection point
      DO 10 I=1,4
        IF(I.EQ.1)THEN
          IF(VERT)THEN
            FOUND=.FALSE.        
          ELSE
            IF(HORIZ)THEN
              XT=XM
              YT=A(2)
            ELSE
              XT=XM
              YT=GR*XT+CN
            ENDIF
            DIST(1)=((A(1)-XT)**2+(A(2)-YT)**2)**0.5  
            DIST(2)=((B(1)-XT)**2+(B(2)-YT)**2)**0.5   
          ENDIF
        ELSEIF(I.EQ.2)THEN
          IF(VERT)THEN
            FOUND=.FALSE.        
          ELSE
            IF(HORIZ)THEN
              XT=XX
              YT=A(2)
            ELSE
              XT=XX
              YT=GR*XT+CN
            ENDIF
            DIST(1)=((A(1)-XT)**2+(A(2)-YT)**2)**0.5  
            DIST(2)=((B(1)-XT)**2+(B(2)-YT)**2)**0.5   
          ENDIF
        ELSEIF(I.EQ.3)THEN
          IF(HORIZ)THEN
            FOUND=.FALSE.             
          ELSE
            IF(VERT)THEN
              YT=YM   
              XT=A(1) 
            ELSE
              YT=YM
              XT=(YT-CN)/GR
            ENDIF
            DIST(1)=((A(1)-XT)**2+(A(2)-YT)**2)**0.5  
            DIST(2)=((B(1)-XT)**2+(B(2)-YT)**2)**0.5   
          ENDIF
        ELSE
          IF(HORIZ)THEN
            FOUND=.FALSE.           
          ELSE
            IF(VERT)THEN
              YT=YX
              XT=A(1)
            ELSE
              YT=YX
              XT=(YT-CN)/GR
            ENDIF
            DIST(1)=((A(1)-XT)**2+(A(2)-YT)**2)**0.5  
            DIST(2)=((B(1)-XT)**2+(B(2)-YT)**2)**0.5   
          ENDIF
        ENDIF

        CALL ECLOSE((DIST(1)-DIST(3)),0.0,0.01,CLOSEA)
        CALL ECLOSE((DIST(2)-DIST(3)),0.0,0.01,CLOSEB)

C Check if the intersection lies between the two points
        IF((DIST(1).LE.DIST(3).OR.CLOSEA).AND.
     &(DIST(2).LE.DIST(3).OR.CLOSEB))THEN
          IF(.NOT.FOUND)THEN
            IF((XT.GE.XM-0.01.AND.XT.LE.XX+0.01).AND.
     &(YT.GE.YM-0.01.AND.YT.LE.YX+0.01))THEN            
              XI=XT
              YI=YT
              FOUND=.TRUE.
            ELSE
              FOUND=.FALSE.
            ENDIF
          ENDIF
        ENDIF    
 10   CONTINUE

      RETURN
      END

*************************** ICNTFM **********************************
C ICNTFM is called from C code interface.
C XYZ - icon vertices
C TXYZ - transformed icon vertices
C MXFM - transformation matrix
C ITFM - transform type 
C 1 - flip about vert
C 2 - flip about horiz
C 3 - rotate by IDAT degrees 
C 
      SUBROUTINE ICNTFM(ITFM)
#include "gnetwk.h"

C Icon commons
C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

      COMMON/IROT/ROTA

      DIMENSION TXYZ(MICN,MICNV,3),T2XYZ(MICN,MICNV,3)
      DIMENSION MXFM(9),TCONCP(MICN,MCNP,2)

      REAL MXFM

      LOGICAL ISEL,CSEL
      
      PI=22.0/7.0

      DO 5 I=1,NNICN

      IF(ISEL(I))THEN

C Move the co-ordinates be centred on the origin
      DO 10 J=1,NIVC(I)
        TXYZ(I,J,1)=VCICON(I,J,1)-XYZICON(I,1)    
        TXYZ(I,J,2)=VCICON(I,J,2)-XYZICON(I,2)
        TXYZ(I,J,3)=VCICON(I,J,3)-XYZICON(I,3) 
        T2XYZ(I,J,1)=0.   
        T2XYZ(I,J,2)=0.
        T2XYZ(I,J,3)=0. 
  10  CONTINUE

C Also transform the connection points
        DO 12 K=1,NCONP(I)
C Inset code for view differentiation here.
          CONCP(I,K,1)=CONCP(I,K,1)-XYZICON(I,1)
          CONCP(I,K,2)=CONCP(I,K,2)-XYZICON(I,2)
  12    CONTINUE  


      DO 20 K=1,9
        MXFM(K)=0.
  20  CONTINUE

C Fill in the non-zero coefficients of the transformation matrix.
      WRITE(*,*) 'Mode:', ITFM
      IF(ITFM.EQ.2)THEN
        MXFM(1)=-1.
        MXFM(5)=1.
        MXFM(9)=1.
      ELSEIF(ITFM.EQ.3)THEN
        MXFM(1)=1.
        MXFM(5)=-1.
        MXFM(9)=1.
      ELSE
        ANG=(ROTA/180.)*PI
        MXFM(1)=COS(ANG)
        MXFM(2)=-SIN(ANG)
        MXFM(4)=SIN(ANG)
        MXFM(5)=COS(ANG)
        MXFM(9)=1.
      ENDIF

C Get transformed values
      DO 30 J=1,NIVC(I)
        T2XYZ(I,J,1)= 
     &TXYZ(I,J,1)*MXFM(1)+TXYZ(I,J,2)*MXFM(2)+TXYZ(I,J,3)*MXFM(3) 
        T2XYZ(I,J,2)= 
     &TXYZ(I,J,1)*MXFM(4)+TXYZ(I,J,2)*MXFM(5)+TXYZ(I,J,3)*MXFM(6) 
        T2XYZ(I,J,3)=
     &TXYZ(I,J,1)*MXFM(7)+TXYZ(I,J,2)*MXFM(8)+TXYZ(I,J,3)*MXFM(9) 
  30  CONTINUE 

C Also transform the connection points
      DO 32 K=1,NCONP(I)
C <<Inset code for view differentiation here.>>
        TCONCP(I,K,1)=CONCP(I,K,1)*MXFM(1)+CONCP(I,K,2)*MXFM(2)
        TCONCP(I,K,2)=CONCP(I,K,1)*MXFM(4)+CONCP(I,K,2)*MXFM(5)
  32  CONTINUE  

C Move the transformed co-ordinates back to their original centre
      DO 40 J=1,NIVC(I)
        T2XYZ(I,J,1)=T2XYZ(I,J,1)+XYZICON(I,1)   
        T2XYZ(I,J,2)=T2XYZ(I,J,2)+XYZICON(I,2) 
        T2XYZ(I,J,3)=T2XYZ(I,J,3)+XYZICON(I,3)
  40  CONTINUE

      DO 42 K=1,NCONP(I)
C <<Inset code for view differentiation here.>>
        TCONCP(I,K,1)=TCONCP(I,K,1)+XYZICON(I,1) 
        TCONCP(I,K,2)=TCONCP(I,K,2)+XYZICON(I,2) 
  42  CONTINUE  

C Finaly return the transformed co-ordinates
      DO 50 J=1,NIVC(I)
        VCICON(I,J,1)=T2XYZ(I,J,1)  
        VCICON(I,J,2)=T2XYZ(I,J,2)
        VCICON(I,J,3)=T2XYZ(I,J,3)
  50  CONTINUE

      DO 52 K=1,NCONP(I)
C <<Inset code for view differentiation here.>>
        CONCP(I,K,1)=TCONCP(I,K,1)
        CONCP(I,K,2)=TCONCP(I,K,2)
  52  CONTINUE  

      ENDIF

      CSEL(I)=.FALSE.

      CALL NETWDRW

  5   CONTINUE

      RETURN
      END


C ********************* GETUTOKENS 
C GETUTOKENS checks a character string (A), returning the number of data
C items (IW) separated by a user defined separator (sep) and an array
C of tokens/words (up to 12 32character words returned) that were in
C the string.
C Useful for parsing compound names.  Note to keep from 
C overwriting the string passed, deal with a copy.
      SUBROUTINE GETUTOKENS(A,sep,IW,WORDS)
      integer lnblnk  ! function definition
      CHARACTER*(*) A
      CHARACTER*32 WORDS(12)
      CHARACTER B*124,C*1,sep*1
      logical unixok

C Clear WORDS.
      do 9 i=1,12
        WORDS(i)='  '
  9   continue

      lastcc = MIN0(LNBLNK(A),123)
      WRITE(B,'(A)',IOSTAT=ISTAT,ERR=1) A(1:lastcc+1)

C Loop through B and convert all separators to blanks.
      DO 100 I=1,LNBLNK(B)+1
        C=B(I:I)
        IF(C.EQ.sep) B(I:I)=' '
  100 CONTINUE

C Loop through B and see how many 'words'.
      K=0
      IW=0
  102 IW=IW+1

C Start by skipping blanks before the word.
   10 K=K+1
      C=B(K:K)
      IF(C.EQ.' ') GOTO 10

C Loop character by character until space separator is found.
   20 K=K+1
      C=B(K:K)
      if(K.GE.LNBLNK(B))then

C Have determined the number of tokes so loop through each and
C assign WORDS.
        if(IW.gt.0)then
          KK=0
          do 42 ij=1,IW
            CALL EGETW(B,KK,WORDS(ij),'W','token',IER)
            if(ier.ne.0)goto 2
  42      continue
        endif
        RETURN
      endif
      IF(C.EQ.' ')GOTO 102
      GOTO 20
      
   1  call isunix(unixok)
      if(unixok)then
        write(6,*) 'GETUTOKENS: error in internal write: ',A
      endif
      return
   2  call isunix(unixok)
      if(unixok)then
        write(6,*) 'GETUTOKENS: error in getting token: ',A
      endif
      return
 
      END


C ***************** removepad 
C removepad takes any initial blanks from string strin.
C ISWD is the length of the resulting string fstr. It is
C assumed that each string is the same length.
      SUBROUTINE removepad(cstr,FSTR,ISWD,IFLAG)
      integer lnblnk  ! function definition

      CHARACTER*(*) cstr,FSTR
      IFLAG=0
      ISWD=0
      fstr = ' '

      K=0
      DO 99 I=1,LEN(cstr)
        IF(cstr(I:I).NE.' '.OR.K.GE.1)THEN
          if(ichar(cstr(I:I)).lt.32)goto 100
          K=K+1
          fstr(K:K)=cstr(I:I)
        ENDIF
 99   CONTINUE

  100 ISWD=LNBLNK(FSTR)
      RETURN

      END

C ************ dupicname ************
C Check for duplicate icon names (against the name of icon
C index icn). The logic looks for [ in the name in case there
C were prior name[1] in the list.
C << logic could be improved. >>
      subroutine dupicname(icn)
#include "gnetwk.h"
      
      integer lnblnk  ! function definition

C Icon commons are in gnetwk.h
      CHARACTER OUTS*124

      NAMATCH=0
      DO 538 I=1,NNICN
        OUTS=' '
        IRLEN=LNBLNK(NWICNM(ICN)) 
C Debug        WRITE(*,*)'IRLEN ',IRLEN  
        IROOT=IRLEN
        DO 727 IR=1,IRLEN
          IF(NWICNM(ICN)(IR:IR).EQ.'[') IROOT=IR-1
  727   CONTINUE
        IRLEN=IROOT
        IF(NWICNM(ICN)(1:IRLEN).EQ.NWICNM(I)(1:IRLEN).AND.I.NE.ICN)THEN
          NAMATCH=NAMATCH+1
          IF(IRLEN.LE.8)THEN
            IF(NAMATCH.LT.10)THEN
              WRITE(OUTS,'(A,A1,I1,A1)')
     &          NWICNM(ICN)(1:LNBLNK(NWICNM(ICN))),'[',NAMATCH,']'
            ELSE
              WRITE(OUTS,'(A,A1,I2,A1)')
     &          NWICNM(ICN)(1:LNBLNK(NWICNM(ICN))),'[',NAMATCH,']'
            ENDIF
            WRITE(*,*) OUTS
            NWICNM(ICN)=OUTS(1:12)
          ELSE
            IF(NAMATCH.LT.10)THEN
              WRITE(OUTS,'(A8,A1,I1,A1)')
     &          NWICNM(ICN)(1:8),'[',NAMATCH,']'
            ELSE
              WRITE(OUTS,'(A8,A1,I2,A1)')
     &          NWICNM(ICN)(1:8),'[',NAMATCH,']'
            ENDIF
            WRITE(*,*) OUTS
            NWICNM(ICN)=OUTS(1:12)
          ENDIF
        ENDIF
  538 CONTINUE 
      return
      end

C ******* copy icon data
C Copy data from icon iorigin to icon inew.
      subroutine copyicon(iorigin,inew,ier)
# include "gnetwk.h"

C Icon commons are in gnetwk.h
C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

      LOGICAL ISEL,CSEL

C Copy data within commons NWKICN and NWKICS.
      ICONTP(inew)=ICONTP(iorigin)
      NWICNM(inew)=NWICNM(iorigin)
      NWICNHIS(inew)=NWICNHIS(iorigin)
      NWICNME(inew)=NWICNME(iorigin)
      NICONATR(inew)=NICONATR(iorigin)
      if(NICONATR(inew).ge.1)then
        do 36 J=1,NICONATR(inew)
          ATRTAG(inew,J,1)=ATRTAG(iorigin,J,1)
          ATRTAG(inew,J,2)=ATRTAG(iorigin,J,2)
          ATRTAG(inew,J,3)=ATRTAG(iorigin,J,3)
          ATRTAG(inew,J,4)=ATRTAG(iorigin,J,4)
          ATRTAG(inew,J,5)=ATRTAG(iorigin,J,5)
          ATRICN(inew,J,1)=ATRICN(iorigin,J,1)
          ATRICN(inew,J,2)=ATRICN(iorigin,J,2)
          ATRICN(inew,J,3)=ATRICN(iorigin,J,3)
          ATRMENU(inew,J)=ATRMENU(iorigin,J)
  36    continue
      endif
      NIVC(inew)=NIVC(iorigin)
      if(NIVC(inew).ge.1)then
        DO 32 J=1,NIVC(inew)
c          VCICON(inew,J,3)=VCICON(iorigin,J,3)
c          VCICON(inew,J,2)=VCICON(iorigin,J,2)
c          VCICON(inew,J,1)=VCICON(iorigin,J,1)                 
  32    CONTINUE
      endif
      NIVE(inew)=NIVE(iorigin)
      if(NIVE(inew).ge.1)then
        DO 304 K=1,NIVE(inew)
          IVEICN(inew,K,5)=IVEICN(iorigin,K,5)
          IVEICN(inew,K,4)=IVEICN(iorigin,K,4)
          IVEICN(inew,K,3)=IVEICN(iorigin,K,3)
          IVEICN(inew,K,2)=IVEICN(iorigin,K,2)
          IVEICN(inew,K,1)=IVEICN(iorigin,K,1)
  304   CONTINUE
      endif
      NIVD(inew)=NIVD(iorigin)
      if(NIVD(inew).ge.1)then
        do 33 K=1,NIVD(inew)
          IVDOT(inew,K,1)=IVDOT(iorigin,K,1)
          IVDOT(inew,K,2)=IVDOT(iorigin,K,2)
          IVDOT(inew,K,3)=IVDOT(iorigin,K,3)
          IVDOT(inew,K,4)=IVDOT(iorigin,K,4)
  33    continue
      endif
      NIVA(inew)=NIVA(iorigin)
      if(NIVA(inew).ge.1)then
        do 29 K=1,NIVA(inew)
          IVARC(inew,K,1)=IVARC(iorigin,K,1)
          IVARC(inew,K,2)=IVARC(iorigin,K,2)
          IVARC(inew,K,3)=IVARC(iorigin,K,3)
          IVARC(inew,K,4)=IVARC(iorigin,K,4)
          IVARC(inew,K,5)=IVARC(iorigin,K,5)
          IVARC(inew,K,6)=IVARC(iorigin,K,6)
          IVARC(inew,K,7)=IVARC(iorigin,K,7)
  29    continue
      endif
      NIVL(inew)=NIVL(iorigin)
      if(NIVL(inew).ge.1)then
        do 31 K=1,NIVL(inew)
          IVLBL(inew,K,1)=IVLBL(iorigin,K,1)
          IVLBL(inew,K,2)=IVLBL(iorigin,K,2)
          IVLBL(inew,K,3)=IVLBL(iorigin,K,3)
          NWICNLBL(inew,K)=NWICNLBL(iorigin,K)
  31    continue
      endif

      NCONP(inew)=NCONP(iorigin)
      DO 44 K=1,NCONP(iorigin)
c        CONCP(inew,K,2)=CONCP(iorigin,K,2)
c        CONCP(inew,K,1)=CONCP(iorigin,K,1)
  44  CONTINUE
      ISEL(inew)=ISEL(iorigin)

      return
      end

C ******* compact icon data list
C Compact icon list by removing selected items (i.e. where
C isel()=.true. Note the calling subroutine currently only
C sets one icon to be deleted.
C << IC currently not used >>
      subroutine compacticon(IC,ier)
# include "gnetwk.h"

C Icon commons are in gnetwk.h
C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

      LOGICAL ISEL,CSEL

      NWICN=0

C Find out which icon is to be deleted and if none return.
      iwhich=0
      do 18 J=1,NNICN
        if(isel(J))then
          iwhich=j
        endif
  18  continue
      if(iwhich.eq.0)return

C Loop through all icons and for unselected items copy to
C the index nwich (which will be the same as `I` until the
C selected icon is passed).
      NWICN=0
      DO 20 I=1,NNICN
        IF(.NOT.ISEL(I))THEN
          NWICN=NWICN+1
          ICONTP(NWICN)=ICONTP(I)
          XYZICON(NWICN,1)=XYZICON(I,1)    
          XYZICON(NWICN,2)=XYZICON(I,2)
          XYZICON(NWICN,3)=XYZICON(I,3) 
          NWICNM(NWICN)=NWICNM(I)
          NWICNHIS(NWICN)=NWICNHIS(I)
          NWICNME(NWICN)=NWICNME(I)
          NICONATR(NWICN)=NICONATR(I)
          if(NICONATR(NWICN).ge.1)then
            do 26 J=1,NICONATR(NWICN)
              ATRTAG(NWICN,J,1)=ATRTAG(I,J,1)
              ATRTAG(NWICN,J,2)=ATRTAG(I,J,2)
              ATRTAG(NWICN,J,3)=ATRTAG(I,J,3)
              ATRTAG(NWICN,J,4)=ATRTAG(I,J,4)
              ATRTAG(NWICN,J,5)=ATRTAG(I,J,5)
              ATRICN(NWICN,J,1)=ATRICN(I,J,1)
              ATRICN(NWICN,J,2)=ATRICN(I,J,2)
              ATRICN(NWICN,J,3)=ATRICN(I,J,3)
              ATRMENU(NWICN,J)=ATRMENU(I,J)
  26        continue
          endif
          NIVC(NWICN)=NIVC(I)
          if(NIVC(NWICN).ge.1)then
            DO 22 J=1,NIVC(NWICN)
              VCICON(NWICN,J,3)=VCICON(I,J,3)
              VCICON(NWICN,J,2)=VCICON(I,J,2)
              VCICON(NWICN,J,1)=VCICON(I,J,1)                 
  22        CONTINUE
          endif
          NIVE(NWICN)=NIVE(I)
          if(NIVE(NWICN).ge.1)then
            DO 24 K=1,NIVE(NWICN)
              IVEICN(NWICN,K,5)=IVEICN(I,K,5)
              IVEICN(NWICN,K,4)=IVEICN(I,K,4)
              IVEICN(NWICN,K,3)=IVEICN(I,K,3)
              IVEICN(NWICN,K,2)=IVEICN(I,K,2)
              IVEICN(NWICN,K,1)=IVEICN(I,K,1)
  24        CONTINUE
          endif
          NIVD(NWICN)=NIVD(I)
          if(NIVD(NWICN).ge.1)then
            do 23 K=1,NIVD(NWICN)
              IVDOT(NWICN,K,1)=IVDOT(I,K,1)
              IVDOT(NWICN,K,2)=IVDOT(I,K,2)
              IVDOT(NWICN,K,3)=IVDOT(I,K,3)
              IVDOT(NWICN,K,4)=IVDOT(I,K,4)
  23        continue
          endif
          NIVA(NWICN)=NIVA(I)
          if(NIVA(NWICN).ge.1)then
            do 19 K=1,NIVA(NWICN)
              IVARC(NWICN,K,1)=IVARC(I,K,1)
              IVARC(NWICN,K,2)=IVARC(I,K,2)
              IVARC(NWICN,K,3)=IVARC(I,K,3)
              IVARC(NWICN,K,4)=IVARC(I,K,4)
              IVARC(NWICN,K,5)=IVARC(I,K,5)
              IVARC(NWICN,K,6)=IVARC(I,K,6)
              IVARC(NWICN,K,7)=IVARC(I,K,7)
  19        continue
          endif
          NIVL(NWICN)=NIVL(I)
          if(NIVL(NWICN).ge.1)then
            do 21 K=1,NIVL(NWICN)
              IVLBL(NWICN,K,1)=IVLBL(I,K,1)
              IVLBL(NWICN,K,2)=IVLBL(I,K,2)
              IVLBL(NWICN,K,3)=IVLBL(I,K,3)
              NWICNLBL(NWICN,K)=NWICNLBL(I,K)
  21        continue
          endif

          NCONP(NWICN)=NCONP(I)
          DO 34 K=1,NCONP(I)
            CONCP(NWICN,K,2)=CONCP(I,K,2)
            CONCP(NWICN,K,1)=CONCP(I,K,1)
            ICNCT(NWICN,IC)=ICNCT(I,IC)
C            ICPP(NWICN,K,1)=ICPP(I,K,1)
C            ICPP(NWICN,K,2)=ICPP(I,K,2)
C            ICPP(NWICN,K,3)=ICPP(I,K,3)
  34      CONTINUE
          ISEL(NWICN)=ISEL(I)

C Primitive parts data structures currently not implemented
C          III=0
C          DO 35 IH=1,NWICN
C            DO 30 IL=1,NCONP(IH)
C              III=III+1
C              CPP(1,III)=ICPP(IH,IL,1)
C              CPP(2,III)=ICPP(IH,IL,2)
C              CPP(3,III)=ICPP(IH,IL,3)
C  30        CONTINUE
C  35      CONTINUE
        ELSE           
          NWCNN=0  ! at selected icon set NWCNN zero

C Delete any associated connections. If the start and end
C icon associated with J loop connection does not match
C the current (to be deleted) connection (index I) copy
C the array data to the same array position. Once a match
C has been made the nwcnn counter will be less than J.
          DO 25 J=1,NICNN
            IF(ICNS(J).NE.I.AND.ICNE(J).NE.I)THEN
              NWCNN=NWCNN+1
              ICNS(NWCNN)=ICNS(J)
              ICNE(NWCNN)=ICNE(J)
              ICNNT(NWCNN)=ICNNT(J)
              ICNSP(NWCNN)=ICNSP(J)
              ICNEP(NWCNN)=ICNEP(J)                  
              NCONWP(NWCNN)=NCONWP(J)
              DO 27 K=1,NCONWP(J)
                CNWNP(NWCNN,K,1)=CNWNP(J,K,1)
                CNWNP(NWCNN,K,2)=CNWNP(J,K,2)
                CNWNP(NWCNN,K,3)=CNWNP(J,K,3)
  27          CONTINUE
              idatrdom(NWCNN)=idatrdom(J)
              DO 28 K=1,MIATRB
                ddmenuatr(NWCNN,K)=ddmenuatr(J,K)
                DO 29 L=1,5
                  ddtagatr(NWCNN,K,L)=ddtagatr(J,K,L)
                  ddatrib(NWCNN,K,L)=ddatrib(J,K,L)
  29            CONTINUE
  28          CONTINUE
              CSEL(NWCNN)=CSEL(J)
            ENDIF
  25      CONTINUE
          NICNN=NWCNN
        ENDIF
  20  CONTINUE
      NNICN=NWICN

C Clean up by loop through all the connections and decrement ICNS() and
C ICNE() where greater than the deleted icon (iwhich).
      do 17 J=1,NICNN
        if(ICNS(J).gt.iwhich) ICNS(J)=ICNS(J)-1
        if(ICNE(J).gt.iwhich) ICNE(J)=ICNE(J)-1
  17  continue

      return
      end
