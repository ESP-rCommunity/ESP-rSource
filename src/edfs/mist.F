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

C Contains: export to radiance routines.
C  genmist - converts water content to mist visualisation input file.

C ********************* GENMIST *********************
C GENMIST converts water content to mist visualisation input file.
      subroutine GENMIST
#include "building.h"
#include "cfd.h"

      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      COMMON/CONCEN/CONf(ntcelx,ntcely,ntcelz),RESORC,SCHMIL,PRODR
      COMMON/CONCEp/CONp(ntcelx,ntcely,ntcelz)

      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)

      COMMON/GEOM2/VolP(ntcelx,ntcely,ntcelz),
     &             VolU(ntcelx,ntcely,ntcelz),
     &             VolV(ntcelx,ntcely,ntcelz),
     &             VolW(ntcelx,ntcely,ntcelz)

      common/VPARTCL/VOLPart(MNVLS,MNZ),SIZPart(MNVLS,MNZ),
     &    DISTPart(MNVLS,MNZ),DENPart(MNVLS,MNZ),ITYPart(MNVLS,MNZ)

      character outs*124
      real VAL

      VAL=0.0

      write (33,*) "# Radiance fog description."
c      IF(SIZPart(1,1).GT.0.0)THEN
        CALL EASKR(VAL,'Droplet diameter (microns)?',' ',0.0,'-',
     &             999.0,'-',1.,'drop diam',IER,0)
        DSIZ=VAL/1000000.
        SIZPart(1,1)=DSIZ
        SIZ=SIZPart(1,1)/2.
c      ENDIF

C Create boxes of mist.
      icount=0
      do 10 I=2,NIM1
        do 20 J=2,NJM1
          do 30 K=2,NKM1
          icount=icount+1

C Calculate ext coefficient for this control volume.
            ECOEF=(1.5*CONf(I,J,K)*VolP(I,J,K)*DENf(I,J,K))/(SIZ/2.)
            WRITE (44,*) I,J,K,CONf(I,J,K),VolP(I,J,K),DENf(I,J,K),SIZ

C Write radiance description.
            write (33,'(a,I6)') "void mist ",icount
            write (33,'(a)') "0"
            write (33,'(a)') "0"
            OUTS=' '
            write (OUTS,'(a,3(f7.3),a)') 
     &               "7 ",ECOEF,ECOEF,ECOEF," 0.999 0.999 0.999 0.84"
            write (33,'(a)') OUTS
            write (33,'(a)') "  "

C Calculate corner and dimendions of the mist filled box allowing for 
C a small gap between control volumes (shrink the volume by 10% in all
C directions).
            CI=XU(I)+(SEW(I)*0.05)
            CJ=YV(J)+(SNS(J)*0.05)
            CK=ZW(K)+(SHL(K)*0.05)
            WI=SEW(I)*0.9
            WJ=SNS(J)*0.9
            WK=SHL(K)*0.9
            write (OUTS,'(a,I6,a,3(f8.5),a,3(f8.5))') "!genbox ",icount,
     &               " box ",WI,WJ,WK," | xform -t ",CI,CJ,CK
            write (33,'(a)') OUTS
            write (33,'(a)') "  "
 30       continue
 20     continue
 10   continue

      close(33)

      return
      end
