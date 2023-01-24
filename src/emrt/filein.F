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

c  FILEIN: Processes zone geometry data from user file. 

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c FILEIN: Takes information in common G1 and preprocesses it.
C Common G1M is used to hold expanded surface list (MSM=MS+6) data.

      SUBROUTINE  FILEIN(ITRC,ITRU,IER)

#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM
      COMMON /PLNEC/ PEQN(4,MSM), PCG(3,MSM), BOX(3,2,MSM),
     &               STMAT(4,4,MSM), SRMAT(4,4,MSM)

C ISPNT(1,)  -  pointer to polygon in polygon file
C ISPNT(2,)  -  pointer to transf. polygon in polygon file
C ISPNT(3,)  -  pointer to transformation matrix
C ISPNT(4,)  -  pointer to reverse transf. matrix
C ISPNT(5,)  -  pointer to bound box polygon in polygon file.
C GAREA and EMISI - Not yet used.
      COMMON /SURPNT/ ISPNT(6,MSM), GAREA(MSM), EMISI(MS)

C << emisivity should come from the mlc file!! >>

      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL

      DIMENSION  IAPNT(100), APOL(1000)
      DIMENSION  CG(3),EQN(4)
      character outs*124

      ier=0

      nsurmf=0

C Move geometry into alternate data structure.
      call ESMZON
      DO 1000 I = 1,NSURM
        nsurmf = nsurmf + 1
        IF (nsurmf .GT. MSM)  GOTO  9300

        CALL  ENCPOL(I,NA,NAP,IAPNT,APOL)
        CALL  SPAREA(NA,IAPNT,APOL,1,GAREA(I))

        INRP = 0
        CALL  PUTPOL(NA,NAP,IAPNT,APOL,INRP)
        ISPNT(1,nsurmf) = INRP

        CALL  MPLNEQN(ITRC,ITRU,1,1,APOL,CG,EQN,IER)
        IF (IER .EQ. -1)  GOTO  2999
        PEQN(1,nsurmf) = EQN(1)
        PEQN(2,nsurmf) = EQN(2)
        PEQN(3,nsurmf) = EQN(3)
        PEQN(4,nsurmf) = EQN(4)

        PCG(1,nsurmf) = CG(1)
        PCG(2,nsurmf) = CG(2)
        PCG(3,nsurmf) = CG(3)
 1000 CONTINUE
      RETURN

C ERROR  TRAPS

 9300 CONTINUE
      WRITE(outs,9305)  nsurmf
 9305 FORMAT(' Number of surfaces exceeds MS in building.h ',I4)
      call edisp(iuout,outs)
      STOP

 2999 call edisp(iuout,' Error in PLNEQN ')
      STOP

      END


C ***************** ESMZON 
C ESMZON makes a copy of the current contents of common blocks G1
C into G1M.
      SUBROUTINE ESMZON
#include "building.h"
#include "geometry.h"
      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM

C Loop through all data and copy their contents into a parallel common
C block for later recovery.
      NSURM=NSUR

      NTVM=NTV
      DO 65 IV=1,NTVM
        XM(IV)=X(IV)
        YM(IV)=Y(IV)
        ZM(IV)=Z(IV)
   65 CONTINUE

      DO 66 IS=1,NSUR
        NVERM(IS)=NVER(IS)
        DO 67 IV=1,NVERM(IS)
          JVNM(IS,IV)=JVN(IS,IV)
   67   CONTINUE
   66 CONTINUE

      RETURN
      END


