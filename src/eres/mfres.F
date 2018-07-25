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

C *************** MFREADH
C MFREADH reads header for fluid flow simulation binary results file.

      SUBROUTINE MFREADH(IFRES,ITRC)
#include "net_flow.h"
#include "net_flow_data.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/LIBOK/IBLIB,IFLIB,ICLIB,IELIB,IPLIB

      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/MFLRESV/FLRESVER

      CHARACTER outs*124,CONTMNAM*12
      LOGICAL CLOSER

C Check that a flow network results file exists.
      if (IFLIB.eq.0) return

C Number of nodes, components and interconnections and results file
C version and optionally number of contaminants in first record.
      IMFREC=1
      read (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) NNOD,NCMP,NCNN,
     &  FLRESVER

C Setup flag if contaminants have been defined or if previous version 
C re-read the first record to get NCONTM.
      if(FLRESVER.lt.2.0)then
        NOCNTM=0
        NCONTM=0
      else
        read (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) NNOD,NCMP,NCNN,
     &  FLRESVER,NCONTM
        IF(NCONTM.NE.0)NOCNTM=1
      endif

C Node data:
C Name, fluid, type, height, temp of other node, def temp, sup data 1 & 2.
      do 10 INOD=1,NNOD
        IMFREC=IMFREC+1
        read (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000)
     &       NDNAM(INOD),NDFLD(INOD),NDTYP(INOD),HNOD(INOD,3),
     &       ITND(INOD),TNOD(INOD),SUPNOD(INOD,1),SUPNOD(INOD,2)
 10   continue

C Component data:
C Name, Type, No of misc components data items, No of misc connection 
C data items, misc components data.
      do 20 ICMP=1,NCMP
        IMFREC=IMFREC+1
        read (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) 
     &      CMNAM(ICMP),ITPCMP(ICMP),ISDCMP(ICMP),ISDCNN(ICMP),
     &      (SUPCMP(ICMP,I), I=1,ISDCMP(ICMP))
 20   continue

C Connections data:
C Pos node, Pos height, Neg node, Neg height, Component, sup node 1,2
      do 30 ICNN=1,NCNN
        IMFREC=IMFREC+1
        read (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000)
     &          NODPS(ICNN),HGTPS(ICNN),NODNE(ICNN),HGTNE(ICNN),
     &          ITPCON(ICNN),NDSCNN(ICNN,1),NDSCNN(ICNN,2)
 30   continue

C Contaminant data:
      IF(NOCNTM.EQ.1)THEN
        DO 40 ICONTM=1,NCONTM
        IMFREC=IMFREC+1
        read (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000)CONTMNAM(ICONTM)
 40     CONTINUE
      ENDIF

C Display network info if requested.
      if (ITRC.eq.1) call MFLIST(iuout)
 50   RETURN

 1000 write(outs,'(A,I5,A)')' MFREADH: error at record',IMFREC,'.'
      call edisp(iuout,outs)
      goto 50

      END
