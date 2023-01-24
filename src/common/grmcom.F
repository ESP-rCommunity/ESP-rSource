C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C This file contains the following subroutines:
C GRM_RD   - Read gremlin specification file.
C GRM_INIT - Initialise data structures.
C GRM_CHK  - Check gremlin definition in cfg file.

C ******************** GRM_RD ********************
C Read gremlin data from file unit IUNIT.
C Data format is *tag value.

      subroutine GRM_RD(IUNIT,IER)

#include "gremlin.h"
  
      common/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      character outs*124,word*32,lword*72

C Read file version.
C "iversn" currently not used, but in the future can be used to maintain
C back-compatibility.
      CALL STRIPC(IUNIT,outs,2,idum,0,'gremlin version',IER)
      if (IER.ne.0) goto 666
      k=0
      CALL EGETW(outs,k,word,'F','gremlin version tag',IER)
      if (IER.ne.0) goto 666
      if (word(1:8).ne.'*version') goto 666
      CALL EGETWI(outs,k,iversn,0,0,'-','gremlin version',IER)
      if (IER.ne.0) goto 666

C Read number of gremlins.
      CALL STRIPC(IUNIT,outs,2,idum,0,'number of gremlins',IER)
      if (IER.ne.0) goto 666
      k=0
      CALL EGETW(outs,k,word,'F','number of gremlins tag',IER)
      if (IER.ne.0) goto 666
      if (word(1:12).ne.'*numGremlins') goto 666
      CALL EGETWI(outs,k,itmp,0,MGRM,'F','number of gremlins',IER)
      if (IER.ne.0) goto 666
      NGRM=itmp
      if (NGRM.eq.0) goto 999

C Read log file path.
      CALL STRIPC(IUNIT,outs,2,idum,0,'gremlin log file',IER)
      if (IER.ne.0) goto 666
      k=0
      CALL EGETW(outs,k,word,'F','gremlin log file tag',IER)
      if (IER.ne.0) goto 666
      if (word(1:8).ne.'*logFile') goto 666
      CALL EGETW(outs,k,lword,'F','gremlin log file',IER)
      if (IER.ne.0) goto 666
      GRMLOG=lword
      if (NGRM.eq.0) goto 999

C For each gremlin ...
      do igrm=1,NGRM

C ... read type.
        CALL STRIPC(IUNIT,outs,2,idum,0,'gremlin type',IER)
        if (IER.ne.0) goto 666
        k=0
        CALL EGETW(outs,k,word,'F','gremlin type tag',IER)
        if (IER.ne.0) goto 666
        if (word(1:5).ne.'*type') goto 666
        CALL EGETWI(outs,k,itmp,0,0,'-','gremlin type',IER)
        if (IER.ne.0) goto 666
        GRMTYP(igrm)=itmp
        
C ... read zone.
        CALL STRIPC(IUNIT,outs,2,idum,0,'gremlin zone',IER)
        if (IER.ne.0) goto 666
        k=0
        CALL EGETW(outs,k,word,'F','gremlin zone tag',IER)
        if (IER.ne.0) goto 666
        if (word(1:5).ne.'*zone') goto 666
        CALL EGETWI(outs,k,itmp,0,NCOMP,'F','gremlin zone',IER)
        if (IER.ne.0) goto 666
        GRMZON(igrm)=itmp

      enddo

  999 RETURN

C Error.
  666 outs='GRM_RD: Error while reading gremlin specification file.'
      call edisp(IUOUT,outs)
      goto 999

      end
  

C ******************** GRM_INIT ********************
C Initialise gremlin data structures before definition.

      subroutine GRM_INIT()

#include "gremlin.h"

      IS_GRM=.false.
      GRMFIL='UNKNOWN'
      GRMLOG='../tmp/rand_pert.log'
      NGRM=0

      end

C ******************** GRM_CHK ********************
C Check for gremlins and set commons accordingly.
C Called after we have detected the tag "*gremlin" in the cfg file.
C LINE is the line from the cfg file which should have the path to the
C gremlin file at position K.
C IUF is a temporary file unit.

      subroutine GRM_CHK(LINE,K,IUF,IER)

#include "gremlin.h"

      character(*) LINE
      character outstr*124,word*20

C Read gremlin file name.
      CALL EGETW(LINE,K,GRMFIL,'W','gremlin file',IER)
      if (IER.ne.0) goto 999

C Open gremlin file.
      CALL ERPFREE(IUF,IER)
      CALL EFOPSEQ(IUF,GRMFIL,1,IER)
      if (IER.ne.0) goto 999

C Read first 2 lines.
      CALL STRIPC(IUF,outstr,0,ND,0,'gremlin file 1st line',IER)
      CALL STRIPC(IUF,outstr,0,ND,0,'gremlin file 2nd line',IER)
      if (IER.ne.0) goto 999

C Get number of gremlins.
      K=0
      CALL EGETW(outstr,K,word,'W','*numGremlins',IER)
      if (word(1:12).ne.'*numGremlins') goto 999
      CALL EGETWI(outstr,K,NGRM,0,MGRM,'W','number of gremlins',IER)
      if (IER.ne.0) goto 999

      if (NGRM.gt.0) IS_GRM=.true.
      CALL ERPFREE(IUF,IER)

  999 RETURN

      end
