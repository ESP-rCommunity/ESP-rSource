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
C AGT_RD   - Read agent specification file.
C AGT_INIT - Initialise data structures.
C AGT_CHK  - Check agent definition in cfg file.

C ******************** AGT_RD ********************
C Read agent data from file unit IUNIT.
C Data format is *tag value.

      subroutine AGT_RD(IUNIT,IER)

#include "agent.h"
      
      common/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      character outs*124,word*32,lword*72

C Read file version.
C "iversn" currently not used, but in the future can be used to maintain
C back-compatibility.
      CALL STRIPC(IUNIT,outs,2,idum,0,'agent version',IER)
      if (IER.ne.0) goto 666
      k=0
      CALL EGETW(outs,k,word,'F','agent version tag',IER)
      if (IER.ne.0) goto 666
      if (word(1:8).ne.'*version') goto 666
      CALL EGETWI(outs,k,iversn,0,0,'-','agent version',IER)
      if (IER.ne.0) goto 666

C Read number of agents.
      CALL STRIPC(IUNIT,outs,2,idum,0,'number of agents',IER)
      if (IER.ne.0) goto 666
      k=0
      CALL EGETW(outs,k,word,'F','number of agents tag',IER)
      if (IER.ne.0) goto 666
      if (word(1:11).ne.'*numAgents') goto 666
      CALL EGETWI(outs,k,itmp,0,MAGT,'F','number of agents',IER)
      if (IER.ne.0) goto 666
      NAGT=itmp
      if (NAGT.eq.0) goto 999

C Read log file path.
      CALL STRIPC(IUNIT,outs,2,idum,0,'agent log file',IER)
      if (IER.ne.0) goto 666
      k=0
      CALL EGETW(outs,k,word,'F','agent log file tag',IER)
      if (IER.ne.0) goto 666
      if (word(1:8).ne.'*logFile') goto 666
      CALL EGETW(outs,k,lword,'F','agent log file',IER)
      if (IER.ne.0) goto 666
      AGTLOG=lword
      if (NAGT.eq.0) goto 999

C For each agent ...
      do iagt=1,NAGT

C ... read type.
        CALL STRIPC(IUNIT,outs,2,idum,0,'agent type',IER)
        if (IER.ne.0) goto 666
        k=0
        CALL EGETW(outs,k,word,'F','agent type tag',IER)
        if (IER.ne.0) goto 666
        if (word(1:5).ne.'*type') goto 666
        CALL EGETWI(outs,k,itmp,0,0,'-','agent type',IER)
        if (IER.ne.0) goto 666
        AGTTYP(iagt)=itmp
        
C ... read zone.
        CALL STRIPC(IUNIT,outs,2,idum,0,'agent zone',IER)
        if (IER.ne.0) goto 666
        k=0
        CALL EGETW(outs,k,word,'F','agent zone tag',IER)
        if (IER.ne.0) goto 666
        if (word(1:5).ne.'*zone') goto 666
        CALL EGETWI(outs,k,itmp,0,NCOMP,'F','agent zone',IER)
        if (IER.ne.0) goto 666
        AGTZON(iagt)=itmp

      enddo

  999 RETURN

C Error.
  666 outs='AGT_RD: Error while reading agent specification file.'
      call edisp(IUOUT,outs)
      goto 999

      end

C ******************** AGT_INIT ********************
C Initialise agent data structures before definition.

      subroutine AGT_INIT()

#include "agent.h"

      IS_AGT=.false.
      AGTFIL='UNKNOWN'
      AGTLOG='../tmp/agent.log'
      NAGT=0

      end

C ******************** AGT_CHK ********************
C Check for agents and set commons accordingly.
C Called after we have detected the tag "*agent" in the cfg file.
C LINE is the line from the cfg file which should have the path to the
C agent file at position K.
C IUF is a temporary file unit.

      subroutine AGT_CHK(LINE,K,IUF,IER)

#include "agent.h"

      character(*) LINE
      character outstr*124,word*20

C Read agent file name.
      CALL EGETW(LINE,K,AGTFIL,'W','agent file',IER)
      if (IER.ne.0) goto 999

C Open agent file.
      CALL ERPFREE(IUF,IER)
      CALL EFOPSEQ(IUF,AGTFIL,1,IER)
      if (IER.ne.0) goto 999

C Read first 2 lines.
      CALL STRIPC(IUF,outstr,0,ND,0,'agent file 1st line',IER)
      CALL STRIPC(IUF,outstr,0,ND,0,'agent file 2nd line',IER)
      if (IER.ne.0) goto 999

C Get number of agents.
      K=0
      CALL EGETW(outstr,K,word,'W','*numAgents',IER)
      if (word(1:10).ne.'*numAgents') goto 999
      CALL EGETWI(outstr,K,NAGT,0,MAGT,'W','number of agents',IER)
      if (IER.ne.0) goto 999

      if (NAGT.gt.0) IS_AGT=.true.
      CALL ERPFREE(IUF,IER)

  999 RETURN

      end
