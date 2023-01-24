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


C ******************** EMSTRR ********************
C Reads a zone moisture data file.

      SUBROUTINE EMSTRR(LMSTR,IUNIT,ICOMP,ITRC,ITRU,IER)
#include "building.h"
#include "espriou.h"
#include "moist.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST05/IPERM(MCOM,MS,ME),PERM(MCOM,MS,ME,3)
      COMMON/MOIST06/ISORP(MCOM,MS,ME),SORP(MCOM,MS,ME,3)
      COMMON/MOIST07/ITVAPD(MCOM,MS,ME),TVAPD(MCOM,MS,ME)

      CHARACTER LMSTR*72,OUTSTR*124,outs*124
      IER=0

C Open existing ASCII moisture data file.
      CALL EFOPSEQ(IUNIT,LMSTR,1,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') LMSTR(1:lnblnk(LMSTR))

C Check moisture file version (legacy files begin with "#  moisture nodes/layer").
C Read lines from file, discarding comments. Look for header on
C newer files. Older files will begin with a description. To allow for
C description with spaces copy directly from OUTSTR rather than parsing
C it into words.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'moist version or header',IER)
      IF(IER.NE.0) goto 102
      if(outstr(1:9).eq.'*Moisture')then
        if(outstr(11:13).eq.'1.0')then
          iMoistVer(icomp)=1
C Defaults to legacy model
          iMoistModel(icomp)=LEGACY

        elseif(outstr(11:13).eq.'2.0')then
          iMoistVer(icomp)=2
C If moisture file version 2.0, read permeability/ sorption
C isotherm model switch (legacy or Otto)

          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'moisture model',IER)
          IF(IER.NE.0) goto 103
          if(outstr(1:6).eq.'LEGACY')then
            iMoistModel(icomp)=LEGACY
          elseif(outstr(1:6).eq.'OTTO  ')then
            iMoistModel(icomp)=OTTO
          else
            goto 104
          endif
        endif
      else
C Rewind legacy file for read and set model to LEGACY
        REWIND(IUNIT,ERR=999)
        iMoistModel(icomp)=LEGACY
      endif

C Read the moisture node distribution information.
C MNDL - number of nodes per layer for each zone construction. 
C        (this is a multiple of the number of nodes used in the
C        conduction calculation, i.e. a multiple of 3.
C MNDC - number of nodes per construction.
C MNDZ - total number of nodes of all constructions in a zone.
      MNDS=MNDS-MNDZ(ICOMP)
      MNDZ(ICOMP)=0
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        NE=NELTS(ICOMP,I)
        CALL STRIPC(IUNIT,OUTSTR,NE,ND,1,'moist nodes/layer',IER)
        IF(IER.NE.0)goto 101
        K=0
        MNDC(ICOMP,I)=1
        DO 20 IL=1,NE
          CALL EGETWI(OUTSTR,K,NM,2,MNM-1,'F','nodes',IER)
          IF(IER.NE.0)goto 101
          MNDL(ICOMP,I,IL)=NM
          MNDC(ICOMP,I)=MNDC(ICOMP,I)+NM
   20   CONTINUE
        MNDZ(ICOMP)=MNDZ(ICOMP)+MNDC(ICOMP,I)
   10 CONTINUE
      MNDS=MNDS+MNDZ(ICOMP)

C Read the data for water vapour permeability for each layer.
C IPERM - index of the permeability data (for each zone, component, layer).
C iperm,ISORP and ITVAPD define the formula to be used to estimate
C hygo-thermal properties. The following exemplar models set these
C indices to 1.
C        ./models/training/constr/adapt/zones/liv_room.mst
C        ./models/training/burdie/zone/bed_3.mst-ii
C        ./models/training/burdie/zone/bed_3.mst
C        ./models/training/wsf/zones/theZone_legacy.mst
C        ./models/training/wsf/zones/theZone_Otto.mst

C PERM contains 3 values describing the vapour permeability, as
C      zone,component,layer,data(1:3).
      DO 30 I=1,NC
        NE=NELTS(ICOMP,I)
      DO 30 IL=1,NE
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'data',IER)
        IF(IER.NE.0)goto 101
        K=0
        CALL EGETWI(OUTSTR,K,INDX,1,1,'F','IPERM index',IER)
        IPERM(ICOMP,I,IL)=INDX
        IF(IER.NE.0)goto 101
        IF(INDX.EQ.1.AND.ND.EQ.4)THEN
          DO 40 IP=1,3
            CALL EGETWR(OUTSTR,K,DATA,0.,1.,'-','PERM() data',IER)
            IF(IER.NE.0)goto 101
            PERM(ICOMP,I,IL,IP)=DATA
   40     CONTINUE
        ELSE
          CALL USRMSG(' ','Un-known index','W')
          RETURN
        ENDIF
   30 CONTINUE

C Read the data for moisture capacity for each layer.
C ISORP - holds the index of each entry.
C SORP  - contains the sorption isotherm data (Uh, A and n).
C SORP  - contains 3 values describing the moisture capacity, as
C         zone,component,layer,data(1:3)
      DO 50 I=1,NC
        NE=NELTS(ICOMP,I)
      DO 50 IL=1,NE
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'data',IER)
        IF(IER.NE.0)goto 101
        K=0
        CALL EGETWI(OUTSTR,K,INDX,1,1,'F','ISORP()index',IER)
        ISORP(ICOMP,I,IL)=INDX
        IF(IER.NE.0)goto 101
        IF(INDX.EQ.1.AND.ND.EQ.4)THEN
          DO 60 IP=1,3
            CALL EGETWR(OUTSTR,K,DATA,0.,1.,'-','SORP() data',IER)
            IF(IER.NE.0)goto 101
            SORP(ICOMP,I,IL,IP)=DATA
   60     CONTINUE
        ELSE
          CALL USRMSG(' ','Un-known moisture index','W')
          RETURN
        ENDIF
   50 CONTINUE

C Read thermal vapor diffusion coefficient for each layer.
C ITVAPD - holds the index. 
C TVAPD  - holds this single value.
C All current exemplar models set the value to 0 since thermal vapour diffusion is not
C active.
C        ./models/training/constr/adapt/zones/liv_room.mst
C        ./models/training/burdie/zone/bed_3.mst-ii
C        ./models/training/burdie/zone/bed_3.mst
C        ./models/training/wsf/zones/theZone_legacy.mst
C        ./models/training/wsf/zones/theZone_Otto.mst

      DO 70 I=1,NC
        NE=NELTS(ICOMP,I)
      DO 70 IL=1,NE
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'data',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,INDX,1,1,'F','index',IER)
        ITVAPD(ICOMP,I,IL)=INDX
        IF(IER.NE.0)RETURN
        IF(INDX.EQ.1.AND.ND.EQ.2)THEN
          CALL EGETWR(OUTSTR,K,DATA,0.,1.,'-','data',IER)
          IF(IER.NE.0)RETURN
          TVAPD(ICOMP,I,IL)=DATA
        ELSE
          CALL USRMSG(' ','Un-known index','W')
          RETURN
        ENDIF
   70 CONTINUE

c Close zone moisture data file.
   6  CALL ERPFREE(IUNIT,ISTAT)

c Trace output.
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,'Zone moisture data file successfully read.')
      ENDIF
      RETURN

C Error messages.
 101  write(outs,'(3a)') 'EMSTRR: error reading ',OUTSTR(1:50),'...'
      call edisp(iuout,outs)
      ier=1
      CALL ERPFREE(IUNIT,ISTAT)
      return

 102  write(outs,'(5a)') 'EMSTRR: error reading first line of file ',
     &   LMSTR(1:lnblnk(LMSTR)),'  ',
     &   OUTSTR(1:50),'...'
      call edisp(iuout,outs)
      ier=1
      CALL ERPFREE(IUNIT,ISTAT)
      return

 103  write(outs,'(3a)') 'EMSTRR: error reading MoistureModel ',
     &   OUTSTR(1:50),'...'
      call edisp(iuout,outs)
      ier=1
      CALL ERPFREE(IUNIT,ISTAT)
      return

 104  write(outs,'(3a)') 'EMSTRR: unknown MoistureModel ',
     &   OUTSTR(1:50),'...'
      call edisp(iuout,outs)
      ier=1
      CALL ERPFREE(IUNIT,ISTAT)
      return

C File rewind errors.
  999 CALL USRMSG('Error rewinding moisture definition file:',
     &  LMSTR,'W')
      IER=1
      GOTO 6

      END

C ******************** EMSTRW ********************
C Writes a zone moisture data file.

      SUBROUTINE EMSTRW(LMSTR,IUNIT,ICOMP,ITRC,ITRU,IER)
#include "building.h"
#include "moist.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST05/IPERM(MCOM,MS,ME),PERM(MCOM,MS,ME,3)
      COMMON/MOIST06/ISORP(MCOM,MS,ME),SORP(MCOM,MS,ME,3)
      COMMON/MOIST07/ITVAPD(MCOM,MS,ME),TVAPD(MCOM,MS,ME)

      CHARACTER LMSTR*72
      IER=0

C Open ASCII moisture data file as unit IUNIT.
      CALL EFOPSEQ(IUNIT,LMSTR,4,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Write the moisture node distribution information
C depending on file version.
      if (iMoistVer(ICOMP).eq.1) then
        WRITE(IUNIT,'(A)')'*Moisture 1.0'
        WRITE(IUNIT,'(A)')'LEGACY'

      elseif (iMoistVer(ICOMP).eq.2) then
        WRITE(IUNIT,'(A)')'*Moisture 2.0'
          if (iMoistModel(ICOMP).eq.LEGACY) then
          WRITE(IUNIT,'(A)')'LEGACY'
          elseif (iMoistModel(ICOMP).eq.OTTO) then
          WRITE(IUNIT,'(A)')'OTTO  '
          endif
      endif

      WRITE(IUNIT,'(A)')'#  moisture nodes/layer'
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        NE=NELTS(ICOMP,I)
        WRITE(IUNIT,'(8I6)')(MNDL(ICOMP,I,IL),IL=1,NE)
   10 CONTINUE

C Write the data for water vapor permeability for each layer.
      WRITE(IUNIT,'(A)')'# Vapour permeability data for each layer'
      DO 30 I=1,NC
        WRITE(IUNIT,'(A,I3)')'# indx |   associated data for surface',I
        NE=NELTS(ICOMP,I)
      DO 30 IL=1,NE
        IN=1
        D1=PERM(ICOMP,I,IL,1)
        D2=PERM(ICOMP,I,IL,2)
        D3=PERM(ICOMP,I,IL,3)
        WRITE(IUNIT,'(3X,I2,6X,3G10.3)')IN,D1,D2,D3
   30 CONTINUE

C Write the data for moisture capacity for each layer.
      WRITE(IUNIT,'(A)')'# Sorption isotherm data for each layer'
      DO 40 I=1,NC
        WRITE(IUNIT,'(A,I3)')
     &          '# indx |   data (Uh, A, n) for construction',I
        NE=NELTS(ICOMP,I)
      DO 40 IL=1,NE
        IN=1
        D1=SORP(ICOMP,I,IL,1)  ! Uh
        D2=SORP(ICOMP,I,IL,2)  ! A
        D3=SORP(ICOMP,I,IL,3)  ! n
        WRITE(IUNIT,'(3X,I2,6X,3G10.3)')IN,D1,D2,D3
   40 CONTINUE

C Write the thermal vapor diffusion coefficient for each layer.
      WRITE(IUNIT,'(2A)')'# Thermal vapour diffusion coefficient',
     &                   ' for each layer'
      DO 50 I=1,NC
        WRITE(IUNIT,'(A,I3)')'# indx |   associated data for surface',I
        NE=NELTS(ICOMP,I)
      DO 50 IL=1,NE
        IN=1
        WRITE(IUNIT,'(3X,I2,6X,G10.3)')IN,TVAPD(ICOMP,I,IL)
   50 CONTINUE

c Close zone moisture data file.
      CALL ERPFREE(IUNIT,ISTAT)

c Trace output.
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,'zone moisture data file successfully saved.')
      ENDIF
      RETURN
      END
