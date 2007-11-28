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

C COMPUT.F of ESRUshd contains the following routines:

C COMPUT: Sets up the parameters relating to shading computation.

C ********************* COMPUT
C 'COMPUT' Sets up the parameters relating to shading computation.

      SUBROUTINE COMPUT(ICOMP,IER,act)
#include "building.h"

      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)

      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/DATA2/ICAL
      COMMON/SHAD3/IOSHD(MOX,MOZ)
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/MTFILE/LTRNS,MULTC,MONS,MONF
      COMMON/SIF1/INSRTD,INSTRI
      common/ract/paction

C iaplic(1) toggle for shading;
C iaplic(2) toggle for insolation;
C iaplic(3) toggle for assessment where:
C         1 is 'all_applicable',
C         0 is manual selection of surfaces.
C nsurfcalc is number of shaded surfaces, lstsfcalc() list of
C applicable surfaces, nsurfinso number of insolation sources and
C isurfinso() list of insolation sources.
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &       nsurfinso(MCOM),isurfinso(MCOM,MS)
      CHARACTER LTRNS*72,TBASE*72,BLKNAME*8,BLKMAT*12,outs*124
      CHARACTER*3 MTHNAM(12)
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER act*1,paction*16,h*72
      LOGICAL OK,dok
      logical newgeo  ! Used to test for new/old geometry file.
      DIMENSION IRECS(MS),AVEDEC(12),IVA(MS)

      DATA AVEDEC/17.,15.,16.,15.,15.,11.,17.,16.,16.,
     &            16.,15.,11./
      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

      newgeo=.false.  ! assume older format geometry.
      IER=0
      IUNIT=IFIL

  35  call eclose(gversion(icomp),1.1,0.01,newgeo)

C If new format geometry and iaplic(icomp,1) already equals 1 then
C all applicable surfaces were set in the zone geometry file.
      if(newgeo)then
        if(iaplic(icomp,1).eq.1)then
          NP=nsurfcalc(icomp)
          do 41 j=1,NP
            iva(j)=lstsfcalc(icomp,j)
  41      continue

C Set up ISC array (surfaces included in the shading analysis).
          ICONT=0
          do 42 I=1,NZSUR(icomp)
            II=I
            ISC(II)=0
            do 43 J=1,NP
              IF(II.EQ.IVA(J))ISC(II)=1
   43       continue
            if(ISC(I).EQ.1)ICONT=1
   42     continue

        elseif(iaplic(icomp,1).eq.0)then

C If iaplic(icomp,1) is zero and nsurfcalc is non-zero then user
C has manually selected surfaces so fill arrays. If zero then
C the user effectively said no shading should be calculated
C so just clear () and () arrays.
          NP=nsurfcalc(icomp)
          if(NP.eq.0)then
            do 44 I=1,NZSUR(icomp)
              II=I
              ISC(II)=0
  44        continue
          else
            do 45 j=1,NP
              iva(j)=lstsfcalc(icomp,j)
   45       continue

C Set up ISC array (surfaces included in the shading analysis).
            ICONT=0
            DO 46 I=1,NZSUR(icomp)
              II=I
              ISC(II)=0
              DO 47 J=1,NP
                IF(II.EQ.IVA(J))ISC(II)=1
   47         CONTINUE
              IF(ISC(I).EQ.1)ICONT=1
   46       CONTINUE
          endif
        endif

      else

C Older geometry file, user required to set preferences for 
C surfaces to include or in the case of silent action just
C do all appliable.
        if(act.eq.'s'.or.paction(1:11).eq.'recalculate')then

C Clear return array.
          do 14 i=1,NZSUR(icomp)
            IVA(I)=0
  14      continue
          NP=0
          DO 13 I=1,NZSUR(icomp)
            ICN=IZSTOCN(icomp,i)
            if(SSOTHER(icn,1)(1:5).eq.'EXTER')then
              NP=NP+1
              IVA(NP)=I
            endif
  13      continue
          if(NP.eq.0)then
            CALL usrmsg('No applicable surfaces found!',' ','W')
            return
          endif
          iaplic(icomp,1)=1
        else
          CALL PICKSSUR(ICOMP,NP,'s',IVA,IER)
          if(ier.ne.0)return
        endif

C Set up ISC array (surfaces included in the shading analysis).
        ICONT=0
        DO 110 I=1,NZSUR(ICOMP)
          II=I
          ISC(II)=0
          DO 120 J=1,NP
            IF(II.EQ.IVA(J))ISC(II)=1
  120     CONTINUE
          IF(ISC(I).EQ.1)ICONT=1
  110   CONTINUE

C Fill ishdirec array.
        nsurfcalc(icomp)=NP
        do 121 j=1,NP
          lstsfcalc(icomp,j)=iva(j)
  121   continue
      endif

C Check that at least one surface has been chosen or gleaned
C from the geometry file.
      IF(ICONT.EQ.0)THEN
        if(newgeo)then
          CALL edisp(iuout,
     &    'No shaded surfaces noted in zone geometry file - skipping!')
          RETURN
        else
          if(act.eq.'s')then
            CALL edisp(iuout,'No surfaces, shading skipped!')
            ICAL=1
            RETURN
          else
            dok=.true.
            h(1)='Surfaces that might be shaded need to be identified '
            h(2)='or the computations cannot be done. '
            CALL ASKOK('No surfaces selected!',
     &              'Re-specify?',OK,dok,2)
            IF(OK)goto 35
            CALL edisp(IUOUT,'Shading computation abandoned!')
            ICAL=1
            RETURN
          endif
        endif
      endif

C Calculate shading for specified period.
      MULTC=2
      if(paction(1:13).ne.'update_silent')then
        CALL edisp(IUOUT,' ')
        CALL edisp(IUOUT,
     &  'A zone transitional shading file will be opened for each')
        CALL edisp(IUOUT,
     &  'computation month to store surface shading data that')
        CALL edisp(IUOUT,
     &  'are required by the internal surface insolation algorithm.')
        CALL edisp(IUOUT,
     &  'These file are named ?Jan, ?Feb etc., where `?` is the')
        CALL edisp(IUOUT,
     &  'zone base name. These files will be automatically deleted')
        CALL edisp(IUOUT,
     &  'when the Shading & Insolation program is exited normally')
        CALL edisp(IUOUT,
     &  '(otherwise they may be deleted manually).')
      endif

      if(act.eq.'s')then
        MONS=1
        MONF=12
      else
        h(1)='An interger value between 1 and 12.'
        CALL EASKI(MONS,' ','Start month number?',
     &    1,'F',12,'F',1,'start month',IER,1)
        CALL EASKI(MONF,' ','End month number?',
     &    MONS,'F',12,'F',1,'end month',IER,1)
      endif
      DO 601 MON=MONS,MONF
        IDAY=INT(AVEDEC(MON))
        CALL EDAY(IDAY,MON,IYD)

C Reset IOSHD before each month's calculation.
        DO 101 I=1,NOX
          DO 201 J=1,NOZ
            IOSHD(I,J)=0
  201     CONTINUE
  101   CONTINUE

C As a separate file will need to be created for each month of a 
C simulation, append 'Jan' or 'Feb' to the end of the base name to
C identify them. Close any previous file on IUNIT and open a new one.
        TBASE=' '
        TBASE=LTRNS(1:LNBLNK(LTRNS))//MTHNAM(MON)
        IRECW=MS+5
        call EFOPRAN(IUNIT,TBASE,IRECW,3,IER)

C Write the first record of the transitional file.
        IREC=1
        DO 801 I=1,MS
          IRECS(I)=0
  801   CONTINUE
        WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(IRECS(I),I=1,MS)
        CALL TFILE1(icomp)
        CALL SHDCON(ICOMP)
        ICAL=1
        INSRTD=0
        CALL SSAVE(ier)
        if(ier.ne.0)then
          call usrmsg('Error condition detected during file save!',
     &                'Terminating calculation.','W')
          return
        endif
  601 CONTINUE
      RETURN

 1000 WRITE(outs,21)IREC
   21 FORMAT('COMPUT: Zone Transitional Shading file error, record',I6)
      call edisp(iuout,outs)
      RETURN
      END
