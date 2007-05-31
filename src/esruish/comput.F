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
      INCLUDE 'building.h'
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/DATA2/ICAL
      COMMON/SHAD3/IOSHD(MOX,MOZ)
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/MTFILE/LTRNS,MULTC,MONS,MONF
      COMMON/SIF1/INSRTD,INSTRI
      common/ract/paction
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &       nsurfinso(MCOM),isurfinso(MCOM,MS)
      CHARACTER LTRNS*72,TBASE*72,BLKNAME*8,BLKMAT*12,outs*124
      CHARACTER*3 MTHNAM(12)
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15
      CHARACTER act*1,paction*16,h*72
      LOGICAL OK,tok,dok
      DIMENSION IRECS(MS),AVEDEC(12),IVA(MS)

      DATA AVEDEC/17.,15.,16.,15.,15.,11.,17.,16.,16.,
     &            16.,15.,11./
      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C If trace requested enable writing of text version of transition file.
      tok=.false.
      if(icout.eq.33)tok=.true.

      IER=0
      IUNIT=IFIL
      iut=ifil+12

C Specify which surfaces for analysis.
 35   if(act.eq.'s'.or.paction(1:11).eq.'recalculate')then

C Clear return array.
        do 14 i=1,NZSUR(icomp)
          IVA(I)=0
  14    continue
        NP=0
        DO 13 I=1,NZSUR(icomp)
          if(SOTHER(I)(1:5).eq.'EXTER')then
            NP=NP+1
            IVA(NP)=I
          endif
  13    continue
        if(NP.eq.0)then
          CALL usrmsg('No applicable surfaces found:',' ','W')
          return
        endif
        iaplic(icomp,1)=1
      else
        CALL PICKSSUR(ICOMP,NP,'s',IVA,IER)
        if(ier.ne.0)return
      endif

C Set up ISC array (surfaces included in the shading analysis).
      ICONT=0
      DO 110 I=1,NSUR
        II=I
        ISC(II)=0
        DO 120 J=1,NP
          IF(II.EQ.IVA(J))ISC(II)=1
  120   CONTINUE
        IF(ISC(I).EQ.1)ICONT=1
  110 CONTINUE

C Fill ishdirec array.
      nsurfcalc(icomp)=NP
      do 121 j=1,NP
        lstsfcalc(icomp,j)=iva(j)
  121 continue

C Check that at least one surface has been chosen.
      IF(ICONT.EQ.0)THEN
        if(act.eq.'s')then
          CALL edisp(iuout,' No surfs, shading computation abandoned!')
          ICAL=1
          RETURN
        else
          dok=.true.
          h(1)='Surfaces which might be shaded need to be identified '
          h(2)='or the computations cannot be done. '
          CALL ASKOK(' No surfaces have been specified!',
     &              ' Do you wish to re-specify?',OK,dok,2)
          IF(OK)goto 35
          CALL edisp(IUOUT,' Shading computation abandoned !')
          ICAL=1
          RETURN
        endif
      ENDIF

C Use multi-month code even if only a single month required.
C Multi-month calculation.
      MULTC=2
      if(paction(1:13).ne.'update_silent')then
        CALL edisp(IUOUT,
     &  ' A transitional shading file will be opened for each month,')
        call edisp(iuout,
     &  ' the base file name will have Jan, Feb, etc. appended.')
      endif

      if(act.eq.'s')then
        MONS=1
        MONF=12
      else
        CALL EASKI(MONS,' ',' Beginning month number ? ',
     &    1,'F',12,'F',1,'start month',IER,0)
        CALL EASKI(MONF,' ',' Ending month number ? ',
     &    MONS,'F',12,'F',1,'end month',IER,0)
      endif
      DO 601 MON=MONS,MONF
        IDAY=INT(AVEDEC(MON))
        CALL EDAY(IDAY,MON,IYD)

C Reset IOSHD before each month calculation.
        DO 101 I=1,NOX
          DO 201 J=1,NOZ
            IOSHD(I,J)=0
  201     CONTINUE
  101   CONTINUE

C As a separate file will need to be created for each month of a 
C simulation, append 'Jan' or 'Feb' on to the end of the base name to
C identify them. Close any previous file on IUNIT and open a new one.
        TBASE=' '
        TBASE=LTRNS(1:LNBLNK(LTRNS))//MTHNAM(MON)
        IRECW=MS+5
        call EFOPRAN(IUNIT,TBASE,IRECW,3,IER)
        WRITE(outs,'(1x,a,a)')MTHNAM(MON),' calculations commenced'
        if(paction(1:13).ne.'update_silent')then
          call usrmsg(outs,' ','-')
        endif

C Write the first record of transitional file.
        IREC=1
        DO 801 I=1,MS
          IRECS(I)=0
  801   CONTINUE
        WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(IRECS(I),I=1,MS)
        if(tok)write(iut,*)irec,(IRECS(I),I=1,MS)
        CALL TFILE1
        CALL SHDCON(ICOMP)
        ICAL=1
        INSRTD=0
        CALL SSAVE(ier)
        if(ier.ne.0)then
          call usrmsg('Error condition detected during file save...',
     &                'ceasing calculations.','W')
          return
        endif
  601 CONTINUE
      RETURN

 1000 WRITE(outs,21)IREC
   21 FORMAT(' Transitional shading file error in `COMPUT` - Rec',I6)
      call edisp(iuout,outs)
      RETURN
      END
