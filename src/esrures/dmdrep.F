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

C Routines which deal with non-specific project demands (fans/lifts/etc.).

C ******************** ERBDMD
C Similar prjdmds.F except do not bother to set idmdinc.
C ERBDMD reads project level demand profiles for fans, pumps, DHW etc.

      SUBROUTINE ERBDMD(ITRC,IUO,IER)
#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "schedule.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/BL2/dmdfla(7)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

      CHARACTER outstr*124,loutstr*248
      character dllsubr*12,dllmesg*124,WORD*20,outs*124,outs2*124
      character calename*32,calentag*12,calendayname*32
      character dstmp*24
      logical dll
      integer lsn

      IER=0

C Check if running in dll mode.
      call isadll(dll)

C Initialise project data file.
      CALL EFOPSEQ(IUO,bdmds,1,IER)
      IF(IER.NE.0)THEN
        write(outs,'(3a)') 'Dispersed demands file ',
     &    bdmds(1:lnblnk(bdmds)),' could not be opened.'
        if(dll)then
          dllsubr='ERBDMD'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(iuout,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') bdmds(1:lnblnk(bdmds))

C Read lines from file, discarding comments. To allow for operation
C names with spaces copy directly from OUTSTR rather than parsing
C it into words.  Use long stip for dmdsdesc line. 
      CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'dispersed notes',IER)
      IF(IER.NE.0)RETURN

C Detect header and set a version number.
      if(loutstr(1:10).eq.'*Dispersed')then
        if(loutstr(12:14).eq.'1.0')then
          idmdver=1
        elseif(loutstr(12:14).eq.'2.0')then
          idmdver=2
        endif
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'header',IER)
        IF(IER.NE.0) goto 1002
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','header tags',IER)
        IF(IER.NE.0) goto 1002
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          CALL EGETRM(OUTSTR,K,dstmp,'W','date stamp',IER)
        endif
        CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'dispersed notes',IER)
        dmdsdesc=LOUTSTR
      else
        dmdsdesc=LOUTSTR
        idmdver=0
      endif

C Debug.
C      write(6,*) 'calendar ',calename
C      write(6,*) 'day types ',nbdaytype

C Loop through all of the calendar day types.
      do 30 idt=1,nbdaytype
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'day type demand periods',IER)
        K=0
        CALL EGETWI(OUTSTR,K,NBCAS(idt),0,MC,'W','periods in day',IER)
        IF(NBCAS(idt).EQ.0)goto 30
        DO 40 I=1,NBCAS(idt)
          CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain detl',IER)
          K=0
          if(ND.eq.7)then
            CALL EGETWI(OUTSTR,K,IBCGT(idt,I),-7,7,'W','casual type',
     &        IER)
          else
            IBCGT(idt,I)=1
          endif

          CALL EGETWI(OUTSTR,K,IBCGS(idt,I),0,24,'W','cas gain start',
     &      IER)
          CALL EGETWI(OUTSTR,K,IBCGF(idt,I),0,24,'W','cas gain finsh',
     &      IER)
          CALL EGETWR(OUTSTR,K,CBMGS(idt,I),0.,0.,'-','cas sensible',
     &      IER)
          CALL EGETWR(OUTSTR,K,CBMGL(idt,I),0.,0.,'-','cas latent',
     &      IER)
          CALL EGETWR(OUTSTR,K,BRADC(idt,I),0.0,1.,'W','cas rad frac',
     &      IER)
          CALL EGETWR(OUTSTR,K,BCONC(idt,I),0.0,1.,'W','cas conv frac',
     &      IER)
          IF(IBCGS(idt,I).GT.IBCGF(idt,I))then

C If periods are out of sync then return with a warning.
            write(outs,'(3a)') ' Period start-end mismatch in...',
     &        OUTSTR(1:50),'...'
            if(dll)then
              dllsubr='ERBDMD'
              dllmesg=outs
              ier=2
              CALL ERPFREE(IUO,ISTAT)
              return
            else
              call edisp(iuout,outs)
              IER=1
              CALL ERPFREE(IUO,ISTAT)
              RETURN
            endif
          endif
          X=BRADC(idt,I)+BCONC(idt,I)
          if(X.GT.1.02)then

C If radiant plus convective is over unity warn.
            write(outs,'(3a)') ' Radiant & convec frc>1.02 in...',
     &        OUTSTR(1:50),'...'
            if(dll)then
              dllsubr='ERBDMD'
              dllmesg=outs
              ier=2
              CALL ERPFREE(IUO,ISTAT)
              return
            else
              call edisp(iuout,outs)
              IER=1
              CALL ERPFREE(IUO,ISTAT)
              RETURN
            endif
          endif
          IF(X.LT..95.AND.ITRC.GT.1)call edisp(iuout,
     &                            ' Demand rad:con sum < 1.0')
   40   CONTINUE
        IF(IER.NE.0)RETURN
   30 continue

C Check to see if demand labels have been added to the end of the file
    8 CALL STRIPC(IUO,OUTSTR,99,ND,1,'Type labels',IERV)
      IF(ND.NE.7.OR.IERV.ne.0) THEN
        goto 1001
      ELSE
        K=0
        DO 1234 ITYP=1,7
          CALL EGETW(OUTSTR,K,blodlabel(ITYP),'W',
     &         'type label',IER) 
 1234   CONTINUE
      ENDIF      

C Check for areas associated with each demand label.
      CALL STRIPC(IUO,OUTSTR,99,ND,1,'Type areas',IERV)
      IF(ND.NE.7.OR.IERV.ne.0) THEN
        goto 1001
      ELSE
        K=0
        DO 1233 ITYP=1,7
          CALL EGETWR(OUTSTR,K,dmdfla(ITYP),0.,99999.,'W',
     &         'type area',IER) 
 1233   CONTINUE
      ENDIF      

 1235 CONTINUE    ! jump back point if types were not found.

C Distributed demands reporting if requested.
      if(ITRC.GE.1)call BDMDINF(iuout,IER)

C Now free project data file.
      CALL ERPFREE(IUO,ISTAT)
      RETURN

C Error messages.
 1001 call usrmsg(' ',' No type names found ...supplying defaults.','-')
      blodlabel(1)='Occup '
      blodlabel(2)='Lights'
      blodlabel(3)='SmPowr'      
      blodlabel(4)='Fans  '
      blodlabel(5)='Pumps '
      blodlabel(6)='Lifts '      
      blodlabel(7)='DHW   '      
      goto 1235

 1002 write(outs,'(3a)') 'ERBDMD: conversion error in...',
     &  OUTSTR(1:50),'...'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      if(dll)then
        dllsubr='ERBDMD'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUO,ISTAT)
        return
      else
        call edisp(iuout,outs)
        call edisp(iuout,outs2)
        IER=1
        CALL ERPFREE(IUO,ISTAT)
        RETURN
      endif

      END

C ****************** BDMDINF (same as in prjdmds.F)
C BDMDINF provides an English description of non-specific project demands
C from common block data.
      SUBROUTINE BDMDINF(ITRU,ier)

#include "building.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

      CHARACTER outs*124
      character calename*32,calentag*12,calendayname*32

      call edisp(itru,' ')
      call edisp(itru,'Notes: ')
      call edisp248(itru,dmdsdesc,72)

      do 30 idt=1,nbdaytype
        ldt=lnblnk(calentag(idt))
        WRITE(outs,'(3A,I3)')' No. ',calentag(idt)(1:ldt),' demands =',
     &    NBCAS(idt)
        call edisp(itru,outs)
        IF(NBCAS(idt).GT.0)THEN
          call edisp(itru,
     &  ' Gain Type Start Finish Sensible  Latent    Radiant   Convec')
          call edisp(itru,
     &  '           Hour  Hour   Magn. (W) Magn. (W) Frac      Frac')
          DO 1270 I=1,NBCAS(idt)
            if(IBCGT(idt,i).gt.0)then
              WRITE(outs,5360)I,blodlabel(IABS(IBCGT(idt,I))),'W  ',
     &          IBCGS(idt,I),IBCGF(idt,I),CBMGS(idt,I),CBMGL(idt,I),
     &          BRADC(idt,I),BCONC(idt,I)
            else
              WRITE(outs,5360)I,blodlabel(IABS(IBCGT(idt,I))),'Wm2',
     &          IBCGS(idt,I),IBCGF(idt,I),CBMGS(idt,I),CBMGL(idt,I),
     &          BRADC(idt,I),BCONC(idt,I)
            endif
            call edisp(itru,outs)
1270      CONTINUE
        ENDIF
        call edisp(itru,' ')
  30  continue

5360  FORMAT(I4,1x,A6,A3,I5,I3,F9.1,F9.1,F11.2,F11.2)

      RETURN
      END

C ******************** DMDSTOT ********************
C DMDSTOT retrieves max, min and totals (XMXUM & XMINSUM are distributed 
C totals while DMAX & DMIN are diversified totals) for non-specific 
C demands. 

      SUBROUTINE DMDSTOT(XMAX,XMIN,XTOT,XAVE)
#include "building.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      dimension XMAX(7),XMIN(7),XTOT(7),XAVE(7),NX(7)
      character prompts*52,prompte*52

      prompts=' Scanning non-specific demands...'
      prompte=' Scanning non-specific demands...done.'

C Setup parameters and call DDMDS for each output day to get required data.
C DDMDS recovers the data in VAL? (and averages output if required.)
      do 5 I=1,7
        XMAX(I)=-1.E+10
        XMIN(I)=1.E+10
        XTOT(I)=0.0
        XAVE(I)=0.0
        NX(I)=0
 5    continue
      call usrmsg(prompts,' ','-')
      do 10 IDAY=IODS,IODF

C Get days worth of data.
        call DDMDS(IDAY)

C Add values in VAL2 to the correct bins. IX in range of 15-21 so
C offset is 14
        do 400 I=1,7
          IX=I+14
          NDTS=24*NTS
          DO 421 J = 1,NDTS,NOUT
C            ih=J/NTS
            IF(IDAY.LE.IODS.AND.(FLOAT(J)/NTS).LT.IOH1)goto 421

            IF (VAL3(IX,J).GT.XMAX(i)) XMAX(i)=VAL3(IX,J)
            IF (VAL3(IX,J).LT.XMIN(i)) XMIN(i)=VAL3(IX,J)
            XTOT(i)=XTOT(i)+VAL3(IX,J)
            NX(i)=NX(i)+1
 421      CONTINUE
 400    CONTINUE
 10   continue

C Calculate averages.
      do 20 I=1,7
        if (NX(I).eq.0) then
          XAVE(I)=0.
        else
          XAVE(I)=XTOT(I)/float(NX(I))
        endif
 20   continue
      call usrmsg(prompte,' ','-')
      return

      END

C ******************** DDMDS ********************

C DDMDS reconstructs one day's values of each of 
C the project demands.
C To save space VAL3(15,I) to VAL3(21,I) are used for temporary storage.
C The method used shifts information from the demands file into the
C equivalent of bps timestep averaging.
C VAL3(15,?) holds occupant demands.
C VAL3(16,?) holds lighting demands.
C VAL3(17,?) holds small power demands.
C VAL3(18,?) holds fans demands.
C VAL3(19,?) holds pump demands.
C VAL3(20,?) holds lifts demands.
C VAL3(21,?) holds DHW demands.

      SUBROUTINE DDMDS(IDAY)
#include "building.h"

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      common/demandd/dhddt1,dhddt2,dhddt3,dhddt4,dhddt5,dhddt6,dhddt7

      IEND=24*NTS

C Initialise 'present values'.
      pddt1=0.0
      pddt2=0.0
      pddt3=0.0
      pddt4=0.0
      pddt5=0.0
      pddt6=0.0
      pddt7=0.0

C Note that the demand at the first timestep of the day is dependent
C on the demand at the last timestep of the previous day.    
C If IDAY is equal to the first day of the simulation then there is no
C previous day so assume DUCCT?= current day.
      IF(IDAY.EQ.IODS.AND.IDAY.EQ.ISDS)THEN
        CALL DMDTYP(IDAY,IEND,hddt1,hddt2,hddt3,hddt4,hddt5,
     &  hddt6,hddt7)
        Dhddt1=hddt1
        Dhddt2=hddt1
        Dhddt3=hddt1
        Dhddt4=hddt1
        Dhddt5=hddt1
        Dhddt6=hddt1
        Dhddt7=hddt1
      ELSEIF(IDAY.EQ.IODS.AND.IDAY.GT.ISDS)THEN

C Need to get the last timestep of the previous day to have the last 
C timestep for interpolation before recovering the demands
C for the day in question.
        CALL DMDTYP(IDAY-1,IEND,hddt1,hddt2,hddt3,hddt4,hddt5,
     &  hddt6,hddt7)
        Dhddt1=hddt1
        Dhddt2=hddt1
        Dhddt3=hddt1
        Dhddt4=hddt1
        Dhddt5=hddt1
        Dhddt6=hddt1
        Dhddt7=hddt1
      ELSEIF(IDAY.GT.IODS)THEN

C Subsequent recovery day - information on previous day has already
C been saved so proceed to get demands for IDAY.
      ENDIF

C For each time step determine each of the demands.
      DO 50, I=1,IEND
        CALL DMDTYP(IDAY,I,hddt1,hddt2,hddt3,hddt4,hddt5,hddt6,hddt7)

C Save average fluxs. If first timestep, then average with
C that of the last of the previous day.
        IF(I.EQ.1)THEN
          VAL3(15,I)=(hddt1+Dhddt1)*0.5
          VAL3(16,I)=(hddt2+Dhddt2)*0.5
          VAL3(17,I)=(hddt3+Dhddt3)*0.5
          VAL3(18,I)=(hddt4+Dhddt4)*0.5
          VAL3(19,I)=(hddt5+Dhddt5)*0.5
          VAL3(20,I)=(hddt6+Dhddt6)*0.5
          VAL3(21,I)=(hddt7+Dhddt7)*0.5
        ELSE
          VAL3(15,I)=(hddt1+Pddt1)*0.5
          VAL3(16,I)=(hddt2+Pddt2)*0.5
          VAL3(17,I)=(hddt3+Pddt3)*0.5
          VAL3(18,I)=(hddt4+Pddt4)*0.5
          VAL3(19,I)=(hddt5+Pddt5)*0.5
          VAL3(20,I)=(hddt6+Pddt6)*0.5
          VAL3(21,I)=(hddt7+Pddt7)*0.5
        ENDIF

C Shift future to present.
        Pddt1=hddt1
        Pddt2=hddt2
        Pddt3=hddt3
        Pddt4=hddt4
        Pddt5=hddt5
        Pddt6=hddt6
        Pddt7=hddt7
   50 CONTINUE

C Save the last timestep for use the next day.
      Dhddt1=hddt1
      Dhddt2=hddt2
      Dhddt3=hddt3
      Dhddt4=hddt4
      Dhddt5=hddt5
      Dhddt6=hddt6
      Dhddt7=hddt7
      RETURN
      END

C ******************** DMDTYP ********************

C DMDTYP is a generic routine to work out the
C demand types 1-7 at each time step.
C Given the information in common block bl3data returns:
C hddt1,hddt2,hddt3,hddt4,hddt5,hddt6,hddt7
C However, because of the averaging in bps, the beginning and ending
C of each period will have to be shifted in the calling routine.
      SUBROUTINE DMDTYP(IDAY,ITSTEP,hddt1,hddt2,hddt3,hddt4,hddt5,
     &  hddt6,hddt7)
#include "building.h"
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN

C Demands for weekdays/saturdays/sundays.
      COMMON/BL2/dmdfla(7)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C Assume that IYEAR is available via RESDEF.
C Determine whether IDAY is a weekday, Saturday or Sunday.
C Determine the month and day of month via EDAYR.
      CALL EDAYR(IDAY,IDAYN,IMTHN)
      CALL EWEEKD(IDAYN,IMTHN,IYEAR,IDWK)

C Find the hour of the day and initialise the values to be returned.
      HOUR=float(ITSTEP)/float(NTS)
      hddt1=0.0
      hddt2=0.0
      hddt3=0.0
      hddt4=0.0
      hddt5=0.0
      hddt6=0.0
      hddt7=0.0

C Note: Because there can be more than one demand of each
C type, hddt? etc. need to be summed.

      IF(IDWK.EQ.7)THEN

C If a Sunday then check if there are any demands at this hour.
        idt = 3
        IF(NBCAS(idt).GE.1)THEN
          DO 10 ICNN=1,NBCAS(idt)
            X7=CBMGS(idt,ICNN)
            IF(HOUR.GT.float(IBCGS(idt,ICNN)).AND.
     &         HOUR.LE.float(IBCGF(idt,ICNN)))THEN
              if(IBCGT(idt,ICNN).EQ.1)THEN
                hddt1=hddt1+X7
              ELSEIF(IBCGT(idt,ICNN).EQ.2)THEN
                hddt2=hddt2+X7
              ELSEIF(IBCGT(idt,ICNN).EQ.3)THEN
                hddt3=hddt3+X7
              ELSEIF(IBCGT(idt,ICNN).EQ.4)THEN
                hddt4=hddt4+X7
              ELSEIF(IBCGT(idt,ICNN).EQ.5)THEN
                hddt5=hddt5+X7
              ELSEIF(IBCGT(idt,ICNN).EQ.6)THEN
                hddt6=hddt6+X7
              ELSEIF(IBCGT(idt,ICNN).EQ.7)THEN
                hddt7=hddt7+X7
              ENDIF
              if(IBCGT(idt,ICNN).EQ.-1)THEN
                hddt1=hddt1+(X7*dmdfla(1))
              ELSEIF(IBCGT(idt,ICNN).EQ.-2)THEN
                hddt2=hddt2+(X7*dmdfla(2))
              ELSEIF(IBCGT(idt,ICNN).EQ.-3)THEN
                hddt3=hddt3+(X7*dmdfla(3))
              ELSEIF(IBCGT(idt,ICNN).EQ.-4)THEN
                hddt4=hddt4+(X7*dmdfla(4))
              ELSEIF(IBCGT(idt,ICNN).EQ.-5)THEN
                hddt5=hddt5+(X7*dmdfla(5))
              ELSEIF(IBCGT(idt,ICNN).EQ.-6)THEN
                hddt6=hddt6+(X7*dmdfla(6))
              ELSEIF(IBCGT(idt,ICNN).EQ.-7)THEN
                hddt7=hddt7+(X7*dmdfla(7))
              ENDIF
            ENDIF
   10     CONTINUE
        ENDIF
      ELSEIF(IDWK.EQ.6)THEN

C If a Saturday then check if there are any demands at this hour.
        idt = 2
        IF(NBCAS(idt).GE.1)THEN
          DO 20 ICNN=1,NBCAS(idt)
            X6=CBMGS(idt,ICNN)
            IF(HOUR.GT.float(IBCGS(idt,ICNN)).AND.
     &              HOUR.LE.float(IBCGF(idt,ICNN)))THEN
              if(IBCGT(idt,ICNN).EQ.1)THEN
                hddt1=hddt1+X6
              ELSEIF(IBCGT(idt,ICNN).EQ.2)THEN
                hddt2=hddt2+X6
              ELSEIF(IBCGT(idt,ICNN).EQ.3)THEN
                hddt3=hddt3+X6
              ELSEIF(IBCGT(idt,ICNN).EQ.4)THEN
                hddt4=hddt4+X6
              ELSEIF(IBCGT(idt,ICNN).EQ.5)THEN
                hddt5=hddt5+X6
              ELSEIF(IBCGT(idt,ICNN).EQ.6)THEN
                hddt6=hddt6+X6
              ELSEIF(IBCGT(idt,ICNN).EQ.7)THEN
                hddt7=hddt7+X6
              ENDIF
              if(IBCGT(idt,ICNN).EQ.-1)THEN
                hddt1=hddt1+(X6*dmdfla(1))
              ELSEIF(IBCGT(idt,ICNN).EQ.-2)THEN
                hddt2=hddt2+(X6*dmdfla(2))
              ELSEIF(IBCGT(idt,ICNN).EQ.-3)THEN
                hddt3=hddt3+(X6*dmdfla(3))
              ELSEIF(IBCGT(idt,ICNN).EQ.-4)THEN
                hddt4=hddt4+(X6*dmdfla(4))
              ELSEIF(IBCGT(idt,ICNN).EQ.-5)THEN
                hddt5=hddt5+(X6*dmdfla(5))
              ELSEIF(IBCGT(idt,ICNN).EQ.-6)THEN
                hddt6=hddt6+(X6*dmdfla(6))
              ELSEIF(IBCGT(idt,ICNN).EQ.-7)THEN
                hddt7=hddt7+(X6*dmdfla(7))
              ENDIF
            ENDIF
   20     CONTINUE
        ENDIF
      ELSE

C If a weekday then check if there are any demands at this hour.
        idt = 1
        IF(NBCAS(idt).GE.1)THEN
          DO 30 ICNN=1,NBCAS(idt)
            X=CBMGS(idt,ICNN)
            IF(HOUR.GT.float(IBCGS(idt,ICNN)).AND.
     &              HOUR.LE.float(IBCGF(idt,ICNN)))THEN
              if(IBCGT(idt,ICNN).EQ.1)THEN
                hddt1=hddt1+X
              ELSEIF(IBCGT(idt,ICNN).EQ.2)THEN
                hddt2=hddt2+X
              ELSEIF(IBCGT(idt,ICNN).EQ.3)THEN
                hddt3=hddt3+X
              ELSEIF(IBCGT(idt,ICNN).EQ.4)THEN
                hddt4=hddt4+X
              ELSEIF(IBCGT(idt,ICNN).EQ.5)THEN
                hddt5=hddt5+X
              ELSEIF(IBCGT(idt,ICNN).EQ.6)THEN
                hddt6=hddt6+X
              ELSEIF(IBCGT(idt,ICNN).EQ.7)THEN
                hddt7=hddt7+X
              ENDIF
              if(IBCGT(idt,ICNN).EQ.-1)THEN
                hddt1=hddt1+(X*dmdfla(1))
              ELSEIF(IBCGT(idt,ICNN).EQ.-2)THEN
                hddt2=hddt2+(X*dmdfla(2))
              ELSEIF(IBCGT(idt,ICNN).EQ.-3)THEN
                hddt3=hddt3+(X*dmdfla(3))
              ELSEIF(IBCGT(idt,ICNN).EQ.-4)THEN
                hddt4=hddt4+(X*dmdfla(4))
              ELSEIF(IBCGT(idt,ICNN).EQ.-5)THEN
                hddt5=hddt5+(X*dmdfla(5))
              ELSEIF(IBCGT(idt,ICNN).EQ.-6)THEN
                hddt6=hddt6+(X*dmdfla(6))
              ELSEIF(IBCGT(idt,ICNN).EQ.-7)THEN
                hddt7=hddt7+(X*dmdfla(7))
              ENDIF
            ENDIF
   30     CONTINUE
        ENDIF
      ENDIF

      RETURN
      END
