C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C A collection of routines dealing with dispersed energy demands (for fans, lifts etc.).
C    ERBDMD: reads the model-defined dispersed demand profiles.
C    DMDSTOT: retrieves max, min, average and total values for each demand.
C    DDMDS: reconstructs one day's values for each demand.
C    DMDTYP: determines the demand types 1-7 (MGTY) at each time step.

C ********** ERBDMD
C Reads the model-defined dispersed demand profiles. (Similar to routine in prjdmds.F
C except does not set idmdinc or report result.)

      SUBROUTINE ERBDMD(ITRC,IUO,IER)
#include "building.h"
#include "espriou.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN,IEOUT
      common/BLM2/dmdfla(MGTY)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER :: icalender,nbcaldays,nbdaytype

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

      character outstr*124,loutstr*248
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
     &    bdmds(1:lnblnk(bdmds)),' could not be opened!'
        if(dll)then
          dllsubr='ERBDMD'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(ioout,' ')
          call edisp(iuout,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') bdmds(1:lnblnk(bdmds))

C Read lines from file, discarding comments. To allow for operation
C names with spaces copy directly from OUTSTR rather than parsing
C it into words. Use long stip for dmdsdesc line. 
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

C Loop through all calendar day types. (Limit is 7 day types.)
      do 30 idt=1,nbdaytype
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'day type demand periods',IER)
        K=0
        CALL EGETWI(OUTSTR,K,NBCAS(idt),0,MC,'W','periods in day',IER)
        IF(NBCAS(idt).EQ.0)goto 30
        DO 40 I=1,NBCAS(idt)
          CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain detl',IER)
          K=0
          if(ND.eq.7)then
            CALL EGETWI(OUTSTR,K,IBCGT(idt,I),-7,MGTY,'W',
     &        'casual type',IER)
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
            write(outs,'(3a)') 'Period start-end mismatch in ',
     &        OUTSTR(1:50),'!'
            if(dll)then
              dllsubr='ERBDMD'
              dllmesg=outs
              ier=2
              CALL ERPFREE(IUO,ISTAT)
              return
            else
              call edisp(iuout,' ')
              call edisp(iuout,outs)
              IER=1
              CALL ERPFREE(IUO,ISTAT)
              RETURN
            endif
          endif
          X=BRADC(idt,I)+BCONC(idt,I)
          if(X.GT.1.02)then

C If radiant plus convective is > 1.0 issue warning..
            write(outs,'(3a)') 'Radiant & convec summation > 1.0 in ',
     &        OUTSTR(1:50),'!'
            if(dll)then
              dllsubr='ERBDMD'
              dllmesg=outs
              ier=2
              CALL ERPFREE(IUO,ISTAT)
              return
            else
              call edisp(iuout,' ')
              call edisp(iuout,outs)
              IER=1
              CALL ERPFREE(IUO,ISTAT)
              RETURN
            endif
          endif
          IF(X.LT..95.AND.ITRC.GT.1)call edisp(iuout,
     &                            'Demand rad+con < 1.0!')
   40   CONTINUE
        IF(IER.NE.0)RETURN
   30 continue

C Check if demand labels have been added to the end of the file.
    8 CALL STRIPC(IUO,OUTSTR,99,ND,1,'Dispersed type labels',IERV)
      IF(ND.NE.7.OR.IERV.ne.0) THEN
        goto 1001
      ELSE
        K=0
        DO 1234 ITYP=1,MGTY
          CALL EGETW(OUTSTR,K,blodlabel(ITYP),'W',
     &         'type label',IER) 
 1234   CONTINUE
      ENDIF      

C Check for areas associated with each demand label.
      CALL STRIPC(IUO,OUTSTR,99,ND,1,'Dispersed type areas',IERV)
      IF(ND.NE.7.OR.IERV.ne.0) THEN
        goto 1001
      ELSE
        K=0
        DO 1233 ITYP=1,MGTY
          CALL EGETWR(OUTSTR,K,dmdfla(ITYP),0.,99999.,'W',
     &      'dispersed demand type area',IER) 
 1233   CONTINUE
      ENDIF      

 1235 CONTINUE    ! jump back point if types not found.

C Free project data file.
      CALL ERPFREE(IUO,ISTAT)
      RETURN

C Error messages.
 1001 call usrmsg(' ','No type names found ... supplying defaults.','-')
      blodlabel(1)='Other '
      blodlabel(2)='Lights'
      blodlabel(3)='SmallPower'      
      blodlabel(4)='Fans  '
      blodlabel(5)='Pumps '
      blodlabel(6)='Lifts '      
      blodlabel(7)='DomesticHW'      
      goto 1235

 1002 write(outs,'(3a)') 'ERBDMD: conversion error in ',
     &  OUTSTR(1:50),'!'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      if(dll)then
        dllsubr='ERBDMD'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUO,ISTAT)
        return
      else
        call edisp(iuout,' ')
        call edisp(iuout,outs)
        call edisp(iuout,outs2)
        IER=1
        CALL ERPFREE(IUO,ISTAT)
        RETURN
      endif

      END


C ******************** DMDSTOT ********************
C Retrieve max, min, average and total values for each demand. 

      SUBROUTINE DMDSTOT(XMAX,XMIN,XTOT,XAVE)
#include "building.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      dimension XMAX(MGTY),XMIN(MGTY),XTOT(MGTY),XAVE(MGTY)
      dimension NX(MGTY)
      character prompts*52,prompte*52

      prompts='Scanning non-specific demands ...'
      prompte='Scanning non-specific demands ... done.'

C Set up parameters and call DDMDS for each output day to get required data.
C DDMDS recovers the data in VAL3 positions 15-21 (and averages output if required.)
      do 5 I=1,MGTY
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

C Add values in VAL3 to the correct bins. IX in range of 15-21 so
C offset from the gain type is 14 within the VAL3 array.
        do 400 I=1,MGTY
          IX=I+14
          NDTS=24*NTS
          DO 421 J = 1,NDTS,NOUT
            IF(IDAY.LE.IODS.AND.(FLOAT(J)/NTS).LT.IOH1)goto 421
            IF (VAL3(IX,J).GT.XMAX(i)) XMAX(i)=VAL3(IX,J)
            IF (VAL3(IX,J).LT.XMIN(i)) XMIN(i)=VAL3(IX,J)
            XTOT(i)=XTOT(i)+VAL3(IX,J)
            NX(i)=NX(i)+1
 421      CONTINUE
 400    CONTINUE
 10   continue

C Calculate averages.
      do 20 I=1,MGTY
        if (NX(I).eq.0) then
          XAVE(I)=0.
        else
          XAVE(I)=XTOT(I)/float(NX(I))
        endif
 20   continue
      call usrmsg(prompte,' ','-')
      return

      END

C ********** DDMDS

C Reconstructs one day's values for each demand.
C To save space VAL3(15,I) to VAL3(21,I) are used for temporary storage.
C The method used shifts information from the demands file into the
C equivalent of bps timestep averaging.
C   VAL3(15,?) - other demands.
C   VAL3(16,?) - lighting demands.
C   VAL3(17,?) - small power demands.
C   VAL3(18,?) - fans demands.
C   VAL3(19,?) - pump demands.
C   VAL3(20,?) - lifts demands.
C   VAL3(21,?) - DHW demands.

      SUBROUTINE DDMDS(IDAY)
#include "building.h"

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      common/demandd/dhddt1,dhddt2,dhddt3,dhddt4,dhddt5,dhddt6,dhddt7

      IEND=24*NTS

C Initialise 'present' values.
      pddt1=0.0; pddt2=0.0; pddt3=0.0; pddt4=0.0
      pddt5=0.0; pddt6=0.0; pddt7=0.0

C Note that the demand at the first time step of the day is dependent
C on the demand at the last time step of the previous day.    
C If IDAY is equal to the first day of the simulation then there is no
C previous day so assume DUCCT?= current day.
      IF(IDAY.EQ.IODS.AND.IDAY.EQ.ISDS)THEN
        CALL DMDTYP(IDAY,IEND,hddt1,hddt2,hddt3,hddt4,hddt5,
     &    hddt6,hddt7)
        Dhddt1=hddt1; Dhddt2=hddt1; Dhddt3=hddt1; Dhddt4=hddt1
        Dhddt5=hddt1; Dhddt6=hddt1; Dhddt7=hddt1
      ELSEIF(IDAY.EQ.IODS.AND.IDAY.GT.ISDS)THEN

C Get the last time step of the previous day to have the last 
C time step for interpolation before recovering the demands
C for the day in question.
        CALL DMDTYP(IDAY-1,IEND,hddt1,hddt2,hddt3,hddt4,hddt5,
     &    hddt6,hddt7)
        Dhddt1=hddt1; Dhddt2=hddt1; Dhddt3=hddt1; Dhddt4=hddt1
        Dhddt5=hddt1; Dhddt6=hddt1; Dhddt7=hddt1
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
        Pddt1=hddt1; Pddt2=hddt2; Pddt3=hddt3; Pddt4=hddt4
        Pddt5=hddt5; Pddt6=hddt6; Pddt7=hddt7
   50 CONTINUE

C Save the last timestep for use the next day.
      Dhddt1=hddt1; Dhddt2=hddt2; Dhddt3=hddt3; Dhddt4=hddt4
      Dhddt5=hddt5; Dhddt6=hddt6; Dhddt7=hddt7
      RETURN
      END

C ********** DMDTYP

C Determines the demand types 1-7 (MGTY) at each time step.
C Given the information in common block bl3data, the routine returns:
C hddt1,hddt2,hddt3,hddt4,hddt5,hddt6,hddt7. However, because of the
C averaging in bps, the beginning and ending of each period is
C shifted in the calling routine.

      SUBROUTINE DMDTYP(IDAY,ITSTEP,hddt1,hddt2,hddt3,hddt4,hddt5,
     &  hddt6,hddt7)
#include "building.h"
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Demands floor area for each demand type.
      COMMON/BLM2/dmdfla(MGTY)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C Assume that IYEAR is available via RESDEF.
C Determine whether IDAY is a weekday, Saturday or Sunday.
C Determine the month and day of month via EDAYR.
C << This will need to change to deal with calendar day types. >>

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

C Because there can be more than one demand of each
C type, hddt? etc. need to be summed.

      IF(IDWK.EQ.7)THEN

C Sunday - check if there are any demands at this hour.
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
              ELSE
C Warm if more than 7 types.
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
              ELSE
C Warm if more than 7 types.
              ENDIF
            ENDIF
   10     CONTINUE
        ENDIF
      ELSEIF(IDWK.EQ.6)THEN

C Saturday - check if there are any demands at this hour.
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
              ELSE
C Warm if more than 7 types.
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
              ELSE
C Warm if more than 7 types.
              ENDIF
            ENDIF
   20     CONTINUE
        ENDIF
      ELSE

C Weekday - check if there are any demands at this hour.
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
              ELSE
C Warm if more than 7 types.
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
              ELSE
C Warm if more than 7 types.
              ENDIF
            ENDIF
   30     CONTINUE
        ENDIF
      ENDIF

      RETURN
      END
