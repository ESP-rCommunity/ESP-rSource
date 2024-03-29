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

C This routine scans a section of a miscellaneous (generic) component data base  
C for component data (based calling mode). The data recovered can be either 
C descriptive strings (for menus), data items or text fields. 
C Current component types which are held include:
C TYPE   - 1-special materials, 2-conductors, 3-power only components,
C          4-building electrical loads, 5-motors and generators.
C MODE   - scanning mode.
C SSTR   - string array passed back from the routine.
C          Strings can also be passed into the routine for searching.
C VAR    - array of real numbers passed back from the routine.
C SPMISC - another array of real numbers which can be passed back.
C NIT    - a counter passed back from or a number passed into 
C          the routine.

      SUBROUTINE MCDBSCAN(TYPE,MODE,SSTR,VAR,SPMISC,NIT,IER)
#include "building.h"
#include "plant.h"
#include "power.h" 
#include "esprdbfile.h" 
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Note string buffer array supports up to 90 lines of text
C for a component.
      DIMENSION SSTR(90),VAR(MPWDAT),SPMISC(MPWDAT)

      CHARACTER OUTSTR*124
      CHARACTER SSTR*72
      CHARACTER DIVR1*6,DIVR2*6,DIVR3*6
      character lworking*144,fs*1
      REAL VAR,SPMISC
      INTEGER MODE,TYPE
      LOGICAL OK,dok,unixok

      IER=0
      OK=.FALSE.
      helpinsub='mcdbscn'  ! set for subroutine
      helptopic='scan_misc_database'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Clear strings outside of the if else structure
      write(DIVR1,'(a)')'      '
      write(DIVR2,'(a)')'      '
      write(DIVR3,'(a)')'      '
      
C Open the miscellaneous components database based on the
C name held in the model cfg file. 
 2    IF(MCMPDBFL(1:3).EQ.'UNK'.or.MCMPDBFL(1:2).EQ.'  ') THEN
        continue
      ENDIF

C Assign field dividers based on the call type.
      IF(TYPE.EQ.1) THEN
        write(DIVR1,'(a)')'*SPMDB'
        write(DIVR2,'(a)')'*SPECM'
        write(DIVR3,'(a)')'*ENDSP'
      ELSEIF(TYPE.EQ.2) THEN
        write(DIVR1,'(a)')'*POWRO'
        write(DIVR2,'(a)')'*POWOC'
        write(DIVR3,'(a)')'*ENDPO'
      ELSEIF(TYPE.EQ.3) THEN
        write(DIVR1,'(a)')'*ECOND'
        write(DIVR2,'(a)')'*CONCO'
        write(DIVR3,'(a)')'*ENDCO'
      ELSE
        CALL EDISP(IUOUT,'Unknown miscellaneous component type!')
        RETURN
      ENDIF
      IMCFIL=IFIL+1

C Open the miscellaneous components database based on value of ipathmsc.
      if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
        CALL EFOPSEQ(IMCFIL,MCMPDBFL,1,IER)
      elseif(ipathmsc.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    MCMPDBFL(1:lnblnk(MCMPDBFL))
        CALL EFOPSEQ(IMCFIL,lworking,1,IER)
      endif

      IF(IER.NE.0) THEN
        dok=.true.
        CALL EASKOK(
     &    'Could not locate the miscellaneous components database!',
     &    'Retry?',OK,nbhelp)
        IF(.NOT.OK) THEN 
          IER=1
          CALL ERPFREE(IMCFIL,ISTAT)
          RETURN
        ELSE
          write(MCMPDBFL,'(a)') 'UNKNOWN'
          GOTO 2
        ENDIF
      ENDIF

C Check that the opened file is a special materials database.
  5   CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'check file',IER)
      IF(IER.NE.0) THEN
        CALL EDISP
     &   (IUOUT,'Problem reading miscellaneous components database!')
        IER=1
        CALL ERPFREE(IMCFIL,ISTAT)
        RETURN

C Check for the match with the appropriate section of the database.
      ELSEIF(OUTSTR(1:6).NE.DIVR1) THEN
        GOTO 5
      ENDIF

C Start stripping according to the mode.
      IF(MODE.EQ.1) THEN

C Get the category headings.
        CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
        CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'at no',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)
        NIT=IVAL
        DO 10 ICAT=1,NIT
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'category ',IER)
          K=0
          SSTR(ICAT)= OUTSTR(1:72) 
  10    CONTINUE 
        CALL ERPFREE(IMCFIL,ISTAT)
        RETURN    
      
      ELSEIF(MODE.EQ.2) THEN

C Recover the required category number. (NIT serves
C as an input and output parameter.
        ICAT=NIT
        NIT=0
  20    CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
        IF(OUTSTR(1:6).EQ.DIVR2) THEN

C If tag is *SPECM, *POWOC or *CONCO read past two more lines to get
C the category number into variable ival and compare with icat.
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'desc ',IER)
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'desc ',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)
          IF(IVAL.EQ.ICAT) THEN 
            CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'desc ',IER)         
            IF(OUTSTR(1:5).EQ.'*DESC') NIT=NIT+1
            CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'desc ',IER)
            SSTR(NIT)=OUTSTR(1:LNBLNK(OUTSTR))
            GOTO 20
          ELSE
            GOTO 20
          ENDIF
        ELSEIF(OUTSTR(1:6).EQ.DIVR3) THEN
          CALL ERPFREE(IMCFIL,ISTAT)
          RETURN
        ELSE
          GOTO 20
        ENDIF
      ELSEIF(MODE.EQ.3) THEN

        DO 25 I=1,MSPMDAT
          SPMISC(I)=0
  25    CONTINUE

C Scan for component data, the required component is contained in
C SSTR(1). This is matched to the required component description.
  30    CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
        IF(OUTSTR(1:6).EQ.DIVR2) THEN
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)
          SPMISC(1)=IVAL
          CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)
          SPMISC(2)=IVAL
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER) 
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)         
          IF(OUTSTR(1:20).EQ.SSTR(1)(1:20)) THEN
            CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER) 
            CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER) 
            K=0
            CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)

C Record the number of data items in the SPMISC array.
            SPMISC(3)=FLOAT(IVAL)

C If a power only component, get the number of data strings as well.
            IF(TYPE.EQ.2) THEN
              CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)
              SPMISC(4)=FLOAT(IVAL)
            ENDIF

C Read in the default data values into the SSTR array.
            DO 40 IDAT=1,INT(SPMISC(3))
              CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
              K=0
              CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','data',IER)
              VAR(IDAT)=VAL
              SSTR(IDAT)=OUTSTR(10:72)
  40        CONTINUE

C If the component is power only, read the rest of the string data.
            IF(TYPE.EQ.2) THEN
              DO 45 IDAT=1,INT(SPMISC(4))*2
                CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
                K=0
                SSTR(IDAT+INT(SPMISC(3)))=OUTSTR(1:72)
  45          CONTINUE
            ENDIF
            CALL ERPFREE(IMCFIL,ISTAT)
            RETURN
          ELSE
            GOTO 30
          ENDIF
        ELSEIF(OUTSTR(1:6).EQ.DIVR3) THEN
          CALL EDISP(IUOUT,'No data found!')
          IER=1
          CALL ERPFREE(IMCFIL,ISTAT)
          RETURN
        ELSE  
          GOTO 30 
        ENDIF
      ELSEIF(MODE.EQ.4) THEN
  50    CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
        IF(OUTSTR(1:6).EQ.DIVR2) THEN
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER) 
          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)         
          IF(OUTSTR(1:20).EQ.SSTR(1)(1:20))THEN
            CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER) 
            CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER) 
            K=0
            CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)

C Skip the data values.
            DO 60 IDAT=1,IVAL
              CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
  60        CONTINUE

            CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER)
            IF(OUTSTR(1:5).EQ.'*TEXT') THEN
  70          CALL STRIPC(IMCFIL,OUTSTR,99,ND,0,'cat ',IER) 
              IF(OUTSTR(1:4).EQ.'*END') THEN
                CALL ERPFREE(IMCFIL,ISTAT)
                RETURN
              ENDIF

C Write out text until end of field/entry or file marker found.           
              CALL EDISP(IUOUT,OUTSTR(1:LNBLNK(OUTSTR)))
              GOTO 70
            ELSE
              CALL EDISP(IUOUT,'No more information is available.')
              CALL ERPFREE(IMCFIL,ISTAT)
              RETURN
            ENDIF
          ELSE
            GOTO 50
          ENDIF
        ELSEIF(OUTSTR(1:6).EQ.DIVR3) THEN
          CALL EDISP(IUOUT,'ERROR - no data found.')
          IER=1
          CALL ERPFREE(IMCFIL,ISTAT)
          RETURN
        ELSE  
          GOTO 50 
        ENDIF

C Get the text description for the component.
      ELSE
        CALL EDISP(IUOUT,'Unknown mode for SPMDBSCAN!')
        IER=1
        CALL ERPFREE(IMCFIL,ISTAT)
        RETURN
      ENDIF
      CALL ERPFREE(IMCFIL,ISTAT)
      RETURN
      END
