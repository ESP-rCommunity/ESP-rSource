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

C This routine scans a section of the defined electrical components data base  
C for special materials data (based on the mode passed to the routine). This
C can be either descriptive strings (for menus); data items or ; 
C text fields. 
C The routine is called with:
C TYPE - 1-special materials 2-conductors, 3-power only components
C        4-building side electrical loads 5-motors and generators.
C MODE - scanning mode
C SSTR - string array passed back from the routine
C        strings can also be passed into the routine for searching
C VAR  - array of real numbers (data) passed back from the routine
C SPMISC - another array of real numbers which can be passed back
C NIT  - a counter passed back from or a number passed into 
C        the routine.

      SUBROUTINE ECDBSCAN(TYPE,MODE,SSTR,VAR,SPMISC,NIT,IER)

#include "building.h" 
#include "esprdbfile.h"
#include "power.h" 
#include "help.h"
     
      integer lnblnk  ! function definition

      COMMON/ECMPDBNAM/ECMPDBFL
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL

      DIMENSION SSTR(90),VAR(MPWDAT),SPMISC(MPWDAT)

      CHARACTER*124 OUTSTR
      CHARACTER ECMPDBFL*72,SSTR*72,ltmp*72,DFILE*72
      CHARACTER DIVR1*6,DIVR2*6,DIVR3*6
      character*72 DFCFD,DECMPDBFL,DICONDBFL
      REAL VAR,SPMISC
      INTEGER MODE,TYPE
      LOGICAL OK

      IER=0
      ltmp='  '
      OK=.FALSE.
      helpinsub='ecdbscb'  ! set for subroutine
      helptopic='electrical_cmps_db'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
C Open the electrical components database file 
 2    IF(ECMPDBFL(1:3).EQ.'UNK'.or.ECMPDBFL(1:2).EQ.'  ') THEN

C Hard coded default data base at the moment, later add to default data
C base list.
C << this should be in the defaults file >>
        ECMPDBFL=DECMPDBFL
        DFILE=DECMPDBFL
        ltmp=ECMPDBFL
        CALL EASKS(ltmp,'Electrical components database filename?',
     &     ' ',72,DFILE,'ecomp dbnam',IER,nbhelp)
        if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN') ECMPDBFL=ltmp
      ENDIF

C Assign field dividers based on the call type.
      DIVR1='    '
      DIVR2='    '
      DIVR3='    '
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
        CALL EDISP(IUOUT,'Unknown electrical component type!')
        RETURN
      ENDIF
      IECFIL=IFIL+1
      CALL EFOPSEQ(IECFIL,ECMPDBFL,1,IER)
      IF(IER.NE.0) THEN
        CALL EASKOK('Electrical components database not found!',
     &    'Retry?',OK,nbhelp)
        IF(.NOT.OK) THEN 
          IER=1
          CALL ERPFREE(IECFIL,ISTAT)
          RETURN
        ELSE
          ECMPDBFL='UNKNOWN'
          GOTO 2
        ENDIF
      ENDIF


C Check that the opened file is a special materials database.
  5   CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'check file',IER)
      
C Check for the end of file
      IF(IER.NE.0) THEN
        CALL EDISP(IUOUT,'ERROR - problem reading electrical database.')
        IER=1
        CALL ERPFREE(IECFIL,ISTAT)
        RETURN

C Check for the match with the appropriate section of the database.
      ELSEIF(OUTSTR(1:6).NE.DIVR1) THEN
        GOTO 5
      ENDIF

C Start stripping according to the mode.
      IF(MODE.EQ.1) THEN

C Get the category headings.
        CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
        CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'at no',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)
        NIT=IVAL
        DO 10 ICAT=1,NIT
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'category ',IER)
          K=0
          SSTR(ICAT)= OUTSTR(1:72) 
  10    CONTINUE 
        CALL ERPFREE(IECFIL,ISTAT)
        RETURN    
      
      ELSEIF(MODE.EQ.2) THEN

C Firstly recover the required category number the NIT variable is overloaded to
C act as an input and output parameter.
        ICAT=NIT
        NIT=0
  20    CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
        IF(OUTSTR(1:6).EQ.DIVR2) THEN
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'desc ',IER)
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'desc ',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)
          IF(IVAL.EQ.ICAT) THEN 
            CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'desc ',IER)         
            IF(OUTSTR(1:5).EQ.'*DESC') NIT=NIT+1
            CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'desc ',IER)
            SSTR(NIT)=OUTSTR(1:LNBLNK(OUTSTR))
            GOTO 20
          ELSE
            GOTO 20
          ENDIF
        ELSEIF(OUTSTR(1:6).EQ.DIVR3) THEN
          CALL ERPFREE(IECFIL,ISTAT)
          RETURN
        ELSE
          GOTO 20
        ENDIF
      ELSEIF(MODE.EQ.3) THEN

C Set MISC variables to zero
        DO 25 I=1,MSPMDAT
          SPMISC(I)=0
  25    CONTINUE

C Scan for component data, the required component is contained in
C SSTR(1) this is matched to the required component description.
  30    CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
        IF(OUTSTR(1:6).EQ.DIVR2) THEN
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)
          SPMISC(1)=IVAL
          CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)
          SPMISC(2)=IVAL
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER) 
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)         
          IF(OUTSTR(1:20).EQ.SSTR(1)(1:20)) THEN
            CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER) 
            CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER) 
            K=0
            CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)

C Record the number of data items in the MISC array.
            SPMISC(3)=FLOAT(IVAL)

C If a power only component get the number of data strings as well
            IF(TYPE.EQ.2) THEN
              CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)
              SPMISC(4)=FLOAT(IVAL)
            ENDIF

C Read in the default data values into the SSTR array.
            DO 40 IDAT=1,INT(SPMISC(3))
              CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
              K=0
              CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','data',IER)
              VAR(IDAT)=VAL
              SSTR(IDAT)=OUTSTR(10:72)
  40        CONTINUE

C If the component is power only read the rest of the string data
            IF(TYPE.EQ.2) THEN
              DO 45 IDAT=1,INT(SPMISC(4))*2
                CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
                K=0
                SSTR(IDAT+INT(SPMISC(3)))=OUTSTR(1:72)
  45          CONTINUE
            ENDIF
            CALL ERPFREE(IECFIL,ISTAT)
            RETURN
          ELSE
            GOTO 30
          ENDIF
        ELSEIF(OUTSTR(1:6).EQ.DIVR3) THEN
          CALL EDISP(IUOUT,'ERROR - no data found.')
          IER=1
          CALL ERPFREE(IECFIL,ISTAT)
          RETURN
        ELSE  
          GOTO 30 
        ENDIF
      ELSEIF(MODE.EQ.4) THEN
  50    CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
        IF(OUTSTR(1:6).EQ.DIVR2) THEN
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER) 
          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)         
          IF(OUTSTR(1:20).EQ.SSTR(1)(1:20))THEN
            CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER) 
            CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER) 
            K=0
            CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','categories',IER)

C Skip the data values.
            DO 60 IDAT=1,IVAL
              CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
  60        CONTINUE

            CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER)
            IF(OUTSTR(1:5).EQ.'*TEXT') THEN
  70          CALL STRIPC(IECFIL,OUTSTR,99,ND,0,'cat ',IER) 
              IF(OUTSTR(1:4).EQ.'*END') THEN
                CALL ERPFREE(IECFIL,ISTAT)
                RETURN
              ENDIF

C Write out text filed until end of field/entry or file marker found.           
              CALL EDISP(IUOUT,OUTSTR(1:LNBLNK(OUTSTR)))
              GOTO 70
            ELSE
              CALL EDISP(IUOUT,'No more information is available.')
              CALL ERPFREE(IECFIL,ISTAT)
              RETURN
            ENDIF
          ELSE
            GOTO 50
          ENDIF
        ELSEIF(OUTSTR(1:6).EQ.DIVR3) THEN
          CALL EDISP(IUOUT,'ERROR - no data found.')
          IER=1
          CALL ERPFREE(IECFIL,ISTAT)
          RETURN
        ELSE  
          GOTO 50 
        ENDIF
C Get the text description for the component

      ELSE
        CALL EDISP(IUOUT,'ERROR - not a known mode for SPMDBSCAN.')
        IER=1
        CALL ERPFREE(IECFIL,ISTAT)
        RETURN
      ENDIF
      CALL ERPFREE(IECFIL,ISTAT)
      RETURN
      END
