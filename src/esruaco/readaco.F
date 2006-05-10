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

C SCNACO : Scans the acoustic file (.aco)
C SCNZONEACO : Scans the information at the zone level and detect if occupant and furniture
C IPubFur : Using a public or furniture name, returns the corresponding index in the Aco file.

C ******************** SCNACO ******************************
C Scans the acoustic file (.aco) and records all 
C information related to the selected zone


C Variables lists for SCNACO 
C -----------------------------------
C IACO     (Integer)  Unit channel of the BCFfile file
C ACOOK    (Logical)  TRUE if ACOFile exits
C PubAbsCo(MaxPub,MNbAbs) Absorbtion coefficient for a public type [-]
C PubType(MaxPub) Name of the public 
C FurAbsCo(MaxPub,MNbAbs) Absorbtion coefficient for a furniture type [-]
C FurType(MaxFur) Name of the furniture
C FurCat(MaxFur)  Category of the furniture (unit / Surf)
C NbPub(MaxPub)   Nb of public for the current public type
C XNbFur(MaxFur)   Nb of unit or surface area for the current furniture

C MaxPub   (Integer)  Nb max of public (acoustic.h)
C MaxFur   (Integer)  Nb max of furniture (acoustic.h)

C Variables list
C---------------

C ACOfile = file name of BCF
C IEask : 1: ask for file display, 0: run in silent mode 
C IUC = Entrance file unit number  (IBCF)
C IER Error code
C IUIN = User input channel
C IUOUT = the message channel
C   1 = should exist, message & error -301 if not. 
C IW(Integer) : 1 = read BCF silently (=2 if not)


      SUBROUTINE SCNACO(IEask,IER)
#include "building.h"
#include "acoustic.h"


      COMMON/OUTIN/IUOUT,IUIN
      COMMON/EVGENL/BCFOK
      COMMON/MetNam/ACOName,CalName(4),LabFreq(MNbAbs)
      COMMON/IFileACO/IACO
      COMMON/AbsoCoef/SysAbsCo(MMLC,MNbAbs),PubAbsCo(MaxPub,MNbAbs),
     &                FurAbsCo(MaxPub,MNbAbs),AirAbs(MNbAbs)
      COMMON/AbsCHAR/PubType(MaxPub),FurType(MaxFur),FurCat(MaxFur)

      LOGICAL ACOOK,BCFOK

      CHARACTER OUTSTR*184,outs*194
      CHARACTER ACOName*72,LabFreq*4,CalName*12
      CHARACTER PubType*12, FurType*12, FurCat*4


C Initialiastion
      IER=0
      ACOOK = .TRUE.
      NbFurnit = 0
      NbPublic  = 0
      DO 10 I = 1,MaxPub
        PubType(I) = 'none'
 10   CONTINUE      
      DO 11 I = 1,MaxFur
        FurType(I) = 'none'
 11   CONTINUE      

C Specify the reading mode.
      IF (IEask.EQ.1) THEN
        CALL EASKAB(' When reading in the header of ACO file, do you',
     &  ' want it:','done silently','with reporting of contents',IW,0)
      ELSE
        IW = 1        
      ENDIF
C      IF (ACOOK) then 
C        WRITE (outs,'(a,a)')' Open ACO file: ',ACOName(:lnblnk(ACOName))
C        IF (IEask.NE.0) CALL EDISP(Iuout,outs)
C      ENDIF
      CALL EFOPSEQ(IACO,ACOName,1,IER)
      IF (IER.LT.0) then
        WRITE (outs,'(a,a)')' Error while opening ACO file : ',
     &                             ACOName(:lnblnk(ACOName))
        CALL EDISP(Iuout,Outs)
        IER=1
        RETURN
      ELSE
        ACOOK = .TRUE.
      ENDIF
      
      IF (ACOOK) then
C Start reading the ACO file
C        IF (IEask.NE.0) CALL EDISP(Iuout,' Start reading data.')
C Read BCF header : version, date, system scope, lifetime information.
  42    CALL STRIPC(IACO,OUTSTR,99,ND,1,'ACO',IER)
        IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
        IF (OUTSTR(1:12).eq.'*Public_type') THEN
          IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
            CALL ErrArgAco(0,OutStr,1,ND-1)
            GOTO 666         
          ENDIF
          K=13
          CALL EGETWI(OUTSTR,K,NbPublic,0,MaxPub,'W',
     &         'Nb of public type',IER)
          IF (IER .GE. 1) goto 666
C Loop trough public information
C Read number, type anf absorption coefficients
          DO 111 I=1,NbPublic
          IF ((I.EQ.1) .AND. (IW.EQ.1)) THEN
C            IF (IEask.NE.0) call edisp(iuout,' Read public block')
          ENDIF
 
            CALL STRIPC(IACO,OUTSTR,99,ND,1,
     &                   'public type information',IER)
            IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
            IF ((IER .GE. 1) .OR. (ND.NE.18)) THEN
              CALL ErrArgAco(0,OutStr,18,ND)
              GOTO 666         
            ENDIF
            K=0
            CALL EGETW(OUTSTR,K,PubType(I),'W','public type',IER)
            IF (IER .GE. 1) goto 666
            DO 121 J=1,MNbAbs
              CALL EGETWR(OutStr,K,PubAbsCo(I,J),0.0,2.0,'W',
     &                    'public absorption coefficients',IER)           
              IF (IER .GE. 1) goto 666 
121         CONTINUE
111       CONTINUE
        ELSEIF (OUTSTR(1:15).eq.'*Furniture_type') then
          IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
C            CALL ErrArg(0,OutStr,1,ND-1)
            GOTO 666         
          ENDIF
          K=16
          CALL EGETWI(OUTSTR,K,NbFurnit,0,MaxFur,'W',
     &         'Nb of public type',IER)


          IF (IER .GE. 1) goto 666
C Loop trough furniture information
C Read type, category and absorption coefficients
          DO 211 I=1,NbFurnit
          IF ((I.EQ.1) .AND. (IW.EQ.1)) THEN
            IF (IEask.NE.0) THEN 
C              call edisp(iuout,' Read furniture block')
            ENDIF
          ENDIF
 
            CALL STRIPC(IACO,OUTSTR,99,ND,1,
     &                   'furniture type information',IER)
            IF ((IER .GE. 1) .OR. (ND.NE.19)) THEN
              CALL ErrArgAco(0,OutStr,19,ND)
              GOTO 666         
            ENDIF
            IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
            K=0
            CALL EGETW(OUTSTR,K,FurType(I),'W','furnit type',IER)
            IF (IER .GE. 1) goto 666
            CALL EGETW(OUTSTR,K,FurCat(I),'W','furnit cat',IER)
            IF (IER .GE. 1) goto 666
            DO 221 J=1,MNbAbs
              CALL EGETWR(OutStr,K,FurAbsCo(I,J),0.0,2.0,'W',
     &                  'furniture absorption coefficients',IER)
              IF (IER .GE. 1) goto 666 
221         CONTINUE
211       CONTINUE
C When furniture has been read, exit the file. The scan of the zone
C public and furniture information is done in scnzoneaco.
          GOTO 777
        ELSE
          call edisp(iuout,'unrecognised tag...')
          call edisp(iuout,outstr)
          goto 666
        ENDIF
        GOTO 42
C     WRITE (outs,'(3a)')' ACO file: ',ACOName(:lnblnk(ACOName)),
C     &                          ' successfully read.'
C    CALL EDISP(Iuout,outs)
 777       CALL EDISP(iuout,' ')
      ENDIF

C Close cfg file before exiting.
 99   CALL ERPFREE(IACO,ISTAT)
      RETURN

 666  WRITE(outs,'(a,a)')' Unable to read the file : ',ACOName
      CALL edisp(iuout,outs)
      CALL EDISP(iuout,' Please check your ACO file.')
      BCFOK = .FALSE. 
      IER=1
      goto 99

      END



C ******************** SCNZONEACO ******************************
C Scans the acoustic file (.aco) and records all 
C information related to the selected zone
C
C Variables lists for SCNZONEACO: 
C ------------------------------
C IACO            - Unit channel of the BCFfile file
C ACOOK           - TRUE if ACOName exits
C PubAbsCo(MaxPub,MNbAbs) - Absorbtion coefficient for a public type [-]
C PubType(MaxPub)  - Name of the public 
C FurAbsCo(MaxPub,MNbAbs) - Absorbtion coefficient for a furniture type [-]
C FurType(MaxFur)  - Name of the furniture
C FurCat(MaxFur)   - Category of the furniture (unit / Surf)
C NbPub(MaxPub)    -  Nb of public for the current public type
C XNbFur(MaxFur)   -  Nb of unit or surface area for the current furniture
C IndexPub(MaxPub) - Public index in list 
C CovSurf(MaxFur)  - Surface covered by the funiture (None or Surface name)
C IndexFur(MaxFur) - Furniture index in list 
C MaxPub  - Nb max of public (acoustic.h)
C MaxFur -  Nb max of furniture (acoustic.h)
C
C Variables list
C---------------
C
C ACOName = file name of BCF
C IEask : 3= display furniture, 2= display occupant, 
C         1= ask for file display, 0= run in silent mode 
C IUC = Entrance file unit number  (IBCF)
C IER Error code
C IUIN = User input channel
C IUOUT = the message channel
C   1 = should exist, message & error -301 if not. 
C IW(Integer) : 1 = read ACO silently (=2 if not)

      SUBROUTINE SCNZONEACO(ZoneName,IEask,IER)
#include "building.h"
#include "acoustic.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/EVGENL/BCFOK
      COMMON/MetNam/ACOName,CalName(4),LabFreq(MNbAbs)
      COMMON/IFileACO/IACO
      COMMON/PubFurZone/NbPub(MaxPub),XNbFur(MaxFur),SNAeff(MS),
     &                  IndexPub(MaxPub),IndexFur(MaxFur)
      COMMON/CPubFurZone/CovSurf(MaxFur)

      LOGICAL ACOOK, BCFOK

      CHARACTER OUTSTR*184,outs*174,TmpStr*12
      CHARACTER LabFreq*4,CalName*12
      CHARACTER ACOName*72, ZoneLabel*12,ZoneName*12,CovSurf*12

C Initialiastion
      IER=0
      ACOOK = .TRUE.
      IDisp = 0
      DO 10 I = 1,MaxPub
        NbPub(I) = 0
        IndexPub(I) = 0
 10   CONTINUE      
      DO 11 I = 1,MaxFur
        XNbFur(I) = 0.
        IndexFur(I) = 0
 11   CONTINUE      

C Specify the reading mode.
      IF (IEask.EQ.1) THEN
        CALL EASKAB(' When reading in the people/furnit. block, do you',
     &  ' want it:','done silently','with reporting of contents',IW,0)
      ELSE
        IW = 1        
      ENDIF

      IF (ACOOK) then 
C        WRITE (outs,'(a,a)')' Open ACO file: ',ACOName(:lnblnk(ACOName))
C        IF (IEask.NE.0) CALL EDISP(Iuout,outs)
      ENDIF
      CALL EFOPSEQ(IACO,ACOName,1,IER)
      IF (IER.LT.0) then
        WRITE (outs,'(a,a)')' Error while opening ACO file : ',
     &                             ACOName(:lnblnk(ACOName))
         if (IEask.ne.0) CALL EDISP(Iuout,Outs)
        IER=1
        RETURN
      ELSE
        ACOOK = .TRUE.
      ENDIF
      
      IF (ACOOK) then
C Start reading the ACO file
C        IF (IEask.NE.0) CALL EDISP(Iuout,
C     &   ' Reading public and furniture information for selected zone')

C Read ACO header : version, date, system scope, lifetime information.
  42    CALL STRIPC(IACO,OUTSTR,99,ND,1,'ACO',IER)
        IF (OUTSTR(1:5).eq.'*Zone') THEN
          IDisp = 1
          IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
          IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
            CALL ErrArgAco(0,OutStr,1,ND-1)
            GOTO 666         
          ENDIF
          K=6
          CALL EGETW(OUTSTR,K,ZoneLabel,'W','zone name',IER)
          IF (IER .GE. 1) goto 666
C Check if readed zone correspond to searched zone
          IF (ZoneName .EQ. ZoneLabel) THEN
            IF (IEask.GE.2) call edisp(iuout,OUTSTR)
686         CALL STRIPC(IACO,OUTSTR,99,ND,1,
     &                   'Public in zone ',IER)
            IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
            IF (OUTSTR(1:15).eq.'*Nb_public_type') then
              IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
                CALL ErrArgAco(0,OutStr,2,ND)
                GOTO 666         
              ENDIF
              K=16
              CALL EGETWI(OUTSTR,K,NbPublic,0,10000,'W',
     &         'Nb of public type for this zone',IER)
              IF (IER .GE. 1) goto 666

              DO 111 I=1,NbPublic
C                IF ((I.EQ.1) .AND. (IW.EQ.1)) THEN
C                  IF (IEask.EQ.1) call edisp(iuout,' Read public block')
C                ENDIF
 
                CALL STRIPC(IACO,OUTSTR,99,ND,1,
     &                   'public type information',IER)
                IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
                IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
                  CALL ErrArgAco(0,OutStr,2,ND)
                  GOTO 666         
                ENDIF
                K=0
                IF (IEask.EQ.2) call edisp(iuout,OUTSTR)
                CALL EGETW(OUTSTR,K,TmpStr,'W',
     &                    'public name',IER)
                IndexPub(I)= IPubFur(1,TmpStr)

                IF (IER .GE. 1) goto 666
                CALL EGETWI(OUTSTR,K,NbPub(I),0,1000,'W',
     &                      'Nb of public',IER)
                IF (IER .GE. 1) goto 666
111           CONTINUE
              GOTO 686

            ELSEIF (OUTSTR(1:18).eq.'*Nb_furniture_type') then
              IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
                CALL ErrArgAco(0,OutStr,2,ND)
                GOTO 666         
              ENDIF
              K=19
              CALL EGETWI(OUTSTR,K,NbFurnit,0,10000,'W',
     &         'Nb of furniture type for this zone',IER)
              IF (IER .GE. 1) goto 666

              DO 121 I=1,NbFurnit
C                IF ((I.EQ.1) .AND. (IW.EQ.1)) THEN
C                  IF (IEask.EQ.1) THEN 
C                    call edisp(iuout,' Read furniture block')
C                  ENDIF
C                ENDIF
 
                CALL STRIPC(IACO,OUTSTR,99,ND,1,
     &                   'furniture type information',IER)
                IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
                IF (IEask.EQ.3) call edisp(iuout,OUTSTR)
                IF ((IER .GE. 1) .OR. (ND.NE.3)) THEN
                  CALL ErrArgAco(0,OutStr,3,ND)
                  GOTO 666         
                ENDIF
                K=0
                CALL EGETW(OUTSTR,K,TmpStr,'W',
     &                    'public name',IER)
                IndexFur(I)= IPubFur(2,TmpStr)
                IF (IER .GE. 1) goto 666
                CALL EGETWR(OUTSTR,K,XNbFur(I),0.,1000.,'W',
     &                      'Nb of public',IER)
                IF (IER .GE. 1) goto 666
                CALL EGETW(OUTSTR,K,CovSurf(I),'W',
     &                    'covering surface',IER)
                IF (IER .GE. 1) goto 686
121           CONTINUE
              GOTO 42
            ENDIF
          ELSE
            GOTO 42
          ENDIF
        ELSEIF (OUTSTR(1:8).eq.'*End_ACO') THEN
          IF (IEask.NE.0) THEN 
            WRITE (outs,'(3a)')' ACO file: ',ACOName(:lnblnk(ACOName)),
     &                          ' successfully read.'
            CALL EDISP(Iuout,outs)
            CALL EDISP(iuout,' ')
            if(BCFOK) then
              call EDISP(iuout,'Please select zone(s) to include.')
              CALL EDISP(iuout,' ')
            endif
          ENDIF
          GOTO 99
        ELSE
          IF ((IW.EQ.2).AND.(IDisp.EQ.1)) call edisp(iuout,OUTSTR) 
          GOTO 42
        ENDIF
        GOTO 42
      ENDIF

C Close ACO file before exiting.
 99   CALL ERPFREE(IACO,ISTAT)
      RETURN

 666  WRITE(outs,'(a,a)')' Unable to read the file : ',ACOName
      CALL edisp(iuout,outs)
      CALL EDISP(iuout,' Please check your ACO file.')
      BCFOK = .FALSE. 
      IER=1
      goto 99

      END


C************ IPubFur*************
C Using a public or furniture name (XPubFur), returns the corresponding
C list index (IPubFur) extracted in the Aco file.
C Scan the list of public (PubType) or furniture(FurType) and compare to 
C XPubFur.
C
C Itype (Integer)         : Search for public (1) or furniture (2)
C XPubFur (Char)          : Public or Furniture name to find

      FUNCTION IPubFur(Itype,XPubFur)

#include "acoustic.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/AbsCHAR/PubType(MaxPub),FurType(MaxFur),FurCat(MaxFur)


      CHARACTER*12 XPubFur
      CHARACTER*72 outs
      CHARACTER PubType*12, FurType*12, FurCat*4 

      IPubFur = 0
      IF (Itype.EQ.1) THEN
        DO 10 It=1,MaxPub
          IF (PubType(It) .EQ. XPubFur) THEN
            IPubFur = It 
            GOTO 363
          ENDIF
 10     CONTINUE
      ELSE
        DO 11 It=1,MaxFur
          IF (FurType(It) .EQ. XPubFur) THEN
            IPubFur = It
            GOTO 363
          ENDIF
 11     CONTINUE
      ENDIF

363   IF (IPubFur .EQ. 0) THEN

      IF (Itype.EQ.1) THEN
        WRITE(outs,'(a,a)')XPubFur,' not in public list.'
      ELSE
        WRITE(outs,'(a,a)')XPubFur,' not in furniture list.'
      ENDIF
        CALL edisp(iuout,outs)
        GOTO 666
      ENDIF
666   RETURN
      END



C *************************ErrArgAco**********************************
C Return error message to Textual feedback, when nb of arguments 
C not valid in a line in a  block of the Aco file 
C and return its location in the file.
C IType (Integer) : Error type as follow
C In the Public list :  Itype = 0
C In the Furniture list :  Itype = 1 
C In the zone list : Itype = 2 
C
C Label (String)  :  
      SUBROUTINE ErrArgAco(Itype,Label,MShould,MIs)

      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER Label*184,outs*174

C Check consistancy
      IF (Itype .EQ. 0) THEN
        WRITE(outs,'(a,a)')' Error in public list: ',Label
      ELSEIF (Itype .EQ. 1) THEN
        WRITE(outs,'(a,I3,a,a)')' Error in furniture list: ',Label
      ELSEIF (Itype .EQ. 2) THEN
        WRITE(outs,'(a,I3,a,a)')' Error in zone list: ',Label
      ENDIF
      Call edisp(iuout, outs)
      WRITE(outs,'(a)')' Nb of arguments is incorrect'
      Call edisp(iuout, outs)
      WRITE(outs,'(a,I3,a,I3,a)')' Should be ',MShould, '   but are  ',
     &      MIs,'   argument(s)'
      Call edisp(iuout, outs)
      RETURN
      END




