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

C This file contains the following subroutines for creating the 
C non-linear thermophysical properties menues:
C        thmenu
C        thrmeq
C        therms
C        thermr
C        thermc
C        indxth
C        thchks
C        thchkf

C **************************  THMENU  *************************
C This subroutine is called by INITPF ( bld.F ) ,or(bps.F) .
C It displays the main nonlinear thermal properties menu .and 
C according to the users selection it calls other subroutines ,
C performs the required process internally, or returns back to 
C INITPF ( bld.F ) .   The common variables are :
C IBTHAL(MTHF,3) : defines the actuator location ( the layers
C                  which will be governed by the associated
C                  thermal equations .
C NTHEQ(MTHF)    : holds the number of thermal equations in each
C                  thermal function.
C BTHDT(MTHF,MTHEQ,MDATA) : holds informations about :
C                * the thermal property type
C                * temperature range for thermal equation validity
C                * the thermal equation's coefficients
C                  values , where the thermal equation estimates the
C                  the value of the thermal property as a function 
C                  of temperature .
C 
C The value of thermal property ( 1:conductivity,2:density,
C and 3:heat capacity ) is governed by :
C
C
C                       b       c       d       e
C          P = A + B * T + C * T + D * T + E * T  -----------( 1 )
C
C   where;
C         P = actual Property value
C         T = layer's mean temperature
C         A = BTHDT(ITHF,ITHEQ,4)
C         B = BTHDT(ITHF,ITHEQ,5)
C         b = BTHDT(ITHF,ITHEQ,6)
C         C = BTHDT(ITHF,ITHEQ,7)
C         |   |      |    |    |
C         :   :      :    :    :
C         e = BTHDT(ITHF,ITHEQ,12)
C
C  and ;
C        BTHDT(ITHF,ITHEQ,1) : property type
C        BTHDT(ITHF,ITHEQ,2) : minimum temperature
C        BTHDT(ITHF,ITHEQ,3) : maximum temperature
C
C **************************************************************
      SUBROUTINE THMENU(ITRC)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G2/precz.
#include "geometry.h"

      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/HELP/IHELPM
      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP) 
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/VTHP11/NTHF,IBTHAL(MTHF,3)
      COMMON/VTHP12/NTHEQ(MTHF),BTHDT(MTHF,MTHEQ,MDATA)
      COMMON/VTHP18/LNLTHP
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)

      DIMENSION IVAL2(4),IVAL3(MCOM),IVAL4(MS),IVAL5(ME)
      LOGICAL INTHPS,INTHPZ
      LOGICAL OK,DOK,SAVEOK,XST

      CHARACTER ITEM(16)*35,ITEM2(4)*25,LNLTHP*72,H*72
      CHARACTER*20 ITEM3(MCOM),ITEM4(MS),ITEM5(ME)
      CHARACTER*95 TEXT,TEXT1
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      character ltmp*72

C Read the complete building thermal and geometical data.
      IUNIT=IFIL+1
      SAVEOK=.FALSE.
      DO 5 ICOMP=1,NCOMP
          call georead(IUNIT,LGEOM(ICOMP),ICOMP,1,IUOUT,IER)
        IF(IER.NE.0)RETURN
        CALL ECONST(LTHRM(ICOMP),IUNIT,ICOMP,0,IUOUT,IER)
        IF(IER.NE.0)RETURN
        NCONST(ICOMP)=NSUR
      DO 5 I=1,NSUR
        NELTS(ICOMP,I)=NE(I)
        NGAPS(ICOMP,I)=NAIRG(I)
        NG=NAIRG(I)
        IF(NG.GT.0)THEN
          DO 6 J=1,NG
            NPGAP(ICOMP,I,J)=IPAIRG(I,J)
    6     CONTINUE
        ENDIF
    5 CONTINUE
      IF(INTHPS)CALL THERMR(LNLTHP,IUNIT,ITRC,IER)
      ITHF=1
   10 CALL USRMSG(' ',' Select from the menu!','-')
      ino=-3
      WRITE(ITEM(1),'(A,I2)')' Total Thermal Functions :  ',NTHF
      ITEM(2)= '  _________________________________'
      IF(NTHF.GT.0)THEN
        WRITE(ITEM(3),'(A,I2)')'a displayed function : ',ITHF
        WRITE(ITEM(4),'(A,I2,2(1X,I2))')'b actuator location  : ',
     &        IBTHAL(ITHF,1),IBTHAL(ITHF,2),IBTHAL(ITHF,3)
        WRITE(ITEM(5),'(A,I2)')'c number of equations: ',NTHEQ(ITHF)
      ELSE
        WRITE(ITEM(3),'(A)')'a displayed function :'
        WRITE(ITEM(4),'(A)')'b actuator location  :'
        WRITE(ITEM(5),'(A)')'c number of equations:'
      ENDIF
      ITEM(6)= '  _________________________________'
      ITEM(7)= 'd define new function'
      ITEM(8)= 'e delete function    '
      ITEM(9)= '  _________________________________'
      ITEM(10)='f read thermal configuration file  '
      ITEM(11)='g write thermal configuration file '
      ITEM(12)='h delete thermal configuration file'
      ITEM(13)='  _________________________________'
      ITEM(14)='! list                      '
      ITEM(15)='? help                      '
      ITEM(16)='- exit this menu            '
      NITEM=16

C Help text for this menu.
      H(1) ='ESP_r allows defining the thermal properties of'  
      H(2) ='materials to be linearly or non-linearly temperature'
      H(3) ='dependent .'
      H(4) ='For defining linear dependency of thermal conductivity:'
      H(5) ='option (k) of simulator menu is recommended du to its'
      H(6) ='flixibility .    '
      H(7) ='       '
      H(8) ='Via this menu you can descripe the variation (or'
      H(9) ='change) of the thermal properties (1: conductivity, ' 
      H(10)='2:density, 3:heat capacity) of any layer (except air'
      H(11)='gaps) as a function of temperature.  You can also '
      H(12)='describe the variation of any thermal property of a '
      H(13)='layer by more than one thermal functions (i.e. each'
      H(14)='thermal function will be valid only for the temperature'
      H(15)='range associated with it). Show me more has details. '
      H(16)='  '
      H(17)=' The available options are :'
      H(18)='  (a)  show the next available thermal function'
      H(19)='  (b)  define the actuator location for displayed '
      H(20)='       thermal function (via  other menues )'
      H(21)='  (c)  define the thermal equation for  displayed '
      H(22)='       thermal function  (via  other menues )'
      H(23)='  (d)  creat a new thermal function which will be'
      H(24)='       displayed in  NON-LINEAR PROPERTIES  menu'
      H(25)='  (e)  delete the displayed thermal function and all'
      H(26)='       data associated with it ' 
      H(27)='  (!)  list the complete  description of  thermal'
      H(28)='       configuration defined so far '
      H(29)='  (?)  display this window'
      H(30)='  (-)  exit from then menu.'

      CALL EMENU('Non-Linear Properties',ITEM,NITEM,INO)

C Prevent actuation of commands associated with the availability of
C at least on thermal function; when there is no function.
      IF(NTHF.EQ.0.AND.(INO.EQ.3.OR.INO.EQ.4.OR.INO.EQ.5.OR.INO.EQ.8
     &   .OR.INO.EQ.11.OR.INO.EQ.14))THEN
         CALL USRMSG(' ',' No thermal functions defined yet !','W')

C Create new thermal function and show it in the menu, and save
C the total number of thermal functions defined so far.
      ELSEIF(INO.EQ.3)THEN
        ITHF=ITHF+1
        IF(ITHF.GT.NTHF)ITHF=1

C define the actuator location 
      ELSEIF(INO.EQ.4)THEN
  200   INPICK=1
        ITEM2(1)=' complete building      '
        ITEM2(2)=' selected zone          '
        ITEM2(3)=' selected construction  '
        ITEM2(4)=' selected layer         '
        H(1)=' This information is required to define the'
        H(2)=' range of layers for which the thermal equations,'
        H(3)=' of the same thermal function, are applied.'
        H(4)='  '
        CALL EPICKS(INPICK,IVAL2,' ',' ',24,4,ITEM2,
     &             'Actuator Location',IER,4)
        I1=IVAL2(1)
        IF(I1.EQ.0)GOTO 10

C actuator location : complete building 
        IF(I1.EQ.1)THEN
           IBTHAL(ITHF,1)=0
           IBTHAL(ITHF,2)=0
           IBTHAL(ITHF,3)=0

C actuator location : other than complete building 
        ELSE
  300     INPICK=1
          DO 20 I=1,NCOMP
            WRITE(ITEM3(I),'(A)')zname(I)
   20     CONTINUE
          CALL EPICKS(INPICK,IVAL3,' ',' ', 20,NCOMP,ITEM3,
     &                '   Zones',IER,0)
          I2=IVAL3(1)
          IBTHAL(ITHF,1)=I2
          IF(I2.EQ.0)GOTO 200

C actuator location : selected zone
          IF(I1.EQ.2)THEN
            IBTHAL(ITHF,2)=0
            IBTHAL(ITHF,3)=0
          ELSE
  400       INPICK=1
            NC=NCONST(I2)
            DO 30 I=1,NC
              WRITE(ITEM4(I),'(A)')SNAME(I2,I)
   30       CONTINUE
            CALL EPICKS(INPICK,IVAL4,' ',' ',20,NC,ITEM4,
     &                  '   Surfaces',IER,0)
            I3=IVAL4(1)
            IBTHAL(ITHF,2)=I3
            IF(I3.EQ.0)GOTO 300

C actuator location : selected construction
            IF(I1.EQ.3)THEN
              IBTHAL(ITHF,3)=0
            ELSE

C actuator location : selected layer
            INPICK=1
            NEL=NELTS(I2,I3)
            NGP=NGAPS(I2,I3)
            M=1
            DO 40 I=1,NEL
              IF(NGP.EQ.0)GOTO 50
              IF(M.GT.NGP)GOTO 50
              IG=NPGAP(I2,I3,M)
              IF(IG.NE.I)GOTO 50
                M=M+1
                WRITE(ITEM5(I),'(A)')' ** AIR GAP ** '    
                GOTO 40
   50           WRITE(ITEM5(I),'(A,1X,I2,1X,A)')'LAYER (',I,')'
   40         CONTINUE
              CALL EPICKS(INPICK,IVAL5,' ',' ',20,NEL,ITEM5,
     &                        '  Layers',IER,0)
              I4=IVAL5(1)
              IBTHAL(ITHF,3)=I4
              IF(I4.EQ.0)GOTO 400
C Check if the selected layer is an air gap 
              DO 55 IGL=1,NGP
                IF(NPGAP(I2,I3,IGL).EQ.IBTHAL(ITHF,3))THEN
                  CALL USRMSG(
     &          ' The selected actuator location is an air gap .',
     &          ' IF not changed; This function will be NEGLECTED','W') 
                ENDIF
   55         CONTINUE
            ENDIF
          ENDIF
        ENDIF

C Modify the thermal equations for the displayed function
      ELSEIF(INO.EQ.5)THEN
        CALL THRMEQ(ITHF)

C prevent creating thermal functions more than the maximum allowed (12)
      ELSEIF(INO.EQ.7)THEN
        IF(NTHF.LT.MTHF)THEN
          NTHF=NTHF+1
          ITHF=NTHF
        ELSE
          CALL USRMSG(' ','Maximum allowed functions reached','W')
        ENDIF

C Delete the displayed thermal function and update the associated data
      ELSEIF(INO.EQ.8)THEN
        IF(ITHF.LT.NTHF)THEN
          DO 60 I=ITHF,NTHF-1
            IBTHAL(I,1)=IBTHAL(I+1,1)
            IBTHAL(I,2)=IBTHAL(I+1,2)
            IBTHAL(I,3)=IBTHAL(I+1,3)
            NTHEQ(I)=NTHEQ(I+1)
          DO 60 J=1,NTHEQ(I)
          DO 60 K=1,MDATA
             BTHDT(I,J,K)=BTHDT(I+1,J,K)
   60     CONTINUE
        ENDIF
        IBTHAL(NTHF,1)=0
        IBTHAL(NTHF,2)=0
        IBTHAL(NTHF,3)=0
        DO 70 J=1,NTHEQ(NTHF)
        DO 70 K=1,MDATA
          BTHDT(NTHF,J,K)=0
   70   CONTINUE
        NTHEQ(NTHF)=0
        NTHF=NTHF-1
        ITHF=ITHF-1
        IF(ITHF.LT.1)ITHF=1

C load a thermal configuration from an existing thermal configuration 
C file.
      ELSEIF(INO.EQ.10)THEN
        H(1)='Enter the name of the thermal configuration file'
        ltmp=LNLTHP
        CALL EASKS(ltmp,' Thermal configuration file name ?',' ',
     &               72,' ',' thermal confg. file ',IER,1)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN') LNLTHP=ltmp
        CALL THERMR(LNLTHP,IUNIT,ITRC,IER)

C Save the defined thermal configuration into a file.
      ELSEIF(INO.EQ.11)THEN
        H(1)='Enter the name of the thermal configuration file'
        ltmp=LNLTHP
        CALL EASKS(ltmp,' Thermal configuration file name ?',' ',
     &               72,' ',' thermal confg. file ',IER,1)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN') LNLTHP=ltmp
        CALL THERMS(LNLTHP,IUNIT,ITRC,IER)
        IF(IER.EQ.0)THEN
          INTHPS=.TRUE.
          SAVEOK=.TRUE.
        ENDIF

C Delete the thermal configuration file.
      ELSEIF(INO.EQ.12)THEN
        IF(INTHPS)THEN
          CALL FINDFIL(LNLTHP,XST)
          IF(XST)THEN
            CALL EFOPSEQ(IUNIT,LNLTHP,3,IER)
            CALL EFDELET(IUNIT,IOS)
          ENDIF
          LNLTHP=' '
          INTHPS=.FALSE.
          SAVEOK=.TRUE.
        ELSE
          CALL USRMSG(' ',
     &              ' thermal configuration file does not exist.','W')
        ENDIF

C List all defined thermal functions with their associated data
      ELSEIF(INO.EQ.(NITEM-2))THEN
        WRITE(TEXT1,'(A,A)')'-------------------------------------',
     &   '--------------------------------------------------------'
        DO 110 I=1,NTHF
          WRITE(TEXT,'(A,A)')'       Function No.          ',
     &              'Actuator location          No. of Eqns'
          CALL EDISP(IUOUT,TEXT)
          CALL EDISP(IUOUT,TEXT1)
          WRITE(TEXT,'(10X,I2,20X,I2,1X,I2,1X,I2,20X,I1)')I,
     &              IBTHAL(I,1),IBTHAL(I,2),IBTHAL(I,3),NTHEQ(I)
          CALL EDISP(IUOUT,TEXT)
          CALL EDISP(IUOUT,' ')
          WRITE(TEXT,'(A,A)')'No T  MIN(Temp.)MAX      A       B',
     &      '        b       C       c       D       d       E       e'
          CALL EDISP(IUOUT,TEXT)
          CALL EDISP(IUOUT,TEXT1)
          DO 120 J=1,NTHEQ(I)
            WRITE(TEXT,150)J,INT(BTHDT(I,J,1)),(BTHDT(I,J,K),K=2,12)
  150       FORMAT(I1,2X,I1,2(1X,F7.2),1X,F8.3,4(1X,F8.5,1X,F6.2))
            CALL EDISP(IUOUT,TEXT)
  120     CONTINUE
          CALL EPAGEW
  110   CONTINUE

C Display the help message
      ELSEIF(INO.EQ.(NITEM-1))THEN
        IHELPM=1
        CALL PHELPD('non-linear',30,'non-linear  ',0,0,IER)

C Return to simulator. Before that check thermal configuration legality.
      ELSEIF(INO.EQ.NITEM)THEN
        ICHCK=0
        CALL THERMC(ICHCK)
        IF(ICHCK.NE.0)THEN
          dok=.false.
          h(1)='There was a problem and if you exit now it will not'
          h(2)='have been resolved. '
          CALL ASKOK(' WARNING messages were detected',
     &               ' Are you sure you want to exit ? ',OK,dok,2)
          IF(.NOT.OK)GOTO 10
        ENDIF
        IF(SAVEOK)THEN
          dok=.true.
          h(1)='There are unsaved changes to the model. These will be'
          h(2)='lost if you exit without saving. '
          CALL ASKOK(' ',
     &    'configuration should be saved. Do this ?',OK,dok,2)
          IF(OK)CALL EMKCFG('-',IER)
        ENDIF
        CALL USRMSG(' ',' ','-')
        RETURN
      ENDIF

C After each command selection and execution ( except the EXIT )
C redisplay the non-linear properties menu .
      GOTO 10
      END

C *************************** THRMEQ ***************************
C THRMEQ displayes the thermal equations menu . This subroutine 
C is called by THMENU .
C ****************************************************************
      SUBROUTINE THRMEQ(ITHF)
#include "building.h"
      integer iCountWords

      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/HELP/IHELPM
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/VTHP12/NTHEQ(MTHF),BTHDT(MTHF,MTHEQ,MDATA)
      CHARACTER ITEM(24)*42,TEXT*40,OUTSTR*124,TEXT1*95,TEXT2*95
      CHARACTER*72 TITLE,THFIL,EQNTXT,EQNMSG,H

      INDEX1=0
      IUNIT=IFIL+1
   10 CALL USRMSG(' ',' ','-')
      ino=-3
      M=NTHEQ(ITHF)
      WRITE(TEXT,'(A,I2)')'Thermal Equations for Function No.',ITHF
      ITEM(1)='  | Eqn | Property |  Minimum |  Maximum |'
      ITEM(2)='  | No. |   type   |     Temperatures    |'
      ITEM(3)='  ----------------------------------------'

C Display the defined thermal equations for this thermal function.
      DO 20 I=1,M
         WRITE(ITEM(I+3),'(4X,I1,7X,I1,7X,F7.2,5X,F7.2)')I,
     &         INT(BTHDT(ITHF,I,1)),(BTHDT(ITHF,I,K),K=2,3)
   20 CONTINUE   
      ITEM(M+4)='  ______________________________________'
      ITEM(M+5)='a add a new equation          '
      ITEM(M+6)='b delete equation             '
      ITEM(M+7)='c list equations              '
      ITEM(M+8)='  ______________________________________'
      ITEM(M+9)='d input thermal file               '
      ITEM(M+10)='e list thermal file               '
      ITEM(M+11)='f add equation from thermal file  '
      ITEM(M+12)='g write equation to thermal file  '
      ITEM(M+13)='  ______________________________________'
      ITEM(M+14)='? help                                 '
      ITEM(M+15)='- exit this menu                       '

C Help text for this menu.
      H(1)='This menu is used to creat and/ or modify the thermal'
      H(2)='equations of the thermal functions.  Modifying a'
      H(3)='thermal equation is acheived by selecting it.'

      CALL EMENU(TEXT,ITEM,M+15,INO)

C prevent creating thermal equations, for a thermal function, more 
C than the maximum allowed number.
      IF(M.EQ.MTHEQ.AND.(INO.EQ.M+5.OR.INO.EQ.M+11))THEN
        CALL USRMSG(' ',
     &    ' Maximum number of equations per function reached!','W')

C prevent actuating the commands which are not applicable when there
C is no thermal equations for this thermal function
      ELSEIF(M.EQ.0.AND.(INO.EQ.M+6.OR.INO.EQ.M+7.OR.INO.EQ.M+12))THEN
        CALL USRMSG(' ',' No thermal equations available!','W')

C Add a new equation manually; or modify existing one.
      ELSEIF(INO.EQ.M+5.OR.(INO.GT.3.AND.INO.LT.(M+4)))THEN

C Add a new equation manually 
        IF(INO.EQ.M+5)THEN
          NTHEQ(ITHF)=NTHEQ(ITHF)+1
          N=NTHEQ(ITHF)
        ELSE
          N=INO-3
        ENDIF
        H(1)='The available thermo-physical property types are:'
        H(2)=' '
        H(3)='1 thermal conductivity'
        H(4)='2 density'
        H(5)='3 thermal capacity.'
        H(6)=' '
        H(7)='The thermal equation is defined by:'
        H(8)='               b       c       d       e'
        H(9)='   P = A + B * T + C * T + D * T + E * T'
        H(10)='   '
        H(11)='where each thermal equation defines the dependence of'
        H(12)='the property for the temperature range provided.'
        WRITE(EQNTXT,101)INT(BTHDT(ITHF,N,1)),(BTHDT(ITHF,N,K),K=2,3)
  101   FORMAT(I2,2(1X,F7.2))
        WRITE(EQNMSG,*)' Enter the property type and temperature range.'
        CALL EASKS(EQNTXT,EQNMSG,' ',72,' ','thermal data',IER,12)
        NCFF = iCountWords(EQNTXT)
        IF(NCFF.NE.3)THEN
          CALL USRMSG(' ',
     &    ' Please enter an integer and two real numbers!','W')
          GOTO 10
        ENDIF
        K=0
        CALL EGETWI(EQNTXT,K,NDUMY,1,3,'F',' ',IER)
        BTHDT(ITHF,N,1)=INT(NDUMY)
        CALL EGETWR(EQNTXT,K,BTHDT(ITHF,N,2),0.,0.,'-',' ',IER)
        CALL EGETWR(EQNTXT,K,BTHDT(ITHF,N,3),0.,0.,'-',' ',IER)
        WRITE(EQNTXT,102)(BTHDT(ITHF,N,K),K=4,12)
  102   FORMAT(F8.3,4(1X,F8.5,1X,F6.2))
        WRITE(EQNMSG,*)' Coefficient value?'
        CALL EASKS(EQNTXT,EQNMSG,' ',72,' ',' data ',IER,13)
        NCFF = iCountWords(EQNTXT)
        IF(NCFF.NE.9)THEN
          CALL USRMSG(' ',' Please enter 9 values!','W')
          DO 110 I12=1,12
            BTHDT(ITHF,N,I12)=0.0
  110     CONTINUE
          GOTO 10
        ENDIF
        K=0
        DO 120 I9=4,12
          CALL EGETWR(EQNTXT,K,BTHDT(ITHF,N,I9),0.,0.,'-',' ',IER)
  120   CONTINUE

C Delete existing equation. Menu allows selection of 
C the thermal equation to be deleted.
      ELSEIF(INO.EQ.M+6)THEN
        CALL USRMSG(' ',' Select the thermal equation to delete.','-') 
        ITEM(M+5)='* cancel last delete command'
   25   CALL EMENU('Delete',ITEM,M+5,INO1)

C Update the data after deleting a thermal equation.
        IF(INO1.GT.3.AND.INO1.LT.(M+4))THEN
          IF(INO1.LT.(M+3))THEN
            DO 30 I2=(INO1-3),(M-1)
              DO 40 I3=1,MDATA
                BTHDT(ITHF,I2,I3)=BTHDT(ITHF,I2+1,I3)
   40         CONTINUE
   30       CONTINUE
          ENDIF
          DO 50 K=1,MDATA
            BTHDT(ITHF,M,K)=0.
   50     CONTINUE
          NTHEQ(ITHF)=NTHEQ(ITHF)-1

C Display the deletion menu as long as the user did not selected
C a valid option.
        ELSEIF(INO1.NE.(M+5))THEN
          GOTO 25
        ENDIF

C List a detailed list of the existing thermal equations for 
C this function.
      ELSEIF(INO.EQ.M+7)THEN
        WRITE(TEXT1,'(A,A)')'-------------------------------------',
     &   '---------------------------------------------------------'
        CALL EDISP(IUOUT,' ')
        WRITE(TEXT2,'(A,A)')'No T  MIN(Temp.)MAX      A       B',
     &    '        b       C       c       D       d       E       e'
        CALL EDISP(IUOUT,TEXT2)
        CALL EDISP(IUOUT,TEXT1)
        DO 51 IE=1,NTHEQ(ITHF)
        WRITE(TEXT2,'(I1,2X,I1,2(1X,F7.2),1X,F8.5,4(1X,F8.5,1X,F6.2))')
     &             IE,INT(BTHDT(ITHF,IE,1)),(BTHDT(ITHF,IE,K),K=2,12)
        CALL EDISP(IUOUT,TEXT2)
   51   CONTINUE

C Loading a thermal data file ,which contains a list of thermal
C equations, in order to modify it, copy from it and/ or list the
C contents.
      ELSEIF(INO.EQ.M+9)THEN
        IF(INDEX1.EQ.1)CALL ERPFREE(IUNIT,IOS)
        H(1)='  Enter the name of the thermal file to be loaded.'
        CALL EASKS(THFIL,' Thermal data file name ?',' ',72,' ',
     &       ' thermal file ',IER,1)
       INDEX1=1
       CALL EFOPSEQ(IUNIT,THFIL,3,IER)
       IF(IER.LT.0)INDEX1=0
       MLIN=0 

C While reading (loading) the thermal data file, display its contents 
C in the text feedback window.
   55  READ(IUNIT,'(A95)',IOSTAT=IOS,END=56)OUTSTR
       MLIN=MLIN+1
       CALL  EDISP(IUOUT,OUTSTR)
       GOTO 55
   56  CONTINUE

C The user requested a list of the content of thermal data file.
C This command will not be executed unless a thermal data file is 
C already loaded.
      ELSEIF(INO.EQ.M+10)THEN
        IF(INDEX1.EQ.1)THEN
          REWIND(IUNIT,ERR=999)
   57     READ(IUNIT,'(A95)',IOSTAT=IOS,END=58)OUTSTR
          CALL  EDISP(IUOUT,OUTSTR)
          GOTO 57
   58     if(IOS.eq.2)then
            CALL USRMSG(' ',
     &        'Permissions reading thermal equations data file!','W')
          else
            continue
          endif
        ELSE
          CALL USRMSG(' ',' No thermal file yet loaded!','W')
        ENDIF

C Defining a new thermal equation by copy an existing equation from the
C thermal data file.
      ELSEIF(INO.EQ.M+11)THEN
        IF(INDEX1.EQ.1)THEN
          H(1)='A listing of the thermal data file can be obtained'
          H(2)='by selecting the appropriate menu option.'
          H(3)='If you cancel then the menu will be re-displayed. '
          MNUM=MLIN/2
          CALL EASKI(INUM,
     &         ' Code for the thermal equation to be added',
     &         ' from the thermal data file?',
     &               1,'F',MNUM,'F',1,'equation number',IERI,2)
          if(ieri.eq.-3)then
            goto 10
          endif
          ILIN=INUM*2
          REWIND(IUNIT,ERR=999)
          DO 60 I4=1,ILIN-1
            READ(IUNIT,'(A95)',IOSTAT=IOS,END=991)OUTSTR
   60    CONTINUE
         CALL STRIPC(IUNIT,OUTSTR,13,ND,1,'coefficients',IER)
         IF(IER.NE.0)GOTO 10
         K=0
         CALL EGETWI(OUTSTR,K,NUM,1,MNUM,'F','code no',IER)
         CALL EGETWI(OUTSTR,K,MDUM,1,3,'F','thermal type',IER)
         BTHDT(ITHF,M+1,1)=FLOAT(MDUM)
         DO 70 I5=2,MDATA
           CALL EGETWR(OUTSTR,K,BTHDT(ITHF,M+1,I5),1.,500.,
     &                      '-','thermal coefficient',IER)
   70    CONTINUE
         NTHEQ(ITHF)=NTHEQ(ITHF)+1
       ELSE
         CALL USRMSG(' ',' No thermal file yet loaded!','W')
       ENDIF

C The user wants to save an existing thermal equation ( defined 
C manually by the user or copyed from another thermal data file )in  
C the loaded thermal data file.
      ELSEIF(INO.EQ.M+12)THEN
        IF(INDEX1.EQ.1)THEN
          H(1)='The title is required to distinguish different'
          H(2)='thermal equations as held in the thermal data file.'
          H(3)='It is recommended that this title includes the'
          H(4)='material and thermo-physical property names.'
          CALL EASKS(TITLE,' Title for this thermal equation?',' ',
     &       72,' ','equation title',IER,4)
          CALL USRMSG(' ',' Select the thermal equation to save:','-') 
          ITEM(M+5)='* cancel last save command'  
   80     CALL EMENU('Save',ITEM,M+5,INO2)
          IF(INO2.GT.3.AND.INO2.LT.(M+4))THEN
            REWIND(IUNIT,ERR=999)
            MLIN=0
   75       READ(IUNIT,'(A95)',IOSTAT=IOS,END=77)OUTSTR
            MLIN=MLIN+1
            GOTO 75
   77       MNUM=MLIN/2
            WRITE(IUNIT,'(A2,A72)',IOSTAT=IOS,ERR=1000)'# ',TITLE
            WRITE(IUNIT,78,IOSTAT=IOS,ERR=1000)MNUM+1,
     &      INT(BTHDT(ITHF,INO2-3,1)),(BTHDT(ITHF,INO2-3,K),K=2,12)
   78       FORMAT(1X,I2,1X,I1,2(1X,F7.2),1X,F8.3,4(1X,F8.5,1X,F6.2))
            MLIN=MLIN+2
          ELSEIF(INO2.NE.(M+5))THEN
            GOTO 80
         ENDIF
       ELSE
         CALL USRMSG(' ',' No thermal file yet loaded!','W')
       ENDIF

C Display the help message.
      ELSEIF(INO.EQ.M+14)THEN
        IHELPM=1
        CALL PHELPD('equation mods',3,'-',0,0,IER)

C Return to the non-linear properties menu and close the thermal data
C file if it was loaded.
      ELSEIF(INO.EQ.M+15)THEN
        IF(INDEX1.EQ.1)CALL ERPFREE(IUNIT,IOS)
        CALL USRMSG(' ',' ','-')
        RETURN
      ENDIF

C After each selection redisplay the
C thermal equations menu.
      GOTO 10

C Error messages
  991 if(IOS.eq.2)then
        CALL USRMSG(' ',
     &    'Permissions error in thermal equations data file!','W')
      else
        CALL USRMSG(' ','Error in thermal equations data file!','W')
      endif
      GOTO 10
  999 if(IOS.eq.2)then
        CALL USRMSG(' ',
     &    'Permissions error rewinding thermal data file!','W')
      else
        CALL USRMSG(' ','Error rewinding thermal data file!','W')
      endif
      GOTO 10
 1000 if(IOS.eq.2)then
        CALL USRMSG(' ',
     &    'Permissions error modifying thermal data file!','W')
      else
        CALL USRMSG(' ','Error modifying thermal data file!','W')
      endif
      GOTO 10
      END

C *****************************  THERMC  ******************************

      SUBROUTINE THERMC(ICHCK)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/VTHP11/NTHF,IBTHAL(MTHF,3)
      COMMON/VTHP12/NTHEQ(MTHF),BTHDT(MTHF,MTHEQ,MDATA)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      DIMENSION LYRCHK(MCOM,MS,ME,MTHF)
      CHARACTER TEXT*115

C Initialize all checking variables .

      DO 200 I=1,NCOMP
      DO 200 J=1,NCONST(I)
      DO 200 K=1,NELTS(I,J)
      DO 200 L=1,NTHF
        LYRCHK(I,J,K,L)=0
  200 CONTINUE
      IER=0

C Clear the text feedback window
      CALL EDISP(IUOUT,' ')

C Check the total number of thermal functions
  
      IF(NTHF.LT.0.OR.NTHF.GT.MTHF)THEN
        WRITE(TEXT,'(A,A)')' FATAL ERROR: illegal number of ',
     &         'thermal equations (MAX = 12).'
        CALL EDISP(IUOUT,TEXT)
        ICHCK=1
        RETURN
      ENDIF
      DO 10 I=1,NTHF 

C Check if the total number of thermal equations per thermal functions 
C is acceptable .
       IF(NTHEQ(I).LE.0.OR.NTHEQ(I).GT.MTHEQ)THEN
         WRITE(TEXT,'(A,A,I2,A)')' WARNING: illegal number of ',
     &           'thermal equations at thermal function : ',I,'.'
         CALL EDISP(IUOUT,TEXT)
         ICHCK=1
         GOTO 10
       ENDIF
       IB1=IBTHAL(I,1)
       IB2=IBTHAL(I,2)
       IB3=IBTHAL(I,3)

C Check if the actuator location is valid.
       IF(IB1.LT.0.OR.IB1.GT.NCOMP.OR.IB2.LT.0.OR.IB2.GT.NCONST(IB1).OR.
     &   IB3.LT.0.OR.IB3.GT.NELTS(IB1,IB2))THEN
         WRITE(TEXT,'(A,A,I2,A)')' WARNING: illegal thermal ',
     &           'actuator location at thermal function : ',I,'.'
         CALL EDISP(IUOUT,TEXT)
         ICHCK=1
       ENDIF

C Check if the layer is already defined by another thermal function
C (actuator location). If that was true check if these two thermal 
C function define the same thermal property for the same temperature 
C range; and give warning massages if that was the case.

C Case 1: complete building.
        IF(IB1.EQ.0)THEN
          DO 20 ICOMP=1,NCOMP
            DO 30 ICONST=1,NCONST(ICOMP)
              DO 40 ILYR=1,NELTS(ICOMP,ICONST)
                DO 50 IFT=1,NTHF
                  ICHK=LYRCHK(ICOMP,ICONST,ILYR,IFT)
                  LYRCHK(ICOMP,ICONST,ILYR,IFT)=I
                  IF(ICHK.EQ.0)THEN
                    LYRCHK(ICOMP,ICONST,ILYR,IFT)=I
                    GOTO 40
                  ELSE
                    DO 60 IEQ=1,NTHEQ(I)
                      CALL THCHKS(I,IEQ,ICHK,ICOMP,ICONST,ILYR,IER)
                      IF(IER.NE.0)ICHCK=1
   60               CONTINUE
                  ENDIF
   50           CONTINUE
   40         CONTINUE
   30       CONTINUE
   20     CONTINUE

C Case 2: complete zone.

        ELSEIF(IB1.GT.0.AND.IB2.EQ.0)THEN
          ICOMP=IB1
          DO 70 ICONST=1,NCONST(ICOMP)
            DO 80 ILYR=1,NELTS(ICOMP,ICONST)
              DO 90 IFT=1,NTHF
                ICHK=LYRCHK(ICOMP,ICONST,ILYR,IFT)
                IF(ICHK.EQ.0)THEN
                  LYRCHK(ICOMP,ICONST,ILYR,IFT)=I
                  GOTO 80
                ELSE
                  DO 100 IEQ=1,NTHEQ(I)
                    CALL THCHKS(I,IEQ,ICHK,ICOMP,ICONST,ILYR,IER)
                    IF(IER.NE.0)ICHCK=1
  100             CONTINUE
                ENDIF
   90         CONTINUE
   80       CONTINUE
   70     CONTINUE

C Case 3: complete surface.

        ELSEIF(IB1.GT.0.AND.IB2.GT.0.AND.IB3.EQ.0)THEN
          ICOMP=IB1
          ICONST=IB2
          DO 110 ILYR=1,NELTS(ICOMP,ICONST)
            DO 120 IFT=1,NTHF
              ICHK=LYRCHK(ICOMP,ICONST,ILYR,IFT)
              IF(ICHK.EQ.0)THEN
                LYRCHK(ICOMP,ICONST,ILYR,IFT)=I
                GOTO 110
              ELSE
                DO 130 IEQ=1,NTHEQ(I)
                  CALL THCHKS(I,IEQ,ICHK,ICOMP,ICONST,ILYR,IER)
                  IF(IER.NE.0)ICHCK=1
  130           CONTINUE
              ENDIF
  120       CONTINUE
  110     CONTINUE

C Case 4: single layer.

        ELSEIF(IB1.GT.0.AND.IB2.GT.0.AND.IB3.GT.0)THEN
          ICOMP=IB1
          ICONST=IB2
          ILYR=IB3
          DO 140 IFT=1,NTHF
            ICHK=LYRCHK(ICOMP,ICONST,ILYR,IFT)
            IF(ICHK.EQ.0)THEN
              LYRCHK(ICOMP,ICONST,ILYR,IFT)=I
              GOTO 160
            ELSE
              DO 150 IEQ=1,NTHEQ(I)
                CALL THCHKS(I,IEQ,ICHK,ICOMP,ICONST,ILYR,IER)
                IF(IER.NE.0)ICHCK=1
  150         CONTINUE
            ENDIF
  140     CONTINUE
  160     CONTINUE
        ENDIF

C Now, check for each thermal equation if there is any overlapping
C in temperature range of similar type thermal equations.
        CALL THCHKF(I,IER)
        IF(IER.NE.0)ICHCK=1

C Check if the temperature range, for all thermal equations, 
C is acceptable.
        DO 350 IEQ=1,NTHEQ(I)
          IF(BTHDT(I,IEQ,2).GT.BTHDT(I,IEQ,3))THEN
            WRITE(TEXT,'(A,A,I2,A,I2,A)')' WARNING: un-acceptable ',
     &       'temperature range at function (',I,') equation (',IEQ,')'
            CALL EDISP(IUOUT,TEXT)
            ICHCK=1
          ENDIF
  350   CONTINUE
   10 CONTINUE

C Check if the layers in partitions are defined similarly
C from both sides.

      DO 170 ICN=1,NCON

C Define the partitions.
        IF(ICT(ICN).EQ.3)THEN
          IPZ1=IC1(ICN)
          IPZ2=IC2(ICN)
          IPS1=IE1(ICN)
          IPS2=IE2(ICN)
          NE=NELTS(IPZ1,IPS1)

C For each layer in the partition ; check the thermal equations
C similarity with the same layer from the other side.
          DO 180 ILYR1=1,NE
            ILYR2=NE+1-ILYR1

C For each thermal function defined.
            DO 190 IFT1=1,NTHF
              ICHK1=LYRCHK(IPZ1,IPS1,ILYR1,IFT1)
              IF(ICHK1.EQ.0)GOTO 180

C Find the similar thermal function defining the same layer from the 
C other side.
              DO 210 IFT2=1,NTHF
                ICHK2=LYRCHK(IPZ2,IPS2,ILYR2,IFT2)

C The similar function found, check the next function defining this 
C layer.
                IF(ICHK2.EQ.ICHK1)GOTO 190
  210         CONTINUE

C This layer is not defined by the same thermal function from the other
C side; check if it is defined by the same thermal equations.
              DO 220 IQ1=1,NTHEQ(ICHK1)
                DO 230 IFT2=1,NTHF
                  ICHK2=LYRCHK(IPZ2,IPS2,ILYR2,IFT2)
                  IF(ICHK2.EQ.0)THEN
        
C This layer is not defined by similar thermal equations from the other
C side; check the next layer.
       WRITE(TEXT,'(3(A,I2),2A,I2,A)')' WARNING : layer (',IPZ1,',',
     &              IPS1,',',ILYR1,') is not defined by similar ',
     &              'equations from zone (',IPZ2,')'
                    CALL EDISP(IUOUT,TEXT)
                    ICHCK=1
                    GOTO 180
                  ENDIF
                  DO 240 IQ2=1,NTHEQ(ICHK2)

C Find the similar equation from the other functions.
                    DO 250 ID=1,MDATA
                      BD1=BTHDT(ICHK1,IQ1,ID)
                      BD2=BTHDT(ICHK2,IQ2,ID)

C If any of the associated data is not similar; check the next equation
C of the other function.
                      IF(ABS(BD1-BD2).GT.1.E-8)GOTO 240
  250               CONTINUE
                    GOTO 220
  240             CONTINUE
  230           CONTINUE  

C This layer is not defined by similar thermal equations from the other
C side; check the next layer.
                WRITE(TEXT,'(3(A,I2),2A,I2,A)')' WARNING: layer (',
     &          IPZ1,',',IPS1,',',ILYR1,') is not defined by similar ',
     &          'equations from zone (',IPZ2,')'
                CALL EDISP(IUOUT,TEXT)
                ICHCK=1
                GOTO 180
  220         CONTINUE
  190       CONTINUE
  180     CONTINUE
        ENDIF
  170 CONTINUE
      RETURN
      END

C ******************************  THCHKS  *****************************
C THCHKS  checks  if  there is any  overlapping in  defining the thermal
C properties of layers.
C *********************************************************************  
      SUBROUTINE THCHKS(I1,J1,I2,IZ,IS,IL,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/VTHP12/NTHEQ(MTHF),BTHDT(MTHF,MTHEQ,MDATA)

      CHARACTER TEXT*115

      ITYP1=INT(BTHDT(I1,J1,1))
      W1=BTHDT(I1,J1,2)
      H1=BTHDT(I1,J1,3)
      DO 10 J2=1,NTHEQ(I2)
        ITYP2=INT(BTHDT(I2,J2,1))
        W2=BTHDT(I2,J2,2)
        H2=BTHDT(I2,J2,3)
        IF(ITYP1.EQ.ITYP2)THEN
          IF(((W1-W2).GT.-1.E-8.AND.W1.LT.H2.AND.H1.GT.W2).OR.
     &      ((H1-H2).LT.1.E-8.AND.H1.GT.W2.AND.W1.LT.H2).OR.
     &      (ABS(W1-H1).LT.1.E-8.AND.ABS(W2-H2).LT.1.E-8.AND.
     &       ABS(W1-W2).LT.1.E-8))THEN
            WRITE(TEXT,'(7(A,I2),A)')' WARNING: equation (',
     &            I2,',',J2,') is overlaped by eqution (',I1,',',J1,
     &            ') at layer(',IZ,',',IS,',',IL,')'
            CALL EDISP(IUOUT,TEXT)
            IER=1
          ENDIF
        ENDIF
   10 CONTINUE
      RETURN
      END

C ******************************  THCHKF  *****************************
C THCHKF  checks  if  there is any  overlapping in  defining the thermal
C properties of layers in the same thermal function.
C *********************************************************************  
      SUBROUTINE THCHKF(IF,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/VTHP12/NTHEQ(MTHF),BTHDT(MTHF,MTHEQ,MDATA)

      CHARACTER TEXT*115
   
      DO 10 J1=1,NTHEQ(IF)-1
        DO 20 J2=J1+1,NTHEQ(IF)
          IF(J1.NE.J2)THEN
            ITYP1=INT(BTHDT(IF,J1,1))
            W1=BTHDT(IF,J1,2)
            H1=BTHDT(IF,J1,3)
            ITYP2=INT(BTHDT(IF,J2,1))
            W2=BTHDT(IF,J2,2)
            H2=BTHDT(IF,J2,3)
            IF(ITYP1.EQ.ITYP2)THEN
              IF(((W1-W2).GT.-1.E-8.AND.W1.LT.H2.AND.H1.GT.W2).OR.
     &          ((H1-H2).LT.1.E-8.AND.H1.GT.W2.AND.W1.LT.H2).OR.
     &          (ABS(W1-H1).LT.1.E-8.AND.ABS(W2-H2).LT.1.E-8.AND.
     &          ABS(W1-W2).LT.1.E-8))THEN
                WRITE(TEXT,'(3(A,I2),A)')' WARNING: equation (',J1,
     &        ') is overlaped by equation (',J2,') at function (',IF,')'
                CALL EDISP(IUOUT,TEXT)
                IER=1
              ENDIF
            ENDIF
          ENDIF
   20   CONTINUE
   10 CONTINUE
      RETURN
      END
