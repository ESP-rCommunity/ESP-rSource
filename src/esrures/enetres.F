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

C ************************************************************************************
C The file contains the following routines:
C ENETRES     - the main electrical controlling routine.
C ENETRCHK    - checks for inconsistency between the network in memory and the
C               the results routine. 
C ERSMENU      - displays a menu of results entities which the user can select.
C ENETHASH    - low level routine which locates entries in the electrical results file.
C ENETGET     - low level routine which reads specific entries from the results file.
C ENETPROC    - processes the data recovered at each time step, sending the
C               data to the appropriate processing routine.
C ************************************************************************************
C Subroutine ENETRES is the main controlling routine for the recovery and
C analysis of electrical network output.
C ************************************************************************************
      SUBROUTINE ENETRES

#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/ELRFIL/ELRFLNAM
      character ELRFLNAM*144

      COMMON/TRC/ITRC

C Default library name. 
      COMMON/ABSRESLIB2/EFILEABS

C Electrical network results library details.
      COMMON/ENELIBRC/IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,NTSREC,NHREC,
     &NPREC

      DIMENSION ITEM(16)

      INTEGER IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,NTSREC,NHREC,
     &NPREC,IFOCUS

      CHARACTER LTMP*144,DFILE*144,ITEM*25,MMODE*1,
     &EFILEABS*144
      LOGICAL OK,dok,XST
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ENETRES'  ! set for cfiles

C Open the electrical results library if necessary. Suggest a file
C name based on the naming scheme for the zone and flow results.
C << to be done >>
      IER=0
    1 CALL EDISP(IUOUT,' ')
      LTMP=' '
      IF(EFILEABS(1:2).NE.'  '.AND.EFILEABS(1:3).NE.'UNK')THEN
        LTMP=EFILEABS(1:LNBLNK(EFILEABS))
      ENDIF
      ELRFLNAM=' '
      DFILE='libe'
      LLEN=LNBLNK(ltmp)
      IF(LLEN.LT.72)LLEN=72
      helptopic='res_electrical_res_lib'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKF(ltmp,'Electrical library name ? ',' ',LLEN,DFILE,
     &        'electrical library file',IER,nbhelp)
      ELRFLNAM=LTMP(1:LNBLNK(LTMP))
      CALL USRMSG(' ',' ','-')
      IER=0 

      INQUIRE (FILE=ELRFLNAM,EXIST=XST)
      IF(XST)THEN

C Set the record length and file number
        IELIBW=MELIBW
        IELFIL=IFIL+MADFIL 
        IELIBHL=MELIBH

C Open the electrical simulation results file
         CALL EFOPRAN(IELFIL,ELRFLNAM,IELIBW,1,IER)
         IF(IER.LT.0) then
          dok=.false.
          helptopic='res_electrical_res_lib'
          call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' Error opening the electrical library.',
     &    'Do you want to try again ?',OK,dok,nbhelp)
          IF(OK)THEN
            GOTO 1
          ELSE
            RETURN
          ENDIF
        ENDIF
      ELSE
        CALL ASKOK(' Cannot find this file.',
     &    'Do you want to try again ?',OK,dok,nbhelp)
        IF(.NOT.OK)RETURN
        GOTO 1
      ENDIF

C Compare the file with library header, warn if inconsistent
      CALL ENETRCHK(IER)
      IF(IER.LT.0)THEN
        dok=.false.
        helptopic='res_electrical_lib_warn'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK('Continue with electrical results analysis?',' ',
     &    OK,dok,nbhelp)
        IF(.NOT.OK) RETURN
      ENDIF

C Set up simulation period period parameters 
C      IDEFLT=1
C      CALL MOOPER

C Set the focus for results analysis an initial parameters
      IFOCUS=1

C Setup for menu text.
    2 ITEM(1)   ='1 Select result db      '
      ITEM(2)   ='  <Not applicable>      '
      ITEM(3)   ='3 Define output period  '
      ITEM(4)   ='  --------------------- '
      ITEM(5)   ='a Graphs                '
      ITEM(6)   ='b Tabular report        '
      ITEM(7)   ='c Statistics            '
      ITEM(8)   ='d Performance indicators'
      ITEM(9)   ='e IPV <N/A>             '
      ITEM(10)  ='  --------------------- '
      IF(ITRC.EQ.0)THEN
        ITEM(11)='r Report >> ON          '
      ELSE
        ITEM(11)='r Report >> OFF         '
      ENDIF
      ITEM(12)=  '                        '
      ITEM(13)=  '? Help                  '
      ITEM(14)=  '- Exit                  '

C Help text for this menu.
      helptopic='res_electrical_lib_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      NITMS=14
      INO=-2
      if(mmod.eq.8)then
        CALL EMENU('electrical analysis',ITEM,NITMS,INO)
      else
        CALL EMENU('electrical',ITEM,NITMS,INO)
      endif
      IF(INO.EQ.NITMS)THEN
        RETURN
      ELSEIF(INO.EQ.NITMS-1)THEN
        helptopic='res_electrical_lib_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('eresults help',nbhelp,'-',0,0,IER)        
      ELSEIF(INO.EQ.NITMS-3)THEN
        IF(ITRC.EQ.1)THEN
          ITRC=0
        ELSE
          ITRC=1
        ENDIF
      ELSEIF(INO.EQ.1)THEN
        GOTO 1
      ELSEIF(INO.EQ.2)THEN
        CONTINUE
      ELSEIF(INO.EQ.3)THEN
        CALL MOOPER
      ELSEIF(INO.EQ.5)THEN

C Check that the user is in graphical mode.
        IF(MMOD.LT.8.AND.MMOD.NE.-6)THEN
          CALL USRMSG('Switch to graphic mode for this facility.',
     &                ' ','W')
          GOTO 2
        ELSE
          MMODE='G'
          CALL ERSMENU(IFOCUS,MMODE)
        ENDIF
      ELSEIF(INO.EQ.6)THEN
        MMODE='T'
        CALL ERSMENU(IFOCUS,MMODE)
      ELSEIF(INO.EQ.7)THEN
        MMODE='S'
C        CALL ERSMENU(IFOCUS,MMODE)
        CALL EDISP(IUOUT,'Sorry, not available yet')
      ELSEIF(INO.EQ.10)THEN
C        CALL EPERMENU
        CALL EDISP(IUOUT,'Sorry, not available yet')
      ELSEIF(INO.EQ.9)THEN
        CALL EDISP(IUOUT,'Sorry, not available yet')
      ELSEIF(INO.EQ.11)THEN
        IF(ITRC.EQ.1)THEN
          ITRC=2
        ELSE
          ITRC=1
        ENDIF
      ELSE
        GOTO 2
      ENDIF

      IF(INO.LT.0.OR.INO.GT.NITMS)THEN
        GOTO 2
      ELSE
        GOTO 2
      ENDIF

      RETURN
      END

C **********************************************************************
C Subroutine ENETRCHK compares the data in the results library header with
C that in the electrical network common block. The routine returns an
C error if this data is found to be inconsistent. 
      SUBROUTINE ENETRCHK(IER)

#include "power.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Common for electrical file 
      COMMON/ENTFILE/ENTFLNAM,IENTXIST

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The connected hybrid components
      COMMON/HYBE/NHYBCOM,HYBCOMNO(MHYCOM),HYCOMTYP(MHYCOM),
     &HYCOMPHTYP(MHYCOM),HYCONEN(MHYCOM,MPHAS),HYLOC(MHYCOM,MPHAS),
     &NHYBDAT(MHYCOM),HYBDAT(MHYCOM,MHYDAT)

C The connected power only components
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C Electrical network results library details.
      COMMON/ENELIBRC/IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,NTSREC,NHREC,
     &NPREC

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NHYBCOM,HYBCOMNO,
     &HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,NHYBDAT,NPOWCOM,POWCOMNO,
     &POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,NPOWCDATS,NECON,
     &ECONO,CONPH,CONPHTYP,CCNO,SENOD,EENOD,IENTXIST
     
      REAL ENODBASEV,ENODBASEP,HYBDAT,POWCDAT

C Characters
      CHARACTER*72 ENTFLNAM,LFIL
      CHARACTER*124 OUTS

C Read the electrical network header (held in the 1st record of the 
C library. 
      IREC=1
      READ(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999) LFIL
      IREC=IREC+1
      READ(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999) NENODR,NECONR,
     &NHYBCOMR,NPOWCOMR,NHREC,NPREC,NTSREC
      IREC=IREC+1
      READ(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999) NTSTEP,NTSTPP,NETS
      IER=0

C Check consistency between library and the current network file.
C Firstly check that a file has already been read in. 
      IF(ENTFLNAM(1:2).EQ.'  '.OR.ENTFLNAM(1:3).EQ.'UNK')THEN
        CALL ENETINIT  
        CALL ENETREAD('N')
      ELSE
        CALL ENETINIT
        CALL ENETREAD('S')
      ENDIF

      IF(LFIL(1:LNBLNK(LFIL)).NE.ENTFLNAM(1:LNBLNK(ENTFLNAM)))THEN
        CALL EDISP(IUOUT,'WARNING: inconsistent file names: ')
        WRITE(OUTS,'(A,A)')
     &'This results file is associated with: ',
     &LFIL(1:LNBLNK(LFIL))
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(A,A)')'The current electrical network is: ',
     &ENTFLNAM(1:LNBLNK(ENTFLNAM))
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,' ')
        IER=-1

C Check for inconsistent numbers of nodes
      ELSEIF(NENOD.NE.NENODR)THEN
        CALL EDISP(IUOUT,'WARNING: inconsistent numbers of nodes: ')
        WRITE(OUTS,'(A,I3)')
     &'No of nodes in results library: ',NENODR
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,' ')
        WRITE(OUTS,'(A,I3)')
     &'No. of nodes in the current electrical network: ',NENOD
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,' ')
        IER=-1

C Check for inconsistent numbers of connections
      ELSEIF(NECON.NE.NECONR)THEN
        CALL EDISP(IUOUT,'WARNING: inconsistent number of connections:')
        WRITE(OUTS,'(A,I3)')
     &'No of connections in results library: ',NECONR
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(A,I3)')
     &'No. of nodes in the current electrical network: ',NECON
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,' ')
        IER=-1

C Check for inconsistent numbers of hybrid components
      ELSEIF(NHYBCOM.NE.NHYBCOMR)THEN

        CALL EDISP(IUOUT,'WARNING: inconsistent numbers of connected ')
        WRITE(OUTS,'(A)')
        CALL EDISP(IUOUT,'HVAC/PV/lights/etc. :')
        WRITE(OUTS,'(A,I3)')
     &'No of connected elements in results library: ',NHYBCOMR
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(A,I3)')
     &'No. of connected elements in the electrical network: ',NHYBCOM
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,' ')
        IER=-1

C Check for inconsistent numbers of power-only components
      ELSEIF(NPOWCOM.NE.NPOWCOMR)THEN
        CALL EDISP(IUOUT,'WARNING: inconsistent numbers of connected ')
        WRITE(OUTS,'(A)')
        CALL EDISP(IUOUT,'power-only components:')
        WRITE(OUTS,'(A,I3)')
     &'No. of power-only components in results library: ',NPOWCOM
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(A,I3)')
     &'No. of power-only components in the electrical network: ',
     &NPOWCOMR
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,' ')
        IER=-1

      ENDIF 

      RETURN 

999   CALL EDISP(IUOUT,'ERROR reading the library header')
      IER=-1
      RETURN

      END
      
C **********************************************************************
C Subroutine ERMENU displays a menu giving the information that can be retrieved 
C from the results library by the user. The menu can be used in graphing, 
C statistics and tabular mode. The focus of the menu is selected by the user
C and this alters the menu presented. 
C Variables in this routine:
C IEGET         - the number of picked elements
C IEGETNO(*)    - picked element data array
C IEGETNO(*,1)  - picked element number
C IEGETNO(*,2)  - picked element type
C IEGETNO(*,3)  - picked type number (e.g. node number)
C IEGETNO(*,4)  - picked element type entity i.d. (e.g. nodal voltage)
C IEGETNO(*,5)  - result set no (currently not used)
C IEAXIS(*,6)  - associated axis type: 
C                 1=voltage (V) 
C                 2=current (A) 
C                 3=power (W/VAR) 
C                 4=misc for power factor and angle (rads)
C IESYMB(*,6)   - symbol type index
C IELINE(*,6)   - line type index
C IELLAB(*,6)   - long label(s) for data item
C IESLAB(*,6)   - short label(s) for data item
C IEULAB(*,6)   - units(s) for data item short label
C 
C The graphing data assigned to each is as follows IEGETNO
C Line type  Axis    Symbol    Label           Description 
C **********************************************************************        
C   1         1       1,11      Node ? V        Node No. ? Voltage          
C  -2         3       3,13      Node ? load     Node No. ? Load            
C  -2         3       4,14      Node ? gen      Node No. ? Generation          
C  -2         3       5,15      Node ? trn      Node No. ? Transmission      
C  -2         3       6,16      Node ? imp      Node No. ? Import   
C  -3         4       7,17      Node ? Lpf      Node No. ? Load power factor 
C  -3         4       8,18      Node ? Gpf      Node No. ? Generation power factor  
C  -3         4       8,18      Node ? Ipf      Node No. ? Import power factor  
C  -2         3       9,19      Conn ? trn      Connection No. ? Transmission  
C  -2         3       10        Conn ? loss     Connection No. ? Loss         
C  -2         3       11        Conn ? p.f.     Connection No. ? Power factor  
C   1         2       12,2      Conn ? curr     Connection No. ? Current      
C  -2         3       13,3      POC ? power     Power only component No. ? Power
C  -2         3       14,4      CMP ? power     Hybrid (HVAC,PV,etc.) No. ? Power
C  -2         3       15,5      Net imp.        Network imported power              
C  -2         3       14,5      Net load        Network load                  
C  -2         3       16,6      Net gen         Network generation            
C   -         -       -         Net trn         Network transmission (not used) 
C  -3         4       16        Net imp pf      Network import p.f.           
C  -3         4       17        Net load pf     Network load p.f.             
C  -3         4       18        Net genr pf     Network genr p.f.           
C  -2         3       19        Net loss        Network loss                
C  -2         3       20,7      Phase ? load    Network phase loading 
C  -2         3       20,8      Phase ? load    Network phase generation
C  -2         3       20,9      Phase ? load    Network phase import       
C ********************************************************************** 

      SUBROUTINE ERSMENU(IFOC,MODE)

#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN

C Export parameters. 
      COMMON/EXPORTI/IXOPEN,IXUNIT,IXPUNIT
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      COMMON/EXPORTTG/XFILE,TG,DELIM
      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN,horaxisdiv

C Data filters
      COMMON/ENFILT/EFILT(MERAWD)
            
C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The connected hybrid components
      COMMON/HYBE/NHYBCOM,HYBCOMNO(MHYCOM),HYCOMTYP(MHYCOM),
     &HYCOMPHTYP(MHYCOM),HYCONEN(MHYCOM,MPHAS),HYLOC(MHYCOM,MPHAS),
     &NHYBDAT(MHYCOM),HYBDAT(MHYCOM,MHYDAT)

C The connected power only components
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)


C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)
     
C Array of picked items
      COMMON/ENETGETN/IEGET,IEGETNO(MENOD,9)
      COMMON/ENETGETS/IELLAB(MENOD,MERPOS),IESLAB(MENOD,MERPOS),
     &IEULAB(MENOD,MERPOS)
      COMMON/ENETGETM/IEAXIS(MENOD,MERPOS),IESYMB(MENOD,MERPOS),
     &IELINE(MENOD,MERPOS)

      DIMENSION ITEM(40)
  
C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NHYBCOM,HYBCOMNO,
     &HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,NHYBDAT,NPOWCOM,POWCOMNO,
     &POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,NPOWCDATS,NECON,
     &ECONO,CONPH,CONPHTYP,CCNO,SENOD,EENOD,IEGET,IEGETNO,IUOUT,
     &IUIN,YAXSET,ADDLIN,horaxisdiv,EFILT

      DIMENSION IND(MENOD),ICN(MECON),IHY(MHYCOM),IPW(MPOWCOM)
     
      REAL ENODBASEV,ENODBASEP,HYBDAT,POWCDAT

      CHARACTER MODE*1

      CHARACTER ITEM*25,IELLAB*64,IESLAB*16,IEULAB*4,DELIM*1,
     &TG*1,XFILE*144,HEADER*32,VALUES*20
      integer IWM  ! for radio button
      integer NITEMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ERSMENU'  ! set for subroutine

C Firstly clear all existing selections and initialise variables.
      CALL EDISP (IUOUT,' ')
      CALL EDISP (IUOUT,'Clearing all data selections ...')
      CALL EDISP (IUOUT,' ')

C Set all data displays to ON
      DO 8 I=1,MERAWD
        EFILT(I)=1
8     CONTINUE
      ADDLIN=0
      IHFLAG=0
      DELIM='-'
      DO 10 I=1,MENOD
        IEGET=0
        DO 12 J=1,9
          IEGETNO(I,J)=0
  12    CONTINUE
        DO 14 K=1,6
          IELLAB(I,K)=' '
          IESLAB(I,K)=' '
          IEAXIS(I,K)=0
          IESYMB(I,K)=0
          IELINE(I,K)=0
 14     CONTINUE
 10   CONTINUE

 20   NBITM=0
      NITEMS=0
      DO 16 K=1,40
        ITEM(K)=' '
16    CONTINUE

      INO=-4
C Make up the menu based on the focus. Where 1=nodes, 2=cables,
C 3=components, 4=CHP, otherwise whole network.
      IF(IFOC.EQ.1)THEN
        ITEM(1)   =' Focus >> Nodes          '
        ITEM(2)   =' ----------------------- '
        ITEM(3)   =' Voltage ...             '
        ITEM(4)   ='a Phasor voltage         '
        ITEM(5)   =' ----------------------- '
        ITEM(6)   =' Power flow ...          '
        ITEM(7)   ='b Loading                '
        ITEM(8)   ='c Generation             '
        ITEM(9)   ='d Transmission           '
        ITEM(10)  ='d Grid import            '
        ITEM(11)  =' ----------------------- '
        ITEM(12)  =' Power factor (p.f.) ..  '
        ITEM(13)  ='e Load p.f.              '
        ITEM(14)  ='f Generation p.f.        '
        ITEM(15)  ='g Import power p.f.      '
        ITEM(16)  =' ----------------------- '

        WRITE(ITEM(17),'(A,I3,A)')
     &             ' Selected ... (',IEGET,')'
        ITEM(18)  =' ----------------------- '
        NBITM=18
      ELSEIF(IFOC.EQ.2)THEN
        ITEM(1)   =' Focus >> Lines/cables.. '
        ITEM(2)   =' ----------------------- '
        ITEM(3)   =' Current ...             '
        ITEM(4)   ='a Phasor current         '
        ITEM(5)   =' ----------------------- '
        ITEM(6)   =' Power flow ...          '
        ITEM(7)   ='b Transmitted power      '
        ITEM(8)   ='c Power loss             '
        ITEM(9)   =' ----------------------  '
        ITEM(10)  =' Power factor (p.f.) ..  '
        ITEM(11)  ='d Transmission p.f.      '
        ITEM(12)  =' ----------------------- '
        WRITE(ITEM(13),'(A,I3,A)')
     &             ' Selected ... (',IEGET,')'
        ITEM(14)  =' ----------------------- '
        NBITM=14
      ELSEIF(IFOC.EQ.3)THEN
        ITEM(1)   =' Focus >> Power comps.   '
        ITEM(2)   =' ----------------------- '
        ITEM(3)   =' Power flow ...          '
        ITEM(4)   ='a Component power flow   '
        ITEM(5)   =' ----------------------- '
        ITEM(6)   =' Miscellaneous ...       '
        ITEM(7)   ='b Additional output N/A  '
        ITEM(8)   =' ----------------------- '
        WRITE(ITEM(9),'(A,I3,A)')
     &             ' Selected ...(',IEGET,') '
        ITEM(10)  =' ----------------------- '
        NBITM=10
      ELSEIF(IFOC.EQ.4)THEN
        ITEM(1)   =' Focus >> HVAC/PV/CHP..  '
        ITEM(2)   =' ----------------------- '
        ITEM(3)   =' Power flow ...          '
        ITEM(4)   ='a Component power flow   '
        ITEM(5)   =' ----------------------- '
        ITEM(6)   =' Miscellaneous ...       '
        ITEM(7)   ='b Additional output  N/A '
        ITEM(8)   =' ----------------------- '
        WRITE(ITEM(9),'(A,I3,A)')
     &             ' Selected ...  (',IEGET,')'
        ITEM(10)  =' ----------------------- '
        NBITM=10
      ELSE
        ITEM(1)   =' Focus >> Whole network  '
        ITEM(2)   =' ----------------------- '
        ITEM(3)   =' Power flow ...          '
        ITEM(4)   ='a Total loading          '
        ITEM(5)   ='b Total generation       '
        ITEM(6)   ='c Total grid import      '
        ITEM(7)   ='d Total losses           '
        ITEM(8)   =' ----------------------- '
        ITEM(9)   =' Power factor (p.f.) ..  '
        ITEM(10)  ='e Loading p.f.           '      
        ITEM(11)  ='f Generation p.f.        '
        ITEM(12)  ='g Import power p.f.      '
        ITEM(13)  =' ----------------------- '
        ITEM(14)  =' Miscellaneous ...       '
        ITEM(15)  ='h Loading by phase       '
        ITEM(16)  ='i Generation by phase    '
        ITEM(17)  ='j Import by phase        '
        ITEM(18)  =' ----------------------- '
        WRITE(ITEM(19),'(A,I3,A)')
     &             ' Selected ...  (',IEGET,')'
        ITEM(20)  =' ----------------------- '
        NBITM=20
      ENDIF

C Write out the the menu tail.     
      IF(MODE.EQ.'G')THEN
        HEADER='Graphical output '
        ITEM(NBITM+1)   ='/ Clear all selections   '
        ITEM(NBITM+2)   ='@ Set axis scale         '
        ITEM(NBITM+3)   ='+ Add another profile    '
        ITEM(NBITM+4)   ='^ Change to tabular/stats'
        ITEM(NBITM+5)   ='# Hide/show output       '
        ITEM(NBITM+6)   ='! Draw graph             '
        ITEM(NBITM+7)   ='                         '
        ITEM(NBITM+8)   ='? Help                   '
        ITEM(NBITM+9)   ='- Exit                   ' 
        NITEMS=NBITM+9
      ELSEIF(MODE.EQ.'T')THEN
        HEADER='Tabular output '
        ITEM(NBITM+1)   ='/ Clear all selections   '
        IF(IXOPEN.EQ.1)then
          ITEM(NBITM+2) ='> Display to >> file     '
        ELSEIF(IXOPEN.EQ.0)then
          ITEM(NBITM+2) ='> Display to >> screen   '
        ENDIF
        IF(IHFLAG.EQ.0)THEN  
          ITEM(NBITM+3)  ='* Time >> 10h30m11s     '
        ELSEIF(IHFLAG.EQ.1)THEN
          ITEM(NBITM+3)  ='* Time >> 0.4375        '
        ENDIF
        IF(DELIM.EQ.'-'.OR.DELIM.EQ.'N')then
          ITEM(NBITM+4) ='^ Delim >> normal        '
        ELSEIF(DELIM.EQ.'T')then             
          ITEM(NBITM+4) ='^ Delim >> tab           '
        ELSEIF(DELIM.EQ.'C')then
          ITEM(NBITM+4) ='^ Delim >> comma         '
        ELSEIF(DELIM.EQ.'S')then
          ITEM(NBITM+4) ='^ Delim >> space         '
        elseif(delim.eq.'X')then
          ITEM(NBITM+4) ='^ delim >> tagged        '
        ENDIF
        ITEM(NBITM+5)   ='+ Change to graph/stats  '
        ITEM(NBITM+6)   ='# Hide/show output       '
        ITEM(NBITM+7)   ='! List output            '
        ITEM(NBITM+8)   ='                         '
        ITEM(NBITM+9)   ='? Help                   '
        ITEM(NBITM+10)  ='- Exit                   ' 
        NITEMS=NBITM+10
      ELSEIF(MODE.EQ.'S')THEN
        HEADER='Statistical output '
        ITEM(NBITM+1)   ='/ Clear all selections   '
        IF(IXOPEN.EQ.1)then
          ITEM(NBITM+2) ='> Display to >> file     '
        ELSEIF(IXOPEN.EQ.0)then
          ITEM(NBITM+2) ='> Display to >> screen   '
        ENDIF
        ITEM(NBITM+3)   ='+ Change to graph/tabular'
        ITEM(NBITM+4)   ='# Hide/show output       '
        ITEM(NBITM+5)   ='! List output            '
        ITEM(NBITM+6)   ='                         '
        ITEM(NBITM+7)   ='? Help                   '
        ITEM(NBITM+8)   ='- Exit                   ' 
        NITEMS=NBITM+8
      ENDIF

C Help text for the menu.
      helptopic='res_electrical_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Set up the menu.
      INO=-2
      CALL EMENU(HEADER,ITEM,NITEMS,INO)

C Generic menu picks - change focus, help and exit      
      IF(INO.EQ.1)THEN
        IF(IFOC.LT.5)THEN
          IFOC=IFOC+1
        ELSE
          IFOC=1 
        ENDIF
        GOTO 20
      ELSEIF(INO.EQ.NBITM+1)THEN

C Clear all existing selections
      DO 28 I=1,MERAWD
        EFILT(I)=1
  28  CONTINUE
      DO 30 I=1,MENOD
          IEGET=0
          DO 40 J=1,8
            IEGETNO(I,J)=0
  40      CONTINUE
          DO 50 K=1,6
            IELLAB(I,K)=' '
            IESLAB(I,K)=' '
            IEAXIS(I,K)=0
            IESYMB(I,K)=0
            IELINE(I,K)=0
 50       CONTINUE
 30     CONTINUE 
        GOTO 20      
      ELSEIF(INO.EQ.NITEMS-4)THEN 
        CALL ENRFILT
        GOTO 20
      ELSEIF(INO.EQ.NITEMS-5)THEN

C Change the mode
        CALL EASKABCD('What output type do you want?',' ','Graphical',
     &'Tabular','Statistics','Continue',IOPT,IER)
        IF(IOPT.EQ.1) THEN
          MODE='G'
        ELSEIF(IOPT.EQ.2)THEN
          MODE='T'
        ELSEIF(IOPT.EQ.3)THEN
          MODE='S'
        ENDIF
        GOTO 20
      ELSEIF(INO.EQ.NITEMS-2)THEN
        GOTO 20
      ELSEIF(INO.EQ.NITEMS-3)THEN

C Display the output
        IF(MODE.EQ.'G')THEN
          CALL ENETPROC('G')
        ELSEIF(MODE.EQ.'T')THEN
          CALL ENETPROC('T')
        ELSEIF(MODE.EQ.'S')THEN
          CALL ENETPROC('S')
        ENDIF
        GOTO 20
      ELSEIF(INO.EQ.NITEMS-1)THEN
        helptopic='res_electrical_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD(HEADER,21,'-',0,0,IER)
        GOTO 20
      ELSEIF(INO.EQ.NITEMS)THEN
        RETURN

C Mode specific control menu items
      ELSEIF(INO.GT.NBITM+1.AND.INO.LT.NITEMS-5)THEN  
        IF(MODE.EQ.'G')THEN

C Graphics control menu pics
          IF(INO.EQ.NBITM+2)THEN

C Set the axis scale
            helptopic='res_electrical_y_axis'
            call gethelptext(helpinsub,helptopic,nbhelp)
            DO 555 IAXIS=1,4
              IF (YAXSET(IAXIS).GT.0) THEN
 501            WRITE (VALUES,'(2(f9.2,1x))') YMIN(IAXIS),YMAX(IAXIS)
                IF (IAXIS.EQ.1) THEN
                  CALL EASKS(VALUES,'  ','Min &max for voltage axis?',
     &              20,' 0. 9999.','temp axis',IER,nbhelp)
                ELSEIF (IAXIS.EQ.2) THEN
                  CALL EASKS(VALUES,'  ','Min & max for current axis?',
     &              20,' 0. 9999. ','temp axis',IER,nbhelp)
                ELSEIF (IAXIS.EQ.3) THEN
                  CALL EASKS(VALUES,'  ','Min & max for power axis?',
     &              20,'0. 1000.','temp axis',IER,nbhelp)
                ELSEIF (IAXIS.EQ.4) THEN
                  CALL EASKS(VALUES,'  ','Min & max for misc axis?',
     &              20,' 0. 1. ','temp axis',IER,nbhelp)
                ENDIF
                CALL USRMSG(' ',' ','-')
                K=0
                CALL EGETWR(VALUES,K,YN,0.,0.,'-','Y min',IER)
                CALL EGETWR(VALUES,K,YM,0.,0.,'-','Y max',IER)
                IF(YN.GT.YM) THEN
                  CALL USRMSG('Maximum value less than minimum value.',
     &           ' Please respecify. ','W')
                  GOTO 501
                ENDIF
                YMAX(IAXIS)=YM
                YMIN(IAXIS)=YN
                YAXSET(IAXIS)=2
              ENDIF
              ADDLIN=1
 555        CONTINUE
            GOTO 20
          ELSEIF(INO.EQ.NBITM+3)THEN

C Add another proifile without re-scaling
            ADDLIN=1
            GOTO 20
          ENDIF 
        ELSEIF(MODE.EQ.'T')THEN
          IF(INO.EQ.NBITM+2)THEN

C Send output to a file
            CALL CTLEXP(XFILE,IXOPEN,IXUNIT,IXPUNIT,'T','Text',IER)
          ELSEIF(INO.EQ.NBITM+3)THEN

C Change time display
C Toggle time format and day separators.
            IHFLAG=IHFLAG+1
            IF(IHFLAG.GT.1)IHFLAG=0
            IF(IHFLAG.EQ.0)THEN
              CALL EDISP(IUOUT,' ')
              CALL EDISP(IUOUT,'standard display time = 10h30 set ')
            ELSEIF(IHFLAG.EQ.1)THEN
              CALL EDISP(IUOUT,' ')
              CALL EDISP(IUOUT,
     &'3rd party graphing time=day fraction set')
            ENDIF
            helptopic='res_electrical_separator'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKAB('Include mark between days when ',
     &        'displaying or writing data :','no','yes',IDH,nbhelp)
            IDHFLG=IDH-1

          ELSEIF(INO.EQ.NBITM+4)THEN

C Change the delimiter type. 
C Toggle delimeter.
            helptopic='res_electrical_delimiter'
            call gethelptext(helpinsub,helptopic,nbhelp)
            IWM=1
            CALL EASKATOG('Delimeter to use between columns of data:',
     &        ' ','normal spaces','single space','tab','comma','tagged',
     &        'continue',' ',IWM,nbhelp)
            if(iwm.eq.1)then
              delim = '-'
            elseif(iwm.eq.2)then
              delim = 'S'
            elseif(iwm.eq.3)then
              delim = 'T'
            elseif(iwm.eq.4)then
              delim = 'C'
            elseif(iwm.eq.5)then
              delim = 'X'
            endif
          ELSE
            GOTO 20
          ENDIF
        ELSEIF(MODE.EQ.'S')THEN 
          IF(INO.EQ.NBITM+2)THEN 

C Send the output to file.
          ELSE
            GOTO 20
          ENDIF
        ELSE
          GOTO 20
        ENDIF  
        GOTO 20   
      ENDIF

C Check that the maximum number of items has not been exceeded.
      IF(IEGET.GE.MENOD) THEN
        CALL EDISP(IUOUT,'The maximum number of recoverable entities ')
        CALL EDISP(IUOUT,'has been reached ... you cannot select any')
        CALL EDISP(IUOUT,'more data for display.')
        GOTO 20
      ENDIF



C Focus-specific menu picks.
      IF(IFOC.EQ.1) THEN

C Focus-specific selections, need to test for correct pick.
        IF(INO.EQ.4.OR.INO.EQ.7.OR.INO.EQ.8.OR.INO.EQ.9
     &.OR.INO.EQ.10.OR.INO.EQ.13.OR.INO.EQ.14.OR.INO.EQ.15)THEN
          CALL USRMSG('Select one or more nodes: ',' ','-')
          CALL ELNODMLS(NNOD,IND)
          CALL USRMSG(' ',' ','-')
        ELSE
          INO=-4
          GOTO 20
        ENDIF
        DO 60 IN=1,NNOD
          INOD=IND(IN)
          IF(INOD.LT.1.OR.INOD.GT.NENOD) GOTO 60
          IEGET=IEGET+1
          IF(INO.EQ.4)THEN

C The nodal voltage
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=1
            IEGETNO(IEGET,3)=INOD
            IEGETNO(IEGET,4)=1
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=1
            IELINE(IEGET,2)=-3
            IESYMB(IEGET,1)=1
            IESYMB(IEGET,2)=11
            IEAXIS(IEGET,1)=1
            IEAXIS(IEGET,2)=4
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Node ',INOD,' Voltage magnitude'   
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Node ',INOD,' Voltage phase angle'
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Node ',INOD,' Volt'
            WRITE(IEULAB(IEGET,1),'(A)') 'V'
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'Node ',INOD,' Ang' 
            WRITE(IEULAB(IEGET,2),'(A)') 'Degs'      
          ELSEIF(INO.EQ.7)THEN

C The nodal load
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=1
            IEGETNO(IEGET,3)=INOD
            IEGETNO(IEGET,4)=2
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=-2
            IELINE(IEGET,2)=-2
            IESYMB(IEGET,1)=3
            IESYMB(IEGET,2)=13
            IEAXIS(IEGET,1)=3
            IEAXIS(IEGET,2)=3
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Node ',INOD,' Loading (W)'  
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Node ',INOD,' Loading (VA)'   
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Node ',INOD,' Load' 
            WRITE(IEULAB(IEGET,1),'(A)') 'W'   
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'Node ',INOD,' Load'  
            WRITE(IEULAB(IEGET,2),'(A)') 'VAR'  
          ELSEIF(INO.EQ.8)THEN

C The nodal generation
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=1
            IEGETNO(IEGET,3)=INOD
            IEGETNO(IEGET,4)=3
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=-2
            IELINE(IEGET,2)=-2
            IESYMB(IEGET,1)=4
            IESYMB(IEGET,2)=14
            IEAXIS(IEGET,1)=3
            IEAXIS(IEGET,2)=3
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Node ',INOD,' Generation (W)' 
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Node ',INOD,' Generation (VA)'   
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Node ',INOD,' Gen'
            WRITE(IEULAB(IEGET,1),'(A)') 'W'   
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'Node ',INOD,' Gen'
            WRITE(IEULAB(IEGET,2),'(A)') 'VAR'  

          ELSEIF(INO.EQ.9)THEN

C The nodal transmission
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=1
            IEGETNO(IEGET,3)=INOD
            IEGETNO(IEGET,4)=4
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=-2
            IELINE(IEGET,2)=-2
            IESYMB(IEGET,1)=5
            IESYMB(IEGET,2)=15
            IEAXIS(IEGET,1)=3
            IEAXIS(IEGET,2)=3
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Node ',INOD,' Transmission (W)' 
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Node ',INOD,' Transmission (VA)'   
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Node ',INOD,' Trn'
            WRITE(IEULAB(IEGET,1),'(A)') 'W'  
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'Node ',INOD,' Trn'
            WRITE(IEULAB(IEGET,2),'(A)') 'VAR' 
          ELSEIF(INO.EQ.10)THEN

C The nodal grid import
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=1
            IEGETNO(IEGET,3)=INOD
            IEGETNO(IEGET,4)=5
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=-2
            IELINE(IEGET,2)=-2
            IESYMB(IEGET,1)=6
            IESYMB(IEGET,2)=16
            IEAXIS(IEGET,1)=3
            IEAXIS(IEGET,2)=3
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Node ',INOD,' Grid import (W)' 
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Node ',INOD,' Grid import (VA)'   
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Node ',INOD,' Im'
            WRITE(IEULAB(IEGET,1),'(A)') 'W' 
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'Node ',INOD,' Im'
            WRITE(IEULAB(IEGET,2),'(A)') 'VAR' 

          ELSEIF(INO.EQ.13)THEN

C The nodal load power factor
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=1
            IEGETNO(IEGET,3)=INOD
            IEGETNO(IEGET,4)=6
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=-3
            IESYMB(IEGET,1)=7
            IEAXIS(IEGET,1)=4
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Node ',INOD,' Load power factor' 
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Node ',INOD,' L pf'
            WRITE(IEULAB(IEGET,1),'(A)') '-'           

          ELSEIF(INO.EQ.14)THEN

C The nodal generation power factor
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=1
            IEGETNO(IEGET,3)=INOD
            IEGETNO(IEGET,4)=7
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=-3
            IESYMB(IEGET,1)=8
            IEAXIS(IEGET,1)=4
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Node ',INOD,' Generation power factor' 
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Node ',INOD,' G pf'
            WRITE(IEULAB(IEGET,1),'(A)') '-'
          ELSEIF(INO.EQ.15)THEN

C The nodal import power factor
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=1
            IEGETNO(IEGET,3)=INOD
            IEGETNO(IEGET,4)=8
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=-3
            IESYMB(IEGET,1)=9
            IEAXIS(IEGET,1)=4
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Node ',INOD,' Import power factor' 
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Node ',INOD,' I pf'
            WRITE(IEULAB(IEGET,1),'(A)') '-'
          ELSE
            IF(IEGET.GT.0)IEGET=IEGET-1
            GOTO 20
          ENDIF
  60    CONTINUE
      ELSEIF(IFOC.EQ.2) THEN
        IF(INO.EQ.4.OR.INO.EQ.7.OR.INO.EQ.8.OR.INO.EQ.11)THEN
          CALL USRMSG('Select one or more connections: ',' ','-')
          CALL ELCONMLS(NCN,ICN)
          CALL USRMSG(' ',' ','-')
        ELSE
          INO=-4
          GOTO 20
        ENDIF
        DO 70 IC=1,NCN
          ICON=ICN(IC)
          IF(ICON.LT.1.OR.ICON.GT.NECON) GOTO 70 
          IEGET=IEGET+1
         
          IF(INO.EQ.4)THEN

C The connection current
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=2
            IEGETNO(IEGET,3)=ICON
            IEGETNO(IEGET,4)=4
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=1
            IELINE(IEGET,2)=-2
            IELINE(IEGET,3)=1
            IELINE(IEGET,4)=-2
            IELINE(IEGET,5)=1
            IELINE(IEGET,6)=-2
            IESYMB(IEGET,1)=13
            IESYMB(IEGET,2)=3
            IESYMB(IEGET,3)=13
            IESYMB(IEGET,4)=3
            IESYMB(IEGET,5)=13
            IESYMB(IEGET,6)=3
            IEAXIS(IEGET,1)=2
            IEAXIS(IEGET,2)=4
            IEAXIS(IEGET,3)=2
            IEAXIS(IEGET,4)=4
            IEAXIS(IEGET,5)=2
            IEAXIS(IEGET,6)=4
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Connection ',IEGET,' Current magnitude ph1'   
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Connection ',IEGET,' Current phase angle ph1'
            WRITE(IELLAB(IEGET,3),'(A,I3,A)') 
     &'Connection ',IEGET,' Current magnitude ph2'   
            WRITE(IELLAB(IEGET,4),'(A,I3,A)') 
     &'Connection ',IEGET,' Current phase angle ph2' 
            WRITE(IELLAB(IEGET,5),'(A,I3,A)') 
     &'Connection ',IEGET,' Current magnitude ph3'   
            WRITE(IELLAB(IEGET,6),'(A,I3,A)') 
     &'Connection ',IEGET,' Current phase angle ph3' 
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Con ',ICON,' I ph1'
            WRITE(IEULAB(IEGET,1),'(A)') 'A'
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'Con ',ICON,' Ang ph1' 
            WRITE(IEULAB(IEGET,2),'(A)') 'Degs'
            WRITE(IESLAB(IEGET,3),'(A,I3,A)') 'Con ',ICON,' I ph2'
            WRITE(IEULAB(IEGET,3),'(A)') 'A'
            WRITE(IESLAB(IEGET,4),'(A,I3,A)') 'Con ',ICON,' Ang ph2'
            WRITE(IEULAB(IEGET,4),'(A)') 'Degs'
            WRITE(IESLAB(IEGET,5),'(A,I3,A)') 'Con ',ICON,' I ph3'
            WRITE(IEULAB(IEGET,5),'(A)') 'A'
            WRITE(IESLAB(IEGET,6),'(A,I3,A)') 'Con ',ICON,' Ang ph3' 
            WRITE(IEULAB(IEGET,6),'(A)') 'Degs'

          ELSEIF(INO.EQ.7)THEN

C The connection transmission
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=2
            IEGETNO(IEGET,3)=ICON
            IEGETNO(IEGET,4)=1
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=1
            IELINE(IEGET,2)=1
            IELINE(IEGET,3)=1
            IELINE(IEGET,4)=1
            IELINE(IEGET,5)=1
            IELINE(IEGET,6)=1
            IESYMB(IEGET,1)=19
            IESYMB(IEGET,2)=9
            IESYMB(IEGET,3)=19
            IESYMB(IEGET,4)=9
            IESYMB(IEGET,5)=19
            IESYMB(IEGET,6)=9
            IEAXIS(IEGET,1)=3
            IEAXIS(IEGET,2)=3
            IEAXIS(IEGET,3)=3
            IEAXIS(IEGET,4)=3
            IEAXIS(IEGET,5)=3
            IEAXIS(IEGET,6)=3
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Connection ',IEGET,' Transmitted power ph1 (W)'   
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Connection ',IEGET,' Transmitted power ph1 (VA)'
            WRITE(IELLAB(IEGET,3),'(A,I3,A)') 
     &'Connection ',IEGET,' Transmitted power ph2 (W)'   
            WRITE(IELLAB(IEGET,4),'(A,I3,A)') 
     &'Connection ',IEGET,' Transmitted power ph2 (VA)' 
            WRITE(IELLAB(IEGET,5),'(A,I3,A)') 
     &'Connection ',IEGET,' Transmitted power ph3 (W)'   
            WRITE(IELLAB(IEGET,6),'(A,I3,A)') 
     &'Connection ',IEGET,' Transmitted power ph3 (W)' 
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Con ',ICON,' Trn ph1'
            WRITE(IEULAB(IEGET,1),'(A)') 'W'
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'Con ',ICON,' Trn ph1'
            WRITE(IEULAB(IEGET,2),'(A)') 'VAR' 
            WRITE(IESLAB(IEGET,3),'(A,I3,A)') 'Con ',ICON,' Trn ph2'
            WRITE(IEULAB(IEGET,3),'(A)') 'W'
            WRITE(IESLAB(IEGET,4),'(A,I3,A)') 'Con ',ICON,' Trn ph2'
            WRITE(IEULAB(IEGET,4),'(A)') 'VAR' 
            WRITE(IESLAB(IEGET,5),'(A,I3,A)') 'Con ',ICON,' Trn ph3'
            WRITE(IEULAB(IEGET,5),'(A)') 'W'
            WRITE(IESLAB(IEGET,6),'(A,I3,A)') 'Con ',ICON,' Trn ph3'
            WRITE(IEULAB(IEGET,6),'(A)') 'VAR'

          ELSEIF(INO.EQ.8)THEN

C The connection loss
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=2
            IEGETNO(IEGET,3)=ICON
            IEGETNO(IEGET,4)=2
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=1
            IELINE(IEGET,2)=1
            IELINE(IEGET,3)=1
            IESYMB(IEGET,1)=10
            IESYMB(IEGET,2)=10
            IESYMB(IEGET,3)=10
            IEAXIS(IEGET,1)=3
            IEAXIS(IEGET,2)=3
            IEAXIS(IEGET,3)=3
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Connection ',IEGET,' Power loss ph1 (W)'   
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Connection ',IEGET,' Power loss ph2 (W)'   
            WRITE(IELLAB(IEGET,3),'(A,I3,A)') 
     &'Connection ',IEGET,' Power loss ph3 (W)'   
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Con ',ICON,' Loss ph1'
            WRITE(IEULAB(IEGET,1),'(A)') 'W'
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'Con ',ICON,' Loss ph2'
            WRITE(IEULAB(IEGET,2),'(A)') 'W'
            WRITE(IESLAB(IEGET,3),'(A,I3,A)') 'Con ',ICON,' Loss ph3'
            WRITE(IEULAB(IEGET,3),'(A)') 'W'

          ELSEIF(INO.EQ.11)THEN

C The connection power factor
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=2
            IEGETNO(IEGET,3)=ICON
            IEGETNO(IEGET,4)=3
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=-2
            IELINE(IEGET,2)=-2
            IELINE(IEGET,3)=-2
            IESYMB(IEGET,1)=11
            IESYMB(IEGET,2)=11
            IESYMB(IEGET,3)=11
            IEAXIS(IEGET,1)=4
            IEAXIS(IEGET,2)=4
            IEAXIS(IEGET,3)=4
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Connection ',IEGET,' Power factor ph1'   
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Connection ',IEGET,' Power factor ph2'   
            WRITE(IELLAB(IEGET,3),'(A,I3,A)') 
     &'Connection ',IEGET,' Power factor ph3'    
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Con ',ICON,' pf ph1'
            WRITE(IEULAB(IEGET,1),'(A)') '-'
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'Con ',ICON,' pf ph2'
            WRITE(IEULAB(IEGET,2),'(A)') '-'
            WRITE(IESLAB(IEGET,3),'(A,I3,A)') 'Con ',ICON,' pf ph3'
            WRITE(IEULAB(IEGET,3),'(A)') '-'
          ELSE
            IF(IEGET.GT.0)IEGET=IEGET-1
            GOTO 20
          ENDIF
  70    CONTINUE
      ELSEIF(IFOC.EQ.3) THEN

C Focus is on components.
        IF(INO.EQ.4)THEN
          CALL USRMSG('Select one or more components: ',' ','-')
          CALL ELPOWMLS(NPW,IPW)
          CALL USRMSG(' ',' ','-')
        ELSE
          INO=-4
          GOTO 20
        ENDIF
        DO 80 IP=1,NPW
          IPOWOC= IPW(IP)
          IF(IPOWOC.LT.1.OR.IPOWOC.GT.NPOWCOM) GOTO 80
          IEGET=IEGET+1

C Power-only component power flows
          IF(INO.EQ.4)THEN
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=3
            IEGETNO(IEGET,3)=IPOWOC
            IEGETNO(IEGET,4)=1
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=1
            IELINE(IEGET,2)=1
            IESYMB(IEGET,1)=13
            IESYMB(IEGET,2)=3
            IEAXIS(IEGET,1)=3
            IEAXIS(IEGET,2)=3
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Power-only comp ',IPOWOC,' Power flow (W) '   
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Power-only comp ',IPOWOC,' Power flow (VA) '        
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'POC ',IPOWOC,' P'
            WRITE(IEULAB(IEGET,1),'(A)') 'W'
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'POC ',IPOWOC,' Q'
            WRITE(IEULAB(IEGET,2),'(A)') 'VAR'
          ELSE
            IF(IEGET.GT.0)IEGET=IEGET-1
            GOTO 20
          ENDIF
  80    CONTINUE
      ELSEIF(IFOC.EQ.4) THEN
        IF(INO.EQ.4)THEN
          CALL USRMSG('Select one or more components: ',' ','-')
          CALL ELHYBMLS(NHY,IHY)
          CALL USRMSG(' ',' ','-')
        ELSE
          INO=-4
          GOTO 20
        ENDIF
        DO 90 IH=1,NHY
          IHYBCOM=IHY(IH)
          IF(IHYBCOM.LT.1.OR.IHYBCOM.GT.NHYBCOM) GOTO 90
          IEGET=IEGET+1

C Hybrid component power flows.
          IF(INO.EQ.4)THEN
            IEGETNO(IEGET,1)=IEGET  
            IEGETNO(IEGET,2)=4
            IEGETNO(IEGET,3)=IHYBCOM
            IEGETNO(IEGET,4)=1
            IEGETNO(IEGET,5)=1
            IELINE(IEGET,1)=1
            IELINE(IEGET,2)=1
            IESYMB(IEGET,1)=14
            IESYMB(IEGET,2)=4
            IEAXIS(IEGET,1)=3
            IEAXIS(IEGET,2)=3
            WRITE(IELLAB(IEGET,1),'(A,I3,A)') 
     &'Hybrid comp ',IHYBCOM,' Power flow (W) '   
            WRITE(IELLAB(IEGET,2),'(A,I3,A)') 
     &'Hybrid comp ',IHYBCOM,' Power flow (VA) '        
            WRITE(IESLAB(IEGET,1),'(A,I3,A)') 'Hyb ',IHYBCOM,' P'
            WRITE(IEULAB(IEGET,1),'(A)') 'W'
            WRITE(IESLAB(IEGET,2),'(A,I3,A)') 'Hyb ',IHYBCOM,' Q'
            WRITE(IEULAB(IEGET,2),'(A)') 'VAR'
          ELSE
            IF(IEGET.GT.0)IEGET=IEGET-1
            GOTO 20
          ENDIF
  90    CONTINUE
      ELSEIF(IFOC.EQ.5) THEN
        IF(INO.EQ.4.OR.INO.EQ.5.OR.INO.EQ.6.OR.INO.EQ.7
     &.OR.INO.EQ.10.OR.INO.EQ.11.OR.INO.EQ.12.OR.INO.EQ.15
     &.OR.INO.EQ.16.OR.INO.EQ.17)THEN
          IEGET=IEGET+1
        ELSE
          INO=-4
          GOTO 20
        ENDIF

C Network Load.
        IF(INO.EQ.4)THEN
          IEGETNO(IEGET,1)=IEGET  
          IEGETNO(IEGET,2)=5
          IEGETNO(IEGET,3)=99
          IEGETNO(IEGET,4)=2
          IEGETNO(IEGET,5)=1
          IEGETNO(IEGET,6)=-2
          IEGETNO(IEGET,7)=16
          IEGETNO(IEGET,8)=6
          IEGETNO(IEGET,9)=3
          IELINE(IEGET,1)=-2
          IELINE(IEGET,2)=-2
          IESYMB(IEGET,1)=16
          IESYMB(IEGET,2)=6
          IEAXIS(IEGET,1)=3
          IEAXIS(IEGET,2)=3
          WRITE(IELLAB(IEGET,1),'(A)') 
     &'Network load (W) '   
          WRITE(IELLAB(IEGET,2),'(A)') 
     &'Network load (VA) '        
          WRITE(IESLAB(IEGET,1),'(A)')'Net Load'
          WRITE(IEULAB(IEGET,1),'(A)') 'W'
          WRITE(IESLAB(IEGET,2),'(A)')'Net Load'
          WRITE(IEULAB(IEGET,2),'(A)') 'VAR'

C Network Generation.
        ELSEIF(INO.EQ.5)THEN
          IEGETNO(IEGET,1)=IEGET  
          IEGETNO(IEGET,2)=5
          IEGETNO(IEGET,3)=99
          IEGETNO(IEGET,4)=3
          IEGETNO(IEGET,5)=1
          IELINE(IEGET,1)=-2
          IELINE(IEGET,2)=-2
          IESYMB(IEGET,1)=17
          IESYMB(IEGET,2)=7
          IEAXIS(IEGET,1)=3
          IEAXIS(IEGET,2)=3
          WRITE(IELLAB(IEGET,1),'(A)') 
     &'Network generation (W) '   
          WRITE(IELLAB(IEGET,2),'(A)') 
     &'Network generation (VA) '        
          WRITE(IESLAB(IEGET,1),'(A)')'Net gen'
          WRITE(IEULAB(IEGET,1),'(A)') 'W'
          WRITE(IESLAB(IEGET,2),'(A)')'Net gen'
          WRITE(IEULAB(IEGET,2),'(A)') 'VAR'

C Network imported power
        ELSEIF(INO.EQ.6)THEN
          IEGETNO(IEGET,1)=IEGET  
          IEGETNO(IEGET,2)=5
          IEGETNO(IEGET,3)=99
          IEGETNO(IEGET,4)=1
          IEGETNO(IEGET,5)=1
          IELINE(IEGET,1)=-2
          IELINE(IEGET,2)=-2
          IESYMB(IEGET,1)=15
          IESYMB(IEGET,2)=5
          IEAXIS(IEGET,1)=3
          IEAXIS(IEGET,2)=3
          WRITE(IELLAB(IEGET,1),'(A)') 
     &'Network import (W) '   
          WRITE(IELLAB(IEGET,2),'(A)') 
     &'Network import (VA) '        
          WRITE(IESLAB(IEGET,1),'(A)')'Net imp'
          WRITE(IEULAB(IEGET,1),'(A)') 'W'
          WRITE(IESLAB(IEGET,2),'(A)')'Net imp'
          WRITE(IEULAB(IEGET,2),'(A)') 'VAR'

        ELSEIF(INO.EQ.7)THEN

C Network losses
          IEGETNO(IEGET,1)=IEGET  
          IEGETNO(IEGET,2)=5
          IEGETNO(IEGET,3)=99
          IEGETNO(IEGET,4)=8
          IEGETNO(IEGET,5)=1
          IELINE(IEGET,1)=-2
          IESYMB(IEGET,1)=16
          IEAXIS(IEGET,1)=3
          WRITE(IELLAB(IEGET,1),'(A)') 
     &'Network loss (W) '   
          WRITE(IESLAB(IEGET,1),'(A)')'Net loss'
          WRITE(IEULAB(IEGET,1),'(A)') 'W'
        ELSEIF(INO.EQ.10)THEN

C Network load power factor
          IEGETNO(IEGET,1)=IEGET  
          IEGETNO(IEGET,2)=5
          IEGETNO(IEGET,3)=99
          IEGETNO(IEGET,4)=7
          IEGETNO(IEGET,5)=1
          IELINE(IEGET,1)=-3
          IESYMB(IEGET,1)=17
          IEAXIS(IEGET,1)=4
          WRITE(IELLAB(IEGET,1),'(A)') 
     &'Network load pf '   
          WRITE(IESLAB(IEGET,1),'(A)')'Net L pf'
          WRITE(IEULAB(IEGET,1),'(A)') '-'
        ELSEIF(INO.EQ.11)THEN

C Network generation power factor
          IEGETNO(IEGET,1)=IEGET  
          IEGETNO(IEGET,2)=5
          IEGETNO(IEGET,3)=99
          IEGETNO(IEGET,4)=6
          IEGETNO(IEGET,5)=1
          IELINE(IEGET,1)=-3
          IESYMB(IEGET,1)=18
          IEAXIS(IEGET,1)=4
          WRITE(IELLAB(IEGET,1),'(A)') 
     &'Network generation pf '   
          WRITE(IESLAB(IEGET,1),'(A)')'Net G pf'
          WRITE(IEULAB(IEGET,1),'(A)') '-'
        ELSEIF(INO.EQ.12)THEN

C Network import power factor
          IEGETNO(IEGET,1)=IEGET  
          IEGETNO(IEGET,2)=5
          IEGETNO(IEGET,3)=99
          IEGETNO(IEGET,4)=5
          IEGETNO(IEGET,5)=1
          IELINE(IEGET,1)=-3
          IESYMB(IEGET,1)=19
          IEAXIS(IEGET,1)=4
          WRITE(IELLAB(IEGET,1),'(A)') 
     &'Network generation pf '   
          WRITE(IESLAB(IEGET,1),'(A)')'Net I pf'
          WRITE(IEULAB(IEGET,1),'(A)') '-'

        ELSEIF(INO.EQ.15)THEN

C Network Loading by phase
          IEGETNO(IEGET,1)=IEGET  
          IEGETNO(IEGET,2)=5
          IEGETNO(IEGET,3)=99
          IEGETNO(IEGET,4)=9
          IEGETNO(IEGET,5)=1
          IELINE(IEGET,1)=1
          IELINE(IEGET,2)=-2
          IELINE(IEGET,3)=1
          IELINE(IEGET,4)=-2
          IELINE(IEGET,5)=1
          IELINE(IEGET,6)=-2
          IESYMB(IEGET,1)=20
          IESYMB(IEGET,2)=10
          IESYMB(IEGET,3)=20
          IESYMB(IEGET,4)=10
          IESYMB(IEGET,5)=20
          IESYMB(IEGET,6)=10
          IEAXIS(IEGET,1)=3
          IEAXIS(IEGET,2)=3
          IEAXIS(IEGET,3)=3
          IEAXIS(IEGET,4)=3
          IEAXIS(IEGET,5)=3
          IEAXIS(IEGET,6)=3
          WRITE(IELLAB(IEGET,1),'(A)') 
     &'Network load phase 1 (W) '
          WRITE(IELLAB(IEGET,2),'(A)') 
     &'Network load phase 1 (VA) '   
          WRITE(IELLAB(IEGET,3),'(A)') 
     &'Network load phase 2 (W) ' 
          WRITE(IELLAB(IEGET,4),'(A)') 
     &'Network load phase 2 (VA) '   
          WRITE(IELLAB(IEGET,5),'(A)') 
     &'Network load phase 3 (W) ' 
          WRITE(IELLAB(IEGET,6),'(A)') 
     &'Network load phase 3 (VA) '   
          WRITE(IESLAB(IEGET,1),'(A)')'Net L ph 1'
          WRITE(IEULAB(IEGET,1),'(A)') 'W'
          WRITE(IESLAB(IEGET,2),'(A)')'Net L ph 1'
          WRITE(IEULAB(IEGET,2),'(A)') 'VAR'
          WRITE(IESLAB(IEGET,3),'(A)')'Net L ph 2'
          WRITE(IEULAB(IEGET,3),'(A)') 'W'
          WRITE(IESLAB(IEGET,4),'(A)')'Net L ph 2'
          WRITE(IEULAB(IEGET,4),'(A)') 'VAR'
          WRITE(IESLAB(IEGET,5),'(A)')'Net L ph 3'
          WRITE(IEULAB(IEGET,5),'(A)') 'W'
          WRITE(IESLAB(IEGET,6),'(A)')'Net L ph 3'
          WRITE(IEULAB(IEGET,6),'(A)') 'VAR'

        ELSEIF(INO.EQ.16)THEN

C Network phase generation
          IEGETNO(IEGET,1)=IEGET  
          IEGETNO(IEGET,2)=5
          IEGETNO(IEGET,3)=99
          IEGETNO(IEGET,4)=10
          IEGETNO(IEGET,5)=1
          IELINE(IEGET,1)=1
          IELINE(IEGET,2)=-2
          IELINE(IEGET,3)=1
          IELINE(IEGET,4)=-2
          IELINE(IEGET,5)=1
          IELINE(IEGET,6)=-2
          IESYMB(IEGET,1)=21
          IESYMB(IEGET,2)=8
          IESYMB(IEGET,3)=21
          IESYMB(IEGET,4)=8
          IESYMB(IEGET,5)=21
          IESYMB(IEGET,6)=8
          IEAXIS(IEGET,1)=3
          IEAXIS(IEGET,2)=3
          IEAXIS(IEGET,3)=3
          IEAXIS(IEGET,4)=3
          IEAXIS(IEGET,5)=3
          IEAXIS(IEGET,6)=3
          WRITE(IELLAB(IEGET,1),'(A)') 
     &'Network generation phase 1 (W) '   
         WRITE(IELLAB(IEGET,2),'(A)') 
     &'Network generation phase 1 (VA) '   
          WRITE(IELLAB(IEGET,3),'(A)') 
     &'Network generation phase 2 (W) ' 
         WRITE(IELLAB(IEGET,4),'(A)') 
     &'Network generation phase 2 (VA) '   
          WRITE(IELLAB(IEGET,5),'(A)') 
     &'Network generation phase 3 (W) ' 
          WRITE(IELLAB(IEGET,6),'(A)') 
     &'Network generation phase 3 (VA) '   
          WRITE(IESLAB(IEGET,1),'(A)')'Net G ph1'
          WRITE(IEULAB(IEGET,1),'(A)') 'W'
          WRITE(IESLAB(IEGET,2),'(A)')'Net G ph1'
          WRITE(IEULAB(IEGET,2),'(A)') 'VAR'
          WRITE(IESLAB(IEGET,3),'(A)')'Net G ph2'
          WRITE(IEULAB(IEGET,3),'(A)') 'W'
          WRITE(IESLAB(IEGET,4),'(A)')'Net G ph2'
          WRITE(IEULAB(IEGET,4),'(A)') 'VAR'
          WRITE(IESLAB(IEGET,5),'(A)')'Net G ph3'
          WRITE(IEULAB(IEGET,5),'(A)') 'W'
          WRITE(IESLAB(IEGET,6),'(A)')'Net G ph3'
          WRITE(IEULAB(IEGET,6),'(A)') 'VAR'

        ELSEIF(INO.EQ.17)THEN

C Network phase import
          IEGETNO(IEGET,1)=IEGET  
          IEGETNO(IEGET,2)=5
          IEGETNO(IEGET,3)=99
          IEGETNO(IEGET,4)=11
          IEGETNO(IEGET,5)=1
          IELINE(IEGET,1)=1
          IELINE(IEGET,2)=-2
          IELINE(IEGET,3)=1
          IELINE(IEGET,4)=-2
          IELINE(IEGET,5)=1
          IELINE(IEGET,6)=-2
          IESYMB(IEGET,1)=22
          IESYMB(IEGET,2)=9
          IESYMB(IEGET,3)=22
          IESYMB(IEGET,4)=9
          IESYMB(IEGET,5)=22
          IESYMB(IEGET,6)=9
          IEAXIS(IEGET,1)=3
          IEAXIS(IEGET,2)=3
          IEAXIS(IEGET,3)=3
          IEAXIS(IEGET,4)=3
          IEAXIS(IEGET,5)=3
          IEAXIS(IEGET,6)=3
          WRITE(IELLAB(IEGET,1),'(A)') 
     &'Network import phase 1 (W) ' 
          WRITE(IELLAB(IEGET,2),'(A)') 
     &'Network import phase 1 (VA) '   
          WRITE(IELLAB(IEGET,3),'(A)') 
     &'Network import phase 2 (W) ' 
          WRITE(IELLAB(IEGET,4),'(A)') 
     &'Network import phase 2 (VA) '   
          WRITE(IELLAB(IEGET,5),'(A)') 
     &'Network import phase 3 (W) ' 
          WRITE(IELLAB(IEGET,6),'(A)') 
     &'Network import phase 3 (VA) '   
          WRITE(IESLAB(IEGET,1),'(A)')'Net Im ph1'
          WRITE(IEULAB(IEGET,1),'(A)') 'W'
          WRITE(IESLAB(IEGET,2),'(A)')'Net Im ph1'
          WRITE(IEULAB(IEGET,2),'(A)') 'VAR'
          WRITE(IESLAB(IEGET,3),'(A)')'Net Im ph2'
          WRITE(IEULAB(IEGET,3),'(A)') 'W'
          WRITE(IESLAB(IEGET,4),'(A)')'Net Im ph2'
          WRITE(IEULAB(IEGET,4),'(A)') 'VAR'
          WRITE(IESLAB(IEGET,5),'(A)')'Net Im ph3'
          WRITE(IEULAB(IEGET,5),'(A)') 'W'
          WRITE(IESLAB(IEGET,6),'(A)')'Net Im ph3'
          WRITE(IEULAB(IEGET,6),'(A)') 'VAR'
        ELSE
          IF(IEGET.GT.0)IEGET=IEGET-1
          GOTO 20
        ENDIF
      ENDIF

      GOTO 20
        
      END

C ***********************************************************************
C Subroutine ENETHASH calculates the relative position of a data item 
C within the electrical results library. The routine returns the record
C number, number of returned values and and the postion(s) of the 
C required value(s). The routine is called with the time step ITS an 
C entity type - IEGETNO(*,2) and an entity number - IEGETNO(*,3)  
C and an enitity number id. - IEGETNO(*,4)
C These are as follows:
C Entity     No.    i.d.           Description                    No. Vals
C   1         ?       1            Node No. ? Voltage                 2
C   1         ?       2            Node No. ? Load                    2
C   1         ?       3            Node No. ? Generation              2
C   1         ?       4            Node No. ? Transmission            2
C   1         ?       5            Node No. ? Import                  2
C   1         ?       6            Node No. ? Load  pf                2
C   1         ?       7            Node No. ? Generation pf           2
C   1         ?       8            Node No. ? Import pf               2
C   2         ?       1            Connection No. ? Transmission      6
C   2         ?       2            Connection No. ? Loss              3
C   2         ?       3            Connection No. ? Power factor      3
C   2         ?       4            Connection No. ? Current           6
C   3         ?       1            Power only component No. ? Power   2
C   4         ?       1            Hybrid (HVAC,PV,etc.) No. ? Power  2
C   5         -       1            Network imported power             2      
C   5         -       2            Network load                       2
C   5         -       3            Network generation                 2
C   5         -       4            Network transmission (not used)    2
C   5         -       5            Network import p.f.                1
C   5         -       6            Network load p.f.                  1
C   5         -       7            Network genr p.f.                  1
C   5         -       8            Network  loss                      1
C   5         -       9            Network  phase loading             6
C   5         -       10           Network  phase generation          6
C   5         -       11           Network  phase import              6
C
C NPOS - gives the number of returned pointers
C IPOS - points to the entry (column number) in a specific record
C ***********************************************************************
      SUBROUTINE ENETHASH(ITS,IENTY,IENO,IENOID,IREC,NPOS,IPOS)

#include "power.h"

C Electrical network results library details.
      COMMON/ENELIBRC/IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,NTSREC,NHREC,
     &NPREC

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)


C Dimension array of data element pointers returned by the routine
      DIMENSION IPOS(MERPOS) 

      INTEGER IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,
     &NTSREC,NHREC,NPREC,NENOD,ENODNO,ENODPHTYP,ENODPH,
     &ENODTYP,NECON,ECONO,CONPH,CONPHTYP,CCNO,SENOD,EENOD
     
      REAL ENODBASEV,ENODBASEP

C Initialise the data pointer array arrays
      NPOS=0
      DO 10 I=1,MERPOS
        IPOS(I)=0
  10  CONTINUE
           
      IF(IENTY.EQ.1)THEN

C Recover nodal data
        IREC=IELIBHL+NTSREC*(ITS-1)+IENO
        IF(IENOID.EQ.1)THEN           
          NPOS=2
          IPOS(1)=1
          IPOS(2)=2          
        ELSEIF(IENOID.EQ.2)THEN
          NPOS=2
          IPOS(1)=4
          IPOS(2)=5
        ELSEIF(IENOID.EQ.3)THEN
          NPOS=2
          IPOS(1)=6
          IPOS(2)=7
        ELSEIF(IENOID.EQ.4)THEN
          NPOS=2
          IPOS(1)=8
          IPOS(2)=9
        ELSEIF(IENOID.EQ.5)THEN
          NPOS=2
          IPOS(1)=10
          IPOS(2)=11
        ELSEIF(IENOID.EQ.6)THEN
          NPOS=1
          IPOS(1)=12
        ELSEIF(IENOID.EQ.7)THEN
          NPOS=1
          IPOS(1)=13
        ELSEIF(IENOID.EQ.8)THEN
          NPOS=1
          IPOS(1)=14
        ENDIF
      ELSEIF(IENTY.EQ.2)THEN

C Recover connection data
        IREC=IELIBHL+NTSREC*(ITS-1)+NENOD+IENO
        IF(IENOID.EQ.1)THEN
          NPOS=6
          IPOS(1)=1
          IPOS(2)=2
          IPOS(3)=7
          IPOS(4)=8
          IPOS(5)=13
          IPOS(6)=14
        ELSEIF(IENOID.EQ.2)THEN
          NPOS=3
          IPOS(1)=3
          IPOS(2)=9
          IPOS(3)=15
        ELSEIF(IENOID.EQ.3)THEN
          NPOS=3
          IPOS(1)=4
          IPOS(2)=10
          IPOS(3)=16
        ELSEIF(IENOID.EQ.4)THEN
          NPOS=6
          IPOS(1)=5
          IPOS(2)=6
          IPOS(3)=11
          IPOS(4)=12
          IPOS(5)=17
          IPOS(6)=18
        ENDIF
      ELSEIF(IENTY.EQ.3)THEN

C Recover power-only component data.
        NPOS=2
        IF((IENO*2).LE.IELIBW)THEN
          IPR=1
          IPOS(1)=IENO*2-1
          IPOS(2)=IENO*2
        ELSE
          IF(MOD(IENO*2,IELIBW).EQ.0)THEN
            IPR=INT(FLOAT(IENO*2)/FLOAT(IELIBW))
          ELSE
            IPR=INT(FLOAT(IENO*2)/FLOAT(IELIBW))+1
          ENDIF
          IPOS(1)=IENO*2-((IPR-1)*IELIBW)-1
          IPOS(2)=IENO*2-((IPR-1)*IELIBW)    
        ENDIF              
        IREC=IELIBHL+NTSREC*(ITS-1)+NENOD+NECON+IPR
      ELSEIF(IENTY.EQ.4)THEN

C Recover hybrid component data positions.
        NPOS=2
        IF((IENO*2).LE.IELIBW)THEN
          IHR=1
          IPOS(1)=IENO*2-1
          IPOS(2)=IENO*2
        ELSE
          IF(MOD(IENO*2,IELIBW).EQ.0)THEN
            IHR=INT(FLOAT(IENO*2)/FLOAT(IELIBW))
          ELSE
            IHR=INT(FLOAT(IENO*2)/FLOAT(IELIBW))+1
          ENDIF
          IPOS(1)=IENO*2-((IHR-1)*IELIBW)-1
          IPOS(2)=IENO*2-((IHR-1)*IELIBW)    
        ENDIF  
        IREC=IELIBHL+NTSREC*(ITS-1)+NENOD+NECON+NPREC+IHR
      ELSEIF(IENTY.EQ.5)THEN

C Recover global network data positions.
        IF(IENOID.LT.9) THEN
          IREC=IELIBHL+NTSREC*(ITS-1)+NENOD+NECON+NPREC+NHREC+1
        ELSE
          IREC=IELIBHL+NTSREC*(ITS-1)+NENOD+NECON+NPREC+NHREC+2
        ENDIF
        IF(IENOID.EQ.1)THEN
          NPOS=2
          IPOS(1)=1
          IPOS(2)=2         
        ELSEIF(IENOID.EQ.2)THEN
          NPOS=2
          IPOS(1)=3
          IPOS(2)=4  
        ELSEIF(IENOID.EQ.3)THEN 
          NPOS=2
          IPOS(1)=5
          IPOS(2)=6         
        ELSEIF(IENOID.EQ.4)THEN 
          NPOS=2
          IPOS(1)=7
          IPOS(2)=8       
        ELSEIF(IENOID.EQ.5)THEN 
          NPOS=1
          IPOS(1)=9
        ELSEIF(IENOID.EQ.6)THEN 
          NPOS=1
          IPOS(1)=10  
        ELSEIF(IENOID.EQ.7)THEN 
          NPOS=1
          IPOS(1)=11 
        ELSEIF(IENOID.EQ.8)THEN 
          NPOS=1
          IPOS(1)=12  
        ELSEIF(IENOID.EQ.9)THEN 
          NPOS=6
          IPOS(1)=1  
          IPOS(2)=2 
          IPOS(3)=7  
          IPOS(4)=8   
          IPOS(5)=13
          IPOS(6)=14
        ELSEIF(IENOID.EQ.10)THEN 
          NPOS=6
          IPOS(1)=3  
          IPOS(2)=4 
          IPOS(3)=9  
          IPOS(4)=10   
          IPOS(5)=15
          IPOS(6)=16  
        ELSEIF(IENOID.EQ.11)THEN 
          NPOS=6
          IPOS(1)=5  
          IPOS(2)=6 
          IPOS(3)=11  
          IPOS(4)=12   
          IPOS(5)=17
          IPOS(6)=18   
        ENDIF     
      ENDIF
      
      RETURN
      END

C ************************************************************************************
C Subroutine ENETPROC retrieves data for a user defined period from the electrical 
C results databases. The routine calls ENETGET, which reads the data from the 
C library into the EVAL1(*) array (this is the electrical equivalent of the
C VAL1 array on the building side), but holds only one time steps of data. 
C
C Based on the calling mode, the routine then displays the data in a graph
C in a file or using statistics. The calling modes are as follows:
C G - Grpahical mode
C S - Stats mode 
C T - Tablular mode 
C ? - Other modes added as required
C
C ************************************************************************************
      SUBROUTINE ENETPROC(MODE)

      CHARACTER MODE*1

C Initialise the data storage array labels and units. 
      IINI=1
      CALL ENETGET(IINI,'I')

C Initialisation: based on the calling routine and set up the basic user 
C feedback information. 
      IF( MODE.EQ.'G')THEN

C Call the graphing routine to scale and initialise axes. 
        CALL ENETGRPH('I')
      ELSEIF( MODE.EQ.'S')THEN
C        CALL ENETSTAT('I')     
      ELSEIF( MODE.EQ.'T')THEN
         CALL ENETTBL('I')
      ENDIF

C Update: data is read from the results library and, based on the calling
C routine is processed accordingly e.g. added to a graph or table. 
      IF( MODE.EQ.'G')THEN
        CALL ENETGRPH('U')
      ELSEIF( MODE.EQ.'S')THEN
C        CALL ENETSTAT('U')     
      ELSEIF( MODE.EQ.'T')THEN
         CALL ENETTBL('U')
      ENDIF
     
C At the end of the defined recovery period call stats, quality routines
C a final time to display processed data data. 
      IF(MODE.EQ.'S')THEN
C         IV=999
C        CALL ENETSTAT('D',IV)
      ELSEIF(MODE.EQ.'Q')THEN
C        CALL ENETPQAL('D',IV)
      ELSEIF(MODE.EQ.'V')THEN
C          CALL ENETPIPV('D',IV)
      ENDIF
      
      RETURN
      END

C **************************************************************************************
C Subroutine ENETGET retrieves the data from the electrical results library and
C reads it into the EVAL1 array. The routine is called with the a time step (passed
C to ENETHASH) and a mode.
C 
C The routine operates in two modes:
C Mode 'I' - The data labels and units associated with picked items are assigned to
C            the EVLABS and EVLABU array.
C Mode '-' - Assigns data read from the library to the EVAL1 array. 

C Variables introduced in this routine are:
C EVAL1(*)  - 1-D arrary holding time series data from the electrical library.
C EVAL2(*)  - 1-D  array, but holding period averaged values. 
C EVLABS(*) - 1-D array holding a matching set of labels for EVAL1 and EVAL2
C EVLABU(*) - 1-D array holding a matching set of units for EVAL1 and EVAL2
C EVLINE(*) - 1-D array holding a matching set of line types for EVAL1 and EVAL2
C EVMARK(*) - 1-D array holding a matching set of line marks for EVAL1 and EVAL2
C EVAXIS(*) - 1-D array holding the appropriate avis for EVAL1 and EVAL2
C **************************************************************************************

      SUBROUTINE ENETGET(IT,MODE)

#include "power.h"

C Electrical network results library details.
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/ENELIBRC/IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,NTSREC,NHREC,
     &NPREC

C Array of picked items
      COMMON/ENETGETN/IEGET,IEGETNO(MENOD,9)
      COMMON/ENETGETS/IELLAB(MENOD,MERPOS),IESLAB(MENOD,MERPOS),
     &IEULAB(MENOD,MERPOS)
      COMMON/ENETGETM/IEAXIS(MENOD,MERPOS),IESYMB(MENOD,MERPOS),
     &IELINE(MENOD,MERPOS)

C Array of recovered data and associated infomation
      COMMON/ERECOV/NVAL1,NVAL2,EVAL1(MERAWD),EVAL2(MERAWD)
      COMMON/ERECOVL/EVLABS(MERAWD),EVLABU(MERAWD)
      COMMON/ERECOVD/EVAXIS(MERAWD),EVMARK(MERAWD),EVLINE(MERAWD)

      DIMENSION IPOS(MERPOS),VAL(MELIBW)
      
      REAL EVAL1,EVAL2

      INTEGER IEGET,NVAL1,NVAL2,IEGETNO,IENTY,IENO,IENOID,EVAXIS,
     &EVMARK,EVLINE

      CHARACTER IELLAB*64,IESLAB*16,EVLABS*16,IEULAB*4,EVLABU*4,MODE*1
      
      IF(MODE.EQ.'I') THEN

C Initialise the data array
        IVAL=0
        DO 10 I=1,MERAWD
          EVAL1(I)=0.0
          EVAL2(I)=0.0      
          EVLABS(I)=' '        
          EVLABU(I)=' ' 
          EVLINE(I)=0 
          EVMARK(I)=0
          EVAXIS(I)=0
  10    CONTINUE

C Set up the recovery array
        IT=1
        DO 20 J=1,IEGET
          IENTY=IEGETNO(J,2)
          IENO=IEGETNO(J,3)
          IENOID=IEGETNO(J,4)
          CALL ENETHASH(IT,IENTY,IENO,IENOID,IREC,NPOS,IPOS)

C Firstly check that the EVAL1 array bounds are not exceeded. Then assign labels and
C information for graphing the data.  
          IF((IVAL+NPOS).GT.MERAWD) RETURN
          DO 30 K=1,NPOS
            IVAL=IVAL+1
            EVLABS(IVAL)=IESLAB(J,K)
            EVLABU(IVAL)=IEULAB(J,K)
            EVLINE(IVAL)=IELINE(J,K)
            EVMARK(IVAL)=IESYMB(J,K)
            EVAXIS(IVAL)=IEAXIS(J,K)
  30      CONTINUE                    
  20    CONTINUE
        NVAL1=IVAL
        NVAL2=IVAL

      ELSE

C Recover the data from the library.
        IVAL=0
        DO 40 J=1,IEGET
          IENTY=IEGETNO(J,2)
          IENO=IEGETNO(J,3)
          IENOID=IEGETNO(J,4)
          CALL ENETHASH(IT,IENTY,IENO,IENOID,IREC,NPOS,IPOS)

C Check the array bounds has been exceeded.
          IF((IVAL+NPOS).GT.MERAWD) RETURN

C Read the record in the library.
          READ(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)(VAL(I),I=1,IELIBW)

C Write the data items to the EVAL1 array. 
          DO 50 K=1,NPOS
            IVAL=IVAL+1
            EVAL1(IVAL)=VAL(IPOS(K))
  50      CONTINUE                    
  40    CONTINUE

      ENDIF
      RETURN

999   CALL EDISP(IUOUT,'ERROR reading electrical results file')
      RETURN
      END

