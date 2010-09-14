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

C Mass flow results analysis menu routines.
C Selections are made and recorded for display.
C The structure is similar to the GOMENU structure in moget.F

C IMFGETNO contains all the information needed for specifying which data to recover
C IMFGETNO(*,1): The IMFGET number - metric identifier
C IMFGETNO(*,2): The chosen node/ connection (meaning assumed from IMFGET) in the
C                case of climate data it gets used to set graphing colour.
C IMFGETNO(*,3): Source node (for ac/h rate calcs, differences and energy?)
C IMFGETNO(*,4): Pointer to list of nodes or connections.  In case of individual
C                connection: -1=-ive flow; 0=net flow; +1=+ive flow
C IMFGETNO(*,5): Line type
C IMFGETNO(*,6): Symbol index
C IMFGETNO(*,7): Associated axis (1=temperature, 2=pressure, 3=mass flow rate
C                4=misc, 5=concentration g/kg)

C IMFLIST - connection list:
C   IMFLIST(LISTID,1) = number of connections listed
C   IMFLIST(LISTID,2->) = connection id

C IMFGET =  1 Node pressure
C IMFGET =  2 Node temperature
C IMFGET =  3 Connection mass flow rate
C IMFGET =  4 Connection volume flow rate
C IMFGET =  5 Connection air change rate
C IMFGET =  6 Connection velocity
C IMFGET =  7 Connection pressure difference
C IMFGET =  8 Connection stack pressure
C IMFGET =  9 Ambient temperature
C IMFGET = 10 Ambient wind speed
C IMFGET = 11 Ambient wind direction
C IMFGET = 12 Ambient RH
C IMFGET = 13 Energy associated with a flow path (given ref node, test node, connection).
C IMFGET = 14 Contaminant concentration

C ******************** MOFLOWSU ********************
C MOFLOWSU clears commons/ sets default values.

      SUBROUTINE MOFLOWSU
#include "net_flow.h"

      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,7)
      COMMON/MFLABEL/SMFLABEL(MFRS),GMFLABEL(MFRS),GLMFLABEL(MFRS)

      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

      character SMFLABEL*32,GMFLABEL*20,GLMFLABEL*32

      integer YAXSET, ADDLIN

C Reset axis max and min values to 1.0 and 0.0 resp.
C YAXSET: Axis max min set =1 automatically scalled, 2= user set.
      do 8 I=1,5
        YMAX(I)=1.0
        YMIN(I)=0.0
        YAXSET(I)=0
 8    continue

C Set add profile label ADDLIN=0 i.e. no add
      ADDLIN=0

C Reset to no choices and current result set.
      NMFGET=0
      do 10 I=1,MFRS
        IMFGETNO(I,1)=0
        IMFGETNO(I,2)=0
        IMFGETNO(I,3)=0
        IMFGETNO(I,4)=0
        IMFGETNO(I,5)=0
        IMFGETNO(I,6)=0
        IMFGETNO(I,7)=0
        SMFLABEL(I)='  '
        GMFLABEL(I)='  '
        GLMFLABEL(I)='  '
 10   continue
      ialstused=0

      return
      end

C ************************ MFMENU ************************
C MFMENU The choices are put into the array IMFGETNO in the
C columns listed above.

      SUBROUTINE MFMENU
#include "building.h"
#include "geometry.h"
#include "net_flow.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      common/recov01/pifltog,lpifl

      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,7)
      COMMON/MFPK2/NMFLST,IMFLIST(MFCNLST,MFCNEL+1)
      COMMON/MFLABEL/SMFLABEL(MFRS),GMFLABEL(MFRS),GLMFLABEL(MFRS)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      common/getmenu/menutype,igetind(65),igetflux(65)

C Optional title for graph and up to 5 other labels and their positions.
      common/grextras/graftitle,grlbl(5),ngrlbl,lblpx(5),lblpy(5)

C Comfort parameters and hash table.
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset,ioucpz(MCOM,72)

C iocpzone is the zone to get occupancy pattern from for use by
C air flow reporting.
      common/mfocup/iocpzone
      common/loadlabel/lodlabel(mcom,7)

      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN

      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      DIMENSION IVALS(MCOM),zn(MCOM)
      CHARACTER ITEM(35)*23, MTITLE*22, values*20
      character H*72,xfile*144,tg*1,delim*1
      character SMFLABEL*32,GMFLABEL*20,GLMFLABEL*32,SUFFIX*6
      CHARACTER outs*124,pifltog*4,lpifl*72
      CHARACTER NDNAM*12,PROMPT1*72,PROMPT2*72,CONTMNAM*12
      character graftitle*64,grlbl*24,t64*64,t24*24
      character ll1*8,ll2*8,ll3*8,lodlabel*6
      character zn*12

      DIMENSION ICPK(MCNN),INPK(MNOD),INPKT(MNOD),INPKT2(MNOD)

      integer YAXSET, ADDLIN
      integer lcname  ! length of contaminate string
      integer IOT,IWM  ! for radio buttons
      integer NITMS,INO ! max items and current menu item

      logical CLIST,NLIST,CNLST,CRLST,OKL,dok,CTLST
#ifdef OSI
      integer iid1,iid2
#else
      integer*8 iid1,iid2
#endif

C The logical variables are required so as user is asked for additional
C information about the choice of metric if necessary.
C  CLIST - connection list
C  NLIST - node list
C  CNLST - connection node list to restrict choice (e.g. all flows through node x)
C  CRLST - reference node list for connection information (e.g. volume for ac/h)
C  CTLST - contaminant list
 1    CLIST=.FALSE.
      NLIST=.FALSE.
      CNLST=.FALSE.
      CRLST=.FALSE.
      CTLST=.FALSE.

C MNGET is the maximum number of metrics allowed to be chosen from the menu.
      if (MENUTYPE.eq.1) then
        return
      elseif (MENUTYPE.eq.2) then
        MTITLE='Frequency histogram'
        MNGET=1
      elseif (MENUTYPE.eq.3) then
        MTITLE='Time series plot'
        MNGET=MFRS-1
      elseif (MENUTYPE.eq.4) then
        MTITLE='Performance metrics'
        MNGET=13
      elseif (MENUTYPE.eq.5) then
        MTITLE='Summary statistics'
        MNGET=1
      elseif (MENUTYPE.eq.6) then
        MTITLE='Sensitivity'
        MNGET=1
      elseif (MENUTYPE.eq.7) then
        MTITLE='Hrs above query point'
        MNGET=1
      elseif (MENUTYPE.eq.8) then
        MTITLE='Hrs below query point'
        MNGET=1
      else
        MNGET=1
        MTITLE='  '
      endif

C Setup menu items.
      ITEM(1) ='2 result set          '
      ITEM(2) ='3 display period      '
      if(MENUTYPE.eq.1)then
       ITEM(3)='4 output style        '
      elseif(MENUTYPE.eq.2)then
       ITEM(3)='4 output >> histogram '
      elseif(MENUTYPE.eq.3)then
       ITEM(3)='4 output >> graph     '
      elseif(MENUTYPE.eq.4)then
       ITEM(3)='4 output >> tabular   '
      elseif(MENUTYPE.eq.5)then
       ITEM(3)='4 output >> statistics'
      elseif(MENUTYPE.eq.6)then
       ITEM(3)='4 output: sensitivity '
      elseif(MENUTYPE.eq.7)then
       ITEM(3)='4 output >> hours over'
      elseif(MENUTYPE.eq.8)then
       ITEM(3)='4 output >> hrs below '
      endif
      ITEM(4) ='  ------------------  '
      ITEM(5) ='a  ambient temperature'
      ITEM(6) ='b  wind speed         '
      ITEM(7) ='c  wind direction     '
      ITEM(8) ='d  ambient RH         '
      ITEM(9) ='e  press @ node       '
      ITEM(10)='f  press diff @ conn  '
      ITEM(11)='g  stack pres @ conn  '
      ITEM(12)='h  node temperature   '
      ITEM(13)='i  mass flow rate     '
      ITEM(14)='j  volume flow rate   '
      ITEM(15)='k  air changes        '
      ITEM(16)='l  velocity @ conn    '
      ITEM(17)='m  contaminant @ node '
      ITEM(18)='n  watts assoc w/ flow'
      ITEM(19)='  ------------------  '
      if(NMFGET.eq.0)then
        ITEM(20)='                      '
      else
        ITEM(20)='q edit selections     '
      endif
      if(ixopen.eq.1)then
        ITEM(21)='> display to >> file  '
      elseif(ixopen.eq.0)then
        ITEM(21)='> display to >> screen'
      endif
      if(IROC.eq.0)then
        ITEM(22)='& data: as values     '
      elseif(IROC.eq.1)then
        ITEM(22)='& data: rate of change'
      endif
      if (MENUTYPE.eq.3) then

C If a time series plot clear selections otherwise show state of occupancy filter.
        ITEM(23)='/ clear all selections'
      else
        if(iocupf.eq.0)then
          ITEM(23)='+ filter >> none      '
        else
          ITEM(23)='+ filter >> occup only'
        endif
      endif
      if (MENUTYPE.eq.3) then

C If plot indicate scale, otherwise toggle time format.
        ITEM(24)='= set axis scale'
      else
        if(IHFLAG.eq.0)then
          ITEM(24)='* time >> 10h30   '
        elseif(IHFLAG.eq.1)then
          ITEM(24)='* time >> 0.4375  '
        endif
      endif
      if (MENUTYPE.eq.3) then

C If plot show add profile, otherwise toggle delimiter state.
        ITEM(25)='+ add another profile'
      else
        if(delim.eq.'-')then
          ITEM(25)='^ delim >> normal '
        elseif(delim.eq.'T')then
          ITEM(25)='^ delim >> TAB    '
        elseif(delim.eq.'C')then
          ITEM(25)='^ delim >> comma  '
        elseif(delim.eq.'S')then
          ITEM(25)='^ delim >> space  '
        elseif(delim.eq.'X')then
          ITEM(25)='^ delim >> tagged      '
        endif
      endif
      if(MENUTYPE.eq.3) then

C If plot give graph command and labels command, otherwise list command.
        ITEM(26)='@ labels        '
        ITEM(27)='! draw graph    '
        nl=2
      elseif(MENUTYPE.eq.4)then
        if(ILFLAG.eq.0)then
          ITEM(26)='@ labels>>short'
        elseif(ILFLAG.eq.1)then
          ITEM(26)='@ labels>>long '
        endif
        ITEM(27)='! list data    '
        nl=2
      else
        nl=0
      endif
      ITEM(26+nl)='? help           '
      ITEM(27+nl)='- exit           '

      NITMS=27+nl
      INO=-2

C Instanciate h() strings for this menu.
      H(1) ='  '
      write(H(1),'(10x,a)') MTITLE(1:lnblnk(MTITLE))
      H(2) ='  '
      H(3) ='Select the required for output.  Some metrics require'
      H(4) ='extra information e.g. zone number, surface number. '
      H(5) ='  '
      write (H(6),'(a,i2,a)')'You have made ',NMFGET,
     &  ' selections, therefore you have a'
      NLEFT=(MNGET-NMFGET)
      write(H(7),'(a,i2,a)')'maximum of ',NLEFT,' selections left.'
      H(8) ='  '
      H(9) ='Display can be redirected to a file; be carefull not '
      H(10)='  to confuse graphical and text output.  '
      H(11)='Filtering (not yet enabled for flow reporting) is based'
      H(12)='on the definition of casual gains (typically occupancy).'
      H(13)='Time can be displayed as hours and minutes (hh,mm) or'
      H(14)='  as a decimal (Day of year.Fraction of day).'
      H(15)='Delimiters between tabular output can be changed; '
      H(16)='  useful when exporting data to 3rd party software.'
      H(17)='Data can be displayed as raw values (rate of '
      H(18)='  change of the value is not yet implemented). '

      CALL EMENU(MTITLE,ITEM,NITMS,INO)

C Check for ADDLIN in time series plot - must choose metric if ADDLIN=1.
      if (MENUTYPE.eq.3.and.ADDLIN.eq.1) then
        if (INO.lt.5.or.INO.gt.19) goto 1
      endif

      IF(INO.EQ.1)then
        CALL MORESS
        INO=-1
      elseif(INO.EQ.2)then
        CALL MOOPER
        INO=-1
      elseif(INO.EQ.3)then
        CALL MOSTYLE
      elseif(INO.EQ.5)then

C Ambient temperature:IMFGET = 9
C Note: overload IMFGETNO(NMFGET,2) so that climate values can
C each have a different line colour.
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=9
        IMFGETNO(NMFGET,2)=9
        IMFGETNO(NMFGET,5)=-302
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=1
        GMFLABEL(NMFGET)='Ambient db Tmp'
        GLMFLABEL(NMFGET)='Ambient dry bulb temperature'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Ambient db temperature (degC)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Ambient db temp change (degC/hr)'
        endif

C If timestep listing go back to see if any other data should be included.
        if(MENUTYPE.eq.4) goto 1
      elseif(INO.EQ.6)then

C Wind speed: IMFGET = 10
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=10
        IMFGETNO(NMFGET,2)=10
        IMFGETNO(NMFGET,5)=-203
        IMFGETNO(NMFGET,6)=6
        IMFGETNO(NMFGET,7)=4
        GMFLABEL(NMFGET)='Wind speed'
        GLMFLABEL(NMFGET)='Wind speed'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Wind speed (m/sec)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Wind speed change (m/sec/hr)'
        endif

C If timestep listing go back to see if any other data should be included.
        if(MENUTYPE.eq.4) goto 1
      elseif(INO.EQ.7)then

C Wind direction: IMFGET = 11
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=11
        IMFGETNO(NMFGET,2)=11
        IMFGETNO(NMFGET,5)=-203
        IMFGETNO(NMFGET,6)=4
        IMFGETNO(NMFGET,7)=4
        GMFLABEL(NMFGET)='Wind dir'
        GLMFLABEL(NMFGET)='Wind direction'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Wind direction (deg)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Wind direction change (deg/hr)'
        endif

C If timestep listing go back to see if any other data should be included.
        if(MENUTYPE.eq.4) goto 1
      elseif(INO.EQ.8)then

C Ambient RH: IMFGET = 12
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=12
        IMFGETNO(NMFGET,2)=12
        IMFGETNO(NMFGET,5)=-202
        IMFGETNO(NMFGET,6)=7
        IMFGETNO(NMFGET,7)=4
        GMFLABEL(NMFGET)='Ambient RH'
        GLMFLABEL(NMFGET)='Ambient relative humidity'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Ambient RH (%)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Ambient RH change (%/hr)'
        endif

C If timestep listing go back to see if any other data should be included.
        if(MENUTYPE.eq.4) goto 1
      elseif(INO.EQ.9)then

C Presure at node. IMFGET = 1
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=1
        IMFGETNO(NMFGET,5)=-102
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=2
        SUFFIX=' Pres'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Node pressure (Pa)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Node pressure change (Pa/hr)'
        endif
        NLIST=.true.
      elseif(INO.EQ.10)then

C Connection pressure difference IMFGET = 7
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=7
        IMFGETNO(NMFGET,5)=-102
        IMFGETNO(NMFGET,6)=2
        IMFGETNO(NMFGET,7)=2
        SUFFIX=' Del P'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Conn delta P (Pa)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Conn delta P change (Pa/hr)'
        endif
        CLIST=.true.
      elseif(INO.EQ.11)then

C Stack pressure: IMFGET = 8
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=8
        IMFGETNO(NMFGET,5)=-102
        IMFGETNO(NMFGET,6)=3
        IMFGETNO(NMFGET,7)=2
        SUFFIX=' Stk P'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Conn stack P (Pa)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Conn stack P change (Pa/hr)'
        endif
        CLIST=.true.
      elseif(INO.EQ.12)then

C Temperature at node: IMFGET = 2
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=2
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=6
        IMFGETNO(NMFGET,7)=1
        SUFFIX=' db T'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Node temperature (degC)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Node temp change (degC/hr)'
        endif
        NLIST=.true.
      elseif(INO.EQ.13)then

C Mass flow rate: IMFGET = 3.
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=3
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=3
        SUFFIX=' kg/s'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Conn flow rate (kg/s)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Conn flow rate cng (kg/s/hr)'
        endif
        CNLST=.true.
        CLIST=.true.
      elseif(INO.EQ.14)then

C Volume flow rate: IMFGET = 4.
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=4
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=2
        IMFGETNO(NMFGET,7)=3
        SUFFIX=' m^3/s'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Conn flow rate (m^3/s)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Conn flow rate cng (m^3/s/hr)'
        endif
        CNLST=.true.
        CLIST=.true.
      elseif(INO.EQ.15)then

C Air change rate: IMFGET = 5.
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=5
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=3
        IMFGETNO(NMFGET,7)=4
        SUFFIX=' ac/h'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Conn air change rate (-)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Conn air change rate cng (-/hr)'
        endif
        CNLST=.true.
        CRLST=.true.
        CLIST=.true.
      elseif(INO.EQ.16)then

C Velocity: IMFGET = 6.
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=6
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=6
        IMFGETNO(NMFGET,7)=4
        SUFFIX=' m/s'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Conn fluid velocity (m/s)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Conn fluid velocity cng (m/s/hr)'
        endif
        CLIST=.true.
      elseif(INO.EQ.17)then

C Contaminant concentration at node: IMFGET = 14
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=14
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=6
        IMFGETNO(NMFGET,7)=5
        SUFFIX=' g/kg'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Node contaminant conc (g/kg)'
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Node contam conc (g/kg/hr)'
        endif
        NLIST=.true.
        CTLST=.true.
      elseif(INO.EQ.18)then

C Energy associated with a flow stream: IMFGET = 13.
        call edisp(iuout,'This option not yet available.')
        goto 1
C        if((NMFGET+1).GT.MFRS)then
C          call usrmsg('Number of graph items exceeded.',' ','P')
C          goto 1
C        endif
C        NMFGET=NMFGET+1
C        IMFGETNO(NMFGET,1)=13
C        IMFGETNO(NMFGET,5)=-3
C        IMFGETNO(NMFGET,6)=7
C        IMFGETNO(NMFGET,7)=4
C        SUFFIX=' ener'
C        if(IROC.eq.0)then
C          SMFLABEL(NMFGET)='Conn energy (Whr)'
C        elseif(IROC.eq.1)then
C          SMFLABEL(NMFGET)='Conn energy cng (Whr/hr)'
C        endif
C << this facility under development) get code from mfetbl.F >>
C << ask for a reference node (probably outside), test node and >>
C << a connection for this calculation. >>
      elseif(INO.EQ.20)then

C Edit selections.
        if(NMFGET.gt.0)then
          call edmfgoget
          goto 1
        endif
      elseif(INO.EQ.21)then

C Output redirect to graphics, tabular listing or a text report.
        if(MENUTYPE.eq.3)then
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'G','Graphics',IER)
        elseif(MENUTYPE.eq.4)then
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'X','Tabular',IER)
        else
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'T','Text',IER)
        endif

C If user request cancel reset ixopen to zero.
        if(ier.eq.-3) ixopen=0
        goto 1
      elseif(INO.EQ.22)then

C Data presentation format (actual or rate of change).
        call usrmsg('Switch from raw data to rate of change or data',
     &    'does not (yet) apply to network flow data...ignored.','W')

C << rate of change needs to be implemented >>
C        if(IROC.eq.0)then
C          write(outs,'(a)')'(currently variable)'
C        else
C          write(outs,'(a)')'(currently rate of change)'
C        endif
C        h(1)='Typical results recovery are of the values within the'
C        h(2)='results library. An alternative is to report the rate '
C        h(3)='of change of a variable. The rate of change is expressed'
C        h(4)='in units per hour (of the normal unit for the variable).'
C        h(5)=' '
C        h(6)='Note: all selections cleared when this option is altered.'
C        CALL EASKABC('Data display options:',outs,
C     &    'variable (default)','rate of change of variable','contine',
C     &    II,6)
C        if(II.ne.3.and.(IROC.ne.(II-1)))then
C          IROC=II-1
C          call usrmsg ('Clearing current selections....','  ','W')
C          call MOFLOWSU
C        endif
      elseif(INO.EQ.23)then

C Clear all selections (time series plot).
        if(MENUTYPE.eq.3) then
          call MOFLOWSU
        else

C Occupancy filter (all types bar time series plot).
C Occupancy filter for flow results requires the user to specify>>
C a thermal zone.
          if(iocupf.eq.0)then

C Casual gain type.  Ask user which constitutes occupancy.
C Labels for loads, use first zones labels as examples. If time
C based set iocut=-1, if always occup set iocut=0.
            write(ll1,'(1x,a6,1x)') lodlabel(1,1)
            write(ll2,'(1x,a6,1x)') lodlabel(1,2)
            write(ll3,'(1x,a6,1x)') lodlabel(1,3)
            call edisp(iuout,' ')
            H(1)='Select a casual gain type to base occupancy on.'
            H(2)='Labels show are from the first zone in the current'
            H(3)='list. Type 1 usually represents casual gains from '
            H(4)='people,although any of the three casual gain types'
            H(5)='can be used. '
            IOT=1
            CALL EASKATOG('Which casual gain represents occupancy:',
     &         '  ','Always occupied',ll1,ll2,ll3,'time',' ',' ',IOT,5)
            if (IOT.gt.1.and.IOT.lt.5) then
              call usrmsg('  ','  ','-')
              iocupf=1
              iocut=IOT-1
              call scnocup(iier)
            elseif(IOT.eq.5)then
              iocupf=1
              iocut=-1
              call scnocup(iier)
            elseif(IOT.eq.1)then
              iocpzone=0
              iocupf=0
              iocut=0
              call scnocup(iier)
            endif
          else
            iocupf=0
            iocpzone=0
          endif

C For flow, need to ask the user which zone is to be used to get
C the occupany pattern from.
          if(iocupf.eq.1)then
            call edisp(iuout,
     &    'Occupancy filtering is based on casual gains in a zone.')
            call edisp(iuout,
     &    'Please select a zone from the list.')
            do 42 i=1,NCOMP
              zn(i)=zname(i)
  42        continue
            H(1)='Pick one zone to get occupancy pattern from. If'
            H(2)='none selected then full time occupancy assumed.'
            INPIC=1
            CALL EPICKS(INPIC,IVALS,
     &        'Which thermal zone has the occupancy pattern you',
     &        'want to use for filtering? ',
     &        12,NCOMP,zn,' zone list',IER,2)
            if(INPIC.eq.0)then
              iocupf=0
              iocut=0
              call scnocup(iier)
              iocpzone=0
            else
              iocpzone=IVALS(1)
            endif
          endif
          goto 1
        endif
      elseif(INO.EQ.24)then
        if(MENUTYPE.eq.3) then

C Set axis limits for the current plot.
          H(1)='Enter the minimum and maximum value for the y-axis.'
          do 555 Iaxis=1,5
            if (YAXSET(Iaxis).gt.0) then
 501          write (values,'(8(f9.2,1x))') YMIN(Iaxis),YMAX(Iaxis)
              if (Iaxis.eq.1) then
                call EASKS(values,'  ','Min &max for temperature axis?',
     &                                 20,' -10 30 ','temp axis',IER,1)
              elseif (Iaxis.eq.2) then
                call EASKS(values,'  ','Min & max for loads axis?',
     &                               20,' -100 100 ','temp axis',IER,1)
              elseif (Iaxis.eq.3) then
                call EASKS(values,'  ','Min & max for intensity axis?',
     &                                 20,' 0 1000 ','temp axis',IER,1)
              elseif (Iaxis.eq.4) then
                call EASKS(values,'  ','Min & max for misc axis?',
     &                                 20,' 0 100 ','temp axis',IER,1)
              elseif (Iaxis.eq.5) then
                call EASKS(values,'  ','Min&max for concntration axis?',
     &                                 20,' 0 0.25 ','conc axis',IER,1)
              endif
              K=0
              CALL EGETWR(values,K,YN,0.,0.,'-','Y min',IER)
              CALL EGETWR(values,K,YM,0.,0.,'-','Y max',IER)
              if (YN.gt.YM) then
                call usrmsg('Maximum value is less than minimum value.',
     &                    ' Please respecify. ','W')
                goto 501
              endif
              YMAX(Iaxis)=YM
              YMIN(Iaxis)=YN
              YAXSET(Iaxis)=2
            endif
 555      continue

C Redraw graph (if applicable).
          if (NMFGET.gt.0) then
            if (YAXSET(1).eq.2.or.YAXSET(2).eq.2.or.YAXSET(3).eq.2.or.
     &          YAXSET(4).eq.2.or.YAXSET(5).eq.2) return
          endif
        else

C Toggle time format and day separators for all other display formats.
          IHFLAG=IHFLAG+1
          if(IHFLAG.GT.1)IHFLAG=0
          if(IHFLAG.eq.0)then
            call edisp(iuout,' ')
            call edisp(iuout,'standard display time = 10h30 set ')
          elseif(IHFLAG.eq.1)then
            call edisp(iuout,' ')
            call edisp(iuout,'3rd party graphing time=day fraction set')
          endif
          H(1)='Places a # separator between days of tabular data.'
          CALL EASKAB('Include mark between days when ',
     &                'displaying or writing data :','no','yes',IDH,1)
          IDHFLG=IDH-1
          goto 1
        endif
      elseif(INO.EQ.25)then
        if(MENUTYPE.eq.3) then

C Add another selection - time series plot only.
          INO=-4
          ADDLIN=1
        else

C Toggle delimeter.
          H(1) ='Tabular data can be sent to file with various '
          H(2) ='delimiters: '
          H(3) ='  spaces (format using spaces to lineup columns)'
          H(4) ='  single space between columns'
          H(5) ='  comma separator (for excel)'
          H(6) ='  tab separator (for excel)'
          H(7) ='  tagged - marked up data'
          IWM=1
          CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &      'normal spaces','single space','tab','comma','tagged',
     &      'continue',' ',IWM,7)
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
          goto 1
        endif
      elseif(INO.eq.26)then
        if(MENUTYPE.eq.3) then

C Control labels if graphic plot.
C Add in a title for this graph 64 char string graftitle.
 290      H(1)='A title (up to 64 characters) can be included.'
          t64=graftitle
          CALL EASKS(t64,' Title for graph ? ',
     &     ' ',64,'  ','graph title',IER,1)
          if(t64(1:2).ne.'  '.and.t64(1:4).ne.'UNKN')then
            graftitle=t64
          else
            goto 290
          endif

C Add or clear labels within a graph.
          if(ngrlbl.eq.0)then
            CALL EASKAB('Labels within graph options:',' ',
     &        'edit/add labels','contine',igopt,0)
            if(igopt.eq.1)then
              igrl=0
              grlbl(1)=' '
              grlbl(2)=' '
              grlbl(3)=' '
              grlbl(4)=' '
              grlbl(5)=' '
            elseif(igopt.eq.2)then
              goto 1
            endif
          elseif(ngrlbl.gt.0)then
            CALL EASKABC('Labels within graph options:',' ',
     &        'clear existing labels','edit/add labels','contine',
     &        igopt,0)
            if(igopt.eq.1)then
              ngrlbl=0
              igrl=0
              grlbl(1)=' '
              grlbl(2)=' '
              grlbl(3)=' '
              grlbl(4)=' '
              grlbl(5)=' '
              goto 1
            elseif(igopt.eq.2)then
              igrl=0
            elseif(igopt.eq.3)then
              goto 1
            endif
          endif
 291      continue

C If there are no labels or some label start at first, if more than 5 exit.
          if(ngrlbl.eq.0)then
            ngrlbl=1
            igrl=1
          elseif(ngrlbl.gt.5)then
            call usrmsg('Array of labels is full.',' ','W')
            goto 1
          else
            igrl=igrl+1
          endif
          t24=grlbl(igrl)
 292      CALL EASKS(t24,
     &     'Label within graph (AFTER editing select a point on graph',
     &     'for the label position).',24,'x','graph label',IER,1)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            grlbl(igrl)=t24
            if(igrl.gt.ngrlbl)ngrlbl=igrl
          else
            goto 292
          endif

C Find out point where text should be drawn.
          CALL trackview(ii,ix,iy)
          lblpx(igrl)=ix
          lblpy(igrl)=iy
          t24=grlbl(igrl)
          iid1=lblpx(igrl); iid2=lblpy(igrl);
          call wstxpt(iid1,iid2,t24)
          dok=.true.
          h(1)='If you say yes you can insert another label in the'
          h(2)='graph. Otherwise you will be returned to the menu.'
          CALL ASKOK('Include another user supplied label?',' ',
     &      OKL,dok,2)
          if(OKL) goto 291

        elseif(MENUTYPE.eq.4) then

C Toggle tabular listing label format.
          ILFLAG=ILFLAG+1
          if(ILFLAG.GT.1)ILFLAG=0
          if(ILFLAG.eq.0)then
            call edisp(iuout,' ')
            call edisp(iuout,'column labels on multi-lines ')
          elseif(ILFLAG.eq.1)then
            call edisp(iuout,' ')
            call edisp(iuout,'one line column labels (spreadsheets)')
          endif
        else
          call edisp(iuout,'selected item 26?? ')
        endif
      elseif(INO.EQ.27.and.(MENUTYPE.eq.3.or.MENUTYPE.eq.4))then

C List out data to text feedback or to file or to graphics.

C Debug.
C        do 999 I=1,NMFGET
C          write(6,*) (IMFGETNO(I,K),K=1,7)
C          write(6,*) SMFLABEL(I)
C          write(6,*) GMFLABEL(I)
C          write(6,*) GLMFLABEL(I)
C          write(6,*) '  '
C 999    continue

        if (NMFGET.lt.1) then
          call usrmsg('No data to list/ plot!',
     &                'Please choose a metric.','W')
          INO=-3
        else

C Write GOGET info to file (so that current data recovery can be
C replicated).
          if(pifltog(1:3).eq.'ON ')call WRITEPIF(MENUTYPE)
          return
        endif

C At this point we just fall past the end of the INO equals statements.

      elseif(INO.EQ.(NITMS-1))then
        CALL PHELPD(MTITLE,18,'-',0,0,IER)
        goto 1
      elseif(INO.EQ.NITMS)then
        call usrmsg('  ','  ','-')
        MENUTYPE=-1
        return
      else

C Did not understand menu item or it was a non-action menu item.
        goto 1
      endif

C Most of the above logic falls through to this point. If an item
C has been selected proceed otherwise loop back to '1'.
      if (INO.lt.0.or.NMFGET.lt.1) goto 1

C Copy data for selected nodes/ connections as required by the logic above.
C Ask for relevent nodes. If CTLST is true then ask for containments.
      if (NLIST) then
        INOPT=0
        NNPK=NNOD
        PROMPT1='Select one or more flow nodes.'
        PROMPT2='  '
        NH=0
        call ASKMFNOD(INOPT,NNPK,INPK,PROMPT1,PROMPT2,NH)
        IF(CTLST)THEN
          IF(NCONTM.GT.0)THEN
            NCPK=NCONTM
            PROMPT1='Select one or more contaminants.'
            PROMPT2=' '
            NH=0
            CALL ASKMFCTM(NCPK,ICPK,PROMPT1,PROMPT2,NH)
          ELSE
            call edisp(iuout,' No contaminants defined')
            NMFGET=NMFGET-1
            GOTO 1
          ENDIF
        ENDIF

C CTLST false so scan through NNPK items and fill in the
c imfgetno array.
        IF(.NOT.CTLST)THEN
          if (NNPK.gt.0) then
            do 10 I=1,NNPK
              lnname=lnblnk(NDNAM(INPK(I)))
              if (I.eq.1) then
                IMFGETNO(NMFGET,2)=INPK(I)
              else
                if((NMFGET+1).GT.MFRS)then
                  call usrmsg('Number of items exceeded.',' ','P')
                  goto 10
                endif
                NMFGET=NMFGET+1
                IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                IMFGETNO(NMFGET,2)=INPK(I)
                IMFGETNO(NMFGET,3)=IMFGETNO(NMFGET-1,3)
                IMFGETNO(NMFGET,4)=IMFGETNO(NMFGET-1,4)
                IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)
              endif
              write (GMFLABEL(NMFGET),'(2a)')
     &          NDNAM(INPK(I))(1:lnname),SUFFIX
              write (GLMFLABEL(NMFGET),'(2a)')
     &          NDNAM(INPK(I))(1:lnname),SUFFIX
 10         continue

C If timestep listing go back to see if any other data should be included.
            if(MENUTYPE.eq.4) goto 1
          else
            NMFGET=NMFGET-1
          endif
        ELSE

C There are some contaminants so fill the arrays and generate
C appropriate labels.
          IF(NNPK.GT.0)THEN
            DO 11 I=1,NNPK
              LNNAME=LNBLNK(NDNAM(INPK(I)))
              IF(I.EQ.1)THEN
                IMFGETNO(NMFGET,2)=INPK(I)
                DO 12 J=1,NCPK
                  LNNAMEC=LNBLNK(CONTMNAM(ICPK(J)))
                  IF(J.EQ.1)THEN
                    IMFGETNO(NMFGET,3)=ICPK(J)
                  ELSE
                    if((NMFGET+1).GT.MFRS)then
                      call usrmsg('Number of items exceeded.',' ','P')
                      goto 11
                    endif
                    NMFGET=NMFGET+1
                    IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                    IMFGETNO(NMFGET,2)=IMFGETNO(NMFGET-1,2)
                    IMFGETNO(NMFGET,3)=ICPK(J)
                    IMFGETNO(NMFGET,4)=IMFGETNO(NMFGET-1,4)
                    IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                    IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                    IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)
                  ENDIF

C GMFLABEL is 20 char so truncate the text.
                  lcname=MIN0(LNNAMEC,7)
                  WRITE(GMFLABEL(NMFGET),'(4a)')
     &              CONTMNAM(ICPK(J))(1:lcname),'@',
     &              NDNAM(INPK(I))(1:8),SUFFIX(1:4)
                  WRITE(GLMFLABEL(NMFGET),'(4a)')
     &              CONTMNAM(ICPK(J))(1:LNNAMEC),' at ',
     &              NDNAM(INPK(I))(1:lnname),SUFFIX(1:lnblnk(SUFFIX))
 12             CONTINUE
              ELSE
                if((NMFGET+1).GT.MFRS)then
                  call usrmsg('Number of items exceeded.',' ','P')
                  goto 11
                endif
                NMFGET=NMFGET+1
                IMFGETNO(NMFGET,2)=INPK(I)
                DO 13 J=1,NCPK
                  LNNAMEC=LNBLNK(CONTMNAM(ICPK(J)))
                  IF(J.GT.1)THEN
                    if((NMFGET+1).GT.MFRS)then
                      call usrmsg('Number of items exceeded.',' ','P')
                      goto 11
                    endif
                    NMFGET=NMFGET+1
                    IMFGETNO(NMFGET,2)=IMFGETNO(NMFGET-1,2)
                  ENDIF
                  IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                  IMFGETNO(NMFGET,3)=ICPK(J)
                  IMFGETNO(NMFGET,4)=IMFGETNO(NMFGET-1,4)
                  IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                  IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                  IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)
                  WRITE(GMFLABEL(NMFGET),'(4a)')
     &              CONTMNAM(ICPK(J))(1:LNNAMEC),' at ',
     &              NDNAM(INPK(I))(1:lnname),SUFFIX
                  WRITE(GLMFLABEL(NMFGET),'(4a)')
     &              CONTMNAM(ICPK(J))(1:LNNAMEC),' at ',
     &              NDNAM(INPK(I))(1:lnname),SUFFIX
 13             CONTINUE
              ENDIF
 11         CONTINUE
          ENDIF
        ENDIF
      elseif (CLIST) then

C Ask if through one connection or more than one.
C If CRLST is true restrict node lists to internal node types.
        if (CRLST) then
          INOPT=1
        else
          INOPT=0
        endif

C Ask if only flows associated with a single node.
        if (CNLST) then
          h(1)='Individual connections: get data for each connection'
          h(2)='  separatly and report separately.'
          h(3)='Total of connections: sum data for selected con-'
          h(4)='  nections. You will first be asked for the node which'
          h(5)='  is related to the connections to be selected (in'
          h(6)='  order to resolve flow directions.'
          h(7)='Total entering node: sum of all flows into/towards the'
          h(8)='  node (e.g. entering a room) from all connections'
          h(9)='  to other internal and boundary nodes.'
          h(10)=' Note that total flow exiting a node equals that '
          h(11)=' entering it for a converged network.'
          h(12)='Total infiltration: sum of all flows into/toward the'
          h(13)='   node (e.g. entering a room) from boundary nodes.'
          h(14)='Inter-node flow: asks for two nodes and reports back'
          h(15)='  net flow between the nodes by summing up flows to '
          h(16)='  and from one node to the other and vice versa'
          call MENUATOL('Select a flow option',' ',
     &   'a individual connections','b total of connections',
     &   'c total entering node','d total from ambient',
     &   'e inter-nodal flow ','f cancel ',' ',' ',' ',' ',' ',
     &   ' ',IDH,IDNO,16)
          if(IDH.eq.1)then
            INODE=0
            itotal=0
          elseif(IDH.eq.2)then
            NNPK=1
            PROMPT1=
     &       'Select the node which is associated with the connections'
            PROMPT2='you want to display flow information for.'
            NH=0
            call ASKMFNOD(INOPT,NNPK,INPK,PROMPT1,PROMPT2,NH)
            INODE=INPK(1)
            itotal=0
          elseif(IDH.eq.3)then

C Note: return INPKT and NNPKT for total entering.
            NNPKT=NNOD
            PROMPT1=
     &       'Select the node(s) for which total flow(s) are reported.'
            PROMPT2=' '
            NH=2
            h(1)='The total flow reported is equal to'
            h(2)='the sum of the flows entering the node.'
            call ASKMFNOD(INOPT,NNPKT,INPKT,PROMPT1,PROMPT2,NH)
            INODE=0
            itotal=1
          elseif(IDH.eq.4.or.idh.eq.5)then
            NNPKT=NNOD
            IF(IDH.EQ.5)NNPKT=1
            PROMPT1='Select node for flow'
            PROMPT2=' '
            NH=0
            call ASKMFNOD(INOPT,NNPKT,INPKT,PROMPT1,PROMPT2,NH)
            if(idh.eq.5)then
              NNPKT=NNOD
              PROMPT1='Select second node'
              NH=0
              call ASKMFNOD(INOPT2,NNPKT,INPKT2,PROMPT1,PROMPT2,NH)
            endif
            INODE=0
            itotal=1
          elseif(IDH.eq.6)then
            NMFGET=NMFGET-1
            goto 1
          endif

C Debug.
C          write(6,*) 'after easkabcd itotal & nnpk ',itotal,nnpk,inpk
C          write(6,*) 'after easkabcd itotal & nnpkt ',itotal,nnpkt,inpkt

        else
          INODE=0
          itotal=0
        endif

C If CRLST is true check if a node was specified above - if not then
C ask for one.
        if (CRLST.and.itotal.eq.0) then
          if (INODE.eq.0) then
            NNPK=1
            PROMPT1='Select the reference node.'
            PROMPT2='  '
            NH=0
            call ASKMFNOD(INOPT,NNPK,INPK,PROMPT1,PROMPT2,NH)
            IMFGETNO(NMFGET,3)=INPK(1)
          else
            IMFGETNO(NMFGET,3)=INODE
          endif
        endif
        NNPK=NCNN

C Ask for connections if required otherwise use value of NNPK from
C nodes questions above. I.E. total flow entering was requested.
        if(itotal.eq.0)call ASKMFCON(INODE,NNPK,ICPK)

C Debug.
C        write(6,*) 'after itotal & nnpk ',itotal,nnpk,inpk
C        write(6,*) 'after itotal & nnpkt ',itotal,nnpkt,inpkt

        lsf=lnblnk(SUFFIX)
        if (NNPK.gt.0) then
          if (INODE.gt.0) then

C Total the flow of separate connections and generate appropriate
C labels for total entering.
            NMFLST=NMFLST+1
            IMFGETNO(NMFGET,4)=NMFLST
            IMFLIST(IMFGETNO(NMFGET,4),1)=NNPK
            do 30 I=1,NNPK
              IMFLIST(IMFGETNO(NMFGET,4),1+I)=ICPK(I)
              if(lnblnk(SUFFIX).le.5)then
                lnname=MIN0(lnblnk(NDNAM(INODE)),11)
              else
                lnname=MIN0(lnblnk(NDNAM(INODE)),10)
              endif
              llnname=lnblnk(NDNAM(INODE))
              write(GMFLABEL(NMFGET),'(3a)') 'Tot ',
     &          NDNAM(INODE)(1:lnname),SUFFIX(1:lsf)
              write(GLMFLABEL(NMFGET),'(3a)') 'Total ',
     &          NDNAM(INODE)(1:llnname),SUFFIX(1:lsf)
 30         continue
          else

C An individual connection.
            if(itotal.eq.0)then
              do 20 I=1,NNPK
                if (I.eq.1) then
                  IMFGETNO(NMFGET,2)=ICPK(I)

C In the case of velocity, find the related component type and reject those
C connections which are not appropriate.
                  if(IMFGETNO(NMFGET,1).eq.6)then
                    icompindex=ITPCON(ICPK(I))
                    icomptype=ITPCMP(icompindex)
                    if(icomptype.eq.10.or.icomptype.eq.15.or.
     &                 icomptype.eq.17.or.icomptype.eq.20.or.
     &                 icomptype.eq.25.or.icomptype.eq.30.or.
     &                 icomptype.eq.35.or.icomptype.eq.310.or.
     &                 icomptype.eq.410)then
                      NMFGET=NMFGET-1
                      write(outs,*) 'Skipping velocity component ',
     &                  icompindex,' of type ',icomptype
                      call edisp(iuout,outs)
                      goto 20
                    elseif(icomptype.eq.40)then
                      area=SUPCMP(icompindex,2)
                    elseif(icomptype.eq.50)then
                      PI = 4.0 * ATAN(1.0)
                  area= PI * (SUPCMP(icompindex,3)*SUPCMP(icompindex,3))
                    elseif(icomptype.eq.110)then
                      area=SUPCMP(icompindex,2)
                    elseif(icomptype.eq.120)then
                      area=SUPCMP(icompindex,2)*SUPCMP(icompindex,3)
                    elseif(icomptype.eq.130)then
                      area=SUPCMP(icompindex,2)*SUPCMP(icompindex,3)
                    elseif(icomptype.eq.210.or.icomptype.eq.220.or.
     &                     icomptype.eq.230.or.icomptype.eq.240.or.
     &                     icomptype.eq.250.or.icomptype.eq.460)then
                      area=SUPCMP(icompindex,3)
                    elseif(icomptype.eq.420)then
                      area=SUPCMP(icompindex,2)
                    else
                      area=1.0
                    endif

C Tell user about the area of the component related to each connection.
                    write(outs,*) 'x-section area of component type ',
     &                icomptype,' is ',area
                    call edisp(iuout,outs)
                  endif
                  llns=lnblnk(NDNAM(NODPS(ICPK(I))))
                  llne=lnblnk(NDNAM(NODNE(ICPK(I))))
                  if(lnblnk(SUFFIX).le.4)then
                    write(GMFLABEL(NMFGET),'(4a)')
     &                NDNAM(NODPS(ICPK(I)))(1:7),'=>',
     &                NDNAM(NODNE(ICPK(I)))(1:7),SUFFIX(1:lsf)
                    write(GLMFLABEL(NMFGET),'(4a)')
     &                NDNAM(NODPS(ICPK(I)))(1:llns),'=>',
     &                NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                  else
                    write(GMFLABEL(NMFGET),'(4a)')
     &                NDNAM(NODPS(ICPK(I)))(1:6),'=>',
     &                NDNAM(NODNE(ICPK(I)))(1:6),SUFFIX(1:lsf)
                    write(GLMFLABEL(NMFGET),'(4a)')
     &                NDNAM(NODPS(ICPK(I)))(1:llns),'=>',
     &                NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                  endif

C Check for bi-directional flow components.  If this is one then ask if net or
C +ive and -ive flows should be reported.  In the latter case an extra entry will
C be made to the MFGET arrays (the existing enrty will either be the net or +ive
C flow and the extra entry would be the -ive flow).
                  icompindex=ITPCON(ICPK(I))
                  icomptype=ITPCMP(icompindex)
                  if(icomptype.eq.130) then
                    h(1)='The reported flow will either be the net'
                    h(2)='flow through this component or the two'
                    h(3)='individual flows (+ive and -ive).'
                    call EASKAB('Bi-directional flow component ',
     &                   'reporting options:','net flow',
     &                   'individual flows',II,3)

C If net flow do nothing (the existing definition is sufficient)
C otherwise add the extra entry.
                    if (II.eq.1) then
                      IMFGETNO(NMFGET,4)=0
                    else
                      if((NMFGET+1).GT.MFRS)then
                        call usrmsg('Number of items exceeded.',' ','P')
                        goto 20
                      endif
                      NMFGET=NMFGET+1
                      IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                      IMFGETNO(NMFGET,2)=ICPK(I)
                      IMFGETNO(NMFGET,3)=IMFGETNO(NMFGET-1,3)
                      IMFGETNO(NMFGET-1,4)=1
                      IMFGETNO(NMFGET,4)=-1
                      IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                      IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                      IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)
                      llns=lnblnk(NDNAM(NODPS(ICPK(I))))
                      llne=lnblnk(NDNAM(NODNE(ICPK(I))))
                      if(lnblnk(SUFFIX).le.4)then
                        write(GMFLABEL(NMFGET-1),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:7),'->',
     &                    NDNAM(NODNE(ICPK(I)))(1:7),SUFFIX(1:lsf)
                        write(GLMFLABEL(NMFGET-1),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:llns),'->',
     &                    NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                        write(GMFLABEL(NMFGET),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:7),'<-',
     &                    NDNAM(NODNE(ICPK(I)))(1:7),SUFFIX(1:lsf)
                        write(GLMFLABEL(NMFGET),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:llns),'<-',
     &                    NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                      else
                        write(GMFLABEL(NMFGET-1),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:6),'->',
     &                    NDNAM(NODNE(ICPK(I)))(1:6),SUFFIX(1:lsf)
                        write(GLMFLABEL(NMFGET-1),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:llns),'->',
     &                    NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                        write(GMFLABEL(NMFGET),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:6),'<-',
     &                    NDNAM(NODNE(ICPK(I)))(1:6),SUFFIX(1:lsf)
                        write(GLMFLABEL(NMFGET),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:llns),'<-',
     &                    NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                      endif
                    endif
                  endif
                else
                  if((NMFGET+1).GT.MFRS)then
                    call usrmsg('Number of items exceeded.',' ','P')
                    goto 20
                  endif

C In the case of flow velocity check for appropriate component and report area.
                  if(IMFGETNO(NMFGET,1).eq.6)then
                    icompindex=ITPCON(ICPK(I))
                    icomptype=ITPCMP(icompindex)
                    if(icomptype.eq.10.or.icomptype.eq.15.or.
     &                 icomptype.eq.17.or.icomptype.eq.20.or.
     &                 icomptype.eq.25.or.icomptype.eq.30.or.
     &                 icomptype.eq.35.or.icomptype.eq.310.or.
     &                 icomptype.eq.410)then
                      write(outs,*) 'Skipping velocity component ',
     &                  icompindex,' of type ',icomptype
                      call edisp(iuout,outs)
                      NMFGET=NMFGET-1
                      goto 20
                    elseif(icomptype.eq.40)then
                      area=SUPCMP(icompindex,2)
                    elseif(icomptype.eq.50)then
                      PI = 4.0 * ATAN(1.0)
                  area= PI * (SUPCMP(icompindex,3)*SUPCMP(icompindex,3))
                    elseif(icomptype.eq.110)then
                      area=SUPCMP(icompindex,2)
                    elseif(icomptype.eq.120)then
                      area=SUPCMP(icompindex,2)*SUPCMP(icompindex,3)
                    elseif(icomptype.eq.130)then
                      area=SUPCMP(icompindex,2)*SUPCMP(icompindex,3)
                    elseif(icomptype.eq.210.or.icomptype.eq.220.or.
     &                     icomptype.eq.230.or.icomptype.eq.240.or.
     &                     icomptype.eq.250.or.icomptype.eq.460)then
                      area=SUPCMP(icompindex,3)
                    elseif(icomptype.eq.420)then
                      area=SUPCMP(icompindex,2)
                    else
                      area=1.0
                    endif
                    write(outs,*) 'x-section area of component type ',
     &                icomptype,' is ',area
                    call edisp(iuout,outs)
                  endif
                  NMFGET=NMFGET+1
                  IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                  IMFGETNO(NMFGET,2)=ICPK(I)
                  IMFGETNO(NMFGET,3)=IMFGETNO(NMFGET-1,3)
                  IMFGETNO(NMFGET,4)=IMFGETNO(NMFGET-1,4)
                  IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                  IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                  IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)
                  llns=lnblnk(NDNAM(NODPS(ICPK(I))))
                  llne=lnblnk(NDNAM(NODNE(ICPK(I))))
                  if(lnblnk(SUFFIX).le.4)then
                    write(GMFLABEL(NMFGET),'(4a)')
     &                NDNAM(NODPS(ICPK(I)))(1:7),'=>',
     &                NDNAM(NODNE(ICPK(I)))(1:7),SUFFIX(1:lsf)
                    write(GLMFLABEL(NMFGET),'(4a)')
     &                NDNAM(NODPS(ICPK(I)))(1:llns),'=>',
     &                NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                  else
                    write(GMFLABEL(NMFGET),'(4a)')
     &                NDNAM(NODPS(ICPK(I)))(1:6),'=>',
     &                NDNAM(NODNE(ICPK(I)))(1:6),SUFFIX(1:lsf)
                    write(GLMFLABEL(NMFGET),'(4a)')
     &                NDNAM(NODPS(ICPK(I)))(1:llns),'=>',
     &                NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                  endif

C Check for bi-directional flow components.  If this is one then ask if net or
C +ive and -ive flows should be reported.  In the latter case an extra entry will
C be made to the MFGET arrays (the existing enrty will either be the net or +ive
C flow and the extra entry would be the -ive flow).
                  icompindex=ITPCON(ICPK(I))
                  icomptype=ITPCMP(icompindex)
                  if(icomptype.eq.130) then
                    h(1)='The reported flow will either be the net'
                    h(2)='flow through this component or the two'
                    h(3)='individual flows (+ive and -ive).'
                    call EASKAB('Bi-directional flow component ',
     &                   'reporting options:','net flow',
     &                   'individual flows',II,3)

C If net flow do nothing (the existing definition is sufficient)
C otherwise add the extra entry.
                    if (II.eq.1) then
                      IMFGETNO(NMFGET,4)=0
                    else
                      if((NMFGET+1).GT.MFRS)then
                        call usrmsg('Number of items exceeded.',' ','P')
                        goto 20
                      endif
                      NMFGET=NMFGET+1
                      IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                      IMFGETNO(NMFGET,2)=ICPK(I)
                      IMFGETNO(NMFGET,3)=IMFGETNO(NMFGET-1,3)
                      IMFGETNO(NMFGET-1,4)=1
                      IMFGETNO(NMFGET,4)=-1
                      IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                      IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                      IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)

C Depending on the length of the strings involved make up
C an appropraite label. llns and llne are the string lengths
C for the start and end nodes.
                      llns=lnblnk(NDNAM(NODPS(ICPK(I))))
                      llne=lnblnk(NDNAM(NODNE(ICPK(I))))
                      if(lnblnk(SUFFIX).le.4)then
                        write(GMFLABEL(NMFGET-1),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:7),'->',
     &                    NDNAM(NODNE(ICPK(I)))(1:7),SUFFIX(1:lsf)
                        write(GLMFLABEL(NMFGET-1),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:llns),'->',
     &                    NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                        write(GMFLABEL(NMFGET),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:7),'<-',
     &                    NDNAM(NODNE(ICPK(I)))(1:7),SUFFIX(1:lsf)
                        write(GLMFLABEL(NMFGET),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:llns),'<-',
     &                    NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                      else
                        write(GMFLABEL(NMFGET-1),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:6),'->',
     &                    NDNAM(NODNE(ICPK(I)))(1:6),SUFFIX(1:lsf)
                        write(GLMFLABEL(NMFGET-1),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:llns),'->',
     &                    NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                        write(GMFLABEL(NMFGET),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:6),'<-',
     &                    NDNAM(NODNE(ICPK(I)))(1:6),SUFFIX(1:lsf)
                        write(GLMFLABEL(NMFGET),'(4a)')
     &                    NDNAM(NODPS(ICPK(I)))(1:llns),'<-',
     &                    NDNAM(NODNE(ICPK(I)))(1:llne),SUFFIX(1:lsf)
                      endif
                    endif
                  endif
                endif
 20           continue
            elseif(idh.eq.3)then

C Total flow into node.
              do 21 I=1,NNPKT
                if (I.eq.1) then
                  IMFGETNO(NMFGET,2)=0
                  IMFGETNO(NMFGET,3)=INPKT(I)
                  IMFGETNO(NMFGET,4)=-1*(INPKT(I))
                  lln=lnblnk(NDNAM(INPKT(I)))
                  if(lnblnk(SUFFIX).le.5)then
                    lnname=MIN0(lnblnk(NDNAM(INPKT(I))),12)
                  else
                    lnname=MIN0(lnblnk(NDNAM(INPKT(I))),10)
                  endif
                  write(GMFLABEL(NMFGET),'(3a)')'->',
     &              NDNAM(INPKT(I))(1:lnname),SUFFIX(1:lsf)
                  write(GLMFLABEL(NMFGET),'(3a)')'->',
     &              NDNAM(INPKT(I))(1:lln),SUFFIX(1:lsf)
                else

C If there are reporting slots available, copy the information set in
C the previous IMFGETNO for use with additional connections.
                  if((NMFGET+1).GT.MFRS)then
                    call usrmsg('Number of items exceeded.',' ','P')
                    goto 22
                  endif
                  NMFGET=NMFGET+1
                  IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                  IMFGETNO(NMFGET,2)=IMFGETNO(NMFGET-1,2)
                  IMFGETNO(NMFGET,3)=INPKT(I)
                  IMFGETNO(NMFGET,4)=-1*(INPKT(I))
                  IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                  IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                  IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)
                  lln=lnblnk(NDNAM(INPKT(I)))
                  if(lnblnk(SUFFIX).le.5)then
                    lnname=MIN0(lnblnk(NDNAM(INPKT(I))),12)
                  else
                    lnname=MIN0(lnblnk(NDNAM(INPKT(I))),10)
                  endif
                  write(GMFLABEL(NMFGET),'(3a)')'->',
     &              NDNAM(INPKT(I))(1:lnname),SUFFIX(1:lsf)
                  write(GLMFLABEL(NMFGET),'(3a)')'->',
     &              NDNAM(INPKT(I))(1:lln),SUFFIX(1:lsf)
                ENDIF
 21           CONTINUE
 22           continue
            elseif(idh.eq.4)then

C Total flow into node from ambient
              do 201 I=1,NNPKT
                if (I.eq.1) then
                  IMFGETNO(NMFGET,2)=-1
                  IMFGETNO(NMFGET,3)=INPKT(I)
                  IMFGETNO(NMFGET,4)=-1*(INPKT(I))
                  lln=lnblnk(NDNAM(INPKT(I)))
                  if(lnblnk(SUFFIX).le.5)then
                    lnname=MIN0(lnblnk(NDNAM(INPKT(I))),11)
                  else
                    lnname=MIN0(lnblnk(NDNAM(INPKT(I))),9)
                  endif
                  write(GMFLABEL(NMFGET),'(3a)')'amb->',
     &              NDNAM(INPKT(I))(1:lnname),SUFFIX(1:lsf)
                  write(GLMFLABEL(NMFGET),'(3a)')'ambient->',
     &              NDNAM(INPKT(I))(1:lln),SUFFIX(1:lsf)
                  NMFGET=NMFGET+1
                  DO 211 NXT=1,7
                    IMFGETNO(NMFGET,NXT)=IMFGETNO(NMFGET-1,NXT)
 211              CONTINUE
                  write(GMFLABEL(NMFGET),'(3a)')'amb<-',
     &              NDNAM(INPKT(I))(1:lnname),SUFFIX(1:lsf)
                  write(GLMFLABEL(NMFGET),'(3a)')'ambient<-',
     &              NDNAM(INPKT(I))(1:lln),SUFFIX(1:lsf)
                else

C If there are reporting slots available, copy the information set in
C the previous IMFGETNO for use with additional connections.
                  if((NMFGET+1).GT.MFRS)then
                    call usrmsg('Number of items exceeded.',' ','P')
                    goto 202
                  endif
                  NMFGET=NMFGET+1
                  IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                  IMFGETNO(NMFGET,2)=IMFGETNO(NMFGET-1,2)
                  IMFGETNO(NMFGET,3)=INPKT(I)
                  IMFGETNO(NMFGET,4)=-1*(INPKT(I))
                  IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                  IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                  IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)
                  lln=lnblnk(NDNAM(INPKT(I)))
                  if(lnblnk(SUFFIX).le.5)then
                    lnname=MIN0(lnblnk(NDNAM(INPKT(I))),11)
                  else
                    lnname=MIN0(lnblnk(NDNAM(INPKT(I))),9)
                  endif
                  write(GMFLABEL(NMFGET),'(3a)')'amb->',
     &              NDNAM(INPKT(I))(1:lnname),SUFFIX(1:lsf)
                  write(GLMFLABEL(NMFGET),'(3a)')'ambient->',
     &              NDNAM(INPKT(I))(1:lln),SUFFIX(1:lsf)
                  NMFGET=NMFGET+1
                  IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                  IMFGETNO(NMFGET,2)=IMFGETNO(NMFGET-1,2)
                  IMFGETNO(NMFGET,3)=INPKT(I)
                  IMFGETNO(NMFGET,4)=-1*(INPKT(I))
                  IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                  IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                  IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)
                  write(GMFLABEL(NMFGET),'(3a)')'amb<-',
     &              NDNAM(INPKT(I))(1:lnname),SUFFIX(1:lsf)
                  write(GLMFLABEL(NMFGET),'(3a)')'ambient<-',
     &              NDNAM(INPKT(I))(1:lln),SUFFIX(1:lsf)
                endif
 201           continue
 202           continue
            elseif(idh.eq.5)then

C Inter-nodal flow
              do 251 I=1,NNPKT
                if (I.eq.1) then
                  IMFGETNO(NMFGET,2)=-2
                  IMFGETNO(NMFGET,3)=INPKT(I)
                  IMFGETNO(NMFGET,4)=-1*(INPKT2(I))
                  lln=lnblnk(NDNAM(INPKT(I)))
                  lln2=lnblnk(NDNAM(INPKT2(I)))
                  if(lnblnk(SUFFIX).le.5)then
                    lnname=MIN0(lnblnk(NDNAM(INPKT(I))),12)
                    lnname2=MIN0(lnblnk(NDNAM(INPKT2(I))),12)
                  else
                    lnname=MIN0(lnblnk(NDNAM(INPKT(I))),10)
                    lnname2=MIN0(lnblnk(NDNAM(INPKT2(I))),12)
                  endif
                  write(GMFLABEL(NMFGET),'(4a)')
     &              NDNAM(INPKT(I))(1:lnname),'->',
     &              NDNAM(INPKT2(I))(1:lnname2),SUFFIX(1:lsf)
                  write(GLMFLABEL(NMFGET),'(4a)')
     &              NDNAM(INPKT(I))(1:lln),'->',
     &              NDNAM(INPKT2(I))(1:lln2),SUFFIX(1:lsf)
                  NMFGET=NMFGET+1
                  DO 261 NXT=1,7
                    IMFGETNO(NMFGET,NXT)=IMFGETNO(NMFGET-1,NXT)
 261              CONTINUE
                  write(GMFLABEL(NMFGET),'(4a)')
     &              NDNAM(INPKT2(I))(1:lnname2),'->',
     &              NDNAM(INPKT(I))(1:lnname),SUFFIX(1:lsf)
                  write(GLMFLABEL(NMFGET),'(4a)')
     &              NDNAM(INPKT2(I))(1:lln2),'->',
     &              NDNAM(INPKT(I))(1:lln),SUFFIX(1:lsf)
                else

C If there are reporting slots available, copy the information set in
C the previous IMFGETNO for use with additional connections.
                  if((NMFGET+1).GT.MFRS)then
                    call usrmsg('Number of items exceeded.',' ','P')
                    goto 203
                  endif
                  NMFGET=NMFGET+1
                  IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                  IMFGETNO(NMFGET,2)=IMFGETNO(NMFGET-1,2)
                  IMFGETNO(NMFGET,3)=IMFGETNO(NMFGET-1,3)
                  IMFGETNO(NMFGET,4)=-1*(INPKT2(I))
                  IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                  IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                  IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)
                  lln=lnblnk(NDNAM(INPKT(I)))
                  lln2=lnblnk(NDNAM(INPKT2(I)))
                  if(lnblnk(SUFFIX).le.5)then
                    lnname=MIN0(lnblnk(NDNAM(INPKT(1))),12)
                    lnname2=MIN0(lnblnk(NDNAM(INPKT2(I))),12)
                  else
                    lnname=MIN0(lnblnk(NDNAM(INPKT(1))),10)
                    lnname2=MIN0(lnblnk(NDNAM(INPKT2(I))),12)
                  endif
                  write(GMFLABEL(NMFGET),'(4a)')
     &              NDNAM(INPKT(1))(1:lnname),'->',
     &              NDNAM(INPKT2(I))(1:lnname2),SUFFIX(1:lsf)
                  write(GLMFLABEL(NMFGET),'(4a)')
     &              NDNAM(INPKT(1))(1:lln),'->',
     &              NDNAM(INPKT2(I))(1:lln2),SUFFIX(1:lsf)
                  NMFGET=NMFGET+1
                  IMFGETNO(NMFGET,1)=IMFGETNO(NMFGET-1,1)
                  IMFGETNO(NMFGET,2)=IMFGETNO(NMFGET-1,2)
                  IMFGETNO(NMFGET,3)=IMFGETNO(NMFGET-1,3)
                  IMFGETNO(NMFGET,4)=-1*(INPKT2(I))
                  IMFGETNO(NMFGET,5)=IMFGETNO(NMFGET-1,5)
                  IMFGETNO(NMFGET,6)=IMFGETNO(NMFGET-1,6)
                  IMFGETNO(NMFGET,7)=IMFGETNO(NMFGET-1,7)
                  write(GMFLABEL(NMFGET),'(4a)')
     &              NDNAM(INPKT2(I))(1:lnname2),'->',
     &              NDNAM(INPKT(1))(1:lnname),SUFFIX(1:lsf)
                  write(GLMFLABEL(NMFGET),'(4a)')
     &              NDNAM(INPKT2(I))(1:lln2),'->',
     &              NDNAM(INPKT(1))(1:lln),SUFFIX(1:lsf)
                endif
 251           continue
 203           continue
            endif
          endif

C If timestep listing go back to see if any other data should be included.
          if(MENUTYPE.eq.4) goto 1
        else

C There has been a problem, so decrement NMFGET.
          NMFGET=NMFGET-1
        endif
      endif

C Debug.
C        do 9999 I=1,NMFGET
C          write(6,*) (IMFGETNO(I,K),K=1,7)
C          write(6,*) SMFLABEL(I)
C          write(6,*) GMFLABEL(I)
C          write(6,*) GLMFLABEL(I)
C          write(6,*) ' ------------------- '
C 9999   continue

      RETURN
      END

C ************************ GOMFGETSU ************************
C GOMFGETSU: Sets up arrays for mass flow data recovery.

      SUBROUTINE GOMFGETSU
#include "net_flow.h"

      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,7)
      COMMON/MFPK2/NMFLST,IMFLIST(MFCNLST,MFCNEL+1)
      COMMON/MFGET/FLOWVALS(MFRS),MFRECVR(MCNN+2+MCONTM)
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      CHARACTER CONTMNAM*12
      DIMENSION ICPK(MCNN)

C Clear record recover array.
      do 10 I=1,MCNN+2+MCONTM
        MFRECVR(I)=0
 10   continue

C Set up array of records to read.
C Always recover node temperatures and pressures.
      MFRECVR(1)=1
      MFRECVR(2)=1
      do 20 I=1,NMFGET

C Contaminant concentrations on record 2+NCONTM
        IF(IMFGETNO(I,1).EQ.14)THEN
          MFRECVR(2+IMFGETNO(I,3))=1
        ENDIF
        if (IMFGETNO(I,1).eq.3.or.IMFGETNO(I,1).eq.4.or.
     &      IMFGETNO(I,1).eq.5.or.IMFGETNO(I,1).eq.6) then

C Connection flows and stack pressure on record 2+NCON+NCONTM.
          if (IMFGETNO(I,2).gt.0) then
            MFRECVR(2+NCONTM+IMFGETNO(I,2))=1
          else
            ILST=IMFGETNO(I,4)
            if(ilst.gt.0)then
              do 30 J=1,IMFLIST(ILST,1)
                MFRECVR(2+NCONTM+IMFLIST(ILST,J+1))=1
 30           continue
            else

C All connections associated with node index -1 * ILST
              call ASKMFCON(ILST,NNPK,ICPK)
              do 40 IJ=1,NNPK
                MFRECVR(2+NCONTM+ICPK(IJ))=1
 40           continue
            endif
          endif
        endif

C Get contaminant concentrations always
C        IF(IMFGETNO(I,1).EQ.14)MFRECVR
 20   continue

      RETURN
      END

C ************************ GOMFGET ************************
C GOMFGET: Retrieves data from the mass flow result library according
C to arrays IMFGETNO and MFRECVR.

      SUBROUTINE GOMFGET(IDAY,IHR,ITS)
#include "net_flow.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,7)
      COMMON/MFGET/FLOWVALS(MFRS),MFRECVR(MCNN+2+MCONTM)
      COMMON/MFREC/FRDAT(MCNN+2+MCONTM,NMRL)

      character outs*124,CONTMNAM*12

C Update timestep array with new records.
C Read required records based on flags in MFRECVR
      IUNIT=IFIL+4
      NEXT=0

C Calculate start record for specified day, hour, timestep.
C Assuming at same frequency as building results
      IHREC=NNOD+NCMP+NCNN+NCONTM+2 + (NCONTM+NCNN+2)*(IDAY-1)*NTS*24 +
     &  (NCONTM+NCNN+2)*((IHR-1)*NTS) + (NCONTM+NCNN+2)*(ITS-1)

C Recover required records for timestep.
      do 10 I=1,MCNN+2+MCONTM
        if (MFRECVR(I).eq.1) then
          IREC=IHREC+I-1
          READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=99)(FRDAT(I,K),K=1,NMRL)

C Dwebug.
C          write(6,*) IDAY,IHR,ITS,IREC,I,(FRDAT(I,K),K=1,10)

        endif
 10   continue

C Put requested data into correct slots.
      do 20 I=1,NMFGET
        IF(NEXT.EQ.1)then
          next=0
          GOTO 20
        endif
        if (IMFGETNO(I,1).eq.1) then

C Node pressure.
          FLOWVALS(I)=FRDAT(1,IMFGETNO(I,2))
        elseif (IMFGETNO(I,1).eq.2) then

C Node temperature.
          FLOWVALS(I)=FRDAT(2,IMFGETNO(I,2))
        elseif (IMFGETNO(I,1).eq.3) then

C Connection net mass flow rate (kg/s).
          if (IMFGETNO(I,2).gt.0) then

C Single connection. Depending on IMFGETNO(I,4) return +ive, -ive or net flow.
            IROW=2+IMFGETNO(I,2)+NCONTM
            if (IMFGETNO(I,4).gt.0) then
              FLOWVALS(I)=FRDAT(IROW,2)
            elseif (IMFGETNO(I,4).lt.0) then
              FLOWVALS(I)=FRDAT(IROW,3)
            else
              FLOWVALS(I)=FRDAT(IROW,2)+FRDAT(IROW,3)
            endif
          elseif(IMFGETNO(I,2).eq.0)then

C All connections, return total flow INTO the node.
            call MFSUMCON(I,1,FLOW1,FLOW2)
            FLOWVALS(I)=FLOW1
          elseif(IMFGETNO(I,2).eq.-1) then

C Return flow to and from ambient for this node
            call MFSUMCON(I,1,FLOW1,FLOW2)
            FLOWVALS(I)=FLOW1
            FLOWVALS(I+1)=FLOW2
            NEXT=1
          elseif(IMFGETNO(I,2).eq.-2) then

C Return inter-node flow for two nodes
            call MFSUMCON(I,1,FLOW1,FLOW2)
            FLOWVALS(I)=FLOW1
            FLOWVALS(I+1)=FLOW2
            NEXT=1
          endif
        elseif (IMFGETNO(I,1).eq.4) then

C Connection net volume flow rate (m^3/s).
          if (IMFGETNO(I,2).gt.0) then

C Single connection. Depending on IMFGETNO(I,4) return +ive, -ive or net flow.
            call MASS2VOL(IMFGETNO(I,2),VFLOW1,VFLOW2)
            if (IMFGETNO(I,4).gt.0) then
              FLOWVALS(I)=VFLOW1
            elseif (IMFGETNO(I,4).lt.0) then
              FLOWVALS(I)=VFLOW2
            else
              FLOWVALS(I)=VFLOW1+VFLOW2
            endif
          elseif(IMFGETNO(I,2).eq.0)then

C All connections, return total flow into node.
            call MFSUMCON(I,2,VFLOW1,VFLOW2)
            FLOWVALS(I)=VFLOW1
          elseif(IMFGETNO(I,2).eq.-1) then

C Return flow to and from ambient for this node
            call MFSUMCON(I,2,VFLOW1,VFLOW2)
            FLOWVALS(I)=VFLOW1
            FLOWVALS(I+1)=VFLOW2
            NEXT=1
          elseif(IMFGETNO(I,2).eq.-2) then

C Return inter-node flow for two nodes
            call MFSUMCON(I,2,VFLOW1,VFLOW2)
            FLOWVALS(I)=VFLOW1
            FLOWVALS(I+1)=VFLOW2
            NEXT=1
          endif
        elseif (IMFGETNO(I,1).eq.5) then

C Connection net air change rate rate (-).  Depending on IMFGETNO(I,4)
C return +ive, -ive or net flow.
          if (IMFGETNO(I,2).gt.0) then
            call MASS2VOL(IMFGETNO(I,2),VFLOW1,VFLOW2)
            if (IMFGETNO(I,4).gt.0) then
              FLOWVALS(I)=VFLOW1
            elseif (IMFGETNO(I,4).lt.0) then
              FLOWVALS(I)=VFLOW2
            else
              FLOWVALS(I)=VFLOW1+VFLOW2
            endif
            FLOWVALS(I)=FLOWVALS(I)*3600.0/SUPNOD(IMFGETNO(I,3),2)
          elseif(IMFGETNO(I,2).eq.0)then

C All connections, return total flow into node.
            call MFSUMCON(I,2,VFLOW1,VFLOW2)
            FLOWVALS(I)=VFLOW1
            FLOWVALS(I)=FLOWVALS(I)*3600.0/SUPNOD(IMFGETNO(I,3),2)
          elseif(IMFGETNO(I,2).eq.-1) then

C Return flow to and from ambient for this node
            call MFSUMCON(I,2,VFLOW1,VFLOW2)
            FLOWVALS(I)=VFLOW1
            FLOWVALS(I+1)=VFLOW2
            FLOWVALS(I)=FLOWVALS(I)*3600.0/SUPNOD(IMFGETNO(I,3),2)
            FLOWVALS(I+1)=FLOWVALS(I+1)*3600.0/SUPNOD(IMFGETNO(I,3),2)
            NEXT=1
          elseif(IMFGETNO(I,2).eq.-2) then

C Return inter-node flow for two nodes
            call MFSUMCON(I,2,VFLOW1,VFLOW2)
            FLOWVALS(I)=VFLOW1
            FLOWVALS(I+1)=VFLOW2
            FLOWVALS(I)=FLOWVALS(I)*3600.0/SUPNOD(IMFGETNO(I,3),2)
            FLOWVALS(I+1)=FLOWVALS(I+1)*3600.0/SUPNOD(IMFGETNO(I,3),2)
            NEXT=1
          endif
        elseif (IMFGETNO(I,1).eq.6) then

C Connection net fluid speed (m/s).  Depending on IMFGETNO(I,4)
C return +ive, -ive or net flow.
          call MASS2VOL(IMFGETNO(I,2),VFLOW1,VFLOW2)
          if (IMFGETNO(I,4).gt.0) then
            FLOWVALS(I)=VFLOW1
          elseif (IMFGETNO(I,4).lt.0) then
            FLOWVALS(I)=VFLOW2
          else
            FLOWVALS(I)=VFLOW1+VFLOW2
          endif
          icompindex=ITPCON(IMFGETNO(I,2))
          icomptype=ITPCMP(icompindex)

C Debug.
C          write(6,*) 'the component index is ',icompindex
C          write(6,*) 'the component type is ',icomptype

          if(icomptype.eq.40)then
            area=SUPCMP(icompindex,2)
          elseif(icomptype.eq.50)then
            PI = 4.0 * ATAN(1.0)
            area= PI * (SUPCMP(icompindex,3)*SUPCMP(icompindex,3))
          elseif(icomptype.eq.110)then
            area=SUPCMP(icompindex,2)
          elseif(icomptype.eq.120)then
            area=SUPCMP(icompindex,2)*SUPCMP(icompindex,3)
          elseif(icomptype.eq.130)then
            area=SUPCMP(icompindex,2)*SUPCMP(icompindex,3)
          elseif(icomptype.eq.210.or.icomptype.eq.220.or.
     &           icomptype.eq.230.or.icomptype.eq.240.or.
     &           icomptype.eq.250.or.icomptype.eq.460)then
            area=SUPCMP(icompindex,3)
          elseif(icomptype.eq.420)then
            area=SUPCMP(icompindex,2)
          else
            area=1.0
          endif

C Debug.
C          write(6,*) 'area of component type ',icomptype,' is ',area

          FLOWVALS(I)=FLOWVALS(I)/area
        elseif (IMFGETNO(I,1).eq.7) then

C Node pressure difference.
          IC1=NODPS(IMFGETNO(I,2))
          IC2=NODNE(IMFGETNO(I,2))
          FLOWVALS(I)=FRDAT(1,IC1)-FRDAT(1,IC2)
        elseif (IMFGETNO(I,1).eq.8) then

C Connection stack pressure.
          FLOWVALS(I)=FRDAT(2+NCONTM+IMFGETNO(I,2),1)
        elseif (IMFGETNO(I,1).eq.9) then

C Ambient temperature.
          IPAR=1
          call MFCLIM(IDAY,IHR,ITS,IPAR,VAL)
          FLOWVALS(I)=VAL
        elseif (IMFGETNO(I,1).eq.10) then

C Ambient wind speed.
          IPAR=2
          call MFCLIM(IDAY,IHR,ITS,IPAR,VAL)
          FLOWVALS(I)=VAL
        elseif (IMFGETNO(I,1).eq.11) then

C Ambient wind direction.
          IPAR=3
          call MFCLIM(IDAY,IHR,ITS,IPAR,VAL)
          FLOWVALS(I)=VAL
        elseif (IMFGETNO(I,1).eq.12) then

C Ambient RH.
          IPAR=4
          call MFCLIM(IDAY,IHR,ITS,IPAR,VAL)
          FLOWVALS(I)=VAL
        elseif (IMFGETNO(I,1).eq.14) then

C Contaminant concentration
          INOD=IMFGETNO(I,2)
          ICONTM=IMFGETNO(I,3)
          FLOWVALS(I)=FRDAT(2+ICONTM,INOD)
        endif
 20   continue

 8    RETURN
 99   write (outs,'(a,i6)') 'GOMFGET: error at record',IREC
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8
      END

C ************************ MASS2VOL ************************
C MASS2VOL: Converts mass flow rate to volume flow rate (kg/s) -> (m^3/s)

      SUBROUTINE MASS2VOL(ICON,VFLOW1,VFLOW2)
#include "net_flow.h"

      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      COMMON/MFREC/FRDAT(MCNN+2+MCONTM,NMRL)
      CHARACTER CONTMNAM*12

C Depending on flow direction calc density for sending node temperature.
      IFLD=NDFLD(NODPS(ICON))
      TPOS=FRDAT(2,NODPS(ICON))
      TNEG=FRDAT(2,NODNE(ICON))
      FLOW1=FRDAT(2+ICON+NCONTM,2)
      if (FLOW1.gt.0.0) then
        VFLOW1=FLOW1/DENSIT(IFLD,TPOS)
      else
        VFLOW1=FLOW1/DENSIT(IFLD,TNEG)
      endif
      FLOW2=FRDAT(2+ICON,3)
      if (FLOW2.gt.0.0) then
        VFLOW2=FLOW2/DENSIT(IFLD,TPOS)
      else
        VFLOW2=FLOW2/DENSIT(IFLD,TNEG)
      endif

      return
      end


C ************************ MFSUMCON ************************
C MFSUMCON: Total mass flow through set of connections.
C  if IMFGETNO(I,4) is greater than 1 then it points to a list of connections
C  if IMFGETNO(I,4) is less than zero then this is a node and all valid
C    connections should be counted.
C IUNIT: 1=kg/s; 2=m^3/s; 3=ac/h

      SUBROUTINE MFSUMCON(IG,IUNIT,FLIN,FLOUT)
#include "net_flow.h"

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      COMMON/MFREC/FRDAT(MCNN+2+MCONTM,NMRL)
      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,7)
      COMMON/MFPK2/NMFLST,IMFLIST(MFCNLST,MFCNEL+1)

      logical LNPS,LNNE
      CHARACTER CONTMNAM*12

      LNPS=.true.
      LNNE=.true.

C Reset flow in and out variables.
      FLIN=0.
      FLOUT=0.

C Connection list (IMFGETNO(IG,4) is pointer to list).
C All connections must have one node in common.
      if (IMFGETNO(IG,4).gt.0) then

C Find common node.
        ICNN=IMFLIST(IMFGETNO(IG,4),2)
        IDPS1=NODPS(ICNN)
        IDNE1=NODNE(ICNN)
        do 5 IP=2,IMFLIST(IMFGETNO(IG,4),1)
          ICNN=IMFLIST(IMFGETNO(IG,4),IP+1)
          IDPS2=NODPS(ICNN)
          IDNE2=NODNE(ICNN)
          if (IDPS2.ne.IDPS1.and.IDNE2.ne.IDPS1) then
            LNPS=.false.
          endif
          if (IDPS2.ne.IDNE1.and.IDNE2.ne.IDNE1) then
            LNNE=.false.
          endif
 5      continue
        if (LNPS.and.(.not.LNNE)) then
          NODE=IDPS1
        elseif (LNNE.and.(.not.LNPS)) then
          NODE=IDNE1
        else

C Debug.
C          write(6,*) 'No common node for connection list.'

          return
        endif

C Loop through connections list.
        do 10 IP=1,IMFLIST(IMFGETNO(IG,4),1)
          ICNN=IMFLIST(IMFGETNO(IG,4),IP+1)
          IDPS=NODPS(ICNN)
          IDNE=NODNE(ICNN)

C Get data for current connection.
          if (IUNIT.eq.1) then
            IROW=2+NCONTM+ICNN
            FLOW1=FRDAT(IROW,2)
            FLOW2=FRDAT(IROW,3)
          elseif (IUNIT.eq.2) then
            call MASS2VOL(ICNN,FLOW1,FLOW2)
          elseif (IUNIT.eq.3) then
            call MASS2VOL(ICNN,FLOW1,FLOW2)
          endif

C Sum flows depending on connection definition (If NODE id the +ive
C or first node then a +ive flow is leaving the node and vice versa).
          if (IDPS.eq.NODE) then
            if (FLOW1.gt.0.0) then
              FLOUT=FLOUT+FLOW1
            else
              FLIN=FLIN-FLOW1
            endif
            if (FLOW2.gt.0.0) then
              FLOUT=FLOUT+FLOW2
            else
              FLIN=FLIN-FLOW2
            endif
          elseif (IDNE.eq.NODE) then
            if (FLOW1.gt.0.0) then
              FLIN=FLIN+FLOW1
            else
              FLOUT=FLOUT-FLOW1
            endif
            if (FLOW2.gt.0.0) then
              FLIN=FLIN+FLOW2
            else
              FLOUT=FLOUT-FLOW2
            endif
          endif

C Debug.
C          write(6,'(3i4,4f8.4)') NODE,IDPS,IDNE,FLOW1,FLOW2,FLIN,FLOUT

10      continue

C Negate FLOUT (the above sum results in positive values for both flows).
        FLOUT=FLOUT*(-1.0)
      elseif(ABS(IMFGETNO(IG,4)).eq.IMFGETNO(IG,3)
     &       .AND.IMFGETNO(IG,2).EQ.0)THEN

C All valid connections to node=-1*IMFGETNO(IG,4)
        NODE=abs(IMFGETNO(IG,4))
        do 30 ICNN=1,NCNN
          IDPS=NODPS(ICNN)
          IDNE=NODNE(ICNN)

C Get data for current connection.
          if (IUNIT.eq.1) then
            IROW=2+NCONTM+ICNN
            FLOW1=FRDAT(IROW,2)
            FLOW2=FRDAT(IROW,3)
          elseif (IUNIT.eq.2) then
            call MASS2VOL(ICNN,FLOW1,FLOW2)
          elseif (IUNIT.eq.3) then
            call MASS2VOL(ICNN,FLOW1,FLOW2)
          endif

C Sum flows depending on connection definition (If NODE id the +ive
C or first node then a +ive flow is leaving the node and vice versa).
          if (IDPS.eq.NODE) then
            if (FLOW1.gt.0.0) then
              FLOUT=FLOUT+FLOW1
            else
              FLIN=FLIN-FLOW1
            endif
            if (FLOW2.gt.0.0) then
              FLOUT=FLOUT+FLOW2
            else
              FLIN=FLIN-FLOW2
            endif
          elseif (IDNE.eq.NODE) then
            if (FLOW1.gt.0.0) then
              FLIN=FLIN+FLOW1
            else
              FLOUT=FLOUT-FLOW1
            endif
            if (FLOW2.gt.0.0) then
              FLIN=FLIN+FLOW2
            else
              FLOUT=FLOUT-FLOW2
            endif
          endif
 30     continue
      elseif(ABS(IMFGETNO(IG,4)).eq.IMFGETNO(IG,3)
     &       .AND.IMFGETNO(IG,2).EQ.-1)THEN

C All valid connections to node=-1*IMFGETNO(IG,4) to/from ambient
        NODE=abs(IMFGETNO(IG,4))
        do 301 ICNN=1,NCNN
          IDPS=NODPS(ICNN)
          IDNE=NODNE(ICNN)

C Get data for current connection.
          if (IUNIT.eq.1) then
            IROW=2+NCONTM+ICNN
            FLOW1=FRDAT(IROW,2)
            FLOW2=FRDAT(IROW,3)
          elseif (IUNIT.eq.2) then
            call MASS2VOL(ICNN,FLOW1,FLOW2)
          elseif (IUNIT.eq.3) then
            call MASS2VOL(ICNN,FLOW1,FLOW2)
          endif

C Sum flows depending on connection definition (If NODE id the +ive
C or first node then a +ive flow is leaving the node and vice versa).
          if (IDPS.eq.NODE.AND.NDTYP(IDNE).GE.2) then
            if (FLOW1.gt.0.0) then
              FLOUT=FLOUT+FLOW1
            else
              FLIN=FLIN-FLOW1
            endif
            if (FLOW2.gt.0.0) then
              FLOUT=FLOUT+FLOW2
            else
              FLIN=FLIN-FLOW2
            endif
          elseif(IDNE.eq.NODE.AND.NDTYP(IDPS).GE.2)then
            if (FLOW1.gt.0.0) then
              FLIN=FLIN+FLOW1
            else
              FLOUT=FLOUT-FLOW1
            endif
            if (FLOW2.gt.0.0) then
              FLIN=FLIN+FLOW2
            else
              FLOUT=FLOUT-FLOW2
            endif
          endif
 301    continue
      elseif(IMFGETNO(IG,4).lt.0.and.IMFGETNO(IG,3).gt.0.and.
     &       IMFGETNO(IG,2).EQ.-2)THEN

C Net flows between nodes -1*(IMFGETNO(IG,4) and (IMFGETNO(IG,3)
        NODE=abs(IMFGETNO(IG,4))
        JNOD=ABS(IMFGETNO(IG,3))
        do 351 ICNN=1,NCNN
          IDPS=NODPS(ICNN)
          IDNE=NODNE(ICNN)

C Get data for current connection.
          if (IUNIT.eq.1) then
            IROW=2+NCONTM+ICNN
            FLOW1=FRDAT(IROW,2)
            FLOW2=FRDAT(IROW,3)
          elseif (IUNIT.eq.2) then
            call MASS2VOL(ICNN,FLOW1,FLOW2)
          elseif (IUNIT.eq.3) then
            call MASS2VOL(ICNN,FLOW1,FLOW2)
          endif

C Sum flows depending on connection definition (If NODE id the +ive
C or first node then a +ive flow is leaving the node and vice versa).
          if (IDPS.eq.NODE.AND.IDNE.EQ.JNOD) then
            if (FLOW1.gt.0.0) then
              FLOUT=FLOUT+FLOW1
            else
              FLIN=FLIN-FLOW1
            endif
            if (FLOW2.gt.0.0) then
              FLOUT=FLOUT+FLOW2
            else
              FLIN=FLIN-FLOW2
            endif
          elseif(IDNE.eq.NODE.AND.IDPS.EQ.JNOD)then
            if (FLOW1.gt.0.0) then
              FLIN=FLIN+FLOW1
            else
              FLOUT=FLOUT-FLOW1
            endif
            if (FLOW2.gt.0.0) then
              FLIN=FLIN+FLOW2
            else
              FLOUT=FLOUT-FLOW2
            endif
          endif
 351    continue
      endif

C Debug.
C         write(6,'(2F8.4)') FLIN,FLOUT

      return
      end


C ************************ MFCLIM ************************
C MFCLIM: returns requested climate parameter for given timestep.

      SUBROUTINE MFCLIM(IDAY,IHR,ITS,IPAR,VAL)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      character outs*124

      DIMENSION IDIF(MT+1),ITMP(MT+1),IDNR(MT+1),
     &          IVEL(MT+1),IDIR(MT+1),IHUM(MT+1)

C Access climate file to recover climate data for requested timestep
      IUNITC=IFIL+5
      IREC=IDAY+ISDS
      if (IREC.gt.365) IREC=1
      READ(IUNITC,REC=IREC,IOSTAT=ISTAT,ERR=1000)(IDIF(J),ITMP(J),
     &IDNR(J),IVEL(J),IDIR(J),IHUM(J),J=1,24)
      IDIF(25)=IDIF(1)
      ITMP(25)=ITMP(1)
      IDNR(25)=IDNR(1)
      IVEL(25)=IVEL(1)
      IDIR(25)=IDIR(1)
      IHUM(25)=IHUM(1)
      IREC=IDAY+ISDS-1
      READ(IUNITC,REC=IREC,IOSTAT=ISTAT,ERR=1000)(IDIF(J),ITMP(J),
     &IDNR(J),IVEL(J),IDIR(J),IHUM(J),J=1,24)

C Set interpolation factor.
      XFAC=real(ITS-1)/real(NTS)

C Get required data item.
      if (IPAR.eq.1) then
        VAL=real(ITMP(IHR))+XFAC*(real(ITMP(IHR+1))-real(ITMP(IHR)))
        VAL=VAL/10.
      elseif (IPAR.eq.2) then
        VAL=real(IVEL(IHR))+XFAC*(real(IVEL(IHR+1))-real(IVEL(IHR)))
        VAL=VAL/10.
      elseif (IPAR.eq.3) then
        VAL=real(IDIR(IHR))+XFAC*(real(IDIR(IHR+1))-real(IDIR(IHR)))
      elseif (IPAR.eq.4) then
        VAL=real(IHUM(IHR))+XFAC*(real(IHUM(IHR+1))-real(IHUM(IHR)))
      endif

 16   return

 1000 write (outs,'(a,i5)') 'MFCLIM: climate file error at record',IREC
      CALL edisp(iuout,outs)
      goto 16

      end

C ************************ EDMFGOGET ************************
C EDMFGOGET allows removal of output requests for flow networks.
      SUBROUTINE EDMFGOGET
#include "epara.h"
#include "building.h"
#include "net_flow.h"

      common/pophelp/h(60)
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,7)

C smflabel char*32 is the topic of a flow item
C gmflabel char*20 is short label for graphs/tables for flow item
C glmflabel char*32 is longer label for flow item
      COMMON/MFLABEL/SMFLABEL(MFRS),GMFLABEL(MFRS),GLMFLABEL(MFRS)

      character VERT(MZS+3)*36,KEY*1,h*72
      character SMFLABEL*32,GMFLABEL*20,GLMFLABEL*32
      integer ORDER,OGET
      dimension INCGET(MFRS),ORDER(MFRS),OGET(MFRS)
      integer MVERT,IVERT ! max items and current menu item

C Sort ORDER of IMFGETNO array for display purposes.
      do 100 I=1,NMFGET
        ORDER(I)=I
        OGET(I)=IMFGETNO(I,1)*10000 + IMFGETNO(I,2)*100 + IMFGETNO(I,3)
 100  continue
      call SORTI(OGET,ORDER,NMFGET,2)

C Set all values of incget to 1
      do 5 I=1,NMFGET
        INCGET(I)=1
 5    continue

C Initialise menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=1
      MCTL=3
      ILEN=NMFGET
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
      IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the
C current menu line index. Build up text strings for the menu.
      VERT(1)='  Mertic                  Active'
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if (INCGET(ORDER(L)).eq.0) then
            WRITE(VERT(M),'(a,1x,2a)')KEY,GLMFLABEL(ORDER(L)),'  '
          else
            WRITE(VERT(M),'(a,1x,2a)')KEY,GLMFLABEL(ORDER(L)),' *'
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ------------------------ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ---')
      ENDIF
      VERT(M+2)  ='? help                     '
      VERT(M+3)  ='- exit and activate changes'

C Instanciate h() strings for this menu.
      H(1)='This menu allows existing metric choices to be '
      H(2)='removed from the list to display. '
      H(3)='Each item can be toggled on/ off until you are'
      H(4)='happy with the list as all items which are marked'
      H(5)='off are removed from the list when you exit.'

C Display menu.
      CALL EMENU('Metric selection',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C Remove inactive elements.
 21     do 20 I=1,NMFGET
          if (INCGET(I).eq.0) then
            do 30 K=I,NMFGET
              if (K.lt.NMFGET) then
                IMFGETNO(K,1)=IMFGETNO(K+1,1)
                IMFGETNO(K,2)=IMFGETNO(K+1,2)
                IMFGETNO(K,3)=IMFGETNO(K+1,3)
                IMFGETNO(K,4)=IMFGETNO(K+1,4)
                IMFGETNO(K,5)=IMFGETNO(K+1,5)
                IMFGETNO(K,6)=IMFGETNO(K+1,6)
                IMFGETNO(K,7)=IMFGETNO(K+1,7)
                SMFLABEL(K)=SMFLABEL(K+1)
                GMFLABEL(K)=GMFLABEL(K+1)
                GLMFLABEL(K)=GLMFLABEL(K+1)
                INCGET(K)=INCGET(K+1)
              else
                IMFGETNO(K,1)=0
                IMFGETNO(K,2)=0
                IMFGETNO(K,3)=0
                IMFGETNO(K,4)=0
                IMFGETNO(K,5)=ISIM
                IMFGETNO(K,6)=1
                IMFGETNO(K,7)=0
                SMFLABEL(K)='  '
                GMFLABEL(K)='  '
                GLMFLABEL(K)='  '
                INCGET(K)=1
              endif
 30         continue
            NMFGET=NMFGET-1
            goto 21
          endif
 20     continue
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        CALL PHELPD('metric selection',5,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Mark connection identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        INCGET(ORDER(IFOC))=INCGET(ORDER(IFOC))+1
        if (INCGET(ORDER(IFOC)).gt.1) INCGET(ORDER(IFOC))=0
        if (INCGET(ORDER(IFOC)).eq.1) then

C Debug.
C          write(6,*) 'selected ifoc order(ifoc) incget(()) ',ifoc,
C     &      order(ifoc),INCGET(ORDER(IFOC)),SMFLABEL(ORDER(ifoc))

        endif
      endif
      goto 3

      end
