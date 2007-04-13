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

C This file contains the following routine:
C    plttab: handles plant results view/setup requests
C    selpc: displays a selection list of plant components
C    opnplt: open and scan the plant database for names etc.
C    dbdat: read additional output for a component.
C    fnddat: finds IPC data for given time
C    mosyssu: clears commons/ sets default values for systems plotting.
C    SYSOUTP: display performance (tabular/graphic/stats) of components.

C ******************** PLTTAB ********************
C PLTTAB handles results view/setup requests for plant.
      subroutine plttab
#include "plant.h"
      PARAMETER (MSYSRS=20)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/DEFLT/IDEFLT
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      common/getmenu/menutype,igetind(65),igetflux(65)
C MENUTYPE 1: not used
C MENUTYPE 2: Frequency histogram (not yet working for systems)
C MENUTYPE 3: Time series plot (not yet working for systems)
C MENUTYPE 4: Performance metrics
C MENUTYPE 5: Summary statistics (not yet working for systems)
C MENUTYPE 6: Sensitivity ( ?? )
C MENUTYPE 7: Hours above query point (not yet working for systems)
C MENUTYPE 8: Hours below query point (not yet working for systems)
C MENUTYPE 9: Psychrometric chart

C IPGET = 1 Temperature deg C
C IPGET = 2 1st flow kg/s
C IPGET = 3 2nd flow kg/s
C IPGET = 4 RH %
C IPGET = 5 SH kgv/kga
C IPGET = 6 Enthalpy KJ/Kg
C IPGET = 7 Dew ptT deg C
C IPGET = 9 Ambient temperature
C IPGET = 10 Additional output
C IPGET = 12 Ambient RH

C Up to MSYSRS data can be plotted/listed.
C IPGETNO contains all the information needed for specifying which data to recover
C IPGETNO(*,1): The IPGET number - metric identifier
C << need to think of what indices are what >>
C IPGETNO(*,2): The chosen component (meaning assumed from IPGET) in the
C                case of climate data it gets used to set graphing colour.
C IPGETNO(*,3): The chosen node
C IPGETNO(*,4): ?
C IPGETNO(*,5): Result set number 
C IPGETNO(*,6): Line type
C IPGETNO(*,7): Symbol index
C IPGETNO(*,8): Associated axis (1=temperature, 2=kg/s, 3=%, 4=KJ/Kg
C               5=misc ??)
      COMMON/MSYSPICK/NPGET,IPGETNO(MSYSRS,8)
      common/pcnam/pcname(mpcom)
      common/adopdesc/adopdsc(mpcom,15)
      common/pcres/ napout(mpcom),apcout(20)
      common/pcdat/ CSV1M(mnodec),CSV2M(mnodec),CSV3M(mnodec),
     &              NNODE(mpcom),nrecpc(mpcom)
      common/ntype/isv(mpcom,mnodec)
      COMMON/SYSLABEL/SPLABEL(MSYSRS),GPLABEL(MSYSRS)

      dimension mcomp(mpcom), ipcndx(mpcom), ivals(15)
      CHARACTER ITEM(26)*23
      character outs*124,xfile*144,tg*1,delim*1,MTITLE*22,h*72
      character pcname*15,pcnam1(mpcom)*15,adopdsc*30
      character aoutstr(15)*30
      character SPLABEL*32,GPLABEL*20,SUFFIX*6,PREFIX*14

C Default value for GRTOOL file hourly flag = 0 (off).
      IHFLAG=0

C Initialise variables
      ipc=1
      istat=0

C MNGET is the maximum number of metrics allowed to be chosen from the menu.
   7  if (MENUTYPE.eq.1) then
        return
      elseif (MENUTYPE.eq.2) then
        MTITLE='Frequency histogram'
        MNGET=1
      elseif (MENUTYPE.eq.3) then
        MTITLE='Time series plot'
        MNGET=MSYSRS-1
      elseif (MENUTYPE.eq.4) then
        MTITLE='Timestep listing'
        MNGET=13
      elseif (MENUTYPE.eq.5) then
        MTITLE='Summary statistics'
        MNGET=MSYSRS-1
      elseif (MENUTYPE.eq.6) then
        MTITLE='Sensitivity'
        MNGET=1
      elseif (MENUTYPE.eq.7) then
        MTITLE='Hrs above query point'
        MNGET=MSYSRS-1
      elseif (MENUTYPE.eq.8) then
        MTITLE='Hrs below query point'
        MNGET=MSYSRS-1
      elseif (MENUTYPE.eq.9) then
        MTITLE='Psychrometric'
        MNGET=1
      else
        MNGET=1
        MTITLE='  '
      endif

C Setup menu text.
      ITEM(1)  ='2 result set    '
      ITEM(2)  ='3 display period '
      ITEM(3)  ='  __________________   '
      if(MENUTYPE.eq.1)then
       ITEM(4) ='5 output style        '
      elseif(MENUTYPE.eq.2)then
       ITEM(4) ='5 output >> histogram '
      elseif(MENUTYPE.eq.3)then
       ITEM(4) ='5 output >> graph     '
      elseif(MENUTYPE.eq.4)then
       ITEM(4) ='5 output >> tabular   '
      elseif(MENUTYPE.eq.5)then
       ITEM(4) ='5 output >> statistics'
      elseif(MENUTYPE.eq.6)then
       ITEM(4) ='5 output: sensitivity '
      elseif(MENUTYPE.eq.7)then
       ITEM(4) ='5 output >> hours over'
      elseif(MENUTYPE.eq.8)then
       ITEM(4) ='5 output >> hrs below '
      elseif(MENUTYPE.eq.9)then
       ITEM(4) ='5 output >> psychromtrc'
      endif
      ITEM(5)  ='  __________________   '
      item(6)  =' node related...       '
      if(MENUTYPE.eq.9)then
        item(7)  ='a -                    '
        item(8)  ='b -                    '
        item(9)  ='c -                    '
        item(10) ='d -                    '
        item(11) ='e -                    '
        item(12) ='f -                    '
        item(13) ='g -                    '
        item(14) ='  ____________________ '
        item(15) =' special...            '
        item(16) ='h -                    '
        item(17) ='i -                    '
        item(18) ='j -                    '
      else
        item(7)  ='a temperature          '
        item(8)  ='b 1st phase flow       '
        item(9)  ='c 2nd phase flow       '
        item(10) ='d relative humidity%   '
        item(11) ='e specific humidity    '
        item(12) ='f enthalpy             '
        item(13) ='g dew point temperature'
        item(14) ='  ____________________ '
        item(15) =' special...            '
        item(16) ='h additional output    '
        item(17) ='i outside dry bulb T   '
        item(18) ='j ambient RH%          '
      endif
      ITEM(19) ='  __________________   '
      if(ixopen.eq.1)then
        ITEM(20)='> output >> file       '
      elseif(ixopen.eq.0)then
        ITEM(20)='> output >> screen     '
      endif
      if(IHFLAG.eq.0)then
        ITEM(21)='* format >> 10h30      '
      elseif(IHFLAG.eq.1)then
        ITEM(21)='* format >> 0.4375     '
      endif

C << Place toggle for ILFLAG here and revise menu >>
      if(delim.eq.'-')then
        ITEM(22)='^ delim >> normal      '
      elseif(delim.eq.'T')then
        ITEM(22)='^ delim >> TAB         '
      elseif(delim.eq.'C')then
        ITEM(22)='^ delim >> comma       '
      elseif(delim.eq.'S')then
        ITEM(22)='^ delim >> space       '
      elseif(delim.eq.'X')then
        ITEM(22)='^ delim >> tagged      '
      endif
      if(MENUTYPE.eq.3) then
        ITEM(23)='! draw graph           '
      elseif(MENUTYPE.eq.4)then
        ITEM(23)='! list data            '
      elseif(MENUTYPE.eq.9)then
        ITEM(23)='! generate chart       '
      else
        ITEM(23)='! generate reports     '
      endif
      ITEM(24)  ='/ clear selections     '
      ITEM(25)  ='? help                 '
      ITEM(26)  ='- exit                 '

C Display menu.
      NITMS=26
    2 INO=-2
    5 continue

C Instanciate the h() strings for this menu.
      h(1) ='The viewing of plant system component performance'
      h(2) ='is controlled by this menu. '
      h(3) =' '
      h(4) =' 3 - set the display period and timestep. '
      h(5) =' 5 - toggle output format between frequency bins,'
      h(6) ='     timestep listings, time series graphs, statistics,'
      h(7) ='     statistics, psychrometric chart.'
      h(8) =' '
      h(9) ='Node related metrics - selection presents a list of'
      h(10)='components that include the metric. Repeat selections'
      h(11)='as required. '
      h(12)=' '
      h(13)='Special metrics - components may include additional'
      h(14)='performance data such and cooling or heating at a node.'
      h(15)='You are expected to know what these data are (minimal '
      h(16)='assistance is provided). '
      h(17)=' '
      H(18)=' > Display can be redirected to a file or the screen.'
      H(19)='  '
      H(20)=' * Time is displayed as hours and minutes (hh,mm) or'
      H(21)='   as a decimal (Day of year.Fraction of day).'
      H(22)='  '
      H(23)=' ^ Delimiters in tabular output can be changed to tab'
      H(24)='   or space or comma or tag or fixed format as required.'
      H(25)=' ! produce graph or report or listing.'
      H(26)='  '
      H(27)='  / clear - reset previous seletions.'

      CALL EMENU(MTITLE,ITEM,NITMS,INO)

C If separator or blank choice loop back again.
      if(ino.EQ.1.or.ino.EQ.3.or.ino.EQ.5.or.ino.EQ.6.or.ino.EQ.14.or.
     &   ino.EQ.15.or.ino.EQ.19.or.ino.eq.0) goto 2

C If one of the fixed items do what is required.
      if(INO.EQ.2)then
        ideflt=0
        call mooper
        goto 7
      elseif(INO.EQ.4)then
        CALL MOSTYLE
        if(menutype.eq.9)then
          call edisp(iuout,' ')
          call edisp(iuout,'In psychrometric mode all that you can')
          call edisp(iuout,'choose is option `! generate chart`.')
        endif
        goto 7
      elseif(INO.EQ.20)then

C Output redirect.
        if(MENUTYPE.eq.3)then
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'G','Graphics',IER)
        elseif(MENUTYPE.eq.4)then
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'X','Tabular',IER)
        else
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'T','Text',IER)
        endif
        goto 7
      elseif(INO.EQ.21)then

C Display hours.
        IHFLAG=IHFLAG+1
        if(IHFLAG.GT.1)IHFLAG=0
        if(IHFLAG.eq.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'standard display time = 10h30 set ')
        elseif(IHFLAG.eq.1)then
          call edisp(iuout,' ')
          call edisp(iuout,'3rd party graphing time = day fraction set')
        endif
        goto 7
      elseif(INO.EQ.22)then

C Toggle delimeter.
        H(1) ='Tabular data can be sent to file with various '
        H(2) ='delimiters: '
        H(3) ='  spaces (format using spaces to lineup columns)'
        H(4) ='  single space between columns'
        H(5) ='  comma separator (for excel)'
        H(6) ='  tab separator (for excel)'
        H(7) ='  tagged - marked up data'
        CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &    'normal spaces','single space','tab','comma','tagged',
     &    'continue',' ',IWM,7)
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
        goto 7
      ELSEIF(INO.EQ.23)THEN

C List or draw what has been selected.
        return
      ELSEIF(INO.EQ.24)THEN

C Clear selections.
        call mosyssu
        call edisp(iuout,'Previous selections cleared.')
        goto 7
      ELSEIF(INO.EQ.25)THEN

C Help.
        CALL PHELPD('systems tabular data',27,'-',0,0,IER)
      ELSEIF(INO.EQ.26)THEN

C To exit without drawing anything set menutype to -1 and return.
        call usrmsg('  ','  ','-')
        MENUTYPE=-1
        return
      endif

C If this point reached then must have selected one of the metrics.
      if(ino.gt.6.and.ino.lt.14) then

C Consider only the components which support the selected variable 
C (i.e. moist air)
        if(ino.eq.9.or.ino.eq.10.or.ino.eq.11.or.ino.eq.13) then
          k=0
          do 30 i=1,npcomp
            do 25 j=1, nnode(i)
              if(isv(i,j).eq.21) then
                k=k+1
                pcnam1(k)=pcname(i)
                ipcndx(k)=i
              endif
  25        continue
  30      continue
        endif

C Consider only the components which support the selected variable 
C (i.e. dry air, moist air and water)
        if(ino.eq.8.or.ino.eq.12) then
          k=0
          do 40 i=1,npcomp
            do 35 j=1, nnode(i)
              if(isv(i,j).eq.0.or.isv(i,j).eq.10.or.
     &           isv(i,j).eq.20.or.isv(i,j).eq.1.or.
     &           isv(i,j).eq.11.or.isv(i,j).eq.21) then
                k=k+1
                pcnam1(k)=pcname(i)
                ipcndx(k)=i
              endif
  35        continue
  40      continue
        endif

C Consider only the components which support temperature 
C (i.e. all components)
        if(ino.eq.7) then
          do 50 i=1,npcomp
            pcnam1(i)=pcname(i)
            ipcndx(i)=i
            k=i
  50      continue
        endif

C Now display components menu
        ipc=1
        call selpc(ipc,mcomp,pcnam1,k)
        if(ipc.eq.0) goto 5
        indx=ipcndx(mcomp(1))
        mcomp(1)=indx
        ipcn =nnode(mcomp(1))


c If selected component has more than one node, then get desired node.
        if(ipcn.gt.1) then
          write(outs,*)' Node number for component  ',
     &      pcname(mcomp(1)),' ?'
          CALL EASKI(nsnd,outs,' ',
     &      1,'F',ipcn,'F',1,'node number',IER,1)
        elseif(ipcn.eq.1) then
          nsnd=1
        endif

C Save selected component and node numbers.
        if((NPGET+1).GT.MNGET)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 7
        endif
        NPGET=NPGET+1
        lname=MIN0(lnblnk(pcname(indx)),11)
        write(PREFIX,'(a)') pcname(indx)(1:lname)

C Now save information for the specific selection.
        if(INO.EQ.7)then

C Temperature (IPGET = 1 ).
          IPGETNO(NPGET,1)=1
          IPGETNO(NPGET,2)=indx
          IPGETNO(NPGET,3)=nsnd
          IPGETNO(NPGET,6)=-302
          IPGETNO(NPGET,7)=1
          IPGETNO(NPGET,8)=1

C Build string for display - the prefix being up to 11 characters
C of the component name and then the node index and then a suffix.
          SUFFIX=' degC'
          if(nsnd.le.9)then
            write(GPLABEL(NPGET),'(2a,i1,a)') PREFIX(1:lname),' n',
     &        nsnd,SUFFIX
          else
            write(GPLABEL(NPGET),'(2a,i2,a)') PREFIX(1:lname),'n',
     &        nsnd,SUFFIX
          endif
          write(outs,'(2a)') 'Selected ',GPLABEL(NPGET)
          call edisp(iuout,outs)
          SPLABEL(NPGET)='Node temperature (degC)'
        ELSEIF(INO.EQ.8)THEN

C First phase flow (IPGET = 2).
          IPGETNO(NPGET,1)=2
          IPGETNO(NPGET,2)=indx
          IPGETNO(NPGET,3)=nsnd
          IPGETNO(NPGET,6)=-203
          IPGETNO(NPGET,7)=6
          IPGETNO(NPGET,8)=2
          SUFFIX=' kg/s1'
          if(nsnd.le.9)then
            write(GPLABEL(NPGET),'(2a,i1,a)') PREFIX(1:lname),' n',
     &        nsnd,SUFFIX
          else
            write(GPLABEL(NPGET),'(2a,i2,a)') PREFIX(1:lname),'n',
     &        nsnd,SUFFIX
          endif
          write(outs,'(2a)') 'Selected ',GPLABEL(NPGET)
          call edisp(iuout,outs)
          SPLABEL(NPGET)='1st phase flow (kg/s)'
        ELSEIF(INO.EQ.9)THEN

C Second phase flow(IPGET = 3).
          IPGETNO(NPGET,1)=3
          IPGETNO(NPGET,2)=indx
          IPGETNO(NPGET,3)=nsnd
          IPGETNO(NPGET,6)=-203
          IPGETNO(NPGET,7)=4
          IPGETNO(NPGET,8)=2
          SUFFIX=' kg/s2'
          if(nsnd.le.9)then
            write(GPLABEL(NPGET),'(2a,i1,a)') PREFIX(1:lname),' n',
     &        nsnd,SUFFIX
          else
            write(GPLABEL(NPGET),'(2a,i2,a)') PREFIX(1:lname),'n',
     &        nsnd,SUFFIX
          endif
          write(outs,'(2a)') 'Selected ',GPLABEL(NPGET)
          call edisp(iuout,outs)
          SPLABEL(NPGET)='2nd phase flow (kg/s)'
        ELSEIF(INO.EQ.10)THEN

C Relative humidity(IPGET=4).
          IPGETNO(NPGET,1)=4
          IPGETNO(NPGET,2)=indx
          IPGETNO(NPGET,3)=nsnd
          IPGETNO(NPGET,6)=-202
          IPGETNO(NPGET,7)=7
          IPGETNO(NPGET,8)=3
          SUFFIX=' %'
          if(nsnd.le.9)then
            write(GPLABEL(NPGET),'(2a,i1,a)') PREFIX(1:lname),' n',
     &        nsnd,SUFFIX
          else
            write(GPLABEL(NPGET),'(2a,i2,a)') PREFIX(1:lname),'n',
     &        nsnd,SUFFIX
          endif
          write(outs,'(2a)') 'Selected ',GPLABEL(NPGET)
          call edisp(iuout,outs)
          SPLABEL(NPGET)='Relative humidity (%)'
        ELSEIF(INO.EQ.11)THEN

C Specific humidity(IPGET = 5).
          IPGETNO(NPGET,1)=5
          IPGETNO(NPGET,2)=indx
          IPGETNO(NPGET,3)=nsnd
          IPGETNO(NPGET,6)=-102
          IPGETNO(NPGET,7)=1
          IPGETNO(NPGET,8)=4
          SUFFIX=' kg/kg'
          if(nsnd.le.9)then
            write(GPLABEL(NPGET),'(2a,i1,a)') PREFIX(1:lname),' n',
     &        nsnd,SUFFIX
          else
            write(GPLABEL(NPGET),'(2a,i2,a)') PREFIX(1:lname),'n',
     &        nsnd,SUFFIX
          endif
          write(outs,'(2a)') 'Selected ',GPLABEL(NPGET)
          call edisp(iuout,outs)
          SPLABEL(NPGET)='Specific humidity (kgv/kga)'
        ELSEIF(INO.EQ.12)THEN

C Enthalpy(IPGET = 6).
          IPGETNO(NPGET,1)=6
          IPGETNO(NPGET,2)=indx
          IPGETNO(NPGET,3)=nsnd
          IPGETNO(NPGET,6)=-102
          IPGETNO(NPGET,7)=2
          IPGETNO(NPGET,8)=4
          SUFFIX=' KJ/Kg'
          if(nsnd.le.9)then
            write(GPLABEL(NPGET),'(2a,i1,a)') PREFIX(1:lname),' n',
     &        nsnd,SUFFIX
          else
            write(GPLABEL(NPGET),'(2a,i2,a)') PREFIX(1:lname),'n',
     &        nsnd,SUFFIX
          endif
          write(outs,'(2a)') 'Selected ',GPLABEL(NPGET)
          call edisp(iuout,outs)
          SPLABEL(NPGET)='Enthalpy (KJ/Kg)'
        ELSEIF(INO.EQ.13)THEN

C Dew point temperature(IPGET = 7).
          IPGETNO(NPGET,1)=7
          IPGETNO(NPGET,2)=indx
          IPGETNO(NPGET,3)=nsnd
          IPGETNO(NPGET,6)=-102
          IPGETNO(NPGET,7)=3
          IPGETNO(NPGET,8)=1
          SUFFIX=' degC'
          if(nsnd.le.9)then
            write(GPLABEL(NPGET),'(2a,i1,a)') PREFIX(1:lname),' n',
     &        nsnd,SUFFIX
          else
            write(GPLABEL(NPGET),'(2a,i2,a)') PREFIX(1:lname),'n',
     &        nsnd,SUFFIX
          endif
          write(outs,'(2a)') 'Selected ',GPLABEL(NPGET)
          call edisp(iuout,outs)
          SPLABEL(NPGET)='Dew point T (C)'
        endif
        goto 5

C Additional output.
      elseif(ino.eq.16) then
        k=0
        do 60 i=1,npcomp
          if(napout(i).gt.0) then
            k=k+1
            pcnam1(k)=pcname(i)
            ipcndx(k)=i
          endif
  60    continue

C Now display components menu
  61    ipc=1
        call selpc(ipc,mcomp,pcnam1,k)
        if(ipc.eq.0) goto 5
        indx=ipcndx(mcomp(1))
        mcomp(1)=indx
        naout=napout(mcomp(1))

C Display a menu showing additional output choices for the 
C component.
        do 66 iaout=1,naout
          aoutstr(iaout)=adopdsc(mcomp(1),iaout)
  66    continue
        ix=1 
        nitems=naout
        call epicks(ix,ivals,' ','Choose an additional output',30,
     &    nitems,aoutstr,'additional output',IER,0)
        if(ix.eq.0) goto 61
        nsnd=ivals(1)

C Save selected component and additional output number.
        if((NPGET+1).GT.MNGET)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 7
        endif
        NPGET=NPGET+1

C Additional output.
        IPGETNO(NPGET,1)=10
        IPGETNO(NPGET,2)=indx
        IPGETNO(NPGET,3)=nsnd
        IPGETNO(NPGET,6)=-102
        IPGETNO(NPGET,7)=5
        IPGETNO(NPGET,8)=5
        SUFFIX=' other'
        if(nsnd.le.9)then
          write(GPLABEL(NPGET),'(2a,i1,a)') PREFIX(1:lname),' n',
     &      nsnd,SUFFIX
        else
          write(GPLABEL(NPGET),'(2a,i2,a)') PREFIX(1:lname),'n',
     &      nsnd,SUFFIX
        endif
        write(outs,'(2a)') 'Selected ',GPLABEL(NPGET)
        call edisp(iuout,outs)
        SPLABEL(NPGET)='Other (-)'
        goto 5
      endif

      IF(INO.EQ.17)THEN

C Ambient db T(IPGET = 9).
        if((NPGET+1).GT.MNGET)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 7
        endif
        NPGET=NPGET+1
        IPGETNO(NPGET,1)=9
        IPGETNO(NPGET,2)=0
        IPGETNO(NPGET,3)=0
        IPGETNO(NPGET,6)=-302
        IPGETNO(NPGET,7)=4
        IPGETNO(NPGET,8)=1
        GPLABEL(NPGET)='Ambient T (C)'
        SPLABEL(NPGET)='Ambient Temp (C)'
      ELSEIF(INO.EQ.18)THEN

C Ambient RH(IPGET = 12).
        if((NPGET+1).GT.MNGET)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 7
        endif
        NPGET=NPGET+1
        IPGETNO(NPGET,1)=12
        IPGETNO(NPGET,2)=0
        IPGETNO(NPGET,3)=0
        IPGETNO(NPGET,6)=-202
        IPGETNO(NPGET,7)=7
        IPGETNO(NPGET,8)=3
        GPLABEL(NPGET)='Ambient RH (%)'
        SPLABEL(NPGET)='Ambient RH (%)'
      else
        INO=-1
        goto 5
      ENDIF
      INO=-2
      goto 7

      END

C ******************** SELPC ********************
C SELPC displays a selection list of plant components
C for the user to select from.
      subroutine selpc(mnpc,mcomp,pcnam1,npc)
#include "plant.h"
      common/pophelp/h(60)
      dimension mcomp(mpcom)

      character H*72, pcnam1(mpcom)*15

c Get number of components.
      if(npc.eq.1) then
         mnpc=1
         mcomp(1)=1
      else

c Get number of each component.
   10   H(1)='You may select any number of components in'
        H(2)='any order or all components. '
        CALL EPICKS(mnpc,mcomp,' ',
     &    ' Which component to include: ',15,npc,pcnam1,
     &    ' Select comp',IER,2)
         if(ier.ne.0) goto 10
         if(mnpc.eq.0)return

      endif
      return
      end


C ******************** opnplt ********************
C opnplt opens plant result file and fills header with
C simulation related information. It is passed npc the
C number of plant components used in the network.
      subroutine opnplt(npc,imode,ier)
#include "plant.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C ICLIM,LCLIM (for climate file)
C default file names for databases

      integer igraphiclib  ! external definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/pres/iunitp
      COMMON/FILEP/IFIL
      COMMON/PPERS/IpSD1,IpSM1,IpSD2,IpSM2,IpSDS,IpSDF,NpTS,IpSav
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/DEFLT/IDEFLT
      common/prav/IDAVER
      COMMON/LIBRCP/IRCPLP,nblock
      common/cctlnm/ctldoc,lctlf
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/pcdbdt/ipcrec(mpcom)
      common/rcmd/LCMDFL
      common/pcdat/ CSV1M(mnodec),CSV2M(mnodec),CSV3M(mnodec),
     &              NNODE(mpcom),nrecpc(mpcom)
      common/pcres/ napout(mpcom),apcout(20)


      character DFILE*72,LTMP*144,outs*124,LCMDFL*144,T72*72
      character lctlf*72,ctldoc*248,scname*12,h*72
      logical XST,ok,dok

      IUNITp=ifil+10
      DFILE='plant.res'

      ier=0
    6 call edisp(iuout,' ')
      if(imode.eq.1) then
        if(LCMDFL(1:2).ne.'  '.or.LCMDFL(1:4).ne.'UNKN')then
          ltmp=lcmdfl
        else
          ltmp=' '
        endif
        llt=lnblnk(ltmp)
        h(1)='The predictions from solution of a plant/system '
        h(2)='network are held in a binary results file which'
        h(3)='is separate from zone and flow results. Please '
        h(4)='supply the name (if the field is blank). '

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(ltmp,' Library name ? ',' ',96,DFILE,
     &        'library file',IER,4)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(ltmp,' Library name ? ',' ',124,DFILE,
     &        'library file',IER,4)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(ltmp,' Library name ? ',' ',144,DFILE,
     &        'library file',IER,4)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(ltmp,' Library name ? ',' ',144,DFILE,
     &      'library file',IER,4)
        else
          CALL EASKF(ltmp,' Library name ? ',' ',96,DFILE,
     &      'library file',IER,4)
        endif

        if(LTMP(1:2).eq.'  '.or.LTMP(1:4).eq.'UNKN')goto 6
      else
        ltmp=LCMDFL
      endif
      IER=0
      XST=.false.
      INQUIRE (FILE=LTMP,EXIST=XST)
      IF(XST)THEN

C Set record length of library file.
        nprl = nprecl
        OPEN (iunitp,FILE=LTMP(:lnblnk(LTMP)),ACCESS='DIRECT',
     &    RECL=nprl*4,FORM='UNFORMATTED',STATUS='OLD',IOSTAT=ISTAT)
          write(outs,'(A,I3)')' Opening file with record length',nprl
          call edisp(iuout,outs)
      ELSE

C Try again ?
        dok=.false.
        h(1)='The plant results file was not found in the folder you'
        h(2)='specified or the name you gave. Check folder and file '
        h(3)='name and then retry. A no will return to main menu. '
        write(outs,'(2a)') ' Could not find ',LTMP(1:lnblnk(LTMP))
        CALL ASKOK(outs,' Do you want to try again?',OK,dok,3)
        IF(.NOT.OK) return
        goto  6
      endif

      IREC=1
C Read number of result-set.
      READ(IUNITp,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM
      IF (NSIM.EQ.0) then
          call edisp(iuout,' ')
          call edisp(iuout,' Results not saved.')
          call erpfree(iunitp,istat)
          return
      endif
      ipset=0
      if(nsim.gt.1) then
         write(outs,'(a,I4,a)')' Library contains',nsim,' results sets'
         call edisp(iuout,outs)
  13     ipset=1
         CALL easki(ipset,' ',' Specify required set ? ',
     &      1,'F',nsim,'F',1,'required set',IER,0)
         IF(IER.NE.0) GOTO 13
      else
         ipset=1
      endif
      NSIM=ipset-1
      IREC=NSIM+2

C Get pointer to control record and start of result data set.
      READ(IUNITp,REC=IREC,IOSTAT=ISTAT,ERR=1000)NST,IRCPLP

C Get Number of plant components value.
      IREC=33
      READ(IUNITp,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &   IDUM,NPCOMP
      if(npcomp.ne.npc) then
         write(outs,'(a)')
     &       'Unequal no. comp in result file and configuration file'
         call edisp(iuout,outs)
         ier=1
         return
      endif
      IREC=IREC+1

C Read pointers to components entries in database.
C First find required number of records.
      nrecs=npcomp/nprecl+1

C Initialise first item number and number of items left
C respectively.
      items=1
      iteml=npcomp
      do 11 ir=1, nrecs
         if(iteml.le.nprecl) then
            iteme=npcomp
         else
            iteme=items+nprecl-1
         endif
         read(IUNITp,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                          (ipcrec(I),i=items,iteme)
         IREC=IREC+1
         items=nprecl*ir+1
         iteml=npcomp-items+1
   11 continue

C Read climate file name.
      READ(IUNITp,REC=IREC,IOSTAT=ISTAT,ERR=1000) LCLIM

C Check that this CLIMATE file can still be assigned. << convert >>
   44 IUNITC=IFIL+5
      CALL FPRAND(IUNITC,ISTAT,144,1,LCLIM)
      IF(ISTAT.LT.0)then
C Climate file not found - ask user for name.
        write(outs,'(A,A,A)') ' Referenced climate file ',
     &     LCLIM(1:lnblnk(LCLIM)), ' not found...'
        call edisp(iuout,outs)
        call erpfree(IUNITC,istat)
        H(1)=' A climate file defines the problem boundary'
        H(2)=' condition. The one included in the results libary'
        H(3)=' was not found, type in the correct name.'
        t72=LCLIM
        CALL EASKS(t72,'  Climate file name ?',
     &     '  ',72,DCLIM,'climate file name',IER,3)
        if(t72(1:2).ne.'  '.and.t72(1:4).ne.'UNKN')then
          LCLIM=t72
        endif
        goto 44
      endif

C Read Climate year from the file if not already done
C so when reading the building configuration file. If 
C we have a plant only configuration then the year number
C has to be read. imode=0 - plant only config
      IF(imode.EQ.0) THEN
      IRECC=366
      READ(IUNITC,REC=IRECC,IOSTAT=ISTAT,ERR=999)IYEAR
      END IF

C Read plant component file name.
      IREC=IREC+1
      READ(IUNITp,REC=IREC,IOSTAT=ISTAT,ERR=1000) Lpcdb

C Read control data for simulation.
      IREC=NST
      write(scname,'(a)')CTLDOC(1:12)
      READ(IUNITp,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &   IpSD1,IpSM1,IpSD2,IpSM2,IpSDS,IpSDF,NpTS,ipsav,SCNAME,IDAVER
      IREC=IREC+1

C In case of no configuration control file active, skip one record
C to allow match with building library for results recovery.
      IF(CTLDOC(1:4).NE.'NONE'.AND.CTLDOC(1:4).NE.'None'.AND.
     &   CTLDOC(1:4).NE.'none') THEN
         IREC=IREC+1
         READ(IUNITp,REC=IREC,IOSTAT=ISTAT,ERR=1000) LCTLF
         IREC=IREC+1
      ENDIF

C Check that IREC=IRCPLP, stop if not equal.
      if(irec.ne.ircplp) then
        write(outs,'(a,i4,a,i4,a)') ' End of header ',irec,
     &   ' & pointer to results set ',ircplp,' dont match.!'
        call edisp(iuout,outs)
      endif

C Determine number of records for all components.
C Add additional output for components if any.
      mxnode=nprecl/3
      nblock=0
      do 717 jj=1, npcomp
         irec=ipcrec(jj)
         call dbdat(jj,irec,nnode(jj),napout(jj))
         nrecs=nnode(jj)/(mxnode+1)+1
         nrecpc(jj)=nrecs
         nblock=nblock+nrecs
         if(napout(jj).ne.0) nblock=nblock+1
  717 continue

C Default set-up.
      
      IF(IDEFLT.EQ.1)then
         IOH1=1
         IOD1=IpsD1
         IOM1=IpsM1
         IOH2=24
         IOD2=IpsD2
         IOM2=IpsM2
         IODS=IpSDS
         IODF=IpSDF
         NOUT=1
         IAV=0
      endif
      return

 999  WRITE(outs,'(a)') ' Simulation year not found'
      call edisp(iuout,outs)
      return
     

 1000 WRITE(outs,'(a,I5)') ' Library error at record',IREC
      call edisp(iuout,outs)
      return
      end

C ************************ dbdat **************************
C Read (??) additional output data for a plant component.
C ipc is the index of the component index.
C irec is the initial record to scan.
C nnode (??) is returned 
C naout (number of additional output data) is returned.
      subroutine dbdat(ipc,irec,nnode,naout)
#include "plant.h"
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      common/ntype/isv(mpcom,mnodec)
      common/adopdesc/adopdsc(mpcom,15)
      LOGICAL OK,dok
      CHARACTER ZDESC*25,cdate*16,buffer*80,adopdsc*30
      character h*72

C If unit for plant component db not yet set, do it.
      if(ipcdb.eq.0)ipcdb=ifil+11

C 'irecln' is defined in 'plantdb.h'.
      irecln=20
  100 call EFOPRAN(ipcdb,lpcdb,IRECLN,1,IER)

C Check for error on openening.
      if(ier.gt.0) then
         ok=.false.
         dok=.false.
         h(1)='The plant component database associated with the model'
         h(2)='could not be opened. Check the model and then the'
         h(3)='folder and the file before trying again. '
         CALL ASKOK('Error occurred on opening data base file',
     &              ' Try again?',OK,dok,3)
         if(ok) goto 100
      elseif(ier.lt.0) then

C File doesn't exist.
         ok=.false.
         dok=.false.
         h(1)='The plant component database associated with the model'
         h(2)='could not be opened. Check the model and then the'
         h(3)='folder and the file before trying again. '
         CALL ASKOK(' Plant component database does not exist.',
     &                 ' Try again?',OK,dok,3)
         CALL USRMSG(' ',' ','-')
         if(ok) goto 100
      endif

C Old file, so check header.
      iorec = 1
      READ (ipcdb,REC=IOREC,IOSTAT=ISTAT,ERR=99999) ZDESC
      IF (ZDESC .NE. ' PLANT COMPONENT DATABASE') then
         ok=.false.
         dok=.false.
         h(1)='The plant component database associated with the model'
         h(2)='could not be opened. Check the model and then the'
         h(3)='folder and the file before trying again. '
         CALL ASKOK(' Not a plant database file.',
     &               ' Try another file? ',OK,dok,3)
         CALL USRMSG(' ',' ','-')
         if(ok) goto 100
      endif

      read(ipcdb,rec=irec,iostat=istat,err=99999)
     &     idum,idum,cdate,nnode,nmatx,nmisc
      irec=irec+1
      read(ipcdb,rec=irec,iostat=istat,err=99999)
     &     idum,idum,idum,idum,idum,ncdata,naout
      irec=irec+3
      read(ipcdb,rec=irec,iostat=istat,err=99999)
     &     (ISV(ipc,INOD),INOD=1,NNODE)


C Read additional output description and type
      if(naout.gt.0) then
        irec=irec + 1 + (NMATX-1)/IRECLN + NMISC + ncdata
        do 160 i=1, naout
          irec=irec+1
          read(ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=99999)adopdsc(ipc,i)
  160   continue
      endif

      return

C Error trap routine.
99999 write(buffer,99099)
99099 format('Error reading plant data base.')
      call edisp(iuout,buffer)
      return
      end

C ************************ fnddat *************************
C Given the day, hour and current step, fnddat reads
C the component 'ipc' results data from plant library
C file.
C State space variables data and additional outout
C data are stored in CSV?M and APCOUT arrays respectively.
      subroutine fnddat(ii,ipc,ier)
#include "plant.h"

      COMMON/pres/iunitp
      COMMON/LIBRCP/IRCPLP,nblock
      common/pcdat/ CSV1M(mnodec),CSV2M(mnodec),CSV3M(mnodec),
     &              NNODE(mpcom),nrecpc(mpcom)
      common/pcres/ napout(mpcom),apcout(20)

C Find data location in result file.
      ier=0
      IREC=IRCPLP+NBLOCK*ii

C Find relative position of record for this component
      if(ipc.gt.1) then
        do 373 ir=2,ipc
          irec=irec+nrecpc(ir-1)
          if(napout(ir-1).gt.0) irec=irec+1
 373    continue
      endif

C Find number of records required.
      nrecs=(nnode(ipc)*3)/(nprecl+1)+1

C Initialise first item number and number of items left
C respectively.
      items=1
      mxnode=nprecl/3
      iteml=nnode(ipc)
      items=1

C Start reading nodal data, perhaps using more than one record.
      do 7000 ir=1, nrecs
        if(iteml.le.mxnode) then
          iteme=nnode(ipc)
        else
          iteme=items+mxnode-1
        endif

C Read each node's temperature, 1th and 2nd phase mass flow rate.
        READ(IUNITp,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &      (CSV1M(JJ),CSV2M(JJ),CSV3M(JJ),JJ=items,iteme)
        IREC=IREC+1
        items=mxnode*ir+1
        iteml=nnode(ipc)-items+1
 7000 continue

C Read additional output if any.
      if(napout(ipc).gt.0)
     &   READ(IUNITp,REC=IREC,IOSTAT=ISTAT,
     &        ERR=1000)(apcout(jj),JJ=1,napout(ipc))
      return

 1000 WRITE(6,'(A,I5)') ' error when reading results for comp ',ipc
      ier=1
      END

C ******************** MOSYSSU ********************
C MOSYSSU clears commons/ sets default values for systems plotting.

      SUBROUTINE MOSYSSU
      PARAMETER (MSYSRS=20)

      COMMON/MSYSPICK/NPGET,IPGETNO(MSYSRS,8)

      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/SYSLABEL/SPLABEL(MSYSRS),GPLABEL(MSYSRS)

      integer YAXSET, ADDLIN
      character SPLABEL*32,GPLABEL*20

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
      NPGET=0
      do 10 I=1,MSYSRS
        IPGETNO(I,1)=0
        IPGETNO(I,2)=0
        IPGETNO(I,3)=0
        IPGETNO(I,4)=0
        IPGETNO(I,5)=0
        IPGETNO(I,6)=0
        IPGETNO(I,7)=0
        IPGETNO(I,8)=0
        SPLABEL(I)='  '
        GPLABEL(I)='  '
 10   continue
      ialstused=0

      return
      end

C ******************** SYSOUTP ********************
C SYSOUTP is the main controlling routine for system line graph
C output, stats, data above or below a setpoint, timestep listings and
C histograms. Occupancy filtering (based on user selected zone) is
C implemented for all but line graphs.
C This output mode allows the various causal relationships 
C to be displayed and so visually investigated.   
C act='p' iget parameters already set.
C If imode is 1 then user is asked to confirm the plant
C results file. If imode is 0 then sysoutp has been called
C imediately after starting res because the file passed on
C the command line was a plant results file.

      SUBROUTINE SYSOUTP(act,imode)
#include "building.h"
#include "plant.h"
C act='p' iget parameters already set.
      PARAMETER (MSYSRS=20)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)

      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN
      COMMON/GRAF2/YSC(5),Yadd(5),TSC,Xadd,IGX(5),ILR(5),DT
      common/grextras/graftitle,grlbl(5),ngrlbl,lblpx(5),lblpy(5)

      COMMON/PPERS/IPSD1,IPSM1,IPSD2,IPSM2,IPSDS,IPSDF,NPTS,ipsav
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/C6/INDCFG
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      common/getmenu/menutype,igetind(65),igetflux(65)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/prav/IDAVER

      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

C Occupancy filter info.
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      common/mfocup/iocpzone

      COMMON/MSYSPICK/NPGET,IPGETNO(MSYSRS,8)
      COMMON/SYSLABEL/SPLABEL(MSYSRS),GPLABEL(MSYSRS)

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      COMMON/FREQS/NBINS,XBIN1,XBINN,DXBIN,IFREQ,IHITS,IMISS,INBIN(MF)
      COMMON/FREQS1/ICBIN(MF),ITOTAL
      common/ntype/isv(mpcom,mnodec)
      common/pcdat/ CSV1M(mnodec),CSV2M(mnodec),CSV3M(mnodec),
     &              NNODE(mpcom),nrecpc(mpcom)
      common/pcres/ napout(mpcom),apcout(20)

      dimension Yold(65),TIMEold(65),val(MSYSRS)
      character outs*124,TLABEL*60,trange*20,tranged*20
      character ALABEL(5)*12,t20*20,act*1,H*72
      CHARACTER PDESCR*60,SDESCR*44,DESCR*7,DESCR1*10
      character SPLABEL*32,GPLABEL*20
      CHARACTER DESCRH*5,DESCRD*5,NT*14,XT*14
      character graftitle*64,grlbl*24,t24*24
      character ETEXT*124,t64*64
      dimension FMAX(MSYSRS), FMIN(MSYSRS), FTOTSD(MSYSRS)
      dimension FAVE(MSYSRS), FSTD(MSYSRS), NF(MSYSRS)
      dimension TMAX(MSYSRS), TMIN(MSYSRS), IQHRS(MSYSRS)

      integer YAXSET, ADDLIN

      logical colok,ok,dok
      LOGICAL spfileok,perok,tstepok,saveok,autook,exitok,startupok

C Begin with the menu type=4 (performance metrics).
      menutype=4
      ideflt=1
      call opnplt(npcomp,imode,ier)
      if(ier.ne.0) return

C Define prompt.
      outs='  '

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.MSYSRS)colok=.true.
      
C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        if(NPGET.ge.1)then
          write(outs,'(2a)')GPLABEL(1)(1:lnblnk(GPLABEL(1))),
     &      '>> file.'
        else
          write(outs,'(a)')' Output being directed to file.' 
        endif
        call edisp(iuout,outs)
      elseif(ixopen.eq.0)then
        if(MMOD.eq.8)call startbuffer()
      endif

C Call the menu of choices (this also sets some default options).
C First clear current selections.
      call MOSYSSU

C Save original date values before calling routines
      ibs3=ID1
      ibs4=IM1
      ibs5=ID2
      ibs6=IM2
      ibs7=ISDS
      ibs8=ISDF
      ibs9=NTS

C Assign new values
      ID1=IPSD1
      IM1=IPSM1
      ID2=IPSD2
      IM2=IPSM2
      ISDS=IPSDS
      ISDF=IPSDF
      NTS=NPTS

C Call the setup menu.
 1    call PLTTAB
      call edisp(iuout,' ')
      if(ixopen.eq.1)then
        itru = ixunit
      else
        itru = icout
      endif
      if (MENUTYPE.eq.-1)then

C User exited plttab via -end rather than to request something displayed.
C Restore original date values
        ID1=ibs3
        IM1=ibs4
        ID2=ibs5
        IM2=ibs6
        ISDS=ibs7
        ISDF=ibs8
        NTS=ibs9
        return
      endif
      if (MENUTYPE.eq.9)then
        CALL PSYCHART('p')
        goto 1
      endif

C Ask for set point value.
      if (MENUTYPE.eq.7.or.MENUTYPE.eq.8)then
        if (act(1:1).eq.'p') then
          continue
        else
          H(1)='The ..'
          XQUERY=0.0
          CALL EASKR(XQUERY,' ','  Test point value ? ',
     &             0.0,'-',0.0,'-',20.0,'query value',IER,1)
        endif
      endif

C TSTART and TFINISH - start and finish timesteps from 00h00 on the 
C first day of output.
      ENTERVAL=1.0/FLOAT(NPTS)/24.0
      istart=((ioh1-1)+(iods-IPSDS)*24)*npts
      if(istart.eq.0)istart=1
      iend  =((ioh2)+(iodf-IPSDS)*24)*npts-1
      TSTART=float(istart)
      TFINSH=float(iend)

C Debug.
C      write(6,*) 'start and finish for graph ',TSTART,TFINSH

C Set add profile label ADDLIN=0 i.e. no add
      ADDLIN=0

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps.
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

C For line graphs, check if axis is currently drawn, if not then scale.
      if (MENUTYPE.eq.3) then
        if(npget.eq.0)then
          call usrmsg('No items selected....',' ','W')
          goto 1
        endif
        do 5 I=1,NPGET
          IAX=IPGETNO(I,8)
          if (YAXSET(IAX).eq.0.or.ADDLIN.eq.0) then

C Setup parameters and call fnddat for each output to get required data.
C Set TIMEold to -ive number so as to identify first point to be plotted.
C The YMAX,MIN variables are for axis 1 through 5 max and min values.
            call AXISINT(3,ALABEL)
            do 6 J=1,65
              TIMEold(J)=-1.
 6          continue
          endif
 5      continue
      elseif (MENUTYPE.eq.4) then

C For timestep listings generate initial lines.
        if(npget.eq.0)then
          call usrmsg('No items selected....',' ','W')
          goto 1
        endif
        if (IHFLAG.eq.1) then
          call edisp(itru,'# Timestep performance metrics.')
        else
          call edisp(itru,'Timestep performance metrics.')
        endif
      endif
      call usrmsg('Scanning data for range of values...','  ','-')

C Reset stats values.Set query hours to zero.
      do 55 I=1,MSYSRS 
        IQHRS(I)=0
        FTOTSD(I)=0.0
        FSTD(I)=0.0
        FAVE(I)=0.0
        NF(I)=0
        FMAX(I)= -1.E+10
        FMIN(I)=  1.E+10
 55   continue

C Set histogram bounds.
      XMAX=-1.E+10
      XMIN=1.E+10

C First recover bounds of selected data when no averaging is required
C << no occupancy sensing at this time >>
      if(iav.eq.0) then
        if(idaver.eq.0) then
           ctime=float(ioh1-1)/24.0+ENTERVAL/2.+float(iods)
        else
           ctime=float(ioh1-1)/24.0+enterval+float(iods)
        endif

C For each timestep.
        do 100 ii=istart, iend, nout

C For each requested item.
          do 40 jstat=1,npget
            ipc =IPGETNO(jstat,2)
            nsnd=IPGETNO(jstat,3)
            if(IPGETNO(jstat,1).eq.9)then

C Asking for ambient db. Recover via call to mfclim
C Current timestep ii to a iday and hour (MINH).
C IPAR is one to get ambient db. Set ITS to 1 to get
C start of the hour.
              iday=INT(ctime)
              call DASYSTIME(II,TIME)
              MINH=INT(time)+1
              IPAR=1
              ITS=1
              call MFCLIM(IDAY,MINH,ITS,IPAR,VALC)
              val(jstat)=VALC
            elseif(IPGETNO(jstat,1).eq.12)then

C Asking for ambient rh.
C IPAR is four to get ambient db. Set ITS to 1 to get
C start of the hour.
              iday=INT(ctime)
              call DASYSTIME(II,TIME)
              MINH=INT(time)+1
              IPAR=4
              ITS=1
              call MFCLIM(IDAY,MINH,ITS,IPAR,VALC)
              val(jstat)=VALC
            else
              call fnddat(ii,ipc,ier)
              if(ier.ne.0)then
                write(6,*) 'An error in fnddat ',ii,ipc,ier
                goto 40
              endif
            endif

C Node temperature (C)
            if(IPGETNO(jstat,1).eq.1)then
              val(jstat)=csv1m(nsnd)

C Node first phase mass flow rate (kg/s)
            elseif(IPGETNO(jstat,1).eq.2)then
              val(jstat)=csv2m(nsnd)

C Node second phase mass flow rate (kg/s)
            elseif(IPGETNO(jstat,1).eq.3)then
              val(jstat)=csv3m(nsnd)
            endif

C Node relative humidity %
            if(IPGETNO(jstat,1).eq.4.and.csv2m(nsnd).gt.0.0) 
     &        val(jstat)=pcrh2(csv1m(nsnd),
     &                         csv3m(nsnd)/csv2m(nsnd),1013.25)

C Node specific humidity (kg(v)/kg(a))
            if(IPGETNO(jstat,1).eq.5.and.csv2m(nsnd).gt.0.0) 
     &        val(jstat)=csv3m(nsnd)/csv2m(nsnd)

C Node enthalpy (J/kg K)
            if(IPGETNO(jstat,1).eq.6.and.csv2m(nsnd).gt.0.0) then

C  ..water..
              if(isv(ipc,nsnd).eq.10.or.isv(ipc,nsnd).eq.20) 
     &           val(jstat)=csv1m(nsnd)*shth2o(csv1m(nsnd))

C  ..dry air..
              if(isv(ipc,nsnd).eq.1.or.isv(ipc,nsnd).eq.11) 
     &           val(jstat)=enthp2(csv1m(nsnd),0.0)

C  ..moist air..
              if(isv(ipc,nsnd).eq.21) 
     &           val(jstat)=enthp2(csv1m(nsnd),
     &                      csv3m(nsnd)/csv2m(nsnd))
            endif

C Node Dew Point temperature (C)
            if(IPGETNO(jstat,1).eq.7.and.csv2m(nsnd).gt.0.0.and.
     &         isv(ipc,nsnd).eq.21) 
     &        val(jstat)=dewpt(csv3m(nsnd)/csv2m(nsnd),1013.25)

C Additional output. Note that 'nsnd' is NOT a node, 
C it is the additional output number for the component considered.
            if(IPGETNO(jstat,1).eq.10) val(jstat)=apcout(nsnd)

C Calculate summary stats (part 1)
            if(VAL(jstat).gt.FMAX(jstat))then
              FMAX(jstat)=VAL(jstat)
              TMAX(jstat)=CTIME
            elseif(VAL(jstat).lt.FMIN(jstat))then
              FMIN(jstat)=VAL(jstat)
              TMIN(jstat)=CTIME
            endif
            FAVE(jstat)=FAVE(jstat)+VAL(jstat)
            NF(jstat)=NF(jstat)+1

C Set max and min for graph axis.
            IAX=IPGETNO(jstat,8)
            if (YAXSET(IAX).lt.2) then
              YMAX(IAX)=MAX(VAL(jstat),YMAX(IAX))
              YMIN(IAX)=MIN(VAL(jstat),YMIN(IAX))
              YAXSET(IAX)=1
            endif

C Set max and min for any histogram, (this follows the pattern
C used in frqdis.F where initial range of data is not filtered
C by occupancy.
            XMAX=MAX(VAL(jstat),XMAX)
            XMIN=MIN(VAL(jstat),XMIN)
   40     continue

C We have all the data for this timestep into val array.
C Debug.
C          write(6,'(F10.5,12(2x,G10.3))')
C     &      ctime,(val(jjj),jjj=1,npget)

C update ctime for the next loop.
          ctime=ctime+enterval*float(nout)
  100   continue
      else

C Case when averaging is required
        if(idaver.eq.0) then
          ctime=float(ioh1-1)/24.0+ENTERVAL/2.+float(iods)
     &              +ENTERVAL*float(nout)/2.
        else
          ctime=float(ioh1-1)/24.0+enterval+float(iods)
     &              +ENTERVAL*float(nout)/2.
        endif
        do 111 jj=1,npget
  111     val(jj)=0.0
        jcount=0
        do 120 ii=istart, iend
          do 60 jstat=1,npget
            ipc =IPGETNO(jstat,2)
            nsnd=IPGETNO(jstat,3)
            if(IPGETNO(jstat,1).eq.9)then

C Asking for ambient db. Recover via call to mfclim
C Current timestep ii to a iday and hour (MINH).
C IPAR is one to get ambient db. Set ITS to 1 to get
C start of the hour.
              iday=INT(ctime)
              call DASYSTIME(II,TIME)
              MINH=INT(time)+1
              IPAR=1
              ITS=1
              call MFCLIM(IDAY,MINH,ITS,IPAR,VALC)
              val(jstat)=VALC
            elseif(IPGETNO(jstat,1).eq.12)then

C Asking for ambient rh.
C IPAR is four to get ambient db. Set ITS to 1 to get
C start of the hour.
              iday=INT(ctime)
              call DASYSTIME(II,TIME)
              MINH=INT(time)+1
              IPAR=4
              ITS=1
              call MFCLIM(IDAY,MINH,ITS,IPAR,VALC)
              val(jstat)=VALC
            else
              call fnddat(ii,ipc,ier)
              if(ier.ne.0)then
                write(6,*) 'An error in fnddat ',ii,ipc,ier
                goto 60
              endif
            endif

C Node temperature (C)
            if(IPGETNO(jstat,1).eq.1)then
              val(jstat)=val(jstat)+csv1m(nsnd)

C Node first phase mass flow rate (kg/s)
            elseif(IPGETNO(jstat,1).eq.2)then
              val(jstat)=val(jstat)+csv2m(nsnd)

C Node second phase mass flow rate (kg/s)
            elseif(IPGETNO(jstat,1).eq.3)then
              val(jstat)=val(jstat)+csv3m(nsnd)
            endif

C Node relative humidity %
            if(IPGETNO(jstat,1).eq.4.and.csv2m(nsnd).gt.0.0) 
     &        val(jstat)=val(jstat)+pcrh2(csv1m(nsnd),
     &                   csv3m(nsnd)/csv2m(nsnd),1013.25)

C Node specific humidity (kg(v)/kg(a))
            if(IPGETNO(jstat,1).eq.5.and.csv2m(nsnd).gt.0.0) 
     &        val(jstat)=val(jstat)+csv3m(nsnd)/csv2m(nsnd)

C Node enthalpy (J/kg K)
            if(IPGETNO(jstat,1).eq.6.and.csv2m(nsnd).gt.0.0) then

C ..water..
              if(isv(ipc,nsnd).eq.10.or.isv(ipc,nsnd).eq.20) 
     &           val(jstat)=val(jstat)+csv1m(nsnd)*shth2o(csv1m(nsnd))

C ..dry air..
              if(isv(ipc,nsnd).eq.1.or.isv(ipc,nsnd).eq.11) 
     &           val(jstat)=val(jstat)+enthp2(csv1m(nsnd),0.0)

C ..moist air..
              if(isv(ipc,nsnd).eq.21) 
     &          val(jstat)=val(jstat)+enthp2(csv1m(nsnd),
     &                     csv3m(nsnd)/csv2m(nsnd))
            endif

C Node Dew Point temperature (C)
            if(IPGETNO(jstat,1).eq.7.and.csv2m(nsnd).gt.0.0.and.
     &         isv(ipc,nsnd).eq.21)  val(jstat)=val(jstat)
     &                          +dewpt(csv3m(nsnd)/csv2m(nsnd),1013.25)

C Additional output.
C Note, in this case 'nsnd' is NOT a node, 
C it is the additional output number for the component considered.
            if(IPGETNO(jstat,1).eq.10)val(jstat)=val(jstat)+apcout(nsnd)
   60     continue
          jcount=jcount+1

C If we are at last timestep of the hour then average it
C and process the information.
          if(jcount.eq.nout) then
            do 112 jj=1,npget
              val(jj)=val(jj)/float(nout)
  112       continue

C Have averaged val array for the current hour.
C Debug
C            write(6,'(f10.5,12(2x,G10.3))')
C     &            ctime,(val(jjj),jjj=1,npget)

C Calculate summary stats (part 1). Loop through each of
C the selected items to set FMAX, TMAX etc.
            do 61 jstat=1,npget
              if(VAL(jstat).gt.FMAX(jstat))then
                FMAX(jstat)=VAL(jstat)
                TMAX(jstat)=CTIME
              elseif(VAL(jstat).lt.FMIN(jstat))then
                FMIN(jstat)=VAL(jstat)
                TMIN(jstat)=CTIME
              endif
              FAVE(jstat)=FAVE(jstat)+VAL(jstat)
              NF(jstat)=NF(jstat)+1

C Set max and min for graph axis.
              IAX=IPGETNO(jstat,8)
              if (YAXSET(IAX).lt.2) then
                YMAX(IAX)=MAX(VAL(jstat),YMAX(IAX))
                YMIN(IAX)=MIN(VAL(jstat),YMIN(IAX))
                YAXSET(IAX)=1
              endif

C Set max and min for any histogram, (this follows the pattern
C used in frqdis.F where initial range of data is not filtered
C by occupancy.
              XMAX=MAX(VAL(jstat),XMAX)
              XMIN=MIN(VAL(jstat),XMIN)
  61        continue

C Tidy up in preparation for the next loop.
            ctime=ctime+enterval*float(nout)
            do 113 jj=1,npget
              val(jj)=0.0
  113       continue
            jcount=0
          endif
  120   continue
      endif

C Calculate average.
      do 56 I=1,NPGET
        if(NF(I).ne.0) FAVE(I)=FAVE(I)/real(NF(I))
  56  continue

      call usrmsg('Scanning data for range of values...done.','  ','P')
      if (MENUTYPE.eq.2) then

C Inform user of the histo range and reset nb of bins and typcial bin size.
        if(npget.eq.0)then
          call usrmsg('No items selected....',' ','W')
          goto 1
        endif
        write(outs,'(a,a,a,f9.2,a,f10.2)')'The ',
     &    GPLABEL(1)(1:lnblnk(GPLABEL(1))),' data range is',
     &    XMIN,' to',XMAX
        call edisp(itru,outs)
        DXBIN=0.0
        NBINS=1

C If in pif mode use default bin setup.
        if (act(1:1).eq.'p'.and.autook)then
          ok=.TRUE.
        else
          dok=.true.
          h(1)='If you need to customize the frequency reporting'
          h(2)='say no, otherwise the default set-up will probably '
          h(3)='work for initial interactive views. '
          call askok('  ','Use default bin set-up?',OK,dok,3)
        endif
        if(ok)then

C Check data range and pick a suitable value for DXBIN. No more than 
C MF bins for any given range. 
          DELTA=XMAX-XMIN
          if (DELTA.lt.1.) then
            DXBIN=0.05
          elseif (DELTA.lt.2.) then
            DXBIN=0.1
          elseif (DELTA.lt.5.) then
            DXBIN=0.25
          elseif (DELTA.lt.15.) then
            DXBIN=0.5
          elseif (DELTA.lt.30.) then
            DXBIN=1.0
          elseif (DELTA.lt.60.) then
            DXBIN=2.0
          elseif (DELTA.lt.150.) then
            DXBIN=5.0
          elseif (DELTA.lt.300.) then
            DXBIN=10.0
          elseif (DELTA.lt.500.) then
            DXBIN=20.0
          elseif (DELTA.lt.1000.) then
            DXBIN=25.0
          elseif (DELTA.lt.2000.) then
            DXBIN=50.0
          elseif (DELTA.lt.5000.) then
            DXBIN=100.0
          elseif (DELTA.lt.10000.) then
            DXBIN=200.0
          elseif (DELTA.lt.20000.) then
            DXBIN=500.0
          elseif (DELTA.lt.50000.) then
            DXBIN=1000.0
          else
            DXBIN=100.0
          endif

C Bin sized from data max & min. 
          XBIN1=ANINT(XMIN/DXBIN)*DXBIN
          XBINN=ANINT(XMAX/DXBIN)*DXBIN
          NB=INT((XBINN-XBIN1)/DXBIN)
          if(NB.ge.MF)then
            write(outs,'(a,3(i4,a))')'The required ',NB,'bins > ',MF,
     &                      '(max number of bins). Using ',MF-1,'bins.'
            call edisp(iuout,outs)
            NB=MF-1
          endif
          NBINS=NB+1
          XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
          XBIN1=XBIN1-XDIFF
          XBINN=XBINN+XDIFF
        else
          XBIN1=XMIN
          XBINN=XMAX
          H(1)='Give minimum bin value for the current parameter.'
          call easkr(XBIN1,' ',' Minimum parameter value for bin ?',
     &                     0.,'-',0.,'-',0.,'min bin parameter',IER,1)
          H(1)='Give maximum bin value for the current parameter.'
          call easkr(XBINN,' ',' Maximum parameter value for bin ?',
     &                  XBIN1,'F',0.,'-',0.,'max bin parameter',IER,1)
          H(1)='The number of frequency bins is required.'
          call easki(NBINS,' ',' Number of bins required ? ',
     &                               1,'F',MF,'F',0,'no of bins',IER,0)
          DXBIN=(XBINN-XBIN1)/NBINS
        endif
        call usrmsg(' ',' ','-')

C Error check on bin width.
        IF(DXBIN.le.0.0)then
          call edisp(iuout,'All data zero: no frequency distribution.')
          call edisp(iuout,'Data range error: plot abandoned.')

C Restore original values before returning
          ID1=ibs3
          IM1=ibs4
          ID2=ibs5
          IM2=ibs6
          ISDS=ibs7
          ISDF=ibs8
          NTS=ibs9
          RETURN
        endif

C Empty bins of hits.
        DO 510 I=1,NBINS
          INBIN(I) = 0
          ICBIN(I) = 0
  510   CONTINUE
        IHITS = 0
        IMISS = 0
        iover = 0
        iunder = 0
      elseif (MENUTYPE.eq.3) then

C Write graph head and set up axis positions and draw.
        IF(MMOD.EQ.8)THEN
#ifdef X11
          call feedbox(menuchw,2,igfw,igfh)
          call opengdisp(menuchw,LIMTTY,2,igdw,igdh)
#endif
          call win3d(menuchw,9,30,4,3,igl,igr,igt,igb,igw,igwh)
          call startbuffer()
        ENDIF

        WRITE(ETEXT,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &    SDESCR(11:lnblnk(SDESCR))
        CALL viewtext(ETEXT,1,0,1)

        iancline=0
        if(graftitle(1:2).ne.'  ')then
          t64=graftitle
          if(iancline.gt.0)CALL viewtext(t64,3,1,2)
          if(iancline.eq.0)CALL viewtext(t64,2,1,2)
        endif

        call AXISPOS
        call AXISDRW(TSTART-1.0,TFINSH,ALABEL,'P')
      endif

C Recover and plot data. If listing to file or text feedback then
C write out labels for each column at the start.
      if (MENUTYPE.eq.4) then
        if(idaver.eq.0) then
           ctime=float(ioh1-1)/24.0+ENTERVAL/2.+float(iods)
        else
           ctime=float(ioh1-1)/24.0+enterval+float(iods)
        endif
        iday=INT(ctime)
        CALL STDATE(IYEAR,IDAY,DESCR,DESCR1)
        if (IDHFLG.eq.1) then
          outs='# '
          call edisp(itru,outs)
          write(outs,'(3A)')'# ',DESCR1,' timestep listing'
          call edisp(itru,outs)
          outs='# '
          call edisp(itru,outs)
        else
          outs='  '
        endif
        if (IHFLAG.eq.1) then
          outs='#Time'
        else
          outs='Time '
        endif
        K=6
        do 20 IL=1,NPGET
          KE=K+11
          if(KE.le.124)then
           write (outs(K:KE),'(a,a)') '|',GPLABEL(IL)(1:10)
           K=K+11
          endif
 20     continue

C Print titles on first day.
        call edisp(itru,outs)
        if (IDHFLG.eq.1) then
          outs='#    '
        else
          outs='     '
        endif
        if (IHFLAG.eq.1) then
          outs='#    '
        else
          outs='     '
        endif
        K=6
        do 225 IL=1,NPGET
          KE=K+11
          if(KE.le.124)then
           write (outs(K:KE),'(a,a)') '|',GPLABEL(IL)(11:20)
           K=K+11
          endif
 225    continue
        call edisp(itru,outs)
        if (IDHFLG.eq.1) then
          outs='#    '
        else
          outs='     '
        endif
        if (IHFLAG.eq.1) then
          outs='#    '
        else
          outs='     '
        endif
      endif

C Now, for each system timestep get the non-averaged data and
C deal with it.
      if(iav.eq.0) then
        if(idaver.eq.0) then
           ctime=float(ioh1-1)/24.0+ENTERVAL/2.+float(iods)
           gtime=TSTART
        else
           ctime=float(ioh1-1)/24.0+enterval+float(iods)
           gtime=TSTART
        endif

C Debug.
C        write(6,*) 'initial ctime gtime ',ctime,gtime,enterval

C For each timestep.
        do 200 ii=istart, iend, nout

C For each requested item.
          do 240 jstat=1,npget
            ipc =IPGETNO(jstat,2)
            nsnd=IPGETNO(jstat,3)
            if(IPGETNO(jstat,1).eq.9)then

C Asking for ambient db. Recover via call to mfclim
C Current timestep ii to a iday and hour (MINH).
C IPAR is one to get ambient db. Set ITS to 1 to get
C start of the hour.
              iday=INT(ctime)
              call DASYSTIME(II,TIME)
              MINH=INT(time)+1
              IPAR=1
              ITS=1
              call MFCLIM(IDAY,MINH,ITS,IPAR,VALC)
              val(jstat)=VALC
            elseif(IPGETNO(jstat,1).eq.12)then

C Asking for ambient rh.
C IPAR is four to get ambient db. Set ITS to 1 to get
C start of the hour.
              iday=INT(ctime)
              call DASYSTIME(II,TIME)
              MINH=INT(time)+1
              IPAR=4
              ITS=1
              call MFCLIM(IDAY,MINH,ITS,IPAR,VALC)
              val(jstat)=VALC
            else
              call fnddat(ii,ipc,ier)
              if(ier.ne.0)then
                write(6,*) 'An error in fnddat ',ii,ipc,ier
                goto 240
              endif
            endif

C Node temperature (C), first phase, 2nd phase.
            if(IPGETNO(jstat,1).eq.1)then
              val(jstat)=csv1m(nsnd)
            elseif(IPGETNO(jstat,1).eq.2)then
              val(jstat)=csv2m(nsnd)
            elseif(IPGETNO(jstat,1).eq.3)then
              val(jstat)=csv3m(nsnd)
            endif

C Node relative humidity %
            if(IPGETNO(jstat,1).eq.4.and.csv2m(nsnd).gt.0.0) 
     &        val(jstat)=pcrh2(csv1m(nsnd),
     &                         csv3m(nsnd)/csv2m(nsnd),1013.25)

C Node specific humidity (kg(v)/kg(a))
            if(IPGETNO(jstat,1).eq.5.and.csv2m(nsnd).gt.0.0) 
     &        val(jstat)=csv3m(nsnd)/csv2m(nsnd)

C Node enthalpy (J/kg K)
            if(IPGETNO(jstat,1).eq.6.and.csv2m(nsnd).gt.0.0) then

C  ..water..
              if(isv(ipc,nsnd).eq.10.or.isv(ipc,nsnd).eq.20) 
     &           val(jstat)=csv1m(nsnd)*shth2o(csv1m(nsnd))

C  ..dry air..
              if(isv(ipc,nsnd).eq.1.or.isv(ipc,nsnd).eq.11) 
     &           val(jstat)=enthp2(csv1m(nsnd),0.0)

C  ..moist air..
              if(isv(ipc,nsnd).eq.21) 
     &           val(jstat)=enthp2(csv1m(nsnd),
     &                      csv3m(nsnd)/csv2m(nsnd))
            endif

C Node Dew Point temperature (C)
            if(IPGETNO(jstat,1).eq.7.and.csv2m(nsnd).gt.0.0.and.
     &         isv(ipc,nsnd).eq.21) 
     &        val(jstat)=dewpt(csv3m(nsnd)/csv2m(nsnd),1013.25)

C Additional output. Note that 'nsnd' is NOT a node, 
C it is the additional output number for the component considered.
            if(IPGETNO(jstat,1).eq.10) val(jstat)=apcout(nsnd)
  240     continue

C We have the data for this timestep into val array.
C Debug.
C          write(6,'(F10.5,12(2x,G10.3))')
C     &            ctime,(val(jjj),jjj=1,npget)

          if (MENUTYPE.eq.4) then

C Timestep listing. Fisrt split time into hours and minutes.
            call DASYSTIME(II,TIME)
            MINH=INT(time)
            MINU=INT((time-AINT(time))*60.+0.5)
            if(MINH.lt.10)then
              IF(MINU.LT.10)THEN
                WRITE(DESCRH,'(a1,i1,a2,i1)')'0',INT(TIME),'h0',MINU
              ELSE
                WRITE(DESCRH,'(a1,i1,a1,i2)')'0',INT(TIME),'h',MINU
              ENDIF
            else
              IF(MINU.LT.10)THEN
                WRITE(DESCRH,'(i2,a2,i1)')INT(TIME),'h0',MINU
              ELSE
                WRITE(DESCRH,'(i2,a1,i2)')INT(TIME),'h',MINU
              ENDIF
            endif
            outs='  '
            if (IHFLAG.eq.0) then 

C Split flow time into hours and minutes, repeating every 24 hours.
              write (outs,'(a5)') DESCRH
              K=6
            else
              write (outs,'(f10.4)') ctime
              K=11
            endif
            do 410 IXY=1,NPGET
              KE=K+11
              if(KE.le.124)then
                write (outs(K:KE),'(f11.4)') VAL(IXY)
                K=K+11
              endif
 410        continue
            call eddisp(itru,outs)
          else

C Do the other menu types. If graph reset scaling factor for appropriate axis.
            do 400 IX=1,NPGET
              if (MENUTYPE.eq.3) then
                Iax=IPGETNO(IX,8)
                call linescale(igl,Xadd,TSC,igb,Yadd(Iax),YSC(Iax))
                if (TIMEold(IX).lt.0.0.or.ADDLIN.eq.1) then
                  TIMEold(IX)=0
                  Yold(IX)=VAL(IX)
                endif
                ioldpos=1
              endif

C If within requested output period.
              IF(GTIME.LT.(TSTART-1.0).or.GTIME.GT.TFINSH)goto 402
              if (MENUTYPE.eq.2) then

C First, recalculate time as it is done in table.F 
C If there is occupancy filter and occupancy then use pattern
C from zone iocpzone. Start with assumption of full occupancy.
                IB = INT(((VAL(IX) - XBIN1) / DXBIN) + 1.)
                if(IB.LE.0)then
                  iunder = iunder +1
                  IMISS = IMISS + 1
                elseif(IB.GT.NBINS)then
                  iover = iover +1
                  IMISS = IMISS + 1
                else
                  INBIN(IB) = INBIN(IB) + 1
                  IHITS = IHITS + 1
                endif
              elseif (MENUTYPE.eq.3) then

C Draw next section of line.
                if(ioldpos.eq.1) call etplot(TIMEold(IX),Yold(IX),0,0)
                ioldpos=0
                IPEN=1
                MARK=0
                RMK=AMOD(GTIME,DT)
                if (RMK.lt.0.001) MARK=6
                TIMEold(IX)=GTIME
                Yold(IX)=VAL(IX)

C If plotting colours for different components then recall which
C item we are dealing with. If the index of the item is greater
C than that of the number of colours re-use an earlier colour.
                if(colok)then
                  if(IPGETNO(IX,2).gt.0)then
                    ICM=IPGETNO(IX,2)
                    if(ICM.gt.nzonec) ICM=IPGETNO(IX,2)-nzonec
                    if(ICM.gt.nzonec) ICM=IPGETNO(IX,2)-nzonec
                    call winscl('z',ICM)
                  endif
                endif

C Plot the current value - this has already been copied to Yold and
C then reset to forground colour.
                call etplot(GTIME,Yold(IX),IPEN,MARK)
                if(colok)call winscl('-',0)
                call forceflush()
              elseif (MENUTYPE.eq.5) then

C Stats, calculate standard deviation.
                FTOTSD(IX)=FTOTSD(IX)+((FAVE(IX)-VAL(IX))**2)
                NF(IX)=NF(IX)+1
              elseif (MENUTYPE.eq.7) then

C Hours above, calculate standard deviation.
                if (VAL(IX).gt.XQUERY)then
                  IQHRS(ix)=IQHRS(IX)+1
                endif
              elseif (MENUTYPE.eq.8) then

C Hours below.
                if (VAL(IX).lt.XQUERY)then
                  IQHRS(ix)=IQHRS(ix)+1
                endif
              endif
 402          continue
 400        CONTINUE
          endif

C Tidy up in preparation for the next loop.
          ctime=ctime+enterval*float(nout)
          gtime=gtime+float(nout)
C          write(6,*) 'incr ii ctime gtime ',ii,ctime,gtime
  200   continue
      else

C Case when averaging IS required
        if(idaver.eq.0) then
          ctime=float(ioh1-1)/24.0+ENTERVAL/2.+float(iods)
     &              +ENTERVAL*float(nout)/2.
          gtime=TSTART
        else
          ctime=float(ioh1-1)/24.0+enterval+float(iods)
     &              +ENTERVAL*float(nout)/2.
          gtime=TSTART
        endif
        do 211 jj=1,npget
  211     val(jj)=0.0
        jcount=0
        do 220 ii=istart, iend
          do 260 jstat=1,npget
            ipc =IPGETNO(jstat,2)
            nsnd=IPGETNO(jstat,3)
            if(IPGETNO(jstat,1).eq.9)then

C Asking for ambient db. Recover via call to mfclim
C Current timestep ii to a iday and hour (MINH).
C IPAR is one to get ambient db. Set ITS to 1 to get
C start of the hour.
              iday=INT(ctime)
              call DASYSTIME(II,TIME)
              MINH=INT(time)+1
              IPAR=1
              ITS=1
              call MFCLIM(IDAY,MINH,ITS,IPAR,VALC)
              val(jstat)=VALC
            elseif(IPGETNO(jstat,1).eq.12)then

C Asking for ambient rh.
C IPAR is four to get ambient db. Set ITS to 1 to get
C start of the hour.
              iday=INT(ctime)
              call DASYSTIME(II,TIME)
              MINH=INT(time)+1
              IPAR=4
              ITS=1
              call MFCLIM(IDAY,MINH,ITS,IPAR,VALC)
              val(jstat)=VALC
            else
              call fnddat(ii,ipc,ier)
              if(ier.ne.0)then
                write(6,*) 'An error in fnddat ',ii,ipc,ier
                goto 260
              endif
            endif

C Node temperature (C), first phase, 2nd phase.
            if(IPGETNO(jstat,1).eq.1)then
              val(jstat)=val(jstat)+csv1m(nsnd)
            elseif(IPGETNO(jstat,1).eq.2)then
              val(jstat)=val(jstat)+csv2m(nsnd)
            elseif(IPGETNO(jstat,1).eq.3)then
              val(jstat)=val(jstat)+csv3m(nsnd)
            endif

C Node relative humidity %
            if(IPGETNO(jstat,1).eq.4.and.csv2m(nsnd).gt.0.0) 
     &        val(jstat)=val(jstat)+pcrh2(csv1m(nsnd),
     &                   csv3m(nsnd)/csv2m(nsnd),1013.25)

C Node specific humidity (kg(v)/kg(a))
            if(IPGETNO(jstat,1).eq.5.and.csv2m(nsnd).gt.0.0) 
     &        val(jstat)=val(jstat)+csv3m(nsnd)/csv2m(nsnd)

C Node enthalpy (J/kg K)
            if(IPGETNO(jstat,1).eq.6.and.csv2m(nsnd).gt.0.0) then

C ..water..
              if(isv(ipc,nsnd).eq.10.or.isv(ipc,nsnd).eq.20) 
     &           val(jstat)=val(jstat)+csv1m(nsnd)*shth2o(csv1m(nsnd))

C ..dry air..
              if(isv(ipc,nsnd).eq.1.or.isv(ipc,nsnd).eq.11) 
     &           val(jstat)=val(jstat)+enthp2(csv1m(nsnd),0.0)

C ..moist air..
              if(isv(ipc,nsnd).eq.21) 
     &          val(jstat)=val(jstat)+enthp2(csv1m(nsnd),
     &                     csv3m(nsnd)/csv2m(nsnd))
            endif

C Node Dew Point temperature (C)
            if(IPGETNO(jstat,1).eq.7.and.csv2m(nsnd).gt.0.0.and.
     &         isv(ipc,nsnd).eq.21)  val(jstat)=val(jstat)
     &                          +dewpt(csv3m(nsnd)/csv2m(nsnd),1013.25)

C Additional output.
C Note, in this case 'nsnd' is NOT a node, 
C it is the additional output number for the component considered.
            if(IPGETNO(jstat,1).eq.10)val(jstat)=val(jstat)+apcout(nsnd)
  260     continue
          jcount=jcount+1

C If we are at last timestep of the hour then average it
C and process the information.
          if(jcount.eq.nout) then
            do 212 jj=1,npget
              val(jj)=val(jj)/float(nout)
  212       continue

C Have averaged val array for the current hour.
C Debug
C            write(6,'(f10.5,12(2x,G10.3))')
C     &            ctime,(val(jjj),jjj=1,npget)

            if (MENUTYPE.eq.4) then

C Split time into hours and minutes.
              call DASYSTIME(II,TIME)
              MINH=INT(time)
              MINU=INT((time-AINT(time))*60.+0.5)
              if(MINH.lt.10)then
                IF(MINU.LT.10)THEN
                  WRITE(DESCRH,'(a1,i1,a2,i1)')'0',INT(TIME),'h0',MINU
                ELSE
                  WRITE(DESCRH,'(a1,i1,a1,i2)')'0',INT(TIME),'h',MINU
                ENDIF
              else
                IF(MINU.LT.10)THEN
                  WRITE(DESCRH,'(i2,a2,i1)')INT(TIME),'h0',MINU
                ELSE
                  WRITE(DESCRH,'(i2,a1,i2)')INT(TIME),'h',MINU
                ENDIF
              endif
              outs='  '
              if (IHFLAG.eq.0) then 

C Split flow time into hours and minutes, repeating every 24 hours.
                write (outs,'(a5)') DESCRH
                K=6
              else
                write (outs,'(f10.4)') ctime
                K=11
              endif
              do 412 IXY=1,NPGET
                KE=K+11
                if(KE.le.124)then
                  write (outs(K:KE),'(f11.4)') VAL(IXY)
                  K=K+11
                endif
 412          continue
              call eddisp(itru,outs)
            else

C Do the other menu types. If graph reset scaling factor for appropriate axis.
              do 401 IX=1,NPGET
                if (MENUTYPE.eq.3) then
                  Iax=IPGETNO(IX,8)
                  call linescale(igl,Xadd,TSC,igb,Yadd(Iax),YSC(Iax))
                  if (TIMEold(IX).lt.0.0.or.ADDLIN.eq.1) then
                    TIMEold(IX)=0
                    Yold(IX)=VAL(IX)
                  endif
                  ioldpos=1
                endif

C For current time:
C Within requested output period.
                IF(GTIME.LT.(TSTART-1.0).or.GTIME.GT.TFINSH)goto 403
                if (MENUTYPE.eq.2) then

C First, recalculate time as it is done in table.F 
C If there is occupancy filter and occupancy then use pattern
C from zone iocpzone. Start with assumption of full occupancy.
                  IB = INT(((VAL(IX) - XBIN1) / DXBIN) + 1.)
                  if(IB.LE.0)then
                    iunder = iunder +1
                    IMISS = IMISS + 1
                  elseif(IB.GT.NBINS)then
                    iover = iover +1
                    IMISS = IMISS + 1
                  else
                    INBIN(IB) = INBIN(IB) + 1
                    IHITS = IHITS + 1
                  endif
                elseif (MENUTYPE.eq.3) then

C Draw next section of line.
                  if (ioldpos.eq.1)call etplot(TIMEold(IX),Yold(IX),0,0)
                  ioldpos=0
                  IPEN=1
                  MARK=0
                  RMK=AMOD((GTIME-(0.5*float(NOUT)/float(NPTS))),DT)
                  if (RMK.lt.0.001) MARK=6
                  TIMEold(IX)=GTIME
                  Yold(IX)=VAL(IX)

C If plotting colours for different components then recall which
C item we are dealing with. If the index of the item is greater
C than that of the number of colours re-use an earlier colour.
                  if(colok)then
                    if(IPGETNO(IX,2).gt.0)then
                      ICM=IPGETNO(IX,2)
                      if(ICM.gt.nzonec) ICM=IPGETNO(IX,2)-nzonec
                      if(ICM.gt.nzonec) ICM=IPGETNO(IX,2)-nzonec
                      call winscl('z',ICM)
                    endif
                  endif

C Plot the current value - this has already been copied to Yold and
C then reset to forground colour.
                  call etplot(GTIME,Yold(IX),IPEN,MARK)
                  if(colok)call winscl('-',0)
                  call forceflush()
                elseif (MENUTYPE.eq.5) then

C Stats, calculate standard deviation.
                  FTOTSD(IX)=FTOTSD(IX)+((FAVE(IX)-VAL(IX))**2)
                  NF(IX)=NF(IX)+1
                elseif (MENUTYPE.eq.7) then

C Hours above, calculate standard deviation.
                  if (VAL(IX).gt.XQUERY)then
                    IQHRS(ix)=IQHRS(IX)+1
                  endif
                elseif (MENUTYPE.eq.8) then

C Hours below.
                  if (VAL(IX).lt.XQUERY)then
                    IQHRS(ix)=IQHRS(ix)+1
                  endif
                endif
  403           continue
 401          CONTINUE
            endif

C Tidy up in preparation for the next loop.
            ctime=ctime+enterval*float(nout)
            gtime=gtime+float(nout)
            do 213 jj=1,npget
              val(jj)=0.0
  213       continue
            jcount=0
          endif
  220   continue
      endif

C Reset add profile.
      ADDLIN=0

C Calculate standard deviation and averages.
      if (MENUTYPE.eq.5) then
        if(npget.eq.0)then
          call usrmsg('No items selected....',' ','W')
          goto 1
        endif
        do 25 I=1,NPGET
          if (NF(I).lt.2) then
            FSTD(I)=0.
          else
            FSTD(I)=sqrt(FTOTSD(I)/real(NF(I)-1))
          endif
 25     continue
      endif

      if (MENUTYPE.eq.2) then

C For histogram, save largest number of hits in a bin (IFREQ).
        IFREQ=0
        DO 700 I=1,NBINS
          IFREQ=MAX(IFREQ,INBIN(I))
  700   CONTINUE

C Calculate cumulative frequency.
        ITOTAL=0
        DO 710 I=1,NBINS
          IF(I.EQ.1)ICBIN(I)=INBIN(I)
          IF(I.GT.1)ICBIN(I)=ICBIN(I-1)+INBIN(I)
          ITOTAL=ITOTAL+INBIN(I)
  710   CONTINUE

C Ask user how data should be presented.
        H(1)='Frequency distribution choices. '
        IDG=1
        IG=1
        CALL MENUATOL('Frequency choices','Frequency reports:',
     &    'a frequency distribution','b cumulative freq. dist.',
     &    'c summary table ','d frequency dist. + table',
     &    'e cumulative dist. + table','f none of the above',
     &    ' ',' ',' ',' ',' ',' ',IG,IDG,1)
        if(IG.eq.6) goto 1
        IF(IG.EQ.1.OR.IG.EQ.2.OR.IG.EQ.4.OR.IG.EQ.5)then

C Invoke drawing routines.  If echoing graphics to file, close it
C after image and advise user. If there is only one item then
C write heading with its identifier, otherwise include (...) to
C indicate that several flow items (e.g. nodes) are included.
          if(MMOD.eq.8)then

C Write graph head and set up axis positions and draw.
            IF(MMOD.EQ.8)THEN
#ifdef X11
              call feedbox(menuchw,2,igfw,igfh)
              call opengdisp(menuchw,LIMTTY,2,igdw,igdh)
#endif
              call win3d(menuchw,9,30,4,3,igl,igr,igt,igb,igw,igwh)
              call startbuffer()
            ENDIF

            WRITE(ETEXT,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &        SDESCR(11:lnblnk(SDESCR))
            CALL viewtext(ETEXT,1,0,1)
            TLABEL='  '
            if(NPGET.eq.1)then
              write (TLABEL,'(2a,f6.2)')
     &          GPLABEL(1)(1:lnblnk(GPLABEL(1))),', Bin width =',
     &          DXBIN
            else
              write (TLABEL,'(2a,f6.2)')
     &          GPLABEL(1)(1:lnblnk(GPLABEL(1))),
     &          ' (...), Bin width =',DXBIN
            endif
            if(IG.eq.1.or.IG.eq.4)CALL MODDRW(1,TLABEL)
            if(IG.eq.2.or.IG.eq.5)CALL MODDRW(2,TLABEL)
          else
            call edisp(iuout,' Sorry, not in graphic mode...')
          endif
        endif
        if(IG.EQ.3.OR.IG.EQ.4.OR.IG.EQ.5)then

C If output to file alter the edisp unit number and then write out
C a tabular report.
          itru = icout
          if(ixopen.eq.1)then
            itru = ixunit
            call edisp(icout,' Output being directed to file... ')
          endif

          call edisp(itru,' ')
          call edisp(itru,GPLABEL(1))
          call edisp(itru,
     &' Bin   data range       Distri-  freq.   cumulative  cumulative')
          call edisp(itru,
     &'                        bution    (%)   distribution  freq (%)')

C Provide initial range and then loop through each.
          XBINS=XBIN1
          XBIN=XBIN1+DXBIN
          DO 101 I=1,NBINS
            write(trange,'(F9.2,a,F9.2)') XBINS,'-',XBIN
            call SDELIM(trange,tranged,'N',IW)
            XBINS=XBIN
            XBIN=XBIN+DXBIN
            CFREQ=(FLOAT(ICBIN(I))/FLOAT(ITOTAL))*100.0
            FREQ=(FLOAT(INBIN(I))/FLOAT(ITOTAL))*100.0
            if(I.eq.1)then
              WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2,a,i6,a)') I,
     &        tranged,INBIN(I),FREQ,ICBIN(I),CFREQ,'  ',iunder,' below'
            elseif(I.eq.NBINS)then
              WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2,a,i6,a)') I,
     &        tranged,INBIN(I),FREQ,ICBIN(I),CFREQ,'  ',iover,' above'
            else
              WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2)')I,tranged,
     &        INBIN(I),FREQ,ICBIN(I),CFREQ
            endif
            call eddisp(itru,outs)
  101     CONTINUE
        endif

C Reset the number of requested items (NPGET) so that if another
C histogram metric is selected it will be the first one.
        call usrmsg('Current selections have been cleared,',
     &    'you will need to re-select.','P')
        NPGET=0
      elseif (MENUTYPE.eq.3) then

C Finally label lines, matching zone line colour if used.
        do 202 I=1,NPGET
          Iax=IPGETNO(I,8)
          call linescale(igl,Xadd,TSC,igb,Yadd(Iax),YSC(Iax))
          t20=GPLABEL(I)
          if(colok)then
            if(IPGETNO(I,2).gt.0)then
              ICM=IPGETNO(I,2)
              if(ICM.gt.nzonec) ICM=IPGETNO(I,2)-nzonec
              if(ICM.gt.nzonec) ICM=IPGETNO(I,2)-nzonec
              call winscl('z',ICM)
            endif
          endif
          call u2pixel(TIMEold(I),Yold(I),ixs,iys)
          call wstxpt(ixs,iys,t20)
          if(colok)call winscl('-',0)
          call forceflush()
 202    continue

C If there are optional user defined labels draw them.
        if(ngrlbl.gt.0)then
          do 201 ijj=1,ngrlbl
            t24=grlbl(ijj)
            call wstxpt(lblpx(ijj),lblpy(ijj),t24)
 201      continue
        endif
      elseif (MENUTYPE.eq.5) then

C Display stats.
        write (outs,'(a,a)') 
     &    '                            Maximum                 Minimum',
     &    '             Mean   Standard'
        call eddisp(itru,outs)
        write (outs,'(a,a)') 
     &    '                        value    occurrence     value    ',
     &    'occurrence     value  deviation'
        call eddisp(itru,outs)
        do 210 I=1,NPGET

C Create string description for time of maximum occurrance.
C TMAX & TMIN will have been recorded as a Julian day.
          MAXDOY=INT(TMAX(I))
          TMAXHR= (TMAX(I)-INT(TMAX(I)))*24.0
          call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
          call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
          write (XT,'(3a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
          MINDOY=INT(TMIN(I))
          TMINHR= (TMIN(I)-INT(TMIN(I)))*24.0
          call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
          call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
          write (NT,'(a,a,a)')DESCR(1:6),'@',DESCRH
          t20=GPLABEL(I)
          write (outs,'(a,1x,2(f8.2,2x,a12,2x),2(f7.2,3x))') t20,
     &      FMAX(I),XT,FMIN(I),NT,FAVE(I),FSTD(I)
          call eddisp(itru,outs)
 210    continue
        if(iocupf.eq.1.and.iocpzone.ne.0)then
          call eddisp(itru,'(above data filtered by occupancy)')
        endif
      elseif (MENUTYPE.eq.7) then

C Display stats + hrs above.
        write (outs,'(a,f7.2)')'Reporting number of hours above ',XQUERY
        call edisp(itru,outs)
        call edisp(itru,'  ')
        write (outs,'(a,a)') 
     &    '                           Maximum                Minimum',
     &    '              Mean    Hours'
        call eddisp(itru,outs)
        write (outs,'(a,a)')
     &    '                       value    occurrence    value    ',
     &    'occurrence      value   above'
        call eddisp(itru,outs)
        do 221 I=1,NPGET

C Create string description for time of maximum occurrance.
          MAXDOY=INT(TMAX(I)/24.)+IODS
          TMAXHR=AMOD(TMAX(I),24.)
          call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
          call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
          write (XT,'(a,a,a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
          MINDOY=INT(TMIN(I)/24.)+IODS
          TMINHR=AMOD(TMIN(I),24.)
          call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
          call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
          write (NT,'(3a)')DESCR(1:6),'@',DESCRH
          AYAB=float(IQHRS(I))*float(NOUT)/float(NTS)
          t20=GPLABEL(I)
          write (outs,'(a,1x,2(f8.2,2x,a12,1x),2f9.2)') t20,
     &      FMAX(I),XT,FMIN(I),NT,FAVE(I),AYAB
          call eddisp(itru,outs)
C          write (6,*) FMAX(I),FMIN(I),FAVE(I),FSTD(I),IQHRS(I)
 221    continue
        if(iocupf.eq.1.and.iocpzone.ne.0)then
          call eddisp(itru,'(above data filtered by occupancy)')
        endif
      elseif (MENUTYPE.eq.8) then

C Display stats + hrs below.
        write (outs,'(a,f7.2)')'Reporting number of hours below ',XQUERY
        call edisp(itru,outs)
        call edisp(itru,'  ')
        write (outs,'(a,a)') 
     &    '                           Maximum                Minimum',
     &    '              Mean    Hours'
        call eddisp(itru,outs)
        write (outs,'(a,a)') 
     &    '                       value    occurrence    value    ',
     &    'occurrence      value   below'
        call eddisp(itru,outs)
        do 230 I=1,NPGET

C Create string description for time of maximum occurrance.
          MAXDOY=INT(TMAX(I)/24.)+IODS
          TMAXHR=AMOD(TMAX(I),24.)
          call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
          call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
          write (XT,'(3a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
          MINDOY=INT(TMIN(I)/24.)+IODS
          TMINHR=AMOD(TMIN(I),24.)
          call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
          call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
          write (NT,'(3a)')DESCR(1:6),'@',DESCRH
          AYAB=float(IQHRS(I))*float(NOUT)/float(NTS)
          t20=GPLABEL(I)
          write (outs,'(a,1x,2(f8.2,2x,a12,1x),2f9.2)') t20,
     &      FMAX(I),XT,FMIN(I),NT,FAVE(I),AYAB
          call eddisp(itru,outs)
C          write (6,*) FMAX(I),FMIN(I),FAVE(I),FSTD(I),IQHRS(I)
 230    continue
        if(iocupf.eq.1.and.iocpzone.ne.0)then
          call eddisp(itru,'(above data filtered by occupancy)')
        endif
      endif

      goto 1

      END
