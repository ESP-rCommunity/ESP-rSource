C This file is part of the ESP-r system.
C Copyright Energy Systemiuouts Research Unit, University of
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

C*******************************************************
C This file contains the following subroutines:
C    newnet menu for defining plant comp, conn & cont temps.
C    pcdefn: edits control variables of available components.
C    condef: defines a connection between current receiving comp (ipcon)
C    cntdef: displays selected components for assigning containment type
C    getnod: displays a menu of nodes for a component (ipcomp).
C    initiv: initialises all variables and fills pcdesc(?) 
C    askpar: selects parameters of component (ipc) to edit.
C    askmfpar displays/edits a list of associated mass flow data
C    stype: displays general system types for user selection.
C    showpc: lets user select component parameter to change.
C    gettrs: display TRNSYS model types
C    ECMPLST presents list of plant components augmented with electrical 
C    eledit: edits electrical details for a particular plant comp.
C    CONTLYR allows the user to select a layer in a construction
C    SCANBPLINK: scans a control file for building plant linkages
C    ASKBPLINK: presents a list of plant components returning the index IS.
C    EDBPLINK: allows building plant link  to be added, deleted or edited.

C*********************** newnet ************************
C Newnet displays menu for defining plant components,
C connections and containment temperatures.

C     simtyp - holds description of simulation type.

      subroutine newnet(iedit,mode)

#include "plant.h"
#include "building.h"
 
      COMMON/C8/LPNAM
      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
 
      COMMON/FILEP/IFIL

C Mass flow network.
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/AFN/IAIRN,LAPROB,LAPRES,LAFRES,ICAAS(MCOM)

C Defaults.
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Plant network.
      common/C23/IFPNF,LPNF

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      common/pcnam/pcname(mpcom)

C Flag for plant ---> fluid link
      COMMON/PFLNK/IPFLNK
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/funit/iucfg, iupdb
      common/mfnchl/mfunit
      common/hfpar/hfpdsc(mpcom,madata)
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)

C Common for the building plant link
      COMMON/BPLINKG/NBPLINK,IBPLINKID(MCF),IPCLINK1(MCF),
     &IPCLINK2(MCF),IPCNLINK1(MCF),IPCNLINK2(MCF),
     &IPCLINKT(MCF),IPCLINKZ(MCF)

      common/cctlnm/ctldoc,lctlf
     
      character*20 simtyp(3)
      character*72  lpnam,laprob, lapres, lafres,LPNF

      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      dimension ival(mpcom)
      CHARACTER*44 ITEM(26)
      CHARACTER H*72,outs*124,ltmp*72,pcname*15, lctlf*72,ctldoc*248
      CHARACTER ICDIR*72, DOIT*248,mode*1,pcdesc*80
      character*68 mscdsc, cvrdsc,hfpdsc
      LOGICAL OK,dok,XST

      CALL EPAGE

C If file already exists then enter edit mode.  
      if(MODE.eq.'G'.and.iedit.eq.1) then 
        dok=.true.
        h(1)='This portion of the interface is under development. '
        h(2)='Using menu driven interface instead of graphics. '
        call askok('You cannot edit in graphical mode...switching to',
     &    'default menu driven mode. Continue? ' ,OK,dok,2)
        if(.not.ok) return                
        MODE='-'
      endif   
            
      IF(iedit.eq.1) THEN
        IOP=1
      ELSE
        IOP=4
      ENDIF

      lpnam =' '   

C Read in the configuration file.   
 210  CALL EFOPSEQ(IFPNF,LPNF,IOP,IER)
      if(IER.GT.0) then
        call edisp(iuout,' Error opening the configuration file.')
        return
      endif

      IF(IUPDB.EQ.0) IUPDB=IFPNF+1
      if(iedit.eq.1) then
        dok=.true.
        h(1)='In case you want to review the plant network. '
        CALL ASKOK(' Display a synopsis of the plant network?',' ',
     &    OK,dok,1)
        IF(OK)THEN
          call pltcfg(IFPNF,iupdb,iuout,1) 
        ELSE
          call pltcfg(IFPNF,iupdb,iuout,0) 
        ENDIF
        close(IFPNF) 
      endif  

      if(lctlf(1:3).NE.'UNK'.AND.lctlf(1:2).NE.'  ') CALL SCANBPLINK

C Open plant component data base.
      call opnpdb(iupdb,1)
      CALL USRMSG(' ',' ','-')

C Read components data from database
      if(iedit.eq.1) then
        do 5 ipc=1,npcomp
          nipc=ipc
          call rcdata(nipc,2)
          do 8 ipar=1, npi(ipc)
            hfpdsc(ipc,ipar)=mscdsc(ipar)
    8     continue
    5   continue
      endif

C First initialise all variables.
      if(iedit.ne.1) call initiv(ipcomp) 

C Let user select the simulation type required.
      if (npmtyp.eq.0) then
        call stype(ix,ival)
        if(ix.eq.0) return
        npmtyp=ix
        CALL USRMSG(' ',' ','-')
      endif

C Fill array for simulation type.
      simtyp(1)='Energy only.       '
      simtyp(2)='Energy + one phase.'
      simtyp(3)='Energy + two phase.'
      

C Initialise the plant fluid linkage (initially none)
      ipflnk=0

C Let the user pick a menu item.
   10 ino=-5

      write(item(1),'(a,a)')'a Problem name :',LPNF(1:20) 
      write(item(2),'(a,1x,a20)') 'b Simulation type :',
     &       simtyp(npmtyp)(1:20)  
      write(item(3),'(a,1x,a20)') 'c Project title   :',lpnam(1:20) 
      item(4)='  ---------------------'
      item(5)=' Problem status...'
      item(6)='  ---------------------'   
      write(item(7),
     &'(a,i3,a)')' No. of components ...           ( ',npcomp,' )'
      item(8)='d  Components   '
      item(9)='  ---------------------'  
      write(item(10),
     &'(a,i3,a)')' No. of connections ...          ( ',npcon,' )'
      item(11)='e  Connections  '
      item(12)='  ---------------------'  
      write(item(13),
     &'(a,i3,a)')' No. of containments ...         ( ',ncont,' )'
      item(14)='f  Containments  '
      item(15)='  ---------------------'  
      write(item(16),
     &'(a,i3,a)')' No. of comp. + electrical data  ( ',npel,' )'  
      item(17)='g  Electrical data '   
      item(18)='  ---------------------'
      item(19)='h Link to fluid flow network '
      item(20)='  ---------------------'
      write(item(21),
     &'(a,i3,a)')' No. of linked zones ...         ( ',nbplink,' )'
      item(22)='i Link plant to zone(s) '
      item(23)='  ---------------------    '
      item(24)='! Update plant config file '
      item(25)='? Help                     '
      item(26)='- Exit                     '
      nitms=26

C If newnet is called with mode 'G' skip menu definition.
      if(mode.eq.'G') then
        ino=nitms
        goto 13
      endif

   12 CALL EMENU('Network definition:edit',ITEM,NITMS,INO)
   13 continue

C Change or re-read problem.
      if (ino.eq.1) then 
        ltmp=LPNF
 301    CALL EASKS(ltmp,' Plant network definition file?',
     &    ' ',72,DPNF,'plant network file',IER,4)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LPNF=ltmp
        else
          call usrmsg('Please re-enter the file name. ',' ','W')
          goto 301
        endif

        inquire(file=LPNF,exist=xst)
        if(.not.xst) then
          CALL USRMSG(' ERROR: cannot find this file.',' ','W')
          GOTO 10
        else
          iedit=1
          GOTO 210            
        endif
      elseif(ino.eq.3) then

C Change project title for problem.
        h(1)='The project title should be used to give a concise '
        h(2)='description of the plant system being modelled.     '
        ltmp=lpnam
        CALL EASKS(ltmp,' Project title? ',' ',72,'no title given ',
     &    'lpnam',IER,2)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          lpnam=ltmp
        endif
        goto 12
      elseif(ino.eq.2) then

C Change simulation type for problem
        call stype(ix,ival)
        if(ix.eq.0) goto 12
        npmtyp=ix
        CALL USRMSG(' ',' ','-')
        goto 12
      elseif(ino.eq.8) then

C Now call subroutine to define each component
C in network.
        call ASKPCMP(' Components ','M',IS,IER)
      elseif(ino.eq.11) then

C Define connections.
        if(npcomp.lt.2) then
          CALL USRMSG(' ',
     &     'NOTE: you must first define two or more components. ','-')
            goto 12
        else
          call ASKPCON(' Connections','M',IS,IER)
        endif
      elseif(ino.eq.14) then

C Define containments.
        if(npcomp.lt.1) then
          CALL USRMSG(' ','You must first define a component.','-')
          goto 12
        else
          CALL ASKCONT(' Containments','M',IS,IER)
        endif
      elseif(ino.eq.17) then

C Allow definition of electrical details.  
        call ECMPLST(' Electrical','M',IS,IER)
      elseif(ino.eq.19) then

C Link the plant network to the flow network.
        h(1)='The plant network is linked to the fluid flow'
        h(2)='network in terms of plant connection to fluid'
        h(3)='flow connection. More than one plant connection'
        h(4)='can be mapped on to a fluid flow connection but'
        h(5)='NOT vice-versa. Each plant connection must have'
        h(6)='a mapping to the fluid flow network. During a'
        h(7)='simulation, the flow rate calculated in the '
        h(8)='flow network is attributed to the plant     '
        h(8)='connection.                                 '
        call phelpd('pdf network def',8,'-',0,0,ier)  

        h(1)='There are two methods of linking the '
        h(2)='plant network to the fluid flow network'
        h(3)='1. manual linkage of connections to conn-'
        h(4)='connections or auto generation of a fluid'
        h(5)='flow network (under development.)'
        call easkabc('Link plant to fluid flow network via ',
     &    'the following methods','manual linkage',
     &    'network generation (risky)','cancel',ipflnk,5)
        if(ipflnk.eq.3) then 
          ipflnk=0
          iflwn=0
          goto 10
        elseif(ipflnk.eq.1) then

C Manual linkage of connections
C Read the existing flow network
          IUF=IFIL+1
          CALL EFOPSEQ(IUF,LAPROB,1,IER)

C << Note: should check to see if LAPROB is a network flow file or
C << a graphic network file (as is done in esystem.F) 
          CALL EMFREAD(IUF,0,IER)
          IF(IER.NE.0)THEN
            CALL EDISP(IUOUT,' ')
            CALL EDISP(IUOUT,'ERROR: Problem reading fluid flow file!')
            CALL EDISP(IUOUT,'Check that one is defined for this model')
            CALL EDISP(IUOUT,' ')
            GOTO 10
          ENDIF

          do 207 ilink=1,npcon
            call edisp(iuout,' ')
            write(outs,'(a,I3)') 'Connection ',ilink 
            call edisp(iuout,outs)
            call edisp(iuout,'Connection data: ')  
            write(outs,'(a)') 
     &        'Component     |connects to | Component  ' 
            call edisp(iuout,outs)  
            write(outs,'(3a)') 
     &        PCNAME(IPC2(ilink)),'     -->    ',PCNAME(IPC1(ilink))
            call edisp(iuout,outs) 
            call edisp(iuout,' ') 
            outs=' '
            if(icffs(ilink).EQ.0)THEN
              write(OUTS,'(A,I3,A)') '(currently UNDEFINED)'
            else
               write(OUTS,'(A,I3,A)') '(currently ',icffs(ilink),')'         
            endif

C List the current fluid flow connections
  206       CALL USRMSG(
     &'This connection links to which fluid flow connection?',
     &outs(1:lnblnk(outs)),'-')
          call ASKCON('  Flow connections','-',INS,INE,IC,IER)            
            IF(IC.GT.0) THEN
              icffs(ilink)=IC
            ELSE
              dok=.true.
              h(1)='You have not selected a flow connection. One is'
              h(2)='required. If you say no then data will be lost.'
              h(3)='If you say yes you get a 2nd chance. '
              CALL ASKOK('No connection chosen, try again ?',
     &          '(see help)',OK,dok,3)
              IF(OK) THEN
                GOTO 206
              ELSE
                GOTO 10
              ENDIF
            ENDIF
 207      continue
          iflwn=1
        elseif(ipflnk.eq.2) then
         if(npcon.gt.0) then
           h(1)='Automatic mass flow network generation '
           h(2)='while functional is still at an early  '
           h(3)='stage of development. Hence for the safety'
           h(4)='of the novice user, flow network generation'
           h(5)='is password protected.'
           dok=.false.
           CALL ASKOK('Auto generate the flow network?','(see help)',
     &       OK,dok,5)
           IF(.not.ok) goto 215
           call easki(ians,' ','Password: ',0,'-',0,'-',0,
     &       'Password',IER,5)
           if(ians.ne.101) goto 215
           mfunit=10
           call askmfn(mfunit)
           call genmfn(2)
           close(mfunit)
           iflwn=1
         endif
        else
          goto 10 
        endif
      elseif(ino.eq.22) then

C Link building to plant
        CALL ASKBPLINK(' Linkages ','M',ILNK,IER)
        dok=.true.
        h(1)='To save any changes in building plant linkage you must'
        h(2)='update the control file.'
        CALL ASKOK(
     &    'Update control file to reflect changes in building-plant',
     &    'links?',OK,dok,2)
        ICTLF=IFIL+1
        IF(OK) THEN 
          CALL CTLWRT(ICTLF,IER)
          IF(IER.EQ.0) THEN
            CALL EDISP(IUOUT,' ')
            CALL EDISP(IUOUT,'Updating control file ...')
            CLOSE(ICTLF)
          ELSE
            CALL EDISP(IUOUT,' ')
            CALL EDISP(IUOUT,'ERROR: control file not updated properly') 
            CALL EDISP(IUOUT,'check that a control file is referenced') 
            CALL EDISP(IUOUT,'in the configuration file.') 
          ENDIF
        ENDIF
      elseif(ino.eq.nitms-2) then

C Overwrite the existing plant configuration file
        iexit=0
        goto 57
      elseif(ino.eq.nitms-1) then
        h(1) ='This is the network definition option of pdf.  Using'
        h(2) ='this option you can define the constituent parts'
        h(3) ='assosciated with an ESP-r plant network.'
        h(4) =' '
        h(5) ='When defining a the components of the network you are'
        h(6) ='presented with a list of available components from the'
        h(7) ='selected plant components database. Pick as many'
        h(8) ='components as are required then exit via the -Exit'
        h(9) ='option.'
        h(10)=' '
        h(11)='Connections are the linkages between plant components'
        h(12)='and are defined in terms of the RECEIVING node then'
        h(13)='the SENDING node (in the direction of fluid flow). '
        h(14)=' '
        h(15)='The mass diversion ratio is the fraction of mass flow'
        h(16)='sent from the SENDING node which reaches the'
        h(17)='RECEIVING node(0 - 1.0).'
        h(18)=' '
        h(19)='The containment specifies the destination of the'
        h(20)='component parasitic loss and may remain unspecified.'
        call phelpd('pdf network def',20,'-',0,0,ier)  
      elseif(ino.eq.nitms) then
        IF(MODE.EQ.'G') THEN
          H(1)=' Folder where icons live (absolute path).'
 220      CALL EASKS(icdir,' folder of icons: ',
     &          ' ',72,'/usr/esru/icons/plant','icon folder',IER,1)
          write(doit,'(a)') 'net '
          call runit(doit,'graph')
          call net2pnf
          MODE='-'
          goto 10
        ENDIF
        dok=.true.
        h(1)='Recent changes may be lost if they are not saved '
        h(2)='to file. '
        CALL ASKOK(' ','Do you want to save any changes?',OK,dok,2)
        iexit=0
        IF(.not.ok) then
          return
        ELSE
          iexit=1
          goto 57
        ENDIF 
      ELSE
        INO=-1
        GOTO 12
      endif
      goto 10

C Deal with saving the file.
 57   continue
      if(npcomp.gt.0) then
        h(1)='This will be created or overwite current configuration. '
        ltmp=LPNF
        CALL EASKS(ltmp,' Updated plant network file name? 
     &     ',' ', 72,' ',' file name',IER,1)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LPNF=ltmp
        else
          goto 57
        endif
      endif

  215 call update
      call descpc
      write(outs,'(a)')' Writing plant configuration file ...'
      call edisp(iuout,outs)
      call pcwrt
      if(IEXIT.eq.1)then
        return
      elseif(IEXIT.eq.0)then
        goto 10
      endif

      end

C*********************** pcdefn ************************
C pcdefn edits control variables of available components.
C Allows the user to specify a value for the control
C variable(s), if any, and to modify component default
C parametrs.

C   npcomp         - Number of selected plant components.
C   nci(?)         - Array holding number of control variables
C                    for component ?.
C   cvrdsc         - Holds description of each control variable.
C   cdata          - Holds value of each control variable of
C                    each selected component.
C   tadata(?,1)    - Number of parameters for component ?.
C   tadata(?,2..n) - value of each parameter.

      subroutine pcdefn

#include "plant.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      common/pophelp/h(60)
      common/pcnam/pcname(mpcom)

      DIMENSION  ival(mpcom)
      LOGICAL OK,dok
      character*68 mscdsc, cvrdsc
      character pcname*15,txt*68,H*72,pcdesc*80,str*15,outs*124

C Now display the menu.
      str=' '
  86  call showpc(ix,ival)
      if (ix.eq.0) return
      if(ix.gt.0) then   
        npcomp=npcomp+1

C Initialise component entry in dbase.
        npref(npcomp)=ival(1)  
     
C Read component name for current problem.
        h(1)='You can designate a meaningful name for the'
        h(2)='selected component. This name will be used in the  '
        h(3)='current problem. The name must not exceed 15 characters.'
        h(4)='N.B no two component names should be the same! '
        CALL EASKS(str,' Component name in problem (No spaces) ? ',
     &  '(15 characters max)',15,' ',' component name',IER,4)
        pcname(npcomp)=str(1:15)

C Call subroutine to read data associated with selected
C component from plant data base.
        call rcdata(npcomp,1)
        if(nci(npcomp).gt.0) then

C If component has any control variable(s),
C prompt user to enter its value.
          do 101 jj=1, nci(npcomp)
            txt=cvrdsc(jj)
            value=0.0
          h(1)='A control variable may be subjected to control action.'
          h(2)='For example, in a heating coil model, the heat'
          h(3)='injected to the air may be an elected control variable.'
          h(4)='If no control loop is defined, a realistic value must'
          h(5)='be assigned to this variabel otherwise its value is '
          h(6)='determined on the basis of some sensed condition and'
          h(7)='the active control law.'
          h(8)=' '
            CALL EASKR(value,' ',txt,0.,'-',0.,'-',0.,
     &        ' item value?',IER,8)
            cdata(npcomp,jj)=value
  101     continue
        endif
        nmisc=npi(npcomp)
        if(nmisc.gt.0) then

C If component has miscellaneous data, display it and
C ask user if he wishes to modify any parameters.
          write(outs,'(2a)') 'Current data for ',pcname(npcomp)
          call edisp(iuout,outs)
          do 909 jj=1,npi(npcomp)
            write(outs,'(a50,a3,g12.5)')
     &        mscdsc(jj),' : ',tadata(npcomp,jj)
            call edisp(iuout,outs)
  909     continue
          call edisp(iuout,' ')
          dok=.false.
          h(1)='The current component has additional parameters '
          h(2)='which can be altered. '
          CALL ASKOK(' ',
     &      'Do you want to change component parameters?',OK,dok,2)
          if(.not.OK) return

C Display miscellaneous data items and let user pick one.
  105     call askpar(itm,ival,npcomp)
          if(itm.ne.0.and.ival(1).ne.0) then

C Read new value for selected item(s) then redisplay.
             do 106 iij = 1,itm
               txt=mscdsc(ival(iij))
               dvalue=tadata(npcomp,ival(iij))
               value=dvalue
               call easkr(value,' ',txt,0.,'-',0.,'-',dvalue,
     &           ' item value?',IER,0)
               tadata(npcomp,ival(iij))=value
  106        continue
             write(outs,'(2a)') 'Updated data for ',pcname(npcomp)
             call edisp(iuout,outs)
             do 910 jj=1,npi(npcomp)
               write(outs,'(a50,a3,g12.5)')
     &           mscdsc(jj),' : ',tadata(npcomp,jj)
               call edisp(iuout,outs)
  910        continue
             call edisp(iuout,' ')
             goto 105
          endif
        endif
        return
      endif
      return
      end

C*********************** condef ************************
C condef defines/edits a connection (ipcon)between current receiving
C comp and sending comp. emod is 'G' to get receiving comp and node.
C A connection type must be specified for each connection.

C   contyp         - Holds description of each connection type.
C   npcon          - number of connections currently defined.
C   ipc1(?)        - receiving component number for connection ?.
C   ipn1(?)        - recieving node number for connection ?.
C   ipct(?)        - connection type for connection ?.
C   ipc2(?)        - sending component number for connection ?.
C   ipn2(?)        - sending node number for connection ?.
C   pcondr(?)      - mass diversion ratio for connection ?.
C   pconsd(?,1..2) - Holds supplementary data for
C                    connection ?.
C   nnodes(?)- number of nodes in component ?.
C   all these arrays are defined in 'plantdf.h'
C   idcon(?) - assigns a connection id number
      subroutine condef(emod,ipcon)

#include "plant.h"
      common/pcnam/pcname(mpcom)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/CONET/IPCOMPA,IPCOMPB
      common/idcn/idcon(mpcon)   
      common/pophelp/h(60)
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      CHARACTER*72 H 
 
      dimension ival(mpcom), contyp(5)    
      character contyp*45,head*19,emod*1,outs*124,str*72,pcname*15
      integer num
      num=0
      
      str=' '

C Let user select receiving component and node in that component.
      IF(EMOD.NE.'G') THEN
        head='Receiving compt:'
        call usrmsg(' ',' ','-')
        IF(IPC1(IPCON).NE.0)THEN
          write(str,'(3A)') '( currently ',PCNAME(IPC1(IPCON)),' )'
        ELSE
          write(str,'(A)') '( currently UNDEFINED )'  
        ENDIF 
        call usrmsg('Select RECEIVING component for this connection:',
     &str,'-')
        CALL ASKPCMP(head,'-',IPCOMP,IER)
        call usrmsg(' ',' ','-')
      ELSE 
         IPCOMP=IPCOMPA
      ENDIF

C If no receiving component selected return.
      if (ipcomp.eq.0)then
        return
      else
        ipc1t=ipcomp
        if(nnodes(ipcomp).eq.1) then
          ipn1t=1
        elseif(nnodes(ipcomp).gt.1) then
           call getnod(ipcomp,knode)
           if(knode.eq.0) return
           ipn1t=knode
        endif
      endif

C Fill array with existing connection types.
      isvt=isv(ipc1t,ipn1t)
      contyp(1)='identical temperature + humidity ratio.(N/A)'
      if(isvt.eq.1.or.isvt.eq.11.or.isvt.eq.21)then
        contyp(2)='From known temperature + humidity ratio.    '
      else
        contyp(2)='From known temperature                      '
      endif
      contyp(3)='From another component.                     '
      contyp(4)='From a building zone air.                   '
      contyp(5)='From ambient air.                           '

C Let user select a connection type.
  107 itm=1
      nctype=5
      h(1) ='The user can connect the RECEIVING component from '
      h(2) ='one of several different elements:              '
      h(3) =' '
      h(4) =' a - Not available yet.  '
      h(5) =' '
      h(6) =' b - Known temperature and humidity ratio; the user '
      h(7) ='     supplies the temperature and humidity ratio of the'
      h(8) ='     fluid flowing to this component.                  '
      h(9) =' '
      h(10)=' c - Another component; the user selects the plant    '
      h(11)='     component SENDING to the RECEIVING component.    '  
      h(12)=' '
      h(13)=' d - Building zone; the selects the building zone which'
      h(14)='     SENDS fluid to the RECEIVING component.           '
      h(15)=' '
      h(16)=' e - Ambient air; the component RECEIVES fluid from '
      h(17)='     outside.                                       '
      IF(IPCT(IPCON).NE.0)THEN
        write(str,'(3A)') '( currently ',CONTYP(IPCT(IPCON)),' )'
      ELSE
        write(str,'(A)') '( currently UNDEFINED )'  
      endif                                       
      call usrmsg(
     &'Select the entity SENDING fluid to this component.',str,'-')
      call epicks(itm,ival,'What is this component connected to? ',
     &    '(see help)',45,nctype,contyp,
     &    ' Connection types',ier,17)
      call usrmsg('  ',' ','-')
      if(ier.ne.0) goto 107
      if(itm.eq.0) return
      ipctt=ival(1)

C Case for connection type 2.
C Known temperature and humidity ratio.
      if(ipctt.eq.2) then
        IF(EMOD.NE.'G') THEN 
          value=20.0
          dvalue=value
          CALL EASKR(value,' ',
     &   'Enter temperature value (C)',0.,'-',0.,'-',dvalue,
     &   ' temperature ?',IER,0)
          supdt1=value
          value=0.001
          dvalue=value
          if(isvt.eq.1.or.isvt.eq.11.or.isvt.eq.21)then
            CALL EASKR(value,' ',
     &   'Enter humidity ratio value (kg v/kg a)',0.,'-',dvalue,
     &   '-',dvalue,' humidity ratio?',IER,0)
           supdt2=value
         else
           supdt2=0.000
         endif
         h(1)='This component receives air from a known temperature+'
         h(2)='humidity ratio but the inlet flow rate is unknown.'
         h(3)='You have two choices:  '
         h(4)=' '
         h(5)='a) select the component in the network which '
         h(6)='supplies this component.                     '
         h(7)=' ' 
         h(8)='b) OR, if no component supplies flow to this one '
         h(9)='i.e. this the 1st component in an open loop system,'
         h(10)='then select ANY driving component (fan) in the '
         h(11)='network and modify the connection mass diversion '
         h(12)='ratio (MDR) such that the fan flow*MDR equals the '
         h(13)='flow you want. The flow rate will then be used as '
         h(14)='the inlet flow rate to this component.'
         h(15)='The connections to the driving component are '
         h(16)='NOT affected. '
         CALL PHELPD('supply component',16,'-',0,0,IER)
         IF(IPC2(IPCON).NE.0)THEN
           write(str,'(3A)') '( currently ',PCNAME(IPC2(IPCON)),' )'
         ELSE
           write(str,'(A)') '( currently UNDEFINED )'  
         endif 
         CALL USRMSG('Select a component:  ',str,'-')
         head='DRIVING compt:'
         CALL ASKPCMP(head,'-',IPCOMP,IER)
         CALL USRMSG(' ',' ','-')
        ELSE 
           IPCOMP=IPCOMPB
        ENDIF
        if (ipcomp.eq.0) then
          return 
        else
          ipc2t=ipcomp
          if(nnodes(ipcomp).eq.1) then
            ipn2t=1
          elseif(nnodes(ipcomp).gt.1) then
            call getnod(ipcomp,knode)
            if(knode.eq.0) return
            ipn2t=knode
          endif
        endif

C Case for connection to another component. 
      elseif(ipctt.eq.3)then
        IF(EMOD.NE.'G')THEN
          IF(IPC2(IPCON).NE.0)THEN
            write(str,'(3A)') '( currently ',PCNAME(IPC2(IPCON)),' )'
          ELSE
            write(str,'(A)') '( currently UNDEFINED )'  
          endif
          call usrmsg('Select the SENDING component:',str,'-')
          head='SENDING compt:'
          CALL ASKPCMP(head,'-',IPCOMP,IER)
          call usrmsg(' ',' ','-')
        ELSE 
          IPCOMP=IPCOMPB
        ENDIF
        if (ipcomp.eq.0) then
          return 
        else
          ipc2t=ipcomp
          if(nnodes(ipcomp).eq.1) then
            ipn2t=1
          elseif(nnodes(ipcomp).gt.1) then
            call getnod(ipcomp,knode)
            if(knode.eq.0) return
            ipn2t=knode
          endif
        endif

C Case for connection to building zone.
      elseif(ipctt.eq.4) then
        IF(EMOD.NE.'G') THEN
          value=1.0
          dvalue=value

C Display  a list of the available zones ....
          WRITE(OUTS,'(A)') 
     &'Select zone from which the component RECEIVES flow. '
          IZ=NINT(pconsd(ipcon,1))
          CALL ASKZONE(IZ,0,OUTS,'Select zone','-','plnt conn',IER)
          call usrmsg(' ',' ','-')
          supdt1=IZ

          h(1)='This component receives air from the specified zone'
          h(2)='and is therefore in the return part of the plant   '
          h(3)='system. Please identify the component which supplies '
          h(4)='air to the zone. '
          CALL PHELPD('supply component',4,'-',0,0,IER) 
          CALL PHELPD('ambient air ',15,'-',0,0,IER) 
          IF(IPC2(IPCON).NE.0)THEN
            write(str,'(3A)') '(currently ',PCNAME(IPC2(IPCON)),')'
          ELSE
            write(str,'(A)') '(currently UNDEFINED)'  
          endif
          call usrmsg('Select the SUPPLY component:',str,'-')
          head='SUPPLY compt:'
          CALL ASKPCMP(head,'-',IPCOMP,IER)
          call usrmsg(' ',' ','-')
        ELSE 
          IPCOMP=IPCOMPB
        ENDIF
        if (ipcomp.eq.0) then
          return 
        else
          ipc2t=ipcomp
          if(nnodes(ipcomp).eq.1) then
            ipn2t=1
          elseif(nnodes(ipcomp).gt.1) then
            call getnod(ipcomp,knode)
            if(knode.eq.0) return
            ipn2t=knode
          endif
        endif                

C Case for connection to ambient air.
      elseif(ipctt.eq.5) then
        IF(EMOD.NE.'G') THEN
          h(1)='This component receives air from outside, however'
          h(2)='the inlet flow rate is unknown. You have two choices'
          h(3)='in how to supply this information:  '
          h(4)='  '
          h(5)='a) select the component which vents air to outside '
          h(6)='if its flow rate is the same as this one.'
          h(7)=' ' 
          h(8)='b) OR, if the vent and supply component flow rates  '
          h(9)='differ then select ANY driving component (fan) in the'
          h(10)='plant network and modify the mass diversion ratio '
          h(11)='(MDR) such that the fan flow*MDR equals the flow '
          h(12)='you want. The flow rate will then be used as'
          h(13)='the inlet flow rate to this component. The '
          h(14)='connections to the driving component are NOT '
          h(15)='affected.'
          CALL PHELPD('ambient air ',15,'-',0,0,IER) 
          IF(IPC2(IPCON).NE.0)THEN
            write(str,'(3A)') '(currently ',PCNAME(IPC2(IPCON)),')'
          ELSE
            write(str,'(A)') '(currently UNDEFINED)'  
          endif
          CALL USRMSG('Select a component: ',str,'-')
          head='DRIVING compt:'
          CALL ASKPCMP(head,'-',IPCOMP,IER)
          call usrmsg(' ',' ','-')
        ELSE 
          IPCOMP=IPCOMPB
        ENDIF
        if (ipcomp.eq.0) then
          return 
        else
          ipc2t=ipcomp
          if(nnodes(ipcomp).eq.1) then
             ipn2t=1
           elseif(nnodes(ipcomp).gt.1) then
            call getnod(ipcomp,knode)
            if(knode.eq.0) return
            ipn2t=knode
          endif
          supdt1=0.0
          ipctt=ipctt-1
        endif
      else     
        return
      endif

C Ask user for a mass diversion ratio for
C this connection.
      value=1.0
      dvalue=value
      H(1)='The mass diversion ratio is the amount of fluid coming '
      H(2)='from the SENDING component that reaches the RECEIVING  '
      H(3)='component. This can be used to specify leakage e.g. if ' 
      H(4)='2% of the air is lost between the sending and receiving'
      H(5)='component, then the diversion ratio is 0.98.           '
      H(6)=' '
      H(7)='Alternatively the mass diversion ratio can be used to  '
      H(8)='specify flow splitting e.g. if a fan supplies four     '
      H(9)='identical ducts, then the mass diversion ratio will    '
      H(10)='be 0.25 between each duct and the fan.                '
      H(11)=' '
      H(12)='NB, the flow rates calculated using mass diversion  '
      H(13)='are NOT used if the plant network is linked to a flow '
      H(14)='network - the flows calculated by the flow network    '
      H(15)='take precidence.                                      '
      CALL EASKR(value,'Mass diversion ratio for this connection(-)',
     &    '(see help)',0.,'-',
     &    0.,'-',dvalue,' mass diversion ?',IER,15)
      pcondrt=value

C Now assign connection data to appropriate variables.
C Also increment connection counter.
      if(emod.eq.'-'.OR.EMOD.EQ.'G') then
        if(itm.ne.0) then
          npcon=npcon+1
          ipc1(npcon)=ipc1t
          ipn1(npcon)=ipn1t
          ipct(npcon)=ipctt
          ipc2(npcon)=ipc2t
          ipn2(npcon)=ipn2t
          pcondr(npcon)=pcondrt
          pconsd(npcon,1)=supdt1
          pconsd(npcon,2)=supdt2
          idcon(npcon)=npcon
          return
        endif
      elseif(emod.eq.'E') then
        num=ipcon
        ipc1(num)=ipc1t
        ipn1(num)=ipn1t
        ipct(num)=ipctt
        ipc2(num)=ipc2t
        ipn2(num)=ipn2t
        pcondr(num)=pcondrt
        pconsd(num,1)=supdt1
        pconsd(num,2)=supdt2
      endif
      return
      end

C*********************** cntdef ************************
C cntdef displays selected components for assigning containment type
C for the selected component.

C   cnttyp         - holds description of each containment type.
C   ncont          - number of specified containments.
C   ipcc(?)        - component number for containment ?.
C   indcp(?)       - type of containment ?.
C   cntdat(?,1..3) - supplementary data for containment ?.

      subroutine cntdef(emod,icont)

#include "plant.h"
#include "building.h"

      common/outin/iuout,iuin
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)    
      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)

      common/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/FILEP/IFIL
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      COMMON/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      dimension ival(mpcom), cnttyp(4)
      character cnttyp*45,  head*19, emod*1, outs*124, h*72
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL

      LOGICAL CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      LOGICAL MODIFY,MODLEN,MODBND

C Fill array with existing containment types.
      cnttyp(1)='Ambient air temperature.             '
      cnttyp(2)='A plant component temperature.       '
      cnttyp(3)='Fixed temperature.                   '
      cnttyp(4)='Zone (air/surface/const) temperature.'

C Let user select a component.
      call usrmsg(
     &'Which component does this containment apply to?',' ','-')
 9    head=' Select component:'
      CALL ASKPCMP(head,'-',IPCOMP,IER)
      call usrmsg(' ',' ','-')
      if (ipcomp.eq.0)  return
      if(ipcomp.gt.0) then
         ipcct=ipcomp

C Check that the component does not already have a 
C containment specified.
         do 11 ichk=1,ncont
            if(emod.eq.'E'.and.ichk.eq.icont) then
              continue
            else
              if (ipcc(ichk).eq.ipcct) then
                call edisp(iuout,' ')
                call edisp(iuout,
     &'A containment has already been defined')
                call edisp(iuout,'for this component !')
                goto 9
              endif
            endif
 11      continue

C Let user select containment type.
  107    itm=1
         ncontyp=4
         call epicks(itm,ival,' ',
     &    ' Select containment type: ',45,ncontyp,cnttyp,
     &    ' Containments types',ier,0)
         if(ier.ne.0) goto 107
         if(itm.eq.0) return
         indctt=ival(1)-1

C Case for containment type 0.
C ambient air temperature + specified (de)increment.
         if(indctt.eq.0) then
           value=0.0
           dvalue=value
           CALL EASKR(value,' ',
     &      'Enter increment or deincrement value (C):',0.,'-',
     &      0.,'-',dvalue,' (de)increment ?',IER,0)
           cntdt1=value
           cntdt2=0.0
           cntdt3=0.0

C Type 1 containment: containment is at temperature of a specified plant
C node - including the self containment case if cntdt1=0 - plus
C specified (de)increment cntdt3.
         elseif(indctt.eq.1) then
           head='Select a component:'
           CALL ASKPCMP(head,'-',IPCOMP,IER)
           if (ipcomp.eq.0) return
           cntdt1=ipcomp
           if(nnodes(ipcomp).eq.1) then
             cntdt2=1.0
           elseif(nnodes(ipcomp).gt.1) then
             call getnod(ipcomp,knode)
             if(knode.eq.0) return
             cntdt2=float(knode)
           endif
           value=0.0
           dvalue=value
           CALL EASKR(value,' ',
     &        'Enter increment or deincrement value (C):',0.,'-',
     &        0.,'-',dvalue,' (de)increment ?',IER,0)
           cntdt3=value

C Type 2 containment: specified temperature cntdt1.
         elseif(indctt.eq.2) then
           value=20.0
           dvalue=value
           CALL EASKR(value,' ','Enter fixed temperature value (C):',
     &        0.,'-',0.,'-',dvalue,' temperature ?',IER,0)
           cntdt1=value
           cntdt2=0.0
           cntdt3=0.0

C Type 3 containment: zone cntdt1, surface cntdt2
C and construction node cntdt3 counted from outside.
         elseif(indctt.eq.3) then
           WRITE(OUTS,'(A)')'Select a zone for the containment:'
           IZ=0
           CALL ASKZONE(IZ,0,OUTS,'Select zone','-','plnt conn',IER)
           if(IZ.eq.0) RETURN
           cntdt1=float(IZ)
           h(1)='You have several options you can select from:'
           h(2)=' '
           h(3)='Zone air point - the component losses/gains heat'
           h(4)='to the zone environmental point. '
           h(5)=' '
           h(6)='At a Surface the component losses/gains heat'
           h(7)='to the selected surface. '
           h(8)=' '
           h(9)='At a Surface the component losses/gains heat'
           h(10)='to a node with a surface construction. '
           CALL EASKABCD('Select the location in the zone ',
     &' ','At the air point','At a surface','Within a surface',
     &'Continue',ICLOC,10)
            IF(ICLOC.EQ.1)THEN
              cntdt2=0.0
              cntdt3=0.0
            ELSEIF(ICLOC.EQ.4) THEN
              RETURN
            ELSEIF(ICLOC.EQ.2.OR.ICLOC.EQ.3)THEN
              CALL EGOMIN(IFIL+1,LGEOM(IZ),IZ,1,0,iuout,IER)

C General image option flags.
              ITDSP=1
              ITBND=1
              ITEPT=0
              ITZNM=0
              ITSNM=0
              ITVNO=1
              ITORG=1
              ITSNR=1
              ITGRD=1
              GRDIS=0.0

              MODIFY=.TRUE.
              MODBND=.TRUE.
              CALL INLNST(1)
              ITVNO=0
              nzg=1
              nznog(1)=IZ
              izgfoc=IZ
              CALL ADJVIEW(ITRC,IER)
              MODIFY=.TRUE.
              MODBND=.TRUE.

              ISO=1
              CALL EPMENSV
              CALL EASKSUR(IZ,ISO,'-',
     &        'Select surface for the containment.',' ',IER)
              CALL EPMENRC 
              IF(ISO.EQ.0)RETURN
              cntdt2=ISO
              cntdt3=0.0
              IF(ICLOC.EQ.3)THEN
                CALL CONTLYR(IZ,ISO,IND)
                cntdt3=float(IND)
              ENDIF
            ENDIF
         endif

C Now assign containment data and increment
C number of containments counter.
        if (emod.eq.'-') then
          ncont=ncont+1
          ipcc(ncont)=ipcct
          indcp(ncont)=indctt
          cntdat(ncont,1)=cntdt1
          cntdat(ncont,2)=cntdt2
          cntdat(ncont,3)=cntdt3
        elseif (emod.eq.'E') then

C In the case of containment editing, don't increment the number of 
C the NCONT counter.
         ipcc(icont)=ipcct
         indcp(icont)=indctt
         cntdat(icont,1)=cntdt1
         cntdat(icont,2)=cntdt2
         cntdat(icont,3)=cntdt3
        endif         
      endif
      return
      end

C*********************** getnod ************************
C getnod displays a menu of nodes for a component (ipcomp). It returns
C selected node number (knode).

      subroutine getnod(ipcomp,knode)
 
#include "plant.h"

      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)

      dimension nodesc(mnodec), ival(mpcom+2)
      character nodesc*15

      do 50 inod=1, nnodes(ipcomp)
      isvt=isv(ipcomp,inod)
      if(isvt.eq.0.or.isvt.eq.10.or.isvt.eq.20) then
         nodesc(inod)='Water node'
      elseif(isvt.eq.1.or.isvt.eq.11.or.isvt.eq.21) then
         nodesc(inod)='Air node'
      elseif(isvt.eq.9.or.isvt.eq.19.or.isvt.eq.29) then
         nodesc(inod)='Solid node'
      endif
   50 continue
      knode=0
   55 itm=1
      call epicks(itm,ival,' ',
     &   ' Select a node: ',15,nnodes(ipcomp),nodesc,
     &   ' Component nodes',ier,0)
      if(ier.ne.0) goto 55
      knode=ival(1)
      return
      end

C*********************** initiv ************************
C initiv initialises all variables and fills pcdesc(?) 
C with description of each component in database.

      subroutine initiv(ipcomp)

#include "plant.h" 

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      character pcdesc*80

C Initialise number of selected components variable.
      npcomp=0

C Initialise number of connections variable.
      npcon=0

C Initialise number of containments variable.
      ncont=0

C Initialise number of electrical components
      npel=0

C Initialise common block arrays.
      do 5 ipcomp=1, mpcom
         npref(ipcomp)=0
         nci(ipcomp)=0
         npi(ipcomp)=0
         do 8 nmsc=1, madata
           tadata(ipcomp,nmsc)=0.0
    8    continue
         do 9 icv=1, mconvr
           cdata(ipcomp,icv)=0.0
    9    continue
    5 continue

C Fill array 'pcdesc' with components description
      call descpc
      return
      end

C ******************** askpar ************************
C askpar selects parameters of component (ipc) to edit.
      subroutine askpar(nitm,ival,ipc)

#include "plant.h" 

      common/pophelp/h(60)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA) 
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pcnam/pcname(mpcom)

      character mscdsc*68, cvrdsc*68,pcname*15,txt*28
      CHARACTER h*72, parlst(madata)*70
      dimension ival(mpcom)

C Display miscellaneous data items.
  105 nitm=npi(ipc)
      h(1)='All components in the data base have default data assigned'
      h(2)='to their parameters. The value of these parameters may be'
      h(3)='changed when for example using a duct with a different'
      h(4)='length. Select the parameters you wish to change and enter'
      h(5)='the new values. '
      h(6)=' '
      do 909 jj=1,npi(ipc)
         write(parlst(jj),'(a50,a3,g12.5)')
     &          mscdsc(jj),' : ',tadata(ipc,jj)
  909 continue
      write(txt,'(2a)') 'Plant comp : ',pcname(ipc)
      call epicks(nitm,ival,' ',
     &  ' Which item(s) to change: ',70,npi(ipc),parlst,txt,ier,6)
      if(ier.ne.0) goto 105
      return
      end

C **********askmfpar*********************************
C askmfpar displays/edits a list of associated mass flow data
C fpr the component (ipc).
      subroutine askmfpar(ipc)

#include"plant.h"
      common/outin/iuout,iuin
      common/pophelp/h(60)
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      common/mfcpar/itypmf(mpcon), isdcmf(mpcon), icnnmf(mpcon),
     &              supcmf(mpcon,17), ltpcmf(mpcon), isdimf(mpcon)
      common/mfddsc/mfdatstr(mpcon,17)
      common/tmpmfdt/tmpmfdat(mpcon,17),imfed(mpcon,17)
      common/pcnam/pcname(mpcom)
      dimension list(21)
      CHARACTER ltpcmf*60, mfdatstr*40,pcname*15
      character txt*27,H*72, list*60, head*44
      
      nconns=0
      do 110 inod=1, nnodes(ipc)
        if(ndcon(ipc,inod).gt.0) nconns=nconns+ndcon(ipc,inod)
  110 continue
      if(nconns.gt.0) then
        call getmfc(ipc,nconns)
      endif

C Build a menu to allow editing of each node in turn.
      icon=1
      ino=-2
115   k=0
      if(isdcmf(icon).eq.0) then
        k=1
        list(k)=' No data for node.'
        goto 121
      endif
      do  120 k=1,isdcmf(icon)
        if(imfed(icon,k).eq.1) then
          write(list(k),'(i2,1x,a40,2x,F10.3)') k,mfdatstr(icon,k),
     &      tmpmfdat(icon,k)
        else
          write(list(k),'(i2,1x,a40,2x,F10.3)') k,mfdatstr(icon,k),
     &      supcmf(icon,k)
        endif
 120  continue
      k=k-1
 121  list(k+1)=' ---------------------------------'
      list(k+2)='+ Next node                       '
      list(k+3)='? Help                            '
      list(k+4)='- Exit                            '
      nlist=k+4
      write(txt,'(a,a)')pcname(ipc),' connect: '
      write(head,'(a,i2)') txt, icon
 130  call emenu(head,list,nlist,ino)
      if(ino.ge.1.and.ino.le.k) then 
        if(ino.eq.1.and.isdcmf(icon).eq.0) goto 130
        deflt=supcmf(icon,ino)
        if(imfed(icon,ino).eq.1) deflt=tmpmfdat(icon,ino)
        write(txt,'(a)')mfdatstr(icon,ino)(1:27)
        call easkr(value,' ',txt,0.,'-',0.,'-',deflt,' item value ?',
     &    IER,0)
       tmpmfdat(icon,ino)=value
       imfed(icon,ino)=1
       goto 115
      elseif(ino.eq.k+2) then
        if(icon+1.le.nconns) then
          icon=icon+1 
        else
          call edisp(iuout,' Last connection !')
        endif
        goto 115
      elseif(ino.eq.k+3) then
        h(1)='This facility allows you to edit the data for the'
        h(2)='mass flow component(s) mapped onto the current'
        h(3)='plant components.'
        call phelpd('pdf network flow node list',3,'-',0,0,IER)
        goto 130
      elseif(ino.eq.k+4) then 
        return
      else
        goto 130
      endif
      end
 

C ******************** stype ************************
C stype: Displays general system types for user selection.
      subroutine stype(ix,ival)

#include "plant.h"
 
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      dimension ival(mpcom)
      character*32 simtyp(4)
      CHARACTER H*72

C Fill array for simulation type.
      simtyp(1)=' Mechanical ventilation system'
      simtyp(2)=' Water-based heating system   '
      simtyp(3)=' Electric heating system      '
      simtyp(4)=' General HVAC system          '

C Let user select the system type required.
   70 IX=1
      h(1) ='ESP-r allows the modelling of all types of plant system.'
      h(2) ='Selecting one of the system types in the menu allows the'
      h(3) ='simulation to be optimised around the system being      '
      h(4) ='modelled. Selecting a system type automatically sets the '
      h(5) ='type of plant simulation to be conducted: '
      h(6) =' '
      h(7) ='Mechanical ventilation system - energy, air and moisture  '
      h(8) ='flows are simulated (energy and two phase flow). In this  '
      h(9) ='system type the ventilation components are modelled       '
      h(10)='explicitly but the boiler system is modelled in a more    '
      h(11)='abstract fashion i.e. using flux-based heating and        '
      h(12)='cooling coil models. The main working fluid is air.       '
      h(13)=' '
      h(14)='Water-based heating system - this system type option is   '
      h(15)='useful for modelling wet central heating systems and solar'
      h(16)='heating systems in which the working fluid is low         '
      h(17)='temperature hot water, so energy and water flows are      '
      h(18)='simulated (energy and one phase flow).                    '
      h(19)=' '
      h(20)='Electric heating system - in this system type the         '
      h(21)='modelling of water flows and air flows is unimportant,    '
      h(22)='heat is regarded as a flux delivered to the heated space. '
      h(23)='Flows are therefore NOT modelled (energy-only simulation).'
      h(24)=' '
      h(25)='General HVAC system - this system type is useful for      '
      h(26)='very detailed models of plant systems i.e. where boiler   '
      h(27)='and air conditioning systems need to be modelled in detail'
      h(28)='and where both hot water and air are the working fluids,  '
      h(29)='e.g. an air heating system with the heating coil supplied '
      h(30)='by modular boilers. Energy, air, water vapour and hot     '  
      h(31)='water flows are all simulated in this type of model       ' 
      h(32)='(energy and two phase flow).                              '
      nsimtp=4
      is=1
      CALL EPICKS(IS,IVAL,
     &   ' Select system type:','(see help) ',32,nsimtp,simtyp,
     &   'system type ',IER,32)
      IX=-1
      if(IVAL(1).eq.1) then
        IX=3
      elseif(IVAL(1).eq.2) then
        IX=2
      elseif(IVAL(1).eq.3) then
        IX=1
      elseif(IVAL(1).eq.4) then
        IX=3
      else
        IX=3
      endif
      if(ier.ne.0) goto 70
      return
      end

C ******************** showpc ************************
C showpc lets user select component parameter to change.
C ix is zero if nothing selecte, one if one selected.
C ival is an array of components. ival(1) is updated to be
C      the component selected.
      subroutine showpc(ix,ival)
   
#include "plant.h"
      common/pophelp/h(60)
      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      common/pcsort/icode(maxpc)

C Temporary arrays to hold 'grouped components.
      character*70 wchdesc(maxpc),ahudesc(maxpc),ppdesc(maxpc),
     &othdesc(maxpc),tmpdesc(maxpc)
      character*80 pcdesc
      dimension ival(mpcom),item(7)
      dimension ndbahu(maxpc),ndbwch(maxpc),ndbpp(maxpc),ndboth(maxpc)
      CHARACTER H*72, item*28, head*28

C Fill the array pcdesc with the database component infornation
      call descpc

C Fill the subarrays with relevant component descriptions.
      nahu=0
      nwch=0
      npp=0
      noth=0
      do 10 i=1,NPCDB
        if(icode(i).gt.0.and.icode(i).lt.200) then
           nahu=nahu+1
           ndbahu(nahu)=i
           ahudesc(nahu)=pcdesc(i)(1:70)
        elseif(icode(i).ge.200.and.icode(i).lt.510) then
           nwch=nwch+1
           ndbwch(nwch)=i
           wchdesc(nwch)=pcdesc(i)(1:70) 
        elseif(icode(i).ge.510.and.icode(i).lt.700) then
           npp=npp+1
           ndbpp(npp)=i
           ppdesc(npp)=pcdesc(i)(1:70)
        elseif(icode(i).ge.700) then
           noth=noth+1
           ndboth(noth)=i
           othdesc(noth)=pcdesc(i)(1:70)
        endif
  10  continue

C Top level menu displaying component categories.
  2   h(1)='This menu shows the broad categories of component '
      h(2)='held in the ESP-r plant components database. Click'
      h(3)='on the desired categories and select the components '
      h(4)='required for the plant network. '
      ino=-1
      nitms=7
      head='Component type'
      item(1)='a air conditioning      '
      item(2)='b wet central heating   '
      item(3)='c primitive part        '
      item(4)='d solar and others      '
      item(5)='--------------------    '
      item(6)='? Help                  '
      item(7)='- Exit                  '

      call emenu(head,item,nitms,ino)

      if(ino.eq.6) then
        call phelpd('class help',4,'-',0,0,IER)
        goto 2
      elseif(ino.ge.1.and.ino.le.4) then
        if(ino.eq.1) then
          ncmp=nahu
          do 101 i1=1,nahu 
            tmpdesc(i1)=ahudesc(i1)
  101     continue          
        elseif(ino.eq.2) then
          ncmp=nwch
          do 102 i2=1,nwch 
            tmpdesc(i2)=wchdesc(i2)
  102     continue 
        elseif(ino.eq.3) then
          ncmp=npp
          do 103 i3=1,npp 
            tmpdesc(i3)=ppdesc(i3)
  103     continue 
        elseif(ino.eq.4) then
          ncmp=noth
          do 104 i4=1,noth 
            tmpdesc(i4)=othdesc(i4)
  104     continue
        endif

        IX=1
        h(1)='This list contains names of all components'
        h(2)='available in the plant component data base.'
        h(3)='Select from this list the components required'
        h(4)='for the problem considered.'
        h(5)=' '
        CALL EPICKS(IX,IVAL,
     &    ' Please select a component from the list provided: ',
     &     ' (Exit) terminates selection.',70,ncmp,tmpdesc,
     &     'Components list',IER,5)

        if(ival(1).eq.0)then
          call edisp(iuout,'nothing selected...')
          goto 2
        else
C debug ....
C Translate ival(1) to the correct database value
          if(ino.eq.1) then
            isel=ival(1)
            ival(1)=ndbahu(isel)
          elseif(ino.eq.2) then
            isel=ival(1)
            ival(1)=ndbwch(isel)
          elseif(ino.eq.3) then
            isel=ival(1)
            ival(1)=ndbpp(isel)
          elseif(ino.eq.4) then
            isel=ival(1)
            ival(1)=ndboth(isel)
          endif
        endif

C debug ....
        if(ier.ne.0) return
      elseif(ino.eq.7) then
        ix=0
        return
      endif
      end

C ******************** gettrs ************************
C gettrs Display TRNSYS model types
      subroutine gettrs(ix,ival)

#include "plant.h"
      common/pophelp/h(60)
      common/trnsys/ittype(mpcom), loctrs(mpcom), idbct(mpcom), ntypes

      dimension ival(mpcom)
      CHARACTER H*72, typdsc(mpcom)*20

      do 10 ityp=1, ntypes
         write(typdsc(ityp),'(a,i5)') 'Model type ', ittype(ityp)
   10 continue

C Now display the menu.
   86 IX=1
      h(1)='This list contains TRNSYS model types'
      h(2)='available with the component you have selected.'
      h(3)='Select from this list the model type required'
      h(4)='for the problem considered.'
      h(5)=' '
      CALL EPICKS(IX,IVAL,
     &  ' Please select a component from the list provided: ',
     &   ' (Exit) terminates selection.',20,ntypes,typdsc,
     &   'TRNSYS types',IER,5)
      if(ier.ne.0.or.ix.eq.0) goto 86
      return
      end

C *********** ECMPLST ***********
C ECMPLST presents a list of plant components augmented with electrical 
C If MOD = 'M' then include option to add an item then the appropriate
C interaction is begun.
      SUBROUTINE ECMPLST(head,MOD,IS,IER)

#include "epara.h"
#include "plant.h"

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom) 
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PCNAM/PCNAME(MPCOM)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      DIMENSION VERT(35),IEPMAP(MPCOM)
      CHARACTER PCNAME*15,STR*8
      character*(*) head
      character head2*21
      CHARACTER H*72,VERT*62,KEY*1,MOD*1
      LOGICAL SELECT

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
      ILEN=NPEL
      IPACT=CREATE
      CALL EKPAGE(IPACT)
  92  IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=6
      
C Initial menu entry setup.

      IER=0
      IVERT=-3

C Firstly loop throught the plant components and pick out those currently
C active.
      IMP=0
      DO 5 I=1,NPCOMP
         IF(IPCELF(I).GT.0)THEN
           IMP=IMP+1
           IEPMAP(IMP)=I
         ENDIF
 5    CONTINUE  
 
C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      WRITE(VERT(1),'(A)')
     &'   Component     |p.f.|   Type   |Nom. Power|Nom. Volt.|Phase'
      DO 10 L=1,ILEN
        IM=IEPMAP(L)
        STR=' '
        if(IPFP(IM).eq.0) then 
          str='resist. '
        elseif(IPFP(IM).eq.-1) then
          str='induct. '
        elseif(IPFP(IM).eq.1) then
          str='capacit.'
        else
          str='UNKNOWN '
        endif  
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),
     &'(A1,1X,A15,1X,F3.2,2X,A8,2X,F8.1,3X,F8.1,4X,I2)')KEY,PCNAME(IM),
     &PFP(IM),STR,PWRP(IM),BVOLTP(IM),IPHP(IM) 
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________ '
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' :',MPM 
      ENDIF

C If MOD has been passed as a M then add an item at the end.
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERT(M+2)='+ Add/Delete/Copy   '
        VERT(M+3)=' '
      ELSE
        VERT(M+2)='                   '
        VERT(M+3)='                   '
      ENDIF
      if(MMOD.EQ.8)then
        VERT(M+4)  ='                   '
      else
        VERT(M+4)  ='< Index select     '
      endif
      VERT(M+4)  ='              '
      VERT(M+5)  ='? Help             '
      VERT(M+6)  ='- Exit             '

C Display the menu.
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 92
      ELSEIF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then return with 0.
        IF(.NOT.SELECT)IS=0
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        H(1)='The list shows the list of plant components with'
        H(2)='electrical details in the current plant network.'
        H(3)='You can add or delete electrical data or select'
        H(4)='an entry to edit it.'
        CALL PHELPD('node connection selection',4,'-',0,0,IER)  
        GOTO 92

      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Script directed input if not in graphic mode.
        if(MMOD.EQ.8)goto 3
        IV=1
  96    CALL EASKI(IV,' ',' Index of component ? ',
     &         1,'F',NPEL,'F',1,'script comp',IER,0)
        if(IER.NE.0)goto 96
        IS=IV
        RETURN

      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Allow a component to be added, checked for uniqueness and returned.
C Code to allow deletion commented out until support code in place.
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          CALL EASKABCD('Select option or continue',' ','Add ','Delete',
     &'Copy','Continue',IOPT,0)
          IF(IOPT.EQ.1) THEN
            if(NPEL+1.LE.MPCOM)then

C Display a list of available plant components and allow user
C to specify power consumption details
              head2='Available Components:'
              call askpcmp(head2,'-',IS,IER)
              if(is.eq.0) goto 92
              CALL ELEDIT(is)
              IS=-1
              ILEN=NPEL
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              GOTO 92
            else
              call usrmsg(' ',' Component list full!','W') 
              IS=0
              RETURN
            endif
          ELSEIF(IOPT.EQ.2) THEN
            call usrmsg('Select details to delete ',' ','-')
            CALL EMENU(head,VERT,MVERT,IDVERT) 
            CALL KEYIND(MVERT,IDVERT,IFOC,IO)   
            IPEL=IFOC
            IF(IPEL.GT.0) THEN          
              K=IEPMAP(IPEL)
              IPCELF(K)=0
              PFP(K)=0.0
              IPFP(K)=-2
              PWRP(K)=0.0
              BVOLTP(K)=0.0
              IPHP(K)=1
              NPEL=NPEL-1
            ENDIF
            ILEN=NPEL
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            GOTO 92
          ELSE
            CALL EDISP(IUOUT,'The copy function is not available')
          ENDIF 
          GOTO 92             
        ELSE
          IVERT=-1
          goto 92
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potential long list to the comp no via KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IS=IFOC
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          IC=IEPMAP(IS)
          CALL ELEDIT(IC)
          ILEN=NPEL
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          GOTO 92
        ELSE
          RETURN
        ENDIF
      ELSE
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 92

      END 
 
C *********** ELEDIT ***********
C ELEDIT edits electrical details for a particular plant 
C component. The information to be entered is as follows: 
C  Power factor 0-1 ratio of real to apparent power usage.
C  Power factor flag ipfp - leading 1,lagging -1 or unity 0 
C  Power consumption, the real power consumption of the component.
C  Operational voltage - voltage at which the component operates.
C  Phase - which phase the component is connected to 1-3 or 4 all three.

      subroutine eledit(IPCOMP)

#include "plant.h"

      common/pcnam/pcname(mpcom)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      common/pophelp/h(60)

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      dimension item(14)    
      character pcname*15,H*72, lodstr*16
      character*40 item
      logical close
     
C Display a list of available plant components and allow user
C to specify power consumption details

C Display a menu showing present values of electrical variables.
C If no present values exist then set defaults
      if(ipcelf(ipcomp).eq.0) then 
        PFP(IPCOMP)=0.0
        IPFP(IPCOMP)=-2
        PWRP(IPCOMP)=0.0
        BVOLTP(IPCOMP)=0.0
        IPHP(IPCOMP)=1
      endif

 10   ino=-4

C Set up loadstring.
      if(IPFP(IPCOMP).eq.0) then 
        lodstr='Resistive'
      elseif(IPFP(IPCOMP).eq.-1) then
        lodstr='Leading current'
      elseif(IPFP(IPCOMP).eq.1) then
        lodstr='Lagging current'
      else
        lodstr='UNKNOWN'
      endif  
       
      write(item(1),'(a,i3)')' No. of electrical components: ',npel
      write(item(2),'(a,a)')' Component name : ', pcname(ipcomp)
      if(ipcelf(ipcomp).gt.0)  then 
        write(item(3),'(a,a)')' a Electrical flag >> ON'
      else
        write(item(3),'(a,a)')' a Electrical flag >> OFF'
      endif
      item(4)='________________________________________'
      item(5)=' ? Help '
      item(6)=' - Exit '
      if(ipcelf(ipcomp).eq.0) then
        nitms=6
        call emenu('Electrical details',item,nitms,ino)
      else
        write(item(5),'(a,F7.5)') 'a Power factor  : ',pfp(ipcomp)
        write(item(6),'(a,a16)')  'b Load type     : ',lodstr
        write(item(7),'(a,F10.2)')'c Nominal power : ',pwrp(ipcomp)
        write(item(8),'(a,F10.2)')'d Op. voltage   : ',bvoltp(ipcomp)
        write(item(9),'(a,i3)')'e Phase         : ',iphp(ipcomp)
        item(10)='______________________________________'
        item(11)=' * Couple to electrical model<N/A> '
        item(12)=' '
        item(13)=' ? Help'
        item(14)=' - Exit'
        nitms=14
        call emenu('Electrical details',item,nitms,ino)
      endif
      if(ipcelf(ipcomp).eq.0)then
        if(ino.eq.3)  then
          ipcelf(ipcomp)=1
          npel=npel+1
          goto 10
        elseif(ino.eq.nitms-1)then
          h(1)='This facility allows you to edit the electrical details'
          h(2)='of the component. Items such as voltage, power factor,'
          h(3)='and phase are constant values.  However, the power'
          h(4)='consumption is purely a starting value as this'
          h(5)='variable is over-written with the calculated value '
          h(6)='each time-step as the simulation progresses.'
          h(7)='  '
          h(8)='The component can either be a load (absorbs power) or a'
          h(9)='generator (power source).   '
          call phelpd('Electr help',9,'-',0,0,IER)
          goto 10
        elseif(ino.eq.nitms)then
          return
        else
          goto 10
        endif
      else
        continue
      endif
      if(ino.eq.3)  then

C Reset electrical flag for component to 'off'
        ipcelf(ipcomp)=0
        npel=npel-1
        goto 10
      elseif(ino.eq.5) then
        val=pfp(ipcomp)
        defval=0.8
        h(1)=' The power factor for the component is the ratio of'
        h(2)=' real power consumption (kW) to apparent power     '
        h(3)=' consumption (kVA) and lies between 1.0 &  0.0.    '
        call easkr(val,' ','Enter the power factor',
     &      0.0,'F',1.0,'F',defval,'Power factor',IER,3) 
        pfp(ipcomp)=val

      elseif(ino.eq.6) then
        h(1)=' The impedance for the component describes '
        h(2)=' whether the component takes a leading or lagging    '
        h(3)=' current. Most plant loads take a lagging current '  
        CALL ECLOSE(PFP(ipcomp),1.0,0.0001,CLOSE)
         IF(CLOSE)THEN
           IPFP(ipcomp)=0
         ELSE
           CALL EASKAB('Is the load current leading or lagging ?',
     &'(see help)','Leading','Lagging',IOPT,3)
           IF(IOPT.EQ.1)THEN
             IPFP(ipcomp)=-1
           ELSE
             IPFP(ipcomp)=1
           ENDIF
         ENDIF             
      elseif(ino.eq.7) then
        val=pwrp(ipcomp)
        defval=0.0
        h(1)=' The nominal real power consumption of the component'
        h(2)=' in kW. '
        call easkr(val,' ','Enter the nominal power consumption (kW)',
     &    1.,'-',1.,'-',defval,'Power consump',IER,2) 
        pwrp(ipcomp)=val
      elseif(ino.eq.8) then 
        val=bvoltp(ipcomp)
        defval=0.0
        h(1)='The operational voltage is the voltage  at which'
        h(2)='the component operates at i.e. 220V, 12V etc. '
        h(3)='For N-phase components, this corresponds to the'
        h(4)='line-neutral voltage.'
        call easkr(val,' ','Enter the operational voltage',0.,'W'
     &    ,1.,'-',defval,'Op. voltage',IER,2)
        bvoltp(ipcomp)= val
      elseif(ino.eq.9) then
        iv=iphp(ipcomp)
        idefval=1
        h(1)=' The phase number for the component describes which'
        h(2)=' phases the  component is connected to  '
        h(3)=' power from 1,2,3 or (4 - all three).'
        call easki(iv,' ','Enter the phase no. ',1,'F',4,
     &    'F',idefval,'Phase no.',IER,3)
        iphp(ipcomp)=iv   
      elseif(ino.eq.13) then
        h(1)='This facility allows you to edit the electrical details'
        h(2)='of the component. Items such as voltage, power factor,'
        h(3)='and phase are constant values.  However, the power'
        h(4)='consumption is purely a starting value as this'
        h(5)='variable is over-written with the calculated value '
        h(6)='each time-step as the simulation progresses.'
        h(7)='  '
        h(8)='The component can either be a load (absorbs power) or a'
        h(9)='generator (power source).   '
        call phelpd('Electr help',9,'-',0,0,IER)
      elseif(ino.eq.14) then
        return
      else
        goto 10
      endif
      goto 10

      end

C ****************CONTLYR**************************************************
C CONTLYR allows the user to select a layer in a construction
C of a particular zone surface as a containment for or to embed a plant component.
C The routine presents a list of surface layers.

      subroutine CONTLYR(IZ,IS,IND)
#include "building.h"

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/FILEP/IFIL

      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/CONDB/LFCON,IFCON,LFMUL,IFMUL
      COMMON/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK

      DIMENSION PNAM(ME),item(ME+10)
      LOGICAL        CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      integer header,footer
      character item*32
      CHARACTER DESC*48,NAM*72,PNAM*20,H*72
      CHARACTER TITL*72,SNAME*12,LFCON*72,LFMUL*72
      CHARACTER SOTHER*15,SMLCN*12,SVFC*4,SOTF*4

C Check composite name of surface with composite name in MLC common.
      imatch=0
      do 5 i=1,nmlc
        if(SMLCN(is)(1:12).eq.DESC(i)(1:12)) then
          imatch=i          
        endif
  5   continue
      if(imatch.eq.0) then
         call edisp(iuout,'Error: probably no MLC defined!')
         return
      endif
        
C Write menu displaying a list of layer names
      header=4
      footer=3
      write(item(1),'(a,a)') 'a surface name: ',SNAME(iz,is)
      write(item(2),'(a,i2)')'b number of layers: ',LAYERS(imatch)
      write(item(3),'(a,a)') 'c MLC name: ',DESC(imatch)(1:12)
      item(4)='  ----------------------------'
      do 10 j=1,LAYERS(imatch)

C If an air layer include the R values in the display.
        CALL ERPCDB(IFCON,IPR(imatch,j),ITRU,IER,DBCON,DBDEN,
     &             DBSHT,E,A,DRV,TITL,NAM)
        IF(IER.EQ.1) THEN
           call edisp(iuout,' Found an error reading PCDB!')
        ENDIF
        IF(IPR(imatch,j).EQ.0)THEN
          PNAM(j)='Air gap'
        ELSE
          PNAM(j)=NAM(1:20)
        ENDIF 
        write(item(header+j),'(a,i2,1x,a12)')' Layer: ',j,PNAM(j)
        ndisp=header+j
  10  continue
      item(ndisp+footer-2)='  ----------------------------' 
      item(ndisp+footer-1)='? help  '
      item(ndisp+footer)  ='- exit  '
      ino=-1
      nitms=ndisp+footer
      call edisp(iuout,' Select a layer for the component.')

  7   call emenu(' Layer for component',item,nitms,ino)
      if(ino.le.header.and.ino.gt.ndisp) ino=-1
      if(ino.gt.header.and.ino.le.ndisp) then

C Ask for the nodal location

C Firstly calculate the default
        nodloc=2*(ino-header)
        nodlocd=nodloc
        h(1)='The value given is the node in the centre of the layer.'
        call easki(nodloc,' Position component at which node ?','  ',
     &nodloc-1,'W',nodloc+1,'W',nodlocd,'cnt node',IER,1)
        IND=nodloc  
        RETURN 
      elseif(ino.eq.ndisp+footer-1) then
        h(1)='Choose a layer at which to situate the component'
        call phelpd('containment',1,'-',0,0,IER)
        ino=-1
        goto 7
      elseif (ino.eq.ndisp+footer) then
        return
      elseif(ino.lt.0) then
        goto 7
      else
        goto 7
      endif

C Return the node location
      return 
      END

C ******************** SCANBPLINK ***************************
C SCANBPLINK scans a control file for building plant linkages
C and stores the linkage data in the BPLINKG common

C Variables:
C NBPLINK   - the number of building plant links
C IBPLINKID  - the control function containing the link
C IPCLINK1  - the linked plant component (emitter)
C IPCLINK2  - secondary linked plant component (extract), if any
C IPCNLINK1 - the linked plant component node no
C IPCNLINK2 - the linked plant component node no
C IPCLINKT  - the linkage type
      SUBROUTINE SCANBPLINK

#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN  
      COMMON/FILEP/IFIL

C Common for the building plant lin
      COMMON/BPLINKG/NBPLINK,IBPLINKID(MCF),IPCLINK1(MCF),
     &IPCLINK2(MCF),IPCNLINK1(MCF),IPCNLINK2(MCF),
     &IPCLINKT(MCF),IPCLINKZ(MCF)

C Commons for control variables
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)

      LOGICAL GOTLINK

C Firstly read the control file
      ICTLF=IFIL+1 
      CALL EZCTLR(ICTLF,0,IUOUT,IER)
      IF(IER.EQ.0) THEN
        CLOSE(ICTLF)
      ELSE
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,'SCANBPLINK ERROR: cannot open control file.')
        RETURN
      ENDIF

C Scan through each of the control loops and get the number of existing building
C plant linkages.

      NBPLINK=0
      IF(ncf.EQ.0) RETURN
   
      DO 10 ICF=1,NCF
        GOTLINK=.FALSE.
        IF(nbcdt(ICF).EQ.0) THEN
          NDT=3
        ELSE
          NDT=nbcdt(ICF)
        ENDIF
        DO 20 IDTYP=1,NDT
          
          DO 30 IPER=1,nbcdp(ICF,IDTYP)
            IF(ibclaw(ICF,IDTYP,IPER).EQ.6) THEN
              IF(.NOT.GOTLINK)THEN
                NBPLINK=NBPLINK+1

C Fill in the linkage details, currently only ONE linkage per control law can be handled. 
                IBPLINKID(NBPLINK)=ICF
                IPCLINK1(NBPLINK)=NINT(BMISCD(ICF,IDTYP,IPER,2))
                IPCNLINK1(NBPLINK)=NINT(BMISCD(ICF,IDTYP,IPER,3))
                IPCLINK2(NBPLINK)=NINT(BMISCD(ICF,IDTYP,IPER,7))
                IPCNLINK2(NBPLINK)=NINT(BMISCD(ICF,IDTYP,IPER,8))
                IPCLINKT(NBPLINK)=NINT(BMISCD(ICF,IDTYP,IPER,4))
                IPCLINKZ(NBPLINK)=iban(ICF,1)
              ENDIF
              GOTLINK=.TRUE.               
            ENDIF
 30       CONTINUE 
 20     CONTINUE
 10   CONTINUE

      RETURN
      END

C ********************** ASKBPLINK *********************************
C ASKBPLINK presents a list of plant component linkages returning the index IS.
C If MOD = 'M' then include option to add an item then the appropriate
C interaction is begun.
      SUBROUTINE ASKBPLINK(head,MOD,IS,IER)
#include "epara.h"
#include "building.h"
#include "plant.h"

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

C Common for the building plant link
      COMMON/BPLINKG/NBPLINK,IBPLINKID(MCF),IPCLINK1(MCF),
     &IPCLINK2(MCF),IPCNLINK1(MCF),IPCNLINK2(MCF),
     &IPCLINKT(MCF),IPCLINKZ(MCF)


      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PCNAM/PCNAME(MPCOM)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      common/pcsort/icode(maxpc)
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      DIMENSION VERT(35), CTYPE(4)
      CHARACTER PCNAME*15

      character*(*) head
      CHARACTER H*72,VERT*64,KEY*1,MOD*1,ctype*12,desc*12,tstr*15,
     &zname*12,zdesc*64,tstr1*15
      LOGICAL SELECT

C Currently three types of linkages are supported.
      ctype(1)= 'convective'
      ctype(2)= 'conv/rad'
      ctype(3)= 'embedded'

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
C Initial menu entry setup.
      ILEN=NBPLINK
      IPACT=CREATE
      CALL EKPAGE(IPACT)

   3  IER=0
      SELECT=.FALSE.
      MHEAD=2
      MCTL=6
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      WRITE(VERT(1),'(A)')
     &'   connected  |  connection |  connected    |  connected     |'
      WRITE(VERT(2),'(A)')
     &'     zone     |     type    |emmiter/supply |   extract      |'
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          IF(IPCLINKT(L).EQ.1)THEN
            WRITE(desc,'(A12)')ctype(1)
          ELSEIF(IPCLINKT(L).EQ.2)THEN
            WRITE(desc,'(A12)')ctype(2)
          ELSE
            WRITE(desc,'(A12)')ctype(3)
          ENDIF
 
C Create reporting strings, avoiding zero pointers.
          IF(IPCLINK1(L).EQ.0) THEN
            WRITE(tstr1,'(A15)')'not yet defined'
          ELSE
            WRITE(tstr1,'(A15)')PCNAME(IPCLINK1(L))
          ENDIF
          IF(IPCLINK2(L).EQ.0) THEN
            WRITE(tstr,'(A15)') 'not yet defined'
          ELSE
            WRITE(tstr,'(A15)')PCNAME(IPCLINK2(L))
          ENDIF
          WRITE(VERT(M),'(A1,1X,A12,1X,A12,2X,A15,1X,A15)')
     &      KEY,zname(IPCLINKZ(L)),DESC,tstr1,tstr
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________ '
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' :',MPM 
      ENDIF

C If MOD has been passed as a M then add an item at the end.
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERT(M+2)='+ Add/Delete/Copy '
        VERT(M+3)=' '
      ELSE
        VERT(M+2)='                   '
        VERT(M+3)='                   '
      ENDIF
      if(MMOD.EQ.8)then
        VERT(M+4)  ='                   '
      else
        VERT(M+4)  ='< Index select     '
      endif
      VERT(M+4)  ='              '
      VERT(M+5)  ='? Help             '
      VERT(M+6)  ='- Exit             '

C Display the menu.
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then return with 0.
        IF(.NOT.SELECT)IS=0
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        H(1)='The list shows the (currently) available links between'
        H(2)='components and zone controls. '
        H(3)='Component linkages can be added or deleted if in editing'
        H(4)='mode.'
        H(5)=' '
        H(6)='It is assumed that you have already setup initial zone'
        H(7)='controls for thermal zones which will be linked to a '
        H(8)='network plant component. '
        CALL PHELPD('node connection selection',8,'-',0,0,IER)
        GOTO 3
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Script directed input if not in graphic mode.
        if(MMOD.EQ.8)goto 3
        IV=1
  96    CALL EASKI(IV,' ',' Index of component ? ',
     &         1,'F',NPCOMP,'F',1,'script comp',IER,0)
        if(IER.NE.0)goto 96
        IS=IV
        RETURN

      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Allow a component to be added, checked for uniqueness and returned.
C Code to allow deletion commented out until support code in place.
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
            IOPT=-1
            CALL EASKABCD('Choose an option or continue ',' ','Add',
     &'Delete','Copy','Continue',IOPT,0)
            IF(IOPT.EQ.1) THEN
              if(NBPLINK+1.LE.MCF)then
                 CALL EDBPLINK('A',99)
                 ILEN=NBPLINK
                 IPACT=CREATE
                 CALL EKPAGE(IPACT)     
                 GOTO 3
              else
                call usrmsg(' ',' Connection list full!','W') 
                IS=0
                GOTO 3
              endif
            ELSEIF(IOPT.EQ.2) THEN
               call usrmsg('Delete which component ? ',' ','-') 
               CALL EMENU(head,VERT,MVERT,IDVERT)
               CALL KEYIND(MVERT,IDVERT,IFOC,IO)
               call usrmsg(' ',' ','-') 
               ID=IFOC
               IF(ID.GT.0.AND.ID.LE.NBPLINK) CALL EDBPLINK('D',ID)
               ILEN=NBPLINK
               IPACT=CREATE
               CALL EKPAGE(IPACT)               
            ELSE
               CALL EDISP(IUOUT,
     &'Sorry, copying components is not available.')
            ENDIF
            GOTO 3
        ELSE
          IVERT=-1
          goto 3
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potential long list to the comp no via KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IE=IFOC
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          CALL EDBPLINK('E',IE)
          ILEN=NBPLINK
          IPACT=CREATE
          CALL EKPAGE(IPACT)   
          GOTO 3
        ELSE
          RETURN
        ENDIF
      ELSE
        IVERT=-1
        goto 3
      ENDIF
      IVERT=-2
      goto 3

      END

C ************************** EDBPLINK *******************************8
C EDBPLINK allows building plant link  to be added, deleted or edited.
C The function is determined by the mode:
C E - edit
C A - add
C D - delete

      SUBROUTINE EDBPLINK(MODE,IS)

#include "building.h"
#include "plant.h"

C Commons for control variables
      COMMON/BCTL/NCF,IBSN(MCF,4),IBAN(MCF,3),NBCDT(MCF),
     &            IBCDV(MCF,MBCDT,2),NBCDP(MCF,MBCDT),
     &            TBCPS(MCF,MBCDT,MBCDP),IBCTYP(MCF,MBCDT,MBCDP),
     &            IBCLAW(MCF,MBCDT,MBCDP),BMISCD(MCF,MBCDT,MBCDP,MISC)

      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      common/cctl/icascf(mcom)
C Common for the building plant lin
      COMMON/BPLINKG/NBPLINK,IBPLINKID(MCF),IPCLINK1(MCF),
     &IPCLINK2(MCF),IPCNLINK1(MCF),IPCNLINK2(MCF),
     &IPCLINKT(MCF),IPCLINKZ(MCF)

      COMMON/POPHELP/H(60)

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/FILEP/IFIL
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      COMMON/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      CHARACTER MODE*1,H*72
      CHARACTER*124 OUTS,ASKSTR
      LOGICAL OK,dok,CLOSE

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL

      LOGICAL CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      LOGICAL MODIFY,MODLEN,MODBND

      IF(MODE.EQ.'A'.OR.MODE.EQ.'E') THEN

C Add a building plant link and update the control data. 

C Link which zone to the plant?
        CALL USRMSG('Which zone is linked to the plant?',' ','-')
        WRITE(OUTS,'(A)') 
     &'Select the zone which couples to the plant network. '
        IF(IS.NE.99) THEN
          IZ=IPCLINKZ(IS)
        ELSE
          IZ=1
        ENDIF
        CALL ASKZONE(IZ,0,OUTS,'Select zone','-','plnt conn',IER)
        CALL USRMSG(' ',' ','-')

C Loop through the existing control loops and ensure that this zone
C has no existing control function.
        IF(MODE.EQ.'A') THEN
          DO 10 ICF=1,NCF
            IF(IBAN(ICF,1).EQ.IZ) THEN
              dok=.true.
              h(1)='This zone has an exisiting control function which'
              h(2)='needs to be deleted before the link to plant can '
              h(3)='be made. '
              CALL ASKOK('Delete existing zone control function?',
     &          '(see help)',OK,dok,3)
              IF(OK) THEN
                DO 692 IDV=ICF,ncf-1
                 ibsn(IDV,1)=ibsn(IDV+1,1)
                 ibsn(IDV,2)=ibsn(IDV+1,2)
                 ibsn(IDV,3)=ibsn(IDV+1,3)
                 iban(IDV,1)=iban(IDV+1,1)
                 iban(IDV,2)=iban(IDV+1,2)
                 iban(IDV,3)=iban(IDV+1,3)
                 nbcdt(IDV)=nbcdt(IDV+1)
                 do 693 IDT=1,nbcdt(IDV)
                   ibcdv(IDV,IDT,1)=ibcdv(IDV+1,IDT,1)
                   ibcdv(IDV,IDT,2)=ibcdv(IDV+1,IDT,2)
                   nbcdp(IDV,IDT)=nbcdp(IDV+1,IDT)
                   do 694 IDP=1,nbcdp(IDV,IDT)
                     tbcps(IDV,IDT,IDP)=tbcps(IDV+1,IDT,IDP)
                     ibctyp(IDV,IDT,IDP)=ibctyp(IDV+1,IDT,IDP)
                     ibclaw(IDV,IDT,IDP)=ibclaw(IDV+1,IDT,IDP)
                     bmiscd(IDV,IDT,IDP,1)=bmiscd(IDV+1,IDT,IDP,1)
                     imis=INT(bmiscd(IDV,IDT,IDP,1))
                     do 695 IPM=2,imis+1
                       bmiscd(IDV,IDT,IDP,IPM)=bmiscd(IDV+1,IDT,IDP,IPM)
  695                continue
  694              continue
  693            continue
  692           CONTINUE

                ncf=ncf-1
C Update all the control function linkages.
                DO 25 IBPL=1,NBPLINK
                  IF(OK.AND.IBPLINKID(IBPL).GT.ICF) 
     &IBPLINKID(IBPL)=IBPLINKID(IBPL)-1
 25             CONTINUE

C Update all the zone->control linkages.
                DO 27 IZN=1,MCOM
                  IF(icascf(IZN).EQ.ICF) icascf(IZN)=0
 27             CONTINUE
              ELSE
                RETURN
              ENDIF
            ENDIF      
 10       CONTINUE
        ENDIF

C Increment the control and B/P linkages.
        IF(MODE.EQ.'A')THEN
          NCF=NCF+1
          ICF=NCF
          NBPLINK=NBPLINK+1
          IBPLINK=NBPLINK
          IBPLINKID(IBPLINK)=ICF
          IPCLINKZ(IBPLINK)=IZ
        ELSE
          ICF=IBPLINKID(IS)
          IPCLINKZ(IBPLINK)=IZ
          IBPLINK=IS
        ENDIF

C Update the control/building linkage.
        ICASCF(IZ)=ICF

C Set day type and period - assume 1 and 1
        NBCDT(ICF)=1

C Set the number of periods to 1
        NBCDP(ICF,1)=1

C Set validity - assume whole year
        IBCDV(ICF,1,1)=1
        IBCDV(ICF,1,2)=365

C Set the control law.
        IBCLAW(ICF,1,1)=6

C Set the controller type
        IBCTYP(ICF,1,1)=0

C Set start - assume.
        TBCPS(ICF,1,1)=0.0

C Set the linked zone, sensor and actuator data.
        IBSN(ICF,1)=IZ
        IBSN(ICF,2)=0
        IBSN(ICF,3)=0
        IBSN(ICF,4)=0

        IBAN(ICF,1)=IZ
        IBAN(ICF,2)=0
        IBAN(ICF,3)=0

C Set the number of misca data items.
        BMISCD(ICF,1,1,1)=7.0

C Determine the linkage type.
        h(1)='There are several type of building plant link:'
        h(2)=' '
        h(3)='Convective - the heat input from the plant is'
        h(4)='delivered to the air pint of the zone, use with'
        h(5)='ventilation plant.'
        h(6)=' '
        h(7)='Mixed - the heat input is delivered to the as a mixture '
        h(8)='of radiant and convective, input. This method can be used'
        h(9)='to (approximately model radiators, chilles ceilings,'
        h(10)=' '
        h(11)='Embedded - the plant heat input is delivered into a  '
        h(12)='building construction; this option can be used to'
        h(13)='explicitly model underfloor heating, chilled ceilings'
        h(14)='radiators, etc.'
        CALL EASKABCD
     &('What is the linkage between the zone and the plant?',' ',
     &'Convective','Mixed','Embedded','Cancel',IOPT,14)
        IF(IOPT.EQ.1) THEN
          BMISCD(ICF,1,1,4)=IOPT
          IPCLINKT(IBPLINK)=IOPT
          ASKSTR='What is the component that links to the zone?'
        ELSEIF(IOPT.EQ.2) THEN
          BMISCD(ICF,1,1,4)=IOPT
          IPCLINKT(IBPLINK)=IOPT
          ASKSTR='What is the component that links to the zone?'
          idvalue=50
          ival=50
          CALL EASKI(ival,' ',
     &'What is the convective fraction (0-100) of heat input :',
     &0,'-',0,'-',idvalue,' cval',IER,0)
        IBAN(ICF,1)=-2
        IBAN(ICF,2)=IZ
        IBAN(ICF,3)=IVAL        
        ELSEIF(IOPT.EQ.3) THEN
          BMISCD(ICF,1,1,4)=FLOAT(IOPT)
          IPCLINKT(IBPLINK)=IOPT
          ASKSTR='What is the embedded component?'

          CALL EGOMIN(IFIL+1,LGEOM(IZ),IZ,1,0,iuout,IER)

C General image option flags.
           ITDSP=1
           ITBND=1
           ITEPT=0
           ITZNM=0
           ITSNM=0
           ITVNO=1
           ITORG=1
           ITSNR=1
           ITGRD=1
           GRDIS=0.0

           MODIFY=.TRUE.
           MODBND=.TRUE.
           CALL INLNST(1)
           ITVNO=0
           nzg=1
           nznog(1)=IZ
           izgfoc=IZ
           CALL ADJVIEW(ITRC,IER)
           MODIFY=.TRUE.
           MODBND=.TRUE.

           ISO=1
           CALL EPMENSV
           CALL EASKSUR(IZ,ISO,'-',
     &     'Select the surface in which the component is embedded.',
     &' ',IER)
            CALL EPMENRC
            IF(ISO.GT.0)THEN
              IBAN(ICF,2)=ISO                
            ELSE
              RETURN
            ENDIF
              CALL CONTLYR(IZ,ISO,IND)
              IF(IND.GT.0)THEN
                IBAN(ICF,3)=IND                     
              ELSE
                RETURN
              ENDIF
        ELSE
          RETURN
        ENDIF

C Select the coupled emitter/extract component(s), depending on linkage 
C type. 
        CALL USRMSG(ASKSTR,' ','-')
        CALL ASKPCMP(' Component','-',ICP,IER)
        IF(ICP.EQ.0) RETURN
        BMISCD(ICF,1,1,2)=ICP
        IPCLINK1(IBPLINK)=ICP
        KNODE=1
        IF(NNODES(IS).GT.1) THEN
          CALL GETNOD(ICP,KNODE)       
          BMISCD(ICF,1,1,3)=FLOAT(KNODE)
          IPCNLINK1(IBPLINK)=KNODE
        ELSE
          BMISCD(ICF,1,1,3)=1.0
          IPCNLINK1(IBPLINK)=1
        ENDIF
        CALL ECLOSE(BMISCD(ICF,1,1,4),1.0,0.01,CLOSE)
        IF(CLOSE) 
     &CALL EASKAB('Is there a coupled extract component?',' ','Yes',
     &'No',IOPT,0)
        IF(IOPT.EQ.1) THEN
          CALL USRMSG('What is the coupled extract component?',' ','-')
          CALL ASKPCMP(' Component','-',ICP,IER)
          IF(ICP.EQ.0) RETURN
          IPCLINK2(IBPLINK)=ICP
          BMISCD(ICF,1,1,7)=ICP
          KNODE=1
          IF(NNODES(IS).GT.1) THEN
            CALL GETNOD(ICP,KNODE)       
            BMISCD(ICF,1,1,8)=FLOAT(KNODE)
            IPCNLINK2(IBPLINK)=KNODE
          ELSE
            BMISCD(ICF,1,1,8)=1.0
            IPCNLINK2(IBPLINK)=1
          ENDIF
        ELSE
          BMISCD(ICF,1,1,7)=0.0
          BMISCD(ICF,1,1,8)=0.0
          IPCLINK2(IBPLINK)=0
          IPCNLINK2(IBPLINK)=0
        ENDIF

C Set the max/min flux linkage between zone and plant to a high value. 
        BMISCD(ICF,1,1,5)=9.9E+04

C MB The minimum value is negated inside BCL06
        BMISCD(ICF,1,1,6)=9.9E+04
      ELSEIF(MODE.EQ.'D') THEN

C Delete the appropriate control routine and menu entry. 
         ICTL=IBPLINKID(IS)
         IF(IS.LT.NBPLINK)THEN
           DO 50 IBP=IS,NBPLINK-1
             IBPLINKID(IS)=IBPLINKID(IS+1)
             IPCLINK1(IS)=IPCLINK1(IS+1)
             IPCLINK2(IS)=IPCLINK2(IS+1)
             IPCNLINK1(IS)=IPCNLINK1(IS+1)
             IPCNLINK2(IS)=IPCNLINK2(IS+1)
             IPCLINKT(IS)=IPCLINKT(IS+1)
             IPCLINKZ(IS)=IPCLINKZ(IS+1)
  50       CONTINUE

         ENDIF
         NBPLINK=NBPLINK-1

C Delete the associated building control function
         CALL EDISP(IUOUT,' ')
         CALL EDISP(IUOUT,'Deleting associated control function ...')
         if(ncf.eq.1)then
           ncf=ncf-1
         elseif(ncf.ge.2)then           
           DO 792 IDV=ICTL,ncf-1
             ibsn(IDV,1)=ibsn(IDV+1,1)
             ibsn(IDV,2)=ibsn(IDV+1,2)
             ibsn(IDV,3)=ibsn(IDV+1,3)
             iban(IDV,1)=iban(IDV+1,1)
             iban(IDV,2)=iban(IDV+1,2)
             iban(IDV,3)=iban(IDV+1,3)
             nbcdt(IDV)=nbcdt(IDV+1)
             do 793 IDT=1,nbcdt(IDV)
               ibcdv(IDV,IDT,1)=ibcdv(IDV+1,IDT,1)
               ibcdv(IDV,IDT,2)=ibcdv(IDV+1,IDT,2)
               nbcdp(IDV,IDT)=nbcdp(IDV+1,IDT)
               do 794 IDP=1,nbcdp(IDV,IDT)
                 tbcps(IDV,IDT,IDP)=tbcps(IDV+1,IDT,IDP)
                 ibctyp(IDV,IDT,IDP)=ibctyp(IDV+1,IDT,IDP)
                 ibclaw(IDV,IDT,IDP)=ibclaw(IDV+1,IDT,IDP)
                 bmiscd(IDV,IDT,IDP,1)=bmiscd(IDV+1,IDT,IDP,1)
                 imis=INT(bmiscd(IDV,IDT,IDP,1))
                 do 795 IPM=2,imis+1
                   bmiscd(IDV,IDT,IDP,IPM)=bmiscd(IDV+1,IDT,IDP,IPM)
  795            continue
  794          continue
  793        continue
  792      CONTINUE
           ncf=ncf-1
         endif

C Update all the control function linkages
        DO 55 IBPL=1,NBPLINK
          IF(IBPLINKID(IBPL).GT.ICTL) 
     &IBPLINKID(IBPL)=IBPLINKID(IBPL)-1
 55     CONTINUE

C Update the zone -> control linkages
        DO 57 IZN=1,MCOM
          IF(icascf(IZN).EQ.ICTL) icascf(IZN)=0
 57     CONTINUE

      ENDIF

      RETURN      
      END

