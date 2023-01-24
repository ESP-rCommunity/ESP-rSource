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


C This file contains the following subroutines:
C    newnet: menu for  plant network definition.
C    pcdefn: edits component control parameters.
C    condef: define inter-component connection.
C    cntdef: define component environmental containment.
C    getnod: displays component nodes for connection.
C    initiv: initialises all variables. 
C    askpar: selects component parameter to edit.
C    askmfpar: displays/edits a list of associated mass flow parameters.
C    stype: displays general system types for user selection.
C    showpc: user selection of component parameter to change.
C    gettrs: display of TRNSYS model types.
C    ECMPLST: present list of plant components with electrical data.
C    eledit: edits electrical parameters for a particular component.
C    CONTLYR: allows the user to select a layer within a construction.
C    SCANBPLINK: scans a control file for building plant linkages.
C    ASKBPLINK: presents a list of plant components.
C    EDBPLINK: allows creation/editing of building/plant links.

C ********** newnet **********
C Displays menu for definition of plant components,
C connections and containment temperatures.

      subroutine newnet(iedit,mode)

#include "plant.h"
#include "building.h"
#include "control.h"      
#include "help.h"

      integer lnblnk  ! function definition
 
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
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Plant network.
      common/C23/IFPNF,LPNF

C Electrical details for plant components.
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &             BVOLTP(mpcom),IPHP(mpcom)

      common/pcnam/pcname(mpcom)


      COMMON/PFLNK/IPFLNK     ! Flag for plant/fluid link
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/funit/iucfg, iupdb
      common/mfnchl/mfunit
      common/hfpar/hfpdsc(mpcom,madata)
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)

C Building/plant link.
      COMMON/BPLINKG/NBPLINK,IBPLINKID(MCF),IPCLINK1(MCF),
     &IPCLINK2(MCF),IPCNLINK1(MCF),IPCNLINK2(MCF),
     &IPCLINKT(MCF),IPCLINKZ(MCF)

      common/cctlnm/ctldoc,lctlf

C Format of plant network file.    
      common/PLNFMT/bPLN_format_long
      logical bPLN_format_long
      
      character*20 simtyp(3)
      character lpnam*72,laprob*72,LPNF*72

      dimension ival(mpcom)
      CHARACTER*44 ITEM(28)
      CHARACTER outs*124,ltmp*72,pcname*15, lctlf*72,ctldoc*248
      CHARACTER ICDIR*72, DOIT*248,mode*1,pcdesc*80
      character*68 mscdsc, cvrdsc,hfpdsc
      LOGICAL OK,XST
      integer NITMS,INO       ! maximum items and current menu item

      logical bInitialized
      data bInitialized / .false. /
      save bInitialized
  
      helpinsub='newnet'      ! set for subroutine

C Initialise bPLN_format_long variabl. bPLN_format_long controls the format
C of the plant network file - defaulted to 'long'. May be changed to 'short'
C and this setting will persist for the current prj session.
      if ( .not. bInitialized ) then
        bPLN_format_long = .true.
        bInitialized = .true.
      endif 

      CALL EPAGE

C If file exists then enter edit mode.  
      if(MODE.eq.'G'.and.iedit.eq.1) then 
        helptopic='plant_graphic_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkok('Editing disabled in graphical mode!',
     &    'Switch to menu mode?',OK,nbhelp)
        if(.not.ok) return                
        MODE='-'
      endif   
            
      IF(iedit.eq.1) THEN
        IOP=1
      ELSE
        IOP=4
      ENDIF

      lpnam =' '   

C Read the system configuration file.   
 210  CALL EFOPSEQ(IFPNF,LPNF,IOP,IER)
      if(IER.GT.0) then
        call edisp(iuout,'Error opening system configuration file.')
        return
      endif

      IF(IUPDB.EQ.0) IUPDB=IFPNF+1
      if(iedit.eq.1) then
        helptopic='plant_config_syn'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Plant network summary?',OK,nbhelp)
        IF(OK)THEN
          call pltcfg(IFPNF,iupdb,iuout,1) 
        ELSE
          call pltcfg(IFPNF,iupdb,iuout,0) 
        ENDIF
        close(IFPNF) 
      else

C If this is a new file then open plant component data base.
        call opnpdb(1)
        CALL USRMSG(' ',' ','-')
      endif

      if(lctlf(1:3).NE.'UNK'.AND.lctlf(1:2).NE.'  ') CALL SCANBPLINK

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
      simtyp(1)='energy only'
      simtyp(2)='energy + one phase'
      simtyp(3)='energy + two phase'
      

C Initialise the plant fluid linkage (initially none)
      ipflnk=0

C Let the user pick a menu item.
   10 ino=-5

      write(item(1),'(a,a)')'a plant network file: ',LPNF(1:20) 
      write(item(2),'(a,a)')'b simulation type: ',
     &       simtyp(npmtyp)(1:20)  
      write(item(3),'(a,a)')'c network title: ',lpnam(1:20) 
      item(4)=' ' 
      write(item(5),'(a,i3,a)')'d components (', npcomp,')'  
      write(item(6),'(a,i3,a)')'e connections (', npcon,')'
      write(item(7),'(a,i3,a)')'f containments (', ncont,')'
      write(item(8),'(a,i3,a)')'g components with electrical data (',
     &                         npel,')' 
      item(9)=' '   
      item(10)='h plant/ fluid network link'
      write(item(11),'(a,i3,a)')'i plant/ building link (',nbplink,')'
      item(11)=' '
      if ( bPLN_format_long ) then
        item(12)='j plant network file format > detailed'
      else
        item(12)='j plant network file format > short'
      endif
      item(13)=' '
      item(14)='! update plant network file'
      item(15)='? help'
      item(16)='- exit menu'
      nitms=16

C If newnet is called with mode 'G' skip menu.
      if(mode.eq.'G') then
        ino=nitms
        goto 13
      endif

   12 CALL EMENU('Network definition: edit',ITEM,NITMS,INO)
   13 continue

C Change plant network file.
      if (ino.eq.1) then 
        ltmp=LPNF
 301    CALL EASKS(ltmp,'Plant network definition file?',
     &    ' ',72,DPNF,'plant network file',IER,4)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LPNF=ltmp
        else
          call usrmsg('Re-enter file name.',' ','W')
          goto 301
        endif

        inquire(file=LPNF,exist=xst)
        if(.not.xst) then
          CALL USRMSG('Cannot find file!',' ','W')
          GOTO 10
        else
          iedit=1
          GOTO 210            
        endif

C Change simulation type.
      elseif(ino.eq.2) then
        call stype(ix,ival)
        if(ix.eq.0) goto 12
        npmtyp=ix
        CALL USRMSG(' ',' ','-')
        goto 12

C Change network title.
      elseif(ino.eq.3) then
        helptopic='plant_title'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ltmp=lpnam
        CALL EASKS(ltmp,' ','Network title?',72,'no title given',
     &    'lpnam',IER,nbhelp)
         if(ltmp(1:2).ne.' '.and.ltmp(1:4).ne.'UNKN')lpnam=ltmp
        goto 12

C Define components.
      elseif(ino.eq.5) then
        call ASKPCMP('Components','M',IS,IER)

C Define connections.
      elseif(ino.eq.6) then
        if(npcomp.lt.2) then
          CALL USRMSG(' ',
     &     'First define two or more components.','-')
            goto 12
        else
          call ASKPCON('Connections','M',IS,IER)
        endif

C Define containments.
      elseif(ino.eq.7) then
        if(npcomp.lt.1) then
          CALL USRMSG(' ','First select a component.','-')
          goto 12
        else
          CALL ASKCONT('Containments','M',IS,IER)
        endif

C Define electrical details.
      elseif(ino.eq.8) then
        call ECMPLST('Electrical','M',IS,IER)

C Link plant and flow networks.
      elseif(ino.eq.10) then
        helptopic='plant_net_to_floe'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkabc(' ','Link plant/ fluid networks via:',
     &       'manual','automatic','cancel',ipflnk,nbhelp)
        if(ipflnk.eq.3) then 
          ipflnk=0
          iflwn=0
          goto 10

C Manual linkage: read the flow network.
        elseif(ipflnk.eq.1) then
          IUF=IFIL+1
          CALL EFOPSEQ(IUF,LAPROB,1,IER)

C Flow network file is text-based.
          if(iairn.eq.1)then
            CALL EMFREAD(IUF,IER)

C Flow network file is graphical.
          elseif(iairn.eq.2)then
            CALL NETREAD(IUF,'S',IER)
            CALL NETTOFLW(ier)

C Flow network file is 3D.
          elseif(iairn.eq.3)then
            CALL MFCDAT
            CALL EMF3DREAD(IUF,'R',IER)
            CALL ERPFREE(IUF,ISTAT)
          endif

          IF(IER.NE.0)THEN
            CALL EDISP(IUOUT,' ')
            CALL EDISP(IUOUT,'Problem reading fluid flow file!')
            CALL EDISP(IUOUT,'Check that one is defined')
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

C List the fluid flow connections.
  206       CALL USRMSG(
     &       'Plant connection links to which fluid flow connection?',
     &       outs(1:lnblnk(outs)),'-')
            call ASKCON('Flow connections','-',INS,INE,IC,IER)            
            IF(IC.GT.0) THEN
              icffs(ilink)=IC
            ELSE
              helptopic='plant_net_no_con'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL EASKOK('No connection selected!',
     &                    'Retry?',OK,nbhelp)
              IF(OK) THEN
                GOTO 206
              ELSE
                GOTO 10
              ENDIF
            ENDIF
 207      continue
          iflwn=1

C Automatic linkage.
        elseif(ipflnk.eq.2) then
         if(npcon.gt.0) then
           helptopic='plant_auto_linkage'
           call gethelptext(helpinsub,helptopic,nbhelp)
           CALL EASKOK(' ','Auto generate flow network?',OK,nbhelp)
           IF(.not.ok) goto 215
           call easki(ians,' ','Password?',0,'-',0,'-',0,
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

C Link plant to building.
      elseif(ino.eq.11) then
        CALL ASKBPLINK('Linkages','M',ILNK,IER)
        helptopic='plant_update_reminder'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Update control file to reflect changes?',
     &    OK,nbhelp)
        ICTLF=IFIL+1
        IF(OK) THEN 
          CALL CTLWRT(ICTLF,IER)
          IF(IER.EQ.0) THEN
            CLOSE(ICTLF)
          ELSE
            CALL EDISP(IUOUT,' ')
            CALL EDISP(IUOUT,'Control file not updated! Check that one') 
            CALL EDISP(IUOUT,'is referenced in the configuration file.') 
          ENDIF
        ENDIF

C Set .pln file format.
      elseif(ino.eq.12) then
        if ( bPLN_format_long ) then
          bPLN_format_long = .false.
        else
          bPLN_format_long = .true.
        endif  
        
C Update the existing plant configuration file.
      elseif(ino.eq.14) then
        iexit=0
        goto 57

C Output menu help.
      elseif(ino.eq.nitms-1) then
        helptopic='plant_config_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call phelpd('pdf network def',nbhelp,'-',0,0,ier)  

C Exit.
      elseif(ino.eq.nitms) then
        IF(MODE.EQ.'G') THEN
          helptopic='plant_icon_folder'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKS(icdir,' ','Icons folder?',72,
     &      '/opt/esp-r/icons/plant','icon folder',IER,nbhelp)
          write(doit,'(a)') 'net'
          call runit(doit,'graph')
          call net2pnf
          MODE='-'
          goto 10
        ENDIF
        helptopic='plant_config_save'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Save changes?',OK,nbhelp)
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

C Update plant network file.
 57   continue
      if(npcomp.gt.0) then
        helptopic='plant_update_save'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ltmp=LPNF
        CALL EASKS(ltmp,' ','Plant network file name?', 
     &      72,' ',' file name',IER,nbhelp)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LPNF=ltmp
        else
          goto 57
        endif
      endif

  215 call update
      call descpc
      call pcwrt
      if(IEXIT.eq.1)then
        return
      elseif(IEXIT.eq.0)then
        goto 10
      endif

      end

C *********************** pcdefn ************************
C Edits control variables of available components.
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
#include "gencompDB.h"
#include "gencompDB_common.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C9plus/UCdbNam(MPCOM),CdbCat(MPCOM),CDBDesc(MPCOM)

      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      common/pcnam/pcname(mpcom)

C Common for new format component database
      COMMON/NCDBFORM/ICDBF

      DIMENSION  ival(mpcom), iloc(3)
      LOGICAL OK,dok
      character*68 mscdsc, cvrdsc
      character pcname*15,txt*68,pcdesc*80,str*15,outs*124,
     &UCDBNam*16, CdbCat*32, CDBDesc*72
  
      helpinsub='pcdefn'      ! set for subroutine

C Now display the menu.
      str=' '

C *** PDB affected
      call showpc(ix,ival,iloc)
      if (ix.eq.0) return
      if(ix.gt.0) then   
        npcomp=npcomp+1

C Initialise component entry in dbase.
        IF(ICDBF.EQ.1)THEN
          UCDBNam(npcomp)=itemtag(ILOC(1),ILOC(2),ILOC(3))
        ELSE
          npref(npcomp)=ival(1) 
        ENDIF
C Record component entry in new format database. 
     
C Read component name for current problem.
        helptopic='plant_comp_nme'
        call gethelptext(helpinsub,helptopic,nbhelp)    
        CALL EASKS(str,'Component name?',
     &  '(15 characters max, no spaces)',15,' ',' component name',
     &  IER,nbhelp)
        pcname(npcomp)=str(1:15)

C Call subroutine to read data associated with selected
C component from plant data base.
        call rcdata(npcomp,1)
        helptopic='plant_ctl_var'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(nci(npcomp).gt.0) then

C If component has any control variable(s),
C prompt user to enter its value.
          do 101 jj=1, nci(npcomp)
            txt=cvrdsc(jj)
            value=0.0
            CALL EASKR(value,'Component controlled variable',txt,0.,
     &        '-',0.,'-',0.,' item value?',IER,nbhelp)
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
          helptopic='plant_add_par'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ','Change data?',OK,nbhelp)
          if(.not.OK) return

C Display miscellaneous data items and let user pick one.
  105     call askpar(itm,ival,npcomp)
          if(itm.ne.0.and.ival(1).ne.0) then

C Read new value for selected item(s) then redisplay.
             helptopic='plant_entry'
             call gethelptext(helpinsub,helptopic,nbhelp)
             do 106 iij = 1,itm
               txt=mscdsc(ival(iij))
               dvalue=tadata(npcomp,ival(iij))
               value=dvalue
               call easkr(value,' ',txt,0.,'-',0.,'-',dvalue,
     &           ' item value?',IER,nbhelp)
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

C *********************** condef ************************
C Defines/edits a connection (ipcon) between current receiving
C component and sending component. A connection type must be
C specified for each connection.

C   contyp           - description of each connection type
C   npcon            - number of connections currently defined
C   ipc1(?)          - receiving component number for connection ?.
C   ipn1(?)          - recieving node number for connection ?.
C   ipct(?)          - connection type for connection ?.
C   ipc2(?)          - sending component number for connection ?.
C   ipn2(?)          - sending node number for connection ?.
C   pcondr(?)        - mass diversion ratio for connection ?.
C   pconsd(?,1,2,..) - supplementary data for connection ?.
C   nnodes(?)        - number of nodes in component ?.
C   idcon(?)         - connection id number
C   all arrays defined in 'plantdf.h'

      subroutine condef(emod,ipcon)

#include "plant.h"
#include "help.h"

      common/pcnam/pcname(mpcom)
      common/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      common/CONET/IPCOMPA,IPCOMPB
      common/idcn/idcon(mpcon)   
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)

      dimension ival(mpcom), contyp(5)
      character contyp*45,head*19,emod*1,outs*124,str*72,pcname*15
      integer num
  
      helpinsub='condef'      ! set for subroutine
  
      num=0
      str=' '

C User to define receiving component and node. EMOD = G is request to
C get receiving component and node.
      IF(EMOD.NE.'G') THEN
        head='Receiving component'
        IF(IPC1(IPCON).NE.0)THEN
          write(str,'(3A)') '(Currently ',PCNAME(IPC1(IPCON)),')'
        ELSE
          write(str,'(A)') '(Currently UNDEFINED)'  
        ENDIF 
        call usrmsg('Receiving component:',str,'-')
        CALL ASKPCMP(head,'C',IPCOMP,IER)
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

C Fill array with connection types.
      isvt=isv(ipc1t,ipn1t)
      contyp(1)=  'to self                               '
      if(isvt.eq.1.or.isvt.eq.11.or.isvt.eq.21)then
        contyp(2)='specified temperature & humidity ratio'
      else
        contyp(2)='specified temperature                 '
      endif
      contyp(3)=  'another plant component               '
      contyp(4)=  'building zone air temperature         '
      contyp(5)=  'ambient air temperature               '

C User selected connection type.
  107 itm=1
      nctype=5
      helptopic='plant_conn_type'
      call gethelptext(helpinsub,helptopic,nbhelp)                                      '
      IF(IPCT(IPCON).NE.0)THEN
        write(str,'(3A)') '(currently ',CONTYP(IPCT(IPCON)),')'
      ELSE
        write(str,'(A)') '(currently UNDEFINED)'  
      endif                                       
      call usrmsg('Select connection type.',str,'-')
      call epicks(itm,ival,' ',
     &    'Component connection type?',38,nctype,contyp,
     &    'Connection type',ier,nbhelp)
      if(ier.ne.0) goto 107
      if(itm.eq.0) return
      ipctt=ival(1)

C Type 1: link to self.
      if(ipctt.eq.1) then
        call edisp(iuout,' ')
        call edisp(iuout,'Option not available. Please reselect.')
        goto 107

C Type 2: specified temperature (and perhaps humidity ratio).
      elseif(ipctt.eq.2) then
        IF(EMOD.NE.'G') THEN 
          value=20.0   ! default
          dvalue=value
          helptopic='data_entry'
          call gethelptext(helpinsub,helptopic,nbhelp)  
          CALL EASKR(value,' ',
     &     'Temperature (C)',0.,'-',0.,'-',dvalue,
     &     'temperature',IER,nbhelp)
          supdt1=value
          value=0.001
          dvalue=value
          if(isvt.eq.1.or.isvt.eq.11.or.isvt.eq.21)then
            CALL EASKR(value,' ',
     &       'Humidity ratio (kg v/kg a)',0.,'-',dvalue,
     &       '-',dvalue,'humidity ratio',IER,nbhelp)
           supdt2=value
         else
           supdt2=0.000
         endif
         helptopic='plant_known_link'
         call gethelptext(helpinsub,helptopic,nbhelp) 
         CALL PHELPD('supply component',nbhelp,'-',0,0,IER)
         IF(IPC2(IPCON).NE.0)THEN
           write(str,'(3A)') '(currently ',PCNAME(IPC2(IPCON)),')'
         ELSE
           write(str,'(A)') '(currently UNDEFINED)'  
         endif 
         CALL USRMSG('Select a component:',str,'-')
         head='DRIVING compt:'
         CALL ASKPCMP(head,'K',IPCOMP,IER)
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

C Type 3: connection to another component. 
      elseif(ipctt.eq.3)then
        IF(EMOD.NE.'G')THEN
          IF(IPC2(IPCON).NE.0)THEN
            write(str,'(3A)') '(Currently ',PCNAME(IPC2(IPCON)),')'
          ELSE
            write(str,'(A)') '(Currently UNDEFINED)'  
          endif
          call usrmsg(' ',' ','-')
          call usrmsg('Select SENDING component.',str,'-')
          head='SENDING compt:'
          CALL ASKPCMP(head,'C',IPCOMP,IER)
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

C Type 4: connection to a building zone.
      elseif(ipctt.eq.4) then
        IF(EMOD.NE.'G') THEN
          value=1.0
          dvalue=value

C Display available zones.
          IZ=NINT(pconsd(ipcon,1))
          CALL ASKZONE(IZ,0,'Connected zone:','-','plnt conn',34,IER)
          call usrmsg(' ',' ','-')
          supdt1=IZ

          IF(IPC2(IPCON).NE.0)THEN
            write(str,'(3A)') '(Currently ',PCNAME(IPC2(IPCON)),')'
          ELSE
            write(str,'(A)') '(Currently UNDEFINED)'  
          endif
          call usrmsg('SUPPLY component:',str,'-')
          head='SUPPLY compt:'
          CALL ASKPCMP(head,'Z',IPCOMP,IER)
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

C Type 5: connection to ambient air.
      elseif(ipctt.eq.5) then
        IF(EMOD.NE.'G') THEN 
          IF(IPC2(IPCON).NE.0)THEN
            write(str,'(3A)') '(Currently ',PCNAME(IPC2(IPCON)),')'
          ELSE
            write(str,'(A)') '(Currently UNDEFINED)'  
          endif
          call usrmsg(' ',' ','-')
          CALL USRMSG('Select a component.',str,'-')
          head='DRIVING compt:'
          CALL ASKPCMP(head,'A',IPCOMP,IER)
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

C Determine the mass diversion ratio for this connection.
      value=1.0
      dvalue=value
      helptopic='plant_mdr'
      call gethelptext(helpinsub,helptopic,nbhelp)  
      CALL EASKR(value,' ','Connection mass diversion ratio',0.,'-',
     &    0.,'-',dvalue,'mass diversion ratio',IER,nbhelp)
      pcondrt=value

C Assign connection data to appropriate variables and increment connection counter.
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

C ********** cntdef **********
C Displays selected components for assigning containment type.

C   cnttyp         - holds description of each containment type.
C   ncont          - number of specified containments.
C   ipcc(?)        - component number for containment ?.
C   indcp(?)       - type of containment ?.
C   cntdat(?,1..3) - supplementary data for containment ?.

      subroutine cntdef(emod,icont)

#include "plant.h"
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "help.h"

      common/outin/iuout,iuin,ieout
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)    
      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/FILEP/IFIL

      dimension ival(mpcom), cnttyp(5)
      character cnttyp*45,  head*19, emod*1, outs*124
  
      helpinsub='cntdef'      ! set for subroutine

C Fill array with existing containment types.
      cnttyp(1)='ambient air temperature'
      cnttyp(2)='plant component temperature'
      cnttyp(3)='specified temperature'
      cnttyp(4)='zone temperature'
      cnttyp(5)='ground temperature'

 9    if(emod.eq.'E') then
        ipcomp=icont
      else

C Let user select a component.
        call usrmsg(
     &   ' ','Select component:','-')
        head='Component?'
        CALL ASKPCMP(head,'C',IPCOMP,IER)
        call usrmsg(' ',' ','-')
      endif

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
                call edisp(iuout,'Containment already defined!')
                call usrmsg(' ','Select component:','-')
                head='Component?'
                CALL ASKPCMP(head,'-',IPCOMP,IER)
                call usrmsg(' ',' ','-')
                goto 9
              endif
            endif
 11      continue

C User selected containment type.
  107    itm=1
         ncontyp=5
         helptopic='plant_cont_type'
         call gethelptext(helpinsub,helptopic,nbhelp)  
         call epicks(itm,ival,' ',
     &    'Containment type:',37,ncontyp,cnttyp,
     &    'Containments type',ier,nbhelp)
         if(ier.ne.0) goto 107
         if(itm.eq.0) return
         indctt=ival(1)-1

C Containment type 0: ambient air temperature + specified (de)increment.
         if(indctt.eq.0) then
           value=0.0
           dvalue=value
           helptopic='plant_cont_off'
           call gethelptext(helpinsub,helptopic,nbhelp) 
           CALL EASKR(value,' ',
     &      'Increment temperature?',0.,'-',
     &      0.,'-',dvalue,'(de)increment?',IER,nbhelp)
           cntdt1=value
           cntdt2=0.0
           cntdt3=0.0

C Containment type 1: specified plant node (includes the
C self containment case if cntdt1=0) + specified (de)increment cntdt3.
         elseif(indctt.eq.1) then
           head='Component?'
           CALL ASKPCMP(head,'C',IPCOMP,IER)
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
           helptopic='plant_cont_off'
           call gethelptext(helpinsub,helptopic,nbhelp) 
           CALL EASKR(value,' ',
     &        'Increment temperature?',0.,'-',
     &        0.,'-',dvalue,'(de)increment',IER,nbhelp)
           cntdt3=value

C Type 2 containment: specified temperature cntdt1.
         elseif(indctt.eq.2) then
           value=20.0
           dvalue=value
           helptopic='plant_cont_fix'
           call gethelptext(helpinsub,helptopic,nbhelp) 
           CALL EASKR(value,' ','Temperature?',
     &        0.,'-',0.,'-',dvalue,'temperature',IER,nbhelp)
           cntdt1=value
           cntdt2=0.0
           cntdt3=0.0

C Type 3 containment: zone cntdt1, surface cntdt2
C and construction node cntdt3 counted from outside.
         elseif(indctt.eq.3) then
           IZ=0
           CALL ASKZONE(IZ,0,'Containment zone:','-','plnt conn',34,IER)
           if(IZ.eq.0) RETURN
           cntdt1=float(IZ)
           helptopic='plant_cont_loc'
           call gethelptext(helpinsub,helptopic,nbhelp) 
           CALL EASKABCD(' ','Location in zone:',
     &       'air','surface','construction','cancel',ICLOC,nbhelp)
            IF(ICLOC.EQ.1)THEN
              cntdt2=0.0
              cntdt3=0.0
            ELSEIF(ICLOC.EQ.4) THEN
              RETURN
            ELSEIF(ICLOC.EQ.2.OR.ICLOC.EQ.3)THEN
              call georead(IFIL+1,LGEOM(IZ),IZ,1,iuout,IER)

C General image option flags.
              ITDSP=1; ITBND=1; ITEPT=0; ITZNM=0; ITSNM=0
              ITVNO=1; ITORG=1; ITSNR=1
              ITGRD=1; GRDIS=0.0; ITPPSW=0

              MODIFYVIEW=.TRUE.
              MODBND=.TRUE.
              CALL INLNST(1)
              ITVNO=0
              nzg=1
              nznog(1)=IZ
              izgfoc=IZ
              CALL ADJVIEW(IER)
              MODIFYVIEW=.TRUE.
              MODBND=.TRUE.

              ISO=1
              CALL EPMENSV
              CALL EASKSUR(IZ,ISO,'-','Containment surface',' ',IER)
              CALL EPMENRC 
              IF(ISO.EQ.0)RETURN
              cntdt2=ISO
              cntdt3=0.0
              IF(ICLOC.EQ.3)THEN
                CALL CONTLYR(IZ,ISO,IND)
                cntdt3=float(IND)
              ENDIF
            ENDIF
         elseif(indctt.eq.5) then
           cntdt1=1.0
           cntdt2=0.0
           cntdt3=0.0
         endif

C Assign containment data and increment number of containments.
        if (emod.eq.'-') then
          ncont=ncont+1
          ipcc(ncont)=ipcct
          indcp(ncont)=indctt
          cntdat(ncont,1)=cntdt1
          cntdat(ncont,2)=cntdt2
          cntdat(ncont,3)=cntdt3
        elseif (emod.eq.'E') then

C For containment editing, do not increment the counter NCONT. 
         ipcc(icont)=ipcct
         indcp(icont)=indctt
         cntdat(icont,1)=cntdt1
         cntdat(icont,2)=cntdt2
         cntdat(icont,3)=cntdt3
        endif         
      endif
      return
      end

C ********** getnod **********
C Displays a menu of nodes for a component (ipcomp) and returns
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
      call epicks(itm,ival,' ','Node?',15,nnodes(ipcomp),nodesc,
     &   'Component nodes',ier,0)
      if(ier.ne.0) goto 55
      knode=ival(1)
      return
      end

C ********** initiv **********
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

C Initialise components.
      npcomp=0

C Initialise connections.
      npcon=0

C Initialise containments.
      ncont=0

C Initialise electrical components
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

C Fill array 'pcdesc' with component descriptions.
      call descpc
      return
      end

C ********** askpar **********
C Selects parameters of component (ipc) to edit.

      subroutine askpar(nitm,ival,ipc)

#include "plant.h" 
#include "help.h"

      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA) 
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      common/pcnam/pcname(mpcom)

      character mscdsc*68, cvrdsc*68,pcname*15,txt*31
      CHARACTER parlst(madata)*70
      dimension ival(mpcom)
  
      helpinsub='askpar'      ! set for subroutine

C Display miscellaneous data items.
  105 nitm=npi(ipc)
      helptopic='plant_edit_par'
      call gethelptext(helpinsub,helptopic,nbhelp)
      do 909 jj=1,npi(ipc)
         write(parlst(jj),'(a50,a3,g12.5)')
     &          mscdsc(jj),' : ',tadata(ipc,jj)
  909 continue
      write(txt,'(2a)') 'Plant component ',pcname(ipc)
      call epicks(nitm,ival,' ',
     &  'Change:',70,npi(ipc),parlst,txt,ier,nbhelp)
      if(ier.ne.0) goto 105
      return
      end

C ********** askmfpar **********
C Displays/edits a list of associated mass flow data
C for component (ipc).

      subroutine askmfpar(ipc)

#include "plant.h"
#include "help.h"

      common/outin/iuout,iuin,ieout
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      common/mfcpar/itypmf(mpcon), isdcmf(mpcon), icnnmf(mpcon),
     &              supcmf(mpcon,17), ltpcmf(mpcon), isdimf(mpcon)
      common/mfddsc/mfdatstr(mpcon,17)
      common/tmpmfdt/tmpmfdat(mpcon,17),imfed(mpcon,17)
      common/pcnam/pcname(mpcom)
      dimension list(21)
      CHARACTER ltpcmf*60, mfdatstr*40,pcname*15
      character txt*27,list*60, head*44
      integer nlist,ino ! max items and current menu item
  
      helpinsub='askmfpar'      ! set for subroutine
      
      nconns=0
      do 110 inod=1, nnodes(ipc)
        if(ndcon(ipc,inod).gt.0) nconns=nconns+ndcon(ipc,inod)
  110 continue
      if(nconns.gt.0) then
        call getmfc(ipc,nconns)
      endif

C Build menu.
      icon=1
      ino=-2
115   k=0
      if(isdcmf(icon).eq.0) then
        k=1
        list(k)='No data for node.'
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
      list(k+2)='+ next connection                 '
      list(k+3)='? help                            '
      list(k+4)='- exit menu                       '
      nlist=k+4
      write(txt,'(a,a)')pcname(ipc),' connect: '
      write(head,'(a,i2)') txt, icon
 130  call emenu(head,list,nlist,ino)
      if(ino.ge.1.and.ino.le.k) then 
        helptopic='flow_entry'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(ino.eq.1.and.isdcmf(icon).eq.0) goto 130
        deflt=supcmf(icon,ino)
        if(imfed(icon,ino).eq.1) deflt=tmpmfdat(icon,ino)
        write(txt,'(a)')mfdatstr(icon,ino)(1:27)
        call easkr(value,' ',txt,0.,'-',0.,'-',deflt,' item value ?',
     &    IER,nbhelp)
       tmpmfdat(icon,ino)=value
       imfed(icon,ino)=1
       goto 115
      elseif(ino.eq.k+2) then
        if(icon+1.le.nconns) then
          icon=icon+1 
        else
          call edisp(iuout,'Last connection!')
        endif
        goto 115
      elseif(ino.eq.k+3) then
        helptopic='plant_edit_mfpar'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call phelpd('pdf network flow node list',nbhelp,'-',0,0,IER)
        goto 130
      elseif(ino.eq.k+4) then 
        return
      else
        goto 130
      endif
      end
 

C ********** stype **********
C Display general system types for user selection.

      subroutine stype(ix,ival)

#include "plant.h"
#include "help.h"

      dimension ival(mpcom)
      character*32 simtyp(4)
  
      helpinsub='stype'      ! set for subroutine

C Fill array for simulation type.
      simtyp(1)=' mechanical ventilation'
      simtyp(2)=' hydronic heating      '
      simtyp(3)=' electric heating      '
      simtyp(4)=' HVAC                  '

C Let user select the system type required.
   70 IX=1
      helptopic='plant_mod_type'
      call gethelptext(helpinsub,helptopic,nbhelp)
      nsimtp=4
      is=1
      CALL EPICKS(IS,IVAL,' ',' ',32,nsimtp,simtyp,
     &   'System type',IER,nbhelp)
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

C ********** showpc **********
C User selection of component parameter to change. ix is 0 if nothing
C selected, 1 if something selected. ival is an array of components; ival(1)
C is updated to be the component selected.

      subroutine showpc(ix,ival,iloc)
   
#include "plant.h"
#include "help.h"

      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      common/pcsort/icode(maxpc)

C Common for new format component database.
      COMMON/NCDBFORM/ICDBF

C Temporary arrays to hold grouped components.
      character*70 wchdesc(maxpc),ahudesc(maxpc),ppdesc(maxpc),
     &             othdesc(maxpc),tmpdesc(maxpc),hndesc(maxpc)
      character*80 pcdesc
      dimension ival(mpcom),item(8),iloc(3)
      dimension ndbahu(maxpc),ndbwch(maxpc),ndbpp(maxpc),ndboth(maxpc)
      dimension ndbhn(maxpc)
      CHARACTER item*28, head*28
      integer nitms,ino ! max items and current menu item
      Logical editable
  
      helpinsub='showpc'      ! set for subroutine

      if (icdbf.gt.0)then
        IX=0
        editable=.false.
        ID=0
        call CDBDomainExplore(EDITABLE,ID,1,IER)
        ILOC(1)=ID
        IF(ID.EQ.0) return
        IDc=ID
        IC=0
        call CDBCategoryExplore(EDITABLE,IDc,IC,1,IER)
        ILOC(2)=IC
        IF(IC.EQ.0) return
        ICc=IC
        IT=0
        call CDBItemExplore(EDITABLE,IDc,ICc,IT,1,IER)
        ILOC(3)=IT
        IF(IT.NE.0) IX=1
        return
      endif
C Fill the array pcdesc with the database component infornation
      call descpc

C *** PDB affected!
C Fill the subarrays with relevant component descriptions.
      nahu=0
      nwch=0
      npp=0
      noth=0
      nhn=0
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
        elseif(icode(i).ge.700.and.icode(i).lt.1340) then
           noth=noth+1
           ndboth(noth)=i
           othdesc(noth)=pcdesc(i)(1:70)
        elseif(icode(i).ge.1340) then
           nhn=nhn+1
           ndbhn(nhn)=i
           hndesc(nhn)=pcdesc(i)(1:70)
        endif
  10  continue

C Top level menu displaying component categories.
  2   helptopic='plant_comp_cat'
      call gethelptext(helpinsub,helptopic,nbhelp)
      ino=-1
      nitms=8
      head='Component types'
      item(1)='a air conditioning      '
      item(2)='b central heating       '
      item(3)='c primitive parts       '
      item(4)='d miscellaneous         '
      item(5)='e district heating      '
      item(6)='--------------------    '
      item(7)='? help                  '
      item(8)='- exit                  '

      call emenu(head,item,nitms,ino)

      if(ino.eq.7) then
        call phelpd('class help',nbhelp,'-',0,0,IER)
        goto 2
      elseif(ino.ge.1.and.ino.le.5) then
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
        elseif(ino.eq.5) then
          ncmp=nhn
          do 105 i5=1,nhn 
            tmpdesc(i5)=hndesc(i5)
  105     continue
        endif

        IX=1
        helptopic='plant_comp_sel'
        call gethelptext(helpinsub,helptopic,nbhelp)

        CALL EPICKS(IX,IVAL,' ',' ',70,ncmp,tmpdesc,
     &     'Components list',IER,nbhelp)

        if(ival(1).eq.0)then
          call edisp(iuout,'nothing selected...')
          goto 2
        else

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
          elseif(ino.eq.5) then
            isel=ival(1)
            ival(1)=ndbhn(isel)
          endif
        endif

        if(ier.ne.0) return
      elseif(ino.eq.8) then
        ix=0
        return
      endif
      end

C ********** gettrs **********
C Display TRNSYS model types.

      subroutine gettrs(ix,ival)

#include "plant.h"
#include "help.h"

      common/trnsys/ittype(mpcom), loctrs(mpcom), idbct(mpcom), ntypes

      dimension ival(mpcom)
      CHARACTER typdsc(mpcom)*20

      helpinsub='newnet'  ! set for subroutine
      helptopic='TRNSYS_type'
      call gethelptext(helpinsub,helptopic,nbhelp)

      do 10 ityp=1, ntypes
         write(typdsc(ityp),'(a,i5)') 'Model type ', ittype(ityp)
   10 continue

C Display menu.
   86 IX=1
      CALL EPICKS(IX,IVAL,' ','Select a component:',20,ntypes,typdsc,
     &   'TRNSYS types',IER,nbhelp)
      if(ier.ne.0.or.ix.eq.0) goto 86
      return
      end

C *********** ECMPLST ***********
C Presents a list of plant components augmented with electrical data.
C If MOD = 'M' then include option to add an item.

      SUBROUTINE ECMPLST(head,MOD,IS,IER)

#include "epara.h"
#include "plant.h"
#include "help.h"

      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom) 
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PCNAM/PCNAME(MPCOM)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      DIMENSION VERT(35),IEPMAP(MPCOM)
      CHARACTER PCNAME*15,STR*8
      character*(*) head
      character head2*21
      CHARACTER VERT*62,KEY*1,MOD*1
      LOGICAL SELECT
      integer MVERT,IVERT,IDVERT ! max items and current menu item
  
      helpinsub='ecmplst'      ! set for subroutine
      helptopic='plant_elec_act'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Initialise node menu variables based on window size. IVERT is the menu
C position, MVERT the current number of menu lines based on selected list.
      ILEN=NPEL
      IPACT=CREATE
      CALL EKPAGE(IPACT)
  92  IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=6
      
C Menu entry setup.
      IER=0
      IVERT=-3

C Loop through plant components and select those currently active.
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

C Number of items displayed.
      MVERT=M+MCTL

C If a long list, include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________ '
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' :',MPM 
      ENDIF

C If MOD has been passed as 'M' then add an item at the end.
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERT(M+2)='+ add/delete/copy'
        VERT(M+3)=' '
      ELSE
        VERT(M+2)=' '
        VERT(M+3)=' '
      ENDIF
      if(MMOD.EQ.8)then
        VERT(M+4)  =' '
      else
        VERT(M+4)  ='< index select'
      endif
      VERT(M+4)  =' '
      VERT(M+5)  ='? help'
      VERT(M+6)  ='- exit menu'

C Display the menu.
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 92

C If no selection been made before exit then return with 0.
      ELSEIF(IVERT.EQ.MVERT)THEN
        IF(.NOT.SELECT)IS=0
        RETURN

C Menu help.
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        helptopic='plant_elec_act'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('node connection selection',nphelp,'-',0,0,IER) 
        GOTO 92

C Script directed input if not in graphic mode.
      ELSEIF(IVERT.EQ.(MVERT-2))THEN
        if(MMOD.EQ.8)goto 3
        IV=1
  96    CALL EASKI(IV,' ',' Index of component ? ',
     &         1,'F',NPEL,'F',1,'script comp',IER,0)
        if(IER.NE.0)goto 96
        IS=IV
        RETURN

C Allow a component to be added, checked for uniqueness and returned.
      ELSEIF(IVERT.EQ.(MVERT-4))THEN
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          CALL EASKABCD(' ','Options:','add ','delete',
     &                  'copy','cancel',IOPT,0)
          IF(IOPT.EQ.1) THEN

C Display a list of plant components and allow user
C to specify power consumption details.
            if(NPEL+1.LE.MPCOM)then
              head2='Available components:'
              call askpcmp(head2,'-',IS,IER)
              if(is.eq.0) goto 92
              CALL ELEDIT(is)
              IS=-1
              ILEN=NPEL
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              GOTO 92
            else
              call usrmsg(' ','Component list full!','W') 
              IS=0
              RETURN
            endif
          ELSEIF(IOPT.EQ.2) THEN
            call usrmsg('Select details to delete.',' ','-')
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
            CALL EDISP(IUOUT,'Copy function not available!')
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
C Edit electrical details for a particular plant component.
C  Power factor - ratio of real to apparent power usage (0-1).
C  Power factor flag (ipfp) - leading 1,lagging -1 or unity 0.
C  Power consumption - real power consumption of the component.
C  Operational voltage - voltage at which the component operates.
C  Phase - connected phase (1-3 or 4 for all three).

      subroutine eledit(IPCOMP)

#include "plant.h"
#include "help.h"

      common/pcnam/pcname(mpcom)

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      dimension item(14)    
      character pcname*15,lodstr*16
      character*40 item
      logical close
      integer nitms,ino ! max items and current menu item
     
C Display a list of available plant components and allow user
C to specify power consumption details.
      helpinsub='newnet'      ! set for subroutine
      helptopic='plant_cmp_elec_det'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display a menu showing present values of electrical variables.
C If no present values exist then set defaults.
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
       
      write(item(1),'(a,i3)')' no. of electrical components: ',npel
      write(item(2),'(a,a)')' component name : ', pcname(ipcomp)
      if(ipcelf(ipcomp).gt.0)  then 
        write(item(3),'(a,a)')' a electrical flag >> ON'
      else
        write(item(3),'(a,a)')' a electrical flag >> OFF'
      endif
      item(4)='________________________________________'
      item(5)=' ? help'
      item(6)=' - exit menu '
      if(ipcelf(ipcomp).eq.0) then
        nitms=6
        call emenu('Electrical details',item,nitms,ino)
      else
        write(item(5),'(a,F7.5)') 'a power factor: ',pfp(ipcomp)
        write(item(6),'(a,a16)')  'b load type: ',lodstr
        write(item(7),'(a,F10.2)')'c nominal power: ',pwrp(ipcomp)
        write(item(8),'(a,F10.2)')'d voltage: ',bvoltp(ipcomp)
        write(item(9),'(a,i3)')'e phase: ',iphp(ipcomp)
        item(10)='______________________________________'
        item(11)=' * couple to electrical model<N/A> '
        item(12)=' '
        item(13)=' ? help'
        item(14)=' - exit menu'
        nitms=14
        call emenu('Electrical details',item,nitms,ino)
      endif
      if(ipcelf(ipcomp).eq.0)then
        if(ino.eq.3)  then
          ipcelf(ipcomp)=1
          npel=npel+1
          goto 10
        elseif(ino.eq.nitms-1)then
          call phelpd('Electr help',nbhelp,'-',0,0,IER)
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

C Reset electrical flag for component to 'off'.
        ipcelf(ipcomp)=0
        npel=npel-1
        goto 10
      elseif(ino.eq.5) then
        val=pfp(ipcomp)
        defval=0.8
        call easkr(val,' ','Power factor?',
     &      0.0,'F',1.0,'F',defval,'Power factor',IER,nbhelp) 
        pfp(ipcomp)=val

      elseif(ino.eq.6) then
        CALL ECLOSE(PFP(ipcomp),1.0,0.0001,CLOSE)
         IF(CLOSE)THEN
           IPFP(ipcomp)=0
         ELSE
           CALL EASKAB(' ','Is the load current leading or lagging?',
     &       'leading','lagging',IOPT,nbhelp)
           IF(IOPT.EQ.1)THEN
             IPFP(ipcomp)=-1
           ELSE
             IPFP(ipcomp)=1
           ENDIF
         ENDIF             
      elseif(ino.eq.7) then
        val=pwrp(ipcomp)
        defval=0.0
        helptopic='plant_cmp_elec_units'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(val,' ','Nominal power consumption?',
     &         1.,'-',1.,'-',defval,'Power consump',IER,nbhelp) 
        pwrp(ipcomp)=val
      elseif(ino.eq.8) then 
        val=bvoltp(ipcomp)
        defval=0.0
        helptopic='plant_cmp_elec_units'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(val,' ','Operational voltage?',0.,'W',
     &        1.,'-',defval,'Op. voltage',IER,nbhelp)
        bvoltp(ipcomp)= val
      elseif(ino.eq.9) then
        iv=iphp(ipcomp)
        idefval=1
        helptopic='plant_cmp_elec_units'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easki(iv,' ','Phase number?',1,'F',4,
     &    'F',idefval,'Phase no.',IER,3)
        iphp(ipcomp)=iv   
      elseif(ino.eq.13) then
          helptopic='plant_cmp_elec_det'
          call gethelptext(helpinsub,helptopic,nbhelp)
        call phelpd('Electr help',nbhelp,'-',0,0,IER)
      elseif(ino.eq.14) then
        return
      else
        goto 10
      endif
      goto 10

      end

C ********** CONTLYR **********
C Allows the user to select a layer in a construction as a containment
C for a plant component.

      subroutine CONTLYR(IZ,IS,IND)

#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "help.h"

      common/OUTIN/IUOUT,IUIN,IEOUT

      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

      DIMENSION PNAM(ME),item(ME+10)
      integer header,footer
      character item*32
      CHARACTER NAM*72,PNAM*20
      integer nitms,ino ! max items and current menu item
  
      helpinsub='contlyr'     ! set for subroutine

C Establish if material data arrays have been filled. If not return
C with ier=1.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('The materials arrays are incomplete so',
     &    'layer selection is not possible.','W')
        return
      endif

C Check composite name of surface with composite name in MLC common.
      imatch=0
      call matchmlcdesc(SMLCN(IZ,is),imatch)
      if(imatch.eq.0) then
         call edisp(iuout,'Error: probably no MLC defined!')
         return
      endif
        
C Write menu displaying a list of layer names
      header=4
      footer=3
      write(item(1),'(a,a)') 'a surface name: ',SNAME(iz,is)
      write(item(2),'(a,i2)')'b number of layers: ',LAYERS(imatch)
      write(item(3),'(a,a)') 'c construction name: ',
     &                       mlcname(imatch)(1:16)
      item(4)='  ----------------------------'
      do 10 j=1,LAYERS(imatch)

C If an air layer include the R values in the display.
        matarrayindex=IPRMAT(imatch,j)   ! which array index
        if(matarrayindex.ge.0)then
 
C And if matarrayindex is zero then reestablish NAM.
          if(matarrayindex.eq.0)then
            NAM='AIR'
            PNAM(j)='Air gap'
          else
            write(NAM,'(a)') matname(matarrayindex)(1:32)
            PNAM(j)=NAM(1:20)
          endif
        endif
        write(item(header+j),'(a,i2,1x,a12)')' Layer: ',j,PNAM(j)
        ndisp=header+j
  10  continue
      item(ndisp+footer-2)='  ----------------------------' 
      item(ndisp+footer-1)='? help'
      item(ndisp+footer)  ='- exit menu'
      ino=-1
      nitms=ndisp+footer
      call edisp(iuout,'Select a layer for the component.')

  7   call emenu('Component layer',item,nitms,ino)
      if(ino.le.header.and.ino.gt.ndisp) ino=-1
      if(ino.gt.header.and.ino.le.ndisp) then

C Ask for the nodal location. Firstly calculate the default.
        nodloc=2*(ino-header)
        nodlocd=nodloc
        helptopic='cont_layer_node'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easki(nodloc,'Position component at which node?','  ',
     &     nodloc-1,'W',nodloc+1,'W',nodlocd,'cnt node',IER,nbhelp) 
        IND=nodloc  
        RETURN 
      elseif(ino.eq.ndisp+footer-1) then
        helptopic='cont_layer_loc'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call phelpd('containment',nbhelp,'-',0,0,IER)
        ino=-1
        goto 7
      elseif (ino.eq.ndisp+footer) then
        return
      elseif(ino.lt.0) then
        goto 7
      else
        goto 7
      endif

C Return the node location.
      return 
      END

C ********** SCANBPLINK **********
C Scan a control file for building plant linkages
C and store the linkage data in the BPLINKG common.

C Variables:
C   NBPLINK   - number of building plant links
C   IBPLINKID - control function containing the link
C   IPCLINK1  - linked plant component (emitter)
C   IPCLINK2  - secondary linked plant component (extract)
C   IPCNLINK1 - linked plant component node number
C   IPCNLINK2 - linked plant component node number
C   IPCLINKT  - linkage type

      SUBROUTINE SCANBPLINK

#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT  
      COMMON/FILEP/IFIL

C Common for the building plant link.
      COMMON/BPLINKG/NBPLINK,IBPLINKID(MCF),IPCLINK1(MCF),
     &IPCLINK2(MCF),IPCNLINK1(MCF),IPCNLINK2(MCF),
     &IPCLINKT(MCF),IPCLINKZ(MCF)

      LOGICAL GOTLINK

C Read the control file.
      ICTLF=IFIL+1 
      CALL EZCTLR(ICTLF,0,IUOUT,IER)
      IF(IER.EQ.0) THEN
        CLOSE(ICTLF)
      ELSE
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,'ERROR: cannot open control file.')
        RETURN
      ENDIF

C Scan through each of the control loops and get the number of existing
C building plant linkages.

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

C Fill in the linkage details, currently only ONE linkage per control law
C can be handled. 
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

C ********** ASKBPLINK **********
C Present a list of plant component linkages, returning the index IS.
C If MOD = 'M' include option to add an item.

      SUBROUTINE ASKBPLINK(head,MOD,IS,IER)

#include "epara.h"
#include "building.h"
#include "geometry.h"
#include "plant.h"
#include "control.h"      
#include "help.h"

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

C Common for the building plant link.
      COMMON/BPLINKG/NBPLINK,IBPLINKID(MCF),IPCLINK1(MCF),
     &IPCLINK2(MCF),IPCNLINK1(MCF),IPCNLINK2(MCF),
     &IPCLINKT(MCF),IPCLINKZ(MCF)


      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PCNAM/PCNAME(MPCOM)

      DIMENSION VERT(35), PCTYPE(4)
      CHARACTER PCNAME*15

      character*(*) head
      CHARACTER VERT*64,KEY*1,MOD*1,pctype*12,desc*12,tstr*15,tstr1*15
      LOGICAL SELECT
      integer MVERT,IVERT,IDVERT ! max items and current menu item

C Currently three types of linkages are supported.
      pctype(1)= 'convective'
      pctype(2)= 'conv/rad'
      pctype(3)= 'embedded'

      helpinsub='newnet'  ! set for subroutine
      helptopic='list_plant_linkages'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
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
            WRITE(desc,'(A12)')pctype(1)
          ELSEIF(IPCLINKT(L).EQ.2)THEN
            WRITE(desc,'(A12)')pctype(2)
          ELSE
            WRITE(desc,'(A12)')pctype(3)
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
        VERT(M+2)='+ add/delete/copy'
        VERT(M+3)=' '
      ELSE
        VERT(M+2)=' '
        VERT(M+3)=' '
      ENDIF
      if(MMOD.EQ.8)then
        VERT(M+4)  =' '
      else
        VERT(M+4)  ='< index select'
      endif
      VERT(M+4)  =' '
      VERT(M+5)  ='? help'
      VERT(M+6)  ='- exit menu'

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
        CALL PHELPD('node connection selection',nbhelp,'-',0,0,IER)
        GOTO 3
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Script directed input if not in graphic mode.
        if(MMOD.EQ.8)goto 3
        IV=1
  96    CALL EASKI(IV,' ','Component?',
     &         1,'F',NPCOMP,'F',1,'script comp',IER,nhelp)
        if(IER.NE.0)goto 96
        IS=IV
        RETURN

      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Allow a component to be added, checked for uniqueness and returned.
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
            IOPT=-1
            CALL EASKABCD(' ','Options:','add','delete',
     &                'copy','cancel',IOPT,nbhelp)
            IF(IOPT.EQ.1) THEN
              if(NBPLINK+1.LE.MCF)then
                 CALL EDBPLINK('A',99)
                 ILEN=NBPLINK
                 IPACT=CREATE
                 CALL EKPAGE(IPACT)     
                 GOTO 3
              else
                call usrmsg(' ','Connection list full!','W') 
                IS=0
                GOTO 3
              endif
            ELSEIF(IOPT.EQ.2) THEN
               call usrmsg(' ','Delete which component?','-') 
               CALL EMENU(head,VERT,MVERT,IDVERT)
               CALL KEYIND(MVERT,IDVERT,IFOC,IO)
               call usrmsg(' ',' ','-') 
               ID=IFOC
               IF(ID.GT.0.AND.ID.LE.NBPLINK) CALL EDBPLINK('D',ID)
               ILEN=NBPLINK
               IPACT=CREATE
               CALL EKPAGE(IPACT)               
            ELSE
               CALL EDISP(IUOUT,'Components copying not available!')
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

C ********** EDBPLINK **********
C Allows building plant links to be added, deleted or edited.
C The function is determined by the mode:
C   E - edit
C   A - add
C   D - delete

      SUBROUTINE EDBPLINK(MODE,IS)

#include "building.h"
#include "model.h"
#include "geometry.h"
#include "plant.h"
#include "control.h"
#include "prj3dv.h"
#include "help.h"

      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      integer icascf
      common/cctl/icascf(mcom)
      COMMON/BPLINKG/NBPLINK,IBPLINKID(MCF),IPCLINK1(MCF),
     &IPCLINK2(MCF),IPCNLINK1(MCF),IPCNLINK2(MCF),
     &IPCLINKT(MCF),IPCLINKZ(MCF)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/FILEP/IFIL

      CHARACTER MODE*1
      CHARACTER*124 OUTS,ASKSTR
      LOGICAL OK,dok,CLOSE

      helpinsub='newnet'  ! set for subroutine
      helptopic='manage_plant_linkages'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IF(MODE.EQ.'A'.OR.MODE.EQ.'E') THEN

C Add a building plant link and update the control data. 
        CALL USRMSG(' ','Which zone is linked to the plant?','-')
        WRITE(OUTS,'(A)')'Zone coupled to the plant network?'
        IF(IS.NE.99) THEN
          IZ=IPCLINKZ(IS)
        ELSE
          IZ=1
        ENDIF
        CALL ASKZONE(IZ,0,'Select zone','-','plnt conn',34,IER)
        CALL USRMSG(' ',' ','-')

C Loop through the existing control loops and ensure that this zone
C has no existing control function.
        IF(MODE.EQ.'A') THEN
          DO 10 ICF=1,NCF
            IF(IBAN(ICF,1).EQ.IZ) THEN
              dok=.true.
              CALL EASKOK(' ','Delete existing zone control function?',
     &          OK,nbhelp)
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
          IBPLINK=IS
          IPCLINKZ(IBPLINK)=IZ
        ENDIF

C Update the control/building linkage.
        ICASCF(IZ)=ICF

C Set day type and period - assume 1 and 1.
        NBCDT(ICF)=1

C Set the number of periods to 1.
        NBCDP(ICF,1)=1

C Set validity - assume whole year.
        IBCDV(ICF,1,1)=1
        IBCDV(ICF,1,2)=365

C Set the control law.
        IBCLAW(ICF,1,1)=6

C Set the controller type.
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
        CALL EASKABCD(' ','Linkage between the zone and the plant?',
     &     'convective','mixed','embedded','cancel',IOPT,nbhelp)
        IF(IOPT.EQ.1) THEN
          BMISCD(ICF,1,1,4)=IOPT
          IPCLINKT(IBPLINK)=IOPT
          ASKSTR='What component links to the zone?'
        ELSEIF(IOPT.EQ.2) THEN
          BMISCD(ICF,1,1,4)=IOPT
          IPCLINKT(IBPLINK)=IOPT
          ASKSTR='What component links to the zone?'
          idvalue=50
          ival=50
          CALL EASKI(ival,' ',
     &      'Convective fraction of heat input:',
     &      0,'-',0,'-',idvalue,' cval',IER,nbhelp)
          IBAN(ICF,1)=-2
          IBAN(ICF,2)=IZ
          IBAN(ICF,3)=IVAL        
        ELSEIF(IOPT.EQ.3) THEN
          BMISCD(ICF,1,1,4)=FLOAT(IOPT)
          IPCLINKT(IBPLINK)=IOPT
          ASKSTR='Embedded component?'

          call georead(IFIL+1,LGEOM(IZ),IZ,1,iuout,IER)

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
          ITPPSW=0

          MODIFYVIEW=.TRUE.
          MODBND=.TRUE.
          CALL INLNST(1)
          ITVNO=0
          nzg=1
          nznog(1)=IZ
          izgfoc=IZ
          CALL ADJVIEW(IER)
          MODIFYVIEW=.TRUE.
          MODBND=.TRUE.

          ISO=1
          CALL EPMENSV
          CALL EASKSUR(IZ,ISO,'-',
     &     'Surface in which the component is embedded.',
     &     ' ',IER)
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

C Select the coupled emitter/extract component(s), depending on
C linkage type. 
        CALL USRMSG(ASKSTR,' ','-')
        CALL ASKPCMP('Component','-',ICP,IER)
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
     &  CALL EASKAB('Is there a coupled extract component?',' ',
     &    'yes','no',IOPT,nbhep)
        IF(IOPT.EQ.1) THEN
          CALL USRMSG('Coupled extract component?',' ','-')
          CALL ASKPCMP('Component','-',ICP,IER)
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

C The minimum value is negated inside BCL06.
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
  50      CONTINUE

         ENDIF
         NBPLINK=NBPLINK-1

C Delete the associated building control function.
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

C Update all control function linkages.
        DO 55 IBPL=1,NBPLINK
          IF(IBPLINKID(IBPL).GT.ICTL) 
     &IBPLINKID(IBPL)=IBPLINKID(IBPL)-1
 55     CONTINUE

C Update the zone -> control linkages.
        DO 57 IZN=1,MCOM
          IF(icascf(IZN).EQ.ICTL) icascf(IZN)=0
 57     CONTINUE

      ENDIF

      RETURN      
      END

