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

C ################# MFPRB1 
C This file contains the following subroutines:
C   MFPROB: Main controller of the mass flow network.
C   MFNDES: Editing facility for nodes
C   MFCDES: Editing facility for components
C   MFLDES: Editing facility for connections
C   MFWRED: Editing facility for flow reduction
C   MFNAUTO: Auto generate elements of the flow network.

C Whenever a new fluid flow component is added, a new MFnnnI
C subroutine should be provided. In addition a new MFnnnC, a
C new entry in MFCDAT and extra lines in MFPROB, MFSOLV, emfnetw
C and (possibly) MFLDES have to be added.

C ********************** MFPROB 
C This is the main controller of the mass flow network model
C description subroutines. The user can create, edit or list a
C fluid flow model description file. 
      SUBROUTINE MFPROB(IER)

#include "net_flow.h"
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

C To signal to MFWRIT that globals are available from graphic network.
C Currently set .FALSE.
      logical haveglobal
      COMMON/MFLOW11/haveglobal
 
C Simulation preferences as per simcon.F in esrubps, some of which filled via
C in the menu below.
C << work in progress >>
C      COMMON/MFSINC/MFBSNC
C      COMMON/MFCALC/IPSMOD,MSLVTP
C      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK
     
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      COMMON/MFLOWIT/fndegc,imix 
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      common/user/browse
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),scfdres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,
     &  selres*72,scfdres*72,sipvres*72

      CHARACTER*33 ITEM1(24)
      CHARACTER LAPROB*72
      CHARACTER LTMP*72,outs*124,outstr*124
      character NDNAM*12,fs*1
      LOGICAL OK,dok,found,XST,clkok,browse,unixok
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      integer NITMS1,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MFPROB'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Temporarily use same file unit as profiles db.
      NDNAM(0)='    '
      fndegc=20.0
      imix=3

      if(IPRODB.eq.IFIL+6)then
        IUM=IPRODB
      else
        IUM=IFIL+6
      endif
      CALL ERPFREE(IUM,ISTAT)
      CALL MFCDAT

C Attempt to read in a flow file. If file has yet to be named then base
C it on cfgroot and place it in the netpth folder (differienciate between
C unix and non-unix machine types).
 179  if(LAPROB(1:2).eq.'  '.or.LAPROB(1:4).eq.'UNKN')then
        if(unixok)then
          if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
            WRITE(LAPROB,'(A,A4)')cfgroot(1:lnblnk(cfgroot)),'.afn'
          else
            WRITE(LAPROB,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.afn'
          endif
        else
          if(netpth(1:2).eq.'  '.or.(ichar(netpth(1:1)).eq.46.and.
     &       ichar(netpth(2:2)).eq.92))then
            WRITE(LAPROB,'(A,A4)')cfgroot(1:lnblnk(cfgroot)),'.afn'
          else
            WRITE(LAPROB,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.afn'
          endif
        endif
      endif
      LTMP=LAPROB
      helptopic='flow_network_file_name'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKSCMD(LTMP,'Flow network file ?',
     &  ' ','dereference',clkok,72,' ','flow network file',IER,nbhelp)
      call usrmsg(' ',' ','- ')

C If user wishes to deselect the current file name to
C blank and update the configuration file.
      if(clkok)then
        if(cfgok.and.(.NOT.browse))then
          LAPROB='  '
          NNOD=0; NCMP=0; NCNN=0; IAIRN=0
          do 88 i=1,ncomp
            ICAAS(i)=0
  88      continue

C If simulation parameters, clear flow results names.
          if(nsset.gt.0)then
            do 89 i=1,MSPS
              sflres(i)='  '
  89        continue
          endif
          CALL EMKCFG('-',IER)
          call usrmsg(' ',' ','- ')
          return
        endif
      endif

      if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN')then
        LAPROB=LTMP
      else
        call usrmsg('Please re-enter the network file name.',' ','W')
        goto 179
      endif

C Read in the specified network file, first check if it is
C an older format file or a graphic network.
      CALL ERPFREE(IUM,ISTAT)
      call FINDFIL(LAPROB,XST)
      IF(XST)THEN
        CALL EFOPSEQ(IUM,LAPROB,1,IER)
        IF(IER.NE.0)goto 179

C Read the file header and check for first-line tag. If 4 items
C then an older file so rewind the file and call emfread.
        CALL STRIPC(IUM,OUTSTR,99,ND,0,'1st line of file',IER)
        IF(IER.NE.0)goto 179
        IF(ND.GE.4)THEN
          REWIND(IUM,ERR=999)
          CALL EMFREAD(IUM,IIER)
          CALL ERPFREE(IUM,ISTAT)
        elseif((ND.eq.1.or.ND.eq.2).and.
     &          OUTSTR(1:18).EQ.'*Graphical_network')then

C Found a graphic network file, advise user and exit.
          call usrmsg(
     &      'This file is a graphic network file rather than a flow',
     &      'network file. Use the `graphic network` option.','W')
          goto 10
        endif

C For now, continue if there seems to be an error, unless file is empty.
        IF((NNOD+NCMP+NCNN).EQ.0) THEN
          CALL EDISP(IUOUT,
     &'Error, fluid flow file has no nodes, connections or components.')
          CALL EDISP(IUOUT,
     &'Possible corrupt fluid flow network description file.')
          GOTO 10
        ENDIF

C Check boundary node supplementary data items.
        INOD=0
        DO 9 INOD=1,NNOD
          if(NDTYP(INOD).eq.3)then
            IF(SUPNOD(INOD,1).LT.1..OR.SUPNOD(INOD,1).GT.NPRE)then
              write(outs,'(3a,i2,a)') 'For node ',NDNAM(INOD),
     &          ' the referenced pressure coef ',SUPNOD(INOD,1),
     &          ' was not found!'
              call edisp(iuout,outs)
            endif
            IF(SUPNOD(INOD,2).LT.0..OR.SUPNOD(INOD,2).GT.360.)then
              write(outs,'(3a,i2,a)') 'For node ',NDNAM(INOD),
     &          ' the orientation ',SUPNOD(INOD,2),
     &          ' is not in the range of 0 to 360!'
              call edisp(iuout,outs)
            endif
          endif
 9      CONTINUE

C List contents of file if required.
        dok=.true.
        helptopic='flow_network_synopsis'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK('Display a synopsis of the flow network?',' ',
     &    OK,dok,nbhelp)
        IF(OK) CALL MFLIST(iuout)
      else
        helptopic='flow_network_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' File not found.','Options:',
     &    'specify another file','make new file',
     &    'make network from zone schedules','cancel',IW,nbhelp)
        if(IW.eq.1)then
          goto 179
        elseif(iw.eq.2)then

C Place in memory the names of the various flow components and reset
C to no flow network.
          CALL MFCDAT
          NNOD=0
          NCMP=0
          NCNN=0
          WRED=1.0
          fndegc=20.0

C Set up a the flow network. 
C Input node data, components data, linkage data, local wind speed 
C reduction factor.
          helptopic='begin_flow_network'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('create net',nbhelp,'-',0,0,IER)

          CALL EASKABC(' Is the new network:',' ',
     &      'all air','all water','mixed',IMIX,nbhelp)
        elseif(iw.eq.3)then

C Place in memory the names of the various flow components and reset
C to no flow network.
          CALL MFCDAT
          NNOD=0
          NCMP=0
          NCNN=0
          WRED=1.0
          fndegc=20.0
          IMIX=1

C Set up a the flow network which minics the state of the zone
C operation files (at least for simple infiltration and ventilation
C settings). 
C Create a node for each zone selected, guess where necessary and
C give warnings to the user.
          helptopic='flow_network_from_opr'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('create oper net',nbhelp,'-',0,0,IER)

C << also should provide user dialog to create flow controls >>
          dok=.true.
          CALL ASKOK('Do you want to auto-generate flow nodes?',
     &      'from zone operation file data (see help)',OK,dok,nbhelp)
          IF(OK)THEN
            CALL MFNAUTO('Z',IER)
          ENDIF
        elseif(iw.eq.4)then
          return
        endif
      endif

C Set up initial menu.
   10 INO=-2

C Display the name of the flow network file. 
      write(ITEM1(1),'(a,a)')'a Network :',LAPROB(1:21)
      if(imix.eq.1)then
        ITEM1(2)  =' air based network           '
      elseif(imix.eq.2)then
        ITEM1(2)  =' water based network         '
      elseif(imix.eq.3)then
        ITEM1(2)  =' air & water network         '
      endif

C Display a description of the network
      write(ITEM1(3),'(a,1x,a)') 'b Network title   :','<N/A>'
      ITEM1(4)    ='  __________________________ '
      ITEM1(5)=' Flow network status...          '
      ITEM1(6)    ='  __________________________ '

C Display model status.
      write(ITEM1(7),
     &'(a,i3,a)')' No. of nodes...       (',nnod,')'
      ITEM1(8)    ='c Nodes                      '
      ITEM1(9)    ='  __________________________ '
      write(ITEM1(10),
     &'(a,i3,a)')' No. of components...  (',ncmp,')'
      ITEM1(11)   ='d Components                 '
      ITEM1(12)   ='  __________________________ '
      write(ITEM1(13),
     &'(a,i3,a)')' No. of connections... (',ncnn,')'
      item1(14)   ='e Connections                '
      ITEM1(15)   ='  __________________________ '

C Other definition functions.
      write(ITEM1(16),
     &'(a,F4.2,a)')' Wind reduction factor (',wred,')'
      ITEM1(17)   ='f Set wind reduction         '

C << place to add in simulation preferences >>
C      if(IPSMOD.eq.1)then
C        ITEMG(9)=   'h Mass flow stack press >> sending node'
C      elseif(IPSMOD.eq.2)then
C        ITEMG(9)=   'h Mass flow stack press >> avg of nodes'
C      elseif(IPSMOD.eq.0)then
C        ITEMG(9)=   'h Mass flow stack press >> NA          '
C      endif
C      if(MSLVTP.eq.1)then
C        ITEMG(10)=  'i Mass flow solution >> Gaussian        '
C      elseif(MSLVTP.eq.2)then
C        ITEMG(10)=  'i Mass flow solution >> LU decomposition'
C      elseif(MSLVTP.eq.0)then
C        ITEMG(10)=  'i Mass flow solution >> NA              '
C      endif
C      if(MFTRAC.eq.-1)then
C        ITEMG(11)=  'j Mass flow trace >> silent             '
C      elseif(MFTRAC.eq.0)then
C        ITEMG(11)=  'j Mass flow trace >> iterations only    '
C      elseif(MFTRAC.eq.1)then
C        ITEMG(11)=  'j Mass flow trace >> data on worst node '
C      elseif(MFTRAC.eq.2)then
C        ITEMG(11)=  'j Mass flow trace >> data on all nodes  '
C      elseif(MFTRAC.eq.3)then
C        ITEMG(11)=  'j Mass flow trace >> all nodes + solver '
C      endif
C      if(MFBSNC.eq.0)then
C        ITEMG(12)=  'k Mass flow timestep >> 1 per hour      '
C      elseif(MFBSNC.eq.1)then
C        ITEMG(12)=  'k Mass flow timestep >> 1 per zone_step '
C      endif

      ITEM1(18)   ='  __________________________ '
      ITEM1(19)   ='g Link nodes and zones       '
      ITEM1(20)   ='  __________________________ '

C File handling functions.
      ITEM1(21)   ='@ Browse network             '
      ITEM1(22)   ='! Save network             '
      ITEM1(23)   ='? help                       '
      ITEM1(24)   ='- Exit                       '
      NITMS1=24

C Help text for this menu.
      helptopic='flow_network_menu_synop'
      call gethelptext(helpinsub,helptopic,nbhelp)

   12 CALL EMENU('Fluid Flow Network',ITEM1,NITMS1,INO)
      IF(INO.EQ.1)THEN

C First check if we're not overwriting existing data
        dok=.false.
        CALL ASKOK(' Reading or creating a new model file will ',
     &    'overwrite any existing data! Is this ok?',OK,dok,nbhelp)
        IF(.NOT.OK)THEN
          GOTO 10
        ELSE
          NNOD=0
          NCMP=0
          NCNN=0
          WRED=1.0
          fndegc=20.0

C Transfer data to fluid flow file and return to main menu.
          helptopic='flow_network_file_name'
          call gethelptext(helpinsub,helptopic,nbhelp)
   78     LTMP=LAPROB
          CALL EASKS(LTMP,' Fluid flow network file ? ',
     &      '(non-grapic version)',72,' ','mass flow file',IER,nbhelp)
          if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN')then
            LAPROB=LTMP
          else
            goto 78
          endif
          CALL EFOPSEQ(IUM,LAPROB,4,IER)
          IF(IER.NE.0)goto 78

          haveglobal=.FALSE.  ! global preferences not yet available
          CALL MFWRIT(IUM)
          CALL ERPFREE(IUM,ISTAT)
        ENDIF
      ELSEIF(INO.EQ.8)THEN

C Display a list of the currently defined nodes. 
C Allow the user to auto generate a network from the existing building
C description. At the moment only nodes are generated.
        IF(NNOD.EQ.0)THEN
          helptopic='flow_auto_generate'
          call gethelptext(helpinsub,helptopic,nbhelp)
          dok=.true.
          CALL ASKOK('Do you want to auto-generate flow nodes?',
     &      '(see help)',OK,dok,nbhelp)
          IF(OK)THEN
            CALL MFNAUTO('N',IER)
          ENDIF
        ENDIF
        CALL ASKNOD(' Nodes','M',IS,IER)
      ELSEIF(INO.EQ.11)THEN

C Display a modifiable list of currently defined connecting components
        CALL ASKCMP(' Components','M',IS,IER)
      ELSEIF(INO.EQ.14)THEN

C Display a list of currently defined connections.
C At the begining detect if there are any conenctions, if not allow the
C user to select "auto-detect" - nifty!
        IF(NCNN.EQ.0)THEN
          dok=.false.
          helptopic='flow_auto_generate'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK('Auto-generate flow connections?','(see help)',
     &      OK,dok,nbhelp)
          IF(OK) THEN
            CALL MFNAUTO('L',IER)
          ENDIF
        ENDIF        
        CALL ASKCON(' Connections','M',INODPS,INODNE,IS,IER)
      ELSEIF(INO.EQ.17)THEN

C Set the wind reduction factor.
        CALL MFWRED
      ELSEIF(INO.EQ.19)THEN

C Link nodes and zones.
        DO 33 IZ=1,NCOMP
          if(ICAAS(IZ).gt.0)then
            write(outs,'(3A)')zname(IZ)(1:lnzname(IZ)),
     &         ' is currently linked to: ',NDNAM(ICAAS(IZ))
            helptopic='link_node_to_zone'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKABC(outs,' ','accept',
     &        'select another','free link',iacc,nbhelp)
            if(iacc.eq.1)then
              goto 33
            elseif(iacc.eq.2)then
              IC=0
              call ASKRNOD(' available nodes','-',IC,IER)
              ICAAS(IZ)=IC
            else
              ICAAS(IZ)=0
            endif
          else
            helptopic='link_node_to_zone'
            call gethelptext(helpinsub,helptopic,nbhelp)
            write(outs,'(A,A)') zname(IZ)(1:lnzname(IZ)),
     &                          ' has no mass flow node.'
            CALL EASKAB(outs,' ','accept','select a node',iacc,nbhelp)
            if(iacc.eq.1)then
              ICAAS(IZ)=0
            elseif(iacc.eq.2)then
              IC=0
              call ASKRNOD(' available nodes','-',IC,IER)
              ICAAS(IZ)=IC
            endif
          endif
  33    CONTINUE

      ELSEIF(INO.EQ.21)THEN
        CALL MFLIST(iuout)
      ELSEIF(INO.EQ.22)THEN

C Transfer data to network flow file and return to main menu.
C Free the file after writing it.
        helptopic='flow_network_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ltmp=LAPROB
   80   CALL EASKS(ltmp,' Fluid flow network file ? ',
     &    ' ',72,' ','mass flow file',IER,nbhelp)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LAPROB=LTMP
        else
          goto 80
        endif
        CALL EFOPSEQ(IUM,LAPROB,4,IER)
        IF(IER.NE.0)goto 80

        haveglobal=.FALSE.  ! global preferences not yet available
        CALL MFWRIT(IUM)
        CALL ERPFREE(IUM,ISTAT)
        OUTS=' '
        WRITE(OUTS,'(A,A)')'Saved fluid flow network: ',
     &LAPROB(1:LNBLNK(LAPROB))
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,' ')
      ELSEIF(INO.EQ.23)THEN
        helptopic='flow_network_menu_synop'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('network data summary',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.24)THEN

C Ask if changes need to be saved
        dok=.true.
        helptopic='flow_network_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK('Do you want to save any changes?',' ',
     &    OK,dok,nbhelp)
        IF(OK)THEN
          ltmp=LAPROB
   81     CALL EASKS(ltmp,' Fluid flow network file ? ',
     &      ' ',72,' ','mass flow file',IER,nbhelp)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            LAPROB=LTMP
          else
            goto 81
          endif
          CALL EFOPSEQ(IUM,LAPROB,4,IER)
          IF(IER.NE.0)goto 81

          haveglobal=.FALSE.  ! global preferences not yet available
          CALL MFWRIT(IUM)
          CALL ERPFREE(IUM,ISTAT)
          OUTS=' '
          WRITE(OUTS,'(A,A)')'Saved fluid flow network: ',
     &LAPROB(1:LNBLNK(LAPROB))
          CALL EDISP(IUOUT,OUTS)
          CALL EDISP(IUOUT,' ')
        
        ENDIF
        if(NCOMP.gt.0.and.nnod.gt.0)then
          found=.false.
          do 42 in=1,NCOMP
            if(ICAAS(in).ne.0)then
              found=.true.
            endif
  42      continue
          if(.not.found)then
            CALL EASKAB(
     &        'No linkages set between the mass flow nodes and the',
     &        'thermal zones. Link now?','Yes','No',
     &        IW,nbhelp)
              if(IW.eq.1)goto 10
          endif
        endif

C Update the configuration file so that connections and mass flow
C network are know.
        call edisp(iuout,' ')
        call edisp(iuout,' Updating configuration mass flow links...')
        CALL EMKCFG('-',IER)
        RETURN
      else
        ino=-1
        goto 12
      endif
      goto 10

C File rewind errors.
  999 WRITE(OUTS,'(2a)') 
     &  ' Error rewinding flow network file ',LAPROB
      CALL EDISP(iuout,OUTS)
      ier=1
      RETURN

      END

C *************** MFNDES
C Fluid flow model description: edit/add/delete/copy nodes. 
C Called with four different modes.
C MOD - 'A' - Add
C       'D' - Delete
C       'E' - Edit
C       'C' - Copy
C 
      SUBROUTINE MFNDES(MOD,INOD)
#include "net_flow.h"
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "epara.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
#include "prj3dv.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLOWIT/fndegc,imix 
      common/MFLDOC/DEPRE(MPRD)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      CHARACTER outs*124,prt*60,DEPRE*40
      CHARACTER NDNAM*12,NODID*12,T12*12
      CHARACTER LAPROB*72,t72*72,t144*144
      CHARACTER PCITM(35)*42,KEY*1,MOD*1
      LOGICAL OK,dok
      integer IVV  ! for radio button
      integer NITMS,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MFNDES'  ! set for subroutine

      CALL USRMSG(' ','Input/edit node data: ','-')

      IFNOD=INOD
C Add/Edit a node to the network. 
      IF(MOD.EQ.'A'.OR.MOD.EQ.'E')THEN
        IF(MOD.EQ.'A')THEN
          IFNOD=NNOD+1
        ELSE
          IFNOD=INOD
        ENDIF

        helptopic='flow_node_names'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IF(MOD.EQ.'A')THEN
          NODID='new_node'
        ELSE
          NODID=NDNAM(IFNOD)
        ENDIF
   11   T12=NODID
        CALL EASKS(T12,' ',' Node name (<= 12 characters): ',12,
     &' ','node name',IER,nbhelp)
        call st2name(T12,NODID)
        DO 16 IN=1,NNOD
          IF(NODID(1:12).EQ.NDNAM(IN)(1:12).AND.IN.NE.IFNOD)THEN
            dok=.true.
            call askok('This a duplicate name...','Try again?',
     &        OK,dok,nbhelp)
            IF(.NOT.OK)RETURN
            goto 11
          endif
  16    CONTINUE
        write(NDNAM(IFNOD),'(a)') NODID(1:12)

C Input node characteristics; start with node fluid
        write(outs,'(3a)') 'Fluid through node ',NDNAM(IFNOD),':'
        helptopic='node_fluid_type'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(imix.eq.1)then
          NDFLD(IFNOD)=1
        elseif(imix.eq.2)then
          NDFLD(IFNOD)=2
        else
          if(NDFLD(IFNOD).eq.1)then
            CALL EASKAB(outs,'(currently air)','air','water',
     &        IW,nbhelp)
          elseif(NDFLD(IFNOD).eq.2)then
            CALL EASKAB(outs,'(currently water)','air','water',
     &        IW,nbhelp)
          else
            CALL EASKAB(outs,'(currently undefined)','air','water',
     &        IW,nbhelp)
          endif
          NDFLD(IFNOD)=IW
        endif

C Node type
   20   if(NDTYP(IFNOD).eq.0)then        
          prt='Node type & pressure: (currently: internal & unknown)'
        elseif(NDTYP(IFNOD).eq.1)then
          prt='Node type & pressure: (currently: internal & known)'
        elseif(NDTYP(IFNOD).eq.2)then
          prt='Node type & pressure: (currently: boundary & known)'
        elseif(NDTYP(IFNOD).eq.3)then
          prt=
     &'Node type & pressure: (currently: boundary & wind induced)'
        endif
        helptopic='node_type_summary'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IVV=1
        CALL EASKATOG(prt,' ',
     &  'internal:unknown','internal:known','boundary:known',
     &  'boundary:wind induced','continue',' ',' ',IVV,nbhelp)
        IF(IVV.EQ.5) RETURN
        NDTYP(IFNOD)=IVV-1
        IF(NDTYP(IFNOD).EQ.3.AND.NDFLD(IFNOD).NE.1)THEN
          CALL USRMSG(' ',' Fluid type must be air','-')
          call usrmsg(' ',' ','-')
          GOTO 20
        ENDIF

C Get associated zone volume and height if type 0 or 1.
        IF(NDTYP(IFNOD).EQ.0.or.NDTYP(IFNOD).EQ.1.AND.NCOMP.gt.0)THEN
          dok=.true.
          helptopic='node_type_summary'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK('Is this flow node associated with a zone?',
     &      '(see help)',OK,dok,nbhelp)
          IF(OK)THEN

C Redraw the bluiding in the graphic screen.
            MODBND=.TRUE.
            MODLEN=.TRUE.
            ITDSP=1; ITBND=1; ITEPT=0; ITZNM=0
            ITSNM=1; ITVNO=1; ITORG=0; ITSNR=1
            ITGRD=1; GRDIS=0.0; ITPPSW=0
            nzg=NCOMP
            if(nzg.gt.0)then
              DO 44 I=1,nzg
                nznog(I)=I
  44          CONTINUE

C (Re)Set all surfaces to standard line width.
              CALL INLNST(1)
              izgfoc=0
              CALL ADJVIEW(IER)
            endif

            write(outs,'(a,a)')'Which zone is associated with ',
     &        NDNAM(IFNOD)
            IC=-1
            IZ=-1
            WRITE(OUTS,'(A)') 
     &        'Select zone to associated with this node. '
            CALL ASKZONE(IC,0,OUTS,'Select zone:','-','mfn locate',IER)
            call usrmsg(' ',' ','-')
            if(IC.EQ.0.OR.IC.EQ.-1)then
              ZVOL=SUPNOD(IFNOD,2)
              VALH=HNOD(IFNOD)
            else
              ICAAS(IC)=IFNOD
              VALH=ZCOG(IC,3)
              ZVOL=VOL(IC)
            endif
          else
            ZVOL=SUPNOD(IFNOD,2)
          endif
        ELSE
          ZVOL=0.0
          VALH=0.0
        ENDIF

C Node supplementary data; zero these data items first.
        DO 35 I=1,MNDS
          SUPNOD(IFNOD,I)=0.
   35   CONTINUE
      
C Depending on the node type:
        IF(NDTYP(IFNOD).EQ.0)THEN

C NDTYP=0; internal node, unknown pressure.
  22      CALL EASKR(ZVOL,' ',' Node volume (m^3) ? ',
     &         0.0,'F',0.,'-',1.0,'node volume',IER,nbhelp)
          if(IER.ne.0)goto 22
          SUPNOD(IFNOD,2)=ZVOL
        ELSEIF(NDTYP(IFNOD).EQ.1)THEN

C NDTYP=1; internal node, known pressure.
   23     VAL=SUPNOD(IFNOD,1)
          CALL EASKR(VAL,' ',' Node pressure (Pa) ? ',
     &       0.0,'-',0.0,'-',1.0,'node pressure',IER,nbhelp)
          SUPNOD(IFNOD,1)=VAL
          CALL EASKR(ZVOL,' ',' Node volume (m^3) ? ',
     &       0.0,'F',0.,'-',1.0,'node volume',IER,nbhelp)
          if(IER.ne.0)goto 23
          SUPNOD(IFNOD,2)=ZVOL
        ELSEIF(NDTYP(IFNOD).EQ.2)THEN

C NDTYP=2; boundary node, known pressure and temperature flag.
          VAL=SUPNOD(IFNOD,1)
   60     CALL EASKR(VAL,' ',' Node pressure (Pa) ? ',
     &       0.0,'-',0.0,'-',1.0,'node pressure',IER,nbhelp)
          SUPNOD(IFNOD,1)=VAL
          CALL EASKAB(' ','temperature flag indicating: ',
     &    'temperature is a constant',
     &    'use outside air temperature',IW,nbhelp)
          IF(IER.NE.0)GOTO 60
          SUPNOD(IFNOD,2)=FLOAT(IW-1)
        ELSEIF(NDTYP(IFNOD).EQ.3)THEN

C NDTYP=3; boundary node, wind induced pressure.

C Allow the user to determine the surface associated with this node. 
          IZ=-1
          helptopic='node_type_summary'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('wind-pressure surface',nbhelp,'-',0,0,IER)

C Redraw the building in the graphic screen.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          ITDSP=1; ITBND=1; ITEPT=0; ITZNM=0
          ITSNM=1; ITVNO=1; ITORG=0; ITSNR=1
          ITGRD=1; GRDIS=0.0; ITPPSW=0
          nzg=NCOMP
          if(nzg.gt.0)then
            DO 45 I=1,nzg
              nznog(I)=I
  45        CONTINUE

C (Re)Set all surfaces to standard line width.
            CALL INLNST(1)
            izgfoc=0
            CALL ADJVIEW(IER)
          endif
          WRITE(OUTS,'(A)') 
     &'Select the zone containing the surface. '
          CALL ASKZONE(IZ,0,OUTS,'Select zone','-','wpn locate',IER)
          IF(IZ.NE.0) VALH=ZCOG(IZ,3)
          call usrmsg(' ',' ','-')
          if(IZ.eq.0) goto 20

C General image option flags.
          call georead(IFIL+1,LGEOM(IZ),IZ,0,ITRU,IER)
          MODIFYVIEW=.TRUE.
          MODBND=.TRUE.
          CALL INLNST(1)
          ITZNM=0
          ITSNM=0
          ITORG=1
          ITSNR=1
          ITGRD=1
          ITVNO=1
          nzg=1
          nznog(1)=IZ
          izgfoc=IZ
          CALL ADJVIEW(IER)

          ISO=1
          WRITE(OUTS,'(A)')
     &'Select the surface subjected to the wind pressure:'
          CALL EPMENSV
          CALL EASKSUR(IZ,ISO,'-',OUTS,' ',IER)
          CALL EPMENRC
 
C Use SSURCOG as the average height of this surface
          ZAVE=0.0
          icc=izstocn(iz,iso)
          ZAVE=SSURCOG(icc,3)
          VALH=ZAVE
          
C Create a menu showing the available database items. 
C Allow user to select one. Begin by reading current database
C (signal by passing a blank string to erprcdb).
          t144='  '
          CALL ERPRCDB(t144,0,3,IER)
          if (ier.ne.0) THEN
            CALL EDISP(IUOUT,
     &        'Error opening pressure coefficients database.')
            RETURN
          endif

C Setup multi-page menu. 
          MHEAD=2
          MCTL=4
          ILEN=NPRE
          IPACT=CREATE
          CALL EKPAGE(IPACT)

C Initial menu entry setup.
 70       IER=0
          ILEN=NPRE
          IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
          PCITM(1)=' W:H  |  Exposure  |  Pitch (roof)'
          PCITM(2)='  (W:H = width to height ratio)   '
          M=MHEAD
          DO 201 IM=1,NPRE
            IF(IM.GE.IST.AND.(IM.LE.(IST+MIFULL)))THEN
              M=M+1
              CALL EMKEY(M,KEY,IER)   
              WRITE(PCITM(M),'(A1,1X,A)')KEY,DEPRE(IM)
            ENDIF
  201     CONTINUE

C Number of actual items displayed.
          NITMS=M+MCTL

C If a long list include page facility text.      
          IF(IPFLG.EQ.0)THEN
            PCITM(M+1)='  ________________________  '
          ELSE
            WRITE(PCITM(M+1),15)IPM,MPM 
 15       FORMAT   ('0 ---Page: ',I2,' of ',I2,' ---')
          ENDIF
          PCITM(M+2)=' '
          PCITM(M+3)='? help                   '
          PCITM(M+4)='- exit this menu         '
          IVERT=-1

C Help text for this menu.
          helptopic='overview_pressure_db'
          call gethelptext(helpinsub,helptopic,nbhelp)
  
C Now display the menu.
          call USRMSG('Select the pressure coefficient set which',
     &'best matches this surface. (see help) ','-') 
          CALL EMENU('Pressure Coefficient Sets:',PCITM,NITMS,IVERT)
          IF(IVERT.EQ.NITMS)THEN
            CALL USRMSG(
     &      'You must select one pressure coefficient set!',' ','W')
            GOTO 70
          ELSEIF(IVERT.EQ.NITMS-1)THEN
            helptopic='overview_pressure_db'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('databases overview',nbhelp,'-',0,0,IER)
            GOTO 70
          ELSEIF(IVERT.EQ.NITMS-3)THEN

C If there are enough items allow paging control via EKPAGE.
            IF(IPFLG.EQ.1)THEN
              IPACT=EDIT
              CALL EKPAGE(IPACT)
            ENDIF
            GOTO 70
          ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(NITMS-MCTL+1))THEN

C Let user select a pc set.
            CALL KEYIND(NITMS,IVERT,IFOC,IO)
            SUPNOD(IFNOD,1)=IFOC
          ELSE
            IVERT=-1
            GOTO 70
          ENDIF

C Ask whether choice is OK.
          write(t72,'(a,a)')'Use ',DEPRE(IFOC)
          dok=.true.
          CALL ASKOK(t72,'as the pressure coefficient set?',
     &      OK,dok,nbhelp)
          IF(.NOT.OK)GOTO 70

C Input angle external node.
          helptopic='boundary_angle_height'
          call gethelptext(helpinsub,helptopic,nbhelp)

C Ask the user to select the external surface associated with the node.
C and extract the azimuth info (via SSPAZI). 
          IF(IZ.EQ.0) THEN
            VAL=SUPNOD(IFNOD,2)
          ELSE
            icc=izstocn(iz,iso)
            VAL=SSPAZI(icc)
          ENDIF
          CALL EASKR(VAL,' ',
     &       'Surface azimuth angle (degrees clockwise from north): ',
     &        0.0,'F',359.0,'F',1.0,'surf azimuth angle',IER,nbhelp)
          IF(IER.NE.0)GOTO 70
          SUPNOD(IFNOD,2)=VAL
        ENDIF

C Node reference height.
        helptopic='boundary_angle_height'
        call gethelptext(helpinsub,helptopic,nbhelp)
   21   CALL EASKR(VALH,' ',' Node reference height above datum (m)?',
     &  -1000.0,'F',1000.0,'F',1.0,'reference height',IER,nbhelp)
        if(IER.ne.0)goto 21
        HNOD(IFNOD)=VALH

C Node temperature (zero out NDTYP=2 & Tflag=1 and NDTYP=3 nodes)
C If the volume of the internal node is not zero then ask
C user for temperature.
        ITND(IFNOD)=0
        TNOD(IFNOD)=0.0
        IF(NDTYP(IFNOD).NE.3.AND.
     &    (NDTYP(IFNOD).NE.2.AND.SUPNOD(IFNOD,2).gt.0.001)) THEN
          helptopic='initail_node_temp'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKAB('Node temperature: ','(see help)',
     &    'Set initial or constant value',
     &    'Set to temperature of another flow node',IW,nbhelp)
          IT=IW-1
          IF(IT.EQ.0) THEN
             ITND(IFNOD)=0
             if(TNOD(IFNOD).lt.0.001)then
               VAL=fndegc
             else
               VAL=TNOD(IFNOD)
            endif
            CALL EASKR(VAL,' ','Node temperature (C) ? ',
     &        -100.0,'F',1000.0,'F',fndegc,'Node temperature',IER,
     &        nbhelp)
             TNOD(IFNOD)=VAL
          ELSE

C Pick associated node.
C Calls cut-down version of ASKNOD (ASKRNOD) to prevent problems with 
C recursion.
            CALL EPMENSV
            call ASKRNOD('  associated node','-',I,IER)
            CALL EPMENRC
            ITND(IFNOD)=I
            write(outs,*)'Current node volume is ',SUPNOD(IFNOD,2)
            call edisp(iuout,outs)
            write(outs,*)'Associated node volume is ',SUPNOD(I,2)
            call edisp(iuout,outs)
            dok=.true.
            helptopic='flow_node_volume'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call askok('Use volume of associated node?',' ',
     &        OK,dok,nbhelp)
            if(ok)SUPNOD(IFNOD,2)=SUPNOD(I,2)
          END IF
          call usrmsg(' ',' ','-')
        ENDIF
        IF(MOD.EQ.'A') NNOD=NNOD+1
        RETURN  
      ELSEIF(MOD.EQ.'D')THEN
  
C Delete a mode from the network
C Ask if it is OK to delete associated connections if not then return
        IFNOD=INOD
        helptopic='flow_node_delete'
        call gethelptext(helpinsub,helptopic,nbhelp)
        dok=.true.
        CALL ASKOK('Delete connections associated with this node?',
     &    '(Recommended)',OK,dok,nbhelp)
        IF(.NOT.OK) RETURN  
      
C Delete associated connections
        DO 40 ICON=NCNN,1,-1
          IF(NODPS(ICON).EQ.IFNOD.OR.NODNE(ICON).EQ.IFNOD)THEN
             CALL MFLDES('D',ICON)
             WRITE(OUTS,'(A,I3)') 'Deleted connection ... ',ICON
             CALL EDISP(IUOUT,OUTS)
          ENDIF
  40    CONTINUE

C Now delete the node. 
        IF(IFNOD.LT.NNOD)THEN
          DO 100 I=IFNOD,NNOD-1
            NDNAM(I)=NDNAM(I+1)
            NDFLD(I)=NDFLD(I+1)
            NDTYP(I)=NDTYP(I+1)
            HNOD(I)=HNOD(I+1)
            DO 110 J=1,MNDS
              SUPNOD(I,J)=SUPNOD(I+1,J)
 110        CONTINUE
            ITND(I)=ITND(I+1)
            TNOD(I)=TNOD(I+1)
 100      CONTINUE
        ENDIF
        NNOD=NNOD-1

C Copy a node in the network
      ELSEIF(MOD.EQ.'C')THEN
        
C Firstly check that the maximum number of nodes is not exceeded.
        IF(NNOD+1.GT.MNOD)THEN
          CALL EDISP(IUOUT,
     &'ERROR: maximum number of nodes will be exceeded ') 
          RETURN
        ENDIF

        IFNOD=NNOD+1

C Get a (unique) name for the copied node. 
        helptopic='flow_node_names'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IF(MOD.EQ.'A')THEN
          NODID='new_node'
        ELSE
          NODID=NDNAM(IFNOD)
        ENDIF
 111    T12=NODID
        CALL EASKS(T12,' ',' Node name (<= 12 characters): ',12,
     &' ','node name',IER,nbhelp)
        call st2name(T12,NODID)
        DO 161 IN=1,NNOD
          IF(NODID(1:12).EQ.NDNAM(IN)(1:12).AND.IN.NE.IFNOD)THEN
            call usrmsg(' ',' This a duplicate name. ','W')
            goto 111
          endif
 161    CONTINUE
        write(NDNAM(IFNOD),'(a)') NODID(1:12)

        IFNOD=NNOD+1
        NDFLD(IFNOD)=NDFLD(INOD)
        NDTYP(IFNOD)=NDTYP(INOD)
        HNOD(IFNOD)=HNOD(INOD)
        DO 120 J=1,MNDS
          SUPNOD(IFNOD,J)=SUPNOD(INOD,J)
 120    CONTINUE
        ITND(IFNOD)=ITND(INOD)
        TNOD(IFNOD)=TNOD(INOD)  
        NNOD=NNOD+1       
      ENDIF


      RETURN 
      END

C ************************ MFCDES 
C Fluid flow model description: input or edit component data
C Called with four different modes.
C MOD - 'A' - Add
C       'D' - Delete
C       'E' - Edit
C       'C' - Copy
      SUBROUTINE MFCDES(MOD,ICMP)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW6/IVALCM(MCMV)
      COMMON/MFLOW7/LVALCM(MCMV)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      dimension clist(MCMV),IVALC(MCMV)
      CHARACTER outs*124,clist*68,prompt*32,T12*12
      CHARACTER LVALCM*60,LTPCMP*60,CMNAM*12,CMPID*12, MOD*1
      LOGICAL OK,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MFCDES'  ! set for subroutine
   
      IF(MOD.EQ.'A'.OR.MOD.EQ.'E')THEN

        IF(MOD.EQ.'A')THEN
          IFCMP=NCMP+1
          CMPID='New_comp'
        ELSE
          IFCMP=ICMP
          CMPID=CMNAM(IFCMP)
        ENDIF
        helptopic='flow_network_comp_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
 4      T12=CMPID
        CALL EASKS(T12,' ',' Component name (<= 12 characters):',
     &    12,' ','comp name',IER,nbhelp)
        call st2name(T12,CMPID)
        ICMP=0
        DO 5 I=1,NCMP
          if(CMPID(1:12).EQ.CMNAM(I)(1:12).AND.I.NE.IFCMP)then
          call usrmsg(' ',' Duplicate name...','W')
          goto 4
        endif
  5     CONTINUE
        CMNAM(IFCMP)=CMPID(1:12)

C Set component type description and zero out all sup. data items
C if adding a component.
        if(MOD.EQ.'A')then
          DO 55 I=1,MCMS
            SUPCMP(IFCMP,I)=0.0
   55     CONTINUE
        endif
      
C Input component characteristics; start with component type.
   20   helptopic='flow_cmp_type_list'
        call gethelptext(helpinsub,helptopic,nbhelp)
        INPIC=1

C Make up a list of component types.
        DO 45 IC=1,MCMV
          WRITE(clist(ic),'(I4,A,A)') IVALCM(IC),' : ',LVALCM(IC)
   45   CONTINUE 

        IF(MOD.EQ.'E')THEN
          write(prompt,'(a,i4)') 'Current component type is: ',
     &ITPCMP(IFCMP)
        ELSE
          write(prompt,'(a)') 'Current component type is: NONE'
        ENDIF
        CALL EPICKS(INPIC,IVALC,' Select a component: ',prompt,
     &      68,MCMV,clist,'Component type & description',IER,nbhelp)
        IF(INPIC.EQ.0) RETURN
        if(IVALC(1).ne.0)then
          IC=IVALC(1)
          ITPCMP(IFCMP)=IVALCM(IC)
          LTPCMP(IFCMP)=LVALCM(IC)
        else
          goto 20
        endif

C Then call appropriate supplementary data input subroutine.
        WRITE(outs,'(1X,A60)') LTPCMP(IFCMP)
        call edisp(iuout,outs)

        IF(ITPCMP(IFCMP).EQ. 10) THEN
          CALL MF010I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ. 11) THEN
          CALL MF011I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ. 12) THEN
          CALL MF012I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ. 15) THEN
          CALL MF015I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ. 17) THEN
          CALL MF017I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ. 20) THEN
          CALL MF020I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ. 25) THEN
          CALL MF025I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ. 30) THEN
          CALL MF030I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ. 35) THEN
          CALL MF035I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ. 40) THEN
          CALL MF040I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ. 50) THEN
          CALL MF050I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.110) THEN
          CALL MF110I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.120) THEN
          CALL MF120I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.130) THEN
          CALL MF130I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.210) THEN
          CALL MF210I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.211) THEN
          CALL MF211I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.220) THEN
          CALL MF220I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.230) THEN
          CALL MF230I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.240) THEN
          CALL MF240I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.250) THEN
          CALL MF250I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.310) THEN
          CALL MF310I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.410) THEN
          CALL MF410I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.420) THEN
          CALL MF420I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.460) THEN
          CALL MF460I(IFCMP,IERL)
        ELSE IF(ITPCMP(IFCMP).EQ.500) THEN
          CALL MF500I(IFCMP,IERL)
        ELSE
          CALL USRMSG(' Unknown component type',' try again..','W')
            goto 20
        END IF

        IF(IERL.NE.0)THEN
          CALL USRMSG(' There was a problem encountered while',
     &              ' getting supplementary data.','W')
        ENDIF
        IF(MOD.EQ.'A') NCMP=NCMP+1
       
      ELSEIF(MOD.EQ.'D')THEN
        IFCMP=ICMP

C Ask if it is OK to delete associated connections if not then return
        dok=.true.
        helptopic='flow_component_delete'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK('Delete connections associated with component?',
     &    '(see help)',OK,dok,nbhelp)
        IF(.NOT.OK) RETURN  
      
C Delete associated connections
        DO 40 ICON=NCNN,1,-1
          IF(ITPCON(ICON).EQ.IFCMP)THEN
             CALL MFLDES('D',ICON)
             WRITE(OUTS,'(A,I3)') 'Deleted connection ... ',ICON
             CALL EDISP(IUOUT,OUTS)
          ENDIF
  40    CONTINUE

C Now delete the component
        IF(ICMP.LT.NCMP) THEN
          DO 76 I=IFCMP,NCMP-1
            CMNAM(I)=CMNAM(I+1)
            LTPCMP(I)=LTPCMP(I+1)
            ITPCMP(I)=ITPCMP(I+1)
            ISDCMP(I)=ISDCMP(I+1)
            ISDCNN(I)=ISDCNN(I+1)
            DO 77 J=1,MCMS
              SUPCMP(I,J)=SUPCMP(I+1,J)
  77        CONTINUE
  76      CONTINUE
        ENDIF 
        NCMP=NCMP-1         

      ELSEIF(MOD.EQ.'C')THEN
        IFCMP=NCMP+1
        CMPID='New_comp'
        helptopic='flow_network_comp_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
 47     T12=CMPID
        CALL EASKS(T12,' ',' Component name (<= 6 characters):',
     &    12,' ','comp name',IER,nbhelp)
        call st2name(T12,CMPID)
        DO 57 I=1,NCMP
          if(CMPID(1:12).EQ.CMNAM(I)(1:12).AND.I.NE.IFCMP)then
          call usrmsg(' ',' Duplicate name...','W')
          goto 47
        endif
  57    CONTINUE
        CMNAM(IFCMP)=CMPID(1:12)
        LTPCMP(IFCMP)=LTPCMP(ICMP)
        ITPCMP(IFCMP)=ITPCMP(ICMP)
        ISDCMP(IFCMP)=ISDCMP(ICMP)
        ISDCNN(IFCMP)=ISDCNN(ICMP)
        DO 88 J=1,MCMS
          SUPCMP(IFCMP,J)=SUPCMP(ICMP,J)
  88    CONTINUE
        NCMP=NCMP+1     
      ENDIF


      RETURN

      END

C *************** MFLDES 
C Fluid flow model file: input or edit connections (linkages)
C Called with four different modes.
C MOD - 'A' - Add
C       'D' - Delete
C       'E' - Edit
C       'C' - Copy
      SUBROUTINE MFLDES(MOD,ICON)

#include "net_flow.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      CHARACTER outs*124
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12,MOD*1
      LOGICAL OK,dok,DMFCON

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MFLDES'  ! set for subroutine

      IF(MOD.EQ.'A'.OR.MOD.EQ.'E')THEN
        IF(MOD.EQ.'A')THEN
          IFCN=NCNN+1
        ELSEIF(MOD.EQ.'E')THEN
          IFCN=ICON
        ENDIF

C Allow a node to be added, checked for uniqueness, matching fluid 
C types and return data. 
  19    IF(MOD.EQ.'E')THEN
          WRITE(OUTS,'(A,A12,A)')
     &'  First (+ve) node: (currently ',NDNAM(NODPS(IFCN)),')'
        ELSE
          WRITE(OUTS,'(A)')
     &'  First (+ve) node: (currently NONE)'
        ENDIF
        CALL EPMENSV
        call ASKRNOD(outs,'-',IS,IER)
        CALL EPMENRC
        IF(IS.EQ.0) RETURN
        IF(MOD.EQ.'E')THEN
          WRITE(OUTS,'(A,A12,A)')
     &'  Second (-ve) node: (currently ',NDNAM(NODNE(IFCN)),')'
        ELSE
          WRITE(OUTS,'(A)')
     &'  Second (-ve) node: (currently NONE)'
        ENDIF
        CALL EPMENSV
        call ASKRNOD(outs,'-',IE,IER)
        CALL EPMENRC
        IF(IE.EQ.0) RETURN
        if(IS.EQ.IE)then
          dok=.true.
          helptopic='flow_connect_to_self'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call askok('You cannot connect a node to itself...',
     &      'try again?',OK,dok,nbhelp)
          IF(.NOT.OK) RETURN
          goto 19
        endif
        if(NDFLD(IS).NE.NDFLD(IE))then
          dok=.true.
          helptopic='flow_fluid_not_match'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call askok('Fluid types to not match...','try again?',
     &      OK,dok,nbhelp)
          IF(.NOT.OK) RETURN
          goto 19
        endif

C Store the node numbers. 
        INODPS=IS
        INODNE=IE

C See if duplicate connection already exists.

        DMFCON=.FALSE.
        DO 22 ICNN=1,NCNN
          IF((INODPS.EQ.NODPS(ICNN).AND.INODNE.EQ.NODNE(ICNN)).OR.
     &(INODPS.EQ.NODNE(ICNN).AND.INODNE.EQ.NODPS(ICNN))) THEN

C Confirm if a new connection is to be added.
            IF(MOD.EQ.'A'.AND.ICNN.NE.IFCN)THEN
              IF(.NOT.DMFCON)THEN
                dok=.true.
                helptopic='flow_additional_con'
                call gethelptext(helpinsub,helptopic,nbhelp)
                CALL ASKOK
     &            ('A connection already exists between these ',
     &            ' two nodes - do you want to add another? ',
     &            OK,dok,nbhelp)
                IF(.NOT.OK) RETURN  
                  DMFCON=.TRUE. 
                ENDIF
 
C Look for a component type 220, 230, 240 or 250.
                ICMP=ITPCON(ICNN)
                ICMPTP=ITPCMP(ICMP)
                IF(ICMPTP.EQ.220.OR.ICMPTP.EQ.230.OR.
     &             ICMPTP.EQ.240.OR.ICMPTP.EQ.250) THEN
                  dok=.true.
                  helptopic='flow_not_allowed_con'
                  call gethelptext(helpinsub,helptopic,nbhelp)
                  CALL ASKOK(
     &'You cannot have a duplicate connection between nodes for',
     &'types 220, 230, 240 and 250. Try again?',OK,dok,nbhelp)  
                IF(.NOT.OK)THEN
                  RETURN
                ELSE
                  GOTO 19
                ENDIF  
              ENDIF
            ENDIF
          ENDIF
  22    CONTINUE


C Set the new connection nodes
        NODPS(IFCN)=IS
        NODNE(IFCN)=IE

C Input connection data; start with height of +ve linkage point.
  60    WRITE(outs,1060) NDNAM(NODPS(IFCN))
 1060   FORMAT(' Height of +ve linkage point relative to node ',A12)
        IF(MOD.EQ.'E')THEN
          VAL=HGTPS(IFCN)
        ELSE
C Calculate a suggested linkage height.
          VAL=((HNOD(NODPS(IFCN))+HNOD(NODNE(IFCN)))/2.)
     &-HNOD(NODPS(IFCN))
        ENDIF
        helptopic='flow_node_height_diff'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(VAL,outs,' ? ',
     &    -100.0,'W',100.0,'W',0.0,'+relative node height',IER,nbhelp)
        IF(IER.NE.0) GOTO 60
        HGTPS(IFCN)=VAL

C Height of -ve linkage point relative to NODNE.
   70   WRITE(outs,1070) NDNAM(NODNE(IFCN))
 1070   FORMAT(' Height of -ve linkage point relative to node ',A12)
        IF(MOD.EQ.'E')THEN        
          VAL=HGTNE(IFCN)
        ELSE
          VAL=((HNOD(NODPS(IFCN))+HNOD(NODNE(IFCN)))/2.)
     &-HNOD(NODNE(IFCN))
        ENDIF
        CALL EASKR(VAL,outs,' ? ',
     &    -100.0,'W',100.0,'W',0.0,'- relative node height',IER,nbhelp)
        IF(IER.NE.0) GOTO 70
        HGTNE(IFCN)=VAL

C Linking fluid flow component.
        IF(MOD.EQ.'E')THEN
          WRITE(OUTS,'(A,A12,A)')
     &'  Linking component: (currently ',CMNAM(ITPCON(IFCN)),')'
        ELSE
          WRITE(OUTS,'(A)')
     &'  Linking component: (currently NONE)'
        ENDIF
   80   CALL EPMENSV
        call ASKRCMP(outs(1:lnblnk(outs)),'-',ICMP,IER)
        CALL EPMENRC

C Checks for component validity ...

C Also check that a flow merge/split component is not being used in a
C duplicate connection.
        ICMPTP=ITPCMP(ICMP)
        DMFCON=.FALSE.
        IF(ICMPTP.EQ.220.OR.ICMPTP.EQ.230.OR.
     &    ICMPTP.EQ.240.OR.ICMPTP.EQ.250) THEN
          DO 88 ICNN=1,NCNN
            IF((INODPS.EQ.NODPS(ICNN).AND.INODNE.EQ.NODNE(ICNN)).OR.
     &(INODPS.EQ.NODNE(ICNN).AND.INODNE.EQ.NODPS(ICNN))) THEN
              IF(ICNN.NE.IFCN)THEN
                DMFCON=.TRUE.
              ENDIF
            ENDIF
 88       CONTINUE
          IF(DMFCON)THEN
            dok=.true.
            helptopic='flow_not_allowed_con'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK(
     &    'You cannot use types 220, 230, 240 and 250 twice between',
     &    'the same two nodes. Try again?',OK,dok,nbhelp)  
            IF(.NOT.OK)THEN
              RETURN
            ELSE
              GOTO 80
            ENDIF 
          ENDIF 
        ENDIF


C This is a known component; check if fluid type ok (= identical to
C fluid type of node on either side (actually 1 side is sufficient)).
        IF(NDFLD(NODPS(IFCN)).NE.INT(SUPCMP(ICMP,1)))then
          call usrmsg(' ',' Fluid type does not match!','W')
          GOTO 80
        endif
        IF(NDFLD(NODNE(IFCN)).NE.INT(SUPCMP(ICMP,1)))then
          call usrmsg(' ',' Fluid type does not match!','W')
          GOTO 80
        endif

        ITPCON(IFCN)=ICMP

C If supplementary node names for this component read the first one.
        IF(ISDCNN(ITPCON(IFCN)).GT.0) THEN 
          call edisp
     &(iuout,' Supplementary node name for this component...')
          write(outs,'(3A,I3,A)') ' currently: ',
     &NDNAM(NDSCNN(IFCN,1)),' (', NDSCNN(IFCN,1),')'
          call edisp(iuout,outs)
          call ASKRNOD('  supplementary node','-',ISN,IER)
          NDSCNN(IFCN,1)=ISN
        ENDIF

C If necessary read a second one.
        IF(ISDCNN(ITPCON(IFCN)).GT.1) THEN
          call edisp
     &(iuout,' 2nd supplementary node name for component...')
          write(outs,'(3A,I3,A)') ' currently: ',
     &NDNAM(NDSCNN(IFCN,2)),' (', NDSCNN(IFCN,2),')'
          call edisp(iuout,outs)
          call ASKRNOD(' 2nd supplementary node','-',ISN2,IER)
          NDSCNN(IFCN,2)=ISN2
        ENDIF

        IF(MOD.EQ.'A') NCNN=NCNN+1
        
      ELSEIF(MOD.EQ.'D')THEN

C Delete a connection
        IFCN=ICON
        IF(IFCN.LT.NCNN) THEN
          DO 76 I=IFCN,NCNN-1
            NODPS(I)=NODPS(I+1)
            NODNE(I)=NODNE(I+1)
            HGTPS(I)=HGTPS(I+1)
            HGTNE(I)=HGTNE(I+1)
            ITPCON(I)=ITPCON(I+1)
            NDSCNN(I,1)=NDSCNN(I+1,1)
            NDSCNN(I,2)=NDSCNN(I+1,2)          
  76      CONTINUE
        ENDIF
        NCNN=NCNN-1  

C Copy a connection   
      ELSEIF(MOD.EQ.'C')THEN
        ICMP=ITPCON(IFCN)
        ICMPTP=ITPCMP(ICMP)
        IF(ICMPTP.EQ.220.OR.ICMPTP.EQ.230.OR.
     &  ICMPTP.EQ.240.OR.ICMPTP.EQ.250) THEN
          CALL EDISP(IUOUT,
     &'You cannot have a duplicate connection between nodes for') 
          CALL EDISP(IUOUT,
     &'component types 220, 230, 240 or 250') 
        ELSE 
          IFCN=NCNN+1
          NODPS(IFCN)=NODPS(ICON)
          NODNE(IFCN)=NODNE(ICON)
          HGTPS(IFCN)=HGTPS(ICON)
          HGTNE(IFCN)=HGTNE(ICON)
          ITPCON(IFCN)=ITPCON(ICON)
          NDSCNN(IFCN,1)=NDSCNN(ICON,1)
          NDSCNN(IFCN,2)=NDSCNN(ICON,2)
          NCNN=NCNN+1  
        ENDIF
      ELSE
        RETURN
      ENDIF

      RETURN 
      END

C ***************** MFWRED 
C Fluid flow model file: edit or input wind speed reduction factor

      SUBROUTINE MFWRED

#include "net_flow.h"
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      CHARACTER outs*124
      LOGICAL OK,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MFWRED'  ! set for subroutine

C Warn if current wind reduction factor is zero
      helptopic='flow_wind_reduction'
      call gethelptext(helpinsub,helptopic,nbhelp)
      IF(WRED.LE.0.0)CALL usrmsg(' The current wind reduction factor ',
     &  ' effectively suggests zero wind pressure !!','W')
      WRITE(outs,1010) WRED
 1010 FORMAT(' Wind speed reduction factor [current value = ',F6.3,'];',
     &       ' edit this ?')
      dok=.true.
      CALL ASKOK(outs,' edit this (see help)?',OK,dok,nbhelp)
      IF(.NOT.OK)RETURN

C Calculation model ?
   11 helptopic='wind_reduction_opts'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABCD('Which calculation model: ','(see help)',
     &  'Input a value','Powerlaw','Logarithmic','LBL model',
     &  ICLC,nbhelp)
      ICALC=ICLC-1

C Direct input of wind speed reduction factor.
      IF(ICALC.EQ.0)THEN
        WRED=1.
  88    CALL EASKR(WRED,' ',' Local wind speed reduction factor ? ',
     &    0.0,'F',100.0,'W',1.0,'- wind speed reduction',IER,nbhelp)
        IF(IER.NE.0)GOTO 88
        RETURN
      ENDIF

C Allow the user to select the roof of the building
      CALL PHELPD('wind model',nbhelp,'-',0,0,IER)
      IZ=0
      CALL ASKZONE(IZ,0,'Select the zone containing the roof.',
     &  'Select zone','-','wpn locate',IER)
      ISZONE=IZ
      IF(IZ.NE.0) VALH=ZCOG(IZ,3)
      call usrmsg(' ',' ','-')
      if(IZ.eq.0) goto 11

      call georead(IFIL+1,LGEOM(IZ),IZ,1,iuout,IER)

C General image option flags.
C << might want to save current values for later recovery >>
      ITDSP=1; ITBND=1; ITEPT=0; ITZNM=0; ITSNM=0
      ITVNO=1; ITORG=1; ITSNR=1
      ITGRD=1; GRDIS=0.0; ITPPSW=0

      MODIFYVIEW=.TRUE.
      MODBND=.TRUE.
      CALL INLNST(1)
      ITVNO=0
      nzg=1
      nznog(1)=ISZONE
      izgfoc=ISZONE
      CALL ADJVIEW(IER)
      MODIFYVIEW=.TRUE.
      MODBND=.TRUE.

      ISO=1
      WRITE(OUTS,'(A)') 'Select the roof surface:'
      CALL EPMENSV
      CALL EASKSUR(ISZONE,ISO,'-',OUTS,' ',IER)
      CALL EPMENRC

C Use SSURCOG as the average height of this surface
      ZAVE=0.0
      icc=izstocn(iszone,iso)
      ZAVE=SSURCOG(icc,3)
      ROOFH=ZAVE

C Roof height ?
  89  CALL EASKR(ROOFH,' ',' Roof height ? ',
     &    0.0,'F',1000.0,'W',1.0,'Roof height',IER,nbhelp)
      IF(IER.NE.0)GOTO 89

C Depending on wind profile model
      IF(ICALC.EQ.1)THEN

C 'powerlaw' wind profile.
  90    CALL EASKR(CF1,'(see help)',' K factor: ',
     &    0.0,'F',100.0,'W',1.0,'K factor',IER,nbhelp)
        CALL EASKR(CF2,'(see help)',' a factor: ',
     &    0.0,'F',100.0,'W',1.0,'K factor',IER,nbhelp)
        if(ier.ne.0)goto 90
        WRED=CF1*ROOFH**CF2
        WRITE(outs,1011) WRED
 1011   FORMAT(' Wind speed reduction factor [new value = ',F6.3,'];')
        dok=.true.
        CALL ASKOK(outs,' Is this OK ?',OK,dok,nbhelp)
        IF(OK)RETURN
        GOTO 11
      ELSEIF(ICALC.EQ.2)THEN

C Logarithmic wind profile.
  91    CALL EASKR(CF1,'(see help)',' z0 for local site: ',
     &    0.0,'F',100.0,'W',1.0,'z0 factor',IER,nbhelp)
        CALL EASKR(CF2,'(see help)',' d  for local site: ',
     &    0.0,'F',ROOFH,'F',1.0,'d factor',IER,nbhelp)
        CALL EASKR(CF3,'(see help)',' z for meteo site [~ 10]   : ',
     &    0.0,'F',0.0,'-',1.0,'z0 factor',IER,nbhelp)
        CALL EASKR(CF4,'(see help)',' z0 for meteo site [~ 0.03]: ',
     &    0.0,'F',0.0,'-',1.0,'z0 factor',IER,nbhelp)
        CALL EASKR(CF5,'(see help)',' d for meteo site [~ 0.0]  : ',
     &    0.0,'F',CF3,'F',1.0,'z0 factor',IER,nbhlep)
        if(ier.ne.0)goto 91

C Based on:
C Us,l/Us,m = (z0,l/z0,m)**0.1 (acc. dictaat Wisse 1990)
        WRED=(CF1/CF4)**0.1 * ALOG((ROOFH-CF2)/CF1)/
     &                        ALOG((CF3  -CF5)/CF4)
        WRITE(outs,1012) WRED
 1012   FORMAT(' Wind speed reduction factor [new value = ',F6.3,'];')
        dok=.true.
        CALL ASKOK(outs,' is this OK ?',OK,dok,nbhelp)
        IF(OK)RETURN
        GOTO 11
      ELSEIF(ICALC.EQ.3)THEN

C LBL model wind profile.
 92     CALL PHELPD('Logarithmic wind profile.',nbhelp,'-',0,0,IER)
        CALL EASKR(CF1,' ',' a for local site: ',
     &    0.0,'F',0.0,'-',1.0,'a factor',IER,nbhelp)
        CALL EASKR(CF2,' ',' b  for local site: ',
     &    0.0,'F',0.0,'-',1.0,'b factor',IER,nbhelp)
        CALL EASKR(CF3,' ',' z for meteo site [~ 10]   : ',
     &    0.0,'F',0.0,'-',1.0,'z factor',IER,nbhelp)
        CALL EASKR(CF4,' ',' z0 for meteo site [~ 1.0]: ',
     &    0.0,'F',0.0,'-',1.0,'z0 factor',IER,nbhelp)
        CALL EASKR(CF5,' ',' d for meteo site [~ 0.15]  : ',
     &    0.0,'F',0.0,'-',1.0,'d factor',IER,nbhelp)
        if(ier.ne.0)goto 92

        WRED=CF1*(ROOFH/10.)**CF2/(CF4*(CF3/10.)**CF5)
        WRITE(outs,1013) WRED
 1013   FORMAT(' Wind speed reduction factor [new value = ',F6.3,'];')
        dok=.true.
        CALL ASKOK(outs,' is this OK ?',OK,dok,nbhelp)
        IF(OK)RETURN
        GOTO 11
      ENDIF

      RETURN
      END

C ********************* MFNAUTO 
C This routine auto-generates elements of the fluid-flow network.
C MODE = 'N' to generate internal nodes based on zone names and volumes.
C      = 'L' to generate flow connections (between zones) list
C      = 'Z' Autogenerate nodes, components and connections based on
C            information in zone operation files. It assumes this will
C            be a new network
C << test whether it might deal with additions to an existing network >>
C << code required to also create relevant flow controls in the case  >>
C << infiltration and/or ventilation schedules require a profile e.g. >>
C << fans run at a given fraction at different periods of the day.    >>

C << should mode Z be visible when user first enters flow facility? >>

      SUBROUTINE MFNAUTO(MODE,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
#include "net_flow.h"
#include "prj3dv.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW6/IVALCM(MCMV)
      COMMON/MFLOW7/LVALCM(MCMV)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      DIMENSION IVALS(MCOM),IFCAB(MCON)

C Maximum infiltration (finfmax) and ventilation (fvntmax) flow rates
C (m3/sec) for each zone. icompforinf is the component number associated
C with unique infiltration flow rates.
      dimension finfmax(MCOM),fvntmax(MCOM),icompforinf(MCOM)

C icompforvent is the component number associated with unique
C ventilation flow rates. isrczforvent is the source zone
C associated with the largest ventilation rate.
      dimension icompforvent(MCOM),isrczforvent(MCOM)
      integer icompforinf,icompforvent,isrczforvent
      real finfmax,fvntmax

      LOGICAL XST
      logical closevz,closeiz,close,foundit
      character NDNAM*12, outs*124, MODE*1
      character LAPROB*72
      CHARACTER LVALCM*60,LTPCMP*60,CMNAM*12

C Variables to keep track of the component index of the standard
C components needed for a network equivalent of zone operations.
      integer noinfforcurrent,noventforcurrent,crackforcurrent
      integer gr02forcurrent,gr04forcurrent,gr06forcurrent
      integer nodeforcurrent,IGN

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MFNAUTO'  ! set for subroutine

      IF(MODE.EQ.'N')THEN

C Auto generate internal nodes based on zone names and volumes.
        helptopic='flow_auto_generate_int'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(OUTS,'(A)') 
     &    'Which zones to include in the fluid flow network?'
        IZN=NCOMP

C Clear zone->node mapping
        DO 7 ICZ=1,IZN
          ICAAS(ICZ)=0
  7     CONTINUE
        CALL EPICKS(IZN,IVALS,' ',OUTS,12,NCOMP,zname,'zone list',
     &    IER,nbhelp)
   
        IF(IZN.EQ.0)THEN
          CALL USRMSG('No zones selected, returning.',' ','W')
          ier=1
          return
        ENDIF
  
        CALL USRMSG(
     &    'Generating flow nodes for selected zones ...',' ','-')  
        DO  10 IZT=1,IZN
          IZ=IVALS(IZT)
          IF(ICAAS(IZ).EQ.0)THEN
            NNOD=NNOD+1

C Get zone information. 
            WRITE(OUTS,'(A,A12)')
     &        'Generating fluid flow node for ',ZNAME(IZ)
            CALL edisp(iuout,OUTS)
            ICAAS(IZ)=NNOD

C Set node type to internal and fill in the data on volume and height 
C and temperature, setting the node name to the zone name.
            NDTYP(NNOD)=0
            NDFLD(NNOD)=1
            HNOD(NNOD)=ZCOG(IZ,3)
            SUPNOD(NNOD,2)=VOL(IZ)
            SUPNOD(NNOD,1)=0.0
            TNOD(NNOD)=20.0
            NDNAM(NNOD)=ZNAME(IZ)
            WRITE(OUTS,'(A,I3,3A)')'Node :',NNOD,' name: ',
     &        NDNAM(NNOD), ' type: internal, unknown pressure '
            CALL edisp(iuout,OUTS)
            WRITE(OUTS,'(A,F10.3,A,F10.3)') ' height (m): ',
     &        HNOD(NNOD), ' volume (m^3): ', SUPNOD(NNOD,2)
            CALL edisp(iuout,OUTS)
          ENDIF
  10    CONTINUE


C Later auto generation code to be added here. 
C Auto generate boundary nodes
C Auto generate components
      ELSEIF(MODE.EQ.'L')THEN

C Auto generate connections between zones which have surfaces
C detected as partition type connections. First scan through
C the internal connections list. 

C << ?? option to look at transparent surfaces facing the outside
C << and ask if they should be cracks?

C << ?? option to look at doors facing the outside and asking if they
C << should have cracks

        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,'Scanning internal connections ...')

C Clear connection->flow connection mapping flags.
        DO 18 ICN=1,NCON
          IFCAB(ICN)=0
  18    CONTINUE

        DO 20 ICN=1,NCON

C Scan connections looking for a type 3 connection
          IF(ICT(ICN).EQ.3)THEN
            IF(IFCAB(ICN).EQ.0)THEN
              IF(ICAAS(IC1(ICN)).EQ.0.OR.ICAAS(IC2(ICN)).EQ.0)THEN 

C Check that the zones have associated flow nodes
                GOTO 20
              ENDIF

C Draw the connected zones and surfaces on the screen.
C Redraw the affected zones in the graphic screen.
              DO 44 I=1,NCOMP
                nznog(I)=0
  44          CONTINUE

              MODBND=.TRUE.
              MODIFYVIEW=.TRUE.
              ITDSP=1; ITBND=1; ITEPT=0; ITZNM=0; 
              ITSNM=1; ITORG=0; ITGRD=1
              LINSTY(ICN)=2
              nzg=2
              nznog(1)=IC1(ICN)
              nznog(2)=IC2(ICN)
              CALL ADJVIEW(IER)

C (Re)Set all surfaces to standard line width.
              CALL INLNST(1)
              nznog(2)=0
              nznog(1)=0

C Ask if the user wants a connection between these two zones. 
              WRITE(OUTS,'(4a)') 'Found connection between ',
     &          NDNAM(ICAAS(IC1(ICN))),' and ',
     &          NDNAM(ICAAS(IC2(ICN)))
              CALL EASKABC(OUTS,'Make this a fluid flow link?',
     &          'Yes','No','Exit auto-connect',IOPT,nbhelp)
              IF(IOPT.EQ.1)THEN
                IFCN=NCNN+1
                NODPS(IFCN)=ICAAS(IC1(ICN))
                NODNE(IFCN)=ICAAS(IC2(ICN))
                HAVEG=(HNOD(IC1(ICN))+HNOD(IC2(ICN)))/2
                HGTPS(IFCN)=HAVEG-HNOD(IC1(ICN))
                HGTNE(IFCN)=HAVEG-HNOD(IC2(ICN))      
C Ask for the connecting component between the two nodes
                CALL ASKRCMP('Select connecting component:','-',IS,IER)

C If an invalid component is selected then skip the connection
                IF(IS.EQ.0) GOTO 20
                ITPCON(IFCN)=IS
                NDSCNN(IFCN,1)=0
                NDSCNN(IFCN,2)=0                 

C Update the flow connections list.
                NCNN=NCNN+1
              ELSEIF(IOPT.EQ.3)THEN
                RETURN
              ENDIF

C Set the connection flag to 1 to indicate the connection has been looked 
C at and deat with by the user.              
              IFCAB(ICN)=1

C Scan the flow connection list again to find the reciprocal connection.
C and set it's mapping flag as well - prevents user being asked about the
C reverse of the connection they have just dealt with.
              DO 23 ICN2=1,NCON
                IF((IC1(ICN).EQ.IC2(ICN2)).AND.(IC2(ICN).EQ.IC1(ICN2))
     &.AND.(IE1(ICN).EQ.IE2(ICN2)).AND.(IE2(ICN).EQ.IE1(ICN2)))THEN
                 IF(ICN2.NE.ICN)THEN
                   IFCAB(ICN2)=1  
                 ENDIF                
                ENDIF
  23          CONTINUE
            ENDIF
          ENDIF
  20    CONTINUE
        CALL EDISP(IUOUT,'Finished autogenerate connections.')
      ELSEIF(MODE.EQ.'Z')THEN

C Autogenerate based on information in zone operation files.
C Auto generate source and sink boundary nodes, make them
C wind-pressure nodes at 1m height and with flat sheltered
C roof pressure coef set facing south.  This method assumes that
C there are no existing nodes, components and connections.
        NNOD=NNOD+1
        write(NDNAM(NNOD),'(a)') 'inf_source'
        NDTYP(NNOD)=3
        NDFLD(NNOD)=1
        HNOD(NNOD)=1.0
        TNOD(NNOD)=0.0
        SUPNOD(NNOD,1)=29.0
        SUPNOD(NNOD,2)=180.0
        NNOD=NNOD+1
        write(NDNAM(NNOD),'(a)') 'inf_sink'
        NDTYP(NNOD)=3
        NDFLD(NNOD)=1
        HNOD(NNOD)=1.0
        TNOD(NNOD)=0.0
        SUPNOD(NNOD,1)=29.0
        SUPNOD(NNOD,2)=180.0

C A node-for-current-zone index that takes into account inf_source & inf_sink
        nodeforcurrent=2

        IZN=NCOMP

C Clear zone->node mapping
        DO 27 ICZ=1,IZN
          ICAAS(ICZ)=0
 27     CONTINUE
        WRITE(OUTS,'(A)') 'Which zones to include?'
        CALL EPICKS(IZN,IVALS,' ',OUTS,12,NCOMP,zname,'zone list',
     &    IER,nbhelp)
   
        IF(IZN.EQ.0)THEN
          CALL USRMSG('No zones were selected. Returning...',' ','W')
          ier=1
          return
        ENDIF 

        CALL edisp(iuout,'Generating flow nodes...')  
        DO  37 IZT=1,IZN

C Clear vent and infil flow rates and vent source zone index (initial pass).
          IZ=IVALS(IZT)
          finfmax(iz)=0.0
          fvntmax(iz)=0.0
          isrczforvent(iz)=0

          IZ=IVALS(IZT)
          IF(ICAAS(IZ).EQ.0)THEN
            NNOD=NNOD+1

C Get zone information. 
            WRITE(OUTS,'(A,A12)')
     &        'Generating flow node for zone:',ZNAME(IZ)
            CALL edisp(iuout,OUTS)
            ICAAS(IZ)=NNOD

C Set node type to internal and fill in the data on volume and height 
C and temperature, setting the node name to the zone name.
            NDTYP(NNOD)=0
            NDFLD(NNOD)=1
            HNOD(NNOD)=ZCOG(IZ,3)
            SUPNOD(NNOD,2)=VOL(IZ)
            SUPNOD(NNOD,1)=0.0
            TNOD(NNOD)=20.0
            NDNAM(NNOD)=ZNAME(IZ)
            WRITE(OUTS,'(A,I3,3A)')'Node :',NNOD,' name: ',
     &        NDNAM(NNOD), ' type: internal, unknown pressure '
            CALL edisp(iuout,OUTS)
            WRITE(OUTS,'(A,F10.3,A,F10.3)') ' height (m): ',
     &        HNOD(NNOD), ' volume (m^3): ', SUPNOD(NNOD,2)
            CALL edisp(iuout,OUTS)
          ENDIF
  37    CONTINUE
   
C A nothing happening infiltration fixed volume component (with
C zero flow), remember index via noinfforcurrent.
        NCMP=NCMP+1
        CMNAM(NCMP)='inflf_.0000'
        ITPCMP(NCMP)=IVALCM(7)
        LTPCMP(NCMP)=LVALCM(7)
        ISDCMP(NCMP)=2
        ISDCNN(NCMP)=0
        SUPCMP(NCMP,1)=1.0
        SUPCMP(NCMP,2)=0.0
        noinfforcurrent=NCMP

C A nothing happening ventilation fixd volume component. (with
C zero flow), remember index via noventforcurrent.
        NCMP=NCMP+1
        CMNAM(NCMP)='ventf_.0000'
        ITPCMP(NCMP)=IVALCM(7)
        LTPCMP(NCMP)=LVALCM(7)
        ISDCMP(NCMP)=2
        ISDCNN(NCMP)=0
        SUPCMP(NCMP,1)=1.0
        SUPCMP(NCMP,2)=0.0
        noventforcurrent=NCMP

C A nothing happening crack 5mm x 1m (so zones with no infiltration
C or ventilation are not perfectly closed), remember index via crackforcurrent.
        NCMP=NCMP+1
        CMNAM(NCMP)='crack_5mm1m'
        ITPCMP(NCMP)=IVALCM(12)
        LTPCMP(NCMP)=LVALCM(12)
        ISDCMP(NCMP)=3
        ISDCNN(NCMP)=0
        SUPCMP(NCMP,1)=1.0
        SUPCMP(NCMP,2)=0.005
        SUPCMP(NCMP,3)=1.000
        crackforcurrent=NCMP

C A small exfiltration grill 0.2m^2, remember index via gr02forcurrent
        NCMP=NCMP+1
        CMNAM(NCMP)='grill_0.2'
        ITPCMP(NCMP)=IVALCM(9)
        LTPCMP(NCMP)=LVALCM(9)
        ISDCMP(NCMP)=3
        ISDCNN(NCMP)=0
        SUPCMP(NCMP,1)=1.0
        SUPCMP(NCMP,2)=0.2
        SUPCMP(NCMP,3)=0.63
        gr02forcurrent=NCMP

C An exfiltration grill 0.4m^2, remember index via gr04forcurrent
        NCMP=NCMP+1
        CMNAM(NCMP)='grill_0.4'
        ITPCMP(NCMP)=IVALCM(9)
        LTPCMP(NCMP)=LVALCM(9)
        ISDCMP(NCMP)=3
        ISDCNN(NCMP)=0
        SUPCMP(NCMP,1)=1.0
        SUPCMP(NCMP,2)=0.4
        SUPCMP(NCMP,3)=0.63
        gr04forcurrent=NCMP

C An exfiltration grill 0.6m^2, remember index via gr06forcurrent
        NCMP=NCMP+1
        CMNAM(NCMP)='grill_0.6'
        ITPCMP(NCMP)=IVALCM(9)
        LTPCMP(NCMP)=LVALCM(9)
        ISDCMP(NCMP)=3
        ISDCNN(NCMP)=0
        SUPCMP(NCMP,1)=1.0
        SUPCMP(NCMP,2)=0.6
        SUPCMP(NCMP,3)=0.63
        gr06forcurrent=NCMP

C Loop through each of the selected zones, scan operations file
C if it exists, if not put in standard crack connection. IZ is
C the index of the zone being considered.
        CALL edisp(iuout,
     &    'Generating infiltration and ventilation components...')
        DO 38 IZT=1,IZN
          IZ=IVALS(IZT)
          IUO=IFIL+1
          call FINDFIL(LPROJ(IZ),XST)
          IF(XST)THEN
            CALL ERPFREE(IUO,ISTAT)
            CALL EROPER(ITRC,ITRU,IUO,IZ,IER)

C Increment nodeforcurrent counter (i.e. the node index that is
C used with the current zone index.
            nodeforcurrent=nodeforcurrent+1
            IGN=0
            DO 1435 IDTY=1,NBDAYTYPE
              IF(NAC(IDTY).EQ.0.AND.IGN.EQ.0)THEN
                IGN=0
              ELSE
                IGN=1
              ENDIF
 1435       CONTINUE
            IF(IGN.EQ.0)THEN

C There are no flow periods on any day, write crack connections only.
              WRITE(OUTS,'(3A)')
     &          'No operation ventilation or infiltration so zone: ',
     &          ZNAME(IZ),' has cracks only.'
              call edisp(iuout,outs)

C Add the source -> crack -> zone connection.
              NCNN=NCNN+1
              NODPS(NCNN)=1
              NODNE(NCNN)=nodeforcurrent
              HGTPS(NCNN)=0.0
              HGTNE(NCNN)=0.0      
              ITPCON(NCNN)=crackforcurrent
              NDSCNN(NCNN,1)=0
              NDSCNN(NCNN,2)=0                 
            else

C Loop through periods for each day type and find
C the maximum infiltration rate (finfmax) and ventilation rate (fvntmax)
C Note works if ventilation is always from only one other zone.
              DO 402 IDTY=1,NBDAYTYPE
                DO 39 L=1,NAC(IDTY)
                  acim = (VOL(IZ)*ACI(IDTY,L))/3600.0
                  acvm = (VOL(IZ)*ACV(IDTY,L))/3600.0
                  if(acim.gt.finfmax(iz)) finfmax(iz)=acim
                  if(acvm.gt.fvntmax(iz))then
                    fvntmax(iz)=acvm
                    isrczforvent(iz)=IPT(IDTY,L)
                  endif
  39            continue
 402          CONTINUE

C Debug.
C              write(6,*) 'iz finfmax fvntmax isrczforvent',iz,
C     &          finfmax(iz),fvntmax(iz),isrczforvent(iz)

              call ECLOSE(finfmax(iz),0.0,0.0001,closeiz)
              call ECLOSE(fvntmax(iz),0.0,0.0001,closevz)
              if(closeiz.and.closevz)then

C Logic:
C If infiltration zero and vent zero then do cracks only
C add the source -> crack -> zone connection now).
                NCNN=NCNN+1
                NODPS(NCNN)=1
                NODNE(NCNN)=nodeforcurrent
                HGTPS(NCNN)=0.0
                HGTNE(NCNN)=0.0      
                ITPCON(NCNN)=crackforcurrent
                NDSCNN(NCNN,1)=0
                NDSCNN(NCNN,2)=0 
                goto 38 
              endif
               
              if(.NOT.closeiz)then

C If we have infiltration....
                if(izt.ge.2)then

C If infiltration non-zero check to see if any other zone
C has the same infiltration rate (re-use its component). Loop
C through all of the previous zones values to do this.
                  foundit=.false.
                  iztj=izt-1
                  do 42 ijj = 1,iztj
                    call ECLOSE(finfmax(iz),finfmax(IVALS(ijj)),
     &                0.0001,close)
                    if(close)then
                      foundit=.true.
                      itis=IVALS(ijj)
                    endif
 42               continue
                  if(foundit)then

C Add that connection now with component (pointed to by icompforinf).
                    NCNN=NCNN+1
                    NODPS(NCNN)=1
                    NODNE(NCNN)=nodeforcurrent
                    HGTPS(NCNN)=0.0
                    HGTNE(NCNN)=0.0      
                    ITPCON(NCNN)=icompforinf(itis)
                    NDSCNN(NCNN,1)=0
                    NDSCNN(NCNN,2)=0                 
                  else

C If infiltraiton is non-zero and unique make a new component
                    NCMP=NCMP+1
                    write(CMNAM(NCMP),'(a,F5.4)') 'inflf_',finfmax(iz)
                    ITPCMP(NCMP)=IVALCM(7)
                    LTPCMP(NCMP)=LVALCM(7)
                    ISDCMP(NCMP)=2
                    ISDCNN(NCMP)=0
                    SUPCMP(NCMP,1)=1.0
                    SUPCMP(NCMP,2)=finfmax(iz)

C Remember link between current iz and infiltration component.
C Given a zone index (ix) it will return the component number
C used in that zone.
                    icompforinf(iz)=NCMP

C Add connection using the new component (remember component index in ITPCON).
                    NCNN=NCNN+1
                    NODPS(NCNN)=1
                    NODNE(NCNN)=nodeforcurrent
                    HGTPS(NCNN)=0.0
                    HGTNE(NCNN)=0.0      
                    ITPCON(NCNN)=icompforinf(iz)
                    NDSCNN(NCNN,1)=0
                    NDSCNN(NCNN,2)=0                 
                  endif

C Figure out which exfiltration grill to use for exfiltration connection.
                  NCNN=NCNN+1
                  NODPS(NCNN)=2
                  NODNE(NCNN)=nodeforcurrent
                  HGTPS(NCNN)=0.0
                  HGTNE(NCNN)=0.0      
                  NDSCNN(NCNN,1)=0
                  NDSCNN(NCNN,2)=0                 
                  if(finfmax(iz).gt.0.0.and.finfmax(iz).le.0.2)then
                    ITPCON(NCNN)=gr02forcurrent
                  elseif(finfmax(iz).gt.0.2.and.finfmax(iz).le.0.8)then
                    ITPCON(NCNN)=gr04forcurrent
                  elseif(finfmax(iz).gt.0.8)then
                    ITPCON(NCNN)=gr06forcurrent
                  endif
                else

C We have one of the initial zones so just generate new component
C and connection for infiltration and exfiltration.
                  NCMP=NCMP+1
                  write(CMNAM(NCMP),'(a,F5.4)') 'inflf_',finfmax(iz)
                  ITPCMP(NCMP)=IVALCM(7)
                  LTPCMP(NCMP)=LVALCM(7)
                  ISDCMP(NCMP)=2
                  ISDCNN(NCMP)=0
                  SUPCMP(NCMP,1)=1.0
                  SUPCMP(NCMP,2)=finfmax(iz)
                  icompforinf(iz)=NCMP
                  NCNN=NCNN+1
                  NODPS(NCNN)=1
                  NODNE(NCNN)=nodeforcurrent
                  HGTPS(NCNN)=0.0
                  HGTNE(NCNN)=0.0      
                  ITPCON(NCNN)=icompforinf(iz)
                  NDSCNN(NCNN,1)=0
                  NDSCNN(NCNN,2)=0                 
                  NCNN=NCNN+1
                  NODPS(NCNN)=2
                  NODNE(NCNN)=nodeforcurrent
                  HGTPS(NCNN)=0.0
                  HGTNE(NCNN)=0.0      
                  NDSCNN(NCNN,1)=0
                  NDSCNN(NCNN,2)=0                 
                  if(finfmax(iz).gt.0.0.and.finfmax(iz).le.0.2)then
                    ITPCON(NCNN)=gr02forcurrent
                  elseif(finfmax(iz).gt.0.2.and.finfmax(iz).le.0.8)then
                    ITPCON(NCNN)=gr04forcurrent
                  elseif(finfmax(iz).gt.0.8)then
                    ITPCON(NCNN)=gr06forcurrent
                  endif
                endif
              endif

              if(.NOT.closevz)then

C If ventilation is non-zero then create the flow component as
C well as the connection from the other zone.
                if(izt.ge.2)then

C Check to see if there is a prior matching ventilation component
C and remember the zone it was associated with in 'itis'.
                  foundit=.false.
                  iztj=izt-1
                  do 43 ijj = 1,iztj
                    call ECLOSE(fvntmax(iz),fvntmax(IVALS(ijj)),
     &                0.0001,close)
                    if(close)then
                      foundit=.true.
                      itis=IVALS(ijj)
                    endif
 43               continue
                  if(foundit)then

C Add that connection now with the component (pointed by icompforvent).
C Note: isrczforvent could be a zero signaling that TA? holds
C a fixed temperature (which could vary from period to period. Warn
C the user because it implies creating a ventilation source thermal
C zone which controls applied to it. Temporarily assign the source
C 'inf_source' node if isrczforvent(itis) is zero
                    NCNN=NCNN+1
                    if(isrczforvent(itis).eq.0)then
                      call edisp(iuout,
     &                  'Zone ventilation to a fixed temperature is')
                      call edisp(iuout,
     &                  'not yet implemented. Using inf_source.')
                      NODPS(NCNN)=1
                    else
C Debug.
C                      write(6,*) 'a icfv isfv itis ',icompforvent(itis),
C     & isrczforvent(itis),iz,icaas(isrczforvent(itis)),isrczforvent(iz),
C     & icaas(isrczforvent(iz))

                      NODPS(NCNN)=icaas(isrczforvent(iz))
                    endif
                    NODNE(NCNN)=nodeforcurrent
                    HGTPS(NCNN)=0.0
                    HGTNE(NCNN)=0.0      
                    ITPCON(NCNN)=icompforvent(itis)
                    NDSCNN(NCNN,1)=0
                    NDSCNN(NCNN,2)=0                 
                  else

C Make a new ventilation component.
                    NCMP=NCMP+1
                    write(CMNAM(NCMP),'(a,F5.4)') 'ventf_',fvntmax(iz)
                    ITPCMP(NCMP)=IVALCM(7)
                    LTPCMP(NCMP)=LVALCM(7)
                    ISDCMP(NCMP)=2
                    ISDCNN(NCMP)=0
                    SUPCMP(NCMP,1)=1.0
                    SUPCMP(NCMP,2)=fvntmax(iz)

C Remember link between current iz and ventilation component.
C Given a zone index (ix) it will return the component number
C used in that zone.
                    icompforvent(iz)=NCMP

C Add connection using the new component (remember component index in ITPCON).
                    NCNN=NCNN+1
                    if(isrczforvent(iz).eq.0)then
                      call edisp(iuout,
     &                  'Zone ventilation to a fixed temperature is')
                      call edisp(iuout,
     &                  'not yet implemented. Using inf_source .')
                      NODPS(NCNN)=1
                    else

C Use isrczforvent to get the index of the zone where ventilation is
C coming from and then use this as an index for icompforvent to get
C the actual component index.

C Debug.
C                      write(6,*) 'b icfv isfv iz ',icompforvent(iz),
C     &                  isrczforvent(iz),iz,icaas(iz),
C     &                  icaas(isrczforvent(iz))

                      NODPS(NCNN)=icaas(isrczforvent(iz))
                    endif
                    NODNE(NCNN)=nodeforcurrent
                    HGTPS(NCNN)=0.0
                    HGTNE(NCNN)=0.0      
                    ITPCON(NCNN)=icompforvent(iz)
                    NDSCNN(NCNN,1)=0
                    NDSCNN(NCNN,2)=0                 
                  endif
                else

C We have one of the initial zones so just generate new component
C and connection for ventilation.
                  NCMP=NCMP+1
                  write(CMNAM(NCMP),'(a,F5.4)') 'ventf_',fvntmax(iz)
                  ITPCMP(NCMP)=IVALCM(7)
                  LTPCMP(NCMP)=LVALCM(7)
                  ISDCMP(NCMP)=2
                  ISDCNN(NCMP)=0
                  SUPCMP(NCMP,1)=1.0
                  SUPCMP(NCMP,2)=fvntmax(iz)
                  icompforvent(iz)=NCMP
                  NCNN=NCNN+1
                  if(isrczforvent(iz).eq.0)then
                    call edisp(iuout,
     &                'Zone ventilation is to a fixed temperature')
                    call edisp(iuout,
     &                'which is not yet implemented. Using inf_source.')
                    NODPS(NCNN)=1
                  else

C Debug.
C                      write(6,*) 'c icfv isfv iz ',icompforvent(iz),
C     &                  isrczforvent(iz),iz,icaas(iz),
C     &                  icaas(isrczforvent(iz))

                    NODPS(NCNN)=icaas(isrczforvent(iz))
                  endif
                  NODNE(NCNN)=nodeforcurrent
                  HGTPS(NCNN)=0.0
                  HGTNE(NCNN)=0.0      
                  ITPCON(NCNN)=icompforvent(iz)
                  NDSCNN(NCNN,1)=0
                  NDSCNN(NCNN,2)=0                 
                endif

                if(closeiz)then

C If ventilation but no infiltration figure out which exfiltration 
C grill to use and write that connection out.
                  NCNN=NCNN+1
                  NODPS(NCNN)=2
                  NODNE(NCNN)=nodeforcurrent
                  HGTPS(NCNN)=0.0
                  HGTNE(NCNN)=0.0      
                  NDSCNN(NCNN,1)=0
                  NDSCNN(NCNN,2)=0                 
                  if(finfmax(iz).gt.0.0.and.finfmax(iz).le.0.2)then
                    ITPCON(NCNN)=gr02forcurrent
                  elseif(finfmax(iz).gt.0.2.and.finfmax(iz).le.0.8)then
                    ITPCON(NCNN)=gr04forcurrent
                  elseif(finfmax(iz).gt.0.8)then
                    ITPCON(NCNN)=gr06forcurrent
                  endif
                endif
              endif
            endif
          ELSE

C There is no operations file so write crack connections only.
            WRITE(OUTS,'(3A)')
     &        'No schedules so making zone: ',ZNAME(IZ),
     &        ' with cracks only.'
            call edisp(iuout,outs)
            NCNN=NCNN+1
            NODPS(NCNN)=1
            NODNE(NCNN)=nodeforcurrent
            HGTPS(NCNN)=0.0
            HGTNE(NCNN)=0.0      
            ITPCON(NCNN)=crackforcurrent
            NDSCNN(NCNN,1)=0
            NDSCNN(NCNN,2)=0                 
          ENDIF
  38    CONTINUE
      ENDIF

C Scan for boundary node connectivity.
              
C Auto generate plant flow network. 
      RETURN
      END
