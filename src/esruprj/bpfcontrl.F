C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C Configuration control file definition.
C This file contains the following subroutines:
C CONTRL : Principal interfacing subroutine that builds menus and
C          governs user interaction
C TOTCTLPER : Finds current number of periods in all control loops
C             of type icfoc.
C EBCNTRL : Edit building control function
C EPCNTRL : Edit plant control function
C EFCNTRL : Edit flow control function
C EGCNTRL : Edit global control function
C SENLOC : Returns sensor location depending upon control domain
C ACTLOC : Returns actuator location depending upon control domain
c ASKMFCTM : Asks for one or more contaminants
C Editctlperiod: generic menu for editing details of a control
C                period for a zone control (later flow control).
C STFMENU: Copies menu and prompts for period editing.  It expects the common
C          block sctl to have been filled for the current period.

      SUBROUTINE CONTRL(ITRC,ITRU,icfoc,IER)
#include "building.h"
#include "plant.h"
#include "epara.h"
#include "net_flow.h"
#include "control.h"
#include "power.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

      common/c1/ncomp,ncon
      common/C21/IFCFG,cfgroot,LCFGF
      common/cctl/icascf(mcom)
      common/cctlnm/ctldoc,lctlf
      common/ctlnm/znctldoc,plctldoc,flctldoc,elctldoc,glctldoc,
     &             opticdoc
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)

      common/pctl/ncl,ipsn(mcl,5),ipan(mcl,4),npcdt(mcl),
     &       ipcdv(mcl,mpcdt,2),npcdp(mcl,mpcdt),tpcps(mcl,mpcdt,mpcdp),
     &       ipctyp(mcl,mpcdt,mpcdp),ipclaw(mcl,mpcdt,mpcdp),
     &       pmiscd(mcl,mpcdt,mpcdp,misc)

      common/fctl4/iasocc(mcc,mcmp),nfsup(mcc)
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)

      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)

C Electrical control.
      COMMON/ECTL/NECL,IESN(MEL,3),IEAN(MEL,3),NECDT(MEL),
     &            IECDV(MEL,MECDT,2),NECDP(MEL,MECDT),
     &            TECPS(MEL,MECDT,MECDP),IECTYP(MEL,MECDT,MECDP),
     &            IECLAW(MEL,MECDT,MECDP),EMISCD(MEL,MECDT,MECDP,MISC)

C Optical controls
      common/octl/nof,iosn(mol,4),ioan(mol,3),nocdt(mol),
     &       iocdv(mol,mocdt,2),nocdp(mol,mocdt),tocps(mol,mocdt,mocdp),
     &       ioclaw(mol,mocdt,mocdp),omiscd(mol,mocdt,mocdp,misco)

      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      common/user/browse
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C mptback is pointer from position in display list to the control loop.
C ipeerptbk is pointer from position in display to day type.
      dimension mptback(35),iperptbk(35)
      CHARACTER*43 VERT(35)
      CHARACTER CTLDOC*248,LCTLF*72,KEY*1,LCFGF*72
      CHARACTER outs*124,H*72,LASCI*72,zname*12,cfgroot*24
      character zdesc*64
      character znctldoc*248,plctldoc*248,flctldoc*248,elctldoc*248
      character glctldoc*248
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,t248*248,SSTR*72
      character sfile*72,snpfile*72,fs*1
      logical LINKED,OK,dok,XST,browse,unixok

C Zeroize all control data.
      CALL EZCTLI
      CTLDOC  ='no overall control description supplied'
      znctldoc='no zone control description supplied'
      plctldoc='no plant control description supplied'
      flctldoc='no flow control description supplied'
      glctldoc='no global control description supplied'

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Attempt to read in the control file. If no current name then suggest
C one. If an existing name also provide option to deference it.
 44   if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
        if(browse)then
          call usrmsg('No control associated with this model,',
     &                'returning with no change.','W')
          return
        endif
        LN=max(1,LNBLNK(cfgroot))
        if(ctlpth(1:2).eq.'  '.or.ctlpth(1:2).eq.'./')then
          WRITE(LCTLF,'(2a)')cfgroot(1:ln),'.ctl'
        else
          WRITE(LCTLF,'(4a)') ctlpth(1:lnblnk(ctlpth)),fs,
     &         cfgroot(1:ln),'.ctl'
        endif

C Present current/default name and a browse option. If browse selected
C then set iclkok variable and jump to browse functions.
        LASCI=LCTLF
        H(1)='The system control file holds the definition of the'
        H(2)='building/plant control systems. The file is optional.'
        H(3)='The browse option presents a list of files in the'
        H(4)='project `ctl` folder.'
        CALL EASKS2CMD(LASCI,' ','Control file?',
     &    'ignore','browse',iclkok,72,' ','Control file',IER,4)
        if(iclkok.eq.1)then
          return
        endif
      else
        LASCI=LCTLF
        if(browse)then
          ICTLF=IFIL+1
          CALL ERPFREE(ICTLF,ISTAT)
          call FINDFIL(LCTLF,XST)
          if(XST)then
C            call edisp(iuout,'Reading existing control file.')
            CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
            goto 54
          else
            call usrmsg('Control file not found. You are browsing',
     &        'so returning without change to the model.','W')
            return
          endif
        endif
        H(1)='The system control file holds the definition of the'
        H(2)='building/ plant control systems. The file is optional.'
        H(3)='The dereference option dereferences the  control file'
        H(4)='from your model but does not delete it. Your may'
        H(5)='re-establish this control by re-referencing the control'
        H(6)='file in a later session.'
        CALL EASKS2CMD(LASCI,' Control file? (see help)',
     &  ' ','dereference','browse',iclkok,72,' ','Control file',IER,6)
      endif
      call usrmsg(' ',' ','-')

C If user wishes to deselect current control reset file name to
C blank and save the configuration file.
      if(iclkok.eq.1)then
        if(cfgok.and.(.NOT.browse))then
          LCTLF='  '
          CALL EMKCFG('-',IER)
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Cannot save control data while in browse',
     &                'mode, you must first `own` the model.','W')
          goto 92
        endif
        return
      elseif(iclkok.eq.2)then
        call edisp(iuout,'2nd alternate selected')
        sfile=' '
        snpfile=' '
        call edisp(iuout,' ')
        call browsefilelist('?','ctl','fil',sfile,snpfile,nfile,iier)
        if(nfile.gt.0)then
          sfile=' '
          snpfile=' '
          call browsefilelist('b','ctl','fil',sfile,snpfile,nfile,
     &        iier)
          if(snpfile(1:2).ne.'  ')then
            write(LASCI,'(3a)')ctlpth(1:lnblnk(ctlpth)),fs,
     &        snpfile(1:lnblnk(snpfile))
          else
            LASCI=LCTLF
          endif
        else
          LASCI=LCTLF
        endif
      endif
      IF(LASCI(1:2).NE.'  ')LCTLF=LASCI
      ICTLF=IFIL+1
      CALL ERPFREE(ICTLF,ISTAT)
      call FINDFIL(LCTLF,XST)
      IF(XST)THEN
        CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
      else
        CALL EASKAB(' File not found.','Options:',
     &    'specify another','make new file',IW,0)
        if(IW.eq.1)goto 44

C Set defaults and ask user which control to focus on first.
        LINKED=.FALSE.
        call ADDCNTL(icfoc,'A',IER)
      endif

C Setup for multi-page menu.
  54  MHEAD=8
      MCTL=6
      call totctlper(icfoc,ncper)
      ILEN=ncper
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup, rescan for length of the list.
   92 IER=0
      call totctlper(icfoc,ncper)
      ILEN=ncper
      IVERT=-3
      write(VERT(2),'(A,A27)')   'b description: ',ctldoc(1:27)
      if(icfoc.eq.0)then
        write(VERT(1),'(A)')     'a control focus >> zones'
        write(VERT(3),'(A,A27)') 'c description: ',znctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',ncf
        VERT(5)=                 'd link loops to zones'
      elseif(icfoc.eq.1)then
        write(VERT(1),'(A)')     'a control focus >> plant'
        write(VERT(3),'(A,A27)') 'c description: ',plctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',ncl
        VERT(5)=                 ' '
      elseif(icfoc.eq.2)then
        write(VERT(1),'(A)')     'a control focus >> vent/hydronic'
        write(VERT(3),'(A,A27)') 'c description: ',flctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',ncc
        VERT(5)=                 ' '
      elseif(icfoc.eq.3)then
        write(VERT(1),'(A)')     'a control focus >> global'
        write(VERT(3),'(A,A27)') 'c description: ',glctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',ngf
        VERT(5)=                 '                                    '
      elseif(icfoc.eq.4)then
        write(VERT(1),'(A)')     'a control focus >> power'
        write(VERT(3),'(A,A27)') 'c description: ',elctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',necl
        VERT(5)=                 '                                    '
      endif
      VERT(6)= ' _________________________________________'
      VERT(7)= ' cntl| sensor |actuator|day |valid |period'
      VERT(8)= ' loop|location|location|type|during|in day'
      m=MHEAD

C Keep track of how many items will be in the variable length section
C of the menu.
      listct=0
      if(icfoc.eq.0.and.ncf.gt.0)then
        do 88 II=1,ncf
          NN=nbcdt(ii)
          if(nbcdt(ii).eq.0)NN=3
          do 87 IJ=1,NN
            listct=listct+1
            if(listct.GE.IST.AND.(listct.LE.(IST+MIFULL)))then
              m=m+1
              mptback(m)=II
              CALL EMKEY(M-MHEAD+4,KEY,IER)
              if(nbcdt(ii).eq.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,a,3I4)') KEY,II,
     &            IBSN(II,1),IBSN(II,2),IBSN(II,3),IBAN(II,1),
     &            IBAN(II,2),IBAN(II,3),' wkd',ibcdv(II,IJ,1),
     &            ibcdv(II,IJ,2),nbcdp(II,1)
              elseif(nbcdt(ii).eq.0.and.IJ.eq.2)then
                iperptbk(m)=2
                WRITE(VERT(M),'(A1,22X,a,3I4)') KEY,
     &            ' Sat',ibcdv(II,2,1),ibcdv(II,2,2),nbcdp(II,2)
              elseif(nbcdt(ii).eq.0.and.IJ.eq.3)then
                iperptbk(m)=3
                WRITE(VERT(M),'(A1,22X,a,3I4)') KEY,
     &            ' Sun',ibcdv(II,3,1),ibcdv(II,3,2),nbcdp(II,3)
              endif
              if(nbcdt(ii).ne.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,I4,3I4)')KEY,II,
     &            IBSN(II,1),IBSN(II,2),IBSN(II,3),IBAN(II,1),
     &            IBAN(II,2),IBAN(II,3),IJ,ibcdv(II,IJ,1),
     &            ibcdv(II,IJ,2),nbcdp(II,IJ)
              elseif(nbcdt(ii).ne.0.and.IJ.gt.1)then
                iperptbk(m)=IJ
                WRITE(VERT(M),'(A1,22X,I4,3I4)')KEY,
     &            IJ,ibcdv(II,IJ,1),ibcdv(II,IJ,2),nbcdp(II,IJ)
              endif
            endif
  87      continue
  88    continue
      elseif(icfoc.eq.1.and.ncl.gt.0)then
        do 188 II=1,ncl
          NN=NPCDT(II)
          if(NPCDT(II).eq.0)NN=3
          do 187 IJ=1,NN
            listct=listct+1
            if(listct.GE.IST.AND.(listct.LE.(IST+MIFULL)))then
              m=m+1
              mptback(m)=II
              CALL EMKEY(M-MHEAD+4,KEY,IER)
              if(npcdt(ii).eq.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,a,3I4)') KEY,II,
     &            IPSN(II,1),IPSN(II,2),IPSN(II,3),IPAN(II,1),
     &            IPAN(II,2),IPAN(II,3),' wkd',ipcdv(II,IJ,1),
     &            ipcdv(II,IJ,2),npcdp(II,1)
              elseif(npcdt(ii).eq.0.and.IJ.eq.2)then
                iperptbk(m)=2
                WRITE(VERT(M),'(A1,22X,a,3I4)') KEY,
     &            ' Sat',ipcdv(II,2,1),ipcdv(II,2,2),npcdp(II,2)
              elseif(npcdt(ii).eq.0.and.IJ.eq.3)then
                iperptbk(m)=3
                WRITE(VERT(M),'(A1,22X,a,3I4)') KEY,
     &            ' Sun',ipcdv(II,3,1),ipcdv(II,3,2),npcdp(II,3)
              endif
              if(npcdt(ii).ne.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,I4,3I4)')KEY,II,
     &            IPSN(II,1),IPSN(II,2),IPSN(II,3),IPAN(II,1),
     &            IPAN(II,2),IPAN(II,3),IJ,ipcdv(II,IJ,1),
     &            ipcdv(II,IJ,2),npcdp(II,IJ)
              elseif(npcdt(ii).ne.0.and.IJ.gt.1)then
                iperptbk(m)=IJ
                WRITE(VERT(M),'(A1,22X,I4,3I4)')KEY,
     &            IJ,ipcdv(II,IJ,1),ipcdv(II,IJ,2),npcdp(II,IJ)
              endif
            endif
 187      continue
 188    continue
      elseif(icfoc.eq.2.and.ncc.gt.0)then
        do 288 II=1,ncc
          NN=NFCDT(II)
          if(NFCDT(II).eq.0)NN=3
          do 287 IJ=1,NN
            listct=listct+1
            if(listct.GE.IST.AND.(listct.LE.(IST+MIFULL)))then
              m=m+1
              mptback(m)=II
              CALL EMKEY(M-MHEAD+4,KEY,IER)
              if(nfcdt(ii).eq.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,a,3I4)') KEY,II,
     &            IFSN(II,1),IFSN(II,2),IFSN(II,3),IFAN(II,1),
     &            IFAN(II,2),IFAN(II,3),' wkd',ifcdv(II,IJ,1),
     &            ifcdv(II,IJ,2),nfcdp(II,1)
              elseif(nfcdt(ii).eq.0.and.IJ.eq.2)then
                iperptbk(m)=2
                WRITE(VERT(M),'(A1,22X,a,3I4)') KEY,
     &            ' Sat',ifcdv(II,2,1),ifcdv(II,2,2),nfcdp(II,2)
              elseif(nfcdt(ii).eq.0.and.IJ.eq.3)then
                iperptbk(m)=3
                WRITE(VERT(M),'(A1,22X,a,3I4)') KEY,
     &            ' Sun',ifcdv(II,3,1),ifcdv(II,3,2),nfcdp(II,3)
              endif
              if(nfcdt(ii).ne.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,I4,3I4)')KEY,II,
     &            IFSN(II,1),IFSN(II,2),IFSN(II,3),IFAN(II,1),
     &            IFAN(II,2),IFAN(II,3),IJ,ifcdv(II,IJ,1),
     &            ifcdv(II,IJ,2),nfcdp(II,IJ)
              elseif(nfcdt(ii).ne.0.and.IJ.gt.1)then
                iperptbk(m)=IJ
                WRITE(VERT(M),'(A1,22X,I4,3I4)')KEY,
     &            IJ,ifcdv(II,IJ,1),ifcdv(II,IJ,2),nfcdp(II,IJ)
              endif
            endif
 287      continue
 288    continue
      elseif(icfoc.eq.3.and.ngf.gt.0)then
        do 388 II=1,ngf
          NN=NGCDT(II)
          if(NGCDT(II).eq.0)NN=3
          do 387 IJ=1,NN
            listct=listct+1
            if(listct.GE.IST.AND.(listct.LE.(IST+MIFULL)))then
              m=m+1
              mptback(m)=II
              CALL EMKEY(M-MHEAD+4,KEY,IER)
              if(ngcdt(ii).eq.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,a,3I4)') KEY,II,
     &            IGSN(II,1),IGSN(II,2),IGSN(II,3),IGAN(II,1),
     &            IGAN(II,2),IGAN(II,3),' wkd',igcdv(II,IJ,1),
     &            igcdv(II,IJ,2),ngcdp(II,1)
              elseif(ngcdt(ii).eq.0.and.IJ.eq.2)then
                iperptbk(m)=2
                WRITE(VERT(M),'(A1,22X,a,3I4)') KEY,
     &            ' Sat',igcdv(II,2,1),igcdv(II,2,2),ngcdp(II,2)
              elseif(ngcdt(ii).eq.0.and.IJ.eq.3)then
                iperptbk(m)=3
                WRITE(VERT(M),'(A1,22X,a,3I4)') KEY,
     &            ' Sun',igcdv(II,3,1),igcdv(II,3,2),ngcdp(II,3)
              endif
              if(ngcdt(ii).ne.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,I4,3I4)')KEY,II,
     &            IGSN(II,1),IGSN(II,2),IGSN(II,3),IGAN(II,1),
     &            IGAN(II,2),IGAN(II,3),IJ,igcdv(II,IJ,1),
     &            igcdv(II,IJ,2),ngcdp(II,IJ)
              elseif(ngcdt(ii).ne.0.and.IJ.gt.1)then
                iperptbk(m)=IJ
                WRITE(VERT(M),'(A1,22X,I4,3I4)')KEY,
     &            IJ,igcdv(II,IJ,1),igcdv(II,IJ,2),ngcdp(II,IJ)
              endif
            endif
 387      continue
 388    continue
      elseif(icfoc.eq.4.and.necl.gt.0)then

C Power controls.
      endif

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list, include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________________________________'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page ------- Part: ',I2,' of ',I2,
     &    ' -----------')
      ENDIF
      VERT(M+2)  ='+ add/delete/copy control loop or day type'
      VERT(M+3)  ='! list or check current control data      '
      VERT(M+4)  ='> save control data                       '
      VERT(M+5)  ='? help                                    '
      VERT(M+6)  ='- exit this menu                          '

C Help text for this menu.
      H(1) ='In order to provide realistic controls for zones,'
      H(2) ='plant systems and flow networks, a configuration'
      H(3) ='control file must be created. If this is not done'
      H(4) ='then the simulation will free float.'
      H(5) =' '
      H(6) ='Toggle the control focus to switch between controlled'
      H(7) ='entities. Select a control definition to edit it.'
      H(8) =' '
      H(9) ='The required data  are as follows.'
      H(10)=' + control system title; then for each sub-system'
      H(11)='   (zones, plant, flow & global):'
      H(12)='     + title & number of control loops; then for'
      H(13)='       each loop:'
      H(14)='         + sensor & actuator location, number of'
      H(15)='           day types; then for each day type:'
      H(16)='             + start & finish dates of validity,'
      H(17)='               number of control periods; then for'
      H(18)='               each period:'
      H(19)='                + controller type, control law,'
      H(20)='                  start time, number of data items'
      H(21)='                  associated with control law and'
      H(22)='                  value of each data item.'
      H(23)=' '
      H(24)='To edit a control loop select it.'

C Now display the menu.
      CALL EMENU('Controls',VERT,MVERT,IVERT)

      IF(IVERT.EQ.1)THEN

C Toggle focus
        icfoc=icfoc+1
        if(icfoc.gt.4)icfoc=0
      ELSEIF(IVERT.EQ.2)THEN

C Edit project level description.
        H(1)='The overall control system described (<64 characters)' 
        H(2)='is used in reports.'
        t248=ctldoc
        CALL EASKS248(t248,' ','Control system description?',
     &    72,'Base case control','overall control desc',IER,2)
        if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')ctldoc=t248
        IF(ctldoc(1:5).EQ.'NONE ')GOTO 92
      ELSEIF(IVERT.EQ.3)THEN

C Edit building/plant/flow/global control description.
        H(1)='Description of control system (<248 characters).'
        if(icfoc.eq.0)then
          t248=znctldoc
          CALL EASKS248(t248,' ','Zone control description?',
     &      72,'Zone control','building control descr.',IER,1)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')znctldoc=t248
        elseif(icfoc.eq.1)then
          t248=plctldoc
          CALL EASKS248(t248,' ','Plant control description?',
     &      72,'Plant control','plant control descr.',IER,1)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')plctldoc=t248
        elseif(icfoc.eq.2)then
          t248=flctldoc
          CALL EASKS248(t248,' ','Flow control description?',
     &      72,'Flow control','flow control descr.',IER,1)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')flctldoc=t248
        elseif(icfoc.eq.3)then
          t248=glctldoc
          CALL EASKS248(t248,' ','Global control description?',
     &      72,'Global control','global control descr.',IER,1)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')glctldoc=t248
        elseif(icfoc.eq.4)then
          t248=elctldoc
          CALL EASKS248(t248,' ','Electrical control description?',
     &      72,'Electrical control','electrical control descr.',IER,1)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')elctldoc=t248
        endif
      ELSEIF(IVERT.EQ.5)THEN

c Relate control functions to the appropriate zones
c from the configuration file.
        if(icfoc.eq.0)then
          NZT=NCOMP
          H(1)=
     &      'This should match the current system configuration file.'
          CALL EASKI(NZT,' ','Number of zones in model?',
     &      1,'F',MCOM,'F',NCOMP,'no of zones',IERI,1)
          if(ieri.eq.-3) goto 92
          NZ=NZT
          IF(NZ.GT.1.OR.NCF.GT.1)GOTO 304

c Match single zone with single control function.
          ICASCF(1)=1
          LINKED=.TRUE.
          GOTO 92

c Relate control function to associated zone.
c Remember zero implies no control.
  304     IF(NZ.EQ.1)then
            H(1)='This is the control index: 0 means no control,'
            H(2)='1 is the first defined control function, 2 the'
            H(3)='second and so on.'
            ica=ICASCF(1)
            CALL EASKI(ICA,'Associated control function',
     &               'for zone 1?',0,'F',MCOM,'F',0,
     &               'associated control',IERI,3)
            if(ieri.eq.-3) goto 92
            ICASCF(1)=ica
          else
            do 311 IJ=1,NZ
              write(outs,'(3A,I2,A)')'for ',zname(IJ),'(',IJ,')?'
              ica=ICASCF(IJ)
              H(1)='This is the control index: 0 means no control,'
              H(2)='1 is the first defined control function, 2 the'
              H(3)='second and so on.'
              CALL EASKI(ICA,'Associated control function',outs,
     &                 0,'F',MCOM,'F',0,
     &                 'associated control',IER,3)
              if(ieri.eq.-3)then
                goto 311
              else
                ICASCF(IJ)=ica
              endif
  311       continue
          endif

c Check for out-of-allowable range.
          IF(NZ.gt.0)then
           DO 300 I=1,NZ
            IF(ICASCF(I).LT.0.OR.ICASCF(I).GT.NCF)then
             write(outs,'(3A,I2,A)')' Zone ',zname(IJ),'(',I,
     &         ') refers to a non-existent'
             call usrmsg(outs,'control function. Respecify.','W')
             goto 304
            endif
  300      CONTINUE
          endif

C Flag that links made.
          LINKED=.TRUE.
        elseif(icfoc.eq.1.or.icfoc.eq.2.or.icfoc.eq.3.or.
     &         icfoc.eq.4)then
          continue
        endif

      ELSEIF(IVERT.EQ.MVERT)THEN

C Local variable ireturn is non-zero to signal user wants to exit.
        IRETURN=0
        if(icfoc.eq.0)then
          if(.NOT.LINKED)then
            dok=.false.
            h(1)='To impose control, the defined control loops must'
            h(2)='be associated with the modelled zones.'
            CALL ASKOK('Control functions have not yet been',
     &        'associated with. Exit anyway?',OK,dok,2)
            IF(OK)then
              IRETURN=1
            else
              goto 92
            endif
          else
            IRETURN=1
          endif
        elseif(icfoc.eq.1.or.icfoc.eq.2.or.icfoc.eq.3.or.
     &         icfoc.eq.4)then
          IRETURN=1
        endif

C If not browsing the model ask user if they want to save any changes
C to control file.
        if(cfgok.and.(.NOT.browse))then
          IRETURN=1
          DOK=.TRUE.
          h(1)='You may have made changes to the control file. If'
          h(2)='you exit without saving these changes will be lost.'
          CALL ASKOK(' ','Save changes to control file?',OK,dok,2)
          IF(OK)THEN
            ICTLF=IFIL+1
            CALL CTLWRT(ICTLF,IER)
C            call edisp(IUOUT,'Saving system configuration file.')
            CALL EMKCFG('-',IER)
          ENDIF
        endif
        IF(IRETURN.EQ.1)RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Explanation
        CALL PHELPD('control help',24,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Save the control file.
        LASCI=LCTLF
        CALL EASKS(LASCI,'Control file?',' ',72,' ','Control file',
     &            IER,22)
        IF(LASCI(1:2).NE.'  ')LCTLF=LASCI

C Save current information into a descriptive file
C and save the system configuration file if not browsing.
        if(cfgok.and.(.NOT.browse))then
          ICTLF=IFIL+1
          CALL CTLWRT(ICTLF,IER)
C          call edisp(IUOUT,'Saving system configuration file.')
          CALL EMKCFG('-',IER)
        else
          call usrmsg('Cannot save control data while in browse',
     &                'mode. You must first `own` the model.','W')
          goto 92
        endif
      ELSEIF(IVERT.EQ.(MVERT-3))THEN
        CALL EASKABC(' ','Options:',
     &    'list control loops','edit control loops','cancel',IRC,2)
        if(irc.eq.1)then

C List control data.
          call edisp(itru,' ')
          call edisp(itru,' Overall description:')
          call edisp248(itru,ctldoc,72)

          if(icfoc.eq.0.and.NCF.gt.0)then
            WRITE(outs,'(a,I2,a)')' Zones control includes ',
     &        NCF,' functions.'
            call edisp(itru,outs)
            call edisp248(itru,znctldoc,72)
            do 144 ii=1,NCF
              call LSTCNTL(itru,0,ii,IER)
 144        continue
            call LSTCNTL(itru,0,0,IER)
          endif
          if(icfoc.eq.1.and.NCL.gt.0)then
            call edisp(itru,' ')
            WRITE(outs,'(a,I2,a)')' Plant control includes ',
     &        NCL,' loops.'
            call edisp(itru,outs)
            call edisp248(itru,plctldoc,72)
            do 145 ii=1,NCL
              call LSTCNTL(itru,1,ii,IER)
 145        continue
          endif
          if(icfoc.eq.2.and.NCC.gt.0)then
            call edisp(itru,' ')
            WRITE(outs,'(a,I2,a)')' Flow control includes ',
     &        NCC,' loops.'
            call edisp(itru,outs)
            call edisp248(itru,flctldoc,72)
            do 146 ii=1,NCC
              call LSTCNTL(itru,2,ii,IER)
 146        continue
          endif
          if(icfoc.eq.3.and.NGF.gt.0)then
            call edisp(itru,' ')
            WRITE(outs,'(a,I2,a)')' Global control includes ',
     &        NGF,' loops.'
            call edisp(itru,outs)
            call edisp248(itru,flctldoc,72)
            do 147 ii=1,NGF
              call LSTCNTL(itru,3,ii,IER)
 147        continue
          endif
          if(icfoc.eq.4.and.NECL.gt.0)then
            call edisp(itru,' ')
            WRITE(outs,'(a,I2,a)')' Electrical control includes ',
     &        NECL,' loops.'
            call edisp(itru,outs)
            call edisp248(itru,elctldoc,72)
            do 247 ii=1,NECL
              call LSTCNTL(itru,4,ii,IER)
 247        continue
          endif
        elseif(irc.eq.2)then
          call ezctlc(iier)
        endif
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Add or delete a zone control function.
C Warn user to update any connections to plant loops.
        h(1)='Delete/add/copy a control loop or day type'
        h(2)='(unless wkd:sat:sun control).'
        CALL EASKATOG(' ','Options:','delete control loop',
     &     'add control loop','copy control loop',
     &     'del/add/copy day type','cancel',' ',' ',IW,2)
        if(IW.eq.1)then
          call ADDCNTL(icfoc,'D',IER)
        elseif(IW.eq.2)then
          call ADDCNTL(icfoc,'A',IER)
        elseif(IW.eq.3)then
          call ADDCNTL(icfoc,'C',IER)
        elseif(IW.eq.4)then

C Add or delete a control day type (unless Weekday/Sat/Sun).
C Begin by selecting the loop to focus on.
          if(icfoc.eq.0)then
            CALL EASKI(IFOC,' ','Zone control function index?'
     &          ,0,'F',ncf,'F',0,'associated control',IERI,4)
            if(ieri.eq.-3) goto 92
            nn=nbcdt(IFOC)
          elseif(icfoc.eq.1)then
            CALL EASKI(IFOC,' ','Plant control function index?',
     &          0,'F',ncl,'F',0,'associated control',IERI,4)
            if(ieri.eq.-3) goto 92
            nn=npcdt(IFOC)
          elseif(icfoc.eq.2)then
            CALL EASKI(IFOC,' ','Flow control function index?',
     &          0,'F',ncc,'F',0,'associated control',IERI,4)
            if(ieri.eq.-3) goto 92
            nn=nfcdt(IFOC)
          elseif(icfoc.eq.3)then
            CALL EASKI(IFOC,' ','Global control function index?',
     &          0,'F',ngf,'F',0,'associated control',IERI,4)
            if(ieri.eq.-3) goto 92
            nn=ngcdt(IFOC)
          elseif(icfoc.eq.4)then
            CALL EASKI(IFOC,' ','Electrical control function index?',
     &          0,'F',necl,'F',0,'associated control',IERI,4)
            if(ieri.eq.-3) goto 92
            nn=necdt(IFOC)
          endif
          if(nn.eq.0)then
            call usrmsg('For the special case of Weekday/Sat/Sun',
     &      'control add/delete day type is not applicable.','W')
            goto 92
          endif 
          call ADDCTLD(icfoc,IFOC,IER)
        elseif(IW.eq.5)then
          goto 92
        endif
        call edisp(iuout,
     &'You may need to update links to zones, plant or flow networks.')
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C If there are enough items, allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MHEAD+M+1))THEN

C Edit control function.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        ii=mptback(IVERT)
        idi=iperptbk(IVERT)

C List out the current details of the control loop.
  64    CALL EVCNTRL(icfoc,II,1,1,'S',SSTR)
        call edisp(itru,' ')
        LN=max(1,LNBLNK(SSTR))
        WRITE(outs,65)II,SSTR(1:LN)
  65    FORMAT(' The sensor for function ',I2,' ',a)
        call edisp(itru,outs)

        CALL EVCNTRL(icfoc,II,1,1,'A',SSTR)
        LN=max(1,LNBLNK(SSTR))
        WRITE(outs,75)II,SSTR(1:LN)
  75    FORMAT(' The actuator for function ',I2,' is ',a)
        call edisp(itru,outs)

C Pull up a multi-choice dialog to see what should be edited.
        CALL EPMENSV
        h(1)='Control function attributes that may be edited:'
        h(2)=' a) sensor location'
        h(3)=' b) actuator location'
        h(4)=' c) for each day type, the period of validity and'
        h(5)='    details for distinct period during the day.'
        if(icfoc.eq.0)then
          call LSTCNTLD(itru,0,ii,idi,IER)
          write(outs,'(a,i2,a)') 'Zone control function (',II,
     &      ') editing.'
        elseif(icfoc.eq.1)then
          call LSTCNTLD(itru,1,ii,idi,IER)
          write(outs,'(a,i2,a)') 'Plant control function (',II,
     &      ') editing.'
        elseif(icfoc.eq.2)then
          call LSTCNTLD(itru,2,ii,idi,IER)
          write(outs,'(a,i2,a)') 'Flow control function (',II,
     &      ') editing.'
        elseif(icfoc.eq.3)then
          call LSTCNTLD(itru,3,ii,idi,IER)
          write(outs,'(a,i2,a)') 'Global control function (',II,
     &      ') editing.'
        elseif(icfoc.eq.4)then
          call LSTCNTLD(itru,4,ii,idi,IER)
          write(outs,'(a,i2,a)') 'Power control function (',II,
     &      ') editing.'
        endif
        idno=0
        isw=0
        call MENUATOL(outs,'Editing Options',
     &   'a sensor details','b actuator details',
     &   'c period of validity','d period data',' ',' ',
     &   ' ',' ',' ',' ',' ',' ',isw,idno,5)
        if(ISW.eq.0)then
          goto 92
        elseif(ISW.eq.1)then

C Do not edit sensor information if Nicol control algorithm
          IF(IFSN(II,2).EQ.7)THEN
            CALL USRMSG(
     &      'Sensor not required for behavioural control model',
     &      ' ','W')
            NCA=1
          ENDIF
          IF(NCA.EQ.1)GOTO 64

C Edit sensor location. 
          CALL EPMENSV
          if(icfoc.eq.0)then
            isn1=IBSN(II,1)
            isn2=IBSN(II,2)
            isn3=IBSN(II,3)
            isn4=IBSN(II,4)
            call senloc('Options:','Zone sensor',icfoc,II,
     &        isn1,isn2,isn3,isn4,isn5,isense,nsupl,'zone sensor',IER)
            IBSN(II,1)=isn1
            IBSN(II,2)=isn2
            IBSN(II,3)=isn3
            IBSN(II,4)=isn4
          elseif(icfoc.eq.1)then
            isn1=IPSN(II,1)
            isn2=IPSN(II,2)
            isn3=IPSN(II,3)
            isn4=IPSN(II,4)
            isn5=IPSN(II,5)
            call senloc('Options:','Plant sensor',icfoc,II,
     &        isn1,isn2,isn3,isn4,isn5,isense,nsupl,'plant sensor',IER)
            IPSN(II,1)=isn1
            IPSN(II,2)=isn2
            IPSN(II,3)=isn3
            IPSN(II,4)=isn4
            IPSN(II,5)=isn5
          elseif(icfoc.eq.2)then
            isn1=IFSN(II,1)
            isn2=IFSN(II,2)
            isn3=IFSN(II,3)
            isn4=IFSN(II,4)
            call senloc('Options:','Flow sensor',icfoc,II,
     &        isn1,isn2,isn3,isn4,isn5,isense,nsupl,'flow sensor',IER)
            IFSN(II,1)=isn1
            IFSN(II,2)=isn2
            IFSN(II,3)=isn3
            IFSN(II,4)=isn4

C Note isense is sensed flow condition used tp determine the flow controller type.    
            IFCTYP(II,1,1)=isense
            nfsup(ii)=nsupl

C Because changes in flow sensor usually require changes to actuation
C call the flow actuation interactions now.
            ian1=IFAN(II,1)
            ian2=IFAN(II,2)
            ian3=IFAN(II,3)
            call actloc('Options:','Flow actuator',icfoc,II,
     &        0,ian1,ian2,ian3,'flow actuator',IER)
            IFAN(II,1)=ian1
            IFAN(II,2)=ian2
            IFAN(II,3)=ian3
          elseif(icfoc.eq.3)then
            ign1=IGSN(II,1)
            ign2=IGSN(II,2)
            ign3=IGSN(II,3)
            ign4=IGSN(II,4)
            call senloc('Options:','Global sensor',icfoc,II,
     &        ign1,ign2,ign3,ign4,ign5,isense,nsupl,'global sensor',IER)
            IGSN(II,1)=ign1
            IGSN(II,2)=ign2
            IGSN(II,3)=ign3
            IGSN(II,4)=ign4
          elseif(icfoc.eq.4)then
            ien1=IESN(II,1)
            ien2=IESN(II,2)
            ien3=IESN(II,3)
            call senloc('Options:','Electrical sensor',icfoc,II,
     &        ien1,ien2,ien3,ien4,ien5,isense,nsupl,'power sensor',IER)
            IESN(II,1)=ien1
            IESN(II,2)=ien2
            IESN(II,3)=ien3
          endif
          CALL EPMENRC
          if (ICFOC.ne.2) then
            call usrmsg('After editing the sensor location it may',
     &        'be necessary to update the actuator location.','W')
          endif
          goto 64
        elseif(ISW.eq.2)then

C Edit actuator location.
C If sensor location -99 ask for up to three surfaces where
C substitutions will occur.
          CALL EPMENSV
          if(icfoc.eq.0)then 
            isn1=IBSN(II,1)
            ian1=IBAN(II,1)
            ian2=IBAN(II,2)
            ian3=IBAN(II,3)
            call actloc('Options:','Zone actuator',icfoc,II,
     &        isn1,ian1,ian2,ian3,'zone actuator',IER)
            IBAN(II,1)=ian1
            IBAN(II,2)=ian2
            IBAN(II,3)=ian3
          elseif(icfoc.eq.1)then
            ian1=IPAN(II,1)
            ian2=IPAN(II,2)
            ian3=IPAN(II,3)
            call actloc('Options:','Plant actuator',icfoc,II,
     &        0,ian1,ian2,ian3,'plant actuator',IER)
            IPAN(II,1)=ian1
            IPAN(II,2)=ian2
            IPAN(II,3)=ian3
          elseif(icfoc.eq.2)then
            ian1=IFAN(II,1)
            ian2=IFAN(II,2)
            ian3=IFAN(II,3)
            call actloc('Options:','Flow actuator',icfoc,II,
     &        0,ian1,ian2,ian3,'flow actuator',IER)
            IFAN(II,1)=ian1
            IFAN(II,2)=ian2
            IFAN(II,3)=ian3
          elseif(icfoc.eq.3)then
            ign1=IGAN(II,1)
            ign2=IGAN(II,2)
            ign3=IGAN(II,3)
            call actloc('Options:','Global actuator',icfoc,II,
     &        0,ign1,ign2,ign3,'global actuator',IER)
            IGAN(II,1)=ign1
            IGAN(II,2)=ign2
            IGAN(II,3)=ign3
          elseif(icfoc.eq.4)then
            ien1=IEAN(II,1)
            ien2=IEAN(II,2)
            ien3=IEAN(II,3)
            call actloc('Options:','Power actuator',icfoc,II,
     &        0,ien1,ien2,ien3,'power actuator',IER)
            IEAN(II,1)=ien1
            IEAN(II,2)=ien2
            IEAN(II,3)=ien3
          endif
          CALL EPMENRC
          call usrmsg(' ',' ','-')
          goto 64
        elseif(ISW.eq.3)then

C Edit date of validity (day type index is idi).
   48     if(icfoc.eq.0)then 
            IBDOY=IBCDV(II,idi,1)
            IEDOY=IBCDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IBCDV(II,idi,1)=IBDOY
            IBCDV(II,idi,2)=IEDOY
            IF(IBCDV(II,idi,2).LT.IBCDV(II,idi,1))GOTO 48
          elseif(icfoc.eq.1)then 
            IBDOY=IPCDV(II,idi,1)
            IEDOY=IPCDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IPCDV(II,idi,1)=IBDOY
            IPCDV(II,idi,2)=IEDOY
            IF(IPCDV(II,idi,2).LT.IPCDV(II,idi,1))GOTO 48
          elseif(icfoc.eq.2)then 
            IBDOY=IFCDV(II,idi,1)
            IEDOY=IFCDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IFCDV(II,idi,1)=IBDOY
            IFCDV(II,idi,2)=IEDOY
            IF(IFCDV(II,idi,2).LT.IFCDV(II,idi,1))GOTO 48
          elseif(icfoc.eq.3)then
            IBDOY=IGCDV(II,idi,1)
            IEDOY=IGCDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IGCDV(II,idi,1)=IBDOY
            IGCDV(II,idi,2)=IEDOY
            IF(IGCDV(II,idi,2).LT.IGCDV(II,idi,1))GOTO 48
          elseif(icfoc.eq.4)then
            IBDOY=IECDV(II,idi,1)
            IEDOY=IECDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IECDV(II,idi,1)=IBDOY
            IECDV(II,idi,2)=IEDOY
            IF(IECDV(II,idi,2).LT.IECDV(II,idi,1))GOTO 48
          endif 
          goto 64
        elseif(ISW.eq.4)then

C Edit period data (day type index is idi), first list the
C current day type data..
          CALL EPMENSV
          if(icfoc.eq.0)then
            call LSTCNTLD(itru,0,II,idi,IER)
            CALL EBCNTRL(ITRC,II,idi,IER)
          elseif(icfoc.eq.1)then
            call LSTCNTLD(itru,1,II,idi,IER)
            CALL EPCNTRL(ITRC,II,idi,IER)
          elseif(icfoc.eq.2)then
            call LSTCNTLD(itru,2,II,idi,IER)
            CALL EFCNTRL(II,idi,IER)
          elseif(icfoc.eq.3)then
            call LSTCNTLD(itru,3,II,idi,IER)
            CALL EGCNTRL(ITRC,II,idi,IER)
          elseif(icfoc.eq.4)then
            call LSTCNTLD(itru,4,II,idi,IER)
            CALL edisp(iuout,'Elecrical editing not available')
          endif
          CALL EPMENRC
          goto 64
        elseif(ISW.eq.0)then
          GOTO 92
        endif
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

      END

C ***** totctlper
C Finds the current number of periods in all control loops
C of type icfoc.
      subroutine totctlper(icfoc,ncper)
#include "building.h"
#include "plant.h"
#include "net_flow.h"
#include "control.h"
#include "power.h"
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)
      common/pctl/ncl,ipsn(mcl,5),ipan(mcl,4),npcdt(mcl),
     &       ipcdv(mcl,mpcdt,2),npcdp(mcl,mpcdt),tpcps(mcl,mpcdt,mpcdp),
     &       ipctyp(mcl,mpcdt,mpcdp),ipclaw(mcl,mpcdt,mpcdp),
     &       pmiscd(mcl,mpcdt,mpcdp,misc)
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)
      COMMON/ECTL/NECL,IESN(MEL,3),IEAN(MEL,3),NECDT(MEL),
     &            IECDV(MEL,MECDT,2),NECDP(MEL,MECDT),
     &            TECPS(MEL,MECDT,MECDP),IECTYP(MEL,MECDT,MECDP),
     &            IECLAW(MEL,MECDT,MECDP),EMISCD(MEL,MECDT,MECDP,MISC)
      common/octl/nof,iosn(mol,4),ioan(mol,3),nocdt(mol),
     &       iocdv(mol,mocdt,2),nocdp(mol,mocdt),tocps(mol,mocdt,mocdp),
     &       ioclaw(mol,mocdt,mocdp),omiscd(mol,mocdt,mocdp,misco)

      if(icfoc.eq.0)then
        ncper=0
        if(ncf.gt.0)then
          do 888 II=1,ncf
            NN=nbcdt(ii)
            if(nbcdt(ii).eq.0)NN=3
            do 887 IJ=1,NN
              ncper=ncper+1
  887       continue
  888     continue
        endif
      elseif(icfoc.eq.1)then
        ncper=0
        if(ncl.gt.0)then
          do 886 II=1,ncl
            NN=npcdt(ii)
            if(npcdt(ii).eq.0)NN=3
            do 885 IJ=1,NN
              ncper=ncper+1
  885       continue
  886     continue
        endif
      elseif(icfoc.eq.2)then
        ncper=0
        if(ncc.gt.0)then
          do 884 II=1,ncc
            NN=nfcdt(ii)
            if(nfcdt(ii).eq.0)NN=3
            do 883 IJ=1,NN
              ncper=ncper+1
  883       continue
  884     continue
        endif
      elseif(icfoc.eq.3)then
        ncper=0
        if(ngf.gt.0)then
          do 882 II=1,ngf
            NN=ngcdt(ii)
            if(ngcdt(ii).eq.0)NN=3
            do 881 IJ=1,NN
              ncper=ncper+1
  881       continue
  882     continue
        endif
      elseif(icfoc.eq.4)then
        ncper=0
        if(necl.gt.0)then
          do 782 II=1,necl
            NN=necdt(ii)
            if(necdt(ii).eq.0)NN=3
            do 781 IJ=1,NN
              ncper=ncper+1
  781       continue
  782     continue
        endif
      endif
      return
      end

C *********** EBCNTRL
C Edit building control function II.
      SUBROUTINE EBCNTRL(ITRC,II,IV,IER)
#include "building.h"
#include "plant.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)
      common/sctl/tcps,ictyp,iclaw,cm(misc)
      
C Plant common required for building/plant flux linkage      
      common/pcdat/nnodes(mpcom),isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)

      DIMENSION SALT(24),IVALS(24)
      CHARACTER*82 VERT(20)

      CHARACTER outs*124,H*72,SSTR*72,TMP*72,SALT*40,hold*40,KEY*1
      character outsa*84,outsb*84,outsc*84,ltmp*248,SSTRB*48
      logical close1,close2,close3,close4,useold

      integer iclawt   ! control law index passed back from editctlperiod
      dimension icsn(4)! for passing sensor location to
      integer icsn
      integer lastndx  ! to keep track of last data position used.

C Number of building functions and day type must be > 0.
      IF(II.LE.0.OR.IV.LE.0)RETURN

C Building control day types.
      J=IV
      JJ=J

C Initial menu entry setup.
   92 IER=0
      IVERT=-3
      WRITE(vert(1),'(a,i3,a,i3)')' function',II,' day type',JJ
      WRITE(vert(2),'(a,i3)')     ' number of periods:',NBCDP(II,JJ)
      WRITE(vert(3),'(a)') ' ________________________________________'
      WRITE(vert(4),'(a)')
     &' per|start|sensed  |actuated | control law     | data'
      WRITE(vert(5),'(a)')
     &' no.|time |property|property |                 |     '
      LL=NBCDP(II,JJ)

C Generate menu entry for each period in day type.
      m=5
      DO 91 KP=1,LL
        KK=KP
        m=m+1
        call stfctl(0,II,JJ,KK)
        CALL EVCNTRL(0,II,JJ,KK,'T',SSTR)
        CALL EVCNTRLAW(0,II,JJ,KK,TMP,LTMP)
        NITEMS=INT(cm(1))
        WRITE(outsa,'(i2,F6.2,1x,a,1x,a)')KK,tcps,SSTR(1:18),TMP(1:18)
        outsb=' '
        outsc=' '
        if(NITEMS.gt.0)then
          if(NITEMS.LE.8)then
            write(outsb,'(8F9.1)')(cm(L),L=2,NITEMS+1)
          else
            write(outsb,'(8F9.1,a)')(cm(L),L=2,9),'..'
          endif
          call SDELIM(outsb,outsc,'S',IW)
        endif
        CALL EMKEY(M-5,KEY,IER)
        write(vert(m),'(1a,1x,a,1x,a)',IOSTAT=IOS,ERR=3) key,
     &    outsa(1:46),outsc(1:32)
  91  continue
      m=m+1
      WRITE(vert(m),'(a)') ' ________________________________________'
      m=m+1
      WRITE(vert(m),'(a)') '* add/ delete a period '
      m=m+1
      WRITE(vert(m),'(a)') '? help '
      m=m+1
      WRITE(vert(m),'(a)') '- exit  '
      mvert=m

C Help text for this menu.
      h(1) ='A number of control periods can be defined for each'
      h(2) ='control day type. Each period has a start time (N.B'
      h(3) ='periods may not overlap), a sensed property, an actuated'
      h(4) ='property, a control law and miscellaneous data items'
      h(5) ='defining available capacity, set-points etc.'
 
C Now display the menu.
      CALL EMENU('Control periods',VERT,MVERT,IVERT)
      if(IVERT.EQ.mvert)then
        return
      elseif(IVERT.EQ.mvert-1)then

C Explanation
       CALL PHELPD('control period help',5,'-',0,0,IER)
      elseif(IVERT.EQ.mvert-2)then

C Add or delete a period.
        h(1)='Periods are added to the end of the list.'
        CALL EASKABC(' ','Options:',
     &    'add period','delete period','cancel',iad,1)
        if(iad.eq.1)then
          call addctldp(0,II,JJ,'A',iier)
        elseif(iad.eq.2)then
          call addctldp(0,II,JJ,'D',iier)
        endif
        goto 92
      ELSEIF(IVERT.GT.5.AND.IVERT.LT.(mvert-3))THEN

C Take the t?cps() i?ctyp(),i?claw(), ?MISCD(II,JJ,KK,?) common and
C stuff into working common array 'cm' for editing.  At end
C of if:elseif:else recover.
        KK=IVERT-5
        call stfctl(0,II,JJ,KK)

        NITEMS=INT(cm(1))
        if(NITEMS.gt.0)then
          WRITE(outs,'(a,i3,a)')' For period',KK,' current data is...'
          call edisp(iuout,outs)
          if(NITEMS.LE.10)then
            write(outs,'(10F12.4)')(cm(L),L=2,NITEMS)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
          else
            write(outs,'(10F12.4)')(cm(L),L=2,10)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
            if(NITEMS.LE.20)then
              write(outs,'(10F12.4)')(cm(L),L=11,NITEMS)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            else
              write(outs,'(10F12.4)')(cm(L),L=11,20)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
              write(outs,'(10F12.4)')(cm(L),L=21,NITEMS)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            endif
          endif
        else
          WRITE(outs,'(a,i3,a)')' For period',KK,' define...'
          call edisp(iuout,outs)
        endif


        call edisp(iuout,' ')

C If thermophysical property substitution set type and law and jump
C to periods.
        if(IBSN(II,1).eq.-99)then
          ictyp=0
          iclaw=1
          goto 62
        endif

C If menu editing version available use it otherwise use the
C sequential question interface. If return from editctlperiod
C having selected a control law it does not deal with then
C this will be signaled by an ier condition equal to the control
C law that has been selected so further user selection of control
C law can be jumped around.
        useold=.false.
        iclawt=-1
  77    if(useold)then
          if(iclawt.ge.0.and.iclawt.le.23)then
            iclaw=iclawt
            IBCLAW(II,JJ,KK)=iclaw
            useold=.false.
            goto 62
          endif
        endif
        if((iclaw.ge.1.and.iclaw.lt.6).or.
     &      iclaw.eq.7.or.iclaw.eq.8.or.iclaw.eq.10.or.
     &      iclaw.eq.13.or.iclaw.eq.14.or.iclaw.eq.15.or.
     &      iclaw.eq.21.or.iclaw.eq.22.or.iclaw.eq.23.
     &      or.iclaw.eq.24)then

C Remember the previous start (prevstart) and the total number of
C period in this control loop (nper) and the number of day types
C included in the control loop defintion (loopdaytype) before
C starting up the menu based control period definition.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92
        endif

        ictyp=0
        SALT(1)= 'Basic control for heating/cooling      '
        SALT(2)= 'Free-float control                     '
        SALT(3)= 'Basic pre-heat or pre-cool control     '
        SALT(4)= 'Fixed heat injection and extraction    '
        SALT(5)= 'PID control action for heating/cooling '
        SALT(6)= 'Flux connection between zone & plant   '
        SALT(7)= 'Multi-stage control with hysteresis    '
        SALT(8)= 'Variable supply temp. with constraints '
        SALT(9)= 'Heat pipe                              '
        SALT(10)='Separate flux ON & OFF set points      '
        SALT(11)='Match sensed & recorded values (ideal) '
        SALT(12)='Match sensed & recorded values (on/off)'
        SALT(13)='Time-proportioning (on/off)            '
        SALT(14)='Floating three-position control        '
        SALT(15)='Optimum start control (with rewind)    '
        SALT(16)='Optimum stop control                   '
        SALT(17)='Fuzzy Logic PI or PD control           '
        SALT(18)='Null control.                          '
        SALT(19)='Multi-sensor heating/cooling           '
        SALT(20)='Evaporative source (surface)           '
        SALT(21)='Slave capacity control                 '
        SALT(22)='VAV cooling with CAV reheat            '
        SALT(23)='Match sensed values (ideal:2 setpoints)'
        SALT(24)='Adaptive human comfort model           '
        if(iclaw.ne.0)then
          write(outs,'(a,a)') 'Current control law: ',SALT(iclaw)
          call edisp(iuout,outs)
          IX=1
        else
          IX=iclaw
        endif
        H(1)='The basic zone controller is an ideal controller that'
        H(2)='will inject or extract sufficient flux (subject to'
        H(3)='the defined heating and cooling capacities) to ensure'
        H(4)='the heating and/or cooling set point.'
        H(5)=' '
        h(6)='A free float controller allows the zone temperature'
        h(7)='to reach whatever temperature is consistent with the '
        h(8)='heat gains and losses in the zone. No environmental'
        h(9)='control is applied. '
        H(10)=' '
        H(11)='The ideal fixed injection controller represents'
        H(12)='systems which operate with a fixed input when heating'
        H(13)='and/or cooling is requested. '
        H(14)=' '
        H(15)='An ideal proportional controller with optional'
        H(16)='integral and/or derivative action time allowing'
        H(17)='full PID control.'
        H(18)=' '
        H(19)='A control which links a zone with a system network'
        H(20)='component.'
        H(21)=' '
        H(22)='A multi-stage controller with hysteresis allows'
        H(23)='the specification of heating and cooling capacities'
        H(24)='for different stages.'
        H(25)=' '
        H(26)='Variable supply temperature with limit constraints '
        H(27)='is an idealised air based control that delivers air'
        H(28)='at a fixed flow rate (same for heating & cooling '
        H(29)='operation). The heating supply temperature varies '
        H(30)='up to a maximum setpoint and the cooling supply '
        H(31)='temperature varies down to a minimum setpoint. '
        H(32)=' '
        H(33)='Control with separate on/off setpoints allows '
        H(34)='separate flux ON and flux OFF set points for '
        H(35)='heating and cooling via ON until OFF point logic.'
        H(36)=' '
        H(37)='Time proportioning control with separate on/off '
        H(38)='setpoints for heating and cooling extends the '
        H(39)='functionality with cycle time limits. '
        H(40)=' '
        H(41)='The floating action (three-position) controller'
        H(42)='uses an open switch and shut switch differential (C)'
        H(43)='for heating and cooling as well as timestep rate of '
        H(44)='change for heating and cooling.'
        H(45)=' '
        H(46)='The optimum start controller allows for three modes'
        H(47)='of operation (each with rewind): '
        H(48)=' (1) default start at 4:00;'
        H(49)=' (2) start at user defined time;'
        H(50)=' (3) time estimate using Birtles & John equation.'
        H(51)=' '
        H(52)='Proportional capacity slave controller. Typically a'
        H(53)='slave control is used to represent an environmental'
        H(54)='control with a thermostat in one zone and actions in'
        H(55)='a number of zones.'
        H(56)=' '
        H(57)='Match sensed values (ideal:2 setpoints) control law:'
        H(58)='this is a combination of the ideal controller and'
        H(59)='control law 11 where values from the temporal file'
        H(60)='are taken to set the setpoints for heating and cooling'
  63    CALL EPICKS(IX,IVALS,' ','Control law:',
     &    40,24,SALT,'control law (indented=old or obscure)',IER,60)
        if(ix.eq.0.or.IVALS(1).eq.0)goto 63
        iclaw=IVALS(1)
        IBCLAW(II,JJ,KK)=iclaw

  62    H(1)='See ESP-r Data Model Section 9 for details.'
        CALL EASKR(tcps,' ','Period start time?',
     &    0.,'F',24.,'F',0.,'control start',IER,1)
        if(KK.GT.1)then
          IF(TBCPS(II,JJ,KK-1).GE.tcps)then
            call usrmsg('Periods out of order.',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcps,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0.','W')
            GOTO 62
          endif
        endif

C If thermophysical property substitution then ask for misc. data.
        if(IBSN(II,1).eq.-99)then
          if(int(cm(2)).eq.0)then
            outs=' Current property substitution is OFF.'
          elseif(int(cm(2)).eq.1)then
            outs=' Current property substitution is ON.'
          endif
          cm(1)=4.0
          H(1)='See ESP-r Data Model Section 9 for details.'
          CALL EASKR(cm(2),outs,' Substitution: 0) off, 1) on?',
     &      0.,'F',1.,'F',0.,'subst on/off',IER,1)

  28      WRITE(HOLD,'(3f8.0)')cm(3),cm(4),cm(5)
          H(1)='These are the indices of the multi-layer'
          H(2)='construction for the 1st, 2nd and 3rd surfaces'
          H(3)='to which susbsitutions are to be made. If less'
          H(4)='than 3 surfaces then assign `0` to the last.'
          if(IBAN(II,1).ne.0)then
            CALL EASKS(HOLD,' ','MLC index for 1st/2nd/3rd surfaces?',
     &        40,'  0   0  0  ','index for subst',IER,3)
            K=0
            CALL EGETWR(HOLD,K,cm(3),1.,18.,'F','1st sur mlc',IER)
            CALL EGETWR(HOLD,K,cm(4),1.,18.,'F','2nd sur mlc',IER)
            CALL EGETWR(HOLD,K,cm(5),1.,18.,'F','3rd sur mlc',IER)
            if(ier.ne.0)goto 28
          else
            cm(3)=0.
            cm(4)=0.
            cm(5)=0.
          endif
          call extrctl(0,II,JJ,KK)
          goto 92
        endif

C Make assumptions about the additional data that will be required.
        H(1)='See ESP-r Data Model Section 9 for details.'
        if(IBCLAW(II,JJ,KK).ge.1.and.IBCLAW(II,JJ,KK).lt.6)then

C Basic control (1) or pre-heat/cool (3) is taken care of by a
C call to editctlperiod.
C Free-floating control (2) is taken care of by a call to editctlperiod.
C Ideal fixed injection (4) is taken care of by a call to editctlperiod.
C PID controller (5) is taken care of by a call to editctlperiod.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawT=ier
            goto 77
          endif
          goto 92

        elseif(IBCLAW(II,JJ,KK).eq.6)then

C Zone - plant connector.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,'Current zone-plant connector data is:')
            call edisp(iuout,
     &      ' Supply comp & node, Index, Max Heat W, Mac Cool W')
            write(outs,'(5F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6)
            call edisp(iuout,outs)
            call edisp(iuout,' Extract Cmpt & Var')
            write(outs,'(2F9.2)') cm(7),cm(8)
            call edisp(iuout,outs)
            call edisp(iuout,' ')
          endif

          H(1)='The number of data items required is either'
          H(2)='5 or 7 and depends on whether there is an'
          H(3)='extract component (e.g fan).'
          CALL EASKR(cm(1),'Number of miscellaneous','data items?',
     &      5.,'F',7.,'F',7.,'no. of misc data items',IER,3)
     
C Display list of plant components:         
          CALL ASKPCMP(' Supply plant component ? ',
     &                 '-',ISELS,IER)
          cm(2)=ISELS
          if(ISELS.eq.0) return
  
          if(nnodes(ISELS).gt.1) then
            call getnod(ISELS,knode)
            if(knode.eq.0) return
            cm(3)=knode
          else
            cm(3)=1.0
          endif

          h(1)=' The coupling type depends upon the component '
          h(2)=' that connects the plant to the building zone'
          h(3)=' For warm/cool air-based systems use type 1,'
          h(4)=' and for radiators, heated floors, etc. '
          h(5)=' use type 2. '    
          CALL EASKR(cm(4),'  Coupling type',
     &      ' 1.) mCpdT calc, 2.) heat flux transfer ? ',
     &      1.,'F',2.,'F',1.,'coupling type',IER,5)

C Check if coupling index = 1. If so determine if the most recent
C zone temperatures should be used when determining the zone 
C flux (Q=m'Cp(Ts' + Ta)) or if the flux should be calculated
C using zone temperatures from the previous timestep :
C (Q=m'Cp(Ts' + Ta'))
          call eclose(cm(4), 1.0, 0.01,close1)
          if ( close1 ) then
            H(1)= ' While the ESP-r simulator can use an iterative'
            H(2)= ' method to solve the combined buidling and'
            H(3)= ' plant systems, it is often acceptable to solve'
            H(4)= ' the two domains sequentially. IN this case the'
            H(5)= ' building domain is solved first. The results'
            H(6)= ' are then saved and the simulation moves on to'
            H(7)= ' the next time step.'
            H(8)= ' '
            H(9)= ' Since the plant domain is solved after the'
            H(10)=' building domain, the building domain solver'
            H(11)=' must use the plant injection temperature from'
            H(12)=' the previous time step when calculating the'
            H(13)=' amount of heat injected into the zone. Two'
            H(14)=' methods are offered for calculating the amount'
            H(15)=' of heat injection/extraction:'
            H(16)=' '
            H(17)=' 1) Sequential boundry conditions: the Simulator' 
            H(18)='    uses the plant boundary conditions from '
            H(19)='    the previous time step but zone boundary'
            H(20)='    conditions from the present time step (to'
            H(21)='    solve the zone-side energy balance). This'
            H(22)='    approach can lead to inequality between'
            H(23)='    the heat flux injected into the zone and '
            H(24)='    the amount extracted from the plant, '
            H(25)='    especially at large time steps.'
            H(26)=' 2) Concurrent boundary conditions: the'
            H(27)='    Simulator uses the zone and plant conditions'
            H(28)='    from the pervious time step to calcuate '
            H(29)='    the heat injection that prevailed over '
            H(30)='    the previous timestep, and injects this '
            H(31)='    into the zone at the current time step. '
            H(32)='    This approach ensures that the heat flows'
            H(33)='    calculated in the building and plant '
            H(34)='    domains can be reconciled over the '
            H(35)='    sumulation period'
            H(36)=' '
            H(37)=' Alternatively, turn on iteration.'
            
            CALL EASKAB(
     &        'Zone heat injection calculation method.',
     &        'Options:',
     &        'sequential boundary conditions',
     &        'concurrent boundary conditions',IW,37)

            if (IW .eq. 2) cm(4) = 5.0

          endif
          CALL EASKR(cm(5),' ','Maximum heating flux (W)?',
     &      0.,'F',0.,'-',1000.,'max heat flux',IER,1)
          CALL EASKR(cm(6),' ','Maximum cooling flux (W)?',
     &      0.,'-',0.,'-',1000.,'max cool flux',IER,1)

          IF(int(cm(1)).EQ.7)THEN
           CALL ASKPCMP(' Extract plant component ? ',
     &                  '-',ISELE,IER)
           cm(7)=ISELE
           if(ISELE.eq.0) return

           if(nnodes(ISELE).gt.1) then
             call getnod(ISELE,knode)
             if(knode.eq.0) return
             cm(8)=knode
           else
             cm(8)=1.0
           endif
          ENDIF
  
        elseif(IBCLAW(II,JJ,KK).eq.7.or.IBCLAW(II,JJ,KK).eq.8)then

C Multi-stage (7) is taken care of by a call to editctlperiod.
C CV HAVC variable supply temperature with limit constraints (8) 
C   is taken care of by a call to editctlperiod.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92
        elseif(IBCLAW(II,JJ,KK).eq.9)then

C Heat pipe.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current heat pipe data is:')
            call edisp(iuout,
     &' Iter indx, Surf, Out node, In node, Crit T, Max iter, Toler W')
            write(outs,'(7F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),
     &        cm(7),cm(8)
            call edisp(iuout,outs)
            call edisp(iuout,' ')
          endif
          cm(1)=9.0
          CALL EASKR(cm(2),' Iteration index',
     &      ' 0) no iteration,  1) iteration ? ',
     &      0.,'F',1.,'-',0.,'iteration index',IER,1)

          CALL EASKR(cm(3),' ',' Surface no with heat pipe ? ',
     &      1.,'F',32.,'F',1.,'surface with heatpipe',IER,1)
          CALL EASKR(cm(4),' ',' Outer node for heatpipe ? ',
     &      1.,'F',8.,'F',1.,'outer node',IER,1)
          CALL EASKR(cm(5),' ',' Inner node for heatpipe ? ',
     &      1.,'F',8.,'F',2.,'inner node',IER,1)
          CALL EASKR(cm(6),' ',' Critical temperature ? ',
     &      -100.,'F',100.,'F',25.,'critical temp',IER,1)
          CALL EASKR(cm(7),' ',' Maximum no of iterations ? ',
     &      1.,'F',100.,'F',1.,'max iterations',IER,1)
          CALL EASKR(cm(8),' ',' Flux difference tolerance ? ',
     &      1.,'F',1000.,'F',10.,'flux tolerance',IER,1)
          CALL EASKR(cm(9),' Trace flag...',
     &    ' 0) no trace, 1) trace ? ',0.,'F',1.,'F',0.,'trace',IER,1)
        elseif(IBCLAW(II,JJ,KK).eq.10)then

C Control with separate on/off setpoints for heating and cooling.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92

        elseif(IBCLAW(II,JJ,KK).eq.11)then

C Match temp to a function of multi-sensor values (ideal mode).
C This logic is also used for the case of a setpoint temperature
C defined in a temporal file.  Note that users should first have
C setup the control via the context->temporal menu so it is usually
C not required that a user do subsequent editing to the zone control
C law ll details.
          H(1)=' - Refer ESP-r Data Model section 9 '
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current `Ideal Match Temp to Multi-Sen` data:')
            call edisp(iuout,
     &     ' Max H W, Min H W, Max C W, Min C W, No. Axy Sen, Sen mode')
            write(outs,'(6F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
            call edisp(iuout,outs)
            
            I6=INT(cm(6))
            ndx=0
            if(I6.gt.2) I6=2
            DO 123 L=1,I6              

C Collect up the current information and generate a descriptive
C string to remind the user.
              ndx=8+4*(L-1)
              call edisp(iuout,' Auxliary Sensor Details')              
              write(outs,'(4F9.2)')cm(ndx),cm(ndx+1),cm(ndx+2),cm(ndx+3)
              call edisp(iuout,outs)
              icsn(1)=nint(cm(ndx))
              icsn(2)=nint(cm(ndx+1))
              icsn(3)=nint(cm(ndx+2))
              icsn(4)=nint(cm(ndx+3))
              SSTR=' '
              call decodesensor(0,icsn,SSTR,SSTRB,ier)
              call edisp(iuout,SSTR)
              if(INT(cm(7)).eq.4)then
                ndx=12+(L-1)+4*(I6-1)         
                write(outs,'(a,F9.2)')' Weighting for sensor ',cm(ndx)
                call edisp(iuout,outs)
              endif
123         continue   
          endif

          H(1)=' The number of data items required is in the range'
          H(2)=' 10 to 16 and depends on the auxiliary sensor mode' 
          H(3)=' selected.'
          H(4)='                                   |no. of| |no. of|'
          H(5)='          Function                 |aux sn| |items |'
          H(6)= '---------------------------------------------------'
          H(7)=' greatest val of aux`y sensors         1       10 '
          H(8)='   `      `  `    `     `              2       14 '  
          H(9)=' least val of aux`y sensors            1       10 '
          H(10)='   `    `  `    `     `               2       14 ' 
          H(11)=' mean val of aux`y sensors            1       10 '
          H(12)='   `   `  `    `     `                2       14 '
          H(13)=' weighting of aux`y sensors           2       16 '
          H(14)=' '
          H(15)='PLUS 1 item for scaling factor (i.e. a % offset)'
          H(16)='PLUS 1 item for offset factor (absolute offset) '
          H(17)=' '
          H(18)='NOTE: if you are using this control to get a setpoint'
          H(19)='from a temporal file a zone control might already have'
          H(20)='created within the model context->temporal data menu.'
          H(21)=' '
          H(22)=' '
          CALL EASKR(cm(1),' ',' No. of misc data items ',
     &      10.,'F',18.,'F',10.,'no. of misc data items',IER,22)

  53      WRITE(HOLD,'(4f8.0)')cm(2),cm(3),cm(4),cm(5)
          H(1)='Heat delivery is normally set high so that the '
          H(2)='target temperature is attained. The heating minimum '
          H(3)='is normally 0 but if > 0 then heating will continue'
          H(4)='at the minimum even when the temperature is'
          H(5)='reached. '
          H(6)='In the unlikely event that you want to control to'
          H(7)='another temperature but allow no cooling capacity '
          H(8)='the control may not work properly.'
          CALL EASKS(HOLD,
     &      ' Max & Min heat capacity, Max & Min cool capacity for',
     &      ' matching another temp: ',40,' 10000. 0. 10000. 0. ',
     &      ' track another max min heat cool',IER,8)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,999999.,'F','Max Heat',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,999999.,'F','Min Heat',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,999999.,'F','Max Cool',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,999999.,'F','Min Cool',IER)
          if(ier.ne.0)goto 53

          H(1)=' At present, this is limited to 2 auxiliary sensors'
          H(2)=' which may be located outside or in other zone(s).'
          CALL EASKR(cm(6),' ',' No of auxiliary sensors ? ',
     &      0.,'F',2.,'F',1.,'no. of auxiliary sensors',IER,2)

          H(1)=' The sensor modes options are:'
          H(2)=' 1: (ref sensor)-(greatest value of aux sensors)' 
          H(3)=' 2: (ref sensor)-(least value of aux sensors)'
          H(4)=' 3: (ref sensor)-(mean value of aux sensors)'
          H(5)=' 4: (ref sensor)-(weighting of aux sensors)'
          CALL EASKR(cm(7),' ',' Multi-sensor mode (see help)?',
     &      1.,'F',4.,'F',0.,' multi-sensor mode',IER,5)
          
C Auxiliary sensor details,
          I6=INT(cm(6))  
          H(1)='The sensor details are essentially the same as '
          H(2)='required in a control point sensor ie. '
          H(3)=' 1st aux item >0 is a zone and or surface, =0 any zone,'
          H(4)='   = -1 measuring plant component,  '
          H(5)='   = -2 measuring mix air:radiant,'
          H(6)='   = -3 ambient where the 2nd item is:'
          H(7)='        0 = db, 1 = sol-air, 2 = wind velo, '
          H(8)='        3 = wind dir, 4 = diffuse rad, '
          H(9)='        5 = direc norm rad, 6 = RH'
          h(10)='  = -5 an item in a temporal file AND the 2nd aux item'
          h(11)='       is the column of the temporal item.'
          h(12)='       NOTE: these would already be set for you if'
          h(12)='       you used the context->temporal data menu'
          h(13)='       option to associate a temporal setpoint item'
          h(14)='       with a model.'
          h(15)=' '
          h(16)=' 2nd item is used to further specify the location '
          h(17)=' 3rd item is used to further specify the location '
          h(18)=' 4th item is always zero '
          H(19)=' - Refer ESP-r Data Model section 9 '
          DO 125 L=1,I6
            ndx=8+4*(L-1)

C Collect up the current information and generate a descriptive
C string to remind the user.
            icsn(1)=nint(cm(ndx))
            icsn(2)=nint(cm(ndx+1))
            icsn(3)=nint(cm(ndx+2))
            icsn(4)=nint(cm(ndx+3))
            SSTR=' '
            call decodesensor(0,icsn,SSTR,SSTRB,ier)
            call edisp(iuout,SSTR)
            CALL EASKR(cm(ndx),' ',
     &        ' Aux sensor detail 1st item (see help)?',
     &        -5.,'F',0.,'-',1.,' Aux sen detail 1st item',IER,19)
            CALL EASKR(cm(ndx+1),' ',' Aux sensor detail 2nd item ?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 2nd item',IER,19)          
            CALL EASKR(cm(ndx+2),' ',' Aux sensor detail 3rd item ?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 3rd item',IER,19)          
            CALL EASKR(cm(ndx+3),' ',
     &        ' Aux sensor detail 4th item (always use zero)?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 4th item',IER,19)
125       continue 
          lastndx=ndx+4         
          
          I7=INT(cm(7))
          if(I7.eq.4)then
            DO 126 L=1,I6
C Weighted auxiliary sensors mode,
              ndx=lastndx  ! set to next slot after aux senor data.        
              H(1)='The sensor weightings must total exactly 100.0'
              H(2)='This allows you to discount one of the sensors.'
              if(i6.eq.1) cm(ndx)=100.0  ! if only one sensor.
              CALL EASKR(cm(ndx),' ',
     &          'Sensor weighting (between 0.0 & 100.0)?',
     &          0.,'F',100.,'F',50.,' Aux sensor weighting',IER,2)
126         continue
          else
            ndx=nint(cm(1))  ! set this so scaling next to last entry. 
            write(6,*) 'lastndx and ndx ',lastndx,ndx,cm(1)   
          endif

C At end of data add in scaling factor and offset. Typically
C a scaling factor is 1.0
          H(1)='The sensor scaling factor is applied to the sensed'
          H(2)='value at each timestep e.g. (20C sensed x 1.1) would'
          H(3)='request a 10% rise over measured. '
          if(cm(ndx).lt.0.01) cm(ndx+1)=1.0
          CALL EASKR(cm(ndx),' ',' Scaling factor ? ',
     &        0.,'F',10.,'F',1.,' scaling factor',IER,3)
          H(1)='The sensor offset allows you to modify the sensed'
          H(2)='value by a fixed amount. '
          CALL EASKR(cm(ndx+1),' ',' Offset factor ? ',
     &        -10.,'F',10.,'F',1.,' offset factor',IER,2)
        elseif(IBCLAW(II,JJ,KK).eq.12)then

C Match temp to a function of multi-sensor values (on-off mode).
          H(1)=' - Refer ESP-r Data Model section 9 '
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current `Ideal Match Temp to Multi-Sen` data:')
            call edisp(iuout,
     &  ' Max Htg W, Max Clg W, Htg Dif, Clg Dif,No. Axy Sen, Sen mode')
            write(outs,'(6F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
            call edisp(iuout,outs)
            
            I6=INT(cm(6))
            DO 133 L=1,I6              
              ndx=8+4*(L-1)
              call edisp(iuout,' Auxiliary Sensor Details')
              write(outs,'(4F9.2)')cm(ndx),cm(ndx+1),cm(ndx+2),cm(ndx+3)
              call edisp(iuout,outs)

C Collect up the current information and generate a descriptive
C string to remind the user.
              icsn(1)=nint(cm(ndx))
              icsn(2)=nint(cm(ndx+1))
              icsn(3)=nint(cm(ndx+2))
              icsn(4)=nint(cm(ndx+3))
              SSTR=' '
              call decodesensor(0,icsn,SSTR,SSTRB,ier)
              call edisp(iuout,SSTR)
              if(INT(cm(7)).eq.4)then
                ndx=12+(L-1)+4*(I6-1)         
                write(outs,'(a,F9.2)')' Weighting for sensor ',cm(ndx)
                call edisp(iuout,outs)
             endif
133        continue   
          endif

          H(1)=' The number of data items required is in the range'
          H(2)=' 10 to 16 and depends on the auxiliary sensor mode' 
          H(3)=' selected.'
          H(4)='                                   |no. of| |no. of|'
          H(5)='          Function                 |aux sn| |items |'
          H(6)= '---------------------------------------------------'
          H(7)=' greatest val of aux`y sensors         1       10 '
          H(8)='   `      `  `    `     `              2       14 '  
          H(9)=' least val of aux`y sensors            1       10 '
          H(10)='   `    `  `    `     `               2       14 ' 
          H(11)=' mean val of aux`y sensors            1       10 '
          H(12)='   `   `  `    `     `                2       14 '
          H(13)=' weighting of aux`y sensors           2       16 '
          H(14)=' - Refer ESP-r Data Model section 9 '
          CALL EASKR(cm(1),' ',' No. of misc data items ',
     &      10.,'F',16.,'F',10.,'no. of misc data items',IER,14)

 153      WRITE(HOLD,'(4f8.0)')cm(2),cm(3),cm(4),cm(5)
          H(1)='Heat delivery is normally set high so that the '
          H(2)='target temperature is attained. The heating minimum '
          H(3)='is normally 0 but if > 0 then heating will continue'
          H(4)='at the minimum even when the temperature is'
          H(5)='reached. '
          H(6)='In the unlikely event that you want to control to'
          H(7)='another temperature but allow no cooling capacity '
          H(8)='the control may not work properly.'
          CALL EASKS(HOLD,
     &      ' Max & Min heat capacity, Max & Min cool capacity for',
     &      ' matching another temp: ',40,' 10000. 0. 10000. 0. ',
     &      ' track another max min heat cool',IER,8)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,999999.,'F','Max Heat',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,999999.,'F','Min Heat',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,999999.,'F','Max Cool',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,999999.,'F','Min Cool',IER)
          if(ier.ne.0)goto 153

          H(1)=' At present, this is limited to 2 auxiliary sensors'
          H(2)=' which may be located outside or in other zone(s).'
          CALL EASKR(cm(6),' ',' No of auxiliary sensors ? ',
     &      0.,'F',2.,'F',1.,'no. of auxiliary sensors',IER,2)

          H(1)=' The sensor modes options are:'
          H(2)=' 1: (ref sensor)-(greatest value of aux sensors)' 
          H(3)=' 2: (ref sensor)-(least value of aux sensors)'
          H(4)=' 3: (ref sensor)-(mean value of aux sensors)'
          H(5)=' 4: (ref sensor)-(weighting of aux sensors)'
          CALL EASKR(cm(7),' ',' Multi-sensor mode ? ',
     &      1.,'F',4.,'F',0.,' multi-sensor mode',IER,5)

C Auxiliary sensor details,
          I6=INT(cm(6))          
          H(1)='The sensor details are essentially the same as '
          H(2)='required in a control point sensor ie. '
          H(3)=' 1st item >0 is a zone and or surface, =0 any zone,'
          H(4)='   = -1 measuring plant component,  '
          H(5)='   = -2 measuring mix air:radiant,'
          H(6)='   = -3 ambient where the 2nd item is:'
          H(7)='        0 = db, 1 = sol-air, 2 = wind velo, '
          H(8)='        3 = wind dir, 4 = diffuse rad, '
          H(9)='        5 = direc norm rad, 6 = RH'
          h(10)=' '
          h(11)=' 2nd item is used to further specify the location '
          h(12)=' 3rd item is used to further specify the location '
          h(13)=' 4th item is always zero '
          H(14)=' - Refer ESP-r Data Model section 9 '
          DO 135 L=1,I6
            ndx=8+4*(L-1)

C Collect up the current information and generate a descriptive
C string to remind the user.
            icsn(1)=nint(cm(ndx))
            icsn(2)=nint(cm(ndx+1))
            icsn(3)=nint(cm(ndx+2))
            icsn(4)=nint(cm(ndx+3))
            SSTR=' '
            call decodesensor(0,icsn,SSTR,SSTRB,ier)
            call edisp(iuout,SSTR)
            CALL EASKR(cm(ndx),' ',' Aux sen detail 1st item ?',
     &        -3.,'F',0.,'-',0.,' Aux sen detail 1st item',IER,14)
            CALL EASKR(cm(ndx+1),' ',' Aux sen detail 2nd item ?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 2nd item',IER,14)          
            CALL EASKR(cm(ndx+2),' ',' Aux sen detail 3rd item ?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 3rd item',IER,14)
            CALL EASKR(cm(ndx+3),' ',
     &        ' Aux sen detail 4th item (always use zero)?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 4th item',IER,14)
135       continue 

          I7=INT(cm(7))
          if(I7.eq.4)then
           DO 136 L=1,I6

C Weighted auxiliary sensors mode,   
            ndx=12+(L-1)+4*(I6-1)
            H(1)=' The sensor weightings must total exactly 100.0'
            CALL EASKR(cm(ndx),' ',' Aux sensor weighting ? ',
     &       0.,'F',100.,'F',50.,' Aux sensor weighting',IER,1)
136        continue
          endif 
        elseif(IBCLAW(II,JJ,KK).eq.13)then

C Time proportioning control with separate on/off setpoints for heating
C and cooling (initial part of the input is similar to control 10).
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92

        elseif(IBCLAW(II,JJ,KK).eq.14)then

C Floating (`three-position') control.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92

        elseif(IBCLAW(II,JJ,KK).eq.15)then

C Logic Optimum Start Controller.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92
        elseif(IBCLAW(II,JJ,KK).eq.16)then

C Logic Optimum Stop Controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current Logic Stop controller data is:')
            call edisp(iuout,
     &      ' Htg cpy, Clg cpy, Htg Set pt, Clg Set pt, Dsrd temp')
            write(outs,'(5F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6)
            call edisp(iuout,outs)
            call edisp(iuout,' Throt rng, Occy Dep, Tim Dif, Init Gues')
            write(outs,'(4F9.2)')cm(7),cm(8),cm(9),cm(10)
            call edisp(iuout,outs)
            call edisp(iuout,' ')
          endif
          cm(1)=9.0
          CALL EASKR(cm(2),' ',' Maximum heating capacity (W) ? ',
     &      0.,'F',0.,'-',1000.,' max htg capacity',IER,0)
          CALL EASKR(cm(3),' ',' Maximum cooling capacity (W) ? ',
     &      0.,'F',0.,'-',1000.,' max clg capacity',IER,0)
          CALL EASKR(cm(4),' ',' Htg Set pt ? ',
     &      0.,'F',0.,'-',20.,' Htg Set pt',IER,0)
          CALL EASKR(cm(5),' ',' Clg Set pt ? ',
     &      0.,'F',0.,'-',20.,' Clg Set pt',IER,0)

          H(1)=' Desired temp is minimum temp at occupancy dept time'
          CALL EASKR(cm(6),' ',' Desired temp ? ',
     &      0.,'F',0.,'-',20.,' Desired temp',IER,1)
          CALL EASKR(cm(7),' ',' Throtl range ? ',
     &      0.,'F',0.,'-',2.,' Throtl range',IER,0)
          CALL EASKR(cm(8),' ',' Occpy dept time ? ',
     &      0.,'F',0.,'-',17.,' Occpy dept time',IER,0)

          H(1)=' If the difference between successive estimates is'
          H(2)=' less then the `Min. Time Diff`, then the latest'
          H(3)=' time is accepted as the `Optimum Stop Time`.'
          H(4)=' - Refer ESP-r Data Model section 9 '
          CALL EASKR(cm(9),' ',' Min time diff ? ',
     &      0.,'F',0.,'-',0.1,' Min time diff',IER,4)
          CALL EASKR(cm(10),' ',' Initial guess ? ',
     &      0.,'F',24.,'F',0.1,' Initial Guess',IER,0)
        elseif(IBCLAW(II,JJ,KK).eq.17)then

C Fuzzy Logic PI-PD Controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current Fuzzy Logic controller data is:')
            call edisp(iuout,
     &      ' Ref data set, Ctl mode, Set pt, Defuz meth')
            write(outs,'(4F9.2)')cm(2),cm(3),cm(4),cm(5)
            call edisp(iuout,outs)
            call edisp(iuout,' Scale(err), Scale(err rate), Scale(o/p)')
            write(outs,'(4F9.2)')cm(6),cm(7),cm(8)
            call edisp(iuout,outs)
            call edisp(iuout,' ')
          endif
          cm(1)=7.0
          H(1)=' Reference must be made to a fuzzy data set.'
          H(2)=' A fuzzy data set holds all the information about'
          H(3)=' the fuzzy membership functions. At least one such '
          H(4)=' set MUST exist if this control law is to used.'
          H(5)=' The data set is currently input by external'
          H(6)=' editorial facility.'
          H(7)=' Refer ESP-r Data Model section 9'
          CALL EASKR(cm(2),' ',' Referenced data set ? ',
     &      0.,'F',20.,'F',1.,' Ref data set',IER,7)

          H(1)=' The control mode nay be one of the following:'
          H(2)=' 1:= Fuzzy logic PI control;'
          H(3)=' 2:= Fuzzy logic PD control.'
          H(4)=' - Refer ESP-r Data Model section 9 '
          CALL EASKR(cm(3),' ',' Control Mode ? ',
     &      1.,'F',2.,'F',1.,' Ctl mode',IER,4)

          CALL EASKR(cm(4),' ',' Set point ? ',
     &    -100.,'F',100.,'F',20.,' Set point',IER,0)

          H(1)=' There is a choice of two defuzzification methods:'
          H(2)=' 1:= Mean-of-Max method;'
          H(3)=' 2:= Centre-of-Area method.'
          CALL EASKR(cm(5),' ',' Defuzzification method ? ',
     &      1.,'F',2.,'F',1.,' Defuz method',IER,3)

          H(1)=' This is used to tune the controller'
          CALL EASKR(cm(6),' ',' Error scale ? ',
     &    -100.,'F',100.,'F',1.,' Error Scale',IER,1)

          CALL EASKR(cm(7),' ',' Change of Error scale ? ',
     &    -100.,'F',100.,'F',1.,' Change of Error Scale',IER,1)

          CALL EASKR(cm(8),' ',' Output scale ? ',
     &    -100.,'F',100.,'F',1.,' Output Scale',IER,1)
        elseif(IBCLAW(II,JJ,KK).eq.18)then

C Null controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Null controller has no data.')
            call edisp(iuout,' ')
          endif
          cm(1)=0.0
        elseif(IBCLAW(II,JJ,KK).eq.19)then

C Ideal multi-sensor ON/OFF heating/cooling controller.
 243      WRITE(HOLD,'(4f8.0)')cm(2),cm(3),cm(4),cm(5)
          h(1)='A multi-sensor control is used to inject/extract heat'
          h(2)='in one zone (termed source) based on conditions in one'
          h(3)='or more locations (termed auxilliary sensors).'
          h(4)=' '
          h(5)='If  any of the auxilliary sensors demands heat then'
          h(6)='heat will be injected in the source. If any of the'
          h(7)='auxilliary sensors demand cooling then heat will be'
          h(8)='extracted from the source. If there are simultaneous'
          h(9)='demands for heating and cooling nothing will happen.'
          h(10)=' '
          h(11)='A classic use is to condition a mixing box/air handler'
          h(12)='used within an air flow network to deliver heating or '
          h(13)='cooling to occupied spaces. '
          h(14)=' '
          h(15)='If you are using a multi-sensor controller to enable'
          h(16)='a chilled ceiling (represented as a zone) or a radiant'
          h(17)='heating panel (represented as a zone) then these set'
          h(18)='points are the temperature of the working fluid in '
          h(19)='the radiant panel. Later you will be asked for the '
          h(20)='setpoints for the occupied space(s). '
          h(21)=' '
          h(22)='Example: to heat a radiant panel to 70C in order '
          h(23)='to get a temperature of 21C in room(s) below'
          h(24)='the multi-sensor control can be used. '
          h(25)=' '
          H(26)='Heat delivery may be less than the max. If minimium'
          H(27)='heating > 0 then heating will continue at a re- '
          H(28)='duced level even when the setpoint is reached. '
          H(29)='If there is no cooling then use zero values in '
          H(30)='conjunction with a hight cooling setpoint. '
          H(31)=' '
          H(32)='Note this type of control is an ON/OFF control and'
          H(33)='thus you should use an appropriately short timestep'
          H(34)='of one or two minutes if there is not much mass in'
          H(35)='the radiant panel or if there is a considerable air'
          H(36)='air flow rate. '
          H(37)=' '
          H(38)='Note: with the control law it is most common to set'
          H(39)='the sensor in the occupied space to be located at'
          H(40)='the zone air node (sensing remote mixed rad/conv is'
          H(41)='??) '
          H(42)=' '
          H(43)=' '
          H(44)='Also for both heating and cooling panels it is '
          H(45)='common to inject at the air node and to use a hc '
          H(46)='coefficient regime to set a high hc coef at the '
          H(47)='surfaces to ensure flux is transferred to the '
          H(48)='of the panel. '
          H(49)=' '
          H(50)='For a mixing box it is usually not necessary to '
          H(51)='use a hc regime in within the box. '

C First 7 data-items are equal to basic controller above.
          call edisp(iuout,' Ideal multi-sensor ON/OFF controller:')
          cm(1)=6.0
          CALL EASKS(HOLD,' Max & Min heat capacity, Max & Min cool ',
     &      ' capacity for basic control (see help): ',
     &      40,' 1000. 0. 1000. 0. ','max min heat cool',IER,51)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,999999.,'F','Max Heat',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,999999.,'F','Min Heat',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,999999.,'F','Max Cool',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,999999.,'F','Min Cool',IER)
          if(ier.ne.0)goto 243

 244      WRITE(HOLD,'(2f8.2)')cm(6),cm(7)
          h(1)='It you are using a multi-sensor controller to enable'
          h(2)='a chilled ceiling (represented as a zone) or a radiant'
          h(3)='heating panel (represented as a zone) then these set '
          h(4)='points are the temperature of the working fluid in '
          h(5)='the radiant panel. Later you will be asked for the '
          h(6)='setpoints for the occupied space. '
          h(7)=' '
          h(8)='Example: to heat a radiant panel to 70C in order '
          h(9)='to get a temperature of 21C in one or more room below'
          h(10)='the multi-sensor control can be used. '
          h(11)=' '
          H(12)='If you do not wish cooling use a high set point '
          H(13)='such as 100C. A 0 deg C cooling setpoint does not'
          H(14)='turn cooling off. '
          CALL EASKS(HOLD,
     &      'Heating & cooling setpoints within the zone where flux',
     &      'is injected or extracted (see help)',40,' 20. 24. ',
     &      'heat cool setpoints',IER,14)
          K=0
          CALL EGETWR(HOLD,K,cm(6),-100.,100.,'F','Heat setp',IER)
          CALL EGETWR(HOLD,K,cm(7),-100.,100.,'F','Cool setp',IER)
          if(ier.ne.0)goto 244

C If no cooling capacity then set cooling set point to 100C.
          CALL ECLOSE(cm(4),0.0,0.001,close1)
          CALL ECLOSE(cm(5),0.0,0.001,close2)
          if(close1.and.close2)then
            call edisp(iuout,' ')
            call edisp(iuout,' No cooling capacity defined - setting')
            call edisp(iuout,' the cooling to (100 deg.C).')
            cm(7)=100.
          endif

          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current `Ideal multi-sensor controller` data:')
            call edisp(iuout,
     &     ' No. Aux Sen')
            write(outs,'(1F9.2)')cm(8)
            call edisp(iuout,outs)
            
            I8=INT(cm(8))
            DO 140 L=1,I8              
              ndx=8+6*(L-1)
              call edisp(iuout,' Auxiliary Sensor Details')
              write(outs,'(6F9.2)')cm(ndx),cm(ndx+1),cm(ndx+2),
     &                             cm(ndx+3),cm(ndx+4),cm(ndx+5)
              call edisp(iuout,outs)

C Collect up the current information and generate a descriptive
C string to remind the user.
              icsn(1)=nint(cm(ndx))
              icsn(2)=nint(cm(ndx+1))
              icsn(3)=nint(cm(ndx+2))
              icsn(4)=nint(cm(ndx+3))
              SSTR=' '
              call decodesensor(0,icsn,SSTR,SSTRB,ier)
              call edisp(iuout,SSTR)
140         continue   
          endif

          H(1)= ' The max number of auxiliary sensors is MSEN'
          H(2)= ' '
          H(3)= ' The number of data items = 7 + 6 * no. of sensors'
          H(4)= ' For each auxiliary sensor, 6 items are needed:'
          H(5)= ' 4 items for the sensor location and two setpoints.'
          H(6)= ' '
          H(7)= ' Example: to sense air temperature is 3rd zone which'
          H(8)= '    needs to be heated to 21.1C and no cooling'
          H(9)= '    3.0 0.0 0.0 0.0 21.1 100.0 '
          H(10)='  '
          H(11)=' So it is worth gathering information on the location'
          H(12)=' of auxillary sensors before answering any further'
          H(13)=' question abouts this control and carefully'
          H(14)=' checking the data/QA report afterwards.'
          H(15)=' '
          H(16)='If you choose cancel the controller data will not be'
          H(17)='altered and you will be returned to the main menu. '

          I8=int(cm(8))
          CALL EASKI(I8,' ',' Number of aux sensors (see help)',
     &      0,'F',MSEN,'F',3,'no. of aux sensors',IERI,17)
          if(ieri.eq.-3) goto 92
          cm(1)=7.+6.*I8
          cm(8)=float(I8)

C Auxiliary sensor details,
          H(1)='The sensor details are essentially the same as '
          H(2)='required in a control point sensor ie. '
          H(3)=' 1st item >0 is a zone and or surface, =0 any zone,'
          H(5)='   = -2 measuring mix air:radiant,'
          H(6)='   = -3 ambient where the 2nd item is:'
          H(7)='        0 = db, 1 = sol-air, 2 = wind velo, '
          H(8)='        3 = wind dir, 4 = diffuse rad, '
          H(9)='        5 = direc norm rad, 6 = RH'
          H(10)=' '
          H(11)= ' Example: to sense air temperature is 3rd zone'
          H(12)= '    3.0  0.0 0.0  '
          H(13)= ' '
          H(14)='This data is followed - per sensor - by the'
          H(15)='heating set-point and cooling set-point for'
          H(16)='that auxiliary sensor.'
          DO 141 L=1,I8
            ndx=9+6*(L-1)

C Collect up the current information and generate a descriptive
C string to remind the user.
            icsn(1)=nint(cm(ndx))
            icsn(2)=nint(cm(ndx+1))
            icsn(3)=nint(cm(ndx+2))
            icsn(4)=nint(cm(ndx+3))
            SSTR=' '
            call decodesensor(0,icsn,SSTR,SSTRB,ier)
            call edisp(iuout,SSTR)
            call edisp(iuout,' ')
            WRITE(outs,'(a,i2,a)')' For aux sensor ',L,' define....'
            call edisp(iuout,outs)
            CALL EASKR(cm(ndx),' ',' Aux sensor detail 1st item ?',
     &        -3.,'F',0.,'-',1.,' Aux sensor detail 1st item',IER,16)
            CALL EASKR(cm(ndx+1),' ',' Aux sensor detail 2nd item ?',
     &        0.,'F',0.,'-',0.,' Aux sensor detail 2nd item',IER,16)
            CALL EASKR(cm(ndx+2),' ',' Aux sensor detail 3rd item ?',
     &        0.,'F',0.,'-',0.,' Aux sensor detail 3rd item',IER,16)
            CALL EASKR(cm(ndx+3),' ',
     &        ' Aux sensor detail 4th item (always use zero)?',
     &        0.,'F',0.,'-',0.,' Aux sensor detail 4th item',IER,16)

            CALL EASKR(cm(ndx+4),' ',' Aux sensor heating set-point ?',
     &        0.,'F',0.,'-',0.,' Aux sensor heating set-point',IER,16)

            CALL EASKR(cm(ndx+5),' ',' Aux sensor cooling set-point ?',
     &        0.,'F',0.,'-',0.,' Aux sensor cooling set-point',IER,16)
141       continue
 
        elseif(IBCLAW(II,JJ,KK).eq.20)then

C Latent/evaporative source, see bcl20 for details.
          mode=int(cm(2))
          Abasin=cm(3)
          alpha= cm(4)
          alphar= cm(5)    
          tbas=  cm(6) 
          tbasex= cm(7) 
          fsupply= cm(8) 
          tsupply= cm(9)   
          abasw= cm(10)  
          ubasw= cm(11) 

C Ask for data.
          h(1)='A wetted surface can either be a wet opaque surface '
          h(2)='in the zone or the floor of the zone can' 
          h(3)='contain a swimming pool, some additional data is '
          h(4)='required from user for this option.'
          CALL EASKAB('Wetted surface options:',' ',
     &    'Wet opaque surface','Swimming pool surface',IW,4)
          mode = IW-1
          if(mode.eq.0)then
C Surface area and HTC picked up from ESP-r
            cm(1)=1.0
            cm(2)=0.0
          elseif(mode.eq.1)then
            h(1)='You have chosen to incorporate a swimming pool model'
            h(2)='into this zone. This requires that 1) the floor' 
            h(3)='internal convective heat transfer coefficient and '  
            h(4)='emissivity are set to small values (i.e. 0.001) '
            h(5)='and that 2) the external boundary condition is set '
            h(6)='to adiabatic. Please do this after defining the pool' 
            h(7)='data.'
            call phelpd('pool control help',7,'-',0,0,IER)       
            h(1)='Data requirements: '
            h(2)=' 1) pool surface area m^2'
            h(3)=' 2) pool surface convective htc W/m^2K'
            h(4)=' 3) pool surface radiative htc W/m^2K'
            h(5)=' 4) pool set point temperature degC'
            h(6)=' 5) surrounding ground temperature degC'
            h(7)=' 6) pool water supply rate kg/s'
            h(8)=' 7) pool water supply temperature degC'
            h(9)= ' 8) submerged wall surface area m^2'
            h(10)= ' 9) submerged wall surface U-value W/m^2'

            write(hold,'(3(f10.4,2X))') Abasin,alpha,alphar
            CALL EASKS(HOLD,'Pool surface area(m^2), convective and',
     &        'radiant heat trans coefts (W/m^2.K)',40,
     &        '100.0   1.5   5.1',
     &        ' surf area and coefts',IER,2)
            K=0
            CALL EGETWR(HOLD,K,Abasin,0.,9999.,'W','Abasin',IER)
            CALL EGETWR(HOLD,K,alpha,0.,99.,'W','alpha',IER)
            CALL EGETWR(HOLD,K,alphar,0.,99.,'W','alphar',IER)
            cm(1)=10.0
            cm(2)=1.0
            cm(3)=Abasin
            cm(4)=alpha
            cm(5)=alphar
            write(hold,'(2(f10.4,2X))') tbas,tbasex
            CALL EASKS(HOLD,'Pool temperature and ground temperature',
     &        '(degC)',40,
     &        '27.0 12.0',
     &        ' pool and ground temperatures',IER,2)
            K=0
            CALL EGETWR(HOLD,K,tbas,0.,99.,'W','tbas',IER)
            CALL EGETWR(HOLD,K,Tbasex,0.,99.,'W','tbasex',IER)
            cm(6)=tbas
            cm(7)=tbasex
            write(hold,'(2(f10.4,2X))') fsupply,tsupply
            CALL EASKS(HOLD,'water supply rate (kg/s), water supply ',
     &        'temp (degC)',40,
     &        '5.0 25.0',
     &        ' water data ',IER,2)   
            k=0
            CALL EGETWR(HOLD,K,fsupply,0.,9999.,'W','fsupply',IER)
            CALL EGETWR(HOLD,K,tsupply,0.,9999.,'W','tsupply',IER)
            cm(8)=fsupply
            cm(9)=tsupply  
            write(hold,'(2(f10.4,2X))') abasw,ubasw
            CALL EASKS(HOLD,'submerged wall surface area (m^2), ',
     &        'submerged wall U-value (W/m^2K)',40,
     &        '100.0 0.3',
     &        ' water data ',IER,2)   
            k=0
            CALL EGETWR(HOLD,K,abasw,0.,9999.,'W','abasw',IER)
            CALL EGETWR(HOLD,K,ubasw,0.,9999.,'W','ubasw',IER)
            cm(10)=abasw
            cm(11)=ubasw                     
          endif
        
        elseif(IBCLAW(II,JJ,KK).eq.21.or.IBCLAW(II,JJ,KK).eq.22)then
 
C Slave Capacity Controller or VAV controller.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92
        elseif(IBCLAW(II,JJ,KK).eq.23)then  

C Read setpoints from temporal file. This control law was developed for
C the UK NCM and takes in setpoints from tdf data and is based on the
C logic on control law 1
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawT=ier
            goto 77
          endif
          goto 92
        elseif(IBCLAW(II,JJ,KK).eq.24)then  

C Adaptive human comfort model. (Same algorithm as used within the flow
C domain therefore refer to same reference publication)
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawT=ier
            goto 77
          endif
          goto 92
        else

C The fall-through (none of the above) control edit.
          H(1)='See ESP-r Data Model section 9 for details. '
          H(2)=' '
          H(3)='If you choose cancel the controller data will not be'
          H(4)='altered and you will be returned to the main menu. '
          CALL EASKI(NITEMS,' ',' No of control law data items ?',
     &      0,'F',MISC,'F',1,'control law data items',IERI,4)
          if(ieri.eq.-3) goto 92
          cm(1)=FLOAT(NITEMS)
          if(NITEMS.GT.0)then
            do 33 MM=2,NITEMS+1
              write(outs,'(A,I3)')' Item ',MM-1
              CALL EASKR(cm(MM),outs,' value: ',
     &          0.,'-',0.,'-',1.,'miscel data',IER,1)
  33        continue
          endif
        endif  

C Extract from working array to bmiscd() common structure
        call extrctl(0,II,JJ,KK)
        call usrmsg(' ',' ','-')
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

 3    if(IOS.eq.2)then
        CALL USRMSG(' ',
     &    'Permission issue writing miscellaneous data.','W')
      else
        CALL USRMSG(' ','Problem writing miscellaneous data.','W')
      endif
      return

      END

C *********** EPCNTRL
C Edit plant control function II.
      SUBROUTINE EPCNTRL(ITRC,II,IV,IER)
#include "building.h"
#include "plant.h"
#include "DG_controller_include.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      common/pctl/ncl,ipsn(mcl,5),ipan(mcl,4),npcdt(mcl),
     &       ipcdv(mcl,mpcdt,2),npcdp(mcl,mpcdt),tpcps(mcl,mpcdt,mpcdp),
     &       ipctyp(mcl,mpcdt,mpcdp),ipclaw(mcl,mpcdt,mpcdp),
     &       pmiscd(mcl,mpcdt,mpcdp,misc)
      common/sctl/tcps,ictyp,iclaw,cm(misc)
      DIMENSION SALT(42),IVALS(42),PSALT(17),IPVALS(17)
      CHARACTER*84 VERT(20)

      CHARACTER outs*124,H*72,SSTR*72,TMP*72,SALT*40,PSALT*40,KEY*1
      character outsa*84,outsb*84,outsc*84,ltmp*248

      CHARACTER hold*60
      logical close1


C Number of plant loops and day type must be > 0.
      IF(II.LE.0.OR.IV.LE.0)RETURN

C Control day types.
      J=IV
      JJ=J

C Initial menu entry setup.
   92 IER=0
      IVERT=-3
      WRITE(vert(1),'(a,i3,a,i3)')' function',II,' day type',JJ
      WRITE(vert(2),'(a,i3)')     ' number of periods:',NPCDP(II,JJ)
      WRITE(vert(3),'(a)') ' ________________________________________'
      WRITE(vert(4),'(a)')
     &' per| start|sensed  |actuated | control law     | data'
      WRITE(vert(5),'(a)')
     &' no.| time |property|property |                 |     '
      LL=NPCDP(II,JJ)

C Generate menu entry for each period in day type.
      m=5
      DO 90 KP=1,LL
        KK=KP
        m=m+1
        call stfctl(1,II,JJ,KK)
        CALL EVCNTRL(1,II,JJ,KK,'T',SSTR)
        CALL EVCNTRLAW(1,II,JJ,KK,TMP,LTMP)
        NITEMS=INT(cm(1))
        WRITE(outsa,'(i2,F7.2,1x,a,1x,a)')KK,tcps,SSTR(1:18),TMP(1:18)
        outsb=' '
        outsc=' '
        if(NITEMS.gt.0)then
          if(NITEMS.LE.9)then
            write(outsb,'(9F9.1)')(cm(L),L=2,NITEMS+1)
          else
            write(outsb,'(9F9.1)')(cm(L),L=2,10)
          endif
          call SDELIM(outsb,outsc,'S',IW)
        endif
        CALL EMKEY(M-5,KEY,IER)
        write(vert(m),'(1a,1x,a,1x,a)',IOSTAT=IOS,ERR=3) key,
     &    outsa(1:47),outsc(1:34)
  90  continue
      m=m+1
      WRITE(vert(m),'(a)') ' ________________________________________'
      m=m+1
      WRITE(vert(m),'(a)') '* add/ delete a period '
      m=m+1
      WRITE(vert(m),'(a)') '? help '
      m=m+1
      WRITE(vert(m),'(a)') '- exit  '
      mvert=m

C Help text for this menu.
      h(1) ='A number of control periods can be defined for each'
      h(2) ='control day type. Each period has a start time (i.e.'
      h(3) ='they do not overlap), a sensed property, an actuated'
      h(4) ='property, a control law and miscel. data which defines'
      h(5) ='capacity, setpoints, etc.'
      h(6) ='  '
      h(7) ='Note: if no change required in an item then accept the'
      h(8) ='current value.'

C Now display the menu.
      CALL EMENU('Control periods',VERT,MVERT,IVERT)
      if(IVERT.EQ.mvert)then
        return
      elseif(IVERT.EQ.mvert-1)then

C Explanation
        CALL PHELPD('control period help',8,'-',0,0,IER)
      elseif(IVERT.EQ.mvert-2)then

C Add or delete a period.
        h(1)='Periods are added to the end of the list. '
        CALL EASKABC('Options:',' ',
     &    'add period','delete period','continue',iad,1)
        if(iad.eq.1)then
          call addctldp(1,II,JJ,'A',iier)
        elseif(iad.eq.2)then
          call addctldp(1,II,JJ,'D',iier)
        endif
        goto 92
      ELSEIF(IVERT.GT.5.AND.IVERT.LT.(mvert-3))THEN

C Take the t?cps() i?ctyp(),i?claw(), ?MISCD(II,JJ,KK,?) common and
C stuff into working common array 'cm' for editing.  At end
C of if:elseif:else recover.
        KK=IVERT-5
        call stfctl(1,II,JJ,KK)
        NITEMS=INT(cm(1))
        if(NITEMS.gt.0)then
          WRITE(outs,'(a,i3,a)')' For period',KK,' current data is...'
          call edisp(iuout,outs)
          if(NITEMS.LE.13)then
            write(outs,'(13F9.1)')(cm(L),L=2,NITEMS+1)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
          else
            write(outs,'(13F9.1)')(cm(L),L=2,13)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
            if(NITEMS.LE.26)then
              write(outs,'(13F9.1)')(cm(L),L=14,NITEMS)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            else
              write(outs,'(13F9.1)')(cm(L),L=14,26)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            endif
          endif
        else
          WRITE(outs,'(a,i3,a)')' For period',KK,' define...'
          call edisp(iuout,outs)
        endif

        SALT(1)= 'senses dry bulb           actuates flux'
        SALT(2)= 'senses dry bulb           actuates flow'
        SALT(3)= 'senses enthalpy           actuates flux'
        SALT(4)= 'senses enthalpy           actuates flow'
        SALT(5)= 'senses 1st ph mass flow   actuates flux'
        SALT(6)= 'senses 1st ph mass flow   actuates flow'
        SALT(7)= 'senses 2nd ph mass flow   actuates flux'
        SALT(8)= 'senses 2nd ph mass flow   actuates flow'
        SALT(9)= 'senses adl plant output   actuates flux'
        SALT(10)='senses adl plant output   actuates flow'
        SALT(11)='senses RH                 actuates flux'
        SALT(12)='senses RH                 actuates flow'
        SALT(13)='senses dry bulb       actuates variable'
        SALT(14)='senses enthalpy       actuates variable'
        SALT(15)='senses 1st ph flow    actuates variable'
        SALT(16)='senses 2nd ph flow    actuates variable'
        SALT(17)='senses plant output   actuates variable'
        SALT(18)='senses RH             actuates variable'
        SALT(19)='senses dry bulb     actuates mass ratio'
        SALT(20)='senses enthalpy     actuates mass ratio'
        SALT(21)='senses 1st ph flow  actuates mass ratio'
        SALT(22)='senses 2nd ph flow  actuates mass ratio'
        SALT(23)='senses plant o/p    actuates mass ratio'
        SALT(24)='senses RH           actuates mass ratio'
        SALT(25)='senses temp diff          actuates flow'
        SALT(26)='senses abs temp diff      actuates flow'
        SALT(27)='senses pressure           actuates flow'
        SALT(28)='senses press diff         actuates flow'
        SALT(29)='senses abs press diff     actuates flow'
        SALT(30)='senses abs mass flow      actuates flow'
        SALT(31)='senses wind speed         actuates flow'
        SALT(32)='senses wind direction     actuates flow'
        SALT(33)='senses dir nor sol rad    actuates flow'
        SALT(34)='senses dif hor sol rad    actuates flow'
        SALT(35)='senses ext rel hum        actuates flow'
        SALT(36)='senses wind speed         actuates flux'
        SALT(37)='senses wind direction     actuates flux'
        SALT(38)='senses dir nor sol rad    actuates flux'
        SALT(39)='senses dif hor sol rad    actuates flux'
        SALT(40)='senses ext rel hum        actuates flux'
        SALT(41)='senses contaminant conc   actuates flow'
        SALT(42)='senses control loops    actuates either '

        call edisp(iuout,' Current controller type: ')
        call edisp(iuout,SALT(IPCTYP(II,JJ,KK)+1))
        IX=1
        H(1)='See ESP-r Data Model section 9 for details. '
        H(2)='Broadly, you want to select based on what is to'
        H(3)='be actuated and what is being sensed. This information'
        H(4)='will be used later when setting the control actions.'
        CALL EPICKS(IX,IVALS,' ',' Controller type: (see help)',
     &         40,42,SALT,'controller type',IER,4)
        if(ix.ne.0)IPCTYP(II,JJ,KK)=IVALS(1)-1
        ictyp=IPCTYP(II,JJ,KK)

C Plant contol law.
        PSALT(1)= 'Period switch off control'
        PSALT(2)= 'P,PI,PID for type 0,2,4,6,8,10 controlr'
        PSALT(3)= 'P,PI,PID for type 1,3,5,7,9,11 controlr'
        PSALT(4)= 'Proportional action for type 12-17 cntr'
        PSALT(5)= 'Optimum start for flux/variable control'
        PSALT(6)= 'Proportional action for type 18-23 cntr'
        PSALT(7)= 'Null controller    '
        PSALT(8)= 'Duty cycle controller'
        PSALT(9)= 'On-off controller'
        PSALT(10)='Multi-sensor on-off controller'
C DG controller
        PSALT(11)='CCHP Cogeneration system controller'
C        PSALT(12)='Hydrogen demand controller'
C        PSALT(13)='Derive signal from boundary condition'
C        PSALT(14)='Adsorption storage unit controller'
C Multisensor for loops (control type is 
C determined in loops that this sensor senses)
        PSALT(12)='Multi-sensor (senses loops)'
        PSALT(13)='PI Room Control'
        PSALT(14)='Outside temperature compensation control'

        write(outs,'(a,a)')' Current control law: ',
     &                      PSALT(IPCLAW(II,JJ,KK)+1)
        call edisp(iuout,outs)
        IX=1
        H(1)='The `type` numbers relate to the selection you made'
        H(2)='in the previouos step. Here is a reminder: '
        H(3)=' '
        H(4)=' type 0 is senses dry bulb actuates flux'
        H(5)=' type 1 is senses dry bulb actuates flow'
        H(6)=' type 2 is senses enthalpy actuates flux'
        H(7)=' type 3 is senses enthalpy actuates flow'
        H(8)=' type 4 is senses 1st ph mass flow actuates flux'
        H(9)=' type 5 is senses 1st ph mass flow actuates flow'
        H(10)=' type 6 is senses 2nd ph mass flow actuates flux'
        H(11)=' type 7 is senses 2nd ph mass flow actuates flow'
        H(12)=' type 8 is senses adl plant output actuates flux'
        H(13)=' type 9 is senses adl plant output actuates flow'
        H(14)=' type 10 is senses RH actuates flux'
        H(15)=' type 11 is senses RH actuates flow'
        H(16)=' type 12 is senses dry bulb actuates variable'
        H(17)=' type 13 is senses enthalpy actuates variable'
        H(18)=' type 14 is senses 1st ph flow actuates variable'
        H(19)=' type 15 is senses 2nd ph flow actuates variable'
        H(20)=' type 16 is senses plant output actuates variable'
        H(21)=' type 17 is senses RH actuates variable'
        H(22)=' type 18 is senses dry bulb actuates mass ratio'
        H(23)=' type 19 is senses enthalpy actuates mass ratio'
        H(24)=' type 20 is senses 1st ph flow actuates mass ratio'
        H(25)=' type 21 is senses 1st ph flow actuates mass ratio'
        H(26)=' type 22 is senses plant o/p actuates mass ratio'
        H(27)=' type 23 is senses RH actuates mass ratio'
        H(28)=' type 24 is senses temp diff actuates flow'
        H(29)=' type 25 is senses abs temp diff actuates flow'
        H(30)=' type 26 is senses pressure actuates flow'
        H(31)=' type 27 is senses press diff actuates flow'
        H(32)=' type 28 is senses abs press diff actuates flow'
        H(33)=' type 29 is senses abs mass flow actuates flow'
        H(34)=' type 30 is senses wind speed actuates flow'
        H(35)=' type 31 is senses wind direction actuates flow'
        H(36)=' type 32 is senses dir nor sol rad actuates flow'
        H(37)=' type 33 is senses dif hor sol rad actuates flow'
        H(38)=' type 34 is senses ext rel hum actuates flow'
        H(39)=' type 35 is senses wind speed actuates flux'
        H(40)=' type 36 is senses wind direction actuates flux'
        H(41)=' type 37 is senses dir nor sol rad actuates flux'
        H(42)=' type 38 is senses dif hor sol rad actuates flux'
        H(43)=' type 39 is senses ext rel hum actuates flux'
        H(44)=' type 33 is senses contaminant conc actuates flow'
        H(45)=' type 41 is senses other loops actuates either'
        H(46)='See ESP-r Data Model section 9 for details. '

 910    IF(IPCTYP(II,JJ,KK).NE.41)THEN
          CALL EPICKS(IX,IPVALS,' ',' Plant control law: ',
     &         40,14,PSALT,'control law',IER,46)
          if(ix.ne.0)iclaw=IPVALS(1)-1

C Skip controllers (11-13) that are not included in the interface
          IF(ICLAW.GE.11)ICLAW=ICLAW+3
        ELSE
          iclaw=14
        ENDIF
        IPCLAW(II,JJ,KK)=iclaw
        IF(IPCTYP(II,JJ,KK).NE.41.AND.IPCLAW(II,JJ,KK).EQ.14)THEN
          H(1)='This control type only works with multisensing of'
          H(2)='control loops. The control type will be set to '
          H(3)='sensing other control loops action (type 41) '
          CALL PHELPD('control type help',3,'-',0,0,IER)
          IPCTYP(II,JJ,KK)=41
        ENDIF

        IF(ICLAW.LT.14.AND.ICLAW.GT.10)GOTO 63
  62    H(1)='See ESP-r Data Model section 9 for details. '
        CALL EASKR(tcps,' ',' Period start time ? ',
     &    0.,'F',24.,'F',0.,'plant control start',IER,1)
        if(KK.GT.1)then
          IF(TPCPS(II,JJ,KK-1).GE.tcps)then
            call usrmsg('Periods out of order..',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcps,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0.','W')
            GOTO 62
          endif
        endif

C Make assumptions about the additional data that will be required.
 63     H(1)='See ESP-r Data Model section 9 for details. '
        if(IPCLAW(II,JJ,KK).eq.0)then
           
C Period switch off control.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Period switch off control has no data.')
            call edisp(iuout,' ')
          endif
          cm(1)=0.0
        elseif(IPCLAW(II,JJ,KK).eq.1)then

C PID flux controller.
          if(itrc.gt.0)then
            call edisp(iuout,' Current PID flux control data is:')
            call edisp(iuout,' Mode, Max Htg/Clg W, Min Htg/Clg W,  
     &       Htg/Clg SP, Htg/Clg TR')
            write(outs,'(5F10.3)')cm(2),cm(3),cm(4),cm(5),cm(6)
            call edisp(iuout,outs)
            
            I2=INT(cm(2))
            if(I2.eq.1.or.I2.eq.-1.or.I2.eq.2.or.I2.eq.-2)then
              I7=INT(cm(7))
              if(I7.eq.1)then
                call edisp(iuout,' Integral action time [secs]')
                write(outs,'(F9.2)')cm(8)
                call edisp(iuout,outs)
                I9=INT(cm(9))
                if(I9.eq.1)then
                  call edisp(iuout,' Derivative action time [secs]')
                  write(outs,'(F9.2)')cm(10)
                  call edisp(iuout,outs)
                endif
              else
                I8=INT(cm(8))
                if(I8.eq.1)then
                  call edisp(iuout,' Derivative action time [secs]')
                  write(outs,'(F9.2)')cm(9)
                  call edisp(iuout,outs)
                endif
              endif              
            elseif(I2.eq.3.or.I2.eq.-3)then
                call edisp(iuout,' Integral action time [secs]')
                write(outs,'(F9.2)')cm(7)
                call edisp(iuout,outs)                  
            elseif(I2.eq.4.or.I2.eq.-4.or.I2.eq.5.or.I2.eq.-5)then
               call edisp(iuout,' Int act time [s], Drv act time[s] ')
               write(outs,'(2F9.2)')cm(7),cm(8)
               call edisp(iuout,outs)                  
             endif
          endif

C Define mode, number of items, std items, then mode specific items.
          H(1)=' Mode options are:'
          H(2)=' 1: P, PI, PD, PID non-recursive positional control'
          H(3)=' 2: P, PI, PD, PID recursive positional control'
          H(4)=' 3: PI recursive vel`y control (rect. integration)'          
          H(5)=' 4: PID recursive vel`y control (rect. integration)'
          H(6)=' 5: PID recursive vel`y control(trapez integration)'
          H(7)=' Note: negate mode number for cooling'
          H(8)=' - Refer ESP-r Data Model section 9 '
          CALL EASKR(cm(2),' ',' PID mode ? ',
     &      -5.,'F',5.,'F',1.,' PID mode',IER,8)
          
          H(1)=' The number of data items required is in the range'
          H(2)=' 6 to 9 and depends on the PID mode selected, i.e.,'
          H(3)=' For modes 1, -1 , 2, -2 :- '
          H(4)='     Positional P              -   7 items required,'
          H(5)='     Positional PI             -   8 items required,'
          H(6)='     Positional PD             -   8 items required,'
          H(7)='     Positional PID            -   9 items required,'
          H(8)=' For mode 3, -3:-'
          H(9)='     Rect. Velocity PID        -   6 items required,'
          H(10)=' For mode 4, -4, 5, -5:-'
          H(11)='    Trap. Velocity PID        -   7 items required,'
          H(12)=' NOTE: if `AHU dynamic set point` option '
          H(13)=' selected (set point = -1001) then add 7 data'
          H(14)=' items to above.'
          H(15)=' - Refer ESP-r Data Model section 9 '
          CALL EASKR(cm(1),' ',' No. of misc data items ?',
     &      6.,'F',16.,'F',7.,'no. of misc data items',IER,15)

 11      WRITE(HOLD,'(2f8.0,2f7.2)')cm(3),cm(4),cm(5),cm(6)
          H(1)='Htg/clg delivery may be less than the max. If min'
          H(2)='htg/clg > 0 then htg/clg will continue at a re- '
          H(3)='duced level even when the setpoint is reached. '
          H(4)=' '
          H(5)='The set point may be specified to be :-' 
          H(6)='(a) a fixed constant value;'
          H(7)=' or'
          H(8)='(b) determined automatically, in a dynamic manner,'
          H(9)='    at each simulation times-step, by means of a '
          H(10)='   psychometric-based algorithm within the program.'
          H(11)='The latter method (denoted by a setpoint of -1001)'
          H(12)='should be used when simulating air conditioning '
          H(13)='systems.'
          H(14)=' Refer ...../esp-r/training/plant/AHU/humidity_ctl'
          H(15)=' training folder.'
          CALL EASKS(HOLD,
     &      ' Max htg/clg cap [W], Min htg/clg cap [W], setpoint,',
     &      ' throtl range for PID ctl: ',60,' 1000. 0. 20. 2. ',
     &      'PID max min stpnt throt',IER,15)
          K=0
         CALL EGETWR(HOLD,K,cm(3),0.,999999.,'F','Mx htg/clg',IER)
         CALL EGETWR(HOLD,K,cm(4),0.,999999.,'F','Min Ht/clg',IER)
         CALL EGETWR(HOLD,K,cm(5),-1001.,100.,'F','Set point',IER)
         CALL EGETWR(HOLD,K,cm(6),0.,999999.,'F','Throt rnge',IER)
          if(ier.ne.0)goto 11

          I2=INT(cm(2))
          if(I2.eq.1.or.I2.eq.-1.or.I2.eq.2.or.I2.eq.-2)then
          H(1)=' Int. action flag: 1 if required; 0 if not required.'
            CALL EASKR(cm(7),' ',' Integral action flag?',
     &        0.,'F',1.,'-',0.,' integral action flag',IER,1)
            I7=INT(cm(7))
            if(I7.eq.1)then
            CALL EASKR(cm(8),' ',' Integral action time [sec]?',
     &        1.,'F',0.,'-',900.,' integral action time',IER,0)
            H(1)=' Derv. action flag: 1 if required; 0 if not req`d.'
            CALL EASKR(cm(9),' ',' derivative action flag?',
     &        0.,'F',1.,'-',0.,' derivative action flag',IER,1)
              I9=INT(cm(9))
              if(I9.eq.1)then
                CALL EASKR(cm(10),' ',' Derivative action time [sec]?',
     &            1.,'F',0.,'-',500.,' derivative action time',IER,0)
              endif
            else
              H(1)=' Drv. action flag: 1 if required; 0 if not rqd.'
              CALL EASKR(cm(8),' ',' derivative action flag?',
     &          0.,'F',1.,'-',0.,' derivative action flag',IER,1)
                I8=INT(cm(8))
                if(I8.eq.1)then
                  CALL EASKR(cm(9),' ',' Derivative action time [sec]?',
     &             1.,'F',0.,'-',500.,' derivative action time',IER,0)
                endif
            endif
          elseif(I2.eq.3.or.I2.eq.-3)then
            CALL EASKR(cm(7),' ',' Integral action time [secs]?',
     &          1.,'F',0.,'-',900.,' integral action time',IER,0)
          elseif(I2.eq.4.or.I2.eq.-4.or.I2.eq.5.or.I2.eq.-5)then
            CALL EASKR(cm(8),' ',' Integral action time [secs]?',
     &          1.,'F',0.,'-',900.,' integral action time',IER,0)
            CALL EASKR(cm(11),' ',' Derivative action time [secs]?',
     &           1.,'F',0.,'-',500.,' derivative action time',IER,0)
          endif
        elseif(IPCLAW(II,JJ,KK).eq.2)then

C PID flow controller.
          if(itrc.gt.0)then
            call edisp(iuout,' Current PID flow control data is:')
            call edisp(iuout,' Mode, Max flow [kg/s], Min flow [kg/s],
     &           setpoint, throt rng')
            write(outs,'(5F10.3)')cm(2),cm(3),cm(4),cm(5),cm(6)
            call edisp(iuout,outs)
            
            I2=INT(cm(2))
            if(I2.eq.1.or.I2.eq.-1.or.I2.eq.2.or.I2.eq.-2)then
              I7=INT(cm(7))
              if(I7.eq.1)then
                call edisp(iuout,' Integral action time [secs]')
                write(outs,'(F9.2)')cm(8)
                call edisp(iuout,outs)
                I9=INT(cm(9))
                if(I9.eq.1)then
                  call edisp(iuout,' Derivative action time [secs]')
                  write(outs,'(F9.2)')cm(10)
                  call edisp(iuout,outs)
                endif
              else
                I8=INT(cm(8))
                if(I8.eq.1)then
                  call edisp(iuout,' Derivative action time [secs]')
                  write(outs,'(F9.2)')cm(9)
                  call edisp(iuout,outs)
                endif
              endif              
            elseif(I2.eq.3.or.I2.eq.-3)then
                call edisp(iuout,' Integral action time [secs]')
                write(outs,'(F9.2)')cm(7)
                call edisp(iuout,outs)                  
            elseif(I2.eq.4.or.I2.eq.-4.or.I2.eq.5.or.I2.eq.-5)then
               call edisp(iuout,' Int act time [s], Drv act time[s] ')
               write(outs,'(2F9.2)')cm(7),cm(8)
            endif
          endif

C Define mode, number of items, std items, then mode specific items.
          H(1)=' Mode options are:'
          H(2)=' 1: P, PI, PD, PID non-recursive positional control'
          H(3)=' 2: P, PI, PD, PID recursive positional control'
          H(4)=' 3: PI recursive vel`y control (rect. integration)'          
          H(5)=' 4: PID recursive vel`y control (rect. integration)'
          H(6)=' 5: PID recursive vel`y control(trapez integration)'
          H(7)=' Note: negate mode number for cooling'
          H(8)=' - Refer ESP-r Data Model section 9 '
          CALL EASKR(cm(2),' ',' PID mode? ',
     &      -5.,'F',5.,'F',1.,' PID mode',IER,8)
          
          H(1)=' The number of data items required is in the range'
          H(2)=' 6 to 9 and depends on the PID mode selected, i.e.,'
          H(3)=' For modes 1, -1 , 2, -2 :- '
          H(4)='     Positional P              -   7 items required,'
          H(5)='     Positional PI             -   8 items required,'
          H(6)='     Positional PD             -   8 items required,'
          H(7)='     Positional PID            -   9 items required,'
          H(8)=' For mode 3, -3:-'
          H(9)='     Rect. Velocity PID        -   6 items required,'
          H(10)=' For mode 4, -4, 5, -5:-'
          H(11)='    Trap. Velocity PID        -   7 items required,'
          H(12)=' NOTE: if `AHU dynamic set point` option '
          H(13)=' selected (set point = -1001) then add 7 data'
          H(14)=' items to above.'
          H(15)=' - Refer ESP-r Data Model section 9 '
          CALL EASKR(cm(1),' ',' No. of misc data items ?',
     &      6.,'F',16.,'F',7.,'no. of misc data items',IER,15)

 21      WRITE(HOLD,'(2f8.0,2f7.2)')cm(3),cm(4),cm(5),cm(6)
          H(1)='The set point may be specified to be :-' 
          H(2)='(a) a fixed constant value;'
          H(3)=' or'
          H(4)='(b) determined automatically, in a dynamic manner,'
          H(5)='    at each simulation times-step, by means of a '
          H(6)='    psychometric-based algorithm within the program.'
          H(7)='The latter method (indicated by a setpoint of -1001)'
          H(8)='should be used when simulating air conditioning '
          H(9)='systems.'
          H(10)=' Refer ...../esp-r/training/plant/AHU/humidity_ctl'
          H(11)=' training folder.'
          CALL EASKS(HOLD,
     &      ' Max flow [kg/s], Min flow [kg/s], setpoint,',
     &      ' throtl range for PID ctl: ',60,' 0.01 0.0001 20. 2.',
     &      'PID max min stpnt throt',IER,11)
          K=0
          CALL EGETWR(HOLD,K,cm(3),0.,100.,'F','Max flow',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,100.,'F','Min flow',IER)
          CALL EGETWR(HOLD,K,cm(5),-1001.,100.,'F','Set pt',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,100.,'F','Throt rnge',IER)
          if(ier.ne.0)goto 21

          I2=INT(cm(2))
          if(I2.eq.1.or.I2.eq.-1.or.I2.eq.2.or.I2.eq.-2)then
          H(1)=' Int. action flag: 1 if required; 0 if not required.'
            CALL EASKR(cm(7),' ',' Integral action flag?',
     &        0.,'F',1.,'-',0.,' integral action flag',IER,1)
            if(INT(cm(7)).eq.1)then
            CALL EASKR(cm(8),' ',' Integral action time [seconds]?',
     &        1.,'F',0.,'-',900.,' integral action time',IER,0)
            H(1)=' Derv. action flag: 1 if required; 0 if not req`d.'
            CALL EASKR(cm(9),' ',' derivative action flag?',
     &        0.,'F',1.,'-',0.,' derivative action flag',IER,1)
              if(INT(cm(9)).eq.1)then
                CALL EASKR(cm(10),' ',' Derivative action time [sec]?',
     &            1.,'F',0.,'-',500.,' derivative action time',IER,0)
              endif
            else
              H(1)=' Drv. action flag: 1 if required; 0 if not rqd.'
              CALL EASKR(cm(8),' ',' derivative action flag?',
     &          0.,'F',1.,'-',0.,' derivative action flag',IER,1)
                if(INT(cm(8)).eq.1)then
                  CALL EASKR(cm(9),' ',' Derivative action time [sec]?',
     &             1.,'F',0.,'-',500.,' derivative action time',IER,0)
                endif
            endif
          elseif(I2.eq.3.or.I2.eq.-3)then
              CALL EASKR(cm(7),' ',' Integral action time [seconds]?',
     &            1.,'F',0.,'-',900.,' integral action time',IER,0)
            elseif(I2.eq.4.or.I2.eq.-4.or.I2.eq.5.or.I2.eq.-5)then
              CALL EASKR(cm(8),' ',' Integral action time [seconds]?',
     &             1.,'F',0.,'-',900.,' integral action time',IER,0)
              CALL EASKR(cm(11),' ',' Derivative action time [secs]?',
     &             1.,'F',0.,'-',500.,' derivative action time',IER,0)
            endif
          elseif(IPCLAW(II,JJ,KK).eq.3)then

C Numerical proportional controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current numerical proportional controller data is:')
            call edisp(iuout,
     &      ' Max & min o/p, upper & lower setpoints, hysteresis ')
            write(outs,'(5F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6)
            call edisp(iuout,outs)
          endif
          cm(1)=5.0

  31      WRITE(HOLD,'(5f7.2)')cm(2),cm(3),cm(4),cm(5),cm(6)
          H(1)='See ESP-r Data Model section 9 for details. '
          CALL EASKS(HOLD,
     &    ' Max output, min output, upper setpoint, lower setpoint, ',
     &    ' output to overcome hysteresis',60,
     &      ' 2. 1. 2. 1. 0.1 ','num`l prop`l ctlr',IER,1)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','Max ouput',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,0.,'-','Min output',IER)
          CALL EGETWR(HOLD,K,cm(4),-100.,100.,'F','Up stpt',IER)
          CALL EGETWR(HOLD,K,cm(5),-100.,100.,'F','Low stpt',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,100.,'F','Hystersis',IER)
          if(ier.ne.0)goto 31
        elseif(IPCLAW(II,JJ,KK).eq.4)then

C Optimum start controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current optimum start controller data is:')
            call edisp(iuout,
     &      ' Max o/p, dtoa, desd temp, 1st B&J coft, 2nd B&J, 3rd B&J')
            write(outs,'(6F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
            call edisp(iuout,outs)
          endif
          cm(1)=6.0

  41      WRITE(HOLD,'(6f7.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
          H(1)='See ESP-r Data Model section 9 for details. '
          CALL EASKS(HOLD,
     &    ' Max output, desired time of arrival, desired temp, ',
     &    ' 1st B&J coeft, 2nd B&J coeft, 3rd B&J coeft ',60,
     &      ' 1000.0 9.0 20. 1. 1. 1. ','optimum start ctlr',IER,1)
          K=0
         CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','Max output',IER)
         CALL EGETWR(HOLD,K,cm(3),0.,24.,'F','Dsrd tm of Arv',IER)
         CALL EGETWR(HOLD,K,cm(4),-100.,100.,'F','Desrd temp',IER)
         CALL EGETWR(HOLD,K,cm(5),-100.,100.,'F','1st B&J cf',IER)
         CALL EGETWR(HOLD,K,cm(6),-100.,100.,'F','2nd B&J cf',IER)
         CALL EGETWR(HOLD,K,cm(7),-100.,100.,'F','3rd B&J cf',IER)
          if(ier.ne.0)goto 41
        elseif(IPCLAW(II,JJ,KK).eq.5)then

C Proportional damper controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current proportional damper controller data is:')
            call edisp(iuout,
     &      ' Max & min dpr pos, flow at max & min, 1st & 2nd conects ')
            write(outs,'(6F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
            call edisp(iuout,outs)
          endif
          cm(1)=6.0

  51      WRITE(HOLD,'(6f7.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
          H(1)='See ESP-r Data Model section 9 for details. '
          CALL EASKS(HOLD,
     &    ' Max damp pos, Min damp pos, flow at max pos, flow at',
     &    ' min pos, 1st connection number, 2nd connection number',60,
     &      ' 1. 0.  ','optimum start ctlr',IER,1)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,1.,'F','Max dmpr pos',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,0.,'F','Min dmpr pos',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,100.,'F','Flo at max',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,100.,'F','Flo at min',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,75.,'F','1st con',IER)
          CALL EGETWR(HOLD,K,cm(7),0.,75.,'F','2nd con',IER)
          if(ier.ne.0)goto 51
        elseif(IPCLAW(II,JJ,KK).eq.6)then
           
C Null controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Null controller has no data.')
            call edisp(iuout,' ')
          endif
          cm(1)=0.0
        elseif(IPCLAW(II,JJ,KK).eq.7)then

C Duty Cycle controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current duty cycle controller data is:')
            call edisp(iuout,' Dty cyc prd,  tim stps off, 1st off ts ')
            write(outs,'(3F9.2)')cm(2),cm(3),cm(4)
            call edisp(iuout,outs)
          endif
          cm(1)=3.0
 
  71      WRITE(HOLD,'(3f7.2)')cm(2),cm(3),cm(4)
          H(1)='All time-steps are in terms of plant time-steps. '
          H(2)=' For example, for a cycle period of, say, 5 '
          H(3)=' time-steps, there could be, say, 2 off, with, e.g'
          H(4)=' the 1st off at the 3rd time-step, and thus the '
          H(5)=' 2nd off at the 4th time-step.'
          H(6)='See ESP-r Data Model section 9 for details. '
          CALL EASKS(HOLD,
     &    ' Duty cycle prd, times steps off, 1st off time-step ',
     &    'in cycle period',60,' 4. 1. 1. ','Duty cycle ctlr',IER,6)
          K=0
          CALL EGETWR(HOLD,K,cm(2),1.,1440.,'F','Cycle perd',IER)
          CALL EGETWR(HOLD,K,cm(3),1.,1440.,'F','off t-stps',IER)
          CALL EGETWR(HOLD,K,cm(4),1.,1440.,'F','1st off ts',IER)
          if(ier.ne.0)goto 71
        elseif(IPCLAW(II,JJ,KK).eq.8)then

C On-off controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current on-off controller data is:')
            call edisp(iuout,' Mode, on setpt, off setpt, o/p at hi,
     &                         o/p at lo, sen lag, act lag')
            write(outs,'(7F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7),
     &                           cm(8)
            call edisp(iuout,outs)
          endif
          cm(1)=7.0

  81      WRITE(HOLD,'(7f7.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7),
     &                            cm(8)
          H(1)='See ESP-r Data Model section 9 for details. '
          CALL EASKS(HOLD,
     &    'Mode of operation, off set point, on set point, o/p at hi',
     &    ' o/p at lo, sen lag, act lag ',60,' 1. 2. 1. 1. 1. 1. 1.',
     &    'on-off ctl',IER,1)
          K=0
          CALL EGETWR(HOLD,K,cm(2),-999.,999.,'W','Mode',IER)
          CALL EGETWR(HOLD,K,cm(3),-100.,100.,'W','on set pnt',IER)
          CALL EGETWR(HOLD,K,cm(4),-100.,100.,'W','off set pnt',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,999999.,'W','output @ hi',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,999999.,'W','output @ lo',IER)
          CALL EGETWR(HOLD,K,cm(7),0.,1440.,'W','sensor lag',IER)
          CALL EGETWR(HOLD,K,cm(8),0.,1440.,'W','actu lag',IER)

          if(ier.ne.0)goto 81
        elseif(IPCLAW(II,JJ,KK).eq.9)then

C Multi-sensor on-off controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current multi-sen on-off ctlr data is:')
            call edisp(iuout,'O/p at hi, o/p at lo, Mode, No. axy sens, 
     &                        Auxy fnc, sen lag, act lag')
            write(outs,'(7F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7),
     &                           cm(8)
            call edisp(iuout,outs)

            I4=INT(cm(4))
            DO 91 L=1,I4             
              ndx=8+4*(L-1)
              call edisp(iuout,' Auxliary Sensor Details')              
              write(outs,'(4F9.2)')cm(ndx),cm(ndx+1),cm(ndx+2),cm(ndx+3)
              call edisp(iuout,outs)
              if(INT(cm(5)).eq.4)then
                ndx=12+(L-1)+4*(I4-1)         
                write(outs,'(a,F9.2)')' Weighting for sensor ',cm(ndx)
                call edisp(iuout,outs)
              endif
 91        continue   
          endif

          H(1)=' The number of data items required is in the range'
          H(2)=' 10 to 16 and depends on: (1) number of auxiliary '
          H(3)=' sensors (ns); (2) the auxiliary sensor function '
          H(4)=' mode selected:- '
          H(5)=' '
          H(6)='                                   |no. of| |no. of|'
          H(7)='          Function                 |aux sn| |items |'
          H(8)= '---------------------------------------------------'
          H(9)=' greatest val of aux`y sensors         1       10 '
          H(10)='   `      `  `    `     `             2       14 '  
          H(11)=' least val of aux`y sensors           1       10 '
          H(12)='   `    `  `    `     `               2       14 ' 
          H(13)=' mean val of aux`y sensors            1       10 '
          H(14)='   `   `  `    `     `                2       14 '
          H(15)=' weighting of aux`y sensors           2       16 '
          H(16)=' - Refer ESP-r Data Model section 9 '
          CALL EASKR(cm(1),' ',' No. of misc data items ',
     &      10.,'F',16.,'F',10.,'no. of misc data items',IER,16)

          CALL EASKR(cm(2),' ',' Output at `hi` status ? ',
     &      0.,'-',0.,'-',1.,'output at `hi` status',IER,0)
          CALL EASKR(cm(3),' ',' Output at `lo` status ? ',
     &      0.,'-',0.,'-',1.,'output at `lo` status',IER,0)

          H(1)= 'Mode = +1 for `ON` if control point is less than ' 
          H(2)='           specified function of aux`y sensor(s). '
          H(3)=' Mode = -1 for `ON` if control point is greater than' 
          H(4)='           specified function of aux`y sensor(s). '
          CALL EASKR(cm(4),' ',' Output mode ? ',
     &      -1.,'F',1.,'F',1.,' output mode',IER,4)

          H(1)=' At present, this is limited to 2 auxiliary sensors.'
          H(2)=' - Refer ESP-r Data Model section 9 '
          CALL EASKR(cm(5),' ',' No of auxiliary sensors ? ',
     &      1.,'F',2.,'F',1.,'no. of auxiliary sensors',IER,2)

          H(1)=' The auxiliary sensor function options are:'
          H(2)=' 1: greatest value of aux sensors ' 
          H(3)=' 2: least value of aux sensors '
          H(4)=' 3: mean value of aux sensors '
          H(5)=' 4: weighting of aux sensors '
          CALL EASKR(cm(6),' ',' Multi-sensor auxy function ? ',
     &      1.,'F',4.,'F',1.,' multi-sensor aux func',IER,5)

          H(1)=' Lag is in terms of plant time-steps'
          CALL EASKR(cm(7),' ',' Sensor lag? ',
     &      1.,'F',4.,'F',0.,' sensor lag',IER,1)

          CALL EASKR(cm(8),' ',' Actuator lag ? ',
     &      1.,'F',4.,'F',0.,' actuator lag',IER,1)

C Auxiliary sensor details,
          I4=INT(cm(4))  
          H(1)=' - Refer ESP-r Data Model Section 9 '
          H(2)='The items are essentially the same as required in a'
          H(3)=' control point sensor. '
          DO 93 L=1,I4
            ndx=8+4*(L-1)
            CALL EASKR(cm(ndx),' ',' Aux sen detail 1st item ? ',
     &        -3.,'F',0.,'-',1.,' Aux sen detail 1st item',IER,3)
            CALL EASKR(cm(ndx+1),' ',' Aux sen detail 2nd item ? ',
     &        0.,'F',0.,'-',0.,' Aux sen detail 2nd item',IER,3)          
            CALL EASKR(cm(ndx+2),' ',' Aux sen detail 3rd item ? ',
     &        0.,'F',0.,'-',0.,' Aux sen detail 3rd item',IER,3)          
            CALL EASKR(cm(ndx+3),' ',
     &        ' Aux sen detail 4th item (always use zero)? ',
     &        0.,'F',0.,'-',0.,' Aux sen detail 4th item',IER,3)
 93       CONTINUE    
          
          I5=INT(cm(5))
          if(I5.eq.4)then
            DO 95 L=1,I5
C Weighted auxiliary sensors mode,
              ndx=12+(L-1)+4*(I4-1)         
              H(1)=' The sensor weightings must total exactly 100.0'
              CALL EASKR(cm(ndx),' ',' Aux sensor weighting ? ',
     &          0.,'F',100.,'F',50.,' Aux sensor weighting',IER,1)
 95         continue
          endif
        elseif(IPCLAW(II,JJ,KK).eq.10)then
          CALL DG_controller_menu(cm)
        elseif(IPCLAW(II,JJ,KK).GE.11.AND.IPCLAW(II,JJ,KK).LE.13)then

C Controllers not supported through the interface yet
          GOTO 910
        elseif(IPCLAW(II,JJ,KK).eq.14)then

C Mutilsensor of control loops
          H(1)='You will be asked to state how many control loops you'
          H(2)='want to sense for this controller. For each loop that'
          H(3)='you specify you will asked a logical relationship'
          H(4)='with the next loop (.AND. or .OR.) e.g. loop 4 .AND.'
          H(5)='loop 7 will give the logical outcome of loop4 and 7'
          H(6)='based on their states at that time.'
          H(7)='The actuator for this component will have its control'
          H(8)='data modified at run time if the multisensor gives'
          H(9)='an ON signal. The value of this signal must be given'
          H(10)='alongwith the index of the control data item e.g.'
          H(11)='whether control data item no. 1 or 2 is being set'
          H(12)=' '
          H(13)='Additionally there is provision to set an overrun'
          H(14)='time. This is the time for which the component / '
          H(15)='parameter actuated by this loop will be on after'
          H(16)='the control loop sensor goes off.'
          CALL EASKI(NCLP,' ',' No. of control loops to sense ',
     &      2,'-',4,'-',2,'no. of misc data items',IER,11)
          cm(1)=real(NCLP*2+3)
          DO 1454 ICLP=1,NCLP
            CALL EASKI(ICLN,' ',' Select control loops number',
     &      1,'-',mcl,'-',1,'ctl loop number',IER,11)
            CM(ICLP*2)=ICLN
            IF(ICLP.NE.NCLP)THEN
              CALL EASKAB('Logical relationship with next loop',' ',
     &        '.OR.','.AND.',iad,11)
              CM(ICLP*2+1)=IAD
            ENDIF
 1454     CONTINUE
          TONV=CM(NCLP*2+1)
          CALL EASKR(TONV,' ',' Signal value at ON ',
     &        1.,'-',1.,'-',1.,'signal on value',IER,11)
          CM(NCLP*2+1)=TONV              

          TONV=CM(NCLP*2+2)
          CALL EASKR(TONV,' ',' Signal value at OFF ',
     &        1.,'-',1.,'-',1.,'signal off value',IER,11)
          CM(NCLP*2+2)=TONV              

          TONV=CM(NCLP*2+3)
          CALL EASKI(ICDAT,' ','Which control item to set at ON'
     &        ,1,'-',1,'-',1,'CDATA for component',IER,11)
          CM(NCLP*2+3)=REAL(ICDAT)

          TONV=CM(NCLP*2+4)
          CALL EASKR(TONV,' ',' Overrun time (min)',
     &        1.,'-',1.,'-',1.,' Overrun time (min)',IER,11)
          CM(NCLP*2+4)=TONV
        elseif(IPCLAW(II,JJ,KK).eq.15)then

C Proportional Integral Room controller
          H(1) ='This control law is a simplified PI controller '
          H(2) ='specially adapted to room control of wet central'
          H(3) ='heating (WCH) systems. It can be used to control the'
          H(4) ='BDATA items of a plant component using PI action.'
          H(5) ='Alternately it can be used in TPMPI (time proportional'
          H(6) ='modulating proportional integral) mode. In this mode '
          H(7) ='BDATA items are not modified but ON time is controlled'
          H(8) ='For the implementation it was developed it was used'
          H(9) ='to control the upper boiler temperature limit of a'
          H(10)='WCH boiler depending on the output temperature of a'
          H(11)='thermostat.'
          H(12)='Control inputs are: '
          H(13)='Setpoint nominal T_st (default value 21)'
          H(14)='Sensed condition T_sen (from sensor definition)'
          H(15)='Setpoint max T_max (default value 90)'
          H(16)='Setpoint min T_min (default value 20)'
          H(17)='Proportional band PB (default value 1)'
          H(18)='Bdata item number of actuated plant component, the' 
          H(19)='value of this parameter will be changed by this '
          H(20)='controller index BDATA (default value 10)'
          H(21)='Integral reset time AIRT (default value 1)'
          H(22)='DELTA=T_sen-T_st'
          H(23)='Integral summation of delta SDELTA'
          H(24)='At simulation run time BDATA value will be '
          H(25)='modified according to the following logic:'
          H(26)='BDATA=(T_max-T_min)*(DELTA/PB+SDELTA/(PB*AIRT)'
          H(27)='       + 0.5*(T_max+T_min)'
          H(28)='If BDATA is bigger than T_max it is reset to T_max or'
          H(29)='If BDATA is smaller than T_min it is reset to T_min'
          H(30)='Optionally an additional control loop can be specified'
          H(31)='it this loop is active then BDATA item number is '
          H(32)='reset to a user defined static value, if you dont'
          H(33)='want to specify an optional loop leave its value = 0'
          H(34)=' '
          H(35)='The TPMPI mode of operation becomes active if cycle'
          H(36)='time is stated to be more than zero.'
          H(37)=' '
          H(38)=' '
          H(39)=' '
          H(40)=' '
          H(41)=' '
          H(42)=' '
          H(43)=' '
          CM(1)=8.0
          call edisp(iuout,' ')
          call edisp(iuout,' current control parameters are:')
          call edisp(iuout,
     &      ' nominal, max, min, prop band, BDATA, reset time')
 101      WRITE(HOLD,'(8(F5.2,1X))')CM(2),CM(3),CM(4),CM(5),CM(6),CM(7)
     &       ,CM(8),CM(9)
          CALL EDISP(IUOUT,HOLD)
          CALL EASKS(HOLD,
     &    ' nominal, max, min, prop band, BDATA, reset time (sec)',
     &    'optional control loop, static value (see help)',
     &    60,HOLD,'PI room ctrlr',IER,30)
          K=0
          CALL EGETWR(HOLD,K,cm(2),1.,1.,'-','Setpoint nominal ',IER)
          CALL EGETWR(HOLD,K,cm(3),1.,1.,'-','Setpoint max',IER)
          CALL EGETWR(HOLD,K,cm(4),1.,1.,'-','Setpoint min',IER)
          CALL EGETWR(HOLD,K,cm(5),1.E-6,1.E6,'F','Prop band',IER)
          CALL EGETWR(HOLD,K,cm(6),1.,1.,'-','BDATA item #',IER)
          CALL EGETWR(HOLD,K,cm(7),1.E-6,1.E6,'F','Int reset time',IER)
          CALL EGETWR(HOLD,K,cm(8),1.,1.,'-','optional cntrl loop',IER)
          CALL EGETWR(HOLD,K,cm(9),1.,1.,'-','BDATA static value',IER)
          if(ier.ne.0)goto 101
        elseif(IPCLAW(II,JJ,KK).eq.16)then

C Outside temperature compensation controller
          H(1) ='This controller modifies BDATA parameters of a plant'
          H(2) ='component at simulation runtime. It was originally '
          H(3) ='used to control boiler upper temperature limit for '
          H(4) ='wet central heating systems.'
          H(5) ='The following parameters are used:'
          H(6) ='coefficients alpha, beta, gamma with default values'
          H(7) =' (3.5 2. and 0. respectively)'
          H(8) ='Nominal temperature 1 = T_st1 (default value 20)'
          H(9) ='Nominal temperature = T_st (default value 21)'
          H(10)='Setpoint max T_max (default value 90)'
          H(11)='Setpoint min T_min (default value 20)'
          H(12)='ambient temperature T_amb (from climate file)'
          H(13)='Sensed condition T_sen (from sensor definition)'
          H(14)='Bdata item number of actuated plant component, the' 
          H(15)='value of this parameter will be changed by this '
          H(16)='controller index BDATA (default value 10)'
          H(17)='At simulation run time the BDATA value will be'
          H(18)='BDATA=T_st1+BETA*(T_st1-TFP)+ALPHA+GAMMA*(T_st-T_sen)'
          H(19)='If BDATA is bigger than T_max it is reset to T_max or'
          H(20)='If BDATA is smaller than T_min it is reset to T_min'
          H(21)='Optionally an additional control loop can be specified'
          H(22)='it this loop is active then BDATA item number is '
          H(23)='reset to a user defined static value, if you dont'
          H(24)='want to specify an optional loop leave its value = 0'
          cm(1)=10.
          call edisp(iuout,' ')
          call edisp(iuout,' current control parameters are:')
          call edisp(iuout,'alpha, beta, gamma')
  111     write(HOLD,'(3(F5.2,1X))')cm(2),cm(3),cm(4)
          CALL EDISP(IUOUT,HOLD)
          CALL EASKS(HOLD,'alpha, beta, gamma',
     &    '(see help)',60,HOLD,'OTC ctrlr',IER,24)
          K=0
          CALL EGETWR(HOLD,K,cm(2),1.,1.,'-','alpha ',IER)
          CALL EGETWR(HOLD,K,cm(3),1.,1.,'-','beta',IER)
          CALL EGETWR(HOLD,K,cm(4),1.,1.,'-','gamma',IER)
          call edisp(iuout,' further control parameters are:')
          call edisp(iuout,'nominal1, nominal, max, min, BDATA')
          IF(IER.NE.0)GOTO 111
  121     WRITE(HOLD,'(7(F5.2,1X))')CM(5),CM(6),CM(7),CM(8),
     &    CM(9),CM(10),CM(11)
          CALL EDISP(IUOUT,HOLD)
          CALL EASKS(HOLD,'nominal1, nominal, max, min, BDATA',
     &    'optional control loop, static value (see help)'
     &    ,60,HOLD,'OTC ctrlr',IER,24)
          K=0
          CALL EGETWR(HOLD,K,cm(5),1.,1.,'-','nominal1',IER)
          CALL EGETWR(HOLD,K,cm(6),1.,1.,'-','nominal',IER)
          CALL EGETWR(HOLD,K,cm(7),1.,1.,'-','max',IER)
          CALL EGETWR(HOLD,K,cm(8),1.,1.,'-','min',IER)
          CALL EGETWR(HOLD,K,cm(9),1.,1.,'-','BDATA item #',IER)
          CALL EGETWR(HOLD,K,cm(10),1.,1.,'-','optional cntrl loop',IER)
          CALL EGETWR(HOLD,K,cm(11),1.,1.,'-','BDATA static value',IER)
          IF(IER.NE.0)GOTO 121
        endif

C Extract from working array to pmiscd() common structure
        call extrctl(1,II,JJ,KK)
        call usrmsg(' ',' ','-')
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

 3    CALL USRMSG(' ','Problem writing miscellaneous data.','W')
      RETURN
      END

C *********** EFCNTRL
C Edit flow control function II.
      SUBROUTINE EFCNTRL(II,IV,IER)
#include "building.h"
#include "net_flow.h"
#include "epara.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
      common/sctl/tcps,ictyp,iclaw,cm(misc)
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

      DIMENSION ICPK(MCNN),INPK(MNOD)
      CHARACTER*84 VERT(36)
      CHARACTER outs*124,H*72,SSTR*72,TMP*72,KEY*1,NDNAM*12
      character outsa*84,outsb*84,outsc*84,hold*42,LTMP*248
      character prmpt1*72,prmpt2*72

C Portions of multi-sensor feedback message.
      character msg1*24,msg2*24,msg3*24,msg4*24
      logical close1,OK,DOK

C Number of functions and day type must be > 0.
      IF(II.LE.0.OR.IV.LE.0)RETURN

C Control day types.
      J=IV
      JJ=J

C Initialise menu size variables based on windows size.
C IVERT is the menu position, MVERT the current
C number of menu lines.
 92   MHEAD=5
      MCTL=4
      ILEN=NFCDP(II,JJ)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
      IER=0
      IVERT=-3
      WRITE(vert(1),'(a,i3,a,i3)')' function',II,' day type',JJ
      WRITE(vert(2),'(a,i3)')     ' number of periods:',NFCDP(II,JJ)
      WRITE(vert(3),'(a)') ' ________________________________________'
      WRITE(vert(4),'(a)')
     &' per| start|sensed  |actuated | control law     | data'
      WRITE(vert(5),'(a)')
     &' no.| time |property|property |                 |     '
      ILEN=NFCDP(II,JJ)

C Generate menu entry for each period in day type.
      m=MHEAD
      DO 91 KP=1,ILEN
        IF(KP.GE.IST.AND.(KP.LE.(IST+MIFULL)))THEN
          KK=KP
          m=m+1
          CALL EMKEY(KP,KEY,IER)
          call stfctl(2,II,JJ,KK)
          CALL EVCNTRL(2,II,JJ,KK,'T',SSTR)
          CALL EVCNTRLAW(2,II,JJ,KK,TMP,LTMP)
          NITEMS=INT(cm(1))
          WRITE(outsa,'(i2,F7.2,1x,a,1x,a)')KK,tcps,SSTR(1:18),TMP(1:18)
          outsb=' '
          outsc=' '
          if(NITEMS.gt.0)then
            if(NITEMS.LE.9)then
              write(outsb,'(9F9.2)')(cm(L),L=2,NITEMS+1)
            else
              write(outsb,'(9F9.2)')(cm(L),L=2,10)
            endif
            call SDELIM(outsb,outsc,'S',IW)
          endif
          write(vert(m),'(1a,1x,a,1x,a)',IOSTAT=IOS,ERR=3) key,
     &      outsa(1:47),outsc(1:34)
        endif
  91  continue

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _______________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' --')
      ENDIF
      WRITE(vert(m+2),'(a)') '* add/ delete a period '
      WRITE(vert(m+3),'(a)') '? help '
      WRITE(vert(m+4),'(a)') '- exit  '

C Help text for this menu.
      h(1) ='A number of control periods can be defined for each'
      h(2) ='control day type. Each period has a start time (i.e.'
      h(3) ='they do not overlap), a sensed property, an actuated'
      h(4) ='property, a control law and miscel. data which defines'
      h(5) ='capacity, setpoints, etc.'
      h(6) ='  '
      h(7) ='Note: if no change required in an item then accept the'
      h(8) ='current value.'

C Now display the menu.
      CALL EMENU('Control periods',VERT,MVERT,IVERT)
      if(IVERT.EQ.mvert)then
        return
      elseif(IVERT.EQ.mvert-1)then

C Explanation
        CALL PHELPD('control period help',8,'-',0,0,IER)
      elseif(IVERT.EQ.mvert-2)then

C Add or delete a period.
        h(1)='Periods are added to the end of the list. '
        CALL EASKABC('Options:',' ',
     &    'add period','delete period','continue',iad,1)
        if(iad.eq.1)then
          call addctldp(2,II,JJ,'A',iier)
        elseif(iad.eq.2)then
          call addctldp(2,II,JJ,'D',iier)
        endif
        goto 92
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Take the t?cps() i?ctyp(),i?claw(), ?MISCD(II,JJ,KK,?) common and
C stuff into working common array 'cm' for editing.  At end
C of if:elseif:else recover.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        KK=IFOC
        call stfctl(2,II,JJ,KK)

        NITEMS=INT(cm(1))
        if(NITEMS.gt.0)then
          WRITE(outs,'(a,i3,a)')' For period',KK,' current data...'
          call edisp(iuout,outs)
          if(NITEMS.LE.13)then
            write(outs,'(13F9.1)')(cm(L),L=2,NITEMS+1)
          else
            write(outs,'(13F9.1)')(cm(L),L=2,13)
          endif
          call SDELIM(outs,outsc,'S',IW)
          call edisp(iuout,outsc)
        else
          WRITE(outs,'(a,i3,a)')' For period',KK,' define...'
          call edisp(iuout,outs)
        endif

        call edisp(iuout,' ')

C Flow controller.  Set type to same as initial for this flow
C function (ie. all periods are the same for flow).
        ictyp=IFCTYP(II,1,1)
        IFCTYP(II,JJ,KK)=ictyp
        write(outs,'(a,i3)')' Current controller type: ',ictyp
        call edisp(iuout,outs)
        call edisp(iuout,'Note: to change this re-edit the flow sensor')

C What is the equivalent for flow.....
        H(1)='See ESP-r Data Model Section 9 for details. '
        H(2)='The range controler is for use with component types'
        H(3)='30 35 40 110 130 and should sense a node temperature '
        H(4)='or outside db temperature. It returns a ratio of the'
        H(5)='nominal rate or area associated with each range. '
        ino=iclaw+1
        ilno=ino
        idno=1
        write(outs,'(a,i2,a)')'Select a control type (currently: ',
     &    iclaw+1,')'
        call MENUATOL(outs,' Available laws','a on/off',
     &   'b proportional with hysteresis',
     &   'c range based (for 30 35 40 110 130)',
     &   'd multi-sensor on/off',
     &   'e human behaviour algorithm ver 1.0',' ',' ',' ',' ',' ',' '
     &   ,' ',ino,idno,5)
        if(ino.eq.0)then
          continue
        elseif(ilno.ne.ino)then
          iclaw=ino-1
          IFCLAW(II,JJ,KK)=iclaw
        endif

  62    H(1)='See ESP-r Data Model Section 9 for details. '
        CALL EASKR(tcps,' ',' Period start time ? ',
     &    0.,'F',24.,'F',0.,'control start',IER,1)
        if(KK.GT.1)then
          IF(TFCPS(II,JJ,KK-1).GE.tcps)then
            call usrmsg('Periods out of order..',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcps,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0','W')
            GOTO 62
          endif
        endif
        TFCPS(II,JJ,KK)=tcps

        if(iclaw.eq.0)then

C Miscellaneous data for on / off controller.
          cm(1)=3.
  53      WRITE(HOLD,'(3f8.2)')cm(2),cm(3),cm(4)
          H(1)= 'Number of items for on/off controller = 3 '
          H(2)= ' 1st is setpoint, '
          H(3)= ' 2nd is action index 1=direct, -1=inverse'
          H(4)= '   where direct is ON above setpoint.'
          H(5)= '   and inverse  is ON below setpoint.'
          H(6)= ' 3rd is fraction of capacity (e.g. if set to 0.5'
          H(7)= '   of the area, door width or flow rate of the'
          H(8)= '   component is 0.5 x its stated value) valid  '
          H(9)= '   fractions are between 0.0 and 1.0 for area based'
          H(10)='   components and 0.1 to 1.0 for door width. A value'
          H(11)='   of 1.0 is equivalent to a simple ON:OFF control.'
          H(12)='   If a door control action would cause it to be less'
          H(13)='   than 10mm wide it is reset to 10mm.'
          CALL EASKS(HOLD,
     &    ' On/Off setpoint, action (1=on above setpoint, -1=on below)',
     &      ' and fraction ON (see help):',42,' 30. 1. 1. ',
     &      ' flow on/off controller',IER,13)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','set pt',IER)
          CALL EGETWR(HOLD,K,cm(3),-1.,1.,'F','action',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,1.,'F','fraction',IER)
          if(int(cm(3)).eq.0)goto 53
          if(ier.ne.0)goto 53
        elseif(iclaw.eq.1)then

C Miscellaneous data for proportional controller.
          cm(1)=5.
  54      WRITE(HOLD,'(5f8.2)')cm(2),cm(3),cm(4),cm(5),cm(6)
          H(1)= 'Number of items for proportional controller = 5 '
          H(2)= '  1st is signal lower limit,'
          H(3)= '  2nd is valve position at lower limit (%),'
          H(4)= '  3rd is signal upper limit, '
          H(5) ='  4th is valve position at upper limit (%),'
          H(6) ='  5th dS to overcome hysteresis,'
          CALL EASKS(HOLD,
     &   ' Proportional: signal lower lmt, lower valve psn (%), signal',
     &   ' upper lmt, upper valve psn (%), hysteresis: ',42,
     &      ' 10.  0.  20. 100.  1.',' flow proportional controller',
     &      IER,6)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','signl low',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,100.,'F','valv psn l',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,0.,'-','signl high',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,100.,'F','valv psn h',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,9999.,'F','ds for hist',IER)
          if(ier.ne.0)goto 54
        elseif(iclaw.eq.2)then
          cm(1)=6.
  55      WRITE(HOLD,'(3f8.2)')cm(2),cm(3),cm(4)
          H(1)= 'Number of items for range controller = 6 '
          H(2)= ' 1st is low setpoint, '
          H(3)= ' 2nd is mid setpint'
          H(4)= ' 3nd is high setpint'
          H(5)= ' which yields 4 ranges as follows:'
          h(6)= '   low range < low setpoint, '
          h(7)= '   default range between low and mid setpoints, '
          h(8)= '   mid range between mid and hi setpoints, '
          h(9)= '   above high setpoint. '
          H(10)=' 4th is low range ratio (of rate or area), '
          H(11)=' 5th is mid range ratio (of rate or area),'
          H(12)=' 6th is high range ratio (of rate or area)'
          h(13)=' '
          h(14)='If within default range then the nominal rate or '
          h(15)='area is used. '
          h(16)=' '
          h(17)='NOTE: this controller works with sensed conditions'
          h(18)='such as node temperature and can be used with '
          h(19)='components 30 35 40 110 and 130. '
          CALL EASKS(HOLD,
     &      ' Range setpoints: low, mid, high (see help)',
     &      ' : ',42,' 0. 10. 20. ',' flow range controller',IER,19)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','low set pt',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,0.,'-','mid set pt',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,0.,'-','high set pt',IER)
          if(ier.ne.0)goto 55
 157      WRITE(HOLD,'(3f8.3)')cm(5),cm(6),cm(7)
          CALL EASKS(HOLD,
     &      ' Ratios of rate or area for: low, mid, high (see help)',
     &      ' : ',42,' 0. 1. 1. ',' flow range controller',IER,16)
          K=0
          CALL EGETWR(HOLD,K,cm(5),0.,0.,'-','low ratio',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,0.,'-','mid ratio',IER)
          CALL EGETWR(HOLD,K,cm(7),0.,0.,'-','high ratio',IER)
          if(ier.ne.0)goto 157
        elseif(iclaw.eq.3)then

C Miscellaneous data for multi sensor flow controller.
          cm(1)=2.
  56      WRITE(HOLD,'(2f8.2)')cm(2),cm(3)
          H(1)= 'Number of items for multi sensor flow'
          H(2)= 'controller = 2 + 8 * no. of sensors'
          H(3)= '  1st is default position: (1=open,0=closed)'
          H(4)= '  2nd is number of auxiliary sensors. Then'
          H(5)= '  for each auxiliary sensor 8 items are needed:'
          H(6)= '    4 items for the sensor location followed by'
          H(7)= '    1 item for the set point temperature'
          H(8)= '    1 item for action flag (1=direct, -1=inverse)'
          H(9)= '    1 item for fraction ON (currently not implemented)'
          H(10)='    1 item for logical action (AND/OR/NOT/SLAVE)'
          H(11)='The connection/component will CHANGE from the default'
          H(12)='position when the sensed value is above the set-point'
          H(13)='if action flag is DIRECT. In case the action flag is'
          H(14)='set to INVERSE, the CHANGE will take place when the'
          H(15)='sensed value is below the set-point.'
          H(16)=' '
          H(17)='This CHANGE when it takes place will switch the  '
          H(18)='connection from its default position (open or'
          H(19)='closed) to the inverse. ' 
          H(20)=' '
          H(21)='Any sensor defined in the sensor location menu will'
          H(22)='be overridden. But sensor needs to be defined to '
          H(23)='pass through error traps therefore define sensor '
          H(24)='location as usual. '
          H(25)=' '
          H(26)='The logical action is a means to combine logically'
          H(27)='the outcomes of the multiple sensors. e.g. if in a'
          H(28)='multisensor control comprising 3 sensors the  '
          H(29)='logical action of sensor1 is AND, sensor2 is OR '
          H(30)='and sensor3 is AND then the outcome of the multisensor'
          H(31)='will be sensor1 AND sensor2 OR sensor3 '
          H(32)='Note that logical action of sensor3 is irrelevant'
          H(33)=' '
          H(34)='If the NOT/SLAVE option is selected the output of this'
          H(35)='control loop will be inverse/same of flow connection'
          H(36)='defined. Any controls on this flow connection must be'
          H(37)='defined before the NOT/SLAVE loop (i.e. this loop) '
          H(38)=' '
          H(39)='The sensor details are essentially the same as '
          H(40)='required in a control point sensor ie. '
          H(41)=' 1st item >0 is a zone and or surface, =0 any zone,'
          H(42)='   = -1 measuring a plant component node,'
          H(43)='   = -2 measuring mix air:radiant,'
          H(44)='   = -3 ambient where the 2nd item is:'
          H(45)='      0 = db,       2 = wind velocity, '
          H(46)='      3 = wind dir, 4 = diffuse radiation, '
          H(47)='      5 = direc norm rad, 6 = RH'
          H(48)='   = -4 measuring a flow side condition.'
          H(49)=' '
          H(50)='Currently only option -3 and -4 are supported and'
          H(51)='only node temperature is supported for option -4'
          H(52)='Therefore the four items for sensor location are'
          H(53)='-3 ? 0 0 where ?=0 or 2 to 6 depending upon climate'
          H(54)='variable. OR. -4 ? 0 0 where ?= node number to'
          H(55)='be sensed.'
          H(56)=' '

C<< To include feed back to text feedback area using USRMSG
          WRITE(OUTS,'(A,I4,A)')'number of sensors (max ',MSEN,' )?' 
          NSEN=INT(CM(3))
          CALL EASKI(NSEN,' ',OUTS,
     &      0,'F',MSEN,'-',1,'no of sensors',IERI,56)
          if(ieri.eq.-3) goto 56
          if(ieri.ne.0)goto 56
          cm(3)=real(NSEN)
          call edisp(iuout,' Ideal multi-sensor controller:')
          cm(1)=2.+8.*NSEN
          OK=.true.
          IF(NSEN.EQ.1)
     &    CALL ASKOK('You have chosen just one sensor, the only option',
     &    'for mulitsensing is the NOT/SLAVE option. Is this correct?'
     &    ,OK,dok,56)
          IF(.NOT.OK)THEN
            GOTO 56
          ELSEIF(NSEN.EQ.1)THEN
            INOT=1
            CALL EASKAB('Choose whether NOT or SLAVE',' ','NOT','SLAVE'
     &      ,INOT,56)
          ENDIF
          IF(NSEN.EQ.1)GOTO 69
 
C Default position: 1=open,0=closed
          CALL EASKAB('Default position? ',' ','closed','open',IW,56)
          cm(2)=IW-1

C Auxiliary sensor details,
          DO 358 ISEN=1,NSEN
            ndx=4+8*(ISEN-1)
            WRITE(OUTS,'(A,I3)')'What to sense for sensor number ',ISEN
            CALL EASKAB(OUTS,' ','Climate variable','Node temperature'
     &      ,IW,56)
            IF(IW.EQ.1)THEN
              CM(NDX)=-3.
              idno=1
              call MENUATOL(' ',' Climate variables','a temperature',
     &        'b wind speed','c wind direction',
     &        'd diffuse radiation','e direct radiation ',
     &        'e relative humidity ',' ',' ',' ',' ',' ',' ',
     &        ino,idno,56)
              IF(INO.EQ.1)THEN
                CM(NDX+1)=0
                msg1=' senses ambient T'
              elseif(INO.eq.2)then
                CM(NDX+1)=INO
                msg1=' senses wind speed'
              elseif(INO.eq.3)then
                CM(NDX+1)=INO
                msg1=' senses wind direction'
              elseif(INO.eq.4)then
                CM(NDX+1)=INO
                msg1=' senses diffuse solar'
              elseif(INO.eq.5)then
                CM(NDX+1)=INO
                msg1=' senses direct solar'
              elseif(INO.eq.6)then
                CM(NDX+1)=INO
                msg1=' senses RH'
              ENDIF
            ELSEIF(IW.EQ.2)THEN
              CM(NDX)=-4.
              call ASKRNOD('  sensed flow node','-',INOD,IER)
              CM(NDX+1)=INOD
              write(msg1,'(2a)') ' senses nd ',ndnam(inod)
            ENDIF
            CM(NDX+2)=0
            CM(NDX+3)=0
            CALL EASKR(cm(ndx+4),' ',' Aux sensor set-point ? ',
     &        -100.,'F',0.,'-',0.,' Aux sensor set-point',IER,56)
            write(msg2,'(a,f5.2)') ' setpoint ',cm(ndx+4)
            CALL EASKAB('Action Flag (see help)',' ','Direct',
     &      'Inverse',IW,56)
            IF(IW.EQ.1)THEN
              cm(ndx+5)=1.0
            ELSEIF(IW.EQ.2)THEN
              cm(ndx+5)=-1.0
            ENDIF
            if(int(cm(ndx+5)).eq.-1)msg3=' inverse action'
            if(int(cm(ndx+5)).eq.1) msg3=' direct action'

C<< To implement "fraction on" in a sensible way (setting it to diffent)
C values for different sensors may cause ambiguity if differnet sensors
C require a connection to open to different fractions
C            CALL EASKR(cm(ndx+6),' ',' Aux sensor "fraction ON"? ',
C     &        0.,'F',1.,'F',1.,' Aux sensor set-point',IER,56)
            cm(ndx+6)=0.
            IF(ISEN.LT.NSEN.OR.ISEN.EQ.1)
     &      CALL EASKABC('Logical action variable',
     &      'with next sensor','AND','OR','NOT',iad,56)
            IF(IAD.EQ.1)THEN
              CM(NDX+7)=1.
              msg4=' AND'
            ELSEIF(IAD.EQ.2)THEN
              CM(NDX+7)=2.
              msg4=' OR'
            ELSEIF(IAD.EQ.3)THEN
              CM(NDX+7)=3.
              msg4=' NOT'
            ENDIF

C Echo back to the user what they just put in.
            write(outs,'(a,i1,4a)') 'For sensor ',isen,
     &        msg1(1:lnblnk(msg1)),msg2(1:lnblnk(msg2)),
     &        msg3(1:lnblnk(msg3)),msg4(1:lnblnk(msg4))
            call edisp(iuout,outs)
 358      CONTINUE
          GOTO 70
 
C For the NOT option allow to read in sensed flow connection
  69      INODE=0
          NNPK=1
          prmpt1='Choose connection'
          prmpt2=' '
          CALL ASKMFCON(INODE,NNPK,ICPK,prmpt1,prmpt2)
          IF(INOT.EQ.2)THEN
            cm(6)=-1.*real(icpk(1))
          ELSE
            cm(6)=real(icpk(1))
          ENDIF

C Adaptive behavioural model as detailed in Comfort Driven Adaptive
C Window Opening Behaviour and the Influence of Building Design. Tuohy,
C P., Rijal, H. B., Humphreys, M. A., Nicol, J. F., Samuel, A. A.,
C Clarke, J. A. Proc. BS 2007. 10th IBPSA Conference and Exhibition.
        elseif(iclaw.eq.4)then

C cm(1)=13 for version 2
          cm(1)=1
          H(1)='The windowfan algorithm models occupant behaviour'
          H(2)='and operates windows and fans based on mean operative'
          H(3)='and outdoor mean temperatures automatically.'
          H(4)='Users need to input the running mean response'
          H(5)='parameter to outside temperature for the building'
          H(6)='which is a constant from 0-0.99 (Default value 0.8)'
          H(7)='The operative (resultant=0.5mean radiant + 0.5dry bulb)'
          H(8)='temperature is required for this algorithm. User has '
          H(9)='to specify internal node to be used to calculate this'
          cm(2)=0.8
          CALL EASKR(cm(2),' ',' Running mean response parameter ? ',
     &    0.,'F',0.99,'F',0.8,'Running mean response parameter',IER,9)

C Uncomment following code for version 2 of algorithm
**          CALL EASKR(cm(3),'  ','Fraction window openable in day ? ',
**     &    0.,'F',1.0,'F',1.0,' Wposs',IER,9)
**          CALL EASKR(cm(4),' ','Fraction fan available for day use?',
**     &    0.,'F',1.0,'F',1.0,' Fposs',IER,9)
**          CALL EASKR(cm(5),' ','Fraction window openable in night ? ',
**     &    0.,'F',1.0,'F',1.0,' WNposs',IER,9)
**          CALL EASKR(cm(6),' ','Fraction AC available for day use?',
**     &    0.,'F',1.0,'F',1.0,' ACavail',IER,9)
**          CALL EASKR(cm(7),' ','Fraction heating available ?',
**     &    0.,'F',1.0,'F',1.0,' Havail',IER,9)
**          CALL EASKR(cm(8),' ','Fraction daylight available?',
**     &    0.,'F',1.0,'F',1.0,' DLposs',IER,9)
**          CALL EASKR(cm(9),' ','Power rating of the fan W/m2?',
**     &    0.,'F',100.,'F',10.0,' PWRfan',IER,9)
**          CALL EASKR(cm(10),' ','Power rating of the lights W/m2?',
**     &    0.,'F',100.,'F',10.0,' PWRlight',IER,9)
**          CALL EASKR(cm(11),' ','Fan comfort temperature adjustment?',
**     &    0.,'F',4.0,'F',2.0,' DTfan',IER,9)
**          CALL EASKR(cm(12),' ','Deadband for window open?',
**     &    0.,'F',8.0,'F',4.0,' WD',IER,9)
**          CALL EASKR(cm(13),' ','Deadband for fan on?',
**     &    0.,'F',8.0,'F',2.0,' FD',IER,9)
**          CALL EASKR(cm(14),' ','Air intake surface ID?',
**     &    0.,'F',20.,'F',1.0,' AIsurf',IER,9)

C Ask for node associated with operative temperature
          prmpt1='Which node is to be used to represent the '
          prmpt2='operative temperature?'
          call edisp(iuout,prmpt1)
          call edisp(iuout,prmpt2)
          INOPT=1
          NNPK=1
 545      call ASKMFNOD(INOPT,NNPK,INPK,prmpt1,prmpt2,9)
          IIN=0
          IF(NDTYP(INPK(1)).GT.2)THEN
            H(1)='Operative node must be inside a zone'
            H(2)='Select an internal node'
            IIN=1
            CALL PHELPD('node selection help',2,'-',0,0,IER)
          ENDIF
          IF(IIN.EQ.1)GOTO 545
          IFSN(II,1)=-3
          IFSN(II,2)=7
          IFSN(II,3)=INPK(1)

C Miscellaneous data for additional flow controllers should follow here.
  70    endif

C Extract from working array to fmiscd() common structure
        call extrctl(2,II,JJ,KK)
        call usrmsg(' ',' ','-')
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

 3    CALL USRMSG(' ','Problem writing miscellaneous data.','W')
      return
      END

C *********** EGCNTRL
C Edit global control function II.
      SUBROUTINE EGCNTRL(ITRC,II,IV,IER)
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)
      common/sctl/tcps,ictyp,iclaw,cm(misc)

      CHARACTER outs*124,H*72,SSTR*72,TMP*72,LTMP*248
      character outsa*84,outsb*84,outsc*84,KEY*1
      character hold*42
      CHARACTER*84 VERT(20)
      logical close1

C Number of global functions and day type must be > 0.
      IF(II.LE.0.OR.IV.LE.0)RETURN

C Control day types.
      J=IV
      JJ=J

C Initial menu entry setup.
   92 IER=0
      IVERT=-3
      WRITE(vert(1),'(a,i3,a,i3)')' function',II,' day type',JJ
      WRITE(vert(2),'(a,i3)')     ' number of periods:',NGCDP(II,JJ)
      WRITE(vert(3),'(a)') ' ________________________________________'
      WRITE(vert(4),'(a)')
     &' per| start|sensed  |actuated | control law     | data'
      WRITE(vert(5),'(a)')
     &' no.| time |property|property |                 |     '
      LL=NGCDP(II,JJ)

C Generate menu entry for each period in day type.
      m=5
      DO 91 KP=1,LL
        KK=KP
        m=m+1
        call stfctl(3,II,JJ,KK)
        CALL EVCNTRL(3,II,JJ,KK,'T',SSTR)
        CALL EVCNTRLAW(3,II,JJ,KK,TMP,LTMP)
        NITEMS=INT(cm(1))
        WRITE(outsa,'(i2,F7.2,1x,a,1x,a)')KK,tcps,SSTR(1:18),TMP(1:18)
        outsb=' '
        outsc=' '
        if(NITEMS.gt.0)then
          if(NITEMS.LE.9)then
            write(outsb,'(9F9.1)')(cm(L),L=2,NITEMS+1)
          else
            write(outsb,'(9F9.1)')(cm(L),L=2,10)
          endif
          call SDELIM(outsb,outsc,'S',IW)
        endif
        CALL EMKEY(M-5,KEY,IER)
        write(vert(m),'(1a,1x,a,1x,a)',IOSTAT=IOS,ERR=3) key,
     &    outsa(1:47),outsc(1:34)
  91  continue
      m=m+1
      WRITE(vert(m),'(a)') ' ________________________________________'
      m=m+1
      WRITE(vert(m),'(a)') '* add/ delete a period '
      m=m+1
      WRITE(vert(m),'(a)') '? help '
      m=m+1
      WRITE(vert(m),'(a)') '- exit  '
      mvert=m

C Help text for this menu.
      h(1) ='A number of control periods can be defined for each'
      h(2) ='control day type. Each period has a start time (i.e.'
      h(3) ='they do not overlap), a sensed property, an actuated'
      h(4) ='property, a control law and miscel. data which defines'
      h(5) ='capacity, setpoints, etc.'
      h(6) ='  '
      h(7) ='Note: if no change required in an item then accept the'
      h(8) ='current value.'

C Now display the menu.
      CALL EMENU('Control periods',VERT,MVERT,IVERT)
      if(IVERT.EQ.mvert)then
        return
      elseif(IVERT.EQ.mvert-1)then

C Explanation
        CALL PHELPD('control period help',8,'-',0,0,IER)
      elseif(IVERT.EQ.mvert-2)then


C Add or delete a period.
        h(1)='Periods are added to the end of the list.'
        CALL EASKABC('Options:',' ',
     &    'add period','delete period','continue',iad,1)
        if(iad.eq.1)then
          call addctldp(3,II,JJ,'A',iier)
        elseif(iad.eq.2)then
          call addctldp(3,II,JJ,'D',iier)
        endif
        goto 92
      ELSEIF(IVERT.GT.5.AND.IVERT.LT.(mvert-3))THEN

C Take the t?cps() i?ctyp(),i?claw(), ?MISCD(II,JJ,KK,?) common and
C stuff into working common array 'cm' for editing.  At end
C of if:elseif:else recover.
        call stfctl(3,II,JJ,KK)

        NITEMS=INT(cm(1))
        if(NITEMS.gt.0)then
          WRITE(outs,'(a,i3,a)')' For period',KK,' current data is...'
          call edisp(iuout,outs)
          if(NITEMS.LE.13)then
            write(outs,'(13F9.1)')(cm(L),L=2,NITEMS+1)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
          else
            write(outs,'(13F9.1)')(cm(L),L=2,13)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
            if(NITEMS.LE.26)then
              write(outs,'(13F9.1)')(cm(L),L=14,NITEMS)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            else
              write(outs,'(13F9.1)')(cm(L),L=14,26)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            endif
          endif
        else
          WRITE(outs,'(a,i3,a)')' For period',KK,' define...'
          call edisp(iuout,outs)
        endif

        call edisp(iuout,' ')
        call edisp(iuout,'Standard flux actuation enabled... ')
        ictyp=0

        H(1)='See ESP-r Data Model Section 9 for details. '
        ino=iclaw
        ilno=ino
        idno=1
        write(outs,'(a,i3,a)')'Select a control law (currently: ',
     &    iclaw,')'
        call MENUATOL(outs,' Available laws',
     &   'a Global on-off controller','b Global capacity controller',
     &   'c Global schedule on controller',' ',' ',' ',' ',' ',
     &   ' ',' ',' ',' ',ino,idno,1)
        if(ino.eq.0)then
          continue
        elseif(ilno.ne.ino)then
          iclaw=ino
          IGCLAW(II,JJ,KK)=iclaw
        endif

  62    H(1)='See ESP-r Data Model Section 9 for details. '
        CALL EASKR(tcps,' ',' Period start time ? ',
     &    0.,'F',24.,'F',0.,'control start',IER,1)
        if(KK.GT.1)then
          IF(TGCPS(II,JJ,KK-1).GE.tcps)then
            call usrmsg('Periods out of order..',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcps,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0.','W')
            GOTO 62
          endif
        endif
        TGCPS(II,JJ,KK)=tcps

        H(1)='See ESP-r Data Model Section 9 for details.'
        if(IGCLAW(II,JJ,KK).eq.1)then
           
C Global on/off control.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current global on/off controller data is:')
            call edisp(iuout,
     &      ' Mode, Minimum loops ON [mode_1], setpoint [mode_2] ')
            write(outs,'(2F9.2)')cm(2),cm(3)
            call edisp(iuout,outs)
          endif
          cm(1)=2.0

  33      WRITE(HOLD,'(2f7.2)')cm(2),cm(3)
          H(1)='See ESP-r Data Model Section 9 for details. '
          CALL EASKS(HOLD,' Mode; Min ON [mode_1] or stpt [mode_2]',
     &      '  ',42,' 1. 1. ','global on-off ctlr',IER,1)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','Mode',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,0.,'-','Min on |setpt',IER)
          if(ier.ne.0)goto 33
        elseif(IGCLAW(II,JJ,KK).eq.2)then

C Global capacity management control.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current global capacity management controller data is:')
            call edisp(iuout,
     &      ' Glb htg capy, Glb clg capy, No shed')
            write(outs,'(3F9.2)')cm(2),cm(3),cm(4)
            call edisp(iuout,outs)
          endif
          cm(1)=3.0

  35      WRITE(HOLD,'(3f7.2)')cm(2),cm(3),cm(4)
          H(1)='See ESP-r Data Model Section 9 for details. '
          CALL EASKS(HOLD,'Glb htg capy, Glb clg capy, No shed ',
     &      '  ',36,' 10000. 10000. 1.','global on-off ctlr',IER,1)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','Htg capy',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,0.,'-','Clg capy',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,0.,'-','No. shed',IER)
          if(ier.ne.0)goto 35
        elseif(IGCLAW(II,JJ,KK).eq.3)then

C Global schedule on control.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current global schedule on controller data is:')
            call edisp(iuout,
     &      ' No. of sched loops')
            write(outs,'(1F9.2)')cm(2)
            call edisp(iuout,outs)
          endif
          cm(1)=1.0

  37      WRITE(HOLD,'(1f7.2)')cm(2)
          H(1)='See ESP-r Data Model Section 9 for details. '
          CALL EASKS(HOLD,'No. of scheduled loops',
     &      '  ',22,' 2.','schedule on controller',IER,1)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','No. sched lps',IER)
          if(ier.ne.0)goto 37
        endif

C Extract from working array to gmiscd() common structure
        call extrctl(3,II,JJ,KK)
        call usrmsg(' ',' ','-')
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

 3    if(IOS.eq.2)then
        CALL USRMSG(' ','Permissions issue writing miscellaeeous data.',
     &    'W')
      else
        CALL USRMSG(' ','Problem writing miscellaeeous data.','W')
      endif
      RETURN
      END


C **************** senloc 
C senloc presents  a list of applicable sensor locations depending
C on the control domain and includes explicit zones list.
C It is passed a prompt, menu title and returns 4 sensor indicators
C (isn1,isn2,isn3,isn4). isense is the condition sensed in the case of 
C flow control, nsup is the number of supplemental items for associated
C flow connections.It is assumed that the user will make one selection only.
      subroutine senloc(prompt,title,icfoc,II,isn1,isn2,isn3,isn4,
     &  isn5,isense,nsup,errmsg,IER)
#include "building.h"
#include "net_flow.h"
#include "plant.h"
#include "epara.h"

      common/pophelp/h(60)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/C1/NCOMP,NCON
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
     
      LOGICAL SELECT,OK,dok
      CHARACTER*(*) prompt,title,errmsg

      DIMENSION SALT(3),IVALS(3),VERT(35),ICPK(MCONTM)
      CHARACTER VERT*40,KEY*1,prompt2*36
      character h*72,outs*124,SALT*40
      integer isn1T,isn2T,isn3T,isn4T,ilayt   ! temporary values for editing.

C Initialise zone menu variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
   3  SELECT=.FALSE.
      if(icfoc.eq.0)then
        MHEAD=14
      elseif(icfoc.eq.1)then
        MHEAD=13
      elseif(icfoc.eq.2)then
        MHEAD=12
      elseif(icfoc.eq.3)then
        MHEAD=2
      elseif(icfoc.eq.4)then
        MHEAD=2
      endif
      MCTL=3
      ILEN=NCOMP
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Build up text strings for the menu. 
      M=MHEAD
      if(icfoc.eq.0)then
        WRITE(VERT(1),'(a)') 'a senses current zone db temp         '
        WRITE(VERT(2),'(a)') 'b senses mix of zone db temp and MRT  '
        WRITE(VERT(3),'(a)') '  senses an ambient condition...      '
        WRITE(VERT(4),'(a)') 'c   dry bulb temperature              '
        WRITE(VERT(5),'(a)') 'd   sol-air temperature (not for flow)'
        WRITE(VERT(6),'(a)') 'e   wind speed                        '
        WRITE(VERT(7),'(a)') 'f   wind direction                    '
        WRITE(VERT(8),'(a)') 'g   diffuse horizontal solar radiation'
        WRITE(VERT(9),'(a)') 'h   direct normal solar radiation     '
        WRITE(VERT(10),'(a)')'i   external relative humidity        '
        WRITE(VERT(11),'(a)')'j references temporal file item       '
        WRITE(VERT(12),'(a)')'k uses value from function generator  '
        WRITE(VERT(13),'(a)')'l changes thermophysical properties   '
        WRITE(VERT(14),'(a)') '  ------------------------------------'
      elseif(icfoc.eq.1)then
        WRITE(VERT(1),'(a)') 'a senses output of a plant component  '
        WRITE(VERT(2),'(a)') 'b senses mix of zone db temp and MRT  '
        WRITE(VERT(3),'(a)') '  senses an ambient condition...      '
        WRITE(VERT(4),'(a)') 'c   dry bulb temperature              '
        WRITE(VERT(5),'(a)') 'd   sol-air temperature (not for flow)'
        WRITE(VERT(6),'(a)') 'e   wind speed                        '
        WRITE(VERT(7),'(a)') 'f   wind direction                    '
        WRITE(VERT(8),'(a)') 'g   diffuse horizontal solar radiation'
        WRITE(VERT(9),'(a)') 'h   direct normal solar radiation     '
        WRITE(VERT(10),'(a)')'i   external relative humidity        '
        WRITE(VERT(11),'(a)')'j references temporal file item       '
        WRITE(VERT(12),'(a)')'k uses value from function generator  '
        WRITE(VERT(13),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.2)then
        WRITE(VERT(1),'(a)') 'a senses plant component node T       '
        WRITE(VERT(2),'(a)') 'b senses mix of zone db temp and MRT  '
        WRITE(VERT(3),'(a)') '  senses an ambient condition...      '
        WRITE(VERT(4),'(a)') 'c   dry bulb temperature              '
        WRITE(VERT(5),'(a)') '    not applicable'
        WRITE(VERT(6),'(a)') 'e   wind speed                        '
        WRITE(VERT(7),'(a)') 'f   wind direction                    '
        WRITE(VERT(8),'(a)') 'g   diffuse horizontal solar radiation'
        WRITE(VERT(9),'(a)') 'h   direct normal solar radiation     '
        WRITE(VERT(10),'(a)')'i   external relative humidity        '
        WRITE(VERT(11),'(a)')'j senses flow node or connection      '
        WRITE(VERT(12),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.3)then
        WRITE(VERT(1),'(a)') 'a sensed condition not used           '
        WRITE(VERT(2),'(a)') '  ------------------------------------'
      elseif(icfoc.eq.4)then
        WRITE(VERT(1),'(a)') 'a power control not yet implemented   '
        WRITE(VERT(2),'(a)') '  ------------------------------------'
      endif
      M=M+1
      CALL EMKEY(M,KEY,IER)
      WRITE(VERT(M),'(2a)')KEY,' senses temp in a specific zone'

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text and info on portion seen.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)= '  ------------------------------------' 
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT ('0 page part: ',I1,' -- of:',I1)
      ENDIF
      VERT(M+2)  ='? help                  '
      VERT(M+3)  ='- exit this menu        '
      write(prompt2,'(a)')' '

C Help text for this menu.
      H(1)='Depending on the control domain a sensor can be located'
      H(2)='at one of many places to measure specified parameters of'
      h(3)='the model.'

C Display the menu.
      call usrmsg(PROMPT,PROMPT2,'-')
      CALL EMENU(title,VERT,MVERT,IVERT)
      IF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then display error message.
        IF(.NOT.SELECT)then
          call usrmsg(errmsg,'Please select a menu option!','W')
          IVERT=-2
          goto 92
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text.
        CALL PHELPD('zone geom file section',3,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Ask for the specific zone.
        CALL EPMENSV
        idef=0
        call askzone(isn1,idef,
     &   'Specific zone associated with this sensor','Select zone','-',
     &    'Specific zone',ier)
        CALL EPMENRC
        SELECT=.TRUE.
        H(1)='This value specifies sensor location in greater'
        H(2)='detail. If >0 then refers to a specific surface.'
        CALL EASKABC('Specific location: ',' ','zone air point',
     &    'surface','continue',IW,2)
        if(IW.eq.1)then
          isn2=0
          isn3=0
        elseif(IW.eq.2)then
          CALL EPMENSV
          call EASKSUR(isn1,isn2,'-','Available surfaces.',' ',IER)
          CALL EPMENRC
          H(1)='If = 0 then `inside` face of surface, '
          H(2)='If > 0 then refers to a node within surface and'
          H(3)='       this option may not result in a reliable'
          H(4)='       control so avoid selecting a node.' 
          CALL EASKABC('Specific location in surface: ',' ',
     &      'at inside face','node within surface (see help)',
     &      'continue',IWN,4)
          if(IWN.eq.1)then
            isn3=0
          elseif(IWN.eq.2)then

C Work with local variables and only instanciate if ier is zero.
            ilayt=0
            isn3t=isn3
            call asksnode(isn1,isn2,ilayt,isn3t,ier)
            if(ier.eq.0)then
              ilay=ilayt
              isn3=isn3t
            else
              goto 3
            endif
          elseif(IWN.eq.3)then
            goto 3
          endif
        elseif(IW.eq.3)then
          goto 3
        endif
        nsup=0

C In the case of mass flow this choice implies no supplemental
C items, but return isense as the control law.
        if(icfoc.eq.2)isense=1
      elseif(ivert.eq.1.and.(icfoc.eq.0))then

C Senses current zone db temp if ideal.
        isn1=0
        isn2=0
        isn3=0
        nsup=0
      elseif(ivert.eq.1.and.(icfoc.eq.1.or.icfoc.eq.2))then

C Senses output (plant control) or temperature (flow control) of a plant component.
C Offer the user a cancel option.
        isn1T=-1
        H(1)='This value specifies the plant component index in '
        H(2)='the plant network definition file. '
        H(3)=' '
        H(4)='If you choose cancel the controller data will not be'
        H(5)='altered and you will be returned to the main menu. '
        isn2T=isn2
        CALL ASKPCMP('Plant component for sensor ? ','-',IP,IER)
        if(IP.eq.0) goto 3
        ISN2T=IP
        H(1)='This value specifies the plant component node (see '
        H(2)='the plant component database). '
        H(3)=' '
        H(4)='If you choose cancel the controller data will not be'
        H(5)='altered and you will be returned to the main menu. '
        isn3T=isn3

        if(nnodes(ip).eq.1) then
          ISN3T=1
        elseif(nnodes(ip).gt.1) then
           call getnod(ip,knode)
           if(knode.eq.0) goto 3
           ISN3T=knode
        endif


C Optionally allow user to detect the temperature between
C two nodes. 
        H(1)='When measuring the temperature in a node of a plant'
        H(2)='component, a sensor can compare this value to the'
        H(3)='temperature in another node in the plant network.'
        H(4)='Choosing `yes` configures the sensor to measure the'
        H(5)='difference in temperature between two nodes, while'
        H(6)='`no` configures the sensor to report the temperature'
        H(7)='in one node.'

        call EAskAB('Does sensor measure the temperature ',
     &              'difference between two nodes?',
     &              'yes', 'no', iWhich, 7 )



C If user has requested measurement of temperature difference,
C determine second component and node. 
        if ( iWhich == 1 ) then
C User has opted ti measure temperature difference
C          H(1)='This value specifies the second plant component '
C          H(2)='index in the plant network definition file. The '
C          H(3)='temperature of a node in this plant component   '
C          H(4)='will be subtracted from the temperature in the  '
C          H(5)='first plant component.'
C          H(6)=' '
C          H(7)='If you choose cancel, a second component will not '
C          H(8)='be specified, and the sensor will only report the '
C          H(9)='the temperature measured in one plant component.'
          
C Get plant component index
          CALL ASKPCMP('Plant component for sensor ? ','-',IP,IER)
          isn4T=IP

C Get node in plant component
          if(IP == 0 ) then
            ISN5T = 0
          elseif ( nnodes(ip) == 1) then
            ISN5T=1
          elseif(nnodes(ip) > 1) then
            call getnod(ip,knode)
            ISN5T=knode
          endif

        else
C User has declined to measure difference between nodes.
          ISN4T = 0
          ISN5T = 0

        endif

C If the user has not canceled instanciate from temporary values.
        nsup=0
        isn1=isn1T
        isn2=isn2T
        isn3=isn3T
        isn4=isn4T
        isn5=isn5T
        
      elseif(ivert.eq.1.and.icfoc.eq.3)then

C Sensed condition does not matter.
        isn1=0
        isn2=0
        isn3=0
        nsup=0
      elseif(ivert.eq.1.and.(icfoc.eq.4))then

C Power control sensor not yet implemented, return zeros.
        isn1=0
        isn2=0
        isn3=0
        nsup=0
      elseif(ivert.eq.2.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2))then

C Senses mix of zone db temp and MRT.
        isn1T=-2
        CALL EPMENSV
        idef=0
        isn2T=isn2
        call askzone(isn2T,idef,'Zone for sensor','Select zone','-',
     &    'MRT sensor',ier)
        CALL EPMENRC
        H(1)='The convective weighting factor is the percent of'
        H(2)='overall energy exchange that is convective. '
        H(3)=' '
        H(4)='If you choose cancel the controller data will not be'
        H(5)='altered and you will be returned to the main menu. '
        isn3T=isn3
        CALL EASKI(isn3T,' ',' Convective weighting factor (%) ?',
     &      0,'F',100,'-',50,'convective weighting',IERI,5)
        if(ieri.eq.-3) goto 3

C In the case of mass flow this choice implies no supplemental
C items, but return isense as the control law.
        if(icfoc.eq.2)isense=1

C If the user has not canceled instanciate from temporary values.
        nsup=0
        isn1=isn1T
        isn2=isn2T
        isn3=isn3T
      elseif(ivert.eq.4.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2))then

C Senses outside db.
        nsup=0
        isn1=-3
        isn2=0
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.5.and.
     &      (icfoc.eq.0.or.icfoc.eq.1))then

C Senses sol-air temperature.
        nsup=0
        isn1=-3
        isn2=1
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.6.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2))then

C Senses wind speed.
        nsup=0
        isn1=-3
        isn2=2
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.7.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2))then

C Senses wind direction.
        nsup=0
        isn1=-3
        isn2=3
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.8.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2))then

C Senses diffuse horizontal solar radiation.
        nsup=0
        isn1=-3
        isn2=4
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.9.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2))then

C Senses direct normal solar radiation.
        nsup=0
        isn1=-3
        isn2=5
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.10.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2))then

C Senses external relative humidity.
        nsup=0
        isn1=-3
        isn2=6
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.11.and.(icfoc.eq.0.or.icfoc.eq.1))then

C Uses information in temporal file.
C << check whether temporal file associated with model. >>
        isn1T= -5
        H(1)='A temporal file holds timestep data including setpoints'
        H(2)='for use with zone or plant controls.   '
        H(3)=' '
        H(4)='If you choose cancel the controller data will not be'
        H(5)='altered and you will be returned to the main menu. '
        isn2T=isn2
        CALL EASKI(isn2T,'Index of the temporal item to associate with',
     &    ' this sensor: ',0,'F',0,'-',1,'temporal file item',IERI,5)
        isn3T=0
        if(ieri.eq.-3) goto 3
        isn1=isn1T
        isn2=isn2T
        isn3=isn3T
      elseif(ivert.eq.11.and.(icfoc.eq.2))then

C Senses network flow node or connection. 
C  isn1= -4  always
C  isn2  index node/connection
C  isn3  2nd index node (for differences), or contaminant number
C  isn4=  0 node, 1=connection (tie in with type later), 2 contaminant control
        H(1)='A typical flow sensor is at a node in the mass flow '
        H(2)='network.  Only in the case of sensing 1st or 2nd phase'
        H(3)='mass flow would a specific connection be used as'
        H(3)='the sensed point. '
        CALL EASKABC(' ',' Flow sensor at:',' flow node (typical) ',
     &    ' specific flow connection (see help) ','continue',IW,3)
        if(IW.eq.1)then

C Flow controller. Note: there is only one `type` per flow function 
C if actuation is at a component.
C Note: isense will eventually be passed to IFCTYP(II,1,1)=isense
  93      isn1= -4
          isn4=0
          H(1) ='Synopsis of sensed properties: '
          H(2) ='a) Nodes linked to thermal zones will take their'
          H(3) =' temperature the last know temperature of the zone.'
          H(4) =' Many users prefer to sense flow node temperatures'
          H(5) =' rather than sense the thermal thermal zones directly.'
          H(6) =' '
          H(7) ='b) enthalpy is know at each node and can be sensed.'
          H(8) =' '
          H(9) ='c) additional plant output might be something like'
          H(10)=' the demand (W) of the component.'
          H(11)=' '
          H(12)='d/e/g/h) the Delta selections are in the form of'
          H(13)=' the value at the 1st node of the connection minus'
          H(14)=' the value at the 2nd node of the connection.'
          H(15)=' For example: if a window should open if the ambient'
          H(16)=' temperature is less than a room node temperature and'
          H(17)=' the connection was defined from ambient to room then'
          H(18)=' the delta T should be negative for an ON state.'
          H(19)=' '
          H(20)='i) contanimate concentration - the units are '
          H(21)=' typically g/kg .....'
          H(22)=' '
          ino=IFCTYP(II,1,1)
          idno=1
          write(outs,'(2a,i3,a)')'Select a property to sense ',
     &     '(currently:',IFCTYP(II,1,1),')'
          call MENUATOL(outs,' Sensed Property',
     &     'a dry bulb temperature at node','b enthalpy at node',
     &     'c additional plant output','d delta T between nodes',
     &     'e absolute delta T between nodes','f pressure at node',
     &     'g delta P between nodes',
     &     'h abs delta P between nodes',
     &     'i contaminant concentration at node',' ',' ',' ',
     &     ino,idno,22)
          if(ino.eq.0)then
            call usrmsg('You did not select a valid control law.',
     &        'Please redefine.','W')
            goto 93
          endif

C Depening on the users selection set isense and nsup.
          nsup=1
          if(ino.eq.1)isense=1
          if(ino.eq.2)isense=3
          if(ino.eq.3)isense=9
          if(ino.eq.4)then
            isense=24
            nsup=2
          endif
          if(ino.eq.5)then
            isense=25
            nsup=2
          endif
          if(ino.eq.6)isense=26
          if(ino.eq.7)then
            isense=27
            nsup=2
          endif
          if(ino.eq.8)then
            isense=28
            nsup=2
          endif
          if(ino.eq.9)then
            NCPK=1
            NH=0
            CALL ASKMFCTM(NCPK,ICPK,'choose','contaminant',NH)
            H(1)=' You need to set contaminant concentrations in g/kg'
            H(2)=' in the control law and not kg/kg as elsewhere'
            CALL PHELPD(' ',2,'-',0,0,IER)
            isn3=icpk(1)
            isn4=2
            isense=41
          endif
        elseif(IW.eq.2)then
          isn1= -4
          SALT(1)='senses 1st ph mass flow at connection  ' 
          SALT(2)='senses 2nd ph mass flow at connection  ' 
          SALT(3)='senses abs mass flow rate at connection' 
          IX=1
          H(1)='See ESP-r Data Model Section 9 for details.'
          CALL EPICKS(IX,IVALS,' ',' Controller type: ',
     &         40,3,SALT,'controller type',IER,1)
          if(ix.ne.0)then
            nsup=2
            if(IVALS(1).eq.1)isense=5
            if(IVALS(1).eq.2)isense=7
            if(IVALS(1).eq.3)isense=29
          endif
          CALL EPMENSV
          call ASKCON('  flow connections','-',INS,INE,IC,IER)
          CALL EPMENRC
          if(IC.gt.0)then
            isn2=IC
            isn3=0
            isn4=1
          endif
        elseif(IW.eq.3)then
          return
        endif
      elseif(ivert.eq.11.and.(icfoc.eq.5))then

C Sense radiation at exterior of surface.
        isn1= -7
        isn2=0
        isn3=0
        isn4=0
      elseif(ivert.eq.12.and.(icfoc.eq.0.or.icfoc.eq.1))then

C Uses value from function generator. 
        isn1T= -6
        H(1)=' The available functions are:'
        H(2)=' '
        H(3)='   Function          supp1    supp2    supp3   supp4'
        H(4)='   ================================================ '
        H(5)='   1. Step:          start    finish   max     min  '
        H(6)='   2. Ramp:          start    finish   max     min  '
        H(7)='   3. Square sine:   max      min      freq    ---  '
        H(8)='   4. Square cosine: max      min      freq    ---  '
        H(9)='   5: Triangular:    max      min      freq    ---  '
        H(10)='  6: Saw-tooth:     max      min      freq    ---  '
        H(11)='  7: Sine:          max      min      freq    shift'
        H(12)='  8: Cosine:        max      min      freq    shift'
        H(13)='  9: Sensed prop`y: inc/dec  ---      ---     ---  '
        H(14)=' - Refer ESP-r Data Model section 9 ' 
        isn2T=isn2
        CALL EASKI(isn2T,' ',' Required mathematical function ?',
     &      1,'F',9,'F',1,'generated function',IER,14)
        if(ieri.eq.-3) goto 3
        isn1=isn1T
        isn2=isn2T
      elseif(ivert.eq.12.and.(icfoc.eq.5))then

C Sense lux via daylight coef method. << ? if other info required >>
        isn1= -8
        isn2=0
        isn3=0
        isn4=0
      elseif(ivert.eq.13.and.(icfoc.eq.0))then

C Changes thermophysical properties.
        isn1T= -99
        H(1)='Upper zone air temperature limit (above which sub- '
        H(2)='stitution occurs. '
        isn2T=isn2
        CALL EASKI(isn2T,' Upper zone air temperature limit',
     &    ' (integer) above which substitution occurs ? ',
     &    0,'-',0,'-',30,'upper air',IERI,2)
        if(ieri.eq.-3) goto 3

        H(1)='Lower zone air temperature limit (below which sub- '
        H(2)='stitution occurs. '
        isn3T=isn3
        CALL EASKI(isn3T,' Lower zone air temperature limit',
     &    ' (integer) below which substitution occurs ? ',
     &    0,'-',0,'-',0,'lower air',IERI,2)
        if(ieri.eq.-3) goto 3
        isn1=isn1T
        isn2=isn2T
        isn3=isn3T
      else

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      endif

C Query user for any nested controls (unless thermo substitution).
      if(icfoc.eq.0.and.isn1.ne.-99)then
        dok=.false.
        h(1)='Most users should say no at this point. Nested functions'
        h(2)='are only used in support of thermophysical property '
        h(3)='substitution controls (and if you say yes you will be'
        h(4)='asked for the index of the control loop that defines '
        h(5)='the substitution). '
        CALL ASKOK(' Are there any nested control functions ?',
     &    ' (currently only property substitution allowed) ',OK,
     &    dok,5)
        if(OK)then
          H(1)='Index of the control function to point to.'
          H(2)='Note that control must be control type -99.'
          H(3)='A cancel restores the previous values. '
          isn4T=isn4
          CALL EASKI(isn4T,' ',
     &      ' Control function (index) to be nested: ',
     &        0,'F',0,'-',1,'nested control',IERI,3)
          if(ieri.eq.-3) then
            continue
          else
            isn4=isn4T
          endif
        else
          isn4=0
        endif
      endif
      return
      END


C **************** actloc 
C actloc presents  a list of applicable actuator locations depending
C on the control domain and includes explicit zones list.
C It is passed a prompt, menu title and returns 3 actuator indicators
C (isn1,isn2,isn3,isn4). isense is the condition sensed in the case of 
C flow control, nsup is the number of supplemental items for associated
C flow connections.It is assumed that the user will make one selection only.
      SUBROUTINE actloc(prompt,title,icfoc,II,isn1,ian1,ian2,ian3,
     &  errmsg,IER)
#include "building.h"
#include "net_flow.h"
#include "plant.h"
#include "epara.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C1/NCOMP,NCON
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
      common/fctl4/iasocc(mcc,MCMP),nfsup(mcc)
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
       common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      LOGICAL SELECT
      CHARACTER*(*) prompt,title,errmsg

      DIMENSION VERT(35)
      CHARACTER VERT*40,KEY*1,zname*12,zdesc*64,prompt2*36
      character h*72,outs*124,LTPCMP*60,CMNAM*12,NDNAM*12
      integer ian1T,ian2T,ian3T,ilayt  ! local variable for editing.

C Initialise zone menu variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
   3  SELECT=.FALSE.
      if(icfoc.eq.0)then
        MHEAD=3
      elseif(icfoc.eq.1)then
        MHEAD=2
      elseif(icfoc.eq.2)then
        MHEAD=3
      elseif(icfoc.eq.3)then
        MHEAD=2
      elseif(icfoc.eq.4)then
        MHEAD=2
      endif
      MCTL=3
      ILEN=NCOMP
      IPACT=CREATE
      CALL EKPAGE(IPACT)

      if(icfoc.eq.0.and.isn1.eq.-99)then

C If sensor location -99 ask for up to three surfaces where substi-.
C tutions will occur. << revise to selection list at some point >>.
C Offer the user option to cancel.
        H(1)='Up to three surfaces may have substitute properties.'
        H(2)='The number required is the index in the surface.'
        H(3)='A zero signals no substitution as follows: '
        H(4)=' three non-zero indicies will switcheds three'
        H(5)=' two non-zero followed by a zero switches two'
        H(6)=' one non-zero followed by two zeros switches one'
        H(7)='If you cancel then data will revert to previous vlues.'
        ian1T=ian1
        CALL EASKI(ian1T,' ',' First surface index (see help)?',
     &     0,'F',MS,'F',1,'1st surf for subst',IERI,7)
        if(ieri.eq.-3) return
        ian2T=ian2
        CALL EASKI(ian2T,' ',' Second surface index (see help)?',
     &     0,'F',MS,'F',1,'2nd surf for subst',IERI,7)
        if(ieri.eq.-3) return
        ian3T=ian3
        CALL EASKI(ian3T,' ',' Third surface index (see help)?',
     &     0,'F',MS,'F',1,'3rd surf for subst',IERI,7)
        if(ieri.eq.-3) return
        ian1=ian1T
        ian2=ian2T
        ian3=ian3T
        return
      endif

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Build up text strings for the menu. 
      M=MHEAD
      if(icfoc.eq.0)then
        WRITE(VERT(1),'(a)')'a at current zone air point           '
        WRITE(VERT(2),'(a)')'b mix of convection and radiation     '
        WRITE(VERT(3),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.1)then
        WRITE(VERT(1),'(a)')'a node within a plant component       '
        WRITE(VERT(2),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.2)then
        WRITE(VERT(1),'(a)')'a single flow connection              '
        WRITE(VERT(2),'(a)')'b flow component (& assoc connections)'
        if(IFCTYP(II,1,1).eq.41)vert(2)='b contaminant source      '
        WRITE(VERT(3),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.3)then
        WRITE(VERT(1),'(a)')'a actuator not used                   '
        WRITE(VERT(2),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.4)then
        WRITE(VERT(1),'(a)')'a power actuator not yet implemented  '
        WRITE(VERT(2),'(a)')'  ------------------------------------'
      endif
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          if(icfoc.eq.0)then
            M=M+1
            CALL EMKEY(M,KEY,IER)
            WRITE(VERT(M),'(3a)')KEY,' air point or surf in ',zname(L)
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text and info on portion seen.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=  '  ------------------------------------'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT ('0 page part: ',I1,' -- of:',I1)
      ENDIF
      VERT(M+2)  ='? help                  '
      VERT(M+3)  ='- exit this menu        '
      write(prompt2,'(a)')' '

C Help text for this menu.
      H(1)='Depending on the control domain actuation can be located'
      H(2)='at one of many locations within the model.'

C Display the menu.
      call usrmsg(PROMPT,PROMPT2,'-')
      CALL EMENU(title,VERT,MVERT,IVERT)
      IF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then display error message.
        IF(.NOT.SELECT)then
          call usrmsg(errmsg,' Please select a menu option. ','W')
          IVERT=-2
          goto 92
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text.
        CALL PHELPD('control actuation point',2,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potentially long list to the zone number via KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        ian1 = IFOC
        H(1)='This value specifies sensor location in greater'
        H(2)='detail. If >0 then refers to a specific surface.'
        CALL EASKABC('Specific location: ',' ','zone air point',
     &    'surface','continue',IW,2)
        if(IW.eq.1)then
          ian2=0
          ian3=0
        elseif(IW.eq.2)then
          CALL EPMENSV
          call EASKSUR(ian1,ian2,'-','Available surfaces.',' ',IER)
          CALL EPMENRC
          H(1)='If = 0 then `inside` face of surface, '
          H(2)='If >0 then refers to a node within surface.'
          CALL EASKABC('Specific location in surface: ',' ',
     &      'at inside face','node within surface','continue',IWN,2)
          if(IWN.eq.1)then
            ian3=0
          elseif(IWN.eq.2)then

C Work with local variables and only instanciate if ier is zero.
            ilayt=0
            ian3t=ian3
            call asksnode(ian1,ian2,ilayt,ian3t,ier)
            if(ier.eq.0)then
              ilay=ilayt
              ian3=ian3t
            else
              goto 3
            endif
          elseif(IWN.eq.3)then
            goto 3
          endif
        elseif(IW.eq.3)then
          goto 3
        endif
      elseif(ivert.eq.1.and.(icfoc.eq.0))then 

C Actuates current zone air point.
        ian1=0
        ian2=0
        ian3=0
      elseif(ivert.eq.1.and.(icfoc.eq.1))then

C Within a plant component (offer the user a cancel option).
        ian1T=-1
        H(1)='This value specifies the plant component index in '
        H(2)='the plant network definition file. '
        H(3)='A cancel restores the previous value. '
        IP=-1
        CALL ASKPCMP('Plant component for actuator ? ','-',IP,IER)
        if(IP.le.0) goto 3
        IAN2T=IP

        if(nnodes(ip).eq.1) then
          IAN3T=1
        elseif(nnodes(ip).gt.1) then
          knode=-1
           call getnod(ip,knode)
           if(knode.le.0) goto 3
           IAN3T=knode
        endif
 
        if(ieri.eq.-3) goto 3
        ian1=ian1T
        ian2=ian2T
        ian3=ian3T
      elseif(ivert.eq.1.and.(icfoc.eq.2))then

C At a flow connection.
        ian1= -3
        CALL EPMENSV
        call ASKCON(' Flow connection?','-',INS,INE,IC,IER)
        CALL EPMENRC
        write(outs,'(6A)') ' Selected ',NDNAM(NODPS(IC)),
     &    ' - ',NDNAM(NODNE(IC)),' via ',CMNAM(ITPCON(IC))
        call edisp(iuout,outs)
        if(IC.gt.0)then
          ian2=IC
          ian3=0
        endif

C If sensor is a -4 and connection type ask for specific
C supplemental nodes.
        if(ifsn(ii,1).eq.-4.and.ian1.eq.-3)then
          CALL EPMENSV
          call ASKRNOD('  sensed flow node','-',INOD,IER)
          CALL EPMENRC
          if(INOD.gt.0)then
            ifsn(ii,2)=INOD
            if(nfsup(ii).eq.2)then
              CALL EPMENSV
              call ASKRNOD(' 2nd sensed flow node','-',INOD,IER)
              CALL EPMENRC
              if(INOD.gt.0)then
C << pev was ifan(ii,3) which was probably a typo >>
                ifsn(ii,3)=INOD
                ian3=INOD
              endif
            else
              ian3=0
            endif
          endif
        endif
      elseif(ivert.eq.1.and.icfoc.eq.3)then

C Sensed condition does not matter.
        ian1=0
        ian2=0
        ian3=0
      elseif(ivert.eq.1.and.(icfoc.eq.4))then

C Power control sensor not yet implemented, return zeros.
        ian1=0
        ian2=0
        ian3=0
      elseif(ivert.eq.2.and.(icfoc.eq.0))then

C Actuates radiant convective mix.
        ian1T=-2
        CALL EPMENSV
        idef=0
        ian2T=ian2
        call askzone(ian2T,idef,'Zone for actuation','Select zone','-',
     &    'mixed injection',ier)
        CALL EPMENRC
        H(1)='The convective weighting factor is the percent of'
        H(2)='overall energy exchange that is convective. '
        H(3)='A cancel restores the previous value. '
        ian3T=ian3
        CALL EASKI(ian3T,' ',' Convective weighting factor (%) ?',
     &      0,'F',100,'-',50,'convective weighting',IERI,3)
        if(ieri.eq.-3) goto 3
        ian1=ian1T
        ian2=ian2T
        ian3=ian3T
      elseif(ivert.eq.2.and.(icfoc.eq.2))then

C At a flow component.
 742    ian1= -4
        CALL EPMENSV
        call ASKRCMP(' Flow component?','-',ICMP,IER)
        CALL EPMENRC
        if((ITPCMP(ICMP).EQ.220.OR.ITPCMP(ICMP).EQ.230.OR.
     &    ITPCMP(ICMP).EQ.240.OR.ITPCMP(ICMP).EQ.250))THEN
          WRITE(outs,'(a,i3,a)') ' Sorry component type ',ITPCMP(ICMP),
     &                           ' cannot be actuated.'
          call edisp(itru,outs)
          goto 742
        elseif(icmp.eq.0)then
          goto 742
        ENDIF
        ian2=ICMP

        call edisp(iuout,' ')
        call edisp(iuout,'A mass flow component (such as a orifice)')
        call edisp(iuout,'may be controlled.  For each instance of a')
        call edisp(iuout,'component that is controlled on a sensed')
        call edisp(iuout,'condition at another node you will be asked')
        call edisp(iuout,'to nominate the node(s). ')
        call edisp(iuout,'A node must be specified for each instance!')
        ian3=0

C Loop through each of the connections and test for a matching
C component type.
        do 842 icl=1,NCNN
          if(CMNAM(ICMP)(1:12).eq.CMNAM(ITPCON(ICL))(1:12))then
            write(outs,'(7A)') 'For connection ',
     &        NDNAM(NODPS(ICL)),' - ',NDNAM(NODNE(ICL)),' via ',
     &        CMNAM(ITPCON(ICL)),' ...'
            call edisp(iuout,outs)
            ian3=ian3+1
            iasocc(II,ian3)=icl

C if isn1.eq.-3 then no supplemental.
            if(nfsup(II).eq.0)then
              ISDCNN(ICL)=0
              NDSCNN(ICL,1)=0
              NDSCNN(ICL,2)=0
              call edisp(iuout,' No supplemental data is required.')
            elseif(nfsup(II).eq.1)then
              ISDCNN(ICL)=1
              CALL EPMENSV
              call ASKRNOD('control on node','-',iv,IER)
              CALL EPMENRC
              NDSCNN(ICL,1)=iv
              NDSCNN(ICL,2)=0
            elseif(nfsup(II).eq.2)then
              ISDCNN(ICL)=2
              CALL EPMENSV
              call ASKRNOD(' 1st sensed node name','-',iv,IER)
              CALL EPMENRC
              NDSCNN(ICL,1)=iv
              CALL EPMENSV
              call ASKRNOD(' 2nd sensed node name','-',iv,IER)
              CALL EPMENRC
              NDSCNN(ICL,2)=iv
            endif
          endif
 842    continue
      else

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      endif

      return
      END

C ************************ ASKMFCTM ************************
C ASKMFCTM: Asks for one or more contaminants
C PROMPT1 and PROMPT2 are text prompt strings
C NH is number of help strings.

      SUBROUTINE ASKMFCTM(NCPK,ICPK,PROMPT1,PROMPT2,NH)
#include "net_flow.h"

      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      DIMENSION ICPK(MCONTM)

      CHARACTER VERTC(MCONTM)*12, CONTMNAM*12
      CHARACTER*(*) PROMPT1, PROMPT2

C Set up array of menu strings.
      if(NCONTM.gt.0)then
        do 10 I=1,NCONTM
          VERTC(I)=CONTMNAM(I)
 10     continue
        call EPICKS(NCPK,ICPK,PROMPT1,PROMPT2,
     &    12,NCONTM,VERTC,'Contaminant select',IER,NH)
      else
        call usrmsg('There are no contaminants to select.',' ','W')
      endif
      return
      end

C ***************** editctlperiod
C Editctlperiod is a generic menu for editing details of a control
C period for a zone control (later flow control). Works with
C the common block sctl which is assumed to have already been filled.
C  head (48 char) title for the menu
C  icfoc (int) is the control domain
C  icloop (int) is the index of the control loop
C  iday (int) is the current day index
C  iper (int) is the current period
C  nper (int) how many periods have been defined.
C  loopdaytype (int) the number of day types defined for this loop
C  prevstart (real) start time of previous period.
C  act (1 char) is ...
C  ier is greater than one if the user has selected a control law
C    that is not supported yet. The calling routine should then
C    deal with the dialogs using standard sequential method.
      subroutine editctlperiod(head,icfoc,icloop,iday,iper,nper,
     &  loopdaytype,prevstart,act,ier)
#include "building.h"
#include "control.h"
#include "epara.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)
      common/sctl/tcps,ictyp,iclaw,cm(misc)

C menu (32 char) menu text for each item
C prompt1 (72 char) first easkr prompt
C prompt2 (72 char) second easkr prompt
C ndec (int) number of decimal places to display (if -1 then do not
C   display the data)
C vmin (real) minimum value acceptable
C vmax (real) maximum value acceptable
C vdef (real) initial/default value
C nctlhelp (int) number of help lines
C nmenulines (int) one less than int(cm(1)) with exceptions
C   for control law one.
      common/sctlmenu/menu(misc),prompt1(misc),prompt2(misc)
      common/sctldata/ndec(misc),vmin(misc),vmax(misc),
     &  vdef(misc),nctlhelp,nmenulines

      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

      character act*1,h*72
      DIMENSION VERT(35),SALT(24),IVALS(24)
      dimension cmlocal(misc)    ! for local editing of cm data.
      real cmlocal
      integer ivals              ! for return of selection array.

      CHARACTER VERT*45,KEY*1,SSTR*72,TMP*72,ltmp*248,SALT*42
      character menu*32,prompt1*72,prompt2*72
      character outs*124,head*48

      integer icfoc,icloop,iday,iper,nper,loopdaytype  ! passed parameters
      logical close,close1,close2,close3,modval,OK,dok ! to test if variable has changed.
      real tcpst  ! for local editing.
      integer imasctlindex    ! index of master control used by slave controller.
      integer iclawcurrent    ! to remember the current control law
      integer ivalue,imini,imaxi,idefa  ! local values for editing as integer

C Set initial logical values.
      close=.true.
      close1=.true.
      close2=.true.
      modval=.false.

C If user changed the control law or major aspects of the control then
C we need to return to this point and update the cm data prior to
C setting up the menu.
  90  if(modval)then
        do 14 ij=1,misc
          cm(ij)=cmlocal(ij)
  14    continue
      endif

C Fill the control period menu and editing range arrays.
      call stfmenu(icfoc,ier)

C Copy cm data to cmlocal.
      tcpst=tcps
      do 11 ij=1,misc
        cmlocal(ij)=cm(ij)
  11  continue

C List current control data for this loop (unless changes are pending).
      if(.NOT.modval) call LSTCNTLD(iuout,icfoc,icloop,iday,IER)

C Define the static portions of the menu.
   91 MHEAD=6
      MCTL=5
      ILEN=nmenulines   ! number of miscel items to edit set in stfmenu
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=nmenulines   ! number of miscel items to edit set in stfmenu
      IVERT=-3

      if(loopdaytype.eq.0)then
        if(iday.eq.1)then
          write(VERT(1),'(a,i2,a,i2)') '  Loop ',icloop,
     &     '  day: Weekday  period: ',iper
        elseif(iday.eq.2)then
          write(VERT(1),'(a,i2,a,i2)') '  Loop ',icloop,
     &     '  day: Saturday  period: ',iper
        elseif(iday.eq.3)then
          write(VERT(1),'(a,i2,a,i2)') '  Loop ',icloop,
     &     '  day: Sunday  period: ',iper
        endif
      else
        write(VERT(1),'(a,i2,a,i2,a,i2)') '  Loop ',icloop,
     &    '  day type:',iday,' period: ',iper
      endif 
      write(VERT(2),'(a)')         '  Sensed & actuated property is... '
      CALL EVCNTRL(icfoc,icloop,iday,iper,'T',SSTR)
      write(VERT(3),'(2a)')        '    ',SSTR(1:40)
      write(VERT(4),'(a)')         '  _________________________________'
      write(VERT(5),'(a,f6.3)')    '1 Starting at: ',tcpst

C Display the control law from the file or if modval is true then
C use the value set when the user selected the other control.
      if(modval)then
        continue
      else
        CALL EVCNTRLAW(icfoc,icloop,iday,iper,TMP,LTMP)
      endif
      write(VERT(6),'(2a)')        '2 Law: ',TMP(1:38)

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
C Notestart at cm(1) (typically blank) unless a change in the
C number of items is needed to swap between minor control
C variants (e.g. PID controler).
    3 M=MHEAD
      if(ILEN.eq.1) goto 19   ! if nothing to edit jump around menu logic
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if(ndec(L).eq.-1)then
            WRITE(VERT(M),'(3a)')KEY,' ',menu(L)
          elseif(ndec(L).eq.0)then
            WRITE(VERT(M),'(4a,F9.0)')KEY,' ',menu(L),':',cmlocal(L)
          elseif(ndec(L).eq.1)then
            WRITE(VERT(M),'(4a,F9.1)')KEY,' ',menu(L),':',cmlocal(L)
          elseif(ndec(L).eq.2)then
            WRITE(VERT(M),'(4a,F9.2)')KEY,' ',menu(L),':',cmlocal(L)
          elseif(ndec(L).eq.3)then
            WRITE(VERT(M),'(4a,F9.3)')KEY,' ',menu(L),':',cmlocal(L)
          elseif(ndec(L).eq.4)then
            WRITE(VERT(M),'(4a,F9.4)')KEY,' ',menu(L),':',cmlocal(L)
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
   19 MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ________________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF

C Offer switch to previous or next period.
      if(iper.gt.1.and.iper.lt.nper)then
        VERT(M+2)='+ Shift to earlier or later period'
      elseif(iper.eq.1)then
        VERT(M+2)='+ Shift to later period          '
      elseif(iper.eq.nper)then
        VERT(M+2)='+ Shift to earlier period        '
      endif
      VERT(M+3)  ='! List details                   '
      VERT(M+4)  ='? Help                           '
      VERT(M+5)  ='- Exit                           '

C Now display the menu.
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.eq.1.or.IVERT.eq.2.or.IVERT.eq.3.or.IVERT.eq.4)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        if(modval)then
          dok=.false.
          CALL ASKOK(' The control period data has changed.',
     &      ' Accept changes?',OK,dok,nctlhelp)
          if(ok)then
            tcps=tcpst
            do 13 ij=1,misc
              cm(ij)=cmlocal(ij)
  13        continue
            call extrctl(icfoc,icloop,iday,iper)
            call edisp(iuout,
     &        'Data accepted (but not yet saved to file)!')
            modval=.false.

C << consider this as a place to also update the control file >>

          endif
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the vertex menu.
        CALL PHELPD('period control',nctlhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List control details.
        if(modval)then
          call usrmsg(
     &      'Listing only shows data saved in file. While some data',
     &      'has changed these have not been saved to file.','W')
        else
          call LSTCNTLD(iuout,icfoc,icloop,iday,IER)
        endif
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Shift to previous or next control period.
        call edisp(iuout,'Switch periods not yet implemented.')
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.5)THEN

C Period start time. Edit and if it changes then check and see if
C the previous period is before it otherwise warn user.
  62    CALL EASKR(tcpst,' ',' Period start time ? ',
     &    0.,'F',24.,'F',0.,'control start',IER,nctlhelp)
        if(iper.GT.1)then
          IF(prevstart.GE.tcpst)then
            call usrmsg('Periods out of order.',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcpst,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0.','W')
            GOTO 62
          endif
        endif
        CALL ECLOSE(tcpst,tcps,0.001,close)
        if(.NOT.close)then
          modval=.true.
          goto 92
        endif
      ELSEIF(IVERT.EQ.6)THEN

C Control law (depending on the control domain). Remember the current
C control law to trap changes.
        iclawcurrent=iclaw
        if(ictyp.eq.0)then
          SALT(1)= 'Basic controller for heating/cooling   '
          SALT(2)= 'Free-float controller                  '
          SALT(3)= 'Basic pre-heat or pre-cool controller  '
          SALT(4)= 'Fixed heat injection and extraction    '
          SALT(5)= 'PID control action for heating/cooling '
          SALT(6)= ' Flux connection between zone & plant  '
          SALT(7)= 'Multi-stage control with hysteresis    '
          SALT(8)= 'CAV variable supply T with constraints '
          SALT(9)= ' Heat pipe from `outside` to inside    '
          SALT(10)='Separate flux ON & OFF set points      '
          SALT(11)=' Match sensed/recorded value (ideal)   '
          SALT(12)=' Match sensed/recorded value (on/off)  '
          SALT(13)='Time-proportioning separate (on/off)   '
          SALT(14)='Floating `three-position` control      '
          SALT(15)='Optimum start (with rewind) control    '
          SALT(16)=' Optimum stop control                  '
          SALT(17)=' Fuzzy Logic PI-PD control             '
          SALT(18)=' Null controller.                      '
          SALT(19)=' Multi-sensor heating/cooling          '
          SALT(20)=' Evaporative source (surface)          '
          SALT(21)='Slave Capacity Controller              '
          SALT(22)='VAV cooling with CAV reheat (BETA)     '
          SALT(23)='Match sensed values (ideal:2 setpoints)'
          SALT(24)='Adaptive human comfort model           '
          if(iclawcurrent.ne.0)then
            write(outs,'(2a)') ' Current control law: ',
     &        SALT(iclawcurrent)
            call edisp(iuout,outs)
            IX=1
          else
            IX=iclawcurrent
          endif

C Re-instanciate help specifically for summary of control laws.
          H(1)='The zone basic controller is an ideal controller which'
          H(2)='will inject or extract sufficient heat (subject to '
          H(3)='the defined heating and cooling capacity) to meet a '
          H(4)='heating and/or cooling setpoint.  '
          H(5)=' '
          h(6)='A free float controller allows the zone temperature to'
          h(7)='reach whatever temperature is consistent with the '
          h(8)='heat gains and losses in the zone. No environmental '
          h(9)='control is applied. '
          H(10)=' '
          H(11)='The ideal fixed injection controller represents '
          H(12)='systems which operate at a fixed rate if heating '
          H(13)='or cooling is requested. '
          H(14)=' '
          H(15)='An ideal proportional controller with optional'
          H(16)='integral (reset) action and/or derivative (rate)'
          H(17)='action time or to represent full PID. '
          H(18)=' '
          H(19)='A control which links a zone with a detailed system'
          H(20)='network component.'
          H(21)=' '
          H(22)='A multi-stage controller with hysteresis has a '
          H(23)='base heating and cooling capacity and, for each '
          H(24)='three stages of increating capacity.'
          H(25)=' '
          H(26)='Variable supply temperature with limit constraints '
          H(27)='is an idealised air based control that delivers air'
          H(28)='at a fixed flow rate (same for heating & cooling '
          H(29)='operation). The heating supply temperature varies '
          H(30)='up to a maximum setpoint and the cooling supply '
          H(31)='temperature varies down to a minimum setpoint. '
          H(32)=' '
          H(33)='Control with separate on/off setpoints allows '
          H(34)='separate flux ON and flux OFF set points for '
          H(35)='heating and cooling via ON until OFF point logic.'
          H(36)=' '
          H(37)='Time proportioning control with separate on/off '
          H(38)='setpoints for heating and cooling extends the'
          H(39)='functionality with cycle time limits.'
          H(40)=' '
          H(41)='The floating action (three-position) controller'
          H(42)='uses an open switch and shut switch differential (C)'
          H(43)='for heating and cooling as well as timestep rate of'
          H(44)='change for heating and cooling.'
          H(45)=' '
          H(46)='The optimum start controller allows for three modes'
          H(47)='of operation (each with rewind): '
          H(48)=' (1) Default start at 4AM '
          H(49)=' (2) Start at user defined time '
          H(50)=' (3) Birtles & John equation time estimate'
          H(51)=' '
          H(52)='Proportional capacity slave controller. Typically a'
          H(53)='slave control is used to represent an environmental'
          H(54)='control with a thermostat in one zone and actions in'
          H(55)='a number of zones. '
          H(56)=' '
          H(57)='Match sensed values (ideal:2 setpoints) control law:'
          H(58)='is a combination of ideal controller and ctrl law 11.'
          H(59)='Values from the temporal file are taken to set setpnts'
          H(60)='Further details within each control law interface. '
  63      CALL EPICKS(IX,IVALS,' ',' Control law option:',
     &      42,24,SALT,'control law (indented=old or obscure)',IER,60)
          if(ix.eq.0.or.IVALS(1).eq.0)goto 63
          if(iclawcurrent.ne.IVALS(1))then

C Control law has changed so provide some default values for the
C new control law prior to re-forming the menu display.
C Note: for those control laws which are not supported with
C the new menu structure return to the calling subroutine.
            modval=.true.
            iclaw=IVALS(1)
            write(TMP,'(a)') SALT(iclaw)  ! update string for menu
            if(iclaw.eq.1)then
              cmlocal(1)=7.0        ! this swap is supported, major menu change.
              cmlocal(2)=1000.0     ! reset to default values
              cmlocal(3)=0.0
              cmlocal(4)=1000.0
              cmlocal(5)=0.0
              cmlocal(6)=20.0
              cmlocal(7)=24.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90
            elseif(iclaw.eq.2)then
              cmlocal(1)=0.0        ! this swap is supported, major menu change.
              goto 90
            elseif(iclaw.eq.4)then
              cmlocal(1)=4.0        ! this swap is supported, major menu change.
              cmlocal(2)=1000.0     ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=20.0
              cmlocal(5)=24.0
              cmlocal(6)=0.0
              cmlocal(7)=0.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90
            elseif(iclaw.eq.5)then
              cmlocal(1)=10.0        ! varies (this is the smallest) no yet supported.
              cmlocal(2)=1.0        ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=0.0
              cmlocal(5)=20.0
              cmlocal(6)=2.0
              cmlocal(7)=1000.0
              cmlocal(8)=0.0
              cmlocal(9)=24.0
              cmlocal(10)=2.0
              cmlocal(11)=600.0
              cmlocal(12)=600.0
              goto 90
            elseif(iclaw.eq.6)then
              cmlocal(1)=5.0        ! varies (this is the smallest)
              ier=6
              return
            elseif(iclaw.eq.7)then
              cmlocal(1)=12.0       ! this swap is supported, major menu change.
              cmlocal(2)=0.0        ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=2000.0
              cmlocal(5)=3000.0
              cmlocal(6)=0.0
              cmlocal(7)=1000.0
              cmlocal(8)=2000.0
              cmlocal(9)=3000.0
              cmlocal(10)=20.0
              cmlocal(11)=2.0
              cmlocal(12)=24.0
              cmlocal(13)=2.0
              goto 90
            elseif(iclaw.eq.8)then
              cmlocal(1)=6.0
              cmlocal(2)=30.0        ! reset to default values
              cmlocal(3)=10.0
              cmlocal(4)=1.0
              cmlocal(5)=18.0
              cmlocal(6)=24.0
              cmlocal(7)=0.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90
            elseif(iclaw.eq.9)then
              cmlocal(1)=9.0
              ier=9
              return
            elseif(iclaw.eq.10)then
              cmlocal(1)=6.0
              cmlocal(2)=1000.0        ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=18.0
              cmlocal(5)=22.0
              cmlocal(6)=26.0
              cmlocal(7)=24.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90
            elseif(iclaw.eq.11)then
              cmlocal(1)=10.0        ! varies (this is the smallest)
              ier=11
              return
            elseif(iclaw.eq.12)then
              cmlocal(1)=10.0        ! varies (this is the smallest)
              ier=12
              return
            elseif(iclaw.eq.13)then
              cmlocal(1)=12.0
              cmlocal(2)=1000.0        ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=18.0
              cmlocal(5)=22.0
              cmlocal(6)=26.0
              cmlocal(7)=24.0
              cmlocal(8)=10.0
              cmlocal(9)=5.0
              cmlocal(10)=5.0
              cmlocal(11)=10.0
              cmlocal(12)=5.0
              cmlocal(13)=5.0
              goto 90
            elseif(iclaw.eq.14)then
              cmlocal(1)=14.0
              cmlocal(2)=20.0        ! reset to default values
              cmlocal(3)=2.0
              cmlocal(4)=1.0
              cmlocal(5)=2.0
              cmlocal(6)=24.0
              cmlocal(7)=2.0
              cmlocal(8)=1.0
              cmlocal(9)=1.0
              cmlocal(10)=1000.0
              cmlocal(11)=0.0
              cmlocal(12)=1.0
              cmlocal(13)=1000.0
              cmlocal(14)=0.0
              cmlocal(15)=1.0
              goto 90
            elseif(iclaw.eq.15)then
              cmlocal(1)=6.0        ! varies (this is the smallest) no yet supported.
              cmlocal(2)=1000.0      ! reset to default values
              cmlocal(3)=20.0
              cmlocal(4)=1.0
              cmlocal(5)=8.0
              cmlocal(6)=0.5
              cmlocal(7)=1.0
              cmlocal(8)=4.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              goto 90
            elseif(iclaw.eq.16)then
              cmlocal(1)=9.0
              ier=16
              return
            elseif(iclaw.eq.17)then
              cmlocal(1)=7.0
              ier=17
              return
            elseif(iclaw.eq.18)then
              cmlocal(1)=0.0
              ier=18
              return
            elseif(iclaw.eq.19)then
              cmlocal(1)=6.0
              ier=19
              return
            elseif(iclaw.eq.20)then
              cmlocal(1)=1.0       ! varies (this is the smallest)
              ier=20
              return
            elseif(iclaw.eq.21)then
              cmlocal(1)=3.0
              cmlocal(2)=1.0        ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=1000.0
              cmlocal(5)=0.0
              cmlocal(6)=0.0
              cmlocal(7)=0.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90
            elseif(iclaw.eq.22)then
              cmlocal(1)=6.0
              cmlocal(2)=1000.0        ! reset to default values
              cmlocal(3)=12.0
              cmlocal(4)=20.0
              cmlocal(5)=1.0
              cmlocal(6)=0.20
              cmlocal(7)=0.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90

C new controller for tdf 
C At the moment it does what the control law 11 does
            elseif(iclaw.eq.23)then
              cmlocal(1)=2.0    
              cmlocal(2)=5.0    
              cmlocal(3)=6.0
              ier=23
              goto 90
            elseif(iclaw.eq.24)then
              cmlocal(1)=8.0     
              cmlocal(2)=1000.0  
              cmlocal(3)=0.0
              cmlocal(4)=1000.0
              cmlocal(5)=0.0
              cmlocal(6)=20.0
              cmlocal(7)=24.0
              cmlocal(8)=0.8
              cmlocal(9)=2.0
              goto 90
            else

            endif
            goto 90
          else

C The user did not alter the control law.
            goto 91
          endif
          goto 92
        elseif(ictyp.eq.1)then

        elseif(ictyp.eq.2)then

        endif
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit data item identified by KEYIND
        CALL KEYIND(MVERT,IVERT,IFOC,IO)

C prompt1 (72 char) first easkr prompt
C prompt2 (72 char) second easkr prompt
C ndec (int) number of decimal places to display
C vmin (real) minimum value acceptable
C vmax (real) maximum value acceptable
C vdef (real) initial/default value
        if(ndec(ifoc).eq.-1)then

C If ndec was a negative one then do not edit this number.
          call edisp(iuout,'Nothing to edit...')
          goto 92
        elseif(ndec(ifoc).eq.0)then

C If ndec was a zero then edit this number as an integer.
          ivalue=nint(cmlocal(ifoc))
          imini=nint(vmin(ifoc))
          imaxi=nint(vmax(ifoc))
          idefa=nint(vdef(ifoc))
          CALL EASKI(ivalue,prompt1(ifoc),prompt2(ifoc),
     &      imini,'F',imaxi,'F',idefa,menu(ifoc),IER,nctlhelp)
          if(ier.eq.-3) goto 92
          cmlocal(ifoc)=real(ivalue)
        else

C If ndec was greater than zero then edit this number as a real.
          CALL EASKR(cmlocal(ifoc),prompt1(ifoc),prompt2(ifoc),
     &      vmin(ifoc),'F',vmax(ifoc),'F',vdef(ifoc),menu(ifoc),
     &      IER,nctlhelp)
          if(ier.eq.-3) goto 92
        endif

C If there were no errors and the value has changed then update the
C menu (in case a different set of items is required). If cancel
C requested simply redisplay the menu.
        call eclose(cmlocal(ifoc),cm(ifoc),0.001,close)
        if(close)then
          goto 92
        endif


C The current value changed so do further processing depending on
C the specific logic of each control law. Typically warning messages
C are given if invalid items are found.
        if(ictyp.eq.0)then
          if(iclaw.eq.1.or.iclaw.eq.3)then
            modval=.true.
            if(ifoc.eq.12)then

C If the user has altered the humidity control then revise the cmlocal() data.
C Switch to zero turns off RH control. Change to 2.0 clears two data slots
C previously used for g/s.
              if(int(cmlocal(ifoc)).eq.0)then
                cmlocal(1)=7.0
                cmlocal(8)=0.0
                cmlocal(9)=0.0
                cmlocal(10)=0.0
                cmlocal(11)=0.0
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.1)then
                cmlocal(1)=11.0
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.2)then
                cmlocal(1)=11.0
                cmlocal(10)=0.0
                cmlocal(11)=0.0
                goto 90    ! major change so menu needs to change
              endif
            elseif(ifoc.eq.4.or.ifoc.eq.5)then

C User altered the cooling capacity so check to see if cooling setpoint needs resetting.
              modval=.true.
              CALL ECLOSE(cmlocal(4),0.0,0.001,close1)
              CALL ECLOSE(cmlocal(5),0.0,0.001,close2)
              if(close1.and.close2)then
                call edisp(iuout,' ')
                call edisp(iuout,
     &    ' Zero cooling capacity - setting cooling setpoint to 100C.')
                cmlocal(7)=100.0
                goto 91   ! minor change
              endif
            else
              modval=.true.
              goto 91   ! minor change
            endif
          elseif(iclaw.eq.2)then
            continue    ! no post processing required.
          elseif(iclaw.eq.4)then
            continue    ! no post processing required.
            modval=.true.
          elseif(iclaw.eq.5)then
            if(ifoc.eq.2)then

C If the user has switched between P PI PD PID modes adjust.
              if(int(cmlocal(ifoc)).eq.1)then
                cmlocal(1)=9.0
                cmlocal(11)=0.0
                cmlocal(12)=0.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.2)then
                cmlocal(1)=10.0
                cmlocal(11)=600.0
                cmlocal(12)=0.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.3)then
                cmlocal(1)=10.0
                cmlocal(11)=600.0
                cmlocal(12)=0.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.4)then
                cmlocal(1)=11.0
                cmlocal(11)=600.0
                cmlocal(12)=600.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              endif
            elseif(ifoc.eq.7.or.ifoc.eq.9)then

C User altered the cooling capacity so check to see if cooling setpoint needs resetting.
              modval=.true.
              CALL ECLOSE(cmlocal(7),0.0,0.001,close1)
              CALL ECLOSE(cmlocal(9),0.0,0.001,close2)
              if(close1.and.close2)then
                call edisp(iuout,' ')
                call edisp(iuout,
     &    ' Zero cooling capacity - setting cooling setpoint to 100C.')
                cmlocal(9)=100.0
                goto 91   ! minor change
              endif
            else
              modval=.true.
              goto 91   ! minor change
            endif
          elseif(iclaw.eq.7)then

C Warn user if stage capacities do not increment.
            cmlocal(1)=12.0
            if(cmlocal(5).lt.cmlocal(4))then
              call edisp(iuout,'Heating stage 3 less than stage 2!')
            endif
            if(cmlocal(4).lt.cmlocal(3))then
              call edisp(iuout,'Heating stage 2 less than stage 1!')
            endif
            if(cmlocal(3).lt.cmlocal(2))then
              call edisp(iuout,'Heating stage 1 less than base!')
            endif
            if(cmlocal(9).lt.cmlocal(8))then
              call edisp(iuout,'Cooling stage 3 less than stage 2!')
            endif
            if(cmlocal(8).lt.cmlocal(7))then
              call edisp(iuout,'Cooling stage 2 less than stage 1!')
            endif
            if(cmlocal(7).lt.cmlocal(6))then
              call edisp(iuout,'Cooling stage 1 less than base!')
            endif
 
C If no cooling capacity then set cooling set point to 100C.
            CALL ECLOSE(cmlocal(7),0.0,0.001,close1)
            CALL ECLOSE(cmlocal(8),0.0,0.001,close2)
            CALL ECLOSE(cmlocal(9),0.0,0.001,close3)
            if(close1.and.close2.and.close3)then
              call edisp(iuout,' ')
              call edisp(iuout,
     &    ' Zero cooling capacity - setting cooling setpoint to 100C.')
              cmlocal(12)=100.
              cmlocal(13)=1.0
              modval=.true.
              goto 91   ! minor change
            endif
          elseif(iclaw.eq.8)then
            continue    ! no post processing required.
          elseif(iclaw.eq.10)then

C Ensure that heating setpoint on if equal to or above heating setpoint off.
            if(cmlocal(4).ge.cmlocal(5)) call usrmsg(
     &          'The heating ON set-point must be lower than',
     &          'the heating OFF set-point.','W')
            if(cmlocal(6).le.cmlocal(7)) call usrmsg(
     &          'The cooling OFF set-point must be lower than',
     &          'the cooling ON set-point.','W')
          elseif(iclaw.eq.13)then

C Ensure that heating setpoint on if equal to or above heating setpoint off.
            if(cmlocal(4).ge.cmlocal(5)) call usrmsg(
     &        'The heating ON set-point must be lower than',
     &        'the heating OFF set-point.','W')
            if(cmlocal(6).le.cmlocal(7)) call usrmsg(
     &        'The cooling OFF set-point must be lower than',
     &        'the cooling ON set-point.','W')

C Ensure that ON + OFF cycle time is not greater than total cycle time.
            if(cmlocal(8).lt.(cmlocal(9)+cmlocal(10))) call usrmsg(
     &        'The heating ON and OFF cycle minutes are greater than',
     &        'the total heating cycle minutes.','W') 
            if(cmlocal(11).lt.(cmlocal(12)+cmlocal(13))) call usrmsg(
     &        'The cooling ON and OFF cycle minutes are greater than',
     &        'the total cooling cycle minutes.','W') 
          elseif(iclaw.eq.14)then
             continue    ! no postprocessing (yet)
          elseif(iclaw.eq.15)then
            if(ifoc.eq.7)then

C If the user has switched between start-up modes adjust.
              if(int(cmlocal(ifoc)).eq.1)then
                cmlocal(1)=6.0
                cmlocal(8)=4.0
                cmlocal(9)=0.0
                cmlocal(10)=0.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.2)then
                cmlocal(1)=7.0
                cmlocal(8)=4.0
                cmlocal(9)=0.0
                cmlocal(10)=0.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.3)then
                cmlocal(1)=9.0
                cmlocal(8)=1.0
                cmlocal(9)=1.0
                cmlocal(10)=1.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              endif
            endif
          elseif(iclaw.eq.21)then

C For master-slave ensure that sensor of this control is non-zero and that
C it points to the same zone as is used by the master controller.
            imasctlindex= int(cmlocal(2))
            if(imasctlindex.eq.0)then
              call usrmsg('You must select a master control loop!',
     &          ' ','W')
            elseif(imasctlindex.gt.ncf)then
              call usrmsg('You must select a existing master',
     &          'control loop!','W')
            endif
            if(ibsn(icloop,1).gt.0.and.imasctlindex.gt.0)then
              if(ibsn(icloop,1).eq.ibsn(imasctlindex,1))then
                continue
              else
                call usrmsg(
     &           'Slave sensor must sense the same zone as the',
     &           'master control loop!','W')
              endif
            else
              call usrmsg(
     &          'Slave sensor must sense the same zone as the',
     &          'master control loop!','W')
            endif
            if(iban(icloop,1).gt.0)then
              continue
            else
              call usrmsg(
     &          'Slave actuator must be in the slave zone!',
     &          ' ','W')
            endif
          elseif(iclaw.eq.22)then
            continue    ! no postprocessing (yet)
          elseif(iclaw.eq.23)then
            modval=.true.
          elseif(iclaw.eq.24)then
            modval=.true.
          else

C << other control laws where processing is required... >>
          endif
        elseif(ictyp.eq.1)then

        elseif(ictyp.eq.2)then

        endif
      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END 

C ****** stfmenu
C Stfmenu copies menu and prompts for period editing.  It expects the common
C block sctl to have been filled for the current period.
      subroutine stfmenu(icfoc,ier)
#include "building.h"
#include "plant.h"
#include "net_flow.h"
#include "control.h"

      common/pophelp/h(60)
      common/sctl/tcps,ictyp,iclaw,cm(misc)

C menu (32 char) menu text for each item
C prompt1 (72 char) first easkr prompt
C prompt2 (72 char) second easkr prompt
C ndec (int) number of decimal places to display (-1 indicates no display)
C vmin (real) minimum value acceptable
C vmax (real) maximum value acceptable
C vdef (real) initial/default value
C nctlhelp (int) number of help lines
C nmenulines (int) usually same as int(cm(1)) with exceptions
C   for control law one.
      common/sctlmenu/menu(misc),prompt1(misc),prompt2(misc)
      common/sctldata/ndec(misc),vmin(misc),vmax(misc),
     &  vdef(misc),nctlhelp,nmenulines

      character menu*32,prompt1*72,prompt2*72,h*72

      if(icfoc.eq.0)then

C Set first item of menu (this item holds number of supplementary data
C items for the loop and should not be edited
          menu(1)='Choose parameter to edit:'
        if(iclaw.eq.1.or.iclaw.eq.3)then

C Set up the help text for the control period menu as well as the
C individual items to be edited (in one big block).
          H(1)='A basic zone controller is an ideal controller that'
          H(2)='will inject or extract heat (subject to the available'
          H(3)='heating and cooling capacity) to maintain a given'
          H(4)='set-point temperature.'
          H(5)=' '
          H(6)='If there is a deadband then the temperature will float'
          H(7)='when the sensed condition is between lower and upper'
          H(8)='set-points.'
          H(9)=' '
          H(10)='If the minimium heating (or cooling) capacity is > 0'
          H(11)='then heating (or cooling) will persist even when'
          H(12)=' none is called for.'
          H(13)=' '
          H(14)='A heating only (or cooling only) scheme may be'
          H(15)='defined by setting a high cooling (or low heating)'
          H(16)='temperature set-point or by defining a zero cooling'
          H(17)='or heating) capacity.'
          H(18)=' '
          H(19)='In addition to temperature control, it is possible'
          H(20)='to activate relative humidity (RH) control by:'
          H(21)=' '  
          H(22)='(1) Injection or extraction of moisture (g/s) to the'
          H(23)='    zone. Humidification and dehumidification'
          H(24)='    capacities are specified in g/s where 1 g/s of'
          H(25)='    moisture equates to a latent gain/loss of'
          H(26)='    approximately +/-2.5kW.'
          H(27)=' '
          H(28)='(2) Heating & cooling the zone. Note however that'
          H(29)='    this approach will over-ride any temperature' 
          H(30)='    control although the defined heating and cooling'
          H(31)='    capacities will be used. This approach could be'
          H(32)='    used to check whether a system which is not'
          H(33)='    capable of moisture injection/extraction can'
          H(34)='    still limit RH swings.'  
          H(35)=' '
          H(36)='Depending on the control method selected, heating or'
          H(37)='dehumidification will commence if the RH rises above'
          H(38)='the upper set-point. Likewise, humidification or'
          H(39)='cooling commences if the RH falls below the lower RH'
          H(40)='set-point.'
          H(41)=' '
          H(42)='With large thermal capacity system the simulation'
          H(43)='time step should be constrained to a maximum of'
          H(44)='15 minutes to give sufficient control regulation'
          H(45)='decision points'
          nctlhelp=45
          nmenulines=12    ! set to the last cm index used.

C Basic control or pre-heat/cool (first array items are never
C used to that the index matches the cm position.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Maximum heating capacity (W)'
          prompt1(2)='Maximum heating capacity (W) which is available'
          prompt2(2)='to meet the demand. '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=1000.0
          menu(3)='Minimum heating capacity (W)'
          prompt1(3)='Minimum heating capacity (W) The flux injected'
          prompt2(3)='will never drop below this level. '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=0.0
          menu(4)='Maximum cooling capacity (W)'
          prompt1(4)='Maximum cooling capacity (W) which is available'
          prompt2(4)='to meet the demand. '
          ndec(4)=1; vmin(4)=0.0; vmax(4)=999999.0; vdef(4)=1000.0
          menu(5)='Minimum cooling capacity (W)'
          prompt1(5)='Minimum cooling capacity (W) The flux extracted'
          prompt2(5)='will never be less than this. '
          ndec(5)=1; vmin(5)=0.0; vmax(5)=999999.0; vdef(5)=0.0
          menu(6)='Heating setpoint (C)'
          prompt1(6)='Heating setpoint (C) (temperature to meet if'
          prompt2(6)='there is sufficient capacity) '
          ndec(6)=3; vmin(6)=-100.0; vmax(6)=100.0; vdef(6)=20.0
          menu(7)='Cooling setpoint (C)'
          prompt1(7)='Cooling setpoint (C (temperature to meet if)'
          prompt2(7)='there is sufficient capacity) '
          ndec(7)=3; vmin(7)=-100.0; vmax(7)=100.0; vdef(7)=24.0

C Depending on the current number of miscel items also do humidity.
          if(int(cm(1)).eq.7.or.int(cm(1)).eq.6)then
            menu(8)=' '
            prompt1(8)=' '
            prompt2(8)=' '
            ndec(8)=-1; vmin(8)=0.0; vmax(8)=100.0; vdef(8)=0.0
            menu(9)=' '
            prompt1(9)=' '
            prompt2(9)=' '
            ndec(9)=-1; vmin(9)=0.0; vmax(9)=100.0; vdef(9)=0.0
            menu(10)=' '
            prompt1(10)=' '
            prompt2(10)=' '
            ndec(10)=-1; vmin(10)=0.0; vmax(10)=100.0; vdef(10)=0.0
            menu(11)=' '
            prompt1(11)=' '
            prompt2(11)=' '
            ndec(11)=-1; vmin(11)=0.0; vmax(11)=100.0; vdef(11)=0.0
            menu(12)='RH control >> OFF '
            prompt1(12)=
     & 'RH control not enabled (value is 0.0) change to 1.0 for'
            prompt2(12)=
     & 'moisture injection or 2.0 for temperature control(see help).'
            ndec(12)=1; vmin(12)=0.0; vmax(12)=2.0; vdef(12)=0.0
            continue
          elseif(int(cm(1)).eq.11)then
            menu(8)='Upper humidity set point (%)'
            prompt1(8)='Upper humidity set point (%)'
            prompt2(8)=' '
            ndec(8)=3; vmin(8)=0.0; vmax(8)=100.0; vdef(8)=40.0
            menu(9)='Lower humidity set point (%)'
            prompt1(9)='Lower humidity set point (%)'
            prompt2(9)=' '
            ndec(9)=3; vmin(9)=0.0; vmax(9)=100.0; vdef(9)=40.0
            if(int(cm(12)).eq.1)then
              menu(10)='Max Humidification rate (g/s)'
              prompt1(10)='Max Humidification rate (g/s)'
              prompt2(10)=' '
              ndec(10)=3; vmin(10)=0.0; vmax(10)=1000.0; vdef(10)=2.0
              menu(11)='Max Dehumidification rate (g/s)'
              prompt1(11)='Max Dehumidification rate (g/s)'
              prompt2(11)=' '
              ndec(11)=3; vmin(11)=0.0; vmax(11)=1000.0; vdef(11)=2.0
              menu(12)='RH control >> via moisture inj'
              prompt1(12)=
     & 'Moisture injection enabled (value is 1.0) change to 2.0'
              prompt2(12)=
     & 'for temperature control or 0.0 for no RH control(see help).'
              ndec(12)=1; vmin(12)=0.0; vmax(12)=2.0; vdef(12)=0.0
            elseif(int(cm(12)).eq.2)then
              menu(10)=' '
              prompt1(10)=' '
              prompt2(10)=' '
              ndec(10)=-1; vmin(10)=0.0; vmax(10)=0.0; vdef(10)=0.0
              menu(11)='  '
              prompt1(11)=' '
              prompt2(11)=' '
              ndec(11)=-1; vmin(11)=0.0; vmax(11)=0.0; vdef(11)=0.0
              menu(12)='RH control >> via temp control'
              prompt1(12)=
     & 'Temperature control enabled (value is 2.0) change to 1.0'
              prompt2(12)=
     & 'for moisture injection or 0.0 for no RH control(see help).'
              ndec(12)=1; vmin(12)=0.0; vmax(12)=2.0; vdef(12)=0.0
            endif
          endif
        elseif(iclaw.eq.2)then
          h(1)='A free float controller allows the zone temperature'
          h(2)='to reach whatever value is consistent with the heat'
          h(3)='gains and losses to the zone. No heating or cooling'
          h(4)='is applied. '
          nctlhelp=5
          nmenulines=1    ! set to the last cm index used.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Free floating control (no data)'
          prompt1(2)=' '
          prompt2(2)=' '
          ndec(2)=-1; vmin(2)=0.0; vmax(2)=0.0; vdef(2)=0.0
        elseif(iclaw.eq.4)then
          H(1)='The ideal fixed injection controller injects a given'
          H(2)='flux when heating or cooling is requested.'
          H(3)=' '
          H(4)='This control is best used with a short simulation time'
          H(5)='step to avoid temperature oscillation.'
          H(6)=' '
          H(7)='Heating (or cooling) may be disabled by setting a'
          H(8)='correspondingly low (or high) set point or by setting'
          H(9)='the corresponding capacity to zero.'
          nctlhelp=9
          nmenulines=5    ! set to the last cm index used.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Heat injection (fixed W) '
          prompt1(2)='Heat injection (fixed W) if temperature drops'
          prompt2(2)='below the heating setpoint. '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=9999999.0; vdef(2)=1000.0
          menu(3)='Cooling extract (fixed W) '
          prompt1(3)='Cooling extract (fixed W) if temperature goes '
          prompt2(3)='over the cooling setpoint. '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=9999999.0; vdef(3)=1000.0
          menu(4)='Heating setpoint (C) '
          prompt1(4)='Heating setpoint (C) below which heating will '
          prompt2(4)='be called for '
          ndec(4)=3; vmin(4)=0.0; vmax(4)=100.0; vdef(4)=20.0
          menu(5)='Cooling setopoint (C) '
          prompt1(5)='Cooling setopoint (C) above which cooling will '
          prompt2(5)='be called for '
          ndec(5)=3; vmin(5)=0.0; vmax(5)=100.0; vdef(5)=24.0
        elseif(iclaw.eq.5)then
          h(1)='This implements an ideal proportional (P) controller'
          h(2)='with optional integral (I) and/or derivative (D)'
          h(3)='action.'
          h(4)=' '
          h(5)='P action produces an output that is proportional to '
          h(6)='the sensed deviation from the set point. This can.'
          h(7)='result in an offset.'
          h(8)=' '
          h(9)='P+I action may be used to elimitate this offset.'
          h(10)=' '
          h(11)='P+D action is typically used where the heating or'
          h(12)='cooling system has a fast response. This helps to'
          h(13)='avoid overshoot.'
          h(14)=' '
          h(15)='PID action combines all the above. As with real PID'
          h(16)='controllers, this controllerneeds to be tuned to the'
          h(17)='zone:'
          h(18)='  * overshoots can be caused by excessive integral'
          h(19)='    action;'
          h(20)='  * continuous oscillations can be caused by'
          h(21)='    excessive derivitive action.'
          h(22)=' '
          h(23)='These controls are best used in association with'
          h(24)='short simulation time steps.'
          h(25)=' '
          H(26)='If the minimium heating (or cooling) capacity is > 0'
          H(27)='then heating (or cooling) will persist even when'
          H(28)='none is called for.'
          H(29)=' '
          H(30)='Heating (or cooling) may be disabled by setting a'
          H(31)='correspondingly low (or high) set point or by setting'
          H(32)='the corresponding capacity to zero.'
          nctlhelp=32
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=100.0; vdef(1)=0.0
          if(nint(cm(2)).eq.1)then
            menu(2)='Mode >> Proportioanl'
            nmenulines=10    ! set to the last cm index used
          elseif(nint(cm(2)).eq.2)then
            menu(2)='Mode >> Proportioanl +integral'
            nmenulines=11    ! set to the last cm index used
          elseif(nint(cm(2)).eq.3)then
            menu(2)='Mode >> Proportioanl +derivitive'
            nmenulines=11    ! set to the last cm index used
          elseif(nint(cm(2)).eq.4)then
            menu(2)='Mode >>Proportioanl P+I+D'
            nmenulines=12    ! set to the last cm index used
          endif
          prompt1(2)='Proportioanl is one, for P+I use two,'
          prompt2(2)='for P+D use three, for P+I+D use four.'
          ndec(2)=0; vmin(2)=0.0; vmax(2)=4.0; vdef(2)=0.0
          menu(3)='Maximum heating capacity (W) '
          prompt1(3)='Maximum heating capacity (W) '
          prompt2(3)=' '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=1000.0
          menu(4)='Minimum heating capacity (W) '
          prompt1(4)='Minimum heating capacity (W) (will not go below'
          prompt2(4)='this amount e.g. for uninsulated pipes)'
          ndec(4)=1; vmin(4)=0.0; vmax(4)=999999.0; vdef(4)=0.0
          menu(5)='Heating setpoint (C)'
          prompt1(5)='Heating setpoint (C) '
          prompt2(5)='(in the middle of the proportional band) '
          ndec(5)=3; vmin(5)=0.0; vmax(5)=100.0; vdef(5)=20.0
          menu(6)='Heating throttling range (C) '
          prompt1(6)='Heating throttling range (proportional band) (C)'
          prompt2(6)=' '
          ndec(6)=3; vmin(6)=0.0; vmax(6)=20.0; vdef(6)=2.0
          menu(7)='Maximum cooling capacity (W)'
          prompt1(7)='Maximum cooling capacity (W) '
          prompt2(7)=' '
          ndec(7)=1; vmin(7)=0.0; vmax(7)=999999.0; vdef(7)=1000.0
          menu(8)='Minimum cooling capacity (W)'
          prompt1(8)='Minimum cooling capacity (W) (will not drop below'
          prompt2(8)='this amount) '
          ndec(8)=1; vmin(8)=0.0; vmax(8)=999999.0; vdef(8)=0.0
          menu(9)='Cooling setpoint (C) '
          prompt1(9)='Cooling setpoint (C) '
          prompt2(9)='(in the middle of the proportional band) '
          ndec(9)=3; vmin(9)=0.0; vmax(9)=100.0; vdef(9)=24.0
          menu(10)='Cooling throttling range (C) '
          prompt1(10)='Cooling throttling range (proportional band) (C)'
          prompt2(10)=' '
          ndec(10)=3; vmin(10)=0.0; vmax(10)=10.0; vdef(10)=2.0
          if(nint(cm(2)).eq.1)then
            menu(11)=' '
            prompt1(11)=' '
            prompt2(11)=' '
            ndec(11)=-1; vmin(11)=0.0; vmax(11)=0.0; vdef(11)=0.0
            menu(12)=' '
            prompt1(12)=' '
            prompt2(12)=' '
            ndec(12)=-1; vmin(12)=0.0; vmax(12)=0.0; vdef(12)=0.0
          elseif(nint(cm(2)).eq.2)then
            menu(11)='Integral action time [secs] '
            prompt1(11)='Integral action time [secs] '
            prompt2(11)=' '
            ndec(11)=1; vmin(11)=0.0; vmax(11)=10000.0; vdef(11)=600.0
            menu(12)=' '
            prompt1(12)=' '
            prompt2(12)=' '
            ndec(12)=-1; vmin(12)=1.0; vmax(12)=0.0; vdef(12)=0.0
          elseif(nint(cm(2)).eq.3)then
            menu(11)='Derivative action time [secs] '
            prompt1(11)='Derivative action time [secs] '
            prompt2(11)='(see help) '
            ndec(11)=1; vmin(11)=1.0; vmax(11)=10000.0; vdef(11)=600.0
            menu(12)=' '
            prompt1(12)=' '
            prompt2(12)=' '
            ndec(12)=-1; vmin(12)=0.0; vmax(12)=0.0; vdef(12)=0.0
          elseif(nint(cm(2)).eq.4)then
            menu(11)='Integral action time [secs] '
            prompt1(11)='Integral action time [secs] '
            prompt2(11)='(see help) '
            ndec(11)=1; vmin(11)=1.0; vmax(11)=10000.0; vdef(11)=600.0
            menu(12)='Derivative action time [secs] '
            prompt1(12)='Derivative action time [secs] '
            prompt2(12)='(see help) '
            ndec(12)=1; vmin(12)=1.0; vmax(12)=10000.0; vdef(12)=600.0
          endif
        elseif(iclaw.eq.6)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.7)then

C A multi-stage controller with hysteresis.
          h(1)='A multi-stage controller (with hysteresis) has'
          h(2)='increasing (or equal) heating and cooling capacities'
          h(3)='defined for several stages. Stage set-points are'
          h(4)='then defined.'
          h(5)=' '
          h(6)='The control logic switches to the least stage that'
          h(7)='will maintain the desired temperature, stepping up'
          h(8)='and down between stages as appropriate.'
          nctlhelp=8
          nmenulines=13    ! set to the last cm index used.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Heat capacity at OFF state (W)'
          prompt1(2)='Heat capacity at OFF state (W) (typically zero'
          prompt2(2)='and a non-zero base load is allowed '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=0.0
          menu(3)='Heat capacity at 1st stage (W) '
          prompt1(3)='Heat capacity at 1st stage (W)'
          prompt2(3)='(must be greater than base load)'
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=1000.0
          menu(4)='Heat capacity at 2nd stage (W)'
          prompt1(4)='Heat capacity at 2nd stage (W)'
          prompt2(4)='(must be greater than 1st stage)'
          ndec(4)=1; vmin(4)=0.0; vmax(4)=999999.0; vdef(4)=2000.0
          menu(5)='Heat capacity at 3rd stage (W)'
          prompt1(5)='Heat capacity at 3rd stage (W)'
          prompt2(5)='(must be greater than 2nd stage)'
          ndec(5)=1; vmin(5)=0.0; vmax(5)=999999.0; vdef(5)=3000.0
          menu(6)='Cool capacity at OFF state (W)'
          prompt1(6)='Cool capacity at OFF state (W) (typically zero'
          prompt2(6)='and a non-zero base load is allowed '
          ndec(6)=1; vmin(6)=0.0; vmax(6)=999999.0; vdef(6)=0.0
          menu(7)='Cool capacity at 1st stage (W)'
          prompt1(7)='Cool capacity at 1st stage (W)'
          prompt2(7)='(must be greater than base load) '
          ndec(7)=1; vmin(7)=0.0; vmax(7)=999999.0; vdef(7)=1000.0
          menu(8)='Cool capacity at 2nd stage (W)'
          prompt1(8)='Cool capacity at 2nd stage (W)'
          prompt2(8)='(must be greater than 1st stage)'
          ndec(8)=1; vmin(8)=0.0; vmax(8)=999999.0; vdef(8)=2000.0
          menu(9)='Cool capacity at 3rd stage (W)'
          prompt1(9)='Cool capacity at 3rd stage (W)'
          prompt2(9)='(must be greater than 2nd stage) '
          ndec(9)=1; vmin(9)=0.0; vmax(9)=999999.0; vdef(9)=3000.0
          menu(10)='Heating setpoint (C)'
          prompt1(10)='Heating setpoint (C)'
          prompt2(10)=' '
          ndec(10)=3; vmin(10)=0.0; vmax(10)=100.0; vdef(10)=20.0
          menu(11)='Heating deadband (C)'
          prompt1(11)='Heating deadband (C)'
          prompt2(11)='(zero or greater)'
          ndec(11)=3; vmin(11)=0.0; vmax(11)=10.0; vdef(11)=1.0
          menu(12)='Cooling setpoint (C)'
          prompt1(12)='Cooling setpoint (C)'
          prompt2(12)=' '
          ndec(12)=3; vmin(12)=0.0; vmax(12)=100.0; vdef(12)=24.0
          menu(13)='Cooling deadband (C)'
          prompt1(13)='Cooling deadband (C)'
          prompt2(13)='(zero or greater)'
          ndec(13)=3; vmin(13)=0.0; vmax(13)=10.0; vdef(13)=1.0
        elseif(iclaw.eq.8)then

C Variable supply temperature with limit constraints.
          h(1)='This control represents an idealised, air based'
          h(2)='system that delivers air at a fixed flow rate (same'
          h(3)='for heating & cooling). The heating supply temperature'
          h(4)='varies up to a maximum set-point and the cooling'
          h(5)='supply temperature varies down to a minimum set-point.'
          h(6)=' '
          h(7)='The room heating and cooling set-points (typically'
          h(8)='defined so that there is a deadband) are met on the'
          h(9)='basis of the heating and/or cooling implicit at the'
          h(10)='defined flow rate and supply temperature, i.e. the'
          h(11)='capacity and efficiency of the supply is not defined.'
          h(12)=' '
          h(13)='This control can be used to check whether an air'
          h(14)='based system is able to meet demand patterns in a'
          h(15)='room prior to specification of a detailed component'
          h(16)='model.'
          nctlhelp=16
          nmenulines=7    ! set to the last cm index used.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Maximum supply temperature (C)'
          prompt1(2)='Maximum (heating) supply temperature (C) '
          prompt2(2)=' '
          ndec(2)=3; vmin(2)=-10.0; vmax(2)=100.0; vdef(2)=30.0
          menu(3)='Minimum supply temperature (C)'
          prompt1(3)='Minimum (cooling) supply temperature (C)'
          prompt2(3)=' '
          ndec(3)=3; vmin(3)=-10.0; vmax(3)=100.0; vdef(3)=10.0
          menu(4)='Air supply flow rate (m^3/sec) '
          prompt1(4)='Air supply flow rate (m^3/sec) '
          prompt2(4)=' '
          ndec(4)=4; vmin(4)=0.0; vmax(4)=100.0; vdef(4)=1.0
          menu(5)='Heating (room) setpoint (C)'
          prompt1(5)='Heating (room) setpoint (C) '
          prompt2(5)=' '
          ndec(5)=3; vmin(5)=-100.0; vmax(5)=100.0; vdef(5)=18.0
          menu(6)='Cooling (room) setpoint (C)'
          prompt1(6)='Cooling (room) setpoint (C)'
          prompt2(6)=' '
          ndec(6)=3; vmin(6)=-100.0; vmax(6)=100.0; vdef(6)=24.0
          if(int(cm(7)).eq.0)then
            menu(7)='Cooling >> enabled '
            prompt1(7)='Cooling is available in the supply (value is'
            prompt2(7)='a zero) change to a one to turn off cooling)'
            ndec(7)=0; vmin(7)=0.0; vmax(7)=1.0; vdef(7)=0.0
          elseif(int(cm(7)).eq.1)then
            menu(7)='Cooling >> not enabled '
            prompt1(7)='Cooling is not available in the supply (value'
            prompt2(7)='is a one) change to zero to turn on cooling)'
            ndec(7)=0; vmin(7)=0.0; vmax(7)=1.0; vdef(7)=0.0
          endif
        elseif(iclaw.eq.9)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.10)then
          
C Control with separate on/off setpoints for heating and cooling.
          h(1)='This control allows separate flux ON and flux OFF'
          h(2)='set points to be specified for heating and cooling.'
          h(3)=' '
          h(4)='During ON periods the specified flux is applied'
          h(5)='until the OFF setpoint is reached. When the OFF'
          h(6)='condition has been reached the flux input is not'
          h(7)='reactivated until and ON state is agsin reached.'
          h(8)=' '
          h(9)= 'Example: if the temperature goes below 18C turn on'
          h(10)='  heating until 22C is reached and keep off until'
          h(11)='  the temperature falls below 18C again.'
          h(12)=' '
          h(13)='Example: if the temperature goes above 26C turn on'
          h(14)='  cooling until 24C is reached and keep off until'
          h(15)='  the temperature rises to 26C.'
          h(16)=' '
          h(17)='If the heating OFF and the cooling OFF setpoints'
          h(18)='overlap then heating and cooling are both ON. '
          h(19)=' '
          h(20)='Note because the full heating or cooling capacity'
          h(21)='is used when an ON state is called for a shorter'
          h(22)='timestep should be used (typically 5 minutes or'
          h(23)='less). Also the heating and cooling capacity should'
          h(24)='match the demands of the room otherwise poor temp-'
          h(25)='erature control will be obtained. '
          h(26)=' '
          h(27)='This control is an alternative to the basic ideal'
          h(28)='controller which only has a single setpoint for '
          h(29)='heating and cooling. '
          h(30)=' '
          H(31)='You are given the option to discard changes when you'
          H(32)='exit the period. '
          nctlhelp=32
          nmenulines=7    ! set to the last cm index used
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Heating (fixed) capacity (W)'
          prompt1(2)='Heating capacity (W) used for ON state'
          prompt2(2)='(injection is at a fixed rate)'
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=1000.0
          menu(3)='Cooling (fixed) capacity (W)'
          prompt1(3)='Cooling capacity (W) used for ON state'
          prompt2(3)='(extraction is at a fixed rate) '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=1000.0
          menu(4)='Heating ON setpoint (C)'
          prompt1(4)='Heating ON setpoint (C) (turn on heating if'
          prompt2(4)='the temperature falls below this setpoint)'
          ndec(4)=3; vmin(4)=-100.0; vmax(4)=100.0; vdef(4)=18.0
          menu(5)='Heating OFF setpoint (C)'
          prompt1(5)='Heating OFF setpoint (C) (turn off heating if'
          prompt2(5)='the temperature goes above this point) '
          ndec(5)=3; vmin(5)=-100.0; vmax(5)=100.0; vdef(5)=22.0
          menu(6)='Cooling ON setpoint (C) '
          prompt1(6)='Cooling ON setpoint (C) (turn on cooling if'
          prompt2(6)='the temperature goes above this setpoint) '
          ndec(6)=3; vmin(6)=-100.0; vmax(6)=100.0; vdef(6)=26.0
          menu(7)='Cooling OFF setpoint (C) '
          prompt1(7)='Cooling OFF setpoint (C) (turn off cooling if'
          prompt2(7)='the temperature goes below this setpoint) '
          ndec(7)=3; vmin(7)=-100.0; vmax(7)=100.0; vdef(7)=24.0
        elseif(iclaw.eq.11)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.12)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.13)then
          
C Time proportioning control with separate on/off setpoints for heating
C and cooling (initial part of the input is similar to control 10).
          h(1)='This control allows separate flux ON and flux OFF'
          h(2)='set points to be specified for heating and cooling'
          h(3)='and extends the functionality of the basic separate '
          h(4)='ON OFF controller by constraining the heating and '
          h(5)='cooling cycle times. '
          h(6)=' '
          h(7)='During ON periods the specified flux is applied'
          h(8)='until the OFF setpoint is reached. When the OFF'
          h(9)='condition has been reached the flux input is not'
          h(10)='reactivated until and ON state is agsin reached.'
          h(11)=' '
          h(12)= 'Example: if the temperature goes below 18C turn on'
          h(13)='  heating until 22C is reached and keep off until'
          h(14)='  the temperature falls below 18C again.'
          h(15)=' '
          h(16)='Example: if the temperature goes above 26C turn on'
          h(17)='  cooling until 24C is reached and keep off until'
          h(18)='  the temperature rises to 26C.'
          h(19)=' '
          h(20)='Warning: if heating OFF and the cooling OFF setpoints'
          h(21)='overlap there will be simultaneous operation.'
          h(22)=' '
          h(23)='This control represents equipment which is constrained'
          h(24)='by minimum ON and OFF cycle times (minutes) as well as'
          h(25)='a total cycle period. For example a heating battery'
          h(26)='which must be on for at least 5 minutes and then must'
          h(27)='be off for 10 minutes before it can be re-stared and'
          h(28)='the mfg recommends the heating system only go through'
          h(29)='three cycles (20 minutes) each hour. '
          h(30)=' '
          h(31)='The ON and OFF and total cycle times in minutes are'
          h(32)='defined for heating and then for cooling. The number'
          h(33)='of minutes of ON and OFF must not exceed the total'
          h(34)='cycle minutes. '
          h(35)=' '
          h(36)='Note because the full heating or cooling capacity'
          h(37)='is used when an ON state is called for a shorter'
          h(38)='timestep should be used (typically 5 minutes or'
          h(39)='less). Also the heating and cooling capacity should'
          h(40)='match the demands of the room otherwise poor temp-'
          h(41)='erature control will be obtained. '
          h(42)=' '
          h(43)='If the simulation timestep can be longer than the '
          h(44)='cycle times then the heating and cooling capacity is'
          h(45)='reduced to compensate. If the simulation timestep is'
          h(46)='shorter than the ON OFF cycle times then it is easier'
          h(47)='to see the control action. '
          h(48)=' '
          h(49)='IT IS UP TO YOU to set an appropriate simulation'
          h(50)='timestep. '
          h(51)=' '
          H(52)='You are given the option to discard changes when you'
          H(53)='exit the period. '
          nctlhelp=53
          nmenulines=13    ! set to the last cm index used
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Heating (fixed) capacity (W)'
          prompt1(2)='Heating capacity (W) used for ON state'
          prompt2(2)='(injection is at a fixed rate)'
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=1000.0
          menu(3)='Cooling (fixed) capacity (W)'
          prompt1(3)='Cooling capacity (W) used for ON state'
          prompt2(3)='(extraction is at a fixed rate) '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=1000.0
          menu(4)='Heating ON setpoint (C)'
          prompt1(4)='Heating ON setpoint (C) (turn on heating if'
          prompt2(4)='the temperature falls below this setpoint)'
          ndec(4)=3; vmin(4)=-100.0; vmax(4)=100.0; vdef(4)=18.0
          menu(5)='Heating OFF setpoint (C)'
          prompt1(5)='Heating OFF setpoint (C) (turn off heating if'
          prompt2(5)='the temperature goes above this point) '
          ndec(5)=3; vmin(5)=-100.0; vmax(5)=100.0; vdef(5)=22.0
          menu(6)='Cooling ON setpoint (C) '
          prompt1(6)='Cooling ON setpoint (C) (turn on cooling if'
          prompt2(6)='the temperature goes above this setpoint) '
          ndec(6)=3; vmin(6)=-100.0; vmax(6)=100.0; vdef(6)=26.0
          menu(7)='Cooling OFF setpoint (C) '
          prompt1(7)='Cooling OFF setpoint (C) (turn off cooling if'
          prompt2(7)='the temperature goes below this setpoint) '
          ndec(7)=3; vmin(7)=-100.0; vmax(7)=100.0; vdef(7)=24.0
          menu(8)='Total minutues in heating cycle'
          prompt1(8)='Total minutues in heating cycle (must be equal'
          prompt2(8)='or greater than the sum of ON and OFF cycles).'
          ndec(8)=2; vmin(8)=2.0; vmax(8)=60.0; vdef(8)=10.0
          menu(9)='Minimum heating ON minutes'
          prompt1(9)='Minimum ON minutes in heating cycle '
          prompt2(9)=' '
          ndec(9)=2; vmin(9)=1.0; vmax(9)=60.0; vdef(9)=5.0
          menu(10)='Minimum heating OFF minutes'
          prompt1(10)='Minimum OFF minutes in heating cycle'
          prompt2(10)=' '
          ndec(10)=2; vmin(10)=1.0; vmax(10)=60.0; vdef(10)=5.0
          menu(11)='Total minutues in cooling cycle'
          prompt1(11)='Total minutues in cooling cycle (must be equal'
          prompt2(11)='or greater than the sum of ON and OFF cycles)'
          ndec(11)=2; vmin(11)=2.0; vmax(11)=60.0; vdef(11)=10.0
          menu(12)='Minimum cooling ON minutes'
          prompt1(12)='Minimum ON minutes in cooling cycle'
          prompt2(12)=' '
          ndec(12)=2; vmin(12)=1.0; vmax(12)=60.0; vdef(12)=5.0
          menu(13)='Minimum cooling OFF minutes'
          prompt1(13)='Minimum OFF minutes in cooling cycle'
          prompt2(13)=' '
          ndec(13)=2; vmin(13)=1.0; vmax(13)=60.0; vdef(13)=5.0
        elseif(iclaw.eq.14)then
          h(1)='BCL14 is a floating action (three-position) controller'
          h(2)='which supports the definition of an open switch and'
          h(3)='shut switch differential (C) for heating and cooling'
          h(4)='as well as timestep rate of change for heating and '
          h(5)='cooling. '
          h(6)=' '
          h(7)='This is one of the so-called obscure zone controllers'
          h(8)='and there might be dragons in the code so take your '
          h(9)='time and be prepaired for some iteration. '
          h(10)=' '
          h(11)='The simulation timestep probably should be short e.g.'
          h(12)='a few minutes.  The simulation trace facility will'
          h(13)='report the current state and the prior state for'
          h(14)='heating and cooling.'
          h(15)=' '
          H(16)='You are given the option to discard changes when you'
          H(17)='exit the period. '
          nctlhelp=17
          nmenulines=15    ! set to the last cm index used
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=100.0; vdef(1)=0.0
          menu(2)='Heating set-point (C)'
          prompt1(2)='Heating set-point (deg.C) (remember there is'
          prompt2(2)='a deadband as well) '
          ndec(2)=3; vmin(2)=0.0; vmax(2)=100.0; vdef(2)=20.0
          menu(3)='Heating deadband (C)'
          prompt1(3)='Heating deadband (C) (suggest that heating'
          prompt2(3)='and cooling T bands do not cross) '
          ndec(3)=3; vmin(3)=0.0; vmax(3)=10.0; vdef(3)=2.0
          menu(4)='Heat shut switch differential C'
          prompt1(4)='heating shut switch differential (C) '
          prompt2(4)=' '
          ndec(4)=3; vmin(4)=0.0; vmax(4)=10.0; vdef(4)=1.0
          menu(5)='Heat open switch differential C'
          prompt1(5)='heat open switch differential (C)'
          prompt2(5)=' '
          ndec(5)=3; vmin(5)=0.0; vmax(5)=10.0; vdef(5)=1.0
          menu(6)='Cooling set-point (C)'
          prompt1(6)='Cooling set-point (deg.C) (remember there is'
          prompt2(6)='a deadband as well) '
          ndec(6)=3; vmin(6)=0.0; vmax(6)=100.0; vdef(6)=24.0
          menu(7)='Cooling deadband (C) '
          prompt1(7)='Cooling deadband (deg.C) (suggest that heating'
          prompt2(7)='and cooling T bands do not cross) '
          ndec(7)=3; vmin(7)=0.0; vmax(7)=10.0; vdef(7)=2.0
          menu(8)='Cool open switch differential C'
          prompt1(8)='cooling open switch differential (C)'
          prompt2(8)=' '
          ndec(8)=3; vmin(8)=0.0; vmax(8)=10.0; vdef(8)=1.0
          menu(9)='Cool shut switch differential C'
          prompt1(9)='Cooling shut switch differential (C)'
          prompt2(9)=' '
          ndec(9)=3; vmin(9)=0.0; vmax(9)=10.0; vdef(9)=1.0
          menu(10)='Maximum heating flux (W)'
          prompt1(10)='Maximum heating flux (W)'
          prompt2(10)=' '
          ndec(10)=1; vmin(10)=0.0; vmax(10)=999999.0; vdef(10)=1000.0
          menu(11)='Minimum heating flux (W) '
          prompt1(11)='Minimum heating flux (W) (typically zero and'
          prompt2(11)='can be non-zero for uninsulated pipes etc.)'
          ndec(11)=1; vmin(11)=0.0; vmax(11)=999999.0; vdef(11)=0.0
          menu(12)='Rate of change heater actuator'
          prompt1(12)='Rate of change of heater actuator (in building'
          prompt2(12)='timesteps) moving from one state to another'
          ndec(12)=0; vmin(12)=0.0; vmax(12)=10.0; vdef(12)=1.0
          menu(13)='Maximum cooling flux (W) '
          prompt1(13)='Maximum cooling flux (W) '
          prompt2(13)=' '
          ndec(13)=1; vmin(13)=0.0; vmax(13)=999999.0; vdef(13)=1000.0
          menu(14)='Minimum cooling flux (W) '
          prompt1(14)='Minimum cooling flux (W) (typically zero and'
          prompt2(14)='can be non-zero for uninsulated pipes etc.)'
          ndec(14)=1; vmin(14)=0.0; vmax(14)=999999.0; vdef(14)=0.0
          menu(15)='Rate of change cooling actuator'
          prompt1(15)='Rate of change of cooling actuator (in building'
          prompt2(15)='timesteps) moving from one state to another.'
          ndec(15)=0; vmin(15)=0.0; vmax(15)=10.0; vdef(15)=1.0
        elseif(iclaw.eq.15)then
          h(1)='The optimum start controller allows for three modes '
          h(2)='of operation: '
          h(3)=' (1) Default start at 4AM'
          h(4)=' (2) Start at user defined time'
          h(5)=' (3) Birtles & John equationq estimate which is '
          h(6)='     based on A New Optimum Start Control Algorithm'
          h(7)='     in Proceedings of International Symposium'
          h(8)='     on Control and Operation of Building HVAC'
          h(9)='     Systems in 1985 Trondheim, Norway'
          h(10)=' '
          h(11)='Note this control is designed for optimum start '
          h(12)='of heating only. It is one of the lesser used '
          h(13)='controls - pay attention to the predicted performance.'
          h(14)=' '
          h(15)='Heating capacity is in W to be used during the'
          h(16)='optimum startup period. '
          h(17)=' '
          h(18)='The heating setpoint (C) is the desired temperature'
          h(19)='at the desired-time-of-arrival. '
          h(20)=' '
          h(21)='The band of acceptable temperatures (C) is applied'
          h(22)='to the heating setpoint to provide an upper and '
          h(23)='lower setpoints. A small band will force tighter'
          h(24)='control. '
          h(25)=' '
          h(26)='If the optimal control does not reach the band of'
          h(27)='acceptable temps at the desired-time-of-arrival '
          h(28)='then it will rewind and try again. You can control '
          h(29)='the time difference between successive trails by '
          h(30)='setting a minimum-time-difference. '
          h(30)=' '
          h(31)='The Birtles & John Eqn. asks for three coefficients '
          h(32)='the first is a heating constant, the second is a '
          h(33)='building constant and the third is an exterior '
          h(34)='temperature constant. Please review the paper '
          h(35)='in order to understand these coefficients. '
          h(36)=' '
          h(37)='This control uses a rewind action that resets the'
          h(38)='simulation timestep and `tries again`. During the'
          h(39)='simulation the `monitor` facility will get confused'
          h(40)='and some graphs in the results module might not '
          h(41)='plot correctly.  Use this control with care. '
          h(42)=' '
          H(43)='You are given the option to discard changes when you'
          H(44)='exit the period. '
          nctlhelp=44
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=100.0; vdef(1)=0.0
          menu(2)='Heating capacity (W) '
          prompt1(2)='Heating capacity (W) available during the '
          prompt2(2)='startup period. '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=1000.0
          menu(3)='Heating setpoint (desired) (C) '
          prompt1(3)='Heating setpoint (C) which we desire to '
          prompt2(3)='reach by the desired time-of-arrival '
          ndec(3)=3; vmin(3)=0.0; vmax(3)=100.0; vdef(3)=20.0
          menu(4)='Temperature differential (C) '
          prompt1(4)='Temperature differential (C) to be applied to'
          prompt2(4)='the setpoint to create an acceptable band'
          ndec(4)=3; vmin(4)=0.0; vmax(4)=10.0; vdef(4)=1.0
          menu(5)='Desired time-of-arrival (hours) '
          prompt1(5)='Desired time-of-arrival (hours) when the '
          prompt2(5)='temperatrure should be within the accept band.'
          ndec(5)=2; vmin(5)=0.0; vmax(5)=24.0; vdef(5)=8.0
          menu(6)='Minimum time difference (hours) '
          prompt1(6)='Minimum time difference (hours) to try before '
          prompt2(6)='or after guess. '
          ndec(6)=3; vmin(6)=0.1; vmax(6)=2.0; vdef(6)=0.2
          if(nint(cm(7)).eq.1)then
            nmenulines=8    ! set to the last cm index used
            menu(7)='Start >> Default of 4.00 a.m.'
            prompt1(7)='Default of 4.00 a.m. (value one), User start'
            prompt2(7)='(value two) and Birtles (value three) '
            ndec(7)=0; vmin(7)=0.0; vmax(7)=3.0; vdef(7)=1.0
            menu(8)=' '
            prompt1(8)=' '
            prompt2(8)=' '
            ndec(8)=-1; vmin(8)=0.0; vmax(8)=0.0; vdef(8)=0.0
            menu(9)=' '
            prompt1(9)=' '
            prompt2(9)=' '
            ndec(9)=-1; vmin(9)=0.0; vmax(9)=0.0; vdef(9)=0.0
            menu(10)=' '
            prompt1(10)=' '
            prompt2(10)=' '
            ndec(10)=-1; vmin(10)=0.0; vmax(10)=0.0; vdef(10)=0.0
          elseif(nint(cm(7)).eq.2)then
            nmenulines=8    ! set to the last cm index used
            menu(7)='Start >> User defined start'
            prompt1(7)='User start (value two ),4.00 a.m. (value one)'
            prompt2(7)='and Birtles (value three) '
            ndec(7)=0; vmin(7)=0.0; vmax(7)=3.0; vdef(7)=1.0
            menu(8)='Initial guess for start (hr)'
            prompt1(8)='Initial guess for start time (hr)'
            prompt2(8)=' '
            ndec(8)=3; vmin(8)=0.0; vmax(8)=24.0; vdef(8)=4.0
            menu(9)=' '
            prompt1(9)=' '
            prompt2(9)=' '
            ndec(9)=-1; vmin(9)=0.0; vmax(9)=0.0; vdef(9)=0.0
            menu(10)=' '
            prompt1(10)=' '
            prompt2(10)=' '
            ndec(10)=-1; vmin(10)=0.0; vmax(10)=0.0; vdef(10)=0.0
          elseif(nint(cm(7)).eq.3)then
            nmenulines=10    ! set to the last cm index used
            menu(7)='Start >> Birtles & John Eqn.'
            prompt1(7)='Birtles & John Eqn. (value three),4.00 a.m.'
            prompt2(7)='(value one), User start (value two)'
            ndec(7)=0; vmin(7)=0.0; vmax(7)=3.0; vdef(7)=1.0
            menu(8)='Birtles & John Eqn. heating coef'
            prompt1(8)='Birtles & John Eqn. heating coef'
            prompt2(8)='(see the referenced paper) '
            ndec(8)=2; vmin(8)=0.0; vmax(8)=99999.0; vdef(8)=1.0
            menu(9)='Birtles & John Eqn. build coef'
            prompt1(9)='Birtles & John Eqn. build coef'
            prompt2(9)='(see the referenced paper) '
            ndec(9)=2; vmin(9)=0.0; vmax(9)=99999.0; vdef(9)=1.0
            menu(10)='Birtles & John Eqn. build coef'
            prompt1(10)='Birtles & John Eqn. building coef'
            prompt2(10)='(see the referenced paper) '
            ndec(10)=2; vmin(10)=0.0; vmax(10)=99999.0; vdef(10)=1.0
          endif
        elseif(iclaw.eq.16)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.17)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.18)then
          menu(1)='Null control has no data.'
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=0; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
        elseif(iclaw.eq.19)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.20)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.21)then
          H(1) ='Proportional capacity slave controller. Typically a'
          h(2) ='slave control is used to represent an environmental'
          h(3) ='control with a thermostat in one zone and actions in'
          h(4) ='a number of zones. Example: a residentail AC system'
          H(5) ='with a central fan delivering to several zones.'
          H(6) =' '
          H(7) ='Flux is delivered to slave control when required by'
          H(8) ='master control, at same % of base (max) capacity.'
          H(9) =' '
          H(10)='You are asked for three values:'
          H(11)='  - index of control loop associated with the'
          H(12)='    master control (which MUST use a basic ideal'
          H(13)='    controller or a free float control and the master'
          H(14)='    MUST also sense a specific zone in the model).'
          H(15)=' '
          h(16)='  - base (maximum) heating capacity for slave'
          H(17)='  - base (maximum  cooling capacity for slave.'
          H(18)=' '
          h(19)='Procedure - first create the master control loop'
          h(20)='and then create a slave loop for each other zone'
          H(21)='in the model which acts as a slave. If the master'
          H(22)='control loop is for weekday Saturday Sunday then'
          H(23)='each slave control should follow this pattern and'
          H(24)='the number of periods should also match. '
          H(25)=' '
          H(26)='The sensor of each slave controller MUST point to'
          H(27)='the SAME zone that the master controller senses.'
          H(28)=' '
          H(29)='The actuator of each slave controller MUST point to'
          h(30)='the slave zone. '
          h(31)=' '
          h(32)='The simulation timestep should typically be short '
          h(33)='enough to reflect the cycling of the environmental'
          h(34)='control system. '
          h(35)=' '
          h(36)='Example: in a house there is 4kW of heating and the'
          h(37)='thermostat is in the lounge which has 1kW of heating'
          h(38)='so the slave capacity in the other zones should'
          h(39)='total to 3kW. Setting the capacity in each slave'
          h(40)='zone can be done via different methods:'
          h(41)=' a) as a proportion of the duct flow or radiator'
          h(42)='    size in each zone'
          h(43)=' b) by doing an initial simulation with each zone'
          h(44)='    ideally controlled and noticing the ratio of'
          h(45)='    the peak demands in each room'
          h(46)=' c) (roughly) as a function of zone volume or floor'
          h(47)='    area.'
          h(48)=' '
          H(49)='You are given the option to discard changes when you'
          H(50)='exit the period. '
          nctlhelp=50
          nmenulines=4    ! set to the last cm index used
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Index of master control loop'
          prompt1(2)='Index of master control loop (this loop should'
          prompt2(2)='already exist) '
          ndec(2)=0; vmin(2)=1.0; vmax(2)=100.0; vdef(2)=1.0
          menu(3)='Max slave heating capacity (W)'
          prompt1(3)='Maximum slave heating capacity (W) (for when'
          prompt2(3)='the master control is at 100%) '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=1000.0
          menu(4)='Max slave cooling capacity (W) '
          prompt1(4)='Maximum slave cooling capacity (W) (for when'
          prompt2(4)='the master control is at 100%)'
          ndec(4)=1; vmin(4)=0.0; vmax(4)=999999.0; vdef(4)=1000.0
        elseif(iclaw.eq.22)then

C Variable supply volume cooling control which reverts to a CAV system
C using reheat for heating mode.
          h(1)='This control represents an ideal air based '
          h(2)='environmental control system that delivers air '
          h(3)='at a variable flow rate and a fixed supply temp'
          h(4)='for cooling to a desired setpoint.'
          h(5)=' '
          h(6)='In heating mode it reverts to a CAV system at the'
          h(7)='minimal supply flow rate with reheat applied up to a '
          h(8)='maximum flux to reach the desired setpoint.'
          h(9)=' '
          h(10)='There is a single desired room setpoint (C) so'
          h(11)='the control law will cycle between VAV and CAV '
          h(12)='model under low-load conditions. '
          h(13)=' '
          h(14)='It is recommended that a short timestep be used with'
          h(15)='this control ( 1 - 5 minutes). '
          h(16)=' '
          h(17)='This control law can be used to check whether an '
          h(18)='air based system is able to meet demand patterns in '
          h(19)='a room prior to specification of a detailed component'
          h(20)='model. If flow rates or supply temperature are not '
          h(21)='well sepecified then the room temperature will drift '
          h(22)='as expected. '
          h(23)=' '
          h(24)='Specification of minimum flow rate is a compromise '
          h(25)='between what is required for CAV heating and excess '
          h(26)='cooling in low-load conditions. '
          h(27)=' '
          h(28)='Typically the maximum flow rate would be equivalent '
          h(29)='to 12-20ach in the zone. Low max flow rates can result'
          h(30)='in failure to meet the desired temperature. And high'
          h(31)='minimum flow rates can result in overcooling in mild '
          h(32)='conditions. '
          h(33)=' '
          h(34)='Additional performance data (e.g. flow rates, reheat)'
          h(35)='if you invoke control trace at simulation time.'
          h(36)=' '
          h(37)='NOTE: THIS LAW SHOULD BE CONSIDERED BETA version.'
          h(38)=' '
          H(39)='You are given the option to discard changes when you '
          H(40)='exit the period. '
          nctlhelp=40
          nmenulines=6    ! set to the last cm index used.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Reheat maximum capacity (W)'
          prompt1(2)='Reheat maximum capacity (W) is injected into '
          prompt2(2)='the CAV supply stream. '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=99999.0; vdef(2)=1000.0
          menu(3)='Supply air temperature (C)'
          prompt1(3)='Supply air temperature (C)'
          prompt2(3)=' '
          ndec(3)=3; vmin(3)=-10.0; vmax(3)=100.0; vdef(3)=12.0
          menu(4)='Desired room air temp (C)'
          prompt1(4)='Desired room air temperature (C)'
          prompt2(4)=' '
          ndec(4)=3; vmin(4)=-10.0; vmax(4)=100.0; vdef(4)=20.0
          menu(5)='Air supply flow max rate (m^3/s)'
          prompt1(5)='Air supply flow maximum rate (m^3/sec) '
          prompt2(5)=' '
          ndec(5)=4; vmin(5)=0.0; vmax(5)=100.0; vdef(5)=1.0
          menu(6)='Air supply flow min rate (m^3/s)'
          prompt1(6)='Air supply flow minimum rate (m^3/sec) '
          prompt2(6)=' '
          ndec(6)=4; vmin(6)=0.0; vmax(6)=100.0; vdef(6)=0.5
        elseif(iclaw.eq.23)then

C Reading heating and cooling setpoints from temporal file
          H(1)='This control law sets the setpoints based on heating'
          H(2)='and cooling temperatures as read from a temporal file'
          H(3)='defined in the model context > impose measured data'
          H(4)='section of the project manager interface. Any amount'
          H(5)='of heating and cooling is added so that the setpoints'
          H(6)='are met.'
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=5.0
          menu(2)='Heating setpoint column number'
          prompt1(2)='Set the column number which contains heating'
          prompt2(2)='setpoint value in the temporal (tdf) file'
          ndec(2)=0; vmin(2)=1.0; vmax(2)=80.0; vdef(2)=5.0
          menu(3)='Heating setpoint column number'
          prompt1(3)='Set the column number which contains cooling'
          prompt2(3)='setpoint value in the temporal (tdf) file'
          ndec(3)=0; vmin(3)=1.0; vmax(3)=80.0; vdef(3)=6.0
          nmenulines=3    ! set to the last cm index used
          nctlhelp=6
        elseif(iclaw.eq.24)then

C Adaptive human comfort model. A comfort temperature is calculated
C based on running mean ambient temperature. If the environment
C temperature of the controlled zone is now within 2 degrees of this
C comfort temperature the control is activated
          H(1) ='Adaptive comfort algorithm models occupant behaviour'
          H(2) ='and operates heating/cooling based on mean operative'
          H(3) ='and outdoor mean temperatures automatically.'
          H(4) ='A comfort temperature is calculated based on the  '
          H(5) ='running mean response parameter that governs how'
          H(6) ='running mean temperature is calculated. This parameter'
          H(7) ='is a constant from 0-0.99 (Default value 0.8)'
          H(8) ='The comfort band also needs to be entered, this is the'
          H(9) ='tolerance from calculated comfort temperature and  '
          H(10)='has a default value of 2degC '
          H(11)='This essentially creates a heating and cooling  '
          H(12)='set point of magnitude of comfort band on both sides'
          H(13)='of the comfort temperature and controls to that '
          H(14)='temperature based on probability that heating/ccoling '
          H(15)='will be switched on. '
          H(16)='The statically entered set points are currently not  '
          H(17)='used in the calculation but are retained for possible '
          H(18)='future use. '
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Maximum heating capacity (W)'
          prompt1(2)='Maximum heating capacity (W) which is available'
          prompt2(2)='to meet the demand. '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=1000.0
          menu(3)='Minimum heating capacity (W)'
          prompt1(3)='Minimum heating capacity (W) The flux injected'
          prompt2(3)='will never drop below this level. '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=0.0
          menu(4)='Maximum cooling capacity (W)'
          prompt1(4)='Maximum cooling capacity (W) which is available'
          prompt2(4)='to meet the demand. '
          ndec(4)=1; vmin(4)=0.0; vmax(4)=999999.0; vdef(4)=1000.0
          menu(5)='Minimum cooling capacity (W)'
          prompt1(5)='Minimum cooling capacity (W) The flux extracted'
          prompt2(5)='will never be less than this. '
          ndec(5)=1; vmin(5)=0.0; vmax(5)=999999.0; vdef(5)=0.0
          menu(6)='Heating setpoint (C)'
          prompt1(6)='Heating setpoint (C) (temperature to meet if'
          prompt2(6)='there is sufficient capacity) '
          ndec(6)=3; vmin(6)=-100.0; vmax(6)=100.0; vdef(6)=20.0
          menu(7)='Cooling setpoint (C)'
          prompt1(7)='Cooling setpoint (C (temperature to meet if)'
          prompt2(7)='there is sufficient capacity) '
          ndec(7)=3; vmin(7)=-100.0; vmax(7)=100.0; vdef(7)=24.0
          menu(8)='Running mean response parameter'
          prompt1(8)='Running mean response parameter'
          prompt2(8)=' '
          ndec(8)=3; vmin(8)=0.0; vmax(8)=0.99; vdef(8)=0.8
          menu(9)='Comfort band '
          prompt1(9)='Comfort band (default is 2deg) '
          prompt2(9)=' '
          ndec(9)=3; vmin(8)=0.0; vmax(8)=0.99; vdef(8)=2.0
          nmenulines=9    ! set to the last cm index used
          nctlhelp=18
        else
          h(1)=' '
          h(2)=' '
          h(3)=' '
          h(4)=' '
          nctlhelp=4
          nmenulines=7    ! set to the last cm index used
          menu(1)=' '
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=2; vmin(1)=0.0; vmax(1)=100.0; vdef(1)=0.0
          menu(2)=' '
          prompt1(2)=' '
          prompt2(2)=' '
          ndec(2)=2; vmin(2)=0.0; vmax(2)=100.0; vdef(2)=0.0
          menu(3)=' '
          prompt1(3)=' '
          prompt2(3)=' '
          ndec(3)=2; vmin(3)=0.0; vmax(3)=100.0; vdef(3)=0.0
          menu(4)=' '
          prompt1(4)=' '
          prompt2(4)=' '
          ndec(4)=2; vmin(4)=0.0; vmax(4)=100.0; vdef(4)=0.0
          menu(5)=' '
          prompt1(5)=' '
          prompt2(5)=' '
          ndec(5)=2; vmin(5)=0.0; vmax(5)=100.0; vdef(5)=0.0
          menu(6)=' '
          prompt1(6)=' '
          prompt2(6)=' '
          ndec(6)=2; vmin(6)=0.0; vmax(6)=100.0; vdef(6)=0.0
          menu(7)=' '
          prompt1(7)=' '
          prompt2(7)=' '
          ndec(7)=2; vmin(7)=0.0; vmax(7)=100.0; vdef(7)=0.0
          menu(8)=' '
          prompt1(8)=' '
          prompt2(8)=' '
          ndec(8)=2; vmin(8)=0.0; vmax(8)=100.0; vdef(8)=0.0
          menu(9)=' '
          prompt1(9)=' '
          prompt2(9)=' '
          ndec(9)=2; vmin(9)=0.0; vmax(9)=100.0; vdef(9)=0.0
          menu(10)=' '
          prompt1(10)=' '
          prompt2(10)=' '
          ndec(10)=2; vmin(10)=0.0; vmax(10)=100.0; vdef(10)=0.0
          menu(11)=' '
          prompt1(11)=' '
          prompt2(11)=' '
          ndec(11)=2; vmin(11)=0.0; vmax(11)=100.0; vdef(11)=0.0
          menu(12)=' '
          prompt1(12)=' '
          prompt2(12)=' '
          ndec(12)=2; vmin(12)=0.0; vmax(12)=100.0; vdef(12)=0.0
          menu(13)=' '
          prompt1(13)=' '
          prompt2(13)=' '
          ndec(13)=2; vmin(13)=0.0; vmax(13)=100.0; vdef(13)=0.0
        endif
      elseif(icfoc.eq.1)then
        continue
      elseif(icfoc.eq.2)then
        continue
      elseif(icfoc.eq.3)then
        continue
      elseif(icfoc.eq.4)then
        continue
      endif
      return
      end
