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


C Subroutines:
C   epltnet - Plant network definition/editing.
C   edcomp  - supports editing of network plant components.
C   condel - deletes a plant connection (lpick) selected by the user
C   delcomp - delets a user specified plant component (ipcomp) 
C   ASKPCMP - presents a list of plant components returning the index IS.
C   ASKPCON - presents a list of plant connections returning the index IS.
C   askcont - presents a list of plant containments returning the index IS.


C ********** epltnet **********
C Plant network definition/editing.

      subroutine epltnet(iedit,ier)

#include "esprdbfile.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

C Plant network.
      common/C23/IFPNF,LPNF

      CHARACTER*32 ITEM(7)
      character mode*1,LPNF*72
      integer NITMS,INO ! max items and current menu item

      helpinsub='pltnet'     ! set for subroutine

C Use next unit up from plant network file for the plant database.
      ifpnf=IFIL+1
      ipcdb=IFPNF+1

C Let the user pick a menu item.
   10 INO=-5
      ITEM(1) ='Define/edit plant network:'
      ITEM(2) ='a using graphical interface'
      ITEM(3) ='b using menu interface     ' 
      ITEM(4) ='  ------------------       '
      ITEM(5) ='                           '
      ITEM(6) ='? help                     '
      ITEM(7) ='- exit menu'
      NITMS=7

C Help text for this menu.
  12  helptopic='plant_net_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Uncomment following line of code when graphical interface becomes
C available, The line setting INO=3 should also be deleted then
C      CALL EMENU(' Plant Network',ITEM,NITMS,INO)
      INO=3
      IF(INO.EQ.2)THEN
        call easki(ians,' ','Password: ',0,'-',0,'-',0,'Password',
     &    IERI,nbhelp)
        if(ieri.eq.-3) goto 10
        if(ians.ne.101) goto 10

C Call the 'net' program for network definition.
        mode='G'
        CALL NEWNET(iedit,mode)
      ELSEIF(INO.EQ.3)THEN

C Define or edit a plant network.
         mode='-'
         call newnet(iedit,mode)
      elseif(ino.eq.NITMS-1) then

C Display help message.
         CALL PHELPD('pdf opening',15,'-',0,0,IER)
      ELSEIF(INO.EQ.NITMS)THEN

C Exit.
        return
      ELSE
        INO=-1
        GOTO 12
      ENDIF
      return
      end

C ********** edcomp **********
C Supports editing of network plant components.

      subroutine edcomp(ipcomp)

#include "plant.h"
#include "help.h"

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD) 
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA) 
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/pcnam/pcname(mpcom)
      common/hfpar/hfpdsc(mpcom,madata)
      dimension ival(mpcom)      
     
      character pcname*15,txt*72,hfpdsc*68,t15*15,outs*124
      character*68 mscdsc, cvrdsc

      helpinsub='pltnet'     ! set for subroutine

C Ask the user what they want to edit.
      helptopic='plant_comp_attributes'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABC(' ','Edit options:',
     & 'name and component data','optional flow data','cancel',IOPT,
     & nbhelp)

C Wrong pick.
      IF(IOPT.EQ.3) RETURN
       
C Change heat flow parameters
      if(IOPT.eq.1) then
C Open plant component data base to initialise plant components
C entry pointer.

C Change component name
  210   t15=pcname(ipcomp)
        helptopic='plant_comp_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(t15,' ',
     &     'Component name?',
     &     15,' ',' component name',IER,nbhelp)
        if(t15(1:2).ne.'  '.and.t15(1:4).ne.'UNKN')then
          pcname(ipcomp)=t15
        else
          goto 210
        endif
        if(ier.ne.0) goto 210

        call rcdata(ipcomp,2)
        if(nci(ipcomp).gt.0) then

C If component has control variable(s),
C prompt user to enter values.
          helptopic='plant_ctl_variable'
          call gethelptext(helpinsub,helptopic,nbhelp)
          do 101 jj=1, nci(ipcomp)
            txt=cvrdsc(jj)
            value=0.0
            value=cdata(ipcomp,jj)
            CALL EASKR(value,' ',txt,0.,'-',0.,'-',0.,
     &        ' item value?',IER,nbhelp)
            cdata(ipcomp,jj)=value
  101     continue
        endif

C Read components data from database.
        do 23 ipc=1,npcomp
          call rcdata(ipc,2)
          do 24 ipar=1, npi(ipc)
  24        hfpdsc(ipc,ipar)=mscdsc(ipar)
  23    continue
        do 15 ipar=1, npi(ipcomp)
          mscdsc(ipar)=hfpdsc(ipcomp,ipar)
  15    continue
   20   call askpar(itm,ival,ipcomp)
        if(itm.ne.0) then

C Read new value for selected item(s).
          do 106 iij = 1,itm
            txt=mscdsc(ival(iij))
            dvalue=tadata(ipcomp,ival(iij))
            value=dvalue
            call easkr(value,' ',txt,0.,'-',0.,'-',dvalue,
     &        ' item value?',IER,nbhelp)
             tadata(ipcomp,ival(iij))=value
  106     continue
          write(outs,'(2a)') 'Updated data for ',pcname(ipcomp)
          call edisp(iuout,outs)
          do 910 jj=1,npi(ipcomp)
            write(outs,'(a50,a3,g12.5)')
     &        mscdsc(jj),' : ',tadata(ipcomp,jj)
            call edisp(iuout,outs)
  910     continue
          goto 20
        endif
        RETURN  
      elseif(IOPT.eq.2) then

C Get the mass flow data for the component from the database.
        call askmfpar(ipcomp)   
        RETURN  
      endif      
      end


C ********** condel **********
C Delete a plant connection (lpick) selected by the user.

      subroutine condel(lpick)

#include "plant.h"

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)

      integer lpick

C Erase the selected connection details. 
       if (lpick.lt.npcon.and.lpick.gt.0) then

C Delete the connection.
         DO 100 i=lpick,npcon-1
          ipc1(i)=ipc1(i+1)
          ipc2(i)=ipc2(i+1)
          ipn1(i)=ipn1(i+1)
          ipn2(i)=ipn2(i+1)
          ipct(i)=ipct(i+1)
          pcondr(i)=pcondr(i+1)
          pconsd(i,2)=pconsd(i+1,2)
100     continue

C Set final array elements to zero.
        ipc1(npcon)=0
        ipc2(npcon)=0
        ipn1(npcon)=0
        ipn2(npcon)=0
        ipct(npcon)=0
        pcondr(npcon)=0
        pconsd(npcon,1)=0
        pconsd(npcon,2)=0
        npcon=npcon-1
      elseif(lpick.eq.npcon) then

C If lpick is the last item in the list then zero values
C and reduce the number of connections by one.
        ipc1(npcon)=0
        ipc2(npcon)=0
        ipn1(npcon)=0
        ipn2(npcon)=0
        ipct(npcon)=0
        pcondr(npcon)=0
        pconsd(npcon,1)=0
        pconsd(npcon,2)=0
        npcon=npcon-1
      else
        return
      endif      
      end

C ********** DELCOMP **********
C Deletes a user specified plant component (ipcomp) and its
C containment, but only if the component is not associated
C with any connections.

       SUBROUTINE DELCOMP(IPCOMP)
       
#include "plant.h"
         
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)

C IPCC(i) - index of plant component for containment i.
      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)
      common/mfng/nodnam(mpcom), fcmp(mpcom)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT  
      common/pcnam/pcname(mpcom)  
      character pcname*15,pcdesc*80,message*72
      
      character*6  nodnam, fcmp

C Check if ipcomp is within allowable range. 
      IF (IPCOMP.EQ.0.OR.IPCOMP.GT.NPCOMP) RETURN

C Check to see if component is associated with another
c connection.
        do 98 i=1,npcon
          if(ipcomp.eq.ipc1(i).or.ipcomp.eq.ipc2(i))then
           write(message,'(a,i2,a,i2,a)') 
     &       'Please delete associated connection ',
     &       ipc1(i),' or ',ipc2(i),' first!'
           call USRMSG(' ',message,'-')
           return
          endif         
98      continue
 
C Check which containment this component is associated with.
        icfound = 0

        do 99 i=1,ncont
         if(ipcomp.eq.ipcc(i)) then

C If user specified component matches ipcc(i) then mark it
C for removal. 
           write(message,'(a,i2,a)') 
     &       'About to delete associated containment ',
     &       ipcc(i),'...'
           icfound = i
           call USRMSG(' ',message,'-')
         elseif(ipcomp.lt.ipcc(i)) then

C Decrement subsequent pointers (because component list is about
C to be compacted as well.
           ipcc(i)=ipcc(i)-1
         endif 
99      continue

C If icfound non-zero then compact the list of containments.
        IF(icfound.GT.0) THEN          
          IF(icfound.LT.NCONT) THEN
            DO 102 J=icfound,NCONT-1
              IPCC(J)=IPCC(J+1)
              INDCP(J)=INDCP(J+1)
              CNTDAT(J,1)=CNTDAT(J+1,1)
              CNTDAT(J,2)=CNTDAT(J+1,2)
              CNTDAT(J,3)=CNTDAT(J+1,3)
102         CONTINUE
          ELSE
            IPCC(icfound)=0
            INDCP(icfound)=0
            CNTDAT(icfound,1)=0.
            CNTDAT(icfound,2)=0.
            CNTDAT(icfound,3)=0.
          ENDIF
          NCONT=NCONT-1
        ENDIF

C Before deleting a component search for references to subsequent
C components and decrement their value so they will point to the
C correct component index after the list is compacted.

C Debug.
C        write(6,*) 'c ipc1 ',ipc1
C        write(6,*) 'c ipc2 ',ipc2

        DO 103 I=1,NPCON
          if(IPC1(i).gt.IPCOMP) IPC1(i)=IPC1(i)-1
          if(IPC2(i).gt.IPCOMP) IPC2(i)=IPC2(i)-1
  103   continue

C Debug.
C        write(6,*) 'd ipc1 ',ipc1
C        write(6,*) 'd ipc2 ',ipc2

C DELETE COMPONENT BY INCREMENTING ARRAY AND SETTING FINAL ARRAY 
C ELEMENTS TO ZERO
C CHECK THAT CHOSEN COMPONENT IS NOT THE FINAL ARRAY ELEMENT

C Debug.
C        write(6,*) 'e pcname ',pcname

        IF(IPCOMP.LT.NPCOMP.AND.IPCOMP.GT.0) then 
          DO 101 I=IPCOMP,(NPCOMP-1)
            PCNAME(I)=PCNAME(I+1)
            PCDESC(I)=PCDESC(I+1)
            NPREF(I)=NPREF(I+1)
            NNODES(I)=NNODES(I+1)
            ICFFS(I)=ICFFS(I+1)
            NPI(I)=NPI(I+1)
            NCI(I)=NCI(I+1)

C MASS FLOW DATA ASSOCAITED WITH COMPONENT        
            FCMP(I)=FCMP(I+1)
            NODNAM(I)=NODNAM(I+1)

C CONNECTIONS DATA FOR  COMPONENT
            DO 200 K=1,MNODEC
              NDCON(I,K)=NDCON(I+1,K)
              ISV(I,K)=ISV(I+1,K)
200         CONTINUE

C COMPONENT DATA
           DO 201 K=1,MMISCD
             CDATA(I,K)=CDATA(I+1,K)
201        CONTINUE
           DO 202 K=1,MADATA
             TADATA(I,K)=TADATA(I+1,K)
202         CONTINUE
101       CONTINUE
        END IF

C SET FINAL ARRAY ELEMENT TO ZERO
        PCNAME(NPCOMP)=' '
        NPREF(NPCOMP)=0
        NNODES(NPCOMP)=0
        ICFFS(NPCOMP)=0
        NPI(NPCOMP)=0
        NCI(NPCOMP)=0
        DO 300 K=1,MNODEC
          NDCON(NPCOMP,K)=0
          ISV(NPCOMP,K)=0
300     CONTINUE
        DO 301 K=1,MMISCD
          CDATA(NPCOMP,K)=0
301     CONTINUE
        DO 302 K=1,MADATA
          TADATA(NPCOMP,K)=0
302     CONTINUE
    
C REDUCE NO OF PLANT COMPONENTS BY ONE
        NPCOMP=NPCOMP-1

C Debug.
C        write(6,*) 'f pcname ',pcname

        RETURN
       END

C ********************** ASKPCMP
C Presents a list of plant components returning the index IS.
C If MOD = 'M' then include option to add an item then the appropriate
C interaction is begun.
C **CDB affected

      SUBROUTINE ASKPCMP(head,MOD,IS,IER)

#include "epara.h"
#include "plant.h"
#include "gencompDB.h"
c#include "gencompDB_common.h"
#include "help.h"

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C9plus/UCdbNam(MPCOM),CdbCat(MPCOM),CDBDesc(MPCOM)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PCNAM/PCNAME(MPCOM)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      common/pcsort/icode(maxpc)

C Common for new format component database.
      COMMON/NCDBFORM/ICDBF


      DIMENSION cVERT(35), CTYPE(5)
      CHARACTER PCNAME*15, UCdbNam*16, CDBCAT*32, CDBDesc*72
      character*80 pcdesc
      integer icdbf

      character*(*) head
      CHARACTER cVERT*48,KEY*1,MOD*1,ctype*16,desc*16
      LOGICAL SELECT,OK
      integer MVERT,IVERT,IDVERT ! max items and current menu item

      helpinsub='pltnet'     ! set for subroutine

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.

C if icdbf check here!
      ctype(1)= 'air conditioning'
      ctype(2)= 'water heating   '
      ctype(3)= 'primitive part  '
      ctype(4)= 'solar and other '
      ctype(5)= 'heat network    '
      ILEN=NPCOMP
      IPACT=CREATE
      CALL EKPAGE(IPACT)

   3  IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=6
      ILEN=NPCOMP

C *** CDB affected!
C Fill the component description arrays.
      CALL DESCPC
C *** needs to be checked!

C Initial menu entry setup.

   92 IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      IF(ICDBF.EQ.0)THEN
        WRITE(cVERT(1),'(A)') ' Name       | ref. no. |      Type'
      ELSE
        WRITE(cVERT(1),'(A)') ' Name       | db ids.  |      Category'
      ENDIF
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          IF(ICDBF.EQ.0)THEN
            NRF=NPREF(L)
            IF(ICODE(NRF).GT.0.AND.ICODE(NRF).LT.200)THEN
              WRITE(DESC,'(A16)')CTYPE(1)
            ELSEIF(ICODE(NRF).GE.200.AND.ICODE(NRF).LT.510)THEN
              WRITE(DESC,'(A16)')CTYPE(2)
            ELSEIF(ICODE(NRF).GE.510.AND.ICODE(NRF).LT.700)THEN
              WRITE(DESC,'(A16)')CTYPE(3)
            ELSEIF(ICODE(NRF).GE.700.AND.ICODE(NRF).LT.1340)THEN
              WRITE(DESC,'(A16)')CTYPE(4)
            ELSE
              WRITE(DESC,'(A16)')CTYPE(5)
            ENDIF
            WRITE(cVERT(M),'(A1,1X,A15,3X,I3,3X,A16)')
     &KEY,PCNAME(L),NPREF(L),DESC
          ELSE            
            WRITE(cVERT(M),'(A1,1X,A15,1X,A7,1X,A16)')
     &KEY,PCNAME(L),UCDBNam(L)(1:7),CDBCAT(L)(1:16)
          ENDIF
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        cVERT(M+1)='  ________________ '
      ELSE
        WRITE(cVERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' :',MPM 
      ENDIF

C If MOD has been passed as a M then add an item at the end.
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        cVERT(M+2)='+ add/delete       '
        cVERT(M+3)=' '
      ELSE
        cVERT(M+2)=' '
        cVERT(M+3)=' '
      ENDIF
      if(MMOD.EQ.8)then
        cVERT(M+4)  =' '
      else
        cVERT(M+4)  ='< index select'
      endif
      cVERT(M+4)  =' '
      cVERT(M+5)  ='? help'
      cVERT(M+6)  ='- exit menu'

C Help text for this menu.
      if(MOD.EQ.'C')then  
        helptopic='plant_cmp_lst'
        call gethelptext(helpinsub,helptopic,nbhelp)
      elseif(MOD.EQ.'Z')then
        helptopic='plant_zone_link'
        call gethelptext(helpinsub,helptopic,nbhelp)
      elseif(MOD.EQ.'A')then
        helptopic='plant_air_link'
        call gethelptext(helpinsub,helptopic,nbhelp)
      elseif(MOD.EQ.'K')then
        helptopic='plant_known_link'
        call gethelptext(helpinsub,helptopic,nbhelp)
      else
        helptopic='plant_cmp_list'
        call gethelptext(helpinsub,helptopic,nbhelp)
      endif

C Display the menu.
      CALL EMENU(head,cVERT,MVERT,IVERT)
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
  96    CALL EASKI(IV,' ',' Index of component?',
     &         1,'F',NPCOMP,'F',1,'script comp',IERI,nbhelp)
        if(ieri.eq.-1)then
          is=0
          return
        elseif(ieri.gt.0) then
          goto 96
        else
          IS=IV
          RETURN
        endif
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Allow a component to be added, checked for uniqueness and returned.
C Code to allow deletion commented out until support code in place.
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          IOPT=-1
          helptopic='plant_cmp_list'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC(' ','Options:','add',
     &         'delete','cancel',IOPT,nbhelp)
          IF(IOPT.EQ.1) THEN
            if(NPCOMP+1.LE.MPCOM)then
C Needs to be if checked
              CALL PCDEFN
              ILEN=NPCOMP
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              GOTO 3
            else
              call usrmsg(' ','Component list full!','W') 
              IS=0
              GOTO 3
            endif
          ELSEIF(IOPT.EQ.2) THEN
             call usrmsg('Delete which component?',' ','-') 
             CALL EMENU(head,cVERT,MVERT,IDVERT)
             CALL KEYIND(MVERT,IDVERT,IFOC,IO)
             call usrmsg(' ',' ','-') 
             IS=IFOC
             IF(IS.GT.0.AND.IS.LE.NPCOMP) CALL DELCOMP(IS)
             ILEN=NPCOMP
             IPACT=CREATE
             CALL EKPAGE(IPACT)
          ELSE
             CALL EDISP(IUOUT,'Copying components not available!')
          ENDIF
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
        IS=IFOC
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          CALL EASKOK(' ','Edit item?',OK,nbhep)
          IF(OK) CALL EDCOMP(IS)
          ILEN=NPCOMP
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

C end of icdbf check!
      END

C ********** ASKPCON **********
C Presents a list of plant connections returning the index IS.
C If MOD = 'M' then include option to add an item when the appropriate
C interaction is begun.

      SUBROUTINE ASKPCON(head,MOD,IS,IER)

#include "epara.h"
#include "plant.h"
#include "building.h"
#include "geometry.h"
#include "help.h"

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD) 
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PCNAM/PCNAME(MPCOM)
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      DIMENSION VERT(35),CSTR(4)
      CHARACTER PCNAME*15,CSTR*9
      character*(*) head
      character sendstr*20,sendnd*12,recvnd*12 
      CHARACTER VERT*88,KEY*1,MOD*1
      LOGICAL SELECT,OK
      integer MVERT,IVERT,IDVERT ! max items and current menu item

      helpinsub='pltnet'     ! set for subroutine

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
      ILEN=NPCON
      IPACT=CREATE
      CALL EKPAGE(IPACT)
   3  IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=6

C Initial menu entry setup.

   92 IER=0
      IVERT=-3

C Fill connectio type strings
      CSTR(1)='fixed val'
      CSTR(2)='to compt '  
      CSTR(3)='zone/amb '  
      CSTR(4)='amb air  '
    
C Write the first menu item
      WRITE(VERT(1)(1:43),'(A)')
     &'Sending comp./node -> receiving comp./node '
      WRITE(VERT(1)(44:66),'(A)')'| con. type | mass div.'

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          ISVT=isv(IPC1(L),IPN1(L))
          IF(IPCT(L).EQ.2) THEN
          WRITE(SENDSTR,'(A15)')PCNAME(IPC2(L))
            if(isvt.eq.0.or.isvt.eq.10.or.isvt.eq.20) then
              WRITE(SENDND,'(A,I2)')'water node',IPN2(L)
            elseif(isvt.eq.1.or.isvt.eq.11.or.isvt.eq.21) then
              WRITE(SENDND,'(A,I2)')'air node  ',IPN2(L)
            elseif(isvt.eq.9.or.isvt.eq.19.or.isvt.eq.29) then
              WRITE(SENDND,'(A,I2)')'solid node',IPN2(L)
            endif            

          ELSEIF(IPCT(L).EQ.1) THEN 
            if(isvt.eq.0.or.isvt.eq.10.or.isvt.eq.20) then
              WRITE(SENDND,'(A,I2)')'water node',IPN2(L)
              WRITE(SENDSTR,'(A)')  'fixed temp' 
            elseif(isvt.eq.1.or.isvt.eq.11.or.isvt.eq.21) then
              WRITE(SENDND,'(A,I2)')'air node',IPN2(L)
              WRITE(SENDSTR,'(A)')  'fix temp/RH' 
            elseif(isvt.eq.9.or.isvt.eq.19.or.isvt.eq.29) then
              WRITE(SENDND,'(A,I2)')'solid node',IPN2(L)
              WRITE(SENDSTR,'(A)')  'fixed temp' 
            endif            
          ELSEIF(IPCT(L).EQ.3) THEN
            IF(PCONSD(L,1).GT.0.) THEN
              ICZ=NINT(PCONSD(L,1))
              WRITE(SENDSTR,'(A)')ZNAME(ICZ)(1:12)
              WRITE(SENDND,'(A)')'zone air'
            ELSE
              WRITE(SENDSTR,'(A)')'outside air  '
              WRITE(SENDND,'(A)')'ambient '
            ENDIF
          ENDIF

          if(isvt.eq.0.or.isvt.eq.10.or.isvt.eq.20) then
            WRITE(RECVND,'(A,I2)')'water node',IPN1(L)
          elseif(isvt.eq.1.or.isvt.eq.11.or.isvt.eq.21) then
            WRITE(RECVND,'(A,I2)')'air node',IPN1(L)
          elseif(isvt.eq.9.or.isvt.eq.19.or.isvt.eq.29) then
            WRITE(RECVND,'(A,I2)')'solid node',IPN1(L)
          endif
          WRITE(VERT(M),
     &'(A1,1X,A15,1X,A12,A8,A15,1X,A12,2X,A9,3X,F5.3)') 
     &KEY,SENDSTR(1:15),SENDND,'   -->  ',PCNAME(IPC1(L)),RECVND,
     &CSTR(IPCT(L)),PCONDR(L)
        ENDIF          
 10   CONTINUE

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
        VERT(M+2)='+ add/delete       '
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
      VERT(M+4)  =' '
      VERT(M+5)  ='? help'
      VERT(M+6)  ='- exit menu'

C Help text for this menu.
      helptopic='component_connections'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
  96    CALL EASKI(IV,' ','Index of component?',
     &         1,'F',NPCOMP,'F',1,'script comp',IERI,nbhelp)
        if(ieri.eq.-1)then
          is=0
          return
        elseif(ieri.gt.0) then
          goto 96
        else
          IS=IV
          RETURN
        endif
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Allow a component to be added, checked for uniqueness and returned.
C Code to allow deletion commented out until support code in place.
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          helptopic='plant_conn_add'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC(' ','Options:','add ','delete',
     &         'cancel',IOPT,nbhelp)
          IF(IOPT.EQ.1) THEN
            if(NPCON+1.LE.MPCON)then 
              CALL CONDEF('-',MPCON)
              ILEN=NPCON
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              IS=-1
              GOTO 3
            else
              call usrmsg(' ','Component list full!','W') 
              IS=0
              GOTO 3
            endif
          ELSEIF(IOPT.EQ.2)THEN
            call usrmsg(' ','Delete which connection?','-') 
            CALL EMENU(HEAD,VERT,MVERT,IDVERT)
            CALL KEYIND(MVERT,IDVERT,IFOC,IO)
            ID=IFOC
            call usrmsg(' ',' ','-')
            CALL CONDEL(ID)
            ILEN=NPCON
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            GOTO 3
          ELSE
            GOTO 3
          ENDIF
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

C Decode from the potential long list to the comp. no. via KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IS=IFOC
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          helptopic='plant_conn_ed'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ','Edit connection?',OK,nbhelp)     

C Edit the selected connection.
          IF(OK) CALL CONDEF('E',IS)
          ILEN=NPCON
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          GOTO 3
        ELSE
          RETURN
        ENDIF
      ELSE
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

C ********** ASKCONT **********
C Presents a list of plant containments returning the index IS.
C If MOD = 'M' then include option to add an item then the appropriate
C interaction is begun.

      SUBROUTINE ASKCONT(head,MOD,IS,IER)

#include "epara.h"
#include "plant.h"
#include "building.h"
#include "geometry.h"
#include "help.h"

      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)  
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PCNAM/PCNAME(MPCOM)
      DIMENSION VERT(35)
      CHARACTER PCNAME*15
      character*(*) head
      CHARACTER VERT*44,KEY*1,MOD*1,CNTSTR*20
      LOGICAL SELECT,OK
      integer MVERT,IVERT ! max items and current menu item

      helpinsub='pltnet'     ! set for subroutine

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
      ILEN=NCONT
      IPACT=CREATE
      CALL EKPAGE(IPACT)
  92  IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=6
      ILEN=NCONT
      
C Initial menu entry setup.

      IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      WRITE(VERT(1),'(A)')
     &'  Component      | Containment descr. | Type'
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          IF(INDCP(L).EQ.0) THEN
            WRITE(CNTSTR,'(A)') 'outside air'
          ELSEIF(INDCP(L).EQ.1)THEN
            WRITE(CNTSTR,'(A,A13)') 'compt: ',
     &PCNAME(NINT(CNTDAT(L,1)))(1:13)
          ELSEIF(INDCP(L).EQ.2)THEN
            WRITE(CNTSTR,'(A,F7.3)') 'fix temp: ',CNTDAT(L,1)
          ELSEIF(INDCP(L).EQ.3)THEN
            WRITE(CNTSTR,'(A,A)') 'zone: ',ZNAME(NINT(CNTDAT(L,1)))
          ELSEIF(INDCP(L).EQ.4)THEN
            WRITE(CNTSTR,'(A)') 'Ground Temp'
          ENDIF
          WRITE(VERT(M),'(A1,1X,A15,1X,A20,1X,I3)')KEY,PCNAME(IPCC(L)),
     &CNTSTR,INDCP(L)
        ENDIF
   10 CONTINUE

C Number of items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________ '
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' :',MPM 
      ENDIF

C If MOD has been passed as a M then add an item at the end.
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERT(M+2)='+ Add (see help for delete) '
        VERT(M+3)=' '
      ELSE
        VERT(M+2)='                   '
        VERT(M+3)='                   '
      ENDIF
      if(MMOD.EQ.8)then
        VERT(M+4)  ='                   '
      else
        VERT(M+4)  ='< index select     '
      endif
      VERT(M+4)  ='              '
      VERT(M+5)  ='? help             '
      VERT(M+6)  ='- exit             '

C Help text for this menu.
      helptopic='list_plant_containments'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
        CALL PHELPD('node connection selection',nbhelp,'-',0,0,IER)  
        GOTO 92

      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Script directed input if not in graphic mode.
        if(MMOD.EQ.8)goto 3
        IV=1
  96    CALL EASKI(IV,' ','Index of component?',
     &         1,'F',NCONT,'F',1,'script comp',IERI,nbhelp)
        if(ieri.eq.-1)then
          is=0
          return
        elseif(ieri.gt.0) then
          goto 96
        else
          IS=IV
          RETURN
        endif
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Allow a component to be added, checked for uniqueness and returned.
C Code to allow deletion commented out until support code in place.
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          helptopic='plant_cont_add'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKAB(' ','Containment options:','add ',
     &      'cancel',IOPT,nbhelp)
          IF(IOPT.EQ.1) THEN
            if(NCONT+1.LE.MPCOM)then
              CALL CNTDEF('-',MPCOM)
              IS=-1
              ILEN=NCONT
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              GOTO 92
            else
              call usrmsg(' ',' Containment list full!','W') 
              IS=0
              RETURN
            endif
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
          helptopic='plant_cont_ed'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ','Edit containment',OK,nbhelp)
          IF(OK) CALL CNTDEF('E',IS)
          ILEN=NCONT
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

C ********** LISTPNF **********
C Displays a brief description of the current plant
C network file. The information shown consists of the plant
C components currently in the network as well as details of 
C connections and containments.

       SUBROUTINE LISTPNF

#include "plant.h"
#include "esprdbfile.h"

C esprdbfile.h supplies the following:
C LPCDB/IPCDB (for plant template database)
      
      integer lnblnk  ! function definition

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)

      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PCNAM/PCNAME(MPCOM)

      character OUTS*72,OUTSTR*124,outs248*248
      CHARACTER PCNAME*15,pcdesc*80

C Write out general information pertaining to plant database and the 
C number of components within the network.
      CALL EDISP(iuout,' ')
      WRITE(outs248,'(a,i3,2a)') ' The plant network contains',NPCOMP,
     &  ' components from ',LPCDB(1:LNBLNK(LPCDB))
      CALL EDISP248(iuout,outs248,100)

C Loop to write out the component specific information.
      WRITE(OUTS,'(A)') 'Plant components in network: '
      CALL EDISP(iuout,OUTSTR)
      DO 100 I=1,NPCOMP
        IPCOMP=I
        CALL EDISP(iuout,' ')
        WRITE(OUTS,'(3a,i2,a,i2)')' Component: ',
     &    pcname(ipcomp),' (',IPCOMP,') db reference ',NPREF(IPCOMP)
        CALL EDISP(iuout,OUTS)
 100  CONTINUE

C  Connection details, including the supplimentary data.
      CALL EDISP(iuout,' ')
      WRITE(OUTS,'(A,I2)')' No of plant component connect: ',NPCON
      CALL EDISP(iuout,OUTS)

      DO 200 J=1,NPCON
        IPCON=J
        CALL EDISP(iuout,' ')
        WRITE(OUTS,7004)IPCON,IPC1(IPCON),IPN1(IPCON),IPCT(IPCON)
 7004   FORMAT(' Connection:',I2,' comp.',I2,' node',I2,' type',I2)
        CALL EDISP(iuout,OUTS)
        WRITE(OUTS,7005)IPC2(IPCON),IPN2(IPCON),PCONDR(IPCON),
     &                      PCONSD(IPCON,1),PCONSD(IPCON,2)
 7005   FORMAT(' supp. data :',I3,I3,3F8.2)
        CALL EDISP(iuout,OUTS)
 200  CONTINUE


C  Reporting on containment details including supplimentary data.
      CALL EDISP(iuout,' ')
      WRITE(OUTS,'(A,I2)')' No of component containments = ',NCONT
      CALL EDISP(iuout,OUTS)

      DO 300 K=1,NCONT
        ICONT=K  
        CALL EDISP(iuout,' ')
        WRITE(OUTS,7041)ICONT,IPCC(ICONT),INDCP(ICONT)
 7041   FORMAT('Containment',I2,', component',I2,' type',I2)
        CALL EDISP(iuout,OUTS)
        WRITE(OUTS,7042)(CNTDAT(ICONT,J),J=1,3)
 7042   FORMAT('Suppl. data: ',3F8.2)
        CALL EDISP(iuout,OUTS)
 300  CONTINUE

C Report on the plant - mass flow connection mapping (if any exists).
      CALL EDISP(iuout,' ')
      WRITE(OUTS,'(A)')
     &    'Plant inter-connection/fluid flow connection mapping:'
      CALL EDISP(iuout,OUTS)
      WRITE(outs,'(15I5)') (ICFFS(J),J=1,NPCON)
      CALL EDISP(iuout,OUTS)
      END

C ********** opnpdb **********
C Opens plant component database and recovers components record pointers.
C    mode = 1 : Read database file name and read data pointers.
C    mode = 2 : Read data pointers only.
C **CDB affected

      subroutine opnpdb(mode)

#include "plant.h"
#include "esprdbfile.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      PARAMETER (IRECLN = 20)
      PARAMETER (MHEADR = 25)

      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Common for screen control.
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

C Common for new format component database.
      COMMON/NCDBFORM/ICDBF      
      LOGICAL OK,unixok
      CHARACTER*25 PZDESC
      character buffer*72,ltmp*72,fs*1
      integer lndbp   ! for length of standard database path
      character lltmp*144  ! to re-create plant database file name with path
      CHARACTER scantype*8
c      character domain_s*16,category_s*16

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer icdbf   !0 if old format, 1 if new format.
      integer ISTRW

      helpinsub='opnpdb'     ! set for subroutine

C Assume old format initially.
      icdbf=0

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      
C      if(mode.eq.1) then
C Initialize plant component database file.

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  100 helptopic='plant_database'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Deal with local or standard db location when making up the selection string.
      lndbp=lnblnk(standarddbpath)
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        lltmp=LPCDB
      elseif(ipathpcdb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    lpcdb(1:lnblnk(lpcdb))
      endif
      llt=lnblnk(lltmp)
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        if(llt.lt.96)then
          ISTRW=96
        elseif(llt.ge.96.and.llt.lt.124)then
          ISTRW=124
        elseif(llt.ge.124.and.llt.le.144)then
          ISTRW=144
        endif
      elseif(iglib.eq.2)then
        ISTRW=144
      else
        ISTRW=96
      endif
      CALL EASKF(lltmp,' ','Plant component database?',
     &  ISTRW,DPCDB,'Database name',IER,nbhelp)
      
C Figure out local or standard database folder.
      call findwhichdbpath('pdb',lltmp,ier)
      lndbp=lnblnk(standarddbpath)

      if(mode.eq.1.or.mode.eq.2) then

C 'irecln' is defined in 'plantdf.h'. If plant template file unit has
C not been set use the unit from the event profiles database. Use
C same opening logic as in eddb.F and moplnt.F
         if(ipcdb.eq.0)ipcdb=iprodb
         if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
           lltmp=LPCDB
         elseif(ipathpcdb.eq.2)then
           write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &       lpcdb(1:lnblnk(lpcdb))  ! prepend db folder path
         endif
         istat=0
         call EFOPRAN(ipcdb,lltmp,irecln,1,istat)

C Check for error on opening.
         if(istat.lt.0) then
C Attempt to open the file as a new format cdb (ASCII) file. 
            IERcdb=0
            scantype='open'
            CALL CDBScanDataBase(ipcdb,lltmp,scantype,IERcdb)
            IF(IERcdb.gt.0) THEN
              CALL EASKOK('Error opening file!',
     &                    'Retry?',OK,nbhelp)
              if(ok) goto 100
            ELSE

C Set the components database flag to active.
             icdbf=1
             RETURN
            ENDIF
         elseif(istat.eq.1) then

C File doesn't exist.
            ok=.false.
            CALL EASKOK('Plant database not found!',
     &                  'Retry?',OK,nbhelp)
            CALL USRMSG(' ',' ','-')
            if(ok) goto 100
         endif

C Old file, so check header.
         IREC = 1
         READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=99999) PZDESC
         IF (PZDESC .NE. ' PLANT COMPONENT DATABASE') then
C Attempt to open the file as a new format cdb (ASCII) file. 
            IERcdb=0
            scantype='open'
            CALL CDBScanDataBase(ipcdb,lltmp,scantype,IERcdb)
            IF(IERcdb.gt.0) THEN
              ok=.false.
              CALL EASKOK('Not a plant database!',
     &                    'Respecify?',OK,nbhelp)
              CALL USRMSG(' ',' ','-')
              if(ok) goto 100
            ELSE

C Set the components database flag to active.
             icdbf=1
             RETURN
            ENDIF
         endif
         IREC = 2
         READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=99999) npcdb
         write(buffer,72) npcdb
  72     format('Plant database contains',i5,' components.')
         call edisp(iuout,buffer)

C Recover item pointers.
         NR = 1
         DO 110 I = 3, MHEADR
            NRR = NR + IRECLN - 1
            IREC = I
            READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=99999)
     &                          (ITMLOC(J,1),J=NR,NRR)
            NR = NR + IRECLN
  110    CONTINUE
      endif
      return

C Error trap routine.
99999 call usrmsg(' ','Error reading plant database!','W')
      goto 100

      end

C **********  rcdata **********
C Reads data associated with component
C ix. Data is read from component database.
C  mode = 1 : read component parameters from database
C  mode = 2 : skip reading component parameters
C **CDB affected

      subroutine rcdata(ipc,mode)

#include "plant.h"
#include "esprdbfile.h"
#include "gencompDB.h"
#include "gencompDB_common.h"

      PARAMETER (IRECLN = 20)      

      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2) 
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C9plus/UCdbNam(MPCOM),CdbCat(MPCOM),CDBDesc(MPCOM)
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      common/pcsort/icode(maxpc)
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      common/trnsys/ittype(mpcom), loctrs(mpcom), idbct(mpcom), ntypes
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Common for new format component database.
      COMMON/NCDBFORM/ICDBF

      character*68 mscdsc, cvrdsc      
      dimension ival(mpcom)
      character zcdate*16,pcdesc*80,level*16,UCdbNam*16,GetWhich*24,
     &GetWhat*24,CDBCAT*32,CDBDesc*72
      character domain_s*16,category_s*16
      integer mode

      if(icdbf.eq.1)then

C Read information from new format database.
       if(mode.eq.1) then
         level='detailed'
       else 
         level='light'
       endif

       GetWhat='parameters'
       GetWhich=UCdbNam(IPC)(1:lnblnk(UCdbNam(IPC)))
       if(GetWhich.NE.' ')THEN

C Get the item parameter data.
         domain_s='-'; category_s='-'
         CALL CDBGetItemData(domain_s,category_s,GetWhich,GetWhat)

C Fill plant commons with recovered data. 
         CALL CDB2Plant(IPC,level)
         ICODE=s_ICODE
         nnodes(ipc)=s_NNODE
         NMATX=s_NMATX
         npi(ipc)=s_MISCD
         NMISc=s_MISCD
       else
         CALL USRMSG('RCDATA: error trying to recover data',
     &'component name appears to be blank','W')
       endif

      else

C Find absolute record in dbase for component
C and read associated data.
      irec=itmloc(npref(ipc),1)

C Check that plant database is actually open and that itmloc is not zero.
      if(irec.eq.0)then

C Open database and extract the plant database pointers.
        call opnpdb(2) 
        irec=itmloc(npref(ipc),1)
      endif

      read(ipcdb,rec=irec,iostat=istat,err=99999)
     &    idum1,icode(ipc),zcdate,nnode,nmatx,nmisc
      npi(ipc)=nmisc
      nnodes(ipc)=nnode
      irec=irec+1
      read(ipcdb,rec=irec,iostat=istat,err=99999) ictype
      idbct(ipc)=ictype
      if(ictype.eq.0) then
         read(ipcdb,rec=irec,iostat=istat,err=99999)
     &      ictype,mncomp,mncon,nadata,nbdata,ncdata,napout
         nci(ipc)=ncdata

C Read number of external connections per node
         irec=irec+2
         read(ipcdb,rec=irec,iostat=istat,err=99999)
     &     (ndcon(ipc,jj),jj=1,nnode)

C Read state variable index.
         irec=irec+1
         read(ipcdb,rec=irec,iostat=istat,err=99999)
     &     (isv(ipc,jj),jj=1,nnode)

C Point to miscellaneous data record.
         irec= itmloc(npref(ipc),1)+6+(nmatx-1)/irecln
         if(nmisc.gt.0) then

C Start reading misc data items description and value.
            do 100 i=1, nmisc
               read(ipcdb,rec=irec,iostat=istat,err=99999)
     &           mscdsc(i), parval
               if(mode.eq.1) tadata(ipc,i)=parval
               irec=irec+1
  100       continue
         endif

C If any control data exists for this component
C read its description.
         if(ncdata.gt.0) then
            do 200 i=1, ncdata
               read(ipcdb,rec=irec,iostat=istat,err=99999)
     &           cvrdsc(i)
               irec=irec+1
  200       continue
         endif
      elseif(ictype.eq.2) then
         read (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=99999)
     &          ictype,ntypes

C Read number of external connections per node
         irec=irec+2
         read(ipcdb,rec=irec,iostat=istat,err=99999)
     &     (ndcon(ipc,jj),jj=1,nnode)

C Read state variable index.
         irec=irec+1
         read(ipcdb,rec=irec,iostat=istat,err=99999)
     &     (isv(ipc,jj),jj=1,nnode)

C *** CDB affected
C Point to TRNSYS model type entries
         irec=irec+1
         irec= irec+((nmatx-1)/irecln+1)
         read (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=99999)
     &           (ittype(ityp),ityp=1, ntypes)
         do 1000 itp=1,ntypes
            irec=irec+1
            loctrs(itp)=irec
            read(ipcdb,rec=irec,iostat=istat,err=99999)
     &          ntadat,ntbdat,ntcdat,ntmisc,ntapot
            irec=irec+ntmisc+ntcdat+ntapot+1
            read(ipcdb,rec=irec,iostat=istat,err=99999)
     &          idum1, idum2, ispmfc
            irec=irec+ispmfc
 1000    continue

C Let user select required type.
         ival(1)=1
         if(ntypes.gt.1) call gettrs(ix,ival)
         jtype=ival(1)

C establish location of this type and
C read data
         irec=loctrs(jtype)
         read(ipcdb,rec=irec,iostat=istat,err=99999)
     &       nadata,nbdata,ncdata,nmisc,ntapot        
         irec=irec+1
         nci(ipc)=ncdata
         npi(ipc)=nmisc
         if(nmisc.gt.0) then

C Start reading misc data items description and value.
            do 1010 i=1, nmisc
               read(ipcdb,rec=irec,iostat=istat,err=99999)
     &           mscdsc(i), parval
               if(mode.eq.1) tadata(ipc,i)=parval
               irec=irec+1
 1010       continue
         endif

C If any control data exists for this component
C read its description.
         if(ncdata.gt.0) then
            do 1020 i=1, ncdata
               read(ipcdb,rec=irec,iostat=istat,err=99999)
     &           cvrdsc(i)
               irec=irec+1
 1020       continue
         endif
      endif

      endif !end of cdb check

      return

C Error trap.
99999 call edisp(iuout,'Error reading plant data base.')
      return
      end

C ********** update **********
C Update descriptions for selected components.
C **CDB affected
      subroutine update

#include "plant.h"
#include "esprdbfile.h"

      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Common for new format component database
      COMMON/NCDBFORM/ICDBF

      CHARACTER pcdesc*80, cdesc*80

      IF(ICDBF.GT.0)RETURN

      do 10 ipc=1,npcomp
         IREC = ITMLOC(npref(ipc),1)+2
         read(ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=99999) cdesc
         WRITE(pcdesc,14) cdesc
   14    FORMAT(A80)
   10 CONTINUE
      return

99999 call edisp(iuout,'update: Error reading plant data base.')
      return
      end

C ********** descpc **********
C Fill array 'pcdesc' with description of each component
C in database.

      subroutine descpc

#include "plant.h"
#include "esprdbfile.h"

      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      common/pcsort/icode(maxpc)

C Common for new format component database
      COMMON/NCDBFORM/ICDBF

      character pcdesc*80,cdesc*80

C Write description of each component in string array.

      IF(ICDBF.GT.0)RETURN

C ***CDB affected.
      DO 10 L=1,npcdb
      irec=itmloc(L,1)
      read(ipcdb,rec=irec,iostat=istat,err=99999)
     &    idum1,icd,zcdate,nnode,nmatx,nmisc
         icode(L)=icd
         IREC = ITMLOC(l,1)+2
         read(ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=99999) cdesc
         WRITE(pcdesc(l),'(a80)') cdesc
   10 CONTINUE
      return

99999 call edisp(iuout,'descpc: Error reading plant data base.')
      return
      end

C ********** getmfs **********
C Reads mass flow component data from plant component database.
C **CDB affected

      subroutine getmfc(ipc,nconns)

#include "plant.h"
#include "esprdbfile.h"

      PARAMETER (IRECLN = 20)
 
      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      common/mfcpar/itypmf(mpcon), isdcmf(mpcon), icnnmf(mpcon),
     &              supcmf(mpcon,17), ltpcmf(mpcon), isdimf(mpcon)
      common/mfddsc/mfdatstr(mpcon,17)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER  ltpcmf*60,mfdatstr*40,tmpstr*68,pcdesc*80,zcdate*16 

      irec=itmloc(npref(ipc),1)
      read(ipcdb,rec=irec,iostat=istat,err=99999)
     &    idum1,idum2,zcdate,nnode,nmatx,nmisc
      irec=irec+1
      read(ipcdb,rec=irec,iostat=istat,err=99999)
     &    ictype,mncomp,mncon,nadata,nbdata,ncdata,napout

C Point to mass flow components data record.
      irec= itmloc(npref(ipc),1)+6+(nmatx-1)/irecln+nmisc+
     &      napout+ ncdata
      do 10 icon=1, nconns
         read(ipcdb,rec=irec,iostat=istat,err=99999)
     &     itypmf(icon), isdimf(icon), isdcmf(icon), icnnmf(icon),
     &     ltpcmf(icon)
         irec=irec+1
         do 20 isup=1, isdcmf(icon)
            read(ipcdb,rec=irec,iostat=istat,err=99999)
     &      tmpstr,supcmf(icon,isup)
            write(mfdatstr(icon,isup),'(a)') tmpstr(1:40)
            irec=irec+1
   20    continue
   10 continue
      return

C Error trap.
99999 call edisp(iuout,'Error reading plant data base.')
      return
      end


C ********** CopyPltNodeCounts **********
C Copies parameters between bps and prj required because
C of a name clash between prj and bps common blocks: prj
C defines the pcdat common as containing the number of nodes
C associated with a plant component, while bps uses the same
C common to store runtime data.

      subroutine CopyPltNodeCounts()
      implicit none

#include "plant.h"

C Misc bps plant data.
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      common/c12ps/npcdat(mpcom,9),ipofs1(mcoefg),ipofs2(mcoefg,mpvar)

C Misc prj plant data.
      common/pcdat/nnodes(mpcom),isv(mpcom,mnodec),
     &               ndcon(mpcom,mnodec)

      integer NPCOMP, NCI, npcdat, ipofs1, ipofs2, iComponent,
     &        nnodes, isv, ndcon

      real cdata 

C Copy nodes from npcdat to nnodes.
      do iComponent = 1, NPCOMP
        nnodes(iComponent) = npcdat(iComponent,8)
      enddo

      return
      end
