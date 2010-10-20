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

C pc_manip hold routines which manipulate entities in
C a plant component database.

C The plant component database has a record width of IRECLN, and
C contains, per record IREC, the following:
C
C Record      Contents
C ===================================================================
C  1          Plant Component Database (with leading space)
C
C  2          Number of components held, next available record
C             (ie current library size)
C
C  3 to       Contains an array defining the start address for each
C   MHEADR    component
C
C  ....then for each component:
C
C  I          Component entry index (used to reference a component
C             from other ESRU/ESP modules), component code (used to
C             call the related coefficient generator), creation
C             date (16 characters or 4 words), number of nodes,
C             number of matrix coefficient entries, number of
C             miscellaneous data items, generic type (40 characters
C             or 10 words)
C
C  I+1        Component description (80 characters or 20 words)
C
C  I+2        Nodal connection potential defining, for each node,
C             the number of allowable external connections
C
C  I+3        Nodal index defining the active energy state variable
C             and the number of phases subjected for mass balance
C             (maximum 2). The coding is identical to ESRUbps's ISV:
C             ISV=0,10,20 node represents water + ....
C             ISV=1,11,21 node represents dry air + ....
C             ISV=9,19,29 node represents some solid material only
C             0 <=ISV<10  for energy balance only
C             10<=ISV<20  for energy + single phase mass balance
C             20<=ISV<30  for energy + two phase mass balances
C
C  I+4 to     Coefficient locations in matrix template
C   I+4+N as
C   required,
C   N=3 max
C
C  I+5+N to   Miscellaneous data describing component
C   I+5+N+M
C   as required,
C   M=3 max
C =====================================================================

C ******** PCLIST *********
C Lists the plant component library in the order selected by the user. 
C Currently available orders are:
C Sequential      : by code number
C Alphabetical    : by plant descriptions
C Defined         : user supplied list
C Unordered:      : as in the library
C
C There is also a
C Summary mode    : outputs entry number,
C                   code & description
C The routine 'PCGET' is called to read
C the data and the routine 'PCWRT' to
C write it to the terminal.
      SUBROUTINE PCLIST
#include "plantdb.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCDATA/ZGTYPE, ZCDATE, ZCDESC
      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)
      common/PCDATC/NPC,ITMLOC(MAXPC,2)
      common/pophelp/h(60)
      COMMON /PL/ INDXPR(MAXPC)


      dimension ival(MAXPC)
      logical ok,dok

      EXTERNAL PCEXCH, COMPDS, COMPNO
      LOGICAL COMPDS, COMPNO
      CHARACTER ZITEM(11)*24
      character LTMP*72,buffer*120,H*72
      CHARACTER ZGTYPE*40,ZCDATE*16,ZCDESC*80,dtdesc*68,adodsc*30
      integer NITEMS,MNUPIK ! max items and current menu item

C Check library is not empty.
      CALL EPAGE
      IF (NPC .EQ. 0) THEN
        call usrmsg(' No components in library',' ','W')
        RETURN
      ENDIF

C Initialize print list.
      DO 100 I = 1, NPC
        INDXPR(I) = I
  100 CONTINUE

C Build menu list options.
        zitem(1) ='a To file              '
        zitem(2) ='  ---------------------'
        zitem(3) ='b In numerical order   '
        zitem(4) ='c In alphabetical order'
        zitem(5) ='d User defined order   '
        zitem(6) ='e Unordered (as in db) '
        zitem(7) ='  ---------------------'
        zitem(8) ='f Summary of entries   '
        zitem(9) ='  ---------------------'
        zitem(10)='? Help                 '
        zitem(11)='- Return to main menu  '

C Display menu and interpret pick.
  110   IUCHAN = IUOUT
  120   NITEMS = 11
        MNUPIK = -5

C Help text for this menu.
        h(1)= 'The plant components database can be listed: '
        h(2)= ' Sequential      : by code number'
        h(3)= ' Alphabetical    : by plant descriptions'
        h(4)= ' Defined         : user supplied list'
        h(5)= ' Unordered:      : as in the library'
        h(6)= ' '
        h(7)= 'To create an ASCII version of the current (binary)'
        h(8)= 'plant component database: '
        h(9)= '  Choose `a To file`, supply a file name then '
        h(10)='  select the unordered listing option.'

C Clear dialogue box.
        call usrmsg(' ',' ','-')
  130   CALL EMENU ('  List options',ZITEM,NITEMS,MNUPIK)

        if(mnupik.eq.1)then

C Output to file.
          CALL EPAGE
          h(1)='The ACII file will hold plant component entries'
          h(2)='when a sorted list option is selected from the menu.'
          ltmp=' '
  210     CALL EASKS(LTMP,' ASCII file name? ',' ',72,' ',
     &      ' ASCII file',IER,2)
          IUCHAN = IFIL+1
          CALL FPOPEN (IUCHAN,ISTAT,4,3,LTMP)
          IF (ISTAT .LT. 0)then
            dok=.false.
            h(1)='It was not possible to write the ASCII file. Please'
            h(2)='check whether you have permission to write in that '
            h(3)='folder and whether you have sufficient disk space.'
            CALL ASKOK(' Unable to open ascii file. ',
     &                 ' Try again?',OK,dok,3)
            IF(OK)GOTO 210
            return
          endif
          IF (ISTAT .EQ. 0) THEN
            dok=.true.
            h(1)='The current contents of the ASCII file will be '
            h(2)='overwittten if you say yes.  '
            CALL ASKOK(' ASCII file exists...F',
     &                 ' Do you want to overwrite it?',OK,dok,2)
            IF(OK)GOTO 250
            GOTO 130
  250       REWIND IUCHAN
          ENDIF

C Write to file
          WRITE (IUCHAN,'(a)')' PLANT COMPONENT DATABASE Listing'
          CALL USRMSG(' ',' Please select listing type from menu','-')
          GOTO 120
        elseif(mnupik.eq.3)then

C Output in numerical order
          CALL EPAGE
          DO 108 I = 1, NPC
            INDXPR(I) = I
  108     CONTINUE
         IF(NPC.GE.100) WRITE(IUOUT,'(a)')' PCLIST: sorting database...'
          CALL PSHELL (NPC,COMPNO,PCEXCH)
          DO 310 I = 1, NPC
            CALL PCGET (INDXPR(I))
            IF (INDXPC .EQ. INDXPR(I)) THEN
              CALL EPAGE
              CALL PCWRT (IUCHAN)
              IF (IUCHAN .EQ. IUOUT)   then
                dok=.true.
                h(1)='A pause to look at the report... '
                CALL ASKOK(' ',' Continue ?',OK,dok,1)
                IF(.NOT.OK) goto 110
              endif
            ENDIF
  310     CONTINUE
          GOTO 110
        elseif(mnupik.eq.4)then

C Output in alphabetical order.
          CALL EPAGE
          DO 107 I = 1, NPC
            INDXPR(I) = I
  107     CONTINUE
          IF(NPC.GE.100) WRITE(IUOUT,*) ' PCLIST: sorting database ...'
          CALL PSHELL (NPC,COMPDS,PCEXCH)
          DO 410 I = 1, NPC
            CALL PCGET (INDXPR(I))
            IF (INDXPC .EQ. INDXPR(I)) THEN
              CALL EPAGE
              CALL PCWRT (IUCHAN)
              IF (IUCHAN .EQ. IUOUT)  then
                dok=.true.
                h(1)='A pause to look at the report... '
                CALL ASKOK(' ',' Continue ?',OK,dok,1)
                IF(.NOT.OK) goto 110
              endif
            ENDIF
  410     CONTINUE
          GOTO 110
        elseif(mnupik.eq.5)then

C Output in defined order.
C Dsiplay components lilst and get selection.
          inpc=npc
          nitems=npc
          call list(inpc,ival,nitems,ix)
          if (ix.gt.0) then
            do 510 i=1,ix
              call pcget(ival(i))
              if (indxpc.gt.0) then
                CALL EPAGE
                call pcwrt(iuchan)
                if(iuchan.eq.iuout) then
                  dok=.true.
                  h(1)='A pause to look at the report... '
                  CALL ASKOK(' ',' Continue ?',OK,dok,1)
                  IF(.NOT.OK) goto 110
                endif
              endif
  510       continue
          endif
          GOTO 110
        elseif(mnupik.eq.6)then

C Output unordered.
          CALL EPAGE
          DO 810 I = 1, NPC
            CALL PCGET (I)
            IF (INDXPC .GT. 0) THEN
              CALL EPAGE
              CALL PCWRT (IUCHAN)
              IF (IUCHAN .EQ. IUOUT)  then
                dok=.true.
                h(1)='A pause to look at the report... '
                CALL ASKOK(' ',' Continue ?',OK,dok,1)
                IF(.NOT.OK) goto 110
              endif
            ENDIF
  810     CONTINUE
          GOTO 110
        elseif(mnupik.eq.8)then

C Output summary.
          CALL EPAGE
          if(iuchan.eq.(ifil+1)) mmod=-1
          call edisp(IUCHAN,' Entry  Model Description')
          DO 910 I = 1,NPC
            CALL PCGET (I)
            IF (INDXPC .EQ. I) THEN
              WRITE (buffer,15) INDXPC,ICODE,ZCDESC(1:LNBLNK(ZCDESC))
   15         FORMAT (I4,' =',I6,' ',A)
              call edisp(iuchan,buffer)
            ENDIF
  910     CONTINUE
          GOTO 110
        elseif(mnupik.eq.10)then
          CALL PHELPD('pdb lists',10,'-',0,0,IER)
        elseif(mnupik.eq.11)then

C Return from list.
          RETURN
        else
          MNUPIK = -1
          GOTO 130
        endif
        MNUPIK = -4
        goto 110

        END

C ******** PCFIND *********
C Given a component code, the library is searched for this
C component and its data is returned.
      SUBROUTINE PCFIND (ICDE)
#include "plantdb.h"
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)
      common/PCDATC/NPC,ITMLOC(MAXPC,2)

      character dtdesc*68,adodsc*30,h*72
      logical ok,dok

      DO 100 I = 1,NPC
        IREC = ITMLOC(I,1)
        IF (IREC .EQ. 0)   GOTO 100
        READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) INDXPC,ICODE
        IF (ICODE .EQ. ICDE) THEN
          CALL PCGET (I)
          GOTO 9000
        ENDIF
  100 CONTINUE
      INDXPC = 0

C Component code not found.
 9000 RETURN

C Error handling.
  1   INDXPC = -1
      dok=.true.
      h(1)= 'Pdb is confused. The component search failed. If you'
      h(2)= 'want to try again say no to this question. '
      CALL ASKOK(' Error in pdb:pcfind component search. ',
     &           ' Exit the application?',OK,dok,2)
      if(OK)then
        CALL EPAGEND
        STOP
      endif
      return
      end

C ******** PCDELT *********
C Deletes a component from the library.
C The user is asked to confirm the entry
C selected before deletion.
      SUBROUTINE PCDELT
#include "plantdb.h"  
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCCTL/MPC, NXTREC
      common/PCDATA/ZGTYPE, ZCDATE, ZCDESC
      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)
      common/PCDATC/NPC,ITMLOC(MAXPC,2)

      CHARACTER ZGTYPE*40,ZCDATE*16,ZCDESC*80,dtdesc*68,adodsc*30
      character h*72,buffer*120

      dimension ival(MAXPC)
      logical ok,dok

C Check if library empty.
      IF (NPC .GT. 0)   GOTO 100
      call usrmsg(' ',' The library is empty','W')
      GOTO 9000

C Dsiplay components lilst and get selection.
  100 inpc=npc
      nitems=npc

C The following statement writes a dummy variable on the last record
C of the binary file so that end-of-file marker is anchored there.
C This was found to be essential on SPARC systems.
C Note that if a new component was inserted then position of
C end-of-file marker will be adjusted accordingly.
      dummy=0.0
      write(ifil,rec=nxtrec,iostat=istat,err=1) dummy
      call list(inpc,ival,nitems,ix)

C Verify selection and get component index
C corresponding to component code.
      if(ix.gt.0) then
        call edisp(iuout,' ')
        call edisp(iuout,' Deleting the following component(s):')
        call edisp(iuout,' ')
        DO 170 I = 1, ix
           CALL PCGET (ival(i))
           IF (INDXPC .EQ. -1)   GOTO 170
C << ncode not defined >>
           ERRFLG = ncode
           IF (INDXPC .EQ. 0)then
             call usrmsg(' Pdb:pcdelt error indxpc=0',' ','W')
             goto 9000
           endif
           call edisp(iuout,' ')
           write(buffer,'(a,a)') ' Generic type : ',zgtype
           call edisp(iuout,buffer)
           write(buffer,'(a,a)') ' Description  : ',zcdesc
           call edisp(iuout,buffer)
  170   CONTINUE

C Prompt user if ok delete component(s).
C If prompt is yes then remove from dbase.
        dok=.false.
        h(1)='Chance to no delete the component. '
        CALL ASKOK(' ',' OK to delete?',OK,dok,1)
        if(.not.ok) goto 100

C First sort index numbers by ascending order.
        if(ix.gt.1) then
          do 300 i=1, (ix-1)
            ilarge=ival(i)
            jn=i
            do 310 j=(i+1),ix
              if(ival(j).gt.ival(i).and.ival(j).gt.ilarge) then
                ilarge=ival(j)
                jn=j
              endif
  310       continue
            ival(jn)=ival(i)
            ival(i) =ilarge
  300     continue
        endif

C Delete componoents from dbase.
        DO 180 I = 1, ix
          CALL PCGET (ival(i))
          IF (INDXPC .EQ. -1)   GOTO 180
C << ncode not defined >>
          ERRFLG = ncode
          if(INDXPC .EQ. 0)then
            call usrmsg(' Pdb:pcdelt error indxpc=0',' ','W')
            goto 9000
          endif
          CALL PCRMV (INDXPC)
  180   continue
        call usrmsg(' ',' Component(s) deleted successfully','-')
      endif

 9000 RETURN
C Error handling.
  1   dok=.true.
      h(1)='Pdb is confused. It could not finish writing the file'
      h(2)='and this is an option to exit the application. '
      CALL ASKOK(' Error in pdb:pcdelt write of last record. ',
     &           ' Exit from ESRUpdb?',OK,dok,2)
      if(OK)then
        CALL EPAGEND
        STOP
      endif
      return
      END

C ******** PCGET ************
C PCGET of ESRUpdb
      SUBROUTINE PCGET (INDEX)
#include "plantdb.h" 
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCDATA/ZGTYPE, ZCDATE, ZCDESC
      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)
      common/PCDATC/NPC,ITMLOC(MAXPC,2)

      common/pcdatd/ictype,mncomp,mncon,micode(maxcmp),icndat(maxcon,5),
     &              rcndat(maxcon,3)

C Mass flow components data
      common/mfcomp/itpcmp(maxcon), isdcmp(maxcon), isdcnn(maxcon),
     &              supcmp(maxcon,maxflw), mfsdsc(maxcon,maxflw),
     &              ltpcmp(maxcon), isdifc(maxcon)

C TRNSYS types data
      common/trnsys/ntypes,ittype(MXTSYS),ntadat(MXTSYS),ntbdat(MXTSYS),
     &              ntcdat(MXTSYS),ntapot(MXTSYS),ntmisc(MXTSYS),
     &              tdtdsc(MXTSYS,maxmsc),tdatms(MXTSYS,maxmsc),
     &              trange(MXTSYS,maxmsc,2),tadods(MXTSYS,mxaout),
     &              ntaotp(MXTSYS,mxaout),ittpcm(MXTSYS,maxcon),
     &              itsdcm(MXTSYS,maxcon),itsdcn(MXTSYS,maxcon),
     &              tsupcm(MXTSYS,maxcon,maxflw),
     &              tmfsds(MXTSYS,maxcon,maxflw),
     &              tltpcm(MXTSYS,maxcon),itsdif(MXTSYS,maxcon)

      CHARACTER ZGTYPE*40,ZCDATE*16,ZCDESC*80,h*72
      character dtdesc*68,mfsdsc*68,tdtdsc*68,tmfsds*68
      character adodsc*30,tadods*30,ltpcmp*60,tltpcm*60
      logical ok,dok

C Get current component description from
C the required position in the library.
      ERRFLG = INDEX
      istat=0
      IF (INDEX .GT. NPC)then
        call usrmsg(' Pdb:pcget error index>NPC',' ','W')
        goto 9000
      endif
      IREC = ITMLOC(INDEX,1)
      IF (IREC .EQ. 0)then
        call usrmsg(' Pdb:pcget error record=0',' ','W')
        goto 9000
      endif
      READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &          INDXPC,ICODE,ZCDATE,NNODE,NMATX,NMISC,ZGTYPE
      irec=irec+1
      read (ifil,REC=IREC,IOSTAT=ISTAT,ERR=1) ictype
      if(ictype.eq.0) 
     &read (ifil,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &          ictype,mncomp,mncon,nadata,nbdata,ncdata,napout
      if(ictype.eq.1) 
     &read (ifil,REC=IREC,IOSTAT=ISTAT,ERR=1)ictype,mncomp,mncon
      if(ictype.eq.2) 
     &read (ifil,REC=IREC,IOSTAT=ISTAT,ERR=1)ictype,ntypes
  
      INDXPC = INDEX
      IF (INDXPC .NE. INDEX)   GOTO 99998
      IREC = IREC+1
      READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) ZCDESC

C Case for single component.
      if(ictype.eq.0) then

C Read external connectivity of each node.
        IREC = IREC+1
        READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) NDCON

C Read state variable index for each node.
        IREC=IREC+1
        READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)ISV
        NR = 1
  100   NRR = NR + IRECLN - 1

C Read position of nonzero coefficients.
        IREC = IREC+1
        READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)(NCPOS(J),J=NR,NRR)
        NR = NR + IRECLN
        IF (NR .LE. NMATX)  GOTO 100

C Read miscellaneous data if any.
        IF (nmisc.gt.0) then
          do 140 i=1, nmisc
            irec=irec+1
            read(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &        dtdesc(i),datams(i),(range(i,l),l=1,2)
  140     continue
        endif

C Read CDATA description.
        if(ncdata.gt.0) then
          do 150 i=1, ncdata
            irec=irec+1
            j=i+nmisc
            read(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) dtdesc(j)
  150     continue
        endif

C Read additional output description and type
        if(napout.gt.0) then
          do 160 i=1, napout
            irec=irec+1
            read(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) adodsc(i),noutyp(i)
  160     continue
        endif

C Read mass flow components data for each connection
        nconns=0
        do 2000 inod=1,nnode
 2000     if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
          if(nconns.gt.0) then
            do 1010 icon=1, nconns
              irec=irec+1
              read(ifil,rec=irec,iostat=istat,err=1)
     &          itpcmp(icon),isdifc(icon),isdcmp(icon),isdcnn(icon),
     &          ltpcmp(icon)
              do 1020 isup=1, isdcmp(icon)
                irec=irec+1
                read(ifil,rec=irec,iostat=istat,err=1)
     &                mfsdsc(icon,isup), supcmp(icon,isup)
 1020         continue
 1010      continue
         endif

C Case for meta component.
      elseif(ictype.eq.1) then
        NR = 1
  120   NRR = NR + IRECLN - 1

C Read code of each component.
        IREC = IREC+1
        read(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)(micode(J),J=NR,NRR)
        NR = NR + IRECLN
        IF (NR .LE. mncomp)   GOTO 120

C Read component inter-connections description.
        do 130 i=1, mncon
          irec=irec+1
          read(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &         (icndat(i,l),l=1,5),(rcndat(i,ll),ll=1,3)
  130   continue

C Case for TRNSYS component.
C Read data ..
      elseif(ictype.eq.2) then
        IREC = IREC+1
        read (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) NDCON
        IREC=IREC+1
        read(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)ISV
        NR = 1
  200   NRR = NR + IRECLN - 1
        IREC = IREC+1
        read (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)(NCPOS(J),J=NR,NRR)
        NR = NR + IRECLN
        IF (NR .LE. NMATX)   GOTO 200

C read TRNSYS type numbers in this record (i.e relative rec no. 7)
        irec=irec+1
        read (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &        (ittype(ityp),ityp=1, ntypes)

C now read data for each type.
        do 3000 ityp=1, ntypes
          irec=irec+1
          read (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &       ntadat(ityp),ntbdat(ityp),ntcdat(ityp),
     &       ntmisc(ityp),ntapot(ityp)

C read miscellaneous data.
          if(ntmisc(ityp).gt.0) then
            do 3140 i=1, ntmisc(ityp)
              irec=irec+1
              read(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &          tdtdsc(ityp,i),tdatms(ityp,i),(trange(ityp,i,l),l=1,2)
 3140       continue
          endif

C read CDATA description.
          if(ntcdat(ityp).gt.0) then
            do 3150 i=1, ntcdat(ityp)
              irec=irec+1
              j=i+ntmisc(ityp)
              read(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)tdtdsc(ityp,j)
 3150       continue
          endif

C read additional output description and type
          if(ntapot(ityp).gt.0) then
            do 3160 i=1, ntapot(ityp)
              irec=irec+1
              read(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &          tadods(ityp,i), ntaotp(ityp,i) 
 3160       continue
          endif

C read mass flow components data for each connection
          nconns=0
          do 4000 inod=1,nnode
 4000       if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
            if(nconns.gt.0) then
              do 3010 icon=1, nconns
                irec=irec+1
                read(ifil,rec=irec,iostat=istat,err=1)
     &              ittpcm(ityp,icon),itsdif(ityp,icon),
     &              itsdcm(ityp,icon),itsdcn(ityp,icon),
     &              tltpcm(ityp,icon)
                do 3020 isup=1, itsdcm(ityp,icon)
                  irec=irec+1
                  read(ifil,rec=irec,iostat=istat,err=1)
     &               tmfsds(ityp,icon,isup),tsupcm(ityp,icon,isup)
 3020           continue
 3010         continue
            endif
 3000     continue
        endif

C Return
 9000 RETURN

C Error handling
99998 INDXPC = -1
      CALL usrmsg(' Pdb:pcget index error',' ','W')
      goto 9000

C Error handling.
  1   dok=.true.
      h(1)='Pdb is confused. It could not finish writing the file'
      h(2)='and this is an option to exit the application. '
      CALL ASKOK(' Error in pdb:pcget reading database. ',
     &           ' Exit from ESRUpdb ?',OK,dok,2)
      if(OK)then
        CALL EPAGEND
        STOP
      endif
      return
      END


C ******** PCPUT ***********
C PCPUT of ESRUpdb
      SUBROUTINE PCPUT (NDX)
#include "plantdb.h" 
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCCTL/MPC, NXTREC
      common/PCDATA/ZGTYPE, ZCDATE, ZCDESC
      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)
      common/PCDATC/NPC,ITMLOC(MAXPC,2)

      common/pcdatd/ictype,mncomp,mncon,micode(maxcmp),icndat(maxcon,5),
     &              rcndat(maxcon,3)

C Mass flow components data
      common/mfcomp/itpcmp(maxcon), isdcmp(maxcon), isdcnn(maxcon),
     &              supcmp(maxcon,maxflw), mfsdsc(maxcon,maxflw),
     &              ltpcmp(maxcon), isdifc(maxcon)

C TRNSYS types data
      common/trnsys/ntypes,ittype(MXTSYS),ntadat(MXTSYS),ntbdat(MXTSYS),
     &              ntcdat(MXTSYS),ntapot(MXTSYS),ntmisc(MXTSYS),
     &              tdtdsc(MXTSYS,maxmsc),tdatms(MXTSYS,maxmsc),
     &              trange(MXTSYS,maxmsc,2),tadods(MXTSYS,mxaout),
     &              ntaotp(MXTSYS,mxaout),ittpcm(MXTSYS,maxcon),
     &              itsdcm(MXTSYS,maxcon),itsdcn(MXTSYS,maxcon),
     &              tsupcm(MXTSYS,maxcon,maxflw),
     &              tmfsds(MXTSYS,maxcon,maxflw),
     &              tltpcm(MXTSYS,maxcon),itsdif(MXTSYS,maxcon)

      CHARACTER ZGTYPE*40,ZCDATE*16,ZCDESC*80
      character dtdesc*68,mfsdsc*68,tdtdsc*68,tmfsds*68
      character adodsc*30,tadods*30,ltpcmp*60,tltpcm*60
      character outs*124,h*72
      logical ok,dok

C Put current component description into
C the required position in the library.
      IF (NDX .GE. MAXPC)then
        call usrmsg('Number of items beyond MAXPC',' ','W')
        return
      endif
      IREC = ITMLOC(NDX,1)

C check if entry is too big for old slot
C or is to be added to the end of library.
C***
C If changing format of component entry,
C fix ISZ to be the entry size in records.
      NNDX = NDX + 1
      if(ictype.eq.0) then

C Calculate number of records required for mass flow components
        nconns=0
        do 2000 inod=1,nnode
          if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
 2000   continue
        nmfrec=0
        do 2010 icon=1, nconns
          nmfrec=nmfrec+isdcmp(icon)
 2010   continue

C Adjust to account for first data record for each flow component.
        nmfrec=nmfrec+1*nconns
        ISZ = 6 + (NMATX-1)/IRECLN + NMISC + ncdata + napout+ nmfrec
      elseif(ictype.eq.1) then
        isz = 5 + (mncomp-1)/irecln + ncdata + nmisc + mncon
      elseif(ictype.eq.2) then
        isz = 7 + (NMATX-1)/IRECLN + ntypes*1
        do 2100 ityp=1, ntypes

C Calculate number of records required for mass flow components.
          nconns=0
          do 2050 inod=1,nnode
            if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
 2050     continue
          nmfrec=0
          do 2060 icon=1, nconns
            nmfrec=nmfrec+itsdcm(ityp,icon)
 2060     continue

C Adjust to account for first data record for each flow
C component.
          nmfrec=nmfrec+1*nconns
          isz = isz + ntmisc(ityp) + ntcdat(ityp) + ntapot(ityp)+nmfrec 
 2100   continue
      endif
      IF ((IREC + ISZ) .gt. ITMLOC(NNDX,1)) THEN

C Remove old entry if too small.
        IF (NDX .LE. NPC)   CALL PCRMV (NDX)

C Make space at end of library.
        NPC = NPC + 1
        ITMLOC(NPC,1) = NXTREC
        ITMLOC(NPC,2) = ICODE
        NXTREC = NXTREC + ISZ
        CALL PCUPDT (1)
        IREC = ITMLOC(NPC,1)
      ENDIF

C Case for single component - write data into item.
      if(ictype.eq.0) then
        nmisc=nadata+nbdata
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &          NDX,ICODE,ZCDATE,NNODE,NMATX,NMISC,ZGTYPE
        irec=irec+1
        if(ictype.eq.0) 
     &      write (ifil,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &          ictype,mncomp,mncon,nadata,nbdata,ncdata,napout
        if(ictype.eq.2) 
     &      write (ifil,REC=IREC,IOSTAT=ISTAT,ERR=1) ictype, ntypes
        IREC = IREC+1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) ZCDESC
        IREC = IREC+1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) NDCON
        IREC=IREC+1
        WRITE(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)ISV
        NR = 1
  100   NRR = NR + IRECLN - 1
        IREC = IREC+1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)(NCPOS(J),J=NR,NRR)
        NR = NR + IRECLN
        IF (NR .LE. NMATX)   GOTO 100

C Write miscellaneous data.
        if(nmisc.gt.0) then
          do 140 i=1, nmisc
            irec=irec+1
            write(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &           dtdesc(i),datams(i),(range(i,l),l=1,2)
  140     continue
        endif

C Write CDATA description.
        if(ncdata.gt.0) then
          do 150 i=1, ncdata
            irec=irec+1
            j=i+nmisc
            write(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) dtdesc(j)
  150     continue
        endif

C Write additional output description and type.
        if(napout.gt.0) then
          do 160 i=1, napout
            irec=irec+1
            write(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) adodsc(i),noutyp(i)
  160     continue
        endif

C Write mass flow components data for each connection.
        if(nconns.gt.0) then
          do 1010 icon=1, nconns
            irec=irec+1
            write(ifil,rec=irec,iostat=istat,err=1)itpcmp(icon),
     &         isdifc(icon),isdcmp(icon),isdcnn(icon),ltpcmp(icon)
            do 1020 isup=1, isdcmp(icon)
               irec=irec+1
               write(ifil,rec=irec,iostat=istat,err=1)
     &           mfsdsc(icon,isup), supcmp(icon,isup)
 1020       continue
 1010     continue
        endif

C Case for meta component.
      elseif(ictype.eq.1) then
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &          NDX,ICODE,ZCDATE,NNODE,NMATX,NMISC,ZGTYPE
        irec=irec+1
        write (ifil,REC=IREC,IOSTAT=ISTAT,ERR=1)ictype,mncomp,mncon
        IREC = IREC+1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) ZCDESC

C First write components code numbers.
        NR = 1
  120   NRR = NR + IRECLN - 1
        IREC = IREC+1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)(micode(J),J=NR,NRR)
        NR = NR + IRECLN
        IF (NR .LE. mncomp)   GOTO 120

C Then write components connections data.
        do 130 i=1, mncon
          irec=irec+1
          write(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &           (icndat(i,l),l=1,5),(rcndat(i,ll),ll=1,3)
  130   continue

      elseif(ictype.eq.2) then

C Case for TRNSYS component, write data into item.
        nmisc=0
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &          NDX,ICODE,ZCDATE,NNODE,NMATX,NMISC,ZGTYPE
        irec=irec+1
        write (ifil,REC=IREC,IOSTAT=ISTAT,ERR=1)ictype, ntypes
        IREC = IREC+1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) ZCDESC
        IREC = IREC+1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) NDCON
        IREC=IREC+1
        WRITE(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)ISV
        NR = 1
  200   NRR = NR + IRECLN - 1
        IREC = IREC+1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)(NCPOS(J),J=NR,NRR)
        NR = NR + IRECLN
        IF (NR .LE. NMATX)   GOTO 200

C Write TRNSYS type numbers in this record (i.e relative rec no. 7).
        irec=irec+1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &           (ittype(ityp),ityp=1, ntypes)

C Now write data for each type.
        do 3000 ityp=1, ntypes
          irec=irec+1
          WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &       ntadat(ityp),ntbdat(ityp),ntcdat(ityp),
     &       ntmisc(ityp),ntapot(ityp)

C Write miscellaneous data.
          if(ntmisc(ityp).gt.0) then
            do 3140 i=1, ntmisc(ityp)
              irec=irec+1
              write(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &            tdtdsc(ityp,i),tdatms(ityp,i),(trange(ityp,i,l),l=1,2)
 3140       continue
          endif

C Write CDATA description.
          if(ntcdat(ityp).gt.0) then
            do 3150 i=1, ntcdat(ityp)
              irec=irec+1
              j=i+ntmisc(ityp)
              write(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)tdtdsc(ityp,j)
 3150       continue
          endif

C Write additional output description and type.
          if(ntapot(ityp).gt.0) then
            do 3160 i=1, ntapot(ityp)
              irec=irec+1
              write(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &              tadods(ityp,i), ntaotp(ityp,i) 
 3160       continue
          endif

C Write mass flow components data for each connection.
          nconns=0
          do 4000 inod=1,nnode
            if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
 4000     continue
          if(nconns.gt.0) then
            do 3010 icon=1, nconns
              irec=irec+1
              write(ifil,rec=irec,iostat=istat,err=1)
     &              ittpcm(ityp,icon),itsdif(ityp,icon),
     &              itsdcm(ityp,icon),itsdcn(ityp,icon),
     &              tltpcm(ityp,icon)
              do 3020 isup=1, itsdcm(ityp,icon)
                irec=irec+1
                write(ifil,rec=irec,iostat=istat,err=1)
     &               tmfsds(ityp,icon,isup),tsupcm(ityp,icon,isup)
 3020         continue
 3010       continue
          endif
 3000   continue
      endif

C Return
      RETURN

C Error handling.
  1   dok=.true.
      h(1)='Pdb is confused. It could not finish writing the file'
      h(2)='and this is an option to exit the application. '
      write(outs,'(a,i4)') 'Error pcput: database writing @ rec: ',irec
      CALL ASKOK(outs,' Exit from ESRUpdb ?',OK,dok,2)
      if(OK)then
        CALL EPAGEND
        stop
      endif
      return
      END

C ********* PCRMV ********
      SUBROUTINE PCRMV (NDX)
#include "plantdb.h"
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCCTL/MPC, NXTREC
      common/PCDATC/NPC,ITMLOC(MAXPC,2)

C Delete entry by compacting library
      INTEGER RECORD(IRECLN)
      logical ok,dok
      character h*72,outs*124

C Copy non null records down
      NNDX = NDX + 1
      IREC1 = ITMLOC(NDX,1)
      IREC2 = 0
      IF (NNDX .LE. MAXPC)   IREC2 = ITMLOC(NNDX,1)
      IF (IREC2 .EQ. 0)   IREC2 = NXTREC

C While irec2 < nxtrec.
  100 IF (IREC2 .GE. NXTREC)   GOTO 200
      READ (IFIL,REC=IREC2,IOSTAT=ISTAT,ERR=1) RECORD
      IREC2 = IREC2 + 1
      WRITE (IFIL,REC=IREC1,IOSTAT=ISTAT,ERR=1) RECORD
      IREC1 = IREC1 + 1
      GOTO 100

C Update header block.
  200 DO 210 I = NNDX, NPC
        LI = I - 1
        ITMLOC(LI,1) = ITMLOC(I,1) - IREC2 + IREC1
        ITMLOC(LI,2) = ITMLOC(I,2)
  210 CONTINUE
      NXTREC = IREC1
      ITMLOC(NPC,1) = 0
      ITMLOC(NPC,2) = 0

C Reset number of pc's
      NPC = NPC - 1

C Rewrite header.
      CALL PCUPDT (1)

C Return
      RETURN

C Error handling
  1   dok=.true.
      h(1)='Pdb is confused. It could not compact the file'
      h(2)='and this is an option to exit the application. '
      write(outs,'(a,i4)') 'Error pcput: database writing @ rec: ',irec
      CALL ASKOK('Error in pdb:pcrmv database compacting. ',
     &  ' Exit from ESRUpdb ?',OK,dok,2)
      if(OK)then
        CALL EPAGEND
        stop
      endif
      return
      END

C ********  PCUPDT **********
C Update header block after ITMLOC has changed. If IFLG = 0, then
C zeroize header information
      SUBROUTINE PCUPDT (IFLG)
#include "plantdb.h"
      common/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCCTL/MPC, NXTREC
      common/PCDATC/NPC,ITMLOC(MAXPC,2)
      CHARACTER PZDESC*25,h*72
      logical ok,dok

C Write header.
      IF (IFLG .EQ. 0) THEN
        PZDESC = ' PLANT COMPONENT DATABASE'
        IREC = 1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) PZDESC

C Zeroize no of items.
        NPC = 0
        NXTREC = MHEADR+1

C Zeroize item pointers
        DO 100 I = 1, MAXPC
          ITMLOC(I,1) = 0
          ITMLOC(I,2) = 0
  100   CONTINUE
      ENDIF

C Update item pointers and count
      IREC = 2
      WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) NPC, NXTREC
      NR = 1
      DO 110 I = 3, MHEADR
        NRR = NR + IRECLN - 1
        IREC = I
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)(ITMLOC(J,1),J=NR,NRR)
        NR = NR + IRECLN
  110 CONTINUE

C Return
      RETURN

C Error handling
  1   dok=.true.
      h(1)='Pdb is confused. It could not update the file'
      h(2)='and this is an option to exit the application. '
      CALL ASKOK('Error in pdb:pcupdt database updating. ',
     &  ' Exit from ESRUpdb ?',OK,dok,2)
      if(OK)then
        CALL EPAGEND
        stop
      endif
      return
      END
