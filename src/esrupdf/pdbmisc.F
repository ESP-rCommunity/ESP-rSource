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

C ************* pdbmisc
C This file contains the following subroutine:
C     opnpdb
C     rcdata
C     update

C*********************** opnpdb ************************
C This subroutine opens plant component database and
C recovers components record pointers.
C    mode = 1 : Read database file name and read data pointers.
C    mode = 2 : Read data pointers only.

      subroutine opnpdb(mode)

      INCLUDE 'plant.h'
      INCLUDE 'esprdbfile.h'
C esprdbfile.h supplies the following:
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C default file names for databases

      PARAMETER (IRECLN = 20)
      PARAMETER (MHEADR = 25)
      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

C Common for screen control.
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      LOGICAL OK,dok
      CHARACTER ZDESC*25,h*72,buffer*72,ltmp*72
      
C      if(mode.eq.1) then
C Initialize plant component database file.
         h(1)='The plant component database binary file holds the'
         h(2)='description of available components.'
         ltmp=lpcdb
  100    CALL EASKS(ltmp,
     &     ' Plant component database file name? ',' ',72,DPCDB,
     &     ' Database name ',IER,2)
         if(ltmp(1:2).ne.' ') lpcdb=ltmp
C      endif
      if(mode.eq.1.or.mode.eq.2) then

C 'irecln' is defined in 'plantdf.h'. If plant component database
C unit is not yet defined set it to the event profile db number.
         if(ipcdb.eq.0) ipcdb=iprodb
         CALL FPRAND (ipcdb,istat,irecln,3,lpcdb)

C Check for error on openening.
         if(istat.lt.0) then
            ok=.false.
            dok=.true.
            h(1)='An error was reported while trying to open the'
            h(2)='plant component database file. Check the folder '
            h(3)='and file name and try again. '
            CALL ASKOK('Error occurred on opening file',
     &              ' Try again? ',OK,dok,3)
            if(ok) goto 100
         elseif(istat.eq.1) then

C File doesn't exist.
            ok=.false.
            dok=.true.
            h(1)='An error was reported while trying to open the'
            h(2)='plant component database file. Check the folder '
            h(3)='and file name and try again. '
            CALL ASKOK(' Plant database file does not exist.',
     &                 ' Try again? ',OK,dok,3)
            if(ok) goto 100
         endif

C Old file, so check header.
         IREC = 1
         READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=99999) ZDESC
         IF (ZDESC .NE. ' PLANT COMPONENT DATABASE') then
            ok=.false.
            dok=.true.
            h(1)='An error was reported while trying to open an'
            h(2)='old plant component database file. Check the folder'
            h(3)='and file name and try again. '
            CALL ASKOK(' Not a plant data base file.',
     &                 ' Try another file? ',OK,dok,3)
            if(ok) goto 100
         endif
         IREC = 2
         READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=99999) npcdb
         write(buffer,72) npcdb
  72     format(' Data base contains',i5,' components.')

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
99999 write(buffer,99099)
99099 format('Error reading plant data base.')
      call edisp(iuout,buffer)
      write(buffer,99098)
99098 format('Please try again.')
      call edisp(iuout,buffer)
      goto 100

      end

C*********************** rcdata ************************
C This subroutine reads data associated with component
C ix. Data is read from component database.
C  mode = 1 : read component parameters from database
C  mode = 2 : skip reading component parameters
C
      subroutine rcdata(ipc,mode)

      INCLUDE 'plant.h'

      PARAMETER (IRECLN = 20)      

      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2) 
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      common/pcsort/icode(maxpc)
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      common/trnsys/ittype(mpcom), loctrs(mpcom), idbct(mpcom), ntypes
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/OUTIN/IUOUT,IUIN
      common/funit/iucfg, iupdb

      character*68 mscdsc, cvrdsc      
      dimension ival(mpcom)
      character*16 zcdate
      character*72 buffer
      character pcdesc*80

C Find absolute record in dbase for component
C and read associated data.
      irec=itmloc(npref(ipc),1)
      read(iupdb,rec=irec,iostat=istat,err=99999)
     &    idum1,icode(ipc),zcdate,nnode,nmatx,nmisc
      npi(ipc)=nmisc
      nnodes(ipc)=nnode
      irec=irec+1
      read(iupdb,rec=irec,iostat=istat,err=99999) ictype
      idbct(ipc)=ictype
      if(ictype.eq.0) then
         read(iupdb,rec=irec,iostat=istat,err=99999)
     &      ictype,mncomp,mncon,nadata,nbdata,ncdata,napout
         nci(ipc)=ncdata

C Read number of external connections per node
         irec=irec+2
         read(iupdb,rec=irec,iostat=istat,err=99999)
     &     (ndcon(ipc,jj),jj=1,nnode)

C Read state variable index.
         irec=irec+1
         read(iupdb,rec=irec,iostat=istat,err=99999)
     &     (isv(ipc,jj),jj=1,nnode)

C Point to miscellaneous data record.
         irec= itmloc(npref(ipc),1)+6+(nmatx-1)/irecln
         if(nmisc.gt.0) then

C Start reading misc data items description and value.
            do 100 i=1, nmisc
               read(iupdb,rec=irec,iostat=istat,err=99999)
     &           mscdsc(i), parval
               if(mode.eq.1) tadata(ipc,i)=parval
               irec=irec+1
  100       continue
         endif

C If any control data exists for this component
C read its description.
         if(ncdata.gt.0) then
            do 200 i=1, ncdata
               read(iupdb,rec=irec,iostat=istat,err=99999)
     &           cvrdsc(i)
               irec=irec+1
  200       continue
         endif
      elseif(ictype.eq.2) then
         read (iupdb,REC=IREC,IOSTAT=ISTAT,ERR=99999)
     &          ictype,ntypes

C Read number of external connections per node
         irec=irec+2
         read(iupdb,rec=irec,iostat=istat,err=99999)
     &     (ndcon(ipc,jj),jj=1,nnode)

C Read state variable index.
         irec=irec+1
         read(iupdb,rec=irec,iostat=istat,err=99999)
     &     (isv(ipc,jj),jj=1,nnode)

C Point to TRNSYS model type entries
         irec=irec+1
         irec= irec+((nmatx-1)/irecln+1)
         read (iupdb,REC=IREC,IOSTAT=ISTAT,ERR=99999)
     &           (ittype(ityp),ityp=1, ntypes)
         do 1000 itp=1,ntypes
            irec=irec+1
            loctrs(itp)=irec
            read(iupdb,rec=irec,iostat=istat,err=99999)
     &          ntadat,ntbdat,ntcdat,ntmisc,ntapot
            irec=irec+ntmisc+ntcdat+ntapot+1
            read(iupdb,rec=irec,iostat=istat,err=99999)
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
         read(iupdb,rec=irec,iostat=istat,err=99999)
     &       nadata,nbdata,ncdata,nmisc,ntapot        
         irec=irec+1
         nci(ipc)=ncdata
         npi(ipc)=nmisc
         if(nmisc.gt.0) then

C Start reading misc data items description and value.
            do 1010 i=1, nmisc
               read(iupdb,rec=irec,iostat=istat,err=99999)
     &           mscdsc(i), parval
               if(mode.eq.1) tadata(ipc,i)=parval
               irec=irec+1
 1010       continue
         endif

C If any control data exists for this component
C read its description.
         if(ncdata.gt.0) then
            do 1020 i=1, ncdata
               read(iupdb,rec=irec,iostat=istat,err=99999)
     &           cvrdsc(i)
               irec=irec+1
 1020       continue
         endif
      endif
      return

C Error trap.
99999 write(buffer,99099)
99099 format('Error reading plant data base.')
      call edisp(iuout,buffer)
      return
      end

C*********************** update ************************
C update descriptions for selected components
      subroutine update

      INCLUDE 'plant.h'

      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/OUTIN/IUOUT,IUIN
      common/funit/iucfg, iupdb

      CHARACTER pcdesc*80, cdesc*80
      character*72 buffer

      do 10 ipc=1,npcomp
         IREC = ITMLOC(npref(ipc),1)+2
         read(iupdb,REC=IREC,IOSTAT=ISTAT,ERR=99999) cdesc
         WRITE(pcdesc,14) cdesc
   14    FORMAT(A80)
   10 CONTINUE
      return

99999 write(buffer,99099)
99099 format('update: Error reading plant data base.')
      call edisp(iuout,buffer)
      return
      end

C*********************** descpc ************************
C Fill array 'pcdesc' with description of each component
C in database.
      subroutine descpc

      INCLUDE 'plant.h'

      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)
      COMMON/OUTIN/IUOUT,IUIN
      common/funit/iucfg, iupdb    
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      common/pcsort/icode(maxpc)

      character pcdesc*80
      CHARACTER cdesc*80
      character*72 buffer

C Write description of each component in string array.
      DO 10 L=1,npcdb
      irec=itmloc(L,1)
      read(iupdb,rec=irec,iostat=istat,err=99999)
     &    idum1,icd,zcdate,nnode,nmatx,nmisc
         icode(L)=icd
         IREC = ITMLOC(l,1)+2
         read(iupdb,REC=IREC,IOSTAT=ISTAT,ERR=99999) cdesc
         WRITE(pcdesc(l),'(a80)') cdesc
   10 CONTINUE
      return

99999 write(buffer,99099)
99099 format('descpc: Error reading plant data base.')
      call edisp(iuout,buffer)
      return
      end

C *********************** getmfs ************************
C This subroutine reads mass flow component data 
C from plant component database.
      subroutine getmfc(ipc,nconns)
      INCLUDE 'plant.h'

      PARAMETER (IRECLN = 20)
 
      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      common/mfcpar/itypmf(mpcon), isdcmf(mpcon), icnnmf(mpcon),
     &              supcmf(mpcon,17), ltpcmf(mpcon), isdimf(mpcon)
      common/mfddsc/mfdatstr(mpcon,17)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/OUTIN/IUOUT,IUIN
      common/funit/iucfg, iupdb

      character*16 zcdate
      CHARACTER  buffer*72, ltpcmf*60, mfdatstr*40, tmpstr*68
      character*80 pcdesc

      irec=itmloc(npref(ipc),1)
      read(iupdb,rec=irec,iostat=istat,err=99999)
     &    idum1,idum2,zcdate,nnode,nmatx,nmisc
      irec=irec+1
      read(iupdb,rec=irec,iostat=istat,err=99999)
     &    ictype,mncomp,mncon,nadata,nbdata,ncdata,napout

C Point to mass flow components data record.
      irec= itmloc(npref(ipc),1)+6+(nmatx-1)/irecln+nmisc+
     &      napout+ ncdata
      do 10 icon=1, nconns
         read(iupdb,rec=irec,iostat=istat,err=99999)
     &     itypmf(icon), isdimf(icon), isdcmf(icon), icnnmf(icon),
     &     ltpcmf(icon)
         irec=irec+1
         do 20 isup=1, isdcmf(icon)
            read(iupdb,rec=irec,iostat=istat,err=99999)
     &      tmpstr,supcmf(icon,isup)
            write(mfdatstr(icon,isup),'(a)') tmpstr(1:40)
            irec=irec+1
   20    continue
   10 continue
      return

C Error trap.
99999 write(buffer,99099)
99099 format('Error reading plant data base.')
      call edisp(iuout,buffer)
      return
      end
