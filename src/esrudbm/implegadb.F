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

C Subroutines to import legacy esp-r databases.
C IMPMLC: Imports from legacy construction file (AFIL) to a generic
C    construction db file (BFIL)
C IMPRIMMAT: Imports from legacy ascii materials file (AFIL) to a generic
C    materials db file (BFIL). 
C IMPRESSC: Imports from legacy pressure coef file (AFIL) to a generic
C    wind pressure db file (BFIL).
C IMPROFL: Imports from legacy ascii profile file (AFIL) to a generic
C    profile db file (BFIL)
C MKAPRC: Creates an ascii materials db based on the
C    current contents of binary file.
C TROPRO creates an ASCII profiles db from the binary profiles db.
C ERPCDB reads one element from a binary materials database
C    returning them via parameter list.
C parsemlcdesc: Parse the DESC string from common block returning text for
C   MLCN (its name) ,OTF (whether OPAQ/TAN) ,OPT (optical name) ,MLCSYM
C   tag for symmetry.

C ************* IMPMLC 
C IMPMLC: Imports from legacy construction file (AFIL) to a generic
C construction db file (BFIL). Unit IFA is the asci file channel and
C IFB as the binary channel. Idbh is position in hash table.
      subroutine impmlc(IFA,AFIL,IFB,BFIL,idbh,IER)
#include "building.h"
#include "esprdbfile.h"
C providing paramaters MGDB,MGCL,MGIT
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendbs/srcchr(24),srcnote(24)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      dimension thrmtg(ME),gaptg(ME),mattag(ME),DT(ME)
      dimension IP(ME),DRA(ME,3),laydesc(ME)
      CHARACTER OUTSTR*124,AFIL*72,BFIL*72,DESC*48,NAM*72,RSTR*124
      CHARACTER WORD*20,MLCN*12,OTF*4,OPT*12,MLCSYM*12
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character itdat*248,itrep*248
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,c8*8,c20*20,outs*124,outsd*124
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248
      character dstmp*24,symname*12,othcolr*12,incolr*12
      character othacou*12,inacou*12,cnstimp*12
      character thrmtg*1,gaptg*1,mattag*12,laydesc*32
      CHARACTER TITL*72,LTMP*72,DFILE*72

C Logical noting whether it was possible to open legacy material
C database to get IR and solar properties.
      logical scanmat
      integer ier

C Initialise variables.
      call clrgdbc(idbh)
      dbtag(idbh)='multicon_db'
      dbmenu(idbh)='Constructions db'
      dbnote(idbh)='Constructions db log...'
      call dstamp(dstmp)
      write(dbdate(idbh),'(a)') dstmp
      call intdbdoc(idbh,'i',afil,ier)
      nbsrc(idbh)=1
      nbunit(idbh)=4
      unitstr(1)='(-)'
      unitnote(1)='(-)'
      unitstr(2)='(km)'
      unitnote(2)='(distance in km)'
      unitstr(3)='(m)'
      unitnote(3)='(thickness in m)'
      unitstr(4)='(index)'
      unitnote(4)='(index)'
      nbcat(idbh)=1
      cattag(1)='convmlc'
      cattype(1)='STD '
      catmenu(1)='Converted construction items'
      call dstamp(dstmp)
      write(catdate(1),'(a)') dstmp
      nbcatdoc(1)=1
      write(catdoc(1),'(2a)')
     &  'Category documentation follows: ',
     &  '(imported from legacy construction db.)'
      iit=0

C The binary file will be overwritten.
      CALL ERPFREE(IFB,IOS)
      idbw=idbwidth(idbh)
      ier=0
      call EFOPRAN(IFB,BFIL,idbw,4,IER)
      if(ier.ne.0)then
        call edisp(iuout,'error opening binary file')
        return
      endif

C Open the ASCII file for reading.
      CALL EFOPSEQ(IFA,AFIL,1,IER)

C Read the number of items in the constructions database, followed by
C the number of layers & construction name.
      CALL STRIPC(IFA,OUTSTR,1,ND,1,'constr db items',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,nbitem(idbh),0,MGIT,'F',
     &              'no of composite constructions',IER)

C Find the legacy materials file used with this legacy construction
C file and open it on unit ifmat. If legacy construction then
C there probably was an earlier scan and conversion of a legacy
C material database so LFMAT should already be instanciated.
C If it was not possible to open the material db then set default
C values for the inside and outside solar and IR.
      LTMP=LFMAT
      DFILE='/usr/esru/esp-r/databases/constr.db2'
  29  CALL EASKS(LTMP,
     &  ' Legacy materials db used with this legacy construction',
     &  ' database?',72,DFILE,'legacy matrl name',IER,1)
      IF(LTMP.EQ.' ')GOTO 29
      IFMAT=IFIL
      call ERPFREE(IFMAT,IOS)
      ier=0
      call EFOPRAN(IFMAT,LTMP,40,1,IER)
      if(ier.ne.0)then
        othir=0.90
        sinir=0.90
        othsol=0.50
        sinsol=0.50
        scanmat=.false.
      else
        scanmat=.true.
      endif

      nbcatit(1)=nbitem(idbh)

C Write out db header and place holders for category start record pointers
C (irscat) and items start record pointers.
      call wgdbhead(IFB,idbh,IER)
      call updirsit(IFB,idbh,'I')
      IREC = 3 + nbunit(idbh) + nbsrc(idbh) + nbdbdoc(idbh) + 2 +12

C Writ category header.
      icl=1
      IREC=IREC+1
      irscat(icl)=IREC
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &  catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)

      IREC=IREC+1
      c8='*CATDOC '
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(1)

C For each item, provide default item data and read existing items.
      DO 10 I=1,nbitem(idbh)
        iit=iit+1
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'constr descr',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,nlay,1,MMLC,'F','layrs',IER)
        nbitdat(iit)=2
        nbitrep(iit)=nlay

C Anything after no of layers is the description (which is overloaded
C to include information on optical properties).
        CALL EGETRM(OUTSTR,K,RSTR,'W','layer description',IER)
        DESC=RSTR(1:LNBLNK(RSTR))

C Parse the DESC string into string tokens.
        call parsemlcdesc(DESC,MLCN,OTF,OPT,MLCSYM)

        symname=MLCSYM
        othcolr='grey50'
        incolr='grey50'
        othacou='none'
        inacou='none'
        cnstimp='none'
        write(itmenu(iit),'(a,20x)') MLCN
        write(itdoc,'(3a)') 'construction ',MLCN(1:lnblnk(MLCN)),
     &    ' is...'
        call dstamp(dstmp)
        write(itdate(iit),'(a)') dstmp

C Match the following 4 lines to instanitm in dbmedit.F
        nbitusc(iit)=23
        ibitusc(iit)=0
        ituntstr='-------cccc------b-----' 
        itsrcstr='-'
        itcnfstr='-----------------------'
        
        DO 15, IL=1,nlay
          CALL STRIPC(IFA,OUTSTR,99,ND,1,'layer',IER)
          IF(IER.NE.0)RETURN
          K=0

C << note this subroutine assumes that the model is associated
C << with a binary version of the materials database. It must
C << be updated to use the material array structures.

C Crate tag for material and reference tag for material db entry.
C If legacy material db was opened then scan for ir and sol values.
          CALL EGETWI(OUTSTR,K,IP(il),0,600,'F','legacy index',IER)
          if(scanmat)then
            itru=0
            if(IL.eq.1)then
              if(IP(IL).eq.0)then
                othir=0.0
                othsol=0.0
              else
                CALL ERPCDB(IFMAT,IP(IL),ITRU,IER,DBCON,DBDEN,
     &            DBSHT,E,A,DRV,TITL,NAM)
                othir=E
                othsol=A
              endif
            elseif(IL.eq.nlay)then
              if(IP(IL).eq.0)then
                othir=0.0
                othsol=0.0
              else
                CALL ERPCDB(IFMAT,IP(IL),ITRU,IER,DBCON,DBDEN,
     &            DBSHT,E,A,DRV,TITL,NAM)
                sinir=E
                sinsol=A
              endif
            endif
          endif
          if(IP(il).le.9)then
            write(mattag(il),'(a,i1)') 'mat',IP(il)
          elseif(IP(il).gt.9.and.IP(il).le.99)then
            write(mattag(il),'(a,i2)') 'mat',IP(il)
          elseif(IP(il).gt.99.and.IP(il).le.999)then
            write(mattag(il),'(a,i3)') 'mat',IP(il)
          endif

C Find matching material tag (sanning items found in material db) and
C remember its name.
          isdbhash=2
          do 40 iln=1,nlay
            laydesc(iln)='not found'
            do 35 itother=1,nbitem(isdbhash)
              if(mattag(iln)(1:12).eq.ittag(itother)(1:12))then
                laydesc(iln)=itmenu(itother)
                goto 36
              endif
  35        continue
  36        continue
  40      continue

          CALL EGETWR(OUTSTR,K,DT(il),0.0001,0.5,'W','thick',IER)

C Current MLDB also include the name of the layer from the materials
C db.  If the layer is air then this name contains the R values for
C air gap.
          CALL EGETRM(OUTSTR,K,RSTR,'W','layer name:gap R',IER)
          NAM=RSTR(1:LNBLNK(RSTR))
          thrmtg(il)='t'
          IF(IP(IL).EQ.0)THEN
            gaptg(il)='g'
            K=0

C Pick up 'AIR' followed by default R values.
            CALL EGETW(NAM,K,WORD,'W','AIR label',IER)
            CALL EGETWR(NAM,K,VAL1,0.,99.0,'W','vert R',IER)
            DRA(IL,1)=VAL1
            CALL EGETWR(NAM,K,VAL2,0.,99.0,'W','floor:ceil R',IER)
            DRA(IL,2)=VAL2
            CALL EGETWR(NAM,K,VAL3,0.,99.0,'W','other R',IER)
            DRA(IL,3)=VAL3
          ELSE
            gaptg(il)='s'
            DRA(IL,1)=0.0
            DRA(IL,2)=0.0
            DRA(IL,3)=0.0
          ENDIF
  15    CONTINUE

C Encode current data into generic db commons.
        call prsmlc(idbh,iit,'p',symname,othcolr,incolr,OTF,OPT,
     &   othacou,inacou,othir,sinir,othsol,sinsol,cnstimp,
     &   nlay,thrmtg,gaptg,mattag,DT,IP,DRA,laydesc,ier)

        IREC=IREC+1
        irsit(iit)=IREC
        itcat(iit)=icl
        if(idbver(idbh).eq.1)then
          nbitdoc=1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &      itdate(iit),itcat(iit),nbitdoc,nbitdat(iit),nbitrep(iit),
     &      nbitusc(iit),ibitusc(iit)
        else
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &      itdate(iit),itcat(iit),nbitdat(iit),nbitrep(iit),
     &      nbitusc(iit),ibitusc(iit)
        endif

C write itdoc into next record.
        c8='*ITDOC  '
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,itdoc

        IREC=IREC+1
        c8='*USC    '
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,ituntstr,itsrcstr,
     &    itcnfstr

C Write mlc data.
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(1)
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(2)
        lastr(idbh)=IREC

        if(nbitrep(iit).gt.0)then
          do 247 ij=1,nbitrep(iit)
            IREC=IREC+1
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itrep(ij)
 247      continue
          lastr(idbh)=IREC
        endif
   10 CONTINUE

C End of db, complete the headder. First update all cat and item pointers.
      IREC = 3 + nbunit(idbh) + nbsrc(idbh) + nbdbdoc(idbh)
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=1,24)
      IREC=IREC+1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=25,MGCL)
      call updirsit(IFB,idbh,'W')
      c20='*ESP-r binary db    '
      irec=1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
      call tagend(IFB,idbh,ier)

C Debug.
C      write(6,*) 'last record in pressc file is ',lastr(idbh),
C     &  lastr(idbh)+1

C Close file before exiting.
      call ERPFREE(IFMAT,IOS)
      CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      call edisp(iuout,' Reached end of legacy file, closing files.')
      RETURN
      
  99  CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      call ERPFREE(IFMAT,IOS)
      RETURN

   1  write(outs,'(a,I6)') 'IMPRESSC: Error writing record ',IREC
      call edisp(iuout,outs)
      ier=1
      goto 99
    
      END

C ************* IMPRIMMAT 
C IMPRIMMAT: Imports from legacy ascii materials file (AFIL) to a generic
C materials db file (BFIL). Unit IFA is the asci file channel and
C IFB as the binary channel. 
      subroutine imprimmat(IFA,AFIL,IFB,BFIL,idbh,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendbs/srcchr(24),srcnote(24)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      CHARACTER IDESC*72,NDESC*72,OUTSTR*124,AFIL*72,BFIL*72
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character itdat*248,itrep*248
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,c8*8,c20*20,outs*124
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248
      character dstmp*24,T12*12,D12*12,lcatype*1
      character acoutag*12,colormtag*12,lcatag*12
      real misc1,misc2
      integer ier

C Initialise variables.
      call clrgdbc(idbh)
      dbtag(idbh)='material'
      dbmenu(idbh)='Materials (list) db'
      write(dbnote(idbh),'(a,a)')dbmenu(idbh)(1:lnblnk(dbmenu(idbh))),
     &  ' log...'
      call dstamp(dstmp)
      write(dbdate(idbh),'(a)') dstmp
      do 7 i=1,12
        unitnote(i)='(-)'
  7   continue
      do 8 i=1,24
        srcnote(i)='Imported from a legacy file.'
  8   continue

C << uncertainty...>>
      nbsrc(idbh)=1
      nbunit(idbh)=4
      unitstr(1)='(-)'
      unitnote(1)='(-)'
      unitstr(2)='W/(m deg.C)'
      unitnote(2)='Conduc W/(m deg.C)'
      unitstr(3)='kg/m**3'
      unitnote(3)='Density kg/m**3'
      unitstr(4)='J/(kg deg.C)'
      unitnote(4)='Specific Heat J/(kg deg.C)'

C Clear database level help and classes.
      call intdbdoc(idbh,'i',afil,ier)
      icl=0
      iit=0

C The binary file will be overwritten.
      CALL ERPFREE(IFB,IOS)
      idbw=idbwidth(idbh)
      ier=0
      call EFOPRAN(IFB,BFIL,idbw,4,IER)
      if(ier.ne.0)then
        call edisp(iuout,'error opening binary file')
        return
      endif

C Open the ASCII file for reading.
      CALL EFOPSEQ(IFA,AFIL,1,IER)

C Proceed.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'no categories',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,nbc,1,1,'-','nb catg',IER)
      nbcat(idbh)=nbc+1

C Write out db header and place holders for category start record pointers
C (irscat) and items start record pointers.
      call wgdbhead(IFB,idbh,IER)
      call updirsit(IFB,idbh,'I')
      IREC = 3 + nbunit(idbh) + nbsrc(idbh) + nbdbdoc(idbh) + 2 +12

C Write initial gasses category
      icl=1
      write(cattag(icl),'(a)') 'Gasses'
      write(catmenu(icl),'(a)') 'Gasses (for air gaps)'
      write(cattype(icl),'(a)') 'GAS     '
      nbcatdoc(icl)=1
      catdoc(1)='Category Gasses: default inclusion in database.'
      call dstamp(dstmp)
      write(catdate(icl),'(a)') dstmp
      nbcatit(icl)=1

      IREC=IREC+1
      irscat(icl)=IREC
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &  catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)
      c8='*CATDOC '
      IREC=IREC+1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(1)

C Write values for classification (see later code for comments).
      iit=iit+1
      NCODE=0
      write(ittag(iit),'(a,i1)') 'mat',NCODE
      write(itmenu(iit),'(a)') 'air (@ 25C)'
      call dstamp(dstmp)
      write(itdate(iit),'(a)') dstmp

C Next 6 lines should match instanitm in dbmedit.F
      nbitusc(iit)=17
      ibitusc(iit)=0
      ituntstr='-------abc-------' 
      itsrcstr='-'
      itcnfstr='-----------------'
      nbitdat(iit)=2
      nbitrep(iit)=0
      write(itdoc,'(a)') 'air (assuming 25C dry)'

      IREC=IREC+1
      irsit(iit)=IREC
      itcat(iit)=icl
      if(idbver(idbh).eq.1)then
        nbitdoc=1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &    itdate(iit),itcat(iit),nbitdoc,nbitdat(iit),nbitrep(iit),
     &    nbitusc(iit),ibitusc(iit)
      else
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &    itdate(iit),itcat(iit),nbitdat(iit),nbitrep(iit),
     &    nbitusc(iit),ibitusc(iit)
      endif
      IREC=IREC+1

C write itdoc into next record.
      c8='*ITDOC  '
      IREC=IREC+1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,itdoc
      IREC=IREC+1
      c8='*USC    '
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,ituntstr,itsrcstr,itcnfstr

      acoutag='none'
      colormtag='none'
      lcatag='none'
      lcatype='-'
      r1lca=0.000
      r2lca=0.000
      DBCON=2.624E-2
      DBDEN=1.177
      DBSHT=1004.9
      E=0.01
      A=0.01
      DRV=1.00
      spec=0.0
      rough=0.0
      misc1=0.0
      misc2=0.0
      call prsmat(idbh,iit,'p',acoutag,colormtag,lcatag,
     &  lcatype,r1lca,r2lc,NCODE,DBCON,DBDEN,DBSHT,E,A,DRV,
     &  spec,rough,misc1,misc2,ier)
      IREC=IREC+1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(1)
      IREC=IREC+1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(2)
      lastr(idbh)=IREC

C For each category, read existing items.
      DO 10 I=2,nbcat(idbh)
        icl=I
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'IC NE NDESC',IER)
        IF(IER.NE.0)RETURN
        K=0

C Note: with initial gasses cagt, the IC read in will be different from
C that of the database.
        CALL EGETWI(OUTSTR,K,IC,0,30,'F','catg',IER)
        CALL EGETWI(OUTSTR,K,NE,0,19,'-','no elem',IER)
        nbcatit(icl)=NE
        CALL EGETRM(OUTSTR,K,NDESC,'-','description',IER)

C Take current documentation and make a name out of it.
        write(T12,'(a)') NDESC(1:12)
        call st2name(T12,D12)
        write(cattag(icl),'(a)') D12
        write(catmenu(icl),'(a)') NDESC(1:32)
        write(cattype(icl),'(a)') 'STD     '
        nbcatdoc(icl)=1
        write(catdoc(1),'(5a)') 'Category ',NDESC(1:lnblnk(NDESC)),
     &    ' imported from the legacy database: ',
     &    AFIL(1:lnblnk(AFIL)),' no other notes entered (yet),'
        call dstamp(dstmp)
        write(catdate(icl),'(a)') dstmp

        IREC=IREC+1
        irscat(icl)=IREC
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &    catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)

        IREC=IREC+1
        c8='*CATDOC '
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(1)

C Write values for classification.
        DO 20 J=1,nbcatit(icl)
          iit=iit+1
          CALL STRIPC(IFA,OUTSTR,0,ND,1,'NCODE,IDESC',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTSTR,K,NCODE,0,600,'F','itm code',IER)
          CALL EGETRM(OUTSTR,K,IDESC,'W','itm description',IER)

C Crate tag for material entry.
          if(NCODE.le.9)then
            write(ittag(iit),'(a,i1)') 'mat',NCODE
          elseif(NCODE.gt.9.and.NCODE.le.99)then
            write(ittag(iit),'(a,i2)') 'mat',NCODE
          elseif(NCODE.gt.99.and.NCODE.le.999)then
            write(ittag(iit),'(a,i3)') 'mat',NCODE
          endif
          write(itmenu(iit),'(a)') IDESC(1:32)
          call dstamp(dstmp)
          write(itdate(iit),'(a)') dstmp

C Set initial item name.
          if(lnblnk(IDESC).lt.32)then
            write(itdoc,'(3a)') 'material ',IDESC(1:lnblnk(IDESC)),
     &        ' is...'
          else
            write(itdoc,'(3a)') 'material ',IDESC(1:32),' is...'
          endif

          IREC=IREC+1
          nbitusc(iit)=17
          ibitusc(iit)=0
          ituntstr='-------abc-------' 
          itsrcstr='-'
          itcnfstr='-----------------'
          nbitdat(iit)=2
          nbitrep(iit)=0
          irsit(iit)=IREC
          itcat(iit)=icl
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &      itdate(iit),itcat(iit),nbitdat(iit),
     &      nbitrep(iit),nbitusc(iit),ibitusc(iit)

C write itdoc into next record.
          c8='*ITDOC  '
          IREC=IREC+1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,itdoc

          IREC=IREC+1
          c8='*USC    '
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,ituntstr,itsrcstr,
     &      itcnfstr

          CALL STRIPC(IFA,OUTSTR,0,ND,1,'data',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWR(outstr,K,DBCON,0.001,300.,'W','conduct',IER)
          CALL EGETWR(outstr,K,DBDEN,1.0,9000.,'W','density',IER)
          CALL EGETWR(outstr,K,DBSHT,1.0,2500.,'W','specc ht',IER)
          CALL EGETWR(outstr,K,E,0.01,0.99,'W','emissivity',IER)
          CALL EGETWR(outstr,K,A,0.01,0.99,'W','absorptivity',IER)
          CALL EGETWR(outstr,K,DRV,1.,20000.,'W','diff resis',IER)

C Assume that specularity is 0.0 and roughness is 0.0
          spec=0.0
          rough=0.0
          misc1=0.0
          misc2=0.0

C Write initial data for material. First write unpacked and then pack it and
C then copy to 96 char array.
          acoutag='none'
          colormtag='none'
          lcatag='none'
          lcatype='-'
          r1lca=0.000
          r2lca=0.000
          call prsmat(idbh,iit,'p',acoutag,colormtag,lcatag,
     &      lcatype,r1lca,r2lca,NCODE,DBCON,DBDEN,DBSHT,E,A,DRV,
     &      spec,rough,misc1,misc2,ier)
          IREC=IREC+1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(1)
          IREC=IREC+1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(2)
          lastr(idbh)=IREC
   20   CONTINUE
   10 CONTINUE

C End of db, complete the headder. First update all cat and item pointers.
      nbitem(idbh)=iit
      IREC = 3 + nbunit(idbh) + nbsrc(idbh) + nbdbdoc(idbh)
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=1,24)
      IREC=IREC+1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=25,MGCL)
      call updirsit(IFB,idbh,'W')
      c20='*ESP-r binary db    '
      irec=1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
      call tagend(IFB,idbh,ier)

C Debug.
C      write(6,*) 'last record in gdb file is ',lastr(idbh),lastr(idbh)+1

C Close file before exiting.
      CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      call edisp(iuout,' Reached end of legacy file, closing files.')
      RETURN
      
  99  CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      RETURN

   1  write(outs,'(a,I6)') 'AMATGMAT: Error writing record ',IREC
      call edisp(iuout,outs)
      ier=1
      goto 99
    
      END

C ************* IMPRESSC 
C IMPRESSC: Imports from legacy pressure coef file (AFIL) to a generic
C wind pressure db file (BFIL). Unit IFA is the asci file channel and
C IFB as the binary channel. 
      subroutine impressc(IFA,AFIL,IFB,BFIL,idbh,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendbs/srcchr(24),srcnote(24)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      DIMENSION FDAT(16)
      CHARACTER OUTSTR*124,AFIL*72,BFIL*72
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character itdat*248,itrep*248
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,c8*8,c20*20,outs*124
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248
      character dstmp*24,setn*40
      integer ier

C Initialise variables.
      call clrgdbc(idbh)
      dbtag(idbh)='pressc_db'
      dbmenu(idbh)='Wind pressure db'
      dbnote(idbh)='Wind pressure db log...'
      call dstamp(dstmp)
      write(dbdate(idbh),'(a)') dstmp
      call intdbdoc(idbh,'i',afil,ier)
      nbsrc(idbh)=2
      srcnote(2)='AIVC: Air Infiltration Calculation Techniques'
      nbunit(idbh)=1
      unitstr(1)='(-)'
      unitnote(1)='(-)'
      nbcat(idbh)=1
      cattag(1)='convcat'
      cattype(1)='STD '
      catmenu(1)='Converted pressure coef.'
      call dstamp(dstmp)
      write(catdate(1),'(a)') dstmp
      nbcatdoc(1)=1
      write(catdoc(1),'(2a)') 'Category convcat documentation ',
     &  'follows: (imported from legacy pressure coef db.)'
      iit=0

C The binary file will be overwritten.
      CALL ERPFREE(IFB,IOS)
      idbw=idbwidth(idbh)
      ier=0
      call EFOPRAN(IFB,BFIL,idbw,4,IER)
      if(ier.ne.0)then
        call edisp(iuout,'error opening binary file')
        return
      endif

C Open the ASCII file for reading.
      CALL EFOPSEQ(IFA,AFIL,1,IER)

C Proceed, there is only one category in a legacy pressure db file
C so put all of these in one category.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'no coef',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,nbitem(idbh),1,1,'-','nb coef',IER)
      nbcatit(1)=nbitem(idbh)

C Write out db header and place holders for category start record pointers
C (irscat) and items start record pointers.
      call wgdbhead(IFB,idbh,IER)
      call updirsit(IFB,idbh,'I')
      IREC = 3 + nbunit(idbh) + nbsrc(idbh) + nbdbdoc(idbh) + 2 +12

C Writ category header.
      icl=1
      IREC=IREC+1
      irscat(icl)=IREC
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &  catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)

      IREC=IREC+1
      c8='*CATDOC '
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(1)

C For each item, read existing items.
      DO 10 I=1,nbitem(idbh)
        iit=iit+1
        if(iit.le.9)then
          write(ittag(iit),'(a,i1)') 'pressc',iit
        elseif(iit.gt.9.and.iit.le.99)then
          write(ittag(iit),'(a,i2)') 'pressc',iit
        elseif(iit.gt.99.and.iit.le.999)then
          write(ittag(iit),'(a,i3)') 'pressc',iit
        endif
        call dstamp(dstmp)
        write(itdate(iit),'(a)') dstmp

C Next 6 lines should match code in instanitm of dbmedit.F
        nbitusc(iit)=16
        ibitusc(iit)=1
        ituntstr='----------------' 
        itsrcstr='a'
        itcnfstr='----------------'
        nbitdat(iit)=2
        nbitrep(iit)=0
        K=0
        CALL STRIPC(IFA,OUTSTR,10,ND,1,'press db data 1st',IER)
        IF(IER.NE.0) GOTO 1
        do 56 naa=1,10
          CALL EGETWR(OUTSTR,K,PC,-1.5,1.5,'W','pres coef',IERV) 
          IF(IER.NE.0) GOTO 1
          FDAT(naa)=PC 
  56    continue

C Read the second line of each set for remaining data and description.
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'press db data 2nd',IER)
        IF(IER.NE.0) GOTO 1
        K=0
        do 65 naa=11,16
          CALL EGETWR(OUTSTR,K,PC,-1.5,1.5,'W','pres coef',IER) 
          IF(IER.NE.0) GOTO 1
          FDAT(naa)=PC 
  65    continue
        CALL EGETRM(OUTSTR,K,setn,'W','pc set name',IER)
        write(itmenu(iit),'(a)') setn(1:32)

        if(lnblnk(setn).lt.32)then
          write(itdoc,'(2a)') setn(1:lnblnk(setn)),' is...'
        else
          write(itdoc,'(2a)') setn(1:32),' is...'
        endif

        IREC=IREC+1
        irsit(iit)=IREC
        itcat(iit)=icl
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &    itdate(iit),itcat(iit),nbitdat(iit),nbitrep(iit),
     &    nbitusc(iit),ibitusc(iit)

C write itdoc into next record.
        c8='*ITDOC  '
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,itdoc

        IREC=IREC+1
        c8='*USC    '
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,ituntstr,itsrcstr,
     &    itcnfstr

C Encode and write values.
        call PRSPRES(idbh,iit,'p',FDAT,ier)
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(1)
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(2)
        lastr(idbh)=IREC
   10 CONTINUE

C End of db, complete the headder. First update all cat and item pointers.
      IREC = 3 + nbunit(idbh) + nbsrc(idbh) + nbdbdoc(idbh)
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=1,24)
      IREC=IREC+1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=25,MGCL)
      call updirsit(IFB,idbh,'W')
      c20='*ESP-r binary db    '
      irec=1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
      call tagend(IFB,idbh,ier)

C Debug.
C      write(6,*) 'last record in pressc file is ',lastr(idbh),
C     &  lastr(idbh)+1

C Close file before exiting.
      CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      call edisp(iuout,' Reached end of legacy file, closing files.')
      RETURN
      
  99  CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      RETURN

   1  write(outs,'(a,I6)') 'IMPRESSC: Error writing record ',IREC
      call edisp(iuout,outs)
      ier=1
      goto 99
    
      END

C ************* IMPROFL 
C IMPROFL: Imports from legacy ascii profile file (AFIL) to a generic
C profile db file (BFIL). Unit IFA is the asci file channel and
C IFB as the binary channel.

C Use the event common blocks for the data - the ASCII file is
C not actually used.
 
      subroutine improfl(IFA,AFIL,IFB,BFIL,idbh,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendbs/srcchr(24),srcnote(24)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

C Event profiles common blocks.
      COMMON/PDBITM/NPDBITEMS,lastrec,NCG(MGIT),NO(MGIT),PDBDESC(MGIT)
      COMMON/PDBELM/ICGS1(MGIT,MPRFDBPER),ICGF1(MGIT,MPRFDBPER),
     &             CGS1(MGIT,MPRFDBPER),CGL1(MGIT,MPRFDBPER)
      CHARACTER PDBDESC*40

      dimension DayT(72),ST(72),FN(72),A(72),B(72),C(72)
      dimension Patr(72)

      CHARACTER AFIL*72,BFIL*72
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character itdat*248,itrep*248
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,c8*8,c20*20,outs*124
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248
      character dstmp*24,DayT*8,Patr*8
      integer ier

C Initialise variables.
      call clrgdbc(idbh)
      dbtag(idbh)='profiles'
      dbmenu(idbh)='Event profiles db'
      dbnote(idbh)='Event profiles db log...'
      call intdbdoc(idbh,'i',afil,ier)
      nbsrc(idbh)=1
      nbunit(idbh)=1
      unitstr(1)='(-)'
      unitnote(1)='(-)'
      call dstamp(dstmp)
      write(dbdate(idbh),'(a)') dstmp
      nbcat(idbh)=1
      cattag(1)='convprofile'
      cattype(1)='STD '
      catmenu(1)='Converted event profiles'
      call dstamp(dstmp)
      write(catdate(1),'(a)') dstmp
      nbcatdoc(1)=1
      write(catdoc(1),'(2a)') 'Category convprofile documentation ',
     &  'follows: (imported from legacy profiles db.)'
      iit=0

C The binary file will be overwritten.
      CALL ERPFREE(IFB,IOS)
      idbw=idbwidth(idbh)
      ier=0
      call EFOPRAN(IFB,BFIL,idbw,4,IER)
      if(ier.ne.0)then
        call edisp(iuout,'error opening binary file')
        return
      endif

C Use the PDBITM and PDBELM common blocks.
      nbitem(idbh)=NPDBITEMS
      nbcatit(1)=nbitem(idbh)

C Write out db header and place holders for category start record pointers
C (irscat) and items start record pointers.
      call wgdbhead(IFB,idbh,IER)
      call updirsit(IFB,idbh,'I')
      IREC = 3 + nbunit(idbh) + nbsrc(idbh) + nbdbdoc(idbh) + 2 +12

C Write category header.
      icl=1
      IREC=IREC+1
      irscat(icl)=IREC
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &  catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)

      IREC=IREC+1
      c8='*CATDOC '
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(1)

C For each item, read existing items.
      DO 10 I=1,nbitem(idbh)
        iit=iit+1
        if(iit.le.9)then
          write(ittag(iit),'(a,i1)') 'profile',iit
        elseif(iit.gt.9.and.iit.le.99)then
          write(ittag(iit),'(a,i2)') 'profile',iit
        elseif(iit.gt.99.and.iit.le.999)then
          write(ittag(iit),'(a,i3)') 'profile',iit
        endif
        call dstamp(dstmp)
        write(itdate(iit),'(a)') dstmp

C There are 8 types of information in the file. next 6 lines
C should match that in instanitm of dbmedit.F
        nbitusc(iit)=5
        ibitusc(iit)=1
        ituntstr='-aaaa' 
        itsrcstr='-'
        itcnfstr='-----'

        nbitdat(iit)=19
        nbitrep(iit)=18

C NCG and DESC from commons.
        write(itmenu(iit),'(a)') PDBDESC(iit)(1:32)
        do 119 J = 1,72
          DayT(j)='all'
          Patr(j)='none'
          ST(j)=0.0
          FN(j)=0.0
          A(j)=0.0
          B(j)=0.0
          C(j)=0.0
  119   continue
        DO 20 J = 1,NCG(iit)
          ST(j)=ICGS1(iit,j)
          FN(j)=ICGF1(iit,j)
          A(j)=CGS1(iit,j)
          B(j)=CGL1(iit,j)
   20   CONTINUE

C If last FN is not 24.0 then increment periods with ST equal to
C the last FN.
        if(FN(NCG(iit)).lt.24.0)then
          NCG(iit)=NCG(iit)+1
          ST(NCG(iit))=FN(NCG(iit)-1)
        endif
        if(lnblnk(PDBDESC(iit)).lt.32)then
          write(itdoc,'(2a)') PDBDESC(iit)(1:lnblnk(PDBDESC(iit))),
     &      ' is...'
        else
          write(itdoc,'(2a)') PDBDESC(iit)(1:32),' is...'
        endif

        IREC=IREC+1
        irsit(iit)=IREC
        itcat(iit)=icl
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &    itdate(iit),itcat(iit),nbitdat(iit),nbitrep(iit),
     &    nbitusc(iit),ibitusc(iit)

C write itdoc into next record.
        c8='*ITDOC  '
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,itdoc

        IREC=IREC+1
        c8='*USC    '
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,ituntstr,itsrcstr,
     &    itcnfstr

C Write values.
C Repeting data next (encode into itrep strings).
        call prsprof(idbh,iit,'p',NCG(iit),Patr,DayT,ST,A,B,C,ier)
        do 246 ij=1,nbitdat(iit)
          IREC=IREC+1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(ij)
 246    continue

        lastr(idbh)=IREC
        if(nbitrep(iit).gt.0)then
          do 247 ij=1,nbitrep(iit)
            IREC=IREC+1
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itrep(ij)
 247      continue
          lastr(idbh)=IREC
        endif
   10 CONTINUE

C End of db, complete the headder. First update all cat and item pointers.
      IREC = 3 + nbunit(idbh) + nbsrc(idbh) + nbdbdoc(idbh)
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=1,24)
      IREC=IREC+1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=25,MGCL)
      call updirsit(IFB,idbh,'W')
      c20='*ESP-r binary db    '
      irec=1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
      call tagend(IFB,idbh,ier)

C Debug.
C      write(6,*) 'last record in profile file is ',lastr(idbh),
C     &  lastr(idbh)+1

C Close file before exiting.
      CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      call edisp(iuout,' Reached end of legacy file, closing files.')
      RETURN
      
  99  CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      RETURN

   1  write(outs,'(a,I6)') 'IMPROFL: Error writing record ',IREC
      call edisp(iuout,outs)
      ier=1
      goto 99
    
      END


C ************* MKAPRC (copy from edcondb.F)
C MKAPRC: Creates an ascii materials db based on the
C current contents of binary file.
      SUBROUTINE MKAPRC(LASCI,IFA,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      DIMENSION ICLN(30),VAL(6)
      CHARACTER IDESC*72,NDESC*72,LASCI*72
      integer ier

C Open the binary materials database (in clase it was closed elsewhere).
      CALL ERPFREE(IFMAT,ISTAT)
      ier=0
      call EFOPRAN(IFMAT,LFMAT,40,1,IER)
      if(ier.ne.0)return
      MATDBOK=.TRUE.

C The ASCII file will be overwritten if it exists.
      CALL EFOPSEQ(IFA,LASCI,4,IER)
      
C Write out a file header.
      WRITE(IFA,30)LASCI(1:lnblnk(LASCI))
  30  FORMAT('# materials db defined in ',A)

C Check binary contents.
      IREC=601
      READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCLASS,(ICLN(K),K=1,30)
      IF(NCLASS.LE.0)THEN
        CALL USRMSG(' binary materials database empty! ',' ','W')
        IER=1
        RETURN
      ENDIF

C Proceed.
      WRITE(IFA,'(I5,4X,A)')NCLASS,'#  number of classifications'
      DO 10 I=1,NCLASS
        IC=ICLN(I)
        IREC=IC*20-19
        READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NE,IDESC
        WRITE(IFA,12)IC,NE,IDESC(:lnblnk(IDESC))
   12   FORMAT('# class|no elements|description',/,I5,I5,3X,A)
        IF(NE.LE.0)goto 10
        DO 20 J=1,NE
          IREC=IREC+1
          READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(VAL(IL),IL=1,6),
     &         NDESC
          NCODE=IREC-1
          WRITE(IFA,13)NCODE,NDESC(1:lnblnk(NDESC)),(VAL(K),K=1,6)
   13     FORMAT( I5,3X,A,/,F13.3,2(', ',F8.3),2(', ',F6.3),', ',F10.3)
   20   CONTINUE
   10 CONTINUE

  99  CALL ERPFREE(IFA,ISTAT)
      RETURN

C File errors.
 1000 CALL USRMSG(' NE,NDESC read error in ',LFMAT,'W')
      IER=1
      goto 99
 1001 CALL USRMSG(' Layer data write error in ',LFMAT,'W')
      IER=1

      END

C **************** TROPRO (copied from esruprj/edpro.F)
C TROPRO creates an ASCII profiles db from the binary profiles db.
C NOTE LASCI is 72 char. This is called only by esrudbm.
      SUBROUTINE TROPRO(IFA,LASCI,IER)
#include "esprdbfile.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PDBITM/NPDBITEMS,lastrec,NCG(MGIT),NO(MGIT),PDBDESC(MGIT)
      COMMON/PDBELM/ICGS1(MGIT,MPRFDBPER),ICGF1(MGIT,MPRFDBPER),
     &             CGS1(MGIT,MPRFDBPER),CGL1(MGIT,MPRFDBPER)
      CHARACTER PDBDESC*40,LASCI*72
      character ermsg*48   ! for read error messages
      character outs*124   ! string buffer for messages
      integer ier

      IER =0

C Because it is a binary file we are expecting a non-zero ier
C does not require that we attempt to scan an ascii version of
C the event profiles db.
      CALL ERPFREE(IPRODB,ISTAT)
      call EFOPRAN(IPRODB,LPRFDB,10,1,IER)
      if(ier.ne.0)return

      ermsg='reading record one number of profiles '
      IREC=1
      READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)NPDBITEMS
      IF(NPDBITEMS.LE.0)THEN
        CALL USRMSG(' ',' Events profiles db is empty !','W') 
        RETURN
      ENDIF

C Open or overwrite the ASCII file.
      CALL EFOPSEQ(IFA,LASCI,4,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' Could not open ASCII file!',LASCI,'W')
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))
      
C Write out a file header.
      WRITE(IFA,30)LASCI(1:lnblnk(LASCI))
  30  FORMAT('# events profiles db defined in ',A)

C Proceed
      ermsg='reading record one number of profiles '
      IREC=1
      READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NPDBITEMS

      WRITE(IFA,21,IOSTAT=ISTAT,ERR=1000)NPDBITEMS
   21 FORMAT(I5,'    # Number of profiles')

      DO 10 I = 1,NPDBITEMS
        write(ermsg,'(a,i2)') 'reading number events in profile ',I
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NCG(I)
        write(ermsg,'(a,i2)') 'reading description of profile ',I
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  PDBDESC(I)

        WRITE(IFA,22)NCG(I),PDBDESC(I)
   22   FORMAT(I5,3X,A40,'  # No periods & description',/,
     &                   '# start finish data data')

C For each period.
        DO 20 J = 1,NCG(I)
          write(ermsg,'(a,i3,a,i2)') 'reading data of profile ',I,
     &      ' at period ',J
          IREC = IREC + 1
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)IS,IF,A,B
          ICGS1(I,J)=IS
          ICGF1(I,J)=IF
          CGS1(I,J)=A
          CGL1(I,J)=B
          WRITE(IFA,23,IOSTAT=ISTAT,ERR=1000)IS,IF,A,B
   23     FORMAT(I4,',',I4,',',F8.3,',',F8.3)
   20   CONTINUE
   10 CONTINUE

  99  CALL ERPFREE(IPRODB,ISTAT)
      CALL ERPFREE(IFA,ISTAT)
      RETURN

 1000 CALL USRMSG(' ',' ASCII file write error in ','W')
      goto 99

 1001 write(outs,'(3a,i4)') 'In profile db: error ',
     &  ermsg(1:lnblnk(ermsg)),' at record ',irec
      call edisp(iuout,outs)
      goto 99

      END


C *************** ERPCDB (copied from esrucom/edatabase.F)
C ERPCDB reads one element from a binary materials database
C returning them via parameter list.
C IFMAT is the unit number of the database,  IEL the requested 
C element, ITRU unit number for user output, IER=0 OK, IER=1 problem.
C ITRC is the trace level.  It is assumed that the database has been
C previously opened and other parts of the code will close it when
C appropriate.  E is the element emissivity, A is the solar absorb, DRV
C is the diffusion resistance, TITL is the classification title, PNAM is
C the material name.

      SUBROUTINE ERPCDB(IFMAT,IEL,ITRU,IER,DBCON,DBDEN,DBSHT,E,A,
     &                  DRV,TITL,PNAM)

      CHARACTER PNAM*72,TITL*72,outs*124
      logical close1,close2,close3

      IER=0
      close1=.false.
      close2=.false.
      close3=.false.

C The record in the material db is IEL + 1 UNLESS the
C reference is to material db 0 (air).
      PNAM=' '
      TITL=' '
      DBCON=0.0
      DBDEN=0.0
      DBSHT=0.0
      IF(IEL.NE.0)THEN
        IR=((IEL-1)/20)*20+1
        READ(IFMAT,REC=IR,IOSTAT=ISTAT,ERR=105)TITL
        IR = IEL + 1
        READ(IFMAT,REC=IR,IOSTAT=ISTAT,ERR=105)DBCON,DBDEN,DBSHT,E,A,
     &       DRV,PNAM

C If all values are still 0.0 then not an actual element.
        CALL ECLOSE(DBCON,0.0,0.001,CLOSE1)
        CALL ECLOSE(DBDEN,0.0,0.001,CLOSE2)
        CALL ECLOSE(DBSHT,0.0,0.001,CLOSE3)
        if(CLOSE1.and.CLOSE2.and.CLOSE3)then
          write(outs,'(A,I3,A,2i4)') ' Material db reference ',IEL,
     &                           ' has no data, or all zero...',IR,IFMAT
          call usrmsg(outs,' Please check your selection! ','W')
          ier=1
          return
        endif
      ELSE

C Air layer encountered so put in zeroes.
        E=0.99
        A=0.99
        DRV=1.0
        PNAM='AIR'
        TITL='GAPS'
      ENDIF

  99  RETURN
 105  write(outs,'(A,I3,A,2i4)') ' Materials db reference ',IEL,
     &    ' read error @ record ',IR,' in file',IFMAT
      call edisp(itru,outs)
      IER=1
      CALL ERPFREE(IFMAT,ISTAT)
      goto 99

      END

C ******* parsemlcdesc (copy from edatabase.F)
C parsemlcdesc: Parse the DESC string from common block returning text for
C MLCN (its name) ,OTF (whether OPAQ/TAN) ,OPT (optical name) ,MLCSYM
C tag for symmetry. Where values are missing or unknown setup defaults.
      subroutine parsemlcdesc(DES,MLCN,OTF,OPT,MLCSYM)
      
      integer lnblnk  ! function definition

      character DES*48,MLCN*12,OTF*4,OPT*12,MLCSYM*12

C Clear parameters to be returned and then copy from DES.
      MLCN=' '
      OTF=' '
      OPT=' '
      MLCSYM=' '
      WRITE(MLCN,'(A)')DES(1:12)
      WRITE(OTF,'(A)') DES(15:18)
      IF(OTF.EQ.' ')OTF='OPAQ'
      IF(OTF.EQ.'TRAN')THEN
        WRITE(OPT,'(A)') DES(21:32)
        IF(OPT.EQ.' ')OPT='UNKNOWN'
      ELSE
        OPT='OPAQUE'
      ENDIF

C Read tag for name of inverted version or symmetry if the length of the
C DESC string could hold the tag, otherwise set MLCSYM to a blank.
      if(lnblnk(DES).gt.35)then
        WRITE(MLCSYM,'(A)') DES(35:46)
      else
        MLCSYM='  '
      endif
      return
      end
