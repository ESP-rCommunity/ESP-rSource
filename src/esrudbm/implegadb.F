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

C ************* IMPMLC 
C IMPMLC: Imports from legacy construction file (AFIL) to a generic
C construction db file (BFIL). Unit IFA is the asci file channel and
C IFB as the binary channel. Idbh is position in hash table.
      subroutine impmlc(IFA,AFIL,IFB,BFIL,idbh,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
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
      dimension IP(ME),DRA(ME,3),glaydesc(ME)
      CHARACTER OUTSTR*124,AFIL*72,BFIL*72,GDESC*48,NAM*72,RSTR*124
      CHARACTER WORD*20
      character OTF*4,OPT*12
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character itdat*248,itrep*248
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,c8*8,c20*20,outs*124
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248
      character dstmp*24,symname*12,othcolr*12,incolr*12
      character othacou*12,inacou*12,cnstimp*12
      character thrmtg*1,gaptg*1,mattag*12,glaydesc*32
      CHARACTER LLTMP*144,DFILE*72

C Logical noting whether it was possible to open legacy material
C database to get IR and solar properties.
      logical scanmat
      logical closemat1,closemat2
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
      write(LLTMP,'(a)') LFMAT(1:lnblnk(LFMAT))
      DFILE='/usr/esru/esp-r/databases/material.db3.a'
  29  CALL EASKS(LLTMP,
     &  'Legacy materials db used with this legacy construction',
     &  'database?',96,DFILE,'legacy matrl name',IER,1)
      IF(LLTMP.EQ.' ')GOTO 29
      IFMAT=IFIL
      call ERPFREE(IFMAT,IOS)
      ier=0
      call rascimat(IFMAT,LLTMP,IER)  ! try current ascii format
      if(ier.eq.-2)then
        ier=0
        call rlegacymat(IFMAT,LLTMP,ier)  ! try older ascii format
        if(ier.ne.0)then
          othir=0.90
          sinir=0.90
          othsol=0.50
          sinsol=0.50
          scanmat=.false.
        else
          call eclose(matver,1.1,0.001,closemat1)
          call eclose(matver,1.2,0.001,closemat2)
          if(closemat1.or.closemat2)then
            call edisp(iuout,' Filled material data arrays...')
            scanmat=.true.
          else
            othir=0.90
            sinir=0.90
            othsol=0.50
            sinsol=0.50
            scanmat=.false.
          endif
        endif
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
        GDESC=RSTR(1:LNBLNK(RSTR))

C Parse the GDESC string into string tokens.
        call matchmlcdesc(GDESC(1:12),im)

        write(symname,'(a)') mlcsymetric(im)(1:12)
        othcolr='grey50'
        incolr='grey50'
        othacou='none'
        inacou='none'
        cnstimp='none'
        write(itmenu(iit),'(a)') mlcname(im)(1:lnmlcname(im))
        write(itdoc,'(3a)') 'construction ',
     &    mlcname(im)(1:lnmlcname(im)),' is...'
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

C Crate tag for material and reference tag for material db entry.
C If material db was opened then scan for ir and sol values.
          CALL EGETWI(OUTSTR,K,IP(il),0,600,'F','legacy index',IER)
          if(scanmat)then

C Generally we want to assign values to the IPRMAT array so for each legacy 
C IPR. NOTE: in case there are multiple items with the same characters
C in the name also check to see if the legacy index associated
C with the found item matches the IPR scanned above. If we are
C still confused mark IPRMAT as -99.
            ileghope=IP(IL)   ! what we hope we will find a match for

C Pass the legacy record number (ileghope) along with the name (NAM) of the
C material and get back the index of the material array that fits.
            call parsematdesc(ileghope,NAM,matarrayindex)

            if(IL.eq.1)then
              if(matarrayindex.eq.0)then
                othir=0.0
                othsol=0.0
              else
                othir=matdboute(matarrayindex)
                othsol=matdbouta(matarrayindex)
              endif
            elseif(IL.eq.nlay)then
              if(IP(IL).eq.0)then
                othir=0.0
                othsol=0.0
              else
                sinir=matdbine(matarrayindex)
                sinsol=matdbina(matarrayindex)
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
            glaydesc(iln)='not found'
            do 35 itother=1,nbitem(isdbhash)
              if(mattag(iln)(1:12).eq.ittag(itother)(1:12))then
                glaydesc(iln)=itmenu(itother)
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
     &   nlay,thrmtg,gaptg,mattag,DT,IP,DRA,glaydesc,ier)

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
#include "profile.h"

C << introduce concept of profiles with multiple day types as well
C << as multiple casual gain types

C << also consider that periods can be defined as reals and only
C << the start of period held
      
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

C Event profiles common blocks via profile.h.

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

C << this is a copy of source from esrucom/edatabase.F >>
C ******* matchmlcdesc
C matchmlcdesc parse string (DES) and see if it matches a MLC
C item - if so return its MLC index. DES might be from SMLCN or some 
C other source.
      subroutine matchmlcdesc(DES,imlcindex)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

C Passed parameters DES length is flexable.
      character*(*) DES
      integer imlcindex

C Local variable.
      character MLCN*32
      integer ii,lnmlcn,lndesc

C Look for index for MLCN in the current constructions file.
      imlcindex=0  ! assume no matching MLC          
      lnmlcn=lnblnk(DES)
      WRITE(MLCN,'(A)')DES
      if(MLCN(1:4).eq.'UNKN') return
      do ii=1,nmlc
        lnl=lnblnk(MLCN)
        if(MLCN(1:lnl).eq.mlcname(ii)(1:lnmlcname(ii))) then
          imlcindex=ii   ! remember MLC index     
        endif
      end do
      return
      end

C << this is a copy of source from esrucom/edatabase.F >>
C ******* parsematdesc
C Parse the NAM string from the MLC common block returning the index
C (matarrayindex) of the version 1.1 or 1.2 materials array.
C Newer MLC databases will include lines in the form:
C   104    0.0130  gypsum plaster : Gypsum based plaster
C and older MLC databases wil include lines in the form:
C   104    0.0130  Gypsum plaster
C So for newer MLC we are searching for ' : ' but in older
C files there will not be such a string.

C The logic initially looks for ' : ' and trys to match the matname 
C variable and if that fails it trys without ' : ' and if that
C fails it trys against matdoc. Lastly the value of
C ileghope is checked against matlegindex. If no match is found a
C warning is given and matarrayindex is returned as zero.

      subroutine parsematdesc(ileghope,NAM,matarrayindex)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk        ! function definition

C Parameters
      integer ileghope      ! the legacy index to match against
      character NAM*72      ! passed string from MLC
      integer matarrayindex ! returned index in materials array

      integer iuout,iuin
      COMMON/OUTIN/IUOUT,IUIN

      integer lnam,ltnam    ! length of material name
      integer ipos,i2pos,iepos ! position start, 2 char along, end
      character testname*32 ! local name for comparison
      character PNAM*72     ! local string variable for scanning
      logical foundcolon    ! true if ' : ' found
      character outs*124

C Initial assumptions.
      foundcolon=.false.
      matarrayindex=0

C If the ileghope is zero then we are dealing with the standard (implied)
C air gap and so no name is going to match and we can return with the
C initial matarrayindex=0.
      if(ileghope.eq.0)then
        return
      endif

C Copy NAM to PNAM and then search for ' : ' in PNAM and save to local
C strings the matname.
      PNAM=NAM
      lnam=lnblnk(PNAM)
      ipos=1   ! initial positions
      i2pos=3
      iepos=lnam-3
      do 42 ip=1,iepos
        if(PNAM(ipos:i2pos).eq.' : ')then

C Save the string prior to ' : ' in testname and then see if any of
C the matname matches.
          foundcolon=.true.
          write(testname,'(a)') PNAM(1:ipos-1)
          ltnam=lnblnk(testname)

C Find a matching matname, if the legacy index associated with this item
C matches ileghope then our search is complete.  If it does not match
C ileghope then there could be duplicate names to check further. If
C ileghope is -99 then return what was found as an index match not possible.
          do 43 imt=1,matdbitems
            if(testname(1:ltnam).eq.matname(imt)(1:ltnam))then
              if(matlegindex(imt).eq.ileghope)then
                matarrayindex=imt
                return
              else
                if(ileghope.eq.-99)then
                  matarrayindex=imt
                  return
                else
                  continue   ! keep looking for a text and index match
                endif
              endif
            endif
  43      continue
        else
          ipos=ipos+1
          i2pos=i2pos+1
        endif
  42  continue

C If we did not find a ' : ' then take the PNAM literally and check against both
C the matname and if that fails against matdoc
      if(.NOT.foundcolon)then

C Find a matching matname.
        do 44 imt=1,matdbitems
C check first the lenght of strings, if the stringsa re not equal the
C material is different
          if(lnblnk(matname(imt)).eq.lnam) then        
             if(PNAM(1:lnam).eq.matname(imt)(1:lnam))then
               matarrayindex=imt
               return
             endif
          endif
  44    continue

C If we got to this point also check for a matching matdoc string.
        do 45 imt=1,matdbitems
          if(PNAM(1:lnam).eq.matdoc(imt)(1:lnam))then
            matarrayindex=imt
            return
          endif
  45    continue
      endif

C There might have been a ' : ' but a text mismatch for matname so check matdoc.
      do 46 imt=1,matdbitems
        if(PNAM(1:lnam).eq.matdoc(imt)(1:lnam))then
          matarrayindex=imt
          return
        endif
  46  continue

C Could not find it via a string match so see if there is a legacy index that
C matches the required index.  This sometimes happens if the current version
C materials database has a material name edited and the MLC database does not
C have a matching string (but the legacy index is correct non-the-less).
      do 47 imt=1,matdbitems
        if(matlegindex(imt).eq.ileghope)then
          matarrayindex=imt
          return
        endif
  47  continue

C If we got here there was no match. The value of matarrayindex remains at zero.
      lnam=lnblnk(PNAM)
      write(outs,*) 
     &  'When scanning materials db looking for legacy index',ileghope
      call edisp(iuout,outs)
      write(outs,*) 'no match found for material named ',PNAM(1:lnam)
      call edisp(iuout,outs)
  
      return
      end
