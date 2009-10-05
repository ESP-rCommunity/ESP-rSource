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

C Subroutines to parse between itdat and itrep commons and domain
C specific data.
C PRSMLC: gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C     construction db
C PRSMAT gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C     material db
C PRSCOLR gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C     colour db
C PRSACOU gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C     acoustic db
C PRSENVCI gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C     an environmental impact constructions impact db entity
C PRSENVI gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C     an environmental impact db
C PRSENVF gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C     an environmental material impact db
C PRSENVP gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C     an assembly (constructions assembly or assembly at project) or
C     maintenance db entity

C *************** PRSMLC
C PRSMLC gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C constr db via esp-r common block gendb8b. iit the requested element,
C IER=0 OK, IER=1 problem.
C symname is name of matching constr (`same` if symetric)
C othcolr & incolr are colour names at other and inside face,
C OTF is tag OPAQ/TRAN, OPT is optical name, othacou and inacou are
C acoustic names at other and inside face, othir and sinir are IR
C emis at other and inside face, othsol and sinsol are solar abs at
C other and inside face. The IR and solar values usually equal that
C of the other and inner face materials.
C cnstimp is the construction impacts name, nlay is number of layers
C thrmtg is a character tag denoting thermal or non-thermal layer,
C gaptg is a character tag denoting if layer is solid or gas, mattag and
C mattag are arrays of matrials names, DT is array 
C of layer thicknesses, IP is array of legacy record id, DRA is array 
C of air gap resistances, laydesc is the corresponding material description.

      subroutine prsmlc(idbh,iit,act,symname,othcolr,incolr,OTF,OPT,
     &  othacou,inacou,othir,sinir,othsol,sinsol,cnstimp,
     &  nlay,thrmtg,gaptg,mattag,DT,IP,DRA,laydesc,ier)
#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h provides gendbfn
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      dimension thrmtg(ME),gaptg(ME),mattag(ME),DT(ME)
      dimension IP(ME),DRA(ME,3),laydesc(ME)
      character act*1,outs*248,outsd*248,WORD*20
      character itdat*248,itrep*248,ittag*12,itmenu*32
      character symname*12,othcolr*12,incolr*12,othacou*12,inacou*12
      character OTF*4,OPT*12,cnstimp*12
      character thrmtg*1,gaptg*1,mattag*12,laydesc*32

C << check ituntstr >>
C Unit strings for constructions:  ituntstr='------cccc------b-----'
      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        outs=' '
        write(outs,'(a)')itdat(1)(1:lnblnk(itdat(1)))
        write(6,*)itdat(1)
        K=0
        CALL EGETW(outs,K,symname,'-','symetr name',IER)
        CALL EGETW(outs,K,othcolr,'-','other colour name',IER)
        CALL EGETW(outs,K,incolr,'-','inside colour name',IER)
        CALL EGETW(outs,K,OTF,'-','OTF tag',IER)
        CALL EGETW(outs,K,OPT,'-','optical tag',IER)
        CALL EGETW(outs,K,othacou,'-','other acoustic name',IER)
        CALL EGETW(outs,K,inacou,'-','inside acoustic name',IER)
        CALL EGETWR(outs,K,othir,0.,1.,'-','other ir',IER)
        CALL EGETWR(outs,K,sinir,0.,0.,'-','inside ir',IER)
        CALL EGETWR(outs,K,othsol,0.,0.,'-','other solar',IER)
        CALL EGETWR(outs,K,sinsol,0.,0.,'-','inside solar',IER)
        CALL EGETW(outs,K,cnstimp,'-','constr impact name',IER)
        CALL EGETWI(outs,K,nlay,0,8,'F','layers',IER)
        if(nbitrep(iit).ne.nlay)then
          write(6,*) 'In prsmlc get mode nbitrep ',nbitrep(iit),
     &      ' ne nlay ',nlay,' for item ',iit
        endif

C Extract layer data.
        do 40 il=1,nlay
          outs=' '
          write(outs,'(a)')itrep(il)(1:lnblnk(itrep(il)))
          write(6,*)itrep(il)
          K=0
          call EGETW(outs,K,WORD,'-','therm layr tag',IER)
          thrmtg(il)=WORD(1:1)
          call EGETW(outs,K,WORD,'-','gap layr tag',IER)
          gaptg(il)=WORD(1:1)
          call EGETW(outs,K,mattag(il),'-','layr mat tag',IER)
          call EGETWR(outs,K,DT(il),0.0001,1.0,'W','thick',IER)
          call EGETWI(outs,K,IP(il),0,MGIT,'F','old index',IER)
          call EGETWR(outs,K,DRA(il,1),0.0,0.0,'-','air 1',IER)
          call EGETWR(outs,K,DRA(il,2),0.0,0.0,'-','air 2',IER)
          call EGETWR(outs,K,DRA(il,3),0.0,0.0,'-','air 3',IER)
          call egetrm(outs,K,laydesc(il),'-','lay descr',IER)
  40    continue
        if(ier.eq.0)return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then
        write(outs,'(13a,4f8.3,3a,i3)',IOSTAT=IOS,ERR=1) symname,' ',
     &    othcolr,' ',incolr,' ',OTF,' ',OPT,' ',othacou,' ',inacou,
     &    othir,sinir,othsol,sinsol,' ',cnstimp,' ',nlay
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)

C Write layer data. Note there may be spaces in laydesc so append this
C to the comma separated string.
        if(nbitrep(iit).ne.nlay)then
          write(6,*) 'In prsmlc put mode nbitrep ',nbitrep(iit),
     &      ' ne nlay ',nlay,' for item ',iit
        endif
        do 39 il=1,nlay
          outs=' '
          write(outs,'(6a,f8.5,i4,3f8.4)',IOSTAT=IOS,ERR=1) 
     &      thrmtg(il),' ',gaptg(il),' ',mattag(il),' ',
     &      DT(il),IP(il),DRA(il,1),DRA(il,2),DRA(il,3)
          call SDELIM(outs,outsd,'C',IW)
          itrep(IL)=' '
          ld=lnblnk(laydesc(il))
          lo=lnblnk(outsd)
          write(itrep(IL),'(2a)')outsd(1:lo),laydesc(il)(1:ld)
   39   continue
        if(ier.eq.0)return
      endif

   1  if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'Error decoding item:',itmenu(iit)
      else
        write(outs,'(2a)') 'Error encoding item:',itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)
      return
      end

C *************** PRSMAT
C PRSMAT gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C material db via esp-r common block gendb8b. iit the requested element,
C IER=0 OK, IER=1 problem.
C acoutag is the acoustic db item name, colormtag colour db name, 
C lcatag is the env impact db item name,
C lcatype (char) specifies material use (fixing-finish-spacer) for non-thermal
C domains and
C r1lac, r2lca are real non-thermal data attributes).
C NCODE is legacy index, E is the element emissivity, A is the solar
C absorb, DRV is the diffusion resistance, spec is specularity, rough is
C roughness, misc1 and misc2 are optional real parameters.

      subroutine prsmat(idbh,iit,act,acoutag,colormtag,lcatag,
     &  lcatype,r1lca,r2lca,NCODE,DBCON,DBDEN,DBSHT,E,A,DRV,
     &  spec,rough,misc1,misc2,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8b/itdat(20),itrep(20)

      character act*1,outs*248,outsd*248,outm*124,lcatype*1
      character itdat*248,itrep*248,ittag*12,itmenu*32
      character acoutag*12,colormtag*12,lcatag*12
      logical close1,close2,close3
      real misc1,misc2

      IER=0
      close1=.false.
      close2=.false.
      close3=.false.

      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        outs=' '
        write(outs,'(a)')itdat(1)(1:lnblnk(itdat(1)))
        write(6,*)itdat(1)
        K=0
        CALL EGETW(outs,K,acoutag,'-','accoustic name',IER)
        CALL EGETW(outs,K,colormtag,'-','colour name',IER)
        CALL EGETW(outs,K,lcatag,'W','lca mat environ name',IER)
        CALL EGETW(outs,K,lcatype,'-','lca mat type',IER)
        CALL EGETWR(outs,K,r1lca,0.00,1000.0,'W','real 1',IER)
        CALL EGETWR(outs,K,r2lca,0.00,1000.0,'W','real 2',IER)
        CALL EGETWI(outs,K,NCODE,0,600,'F','old index',IER)
        CALL EGETWR(outs,K,DBCON,0.001,300.,'W','conduct',IER)
        CALL EGETWR(outs,K,DBDEN,1.0,9000.,'W','density',IER)
        CALL EGETWR(outs,K,DBSHT,1.0,2500.,'W','specc ht',IER)
        CALL EGETWR(outs,K,E,0.01,0.99,'W','emissivity',IER)
        CALL EGETWR(outs,K,A,0.01,0.99,'W','absorptivity',IER)
        CALL EGETWR(outs,K,DRV,1.,20000.,'W','diffu resis',IER)
        CALL EGETWR(outs,K,spec,0.,2.,'W','specularity',IER)
        CALL EGETWR(outs,K,rough,0.,2.,'W','roughness',IER)
        CALL EGETWR(outs,K,misc1,0.,0.,'-','misc 1',IER)
        CALL EGETWR(outs,K,misc2,0.,0.,'-','misc 2',IER)
        CALL ECLOSE(DBCON,0.0,0.001,CLOSE1)
        CALL ECLOSE(DBDEN,0.0,0.001,CLOSE2)
        CALL ECLOSE(DBSHT,0.0,0.001,CLOSE3)
        if(CLOSE1.and.CLOSE2.and.CLOSE3)then
          write(outm,'(A,I3,3A)') ' Material db reference (',iit,
     &      ') ',ittag(iit),' has no data, or all zero...'
          call usrmsg(outm,' Please check your selection! ','W')
          ier=1
          return
        endif
        if(ier.eq.0)return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then
        write(outs,'(9a,2f8.4,I3,10F10.3)')  acoutag,' ',colormtag,' ',
     &    ' ',lcatag,' ',lcatype,' ',r1lca,r2lca,NCODE,DBCON,
     &    DBDEN,DBSHT,E,A,DRV,spec,rough,misc1,misc2
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)
        if(ier.eq.0)return
      endif

  1   if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'Error decoding material item:',
     &    itmenu(iit)
      else
        write(outs,'(2a)') 'Error encoding material item:',
     &    itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)
      return
      end

C *************** PRSCOLR
C PRSCOLR gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C colour db via esp-r common block gendb8b. iit the requested element,
C IER=0 OK, IER=1 problem.
C IR255 IG255 IB255 are red/green/blue values, greyrefl is grey
C reflectance, coltype indicates type of colour pattern, colptrn is
C a colour calculation or pattern file name.

      subroutine prscolr(idbh,iit,act,IR255,IG255,IB255,greyrefl,
     &  coltype,colptrn,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8b/itdat(20),itrep(20)

      character act*1,outs*248,outsd*248
      character itdat*248,itrep*248,ittag*12,itmenu*32
      character coltype*12,colptrn*72

      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        outs=' '
        write(outs,'(a)')itdat(1)(1:lnblnk(itdat(1)))
        write(6,*)itdat(1)
        K=0
        CALL EGETWI(outs,K,IR255,0,256,'W','red 0-255',IER)
        CALL EGETWI(outs,K,IG255,0,256,'W','green 0-255',IER)
        CALL EGETWI(outs,K,IB255,0,256,'W','blue 0-255',IER)
        CALL EGETWR(outs,K,greyrefl,0.00,1.0,'W','grey',IER)
        call egetp(outs,K,coltype,'W','colr type',IER)
        call egetrm(outs,K,colptrn,'W','colr patern',IER)
        if(ier.eq.0)return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then
        write(outs,'(1x,3i4,f8.4,3a)')  IR255,IG255,IB255,greyrefl,
     &    coltype,' ',colptrn
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)
        if(ier.eq.0)return
      endif

      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'Error decoding item:',itmenu(iit)
      else
        write(outs,'(2a)') 'Error encoding item:',itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)
      return
      end

C *************** PRSACOU
C PRSACOU gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C acoustic db via esp-r common block gendb8b. iit the requested element,
C IER=0 OK, IER=1 problem.
C atype (M or C) indicates material or construction, aunit makes explicit
C the scope of the data (per m^2 or person etc.), FRQ are the coef
C 17 different frequencies.
      subroutine prsacou(idbh,iit,act,atype,aunit,FRQ,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8b/itdat(20),itrep(20)

      dimension FRQ(17)
      character act*1,outs*248,outsd*248
      character itdat*248,itrep*248,ittag*12,itmenu*32,atype*4,aunit*8

      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        outs=' '
        write(outs,'(a)')itdat(1)(1:lnblnk(itdat(1)))
        K=0
        CALL EGETW(outs,K,atype,'-','M or C tag',IER)
        CALL EGETW(outs,K,aunit,'-','unit tag',IER)
        do 99 im=1,17
          CALL EGETWR(outs,K,FRQ(im),0.00,10.0,'W','frq coef',IER)
 99     continue
        if(ier.eq.0)return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then
        write(outs,'(a4,a8,17f7.3)') atype,aunit,(FRQ(j),j=1,17)
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)
        if(ier.eq.0)return
      endif

      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'Error decoding item:',itmenu(iit)
      else
        write(outs,'(2a)') 'Error encoding item:',itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)
      return
      end

C *************** PRSENVCI
C PRSENVCI gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C an environmental impact constructions impact db entity via esp-r
C common block gendb8b. iit the requested element, IER=0 OK, IER=1 problem.
C Used with the following category types: CONSIMP
C lassurn is name of LCA assembly name (surface related),
C laspern is name of LCA assembly name (perimeter related),
C trbsn 1st/2nd/3rd transport to building site name,
C dtrbs 1st/2nd/3rd distance to building site (1st),
C rtrbs 1st/2nd/3rd loss rate in transport to building site (1st),
C assite is name of assembly at site,
C lasmat 1st/2nd/3rd maintenance names.
      subroutine prsenvci(idbh,iit,act,lassurn,laspern,ntrn,trbsn,
     &  dtrbs,rtrbs,assite,lasmat,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8b/itdat(20),itrep(20)

      dimension trbsn(3),dtrbs(3),rtrbs(3),assite(2),lasmat(2)
      character act*1,outs*248,outsd*248,ct*8
      character itdat*248,itrep*248,ittag*12,itmenu*32
      character cattag*12,cattype*8,catmenu*32,catdate*24,catdoc*248
      character assite*12,lassurn*12,laspern*12,trbsn*12,lasmat*12

C Check that the item is in an allowable category.
C These are the units for this category:
C     ituntstr='---aa-aa-aa----' 
      ict=itcat(iit)
C debug...
C      call lstcat(6,ict,ier)
      ct=cattype(ict)
      if(ct(1:7).eq.'CONSIMP')then
        continue
      else
        call usrmsg('Cannot encode or decode items of this type.',
     &    ' ','W')
      endif
      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        outs=' '
        write(outs,'(a)')itdat(1)(1:lnblnk(itdat(1)))
        write(6,*)itdat(1)
        K=0
        CALL EGETW(outs,K,lassurn,'-','cnstr assembly surf',IER)
        CALL EGETW(outs,K,laspern,'-','cnstr assembly perm',IER)
        CALL EGETW(outs,K,trbsn(1),'-','1st transport name',IER)
        CALL EGETWR(outs,K,dtrbs(1),0.,0.,'-','1st trn dist',IER)
        CALL EGETWR(outs,K,rtrbs(1),0.,1.,'-','1st trn brk',IER)
        CALL EGETW(outs,K,trbsn(2),'-','2nd transport name',IER)
        CALL EGETWR(outs,K,dtrbs(2),0.,0.,'-','2nd trn dist',IER)
        CALL EGETWR(outs,K,rtrbs(2),0.,1.,'-','2nd trn brk',IER)
        CALL EGETW(outs,K,trbsn(3),'-','3rd transport name',IER)
        CALL EGETWR(outs,K,dtrbs(3),0.,0.,'-','3rd trn dist',IER)
        CALL EGETWR(outs,K,rtrbs(3),0.,1.,'-','3rd trn brk',IER)
        CALL EGETW(outs,K,assite(1),'-','assembly at site1',IER)
        CALL EGETW(outs,K,assite(2),'-','assembly at site2',IER)
        CALL EGETW(outs,K,lasmat(1),'-','1st maint name',IER)
        CALL EGETW(outs,K,lasmat(2),'-','2nd maint name',IER)

C Find the number of transports used.
        if(trbsn(1)(1:4).eq.'none'.and.trbsn(2)(1:4).eq.'none'.and.
     &     trbsn(3)(1:4).eq.'none')then
          ntrn=0
        elseif(trbsn(1)(1:4).ne.'none'.and.trbsn(2)(1:4).eq.'none'.and.
     &     trbsn(3)(1:4).eq.'none')then
          ntrn=1
        elseif(trbsn(1)(1:4).ne.'none'.and.trbsn(2)(1:4).ne.'none'.and.
     &     trbsn(3)(1:4).eq.'none')then
          ntrn=2
        elseif(trbsn(1)(1:4).ne.'none'.and.trbsn(2)(1:4).ne.'none'.and.
     &     trbsn(3)(1:4).ne.'none')then
          ntrn=3
        else
          ntrn=0
        endif
        if(ier.eq.0)return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then
        write(outs,'(6a,f8.2,f8.4,3a,f8.2,f8.4,2a,f8.2,f8.4,8a)',
     &    IOSTAT=IOS,ERR=1) 
     &    lassurn,' ',laspern,' ',trbsn(1),' ',dtrbs(1),
     &    rtrbs(1),' ',trbsn(2),' ',dtrbs(2),rtrbs(2), trbsn(3),
     &    ' ',dtrbs(3),rtrbs(3),' ',assite(1),' ',assite(2),' ',
     &    lasmat(1),' ',lasmat(2)
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)
        if(ier.eq.0)return
      endif

  1   if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'Error decoding item:',itmenu(iit)
      else
        write(outs,'(2a)') 'Error encoding item:',itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)
      return
      end

C *************** PRSENVI
C PRSENVI gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C an environmental impact db via esp-r common block gendb8b. iit the
C requested element, IER=0 OK, IER=1 problem.
C trbsn transport names (3), dtrbs transport distances (3),
C eimp (9 effects),ntrn is the number of transports.
C Used with the following category types: RECYCP, INCENP DUMPP
      subroutine prsenvi(idbh,iit,act,ntrn,trbsn,dtrbs,eimp,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8b/itdat(20),itrep(20)

      dimension trbsn(3),dtrbs(3),eimp(9)
      character act*1,outs*248,outsd*248,ct*8,trbsn*12
      character itdat*248,itrep*248,ittag*12,itmenu*32
      character cattag*12,cattype*8,catmenu*32,catdate*24,catdoc*248

C Check that the item is in an allowable category.
C For INCENP  ituntstr='-b-b-bcdefcccccc' 
C For DUMPP   ituntstr='-b-b-bcdefcccccc'
C For RECYCP  ituntstr='-b-b-bcdefcccccc'
      ict=itcat(iit)
C Debug...
C      call lstcat(6,ict,ier)
      ct=cattype(ict)
      if(ct(1:6).eq.'RECYCP'.or.
     &   ct(1:6).eq.'INCENP'.or.ct(1:5).eq.'DUMPP')then
        continue
      else
        call usrmsg('Cannot encode or decode items of this type.',
     &    ' ','W')
      endif
      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        outs=' '
        write(outs,'(a)')itdat(1)(1:lnblnk(itdat(1)))
        write(6,*)itdat(1)
        K=0
        CALL EGETW(outs,K,trbsn(1),'-','1st transport name',IER)
        CALL EGETWR(outs,K,dtrbs(1),0.,0.,'-','1st trn dist',IER)
        CALL EGETW(outs,K,trbsn(2),'-','2nd transport name',IER)
        CALL EGETWR(outs,K,dtrbs(2),0.,0.,'-','2nd trn dist',IER)
        CALL EGETW(outs,K,trbsn(3),'-','3rd transport name',IER)
        CALL EGETWR(outs,K,dtrbs(3),0.,0.,'-','3rd trn dist',IER)
        do 99 im=1,9
         CALL EGETWR(outs,K,eimp(im),0.00,10.0,'W','impacts',IER)
 99     continue

C Find the number of transports used.
        if(trbsn(1)(1:4).eq.'none'.and.trbsn(2)(1:4).eq.'none'.and.
     &     trbsn(3)(1:4).eq.'none')then
          ntrn=0
        elseif(trbsn(1)(1:4).ne.'none'.and.trbsn(2)(1:4).eq.'none'.and.
     &     trbsn(3)(1:4).eq.'none')then
          ntrn=1
        elseif(trbsn(1)(1:4).ne.'none'.and.trbsn(2)(1:4).ne.'none'.and.
     &     trbsn(3)(1:4).eq.'none')then
          ntrn=2
        elseif(trbsn(1)(1:4).ne.'none'.and.trbsn(2)(1:4).ne.'none'.and.
     &     trbsn(3)(1:4).ne.'none')then
          ntrn=3
        else
          ntrn=0
        endif
        if(ier.eq.0)return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then
        write(outs,'(2a,f8.2,3a,f8.2,3a,f8.2,1P9e10.3)',
     &    IOSTAT=IOS,ERR=1) 
     &    trbsn(1),' ',dtrbs(1),' ',trbsn(2),' ',dtrbs(2),' ',
     &    trbsn(3),' ',dtrbs(3),eimp(1),eimp(2),eimp(2),eimp(3),
     &    eimp(4),eimp(5),eimp(6),eimp(7),eimp(8),eimp(9)
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)
        if(ier.eq.0)return
      endif

  1   if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'Error decoding item:',itmenu(iit)
      else
        write(outs,'(2a)') 'Error encoding item:',itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)
      return
      end

C *************** PRSENVF
C PRSENVF gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C an environmental material impact db entity via esp-r
C common block gendb8b. iit the requested element, IER=0 OK, IER=1 problem.
C Used with the following category types: MATLCA
C fablif=material life, brkias=breakage during assembly,
C trbsn (transport names), dtrbs (distances), rtrbs (breakage > asmbly,
C rn=recycling name, rr=recycling rate, in= incineration name,
C ri incineration rate, dn=dump name, rd=dump rate, eimp=9 impacts.
      subroutine prsenvf(idbh,iit,act,fablif,brkias,ntrn,trbsn,dtrbs,
     &  rtrbs,rn,rr,in,ri,dn,rd,eimp,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8b/itdat(20),itrep(20)

      dimension eimp(9),trbsn(3),dtrbs(3),rtrbs(3)
      character act*1,outs*248,outsd*248,ct*8,trbsn*12
      character itdat*248,itrep*248,ittag*12,itmenu*32,rn*12,in*12,dn*12
      character cattag*12,cattype*8,catmenu*32,catdate*24,catdoc*248

C Check that the item is in an allowable category.
C Units for MATLCA are:  ituntstr='a--b--b--b-------cdef' 
      ict=itcat(iit)
C Debug...
      call lstcat(6,ict,ier)
      ct=cattype(ict)
      if(ct(1:6).eq.'MATLCA')then
        continue
      else
        call usrmsg('Cannot encode or decode items of this type.',
     &    ' ','W')
      endif
      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        outs=' '
        write(outs,'(a)')itdat(1)(1:lnblnk(itdat(1)))
        K=0
        CALL EGETWR(outs,K,fablif,0.00,1000.0,'W','life',IER)
        CALL EGETWR(outs,K,brkias,0.00,1.0,'W','brkias',IER)
        CALL EGETW(outs,K,trbsn(1),'-','1st transport name',IER)
        CALL EGETWR(outs,K,dtrbs(1),0.,0.,'-','1st trn dist',IER)
        CALL EGETWR(outs,K,rtrbs(1),0.,1.,'-','1st trn brk',IER)
        CALL EGETW(outs,K,trbsn(2),'-','2nd transport name',IER)
        CALL EGETWR(outs,K,dtrbs(2),0.,0.,'-','2nd trn dist',IER)
        CALL EGETWR(outs,K,rtrbs(2),0.,1.,'-','2nd trn brk',IER)
        CALL EGETW(outs,K,trbsn(3),'-','3rd transport name',IER)
        CALL EGETWR(outs,K,dtrbs(3),0.,0.,'-','3rd trn dist',IER)
        CALL EGETWR(outs,K,rtrbs(3),0.,1.,'-','3rd trn brk',IER)
        CALL EGETW(outs,K,rn,'-','recycling name',IER)
        CALL EGETWR(outs,K,rr,0.00,1.0,'W','recycling rt',IER)
        CALL EGETW(outs,K,in,'-','inciner name',IER)
        CALL EGETWR(outs,K,ri,0.00,1.0,'W','inciner rt',IER)
        CALL EGETW(outs,K,dn,'-','dump name',IER)
        CALL EGETWR(outs,K,rd,0.00,1.0,'W','dump rt',IER)
        do 99 im=1,9
         CALL EGETWR(outs,K,eimp(im),0.00,10.0,'W','impacts',IER)
 99     continue

C Find the number of transports used.
        if(trbsn(1)(1:4).eq.'none'.and.trbsn(2)(1:4).eq.'none'.and.
     &     trbsn(3)(1:4).eq.'none')then
          ntrn=0
        elseif(trbsn(1)(1:4).ne.'none'.and.trbsn(2)(1:4).eq.'none'.and.
     &     trbsn(3)(1:4).eq.'none')then
          ntrn=1
        elseif(trbsn(1)(1:4).ne.'none'.and.trbsn(2)(1:4).ne.'none'.and.
     &     trbsn(3)(1:4).eq.'none')then
          ntrn=2
        elseif(trbsn(1)(1:4).ne.'none'.and.trbsn(2)(1:4).ne.'none'.and.
     &     trbsn(3)(1:4).ne.'none')then
          ntrn=3
        else
          ntrn=0
        endif
        if(ier.eq.0)return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then
        write(outs,'(f8.2,f8.4,3a,f8.2,f8.4,3a,f8.2,f8.4,2a,2f8.4,
     &    3a,f8.4,3a,f8.4,3a,f8.4,1P9e10.3)') 
     &    fablif,brkias,' ',trbsn(1),' ',dtrbs(1),rtrbs(1),' ',
     &    trbsn(2),' ',dtrbs(2),rtrbs(2),trbsn(3),' ',
     &    dtrbs(3),rtrbs(3),' ',rn,' ',rr,' ',in,' ',ri,' ',dn,' ',rd,
     &    eimp(1),eimp(2),eimp(2),eimp(3),
     &    eimp(4),eimp(5),eimp(6),eimp(7),eimp(8),eimp(9)
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)
        if(ier.eq.0)return
      endif

  1   if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'Error decoding item:',itmenu(iit)
      else
        write(outs,'(2a)') 'Error encoding item:',itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)
      return
      end

C *************** PRSENVP
C PRSENVP gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C an assembly (constructions assembly or assembly at project) or
C maintenance db entity via esp-r
C common block gendb8b. iit the requested element, IER=0 OK, IER=1 problem.
C Used with the category types: ASMPRJ, CNSTRMNT, CNSTRASM
C brkasp=breakage assembly on project,eimp=9 impacts.
      subroutine prsenvp(idbh,iit,act,tag,brkasp,eimp,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8b/itdat(20),itrep(20)

      dimension eimp(9)
      character act*1,outs*248,outsd*248,ct*8,tag*1
      character itdat*248,itrep*248,ittag*12,itmenu*32
      character cattag*12,cattype*8,catmenu*32,catdate*24,catdoc*248

C Check that the item is in an allowable category.
C For ASMPRJ units are: ituntstr='--cdefccccccc' 
C For CNSTRMNT units are: ituntstr='-acdefccccccc' 
C For CNSTRASM units are: ituntstr='--cdefccccccc' 
      ict=itcat(iit)
C Debug...
C      call lstcat(6,ict,ier)
      ct=cattype(ict)
      if(ct(1:6).eq.'ASMPRJ'.or.ct(1:8).eq.'CNSTRMNT'.or.
     &   ct(1:8).eq.'CNSTRASM')then
        continue
      else
        call usrmsg('Cannot encode or decode items of this type.',
     &    ' ','W')
      endif
      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        outs=' '
        write(outs,'(a)')itdat(1)(1:lnblnk(itdat(1)))
        K=0
        CALL EGETW(outs,K,tag,'-','tag (s-p-a)',IER)
        CALL EGETWR(outs,K,brkasp,0.00,1.0,'W','brkasp',IER)
        do 99 im=1,9
         CALL EGETWR(outs,K,eimp(im),0.00,10.0,'W','impacts',IER)
 99     continue
        if(ier.eq.0)return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then
        write(outs,'(2a,f8.4,1P9e10.3)')tag,' ',brkasp,
     &    eimp(1),eimp(2),eimp(2),eimp(3),
     &    eimp(4),eimp(5),eimp(6),eimp(7),eimp(8),eimp(9)
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)
        if(ier.eq.0)return
      endif

      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'Error decoding item:',itmenu(iit)
      else
        write(outs,'(2a)') 'Error encoding item:',itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)

      return
      end

C *************** PRSTRANSP
C PRSTRANSP gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C transport db via esp-r common block gendb8b. iit the requested element,
C IER=0 OK, IER=1 problem. eimp is an array of environmental impacts:
C GWP is global warming potential, AP is acidifacation potential, PS
C is photo smog potential, UNRE is utilisation of nonrenewable energy.
C << this needs to be changed to 9 impacts >>
      subroutine prstransp(idbh,iit,act,eimp,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8b/itdat(20),itrep(20)

      dimension eimp(9)
      character act*1,outs*248,outsd*248
      character itdat*248,itrep*248,ittag*12,itmenu*32

C Unit strings for transport:  ituntstr='abcd------'
      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        do 42 i=1,9
          eimp(i)=0.0
  42    continue
        do 99 im=1,9
         CALL EGETWR(outs,K,eimp(im),-0.10,100.0,'W','impacts',IER)
 99     continue
        if(ier.eq.0)return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then
        write(outs,'(1P9e10.3)') (eimp(j),j=1,9)
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)
        if(ier.eq.0)return
      endif

      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'prstransp error decoding:',itmenu(iit)
      else
        write(outs,'(2a)') 'prstransp error encoding:',itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)
      return
      end

C *************** PRSPRES
C PRSPRES gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C wind pressure db via esp-r common block gendb8b. iit the requested element,
C IER=0 OK, IER=1 problem.
C FDAT is the pressure coefficient at 16 angles.
      subroutine prspres(idbh,iit,act,FDAT,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8b/itdat(20),itrep(20)

      dimension FDAT(16)
      character act*1,outs*248,outsd*248
      character itdat*248,itrep*248,ittag*12,itmenu*32

      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        outs=' '
        write(outs,'(a)')itdat(1)(1:lnblnk(itdat(1)))
        K=0
        do 99 im=1,16
         CALL EGETWR(outs,K,FDAT(im),-10.00,10.0,'W','coef',IER)
 99     continue
        if(ier.eq.0)return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then
        write(outs,'(16F8.3)',IOSTAT=IOS,ERR=1)(FDAT(IL),IL=1,16)
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)
        if(ier.eq.0)return
      endif

    1 if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'Error decoding item:',itmenu(iit)
      else
        write(outs,'(2a)') 'Error encoding item:',itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)
      return
      end

C *************** PRSPROF
C << this needs thinking about >>
C PRSPROF gets/decodes (act=`g`) or puts/encodes (act=`p`) one item of
C event profile db via esp-r common block gendb8b. iit the requested element,
C IER=0 OK, IER=1 problem.
C ST(72) are start times, A(72),B(72),C(72) are values
C associated with each period  C & D are extensions to the
C data model. Note: start time is held as a real number.
C DayT (8 char string) indicates the day type associated with the
C period (an extension to data model). Patr(72) is an array
C of 8 char attributes (an extension to data model which might
C point to a component or occupant type) written to itdat(2-7).

C Pack data - six lines each (1-12 1st line, 13-24 2nd line 25-35 3rd line etc.
C write day type (str 8 char) in itdat(8-13)
C write start (as real) in itdat(14-19)
C write data a in itrep(1-6)
C write data b in itrep(7-12)
C write data c in itrep(13-18)
      subroutine prsprof(idbh,iit,act,NCG,Patr,DayT,ST,A,B,C,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8b/itdat(20),itrep(20)

      dimension DayT(72),ST(72),FN(72),A(72),B(72),C(72)
      dimension Patr(72)
      character act*1,outs*248,outsd*248,ct*8
      character itdat*248,itrep*248,ittag*12,itmenu*32,DayT*8,Patr*8
      character cattag*12,cattype*8,catmenu*32,catdate*24,catdoc*248
      character ihold*72

      ict=itcat(iit)
      ct=cattype(ict)
      if(ct(1:6).eq.'CASUAL')then
        continue
      else
        call usrmsg('Cannot encode or decode items of this type.',
     &    ' ','W')
      endif

      if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then

C Get number of periods. << get day type labels from itdat(1) ? >>
        outs=' '
        write(outs,'(a)')itdat(1)(1:lnblnk(itdat(1)))
        K=0
        CALL EGETWI(outs,K,NAD,1,10,'W','nb active day types',IER)
        CALL EGETWI(outs,K,NOP,0,240,'W','nb overall periods',IER)
        CALL EGETWI(outs,K,ND1P,0,24,'W','nb dt1 periods',IER)
        CALL EGETWI(outs,K,ND2P,0,24,'W','nb dt2 periods',IER)
        CALL EGETWI(outs,K,ND3P,0,24,'W','nb dt3 periods',IER)
        CALL EGETWI(outs,K,ND4P,0,24,'W','nb dt4 periods',IER)
        CALL EGETWI(outs,K,ND5P,0,24,'W','nb dt5 periods',IER)
        CALL EGETWI(outs,K,ND6P,0,24,'W','nb dt6 periods',IER)
        CALL EGETWI(outs,K,ND7P,0,24,'W','nb dt7 periods',IER)
        CALL EGETWI(outs,K,ND8P,0,24,'W','nb dt8 periods',IER)
        CALL EGETWI(outs,K,ND9P,0,24,'W','nb dt9 periods',IER)
        CALL EGETWI(outs,K,ND10P,0,24,'W','nb dt10 periods',IER)

C If standard casual gain with repeating sets of daytype,atrib,sens,latent
C then assume there will be up to 5 per repeating line.
        if(ct(1:6).eq.'CASUAL')then
          icount=0
          icol=1
          do 42 i=1,NOP
            if(icol.eq.0)then
              icount=icount+1
              outs=' '
              write(outs,'(a)')itrep(icount)(1:lnblnk(itrep(icount)))
              K=0
            endif
            if(icol.gt.5)then
              icol=0
            else
              CALL EGETW(outs,K,DayT(i),'W','Day type',IER)
              CALL EGETW(outs,K,Patr(i),'W','period atrib',IER)
              CALL EGETWR(outs,K,ST(i),0.00,24.0,'W','start',IER)
              CALL EGETWR(outs,K,A(i),0.0,0.0,'-','1st val',IER)
              CALL EGETWR(outs,K,B(i),0.0,0.0,'-','2nd val',IER)
              C(i)=0.0
            endif
            icol=icol+1
  42      continue
        elseif(ct(1:7).eq.'3CASUAL')then

C << to be done >>
        elseif(ct(1:7).eq.'7CASUAL')then

C << to be done >>
        elseif(ct(1:8).eq.'THERMOST')then

C << to be done >>
        endif
        return
      elseif(act(1:1).eq.'P'.or.act(1:1).eq.'p')then

C Put data into various lines.
        write(outs,'(12i4)',IOSTAT=IOS,ERR=1)NAD,NOP,ND1P,ND2P,ND3P,
     &    ND4P,ND5P,ND6P,ND7P,ND8P,ND9P,ND10P
        call SDELIM(outs,outsd,'C',IW)
        itdat(1)=' '
        lo=lnblnk(outsd)
        write(itdat(1),'(a)')outsd(1:lo)

C Write out repeating information. If CASUAL then write as many as
C 5 groups per line.
        if(ct(1:6).eq.'CASUAL')then
          icount=0
          icol=1
          ihold=' '
          ix=1
          ixl=0
          outs=' '
          do 43 i=1,NOP

C Write group of fields for period i to a holding string ihold.
            write(ihold,'(4a,f8.4,a,f9.3,a,f9.3)') DayT(i),' ',
     &        Patr(i),' ',ST(i),' ',A(i),' ',B(i)
            lna=lnblnk(ihold)
            ixl=ix+lna
            write(outs(ix:ixl),'(a)')ihold(1:lna)
            ix=ix+lna+1
            icol=icol+1

C If we have reached the 5th colum or NOP period then write outs.
            if(icol.gt.5.or.i.eq.NOP)then
              icol=0
              icount=icount+1
              write(itrep(icount),'(a)') outs(1:lnblnk(outs))
              write(6,*) icount,outs(1:lnblnk(outs))
              outs=' '
            endif
  43      continue
        endif
        return
      endif

    1 if(act(1:1).eq.'G'.or.act(1:1).eq.'g')then
        write(outs,'(2a)') 'Error decoding item:',itmenu(iit)
      else
        write(outs,'(2a)') 'Error encoding item:',itmenu(iit)
      endif
      call edisp(iuout,outs)
      write(outs,'(2a)') 'in ',gendbfn(idbh)
      call edisp(iuout,outs)
      return
      end
