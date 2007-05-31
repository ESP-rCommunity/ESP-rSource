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

C This file contains source code to create and read bcf
C (building contents file) based on information extracted
C from generic databases and common blocks.

C ********** bcfwrite ********
C bcfwrite takes a list of constructions (clist) and builds
C a bcf file. ilen is the number of constructions.
      subroutine bcfwrite(ifa,file,act,ilen,clist,ier)
      INCLUDE 'building.h'
      INCLUDE 'espriou.h'
C espriou.h provides currentfile.
      INCLUDE 'esprdbfile.h'
C providing paramaters MGDB,MGCL,MGIT
      COMMON/OUTIN/IUOUT,IUIN
      common/C21/IFCFG,cfgroot,LCFGF

C Generic db commons: file log, current classifications,
C and current items then lengths of each list.
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc

C Working list of constructions and materials << >>
      dimension clist(100),mlist(100),FRQ(17),indexc(100)
      dimension thrmtg(ME),gaptg(ME),mattag(ME),DT(ME)
      dimension IP(ME),DRA(ME,3),laydesc(ME),assite(2),lasmat(2)
      dimension eimp(9),trbsn(3),dtrbs(3),rtrbs(3),teimp(9)

      character tab*1,tag*1,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character outs*124,outsd*124
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72
      character clist*12,mlist*12,cfgroot*24,LCFGF*72

      character symname*12,othcolr*12,incolr*12,othacou*12,inacou*12
      character OTF*4,OPT*12,cnstimp*12,dstmp*24
      character thrmtg*1,gaptg*1,mattag*12,laydesc*32

      character lcaitem*12
      character acouitem*12,colormitem*12
      character acoumenu*32,colormenu*32,lcamenu*32
      character lcatype*1,mmatmenu*32,db*12,act*1 

      character rn*12,in*12,dn*12,trbsn*12,trnmenu*32,file*72
      character conitem*12,coltype*12,colptrn*72,atype*4,aunit*8
      character rnmenu*32,inmenu*32,dnmenu*32
      character oacumenu*32,iacumenu*32,ocmenu*32,icmenu*32,cnstm*32
      character ctag*12,cmenu*32
      character assite*12,lassurn*12,laspern*12,lasmat*12,lasm2*32
      character lassurm*32,lasperm*32,asitm1*32,asitm2*32,lasm1*32
      character bl2*2

      logical found
      real misc1,misc2

C Setup a tab character.
C      tab=CHAR(9)
      tab=','
      bl2='  '
      projlife=80.0

      do 40 i=1,100
        mlist(i)=' '
        indexc(i)=0
  40  continue

C Open the ascii file and write headder.
      CALL EFOPSEQ(IFA,file,4,IER)
      if(ier.ne.0)return
      write(IFA,'(a)',iostat=ios,err=1) '*Project_constructions'
      write(IFA,'(3a)',iostat=ios,err=1) '*Version',tab,
     &  ' 1.19   # version'
      call dstamp(dstmp)
      write(IFA,'(3a)',iostat=ios,err=1) '*Date',tab,dstmp

      write(IFA,'(4a)',iostat=ios,err=1) '*System',tab,
     &  LCFGF(1:lnblnk(LCFGF)),' # cfg'
      write(IFA,'(7a)',iostat=ios,err=1) '*Scope',tab,'thermal',tab,
     &  'acoustic',tab,'eia'
      write(IFA,'(2a,f8.2,a)',iostat=ios,err=1) '*Lifetime',tab,
     &  projlife,' # project lifetime [yr]'

C Setup to use the constructions db.
      db='multicon_db'
      idbh=1
      IFB=igendbf(1)
      call scandbhd(IFB,idbh,ier)
      write(currentfile,'(a)') gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))

C Search each construction until name match found so that a
C list of the materials used can be built.
      iim=0
      icd=0
      do 42 icl=1,ilen
        iit=0
        do 35 it=1,nbitem(idbh)
          if(clist(icl)(1:12).eq.ittag(it)(1:12))then
            call rgenitem(IFB,idbh,it,ier)
            if(ier.ne.0)then
              call edisp(iuout,'Problem reading this item.')
            else
              iit=it
              icd=icd+1
              indexc(icd)=it
              goto 139
            endif
          endif
  35    continue

C If reached end of items and no match then return error condition.
 139    if(iit.eq.0)then
          call usrmsg('a construction not located... ',bl2,'W')
        endif

C Scan the constructions data from the common blocks.
        call rgenitem(IFB,idbh,iit,ier)
        call PRSMLC(idbh,iit,'g',symname,othcolr,incolr,OTF,OPT,
     &    othacou,inacou,othir,sinir,othsol,sinsol,cnstimp,
     &    nlay,thrmtg,gaptg,mattag,DT,IP,DRA,laydesc,ier)

C Add to list of tags (mlist) of unique materials.
        do 43 ij=1,nlay
          if(iim.eq.0)then
            iim=1
            mlist(1)=mattag(ij)
          elseif(iim.eq.1)then
            found=.false.
            iim=iim+1
            if(mattag(ij)(1:12).eq.mlist(1)(1:12))found=.true.
            if(.NOT.found)then
              mlist(iim)=mattag(ij)
            else
              iim=iim-1
            endif
          elseif(iim.gt.1)then
            found=.false.
            iim=iim+1
            do 44 ik=1,iim-1
              if(mattag(ij)(1:12).eq.mlist(ik)(1:12))found=.true.
  44        continue
            if(.NOT.found)then
              mlist(iim)=mattag(ij)
            else
              iim=iim-1
            endif
          endif
  43    continue
  42  continue

C Setup file unit numbers and hash indices for the various
C databases to be scanned.
      idbh2=2
      IFB2=igendbf(2)
      idbh5=5
      IFB5=igendbf(5)
      idbh6=6
      IFB6=igendbf(6)
      idbh8=8
      IFB8=igendbf(8)
      idbh9=9
      IFB9=igendbf(9)

C After writing number of materials and constructions, loop
C through each of the materials and extract and write data.
      write(IFA,'(2a,i4)',iostat=ios,err=1) '*Nb_mat',tab,iim
      write(IFA,'(2a,i4)',iostat=ios,err=1) '*Nb_comp',tab,icd
      write(IFA,'(a)',iostat=ios,err=1) '# ----start of materials----'
      do 142 ij=1,iim

C Get index of material then read and parse the item. If index of
C `conitem` not found then skip further parsing.
        conitem=mlist(ij)
        write(currentfile,'(a)') 
     &    gendbfn(idbh2)(1:lnblnk(gendbfn(idbh2)))
        call prsmenu(idbh2,conitem,mmatmenu,imat,imatcat)
        if(imat.eq.0)then
          write(outs,'(4a)') 'No data for (',conitem,') ',mmatmenu
          call edisp(iuout,outs)
          goto 142
        endif
        write(outs,'(2a)') 'Getting data for ',mmatmenu
        call edisp(iuout,outs)

C Read material item and recover associated text for its attributes.
        call rgenitem(IFB2,idbh2,imat,ier)
        if(ier.ne.0)then
          write(outs,'(4a)')'Corrupt data for (',conitem,') ',mmatmenu
          call edisp(iuout,outs)
          goto 142
        endif
        call PRSMAT(idbh2,imat,'g',acouitem,colormitem,lcaitem,
     &    lcatype,r1lca,r2lca,NCODE,DBCON,DBDEN,DBSHT,E,A,DRV,
     &    spec,rough,misc1,misc2,ier)
        write(currentfile,'(a)')
     &    gendbfn(idbh5)(1:lnblnk(gendbfn(idbh5)))
        call prsmenu(idbh5,acouitem,acoumenu,iacou,iacoucat)
        write(currentfile,'(a)')
     &    gendbfn(idbh6)(1:lnblnk(gendbfn(idbh6)))
        call prsmenu(idbh6,colormitem,colormenu,icolr,icolrcat)
        write(currentfile,'(a)')
     &    gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
        call prsmenu(idbh8,lcaitem,lcamenu,ilca,ilcacat)

C Recover category information for materials.
        IFB=igendbf(idbh2)
        call scandbhd(IFB,idbh2,ier)
        write(currentfile,'(a)') 
     &    gendbfn(idbh2)(1:lnblnk(gendbfn(idbh2)))

C Write material attributes.
        lcm=lnblnk(catmenu(imatcat))
        lml=lnblnk(conitem)
        lmm=lnblnk(mmatmenu)
        write(IFA,'(a)') '# '
        write(IFA,'(a)') '# material category, tag, name, attributes'
        write(IFA,'(7a)') '*Material',tab,catmenu(imatcat)(1:lcm),
     &    tab,conitem(1:lml),tab,mmatmenu(1:lmm)
        write(IFA,'(7a)',iostat=ios,err=1) '*Domains',tab,
     &    acouitem(1:lnblnk(acouitem)),
     &    tab,colormitem(1:lnblnk(colormitem)),tab,
     &    lcaitem(1:lnblnk(lcaitem))

C Recover and write other material data. If none or unknown write place holder.
        write(IFA,'(a)') 
     &  '# thermal details: tag, cond, dens, spec ht, emis abs etc.'
        write(outs,'(a,F8.2,F6.0,F7.0,F5.2,F5.2,F7.0,F6.2,F6.2,2F4.0)')
     &      '*Thermal ',DBCON,DBDEN,DBSHT,E,A,DRV,spec,
     &      rough,misc1,misc2
        call SDELIM(outs,outsd,'C',IW)
        write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))

C Recover and print colour data. If none or unknown skip writing.
        if(colormitem(1:4).eq.'none'.or.icolr.eq.0)then
          continue
        else
          continue
        endif

C Recover and print materials acoustics. If none or unknown skip writing.
        if(acouitem(1:4).eq.'none'.or.iacou.eq.0)then
          continue
        else
          continue
        endif

C Recover material effects. If none or unknown skip writing.
        if(lcaitem(1:4).eq.'none')then
          continue
          ntrn=0
        else

C Index ilca known, read item, parse data and then recover menu
C and indices of rn in dn.
          write(currentfile,'(a)') 
     &      gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
          call scandbhd(IFB8,idbh8,ier)
          write(IFA,'(a)') 
     &  '# env effects: tag, nb trn, life, breakage, misc data, impacts'
          call rgenitem(IFB8,idbh8,ilca,ier)
          call prsenvf(idbh8,ilca,'g',fablif,brkias,ntrn,trbsn,dtrbs,
     &      rtrbs,rn,rr,in,ri,dn,rd,eimp,ier)
          call prsmenu(idbh8,rn,rnmenu,irn,irncat)
          call prsmenu(idbh8,in,inmenu,iin,iincat)
          call prsmenu(idbh8,dn,dnmenu,idn,idncat)

          write(outs,'(3a,i2,f7.2,f6.3,2f8.4)',iostat=ios,err=1)
     &      '*Mat_effects ',lcaitem,' ',ntrn,fablif,brkias,r1lca,r2lca
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          write(outs,'(1P4e10.3)') eimp(1),eimp(2),eimp(3),eimp(4)
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))

C For each of the transports to site ,recover and write data.
          if(ntrn.gt.0)then
            write(currentfile,'(a)') 
     &        gendbfn(idbh9)(1:lnblnk(gendbfn(idbh9)))
            call scandbhd(IFB9,idbh9,ier)
            do 144, itr=1,ntrn
              if(itr.eq.1)write(IFA,'(a)') 
     &          '# assembly transport: tag distance, breakage, impacts'
              call prsmenu(idbh9,trbsn(itr),trnmenu,itrn,itrncat)
              if(itrn.eq.0)then
                continue
              else
                call rgenitem(IFB9,idbh9,itrn,ier)
                call prstransp(idbh9,itrn,'g',teimp,ier)
                write(outs,'(3a,f7.2,a,f6.3,1P4e10.3)',iostat=ios,err=1)
     &            '*Tran_assembly_site ',trbsn(itr),tab,dtrbs(itr),tab,
     &            rtrbs(itr),teimp(1),teimp(2),teimp(3),teimp(4)
                call SDELIM(outs,outsd,'C',IW)
                write(IFA,'(a)',iostat=ios,err=1)outsd(1:lnblnk(outsd))  
              endif
 144        continue
          endif

C Recycling: recover recycling transports and impacts and print data.
C If 'none' or not found in db then skip printing.
          if(rn(1:4).eq.'none'.or.irn.eq.0)then
            continue
            ntrn=0
          else
            write(currentfile,'(a)') 
     &        gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
            call scandbhd(IFB8,idbh8,ier)
            write(IFA,'(a)') 
     &        '# recycling: tag, no transports, rate, impacts'
            call rgenitem(IFB8,idbh8,irn,ier)
            call prsenvi(idbh8,irn,'g',ntrn,trbsn,dtrbs,eimp,ier)
            write(outs,'(2a,i3,f6.3,1P4e10.3)',iostat=ios,err=1)
     &        '*Dec_Recyc ',rn,ntrn,rr,eimp(1),eimp(2),eimp(3),eimp(4)
            call SDELIM(outs,outsd,'C',IW)
            write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))   
          endif
          if(ntrn.gt.0)then
            write(currentfile,'(a)') 
     &        gendbfn(idbh9)(1:lnblnk(gendbfn(idbh9)))
            call scandbhd(IFB9,idbh9,ier)
            do 145, itr=1,ntrn
              if(itr.eq.1)write(IFA,'(a)') 
     &          '# recycling transport: distance, rate, impacts'
              call prsmenu(idbh9,trbsn(itr),trnmenu,itrn,itrncat)
              if(itrn.eq.0)then
                continue
              else
                call rgenitem(IFB9,idbh9,itrn,ier)
                call prstransp(idbh9,itrn,'g',teimp,ier)
                write(outs,'(3a,f7.2,1P4e10.3)',iostat=ios,err=1)
     &            '*Tran_recyc ',trbsn(itr),tab,dtrbs(itr),
     &            teimp(1),teimp(2),teimp(3),teimp(4)
                call SDELIM(outs,outsd,'C',IW)
                write(IFA,'(a)',iostat=ios,err=1)outsd(1:lnblnk(outsd))  
              endif
 145        continue
          endif

C Incineration: recover and print data if it exists.
          if(in(1:4).eq.'none'.or.iin.eq.0)then
            continue
            ntrn=0
          else
            write(currentfile,'(a)') 
     &        gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
            call scandbhd(IFB8,idbh8,ier)
            write(IFA,'(a)') 
     &        '# incineration: tag, no transports, rate, impacts'
            call rgenitem(IFB8,idbh8,iin,ier)
            call prsenvi(idbh8,iin,'g',ntrn,trbsn,dtrbs,eimp,ier)
            write(outs,'(2a,i3,f6.3,1P4e10.3)',iostat=ios,err=1)
     &        '*Dec_Incin ',in,ntrn,ri,eimp(1),eimp(2),eimp(3),eimp(4)
            call SDELIM(outs,outsd,'C',IW)
            write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd)) 
          endif
          if(ntrn.gt.0)then
            write(currentfile,'(a)') 
     &        gendbfn(idbh9)(1:lnblnk(gendbfn(idbh9)))
            call scandbhd(IFB9,idbh9,ier)
            do 146, itr=1,ntrn
              call prsmenu(idbh9,trbsn(itr),trnmenu,itrn,itrncat)
              if(itrn.eq.0)then
                continue
              else
                call rgenitem(IFB9,idbh9,itrn,ier)
                call prstransp(idbh9,itrn,'g',teimp,ier)
                write(outs,'(3a,f7.2,1P4e10.3)',iostat=ios,err=1)
     &            '*Tran_Incin ',trbsn(itr),tab,dtrbs(itr),
     &            teimp(1),teimp(2),teimp(3),teimp(4)
                call SDELIM(outs,outsd,'C',IW)
                write(IFA,'(a)',iostat=ios,err=1)outsd(1:lnblnk(outsd)) 
              endif
 146        continue
          endif

C Dump: recover and print data if it exists.
          if(dn(1:4).eq.'none'.or.idn.eq.0)then
            continue
            ntrn = 0
          else
            write(currentfile,'(a)') 
     &        gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
            call scandbhd(IFB8,idbh8,ier)
            write(IFA,'(a)') '# dump: tag, no transports, rate, impacts'
            call rgenitem(IFB8,idbh9,idn,ier)
            call prsenvi(idbh8,idn,'g',ntrn,trbsn,dtrbs,eimp,ier)
            write(outs,'(2a,i3,f6.3,1P4e10.3)',iostat=ios,err=1)
     &        '*Dec_Dump ',dn,ntrn,rd,eimp(1),eimp(2),eimp(3),eimp(4)
            call SDELIM(outs,outsd,'C',IW)
            write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))  
          endif
          if(ntrn.gt.0)then
            write(currentfile,'(a)') 
     &        gendbfn(idbh9)(1:lnblnk(gendbfn(idbh9)))
            call scandbhd(IFB9,idbh9,ier)
            do 147, itr=1,ntrn
              call prsmenu(idbh9,trbsn(itr),trnmenu,itrn,itrncat)
              if(itrn.eq.0)then
                continue
              else
                call rgenitem(IFB9,idbh9,itrn,ier)
                call prstransp(idbh9,itrn,'g',teimp,ier)
                write(outs,'(3a,f7.2,1P4e10.3)',iostat=ios,err=1)
     &            '*Tran_Dump ',trbsn(itr),tab,dtrbs(itr),
     &            teimp(1),teimp(2),teimp(3),teimp(4)
                call SDELIM(outs,outsd,'C',IW)
                write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))  
              endif
 147        continue
          endif
        endif
 142  continue
      write(IFA,'(a)') '*End_Material'
      write(IFA,'(a)') '# '

C Recover and write out construction data. iit is set to ijth value
C from the indexc hash list. The pattern is to scan the database
C header just prior to each rgenitem. Note this is not needed prior
C to calls to prsmenu because it contains a scandbhd call.
      idbh=1
      IFB=igendbf(idbh)
      write(IFA,'(a)') '# ----start of constructions---'
      do 32 ij=1,icd
        iit=indexc(ij)
        write(currentfile,'(a)') gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
        call scandbhd(IFB,idbh,ier)
        call rgenitem(IFB,idbh,iit,ier)
        call PRSMLC(idbh,iit,'g',symname,othcolr,incolr,OTF,OPT,
     &    othacou,inacou,othir,sinir,othsol,sinsol,cnstimp,
     &    nlay,thrmtg,gaptg,mattag,DT,IP,DRA,laydesc,ier)
        ctag=ittag(iit)
        call prsmenu(idbh,ctag,cmenu,ic,iccat)
        write(currentfile,'(a)') 
     &    gendbfn(idbh5)(1:lnblnk(gendbfn(idbh5)))
        call prsmenu(idbh5,othacou,oacumenu,ioacou,iacoucat)
        call prsmenu(idbh5,inacou,iacumenu,iacou,iacoucat)
        write(currentfile,'(a)') 
     &    gendbfn(idbh6)(1:lnblnk(gendbfn(idbh6)))
        call prsmenu(idbh6,othcolr,ocmenu,iocolr,icolrcat)
        call prsmenu(idbh6,incolr,icmenu,icolr,icolrcat)
        write(currentfile,'(a)') 
     &    gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
        call prsmenu(idbh8,cnstimp,cnstm,icnstimp,icnstcat)
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)

        write(IFA,'(a)') '# '
        write(IFA,'(a)')
     &     '# construction: index, tag, name, OPQ/TRN, layers'
        write(IFA,'(2a,i3,7a,i2,a)',iostat=ios,err=1)'*Gen_constr',tab,
     &    iit,tab,ctag(1:lnblnk(ctag)),tab,cmenu(1:lnblnk(cmenu)),tab,
     &    OTF,tab,nlay,tab
        write(IFA,'(3a)',iostat=ios,err=1) '*Optics',tab,OPT

C Recover and print other face colour data. If none or unknown skip.
        write(IFA,'(2a)')'# other & inside face colour detail: tag,',
     &    ' red, green, blue, grey, type, pattern'
        if(othcolr(1:4).eq.'none'.or.iocolr.eq.0)then
          continue
        else
          write(currentfile,'(a)') 
     &      gendbfn(idbh6)(1:lnblnk(gendbfn(idbh6)))
          call scandbhd(IFB6,idbh6,ier)
          call rgenitem(IFB6,idbh6,iocolr,ier)
          call PRSCOLR(idbh6,iocolr,'g',IR255,IG255,IB255,greyrefl,
     &      coltype,colptrn,ier)
          write(outs,'(2a,3i7,f9.3,1x,a)',iostat=ios,err=1) 
     &      '*Colour_other ',othcolr,IR255,IG255,IB255,greyrefl,coltype
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          write(IFA,'(a)',iostat=ios,err=1) colptrn
        endif

C Recover and print inside face colour data. If none or unknown skip.
        if(incolr(1:4).eq.'none'.or.icolr.eq.0)then
          continue
        else
          write(currentfile,'(a)') 
     &      gendbfn(idbh6)(1:lnblnk(gendbfn(idbh6)))
          call scandbhd(IFB6,idbh6,ier)
          call rgenitem(IFB6,idbh6,icolr,ier)
          call PRSCOLR(idbh6,icolr,'g',IR255,IG255,IB255,greyrefl,
     &      coltype,colptrn,ier)
          write(outs,'(2a,3i7,f9.3,1x,a)',iostat=ios,err=1) 
     &      '*Colour_inside ',incolr,IR255,IG255,IB255,greyrefl,coltype
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          write(IFA,'(a)',iostat=ios,err=1) colptrn
        endif

C Recover and print other face acoustics. If none or unknown skip.
        if(othacou(1:4).eq.'none'.or.ioacou.eq.0)then
          continue
        else
          write(currentfile,'(a)') 
     &      gendbfn(idbh5)(1:lnblnk(gendbfn(idbh5)))
          call scandbhd(IFB5,idbh5,ier)
          write(IFA,'(2a)') '# other & inside face acoustic details:',
     &    ' tag, type, unit, 17 coefs.'
          call rgenitem(IFB5,idbh5,ioacou,ier)
          call PRSACOU(idbh5,ioacou,'g',atype,aunit,FRQ,ier)
          write(outs,'(6a)',iostat=ios,err=1) '*Acou_other ',othacou,
     &      ' ',atype,' ',aunit
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          write(outs,'(17f7.3)') (FRQ(ji),ji=1,17)
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
        endif

C Recover and print inside face acoustics. If none or unknown skip.
        if(inacou(1:4).eq.'none'.or.iacou.eq.0)then
          continue
        else
          write(currentfile,'(a)') 
     &      gendbfn(idbh5)(1:lnblnk(gendbfn(idbh5)))
          call scandbhd(IFB5,idbh5,ier)
          call rgenitem(IFB5,idbh5,iacou,ier)
          call PRSACOU(idbh5,iacou,'g',atype,aunit,FRQ,ier)
          write(outs,'(6a)',iostat=ios,err=1) '*Acou_inside ',inacou,
     &      ' ',atype,' ',aunit
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          write(outs,'(17f7.3)') (FRQ(ji),ji=1,17)
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
        endif

C Recover and print constructions assembly impacts and the associated 
C transports, assembly on project and maintenance data.
        write(IFA,'(a)') 
     &  '# constructions assembly details: tag, nb of assembly impacts'
        if(cnstimp(1:4).eq.'none'.or.icnstimp.eq.0)then
          continue
          ntrn=0
        else
          write(currentfile,'(a)') 
     &      gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
          call scandbhd(IFB8,idbh8,ier)
          call rgenitem(IFB8,idbh8,icnstimp,ier)
          call prsenvci(idbh8,icnstimp,'g',lassurn,laspern,ntrn,trbsn,
     &      dtrbs,rtrbs,assite,lasmat,ier)
          call prsmenu(idbh8,lassurn,lassurm,iassur,iasscat)
          call prsmenu(idbh8,laspern,lasperm,iasper,iaspcat)
          call prsmenu(idbh8,assite(1),asitm1,iasit1,iasitcat)
          call prsmenu(idbh8,assite(2),asitm2,iasit2,iasitcat)
          call prsmenu(idbh8,lasmat(1),lasm1,iasm1,iasm1cat)
          call prsmenu(idbh8,lasmat(2),lasm2,iasm2,iasm2cat)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)

C Figure out how many assembly impacts.
          nai=0
          if(lassurn(1:4).eq.'none'.or.iassur.eq.0)then
            continue
          else
            nai=nai+1
          endif
          if(laspern(1:4).eq.'none'.or.iasper.eq.0)then
            continue
          else
            nai=nai+1
          endif
          write(outs,'(2a,i2)',iostat=ios,err=1) '*Constr_assembly ',
     &      tab,nai
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))

C Write out constructions assembly impacts.
          if(lassurn(1:4).eq.'none'.or.iassur.eq.0)then
            continue
          else
            write(currentfile,'(a)') 
     &        gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
            call scandbhd(IFB8,idbh8,ier)
            call rgenitem(IFB8,idbh8,iassur,ier)
            call prsenvp(idbh8,iassur,'g',tag,brkasp,eimp,ier)
            write(outs,'(4a,f6.3,1P4e10.3)',iostat=ios,err=1) tag,tab,
     &        lassurn,tab,brkasp,eimp(1),eimp(2),eimp(3),eimp(4)
            call SDELIM(outs,outsd,'C',IW)
            write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          endif
          if(laspern(1:4).eq.'none'.or.iasper.eq.0)then
            continue
          else
            write(currentfile,'(a)') 
     &        gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
            call scandbhd(IFB8,idbh8,ier)
            call rgenitem(IFB8,idbh8,iasper,ier)
            call prsenvp(idbh8,iasper,'g',tag,brkasp,eimp,ier)
            write(outs,'(4a,f6.3,1P4e10.3)',iostat=ios,err=1) tag,tab,
     &        laspern,tab,brkasp,eimp(1),eimp(2),eimp(3),eimp(4)
            call SDELIM(outs,outsd,'C',IW)
            write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          endif

C If there are transports for construction write them out.
          if(ntrn.gt.0)then
            write(currentfile,'(a)') 
     &        gendbfn(idbh9)(1:lnblnk(gendbfn(idbh9)))
            call scandbhd(IFB9,idbh9,ier)
            do 47, itr=1,ntrn
              call prsmenu(idbh9,trbsn(itr),trnmenu,itrn,itrncat)
              if(itrn.eq.0)then
                continue
              else
                call rgenitem(IFB9,idbh9,itrn,ier)
                call prstransp(idbh9,itrn,'g',teimp,ier)
                write(outs,'(3a,f7.2,f6.3,1P4e10.3)',iostat=ios,err=1)
     &            '*Tran_constr ',trbsn(itr),tab,dtrbs(itr),rtrbs(itr),
     &            teimp(1),teimp(2),teimp(3),teimp(4)
                call SDELIM(outs,outsd,'C',IW)
                write(IFA,'(a)',iostat=ios,err=1)outsd(1:lnblnk(outsd))  
              endif
 47         continue
          endif

C Figure out how many assembly on site impacts.
          naos=0
          if(assite(1)(1:4).eq.'none'.or.iasit1.eq.0)then
            continue
          else
            naos=naos+1
          endif
          if(assite(2)(1:4).eq.'none'.or.iasit2.eq.0)then
            continue
          else
            naos=naos+1
          endif
          write(outs,'(2a,i2)',iostat=ios,err=1) '*Assembly_at_site ',
     &      tab,naos
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))

C If there are assembly at site data write them out.
          if(assite(1)(1:4).eq.'none'.or.iasit1.eq.0)then
            continue
          else
            write(currentfile,'(a)') 
     &        gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
            call scandbhd(IFB8,idbh8,ier)
            call rgenitem(IFB8,idbh8,iasit1,ier)
            call prsenvp(idbh8,iasit1,'g',tag,brkasp,eimp,ier)
            write(outs,'(2a,f6.3,1P4e10.3)',iostat=ios,err=1) 
     &        assite(1),tab,brkasp,eimp(1),eimp(2),eimp(3),eimp(4)
            call SDELIM(outs,outsd,'C',IW)
            write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          endif
          if(assite(2)(1:4).eq.'none'.or.iasit2.eq.0)then
            continue
          else
            write(currentfile,'(a)') 
     &        gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
            call scandbhd(IFB8,idbh8,ier)
            call rgenitem(IFB8,idbh8,iasit2,ier)
            call prsenvp(idbh8,iasit2,'g',tag,brkasp,eimp,ier)
            write(outs,'(2a,f6.3,1P4e10.3)',iostat=ios,err=1) 
     &        assite(2),tab,brkasp,eimp(1),eimp(2),eimp(3),eimp(4)
            call SDELIM(outs,outsd,'C',IW)
            write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          endif

C Figure out how many construction maintenance impacts.
          ncm=0
          if(lasmat(1)(1:4).eq.'none'.or.iasm1.eq.0)then
            continue
          else
            ncm=ncm+1
          endif
          if(lasmat(2)(1:4).eq.'none'.or.iasm2.eq.0)then
            continue
          else
            ncm=ncm+1
          endif
          write(outs,'(2a,i2)',iostat=ios,err=1) '*Constr_maint ',
     &      tab,ncm
          call SDELIM(outs,outsd,'C',IW)
          write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))

C Write out constructions maint. impacts. Note: brkasp in this case
C is the maint period.
          if(lasmat(1)(1:4).eq.'none'.or.iasm1.eq.0)then
            continue
          else
            write(currentfile,'(a)') 
     &        gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
            call scandbhd(IFB8,idbh8,ier)
            call rgenitem(IFB8,idbh8,iasm1,ier)
            call prsenvp(idbh8,iasm1,'g',tag,brkasp,eimp,ier)
            write(outs,'(4a,f6.3,1P4e10.3)',iostat=ios,err=1) tag,tab,
     &        lasmat(1),tab,brkasp,eimp(1),eimp(2),eimp(3),eimp(4)
            call SDELIM(outs,outsd,'C',IW)
            write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          endif
          if(lasmat(2)(1:4).eq.'none'.or.iasm2.eq.0)then
            continue
          else
            write(currentfile,'(a)') 
     &        gendbfn(idbh8)(1:lnblnk(gendbfn(idbh8)))
            call scandbhd(IFB8,idbh8,ier)
            call rgenitem(IFB8,idbh8,iasm2,ier)
            call prsenvp(idbh8,iasm2,'g',tag,brkasp,eimp,ier)
            write(outs,'(4a,f6.3,1P4e10.3)',iostat=ios,err=1) tag,tab,
     &       lasmat(2),tab,brkasp,eimp(1),eimp(2),eimp(3),eimp(4)
            call SDELIM(outs,outsd,'C',IW)
            write(IFA,'(a)',iostat=ios,err=1) outsd(1:lnblnk(outsd))
          endif
        endif

C Layer data.
        if(nlay.gt.0)then
          write(IFA,'(a)') 
     & '# layer T|S, mat category, mat tag, index, legacy index, thick'
          write(IFA,'(a)') '*Layer'
          do 29 il=1,nlay
            write(currentfile,'(a)') 
     &        gendbfn(idbh2)(1:lnblnk(gendbfn(idbh2)))
            call scandbhd(IFB2,idbh2,ier)
            call prsmenu(idbh2,mattag(il),mmatmenu,imat,imatcat)

            lcm=lnblnk(catmenu(imatcat))
            write(IFA,'(8a,i3,a,i3,a,F9.5,3a)',iostat=ios,err=1) 
     &      thrmtg(il),tab,gaptg(il),tab,catmenu(imatcat)(1:lcm),
     &        tab,mattag(il)(1:lnblnk(mattag(il))),tab,imat,tab,IP(il),
     &        tab,DT(il),tab,laydesc(il)(1:lnblnk(laydesc(il))),tab
  29      continue
        endif
  32  continue
      write(IFA,'(a)',iostat=ios,err=1) '*End_Construction'

C Close file before exiting.
      CALL ERPFREE(IFA,IOS)
      return

    1 write(outs,'(a,I6)') ' Error writing data '
      call edisp(iuout,outs)
      CALL ERPFREE(IFA,IOS)
      return
      end




