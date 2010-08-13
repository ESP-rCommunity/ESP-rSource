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

C Subroutines:
C  STNDRDS - read standards model description file
C  RUNSTD - read model details and run simulations
C  dotdatrecover - recover results and compare against known results

C ********* STNDRDS
C STNDRDS - Scan the validation models file, parse information and offer the user
C selection of or information about one or more of these.
C iexfil is the file unit number.
C exfil is the validation models file name 
      subroutine STNDRDS
#include "espriou.h"
C espriou.h provides currentfile.
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/FILEP/IFIL
      common/vldlst/vldlbl,vldfl
      character vldlbl*20,vldfl*72

C name is array of string holding *name from the file.
C inl2ngr is a pointer from the menu item back to the group index.
C group is array of strings selname(i)='UNKNOWN'holding *groups in the file.
C gitem is menu string array for high level group selection
C gh() is array of help entries.

      dimension group(15),gitem(20)
      dimension inl2ngr(50)
      character gitem*44,outstr*124,word*20
      character exfil*72,group*42,tgroup*42,key*1
      character H*72,separ*42
      logical XST
      integer irpt  ! for radio button

#ifdef OSI
      integer impx,impy,iwe
#else
      integer*8 impx,impy,iwe
#endif

C vldfl is set in startup.F
      currentfile='  '
      XST=.false.
      if(vldfl(1:2).ne.'  '.and.vldfl(1:4).ne.'UNKN')then
        INQUIRE (FILE=vldfl,EXIST=XST)
        if (.not.XST) then

C Debug.
C          write(6,*) vldfl

          call edisp(iuout,'No standards list available (file missing)')
          call edisp(iuout,'returning to main menu.')
          return
        endif
      else
        call edisp(iuout,'No standards list available (undefined)')
        call edisp(iuout,'returning to main menu.')
        return
      endif
      exfil=vldfl
      write(currentfile,'(a)') vldfl(1:lnblnk(vldfl))
      
C Open the validation models file.
      IER=0
      iexfil=IFIL+1
      CALL EFOPSEQ(iexfil,exfil,1,IER)
      if(ier.ne.0) then
        call edisp(iuout,'Cannot open standards description file')
        call edisp(iuout,'returning to main menu.')
        ier=1
        return
      endif
      
C Read the header and see if '*VALIDATION'
      CALL STRIPC(iexfil,outstr,0,ND,0,'validation header',IER)
      if(outstr(1:11).ne.'*VALIDATION')then
        call edisp(iuout,' Not a validation models file!')
        ier=1
        return
      endif
 
C ngr is the number of groups found in the file (max 15).
C ing is index of menu string to be written to. this is separate from
C     inl so that labels do not count in the generation of KEY characters.
C inl is the total number of groups and labels to display.
      ngr=0
      ing=0
      inl=0
 12   CALL STRIPC(iexfil,outstr,0,ND,0,'group id',IER)
      if(ier.ne.0)goto 901
      K=0
      CALL EGETW(outstr,K,word,'W','group id',IER)
      if(word(1:6).eq.'*group')then

C If we have less than 15 groups, increment ngr, inl, ing. Set
C KEY character using ing value, write menu string gitem and set
C pointer inl2ngr for menu line inl to ngr value.
        if(ngr.lt.15)then
          ngr=ngr+1
          inl=inl+1
          ing=ing+1
          CALL EMKEY(ing,KEY,IER)
          call egetrm(outstr,K,group(ngr),'W','group id',IER)
          write(gitem(inl),'(a1,1x,a42)') key, group(ngr)(1:42)
          inl2ngr(inl)=ngr
        else
          call edisp(iuout,' Too many groups!')
        endif
      elseif(word(1:6).eq.'*label')then

C A label included. Only increment inl and set pointer to zero.
        inl=inl+1
        separ=' '
        call egetrm(outstr,K,separ,'W','label',IER)
        ln=max(1,lnblnk(separ))
        ln=min0(ln,42)
        write(gitem(inl),'(2x,a)') separ(1:ln)
        inl2ngr(inl)=0
      endif
      
C Loop unti EOF error detected above (jumps to 901 and closes file).
      goto 12
 901  CALL ERPFREE(iexfil,ISTAT)

C Present the display, check the maximum width of gitem and
C use vwmenu or emenu depeding on graphic or text display.
 444  ICOG= -2
      gitem(inl+1)=  '  _____________________ '
      gitem(inl+2)=  '* all available models  '
      gitem(inl+3)=  '  _____________________ '
      gitem(inl+4)=  '? help                  '
      gitem(inl+5)=  '- exit validation tests '
      NCOG=inl+5

C Help @ validation list file level.
      h(1)='These validation sets are available for use in testing'
      h(2)=' '
      h(3)='If you select one of the groups, you will be presented'
      h(4)='with its members. For ASHRAE 140 use you should select '
      h(5)='all of the members of the group. '
      h(6)=' '
      h(7)='If you wish to select a subset of a group then click on'
      h(8)='the tests of interest. '
      IW=0
      do 141 ij=1,NCOG
        IW=max(lnblnk(gitem(ij)),IW)
 141  continue
 
C If in graphics mode use a variable width menu.
      if(MMOD.EQ.8)then
        impx=0
        impy=0
        iwe=iw
        CALL VWMENU('Validation Sets',gitem,NCOG,impx,impy,iwe,
     &     irpx,irpy,ICOG)
      else
        CALL EMENU('Validation Sets',gitem,NCOG,ICOG)
      endif
      
C Deal with the user selection.
      IF(ICOG.EQ.NCOG)THEN

C Exit.  Close file and return to calling menu.
        CALL ERPFREE(iexfil,ISTAT)
        return
      ELSEIF(ICOG.EQ.(NCOG-1))THEN

C Display help messages.
        CALL PHELPD('validation list menu',8,'-',0,0,IER)
      ELSEIF(ICOG.EQ.(NCOG-3))THEN

C Automatically run all of the available models.
        h(1)=' Results can be either dispayed or saved in'
        h(2)=' a text file.'
        h(3)=' Cancel to quit analysis.'
        irpt=1
        CALL EASKATOG(' Reporting to:',' ','file',
     &     'screen','cancel',' ',' ',' ',' ',irpt,3)
        if(irpt.ne.3)then
          do 200 IGRP=1,NGR
            TGROUP=group(IGRP)
            IFLG=1
            if (IGRP.eq.NGR) IFLG=-1
            call RUNSTD(exfil,TGROUP,IFLG,irpt)
 200      continue
        endif

C Select specific models from a group.
      ELSEIF(ICOG.GE.1.AND.ICOG.LT.(NCOG-4))THEN
        if(gitem(ICOG)(1:2).eq.'  ')goto 444
        if(inl2ngr(ICOG).gt.0.and.inl2ngr(ICOG).le.15)then
          igrsel=inl2ngr(ICOG)
          IFLG=0
          IRPT=0
          TGROUP=group(igrsel)
          call RUNSTD(exfil,TGROUP,IFLG,IRPT)
        endif
      endif

C Redisplay menu.
      goto 444
      end


C *********************************************************************
C RUNSTD: read model details from the specified file and group and run it
C *********************************************************************
C LVFILE - name of the file containing model details
C LGROUP - name of group
C IFLG - =1 select all models automatically
C        =0 allow user selection
C IRPT - report to file (=1) or screen (=2) or unknown (=0, will ask)
      SUBROUTINE RUNSTD(LVFILE,LGROUP,IFLG,IRPT)
#include "building.h"

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/FILEP/IFIL
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      
      
C nrecdata - for each test number of metrics to recover
C recdata - for each recovery option: minimum and maximum range and previous values
C simdata - for each recovery option current simulation result
C recdesc - for each recovery option descriptive string, data description and 
C   third slot for description used by res (read from dat file and used in 
C   reporting here.
C irecdata - zone and surface number for results recovery (=0 if all)
      parameter (MNVCASE=40,MNVOPT=3,MNVRES=40)
      common/vldstd1/nrecdata(MNVCASE),irecdata(MNVCASE,MNVRES,2)
      common/vldstd2/recdata(MNVCASE,MNVRES,3),
     &               simdata(MNVCASE,MNVOPT,MNVRES)
      common/vldstd3/recdesc(MNVCASE,MNVRES,3)
      character recdesc*24

      character name(MNVCASE)*24,stddesc(MNVCASE)*72
      dimension ncfgitem(MNVCASE)
      character cfgitem(MNVCASE,MNVOPT)*72
      character rootitem(MNVCASE,MNVOPT)*72
      character actitem(MNVCASE,MNVOPT)*72
      character recitem(MNVCASE,MNVOPT)*72
      character pathitem(MNVCASE,MNVOPT)*72
      
      dimension itemval(MNVCASE)

      character outstr*124,word*20,t24*24,t72*72
      character LVFILE*72,LGROUP*42,tgroup*42
      character H*72,outs*256,doit*300
      character xfile*144,tg*1,delim*1,t12*12
      character chkresult*8
      
      logical XST,ok,dok,CLOSE,unixok
      integer irpt  ! for radio button
      
C Debug.
C      write(6,*) 'Debug: call pars:',LVFILE,LGROUP,iflg,irpt

      call isunix(unixok)

C Initialise variables.
      IVFILE=IFIL+1
      do 10 I=1,MNVCASE
        do 20 J=1,MNVOPT
          cfgitem(I,J)=' '
          rootitem(I,J)=' '
          actitem(I,J)=' '
          recitem(I,J)=' '
          pathitem(I,J)=' '
          do 30 K=1,MNVRES
            simdata(I,J,K)=0.
            irecdata(I,K,1)=0
            irecdata(I,K,2)=0
            recdata(I,K,1)=0.
            recdata(I,K,2)=0.
            recdata(I,K,3)=0.
            recdesc(I,K,1)=' '
            recdesc(I,K,2)=' '
            recdesc(I,K,3)=' '
 30       continue
 20     continue
        ncfgitem(I)=0
        name(I)=' '
 10   continue
 
C Open file.
      call EFOPSEQ(IVFILE,LVFILE,1,IER)
      call STRIPC(IVFILE,outstr,0,ND,0,'Standards 1st line',IER)
      if (outstr(1:11).ne.'*VALIDATION') then
        call edisp(iuout,' Not a validation models file!')
        ier=1
        return
      endif

C File opened successfully.  Read contents line by line.
  7   call STRIPC(IVFILE,outstr,0,ND,0,'group id',IER)
      if(ier.ne.0)goto 100
      K=0
      call EGETW(outstr,K,word,'W','group id',IER)
      if (word(1:6).eq.'*group') then
        call egetrm(outstr,K,TGROUP,'W','group name',IER)
        IL=max(1,lnblnk(TGROUP))
        if (TGROUP(1:IL).eq.LGROUP(1:IL)) then
        
C This is our group (selected in previous menu).  Read model details.
C Reset number of models and minimum menu width.
          inm=0
          IW=12
  39      call STRIPC(IVFILE,outstr,0,ND,0,'item id',IER)
          K=0
          call EGETW(outstr,K,word,'W','member tags',IER)
          if (word(1:5).eq.'*name') then
            inm=inm+1
            call egetrm(outstr,K,name(inm),'W','test name',IER)
            IW=max(IW,lnblnk(name(inm)))
          elseif(word(1:4).eq.'*cfg')then
          
C There may be multiple cfg files associated with this test.  The data 
C in the definition file has to be in order for this to work.  Remember 
C the current cfg file definition that we are reading (icc) and store 
C the number for this model (ncfgitem(inm)).
            ncfgitem(inm)=ncfgitem(inm)+1
            icc=ncfgitem(inm)
            call egetrm(outstr,K,cfgitem(inm,icc),'W','cfg name',IER)
          elseif(word(1:5).eq.'*root')then
            call egetrm(outstr,K,rootitem(inm,icc),'W','model folder',
     &        IER)
          elseif(word(1:4).eq.'*act')then
            call egetrm(outstr,K,actitem(inm,icc),'W','act command',IER)
          elseif(word(1:4).eq.'*rec')then
            call egetrm(outstr,K,recitem(inm,icc),'W','res command',IER)
          elseif(word(1:5).eq.'*disp')then
            call egetrm(outstr,K,pathitem(inm,icc),'W','res disp',IER)
          elseif(word(1:7).eq.'*stdval')then
            call egetrm(outstr,K,stddesc(inm),'W','std value desc',IER)
          elseif(word(1:7).eq.'*metric')then

C Loop through definition of metrics to be recovered.
            IR=0
  49        call STRIPC(IVFILE,outstr,0,ND,0,'item id',IER)
            K=0
            call EGETW(outstr,K,word,'W','metric data',IER)
            if (word(1:10).ne.'*endmetric') then
             K=0
             IR=IR+1
             nrecdata(inm)=IR
             call egetwr(outstr,K,xv,0.,0.,'-','min bound',IER)
             recdata(inm,IR,1)=xv
             call egetwr(outstr,K,xv,0.,0.,'-','max bound',IER)
             recdata(inm,IR,2)=xv
             call egetw(outstr,K,t24,'W','desciption',IER)
             recdesc(inm,IR,1)=t24
             call egetw(outstr,K,t24,'W','data label',IER)
             recdesc(inm,IR,2)=t24
             call egetwi(outstr,K,iv,0,MCOM,'W','zone/all id',IER)
             irecdata(inm,IR,1)=iv
             call egetwi(outstr,K,iv,0,MS,'W','surface id',IER)
             irecdata(inm,IR,2)=iv
             call egetwr(outstr,K,xv,0.,0.,'-','std value',IER)
             recdata(inm,IR,3)=xv
             goto 49
            endif
          elseif(word(1:6).eq.'*group')then
          
C End of group - stop reading.
            goto 100
          elseif(word(1:6).eq.'*end')then
          
C End of file (hence group) - stop reading.
            goto 100
          endif
          
C Continue reading group details from file.
          goto 39
        endif
      elseif(word(1:6).eq.'*end')then
          
C End of file (hence group) - stop reading.
        goto 100
      endif
          
C Continue reading details from file - still looking for selected group.
      goto 7
      
C If we have reached the next group it is ok to present the list.
C First close file.
 100  CALL ERPFREE(IVFILE,ISTAT)
      if (INM.eq.0) then
        call usrmsg('No models found',' ','W')
        return
      endif
      
C Allow model selection if IFLG=0 otherwise all models in group will be 
C processed.
      INPIC=INM 

C Debug.
C      write(6,*) 'Debug: iflg: ',IFLG

      if (IFLG.eq.0) then
        H(1)='Select one or more tests from this group.  The selection'
        H(2)='will be simulated and the results compared to the stored '
        H(3)='results. '
        CALL EPICKS(INPIC,itemval,' ',' Which tests?',
     &            IW,INM,name,'test list',IER,3)
      endif

C Check models have been selected.
      if (INPIC.eq.0) then

C Debug.
C        write(6,*) 'Debug: no tests were selected. '
        return
      endif

C Process selected models.
      write (outs,'(2a)') 'Selected tests from group ',LGROUP
      call edisp(iuout,outs)
      do 999 I=1,inpic
        if (IFLG.eq.0) then
          imod=itemval(I)
        else
          imod=I
        endif
        write (outs,'(2a)') ' ',name(imod)
        call edisp(iuout,outs)
 999  continue

      if (IRPT.eq.0) then
        h(1)=' Results can be either dispayed or saved in'
        h(2)=' a text file.'
        h(3)=' Cancel to quit analysis.'
        irpt=1
        CALL EASKATOG(' Reporting to:',' ','file',
     &     'screen','cancel',' ',' ',' ',' ',irpt,3)
      endif
      if (irpt.eq.1) then
 9      itu=ixunit
        if(unixok)then
          write(xfile,'(a)')'/tmp/Stand_tests.txt'
        else
          write(xfile,'(a)')'Stand_tests.txt'
        endif
        call ctlexp(xfile,ixopen,ixloc,ixunit,'T','exported_res',IER)
        XST=.false.
        call findfil(xfile,XST)
        if(XST)then
          dok=.false.
          h(1)='Chance to change file name or location. '
          CALL ASKOK('File already exists.','Change name?',OK,dok,1)
          if (OK) goto 9
        endif
        
C Set irpt to -1 so that if running multiple groups this question will 
C not be asked again.
        irpt=-1
      elseif (irpt.eq.2) then
        itu=iuout                     
      elseif (irpt.eq.3) then
        return
      endif
        
C The models selected are now processed. bps (simulations) start for each 
C one of them. It reads the models configuration name and the name of the 
C simulation settings from the text file.
      do 200 ipick=1,INPIC
      
C If user selected models then selection will be held in itemval array,
C otherwise run all models.
        if (IFLG.eq.0) then
          imod=itemval(ipick)
        else
          imod=ipick
        endif

C This will write out the titles for the results.
        write (outs,'(4a)') 'Test: ',LGROUP(1:lnblnk(LGROUP)),
     &    ' - ',name(imod)
        call edisp(itu,outs)
        do 210 iopt=1,ncfgitem(imod)
          call tstamp('>','PRJ: beginning simulation')
          write (outs,'(a,1x,a)') 
     &      cfgitem(imod,iopt)(1:lnblnk(cfgitem(imod,iopt))),
     &      actitem(imod,iopt)(1:lnblnk(actitem(imod,iopt)))
          write(doit,'(2a)') 'bps -mode text -file ',outs
          call usrmsg(doit,' ','-')
          call runit(doit,'-')
          write (outs,'(2a)') doit(1:lnblnk(doit)),'...done.'
          call tstamp('>','PRJ: beginning results analysis')

C After the bps (simulation), the results recovery starts.
C It reads the models recovery file name from the text file  
          write(doit,'(2a)') 'res -mode text -file ',
     &        recitem(imod,iopt)(1:lnblnk(recitem(imod,iopt)))
          call usrmsg(outs,doit,'-')
          call runit(doit,'-')

C Now process results.
          call dotdatrecover(pathitem(imod,iopt),imod,iopt)
          
C Remove result and disp file.  First extract results file name from results 
C command (its the first word in the string recitem).
          k=0
          call egetw(recitem(imod,iopt),K,t72,'W','results file',IER)
          write(doit,'(4a)') 'rm -f ',t72(1:lnblnk(t72)),' ',
     &       pathitem(imod,iopt)(1:lnblnk(pathitem(imod,iopt)))
          call runit(doit,'-')
 210    continue
 
C Compare model variants (if any) and print results. <<  >>
C If one model print results.
C If two models then print model 2 - model 1 results.
C If three models then print model (2-1)/(3-1) results.
C Loop through output strings to find longest length.
        ltitle=18
        do 290 i=1,nrecdata(imod)
        
C Depending on the analysis type the test description will be different 
C lengths.
          if (ncfgitem(imod).eq.1) then
            ln=lnblnk(recdesc(imod,i,1))+lnblnk(recdesc(imod,i,3))
          elseif (ncfgitem(imod).eq.2) then
            ln=5+lnblnk(recdesc(imod,i,3))
          elseif (ncfgitem(imod).eq.3) then
            ln=lnblnk(recdesc(imod,i,1))+lnblnk(recdesc(imod,i,3))
          endif
          ltitle=max(ln,ltitle)
 290    continue
        write (outs,'(a)') 'Output description'
        ltitle=ltitle+3
        lend=ltitle+58
        write (outs(ltitle:lend),'(a)') 
     &    'Simulation     Range    Minimum      Maximum      Previous'
        call edisp(itu,outs)
        write (outs,'(a)') ' '
        write (outs(ltitle:lend),'(a)')
     &    '  result       check     bound        bound        result*'
        call edisp(itu,outs)    
        do 300 ichk=1,nrecdata(imod)
          if (ncfgitem(imod).eq.1) then
            xd=simdata(imod,1,ichk)
          elseif (ncfgitem(imod).eq.2) then
            xd=simdata(imod,2,ichk)-simdata(imod,1,ichk)
          elseif (ncfgitem(imod).eq.3) then

C Debug.
C            write(6,*) 'Results:',(simdata(imod,ii,ichk),ii=1,3)

            xda=simdata(imod,2,ichk)-simdata(imod,1,ichk)
            xdb=simdata(imod,3,ichk)-simdata(imod,1,ichk)
            if (abs(xda-xdb).lt.0.000001) then
              xd=1.
            else
              xd=xda/xdb
            endif
          endif
          xmin=recdata(imod,ichk,1)
          xmax=recdata(imod,ichk,2)
          
C Round results to 4 significant figures and create reporting string.
          NSIG=4
          call sigfig(xd,NSIG,RNO,t12,LSTR)
          xd=RNO
          if (xd.gt.xmax.or.xd.lt.xmin) then
            chkresult=' outside'
          else
            chkresult='  inside'
          endif
          CALL ECLOSE(xmin,xmax,0.0001,CLOSE)
          if(CLOSE) then
            chkresult='no range'
          endif
          lna=lnblnk(recdesc(imod,ichk,3))
          lnb=lnblnk(recdesc(imod,ichk,1))
          if (lna.gt.0) then
            if (ncfgitem(imod).eq.1) then
              write (outs,'(3a)') recdesc(imod,ichk,3)(1:lna),
     &                        ' ',recdesc(imod,ichk,1)(1:lnb)
            elseif (ncfgitem(imod).eq.2) then
              write (outs,'(2a)') 'delta ',recdesc(imod,ichk,1)(1:lnb)
            elseif (ncfgitem(imod).eq.3) then
              write (outs,'(3a)') recdesc(imod,ichk,3)(1:lna),
     &                        ' ',recdesc(imod,ichk,1)(1:lnb)
            endif
            lout=ltitle+1
            lend=lout+21
            write (outs(lout:lend),'(3a)') t12,' ',chkresult
            call sigfig(xmin,NSIG,RNO,t12,LSTR)
            lout=lend+2
            lend=lout+12
            write (outs(lout:lend),'(a,1x)') t12
            call sigfig(xmax,NSIG,RNO,t12,LSTR)
            lout=lend+1
            lend=lout+12
            write (outs(lout:lend),'(a,1x)') t12
            call sigfig(recdata(imod,ichk,3),NSIG,RNO,t12,LSTR)
            lout=lend+1
            lend=lout+12
            write (outs(lout:lend),'(a,1x)') t12
            call edisp(itu,outs)
          endif
 300    continue
        write (outs,'(2a)') ' *previous results from: ',stddesc(imod)
        call edisp(itu,outs)
        call edisp(itu,' --------------------------------------------')
 200  continue

C If writing to file 
      if (irpt.eq.1.or.iflg.eq.-1) CALL ERPFREE(ixunit,ISTAT)

      RETURN
      END
      
      
C *********** dotdatrecover
C dotdatrecover is used to scan the *.dat file for lines after *data
C dispfl is the file containing the results
C imod is the current case.

      subroutine dotdatrecover(dispfl,imod,iopt)
#include "espriou.h"
      common/OUTIXSTN/IUOUT,IUIN
      common/FILEP/IFIL

      parameter (MNVCASE=40,MNVOPT=3,MNVRES=40)
      common/vldstd1/nrecdata(MNVCASE),irecdata(MNVCASE,MNVRES,2)
      common/vldstd2/recdata(MNVCASE,MNVRES,3),
     &               simdata(MNVCASE,MNVOPT,MNVRES)
      common/vldstd3/recdesc(MNVCASE,MNVRES,3)
      character recdesc*24

      character outstr*124,word*24,dispfl*72
      
      dimension imet(MNVRES),icol(MNVRES)

      logical XST

C Open the recovered results file (*.dat) and read in results data.
      idispfl=IFIL+1
      XST=.false.
      if(dispfl(1:2).ne.'  '.and.dispfl(1:4).ne.'UNKN')then     
        INQUIRE (FILE=dispfl,EXIST=XST)
        if(XST)then
          CALL EFOPSEQ(idispfl,dispfl,1,IER)
          if (IER.ne.0) then
            call edisp(iuout,'Error opening results data file;')
            call edisp(iuout,' returning to main menu.')
            return
          endif
        else
          call edisp(iuout,'No results data file available;')
          call edisp(iuout,' returning to main menu.')
          return
        endif
      else
        call edisp(iuout,'Results data file name undefined;')
        call edisp(iuout,' returning to main menu.')
        return
      endif

C File opened successfully now scan contents.
      write (currentfile,'(a)') dispfl(1:lnblnk(dispfl))
 63   CALL STRIPC(idispfl,outstr,0,ND,0,'table id',IER)
 
C If something wrong or end of file return.
      if (IER.ne.0) return
      K=0
      CALL EGETW(outstr,K,word,'-','output tag',IER)
      
C Do nothing until *output tag reached.
      if(word(1:7).ne.'*output')goto 63
      
C The next word on this line should match one of the required metrics 
C in the recdesc variable.
      CALL EGETW(outstr,K,word,'-','output description',IER)

C Debug.
C      write(6,*) 'Reading table: ',word

C Loop through the recdesc strings and find the id of the metric in 
C the current output table (imet).  Note there may be one or more 
C matches here so imet is not necessarily unique - hence storing imet 
C as an array of nmet natches.
      nmet=0
      do 10 i=1,nrecdata(imod)
        lr=lnblnk(recdesc(imod,i,1))
        lw=lnblnk(word)
        if(lr.eq.lw.and.recdesc(imod,i,1)(1:lr).eq.word(1:lr))then
          nmet=nmet+1
          imet(nmet)=i

C Debug.
C          write(6,*) 'Match: ',nmet,i,recdesc(imod,i,1)

        endif
 10   continue

C Data in this table not required - read the next table.
      if (nmet.eq.0) goto 63

C Now read data for this entry.
      CALL STRIPC(idispfl,outstr,0,ND,0,'report type',IER)
      if (IER.ne.0) return
      K=0
      CALL EGETW(outstr,K,word,'-','report type tag',IER)

C Debug.
C      write(6,*) 'Reading report type: ',word
      
C For a stats report read data.
      if(word(1:6).eq.'*stats')then
 105    CALL STRIPC(idispfl,outstr,0,ND,0,'stats:title',IER)
        if (IER.ne.0) return
        K=0
        CALL EGETW(outstr,K,word,'-','title',IER)
        if (word(1:6).eq.'*title') then

C Scan the next line for the text in recdesc(imod,imet,2) for each 
C result metric required, i.e. all nmet of them.  Read the line first.
          id=0
          do 110 i=1,MNVRES
            icol(i)=0
 110      continue
          CALL STRIPC(idispfl,outstr,99,ND,0,'stats:title',IER)
          if (IER.ne.0) return
          K=0
 101      CALL EGETW(outstr,K,word,'-','title',IER)
          ln=lnblnk(word)
          id=id+1

C Debug.
C          write(6,*) 'Reading column ',id,': ',word(1:ln)
          
C Loop through all nemt here.
          do 120 im=1,nmet

C Debug.
C            write(6,*) 'Match? ',im,imet(im),recdesc(imod,imet(im),2)

            if (word(1:ln).eq.recdesc(imod,imet(im),2)(1:ln))then
              icol(im)=id

C Debug.
C              write(6,*) 'Column match',id

            endif
 120      continue
          if (id.lt.ND) goto 101

C Debug.
C          do 122 im=1,nmet
C            write(6,*) 'Identified col ',ICOL(im),' for ',
C     &        recdesc(imod,imet(im),2),' recovery.'
C 122      continue

        elseif (word(1:5).eq.'*data') then

C Scan following lines for one relating to zone irecdata(imod,imet,1), then
C read the data in the column identified above (ICOL).

C Debug.
C          write(6,*) 'data line: ',outstr(1:lnblnk(outstr))

          iv=0
          call EGETWI(outstr,K,iv,0,0,'-','iget number',IER)
          call EGETWI(outstr,K,izone,0,0,'-','zone number',IER)
          call EGETWI(outstr,K,isurf,0,0,'-','zone number',IER)
          
C Loop through all nmet possibilities.
          do 130 im=1,nmet

C Debug.
C            write(6,*) 'data zone: ',izone,irecdata(imod,imet(im),1)
C            write(6,*) 'data surf: ',isurf,irecdata(imod,imet(im),2)
          
C If zone and surface (=0 surface not important) match the requested 
C ones (in irecdata) then read the correct columns data.
            if (izone.eq.irecdata(imod,imet(im),1).and.
     &          isurf.eq.irecdata(imod,imet(im),2)) then
              CALL STRIPC(idispfl,outstr,0,ND,0,'stats:data',IER)
              if (IER.ne.0) return
              K=0

C Debug.
C              write(6,*) 'result line: ',outstr(1:lnblnk(outstr))

              do 133 ic=1,icol(im)
                CALL EGETW(outstr,K,word,'-','data',IER)
                
C Save results description string for use in reporting.
                if (ic.eq.1) then
                  recdesc(imod,imet(im),3)=word
                endif
 133          continue
              K=0
              call EGETWR(word,K,xv,0.,0.,'-','result',IER)

C Debug.
C              write(6,*) 'value: ',xv,' from column ',icol(im)

              simdata(imod,iopt,imet(im))=xv
            endif
 130      continue
        elseif (word(1:10).eq.'*end_stats') then

C Debug.
C          write(6,*) 'END of stats table '

C End of stats report move onto next output table.
          goto 63
        endif

C Loop back to read next line of stats report.
        goto 105
      
C For a table report read data.
      elseif(word(1:6).eq.'*table')then
      
C Check which column holds data.
 200    CALL STRIPC(idispfl,outstr,99,ND,0,'table',IER)
        if (IER.ne.0) return
        K=0
        CALL EGETW(outstr,K,word,'-','table:key',IER)
        if (word(1:4).eq.'*col') then

C Scan following lines for one relating to zone irecdata(imod,imet,1), then
C read the data in the column identified above (ICOL).

C Debug.
C          write(6,*) 'col line: ',outstr(1:lnblnk(outstr))

          iv=0
          call EGETWI(outstr,K,ic,0,0,'-','col number',IER)
          if (ND.gt.3) then
            call EGETWI(outstr,K,iv,0,0,'-','iget nuber',IER)
            call EGETWI(outstr,K,izone,0,0,'-','zone number',IER)
            call EGETWI(outstr,K,isurf,0,0,'-','zone number',IER)

C Loop through all nmet possibilities.
            do 230 ichk=1,nrecdata(imod)

C Debug.
C            write(6,*) 'data zone: ',izone,irecdata(imod,ichk,1)
C            write(6,*) 'data surf: ',isurf,irecdata(imod,ichk,2)

C If zone and surface (=0 surface not important) match the requested 
C ones (in irecdata) then save column number.
              if (izone.eq.irecdata(imod,ichk,1).and.
     &            isurf.eq.irecdata(imod,ichk,2)) then

C Debug.
C                write(6,*) 'col: ',ic,' for ',izone,isurf,' ichk ',ir

                icol(ichk)=ic
              endif
 230        continue
          endif
        elseif (word(1:5).eq.'*time') then

C Read data line for each time.
 250      CALL STRIPC(idispfl,outstr,99,ND,0,'table data',IER)
          if (IER.ne.0) return
          if (outstr(1:10).eq.'*end_table') then

C Debug.
C            write(6,*) 'END of table '

C End of stats report move onto next output table.
            goto 63
          endif

C Check each recovery option to see if it matches.
C          write(6,*) 'result line: ',outstr(1:lnblnk(outstr))

          do 235 ichk=1,nrecdata(imod)

C Convert key word into a real number.
            K=0
            CALL EGETWR(recdesc(imod,ichk,2),K,cv,0.,0.,'-',
     &            'std value',IER)
            K=0
            CALL EGETWR(outstr,K,time,0.,0.,'-','result time',IER)
            do 240 ic=2,ND
              CALL EGETWR(outstr,K,xv,0.,0.,'-','result value',IER)
                
C If within 5 minutes and column matches then accept match.

C Debug.
C              write(6,*) 'col:',ic,' xv:',xv
              if (abs(cv-time).lt.0.005.and.ic.eq.icol(ichk)) then

C Debug.
C                write(6,*) 'value: ',xv,' from column ',icol(ichk),cv

                simdata(imod,iopt,ichk)=xv
                write (recdesc(imod,ichk,3),'(f9.4)') cv
              endif
 240        continue
 235      continue

C Read next line from table.
          goto 250
        elseif (word(1:10).eq.'*end_table') then

C Debug.
C          write(6,*) 'end of table '

C End of stats report move onto next output table.
          goto 63
        endif

C Loop back to read next line of table report.
        goto 200
      endif

      return
      end
