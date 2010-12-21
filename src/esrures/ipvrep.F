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

C Subroutines in this file:
C  ipvrep - manage and write out an integrated performance view.
C  mkannrep - write IPV report based on description held in common.
C  ipvgtcap - recover capacity data from an intermediate report.
C  ipvgtdmd - recover demand data (per m^2) from intermediate report
C  ipvgcmft - recover comfort data from a intermediate report
C  aggreghd - hourly aggregate energy demand per unit time.
C  aggregpv - recover pv electrical output
C  ipvexp - control feedback to IPV report file or text feedback area.
C  bemsrep - write BEMS report (for IPV).

C *********** ipvrep
C Write IPV report based on description held in common.
      subroutine ipvrep(iier)
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "ipvdata.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTPCH/ICOUT
      common/outpvrb/ioutvrb
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/CLMDT1/CLMLOC
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/BL2/dmdfla(7)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C Comfort parameters and hash table.
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset,ioucpz(MCOM,72)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

      COMMON/GETPIK/NGET,IGETNO(MZS,9)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)

C ihflag = 0 write 13h30, ihflag = 1 write 0.5625
C idhflg = 0 no day demarcations, idhflg = 1 write demarcation
C          between tabular reporting days.
C ilflag = 0 tabular labels on multi-lines, ilflag = 1 on one line.
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

C IPV description.
      common/IPVF/lipvdatf

C << is pvdayhr useful? >>
      common/spm55/pvdayhr(2*MTS)
      common/ract/paction
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

C Pre calculated data (filtered).
      common/MMAFDAT/IFILT,RDFMAX(MCOM,3,MZRL),RDFMIN(MCOM,3,MZRL),
     &    RDFAVE(MCOM,3,MZRL),RDTFMAX(MCOM,3,MZRL),RDTFMIN(MCOM,3,MZRL)
      
      dimension ITEM(13),edput(7,MTS),ddmax(7),ddmin(7),dtot(7),dave(7)
      dimension XMAX(7),XMIN(7),XAVE(7),XTOT(7),DMAX(7),DMIN(7),XMSUM(7)
      dimension XMINSUM(7)
      character tab*1,outs*124,LTIME*5,outsn*124,outsd*124,outsc*124
      character outsl*248,outlsd*248,outlsc*248
      character xfile*144,tg*1,delim*1,H*72,ITEM*18,lipvdatf*72
      character SUFFIX*8,paction*16
      CHARACTER ltmp*72,LJTIME*8
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      CHARACTER selres*72,CLMLOC*30
      character descr*7,descrst*10,descrfn*10,dstmp*24,sq*1
      character ipvaction*3  ! to signal external IPV file

      logical XST,OK,DOK,pvok,usermode,within,dmdsok
      LOGICAL spfileok,perok,tstepok,saveok,autook,exitok,startupok
      integer IWM  ! for radio button
      integer NITMS,INO ! max items and current menu item

C Remember the initial periods.
      jiod1=IOD1
      jiom1=IOM1
      jioh1=IOH1
      jiod2=IOD2
      jiom2=IOM2
      jioh2=IOH2
      jiods=IODS
      jiodf=IODF
      jnout=NOUT
      jiav=IAV
      pvok=.false.
      dmdsok=.false.

C Make up a single quote for IPV
      sq = char(39)

      if(NTS.gt.1)then
        NOUT=NTS
        IAV=1
        call edisp(iuout,'Ouput reset to give hourly averaged report.')
      endif


C Read in the existing IPV description.
      if(.NOT.autook)then
        if(icfgv.lt.4)then
          h(1)='This file holds a description of an Integrated'
          h(2)='Performance View of this model, including periods,'
          h(3)='climatic dd information and assiciated zones.'
          ltmp=lipvdatf
          CALL EASKS(ltmp,'IPV description file ?',
     &      ' ',72,'xxx.ipv','IPV file',IER,3)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            lipvdatf=ltmp
          endif
        endif
      endif

C Integrated Performance View. If compact only print description,
C if verbose print periods, seasons and ratios as well.
      if(icfgv.lt.4)then
        if(lnblnk(lipvdatf).eq.0)then
          call usrmsg('Zero length IPV description: ',lipvdatf,'W')
          return
        elseif(lipvdatf(1:7).eq.'UNKNOWN')then
          call usrmsg('UNKNOWN IPV description: ',lipvdatf,'W')
          return
        else
          call FINDFIL(lipvdatf,XST)
          IF(XST)then
            ipvaction='ipv'
            call ripvdat(ifil+1,lipvdatf,ipvaction,ieripv)
            call edisp(itu,' ')
          else
            call usrmsg('Could not find IPV description: ',lipvdatf,
     &        'W')
            return
          endif
        endif
      else

C Check if embedded within configuration file.
        if(nipvassmt.eq.0)then ! there was nothing in the cfg file.
          call usrmsg('No assessments in IPV description: ',' ','W')
          return
        endif
      endif

C Read project demands file if it exists. Note that erbdmd does
C not know about idmdinc() so this needs to be applied.
      IUO=IFIL+1
      call FINDFIL(bdmds,XST)
      IF(XST)THEN
        CALL ERPFREE(IUO,ISTAT)
        CALL ERBDMD(0,IUO,IER)
        dmdsok=.true.
      else
        dmdsok=.false.
      endif

C If a command line action passed, set local parameters, if simulation
C parameter sets known then check for seasonal ipv report file name.
      ioutvrb = 1
      delim = 'C'
      tab=','
      if(paction(1:8).eq.'ipv_win '.or.paction(1:8).eq.'ipv_win1'.or.
     &   paction(1:8).eq.'ipv_ann ')then
        iseas = 1
        if(nsset.ge.1)then
          write(xfile,'(a)') sipvres(1:lnblnk(sipvres))
        endif
        call ipvexp(xfile,ixopen,ixunit,'o','T','IPV intermediate',IER)

C If there was a cancel request return, otherwise go on to label 49.
        if(ier.eq.-3)then
          return
        else
          usermode=.false.
          goto 49
        endif
      elseif(paction(1:7).eq.'ipv_trn'.or.
     &       paction(1:7).eq.'ipv_spr')then

C In this case the data will be appended to an earlier file. 
        iseas = 2
        if(nsset.ge.1)then
          write(xfile,'(a)') sipvres(1:lnblnk(sipvres))
        endif
        call ipvexp(xfile,ixopen,ixunit,'a','T','IPV reopened',IER)
        if(ier.eq.-3)then
          return
        else
          usermode=.false.
          goto 49
        endif
      elseif(paction(1:7).eq.'ipv_sum')then
        iseas = 3
        if(nsset.ge.1)then
          write(xfile,'(a)') sipvres(1:lnblnk(sipvres))
        endif
        call ipvexp(xfile,ixopen,ixunit,'a','T','IPV sum',IER)
        if(ier.eq.-3)then
          return
        else
          usermode=.false.
          goto 49
        endif
      elseif(paction(1:7).eq.'ipv_aut')then
        iseas = 4
        if(nsset.ge.1)then
          write(xfile,'(a)') sipvres(1:lnblnk(sipvres))
        endif
        call ipvexp(xfile,ixopen,ixunit,'a','T','IPV sum',IER)
        if(ier.eq.-3)then
          return
        else
          usermode=.false.
          goto 49
        endif
      elseif(paction(1:8).eq.'ipv_win2')then
        iseas = 5
        if(nsset.ge.1)then
          write(xfile,'(a)') sipvres(1:lnblnk(sipvres))
        endif
        call ipvexp(xfile,ixopen,ixunit,'a','T','IPV sum',IER)
        if(ier.eq.-3)then
          return
        else
          usermode=.false.
          goto 49
        endif
      elseif(paction(1:10).eq.'ipv_annual')then
        delim = 'C'
        call mkannrep(iier)
        return
      else

C start with normal output verbosity and winter season.
        iseas = 1
        tab=' '
        ioutvrb = 2
      endif

C Interactive commands.
  3   if(iseas.eq.1)then
        ITEM(1)= 'a Period >> winter'
      elseif(iseas.eq.2)then
        ITEM(1)= 'a Period >> spring'
      elseif(iseas.eq.3)then
        ITEM(1)= 'a Period >> summer'
      elseif(iseas.eq.4)then
        ITEM(1)= 'a Period >> autumn'
      elseif(iseas.eq.5)then
        ITEM(1)= 'a Period >> winter'
      endif
      ITEM(2)   ='  ______________  '
      ITEM(3)   ='c generate report '
      ITEM(4)   ='d edit report '
      ITEM(5)   ='e merge reports '
      ITEM(6)   ='  ______________  '
      ITEM(7)   ='  Formatting...   '
      if(ixopen.eq.1)then
        ITEM(8)= '> Output >> file  '
      elseif(ixopen.eq.0)then
        ITEM(8)= '> Output >> screen'
      endif
      if(delim.eq.'-')then
        ITEM(9)= '^ Delim >> normal '
      elseif(delim.eq.'T')then
        ITEM(9)= '^ Delim >> TAB    '
      elseif(delim.eq.'C')then
        ITEM(9)= '^ Delim >> comma  '
      elseif(delim.eq.'S')then
        ITEM(9)= '^ Delim >> space  '
      elseif(delim.eq.'X')then
        ITEM(9)= '^ delim >> tagged '
      endif
      write(item(10),'(a,i2)') '* Output @ ',NOUT
      if(IAV.eq.0)then
        item(11)='! Output not avgd '
      elseif(IAV.eq.1)then
        item(11)='! Output averaged '
      endif
      ITEM(12)  ='? Help            '
      ITEM(13)  ='- Exit            '
      NITMS=13

C Instanciate h() strings for this menu.
      h(1) ='This facility is under development.'
      h(2) =' '
      h(3) ='Produces seasonal and annual integrated performance'
      h(4) ='views (IPV) of the current model based on data'
      h(5) ='defined in an IPV definition.  To generate a report'
      h(6) ='for each season, toggle to the required season'
      h(7) ='(which matches the current results database) and'
      h(8) ='direct report to feedback or a file.  Also set the'
      h(9) ='delimeter depending on the use of the report. Use'
      h(10)='a comma or tab if exporting.'
      h(11)=' '
      h(12)='Remember to toggle to the relevant season before'
      h(13)='generating a seasonal report.'
      h(14)=' '
      h(15)='To generate an annual report which includes scaled'
      h(16)='data and emissions etc., supply the file names for'
      h(17)='each seasonal report.'

      INO=-2
   4  CALL EMENU('IPV Output',ITEM,NITMS,INO)

      if(INO.eq.NITMS)then

C Remember the initial periods.
        iod1=JIOD1
        iom1=JIOM1
        ioh1=JIOH1
        iod2=JIOD2
        iom2=JIOM2
        ioh2=JIOH2
        iods=JIODS
        iodf=JIODF
        nout=JNOUT
        iav=JIAV
        NZ=NCOMP
        do 9 im=1,NCOMP
          NZNO(im)=im
  9     continue
        call usrmsg('Resetting the display period and zone list',
     &    '(which have altered for IPV reporting).','-')
        return
      elseif(INO.eq.NITMS-1)then
        CALL PHELPD('ipv reporting',17,'-',0,0,IER)
      elseif(INO.eq.NITMS-2)then

C Toggle averaging.
        if(NOUT.eq.1)then
          IAV=0
        else
          dok=.true.
          h(1)='Typically, information included in an Integreated'
          h(2)='Performance View is integrated to once per hour if'
          h(3)='the simulation was undertaken at a higher frequency.'
          h(4)=' '
          h(5)='Some `agents` which display IPV data are unable to'
          h(6)='work with high frequency data.'
          call askok(' ',
     &      ' Average results over each output interval?',ok,dok,6)
          if(ok)then
            IAV=1
          else
            IAV=0
          endif
        endif
      elseif(INO.eq.NITMS-3)then
        CALL EASKI(NOUT,' ',' Output time-step increment ?',
     &   1,'F',60,'F',NTS,'output time-step',IER,17)
      elseif(INO.eq.NITMS-4)then

C Toggle delimeter.
        H(1) ='Tabular data can be sent to file with various'
        H(2) ='delimiters: '
        H(3) ='  spaces (format using spaces to lineup columns)'
        H(4) ='  single space between columns'
        H(5) ='  comma separator (for excel)'
        H(6) ='  tab separator (for excel)'
        H(7) ='  tagged - marked up data'
        IWM=1
        CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &    'normal spaces','single space','tab','comma','tagged',
     &    'continue',' ',IWM,7)
        if(iwm.eq.1)then
          delim = '-'
          tab=' '
        elseif(iwm.eq.2)then
          delim = 'S'
          tab=' '
        elseif(iwm.eq.3)then
          delim = 'T'
          tab=CHAR(9)
        elseif(iwm.eq.4)then
          delim = 'C'
          tab=','
        elseif(iwm.eq.5)then
          delim = 'X'
          tab=','
        endif
      elseif(INO.eq.NITMS-5)then

C Toggle between writing to export or to screen...
        call ctlexp(xfile,ixopen,ixunit,ixpunit,'T','Tabular',IER)
      elseif(INO.eq.1)then
        iseas = iseas + 1
        if(iseas.gt.5)iseas=1
      elseif(INO.eq.3)then
        usermode=.true.
        goto 49
      elseif(INO.eq.5)then

C Merge the seasonal reports into an annual report.
        call mkannrep(iier)
      else
        INO=-4
        GOTO 4
      endif
      INO=-4
      GOTO 3

C Silent generation of IPV output. If redirection to file inform user
C and adjust output channel.
  49  continue
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        outs = 'IPV data >> to file...'
        call usrmsg(outs,' ','-')
      endif

C If IPV report format has not been set ask the user (i.e. zero value).
      if(ipvform.eq.0)then
        h(1)='Text is human readable, comman separated is for use in'
        h(2)='templates and java is for an interactive agent.'
        CALL EASKABC('Format output for:',' ','Text','comma separated',
     &    'java',ipvform,2)
      endif

C If text output the set tab to a space and delimeter to -.
      if(ipvform.eq.1)then
        delim = '-'
        tab=' '
      else
        delim = 'C'
        tab=','
      endif


C If the first or only season then write out a heading.
      if(iseas.eq.1)then
        if(ipvform.eq.1.or.ipvform.eq.2)then
          WRITE(outs,'(2A,i1,a,i1)',IOSTAT=IOS,ERR=1) '*IPV_REPORT',
     &      tab,ipvversion,tab,ipvform
          call edisp(itru,outs)
          if(icfgv.lt.4)then
            call edisp(itru,lipvdatf)
          else
            call edisp(itru,'defined_within_cfg_file')
          endif
        elseif(ipvform.eq.3)then
          if(icfgv.lt.4)then
            WRITE(outs,'(3A)',IOSTAT=IOS,ERR=1) '*IPV_REPORT',tab,
     &        lipvdatf(1:lnblnk(lipvdatf))
          else
            WRITE(outs,'(3A)',IOSTAT=IOS,ERR=1) '*IPV_REPORT',tab,
     &        'defined_within_cfg_file'
          endif
          call edisp(itru,outs)
        endif
        call edisp(itru,' ')

        write(outs,'(3a)') '*title',tab,ipvtitl
        call edisp(itru,outs)
        write(outs,'(3a)') '*version',tab,ipvvers
        call edisp(itru,outs)
        call dstamp(dstmp)
        write(outs,'(3a)') '*date',tab,dstmp
        call edisp(itru,outs)

C If java then include fields for `project contact` and `site location`
        if(ipvform.eq.3)then
          write(outs,'(3a)') '*contact',tab,'See synopsis'
          call edisp(itru,outs)
          write(outs,'(3a)') '*location',tab,'See synopsis'
          call edisp(itru,outs)
        endif

C Open the climate file and get its location name.
        IUNITC=IFIL+5
        CALL FPRAND(IUNITC,ISTAT,144,1,LCLIM)
        IRECC=367
        READ(IUNITC,REC=IRECC,IOSTAT=IOS,ERR=102)CLMLOC
        CALL  ERPFREE(IUNITC,ISTAT)
        write(outs,'(a,a1,a)') '*climate',tab,CLMLOC
        call edisp(itru,outs)
        write(outs,'(a,a1,f6.2)') '*latitude',tab,sitelat
        call edisp(itru,outs)
        write(outs,'(a,a1,f6.2)') '*longitude',tab,sitelongdif
        call edisp(itru,outs)

C Write synopsis out as one long string.
        if(itru.eq.iuout)then
          write(outs,'(a,a1,a)') '*synopsis',tab,ipvsynop(1:96)
          call edisp(itru,outs)
        else
          write(itru,'(a,a1,a)') '*synopsis',tab,
     &      ipvsynop(1:lnblnk(ipvsynop))
        endif
        call edisp(itru,' ')

        WRITE(outs,'(2A,i3)',IOSTAT=IOS,ERR=1)'*simulations',tab,
     &    nipvassmt
        call edisp(itru,outs)
        call edisp(itru,'# start & finish days for each season')
        do 44 ij=1,nipvassmt
          call stdate(iyear,ipvastjd(ij),descr,descrst)
          call stdate(iyear,ipvafnjd(ij),descr,descrfn)
          WRITE(outs,'(2a,i4,3a,i4,2a)',IOSTAT=IOS,ERR=1)
     &      descrst,' ',iyear,tab,descrfn,' ',iyear,
     &      tab,ipvadesc(ij)
          call edisp(itru,outs)
 44     continue
        call edisp(itru,'  ')
        WRITE(outs,'(A,i3)',IOSTAT=IOS,ERR=1)'*days',nipvdispjd
        call eddisp(itru,outs)
        call edisp(itru,'# list of days with detailed information')
        if(nipvdispjd.ge.1)then
          do 944 L=1,nipvdispjd
            call stdate(iyear,ipvdispjd(L),descr,descrst)
            WRITE(outs,'(2a,i4)',IOSTAT=IOS,ERR=1) 
     &        descrst,' ',iyear
            call edisp(itru,outs)
  944     continue
        endif
      endif

C If the first or only season then write concept of group here.
C Loop first through any demand sets and then loop through any metrics.
      if(iseas.eq.1)then
        call edisp(itru,'  ')
        WRITE(outs,'(2A,i2)',IOSTAT=IOS,ERR=1)'*groups',tab,neds+nms
        call edisp(itru,outs)
        call edisp(itru,'# group name & number of zones & zone names')
        if(neds.gt.0)then
          do 27 ij=1,neds
            if(nzedg(ij).le.9)then
              write(outsn,'(2a,i1)')zedsdoc(ij)(1:lnblnk(zedsdoc(ij))),
     &          tab,nzedg(ij)
            else
              write(outsn,'(2a,i2)')zedsdoc(ij)(1:lnblnk(zedsdoc(ij))),
     &          tab,nzedg(ij)
            endif

C Use logic similar to ZNLIST in esrures/utils.F
C Use longer outs string length in case of longer list.
            length=0
            do 242 ik=1,nzedg(ij)
              lna=lnzname(izedg(ij,ik))
              length=length+lna+1
 242        continue
            if(length.lt.236)then
              WRITE(outlsc,5,iostat=ios,err=1)
     &         (zname(izedg(ij,ik)),ik=1,nzedg(ij))
    5         FORMAT(20(a12,' '))
              call sdelim(outlsc,outlsd,'C',IW)
              WRITE(outsl,'(3A)',IOSTAT=IOS,ERR=1)
     &          outsn(1:lnblnk(outsn)),tab,outlsd(1:lnblnk(outlsd))
              call edisp(itru,outsl)
            endif
  27      continue
        endif
        if(nms.gt.0)then

C Use longer string length in case of many zones.
          do 127 ij=1,nms
            write(outsn,'(2a,i2)')metgroup(ij)(1:lnblnk(metgroup(ij))),
     &        tab,nzmg(ij)
            length=0
            do 142 ik=1,nzmg(ij)
              lna=lnzname(izmg(ij,ik))
              length=length+lna+1
 142        continue
            if(length.lt.236)then
              WRITE(outlsc,5,iostat=ios,err=1)
     &         (zname(izmg(ij,ik)),ik=1,nzmg(ij))
              call sdelim(outlsc,outlsd,'C',IW)
              WRITE(outsl,'(3A)',IOSTAT=IOS,ERR=1)
     &          outsn(1:lnblnk(outsn)),tab,outlsd(1:lnblnk(outlsd))
              call edisp(itru,outsl)
            endif
 127      continue
        endif
        call edisp(itru,'*end_heading')
      endif

C Depending on the current season, write a tag that will give
C later data scanning code a point to look for.
      call edisp(itru,' ')
      WRITE(outs,'(2A,i2,2a)',IOSTAT=IOS,ERR=1)'*assessment',tab,
     &  iseas,tab,ipvadesc(iseas)
      call edisp(itru,outs)
      IOH1=1
      IOH2=24
      call EDAYR(ipvastjd(iseas),IOD1,IOM1)
      call EDAYR(ipvafnjd(iseas),IOD2,IOM2)
      CALL EDAY(IOD1,IOM1,IODS)
      CALL EDAY(IOD2,IOM2,IODF)

C Loop through each of the current demand sets.
      do 888 ij=1,neds

C Maximum Capacity.
C Do capacity calculations. Start with first set of demand/capacity zones.
C If no zones in first set then skip to 2nd set.
        THE=0.
        TCE=0.
        tuclight=0.0
        tlight=0.0
        tpumps=0.
        tsmlpw=0.0
        tdhw=0.
        ISET=ISIM

C Clear storage arrays.
        do 78 im=1,7
          XMAX(im)=0.0
          XMIN(im)=0.0
          XAVE(im)=0.0
          XTOT(im)=0.0
          DMAX(im)=0.0
          DMIN(im)=0.0
          XMSUM(im)=0.0
          XMINSUM(im)=0.0
  78    continue
        NZ=nzedg(ij)

C Setup recovery options for heating then cooling and lighting
C capacity. If no zones, write out place holder for heating and cooling.
        if(NZ.eq.0)then

C If no zones, continue with the storage arrays at zero.
          continue
        else
          NGET=nzedg(ij)
          DO 50 I=1,NZ
            NZNO(I)=izedg(ij,I)
            IGETNO(I,1)=8
            IGETNO(I,2)=izedg(ij,I)
            IGETNO(I,3)=0
            IGETNO(I,4)=0
            IGETNO(I,5)=ISET
            SLABEL(I)='Heating flux injection (W)'
            LNSLABEL(I)=26
   50     CONTINUE
          CALL MOSMSD('i',XMAX(1),XMIN(1),XAVE(1),XTOT(1),DMAX(1),
     &      DMIN(1),XMSUM(1),XMINSUM(1))
          THE=XTOT(1)

C In the case of cooling, the values in the reports below for array
C element 2 will use the XMIN(2) etc. rather than XMAX(2) so that
C cooling is reported as positive values.
          DO 51 I=1,NZ
            IGETNO(I,1)=9
            IGETNO(I,2)=izedg(ij,I)
            SLABEL(I)='Cooling flux (W)'
            LNSLABEL(I)=16
   51     CONTINUE
          CALL MOSMSD('i',XMAX(2),XMIN(2),XAVE(2),XTOT(2),DMAX(2),
     &      DMIN(2),XMSUM(2),XMINSUM(2))
          TCE=XTOT(2)

C Get Max Min lighting casual gains and then integrated lighting.
          DO 52 I=1,NZ
            IGETNO(I,1)=15
            IGETNO(I,3)=5
            SLABEL(I)='Lighting (unctld) gains (W)'
            LNSLABEL(I)=27
   52     CONTINUE
          CALL MOSMSD('i',XMAX(3),XMIN(3),XAVE(3),XTOT(3),DMAX(3),
     &      DMIN(3),XMSUM(3),XMINSUM(3))
          tuclight=XTOT(3)/1000.

          DO 53 I=1,NZ
            IGETNO(I,1)=15
            IGETNO(I,3)=7
            SLABEL(I)='Lighting (ctld) gains (W)'
            LNSLABEL(I)=25
   53     CONTINUE
          CALL MOSMSD('i',XMAX(4),XMIN(4),XAVE(4),XTOT(4),DMAX(4),
     &      DMIN(4),XMSUM(4),XMINSUM(4))
          tlight=XTOT(4)/1000.

C Use the (5) part of arrays for fans and pumps.

          DO 54 I=1,NZ
            IGETNO(I,1)=15
            IGETNO(I,3)=6
            SLABEL(I)='Small Power (W)'
            LNSLABEL(I)=15
   54     CONTINUE
          CALL MOSMSD('i',XMAX(6),XMIN(6),XAVE(6),XTOT(6),DMAX(6),
     &      DMIN(6),XMSUM(6),XMINSUM(6))
          tsmlpw=XTOT(6)/1000.

C Use the (7) part of arrays for fans and pumps.
        endif

C First write diversified total. The IGET number is fixed at 70.
        call edisp(itru,' ')
        write(outs,'(9a)') '*report',tab,'70',tab,'diversified',
     &    tab,'capacity',tab,zedsdoc(ij)(1:lnblnk(zedsdoc(ij)))
        call edisp(itru,outs)
        write(outs,'(5a)') '*title',tab,'Diversified capacity',tab,'W'
        call edisp(itru,outs)
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting (unctld)',tab,
     &    'Lighting (ctld)',tab,'Fans',tab,'Small Power',tab,
     &    'Hot water'
        call edisp(itru,outs)
        write(outs,'(a,7F12.1)') '*data ',DMAX(1)*1000.0,
     &    abs(DMIN(2)*1000.0),DMAX(3),DMAX(4),DMAX(5),DMAX(6),DMAX(7)
        call eddisp(itru,outs)
        call edisp(itru,'*end_report')
        call edisp(itru,' ')

C Write diffuse total. The IGET number is fixed at 70.
        write(outs,'(9a)') '*report',tab,'70',tab,'diffuse',
     &    tab,'capacity',tab,zedsdoc(ij)(1:lnblnk(zedsdoc(ij)))
        call edisp(itru,outs)
        write(outs,'(5a)') '*title',tab,'Diffuse capacity',tab,'W'
        call edisp(itru,outs)
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting (unctld)',tab,
     &    'Lighting (ctld)',tab,'Fans',tab,'Small Power',tab,
     &    'Hot water'
        call edisp(itru,outs)
        write(outs,'(a,7F12.1)')'*data ',XMSUM(1)*1000.0,
     &    abs(XMINSUM(2)*1000.0),XMSUM(3),XMSUM(4),XMSUM(5),XMSUM(6),
     &    XMSUM(7)
        call eddisp(itru,outs)
        call edisp(itru,'*end_report')
        call edisp(itru,' ')

C Write maximum in any one zone. The IGET number is fixed at 70.
        write(outs,'(9a)') '*report',tab,'70',tab,'in_one_zone',
     &    tab,'capacity',tab,zedsdoc(ij)(1:lnblnk(zedsdoc(ij)))
        call edisp(itru,outs)
        write(outs,'(5a)') '*title',tab,
     &    'Maximum in any one zone capacity',tab,'W'
        call edisp(itru,outs)
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting (unctld)',tab,
     &    'Lighting (ctld)',tab,'Fans',tab,'Small Power',tab,
     &    'Hot water'
        call edisp(itru,outs)
        write(outs,'(a,7F12.1)')'*data ',XMAX(1)*1000.0,
     &    abs(XMIN(2)*1000.0),XMAX(3),XMAX(4),XMAX(5),XMAX(6),XMAX(7)
        call eddisp(itru,outs)
        call edisp(itru,'*end_report')
        call edisp(itru,' ')

C Write minimum capacity. The IGET number is fixed at 70.
C Note: the diversified minimums are reports.
        write(outs,'(9a)') '*report',tab,'70',tab,'minimum',
     &    tab,'capacity',tab,zedsdoc(ij)(1:lnblnk(zedsdoc(ij)))
        call edisp(itru,outs)
        write(outs,'(5a)') '*title',tab,
     &    'Minimum capacity (diversified)',tab,'W'
        call edisp(itru,outs)
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting (unctld)',tab,
     &    'Lighting (ctld)',tab,'Fans',tab,'Small Power',tab,
     &    'Hot water'
        call edisp(itru,outs)
        write(outs,'(a,7F12.1)')'*data ',DMIN(1)*1000.0,
     &    abs(DMAX(2)*1000.0),DMIN(3),DMIN(4),DMIN(5),DMIN(6),DMIN(7)
        call eddisp(itru,outs)
        call edisp(itru,'*end_report')
        call edisp(itru,' ')

C Integrate heating cooling and lighting demands for the period.
C The IGET number is fixed at 70.
        write(outs,'(9a)') '*report',tab,'70',tab,'demand',
     &    tab,'integrated',tab,zedsdoc(ij)(1:lnblnk(zedsdoc(ij)))
        call edisp(itru,outs)
        write(outs,'(5a)') '*title',tab,'Integrated demand',tab,'kWhr'
        call edisp(itru,outs)
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting (unctld)',tab,
     &    'Lighting (ctld)',tab,'Fans',tab,'Small Power',tab,
     &    'Hot water'
        call edisp(itru,outs)
        write(outs,'(a,7F13.2)')'*data ',THE,abs(TCE),tuclight,tlight,
     &    tpumps,tsmlpw,tdhw
        call eddisp(itru,outs)
        call edisp(itru,'*end_report')
        call edisp(itru,' ')
 888  continue

C *****Maximum Capacity for non-specific demands, also ask
C for any PV data. Note that erbdmd does
C not know about idmdinc() so this needs to be applied.
      if(dmdsok)then
        call dmdstot(ddmax,ddmin,dtot,dave)
        if(idmdinc(1).eq.0)then
          ddmax(1)=0.0
          ddmin(1)=0.0
          dtot(1)=0.0
          dave(1)=0.0
        endif
        if(idmdinc(2).eq.0)then
          ddmax(2)=0.0
          ddmin(2)=0.0
          dtot(2)=0.0
          dave(2)=0.0
        endif
        if(idmdinc(3).eq.0)then
          ddmax(3)=0.0
          ddmin(3)=0.0
          dtot(3)=0.0
          dave(3)=0.0
        endif
        if(idmdinc(4).eq.0)then
          ddmax(4)=0.0
          ddmin(4)=0.0
          dtot(4)=0.0
          dave(4)=0.0
        endif
        if(idmdinc(5).eq.0)then
          ddmax(5)=0.0
          ddmin(5)=0.0
          dtot(5)=0.0
          dave(5)=0.0
        endif
        if(idmdinc(6).eq.0)then
          ddmax(6)=0.0
          ddmin(6)=0.0
          dtot(6)=0.0
          dave(6)=0.0
        endif
        if(idmdinc(7).eq.0)then
          ddmax(7)=0.0
          ddmin(7)=0.0
          dtot(7)=0.0
          dave(7)=0.0
        endif
        pvmax=0.
        totpv=0.
        call aggregpv(iseas,pvok,pvmax,totpv,iier)
        
C The IGET number is fixed at 71.
        SLABEL(1)='Dispersed demands'; LNSLABEL(1)=17
        call edisp(itru,' ')
        write(outs,'(9a)') '*report',tab,'71',tab,'maximum',
     &    tab,'capacity',tab,'Dispersed'
        call edisp(itru,outs)
        write(outs,'(5a)') '*title',tab,
     &    'Maximum capacity',tab,'W'
        call edisp(itru,outs)
        if(pvok)then
          write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'8'
          call edisp(itru,outs)
          write(outs,'(17A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &      'Lighting (unctld)',tab,'Lighting (ctld)',tab,
     &      'Fans',tab,'Pumps',tab,'Lifts',tab,'Small Power',tab,
     &      'Hot water',tab,'PV'
          call edisp(itru,outs)
          write(outs,'(a,F12.1,a,6F12.1)') '*data ',ddmax(2),' 0.000',
     &      ddmax(4),ddmax(5),ddmax(6),ddmax(3),ddmax(7),pvmax
          call eddisp(itru,outs)
        else
          write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
          call edisp(itru,outs)
          write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &      'Lighting (unctld)',tab,'Lighting (ctld)',tab,
     &      'Fans',tab,'Pumps',tab,'Lifts',tab,'Small Power',tab,
     &      'Hot water'
          call edisp(itru,outs)
          write(outs,'(a,F12.1,a,5F12.1)') '*data ',ddmax(2),' 0.000',
     &      ddmax(4),ddmax(5),ddmax(6),ddmax(3),ddmax(7)
          call eddisp(itru,outs)
        endif
        call edisp(itru,'*end_report')
        call edisp(itru,' ')
        
C The IGET number is fixed at 71.
        SLABEL(1)='Dispersed demands'; LNSLABEL(1)=17
        write(outs,'(9a)') '*report',tab,'71',tab,'minimum',
     &    tab,'capacity',tab,'Dispersed'
        call edisp(itru,outs)
        write(outs,'(5a)') '*title',tab,'Minimum capacity',tab,'W'
        call edisp(itru,outs)
        if(pvok)then
          write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'8'
          call edisp(itru,outs)
          write(outs,'(17A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &      'Lighting (unctld)',tab,'Lighting (ctld)',tab,
     &      'Fans',tab,'Pumps',tab,'Lifts',tab,'Small Power',tab,
     &      'Hot water',tab,'PV'
          call edisp(itru,outs)
          write(outs,'(a,F12.1,a,5F12.1,a)') '*data ',ddmin(2),' 0.000',
     &      ddmin(4),ddmin(5),ddmin(6),ddmin(3),ddmin(7),' 0.000'
          call eddisp(itru,outs)
        else
          write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
          call edisp(itru,outs)
          write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &      'Lighting (unctld)',tab,'Lighting (ctld)',tab,
     &      'Fans',tab,'Pumps',tab,'Lifts',tab,'Small Power',tab,
     &      'Hot water'
          call edisp(itru,outs)
          write(outs,'(a,F12.1,a,5F12.1)') '*data ',ddmin(2),' 0.000',
     &      ddmin(4),ddmin(5),ddmin(6),ddmin(3),ddmin(7)
          call eddisp(itru,outs)
        endif
        call edisp(itru,'*end_report')
        call edisp(itru,' ')
        
C The IGET number is fixed at 71.
        SLABEL(1)='Dispersed demands'; LNSLABEL(1)=17
        write(outs,'(9a)') '*report',tab,'71',tab,'average',
     &    tab,'capacity',tab,'Dispersed'
        call edisp(itru,outs)
        write(outs,'(5a)') '*title',tab,'Average capacity',tab,'W'
        call edisp(itru,outs)
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Lighting (unctld)',tab,'Lighting (ctld)',tab,
     &    'Fans',tab,'Pumps',tab,'Lifts',tab,'Small Power',tab,
     &    'Hot water'
        call edisp(itru,outs)
        write(outs,'(a,F12.1,a,5F12.1)') '*data ',dave(2),' 0.000',
     &    dave(4),dave(5),dave(6),dave(3),dave(7)
        call eddisp(itru,outs)
        call edisp(itru,'*end_report')
        call edisp(itru,' ')

C Convert integrated demands into kWhr.
        if(dtot(1).gt.0.001)dtot(1)=dtot(1)/1000.0
        if(dtot(2).gt.0.001)dtot(2)=dtot(2)/1000.0
        if(dtot(3).gt.0.001)dtot(3)=dtot(3)/1000.0
        if(dtot(4).gt.0.001)dtot(4)=dtot(4)/1000.0
        if(dtot(5).gt.0.001)dtot(5)=dtot(5)/1000.0
        if(dtot(6).gt.0.001)dtot(6)=dtot(6)/1000.0
        if(dtot(7).gt.0.001)dtot(7)=dtot(7)/1000.0
        if(totpv.gt.0.001)totpv=totpv/1000.0

C The IGET number is fixed at 71.
        write(outs,'(9a)') '*report',tab,'71',tab,'demand',
     &    tab,'integrated',tab,'Dispersed'
        call edisp(itru,outs)
        write(outs,'(5a)') '*title',tab,'Integrated demand',tab,'kWhr'
        call edisp(itru,outs)
        if(pvok)then
          write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'8'
          call edisp(itru,outs)
          write(outs,'(17A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &      'Lighting (unctld)',tab,'Lighting (ctld)',tab,
     &      'Fans',tab,'Pumps',tab,'Lifts',tab,'Small Power',tab,
     &      'Hot water',tab,'PV'
          call edisp(itru,outs)
          write(outs,'(a,F13.2,a,6F13.2)') '*data ',dtot(2),' 0.000',
     &      dtot(4),dtot(5),dtot(6),dtot(3),dtot(7),totpv
          call eddisp(itru,outs)
        else
          write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
          call edisp(itru,outs)
          write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &      'Lighting (unctld)',tab,'Lighting (ctld)',tab,
     &      'Fans',tab,'Pumps',tab,'Lifts',tab,'Small Power',tab,
     &      'Hot water'
          call edisp(itru,outs)
          write(outs,'(a,F13.2,a,5F13.2)') '*data ',dtot(2),' 0.000',
     &      dtot(4),dtot(5),dtot(6),dtot(3),dtot(7)
          call eddisp(itru,outs)
        endif
        call edisp(itru,'*end_report')
        call edisp(itru,' ')
      endif

C Metrics - first produce frequency bin for each metric.
      if(nms.ge.1)then
        do 77 ig=1,nms
          if(imetget(ig).eq.73)then
            continue
          elseif(imetget(ig).eq.74)then
            continue
          else
            write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &        'distribution',tab,msdoc(ig)(1:lnblnk(msdoc(ig))),tab,
     &         metgroup(ig)
            call edisp(itru,outs)
          endif
          if(imetget(ig).eq.1)then
            write(outs,'(5a)') '*title',tab,'Dry bulb temperature',tab,
     &        'degC'
            call edisp(itru,outs)
          elseif(imetget(ig).eq.6)then
            write(outs,'(5a)') '*title',tab,'Resultant temperature',
     &        tab,'degC'
            call edisp(itru,outs)
          elseif(imetget(ig).eq.11)then
            write(outs,'(5a)') '*title',tab,'Infiltration load',tab,'W'
            call edisp(itru,outs)
          elseif(imetget(ig).eq.12)then
            write(outs,'(5a)') '*title',tab,'Ventilation load',tab,'W'
            call edisp(itru,outs)
          elseif(imetget(ig).eq.13)then
            write(outs,'(5a)') '*title',tab,'Relative humidity',tab,'%'
            call edisp(itru,outs)
          elseif(imetget(ig).eq.15)then
            write(outs,'(5a)') '*title',tab,'Total casual gains',tab,
     &        'W'
            call edisp(itru,outs)
          elseif(imetget(ig).eq.38)then
            write(outs,'(5a)') '*title',tab,
     &        'Solar entering from outside',tab,'W'
            call edisp(itru,outs)
          elseif(imetget(ig).eq.40)then
            write(outs,'(5a)') '*title',tab,'Total solar absorbed',
     &        tab,'W'
            call edisp(itru,outs)
          elseif(imetget(ig).eq.73)then
            write(outs,'(5a)') '*title',tab,'Visual comfort',tab,'-'
          elseif(imetget(ig).eq.74)then
            write(outs,'(5a)') '*title',tab,'Cnv+Rad & Latent',tab,'Wh'
          endif

C Recover information for each metric.
          THE=0.
          TCE=0.
          ISET=ISIM
          NZ=nzmg(ig)
          if(NZ.eq.0)then
            continue
          else
            NGET=nzmg(ig)

C Set up selections.
            DO 250 I=1,NZ
              NZNO(I)=izmg(ig,I)
              if(imetget(ig).eq.1)then
                IGETNO(I,1)=1
                IGETNO(I,2)=izmg(ig,I)
                IGETNO(I,3)=0
                IGETNO(I,4)=0
                IGETNO(I,5)=ISET
                IGETNO(I,6)=1
                IGETNO(I,7)=6
                IGETNO(I,8)=1
                SLABEL(I)='Dry bulb temperature,degC'; LNSLABEL(I)=25
              elseif(imetget(ig).eq.6)then
                IGETNO(I,1)=6
                IGETNO(I,2)=izmg(ig,I)
                IGETNO(I,3)=0
                IGETNO(I,4)=0
                IGETNO(I,5)=ISET
                IGETNO(I,6)=1
                IGETNO(I,7)=5
                IGETNO(I,8)=1
                SLABEL(I)='Resultant temperature,degC'; LNSLABEL(I)=26
              elseif(imetget(ig).eq.11)then
                IGETNO(I,1)=11
                IGETNO(I,2)=izmg(ig,I)
                IGETNO(I,3)=0
                IGETNO(I,4)=0
                IGETNO(I,5)=ISET
                IGETNO(I,6)=-3
                IGETNO(I,7)=4
                IGETNO(I,8)=2
                SLABEL(I)='Infiltration load,W'; LNSLABEL(I)=19
              elseif(imetget(ig).eq.12)then
                IGETNO(I,1)=12
                IGETNO(I,2)=izmg(ig,I)
                IGETNO(I,3)=0
                IGETNO(I,4)=0
                IGETNO(I,5)=ISET
                IGETNO(I,6)=-3
                IGETNO(I,7)=9
                IGETNO(I,8)=2
                SLABEL(I)='Ventilation load,W'; LNSLABEL(I)=18
              elseif(imetget(ig).eq.13)then
                IGETNO(I,1)=13
                IGETNO(I,2)=izmg(ig,I)
                IGETNO(I,3)=0
                IGETNO(I,4)=0
                IGETNO(I,5)=ISET
                IGETNO(I,6)=-202
                IGETNO(I,7)=4
                IGETNO(I,8)=4
                SLABEL(I)='Relative humidity,%'; LNSLABEL(I)=19
              elseif(imetget(ig).eq.15)then
                IGETNO(I,1)=15
                IGETNO(I,2)=izmg(ig,I)
                IGETNO(I,3)=1
                IGETNO(I,4)=0
                IGETNO(I,5)=ISET
                IGETNO(I,6)=-3
                IGETNO(I,7)=12
                IGETNO(I,8)=2
                SLABEL(I)='Total casual gains,W'; LNSLABEL(I)=20
              elseif(imetget(ig).eq.38)then
                IGETNO(I,1)=38
                IGETNO(I,2)=izmg(ig,I)
                IGETNO(I,3)=0
                IGETNO(I,4)=0
                IGETNO(I,5)=ISET
                IGETNO(I,6)=-104
                IGETNO(I,7)=7
                IGETNO(I,8)=2
                SLABEL(I)='Solar entering from outside,W'
                LNSLABEL(I)=29
              elseif(imetget(ig).eq.40)then
                IGETNO(I,1)=40
                IGETNO(I,2)=izmg(ig,I)
                IGETNO(I,3)=0
                IGETNO(I,4)=0
                IGETNO(I,5)=ISET
                IGETNO(I,6)=-104
                IGETNO(I,7)=9
                IGETNO(I,8)=2
                SLABEL(I)='Total solar absorbed,W'; LNSLABEL(I)=22
              elseif(imetget(ig).eq.73)then
                IGETNO(I,1)=73
                IGETNO(I,2)=izmg(ig,I)
                IGETNO(I,3)=0
                IGETNO(I,4)=0
                IGETNO(I,5)=ISET
                IGETNO(I,6)=1
                IGETNO(I,7)=6
                IGETNO(I,8)=1
                SLABEL(I)='Guth comfort index,-'; LNSLABEL(I)=20
              elseif(imetget(ig).eq.74)then
                IGETNO(I,1)=74
                IGETNO(I,2)=izmg(ig,I)
                IGETNO(I,3)=1
                IGETNO(I,4)=0
                IGETNO(I,5)=ISET
                IGETNO(I,6)=-3
                IGETNO(I,7)=12
                IGETNO(I,8)=2
                SLABEL(I)='Plnt cnv+rad&latent,Wh'; LNSLABEL(I)=22
              endif
  250       CONTINUE

C Frequency binning for comfort, when finished, reset iocupf.
            if(imetmsc(ig,2).ne.0)then
              iocupf=1
              iocut=imetmsc(ig,1)
              call scnocup(iier)
              if (iocut.ne.IFILT) call MMAPER
            else
              iocupf=0
            endif
            if(imetget(ig).eq.73)then
              continue
            elseif(imetget(ig).eq.74)then
              continue
            else
              CALL MODGRF('m',imetget(ig))
              call edisp(itru,'*end_report')
              call edisp(itru,' ')
            endif

C Next grab the stats for current metric. 
            if(imetget(ig).eq.73)then
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'stats',tab,msdoc(ig)(1:lnblnk(msdoc(ig))),tab,
     &          metgroup(ig)
              call edisp(itru,outs)
              write(outs,'(3a)') '*title',tab,SLABEL(1)(1:LNSLABEL(1))
              call edisp(itru,outs)
              if(imetmsc(ig,1).eq.2)then

C Guth report.
               write(outs,'(7a)') '*format',tab,'table',tab,'13',tab,'2'
               call edisp(itru,outs)
               write(outs,'(5A)',IOSTAT=IOS,ERR=1) '*fields',tab,'xx',
     &           tab,'yy'
                call edisp(itru,outs)
                call edisp(itru,'*data ')
                call eddisp(itru,'-60.0  0. ')
                call eddisp(itru,'-50.0  0. ')
                call eddisp(itru,'-40.0  0. ')
                call eddisp(itru,'-30.0  0. ')
                call eddisp(itru,'-20.0  0. ')
                call eddisp(itru,'-10.0  0. ')
                call eddisp(itru,'  0.0  0. ')
                call eddisp(itru,' 10.0  0. ')
                call eddisp(itru,' 20.0  0. ')
                call eddisp(itru,' 30.0  0. ')
                call eddisp(itru,' 40.0  0. ')
                call eddisp(itru,' 50.0  0. ')
                call eddisp(itru,' 60.0  0. ')
                call edisp(itru,'*end_report')
                call edisp(itru,' ')
              elseif(imetmsc(ig,1).eq.3)then

C Daylight factors.
                write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'2'
                call edisp(itru,outs)
                write(outs,'(5A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &            'xx',tab,'yy'
                call edisp(itru,outs)
                call edisp(itru,'*data ')
                call eddisp(itru,'value value')
                call edisp(itru,'*end_report')
                call edisp(itru,' ')
              else
                write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'2'
                call edisp(itru,outs)
                write(outs,'(5A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &            'xx',tab,'yy'
                call edisp(itru,outs)
                call edisp(itru,'*data ')
                call eddisp(itru,'value value')
                call edisp(itru,'*end_report')
                call edisp(itru,' ')
              endif
            elseif(imetget(ig).eq.74)then
              call MOEDMDS('i',the,tzhtime,tce,tzctime,tlhe,tzlhtime,
     &          tlce,tzlctime)

C Write stats for current metric.
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'use',tab,msdoc(ig)(1:lnblnk(msdoc(ig))),tab,
     &          metgroup(ig)
              call edisp(itru,outs)

              write(outs,'(3a)') '*title',tab,SLABEL(1)(1:LNSLABEL(1))
              call edisp(itru,outs)
              write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'8'
              call edisp(itru,outs)
              write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &          'heating',tab,'heathours',tab,'cooling',tab,
     &          'coolinghours',tab,'latentheat',tab,
     &          'latentheathours',tab,'latentcooling',tab,
     &          'latentcoolinghours'
              call edisp(itru,outs)
              write(outs,'(a,4(F12.3,F7.1))')'*data ',the,tzhtime,tce,
     &          tzctime,tlhe,tzlhtime,tlce,tzlctime
              call eddisp(itru,outs)
              call edisp(itru,'*end_report')
              call edisp(itru,' ')
            else
              CALL MOSMSD('i',XMAX(1),XMIN(1),XAVE(1),XTOT(1),DMAX(1),
     &          DMIN(1),XMSUM(1),XMINSUM(1))

C Write stats for current metric.
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'stats',tab,msdoc(ig)(1:lnblnk(msdoc(ig))),tab,
     &          metgroup(ig)
              call edisp(itru,outs)

              write(outs,'(3a)') '*title',tab,SLABEL(1)(1:LNSLABEL(1))
              call edisp(itru,outs)
              if(imetget(ig).eq.1.or.imetget(ig).eq.6)then
                write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'3'
                call edisp(itru,outs)
                write(outs,'(7A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &            'maximum',tab,'minimum',tab,'average'
                call edisp(itru,outs)
                write(outs,'(a,3F12.3)')'*data ',XMAX(1),XMIN(1),XAVE(1)
                call eddisp(itru,outs)
              else
                write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
                call edisp(itru,outs)
                write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &            'maximum',tab,'minimum',tab,'average',tab,
     &            'diversified_max',tab,'distributed_max',tab,
     &            'diversified_min',tab,'distributed_min'
                call edisp(itru,outs)
                write(outs,'(a,7F12.3)')'*data ',XMSUM(1),XMIN(1),
     &            XAVE(1),DMAX(1),XMSUM(1),DMIN(1),XMINSUM(1)
                call eddisp(itru,outs)
              endif
              call edisp(itru,'*end_report')
              call edisp(itru,' ')
            endif

C Reset occupancy.
            iocupf=0
          endif
  77    continue
      endif

C Loop through each display day and check to see if it is in
C the current seasons assessment. If it is then proceed to
C recover the relevant information for each of the demand
C sets and metrics.
      IOH1=1
      IOH2=24
      if(nipvdispjd.eq.0)then

C If no days to display jump to aggregate.
        goto 477
      endif
      do 345 ik=1,nipvdispjd

C Check to see if the display day is during the current period.
        within=.false.
        itest=ipvdispjd(ik)
        if(iseas.eq.1)then
          if(itest.ge.ipvastjd(1).and.itest.le.ipvafnjd(1))then
            within=.true.
            IDAY=itest
          endif
        elseif(iseas.eq.2)then
          if(itest.ge.ipvastjd(2).and.itest.le.ipvafnjd(2))then
            within=.true.
            IDAY=itest
          endif
        elseif(iseas.eq.3)then
          if(itest.ge.ipvastjd(3).and.itest.le.ipvafnjd(3))then
            within=.true.
            IDAY=itest
          endif
        elseif(iseas.eq.4)then
          if(itest.ge.ipvastjd(4).and.itest.le.ipvafnjd(4))then
            within=.true.
            IDAY=itest
          endif
        elseif(iseas.eq.5)then
          if(itest.ge.ipvastjd(5).and.itest.le.ipvafnjd(5))then
            within=.true.
            IDAY=itest
          endif
        endif

C Display day is not within an assessment period, Jump to next display day.
        if(.NOT.within) goto 345

C There is information to display for this day.
        call EDAYR(iday,IOD1,IOM1)
        call EDAYR(iday,IOD2,IOM2)
        call chktime(iday,istart,iend)
        CALL EDAY(IOD1,IOM1,IODS)
        CALL EDAY(IOD2,IOM2,IODF)
        call stdate(iyear,ipvdispjd(ik),descr,descrst)
        WRITE(outs,'(4a,i4)',IOSTAT=IOS,ERR=1)'*day_to_display',tab,
     &    descrst,' ',iyear
        call edisp(itru,outs)

C Clear edput array.
        do 11 I=1,7
          do 12 j=1,24*NTS
            edput(i,j)=0.
   12     continue
   11   continue
        ISET=ISIM

C Reset the format of timestep reporting to julian decimal.
        LIHFLAG=IHFLAG
        IHFLAG=1

C Loop through each of the current demand sets to gather hourly data
C for the table.
        do 988 ij=1,neds
          NZ=nzedg(ij)
          if(NZ.gt.0)then
            NGET=nzedg(ij)
            DO 60 I=1,NZ
              NZNO(I)=izedg(ij,I)

C Setup recovery options for heating flux then cooling flux and then
C uncontrolled and controlled lighting.
              IGETNO(I,1)=8
              IGETNO(I,2)=izedg(ij,I)
              IGETNO(I,3)=0
              IGETNO(I,4)=0
              IGETNO(I,5)=ISET
              SLABEL(I)='Heating flux injection W'
   60       CONTINUE
            call GOGET(IDAY)
            DO 61 I=1,NZ
              do 62 j=ISTART,IEND,NOUT
                edput(1,j)=edput(1,j)+(VAL2(i,j)*1000.0)
   62         continue
   61       continue
            DO 63 I=1,NZ
              IGETNO(I,1)=9
              SLABEL(I)='Cooling flux (W)'; LNSLABEL(I)=16
   63       CONTINUE
            call GOGET(IDAY)
            DO 64 I=1,NZ
              do 265 j=ISTART,IEND,NOUT
                edput(2,j)=edput(2,j)+(VAL2(i,j)*1000.0)
  265         continue
   64       continue
            DO 66 I=1,NZ
              IGETNO(I,1)=15
              IGETNO(I,3)=5
              SLABEL(I)='Lighting (unctld) gains (W)'
              LNSLABEL(I)=27
   66       CONTINUE
            call GOGET(IDAY)
            DO 67 I=1,NZ
              do 68 j=ISTART,IEND,NOUT
                edput(3,j)=edput(3,j)+VAL2(i,j)
   68         continue
   67       continue
            DO 70 I=1,NZ
              IGETNO(I,1)=15
              IGETNO(I,3)=7
              SLABEL(I)='Lighting (ctld) gains (W)'; LNSLABEL(I)=25
   70       CONTINUE
            call GOGET(IDAY)
            DO 71 I=1,NZ
              do 72 j=ISTART,IEND,NOUT
                edput(3,j)=edput(3,j)+VAL2(i,j)
   72         continue
   71       continue

C Fans (currently filled with 0.0).
            do 73 j=ISTART,IEND,NOUT
              edput(4,j)=0.
   73       continue

C Small power (assumed to be equipment casual gains)
            DO 74 I=1,NZ
              IGETNO(I,1)=15
              IGETNO(I,3)=6
              SLABEL(I)='Small power (W)'; LNSLABEL(I)=15
   74       CONTINUE
            call GOGET(IDAY)
            DO 75 I=1,NZ
              do 76 j=ISTART,IEND,NOUT
                edput(5,j)=edput(5,j)+VAL2(i,j)
   76         continue
   75       continue

C Domestic hot water (currently filled with 0.0)
            do 79 j=ISTART,IEND,NOUT
              edput(6,j)=0.0
   79       continue
          endif

C Write out timestep demands.
          call edisp(itru,' ')
          write(outs,'(9a)') '*report',tab,'70',tab,'demand',tab,
     &     'per_unit_time',tab,zedsdoc(ij)(1:lnblnk(zedsdoc(ij)))
          call edisp(itru,outs)
          write(outs,'(5a)')'*title',tab,'Energy Demand per Unit Time',
     &      tab,'W'
          call edisp(itru,outs)
          write(outs,'(7a)') '*format',tab,'tabular',tab,'24',tab,'7'
          call edisp(itru,outs)
          write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,'Time',
     &      tab,'Heating',tab,'Cooling',tab,'Lighting',tab,'Fans',tab,
     &      'Small Power',tab,'Hot water'
          call edisp(itru,outs)
          call edisp(itru,'*data ')
          ihr=0
          do 90 j=ISTART,IEND,NOUT

C Compute current time (as in ZONTAB). Suggestion from L. Prazeres that
C cooling should be presented as absolute value.
            call DATIME(J,ATIME)
            if (IHFLAG.eq.0) then 
              if(ipvform.eq.3)then
                call SJTIME(J,LJTIME)
                write(outs,'(4a,6F12.1)') sq,ljtime,sq,' ',edput(1,j),
     &            abs(edput(2,j)),edput(3,j),edput(4,j),edput(5,j),
     &            edput(6,j)
              else
                call STIME(J,LTIME)
                write(outs,'(2a,6F12.1)') ltime,' ',edput(1,j),
     &            abs(edput(2,j)),edput(3,j),edput(4,j),edput(5,j),
     &            edput(6,j)
              endif
            else
              if(ipvform.eq.3)then
                call SJTIME(J,LJTIME)
                write(outs,'(4a,6F12.1)') sq,ljtime,sq,' ',edput(1,j),
     &            abs(edput(2,j)),edput(3,j),edput(4,j),edput(5,j),
     &            edput(6,j)
              else
                RDOTY=FLOAT(IDAY)+(ATIME/24.)
                write(outs,'(F10.4,6F12.1)')RDOTY,edput(1,j),
     &            abs(edput(2,j)),edput(3,j),edput(4,j),edput(5,j),
     &            edput(6,j)
              endif
            endif
            call eddisp(itru,outs)
   90     continue
          call edisp(itru,'*end_report')

C Do this for the next set of zones.
C Before looping back clear edput array.
          do 13 I=1,7
            do 14 j=1,24*NTS
              edput(i,j)=0.
   14       continue
   13     continue
 988    continue

C ***** Do this for non-specific demands.
C If PV included take the largest floor area in the dmdfla.
        if(dmdsok)then
          pvfla=1.
          if(pvok)then
            pvfla=AMAX1(pvfla,dmdfla(1),dmdfla(2),dmdfla(3),dmdfla(4),
     &            dmdfla(5),dmdfla(6),dmdfla(7))

C Debug.
C            write(6,*) 'pvfloor area is ',pvfla

          endif

C Clear edput array.
          do 15 I=1,7
            do 16 j=1,24*NTS
              edput(i,j)=0.
   16       continue
   15     continue
          call DDMDS(IDAY)
          DO 181 i=1,7
            ix=i+14
            do 182 j=ISTART,IEND,NOUT
              if(idmdinc(i).eq.1.and.dmdfla(i).gt.0.001)then
                edput(i,j)= edput(i,j) + (VAL3(ix,j)/dmdfla(i))
              endif
  182       continue
  181     continue

C Write out timestep results depending on whether there is PV
C data and the hourly format.
          call edisp(itru,'  ')
          write(outs,'(9a)') '*report',tab,'71',tab,'demand',tab,
     &     'per_unit_time',tab,'Dispersed'
          call edisp(itru,outs)
          write(outs,'(5a)')'*title',tab,'Energy Demand per Unit Time',
     &      tab,'W'
          call edisp(itru,outs)
          if(pvok)then
            write(outs,'(7a)') '*format',tab,'tabular',tab,'24',tab,'9'
            call edisp(itru,outs)
            write(outs,'(19A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &       'Time',tab,'Occup',tab,'Lighting',tab,'Small Power',tab,
     &       'Fans',tab,'Pumps',tab,'Lifts',tab,'Hot water',tab,'PV'
          else
            write(outs,'(7a)') '*format',tab,'tabular',tab,'24',tab,'8'
            call edisp(itru,outs)
            write(outs,'(17A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &       'Time',tab,'Occup',tab,'Lighting',tab,'Small Power',tab,
     &       'Fans',tab,'Pumps',tab,'Lifts',tab,'Hot water'
          endif
          call edisp(itru,outs)
          call edisp(itru,'*data ')

C Get day offset from start of season, so know how far to skip
C in the pvdayhr array.
          ihr=0
          ipvhr=((IDAY-JIODS)*24)
          do 180 j=ISTART,IEND,NOUT

C Compute current time (as in ZONTAB).
C Depending on the ihflag write as julian decimal or 10h30 format.
            call DATIME(J,ATIME)
            ihr=ihr+1
            if(pvok)then
              if(IHFLAG.eq.0)then 
                if(ipvform.eq.3)then
                  call SJTIME(J,LJTIME)
C                 ipvhr=ipvhr+1
                  write(outs,'(4a,8F12.3)')sq,ljtime,sq,' ',edput(1,j),
     &            edput(2,j),edput(3,j),edput(4,j),edput(5,j),
     &            edput(6,j),edput(7,j),pvdayhr(ipvhr)/pvfla
                else
                  call STIME(J,LTIME)
C                 ipvhr=ipvhr+1
                  write(outs,'(2a,8F12.3)') ltime,' ',edput(1,j),
     &            edput(2,j),edput(3,j),edput(4,j),edput(5,j),
     &            edput(6,j),edput(7,j),pvdayhr(ipvhr)/pvfla
                endif
              else
                if(ipvform.eq.3)then
                  call SJTIME(J,LJTIME)
C                 ipvhr=ipvhr+1
                  write(outs,'(4a,8F12.3)')sq,ljtime,sq,' ',edput(1,j),
     &            edput(2,j),edput(3,j),edput(4,j),edput(5,j),
     &            edput(6,j),edput(7,j),pvdayhr(ipvhr)/pvfla
                else
                  RDOTY=FLOAT(IDAY)+(ATIME/24.)
                  write(outs,'(F10.4,8F12.3)') RDOTY,edput(1,j),
     &            edput(2,j),edput(3,j),edput(4,j),edput(5,j),
     &            edput(6,j),edput(7,j),pvdayhr(ipvhr)/pvfla
                endif
              endif
            else
              if(IHFLAG.eq.0)then 
                if(ipvform.eq.3)then
                  call SJTIME(J,LJTIME)
                  write(outs,'(4a,7F12.3)')sq,ljtime,sq,' ',edput(1,j),
     &              edput(2,j),edput(3,j),edput(4,j),edput(5,j),
     &              edput(6,j),edput(7,j)
                else
                  call STIME(J,LTIME)
                  write(outs,'(2a,7F12.3)') ltime,' ',edput(1,j),
     &              edput(2,j),edput(3,j),edput(4,j),edput(5,j),
     &              edput(6,j),edput(7,j)
                endif
              else
                if(ipvform.eq.3)then
                  call SJTIME(J,LJTIME)
                  write(outs,'(4a,7F12.3)')sq,ljtime,sq,' ',edput(1,j),
     &              edput(2,j),edput(3,j),edput(4,j),edput(5,j),
     &              edput(6,j),edput(7,j)
                else
                  RDOTY=FLOAT(IDAY)+(ATIME/24.)
                  write(outs,'(F10.4,7F12.3)') RDOTY,edput(1,j),
     &              edput(2,j),edput(3,j),edput(4,j),edput(5,j),
     &              edput(6,j),edput(7,j)
                endif
              endif
            endif
            call eddisp(itru,outs)
  180     continue
          call edisp(itru,'*end_report')
          call edisp(itru,' ')

C Restore the hour format flag.
          IHFLAG=LIHFLAG
        endif

C Loop through each of the metrics and generate a timestep listing
C for the associated zones during this display day.
        if(nms.ge.1)then
          do 377 ig=1,nms
            if(imetget(ig).eq.1)then
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'zone_dbt',tab,msdoc(ig)(1:lnblnk(msdoc(ig))),tab,
     &          metgroup(ig)
            elseif(imetget(ig).eq.6)then
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'thermal_comfort',tab,msdoc(ig)(1:lnblnk(msdoc(ig))),
     &          tab,metgroup(ig)
            elseif(imetget(ig).eq.11)then
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'infiltration load',tab,msdoc(ig)(1:lnblnk(msdoc(ig))),
     &          tab,metgroup(ig)
            elseif(imetget(ig).eq.12)then
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'ventilation load',tab,msdoc(ig)(1:lnblnk(msdoc(ig))),
     &          tab,metgroup(ig)
            elseif(imetget(ig).eq.13)then
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'relative humidity',tab,msdoc(ig)(1:lnblnk(msdoc(ig))),
     &          tab,metgroup(ig)
            elseif(imetget(ig).eq.15)then
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'total casual gains',tab,msdoc(ig)(1:lnblnk(msdoc(ig))),
     &          tab,metgroup(ig)
            elseif(imetget(ig).eq.38)then
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'solar entering from outside',tab,
     &          msdoc(ig)(1:lnblnk(msdoc(ig))),tab,metgroup(ig)
            elseif(imetget(ig).eq.40)then
              write(outs,'(2a,i2,6a)') '*report',tab,imetget(ig),tab,
     &          'total solar absorbed',tab,
     &          msdoc(ig)(1:lnblnk(msdoc(ig))),tab,metgroup(ig)
            elseif(imetget(ig).eq.73)then
              goto 377
            elseif(imetget(ig).eq.74)then
              goto 377
            endif
            call edisp(itru,outs)
            iocupf=0
            ISET=ISIM
            NZ=nzmg(ig)
            if(NZ.eq.0)then
              continue
            else
              NGET=nzmg(ig)

C Set up selections (similar to code in moget.F).
C Set hour format flag to julian decimal for following reports.
              LIHFLAG=IHFLAG
              IHFLAG=1
              DO 350 I=1,NZ
                NZNO(I)=izmg(ig,I)
                if(imetget(ig).eq.1)then
                  SUFFIX=' db T'
                  IGETNO(I,1)=1
                  IGETNO(I,2)=izmg(ig,I)
                  IGETNO(I,3)=0
                  IGETNO(I,4)=0
                  IGETNO(I,5)=ISET
                  IGETNO(I,6)=1
                  IGETNO(I,7)=6
                  IGETNO(I,8)=1
                  SLABEL(I)='Dry bulb temperature,degC'
                  LNSLABEL(I)=25
                elseif(imetget(ig).eq.6)then
                  SUFFIX=' Res T'
                  IGETNO(I,1)=6
                  IGETNO(I,2)=izmg(ig,I)
                  IGETNO(I,3)=0
                  IGETNO(I,4)=0
                  IGETNO(I,5)=ISET
                  IGETNO(I,6)=1
                  IGETNO(I,7)=5
                  IGETNO(I,8)=1
                  SLABEL(I)='Resultant temperature,degC'
                  LNSLABEL(I)=25
                elseif(imetget(ig).eq.11)then
                  SUFFIX=' Inf'
                  IGETNO(I,1)=11
                  IGETNO(I,2)=izmg(ig,I)
                  IGETNO(I,3)=0
                  IGETNO(I,4)=0
                  IGETNO(I,5)=ISET
                  IGETNO(I,6)=-3
                  IGETNO(I,7)=4
                  IGETNO(I,8)=2
                  SLABEL(I)='Infiltration load,W'; LNSLABEL(I)=19
                elseif(imetget(ig).eq.12)then
                  SUFFIX=' Vent'
                  IGETNO(I,1)=12
                  IGETNO(I,2)=izmg(ig,I)
                  IGETNO(I,3)=0
                  IGETNO(I,4)=0
                  IGETNO(I,5)=ISET
                  IGETNO(I,6)=-3
                  IGETNO(I,7)=9
                  IGETNO(I,8)=2
                  SLABEL(I)='Ventilation load,W'; LNSLABEL(I)=18
                elseif(imetget(ig).eq.13)then
                  SUFFIX=' RH'
                  IGETNO(I,1)=13
                  IGETNO(I,2)=izmg(ig,I)
                  IGETNO(I,3)=0
                  IGETNO(I,4)=0
                  IGETNO(I,5)=ISET
                  IGETNO(I,6)=-202
                  IGETNO(I,7)=4
                  IGETNO(I,8)=4
                  SLABEL(I)='Relative humidity,%'; LNSLABEL(I)=19
                elseif(imetget(ig).eq.15)then
                  SUFFIX=' TotCnv'
                  IGETNO(I,1)=15
                  IGETNO(I,2)=izmg(ig,I)
                  IGETNO(I,3)=1
                  IGETNO(I,4)=0
                  IGETNO(I,5)=ISET
                  IGETNO(I,6)=-3
                  IGETNO(I,7)=12
                  IGETNO(I,8)=2
                  SLABEL(I)='Total casual gains,W'; LNSLABEL(I)=20
                elseif(imetget(ig).eq.38)then
                  SUFFIX=' SolinEx'
                  IGETNO(I,1)=38
                  IGETNO(I,2)=izmg(ig,I)
                  IGETNO(I,3)=0
                  IGETNO(I,4)=0
                  IGETNO(I,5)=ISET
                  IGETNO(I,6)=-104
                  IGETNO(I,7)=7
                  IGETNO(I,8)=2
                  SLABEL(I)='Solar entering from outside,W'
                  LNSLABEL(I)=29
                elseif(imetget(ig).eq.40)then
                  SUFFIX=' TSolabs'
                  IGETNO(I,1)=40
                  IGETNO(I,2)=izmg(ig,I)
                  IGETNO(I,3)=0
                  IGETNO(I,4)=0
                  IGETNO(I,5)=ISET
                  IGETNO(I,6)=-104
                  IGETNO(I,7)=9
                  IGETNO(I,8)=2
                  SLABEL(I)='Total solar absorbed,W'; LNSLABEL(I)=22
                elseif(imetget(ig).eq.73)then
                  SUFFIX=' Guth'
                  IGETNO(I,1)=73
                  IGETNO(I,2)=izmg(ig,I)
                  IGETNO(I,3)=0
                  IGETNO(I,4)=0
                  IGETNO(I,5)=ISET
                  IGETNO(I,6)=1
                  IGETNO(I,7)=6
                  IGETNO(I,8)=1
                  SLABEL(I)='Guth comfort index,-'; LNSLABEL(I)=20
                endif
                lnz=lnzname(izmg(ig,i))
                write (GLABEL(I),'(a,a)')ZNAME(izmg(ig,i))(1:lnz),
     &            SUFFIX
  350         CONTINUE

              write(outs,'(3a)') '*title',tab,SLABEL(1)(1:LNSLABEL(1))
              call edisp(itru,outs)

              write(outs,'(6a,i2)') '*format',tab,'tabular',
     &          tab,'24',tab,nzmg(ig)+1
              call edisp(itru,outs)
              write(outs,'(32A)',IOSTAT=IOS,ERR=1) '*fields Time ',
     &          (zname(izmg(ig,j)),j=1,nzmg(ig))
              call eddisp(itru,outs)
              call edisp(itru,'*data ')

C If java format IPV report then use alternative tabular listing.
              if(ipvform.eq.3)then
                call zontab('m')
              else
                call zontab('p')
              endif
              call edisp(itru,'*end_report')
              call edisp(itru,' ')

C Restore hour format flag.
              IHFLAG=LIHFLAG
            endif
 377      continue
        endif
        call edisp(itru,'*end_day_to_display')
        call edisp(itru,' ')
 345  continue


C Write the hourly aggregate demands for whole period if requested.
 477  if(.NOT.autook)then
        dok=.false.
        h(1)='If you want to assess hourly patterns you should say'
        h(2)='yes to this question. NOTE: this will increase the '
        h(3)='size of the report, especially as the number of days'
        h(4)='in the assessments increase. '
        call askok('Include hourly listing (for assessment period) of',
     &    'aggregate delivered energy (by type)?',ok,dok,4)
      else
        if(iaggr.eq.0)then
          ok=.false.
        elseif(iaggr.eq.1)then
          ok=.true.
        endif
      endif
      if(ok)then
        call aggreghd(itru,iseas,pvok)
        call edisp(itru,' ')
      endif
      call edisp(itru,'*end_assessment')
      if(nipvassmt.eq.1.and.iseas.eq.1)then
        call edisp(itru,'*end ')
      elseif(nipvassmt.eq.3.and.iseas.eq.3)then
        call edisp(itru,'*end ')
      elseif(nipvassmt.eq.5.and.iseas.eq.5)then
        call edisp(itru,'*end ')
      else
        call edisp(itru,' ')
      endif
      if(usermode)then
        INO=-4
        GOTO 3
      else

C Close the export file.
        call ipvexp(xfile,ixopen,ixunit,'a','T','Tabular',IER)
        if(.NOT.autook)then
          INO=-4
          GOTO 3
        else

C Silent mode: re-establish the initial periods and return.
          iod1=JIOD1
          iom1=JIOM1
          ioh1=JIOH1
          iod2=JIOD2
          iom2=JIOM2
          ioh2=JIOH2
          iods=JIODS
          iodf=JIODF
          nout=JNOUT
          iav=JIAV
          NZ=NCOMP
          do 91 im=1,NCOMP
            NZNO(im)=im
  91      continue
          return
        endif
      endif

 1    if(IOS.eq.2)then
        CALL USRMSG('No permission to write IPV report.',' ','W')
      else
        CALL USRMSG('Problem writing IPV report.',' ','W')
      endif
      CALL  ERPFREE(ixunit,ISTAT)
      return

  102 if(IOS.eq.2)then
        call usrmsg(' ','No permission to read climate file.','W')
      else
        call usrmsg(' ','Climate file cannot be assigned.','W')
      endif
      CALL  ERPFREE(ixunit,ISTAT)
      return

      end


C ********* mkannrep
C Write IPV report based on description held in common.
      subroutine mkannrep(iier)
#include "building.h"
#include "ipvdata.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTPCH/ICOUT
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      
      dimension NHW(9,MSPS)
      dimension WHD(MSPS),WCD(MSPS),WLD(MSPS),WFD(MSPS),WSPD(MSPS)
      dimension WDHWD(MSPS),WPVD(MSPS),WHC(MSPS),WCC(MSPS)
      dimension WLC(MSPS),WFC(MSPS),WSPC(MSPS),WDHWC(MSPS),WPVC(MSPS)
      character outs*124,outsd*124,outsn*124
      character xfile*144,tg*1,delim*1,h*72,tab*1,OUTSTR*124,sq*1
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72

      logical XST,dmdsok,closea,closeb,closec,closed,closee

C Make up a single quote for IPV
      sq = char(39)

C Read project demands file if it exists.
      IUO=IFIL+1
      call FINDFIL(bdmds,XST)
      IF(XST)THEN
        CALL ERPFREE(IUO,ISTAT)
        CALL ERBDMD(0,IUO,IER)
        dmdsok=.true.
      else
        dmdsok=.false.
      endif

C Interact with user if in interactive mode.
      h(1)='An annual IPV report is derived from the intermediate '
      h(2)='IPV report (previously generated). '
      ifu=ifil+1
 342  if(nipvassmt.ge.1)then
        call FINDFIL(sipvres,XST)
        IF(.NOT.XST)then
          call usrmsg('Could not find intermediate IPV report.',' ','W')
          goto 342
        endif
        CALL EFOPSEQ(ifu,sipvres,1,IER)
        CALL STRIPC(ifu,OUTSTR,99,ND,1,'line 1',IER)
        IF(IER.NE.0)goto 342
        if(OUTSTR(1:11).ne.'*IPV_REPORT')then
          call usrmsg('Not an intermediate IPV report. ',sipvres,'W')
          goto 342
        endif
        CALL  ERPFREE(ifu,ISTAT)
      endif

C If IPV report format has not been set ask the user.
      if(ipvform.eq.0)then
        h(1)='Text is human readable, comman separated is for use in'
        h(2)='templates and java is for an interactive agent.'
        CALL EASKABC('Format output for:',' ','Text','comma separated',
     &    'java',ipvform,2)
      endif

C Set tab based on format of report.
      iof = ipvform
      if(iof.eq.1)then
        tab = ' '
      else
        tab = ','
      endif

C Scan the intermediate file for all the data the annual
C summary requires, then it should close the intermediate file
C and open it in append mode to dump out the collected text.
      CALL  ERPFREE(ifu,ISTAT)

C Gather performance data from the intermediate file.
      call ipvgtdmd(ifu,dmdsok,WHD,WCD,WLD,WFD,WSPD,WDHWD,WPVD,
     &  denom,ier)  

C Gather capacity information from the intermediate report.
      call ipvgtcap(ifu,dmdsok,WHC,WCC,WLC,WFC,WSPC,WDHWC,WPVC,
     &  denomc,ier)

C Gather emissions performance data from the intermediate file.
      call ipvgtdmd(ifu,dmdsok,WHD,WCD,WLD,WFD,WSPD,WDHWD,WPVD,
     &  denom,ier)  

C Recover the distribution of resultant temperature in each bin for each season.
      call ipvgcmft(ifu,NHW,ier)
      CALL  ERPFREE(ifu,ISTAT)

C Append the summary to the intermediate report to create finished report.
      write(xfile,'(a)') sipvres(1:lnblnk(sipvres))
      call ipvexp(xfile,ixopen,ixunit,'a','T','IPV summay',IER)

C If there was a cancel request return, otherwise write the report.
      if(ier.eq.-3)then
        delim = '-'
        return
      endif
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        outs = 'IPV summary data >> to file...'
        call usrmsg(outs,' ','-')
      endif

C If the number of assessments is more than one and the scaling factors
C for each season are something other than unity, generate seasonal
C sub-totals as well.
      if(nipvassmt.gt.1)then
        do 448 ij=1,nipvassmt

          call edisp(itru,' ')
          write(outs,'(3a)') '*Seasonal_summary',tab,
     &      ipvadesc(ij)(1:lnblnk(ipvadesc(ij)))
          call edisp(itru,outs)
          call edisp(itru,' ')
          closea=.false.
          closeb=.false.
C          call eclose(ddmheat(ij),1.0,0.01,closea)
C          call eclose(ddmcool(ij),1.0,0.01,closeb)
          if(closea.and.closeb)then
            continue
          else
            call ipvseadmd(ifu,ij,dmdsok,SWHD,SWCD,SWLD,SWFD,SWSPD,
     &        SWDHWD,SWPVD,denom,ier)
            THE= SWHD*ddmheat(ij)
            THEKW= SWHD*ddmheat(ij)*denom
            TCE= abs((SWCD*ddmcool(ij)))
            TCEKW= abs((SWCD*ddmcool(ij)*denom))
            tlight= SWLD*ddmlight(ij)
            tlightkw= SWLD*ddmlight(ij)*denom
            tpumps= SWFD*ddmfan(ij)
            tpumpskw= SWFD*ddmfan(ij)*denom
            tsmlpw= SWSPD*ddmsmlpw(ij)
            tsmlpwkw= SWSPD*ddmsmlpw(ij)*denom
            tdhw= SWDHWD*ddmdhw(ij)
            tdhwkw= SWDHWD*ddmdhw(ij)*denom
            tpv= SWPVD*ddmdhw(ij)
            tpvkw= SWPVD*ddmdhw(ij)*denom

C << find an iget number for this >>
            write(outs,'(9a)') '*report',tab,'98',tab,'energy',tab,
     &        'performance',tab,'aggregate'
            call edisp(itru,outs)
            write(outs,'(5a)') '*title',tab,'Integrated demand',
     &        tab,'kWh/m^2'
            call edisp(itru,outs)
            if(tpv.gt.0.001)then
              write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
              call edisp(itru,outs)
              write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &          'Heating',tab,'Cooling',tab,'Lighting',tab,
     &          'Fans',tab,'Small Power',tab,'Hot water',tab,'PV'
              call edisp(itru,outs)
              write(outs,'(a,7F12.3)') '*data ',THE,TCE,tlight,tpumps,
     &          tsmlpw,tdhw,tpv
              call eddisp(itru,outs)
            else
              write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'6'
              call edisp(itru,outs)
              write(outs,'(13A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &          'Heating',tab,'Cooling',tab,'Lighting',tab,
     &          'Fans',tab,'Small Power',tab,'Hot water'
              call edisp(itru,outs)
              write(outs,'(a,6F12.3)') '*data ',THE,TCE,tlight,tpumps,
     &          tsmlpw,tdhw
              call eddisp(itru,outs)
            endif
        
C Note: units follow tab (suggesstion by Luis Prazeres to not print total
C line in the seasonal section).
            Total = THE+abs(TCE)+tlight+tpumps+tsmlpw+tdhw
            call edisp(itru,'*end_report')
            call edisp(itru,' ')

C Report for the total building as well
            write(outs,'(9a)') '*report',tab,'98',tab,'energy',tab,
     &        'building_performance',tab,'aggregate'
            call edisp(itru,outs)
            write(outs,'(5a)') '*title',tab,
     &        'Integrated building demand',tab,'kWh'
            call edisp(itru,outs)
            if(tpv.gt.0.001)then
              write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
              call edisp(itru,outs)
              write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &          'Heating',tab,'Cooling',tab,'Lighting',tab,
     &          'Fans',tab,'Small Power',tab,'Hot water',tab,'PV'
              call edisp(itru,outs)
              write(outs,'(a,7F12.3)') '*data ',THEKW,TCEKW,tlightkw,
     &          tpumpskw,tsmlpwkw,tdhwkw,tpvkw
              call eddisp(itru,outs)
            else
              write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'6'
              call edisp(itru,outs)
              write(outs,'(13A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &          'Heating',tab,'Cooling',tab,'Lighting',tab,
     &          'Fans',tab,'Small Power',tab,'Hot water'
              call edisp(itru,outs)
              write(outs,'(a,6F12.1)') '*data ',THEKW,TCEKW,tlightkw,
     &          tpumpskw,tsmlpwkw,tdhwkw
              call eddisp(itru,outs)
            endif
        
            call edisp(itru,'*end_report')
            call edisp(itru,' ')
            write(outs,'(3a)') '*End_seasonal_summary',tab,
     &        ipvadesc(ij)(1:lnblnk(ipvadesc(ij)))
            call edisp(itru,outs)
          endif
 448    continue
      endif

      call edisp(itru,' ')
      call edisp(itru,'*Summary')
      call edisp(itru,' ')

C Write performance data using seasonal dd* adjustments.
      THE=0.0
      THEKW=0.0
      TCE=0.0
      TCEKW=0.0
      tlight=0.0
      tlightkw=0.0
      tpumps=0.0
      tpumpskw=0.0
      tsmlpw=0.0
      tsmlpwkw=0.0
      tdhw=0.0
      tdhwkw=0.0
      tpv=0.0
      tpvkw=0.0
      do 446 ij=1,nipvassmt
        THE= THE +(WHD(ij)*ddmheat(ij))
        THEKW= THEKW +(WHD(ij)*ddmheat(ij)*denom)
        TCE= TCE + abs((WCD(ij)*ddmcool(ij)))
        TCEKW= TCEKW + abs((WCD(ij)*ddmcool(ij)*denom))
        tlight= tlight + (WLD(ij)*ddmlight(ij))
        tlightkw= tlightkw + (WLD(ij)*ddmlight(ij)*denom)
        tpumps= tpumps + (WFD(ij)*ddmfan(ij))
        tpumpskw= tpumpskw + (WFD(ij)*ddmfan(ij)*denom)
        tsmlpw= tsmlpw + (WSPD(ij)*ddmsmlpw(ij))
        tsmlpwkw= tsmlpwkw + (WSPD(ij)*ddmsmlpw(ij)*denom)
        tdhw= tdhw + (WDHWD(ij)*ddmdhw(ij))
        tdhwkw= tdhwkw + (WDHWD(ij)*ddmdhw(ij)*denom)

C << get a multiplier for PV supply >>.
        tpv= tpv + (WPVD(ij)*ddmdhw(ij))
        tpvkw= tpvkw + (WPVD(ij)*ddmdhw(ij)*denom)
 446  continue

C << find an iget number for this >>
      write(outs,'(9a)') '*report',tab,'98',tab,'energy',tab,
     &  'performance',tab,'aggregate'
      call edisp(itru,outs)
      write(outs,'(5a)') '*title',tab,'Integrated demand',tab,
     &  'kWh/m^2.a'
      call edisp(itru,outs)
      if(tpv.gt.0.001)then
        write(outs,'(7a)') '*format',tab,'table',tab,'2',tab,'7'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting',tab,
     &    'Fans',tab,'Small Power',tab,'Hot water',tab,'PV'
        call edisp(itru,outs)
        write(outs,'(a,7F12.3)') '*data ',THE,TCE,tlight,tpumps,tsmlpw,
     &    tdhw,tpv
        call eddisp(itru,outs)
      else
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'6'
        call edisp(itru,outs)
        write(outs,'(13A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting',tab,
     &    'Fans',tab,'Small Power',tab,'Hot water'
        call edisp(itru,outs)
        write(outs,'(a,6F12.3)') '*data ',THE,TCE,tlight,tpumps,tsmlpw,
     &    tdhw
        call eddisp(itru,outs)
      endif
        
      call edisp(itru,'*end_report')
      call edisp(itru,' ')

C Report for the total building as well
C << find an iget number for this >>
      write(outs,'(9a)') '*report',tab,'98',tab,'energy',tab,
     &  'building_performance',tab,'aggregate'
      call edisp(itru,outs)
      write(outs,'(5a)') '*title',tab,'Integrated building demand',tab,
     &  'kWh/a'
      call edisp(itru,outs)
      if(tpv.gt.0.001)then
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting',tab,
     &    'Fans',tab,'Small Power',tab,'Hot water',tab,'PV'
        call edisp(itru,outs)
        write(outs,'(a,7F12.3)') '*data ',THEKW,TCEKW,tlightkw,tpumpskw,
     &    tsmlpwkw,tdhwkw,tpvkw
        call eddisp(itru,outs)
      else
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'6'
        call edisp(itru,outs)
        write(outs,'(13A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting',tab,
     &    'Fans',tab,'Small Power',tab,'Hot water'
        call edisp(itru,outs)
        write(outs,'(a,6F12.1)') '*data ',THEKW,TCEKW,tlightkw,tpumpskw,
     &    tsmlpwkw,tdhwkw
        call eddisp(itru,outs)
      endif
        
C Note: units follow tab. This total no longer used in the report.
C      Total = THEKW+abs(TCEKW)+tlightkw+tpumpskw+tsmlpwkw+tdhwkw
C      write(outs,'(4a,F12.1)') '*data ',tab,'Total',tab,total
C      call eddisp(itru,outs)
      call edisp(itru,'*end_report')
      call edisp(itru,' ')

C Report capacity information gathered from the intermediate report.
      HC=AMAX1(WHC(1),WHC(2),WHC(3),WHC(4),WHC(5))
      CC=AMAX1(WCC(1),WCC(2),WCC(3),WCC(4),WCC(5))
      CL=AMAX1(WLC(1),WLC(2),WLC(3),WLC(4),WLC(5))
      FC=AMAX1(WFC(1),WFC(2),WFC(3),WFC(4),WFC(5))
      SPC=AMAX1(WSPC(1),WSPC(2),WSPC(3),WSPC(4),WSPC(5))
      DHWC=AMAX1(WDHWC(1),WDHWC(2),WDHWC(3),WDHWC(4),WDHWC(5))
      PVC=AMAX1(WPVC(1),WPVC(2),WPVC(3),WPVC(4),WPVC(5))

C Debug...
C      write(6,*) 'hc cc cl fc spc dhwc pvc ',hc,cc,cl,fc,spc,dhwc,pvc

C IGET = 74 IPV maximum power capacity (W/m^2)
      write(outs,'(9a)') '*report',tab,'74',tab,'power',tab,'capacity',
     &  tab,'aggregate'
      call edisp(itru,outs)
      write(outs,'(5a)') '*title',tab,'Maximum capacity',tab,
     &  'W/m^2'
      call edisp(itru,outs)
      if(PVC.gt.0.001)then
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting',tab,
     &    'Fans',tab,'Small Power',tab,'Hot water',tab,'PV'
        call edisp(itru,outs)
        write(outs,'(a,7F12.3)') '*data ',HC,CC,CL,FC,SPC,DHWC,PVC
        call eddisp(itru,outs)
      else
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'6'
        call edisp(itru,outs)
        write(outs,'(13A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting',tab,
     &    'Fans',tab,'Small Power',tab,'Hot water'
        call edisp(itru,outs)
        write(outs,'(a,6F12.3)') '*data ',HC,CC,CL,FC,SPC,DHWC
        call eddisp(itru,outs)
      endif
      call edisp(itru,'*end_report')
      call edisp(itru,' ')


C IGET = 74 IPV maximum power capacity (kW)
      HC=(HC*denomc)*0.001
      CC=(CC*denomc)*0.001
      CL=(CL*denomc)*0.001
      FC=(FC*denomc)*0.001
      SPC=(SPC*denomc)*0.001
      DHWC=(DHWC*denomc)*0.001
      PVC=(PVC*denomc)*0.001
      write(outs,'(9a)') '*report',tab,'74',tab,'power',tab,'capacity',
     &  tab,'aggregate'
      call edisp(itru,outs)
      write(outs,'(5a)') '*title',tab,'Maximum building capacity',tab,
     &  'kW'
      call edisp(itru,outs)
      if(PVC.gt.0.001)then
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'7'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting',tab,
     &    'Fans',tab,'Small Power',tab,'Hot water',tab,'PV'
        call edisp(itru,outs)
        write(outs,'(a,7F13.2)') '*data ',HC,CC,CL,FC,SPC,DHWC,PVC
        call eddisp(itru,outs)
      else
        write(outs,'(7a)') '*format',tab,'table',tab,'1',tab,'6'
        call edisp(itru,outs)
        write(outs,'(13A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &    'Heating',tab,'Cooling',tab,'Lighting',tab,
     &    'Fans',tab,'Small Power',tab,'Hot water'
        call edisp(itru,outs)
        write(outs,'(a,6F13.2)') '*data ',HC,CC,CL,FC,SPC,DHWC
        call eddisp(itru,outs)
      endif
      call edisp(itru,'*end_report')
      call edisp(itru,' ')

C Emissions section. Write out in the form of 'CO2=12.0,14.0,7.1,77.0'
C Convert from g/m^2/yr to kg/m^2/yr
      write(outs,'(9a)') '*report',tab,'75',tab,'emissions',tab,
     &  'pollutant',tab,'aggregate'
      call edisp(itru,outs)
      write(outs,'(5a)') '*title',tab,'Pollutant emissions',tab,
     &  'kg/m^2.a'
      call edisp(itru,outs)
      write(outs,'(7a)') '*format',tab,'table',tab,'3',tab,'6'
      call edisp(itru,outs)
      write(outs,'(21A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &  'Pollutant',tab,'CO2',tab,'NOx',tab,'SOx',tab,
     &  'Heating',tab,'Cooling',tab,'Lighting',tab,
     &  'Fans',tab,'Small Power',tab,'Hot water'
      call edisp(itru,outs)

C Write emissions performance data from the intermediate file.
      THE=0.0
      TCE=0.0
      tlight=0.0
      tpumps=0.0
      tsmlpw=0.0
      tdhw=0.0
      do 447 ij=1,nipvassmt
        THE= THE +(WHD(ij)*ddmheat(ij))
        TCE= TCE + abs((WCD(ij)*ddmcool(ij)))
        tlight= tlight + (WLD(ij)*ddmlight(ij))
        tpumps= tpumps + (WFD(ij)*ddmfan(ij))
        tsmlpw= tsmlpw + (WSPD(ij)*ddmsmlpw(ij))
        tdhw= tdhw + (WDHWD(ij)*ddmdhw(ij))
 447  continue
      THE = THE *pcnvht
      TCE = TCE *pcnvcl
      tlight = tlight *pcnvcl
      tpumps = tpumps *pcnvfn
      tsmlpw = tsmlpw *pcnvsp
      tdhw = tdhw *pcnvhw

C Debug.
C      write(6,*) 'the hce tlight tpumps tsmlpw tdhw ',the,tce,tlight,
C     &  tpumps,tsmlpw,tdhw

C Conversions from g/m^2/yr to kg/m^2/yr
      write(outsn,'(6F9.3)') THE*phtco2/1000.,TCE*pclco2/1000.,
     &    tlight*pltco2/1000.,tpumps*pfnco2/1000.,
     &    tsmlpw*pspco2/1000.,tdhw*phwco2/1000.
      call SDELIM(outsn,outsd,'C',IW)
      write(outs,'(3a)') '*data',tab,outsd(1:lnblnk(outsd))
      call edisp(itru,outs)
      write(outsn,'(6F9.3)') THE*phtnox/1000.,TCE*pclnox/1000.,
     &    tlight*pltnox/1000.,tpumps*pfnnox/1000.,
     &    tsmlpw*pspnox/1000.,tdhw*phwnox/1000.
      call SDELIM(outsn,outsd,'C',IW)
      write(outs,'(3a)') '*data',tab,outsd(1:lnblnk(outsd))
      call edisp(itru,outs)
      write(outsn,'(6F9.3)') THE*phtsox/1000.,TCE*pclsox/1000.,
     &    tlight*pltsox/1000.,tpumps*pfnsox/1000.,
     &    tsmlpw*pspsox/1000.,tdhw*phwsox/1000.
      call SDELIM(outsn,outsd,'C',IW)
      write(outs,'(3a)') '*data',tab,outsd(1:lnblnk(outsd))
      call edisp(itru,outs)
      call edisp(itru,'*end_report')
      call edisp(itru,' ')

C Emissions section. Write out in the form of 'CO2=12.0,14.0,7.1,77.0'
C Convert from g/m^2/yr to kg/yr
      THE=0.0
      TCE=0.0
      tlight=0.0
      tpumps=0.0
      tsmlpw=0.0
      tdhw=0.0
      do 449 ij=1,nipvassmt
        THE= THE +(WHD(ij)*ddmheat(ij))
        TCE= TCE + abs((WCD(ij)*ddmcool(ij)))
        tlight= tlight + (WLD(ij)*ddmlight(ij))
        tpumps= tpumps + (WFD(ij)*ddmfan(ij))
        tsmlpw= tsmlpw + (WSPD(ij)*ddmsmlpw(ij))
        tdhw= tdhw + (WDHWD(ij)*ddmdhw(ij))
 449  continue
      THE = THE *pcnvht*denom
      TCE = TCE *pcnvcl*denom
      tlight = tlight *pcnvcl*denom
      tpumps = tpumps *pcnvfn*denom
      tsmlpw = tsmlpw *pcnvsp*denom
      tdhw = tdhw *pcnvhw
      write(outs,'(9a)') '*report',tab,'75',tab,'emissions',tab,
     &  'pollutant',tab,'aggregate'
      call edisp(itru,outs)
      write(outs,'(5a)') '*title',tab,'Pollutant building emissions',
     &  tab,'kg/a'
      call edisp(itru,outs)
      write(outs,'(7a)') '*format',tab,'table',tab,'3',tab,'6'
      call edisp(itru,outs)
      write(outs,'(21A)',IOSTAT=IOS,ERR=1) '*fields',tab,
     &  'Pollutant',tab,'CO2',tab,'NOx',tab,'SOx',tab,
     &  'Heating',tab,'Cooling',tab,'Lighting',tab,
     &  'Fans',tab,'Small Power',tab,'Hot water'
      call edisp(itru,outs)

C Conversions from g/m^2/yr to kg/yr
      write(outsn,'(6F10.2)') THE*phtco2/1000.,TCE*pclco2/1000.,
     &    tlight*pltco2/1000.,tpumps*pfnco2/1000.,
     &    tsmlpw*pspco2/1000.,tdhw*phwco2/1000.
      call SDELIM(outsn,outsd,'C',IW)
      write(outs,'(3a)') '*data',tab,outsd(1:lnblnk(outsd))
      call edisp(itru,outs)
      write(outsn,'(6F10.2)') THE*phtnox/1000.,TCE*pclnox/1000.,
     &    tlight*pltnox/1000.,tpumps*pfnnox/1000.,
     &    tsmlpw*pspnox/1000.,tdhw*phwnox/1000.
      call SDELIM(outsn,outsd,'C',IW)
      write(outs,'(3a)') '*data',tab,outsd(1:lnblnk(outsd))
      call edisp(itru,outs)
      write(outsn,'(6F10.2)') THE*phtsox/1000.,TCE*pclsox/1000.,
     &    tlight*pltsox/1000.,tpumps*pfnsox/1000.,
     &    tsmlpw*pspsox/1000.,tdhw*phwsox/1000.
      call SDELIM(outsn,outsd,'C',IW)
      write(outs,'(3a)') '*data',tab,outsd(1:lnblnk(outsd))
      call edisp(itru,outs)
      call edisp(itru,'*end_report')
      call edisp(itru,' ')

C IGET = 76 IPV overall thermal comfort distribution
      write(outs,'(8a)') '*report','76',tab,'distribution',tab,
     &   'thermal_comfort',tab,'aggregate'
      call edisp(itru,outs)
      write(outs,'(5a)') '*title',tab,'Resultant Temperature',tab,
     &   'degC'
      call edisp(itru,outs)
      write(outs,'(6a,i1,6a)') '*format',tab,'frequency',tab,'9',tab,
     &  nipvassmt+1,tab,'16.0',tab,'2.0',tab,'30.0'
      call edisp(itru,outs)

C << fields should be each season name >> 
      if(nipvassmt.eq.1)then
        write(outs,'(a)')'*fields range annual '
      elseif(nipvassmt.eq.3)then
        write(outs,'(a)')'*fields range winter transition summer'
      elseif(nipvassmt.eq.5)then
        write(outs,'(2a)')'*fields range winter_early spring summer',
     &    ' autumn winter_late'
      endif
      call eddisp(itru,outs)
      call edisp(itru,'*data ')

C Write the distribution of resultant temperature in each bin for each season.
      do 198 ikk=1,9
        write(outsn,'(5i7)') (NHW(ikk,l),l=1,nipvassmt)
        call SDELIM(outsn,outsd,'C',IW)
        if(ikk.eq.1)then
          if(ipvform.eq.3)then
            write(outs,'(5a)') sq,'<16',sq,tab,outsd(1:lnblnk(outsd))
          else
            write(outs,'(3a)') '<16',tab,outsd(1:lnblnk(outsd))
          endif
        elseif(ikk.eq.2)then
          if(ipvform.eq.3)then
            write(outs,'(5a)') sq,'16-18',sq,tab,outsd(1:lnblnk(outsd))
          else
            write(outs,'(3a)') '16-18',tab,outsd(1:lnblnk(outsd))
          endif
        elseif(ikk.eq.3)then
          if(ipvform.eq.3)then
            write(outs,'(5a)') sq,'18-20',sq,tab,outsd(1:lnblnk(outsd))
          else
            write(outs,'(3a)') '18-20',tab,outsd(1:lnblnk(outsd))
          endif
        elseif(ikk.eq.4)then
          if(ipvform.eq.3)then
            write(outs,'(5a)') sq,'20-22',sq,tab,outsd(1:lnblnk(outsd))
          else
            write(outs,'(3a)') '20-22',tab,outsd(1:lnblnk(outsd))
          endif
        elseif(ikk.eq.5)then
          if(ipvform.eq.3)then
            write(outs,'(5a)') sq,'22-24',sq,tab,outsd(1:lnblnk(outsd))
          else
            write(outs,'(3a)') '22-24',tab,outsd(1:lnblnk(outsd))
          endif
        elseif(ikk.eq.6)then
          if(ipvform.eq.3)then
            write(outs,'(5a)') sq,'24-26',sq,tab,outsd(1:lnblnk(outsd))
          else
            write(outs,'(3a)') '24-26',tab,outsd(1:lnblnk(outsd))
          endif
        elseif(ikk.eq.7)then
          if(ipvform.eq.3)then
            write(outs,'(5a)') sq,'26-28',sq,tab,outsd(1:lnblnk(outsd))
          else
            write(outs,'(3a)') '26-28',tab,outsd(1:lnblnk(outsd))
          endif
        elseif(ikk.eq.8)then
          if(ipvform.eq.3)then
            write(outs,'(5a)') sq,'28-30',sq,tab,outsd(1:lnblnk(outsd))
          else
            write(outs,'(3a)') '28-30',tab,outsd(1:lnblnk(outsd))
          endif
        elseif(ikk.eq.9)then
          if(ipvform.eq.3)then
            write(outs,'(5a)') sq,'>30',sq,tab,outsd(1:lnblnk(outsd))
          else
            write(outs,'(3a)') '>30',tab,outsd(1:lnblnk(outsd))
          endif
        endif
        call edisp(itru,outs)
 198  continue
      call edisp(itru,'*end_report')
      call edisp(itru,'*end ')

      delim = '-'
      call ipvexp(xfile,ixopen,ixunit,'a','T','Tabular',IER)
      return

 1    if(IOS.eq.2)then
        CALL USRMSG('No permission to write IPV report.',' ','W')
      else
        CALL USRMSG('Problem writing IPV report.',' ','W')
      endif
      CALL  ERPFREE(ixunit,ISTAT)
      return
      end

C *********** ipvgtcap
C Recover capacity data from an intermediate report. It is called onced
C from mkannrep.
C << at some point include arrays for separate reporting of pumps and lifts. >>
      subroutine ipvgtcap(ifu,dmdsok,WHC,WCC,WLC,WFC,WSPC,WDHWC,
     &  WPVC,denom,ier)  

#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

C Uncomment next line if debugging required.
C      common/OUTIN/IUOUT,IUIN

C IPV description via ipvdata.h.

      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

      dimension WHC(MSPS),WCC(MSPS),WLC(MSPS),WFC(MSPS),WSPC(MSPS)
      dimension WDHWC(MSPS),WPVC(MSPS),HCS(MSPS,11),CCS(MSPS,11)
      dimension ZLUCS(MSPS,11),ZLCS(MSPS,11),FCS(MSPS,11)
      dimension SPCS(MSPS,11),DHWCS(MSPS,11),PVC(MSPS,11)
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72
      character outstr*124,word*20,third*20,name*12
      logical dmdsok,atassment,atreports

C Open intermediate report.
      CALL EFOPSEQ(ifu,sipvres,1,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') sipvres(1:lnblnk(sipvres))

C Clear data arrays.
      do 77 ik=1,5
        WHC(ik)=0.0
        WCC(ik)=0.0
        WLC(ik)=0.0
        WFC(ik)=0.0
        WSPC(ik)=0.0
        WDHWC(ik)=0.0
        WPVC(ik)=0.0
        do 78 ij=1,11
          HCS(ik,ij)=0.0
          CCS(ik,ij)=0.0
          ZLUCS(ik,ij)=0.0
          ZLCS(ik,ij)=0.0
          FCS(ik,ij)=0.0
          SPCS(ik,ij)=0.0
          DHWCS(ik,ij)=0.0
          PVC(ik,ij)=0.0
  78    continue
  77  continue

      do 144 ik=1,nipvassmt

C Position pointer at the start of the relevant assessment.
        call jumpassmt(ifu,ik,atassment)
        if(.NOT.atassment)then
          goto 144
        endif

C Position pointer to get aggregate for each energy demand set.
        do 145 iset=1,neds

          iindex=70
          third='diversified'
          call jumpreport(ifu,iindex,zedsdoc(iset),third,atreports)
          if(.NOT.atreports)then
            goto 145
          endif

C Skip title and format and fields and read capacity data line
  43      CALL STRIPC(ifu,OUTSTR,0,ND,1,'data',IER)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
          if(WORD(1:5).eq.'*data')then
           CALL EGETWR(OUTSTR,K,HCS(ik,iset),0.,0.,'-','heat cap',IER)
           CALL EGETWR(OUTSTR,K,CC1,0.,0.,'-','cool cap',IER)
           CCS(ik,iset)=abs(CC1)
           CALL EGETWR(OUTSTR,K,ZLUCS(ik,iset),0.,0.,'-','ltuc cap',IER)
           CALL EGETWR(OUTSTR,K,ZLCS(ik,iset),0.,0.,'-','ltct cap',IER)
           CALL EGETWR(OUTSTR,K,FCS(ik,iset),0.,0.,'-','fans cap',IER)
           CALL EGETWR(OUTSTR,K,SPCS(ik,iset),0.,0.,'-','smp cap',IER)
           CALL EGETWR(OUTSTR,K,DHWCS(ik,iset),0.,0.,'-','dhw cap',IER)
          else
            goto 43
          endif
  145   continue

C Skip title and format and fields and read capacity data line
        if(dmdsok)then

C Position pointer to get aggregate for dispersed capacity.
          iindex=71
          name='Dispersed'
          third='maximum'
          call jumpreport(ifu,iindex,name,third,atreports)
          if(.NOT.atreports)then
            continue
          else
  243       CALL STRIPC(ifu,OUTSTR,99,ND,1,'data',IER)
            K=0
            CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
            if(WORD(1:5).eq.'*data')then
             CALL EGETWR(OUTSTR,K,ZLUCS(ik,11),0.,0.,'-','ltuc cap',IER)
             CALL EGETWR(OUTSTR,K,ZLCS(ik,11),0.,0.,'-','ltct cap',IER)
             CALL EGETWR(OUTSTR,K,dfan,0.,0.,'-','fans cap',IER)
             CALL EGETWR(OUTSTR,K,dpump,0.,0.,'-','pump cap',IER)
             CALL EGETWR(OUTSTR,K,dlifts,0.,0.,'-','lifts cap',IER)
             FCS(ik,11)=dfan+dpump+dlifts
             CALL EGETWR(OUTSTR,K,SPCS(ik,11),0.,0.,'-','smp cap',IER)
             CALL EGETWR(OUTSTR,K,DHWCS(ik,11),0.,0.,'-','dhw cap',IER)
             if(ND.eq.9)then
               CALL EGETWR(OUTSTR,K,PVC(ik,11),0.,0.,'-','PV cap',IER)
             endif
            else
              goto 243
            endif
          endif
        endif

C Find denominator (scaled floor area), then for each of the
C energy demand sets get their contribution to each type of data.
        denom=0.0
        do 61 ij=1,neds
          denom=denom +(edgflr(ij)*edgsca(ij))
          WHC(ik) = WHC(ik) +(HCS(ik,ij)*edgsca(ij))
          WCC(ik) = WCC(ik) +(CCS(ik,ij)*edgsca(ij))
          WLC(ik) = WLC(ik) +((ZLUCS(ik,ij)+ZLCS(ik,ij))*edgsca(ij))
          WFC(ik) = WFC(ik) +(FCS(ik,ij)*edgsca(ij))
          WSPC(ik) = WSPC(ik) +(SPCS(ik,ij)*edgsca(ij))
          WDHWC(ik) = WDHWC(ik) +(DHWCS(ik,ij)*edgsca(ij))
  61    continue

C Add in the dispersed capacity for each type of data.
        WHC(ik) = (WHC(ik) + HCS(ik,11))/denom
        WCC(ik) = abs(WCC(ik) + CCS(ik,11))/denom
        WLC(ik) = (WLC(ik) + (ZLUCS(ik,11)+ZLCS(ik,11)))/denom
        WFC(ik) = (WFC(ik) + FCS(ik,11))/denom
        WSPC(ik) = (WSPC(ik) + SPCS(ik,11))/denom
        WDHWC(ik) = (WDHWC(ik) + DHWCS(ik,11))/denom
        WPVC(ik) = PVC(ik,11)/denom
  144 continue

C Debug.
C      write(iuout,*) 'denom c ',denom
C      write(iuout,*) 'WHC ',WHC
C      write(iuout,*) 'WCC ',WCC
C      write(iuout,*) 'WLC ',WLC
C      write(iuout,*) 'WFC ',WFC
C      write(iuout,*) 'WSPC ',WSPC
C      write(iuout,*) 'WDHWC ',WDHWC
C      write(iuout,*) 'WPVC ',WPVC

C close the file and return.
      CALL  ERPFREE(ifu,ISTAT)
      return
      end

C *********** ipvgtdmd
C Recover demand data (per m^2) from intermediate IPV report (not converted
C to primary units). Scan each assessment and return arrays.
C ipvgtdmd is called once from mkannrep.
C << at some point include additional arrays for pumps and lifts >>
      subroutine ipvgtdmd(ifu,dmdsok,WHD,WCD,WLD,WFD,WSPD,WDHWD,
     &   WPVD,denom,ier)  

#include "building.h"
#include "espriou.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

C Uncomment the next line if debugging required.
      common/OUTIN/IUOUT,IUIN
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

C For each simulation which was run and type of demand, and 
C for one more than the possible
C number of demand sets (the extra is for distributed demands).
      dimension WHD(MSPS),WCD(MSPS),WLD(MSPS),WFD(MSPS),WSPD(MSPS)
      dimension WDHWD(MSPS),WPVD(MSPS),HDS(MSPS,11),CDS(MSPS,11)
      dimension ZLUDS(MSPS,11),ZLDS(MSPS,11),FDS(MSPS,11)
      dimension SPDS(MSPS,11),DHWDS(MSPS,11),DPVD(MSPS,11)
      character outstr*124,act*1,word*20
      character name*12,third*20
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72
      logical dmdsok,atassment,atreports

C Open intermediate report.
      CALL EFOPSEQ(ifu,sipvres,1,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') sipvres(1:lnblnk(sipvres))

C Clear data arrays.
      do 77 ik=1,5
        WHD(ik)=0.0
        WCD(ik)=0.0
        WLD(ik)=0.0
        WFD(ik)=0.0
        WSPD(ik)=0.0
        WDHWD(ik)=0.0
        WPVD(ik)=0.0
        do 78 ij=1,11
          HDS(ik,ij)=0.0
          CDS(ik,ij)=0.0
          ZLUDS(ik,ij)=0.0
          ZLDS(ik,ij)=0.0
          FDS(ik,ij)=0.0
          SPDS(ik,ij)=0.0
          DHWDS(ik,ij)=0.0
          DPVD(ik,ij)=0.0
  78    continue
  77  continue

      do 144 ik=1,nipvassmt

C Position pointer at the start of the relevant assessment.
        call jumpassmt(ifu,ik,atassment)
        if(.NOT.atassment)then
          goto 144
        endif

C Position pointer to get aggregate for each energy demand set.
        do 145 iset=1,neds

          iindex=70
          third='demand'
          call jumpreport(ifu,iindex,zedsdoc(iset),third,atreports)
          if(.NOT.atreports)then
            goto 145
          endif

C Skip title and format and fields and read demand data line
  43      CALL STRIPC(ifu,OUTSTR,0,ND,1,'data',IER)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
          if(WORD(1:5).eq.'*data')then
           CALL EGETWR(OUTSTR,K,HDS(ik,iset),0.,0.,'-','heat dmd',IER)
           CALL EGETWR(OUTSTR,K,CDS(ik,iset),0.,0.,'-','cool dmd',IER)
           CALL EGETWR(OUTSTR,K,ZLUDS(ik,iset),0.,0.,'-','ltuc dmd',IER)
           CALL EGETWR(OUTSTR,K,ZLDS(ik,iset),0.,0.,'-','ltct dmd',IER)
           CALL EGETWR(OUTSTR,K,FDS(ik,iset),0.,0.,'-','fans dmd',IER)
           CALL EGETWR(OUTSTR,K,SPDS(ik,iset),0.,0.,'-','smp dmd',IER)
           CALL EGETWR(OUTSTR,K,DHWDS(ik,iset),0.,0.,'-','dhw dmd',IER)
          else
            goto 43
          endif
  145   continue

C Skip title and format and fields and read demand data line
        if(dmdsok)then

C Position pointer to get aggregate for dispersed demands.
          iindex=71
          name='Dispersed'
          third='demand'
          call jumpreport(ifu,iindex,name,third,atreports)
          if(.NOT.atreports)then
            continue
          else
  243       CALL STRIPC(ifu,OUTSTR,99,ND,1,'data',IER)
            K=0
            CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
            if(WORD(1:5).eq.'*data')then
             CALL EGETWR(OUTSTR,K,ZLUDS(ik,11),0.,0.,'-','ltuc dmd',IER)
             CALL EGETWR(OUTSTR,K,ZLDS(ik,11),0.,0.,'-','ltct dmd',IER)
             CALL EGETWR(OUTSTR,K,dfan,0.,0.,'-','fans dmd',IER)
             CALL EGETWR(OUTSTR,K,dpump,0.,0.,'-','pump dmd',IER)
             CALL EGETWR(OUTSTR,K,dlifts,0.,0.,'-','lifts dmd',IER)
             FDS(ik,11)=dfan+dpump+dlifts
             CALL EGETWR(OUTSTR,K,SPDS(ik,11),0.,0.,'-','smp dmd',IER)
             CALL EGETWR(OUTSTR,K,DHWDS(ik,11),0.,0.,'-','dhw dmd',IER)
             if(ND.eq.9)then
               CALL EGETWR(OUTSTR,K,DPVD(ik,11),0.,0.,'-','PV sply',IER)
             endif
            else
              goto 243
            endif
          endif
        endif

C Find denominator (scaled floor area), then for each of the
C energy demand sets get their contribution to each type of data.
        denom=0.0
        do 61 ij=1,neds
          denom=denom +(edgflr(ij)*edgsca(ij))
          WHD(ik) = WHD(ik) +(HDS(ik,ij)*edgsca(ij))
          WCD(ik) = WCD(ik) +(CDS(ik,ij)*edgsca(ij))
          WLD(ik) = WLD(ik) +((ZLUDS(ik,ij)+ZLDS(ik,ij))*edgsca(ij))
          WFD(ik) = WFD(ik) +(FDS(ik,ij)*edgsca(ij))
          WSPD(ik) = WSPD(ik) +(SPDS(ik,ij)*edgsca(ij))
          WDHWD(ik) = WDHWD(ik) +(DHWDS(ik,ij)*edgsca(ij))
  61    continue

C Add in the dispersed demands for each type of data.
        WHD(ik) = (WHD(ik) + HDS(ik,11))/denom
        WCD(ik) = abs(WCD(ik) + CDS(ik,11))/denom
        WLD(ik) = (WLD(ik) + (ZLUDS(ik,11)+ZLDS(ik,11)))/denom
        WFD(ik) = (WFD(ik) + FDS(ik,11))/denom
        WSPD(ik) = (WSPD(ik) + SPDS(ik,11))/denom
        WDHWD(ik) = (WDHWD(ik) + DHWDS(ik,11))/denom
        WPVD(ik)=DPVD(ik,11)/denom
  144 continue

C Debug.
C      write(iuout,*) 'denom ',denom
C      write(iuout,*) 'WHD ',WHD
C      write(iuout,*) 'WCD ',WCD
C      write(iuout,*) 'WLD ',WLD
C      write(iuout,*) 'WFD ',WFD
C      write(iuout,*) 'WSPD ',WSPD
C      write(iuout,*) 'WDHWD ',WDHWD
C      write(iuout,*) 'WPVD ',WPVD

C close the file and return.
      CALL  ERPFREE(ifu,ISTAT)
      return
      end

C *********** ipvseadmd
C Recover seasonal demand data (per m^2) from intermediate IPV report (not converted
C to primary units). Scan requested assessment (ism) and return values.
C ipvseadmd is called from mkannrep.
C << at some point include additional arrays for pumps and lifts >>
      subroutine ipvseadmd(ifu,ism,dmdsok,SWHD,SWCD,SWLD,SWFD,SWSPD,
     &   SWDHWD,SWPVD,denom,ier)  

#include "building.h"
#include "espriou.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

C Uncomment the next line if debugging required.
      common/OUTIN/IUOUT,IUIN
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

C For each type of demand, and for one more than the possible
C number of demand sets (the extra is for distributed demands).
      dimension HDS(11),CDS(11)
      dimension ZLUDS(11),ZLDS(11),FDS(11)
      dimension SPDS(11),DHWDS(11),DPVD(11)
      character outstr*124,act*1,word*20
      character name*12,third*20
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72
      logical dmdsok,atassment,atreports

C Open intermediate report.
      CALL EFOPSEQ(ifu,sipvres,1,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') sipvres(1:lnblnk(sipvres))

C Clear data arrays.
      SWHD=0.0
      SWCD=0.0
      SWLD=0.0
      SWFD=0.0
      SWSPD=0.0
      SWDHWD=0.0
      SWPVD=0.0
      do 78 ij=1,11
        HDS(ij)=0.0
        CDS(ij)=0.0
        ZLUDS(ij)=0.0
        ZLDS(ij)=0.0
        FDS(ij)=0.0
        SPDS(ij)=0.0
        DHWDS(ij)=0.0
        DPVD(ij)=0.0
  78  continue

      ik=ism

C Position pointer at the start of the relevant assessment.
      call jumpassmt(ifu,ik,atassment)
      if(.NOT.atassment)then
        return
      endif

C Position pointer to get aggregate for each energy demand set.
      do 145 iset=1,neds

        iindex=70
        third='demand'
        call jumpreport(ifu,iindex,zedsdoc(iset),third,atreports)
        if(.NOT.atreports)then
          goto 145
        endif

C Skip title and format and fields and read demand data line
  43    CALL STRIPC(ifu,OUTSTR,0,ND,1,'data',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
        if(WORD(1:5).eq.'*data')then
         CALL EGETWR(OUTSTR,K,HDS(iset),0.,0.,'-','heat dmd',IER)
         CALL EGETWR(OUTSTR,K,CDS(iset),0.,0.,'-','cool dmd',IER)
         CALL EGETWR(OUTSTR,K,ZLUDS(iset),0.,0.,'-','ltuc dmd',IER)
         CALL EGETWR(OUTSTR,K,ZLDS(iset),0.,0.,'-','ltct dmd',IER)
         CALL EGETWR(OUTSTR,K,FDS(iset),0.,0.,'-','fans dmd',IER)
         CALL EGETWR(OUTSTR,K,SPDS(iset),0.,0.,'-','smp dmd',IER)
         CALL EGETWR(OUTSTR,K,DHWDS(iset),0.,0.,'-','dhw dmd',IER)
        else
          goto 43
        endif
  145 continue

C Skip title and format and fields and read demand data line
      if(dmdsok)then

C Position pointer to get aggregate for dispersed demands.
        iindex=71
        name='Dispersed'
        third='demand'
        call jumpreport(ifu,iindex,name,third,atreports)
        if(.NOT.atreports)then
          continue
        else
  243     CALL STRIPC(ifu,OUTSTR,99,ND,1,'data',IER)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
          if(WORD(1:5).eq.'*data')then
            CALL EGETWR(OUTSTR,K,ZLUDS(11),0.,0.,'-','ltuc dmd',IER)
            CALL EGETWR(OUTSTR,K,ZLDS(11),0.,0.,'-','ltct dmd',IER)
            CALL EGETWR(OUTSTR,K,dfan,0.,0.,'-','fans dmd',IER)
            CALL EGETWR(OUTSTR,K,dpump,0.,0.,'-','pump dmd',IER)
            CALL EGETWR(OUTSTR,K,dlifts,0.,0.,'-','lifts dmd',IER)
            FDS(11)=dfan+dpump+dlifts
            CALL EGETWR(OUTSTR,K,SPDS(11),0.,0.,'-','smp dmd',IER)
            CALL EGETWR(OUTSTR,K,DHWDS(11),0.,0.,'-','dhw dmd',IER)
            if(ND.eq.9)then
              CALL EGETWR(OUTSTR,K,DPVD(11),0.,0.,'-','PV sply',IER)
            endif
          else
            goto 243
          endif
        endif
      endif

C Find denominator (scaled floor area), then for each of the
C energy demand sets get their contribution to each type of data.
      denom=0.0
      do 61 ij=1,neds
        denom=denom +(edgflr(ij)*edgsca(ij))
        SWHD = SWHD +(HDS(ij)*edgsca(ij))
        SWCD = SWCD +(CDS(ij)*edgsca(ij))
        SWLD = SWLD +((ZLUDS(ij)+ZLDS(ij))*edgsca(ij))
        SWFD = SWFD +(FDS(ij)*edgsca(ij))
        SWSPD = SWSPD +(SPDS(ij)*edgsca(ij))
        SWDHWD = SWDHWD +(DHWDS(ij)*edgsca(ij))
  61  continue

C Add in the dispersed demands for each type of data.
      SWHD = (SWHD + HDS(11))/denom
      SWCD = abs(SWCD + CDS(11))/denom
      SWLD = (SWLD + (ZLUDS(11)+ZLDS(11)))/denom
      SWFD = (SWFD + FDS(11))/denom
      SWSPD = (SWSPD + SPDS(11))/denom
      SWDHWD = (SWDHWD + DHWDS(11))/denom
      SWPVD = DPVD(11)/denom

C Debug.
C      write(iuout,*) 'denom ',denom
C      write(iuout,*) 'SWHD ',SWHD,' SWCD ',SWCD,' SWLD ',SWLD
C      write(iuout,*) 'SWFD ',SWFD,' SWSPD ',SWSPD,'SWDHWD ',SWDHWD
C      write(iuout,*) 'SWPVD ',SWPVD

C close the file and return.
      CALL  ERPFREE(ifu,ISTAT)
      return
      end

C *********** ipvgcmft
C Recover comfort data from a seasonal report.
C Note if Java IPV because different logic required.
      subroutine ipvgcmft(ifu,NH,ier)  
#include "building.h"
#include "espriou.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

C Uncomment the next line if debugging required.
C      common/OUTIN/IUOUT,IUIN
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

      character outstr*124,word*20
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,third*20
      dimension nh(9,5)
      logical atassment,atreports

C Open intermediate report.
      CALL EFOPSEQ(ifu,sipvres,1,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') sipvres(1:lnblnk(sipvres))

C Clear data arrays.
      do 77 ik=1,nipvassmt
        do 78 ij=1,9
          nh(ij,ik)=0
  78    continue
  77  continue

C For each assessment.
      do 144 ik=1,nipvassmt

C Position pointer at the start of the relevant assessment.
        call jumpassmt(ifu,ik,atassment)
        if(.NOT.atassment)then
          goto 144
        endif

C Position pointer to get frequency for each metric set if it
C has iget index of 6 << could be more flexable >>.
C Pass it the group name for this comfort set (the ipv description
C might include several of type comfort) << still to be detected >>
        do 145 iset=1,nms
          if(imetget(iset).eq.6)then
            iindex=6
            third='distribution'
            call jumpreport(ifu,iindex,metgroup(iset),third,atreports)
            if(.NOT.atreports)then
              goto 145
            endif

C Skip title and format and fields and read capacity data line
  43        CALL STRIPC(ifu,OUTSTR,0,ND,1,'distrib data',IER)
            K=0
            CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
            if(WORD(1:5).eq.'*data')then
              continue
            else
              goto 43
            endif

C Scan 9 lines for comfort info and then read 2nd or 3rd item in bin info.
            do 44 i=1,9
              CALL STRIPC(ifu,OUTSTR,99,ND,1,'bin data',IER)
              K=0
              if(ipvform.eq.3)then
                continue
              else
                CALL EGETWI(OUTSTR,K,ib,0,0,'-','bin no',IER)
              endif
              CALL EGETW(OUTSTR,K,WORD,'W','range',IFLAG)
              CALL EGETWI(OUTSTR,K,NH(i,ik),0,0,'-','cmft bin',IER)
  44        continue
          else
            continue
          endif
 145    continue
 144  continue
      CALL  ERPFREE(ifu,ISTAT)
      return
      end

C ************** aggreghd
C Hourly aggregate energy demand per unit time.
      subroutine aggreghd(itru,iseas,pvok)
#include "building.h"
#include "ipvdata.h"

C Uncomment the next line if debugging required.
C      common/OUTIN/IUOUT,IUIN
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/BL2/dmdfla(7)

      COMMON/GETPIK/NGET,IGETNO(MZS,9)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

C IPV description via ipvdata.h.

C << is this required? >>
      common/spm55/pvdayhr(2*MTS)

      dimension edput(7,MTS)
      character tab*1,outs*124,xfile*144,tg*1,delim*1
      logical pvok

C Set separator as in rest of seasonal report.
      if(delim.eq.'-')then
        tab=' '
      elseif(delim.eq.'S')then
        tab=' '
      elseif(delim.eq.'T')then
        tab=CHAR(9)
      elseif(delim.eq.'C')then
        tab=','
      endif

      pvfla=1.
      if(pvok)then
        pvfla=AMAX1(pvfla,dmdfla(1),dmdfla(2),dmdfla(3),dmdfla(4),
     &              dmdfla(5),dmdfla(6),dmdfla(7))

C Debug.
C       write(6,*) 'pvfloor area is ',pvfla

      endif

C Set the recovery period to match seasonal report.
      ihr=0
      IOH1=1
      IOH2=24

      if(iseas.eq.1)then
        call EDAYR(ipvastjd(1),IOD1,IOM1)
        call EDAYR(ipvafnjd(1),IOD2,IOM2)
      elseif(iseas.eq.2)then
        call EDAYR(ipvastjd(2),IOD1,IOM1)
        call EDAYR(ipvafnjd(2),IOD2,IOM2)
      elseif(iseas.eq.3)then
        call EDAYR(ipvastjd(3),IOD1,IOM1)
        call EDAYR(ipvafnjd(3),IOD2,IOM2)
      elseif(iseas.eq.4)then
        call EDAYR(ipvastjd(4),IOD1,IOM1)
        call EDAYR(ipvafnjd(4),IOD2,IOM2)
      elseif(iseas.eq.5)then
        call EDAYR(ipvastjd(5),IOD1,IOM1)
        call EDAYR(ipvafnjd(5),IOD2,IOM2)
      endif
      CALL EDAY(IOD1,IOM1,IODS)
      CALL EDAY(IOD2,IOM2,IODF)

C Find out how many hours in the tabular report.
      ihr=0
      do 100 IDAY=IODS,IODF
        call chktime(iday,istart,iend)
        do 120 j=ISTART,IEND,NOUT
          ihr=ihr+1
  120   continue
  100 continue
      call edisp(itru,' ')
      write(outs,'(7a)') '*report',tab,'72',tab,
     &  '*delivered_per_unit_time',tab,'Aggregate'
      call edisp(itru,outs)
      write(outs,'(5a)')'*title',tab,
     &  'Energy Delivered per Unit Time',tab,'W/m^2'
      call edisp(itru,outs)
      if(pvok)then
        write(outs,'(4a,i4,2a)') '*format',tab,'tabular',tab,ihr,tab,'7'
        call edisp(itru,outs)
        write(outs,'(17A)',IOSTAT=IOS,ERR=1) '*fields',tab,'Time',
     &  tab,'Heating',tab,'Cooling',tab,'Lighting',tab,
     &  'Fans pumps lifts',tab,'Small Power',tab,'Hot water',tab,'PV'
      else
        write(outs,'(4a,i4,2a)') '*format',tab,'tabular',tab,ihr,tab,'6'
        call edisp(itru,outs)
        write(outs,'(15A)',IOSTAT=IOS,ERR=1) '*fields',tab,'Time',
     &  tab,'Heating',tab,'Cooling',tab,'Lighting',tab,
     &  'Fans pumps lifts',tab,'Small Power',tab,'Hot water'
      endif
      call edisp(itru,outs)
      call edisp(itru,'*data ')
      ihr=0

C Loop through each day of the period, clear array and then recover
C data for energy demand sets and non-zone demands.
      denom=0.0
      do 61 ij=1,neds
        denom=denom +(edgflr(ij)*edgsca(ij))
 61   continue

      ipvhr=0
      do 10 IDAY=IODS,IODF
        call chktime(iday,istart,iend)
        do 11 I=1,7
          do 12 j=1,24*NTS
            edput(i,j)=0.
   12     continue
   11   continue
        ISET=ISIM

C Loop through each of the current demand sets.
        do 888 ij=1,neds
          NZ=nzedg(ij)
          if(NZ.eq.0)then

C If no zones, write out place holder for heating and cooling.
            call edisp(itru,'No zones in this demand set.')
          else
            NGET=nzedg(ij)
            DO 60 I=1,NZ
              NZNO(I)=izedg(ij,I)

C Setup recovery options for heating flux then cooling flux and then
C uncontrolled and controlled lighting.
              IGETNO(I,1)=8
              IGETNO(I,2)=izedg(ij,I)
              IGETNO(I,3)=0
              IGETNO(I,4)=0
              IGETNO(I,5)=ISET
              SLABEL(I)='Heating flux injection (W)'
              LNSLABEL(I)=26
   60       CONTINUE
            call GOGET(IDAY)
            DO 161 I=1,NZ
              do 62 j=ISTART,IEND,NOUT
                edput(1,j)=edput(1,j)+
     &            (((VAL2(i,j)*edgsca(ij))/denom)*1000.0)
   62         continue
  161       continue
            DO 63 I=1,NZ
              IGETNO(I,1)=9
              SLABEL(I)='Cooling flux (W)'
              LNSLABEL(I)=16
   63       CONTINUE
            call GOGET(IDAY)
            DO 64 I=1,NZ
              do 265 j=ISTART,IEND,NOUT
                edput(2,j)=edput(2,j)+
     &            (abs((VAL2(i,j)*edgsca(ij))/denom)*1000.0)
  265         continue
   64       continue
            DO 66 I=1,NZ
              IGETNO(I,1)=15
              IGETNO(I,3)=5
              SLABEL(I)='Lighting (unctld) gains (W)'
              LNSLABEL(I)=27
   66       CONTINUE
            call GOGET(IDAY)
            DO 67 I=1,NZ
              do 68 j=ISTART,IEND,NOUT
                edput(3,j)=edput(3,j)+((VAL2(i,j)*edgsca(ij))/denom)
   68         continue
   67       continue
            DO 70 I=1,NZ
              IGETNO(I,1)=15
              IGETNO(I,3)=7
              SLABEL(I)='Lighting (ctld) gains (W)'
              LNSLABEL(I)=25
   70       CONTINUE
            call GOGET(IDAY)
            DO 71 I=1,NZ
              do 72 j=ISTART,IEND,NOUT
                edput(3,j)=edput(3,j)+((VAL2(i,j)*edgsca(ij))/denom)
   72         continue
   71       continue

C Small power (assumed to be equipment casual gains)
            DO 74 I=1,NZ
              IGETNO(I,1)=15
              IGETNO(I,3)=6
              SLABEL(I)='Small power (W)'
              LNSLABEL(I)=15
   74       CONTINUE
            call GOGET(IDAY)
            DO 75 I=1,NZ
              do 76 j=ISTART,IEND,NOUT
                edput(5,j)=edput(5,j)+((VAL2(i,j)*edgsca(ij))/denom)
   76         continue
   75       continue
          endif
 888    continue

C For non-zone-specific demands.
        call DDMDS(IDAY)
        do 182 j=ISTART,IEND,NOUT
          if(idmdinc(2).eq.1.and.dmdfla(2).gt.0.001)then
            edput(3,j)= edput(3,j) + (VAL3(16,j)/dmdfla(2))
          endif
          if(idmdinc(3).eq.1.and.dmdfla(3).gt.0.001)then
            edput(5,j)= edput(5,j) + (VAL3(17,j)/dmdfla(3))
          endif
          if(idmdinc(4).eq.1.and.dmdfla(4).gt.0.001)then
            edput(4,j)= edput(4,j) + (VAL3(18,j)/dmdfla(4))
          endif
          if(idmdinc(5).eq.1.and.dmdfla(5).gt.0.001)then
            edput(4,j)= edput(4,j) + (VAL3(19,j)/dmdfla(5))
          endif
          if(idmdinc(6).eq.1.and.dmdfla(6).gt.0.001)then
            edput(4,j)= edput(4,j) + (VAL3(20,j)/dmdfla(6))
          endif
          if(idmdinc(7).eq.1.and.dmdfla(7).gt.0.001)then
            edput(6,j)= edput(6,j) + (VAL3(21,j)/dmdfla(7))
          endif
  182   continue

C Write out hourly results.
        do 90 j=ISTART,IEND,NOUT
          ihr=ihr+1
          if(pvok)then
            ipvhr=ipvhr+1
            write(outs,'(i4,7F12.3)')ihr,edput(1,j),edput(2,j),
     &        edput(3,j),edput(4,j),edput(5,j),edput(6,j),
     &        pvdayhr(ipvhr)/pvfla
          else
            write(outs,'(i4,6F12.3)')ihr,edput(1,j),edput(2,j),
     &        edput(3,j),edput(4,j),edput(5,j),edput(6,j)
          endif
          call eddisp(itru,outs)
   90   continue
  10  continue
      call edisp(itru,'*end_report')
      call edisp(itru,' ')
      return

 1    if(IOS.eq.2)then
        CALL USRMSG('No permission to write aggregate data..',' ','W')
      else
        CALL USRMSG('Problem writing aggregate data.',' ','W')
      endif
      CALL  ERPFREE(ixunit,ISTAT)
      return
      end

C ********** aggregpv
C Recover pv electrical output in terms of an hourly integration
C of all nodes (W).
      subroutine aggregpv(iseas,pvok,pvmax,totpv,iier)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

C Special materials file flag
      common/spmfxst/ispmxist,spflnam  
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      common/spm55/pvdayhr(2*MTS)

      character outs*124,H*72,spflnam*72
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72
      logical OK,DOK,XST,pvok

C Special material output.
      iier=0
      do 4 i=1,MTS*2
        pvdayhr(i)=0.
   4  continue

C If no special materials defined then do not bother asking
C for output file.
      if(ispmxist.eq.0)then
        pvok=.false.
        iier=0
        return
      endif
      dok=.true.
      h(1)='There are active/special materials in the model and'
      h(2)='thus there might be a report generated for their '
      h(3)='performance. If you know the name say yes and '
      h(4)='then supply the name of the file. '
      call askok('Is there a special materials output file',
     &           'associated with this simulation?',OK,dok,4)
      if(.NOT.OK)then
        pvok=.false.
        iier=0
        return
      endif

C The following section of code was written to scan a separate
C PV output file which no longer exists. This has been commented
C out pending replacement with pattern used elsewhere in res.
C Open the special materials file to check number of entries and
C which ones relate to PV.
       pvok=.false.
C      ispmfil=ifil+1
C      if(ispmxist.gt.0)then
C        call erpfree(ispmfil,istat)
C        call FINDFIL(spflnam,XST)
C        if(XST)then
C          call efopseq(ispmfil,spflnam,1,IER)
C          if(IER.NE.0) then
C            write(outs,'(a,a)') 'Error opening file ',spflnam
C            call edisp(iuout,outs)
C            call erpfree(ispmfil,istat)
C            pvok=.false.
C            iier=1
C            return
C          else
C            call erpfree(ispmfil,istat)
C            call spminit
C            call erpfree(ispmfil,istat)
C            pvok=.true.
C          endif
C        endif

C Read power data. << standard call to ?? needs to be implemented>>
C        if(istep.eq.1)then
C          call edisp(iuout,' Reporting frequency is once per hour.')
C          IRTB=1
C        else
C          IRTB=2
C        endif
C      endif

      return
      end

C IPVEXP ***********
C IPVEXP: Control feedback to export file or text feedback area. This
C is a variant of ctlexp which does not create a separate parameter
C file and allows appending to an existing file.  This is a variant
C of esrucom/ctlexp.F subroutine ctlexp.F The idea of an act command
C might be of interest in the standard version. This subroutine might
C move into esrucom/ctlexp.F in the future (once there is no name
C clash with the two variants of ctlexp (res version and everything else).

C Assign user-specified export file - checking if local or remote.
C act = 'o' to open new file, act = 'a' to append to an existing
C file. tg is a character T text or X tabular info being saved.
      subroutine ipvexp(xfile,ixopen,ixunit,act,tg,msg,IER)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/rcmd/LCMDFL
      common/rpath/path

C Simulator parameters. autook signals silent operation.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok

      character*(*) msg
      character H*72,xfile*144,path*72,outs*124,outs248*248
      character tg*1,tfile*72,act*1
      character LCMDFL*144,longtfile*144,longtfiledos*144
      LOGICAL spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical concat,remote,unixok,there

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Each call is a toggle.
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs248,'(2a)') 'closing export file: ',xfile
        call edisp248(iuout,outs248,100)
        if(tg.eq.'X')then
          CALL ERPFREE(ixunit,ISTAT)
          call usrmsg(' ','reporting >> to display ','-')
        elseif(tg.eq.'T')then
          CALL ERPFREE(ixunit,ISTAT)
        endif
      elseif(ixopen.eq.1)then
        iw=0
        remote=.false.
        if(unixok)then
          if(path(1:2).ne.'./'.and.path(1:2).ne.'  ')remote=.true.
        else
          if(path(1:2).ne.'  ')remote=.true.
          if(ichar(path(1:1)).eq.46.and.
     &       ichar(path(2:2)).eq.92)remote=.false.
        endif
        if(remote)then
          write(outs,'(A,A)') ' The current path is: ',path
          call edisp(iuout,outs)
          if (autook) then
            IW=2   ! iw needs to be non-zero for subsequent code to work.
          else
            h(1)='If using path then the export file will be placed in'
            h(2)='the model folder. Otherwise nominate a folder and'
            h(3)='file name for the export file (i.e. /tmp/junk.exp or'
            h(4)='/home/ralph/reports/junk.exp) '
            h(5)=' '
            h(6)='If you selected this option in error click continue.'
            write(outs,'(A,A)') ' The model is in folder ',path
            CALL EASKABC(outs,' place export file:',
     &        'in the model folder','user defined folder','continue',
     &        iw,6)
            if(iw.eq.3)then
              ixopen=0
              return    ! does ixopen need to be reset?
            endif
          endif
        else
          IW=2   ! iw needs to be non-zero for subsequent code to work.
        endif
        h(1)='This file is a text file which can be used in'
        h(2)='reports or for third party tools. '
        if (.NOT.autook) then
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          llt=lnblnk(xfile)
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              CALL EASKF(xfile,' Export file name? ',
     &          ' ',96,'export.txt','export file name',IER,2)
            elseif(llt.ge.96.and.llt.lt.124)then
              CALL EASKF(xfile,' Export file name? ',
     &          ' ',124,'export.txt','export file name',IER,2)
            elseif(llt.ge.124.and.llt.le.144)then
              CALL EASKF(xfile,' Export file name? ',
     &          ' ',144,'export.txt','export file name',IER,2)
            endif
          elseif(iglib.eq.2)then
            CALL EASKF(xfile,' Export file name? ',
     &        ' ',144,'export.txt','export file name',IER,2)
          else
            CALL EASKF(xfile,' Export file name? ',
     &        ' ',96,'export.txt','binary events db',IER,2)
          endif

C If user request cancel then return after resetting ixopen to zero.
          if(ier.eq.-3)then
            ixopen=0
            return
          endif
        endif
        write(outs248,'(3A)')' opened ',xfile(1:lnblnk(xfile)),
     &                    ' for export.'
        call edisp248(iuout,outs248,100)
        if(iw.eq.1)then
          if(tg.eq.'X'.or.tg.eq.'T')then
            if(act.eq.'o'.or.act.eq.'O')then
              call efopseq(ixunit,xfile,4,IER)
            elseif(act.eq.'a'.or.act.eq.'A')then

C If appending check and see if it exists. If so close it and then
C open it with APPEND option.
              longtfile=' '
              if(unixok)then
                call addpath(xfile,longtfile,concat)
              else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
                call addpath(xfile,longtfile,concat)
                call cmdfiledos(longtfile,longtfiledos,ier)
                longtfile=' '
                longtfile=longtfiledos
              endif
              ltf=max(1,LNBLNK(longtfile))
              there=.false.
              INQUIRE(FILE=longtfile(1:ltf),EXIST=there)
              if(there)then
                close(ixunit)
               open(ixunit,file=longtfile(1:ltf),position='APPEND',
     &           status='UNKNOWN',err=1)
              else
                call efopseq(ixunit,xfile,4,IER)
              endif
              if(ier.ne.0)return
            endif
          endif
        elseif(iw.eq.2)then
          if(tg.eq.'X'.or.tg.eq.'T')then
            if(act.eq.'o'.or.act.eq.'O')then
              CALL ERPFREE(ixunit,ISTAT)
              call FPOPEN(ixunit,ISTAT,1,3,xfile)
              if(ISTAT.lt.0)return
            elseif(act.eq.'a'.or.act.eq.'A')then

C If appending check and see if it exists. If so close it and then
C open it with APPEND option.
              ltf=max(1,LNBLNK(xfile))
              there=.false.
              INQUIRE(FILE=xfile(1:ltf),EXIST=there)
              if(there)then
                close(ixunit)
               open(ixunit,file=xfile(1:ltf),position='APPEND',
     &           status='UNKNOWN',err=1)
              else
                call efopseq(ixunit,xfile,4,IER)
              endif
              if(ier.ne.0)return
            endif
          endif
        elseif(iw.eq.3)then
          return
        endif
        if(iw.eq.1)then
          write(outs248,'(1x,3A)') msg(1:lnblnk(msg)),
     &      ' reporting >> to ',longtfile(1:lnblnk(longtfile))
        else
          write(outs248,'(1x,3A)') msg(1:lnblnk(msg)),
     &      ' reporting >> to ',xfile(1:lnblnk(xfile))
        endif
        call edisp248(iuout,outs248,100)
      endif
      RETURN

 1    CALL USRMSG(' Problem writing IPV report.',' ','W')
      CALL  ERPFREE(ixunit,ISTAT)
      return
      END


C ********************** bemsrep **********************
C bemsrep: Write IPV report based on description held in common.
      subroutine bemsrep(actf,bact,iier)
#include "building.h"

      common/OUTIN/IUOUT,IUIN

C nrepro number of groups of entities of interest for BEMS.
C resgrp(i) types of variables (i.e. 1=temps, 2=fluxes, 3=power etc.)
C resloc(i,j) for each variable type (i.e. 1,1=for group 1, slot 1)
C izb() specific zones of interest. 
C isb() 2nd index to specify model entity.
C bemstime time (fraction of julian day) for assessment.
      character actf*72,bact*4

C Call the bems interaction file.
C      call rintera(actf,iier)
C      call rebsim(bact,'RES')

      return
      end

C **************** jumpassmt
C jumpassmt rewinds and then scans intermidiate IPV file for
C a line with *assessment followed by an index that matches
C the index parameter. When found it returns having set
C the logical atassment to .true.
      subroutine jumpassmt(ifu,index,atassment)
      common/OUTIN/IUOUT,IUIN

      character outstr*124
      logical atassment

C Position pointer at the start of the relevant assessment.
      REWIND(IFU,ERR=999)
      line=0
      atassment=.false.
 143  CALL STRIPC(ifu,OUTSTR,0,ND,1,'assessment',IER)
      IF(IER.NE.0)goto 1
      if(OUTSTR(1:11).eq.'*assessment')then
        K=11
        CALL EGETWI(OUTSTR,K,id1,1,5,'W','assessment nb',IIER)
        if(id1.eq.index)then

C Debug.
C          write(6,*) 'at line number ',line

          atassment=.true.
          return
        else
          line=line+1

C Debug.
C          write(6,*) 'skipping assessment at line number ',line

          goto 143
        endif
      else
        line=line+1
        goto 143
      endif

  999 write(iuout,*)' problem rewinding the intermediate file'
    1 return
      end


C **************** jumpreport
C jumpreport scans intermidiate IPV file for
C a line with *report followed by an IGET index (iindex) to match
C and the set 'name' to match or 'Dispersed' and 'third' to match
C the type of data (diversified/diffuse/maximum/demand/etc.)
C It returns the logical atreport to .true. if the correct
C report line has been found.
      subroutine jumpreport(ifu,iindex,name,third,atreports)
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN

      character outstr*124,word1*20,word2*20,word3*20,third*20,name*12
      logical atreports,okname,oktype

C Set test logic to false.
      atreports=.false.
      okname=.false.
      oktype=.false.
 143  CALL STRIPC(ifu,OUTSTR,0,ND,1,'*report',IER)
      IF(IER.NE.0)goto 1
      if(OUTSTR(1:7).eq.'*report')then
        K=7
        CALL EGETWI(OUTSTR,K,id1,1,75,'W','iget',IIER)
        if(id1.eq.iindex)then

C Check and see if the type of report matches.
          CALL EGETW(OUTSTR,K,WORD1,'W','third',IFLAG)
          lz=lnblnk(third)
          lw=lnblnk(WORD1)
          if((WORD1(1:lw).eq.third(1:lz)).or.
     &       (WORD1(1:lw).eq.third(1:lz-1)))then
            oktype=.true.
          endif
          CALL EGETW(OUTSTR,K,WORD2,'W','metric clarification',IFLAG)
          CALL EGETW(OUTSTR,K,WORD3,'W','set name',IFLAG)
          if(WORD3(1:9).eq.'Dispersed')then
            okname=.true.
          else

C Check to see if name matches WORD3 either exactly or
C in the case of the name string having a trailing comma.
            lz=lnblnk(name)
            if((WORD3(1:lnblnk(WORD3)).eq.name(1:lz)).or.
     &         (WORD3(1:lnblnk(WORD3)).eq.name(1:lz-1)))then
              okname=.true.
            else
              goto 143
            endif
          endif

C If sufficient criteria return, otherwise read further in file.
          if(okname.and.oktype)then
            atreports=.true.
            return
          else
            goto 143
          endif
        else
          goto 143
        endif
      else
        goto 143
      endif
  1   return
      end
