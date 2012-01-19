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

C Subroutine included:
C IPVCMD: act on command line IPV directives.
C IPVDAT: define information used in generating IPV.
C ipvseasons: presents a list of typical seasons/assessments and
C             returns a string which sumarizes the choice.
C ipvdatinit: initializes IPV data structures based on the passed act
C IPV2SIMPAR: copy relevant data from IPV description to
C             simulation parameter sets.
C getmultip: looks for typical week in each season based on closest
C            degree days and radiation patterns.

C ************* IPVCMD
C IPVCMD acts on command line IPV directives. Allows model to be
C altered based on information held in IPV definition.
      subroutine IPVCMD(act,inf)
#include "building.h"
#include "model.h"
#include "uncertainty.h"
#include "espriou.h"
#include "control.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/C21/IFCFG,cfgroot,LCFGF

C Path to model and command line file (if any). Browse
C is a logical flag, .true. restricts update/save options
      common/rcmd/LCMDFL

C IPV description via ipvdata.h.
      common/IPVF/lipvdatf

C IDST tool commands
      common/idsta/CMDCFG,CMDCLM,CMDCLMN,CMDNOTE,CMDRES,CMDLBL(10),
     &             CMDLNAM(10)
      common/idstb/nidst
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)

      character*(*) inf
      character LCFGF*72
      character LCMDFL*144,outs*124,outstr*124,cfgroot*24
      character lipvdatf*72
      character CMDCFG*72,CMDCLM*72,CMDCLMN*72,CMDNOTE*64,CMDRES*72
      character ALOC*12,ALOCLBL*12,ALOCTYP*4,CMDLBL*12,CMDLNAM*80
      CHARACTER word*24
      character cpth*48,cmdn*48,runmode*12,keepres*4,donegdst*72
      character ipvaction*3  ! to signal external ipv file
      character act*1

C Check if IPV file known and fill common blocks before reading command data.
      if(icfgv.lt.4)then
        if(lnblnk(lipvdatf).eq.0)then
          call usrmsg('IPV not defined. Cannot process command.',
     &      ' ','W')
          goto 223
        elseif(lipvdatf(1:7).eq.'UNKNOWN')then
          call usrmsg('IPV UNKNOWN. Cannot process command.',' ','W')
          goto 223
        else
	  ipvaction='ipv'
          call ripvdat(IFCFG,lipvdatf,ipvaction,ier)
        endif
      else
        if(nipvassmt.eq.0)then ! there was nothing in the cfg file.
          call usrmsg('IPV not defined. Cannot process command.',
     &      ' ','W')
          goto 223
        endif
      endif
      
      runmode='interactive'
      keepres='yes'
      LCMDFL=inf
      donegdst=' '
      write(donegdst,'(a,a5)')inf(1:lnblnk(inf)),'.done'
      nidst=0
      CALL ERPFREE(IFCFG,ISTAT)
      call FPOPEN(IFCFG,ISTAT,1,1,LCMDFL)
      CALL STRIPC(IFCFG,OUTSTR,0,ND,1,'IDST header',IER)
      if(OUTSTR(1:14).eq.'*IDST COMMANDS')then
222     CALL STRIPC(IFCFG,OUTSTR,0,ND,0,'tags',IIER)
        if(iier.ne.0)then

C Debug.
C          write(6,*)'idst ',nidst

          goto 223
        endif
        if(OUTSTR(1:5).eq.'*note')then
          k=6
          CALL EGETRM(OUTSTR,K,CMDNOTE,'W','idst cmd note',IER)
        elseif(OUTSTR(1:9).eq.'*run_mode')then
          k=10
          CALL EGETW(OUTSTR,K,runmode,'W','idst runmod',IER)
        elseif(OUTSTR(1:13).eq.'*keep_results')then
          k=14
          CALL EGETW(OUTSTR,K,keepres,'W','idst keep',IER)
        elseif(OUTSTR(1:9).eq.'*abs_path')then
          k=10
          CALL EGETRM(OUTSTR,K,cpth,'W','idst cfg path',IER)
        elseif(OUTSTR(1:8).eq.'*win_cfg')then
          k=8
          CALL EGETRM(OUTSTR,K,CMDCFG,'W','idst winter model cfg',IER)
          if(cpth(1:2).ne.'  ')then
            write(CMDCFG,'(a,a)') cpth(1:lnblnk(cpth)),
     &        cmdn(1:lnblnk(cmdn))
          else
            write(CMDCFG,'(a)') cmdn(1:lnblnk(cmdn))
          endif
          LCMDFL=CMDCFG
        elseif(OUTSTR(1:8).eq.'*tra_cfg')then
          k=8
          CALL EGETRM(OUTSTR,K,CMDCFG,'W','idst trans model cfg',IER)
          if(cpth(1:2).ne.'  ')then
            write(CMDCFG,'(a,a)') cpth(1:lnblnk(cpth)),
     &        cmdn(1:lnblnk(cmdn))
          else
            write(CMDCFG,'(a)') cmdn(1:lnblnk(cmdn))
          endif
          LCMDFL=CMDCFG
        elseif(OUTSTR(1:8).eq.'*sum_cfg')then
          k=8
          CALL EGETRM(OUTSTR,K,CMDCFG,'W','idst summer model cfg',IER)
          if(cpth(1:2).ne.'  ')then
            write(CMDCFG,'(a,a)') cpth(1:lnblnk(cpth)),
     &        cmdn(1:lnblnk(cmdn))
          else
            write(CMDCFG,'(a)') cmdn(1:lnblnk(cmdn))
          endif
          LCMDFL=CMDCFG
        elseif(OUTSTR(1:13).eq.'*name_climate')then
          k=14
          CALL EGETRM(OUTSTR,K,CMDCLMN,'W','idst climate alias',IER)
        elseif(OUTSTR(1:8).eq.'*climate')then
          k=9
          CALL EGETRM(OUTSTR,K,CMDCLM,'W','idst climate file',IER)
        elseif(OUTSTR(1:16).eq.'*name_ext_window')then
          k=17
          CALL EGETRM(OUTSTR,K,CMDLNAM(nidst),'W',
     &      'idst ext window long name',IER)
        elseif(OUTSTR(1:11).eq.'*ext_window')then

C Work with temporary string and strip of .prn or .wis at end of the name. 
          nidst=nidst+1
          k=12
          CALL EGETRM(OUTSTR,K,word,'W','idst anchor loc',IER)
          lw=lnblnk(word)
          if(word(lw-3:lw).eq.'.prn')then
            write(ALOC(nidst),'(a)') word(1:lw-4)
          elseif(word(lw-3:lw).eq.'.wis')then
            write(ALOC(nidst),'(a)') word(1:lw-4)
          else
            write(ALOC(nidst),'(a)') word(1:lnblnk(word))
          endif
          CMDLBL(nidst)='ext_window'

C Debug.
C          write(6,*) word
C          write(6,*) nidst,ALOC(nidst)

        elseif(OUTSTR(1:16).eq.'*name_int_window')then
          k=17
          CALL EGETRM(OUTSTR,K,CMDLNAM(nidst),'W',
     &      'idst int window long name',IER)
        elseif(OUTSTR(1:11).eq.'*int_window')then

C Work with temporary string and strip of .prn or .wis at end of the name. 
          nidst=nidst+1
          k=12
          CALL EGETRM(OUTSTR,K,word,'W','ist anchor loc',IER)
          lw=lnblnk(word)
          if(word(lw-3:lw).eq.'.prn')then
            write(ALOC(nidst),'(a)') word(1:lw-4)
          elseif(word(lw-3:lw).eq.'.wis')then
            write(ALOC(nidst),'(a)') word(1:lw-4)
          else
            write(ALOC(nidst),'(a)') word(1:lnblnk(word))
          endif
          CMDLBL(nidst)='int_window'

C Debug.
C          write(6,*) word
C          write(6,*) nidst,ALOC(nidst)

        elseif(OUTSTR(1:16).eq.'*name_atr_window')then
          k=17
          CALL EGETRM(OUTSTR,K,CMDLNAM(nidst),'W',
     &      'idst atr window long name',IER)
        elseif(OUTSTR(1:11).eq.'*atr_window')then

C Work with temporary string and strip of .prn or .wis at end of the name. 
          nidst=nidst+1
          k=12
          CALL EGETRM(OUTSTR,K,word,'W','anchor loc',IER)
          lw=lnblnk(word)
          if(word(lw-3:lw).eq.'.prn')then
            write(ALOC(nidst),'(a)') word(1:lw-4)
          elseif(word(lw-3:lw).eq.'.wis')then
            write(ALOC(nidst),'(a)') word(1:lw-4)
          else
            write(ALOC(nidst),'(a)') word(1:lnblnk(word))
          endif
          CMDLBL(nidst)='atr_window'

C Debug.
C          write(6,*) word
C          write(6,*) nidst,ALOC(nidst)

        elseif(OUTSTR(1:4).eq.'*res')then
          k=4
          CALL EGETRM(OUTSTR,K,CMDRES,'W','idst report',IER)
        elseif(OUTSTR(1:18).eq.'*update_ctl_period')then

C Update control timing (for use with BEMS software). Data is:
C bld control loop index, period number, (new) start time.
          K=19
          CALL EGETWI(OUTSTR,K,ibcli,1,ncf,'W',
     &      'idst ctl loop to adjust index',IER)
          CALL EGETWI(OUTSTR,K,ibcldt,1,nbcdt(ibcli),
     &      'W','idst ctl to adjust day type',IER)
          CALL EGETWI(OUTSTR,K,ibclp,1,
     &       nbcdp(ibcli,ibcldt),'-','idst ctl period',IER)
          CALL EGETWR(OUTSTR,K,bclst,0.,24.,'W','new time',IER)
          write(outs,'(a)') 'Updating control as follows:'
          call edisp(iuout,outs)
          write(outs,'(a,i2,a,i2,a,i2)') ' Loop ',ibcli,
     &      ' day type ',ibcldt,' period ',ibclp
          call edisp(iuout,outs)
          write(outs,'(a)') 'New start time is:',bclst
          call edisp(iuout,outs)
          tbcps(ibcli,ibcldt,ibclp)=bclst
        endif
        ICTLF=IFIL+1
        CALL CTLWRT(ICTLF,IER)
        goto 222
      endif
      
 223  CALL ERPFREE(IFCFG,ISTAT)
      if(NALOC.gt.0.and.nidst.gt.0)then

C Update the IPV file for changes in periods etc.
        if(icfgv.lt.4)then
          ipvaction='ipv'
          call mkipvdat(ifcfg,lipvdatf,ipvaction)
          CALL EMKCFG('-',IER)
          call procgdst(itrc,runmode,keepres,donegdst,ier)
        else
          CALL EMKCFG('-',IER)
          call procgdst(itrc,runmode,keepres,donegdst,ier)
        endif
      endif

      return
      end

C ************* IPVDAT
C Define information used in generating IPV.
C If act = 'i' then initialise variables.
      subroutine ipvdat(act)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "espriou.h"  
C espriou.h for climatelist file name.
#include "seasons.h"
C seasons.h provides typper and typsea
#include "ipvdata.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C IPV description.
      common/IPVF/lipvdatf

C Simulation parameter sets.
C      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

C ITM is main IPV menu.
C ITMGET is menu for selecting metric types.
C ITME is menu for energy deman sets.
C ITMM is menu for metric sets.
      DIMENSION ITMGET(15),ITM(31),ITMM(13),ITME(14),IVALS(MCOM)
      character ITM*48,ITMM*41,ITME*33,ITMGET*33,hold*40,hold5*50
      character act*1
      character lipvdatf*72,outs*124
      CHARACTER DS*7,DS1*10
      character*72 LTMP,LCFGF
      character cfgroot*24,t40*40,t72*72,t248*248
      character descr*7,descrst*10,descrfn*10
      character descra*7,descrb*7,descrc*7,descrd*7,descre*7
      character t12*12,key*1
      character simact*6,ipvaction*3
      logical MODSIT,XST,OK
      integer nitms,INO,nitmms,INOM,nitmget,INOG,nitmes,INOE ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ipvdat'  ! set for subroutine

C If act = 'i' then initialise variables. Initial assumption
C is for an annual assessment.
      t40=' '
      t72=' '
      lr=lnblnk(cfgroot)
      simact='------'  ! to signal initial state of choice

C Rescan the `climatelist` file. Check if this climate is
C in the list. If not instanciate season and typical start and end dates.
      if(ihaveseason.gt.0)then
        continue
      else
        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then
          IUF=IFIL+2
          call scancdblist(IUF,LCLIM,ok,ier)
          if(ok)then
            continue
          else

C Set default early winter, spring, summer, autumn, late winter periods.
            CALL EDAY(9,1,ia1wins)
            CALL EDAY(15,1,ia1winf)
            CALL EDAY(6,3,ia1sprs)
            CALL EDAY(12,3,ia1sprf)
            CALL EDAY(11,7,iasums)
            CALL EDAY(17,7,iasumf)
            CALL EDAY(2,10,ia2sprs)
            CALL EDAY(8,10,ia2sprf)
            CALL EDAY(20,11,ia2wins)
            CALL EDAY(26,11,ia2winf)

C Default season definitions.
            CALL EDAY(1,1,is1wins)
            CALL EDAY(28,2,is1winf)
            CALL EDAY(1,11,is2wins)
            CALL EDAY(31,12,is2winf)
            CALL EDAY(1,3,is1sprs)
            CALL EDAY(30,4,is1sprf)
            CALL EDAY(1,9,is2sprs)
            CALL EDAY(31,10,is2sprf)
            CALL EDAY(1,5,is1sums)
            CALL EDAY(31,8,is1sumf)
          endif
        endif
      endif

C If creating and IPV from scratch clear, select season and
C initialise the IPV common blocks.
      if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
        call clearipvdat(act)
        call ipvseasons(simact)
        call ipvdatinit(simact)
        ipvsimu=simact   ! remember which one was selected
        MODSIT=.true.
      else
        MODSIT=.false.
      endif

C Scan any demands file.
      IUO=IFIL+1
      XST=.FALSE.
      call FINDFIL(bdmds,XST)
      IF(XST)THEN
        CALL ERPFREE(IUO,ISTAT)
        CALL ERBDMD(0,ITRU,IUO,IER)
        CALL ERPFREE(IUO,ISTAT)
      ENDIF

    3 INO=-4
      IIER=0

      WRITE(ITM(1),'(2A)')     'a title   : ',ipvtitl(1:32)
      WRITE(ITM(2),'(2A)')     'b version : ',ipvvers(1:32)
      WRITE(ITM(3),'(2A)')     'c synopsis: ',ipvsynop(1:32)
      WRITE(ITM(4),'(A,I3)')   'd images  : ',nipvimg
      if(ipvform.eq.0)then
        WRITE(ITM(5),'(a)')    'e report format >> unspecified'
      elseif(ipvform.eq.1)then
        WRITE(ITM(5),'(a)')    'e report format >> readable'
      elseif(ipvform.eq.2)then
        WRITE(ITM(5),'(a)')    'e report format >> tab separated'
      elseif(ipvform.eq.3)then
        WRITE(ITM(5),'(a)')    'e report format >> java (i2pv)'
      endif
      WRITE(ITM(6),'(a,i2,a)') '1 performance metrics (',nms,') '
      WRITE(ITM(7),'(a,i2,a)') '2 demand sets (',neds,') '
      WRITE(ITM(8),'(a,i2,a)') '3___simulations (',nipvassmt,
     &  ')____________days descrip___'
      
      if(nipvassmt.eq.0)then
        m=9
        WRITE(ITM(9),'(a)')        'j  simulation not yet defined '
      elseif(nipvassmt.eq.1)then
        m=9
        call stdate(iyear,ipvastjd(1),descr,descrst)
        call stdate(iyear,ipvafnjd(1),descr,descrfn)
        jjd1=(ipvafnjd(1)-ipvastjd(1))+1
        if(simact(1:6).eq.'------')then
          WRITE(ITM(9),'(4a,i4,2a)') 'j  annual ',descrst,' ',descrfn,
     &    jjd1,' ',ipvadesc(1)(1:10)
        elseif(simact(1:5).eq.'icwin')then
          WRITE(ITM(9),'(4a,i4,2a)') 'j  winter ',descrst,' ',descrfn,
     &    jjd1,' ',ipvadesc(1)(1:10)
        elseif(simact(1:5).eq.'icspr')then
          WRITE(ITM(9),'(4a,i4,2a)') 'j  spring ',descrst,' ',descrfn,
     &    jjd1,' ',ipvadesc(1)(1:10)
        elseif(simact(1:5).eq.'icsum')then
          WRITE(ITM(9),'(4a,i4,2a)') 'j  summer ',descrst,' ',descrfn,
     &    jjd1,' ',ipvadesc(1)(1:10)
        elseif(simact(1:5).eq.'icaut')then
          WRITE(ITM(9),'(4a,i4,2a)') 'j  autumn ',descrst,' ',descrfn,
     &    jjd1,' ',ipvadesc(1)(1:10)
        elseif(simact(1:3).eq.'ias')then
          WRITE(ITM(9),'(4a,i4,2a)') 'j  annual ',descrst,' ',descrfn,
     &    jjd1,' ',ipvadesc(1)(1:10)
        elseif(simact(1:3).eq.'i1d')then
          WRITE(ITM(9),'(4a,i4,2a)') 'j  single ',descrst,' ',descrfn,
     &    jjd1,' ',ipvadesc(1)(1:10)
        endif
      elseif(nipvassmt.eq.3)then
        m=11
        call stdate(iyear,ipvastjd(1),descr,descrst)
        call stdate(iyear,ipvafnjd(1),descr,descrfn)
        jjd1=(ipvafnjd(1)-ipvastjd(1))+1
        WRITE(ITM(9),'(4a,i4,2a)') 'j  winter ',descrst,' ',descrfn,
     &    jjd1,' ',ipvadesc(1)(1:10)
        call stdate(iyear,ipvastjd(2),descr,descrst)
        call stdate(iyear,ipvafnjd(2),descr,descrfn)
        jjd2=(ipvafnjd(2)-ipvastjd(2))+1
        WRITE(ITM(10),'(4a,i4,2a)') 'k  trans  ',descrst,' ',descrfn,
     &    jjd2,' ',ipvadesc(2)(1:10)
        call stdate(iyear,ipvastjd(3),descr,descrst)
        call stdate(iyear,ipvafnjd(3),descr,descrfn)
        jjd3=(ipvafnjd(3)-ipvastjd(3))+1
        WRITE(ITM(11),'(4a,i4,2a)')  'l  summer ',descrst,' ',descrfn,
     &    jjd3,' ',ipvadesc(3)(1:10)
      elseif(nipvassmt.eq.5)then
        m=13
        call stdate(iyear,ipvastjd(1),descr,descrst)
        call stdate(iyear,ipvafnjd(1),descr,descrfn)
        jjd1=(ipvafnjd(1)-ipvastjd(1))+1
        WRITE(ITM(9),'(4a,i4,2a)')  'j  1st win ',descrst,' ',descrfn,
     &    jjd1,' ',ipvadesc(1)(1:10)
        call stdate(iyear,ipvastjd(2),descr,descrst)
        call stdate(iyear,ipvafnjd(2),descr,descrfn)
        jjd2=(ipvafnjd(2)-ipvastjd(2))+1
        WRITE(ITM(10),'(4a,i4,2a)')  'k  spring  ',descrst,' ',descrfn,
     &    jjd2,' ',ipvadesc(2)(1:10)
        call stdate(iyear,ipvastjd(3),descr,descrst)
        call stdate(iyear,ipvafnjd(3),descr,descrfn)
        jjd3=(ipvafnjd(3)-ipvastjd(3))+1
        WRITE(ITM(11),'(4a,i4,2a)')  'l  summer  ',descrst,' ',descrfn,
     &    jjd3,' ',ipvadesc(3)(1:10)
        call stdate(iyear,ipvastjd(4),descr,descrst)
        call stdate(iyear,ipvafnjd(4),descr,descrfn)
        jjd4=(ipvafnjd(4)-ipvastjd(4))+1
        WRITE(ITM(12),'(4a,i4,2a)')  'm  autumn  ',descrst,' ',descrfn,
     &    jjd4,' ',ipvadesc(4)(1:10)
        call stdate(iyear,ipvastjd(5),descr,descrst)
        call stdate(iyear,ipvafnjd(5),descr,descrfn)
        jjd5=(ipvafnjd(5)-ipvastjd(5))+1
        WRITE(ITM(13),'(4a,i4,2a)')  'n  2nd win ',descrst,' ',descrfn,
     &    jjd5,' ',ipvadesc(5)(1:10)
      endif
      WRITE(ITM(m+1),'(a,i2,a)') '4  display days (',nipvdispjd,') '
      ITM(m+2)=      ' ___seasons (from climate list)________________ '
      ITM(m+3)=      '         winter  spring  summer  autumn  winter '
      call stdate(iyear,is1wins,descra,descrst)
      call stdate(iyear,is1sprs,descrb,descrst)
      call stdate(iyear,is1sums,descrc,descrst)
      call stdate(iyear,is2sprs,descrd,descrst)
      call stdate(iyear,is2wins,descre,descrst)
      WRITE(ITM(m+4),'(10a)') '  start  ',descra,' ',descrb,' ',descrc,
     &  ' ',descrd,' ',descre
      call stdate(iyear,is1winf,descra,descrfn)
      call stdate(iyear,is1sprf,descrb,descrfn)
      call stdate(iyear,is1sumf,descrc,descrfn)
      call stdate(iyear,is2sprf,descrd,descrfn)
      call stdate(iyear,is2winf,descre,descrfn)
      WRITE(ITM(m+5),'(10a)') '  finish ',descra,' ',descrb,' ',descrc,
     &  ' ',descrd,' ',descre
      jd1=(is1winf-is1wins)+1
      jd2=(is1sprf-is1sprs)+1
      jd3=(is1sumf-is1sums)+1
      jd4=(is2sprf-is2sprs)+1
      jd5=(is2winf-is2wins)+1
      WRITE(ITM(m+6),'(a,i4,1x,4i8)')'  days   ',jd1,jd2,jd3,jd4,jd5

      if(nipvassmt.eq.0.or.nipvassmt.eq.1)then
        ITM(m+7)=                    ' ratios for all seasons...     '
        WRITE(ITM(m+8), '(a,f7.2)')  'q  heating    ',ddmheat(1)
        WRITE(ITM(m+9), '(a,f7.2)')  'r  cooling    ',ddmcool(1)
        WRITE(ITM(m+10),'(a,f7.2)')  's  lighting   ',ddmlight(1)
        WRITE(ITM(m+11),'(a,f7.2)')  't  small power',ddmsmlpw(1)
        WRITE(ITM(m+12),'(a,f7.2)')  'u  fans&pumps ',ddmfan(1)
        WRITE(ITM(m+13),'(a,f7.2)')  'v  DHW        ',ddmdhw(1)
      elseif(nipvassmt.eq.3)then
        ITM(m+7)=      ' ratios for winter transition summer         '
        WRITE(ITM(m+8),'(a,3f7.2)')  'q  heating    ',ddmheat(1),
     &    ddmheat(2),ddmheat(3)
        WRITE(ITM(m+9),'(a,3f7.2)')  'r  cooling    ',ddmcool(1),
     &    ddmcool(2),ddmcool(3)
        WRITE(ITM(m+10),'(a,3f7.2)') 's  lighting   ',ddmlight(1),
     &    ddmlight(2),ddmlight(3)
        WRITE(ITM(m+11),'(a,3f7.2)') 't  small power',ddmsmlpw(1),
     &    ddmsmlpw(2),ddmsmlpw(3)
        WRITE(ITM(m+12),'(a,3f7.2)') 'u  fans&pumps ',ddmfan(1),
     &    ddmfan(2),ddmfan(3)
        WRITE(ITM(m+13),'(a,3f7.2)') 'v  DHW        ',ddmdhw(1),
     &    ddmdhw(2),ddmdhw(3)
      elseif(nipvassmt.eq.5)then
        ITM(m+7)=    ' ratios for:  winter spring summer autumn winter'
        WRITE(ITM(m+8),'(a,5f7.2)')  'q heating    ',ddmheat(1),
     &    ddmheat(2),ddmheat(3),ddmheat(4),ddmheat(5)
        WRITE(ITM(m+9),'(a,5f7.2)')  'r cooling    ',ddmcool(1),
     &    ddmcool(2),ddmcool(3),ddmcool(4),ddmcool(5)
        WRITE(ITM(m+10),'(a,5f7.2)') 's lighting   ',ddmlight(1),
     &    ddmlight(2),ddmlight(3),ddmlight(4),ddmlight(5)
        WRITE(ITM(m+11),'(a,5f7.2)') 't small power',ddmsmlpw(1),
     &    ddmsmlpw(2),ddmsmlpw(3),ddmsmlpw(4),ddmsmlpw(5)
        WRITE(ITM(m+12),'(a,5f7.2)') 'u fans&pumps ',ddmfan(1),
     &    ddmfan(2),ddmfan(3),ddmfan(4),ddmfan(5)
        WRITE(ITM(m+13),'(a,5f7.2)') 'v DHW        ',ddmdhw(1),
     &    ddmdhw(2),ddmdhw(3),ddmdhw(4),ddmdhw(5)
      endif
      ITM(m+14)=   '5 re-scan climate for seasons or day ratios '
      ITM(m+15)=   '! list IPV data                             '
      ITM(m+16)=   '? help                                      '
      ITM(m+17)=   '- exit this menu                            '
      nitms=m+17

C Help text for this menu.
  4   helptopic='ipv_menu_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      if(mmod.eq.8)then
        CALL EMENU('Integrated Performance View data',ITM,nitms,INO)
      else
        CALL EMENU('IPV description',ITM,nitms,INO)
      endif

      if(INO.EQ.nitms)then

C If data updated, write to IPV definition file and update link in cfg.
C Also update or create matched simulation parameter sets.
        if(MODSIT)then
          if(icfgv.lt.4)then
            lr=lnblnk(cfgroot)
            if(lnblnk(lipvdatf).eq.0)then
              write(ltmp,'(2a)') cfgroot(1:lr),'.ipv'
            elseif(lipvdatf(1:7).eq.'UNKNOWN')then
              write(ltmp,'(2a)') cfgroot(1:lr),'.ipv'
            else
              ltmp=lipvdatf
            endif
            CALL EASKS(ltmp,'IPV description file ?',
     &        ' ',72,'xxx.ipv','IPV file',IER,nbhelp)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN') lipvdatf=ltmp

C Write out the current IPV definitions.
	    ipvaction='ipv'
            call mkipvdat(ifil+1,lipvdatf,ipvaction)

            if(nipvassmt.eq.1.or.nipvassmt.eq.3.or.nipvassmt.eq.5)then

C If one of the standard number of IPV assessments transfer the relevant
C data into the simulation parameter set data structure.
              call ipv2simpar(ipvsimu)
            endif
            CALL EMKCFG('-',IER)
          else

C IPV data is held in the configuration file. Transfer data to the
C simulation parameter data structure if appropriate and upd the
C model confituration file.
            if(nipvassmt.eq.1.or.nipvassmt.eq.3.or.nipvassmt.eq.5)then
              call ipv2simpar(ipvsimu)
            endif
            CALL EMKCFG('-',IER)
          endif
        endif
        return
      elseif(INO.EQ.nitms-1)then
        helptopic='ipv_menu_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('IPV setup',nbhelp,'-',0,0,IER)
      elseif(INO.EQ.1)then
        t40=ipvtitl
        CALL EASKS(t40,'IPV tile?',' ',40,'TITLE','IPV Tile',
     &    IER,nbhelp)
        if(t40(1:2).ne.'  ')ipvtitl=t40
        MODSIT=.true.
      elseif(INO.EQ.2)then
        t40=ipvvers
        CALL EASKS(t40,'version ?',' ',40,'base','version',
     &    IER,nbhelp)
        if(t40(1:2).ne.'  ')ipvvers=t40
        MODSIT=.true.
      elseif(INO.EQ.3)then
        call edisp(iuout,'Current synopsis...')
        t248=ipvsynop
        CALL EASKS248(t248,'Synopsis',' ',72,
     &    'No sysnopsis provided for this model.','synopsis',
     &    IER,nbhelp)
        if(t248(1:2).ne.'  ')ipvsynop=t248
        MODSIT=.true.
      elseif(INO.EQ.4)then

C Images associated with the IPV.
        write(outs,'(a,i2,a)') 'There are currently ',nipvimg,
     &    ' images associated with this IPV.'
        CALL EASKABCD(outs,'Choices: ','list',
     &      'edit','add','cancel',IW,nbhelp)
        if(IW.eq.1)then
          if(nipvimg.gt.0)then
            do 52 i=1,nipvimg
              call edisp(iuout,lipvimg(i))
   52       continue
          endif
        elseif(IW.eq.2)then
          CALL EASKI(IFOC,'Which image (give index) ? ',' ',
     &        1,'F',nipvimg,'F',1,'ipv image',IERI,nbhelp)
          if(ieri.eq.-3) then
            INO=-1
            goto 3
          endif
          t72=lipvimg(IFOC)
          CALL EASKS(t72,'Image file name:',' ',72,'xxx','image file',
     &      IER,nbhelp)
          if(t72(1:2).ne.'  ')lipvimg(IFOC)=t72
        elseif(IW.eq.3)then
          nipvimg=nipvimg+1
          if(nipvimg.le.4)then
            lipvimg(nipvimg)=' '
            t72=lipvimg(nipvimg)
            CALL EASKS(t72,'Image file name:',' ',72,'xx','image file',
     &        IER,nbhelp)
            if(t72(1:2).ne.'  ')lipvimg(nipvimg)=t72
          else
            nipvimg=4
          endif
        elseif(IW.eq.4)then
          INO=-1
          goto 3
        endif
      elseif(INO.EQ.5)then

C Toggle between reporting formats.
        ipvform=ipvform+1
        if(ipvform.gt.3) ipvform = 0
        goto 3
      elseif(INO.EQ.6)then

C Deal with performance metrics.
        goto 42
      elseif(INO.EQ.7)then

C Demand sets - list current sets and then display as a menu.
        call listipvdat(iuout,'d',ier)
        goto 142
      elseif(INO.EQ.8)then

C Toggle the number of assessments. Also set initial display days
C so they fall within the assessments being caried out.

C Ask for which season/assessment period(s).
        call ipvseasons(simact)
        call ipvdatinit(simact)
        ipvsimu=simact   ! remember which one was selected

        if(nipvassmt.eq.1)then
          m=9
          MODSIT=.true.
        elseif(nipvassmt.eq.3)then
          m=11
          MODSIT=.true.
        elseif(nipvassmt.eq.5)then
          m=13
          MODSIT=.true.
        endif

C Debug.
C        write(6,*)ipvastjd
C        write(6,*)ipvafnjd
C        write(6,*)ipvdispjd
C        write(6,*) ok

      elseif(INO.EQ.9.and.
     &      (nipvassmt.eq.1.or.nipvassmt.eq.3.or.nipvassmt.eq.5))then

C Present pop-up of the current period(s) and then allow editing
C in terms of day and month which is then converted to a julian
C day for storage.
   91   hold = ' '
        CALL STDATE(IYEAR,ipvastjd(1),DS,DS1)
        write(outs,'(a,i4,2x,a)') ' Start : ',ipvastjd(1),DS1
        call edisp(iuout,outs)
        CALL STDATE(IYEAR,ipvafnjd(1),DS,DS1)
        write(outs,'(a,i4,2x,a)') ' Finish: ',ipvafnjd(1),DS1
        call edisp(iuout,outs)
        call edayr(ipvastjd(1),id1,im1)
        call edayr(ipvafnjd(1),id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        if(nipvassmt.eq.1)then
          CALL EASKS(HOLD,'Simulation period: start (day & month) and',
     &      'finish (day & month):',40,' 9 1 15 1','single sim period',
     &      IIER,nbhelp)
        elseif(nipvassmt.eq.3)then
          CALL EASKS(HOLD,'Winter period: start (day & month) and',
     &      'finish (day & month):',40,' 9 1 15 1','winter sim period',
     &      IIER,nbhelp)
        elseif(nipvassmt.eq.5)then
          CALL EASKS(HOLD,'1st winter period: start (day & month) and',
     &      'finish (day & month):',40,' 9 1 15 1','winter sim period',
     &      IIER,nbhelp)
        endif
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','sim st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','sim st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','sim fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','sim fn month',IIER)
        CALL EDAY(id1,im1,ipvastjd(1))
        CALL EDAY(id2,im2,ipvafnjd(1))
        if(iier.ne.0)goto 91
        MODSIT=.true.
      elseif(INO.EQ.10.and.(nipvassmt.eq.3.or.nipvassmt.eq.5))then
   92   hold = ' '
        CALL STDATE(IYEAR,ipvastjd(2),DS,DS1)
        write(outs,'(a,i4,2x,a)') ' Start : ',ipvastjd(2),DS1
        call edisp(iuout,outs)
        CALL STDATE(IYEAR,ipvafnjd(2),DS,DS1)
        write(outs,'(a,i4,2x,a)') ' Finish: ',ipvafnjd(2),DS1
        call edisp(iuout,outs)
        call edayr(ipvastjd(2),id1,im1)
        call edayr(ipvafnjd(2),id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        if(nipvassmt.eq.3)then
          CALL EASKS(HOLD,'Transition period: start (day & month) and',
     &      'finish (day & month):',40,' 9 1 15 1','trns sim period',
     &      IIER,nbhelp)
        elseif(nipvassmt.eq.5)then
          CALL EASKS(HOLD,'Spring period: start (day & month) and',
     &      'finish (day & month):',40,' 9 1 15 1','spring sim period',
     &      IIER,nbhelp)
        endif
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','sim st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','sim st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','sim fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','sim fn month',IIER)
        CALL EDAY(id1,im1,ipvastjd(2))
        CALL EDAY(id2,im2,ipvafnjd(2))
        if(iier.ne.0)goto 92
        MODSIT=.true.
      elseif(INO.EQ.11.and.(nipvassmt.eq.3.or.nipvassmt.eq.5))then
   93   hold = ' '
        CALL STDATE(IYEAR,ipvastjd(3),DS,DS1)
        write(outs,'(a,i4,2x,a)') ' Start : ',ipvastjd(3),DS1
        call edisp(iuout,outs)
        CALL STDATE(IYEAR,ipvafnjd(3),DS,DS1)
        write(outs,'(a,i4,2x,a)') ' Finish: ',ipvafnjd(3),DS1
        call edisp(iuout,outs)
        call edayr(ipvastjd(3),id1,im1)
        call edayr(ipvafnjd(3),id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Summer period: start (day & month) and',
     &    'finish (day & month):',40,' 9 1 15 1','summer sim period',
     &    IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','sim st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','sim st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','sim fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','sim fn month',IIER)
        CALL EDAY(id1,im1,ipvastjd(3))
        CALL EDAY(id2,im2,ipvafnjd(3))
        if(iier.ne.0)goto 93
        MODSIT=.true.
      elseif(INO.EQ.12.and.nipvassmt.eq.5)then
   94   hold = ' '
        CALL STDATE(IYEAR,ipvastjd(4),DS,DS1)
        write(outs,'(a,i4,2x,a)') ' Autumn start : ',ipvastjd(4),DS1
        call edisp(iuout,outs)
        CALL STDATE(IYEAR,ipvafnjd(4),DS,DS1)
        write(outs,'(a,i4,2x,a)') ' Autumn finish: ',ipvafnjd(4),DS1
        call edisp(iuout,outs)
        call edayr(ipvastjd(4),id1,im1)
        call edayr(ipvafnjd(4),id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Autumn period: start (day & month) and',
     &    'finish (day & month):',40,' 9 1 15 1','autm sim period',
     &    IIER,4)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','sim st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','sim st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','sim fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','sim fn month',IIER)
        CALL EDAY(id1,im1,ipvastjd(4))
        CALL EDAY(id2,im2,ipvafnjd(4))
        if(iier.ne.0)goto 94
        MODSIT=.true.
      elseif(INO.EQ.13.and.nipvassmt.eq.5)then
   95   hold = ' '
        CALL STDATE(IYEAR,ipvastjd(5),DS,DS1)
        write(outs,'(a,i4,2x,a)')' 2nd winter start : ',ipvastjd(4),DS1
        call edisp(iuout,outs)
        CALL STDATE(IYEAR,ipvafnjd(5),DS,DS1)
        write(outs,'(a,i4,2x,a)')' 2nd winter finish: ',ipvafnjd(4),DS1
        call edisp(iuout,outs)
        call edayr(ipvastjd(5),id1,im1)
        call edayr(ipvafnjd(5),id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'2nd winter period: start (day & month) and',
     &    'finish (day & month):',40,' 9 1 15 1','2nd win sim period',
     &    IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','sim st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','sim st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','sim fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','sim fn month',IIER)
        CALL EDAY(id1,im1,ipvastjd(5))
        CALL EDAY(id2,im2,ipvafnjd(5))
        if(iier.ne.0)goto 95
        MODSIT=.true.
      elseif(INO.EQ.m+1)then

C Display days.
C Initial values should be defined when the number of assessments
C and the assessment periods were setup (e.g. the 2nd day of each
C assessment.
        if(nipvdispjd.gt.0)then
          do 195 ij=1,nipvdispjd
  194       hold = ' '
            CALL STDATE(IYEAR,ipvdispjd(ij),DS,DS1)
            write(outs,'(a,i4,2x,a)') ' Display day: ',ipvdispjd(ij),DS1
            call edisp(iuout,outs)
            call edayr(ipvdispjd(ij),id1,im1)
            WRITE(HOLD,'(2I4)')id1,im1
            CALL EASKS(HOLD,'Display day & month: (should be',
     &        '(within a simulation period):',40,' 9 1','disp day',
     &        IIER,nbhelp)
            K=0
            CALL EGETWI(HOLD,K,id1,1,31,'W','sim st day',IIER)
            CALL EGETWI(HOLD,K,im1,1,12,'W','sim st month',IIER)
            CALL EDAY(id1,im1,ipvdispjd(ij))
            if(iier.ne.0)goto 194
            MODSIT=.true.
  195     continue
        endif
        CALL EASKAB('Options: ',' ','add another day','continue',
     &    IW,nbhelp)
        if(IW.eq.1)then
          nipvdispjd=nipvdispjd+1
          ij=nipvdispjd
  196     HOLD='  12   12 '
          CALL EASKS(HOLD,'Display day & month: (should be',
     &      '(within a simulation period):',40,' 9 1','disp day',
     &      IIER,nbhelp)
          K=0
          CALL EGETWI(HOLD,K,id1,1,31,'W','display day',IIER)
          CALL EGETWI(HOLD,K,im1,1,12,'W','display month',IIER)
          CALL EDAY(id1,im1,ipvdispjd(ij))
          if(iier.ne.0)goto 196
          CALL STDATE(IYEAR,ipvdispjd(ij),DS,DS1)
          write(outs,'(a,i4,2x,a)') ' New day: ',ipvdispjd(ij),DS1
          call edisp(iuout,outs)
          MODSIT=.true.
        endif
      elseif(INO.EQ.m+8)then

C Heating degree day multiplier or day ratio.
   96   hold5 = ' '
        WRITE(HOLD5,'(5F7.3)') ddmheat(1),ddmheat(2),ddmheat(3),
     &    ddmheat(4),ddmheat(5)
        helptopic='ipv_season_multip'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(HOLD5,'Seasonal multiplier for heating:',
     &    ' ',50,' 17. 17. 17. 17.  17.','heating dd',IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD5,K,ddmheat(1),1.,999.,'W','win ht',IIER)
        CALL EGETWR(HOLD5,K,ddmheat(2),1.,999.,'W','spr ht',IIER)
        CALL EGETWR(HOLD5,K,ddmheat(3),1.,999.,'W','sum ht',IIER)
        CALL EGETWR(HOLD5,K,ddmheat(4),1.,999.,'W','aut ht',IIER)
        CALL EGETWR(HOLD5,K,ddmheat(5),1.,999.,'W','win ht',IIER)
        if(iier.ne.0)goto 96
        MODSIT=.true.
      elseif(INO.EQ.m+9)then

C Cooling degree day multiplier or day ratio.
   97   hold5 = ' '
        WRITE(HOLD5,'(5F7.3)') ddmcool(1),ddmcool(2),ddmcool(3),
     &    ddmcool(4),ddmcool(5)
        helptopic='ipv_season_multip'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(HOLD5,'Seasonal multiplier for cooling:',
     &    ' ',50,' 17. 17. 17. 17.  17.','cooling dd',IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD5,K,ddmcool(1),1.,999.,'W','win cl',IIER)
        CALL EGETWR(HOLD5,K,ddmcool(2),1.,999.,'W','spr cl',IIER)
        CALL EGETWR(HOLD5,K,ddmcool(3),1.,999.,'W','sum cl',IIER)
        CALL EGETWR(HOLD5,K,ddmcool(4),1.,999.,'W','aut cl',IIER)
        CALL EGETWR(HOLD5,K,ddmcool(5),1.,999.,'W','win cl',IIER)
        if(iier.ne.0)goto 97
        MODSIT=.true.
      elseif(INO.EQ.m+10)then

C Lighting day ratio.
   98   hold5 = ' '
        WRITE(HOLD5,'(5F7.3)') ddmlight(1),ddmlight(2),ddmlight(3),
     &    ddmlight(4),ddmlight(5)
        helptopic='ipv_season_multip'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(HOLD5,'Seasonal multiplier for lighting:',
     &    ' ',50,' 17. 17. 17. 17.  17.','lighting dd',IIER,12)
        K=0
        CALL EGETWR(HOLD5,K,ddmlight(1),1.,999.,'W','win lt',IIER)
        CALL EGETWR(HOLD5,K,ddmlight(2),1.,999.,'W','spr lt',IIER)
        CALL EGETWR(HOLD5,K,ddmlight(3),1.,999.,'W','sum lt',IIER)
        CALL EGETWR(HOLD5,K,ddmlight(4),1.,999.,'W','aut lt',IIER)
        CALL EGETWR(HOLD5,K,ddmlight(5),1.,999.,'W','win lt',IIER)
        if(iier.ne.0)goto 98
        MODSIT=.true.
      elseif(INO.EQ.m+11)then

C Small power day ratio.
   99   hold5 = ' '
        WRITE(HOLD5,'(5F7.3)') ddmsmlpw(1),ddmsmlpw(2),ddmsmlpw(3),
     &    ddmsmlpw(4),ddmsmlpw(5)
        helptopic='ipv_season_multip'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(HOLD5,'Seasonal multiplier for small power:',
     &    ' ',50,' 17. 17. 17. 17.  17.','small pwr dd',IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD5,K,ddmsmlpw(1),1.,999.,'W','win spw',IIER)
        CALL EGETWR(HOLD5,K,ddmsmlpw(2),1.,999.,'W','spr spw',IIER)
        CALL EGETWR(HOLD5,K,ddmsmlpw(3),1.,999.,'W','sum spw',IIER)
        CALL EGETWR(HOLD5,K,ddmsmlpw(4),1.,999.,'W','aut spw',IIER)
        CALL EGETWR(HOLD5,K,ddmsmlpw(5),1.,999.,'W','win spw',IIER)
        if(iier.ne.0)goto 99
        MODSIT=.true.
      elseif(INO.EQ.m+12)then

C Fans and pumps day ratio.
  100   hold5 = ' '
        WRITE(HOLD5,'(5F7.3)') ddmfan(1),ddmfan(2),ddmfan(3),
     &    ddmfan(4),ddmfan(5)
        helptopic='ipv_season_multip'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(HOLD5,'Seasonal multiplier for fans & pumps:',
     &    ' ',50,' 17. 17. 17. 17.  17.','fan dd',IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD5,K,ddmfan(1),1.,999.,'W','win fan',IIER)
        CALL EGETWR(HOLD5,K,ddmfan(2),1.,999.,'W','spr fan',IIER)
        CALL EGETWR(HOLD5,K,ddmfan(3),1.,999.,'W','sum fan',IIER)
        CALL EGETWR(HOLD5,K,ddmfan(4),1.,999.,'W','aut fan',IIER)
        CALL EGETWR(HOLD5,K,ddmfan(5),1.,999.,'W','win fan',IIER)
        if(iier.ne.0)goto 100
        MODSIT=.true.
      elseif(INO.EQ.m+13)then

C Domestic hot water day ratio.
  101   hold5 = ' '
        WRITE(HOLD5,'(5F7.3)') ddmdhw(1),ddmdhw(2),ddmdhw(3),
     &    ddmdhw(4),ddmdhw(5)
        helptopic='ipv_season_multip'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(HOLD5,'Seasonal multipliers for domestic hot water:',
     &    ' ',50,' 17. 17. 17. 17.  17.','DHW dd',IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD5,K,ddmdhw(1),1.,999.,'W','win DHW',IIER)
        CALL EGETWR(HOLD5,K,ddmdhw(2),1.,999.,'W','spr DHW',IIER)
        CALL EGETWR(HOLD5,K,ddmdhw(3),1.,999.,'W','sum DHW',IIER)
        CALL EGETWR(HOLD5,K,ddmdhw(4),1.,999.,'W','aut DHW',IIER)
        CALL EGETWR(HOLD5,K,ddmdhw(5),1.,999.,'W','win DHW',IIER)
        if(iier.ne.0)goto 101
        MODSIT=.true.
      elseif(INO.EQ.m+14)then
        helptopic='ipv_season_scan'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC('Options: ',' ',
     &    'scan climatelist for seasons & periods',
     &    'scan for day/DD ratios','continue',IW,nbhelp)
        if(IW.eq.1.or.IW.eq.2)then

C Rescan climate to acquire ratios from the climatelist file.
          INQUIRE (FILE=cdblfil,EXIST=XST)
          if(XST)then
            IUF=IFIL+2
            call scancdblist(IUF,LCLIM,ok,ier)
          endif
        endif
        if(IW.eq.1)then

C Re-scan climate seasons and periods.
          ipvastjd(1)=ia1wins
          ipvafnjd(1)=ia1winf
          ipvastjd(2)=ia1sprs
          ipvafnjd(2)=ia1sprf
          ipvastjd(3)=iasums
          ipvafnjd(3)=iasumf
          ipvastjd(4)=ia2sprs
          ipvafnjd(4)=ia2sprf
          ipvastjd(5)=ia2wins
          ipvafnjd(5)=ia2winf
          MODSIT=.true.
        elseif(IW.eq.2)then

C Local (stripped) version of a subroutine from clmper.F
          call getmultip

C Loop through five (?) seasons and set dd arrays.
          do 105 ij=1,5
            ddmheat(ij)=dmheat(ij)
            ddmcool(ij)=dmcool(ij)
            ddmlight(ij)=dmlight(ij)
            ddmsmlpw(ij)=dmsmlpw(ij)
            ddmfan(ij)=dmfan(ij)
            ddmdhw(ij)=dmdhw(ij)
  105     continue
          MODSIT=.true.
        endif
      elseif(INO.EQ.m+15)then

C List current metrics and demand sets.
        call listipvdat(iuout,'a',ier)
      else
        INO=-4
        GOTO 4
      endif
      INO=-4
      GOTO 3

C Deal with metrics (and then return to appropriate point in main menue).
  42  continue
      M=1
      ITMM(1) =   '___metric______zones_area_scaling_weight'
      if(nms.eq.0)then
        WRITE(ITMM(2),'(3A,i3,F7.1,2F5.1)')'a ',metrglbl(1)(1:17),':',
     &    nzmg(1),emgflr(1),emgsca(1),emgwtg(1)
        M=2
      else
        do 102 L=1,nms
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(ITMM(M),'(4A,i3,F7.1,2F5.1)') key,' ',
     &      metrglbl(L)(1:17),':',nzmg(L),emgflr(L),emgsca(L),emgwtg(L)
 102    continue
      endif
      ITMM(M+1)=   '+ add/delete metric set            '
      ITMM(M+2)=   '! list metrics                     '
      ITMM(M+3)=   '? help                             '
      ITMM(M+4)=   '- exit this menu                   '
      nitmms=M+4

C Help text for this menu.
 43   helptopic='ipv_menu_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('IPV metric sets',ITMM,nitmms,INOM)
      if(INOM.GE.2.and.INOM.le.nitmms-4)then
        ijj=INOM-1
        goto 44
      elseif(INOM.EQ.nitmms-3)then

C Add delete copy metric set.
        CALL EASKABC('Metric set options:',' ','add','delete',
     &    'do nothing',IRT,nbhelp)
        if(IRT.eq.1)then
          if(nms.lt.MIPVM)then
            nms=nms+1
            ijj=nms
            goto 44
          endif
        elseif(IRT.eq.2)then
          CALL EASKI(IFOC,'Which metric set (give index) ? ',
     &      '(zero to skip)',0,'F',nms,'F',1,'demand set index',
     &      IERI,nbhelp)
          if(ieri.eq.-3)then
            continue
          elseif(ifoc.eq.0)then
            continue
          else
            do 111 is=ifoc,nms-1
              imetget(is)=imetget(is+1)
              imetmsc(is,1)=imetmsc(is+1,1)
              imetmsc(is,2)=imetmsc(is+1,2)
              nzmg(is)=nzmg(is+1)
              emgflr(is)=emgflr(is+1)
              emgsca(is)=emgsca(is+1)
              emgwtg(is)=emgwtg(is+1)
              metrglbl(is)=metrglbl(is+1)
              msdoc(is)=msdoc(is+1)
              metgroup(is)=metgroup(is+1)
              do 112 iss=1,MCOM
                izmg(is,iss)=izmg(is+1,iss)
 112          continue
 111        continue
            nms=nms-1
            MODSIT=.true.
          endif
        elseif(IRT.eq.3)then
          continue
        endif
      elseif(INOM.EQ.nitmms-2)then

C List current metrics.
        call listipvdat(iuout,'m',ier)
      elseif(INOM.EQ.nitmms-1)then
        helptopic='ipv_menu_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('IPV metrics',nbhelp,'-',0,0,IER)
      elseif(INOM.EQ.nitmms)then
        INO=-4
        GOTO 3
      else
        INOM=-3
        GOTO 43
      endif
      INOM=-4
      GOTO 42

 44   continue
C In esrures IGET indices are used. Currently comfort for an IPV makes
C use of resultant temperature (IGET=6).
C IGET = 70 IPV demands set (heating/cooling/lights/small_power/fans/DHW/PV)
C IGET = 71 IPV distributed demands (light (unctld)/light (ctld)/ fans
C            /pumps/lifts/small_power/DHW/PV
C IGET = 73 IPV visual comfort (various IGET,4) where 1=Guth, 2=glare,
C           3=daylight factors
C IGET = 74 IPV conv+radiant & latent zone injection
 777  ITMGET(1) ='a zone resultant temperature'
      ITMGET(2) ='b zone dry bulb temperature '
      ITMGET(3) ='c zone relative humidity (%)'
      ITMGET(4) ='d zone infiltration load    '
      ITMGET(5) ='e zone ventilation load     '
      ITMGET(6) ='f zone casual gains (all)   '
      ITMGET(7) ='g zone solar (from outside) '
      ITMGET(8) ='h zone solar (absorbed in)  '
      ITMGET(9) ='i visual comfort (Guth)     '
      ITMGET(10)='j visual comfort (glare)    '
      ITMGET(11)='k daylight factors          '
      ITMGET(12)='l zone cnv + rad & latent   '
      ITMGET(13)='* NO CHOICE AT THIS TIME    '
      ITMGET(14)='? help                      '
      ITMGET(15)='- exit                      '
      nitmget=15

C Help text for this menu.
      helptopic='ipv_metric_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Metric options',ITMGET,nitmget,INOG)
      if(INOG.eq.nitmget)then
        goto 43
      elseif(INOG.eq.nitmget-1)then
        helptopic='ipv_metric_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('metric options',nbhelp,'-',0,0,IER)
        goto 777
      elseif(INOG.eq.nitmget-2)then
        goto 43
      elseif(INOG.eq.1)then
        imetget(ijj)=6
        imetmsc(ijj,1)=6
        imetmsc(ijj,2)=1
        msdoc(ijj)='comfort'
        metgroup(ijj)='ocup_zones'
        metrglbl(ijj)='Resultant T (degC)'
      elseif(INOG.eq.2)then
        imetget(ijj)=1
        imetmsc(ijj,1)=1
        imetmsc(ijj,2)=0
        metgroup(ijj)='dbt_zones'
        msdoc(ijj)='ZndbT'
        metrglbl(ijj)='Zone db T (degC)'
      elseif(INOG.eq.3)then
        imetget(ijj)=13
        imetmsc(ijj,1)=13
        imetmsc(ijj,2)=0
        msdoc(ijj)='ZnRH'
        metgroup(ijj)='rh_zones'
        metrglbl(ijj)='Zone rel humid (%)'
      elseif(INOG.eq.4)then
        imetget(ijj)=11
        imetmsc(ijj,1)=11
        imetmsc(ijj,2)=0
        msdoc(ijj)='Infil'
        metgroup(ijj)='infil_zones'
        metrglbl(ijj)='Infiltration (W)'
      elseif(INOG.eq.5)then
        imetget(ijj)=12
        imetmsc(ijj,1)=12
        imetmsc(ijj,2)=0
        msdoc(ijj)='Vent'
        metgroup(ijj)='vent_zones'
        metrglbl(ijj)='Ventilation (W)'
      elseif(INOG.eq.6)then
        imetget(ijj)=15
        imetmsc(ijj,1)=1
        imetmsc(ijj,2)=0
        msdoc(ijj)='TotCasG'
        metgroup(ijj)='cas_zones'
        metrglbl(ijj)='Total casual gn (W)'
      elseif(INOG.eq.7)then
        imetget(ijj)=38
        imetmsc(ijj,1)=1
        imetmsc(ijj,2)=0
        msdoc(ijj)='SolinEx'
        metgroup(ijj)='sol_zones'
        metrglbl(ijj)='Solar via outside(W)'
      elseif(INOG.eq.8)then
        imetget(ijj)=40
        imetmsc(ijj,1)=1
        imetmsc(ijj,2)=0
        metgroup(ijj)='solabs_zones'
        msdoc(ijj)='TSolabs'
        metrglbl(ijj)='Solar absorbed (W)'
      elseif(INOG.eq.9)then
        imetget(ijj)=73
        imetmsc(ijj,1)=1
        imetmsc(ijj,2)=0
        msdoc(ijj)='Guth'
        metgroup(ijj)='guth_zones'
        metrglbl(ijj)='Guth comfort (-)'
      elseif(INOG.eq.10)then
        imetget(ijj)=73
        imetmsc(ijj,1)=2
        imetmsc(ijj,2)=0
        msdoc(ijj)='glare'
        metgroup(ijj)='glare_zones'
        metrglbl(ijj)='Glare (-)'
      elseif(INOG.eq.11)then
        imetget(ijj)=73
        imetmsc(ijj,1)=3
        imetmsc(ijj,2)=0
        msdoc(ijj)='DayF'
        metgroup(ijj)='day_zones'
        metrglbl(ijj)='Daylight factors (%)'
      elseif(INOG.eq.12)then
        imetget(ijj)=74
        imetmsc(ijj,1)=1
        imetmsc(ijj,2)=0
        msdoc(ijj)='Plt_C+R&L'
        metgroup(ijj)='pltcrl_zones'
        metrglbl(ijj)='Plnt cnv+rad&lat (W)'
      endif

      INPIC=NCOMP
      CALL EPICKS(INPIC,IVALS,' ',
     &  ' Which zones to associate with this metric: ',
     &  12,NCOMP,zname,' zone list',IER,nbhelp)

      t12=metgroup(ijj)
      CALL EASKS(t12,
     &  'Unique identifier for group of zones associated with metric?',
     &  ' ',12,'offices','metric group name',IER,nbhelp)

C Find base area of each of the zones.
      TFLA=0.
      do 24 mz=1,INPIC
        izmg(ijj,mz)=IVALS(mz)
        TFLA=TFLA+ZBASEA(ivals(mz))
  24  continue
      CALL EASKR(TFLA,' ',' Associated floor area (m^2)? ',
     &  0.0,'F',9999.0,'W',0.,'associated floor m2',IERR,nbhelp)

      icgv=imetmsc(ijj,1)
      CALL EASKI(icgv,'Casual gain type associated with occupancy',
     &  '(0 = full time): ',
     &  0,'F',3,'F',1,'casual type for comfort',IERI,nbhelp)
      if(ieri.eq.-3) then
        goto 43
      else

C Ok to instanciate the data and then jump to 42 to process it.
        nzmg(ijj)=INPIC
        if(t12(1:2).ne.'  '.and.t12(1:4).ne.'UNKN')metgroup(ijj)=t12
        if(ierr.eq.0)emgflr(ijj)=TFLA
        imetmsc(ijj,1)=icgv
        MODSIT=.true.
        goto 42
      endif

C Deal with energy sets (and then return to appropriate point in main menue).
 142  continue
      M=1
      ITME(1) =   ' __set name zones area scaling__'
      if(neds.eq.0)then
        WRITE(ITME(2),'(2A,i3,F7.1,f5.1)')'a ',zedsdoc(1),nzedg(1),
     &    edgflr(1),edgsca(1)
        M=2
      else
        do 10 L=1,neds
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(ITME(M),'(3A,i3,F7.1,f5.1)')key,' ',zedsdoc(L),
     &      nzedg(L),edgflr(L),edgsca(L)
  10    continue
      endif
      ITME(M+1)=   '+ add/delete demand set        '
      if(iaggr.eq.1)then
        ITME(M+2)= '* timestep aggregate >> ON     '
      else
        ITME(M+2)= '* timestep aggregate >> OFF    '
      endif
      ITME(M+3)=   '! list demand sets             '
      ITME(M+4)=   '? help                         '
      ITME(M+5)=   '- exit this menu               '
      nitmes=M+5

C Help text for this menu.
 143  helptopic='ipv_demand_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('IPV energy demand sets',ITME,nitmes,INOE)
      if(INOE.GE.2.and.INOE.le.nitmes-5)then
        iset=INOE-1
        goto 144
      elseif(INOE.EQ.nitmes-4)then

C Add delete copy demand set.
        CALL EASKABC('Demand set options:',' ','add','delete',
     &    'do nothing',IRT,nbhelp)
        if(IRT.eq.1)then
          if(neds.lt.MIPVM)then
            neds=neds+1
            iset=neds
            goto 144
          endif
        elseif(IRT.eq.2)then
          CALL EASKI(IFOC,'Which demand set (give index) ? ',
     &      '(zero to skip)',0,'F',neds,'F',1,'demand set index',
     &      IERI,nbhelp)
          if(ieri.eq.-3) then
            continue
          elseif(ifoc.eq.0)then
            continue
          else
            do 11 is=ifoc,neds-1
              zedsdoc(is)=zedsdoc(is+1)
              idgmsc(is,1)=idgmsc(is+1,1)
              idgmsc(is,2)=idgmsc(is+1,2)
              nzedg(is)=nzedg(is+1)
              edgflr(is)=edgflr(is+1)
              edgsca(is)=edgsca(is+1)
              do 12 iss=1,MCOM
                izedg(is,iss)=izedg(is+1,iss)
  12          continue
  11        continue
            neds=neds-1
            MODSIT=.true.
          endif
        elseif(IRT.eq.3)then
          continue
        endif
      elseif(INOE.EQ.nitmes-3)then
        if(iaggr.eq.1)then
          iaggr=0
          call edisp(iuout,'No timestep aggregate reporting.')
          MODSIT=.true.
        else
          iaggr=1
          call edisp(iuout,'Timestep aggregate reporting included.')
          MODSIT=.true.
        endif
      elseif(INOE.EQ.nitmes-2)then

C List current demand sets..
        call listipvdat(iuout,'d',ier)
      elseif(INOE.EQ.nitmes-1)then
        helptopic='ipv_demand_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('IPV demand sets',nbhelp,'-',0,0,IER)
      elseif(INOE.EQ.nitmes)then
        INO=-4
        GOTO 3
      else
        INOM=-3
        GOTO 143
      endif
      INOM=-4
      GOTO 142

C Editing or creation of a deman set.
  144 continue
      t12=zedsdoc(iset)
      CALL EASKS(t12,'Identifier for this demand set ?',
     &  ' ',12,'offices','demand set name',IER,nbhelp)
      if(t12(1:2).ne.'  '.and.t12(1:4).ne.'UNKN')zedsdoc(iset)=t12

      INPIC=NCOMP
      CALL EPICKS(INPIC,IVALS,' ',
     &  ' Which zones to include in this energy demand set: ',
     &  12,NCOMP,zname,'zone list',IER,nbhelp)
      nzedg(iset)=INPIC
      TFLA=0.
      do 26 mz=1,nzedg(iset)
        izedg(iset,mz)=IVALS(mz)
        TFLA=TFLA+ZBASEA(ivals(mz))
  26  continue
      edgflr(iset)=TFLA
      CALL EASKR(edgflr(iset),' ','Energy set floor area (m^2)? ',
     &  0.0,'F',9999.0,'W',1.,'energy set m2',IER,nbhelp)
      CALL EASKR(edgsca(iset),' ','Multiplier for energy set? ',
     &  0.0,'F',9999.0,'W',1.,'1st set mult',IER,nbhelp)
      MODSIT=.true.
      goto 142

      end

C ************* ipvseasons
C ipvseasons presents a list of typical seasons/assessments and
C returns a string which sumarizes the choice (see list of values
C in subroutine ipvdatinit.
      subroutine ipvseasons(simact)

      DIMENSION ITM(22)
      character ITM*33
      character simact*6
      integer nitms,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ipvseasons'  ! set for subroutine

      simact='ias   '   ! assume annual assessment

    3 INO=-4
      ITM(1)= 'a typical week in winter         '
      ITM(2)= 'b typical fortnight in winter    '
      ITM(3)= 'c winter season (Jan-??)         '
      ITM(4)= 'd typical week in spring         '
      ITM(5)= 'e typical fortnight in spring    '
      ITM(6)= 'f spring season                  '
      ITM(7)= 'g typical week in summer         '
      ITM(8)= 'h typical fortnight in summer    '
      ITM(9)= 'i summer season                  '
      ITM(10)='j typical week in autumn         '
      ITM(11)='k typical fortnight in autumn    '
      ITM(12)='l autumn season                  '
      ITM(13)='m annual assessment (the default)'
      ITM(14)='n three seasons (all days)       '
      ITM(15)='o three seasons (typical week)   '
      ITM(16)='p three seasons (user defined)   '
      ITM(17)='q five seasons (all days)        '
      ITM(18)='r five seasons (typical week)    '
      ITM(19)='s five seasons (user defined)    '
      ITM(20)='t default period                 '
      ITM(21)='? help                           '
      ITM(22)='- exit this menu                 '
      nitms=22

      helptopic='ipv_assessment_regime'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EMENU('Options for assessments',ITM,nitms,INO)

      if(INO.EQ.nitms)then
        return
      elseif(INO.EQ.nitms-1)then
        helptopic='ipv_assessment_regime'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('IPV seasons',nbhelp,'-',0,0,IER)
        goto 3
      elseif(INO.eq.1)then 
        simact='icwint'
      elseif(INO.eq.2)then 
        simact='icwinf'
      elseif(INO.eq.3)then 
        simact='icwins'
      elseif(INO.eq.4)then 
        simact='icsprt'
      elseif(INO.eq.5)then 
        simact='icsprf'
      elseif(INO.eq.6)then 
        simact='icsprs'
      elseif(INO.eq.7)then 
        simact='icsumt'
      elseif(INO.eq.8)then 
        simact='icsumf'
      elseif(INO.eq.9)then 
        simact='icsums'
      elseif(INO.eq.10)then 
        simact='icautt'
      elseif(INO.eq.11)then 
        simact='icautf'
      elseif(INO.eq.12)then 
        simact='icauts'
      elseif(INO.eq.13)then 
        simact='ias   '
      elseif(INO.eq.14)then 
        simact='i3s   '
      elseif(INO.eq.15)then 
        simact='i3t   '
      elseif(INO.eq.16)then 
        simact='i3u   '    ! ? how to communicate the periods?
      elseif(INO.eq.17)then 
        simact='i5s   '
      elseif(INO.eq.18)then 
        simact='i5t   '
      elseif(INO.eq.19)then 
        simact='i5u   '
      elseif(INO.eq.20)then
        simact='i1d   '
      endif
      return

      end

C ************* ipvdatinit
C ipvdatinit initializes IPV data structures based on the passed act
C parameter where
C   act='icwint' initial values using winter climate typical week
C      ='icwinf' initial values using winter climate fortnight
C      ='icwins' initial values using winter climate season
C      ='icsprt' initial values using spring climate typical week
C      ='icsprf' initial values using spring climate fortnight
C      ='icsprs' initial values using spring climate season
C      ='icsumt' initial values using summer climate typical week
C      ='icsumf' initial values using summer climate fortnight
C      ='icsums' initial values using summer climate season
C      ='icautt' initial values using autumn climate typical week
C      ='icautf' initial values using autumn climate fortnight
C      ='icauts' initial values using autumn climate season
C      ='ias' initial annual (365 days)
C      ='i3s' initial 3 season using climate seasons
C      ='i3t' initial 3 season using climate typical week
C      ='i3u' initial 3 season user defined periods
C      ='i5s' initial 5 season using climate seasons
C      ='i5t' initial 5 season using climate typical week
C      ='i5u' initial 5 season user defined periods
C      ='i--' initial values for call from interface
C      ='i1d' initial default period ??
      subroutine ipvdatinit(act)
#include "building.h"
C#include "espriou.h"  
C espriou.h for climatelist file name.
#include "seasons.h"
C seasons.h provides typper and typsea
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

      common/C21/IFCFG,cfgroot,LCFGF

C Simulation parameter sets.
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

      character act*6,LCFGF*72,cfgroot*24,cr*24

C Set root name string and length.
      cr=cfgroot
      lr=lnblnk(cfgroot)

      if(act(1:3).eq.'i--'.or.act(1:3).eq.'I--')then

C If there are 1/3/5 simulation parameter sets then assume the
C same for number of assessments, otherwise set to one annual.
        if(nsset.eq.1)then
          nipvassmt=1
          CALL EDAY(1,1,ipvastjd(1))
          CALL EDAY(31,12,ipvafnjd(1))
          write(ipvadesc(1),'(2A)')cr(1:lr),' annual run'
          nipvdispjd=5
          ipvdispjd(1)=ia1wins+1
          ipvdispjd(2)=ia1sprs+1
          ipvdispjd(3)=iasums+1
          ipvdispjd(4)=ia2sprs+1
          ipvdispjd(5)=ia2wins+1
        elseif(nsset.eq.3)then
          nipvassmt=3
          ipvastjd(1)=ia1wins
          ipvafnjd(1)=ia1winf
          ipvastjd(2)=ia1sprs
          ipvafnjd(2)=ia1sprf
          ipvastjd(3)=iasums
          ipvafnjd(3)=iasumf
          nipvdispjd=3
          ipvdispjd(1)=ia1wins+1
          ipvdispjd(2)=ia1sprs+1
          ipvdispjd(3)=iasums+1
          write(ipvadesc(1),'(2A)')cr(1:lr),' winter run'
          write(ipvadesc(2),'(2A)')cr(1:lr),' transition run'
          write(ipvadesc(3),'(2A)')cr(1:lr),' summer run'
        elseif(nsset.eq.5)then
          nipvassmt=5
          ipvastjd(1)=ia1wins
          ipvafnjd(1)=ia1winf
          ipvastjd(2)=ia1sprs
          ipvafnjd(2)=ia1sprf
          ipvastjd(3)=iasums
          ipvafnjd(3)=iasumf
          ipvastjd(4)=ia2sprs
          ipvafnjd(4)=ia2sprf
          ipvastjd(5)=ia2wins
          ipvafnjd(5)=ia2winf
          nipvdispjd=5
          ipvdispjd(1)=ia1wins+1
          ipvdispjd(2)=ia1sprs+1
          ipvdispjd(3)=iasums+1
          ipvdispjd(4)=ia2sprs+1
          ipvdispjd(5)=ia2wins+1
          write(ipvadesc(1),'(2A)')cr(1:lr),' 1st winter run'
          write(ipvadesc(2),'(2A)')cr(1:lr),' spring run'
          write(ipvadesc(3),'(2A)')cr(1:lr),' summer run'
          write(ipvadesc(4),'(2A)')cr(1:lr),' autumn run'
          write(ipvadesc(5),'(2A)')cr(1:lr),' 2nd winter run'
        else

C Something other than 1/3/5
          nipvassmt=1
          CALL EDAY(1,1,ipvastjd(1))
          CALL EDAY(31,12,ipvafnjd(1))
          nipvdispjd=5
          ipvdispjd(1)=ia1wins+1
          ipvdispjd(2)=ia1sprs+1
          ipvdispjd(3)=iasums+1
          ipvdispjd(4)=ia2sprs+1
          ipvdispjd(5)=ia2wins+1
          write(ipvadesc(1),'(2A)')cr(1:lr),' annual run'
        endif
      elseif(act(1:6).eq.'icwint'.or.act(1:6).eq.'Icwint')then
        nipvassmt=1    ! winter typical week
        ipvastjd(1)=ia1wins
        ipvafnjd(1)=ia1winf
        nipvdispjd=1
        ipvdispjd(1)=ia1wins+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' winter typ'
      elseif(act(1:6).eq.'icwinf'.or.act(1:6).eq.'Icwinf')then
        nipvassmt=1    ! winter typical fortnight
        if(ia1wins.gt.3)then
          ipvastjd(1)=ia1wins-3
          ipvafnjd(1)=ia1winf+4
        else
          ipvastjd(1)=ia1wins
          ipvafnjd(1)=ia1winf+7
        endif
        nipvdispjd=1
        ipvdispjd(1)=ia1wins+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' winter ftn'
      elseif(act(1:6).eq.'icwins'.or.act(1:6).eq.'Icwins')then
        nipvassmt=1    ! winter full season
        ipvastjd(1)=is1wins
        ipvafnjd(1)=is1winf
        nipvdispjd=1
        ipvdispjd(1)=ia1wins+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' winter sea'
      elseif(act(1:6).eq.'icsprt'.or.act(1:6).eq.'Icsprt')then
        nipvassmt=1    ! spring typical week
        ipvastjd(1)=ia1sprs
        ipvafnjd(1)=ia1sprf
        nipvdispjd=1
        ipvdispjd(1)=ia1sprs+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' spring typ'
      elseif(act(1:6).eq.'icsprf'.or.act(1:6).eq.'Icsprf')then
        nipvassmt=1    ! spring typical fortnight
        ipvastjd(1)=ia1sprs-3
        ipvafnjd(1)=ia1sprf+4
        nipvdispjd=1
        ipvdispjd(1)=ia1sprs+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' spring ftn'
      elseif(act(1:6).eq.'icsprs'.or.act(1:6).eq.'Icsprs')then
        nipvassmt=1    ! spring full season
        ipvastjd(1)=is1sprs
        ipvafnjd(1)=is1sprf
        nipvdispjd=1
        ipvdispjd(1)=ia1sprs+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' spring sea'
      elseif(act(1:6).eq.'icsumt'.or.act(1:6).eq.'Icsumt')then
        nipvassmt=1    ! summer typical week
        ipvastjd(1)=iasums
        ipvafnjd(1)=iasumf
        nipvdispjd=1
        ipvdispjd(1)=iasums+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' summer typ'
      elseif(act(1:6).eq.'icsumf'.or.act(1:6).eq.'Icsumf')then
        nipvassmt=1   ! summer typical fortnight
        ipvastjd(1)=iasums-3
        ipvafnjd(1)=iasumf+4
        nipvdispjd=1
        ipvdispjd(1)=iasums+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' summer ftn'
      elseif(act(1:6).eq.'icsums'.or.act(1:6).eq.'Icsums')then
        nipvassmt=1    ! summer full season
        ipvastjd(1)=is1sums
        ipvafnjd(1)=is1sumf
        nipvdispjd=1
        ipvdispjd(1)=iasums+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' summer sea'
      elseif(act(1:6).eq.'icautt'.or.act(1:6).eq.'Icautt')then
        nipvassmt=1    ! autumn typical week
        ipvastjd(1)=ia2sprs
        ipvafnjd(1)=ia2sprf
        nipvdispjd=1
        ipvdispjd(1)=ia2sprs+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' autumn typ'
      elseif(act(1:6).eq.'icautf'.or.act(1:6).eq.'Icautf')then
        nipvassmt=1   ! autumn typical fortnight
        ipvastjd(1)=ia2sprs-3
        ipvafnjd(1)=ia2sprf+4
        nipvdispjd=1
        ipvdispjd(1)=ia2sprs+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' autumn ftn'
      elseif(act(1:6).eq.'icauts'.or.act(1:6).eq.'Icauts')then
        nipvassmt=1    ! autumn full season
        ipvastjd(1)=is2sprs
        ipvafnjd(1)=is2sprf
        nipvdispjd=1
        ipvdispjd(1)=ia2sprs+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' autumn sea'
      elseif(act(1:3).eq.'ias'.or.act(1:3).eq.'Ias')then
        nipvassmt=1    ! annual (full year)
        CALL EDAY(1,1,ipvastjd(1))
        CALL EDAY(31,12,ipvafnjd(1))
        nipvdispjd=5
        ipvdispjd(1)=ia1wins+1
        ipvdispjd(2)=ia1sprs+1
        ipvdispjd(3)=iasums+1
        ipvdispjd(4)=ia2sprs+1
        ipvdispjd(5)=ia2wins+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' annual run'
      elseif(act(1:3).eq.'i1d'.or.act(1:3).eq.'I1d')then
        nipvassmt=1   ! single default period 
        ipvastjd(1)=ia1wins
        ipvafnjd(1)=ia1winf
        nipvdispjd=1
        ipvdispjd(1)=ia1wins+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' single def'
      elseif(act(1:3).eq.'i3s'.or.act(1:3).eq.'I3s')then
        nipvassmt=3    ! three seasons (all days in each)
        ipvastjd(1)=is1wins
        ipvafnjd(1)=is1winf
        ipvastjd(2)=is1sprs
        ipvafnjd(2)=is1sprf
        ipvastjd(3)=is1sums
        ipvafnjd(3)=is1sumf
        nipvdispjd=3
        ipvdispjd(1)=ia1wins+1
        ipvdispjd(2)=ia1sprs+1
        ipvdispjd(3)=iasums+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' winter sea'
        write(ipvadesc(2),'(2A)')cr(1:lr),' transition sea'
        write(ipvadesc(3),'(2A)')cr(1:lr),' summer sea'
      elseif(act(1:3).eq.'i3t'.or.act(1:3).eq.'I3t')then
        nipvassmt=3    ! three seasons (one week in each)
        ipvastjd(1)=ia1wins
        ipvafnjd(1)=ia1winf
        ipvastjd(2)=ia1sprs
        ipvafnjd(2)=ia1sprf
        ipvastjd(3)=iasums
        ipvafnjd(3)=iasumf
        nipvdispjd=3
        ipvdispjd(1)=ia1wins+1
        ipvdispjd(2)=ia1sprs+1
        ipvdispjd(3)=iasums+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' winter run'
        write(ipvadesc(2),'(2A)')cr(1:lr),' transition run'
        write(ipvadesc(3),'(2A)')cr(1:lr),' summer run'
      elseif(act(1:3).eq.'i5s'.or.act(1:3).eq.'I5s')then
        nipvassmt=5    ! five seasons (all days in each)
        ipvastjd(1)=is1wins
        ipvafnjd(1)=is1winf
        ipvastjd(2)=is1sprs
        ipvafnjd(2)=is1sprf
        ipvastjd(3)=is1sums
        ipvafnjd(3)=is1sumf
        ipvastjd(4)=is2sprs
        ipvafnjd(4)=is2sprf
        ipvastjd(5)=is2wins
        ipvafnjd(5)=is2winf
        nipvdispjd=5
        ipvdispjd(1)=is1wins+1
        ipvdispjd(2)=is1sprs+1
        ipvdispjd(3)=is1sums+1
        ipvdispjd(4)=is2sprs+1
        ipvdispjd(5)=is2wins+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' 1st winter sea'
        write(ipvadesc(2),'(2A)')cr(1:lr),' spring sea'
        write(ipvadesc(3),'(2A)')cr(1:lr),' summer sea'
        write(ipvadesc(4),'(2A)')cr(1:lr),' autumn sea'
        write(ipvadesc(5),'(2A)')cr(1:lr),' 2nd winter sea'
      elseif(act(1:3).eq.'i5t'.or.act(1:3).eq.'I5t')then
        nipvassmt=5    ! five seasons (one week in each)
        ipvastjd(1)=ia1wins
        ipvafnjd(1)=ia1winf
        ipvastjd(2)=ia1sprs
        ipvafnjd(2)=ia1sprf
        ipvastjd(3)=iasums
        ipvafnjd(3)=iasumf
        ipvastjd(4)=ia2sprs
        ipvafnjd(4)=ia2sprf
        ipvastjd(5)=ia2wins
        ipvafnjd(5)=ia2winf
        nipvdispjd=5
        ipvdispjd(1)=ia1wins+1
        ipvdispjd(2)=ia1sprs+1
        ipvdispjd(3)=iasums+1
        ipvdispjd(4)=ia2sprs+1
        ipvdispjd(5)=ia2wins+1
        write(ipvadesc(1),'(2A)')cr(1:lr),' 1st winter run'
        write(ipvadesc(2),'(2A)')cr(1:lr),' spring run'
        write(ipvadesc(3),'(2A)')cr(1:lr),' summer run'
        write(ipvadesc(4),'(2A)')cr(1:lr),' autumn run'
        write(ipvadesc(5),'(2A)')cr(1:lr),' 2nd winter run'
      endif

C Reset ddm* data structures for assessments that span a
C whole season.
      if(act(1:6).eq.'icwins'.or.act(1:6).eq.'Icwins')then
        continue
      elseif(act(1:6).eq.'icsprs'.or.act(1:6).eq.'Icsprs')then
        continue
      elseif(act(1:6).eq.'icsums'.or.act(1:6).eq.'Icsums')then
        continue
      elseif(act(1:6).eq.'icauts'.or.act(1:6).eq.'Icauts')then
        continue
      elseif(act(1:3).eq.'ias'.or.act(1:3).eq.'Ias')then
        continue
      elseif(act(1:3).eq.'i3s'.or.act(1:3).eq.'I3s')then
        continue
      elseif(act(1:3).eq.'i5s'.or.act(1:3).eq.'I5s')then
        continue
      else
        return
      endif
      do 106 ij=1,5
        ddmheat(ij)=1.0
        ddmcool(ij)=1.0
        ddmlight(ij)=1.0
        ddmsmlpw(ij)=1.0
        ddmfan(ij)=1.0
        ddmdhw(ij)=1.0
  106 continue
      return
      end


C ************* IPV2SIMPAR
C Copy relevant data from IPV description to simulation parameter
C sets. See subroutine IPVDAT for common block descriptions.
C act is one of the character tags from ipvseasons or recovered
C from the IPV description file.

C << add logic to allow for two extreme seasons

      subroutine ipv2simpar(act)
#include "building.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      COMMON/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/PREC7/ITCNST

C IPV description via ipvdata.h.
      common/IPVF/lipvdatf
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

C Simulation parameter sets.
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      COMMON/ENTFILE/ENTFLNAM,IENTXIST

      character act*6,lipvdatf*72,LCFGF*72,cfgroot*24,cr*24,tag*3
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spfdescr*8
      CHARACTER LAPROB*72,ENTFLNAM*72
      logical MSTROK,MSTRZN

      if(nipvassmt.eq.1.or.nipvassmt.eq.3.or.nipvassmt.eq.5)then
        continue
      else
        call usrmsg('IPV does not use one, three or five simulations',
     &    'so unsure how to setup simulation parameters.','W')
        return
      endif

C If simulation parameter set save level is still -1 then there has
C probably not been a simulation parameter set defined, so set some
C of the common variables.
      if(issave.eq.-1)then
        call edisp(iuout,'Scanning for startup days')
        call scntcnst(TDM,istd,TCM,ISTC)
        if(isstup.eq.0)isstup=ITCNST
        isbnstep=1
        ispnstep=10
        issave=4
        isavgh=0
      endif

      cr=cfgroot
      lr=lnblnk(cfgroot)
      nsset=nipvassmt
      if(nipvassmt.ge.1)then
        do 442 ijk=1,nipvassmt
          call EDAYR(ipvastjd(ijk),isstday(ijk),isstmon(ijk))
          call EDAYR(ipvafnjd(ijk),isfnday(ijk),isfnmon(ijk))
  442   continue
      endif

      if(nipvassmt.eq.1)then
        if(act(1:5).eq.'icwin'.or.act(1:5).eq.'Icwin')then
          tag='win'
        elseif(act(1:5).eq.'icspr'.or.act(1:5).eq.'Icspr')then
          tag='spr'
        elseif(act(1:5).eq.'icsum'.or.act(1:5).eq.'Icsum')then
          tag='sum'
        elseif(act(1:5).eq.'icaut'.or.act(1:5).eq.'Icaut')then
          tag='aut'
        elseif(act(1:3).eq.'ias'.or.act(1:3).eq.'Ias')then
          tag='ann'
        endif
          write(spfdescr(1),'(a)') tag
        if(INDCFG.ne.2)then
          WRITE(sblres(1),'(4A)')cr(1:lr),'_',tag,'.res'
        endif
        if(IAIRN.ge.1)then
          WRITE(sflres(1),'(4A)')cr(1:lr),'_',tag,'.mfr'
        endif
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          WRITE(splres(1),'(4A)')cr(1:lr),'_',tag,'.plr'
        endif
        if(MSTROK)then
          WRITE(smstres(1),'(4A)')cr(1:lr),'_',tag,'.msr'
        endif
        if(ientxist.gt.0)then
          WRITE(selres(1),'(4A)')cr(1:lr),'_',tag,'.elr'
        endif
      elseif(nipvassmt.eq.3)then
        spfdescr(1)='win'
        spfdescr(2)='trn'
        spfdescr(3)='sum'
        if(INDCFG.ne.2)then
          WRITE(sblres(1),'(2A)')cr(1:lr),'_win.res'
          WRITE(sblres(2),'(2A)')cr(1:lr),'_trn.res'
          WRITE(sblres(3),'(2A)')cr(1:lr),'_sum.res'
        endif
        if(IAIRN.ge.1)then
          WRITE(sflres(1),'(2A)')cr(1:lr),'_win.mfr'
          WRITE(sflres(2),'(2A)')cr(1:lr),'_trn.mfr'
          WRITE(sflres(3),'(2A)')cr(1:lr),'_sum.mfr'
        endif
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          WRITE(splres(1),'(2A)')cr(1:lr),'_win.plr'
          WRITE(splres(2),'(2A)')cr(1:lr),'_trn.plr'
          WRITE(splres(3),'(2A)')cr(1:lr),'_sum.plr'
        endif
        if(MSTROK)then
          WRITE(smstres(1),'(2A)')cr(1:lr),'_win.msr'
          WRITE(smstres(2),'(2A)')cr(1:lr),'_trn.msr'
          WRITE(smstres(3),'(2A)')cr(1:lr),'_sum.msr'
        endif
        if(ientxist.gt.0)then
          WRITE(selres(1),'(2A)')cr(1:lr),'_win.elr'
          WRITE(selres(2),'(2A)')cr(1:lr),'_trn.elr'
          WRITE(selres(3),'(2A)')cr(1:lr),'_sum.elr'
        endif
      elseif(nipvassmt.eq.5)then
        spfdescr(1)='win1'
        spfdescr(2)='spr'
        spfdescr(3)='sum'
        spfdescr(4)='aut'
        spfdescr(5)='win2'
        if(INDCFG.ne.2)then
          WRITE(sblres(1),'(2A)')cr(1:lr),'_win1.res'
          WRITE(sblres(2),'(2A)')cr(1:lr),'_spr.res'
          WRITE(sblres(3),'(2A)')cr(1:lr),'_sum.res'
          WRITE(sblres(4),'(2A)')cr(1:lr),'_aut.res'
          WRITE(sblres(5),'(2A)')cr(1:lr),'_win2.res'
        endif
        if(IAIRN.ge.1)then
          WRITE(sflres(1),'(2A)')cr(1:lr),'_win1.mfr'
          WRITE(sflres(2),'(2A)')cr(1:lr),'_spr.mfr'
          WRITE(sflres(3),'(2A)')cr(1:lr),'_sum.mfr'
          WRITE(sflres(4),'(2A)')cr(1:lr),'_aut.mfr'
          WRITE(sflres(5),'(2A)')cr(1:lr),'_win2.mfr'
        endif
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          WRITE(splres(1),'(2A)')cr(1:lr),'_win1.plr'
          WRITE(splres(2),'(2A)')cr(1:lr),'_spr.plr'
          WRITE(splres(3),'(2A)')cr(1:lr),'_sum.plr'
          WRITE(splres(4),'(2A)')cr(1:lr),'_aut.plr'
          WRITE(splres(5),'(2A)')cr(1:lr),'_win2.plr'
        endif
        if(MSTROK)then
          WRITE(smstres(1),'(2A)')cr(1:lr),'_win1.msr'
          WRITE(smstres(2),'(2A)')cr(1:lr),'_spr.msr'
          WRITE(smstres(3),'(2A)')cr(1:lr),'_sum.msr'
          WRITE(smstres(4),'(2A)')cr(1:lr),'_aut.msr'
          WRITE(smstres(5),'(2A)')cr(1:lr),'_win2.msr'
        endif
        if(ientxist.gt.0)then
          WRITE(selres(1),'(2A)')cr(1:lr),'_win1.elr'
          WRITE(selres(2),'(2A)')cr(1:lr),'_spr.elr'
          WRITE(selres(3),'(2A)')cr(1:lr),'_sum.elr'
          WRITE(selres(4),'(2A)')cr(1:lr),'_aut.elr'
          WRITE(selres(5),'(2A)')cr(1:lr),'_win2.elr'
        endif
      endif
      if(lnblnk(lipvdatf).eq.0)then
      elseif(lipvdatf(1:7).eq.'UNKNOWN')then
      else
        WRITE(sipvres,'(2A)')cr(1:lr),'ipv.rep'
      endif
      return
      end


C ******* getmultip *******
C getmultip looks for typical week in each season based on closest
C degree days and radiation patterns. Note: adapted from DDRADSUM
C in clmsyn.F Logic includes logic that if the heating or cooling
C derived from scaling is less than a simple time multiplier then
C the time multiplier is used.

C << If there are 5 seasons this works ok. What if the user
C << is using 3 seasons?  Or if the user has defined their own
C << periods for assessment e.g. a fortnight?

      SUBROUTINE getmultip
#include "building.h"
#include "esprdbfile.h"
#include "seasons.h"
C seasons.h provides typper and typsea
#include "ipvdata.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

C Redirected text/graphics parameters.
      character xfile*144,tg*1,delim*1
      common/exporttg/xfile,tg,delim
      common/exporttgi/ixopen,ixloc,ixunit

C dm* variables are from ipvdata.h.
C sea* variables are from season.h

      character outs*124

      real HBT,CBT   ! local base temperatures for heating and cooling DD calcs

C For each of the seasons:
C  cddratio is the season / assessment ratio for cooling
C  hddratio is the season / assessment ratio for heating
      dimension cddratio(MIPVA)
      dimension hddratio(MIPVA)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='getmultip'  ! set for subroutine

      if(is1wins.eq.0.or.is2wins.eq.0.or.is1sprs.eq.0)then
        call usrmsg('No winter|transition|summer season definitions',
     &    'found in the climate database. Skipping.','W')
        return
      endif

C Initial weightings for heating dd, cooling dd, solar radiation (from seasons.h).
      hddw=1.0
      cddw=1.0
      radw=1.0

C Set delimiter in reports to standard/literal format.
      delim = '-'

C First check if climate datase exists: if so open.
      CALL ERPFREE(ICLIM,ISTAT)

C << todo take into account location >>
      CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
      if(ISTAT.ge.0)then
        CALL ERPFREE(ICLIM,ISTAT)
        CALL FPRAND(ICLIM,ISTAT,144,1,LCLIM)
        call edisp(iuout,' opened climate file')
      endif

C Scan the climate file for CLAT and CLONG details.
      IREC=366
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)ICYEAR,ICDNGH
      IF(ICYEAR.EQ.0)goto 1000
      IREC=368
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG

C Help text for the dialogs
      helptopic='seasonal_climate_scan'
      call gethelptext(helpinsub,helptopic,nbhelp)

      if(hddbaset.lt.0.1)then
        hddbaset=17.0
      endif
      HBT=hddbaset
      CALL EASKR(HBT,' ',' Heating base temperature ? ',
     &  -10.,'W',40.,'W',17.0,'DD heating base temp',IER,nbhelp)
      hddbaset=HBT

      if(cddbaset.lt.0.1)then
        cddbaset=21.0
      endif
      CBT=cddbaset
      CALL EASKR(CBT,' ',' Cooling base temperature ? ',
     &  -10.,'W',40.,'W',21.0,'DD cooling base temp',IER,nbhelp)
      cddbaset=CBT

C If output to file alter the edisp unit number.
      itru = iuout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file... ',' ','-')
      endif

C Write out beginning of report.
      WRITE(outs,12) hddbaset,cddbaset
  12  FORMAT(' Degree day analysis: heating base at',F6.1,' & cooling',
     &       F6.1,' Deg C')
      call edisp(itru,outs)
      call edisp(itru,'  ')
      
C Loop through the climate file and calculate data. Do not bother
C to update the variables for typical weeks in climatelist.
      itrc=2
      call clmddscan(itrc,itru,'s','p',5,iyear)

C If there are three assessments used in the IPV advise the user of
C alternative ratios which combine both winters and both transition
C seasons.
      if(nipvassmt.eq.3)then

C Convert from julian day of the start of the assessment to the end
C of the assessment to number of days in period and get ratio with
C the number of days in the two seasons.
        ijulst=iwkbstrt(1)
        ijulfn=iwkbstrt(1)+6
        dayratio=(((is1winf-is1wins)+1)+((is2winf-is2wins)+1))/
     &           ((ijulfn-ijulst)+1)

C Ratio of first and second winter total heating and cooling degree
C days and the best first winter assessment week degree days.
        if(wkheatdd(1).gt.0.01)then
          xx=(seahddtot(1)+seahddtot(5))/wkheatdd(1)
          hddratio(1)=(seahddtot(1)+seahddtot(5))/wkheatdd(1)
          if(xx.gt.dayratio)then
            hddratio(1)=xx
          else
            hddratio(1)=dayratio
          endif
        else
          hddratio(1)=dayratio
        endif
        if(wkcooldd(1).gt.0.01)then
          xx=(seacddtot(1)+seacddtot(5))/wkcooldd(1)
          if(xx.gt.dayratio)then
            cddratio(1)=xx
          else
            cddratio(1)=dayratio
          endif
        else
          cddratio(1)=dayratio
        endif
        write(outs,'(a,F5.2,a,F5.2,a,F5.2,a)')'is ',dayratio,' days & ',
     &    hddratio(1),' HDD & ',cddratio(1),' CDD.'
        call easkab(
     &    'The ratio of the winter assessment to all winter seasons',
     &    outs,'accept','continue',
     &    iok,nbhelp)
        if(iok.eq.1)then
          dmheat(1)=hddratio(1)
          dmcool(1)=cddratio(1)
          dmlight(1)=dayratio
          dmsmlpw(1)=dayratio
          dmfan(1)=dayratio
          dmdhw(1)=dayratio
        endif

C Do the transitional seasons.
        ijulst=iwkbstrt(2)
        ijulfn=iwkbstrt(2)+6
        dayratio=(((is1sprf-is1sprs)+1)+((is2sprf-is2sprs)+1))/
     &           ((ijulfn-ijulst)+1)
        if(wkheatdd(2).gt.0.01)then
          xx=(seahddtot(2)+seahddtot(4))/wkheatdd(2)
          if(xx.gt.dayratio)then
            hddratio(2)=xx
          else
            hddratio(2)=dayratio
          endif
        else
          hddratio(2)=dayratio
        endif
        if(wkcooldd(2).gt.0.01)then
          xx=(seacddtot(2)+seacddtot(4))/wkcooldd(2)
          if(xx.gt.dayratio)then
            cddratio(2)=xx
          else
            cddratio(2)=dayratio
          endif
        else
          cddratio(2)=dayratio
        endif
        write(outs,'(a,F5.2,a,F5.2,a,F5.2,a)')'is ',dayratio,' days & ',
     &    hddratio(2),' HDD & ',cddratio(2),' CDD.'
        call easkab(
     &    'The ratio of the trans assessment to all trans seasons',
     &    outs,'accept','continue',
     &    iok,nbhelp)
        if(iok.eq.1)then
          dmheat(2)=hddratio(2)
          dmcool(2)=cddratio(2)
          dmlight(2)=dayratio
          dmsmlpw(2)=dayratio
          dmfan(2)=dayratio
          dmdhw(2)=dayratio
        endif
      endif

      RETURN

C Error message trap.
 1000 call edisp(IUOUT,' No data in file')
      RETURN
      END

