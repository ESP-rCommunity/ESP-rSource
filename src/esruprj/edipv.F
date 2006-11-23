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
C IPV2SIMPAR: copy relevant data from IPV description to
C             simulation parameter sets.
C getmultip: looks for typical week in each season based on closest
C            degree days and radiation patterns.
C CLMGET: (duplicate of code in esruclm/cfiles.F)
C AZALT: (duplicate of code in esruclm/clmint.F)

C ************* IPVCMD
C IPVCMD acts on command line IPV directives. Allows model to be
C altered based on information held in IPV definition.
      subroutine IPVCMD(act,inf)
#include "building.h"
#include "uncertainty.h"

      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/C21/IFCFG,cfgroot,LCFGF

C Path to model and command line file (if any). Browse
C is a logical flag, .true. restricts update/save options
      common/rcmd/LCMDFL

C IPV description.
      common/IPVF/lipvdatf

C nipvassmt is the number of assessments (runs) in an IPV (1=annual,
C 3=win/trn/sum, 5=win/spr/sum/autumn/win. 
C ipvastjd() are the assessment start julian dates,
C ipvafnjd are the assessment finish julian dates and
C nipvdispjd number of days to display in detail,
C ipvdispjd is list of julian days (must be within the assessment period).
C ipvadesc is a 40 char description associated with each assessment.
      common/IPVSEA/nipvassmt,nipvdispjd,ipvastjd(MIPVA),
     &              ipvafnjd(MIPVA),ipvdispjd(10)

C IDST tool commands
      common/idsta/CMDCFG,CMDCLM,CMDCLMN,CMDNOTE,CMDRES,CMDLBL(10),
     &             CMDLNAM(10)
      common/idstb/nidst
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)

C Control commons (for command file updating)
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)

C Current file (for use by low level I/O calls)
      common/curfile/currentfile

      character*(*) inf
      character LCFGF*72
      character LCMDFL*144,outs*124,outstr*124,cfgroot*24
      character lipvdatf*72
      character CMDCFG*72,CMDCLM*72,CMDCLMN*72,CMDNOTE*64,CMDRES*72
      character ALOC*12,ALOCLBL*12,ALOCTYP*4,CMDLBL*12,CMDLNAM*80
      CHARACTER word*24
      character cpth*48,cmdn*48,runmode*12,keepres*4,donegdst*72
      CHARACTER currentfile*72
      character act*1

C Check if IPV file known and fill common blocks before reading command data.
      if(lnblnk(lipvdatf).eq.0)then
        call usrmsg('IPV not defined. Cannot process command.',' ','W')
        goto 223
      elseif(lipvdatf(1:7).eq.'UNKNOWN')then
        call usrmsg('IPV UNKNOWN. Cannot process command.',' ','W')
        goto 223
      else
        call ripvdat(IFCFG,lipvdatf,ier)
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
C          write(6,*)'idst ',nidst
          goto 223
        endif
        if(OUTSTR(1:5).eq.'*note')then
          k=6
          CALL EGETRM(OUTSTR,K,CMDNOTE,'W','cmd note',IER)
        elseif(OUTSTR(1:9).eq.'*run_mode')then
          k=10
          CALL EGETW(OUTSTR,K,runmode,'W','runmod',IER)
        elseif(OUTSTR(1:13).eq.'*keep_results')then
          k=14
          CALL EGETW(OUTSTR,K,keepres,'W','keep',IER)
        elseif(OUTSTR(1:9).eq.'*abs_path')then
          k=10
          CALL EGETRM(OUTSTR,K,cpth,'W','cfg path',IER)
        elseif(OUTSTR(1:8).eq.'*win_cfg')then
          k=8
          CALL EGETRM(OUTSTR,K,CMDCFG,'W','config',IER)
          if(cpth(1:2).ne.'  ')then
            write(CMDCFG,'(a,a)') cpth(1:lnblnk(cpth)),
     &        cmdn(1:lnblnk(cmdn))
          else
            write(CMDCFG,'(a)') cmdn(1:lnblnk(cmdn))
          endif
          LCMDFL=CMDCFG
        elseif(OUTSTR(1:8).eq.'*tra_cfg')then
          k=8
          CALL EGETRM(OUTSTR,K,CMDCFG,'W','config',IER)
          if(cpth(1:2).ne.'  ')then
            write(CMDCFG,'(a,a)') cpth(1:lnblnk(cpth)),
     &        cmdn(1:lnblnk(cmdn))
          else
            write(CMDCFG,'(a)') cmdn(1:lnblnk(cmdn))
          endif
          LCMDFL=CMDCFG
        elseif(OUTSTR(1:8).eq.'*sum_cfg')then
          k=8
          CALL EGETRM(OUTSTR,K,CMDCFG,'W','config',IER)
          if(cpth(1:2).ne.'  ')then
            write(CMDCFG,'(a,a)') cpth(1:lnblnk(cpth)),
     &        cmdn(1:lnblnk(cmdn))
          else
            write(CMDCFG,'(a)') cmdn(1:lnblnk(cmdn))
          endif
          LCMDFL=CMDCFG
        elseif(OUTSTR(1:13).eq.'*name_climate')then
          k=14
          CALL EGETRM(OUTSTR,K,CMDCLMN,'W','climate alias',IER)
        elseif(OUTSTR(1:8).eq.'*climate')then
          k=9
          CALL EGETRM(OUTSTR,K,CMDCLM,'W','climate',IER)
        elseif(OUTSTR(1:16).eq.'*name_ext_window')then
          k=17
          CALL EGETRM(OUTSTR,K,CMDLNAM(nidst),'W',
     &      'ext window long name',IER)
        elseif(OUTSTR(1:11).eq.'*ext_window')then

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
          CMDLBL(nidst)='ext_window'
C          write(6,*) word
C          write(6,*) nidst,ALOC(nidst)
        elseif(OUTSTR(1:16).eq.'*name_int_window')then
          k=17
          CALL EGETRM(OUTSTR,K,CMDLNAM(nidst),'W',
     &      'int window long name',IER)
        elseif(OUTSTR(1:11).eq.'*int_window')then

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
          CMDLBL(nidst)='int_window'
C          write(6,*) word
C          write(6,*) nidst,ALOC(nidst)
        elseif(OUTSTR(1:16).eq.'*name_atr_window')then
          k=17
          CALL EGETRM(OUTSTR,K,CMDLNAM(nidst),'W',
     &      'atr window long name',IER)
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
C          write(6,*) word
C          write(6,*) nidst,ALOC(nidst)
        elseif(OUTSTR(1:4).eq.'*res')then
          k=4
          CALL EGETRM(OUTSTR,K,CMDRES,'W','report',IER)
        elseif(OUTSTR(1:18).eq.'*update_ctl_period')then

C Update control timing (for use with BEMS software). Data is:
C bld control loop index, period number, (new) start time.
          K=19
          CALL EGETWI(OUTSTR,K,ibcli,1,ncf,'W',
     &      'ctl loop indx',IER)
          CALL EGETWI(OUTSTR,K,ibcldt,1,nbcdt(ibcli),
     &      'W','ctl day type',IER)
          CALL EGETWI(OUTSTR,K,ibclp,1,
     &       nbcdp(ibcli,ibcldt),'-','ctl period',IER)
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
        call mkipvdat(ifcfg,lipvdatf)
        CALL EMKCFG('-',IER)
        call procgdst(itrc,runmode,keepres,donegdst,ier)
      endif

      return
      end

C ************* IPVDAT
C Define information used in generating IPV.
      subroutine ipvdat(act)
#include "building.h"

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      COMMON/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,6),IUZBASEA(MCOM)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C IPV description.
      common/IPVF/lipvdatf

C  ipvtitl = main title of IPV
C  ipvvers = version text i.e. 'base case'
C  ipvsynop= up to 248 characters of text.
      common/IPVA/ipvtitl,ipvvers,ipvsynop

C List of metric sets: nms is number of sets, imetget is the
C esrures iget index for the metric selected, imetmsc holds two
C miscelaneous indices for use by the metric, nzmg is the
C number of zones in each set, emgflr is the total floor area in
C each set, emgsca is a scaling factor (e.g. to scale these zones
C towards a whole building), emgwtg is a weighting factor for the
C metric, izmg is the list of zones in each set.
C metrglbl is 20 char label equivalent to GLABEL in esrures
C msdoc is 12 char identifier for each metric set.
C metgroup is 12 char identifier for the group of zones associated
C   with this metric.
      common/IPVMS/nms,imetget(MIPVM),imetmsc(MIPVM,2),nzmg(MIPVM),
     &  emgflr(MIPVM),emgsca(MIPVM),emgwtg(MIPVM),izmg(MIPVM,MCOM)
      common/IPVMDS/metrglbl(MIPVM),msdoc(MIPVM),metgroup(MIPVM)

C List of energy demand sets: neds is number of sets, idgmsc() are
C miscel data describing set, iaggr=0 no timestep aggregate reporting
C iaggr=1 timestep aggregate data included, nzedg is the
C number of zones in each set, edgflr is the total floor area in
C each set, edgsca is a scaling factor (e.g. to scale these zones
C towards a whole building), izedg is the list of zones in each set.
C zedsdoc is 12 char identifier for each set.
      common/IPVEDS/neds,idgmsc(MIPVM,2),iaggr,nzedg(MIPVM),
     &  edgflr(MIPVM),edgsca(MIPVM),izedg(MIPVM,MCOM)
      common/IPVDEDS/zedsdoc(MIPVM)

C nipvassmt is the number of assessments (runs) in an IPV (1=annual,
C 3=win/trn/sum, 5=win/spr/sum/autumn/win. 
C ipvastjd() are the assessment start julian dates,
C ipvafnjd are the assessment finish julian dates and
C nipvdispjd number of days to display in detail,
C ipvdispjd is list of julian days to display (must be within an
C   assessment period).
C ipvadesc is a 40 char description associated with each assessment.
      common/IPVSEA/nipvassmt,nipvdispjd,ipvastjd(MIPVA),
     &              ipvafnjd(MIPVA),ipvdispjd(10)
      common/IPVSEALB/ipvadesc(MIPVA)

C Season definitions (from climatelist file). 2 periods for winter
C (i.e. nov-dec and jan-feb), transition (i.e. mar-may & sep-oct)
C and one period for summer.
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf

C Typical periods win1|trn1|sum|trn2|win2 with start,finish
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf

C ddm* are the degree-day multipliers (or user defined multipliers) between
C the periods simulated and the whole season for heating, cooling, lights,
C small power, fans, domestic hot water. For nipvassmt=1 these are 
C initially set to 1.0.
      common/IPVDDM/ddmheat(MIPVA),ddmcool(MIPVA),ddmlight(MIPVA),
     &              ddmsmlpw(MIPVA),ddmfan(MIPVA),ddmdhw(MIPVA)

C dm* are the degree-day and day-ratio multipliers between the
C typical periods and the whole season for heating (DD), cooling (DD),
C lights (ratio), small power (ratio), fans (ratio), domestic hot
C water (ratio). For nipvassmt=1 these are initially set to 1.0.
C This information is potentially used to support IPV calculations.
      common/CLMDM/dmheat(MSPS),dmcool(MSPS),dmlight(MSPS),
     &              dmsmlpw(MSPS),dmfan(MSPS),dmdhw(MSPS)
      common/C22/ICLIM,LCLIM
      common/clmlst/cdblbl,cdblfil

      common/IPVI/nipvimg,lipvimg(4)

C Description of distributed demands and file name.
      COMMON/BL1/dmdsdesc,bdmds

C Simulation parameter sets.
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

C ITM is main IPV menu.
C ITMGET is menu for selecting metric types.
C ITME is menu for energy deman sets.
C ITMM is menu for metric sets.
      DIMENSION ITMGET(15),ITM(31),ITMM(13),ITME(14),IVALS(MCOM)
      character ITM*48,ITMM*41,ITME*33,ITMGET*33,H*72,hold*40,hold5*50
      character act*1,zname*12,zdesc*64,ipvtitl*40,ipvvers*40
      character ipvsynop*248,lipvimg*72,lipvdatf*72,outs*124
      CHARACTER DS*7,DS1*10
      character*72 LTMP,LCFGF
      CHARACTER dmdsdesc*248,bdmds*72
      character LCLIM*72,cdblbl*20,cdblfil*72
      character cfgroot*24,cr*24,t40*40,t72*72,t248*248
      character descr*7,descrst*10,descrfn*10
      character descra*7,descrb*7,descrc*7,descrd*7,descre*7
      character ipvadesc*40,t12*12,key*1
      character zedsdoc*12,msdoc*12,metrglbl*20,metgroup*12
      logical MODSIT,XST,OK,next,changed

C If act = 'i' then initialise variables. Initial assumption
C is for an annual assessment.
      t40=' '
      t72=' '
      cr=cfgroot
      lr=lnblnk(cfgroot)

C Check an see if there are any default periods from the climatelist.
C If not instanciate season start and end dates.
      if((ia1wins+ia1sprs+iasums+ia2sprs+ia2wins).eq.0)then
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

      if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then

C Initialise the IPV common blocks.
        call clearipvdat(act,ier)

C If there are 1/3/5 simulation parameter sets then assume the
C same for number of assessments, otherwise set to one annual.
        if(nsset.eq.3)then
          nipvassmt=3
          ipvastjd(1)=ia1wins
          ipvafnjd(1)=ia1winf
          ipvastjd(2)=ia1sprs
          ipvafnjd(2)=ia1sprf
          ipvastjd(3)=iasums
          ipvafnjd(3)=iasumf
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
          write(ipvadesc(1),'(2A)')cr(1:lr),' 1st winter run'
          write(ipvadesc(2),'(2A)')cr(1:lr),' spring run'
          write(ipvadesc(3),'(2A)')cr(1:lr),' summer run'
          write(ipvadesc(4),'(2A)')cr(1:lr),' autumn run'
          write(ipvadesc(5),'(2A)')cr(1:lr),' 2nd winter run'
        else

C << if there is one simulation parameter set then if the dates
C << are different the user could confirm whether it should be
C << used or not.
          nipvassmt=1
          CALL EDAY(1,1,ipvastjd(1))
          CALL EDAY(31,12,ipvafnjd(1))
          write(ipvadesc(1),'(2A)')cr(1:lr),' annual run'
        endif
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

C ipvastjd() are the assessment start julian dates,
C ipvafnjd() are the assessment finish julian dates
C Rescan climatelist file to acquire seasons and periods.
      INQUIRE (FILE=cdblfil,EXIST=XST)
      if(XST)then
        IUF=IFIL+2
        call scancdblist(IUF,cdblfil,LCLIM,ok,ier)
        if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
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
        endif
      else

C Set defaults as in esrucom/rcdblist.F Only set ipvastjd and ipvafnjd
C if subroutine was called in initilisation mode.
        if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
          CALL EDAY(9,1,ipvastjd(1))
          CALL EDAY(15,1,ipvafnjd(1))
          CALL EDAY(6,3,ipvastjd(2))
          CALL EDAY(12,3,ipvafnjd(2))
          CALL EDAY(11,7,ipvastjd(3))
          CALL EDAY(17,7,ipvafnjd(3))
          CALL EDAY(2,10,ipvastjd(4))
          CALL EDAY(8,10,ipvafnjd(4))
          CALL EDAY(20,11,ipvastjd(5))
          CALL EDAY(26,11,ipvafnjd(5))
        endif
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

    3 INO=-4
      IIER=0

      WRITE(ITM(1),'(2A)')     'a title   : ',ipvtitl(1:32)
      WRITE(ITM(2),'(2A)')     'b version : ',ipvvers(1:32)
      WRITE(ITM(3),'(2A)')     'c synopsis: ',ipvsynop(1:32)
      WRITE(ITM(4),'(A,I3)')   'd images  : ',nipvimg
      WRITE(ITM(5),'(a)') 
     &  ' ______________________________________________'
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
        WRITE(ITM(9),'(4a,i4,2a)') 'j  annual ',descrst,' ',descrfn,
     &    jjd1,' ',ipvadesc(1)(1:10)
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
  4   h(1)='Definition of an Integrated Performance View (IPV) '
      h(2)=' '
      h(3)='It includes descriptive information: title, version'
      h(4)='and synopsis and one or more images. These can be'
      h(5)='edited directly.'
      h(6)=' '
      h(7)='Performance metrics (comfort, zone dry bulb temp, etc.)'
      h(8)='are associated with a set of thermal zones and have'
      h(9)='weighting factors (to assist in ranking performance).'
      h(10)=' '
      h(11)='Demand sets are sets of zones for which heating, cooling'
      h(12)='lighting, small power and DHW are reported. Each set'
      h(13)='has a scaling factor so that, for example 2 offices'
      h(14)='zones can be scaled to represent 15 offices.'
      h(15)=' '
      h(16)='Annual performance is scaled from an annual run, three'
      h(17)='seasonal runs: winter/transition/summer, or five runs:'
      h(18)='1st winter/spring/summer/autumn/2nd winter. Each run '
      h(19)='has date and heating and cooling degree day ratio '
      h(20)='attributes. Periods can be setup based on a best-fit'
      h(21)='scan of climate data (see option 5).'
      h(22)=' '
      h(23)='Display days: are specific days (up to 10) for which'
      h(24)='timestep data is recorded.'
      h(25)=' '
      h(26)='Seasons are based on information in the `climatelist`'
      h(27)='file. This information is included to clarify the'
      h(28)='relationship between each season and the 1/3/5 sim-'
      h(29)='ulations being run. '
      h(30)=' '
      h(31)='Ratios: are typically ratios between the number of'
      h(32)='days (or heating/cooling degree days) of the season'
      h(33)='and the assessment period. For an annual simulation'
      h(34)='the ratios are 1.00.'
      h(35)=' '
      h(36)='Option 5 `re-scan climate` is a two step process;'
      h(37)='the first to scan the `climatelist` file for the '
      h(38)='days in each season; the second being scanning the'
      h(39)='climate data to find the best fit week in each'
      h(40)='season. '
      h(41)=' '
      h(42)='Setting up an integrated performance view: '
      h(43)='a) edit the title version and synopsis, '
      h(44)='b) select relevant performance metrics for the project,'
      h(45)='c) select the number of simulations,'
      h(46)='d) re-scan the climate file for seasons,'
      h(47)='e) re-scan the climate for ratios,'
      h(48)='f) select display days (within the simulations).'
      h(49)=' '

      if(mmod.eq.8)then
        CALL EMENU('Integrated Performance View data',ITM,nitms,INO)
      else
        CALL EMENU('IPV description',ITM,nitms,INO)
      endif

      if(INO.EQ.nitms)then

C If data updated, write to IPV definition file and update link in cfg.
C Also update or create matched simulation parameter sets.
        if(MODSIT)then
          h(1)='This file holds a description of an Integrated '
          h(2)='Performance View of this model, including periods,'
          h(3)='climatic degree day information & associated zones.'
          ltmp=lipvdatf
          CALL EASKS(ltmp,'IPV description file ?',
     &      ' ',72,'xxx.ipv','IPV file',IER,3)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN') lipvdatf=ltmp

C Write out the current IPV definitions.
          call mkipvdat(ifil+1,lipvdatf)

          if(nipvassmt.eq.1.or.nipvassmt.eq.3.or.nipvassmt.eq.5)then

C If one of the standard number of IPV assessments transfer the relevant
C data into the simulation parameter set data structure.
            call ipv2simpar('-')
          endif
          CALL EMKCFG('-',IER)
        endif
        return
      elseif(INO.EQ.nitms-1)then
        CALL PHELPD('IPV setup',10,'-',0,0,IER)
      elseif(INO.EQ.1)then
        H(1)='The main IPV title (<=40 char). '
        t40=ipvtitl
        CALL EASKS(t40,'IPV tile?',' ',40,'TITLE','IPV Tile',IER,1)
        if(t40(1:2).ne.'  ')ipvtitl=t40
        MODSIT=.true.
      elseif(INO.EQ.2)then
        H(1)='The version of the project or IPV (<=40 char). As in'
        h(2)='base case or reference 1 etc. '
        t40=ipvvers
        CALL EASKS(t40,'version ?',' ',40,'base','version',IER,2)
        if(t40(1:2).ne.'  ')ipvvers=t40
        MODSIT=.true.
      elseif(INO.EQ.3)then
        call edisp(iuout,'Current synopsis...')
        h(1)='The synopsis is up to 248 characters of text to describe'
        h(2)='the model and intent of the ipv. You will be presented'
        h(3)='with the synopsis in sections for purposes of editing.'
        h(4)='(you can skip to the left and right within the block'
        h(5)='as required). '
        t248=ipvsynop
        CALL EASKS248(t248,'Synopsis',' ',72,
     &    'No sysnopsis provided for this model.','synopsis',IER,5)
        if(t248(1:2).ne.'  ')ipvsynop=t248
        MODSIT=.true.
      elseif(INO.EQ.4)then

C Images associated with the IPV.
        H(1)='Images (GIF) can be included in an IPV.'
        write(outs,'(a,i2,a)') 'There are currently ',nipvimg,
     &    ' images associated with this IPV.'
        CALL EASKABCD(outs,'Choices: ','list',
     &      'edit','add','cancel',IW,1)
        if(IW.eq.1)then
          if(nipvimg.gt.0)then
            do 52 i=1,nipvimg
              call edisp(iuout,lipvimg(i))
   52       continue
          endif
        elseif(IW.eq.2)then
          CALL EASKI(IFOC,'Which image (give index) ? ',' ',
     &        1,'F',nipvimg,'F',1,'ipv image',IER,1)
          h(1)='This image file will be associated with the IPV. '
          t72=lipvimg(IFOC)
          CALL EASKS(t72,'Image file name:',' ',72,'xxx','image file',
     &      IER,1)
          if(t72(1:2).ne.'  ')lipvimg(IFOC)=t72
        elseif(IW.eq.3)then
          nipvimg=nipvimg+1
          if(nipvimg.le.4)then
            lipvimg(nipvimg)=' '
            t72=lipvimg(nipvimg)
            CALL EASKS(t72,'Image file name:',' ',72,'xx','image file',
     &        IER,1)
            if(t72(1:2).ne.'  ')lipvimg(nipvimg)=t72
          else
            nipvimg=4
          endif
        elseif(IW.eq.4)then
          INO=-1
          goto 3
        endif
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
        h(1)='An IPV can be created from one or three or five separate'
        h(2)='simulations. A single run would normally be an annual '
        h(3)='simulation. A set of three runs would be equivalent'
        h(4)='to winter/transition/summer assessments and a set'
        h(5)='of five runs would be equivalent to 1st winter/spring/'
        h(6)='summer/autumn/2nd winter assessments.'
        h(7)=' '
        h(8)='The period of the assessment can be the full number of'
        h(9)='days in the year or season (or fewer days). One approach'
        h(10)='is to simulation one week in each season and then scale'
        h(11)='the predictions. The `re-scan climate` facility can be'
        h(12)='used to quickly determine both the tyical weeks to '
        h(13)='simulate as well as applicable ratios.'
        h(14)=' '
        h(15)='Note: you can run short simulations for other periods,'
        h(16)='but you will have to provide your own ratios.'
        h(17)=' '
        h(18)='The use of less-than-a-year simulations is often useful'
        h(19)='for larger models or for models which require a short '
        h(20)='simulation timestep. With carefully selected ratios '
        h(21)='the scaled annual predictions can be very close to that'
        h(22)='of an explicit annual simulation. '
        CALL EASKABCD(' Options:',' ','one (annual) run',
     &   'three seasons','five seasons','continue',IRO,22)
        cr=cfgroot
        lr=lnblnk(cfgroot)
        if(IRO.eq.1)then
          m=9
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
          MODSIT=.true.
        elseif(IRO.eq.2)then
          m=11
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
          MODSIT=.true.
        elseif(IRO.eq.3)then

C Five seasons - ask if single week or full season to be included
C in the assessment. If full season set scaling to 1.0.
          h(1)='You ca reduce the computation time and file size if'
          h(2)='you apply scaling factors from short assessments to'
          h(3)='seasonal performance (ESP-r can derive these). Or '
          h(4)='you can run a full season. '
          call EASKABC('Choices for length of assessment:','(see help)',
     &      'one week','full season','cancel',IW,4)
          if(IW.eq.3) then
            continue 
          elseif(IW.eq.1) then
            m=13
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
          elseif(IW.eq.2) then
            m=13
            nipvassmt=5
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
            do 106 ij=1,5
              ddmheat(ij)=1.0
              ddmcool(ij)=1.0
              ddmlight(ij)=1.0
              ddmsmlpw(ij)=1.0
              ddmfan(ij)=1.0
              ddmdhw(ij)=1.0
  106       continue
          endif
          if(IW.eq.1.or.IW.eq.2) then
            write(ipvadesc(1),'(2A)')cr(1:lr),' 1st winter run'
            write(ipvadesc(2),'(2A)')cr(1:lr),' spring run'
            write(ipvadesc(3),'(2A)')cr(1:lr),' summer run'
            write(ipvadesc(4),'(2A)')cr(1:lr),' autumn run'
            write(ipvadesc(5),'(2A)')cr(1:lr),' 2nd winter run'
            MODSIT=.true.
          endif
        elseif(IRO.eq.4)then
          continue
        endif
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
        H(1)='Simulation start day & month and finish day and month. '
        H(2)=' '
        CALL STDATE(IYEAR,ipvastjd(1),DS,DS1)
        write(h(3),'(a,i4,2x,a)') ' Start : ',ipvastjd(1),DS1
        CALL STDATE(IYEAR,ipvafnjd(1),DS,DS1)
        write(h(4),'(a,i4,2x,a)') ' Finish: ',ipvafnjd(1),DS1
        CALL PHELPD('periods',4,'-',0,0,IER)
        call edayr(ipvastjd(1),id1,im1)
        call edayr(ipvafnjd(1),id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        if(nipvassmt.eq.1)then
          CALL EASKS(HOLD,'Simulation period: start (day & month) and',
     &      'finish (day & month):',40,' 9 1 15 1','single sim period',
     &      IIER,4)
        elseif(nipvassmt.eq.3)then
          CALL EASKS(HOLD,'Winter period: start (day & month) and',
     &      'finish (day & month):',40,' 9 1 15 1','winter sim period',
     &      IIER,4)
        elseif(nipvassmt.eq.5)then
          CALL EASKS(HOLD,'1st winter period: start (day & month) and',
     &      'finish (day & month):',40,' 9 1 15 1','winter sim period',
     &      IIER,4)
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
        H(1)='Simulation start day & month and finish day and month. '
        H(2)=' '
        CALL STDATE(IYEAR,ipvastjd(2),DS,DS1)
        write(h(3),'(a,i4,2x,a)') ' Start : ',ipvastjd(2),DS1
        CALL STDATE(IYEAR,ipvafnjd(2),DS,DS1)
        write(h(4),'(a,i4,2x,a)') ' Finish: ',ipvafnjd(2),DS1
        CALL PHELPD('periods',4,'-',0,0,IER)
        call edayr(ipvastjd(2),id1,im1)
        call edayr(ipvafnjd(2),id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        if(nipvassmt.eq.3)then
          CALL EASKS(HOLD,'Transition period: start (day & month) and',
     &      'finish (day & month):',40,' 9 1 15 1','trns sim period',
     &      IIER,4)
        elseif(nipvassmt.eq.5)then
          CALL EASKS(HOLD,'Spring period: start (day & month) and',
     &      'finish (day & month):',40,' 9 1 15 1','spring sim period',
     &      IIER,4)
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
        H(1)='Simulation start day & month and finish day and month. '
        H(2)=' '
        CALL STDATE(IYEAR,ipvastjd(3),DS,DS1)
        write(h(3),'(a,i4,2x,a)') ' Start : ',ipvastjd(3),DS1
        CALL STDATE(IYEAR,ipvafnjd(3),DS,DS1)
        write(h(4),'(a,i4,2x,a)') ' Finish: ',ipvafnjd(3),DS1
        CALL PHELPD('periods',4,'-',0,0,IER)
        call edayr(ipvastjd(3),id1,im1)
        call edayr(ipvafnjd(3),id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Summer period: start (day & month) and',
     &    'finish (day & month):',40,' 9 1 15 1','summer sim period',
     &    IIER,4)
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
        H(1)='Simulation start day & month and finish day and month. '
        H(2)=' '
        CALL STDATE(IYEAR,ipvastjd(4),DS,DS1)
        write(h(3),'(a,i4,2x,a)') ' Autumn start : ',ipvastjd(4),DS1
        CALL STDATE(IYEAR,ipvafnjd(4),DS,DS1)
        write(h(4),'(a,i4,2x,a)') ' Autumn finish: ',ipvafnjd(4),DS1
        CALL PHELPD('periods',4,'-',0,0,IER)
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
        H(1)='Simulation start day & month and finish day and month. '
        H(2)=' '
        CALL STDATE(IYEAR,ipvastjd(5),DS,DS1)
        write(h(3),'(a,i4,2x,a)') ' 2nd winter start : ',ipvastjd(4),DS1
        CALL STDATE(IYEAR,ipvafnjd(5),DS,DS1)
        write(h(4),'(a,i4,2x,a)') ' 2nd winter finish: ',ipvafnjd(4),DS1
        CALL PHELPD('periods',4,'-',0,0,IER)
        call edayr(ipvastjd(5),id1,im1)
        call edayr(ipvafnjd(5),id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'2nd winter period: start (day & month) and',
     &    'finish (day & month):',40,' 9 1 15 1','2nd win sim period',
     &    IIER,4)
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
C << initial values should be defined when the number of assessments
C << and the assessment periods were setup (e.g. the 2nd day of each
C << assessment.
        if(nipvdispjd.gt.0)then
          do 195 ij=1,nipvdispjd
  194       hold = ' '
            H(1)='Display day & month. This is a day (one of up to 10)'
            H(2)='where detailed hourly data will be recovered.'
            CALL STDATE(IYEAR,ipvdispjd(ij),DS,DS1)
            write(h(3),'(a,i4,2x,a)') ' Display day: ',ipvdispjd(ij),DS1
            write(outs,'(a,i4,2x,a)') ' Display day: ',ipvdispjd(ij),DS1
            call edisp(iuout,outs)
            call edayr(ipvdispjd(ij),id1,im1)
            WRITE(HOLD,'(2I4)')id1,im1
            CALL EASKS(HOLD,'Display day & month: (should be',
     &        '(within a simulation period):',40,' 9 1','disp day',
     &        IIER,3)
            K=0
            CALL EGETWI(HOLD,K,id1,1,31,'W','sim st day',IIER)
            CALL EGETWI(HOLD,K,im1,1,12,'W','sim st month',IIER)
            CALL EDAY(id1,im1,ipvdispjd(ij))
            if(iier.ne.0)goto 194
            MODSIT=.true.
  195     continue
        endif
        CALL EASKAB('Options: ',' ','add another day','continue',IW,1)
        if(IW.eq.1)then
          nipvdispjd=nipvdispjd+1
          ij=nipvdispjd
  196     HOLD='  12   12 '
          CALL EASKS(HOLD,'Display day & month: (should be',
     &      '(within a simulation period):',40,' 9 1','disp day',
     &      IIER,2)
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
        H(1)='These multipliers allow for scaling of predictions'
        H(2)='from a simulation period to a season.  If the'
        h(3)='simulation period equals the season then use 1.0 or'
        h(4)='use either a heating degree day ratio or a day ratio'
        h(5)=' '
        h(6)='If single simulation only edit first number. '
        h(7)='If three simulations only edit first three numbers.'
        h(8)='If five simulations edit all numbers.'
        h(9)=' '
        h(10)='Note: the `rescan climate` function can be used to'
        h(11)='find these ratios if the simulation period is a '
        h(12)='week.'
        CALL EASKS(HOLD5,'Seasonal multiplier for heating:',
     &    ' ',50,' 17. 17. 17. 17.  17.','heating dd',IIER,12)
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
        H(1)='These multipliers allow for scaling of predictions'
        H(2)='from a simulation period to a season.  If the'
        h(3)='simulation period equals the season then use 1.0 or'
        h(4)='use either a cooling degree day ratio or a day ratio'
        h(5)=' '
        h(6)='If single simulation only edit first number. '
        h(7)='If three simulations only edit first three numbers.'
        h(8)='If five simulations edit all numbers.'
        h(9)=' '
        h(10)='Note: the `rescan climate` function can be used to'
        h(11)='find these ratios if the simulation period is a '
        h(12)='week.'
        CALL EASKS(HOLD5,'Seasonal multiplier for cooling:',
     &    ' ',50,' 17. 17. 17. 17.  17.','cooling dd',IIER,12)
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
        H(1)='These multipliers allow for scaling of predictions'
        H(2)='from a simulation period to a season.  If the'
        h(3)='simulation period equals the season then use 1.0 or'
        h(4)='use either a daylighting ratio or a day ratio'
        h(5)=' '
        h(6)='If single simulation only edit first number. '
        h(7)='If three simulations only edit first three numbers.'
        h(8)='If five simulations edit all numbers.'
        h(9)=' '
        h(10)='Note: the `rescan climate` function can be used to'
        h(11)='find these ratios if the simulation period is a '
        h(12)='week.'
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
        H(1)='These multipliers allow for scaling of predictions'
        H(2)='from a simulation period to a season.  If the'
        h(3)='simulation period equals the season then use 1.0 or'
        h(4)='use a day ratio (simulation period -> season)'
        h(5)=' '
        h(6)='If single simulation only edit first number. '
        h(7)='If three simulations only edit first three numbers.'
        h(8)='If five simulations edit all numbers.'
        h(9)=' '
        h(10)='Note: the `rescan climate` function can be used to'
        h(11)='find these ratios if the simulation period is a '
        h(12)='week.'
        CALL EASKS(HOLD5,'Seasonal multiplier for small power:',
     &    ' ',50,' 17. 17. 17. 17.  17.','small pwr dd',IIER,12)
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
        H(1)='These multipliers allow for scaling of predictions'
        H(2)='from a simulation period to a season.  If the'
        h(3)='simulation period equals the season then use 1.0 or'
        h(4)='use a day ratio (simulation period -> season)'
        h(5)=' '
        h(6)='If single simulation only edit first number. '
        h(7)='If three simulations only edit first three numbers.'
        h(8)='If five simulations edit all numbers.'
        h(9)=' '
        h(10)='Note: the `rescan climate` function can be used to'
        h(11)='find these ratios if the simulation period is a '
        h(12)='week.'
        CALL EASKS(HOLD5,'Seasonal multiplier for fans & pumps:',
     &    ' ',50,' 17. 17. 17. 17.  17.','fan dd',IIER,12)
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
        H(1)='These multipliers allow for scaling of predictions'
        H(2)='from a simulation period to a season.  If the'
        h(3)='simulation period equals the season then use 1.0 or'
        h(4)='use a day ratio (simulation period -> season).'
        h(5)=' '
        h(6)='If single simulation only edit first number. '
        h(7)='If three simulations only edit first three numbers.'
        h(8)='If five simulations edit all numbers.'
        h(9)=' '
        h(10)='Note: the `rescan climate` function can be used to'
        h(11)='find these ratios if the simulation period is a '
        h(12)='week.'
        CALL EASKS(HOLD5,'Seasonal multipliers for domestic hot water:',
     &    ' ',50,' 17. 17. 17. 17.  17.','DHW dd',IIER,12)
        K=0
        CALL EGETWR(HOLD5,K,ddmdhw(1),1.,999.,'W','win DHW',IIER)
        CALL EGETWR(HOLD5,K,ddmdhw(2),1.,999.,'W','spr DHW',IIER)
        CALL EGETWR(HOLD5,K,ddmdhw(3),1.,999.,'W','sum DHW',IIER)
        CALL EGETWR(HOLD5,K,ddmdhw(4),1.,999.,'W','aut DHW',IIER)
        CALL EGETWR(HOLD5,K,ddmdhw(5),1.,999.,'W','win DHW',IIER)
        if(iier.ne.0)goto 101
        MODSIT=.true.
      elseif(INO.EQ.m+14)then
        h(1)='Each climate data set may have different seasons and'
        h(2)='different typical assessment periods. Choosing this '
        h(3)='will rescan a `climatelist` file and update display.'
        h(4)=' '
        h(5)='Both the climate tool and the project manager can scan'
        h(6)='the current climate file to locate the best fit week '
        h(7)='in each season and return the ratios of days or degree'
        h(8)='days (for heating and cooling) for use in scaling short'
        h(9)='simulations to whole seasons.'
        CALL EASKABC('Options: ',' ','scan for seasons & periods',
     &      'scan for day/DD ratios','continue',IW,9)
        if(IW.eq.1.or.IW.eq.2)then

C Rescan climate to acquire ratios.
          INQUIRE (FILE=cdblfil,EXIST=XST)
          if(XST)then
            IUF=IFIL+2
            call scancdblist(IUF,cdblfil,LCLIM,ok,ier)
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

C Local (stripped) version of a subroutine from clmsyn.F
          call getmultip
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
 43   h(1)='Definition of an Integrated Performance View (IPV)'
      h(2)='includes one or more performance metrics. These are'
      h(3)='a subset of the metrics which can be found in the'
      h(4)='results analysis module. '
      h(5)=' '
      h(6)='Each selected metric has an associated list of zones,'
      h(7)='and a weighting factor (to give priority in multi-'
      h(8)='criteria assessments.'

      CALL EMENU('IPV metric sets',ITMM,nitmms,INOM)
      if(INOM.GE.2.and.INOM.le.nitmms-4)then
        ijj=INOM-1
        goto 44
      elseif(INOM.EQ.nitmms-3)then

C Add delete copy metric set.
        h(1)='You can add another metric set or delete an existing set'
        CALL EASKABC('Metric set options:',' ','add','delete',
     &    'do nothing',IRT,1)
        if(IRT.eq.1)then
          if(nms.lt.MIPVM)then
            nms=nms+1
            ijj=nms
            goto 44
          endif
        elseif(IRT.eq.2)then
          CALL EASKI(IFOC,'Which metric set (give index) ? ',
     &      '(zero to skip)',0,'F',nms,'F',1,'demand set index',IER,1)
          if(ifoc.eq.0)then
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
        CALL PHELPD('IPV metrics',8,'-',0,0,IER)
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
      ITMGET(13)='? help                      '
      ITMGET(14)='- exit                      '
      nitmget=14

C Help text for this menu.
      h(1)='This is a sub-set of the metrics that can be selected'
      h(2)='in the results analysis module. '
      h(3)=' '
      h(4)='For each metric the IPV will include statistics,'
      h(5)='frequency reporting and timestep data for display days.'

      CALL EMENU('Metric options',ITMGET,nitmget,INOG)
      if(INOG.eq.nitmget)then
        goto 43
      elseif(INOG.eq.nitmget-1)then
        CALL PHELPD('metric options',5,'-',0,0,IER)
        goto 777
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
     &  12,NCOMP,zname,' zone list',IER,1)
      nzmg(ijj)=INPIC

      h(1)='Give a short (12 char or less) unique identifier for the'
      h(2)='group of zones associated with this metric. '
      t12=metgroup(ijj)
      CALL EASKS(t12,
     &  'Unique identifier for group of zones associated with metric?',
     &  ' ',12,'offices','metric group name',IER,2)
      if(t12(1:2).ne.'  '.and.t12(1:4).ne.'UNKN')metgroup(ijj)=t12

C Find base area of each of the zones.
      TFLA=0.
      do 24 mz=1,nzmg(ijj)
        izmg(ijj,mz)=IVALS(mz)
        TFLA=TFLA+ZBASEA(ivals(mz))
  24  continue
      H(1)='Floor area for zones associated with this metric.'
      CALL EASKR(TFLA,' ',' Associated floor area (m^2)? ',
     &  0.0,'F',9999.0,'W',0.,'associated floor m2',IER,1)
      if(ier.eq.0)emgflr(ijj)=TFLA

      h(1)='If metric is to be assessed only during occupied '
      h(2)='periods then supply the casual gain type assiciated '
      h(3)='with occupancy. A zero means full time occupancy. '
      icgv=imetmsc(ijj,1)
      CALL EASKI(icgv,'Casual gain type associated with occupancy',
     &  '(0 = full time): ',
     &  0,'F',3,'F',1,'casual type for comfort',IER,3)
      if(ier.eq.0)imetmsc(ijj,1)=icgv
      MODSIT=.true.
      goto 42


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
 143  h(1)='Definition of an Integrated Performance View (IPV)'
      h(2)='include one or more energy demand sets. These are groups'
      h(3)='of zones which can be combined and/or scaled for'
      h(4)='reporting and/or analysis purposes.'

      CALL EMENU('IPV energy demand sets',ITME,nitmes,INOE)
      if(INOE.GE.2.and.INOE.le.nitmes-5)then
        iset=INOE-1
        goto 144
      elseif(INOE.EQ.nitmes-4)then

C Add delete copy demand set.
        h(1)='You can add another demand set or delete an existing set'
        CALL EASKABC('Demand set options:',' ','add','delete',
     &    'do nothing',IRT,1)
        if(IRT.eq.1)then
          if(neds.lt.MIPVM)then
            neds=neds+1
            iset=neds
            goto 144
          endif
        elseif(IRT.eq.2)then
          CALL EASKI(IFOC,'Which demand set (give index) ? ',
     &      '(zero to skip)',0,'F',neds,'F',1,'demand set index',IER,1)
          if(ifoc.eq.0)then
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
        CALL PHELPD('IPV demand sets',4,'-',0,0,IER)
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
      h(1)='Give a short (12 char or less) identifier for this '
      h(2)='energy demand set to help you remember why you'
      h(3)='created this set.'
      t12=zedsdoc(iset)
      CALL EASKS(t12,'Identifier for this demand set ?',
     &  ' ',12,'offices','demand set name',IER,3)
      if(t12(1:2).ne.'  '.and.t12(1:4).ne.'UNKN')zedsdoc(iset)=t12

      h(1)='Pick one, several or all zones for inclusion. You can'
      h(2)='have one or two sets of zones which can be scaled up '
      h(3)='to represent a whole building. One set might'
      h(4)='represent offices and the other support zones.'
      INPIC=NCOMP
      CALL EPICKS(INPIC,IVALS,' ',
     &  ' Which zones to include in this energy demand set: ',
     &  12,NCOMP,zname,'zone list',IER,4)
      nzedg(iset)=INPIC
      TFLA=0.
      do 26 mz=1,nzedg(iset)
        izedg(iset,mz)=IVALS(mz)
        TFLA=TFLA+ZBASEA(ivals(mz))
  26  continue
      edgflr(iset)=TFLA
      h(1)='Floor area for this energy related set of zones.'
      CALL EASKR(edgflr(iset),' ','Energy set floor area (m^2)? ',
     &  0.0,'F',9999.0,'W',1.,'energy set m2',IER,1)
      h(1)='Multiplier from this set to building. Example: if'
      h(2)='set results needed to be multiplied by 4.2 '
      CALL EASKR(edgsca(iset),' ','Multiplier for energy set? ',
     &  0.0,'F',9999.0,'W',1.,'1st set mult',IER,1)
      MODSIT=.true.
      goto 142

      end


C ************* IPV2SIMPAR
C Copy relevant data from IPV description to simulation parameter
C sets. See subroutine IPVDAT for common block descriptions.
      subroutine ipv2simpar(act)
#include "building.h"

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      COMMON/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/PREC7/ITCNST

C IPV description.
      common/IPVF/lipvdatf
      common/IPVSEA/nipvassmt,nipvdispjd,ipvastjd(MIPVA),
     &              ipvafnjd(MIPVA),ipvdispjd(10)
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

C Pressure database.
      common/APRES/LAPRES,IFPRE

C Simulation parameter sets.
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      COMMON/ENTFILE/ENTFLNAM,IENTXIST

      character act*1,lipvdatf*72,LCFGF*72,cfgroot*24,cr*24
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spfdescr*8
      CHARACTER*72 LAPROB,LAPRES,ENTFLNAM
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
        spfdescr(1)='ann'
        if(INDCFG.ne.2)then
          WRITE(sblres(1),'(2A)')cr(1:lr),'_ann.res'
        endif
        if(IAIRN.ge.1)then
          WRITE(sflres(1),'(2A)')cr(1:lr),'_ann.mfr'
        endif
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          WRITE(splres(1),'(2A)')cr(1:lr),'_ann.plr'
        endif
        if(MSTROK)then
          WRITE(smstres(1),'(2A)')cr(1:lr),'_ann.msr'
        endif
        if(ientxist.gt.0)then
          WRITE(selres(1),'(2A)')cr(1:lr),'_ann.elr'
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
      SUBROUTINE getmultip
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMDT0/CLAT,CLONG
      COMMON/RADTYP/IDNGH
      common/C22/ICLIM,LCLIM

      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)

C Season definitions. 2 periods for winter (i.e. nov-dec and
C jan-feb), transition (i.e. mar-may & sep-oct) and one period for summer.
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf

C dm* are the degree-day and day-ratio multipliers between the
C typical periods and the whole season for heating (DD), cooling (DD),
C lights (ratio), small power (ratio), fans (ratio), domestic hot
C water (ratio). For nipvassmt=1 these are initially set to 1.0.
C This information is potentially used to support IPV calculations.
      common/CLMDM/dmheat(MSPS),dmcool(MSPS),dmlight(MSPS),
     &              dmsmlpw(MSPS),dmfan(MSPS),dmdhw(MSPS)
      common/IPVSEA/nipvassmt,nipvdispjd,ipvastjd(MIPVA),
     &              ipvafnjd(MIPVA),ipvdispjd(10)

      character H*72,outs*124,PERST1*14,PERST2*44,PERST3*44
      character DESCR*7,DESCR1*10,title*40,hold*32
      character LCLIM*72
      DIMENSION ALT(24),AZI(24),GHSUM(MIPVA),DAYGLOB(MIPVA)
      dimension seacddwk(MSPS),seahddtot(MSPS),seacddtot(MSPS)
      dimension seahddwk(MSPS)

C For each of the seasons: wkdiff is the difference in the performance
C parameters for the assessment period and the season, iwkbest
C is the index of the best week, iwkbstrt is the julian day start
C of the best week, wkheatdd is heating degree days in the seasons
C best week and wkcooldd is the cooling degree days in the seasons
C best week.
      dimension wkdiff(MIPVA),iwkbest(MIPVA),iwkbstrt(MIPVA)
      dimension wkheatdd(MIPVA),wkcooldd(MIPVA),cddratio(MIPVA)
      dimension hddratio(MIPVA)

      if(is1wins.eq.0.or.is2wins.eq.0.or.is1sprs.eq.0)then
        call usrmsg('No winter|transition|summer season definitions',
     &    'found in the climate database. Skipping.','W')
        return
      endif

C Initial weightings for heading dd, cooling dd, solar radiation.
      hddw=1.0
      cddw=1.0
      radw=1.0

C First check if it exists: if so open, otherwise create with 0 length.
      CALL ERPFREE(ICLIM,ISTAT)
      CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
      if(ISTAT.ge.0)then
        CALL ERPFREE(ICLIM,ISTAT)
        CALL FPRAND(ICLIM,ISTAT,144,1,LCLIM)
        call edisp(iuout,' opened climate file')
      endif

C Scan the climate file for some of its details.
      IREC=366
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR,IDNGH
      IF(IYEAR.EQ.0)goto 1000
      IREC=367
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLMLOC
      IREC=368
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG

      H(1)='Degree day calculations are derived from a specific '
      H(2)='base temperature (deg C). '
      HBT=17.
      CALL EASKR(HBT,' ',' Heating base temperature ? ',
     &           -10.,'W',40.,'W',17.0,'DD heating base temp',IER,2)
      CBT=21.
      CALL EASKR(CBT,' ',' Cooling base temperature ? ',
     &           -10.,'W',40.,'W',21.0,'DD cooling base temp',IER,2)

C If output to file alter the edisp unit number.
      itru = iuout

C Reset text feedback.
      DNM=0.
      HRM=0.
      UPMC=0.
      HRMC=0.

      WRITE(outs,12)HBT,CBT
  12  FORMAT(' Degree day analysis: heating base at',F6.1,' & cooling',
     &       F6.1,' Deg C')
      call edisp(itru,outs)
      call edisp(itru,'  ')

C INITIALISE PARAMETERS FOR WHOLE PERIOD
      DNI=0
      HRTOT=0.
      UPIC=0
      HRTOTC=0.

C Loop through each seasonal period.
      do 42 k=1,MSPS
        GHSUM(k)=0.
        DAYGLOB(k)=0.
        seahddwk(k)=0.
        seacddwk(k)=0.
        seahddtot(k)=0.
        seacddtot(k)=0.
        dmheat(k)=1.0
        dmcool(k)=1.0
        dmlight(k)=1.0
        dmsmlpw(k)=1.0
        dmfan(k)=1.0
        dmdhw(k)=1.0
        if(k.eq.1)then
          title='Winter (early year) season '
          DNI=0
          HRTOT=0.
          UPIC=0
          HRTOTC=0.
          IDSS=is1wins
          IDSF=is1winf
        elseif(k.eq.2)then
          title='Spring season '
          IDSS=is1sprs
          IDSF=is1sprf
        elseif(k.eq.3)then
          title='Summer season '
          IDSS=is1sums
          IDSF=is1sumf
        elseif(k.eq.4)then
          title='Autumn season '
          IDSS=is2sprs
          IDSF=is2sprf
        elseif(k.eq.5)then
          title='Winter (late year) season '
          IDSS=is2wins
          IDSF=is2winf
        endif
        CALL EPERSTR(IYEAR,IDSS,0,IDSF,24,1,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

C For each day read climate data, set parameters and analyse.
        DO 10 I=IDSS,IDSF
          IDD=I
          CALL CLMGET(IDD)
          CALL AZALT(IDD,1,CLONG,CLAT,ALT,AZI)
          DNJ=0
          UPJC=0.
          DAYL=24.0
          DO 20 J=1,24
            IF(ALT(J).LT.0.001)THEN
              DIRH=0.
            else
              IF(IDNGH.EQ.0)THEN
                DIRH=CDNR(J)*ESIND(ALT(J))
              ELSE
                DIRH=CDNR(J)-CDIF(J)
              ENDIF
              GHSUM(k)=(GHSUM(k)+DIRH+CDIF(J))
            endif

            TT=CTMP(J)
            IF(TT.GT.HBT)then
            else

C T below heating BASE,SAVE DEG-HRS IN DNJ
              DNJ=DNJ+(HBT-TT)
            endif
            IF(TT.GT.CBT)then

C T above cooling BASE ,SAVE DEG-HRS IN UPJC
              UPJC=UPJC+(TT-CBT)
            endif
   20     CONTINUE
          XDN=DNJ/24
          XUPC=UPJC/24

C Update seasonal parameters and go to the next day.
          DNM=DNM+DNJ
          HRM=HRM+DAYL
          UPMC=UPMC+UPJC
          HRMC=HRMC+DAYL
   10   CONTINUE

C Compute parameters (total & average degree days) for this season.
        XHR=24.0/HRM
        XHRC=24.0/HRMC
        XDN=DNM/24
        XUPC=UPMC/24
        YDN=XHR*XDN
        seahddwk(k)=YDN*7.
        YUPC=XHRC*XUPC
        seacddwk(k)=YUPC*7.

C Number of days in season is IDSF-IDSS+1. DAYGLOB is daily mean
C global horizontal solar for season.
        DAYGLOB(k)=(GHSUM(k)/(IDSF-IDSS+1))/1000.

C Report on the current season.
        call eddisp(itru,' ')
        WRITE(outs,'(3a)')title(1:lnblnk(title)),' ',PERST2
        call eddisp(itru,outs)
        WRITE(outs,'(a,F8.2,a,F8.2,a,F9.2)')' Heat DD avg/day   ',YDN,
     &    '  Cool DD avg/day  ',YUPC,'  Rad avg/day ',DAYGLOB(k)
        call eddisp(itru,outs)
        WRITE(outs,'(a,F8.2,a,F8.2,a,F9.2)')' Heat DD avg/week  ',
     &    seahddwk(k),'  Cool DD avg/week ',seacddwk(k),
     &    '  Rad avg/week',DAYGLOB(k)*7.
        call eddisp(itru,outs)
        WRITE(outs,'(a,F8.2,a,F8.2,a,F9.2)')' Season heat DD    ',XDN,
     &     '  Season cool DD   ',XUPC,'  Rad total   ',GHSUM(k)/1000.
        call eddisp(itru,outs)
        seahddtot(k)=XDN
        seacddtot(k)=XUPC

C UPDATE WHOLE PERIOD
        HRTOT=HRTOT+HRM
        DNI=DNI+DNM
        DNM=0.
        HRM=0.
        HRTOTC=HRTOTC+HRMC
        UPIC=UPIC+UPMC
        UPMC=0.
        HRMC=0.
  42  continue

C Loop through each week in each season and check against the
C calculated average data.
C Loop through each seasonal period.
      h(1)='Typical weeks are found by scanning the climate data'
      h(2)='for a week in each season which has the least deviation'
      h(3)='in heating degree days (DD), cooling degree days, and'
      h(4)='solar radiation. '
      h(5)=' '
      h(6)='It reports the parameters it uses to determine the'
      h(7)='best fit as well as several ratios between the best'
      h(8)='week and the season (ratio of julian days, ratio of'
      h(9)='heating degree days and ratio of cooling degree days'
      h(10)='all of which are parameters required for generating'
      h(11)='an integrated performance view (IPV).'
      call easkab('Options:',' ','search for typical weeks',
     &  'continue',iok,11)
      if(iok.eq.2)return

C Allow user to define weightings.
      H(1)='Weightings for heating degree days, cooling degree days'
      H(2)='and solar radiation can be given. The default is equal'
      H(3)='weightings for each.'
      write(hold,'(3f6.3)') hddw,cddw,radw
 143  CALL EASKS(HOLD,'Weightings for HDD CDD Solar:',' ',32,
     &  ' 1.0 1.0 1.0 ','weightings for hdd cdd solar',IER,3)
      K=0
      CALL EGETWR(HOLD,K,hddw,0.0,1.0,'W','hdd weight',IER)
      CALL EGETWR(HOLD,K,cddw,0.0,1.0,'W','cdd weight',IER)
      CALL EGETWR(HOLD,K,radw,0.0,1.0,'W','solar weight',IER)
      if(ier.ne.0)goto 143

C Loop through each of the assessment periods.
      IWK=0
      do 43 k=1,MIPVA
        if(k.eq.1)then
          call edisp(itru,'Winter (early year) season.')
          IDSS=is1wins
          IDSF=is1winf
        elseif(k.eq.2)then
          call edisp(itru,'Spring season.')
          IDSS=is1sprs
          IDSF=is1sprf
        elseif(k.eq.3)then
          call edisp(itru,'Summer season.')
          IDSS=is1sums
          IDSF=is1sumf
        elseif(k.eq.4)then
          call edisp(itru,'Autumn season.')
          IDSS=is2sprs
          IDSF=is2sprf
        elseif(k.eq.5)then
          call edisp(itru,'Winter (late year) season.')
          IDSS=is2wins
          IDSF=is2winf
        endif
        iwkbest(k)=0
        iwkbstrt(k)=0
        wkdiff(k)=3.
        GHSUMW=0.
        itrc=0
        CALL EPERSTR(IYEAR,IDSS,0,IDSF,24,1,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
        WRITE(outs,'(a)')PERST2
        call edisp(itru,outs)

C For each day read climate data, set parameters and analyse.
C Find day of week for start of the period. IWK is week number, IDSOW
C is the julian day at the start of the week.
        CALL EDAYR(IDSS,IDAYN,IMTHN)
        CALL EWEEKD(IDAYN,IMTHN,IYEAR,IXDWK)
        IDSOW=IDSS
        ix=0

C For each day read climate data, set parameters and analyse.
        DO 11 I=IDSS,IDSF
          IDD=I
          ix=ix+1
          CALL CLMGET(IDD)
          CALL AZALT(IDD,1,CLONG,CLAT,ALT,AZI)
          DNJ=0
          UPJC=0
          DO 21 J=1,24
            IF(ALT(J).LT.0.001)THEN
              DIRH=0.
            else
              IF(IDNGH.EQ.0)THEN
                DIRH=CDNR(J)*ESIND(ALT(J))
              ELSE
                DIRH=CDNR(J)-CDIF(J)
              ENDIF
              GHSUMW=(GHSUMW+DIRH+CDIF(J))
            endif

            TT=CTMP(J)
            IF(TT.GT.HBT)then
            else

C T below heating BASE,SAVE DEG-HRS IN DNJ
              DNJ=DNJ+(HBT-TT)
            endif
            IF(TT.GT.CBT)then

C T above cooling BASE ,SAVE DEG-HRS IN UPJC
              UPJC=UPJC+(TT-CBT)
            endif
   21     CONTINUE

C Convert day to D,M
          II=I
          CALL EDAYR(II,IDD,IMM)
          CALL EWEEKD(IDD,IMM,IYEAR,IDWK)

C Prepare for output, check if week complete.
          IF(II.eq.IDSS)GO TO 41
          IF(IDWK.ne.IXDWK)GO TO 41
          iwk=iwk+1

C Compute parameters (total & average degree days) for this week.
          XHR=24.0/HRM
          XHRC=24.0/HRMC
          XDN=DNM/24
          XUPC=UPMC/24
          call stdate(iyear,IDSOW,DESCR,DESCR1)
          YDN=XHR*XDN
          YUPC=XHRC*XUPC

C Deal with radiation.
          if(seahddwk(k).gt.0.001)then
            PARM1=ABS(XDN-seahddwk(k))/seahddwk(k)
          else
            PARM1=0.
          endif
          if(seacddwk(k).gt.0.001)then
            PARM2=ABS(XUPC-seacddwk(k))/seacddwk(k)
          else
            PARM2=0.
          endif
          PARM3=ABS((GHSUMW/1000.)-(DAYGLOB(k)*7.))/(DAYGLOB(k)*7.)
          PARMTOT=(PARM1*hddw)+(PARM2*cddw)+(PARM3*radw)
          if(PARMTOT.lt.wkdiff(k))then
            wkdiff(k)=PARMTOT
            iwkbest(k)=iwk
            iwkbstrt(k)=IDSOW
            wkheatdd(k)=XDN
            wkcooldd(k)=XUPC
          endif

C Reset weekly total global horizontal.
          GHSUMW=0.

C Update whole period.
          IDSOW=II
          HRTOT=HRTOT+HRM
          DNI=DNI+DNM
          DNM=0.
          HRM=0.
          HRTOTC=HRTOTC+HRMC
          UPIC=UPIC+UPMC
          UPMC=0.
          HRMC=0.

C CONVERT TO DEG-DAYS PER DAY
  41      CONTINUE
          XDN=DNJ/24
          XUPC=UPJC/24

C UPDATE PARAMETERS FOR THIS week
          DNM=DNM+DNJ
          HRM=HRM+DAYL
          UPMC=UPMC+UPJC
          HRMC=HRMC+DAYL

C AND GO TO NEXT DAY
   11   CONTINUE

C NOW LAST week, compute parameters for last week
        XHR=24.0/HRM
        XHRC=24.0/HRMC
        XDN=DNM/24
        XUPC=UPMC/24
        iwk=iwk+1
        call stdate(iyear,IDSOW,DESCR,DESCR1)
        YDN=XHR*XDN
        YUPC=XHRC*XUPC

C Deal with radition here for last week of season.
        if(seahddwk(k).gt.0.001)then
          PARM1=ABS(XDN-seahddwk(k))/seahddwk(k)
        else
          PARM1=0.
        endif
        if(seacddwk(k).gt.0.001)then
          PARM2=ABS(XUPC-seacddwk(k))/seacddwk(k)
        else
          PARM2=0.
        endif
        PARM3=ABS((GHSUMW/1000.)-(DAYGLOB(k)*7.))/(DAYGLOB(k)*7.)
        PARMTOT=(PARM1*hddw)+(PARM2*cddw)+(PARM3*radw)
        WRITE(outs,'(a,3F9.3,a,F9.3)')' Params (heat|cool|solar)',
     &    PARM1,PARM2,PARM3,' Params total ',PARMTOT
        call edisp(itru,outs)
        if(PARMTOT.lt.wkdiff(k))then
          wkdiff(k)=PARMTOT
          iwkbest(k)=iwk
          iwkbstrt(k)=IDSOW
          wkheatdd(k)=XDN
          wkcooldd(k)=XUPC
        endif
        call stdate(iyear,iwkbstrt(k),DESCR,DESCR1)
        WRITE(outs,'(a,i3,3a,F6.2,a,F6.2)')' Best week is nb',
     &    iwkbest(k),' ',DESCR1,' with HDD of ',wkheatdd(k),
     &    ' & CDD of ',wkcooldd(k)
        call edisp(itru,outs)

C Convert from julian day of the start of the assessment to the end
C of the assessment to number of days in period and get ratio with
C the number of days in the season.
        ijulst=iwkbest(k)
        ijulfn=iwkbest(k)+6

C Ratio of seasonal totoal heating and cooling degree days and the
C best assessment week heating and cooling degree days.
        if(wkheatdd(k).gt.0.01)then
          hddratio(k)=seahddtot(k)/wkheatdd(k)
        else
          hddratio(k)=1.0
        endif
        if(wkcooldd(k).gt.0.01)then
          cddratio(k)=seacddtot(k)/wkcooldd(k)
        else
          cddratio(k)=1.0
        endif
        if(k.eq.1)then
          dayratio=((is1winf-is1wins)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'early winter ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'early winter ratio of season/assessment heat DD: ',
     &      hddratio(k),' & cool DD:',cddratio(k)
          call edisp(iuout,outs)
        elseif(k.eq.2)then
          dayratio=((is1sprf-is1sprs)+1)/((ijulfn-ijulst)+1)
C Debug...
          write(outs,'(a,F6.3)') 
     &      'spring ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'spring ratio of season/assessment heat DD: ',
     &      hddratio(k),' & cool DD:',cddratio(k)
          call edisp(iuout,outs)
        elseif(k.eq.3)then
          dayratio=((is1sumf-is1sums)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'summer ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'summer ratio of season/assessment heat DD: ',
     &      hddratio(k),' & cool DD:',cddratio(k)
          call edisp(iuout,outs)
        elseif(k.eq.4)then
          dayratio=((is2sprf-is2sprs)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'autumn ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'autumn ratio of season/assessment heat DD: ',
     &      hddratio(k),' & cool DD:',cddratio(k)
          call edisp(iuout,outs)
        elseif(k.eq.5)then
          dayratio=((is2winf-is2wins)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'late winter ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'late winter ratio of season/assessment heat DD: ',
     &      hddratio(k),' & cool DD:',cddratio(k)
          call edisp(iuout,outs)
        endif

C For the current season set degree day or day ratios.
        if(hddratio(k).gt.dayratio)then
          dmheat(k)=hddratio(k)
        else
          dmheat(k)=dayratio
        endif
        if(cddratio(k).gt.dayratio)then
          dmcool(k)=cddratio(k)
        else
          dmcool(k)=dayratio
        endif
        dmlight(k)=dayratio
        dmsmlpw(k)=dayratio
        dmfan(k)=dayratio
        dmdhw(k)=dayratio
        h(1)='You might wish to record this information before going'
        h(2)='to the next season.'
        if(k.eq.5)then
          iok=1
        else
          call easkab('Options:',' ','look at next season','continue',
     &      iok,2)
        endif
        if(iok.eq.2)return
        ix=1
        call edisp(iuout,' ')
 43   continue

C If there are three assessments used in the IPV advise the user of
C alternative ratios which combine both winters and both transition
C seasons.
      if(nipvassmt.eq.3)then
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
C          hddratio(1)=1.0
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
C          cddratio(1)=1.0
        endif
        write(outs,'(a,F5.2,a,F5.2,a,F5.2,a)')'is ',dayratio,' days & ',
     &    hddratio(1),' HDD & ',cddratio(1),' CDD.'
        h(1)='There is one winter assessment to represent the first and'
        h(2)='second winter seasons. The ratios is of the winter'
        h(3)='assessment period to all winter days and/or degree days.'
        call easkab(
     &    'The ratio of the winter assessment to all winter seasons',
     &    outs,'accept','continue',
     &    iok,3)
        if(iok.eq.1)then
          dmheat(1)=hddratio(1)
          dmcool(1)=cddratio(1)
          dmlight(1)=dayratio
          dmsmlpw(1)=dayratio
          dmfan(1)=dayratio
          dmdhw(1)=dayratio
        endif
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
C          hddratio(2)=1.0
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
C          cddratio(2)=1.0
        endif
        write(outs,'(a,F5.2,a,F5.2,a,F5.2,a)')'is ',dayratio,' days & ',
     &    hddratio(2),' HDD & ',cddratio(2),' CDD.'
        h(1)='There is one transition assessment to represent spring'
        h(2)='and autumn seasons. The ratios is of the transition'
        h(3)='assessment period to all transition days and/or DD.'
        call easkab(
     &    'The ratio of the trans assessment to all trans seasons',
     &    outs,'accept','continue',
     &    iok,2)
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

C ---------- CLMGET (duplicate of code in esruclm/cfiles.F
C Recover one day's data from climate file into common block for use.
      SUBROUTINE CLMGET(IDAY)
      PARAMETER (MT=24)
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     A              CDIR(MT),CHUM(MT)
      COMMON/CLMVAR/VNAME(6)
      CHARACTER*32 VNAME
      DIMENSION ICLM(24,6)
      character outs*124

      VNAME(1)='Diffuse horiz.rad. W/m^2'
      VNAME(2)='Dry bulb temp.     deg.C'
      VNAME(3)='Direct normal rad. W/m^2'
      VNAME(4)='Wind velocity        m/s'
      VNAME(5)='Wind direction      deg.'
      VNAME(6)='Relative humidity    pct'

      IF(IDAY.EQ.NDAY)GO TO 50
      IREC=IDAY
      READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)((ICLM(J,K),K=1,6),
     &J=1,24)
      DO 40 J=1,24
        CDIF(J)=real(ICLM(J,1))
        CTMP(J)=0.1* real(ICLM(J,2))
        CDNR(J)=real(ICLM(J,3))
        CVEL(J)=0.1* real(ICLM(J,4))
        CDIR(J)=real(ICLM(J,5))
        CHUM(J)=real(ICLM(J,6))
  40  CONTINUE
      NDAY=IDAY

  50  CONTINUE
      RETURN

C Error messages.
1000  write(outs,'(A,I4)')' Problem reading record ',IREC
      call edisp(iuout,outs)
      call edisp(iuout,' substituting zeros for data.')
      DO 10 J=1,24
        CDIF(J)=0.0
        CTMP(J)=0.0
        CDNR(J)=0.0
        CVEL(J)=0.0
        CDIR(J)=0.0
        CHUM(J)=0.0
  10  CONTINUE

      RETURN
      END

C ************ AZALT (duplicate of code in esruclm/clmint.F)
C 'AZALT' CALCULATES THE SOLAR AZIMUTH AND THE SOLAR ALTITUDE
C ANGLES AT EACH HOUR OF THE DAY , FOR ANY SITE.
      SUBROUTINE AZALT(IYD,ITS,RLONGT,RLATIT,SOLALT,SOLAZI)
      COMMON/UAZALT/INIT,GLAT,GLON
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      DIMENSION SOLALT(24),SOLAZI(24)
      logical OK,DOK,close
      character h*72

      PI = 4.0 * ATAN(1.0)
      INIT = 0
      GLAT = 55.9
      GLON = -4.15

C  IYD = DAY NUMBER OF THE YEAR (JAN 1=1, JAN 2=2,..., DEC 31=365)
C  ITS = TIME SYSTEM UNDER ANALYSIS, I.e. IF LOCAL MEAN
C        TIME (LMT), INPUT 1, IF LOCAL APPARENT TIME ,(LAT) ,INPUT 2.
C  RLONGT = LONGITUDINAL DIFFERENCE OF THE SITE MEASURED EAST(+VE)
C           OR WEST(-VE) OF THE REFERENCE TIME ZONE. IN BRITAIN THE
C           REFERENCE TIME ZONE IS AT GREENWICH (0 DEG LONGITUDE). (DEG)
C  RLATIT = LATITUDE OF SITE (NORTH +VE, SOUTH -VE) (DEGREES)

C  CHECK LATITUDE AND LONGITUDE.
      IF(INIT.EQ.1)GOTO 18
      IF(RLATIT.GT.90.0.OR.RLATIT.LT.-90.0)goto 14
      IF(RLONGT.GT.15.0.OR.RLONGT.LT.-15.0)goto 14
 
C  THE SIN AND COS OF THE LATITUDE ARE EVALUATED.
   15 SDLAT=ESIND(RLATIT)
      CDLAT=ECOSD(RLATIT)

C  CALCULATION OF THE EQUATION OF TIME.
      IF(ITS.EQ.2)goto 11
      A=1.978*IYD-160.22
      B=0.989*IYD-80.11
      EQTIME=0.1645*ESIND(A)-0.1255*ECOSD(B)-0.025*ESIND(B)

C  CALCULATION OF THE DECLINATION.
   11 A=280.1+0.9863*IYD
      DECLIN=23.45*ESIND(A)
      SDDECL=ESIND(DECLIN)
      CDDECL=ECOSD(DECLIN)

C  IF LMT IS USED THEN THE LONGITUDE AND EQUATION OF TIME
C  WILL EFFECT THE SOLAR TIME AT THE USERS SITE. A
C  CORRECTION WILL THEREFORE BE REQUIRED.
      DO 10 IHR=1,24
      IF(ITS.EQ.2)GOTO 1
      TIME=IHR+(EQTIME+(RLONGT/15.0))
      GOTO 2
    1 TIME=IHR
    2 TIMCOE=15.0*(12.0-TIME)
      CDTIME=ECOSD(TIMCOE)
      ABSTMC=ABS(TIMCOE)
C      SDTIME=ESIND(TIMCOE)
      ASDTIM=ESIND(ABSTMC)

C  CALCULATION OF THE SOLAR ALTITUDE.
      ALT=SDLAT*SDDECL+CDLAT*CDDECL*CDTIME
      SOLALT(IHR)=ASIN(ALT)*180.0/PI

C  SOLAR AZIMUTH.
      AZMUTH=(CDDECL*ASDTIM)/ECOSD(SOLALT(IHR))
      IF(AZMUTH.LT.-1.0)AZMUTH=-1.0
      IF(AZMUTH.GT.1.0)AZMUTH=1.0
      SOLAZI(IHR)=ASIN(AZMUTH)*180.0/PI

C  Correct the azimuthal angle for time of day and whether
C  in north or south hemispheres.
      X=CDTIME
      call eclose(RLATIT,0.0,0.001,close)
      IF(close)goto 13
      call eclose(RLATIT,90.0,0.001,close)
      IF(close)goto 8
      Y=(CDLAT/SDLAT)*(SDDECL/CDDECL)
      goto 9
    8 Y=0.0
      goto 9
   13 Y=100.0*(SDDECL/CDDECL)
    9 IF(Y-X)3,4,5
    3 IF(RLATIT.GE.0.0)goto 6
      goto 7
    5 IF(RLATIT.LT.0.0)goto 6
      goto 7
    4 IF(TIME.LE.12.0)SOLAZI(IHR)=90.0
      IF(TIME.GT.12.0)SOLAZI(IHR)=270.0
      goto 10
    6 IF(TIME.LE.12.0)SOLAZI(IHR)=180.0-SOLAZI(IHR)
      IF(TIME.GT.12.0)SOLAZI(IHR)=180.0+SOLAZI(IHR)
      goto 10
    7 IF(TIME.GT.12.0)SOLAZI(IHR)=360.0-SOLAZI(IHR)
   10 CONTINUE
      RETURN

   14 dok=.true.
      h(1)=' Lat/Lon error in AZALT :'
      h(2)=' default site Glasgow (55.9N, 4.15W)'
      CALL ASKOK('Error in Latitude or longitude',
     &  'Ok to respecify? (no will abort ESP-r)',OK,dok,2)
      if(.NOT.OK)STOP
   18 RLATIT=GLAT
      RLONGT=GLON
      INIT=1
      goto 15
      END
