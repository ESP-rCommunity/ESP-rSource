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

C This file contains the following routines:
C      MOSTAT
C      MOSREG
C      MOSINF
C      MOSETS
C      MORPER
C      MOSGET
C      MOREGR
C      MORSTU
C      MOSMSD

C ******************** MOSTAT ********************

C MOSTAT allows statistical analysis of the simulation
C results. Available at present:

      SUBROUTINE MOSTAT
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/DEFLT/IDEFLT

      COMMON/GETPIK/NGET,IGETNO(MZS,9)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      common/getmenu/menutype,igetind(65),igetflux(65)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/recver/izver,ipver,iever
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      common/zcondhr/izctime(MCON)

C SNAPSH is a logical flag to request variation in header.
      COMMON/SNAP/SNAPSH
      logical SNAPSH,close
      DIMENSION ATI(8),RTI(8),TATI(8),TRTI(8),NDYMTH(12)
      dimension ITEM(24)
      CHARACTER ITEM*23,tg*1,delim*1,xfile*144
      CHARACTER PDESCR*60,SDESCR*44
      character prompt*124,outs*124
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER RSNAME*40
      CHARACTER*3 NMTHNM(12)
      integer IWM,ihd  ! for radio buttons
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MOSTAT'  ! set for subroutine

      DATA NDYMTH/31,59,90,120,151,181,212,243,273,304,334,365/
      DATA NMTHNM/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &'Aug','Sep','Oct','Nov','Dec'/

C Setup for menu.
 1    ITEM(1) ='2 select result set   '
      ITEM(2) ='3 define output period'
      ITEM(3) ='4 select zones        '
      ITEM(4) ='  ------------------- '
      ITEM(5) ='a summary statistics  '
      ITEM(6) ='b frequency table     '
      ITEM(7) ='  ------------------- '
      ITEM(8) ='c hours above a value '
      ITEM(9) ='d hours below a value '
      ITEM(10)='  ------------------- '

C If implementing checking against CEN statistics then remove item 10 from 
C list of illegal choices.
C      ITEM(10)='e CEN statistics     '
      ITEM(11)='f energy delivered    '
      ITEM(12)='g casual gains distrib'
      ITEM(13)='h zone energy balance '
      ITEM(14)='i surface energy balnc'
      ITEM(15)='j surface condensation'
      ITEM(16)='k intrstl condensation'
      ITEM(17)='  ------------------- '
      ITEM(18)='l monthy gains/losses '
      ITEM(19)='m monthy temp. stats '
      ITEM(20)='  ------------------- '
      if(ixopen.eq.1)then
        ITEM(21)='> output >> file    '
      elseif(ixopen.eq.0)then
        ITEM(21)='> output >> screen  '
      endif
      if(delim.eq.'-')then
        ITEM(22)='^ delim >> normal      '
      elseif(delim.eq.'T')then
        ITEM(22)='^ delim >> tab         '
      elseif(delim.eq.'C')then
        ITEM(22)='^ delim >> comma       '
      elseif(delim.eq.'S')then
        ITEM(22)='^ delim >> space       '
      elseif(delim.eq.'X')then
        ITEM(22)='^ delim >> tagged      '
      endif
      ITEM(23)='? help               '
      ITEM(24)='- exit               '

 2    NITMS=24
      INO=-3

C Instanciate help strings for the menu.
      helptopic='res_statistics_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Enquire about',ITEM,NITMS,INO)

      IF(INO.EQ.1)then
        CALL MORESS
        IDEFLT=0
      elseif(INO.EQ.2)then
        CALL MOOPER
      elseif(INO.EQ.3)then
        CALL MOZDFN
      elseif(INO.EQ.5)then

C Summary statistics.
        CALL MOSMSD('-',VMX,VMN,VAVE,VTOT,DIVMX,DIVMN,DISTMX,DISTMN)
      elseif(INO.EQ.6) then

C Frequency histogram tableau.
        ivvv=0
        CALL MODGRF('t',ivvv)
      elseif(INO.EQ.8) then

C Hours above given value.
        CALL MOSQHR('-',1,0.0)
      elseif(INO.EQ.9) then

C Hours below given value.
        CALL MOSQHR('-',-1,0.0)
      elseif(INO.EQ.10)then

C Comparison to CEN statistics (if implemented)
        CALL EASKI(IANS,' ',' Password: ',
     &           0,'-',0,'-',0,'Password',IER,0)
        IF(IANS.NE.101)GOTO 2

C Call the menu of choices (this also sets some default options).
C The logical variable totals controls the displaying of cumulative 
C data for all the zones chosen.
        NGET=9
        do 11 I=1,9
          IGETNO(I,1)=6
          IGETNO(I,2)=I
          IGETNO(I,3)=0
          IGETNO(I,4)=0
          IGETNO(I,5)=ISIM
          SLABEL(I)='Resultant temperature (degC)'
          LNSLABEL(I)=28
 11     continue
        CALL MOSMSDCEN('s')
      elseif(INO.EQ.11)then
        call MOEDMDS('-',the,tzhtime,tce,tzctime,tlhe,tzlhtime,
     &    tlce,tzlctime)
      elseif(INO.EQ.12)then
        call CASDIST('-')
      elseif(INO.EQ.13)then

C Setup for zone balance. Confirm if snapshot or integrated report.
C If output to file alter the edisp unit number.
        call EASKABC('Energy balance options: ',' ',
     &    'snapshot','integrated over time','cancel',isnapsh,nbhelp)
        if(isnapsh.eq.1)then
          SNAPSH=.TRUE.
          IMO=IOM1
          IDO=IOD1
          CALL ASKRTIM(IFDAY,IMO,IDO,IDAY,TIME,IT,IER)
          JIOD1=IOD1
          JIOM1=IOM1
          JIOH1=IOH1
          JIOD2=IOD2
          JIOM2=IOM2
          JIOH2=IOH2
          JIODS=IODS
          JIODF=IODF
          IODS=IDAY
          IODF=IDAY
          IOH1=IT
          IOH2=IT
        elseif(isnapsh.eq.2)then
          SNAPSH=.FALSE.
        elseif(isnapsh.eq.3)then
          goto 1
        endif

C Check what to focus on and if a snapshot display.
C Snapshot done via using the time requested
C to temporarily overwrite the IOH1 IOH2 and the
C standard energy balance.  After finished then reset
C the period to previous settings.
        helptopic='res_air_point_forms'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call EASKABC('Zone energy balance: ','(grouped by)',
     &    'plant status','gain/loss','cancel',IWHICH,nbhelp)
        if(IWHICH.eq.3)goto 1

        itru = icout
        prompt = ' '
        if(ixopen.eq.1)then
          itru = ixunit
        endif

C Call the menu of choices (this also sets some default options).
        MENUTYPE=9
        call GOMSETUP
        call GOMENU
        if(ixopen.eq.1)then
          itru = ixunit
          if(NGET.ge.1)then
            write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
          else
            write(prompt,'(a)')' Output being directed to file. ' 
          endif
          call usrmsg(prompt,'Scanning for range of values...','-')
        else
          write(prompt,'(3a)') 'Scanning for ',
     &      SLABEL(1)(1:LNSLABEL(1)),'...'
          itru = icout
          call usrmsg(prompt,'  ','-')
        endif

        ISET=ISIM
        CALL HDDATE(PDESCR)
        CALL HDSTEP(SDESCR)
        lsn1=MIN0(lnblnk(RFILE),22)
        if(NSIM.gt.1)then
        write(outs,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        else
          write(outs,'(4A)')'Lib: ',RFILE(1:lsn1),
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        endif
        call edisp(itru,outs)
        write(outs,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &   SDESCR(11:lnblnk(SDESCR))
        call edisp(itru,outs)

C For each selected zone generate energy balance report.
        DO 65 JZ=1,NZ
          izone=NZNO(JZ)
          if(ISAVE.EQ.4)then
            CALL MOZCAS(IZONE,IWHICH,NIT)
          elseif(ISAVE.eq.2.or.ISAVE.eq.3)then
            if(izver.ge.2)then
              CALL MOZCAS(izone,IWHICH,NIT)
            else
              call usrmsg(
     &          'The results library is an older format and a full',
     &          'zone energy balance cannot be recovered.','W')
              CALL MOZCAS(izone,IWHICH,NIT)
            endif
          endif
 65     continue
        if(SNAPSH)then
          call usrmsg(' switching from snapshot to integration...',
     &        ' ','P')
          call usrmsg(' ',' ','-')
          SNAPSH=.FALSE.
          IOD1=JIOD1
          IOM1=JIOM1
          IOH1=JIOH1
          IOD2=JIOD2
          IOM2=JIOM2
          IOH2=JIOH2
          IODS=JIODS
          IODF=JIODF
        endif
        if(ixopen.eq.1)then
          call usrmsg(prompt,'Scanning for range of values...done.','-')
        else
          call usrmsg('  ','  ','-')
        endif
      elseif(INO.EQ.14)then

C Surface energy balance. Confirm if snapshot or integrated report.
C If output to file alter the edisp unit number.
        if(ISAVE.ne.4)then
          call usrmsg(' The simulation save option does not',
     &                ' allow surface energy balances.','W')
          goto 1
        endif
        helptopic='res_surf_point_forms'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call EASKABC('Energy balance options: ',' ',
     &    'snapshot','integrated over time','cancel',isnapsh,nbhelp)
        if(isnapsh.eq.1)then
          SNAPSH=.TRUE.
          IMO=IOM1
          IDO=IOD1
          CALL ASKRTIM(IFDAY,IMO,IDO,IDAY,TIME,IT,IER)
          JIOD1=IOD1
          JIOM1=IOM1
          JIOH1=IOH1
          JIOD2=IOD2
          JIOM2=IOM2
          JIOH2=IOH2
          JIODS=IODS
          JIODF=IODF
          IODS=IDAY
          IODF=IDAY
          IOH1=IT
          IOH2=IT
        elseif(isnapsh.eq.2)then
          SNAPSH=.FALSE.
        elseif(isnapsh.eq.3)then
          goto 1
        endif

        helptopic='res_surf_point_forms'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call EASKABC('Surface energy balance: ','(grouped by)',
     &    'plant status','gain/ loss','cancel',IWHICH,nbhelp)
        if(IWHICH.eq.3)goto 1
        IWHICH=IWHICH+3
        itru = icout
        prompt = ' '
        if(ixopen.eq.1)then
          itru = ixunit
        endif

C Call the menu of choices (this also sets some default options).
        MENUTYPE=12
        call GOMSETUP
        call GOMENU
        if(ixopen.eq.1)then
          itru = ixunit
          if(NGET.ge.1)then
            write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
          else
            write(prompt,'(a)')' Output being directed to file. ' 
          endif
          call usrmsg(prompt,'Scanning for range of values...','-')
        else
          write(prompt,'(3a)') 'Scanning for ',
     &      SLABEL(1)(1:LNSLABEL(1)),'...'
          itru = icout
          call usrmsg(prompt,'  ','-')
        endif

        ISET=ISIM
        CALL HDDATE(PDESCR)
        CALL HDSTEP(SDESCR)
        lsn1=MIN0(lnblnk(RFILE),22)
        if(NSIM.gt.1)then
        write(outs,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        else
          write(outs,'(4A)')'Lib: ',RFILE(1:lsn1),
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        endif
        call edisp(itru,outs)
        write(outs,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &   SDESCR(11:lnblnk(SDESCR))
        call edisp(itru,outs)

C For each selected zone and surface generate energy balance report.
        DO 165 KGET=1,NGET
          izone=IGETNO(KGET,2)
          isfn =IGETNO(KGET,3)
          CALL MOSCAS(izone,isfn,IWHICH)
 165    continue
        if(SNAPSH)then
          call usrmsg(' switching from snapshot to integration...',
     &        ' ','P')
          call usrmsg(' ',' ','-')
          SNAPSH=.FALSE.
          IOD1=JIOD1
          IOM1=JIOM1
          IOH1=JIOH1
          IOD2=JIOD2
          IOM2=JIOM2
          IOH2=JIOH2
          IODS=JIODS
          IODF=JIODF
        endif
        if(ixopen.eq.1)then
          call usrmsg(prompt,'Scanning for range of values...done.','-')
        else
          call usrmsg('  ','  ','-')
        endif
      elseif(INO.EQ.15)then

C Surface condensation. Setup GOGET variables and advise user while
C scanning data. Call MOSURC for each selected zone in turn.
        itru = icout
        prompt = ' '
        if(ixopen.eq.1)then
          itru = ixunit
        endif
        MENUTYPE=14
        call GOMSETUP
        call GOMENU
        if(ixopen.eq.1)then
          itru = ixunit
          if(NGET.ge.1)then
            write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
          else
            write(prompt,'(a)')' Output being directed to file. ' 
          endif
          call usrmsg(prompt,'Scanning for range of values...','-')
        else
          write(prompt,'(3a)') 'Scanning for ',
     &      SLABEL(1)(1:LNSLABEL(1)),'...'
          itru = icout
          call usrmsg(prompt,'  ','-')
        endif
        ISET=ISIM
        CALL HDDATE(PDESCR)
        CALL HDSTEP(SDESCR)
        lsn1=MIN0(lnblnk(RFILE),22)
        if(NSIM.gt.1)then
        write(outs,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        else
          write(outs,'(4A)')'Lib: ',RFILE(1:lsn1),
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        endif
        call edisp(itru,outs)
        write(outs,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &   SDESCR(11:lnblnk(SDESCR))
        call edisp(itru,outs)

C For each selected zone and surface condensation report.
        write(outs,'(2a)')
     &  ' Total occurances = timesteps with condensation',
     &  ' on at least one surface.'
        call edisp(itru,outs)
        DO 166 KGET=1,NGET
          izone=IGETNO(KGET,2)
          izrh =IGETNO(KGET,3)
          iverb =IGETNO(KGET,4)
          izctime(izone)=0
          CALL MOSURC(izone,izrh,iverb)
 166    continue
        if(ixopen.eq.1)then
          call usrmsg(prompt,'Scanning for range of values...done.','-')
        else
          call usrmsg('  ','  ','-')
        endif
      elseif(INO.EQ.16)then

C Interstitial condensation.
        if(ISAVE.eq.3)then
          CALL ASKZON(ICOMP)
          CALL FINDCND(ICOMP)
        else
          call usrmsg(' The simulation save option does not allow',
     &                ' intersitital condensation checks.','W')
          goto 1
        endif
      elseif(INO.EQ.18)then

C Monthly zone gains and losses report.
        CALL MGLTBL
      elseif(INO.EQ.19)then

C Monthly db temperature and resultant temperature stats.
        itru = icout
        prompt = ' '
        if(ixopen.eq.1)then
          itru = ixunit
        endif
        write(outs,'(a,8x,a,8x,a)')' Zone','Internal air temperature',
     &    'Internal resultant temp.'
        call edisp(itru,outs)
        write(outs,'(4x,2a)') 'Period  Max.   at   Min.   at   Mean ',
     &    ' Max.   at   Min.   at   Mean'
        call edisp(itru,outs)
        call edisp(itru,' ')

C Per zone for each day in period.
        DO 860 I=1,NZ
          IZN=NZNO(I)
          IMNTH=IOM1
          ATI(8)=0.0
          INITL=0
          ISET=ISIM

C Get statistics for the day.
          DO 850 J=IODS,IODF
            JD=J
            CALL CHKTIME(JD,ISTART,IEND)

C Get zone db temp into GVAL.
            CALL GZAIRT(JD,IZN,ISET)

C Get zone resultant temp into XDUM1.
            CALL CZRESL(JD,IZN,ISET)

            DO 830 K=ISTART,IEND
              KT=K
              call eclose(ATI(8),0.0,0.001,close)
              IF(close)goto 828
              IF(GVAL(KT).LE.ATI(1))GOTO 821
              ATI(1)=GVAL(KT)
              ATI(2)=JD
              ATI(3)=KT
              goto 822
  821         IF(GVAL(KT).GE.ATI(4))GOTO 822
              ATI(4)=GVAL(KT)
              ATI(5)=JD
              ATI(6)=KT
  822         ATI(7)=ATI(7)+GVAL(KT)
              ATI(8)=ATI(8)+1.0
              IF(XDUM1(KT).LE.RTI(1))GOTO 826
              RTI(1)=XDUM1(KT)
              RTI(2)=JD
              RTI(3)=KT
              goto 827
  826         IF(XDUM1(KT).GE.RTI(4))GOTO 827
              RTI(4)=XDUM1(KT)
              RTI(5)=JD
              RTI(6)=KT
  827         RTI(7)=RTI(7)+XDUM1(KT)
              RTI(8)=RTI(8)+1.0
             goto  830
  828         RTI(1)=XDUM1(KT)
              RTI(2)=JD
              RTI(3)=KT
              RTI(4)=XDUM1(KT)
              RTI(5)=JD
              RTI(6)=KT
              RTI(7)=XDUM1(KT)
              RTI(8)=1.0
              ATI(1)=GVAL(KT)
              ATI(2)=JD
              ATI(3)=KT
              ATI(4)=GVAL(KT)
              ATI(5)=JD
              ATI(6)=KT
              ATI(7)=GVAL(KT)
              ATI(8)=1.0
  830       CONTINUE

C Check if month finished.
            IF(JD.LT.NDYMTH(IMNTH).AND.JD.NE.IODF)goto 850

C Dump this month's statistics.
            ND1=INT(ATI(2))
            IF (IMNTH.NE.1) ND1=ND1-NDYMTH(IMNTH-1)
            NT1=INT(ATI(3))/NTS
            ND2=INT(ATI(5))
            IF (IMNTH.NE.1) ND2=ND2-NDYMTH(IMNTH-1)
            NT2=INT(ATI(6))/NTS
            ATI7=ATI(7)/ATI(8)
            ND3=INT(RTI(2))
            IF (IMNTH.NE.1) ND3=ND3-NDYMTH(IMNTH-1)
            NT3=INT(RTI(3))/NTS
            ND4=INT(RTI(5))
            IF (IMNTH.NE.1) ND4=ND4-NDYMTH(IMNTH-1)
            NT4=INT(RTI(6))/NTS
            RTI7=RTI(7)/RTI(8)
            IF(IMNTH.EQ.IOM1)THEN
              WRITE(outs,904)IZN,NMTHNM(IMNTH),
     &          ATI(1),ND1,NT1,ATI(4),ND2,NT2,ATI7,
     &          RTI(1),ND3,NT3,RTI(4),ND4,NT4,RTI7
  904         FORMAT(I4,A5,' ',2(2(F6.2,I3,':',I2),F6.2))
              call eddisp(itru,outs)
            ELSE
              WRITE(outs,903)NMTHNM(IMNTH),
     &          ATI(1),ND1,NT1,ATI(4),ND2,NT2,ATI7,
     &          RTI(1),ND3,NT3,RTI(4),ND4,NT4,RTI7
  903         FORMAT(4X,A5,' ',2(2(F6.2,I3,':',I2),F6.2))
              call eddisp(itru,outs)
            ENDIF

            IF(INITL.GE.1)goto 841
            DO 840 IJK=1,8
              TATI(IJK)=ATI(IJK)
              TRTI(IJK)=RTI(IJK)
  840       CONTINUE
            goto 849
  841       IF(ATI(1).LE.TATI(1))goto 842
            TATI(1)=ATI(1)
            TATI(2)=ATI(2)
            TATI(3)=ATI(3)
            goto 843
  842       IF(ATI(4).GE.TATI(4))goto 843
            TATI(4)=ATI(4)
            TATI(5)=ATI(5)
            TATI(6)=ATI(6)
  843       TATI(7)=TATI(7)+ATI(7)
            TATI(8)=TATI(8)+ATI(8)
            IF(RTI(1).LE.TRTI(1))goto 844
            TRTI(1)=RTI(1)
            TRTI(2)=RTI(2)
            TRTI(3)=RTI(3)
            goto 845
  844       IF(RTI(4).GE.TRTI(4))goto 845
            TRTI(4)=RTI(4)
            TRTI(5)=RTI(5)
            TRTI(6)=RTI(6)
  845       TRTI(7)=TRTI(7)+RTI(7)
            TRTI(8)=TRTI(8)+RTI(8)
  849       ATI(8)=0.0
            RTI(8)=0.0
            IMNTH=IMNTH+1
            INITL=INITL+1
  850     CONTINUE

C Dump annual statistics.
          TATI7=TATI(7)/TATI(8)
          IYD=INT(TATI(2))
          CALL EDAYR(IYD,ND1,NM1)
          IYD=INT(TATI(5))
          CALL EDAYR(IYD,ND2,NM2)
          IYD=INT(TRTI(2))
          CALL EDAYR(IYD,ND3,NM3)
          IYD=INT(TRTI(5))
          CALL EDAYR(IYD,ND4,NM4)
          TRTI7=TRTI(7)/TRTI(8)
          write(outs,9015)TATI(1),ND1,NM1,TATI(4),ND2,NM2,TATI7,
     &      TRTI(1),ND3,NM3,TRTI(4),ND4,NM4,TRTI7
 9015     format(5X,'Ann ',' ',2(2(F6.2,I3,':',I2),F6.2))
          call eddisp(itru,outs)
          call edisp(itru,' ')
  860   CONTINUE
      elseif(INO.EQ.(NITMS-3)) then
        call ctlexp(xfile,ixopen,ixunit,ixpunit,'X','Tabular',IER)
      elseif(INO.EQ.(NITMS-2)) then

C Toggle delimeter.
        helptopic='res_stats_delimiter'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IWM=1
        CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &    'normal spaces','single space','tab','comma','tagged',
     &    'continue',' ',IWM,nbhelp)
        if(iwm.eq.1)then
          delim = '-'
        elseif(iwm.eq.2)then
          delim = 'S'
        elseif(iwm.eq.3)then
          delim = 'T'
        elseif(iwm.eq.4)then
          delim = 'C'
        elseif(iwm.eq.5)then
          delim = 'X'
        endif
      elseif(INO.EQ.(NITMS-1)) then

C Display help messages.

C Menu help text.
        helptopic='res_help_choices'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ihd=1
        call easkatog(' ','Help options:','overview','summary stats',
     &   'frequency table','energy balances','detailed stats',
     &   'monthly gain loss','cancel',ihd,nbhelp)
        if(ihd.eq.1)then
          helptopic='res_statistics_menu'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Enquire about',nbhelp,'-',0,0,IER)
        elseif(ihd.eq.2)then
          helptopic='res_help_sum_stats'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Enquire summary stats',nbhelp,'-',0,0,IER)

        elseif(ihd.eq.3)then
          helptopic='res_help_freq_table'
          call gethelptext(helpinsub,helptopic,nbhelp)
         CALL PHELPD('Enquire frequency',nbhelp,'-',0,0,IER)

        elseif(ihd.eq.4)then
          helptopic='res_help_zone_bal'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Enquire energy bal',nbhelp,'-',0,0,IER)

        elseif(ihd.eq.5)then

          helptopic='res_help_specific_topic'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Enquire detailed',nbhelp,'-',0,0,IER)

        elseif(ihd.eq.6)then

          helptopic='res_help_monthly_gl'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Enquire montly',nbhelp,'-',0,0,IER)
        elseif(ihd.eq.7)then
          continue
        endif

      elseif(INO.EQ.NITMS) then
        RETURN
      else
        goto 2
      endif
      goto 1
      END

C ******************** MOSMSD ********************
C MOSMSD uses the generic menu and get routines to retrieve max, min and
C other basic statistical data. 
C Returns the following on the command line (used for IPV reporting).
C VMX,VMN,VAVE The max, min and average value for the selected metric for 
C   all active zones (as defined in IGETNO).
C DIVMX,DIVMN The diversified maximum and minimum values.
C DISTMX,DISTMN The distributed maximum and minimum values.
C act='i' called from ipv: sets bin width uses current IGETS
C act='p' iget parameters already set.

      SUBROUTINE MOSMSD(act,VMX,VMN,VAVE,VTOT,DIVMX,DIVMN,DISTMX,DISTMN)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM

      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      integer ireportunit ! zero is default one is W two is kW three Joules
      common/repunit/ireportunit

      dimension YMAX(MZS), YMIN(MZS), YTOTSD(MZS)
      dimension YAVE(MZS), YSTD(MZS), NY(MZS), YTOT(MZS)
      dimension TMAX(MZS), TMIN(MZS)
      character xfile*144,tg*1,delim*1
      character outs*124,act*1
      character prompt*124,prompte*124
      CHARACTER DESCR*7,DESCR1*10,DESCRH*5,DESCRD*5,NT*14,XT*14
      CHARACTER RSNAME*40
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER PDESCR*60,SDESCR*44,t12*12

      logical LARGE,HAVEIT

C If output to file alter the edisp unit number. Skip if in IPV mode.
      prompt = '  '
      if (act(1:1).ne.'i') then
        itru = icout
        if(ixopen.eq.1)then
          itru = ixunit
        endif
      endif

C Set all returned data to zero.
 1    VMX=0.
      VMN=0.
      VAVE=0.
      VTOT=0.
      DIVMX=0.
      DIVMN=0.
      DISTMX=0.
      DISTMN=0.

C Call the menu of choices (this also sets some default options).
C The logical variable totals controls the displaying of cumulative 
C data for all the zones chosen.  
      if(act(1:1).eq.'-')then
        MENUTYPE=5
C        write(6,*) 'in stats ireportunit is ',ireportunit
        ireportunit=0  ! assume normal for stats
        call GOMSETUP
        call GOMENU
        if(ixopen.eq.1)then
          if(NGET.ge.1)then
            write(prompt,'(3a)') 'Scanning ',
     &        SLABEL(1)(1:LNSLABEL(1)),' >> file...'
          else
            write(prompt,'(a)') ' Scanning data >> file...' 
          endif
          itru = ixunit
        else
          if(NGET.ge.1)then
            write(prompt,'(3a)') 'Scanning ',
     &        SLABEL(1)(1:LNSLABEL(1)),'...'
          else
            write(prompt,'(a)') 'Scanning...' 
          endif
          itru = icout
        endif
        if (MENUTYPE.eq.-1) return
      else
        if(NGET.ge.1)then
          write(prompt,'(3a)') 'Scanning ',
     &      SLABEL(1)(1:LNSLABEL(1)),'...'
        else
          write(prompt,'(a)') 'Scanning...' 
        endif
      endif
      write(prompte,'(2a)') prompt(1:lnblnk(prompt)),'done.'

C Setup parameters and call GOGET for each output day to get required data.
C GOGET recovers the data in VAL2 (and averages output if required.)
C Variables begining X are for all zones selected whereas variables 
C starting Y are zone based.
      do 5 I=1,MZS
        YTOTSD(I)=0.0
        YSTD(I)=0.0
        NY(I)=0
        YTOT(I)=0.0
 5    continue
      XMAX=-1.E+10
      XMIN=1.E+10
      XTOT=0.0
      XAVE=0.0
      NX=0
      call usrmsg(prompt,' ','-')

C First recover bounds of selected data and calculate distributed max, min.
C If we can recover it efficiently use GETSTATS otherwise use MMACAL.
      do 40 IX=1,NGET
        call GETSTATS(IX,HAVEIT,DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN)
        if(HAVEIT)then

C Debug.
C          write(6,*) 'get stats ',DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN
        else
          call MMACAL(IX,DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN)

C Debug.
C          write(6,*) 'get MMACAL ',DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN
        endif
        YMAX(IX)=DMAX
        YMIN(IX)=DMIN
        YAVE(IX)=DAVE
        TMAX(IX)=TXMAX
        TMIN(IX)=TXMIN
        DISTMX=DISTMX+DMAX
        DISTMN=DISTMN+DMIN
 40   continue

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS
      do 10 IDAY=IODS,IODF
        call GOGET(IDAY)

C Add values in VAL2 to there correct bins. Loop through selected zones.
        do 400 IX=1,NGET

          IZONE=IGETNO(IX,2)
          DO 421 J = 1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
            IHRD=(IDAY-IODS)*24
            call DATIME(J,ATIME)
            TIME=float(IHRD)+ATIME

C Within requested output period.
            IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
            ih=int(ATIME+1.)
            ioc=1
            if(iocupf.eq.1) call getocup(IZONE,IDAY,ih,ioc,ier)
            if(ioc.ne.0) then
              YTOTSD(IX)=YTOTSD(IX)+((YAVE(IX)-VAL2(IX,J))**2)
              NY(IX)=NY(IX)+1
              YTOT(IX)=YTOT(IX)+VAL2(IX,J)

C Diversified values. Do this only once.
C Sum all NGET values for current timestep.
              if (IX.eq.1) then
                XMAXC=0.0
                XMINC=0.0
                do 451 ISUM=1,NGET
                  IF(VAL2(ISUM,J).GT.0.)XMAXC=XMAXC+VAL2(ISUM,J)
                  IF(VAL2(ISUM,J).LT.0.)XMINC=XMINC+VAL2(ISUM,J)
 451            continue
                if (XMAXC.gt.DIVMX) then
                  DIVMX=XMAXC
                  DIVMXT=TIME
                  if (DIVMX.gt.1000.) LARGE=.TRUE.
                endif
                if (XMINC.lt.DIVMN) then
                  DIVMN=XMINC
                  DIVMNT=TIME
                  if (DIVMN.lt.-1000.) LARGE=.TRUE.
                endif
              endif

C Find largest value for current timestep and zone.
              XMAX=MAX(XMAX,VAL2(IX,J))
              XMIN=MIN(XMIN,VAL2(IX,J))
              XTOT=XTOT+VAL2(IX,J)
              NX=NX+1
            endif
 421      CONTINUE
 400    CONTINUE
 10   continue

C Calculate standard deviation and averages.
      do 25 I=1,NGET
        if (NY(I).lt.2) then
          YSTD(I)=0.
        else
          YSTD(I)=sqrt(YTOTSD(I)/float(NY(I)-1))
        endif
 25   continue
      if (NX.ne.0) XAVE=XTOT/float(NX)

      call usrmsg(prompte,'  ','P')
      if(MMOD.eq.8)call usrmsg('  ','  ','-')

C Set IPV data and return.
      if(act(1:1).eq.'i') then
        VMX=XMAX
        VMN=XMIN
        VAVE=XAVE
        VTOT=XTOT
        return
      endif

C -- Now display the recovered data. --
C At each step here need to check if default space delimiter has been 
C changed.
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)
      lsn1=MIN0(lnblnk(RFILE),22)
      
C Write tag line if tagged output requested otherwise write a line
C separator between reports.
      if (delim.eq.'X') then
        write(outs,'(a)')'*stats'
        call edisp(itru,outs)
      else
        call edisp(itru,' ')
      endif
      
C If writing to file prefix text with a '#' mark.
      if(ixopen.eq.1)then
        if(NSIM.gt.1)then
          WRITE(outs,'(3A,I4,2A)')'# Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &     ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        else
          WRITE(outs,'(4A)')'# Lib: ',RFILE(1:lsn1),
     &     ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        endif
        call edisp(itru,outs)
        WRITE(outs,'(4A)')'# ',PDESCR(1:lnblnk(PDESCR)),' ',
     &                    SDESCR(11:lnblnk(SDESCR))
        call edisp(itru,outs)
        write (outs,'(2a)') '# ',SLABEL(NGET)
        call edisp(itru,outs)
      else
        if(NSIM.gt.1)then
          WRITE(outs,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &     ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        else
          WRITE(outs,'(4A)')'Lib: ',RFILE(1:lsn1),
     &     ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        endif
        call edisp(itru,outs)
        WRITE(outs,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &                    SDESCR(11:lnblnk(SDESCR))
        call edisp(itru,outs)
        write (outs,'(a)') SLABEL(NGET)
        call edisp(itru,outs)
        call edisp(itru,'  ')
      endif
      
C Write specific cases first, generally in numerical order of IGET.
C Climate data therefore do not print zone names.
      if (IGETNO(NGET,1).eq.2.or.IGETNO(NGET,1).eq.19.or.
     &    IGETNO(NGET,1).eq.20.or.IGETNO(NGET,1).eq.21.or.
     &    IGETNO(NGET,1).eq.22.or.IGETNO(NGET,1).eq.26) then
        if(ixopen.eq.1)then
          if (IROC.eq.-1) then
            write(outs,'(2a)')'Max_value Max_occur Min_value ',
     &                      'Min_occur Ave_value Std_dev Total'
          else
            write(outs,'(2a)')'Max_value Max_occur Min_value ',
     &                      'Min_occur Ave_value Std_dev'
          endif
          call eddisp(itru,outs)
        else
          if (IROC.eq.-1) then
            write (outs,'(2a)') '    Maximum                  Minimum',
     &                      '                Mean   Standard  Total'
          else
            write (outs,'(2a)') '    Maximum                  Minimum',
     &                      '                Mean   Standard'
          endif
          call eddisp(itru,outs)
          write (outs,'(2a)') '  value    occurrence      value',
     &            '    occurrence      value  deviation'
          call eddisp(itru,outs)
        endif

C Write tag line if tagged output requested.
        if (delim.eq.'X') then
          write(outs,'(a,5i4)')'*data ',(IGETNO(NGET,IX),IX=1,5)
          call eddisp(itru,outs)
        endif
      
C Now generate climate data output text.
        if (NX.eq.0) then
          if(ixopen.eq.1)then
            write(outs,'(a)')'# -- No data: probably due to filtering.'
          else
            write(outs,'(a)')' -- No data: probably due to filtering.'
          endif
        else

C Create string description for time of maximum occurrance.
          MAXDOY=INT(TMAX(1)/24.)+IODS
          TMAXHR=AMOD(TMAX(1),24.)
          call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
          call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
          write (XT,'(3a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
          MINDOY=INT(TMIN(1)/24.)+IODS
          TMINHR=AMOD(TMIN(1),24.)
          call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
          call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
          write (NT,'(3a)')DESCR(1:6),'@',DESCRH
          if (IROC.eq.-1) then
            write (outs,'(2(f7.1,2x,a12,4x),2(f7.2,3x),g12.6)') YMAX(1),
     &                            XT,YMIN(1),NT,YAVE(1),YSTD(1),YTOT(1)
          else
            write (outs,'(2(f7.1,2x,a12,4x),2(f7.2,3x))') YMAX(1),
     &                            XT,YMIN(1),NT,YAVE(1),YSTD(1)
          endif
        endif

C Write climate data text as required.
        call eddisp(itru,outs)
      else

C All other IGET's. Display zone based information.
C Generate and display column headings.
        if(ixopen.eq.1)then
          if (delim.eq.'X') then
            write(outs,'(a)')'*title'
            call edisp(itru,outs)
          endif
          if (IROC.eq.-1) then
            write(outs,'(2a)')'Description Max_value Max_occur ',
     &                 'Min_value Min_occur Ave_value Std_dev Total'
          else
            write(outs,'(2a)')'Description Max_value Max_occur ',
     &                 'Min_value Min_occur Ave_value Std_dev'
          endif
          call eddisp(itru,outs)
        else
          if (IROC.eq.-1) then
            write(outs,'(2a)')' Description           Maximum         ',
     & '          Minimum                 Mean         Standard   Total'
          else
            write(outs,'(2a)')' Description           Maximum         ',
     & '          Minimum                 Mean         Standard'
          endif
          call eddisp(itru,outs)
          write(outs,'(2a)')  '                     value   occurrence',
     & '        value   occurrence        value        deviation'
          call eddisp(itru,outs)
        endif
        do 30 I=1,NGET

C Write tag line if tagged output requested.
          if (delim.eq.'X') then
            write(outs,'(a,5i4)')'*data ',(IGETNO(I,IX),IX=1,5)
            call eddisp(itru,outs)
          endif
          if (NY(I).eq.0) then
            write (outs,'(a,a)') GLABEL(I)(1:LNGLABEL(I)),
     &           ' No data: probably due to filtering.'
          else

C Create string description for time of maximum occurrance.
            MAXDOY=INT(TMAX(I)/24.)+IODS
            TMAXHR=AMOD(TMAX(I),24.)
            call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
            call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
            write (XT,'(3a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
            MINDOY=INT(TMIN(I)/24.)+IODS
            TMINHR=AMOD(TMIN(I),24.)
            call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
            call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
            write (NT,'(3a)')DESCR(1:6),'@',DESCRH

C Build up string for displaying.
            write (outs,'(a)') GLABEL(I)(1:20)
            
C Write out results to 5 significant figures starting at col 22.
            NSIG=5
            call sigfig(YMAX(I),NSIG,RNO,t12,LSTR)
            write (outs(22:47),'(a,1x,a,1x)') t12(1:LSTR),XT
            call sigfig(YMIN(I),NSIG,RNO,t12,LSTR)
            write (outs(48:73),'(a,1x,a,1x)') t12(1:LSTR),NT
            call sigfig(YAVE(I),NSIG,RNO,t12,LSTR)
            write (outs(74:86),'(a,1x)') t12
            call sigfig(YSTD(I),NSIG,RNO,t12,LSTR)
            write (outs(87:99),'(a,1x)') t12
            if (IROC.eq.-1) then
              call sigfig(YTOT(I),NSIG,RNO,t12,LSTR)
              write (outs(100:112),'(a)') t12
            endif
          endif
          call eddisp(itru,outs)
 30     continue

C Generate 'all zones' text.  Include times for maximum and minimum 
C heating and cooling.
        if (delim.eq.'X') then
          write(outs,'(a,i4,a)')'*data ',IGETNO(NGET,1),' 0 0 0 0'
          call eddisp(itru,outs)
        else
          call edisp(itru,'  ')
        endif
        if (NX.eq.0) then
          if(ixopen.eq.1)then
            write(outs,'(a)')'# -- No data: probably due to filtering.'
          else
            write(outs,'(a)')' -- No data: probably due to filtering.'
          endif
        else

C If zone sensible heating, sensible cooling, sensible load or
C zone latent loads do the following report.
          if (IGETNO(NGET,1).eq.8.or.IGETNO(NGET,1).eq.9.or.
     &        IGETNO(NGET,1).eq.10.or.IGETNO(NGET,1).eq.30.or.
     &        IGETNO(NGET,1).eq.31.or.IGETNO(NGET,1).eq.32.or.
     &        IGETNO(NGET,1).eq.33) then

C Create string description for time of maximum occurrance.
            MAXDOY=INT(DIVMXT/24.)+IODS
            TMAXHR=AMOD(DIVMXT,24.)
            call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
            call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
            write (XT,'(3a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
            MINDOY=INT(DIVMNT/24.)+IODS
            TMINHR=AMOD(DIVMNT,24.)
            call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
            call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
            write (NT,'(3a)')DESCR(1:6),'@',DESCRH
            
C Build up string for displaying.
            write (outs,'(3x,a,14x)') 'All'
            
C Write out results to 5 significant figures.
            NSIG=5
            call sigfig(DIVMX,NSIG,RNO,t12,LSTR)
            write (outs(21:46),'(a,1x,a,1x)') t12(1:LSTR),XT
            call sigfig(DIVMN,NSIG,RNO,t12,LSTR)
            write (outs(47:72),'(a,1x,a,1x)') t12(1:LSTR),NT
            write (outs(73:98),'(a)') '     --           --     '
            if (IROC.eq.-1) then
              call sigfig(XTOT,NSIG,RNO,t12,LSTR)
              write (outs(99:111),'(a)') t12
            endif
          else
          
C Build up string for displaying.
            write (outs,'(3x,a,15x)') 'All'
            
C Write out results to 5 significant figures.
            NSIG=5
            call sigfig(XMAX,NSIG,RNO,t12,LSTR)
            write (outs(22:47),'(a,1x,a,1x)') t12(1:LSTR),'     -- '
            call sigfig(XMIN,NSIG,RNO,t12,LSTR)
            write (outs(48:73),'(a,1x,a,1x)') t12(1:LSTR),'     -- '
            call sigfig(XAVE,NSIG,RNO,t12,LSTR)
            write (outs(74:99),'(a,1x,a)') t12,'     -- '
            if (IROC.eq.-1) then
              call sigfig(XTOT,NSIG,RNO,t12,LSTR)
              write (outs(100:112),'(a)') t12
            endif
          endif
        endif

C Display 'all zones' text.
        call eddisp(itru,outs)
      endif

C End tag for tagged data
      if (delim.eq.'X') then
        write(outs,'(a)')'*end_stats'
        call edisp(itru,outs)
      endif

C Allow other/ re- display?
      if(act(1:1).eq.'p')then
        return
      else
        goto 1
      endif

      END

C ******************** MOSMSDCEN ********************
C MOSMSDCEN uses the generic menu and get routines to retrieve max, min and
C other basic statistical data and then compares it to the values in the CEN 
C standard.

      SUBROUTINE MOSMSDCEN(act)
#include "building.h"

C Get common precz from geometry.h
#include "geometry.h"
      
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/SETPIK/NS,NSNO(MNRS),ISETON(MNRS),IMET,IFAFLG(MNRS,MNFA)
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      dimension YMAX(MZS), YMIN(MZS), YTOT(MZS), YTOTSD(MZS)
      dimension YAVE(MZS), NY(MZS)
      dimension CENMAX(9), CENMIN(9), CENAVE(9)

      character outs*124,act*1
      character prompt*124
      CHARACTER PDESCR*60,SDESCR*44,RSNAME*40
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      logical display

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        if(NGET.ge.1)then
          write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
        else
          write(prompt,'(a)')' Output being directed to file. ' 
        endif
      elseif(ixopen.eq.0)then
        prompt = ' '
      endif

      if(act(1:1).eq.'s')then
C Ask for the CEN standard.
        ISTAND=1
        display=.FALSE.

C Get the CEN data into the results array.
        if (ISTAND.eq.1) then

C CEN summer temperatures, geometry A.
          DATA CENMAX/38.9,34.7,33.7,37.8,32.5,32.4,40.3,35.0,33.6/
          DATA CENAVE/35.9,29.8,29.1,35.9,29.5,29.1,38.6,31.6,30.2/
          DATA CENMIN/33.2,25.3,25.3,34.4,26.5,26.3,37.0,28.2,27.5/
        endif
      endif

C Setup parameters and call GOGET for each output day to get required data.
C GOGET recovers the data in VAL2 (and averages output if required.)
C Variables begining X are for all zones selected whereas variables 
C starting Y are zone based.
      do 5 I=1,MZS
        YMAX(I)=-1.E+10
        YMIN(I)=1.E+10
        YTOT(I)=0.0
        YTOTSD(I)=0.0
        YAVE(I)=0.0
        NY(I)=0
 5    continue
      XMAX=-1.E+10
      XMIN=1.E+10
      XTOT=0.0
      XTOTSD=0.0
      XAVE=0.0
      NX=0
      call usrmsg(prompt,'Scanning for range of values...','-')

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS
 99   do 10 IDAY=IODS,IODF
        call GOGET(IDAY)

C Add values in VAL2 to there correct bins. Loop through selected zones.
        do 400 IX=1,NGET
          IZONE=IGETNO(IX,2)
          if (IZONE.ne.0) then 
            NZNO(IX)=IZONE
          endif
          DO 421 J = 1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
C Add half a timestep to TIME if data averaged.
            IHRD=(IDAY-IODS)*24
            call DATIME(J,ATIME)
            TIME=float(IHRD)+ATIME

C Within requested output period.
            IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
            ih=int(ATIME+1.)
            ioc=1
            if(iocupf.eq.1) call getocup(IZONE,IDAY,ih,ioc,ier)
            if(ioc.ne.0) then
              if (display) then
                XTOTSD=XTOTSD+((XAVE-VAL2(IX,J))**2)
                YTOTSD(IX)=YTOTSD(IX)+((YAVE(IX)-VAL2(IX,J))**2)
              else
                IF (VAL2(IX,J).GT.XMAX) XMAX=VAL2(IX,J)
                IF (VAL2(IX,J).LT.XMIN) XMIN=VAL2(IX,J)
                XTOT=XTOT+VAL2(IX,J)
                NX=NX+1
                IF (VAL2(IX,J).GT.YMAX(IX)) YMAX(IX)=VAL2(IX,J)
                IF (VAL2(IX,J).LT.YMIN(IX)) YMIN(IX)=VAL2(IX,J)
                YTOT(IX)=YTOT(IX)+VAL2(IX,J)
                NY(IX)=NY(IX)+1
              endif
            endif
 421      CONTINUE
 400    CONTINUE
 10   continue

C Calculate averages and rescan the data to calculate std deviations.
      if (.not.display) then
        do 20 I=1,NGET
          if (NY(I).eq.0) then
            YAVE(I)=0.
          else
            YAVE(I)=YTOT(I)/float(NY(I))
          endif
 20     continue
        if (NX.eq.0) then
          XAVE=0.
        else
          XAVE=XTOT/float(NX)
        endif
        display=.TRUE.
        goto 99
      endif
      call usrmsg(prompt,'Scanning for range of values...done.','P')
      call usrmsg('  ','  ','-')

C Now display the calculated data.
      call edisp(itru,'  ')
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)
      if (NS.lt.2) then
        write (outs,'(1X,A44,A5,A22,A5,A22)')
     &            SDESCR,' Lib:',RFILE(1:22),' Set:',RSNAME(ISIM)(1:22)
      else
        write (outs,'(1X,A44,A5,A22)') SDESCR,' Lib:',RFILE(1:22)
      endif
      call edisp(itru,outs)
      write (outs,'(a)') SLABEL(NGET)
      call edisp(itru,outs)
      call edisp(itru,'  ')

      write (outs,'(a,a)')'   Zone            Maximum       ',
     &                                        'Minimum       Mean'
      call edisp(itru,outs)
      write (outs,'(a,a)')'id name                 Delta',
     &                                  '         Delta         Delta'
      call edisp(itru,outs)
      do 30 I=1,NGET
        if (NY(I).eq.0) then
          write (outs,'(i2,1x,a12,a)') NZNO(I), zname(IGETNO(I,2)), 
     &                           'No data: probably due to filtering.'
        else
          write (outs,'(i2,1x,a12,3(f8.1,f6.1))') NZNO(I),
     &         zname(IGETNO(I,2)),YMAX(I),(YMAX(I)-CENMAX(I)),
     &         YMIN(I),(YMIN(I)-CENMIN(I)),YAVE(I),(YAVE(I)-CENAVE(I))
        endif
        call edisp(itru,outs)
 30   continue

      return
      END

C ******************** MOSQHR ********************
C MOSQHR: find number of hours above or below a given value using generic 
C         routines.
C  iabove >1 find hours above value
C  iabove <1 find hours below value
C act='p' iget parameters already set.

      SUBROUTINE MOSQHR(act,iabove,tval)
#include "building.h"
      
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)

      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      character prompt*124,outs*124
      CHARACTER DESCR*7,DESCR1*10,DESCRH*5,DESCRD*5,NT*14,XT*14
      CHARACTER RSNAME*40
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER PDESCR*60,SDESCR*44,act*1

      dimension YMAX(MZS), YMIN(MZS), YTOT(MZS)
      dimension YAVE(MZS), NY(MZS),NYABOVE(MZS),NYBELOW(MZS)
      dimension TMAX(MZS), TMIN(MZS)

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MOSQHR'  ! set for subroutine

C If output to file alter the edisp unit number.
      itru = icout
      prompt = ' '
      if(ixopen.eq.1)then
        itru = ixunit
      endif

C Call the menu of choices (this also sets some default options).
 1    if(act(1:1).eq.'-') then
        if (IABOVE.gt.0) then
          MENUTYPE=7
        else
          MENUTYPE=8
        endif
        call GOMSETUP
        call GOMENU
        if(ixopen.eq.1)then
          itru = ixunit
          if(NGET.ge.1)then
            write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
          else
            write(prompt,'(a)')' Output being directed to file. ' 
          endif
        else
          itru = icout
        endif
        if (MENUTYPE.eq.-1) return
      endif

C Ask for set point value.
      if (act(1:1).eq.'p') then
        XQUERY=tval
      else
        helptopic='res_above_below_pt'
        call gethelptext(helpinsub,helptopic,nbhelp)
        XQUERY=0.0
        CALL EASKR(XQUERY,' ','  Test point value ? ',
     &    0.0,'-',0.0,'-',20.0,'query value',IER,nbhelp)
      endif

C Setup parameters and call GOGET for each output day to get required data.
C GOGET recovers the data in VAL2 (and averages output if required.)
C Variables begining X are for all zones selected whereas variables 
C starting Y are zone based.
      do 5 I=1,MZS
        YMAX(I)=-1.E+10
        YMIN(I)=1.E+10
        YTOT(I)=0.0
        YAVE(I)=0.0
        NY(I)=0
        TMAX(I)=0.0
        TMIN(I)=0.0
        NYABOVE(I)=0
        NYBELOW(I)=0
 5    continue
      XMAX=-1.E+10
      XMIN=1.E+10
      XTOT=0.0
      NX=0
      NXABOVE=0
      NXBELOW=0
      call usrmsg(prompt,'Scanning for range of values...','-')

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS
      do 10 IDAY=IODS,IODF
        call GOGET(IDAY)

C Check values in VAL2 against XQUERY.
        do 400 IX=1,NGET
          IZONE=IGETNO(IX,2)
          DO 421 J = 1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
            IHRD=(IDAY-IODS)*24
            call DATIME(J,ATIME)
            TIME=float(IHRD)+ATIME

C Within requested output period.
            IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
            ih=int(ATIME+1.)
            ioc=1
            if(iocupf.eq.1) call getocup(IZONE,IDAY,ih,ioc,ier)
            if(ioc.ne.0) then

C Find largest value for current timestep and zone.
              IF (VAL2(IX,J).GT.XMAX) XMAX=VAL2(IX,J)
              IF (VAL2(IX,J).LT.XMIN) XMIN=VAL2(IX,J)
              XTOT=XTOT+VAL2(IX,J)
              NX=NX+1

C Find largest value for current timestep, note time.
              IF (VAL2(IX,J).GT.YMAX(IX)) then
                YMAX(IX)=VAL2(IX,J)
                TMAX(IX)=TIME
              endif
              IF (VAL2(IX,J).LT.YMIN(IX)) then
                YMIN(IX)=VAL2(IX,J)
                TMIN(IX)=TIME
              endif
              YTOT(IX)=YTOT(IX)+VAL2(IX,J)
              NY(IX)=NY(IX)+1

C Check current value against query value.
              if (iabove.gt.0) then
                if (VAL2(IX,J).gt.XQUERY) then
                  NYABOVE(IX)=NYABOVE(IX)+1
                  NXABOVE=NXABOVE+1
                else
                  NYBELOW(IX)=NYBELOW(IX)+1
                  NXBELOW=NXBELOW+1
                endif
              else
                if (VAL2(IX,J).lt.XQUERY) then
                  NYBELOW(IX)=NYBELOW(IX)+1
                  NXBELOW=NXBELOW+1
                else
                  NYABOVE(IX)=NYABOVE(IX)+1
                  NXABOVE=NXABOVE+1
                endif
              endif
            endif
 421      CONTINUE
 400    CONTINUE
 10   continue

C Calculate averages.
      do 20 I=1,NGET
        if (NY(I).eq.0) then
          YAVE(I)=0.
        else
          YAVE(I)=YTOT(I)/float(NY(I))
        endif
 20   continue

      call usrmsg(prompt,'Scanning for range of values...done.','P')
      call usrmsg('  ','  ','-')

C -- Now display the recovered data. --
C At each step here need to check if default space delimiter has been 
C changed.
      call edisp(itru,'  ')
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)
      WRITE(outs,'(3A,I4,2A)')
     &             'Lib: ',RFILE(1:22),' Set:',ISIM,': ',RSNAME(ISIM)
      call edisp(itru,outs)
      WRITE(outs,'(A)')PDESCR
      call edisp(itru,outs)
      WRITE(outs,'(A)')SDESCR
      call edisp(itru,outs)
      write (outs,'(a)') SLABEL(NGET)
      call edisp(itru,outs)
      if (iabove.gt.0) then
        write (outs,'(a,f7.2)')'Reporting number of hours above ',XQUERY
      else
        write (outs,'(a,f7.2)')'Reporting number of hours below ',XQUERY
      endif
      call edisp(itru,outs)
      call edisp(itru,'  ')

C Write specific cases first, generally in numerical order of IGET.
C Climate data therefore do not print zone names.
      if (IGETNO(NGET,1).eq.2.or.IGETNO(NGET,1).eq.19.or.
     &    IGETNO(NGET,1).eq.20.or.IGETNO(NGET,1).eq.21.or.
     &    IGETNO(NGET,1).eq.22.or.IGETNO(NGET,1).eq.26) then
        write (outs,'(a,a)') '    Maximum               Minimum',
     &                      '              Mean    No of hours'
        call eddisp(itru,outs)
        write (outs,'(a,a)') '  value    occurrence   value',
     &            '    occurrence    value   above    below'
        call eddisp(itru,outs)

C Now generate climate data output text.
        if (NX.eq.0) then
          write (outs,'(a)') ' -- No data: probably due to filtering.'
        else

C Create string description for time of maximum occurrance.
          MAXDOY=INT(TMAX(1)/24.)+IODS
          TMAXHR=AMOD(TMAX(1),24.)
          call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
          call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
          write (XT,'(3a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
          MINDOY=INT(TMIN(1)/24.)+IODS
          TMINHR=AMOD(TMIN(1),24.)
          call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
          call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
          write (NT,'(3a)')DESCR(1:6),'@',DESCRH
          AYAB=float(NYABOVE(1))*float(NOUT)/float(NTS)
          AYBE=float(NYBELOW(1))*float(NOUT)/float(NTS)
          write (outs,'(2(f7.1,2x,a12,1x),3f8.2)')YMAX(1),XT,
     &                                  YMIN(1),NT,YAVE(1),AYAB,AYBE
        endif

C Write climate data text as required.
        call eddisp(itru,outs)
      else

C All other IGET's. Display zone based information.
C Generate and display column headings.
        if(IGETNO(NGET,9).ge.15)then
          write(outs,'(a,a)')' Description              Maximum',
     & '                Minimum             Mean    No of hours     %'
          call eddisp(itru,outs)
          write(outs,'(a,a)')
     & '                        value  occurrence',
     & '     value  occurrence     value   above  below   above'
          call eddisp(itru,outs)
        elseif(IGETNO(NGET,9).le.12)then
          write(outs,'(a,a)')' Description       Maximum',
     & '                Minimum            Mean    No of hours     %'
          call eddisp(itru,outs)
          write(outs,'(a,a)')'                value  occurrence',
     & '     value  occurrence     value   above  below   above'
          call eddisp(itru,outs)
        else
          write(outs,'(a,a)')' Description          Maximum',
     & '                Minimum            Mean    No of hours     %'
          call eddisp(itru,outs)
          write(outs,'(a,a)')'                    value  occurrence',
     & '     value  occurrence    value   above  below   above'
          call eddisp(itru,outs)
        endif
        do 30 I=1,NGET
          if (NY(I).eq.0) then
            write (outs,'(2a)') GLABEL(I)(1:LNGLABEL(I)),
     &       ' No data: probably due to filtering.'
          else

C Create string description for time of maximum occurrance.
            MAXDOY=INT(TMAX(I)/24.)+IODS
            TMAXHR=AMOD(TMAX(I),24.)
            call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
            call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
            write (XT,'(3a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
            MINDOY=INT(TMIN(I)/24.)+IODS
            TMINHR=AMOD(TMIN(I),24.)
            call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
            call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
            write (NT,'(3a)')DESCR(1:6),'@',DESCRH
            AYAB=float(NYABOVE(I))*float(NOUT)/float(NTS)
            AYBE=float(NYBELOW(I))*float(NOUT)/float(NTS)
            APYAB=(AYAB/(AYAB+AYBE))*100.
            if(IGETNO(NGET,9).ge.15)then
              write (outs,1102) GLABEL(I)(1:20),YMAX(I),XT,YMIN(I),NT,
     &          YAVE(I),AYAB,AYBE,APYAB
            elseif(IGETNO(NGET,9).le.12)then
              write (outs,1102) GLABEL(I)(1:12),YMAX(I),XT,YMIN(I),NT,
     &          YAVE(I),AYAB,AYBE,APYAB
            else
              write (outs,1102) GLABEL(I)(1:15),YMAX(I),XT,YMIN(I),NT,
     &          YAVE(I),AYAB,AYBE,APYAB
            endif
 1102       format (a,1x,2(f8.2,1x,a12,1x),3f8.2,f7.1)
          endif
          call eddisp(itru,outs)
 30     continue

C Generate 'all zones' text.  
C Remember that depending on query the number of hours will be 
C 'greater than' or 'greater than or equal' to the query point.
        if (NX.eq.0) then
          write(outs,'(a)') ' -- No data: probably due to filtering.'
          call edisp(itru,'  ')
          call edisp(itru,outs)
        else
          call edisp(itru,'  ')
          AXAB=float(NXABOVE)*float(NOUT)/float(NTS)
          AXBE=float(NXBELOW)*float(NOUT)/float(NTS)
          APXAB=(AXAB/(AXAB+AXBE))*100.
          APXBE=(AXBE/(AXAB+AXBE))*100.
          if (iabove.gt.0) then
            write (outs,1104) ' Total number of hours',
     &        ' greater than query point:',AXAB,'  (',APXAB,'%)'
            call edisp(itru,outs)
            write (outs,1104) ' Total number of hours',
     &        ' less than or equal to query point:',AXBE,
     &        '  (',APXBE,'%)'
            call edisp(itru,outs)
          else
            write (outs,1104) ' Total number of hours',
     &        ' less than query point:',AXBE,'  (',APXBE,'%)'
            call edisp(itru,outs)
            write (outs,1104) ' Total number of hours',
     &        ' greater than or equal to query point:',AXAB,
     &        '  (',APXAB,'%)'
            call edisp(itru,outs)
          endif
 1104     format (a,a,f10.2,a,f5.1,a)
        endif
      endif

C Allow other/ re- display?
      if(act(1:1).eq.'p')then
        return
      else
        goto 1
      endif

      END

C ******************** MOEDMDS ********************
C MOEDMDS: energy demands (radiant + convective) integrated over the
C assessment period and number of hours required. 
C Called by IPV report if act = 'i'
C Called by normal stats if act = '-'
C Also called by recovert if act = 'p' << needs checking >>

      SUBROUTINE MOEDMDS(act,the,tzhtime,tce,tzctime,tlhe,tzlhtime,
     &  tlce,tzlctime)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM
      COMMON/ZONPIK/NZ,NZNO(MCOM)

      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      character prompt*124,outs*124,prompte*124
      CHARACTER RSNAME*40
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER PDESCR*60,SDESCR*44,act*1
      DIMENSION YDUM(MTS)

C If output to file alter the edisp unit number.
      prompt = ' '
      if (act(1:1).ne.'i') then
        itru = icout
        if(ixopen.eq.1)then
          itru = ixunit
        endif
      else
        itru = icout
        if(ixopen.eq.1)then
          itru = ixunit
        endif
      endif

C Set all returned data to zero.
C THE is total sensible heating, TCE is total sensible cooling.
      THE=0.
      TCE=0.
      tzhtime=0.
      tzctime=0.

C TLHE is total latent humidification, TLCE is total latent dehumidification.
      TLHE=0.
      TLCE=0.
      tzlhtime=0.
      tzlctime=0.

C Call the menu of choices (this also sets some default options).
 1    if(act(1:1).ne.'i') then
        MENUTYPE=10
        call GOMSETUP
        call GOMENU
        if(ixopen.eq.1)then
          itru = ixunit
          if(NGET.ge.1)then
            write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
          else
            write(prompt,'(a)')' Output being directed to file. ' 
          endif
        else
          itru = icout
        endif
        if (MENUTYPE.eq.-1) return
      else
        if(NGET.ge.1)then
          write(prompt,'(3a)') 'Scanning ',
     &      SLABEL(1)(1:LNSLABEL(1)),'...'
        else
          write(prompt,'(a)') 'Scanning...' 
        endif
      endif
      write(prompte,'(2a)') prompt(1:lnblnk(prompt)),'done.'

C Energy requirements.
      ISET=ISIM
      if(act(1:1).ne.'i') then
        CALL HDDATE(PDESCR)
        CALL HDSTEP(SDESCR)
        lsn1=MIN0(lnblnk(RFILE),22)
        if(NSIM.gt.1)then
          write(outs,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &      ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        else
          write(outs,'(4A)')'Lib: ',RFILE(1:lsn1),
     &      ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
        endif
        call edisp(itru,outs)
        write(outs,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &    SDESCR(11:lnblnk(SDESCR))
        call edisp(itru,outs)

        call edisp(itru,' ')
        call edisp(itru,
     &' Zone total sensible and latent plant used (kWhrs)')
        write(outs,'(2A)')
     &'    Zone             Sensible heating  Sensible cooling',
     &'   Humidification     Dehumidification'
        call edisp(itru,outs)
        write(outs,'(2A)')
     &' id name             Energy   No. of   Energy   No. of ',
     &'   Energy   No. of    Energy   No. of'
        call edisp(itru,outs)
        write(outs,'(2A)')
     &'                     (kWhrs)  Hr rqd   (kWhrs)  Hr rqd ',
     &'   (kWhrs)  Hr rqd    (kWhrs)  Hr rqd'
        call edisp(itru,outs)
      endif

C Find the number of hours represented by an output timestep.
      TFACT=FLOAT(NOUT)/FLOAT(NTS)

C For each requested zone and for every computational time-step, get
C results for complete day.
      DO 65 J=1,NZ
        IZ=NZNO(J)
        ZHE=0.
        ZCE=0.
        ZLHE=0.
        ZLCE=0.
        zhtime=0.
        zctime=0.
        zlhtime=0.
        zlctime=0.
        DO 55 I=IODS,IODF
          ID=I
          NEL=0

C Get the zone plant flux and average if required.
          CALL GZQM(ID,IZ,ISET)
          CALL FLTIAV(ID,GVAL,YDUM,NEL)

C Calcalate the sums and statistics to display during relevant period.
          CALL CHKTIME(ID,ISTART,IEND)
          CALL SUM1V(YDUM,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
          ZHE=ZHE+(SGT/1000.)
          ZCE=ZCE+(SLT/1000.)
          ZHTIME=ZHTIME+FLOAT(IGT)*TFACT
          Zctime=Zctime+FLOAT(ILT)*TFACT

C Get zone humidification flux and average if required.
          CALL GZLLH(ID,IZ,ISET)
          CALL FLTIAV(ID,GVAL,YDUM,NEL)

C Calcalate the sums and statistics to display during relevant period.
          CALL CHKTIME(ID,ISTART,IEND)
          CALL SUM1V(YDUM,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
          ZLHE=ZLHE+(SGT/1000.)
          ZLHTIME=ZLHTIME+FLOAT(IGT)*TFACT

C Get zone de-humidification flux and average if required.
          CALL GZLLC(ID,IZ,ISET)
          CALL FLTIAV(ID,GVAL,YDUM,NEL)

C Calcalate the sums and statistics to display during relevant period.
          CALL CHKTIME(ID,ISTART,IEND)
          CALL SUM1V(YDUM,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
          ZLCE=ZLCE+(SLT/1000.)
          zlctime=zlctime+FLOAT(ILT)*TFACT
   55   CONTINUE
        THE=THE+ZHE
        TCE=TCE+ZCE
        TLHE=TLHE+ZLHE
        TLCE=TLCE+ZLCE
        tzhtime=tzhtime+zhtime
        tzctime=tzctime+zctime
        tzlhtime=tzlhtime+zlhtime
        tzlctime=tzlctime+zlctime
        if(act(1:1).ne.'i') then
          write(outs,66)IZ,zname(IZ),ZHE,ZHTIME,ZCE,Zctime,ZLHE,
     &      ZLHTIME,ZLCE,zlctime
   66     format(I3,1X,A12,4(F11.2,F8.1))
          call eddisp(itru,outs)
        endif
   65 CONTINUE
      if(act(1:1).ne.'i') then
        if(THE.gt.99999.0.or.TCE.lt.-99999.0)then
          write(outs,67)THE,tzhtime,TCE,tzctime,TLHE,tzlhtime,TLCE,
     &      tzlctime
   67     format(1X,'   All',8X,4(F11.1,F8.0))
        else
          write(outs,68)THE,tzhtime,TCE,tzctime,TLHE,tzlhtime,TLCE,
     &      tzlctime
   68     format(1X,'   All',9X,4(F11.1,F8.1))
        endif
        call edisp(itru,' ')
        call eddisp(itru,outs)
        call edisp(itru,' ')
      endif
      call usrmsg(prompte,'  ','P')
      if(MMOD.eq.8)call usrmsg('  ','  ','-')

      return
      end

C ******************** CASDIST ********************

C Reporting of casual gains distribution (radiant and convective)
C within current zone set.

C << Future update would be to read the explicit radiant gains for
C << each surface if save level 4 so that the zone construction
C << file does not need to be scanned to get the surface emissivity.

      SUBROUTINE CASDIST(act)

#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      COMMON/FILEP/IFIL
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)
      common/recver/izver,ipver,iever

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      COMMON/RECV5/SNA(MCOM,MS)
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM
      COMMON/ZONPIK/NZ,NZNO(MCOM)

      CHARACTER PDESCR*60,SDESCR*44,CSTR*25
      CHARACTER*13 LABELS(13)
      CHARACTER*5 LABSurf(2)
      character outs*124,act*1,prompt*124
      CHARACTER RSNAME*40
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      DIMENSION XVAL1(MZS)
      DIMENSION ST(6,10)

C Check for sufficient information.
      IF(ISAVE.le.1)THEN
        CALL EPAGEW
        RETURN
      ENDIF

      LABSurf(1)= ' @opq'
      LABSurf(2)= ' @trn'

C If output to file alter the edisp unit number.
      itru = icout
      prompt = ' '
      if(ixopen.eq.1)then
        itru = ixunit
      endif

C Call the menu of choices (this also sets some default options).
      if(act(1:1).eq.'-') then
        MENUTYPE=11
        call GOMSETUP
        call GOMENU
        if(ixopen.eq.1)then
          itru = ixunit
          if(NGET.ge.1)then
            write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
          else
            write(prompt,'(a)')' Output being directed to file. ' 
          endif
        else
          itru = icout
        endif
        if (MENUTYPE.eq.-1) return
      endif

C Energy requirements.
      call usrmsg(prompt,'Scanning for range of values...','-')
      ISET=ISIM
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)
      lsn1=MIN0(lnblnk(RFILE),22)
      if(NSIM.gt.1)then
        write(outs,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      else
        write(outs,'(4A)')'Lib: ',RFILE(1:lsn1),
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      endif
      call edisp(itru,outs)
      write(outs,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &   SDESCR(11:lnblnk(SDESCR))
      call edisp(itru,outs)

C Initialiase variables for totals over all selected zones.
      TPTTOTCG=0.
      TPCV=0.
      TPTTR=0.
      TPTOTE=0.
      TPTTI=0.
      TPTTG=0.
 
      do 66 ii=1,10
        do 67 ij=1,6
          ST(ij,ii)=0.
 67     continue
 66   continue

C Read geometry and construction files and setup labels.
      DO 65 JZ=1,NZ
        izone=NZNO(JZ)
        iunit=ifil+1
        call georead(iunit,LGEOM(izone),izone,0,iuout,ier)

        CALL ECONST(LTHRM(IZONE),IUNIT,izone,0,IUOUT,IER)

C Update labels to match the current zone.
C << revise for additional casual gains >>
        write(LABELS(1),'(1x,a,4x)')lodlabel(izone,1)
        write(LABELS(2),'(1x,a,4x)')lodlabel(izone,1)
        write(LABELS(3),'(1x,a,4x)')lodlabel(izone,2)
        write(LABELS(4),'(1x,a,4x)')lodlabel(izone,2)
        write(LABELS(5),'(1x,a,4x)')lodlabel(izone,3)
        write(LABELS(6),'(1x,a,4x)')lodlabel(izone,3)
        write(LABELS(7),'(2a)')' Ctl ',lodlabel(izone,1)
        write(LABELS(8),'(2a)')' Ctl ',lodlabel(izone,1)
        write(LABELS(9),'(2a)')' Ctl ',lodlabel(izone,2)
        write(LABELS(10),'(2a)')' Ctl ',lodlabel(izone,2)
        write(LABELS(11),'(2a)')' Ctl ',lodlabel(izone,3)
        write(LABELS(12),'(2a)')' Ctl ',lodlabel(izone,3)
        write(LABELS(13),'(a)')' Totals'

C Output general header information.
        write(outs,'(3a,i2,a)')
     &    ' Casual gains distribution (kWhrs) for ',
     &    zname(IZONE)(1:lnzname(IZONE)),' (',IZONE,')'
        call edisp(itru,' ')
        call edisp(itru,outs)

C If izver >= 4 reads data directly so zone operation file not necessary.
        if(izver.eq.4)then
          continue
        else

C Prepare to recover information from results library.
          IUNIT=IFIL+1
          CALL EROPER(0,iuout,iunit,izone,IER)  ! for older results library
          if(ip3ver(izone).eq.0)then
            write(outs,'(a,i2,a)') 
     &      'Operation file for zone ',izone,' periods being sorted!'
            call edisp(iuout,outs)
            call PROCESSOLDCAS(izone,0,iuout)
          endif
          IF(IER.EQ.1)RETURN
          IF(ICGC(IZONE).EQ.1)CALL ercgcf(0,iuout,LCGCIN(izone),
     &      IZONE,ier)
        endif

        N=24*NTS
        TCMULT=FLOAT(NOUT)/(FLOAT(NTS)*1000.)
        NN=NZSUR(IZONE)

C IGT is number of occupied timesteps, IGTL is number of lighting timesteps, IGTS
C is number of small power timesteps, IGTC is controlled timesteps.
        IGT=0
        IGTL=0
        IGTS=0
        IGTC=0
        ZOTIME=0.
        ZLTIME=0.
        ZSTIME=0.
        ZCTIME=0.

C Find the number of hours represented by an output timestep.
        TFACT=FLOAT(NOUT)/FLOAT(NTS)

C Radiant casual gains are area AND emissivity weighted.
        emarea=0.0
        do 80 kk=1,NN
          emarea=emarea+sna(izone,kk)*EMISI(kk)
  80    continue

C Initialise summation locations: VAL1(I,J).
        DO 202 I=1,MZS
          XVAL1(I)=0.
          DO 201 J=1,N
            VAL2(I,J)=0.
  201     CONTINUE
  202   CONTINUE

C Find the total surface area * emissivity in each surface
C connection class. << This could be simplified with new izver. >>
        SURFAE=0.
        SURFAI=0.
        SURFAG=0.
        SURFTE=0.
        SURFTG=0.
        SURFTI=0.
        DO 210 ISN= 1,NN
          CALL SURADJ(IZONE,ISN,IE,T,IZC,ISC,ICN,CSTR)
          IF(IE.EQ.0)THEN
            if(SSOTF(ICN)(1:4).EQ.'OPAQ'.OR.
     &         SSOTF(ICN)(1:4).EQ.'UNKN')THEN
              SURFAE=SURFAE+SNA(IZONE,ISN)*EMISI(ISN)
            else
              SURFTE=SURFTE+SNA(IZONE,ISN)*EMISI(ISN)
            endif
          ELSEIF(IE.EQ.4)THEN
            if(SSOTF(ICN)(1:4).EQ.'OPAQ'.OR.
     &         SSOTF(ICN)(1:4).EQ.'UNKN')THEN
              SURFAG=SURFAG+SNA(IZONE,ISN)*EMISI(ISN)
            else
              SURFTG=SURFTG+SNA(IZONE,ISN)*EMISI(ISN)
            endif
          ELSE
            if(SSOTF(ICN)(1:4).EQ.'OPAQ'.OR.
     &         SSOTF(ICN)(1:4).EQ.'UNKN')THEN
              SURFAI=SURFAI+SNA(IZONE,ISN)*EMISI(ISN)
            else
              SURFTI=SURFTI+SNA(IZONE,ISN)*EMISI(ISN)
            endif
          ENDIF
  210   CONTINUE

C Day-by-day recovery.
        DO 30 IDAY=IODS,IODF
          CALL CHKTIME(IDAY,ISTART,IEND)

C Clear VAL1.
          DO 18 I=1,MZS
            DO 19 II=ISTART,IEND
              VAL1(I,II)=0.
   19       CONTINUE
   18     CONTINUE

C For requested zone and day get convective casual gains results.
C If type one casual > 0. then occupied during timestep (and increment
C IGTL, IGTS, IGTC.
          CALL ZCASC(IZONE,IDAY)
          DO 50, I=1,IEND
            VAL1(1,I)=VAL3(15,I)
            if(I.ge.ISTART.and.VAL1(1,I).gt.0.0)IGT=IGT+1
            VAL1(2,I)=VAL3(16,I)
            if(I.ge.ISTART.and.VAL1(2,I).gt.0.0)IGTL=IGTL+1
            VAL1(3,I)=VAL3(17,I)
            if(I.ge.ISTART.and.VAL1(3,I).gt.0.0)IGTS=IGTS+1
            VAL1(4,I)=VAL3(18,I)
            if(I.ge.ISTART.and.VAL1(4,I).gt.0.0)IGTC=IGTC+1
            VAL1(5,I)=VAL3(19,I)
            if(I.ge.ISTART.and.VAL1(5,I).gt.0.0)IGTC=IGTC+1
            VAL1(6,I)=VAL3(20,I)
            if(I.ge.ISTART.and.VAL1(6,I).gt.0.0)IGTC=IGTC+1
   50     CONTINUE

C Get casual radiant amount for all surfaces and store uncontrolled
C and controlled types 1,2,3 in VAL1(5,I) to VAL1(10,I).
          do 52 ISS=1,NN
            CALL ZCASR(IZONE,ISS,IDAY)
            DO 28 I=1,N
              VAL1(7,I)=VAL1(7,I)+VAL3(15,I)
              VAL1(8,I)=VAL1(8,I)+VAL3(16,I)
              VAL1(9,I)=VAL1(9,I)+VAL3(17,I)
              VAL1(10,I)=VAL1(10,I)+VAL3(18,I)
              VAL1(11,I)=VAL1(11,I)+VAL3(19,I)
              VAL1(12,I)=VAL1(12,I)+VAL3(20,I)
   28       CONTINUE
   52     continue

          DO 400 J=ISTART,IEND,NOUT
            JT=J

C Compute average of VAL1() based on IAV.
            CALL VALIAV(12,JT,XVAL1)

C Sum to VAL2 for printing.
            DO 410 K=1,12
              VAL2(K,1)=VAL2(K,1)+XVAL1(K)*TCMULT
  410       CONTINUE
  400     CONTINUE
   30   CONTINUE

C Find number of hours for occupants, lights, small power, controlled.
        ZOTIME=FLOAT(IGT)*TFACT
        ZLTIME=FLOAT(IGTL)*TFACT
        ZSTIME=FLOAT(IGTS)*TFACT
        ZCTIME=FLOAT(IGTC)*TFACT

C Print the data and labels for uncontrolled casual gains.
        write(outs,128)
  128   format(' Gains     |   Total   | Convective|    Radiative  |',
     &         '        Radiant by connection type')
        call edisp(itru,outs)
        write(outs,1281)
 1281   format(' type      | (Con+Rad) | part (air)|     on surf   |',
     &         '    external    internal     ground ')
        call edisp(itru,outs)

C Split radiant by surface connection type, begin by multiplying
C each casual by total surface area for that connnection type
C and dividing by the zone opaque area.  Write it out and then
C sum it to TOT0 etc. If no casual gain control then don't bother
C to print out latter 6 rows.
        IF(ICGC(IZONE).EQ.0)THEN
          IRLC=9
        else
          IRLC=12
        endif

        TR=0.
        TOR=0.
        TTR=0.
        TOTE=0.
        TTI=0.
        TTG=0.
        TTOTCG=0.
        DO 420 IR=7,IRLC

C Apply radiant by surface groupings.
          CE =VAL2(IR,1)*SURFAE/emarea
          CTE=VAL2(IR,1)*SURFTE/emarea
          CI =VAL2(IR,1)*SURFAI/emarea
          CTI=VAL2(IR,1)*SURFTI/emarea
          CG =VAL2(IR,1)*SURFAG/emarea
          CTG=VAL2(IR,1)*SURFTG/emarea
          TOR =CE+CI+CG
          TR =CTE+CTI+CTG
          TOTCG=VAL2(IR-6,1) + TOR + TR

C Update IR.
          IF(IR.EQ.7)IRL=1
          IF(IR.EQ.8)IRL=3
          IF(IR.EQ.9)IRL=5
          IF(IR.EQ.10)IRL=7
          IF(IR.EQ.11)IRL=9
          IF(IR.EQ.12)IRL=11

          write(outs,126)LABELS(IRL)(1:11),TOTCG,VAL2(IR-6,1),
     &                   TOR,LABSurf(1),CE,CI,CG
          call edisp(itru,outs)
          write(outs,127) TR, LABSurf(2),CTE,CTI,CTG
          call edisp(itru,outs)
          TTR=TTR+CE+CI+CG+CTE+CTI+CTG
          TOTE=TOTE+CE+CTE
          TTI=TTI+CI+CTI
          TTG=TTG+CG+CTG
          TTOTCG=TTOTCG+TOTCG

C Add each value to have the sub-total over all the selected zones
          ST(IR-6,1)=ST(IR-6,1)+TOTCG
          ST(IR-6,2)=ST(IR-6,2)+VAL2(IR-6,1)
          ST(IR-6,3)=ST(IR-6,3)+TOR
          ST(IR-6,4)=ST(IR-6,4)+CE
          ST(IR-6,5)=ST(IR-6,5)+CI
          ST(IR-6,6)=ST(IR-6,6)+CG
          ST(IR-6,7)=ST(IR-6,7)+TR
          ST(IR-6,8)=ST(IR-6,8)+CTE
          ST(IR-6,9)=ST(IR-6,9)+CTI
          ST(IR-6,10)=ST(IR-6,10)+CTG
  420   CONTINUE

C Totals:
        CV=VAL2(1,1)+VAL2(2,1)+VAL2(3,1)+VAL2(4,1)+VAL2(5,1)+VAL2(6,1)

        write(outs,118)' Totals    ',TTOTCG,CV,TTR,TOTE,TTI,TTG
        call edisp(itru,outs)

  126   format(A,3F12.2,A,3F12.2)
  127   format(35X,F12.2,A,3F12.2)
  118   format(A,3F12.2,4x,3F12.2)

        write(outs,'(a,f9.2)') ' Number of hours occupied: ',ZOTIME
        call edisp(itru,outs)
        write(outs,'(a,f9.2)') ' Number of hours with lights: ',ZLTIME
        call edisp(itru,outs)
        write(outs,'(a,f9.2)') ' Number of hours with small power: ',
     &    ZSTIME
        call edisp(itru,outs)
        write(outs,'(a,f9.2)') ' Number of hours with ctld gains: ',
     &    ZCTIME
        call edisp(itru,outs)

C Total of 'Totals' over selected zones    
        TPTTOTCG=TPTTOTCG+TTOTCG
        TPCV=TPCV+CV
        TPTTR=TPTTR+TTR
        TPTOTE=TPTOTE+TOTE
        TPTTI=TPTTI+TTI
        TPTTG=TPTTG+TTG
  65  continue

C Display tthe total over all selected zones
      IF((JZ.EQ.NZ+1).AND.(NZ.GT.1)) THEN
C Output general header information.
        call edisp(itru,'  ')
        call edisp(itru,
     &    ' Casual gains distribution (kWhrs) for ALL SELECTED ZONES')

C Print the data and labels for uncontrolled casual gains.
        write(outs,'(2a)')
     &    ' Gains     |   Total   | Convective|    Radiative  | ',
     &    '       Radiant by connection type'
        call edisp(itru,outs)
        write(outs,'(2a)')
     &    ' type      | (Con+Rad) | part (air)|     on surf   | ',
     &    '    external    internal     ground '
        call edisp(itru,outs)
        DO 520 IIR=7,IRLC
          IF(IIR.EQ.7)IRL=1
          IF(IIR.EQ.8)IRL=3
          IF(IIR.EQ.9)IRL=5
          IF(IIR.EQ.10)IRL=7
          IF(IIR.EQ.11)IRL=9
          IF(IIR.EQ.12)IRL=11
          write(outs,226)LABELS(IRL)(1:11),ST(IIR-6,1),ST(IIR-6,2),
     &      ST(IIR-6,3),LABSurf(1),ST(IIR-6,4),ST(IIR-6,5),ST(IIR-6,6)
          call edisp(itru,outs)

          write(outs,227)ST(IIR-6,7),LABSurf(2),ST(IIR-6,8),ST(IIR-6,9),
     &                   ST(IIR-6,10)
          call edisp(itru,outs)
  520   CONTINUE
        write(outs,218)' Totals    ',TPTTOTCG,TPCV,TPTTR,TPTOTE,TPTTI,
     &    TPTTG
        call edisp(itru,outs)
  226   format(A,3F12.2,A,3F12.2)
  227   format(35X,F12.2,A,3F12.2)
  218   format(A,3F12.2,5x,3F12.2)
      ENDIF

      call edisp(itru,' ')
      call edisp(itru,
     &' (@opq & @trn = associated with opaque or transparent surfaces)')
      call usrmsg(prompt,'Scanning for range of values...done.','P')

      RETURN
      END
