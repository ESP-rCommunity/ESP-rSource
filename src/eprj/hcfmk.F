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


C This file contains routines for creating and editing a zone
C convection-calculation control file:
C   HCFMK   Controlling routine for creating/editing convection file.
C   HCFMKHL High level questions about regime.
C   EHCMAN  Controls manual creation/editing of convection file.
C   EHCMANINT  Set initial values of convection file.
C   EPHCF   Edits one period of convection-calculation control data.
C   EMKHTC  Writes convection file.
C   EMKHTC2  Writes multi-day convection file.
C   EHTCFF  Reads convection file.
C   EconvA  Controls input for buoyancy-driven convective regime A.
C   EconvB  Controls input for buoyancy-driven convective regime B.
C   EconvC  Controls input for mechanical-driven convective regime C.
C   EconvD  Controls input for mechanical-driven convective regime D.
C   EconvE  Controls input for mixed flow convective regime E.
C   EconvF  Controls input for open vertical channel convection, regime F

C ******************** HCFMK ********************
C HCFMK controls the creation and editing of a convection-regime
C control file.

      SUBROUTINE HCFMK(ITRU,IUF,ICOMP,IER)
#include "building.h"
#include "model.h"
#include "schedule.h"
#include "geometry.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      CHARACTER hcfpdescr*72
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      CHARACTER*72 LTMP
      character HFILE*72,head*33
      CHARACTER ITEM(31)*48        ! menu items
      character OUTSTR*124,outs*124
      character KEY*1
      logical OK,modcfg,XST,clkok
      integer NITEMS,INO ! max items and current menu item
      integer ISTRW
      integer iday       ! for toggling through multiple day types

C Short and long descriptions of correlations.
      character hcshtdescr*32  ! short description
      character hclngdescr*48   ! long description
      dimension hcshtdescr(-1:99),hclngdescr(-1:99)

      helpinsub='hcfmk'  ! set for subroutine

C Before presenting hc regime, make up a suitable file name
C in case it is useful.
      modcfg=.false.
      IHCFP(1)=0
      iday=1       ! start with assumption of one day type.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(HFILE,'(2A)')zname(ICOMP)(1:lnzname(ICOMP)),'.htc'
      else
        WRITE(HFILE,'(4A)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.htc'
      endif
      IF(IHC(ICOMP).EQ.1)THEN

C A convection file is referenced, confirm file name, offer
C option to dereference. If ok check its format.
        LTMP=LHCCO(ICOMP)
        helptopic='zone_htc_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        clkok=.false.
        ISTRW=72
  245   CALL EASKSCMD(LTMP,'Zone convective regime file?',
     &    ' ','dereference',clkok,ISTRW,HFILE,'conv regime file',
     7    IER,nbhelp)
        if(clkok)then
          call edisp(iuout,'Removing reference to conv regime file.')
          LHCCO(ICOMP)='UNKNOWN'
          IHC(ICOMP)=0
          modcfg=.true.
          CALL EMKCFG('-',IER)
          modcfg=.false.
          return
        endif
        if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN')then
          LHCCO(ICOMP)=LTMP
        else
          goto 245
        endif

        call FINDFIL(LHCCO(ICOMP),XST)
        IF(XST)THEN
          CALL EFOPSEQ(IUF,LHCCO(ICOMP),1,IER)
          IF(ier.eq.0)THEN

C Does the 1st and 2nd line have one item and the 3rd line have two items?
            CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc line 1',IER)
            CALL STRIPC(IUF,OUTSTR,99,num2,1,'htc line 2',IER)
            CALL STRIPC(IUF,OUTSTR,99,num3,1,'htc line 3',IER)
            if(num1.eq.1.and.num2.eq.1.and.num3.eq.2)then
              continue   ! a legacy format we can read
            else

C See if HC file has header and date stamp. If not, check with
C user to see if this file should be overwritten with HC control
C data.
              rewind(iuf,err=392)
              CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc line 1',IER)
              if(OUTSTR(1:7).eq.'*HC_CTL'.or.
     &           OUTSTR(1:7).eq.'*hc_ctl')then
                continue
              else
                write(outs,'(2a)')
     &            LHCCO(icomp)(1:lnblnk(LHCCO(icomp))),
     &            ' is not a HC control file!'
                helptopic='zone_htc_expected'
                call gethelptext(helpinsub,helptopic,nbhelp)
                CALL EASKOK(outs,
     &            'Overwrite anyway?',OK,nbhelp)
                if(.NOT.OK)then
                  call usrmsg('Exiting conv regime facility',
     &                    '(existing file not touched)','W')
                  ier=0
                  return
                endif
                IHC(ICOMP)=1
                call efdelet(IUF,ISTAT) 
                modcfg=.true.
                goto 43
              endif
            endif
          ENDIF
          CALL ERPFREE(IUF,ISTAT)

C Read a legacy or current convection file.
          call ehtcff(LHCCO(ICOMP),IUF,IER)
          if(ier.eq.-301)then
            helptopic='zone_htc_agree_save'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKOK('HC file not found!',
     &         'Create one?',OK,nbhelp) 
            if(.NOT.OK)return
            IHC(ICOMP)=1 
            LHCCO(ICOMP)=HFILE
            modcfg=.true.
          endif
        ENDIF

      ELSEIF(IHC(ICOMP).NE.1)THEN

C There is no mention of hc file so assign with default name and
C check to see if this can be read in. Offer option to dereference
C and jump back if hc facility entered in error.
        if(LHCCO(ICOMP)(1:2).eq.'  '.or.LHCCO(ICOMP)(1:4).eq.'UNKN')then
          LTMP=HFILE
        else
          LTMP=LHCCO(ICOMP)
        endif
        helptopic='zone_htc_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        clkok=.false.
        ISTRW=72
  246   CALL EASKSCMD(LTMP,'Zone convective regime file?',
     &    ' ','dereference',clkok,ISTRW,HFILE,'conv regime file',IER,7)
        if(clkok)then
          call edisp(iuout,'Canceling convective regime for this zone.')
          LHCCO(ICOMP)='UNKNOWN'
          IHC(ICOMP)=0
          modcfg=.true.
          CALL EMKCFG('-',IER)
          modcfg=.false.
          return
        else
          if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN')then
            LHCCO(ICOMP)=LTMP
            IHC(ICOMP)=1 
            call FINDFIL(LHCCO(ICOMP),XST)
            IF(XST)THEN
              call edisp(iuout,'Reading existing hc regime file...')
              call ehtcff(LHCCO(ICOMP),IUF,IER)
            else

C Ask some high level questions prior to creating file.
C Define number of day types, ask about which control regime.
              call HCFMKHL(ITRU,IUF,ICOMP,IER)
            endif
            modcfg=.true.
          else
            goto 246
          endif
        endif
      ENDIF

  43  if(modcfg)CALL EMKCFG('-',IER)

C Display the reference zone.
      call georead(IUF,LGEOM(ICOMP),ICOMP,0,iuout,IER)
      MODIFYVIEW=.TRUE.
      MODBND=.TRUE.

C Reset all surface lines to standard width. Include surface
C names, turn off grid and origin.
      CALL INLNST(1)
      itsnm=0; itorg=1; itgrd=1
      nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
      CALL redraw(IER)
      if(MMOD.EQ.8) call redrawbuttons()

C Confirm if one day type or follow current day types.
   21 INO = -2
      if(IHCDT.eq.1)then
        write(ITEM(1),'(a)')'a focus: All days           '
      elseif(IHCDT.gt.1)then
        write(ITEM(1),'(2a)')'a focus >> ',CALENTAG(iday)
      elseif(IHCDT.eq.0)then
        IHCDT=1
        write(ITEM(1),'(a)')'a focus: All days                '
      endif
      ITEM(2) =             'b list current regime            '
      ITEM(3) =             'c list available correlations    '
      ITEM(4) =             '  ---- time ---  ---- type ----- '
      m=4; n=0
      do loop=1,IHCFP(iday)
        n=n+1
        CALL EMKEY(loop+3,KEY,IER)
        write(item(m+n),'(2a,f6.2,f6.2,2a)') key,' ',ST(iday,loop),
     &     EN(iday,loop),' ',hcfpdescr(iday,loop)(1:32)
      enddo
      ITEM(m+n+1)=          '> save                        '
      ITEM(m+n+2)=          '! reset regime                '
      ITEM(m+n+3)=          '? help                        '
      ITEM(m+n+4)=          '- exit                        '
      NITEMS=m+n+4

C Redraw wireframe on each return.
      CALL INLNST(1)
      itsnm=0; itorg=1; itgrd=1
      nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
      CALL redraw(IER)

C Help text for this menu.
      helptopic='zone_htc_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Present options.
      write(head,'(2a)') ' Convective regime in ',zname(ICOMP)(1:11) 
      CALL EMENU(head,ITEM,NITEMS,INO)

      if(INO.EQ.1.and.IHCDT.gt.1)then
        if(iday.lt.NBDAYTYPE)then
          iday=iday+1
        elseif(iday.eq.NBDAYTYPE)then
          iday=1
        endif
        goto 21  ! update focus day and menu
      elseif(INO.eq.2)then

C List current data.
        call listhtc(iuout,ICOMP,'f',IER)
        goto 21
      elseif(INO.eq.3)then

C Recover and list out the available correlations.
        call gethccornames(hcshtdescr,hclngdescr)
        call edisp(iuout,'Index description')
        write(outs,'(2a)') ' -1 ',hclngdescr(-1)
        call edisp(iuout,outs)
        do ij=1,99
          if(hclngdescr(ij)(1:3).eq.'not')then
            continue
          else
            write(outs,'(i3,2a)') ij,' ',hclngdescr(ij)
            call edisp(iuout,outs)
          endif
        enddo
        goto 21
      elseif(INO.eq.4)then
        goto 21
      elseif(INO.gt.4.and.INO.lt.NITEMS-3)then

C Depending on the value of hclngdescr jump to the correct subroutine.
        ifoc=ino-4
        if(hcfpdescr(iday,ifoc)(1:39).eq.
     &    'surf-to-air deltaT via in-floor heating')then
          iopt=1  ! Regime `A'. Buoyancy caused by in-floor heating.
          call EconvA(ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:41).eq.
     &    'surf-to-air deltaT via wall panel heating')then
          iopt=2  ! Regime `A'. Buoyancy caused by heated wall panel
          call EconvA(ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:37).eq.
     &    'surf-to-air deltaT via another factor')then
          iopt=3  ! Regime `A'. Buoyancy caused by buoyancy other temp diff.
          call EconvA(ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:35).eq.
     &    'convection from heater under window')then
          iopt=1  ! Regime `B'. Buoyancy caused by heater under window.
          call EconvB(ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:40).eq.
     &    'convection from heater (not under window)')then
          iopt=2  ! Regime `B'. Buoyancy caused by heater not under window.
          call EconvB(ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:36).eq.
     &    'Molina & Maestre ventilated channels'.or.
     &    hcfpdescr(iday,ifoc)(1:27).eq.
     &    'convec. vert channel Molina')then
          iopt=1  ! Open vertical channel convection - Molina & Maestre correlation
          call EconvF (ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:38).eq.
     &    'Bar-Cohen buoyancy ventilated channels'.or.
     &    hcfpdescr(iday,ifoc)(1:30).eq.
     &    'convec. vert channel Bar-Cohen')then
          iopt=2  ! Open vertical channel convection - Bar-Cohen correlation
          call EconvF (ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:18).eq.
     &    'convec. via VAV VT')then
          call easkab('There are two possible variants of HVAC',
     &      'Options:','mech HVAC VAV with CV heating',
     &      'mech HVAC VAV with VV heating',IW,nbhelp)
          if(IW.eq.1)iopt=1
          if(IW.eq.2)iopt=2
          call EconvC(ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:16).eq.'convec. via CVVT')then
          iopt=3
          call EconvC(ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:22).eq.
     &    'mixed conv. via VAV VT')then
          call easkab('There are two possible variants of mixed HVAC',
     &      'Options:','Mixed flow VAV with CV heating',
     &      'Mixed flow VAV with VV heating',IW,nbhelp)
          if(IW.eq.1)iopt=1
          if(IW.eq.2)iopt=2
          call EconvE(ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:22).eq.
     &    'mixed convec. via CVVT')then
          iopt=3  ! Regime `E'. Mixed flow model. CV with variable temperatures.
          call EconvE(ITRU,IUF,ICOMP,iopt,iday,IER)
        elseif(hcfpdescr(iday,ifoc)(1:23).eq.
     &    'User supplied hc values'.or.
     &    hcfpdescr(iday,ifoc)(1:38).eq.
     &    'User specified convection coefficients')then
          call easkab(' ','Options:','edit period details',
     &      'manage one or more periods and set default',IW,nbhelp)
          if(IW.eq.1)then
            iopt=1
            call EPHCF(ICOMP,ifoc,NS,iopt,iday,IER)
          endif
          if(IW.eq.2)then
            iopt=1
            call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)
          endif
        elseif(hcfpdescr(iday,ifoc)(1:31).eq.
     &    'CEN user edited hc coefficients')then

C If existing offer user direct edit or general management.
          call easkab(' ','Options:','edit period details',
     &      'manage one or more periods and set default',IW,nbhelp)
          if(IW.eq.1)then
            iopt=5
            call EPHCF(ICOMP,ifoc,NS,iopt,iday,IER)
          endif
          if(IW.eq.2)then
            iopt=5
            call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)
          endif
        elseif(hcfpdescr(iday,ifoc)(1:29).eq.
     &    'User supplied hc correlations')then
          call easkab(' ','Options:','edit period details',
     &      'manage one or more periods and set default',IW,nbhelp)
          if(IW.eq.1)then
            iopt=2
            call EPHCF(ICOMP,ifoc,NS,iopt,iday,IER)
          endif
          if(IW.eq.2)then
            iopt=2
            call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)
          endif
        elseif(hcfpdescr(iday,ifoc)(1:38).eq.
     &    'User supplied adaptive hc correlations')then
          call easkab(' ','Options:','edit period details',
     &      'manage one or more periods and set default',IW,nbhelp)
          if(IW.eq.1)then
            iopt=3
            call EPHCF(ICOMP,ifoc,NS,iopt,iday,IER)
          endif
          if(IW.eq.2)then
            iopt=3
            call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)
          endif
        elseif(hcfpdescr(iday,ifoc)(1:27).eq.
     &    'CEN default hc coefficients')then
          call easkab(' ','Options:','edit period details',
     &      'manage one or more periods and set default',IW,nbhelp)
          if(IW.eq.1)then
            iopt=4
            call EPHCF(ICOMP,ifoc,NS,iopt,iday,IER)
          endif
          if(IW.eq.2)then
            iopt=4
            call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)
          endif
        elseif(hcfpdescr(iday,ifoc)(1:31).eq.
     &    'CEN user edited hc coefficients')then
          call easkab(' ','Options:','edit period details',
     &      'manage one or more periods and set default',IW,nbhelp)
          if(IW.eq.1)then
            iopt=5
            call EPHCF(ICOMP,ifoc,NS,iopt,iday,IER)
          endif
          if(IW.eq.2)then
            iopt=5
            call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)
          endif
        else

C Did not match so assume user defined hc coefficients.
          iopt=1
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)
        endif
        goto 21

      elseif(INO.eq.NITEMS-3)then

C Write data to file.
        CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)
        goto 21

      elseif(INO.eq.NITEMS-2)then

C User requests a redefintion of HC regime.
        call HCFMKHL(ITRU,IUF,ICOMP,IER)
        CALL EMKCFG('-',IER)
        goto 21

      elseif(INO.eq.NITEMS-1)then
        helptopic='zone_htc_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('convective calculations',nbhelp,'-',0,0,IER)
        goto 21
      elseif(INO.eq.NITEMS)then
        return
      else
        goto 21
      endif
      goto 21

C Error messages.
  392 call edisp(iuout,' Unable to rewind HC file.')
      return
      END


C ******************** HCFMKHL ********************
C HCFMKHL user chooses high level attributes of HC regime.

      SUBROUTINE HCFMKHL(ITRU,IUF,ICOMP,IER)
#include "building.h"
#include "model.h"
#include "help.h"
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      character hctypes(19)*32
      integer iday       ! for toggling through multiple day types

      integer IVALS(19)

      helpinsub='hcfmk'  ! set for subroutine

C Make up the list of hctypes for user to select from.
      hctypes(1) ='bouyancy via in-floor heating'
      hctypes(2) ='bouyancy via heated wall panel'
      hctypes(3) ='bouyancy via other temp diff'
      hctypes(4) ='heater under window'
      hctypes(5) ='heater not under window'
      hctypes(6) ='vertical channel - Molina'
      hctypes(7) ='vertical channel - Bar-Cohen'
      hctypes(8) ='mech HVAC VAV with CV heating'
      hctypes(9) ='mech HVAC VAV with VV heating'
      hctypes(10)='mech HVAC CV with variable temp'
      hctypes(11)='via circulation fan'
      hctypes(12)='mixed flow VAV with CV heating'
      hctypes(13)='mixed flow VAV with VV heating'
      hctypes(14)='mixed flow CV with variable temp'
      hctypes(15)='User defined hc coefficients'
      hctypes(16)='User defined hc correlations'
      hctypes(17)='User defined adaptive correlat.'
      hctypes(18)='Default CEN values'
      hctypes(19)='User edited CEN values'

C Ask some high level questions prior to creating file.
C Define number of day types, ask about which control regime.
      helptopic='zone_htc_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkab('Daytypes for heat transfer coefficients',
     &  'Options:','All days treated the same',
     &  'follow model day types',IW,nbhelp)
      if(IW.eq.1)IHCDT=1
      if(IW.eq.2)IHCDT=NBDAYTYPE
      iday=1   ! focus on the first (or all) day type
      INPICK=1
      CALL EPICKS(INPICK,IVALS,'General pattern of heat transfer',
     &  'and then define periods etc.',32,19,hctypes,
     &  'Heat transfer coefficients/correlations',IER,nbhelp)
      if(IVALS(1).eq.1)then
        iopt=1  ! Regime `A'. Buoyancy caused by in-floor heating.
        call EconvA(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.2)then
        iopt=2  ! Regime `A'. Buoyancy caused by heated wall panel
        call EconvA(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.3)then
        iopt=3  ! Regime `A'. Buoyancy caused by buoyancy other temp diff.
        call EconvA(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.4)then
        iopt=1  ! Regime `B'. Buoyancy caused by heater under window.
        call EconvB(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.5)then
        iopt=2  ! Regime `B'. Buoyancy caused by heater not under window.
        call EconvB(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.6)then
        iopt=1  ! Open vertical channel convection - Molina & Maestre correlation
        call EconvF (ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.7)then
        iopt=2  ! Open vertical channel convection - Bar-Cohen correlation
        call EconvF (ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.8)then
        iopt=1  ! VAV with CV heating.
        call EconvC(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.9)then
        iopt=2  ! Regime `C'. VAV with VV heating.
        call EconvC(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.10)then
        iopt=3  ! Regime `C'. CV with variable temperature.
        call EconvC(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.11)then
        call EconvD(ITRU,IUF,ICOMP,iday,IER)  ! Fan
      elseif(IVALS(1).eq.12)then
        iopt=1  ! Regime `E'. Mixed flow model. VAV with CV heating.
        call EconvE(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.13)then
        iopt=2  ! Regime `E'. Mixed flow model. VAV with VV heating.
        call EconvE(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.14)then
        iopt=3  ! Regime `E'. Mixed flow model. CV with variable temperatures.
        call EconvE(ITRU,IUF,ICOMP,iopt,iday,IER)
      elseif(IVALS(1).eq.15)then
        iopt=1
        call easkab(' ','Options:','set initial values',
     &    'manage one or more periods',IW,nbhelp)
        if(IW.eq.1)then
          call EHCMANINIT(ICOMP,iopt,iday,IER)
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)  ! User supplied hc values.
        else
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)  ! User supplied hc values.
        endif
      elseif(IVALS(1).eq.16)then
        iopt=2
        call easkab(' ','Options:','set initial values',
     &    'manage one or more periods',IW,nbhelp)
        if(IW.eq.1)then
          call EHCMANINIT(ICOMP,iopt,iday,IER)
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)  ! User supplied hc correlations.
        else
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)  ! User supplied hc correlations.
        endif
      elseif(IVALS(1).eq.17)then
        iopt=3
        call easkab(' ','Options:','set initial values',
     &    'manage one or more periods',IW,nbhelp)
        if(IW.eq.1)then
          call EHCMANINIT(ICOMP,iopt,iday,IER)
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)  ! User supplied adaptive hc correlations.
        else
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)  ! User supplied adaptive hc correlations.
        endif
      elseif(IVALS(1).eq.18)then
        iopt=4
        call easkab(' ','Options:','set initial values',
     &    'manage one or more periods',IW,nbhelp)
        if(IW.eq.1)then
          call EHCMANINIT(ICOMP,iopt,iday,IER)
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)  ! CEN default hc coefficients.
        else
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)  ! CEN default hc coefficients.
        endif
      elseif(IVALS(1).eq.19)then
        iopt=5
        call easkab(' ','Options:','set initial values',
     &    'manage one or more periods',IW,nbhelp)
        if(IW.eq.1)then
          call EHCMANINIT(ICOMP,iopt,iday,IER)
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)  ! CEN user edited hc coefficients.
        else
          call EHCMAN(ITRU,IUF,ICOMP,iopt,iday,IER)  ! CEN user edited hc coefficients.
        endif
      endif
      return
      end

C ******************** EconvA ********************
C EconvA controls the input of data related to convective regime A, buoyancy
C        driven flow caused by surface-to-air temperature differences.
C IOPT: 1 = in-floor heating, 2 = heated wall panel, 3 = other.
C id:   Day type to focus on.
      SUBROUTINE EconvA(ITRU,IUF,ICOMP,iopt,id,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      COMMON/HCCEN/CENLOC(MDTY,MP,14)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER SN*12,outs*124,hcfpdescr*72
      logical OK

      helpinsub='hcfmk'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP(id) = 1
      ST(id,IPER) = 0.
      EN(id,IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(id,IPER) = 3
      hcfpdescr(id,IPER) = 'surf-to-air deltaT'

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
      helptopic='zone_htc_buoyancy_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkok(' ',
     &  'Summary of bouyancy-driven flow options?',ok,nbhelp)
      if(OK)then

C Add lots of help text here.
        if(iopt.eq.1)then
          helptopic='htc_buoyancy_floor'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Cause of dT',nbhelp,'-',0,0,IER)
        elseif(iopt.eq.2)then
          helptopic='htc_buoyancy_wall_panel'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Cause of dT',nbhelp,'-',0,0,IER)
        elseif(iopt.eq.3)then
          helptopic='htc_buoyancy_temp_diff'
          call gethelptext(helpinsub,helptopic,nbhelp)
         CALL PHELPD('Cause of dT',nbhelp,'-',0,0,IER)
        endif
      endif

      IF(IOPT.EQ.1)THEN

C In-floor heating.
C When heating is on ==> Awbi-Hatton for floor, Khalifa eq 3 for walls and windows,
C and Khalifa eq 4 for ceiling.
C When heating is off ==> Accept simulation toggle (Alamdari-Hammond if defaulted).
C Examine each surface in the zone. Select ICOR based on surface orientation.
C ANGLE=0 for walls and other vertical surfaces; ANGLE=-90 for floors;
C and ANGLE=+90 for ceilings.
        hcfpdescr(id,IPER) = 'surf-to-air deltaT via in-floor heating'
        DO 10 ISUR=1,NZSUR(ICOMP)
          ANGLE = SPELV(icomp,isur)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. Set control law to 1:
            CVdata(id,IPER,ISUR,1) = 1.

C Correlation (ICOR) to use when heating on:
            CVdata(id,IPER,ISUR,2) = 6.

C Correlation to use when heating off (-1 means sim toggle):
            CVdata(id,IPER,ISUR,3) = -1.

C Remaining fields not significant for control law 1:
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = 55.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = 61.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          endif
   10   CONTINUE

       ELSEIF(IOPT.EQ.2)THEN

C Heated wall panel (e.g. hydronic wall panel).
C When heating is on ==> Awbi-Hatton for heated wall, Khalifa eq 6 for
C non-heated walls and windows, Khalifa eq 7 for ceiling, and Alamdari-Hammond
C stably stratified correlation for floor.
C When heating is off ==> Accept simulation toggle (Alamdari-Hammond if defaulted).
C Examine each surface in the zone. Select ICOR based on surface orientation.
C ANGLE=0 for walls and other vertical surfaces; ANGLE=-90 for floors;
C and ANGLE=+90 for ceilings.
        hcfpdescr(id,IPER) = 'surf-to-air deltaT via wall panel heating'
        DO 11 ISUR=1,NZSUR(ICOMP)
          ANGLE = SPELV(icomp,isur)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. See `in-floor' heating for definitions of CVdata.
C Treatment for wall depends on whether it is a heated surface.
            CVdata(id,IPER,ISUR,1) = 1.
            SN=SNAME(icomp,isur)
            helptopic='htc_buoyancy_wall_panel'
            call gethelptext(helpinsub,helptopic,nbhelp)
            WRITE(outs,'(3a)') 'Is surface ',SN(1:lnblnk(SN)),
     &                         'heated?'
            CALL EASKOK(' ',outs,OK,nbhelp)
            if(OK)then

C Awbi-Hatton for heated walls.
              CVdata(id,IPER,ISUR,2) = 14.
            else

C Khalifa eq 6 for non-heated walls.
              CVdata(id,IPER,ISUR,2) = 5.
            endif
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = 70.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = 60.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          endif
   11   CONTINUE

      ELSEIF(IOPT.EQ.3)THEN

C Surface-air temperature difference caused by another factor, such as
C heat transfer through envelope, solar insolation, or a chilled ceiling panel.
C Regardless of state of plant injection, use correlations based on simulation
C toggle (Alamdari-Hammond by default).

C Process each surface in the zone.
        hcfpdescr(id,IPER) = 'surf-to-air deltaT via another factor'
        DO 12 ISUR=1,NZSUR (ICOMP)
          CVdata(id,IPER,ISUR,1) = 1.
          CVdata(id,IPER,ISUR,2) = -1.
          CVdata(id,IPER,ISUR,3) = -1.
          CVdata(id,IPER,ISUR,4) = 0.
          CVdata(id,IPER,ISUR,5) = 99.
          CVdata(id,IPER,ISUR,6) = 99.
          CVdata(id,IPER,ISUR,7) = 99.
          CVdata(id,IPER,ISUR,8) = 99.
   12   CONTINUE
      ENDIF

C If there are multiple day types then offer to replicate
C data to subsequent day types.
      if(IHCDT.eq.NBDAYTYPE)then
        NS=NZSUR(icomp)
        CALL EASKOK('Regime follows model day types.',
     &    'Apply these values to subsequent days?',OK,nbhelp)
        if(OK)then
          do idl=id,IHCDT
            ihcfp(idl)=ihcfp(id)
            DO IG = 1,IHCFP(id)
              ST(idl,IG) = ST(id,IG)
              EN(idl,IG) = EN(id,IG)
              ICTL(idl,IG) = ICTL(id,IG)
              hcfpdescr(idl,IG) = hcfpdescr(id,IG)
              do J = 1,NS
                HCI(idl,IG,J) = HCI(id,IG,J)
                HCE(idl,IG,J) = HCE(id,IG,J)
                IHCI(idl,IG,J) = IHCI(idl,IG,J)
                IHCE(idl,IG,J) = IHCE(idl,IG,J)
                do jj=1,8
                  CVdata(idl,IG,J,jj) = CVdata(id,IG,J,jj)
                enddo
              enddo  ! of NS
              do jj=1,14
                CENLOC(idl,IG,jj)=CENLOC(id,IG,jj)
              enddo
            enddo    ! of IHCFP(id)
          enddo      ! of IHCDT
        endif
      endif

C Write .htc file then return to higher level menu.
      call edisp(iuout,'The selected HC regime is being saved.')
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

C Check if user wants to list current data.
      call edisp(iuout,'The current flow data follows...')
      call listhtc(iuout,ICOMP,'f',IER)
      return
      END


C ******************** EconvB ********************
C EconvB controls the input of data related to convective regime B, buoyancy
C        driven flow caused by a heater located within the room.
C IOPT: 1 = heater under a window, 2 = heater not under a window. 
      SUBROUTINE EconvB(ITRU,IUF,ICOMP,iopt,id,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      COMMON/HCCEN/CENLOC(MDTY,MP,14)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER SN*12,outs*124,hcfpdescr*72
      logical OK

      helpinsub='hcfmk'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP(1) = 1
      ST(id,IPER) = 0.
      EN(id,IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(id,IPER) = 3
      hcfpdescr(id,IPER) = 'heater w/in room '

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
      helptopic='zone_htc_device_summary'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkok(' ','Summary of bouyancy-driven flow options?',
     &           ok,nbhelp)
      if(OK)then

C Add lots of help text here.
        helptopic='zone_htc_device_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Location of heater',nbhelp,'-',0,0,IER)
      endif

C Establish location of heating device.
      IF(IOPT.EQ.1)THEN

C Heating device located under a window.
C When heating is on ==> Khalifa eq 6 for walls, Khalifa eq 7 for ceiling
C and Alamdari-Hammond stably stratified correlation for floors. User has
C choice of Khalifa eq 9 or eq 6 for windows.
C When heating is off ==> Accept simulation toggle (Alamdari-Hammond if defaulted).
C Examine each surface in the zone. Select ICOR based on surface orientation.
C ANGLE=0 for walls and other vertical surfaces; ANGLE=-90 for floors;
C and ANGLE=+90 for ceilings.
        hcfpdescr(id,IPER) = 'convection from heater under window'
        DO 10 ISUR=1,NZSUR(ICOMP)
          ANGLE = SPELV(icomp,isur)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. Set control law to 1:
            CVdata(id,IPER,ISUR,1) = 1.

C Treatment (ICOR) depends on whether this is a wall or window.
            if(SOTF(icomp,isur)(1:4).ne.'OPAQ')then

C It's a window. Use Khalifa window correlation or wall correlation?
C Add some help here to explain that the window correlations may
C overpredict convection.
              SN=SNAME(icomp,isur)
              WRITE(outs,'(3a)') 'Use window correlation for ',
     &                           SN(1:lnblnk(SN)),'?'
              helptopic='zone_htc_device_options'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL EASKOK(' ',outs,OK,nbhelp)
              if(OK)then
                CVdata(id,IPER,ISUR,2) = 30.
              else
                CVdata(id,IPER,ISUR,2) = 5.
              endif
            else

C It's a wall. No choice to make.
              CVdata(id,IPER,ISUR,2) = 5.
            endif

C Correlation (ICOR) to use when heating off (-1 means sim toggle):
            CVdata(id,IPER,ISUR,3) = -1.

C Remaining fields not significant for control law 1:
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = 70.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = 60.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          endif
   10   CONTINUE

       ELSEIF(IOPT.EQ.2)THEN

C Heating device in room, but device not located under window.
C When heating is on ==> Khalifa eq 5 for walls adjacent to heater, Khalifa
C eq 3 for walls not adjacent to heater, Khalifa eq 4 for ceiling
C and Alamdari-Hammond stably stratified correlation for floors. User has
C choice of Khalifa eq 10 or eq 3 for windows.
C When heating is off ==> Accept simulation toggle (Alamdari-Hammond if defaulted).
C Examine each surface in the zone. Select ICOR based on surface orientation.
C ANGLE=0 for walls and other vertical surfaces; ANGLE=-90 for floors;
C and ANGLE=+90 for ceilings.
        hcfpdescr(id,IPER) = 'convection from heater (not under window)'
        DO 11 ISUR=1,NZSUR(ICOMP) 
          ANGLE = SPELV(icomp,isur)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. See `heater under window'for definitions of CVdata.
            CVdata(id,IPER,ISUR,1) = 1.

C Treatment depends on whether this is a wall or window.
            if(SOTF(icomp,isur)(1:4).ne.'OPAQ')then

C It's a window. Use Khalifa window correlation (ICOR=31)
C or wall correlation (ICOR=6)?
C Add some help here to explain that the window correlations may
C overpredict convection.
              SN=SNAME(icomp,isur)
              WRITE(outs,'(3a)') 'Use window correlation for ',
     &                           SN(1:lnblnk(SN)),'?'
              helptopic='zone_htc_device_options'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL EASKOK(' ',outs,OK,nbhelp)
              if(OK)then
                CVdata(id,IPER,ISUR,2) = 31.
              else
                CVdata(id,IPER,ISUR,2) = 6.
              endif
            else

C It's a wall. Treatment depends on location relative to heater.
              SN=SNAME(icomp,isur)
              WRITE(outs,'(3a)') 'Is surface ',SN(1:lnblnk(SN)),
     &                           ' close to the heater?'
              helptopic='zone_htc_device_options'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL EASKOK(' ',outs,OK,nbhelp)
              if(OK)then
                CVdata(id,IPER,ISUR,2) = 7.
              else
                CVdata(id,IPER,ISUR,2) = 6.
              endif
            endif
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = 70.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = 61.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          endif
   11   CONTINUE
      ENDIF

C If there are multiple day types then offer to replicate
C data to subsequent day types.
      if(IHCDT.eq.NBDAYTYPE)then
        NS=NZSUR(icomp)
        CALL EASKOK('Regime follows model day types.',
     &    'Apply these values to subsequent days?',OK,nbhelp)
        if(OK)then
          do idl=id,IHCDT
            ihcfp(idl)=ihcfp(id)
            DO IG = 1,IHCFP(id)
              ST(idl,IG) = ST(id,IG)
              EN(idl,IG) = EN(id,IG)
              ICTL(idl,IG) = ICTL(id,IG)
              hcfpdescr(idl,IG) = hcfpdescr(id,IG)
              do J = 1,NS
                HCI(idl,IG,J) = HCI(id,IG,J)
                HCE(idl,IG,J) = HCE(id,IG,J)
                IHCI(idl,IG,J) = IHCI(idl,IG,J)
                IHCE(idl,IG,J) = IHCE(idl,IG,J)
                do jj=1,8
                  CVdata(idl,IG,J,jj) = CVdata(id,IG,J,jj)
                enddo
              enddo  ! of NS
              do jj=1,14
                CENLOC(idl,IG,jj)=CENLOC(id,IG,jj)
              enddo
            enddo    ! of IHCFP(id)
          enddo      ! of IHCDT
        endif
      endif

C Write .htc file then return to higher level menu.
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

C List summary of current data.
      call listhtc(iuout,ICOMP,'s',IER)
      return

      END


C ******************** EconvC ********************
C EconvC controls the input of data related to convective regime C, where
C        convection is dominated by mechanical forces caused by an air-handling
C        system delivering heated or cooled air to the room through ceiling
C        or wall mounted diffusers.
C IOPT: 1 = VAV with CV heating, 2 = VAV with VV heating, 3 = CV variable T.
C       4 = ??
      SUBROUTINE EconvC(ITRU,IUF,ICOMP,iopt,id,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      COMMON/HCCEN/CENLOC(MDTY,MP,14)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER outs*124,hcfpdescr*72
      character*24 hcphrase(5)
      CHARACTER*25 ITEM(15)
      logical ok
      real airflow   ! temprary value for editing
      integer NITEMS,Ioa,Idiff,Ifor ! max items and current menu item

      helpinsub='hcfmk'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP(id) = 1
      ST(id,IPER) = 0.
      EN(id,IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(id,IPER) = 3
      hcfpdescr(id,IPER) = 'mechanical dominated'

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
  20  helptopic='zone_htc_mechanical_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkok(' ','Summary of mechanically-driven flow options?',
     &            ok,nbhelp)
      if(OK)then

C Add lots of help here.
        helptopic='zone_htc_mechanical_opt'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('HVAC system type',nbhelp,'-',0,0,IER)
      endif

C Define system type for .htc file. Same system for all surfaces:
      Isystp = IOPT+1
      do 50 ISUR=1,NZSUR(ICOMP)
        CVdata(id,IPER,ISUR,1) = Isystp
   50 continue

C Get HVAC air flow rate and temperature control parameters.
      IF(Isystp.eq.4)THEN

C Constant-volume variable-temperature system. Get air flow rate,
C and minimum and maximum allowable diffuser temperatures.
        hcphrase(1) = 'convec. via CVVT'

        helptopic='zone_htc_mech_sys_4'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(outs,'(a)')' air flow rate in ac/h'
        airflow=CVdata(id,IPER,1,2)  ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,nbhelp)
        do 51 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,2) = airflow
   51   continue

        WRITE(outs,'(a)')' minimum supply air temp'
        Tmin=CVdata(id,IPER,1,4)  ! give it a typical value
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'min T',IER,nbhelp)
        do 52 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,4) = Tmin
   52   continue

        WRITE(outs,'(a)')' maximum supply air temp'
        Tmax=CVdata(id,IPER,1,3)  ! give it a typical value
        CALL EASKR(Tmax,outs,' ',
     &             5.0,'W',80.,'W',43.0,'max T',IER,nbhelp)
        do 53 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,3) = Tmax
          CVdata(id,IPER,ISUR,6) = 99.
   53   continue

      ELSE

C Variable-volume system. Get cooling setpoint temperature for supplied
C air, the maximum allowable diffuser temperature, and the minimum and
C and maximum air flow rates.
        hcphrase(1) = 'convec. via VAV VT'
        helptopic='zone_htc_mech_VAV_VT'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(outs,'(a)')' diffuser setpoint temp for cooling'
        Tmin=CVdata(id,IPER,1,2)  ! give it a typical value
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'setpoint T',IER,nbhelp)
        do 54 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,2) = Tmin
   54   continue

        WRITE(outs,'(a)')' min air flow rate in ac/h'
        airflow=CVdata(id,IPER,1,3) ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,nbhelp)
        do 55 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,3) = airflow
   55   continue

        WRITE(outs,'(a)')' max air flow rate in ac/h'
        airflow=CVdata(id,IPER,1,4) ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',9.0,'ac/h',IER,nbhelp)
        do 56 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,4) = airflow
   56   continue

        WRITE(outs,'(a)')' max diffuser temperature'
        Tmax=CVdata(id,IPER,1,6) ! give it a typical value
        CALL EASKR(Tmax,outs,' ',
     &             15.0,'W',80.,'W',43.0,'max supply T',IER,nbhelp)
        do 57 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,6) = Tmax
   57   continue
      ENDIF

C Establish how outdoor air is treated in the model.
   23 Ioa = -2
      ITEM(1) ='a no outdoor air    '
      ITEM(2) ='b infiltration      '
      ITEM(3) ='c vent from another zone'
      ITEM(4)='? Help         '
      ITEM(5)='- Exit         '
      NITEMS=5

C Help text for this menu.
      helptopic='zone_htc_ahu_outside'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU(' Outdoor air treated as:',ITEM,NITEMS,Ioa)
      if(Ioa.eq.1)then
        hcphrase(2) = 'no outdoor air'
      elseif(Ioa.eq.2)then
        hcphrase(2) = 'infiltration'
      elseif(Ioa.eq.3)then
        hcphrase(2) = 'vent from zone'
      elseif(Ioa.eq.4)then
        helptopic='zone_htc_mech_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Outdoor air treated as',nbhelp,'-',0,0,IER)
        goto 23
      elseif(Ioa.eq.5)then

C Instead of going to further detail they have asked to return
        CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)
        return
      endif

C Define outdoor air treatment for for .htc file:
      Ioa = Ioa-1
      do 58 ISUR=1,NZSUR(ICOMP)
        CVdata(id,IPER,ISUR,5) = Ioa
   58 continue

C Establish location of supply air diffusers (used to select the HC correlations).
   21 Idiff = -2
      ITEM(1) ='a ceiling diffuser  '
      ITEM(2) ='b wall diffuser     '
      ITEM(3)='? Help         '
      ITEM(4)='- Exit         '
      NITEMS=4

C Help text for this menu.
      helptopic='zone_htc_mech_diffuers'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU(' Air supplied through',ITEM,NITEMS,Idiff)

      IF(Idiff.EQ.1)THEN

C Air supplied through radial ceiling diffusers.
C When heating is on ==> User has choice of Fisher `thesis' or
C Fisher & Pedersen `ASHRAE' correlations for walls, floor, and ceiling.
C When heating is off ==> Accept simulation toggle (Alamdari-Hammond if defaulted).
C Determine whether to use Fisher `thesis' or Fisher & Pedersen `ASHRAE'
C correlations. The choice applies to all surfaces in the room.
        hcphrase(3) = 'ceil diffuser'
   22   Ifor = -2
        ITEM(1) ='a Fisher & Pedersen '
        ITEM(2) ='b Fisher thesis     '
        ITEM(3)='? Help         '
        NITEMS=3
        CALL EMENU(' Which correlations?',ITEM,NITEMS,Ifor)
        if(Ifor.eq.3)then

C Add lots of help here.
          helptopic='zone_htc_mech_diffuers'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Which correlations',nbhelp,'-',0,0,IER)
          goto 22
        endif

C Set correlation for each surface in zone. This differs from the buoyant
C convective regimes in that the ICOR value for the correlation is not set
C here. Rather, an index is written to the .htc file. This index is
C subsequently used during the time-step simulation to set ICOR. Set the 8'th
C field of CVdata to -1 to indicate that when the system is off the
C simulation toggle will determine the convection correlation to use
C (Alamdari-Hammond by default).
        DO 10 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,7) = Ifor
          CVdata(id,IPER,ISUR,8) = -1.
   10   CONTINUE

       ELSEIF(Idiff.EQ.2)THEN

C Air supplied through wall diffuser: free horizontal jet.
C Set correlation for each surface in zone.
        hcphrase(3) = 'wall diffuser'
        DO 11 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,7) = 3.
          CVdata(id,IPER,ISUR,8) = -1.
   11   CONTINUE

      ELSEIF(Idiff.EQ.3)THEN

        CALL PHELPD('Diffuser location',18,'-',0,0,IER)
        goto 21

      ELSEIF(Idiff.EQ.4)THEN

C User has not added detail, just asked to return. 
        return
      ELSE
        goto 21
      ENDIF

C Concatenate the phrases to make hcfpdescr.
      lhcp1=lnblnk(hcphrase(1))
      lhcp2=lnblnk(hcphrase(2))
      lhcp3=lnblnk(hcphrase(3))
      write(hcfpdescr(id,IPER),'(5a)') hcphrase(1)(1:lhcp1),' ',
     &  hcphrase(2)(1:lhcp2),' ',hcphrase(3)(1:lhcp3)

C If more than one day type confirm if these defaults for fixed
C correlations should be applied to subsequent days.
      if(IHCDT.eq.NBDAYTYPE)then
        NS=NZSUR(icomp)
        CALL EASKOK('Regime follows model day types.',
     &    'Apply these defaults to subsequent days?',OK,nbhelp)
        if(OK)then
          do idl=id,IHCDT
            ihcfp(idl)=ihcfp(id)
            DO IG = 1,IHCFP(id)
              ST(idl,IG) = ST(id,IG)
              EN(idl,IG) = EN(id,IG)
              ICTL(idl,IG) = ICTL(id,IG)
              hcfpdescr(idl,IG) = hcfpdescr(id,IG)
              do J = 1,NS
                HCI(idl,IG,J) = HCI(id,IG,J)
                HCE(idl,IG,J) = HCE(id,IG,J)
                IHCI(idl,IG,J) = IHCI(idl,IG,J)
                IHCE(idl,IG,J) = IHCE(idl,IG,J)
                do jj=1,8
                  CVdata(idl,IG,J,jj) = CVdata(id,IG,J,jj)
                enddo
              enddo  ! of NS
              do jj=1,14
                CENLOC(idl,IG,jj)=CENLOC(id,IG,jj)
              enddo
            enddo    ! of IHCFP(id)
          enddo      ! of IHCDT
        endif
      endif

C List current data.
      call listhtc(iuout,ICOMP,'f',IER)

C All data has been acquired. Write .htc file then return to higher
C level menu.
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

      RETURN
      END


C ******************** EconvD ********************
C EconvD controls the input of data related to convective regime D, mechanically
C        driven flow caused by a circulating fan within the room. There is no
C        intentional supply or extract of air from the room.
      SUBROUTINE EconvD(ITRU,IUF,ICOMP,id,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      COMMON/HCCEN/CENLOC(MDTY,MP,14)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER SN*12,outs*124,hcfpdescr*72
      logical OK

      helpinsub='hcfmk'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP(id) = 1
      ST(id,IPER) = 0.
      EN(id,IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(id,IPER) = 3
      hcfpdescr(id,iper) = 'convection via fan in room'

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
      helptopic='zone_htc_circ_fan_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkok(' ',
     &  'Summary of options for flow caused by a circulating fan?',
     &      ok,nbhelp)
      if(OK)then

C Display help info to inform user what this should be used for.
        helptopic='zone_htc_circ_fan_synop'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Outdoor air treated as',nbhelp,'-',0,0,IER)
      endif

C When heating is on ==> Khalifa eq 8 for walls opposite fan, Khalifa eq 3
C for other walls, Khalifa eq 4 for ceiling, and Alamdari-Hammond stably
C stratified correlation for floors.
C When heating is off ==> Accept simulation toggle (Alamdari-Hammond if defaulted).
C Examine each surface in the zone. Select ICOR based on surface orientation.
C ANGLE=0 for walls and other vertical surfaces; ANGLE=-90 for floors;
C and ANGLE=+90 for ceilings.
      DO 10 ISUR=1,NZSUR(ICOMP)
        ANGLE = SPELV(icomp,isur)
        if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. Set control law to 1:
          CVdata(id,IPER,ISUR,1) = 1.

C Treatment (ICOR) depends on whether fan blows directly on the surface.
          SN=SNAME(icomp,isur)
          WRITE(outs,'(3a)') 'Does fan blow directly on surface ',
     &                       SN(1:lnblnk(SN)),'?'
          helptopic='zone_htc_circ_fan_synop'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ',outs,OK,nbhelp)
          if(OK)then
            CVdata(id,IPER,ISUR,2) = 8.
          else
            CVdata(id,IPER,ISUR,2) = 6.
          endif

C Correlation (ICOR) to use when heating off (-1 means sim toggle):
          CVdata(id,IPER,ISUR,3) = -1.

C Remaining fields not significant for control law 1:
          CVdata(id,IPER,ISUR,4) = 0.
          CVdata(id,IPER,ISUR,5) = 99.
          CVdata(id,IPER,ISUR,6) = 99.
          CVdata(id,IPER,ISUR,7) = 99.
          CVdata(id,IPER,ISUR,8) = 99.
        elseif(ANGLE.LT.-45.) then
C Floor.
          CVdata(id,IPER,ISUR,1) = 1.
          CVdata(id,IPER,ISUR,2) = 70.
          CVdata(id,IPER,ISUR,3) = -1.
          CVdata(id,IPER,ISUR,4) = 0.
          CVdata(id,IPER,ISUR,5) = 99.
          CVdata(id,IPER,ISUR,6) = 99.
          CVdata(id,IPER,ISUR,7) = 99.
          CVdata(id,IPER,ISUR,8) = 99.
        elseif(ANGLE.GT.45.) then
C Ceiling.
          CVdata(id,IPER,ISUR,1) = 1.
          CVdata(id,IPER,ISUR,2) = 61.
          CVdata(id,IPER,ISUR,3) = -1.
          CVdata(id,IPER,ISUR,4) = 0.
          CVdata(id,IPER,ISUR,5) = 99.
          CVdata(id,IPER,ISUR,6) = 99.
          CVdata(id,IPER,ISUR,7) = 99.
          CVdata(id,IPER,ISUR,8) = 99.
        endif
   10 CONTINUE

C If more than one day type confirm if these defaults for fixed
C correlations should be applied to subsequent days.
      if(IHCDT.eq.NBDAYTYPE)then
        helpinsub='hcfmk'  ! set for subroutine
        helptopic='zone_htc_daytypes'
        call gethelptext(helpinsub,helptopic,nbhelp)
        NS=NZSUR(icomp)
        CALL EASKOK('Regime follows model day types.',
     &    'Apply these defaults to subsequent days?',OK,nbhelp)
        if(OK)then
          do idl=id,IHCDT
            ihcfp(idl)=ihcfp(id)
            DO IG = 1,IHCFP(id)
              ST(idl,IG) = ST(id,IG)
              EN(idl,IG) = EN(id,IG)
              ICTL(idl,IG) = ICTL(id,IG)
              hcfpdescr(idl,IG) = hcfpdescr(id,IG)
              do J = 1,NS
                HCI(idl,IG,J) = HCI(id,IG,J)
                HCE(idl,IG,J) = HCE(id,IG,J)
                IHCI(idl,IG,J) = IHCI(idl,IG,J)
                IHCE(idl,IG,J) = IHCE(idl,IG,J)
                do jj=1,8
                  CVdata(idl,IG,J,jj) = CVdata(id,IG,J,jj)
                enddo
              enddo  ! of NS
              do jj=1,14
                CENLOC(idl,IG,jj)=CENLOC(id,IG,jj)
              enddo
            enddo    ! of IHCFP(id)
          enddo      ! of IHCDT
        endif
      endif

C List current data.
      call listhtc(iuout,ICOMP,'f',IER)

C Write .htc file before returning.
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

      RETURN
      END


C ******************** EconvE ********************
C EconvE controls the input of data related to convective regime E, mixed
C        flow. Currently this is limited to rooms conditioned by HVAC systems
C        supplying air through ceiling-mounted diffusers, and in which buoyancy
C        forces are generated by surface-to-air temperature differences.
C IOPT: 1 = VAV with CV heat, 2 = VAV with VV heating, 3 = CV with var temp.
      SUBROUTINE EconvE(ITRU,IUF,ICOMP,iopt,id,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER outs*124,hcfpdescr*72
      character*24 hcphrase(5)
      CHARACTER*25 ITEM(15)
      logical ok
      integer NITEMS,Ioa ! max items and current menu item

      helpinsub='hcfmk'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP(id) = 1
      ST(id,IPER) = 0.
      EN(id,IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(id,IPER) = 3
      hcfpdescr(id,iper) = 'mixed flow ceil difusers'

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
      helptopic='zone_htc_mixed_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkok(' ',
     &  'Summary of bouyancy-driven and mechanical flow options?',ok,2)
      if(OK)then

C Add lots of help here.
        helptopic='zone_htc_mixed_opts'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Mixed convection',nbhelp,'-',0,0,IER)
      endif

C Establish HVAC system type.
   20 continue

C Define system type for .htc file. Same system for all surfaces:
      Isystp = IOPT+1
      do 50 ISUR=1,NZSUR(ICOMP)
        CVdata(id,IPER,ISUR,1) = Isystp
   50 continue

C Get HVAC air flow rate and temperature control parameters.
      IF(Isystp.eq.4)THEN

C Constant-volume variable-temperature system. Get air flow rate,
C and minimum and maximum allowable diffuser temperatures.
        hcphrase(1) = 'mixed convec. via CVVT'
        helptopic='zone_htc_CVVT'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(outs,'(a)')' air flow rate in ac/h'
        airflow=CVdata(id,IPER,1,2)   ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,3)
        do 51 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,2) = airflow
   51   continue

        WRITE(outs,'(a)')' minimum supply air temp'
        Tmin=CVdata(id,IPER,1,4)   ! give it a typical value 
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'min T',IER,nbhelp)
        do 52 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,4) = Tmin
   52   continue

        WRITE(outs,'(a)')' maximum supply air temp'
        Tmax=CVdata(id,IPER,1,3)   ! give it a typical value 
        CALL EASKR(Tmax,outs,' ',
     &             5.0,'W',80.,'W',43.0,'max T',IER,nbhelp)
        do 53 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,3) = Tmax
          CVdata(id,IPER,ISUR,6) = 99.
   53   continue

      ELSE

C Variable-volume system. Get cooling setpoint temperature for supplied
C air, the maximum allowable diffuser temperature, and the minimum and
C and maximum air flow rates.
        hcphrase(1) = 'mixed conv. via VAV VT'
        helptopic='zone_htc_VAVVT'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(outs,'(a)')' diffuser setpoint temp for cooling'
        Tmin=CVdata(id,IPER,1,2)   ! give it a typical value 
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'setpoint T',IER,nbhelp)
        do 54 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,2) = Tmin
   54   continue

        WRITE(outs,'(a)')' min air flow rate in ac/h'
        airflow=CVdata(id,IPER,1,3)  ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,nbhelp)
        do 55 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,3) = airflow
   55   continue

        WRITE(outs,'(a)')' max air flow rate in ac/h'
        airflow = CVdata(id,IPER,1,4)  ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',9.0,'ac/h',IER,nbhelp)
        do 56 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,4) = airflow
   56   continue

        WRITE(outs,'(a)')' max diffuser temperature'
        Tmax=CVdata(id,IPER,1,6)  ! give it a typical value
        CALL EASKR(Tmax,outs,' ',
     &             15.0,'W',80.,'W',43.0,'max supply T',IER,nbhelp)
        do 57 ISUR=1,NZSUR(ICOMP)
          CVdata(id,IPER,ISUR,6) = Tmax
   57   continue
      ENDIF

C Establish how outdoor air is treated in the model.
   23 Ioa = -2
      ITEM(1) ='a no outdoor air    '
      ITEM(2) ='b infiltration      '
      ITEM(3) ='c vent from another zone'
      ITEM(4)='? Help         '
      ITEM(5)='- Exit         '
      NITEMS=5

C Help text for the menu.
      helptopic='zone_htc_ahu_outside'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU(' Outdoor air treated as:',ITEM,NITEMS,Ioa)
      if(Ioa.eq.1)then
        hcphrase(2) = 'no outdoor air'
      elseif(Ioa.eq.2)then
        hcphrase(2) = 'infiltration'
      elseif(Ioa.eq.3)then
        hcphrase(2) = 'vent from zone'
      elseif(Ioa.eq.4)then
        helptopic='zone_htc_ahu_outside'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Outdoor air treated as',nbhelp,'-',0,0,IER)
        goto 23
      elseif(Ioa.eq.5)then

C Save current information and return.
        CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)
        return
      endif

C Define outdoor air treatment for for .htc file:
      Ioa = Ioa-1
      do 58 ISUR=1,NZSUR(ICOMP)
        CVdata(id,IPER,ISUR,5) = Ioa
   58 continue

C When heating is on ==> Use mixed flow model.
C When heating is off ==> Accept simulation toggle (Alamdari-Hammond if defaulted).
C Set correlation for each surface in zone. This differs from the buoyant
C convective regimes in that the ICOR value for the correlation is not set
C here. Rather, an index is written to the .htc file. This index is
C subsequently used during the time-step simulation to set ICOR. Set the 8'th
C field of CVdata to -1 to indicate that when the system is off the
C simulation toggle will determine the convection correlation to use
C (Alamdari-Hammond by default).
      DO 10 ISUR=1,NZSUR(ICOMP)
        CVdata(id,IPER,ISUR,7) = 4.
        CVdata(id,IPER,ISUR,8) = -1.
   10 CONTINUE

C Concatenate the phrases to make hcfpdescr.
      lhcp1=lnblnk(hcphrase(1))
      lhcp2=lnblnk(hcphrase(2))
      lhcp3=lnblnk(hcphrase(3))
      write(hcfpdescr(id,IPER),'(5a)') hcphrase(1)(1:lhcp1),' ',
     &  hcphrase(2)(1:lhcp2),' ',hcphrase(3)(1:lhcp3)

C List summary of current data.
      call listhtc(iuout,ICOMP,'s',IER)

C All data has been acquired. Write .htc file then return to higher
C level menu.
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

      RETURN
      END


C ******************** EHCMAN ********************
C EHCMAN controls manual creation/editing of convection file.
C If iopt is non-zero then user is being specific.
      SUBROUTINE EHCMAN(ITRU,IUF,ICOMP,iopt,id,IER)
#include "building.h"
#include "model.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32

      character outs*124,hcfpdescr*72,KEY*1
      CHARACTER*48 ITEM(22)
      integer NITEMS,INO ! max items and current menu item
      real valin,valout ! for initial values

C Short and long descriptions of correlations.
      character hcshtdescr*32  ! short description
      character hclngdescr*48   ! long description
      dimension hcshtdescr(-1:99),hclngdescr(-1:99)

      helpinsub='hcfmk'  ! set for subroutine

   20 INO = -2
      ITEM(1) ='a Initialise fixed hc values '
      write(ITEM(2),'(a,a)')  '  Day: ',CALENTAG(id)
      write(ITEM(3),'(a,I2)') '  Periods: ',IHCFP(id)
      ITEM(4) =               '  ---- time ---  ---- type ----- '
      m=4; n=0
      do loop=1,IHCFP(id)
        n=n+1
        CALL EMKEY(loop+1,KEY,IER)
        write(item(m+n),'(2a,f6.2,f6.2,2a)') key,' ',ST(id,loop),
     &     EN(id,loop),' ',hcfpdescr(id,loop)(1:32)
      enddo
      ITEM(m+n+1) ='  _________________________  '
      ITEM(m+n+2) ='1 Add a period               '
      ITEM(m+n+3) ='3 Delete a period            '
      ITEM(m+n+4) ='* Find hc (equivalent m/s)   '
      ITEM(m+n+5) ='! List current data          '
      ITEM(m+n+6) =': List available correlations'
      ITEM(m+n+7) ='> Save                       '
      ITEM(m+n+8) ='? Help                       '
      ITEM(m+n+9) ='- Exit                       '
      NITEMS=m+n+9

C Help text for this menu.
      helptopic='zone_htc_manual_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU(' User defined or CEN values',ITEM,NITEMS,INO)
      IF(INO.EQ.1)then

C Create from scratch. Initialise values to default value before
C calling editing routine. If fixed coefficients allow user to
C specify inside or outside or both values, otherwise set to default.
        call EHCMANINIT(ICOMP,iopt,id,IER)
        goto 20  ! redisplay the menue

      ELSEIF(INO.EQ.NITEMS-7)then

C Add control period. Ask about defaults or not.
        if(IHCFP(id).lt.MBP)then

C Confirm use of default Alamdahri or an alternative for inside hc.
          CALL EASKABCD('Initial HC assumptions for all hours.',
     &    'Options:','default inside & outside',
     &    'user inside default outside','user inside and outside',
     &    'cancel',INZOPT,nbhelp)
          if(INZOPT.eq.1)then
            valin= -1.0
            valout= -1.0
          elseif(INZOPT.eq.2)then
            valin= -1.0
            CALL EASKR(valin,' ','Initial inside coef [W/m2/degC]:',
     &        -1.,'F',1000.,'F',-1.0,'inside hc',IER,nbhelp)
            valout= -1.0
          elseif(INZOPT.eq.3)then
            valin= -1.0
            CALL EASKR(valin,' ','Initial inside coef [W/m2/degC]:',
     &        -1.,'F',1000.,'F',-1.0,'inside hc',IER,nbhelp)
            valout= -1.0
            CALL EASKR(valout,' ','Initial outside coef [W/m2/degC]:',
     &        -1.,'F',1000.,'F',-1.0,'outside hc',IER,nbhelp)
          elseif(INZOPT.eq.4)then
            goto 20
          endif
          IHCFP(id) = IHCFP(id) + 1
          IN = IHCFP(id)
          do 252 J = 1,NS
            HCI(id,IN,J)= valin
            HCE(id,IN,J)= valout
            IHCI(id,IN,J) = -1
            IHCE(id,IN,J) = -1
            do 254 jj=1,8
              CVdata(id,IN,J,jj) = 0.
  254       continue
  252     continue
          call EPHCF(ICOMP,IN,NS,iopt,id,IER)
        else
          call edisp(iuout,' File already contains 3 control periods.')
        endif

      elseif(INO.gt.4.and.INO.lt.NITEMS-8)then

        in=ino-4  ! Edit control period.
        call EPHCF(ICOMP,IN,NS,iopt,id,IER)

      ELSEIF(INO.EQ.NITEMS-6)then

C Delete control period.
        CALL EASKI(IN,' ',' Delete which control period ? ',
     &          1,'F',IHCFP(id),'F',1,'delete period',IERI,nbhelp)
        if(ieri.eq.-3)then
          goto 20
        endif
        IF (IN .EQ. IHCFP(id))  GOTO  790
        J01=IN+1
        DO 740 I=J01,IHCFP(id)
          ST(id,I-1)=ST(id,I)
          EN(id,I-1)=EN(id,I)
          DO 750 K=1,NS
            HCI(id,I-1,K)=HCI(id,I,K)
            HCE(id,I-1,K)=HCE(id,I,K)
            IHCI(id,I-1,K) = IHCI(id,I,K)
            IHCE(id,I-1,K) = IHCE(id,I,K)
            do 255 jj=1,8
              CVdata(id,I-1,K,jj) = CVdata(id,I,K,jj)
  255       continue
  750     CONTINUE
  740   CONTINUE
  790   IHCFP(id) = IHCFP(id) -1

      ELSEIF(INO.EQ.NITEMS-5)then

C Given a velocity, give approx hc for flow || with face.
        V1=0.15
        CALL EASKR(V1,' ','Air velocity (m/s): ',
     &    0.,'F',24.,'F',0.15,'air velocity',IER,nbhelp)
        V=V1
        HCOFPAR=2.8+3.0*V
        V=.5*V1
        IF(V.GT.0.5)V=0.5
        IF(V1.GT.2.)V=.25*V1
        HCOFPER=2.8+3.0*V
        call edisp(iuout,' ')
        write(outs,'(a,f8.4)') 'APPROXIMATE! Hc for an air velocity ',
     &    V1
        call edisp(iuout,outs)
        write(outs,'(a,f11.3)') 'Hc air flow parallel to surf ',HCOFPAR
        call edisp(iuout,outs)
        write(outs,'(a,f11.3)')'Hc air flow perpendic to surf ',HCOFPER
        call edisp(iuout,outs)
        call edisp(iuout,
     &   '(for comparison) Alamdari & Hammond for 1deg DT, 2 metre ht')
        DT=1.0
        HT = 2.
        AH=((1.50*((DT/HT)**0.25))**6.0
     &      +(1.23*((DT)**(1.0/3.0)))**6.0)**(1.0/6.0)
        write(outs,'(f11.3,a)') AH,' vertical surface'
        call edisp(iuout,outs)
        AH=((1.4*((DT/HT)**0.25))**6.0
     &      +(1.63*(DT**(1.0/3.0)))**6.0)**(1.0/6.0)
        write(outs,'(f11.3,a)') AH,' upwards flow'
        call edisp(iuout,outs)
        AH=0.60*((DT/(HT**2.0))**0.2)
        write(outs,'(f11.3,a)') AH,' downwards flow'
        call edisp(iuout,outs)

      ELSEIF(INO.EQ.NITEMS-4)then

C List HC control data.
        call listhtc(iuout,ICOMP,'f',IER)

      ELSEIF(INO.EQ.NITEMS-3)then

C Recover and list out the available correlations.
        call gethccornames(hcshtdescr,hclngdescr)
        call edisp(iuout,'Index description')
        write(outs,'(2a)') ' -1 ',hclngdescr(-1)
        call edisp(iuout,outs)
        do ij=1,99
          if(hclngdescr(ij)(1:3).eq.'not')then
            continue
          else
            write(outs,'(i3,2a)') ij,' ',hclngdescr(ij)
            call edisp(iuout,outs)
          endif
        enddo

      ELSEIF(INO.EQ.NITEMS-2)then

C Write data to file.
        CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)
      ELSEIF (INO.EQ.NITEMS-1)then

        helptopic='zone_htc_manual_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('conv coef data',nbhelp,'-',0,0,IER)

      ELSEIF (INO.EQ.NITEMS)then
        RETURN
      else
        goto 20
      endif
      goto 20

      END


C ******************** EHCMANINIT ********************
C EHCMANINIT initializes creation of user defined hc coef./corel
C If iopt is non-zero then user is being specific.
      SUBROUTINE EHCMANINIT(ICOMP,iopt,id,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      COMMON/HCCEN/CENLOC(MDTY,MP,14)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      character hcfpdescr*72
      integer ihcfpt    ! for local editing.
      real valin,valout ! for initial values
      character msg*42
      logical close,ok

      helpinsub='hcfmk'  ! set for subroutine

C Create from scratch. Initialise values to default value before
C calling editing routine. If fixed coefficients allow user to
C specify inside or outside or both values, otherwise set to default.
      helptopic='zone_user_htc_init'
      call gethelptext(helpinsub,helptopic,nbhelp)
      valin= -1.0
      valout= -1.0
      ihcfpt=ihcfp(id)
      CALL EASKI(ihcfpt,' ','Number of control periods?',
     &  1,'F',MBP,'F',1,'no periods',IERI,nbhelp)
      if(ieri.eq.-3)then
        return
      elseif(ieri.eq.0)then
        ihcfp(id)=ihcfpt

        do L=1,ihcfpt ! Ask for periods.
          write(msg,'(a,i1,a)') 'Period ',L,' start time?'
          V1=ST(id,L)
          CALL EASKR(V1,' ',msg,
     &      0.,'F',24.,'F',0.,'start',IER,nbhelp)
          ST(id,L)=V1
          V2=EN(id,L)
          call eclose(V2,0.0,0.001,close)
          if(close) V2=24.
          write(msg,'(a,i1,a)') 'Period ',L,' end time?'
          CALL EASKR(V2,' ',msg,
     &      V1,'F',24.,'F',0.,'start',IER,nbhelp)
          EN(id,L)=V2
        enddo
      endif
      NS=NZSUR(icomp)
      IW=1   ! assume fixed coefficients.
      if(iopt.eq.0)then
        msg='no coefficients/correlations selected'
      elseif(iopt.eq.1)then
        msg='suggest: user supplied hc values'
      elseif(iopt.eq.2)then
        msg='suggest: user supplied hc correlations'
      elseif(iopt.eq.3)then
        msg='suggest: adaptive hc correlations'
      elseif(iopt.eq.4)then
        msg='suggest: CEN default hc coefficients'
      elseif(iopt.eq.5)then
        msg='suggest: CEN user edited hc coefficients'
      endif
        
      CALL EASKATOG(msg,'Options:',
     &  'fixed coefficients','fixed correl.','adaptive correl.',
     &  'CEN coefficients','cancel',' ',' ',IW,nbhelp)
      if(IW.eq.2.or.IW.eq.3.or.IW.eq.4)then
        valin= -1.0
        valout= -1.0
      elseif(iw.eq.5)then
        return
      else

C Confirm use of default Alamdahri or an alternative for inside hc.
        CALL EASKABCD('Initial HC assumptions for all hours.',
     &    'Options:','default inside & outside',
     &    'user inside default outside','user inside and outside',
     &    'cancel',INZOPT,nbhelp)
        if(INZOPT.eq.1)then
          valin= -1.0
          valout= -1.0
        elseif(INZOPT.eq.2)then
          valin= -1.0
          CALL EASKR(valin,' ','Initial inside coef [W/m2/degC]:',
     &      -1.,'F',1000.,'F',-1.0,'inside hc',IER,nbhelp)
          valout= -1.0
        elseif(INZOPT.eq.3)then
          valin= -1.0
          CALL EASKR(valin,' ','Initial inside coef [W/m2/degC]:',
     &      -1.,'F',1000.,'F',-1.0,'inside hc',IER,nbhelp)
            valout= -1.0
          CALL EASKR(valout,' ','Initial outside coef [W/m2/degC]:',
     &      -1.,'F',1000.,'F',-1.0,'outside hc',IER,nbhelp)
        elseif(INZOPT.eq.4)then
          ihcfp=0  ! reset
          return
        endif
      endif

C For each period set inside and outside coefficients, clear
C CVdata array.
      DO 250 I = 1,IHCFP(id)
        ICTL(id,I) = iw
        if(iopt.eq.1)then
          hcfpdescr(id,I) = 'User supplied hc values'
        elseif(iopt.eq.2)then
          hcfpdescr(id,I) = 'User supplied hc correlations'
        elseif(iopt.eq.3)then
          hcfpdescr(id,I) = 'User supplied adaptive hc correlations'
        elseif(iopt.eq.4)then
          hcfpdescr(id,I) = 'CEN default hc coefficients'
        elseif(iopt.eq.5)then
          hcfpdescr(id,I) = 'CEN user edited hc coefficients'
        else
          hcfpdescr(id,I) = 'User specified convection coefficients'
        endif
        do J = 1,NS
          HCI(id,I,J) = valin
          HCE(id,I,J) = valout
          IHCI(id,I,J) = -1
          IHCE(id,I,J) = -1
          do jj=1,8
            CVdata(id,I,J,jj) = 0.
          enddo
          if(iopt.eq.4)then  ! set default CEN values
            CENLOC(id,i,3)=5.0; CENLOC(id,i,4)=0.7
            CENLOC(id,i,5)=8.0; CENLOC(id,i,6)=5.0
            CENLOC(id,i,7)=0.7; CENLOC(id,i,8)=8.0
            CENLOC(id,i,9)=5.0; CENLOC(id,i,10)=0.7
            CENLOC(id,i,11)=8.0; CENLOC(id,i,12)=2.5
            CENLOC(id,i,13)=2.5; CENLOC(id,i,14)=8.0
          endif
        enddo  ! of NS
        if(iopt.eq.4.or.iopt.eq.5)then  ! set CEN base & top
          IS=nint(CENLOC(id,I,1))
          CALL EASKSUR(ICOMP,IS,'A',
     &      'Select surface for base of room.',
     &      '(should be roughly horizontal)',IER)
          if(IS.ne.0)then
            CENLOC(id,I,1)=real(IS)
          endif
          IS=nint(CENLOC(id,I,2))
          CALL EASKSUR(ICOMP,IS,'A',
     &      'Select surface for top of room.',
     &      '(should be roughly horizontal)',IER)
          if(IS.ne.0)then
            CENLOC(id,I,2)=real(IS)
          endif
        endif

C If more than one day type confirm if these defaults for fixed
C correlations should be applied to subsequent days.
        if(IHCDT.eq.NBDAYTYPE)then
          CALL EASKOK('Regime follows model day types.',
     &      'Apply these defaults to subsequent days?',OK,nbhelp)
          if(OK)then
            do idl=id,IHCDT
              ihcfp(idl)=ihcfp(id)
              DO IG = 1,IHCFP(id)
                ST(idl,IG) = ST(id,IG)
                EN(idl,IG) = EN(id,IG)
                ICTL(idl,IG) = ICTL(id,IG)
                hcfpdescr(idl,IG) = hcfpdescr(id,IG)
                do J = 1,NS
                  HCI(idl,IG,J) = HCI(id,IG,J)
                  HCE(idl,IG,J) = HCE(id,IG,J)
                  IHCI(idl,IG,J) = IHCI(idl,IG,J)
                  IHCE(idl,IG,J) = IHCE(idl,IG,J)
                  do jj=1,8
                    CVdata(idl,IG,J,jj) = CVdata(id,IG,J,jj)
                  enddo
                enddo  ! of NS
                do jj=1,14
                  CENLOC(idl,IG,jj)=CENLOC(id,IG,jj)
                enddo
              enddo    ! of IHCFP(id)
            enddo      ! of IHCDT
          endif
        endif
  250 CONTINUE
      call edisp(iuout,' ')
      call edisp(iuout,'Initial values not yet saved to the hc file.')
      return  ! return to the menue
      end

C ******************** EconvF ********************
C EconvF controls the input of data related to convective regime F, buoyancy
C        driven flow in open vertical channels.
C Derived from code in EconvA.
C IOPT: 1 = Molina & Maestre, 2 = Bar-Cohen correlation.
      SUBROUTINE EconvF(ITRU,IUF,ICOMP,IOPT,id,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C IAORZ is the air origin zone. '0' if outdoor air, zone number otherwise.
C CWIDTH and CHEIGHT are the channel width and height
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      COMMON/HCCEN/CENLOC(MDTY,MP,14)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      CHARACTER outs*124
      character hcfpdescr*72
      character*24 hcphrase(5)
      logical ok

      helpinsub='hcfmk'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP(id) = 1
      ST(id,IPER) = 0.
      EN(id,IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(id,IPER) = 3

C: Initalize channel variables
      IAORZ=-1
      CWIDTH=0.0
      CHEIGHT=0.0

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
      helptopic='zone_htc_vert_chan_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkok('Summary of options for bouyancy-driven flow',
     &  'in open vertical channel?',ok,nbhelp)
      if(OK)then

C Context help for vertical channel.
        helptopic='zone_htc_vert_chan_data'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Channel correlations',nbhelp,'-',0,0,IER)
      endif

C Establish the cause of the buoyancy driving force.
C Choose where is the air comming into the channel.
      helptopic='zone_htc_vert_chan_data'
      call gethelptext(helpinsub,helptopic,nbhelp)
      IF((IOPT.EQ.1).OR.(IOPT.EQ.2)) THEN
        CALL EASKAB(' ','Air enters channel from','Outdoor',
     &    'Another zone',IW,nbhelp)
        IF (IW.EQ.1) THEN
          IAORZ=0
          hcphrase(3) = 'from outdoor'
        ELSE
          CALL ASKZONE(IAORZ,0,
     &      'Air origin zone','-','A zone must be selected',32,ier)
        ENDIF
        IF (IAORZ.EQ.ICOMP) THEN
          CALL USRMSG('This selection is typical of CLOSED channels',
     &      '  ','W')  
        ENDIF
        write(outs,'(A,I2)')'The air enters from zone number ',IAORZ
        call edisp(itru,outs)  
        hcphrase(3) = 'from other zone'
      ENDIF

      IF(IOPT.EQ.1)THEN

C Molina & Maestre correlation developed in the SOLVENT project.
C The control law ICOR= 15 will be assigned to all surfaces in the channel
        hcphrase(1) = 'convec. vert channel'
        hcphrase(2) = 'Molina'
        DO 10 ISUR=1,NZSUR(ICOMP)
          ANGLE = SPELV(icomp,isur)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. Apply correlation 
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = 15.
            CVdata(id,IPER,ISUR,3) = 15.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
           elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = -1.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = -1.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          endif
 10     CONTINUE

      ELSEIF(IOPT.EQ.2)THEN

C Bar-Cohen and Roshenow
C The correlation will be assigned to the vertical surfaces
C independently of the heating being turned on of off
C This correlation will have an ICOR of 16.
        hcphrase(1) = 'convec. vert channel'
        hcphrase(2) = 'Bar-Cohen'

C Ask Channel width
        CALL EASKR(CWIDTH,'Channel width (m)?',
     &    ' ',0.01,'W',1.0,'W',0.05,'Channel width',IER,nbhelp) 

C Ask Channel height
        CALL EASKR(CHEIGHT,'Channel height ?',
     &    ' ',0.1,'W',100.0,'W',1.0,'Channel height',IER,nbhelp) 

C Examine each surface in the zone. Select ICOR based on surface orientation.
C ANGLE=0 for walls and other vertical surfaces; ANGLE=-90 for floors;
C and ANGLE=+90 for ceilings.
        DO 11 ISUR=1,NZSUR(ICOMP)
          ANGLE = SPELV(icomp,isur)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. Apply correlation 
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = 16.
            CVdata(id,IPER,ISUR,3) = 16.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = -1.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(id,IPER,ISUR,1) = 1.
            CVdata(id,IPER,ISUR,2) = -1.
            CVdata(id,IPER,ISUR,3) = -1.
            CVdata(id,IPER,ISUR,4) = 0.
            CVdata(id,IPER,ISUR,5) = 99.
            CVdata(id,IPER,ISUR,6) = 99.
            CVdata(id,IPER,ISUR,7) = 99.
            CVdata(id,IPER,ISUR,8) = 99.
          endif
   11   CONTINUE
      ENDIF

C Concatenate the phrases to make hcfpdescr.
      lhcp1=lnblnk(hcphrase(1))
      lhcp2=lnblnk(hcphrase(2))
      lhcp3=lnblnk(hcphrase(3))
      write(hcfpdescr(id,IPER),'(5a)') hcphrase(1)(1:lhcp1),' ',
     &  hcphrase(2)(1:lhcp2),' ',hcphrase(3)(1:lhcp3)

C If there are multiple day types then offer to replicate
C data to subsequent day types.
      if(IHCDT.eq.NBDAYTYPE)then
        NS=NZSUR(icomp)
        CALL EASKOK('Regime follows model day types.',
     &    'Apply these values to subsequent days?',OK,nbhelp)
        if(OK)then
          do idl=id,IHCDT
            ihcfp(idl)=ihcfp(id)
            DO IG = 1,IHCFP(id)
              ST(idl,IG) = ST(id,IG)
              EN(idl,IG) = EN(id,IG)
              ICTL(idl,IG) = ICTL(id,IG)
              hcfpdescr(idl,IG) = hcfpdescr(id,IG)
              do J = 1,NS
                HCI(idl,IG,J) = HCI(id,IG,J)
                HCE(idl,IG,J) = HCE(id,IG,J)
                IHCI(idl,IG,J) = IHCI(idl,IG,J)
                IHCE(idl,IG,J) = IHCE(idl,IG,J)
                do jj=1,8
                  CVdata(idl,IG,J,jj) = CVdata(id,IG,J,jj)
                enddo
              enddo  ! of NS
              do jj=1,14
                CENLOC(idl,IG,jj)=CENLOC(id,IG,jj)
              enddo
            enddo    ! of IHCFP(id)
          enddo      ! of IHCDT
        endif
      endif

C Write .htc file then return to higher level menu.
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)
      RETURN

      END

C ******************** EPHCF ********************
C EPHCF edits one period of convection-calculation control data.
C IZ is the zone.
C IN is the period index.
C NS is the number of surface in the zone.
C IOPT is user topic preference.
      SUBROUTINE EPHCF(IZ,IN,NS,iopt,id,IER)
#include "epara.h"
#include "building.h"
#include "geometry.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      COMMON/HCCEN/CENLOC(MDTY,MP,14)
      character hcshtdescr*32  ! short description
      character hclngdescr*48   ! long description
      dimension hcshtdescr(-1:99),hclngdescr(-1:99)
      character outs*124,SN*12,SNH*16,ITEM(MS+5)*34,KEY*1
      CHARACTER stuff*10,outs2*124
      character hcfpdescr*72
      character msg*42
      logical close,ok
      integer IW  ! for radio button
      integer NITEMS,INO ! max items and current menu item
      integer ivals(99)

      helpinsub='hcfmk'  ! set for subroutine

      NS=NZSUR(IZ)

      helptopic='zone_htc_control_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Recover and list out the available correlations.
      call gethccornames(hcshtdescr,hclngdescr)
      if(IN.eq.0)then
        call edisp(iuout,'Period is unknown. Returning')
        return
      endif

C Get time period for control interval.
      write(msg,'(a,i1,a)') 'Period ',IN,' start time?'
      V1=ST(id,IN)
      CALL EASKR(V1,' ',msg,
     &          0.,'F',24.,'F',0.,'start',IER,nbhelp)
      ST(id,IN)=V1
      V2=EN(id,IN)
      call eclose(V2,0.0,0.001,close)
      if(close) V2=24.
      write(msg,'(a,i1,a)') 'Period ',IN,' end time?'
      CALL EASKR(V2,' ',msg,
     &          V1,'F',24.,'F',0.,'start',IER,nbhelp)
      EN(id,IN)=V2

  19  continue

C Determine type of control over calculations.
      helptopic='zone_htc_control_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)
      IW=1
      if(iopt.eq.0)then
        msg='no coefficients/correlations yet'
      elseif(iopt.eq.1)then
        msg='suggest: user supplied hc values'
      elseif(iopt.eq.2)then
        msg='suggest: user supplied hc correlations'
      elseif(iopt.eq.3)then
        msg='suggest: adaptive hc correlations'
      elseif(iopt.eq.4)then
        msg='suggest: CEN default hc coefficients'
      elseif(iopt.eq.5)then
        msg='suggest: CEN user edited hc coefficients'
      endif
        
      CALL EASKATOG(msg,'Options: ',
     &  'fixed coefficients ','fixed correl.','adaptive correl.',
     &  'CEN coefficients ','cancel',' ',' ',IW,nbhelp)
      IF(iw.eq.5) RETURN
      ICTL(id,IN) = iw

C There is one IF ENDIF block for each of the options.
      IF(iw.eq.1) THEN

C Fixed coefficients control.
        hcfpdescr(id,IN) = 'User supplied hc values'

C Confirm use of default Alamdahri or an alternative for inside hc.
        CALL EASKABCD('Initial HC assumptions for THIS PERIOD.',
     &    'Options:','default inside & outside',
     &    'user inside default outside','user inside and outside',
     &    'accept current',INZOPT,nbhelp)
        if(INZOPT.eq.1)then
          valin= -1.0
          valout= -1.0
        elseif(INZOPT.eq.2)then
          valin= -1.0
          CALL EASKR(valin,' ','Initial inside coef [W/m2/degC]:',
     &      -1.,'F',1000.,'F',-1.0,'inside hc',IER,nbhelp)
          valout= -1.0
        elseif(INZOPT.eq.3)then
          valin= -1.0
          CALL EASKR(valin,' ','Initial inside coef [W/m2/degC]:',
     &      -1.,'F',1000.,'F',-1.0,'inside hc',IER,nbhelp)
          valout= -1.0
          CALL EASKR(valout,' ','Initial outside coef [W/m2/degC]:',
     &      -1.,'F',1000.,'F',-1.0,'outside hc',IER,nbhelp)
        elseif(INZOPT.eq.4)then
          goto 42
        endif
        do J = 1,NS
          HCI(id,IN,J)= valin
          HCE(id,IN,J)= valout
          IHCI(id,IN,J) = -1
          IHCE(id,IN,J) = -1
          do jj=1,8
            CVdata(id,IN,J,jj) = 0.
          enddo
        enddo

C Initialise zone vertex menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
  42    MHEAD=1
        MCTL=3
        ILEN=NS
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Initial menu entry setup.
  20    IER=0
        ILEN=NS
        INO=-2

C Loop through the items until the page to be displayed. M is the
C current menu line index. Build up text strings for the menu.
        ITEM(1) ='  Surface   orient.  hci  hce'
    3   M=MHEAD
        DO 10 L=1,ILEN
          IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
            M=M+1
            CALL EMKEY(L,KEY,IER)
            WRITE(ITEM(M),'(a,1x,3a,2f7.2)') KEY,SNAME(iz,l),' ',
     &        SVFC(iz,l),HCI(id,IN,L),HCE(id,IN,L)
          ENDIF
   10   CONTINUE

C Number of actual items displayed.
        NITEMS=M+MCTL

C If a long list include page facility text.
        IF(IPFLG.EQ.0)THEN
          ITEM(M+1) ='  __________________________ '
        ELSE
          WRITE(ITEM(M+1),15)IPM,MPM
   15     FORMAT   ('0 page: ',I2,' of ',I2,' --------')
        ENDIF
        ITEM(M+2) ='? Help                  '
        ITEM(M+3) ='- Exit                  '

C Help text for this menu.
        helptopic='zone_htc_control_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)

        write(msg,'(a,i1,a)') 'Period ',IN,' convection coefficients'
        CALL EMENU(msg,ITEM,NITEMS,INO)

C Edit data or display help.
        if (INO.eq.NITEMS) then
          goto 99
        elseif (INO.eq.NITEMS-1) then
          CALL PHELPD('conv coef data',nbhelp,'-',0,0,IER)
        elseif (INO.eq.NITEMS-2) then

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF

        elseif(INO.GT.MHEAD.AND.INO.LT.(NITEMS-MCTL+1))THEN

C Edit item identified by KEYIND.
          CALL KEYIND(NITEMS,INO,IS,IO)
          write(outs,'(a,i3,1x,a)')'Hc values for surface',is,
     &      SNAME(iz,is)(1:lnblnk(SNAME(iz,is)))
          write (SNH,'(2f7.2)') HCI(id,IN,IS),HCE(id,IN,IS)
          CALL EASKS(SNH,outs,'inside, outside?',16,' -1.0 -1.0 ',
     &      'hc values',IER,nbhelp)
          K=0
          CALL EGETWR(SNH,K,HCI(id,IN,IS),-1.5,1000.,'F',
     &      'hci value',IER)
          CALL EGETWR(SNH,K,HCE(id,IN,IS),-1.5,1000.,'F',
     &      'hce value',IER)
        else

C Not one of the legal menu choices.
          INO=-1
          goto 3
        endif
        goto 20
 99     continue
      ENDIF

C Selected correlations control: Each correlation is identified 
C by an index. Refer to src/ebld/convect1.F,
C subroutine HTOVER over-rides the normal treatment of surface
C heat transfer. Also see SELCOR2 and HTBOUY.
      IF(iw.eq.2) THEN
        helptopic='zone_htc_correlat_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        hcfpdescr(id,IN) = 'User supplied hc correlations'
        DO 71 J=1,NS
          SN=SNAME(iz,j)
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Correlation for surface',J,
     &           SN(1:lnblnk(SN)),' inside : period',IN
          IH=IHCI(id,IN,J)
          write(outs2,'(2a)') 'Currently: ',hclngdescr(IH)
          call edisp(iuout,outs2)
          INPICK=1
          CALL EPICKS(INPICK,IVALS,outs,
     &      'Select one',48,99,hclngdescr,
     &      'Heat transfer correlations',IER,nbhelp)
          IHCI(id,IN,J)=IVALS(1)
   71   CONTINUE
        DO 72 J=1,NS
          SN=SNAME(iz,j)
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Correlation for surface',J,
     &           SN(1:lnblnk(SN)),' outside : period',IN
          IH=IHCE(id,IN,J)
          CALL EASKI(IH,outs,' (-1.= simulation toggle = default) ? ',
     &      -2,'W',100,'W',-1,'correlation outside',IERI,nbhelp)
          if(ieri.eq.-3)then
            continue
          else
            IHCE(id,IN,J)=IH
          endif
   72   CONTINUE
      ENDIF

C Adaptive control.
      IF(iw.eq.3) THEN
        helptopic='zone_htc_adaptive_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        hcfpdescr(id,IN) = 'User supplied adaptive hc correlations'
        DO 81 J=1,NS
          SN=SNAME(iz,j)
          Ilaw=99
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Control law for surface',J,
     &           SN(1:lnblnk(SN)),' inside : period',IN
          CALL EASKI(Ilaw,outs,' (99=no control for this surface)',
     &      0,'W',100,'W',99,'control law inside',IERI,nbhelp)
          if(ieri.eq.-3)then
            goto 81
          endif
          CVdata(id,IN,J,1) = Ilaw

C Control law 1 (for when heated and not heated).
          if(Ilaw.eq.1) then
            helptopic='zone_htc_adaptive_c1'
            call gethelptext(helpinsub,helptopic,nbhelp)
            stuff=' -1 -1 '
  102       CALL EASKS(stuff,'Correlation indices for both',
     &        'ON & OFF heating states',10,' -1  -1',
     &        'data for adaptive law 1',IER,nbhelp)
            K=0
            CALL EGETWI(stuff,K,itmp,-1,100,'F','corr ON',IER)
            if(ier.ne.0) goto 102
            CVdata(id,IN,J,2) = itmp
            CALL EGETWI(stuff,K,itmp,-1,100,'F','corr OFF',IER)
              if(ier.ne.0) goto 102
            CVdata(id,IN,J,3) = itmp

C Fourth data item not used for control law 1.
            CVdata(id,IN,J,4) = 0.

C No control.
          elseif(Ilaw.eq.99) then
            CVdata(id,IN,J,2) = 99.
            CVdata(id,IN,J,3) = 99.
            CVdata(id,IN,J,4) = 99.
C Other control laws not yet supported.
          else
          endif
   81   CONTINUE

        helptopic='zone_htc_adaptive_sim'
        call gethelptext(helpinsub,helptopic,nbhelp)
        DO 82 J=1,NS
          SN=SNAME(iz,j)
          Ilaw=99
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Control law for surface',J,
     &           SN(1:lnblnk(SN)),' outside : period',IN
          CALL EASKI(Ilaw,outs,' (99=no control for this surface)',
     &      0,'W',100,'W',99,'adapt control law outside',IERI,nbhelp)
          if(ieri.eq.-3)then
            goto 82
          endif
          CVdata(id,IN,J,5) = Ilaw

C Control law 1.
          if(Ilaw.eq.1) then
            helptopic='zone_htc_adaptive_c1'
            call gethelptext(helpinsub,helptopic,nbhelp)
  103       CALL EASKS(stuff,' correlation for ON',
     &                 ' correlation for OFF',10,' -1  -1',
     &                 'data for law 1',IER,nbhelp)
            K=0
            CALL EGETWI(stuff,K,itmp,-1,100,'F','corr ON',IER)
            if(ier.ne.0) goto 103
            CVdata(id,IN,J,6) = itmp
            CALL EGETWI(stuff,K,itmp,-1,100,'F','corr OFF',IER)
              if(ier.ne.0) goto 103
            CVdata(id,IN,J,7) = itmp

C Eight data item not used for control law 1.
            CVdata(id,IN,J,8) = 0.

C No control.
          elseif(Ilaw.eq.99) then
            CVdata(id,IN,J,6) = 99.
            CVdata(id,IN,J,7) = 99.
            CVdata(id,IN,J,8) = 99.
          else
          
C Other control laws not yet supported.
            continue
          endif
   82   CONTINUE
      ENDIF

C CEN (or similar) specified hc control.
      IF(iw.eq.4) THEN

C If the first two values are zero then probably nothing setup so
C ask user if default values are to be used.
        ibase=nint(CENLOC(id,IN,1))
        itop=nint(CENLOC(id,IN,2))
        if(ibase.eq.0.and.itop.eq.0)then
          helptopic='zone_htc_cen_default'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ','Use default description?',
     &          OK,nbhelp)
          if(ok)then
            CENLOC(id,in,3)=5.0; CENLOC(id,in,4)=0.7
            CENLOC(id,in,5)=8.0; CENLOC(id,in,6)=5.0
            CENLOC(id,in,7)=0.7; CENLOC(id,in,8)=8.0
            CENLOC(id,in,9)=5.0; CENLOC(id,in,10)=0.7
            CENLOC(id,in,11)=8.0; CENLOC(id,in,12)=2.5
            CENLOC(id,in,13)=2.5; CENLOC(id,in,14)=8.0
            hcfpdescr(id,IN) = 'CEN default hc coefficients'
          endif
        endif

C Initial menu entry setup.
  21    IER=0
        INO=-2
        ibase=nint(CENLOC(id,IN,1))
        if(ibase.ne.0)then
          write(item(1),'(2a)') 'a base surf is ',SNAME(iz,ibase)
        else
          write(item(1),'(2a)') 'a base surf is ','not yet defined'
        endif
        itop=nint(CENLOC(id,IN,2))
        if(itop.ne.0)then
          write(item(2),'(2a)') 'b top surf is ',SNAME(iz,itop)
        else
          write(item(2),'(2a)') 'b top surf is ','not yet defined'
        endif
        write(item(3),'(a)')  ' ---floor--------      '
        write(item(4),'(a,f7.3)') 'c upward flow hc   ',CENLOC(id,IN,3)
        write(item(5),'(a,f7.3)') 'd downward flow hc ',CENLOC(id,IN,4)
        write(item(6),'(a,f7.3)') 'e outside hc       ',CENLOC(id,IN,5)
        write(item(7),'(a)')  ' --ceiling-------      '
        write(item(8),'(a,f7.3)') 'f upward flow hc   ',CENLOC(id,IN,6)
        write(item(9),'(a,f7.3)') 'g downward flow hc ',CENLOC(id,IN,7)
        write(item(10),'(a,f7.3)')'h outside hc       ',CENLOC(id,IN,8)
        write(item(11),'(a)') ' --sloped--------      '
        write(item(12),'(a,f7.3)')'i upward flow hc   ',CENLOC(id,IN,9)
        write(item(13),'(a,f7.3)')'j downward flow hc ',CENLOC(id,IN,10)
        write(item(14),'(a,f7.3)')'k outside hc       ',CENLOC(id,IN,11)
        write(item(15),'(a)') ' --wall----------      '
        write(item(16),'(a,f7.3)')'l inside hc        ',CENLOC(id,IN,12)
        write(item(17),'(a,f7.3)')'m partition hc     ',CENLOC(id,IN,13)
        write(item(18),'(a,f7.3)')'n outside hc       ',CENLOC(id,IN,14)
        write(item(19),'(a)') ' _____________________ '
        write(item(20),'(a)') 'o (re) set defaults    '
        write(item(21),'(a)') '? help                 '
        write(item(22),'(a)') '- exit                 '

C Number of items displayed.
        helptopic='zone_htc_cen_default'
        call gethelptext(helpinsub,helptopic,nbhelp)
        NITEMS=22

        CALL EMENU('CEN coefficients',ITEM,NITEMS,INO)

C Edit data or display help.
        if (INO.eq.NITEMS) then

C Exit this menu (to get back to the top level of the hc regime)
C silently if base and top surfaces have been identified.
          ibase=nint(CENLOC(id,IN,1))
          itop=nint(CENLOC(id,IN,2))
          if(ibase.eq.0.and.itop.eq.0)then
            CALL EASKOK('Base and top surfaces zone not yet defined!',
     &        'Exit?',OK,nbhelp)
            if(ok)then
              goto 19
            else
              goto 21
            endif 
          endif
          return

        elseif (INO.eq.NITEMS-1) then

          helptopic='zone_htc_cen_default'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('CEN conv coef data',nbhelp,'-',0,0,IER)

        elseif (INO.eq.1) then

          IS=nint(CENLOC(id,IN,1))
          CALL EASKSUR(IZ,IS,'A','Select surface for base of room.',
     &      '(should be roughly horizontal)',IER)
          if(IS.ne.0)then
            CENLOC(id,IN,1)=real(IS)
          endif

        elseif (INO.eq.2) then

          IS=nint(CENLOC(id,IN,2))
          CALL EASKSUR(IZ,IS,'A','Select surface for top of room.',
     &      '(should be roughly horizontal)',IER)
          if(IS.ne.0)then
            CENLOC(id,IN,2)=real(IS)
          endif

        elseif (INO.eq.4) then

          V1=CENLOC(id,IN,3)
          CALL EASKR(V1,' ','Hc for upwards heat flow at floors?',
     &       0.1,'F',99.,'F',5.0,'floor hc upwards',IER,nbhelp)
          CENLOC(id,IN,3)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.5) then

          V1=CENLOC(id,IN,4)
          CALL EASKR(V1,' ','Hc for downwards heat flow at floors?',
     &       0.1,'F',99.,'F',0.7,'floor hc downwards',IER,nbhelp)
          CENLOC(id,IN,4)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.6) then

          V1=CENLOC(id,IN,5)
          CALL EASKR(V1,' ','Hc at outside (exterior) floors?',
     &       0.1,'F',99.,'F',8.0,'floor hc exterior',IER,nbhelp)
          CENLOC(id,IN,5)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.8) then

          V1=CENLOC(id,IN,6)
          CALL EASKR(V1,' ','Hc for upwards heat flow at ceilings?',
     &       0.1,'F',99.,'F',5.0,'ceil hc upwards',IER,nbhelp)
          CENLOC(id,IN,6)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.9) then

          V1=CENLOC(id,IN,7)
          CALL EASKR(V1,' ','Hc for downwards heat flow at ceilings?',
     &       0.1,'F',99.,'F',0.7,'ceil hc downwards',IER,nbhelp)
          CENLOC(id,IN,7)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.10) then

          V1=CENLOC(id,IN,8)
          CALL EASKR(V1,' ','Hc at outside (exterior) ceilings?',
     &       0.1,'F',99.,'F',8.0,'ceil hc exterior',IER,nbhelp)
          CENLOC(id,IN,8)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.12) then

          V1=CENLOC(id,IN,9)
          CALL EASKR(V1,' ','Hc for upwards heat flow at sloped?',
     &       0.1,'F',99.,'F',5.0,'sloped hc upwards',IER,nbhelp)
          CENLOC(id,IN,9)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.13) then

          V1=CENLOC(id,IN,10)
          CALL EASKR(V1,' ','Hc for downwards heat flow at sloped?',
     &       0.1,'F',99.,'F',0.7,'sloped hc downwards',IER,nbhelp)
          CENLOC(id,IN,10)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.14) then

          V1=CENLOC(id,IN,11)
          CALL EASKR(V1,' ','Hc at outside (exterior) sloped?',
     &       0.1,'F',99.,'F',8.0,'sloped hc exterior',IER,nbhelp)
          CENLOC(id,IN,11)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.16) then

          V1=CENLOC(id,IN,12)
          CALL EASKR(V1,' ','Hc at inside face of walls?',
     &       0.1,'F',99.,'F',2.5,'walls hc inside',IER,nbhelp)
          CENLOC(id,IN,12)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.17) then

          V1=CENLOC(id,IN,13)
          CALL EASKR(V1,' ','Hc at other (partition) face of walls?',
     &       0.1,'F',99.,'F',2.5,'walls hc partition',IER,nbhelp)
          CENLOC(id,IN,13)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.18) then

          V1=CENLOC(id,IN,14)
          CALL EASKR(V1,' ','Hc at outside (exterior) walls?',
     &       0.1,'F',99.,'F',8.0,'walls hc exterior',IER,nbhelp)
          CENLOC(id,IN,14)=V1
          hcfpdescr(id,IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.20) then

C Reset hc to default values.
          CENLOC(id,in,3)=5.0; CENLOC(id,in,4)=0.7
          CENLOC(id,in,5)=8.0; CENLOC(id,in,6)=5.0
          CENLOC(id,in,7)=0.7; CENLOC(id,in,8)=8.0
          CENLOC(id,in,9)=5.0; CENLOC(id,in,10)=0.7
          CENLOC(id,in,11)=8.0; CENLOC(id,in,12)=2.5
          CENLOC(id,in,13)=2.5; CENLOC(id,in,14)=8.0
          hcfpdescr(id,IN) = 'CEN default hc coefficients'
        else
          goto 21
        endif
        goto 21
      endif

      RETURN
      END


C ******************** EMKHTC ********************
C EMKHTC writes the convection-calculation control file (usually named *.htc).
C It contains a header with a date stamp and associated zone name
C As well as documentation based on the last edit by the user.
      SUBROUTINE EMKHTC(FILE,ICOMP,IUF,ITRU,IER)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C     integer        ! IHCDT zero follows model day types, 1 = all days the same
C     integer IHCFP() ! number of hc control periods for each day type
C     real ST,EN     ! start and stop time of each period
C     real HCI       ! interior face hc coefficient (day,period,surface)
C     real HCE       ! other face hc coefficient (day,period,surface)
C     integer ICTL   ! hc control (day,period,surface) 1 is fixed coefficient 
C                    ! 2 is fixed correlation 3 is adaptive correlation 
C                    ! 4 is CEN standards
C     integer IHCI   ! which inside correlation (day,period,surface)
C                    ! -1 use standard correlation for surface location
C     integer IHCE   ! which other side correlation (day,period,surface)
C                    ! -1 use standard correlation for surface location
C     real CVdata    ! correlation attributes (day,period,surface,jj attribute)
C       jj=1:   control law for interior face
C       jj=2-4: data items associated with control law for interior
C       jj=5:   control law for other face
C       jj=6-8: data items associated with control law for other face
      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)

C     real CENLOC (per period,jj attributes)
C       jj=1 index of a floor surface
C       jj=2 index of a ceiling surface
C       jj=3-5 floor upwd_inside, downwd_inside, external
C       jj=6-8 ceiling upwd_inside, downwd_inside, external
C       jj=9-11 sloped upwd_inside, downwd_inside, external
C       jj=12-14 wall inside, partition, external
      COMMON/HCCEN/CENLOC(MDTY,MP,14)
      COMMON/OVRWT/AUTOVR
      logical AUTOVR
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      dimension HCIA(MS),HCEA(MS),IHCIA(MS),IHCEA(MS)
      character*12 sn(MS)
      character hcfpdescr*72,dstmp*24,t117*117,FILE*72
      CHARACTER louts*248
      character tokens*124,comment*76,aligned_str*124

C Short and long descriptions of correlations.
      character hcshtdescr*32  ! short description
      character hclngdescr*48   ! long description
      dimension hcshtdescr(-1:99),hclngdescr(-1:99)

      if(AUTOVR)then
        CALL EFOPSEQ(IUF,FILE,3,IER)
      else
        CALL EFOPSEQ(IUF,FILE,4,IER)
      endif

C Also list the names of the surfaces of the zone use more than
C one line if necessary.
      do ij=1,nzsur(icomp)
        sn(ij)=sname(icomp,ij)
      enddo

      call dstamp(dstmp)
      write(tokens,'(a)',IOSTAT=ISTAT,ERR=990)  '*HC_CTL'
      write(comment,'(2a)',IOSTAT=ISTAT,ERR=990)  
     &  'heat transfer control for ',
     &  zname(icomp)(1:lnzname(icomp))
      call align_comment(32,tokens,comment,aligned_str)
      write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))

C Date string depricated.
C      write(tokens,'(2a)',IOSTAT=ISTAT,ERR=990)  '*Date,',dstmp
C      write(comment,'(a)') 'latest file modification'
C      call align_comment(32,tokens,comment,aligned_str)
C      write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(a,i3)',IOSTAT=ISTAT,ERR=990)'*Days',IHCDT
      write(comment,'(a)') 'number of control day types'
      call align_comment(32,tokens,comment,aligned_str)
      write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))
      do L=1,IHCDT
        if(IHCDT.eq.1)then
          WRITE(tokens,'(I3)',IOSTAT=ISTAT,ERR=990) IHCFP(L)
          write(comment,'(a)') 
     &      'number control intervals for all day types'
          call align_comment(32,tokens,comment,aligned_str)
          write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))
        else
          WRITE(tokens,'(I3)',IOSTAT=ISTAT,ERR=990) IHCFP(L)
          write(comment,'(2a)') 'number control intervals for ',
     &      calentag(L)
          call align_comment(32,tokens,comment,aligned_str)
          write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif
        write(tokens,'(I3)',IOSTAT=ISTAT,ERR=990) NZSUR(ICOMP)
        write(comment,'(a)') 'number of surfaces'
        call align_comment(32,tokens,comment,aligned_str)
        write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))

C If ICTL = 3 and only one period skip listing of surfaces.
        if(IHCFP(L).eq.1.and.ICTL(L,1).eq.3)then
          continue
        else
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            len117=1
            nsur=nzsur(icomp)
            call aslist(ipos,nsur,sn,MS,'S',t117,len117,itrunc)
            WRITE(IUF,'(3a)',IOSTAT=ISTAT,ERR=990) '# (',
     &        t117(1:len117),')'
            ipos=itrunc+1
          end do
        endif
        DO 930 I = 1,IHCFP(L)
          WRITE(tokens,'(F6.2,F7.2)',IOSTAT=ISTAT,ERR=990)
     &    ST(L,I),EN(L,I)
          write(comment,'(a)') 'start and end time of interval'
          call align_comment(32,tokens,comment,aligned_str)
          write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))

          WRITE(tokens,'(I3)',IOSTAT=ISTAT,ERR=990) ICTL(L,I)
          if(ICTL(L,I).eq.1)then
            write(comment,'(a)')
     &        'fixed coefficients calculation control type'
          elseif(ICTL(L,I).eq.2)then
            write(comment,'(a)')
     &        'fixed correlation calculation control type'
          elseif(ICTL(L,I).eq.3)then
            write(comment,'(a)')
     &        'adaptive calculation control type'
          elseif(ICTL(L,I).eq.4)then
            write(comment,'(a)')
     &        'CEN calculation control type'
          endif
          call align_comment(32,tokens,comment,aligned_str)
          write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))

          write(iuf,'(2a)',IOSTAT=ISTAT,ERR=990) '*Doc,',
     &    hcfpdescr(L,i)(1:lnblnk(hcfpdescr(L,i)))
          if(ICTL(L,I).eq.1) then

C Convection coefficients. Copy to local array and then write out
C on as many lines as necessary.
            do 38 ij = 1,NZSUR(ICOMP)
              HCIA(ij) = HCI(L,i,ij)
              HCEA(ij) = HCE(L,i,ij)
  38        continue
            itrunc=1
            ipos=1
            do while (itrunc.ne.0)
              nsur=nzsur(icomp)
             call arlist(ipos,nsur,HCIA,MS,'C',louts,loutln,itrunc)
              write(iuf,'(1x,a)',IOSTAT=ios,ERR=990) louts(1:loutln)
              ipos=itrunc+1
            end do
            itrunc=1
            ipos=1
            do while (itrunc.ne.0)
              nsur=nzsur(icomp)
              call arlist(ipos,nsur,HCEA,MS,'C',louts,loutln,itrunc)
              write(iuf,'(1x,a)',IOSTAT=ios,ERR=990) louts(1:loutln)
              ipos=itrunc+1
            end do

          elseif(ICTL(L,I).eq.2) then

C Correlation indices. Copy to local array and then write out
C on as many lines as necessary.
            do 39 ij = 1,NZSUR(ICOMP)
              IHCIA(ij) = IHCI(L,i,ij)
              IHCEA(ij) = IHCE(L,i,ij)
  39        continue
            itrunc=1
            ipos=1
            do while (itrunc.ne.0)
              nsur=nzsur(icomp)
              call ailist(ipos,nsur,IHCIA,MS,'C',louts,loutln,itrunc)
              write(iuf,'(1x,a)',IOSTAT=ios,ERR=990) louts(1:loutln)
              ipos=itrunc+1
            end do
            itrunc=1
            ipos=1
            do while (itrunc.ne.0)
              nsur=nzsur(icomp)
              call ailist(ipos,nsur,IHCEA,MS,'C',louts,loutln,itrunc)
              write(iuf,'(1x,a)',IOSTAT=ios,ERR=990) louts(1:loutln)
              ipos=itrunc+1
            end do

          elseif(ICTL(L,I).eq.3) then

C Control laws and associated data for each surface.
            call gethccornames(hcshtdescr,hclngdescr)
            do 37 J=1,NZSUR(ICOMP)
              icindex=nint(CVdata(L,I,J,2))
              write(comment,'(3a)')
     &          sn(J)(1:lnblnk(sn(J))),' ',hcshtdescr(icindex)
              WRITE(tokens,'(8(f5.0))',IOSTAT=ISTAT,ERR=990)
     &            (CVdata(L,I,J,jj),jj=1,8)
              call align_comment(42,tokens,comment,aligned_str)
              write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))
   37       continue

C  Additional information for air channels
            WRITE(tokens,'(I3,2F6.3)',IOSTAT=ISTAT,ERR=990)
     &        IAORZ,CWIDTH,CHEIGHT
            write(comment,'(a)')
     &    'Air origin zone (+ chan. width & height if Bar-Cohen cor.)'
            call align_comment(42,tokens,comment,aligned_str)
            write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))

          elseif(ICTL(L,I).eq.4) then

C CEN 13791 mode sets values for specific locations.
            WRITE(tokens,'(a,2F7.1)',IOSTAT=ISTAT,ERR=990)
     &      '*basetop ',CENLOC(L,I,1),CENLOC(L,I,2)
            write(comment,'(a)')
     &        'surface indices of a typcial base & top'
            call align_comment(32,tokens,comment,aligned_str)
            write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))

            WRITE(tokens,'(a,3F7.3)',IOSTAT=ISTAT,ERR=990)
     &      '*floor   ',CENLOC(L,I,3),CENLOC(L,I,4),CENLOC(L,I,5)
            write(comment,'(a)')
     &        'CEN floor upwd_inside, downwd_inside, external'
            call align_comment(32,tokens,comment,aligned_str)
            write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))

            WRITE(tokens,'(a,3F7.3)',IOSTAT=ISTAT,ERR=990)
     &      '*ceiling ',CENLOC(L,I,6),CENLOC(L,I,7),CENLOC(L,I,8)
            write(comment,'(a)')
     &        'CEN ceiling upwd_inside, downwd_inside, external'
            call align_comment(32,tokens,comment,aligned_str)
            write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))

            WRITE(tokens,'(a,3F7.3)',IOSTAT=ISTAT,ERR=990)
     &      '*slope   ',CENLOC(L,I,9),CENLOC(L,I,10),CENLOC(L,I,11)
            write(comment,'(a)')
     &        'CEN sloped upwd_inside, downwd_inside, external'
            call align_comment(32,tokens,comment,aligned_str)
            write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))

            WRITE(tokens,'(a,3F7.3)',IOSTAT=ISTAT,ERR=990)
     &      '*wall    ',CENLOC(L,I,12),CENLOC(L,I,13),CENLOC(L,I,14)
            write(comment,'(a)')
     &        'CEN wall inside, partition, external'
            call align_comment(32,tokens,comment,aligned_str)
            write(iuf,'(a)') aligned_str(1:lnblnk(aligned_str))
          endif
  930   CONTINUE
      ENDDO  ! of IHCDT
      CALL ERPFREE(IUF,ISTAT)
      RETURN

  990 call edisp(ITRU,' HC File write error.')
      CALL ERPFREE(IUF,ISTAT)
      RETURN
      END

C ******************** EHTCFF ********************
C EHTCFF reads a legacy or current format convection calculation
C control file (usually named *.htc).

      SUBROUTINE EHTCFF(FILE,IUF,IER)
#include "building.h"
#include "espriou.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT
      common/HCFPHI/hcfpdescr(MDTY,MBP)

C CENLOC CEN specifications for each period
C       jj=1 index of a floor surface
C       jj=2 index of a ceiling surface
C       jj=3-5 floor upwd_inside, downwd_inside, external
C       jj=6-8 ceiling upwd_inside, downwd_inside, external
C       jj=9-11 sloped upwd_inside, downwd_inside, external
C       jj=12-14 wall inside, partition, external
      COMMON/HCCEN/CENLOC(MDTY,MBP,14)

      character*72 FILE,hcfpdescr*72,outstr*124,word*24
      dimension HCIA(MS),HCEA(MS),IHCIA(MS),IHCEA(MS)
      logical newer
        
      CALL EFOPSEQ(IUF,FILE,1,IER)
      IF(ier.eq.0)THEN
        write(currentfile,'(a)') FILE(1:lnblnk(FILE))

C Does the 1st and 2nd line have one item and the 3rd line have two items?
C Or does the file start with '*HC_CTL'?
        CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc line 1',IER)
        CALL STRIPC(IUF,OUTSTR,99,num2,1,'htc line 2',IER)
        CALL STRIPC(IUF,OUTSTR,99,num3,1,'htc line 3',IER)
        if(num1.eq.1.and.num2.eq.1.and.num3.eq.2)then
          rewind(iuf,err=392)
          newer=.false.
          IHCDT=1  ! all days the same
        else

C Rewind file check first line and if *HC_Ctl.
          rewind(iuf,err=392)
          CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc header',IER)
          if(OUTSTR(1:7).eq.'*HC_CTL'.or.
     &       OUTSTR(1:7).eq.'*hc_ctl')then
            newer=.true.
            CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc date or day type',IER)
            if(OUTSTR(1:5).eq.'*Date')then
              CALL STRIPC(IUF,OUTSTR,99,num1,1,'Day types',IER)
              if(OUTSTR(1:5).eq.'*Days')then
                K=7
                CALL EGETWI(OUTSTR,K,N,1,20,'F','nb day types',IER)
                IHCDT=n
              else
                IHCDT=1  ! all days the same
                backspace(IUF)  ! unread the current line of the file
              endif
            elseif(OUTSTR(1:5).eq.'*Days')then
              K=7
              CALL EGETWI(OUTSTR,K,N,1,20,'F','nb day types',IER)
              IHCDT=n
            endif
          else

C Not a recognised file << what error code to return? >>
            goto 390
          endif
        endif

C Loop for each day type. Begin with time intervals.
        do L=1,IHCDT
          CALL STRIPC(IUF,OUTSTR,1,num1,1,'htc periods',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IHCFP(L),1,MP,'W','htc periods',IER)
          if(ier.ne.0) goto 390

C Number of surfaces.
          CALL STRIPC(IUF,OUTSTR,1,num1,1,'htc surfaces',IER)
          K=0
          CALL EGETWI(OUTSTR,K,NS,1,MS,'W','htc surfaces',IER)
          DO 330 I = 1,IHCFP(L)

C Start and end times of interval.
            CALL STRIPC(IUF,OUTSTR,2,num1,1,'htc start - end',IER)
            K=0
            CALL EGETWR(OUTSTR,K,ST(L,I),0.,24.,'W','htc start',IER)
            CALL EGETWR(OUTSTR,K,EN(L,I),ST(L,I),24.,'W','htc end',IER)


C Control `type' for interval. Note: older files might not have this
C line. If there is only one item on the line then read it, otherwise
C if there are NS items on the line then it is an older file.
            CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc ctl type',IER)
            if(num1.eq.1)then
              K=0
              CALL EGETWI(OUTSTR,K,ICTL(L,I),1,4,'W','htc ctl type',IER)

C If newer format then scan for hcfpdescr.
              if(newer)then
                CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc ctl doc',IER)
                K=0
                CALL EGETW(OUTSTR,K,WORD,'W','*doc',IFLAG)
                call egetrm(outstr,K,hcfpdescr(L,i),'W','hc doc',IER)
              else
                hcfpdescr(L,i)=
     &            'Imported legacy file with no regime docs.'
              endif
            else
              ICTL(L,I)=1
              hcfpdescr(L,i)='Imported legacy file assuming ctl type 1.'
            endif

            if(ICTL(L,I).eq.1) then

C Convection coefficients - read one for each surface (inside face)
C and then read one for each surface (other side face). Note that
C the `use default value` indicator is -1 to allow that in range
C checking.
              CALL EGETWRA(IUF,HCIA,NS,-1.,1000.,'W',
     &          'fixed inside face hc',IER)
              CALL EGETWRA(IUF,HCEA,NS,-1.,1000.,'W',
     &          'fixed other face hc',IER)
              DO 114 KV=1,NS
                HCI(L,I,KV)=HCIA(KV)
                HCE(L,I,KV)=HCEA(KV)
  114         CONTINUE

            elseif(ICTL(L,I).eq.2) then

C Correlation indices.
              CALL EGETWIA(IUF,IHCIA,NS,-1,99,'W',
     &          'inside face hc cor',IER)
              CALL EGETWIA(IUF,IHCEA,NS,-1,99,'W',
     &          'other face hc cor',IER)
              DO 115 KV=1,NS
                IHCI(L,I,KV)=IHCIA(KV)
                IHCE(L,I,KV)=IHCEA(KV)
  115         CONTINUE

            elseif(ICTL(L,I).eq.3) then

C Control laws and associated data for each surface.
C << needs range check >>
              do 30 J=1,NS
                READ(IUF,*,IOSTAT=ISTAT,ERR=390)
     &            (CVdata(L,I,J,jj),jj=1,8)
   30         continue

C Read IAORZ,CWIDTH and CHEIGHT of ventilated channels. This line
C may not exist in all hc files.
              READ(IUF,*,IOSTAT=ISTAT,ERR=390,END=391)
     &          IAORZ,CWIDTH,CHEIGHT

            elseif(ICTL(L,I).eq.4) then

C CEN mode sets values for specific conditions.
              CALL STRIPC(IUF,OUTSTR,99,num,1,'htc cen base top',IER)
              if(OUTSTR(1:8).eq.'*basetop')then
                K=8
                CALL EGETWR(OUTSTR,K,V1,1.,100.,'W','htc cen base',IER)
                CALL EGETWR(OUTSTR,K,V2,1.,100.,'W','htc cen top',IER)
                CENLOC(L,i,1)=V1
                CENLOC(L,i,2)=V2
              endif
              CALL STRIPC(IUF,OUTSTR,99,num,1,'htc cen floor',IER)
              if(OUTSTR(1:6).eq.'*floor')then
                K=6
                CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen upward',IER)
                CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen down',IER)
                CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
                CENLOC(L,i,3)=V1
                CENLOC(L,i,4)=V2
                CENLOC(L,i,5)=V3
              endif
              CALL STRIPC(IUF,OUTSTR,99,num,1,'htc cen ceiling',IER)
              if(OUTSTR(1:8).eq.'*ceiling')then
                K=8
                CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen upward',IER)
                CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen down',IER)
                CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
                CENLOC(L,i,6)=V1
                CENLOC(L,i,7)=V2
                CENLOC(L,i,8)=V3
              endif
              CALL STRIPC(IUF,OUTSTR,99,num,1,'htc cen slope',IER)
              if(OUTSTR(1:6).eq.'*slope')then
                K=6
                CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen upward',IER)
                CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen down',IER)
                CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
                CENLOC(L,i,9)=V1
                CENLOC(L,i,10)=V2
                CENLOC(L,i,11)=V3
              endif
              CALL STRIPC(IUF,OUTSTR,99,num,1,'htc cen wall',IER)
              if(OUTSTR(1:5).eq.'*wall')then
                K=5
                CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen inside',IER)
                CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen ptn',IER)
                CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
                CENLOC(L,i,12)=V1
                CENLOC(L,i,13)=V2
                CENLOC(L,i,14)=V3
              endif
            endif
  330     CONTINUE
        ENDDO ! of IHCDT
        CALL ERPFREE(IUF,ISTAT)
      endif
      RETURN

C Error messages.
  390 call edisp(iuout,' HC File read error.')
      CALL ERPFREE(IUF,ISTAT)
      RETURN
  391 call edisp(iuout,
     & ' HC File scan of optional IAORZ CWIDTH CHEIGHT not found.')
      CALL ERPFREE(IUF,ISTAT)
      RETURN
  392 call edisp(iuout,' Unable to rewind HC File.')
      CALL ERPFREE(IUF,ISTAT)
      RETURN

      END

C ************* listhtc
C listhtc reports on the current heat transfer correlation definition
C held in a *.htc file.
      subroutine listhtc(ITRU,ICOMP,act,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

C Parameters. 
      integer itru    ! file unit to write to
      integer icomp   ! focus zone
      character act*1 ! 's' summary 'f' full
      integer ier     ! non-zero if an issue

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      COMMON/HCCEN/CENLOC(MDTY,MP,14)
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32

      character outs*124,outs2*124,outs3*124
      character hcfpdescr*72
      logical indefault,outdefault   ! default treatment
      logical indefault2,outdefault2 ! default treatment in 2nd column
      logical inuser,outuser         ! user defined
      logical odd                    ! for double column reporting

C Short and long descriptions of correlations.
      character hcshtdescr*32  ! short description
      character hclngdescr*48   ! long description
      dimension hcshtdescr(-1:99),hclngdescr(-1:99)

C Check if there is a *.htc file for this zone.
      if(IHC(ICOMP).EQ.0)then
        return
      endif

C List control data.
      NS=NZSUR(icomp)
      call edisp(itru,' ')
      do L=1,IHCDT
        if(IHCDT.eq.1)then
         WRITE(outs,'(A,I2)')' Number of control periods: ',IHCFP(L)
        else
         WRITE(outs,'(A,I2,2a)')' Number of control periods: ',IHCFP(L),
     &    ' for ',calentag(L)
        endif
        call edisp(itru,' ')
        call edisp(itru,outs)

C If nothing to report, return.
        if(ihcfp(L).eq.0) return
        if(NS.eq.0) return

C Recover and list out the available correlations.
        call gethccornames(hcshtdescr,hclngdescr)

        DO 850 I = 1,IHCFP(L)
          WRITE(outs,5202)I,ST(L,I),EN(L,I)
 5202     FORMAT(' Period',I3,' start',F7.2,' finish',F7.2)
          call edisp(itru,outs)
          if(ICTL(L,I).eq.1)then
           write(outs,'(a)') ' User specified convection coefficients'
          elseif(ICTL(L,I).eq.2)then
           write(outs,'(a)') ' User specified convection regime'
          elseif(ICTL(L,I).eq.3)then
           write(outs,'(a)') ' Adaptive (mixed) convection regime'
          elseif(ICTL(L,I).eq.4)then
           write(outs,'(a)') ' CEN convection regime:'
          endif
          write(outs,'(1x,a)') hcfpdescr(L,I)(1:lnblnk(hcfpdescr(L,i)))
          call edisp(itru,outs)
          if(ICTL(L,I).eq.1.and.
     &      act(1:1).eq.'f'.or.act(1:1).eq.'F') then

C Convection coefficients. If lots of surfaces double the reporting.
            im=MOD(NS,2)
            odd=.false.
            if(im.eq.1) odd=.true.
            if(NS.lt.12)then   ! Write as a single column
            call edisp(itru,
     &        '   Surface orientation    Inside    Outside')
              DO 860 J=1,NS
                call eclose(HCI(L,I,J),-1.0,0.01,indefault)
                call eclose(HCE(L,I,J),-1.0,0.01,outdefault)
                if(indefault.and.outdefault)then
                  WRITE(outs,'(I3,5a)') J,' ',SNAME(icomp,j),' (',
     &            SVFC(icomp,j),')   default   default'
                elseif(indefault.and.(.NOT.outdefault))then
                  WRITE(outs,'(I3,5a,F8.3)') J,' ',SNAME(icomp,j),' (',
     &            SVFC(icomp,j),')   default ',HCE(L,I,J)
                elseif(.NOT.indefault.and.outdefault)then
                  WRITE(outs,'(I3,5a,F8.3,a)') J,' ',SNAME(icomp,j),
     &            ' (',SVFC(icomp,j),')  ',HCI(L,I,J),'   default '
                else
                  WRITE(outs,'(I3,5a,2(1X,F8.3))') J,' ',SNAME(icomp,j),
     &            ' (',SVFC(icomp,j),') ',HCI(L,I,J),HCE(L,I,J)
                endif
                call edisp(itru,outs)
  860         CONTINUE
            else

C Write data as double column.
              write(outs,'(2a)') 
     &          '   Surface orientation    Inside    Outside',
     &          '   Surface orientation    Inside    Outside'
              call edisp(itru,outs)
              MNU=(NS/2)
              DO 193 J=1,MNU
                call eclose(HCI(L,I,J),-1.0,0.01,indefault)
                call eclose(HCE(L,I,J),-1.0,0.01,outdefault)
                call eclose(HCI(L,I,J+MNU),-1.0,0.01,indefault2)
                call eclose(HCE(L,I,J+MNU),-1.0,0.01,outdefault2)
                if(indefault.and.outdefault)then   ! Generate text for 1st column.
                  WRITE(outs,'(I3,5a)') J,' ',SNAME(icomp,j),' (',
     &            SVFC(icomp,j),')   default   default'
                elseif(indefault.and.(.NOT.outdefault))then
                  WRITE(outs,'(I3,5a,F8.3)') J,' ',SNAME(icomp,j),' (',
     &            SVFC(icomp,j),')   default ',HCE(L,I,J)
                elseif(.NOT.indefault.and.outdefault)then
                  WRITE(outs,'(I3,5a,F8.3,a)') J,' ',SNAME(icomp,j),
     &            ' (',SVFC(icomp,j),')  ',HCI(L,I,J),'   default '
                else
                  WRITE(outs,'(I3,5a,2(1X,F8.3))') J,' ',SNAME(icomp,j),
     &            ' (',SVFC(icomp,j),') ',HCI(L,I,J),HCE(L,I,J)
                endif 
                if(indefault2.and.outdefault2)then   ! Generate text for 2nd column.
                  WRITE(outs2,'(I3,5a)') J+MNU,' ',SNAME(icomp,j+MNU),
     &            ' (',SVFC(icomp,j+MNU),')   default   default'
                elseif(indefault2.and.(.NOT.outdefault2))then
                  WRITE(outs2,'(I3,5a,F8.3)') J+MNU,' ',
     &            SNAME(icomp,j+MNU),' (',
     &            SVFC(icomp,j+MNU),')   default ',HCE(L,I,J+MNU)
                elseif(.NOT.indefault2.and.outdefault2)then
                  WRITE(outs2,'(I3,5a,F8.3,a)') J+MNU,' ',
     &            SNAME(icomp,j+MNU),' (',SVFC(icomp,j+MNU),')  ',
     &            HCI(L,I,J+MNU),'   default '
                else
                  WRITE(outs2,'(I3,5a,2(1X,F8.3))') J+MNU,' ',
     &            SNAME(icomp,j+MNU),' (',SVFC(icomp,j+MNU),') ',
     &            HCI(L,I,J+MNU),HCE(L,I,J+MNU)
                endif
                write(outs3,'(3a)') outs(1:lnblnk(outs)),' ',
     &            outs2(1:lnblnk(outs2))
                call edisp(itru,outs3)
  193         CONTINUE

C Put odd vertex in left column.
              if(odd)then
                J=NS
                call eclose(HCI(L,I,J),-1.0,0.01,indefault)
                call eclose(HCE(L,I,J),-1.0,0.01,outdefault)
                if(indefault.and.outdefault)then
                  WRITE(outs,'(44x,I3,5a)') J,' ',SNAME(icomp,j),' (',
     &            SVFC(icomp,j),')   default   default'
                elseif(indefault.and.(.NOT.outdefault))then
                  WRITE(outs,'(44x,I3,5a,F8.3)') J,' ',SNAME(icomp,j),
     &            ' (',SVFC(icomp,j),')   default ',HCE(L,I,J)
                elseif(.NOT.indefault.and.outdefault)then
                  WRITE(outs,'(44x,I3,5a,F8.3,a)') J,' ',SNAME(icomp,j),
     &            ' (',SVFC(icomp,j),')  ',HCI(L,I,J),'   default '
                else
                  WRITE(outs,'(44x,I3,5a,2(1X,F8.3))') J,' ',
     &            SNAME(icomp,j),' (',
     &            SVFC(icomp,j),') ',HCI(L,I,J),HCE(L,I,J)
                endif
                call edisp(itru,outs)
              endif
            endif
          elseif(ICTL(L,I).eq.2.and.
     &      act(1:1).eq.'f'.or.act(1:1).eq.'F') then

C Correlation indices.
            call edisp(itru,'   Surface orientation  Inside    Outside')
            DO 861 J=1,NS
              indefault=.false.; outdefault=.false.
              if(IHCI(L,I,J).eq.-1) indefault=.true.
              if(IHCE(L,I,J).eq.-1) outdefault=.true.
              if(indefault.and.outdefault)then
                WRITE(outs,'(I3,5a)') J,' ',SNAME(icomp,j),' (',
     &          SVFC(icomp,j),')  default   default'
                call edisp(itru,outs)
              elseif(indefault.and.(.NOT.outdefault))then
                WRITE(outs,'(I3,5a,I8)') J,' ',SNAME(icomp,j),' (',
     &          SVFC(icomp,j),')  default ',IHCE(L,I,J)
                call edisp(itru,outs)
                write(outs,'(2a)')'   other face ',
     &          hclngdescr(IHCE(L,I,J))
                call edisp(itru,outs)
              elseif(.NOT.indefault.and.outdefault)then
                WRITE(outs,'(I3,5a,I8,a)') J,' ',SNAME(icomp,j),' (',
     &          SVFC(icomp,j),') ',IHCI(L,I,J),'  default '
                call edisp(itru,outs)
                write(outs,'(2a)')'   inside face ',
     &          hclngdescr(IHCI(L,I,J))
                call edisp(itru,outs)
              else
                WRITE(outs,5205) J,SNAME(icomp,j),' (',SVFC(icomp,j),
     &          ') ',IHCI(L,I,J),IHCE(L,I,J)
 5205           FORMAT(I3,1x,4a,2(2X,I8))
                call edisp(itru,outs)
                write(outs,'(2a)')'   inside face ',
     &          hclngdescr(IHCI(L,I,J))
                call edisp(itru,outs)
                write(outs,'(2a)')'   other face  ',
     &            hclngdescr(IHCE(L,I,J))
                call edisp(itru,outs)
              endif
  861       CONTINUE

          elseif(ICTL(L,I).eq.3.and.
     &      act(1:1).eq.'f'.or.act(1:1).eq.'F') then

C Control laws and associated data.
            write(outs,'(2a)') ' Surface  Orientation ',
     &      '----- Inside -----     ----- Outside -----'
            call edisp(itru,outs)
            DO 862 J=1,NS
              WRITE(outs,5206)J,SNAME(icomp,j),' (',SVFC(icomp,j),') ',
     &        (CVdata(L,I,J,jj),jj=1,8)
 5206         FORMAT(I3,1x,4a,4(f4.0,1x),3x,4(f4.0,1x))
              call edisp(itru,outs)
  862       CONTINUE

          elseif(ICTL(L,I).eq.4.and.
     &      act(1:1).eq.'f'.or.act(1:1).eq.'F') then

            ibase=nint(CENLOC(L,I,1))
            itop=nint(CENLOC(L,I,2))
            if(ibase.ne.0.and.itop.ne.0)then
              lssn1=lnblnk(SNAME(icomp,ibase)) 
              lssn2=lnblnk(SNAME(icomp,itop)) 
              write(outs,'(5a)') ' CEN regime based on typical floor ',
     &          SNAME(icomp,ibase)(1:lssn1),' and typcial ceiling ',
     &          SNAME(icomp,itop)(1:lssn2),'.'
            else
              write(outs,'(2a)') ' CEN regime with undefined floor ',
     &        ' and/or ceiling surfaces.'
            endif
            call edisp(itru,outs)
            write(outs,'(a,F7.3,a,F7.3,a,F7.3,a)') 
     &      'Floor upwards flow hc is ',CENLOC(L,i,3),
     &      ' and downwards ',CENLOC(L,i,4),
     &      ' and external ',CENLOC(L,i,5),'.'
            call edisp(itru,outs)
            write(outs,'(a,F7.3,a,F7.3,a,F7.3,a)') 
     &      'Ceiling upwards flow hc is ',CENLOC(L,i,6),
     &      ' and downwards ',CENLOC(L,i,7),
     &      ' and external ',CENLOC(L,i,8),'.'
            call edisp(itru,outs)
            write(outs,'(a,F7.3,a,F7.3,a,F7.3,a)') 
     &      'Sloped upwards flow hc is ',CENLOC(L,i,9),
     &      ' and downwards ',CENLOC(L,i,10),
     &      ' and external ',CENLOC(L,i,11),'.'
            call edisp(itru,outs)
            write(outs,'(a,F7.3,a,F7.3,a,F7.3,a)') 
     &      'Walls inside hc is ',CENLOC(L,i,12),
     &      ' and other side of partitions ',CENLOC(L,i,13),
     &      ' and external ',CENLOC(L,i,14),'.'
            call edisp(itru,outs)
          endif
  850   CONTINUE
      enddo  ! of IHCDT
      return
      end

C ************* gethccornames
C gethccornames returns a terse and verbose array of strings describing
C the in-built heat transfer correlations in ESP-r.
      subroutine gethccornames(hcshtdescr,hclngdescr)
      
      character hcshtdescr*32  ! short description
      character hclngdescr*48   ! long description
      dimension hcshtdescr(-1:99),hclngdescr(-1:99)
      integer ij  ! loop counter

C Set all to not applicable and then instantiate available options.
      do ij=1,99
        hcshtdescr(ij)='not applicable'
        hclngdescr(ij)='not applicable'
      enddo
      
      hcshtdescr(-1)='default correlation             '
      hclngdescr(-1)='default hc correlation                          '
      hcshtdescr(0)='not applicable                  '
      hclngdescr(0)='not applicable                                  '
      hcshtdescr(1)='Alamdari & Hammond vertical     '
      hclngdescr(1)='Alamdari & Hammond for vertical surfaces        '
      hcshtdescr(2)='Halcrow invariant low vertical  '
      hclngdescr(2)='Halcrow time invariant low for vertical surfaces'
      hcshtdescr(3)='Halcrow invariant high vertical '
      hclngdescr(3)='Halcrow time invariant high vertical surfaces   '
      hcshtdescr(4)='CIBSE for vertical surfaces     '
      hclngdescr(4)='CIBSE for vertical surfaces                     '
      hcshtdescr(5)='Khalifa & Marshall non heat vert'
      hclngdescr(5)='Khalifa & Marshall non heated vertical surfaces '
      hcshtdescr(6)='Khalifa & M with fan or flor htg'
      hclngdescr(6)='Khalifa & Marshall fan in room or floor heating '
      hcshtdescr(7)='Khalifa & Marshall rad not @ win'
      hclngdescr(7)='Khalifa & Marshall room rad not under window    '
      hcshtdescr(8)='Khalifa & M fan htr wall opo fan'
      hclngdescr(8)='Khalifa & Mar fan heater in room wall oppos fan '
      hcshtdescr(9)='CEN for vertical surface (2.5)  '
      hclngdescr(9)='CEN for vertical surface (hc2.5)                '
      hcshtdescr(10)='Fisher & P for radial ceil diffu'
      hclngdescr(10)='Fisher & Pedersen for radial ceiling diffuser   '
      hcshtdescr(11)='Fisher for radial ceil diffuser '
      hclngdescr(11)='Fisher thesis for radial ceiling diffuser       '
      hcshtdescr(12)='Fisher for free horizontal jet  '
      hclngdescr(12)='Fisher for free horizontal jet                  '
      hcshtdescr(13)='Fisher thesis mixed convection  '
      hclngdescr(13)='Fisher thesis mixed convection radial ceil diffu'
      hcshtdescr(14)='Awbi and Hatton                 '
      hclngdescr(14)='Awbi and Hatton                                 '
      hcshtdescr(15)='Molina & Maestre vented channels'
      hclngdescr(15)='Molina & Maestre ventilated channels            '
      hcshtdescr(16)='Bar-Cohen ventilated channels   '
      hclngdescr(16)='Bar-Cohen buoyancy ventilated channels          '
      hcshtdescr(17)='ISO 15099 corr buoyancy in chan '
      hclngdescr(17)='ISO 15099 corr buoyancy flow in vented vert chan'
      hcshtdescr(18)='Gnielinski corr force vent chan '
      hclngdescr(18)='Gnielinski corr for (forced) vented vert channel'
      hcshtdescr(20)='Alamdari & Hammond vertical 2.6X'
      hclngdescr(20)='Alamdari & Hammond 2.65X test for vertical surf '
      hcshtdescr(21)='Alamdari & Hammond vertical 3X  '
      hclngdescr(21)='Alamdari & Hammond 3X test for vertical surfaces'
      hcshtdescr(22)='Alamdari & Hammond vertical 4.7X'
      hclngdescr(22)='Alamdari & Hammond 4.69X test for vertical surf '
      hcshtdescr(23)='Alamdari & Hammond vertical 1.7X'
      hclngdescr(23)='Alamdari & Hammond 1.76X test for vertical surf '
      hcshtdescr(30)='Khalifa & M window over radiator'
      hclngdescr(30)='Khalifa & Marshall window over radiator         '
      hcshtdescr(31)='Khalifa & M windows no radiator '
      hclngdescr(31)='Khalifa & Marshall windows w/o a radiator       '
      hcshtdescr(50)='Alamdari and Hammond horiz surf '
      hclngdescr(50)='Alamdari and Hammond horizontal surfaces        '
      hcshtdescr(51)='Halcrow time invariant low horiz'
      hclngdescr(51)='Halcrow time invariant low horizontal surfaces  '
      hcshtdescr(52)='Halcrow time invariant high horz'
      hclngdescr(52)='Halcrow time invariant high horizontal surfaces '
      hcshtdescr(53)='CIBSE for horizontal surfaces   '
      hclngdescr(53)='CIBSE for horizontal surfaces (hc4.3)           '
      hcshtdescr(54)='CEN for horizontal surfaces     '
      hclngdescr(54)='CEN for horizontal surfaces (hc5.0)             '
      hcshtdescr(55)='Awbi and Hatton horiz surfaces  '
      hclngdescr(55)='Awbi and Hatton horizontal surfaces             '
      hcshtdescr(60)='Khalifa & Marshall ceilings rad '
      hclngdescr(60)='Khalifa & Marshall ceilings rad @win or htd wall'
      hcshtdescr(61)='Khalifa & Marshall ceilings fan '
      hclngdescr(61)='Khalifa & Marshall ceilings fan htr or floor htg'
      hcshtdescr(70)='Alamdari and Hammond horiz strat'
      hclngdescr(70)='Alamdari and Hammond stably strat at horiz surf '
      hcshtdescr(71)='Halcrow invariant low strat     '
      hclngdescr(71)='Halcrow time invariant low stratified horiz     '
      hcshtdescr(72)='Halcrow invariant high strat    '
      hclngdescr(72)='Halcrow time invariant low stratified horiz     '
      hcshtdescr(73)='CIBSE (stratified) horizontal   '
      hclngdescr(73)='CIBSE (stratified) horizontal (hc1.5)           '
      hcshtdescr(74)='CEN (stratified) horizontal     '
      hclngdescr(74)='CEN (stratified) horizontal (hc0.7)             '
      hcshtdescr(80)='Fisher & P floor forced mixed   '
      hclngdescr(80)='Fisher & P floor forced mixed with rad ceil dif '
      hcshtdescr(81)='Fisher thesis floor forced mixed'
      hclngdescr(81)='Fisher thesis floor forced mixed w rad ceil dif '
      hcshtdescr(82)='Fisher thesis floor w forced jet'
      hclngdescr(82)='Fisher thesis floor forced mixed with horiz jet '
      hcshtdescr(83)='Fisher floor forced-free rad dif'
      hclngdescr(83)='Fisher floor forced-free with rad ceil diffu    '
      hcshtdescr(90)='Fisher & P ceil ASHRAE   '
      hclngdescr(90)='Fisher & P ceiling ASHRAE with radial ceil difus'
      hcshtdescr(91)='Fisher thesis ceil radial diffu '
      hclngdescr(91)='Fisher thesis ceiling with radial ceil difuser  '
      hcshtdescr(92)='Fisher thesis ceil free horz jet'
      hclngdescr(92)='Fisher thesis ceiling with free horizontal jet  '
      hcshtdescr(93)='Fisher thesis ceil mixed radial '
      hclngdescr(93)='Fisher thesis ceiling mixed with radial diffuser'
      hcshtdescr(99)='adaptive control                '
      hclngdescr(99)='adaptive control using fixed coefficients       '
      
      return
      end
