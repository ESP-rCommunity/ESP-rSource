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

C This file contains routines for creating and editing a zone
C convection-calculation control file:
C   HCFMK   Controlling routine for creating/editing convection file.
C   EHCMAN  Controls manual creation/editing of convection file.
C   EPHCF   Edits one period of convection-calculation control data.
C   EMKHTC  Writes convection file.
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

C geometry.h provides commons G0/G2/precz.
#include "geometry.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/user/browse

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)

      CHARACTER*72 LTMP
      character HFILE*72,head*33
      CHARACTER ITEM(31)*33
      character OUTSTR*124,outs*124
      logical OK,dok,modcfg,XST,clkok,browse
      integer NITEMS,INO ! max items and current menu item

C Short and long descriptions of correlations.
      character hcshtdescr*32  ! short description
      character hclngdescr*48   ! long description
      dimension hcshtdescr(-1:99),hclngdescr(-1:99)

#ifdef OSI
      integer icfg_type  ! model cfg type
      integer icfgz      ! if non-zero then there are zones
      integer icfgn      ! if non-zero then there are networks
      integer icfgc      ! if non-zero then cfg file known
      integer icfgdfn    ! if non-zero then cdf domain exists
      integer iicfgz     ! there are zone related images
      integer iicfgn     ! there are network related images
      integer iicfgc     ! there are control related images
      integer iicfgdfn   ! there are cfd related images
#else
      integer*8 icfg_type,icfgz,icfgn,icfgc,icfgdfn,iicfgz
      integer*8 iicfgn,iicfgc,iicfgdfn
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='HCFMK'  ! set for subroutine

C Before presenting hc regime, make up a suitable file name
C in case it is useful.
      modcfg=.false.
      IHCFP=0
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(HFILE,'(A,A4)')zname(ICOMP)(1:lnzname(ICOMP)),'.htc'
      else
        WRITE(HFILE,'(A,A,A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.htc'
      endif
      IF(IHC(ICOMP).EQ.1)THEN

C A convection file is referenced. If browsing open existing file and
C proceed to menu. If file does not exist return.
        if(browse)then
          call FINDFIL(LHCCO(ICOMP),XST)
          IF(XST)THEN
            call ehtcff(LHCCO(ICOMP),IUF,ITRU,IER)
            if(ier.eq.0)then
              goto 43
            else
              call usrmsg('Problem reading conv regime for this zone.',
     &                    'returning with no action taken.','W')
              ier=0
              return
            endif
          else
            call usrmsg('Could not find conv regime for this zone.',
     &                  'returning with no action taken.','W')
            ier=0
            return
          endif
        endif

C If not browsing confirm file name, 
C offer option to dereference. If ok check its format.
        LTMP=LHCCO(ICOMP)
        helptopic='zone_htc_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        clkok=.false.
  245   CALL EASKSCMD(LTMP,'Zone conv regime file ?',
     &    ' ','dereference',clkok,72,HFILE,'conv regime file',
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
              continue
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
     &            ' is not a HC control file.'
                dok=.false.
                helptopic='zone_htc_expected'
                call gethelptext(helpinsub,helptopic,nbhelp)
                CALL ASKOK(outs,
     &            'Overwrite file with current data (see help)?',
     &             OK,dok,nbhelp)
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

C Convection file is in current format so read contents.
          call ehtcff(LHCCO(ICOMP),IUF,ITRU,IER)
          if(ier.eq.-301)then
            dok=.true.
            helptopic='zone_htc_agree_save'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK(
     &      'Could not find a readable conv regime file for',
     &      'this zone. Do you want to create one?',OK,dok,nbhelp)
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
        if(browse)then
          call usrmsg('There is no conv regime for this zone.',
     &                'returning with no action taken.','W')
          ier=0
          return
        endif
        if(LHCCO(ICOMP)(1:2).eq.'  '.or.LHCCO(ICOMP)(1:4).eq.'UNKN')then
          LTMP=HFILE
        else
          LTMP=LHCCO(ICOMP)
        endif
        helptopic='zone_htc_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        clkok=.false.
  246   CALL EASKSCMD(LTMP,'Zone conv regime file ?',
     &    ' ','dereference',clkok,72,HFILE,'conv regime file',IER,7)
        if(clkok)then
          call edisp(iuout,'Canceling conv regime for this zone.')
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
              call ehtcff(LHCCO(ICOMP),IUF,ITRU,IER)
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
      itsnm=0
      itorg=1
      itgrd=1
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)
      call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
      if(MMOD.EQ.8)call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)

C Determine the convective regime (A-G)..
   20 INO = -2
      write(ITEM(1),'(a)') ' Buoyancy-driven flow         '
      ITEM(2) =            '   surf-to-air temp diff...   '
      ITEM(3) =            'a    in-floor heating         '
      ITEM(4) =            'b    heated wall panel        '
      ITEM(5) =            'c    other temp diff          '
      ITEM(6) =            '   heater located in room...  '
      ITEM(7) =            'd    heater under window      '
      ITEM(8) =            'e    heater not under window  '
      ITEM(9) =            '   open vertical channel...   '
      ITEM(10)=            'f    Molina & Maestre corl    '
      ITEM(11)=            'g    Bar-Cohen and Ros. corl  '
      ITEM(12)=            ' _________________________    '
      write(ITEM(13),'(a)')' Mechanically driven flow     '
      ITEM(14)=            '   air-based HVAC system...   '
      ITEM(15)=            'h    VAV with CV heating      '
      ITEM(16)=            'i    VAV with VV heating      '
      ITEM(17)=            'j    CV with variable temp    '
      ITEM(18)=            '   circulation within zone... '
      ITEM(19)=            'k    via circulation fan      '
      ITEM(20)=            ' ________________________     '
      ITEM(21) =           '  Mixed flow (buoyancy + mech)'
      ITEM(22)=            'l    VAV with CV heating      '
      ITEM(23)=            'm    VAV with VV heating      '
      ITEM(24)=            'n    CV with variable temp    '
      ITEM(25)=            ' _________________________    '
      ITEM(26)=            'o User defined or CEN values  '

      ITEM(27)=            '! list                        '
      ITEM(28)=            'p list available correlations '
      ITEM(29)=            '> save                        '
      ITEM(30)=            '? help                        '
      ITEM(31)=            '- exit                        '
      NITEMS=31

C Redraw wireframe on each return.
      CALL INLNST(1)
      itsnm=0
      itorg=1
      itgrd=1
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)

C Help text for this menu.
      helptopic='zone_htc_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Present options.
      write(head,'(2a)') ' Convective regime in ',zname(ICOMP)(1:11) 
      CALL EMENU(head,ITEM,NITEMS,INO)

      IF(INO.EQ.3)then

C Convective regime `A'. Buoyancy caused by in-floor heating.
        iopt=1
        call EconvA(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.4)then

C Convective regime `A'. Buoyancy caused by heated wall panel
        iopt=2
        call EconvA(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.5)then

C Convective regime `A'. Buoyancy caused by buoyancy other temp diff.
        iopt=3
        call EconvA(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.7)then

C Convective regime `B'. Buoyancy caused by heater under window.
        iopt=1
        call EconvB(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.8)then

C Convective regime `B'. Buoyancy caused by heater not under window.
        iopt=2
        call EconvB(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.10)then

C Open vertical channel convection - Molina & Maestre correlation        
        iopt=1
        call EconvF (ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.11)then

C Open vertical channel convection - Bar-Cohen correlation        
        iopt=2
        call EconvF (ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.15)then

C Convective regime `C'. Convection dominated by mechanical forces caused
C by an air-handling system delivering heated or cooled air to the room
C through ceiling or floor mounted diffusers. VAV with CV heating.
        iopt=1
        call EconvC(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.16)then

C Convective regime `C'. VAV with VV heating.
        iopt=2
        call EconvC(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.17)then

C Convective regime `C'. CV with variable temperature.
        iopt=3
        call EconvC(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.19)then

C Convective regime `D'.
C ==> Khalifa 8 for walls opposite fan (requires list of walls)
C     Khalifa 3 for walls not opposite fan
C     Khalifa 4 for ceilings
C     Alamdari-Hammond for floors
C     Toggle to Alamdari-Hammond for all surf when no heat or no cool
        call EconvD(ITRU,IUF,ICOMP,IER)

      ELSEIF(INO.EQ.22)then

C Convective regime `E'. Mixed flow model. VAV with CV heating.
        iopt=1
        call EconvE(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.23)then

C Convective regime `E'. Mixed flow model. VAV with VV heating.
        iopt=2
        call EconvE(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.24)then

C Convective regime `E'. Mixed flow model. CV with variable temperatures.
        iopt=3
        call EconvE(ITRU,IUF,ICOMP,iopt,IER)

      ELSEIF(INO.EQ.26)then

C User will manually create convection file.
        call EHCMAN(ITRU,IUF,ICOMP,IER)

      ELSEIF(INO.eq.27)then

C List current data.
        call listhtc(iuout,ICOMP,IER)

      ELSEIF(INO.eq.28)then

C Recover and list out the available correlations.
        call gethccornames(hcshtdescr,hclngdescr)
        call edisp(iuout,'Index description')
        write(outs,'(2a)') ' -1 ',hclngdescr(-1)
        call edisp(iuout,outs)
        do 42 ij=1,99
          if(hclngdescr(ij)(1:3).eq.'not')then
            continue
          else
            write(outs,'(i3,2a)') ij,' ',hclngdescr(ij)
            call edisp(iuout,outs)
          endif
  42    continue

      ELSEIF (INO.EQ.29)then

C Write data to file.
        CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

      ELSEIF (INO.EQ.30)then
      
C Display help info.
C Note that the ACA facility is described in section 5.2
C of the following thesis (available in PDF format from
C www.esru.strath.ac.uk). An example application is given
C in section 6.3.1 of this document.
C
C Beausoleil-Morrison I. (2000), The Adaptive Coupling
C of Heat and Air Flow Modelling within Dynamic
C Whole-Building Simulation, PhD Thesis,
C University of Strathclyde, Glasgow UK.
        helptopic='zone_htc_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('convective calculations',nbhelp,'-',0,0,IER)

      ELSEIF (INO.EQ.31)then
        RETURN
      ELSE
        goto 20
      ENDIF
      goto 20

C Error messages.
  392 call edisp(iuout,' Unable to rewind HC file.')
      return
      END


C ******************** EconvA ********************
C EconvA controls the input of data related to convective regime A, buoyancy
C        driven flow caused by surface-to-air temperature differences.
C IOPT: 1 = in-floor heating, 2 = heated wall panel, 3 = other.
      SUBROUTINE EconvA(ITRU,IUF,ICOMP,iopt,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      CHARACTER SN*12,outs*124,hcfpdescr*72
      logical OK,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EconvA'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP = 1
      ST(IPER) = 0.
      EN(IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(IPER) = 3
      hcfpdescr(IPER) = 'surf-to-air deltaT'

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
      dok=.true.
      helptopic='zone_htc_buoyancy_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call askok('View a synopsis of Bouyancy-driven flow',
     &           'options and background information?',ok,dok,nbhelp)
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
        hcfpdescr(IPER) = 'surf-to-air deltaT via in-floor heating'
        DO 10 ISUR=1,NZSUR(ICOMP)
          icc=izstocn(icomp,isur) 
          ANGLE = SSPELV(icc)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. Set control law to 1:
            CVdata(IPER,ISUR,1) = 1.

C Correlation (ICOR) to use when heating on:
            CVdata(IPER,ISUR,2) = 6.

C Correlation to use when heating off (-1 means sim toggle):
            CVdata(IPER,ISUR,3) = -1.

C Remaining fields not significant for control law 1:
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = 55.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = 61.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
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
        hcfpdescr(IPER) = 'surf-to-air deltaT via wall panel heating'
        DO 11 ISUR=1,NZSUR(ICOMP)
          icc=izstocn(icomp,isur) 
          ANGLE = SSPELV(icc)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. See `in-floor' heating for definitions of CVdata.
C Treatment for wall depends on whether it is a heated surface.
            CVdata(IPER,ISUR,1) = 1.
            SN=SSNAME(ICC)
            dok=.false.
            helptopic='htc_buoyancy_wall_panel'
            call gethelptext(helpinsub,helptopic,nbhelp)
            WRITE(outs,'(3a)') ' Is surface ',SN(1:lnblnk(SN)),
     &                         ' heated?'
            CALL ASKOK(' ',outs,OK,dok,nbhelp)
            if(OK)then

C Awbi-Hatton for heated walls.
              CVdata(IPER,ISUR,2) = 14.
            else

C Khalifa eq 6 for non-heated walls.
              CVdata(IPER,ISUR,2) = 5.
            endif
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = 70.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = 60.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          endif
   11   CONTINUE

      ELSEIF(IOPT.EQ.3)THEN

C Surface-air temperature difference caused by another factor, such as
C heat transfer through envelope, solar insolation, or a chilled ceiling panel.
C Regardless of state of plant injection, use correlations based on simulation
C toggle (Alamdari-Hammond by default).

C Process each surface in the zone.
        hcfpdescr(IPER) = 'surf-to-air deltaT via another factor'
        DO 12 ISUR=1,NZSUR (ICOMP)
          CVdata(IPER,ISUR,1) = 1.
          CVdata(IPER,ISUR,2) = -1.
          CVdata(IPER,ISUR,3) = -1.
          CVdata(IPER,ISUR,4) = 0.
          CVdata(IPER,ISUR,5) = 99.
          CVdata(IPER,ISUR,6) = 99.
          CVdata(IPER,ISUR,7) = 99.
          CVdata(IPER,ISUR,8) = 99.
   12   CONTINUE
      ENDIF

C Write .htc file then return to higher level menu.
      call edisp(iuout,'The selected HC regime is being saved.')
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

C Check if user wants to list current data.
      call edisp(iuout,'The current flow data follows...')
      call listhtc(iuout,ICOMP,IER)
      return
      END


C ******************** EconvB ********************
C EconvB controls the input of data related to convective regime B, buoyancy
C        driven flow caused by a heater located within the room.
C IOPT: 1 = heater under a window, 2 = heater not under a window. 
      SUBROUTINE EconvB(ITRU,IUF,ICOMP,iopt,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      CHARACTER SN*12,outs*124,h*72,hcfpdescr*72
      logical OK,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EconvB'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP = 1
      ST(IPER) = 0.
      EN(IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(IPER) = 3
      hcfpdescr(IPER) = 'heater w/in room '

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
      dok=.true.
      helptopic='zone_htc_device_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call askok('View a synopsis of Bouyancy-driven flow',
     &           'options and background information?',ok,dok,nbhelp)
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
        hcfpdescr(IPER) = 'convection from heater under window'
        DO 10 ISUR=1,NZSUR(ICOMP)
          icc=izstocn(icomp,isur) 
          ANGLE = SSPELV(icc)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. Set control law to 1:
            CVdata(IPER,ISUR,1) = 1.

C Treatment (ICOR) depends on whether this is a wall or window.
            if(SSOTF(icc)(1:4).ne.'OPAQ')then

C It's a window. Use Khalifa window correlation or wall correlation?
C Add some help here to explain that the window correlations may
C overpredict convection.
              SN=SSNAME(ICC)
              WRITE(outs,'(3a)') ' Use window eq for ',
     &                           SN(1:lnblnk(SN))
              dok=.false.
              helptopic='zone_htc_device_options'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL ASKOK(outs,'(see help)',OK,dok,nbhelp)
              if(OK)then
                CVdata(IPER,ISUR,2) = 30.
              else
                CVdata(IPER,ISUR,2) = 5.
              endif
            else

C It's a wall. No choice to make.
              CVdata(IPER,ISUR,2) = 5.
            endif

C Correlation (ICOR) to use when heating off (-1 means sim toggle):
            CVdata(IPER,ISUR,3) = -1.

C Remaining fields not significant for control law 1:
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = 70.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = 60.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
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
        hcfpdescr(IPER) = 'convection from heater (not under window)'
        DO 11 ISUR=1,NZSUR(ICOMP) 
          icc=izstocn(icomp,isur) 
          ANGLE = SSPELV(ICC)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. See `heater under window'for definitions of CVdata.
            CVdata(IPER,ISUR,1) = 1.

C Treatment depends on whether this is a wall or window.
            if(SSOTF(ICC)(1:4).ne.'OPAQ')then

C It's a window. Use Khalifa window correlation (ICOR=31)
C or wall correlation (ICOR=6)?
C Add some help here to explain that the window correlations may
C overpredict convection.
              SN=SSNAME(ICC)
              WRITE(outs,'(3a)') ' Use window eq for ',
     &                           SN(1:lnblnk(SN))
              dok=.false.
              helptopic='zone_htc_device_options'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL ASKOK(outs,'(see help)',OK,dok,nbhelp)
              if(OK)then
                CVdata(IPER,ISUR,2) = 31.
              else
                CVdata(IPER,ISUR,2) = 6.
              endif
            else

C It's a wall. Treatment depends on location relative to heater.
              SN=SSNAME(ICC)
              WRITE(outs,'(3a)') ' Is surface ',SN(1:lnblnk(SN)),
     &                           ' close to the heater?'
              dok=.false.
              helptopic='zone_htc_device_options'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL ASKOK(outs,'(see help)',OK,dok,nbhelp)
              if(OK)then
                CVdata(IPER,ISUR,2) = 7.
              else
                CVdata(IPER,ISUR,2) = 6.
              endif
            endif
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = 70.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = 61.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          endif
   11   CONTINUE
      ENDIF

C Write .htc file then return to higher level menu.
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

C List current data.
      call listhtc(iuout,ICOMP,IER)
      return

      END


C ******************** EconvC ********************
C EconvC controls the input of data related to convective regime C, where
C        convection is dominated by mechanical forces caused by an air-handling
C        system delivering heated or cooled air to the room through ceiling
C        or wall mounted diffusers.
C IOPT: 1 = VAV with CV heating, 2 = VAV with VV heating, 3 = CV variable T.
C       4 = ??
      SUBROUTINE EconvC(ITRU,IUF,ICOMP,iopt,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER outs*124,hcfpdescr*72
      character*24 hcphrase(5)
      CHARACTER*25 ITEM(15)
      logical ok,dok
      real airflow   ! temprary value for editing
      integer NITEMS,Ioa,Idiff,Ifor ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EconvC'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP = 1
      ST(IPER) = 0.
      EN(IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(IPER) = 3
      hcfpdescr(IPER) = 'mechanical dominated'

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
  20  dok=.true.
      helptopic='zone_htc_mechanical_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call askok('View a synopsis of Mechanically-driven flow',
     &            'options and background information?',ok,dok,nbhelp)
      if(OK)then

C Add lots of help here.
        helptopic='zone_htc_mechanical_opt'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('HVAC system type',nbhelp,'-',0,0,IER)
      endif

C Define system type for .htc file. Same system for all surfaces:
      Isystp = IOPT+1
      do 50 ISUR=1,NZSUR(ICOMP)
        CVdata(IPER,ISUR,1) = Isystp
   50 continue

C Get HVAC air flow rate and temperature control parameters.
      IF(Isystp.eq.4)THEN

C Constant-volume variable-temperature system. Get air flow rate,
C and minimum and maximum allowable diffuser temperatures.
        hcphrase(1) = 'convec. via CVVT'

        helptopic='zone_htc_mech_sys_4'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(outs,'(a)')' air flow rate in ac/h'
        airflow=CVdata(IPER,1,2)  ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,nbhelp)
        do 51 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,2) = airflow
   51   continue

        WRITE(outs,'(a)')' minimum supply air temp'
        Tmin=CVdata(IPER,1,4)  ! give it a typical value
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'min T',IER,nbhelp)
        do 52 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,4) = Tmin
   52   continue

        WRITE(outs,'(a)')' maximum supply air temp'
        Tmax=CVdata(IPER,1,3)  ! give it a typical value
        CALL EASKR(Tmax,outs,' ',
     &             5.0,'W',80.,'W',43.0,'max T',IER,nbhelp)
        do 53 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,3) = Tmax
          CVdata(IPER,ISUR,6) = 99.
   53   continue

      ELSE

C Variable-volume system. Get cooling setpoint temperature for supplied
C air, the maximum allowable diffuser temperature, and the minimum and
C and maximum air flow rates.
        hcphrase(1) = 'convec. via VAV VT'
        helptopic='zone_htc_mech_VAV_VT'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(outs,'(a)')' diffuser setpoint temp for cooling'
        Tmin=CVdata(IPER,1,2)  ! give it a typical value
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'setpoint T',IER,nbhelp)
        do 54 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,2) = Tmin
   54   continue

        WRITE(outs,'(a)')' min air flow rate in ac/h'
        airflow=CVdata(IPER,1,3) ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,nbhelp)
        do 55 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,3) = airflow
   55   continue

        WRITE(outs,'(a)')' max air flow rate in ac/h'
        airflow=CVdata(IPER,1,4) ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',9.0,'ac/h',IER,nbhelp)
        do 56 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,4) = airflow
   56   continue

        WRITE(outs,'(a)')' max diffuser temperature'
        Tmax=CVdata(IPER,1,6) ! give it a typical value
        CALL EASKR(Tmax,outs,' ',
     &             15.0,'W',80.,'W',43.0,'max supply T',IER,nbhelp)
        do 57 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,6) = Tmax
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
      helptopic='zone_htc_mech_menu'
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
        CALL PHELPD('Outdoor air treated as',23,'-',0,0,IER)
        goto 23
      elseif(Ioa.eq.5)then

C Instead of going to further detail they have asked to return
        CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)
        return
      endif

C Define outdoor air treatment for for .htc file:
      Ioa = Ioa-1
      do 58 ISUR=1,NZSUR(ICOMP)
        CVdata(IPER,ISUR,5) = Ioa
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
          CVdata(IPER,ISUR,7) = Ifor
          CVdata(IPER,ISUR,8) = -1.
   10   CONTINUE

       ELSEIF(Idiff.EQ.2)THEN

C Air supplied through wall diffuser: free horizontal jet.
C Set correlation for each surface in zone.
        hcphrase(3) = 'wall diffuser'
        DO 11 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,7) = 3.
          CVdata(IPER,ISUR,8) = -1.
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
      write(hcfpdescr(IPER),'(5a)') hcphrase(1)(1:lhcp1),' ',
     &  hcphrase(2)(1:lhcp2),' ',hcphrase(3)(1:lhcp3)

C List current data.
      call listhtc(iuout,ICOMP,IER)

C All data has been acquired. Write .htc file then return to higher
C level menu.
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

      RETURN
      END


C ******************** EconvD ********************
C EconvD controls the input of data related to convective regime D, mechanically
C        driven flow caused by a circulating fan within the room. There is no
C        intentional supply or extract of air from the room.
      SUBROUTINE EconvD(ITRU,IUF,ICOMP,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      CHARACTER SN*12,outs*124,hcfpdescr*72
      logical OK,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EconvD'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP = 1
      ST(IPER) = 0.
      EN(IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(IPER) = 3
      hcfpdescr(iper) = 'convection via fan in room'

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
      dok=.true.
      helptopic='zone_htc_circ_fan_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call askok(
     &  'View a synopsis of mechanically driven flow caused by a ',
     &  'circulating fan within the room options?',
     &  ok,dok,nbhelp)
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
        icc=izstocn(icomp,isur) 
        ANGLE = SSPELV(icc)
        if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. Set control law to 1:
          CVdata(IPER,ISUR,1) = 1.

C Treatment (ICOR) depends on whether fan blows directly on the surface.
          SN=SSNAME(ICC)
          WRITE(outs,'(3a)') ' Does fan blow directly on surface ',
     &                       SN(1:lnblnk(SN))
          dok=.false.
          helptopic='zone_htc_circ_fan_synop'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(outs,'(see help)',OK,dok,nbhelp)
          if(OK)then
            CVdata(IPER,ISUR,2) = 8.
          else
            CVdata(IPER,ISUR,2) = 6.
          endif

C Correlation (ICOR) to use when heating off (-1 means sim toggle):
          CVdata(IPER,ISUR,3) = -1.

C Remaining fields not significant for control law 1:
          CVdata(IPER,ISUR,4) = 0.
          CVdata(IPER,ISUR,5) = 99.
          CVdata(IPER,ISUR,6) = 99.
          CVdata(IPER,ISUR,7) = 99.
          CVdata(IPER,ISUR,8) = 99.
        elseif(ANGLE.LT.-45.) then
C Floor.
          CVdata(IPER,ISUR,1) = 1.
          CVdata(IPER,ISUR,2) = 70.
          CVdata(IPER,ISUR,3) = -1.
          CVdata(IPER,ISUR,4) = 0.
          CVdata(IPER,ISUR,5) = 99.
          CVdata(IPER,ISUR,6) = 99.
          CVdata(IPER,ISUR,7) = 99.
          CVdata(IPER,ISUR,8) = 99.
        elseif(ANGLE.GT.45.) then
C Ceiling.
          CVdata(IPER,ISUR,1) = 1.
          CVdata(IPER,ISUR,2) = 61.
          CVdata(IPER,ISUR,3) = -1.
          CVdata(IPER,ISUR,4) = 0.
          CVdata(IPER,ISUR,5) = 99.
          CVdata(IPER,ISUR,6) = 99.
          CVdata(IPER,ISUR,7) = 99.
          CVdata(IPER,ISUR,8) = 99.
        endif
   10 CONTINUE

C List current data.
      call listhtc(iuout,ICOMP,IER)

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
      SUBROUTINE EconvE(ITRU,IUF,ICOMP,iopt,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER outs*124,hcfpdescr*72
      character*24 hcphrase(5)
      CHARACTER*25 ITEM(15)
      logical ok,dok
      integer NITEMS,Ioa ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EconvE'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP = 1
      ST(IPER) = 0.
      EN(IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(IPER) = 3
      hcfpdescr(iper) = 'mixed flow ceil difusers'

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
      dok=.true.
      helptopic='zone_htc_mixed_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call askok(
     &  'View synopsis of mixed Bouyancy-driven and mechanical',
     &  'flow options and background information?',ok,dok,2)
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
        CVdata(IPER,ISUR,1) = Isystp
   50 continue

C Get HVAC air flow rate and temperature control parameters.
      IF(Isystp.eq.4)THEN

C Constant-volume variable-temperature system. Get air flow rate,
C and minimum and maximum allowable diffuser temperatures.
        hcphrase(1) = 'mixed convec. via CVVT'
        helptopic='zone_htc_CVVT'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(outs,'(a)')' air flow rate in ac/h'
        airflow=CVdata(IPER,1,2)   ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,3)
        do 51 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,2) = airflow
   51   continue

        WRITE(outs,'(a)')' minimum supply air temp'
        Tmin=CVdata(IPER,1,4)   ! give it a typical value 
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'min T',IER,nbhelp)
        do 52 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,4) = Tmin
   52   continue

        WRITE(outs,'(a)')' maximum supply air temp'
        Tmax=CVdata(IPER,1,3)   ! give it a typical value 
        CALL EASKR(Tmax,outs,' ',
     &             5.0,'W',80.,'W',43.0,'max T',IER,nbhelp)
        do 53 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,3) = Tmax
          CVdata(IPER,ISUR,6) = 99.
   53   continue

      ELSE

C Variable-volume system. Get cooling setpoint temperature for supplied
C air, the maximum allowable diffuser temperature, and the minimum and
C and maximum air flow rates.
        hcphrase(1) = 'mixed conv. via VAV VT'
        helptopic='zone_htc_VAVVT'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(outs,'(a)')' diffuser setpoint temp for cooling'
        Tmin=CVdata(IPER,1,2)   ! give it a typical value 
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'setpoint T',IER,nbhelp)
        do 54 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,2) = Tmin
   54   continue

        WRITE(outs,'(a)')' min air flow rate in ac/h'
        airflow=CVdata(IPER,1,3)  ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,nbhelp)
        do 55 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,3) = airflow
   55   continue

        WRITE(outs,'(a)')' max air flow rate in ac/h'
        airflow = CVdata(IPER,1,4)  ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',9.0,'ac/h',IER,nbhelp)
        do 56 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,4) = airflow
   56   continue

        WRITE(outs,'(a)')' max diffuser temperature'
        Tmax=CVdata(IPER,1,6)  ! give it a typical value
        CALL EASKR(Tmax,outs,' ',
     &             15.0,'W',80.,'W',43.0,'max supply T',IER,nbhelp)
        do 57 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,6) = Tmax
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
        CVdata(IPER,ISUR,5) = Ioa
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
        CVdata(IPER,ISUR,7) = 4.
        CVdata(IPER,ISUR,8) = -1.
   10 CONTINUE

C Concatenate the phrases to make hcfpdescr.
      lhcp1=lnblnk(hcphrase(1))
      lhcp2=lnblnk(hcphrase(2))
      lhcp3=lnblnk(hcphrase(3))
      write(hcfpdescr(IPER),'(5a)') hcphrase(1)(1:lhcp1),' ',
     &  hcphrase(2)(1:lhcp2),' ',hcphrase(3)(1:lhcp3)

C List current data.
      call listhtc(iuout,ICOMP,IER)

C All data has been acquired. Write .htc file then return to higher
C level menu.
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

      RETURN
      END


C ******************** EHCMAN ********************
C EHCMAN controls manual creation/editing of convection file.

      SUBROUTINE EHCMAN(ITRU,IUF,ICOMP,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)

      character outs*124,hcfpdescr*72
      CHARACTER*32 ITEM(13)
      integer NITEMS,INO ! max items and current menu item
      integer ihcfpt    ! for local editing.
      real valin,valout ! for initial values

C Short and long descriptions of correlations.
      character hcshtdescr*32  ! short description
      character hclngdescr*48   ! long description
      dimension hcshtdescr(-1:99),hclngdescr(-1:99)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EHCMAN'  ! set for subroutine

   20 INO = -2
      ITEM(1) ='a Initialise fixed hc values '
      ITEM(2) ='  _________________________  '
      write(ITEM(3),'(a,I2)') '  Periods: ',IHCFP
      ITEM(4) ='1 Add a period               '
      ITEM(5) ='2 Edit a period              '
      ITEM(6) ='3 Delete a period            '
      ITEM(7) ='  _________________________  '
      ITEM(8) ='* Find hc (equivalent m/s)   '
      ITEM(9) ='! List current data          '
      ITEM(10)='c List available correlations'
      ITEM(11)='> Save                       '
      ITEM(12)='? Help                       '
      ITEM(13)='- Exit                     '
      NITEMS=13

C Help text for this menu.
      helptopic='zone_htc_manual_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU(' User defined or CEN values',ITEM,NITEMS,INO)
      IF(INO.EQ.1)then

C Create from scratch. Initialise values to default value before
C calling editing routine. If fixed coefficients allow user to
C specify inside or outside or both values, otherwise set to default.
        ihcfpt=ihcfp
        CALL EASKI(ihcfpt,' ',' Number of control periods ?',
     &          1,'F',MP,'F',1,'no periods',IERI,nbhelp)
        if(ieri.eq.-3)then
          goto 20
        elseif(ieri.eq.0)then
          ihcfp=ihcfpt
        endif
        NS=NZSUR(icomp)
        IW=1
        CALL EASKATOG('calculation control (see help):',' ',
     &    'fixed coefficients','fixed correlations','adaptive cor',
     &    'CEN coef','cancel',' ',' ',IW,nbhelp)
        if(IW.eq.2.or.IW.eq.3.or.IW.eq.4)then
          valin= -1.0
          valout= -1.0
        elseif(iw.eq.5)then
          goto 20
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
     &        0.,'F',1000.,'F',-1.0,'inside hc',IER,nbhelp)
            valout= -1.0
          elseif(INZOPT.eq.3)then
            valin= -1.0
            CALL EASKR(valin,' ','Initial inside coef [W/m2/degC]:',
     &      0.,'F',1000.,'F',-1.0,'inside hc',IER,nbhelp)
            valout= -1.0
            CALL EASKR(valout,' ','Initial outside coef [W/m2/degC]:',
     &      0.,'F',1000.,'F',-1.0,'outside hc',IER,nbhelp)
          elseif(INZOPT.eq.4)then
            ihcfp=0  ! reset
            goto 20
          endif
        endif
        DO 250 I = 1,IHCFP
          ICTL(I) = iw
          hcfpdescr(I) = 'User specified convection coefficients'
          do 251 J = 1,NS
            HCI(I,J) = valin
            HCE(I,J) = valout
            IHCI(I,J) = -1
            IHCE(I,J) = -1
            do 253 jj=1,8
              CVdata(I,J,jj) = 0.
  253       continue
  251     continue

C << why call this - this is for subsequent editing >>
          call EPHCF(ICOMP,I,NS,IER)
  250   CONTINUE
        goto 20  ! redisplay the menue

      ELSEIF(INO.EQ.4)then

C Add control period. Ask about defaults or not.
        if(IHCFP.lt.MP)then

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
     &        0.,'F',1000.,'F',-1.0,'inside hc',IER,nbhelp)
            valout= -1.0
          elseif(INZOPT.eq.3)then
            valin= -1.0
            CALL EASKR(valin,' ','Initial inside coef [W/m2/degC]:',
     &        0.,'F',1000.,'F',-1.0,'inside hc',IER,nbhelp)
            valout= -1.0
            CALL EASKR(valout,' ','Initial outside coef [W/m2/degC]:',
     &        0.,'F',1000.,'F',-1.0,'outside hc',IER,nbhelp)
          elseif(INZOPT.eq.4)then
            goto 20
          endif
          IHCFP = IHCFP + 1
          IN = IHCFP
          do 252 J = 1,NS
            HCI(IN,J)= valin
            HCE(IN,J)= valout
            IHCI(IN,J) = -1
            IHCE(IN,J) = -1
            do 254 jj=1,8
              CVdata(IN,J,jj) = 0.
  254       continue
  252     continue
          call EPHCF(ICOMP,IN,NS,IER)
        else
          call edisp(iuout,' File already contains 3 control periods.')
        endif

      ELSEIF(INO.EQ.5)then

C Change control period.
        CALL EASKI(IN,' ',' Which control period ? ',
     &          1,'F',IHCFP,'F',1,'change period',IERI,nbhelp)
        if(ieri.eq.-3)then
          goto 20
        else
          call EPHCF(ICOMP,IN,NS,IER)
        endif

      ELSEIF(INO.EQ.6)then

C Delete control period.
        CALL EASKI(IN,' ',' Delete which control period ? ',
     &          1,'F',IHCFP,'F',1,'delete period',IERI,nbhelp)
        if(ieri.eq.-3)then
          goto 20
        endif
        IF (IN .EQ. IHCFP)  GOTO  790
        J01=IN+1
        DO 740 I=J01,IHCFP
          ST(I-1)=ST(I)
          EN(I-1)=EN(I)
          DO 750 K=1,NS
            HCI(I-1,K)=HCI(I,K)
            HCE(I-1,K)=HCE(I,K)
            IHCI(I-1,K) = IHCI(I,K)
            IHCE(I-1,K) = IHCE(I,K)
            do 255 jj=1,8
              CVdata(I-1,K,jj) = CVdata(I,K,jj)
  255       continue
  750     CONTINUE
  740   CONTINUE
  790   IHCFP = IHCFP -1

      ELSEIF(INO.EQ.8)then

C Given a velocity, give approx hc for flow || with face.
        V1=0.15
        CALL EASKR(V1,' ','Air velocity (m/s) [see help message] : ',
     &          0.,'F',24.,'F',0.15,'air velocity',IER,nbhelp)
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

      ELSEIF(INO.EQ.9)then

C List HC control data.
        call listhtc(iuout,ICOMP,IER)


      ELSEIF(INO.EQ.10)then

C Recover and list out the available correlations.
        call gethccornames(hcshtdescr,hclngdescr)
        call edisp(iuout,'Index description')
        write(outs,'(2a)') ' -1 ',hclngdescr(-1)
        call edisp(iuout,outs)
        do 42 ij=1,99
          if(hclngdescr(ij)(1:3).eq.'not')then
            continue
          else
            write(outs,'(i3,2a)') ij,' ',hclngdescr(ij)
            call edisp(iuout,outs)
          endif
  42    continue

      ELSEIF(INO .EQ. 11)then

C Write data to file.
        CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)
      ELSEIF (INO .EQ. 12)then

        helptopic='zone_htc_manual_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('conv coef data',nbhelp,'-',0,0,IER)

      ELSEIF (INO .EQ.13)then
        RETURN
      else
        goto 20
      endif
      goto 20

      END

C ******************** EconvF ********************
C EconvF controls the input of data related to convective regime F, buoyancy
C        driven flow in open vertical channels.
C Derived from code in EconvA.
C IOPT: 1 = Molina & Maestre, 2 = Bar-Cohen correlation.
      SUBROUTINE EconvF(ITRU,IUF,ICOMP,IOPT,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C IAORZ is the air origin zone. '0' if outdoor air, zone number otherwise.
C CWIDTH and CHEIGHT are the channel width and height
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT
      common/HCFPHI/hcfpdescr(MP)

      CHARACTER outs*124
      character hcfpdescr*72
      character*24 hcphrase(5)
      logical ok,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EconvF'  ! set for subroutine

C These approaches are active for all time-steps of the simulation
C (i.e. there is only one convection-calculation control period).
      IPER = 1
      IHCFP = 1
      ST(IPER) = 0.
      EN(IPER) = 24.

C All employ `type 3', or `adaptive', control over the convection calculations.
      ICTL(IPER) = 3

C: Initalize channel variables
      IAORZ=-1
      CWIDTH=0.0
      CHEIGHT=0.0

C Use the G7 orientation of the surfaces in the zone.
C Provide a synopsis.
      dok=.true.
      helptopic='zone_htc_vert_chan_ok'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call askok('View a synopsis of Bouyancy-driven flow in open ',
     &  'vertical channel options and background information?',
     &  ok,dok,nbhelp)
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
          CALL ASKZONE(IAORZ,0,' Air enters channel from which zone?',
     &      'Air origin zone','-','A zone must be selected',ier)
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
          icc=izstocn(icomp,isur) 
          ANGLE = SSPELV(icc)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. Apply correlation 
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = 15.
            CVdata(IPER,ISUR,3) = 15.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
           elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = -1.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = -1.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
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
        CALL EASKR(CWIDTH,'Channel width ?',
     &    ' ',0.01,'W',1.0,'W',0.05,'Channel width',IER,nbhelp) 

C Ask Channel height
        CALL EASKR(CHEIGHT,'Channel height ?',
     &    ' ',0.1,'W',100.0,'W',1.0,'Channel height',IER,nbhelp) 

C Examine each surface in the zone. Select ICOR based on surface orientation.
C ANGLE=0 for walls and other vertical surfaces; ANGLE=-90 for floors;
C and ANGLE=+90 for ceilings.
        DO 11 ISUR=1,NZSUR(ICOMP)
          icc=izstocn(icomp,isur) 
          ANGLE = SSPELV(icc)
          if(ANGLE.LE.45..AND.ANGLE.GE.-45.) then

C Vertical surface. Apply correlation 
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = 16.
            CVdata(IPER,ISUR,3) = 16.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.LT.-45.) then
C Floor.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = -1.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          elseif(ANGLE.GT.45.) then
C Ceiling.
            CVdata(IPER,ISUR,1) = 1.
            CVdata(IPER,ISUR,2) = -1.
            CVdata(IPER,ISUR,3) = -1.
            CVdata(IPER,ISUR,4) = 0.
            CVdata(IPER,ISUR,5) = 99.
            CVdata(IPER,ISUR,6) = 99.
            CVdata(IPER,ISUR,7) = 99.
            CVdata(IPER,ISUR,8) = 99.
          endif
   11   CONTINUE
      ENDIF

C Concatenate the phrases to make hcfpdescr.
      lhcp1=lnblnk(hcphrase(1))
      lhcp2=lnblnk(hcphrase(2))
      lhcp3=lnblnk(hcphrase(3))
      write(hcfpdescr(IPER),'(5a)') hcphrase(1)(1:lhcp1),' ',
     &  hcphrase(2)(1:lhcp2),' ',hcphrase(3)(1:lhcp3)

C Write .htc file then return to higher level menu.
      CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)
      RETURN

      END

C ******************** EPHCF ********************
C EPHCF edits one period of convection-calculation control data.
C IZ is the zone.
C IN is the period index
C NS is the number of surface in the zone << why pass this value? >>
      SUBROUTINE EPHCF(IZ,IN,NS,IER)
#include "epara.h"
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/HCCEN/CENLOC(MP,14)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      character outs*124,SN*12,SNH*16,ITEM(MS+5)*34,KEY*1
      CHARACTER stuff*10
      character hcfpdescr*72
      logical close,ok,dok
      integer IW  ! for radio button
      integer NITEMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EPHCF'  ! set for subroutine

      NS=NZSUR(IZ)

      helptopic='zone_htc_control_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Get time period for control interval.
      V1=ST(IN)
      CALL EASKR(V1,' ',' Start time ? ',
     &          0.,'F',24.,'F',0.,'start',IER,nbhelp)
      ST(IN)=V1
      V2=EN(IN)
      call eclose(V2,0.0,0.001,close)
      if(close) V2=24.
      CALL EASKR(V2,' ',' End time ? ',
     &          V1,'F',24.,'F',0.,'start',IER,nbhelp)
      EN(IN)=V2

  19  continue

C Determine type of control over calculations.
      helptopic='zone_htc_control_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)
      IW=1
      CALL EASKATOG('calculation control (see help):',' ',
     &  'fixed coefficients','fixed correlations','adaptive cor',
     &  'CEN coef','cancel',' ',' ',IW,nbhelp)
      IF(iw.eq.5) RETURN
      ICTL(IN) = iw

C There is one IF ENDIF block for each of the options.
      IF(iw.eq.1) THEN

C Fixed coefficients control: .
        hcfpdescr(IN) = 'User supplied hc values'

C Initialise zone vertex menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
        MHEAD=1
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
            icc=izstocn(iz,l)
            CALL EMKEY(L,KEY,IER)
            WRITE(ITEM(M),'(a,1x,3a,2f7.2)') KEY,SSNAME(icc),' ',
     &        SSVFC(icc),HCI(IN,L),HCE(IN,L)
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

        CALL EMENU('Convection coefficients',ITEM,NITEMS,INO)

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
          icc=izstocn(iz,is) 
          write(outs,'(a,i3,1x,a)')'Hc values for surface',is,
     &      SSNAME(ICC)(1:lnblnk(SSNAME(ICC)))
          write (SNH,'(2f7.2)') HCI(IN,IS),HCE(IN,IS)
          CALL EASKS(SNH,outs,'inside, outside?',16,' -1.0 -1.0 ',
     &      'hc values',IER,nbhelp)
          K=0
          CALL EGETWR(SNH,K,HCI(IN,IS),-1.5,1000.,'F','hci value',IER)
          CALL EGETWR(SNH,K,HCE(IN,IS),-1.5,1000.,'F','hce value',IER)
        else

C Not one of the legal menu choices.
          INO=-1
          goto 3
        endif
        goto 20
 99     continue
      ENDIF

C Selected correlations control: Each correlation is identified 
C by an index. Refer to /usr/esru/src/esp-r/esrubld/convect1.F,
C subroutine HTOVER over-rides the normal treatment of surface
C heat transfer. Also see SELCOR2 and HTBOUY.
      IF(iw.eq.2) THEN
        helptopic='zone_htc_correlat_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        hcfpdescr(IN) = 'User supplied hc correlations'
        DO 71 J=1,NS
          icc=izstocn(iz,j) 
          SN=SSNAME(icc)
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Correlation for surface',J,
     &           SN(1:lnblnk(SN)),' inside : period',IN
          IH=IHCI(IN,J)
          CALL EASKI(IH,outs,' (-1.= simulation toggle = default) ? ',
     &      -2,'W',100,'W',-1,'correlation inside',IERI,nbhelp)
          if(ieri.eq.-3)then
            continue
          else
            IHCI(IN,J)=IH
          endif
   71   CONTINUE
        DO 72 J=1,NS
          icc=izstocn(iz,j) 
          SN=SSNAME(icc)
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Correlation for surface',J,
     &           SN(1:lnblnk(SN)),' outside : period',IN
          IH=IHCE(IN,J)
          CALL EASKI(IH,outs,' (-1.= simulation toggle = default) ? ',
     &      -2,'W',100,'W',-1,'correlation outside',IERI,nbhelp)
          if(ieri.eq.-3)then
            continue
          else
            IHCE(IN,J)=IH
          endif
   72   CONTINUE
      ENDIF

C Adaptive control.
      IF(iw.eq.3) THEN
        helptopic='zone_htc_adaptive_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        hcfpdescr(IN) = 'User supplied adaptive hc correlations'
        DO 81 J=1,NS
          icc=izstocn(iz,j) 
          SN=SSNAME(icc)
          Ilaw=99
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Control law for surface',J,
     &           SN(1:lnblnk(SN)),' inside : period',IN
          CALL EASKI(Ilaw,outs,' (99=no control for this surface)',
     &      0,'W',100,'W',99,'control law inside',IERI,nbhelp)
          if(ieri.eq.-3)then
            goto 81
          endif
          CVdata(IN,J,1) = Ilaw

C Control law 1 (for when heated and not heated).
          if(Ilaw.eq.1) then
            helptopic='zone_htc_adaptive_c1'
            call gethelptext(helpinsub,helptopic,nbhelp)
  102       CALL EASKS(stuff,' correlation indices',
     &        ' ON & OFF heating states',10,' -1  -1',
     &        'data for adaptive law 1',IER,nbhelp)
            K=0
            CALL EGETWI(stuff,K,itmp,-1,100,'F','corr ON',IER)
            if(ier.ne.0) goto 102
            CVdata(IN,J,2) = itmp
            CALL EGETWI(stuff,K,itmp,-1,100,'F','corr OFF',IER)
              if(ier.ne.0) goto 102
            CVdata(IN,J,3) = itmp

C Fourth data item not used for control law 1.
            CVdata(IN,J,4) = 0.

C No control.
          elseif(Ilaw.eq.99) then
            CVdata(IN,J,2) = 99.
            CVdata(IN,J,3) = 99.
            CVdata(IN,J,4) = 99.
C Other control laws not yet supported.
          else
          endif
   81   CONTINUE

        helptopic='zone_htc_adaptive_sim'
        call gethelptext(helpinsub,helptopic,nbhelp)
        DO 82 J=1,NS
          icc=izstocn(iz,j) 
          SN=SSNAME(icc)
          Ilaw=99
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Control law for surface',J,
     &           SN(1:lnblnk(SN)),' outside : period',IN
          CALL EASKI(Ilaw,outs,' (99=no control for this surface)',
     &      0,'W',100,'W',99,'adapt control law outside',IERI,nbhelp)
          if(ieri.eq.-3)then
            goto 82
          endif
          CVdata(IN,J,5) = Ilaw

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
            CVdata(IN,J,6) = itmp
            CALL EGETWI(stuff,K,itmp,-1,100,'F','corr OFF',IER)
              if(ier.ne.0) goto 103
            CVdata(IN,J,7) = itmp

C Eight data item not used for control law 1.
            CVdata(IN,J,8) = 0.

C No control.
          elseif(Ilaw.eq.99) then
            CVdata(IN,J,6) = 99.
            CVdata(IN,J,7) = 99.
            CVdata(IN,J,8) = 99.
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
        ibase=nint(CENLOC(IN,1))
        itop=nint(CENLOC(IN,2))
        if(ibase.eq.0.and.itop.eq.0)then
          helptopic='zone_htc_cen_default'
          call gethelptext(helpinsub,helptopic,nbhelp)
          dok=.true.
          CALL ASKOK('This is probably your initial description for',
     &      'this zone. Do you want to use default values (see help)?',
     &      OK,dok,nbhelp)
          if(ok)then
            CENLOC(in,3)=5.0; CENLOC(in,4)=0.7; CENLOC(in,5)=8.0
            CENLOC(in,6)=5.0; CENLOC(in,7)=0.7; CENLOC(in,8)=8.0
            CENLOC(in,9)=5.0; CENLOC(in,10)=0.7; CENLOC(in,11)=8.0
            CENLOC(in,12)=2.5; CENLOC(in,13)=2.5; CENLOC(in,14)=8.0
            hcfpdescr(IN) = 'CEN default hc coefficients'
          endif
        endif

C Initial menu entry setup.
  21    IER=0
        INO=-2
        ibase=nint(CENLOC(IN,1))
        if(ibase.ne.0)then
          icc1=izstocn(iz,ibase) 
          write(item(1),'(2a)') 'a base surf is ',SSNAME(icc1)
        else
          write(item(1),'(2a)') 'a base surf is ','not yet defined'
        endif
        itop=nint(CENLOC(IN,2))
        if(itop.ne.0)then
          icc2=izstocn(iz,itop) 
          write(item(2),'(2a)') 'b top surf is ',SSNAME(icc2)
        else
          write(item(2),'(2a)') 'b top surf is ','not yet defined'
        endif
        write(item(3),'(a)')  ' ---floor--------      '
        write(item(4),'(a,f7.3)') 'c upward flow hc   ',CENLOC(IN,3)
        write(item(5),'(a,f7.3)') 'd downward flow hc ',CENLOC(IN,4)
        write(item(6),'(a,f7.3)') 'e outside hc       ',CENLOC(IN,5)
        write(item(7),'(a)')  ' --ceiling-------      '
        write(item(8),'(a,f7.3)') 'f upward flow hc   ',CENLOC(IN,6)
        write(item(9),'(a,f7.3)') 'g downward flow hc ',CENLOC(IN,7)
        write(item(10),'(a,f7.3)')'h outside hc       ',CENLOC(IN,8)
        write(item(11),'(a)') ' --sloped--------      '
        write(item(12),'(a,f7.3)')'i upward flow hc   ',CENLOC(IN,9)
        write(item(13),'(a,f7.3)')'j downward flow hc ',CENLOC(IN,10)
        write(item(14),'(a,f7.3)')'k outside hc       ',CENLOC(IN,11)
        write(item(15),'(a)') ' --wall----------      '
        write(item(16),'(a,f7.3)')'l inside hc        ',CENLOC(IN,12)
        write(item(17),'(a,f7.3)')'m partition hc     ',CENLOC(IN,13)
        write(item(18),'(a,f7.3)')'n outside hc       ',CENLOC(IN,14)
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
          ibase=nint(CENLOC(IN,1))
          itop=nint(CENLOC(IN,2))
          if(ibase.eq.0.and.itop.eq.0)then
            dok=.false.
            CALL ASKOK('The base and top surfaces of the room have',
     &        'not yet been defined. Exit without defining these?',
     &        OK,dok,nbhelp)
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

          IS=nint(CENLOC(IN,1))
          CALL EASKSUR(IZ,IS,'A','Select surface for base of room.',
     &      '(should be roughly horizontal)',IER)
          if(IS.ne.0)then
            CENLOC(IN,1)=real(IS)
          endif

        elseif (INO.eq.2) then

          IS=nint(CENLOC(IN,2))
          CALL EASKSUR(IZ,IS,'A','Select surface for top of room.',
     &      '(should be roughly horizontal)',IER)
          if(IS.ne.0)then
            CENLOC(IN,2)=real(IS)
          endif

        elseif (INO.eq.4) then

          V1=CENLOC(IN,3)
          CALL EASKR(V1,' ','Hc for upwards heat flow at floors?',
     &       0.1,'F',99.,'F',5.0,'floor hc upwards',IER,nbhelp)
          CENLOC(IN,3)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.5) then

          V1=CENLOC(IN,4)
          CALL EASKR(V1,' ','Hc for downwards heat flow at floors?',
     &       0.1,'F',99.,'F',0.7,'floor hc downwards',IER,nbhelp)
          CENLOC(IN,4)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.6) then

          V1=CENLOC(IN,5)
          CALL EASKR(V1,' ','Hc at outside (exterior) floors?',
     &       0.1,'F',99.,'F',8.0,'floor hc exterior',IER,nbhelp)
          CENLOC(IN,5)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.8) then

          V1=CENLOC(IN,6)
          CALL EASKR(V1,' ','Hc for upwards heat flow at ceilings?',
     &       0.1,'F',99.,'F',5.0,'ceil hc upwards',IER,nbhelp)
          CENLOC(IN,6)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.9) then

          V1=CENLOC(IN,7)
          CALL EASKR(V1,' ','Hc for downwards heat flow at ceilings?',
     &       0.1,'F',99.,'F',0.7,'ceil hc downwards',IER,nbhelp)
          CENLOC(IN,7)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.10) then

          V1=CENLOC(IN,8)
          CALL EASKR(V1,' ','Hc at outside (exterior) ceilings?',
     &       0.1,'F',99.,'F',8.0,'ceil hc exterior',IER,nbhelp)
          CENLOC(IN,8)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.12) then

          V1=CENLOC(IN,9)
          CALL EASKR(V1,' ','Hc for upwards heat flow at sloped?',
     &       0.1,'F',99.,'F',5.0,'sloped hc upwards',IER,nbhelp)
          CENLOC(IN,9)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.13) then

          V1=CENLOC(IN,10)
          CALL EASKR(V1,' ','Hc for downwards heat flow at sloped?',
     &       0.1,'F',99.,'F',0.7,'sloped hc downwards',IER,nbhelp)
          CENLOC(IN,10)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.14) then

          V1=CENLOC(IN,11)
          CALL EASKR(V1,' ','Hc at outside (exterior) sloped?',
     &       0.1,'F',99.,'F',8.0,'sloped hc exterior',IER,nbhelp)
          CENLOC(IN,11)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.16) then

          V1=CENLOC(IN,12)
          CALL EASKR(V1,' ','Hc at inside face of walls?',
     &       0.1,'F',99.,'F',2.5,'walls hc inside',IER,nbhelp)
          CENLOC(IN,12)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.17) then

          V1=CENLOC(IN,13)
          CALL EASKR(V1,' ','Hc at other (partition) face of walls?',
     &       0.1,'F',99.,'F',2.5,'walls hc partition',IER,nbhelp)
          CENLOC(IN,13)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.18) then

          V1=CENLOC(IN,14)
          CALL EASKR(V1,' ','Hc at outside (exterior) walls?',
     &       0.1,'F',99.,'F',8.0,'walls hc exterior',IER,nbhelp)
          CENLOC(IN,14)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'

        elseif (INO.eq.20) then

C Reset hc to default values.
          CENLOC(in,3)=5.0; CENLOC(in,4)=0.7; CENLOC(in,5)=8.0
          CENLOC(in,6)=5.0; CENLOC(in,7)=0.7; CENLOC(in,8)=8.0
          CENLOC(in,9)=5.0; CENLOC(in,10)=0.7; CENLOC(in,11)=8.0
          CENLOC(in,12)=2.5; CENLOC(in,13)=2.5; CENLOC(in,14)=8.0
          hcfpdescr(IN) = 'CEN default hc coefficients'
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

      COMMON/OUTIN/IUOUT,IUIN

C     integer IHCFP  ! number of hc control periods
C     real ST,EN     ! start and stop time of each period
C     real HCI       ! interior face hc coefficient (period,surface)
C     real HCE       ! other face hc coefficient (period,surface)
C     integer ICTL   ! hc control (period,surface) 1 is fixed coefficient 
C                    ! 2 is fixed correlation 3 is adaptive correlation 
C                    ! 4 is CEN standards
C     integer IHCI   ! which inside correlation (period,surface)
C                    ! -1 use standard correlation for surface location
C     integer IHCE   ! which other side correlation (period,surface)
C                    ! -1 use standard correlation for surface location
C     real CVdata    ! correlation attributes (period,surface, jj attribute)
C       jj=1:   control law for interior face
C       jj=2-4: data items associated with control law for interior
C       jj=5:   control law for other face
C       jj=6-8: data items associated with control law for other face
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)

C     real CENLOC (per period,jj attributes)
C       jj=1 index of a floor surface
C       jj=2 index of a ceiling surface
C       jj=3-5 floor upwd_inside, downwd_inside, external
C       jj=6-8 ceiling upwd_inside, downwd_inside, external
C       jj=9-11 sloped upwd_inside, downwd_inside, external
C       jj=12-14 wall inside, partition, external
      COMMON/HCCEN/CENLOC(MP,14)
      COMMON/Vld20/Vldtng
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT
      common/HCFPHI/hcfpdescr(MP)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      dimension HCIA(MS),HCEA(MS),IHCIA(MS),IHCEA(MS)
      character*12 sn(MS)
      character hcfpdescr*72,dstmp*24,t117*117,FILE*72
      CHARACTER louts*248
      logical Vldtng

      if(Vldtng)then
        CALL EFOPSEQ(IUF,FILE,3,IER)
      else
        CALL EFOPSEQ(IUF,FILE,4,IER)
      endif

      call dstamp(dstmp)
      write(iuf,'(2a)',IOSTAT=ISTAT,ERR=990) '*HC_CTL for zone ',
     &  zname(icomp)(1:lnzname(icomp))
      write(iuf,'(2a)',IOSTAT=ISTAT,ERR=990) '*Date,',dstmp
      WRITE(IUF,'(I3,a)',IOSTAT=ISTAT,ERR=990)
     &  IHCFP,' # number control intervals'
      WRITE(IUF,'(I3,a)',IOSTAT=ISTAT,ERR=990)
     &  NZSUR(ICOMP),' # number of surfaces'

C Also list the names of the surfaces of the zone use more than
C one line if necessary.
      do 42 ij=1,nzsur(icomp)
        icc=izstocn(icomp,ij) 
        sn(ij)=ssname(icc)
  42  continue
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        len117=1
        nsur=nzsur(icomp)
        call aslist(ipos,nsur,sn,MS,'S',t117,len117,itrunc)
        WRITE(IUF,'(3a)',IOSTAT=ISTAT,ERR=990) '# (',
     &    t117(1:len117),')'
        ipos=itrunc+1
      end do

      DO 930 I = 1,IHCFP
        WRITE(IUF,'(F6.2,3X,F6.2,a)',IOSTAT=ISTAT,ERR=990)
     &    ST(I),EN(I),' # start and end time of interval'
        if(ICTL(I).eq.1)then
          WRITE(IUF,'(I3,a)',IOSTAT=ISTAT,ERR=990)
     &      ICTL(I),' # fixed coefficients calculation control type'
        elseif(ICTL(I).eq.2)then
          WRITE(IUF,'(I3,a)',IOSTAT=ISTAT,ERR=990)
     &      ICTL(I),' # fixed correlation calculation control type'
        elseif(ICTL(I).eq.3)then
          WRITE(IUF,'(I3,a)',IOSTAT=ISTAT,ERR=990)
     &      ICTL(I),' # adaptive calculation control type'
        elseif(ICTL(I).eq.4)then
          WRITE(IUF,'(I3,a)',IOSTAT=ISTAT,ERR=990)
     &      ICTL(I),' # CEN calculation control type'
        endif
        write(iuf,'(2a)',IOSTAT=ISTAT,ERR=990) '*Doc,',
     &    hcfpdescr(i)(1:lnblnk(hcfpdescr(i)))
        if(ICTL(I).eq.1) then

C Convection coefficients. Copy to local array and then write out
C on as many lines as necessary.
          do 38 ij = 1,NZSUR(ICOMP)
            HCIA(ij) = HCI(i,ij)
            HCEA(ij) = HCE(i,ij)
  38      continue
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

        elseif(ICTL(I).eq.2) then

C Correlation indices. Copy to local array and then write out
C on as many lines as necessary.
          do 39 ij = 1,NZSUR(ICOMP)
            IHCIA(ij) = IHCI(i,ij)
            IHCEA(ij) = IHCE(i,ij)
  39      continue
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

        elseif(ICTL(I).eq.3) then

C Control laws and associated data for each surface
          do 37 J=1,NZSUR(ICOMP)
           WRITE(IUF,'(8(f5.0))',IOSTAT=ISTAT,ERR=990)
     &            (CVdata(I,J,jj),jj=1,8)
   37     continue

C  Additional information for air channels
          WRITE(IUF,'(I3,F6.3,F6.3,A)',IOSTAT=ISTAT,ERR=990)
     &      IAORZ,CWIDTH,CHEIGHT,
     &  ' # Air origin zone (+ chan. width & height if Bar-Cohen cor.)'

        elseif(ICTL(I).eq.4) then

C CEN 13791 mode sets values for specific locations.
         WRITE(IUF,'(a,2F7.1,a)',IOSTAT=ISTAT,ERR=990)
     &     '*basetop ',CENLOC(I,1),CENLOC(I,2),
     &     ' #  index (within the zone) of a typcial base & top'
         WRITE(IUF,'(a,3F7.3,a)',IOSTAT=ISTAT,ERR=990)
     &     '*floor   ',CENLOC(I,3),CENLOC(I,4),CENLOC(I,5),
     &     ' #  CEN floor upwd_inside, downwd_inside, external'
         WRITE(IUF,'(a,3F7.3,a)',IOSTAT=ISTAT,ERR=990)
     &     '*ceiling ',CENLOC(I,6),CENLOC(I,7),CENLOC(I,8),
     &     ' #  CEN ceiling upwd_inside, downwd_inside, external'
         WRITE(IUF,'(a,3F7.3,a)',IOSTAT=ISTAT,ERR=990)
     &     '*slope   ',CENLOC(I,9),CENLOC(I,10),CENLOC(I,11),
     &     ' #  CEN sloped upwd_inside, downwd_inside, external'
         WRITE(IUF,'(a,3F7.3,a)',IOSTAT=ISTAT,ERR=990)
     &     '*wall    ',CENLOC(I,12),CENLOC(I,13),CENLOC(I,14),
     &     ' #  CEN wall inside, partition, external'
        endif
  930 CONTINUE
      CALL ERPFREE(IUF,ISTAT)
      RETURN

  990 call edisp(ITRU,' HC File write error.')
      CALL ERPFREE(IUF,ISTAT)
      RETURN
      END

C ******************** EHTCFF ********************
C EHTCFF reads the convection-calculation control file (usually named *.htc).

      SUBROUTINE EHTCFF(FILE,IUF,ITRU,IER)
#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT
      common/HCFPHI/hcfpdescr(MP)

C CENLOC CEN specifications for each period
C       jj=1 index of a floor surface
C       jj=2 index of a ceiling surface
C       jj=3-5 floor upwd_inside, downwd_inside, external
C       jj=6-8 ceiling upwd_inside, downwd_inside, external
C       jj=9-11 sloped upwd_inside, downwd_inside, external
C       jj=12-14 wall inside, partition, external
      COMMON/HCCEN/CENLOC(MP,14)

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
        else

C Rewind file check first line and if *HC_Ctl read 2nd line as datestamp.
          rewind(iuf,err=392)
          CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc header',IER)
          if(OUTSTR(1:7).eq.'*HC_CTL'.or.
     &       OUTSTR(1:7).eq.'*hc_ctl')then
            newer=.true.
            CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc date',IER)
          else

C Not a recognised file << what error code to return? >>
            goto 390
          endif
        endif
        call edisp(ITRU,' Opened existing hc control file.')

C Number of control time intervals.
        CALL STRIPC(IUF,OUTSTR,1,num1,1,'htc periods',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IHCFP,1,MP,'W','htc periods',IER)
        if(ier.ne.0) goto 390

C Number of surfaces.
        CALL STRIPC(IUF,OUTSTR,1,num1,1,'htc surfaces',IER)
        K=0
        CALL EGETWI(OUTSTR,K,NS,1,MS,'W','htc surfaces',IER)
        DO 330 I = 1,IHCFP

C Start and end times of interval.
          CALL STRIPC(IUF,OUTSTR,2,num1,1,'htc start - end',IER)
          K=0
          CALL EGETWR(OUTSTR,K,ST(I),0.,24.,'W','htc start',IER)
          CALL EGETWR(OUTSTR,K,EN(I),ST(I),24.,'W','htc end',IER)

C Control `type' for interval. Note: older files might not have this
C line. If there is only one item on the line then read it, otherwise
C if there are NS items on the line then it is an older file.
          CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc ctl type',IER)
          if(num1.eq.1)then
            K=0
            CALL EGETWI(OUTSTR,K,ICTL(I),1,4,'W','htc ctl type',IER)

C If newer format then scan for hcfpdescr.
            if(newer)then
              CALL STRIPC(IUF,OUTSTR,99,num1,1,'htc ctl doc',IER)
              K=0
              CALL EGETW(OUTSTR,K,WORD,'W','*doc',IFLAG)
              call egetrm(outstr,K,hcfpdescr(i),'W','hc doc',IER)
            else
              hcfpdescr(i)='Imported legacy file with no regime docs.'
            endif
          else
            ICTL(I)=1
            hcfpdescr(i)='Imported legacy file assuming ctl type 1.'
          endif

          if(ICTL(I).eq.1) then

C Convection coefficients - read one for each surface (inside face)
C and then read one for each surface (other side face). Note that
C the `use default value` indicator is -1 to allow that in range
C checking.
            CALL EGETWRA(IUF,HCIA,NS,-1.,1000.,'W',
     &        'fixed inside face hc',IER)
            CALL EGETWRA(IUF,HCEA,NS,-1.,1000.,'W',
     &        'fixed other face hc',IER)
            DO 114 KV=1,NS
              HCI(I,KV)=HCIA(KV)
              HCE(I,KV)=HCEA(KV)
  114       CONTINUE

          elseif(ICTL(I).eq.2) then

C Correlation indices.
            CALL EGETWIA(IUF,IHCIA,NS,-1,99,'W',
     &               'inside face hc cor',IER)
            CALL EGETWIA(IUF,IHCEA,NS,-1,99,'W',
     &               'other face hc cor',IER)
            DO 115 KV=1,NS
              IHCI(I,KV)=IHCIA(KV)
              IHCE(I,KV)=IHCEA(KV)
  115       CONTINUE

          elseif(ICTL(I).eq.3) then

C Control laws and associated data for each surface.
C << needs range check >>
            do 30 J=1,NS
              READ(IUF,*,IOSTAT=ISTAT,ERR=390)(CVdata(I,J,jj),jj=1,8)
   30       continue

C Read IAORZ,CWIDTH and CHEIGHT of ventilated channels. This line
C may not exist in all hc files.
            READ(IUF,*,IOSTAT=ISTAT,ERR=390,END=391)
     &        IAORZ,CWIDTH,CHEIGHT

          elseif(ICTL(I).eq.4) then

C CEN mode sets values for specific conditions.
            CALL STRIPC(IUF,OUTSTR,99,num,1,'htc cen base top',IER)
            if(OUTSTR(1:8).eq.'*basetop')then
              K=8
              CALL EGETWR(OUTSTR,K,V1,1.,100.,'W','htc cen base',IER)
              CALL EGETWR(OUTSTR,K,V2,1.,100.,'W','htc cen top',IER)
              CENLOC(i,1)=V1
              CENLOC(i,2)=V2
            endif
            CALL STRIPC(IUF,OUTSTR,99,num,1,'htc cen floor',IER)
            if(OUTSTR(1:6).eq.'*floor')then
              K=6
              CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen upward',IER)
              CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen down',IER)
              CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
              CENLOC(i,3)=V1
              CENLOC(i,4)=V2
              CENLOC(i,5)=V3
            endif
            CALL STRIPC(IUF,OUTSTR,99,num,1,'htc cen ceiling',IER)
            if(OUTSTR(1:8).eq.'*ceiling')then
              K=8
              CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen upward',IER)
              CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen down',IER)
              CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
              CENLOC(i,6)=V1
              CENLOC(i,7)=V2
              CENLOC(i,8)=V3
            endif
            CALL STRIPC(IUF,OUTSTR,99,num,1,'htc cen slope',IER)
            if(OUTSTR(1:6).eq.'*slope')then
              K=6
              CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen upward',IER)
              CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen down',IER)
              CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
              CENLOC(i,9)=V1
              CENLOC(i,10)=V2
              CENLOC(i,11)=V3
            endif
            CALL STRIPC(IUF,OUTSTR,99,num,1,'htc cen wall',IER)
            if(OUTSTR(1:5).eq.'*wall')then
              K=5
              CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen inside',IER)
              CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen ptn',IER)
              CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
              CENLOC(i,12)=V1
              CENLOC(i,13)=V2
              CENLOC(i,14)=V3
            endif
          endif
  330   CONTINUE
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
      subroutine listhtc(ITRU,ICOMP,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/HCCEN/CENLOC(MP,14)

      character outs*124
      character hcfpdescr*72
      logical indefault,outdefault   ! default treatment
      logical inuser,outuser         ! user defined

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
      WRITE(outs,'(A,I2)')' Number of control periods: ',IHCFP
      call edisp(itru,outs)
      call edisp(itru,' ')

C If nothing to report, return.
      if(ihcfp.eq.0) return
      if(NS.eq.0) return

C Recover and list out the available correlations.
      call gethccornames(hcshtdescr,hclngdescr)

      DO 850 I = 1,IHCFP
        WRITE(outs,5202)I,ST(I),EN(I)
 5202   FORMAT(' Period',I3,' start',F7.2,' finish',F7.2)
        call edisp(itru,outs)
        if(ICTL(I).eq.1)then
          write(outs,'(a)') ' User specified convection coefficients'
        elseif(ICTL(I).eq.2)then
          write(outs,'(a)') ' User specified convection regime'
        elseif(ICTL(I).eq.3)then
          write(outs,'(a)') ' Adaptive (mixed) convection regime'
        elseif(ICTL(I).eq.4)then
          write(outs,'(a)') ' CEN convection regime:'
        endif
        call edisp(itru,outs)
        write(outs,'(1x,a)') hcfpdescr(I)(1:lnblnk(hcfpdescr(i)))
        call edisp(itru,outs)
        if(ICTL(I).eq.1) then

C Convection coefficients.
          call edisp(itru,'   Surface orinetation  Inside     Outside')
          DO 860 J=1,NS
            icc=izstocn(icomp,j)
            call eclose(HCI(I,J),-1.0,0.01,indefault)
            call eclose(HCE(I,J),-1.0,0.01,outdefault)
            if(indefault.and.outdefault)then
              WRITE(outs,'(I2,5a)') J,' ',SSNAME(ICC),' (',
     &          SSVFC(ICC),')   default   default'
            elseif(indefault.and.(.NOT.outdefault))then
              WRITE(outs,'(I2,5a,F8.3)') J,' ',SSNAME(ICC),' (',
     &          SSVFC(ICC),')   default ',HCE(I,J)
            elseif(.NOT.indefault.and.outdefault)then
              WRITE(outs,'(I2,5a,F8.3,a)') J,' ',SSNAME(ICC),' (',
     &          SSVFC(ICC),')  ',HCI(I,J),'   default '
            else
              WRITE(outs,'(I2,5a,2(1X,F8.3))') J,' ',SSNAME(ICC),
     &          ' (',SSVFC(ICC),') ',HCI(I,J),HCE(I,J)
            endif
            call edisp(itru,outs)
  860     CONTINUE

        elseif(ICTL(I).eq.2) then

C Correlation indices.
          call edisp(itru,'   Surface orientation  Inside     Outside')
          DO 861 J=1,NS
            icc=izstocn(icomp,j)
            indefault=.false.; outdefault=.false.
            if(IHCI(I,J).eq.-1) indefault=.true.
            if(IHCE(I,J).eq.-1) outdefault=.true.
            if(indefault.and.outdefault)then
              WRITE(outs,'(I2,5a)') J,' ',SSNAME(ICC),' (',
     &          SSVFC(ICC),')  default   default'
              call edisp(itru,outs)
            elseif(indefault.and.(.NOT.outdefault))then
              WRITE(outs,'(I2,5a,I8)') J,' ',SSNAME(ICC),' (',
     &          SSVFC(ICC),')  default ',IHCE(I,J)
              call edisp(itru,outs)
              write(outs,'(2a)')'   other face ',hclngdescr(IHCE(I,J))
              call edisp(itru,outs)
            elseif(.NOT.indefault.and.outdefault)then
              WRITE(outs,'(I2,5a,I8,a)') J,' ',SSNAME(ICC),' (',
     &          SSVFC(ICC),') ',IHCI(I,J),'  default '
              call edisp(itru,outs)
              write(outs,'(2a)')'   inside face ',hclngdescr(IHCI(I,J))
              call edisp(itru,outs)
            else
              WRITE(outs,5205) J,SSNAME(icc),' (',SSVFC(icc),') ',
     &          IHCI(I,J),IHCE(I,J)
 5205         FORMAT(I2,1x,4a,2(2X,I8))
              call edisp(itru,outs)
              write(outs,'(2a)')'   inside face ',hclngdescr(IHCI(I,J))
              call edisp(itru,outs)
              write(outs,'(2a)')'   other face  ',hclngdescr(IHCE(I,J))
              call edisp(itru,outs)
            endif
  861     CONTINUE

        elseif(ICTL(I).eq.3) then

C Control laws and associated data.
          write(outs,'(2a)') ' Surface  Orientation ',
     &      '----- Inside -----     ----- Outside -----'
          call edisp(itru,outs)
          DO 862 J=1,NS
            icc=izstocn(icomp,j) 
            WRITE(outs,5206)J,SSNAME(ICC),' (',SSVFC(ICC),') ',
     &        (CVdata(I,J,jj),jj=1,8)
 5206       FORMAT(I2,1x,4a,4(f4.0,1x),3x,4(f4.0,1x))
            call edisp(itru,outs)
  862     CONTINUE

        elseif(ICTL(I).eq.4) then

          ibase=nint(CENLOC(I,1))
          itop=nint(CENLOC(I,2))
          if(ibase.ne.0.and.itop.ne.0)then
            icc1=izstocn(icomp,ibase) 
            icc2=izstocn(icomp,itop)
            lssn1=lnblnk(SSNAME(icc1)) 
            lssn2=lnblnk(SSNAME(icc2)) 
            write(outs,'(5a)') ' CEN regime based on typical floor ',
     &        SSNAME(icc1)(1:lssn1),' and typcial ceiling ',
     &        SSNAME(icc2)(1:lssn2),'.'
          else
            write(outs,'(2a)') ' CEN regime with undefined floor ',
     &        ' and/or ceiling surfaces.'
          endif
          call edisp(itru,outs)
          write(outs,'(a,F7.3,a,F7.3,a,F7.3,a)') 
     &      'Floor upwards flow hc is ',CENLOC(i,3),
     &      ' and downwards ',CENLOC(i,4),
     &      ' and external ',CENLOC(i,5),'.'
          call edisp(itru,outs)
          write(outs,'(a,F7.3,a,F7.3,a,F7.3,a)') 
     &      'Ceiling upwards flow hc is ',CENLOC(i,6),
     &      ' and downwards ',CENLOC(i,7),
     &      ' and external ',CENLOC(i,8),'.'
          call edisp(itru,outs)
          write(outs,'(a,F7.3,a,F7.3,a,F7.3,a)') 
     &      'Sloped upwards flow hc is ',CENLOC(i,9),
     &      ' and downwards ',CENLOC(i,10),
     &      ' and external ',CENLOC(i,11),'.'
          call edisp(itru,outs)
          write(outs,'(a,F7.3,a,F7.3,a,F7.3,a)') 
     &      'Walls inside hc is ',CENLOC(i,12),
     &      ' and other side of partitions ',CENLOC(i,13),
     &      ' and external ',CENLOC(i,14),'.'
          call edisp(itru,outs)
        endif
  850 CONTINUE

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

C Set all to not applicable and then instanciate available options.
      do 42 ij=1,99
        hcshtdescr(ij)='not applicable'
        hclngdescr(ij)='not applicable'
  42  continue
      
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
