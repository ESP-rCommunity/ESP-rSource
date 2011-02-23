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
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN
      common/user/browse

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)

      CHARACTER*72 LTMP
      character HFILE*72,H*72,head*33
      CHARACTER ITEM(30)*33
      character OUTSTR*124,outs*124
      logical OK,dok,MODIFY,MODLEN,MODBND,modcfg,XST,clkok,browse
      integer NITEMS,INO ! max items and current menu item

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
        h(1)='A zone heat transfer coefficient file holds directives'
        h(2)='instructing esp-r to either use a specific set of heat'
        h(3)='transfer coefficients (e.g. related to ceiling diffusers'
        h(4)='or user supplied data for surfaces in a zone. '
        h(5)=' '
        h(6)='If you do not want to ignore the information in this '
        h(7)='file click on dereference. '
        clkok=.false.
  245   CALL EASKSCMD(LTMP,'Zone conv regime file ?',
     &    ' ','dereference',clkok,72,HFILE,'conv regime file',IER,7)
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
                h(1)='The file was not of the expected (heat transfer'
                h(2)='regime) and you should double check before you'
                h(3)='agree to overwrite it with hc regime data. '
                h(4)=' '
                h(5)='If you say no you will be returned to the zone '
                h(6)='composition menu. '
                CALL ASKOK(outs,
     &            'Overwrite file with current data (see help)?',
     &             OK,dok,3)
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
            h(1)='Answering yes will create a new file to hold the'
            h(2)='conv regime data. '
            h(3)=' '
            h(4)='A no answer will return without changing the model.'
            CALL ASKOK(
     &      'Could not find a readable conv regime file for',
     &      'this zone. Do you want to create one?',OK,dok,4)
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
        h(1)='A zone heat transfer coefficient file holds directives'
        h(2)='instructing esp-r to either use a specific set of heat'
        h(3)='transfer coefficients (e.g. related to ceiling diffusers'
        h(4)='or user supplied data for surfaces in a zone. '
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
      MODIFY=.TRUE.
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
      ITEM(28)=            '! save                        '
      ITEM(29)=            '? help                        '
      ITEM(30)=            '- exit                        '
      NITEMS=30

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
      h(1)='The choice of algorithm for calculating internal surface'
      h(2)='convection coefficients (int-hc) can significantly affect'
      h(3)='simulation results and can even impact design decisions.'
      h(4)='It is important to recognize that by default int-hc'
      h(5)='values are calculated using a buoyancy-flow relation. The'
      h(6)='facility implemented here allows you to drop this'
      h(7)='assumption and to calculate int-hc values in a more'
      h(8)='rigorous fashion.'
      h(9)=' '
      h(10)='The adaptive convection algorithm (ACA) implemented'
      h(11)='under options `a` through `n` controls the calculation'
      h(12)='of int-hc in a dynamic fashion. Based upon minimal data'
      h(13)='input, the ACA appraises a configuration to assign an'
      h(14)='appropriate set of int-hc correlation equations to each'
      h(15)='internal surface. Then as the simulation evolves, the'
      h(16)='ACA toggles between the int-hc equations in response to'
      h(17)='the prevailing convective regime.'
      h(18)=' '
      h(19)='Choose the option that best describes the principle'
      h(20)='convection regime within the zone. You will be'
      h(21)='prompted for supplementary data in subsequent menus.'
      h(22)=' '
      h(23)='Simulation time-steps of 15 minutes or less are'
      h(24)='recommended when this facility is used.'
      h(25)=' '
      h(26)='Options a-c are appropriate when convection in the zone'
      h(27)='is driven primarily by buoyancy resulting from surface-'
      h(28)='to-air temperature differences caused by in-floor'
      h(29)='heating, a heated wall panel, sun-patch or cold window.'
      h(30)=' '
      h(31)='Options d-e are appropriate when convection is driven'
      h(32)='primarily by buoyancy resulting from a heating device'
      h(33)='(e.g. radiator, stove) located either undeer a window'
      h(34)='or at some other location in the zone.'
      h(35)=' '
      h(36)='Options f-g are specific for treating convection inside'
      h(37)='open vertical channels with bouyancy driven flow.'
      h(38)='The correlations apply to the vertical surfaces'
      h(39)='and operate all the time'
      h(40)=' '
      h(41)='Options h-j are appropriate when convection is driven'
      h(42)='primarily by an air-handling system that supplies'
      h(43)='heated or cooled air to the zone through ceiling'
      h(44)='floor or wall-mounted diffusers. It should not be used'
      h(45)='when buoyancy is also a significant factor.'
      h(46)=' '
      h(46)='Option k is appropriate when convection in the zone'
      h(47)='is driven primarily by a heating or cooling device that'
      h(48)='circulates air within the zone. There is no intentional'
      h(49)='supply or extract of air from the zone.'
      h(50)=' '
      h(51)='Options l-m are appropriate when convection in the zone'
      h(52)='is driven by both mechanical and buoyant forces. The'
      h(53)='mechanical forces are caused by an air handling system'
      h(54)='(AHU) that supplies heated or cooled air to the zone'
      h(55)='through ceiling, floor, or wall-mounted diffusers. The'
      h(56)='buoyant forces are caused by surface-to-air temperature'
      h(57)='differences. This option is preferred over the'
      h(58)='`air-based HVAC system` convective regime as it captures'
      h(59)='the combined influence of buoyant and forced effects.' 

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
      ELSEIF (INO.EQ.28)then

C Write data to file.
        CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)

      ELSEIF (INO.EQ.29)then
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
        CALL PHELPD('convective calculations',59,'-',0,0,IER)
      ELSEIF (INO.EQ.30)then
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

      common/pophelp/h(60)
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C24/IZSTOCN(MCOM,MS)

      CHARACTER SN*12,outs*124,h*72,hcfpdescr*72
      logical OK,dok

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
      h(1)='There are lots of options. You probably want to '
      h(2)='review the synopsis before selecting anything! '
      call askok('View a synopsis of Bouyancy-driven flow',
     &           'options and background information?',ok,dok,2)
      if(OK)then

C Add lots of help text here.
        h(1)='This choice is appropriate when convection in the zone'
        h(2)='is driven primarily by buoyancy resulting from'
        h(3)='surface-to-air temperature differences.'
        h(4)=' '
        if(iopt.eq.1)then
         h(5)='Use option `in-floor heating` when you have defined a'
         h(6)='zone with floor surface(s) heated by flux injection'
         h(7)='into the floor construction or by flux injection into'
         h(8)='a plenum under the floor surface. It is assumed that'
         h(9)='such a zone control is or will be defined.'
         h(10)=' '
         h(11)='You will be prompted for the surface(s) which are '
         h(12)='heated. '
         h(13)=' '
         h(14)='ESP-r models convection as follows: '
         h(15)='  walls:   Khalifa eq 3 when heat on; Alamdari-Hammond'
         h(16)='           vertical eq (or sim toggle) when heat off.'
         h(17)='  windows: Khalifa eq 3 when heat on; Alamdari-Hammond'
         h(18)='           vertical eq (or sim toggle) when heat off.'
         h(19)='  floor:   Awbi-Hatton heated floor eq when heat on;'
         h(20)='           Alamdari-Hammond stably stratified/buoyant'
         h(21)='           horizontal eq (or sim toggle) when heat off.'
         h(22)='  ceiling: Khalifa eq 4 when heat on; Alamdari-Hammond'
         h(23)='           stably stratified or buoyant horizontal eq'
         h(24)='           (or sim toggle) when heat off.'
         h(25)=' '
         h(26)='Refer to Section 5.2 and in particular Table 5.1 of'
         h(27)='Beausoleil-Morrison (2000) for further details and'
         h(28)='references to the int-hc correlations.'
         CALL PHELPD('Cause of dT',28,'-',0,0,IER)
        elseif(iopt.eq.2)then
         h(5)='Use option `heated wall panel` when you have defined a'
         h(6)='zone with surface(s) heated by flux injection into the'
         h(7)='panel construction or by flux injection into a zone'
         h(8)='which represents a panel heater. It is assumed that'
         h(9)='such a zone control is or will be defined.'
         h(10)=' '
         h(11)='You will be prompted for the surface(s) which are '
         h(12)='heated. '
         h(13)=' '
         h(14)='ESP-r models convection as follows: '
         h(15)='  heated walls: Awbi-Hatton heated wall eq when heat'
         h(16)='                on; Alamdari-Hammond vert eq (or sim'
         h(17)='                toggle) when heat off.'
         h(18)='  non-heated:   Khalifa eq 6 when heat on; Alamdari-'
         h(19)='  walls:        Hammond vert eq (or toggle) when off.'
         h(20)='  windows: Khalifa eq 6 when heat on; Alamdari-Hammond'
         h(21)='           vertical eq (or sim toggle) when heat off.'
         h(22)='  floor:   Alamdari-Hammond stably stratified eq when'
         h(23)='           heat on; Alamdari-Hammond stably stratified'
         h(24)='           or buoyant horizontal eq (or sim toggle)'
         h(25)='           when heat is off.'
         h(26)='  ceiling: Khalifa eq 7 when heat on; Alamdari-Hammond'
         h(27)='           stably stratified or buoyant horizontal eq'
         h(28)='           (or sim toggle) when heat off.'
         h(29)=' '
         h(30)='Refer to Section 5.2 and in particular Table 5.1 of'
         h(31)='Beausoleil-Morrison (2000) for further details and'
         h(32)='references to the int-hc correlations.'
         CALL PHELPD('Cause of dT',32,'-',0,0,IER)
        elseif(iopt.eq.3)then
         h(5)='The option `other temp diff` is the appropriate choice'
         h(6)='when the temperature diff is caused by a sun patch or'
         h(7)='by heat transfer through the fabric (e.g. a cold window '
         h(8)='surface). '
         h(9)=' '
         h(10)=' '
         h(11)='ESP-r models convection as follows: '
         h(12)=' walls:   Alamdari-Hammond vertical eq (or sim toggle)'
         h(13)='          when heat on or off.'
         h(14)=' windows: Alamdari-Hammond vertical eq (or sim toggle)'
         h(15)='          when heat on or off.'
         h(16)=' floor:   Alamdari-Hammond stably stratified or buoyant'
         h(17)='          horizontal eq (or sim toggle) when heat on'
         h(18)='          or off.'
         h(19)=' ceiling: Alamdari-Hammond stably stratified or buoyant'
         h(20)='          horizontal eq (or sim toggle) when heat on'
         h(21)='          or off.'
         h(22)=' '
         h(23)='Refer to Section 5.2 and in particular Table 5.1 of'
         h(24)='Beausoleil-Morrison (2000) for further details and'
         h(25)='references to the int-hc correlations.'
         CALL PHELPD('Cause of dT',25,'-',0,0,IER)
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
            h(1)='Treatment for wall depends on whether it is a'
            h(2)='heated surface.'
            WRITE(outs,'(3a)') ' Is surface ',SN(1:lnblnk(SN)),
     &                         ' heated?'
            CALL ASKOK(' ',outs,OK,dok,2)
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
      COMMON/C24/IZSTOCN(MCOM,MS)

      CHARACTER SN*12,outs*124,h*72,hcfpdescr*72
      logical OK,dok

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
      h(1)='There are lots of options. You probably want to '
      h(2)='review the synopsis before selecting anything! '
      call askok('View a synopsis of Bouyancy-driven flow',
     &           'options and background information?',ok,dok,2)
      if(OK)then

C Add lots of help text here.
        h(1)='This choice is appropriate when convection in the zone'
        h(2)='is driven primarily by buoyancy resulting from'
        h(3)='a heating device (e.g. radiator, stove) located within'
        h(4)='the zone.'
        h(5)=' '
        h(6)='Indicate whether the heating device is located beneath'
        h(7)='a window or not as this will influence the convective'
        h(8)='regime within the zone.'
        h(9)=' '
        h(10)='You will be prompted to indicate whether you want to'
        h(11)='apply Khalifa`s window correlations or alternate'
        h(12)='equations. As discussed in Section 5.2 of'
        h(13)='Beausoleil-Morrison (2000), it is suspected that'
        h(14)='Khalifa`s window correlations overpredict int-hc.'
        h(15)=' '
        h(16)='For option `a` the ACA models convection as follows:'
        h(17)='  walls:   Khalifa eq 6 when heat on; Alamdari-Hammond'
        h(18)='           vertical eq (or sim toggle) when heat off.'
        h(19)='  windows: Khalifa eq 6 or 9 (user choice) when heat on;'
        h(20)='           Alamdari-Hammond  vertical eq (or sim toggle)'
        h(21)='           when heat off.'
        h(22)='  floor:   Alamdari-Hammond stably stratified eq when'
        h(23)='           heat on; Alamdari-Hammond stably stratified'
        h(24)='           or buoyant horizontal eq (or sim toggle) when'
        h(25)='           heat off.'
        h(26)='  ceiling: Khalifa eq 7 when heat on; Alamdari-Hammond'
        h(27)='           stably stratified or buoyant horizontal eq'
        h(28)='           (or sim toggle) when heat off.'
        h(29)=' '
        h(30)='For option `b` the ACA models convection as follows:'
        h(31)='(you will be prompted to indicate which wall surfaces'
        h(32)='are adjacent to the heating device):'
        h(33)='  walls close to heater:'
        h(34)='           Khalifa eq 5 when heat on; Alamdari-Hammond'
        h(35)='           vertical eq (or sim toggle) when heat off.'
        h(36)='  walls not close to heater:'
        h(37)='           Khalifa eq 3 when heat on; Alamdari-Hammond'
        h(38)='           vertical eq (or sim toggle) when heat off.'
        h(39)='  windows: Khalifa eq 3 or 10 (user choice) when heat'
        h(40)='           on; Alamdari-Hammond  vertical eq (or sim'
        h(41)='           toggle) when heat off.'
        h(42)='  floor:   Alamdari-Hammond stably stratified eq when'
        h(43)='           heat on; Alamdari-Hammond stably stratified'
        h(44)='           or buoyant horizontal eq (or sim toggle) when'
        h(45)='           heat off.'
        h(46)='  ceiling: Khalifa eq 4 when heat on; Alamdari-Hammond'
        h(47)='           stably stratified or buoyant horizontal eq'
        h(48)='           (or sim toggle) when heat off.'
        h(49)=' '
        h(50)='Refer to Section 5.2 and in particular Table 5.1 of'
        h(51)='Beausoleil-Morrison (2000) for further details and'
        h(52)='references to the int-hc correlations.'
        CALL PHELPD('Location of heater',52,'-',0,0,IER)
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
              h(1)='Surface transparent. Use Khalifa window correlation'
              h(2)='(yes) or wall correlation (no)? '
              CALL ASKOK(outs,'(see help)',OK,dok,2)
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
              h(1)='Surface transparent. Use Khalifa window correlation'
              h(2)='(yes) or wall correlation (no)? '
              CALL ASKOK(outs,'(see help)',OK,dok,2)
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
              h(1)='Surface is a wall. If it is close to the heater'
              h(2)='say yes if not say no.'
              CALL ASKOK(outs,'(see help)',OK,dok,2)
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
      SUBROUTINE EconvC(ITRU,IUF,ICOMP,iopt,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER outs*124,h*72,hcfpdescr*72
      character*24 hcphrase(5)
      CHARACTER*25 ITEM(15)
      logical ok,dok
      real airflow   ! temprary value for editing
      integer NITEMS,Ioa,Idiff,Ifor ! max items and current menu item

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
      h(1)='There are lots of options. You probably want to '
      h(2)='review the synopsis before selecting anything! '
      call askok('View a synopsis of Mechanically-driven flow',
     &            'options and background information?',ok,dok,2)
      if(OK)then

C Add lots of help here.
        h(1)='This choice is appropriate when convection in the zone'
        h(2)='is driven primarily by an air-handling system that'
        h(3)='supplies heated or cooled air to the zone through ceiling'
        h(4)='floor or wall-mounted diffusers. It should not be used'
        h(5)='when buoyancy is also a significant factor. Buoyancy is'
        h(6)='often negligible for cooling situations, but usually'
        h(7)='important for heating situations. The `mixed flow`'
        h(8)='convective regime is generally recommended as an'
        h(9)='alternative to this choice as it can capture the combined'
        h(10)='influence of buoyant and forced effects.' 
        h(11)=' '
        h(12)='Indicate the air-handling system type. This determines'
        h(13)='the temperature and flowrate of air introduced to the'
        h(14)='zone and thus the convective regime.'
        h(15)=' '
        h(16)='NOTES: variable air volume system with constant volume'
        h(17)='heating. When the zone requires cooling, the supply'
        h(18)='air is chilled to a constant temp while the system'
        h(19)='adjusts the air flow to meet the cooling load. When the'
        h(20)='zone requires heat, the system supplies air at a fixed'
        h(21)='flow rate, equal to the min damper position of the VAV'
        h(22)='box. Reheat coils warm the air upstream of the diffuser'
        h(23)='to meet the zone`s heating load.'
        h(24)=' '
        h(25)='NOTE: variable air volume system with variable volume'
        h(26)='heating. It operates the same as VAV with CV in'
        h(27)='cooling mode. But when the zone requires heating, the'
        h(28)='dampers and reheat coils work in conjunction to satisfy'
        h(29)='the heating load. At low heat loads the system delivers'
        h(30)='air at the min flowrate and the reheat coil heats the'
        h(31)='air to meet the load. When the heating load is'
        h(32)='sufficiently high that the reheat coil cannot meet the'
        h(33)='load, the damper opens to increase the flowrate.'
        h(34)=' '
        h(35)='NOTE: constant-volume variable-temperature system'
        h(36)='delivers a constant flow of air, varying the temperature'
        h(37)='of the supply air in response to the zone load.'
        h(38)=' '
        h(39)=' '
        h(40)='It is important to note that the description given here'
        h(41)='is used to simulate the convective regime induced in the'
        h(42)='zone by the HVAC air handling system. It does not model'
        h(43)='the performance and energy draws of the system`s'
        h(44)='components. Therefore, you should ensure that the data'
        h(45)='given here is consistent with the zone control data or'
        h(46)='the explicit plant data that describes the HVAC system.'
        CALL PHELPD('HVAC system type',46,'-',0,0,IER)
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

        h(1)='Enter the air flow rate supplied by the system to the'
        h(2)='zone. This is expressed in terms of zone air changes per'
        h(3)='hour.'
        WRITE(outs,'(a)')' air flow rate in ac/h'
        airflow=CVdata(IPER,1,2)  ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,3)
        do 51 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,2) = airflow
   51   continue

        h(1)='Enter the minimum air temperature that can be supplied'
        h(2)='by the system to the zone. This is the design temperature'
        h(3)='for cooling and is measured at the diffuser.'
        WRITE(outs,'(a)')' minimum supply air temp'
        Tmin=CVdata(IPER,1,4)  ! give it a typical value
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'min T',IER,3)
        do 52 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,4) = Tmin
   52   continue

        h(1)='Enter the maximum air temperature that can be supplied'
        h(2)='by the system to the zone. This is the design temperature'
        h(3)='for heating and is measured at the diffuser.'
        WRITE(outs,'(a)')' maximum supply air temp'
        Tmax=CVdata(IPER,1,3)  ! give it a typical value
        CALL EASKR(Tmax,outs,' ',
     &             5.0,'W',80.,'W',43.0,'max T',IER,3)
        do 53 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,3) = Tmax
          CVdata(IPER,ISUR,6) = 99.
   53   continue
      ELSE

C Variable-volume system. Get cooling setpoint temperature for supplied
C air, the maximum allowable diffuser temperature, and the minimum and
C and maximum air flow rates.
        hcphrase(1) = 'convec. via VAV VT'
        h(1)='Enter the temperature at which air is supplied to the'
        h(2)='zone when the system is cooling.'
        WRITE(outs,'(a)')' diffuser setpoint temp for cooling'
        Tmin=CVdata(IPER,1,2)  ! give it a typical value
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'setpoint T',IER,2)
        do 54 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,2) = Tmin
   54   continue

        h(1)='Enter the minimum air flow rate that the system can'
        h(2)='supply to the zone. This is flow rate that occurs when'
        h(3)='the dampers in the VAV box are at their minimum'
        h(4)='position. This is expressed in terms of zone air'
        h(5)='changes per hour.'
        WRITE(outs,'(a)')' min air flow rate in ac/h'
        airflow=CVdata(IPER,1,3) ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,5)
        do 55 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,3) = airflow
   55   continue

        h(1)='Enter the max air flow rate that the system can supply'
        h(2)='to the zone. This is design flow rate and occurs when the'
        h(3)='dampers in the VAV box are at their max position. This'
        h(4)='is expressed in terms of zone air changes per hour.'
        WRITE(outs,'(a)')' max air flow rate in ac/h'
        airflow=CVdata(IPER,1,4) ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',9.0,'ac/h',IER,4)
        do 56 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,4) = airflow
   56   continue

        h(1)='Enter the maximum air temperature that can be supplied'
        h(2)='by the system to the zone. This is the design temperature'
        h(3)='for heating and is measured at the diffuser. Note that'
        h(4)='if the zone`s heating load cannot be satisfied by this'
        h(5)='supply air temp, it is assumed that any remaining heat is'
        h(6)='supplied from another source (e.g. baseboard heaters)'
        h(7)='which does not affect the flow regime.'
        WRITE(outs,'(a)')' max diffuser temperature'
        Tmax=CVdata(IPER,1,6) ! give it a typical value
        CALL EASKR(Tmax,outs,' ',
     &             15.0,'W',80.,'W',43.0,'max supply T',IER,7)
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
      h(1)='Typically the air handling unit (AHU) conditions outdoor'
      h(2)='air. In this case, the conditioning of the outdoor air'
      h(3)='is performed by the AHU`s heating or cooling coil.'
      h(4)=' '
      h(5)='You can choose to represent the outdoor air in your model'
      h(6)='as infiltration to the zone. This would give the correct'
      h(7)='total heating or cooling load for the zone. However, in'
      h(8)='this case it is necessary to subtract the outdoor air'
      h(9)='load from the zone`s total load. In this way, the correct'
      h(10)='temperature and flow rate at the diffuser can be'
      h(11)='calculated. This is handled automatically when you'
      h(12)='select option `b`.'
      h(13)=' '
      h(14)='Alternatively, you can represent the AHU as another'
      h(15)='zone. You would have infiltration of outdoor air to the'
      h(16)='AHU zone then ventilation from the AHU zone to the'
      h(17)='occupied zone. The energy inj/extracted to condition'
      h(18)='this ventilation to the zone air-point temp must be'
      h(19)='subtracted from the zone`s total load to correctly'
      h(20)='calculate the temperature and flow rate at the diffuser.'
      h(21)='This is handled automatically with option `c`.'
      h(22)=' '
      h(23)='Use option `a` if you have no outdoor air in your model.'

      CALL EMENU(' Outdoor air treated as:',ITEM,NITEMS,Ioa)
      if(Ioa.eq.1)then
        hcphrase(2) = 'no outdoor air'
      elseif(Ioa.eq.2)then
        hcphrase(2) = 'infiltration'
      elseif(Ioa.eq.3)then
        hcphrase(2) = 'vent from zone'
      elseif(Ioa.eq.4)then
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
      h(1)='The location of the supply diffusers affects the'
      h(2)='convective regime in the zone. Select the option'
      h(3)='that most closely describes the location of the'
      h(4)='diffusers in the zone.'
      h(5)=' '
      h(6)='If the diffusers are located within the ceiling, you'
      h(7)='will be given the choice of using the Fisher'
      h(8)='ceiling-jet or the Fisher & Pedersen correlations.'
      h(9)='The Fisher horizontal-jet correlations will be used'
      h(10)='if the diffusers are located in the wall.'
      h(11)=' '
      h(12)='The Fisher or Fisher & Pedersen correlations will be'
      h(13)='used when the AHU is on, while the Alamdari-Hammond'
      h(14)='correlations will be used when it is off.'
      h(15)=' '
      h(16)='Refer to Section 5.2 and in particular Table 5.1 of'
      h(17)='Beausoleil-Morrison (2000) for further details and'
      h(18)='references to the int-hc correlations.'

      CALL EMENU(' Air supplied through',ITEM,NITEMS,Idiff)

      IF(Idiff.EQ.1)THEN
        hcphrase(3) = 'ceil diffuser'

C Air supplied through radial ceiling diffusers.
C When heating is on ==> User has choice of Fisher `thesis' or
C Fisher & Pedersen `ASHRAE' correlations for walls, floor, and ceiling.
C When heating is off ==> Accept simulation toggle (Alamdari-Hammond if defaulted).
C Determine whether to use Fisher `thesis' or Fisher & Pedersen `ASHRAE'
C correlations. The choice applies to all surfaces in the room.
   22   Ifor = -2
        ITEM(1) ='a Fisher & Pedersen '
        ITEM(2) ='b Fisher thesis     '
        ITEM(3)='? Help         '
        NITEMS=3
        h(1)='Two alternate correlations are available to calculate'
        h(2)='int-hc values for AHU`s that supply air through ceiling'
        h(3)='diffusers. Both were generated from the same'
        h(4)='experimental data set but use different functional'
        h(5)='forms. Refer to Appendix C of Beausoleil-Morrison'
        h(6)='(2000) for a discussion of this topic.'
        CALL EMENU(' Which correlations?',ITEM,NITEMS,Ifor)
        if(Ifor.eq.3)then

C Add lots of help here.
          CALL PHELPD('Which correlations',6,'-',0,0,IER)
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

      common/pophelp/h(60)
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C24/IZSTOCN(MCOM,MS)

      CHARACTER SN*12,outs*124,h*72,hcfpdescr*72
      logical OK,dok

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
      h(1)='There are lots of options. You probably want to '
      h(2)='review the synopsis before selecting anything! '
      call askok(
     &  'View a synopsis of mechanically driven flow caused by a ',
     &  'circulating fan within the room options?',
     &  ok,dok,2)
      if(OK)then

C Display help info to inform user what this should be used for.
        h(1)='This choice is appropriate when convection in the zone'
        h(2)='is driven primarily by a heating or cooling device that'
        h(3)='circulates air within the zone. There is no intentional'
        h(4)='supply or extract of air from the zone.'
        h(5)=' '
        h(6)='You will be prompted to indicate which surfaces are'
        h(7)='struck by the fan as this augments heat transfer.'
        h(8)=' '
        h(9)='The ACA models convection as follows:'
        h(10)='  walls, fan blowing on:'
        h(11)='           Khalifa eq 8 when heat on; Alamdari-Hammond'
        h(12)='           vertical eq (or sim toggle) when heat off.'
        h(13)='  walls, fan not blowing on:'
        h(14)='           Khalifa eq 3 when heat on; Alamdari-Hammond'
        h(15)='           vertical eq (or sim toggle) when heat off.'
        h(16)='  windows, fan blowing on:'
        h(17)='           Khalifa eq 8 when heat on; Alamdari-Hammond'
        h(18)='           vertical eq (or sim toggle) when heat off.'
        h(19)='  windows, fan not blowing on:'
        h(20)='           Khalifa eq 3 when heat on; Alamdari-Hammond'
        h(21)='           vertical eq (or sim toggle) when heat off.'
        h(22)='  floor:   Alamdari-Hammond stably stratified or buoyant'
        h(23)='           horizontal eq (or sim toggle) when heat on'
        h(24)='           or off.'
        h(25)='  ceiling: Khalifa eq 4 when heat on; Alamdari-Hammond'
        h(26)='           stably stratified or buoyant horizontal eq'
        h(27)='           (or sim toggle) when heat off.'
        CALL PHELPD('Outdoor air treated as',27,'-',0,0,IER)
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
          h(1)='Surface is oriented so it might get air from the '
          h(2)='fan heater. If this is the case say yes. '
          CALL ASKOK(outs,'(see help)',OK,dok,2)
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

      common/pophelp/h(60)
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER outs*124,h*72,hcfpdescr*72
      character*24 hcphrase(5)
      CHARACTER*25 ITEM(15)
      logical ok,dok
      integer NITEMS,Ioa ! max items and current menu item

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
      h(1)='There are lots of options. You probably want to '
      h(2)='review the synopsis before selecting anything! '
      call askok(
     &  'View synopsis of mixed Bouyancy-driven and mechanical',
     &  'flow options and background information?',ok,dok,2)
      if(OK)then

C Add lots of help here.
        h(1)='This choice is appropriate when convection in the zone'
        h(2)='is driven by both mechanical and buoyant forces. The'
        h(3)='mechanical forces are caused by an air handling system'
        h(4)='(AHU) that supplies heated or cooled air to the zone'
        h(5)='through ceiling, floor, or wall-mounted diffusers. The'
        h(6)='buoyant forces are caused by surface-to-air temperature'
        h(7)='differences. This option is preferred over the'
        h(8)='`air-based HVAC system` convective regime as it captures'
        h(9)='the combined influence of buoyant and forced effects.'
        h(10)=' '
        h(11)='Indicate the air-handling system type. This determines'
        h(12)='the temperature and flow rate of air introduced to the'
        h(13)='zone and thus the convective regime.'
        h(14)=' '
        if(iopt.eq.1)then
         h(15)='Option `VAV with CV heating` is a variable air volume'
         h(16)='system with constant vol heating.'
         h(17)='When the zone requires cooling, the supply air'
         h(18)='is chilled to a constant temp while the system'
         h(19)='adjusts the air flow to meet the cooling load. When the'
         h(20)='zone requires heat, the system supplies air at a fixed'
         h(21)='flow rate, equal to the min damper position of the VAV'
         h(22)='box. Reheat coils warm the air upstream of the diffuser'
         h(23)='to meet the zone`s heating load.'
         h(24)=' '
        elseif(iopt.eq.2)then
         h(15)='Option `VAV with VV heating` is a variable air volume'
         h(16)='system with variable volume heating. It operates the'
         h(17)='same as `VAV with CV heating` in cooling model. In '
         h(18)='heating mode, the dampers and reheat coils work in'
         h(19)='in conjunction to satisfy the heating load.'
         h(20)='At low heat loads the system delivers air at the low'
         h(21)='flowrate and the reheat coil heats the air to meet the'
         h(22)='load. If the reheat coil cannot meet the load,'
         h(23)='the damper opens to increase the flowrate.'
         h(24)=' '
        elseif(iopt.eq.3)then
         h(15)='Option `n` is a constant-volume variable-temperature'
         h(16)='system. It delivers a constant flow of air, varying the'
         h(17)='temperature of the supply air in response to the zone'
         h(18)='load.'
         h(19)=' '
         h(20)='<< more comments to be added... >> '
         h(21)=' '
         h(22)=' '
         h(23)=' '
        endif
        h(24)='It is important to note that the description given here'
        h(25)='is used to simulate the convective regime induced in the'
        h(26)='zone by the HVAC air handling system. It does not model'
        h(27)='the performance and energy draws of the system`s'
        h(28)='components. Therefore, you should ensure that the data'
        h(29)='given here is consistent with the zone control data or'
        h(30)='the explicit plant data that describes the HVAC system.'
        h(31)=' '
        h(32)='Regardless of HVAC system type, the Beausoleil-Morrison'
        h(33)='mixed flow correlations will be used when the AHU is on,'
        h(34)='while the Alamdari-Hammond correlations will be used'
        h(35)='when it is off.'
        h(36)=' '
        h(37)='Refer to Section 5.2 and in particular Table 5.1 of'
        h(38)='Beausoleil-Morrison (2000) for further details and'
        h(39)='references to the int-hc correlations.'
        CALL PHELPD('Mixed convection',39,'-',0,0,IER)
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
        h(1)='Enter the air flow rate supplied by the system to the'
        h(2)='zone. This is expressed in terms of zone air changes per'
        h(3)='hour.'
        WRITE(outs,'(a)')' air flow rate in ac/h'
        airflow=CVdata(IPER,1,2)   ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,3)
        do 51 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,2) = airflow
   51   continue

        h(1)='Enter the minimum air temperature that can be supplied'
        h(2)='by the system to the zone. This is the design temperature'
        h(3)='for cooling and is measured at the diffuser.'
        WRITE(outs,'(a)')' minimum supply air temp'
        Tmin=CVdata(IPER,1,4)   ! give it a typical value 
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'min T',IER,3)
        do 52 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,4) = Tmin
   52   continue

        h(1)='Enter the maximum air temperature that can be supplied'
        h(2)='by the system to the zone. This is the design temperature'
        h(3)='for heating and is measured at the diffuser.'
        WRITE(outs,'(a)')' maximum supply air temp'
        Tmax=CVdata(IPER,1,3)   ! give it a typical value 
        CALL EASKR(Tmax,outs,' ',
     &             5.0,'W',80.,'W',43.0,'max T',IER,3)
        do 53 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,3) = Tmax
          CVdata(IPER,ISUR,6) = 99.
   53   continue
      ELSE

C Variable-volume system. Get cooling setpoint temperature for supplied
C air, the maximum allowable diffuser temperature, and the minimum and
C and maximum air flow rates.
        hcphrase(1) = 'mixed conv. via VAV VT'
        h(1)='Enter the temperature at which air is supplied to the'
        h(2)='zone when the system is cooling.'
        WRITE(outs,'(a)')' diffuser setpoint temp for cooling'
        Tmin=CVdata(IPER,1,2)   ! give it a typical value 
        CALL EASKR(Tmin,outs,' ',
     &             5.0,'W',30.,'W',13.0,'setpoint T',IER,2)
        do 54 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,2) = Tmin
   54   continue

        h(1)='Enter the minimum air flow rate that the system can'
        h(2)='supply to the zone. This is flow rate that occurs when'
        h(3)='the dampers in the VAV box are at their minimum'
        h(4)='position. This is expressed in terms of zone air'
        h(5)='changes per hour.'
        WRITE(outs,'(a)')' min air flow rate in ac/h'
        airflow=CVdata(IPER,1,3)  ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',3.0,'ac/h',IER,5)
        do 55 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,3) = airflow
   55   continue

        h(1)='Enter the max air flow rate that the system can supply'
        h(2)='to the zone. This is design flow rate and occurs when the'
        h(3)='dampers in the VAV box are at their max position. This'
        h(4)='is expressed in terms of zone air changes per hour.'
        WRITE(outs,'(a)')' max air flow rate in ac/h'
        airflow = CVdata(IPER,1,4)  ! give it a typical value
        CALL EASKR(airflow,outs,' ',
     &             3.0,'W',100.,'W',9.0,'ac/h',IER,4)
        do 56 ISUR=1,NZSUR(ICOMP)
          CVdata(IPER,ISUR,4) = airflow
   56   continue

        h(1)='Enter the maximum air temperature that can be supplied'
        h(2)='by the system to the zone. This is the design temperature'
        h(3)='for heating and is measured at the diffuser. Note that'
        h(4)='if the zone`s heating load cannot be satisfied by this'
        h(5)='supply air temp, it is assumed that any remaining heat is'
        h(6)='supplied from another source (e.g. baseboard heaters)'
        h(7)='which does not affect the flow regime.'
        WRITE(outs,'(a)')' max diffuser temperature'
        Tmax=CVdata(IPER,1,6)  ! give it a typical value
        CALL EASKR(Tmax,outs,' ',
     &             15.0,'W',80.,'W',43.0,'max supply T',IER,7)
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
      h(1)='Typically the air handling unit (AHU) conditions outdoor'
      h(2)='air. In this case, the conditioning of the outdoor air'
      h(3)='is performed by the AHU`s heating or cooling coil.'
      h(4)=' '
      h(5)='You can choose to represent the outdoor air in your model'
      h(6)='as infiltration to the zone. This would give the correct'
      h(7)='total heating or cooling load for the zone. However, in'
      h(8)='this case it is necessary to subtract the outdoor air'
      h(9)='load from the zone`s total load. In this way, the correct'
      h(10)='temperature and flow rate at the diffuser can be'
      h(11)='calculated. This is handled automatically when you'
      h(12)='select option `b`.'
      h(13)=' '
      h(14)='Alternatively, you can represent the AHU as another'
      h(15)='zone. You would have infiltration of outdoor air to the'
      h(16)='AHU zone then ventilation from the AHU zone to the'
      h(17)='occupied zone. The energy inj/extracted to condition'
      h(18)='this ventilation to the zone air-point temp must be'
      h(19)='subtracted from the zone`s total load to correctly'
      h(20)='calculate the temperature and flow rate at the diffuser.'
      h(21)='This is handled automatically with option `c`.'
      h(22)=' '
      h(23)='Use option `a` if you have no outdoor air in your model.'

      CALL EMENU(' Outdoor air treated as:',ITEM,NITEMS,Ioa)
      if(Ioa.eq.1)then
        hcphrase(2) = 'no outdoor air'
      elseif(Ioa.eq.2)then
        hcphrase(2) = 'infiltration'
      elseif(Ioa.eq.3)then
        hcphrase(2) = 'vent from zone'
      elseif(Ioa.eq.4)then
        CALL PHELPD('Outdoor air treated as',23,'-',0,0,IER)
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
      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)

      character H*72,outs*124,hcfpdescr*72
      CHARACTER*30 ITEM(12)
      integer NITEMS,INO ! max items and current menu item
      integer ihcfpt    ! for local editing.

   20 INO = -2
      ITEM(1) ='c Initialise fixed values  '
      ITEM(2) ='  _______________________  '
      write(ITEM(3),'(a,I2)') '  Periods: ',IHCFP
      ITEM(4) ='1 Add a period             '
      ITEM(5) ='2 Edit a period            '
      ITEM(6) ='3 Delete a period          '
      ITEM(7) ='  _______________________  '
      ITEM(8) ='* Find hc (equivalent m/s) '
      ITEM(9) ='! List current data        '
      ITEM(10)='> Save                     '
      ITEM(11)='? Help                     '
      ITEM(12)='- Exit                     '
      NITEMS=12

C Help text for this menu.
      h(1)='The convection calculations file contains data for'
      h(2)='controlling the calculation of internal and/or external'
      h(3)='surface convection heat transfer for selected surfaces.'
      h(4)=' '
      h(5)='It is possible to override the default convection'
      h(6)='coefficient correlations for specified surfaces and for'
      h(7)='specified time intervals.'
      h(8)=' '
      h(9) ='Four `convection-calculation control` methods are'
      h(10)='supported when you add or edit a period: '
      h(11)=' '
      h(12)='fixed coefficients - coefficients can be specified for'
      h(13)='each surface (interior and exterior to the building).'
      h(14)='The initialise fixed values option creates initial '
      h(14)='values for this option. '
      h(15)=' '
      h(16)='fixed correlations - specify a correlation to use'
      h(17)='for each surface (only applicable for interior '
      h(18)='surfaces). This could allow, for example, the use of'
      h(19)='the Alamdari & Hammond correlations for a wall, while '
      h(20)='using one of the Khalifa & Marshall correlations for a '
      h(21)='window. '
      h(22)=' '
      h(23)='adaptive - allows adaptive control of the calculations '
      h(24)='(only applicable for interior surfaces). '
      h(25)=' '
      h(26)='CEN - supports CEN standard coefficient regimes or'
      h(27)='regimes which are similar in approach. Applicable to'
      h(28)='both inside and outside surfaces. Note this option'
      h(29)='includes its own method of setting default values. CEN'
      h(30)='values are almost always imposed for the whole day'
      h(31)='rather than mixed with other convective regimes. '
      h(32)=' '
      h(33)='Note that the `outside` surface of a mlc may be interior'
      h(34)='to the building, ie. it may face the air-point of an '
      h(35)='adjacent zone. '
      h(36)=' '
      h(37)='Remember to save any changes before you leave this menu.'
      h(38)=' '
      h(39)='A cancel option should leave the model unchanged (please'
      h(40)='check to ensure the model).'

      CALL EMENU(' User defined or CEN values',ITEM,NITEMS,INO)
      IF(INO.EQ.1)then

C Create from scratch. Initialise values to default value before
C calling editing routine.
        ihcfpt=ihcfp
        CALL EASKI(ihcftp,' ',' Number of control periods ?',
     &          1,'F',MP,'F',1,'no periods',IERI,40)
        if(ieri.eq.-3)then
          goto 20
        elseif(ieri.eq.0)then
          ihcfp=ihcfpt
        endif
        NS=NZSUR(icomp)
        DO 250 I = 1,IHCFP
          hcfpdescr(I) = 'User supplied hc values'
          do 251 J = 1,NS
            HCI(I,J) = -1.0
            HCE(I,J) = -1.0
            IHCI(I,J) = -1
            IHCE(I,J) = -1
            do 253 jj=1,8
              CVdata(I,J,jj) = 0.
  253       continue
  251     continue
          call EPHCF(ICOMP,I,NS,IER)
  250   CONTINUE
      ELSEIF(INO.EQ.4)then

C Add control period,
        if(IHCFP.lt.MP)then
          IHCFP = IHCFP + 1
          IN = IHCFP
          do 252 J = 1,NS
            HCI(IN,J)= -1.0
            HCE(IN,J)= -1.0
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
     &          1,'F',IHCFP,'F',1,'change period',IERI,40)
        if(ieri.eq.-3)then
          goto 20
        else
          call EPHCF(ICOMP,IN,NS,IER)
        endif
      ELSEIF(INO.EQ.6)then

C Delete control period.
        CALL EASKI(IN,' ',' Delete which control period ? ',
     &          1,'F',IHCFP,'F',1,'delete period',IERI,40)
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
        H(1)='To give guidence on setting hc values you may supply '
        H(2)='an air velocity (m/s) and an APPROXIMATE set of '
        H(3)='hc values will be returned.  The method used is as in'
        H(4)='esp-r external hc calculations. '
        H(5)=' '
        H(6)='For comparison, typcial inside correlations based on '
        H(7)='Alamdari & Hammond (assuming 1 degree C delta T and a'
        H(8)='typical surface dimension of 2m.). '
        V1=0.15
        CALL EASKR(V1,' ','Air velocity (m/s) [see help message] : ',
     &          0.,'F',24.,'F',0.15,'air velocity',IER,8)
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
      ELSEIF (INO .EQ. 10)then

C Write data to file.
        CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)
      ELSEIF (INO .EQ. 11)then
        CALL PHELPD('conv coef data',37,'-',0,0,IER)
      ELSEIF (INO .EQ.12)then
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

      common/pophelp/h(60)
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C24/IZSTOCN(MCOM,MS)

C IAORZ is the air origin zone. '0' if outdoor air, zone number otherwise.
C CWIDTH and CHEIGHT are the channel width and height
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT
      common/HCFPHI/hcfpdescr(MP)

      CHARACTER outs*124,h*72
      character hcfpdescr*72
      character*24 hcphrase(5)
      logical ok,dok

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
      h(1)='There are lots of options. You probably want to '
      h(2)='review the synopsis before selecting anything! '
      call askok('View a synopsis of Bouyancy-driven flow in open ',
     &  'vertical channel options and background information?',ok,dok,2)
      if(OK)then

C Context help for vertical channel.
        h(1)='This choice is appropriate for open vertical channels'
        h(2)='with flow driven primarily by buoyancy resulting from'
        h(3)='surface-to-air temperature differences.'
        h(4)=' '
        h(5)='Molina & Maestre correlation was developed specifically'
        h(6)='for the SOLVENT ventilated window with a gap of 4.5 cm.'
        h(7)='For more details refer to proceedings of the '
        h(8)='EPIC AIVC 2002 conference. '
        h(9)='  '
        h(10)='Bar-Cohen and Rosenhow is a generic correlation for    '
        h(11)='open vertical channels. See Fundamentals of Heat and '
        h(12)='Mass Transfer, Incroppera and DeWitt for more details. '
        CALL PHELPD('Channel correlations',12,'-',0,0,IER)
      endif

C Establish the cause of the buoyancy driving force.
C Choose where is the air comming into the channel.
      IF((IOPT.EQ.1).OR.(IOPT.EQ.2)) THEN
        CALL EASKAB(' ','Air enters channel from','Outdoor',
     &    'Another zone',IW,0)
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
        h(1)='Enter the channel width'
        h(2)='(inside pane-to-pane distance)'
        CALL EASKR(CWIDTH,'Channel width ?',
     &    ' ',0.01,'W',1.0,'W',0.05,'Channel width',IER,2) 

C Ask Channel height
        h(1)='Enter the channel height'
        CALL EASKR(CHEIGHT,'Channel height ?',
     &    ' ',0.1,'W',100.0,'W',1.0,'Channel height',IER,1) 

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

      common/pophelp/h(60)
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/HCCEN/CENLOC(MP,14)
      COMMON/C24/IZSTOCN(MCOM,MS)
      character H*72,outs*124,SN*12,ITEM(MS+5)*30,KEY*1
      CHARACTER stuff*10
      character hcfpdescr*72
      logical close,ok,dok
      integer IW  ! for radio button
      integer NITEMS,INO ! max items and current menu item

      NS=NZSUR(IZ)

C Get time period for control interval.
      V1=ST(IN)
      H(1)='Time must be between 0. and 24. '
      CALL EASKR(V1,' ',' Start time ? ',
     &          0.,'F',24.,'F',0.,'start',IER,1)
      ST(IN)=V1
      V2=EN(IN)
      call eclose(V2,0.0,0.001,close)
      if(close) V2=24.
      H(1)='Time must be between 0. and 24. '
      CALL EASKR(V2,' ',' End time ? ',
     &          V1,'F',24.,'F',0.,'start',IER,1)
      EN(IN)=V2

  19  continue

C Determine type of control over calculations.
      h(1) ='Four `convection-calculation control` methods are'
      h(2) ='supported: '
      h(3) =' '
      h(4) ='fixed coefficients - allows coefficients to be specified '
      h(5) ='for each surface (interior and exterior to the building).'
      h(6) =' '
      h(7) ='fixed correlations - allows you to specify a correlation '
      h(8) ='to use for each surface (only applicable for interior '
      h(9) ='surfaces). This could allow, for example, the use of'
      h(10)='the Alamdari & Hammond correlations for a wall, while '
      h(11)='using one of the Khalifa & Marshall correlations for a '
      h(12)='window. '
      h(13)=' '
      h(14)='adaptive - allows adaptive control of the calculations '
      h(15)='(only applicable for interior surfaces). '
      h(16)=' '
      h(17)='CEN - supports CEN standard coefficient regimes or'
      h(18)='regimes which are similar in approach. '
      h(19)=' '
      h(20)='Note that the `outside` surface of a mlc may be interior'
      h(21)='to the building, ie. it may face the air-point of an '
      h(22)='adjacent zone. '
      IW=1
      CALL EASKATOG('calculation control (see help):',' ',
     &  'fixed coefficients','fixed correlations','adaptive cor',
     &  'CEN coef','cancel',' ',' ',IW,22)
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
        ITEM(1) ='  Surface   orien.  hci  hce'
    3   M=MHEAD
        DO 10 L=1,ILEN
          IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
            M=M+1
            icc=izstocn(iz,l)
            CALL EMKEY(L,KEY,IER)
            WRITE(ITEM(M),'(a,1x,3a,2f5.1)') KEY,SSNAME(icc),' ',
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
        H(1)='This data is usually from measurements. '
        H(2)='A -1 signals use of calculated values. '

        CALL EMENU('Conv coefficients',ITEM,NITEMS,INO)

C Edit data or display help.
        if (INO.eq.NITEMS) then
          goto 99
        elseif (INO.eq.NITEMS-1) then
          CALL PHELPD('conv coef data',2,'-',0,0,IER)
        elseif (INO.eq.NITEMS-2) then

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF
        elseif(INO.GT.MHEAD.AND.INO.LT.(NITEMS-MCTL+1))THEN

C Edit vertex identified by KEYIND.
          CALL KEYIND(NITEMS,INO,IS,IO)
          icc=izstocn(iz,is) 
          write(outs,'(a,i3,1x,a)')'Hc values for surface',is,
     &      SSNAME(ICC)(1:lnblnk(SSNAME(ICC)))
          H(1)='This data is usually from measurements. '
          H(2)='A -1 signals use of calculated values. '
          write (SN,'(2f5.1,1x)') HCI(IN,IS),HCE(IN,IS)
          CALL EASKS(SN,outs,'inside, outside?',12,' -1.0 -1.0',
     &                                            'hc values',IER,2)
          K=0
          CALL EGETWR(SN,K,HCI(IN,IS),-1.5,1000.,'F','hci value',IER)
          CALL EGETWR(SN,K,HCE(IN,IS),-1.5,1000.,'F','hce value',IER)
        else

C Not one of the legal menu choices.
          INO=-1
          goto 3
        endif
        goto 20
 99     continue
      ENDIF

C Selected correlations control: .
      IF(iw.eq.2) THEN
        H(1) ='A number of empirical correlations and fixed values, '
        H(2) ='extracted from the following sources, are supported: '
        H(3) =' '
        H(4) ='Alamdari and Hammond (1983), `Improved Data '
        H(5) ='Correlations for Buoyancy-Driven Convection in Rooms`,'
        H(6) ='BSER&T, 4(3) 106-12. '
        H(7) =' '
        H(8) ='Khalifa and Marshall (1990), `Validation of Heat '
        H(9) ='Transfer Coefficients on Interior Building Surfaces '
        H(10)='Using a Real-Sized Indoor Test Cell`, Int. J. Heat '
        H(11)='and Mass Transfer, 33 (10) 2219-2236. '
        H(12)=' '
        H(13)='Halcrow (1987), `Heat Transfer at Internal Building '
        H(14)='Surfaces`, Report to ETSU, British Department of '
        H(15)='Energy. '
        H(16)=' '
        H(17)='CIBSE Guide (1988), 5th Edition, Table A3.4.'
        H(18)=' '
        H(19)='CEN (1995), `Thermal Performance of Buildings, Room'
        H(20)='Cooling Load Calculation, General Criteria for a'
        H(21)='Simplified Calculation Method`, CEN/TC89/WG6 N.225 /'
        H(22)='JWG-N79, Section 6.2.2. '
        H(23)=' '
        H(24)='Each correlation is identified by an index.  Refer to'
        H(25)='/usr/esru/src/esp-r/esrubld/convect2.F, subroutine'
        H(26)='SELCOR1, where the index numbers held in the HTC file'
        H(27)='(what you supply NOW) are then matched with ICOR'
        H(28)='indicies in HTBOUY. A summary is listed below: '
        H(29)='IHTC ICOR           ICOR          ICOR'
        H(30)=' 1   1 for vertical, 50 horiz up, 70 horiz down'
        H(31)=' 2   5 for vertical, 50 or 60 horiz up, 70 horiz down'
        H(32)=' 3   6 for vertical, 50 or 61 horiz up, 70 horiz down'
        H(33)=' 4   2 for vertical, 51 horiz up, 71 horiz down'
        H(34)=' 5   3 for vertical, 52 horiz up, 72 horiz down'
        H(35)=' 6   4 for vertical, 53 horiz up, 73 horiz down'
        H(36)=' 7   9 for vertical, 54 horiz up, 74 horiz down'
        H(37)=' '
        H(38)='The ICOR indices are as follows: '
        H(39)='1 Alamdari & Hammond for vertical surfaces '
        H(40)='2 Halcrow time invariant low for vertical surfaces'
        H(41)='3 Halcrow time invariant high for vertical surfaces '
        H(42)='4 CIBSE for vertical surfaces '
        H(43)='5 Khalifa & Marshall for non heated vertical surfaces'
        H(44)='6 Khalifa & Marshall fan in room or floor heating'
        H(45)='7 Khalifa & Marshall radiator in room not under window'
        H(46)='8 Khalifa & Marshall fan heater in room wall oppos fan'
        H(47)='9 CEN for vertical surface'
        H(48)='10 Fisher & Pedersen for radial ceiling diffuser'
        H(49)='11 Fisher for radial ceiling diffuser'
        H(50)='12 Fisher for free horizontal jet'
        H(51)='13 Fisher thesis mixed convection'
        H(52)='14 Awbi and Hatton.'
        H(53)='15 Molina & Maestre ventilated channels '
        H(54)='16 Bar-Cohen ventilated channels '
        H(55)='30 Khalifa & Marshall window over radiator'
        H(56)='31 onwards - see source code.'
        H(57)='-1 indicates that a correlation will be chosen based '
        H(58)='on the simulation toggle. Correlation chosen for '
        H(59)='surfaces exterior to the building will be ignored. '
        hcfpdescr(IN) = 'User supplied hc correlations'
        DO 71 J=1,NS
          icc=izstocn(iz,j) 
          SN=SSNAME(icc)
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Correlation for surface',J,
     &           SN(1:lnblnk(SN)),' inside : period',IN
          IH=IHCI(IN,J)
          CALL EASKI(IH,outs,' (-1.= simulation toggle = default) ? ',
     &               -2,'W',100,'W',-1,'correlation inside',IERI,59)
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
     &               -2,'W',100,'W',-1,'correlation outside',IERI,59)
          if(ieri.eq.-3)then
            continue
          else
            IHCE(IN,J)=IH
          endif
   72   CONTINUE
      ENDIF

C Adaptive control.
      IF(iw.eq.3) THEN
        H(1) ='It is possible for the convection calculations to '
        H(2) ='`adapt` as the simulation progresses.  You must assign '
        H(3) ='a `control law` for each interior surface, which '
        H(4) ='defines how the calculations will adapt. The following '
        H(5) ='control laws are available: '
        H(6) =' '
        H(7) ='Law 1: two correlations specified for each surface; '
        H(8) ='one correlation used when plant is heating zone '
        H(9) ='(radiation or convection); other correlation used when '
        H(10)='zone is unheated. Appropriate when plant system alters '
        H(11)='convection regime within zone. '
        H(12)=' '
        H(13)='Adaptive control is only applicable for interior '
        H(14)='building surfaces. Control laws specified for exterior '
        H(15)='surfaces will be ignored. As well, a control law '
        H(16)='specified for an `outside` surface which faces the '
        H(17)='air-point of an adjacent zone will be ignored: the law'
        H(18)='specified in the neighbouring zone will take precedence.'
        H(19)='Specify law `99` in these cases where adaptive control'
        H(20)='is not applicable. '
        H(21)=' '
        H(22)='A cancel leaves the current control law unchanged.'
        hcfpdescr(IN) = 'User supplied adaptive hc correlations'
        DO 81 J=1,NS
          icc=izstocn(iz,j) 
          SN=SSNAME(icc)
          Ilaw=99
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Control law for surface',J,
     &           SN(1:lnblnk(SN)),' inside : period',IN
          CALL EASKI(Ilaw,outs,' (99=no control for this surface)',
     &               0,'W',100,'W',99,'control law inside',IERI,22)
          if(ieri.eq.-3)then
            goto 81
          endif
          CVdata(IN,J,1) = Ilaw

C Control law 1.
          if(Ilaw.eq.1) then
            H(1) ='Correlation for ON = correlation to use when plant'
            H(2) ='is heating zone. '
            H(3) =' '
            H(4) ='Correlation for OFF = correlation to use when zone'
            H(5) ='is unheated. '
            H(6) =' '
            H(7) ='Each correlation is identified by an index.  Refer'
            H(8) ='to /usr/esru/src/esp-r/esrubld/convect.F,'
            H(9) ='subroutine HTBUOY, where the index numbers are'
            H(10)='assigned to ICOR. '
            H(11)=' '
            H(12)='-1 indicates that a correlation will be chosen'
            H(13)='based on the simulation toggle. '
  102       CALL EASKS(stuff,' correlation indices',
     &                 ' ON & OFF states',10,' -1  -1',
     &                 'data for law 1',IER,13)
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
        H(1) ='It is possible for the convection calculations to '
        H(2) ='`adapt` as the simulation progresses.  You must assign '
        H(3) ='a `control law` for each interior surface, which '
        H(4) ='defines how the calculations will adapt. The following '
        H(5) ='control laws are available: '
        H(6) =' '
        H(7) ='Law 1: two correlations specified for each surface; '
        H(8) ='one correlation used when plant is heating zone '
        H(9) ='(radiation or convection); other correlation used when '
        H(10)='zone is unheated. Appropriate when plant system alters '
        H(11)='convection regime within zone. '
        H(12)=' '
        H(13)='Adaptive control is only applicable for interior '
        H(14)='building surfaces. Control laws specified for exterior '
        H(15)='surfaces will be ignored. As well, a control law '
        H(16)='specified for an `outside` surface which faces the '
        H(17)='air-point of an adjacent zone will be ignored: the law'
        H(18)='specified in the neighbouring zone will take precedence.'
        H(19)='Specify law `99` in these cases where adaptive control'
        H(20)='is not applicable. '
        H(21)=' '
        H(22)='A cancel leaves the current control law unchanged.'
        DO 82 J=1,NS
          icc=izstocn(iz,j) 
          SN=SSNAME(icc)
          Ilaw=99
          WRITE(outs,'(a,i3,1x,a,a,I2)')' Control law for surface',J,
     &           SN(1:lnblnk(SN)),' outside : period',IN
          CALL EASKI(Ilaw,outs,' (99=no control for this surface)',
     &               0,'W',100,'W',99,'control law outside',IERI,22)
          if(ieri.eq.-3)then
            goto 82
          endif
          CVdata(IN,J,5) = Ilaw

C Control law 1.
          if(Ilaw.eq.1) then
            H(1) ='Correlation for ON = correlation to use when plant '
            H(2) ='is heating zone. '
            H(3) =' '
            H(4) ='Correlation for OFF = correlation to use when zone '
            H(5) ='is unheated. '
            H(6) =' '
            H(7) ='Each correlation is identified by an index.  Refer '
            H(8) ='to /usr/esru/src/esp-r/esrubld/convect.F, '
            H(9) ='subroutine HTBUOY, where the index numbers are '
            H(10)='assigned to ICOR. '
            H(11)=' '
            H(12)='-1 indicates that a correlation will be chosen '
            H(13)='based on the simulation toggle. '
  103       CALL EASKS(stuff,' correlation for ON',
     &                 ' correlation for OFF',10,' -1  -1',
     &                 'data for law 1',IER,13)
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
C Other control laws not yet supported.
          else
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
          H(1) ='Default values are: '
          H(2) =' heat flow upwards (floor/ceiling/sloped) 5.0'
          H(3) =' heat flow downwards (floor/ceiling/sloped) 0.7'
          H(4) =' heat flow at walls (inside/partition side/) 2.5'
          H(5) =' heat flow at otherside exterior 8.0 (4+4v) where'
          H(6) ='  v is assumed to be 1m/s'
          dok=.true.
          CALL ASKOK('This is probably your initial description for',
     &      'this zone. Do you want to use default values (see help)?',
     &      OK,dok,6)
          if(ok)then
            CENLOC(in,3)=5.0
            CENLOC(in,4)=0.7
            CENLOC(in,5)=8.0
            CENLOC(in,6)=5.0
            CENLOC(in,7)=0.7
            CENLOC(in,8)=8.0
            CENLOC(in,9)=5.0
            CENLOC(in,10)=0.7
            CENLOC(in,11)=8.0
            CENLOC(in,12)=2.5
            CENLOC(in,13)=2.5
            CENLOC(in,14)=8.0
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
        H(1) ='CEN standards impose specific hc values for surfaces'
        H(2) ='facing specific directions and if the surface is a '
        H(3) ='floor or ceiling the direction of heat flow is taken'
        H(4) ='into account. The hc at surfaces facing the outside '
        H(5) ='tends to be a standard value. '
        H(6) =' '
        H(7) ='In this facility you can over-ride the standard values'
        H(8) ='in case there are country-specific values. '
        H(9) =' '
        H(10)='If you want to take the standard published values then'
        H(11)='the task is only to identify which surface in the zone'
        H(12)='is considered the typical floor and which surface is '
        H(13)='a typical ceiling (CEN needs to know this). '
        H(14) =' '
        H(15) ='Default values are: '
        H(16) =' heat flow upwards (floor/ceiling/sloped) 5.0'
        H(17) =' heat flow downwards (floor/ceiling/sloped) 0.7'
        H(18) =' heat flow at walls (inside/partition side/) 2.5'
        H(19) =' heat flow at otherside exterior 8.0 (4+4v) where'
        H(20) ='  v is assumed to be 1m/s'
        H(21) =' '
        H(22) ='Sometimes CEN tests require you to lump radiant and'
        H(23) ='convective values at the outside (typically in con-'
        H(24) ='junction with zero emissivity at the outside) and '
        H(25) ='in this case you would need to edit the standard '
        H(26) ='value of 8.0. '
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
     &        OK,dok,26)
            if(ok)then
              goto 19
            else
              goto 21
            endif 
          endif
          return
        elseif (INO.eq.NITEMS-1) then
          CALL PHELPD('CEN conv coef data',20,'-',0,0,IER)
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
     &       0.1,'F',99.,'F',5.0,'floor hc upwards',IER,26)
          CENLOC(IN,3)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.5) then
          V1=CENLOC(IN,4)
          CALL EASKR(V1,' ','Hc for downwards heat flow at floors?',
     &       0.1,'F',99.,'F',0.7,'floor hc downwards',IER,26)
          CENLOC(IN,4)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.6) then
          V1=CENLOC(IN,5)
          CALL EASKR(V1,' ','Hc at outside (exterior) floors?',
     &       0.1,'F',99.,'F',8.0,'floor hc exterior',IER,26)
          CENLOC(IN,5)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.8) then
          V1=CENLOC(IN,6)
          CALL EASKR(V1,' ','Hc for upwards heat flow at ceilings?',
     &       0.1,'F',99.,'F',5.0,'ceil hc upwards',IER,26)
          CENLOC(IN,6)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.9) then
          V1=CENLOC(IN,7)
          CALL EASKR(V1,' ','Hc for downwards heat flow at ceilings?',
     &       0.1,'F',99.,'F',0.7,'ceil hc downwards',IER,26)
          CENLOC(IN,7)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.10) then
          V1=CENLOC(IN,8)
          CALL EASKR(V1,' ','Hc at outside (exterior) ceilings?',
     &       0.1,'F',99.,'F',8.0,'ceil hc exterior',IER,26)
          CENLOC(IN,8)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.12) then
          V1=CENLOC(IN,9)
          CALL EASKR(V1,' ','Hc for upwards heat flow at sloped?',
     &       0.1,'F',99.,'F',5.0,'sloped hc upwards',IER,26)
          CENLOC(IN,9)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.13) then
          V1=CENLOC(IN,10)
          CALL EASKR(V1,' ','Hc for downwards heat flow at sloped?',
     &       0.1,'F',99.,'F',0.7,'sloped hc downwards',IER,26)
          CENLOC(IN,10)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.14) then
          V1=CENLOC(IN,11)
          CALL EASKR(V1,' ','Hc at outside (exterior) sloped?',
     &       0.1,'F',99.,'F',8.0,'sloped hc exterior',IER,26)
          CENLOC(IN,11)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.16) then
          V1=CENLOC(IN,12)
          CALL EASKR(V1,' ','Hc at inside face of walls?',
     &       0.1,'F',99.,'F',2.5,'walls hc inside',IER,26)
          CENLOC(IN,12)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.17) then
          V1=CENLOC(IN,13)
          CALL EASKR(V1,' ','Hc at other (partition) face of walls?',
     &       0.1,'F',99.,'F',2.5,'walls hc partition',IER,26)
          CENLOC(IN,13)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.18) then
          V1=CENLOC(IN,14)
          CALL EASKR(V1,' ','Hc at outside (exterior) walls?',
     &       0.1,'F',99.,'F',8.0,'walls hc exterior',IER,26)
          CENLOC(IN,14)=V1
          hcfpdescr(IN) = 'CEN user edited hc coefficients'
        elseif (INO.eq.20) then

C Reset hc to default values.
          CENLOC(in,3)=5.0
          CENLOC(in,4)=0.7
          CENLOC(in,5)=8.0
          CENLOC(in,6)=5.0
          CENLOC(in,7)=0.7
          CENLOC(in,8)=8.0
          CENLOC(in,9)=5.0
          CENLOC(in,10)=0.7
          CENLOC(in,11)=8.0
          CENLOC(in,12)=2.5
          CENLOC(in,13)=2.5
          CENLOC(in,14)=8.0
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
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      COMMON/HCCEN/CENLOC(MP,14)
      COMMON/Vld20/Vldtng
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT
      common/HCFPHI/hcfpdescr(MP)
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
      WRITE(IUF,'(I4,a)',IOSTAT=ISTAT,ERR=990)
     &        IHCFP,' # number control intervals'
      WRITE(IUF,'(I4,a)',IOSTAT=ISTAT,ERR=990)
     &        NZSUR(ICOMP),' # number of surfaces'

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
     &          ST(I),EN(I),' # start and end time of interval'
        if(ICTL(I).eq.1)then
          WRITE(IUF,'(I4,a)',IOSTAT=ISTAT,ERR=990)
     &          ICTL(I),' # fixed coef. calculation control type'
        elseif(ICTL(I).eq.2)then
          WRITE(IUF,'(I4,a)',IOSTAT=ISTAT,ERR=990)
     &          ICTL(I),' # fixed correlation calculation control type'
        elseif(ICTL(I).eq.3)then
          WRITE(IUF,'(I4,a)',IOSTAT=ISTAT,ERR=990)
     &          ICTL(I),' # adaptive calculation control type'
        elseif(ICTL(I).eq.4)then
          WRITE(IUF,'(I4,a)',IOSTAT=ISTAT,ERR=990)
     &          ICTL(I),' # CEN calculation control type'
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
            CALL EGETWRA(IUF,HCIA,NS,-1.,99.,'W','inside face hc',IER)
            CALL EGETWRA(IUF,HCEA,NS,-1.,99.,'W','other face hc',IER)
            DO 114 KV=1,NS
              HCI(I,KV)=HCIA(KV)
              HCE(I,KV)=HCEA(KV)
  114       CONTINUE
          elseif(ICTL(I).eq.2) then

C Correlation indices. << what is upper range? >>
            CALL EGETWIA(IUF,IHCIA,NS,0,90,'W',
     &               'inside face hc cor',IER)
            CALL EGETWIA(IUF,IHCEA,NS,0,90,'W',
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

      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/HCFP/IHCFP,ST(MP),EN(MP),HCI(MP,MS),HCE(MP,MS),
     &            ICTL(MP),IHCI(MP,MS),IHCE(MP,MS),CVdata(MP,MS,8)
      common/HCFPHI/hcfpdescr(MP)
      COMMON/HCCEN/CENLOC(MP,14)

      character H*72,outs*124
      character hcfpdescr*72

C Check if there is a *.htc file for this zone.
      if(IHC(ICOMP).EQ.0)then
        return
      endif

C List control data.
      NS=NZSUR(icomp)
      call edisp(itru,' ')
      WRITE(outs,'(A,I2)')' Number of control periods: ',IHCFP
      call edisp(itru,outs)
      WRITE(outs,'(A,I2)')' Number of surfaces =',NS
      call edisp(itru,outs)
      call edisp(itru,' ')

C If nothing to report, return.
      if(ihcfp.eq.0) return
      if(NS.eq.0) return

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
          call edisp(itru,' Surface            Inside   Outside ')
          DO 860 J=1,NS
            icc=izstocn(icomp,j) 
            WRITE(outs,5203)J,SSNAME(ICC),' (',SSVFC(ICC),') ',
     &        HCI(I,J),HCE(I,J)
 5203       FORMAT(I2,1x,4a,2(1X,F9.3))
            call edisp(itru,outs)
  860     CONTINUE
        elseif(ICTL(I).eq.2) then

C Correlation indices.
          call edisp(itru,' Surface            Inside       Outside')
          DO 861 J=1,NS
            icc=izstocn(icomp,j) 
            WRITE(outs,5205) J,SSNAME(icc),' (',SSVFC(icc),') ',
     &        IHCI(I,J),IHCE(I,J)
 5205       FORMAT(I2,1x,4a,2(2X,I9))
            call edisp(itru,outs)
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
