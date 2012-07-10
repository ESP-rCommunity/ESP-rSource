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

C ********************************************************************
C This file contains subroutines relating to the complex fenestration
C construction (CFC).
C
C Main Reference:
C     Lomanowski, B.A. (2008) 'Implementation of Window Shading Models
C     into Dynamic Whole-Building Simulation', MASc Thesis, University
C     of Waterloo.
C
C ********************************************************************

C The following routines are included (copyright Achim Geissler):
C
C   CFC2rad            Writes Radiance files for CFC constructions.
C                      This subroutine expects that CFC commons are 
C                      populated! File names / handles are used via 
C                      e2r_common.h
C
C   CFCGlazing         Writes glazing materials file for CFC 
C                      constructions
C
C   CFCDetailedBlind   Writes detailed blind generation (genblinds)
C
C   CFCCreateBRTDfun   Create detailed glazing model (BRTDfunc) for 
C                      a CFC glazing description. -- dummy --


C ********************************************************************
C                         -- CFC2rad --
C
C Create Radiance definitions for CFC surfaces. This routine is
C called in e2r and bps (casual.F / intlum). There are three different cases
C possible:
C  1) Use overall model for glass and blinds (ExplicitVB false
C     and BRTDfunc false)
C  2) Use simple glazing model and exlplict blind model (ExplicitVB
C     true and BRTDfunc false) and
C  3) Use detailed glazing model (BRTDfunc) and explicit blind 
C     model (ExplicitVB true and BRTDfunc true).
C
C May 13: Called from esrue2r/e2rform.F subroutine mkriofil(), line 813.
C Jun 16: Called from esrubld/casual.F subroutine intlum(), line 1425.
C 
C Created by: Achim Geissler
C Initial Creation: May 2012
C 
C Call parameters
C   init       toggle for first call in mkriofil() init=2, first
C              call in intlum() init=1 and subsequent 
C              calls in intlum() init=0
C <<   file handling? For init, assume file(s) open, for subsequent
C      calls open / close, here? >>
C   icomp      zone 
C   isurf      surface
C   ExplicitVB toggle - if true, then explicit (genblinds) model is
C              used for venetian type blinds
C   BRTDfunc   toggle - if true, then separate setup for glazing and
C              blind for (very) detailed calculations is used. If 
C              false, then standard "glass" material is used for
C              glazing
C
C Return values
C   None (?)   >> writes Radiance definitions for CFC constructions
C                 to files 'imatfil' and 'icfcfil'
C
C ********************************************************************
      subroutine CFC2rad(init,icomp,isurf)
c      USE h3kmodule ! ? necessary??

      IMPLICIT NONE
#include "building.h"
#include "geometry.h"
#include "CFC_common.h"
#include "e2r_common.h"

C Parameters
      integer init,icomp,isurf

C Commons
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      
      COMMON/CFCtemp/firstzone,lastzone,lasttype
      integer firstzone,lastzone,lasttype

C Local variables
      real r,PI,incang,srefl
      real VTDIR,VTDIF,VTRAN,gvt1,gvt

      integer ne,i,iz,im,itype,icfctp,icn,vb_lay
c      integer ne,iz,im,itype,nslats,icfctp,icn,vb_lay
      logical firstzisset

      character matname*72

      integer store_blind_setting

C Function declarations
      integer lnblnk

C     Search through model to get first and last CFC ...
      if (init.ge.1) then
        firstzisset=.false.
        do 5 iz=1,ncomp
          do 10 im=1,nzsur(iz)
            icfctp=icfcfl(iz,im)
            if (icfctp.eq.1) then
              CFCCoupling(iz)=.true. ! this zone uses CFCs ...
              if (.not.firstzisset) then
                firstzone=iz
                firstzisset=.true.
              endif
              lastzone=iz
            endif
            if (icfctp.eq.ncfc(iz)) then
              lasttype=icfctp
            endif
 10       continue
  5     continue
      endif


C DEBUG:
c      write(*,*)'CFC2Rad; init=',init,' icomp=',icomp,' isurf',
c     &    isurf,' firstzone=',firstzone,' lasttype=',lasttype
C Now conditionally creat model ...
      icfctp=icfcfl(icomp,isurf) ! set type to current

C DEBUG:
c      write(*,*)'ExplicitVB=',ExplicitVB(icomp,icfctp),
c     &         ' BRTDFunc=',BRTDFunc(icomp,icfctp)

C Check if combination of logical parameters is valid
      if (BRTDfunc(icomp,icfctp)) then
        if (.not.ExplicitVB(icomp,icfctp)) then
        ! not possible!
          call usrmsg(' BRTDfunc must use explicit blind!',
     &                ' Aborting!','W')
          return
        endif
      endif


      if (icfctp.ge.1) then ! we have a CFC
cx        write(*,*)'CFC type ',icfctp

        if (BRTDfunc(icomp,icfctp)) then !
          call CFCCreateBRTDfun(init,icomp,isurf)

        else ! use 'glass'
          if (ExplicitVB(icomp,icfctp)) then
            store_blind_setting=i_shd(icomp,icfctp)
            i_shd(icomp,icfctp)=0 ! glazing data is w/o blind
          endif

C         Write glazing materials file for CFC construction
          call CFCGlazing(icomp,isurf)

          if (ExplicitVB(icomp,icfctp)) then
C           Restore blind position
            i_shd(icomp,icfctp)=store_blind_setting
          endif

        endif ! BRTDfunc ?

C       Glass data is set and Radiance data written. Now the blind
C       is generated, if "Explicit".

        if (ExplicitVB(icomp,icfctp)) then

C         get layer number in CFC 
          ne=ncfc_el(icomp,icfctp) ! number of cfc layers for this type
          do 110 i=1,ne
            itype=icfcltp(icomp,icfctp,i)
            if (itype.eq.2) vb_lay=i
  110     continue

          if (vb_lay.ne.1) then
C           If it is not "1", then it is an
C           intermediate blind ... do what? "Not possible"? (!!)
            call usrmsg(
     &            ' Invalid blind position for Radiance coupling ',
     &            ' in explicit VB mode! Aborting!','W')
            return
          endif

          if (init.eq.2) then    
            ! Generate slat material description and append to 
            ! materials file imatfil (which is assumed open!)

            icn=IZSTOCN(icomp,isurf)
            WRITE(matname,'(5a,I1)') 'vb_',
     &                    SSMLCN(icn)(1:lnblnk(SSMLCN(icn))),'_',
     &                    SSNAME(icn)(1:lnblnk(SSNAME(icn))),'_',
     &                    icomp

C           Check for transmission of slat material, if > zero,
C           use "trans" material. Otherwise "plastic"
            if (visT(icomp,icfctp,vb_lay).gt.0.0) then
            ! material 'trans'
              WRITE(imatfil,'(2a)') 
     &             'void trans ',matname(1:lnblnk(matname))
              WRITE(imatfil,'(a)') '0 '
              WRITE(imatfil,'(a)') '0 '
              srefl=visRf(icomp,icfctp,vb_lay)

C             Convert visible transmission to transmissivity 
C             (as per Ward 1/12/94 Radiance 2.4 paper).
              VTRAN=visT(icomp,icfctp,vb_lay)
              if((VTRAN).gt.0.01)then
                gvt1=sqrt(0.8402528435+(0.007252224*VTRAN*VTRAN))
                gvt=(gvt1-0.9166530661)/0.0036261119/VTRAN
              else
                gvt=0.01
              endif

C t_red t_green t_blue specularity roughness transmissivity transmitted_specular
              WRITE(imatfil,'(a,3F5.2,a,F5.2,a)')
     &              '7 ',srefl,srefl,srefl,' 0.01 0.10 ',gvt,'  0.97'
cx     &              visT(icomp,icfctp,vb_lay),'  0.97'
Cx                    *** need to transform visT() to "Radiance transmissivity"??!!
              WRITE(imatfil,'(a)') '  '

            else ! material 'plastic'
              WRITE(imatfil,'(2a)') 
     &             'void plastic ',matname(1:lnblnk(matname))
              WRITE(imatfil,'(a)') '0 '
              WRITE(imatfil,'(a)') '0 '
C 'back' vs. 'front' concept in Radiance for blind??
              srefl=visRf(icomp,icfctp,vb_lay)
C t_red t_green t_blue specularity roughness
              WRITE(imatfil,'(a,3F5.2,a)')
     &              '5 ',srefl,srefl,srefl,' 0 0 '
              WRITE(imatfil,'(a)') '  '
            endif ! slat transmissivity?

C==??? ... also automatically generate blind casing obstruction for
C          explicit blind ???

          endif ! blind init, i.e. write materials file

C         Generate detailed blind description and write to
C         file icfcfil
          call CFCDetailedBlind(icomp,isurf)

        endif ! ExplicitVB

      endif ! CFC type >= 1 (i.e. it is actually a CFC)

      return
      end


C ********************************************************************
C                         -- CFCGlazing --
C
C Create Radiance glazing material definition for CFC surfaces.
C 
C Created by: Achim Geissler
C Initial Creation: June 2012
C 
C Call parameters
C   icomp      zone 
C   isurf      surface
C
C Return values
C   None (?)   >> writes Radiance definitions to file 'icfcfil'
C
C ********************************************************************
      subroutine CFCGlazing(icomp,isurf)

      IMPLICIT NONE
#include "building.h"
#include "esprdbfile.h"
#include "site.h"
#include "geometry.h"
#include "CFC_common.h"
#include "e2r_common.h"

C Parameters
      integer icomp,isurf

C Commons
      integer ifil
      COMMON/FILEP/IFIL

      COMMON/OUTIN/IUOUT,IUIN
      integer IUOUT,IUIN

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

c      COMMON/SUNPOS/SAZI,SALT,ISUNUP
c      real SAZI,SALT
c      integer ISUNUP

      COMMON/CFCtemp/firstzone,lastzone,lasttype
      integer firstzone,lastzone,lasttype

C     Climate data (present and future time rows)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      real QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM

C Local variables 
      integer icfctp,icn,ISTAT,itsys,IER,IANS

      real incang,dummy1,dummy2,CAI,psazi,psalt
      real VTDIR,VTDIF,VTRAN,gvt1,gvt,extDIR0,extDIF0

      real proang,CFCabs
      real TRANSBB_SYSV,TRANSD_SYSV,REFL_SYSV,CFCabsV
      real TRANSD_SYS_BD,TRANSD_SYS_SKY,TRANSD_SYS_GRD
      real tmpIBM,tmpIDF,EXDIR,EXDIF,EXDIFSKY,EXDIFGRD
      real AbsSolDir,AbsSolDifGrd,AbsSolDifSky,AbsVis

      character glaznam*72,cfcfile*72,outs*124

      integer iSolVis ! switch for CFC solar / visual data calculation
      integer i_direct ! switch for calling solar_multilayer

      DIMENSION AbsSolDir(me),AbsSolDifGrd(me)
      DIMENSION AbsSolDifSky(me),AbsVis(me)

      DIMENSION psalt(24),psazi(24)

C Function declarations
      integer lnblnk

      icfctp=icfcfl(icomp,isurf) ! get type

C     Write header only once!
C     File is closed in CFCDetailedBlind() !
      if (icomp.eq.firstzone .and. icfctp.eq.1) then
        CALL ERPFREE(icfcfil,ISTAT) ! icfcfil is in e2r commons and is set in e2r main
        IF(ISTAT.LT.0)THEN
          call usrmsg(' CFCGlazing: Problem detected in ERPFREE / ',
     &                 cfcfil,'W')
c          IER=1
          return
        ENDIF

c        write(*,*)'* CFCGlazing, runpath=',runpath(1:lnblnk(runpath)),
c     &             ', cfcfil=',cfcfil(1:lnblnk(cfcfile))

        write(cfcfile,'(2a)')runpath(1:lnrp),cfcfil(1:lnblnk(cfcfil))
c        write(*,*)'* CFCGlazing, cfcfile=',cfcfile(1:lnblnk(cfcfile))
        call FPOPEN(icfcfil,ISTAT,1,3,cfcfile)

        IF(ISTAT.LT.0)THEN
          call usrmsg(' CFCGlazing: Problem detected while opening:',
     &                   cfcfil,'W')
c          IER=1
          return
        ENDIF

        WRITE(icfcfil,'(a)')'# Radiance CFC glazing definitions'
        WRITE(icfcfil,'(a)')
     &               '# (first line of file must not be edited)'
        WRITE(icfcfil,'(a)')'  '
      endif

C DEBUG:
cx          write(*,*)'CFC type ',icfctp,', vb on/off: ',
cx     &              i_shd(icomp,icfctp),', vb angle: ',
cx     &              vb_phiNew(icomp,icfctp)

C Values for radiation and solar angle should only be set for
C non-conflation type calls ("visualization" calls).
        tmpIDF=0.0   ! interior diffuse source
        tmpIBM=0.0   ! interior beam source
        extDIR0=1.0E-3 ! ?? is there a problem with "0"?  10.07.2012 ... 
        extDIF0=1.0E-3 !
        i_direct=0   ! toggle for ... ?

        if (SCENEPURP(ISCENE)(1:8).eq.'External'.or.
     &          SCENEPURP(ISCENE)(1:8).eq.'Internal'.or.
     &          SCENEPURP(ISCENE)(1:5).eq.'Glare') then
C         == from bps.F, line 984 ===
          call EFOPRAN(ifil,lclim(1:lnblnk(lclim)),144,0,IER)

          if (IER.ne.0) then
            write(outs,1000)lclim(1:lnblnk(lclim))
 1000       format(' ** CFC error: EFOPRAN file open: ','(A)','.')
            call edisp(iuout,outs)
            goto 9999
          endif

C         Set data to values according to chosen day/season
C         /sky1/ common holds 'irdoy' - day of year and 'rtime' - time of day
C         irdoy & rtime are set in "settime()", called in 'autorad'
C         Recover one day's data from climate file into common 
C         block for use.
          call CLMGET(irdoy) ! stores to commons CDIR() etc.
          EXDIR=CDIR(int(rtime))
          EXDIF=CDIF(int(rtime))
C         ... rather call MZSRAD()??
          EXDIFSKY=EXDIF*(1.-rgrfl)
          EXDIFGRD=EXDIF*rgrfl

          itsys=1 ! local mean time (2 would be local apparent time)
          call AZALT(irdoy,itsys,sitelongdif,sitelat,psalt,psazi)

C         == from solar.F ===
C         Calculate the angle of incidence between the sun and 
C         surface ISRF, ANGI; COS of ANGI, CAI;
          CALL CFCMZSCAI(icomp,isurf,CAI,incang)

          if (ExplicitVB(icomp,icfctp)) then
            incang=0. ! "normal to surface"
          endif

        elseif (SCENEPURP(ISCENE)(1:8).eq.'Day_fact') then
C         Set data to daylight factor (DF) typical values
C         "10 klux" diffuse sky => corresp. EXDIF?; EXDIR=0.
C == casual.F, line 1177 == ... use this?
C Calculate reference horizontal illuminance:
C          call REFILL(QDIF,QDIR,SALT,SAZI,IDYP,refell)

          EXDIR=0.
          EXDIF=10000./179. !??
          EXDIFSKY=EXDIF*(1.-groundrefl)
          EXDIFGRD=EXDIF*groundrefl
          incang=0.
          write(*,*)'* Day_fact with CFC EXPERIMENTAL ( ;o) )'
cx          return

        elseif (SCENEPURP(ISCENE)(1:8).eq.'Coupling') then
C         Set data to current values
          EXDIR=QDP
          EXDIF=QFP
          EXDIFSKY=EXDIF*(1.-groundrefl)
          EXDIFGRD=EXDIF*groundrefl

          CALL CFCMZSCAI(icomp,isurf,CAI,incang)

          if (ExplicitVB(icomp,icfctp)) then
            incang=0. ! "normal to surface"
          endif

        elseif (SCENEPURP(ISCENE)(1:7).eq.'Sun_pos') then
C         Pick start and end time ???
          write(*,*)'* Sun_Pos not implemented with CFC'
          return
        endif

C       Check error state
        IF(incang.LT.0..OR.incang.GT.90.)THEN
          write(outs,1010)incang
 1010     format(' ** CFC error: MZSLGN incidence angle = ',
     &                                               F10.3,'.')
          call edisp(iuout,outs)
          goto 9999
        ENDIF

C ******************* from solar.F / line 1748 ff. *********************
C The following code should probably be put in a designated subroutine.

C Is surface facing away from the sun so that it can only
C receive diffuse component. IANS=1; yes, self-shaded.
        IANS=0
        CALL CFCMZSFSH(icomp,isurf,IANS)
cx ... necessary? (line 579 ff. solar.F)

        iSolVis=1  ! calculate visual properties, only

        if (IANS.eq.1) then
C       surface is self-shaded
             incang=0.
             proang=0.
C            Save profile angle for CFC output, -1 indicates self shaded
             proang_s(icomp,icfctp,isurf)=-1. 
        else ! not self shaded
          call profile_angle(icomp,isurf,icfctp,proang)
        endif

        ! values include (optional) blinds 
        ! at current blind angle!
        call cfc_eff_opt_prop(icomp,isurf,icfctp,incang,proang,
     &                                                     iSolVis)

C DEBUG:
c          write(*,*)'** Blind posn=',i_shd(icomp,icfctp),
c     &          ' Blind angle=',vb_phiNew(icomp,icfctp),
c     &          ' EXDIR=',EXDIR,' EXDIF=',EXDIF,
c     &          ' EXDIFSKY=',EXDIFSKY,' EXDIFGRD=',EXDIFGRD,
c     &          ' incang=',incang,' proang=',proang

C       Check whether sky and ground diffuse calc. is requested
        if (i_ground_sky_diff_calc.eq.1) then

C         Calculate CFC system visual BEAM transmitted
          call solar_multilayer(i_direct,icomp,isurf,icfctp,
     &                EXDIR,extDIF0,
     &                tmpIBM,tmpIDF,iSolVis,
     &                TRANSBB_SYSV,TRANSD_SYS_BD,dummy1,AbsSolDir)

C         Calculate CFC system solar SKY diffuse transmitted
          call solar_multilayer(i_sky,icomp,isurf,icfctp,
     &                extDIR0,EXDIFSKY,
     &                tmpIBM,tmpIDF,iSolVis,
     &                dummy1,TRANSD_SYS_SKY,dummy2,AbsSolDifSky)

C         Calculate CFC system solar GROUND diffuse transmitted
          call solar_multilayer(i_ground,icomp,isurf,icfctp,
     &                extDIR0,EXDIFGRD,
     &                tmpIBM,tmpIDF,iSolVis,
     &                dummy1,TRANSD_SYS_GRD,dummy2,AbsSolDifGrd)

C         Sum up diffuse transmitted contributions (sky + ground)
          TRANSD_SYSV = TRANSD_SYS_BD +
     &                    TRANSD_SYS_SKY + TRANSD_SYS_GRD 
C         Absorbed visual is of no interest

        else 
C         Legacy model.
C         Calculate CFC system visual beam transmitted, diffuse transmitted.
          call solar_multilayer(i_direct,icomp,isurf,icfctp,
     &        EXDIR,EXDIF,
     &        tmpIBM,tmpIDF,iSolVis,TRANSBB_SYSV,TRANSD_SYSV,
     &        REFL_SYSV,AbsVis)
        endif

C       Normalise visual data to radiation available for output/storing of 
C       fraction. These fractions are used in "casual.F/DWINVT()" as light 
C       transmittance values (direct/diffuse) - the fraction is based on 
C       solar radiation with visual properties for CFC layers. 
        if (EXDIR.gt.0.) then
          transBB_V(icomp,isurf)=TRANSBB_SYSV !/EXDIR
        else
          transBB_V(icomp,isurf)=0.
        endif
        if (i_ground_sky_diff_calc.eq.1) then
          if ((EXDIR+EXDIFSKY+EXDIFGRD).gt.0.) then
            transD_V(icomp,isurf)=TRANSD_SYSV ! /(EXDIR+EXDIFSKY
!     &                                     +EXDIFGRD)
C           transBB_V is W/m2 transmitted 'beam-beam', transD_V is W/m2
C           transmitted 'beam-diffuse' + 'diffuse-diffuse'. Average "overall"
C           transmission fraction is thus:
            VTRAN=(transBB_V(icomp,isurf)+transD_V(icomp,isurf))/
     &                                      (EXDIR+EXDIFSKY+EXDIFGRD)
          else
             transD_V(icomp,isurf)=0.
             VTRAN=0.
          endif
        else ! legacy
          if ((EXDIR+EXDIF).gt.0.) then
            transD_V(icomp,isurf)=TRANSD_SYSV ! /(EXDIR+EXDIF)
C           See above for description.
            VTRAN=(transBB_V(icomp,isurf)+transD_V(icomp,isurf))/
     &                                                  (EXDIR+EXDIF)
          else
             transD_V(icomp,isurf)=0.
             VTRAN=0.
          endif
         
        endif

C ******************************************************************

cx          VTDIR=transBB_V(icomp,isurf) 
cx          VTDIF=transD_V(icomp,isurf)
cx          VTRAN=VTDIR+VTDIF ! not really good ...

C       Convert visible transmission to transmissivity 
C       (as per Ward 1/12/94 Radiance 2.4 paper).
        if((VTRAN).gt.0.01)then
          gvt1=sqrt(0.8402528435+(0.007252224*VTRAN*VTRAN))
          gvt=(gvt1-0.9166530661)/0.0036261119/VTRAN
        else
          gvt=0.01
        endif

C       Write glazing material to file. Use construction name
C       extended with surface name and zone number (the same 
C       construction can be used by different CFC layer setups 
C       / blind settings(!) spread over more than one zone
        icn=IZSTOCN(icomp,isurf)
        write(glaznam,'(4a,I1)')
     &                  SSMLCN(icn)(1:lnblnk(SSMLCN(icn))),'_',
     &                  SSNAME(icn)(1:lnblnk(SSNAME(icn))),'_',
     &                  icomp
        WRITE(icfcfil,'(a,a)') 'void glass ',
     &               glaznam(1:lnblnk(glaznam))
        WRITE(icfcfil,'(a)') '0 '
        WRITE(icfcfil,'(a)') '0 '
        WRITE(icfcfil,'(a,3F6.2)') '3  ',gvt,gvt,gvt
        WRITE(icfcfil,'(a)') '  '

9999  continue

      return
      end


C ********************************************************************
C                         -- CFCDetailedBlind --
C
C Create Radiance definition for explicit blind model (ExplicitVB
C true) 
C 
C Created by: Achim Geissler
C Initial Creation: June 2012
C 
C Call parameters
C   icomp      zone 
C   isurf      surface
C
C Return values
C   None (?)   >> writes Radiance definitions to file 'icfcfil'
C
C ********************************************************************
      subroutine CFCDetailedBlind(icomp,isurf)

      IMPLICIT NONE
#include "building.h"
#include "geometry.h"
#include "CFC_common.h"
#include "e2r_common.h"

C Parameters
      integer icomp,isurf

C Commons
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/CFCtemp/firstzone,lastzone,lasttype
      integer firstzone,lastzone,lasttype

C Local variables
      integer icfctp,icn,nslats,ISTAT

      real sw,sp,sr,phi,crown,wr,sth,tilt
      real x0,y0,z0,cfc_width,cfc_height
      real dy,vb_width,vb_height

      character matname*72,cmdstr*144,transfstr*144
      character curvature*4

C Function declarations
      integer lnblnk


C Body
      if (i_shd(icomp,icfcfl(icomp,isurf)).eq.0) then
      
C     ... actually, do nothing!
        write(*,'(a,i1,a,i1,a)')'** CFCDetailedBlind(): i_shd(',
     &      icomp,',',icfcfl(icomp,isurf),') = 0, i.e. blind is up!'

      else ! blind is down, so create it ...

        icfctp=icfcfl(icomp,isurf) ! get type

C -------------------------------------------------------------------
C     Get lower left corner of CFC surface and surface width
C     The Venetian type blind will be created wider by 2 x dy, so that 
C     it overlaps the glazing area. Typically such an overlap will be
C     in the range of 30 to 50 millimeters either side.
C -------------------------------------------------------------------


cx====
        dy=0.05 ! set fixed value, for now

CCC e2rform.F, line 1406 ff.
C Shift surface inwards by 3mm (only if extern=true).
c            N=NVER(isf)
C            if (extern) then
c              vdis = -0.003

C Find transformation matrices that normalise face.
c              call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
c              IF (IERR .NE. 0)then
c                write(outs,'(a,a)') ' PLEQN problem with ',zsn(isf)
c                call edisp(itru,outs)
c              endif
c              DO 352 K = 1,3
c                TRNS(k)=EQN(k)*vdis
c  352         continue
C            else
C              TRNS(1)= 0.00
C              TRNS(2)= 0.00
C              TRNS(3)= 0.00
C            endif
c            do 353 k=1,N
c              XT(k)=XX(k)+TRNS(1)
c              YT(k)=YY(k)+TRNS(2)
c              ZT(k)=ZZ(k)+TRNS(3)
c  353       continue

cx=====
          
        x0=0.08  ! use lower left corner (?)
        y0=0.    ! use lower left corner (?)

        cfc_width=2.84 ! x0=0.08, x1=2.92
      
        if (icfctp.eq.1) then
          cfc_height=2.04 ! "win_bot"
          z0=0.38
        else
          cfc_height=0.8 ! "win_top"
          z0=2.42
        endif

        vb_width=cfc_width+2.*dy
        vb_height=cfc_height

        x0=x0-dy

C -------------------------------------------------------------------
C         Get venetian type blind data (if any)
C -------------------------------------------------------------------
C         Assign slat blind descriptors to local variables
        sw=vb_w(icomp,icfctp)/1000.        ! slat width in [m]
        sp=vb_s(icomp,icfctp)/1000.        ! slat pitch in [m]
        phi=vb_phiNew(icomp,icfctp)        ! blind slat angle
C DEBUG
cx        write(*,*)'** slat angle=',phi

        crown=vb_crown(icomp,icfctp)/1000. ! crown height [m]
cx        wr=vb_wr(icomp,icfctp)             ! width/curvature ratio
cx        sth=vb_t(icomp,icfctp)             ! max slat thickness
c        tilt=PELV(icomp,isurf)            ! tilt of cfc surface

c       if (??) then ! ?? is there a CFC-setting for this??
          write(curvature,'(a)')' -r '       ! slats curved downward (standard)
c       else
c          write(curvature,'(a)')' -r '      ! slats curved upward
c       endif

C -------------------------------------------------------------------
C         Create Venetian type blind description (genblinds) with 
C         current slat angle
C -------------------------------------------------------------------

C the "scale" used in genblind is the slat width (parameter 
C "depth" in call to genblinds). All other numbers given
C for the dimension should be calculated accordingly.

C lower inside left corner (viewed from outside) is 0,0,0 (?!) 
C       -s is the scale, enter slat width in [m] here!
C               -rotation z, always necessary
C                       -rotation z should be actual surface azimuth (psazi?)
C                                  -translation to lower left coordinate of CFC surface
C xform -s 0.08 -rz -90 -rz  4.118 -t  0.000  -16.408  0.06
C
C - x and y seem to be switched in Radiance ... (needs checking!)
C - file handling for non-init calls, i.e. when called on time step basis??
C   Open/close file? 

        ! calculate number of slats necessary to cover height
        nslats=ceiling(vb_height/sp) !+1

        ! rescale Venetian blind width and height to multiples of
        ! slat width
        vb_width=vb_width/sw
        vb_height=vb_height/sw

        ! calculate slat curvature radius sr (this is the value genblinds 
        ! expects) from given crown height and then rescale to multiple 
        ! of slat width ...
        sr=(crown*crown+sw*sw/4.)/(2.*crown)/sw

        icn=IZSTOCN(icomp,isurf)
        WRITE(matname,'(5a,I1)') 'vb_',
     &                    SSMLCN(icn)(1:lnblnk(SSMLCN(icn))),'_',
     &                    SSNAME(icn)(1:lnblnk(SSNAME(icn))),'_',
     &                    icomp

C     genblinds mat       name  depth width  height nslats angle [ -r|+r rcurv ]
C    !genblinds <matname> blind  1    32.165   44     49  <<-phi>>  <<-r sr>>
        WRITE(cmdstr,
     &        '(3a,I1,a,I1,a,I1,a,F7.2,a,F6.2,a,I3,a,F5.1,a,F5.3)') 
     &        '!genblinds ',matname(1:lnblnk(matname)),
     &        ' blind_z',icomp,'_',icfctp,' ',1,' ',
     &        vb_width,' ',vb_height,' ',nslats,' ',phi,
     &        curvature,sr
cx     &        ' -r ',crown

        WRITE(transfstr,
     &             '(a,F7.4,a,3(F6.3,1x))')
     &             'xform -s ',sw,' -rz 90 -t ',
     &             cfc_width+2.*dy,-(sw+0.04),z0 !+sp/2.
          ! use esp_y for lower right corner, here ("x")?

        WRITE(icfcfil,'(a)') ' '
        WRITE(icfcfil,'(a)') '# generate blinds and transfer:'
        WRITE(icfcfil,'(3a)') cmdstr(1:lnblnk(cmdstr)),' | ',
     &                            transfstr(1:lnblnk(transfstr))
        WRITE(icfcfil,'(a)') '  '

C       File is opened in subroutine CFCGlazing()!
C       Free file handle after last CFC is written
        if (icomp.eq.lastzone .and. icfctp.eq.lasttype) then
          CALL ERPFREE(icfcfil,ISTAT)
        endif

      endif ! blind not up

      return
      end


C ********************************************************************
C                         -- CFCCreateBRTDfun --
C
C Create detailed glazing model (BRTDfunc) for a CFC glazing
C description.
C
C Created by: Achim Geissler
C Initial Creation: June 2012
C 
C Call parameters
C   init       toggle for first call in mkriofil() init=2, first
C              call in intlum() init=1 and subsequent 
C              calls in intlum() init=0
C   icomp      zone 
C   isurf      surface
C
C Return values
C   None (?)   >> writes Radiance definitions to file 'icfcfil'
C
C ********************************************************************
      subroutine CFCCreateBRTDfun(init,icomp,isurf)

      IMPLICIT NONE
#include "building.h"
#include "geometry.h"
#include "CFC_common.h"
#include "e2r_common.h"

C Parameters
      integer init,icomp,isurf

C Local variables
      integer i,ne,icfctp,itype

      write(*,*)' ** not implemented! **'
      return

      if (init.eq.1) then ! initialization, build model

        icfctp=icfcfl(icomp,isurf) ! get type

        ne=ncfc_el(icomp,icfctp) ! number of cfc layers for this type

C       Loop through CFC layers and generate ... 

        do 100 i=1,ne
          itype=icfcltp(icomp,icfctp,i)
 
          if (itype.eq.1) then  ! glass layer
C         ----------------------------------------------------------
C         Create BRTDfunc from glazing layer properties ...
C         ----------------------------------------------------------

C         ... the BRTD-generation requires
C         knowledge of the individual glass layers 
C         - set up to use IGDB index?
C         - or "file" name (product / coating name)?

C         Step 1:
c           call glaze with layer buildup acc. to CFC description
c           (without (!) blind)

C         Step 2:
c           append output of glaze to materials file from e2r

C         Step 3:
c           ??

C############################################
C# Glazing produced by Radiance glaze script
C# Fri Aug  7 16:14:41 WEDT 2009
C# Material surface normal points to interior
C# Number of panes in system: 2
C# Exterior surface s1 type: clear glass
C# Inner surface s2 type: SYP YRE0146
C# Inner surface s3 type: clear glass
C# Interior surface s4 type: clear glass
C# Exterior normal hemispherical reflectance: 0.331951826
C# Interior normal hemispherical reflectance: 0.130844771
C# Normal hemispherical transmittance: 0.40798918
C#
C void BRTDfunc SYP_YRE0146
C 10
C if(Rdot,cr(fr(0.074),ft(0.862),fr(0.087)),cr(fr(0.291),ft(0.419),fr(0.074)))
C if(Rdot,cr(fr(0.077),ft(0.890),fr(0.057)),cr(fr(0.320),ft(0.480),fr(0.077)))
C if(Rdot,cr(fr(0.079),ft(0.886),fr(0.135)),cr(fr(0.371),ft(0.458),fr(0.079)))
C ft(0.862)*ft(0.419)
C ft(0.890)*ft(0.480)
C ft(0.886)*ft(0.458)
C        0 0 0
C        glaze2.cal
C 0
C 9
C        0 0 0
C        0 0 0
C        0 0 0

C      e2rform.F, line 596 ff.:
c      WRITE(iglzfil,'(a)') 'void  glass  unknown_glz'
c      WRITE(iglzfil,'(a)') '0 '
c      WRITE(iglzfil,'(a)') '0 '
c      WRITE(iglzfil,'(a)') '3    0.87    0.87    0.87'
c      WRITE(iglzfil,'(a)') '  '


C void alias ADIC_CFC SYP_YRE0146

          endif ! itype
100     continue ! CFC layer loop

      else ! not init
        ! ??? what here? 
      endif ! init

      return
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ******************** Copy of MZSCAI ********************
C MZSCAI computes the cosine of the solar angle of incidence.
      SUBROUTINE CFCMZSCAI(ICOMP,II,CAI,ANGI)
      implicit none
#include "building.h"

C Parameters
      integer ICOMP,II
      real CAI,ANGI

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      real PAZI,PELV

      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      real sazi,salt
      integer isunup

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      
      real PI,R,PSAZI

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C Surface-solar azimuth angle for this external construction.
      PSAZI=ABS(PAZI(ICOMP,II)-SAZI)

C Angle of incidence and its cosine (Iain Macdonald Feb 2006).
      CAI=COS(SALT*R)*SIN((90.-PELV(ICOMP,II))*R)*COS(PSAZI*R)+
     &    SIN(SALT*R)*COS((90.-PELV(ICOMP,II))*R)

      IF(CAI.LT.0.) CAI=0.

      ANGI=ACOS(CAI)/R
C Establish angle of incidence index (IND1) for
C glazing property interpolation purposes; where


C Test if standard optical data or bidirectional data.
c      IF(NSTMCFL(IZSTOCN(ICOMP,II)).EQ.0) THEN
        IF(ANGI.GT.-0.01.AND.ANGI.LT.0.0)ANGI=0.0
        IF(ANGI.GT.90.0.AND.ANGI.LT.90.01)ANGI=90.0
        IF(ANGI.LT.0..OR.ANGI.GT.90.)goto 9999
c      ELSE

c      ENDIF

 9999 RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ***************** Copy of MZSFSH ********************
C MZSFSH determines if an external surface faces 'away' from
C the sun so causing self-shading.

C IANS=0 ; no self-shading
C IANS=1 ; self-shading occurs

      SUBROUTINE CFCMZSFSH(ICOMP,IS,IANS)
      implicit none
#include "building.h"
      integer icomp,is,ians

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      real PAZI,PELV
      
      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      real sazi,salt
      integer isunup

      real PI,R,X1,X2,X3,X4,X5,CAI,PSAZI

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C Determine the surface-solar azimuth angle.
      PSAZI=ABS(PAZI(ICOMP,IS)-SAZI)

C Cosine of angle of incidence.
      X1=COS(SALT*R)
      X2=SIN((90.-PELV(ICOMP,IS))*R)
      X3=COS(PSAZI*R)
      X4=SIN(SALT*R)
      X5=COS((90.-PELV(ICOMP,IS))*R)
      CAI=X1*X2*X3+X4*X5

C Surface is self-shaded if CAI is less than or
C equal to zero.
      IANS=0
      IF(CAI.LE.0.)IANS=1
      RETURN
      END


