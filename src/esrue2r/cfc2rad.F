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

C it contains the following routines, copyright Achim Geissler:
C
C      CFC2rad  Writes Radiance files for CFC constructions. This
C               subroutine expects that CFC commons are populated!
C               File names / handles are used via e2r_common.h


C ********************************************************************
C                         -- CFC2rad --
C
C Create Radiance definitions for CFC surfaces. This routine is
C designed to be called in e2r.
C
C May 13: Called from mkriofil(), line 833.
C 
C Created by: Achim Geissler
C Initial Creation Date: May 2012
C 
C Call parameters
C   init       toggle for first call init=1 and subsequent 
C              calls (slat angle control) init=0
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
C   None (?)   >> writes Radiance definitions to file 'icfcfil'
C
C ********************************************************************
      subroutine CFC2rad(init,icomp,isurf,ExplicitVB,BRTDfunc)
c      USE h3kmodule ! ? necessary??

      IMPLICIT NONE
#include "building.h"
#include "geometry.h"
#include "CFC_common.h"
#include "e2r_common.h"

C Parameters
      integer iua,init,icomp,isurf
      logical ExplicitVB,BRTDfunc

C Commons
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C Local variables
      real r,PI,incang,srefl
      real sw,sp,phi,crown,wr,sth,tilt
      real x0,y0,z0,cfc_width,cfc_height
      real dy,vb_width,vb_height
      real VTDIR,VTDIF,VTRAN,gvt1,gvt

      integer ne,i,itype,nslats,icfctp,icn,vb_lay

      character matname*72,cmdstr*144,transfstr*144,glaznam*72

C *** from solar.F ***
C Local variables for CFCs
      real proang
      real CFCabs
      real TRANSBB_SYSV,TRANSD_SYSV,REFL_SYSV,CFCabsV
      real TRANSD_SYS_BD,TRANSD_SYS_SKY,TRANSD_SYS_GRD
      real dummy1,dummy2
      real tmpIBM,tmpIDF,EXDIR,EXDIF,EXDIFSKY,EXDIFGRD
      real AbsSolDir,AbsSolDifGrd,AbsSolDifSky,AbsVis

      character ctmp*2

      integer iSolVis ! switch for CFC solar / visual data calculation
      integer i_direct ! switch for calling solar_multilayer

      DIMENSION AbsSolDir(me),AbsSolDifGrd(me)
      DIMENSION AbsSolDifSky(me),AbsVis(me)

C ********************

C Function declarations
      integer lnblnk

C Check if combination of logical parameters is valid
      if (BRTDfunc) then
        if (.not.ExplicitVB) then
        ! not possible!
          call usrmsg(' BRTDfunc must use explicit blind!',
     &                ' Aborting!','W')
          return
        endif
      endif

cx      write(*,*)' ** CFC2rad ** '
C Misc preliminary calcs
      PI = 4.0 * ATAN(1.0)
      r=PI/180.

      icfctp=icfcfl(icomp,isurf) ! get type
      if (icfctp.ge.1) then ! we have a CFC
cx        write(*,*)'CFC type ',icfctp

        if (BRTDfunc) then !

          ne=ncfc_el(icomp,icfctp) ! number of cfc layers for this type

C         Loop through CFC layers and generate ... 

          do 100 i=1,ne
            itype=icfcltp(icomp,icfctp,i)
 
            if (itype.eq.1) then  ! glass layer
              if (init.eq.1) then ! initialization, build model
                write(*,*)' ** not implemented! **'
                return
C               ----------------------------------------------------------
C               Create BRTDfunc from glazing layer properties ...
C               ----------------------------------------------------------

C               ... the BRTD-generation requires
C               knowledge of the individual glass layers 
C               - set up to use IGDB index?
C               - or "file" name (product / coating name)?

C               Step 1:
c                 call glaze with layer buildup acc. to CFC description
c                 (without (!) blind)

C             Step 2:
c               append output of glaze to materials file from e2r

C             Step 3:
c               ??

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

              else ! not init
                ! ??? what here? 
                ! Actually, for "not BRTDfunc", the above material would need to be
                ! replaced / updated if blind setting has changed ...
              endif ! init for itype glass
            endif ! itype
100       continue ! CFC layer loop

        else ! use 'glass', not BRTDfunc
          if (ExplicitVB) then
            i_shd(icomp,icfctp)=0 ! glazing data is w/o blind
          endif

cx          write(*,*)'CFC type ',icfctp,', vb on/off: ',
cx     &              i_shd(icomp,icfctp),', vb angle: ',
cx     &              vb_phiNew(icomp,icfctp)

C ******************* from solar.F ********************************
C The following code should probably be put in a designated subroutine.
C Values for radiation and solar angle should only be set for
C non-conflation type calls ("visualization" calls).
          tmpIDF=0.0   ! interior diffuse source
          tmpIBM=0.0   ! interior beam source
          i_direct=0   ! toggle for ... ?

          EXDIR=10.    ! does this matter for calculated values?
          EXDIF=10.    ! A better approach would be to use 
          EXDIFSKY=5.  ! some kind of realistic values based on
          EXDIFGRD=5.  ! type of simulation. For conflation, the
                       ! current actual values, of course.
          incang=55. ! this should be calculated acc. to actual
                     ! situation ...

          iSolVis=1  ! calculate visual properties

          ! Determine values corresponding to current incidence 
          ! angle (??) on surface, values include (optional) blinds 
          ! at current blind angle
          call profile_angle(icomp,isurf,icfctp,proang)

          call cfc_eff_opt_prop(icomp,isurf,icfctp,incang,proang,
     &                                                     iSolVis)

C            Check whether sky and ground diffuse calc. is requested
             if (i_ground_sky_diff_calc.eq.1) then

C              Calculate CFC system visual BEAM transmitted
               call solar_multilayer(i_direct,icomp,isurf,icfctp,
     &                   EXDIR,tmpIDF,
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   TRANSBB_SYSV,TRANSD_SYS_BD,dummy1,AbsSolDir)

C              Calculate CFC system solar SKY diffuse transmitted
               call solar_multilayer(i_sky,icomp,isurf,icfctp,
     &                   tmpIBM,EXDIFSKY,
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   dummy1,TRANSD_SYS_SKY,dummy2,AbsSolDifSky)

C              Calculate CFC system solar GROUND diffuse transmitted
               call solar_multilayer(i_ground,icomp,isurf,icfctp,
     &                   tmpIBM,EXDIFGRD,
     &                   tmpIBM,tmpIDF,iSolVis,
     &                   dummy1,TRANSD_SYS_GRD,dummy2,AbsSolDifGrd)

C              Sum up diffuse transmitted contributions (sky + ground)
               TRANSD_SYSV = TRANSD_SYS_BD +
     &                       TRANSD_SYS_SKY + TRANSD_SYS_GRD 
C              Absorbed visual is of no interest

             else 
C             Legacy model.
C             Calculate CFC system visual beam transmitted, diffuse transmitted.
              call solar_multilayer(i_direct,icomp,isurf,icfctp,
     &           EXDIR,EXDIF,
     &           tmpIBM,tmpIDF,iSolVis,TRANSBB_SYSV,TRANSD_SYSV,
     &           REFL_SYSV,AbsVis)
             endif

C visual data, normalise to radiation available for output/storing of 
C fraction. These fractions are used in "casual.F/DWINVT()" as light 
C transmittance values (direct/diffuse) - the fraction is based on 
C solar radiation with visual properties for CFC layers. 
            if (EXDIR.gt.0.) then
              transBB_V(icomp,isurf)=TRANSBB_SYSV/EXDIR
            else
              transBB_V(icomp,isurf)=0.
            endif
            if (i_ground_sky_diff_calc.eq.1) then
              if ((EXDIR+EXDIFSKY+EXDIFGRD).gt.0.) then
                transD_V(icomp,isurf)=TRANSD_SYSV/(EXDIR+EXDIFSKY
     &                                     +EXDIFGRD)
              else
                 transD_V(icomp,isurf)=0.
              endif
            else ! legacy
              if ((EXDIR+EXDIF).gt.0.) then
                 transD_V(icomp,isurf)=TRANSD_SYSV/(EXDIR+EXDIF)
              else
                 transD_V(icomp,isurf)=0.
              endif
             
            endif


C ******************************************************************

          VTDIR=transBB_V(icomp,isurf) 
          VTDIF=transD_V(icomp,isurf)
          VTRAN=VTDIR+VTDIF ! not really good ...
          if((VTRAN).gt.0.02)then
            gvt1=sqrt(0.8402528435+(0.007252224*VTRAN*VTRAN))
            gvt=(gvt1-0.9166530661)/0.0036261119/VTRAN
          else
            gvt1=0.02
            gvt=0.02
          endif
C         Write glazing material to file. Use construction name
C         extended with surface name and zone number (the same 
C         construction can be used by different CFC layer setups 
C         / blind settings(!) spread over more than one zone
          icn=IZSTOCN(icomp,isurf)
          write(glaznam,'(4a,I1)')
     &                    SSMLCN(icn)(1:lnblnk(SSMLCN(icn))),'_',
     &                    SSNAME(icn)(1:lnblnk(SSNAME(icn))),'_',
     &                    icomp
          WRITE(icfcfil,'(a,a)') 'void glass ',
     &                 glaznam(1:lnblnk(glaznam))
          WRITE(icfcfil,'(a)') '0 '
          WRITE(icfcfil,'(a)') '0 '
          WRITE(icfcfil,'(a,3F6.2)') '3  ',gvt,gvt,gvt
          WRITE(icfcfil,'(a)') '  '

        endif ! BRTDfunc ?

C Glass data is set and Radiance data written. Now the blind
C is generated, if "Explicit".

        if (ExplicitVB) then
          write(*,*)' ** not implemented (2)! **'

C       get layer number in CFC 
          ne=ncfc_el(icomp,icfctp) ! number of cfc layers for this type
          do 110 i=1,ne
            itype=icfcltp(icomp,icfctp,i)
            if (itype.eq.2) vb_lay=i
  110     continue

          if (vb_lay.ne.1) then
C           If it is not "1", then it is an
C           intermediate blind ... do what? "Not possible"? (!!)
       write(*,*)' Invalid blind position for Radiance coupling',
     &           ' in explicit VB mode!'
cx            return
          endif

C----------------------------------------------------------------
C         Initialization?
C----------------------------------------------------------------
          if (init.eq.1) then    ! initialization, once-off steps
            ! Generate slat material description and append to 
            ! materials file 

C           Check for transmission of slat material, if > zero,
C           use "trans" material. Otherwise "plastic"
            icn=IZSTOCN(icomp,isurf)
            WRITE(matname,'(5a,I1)') 'vb_',
     &                    SSMLCN(icn)(1:lnblnk(SSMLCN(icn))),'_',
     &                    SSNAME(icn)(1:lnblnk(SSNAME(icn))),'_',
     &                    icomp

            if (visT(icomp,icfctp,vb_lay).gt.0.0) then
            ! material 'trans'
              WRITE(imatfil,'(2a)') 
     &             'void trans ',matname(1:lnblnk(matname))
              WRITE(imatfil,'(a)') '0 '
              WRITE(imatfil,'(a)') '0 '
              srefl=visRf(icomp,icfctp,vb_lay)
C t_red t_green t_blue specularity roughness transmissivity transmitted_specular
              WRITE(imatfil,'(a,3F5.2,a,F5.2,a)')
     &              '7 ',srefl,srefl,srefl,' 0.01 0.10 ',
     &              visT(icomp,icfctp,vb_lay),'  0.97'
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

          endif ! init for blind

C -------------------------------------------------------------------
C     Get lower left corner of CFC surface and surface width
C     The Venetian type blind will be created wider by 2 x dy, so that 
C     it overlaps the glazing area. Typically such an overlap will be
C     in the range of 30 to 50 millimeters either side.
C -------------------------------------------------------------------
          dy=0.05 ! set fixed value, for now
          
          x0=0.08
          y0=0.
 
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
          sw=vb_w(icomp,icfctp)/1000.    ! slat width in [m]
          sp=vb_s(icomp,icfctp)/1000.    ! slat pitch
          phi=vb_phiNew(icomp,icfctp)    ! blind slat angle
          crown=vb_crown(icomp,icfctp)   ! crown height [mm]
c           wr=vb_wr(icomp,icfctp)         ! width/curvature ratio
c           sth=vb_t(icomp,icfctp)         ! max slat thickness
cx           tilt=PELV(icomp,isurf)         ! tilt of cfc surface

C -------------------------------------------------------------------
C         Create Venetian type blind description (genblinds) with 
C         current slat angle
C -------------------------------------------------------------------

C "blinds_30.norm" from model ADIC/Elev1:
C!genblinds Duraskin blind 1 32.165 44 49 -30 +r 2.5 | xform -s 0.08 -rz -90 -rz  4.118 -t  0.000  -16.408  0.06

C the "scale" used here is normalized to slat width (parameter 
C "depth" in call to genblinds). All other numbers given
C for the dimension should be calculated accordingly.
C genblinds mat      name depth width height nslats angle [ -r|+r rcurv ]
C!genblinds <matname> blind 1    32.165  44     49 -<<phi>> +r <<crown>> | 

C lower inside left corner (viewed from outside) is 0,0,0 (?!) 
C       -s is the scale, enter slat width in [m] here!
C               -rotation z, always necessary
C                       -rotation z should be actual surface azimuth (psazi?)
C                                  -translation to lower left coordinate of 
C                                   CFC surface, lift by a small amount (?)
C xform -s 0.08 -rz -90 -rz  4.118 -t  0.000  -16.408  0.06
C
C - x and y seem to be switched in Radiance ... (needs checking!)
C - for some reason, there is a large gap between the split vb-s
C   of the test model ... (needs checking!)
C - file handling for non-init calls, i.e. when called on time step basis??

            ! calculate number of slats necessary to cover height
            nslats=ceiling(vb_height/sp) !+1
            ! rescale Venetian blind width and height to multiples of
            ! slat width
            vb_width=vb_width/sw
            vb_height=vb_height/sw

            WRITE(cmdstr,
     &             '(3a,I1,a,I1,a,F7.2,a,F6.2,a,I3,a,F5.1,a,F5.3)') 
     &             '!genblinds ',matname(1:lnblnk(matname)),
     &             ' blind_',icfctp,' ',1,' ',
     &             vb_width,' ',vb_height,' ',nslats,' ',phi,
     &             ' +r ',crown

            WRITE(transfstr,
     &             '(a,F7.4,a,3(F6.3,1x))')
     &             'xform -s ',sw,' -rz 90 -t ',
     &             cfc_width+2*dy,-sw,z0 !+sp/2.
               ! use esp_y for lower right corner, here ("x")?

            WRITE(icfcfil,'(a)') ' '
            WRITE(icfcfil,'(a)') '# generate blinds and transfer:'
            WRITE(icfcfil,'(3a)') cmdstr(1:lnblnk(cmdstr)),' | ',
     &                            transfstr(1:lnblnk(transfstr))
            WRITE(icfcfil,'(a)') '  '

        endif ! ExplicitVB
      endif ! CFC type >= 1 (i.e. it is actually a CFC)

      return
      end

