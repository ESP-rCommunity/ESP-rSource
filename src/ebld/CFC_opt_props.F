C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2012.
    
C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

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
C Created by: Bart Lomanowski
C Initial Creation Date: April 2007
C
C Main CFC Reference:
C     Lomanowski, B.A. (2008) 'Implementation of Window Shading Models
C     into Dynamic Whole-Building Simulation', MASc Thesis, University
C     of Waterloo.
C     Available online:
C     http://uwspace.uwaterloo.ca/bitstream/10012/4164/1/Lomanowski_Bartosz.pdf
C
C     
C Reference for implementation of roller blinds, pleated drapes and 
C insect screens:
C     Joong, K. (2011) ' Implementation of Roller Blind, Pleated Drape and
C     Insect Screen Models into the CFC Module of the 
C     ESP-r Building Energy Simulation Tool', MASc Thesis, University
C     of Waterloo.
C     Available online:
C     http://uwspace.uwaterloo.ca/bitstream/10012/6199/1/Joong_Kenneth.pdf
C
C ********************************************************************

C This file contains the subroutine
C
C       cfc_eff_opt_prop
C
C which calculates solar (and visual) optical properties and all
C necessary subroutines called:
C
C       vb_eff_diff_properties
C       SOLMATS
C       VB_SOL46_CURVE
C       VB_SOL46_THICK
C       VB_SOL_DD_THICK
C       VB_SOL4
C       VB_SOL6
C       IS_DIFF
C       IS_F
C       IS_BEAM
C       IS_OPENNESS
C       IS_DSRATIO
C       RB_DIFF
C       RB_BEAM
C       RB_F
C       FM_F
C       FM_DIFF
C       FM_BEAM
C       PD_DIFF
C       PD_BEAM
C       PD_BEAM_CASE_I
C       PD_BEAM_CASE_II
C       PD_BEAM_CASE_III
C       PD_BEAM_CASE_IV
C       PD_BEAM_CASE_V
C       PD_BEAM_CASE_VI
C       HEMINT

C ********************************************************************
C                         --cfc_eff_opt_prop--
C
C Determine solar optical properties of CFC glazing/shading layers
C at each time-step. 
C 
C Glazing layers (CFC layer type 1) are dependent on incidence angle. 
C Off normal property adjustment based on clear or bronze reference
C glass. See Appendix B in:
C
C References:
C     Lomanowski, B.A. (2008) 'Implementation of Window Shading Models
C     into Dynamic Whole-Building Simulation', MASc Thesis, University
C     of Waterloo.
C
C     Joong, K. (2011) ' Implementation of Roller Blind, Pleated Drape and
C     Insect Screen Models into the CFC Module of the 
C     ESP-r Building Energy Simulation Tool', MASc Thesis, University
C     of Waterloo.
C
C Slat-type blind layers (CFC layer type 2) are dependent on profile
C angle. Effective slat-blind solar optical properties are determined
C for both curved slats and flat slats with thickness correction. 
C Refer to Chapter 4 in B.Lomanowski MASc Thesis (2008) for model
C details. 
C
C **STILL NEED VISIBLE PROPERTY CALCULATION
C
C ********************************************************************
      subroutine cfc_eff_opt_prop(icomp,isurf,icfctp,anginc,
     &      omega_v_in,omega_h_in)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer iuout,iuin,ieout

C need thickness of glass layers for Fresnel calcs
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      real thrmli

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      integer ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow

      common/CFCsolarS/RadIncDirS(mcom,ms),RadIncDifS(mcom,ms),
     &      RadIncDifSky(mcom,ms),RadIncDifGrd(mcom,ms),
     &      transBB_S(mcom,ms),transD_S(mcom,ms),refl_S(mcom,ms),
     &      issur_self_shd(mcom,ms),proang_s(mcom,mcfc,ms),
     &      qtmca_ext(mcom,ms,mn),omega_v_s(mcom,mcfc,ms),
     &      omega_h_s(mcom,mcfc,ms)
      real RadIncDirS,RadIncDifS,RadIncDifSky,RadIncDifGrd
      real transBB_S,transD_S,refl_S
      real proang_s,qtmca_ext,omega_v_s,omega_h_s
      integer issur_self_shd

      integer icomp,isurf,icfctp

C TODO: DEFINE THESE VARIABLES    

      real omega_v_in, omega_h_in
      real vb_proang,anginc,OMEGA_V,OMEGA_H,OHM_V_RAD,OHM_H_RAD

      real pi,r
      real w,s,phi,crown,wr,t,Fr !local slat width, spacing, crown, w/r ratio, thickness, fullness ratio
      real sl_rad_overw
      real openness

C variables for calculation of sky and ground diffuse transmittance
      real element_proang_sky ! profile angle of sky element
      real element_proang_grd ! profile angle of ground element
      integer nelements       ! number of integration slices
      real SolTFskydiff       ! front sky diffuse transmittance (slat blinds with horizontal slats)
      real SolRFskydiff       ! front sky diffuse reflectance (slat blinds with horizontal slats)
      real SolTFgrddiff       ! front ground diffuse transmittance (slat blinds with horizontal slats)
      real SolRFgrddiff       ! front ground diffuse reflectance (slat blinds with horizontal slats)
      real SolTBskydiff       ! back sky diffuse transmittance (slat blinds with horizontal slats)
      real SolRBskydiff       ! back sky diffuse reflectance (slat blinds with horizontal slats)
      real SolTBgrddiff       ! back ground diffuse transmittance (slat blinds with horizontal slats)
      real SolRBgrddiff       ! back ground diffuse reflectance (slat blinds with horizontal slats)

      integer iter,j ,jj      ! iterators

C temporary INPUT variables to hold solar and visible properties as
C read from *.cfc file
      real SolRufs,SolRdfs,SolTslat!,VisRufs,VisRdfs,VisTslat
      real SolRB_drp, SolRF_drp, SolT_drp
      real SolTf_tot_drp, SolTb_tot_drp, SolTf_bd_drp, SolTb_bd_drp
      
C temporary INPUT variables to hold effective front solar optical properties of drape fabrics      
      real SolRF_mat, SolTf_tot_mat, SolT_mat, SolTf_bd_mat
        
C temporary INPUT variables to hold effective back solar optical properties of drape fabrics      
      real SolRB_mat, SolTb_tot_mat, SolTb_bd_mat

C temporary OUTPUT variables to hold effective front and back solar optical properties of drape fabrics      
      real SolRf_dd_mat, SolTf_dd_mat, SolRb_dd_mat, SolTb_dd_mat
      real OutTAUFF_DD, OutTAUBF_DD, OutRHOFF_DD, OutRHOBF_DD  

C temporary OUTPUT variables to hold effective solar and visible properties of shading layer
      real OutRFbd,OutTFbb,OutTFbd,OutRBbd,OutTBbb,OutTBbd
      real OutRFdd,OutRBdd,OutTFdd,OutTBdd
      
C clear glass Fresnel calcs
      real N2,KL,THETA1,THETA2,TAU_A,RPERP,RPARL
      real TAUPERP,TAUPARL,RHOPERP,RHOPARL
      real RclearON,RclearDN,TclearON,TclearDN 
      CHARACTER outs*124

      REAL SMALL_ERROR, PIOVER2, DTOR, RTOD
      PARAMETER (PI = 4.0 * ATAN(1.0), SMALL_ERROR = 0.000001)
      PARAMETER (PIOVER2 = PI/2., DTOR = PI / 180., RTOD = 180./ PI)
      
      LOGICAL DODIFFUSE

C misc local variables
      integer ne,i,itype

      ne=ncfc_el(icomp,icfctp)

C convert profile and incidence angles to radians

      anginc=anginc*DTOR
     
C TODO: Updte add_to_xml_reporting calls to new format
c      call add_to_xml_reporting (
c     &         anginc/DTOR,
c     &         'building/zone_01/envelope/windows/INCI_ANGLE',
c     &         'units', '(rad)',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
     
      OHM_V_RAD = omega_v_in * DTOR
      OHM_H_RAD = omega_h_in * DTOR
      
c      call add_to_xml_reporting (
c     &         OHM_V_RAD,
c     &         'building/zone_01/envelope/windows/OHM_V',
c     &         'units', '(rad)',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
     
c      call add_to_xml_reporting (
c     &         OHM_H_RAD,
c     &         'building/zone_01/envelope/windows/OHM_H',
c     &         'units', '(rad)',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )  
   
C Loop through CFC layer types and compute solar optical properties for 
C each layer, set air gap properties to 0 except for bb and dd transmission,
C set these to 1.
        
      do 100 i=1,ne
        itype=icfcltp(icomp,icfctp,i)
         
C......Layer type is GLAZING
       if(itype.eq.iGlazing)then

C.........Establish extinction coefficient (KL) and refractive index (N2).
C.........IF normal solar transmittance >0.645, use clear reference glass
C.........IF normal solar transmittance <= 0.645, use bronze reference glass
          if(SolT(icomp,icfctp,i).gt.0.645)then
            N2 = 1.526
            KL = 25.7*THRMLI(icomp,isurf,i,4) ![thickness in m]
          else
            N2 = 1.7
            KL = 100.6*THRMLI(icomp,isurf,i,4) ![thickness in m]
          end if

C.........Calculate beam DIRECT NORMAL incidence transmittance and reflectance
C.........for reference glass based on extinction coefficient and refractive
C.........index.
          TAU_A = EXP(-1.*KL)
          RPERP=((N2-1.)/(N2+1.))**2
          TclearDN=TAU_A*(1.-RPERP)*(1.-RPERP)
     &      /(1.-(RPERP*RPERP*TAU_A*TAU_A))
          RclearDN=RPERP*(1.+(TAU_A*TclearDN))

          THETA1=ABS(anginc)
          IF (THETA1.LT.0.1)THEN
            TclearON=TclearDN
            RclearON=RclearDN
          ELSE
C...........Calculate beam OFF-NORMAL incidence transmittance and reflectance for
C...........reference glass based on extinction coefficient and refractive index 
C...........using Snell's law, Fresnel equations.
            THETA2 = ASIN((SIN(THETA1))/N2)
            TAU_A = EXP(-1.0*KL/COS(THETA2))
            RPERP = ((SIN(THETA2-THETA1))/(SIN(THETA2+THETA1)))**2
            RPARL = ((TAN(THETA2-THETA1))/(TAN(THETA2+THETA1)))**2
            TAUPERP = (TAU_A*(1.0-RPERP)*(1.0-RPERP))/
     &            (1.0-(RPERP*RPERP*TAU_A*TAU_A))
            TAUPARL = (TAU_A*(1.0-RPARL)*(1.0-RPARL))/
     &            (1.0-(RPARL*RPARL*TAU_A*TAU_A))
            RHOPERP = RPERP*(1.0+(TAU_A*TAUPERP))
            RHOPARL = RPARL*(1.0+(TAU_A*TAUPARL))
            TclearON = (TAUPERP+TAUPARL)/2.0
            RclearON = (RHOPERP+RHOPARL)/2.0
          END IF

C.........Calculate glazing layer beam OFF-NORMAL front and back transmittance
C.........and reflectance based on clear glass off-normal and normal values.
          SolTFbb(icomp,isurf,icfctp,i)=
     &      (TclearON/TclearDN)*SolT(icomp,icfctp,i)

          SolTBbb(icomp,isurf,icfctp,i)=SolTFbb(icomp,isurf,icfctp,i)

          SolRFbb(icomp,isurf,icfctp,i)=1.-
     &      (((1.-RclearON)/(1.-RclearDN))*(1.-SolRF(icomp,icfctp,i)))

          SolRBbb(icomp,isurf,icfctp,i)=1.-
     &      (((1.-RclearON)/(1.-RclearDN))*(1.-SolRB(icomp,icfctp,i)))

C.........Calculate DIFFUSE-DIFFUSE incidence transmittance and reflectance
C.........for glass template based on extinction coefficient and refractive
C.........index.
          THETA1=51.* DTOR
          THETA2 = ASIN((SIN(THETA1))/N2)
          TAU_A = EXP(-1.0*KL/COS(THETA2))
          RPERP = ((SIN(THETA2-THETA1))/(SIN(THETA2+THETA1)))**2
          RPARL = ((TAN(THETA2-THETA1))/(TAN(THETA2+THETA1)))**2
          TAUPERP = (TAU_A*(1.0-RPERP)*(1.0-RPERP))/
     &            (1.0-(RPERP*RPERP*TAU_A*TAU_A))
          TAUPARL = (TAU_A*(1.0-RPARL)*(1.0-RPARL))/
     &            (1.0-(RPARL*RPARL*TAU_A*TAU_A))
          RHOPERP = RPERP*(1.0+(TAU_A*TAUPERP))
          RHOPARL = RPARL*(1.0+(TAU_A*TAUPARL))
          TclearON = (TAUPERP+TAUPARL)/2.0
          RclearON = (RHOPERP+RHOPARL)/2.0

C.........Calculate diffuse-diffuse for given glass layer
          SolTFdd(icomp,isurf,icfctp,i)=
     &      (TclearON/TclearDN)*SolT(icomp,icfctp,i)

          SolTBdd(icomp,isurf,icfctp,i)=SolTFdd(icomp,isurf,icfctp,i)

          SolRFdd(icomp,isurf,icfctp,i)=1.-
     &      (((1.-RclearON)/(1.-RclearDN))*(1.-SolRF(icomp,icfctp,i)))

          SolRBdd(icomp,isurf,icfctp,i)=1.-
     &      (((1.-RclearON)/(1.-RclearDN))*(1.-SolRB(icomp,icfctp,i)))

C.........Set beam-diffuse eq. 0
          SolTFbd(icomp,isurf,icfctp,i)=0.0
          SolTBbd(icomp,isurf,icfctp,i)=0.0
          SolRFbd(icomp,isurf,icfctp,i)=0.0
          SolRBbd(icomp,isurf,icfctp,i)=0.0
!         VisTFbd(icomp,isurf,icfctp,i)=0.0
!         VisTBbd(icomp,isurf,icfctp,i)=0.0
!         VisRFbd(icomp,isurf,icfctp,i)=0.0
!         VisRBbd(icomp,isurf,icfctp,i)=0.0

C......Layer type is VENETIAN BLIND
       elseif(itype.eq.iVenBlind)then

        IF(vb_VorH(icomp,icfctp).eq.'HORZ')then

           vb_proang=OHM_V_RAD
      
         ELSEIF(vb_VorH(icomp,icfctp).eq.'VERT')then

           vb_proang=OHM_H_RAD

         ELSE
           vb_proang=90.
         
        ENDIF
         
        proang_s(icomp,icfctp,isurf) = vb_proang
         
C.........If shade is retracted set solar transmittance to 1 and skip to next layer
          if(i_shd(icomp,icfctp).eq.0)then
            SolTFbb(icomp,isurf,icfctp,i)=0.999
            SolRFbd(icomp,isurf,icfctp,i)=0.001
            SolTFbd(icomp,isurf,icfctp,i)=0.001
            SolTFdd(icomp,isurf,icfctp,i)=0.999
            SolRFdd(icomp,isurf,icfctp,i)=0.001
            SolTBbb(icomp,isurf,icfctp,i)=0.999
            SolRBbd(icomp,isurf,icfctp,i)=0.001
            SolTBbd(icomp,isurf,icfctp,i)=0.001
            SolTBdd(icomp,isurf,icfctp,i)=0.999
            SolRBdd(icomp,isurf,icfctp,i)=0.001

            ! also set the sky and ground diffuse properties
            ! if the sky/ground calc has been requested. 
            if (i_ground_sky_diff_calc.eq.1) then
              SolTFskydd(icomp,isurf,icfctp,i)=0.999
              SolRFskydd(icomp,isurf,icfctp,i)=0.001
              SolTFgrddd(icomp,isurf,icfctp,i)=0.999
              SolRFgrddd(icomp,isurf,icfctp,i)=0.001

              SolTBskydd(icomp,isurf,icfctp,i)=0.999
              SolRBskydd(icomp,isurf,icfctp,i)=0.001
              SolTBgrddd(icomp,isurf,icfctp,i)=0.999
              SolRBgrddd(icomp,isurf,icfctp,i)=0.001
            end if

            goto 100
          endif
        
C.........set beam-beam reflectances to zero
          SolRFbb(icomp,isurf,icfctp,i)=0.0
          SolRBbb(icomp,isurf,icfctp,i)=0.0
!         VisRFbb(icomp,isurf,icfctp,i)=0.0
!         VisRBbb(icomp,isurf,icfctp,i)=0.0

C.........assign direct normal solar/visible properties to local variables
          SolRufs=solRF(icomp,icfctp,i)
          SolRdfs=solRB(icomp,icfctp,i)
          SolTslat=solT(icomp,icfctp,i)
!         VisRdfs=visRF(icomp,icfctp,i)
!         VisRufs=visRB(icomp,icfctp,i)
!         VisTslat=visT(icomp,icfctp,i)

C.........assign slat blind descriptors to local variables
          w=vb_w(icomp,icfctp)
          s=vb_s(icomp,icfctp)
          phi=vb_phiNew(icomp,icfctp)*DTOR
          crown=vb_crown(icomp,icfctp)
          wr=vb_wr(icomp,icfctp)
          t=vb_t(icomp,icfctp)

          if(crown.gt.t)then !slat is CURVED

            sl_rad_overw=1./wr

C...........calculate front effective solar optical properties and assign to commons
            call VB_SOL46_CURVE(s,w,sl_rad_overw,phi,vb_proang,
     &            SolRdfs,SolRufs,SolTslat,OutRFbd,OutTFbb,OutTFbd)
            SolTFbb(icomp,isurf,icfctp,i)=OutTFbb
            SolRFbd(icomp,isurf,icfctp,i)=OutRFbd
            SolTFbd(icomp,isurf,icfctp,i)=OutTFbd

C...........calculate front effective visible properties and assign to commons
!           call VB_SOL46_CURVE(s,w,sl_rad_overw,phi,vb_proang,
!      &            VisRdfs,VisRufs,VisTslat,OutRFbd,OutTFbb,OutTFbd)
!           VisTFbb(icomp,isurf,icfctp,i)=OutTFbb
!           VisRFbd(icomp,isurf,icfctp,i)=OutRFbd
!           VisTFbd(icomp,isurf,icfctp,i)=OutTFbd

C...........CALCULATE GROUND AND SKY DIFFUSE COMPONENTS (FRONT SURFACE)
C...........Check whether calculation of ground and sky diffuse
C...........components has been requested. 
            
            if (i_ground_sky_diff_calc.eq.1) then

              nelements    = 9   ! number of integration slices (10 deg increments)
              SolTFskydiff = 0.0 ! reset the sky diffuse transmittance
              SolRFskydiff = 0.0 ! reset the sky diffuse reflectance  
              SolTFgrddiff = 0.0 ! reset the ground diffuse transmittance
              SolRFgrddiff = 0.0 ! reset the ground diffuse reflectance            
            
C.............Perform integration of sky elements based on the midpoint rule:
C.............integral(a to b) f(x)dx = Sum(1 to n) f(a + (i-1/2)(b-a)/n) * ((b-a)/n)
              integrate_front_elements_CURVED: do iter = 1, nelements
      
                element_proang_sky = (90./float(nelements)) * 
     &                               (float(iter)-0.5) * 1.
 
                element_proang_sky = element_proang_sky * DTOR       ! convert to radians

                element_proang_grd = element_proang_sky * (-1.0)    ! profile angle for ground

C...............SKY CALCULATION
C...............calculate the beam-beam and beam-diffuse transmission
C...............at the current sky element profile angle
                call  VB_SOL46_CURVE(s,w,t,phi,
     &                              element_proang_sky,           ! profile angle of sky element
     &                              SolRdfs,SolRufs,SolTslat,     ! slat properties input
     &                              OutRFbd,OutTFbb,OutTFbd)      ! overall effective properties output

C...............Sum up each sky element's contribution to the sky
C...............diffuse transmittance and reflectance.
                SolTFskydiff = SolTFskydiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutTFbb + OutTFbd) * 
     &                     cos(element_proang_sky))         ! f(a + (i-1/2)(b-a)/n)
                SolRFskydiff = SolRFskydiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutRFbd) * cos(element_proang_sky))  ! f(a + (i-1/2)(b-a)/n)
C...............END SKY CALCULATION

C...............GROUND CALCULATION
C...............calculate the beam-beam and beam-diffuse transmission
C...............at the current ground element profile angle
                call  VB_SOL46_CURVE(s,w,t,phi,
     &                              element_proang_grd,           ! profile angle of sky element
     &                              SolRdfs,SolRufs,SolTslat,     ! slat properties input
     &                              OutRFbd,OutTFbb,OutTFbd)      ! overall effective properties output

C...............Sum up each sky element's contribution to the sky
C...............diffuse transmittance and reflectance.
                SolTFgrddiff = SolTFgrddiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutTFbb + OutTFbd) * 
     &                      cos(element_proang_grd))         ! f(a + (i-1/2)(b-a)/n)
                SolRFgrddiff = SolRFgrddiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutRFbd) * cos(element_proang_grd))  ! f(a + (i-1/2)(b-a)/n)
C...............END GROUND CALCULATION

              end do integrate_front_elements_CURVED

              SolTFskydd(icomp,isurf,icfctp,i)=SolTFskydiff
              SolRFskydd(icomp,isurf,icfctp,i)=SolRFskydiff
              SolTFgrddd(icomp,isurf,icfctp,i)=SolTFgrddiff
              SolRFgrddd(icomp,isurf,icfctp,i)=SolRFgrddiff

            end if ! end sky/ground diffuse request

C...........Calculate front effective diffuse (hemispherical) 
C...........properties and assign to commons

            call vb_eff_diff_properties(w,s,phi*RTOD,SolRufs,
     &              SolRdfs,SolTslat,OutRFdd,OutTFdd)
            SolTFdd(icomp,isurf,icfctp,i)=OutTFdd
            SolRFdd(icomp,isurf,icfctp,i)=OutRFdd

C...........Repeate procedure for BACK surface
            phi=phi*(-1.0)

C...........calculate back effective solar optical properties adn assign to commons
            call VB_SOL46_CURVE(s,w,sl_rad_overw,phi,vb_proang,
     &            SolRdfs,SolRufs,SolTslat,OutRBbd,OutTBbb,OutTBbd)
            SolTBbb(icomp,isurf,icfctp,i)=OutTBbb
            SolRBbd(icomp,isurf,icfctp,i)=OutRBbd
            SolTBbd(icomp,isurf,icfctp,i)=OutTBbd

C...........calculate back effective visible properties and assign to commons
!           call VB_SOL46_CURVE(s,w,sl_rad_overw,phi,proang,
!      &            VisRdfs,VisRufs,VisTslat,OutRBbd,OutTBbb,OutTBbd)
!           VisTBbb(icomp,isurf,icfctp,i)=OutTBbb
!           VisRBbd(icomp,isurf,icfctp,i)=OutRBbd
!           VisTBbd(icomp,isurf,icfctp,i)=OutTBbd

C...........CALCULATE GROUND AND SKY DIFFUSE COMPONENTS (BACK SURFACE)
C...........Check whether calculation of ground and sky diffuse
C...........components has been requested. 
            if (i_ground_sky_diff_calc.eq.1) then

              nelements    = 9   ! number of integration slices (10 deg increments)
              SolTBskydiff = 0.0 ! reset the sky diffuse transmittance
              SolRBskydiff = 0.0 ! reset the sky diffuse reflectance  
              SolTBgrddiff = 0.0 ! reset the ground diffuse transmittance
              SolRBgrddiff = 0.0 ! reset the ground diffuse reflectance            
            
C.............Perform integration of sky elements based on the midpoint rule:
C.............integral(a to b) f(x)dx = Sum(1 to n) f(a + (i-1/2)(b-a)/n) * ((b-a)/n)
              integrate_back_elements_CURVED: do iter = 1, nelements
      
                element_proang_sky = (90./float(nelements)) * 
     &                               (float(iter)-0.5) * 1.
 
                element_proang_sky = element_proang_sky * DTOR       ! convert to radians

                element_proang_grd = element_proang_sky * (-1.0)    ! profile angle for ground

C...............SKY CALCULATION
C...............calculate the beam-beam and beam-diffuse transmission
C...............at the current sky element profile angle
                call  VB_SOL46_THICK(s,w,t,phi,
     &                              element_proang_sky,           ! profile angle of sky element
     &                              SolRdfs,SolRufs,SolTslat,     ! slat properties input
     &                              OutRBbd,OutTBbb,OutTBbd)      ! overall effective properties output

C...............Sum up each sky element's contribution to the sky
C...............diffuse transmittance and reflectance.
                SolTBskydiff = SolTBskydiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutTBbb + OutTBbd) * 
     &                     cos(element_proang_sky))         ! f(a + (i-1/2)(b-a)/n)
                SolRBskydiff = SolRBskydiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutRBbd) * cos(element_proang_sky))  ! f(a + (i-1/2)(b-a)/n)
C...............END SKY CALCULATION

C...............GROUND CALCULATION
C...............calculate the beam-beam and beam-diffuse transmission
C...............at the current ground element profile angle
                call  VB_SOL46_THICK(s,w,t,phi,
     &                              element_proang_grd,           ! profile angle of sky element
     &                              SolRdfs,SolRufs,SolTslat,     ! slat properties input
     &                              OutRBbd,OutTBbb,OutTBbd)      ! overall effective properties output

C...............Sum up each sky element's contribution to the sky
C...............diffuse transmittance and reflectance.
                SolTBgrddiff = SolTBgrddiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutTBbb + OutTBbd) * 
     &                      cos(element_proang_grd))         ! f(a + (i-1/2)(b-a)/n)
                SolRBgrddiff = SolRBgrddiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutRBbd) * cos(element_proang_grd))  ! f(a + (i-1/2)(b-a)/n)
C...............END GROUND CALCULATION

              end do integrate_back_elements_CURVED

              SolTBskydd(icomp,isurf,icfctp,i)=SolTBskydiff
              SolRBskydd(icomp,isurf,icfctp,i)=SolRBskydiff
              SolTBgrddd(icomp,isurf,icfctp,i)=SolTBgrddiff
              SolRBgrddd(icomp,isurf,icfctp,i)=SolRBgrddiff

            end if ! end sky/ground diffuse request

C...........Calculate back effective diffuse (hemispherical) 
C...........properties and assign to commons
            call vb_eff_diff_properties(w,s,phi*RTOD,SolRufs,
     &            SolRdfs,SolTslat,OutRBdd,OutTBdd)
            SolTBdd(icomp,isurf,icfctp,i)=OutTBdd
            SolRBdd(icomp,isurf,icfctp,i)=OutRBdd

          else ! slat is considered FLAT with thickness t [mm]

C...........calculate front effective solar optical properties and assign to commons
            call VB_SOL46_THICK(s,w,t,phi,vb_proang,
     &            SolRdfs,SolRufs,SolTslat,OutRFbd,OutTFbb,OutTFbd)
            SolTFbb(icomp,isurf,icfctp,i)=OutTFbb
            SolRFbd(icomp,isurf,icfctp,i)=OutRFbd
            SolTFbd(icomp,isurf,icfctp,i)=OutTFbd

C TODO: replace with new format
c      call add_to_xml_reporting (
c     &         OutRFbd,
c     &         'building/zone_01/envelope/windows/RHO_F_BD_pd',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
c      call add_to_xml_reporting (
c     &         OutTFbb,
c     &         'building/zone_01/envelope/windows/TAU_F_BB_pd',
c     &         'units', '()',
c     &         'BEAM-BEAM FRONT TRANSMISSION'
c     &         //' (all zones) ' )     
c      call add_to_xml_reporting (
c     &         OutTFbd,
c     &         'building/zone_01/envelope/windows/TAU_F_BD_pd',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT TRANSMISSION'
c     &         //' (all zones) ' )     
     
C...........calculate front effective visible properties and assign to commons
!           call VB_SOL46_THICK(s,w,t,phi,proang,
!      &            VisRdfs,VisRufs,VisTslat,OutRFbd,OutTFbb,OutTFbd)
!           VisTFbb(icomp,isurf,icfctp,i)=OutTFbb
!           VisRFbd(icomp,isurf,icfctp,i)=OutRFbd
!           VisTFbd(icomp,isurf,icfctp,i)=OutTFbd

C...........CALCULATE GROUND AND SKY DIFFUSE COMPONENTS (FRONT SURFACE)
C...........Check whether calculation of ground and sky diffuse
C...........components has been requested. 
            if (i_ground_sky_diff_calc.eq.1) then

              nelements    = 9   ! number of integration slices (10 deg increments)
              SolTFskydiff = 0.0 ! reset the sky diffuse transmittance
              SolRFskydiff = 0.0 ! reset the sky diffuse reflectance  
              SolTFgrddiff = 0.0 ! reset the ground diffuse transmittance
              SolRFgrddiff = 0.0 ! reset the ground diffuse reflectance            
            
C.............Perform integration of sky elements based on the midpoint rule:
C.............integral(a to b) f(x)dx = Sum(1 to n) f(a + (i-1/2)(b-a)/n) * ((b-a)/n)
              integrate_front_elements_FLAT: do iter = 1, nelements
      
                element_proang_sky = (90./float(nelements)) * 
     &                               (float(iter)-0.5) * 1.
 
                element_proang_sky = element_proang_sky * DTOR       ! convert to radians

                element_proang_grd = element_proang_sky * (-1.0)    ! profile angle for ground

C...............SKY CALCULATION
C...............calculate the beam-beam and beam-diffuse transmission
C...............at the current sky element profile angle
                call  VB_SOL46_THICK(s,w,t,phi,
     &                              element_proang_sky,           ! profile angle of sky element
     &                              SolRdfs,SolRufs,SolTslat,     ! slat properties input
     &                              OutRFbd,OutTFbb,OutTFbd)      ! overall effective properties output

C...............Sum up each sky element's contribution to the sky
C...............diffuse transmittance and reflectance.
                SolTFskydiff = SolTFskydiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutTFbb + OutTFbd) * 
     &                     cos(element_proang_sky))         ! f(a + (i-1/2)(b-a)/n)
                SolRFskydiff = SolRFskydiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutRFbd) * cos(element_proang_sky))  ! f(a + (i-1/2)(b-a)/n)
C...............END SKY CALCULATION

C...............GROUND CALCULATION
C...............calculate the beam-beam and beam-diffuse transmission
C...............at the current ground element profile angle
                call  VB_SOL46_THICK(s,w,t,phi,
     &                              element_proang_grd,           ! profile angle of sky element
     &                              SolRdfs,SolRufs,SolTslat,     ! slat properties input
     &                              OutRFbd,OutTFbb,OutTFbd)      ! overall effective properties output

C...............Sum up each sky element's contribution to the sky
C...............diffuse transmittance and reflectance.
                SolTFgrddiff = SolTFgrddiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutTFbb + OutTFbd) * 
     &                      cos(element_proang_grd))         ! f(a + (i-1/2)(b-a)/n)
                SolRFgrddiff = SolRFgrddiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutRFbd) * cos(element_proang_grd))  ! f(a + (i-1/2)(b-a)/n)
C...............END GROUND CALCULATION

              end do integrate_front_elements_FLAT

              SolTFskydd(icomp,isurf,icfctp,i)=SolTFskydiff
              SolRFskydd(icomp,isurf,icfctp,i)=SolRFskydiff
              SolTFgrddd(icomp,isurf,icfctp,i)=SolTFgrddiff
              SolRFgrddd(icomp,isurf,icfctp,i)=SolRFgrddiff

            end if ! end sky/ground diffuse request

C...........Calculate front effective diffuse (hemispherical) 
C...........properties and assign to commons
            call VB_SOL_DD_THICK(s,w,t,phi,SolRufs,SolRdfs,
     &            SolTslat,OutRFdd,OutTFdd)
            SolTFdd(icomp,isurf,icfctp,i)=OutTFdd
            SolRFdd(icomp,isurf,icfctp,i)=OutRFdd

C TODO: change to new format
c      call add_to_xml_reporting (
c     &         OutRFdd,
c     &         'building/zone_01/envelope/windows/RHO_F_DD_pd',
c     &         'units', '()',
c     &         'DIFF-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
c      call add_to_xml_reporting (
c     &         OutTFdd,
c     &         'building/zone_01/envelope/windows/TAU_F_DD_pd',
c     &         'units', '()',
c     &         'DIFF-DIFF FRONT TRANSMISSION'
c     &         //' (all zones) ' )
     
C...........Repeate procedure for BACK surface
            phi=phi*(-1.0)

C...........calculate back effective solar optical properties adn assign to commons
            call VB_SOL46_THICK(s,w,t,phi,vb_proang,
     &            SolRdfs,SolRufs,SolTslat,OutRBbd,OutTBbb,OutTBbd)
            SolTBbb(icomp,isurf,icfctp,i)=OutTBbb
            SolRBbd(icomp,isurf,icfctp,i)=OutRBbd
            SolTBbd(icomp,isurf,icfctp,i)=OutTBbd

C...........calculate back effective visible properties and assign to commons
!           call VB_SOL46_THICK(s,w,t,phi,proang,
!      &            VisRdfs,VisRufs,VisTslat,OutRBbd,OutTBbb,OutTBbd)
!           VisTBbb(icomp,isurf,icfctp,i)=OutTBbb
!           VisRBbd(icomp,isurf,icfctp,i)=OutRBbd
!           VisTBbd(icomp,isurf,icfctp,i)=OutTBbd

C...........CALCULATE GROUND AND SKY DIFFUSE COMPONENTS (BACK SURFACE)
C...........Check whether calculation of ground and sky diffuse
C...........components has been requested. 
            if (i_ground_sky_diff_calc.eq.1) then

              nelements    = 9   ! number of integration slices (10 deg increments)
              SolTBskydiff = 0.0 ! reset the sky diffuse transmittance
              SolRBskydiff = 0.0 ! reset the sky diffuse reflectance  
              SolTBgrddiff = 0.0 ! reset the ground diffuse transmittance
              SolRBgrddiff = 0.0 ! reset the ground diffuse reflectance            
            
C.............Perform integration of sky elements based on the midpoint rule:
C.............integral(a to b) f(x)dx = Sum(1 to n) f(a + (i-1/2)(b-a)/n) * ((b-a)/n)
              integrate_back_elements_FLAT: do iter = 1, nelements
      
                element_proang_sky = (90./float(nelements)) * 
     &                               (float(iter)-0.5) * 1.
 
                element_proang_sky = element_proang_sky * DTOR       ! convert to radians

                element_proang_grd = element_proang_sky * (-1.0)    ! profile angle for ground

C...............SKY CALCULATION
C...............calculate the beam-beam and beam-diffuse transmission
C...............at the current sky element profile angle
                call  VB_SOL46_THICK(s,w,t,phi,
     &                              element_proang_sky,           ! profile angle of sky element
     &                              SolRdfs,SolRufs,SolTslat,     ! slat properties input
     &                              OutRBbd,OutTBbb,OutTBbd)      ! overall effective properties output

C...............Sum up each sky element's contribution to the sky
C...............diffuse transmittance and reflectance.
                SolTBskydiff = SolTBskydiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutTBbb + OutTBbd) * 
     &                     cos(element_proang_sky))         ! f(a + (i-1/2)(b-a)/n)
                SolRBskydiff = SolRBskydiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutRBbd) * cos(element_proang_sky))  ! f(a + (i-1/2)(b-a)/n)
C...............END SKY CALCULATION

C...............GROUND CALCULATION
C...............calculate the beam-beam and beam-diffuse transmission
C...............at the current ground element profile angle
                call  VB_SOL46_THICK(s,w,t,phi,
     &                              element_proang_grd,           ! profile angle of sky element
     &                              SolRdfs,SolRufs,SolTslat,     ! slat properties input
     &                              OutRBbd,OutTBbb,OutTBbd)      ! overall effective properties output

C...............Sum up each sky element's contribution to the sky
C...............diffuse transmittance and reflectance.
                SolTBgrddiff = SolTBgrddiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutTBbb + OutTBbd) * 
     &                      cos(element_proang_grd))         ! f(a + (i-1/2)(b-a)/n)
                SolRBgrddiff = SolRBgrddiff + 
     &                     ((pi/2.)/float(nelements))*                  ! (b-a)/n
     &                     ((OutRBbd) * cos(element_proang_grd))  ! f(a + (i-1/2)(b-a)/n)
C...............END GROUND CALCULATION

              end do integrate_back_elements_FLAT

              SolTBskydd(icomp,isurf,icfctp,i)=SolTBskydiff
              SolRBskydd(icomp,isurf,icfctp,i)=SolRBskydiff
              SolTBgrddd(icomp,isurf,icfctp,i)=SolTBgrddiff
              SolRBgrddd(icomp,isurf,icfctp,i)=SolRBgrddiff

            end if ! end sky/ground diffuse request

C...........Calculate back effective diffuse (hemispherical) 
C...........properties and assign to commonss
            call VB_SOL_DD_THICK(s,w,t,phi,SolRufs,SolRdfs,
     &            SolTslat,OutRBdd,OutTBdd)
            SolTBdd(icomp,isurf,icfctp,i)=OutTBdd
            SolRBdd(icomp,isurf,icfctp,i)=OutRBdd

          end if
   
C......Layer type is a PLEATED DRAPE   
       elseif(itype.eq.iPleatedDrape)then
            
            OMEGA_V = ABS( MAX( -89.5*DTOR, MIN( 89.5*DTOR, OHM_V_RAD)))
            OMEGA_H = ABS( MAX( -89.5*DTOR, MIN( 89.5*DTOR, OHM_H_RAD)))

C.....TODO: CHange to new format
c      call add_to_xml_reporting (
c     &         OMEGA_V,
c     &         'building/zone_01/envelope/windows/OMEGA_V',
c     &         'units', '()',
c     &         'VERTICAL INCIDENCE ANGLE'
c     &         //' (all zones) ' )

c      call add_to_xml_reporting (
c     &         OMEGA_H,
c     &         'building/zone_01/envelope/windows/OMEGA_H',
c     &         'units', '()',
c     &         'HORIZONTAL INCIDENCE ANGLE'
c     &         //' (all zones) ' )
     
C.........If shade is retracted set solar transmittance to 1 and skip to next layer
          if(i_shd(icomp,icfctp).eq.0)then
            SolTFbb(icomp,isurf,icfctp,i)=0.999
            SolRFbd(icomp,isurf,icfctp,i)=0.001
            SolTFbd(icomp,isurf,icfctp,i)=0.001
            SolTFdd(icomp,isurf,icfctp,i)=0.999
            SolRFdd(icomp,isurf,icfctp,i)=0.001
            SolTBbb(icomp,isurf,icfctp,i)=0.999
            SolRBbd(icomp,isurf,icfctp,i)=0.001
            SolTBbd(icomp,isurf,icfctp,i)=0.001
            SolTBdd(icomp,isurf,icfctp,i)=0.999
            SolRBdd(icomp,isurf,icfctp,i)=0.001
            goto 100
          endif

C.........set beam-beam reflectances to zero
          SolRFbb(icomp,isurf,icfctp,i)=0.0
          SolRBbb(icomp,isurf,icfctp,i)=0.0

C.........assign direct normal solar/visible MATERIAL properties to local variables
          SolRf_mat=solRF(icomp,icfctp,i)
          SolRb_mat=solRB(icomp,icfctp,i)
          SolT_mat=solT(icomp,icfctp,i)
          SolTf_tot_mat=SolTf_tot(icomp,icfctp,i)
          SolTb_tot_mat=SolTb_tot(icomp,icfctp,i)
          SolTf_bd_mat=SolTf_bd(icomp,icfctp,i)
          SolTb_bd_mat=SolTb_bd(icomp,icfctp,i)

C.........assign pleated drape descriptors to local variables
          openness = SolT_mat
          w=drp_w(icomp,icfctp)
          s=drp_s(icomp,icfctp)         

C.........calculate front effective material solar diff-diff properties
          CALL FM_DIFF(SolRf_mat, SolTf_tot_mat, SolT_mat, itype,
     &      OutRHOFF_DD,OutTAUFF_DD)

C.........calculate back effective material solar diff-diff properties
          CALL FM_DIFF(SolRb_mat, SolTb_tot_mat, SolT_mat, itype,
     &      OutRHOBF_DD,OutTAUBF_DD)        

C.........calculate front effective pleated drape solar diff-diff properties and assign to commons
          CALL PD_DIFF(s, w, OutRHOFF_DD, OutRHOBF_DD, OutTAUFF_DD,
     &      OutRFdd, OutTFdd)

          SolTFdd(icomp,isurf,icfctp,i)=OutTFdd
          SolRFdd(icomp,isurf,icfctp,i)=OutRFdd

C TODO: change to new format
c      call add_to_xml_reporting (
c     &         OutRFdd,
c     &         'building/zone_01/envelope/windows/RHO_F_DD_pd',
c     &         'units', '()',
c     &         'DIFF-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
c      call add_to_xml_reporting (
c     &         OutTFdd,
c     &         'building/zone_01/envelope/windows/TAU_F_DD_pd',
c     &         'units', '()',
c     &         'DIFF-DIFF FRONT TRANSMISSION'
c     &         //' (all zones) ' )
        
C.........calculate back effective pleated drape solar diff-diff properties and assign to commons
          CALL PD_DIFF(s, w, OutRHOBF_DD, OutRHOFF_DD, OutTAUFF_DD,
     &      OutRBdd, OutTBdd)

          SolTBdd(icomp,isurf,icfctp,i)=OutTBdd
          SolRBdd(icomp,isurf,icfctp,i)=OutRBdd
            
C.........calculate front effective pleated drape solar beam- properties and assign to commons
          CALL PD_BEAM(s, w, OHM_V_RAD, OHM_H_RAD,
     &    SolRf_mat, SolT_mat, SolTf_bd_mat, OutRHOFF_DD, OutTAUFF_DD,
     &    SolRb_mat, SolT_mat, SolTb_bd_mat, OutRHOBF_DD, OutTAUFF_DD,
     &    OutRFbd, OutTFbb, OutTFbd)
        
          SolTFbb(icomp,isurf,icfctp,i)=OutTFbb
          SolRFbd(icomp,isurf,icfctp,i)=OutRFbd
          SolTFbd(icomp,isurf,icfctp,i)=OutTFbd 
 
C TODO: change to new format      
c       call add_to_xml_reporting (
c     &         SolRFbd(icomp,isurf,icfctp,i),
c     &         'building/zone_01/envelope/windows/RHO_F_BD_pd',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
c      call add_to_xml_reporting (
c     &         SolTFbb(icomp,isurf,icfctp,i),
c     &         'building/zone_01/envelope/windows/TAU_F_BB_pd',
c     &         'units', '()',
c     &         'BEAM-BEAM FRONT TRANSMISSION'
c     &         //' (all zones) ' )     
c      call add_to_xml_reporting (
c     &         SolTFbd(icomp,isurf,icfctp,i),
c     &         'building/zone_01/envelope/windows/TAU_F_BD_pd',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT TRANSMISSION'
c     &         //' (all zones) ' )        

C.........calculate back effective pleated drape solar beam- properties and assign to commons   
          CALL PD_BEAM(s, w, OHM_V_RAD, OHM_H_RAD,
     &    SolRb_mat, SolT_mat, SolTb_bd_mat, OutRHOBF_DD, OutTAUFF_DD,
     &    SolRf_mat, SolT_mat, SolTf_bd_mat, OutRHOFF_DD, OutTAUFF_DD,
     &    OutRBbd, OutTBbb, OutTBbd)

          SolTBbb(icomp,isurf,icfctp,i)=OutTBbb
          SolRBbd(icomp,isurf,icfctp,i)=OutRBbd
          SolTBbd(icomp,isurf,icfctp,i)=OutTBbd
      
C......Layer type is ROLLER BLIND
       elseif(itype.eq.iRollerBlind)then !roller blind type

C.........If shade is retracted set solar transmittance to 1 and skip to next layer
          if(i_shd(icomp,icfctp).eq.0)then
            SolTFbb(icomp,isurf,icfctp,i)=0.999
            SolRFbd(icomp,isurf,icfctp,i)=0.001
            SolTFbd(icomp,isurf,icfctp,i)=0.001
            SolTFdd(icomp,isurf,icfctp,i)=0.999
            SolRFdd(icomp,isurf,icfctp,i)=0.001
            SolTBbb(icomp,isurf,icfctp,i)=0.999
            SolRBbd(icomp,isurf,icfctp,i)=0.001
            SolTBbd(icomp,isurf,icfctp,i)=0.001
            SolTBdd(icomp,isurf,icfctp,i)=0.999
            SolRBdd(icomp,isurf,icfctp,i)=0.001
            goto 100
          endif      
        
C.........set beam-beam reflectances to zero
          SolRFbb(icomp,isurf,icfctp,i)=0.0
          SolRBbb(icomp,isurf,icfctp,i)=0.0

C.........assign direct normal solar/visible MATERIAL properties to local variables
          SolRF_mat=solRF(icomp,icfctp,i)
          SolRB_mat=solRB(icomp,icfctp,i)
          SolT_mat=solT(icomp,icfctp,i)
          SolTf_tot_mat=SolTf_tot(icomp,icfctp,i)
          SolTb_tot_mat=SolTb_tot(icomp,icfctp,i)
          SolTf_bd_mat=SolTf_bd(icomp,icfctp,i)
          SolTb_bd_mat=SolTb_bd(icomp,icfctp,i)

C.........assign pleated drape descriptors to local variables
          openness = SolT_mat
      
C.........The following is used for determining short wave drape properties
      
          CALL RB_DIFF(SolRF_mat,SolTf_tot_mat,SolT_mat,itype,
     &          OutRFdd,OutTFdd)

          SolTFdd(icomp,isurf,icfctp,i)=OutTFdd
          SolRFdd(icomp,isurf,icfctp,i)=OutRFdd

C TODO: change to new format
c      call add_to_xml_reporting (
c     &         OutRFdd,
c     &         'building/zone_01/envelope/windows/RHO_F_DD_pd',
c     &         'units', '()',
c     &         'DIFF-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
c      call add_to_xml_reporting (
c     &         OutTFdd,
c     &         'building/zone_01/envelope/windows/TAU_F_DD_pd',
c     &         'units', '()',
c     &         'DIFF-DIFF FRONT TRANSMISSION'
c     &         //' (all zones) ' )
     
          CALL RB_DIFF(SolRB_mat,SolTb_tot_mat,SolT_mat,itype,
     &          OutRBdd,OutTBdd)        

          SolTBdd(icomp,isurf,icfctp,i)=OutTBdd
          SolRBdd(icomp,isurf,icfctp,i)=OutRBdd 
      
C.........Calculate front effective solar optical properties      
          CALL RB_BEAM(anginc,SolRF_mat,SolTf_tot_mat,SolT_mat,
     &          OutRFbd, OutTFbb, OutTFbd)

          SolRFbd(icomp,isurf,icfctp,i)=OutRFbd
          SolTFbb(icomp,isurf,icfctp,i)=OutTFbb
          SolTFbd(icomp,isurf,icfctp,i)=OutTFbd

c      call add_to_xml_reporting (
c     &         OutRFbd,
c     &         'building/zone_01/envelope/windows/RHO_F_BD_pd',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
c      call add_to_xml_reporting (
c     &         OutTFbb,
c     &         'building/zone_01/envelope/windows/TAU_F_BB_pd',
c     &         'units', '()',
c     &         'BEAM-BEAM FRONT TRANSMISSION'
c     &         //' (all zones) ' )     
c      call add_to_xml_reporting (
c     &         OutTFbd,
c     &         'building/zone_01/envelope/windows/TAU_F_BD_pd',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT TRANSMISSION'
c     &         //' (all zones) ' )        
     
C.........Calculate back effective solar optical properties  
          CALL RB_BEAM(anginc,SolRB_mat,SolTb_tot_mat,SolT_mat,
     &          OutRBbd, OutTBbb, OutTBbd)
      
          SolRBbd(icomp,isurf,icfctp,i)=OutRBbd
          SolTBbb(icomp,isurf,icfctp,i)=OutTBbb
          SolTBbd(icomp,isurf,icfctp,i)=OutTBbd
             
C......Layer type is INSECT SCREEN
       elseif(itype.eq.iInsectScreen)then

C.........If shade is retracted set solar transmittance to 1 and skip to next layer
          if(i_shd(icomp,icfctp).eq.0)then
            SolTFbb(icomp,isurf,icfctp,i)=0.999
            SolRFbd(icomp,isurf,icfctp,i)=0.001
            SolTFbd(icomp,isurf,icfctp,i)=0.001
            SolTFdd(icomp,isurf,icfctp,i)=0.999
            SolRFdd(icomp,isurf,icfctp,i)=0.001
            SolTBbb(icomp,isurf,icfctp,i)=0.999
            SolRBbd(icomp,isurf,icfctp,i)=0.001
            SolTBbd(icomp,isurf,icfctp,i)=0.001
            SolTBdd(icomp,isurf,icfctp,i)=0.999
            SolRBdd(icomp,isurf,icfctp,i)=0.001
            goto 100
          endif      
        
C.........set beam-beam reflectances to zero
          SolRFbb(icomp,isurf,icfctp,i)=0.0
          SolRBbb(icomp,isurf,icfctp,i)=0.0

C.........assign direct normal solar/visible MATERIAL properties to local variables
          SolRF_mat=solRF(icomp,icfctp,i)
          SolRB_mat=solRB(icomp,icfctp,i)
          SolT_mat=solT(icomp,icfctp,i)
          SolTf_tot_mat=SolTf_tot(icomp,icfctp,i)
          SolTb_tot_mat=SolTb_tot(icomp,icfctp,i)
          SolTf_bd_mat=SolTf_bd(icomp,icfctp,i)
          SolTb_bd_mat=SolTb_bd(icomp,icfctp,i)

C.........assign insect screen descriptors to local variables
          openness = SolT_mat

C.........Calculates insect screen front diffuse-diffuse solar optical properties      
          CALL IS_DIFF(SolRF_mat,SolTf_tot_mat,SolT_mat,itype,
     &          OutRFdd,OutTFdd)

          SolTFdd(icomp,isurf,icfctp,i)=OutTFdd
          SolRFdd(icomp,isurf,icfctp,i)=OutRFdd
 
C TODO: change to new format
c      call add_to_xml_reporting (
c     &         OutRFdd,
c     &         'building/zone_01/envelope/windows/RHO_F_DD_pd',
c     &         'units', '()',
c     &         'DIFF-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
c      call add_to_xml_reporting (
c     &         OutTFdd,
c     &         'building/zone_01/envelope/windows/TAU_F_DD_pd',
c     &         'units', '()',
c     &         'DIFF-DIFF FRONT TRANSMISSION'
c     &         //' (all zones) ' )
     
C.........Calculates insect screen back diffuse-diffuse solar optical properties  
          CALL IS_DIFF(SolRB_mat,SolTb_tot_mat,SolT_mat,itype,
     &          OutRBdd,OutTBdd)        

          SolTBdd(icomp,isurf,icfctp,i)=OutTBdd
          SolRBdd(icomp,isurf,icfctp,i)=OutRBdd

C.........Calculate front effective insect screen off-normal solar optical properties
          CALL IS_BEAM(anginc,SolRF_mat,SolTf_tot_mat,SolT_mat,
     &          OutRFbd, OutTFbb, OutTFbd) 

          SolRFbd(icomp,isurf,icfctp,i)=OutRFbd
          SolTFbb(icomp,isurf,icfctp,i)=OutTFbb
          SolTFbd(icomp,isurf,icfctp,i)=OutTFbd

C TODO: change to new format
c      call add_to_xml_reporting (
c     &         OutRFbd,
c     &         'building/zone_01/envelope/windows/RHO_F_BD_pd',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
c      call add_to_xml_reporting (
c     &         OutTFbb,
c     &         'building/zone_01/envelope/windows/TAU_F_BB_pd',
c     &         'units', '()',
c     &         'BEAM-BEAM FRONT TRANSMISSION'
c     &         //' (all zones) ' )     
c      call add_to_xml_reporting (
c     &         OutTFbd,
c     &         'building/zone_01/envelope/windows/TAU_F_BD_pd',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT TRANSMISSION'
c     &         //' (all zones) ' )   
     
C.........Calculate back effective insect screen off-normal solar optical properties 
          CALL IS_BEAM(anginc,SolRB_mat,SolTb_tot_mat,SolT_mat,
     &          OutRBbd, OutTBbb, OutTBbd)
     
          SolRBbd(icomp,isurf,icfctp,i)=OutRBbd
          SolTBbb(icomp,isurf,icfctp,i)=OutTBbb
          SolTBbd(icomp,isurf,icfctp,i)=OutTBbd
      
C......Layer type is GAS GAP
       elseif(itype.eq.iGasGap)then

C.........set gas gap properties to 0 except for bb and dd transmission, set to 1. 
          SolTFbb(icomp,isurf,icfctp,i)=0.999
          SolTBbb(icomp,isurf,icfctp,i)=0.999
          SolRFbb(icomp,isurf,icfctp,i)=0.0
          SolRBbb(icomp,isurf,icfctp,i)=0.0
          SolTFbd(icomp,isurf,icfctp,i)=0.0
          SolTBbd(icomp,isurf,icfctp,i)=0.0
          SolRFbd(icomp,isurf,icfctp,i)=0.0
          SolRBbd(icomp,isurf,icfctp,i)=0.0
          SolTFdd(icomp,isurf,icfctp,i)=0.999
          SolTBdd(icomp,isurf,icfctp,i)=0.999
          SolRFdd(icomp,isurf,icfctp,i)=0.0
          SolRBdd(icomp,isurf,icfctp,i)=0.0

        else

          write(outs,'(a,i2,a,i2,a,i2,a)')
     &      'ERROR: layer type', itype, ' for layer ',i,' of CFC type',
     &      icfctp,' is not supported.'
          CALL EDISP(IUOUT,outs)
          goto 1000
        end if 

  100 continue

 1000 return

      end

C ********************************************************************
C                    --vb_eff_diff_properties--
C
C Calculates the effective diffuse radiation properties of a slat-type
C blind layer. The method applies to isotropic diffuse solar radiation 
C and longwave radiation. Returns the transmittance (tauvb), and front-
C side reflectance (rhofvb) of the slat blind layer. To obtain back-
C side reflectance call a second time with the same input data except
C negate the slat angle (phi).
C
C Calculation method based on four surface slat model. 
C 
C Four-surface model geometry: ad - front ficticious (surface 1)
C                              cf - back ficticious (sruface 2)
C                              ac - top slat (surface 3)
C                              df - bottom slat (surface 4)
C
C Reference:
C Kotey, N.A., Wright, J.L. (2006) 'Simplified Solar Optical
C Calculations for Windows with Venetian Blinds', Proceedings of
C the 31st Conference of the Solar Energy Society of Canada Inc.
C (SESCI) and 1st Solar Buildings Conference (SBRN), Montreal,
C Quebec, Canada.
C
C *******************************************************************
      subroutine vb_eff_diff_properties(w,s,phi,rhof,rhob,tau,
     &             rhofvb,tauvb)
      IMPLICIT NONE
      real s            ! slat spacing (any length units; as long as same units as w)
      real w            ! slat tip-to-tip width (any length units; same units as s)
      real phi          ! slat angle, degrees (-90 <= PHI <= 90)
      real phi_rad      ! slat angle, radians
      real rhof         ! front slat reflectance input
                        ! treat as reflectance of upwards facing slat 
      real rhob         ! back slat reflectance input
                        ! treat as reflectance of downwards facing slat
      real rho_down     ! reflectance of downward-facing slat surfaces
      real rho_up       ! reflectance of upward-facing slat surfaces
      real tau          ! longwave transmittance of slats
      real rhofvb       ! returned: front side effective longwave reflectance of venetian blind
      real tauvb        ! returned: effective longwave transmittance of venetian blind
      real pi
C      real ab,bc        ! lengths of slat sub-surfaces
      !real ae,bd,bf,ce
      real cd,af        ! lengths of the diagonal strings used in the four-surface model
      real F12_4,F13_4, F14_4, F31_4, F41_4, FSS_4                      ! four-surface model shape factors
      real C3, B3, C4, B4, K3, K4, DEN                                 ! temporaries

C Front reflectance of slats given in *.cfc file
C corresponds to upwards facing slat surface reflectance.
C Back reflectance of slats given in *.cfc file
C corresponds to downwards facing slat surface reflectance
      rho_up=rhof
      rho_down=rhob 

      pi=3.14159265
      phi_rad=phi*pi/180.      !convert to radians

C overlap surface length = slat width - slat spacing
C      ab=w-s
C      bc=s
C      de=bc
C      ef=ab

C diagonal string lengths used to calculate shape factors using Hottel's method
      !ae = sqrt ((de*cos(phi_rad))**2   +   (s-de*sin(phi_rad))**2)
      !bd = sqrt ((s+ab*sin(phi_rad))**2 +   (ab*cos(phi_rad))**2)
      cd = sqrt ((W*cos(phi_rad))**2    +   (S+W*sin(phi_rad))**2)
      af = sqrt ((W*cos(phi_rad))**2    +   (S-W*sin(phi_rad))**2)
      !bf = sqrt ((bc*cos(phi_rad))**2   +   (s-bc*sin(phi_rad))**2)
      !ce = sqrt ((ef*cos(phi_rad))**2   +   (s+ef*sin(phi_rad))**2)

      F13_4 = (w+s-cd)/(2.*s)
      F14_4 = (w+s-af)/(2.*s)
      FSS_4 = 1. - (s/w)*(F13_4+F14_4)
      F31_4 = (s/w)*F13_4
      F41_4 = (s/w)*F14_4
      F12_4 = 1. - F13_4 - F14_4

      DEN = 1. - (tau*FSS_4)                      ! DENOMINATOR - USED FOUR TIMES
      C3 = (rho_down*F31_4 + tau*F41_4)/DEN
      B3 = (rho_down*FSS_4)/DEN
      C4 = (rho_up*F41_4 + tau*F31_4)/DEN
      B4 = (rho_up*FSS_4)/DEN

      K3 = (C3 + (B3*C4))/(1.0 - (B3*B4))
      K4 = (C4 + (B4*C3))/(1.0 - (B3*B4))

      tauvb = F12_4 + (F14_4*K3) + (F13_4*K4)      ! transmittance of VB (equal front/back)
      rhofvb = (F13_4*K3) + (F14_4*K4)          ! diffuse reflectance of VB front-side

      IF (TAUVB < 0.) TAUVB = 0.
      IF (RHOFVB < 0.) RHOFVB = 0.

      return
      end


C ********************************************************************
C                             --SOLMATS--
C 
C Solves square matrix by the elimination method supplemented by a 
C search for the largest pivotal element at each stage. 
C
C ********************************************************************
      SUBROUTINE SOLMATS( N, A, XSOL)
      IMPLICIT NONE
      INTEGER N               ! # of active rows in A
      REAL A(N,N+2),XSOL(N)   ! minimum required dimensions: A( N, N+2), XSOL( N)
      REAL CMAX, TEMP, C, Y, D
      INTEGER NM1, NP1, NP2, I, J, L, LP, NOS, NI, NJ

      NM1=N-1
      NP1=N+1
      NP2=N+2

      DO I=1,N
        A(I,NP2)=0.0
      END DO

      DO I=1,N
        DO J=1,NP1
          A(I,NP2)=A(I,NP2)+A(I,J)
        END DO
      END DO

      DO L=1,N-1
        CMAX=A(L,L)
        LP=L+1
        NOS=L

        DO I=LP,N
          IF(ABS(CMAX).LT.ABS(A(I,L)))THEN
            CMAX=A(I,L)
            NOS=I
          ENDIF
        END DO

!     Swap rows
        IF (NOS.NE.L) THEN
          DO J=1,NP2
            TEMP=A(L,J)
            A(L,J)=A(NOS,J)
            A(NOS,J)=TEMP
          END DO
        END IF

        DO I=LP,N
          C=0.0
          Y=-A(I,L)/A(L,L)
          DO J=L,NP2
            A(I,J)=A(I,J)+Y*A(L,J)
          END DO
          DO J=L,NP1
            C=C+A(I,J)
          END DO
        END DO
      END DO

!  Now backsubstitute
      XSOL(N)=A(N,NP1)/A(N,N)
      DO I=1,NM1
        NI=N-I
        D=0.0
        DO J=1,I
          NJ=N+1-J
          D=D+A(NI,NJ)*XSOL(NJ)
        END DO
        XSOL(NI)=(A(NI,NP1)-D)/A(NI,NI)
      END DO

      return

      END


C ********************************************************************
C                          --VB_SOL46_CURVE--
C 
C Four and six surface curve-slat model with slat transmittance.
C
C
C Calculates the effective solar properties of a slat-type blind layer.
C Returns the transmittance and front-side reflectance of the blind
C layer. If you want the back-side reflectance call the subroutine a 
C second time with the same input data - except negate the slat angle,
C PHI_RAD.
C
C INPUT:
C S = slat spacing (any unit for lenght but the same as used for W)
C W = slat width, tip-to-tip (any unit for length but same as S)
C SL_RAD_OVERW = slat curvature correction factor
C PHI_RAD = slat angle (in radians) is positive when the front-side
C           slat tip is low
C OHM_RAD = solar profile angle (radians) 
C           ( must be -90*PI/180 < OHM_RAD < 90*PI/180 )
C RHOUFS_SLAT = solar reflectance of upward-facing slat surfaces
C RHODFS_SLAT = solar reflectance of downward-facing slat surfaces
C TAU_SLAT = solar transmittance of slat 
C ALL SOLAR SLAT PROPERTIES - INCIDENT TO DIFFUSE
C
C RETURNED:
C TAUSFBB = solar beam-to-beam transmittance of the slat blind front
C           surface
C TAUSFBD = solar beam-to-diffuse transmittance of the slat blind 
C           front surface
C RHOSFBD = solar beam-to-diffuse reflectance of the slat blind 
C           front surface
C
C Reference:
C Kotey, N.A., Collins, M.R., Wright, J.L, Jiang, T. (2008)
C 'A Simplified Method for Calculating the Effective Solar Optical
C Properties of a Venetian Blind Layer for Building Energy 
C Simulation', ASME Journal of Solar Energy Engineering, In Review.
C
C ********************************************************************
      SUBROUTINE VB_SOL46_CURVE(S,W,SL_RAD_OVERW,PHI_RAD,OHM_RAD,
     &      RHODFS_SLAT,RHOUFS_SLAT,TAU_SLAT,RHOSFBD,TAUSFBB,TAUSFBD)
      IMPLICIT NONE

C INPUT VARIABLES
      REAL S 
      REAL W 
      REAL SL_RAD_OVERW
      REAL PHI_RAD 
      REAL OHM_RAD
      REAL RHODFS_SLAT 
      REAL RHOUFS_SLAT 
      REAL TAU_SLAT

C OUTPUT VARIABLES  
      REAL RHOSFBD 
      REAL TAUSFBB 
      REAL TAUSFBD

      REAL DE, PHI, OMEGA
      REAL SL_RAD,SL_THETA,Slope,T_CORR_D,T_CORR_F,RHO_TEMP,TAU_TEMP
      REAL XA,XB,XC,XD,XF,YA,YB,YC,YD,YF 
      REAL PI, SMALL_ERROR
      PARAMETER (PI = 3.14159265359, SMALL_ERROR = 0.000001)
      INTEGER CORR

      DE = 0.0          ! INITIALIZE DE
      PHI = 0.0         ! INITIALIZE PHI
      OMEGA = 0.0       ! INITIALIZE OMEGA
      CORR=1

!--------------------------------------------------------------------
!  CHECK TO SEE IF SLAT ANGLE IS GREATER THAN 90 DEGREES
!  AND SET THE SLAT ANGLE TO 90 DEGREES 
!  (THIS IS DONE FOR BOTH POSITIVE AND NEGATIVE SLAT ANGLES)
      IF ( (PHI_RAD.GT.(90.0*PI/180.0)) .OR. 
     &     (PHI_RAD.LT.(-90.0*PI/180.0)) ) THEN
        IF (PHI_RAD.LT.0.0) THEN
           PHI = -90.0*PI/180.0
        ELSE
           PHI = 90.0*PI/180.0
        ENDIF
      ELSE
        PHI = PHI_RAD     ! USUAL ACTION
      ENDIF
!--------------------------------------------------------------------

!  CHECK TO SEE IF PROFILE ANGLE IS GREATER THAN 90 DEGREES
!  AND SET THE PROFILE ANGLE TO 89.5 DEGREES 
!  (THIS IS DONE FOR BOTH POSITIVE AND NEGATIVE PROFILE ANGLES)
      IF ( (OHM_RAD.GT.(89.5*PI/180.0)) .OR. 
     &     (OHM_RAD.LT.(-89.5*PI/180.0)) ) THEN
        IF (OHM_RAD.LT.0.0) THEN
          OMEGA = -89.5*PI/180.0
        ELSE
          OMEGA = 89.5*PI/180.0
        ENDIF
      ELSE
        OMEGA = OHM_RAD   ! USUAL ACTION
      ENDIF


      SL_RAD=W*SL_RAD_OVERW      
      SL_THETA = 2.0*(ASIN(0.5/SL_RAD_OVERW))

!--------------------------------------------------------------------

      IF (CORR.GT.0) THEN    ! CORRECT FOR SLAT CURVATURE BY SETTING CORR = 1

!  DETERMINE BOUNDS FOR CURVATURE CORRECTION AND APPLY CORRECTION TO BEAM-BEAM TRANSMITTANCE
        IF(ABS(PHI+OMEGA).LT.(SL_THETA/2.0)) THEN

!  CALCULATE BEAM TRANSMISSION
          XA=SL_RAD*SIN(-SL_THETA/2.0)  !Glass-side end coordinate
          YA=SL_RAD*COS(-SL_THETA/2.0)
          XB=-XA      !Indoor-side end coordinate
          YB=YA
          YC=SL_RAD*COS(PHI+OMEGA)  !Tangent to slat in irradiance direction
          XC=SQRT(SL_RAD**2-YC**2)
          Slope=-XC/YC
          IF (ABS(Slope).LT.SMALL_ERROR) THEN
            XD=0.0
            YD=YA
            !XE=0.0
            !YE=YD
          ELSE
            IF ((PHI+OMEGA).LT.0.0) THEN
              XC=-XC
              Slope=-Slope
              XD=(YB-Slope*XB)/(-1.0/Slope-Slope)
              XF=(YA-Slope*XA)/(-1.0/Slope-Slope)
              !XE=XA+2.0*ABS(XA-XF)
            ELSE
              XD=(YA-Slope*XA)/(-1.0/Slope-Slope)
              XF=(YB-Slope*XB)/(-1.0/Slope-Slope)
              !XE=XB-2.0*ABS(XB-XF)
            ENDIF
            YD=-XD/Slope
            !YE=-XE/Slope
            YF=-XF/Slope
          ENDIF

          T_CORR_D=SQRT((XC-XD)**2+(YC-YD)**2)  !Slat thickness perpendicular to light direction
          T_CORR_F=SQRT((XC-XF)**2+(YC-YF)**2)

          TAUSFBB=1.0-T_CORR_D/(S*COS(OMEGA))

        ELSE ! DO NOT APPLY CURVATURE CORRECTION TO BEAM-BEAM TRANSMITTANCE

          DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))
!  CHECK TO SEE IF THERE IS DIRECT BEAM TRANSMISSION
          IF((DE/W).GT.(1.0 - SMALL_ERROR)) THEN         ! YES
            TAUSFBB = (DE-W)/DE
            IF(TAUSFBB.LT.0.0) TAUSFBB = 0.0
          ELSE                                ! NO
            TAUSFBB = 0.0
          ENDIF

        ENDIF

! CHECK TO SEE IF CURVATURE CORRECTION INCLUDES DOUBLE BLOCKAGE      
! (TAUSFBB < 0.0 AND SET TAUSFBB = 0.0)
        IF(TAUSFBB.LT.0.0) THEN  ! YES, THERE IS DOUBLE BLOCKAGE

          TAUSFBB = 0.0 

! DO NOT APPLY CURVATURE CORRECTION TO RHOSFBD, TAUSFBD IF TAUSFBB < 0.0
          DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))
          IF((DE/W).GT.(1.0 - SMALL_ERROR)) THEN          ! YES
            CALL VB_SOL4(S,W,OMEGA,DE,PHI,
     &        RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)
          ELSE                                ! NO
            CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &        RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)
          ENDIF

        ELSE  ! NO, THERE IS NO DOUBLE BLOCKAGE

          IF(ABS(PHI+OMEGA).LT.(SL_THETA/2.0)) THEN  ! YES, APPLY CURVATURE CORRECTION 

            XA=SL_RAD*SIN(-SL_THETA/2.0)      !Glass-side end coordinate
            YA=SL_RAD*COS(-SL_THETA/2.0)
            XB=-XA                              !Indoor-side end coordinate
            YB=YA
            YC=SL_RAD*COS(PHI+OMEGA)  !Tangent to slat in irradiance direction
            XC=SQRT(SL_RAD**2-YC**2)
            Slope=-XC/YC
            IF (ABS(Slope).LT.SMALL_ERROR) THEN
              XD=0.0
              YD=YA
             !XE=0.0
             !YE=YD
            ELSE
              IF ((PHI+OMEGA).LT.0.0) THEN
                XC=-XC
                Slope=-Slope
                XD=(YB-Slope*XB)/(-1.0/Slope-Slope)
                XF=(YA-Slope*XA)/(-1.0/Slope-Slope)
                !XE=XA+2.0*ABS(XA-XF)
              ELSE
                XD=(YA-Slope*XA)/(-1.0/Slope-Slope)
                XF=(YB-Slope*XB)/(-1.0/Slope-Slope)
                !XE=XB-2.0*ABS(XB-XF)
              ENDIF
              YD=-XD/Slope
              !YE=-XE/Slope
              YF=-XF/Slope
            ENDIF

            T_CORR_D=SQRT((XC-XD)**2+(YC-YD)**2)  !Slat thickness perpendicular to light direction
            T_CORR_F=SQRT((XC-XF)**2+(YC-YF)**2)

            IF ((PHI+OMEGA).GE.0.0) THEN       !Slat is lit from above
              DE=XC-XA
              CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &          RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)
              RHOSFBD=RHOSFBD*T_CORR_D/(S*COS(OMEGA))
              TAUSFBD=TAUSFBD*T_CORR_D/(S*COS(OMEGA))

            ELSE  !Slat is lit from below
              DE=XC-XA
              CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &          RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)
              RHO_TEMP=RHOSFBD*T_CORR_F/(S*COS(OMEGA))
              TAU_TEMP=TAUSFBD*T_CORR_F/(S*COS(OMEGA))
              DE=ABS(XB-XF)
              CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &          RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)
              RHOSFBD=RHOSFBD*(T_CORR_D-T_CORR_F)/
     &               (S*COS(OMEGA))+RHO_TEMP
              TAUSFBD=TAUSFBD*(T_CORR_D-T_CORR_F)/
     &               (S*COS(OMEGA))+TAU_TEMP
            ENDIF


          ELSE ! NO, DO NOT APPLY CURVATURE CORRECTION

            DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))
            IF((DE/W).GT.(1.0 - SMALL_ERROR)) THEN          ! YES
              CALL VB_SOL4(S, W, OMEGA, DE, PHI,
     &          RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)

            ELSE       ! NO
              CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &          RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)

            ENDIF
          ENDIF
        ENDIF
      ELSE   ! DO NOT CORRECT FOR SLAT CURVATURE
!----------------------------------------------------------------------------

!  CHECK TO SEE IF BEAM IS ALLIGNED WITH SLATS
        IF(ABS(PHI + OMEGA).LT.SMALL_ERROR) THEN  ! YES!
          RHOSFBD =       0.0 
          TAUSFBB =       1.0  
          TAUSFBD =       0.0
        ELSE                             ! BEAM NOT ALLIGNED WITH SLATS
          RHOSFBD =       0.0 
          TAUSFBB =       0.0  
          TAUSFBD =       0.0
          DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))
!----------------------------------------------------------------------------

!  CHECK TO SEE IF THERE IS DIRECT BEAM TRANSMISSION
          IF((DE/W).GT.(1.0 - SMALL_ERROR)) THEN          ! YES
            TAUSFBB = (DE-W)/DE
            IF(TAUSFBB.LT.0.0) TAUSFBB = 0.0
            CALL VB_SOL4(S, W, OMEGA, DE, PHI,
     &        RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)
          ELSE      ! NO
            TAUSFBB = 0.0
            CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &        RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)
          ENDIF  !  END CHECK FOR DIRECT BEAM TRANSMISSION
        ENDIF  ! END CHECK TO SEE IF BEAM ALLIGNED WITH SLATS

      ENDIF

!  FINAL CHECK FOR NEGATIVE VALUES
      IF(RHOSFBD.LT.0.0)RHOSFBD=0.0
      IF(TAUSFBD.LT.0.0)TAUSFBD=0.0
      IF(TAUSFBB.LT.0.0)TAUSFBB=0.0

      RETURN
      END


C ********************************************************************
C                          --VB_SOL46_THICK--
C 
C Four and six surface thick-slat model with slat transmittance.
C Thickness correction using EnergyPlus correction factor F_edge.
C Details of EnergyPlus thickness correction found in:
C EnergyPlus Engineering Reference (2008), US DOE
C
C Calculates the effective solar properties of a slat-type blind layer.
C Returns the transmittance and front-side reflectance of the blind
C layer. If you want the back-side reflectance call the subroutine a 
C second time with the same input data - except negate the slat angle,
C PHI_RAD.
C
C INPUT:
C S = slat spacing (any unit for lenght but the same as used for W)
C W = slat width, tip-to-tip (any unit for length but same as S)
C T = slat thickness (any unit but same as s and w)
C PHI_RAD = slat angle (in radians) is positive when the front-side
C           slat tip is low
C OHM_RAD = solar profile angle (radians) 
C           ( must be -90*PI/180 < OHM_RAD < 90*PI/180 )
C RHOUFS_SLAT = solar reflectance of upward-facing slat surfaces
C RHODFS_SLAT = solar reflectance of downward-facing slat surfaces
C TAU_SLAT = solar transmittance of slat 
C ALL SOLAR SLAT PROPERTIES - INCIDENT TO DIFFUSE
C
C RETURNED:
C TAUSFBB = solar beam-to-beam transmittance of the slat blind front
C           surface
C TAUSFBD = solar beam-to-diffuse transmittance of the slat blind 
C           front surface
C RHOSFBD = solar beam-to-diffuse reflectance of the slat blind 
C           front surface
C
C Reference:
C Kotey, N.A., Collins, M.R., Wright, J.L, Jiang, T. (2008)
C 'A Simplified Method for Calculating the Effective Solar Optical
C Properties of a Venetian Blind Layer for Building Energy 
C Simulation', ASME Journal of Solar Energy Engineering, In Review.
C
C ********************************************************************
      SUBROUTINE VB_SOL46_THICK(S, W, T, PHI_RAD, OHM_RAD,
     &  RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBB, TAUSFBD)
      IMPLICIT NONE
C INPUT VARIABLES
      REAL S 
      REAL W
      REAL T 
      REAL PHI_RAD 
      REAL OHM_RAD
      REAL RHODFS_SLAT 
      REAL RHOUFS_SLAT 
      REAL TAU_SLAT

C OUTPUT VARIABLES
      REAL RHOSFBD 
      REAL TAUSFBB 
      REAL TAUSFBD

      REAL DE, PHI, OMEGA, FEDGE, H
      REAL PI, SMALL_ERROR
      PARAMETER (PI = 3.14159265359, SMALL_ERROR = 0.000001)

      DE = 0.0          ! INITIALIZE DE
      PHI = 0.0              ! INITIALIZE PHI
      OMEGA = 0.0      ! INITIALIZE OMEGA


!-----------------------------------------------------------------------------------
!  CHECK TO SEE IF SLAT ANGLE IS EQUAL TO OR GREATER THAN 89.5 DEGREES
!  AND SET THE SLAT ANGLE TO 89.5 DEGREES 
!  (THIS IS DONE FOR BOTH POSITIVE AND NEGATIVE SLAT ANGLES)
      IF((PHI_RAD.ge.(89.5*PI/180.0)).OR.
     &   (PHI_RAD.le.(-89.5*PI/180.0)))THEN
        IF (PHI_RAD.lt.0.0) THEN
          PHI = -89.5*PI/180.0
        ELSE
          PHI = 89.5*PI/180.0
        ENDIF
      ELSE
        PHI = PHI_RAD     ! USUAL ACTION
      ENDIF
!-----------------------------------------------------------------------------------

!  CHECK TO SEE IF PROFILE ANGLE IS GREATER THAN 90 DEGREES
!  AND SET THE PROFILE ANGLE TO 89.5 DEGREES 
!  (THIS IS DONE FOR BOTH POSITIVE AND NEGATIVE PROFILE ANGLES)
      IF((OHM_RAD.gt.(89.5*PI/180.0)).OR.
     &   (OHM_RAD.lt.(-89.5*PI/180.0)))THEN
        IF (OHM_RAD.lt.0.0) THEN
          OMEGA = -89.5*PI/180.0
        ELSE
          OMEGA = 89.5*PI/180.0
        ENDIF
      ELSE
        OMEGA = OHM_RAD   ! USUAL ACTION
      ENDIF
!-----------------------------------------------------------------------------------

! CALCULATE SLAT THICKNESS CORRECTION FACTOR (EnergyPlus model)

      H=(S+T)-(T/COS(PHI))
      IF(H.LE.0.001)H=0.00 ! SLATS ARE TOUCHING

      FEDGE = (T*ABS(COS(PHI+OMEGA)))/
     &  ((H + T/(ABS(COS(PHI))))*(ABS(COS(OMEGA))))

!------------------------------------------------------------------------------------

C Check to see if beam is alligned with slats.
      IF(ABS(PHI + OMEGA).LT.SMALL_ERROR) THEN  ! YES
        RHOSFBD = FEDGE*((RHOUFS_SLAT+RHODFS_SLAT)/2.0)
        TAUSFBB =       1.0-FEDGE
        TAUSFBD =       0.0

      ELSE                             ! BEAM NOT ALLIGNED WITH SLATS
        RHOSFBD =       0.0
        TAUSFBB =       0.0
        TAUSFBD =       0.0

        DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))

C Check to see if there is direct beam transmission.
        IF((DE/W).GT.(1.0 - SMALL_ERROR)) THEN          ! YES
          TAUSFBB = ((DE-W)/DE) * (1.-FEDGE)
          IF(TAUSFBB.LT.0.0) TAUSFBB = 0.0
          CALL VB_SOL4(S, W, OMEGA, DE, PHI,
     &      RHODFS_SLAT, RHOUFS_SLAT, 
     &      TAU_SLAT,RHOSFBD, TAUSFBD)

          RHOSFBD= RHOSFBD*(1.-FEDGE) +
     &             FEDGE*((RHOUFS_SLAT+RHODFS_SLAT)/2.0)

          TAUSFBD = TAUSFBD*(1.-FEDGE)

        ELSE     ! NO
          TAUSFBB = 0.0
          CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &      RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)

          RHOSFBD= RHOSFBD*(1.-FEDGE) + 
     &      FEDGE*((RHOUFS_SLAT+RHODFS_SLAT)/2.0)

          TAUSFBD = TAUSFBD*(1.-FEDGE)

        ENDIF  !  END CHECK FOR DIRECT BEAM TRANSMISSION
      ENDIF  ! END CHECK TO SEE IF BEAM ALLIGNED WITH SLATS

C Final check for negativew values.
      IF(RHOSFBD.LT.0.0)RHOSFBD=0.0
      IF(TAUSFBD.LT.0.0)TAUSFBD=0.0
      IF(TAUSFBB.LT.0.0)TAUSFBB=0.0

      RETURN
      END


C ********************************************************************
C                             --VB_SOL4--
C 
C Four surface flat-slat model with slat transmittance.
C
C Calculates the effective solar properties of a slat-type blind layer.
C Returns the transmittance and front-side reflectance of the blind
C layer. If you want the back-side reflectance call the subroutine a 
C second time with the same input data - except negate the slat angle,
C PHI.
C
C INPUT:
C S = slat spacing (any unit for lenght but the same as used for W)
C W = slat width, tip-to-tip (any unit for length but same as S)
C PHI = slat angle (in radians) is positive when the front-side
C           slat tip is low
C OMEGA = solar profile angle (radians) 
C           ( must be -90*PI/180 < OMEGA < 90*PI/180 )
C RHO4 = solar reflectance of upward-facing slat surfaces
C RHO3 = solar reflectance of downward-facing slat surfaces
C TAU = solar transmittance of slat 
C ALL SOLAR SLAT PROPERTIES - INCIDENT TO DIFFUSE
C
C INTERMEDIATE VARIABLES:
C   AF, C  = Lengths of diagonal strings used in four surface model
C   F13, F14, F23, F24, F34, F43 =  shape factors
C   Z3, Z4 = Diffuse source terms from surfaces 3 and 4 due to incident
C            beam radiation      
C   J3, J4 = Radiosities from surfaces 3 and 4
C   B3, B4, C3, C4 = temporarily used variables
C
C RETURNED:
C TAUSFBD = solar beam-to-diffuse transmittance of the slat blind 
C           front surface
C RHOSFBD = solar beam-to-diffuse reflectance of the slat blind 
C           front surface
C
C Reference:
C Kotey, N.A., Collins, M.R., Wright, J.L, Jiang, T. (2008)
C 'A Simplified Method for Calculating the Effective Solar Optical
C Properties of a Venetian Blind Layer for Building Energy 
C Simulation', ASME Journal of Solar Energy Engineering, In Review.
C
C ********************************************************************
      SUBROUTINE VB_SOL4(S,W,OMEGA,DE,PHI,RHO3,RHO4,TAU,RHOSFBD,TAUSFBD)
      IMPLICIT NONE
C INPUT VARIABLES
      REAL S 
      REAL W 
      REAL PHI
      REAL RHO3
      REAL RHO4
      REAL TAU  
      REAL DE 
      REAL OMEGA

C OUTPUT VARIABLES
      REAL RHOSFBD 
      REAL TAUSFBD

      REAL AF, CD
      REAL F13, F14, F23, F24, F34, F43
      REAL Z3, Z4
      REAL J3, J4
      REAL B3, B4, C3, C4
      REAL PI, SMALL_ERROR
      PARAMETER (PI = 3.14159265359, SMALL_ERROR = 0.000001)

      AF = SQRT ((W*COS(PHI))**2   +   (S - W*SIN(PHI))**2)
      CD = SQRT ((W*COS(PHI))**2   +   (S + W*SIN(PHI))**2)
!-----------------------------------------------------------------------------------      
!  CHECK TO SEE WHICH SIDE OF SLAT IS SUNLIT
      IF((PHI + OMEGA).GE.0.0) THEN   ! SUN SHINES ON TOP OF SLAT

        Z3 = TAU*S/DE
        Z4 = RHO4*S/DE

      ELSE  ! SUN SHINES ON BOTTOM OF SLAT      
        Z3 = RHO3*S/DE
        Z4 = TAU*S/DE 
      ENDIF

!-----------------------------------------------------------------------------------      
!  CHECK TO SEE IF VENETIAN BLIND IS CLOSED
      IF( ABS(PHI - (PI/2.0)).LT.SMALL_ERROR ) THEN    !VENETIAN BLIND IS CLOSED

! CHECK TO SEE IF THERE ARE GAPS IN BETWEEN SLATS WHEN THE BLIND IS CLOSED
        IF(W.LT.S) THEN      !YES, THERE ARE GAPS IN BETWEEN SLATS
          RHOSFBD = (W/S)*RHO4
          TAUSFBD = (W/S)*TAU
        ELSE    ! NO, THERE ARE NO GAPS IN BETWEEN SLATS
          RHOSFBD = RHO4
          TAUSFBD = TAU
        ENDIF    ! END OF CHECK FOR GAPS IN BETWEEN SLATS

      ELSE   !VENETIAN BLIND IS OPENED

        F13 = (S+W-CD)/(2.0*S)
        F14 = (S+W-AF)/(2.0*S)
        F23 = (S+W-AF)/(2.0*S)
        F24 = (S+W-CD)/(2.0*S)
        F34 = (CD+AF-2.0*S)/(2.0*W)
        F43 = (CD+AF-2.0*S)/(2.0*W)

        C3 = 1.0 / (1.0 - TAU*F43)
        B3 = (RHO3*F34) / (1.0 - TAU*F43)
        C4 = 1.0 / (1.0 - TAU*F34)
        B4 = (RHO4*F43) / (1.0 - TAU*F34)
        J3 = (C3*Z3 + B3*C4*Z4) / (1.0 - B3*B4)
        J4 = (C4*Z4 + B4*C3*Z3) / (1.0 - B3*B4)

        RHOSFBD = F13*J3 + F14*J4
        TAUSFBD = F23*J3 + F24*J4
      ENDIF       ! END OF CHECK FOR CLOSED BLIND

!  FINAL CHECK FOR NEGATIVE VALUES
      IF(RHOSFBD.LT.0.0)RHOSFBD=0.0
      IF(TAUSFBD.LT.0.0)TAUSFBD=0.0
      RETURN
      END


C ********************************************************************
C                             --VB_SOL6--
C 
C Six surface flat-slat model with slat transmittance.
C
C Calculates the effective solar properties of a slat-type blind layer.
C Returns the transmittance and front-side reflectance of the blind
C layer. If you want the back-side reflectance call the subroutine a 
C second time with the same input data - except negate the slat angle,
C PHI.
C
C INPUT:
C S = slat spacing (any unit for lenght but the same as used for W)
C W = slat width, tip-to-tip (any unit for length but same as S)
C PHI = slat angle (in radians) is positive when the front-side
C           slat tip is low
C OMEGA = solar profile angle (radians) 
C           ( must be -90*PI/180 < OMEGA < 90*PI/180 )
C RHO46 = solar reflectance of upward-facing slat surfaces
C RHO35 = solar reflectance of downward-facing slat surfaces
C TAU = solar transmittance of slat 
C ALL SOLAR SLAT PROPERTIES - INCIDENT TO DIFFUSE
C
C INTERMEDIATE VARIABLES:
C AB, AE, AF, BC, BD, BF, CD, CE, EF  =  Lengths of slat segments and 
C                                        diagonal strings
C F13, F14, F23, F24, F34, F36, F15, F16 = Shape factors
C F43, F45, F54, F56, F63, F65, F25, F26 = Shape factors
C
C Z3, Z4 = Diffuse source terms from surfaces 3 and 4 due to incident
C          beam radiation
C J3, J4, J5, J6 = Radiosities from surfaces 3,4,5 and 6
C B3, B4, C3, C4 = Temporarily used variables
C N = Number of rows (or columns) in the NxN matrix used to solve for 
C     radiosities J3, J4, J4, J6
C A(20,22) = Coefficients of the radiosity equations matrix
C XSOL(20) = Solution vector (obtained after solving the radiosity
C            equations matrix
C
C RETURNED:
C TAUSFBD = solar beam-to-diffuse transmittance of the slat blind 
C           front surface
C RHOSFBD = solar beam-to-diffuse reflectance of the slat blind 
C           front surface
C
C Reference:
C Kotey, N.A., Collins, M.R., Wright, J.L, Jiang, T. (2008)
C 'A Simplified Method for Calculating the Effective Solar Optical
C Properties of a Venetian Blind Layer for Building Energy 
C Simulation', ASME Journal of Solar Energy Engineering, In Review.
C
C ********************************************************************

      SUBROUTINE VB_SOL6(S,W,OMEGA,DE,PHI,RHO35,RHO46,TAU,RHOSFBD,
     &      TAUSFBD)
      IMPLICIT NONE
      REAL S 
      REAL W 
      REAL PHI
      REAL RHO35
      REAL RHO46
      REAL TAU  
      REAL DE 
      REAL OMEGA
      REAL RHOSFBD 
      REAL TAUSFBD

      REAL AB, AE, AF, BC, BD, BF, CD, CE, EF
      REAL F13, F14, F23, F24, F34, F36, F15, F16
      REAL F43, F45, F54, F56, F63, F65, F25, F26
      REAL Z3, Z4
      REAL J3, J4, J5, J6
      INTEGER N
      REAL A(4,6)
      REAL XSOL(4)
      REAL PI, SMALL_ERROR
      PARAMETER(PI = 3.14159265359, SMALL_ERROR = 0.000001)
      
!-----------------------------------------------------------------------------------

!  CHECK TO SEE WHICH SIDE OF SLAT IS SUNLIT
      IF((PHI + OMEGA).GE.0.0) THEN   ! SUN SHINES ON TOP OF SLAT
        Z3 = TAU*S/DE
        Z4 = RHO46*S/DE
      ELSE  ! SUN SHINES ON BOTTOM OF SLAT      
        Z3 = RHO35*S/DE
        Z4 = TAU*S/DE
      ENDIF
!-----------------------------------------------------------------------------------
      
!  CHECK TO SEE IF VENETIAN BLIND IS CLOSED
      IF( ABS(PHI - (PI/2.0)).LT.SMALL_ERROR ) THEN  !VENETIAN BLIND IS CLOSED

! CHECK TO SEE IF THERE ARE GAPS IN BETWEEN SLATS WHEN THE BLIND IS CLOSED
        IF(W .LT. S) THEN    !YES, THERE ARE GAPS IN BETWEEN SLATS
          RHOSFBD = (W/S)*RHO46
          TAUSFBD = (W/S)*TAU
        ELSE        ! NO, THERE ARE NO GAPS IN BETWEEN SLATS
          RHOSFBD = RHO46
          TAUSFBD = TAU
        ENDIF       ! END OF CHECK FOR GAPS IN BETWEEN SLATS

      ELSE        !VENETIAN BLIND IS OPENED
        AB = DE
        AF = SQRT ((W*COS(PHI))**2   +   (S - W*SIN(PHI))**2)
        BC = W - AB
        EF = BC
        BD = SQRT ((DE*COS(PHI))**2   +  (S + DE*SIN(PHI))**2)
        BF = SQRT ((EF*COS(PHI))**2   +  (S - EF*SIN(PHI))**2)
        CD = SQRT ((W*COS(PHI))**2    +  (S + W*SIN(PHI))**2)
        CE = SQRT ((EF*COS(PHI))**2   +  (S + EF*SIN(PHI))**2)
        AE = SQRT ((DE*COS(PHI))**2   +  (S - DE*SIN(PHI))**2)

        F13 = (S + AB - BD) / (2.0*S)
        F14 = (S + DE - AE) / (2.0*S)
        F15 = (W + BD - (AB + CD)) / (2.0*S)
        F16 = (W + AE - (AF + DE)) / (2.0*S)
        F23 = (W + BF - (BC + AF)) / (2.0*S)
        F24 = (W + CE - (CD + EF)) / (2.0*S)
        F25 = (S + BC - BF) / (2.0*S)
        F26 = (S + EF - CE) / (2.0*S)
        F34 = (AE + BD - 2.0*S) / (2.0*AB)
        F36 = (AF + S - (AE + BF)) / (2.0*AB)
        F43 = (AE + BD - 2.0*S) / (2.0*DE)
        F45 = (CD + S - (BD + CE)) / (2.0*DE)
        F54 = (CD + S - (BD + CE)) / (2.0*BC)
        F56 = (CE + BF - 2.0*S) / (2.0*BC)
        F63 = (AF + S - (AE + BF)) / (2.0*EF)
        F65 = (BF + CE - 2.0*S) / (2.0*EF)

        N = 4

        A(N,N+2) = 0.0    ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
        XSOL(N) = 0.0   ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 

! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX

        A(1,1) = 1.0 - TAU*F43
        A(1,2) = -RHO35*F34
        A(1,3) = -TAU*F45
        A(1,4) = -RHO35*F36
        A(1,5) = Z3
        A(2,1) = -RHO46*F43
        A(2,2) = 1.0 - TAU*F34
        A(2,3) = -RHO46*F45
        A(2,4) = -TAU*F36
        A(2,5) = Z4
        A(3,1) = -TAU*F63
        A(3,2) = -RHO35*F54
        A(3,3) = 1.0 - TAU*F65
        A(3,4) = -RHO35*F56
        A(3,5) = 0.0
        A(4,1) = -RHO46*F63
        A(4,2) = -TAU*F54
        A(4,3) = -RHO46*F65
        A(4,4) = 1.0 - TAU*F56
        A(4,5) = 0.0


        CALL SOLMATS(N,A,XSOL)

        J3 = XSOL(1)
        J4 = XSOL(2)
        J5 = XSOL(3)
        J6 = XSOL(4)

        RHOSFBD = F13*J3 + F14*J4 + F15*J5 + F16*J6
        TAUSFBD = F23*J3 + F24*J4 + F25*J5 + F26*J6
      ENDIF! END OF CHECK FOR CLOSED BLIND
!-----------------------------------------------------------------------------------

!  FINAL CHECK FOR NEGATIVE VALUES
      IF(RHOSFBD.LT.0.0)RHOSFBD=0.0
      IF(TAUSFBD.LT.0.0)TAUSFBD=0.0

      RETURN
      END


C ********************************************************************
C                          --VB_SOL_DD_THICK--
C 
C Four surface flat-slat model with slat transmittance.
C Thickness correction using EnergyPlus correction factor.
C Details of EnergyPlus thickness correction found in:
C EnergyPlus Engineering Reference (2008), US DOE
C
C Calculates the effective diffuse-to-diffuse solar optical propertis
C of a slat-type blind layer.
C Returns the transmittance and front-side reflectance of the blind
C layer. If you want the back-side reflectance call the subroutine a 
C second time with the same input data - except negate the slat angle,
C PHI_RAD.
C
C INPUT:
C S = slat spacing (any unit for lenght but the same as used for W)
C W = slat width, tip-to-tip (any unit for length but same as S)
C T = slat thickness (any unit but same as s and w)
C PHI_RAD = slat angle (in radians) is positive when the front-side
C           slat tip is low
C OHM_RAD = solar profile angle (radians) 
C           ( must be -90*PI/180 < OHM_RAD < 90*PI/180 )
C RHOUFS_SLAT = solar reflectance of upward-facing slat surfaces
C RHODFS_SLAT = solar reflectance of downward-facing slat surfaces
C TAU_SLAT = solar transmittance of slat 
C ALL SOLAR SLAT PROPERTIES - INCIDENT TO DIFFUSE
C
C RETURNED:
C TAUSFDD = solar diffuse-to-diffuse transmittance of the slat blind 
C           front surface
C RHOSFDD = solar diffuse-to-diffuse reflectance of the slat blind 
C           front surface
C
C Reference:
C Kotey, N.A., Collins, M.R., Wright, J.L, Jiang, T. (2008)
C 'A Simplified Method for Calculating the Effective Solar Optical
C Properties of a Venetian Blind Layer for Building Energy 
C Simulation', ASME Journal of Solar Energy Engineering, In Review.
C
C ********************************************************************
      SUBROUTINE VB_SOL_DD_THICK(S, W, T, PHI_RAD, RHOUFS_SLAT,
     &      RHODFS_SLAT, TAU_SLAT, RHOSFDD, TAUSDD)
      IMPLICIT NONE

C INPUT VARIABLES
      REAL S 
      REAL W 
      REAL T 
      REAL PHI_RAD 
      REAL RHOUFS_SLAT 
      REAL RHODFS_SLAT 
      REAL TAU_SLAT 

C OUTPUT VARIABLES
      REAL RHOSFDD
      REAL TAUSDD

      REAL CD, AF,FEDGE_DIFF, H, PHI
      REAL F13, F14, F12, F31, F41, FSS 
      REAL C3, B3, C4, B4, K3, K4 
      REAL DEN  
      REAL PI
      PARAMETER (PI = 3.14159265359)

!-----------------------------------------------------------------------------------
      !  CHECK TO SEE IF SLAT ANGLE IS EQUAL TO OR GREATER THAN 89.5 DEGREES
      !  AND SET THE SLAT ANGLE TO 89.5 DEGREES 
      !  (THIS IS DONE FOR BOTH POSITIVE AND NEGATIVE SLAT ANGLES)
      IF((PHI_RAD.ge.(89.5*PI/180.0)).OR.
     &   (PHI_RAD.le.(-89.5*PI/180.0)))THEN
        IF (PHI_RAD.lt.0.0) THEN
          PHI = -89.5*PI/180.0
        ELSE
          PHI = 89.5*PI/180.0
        ENDIF
      ELSE
        PHI = PHI_RAD  ! USUAL ACTION
      ENDIF
!-----------------------------------------------------------------------------------

      CD = SQRT ((W*COS(PHI))**2   +   (S + W*SIN(PHI))**2)
      AF = SQRT ((W*COS(PHI))**2   +   (S - W*SIN(PHI))**2)

      F13 = (W+S-CD)/(2.*S)       ! SHAPE FACTOR FRONT OPENING TO TOP SLAT
      F14 = (W+S-AF)/(2.*S)       ! SHAPE FACTOR FRONT OPENING TO BOTTOM SLAT
      FSS = 1.0 - (S/W)*(F13+F14) ! SLAT-TO-SLAT SHAPE FACTOR
      F31 = (S/W)*F13
      F41 = (S/W)*F14
      F12 = 1.0 - F13 - F14   !FRONT OPENING TO BACK OPENING SHAPE FACTOR

      DEN = 1.0 - (TAU_SLAT*FSS)         ! DENOMINATOR - USED FOUR TIMES
      C3 = (RHODFS_SLAT*F31 + TAU_SLAT*F41)/DEN
      B3 = (RHODFS_SLAT*FSS)/DEN
      C4 = (RHOUFS_SLAT*F41 + TAU_SLAT*F31)/DEN
      B4 = (RHOUFS_SLAT*FSS)/DEN

      K3 = (C3 + (B3*C4))/(1.0 - (B3*B4))
      K4 = (C4 + (B4*C3))/(1.0 - (B3*B4))

      H=(S+T)-(T/COS(PHI))
      IF(H.LE.0.001)H=0.00 !SLATS ARE TOUCHING

      FEDGE_DIFF = ABS((ABS(COS(PHI))-
     &      ABS(SIN(PHI)))/(H/T + 1./ABS(COS(PHI))))

      TAUSDD = (1.-FEDGE_DIFF)*(F12 + (F14*K3) + (F13*K4)) 
      
      IF (TAUSDD.LT.0.0) TAUSDD = 0.0

      RHOSFDD =   (1.-FEDGE_DIFF)*((F13*K3) + (F14*K4)) + 
     &            (FEDGE_DIFF)*((RHOUFS_SLAT+RHODFS_SLAT)/2.0) 

      IF(RHOSFDD.LT.0.0) RHOSFDD = 0.0

      RETURN
      END

!******************************************************************************
! IS (insect screen) models
!******************************************************************************

!****************************************************************************
!  SUBROUTINE: IS_DIFF (Insect screen diffuse properties)
!  Calculates insect screen diffuse-diffuse solar optical properties by integrating  
!  the corresponding properties over the hemisphere
!****************************************************************************
      SUBROUTINE IS_DIFF( RHO_BT0, TAU_BT0, TAU_BB0, ltyp, 
     &RHO_DD, TAU_DD) 
      IMPLICIT NONE
      REAL RHO_BT0      ! normal incidence beam-total reflectance
      REAL TAU_BT0      ! normal incidence beam-total transmittance
                                                      !   TAU_BT0 = TAU_BB0 + TAU_BD0
      REAL TAU_BB0      ! normal incidence beam-beam transmittance      
      REAL RHO_DD      ! returned: diffuse-diffuse reflectance
      REAL TAU_DD      ! returned: diffuse-diffuse transmittance
      REAL P(3)
      REAL IN_HEMINT
      INTEGER ltyp
      
      P(1) = RHO_BT0
      P(2) = TAU_BT0
      P(3) = TAU_BB0
      
      CALL HEMINT(ltyp, 1, P, IN_HEMINT)
            RHO_DD = IN_HEMINT
            
      CALL HEMINT(ltyp, 2, P, IN_HEMINT)
            TAU_DD = IN_HEMINT

      IF (RHO_DD + TAU_DD > 1.) THEN
            IF (RHO_DD + TAU_DD > 1.10) THEN
      WRITE( *,'("IS_DIFF inconsistent properties: TAU=",F6.4,
     &"   RHO=",F6.4)') 
     &RHO_DD, TAU_DD
            END IF
            TAU_DD = 1. - RHO_DD
      END IF

      END SUBROUTINE IS_DIFF

!-------------------------------------------------------------------------------

      SUBROUTINE IS_F(THETA, OPT, P, OUT_IS_F)            !insect screen integrand

      IMPLICIT NONE
            
      REAL THETA            ! incidence angle, radians
      INTEGER OPT            ! options (1=reflectance, 2=transmittance)
      REAL P(3)      ! parameters
      REAL RHO_BD, TAU_BB, TAU_BD
      REAL OUT_IS_F
      
      CALL IS_BEAM( THETA, P(1), P(2), P(3), RHO_BD, TAU_BB, TAU_BD)
      
      IF (OPT == 1) THEN
            OUT_IS_F = RHO_BD
      ELSE IF (OPT == 2) THEN
            OUT_IS_F = TAU_BB + TAU_BD
      ELSE
            OUT_IS_F = -1.
      ENDIF

      END SUBROUTINE IS_F

!****************************************************************************
! SUBROUTINE: IS_BEAM (Insect screen beam solar-optical properties)
!      
! Calculates insect screen off-normal solar optical properties
!   using semi-empirical relations.
!****************************************************************************
      SUBROUTINE IS_BEAM(xTHETA, RHO_BT0, TAU_BT0, TAU_BB0, RHO_BD, 
     &TAU_BB, TAU_BD)
      
      IMPLICIT NONE
      
      REAL xTHETA            ! incidence angle, radians (0 - PI/2)
      REAL RHO_BT0      ! beam-total reflectance
      REAL TAU_BT0      ! beam-total transmittance at normal incidence
                                                !   TAU_BTO = TAU_BB0 + TAU_BD0
      REAL TAU_BB0      ! beam-beam transmittance at normal incidence
      REAL RHO_BD            ! returned: beam-diffuse reflectance      
      REAL TAU_BB            ! returned: beam-beam transmittance
      REAL TAU_BD            ! returned: beam-diffuse transmittance

      REAL THETA                  ! working incident angle, radians
      REAL COSTHETA            ! cosine( theta)
      REAL THETA_CUTOFF      ! cutoff angle, radians (beyond which TAU_BB = 0)
      REAL B                        ! working temp
      REAL RHO_W                  ! apparent wire reflectance
      REAL RHO_BT90            ! beam-total reflectance at 90 deg incidence
      REAL TAU_BT                  ! beam-total transmittance
      REAL IS_DSRATIO_IN      
      
      REAL PI, SMALL_ERROR, PIOVER2, DTOR, RTOD
      PARAMETER (PI = 3.14159265359, SMALL_ERROR = 0.000001)
      PARAMETER (PIOVER2 = PI/2., DTOR = PI / 180., RTOD = 180./ PI)

      THETA = MIN( 89.99*DTOR, xTHETA)
      COSTHETA = COS( THETA)

      RHO_W = RHO_BT0/MAX(.00001, 1.-TAU_BB0)
      B = -.45 * LOG( MAX( RHO_W, .01))

      RHO_BT90 = RHO_BT0 + (1. - RHO_BT0)*(.35 * RHO_W)

!     RHO_BD = P01( RHO_BT0 + (RHO_BT90 - RHO_BT0) * (1.-COSTHETA**B), "IS_BEAM RhoBD")

      RHO_BD = RHO_BT0 + (RHO_BT90 - RHO_BT0) * (1.-COSTHETA**B)

      IF (TAU_BT0 < .00001) THEN
            TAU_BB = 0.
            TAU_BT = 0.
      ELSE
            
            CALL IS_DSRATIO(TAU_BB0,IS_DSRATIO_IN)
            
            THETA_CUTOFF = ACOS(IS_DSRATIO_IN)

            IF (THETA >= THETA_CUTOFF) THEN
                  TAU_BB = 0.
            ELSE
                  B = -.45*LOG( MAX( TAU_BB0, .01)) + .1
                  
            !      TAU_BB = P01( TAU_BB0 * COS( PIOVER2*THETA/THETA_CUTOFF)**B, "IS_BEAM TauBB")
                  TAU_BB = TAU_BB0 * COS( PIOVER2*THETA/THETA_CUTOFF)**B
            
            END IF

            B = -.65 * LOG( MAX( TAU_BT0, .01)) + .1

      !      TAU_BT = P01( TAU_BT0 * COSTHETA**B, "IS_BEAM TauBT")
            TAU_BT = TAU_BT0 * COSTHETA**B
            
      END IF

!      TAU_BD = P01( TAU_BT-TAU_BB, "IS_BEAM TauBD")

      TAU_BD = TAU_BT-TAU_BB

      END SUBROUTINE IS_BEAM
!------------------------------------------------------------------------------

      SUBROUTINE IS_OPENNESS(D, S, IS_OPENNESS_OUT)            ! openness from wire geometry
! returns openness (0 - 1)
      IMPLICIT NONE
      REAL D                  ! wire diameter
      REAL S                  ! wire spacing (note D-S = distance between wires)
      REAL IS_OPENNESS_OUT
      
      IF (S > 0.) THEN
      
            IS_OPENNESS_OUT = (MAX( S-D, 0.) / S)**2
            
      ELSE
      
            IS_OPENNESS_OUT = 0.
            
      END IF

      END SUBROUTINE IS_OPENNESS
      
!------------------------------------------------------------------------------

      SUBROUTINE IS_DSRATIO(OPENNESS, IS_DSRATIO_OUT)            ! wire geometry from openness
! returns diameter / spacing ratio
      IMPLICIT NONE
      REAL OPENNESS      ! openness
      REAL IS_DSRATIO_OUT
      REAL IS_OPENNESS_IN
      
      IF (OPENNESS > 0.) THEN
            IS_DSRATIO_OUT = 1. - MIN( SQRT( OPENNESS), 1.)
      ELSE
            IS_DSRATIO_OUT = 0.
      END IF

      CALL IS_OPENNESS(IS_DSRATIO_OUT, 1., IS_OPENNESS_IN)
      
      IF (ABS(IS_OPENNESS_IN - OPENNESS) > .00001) THEN
            WRITE (*, "('IS_DSDATIO openness mismatch')")
      END IF

      END SUBROUTINE IS_DSRATIO

!==============================================================================        
        
!******************************************************************************
! RB (roller blind) models
!******************************************************************************

!****************************************************************************
!  SUBROUTINE: RB_DIFF
!  Calculates roller blind diffuse-diffuse solar optical properties by integrating  
!  the corresponding properties over the hemisphere
!****************************************************************************
      SUBROUTINE RB_DIFF( RHO_BT0, TAU_BT0, TAU_BB0, ltyp, 
     &RHO_DD, TAU_DD)
      
      IMPLICIT NONE
      
      REAL RHO_BT0      ! normal incidence beam-total reflectance
      REAL TAU_BT0      ! normal incidence beam-total transmittance
                                                      !   TAUFF_BT0 = TAUFF_BB0 + TAUFF_BD0
      REAL TAU_BB0      ! normal incidence beam-beam transmittance      
      REAL RHO_DD      ! returned: diffuse-diffuse reflectance
      REAL TAU_DD      ! returned: diffuse-diffuse transmittance
      REAL P(3)
      REAL IN_HEMINT
      INTEGER ltyp
      
      RHO_DD = RHO_BT0

      P(1) = RHO_BT0
      P(2) = TAU_BT0
      P(3) = TAU_BB0

      CALL HEMINT(ltyp, 0, P, IN_HEMINT)
      
      TAU_DD = IN_HEMINT
      
      IF (RHO_DD + TAU_DD > 1.) THEN
!#if defined( _DEBUG)
        
            IF (RHO_DD + TAU_DD > 1.10) THEN
               WRITE( *, '("RB_DIFF inconsistent properties: TAU=",F6.4,
     &  "   RHO=", F6.4)') RHO_DD, TAU_DD
      
            END IF
!#endif
            TAU_DD = 1. - RHO_DD
            
      END IF

      END SUBROUTINE RB_DIFF
!----------------------------------------------------------------------------

      SUBROUTINE RB_F(THETA, OPT, P, OUT_RB_F)            ! roller blind integrand

      IMPLICIT NONE

      REAL THETA            ! incidence angle, radians
      INTEGER OPT            ! options (unused)
      REAL P(3)            ! parameters
      REAL RHO_BD, TAU_BB, TAU_BD            
      REAL OUT_RB_F

      CALL RB_BEAM(THETA, P(1), P(2), P(3), RHO_BD, TAU_BB, TAU_BD)
            
      OUT_RB_F = TAU_BB + TAU_BD

      END SUBROUTINE RB_F

!****************************************************************************
!  SUBROUTINE: RB_BEAM (Roller blind off-normal properties)
!      
!  Calculates the roller blind off-normal properties using
!     semi-empirical relations 
!****************************************************************************
      SUBROUTINE RB_BEAM(xTHETA, RHO_BT0, TAU_BT0, TAU_BB0, 
     &RHO_BD,TAU_BB, TAU_BD)
      
      IMPLICIT NONE

      REAL xTHETA            ! angle of incidence, radians (0 - PI/2)
      REAL RHO_BT0      ! normal incidence beam-total front reflectance
      REAL TAU_BT0      ! normal incidence beam-total transmittance
                                                      !   TAU_BT0 = TAU_BB0 + TAU_BD0
      REAL TAU_BB0      ! normal incidence beam-beam transmittance
                                                      !   (openness)
      REAL RHO_BD      ! returned: beam-diffuse front reflectance
      REAL TAU_BB      ! returned: beam-beam transmittance
      REAL TAU_BD      ! returned: beam-diffuse transmittance

      REAL THETA                  ! working angle of incidence (limited < 90 deg)
      REAL TAUM0                  ! apparent blind material transmittance at normal incidence
      REAL THETA_CUTOFF      ! cutoff angle, radians (angle beyond which total transmittance goes to zero)
      REAL TAUBT_EXPO            ! exponent in the beam-total transmittance model
      REAL TAUBB_EXPO            ! exponent in the beam-beam transmittance model
      REAL TAU_BT                  ! beam-total transmittance

      REAL PI, SMALL_ERROR, PIOVER2, DTOR, RTOD
      PARAMETER (PI = 3.14159265359, SMALL_ERROR = 0.000001)
      PARAMETER (PIOVER2 = PI/2., DTOR = PI / 180., RTOD = 180./ PI)
      
      THETA = MIN( 89.99*DTOR, xTHETA)

      IF (TAU_BB0 > .9999) THEN
            TAU_BB = 1.
            TAU_BT = 1.
      ELSE
            ! beam total
            TAUM0 = MIN( 1., (TAU_BT0 - TAU_BB0) / (1.0-TAU_BB0))
            IF (TAUM0 <= 0.33) THEN
                  TAUBT_EXPO = .133 * (TAUM0 + .003)**(-.467)
            ELSE
                  TAUBT_EXPO = .33 * (1. - TAUM0)
            ENDIF
            TAU_BT = TAU_BT0 * COS( THETA)**TAUBT_EXPO      ! always 0 - 1

            THETA_CUTOFF = DTOR*(90. - 25. * COS( TAU_BB0 * PIOVER2))
            IF (THETA >= THETA_CUTOFF) THEN
                  TAU_BB = 0.
            ELSE
             TAUBB_EXPO = .6 * COS( TAU_BB0 * PIOVER2)**.3
         TAU_BB = TAU_BB0 * COS( PIOVER2*THETA/THETA_CUTOFF)**TAUBB_EXPO
                  ! BB correlation can produce results slightly larger than BT
                  ! Enforce consistency
             TAU_BB = MIN( TAU_BT, TAU_BB)
            END IF
      END IF

      RHO_BD = RHO_BT0
!      TAU_BD = P01( TAU_BT-TAU_BB, "RB_BEAM TauBD")
      TAU_BD = TAU_BT-TAU_BB

      END SUBROUTINE RB_BEAM        
        
!******************************************************************************
! PD (pleated drape) models and associated FM (fabric) methods
!****************************************************************************

!****************************************************************************
!  SUBROUTINE: FM_DIFF (Fabric diffuse properties)
!  Calculates fabric diffuse-diffuse solar optical properties by integrating  
!    the corresponding beam properties over the hemisphere
!****************************************************************************
      SUBROUTINE FM_DIFF(RHO_BT0, TAU_BT0, TAU_BB0, ltyp, 
     &RHO_DD, TAU_DD) 

      IMPLICIT NONE
      
      REAL RHO_BT0      ! fabric beam-total reflectance at normal incidence    
      REAL TAU_BT0      ! fabric beam-total transmittance at normal incidence
                                                      !   (TAU_BT0 = TAU_BB0 + TAU_BD0)
      REAL TAU_BB0      ! forward facing fabric beam-beam transmittance at normal incidence      
      REAL RHO_DD            ! returned: fabric diffuse-diffuse reflectance
      REAL TAU_DD            ! returned: fabric diffuse-diffuse transmittance
      REAL TAU_BD0, P(3)       
      REAL IN_HEMINT
      INTEGER ltyp, hipRHO, hipTAU
      
      hipRHO = 1
      hipTAU = 2

      TAU_BD0 = TAU_BT0 - TAU_BB0
   
      P(1) = RHO_BT0
      P(2) = TAU_BT0
      P(3) = TAU_BB0

C      RHO_DD = HEMINT(hipRHO, P, IN_HEMINT)
C      TAU_DD = HEMINT(hipTAU, P, IN_HEMINT)

      CALL HEMINT(ltyp, hipRHO, P, IN_HEMINT)
      RHO_DD = IN_HEMINT
      
      CALL HEMINT(ltyp, hipTAU, P, IN_HEMINT)
      TAU_DD = IN_HEMINT      
      
      IF (RHO_DD + TAU_DD > 1.) THEN
            IF (RHO_DD + TAU_DD > 1.10) THEN
              WRITE( *, '("FM_DIFF inconsistent properties: TAU=",F6.4,"   
     &            RHO=",F6.4)') RHO_DD, TAU_DD
            END IF
            TAU_DD = 1. - RHO_DD
      END IF
      
      END SUBROUTINE FM_DIFF
      
!-------------------------------------------------------------------------------

      SUBROUTINE FM_F( THETA, Opt, P, OUT_FM_F)            ! fabric property integrand

      IMPLICIT NONE
      REAL THETA            ! incidence angle, radians
      INTEGER Opt            ! options (hipRHO, hipTAU)
!      INTEGER hipDIM
      REAL P(3)  !P( hipDIM)      ! parameters
      REAL RHO_BD, TAU_BB, TAU_BD
      REAL OUT_FM_F
      
!      hipDIM = 3
            
!            write(96,*) 'FM_BEAM CALLED FROM FM_F START', THETA
            
          CALL FM_BEAM( THETA, P(1), P(2), P(3), RHO_BD, TAU_BB, TAU_BD)

            IF (OPT == 1) THEN
                  OUT_FM_F = RHO_BD
            ELSE IF (OPT == 2) THEN
                  OUT_FM_F = TAU_BB+TAU_BD
            ELSE
                  OUT_FM_F = -1.
            ENDIF
            
      END SUBROUTINE FM_F

!**********************************************************************************************************************
!  SUBROUTINE: FM_BEAM
!
!  Calculates the solar optical properties of a fabric for beam radiation incident on the forward facing surface
!  using optical properties at normal incidence and semi-empirical relations.
!  If you want the solar optical properties for the backward facing surface, call the subroutine again and supply it
!     with the corresponding backward facing surface optical properties at normal incidence.
!**********************************************************************************************************************
      SUBROUTINE FM_BEAM( xTHETA, RHO_BT0, TAU_BT0, TAU_BB0, RHO_BD, 
     &      TAU_BB, TAU_BD)
       
      IMPLICIT NONE
      
      REAL xTHETA            ! incidence angle, radians (0 - PI/2)
      REAL RHO_BT0      ! fabric beam-total reflectance
      REAL TAU_BT0      ! fabric beam-total transmittance at normal incidence
                              !   TAU_BTO = TAU_BB0 + TAU_BD0
      REAL TAU_BB0      ! fabric beam-beam transmittance at normal incidence
                              !   = openness
      REAL RHO_BD      ! returned: fabric beam-diffuse reflectance      
      REAL TAU_BB      ! returned: fabric beam-beam transmittance
      REAL TAU_BD      ! returned: fabric beam-diffuse transmittance

      REAL THETA            ! working incident angle, radians
      REAL COSTHETA      ! cosine( theta)
      REAL R, B            ! working temps
      REAL RHO_Y            ! apparent yarn reflectance
      REAL RHO_BT90      ! beam-total reflectance at 90 deg incidence
      REAL TAU_BT            ! beam-total transmittance

      REAL PI, SMALL_ERROR, PIOVER2, DTOR, RTOD
      PARAMETER (PI = 3.14159265359, SMALL_ERROR = 0.000001)
      PARAMETER (PIOVER2 = PI/2., DTOR = PI / 180., RTOD = 180./ PI)
      
      THETA = ABS( MAX( -89.99*DTOR, MIN( 89.99*DTOR, xTHETA)))
                              ! limit -89.99 - +89.99
                              ! by symmetry, optical properties same at +/- theta
                              
      COSTHETA = COS( THETA)
      
      RHO_Y = RHO_BT0/MAX( .00001, 1.0-TAU_BB0)
      R = 0.7 * RHO_Y**.7
      RHO_BT90 = RHO_BT0 + (1. - RHO_BT0)*R
      B = 0.6
      !RHO_BD = P01( RHO_BT0 + (RHO_BT90-RHO_BT0) * (1.0-COSTHETA**B), "FM_BEAM RhoBD")

      RHO_BD = RHO_BT0 + (RHO_BT90-RHO_BT0) * (1.0-COSTHETA**B)
      
      IF (RHO_BD < 0.) RHO_BD = 0.
      
      IF (TAU_BT0 < .00001) THEN
      
            TAU_BB = 0.
            TAU_BD = 0.
            
      ELSE
      
            B = MAX( -.5 * LOG( MAX( TAU_BB0, .01)), .35)
            TAU_BB = TAU_BB0 * COSTHETA**B
            
            B = MAX( -.5 * LOG( MAX( TAU_BT0, .01)), .35)
            TAU_BT = TAU_BT0 * COSTHETA**B

      !      TAU_BD = P01( TAU_BT - TAU_BB, "FM_BEAM TauBD")
      
            TAU_BD = TAU_BT - TAU_BB
      
            IF (TAU_BD < 0.) TAU_BD = 0.
      
      END IF
      
      END SUBROUTINE FM_BEAM

!****************************************************************************
!  SUBROUTINE: PD_DIFF (Eight surface flat-fabric model with rectangular enclosure)
!
!  Calculates the effective diffuse properties of a drapery layer.
!    Used for both LW and solar diffuse.
!  Returns the transmittance and the front-side reflectance of the drapery layer.
!  If you want the back-side reflectance call the subroutine a second time 
!     with reversed front and back properties
!****************************************************************************

      SUBROUTINE PD_DIFF( S, W, RHOFF_DD, RHOBF_DD, TAUF_DD, 
     &RHOFDD, TAUFDD) 
      IMPLICIT NONE
      REAL S                        ! pleat spacing (> 0)
      REAL W                        ! pleat depth (>=0, same units as S)
      REAL RHOFF_DD            ! fabric front diffuse-diffuse reflectance
      REAL RHOBF_DD            ! fabric back diffuse-diffuse reflectance
      REAL TAUF_DD            ! fabric diffuse-diffuse transmittance
      REAL RHOFDD            ! returned: drape diffuse-diffuse reflectance
      REAL TAUFDD            ! returned: drape diffuse-diffuse transmittance

      REAL AK, CG                                    ! length of diagonal strings of the rectangular enclosure
      REAL F12, F14, F32, F21, F31, F34, F24, F41, F42            ! shape factors      
      REAL F57, F56, F58, F67, F65, F68, F75, F76, F78
      REAL F85, F87, F86
      REAL J1, J2, J4, J7, J6, J8            ! radiosity, surface i
      REAL G1, G3, G5, G7                        ! irradiance, surface i
      INTEGER N 
      PARAMETER (N = 6)

      REAL A( N, N+2)                                                
      REAL XSOL( N)

      REAL SMALL_ERROR
      PARAMETER (SMALL_ERROR = 0.000001)
      
      IF (W/S < SMALL_ERROR) THEN
            ! flat drape (no pleats)
            RHOFDD = RHOFF_DD
            TAUFDD = TAUF_DD
            RETURN
      END IF                                                      

            
! SOLVE FOR DIAGONAL STRINGS AND SHAPE FACTORS 
      
      AK = SQRT (S*S + W*W)
      CG = AK            
      
      F12 = (S+W-AK)/(2.0*S)
      F14 = (S+W-CG)/(2.0*S)
      F32 = F14
      F31 = (AK+CG-2.0*W)/(2.0*S)
      F34 = F12
      F21 = (S+W-AK)/(2.0*W)
      F24 = (AK+CG-2.0*S)/(2.0*W)
      F41 = (S+W-CG)/(2.0*W)
      F42 = F24
      F57 = F31
      F56 = F12
      F58 = F14
      F75 = F31
      F76 = F32
      F78 = F34
      F67 = F41
      F65 = F21
      F68 = F24
      F85 = F41
      F87 = F21
      F86 = F42
      
!      data A/ (N * (N+2)) * 0.0/      ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
!      data XSOL/ N *0.0/            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 
      
      A = 0.0      ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
      XSOL = 0.0      ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 

! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX
      
      A(1,1) = 1.0  
      A(1,2) = -RHOBF_DD*F12
      A(1,3) = -RHOBF_DD*F14
      A(1,4) = 0.0
      A(1,5) = 0.0
      A(1,6) = 0.0
      A(1,7) = TAUF_DD 
      A(2,1) = -RHOBF_DD*F21
      A(2,2) = 1.0
      A(2,3) = -RHOBF_DD*F24
      A(2,4) = -TAUF_DD*F87
      A(2,5) = -TAUF_DD*F86
      A(2,6) = 0.0
      A(2,7) = TAUF_DD*F85 
      A(3,1) = -RHOBF_DD*F41
      A(3,2) = -RHOBF_DD*F42
      A(3,3) = 1.0
      A(3,4) = -TAUF_DD*F67
      A(3,5) = 0.0
      A(3,6) = -TAUF_DD*F68
      A(3,7) = TAUF_DD*F65
      A(4,1) = 0.0
      A(4,2) = 0.0
      A(4,3) = 0.0
      A(4,4) = 1.0
      A(4,5) = -RHOFF_DD*F76
      A(4,6) = -RHOFF_DD*F78
      A(4,7) = RHOFF_DD*F75
      A(5,1) = -TAUF_DD*F41
      A(5,2) = -TAUF_DD*F42
      A(5,3) = 0.0
      A(5,4) = -RHOFF_DD*F67
      A(5,5) = 1.0
      A(5,6) = -RHOFF_DD*F68
      A(5,7) = RHOFF_DD*F65
      A(6,1) = -TAUF_DD*F21
      A(6,2) = 0.0
      A(6,3) = -TAUF_DD*F24
      A(6,4) = -RHOFF_DD*F87
      A(6,5) = -RHOFF_DD*F86
      A(6,6) = 1.0
      A(6,7) = RHOFF_DD*F85

      CALL SOLMATS( N, A, XSOL)

      J1 = XSOL(1)
      J2 = XSOL(2)
      J4 = XSOL(3)
      J7 = XSOL(4)
      J6 = XSOL(5)
      J8 = XSOL(6)

      G1 = F12*J2+F14*J4
      G3 = F32*J2+F31*J1+F34*J4
      G5 = F57*J7+F56*J6+F58*J8
      G7 = F75+F76*J6+F78*J8

      TAUFDD = (G3+TAUF_DD*G7)/2.
      RHOFDD = (RHOFF_DD+TAUF_DD*G1+G5)/2.0
      
      if(TAUFDD < 0.) TAUFDD = 0.
      if(RHOFDD < 0.) RHOFDD = 0.
      
      END SUBROUTINE PD_DIFF

!****************************************************************************
!  SUBROUTINE: PD_BEAM (Pleated drape flat-fabric model with rectangular enclosure)
!
!  purpose:  calculates the effective front-side solar optical properties of a drapery layer.
!            back-side properties can be calculated by calling with interchanged fabric properties
!****************************************************************************

      SUBROUTINE PD_BEAM( S, W, OHM_V_RAD, OHM_H_RAD,
     &      RHOFF_BT0, TAUFF_BB0, TAUFF_BD0, RHOFF_DD, TAUFF_DD,
     &      RHOBF_BT0, TAUBF_BB0, TAUBF_BD0, RHOBF_DD, TAUBF_DD,
     &      RHO_BD, TAU_BB, TAU_BD)

      IMPLICIT NONE
      REAL S                        ! pleat spacing (> 0)
      REAL W                        ! pleat depth (>=0, same units as S)
      REAL OHM_V_RAD            ! vertical profile angle, radians
                                    !   +=above horiz)
      REAL OHM_H_RAD            ! horizontal profile angle, radians
                                    !   +=clockwise when viewed from above)
                              ! fabric properties
      REAL RHOFF_BT0, TAUFF_BB0, TAUFF_BD0, RHOFF_DD, TAUFF_DD ! front (outside)
      REAL RHOBF_BT0, TAUBF_BB0, TAUBF_BD0, RHOBF_DD, TAUBF_DD ! back (inside)
      REAL RHO_BD            ! returned: drape front beam-diffuse reflectance
      REAL TAU_BB            ! returned: drape beam-beam transmittance
      REAL TAU_BD            ! returned: drape beam-diffuse transmittance

      REAL DE ! length of directly illuminated surface on side of pleat that
                 !   is open on front (same units as S and W) 
      REAL EF ! length of pleat side shaded surface (W-DE) (same units as S and W)

      REAL OMEGA_V, OMEGA_H            ! profile angles limited to +/- PI/2
      REAL TAUFF_BT0, TAUBF_BT0
      REAL THETA_PARL_temp, THETA_PERP_temp      ! stores part of the value to be used in THETA_PARL, THETA_PERP
      REAL THETA_PARL, THETA_PERP      ! beam incidence angles on pleat surface parallel / perpendicular
                                          !   to window plane
      REAL RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
      REAL RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
      REAL RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
      REAL RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP
      
      REAL PI, SMALL_ERROR, PIOVER2, DTOR, RTOD
      PARAMETER (PI = 3.14159265359, SMALL_ERROR = 0.000001)
      PARAMETER (PIOVER2 = PI/2., DTOR = PI / 180., RTOD = 180./ PI)
      
      OMEGA_V = ABS( MAX( -89.5*DTOR, MIN( 89.5*DTOR, OHM_V_RAD)))
!      OMEGA_V = 0
      OMEGA_H = ABS( MAX( -89.5*DTOR, MIN( 89.5*DTOR, OHM_H_RAD)))
                                    ! limit profile angles -89.5 - +89.5
                                    ! by symmetry, properties same for +/- profile angle

!      THETA_PARL = ACOS(ABS(COS(ATAN(TAN(OMEGA_V)*COS(OMEGA_H)))*COS(OMEGA_H)))      
!      THETA_PERP = ACOS(ABS(COS(ATAN(TAN(OMEGA_V)*SIN(OMEGA_H)))*SIN(OMEGA_H)))
      
      THETA_PARL_temp = ATAN(TAN(OMEGA_V)*COS(OMEGA_H))
      THETA_PERP_temp = ATAN(TAN(OMEGA_V)*SIN(OMEGA_H))
      
      ! incidence angles on pleat front/back (_PARL) and sides (_PERP)
      THETA_PARL = ACOS(ABS(COS(THETA_PARL_temp)*COS(OMEGA_H)))      
      THETA_PERP = ACOS(ABS(COS(THETA_PERP_temp)*SIN(OMEGA_H)))
 
C TODO: Convert to new format     
c      call add_to_xml_reporting (
c     &         THETA_PARL*RTOD,
c     &         'building/zone_01/envelope/windows/THETA_PARL',
c     &         'units', '(degrees)',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
     
c      call add_to_xml_reporting (
c     &         THETA_PERP*RTOD,
c     &         'building/zone_01/envelope/windows/THETA_PERP',
c     &         'units', '(degrees)',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
     
      ! off-normal fabric properties, front surface
      TAUFF_BT0 = TAUFF_BB0 + TAUFF_BD0
      
      CALL FM_BEAM( THETA_PARL, RHOFF_BT0, TAUFF_BT0, TAUFF_BB0, 
     &      RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL)

c      call add_to_xml_reporting (
c     &         RHOFF_BT0,
c     &         'building/zone_01/envelope/windows/RHOFF_BT0',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
     
c      call add_to_xml_reporting (
c     &         TAUFF_BT0,
c     &         'building/zone_01/envelope/windows/TAUFF_BT0',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
     
c      call add_to_xml_reporting (
c     &         TAUFF_BB0,
c     &         'building/zone_01/envelope/windows/TAUFF_BB0',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )     
        
      IF (W/S < SMALL_ERROR) THEN
            ! flat drape (no pleats) -- return fabric properties
        RHO_BD = RHOFF_BT_PARL
        TAU_BD = TAUFF_BD_PARL
        TAU_BB = TAUFF_BB_PARL
           
        RETURN
      END IF
      
      CALL FM_BEAM( THETA_PERP, RHOFF_BT0, TAUFF_BT0, TAUFF_BB0, 
     &      RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP)

      ! Off-normal fabric properties, back surface
      TAUBF_BT0 = TAUBF_BB0 + TAUBF_BD0

      CALL FM_BEAM( THETA_PARL, RHOBF_BT0, TAUBF_BT0, TAUBF_BB0, 
     &      RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL)

      CALL FM_BEAM( THETA_PERP, RHOBF_BT0, TAUBF_BT0, TAUBF_BB0, 
     &      RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP)

      DE = S * ABS(COS(OMEGA_H) / MAX( .000001, SIN(OMEGA_H)) )
      EF = W-DE

      ! select geometric case
      IF ( DE < W - SMALL_ERROR) THEN
            ! illuminated length less than pleat depth
        IF (DE < EF - SMALL_ERROR) THEN
                  ! illum < shade
                  CALL PD_BEAM_CASE_I( S, W, OMEGA_H, DE,
     &                  RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &                  RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &                  RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &                  RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &                  RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &                  RHO_BD, TAU_BD, TAU_BB)
                  
        ELSE IF (DE <= EF + SMALL_ERROR) THEN
                  ! illum and shade equal
                  CALL PD_BEAM_CASE_II( S, W, OMEGA_H, DE,
     &                  RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &                  RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &                  RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &                  RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &                  RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &                  RHO_BD, TAU_BD, TAU_BB)      
                  
        ELSE
                  ! illum > shade
                  CALL PD_BEAM_CASE_III( S, W, OMEGA_H, DE,
     &                  RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &                  RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &                  RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &                  RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &                  RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &                  RHO_BD, TAU_BD, TAU_BB)     
                  
        END IF
    
      ELSE IF (DE <= W + SMALL_ERROR) THEN
            ! illum length same as pleat depth
        CALL PD_BEAM_CASE_IV( S, W, OMEGA_H, DE,
     &            RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &            RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &            RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &            RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &            RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &            RHO_BD, TAU_BD, TAU_BB)      
                  
      ELSE IF (DE < 9000.*S) THEN
            ! some direct illum on pleat back
        CALL PD_BEAM_CASE_V( S, W, OMEGA_H, DE,
     &            RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &            RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &            RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &            RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &            RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &            RHO_BD, TAU_BD, TAU_BB)      
                  
      ELSE
            ! beam parallel to pleat sides (no direct illum on pleat back)
        CALL PD_BEAM_CASE_VI( S, W, OMEGA_H, DE,
     &            RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &            RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &            RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &            RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &            RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &            RHO_BD, TAU_BD, TAU_BB)      
                  
      ENDIF
C TODO: convert to new format
c      call add_to_xml_reporting (
c     &         RHO_BD,
c     &         'building/zone_01/envelope/windows/RHO_BD',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )
     
c      call add_to_xml_reporting (
c     &         TAU_BB,
c     &         'building/zone_01/envelope/windows/TAU_BB',
c     &         'units', '()',
c    &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )

c      call add_to_xml_reporting (
c     &         TAU_BD+TAU_BB,
c     &         'building/zone_01/envelope/windows/TAU_BT',
c     &         'units', '()',
c     &         'BEAM-DIFF FRONT REFLECTION'
c     &         //' (all zones) ' )     
     
      END SUBROUTINE PD_BEAM

!****************************************************************************
!
!  SUBROUTINE: PD_BEAM_CASE_I (FOURTEEN SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE)
!
!****************************************************************************
      SUBROUTINE PD_BEAM_CASE_I( S, W,  OMEGA_H, DE,
     &      RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &      RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &      RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &      RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &      RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &      RHO_BD, TAU_BD, TAU_BB)      

      IMPLICIT NONE
      
      REAL S                        ! pleat spacing (> 0)
      REAL W                        ! pleat depth (>=0, same units as S)
      REAL OMEGA_H            ! horizontal profile angle, radians
      REAL DE                        ! width of illumination on pleat bottom (same units as S)

                                    ! fabric properties at current (off-normal) incidence
                                    !   _PARL = surface parallel to window (pleat top/bot)
                                    !   _PERP = surface perpendicular to window (pleat side)
      REAL RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
      REAL RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
      REAL RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
      REAL RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP

      REAL RHOFF_DD            ! fabric front diffuse-diffuse reflectance
      REAL RHOBF_DD            ! fabric back diffuse-diffuse reflectance
      REAL TAUFF_DD            ! fabric front diffuse-diffuse transmittance
      REAL TAUBF_DD            ! fabric back diffuse-diffuse transmittance
      REAL RHO_BD            ! returned: drape front beam-diffuse reflectance
      REAL TAU_BD            ! returned: drape front beam-diffuse transmittance
      REAL TAU_BB            ! returned: drape front beam-beam transmittance

      REAL TAUBF_BT_PERP

      REAL AB, GN, NP, GP, NK, PK, BC, AN, AP, AK, BG, BP, CG, BK, CP ! lengths of surfaces and diagonal strings
      REAL CN, Z1_BB,      Z7_BB      ! beam source terms
      REAL Z1_BD, Z2_BD, Z7_BD, Z3_BD, Z9_BD, Z13_BD, Z14_BD      ! diffuse source terms due to incident beam radiation
      ! shape factors
      REAL F12, F13, F14, F16, F17, F21, F25, F26, F27
      REAL F31, F35, F36, F37
      REAL F41, F45, F46, F47, F51, F52, F53, F54, F56, F57, F61                               
      REAL F62, F63, F64, F71, F72, F73, F74
      REAL F89, F810, F811, F812, F813
      REAL F814, F911, F912, F913, F914
      REAL F1011, F1012, F1013, F1014      
      REAL F119, F1110, F1112, F1113, F1114
      REAL F129, F1210, F1211, F139
      REAL F1310, F1311, F149, F1410, F1411
      REAL J1, J2, J3, J4, J6, J7, J9, J10, J11, J12, J13, J14      ! radiosity, surface i
      REAL G1, G5, G8,      G11                                           ! irradiance, surface i
      INTEGER N 
      PARAMETER (N = 12)                        
      REAL A( N, N+2)      ! coefficients of the radiosity equations matrix                                                
      REAL XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)                                                      

!-----------------------------------------------------------------------------------

#if defined( PD_PRINTCASE)
      PRINT *, OMEGA_H/DTOR, "I"
#endif
            
      TAUBF_BT_PERP = TAUBF_BD_PERP + TAUBF_BB_PERP

      AB = DE
      GN = DE
      NP = DE
      GP = 2.0*DE
      NK = W-DE
      PK = W-2.0*DE
      BC = NK
      AN = SQRT(S*S+DE*DE)
      AP = SQRT(S*S+GP*GP)
      AK = SQRT(W*W+S*S)
      BG = AN
      BP = AN
      CG = AK
      BK = SQRT(S*S+BC*BC)
      CP = SQRT(S*S+PK*PK)
      CN = SQRT(S*S+NK*NK)
            
      Z1_BB = TAUFF_BB_PARL
      Z1_BD = TAUFF_BD_PARL
      Z2_BD = Z1_BB*RHOBF_BT_PERP*S/GN
      Z7_BB = TAUFF_BB_PERP*S/DE
      Z7_BD = TAUFF_BD_PERP*S/DE
      Z3_BD = Z7_BB*RHOBF_BT_PERP
      Z9_BD = RHOFF_BT_PERP*S/DE
      Z13_BD = Z7_BB*TAUBF_BT_PERP
      Z14_BD = Z1_BB*TAUBF_BT_PERP*S/GN

      F12 = (S+GN-AN)/(2.0*S)
      F13 = (AN+GP-(GN+AP))/(2.0*S)
      F14 = (AP+W-(GP+AK))/(2.0*S)
      F16 = (W+BG-(AB+CG))/(2.0*S)
      F17 = (S+AB-BG)/(2.0*S)
      F21 = (S+GN-AN)/(2.0*GN)
      F25 = (W+CN-(CG+NK))/(2.0*GN)
      F26 = (CG+S-(BG+CN))/(2.0*GN)
      F27 = (AN+BG-2.0*S)/(2.0*GN)
      F31 = (AN+GP-(GN+AP))/(2.0*NP)
      F35 = (NK+CP-(CN+PK))/(2.0*NP)
      F36 = (CN+BP-(S+CP))/(2.0*NP)
      F37 = (S+AP-(AN+BP))/(2.0*NP)
      F41 = (W+AP-(GP+AK))/(2.0*PK)
      F45 = (S+PK-CP)/(2.0*PK)
      F46 = (CP+BK-(S+BP))/(2.0*PK)
      F47 = (BP+AK-(AP+BK))/(2.0*PK)
      F51 = (AK+CG-2.0*W)/(2.0*S)
      F52 = (W+CN-(CG+NK))/(2.0*S)
      F53 = (NK+CP-(CN+PK))/(2.0*S)
      F54 = (S+PK-CP)/(2.0*S)
      F56 = (S+BC-BK)/(2.0*S)
      F57 = (W+BK-(BC+AK))/(2.0*S)
      F61 = (W+BG-(AB+CG))/(2.0*BC)
      F62 = (S+CG-(BG+CN))/(2.0*BC)
      F63 = (CN+BP-(S+CP))/(2.0*BC)
      F64 = (BK+CP-(S+BP))/(2.0*BC)
      F71 = F21
      F72 = F27
      F73 = F37
      F74 = (BP+AK-(BK+AP))/(2.0*AB)
      F89 = F12
      F810 = F16
      F811 = F51
      F812 = F14
      F813 = F13
      F814 = F12
      F911 = F25
      F912 = F74
      F913 = F73
      F914 = F27
      F1011 = (BC+S-BK)/(2.0*BC)
      F1012 = F64
      F1013 = F63
      F1014 = F62
      F119 = F57
      F1110 = F56
      F1112 = F54
      F1113 = F53
      F1114 = F52
      F129 = F47
      F1210 = F46
      F1211 = F45
      F139 = F37
      F1310 = F36
      F1311 = F35
      F149 = F27
      F1410 = F26
      F1411 = F25

!-----------------------------------------------------------------------------------
!      data A/ (N * (N+2)) * 0.0/      ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
!      data XSOL/ N *0.0/            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 
      
      A = 0.0            ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
      XSOL = 0.0            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 

! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX
      
      A(1,1)      = 1.0
      A(1,2)      = -RHOBF_DD*F12
      A(1,3)      = -RHOBF_DD*F13
      A(1,4)      = -RHOBF_DD*F14
      A(1,5)      = -RHOBF_DD*F16
      A(1,6)      = -RHOBF_DD*F17
      A(1,7)      = 0.0
      A(1,8)      = 0.0 
      A(1,9)      = 0.0
      A(1,10)     = 0.0 
      A(1,11)     = 0.0
      A(1,12)     = 0.0 
      A(1,13)     = Z1_BD
      A(2,1)      = -RHOBF_DD*F21
      A(2,2)      = 1.0
      A(2,3)      = 0.0
      A(2,4)      = 0.0
      A(2,5)      = -RHOBF_DD*F26
      A(2,6)      = -RHOBF_DD*F27
      A(2,7)      = -TAUFF_DD*F149
      A(2,8)      = -TAUFF_DD*F1410
      A(2,9)      = -TAUFF_DD*F1411
      A(2,10)     = 0.0
      A(2,11)     = 0.0
      A(2,12)     = 0.0
      A(2,13)     = Z2_BD
      A(3,1)      = -RHOBF_DD*F31
      A(3,2)      = 0.0
      A(3,3)      = 1.0
      A(3,4)      = 0.0
      A(3,5)      = -RHOBF_DD*F36
      A(3,6)      = -RHOBF_DD*F37
      A(3,7)      = -TAUFF_DD*F139
      A(3,8)      = -TAUFF_DD*F1310
      A(3,9)      = -TAUFF_DD*F1311
      A(3,10)     = 0.0
      A(3,11)     = 0.0
      A(3,12)     = 0.0
      A(3,13)     = Z3_BD
      A(4,1)      = -RHOBF_DD*F41
      A(4,2)      = 0.0
      A(4,3)      = 0.0
      A(4,4)      = 1.0
      A(4,5)      = -RHOBF_DD*F46
      A(4,6)      = -RHOBF_DD*F47
      A(4,7)      = -TAUFF_DD*F129
      A(4,8)      = -TAUFF_DD*F1210
      A(4,9)      = -TAUFF_DD*F1211 
      A(4,10)     = 0.0
      A(4,11)     = 0.0
      A(4,12)     = 0.0
      A(4,13)     = 0.0
      A(5,1)      = -RHOBF_DD*F61
      A(5,2)      = -RHOBF_DD*F62
      A(5,3)      = -RHOBF_DD*F63
      A(5,4)      = -RHOBF_DD*F64
      A(5,5)      = 1.0
      A(5,6)      = 0.0
      A(5,7)      = 0.0
      A(5,8)      = 0.0
      A(5,9)      = -TAUFF_DD*F1011
      A(5,10)     = -TAUFF_DD*F1012
      A(5,11)     = -TAUFF_DD*F1013
      A(5,12)     = -TAUFF_DD*F1014 
      A(5,13)     = 0.0
      A(6,1)      = -RHOBF_DD*F71
      A(6,2)      = -RHOBF_DD*F72
      A(6,3)      = -RHOBF_DD*F73
      A(6,4)      = -RHOBF_DD*F74
      A(6,5)      = 0.0
      A(6,6)      = 1.0
      A(6,7)      = 0.0
      A(6,8)      = 0.0
      A(6,9)      = -TAUFF_DD*F911
      A(6,10)     = -TAUFF_DD*F912
      A(6,11)     = -TAUFF_DD*F913
      A(6,12)     = -TAUFF_DD*F914
      A(6,13)     = Z7_BD
      A(7,1)      = -TAUBF_DD*F71
      A(7,2)      = -TAUBF_DD*F72
      A(7,3)      = -TAUBF_DD*F73
      A(7,4)      = -TAUBF_DD*F74
      A(7,5)      = 0.0
      A(7,6)      = 0.0
      A(7,7)      = 1.0
      A(7,8)      = 0.0
      A(7,9)      = -RHOFF_DD*F911
      A(7,10)     = -RHOFF_DD*F912
      A(7,11)     = -RHOFF_DD*F913
      A(7,12)     = -RHOFF_DD*F914
      A(7,13)     = Z9_BD
      A(8,1)      = -TAUBF_DD*F61
      A(8,2)      = -TAUBF_DD*F62
      A(8,3)      = -TAUBF_DD*F63
      A(8,4)      = -TAUBF_DD*F64
      A(8,5)      = 0.0
      A(8,6)      = 0.0
      A(8,7)      = 0.0
      A(8,8)      = 1.0 
      A(8,9)      = -RHOFF_DD*F1011
      A(8,10)     = -RHOFF_DD*F1012
      A(8,11)     = -RHOFF_DD*F1013
      A(8,12)     = -RHOFF_DD*F1014
      A(8,13)     = 0.0
      A(9,1)      = 0.0
      A(9,2)      = 0.0
      A(9,3)      = 0.0
      A(9,4)      = 0.0
      A(9,5)      = 0.0
      A(9,6)      = 0.0
      A(9,7)      = -RHOFF_DD*F119
      A(9,8)      = -RHOFF_DD*F1110
      A(9,9)      = 1.0
      A(9,10)     = -RHOFF_DD*F1112
      A(9,11)     = -RHOFF_DD*F1113
      A(9,12)     = -RHOFF_DD*F1114
      A(9,13)     = 0.0
      A(10,1)     = -TAUBF_DD*F41
      A(10,2)     = 0.0
      A(10,3)     = 0.0
      A(10,4)     = 0.0
      A(10,5)     = -TAUBF_DD*F46
      A(10,6)     = -TAUBF_DD*F47
      A(10,7)     = -RHOFF_DD*F129
      A(10,8)     = -RHOFF_DD*F1210
      A(10,9)     = -RHOFF_DD*F1211 
      A(10,10)    = 1.0 
      A(10,11)    = 0.0
      A(10,12)    = 0.0
      A(10,13)    = 0.0
      A(11,1)     = -TAUBF_DD*F31
      A(11,2)     = 0.0
      A(11,3)     = 0.0
      A(11,4)     = 0.0
      A(11,5)     = -TAUBF_DD*F36
      A(11,6)     = -TAUBF_DD*F37
      A(11,7)     = -RHOFF_DD*F139
      A(11,8)     = -RHOFF_DD*F1310
      A(11,9)     = -RHOFF_DD*F1311
      A(11,10)    = 0.0
      A(11,11)    = 1.0
      A(11,12)    = 0.0
      A(11,13)    = Z13_BD
      A(12,1)     = -TAUBF_DD*F21
      A(12,2)     = 0.0
      A(12,3)     = 0.0
      A(12,4)     = 0.0
      A(12,5)     = -TAUBF_DD*F26
      A(12,6)     = -TAUBF_DD*F27
      A(12,7)     = -RHOFF_DD*F149
      A(12,8)     = -RHOFF_DD*F1410
      A(12,9)     = -RHOFF_DD*F1411
      A(12,10)    = 0.0
      A(12,11)    = 0.0
      A(12,12)    = 1.0
      A(12,13)    = Z14_BD

      
      CALL SOLMATS( N, A,XSOL)

      J1 = XSOL(1)
      J2 = XSOL(2)
      J3 = XSOL(3)
      J4 = XSOL(4)
      J6 = XSOL(5)
      J7 = XSOL(6)
      J9 = XSOL(7)
      J10 = XSOL(8)
      J11 = XSOL(9)
      J12 = XSOL(10)
      J13 = XSOL(11)
      J14 = XSOL(12)
      
      G1 = F12*J2+F13*J3+F14*J4+F16*J6+F17*J7
      G5 = F56*J6+F57*J7+F51*J1+F52*J2+F53*J3+F54*J4
      G8 = F89*J9+F810*J10+F811*J11+F812*J12+F813*J13+F814*J14
      G11 = F1112*J12+F1113*J13+F1114*J14+F119*J9+F1110*J10                  
      
      TAU_BB = 0.0
      TAU_BD = (G5+TAUFF_DD*G11)/2.0
      RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G8)/2.0
                                                 
      END SUBROUTINE PD_BEAM_CASE_I


!****************************************************************************
!
!  SUBROUTINE: PD_BEAM_CASE_II (TWELVE SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE)
!
!****************************************************************************

      SUBROUTINE PD_BEAM_CASE_II( S, W, OMEGA_H, DE,
     &      RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &      RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &      RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &      RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &      RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &      RHO_BD, TAU_BD, TAU_BB)      

      IMPLICIT NONE
      REAL S                        ! pleat spacing (> 0)
      REAL W                        ! pleat depth (>=0, same units as S)
      REAL OMEGA_H            ! horizontal profile angle, radians
      REAL DE                        ! width of illumination on pleat bottom (same units as S)

                                    ! fabric properties at current (off-normal) incidence
                                    !   _PARL = surface parallel to window (pleat top/bot)
                                    !   _PERP = surface perpendicular to window (pleat side)
      REAL RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
      REAL RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
      REAL RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
      REAL RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP

      REAL RHOFF_DD            ! fabric front diffuse-diffuse reflectance
      REAL RHOBF_DD            ! fabric back diffuse-diffuse reflectance
      REAL TAUFF_DD            ! fabric front diffuse-diffuse transmittance
      REAL TAUBF_DD            ! fabric back diffuse-diffuse transmittance
      REAL RHO_BD            ! returned: drape front beam-diffuse reflectance
      REAL TAU_BD            ! returned: drape front beam-diffuse transmittance
      REAL TAU_BB            ! returned: drape front beam-beam transmittance

      REAL TAUBF_BT_PERP

      REAL AB, GN, NK, BC, AN, AK, BG, CG, BK, CN                  ! lengths of surfaces and diagonal strings                  
      REAL Z1_BD, Z2_BD, Z3_BD, Z6_BD, Z8_BD, Z11_BD, Z12_BD        ! diffuse source terms due to incident beam radiation 
      REAL Z1_BB, Z6_BB      ! beam source terms due to incident beam radiation 
      ! shape factors      
      REAL F12, F13, F15, F16, F21, F25, F26, F31, F35, F36
      REAL F41, F42, F43, F45, F46
      REAL F51, F52, F53, F54, F61, F62, F63                              
      REAL F78, F79, F710, F711, F712, F810, F811, F812, F910, F911      
      REAL F912, F108, F109, F1011, F1012, F118, F119, F1110
      REAL F128, F129, F1210

      REAL J1, J2, J3, J5, J6, J8, J9, J10, J11, J12      ! radiosity, surface i
      REAL G1, G4, G7, G10                              ! irradiance, surface i
      INTEGER N
      PARAMETER (N = 10)
      REAL A( N, N+2)      ! coefficients of the radiosity equations matrix
      REAL XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)
!-----------------------------------------------------------------------------------

#if defined( PD_PRINTCASE)
      PRINT *, OMEGA_H/DTOR, "II"
#endif

      TAUBF_BT_PERP = TAUBF_BD_PERP + TAUBF_BB_PERP

      AB = DE
      GN = DE
      NK = W-DE
      BC = NK
      AN = SQRT(S*S+DE*DE)
      AK = SQRT(W*W+S*S)
      BG = AN
      CG = AK
      BK = SQRT(S*S+BC*BC)
      CN = SQRT(S*S+NK*NK)
            
      Z1_BB = TAUFF_BB_PARL
      Z1_BD = TAUFF_BD_PARL
      Z2_BD = Z1_BB*RHOBF_BT_PERP*S/GN
      Z6_BB = TAUFF_BB_PERP*S/DE
      Z6_BD = TAUFF_BD_PERP*S/DE
      Z3_BD = Z6_BB*RHOBF_BT_PERP
      Z8_BD = RHOFF_BT_PERP*S/DE
      Z11_BD = Z6_BB*TAUBF_BT_PERP
      Z12_BD = Z1_BB*TAUBF_BT_PERP*S/GN

      F12 = (S+GN-AN)/(2.0*S)
      F13 = (W+AN-(GN+AK))/(2.0*S)
      F15 = (W+BG-(AB+CG))/(2.0*S)
      F16 = (S+AB-BG)/(2.0*S)
      F21 = (S+GN-AN)/(2.0*GN)
      F25 = (S+CG-(BG+CN))/(2.0*GN)
      F26 = (AN+BG-2.0*S)/(2.0*GN)
      F31 = (W+AN-(GN+AK))/(2.0*NK)
      F35 = (BK+CN-2.0*S)/(2.0*NK)
      F36 = (S+AK-(AN+BK))/(2.0*NK)
      F41 = (AK+CG-2.0*W)/(2.0*S)
      F42 = (W+CN-(CG+NK))/(2.0*S)
      F43 = (S+NK-CN)/(2.0*S)
      F45 = (S+BC-BK)/(2.0*S)
      F46 = (W+BK-(AK+BC))/(2.0*S)
      F51 = (W+BG-(AB+CG))/(2.0*BC)
      F52 = (S+CG-(BG+CN))/(2.0*BC)
      F53 = (BK+CN-2.0*S)/(2.0*BC)
      F54 = (S+BC-BK)/(2.0*BC)
      F61 = (S+AB-BG)/(2.0*AB)
      F62 = (AN+BG-2.0*S)/(2.0*AB)
      F63 = (S+AK-(AN+BK))/(2.0*AB)
      F78 = F12
      F79 = F13
      F710 = (AK+CG-2.0*W)/(2.0*S)
      F711 = F15
      F712 = F16
      F810 = (W+CN-(CG+NK))/(2.0*S)
      F811 = F25
      F812 = F26
      F910 = (S+NK-CN)/(2.0*NK)
      F911 = F35
      F912 = F36
      F108 = F42
      F109 = F43
      F1011 = F45
      F1012 = F46
      F118 = F52
      F119 = F53
      F1110 = (S+BC-BK)/(2.0*NK)
      F128 = F62
      F129 = F63
      F1210 = (W+BK-(AK+BC))/(2.0*GN)


!-----------------------------------------------------------------------------------

!      data A/ (N * (N+2)) * 0.0/      ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
!      data XSOL/ N *0.0/            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 
      
      A = 0.0            ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
      XSOL = 0.0            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 

! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX
      
      A(1,1)      = 1.0
      A(1,2)      = -RHOBF_DD*F12
      A(1,3)      = -RHOBF_DD*F13
      A(1,4)      = -RHOBF_DD*F15
      A(1,5)      = -RHOBF_DD*F16 
      A(1,6)      = 0.0 
      A(1,7)      = 0.0
      A(1,8)      = 0.0
      A(1,9)      = 0.0
      A(1,10)     = 0.0
      A(1,11)     = Z1_BD
      A(2,1)      = -RHOBF_DD*F21
      A(2,2)      = 1.0
      A(2,3)      = 0.0
      A(2,4)      = -RHOBF_DD*F25
      A(2,5)      = -RHOBF_DD*F26
      A(2,6)      = -TAUFF_DD*F128
      A(2,7)      = -TAUFF_DD*F129
      A(2,8)      = -TAUFF_DD*F1210
      A(2,9)      = 0.0
      A(2,10)     = 0.0 
      A(2,11)     = Z2_BD
      A(3,1)      = -RHOBF_DD*F31
      A(3,2)      = 0.0
      A(3,3)      = 1.0
      A(3,4)      = -RHOBF_DD*F35
      A(3,5)      = -RHOBF_DD*F36
      A(3,6)      = -TAUFF_DD*F118
      A(3,7)      = -TAUFF_DD*F119
      A(3,8)      = -TAUFF_DD*F1110
      A(3,9)      = 0.0
      A(3,10)     = 0.0
      A(3,11)     = Z3_BD
      A(4,1)      = -RHOBF_DD*F51
      A(4,2)      = -RHOBF_DD*F52
      A(4,3)      = -RHOBF_DD*F53
      A(4,4)      = 1.0
      A(4,5)      = 0.0
      A(4,6)      = 0.0
      A(4,7)      = 0.0
      A(4,8)      = -TAUFF_DD*F910
      A(4,9)      = -TAUFF_DD*F911
      A(4,10)     = -TAUFF_DD*F912
      A(4,11)     = 0.0
      A(5,1)      = -RHOBF_DD*F61
      A(5,2)      = -RHOBF_DD*F62
      A(5,3)      = -RHOBF_DD*F63
      A(5,4)      = 0.0 
      A(5,5)      = 1.0
      A(5,6)      = 0.0
      A(5,7)      = 0.0
      A(5,8)      = -TAUFF_DD*F810
      A(5,9)      = -TAUFF_DD*F811
      A(5,10)     = -TAUFF_DD*F812
      A(5,11)     = Z6_BD
      A(6,1)      = -TAUBF_DD*F61
      A(6,2)      = -TAUBF_DD*F62
      A(6,3)      = -TAUBF_DD*F63
      A(6,4)      = 0.0
      A(6,5)      = 0.0
      A(6,6)      = 1.0
      A(6,7)      = 0.0
      A(6,8)      = -RHOFF_DD*F810
      A(6,9)      = -RHOFF_DD*F811
      A(6,10)     = -RHOFF_DD*F812
      A(6,11)     = Z8_BD
      A(7,1)      = -TAUBF_DD*F51
      A(7,2)      = -TAUBF_DD*F52
      A(7,3)      = -TAUBF_DD*F53
      A(7,4)      = 0.0
      A(7,5)      = 0.0
      A(7,6)      = 0.0
      A(7,7)      = 1.0
      A(7,8)      = -RHOFF_DD*F910
      A(7,9)      = -RHOFF_DD*F911
      A(7,10)     = -RHOFF_DD*F912
      A(7,11)     = 0.0
      A(8,1)      = 0.0
      A(8,2)      = 0.0
      A(8,3)      = 0.0
      A(8,4)      = 0.0
      A(8,5)      = 0.0
      A(8,6)      = -RHOFF_DD*F108
      A(8,7)      = -RHOFF_DD*F109
      A(8,8)      = 1.0
      A(8,9)      = -RHOFF_DD*F1011
      A(8,10)     = -RHOFF_DD*F1012
      A(8,11)     = 0.0
      A(9,1)      = -TAUBF_DD*F31
      A(9,2)      = 0.0
      A(9,3)      = 0.0
      A(9,4)      = -TAUBF_DD*F35
      A(9,5)      = -TAUBF_DD*F36
      A(9,6)      = -RHOFF_DD*F118
      A(9,7)      = -RHOFF_DD*F119
      A(9,8)      = -RHOFF_DD*F1110
      A(9,9)      = 1.0 
      A(9,10)     = 0.0
      A(9,11)     = Z11_BD
      A(10,1)     = -TAUBF_DD*F21
      A(10,2)     = 0.0
      A(10,3)     = 0.0
      A(10,4)     = -TAUBF_DD*F25
      A(10,5)     = -TAUBF_DD*F26
      A(10,6)     = -RHOFF_DD*F128
      A(10,7)     = -RHOFF_DD*F129
      A(10,8)     = -RHOFF_DD*F1210
      A(10,9)     = 0.0 
      A(10,10)    = 1.0
      A(10,11)    = Z12_BD
      
      CALL SOLMATS(N,A,XSOL)

      J1 = XSOL(1)
      J2 = XSOL(2)
      J3 = XSOL(3)
      J5 = XSOL(4)
      J6 = XSOL(5)
      J8 = XSOL(6)
      J9 = XSOL(7)
      J10 = XSOL(8)
      J11 = XSOL(9)
      J12 = XSOL(10)
      
      G1 = F12*J2+F13*J3+F15*J5+F16*J6
      G4 = F41*J1+F42*J2+F43*J3+F45*J5+F46*J6
      G7 = F78*J8+F79*J9+F710*J10+F711*J11+F712*J12
      G10 = F108*J8+F109*J9+F1011*J11+F1012*J12
      
      TAU_BB = 0.0
      TAU_BD = (G4+TAUFF_DD*G10)/2.0
      RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G7)/2.0
                                                 
      END SUBROUTINE PD_BEAM_CASE_II
      
!****************************************************************************
!
!  SUBROUTINE: PD_BEAM_CASE_III (TWELVE SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE)
!
!  INTERMEDIATE VARIABLES:
!   F12,    F13,  F15,  F16,   F21, F25, F26, F31, F35, F36, F41, F42, F43, F45, F46 = SHAPE FACTORS 
!   F51, F52, F53, F54,  F61, F62, F63, F78, F79, F710, F711, F712, F810, F811, F812 = SHAPE FACTORS
!   F910, F911, F912, F108, F109, F1011, F1012, F118, F119, F1110, F128, F129, F1210 = SHAPE FACTORS 
!   Z1_BD, Z2_BD, Z6_BD, Z3_BD, Z8_BD, Z11_BD, Z12_BD = DIFFUSE SOURCE TERMS DUE TO INCIDENT BEAM RADIATION ON SURFACES 1, 2, 6, 3, 11 AND 12.
!   Z1_BB, Z6_BB = BEAM SOURCE TERMS DUE TO INCIDENT BEAM RADIATION ON SURFACES 1 AND 6.
!  
!****************************************************************************

      SUBROUTINE PD_BEAM_CASE_III( S, W, OMEGA_H, DE,
     &      RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &      RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &      RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &      RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &      RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &      RHO_BD, TAU_BD, TAU_BB)      

      IMPLICIT NONE
      REAL S                        ! pleat spacing (> 0)
      REAL W                        ! pleat depth (>=0, same units as S)
      REAL OMEGA_H            ! horizontal profile angle, radians
      REAL DE                        ! width of illumination on pleat bottom (same units as S)

                                    ! fabric properties at current (off-normal) incidence
                                    !   _PARL = surface parallel to window (pleat top/bot)
                                    !   _PERP = surface perpendicular to window (pleat side)
      REAL RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
      REAL RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
      REAL RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
      REAL RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP

      REAL RHOFF_DD            ! fabric front diffuse-diffuse reflectance
      REAL RHOBF_DD            ! fabric back diffuse-diffuse reflectance
      REAL TAUFF_DD            ! fabric front diffuse-diffuse transmittance
      REAL TAUBF_DD            ! fabric back diffuse-diffuse transmittance
      REAL RHO_BD            ! returned: drape front beam-diffuse reflectance
      REAL TAU_BD            ! returned: drape front beam-diffuse transmittance
      REAL TAU_BB            ! returned: drape front beam-beam transmittance


      REAL TAUBF_BT_PERP

      REAL AB, GN, NK, BC, AN, AK, BG, CG, BK, CN            ! lengths for surfaces and diagonal strings
      REAL Z1_BB, Z6_BB      ! beam source terms
      REAL Z1_BD, Z2_BD, Z6_BD, Z3_BD, Z8_BD, Z11_BD, Z12_BD ! diffuse source terms
      ! shape factors
      REAL F12, F13, F15, F16, F21, F25, F26, F31, F35, F36
      REAL F41, F42, F43, F45, F46, F51, F52, F53, F54
      REAL F61, F62, F63
      REAL F78, F79, F710, F711, F712, F810, F811, F812
      REAL F910, F911, F912
      REAL F108, F109, F1011, F1012, F118, F119, F1110
      REAL F128, F129, F1210             
      REAL J1, J2, J3, J5, J6, J8, J9, J10, J11, J12      ! radiosity, surface i
      REAL G1, G4, G7, G10                              ! irradiance, surface i
      INTEGER N
      PARAMETER (N = 10)                  
      REAL A( N, N+2)      ! coefficients of the radiosity equations matrix                                                
      REAL XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)                                                      

!-----------------------------------------------------------------------------------

!#if defined( PD_PRINTCASE)
!      PRINT *, OMEGA_H/DTOR, "III"
!#endif
      
      TAUBF_BT_PERP = TAUBF_BD_PERP + TAUBF_BB_PERP

      AB = DE
      GN = DE
      NK = W-DE
      BC = NK
      AN = SQRT(S*S+DE*DE)
      AK = SQRT(W*W+S*S)
      BG = AN
      CG = AK
      BK = SQRT(S*S+BC*BC)
      CN = SQRT(S*S+NK*NK)
            
      Z1_BB = TAUFF_BB_PARL
      Z1_BD = TAUFF_BD_PARL
      Z2_BD = Z1_BB*RHOBF_BT_PERP*S/GN
      Z6_BB = TAUFF_BB_PERP*S/DE
      Z6_BD = TAUFF_BD_PERP*S/DE
      Z3_BD = Z6_BB*RHOBF_BT_PERP
      Z8_BD = RHOFF_BT_PERP*S/DE
      Z11_BD = Z6_BB*TAUBF_BT_PERP
      Z12_BD = Z1_BB*TAUBF_BT_PERP*S/GN

      F12 = (S+GN-AN)/(2.0*S)
      F13 = (W+AN-(GN+AK))/(2.0*S)
      F15 = (W+BG-(AB+CG))/(2.0*S)
      F16 = (S+AB-BG)/(2.0*S)
      F21 = (S+GN-AN)/(2.0*GN)
      F25 = (S+CG-(BG+CN))/(2.0*GN)
      F26 = (AN+BG-2.0*S)/(2.0*GN)
      F31 = (W+AN-(GN+AK))/(2.0*NK)
      F35 = (BK+CN-2.0*S)/(2.0*NK)
      F36 = (S+AK-(AN+BK))/(2.0*NK)
      F41 = (AK+CG-2.0*W)/(2.0*S)
      F42 = (W+CN-(CG+NK))/(2.0*S)
      F43 = (S+NK-CN)/(2.0*S)
      F45 = (S+BC-BK)/(2.0*S)
      F46 = (W+BK-(AK+BC))/(2.0*S)
      F51 = (W+BG-(AB+CG))/(2.0*BC)
      F52 = (S+CG-(BG+CN))/(2.0*BC)
      F53 = (BK+CN-2.0*S)/(2.0*BC)
      F54 = (S+BC-BK)/(2.0*BC)
      F61 = (S+AB-BG)/(2.0*AB)
      F62 = (AN+BG-2.0*S)/(2.0*AB)
      F63 = (S+AK-(AN+BK))/(2.0*AB)
      F78 = F12
      F79 = F13
      F710 = (AK+CG-2.0*W)/(2.0*S)
      F711 = F15
      F712 = F16
      F810 = (W+CN-(CG+NK))/(2.0*S)
      F811 = F25
      F812 = F26
      F910 = (S+NK-CN)/(2.0*NK)
      F911 = F35
      F912 = F36
      F108 = F42
      F109 = F43
      F1011 = F45
      F1012 = F46
      F118 = F52
      F119 = F53
      F1110 = (S+BC-BK)/(2.0*NK)
      F128 = F62
      F129 = F63
      F1210 = (W+BK-(AK+BC))/(2.0*GN)


!-----------------------------------------------------------------------------------

!      data A/ (N * (N+2)) * 0.0/      ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
!      data XSOL/ N *0.0/            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 
      
      A = 0.0            ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
      XSOL = 0.0            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 

      ! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX
            
      A(1,1)      = 1.0
      A(1,2)      = -RHOBF_DD*F12
      A(1,3)      = -RHOBF_DD*F13
      A(1,4)      = -RHOBF_DD*F15
      A(1,5)      = -RHOBF_DD*F16 
      A(1,6)      = 0.0 
      A(1,7)      = 0.0
      A(1,8)      = 0.0
      A(1,9)      = 0.0
      A(1,10)     = 0.0
      A(1,11)     = Z1_BD
      A(2,1)      = -RHOBF_DD*F21
      A(2,2)      = 1.0
      A(2,3)      = 0.0
      A(2,4)      = -RHOBF_DD*F25
      A(2,5)      = -RHOBF_DD*F26
      A(2,6)      = -TAUFF_DD*F128
      A(2,7)      = -TAUFF_DD*F129
      A(2,8)      = -TAUFF_DD*F1210
      A(2,9)      = 0.0
      A(2,10)     = 0.0 
      A(2,11)     = Z2_BD
      A(3,1)      = -RHOBF_DD*F31
      A(3,2)      = 0.0
      A(3,3)      = 1.0
      A(3,4)      = -RHOBF_DD*F35
      A(3,5)      = -RHOBF_DD*F36
      A(3,6)      = -TAUFF_DD*F118
      A(3,7)      = -TAUFF_DD*F119
      A(3,8)      = -TAUFF_DD*F1110
      A(3,9)      = 0.0
      A(3,10)     = 0.0
      A(3,11)     = Z3_BD
      A(4,1)      = -RHOBF_DD*F51
      A(4,2)      = -RHOBF_DD*F52
      A(4,3)      = -RHOBF_DD*F53
      A(4,4)      = 1.0
      A(4,5)      = 0.0
      A(4,6)      = 0.0
      A(4,7)      = 0.0
      A(4,8)      = -TAUFF_DD*F910
      A(4,9)      = -TAUFF_DD*F911
      A(4,10)     = -TAUFF_DD*F912
      A(4,11)     = 0.0
      A(5,1)      = -RHOBF_DD*F61
      A(5,2)      = -RHOBF_DD*F62
      A(5,3)      = -RHOBF_DD*F63
      A(5,4)      = 0.0 
      A(5,5)      = 1.0
      A(5,6)      = 0.0
      A(5,7)      = 0.0
      A(5,8)      = -TAUFF_DD*F810
      A(5,9)      = -TAUFF_DD*F811
      A(5,10)     = -TAUFF_DD*F812
      A(5,11)     = Z6_BD
      A(6,1)      = -TAUBF_DD*F61
      A(6,2)      = -TAUBF_DD*F62
      A(6,3)      = -TAUBF_DD*F63
      A(6,4)      = 0.0
      A(6,5)      = 0.0
      A(6,6)      = 1.0
      A(6,7)      = 0.0
      A(6,8)      = -RHOFF_DD*F810
      A(6,9)      = -RHOFF_DD*F811
      A(6,10)     = -RHOFF_DD*F812
      A(6,11)     = Z8_BD
      A(7,1)      = -TAUBF_DD*F51
      A(7,2)      = -TAUBF_DD*F52
      A(7,3)      = -TAUBF_DD*F53
      A(7,4)      = 0.0
      A(7,5)      = 0.0
      A(7,6)      = 0.0
      A(7,7)      = 1.0
      A(7,8)      = -RHOFF_DD*F910
      A(7,9)      = -RHOFF_DD*F911
      A(7,10)     = -RHOFF_DD*F912
      A(7,11)     = 0.0
      A(8,1)      = 0.0
      A(8,2)      = 0.0
      A(8,3)      = 0.0
      A(8,4)      = 0.0
      A(8,5)      = 0.0
      A(8,6)      = -RHOFF_DD*F108
      A(8,7)      = -RHOFF_DD*F109
      A(8,8)      = 1.0
      A(8,9)      = -RHOFF_DD*F1011
      A(8,10)     = -RHOFF_DD*F1012
      A(8,11)     = 0.0
      A(9,1)      = -TAUBF_DD*F31
      A(9,2)      = 0.0
      A(9,3)      = 0.0
      A(9,4)      = -TAUBF_DD*F35
      A(9,5)      = -TAUBF_DD*F36
      A(9,6)      = -RHOFF_DD*F118
      A(9,7)      = -RHOFF_DD*F119
      A(9,8)      = -RHOFF_DD*F1110
      A(9,9)      = 1.0 
      A(9,10)     = 0.0
      A(9,11)     = Z11_BD
      A(10,1)     = -TAUBF_DD*F21
      A(10,2)     = 0.0
      A(10,3)     = 0.0
      A(10,4)     = -TAUBF_DD*F25
      A(10,5)     = -TAUBF_DD*F26
      A(10,6)     = -RHOFF_DD*F128
      A(10,7)     = -RHOFF_DD*F129
      A(10,8)     = -RHOFF_DD*F1210
      A(10,9)     = 0.0 
      A(10,10)    = 1.0
      A(10,11)    = Z12_BD
      
      CALL SOLMATS(N,A,XSOL)

      J1 = XSOL(1)
      J2 = XSOL(2)
      J3 = XSOL(3)
      J5 = XSOL(4)
      J6 = XSOL(5)
      J8 = XSOL(6)
      J9 = XSOL(7)
      J10 = XSOL(8)
      J11 = XSOL(9)
      J12 = XSOL(10)
      
      G1 = F12*J2+F13*J3+F15*J5+F16*J6
      G4 = F41*J1+F42*J2+F43*J3+F45*J5+F46*J6
      G7 = F78*J8+F79*J9+F710*J10+F711*J11+F712*J12
      G10 = F108*J8+F109*J9+F1011*J11+F1012*J12                  
      
      TAU_BB = (TAUFF_BB_PERP*(AB-NK)*ABS(SIN(OMEGA_H)))/
     &(2.0*S*ABS(COS(OMEGA_H)))
      TAU_BD = (G4+TAUFF_DD*G10)/2.0
      RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G7)/2.0
                                                 
      END SUBROUTINE PD_BEAM_CASE_III
      
!****************************************************************************
!
!  SUBROUTINE: PD_BEAM_CASE_IV (EIGHT SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE)
!  
!****************************************************************************

      SUBROUTINE PD_BEAM_CASE_IV( S, W, OMEGA_H, DE,
     &      RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &      RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &      RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &      RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &      RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &      RHO_BD, TAU_BD, TAU_BB)      

      IMPLICIT NONE
      REAL S                        ! pleat spacing (> 0)
      REAL W                        ! pleat depth (>=0, same units as S)
      REAL OMEGA_H            ! horizontal profile angle, radians
      REAL DE                        ! width of illumination on pleat bottom (same units as S)

                                    ! fabric properties at current (off-normal) incidence
                                    !   _PARL = surface parallel to window (pleat top/bot)
                                    !   _PERP = surface perpendicular to window (pleat side)
      REAL RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
      REAL RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
      REAL RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
      REAL RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP

      REAL RHOFF_DD            ! fabric front diffuse-diffuse reflectance
      REAL RHOBF_DD            ! fabric back diffuse-diffuse reflectance
      REAL TAUFF_DD            ! fabric front diffuse-diffuse transmittance
      REAL TAUBF_DD            ! fabric back diffuse-diffuse transmittance
      REAL RHO_BD            ! returned: drape front beam-diffuse reflectance
      REAL TAU_BD            ! returned: drape front beam-diffuse transmittance
      REAL TAU_BB            ! returned: drape front beam-beam transmittance


      REAL TAUBF_BT_PERP

      REAL AK, CG                  ! length of diagonal strings
      REAL Z1_BB                  ! beam source term
      REAL Z1_BD, Z2_BD, Z4_BD, Z6_BD, Z8_BD      ! diffuse source terms
      ! shape factors      
      REAL F12, F14, F21, F24, F31, F32, F34, F41, F42
      REAL F56, F57, F58
      REAL F67, F68, F76, F78, F86, F87                  
      REAL J1, J2, J4, J6, J7, J8      ! radiosity, surface i
      REAL G1, G3, G5, G7                   ! irradiance, surface i
      INTEGER N
      PARAMETER (N = 6)
      REAL :: A( N, N+2)      ! coefficients of the radiosity equations matrix
      REAL :: XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)
      
!-----------------------------------------------------------------------------------

!#if defined( PD_PRINTCASE)
!      PRINT *, OMEGA_H/DTOR, "IV"
!#endif
      
      TAUBF_BT_PERP = TAUBF_BD_PERP + TAUBF_BB_PERP

      AK = SQRT(W*W+S*S)
      CG = AK
                        
      Z1_BB = TAUFF_BB_PARL
      Z1_BD = TAUFF_BD_PARL
      Z2_BD = Z1_BB*RHOBF_BT_PERP*S/W
      Z4_BD = TAUFF_BD_PERP*S/W
      Z6_BD = RHOFF_BT_PERP*S/W
      Z8_BD = Z1_BB*TAUBF_BT_PERP*S/W

      F12 = (S+W-AK)/(2.0*S)
      F14 = (S+W-CG)/(2.0*S)
      F21 = (S+W-AK)/(2.0*W)
      F24 = (AK+CG-2.0*S)/(2.0*W)
      F31 = (AK+CG-2.0*W)/(2.0*S)
      F32 = F12
      F34 = F12
      F41 = F21
      F42 = F24
      F56 = F12
      F57 = F31
      F58 = F14
      F67 = F41
      F68 = F24
      F76 = F32
      F78 = F34
      F86 = F42
      F87 = F21
            
            
!-----------------------------------------------------------------------------------

!      data A/ (N * (N+2)) * 0.0/      ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
!      data XSOL/ N *0.0/            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 
      
      A = 0.0            ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
      XSOL = 0.0            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 

      ! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX
            
      A(1,1)      = 1.0
      A(1,2)      = -RHOBF_DD*F12
      A(1,3)      = -RHOBF_DD*F14
      A(1,4)      = 0.0
      A(1,5)      = 0.0
      A(1,6)      = 0.0
      A(1,7)      = Z1_BD
      A(2,1)      = -RHOBF_DD*F21
      A(2,2)      = 1.0
      A(2,3)      = -RHOBF_DD*F24
      A(2,4)      = -TAUFF_DD*F86
      A(2,5)      = -TAUFF_DD*F87
      A(2,6)      = 0.0 
      A(2,7)      = Z2_BD
      A(3,1)      = -RHOBF_DD*F41
      A(3,2)      = -RHOBF_DD*F42
      A(3,3)      = 1.0
      A(3,4)      = 0.0
      A(3,5)      = -TAUFF_DD*F67
      A(3,6)      = -TAUFF_DD*F68
      A(3,7)      = Z4_BD
      A(4,1)      = -TAUBF_DD*F41
      A(4,2)      = -TAUBF_DD*F42
      A(4,3)      = 0.0 
      A(4,4)      = 1.0
      A(4,5)      = -RHOFF_DD*F67
      A(4,6)      = -RHOFF_DD*F68
      A(4,7)      = Z6_BD
      A(5,1)      = 0.0
      A(5,2)      = 0.0
      A(5,3)      = 0.0
      A(5,4)      = -RHOFF_DD*F76
      A(5,5)      = 1.0
      A(5,6)      = -RHOFF_DD*F78
      A(5,7)      = 0.0
      A(6,1)      = -TAUBF_DD*F21
      A(6,2)      = 0.0
      A(6,3)      = -TAUBF_DD*F24
      A(6,4)      = -RHOFF_DD*F86
      A(6,5)      = -RHOFF_DD*F87
      A(6,6)      = 1.0
      A(6,7)      = Z8_BD

      CALL SOLMATS(N,A,XSOL)

      J1 = XSOL(1)
      J2 = XSOL(2)
      J4 = XSOL(3)
      J6 = XSOL(4)
      J7 = XSOL(5)
      J8 = XSOL(6)
            
      G1 = F12*J2+F14*J4
      G3 = F31*J1+F32*J2+F34*J4
      G5 = F56*J6+F57*J7+F58*J8
      G7 = F76*J6+F78*J8                  
      
      TAU_BB = TAUFF_BB_PERP/2
      TAU_BD = (G3+TAUFF_DD*G7)/2.0
      RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G5)/2.0
                                                 
      END SUBROUTINE PD_BEAM_CASE_IV

!****************************************************************************
!
!  SUBROUTINE: PD_BEAM_CASE_V (NINE SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE)
!
!****************************************************************************

      SUBROUTINE PD_BEAM_CASE_V( S, W, OMEGA_H, DE,
     &      RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &      RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &      RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &      RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &      RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &      RHO_BD, TAU_BD, TAU_BB)      

      IMPLICIT NONE
      REAL S                        ! pleat spacing (> 0)
      REAL W                        ! pleat depth (>=0, same units as S)
      REAL OMEGA_H            ! horizontal profile angle, radians
      REAL DE                        ! width of illumination on pleat bottom (same units as S)

                                    ! fabric properties at current (off-normal) incidence
                                    !   _PARL = surface parallel to window (pleat top/bot)
                                    !   _PERP = surface perpendicular to window (pleat side)
      REAL RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
      REAL RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
      REAL RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
      REAL RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP

      REAL RHOFF_DD            ! fabric front diffuse-diffuse reflectance
      REAL RHOBF_DD            ! fabric back diffuse-diffuse reflectance
      REAL TAUFF_DD            ! fabric front diffuse-diffuse transmittance
      REAL TAUBF_DD            ! fabric back diffuse-diffuse transmittance
      REAL RHO_BD            ! returned: drape front beam-diffuse reflectance
      REAL TAU_BD            ! returned: drape front beam-diffuse transmittance
      REAL TAU_BB            ! returned: drape front beam-beam transmittance


      REAL TAUBF_BT_PERP


      REAL AK, CG, MK, DK, MF, DM, GM, GF            ! lengths of surfaces and diagonal strings                              
      REAL Z1_BB      ! beam source term
      REAL Z1_BD, Z2_BD, Z4_BD, Z6_BD, Z7_BD, Z9_BD      ! diffuse source terms
      ! shape factors
      REAL F12, F14, F21, F24, F31, F32, F34, F41, F42
      REAL F56, F57, F58, F59
      REAL F67, F68, F69, F76, F79, F86, F89, F96, F97, F98                        
      REAL J1, J2, J4, J6, J7, J8, J9            ! radiosities
      REAL G1, G3, G5, G7, G8                         ! irradiances
      INTEGER N
      PARAMETER (N = 7)
      REAL :: A( N, N+2)      ! coefficients of the radiosity equations matrix
      REAL :: XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)
      
!-----------------------------------------------------------------------------------

!#if defined( PD_PRINTCASE)
!      PRINT *, OMEGA_H/DTOR, "V"
!#endif

      TAUBF_BT_PERP = TAUBF_BD_PERP + TAUBF_BB_PERP

      AK = SQRT(W*W+S*S)
      CG = AK
      MK = (W*ABS(SIN(OMEGA_H)))/ABS(COS(OMEGA_H))
      DK = AK
      MF = S-MK
      DM = SQRT(W*W+MF*MF)
      GM = SQRT(W*W+MK*MK)
      GF = AK
                        
      Z1_BB = TAUFF_BB_PARL
      Z1_BD = TAUFF_BD_PARL
      Z2_BD = Z1_BB*RHOBF_BT_PERP*S/DE
      Z4_BD = TAUFF_BD_PERP*S/DE
      Z6_BD = RHOFF_BT_PERP*S/DE
      Z7_BD = RHOFF_BT_PARL
      Z9_BD = Z1_BB*TAUBF_BT_PERP*S/DE

      F12 = (S+W-AK)/(2.0*S)
      F14 = (S+W-CG)/(2.0*S)
      F21 = (S+W-AK)/(2.0*W)
      F24 = (AK+CG-2.0*S)/(2.0*W)
      F31 = (AK+CG-2.0*W)/(2.0*S)
      F32 = F14
      F34 = F12
      F41 = F21
      F42 = F24
      F56 = F12
      F57 = (DM+GF-(GM+W))/(2.0*S)
      F58 = (DK+GM-(DM+W))/(2.0*S)
      F59 = F14
      F67 = (W+MF-DM)/(2.0*W)
      F68 = (DM+S-(DK+MF))/(2.0*W)
      F69 = F24
      F76 = (W+MF-DM)/(2.0*MF)
      F79 = (GM+S-(GF+MK))/(2.0*MF)
      F86 = (DM+S-(DK+MF))/(2.0*MK)
      F89 = (W+MK-GM)/(2.0*MK)
      F96 = F42
      F97 = (GM+S-(GF+MK))/(2.0*W)
      F98 = (W+MK-GM)/(2.0*W)
            
      
!-----------------------------------------------------------------------------------

!      data A/ (N * (N+2)) * 0.0/      ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
!      data XSOL/ N *0.0/            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 
      
      A = 0.0            ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
      XSOL = 0.0            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 

! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX
            
      A(1,1)      = 1.0
      A(1,2)      = -RHOBF_DD*F12
      A(1,3)      = -RHOBF_DD*F14
      A(1,4)      = 0.0
      A(1,5)      = 0.0
      A(1,6)      = 0.0
      A(1,7)      = 0.0
      A(1,8)      = Z1_BD
      A(2,1)      = -RHOBF_DD*F21
      A(2,2)      = 1.0
      A(2,3)      = -RHOBF_DD*F24
      A(2,4)      = -TAUFF_DD*F96
      A(2,5)      = -TAUFF_DD*F97
      A(2,6)      = -TAUFF_DD*F98 
      A(2,7)      = 0.0
      A(2,8)      = Z2_BD
      A(3,1)      = -RHOBF_DD*F41
      A(3,2)      = -RHOBF_DD*F42
      A(3,3)      = 1.0
      A(3,4)      = 0.0
      A(3,5)      = -TAUFF_DD*F67
      A(3,6)      = -TAUFF_DD*F68
      A(3,7)      = -TAUFF_DD*F69
      A(3,8)      = Z4_BD
      A(4,1)      = -TAUBF_DD*F41
      A(4,2)      = -TAUBF_DD*F42
      A(4,3)      = 0.0
      A(4,4)      = 1.0
      A(4,5)      = -RHOFF_DD*F67
      A(4,6)      = -RHOFF_DD*F68
      A(4,7)      = -RHOFF_DD*F69
      A(4,8)      = Z6_BD
      A(5,1)      = 0.0
      A(5,2)      = 0.0
      A(5,3)      = 0.0
      A(5,4)      = -RHOFF_DD*F76
      A(5,5)      = 1.0
      A(5,6)      = 0.0
      A(5,7)      = -RHOFF_DD*F79
      A(5,8)      = Z7_BD
      A(6,1)      = 0.0
      A(6,2)      = 0.0
      A(6,3)      = 0.0
      A(6,4)      = -RHOFF_DD*F86
      A(6,5)      = 0.0
      A(6,6)      = 1.0
      A(6,7)      = -RHOFF_DD*F89
      A(6,8)      = 0.0
      A(7,1)      = -TAUBF_DD*F21
      A(7,2)      = 0.0
      A(7,3)      = -TAUBF_DD*F24
      A(7,4)      = -RHOFF_DD*F96
      A(7,5)      = -RHOFF_DD*F97
      A(7,6)      = -RHOFF_DD*F98
      A(7,7)      = 1.0
      A(7,8)      = Z9_BD

      CALL SOLMATS(N,A,XSOL)

      J1 = XSOL(1)
      J2 = XSOL(2)
      J4 = XSOL(3)
      J6 = XSOL(4)
      J7 = XSOL(5)
      J8 = XSOL(6)
      J9 = XSOL(7)
            
      G1 = F12*J2+F14*J4
      G3 = F31*J1+F32*J2+F34*J4
      G5 = F56*J6+F57*J7+F58*J8+F59*J9
      G7 = F76*J6+F79*J9
      G8 = F86*J6+F89*J9            
      
      TAU_BB = (2.0*(DE-W)*ABS(SIN(OMEGA_H))*TAUFF_BB_PARL+
     &(S*ABS(COS(OMEGA_H))-(DE-W)*ABS(SIN(OMEGA_H)))*TAUFF_BB_PERP)/
     &       (2.0*S*ABS(COS(OMEGA_H)))
     
      TAU_BD = (S*G3+TAUFF_DD*(MK*G8+MF*G7)+MF*TAUFF_BD_PARL)/(2.0*S)
      RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G5)/2.0
                                                 
      END SUBROUTINE PD_BEAM_CASE_V


!****************************************************************************
!
!  SUBROUTINE: PD_BEAM_CASE_VI (EIGHT SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE)
!
!****************************************************************************
      SUBROUTINE PD_BEAM_CASE_VI( S, W, OMEGA_H, DE,
     &      RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL,
     &      RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL,
     &      RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP,
     &      RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP,
     &      RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD,
     &      RHO_BD, TAU_BD, TAU_BB)      

      IMPLICIT NONE
      REAL S                        ! pleat spacing (> 0)
      REAL W                        ! pleat depth (>=0, same units as S)
      REAL OMEGA_H            ! horizontal profile angle, radians
      REAL DE                        ! width of illumination on pleat bottom (same units as S)

                                    ! fabric properties at current (off-normal) incidence
                                    !   _PARL = surface parallel to window (pleat top/bot)
                                    !   _PERP = surface perpendicular to window (pleat side)
      REAL RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
      REAL RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
      REAL RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
      REAL RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP

      REAL RHOFF_DD            ! fabric front diffuse-diffuse reflectance
      REAL RHOBF_DD            ! fabric back diffuse-diffuse reflectance
      REAL TAUFF_DD            ! fabric front diffuse-diffuse transmittance
      REAL TAUBF_DD            ! fabric back diffuse-diffuse transmittance
      REAL RHO_BD            ! returned: drape front beam-diffuse reflectance
      REAL TAU_BD            ! returned: drape front beam-diffuse transmittance
      REAL TAU_BB            ! returned: drape front beam-beam transmittance


      REAL TAUBF_BT_PERP

      REAL AK, CG                  ! length of diagonal strings                  
      REAL Z1_BD, Z7_BD      ! diffuse source termps
      ! shape factors      
      REAL F12, F14, F21, F24, F31, F32, F34, F41, F42, F56, F57, F58
      REAL F67, F68, F76, F78, F86, F87                  
      REAL J1, J2, J4, J6, J7, J8      ! radiosity, surface i
      REAL G1, G3, G5, G7                   ! irradiance, surface i
      INTEGER N
      PARAMETER (N = 6)                              
      REAL A( N, N+2)      ! coefficients of the radiosity equations matrix                                                
      REAL XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)                                                      

!-----------------------------------------------------------------------------------      
#if defined( PD_PRINTCASE)
      PRINT *, OMEGA_H/DTOR, "VI"
#endif

      AK = SQRT(W*W+S*S)
      CG = AK
                        
      Z1_BD = TAUFF_BD_PARL      
      Z7_BD = RHOFF_BT_PARL


      F12 = (S+W-AK)/(2.0*S)
      F14 = (S+W-CG)/(2.0*S)
      F21 = (S+W-AK)/(2.0*W)
      F24 = (AK+CG-2.0*S)/(2.0*W)
      F31 = (AK+CG-2.0*W)/(2.0*S)
      F32 = F12
      F34 = F14
      F41 = F21
      F42 = F24
      F56 = F12
      F57 = F31
      F58 = F14
      F67 = F41
      F68 = F24
      F76 = F14
      F78 = F14
      F86 = F42
      F87 = F21
      
      
!-----------------------------------------------------------------------------------

!      data A/ (N * (N+2)) * 0.0/      ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS 
!      data XSOL/ N *0.0/            ! INITIALIZE SOLUTION VECTOR COEFFICIENTS 
      
      A = 0.0
      XSOL = 0.0

! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX
      
      A(1,1)      = 1.0
      A(1,2)      = -RHOBF_DD*F12
      A(1,3)      = -RHOBF_DD*F14
      A(1,4)      = 0.0
      A(1,5)      = 0.0
      A(1,6)      = 0.0
      A(1,7)      = Z1_BD
      A(2,1)      = -RHOBF_DD*F21
      A(2,2)      = 1.0
      A(2,3)      = -RHOBF_DD*F24
      A(2,4)      = -TAUFF_DD*F86
      A(2,5)      = -TAUFF_DD*F87
      A(2,6)      = 0.0 
      A(2,7)      = 0.0
      A(3,1)      = -RHOBF_DD*F41
      A(3,2)      = -RHOBF_DD*F42
      A(3,3)      = 1.0
      A(3,4)      = 0.0
      A(3,5)      = -TAUFF_DD*F67
      A(3,6)      = -TAUFF_DD*F68
      A(3,7)      = 0.0
      A(4,1)      = -TAUBF_DD*F41
      A(4,2)      = -TAUBF_DD*F42
      A(4,3)      = 0.0 
      A(4,4)      = 1.0
      A(4,5)      = -RHOFF_DD*F67
      A(4,6)      = -RHOFF_DD*F68
      A(4,7)      = 0.0
      A(5,1)      = 0.0
      A(5,2)      = 0.0
      A(5,3)      = 0.0
      A(5,4)      = -RHOFF_DD*F76
      A(5,5)      = 1.0
      A(5,6)      = -RHOFF_DD*F78
      A(5,7)      = Z7_BD
      A(6,1)      = -TAUBF_DD*F21
      A(6,2)      = 0.0
      A(6,3)      = -TAUBF_DD*F24
      A(6,4)      = -RHOFF_DD*F86
      A(6,5)      = -RHOFF_DD*F87
      A(6,6)      = 1.0
      A(6,7)      = 0.0

      CALL SOLMATS(N,A,XSOL)

      J1 = XSOL(1)
      J2 = XSOL(2)
      J4 = XSOL(3)
      J6 = XSOL(4)
      J7 = XSOL(5)
      J8 = XSOL(6)
            
      G1 = F12*J2+F14*J4
      G3 = F31*J1+F32*J2+F34*J4
      G5 = F56*J6+F57*J7+F58*J8
      G7 = F76*J6+F78*J8                  
      
      TAU_BB = TAUFF_BB_PARL
      TAU_BD = (G3+TAUFF_DD*G7+TAUFF_BD_PARL)/2.0
      RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G5)/2.0
                                                 
      END SUBROUTINE PD_BEAM_CASE_VI

!****************************************************************************
!  SUBROUTINE: HEMINT
!     Romberg integration of property function over hemispehere
!****************************************************************************

      SUBROUTINE HEMINT(layer_type, F_Opt, F_P, OUT_HEMINT)
      IMPLICIT NONE
!      REAL F                  ! property integrand function
      INTEGER F_Opt            ! options passed to F() (hipRHO, hipTAU)
      INTEGER hipDIM
      INTEGER KMAX            ! max steps
      INTEGER NPANMAX
      REAL TOL                  ! convergence tolerance
      REAL T(8, 8), FX
      REAL X1, X2, X, DX, SUM, DIFF
      INTEGER nPan, I, K, L, iPX
      REAL PI, PIOVER2
      REAL F_P(3)            ! parameters passed to F()
      REAL OUT_HEMINT
      REAL IN_F
      INTEGER layer_type
      
      PARAMETER (PI = 3.14159265358979)
      PARAMETER (KMAX = 8, hipDIM = 3)
      
      NPANMAX = 2**KMAX
      TOL = .0005
      PIOVER2 = PI/2.
      
      X1 = 0.                  ! integration limits
      X2 = PIOVER2
      nPan=1
      SUM = 0.
      DO K = 1, KMAX
            DX = (X2-X1)/nPan
            iPX = NPANMAX / nPan
            DO I = 0, nPan
                  IF (K == 1 .OR. MOD( I*iPX, iPX*2) /= 0) THEN
                        ! evaluate ingegrand function for new X values
                        !   2 * sin( x) * cos( x) covers hemisphere with single intregral
                        X = X1 + I*DX
                        
!                        write(96,*) 'Input angle theta for FM_BEAM', X
                        
                        if (layer_type == 3) then
                        
                              CALL FM_F( X, F_Opt, F_P, IN_F)
                        
                        elseif (layer_type == 4) then
                        
                            CALL RB_F( X, F_Opt, F_P, IN_F)

                        elseif (layer_type == 5) then
                        
                            CALL IS_F( X, F_Opt, F_P, IN_F)
                                                      
                        end if
                        
                        FX  = 2. * SIN( X) * COS( X) * IN_F
                        IF (K == 1) FX = FX / 2.
                        SUM = SUM + FX
                  END IF
            END DO

            T(1,K) = DX * SUM

            ! trapezoid result - i.e., first column Romberg entry
            ! Now complete the row
            IF (K > 1) THEN
                  DO L=2,K
                        T(L,K) = ((4.**(L-1))*T(L-1,K) - T(L-1,K-1)) 
     &                         / (4.**(L-1)-1.)
                  END DO
                  ! check for convergence
                  !    do 8 panels minimum, else can miss F() features
                  IF ( nPan >= 8) THEN
                        DIFF = ABS( T(K,K) - T(K-1, K-1))
                        IF (DIFF < TOL) EXIT
                  END IF
            END IF
            nPan = 2 * nPan
      END DO
      IF (K > KMAX) THEN
            WRITE( *, "('HEMINT convergence failure')")
            K = KMAX
      END IF
      !OUT_HEMINT = P01( T( K, K), "OUT_HEMINT")
      OUT_HEMINT = T( K, K)
      
      IF (OUT_HEMINT < 0.) OUT_HEMINT = 0.
      
      END SUBROUTINE HEMINT

