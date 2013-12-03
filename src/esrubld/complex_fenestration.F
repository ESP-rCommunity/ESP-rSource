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

C it contains the following routines:
C
C      CFC_thermal_processing
C      qlwCFC
C      FSsolve
C      cfc_eff_opt_prop
C      solar_multilayer
C      cfc_convection
C      ConvVertCav
C      vb_eff_diff_properties
C      CFC_time_dependent_Rgap
C      SOLMATS
C      VB_SOL46_CURVE
C      VB_SOL46_THICK
C      VB_SOL_DD_THICK
C      VB_SOL4
C      VB_SOL6
C       IS_DIFF
C       IS_F
C       IS_BEAM
C       IS_OPENNESS
C       IS_DSRATIO
C      RB_DIFF
C      RB_BEAM
C       RB_F
C       FM_DIFF
C      FM_BEAM
C      PD_LW
C      PD_DIFF
C      PD_BEAM
C      PD_BEAM_CASE_I
C      PD_BEAM_CASE_II
C      PD_BEAM_CASE_III
C      PD_BEAM_CASE_IV
C      PD_BEAM_CASE_V
C      PD_BEAM_CASE_VI
C      OPENNESS_LW
C      P01 (FUNCTION)
C      HEMINT
C      SETCoef
C      TDMAsol
C      profile_angle
C      set_HR_to_zero
C      set_QELW_to_zero
C      CFCoutput
C      CFC_control
C      get_incident_solrad

C ********************************************************************
C                   --fenestration_controller--
C
C Main controller for thermal processing of CFCs. It handles:
C     - Calculation of effective longwave radiative properties of
C       slat-type blinds
C     - Calculation of longwave exchange generation terms
C     - Calculation of convective resistors and convective generation
C       terms
C
C ********************************************************************
      subroutine CFC_thermal_processing(icomp)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/ts4/itsitr(msch),itrpas

      integer nconst,nelts,ngaps,npgap
      integer nnds,nndz,nndc,nndl
      real ei,ee,ai,ae
      integer ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer itsitr,itrpas

C Local variables to hold solar optical and longwave properties 
C of each layer in CFC.
      real tmp_lwEF,tmp_lwEB,tmp_lwT !blind longwave properties
      real SolT_mat
      real tmp_vb_w, tmp_vb_s,tmp_vb_phi !venetian blind descriptors
      real tmp_drp_w, tmp_drp_s,tmp_drp_Fr !drape blind descriptors
      real eff_lwE,eff_lwT,eff_lwTx !effective longwave properties
      real OPENNESS_FABRIC !roller blind and insect screen property = tau_bb
        
      integer icomp,i,j,isur,nc,ne,icfctp,ncfc_elmnt
      integer nn

      real diff
      
      nc=nconst(icomp)

C ******** Calculate effective longwave properties for each CFC layer

      do 10 i = 1, nc

        if(icfcfl(icomp,i).gt.0)then
          icfctp = icfcfl(icomp,i)

C.........Only calculate at first time-step and when slat angle change occurs
C.........(effective longwave radiative properties are a function of geometry only).
          diff = abs(vb_phiNew(icomp,icfctp) - vb_phiOld(icomp,icfctp))
          IF((NSINC.eq.1).or.(diff.lt.0.1))then

            do 20 j = 1, ncfc_el(icomp,icfctp)

C.............Layer is VENETIAN BLIND
              if(icfcltp(icomp,icfctp,j).eq.iVenBlind)then

C...............Assign longwave slat properties from commons.
                tmp_lwEF = rlwEF_sv(icomp,icfctp,j)
                tmp_lwEB = rlwEB_sv(icomp,icfctp,j)
                tmp_lwT  = rlwT_sv(icomp,icfctp,j)

C...............Assign blind descriptors from commons.
                tmp_vb_w = vb_w(icomp,icfctp)
                tmp_vb_s = vb_s(icomp,icfctp)
                tmp_vb_phi = vb_phiNew(icomp,icfctp)
            
C...............Determine effective longwave properties for FRONT layer surface (facing
C...............the outdoors).
                call vb_eff_diff_properties(tmp_vb_w,tmp_vb_s,
     &            tmp_vb_phi,tmp_lwEF,tmp_lwEB,tmp_lwT,eff_lwE,eff_lwT)

C...............Save effective properties (front reflectance and transmittance) into 
C...............common variables.
                rlwEF(icomp,icfctp,j) = eff_lwE
                rlwT(icomp,icfctp,j)  = eff_lwT

C...............Determine effective longwave properties for BACK layer surface (facing
C...............the indoors) by setting negative slat angle
                call vb_eff_diff_properties(tmp_vb_w,tmp_vb_s,
     &            tmp_vb_phi*(-1.0),tmp_lwEF,tmp_lwEB,tmp_lwT,
     &            eff_lwE,eff_lwT)

C...............Save efective back reflectance into common variable
                rlwEB(icomp,icfctp,j) = eff_lwE
            
C...............If shade is retracted, set longwave transmittance to 1.             
                if(i_shd(icomp,icfctp).eq.0)then
                  rlwEF(icomp,icfctp,j) = 0.001
                  rlwEB(icomp,icfctp,j) = 0.001
                  rlwT(icomp,icfctp,j)  = 0.999
                endif
                  
C.............Layer is PLEATED DRAPE
              elseif(icfcltp(icomp,icfctp,j).eq.iPleatedDrape)then
            
C...............Assign longwave drape properties to commons, properties at 0 openness
                tmp_lwEF = rlwEF_sv(icomp,icfctp,j)
                tmp_lwEB = rlwEB_sv(icomp,icfctp,j)
                tmp_lwT  = rlwT_sv(icomp,icfctp,j)

                SolT_mat = solT(icomp,icfctp,j)

C...............Assign drape descriptors from commons
                tmp_drp_w = drp_w(icomp,icfctp)
                tmp_drp_s = drp_s(icomp,icfctp)
            
C...............Calculate front effective longwave optical properties adn assign to commons        
                call PD_LW(tmp_drp_s,tmp_drp_w,SolT_mat,tmp_lwEF,
     &          tmp_lwEB,tmp_lwT,eff_lwE,eff_lwT)
            
C...............Save effective properties (front reflectance and transmittance) into 
C...............common variables
                rlwEF(icomp,icfctp,j) = eff_lwE
                rlwT(icomp,icfctp,j)  = eff_lwT

C...............Calculate back effective longwave optical properties adn assign to commons
                call PD_LW(tmp_drp_s,tmp_drp_w,SolT_mat,tmp_lwEB,
     &          tmp_lwEF,tmp_lwT,eff_lwE,eff_lwTx)
            
C...............Save efective back reflectance into common variable
                rlwEB(icomp,icfctp,j) = eff_lwE
            
C................If shade is retracted, set longwave transmittance to 1.             
                 IF(i_shd(icomp,icfctp).eq.0)THEN
                    rlwEF(icomp,icfctp,j) = 0.001
                    rlwEB(icomp,icfctp,j) = 0.001
                    rlwT(icomp,icfctp,j)  = 0.999
                 ENDIF
      
C.............Layer is ROLLER BLIND
              elseif(icfcltp(icomp,icfctp,j).eq.iRollerBlind)then
            
C...............Assign longwave drape properties to commons, at 0 openness
                tmp_lwEF = rlwEF_sv(icomp,icfctp,j)
                tmp_lwEB = rlwEB_sv(icomp,icfctp,j)
                tmp_lwT  = rlwT_sv(icomp,icfctp,j)

C...............Assign roller blind descriptor
                OPENNESS_FABRIC = solT(icomp,icfctp,j)
            
C...............Calculate front effective longwave optical properties adn assign to commons        
                CALL OPENNESS_LW(OPENNESS_FABRIC,tmp_lwEF,
     &          tmp_lwT, eff_lwE, eff_lwT)
            
C...............Save effective properties (front emittance and transmittance) into 
C...............common variables
                rlwEF(icomp,icfctp,j) = eff_lwE
                rlwT(icomp,icfctp,j)  = eff_lwT
        
C...............Calculate back effective longwave optical properties adn assign to commons   
                CALL OPENNESS_LW(OPENNESS_FABRIC, tmp_lwEB,
     &          tmp_lwT, eff_lwE, eff_lwTx)
       
C...............Save effective back emittance into common variable
                rlwEB(icomp,icfctp,j) = eff_lwE
        
C...............If shade is retracted, set longwave transmittance to 1.             
                IF(i_shd(icomp,icfctp).eq.0)THEN
                    rlwEF(icomp,icfctp,j) = 0.001
                    rlwEB(icomp,icfctp,j) = 0.001
                    rlwT(icomp,icfctp,j)  = 0.999
                ENDIF              
            
C.............Layer is INSECT SCREEN
              elseif(icfcltp(icomp,icfctp,j).eq.iInsectScreen)then
            
C...............Assign longwave drape properties to commons, at 0 openness
                tmp_lwEF = rlwEF_sv(icomp,icfctp,j)
                tmp_lwEB = rlwEB_sv(icomp,icfctp,j)
                tmp_lwT  = rlwT_sv(icomp,icfctp,j)

C...............Assign insect screen descriptor
                OPENNESS_FABRIC = solT(icomp,icfctp,j)
            
C...............Calculate front effective longwave optical properties adn assign to commons        
                CALL OPENNESS_LW(OPENNESS_FABRIC,tmp_lwEF, 
     &          tmp_lwT, eff_lwE, eff_lwT)
     
C...............Save effective properties (front emittance and transmittance) into 
C...............common variables
                rlwEF(icomp,icfctp,j) = eff_lwE
                rlwT(icomp,icfctp,j)  = eff_lwT
        
C...............Calculate back effective longwave optical properties adn assign to commons
                CALL OPENNESS_LW(OPENNESS_FABRIC, tmp_lwEB, 
     &          tmp_lwT, eff_lwE, eff_lwTx)
     
C...............Save effective back emittance into common variable
                rlwEB(icomp,icfctp,j)=eff_lwE
            
C...............If shade is retracted, set longwave transmittance to 1.             
                IF(i_shd(icomp,icfctp).eq.0)THEN
                    rlwEF(icomp,icfctp,j) = 0.001
                    rlwEB(icomp,icfctp,j) = 0.001
                    rlwT(icomp,icfctp,j)  = 0.999
                ENDIF
      
              end if
            
  20        continue

          END IF

        end if

  10  continue

C Replace emissivities of external/internal CFC layers to GSLedit
C properties.
C     Note: This only affects glazing layers for which ESP-r performs
C           the standard longwave calculation. When a blind layer is 
C           present on indoor/outdoor side, the default ESP-r longwave
C           calculations are replaced with the CFC routine in 
C           subroutine qlwCFC.
      do 1111 i=1,nc
            
        if(icfcfl(icomp,i).gt.0)then

          icfctp=icfcfl(icomp,i)
          ncfc_elmnt=ncfc_el(icomp,icfctp)

          ei(icomp,i)=rlwEB(icomp,icfctp,ncfc_elmnt)
          ee(icomp,i)=rlwEF(icomp,icfctp,1)
        end if

 1111 continue

C ******** Determine CFC longwave radiant source terms

C Set present time values and initialise future values.
C qcfc_lw(,,,1) relates to present time-row, (,,,2) to future
C time-row.
      do 32 i=1,nc
        nn=nndc(icomp,i)
        do 31 j=1,nn
        
          if(itrpas.eq.0)then
            qcfc_lw(icomp,i,j,1)=qcfc_lwF(icomp,i,j)   !future term from previous timestep
            qcfc_lwP(icomp,i,j) =qcfc_lwF(icomp,i,j)
          else
            qcfc_lw(icomp,i,j,1)=qcfc_lwP(icomp,i,j)   !if iterating use values from first iteration
            qcfc_lw(icomp,i,j,2)=qcfc_lwF(icomp,i,j)
          endif
 
          if(NSINC.eq.1)qcfc_lw(icomp,i,j,1)=0.
          if(itrpas.eq.0)qcfc_lw(icomp,i,j,2)=0.
 31     continue
 32   continue

C If CFC surface, calculate longwave exchanges.
      do 1000 isur=1,nc
        if(icfcfl(icomp,isur).ge.1)then
          if(itrpas.eq.0) call qlwCFC(icomp,isur)
        end if
 1000 continue

C Prepare for next time step.
      do 34 i=1,nc
        nn=nndc(icomp,i)
        do 33 j=1,nn
          qcfc_lwF(icomp,i,j)=qcfc_lw(icomp,i,j,2)
  33    continue
  34  continue

C ******** Determine CFC convective gap resistances and convective 
C source terms for outdoor/indoor shades. 

C Set present time values and initialise future values.
C qcfc_cv(,,,1) relates to present time-row, (,,,2) to future
C time-row.
      do 35 i=1,nc
        nn=nndc(icomp,i)
        do 36 j=1,nn
          if(itrpas.eq.0)then
            qcfc_cv(icomp,i,j,1)=qcfc_cvF(icomp,i,j)   !future term from previous timestep
            qcfc_cvP(icomp,i,j) =qcfc_cvF(icomp,i,j)
          else
            qcfc_cv(icomp,i,j,1)=qcfc_cvP(icomp,i,j)   !if iterating use values from first iteration
            qcfc_cv(icomp,i,j,2)=qcfc_cvF(icomp,i,j)
          endif
         
          if(NSINC.eq.1) qcfc_cv(icomp,i,j,1)=0.
          if(itrpas.eq.0) qcfc_cv(icomp,i,j,2)=0.
 36     continue
 35   continue

C cfcRgap(,,,1) relates to present time-row, (,,,2) to future
C time-row.

C Iterate through each multi-layer construction.
      NC=NCONST(ICOMP)
      DO 37 ISUR=1,NC
        ne=nelts(icomp,isur)
        do 38 i=1,ne
C Set present time-row gap resistance values for initial time-step.
          if(NSINC.EQ.1) then !if timestep counter =1 
            cfcRgap(icomp,isur,i,1)=1.0
          else

            if(itrpas.eq.0)then
              cfcRgap(icomp,isur,i,1)=cfcRgapF(icomp,isur,i)   !future term from previous timestep
              cfcRgapP(icomp,isur,i) =cfcRgapF(icomp,isur,i)
            else
              cfcRgap(icomp,isur,i,1)=cfcRgapP(icomp,isur,i)   !if iterating use values from first iteration
              cfcRgap(icomp,isur,i,2)=cfcRgapF(icomp,isur,i)
            endif
              
          endif
          if(itrpas.eq.0) cfcRgap(icomp,isur,i,2)=1.0
 38     continue

! TESTING: set gap resistances to constant here.
!        cfcRgap(icomp,7,2,2)=1.79
!        cfcRgap(icomp,7,4,2)=2.82
 37   continue

C Set present time-row convective source term values for initial 
C time-step.
      if(NSINC.EQ.1) then !if timestep counter =1 
        q_airndConv_to_cfc(icomp,1)=0.
      else
          
        if(itrpas.eq.0)then
          q_airndConv_to_cfc(icomp,1)=q_airndConv_to_cfcF(icomp)    !future term from previous timestep
          q_airndConv_to_cfcP(icomp) =q_airndConv_to_cfcF(icomp)
        else
          q_airndConv_to_cfc(icomp,1)=q_airndConv_to_cfcP(icomp)    !if iterating use values from first iteration
          q_airndConv_to_cfc(icomp,2)=q_airndConv_to_cfcF(icomp)
        endif
            
      endif
          
      if(itrpas.eq.0) q_airndConv_to_cfc(icomp,2)=0.


C Calculate convective gap resistances and source terms
      if(itrpas.eq.0) call cfc_convection(icomp)


C Prepare for next time step
      do 39 i=1,nc
        nn=nndc(icomp,i)
        do 40 j=1,nn
          qcfc_cvF(icomp,i,j)=qcfc_cv(icomp,i,j,2)
  40    continue
  39  continue

      do 41 i=1,nc
        ne=nelts(icomp,i)
        do 42 j=1,ne
          cfcRgapF(icomp,i,j)=cfcRgap(icomp,i,j,2)
 42     continue
 41   continue
      q_airndConv_to_cfcF(icomp)=q_airndConv_to_cfc(icomp,2)

C If CFC, modify difference equation coefficients with time dependent
C gas gap resistances
      call CFC_time_dependent_Rgap(icomp) 

      return

      end


C ********************************************************************
C                              --qlwCFC--
C
C Determine source fluxes for CFC longwave exchange. Longwave radiant
C exchange occurs between CFC layers and across non-adjacent layers
C in the presence of a diathermanous layer (semi-transparent to 
C longwave radiation).It can also extend from any CFC layer to the 
C external surroundings and interior surfaces.
C
C ********************************************************************
      subroutine qlwCFC(icomp,isurf)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      integer nconst,nelts,ngaps,npgap
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      real TFC,QFC
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      real TFS,QFS
      COMMON/PREC1/SNA(MCOM,MS)
      real sna
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      real ei,ee,ai,ae
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      integer nnds,nndz,nndc,nndl
C      common/ts4/itsitr(msch),itrpas
C      integer itsitr,itrpas
C      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
C      integer ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

C common to hold indices of merit (U-value, SHGC) of CFC for reporting
      common/cfc_U_SHGC/hr_l_to_l(mcom,ms,me,me),
     &                  hr_cfc_to_sur(mcom,ms,me,ms),
     &                  hr_cfc_to_env(mcom,ms,me)
      real hr_l_to_l,hr_cfc_to_sur,hr_cfc_to_env

C CFC configuration flags
      integer iDiaOut,iDiaIn

C misc local variables
      integer nc,ne,icfctp,icount,nMatSize,i,j,icomp,isurf
      integer nExtPart,nIntPart,nCFCpart,k,inode,nnod,nn,n,l
      real fnnod

C local variables
      real epsf(ME)        ! front emissivity
      real epsb(ME)        ! back emissivity
      real tnd(MN)         ! nodal temp [K]

C lw heat transfer between pairs of surfaces
      real q(ME,ME)        ! heat tsfer between CFC layers [W]
      real qZnsur(ME,MS)   ! heat tsfer rate between CFC and zone surfaces [W]
      real qZnsurTot(MS)   ! total heat tsfer rate for internal zone surface due to exchange with CFC [W]
      real qlwtot(ME)      ! total heat tsfer rate for CFC layer [W]
      real qenv(ME)        ! heat tsfer rate between CFC layers and environment[W]
      real sigma           ! stefan-boltzman constant [W/m^2 K^4]

C infinite plates radiant heat transfer coefficient for TESTING
C      real hr_inf_plates  ! [W/m2 K] 

      !logical VBxst       !true if slat-type blind present in CFC type

      parameter(sigma=5.6704E-8)

      nc=NCONST(icomp)
      icfctp=icfcfl(icomp,isurf)
      ne=NELTS(icomp,isurf)

      iDiaOut=0
      iDiaIn=0
      icount=0
      !VBxst=.false.

C Initialize source terms
      do 1 i=1,ne
        do 2 j=i,ne
          q(i,j)=0.
  2     continue
        do 3 j=1,nc
          qZnsur(i,j)=0.
  3     continue
        qlwtot(i)=0.
  1   continue

      do 4 i=1,nc
        qZnsurTot(i)=0.
  4   continue

      do 5 i=1,ne,2
        qenv(i)=0.
  5   continue

C does CFC type contain a slat-type blind layer? 
C      do 8 i=1,ne,1
C        if(icfcltp(icomp,icfctp,i).ge.2)then
C          VBxst=.true.
C        else
C          VBxst=.false.
C        end if
C  8   continue

c *******************************************************************
C STEP 1: establish matrix size depending on CFC configuration
C     case 1 - no external or internal diathermanous layers
C     case 2 - internal diathermanous layer exists
C     case 3 - external diathermanous layer exists, 
C     case 4 - internal and external diathermanous layers exist

C set CFC configuration flags used to establish matrix structure
      if(icfcltp(icomp,icfctp,1).ge.2)then
        iDiaOut=1
      end if

      if(icfcltp(icomp,icfctp,ne).ge.2)then
        iDiaIn=1
      end if

C only carry out Script F shape factor calculation at first timestep and 
C if New slat angle is different from Old slat angle. 
!       IF((NSINC.eq.1).or.
!      &   (VBxst.and.vb_phiNew(icomp,icfctp).ne.
!      &   .vb_phiOld(icomp,icfctp)))then
!         write(93,*)
!         write(93,*)'SLAT ANGLE IS CHANGING, CALLING FSSOLVE'

C count number of CFC surfaces that exchange LW radiation,skipping airgaps
      do 10 i=1,ncfc_el(icomp,icfctp)
        if(icfcltp(icomp,icfctp,i).gt.0)then
          icount=icount+1
        else
          !air gap, do nothing
        end if
  10  continue

C two radiating surfaces per layer
      nCFCPart=icount*2

C establish size of each matrix partition (exterior block, CFC block and
C interior zone block, assuming CFC is external, IE=0) 
      if(iDiaOut.eq.0.and.iDiaIn.eq.0)then      !no internal or external diathermanous layers
        nExtPart=0
        nIntPart=0
      elseif(iDiaOut.eq.1.and.iDiaIn.eq.0)then     !external diathermanous layer
        nExtPart=1
        nIntPart=0
      elseif(iDiaOut.eq.0.and.iDiaIn.eq.1)then     !internal diathermanouslayer
        nExtPart=0
        nIntPart=nc                          !no. of constructions in current zone
      elseif(iDiaOut.eq.1.and.iDiaIn.eq.1)then     !internal and external diathermanous layer
        nExtPart=1
        nIntPart=nc
      end if

      nMatSize=nExtPart+nCFCPart+nIntPart    !square matrix size 

C *******************************************************************
C STEP 2:
C call FSsolve to set-up and solve matrix and determine script F 
C exchange factors
      call FSsolve(icomp,isurf,icfctp,iDiaIn,iDiaOut,
     &  nMatSize,nExtPart,nCFCPart)

!       END IF

C If slat angle is constant, use the same Script F configuration 
C factors as previous timestep. These will be constant as long as 
C slat angle doesn't change. 


C *******************************************************************
C STEP 3:
C Now know all script F configuration factors.
C Determine heat exchange between each pair of surfaces in CFC 
C construction and internal zone surfaces.

C assign nodal temperatures and convert to kelvin
      nn=nndc(icomp,isurf)
      do 30 i=1,nn
C        if(i.lt.nn)then
           tnd(i)=tfc(icomp,isurf,i)+273.15
C        else
C          tsrf=tfs(icomp,isurf)+273.15
C        end if
  30  continue

C assign emittance to front and back surfaces of cfc type itp
      do 60 n=1,ne,2
        epsf(n)=rlwEF(icomp,icfctp,n)
        epsb(n)=rlwEB(icomp,icfctp,n)
  60  continue

C STEP 3a:
C Determine heat exchange between back and front surfaces of one layer
C to back and front surfaces of another layer, for each pair of layers
C in the CFC.
C
C Also determine radiant heat transfer coefficients - not used currently
C
C This method treats each layer as one node, so that the middle node 
C temperature is used. Assuming glass/shade layers have low thermal mass,
C the nodal temperatures of each layer are very close so this works well.

      do 70 j=1,nCFCPart,2
        do 80 k=j,nCFCPart,2
C PRIMARY FORM OF THE EQUATION
!         q(j,k)=
!      &  (epsf(j)*sigma*(tnd(j*2)**4)*FSff(icomp,isurf,j,k)*epsf(k)-
!      &  epsf(k)*sigma*(tnd(k*2)**4)*FSff(icomp,isurf,j,k)*epsf(j)+
!      &  epsb(j)*sigma*(tnd(j*2)**4)*FSbf(icomp,isurf,j,k)*epsf(k)-
!      &  epsf(k)*sigma*(tnd(k*2)**4)*FSbf(icomp,isurf,j,k)*epsb(j)+
!      &  epsf(j)*sigma*(tnd(j*2)**4)*FSfb(icomp,isurf,j,k)*epsb(k)-
!      &  epsb(k)*sigma*(tnd(k*2)**4)*FSfb(icomp,isurf,j,k)*epsf(j)+
!      &  epsb(j)*sigma*(tnd(j*2)**4)*FSbb(icomp,isurf,j,k)*epsb(k)-
!      &  epsb(k)*sigma*(tnd(k*2)**4)*FSbb(icomp,isurf,j,k)*epsb(j))*
!      &  -1.0*SNA(icomp,isurf)

C SIMPLIFIED VERSION OF EQUATION ABOVE. THE TWO EQUATIONS ARE EQUAL.
          q(j,k)=
     &      (epsf(j)*FSff(icomp,isurf,j,k)*epsf(k)+
     &       epsb(j)*FSbf(icomp,isurf,j,k)*epsf(k)+
     &       epsf(j)*FSfb(icomp,isurf,j,k)*epsb(k)+
     &       epsb(j)*FSbb(icomp,isurf,j,k)*epsb(k))*
     &       sigma*((tnd(j*2)**4)-(tnd(k*2)**4))*
     &       (-1.0*SNA(icomp,isurf))


C DETERMINE RADIANT HEAT TRANSFER COEFF. FOR INDICES OF MERIT 
C REPORTING. CURRENTLY NOT USED. 

          if(j.ne.k)then
            hr_l_to_l(icomp,isurf,j,k)=
     &      (epsf(j)*FSff(icomp,isurf,j,k)*epsf(k)+
     &      epsb(j)*FSbf(icomp,isurf,j,k)*epsf(k)+
     &      epsf(j)*FSfb(icomp,isurf,j,k)*epsb(k)+
     &      epsb(j)*FSbb(icomp,isurf,j,k)*epsb(k))*
     &      sigma*((tnd(j*2)**2)+(tnd(k*2)**2))*
     &      (tnd(j*2)+tnd(k*2))
          else
            hr_l_to_l(icomp,isurf,j,k)=0.
          end if

C TESTING: for two glazing surfaces, use infinite plates equation
C          to test against the above equation
!          hr_inf_plates=(sigma*(tnd(j*2)+tnd(k*2))*
!      &      ((tnd(j*2)**2)+(tnd(k*2)**2)))/((1/epsb(j))+(1/epsf(k))-1)

  80    continue
  70  continue

C Add up longwave exchange fluxes to determine layer source terms
      do 90 j=1,nCFCPart,2
        qlwtot(j)=0.
        do 100 k=j,nCFCPart,2
          qlwtot(j)=qlwtot(j)+q(j,k)
  100   continue
        do 110 l=j,1,-2
          if(l.ne.j)then
            qlwtot(j)=qlwtot(j)-q(l,j)
          end if
  110   continue
   90 continue

C STEP 3b:
C If interior diathermanous layer exists (i.e., iDiaIn=1), 
C determine heat exchange between each CFC layer and interior 
C zone surface. 
      IF(iDiaIn.eq.1)then
        do 120 j=1,nCFCPart,2
          do 130 k=1,nc
            nn=nndc(icomp,k)
            if(k.ne.isurf)then !omit interior blind surface

C PRIMARY FORM OF THE EQUATION
!            qZnsur(j,k)=
!      &    (epsf(j)*sigma*(tnd(j*2)**4)*FSfZn(icomp,isurf,j,k)
!      &    *EI(icomp,k)-
!      &    EI(icomp,k)*sigma*((tfs(icomp,k)+273.15)**4)*
!      &    FSfZn(icomp,isurf,j,k)*epsf(j)+
!      &    epsb(j)*sigma*(tnd(j*2)**4)*FSbZn(icomp,isurf,j,k)
!      &    *EI(icomp,k)-
!      &    EI(icomp,k)*sigma*((tfs(icomp,k)+273.15)**4)*
!      &    FSbZn(icomp,isurf,j,k)*epsb(j))*
!      &    -1.0*SNA(icomp,k)

C SIMPLIFIED VERSION OF EQUATION ABOVE. THE TWO EQUATIONS ARE EQUAL.
              qZnsur(j,k)=
     &         (epsf(j)*FSfZn(icomp,isurf,j,k)*EI(icomp,k)+
     &          epsb(j)*FSbZn(icomp,isurf,j,k)*EI(icomp,k))*
     &         (SNA(icomp,k)*(-1.0))*sigma*
     &         ((tnd(j*2)**4)-((tfs(icomp,k)+273.15)**4))


C DETERMINE RADIANT HEAT TRANSFER COEFF. FOR INDICES OF MERIT 
C REPORTING.(CURRENTLY NOT USED)
              hr_cfc_to_sur(icomp,isurf,j,k)=
     &         (epsf(j)*FSfZn(icomp,isurf,j,k)*EI(icomp,k)+
     &          epsb(j)*FSbZn(icomp,isurf,j,k)*EI(icomp,k))*
     &         (SNA(icomp,k)/SNA(icomp,isurf))*sigma*
     &         ((tnd(j*2)**2)+((tfs(icomp,k)+273.15)**2))*
     &         (tnd(j*2)+(tfs(icomp,k)+273.15))

            else 
              qZnsur(j,k)=0.
              hr_cfc_to_sur(icomp,isurf,j,k)=0.
            end if

 130      continue
 120    continue

C Initialize output data
        q_cfclw_toRoom(icomp,isurf)=0.0

C Add longwave exchanges with interior zone surfaces to CFC layer
C source terms.
        do 140 j=1,nCFCPart,2
          do 150 k=1,nc
             if(k.ne.isurf)then !omit interior blind surface
               qlwtot(j)=qlwtot(j)+qZnsur(j,k)
               q_cfclw_toRoom(icomp,isurf)=
     &           q_cfclw_toRoom(icomp,isurf)+(qZnsur(j,k)/
     &           SNA(icomp,isurf)) ! save for output and divide by area to get [W/m2}
             end if
 150       continue
 140     continue

C Add longwave exchanges with CFC layers to interior zone surface
C node. 
         do 160 i=1,nc
           do 170 j=1,nCFCPart,2
             if(i.ne.isurf)then
               qZnsurTot(i)=qZnsurTot(i)-qZnsur(j,i)
             else
               qZnsurTot(i)=0.
             end if
 170       continue
 160     continue

      END IF

C STEP 3c:
C Determine longwave exchange between each CFC layer and external
C surroundings. The mean radiant temperature (teqv) consists
C of buildings, sky and ground components. 
      IF(iDiaOut.eq.1)then
        do 180 j=1,nCFCPart,2
          qenv(j)=
     &    (epsf(j)*sigma*(tnd(j*2)**4)*FSfenv(icomp,isurf,j)-
     &    sigma*(teqv(isurf)**4)*FSfenv(icomp,isurf,j)*epsf(j)+
     &    epsb(j)*sigma*(tnd(j*2)**4)*FSbenv(icomp,isurf,j)-
     &    sigma*(teqv(isurf)**4)*FSbenv(icomp,isurf,j)*epsb(j))
     &    *SNA(icomp,isurf)*(-1.0)


C Radiant heat transfer coefficient from each CFC layer to external
C surroundings (currently not used)
          hr_cfc_to_env(icomp,isurf,j)=
     &    (epsf(j)*FSfenv(icomp,isurf,j)+
     &     epsb(j)*FSbenv(icomp,isurf,j))*sigma*
     &     ((tnd(j*2)**2)+(teqv(isurf)**2))*
     &     (tnd(j*2)+teqv(isurf))

 180    continue


C Initialize output data
        q_cfclw_toExt(icomp,isurf)=0.0

C Add external longwave exchanges to CFC layer source terms
        do 190 i=1,nCFCPart,2
          qlwtot(i)=qlwtot(i)+qenv(i)
          q_cfclw_toExt(icomp,isurf)=
     &    q_cfclw_toExt(icomp,isurf)+(qenv(i)/SNA(icomp,isurf)) !save for output and divide by area to get [W/m2]
 190    continue

      END IF

C STEP 4: Split layer source terms to nodal source terms 
C and to internal surface nodes of all zone surfaces which exchange
C longwave radiation with CFC

C Assign heat injection terms to current CFC layers
      ne=nelts(icomp,isurf)
      inode=1
      do 200 i=1,ne

C Divide heat injection terms by surface area.
C ('W' time-dependent coefficients in MZSETU require [W/m^2]).
        qlwtot(i)=qlwtot(i)/SNA(icomp,isurf)

        nnod=nndl(icomp,isurf,i)
        fnnod=float(nnod)
        qcfc_lw(icomp,isurf,inode,2)=qcfc_lw(icomp,isurf,inode,2)+
     &                               qlwtot(i)/(fnnod*2.)
        do 210 j=2,nnod
          inode=inode+1
          qcfc_lw(icomp,isurf,inode,2)=qcfc_lw(icomp,isurf,inode,2)+
     &                                 qlwtot(i)/fnnod
  210   continue

        inode=inode+1
        qcfc_lw(icomp,isurf,inode,2)=qcfc_lw(icomp,isurf,inode,2)+
     &                               qlwtot(i)/(fnnod*2.)
  200 continue

      do 220 i=1,nc

C Divide heat injection terms by surface area.
        qZnsurTot(i)=qZnsurTot(i)/SNA(icomp,i)

        nn=nndc(icomp,i)
        ne=nelts(icomp,i)
        inode=nn
        nnod=nndl(icomp,i,ne)
        fnnod=float(nnod)

C If construction is CFC and the inside layer is a slat-type blind
C or any other type of shading device
C then assign source terms to all nodes in the inside layer.
C Otherwise assign heat injection to only the inside surface node
C for non CFC surfaces.
        icfctp=icfcfl(icomp,i)

        IF(icfctp.gt.0)then
          if(icfcltp(icomp,icfctp,ne).ge.2)then

            qcfc_lw(icomp,i,inode,2)=qcfc_lw(icomp,i,inode,2)+
     &                               qZnsurTot(i)/(fnnod*2.)
            do 230 j=nn-1,nn-nnod+1,-1
              inode=inode-1
              qcfc_lw(icomp,i,inode,2)=qcfc_lw(icomp,i,inode,2)+
     &                                 qZnsurTot(i)/fnnod
  230       continue
            inode=inode-1
            qcfc_lw(icomp,i,inode,2)=qcfc_lw(icomp,i,inode,2)+
     &                               qZnsurTot(i)/(fnnod*2.)
          else
            qcfc_lw(icomp,i,inode,2)=qcfc_lw(icomp,i,inode,2)+
     &                               qZnsurTot(i)
          endif

        ELSE
          qcfc_lw(icomp,i,inode,2)=qcfc_lw(icomp,i,inode,2)+
     &                             qZnsurTot(i)
        END IF
  220 continue

      return
      end


C ********************************************************************
C                            --FSsolve--
C
C Determine exchange factors for use in longwave source term calc.
C
C General theory of exchange factor method outlined in Appendix C of:
C
C Lomanowski, B.A. (2008) 'Implementation of Window Shading Models
C into Dynamic Whole-Building Simulation', MASc Thesis, University
C of Waterloo.
C 
C Intra-constructional exchange factors for each pair of CFC layers
C are stored in COMMON/CFC_FS
C Exchange factors between CFC layers and external surrounds are 
C stored in COMMON/CFC_FSenv
C Exchange factors between CFC layers and internal surfaces are 
C stored in COMMON/CFC_FSZn
C
C ********************************************************************
      subroutine FSsolve(icomp,isurf,icfctp,iDiaIn,iDiaOut,
     &                  N,nExtPart,nCFCPart)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      integer nconst,nelts,ngaps,npgap
      COMMON/V2/CFB(MCOM,MST,MST)
      real cfb
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      real ei,ee,ai,ae

C misc local variables
      integer icomp,isurf,icfctp,nc,i,j,indx
      integer iDiaIn,iDiaOut,irow,icol,k,indxj,ij,jk
      integer N                   ! matrix size
      integer nExtPart,nCFCPart   ! matrix partition sizes
      real B(N,N+2),tmpB(N,N+2)   ! radiosity matrix
      real JsolB(N)               ! solution matrix

      nc=NCONST(icomp)

C initialize matrix and fill it with zeros
C diagonal elements are set to 1
      do 10 i=1,N
        do 20 j=1,N+1
          if(i.eq.j.and.j.ne.N+1)then            
            B(i,j)=1. !diagonal elements set to 1      
          else
            B(i,j)=0.      
          end if
  20    continue
  10  continue


C populate CFC block of 'A' matrix with longwave radiative properties of 
C CFC layers each non-gas gap layer has two radiating surfaces, front 
C (facing outdoors),back (facing indoors)
      do 30 i=1,nCFCPart,2 

        indx=nExtPart+i   !matrix index, account for external partition (ie.if nExtPart>0)

C.......reflectances and trasmittances
        if(i.eq.1)then
C.........include LW exchange with environment if iDiaOut=1
          if(iDiaOut.eq.1)then
            B(indx,indx-1)=-(1.-rlwEF(icomp,icfctp,i)-
     &                     rlwT(icomp,icfctp,i))
            B(indx+1,indx-1)=-rlwT(icomp,icfctp,i)
          endif

          B(indx,indx+2)=-rlwT(icomp,icfctp,i)
          B(indx+1,indx+2)=-(1.-rlwEB(icomp,icfctp,i)-
     &                     rlwT(icomp,icfctp,i))
        elseif(i.eq.nCFCPart-1)then
          B(indx,indx-1)=-(1.-rlwEF(icomp,icfctp,i)-
     &                   rlwT(icomp,icfctp,i))
          B(indx+1,indx-1)=-rlwT(icomp,icfctp,i)
        else
          B(indx,indx-1)=-(1.-rlwEF(icomp,icfctp,i)-
     &                   rlwT(icomp,icfctp,i))
          B(indx,indx+2)=-rlwT(icomp,icfctp,i)
          B(indx+1,indx-1)=-rlwT(icomp,icfctp,i)
          B(indx+1,indx+2)=-(1.-rlwEB(icomp,icfctp,i)-
     &                     rlwT(icomp,icfctp,i))
        end if

  30  continue

C Fill in the rest of matrix for zone surfaces interaction with indoor blind
      if(iDiaIn.eq.1)then
        do 41 i=1,nc
          do 51 j=1,nc
            IF(j.ne.isurf)THEN
              irow=nExtPart+nCFCPart+i
              icol=nExtPart+nCFCPart+j

              if(i.eq.1)then
                B(irow-1,icol)=(1.-rlwEB(icomp,icfctp,irow-2)-
     &                         rlwT(icomp,icfctp,irow-2))
     &                         *CFB(icomp,isurf,j)*(-1.)
                B(irow-2,icol)=rlwT(icomp,icfctp,irow-2)
     &                         *CFB(icomp,isurf,j)*(-1.)
              end if

              if(i.ne.isurf)then

C...............The following does not fully account for the transparency of another CFC
C...............indoor shade in the zone.
C...............Assume reflectance of the 'other' blind is 1-EI. Otherwise lw exchange 
C...............between both CFC indoor layers is necessary.
                if(j.eq.1)then
                  B(irow,icol-1)=(1.-EI(icomp,i))
     &                           *CFB(icomp,i,isurf)*(-1.)
                end if
                if(irow.ne.icol)then
                  B(irow,icol)=(1.-EI(icomp,i))
     &                         *CFB(icomp,i,j)*(-1.) !fill in bottom block
                end if

              end if
            END IF
  51      continue
  41    continue
  
      end if

C MATRIX SET-UP COMPLETE. NOW TURN ON EACH SURFACE AND DETERMINE SCRIPT F 
C EXCHANGE FACTORS

C Solve for exchange factors
      DO 60 i=1,nCFCPart

        indx=nExtPart+i   !matrix index, account for external partition (ie.if nExtPart>0)

C.......copy 'B' matrix to 'tmpB' matrix - use tmpB for solving and retain B 
C.......matrix for next iteration
        do 701 j=1,N
          do 801 k=1,N+1
            tmpB(j,k)=B(j,k)
  801     continue
  701   continue

C.......TURN ON SOURCE TERM
        tmpB(indx,N+1)=1.
        if(i.ne.1)then
          tmpB(indx-1,N+1)=0.
        end if

C.......solve matrix with only one surface 'turned on'
        call SOLMATS(N,tmpB,JsolB)

 
C.......Assign exchange factors based on radiosity solution for 'turned on'
C.......surface to all other surfaces in enclosure.
C
C.......Each non gas-gap layer in CFC has two surfaces, front and back, 
C.......layers are indexed 1,3,5,7.., skipping air gaps which are indexed 2,4,6,8...
C
C.......Need to distinguish between front and back surfaces.
        IF(mod(i,2).ne.0)then   !source is front surface

C.........Calculate exchange factor for intra-constructional CFC layers.
          do 100 j=i,nCFCPart,2
            indxj=j+nExtPart
            if(j.eq.i.and.j.ne.nCFCPart-1)then 
              FSff(icomp,isurf,i,j)=0.
              FSfb(icomp,isurf,i,j)=JsolB(indxj+2)
            elseif(j.ge.nCFCPart-1)then
              if(j.eq.i)then
                FSff(icomp,isurf,i,j)=0.
              else
                FSff(icomp,isurf,i,j)=JsolB(indxj-1)
              end if
              if(iDiaIn.eq.1)then
                !irradiance on back of inside blind
                FSfb(icomp,isurf,i,j)=0.   !initialize

                do 115 jk=1,nc
                  if(jk.ne.isurf)then
                    FSfb(icomp,isurf,i,j)=
     &                FSfb(icomp,isurf,i,j)+
     &                CFB(icomp,isurf,jk)*JsolB(nExtPart+nCFCPart+jk)    
                  end if
 115            continue

              else
                FSfb(icomp,isurf,i,j)=0.
              end if
            else
              FSff(icomp,isurf,i,j)=JsolB(indxj-1)
              FSfb(icomp,isurf,i,j)=JsolB(indxj+2)
            end if
100       continue

C.........Calculate exchange factor from each CFC layer to external
C.........surroundings.
          if(iDiaOut.eq.1)then
            FSfenv(icomp,isurf,i)=JsolB(2) !radiosity of outside surface of CFC
          end if

C.........Calculate exchange factor from each CFC layer to each internal
C.........zone surface.
          if(iDiaIn.eq.1)then

            do 111 ij=1,nc
              FSfZn(icomp,isurf,i,ij)=0.   !initialize

              if(ij.ne.isurf)then
                do 112 jk=1,nc
                  if(jk.ne.isurf)then
                    FSfZn(icomp,isurf,i,ij)=
     &                FSfZn(icomp,isurf,i,ij)+
     &                CFB(icomp,ij,jk)
     &                *JsolB(nExtPart+nCFCPart+jk)
                  end if
 112            continue
                FSfZn(icomp,isurf,i,ij)=
     &            FSfZn(icomp,isurf,i,ij)+
     &            CFB(icomp,ij,isurf)*JsolB(nExtPart+nCFCPart)
              end if

 111        continue

          end if

        ELSEIF(mod(i,2).eq.0)then  !source is back surface
                
C.........Calculate exchange factor for intra-constructional CFC layers.
          do 110 j=i,nCFCPart,2
            indxj=j+nExtPart
            if(j.eq.i.and.j.lt.nCFCPart-2)then 
              FSbb(icomp,isurf,i-1,j-1)=0.
              FSbf(icomp,isurf,i-1,j+1)=JsolB(indxj)
              FSbb(icomp,isurf,i-1,j+1)=JsolB(indxj+3)
            elseif(j.eq.nCFCPart-2)then
              FSbf(icomp,isurf,i-1,j+1)=JsolB(indxj)
              if(iDiaIn.eq.1)then
                !irradiance on back of inside blind
                FSbb(icomp,isurf,i-1,j+1)=0.   !initialize
                do 116 jk=1,nc
                  if(jk.ne.isurf)then
                    FSbb(icomp,isurf,i-1,j+1)=
     &                FSbb(icomp,isurf,i-1,j+1)+
     &                CFB(icomp,isurf,jk)
     &                *JsolB(nExtPart+nCFCPart+jk)                        
                  end if
 116            continue
              else
                FSbb(icomp,isurf,i-1,j+1)=0.
              end if
            elseif(i.eq.j.and.j.eq.nCFCPart)then
              FSbb(icomp,isurf,i-1,j-1)=0.
            elseif(i.ne.j.and.j.ne.nCFCPart)then
              FSbf(icomp,isurf,i-1,j+1)=JsolB(indxj)
              FSbb(icomp,isurf,i-1,j+1)=JsolB(indxj+3)
            else
             !do nothing
            end if
 110      continue

C.........Calculate exchange factor from each CFC layer to external
C.........surroundings.
          if(iDiaOut.eq.1)then
            FSbenv(icomp,isurf,i-1)=JsolB(2) !radiosity of outside surface of CFC
          end if
 
C Calculate exchange factor from each CFC layer to each internal
C zone surface.
          if(iDiaIn.eq.1)then

            do 113 ij=1,nc
              FSbZn(icomp,isurf,i-1,ij)=0.   !initialize

              if(ij.ne.isurf)then

                do 114 jk=1,nc
                  if(jk.ne.isurf)then
 
                    FSbZn(icomp,isurf,i-1,ij)=
     &                FSbZn(icomp,isurf,i-1,ij)+
     &                CFB(icomp,ij,jk)*JsolB(nExtPart+nCFCPart+jk)
       
                  end if
 114            continue

                FSbZn(icomp,isurf,i-1,ij)=
     &            FSbZn(icomp,isurf,i-1,ij)+
     &            CFB(icomp,ij,isurf)*JsolB(nExtPart+nCFCPart)
              end if

 113        continue

          end if

        END IF

  60  CONTINUE

      return
        
      end


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

      COMMON/OUTIN/IUOUT,IUIN
      integer iuout,iuin

C need thickness of glass layers for Fresnel calcs
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      real thrmli

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      integer ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

      COMMON/CFCsolarS/RadIncDirS(mcom,ms),RadIncDifS(mcom,ms),
     &      transBB_S(mcom,ms),transD_S(mcom,ms),refl_S(mcom,ms),
     &      issur_self_shd(mcom,ms),proang_s(mcom,mcfc,ms),
     &      qtmca_ext(mcom,ms,mn),omega_v_s(mcom,mcfc,ms),
     &      omega_h_s(mcom,mcfc,ms)

      real RadIncDirS,RadIncDifS,transBB_S,transD_S,refl_S
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
C                        --solar_multilayer--
C
C Calculates reflected, transmitted and absorbed solar fluxes for a 
C glazing/shading multilayer system. A solar flux balance is 
C established for each layer, including beam-beam fluxes, beam-diffuse 
C fluxes due to scattering shading layers, and diffuse-diffuse fluxes.
C
C Details in:
C Wright, J.L., Kotey, N.A. (2006) 'Solar Absorption by Each Element 
C in a Glazing/Shading Layer Array',ASHRAE Transactions, Vol. 112, 
C Pt. 2. pp. 3-12.
C
C INPUT:
C GBM - External beam solar irradiance [W/m2]
C GDF - External diffuse solar irradiance [W/m2]
C IBM - Internal beam solar irradiance [W/m2]
C IDF - Internal diffuse solar irradiance [W/m2]
C 
C OUTPUT:
C TRANSBB_SYS - Total transmitted beam solar flux [W/m2]
C TRANSD_SYS - Total transmitted diffuse solar flux [W/m2]
C REFL_SYS - Total reflected solar flux (beam+diffuse) [W/m2]
C AbsSol - Absorbed solar flux at each layer [W/m2]
C
C ********************************************************************
      subroutine solar_multilayer(icalc_mode,icomp,isurf,icfctp,
     &           GBM,GDF,IBM,IDF,
     &           TRANSBB_SYS,TRANSD_SYS,REFL_SYS,AbsSol)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"

      INTEGER LayNo,n,m,i,icomp,isurf,icfctp,icalc_mode
      INTEGER itype
      REAL GBM, GDF, IDF,IBM      
      REAL TRANSBB_SYS, TRANSBD_SYS, TRANSDD_SYS, REFL_SYS
      REAL TRANSD_SYS
!      REAL TRANS_SYS
      REAL AbsSol
      REAL rfdd,rbdd,tfdd,tbdd
      REAL rfbd,rbbd,tfbd,tbbd
      REAL rfbb,rbbb,tfbb,tbbb
      REAL BB,BD,DD,D,SS
      REAL Bminus,Bplus
      REAL Dminus,Dplus
      REAL ae,ap,aw,bp
      INTEGER xx 
      DIMENSION rfdd(me),rbdd(me),tfdd(me),tbdd(me)   ! SW Layer Properties
      DIMENSION rfbd(me),rbbd(me),tfbd(me),tbbd(me)   ! SW Layer Properties
      DIMENSION rfbb(me),rbbb(me),tfbb(me),tbbb(me)   ! SW Layer Properties
      DIMENSION BB(me*2+2),BD(me*2+2),DD(me*2+2),D(me*2+2),SS(me)
      DIMENSION AbsSol(me)
      DIMENSION Bminus(me*2+2),Bplus(me*2+2)
      DIMENSION Dminus(me*2+2),Dplus(me*2+2)
      DIMENSION ae(me*2+2),ap(me*2+2),aw(me*2+2),bp(me*2+2)

      LayNo=ncfc_el(icomp,icfctp)


C Assign optical properties for each layer from commons to temp arrays
      do 10 i=1,LayNo
            rfbb(i)=SolRFbb(icomp,isurf,icfctp,i)
            rbbb(i)=SolRBbb(icomp,isurf,icfctp,i)
            tfbb(i)=SolTFbb(icomp,isurf,icfctp,i)
            tbbb(i)=SolTBbb(icomp,isurf,icfctp,i)
            rfbd(i)=SolRFbd(icomp,isurf,icfctp,i)
            rbbd(i)=SolRBbd(icomp,isurf,icfctp,i)
            tfbd(i)=SolTFbd(icomp,isurf,icfctp,i)
            tbbd(i)=SolTBbd(icomp,isurf,icfctp,i)

            itype=icfcltp(icomp,icfctp,i)

C...........Set sky and ground diffuse properties for slat blinds, if requested
            if(itype.eq.2.and.icalc_mode.eq.i_sky)then
              rfdd(i)=SolRFskydd(icomp,isurf,icfctp,i)
              rbdd(i)=SolRBskydd(icomp,isurf,icfctp,i)
              tfdd(i)=SolTFskydd(icomp,isurf,icfctp,i)
              tbdd(i)=SolTBskydd(icomp,isurf,icfctp,i)
            elseif(itype.eq.2.and.icalc_mode.eq.i_ground)then
              rfdd(i)=SolRFgrddd(icomp,isurf,icfctp,i)
              rbdd(i)=SolRBgrddd(icomp,isurf,icfctp,i)
              tfdd(i)=SolTFgrddd(icomp,isurf,icfctp,i)
              tbdd(i)=SolTBgrddd(icomp,isurf,icfctp,i)
            else
              rfdd(i)=SolRFdd(icomp,isurf,icfctp,i)
              rbdd(i)=SolRBdd(icomp,isurf,icfctp,i)
              tfdd(i)=SolTFdd(icomp,isurf,icfctp,i)
              tbdd(i)=SolTBdd(icomp,isurf,icfctp,i)
            end if

 10   continue

!MULTILAYER CALCULATION
      n=LayNo+2
      m=2*n-4
!Beam fluxes
      CALL SETCoef(tbbb,tfbb,rbbb,rfbb,GBM,n,aw,ap,ae,bp)
! back beam source...used for interior insolation distribution
      bp(1)=-IBM
      CALL TDMAsol(ae,ap,aw,bp,n,BB)
      BB(m+1)=rfbb(1)*GBM+tbbb(1)*BB(m)
!Diffuse-beam fluxes      
      CALL SETCoef(tbdd,tfdd,rbdd,rfdd,GBM,n,aw,ap,ae,bp)
      bp(1)=-0.00001*BB(1)
      bp(2)=-(rbbd(n-2)*BB(2)+tfbd(n-2)*BB(3))
      DO i=2,n-2,1
            xx=2*i-1
            bp(xx)=-(rfbd(n-i)*BB(2*i-1)+tbbd(n-i)*BB(2*i-2))
            xx=2*i
            bp(xx)=-(rbbd(n-i-1)*BB(2*i)+tfbd(n-i-1)*BB(2*i+1))
      ENDDO
      i=n-2
      xx=2*i
      ae(xx)=0.
      bp(xx)=-(rbbd(1)*BB(2*i)+tfbd(1)*(GBM))
      i=n-1
      xx=2*i-1
      bp(xx)=-((rfbd(1))*(GBM)+(tbbd(1))*(BB(2*i-2)))
      CALL TDMAsol(ae,ap,aw,bp,n,BD)
      BD(m+1)=rfbd(1)*GBM+tbbd(1)*BB(m)+tbdd(1)*BD(m)
!Diffuse fluxes
      CALL SETCoef(tbdd,tfdd,rbdd,rfdd,GDF,n,aw,ap,ae,bp)
      bp(1)=-IDF
      CALL TDMAsol(ae,ap,aw,bp,n,DD)
      DD(m+1)=rfdd(1)*GDF+tbdd(1)*DD(m)
      DO i=1,m,1
            D(i)=DD(i)+BD(i)
      ENDDO
      D(m+1)=DD(m+1)+BD(m+1)
!Transmittance and Reflectance Calculation
      TRANSBB_SYS = BB(1)! /GBM
      TRANSBD_SYS = BD(1)! /GBM
      TRANSDD_SYS = DD(1)! /GDF
      TRANSD_SYS=TRANSDD_SYS+TRANSBD_SYS
!      TRANS_SYS =(BB(1)+D(1)) !/(GBM+GDF)
      REFL_SYS=(BB(m+1)+D(m+1))! /(GBM+GDF)
!Layer Absorptance Calculation
      DO i=1,n-2,1
            xx=2*i-1
            Bminus(n-i)=BB(xx)
            Dminus(n-i)=D(xx)
            xx=2*i
            Bplus(n-i)=BB(xx)
            Dplus(n-i)=D(xx)
      ENDDO
      Bminus(1)=GBM
      Bplus(1)=BB(m+1)
      Dminus(1)=GDF
      Dplus(1)=D(m+1)
      DO i=1,n-2,1
          SS(i)=(Bminus(i)-Bplus(i)+Bplus(i+1)-Bminus(i+1)+ Dminus(i)
     &                  -Dplus(i)+Dplus(i+1)-Dminus(i+1)) !/ (GBM+GDF)
          AbsSol(i)=SS(i)
      ENDDO

      IF (GBM.lt.0.01) THEN
            GBM=0.0     ! reset back to zero
      ENDIF

      RETURN
      END


C ********************************************************************
C                        --cfc_convection--
C
C Compute time/temperature dependent convective gap resistances. 
C For indoor/outdoor slat-type blinds, calculate convective source 
C terms due to heat exchange with indoor/outdoor air.
C 
C Current convection models require that shade layer is adjacent to
C glass layer(s) or is vented to inside/outside. Two contiguous shade
C layers are not permitted. 
C
C Refer to Chapter 4 in: 
C Lomanowski, B.A. (2008) 'Implementation of Window Shading Models
C into Dynamic Whole-Building Simulation', MASc Thesis, University
C of Waterloo.
C
C ********************************************************************

      subroutine cfc_convection(icomp)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"
      COMMON/OUTIN/IUOUT,IUIN
      integer iuout,iuin

C      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
C      integer ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      integer nconst,nelts,ngaps,npgap
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      real hcip,hcif,hcop,hcof
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      real thrmli
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      real TFC,QFC
c      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
c      real TFS,QFS
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      integer nnds,nndz,nndc,nndl
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &ARF(MCOM,MS)
      integer ie
      real atp,atf,arp,arf
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      real tfa,qfa
      COMMON/PREC1/SNA(MCOM,MS)
      real sna
      common/CORIND/ICORI(MCOM,MS),ICORE(MCOM,MS)
      integer icori,icore
      COMMON/PREC1I/APRAT(MCOM,MS),HEIGHT(MCOM,MS)
      real aprat,height

      integer nc,nn,ne,i,j,icomp,isur,icfctp,inode,nnod,itypenext
      integer itype,layertype,nd_blnd,nd_cav,nd_glass, IER
      real Tglass,Text,fnnod,Tavg
      real q_gen_glass
      real b,air_cond,q_blind_airnd,q_glass_airnd
      real h_glass_blind,h_blind_room,h_glass_room
      real f_phi
      real DT,HGT,ARHY
      integer ICOR
      real w,phi_rad,pi,L,lthk
      real nstar,TA,TB,hc
      real diff
      real Rgap_shd_retracted
        
      CHARACTER outs*124

      IER=0
      pi=3.14159265
      nstar=0.67 !cavity width modification factor

      nc=NCONST(ICOMP)

      DO 1001 isur=1,nc
  
        icfctp=icfcfl(icomp,isur) !CFC type
        
        IF(icfctp.gt.0)then    !if cfc construction
        
          ne=NELTS(ICOMP,ISUR)
        
          do 1005 i = 1 , ne
         
            layertype = icfcltp(icomp,icfctp,i)
        
            if(layertype .eq. iVenBlind) then
              phi_rad = vb_phiNew(icomp,icfctp)*pi/180. !convert to radians
              w = vb_w(icomp,icfctp)
            elseif(layertype .eq. iPleatedDrape) then
              w = drp_w(icomp,icfctp)
            elseif(layertype .eq. iRollerBlind) then
C.............TODO: Check that this is thick enough for stability
              w = 1.     !Assumed effective layer thickness for program stability in convective models
            elseif(layertype .eq. iInsectScreen) then
C.............TODO: Check that this is thick enough for stability            
              w = bug_d(icomp,icfctp)      !Assumed effective layer thickness for program stability in convective models
            endif
        
 1005     continue

C.........LOOP THROUGH CFC LAYERS
          do 100 i=1,ne
        
C...........Setup temp variables for current layer and gas gap, next and previous layer,
C...........and previous gas gap for computing convection coefficient
            itype=icfcltp(icomp,icfctp,i)
      
            if(i.le.(ne-2))then
              itypenext=icfcltp(icomp,icfctp,i+2) ! next non gap layer in CFC
            else
              itypenext=0
            end if

            if(i.eq.1.and.itype.ge.iVenBlind)then ! outside blind present

              if(itypenext.eq.iGlazing)then      ! next non-gap layer is glass, OK

C...............OUTDOOR SHADE CONVECTION MODEL

C...............Cavity is fully vented, no convective exchange between blind and glass.
C...............Cavity resistance is set to a very high value - essentially an open
C...............circuit.
                cfcRgap(icomp,isur,i+1,2)=1000.

C...............Determine source term for convective exchange between external glass
C...............layer and outdoor air.
                nd_blnd=nndl(icomp,isur,1)
                nd_cav=nd_blnd+nndl(icomp,isur,2)
                nd_glass=nd_cav+1
                Tglass=tfc(icomp,isur,nd_glass)
                Text=(ATP(icomp,isur)+ATF(icomp,isur))/2.

                q_gen_glass=(hcof(icomp,isur)
     &                      *SNA(icomp,isur)*(Tglass-Text))*(-1.)
                q_glass_extS(icomp,isur)=q_gen_glass/SNA(icomp,isur) ! save for output

C...............Double the external convective heat transfer coefficient (calculated in MZCONV)
C...............to account for front and back surface of the outdoor blind layer.
                if(i_shd(icomp,icfctp).eq.0)then
                  ! if shade is retracted, decouple it from thermal zone
                  hcof(icomp,isur)=0.001
                else  
                  hcof(icomp,isur)=2.*hcof(icomp,isur)
                endif
              
C...............Assign source terms to external glass layer nodes
C...............
C...............First node of external glass layer
                inode=nd_glass

C...............Divide source term by surface area to get [W/m^2].
C...............'W' time-dependent coefficients in MZSETU require [W/m^2].
                q_gen_glass=q_gen_glass/SNA(icomp,isur)

                nnod=nndl(icomp,isur,3) ! no. of nodes of outermost glass layer
                fnnod=float(nnod)

                qcfc_cv(icomp,isur,inode,2)=
     &            qcfc_cv(icomp,isur,inode,2) + q_gen_glass/(fnnod*2.)

                do 35 j=2,nnod
                  inode=inode+1
                  qcfc_cv(icomp,isur,inode,2)=
     &              qcfc_cv(icomp,isur,inode,2) + q_gen_glass/fnnod
  35            continue

                inode=inode+1
                qcfc_cv(icomp,isur,inode,2)=
     &            qcfc_cv(icomp,isur,inode,2) + q_gen_glass/(fnnod*2.)

C...............Save future qcfc_cv term for next time step.
                nn=nndc(icomp,isur)
                do 36 j=1,nn
                  qcfc_cvF(icomp,isur,j)=qcfc_cv(icomp,isur,j,2)
  36            continue

C.............Error, next to outside blind layer is not glass
              else  
                IER=1
                goto 300 
              endif

C.............Outside blind layer is now processed, including first gap from outside


            elseif(i.lt.ne.and.itype.ne.iGasGap) then ! if layer type is not a gap and if not last layer


C.............Get the two cavity surface temperatures.
              TA=tfc(icomp,isur,i*2+1)       ! temperature of boundary node of current element and next gas gap [C]
              TB=tfc(icomp,isur,(i+1)*2+1)   ! temperature of boundary node of next layer [C]
              lthk=THRMLI(icomp,isur,i+1,4)  ! thickness of gas gap layer [m]

              IF(itype.eq.iGlazing)THEN ! layer is glass

                if(itypenext.ge.iVenBlind)then !next layer is a shade layer

                  if(i+2.eq.ne)goto 200 !skip if next layer is last layer
                
                  if(itypenext.eq.iVenBlind)then !edited 15/02/2011                    
                    L=((lthk*2.)-(nstar*(w/1000.)*cos(phi_rad)))/2.0 !modified slat wide
                  elseif(itypenext.gt.iVenBlind)then
                    L=((lthk*2.)-(w/1000.))/2 !assuming drapes are impermeable
                  endif

          !TODO: check if hc reset here is not needed
                !hc=0.

C.................If shade is retracted determine gap thickness between glass layers
                  if(i_shd(icomp,icfctp).eq.0)then
                    TB=tfc(icomp,isur,(i+3)*2+1)
                    L=THRMLI(icomp,isur,i+1,4)+THRMLI(icomp,isur,i+3,4)
                  endif
                
                  CALL ConvVertCav(L,TA,TB,rmlr_mass(icomp,icfctp,i+1),
     &            cond_A(icomp,icfctp,i+1),cond_B(icomp,icfctp,i+1),
     &            visc_A(icomp,icfctp,i+1),visc_B(icomp,icfctp,i+1),
     &            spht_A(icomp,icfctp,i+1),spht_B(icomp,icfctp,i+1),hc)
                
C.................If shade is retracted, only assign 1/2 of the convective
C.................coefficient to this air gap and the remainder to the 
C.................air gap on the other side of the (retracted) shade. 
                  if(i_shd(icomp,icfctp).eq.0)then
                    Rgap_shd_retracted=1./(hc*2.)
                    cfcRgap(icomp,isur,i+1,2)=Rgap_shd_retracted
                  else
                    cfcRgap(icomp,isur,i+1,2)=1./hc
                  endif
    
                elseif(itypenext.eq.iGlazing)then    !next layer is glass, use cavity correlation
                  L=lthk
                  !hc=0.

                  CALL ConvVertCav(L,TA,TB,rmlr_mass(icomp,icfctp,i+1),
     &            cond_A(icomp,icfctp,i+1),cond_B(icomp,icfctp,i+1),
     &            visc_A(icomp,icfctp,i+1),visc_B(icomp,icfctp,i+1),
     &            spht_A(icomp,icfctp,i+1),spht_B(icomp,icfctp,i+1),hc)

                  cfcRgap(icomp,isur,i+1,2)=1./hc

                else
                  !do nothing
                end if

              ELSEIF(itype.ge.iVenBlind)THEN !layer is a shade layer
              
                diff=abs(rmlr_mass(icomp,icfctp,i+1)-
     &               rmlr_mass(icomp,icfctp,i-1))
                if(diff.gt.0.1)then ! check that gas type is the same on both sides
                  IER=2
                  goto 300
                end if

                if(itypenext.ge.iVenBlind)then !next layer is a blind, not supported
                  IER=1
                  goto 300
                elseif(itypenext.eq.iGlazing)then

                  if(itype.eq.iVenBlind)then !added 15/02/2011
                    L=((lthk*2.)-(nstar*(w/1000.)*cos(phi_rad)))/2.0 !modified slat width
                  elseif(itype.gt.iVenBlind)then
                    L=((lthk*2.)-(w/1000.))/2 !assuming drapes are impermeable      
                  endif

                  !hc=0.
                  CALL ConvVertCav(L,TA,TB,rmlr_mass(icomp,icfctp,i+1),
     &            cond_A(icomp,icfctp,i+1),cond_B(icomp,icfctp,i+1),
     &            visc_A(icomp,icfctp,i+1),visc_B(icomp,icfctp,i+1),
     &            spht_A(icomp,icfctp,i+1),spht_B(icomp,icfctp,i+1),hc)
                
C.................If shade is retracted assign appropriate resistance calculated above
                  if(i_shd(icomp,icfctp).eq.0)then
                    cfcRgap(icomp,isur,i+1,2)=Rgap_shd_retracted
                  else
                    cfcRgap(icomp,isur,i+1,2)=1./hc
                  endif
                
                else
                 !do nothing
                end if

              ELSE
                !do nothing
              END IF

  200         continue

C...........Last layer
            elseif(i.eq.ne.and.itype.ne.iGasGap)then

              if(itype.eq.iGlazing)then ! layer is glass
                !do nothing, coefficient supplied by ESP-r
              elseif(itype.ge.iVenBlind)then ! layer is a shade layer

C.............Convection for an indoor venetian blind configuration consists of:
C.............    - convection between innermost glass and blind layer
C.............    - convection from glass to room air
C.............    - convection from blind to room air (front and back)
C.............
C.............METHOD (See Appendix A of Lomanowski, B.A. (2008) MASc thesis)
C............   1. For the back surface of the blind, default ESP-r convection coeff. is 
C............      applied. 
C............   2. For the glass to blind convection coeff., simple conduction in the 
C............      cavity is used and applied to the cfcRgap resistance.
C............   3. For the glass to room air and blind front surface to room air, 
C............      a source term is introduced to the glass and blind nodes and 
C............      zone air point node to account for convection from these surfaces 
C............      to the room air.

C...............2. Apply Simple Conduction between glass and blind
                Tavg=(tfc(icomp,isur,ne*2)+tfc(icomp,isur,(ne-2)*2))/2.  !T for properties calc
  
                if(itype.eq.iVenBlind)then    
C.................innermost cavity layer thickness corrected for effective slat width(m)  
                  b=thrmli(icomp,isur,ne-1,4)+ 
     &               ((w/1000.)-(nstar*(w/1000.)*cos(phi_rad)))
                elseif(itype.eq.iPleatedDrape)then  
C.................Pleated drape thickness corrected by adding 1/2 of the pleated drape width
                  b=thrmli(icomp,isur,ne-1,4)+((w/1000.)/2.0)   
                elseif(itype.eq.iRollerBlind.or.
     &                 itype.eq.iInsectScreen)then
C.................For flat shade layers thickness is not modified from air gap thickness
                  b=thrmli(icomp,isur,ne-1,4)                    
                endif     

                air_cond=0.02538+((Tavg-290.)/10.)*(0.02614-0.02538) !conductiviy of air(W/m.K)
                h_glass_blind=air_cond/b
                cfcRgap(icomp,isur,i-1,2)=1./h_glass_blind

C...............If shade is retracted decouple the shade and interior glass surface
C...............by setting resistance to very high value. 
                if(i_shd(icomp,icfctp).eq.0)
     &            cfcRgap(icomp,isur,i-1,2)=1000.

C...............3. Determine convection from glass and blind to room air.

C...............h_blind_room from back surface of the blind to room air is a function of innermost cavity 
C...............thickness b(m). When b is large h_blind_room=hcif (ESP-r assigned
C...............convective coefficient). When b is very small, h_blind_room goes to 
C...............zero. Inbetween h_blind_room varies exponentially. h_blind_room is 
C...............also mosidifed by f_phi, a function which 
C...............accounts for slat angles which pump air through the cavity to the 
C...............room air, thus increasing the convective heat transfer.

C...............If shade is retracted, set convective heat transfer coefficients
C...............from blind to room to very small value (large resistance)
C...............to remove influence of the blind on the zone air temperature.
                if(i_shd(icomp,icfctp).eq.0)then
                  h_blind_room=0.001
                  hcif(icomp,isur)=0.001
                else
                  if(itype.eq.iVenBlind)then
                    f_phi=1.0 + 0.2 * abs(sin(2.0 * phi_rad)) !slat angle penalty function          
                    h_blind_room=
     &                hcif(icomp,isur)*(1.0-exp(-4.6*b/0.1))*f_phi
                    hcif(icomp,isur)=hcif(icomp,isur)*f_phi
                  elseif(itype.gt.iVenBlind)then 
                    h_blind_room=
     &              hcif(icomp,isur)*(1.0-exp(-4.6*b/0.1))    
                  endif
                endif
 
C...............Determine source term for shade layer
                q_blind_airnd=h_blind_room
     &            *SNA(icomp,isur)*(tfa(icomp)-tfc(icomp,isur,ne*2))

C...............Save for output and XML
                h_blind_roomS(icomp,isur)=h_blind_room
                q_blind_airndS(icomp,isur)=q_blind_airnd/SNA(icomp,isur)
              
C...............For glass, have to adjust the indoor convecion coefficient 'hcif'
C...............for glass temperature. Use HTBUOY knowing the glass and air temps, 
C...............as well as ICOR, the correlation chosen after interrogating the 
C...............surface. 
C...............Set air & surface temperatures and temperature difference.
                DT=ABS(tfa(icomp)-tfc(icomp,isur,((ne-2)*2)+1))
C...............Calculate the HTC using one of the empirical correlations.
                ICOR = ICORI(ICOMP,ISUR)
                HGT = HEIGHT(ICOMP,ISUR)
                ARHY = APRAT(ICOMP,ISUR)
                CALL HTBUOY(HC,ICOR,ICOMP,ISUR,DT,HGT,ARHY)
                h_glass_room=HC*(1.0-exp(-4.6*b/0.1))
              
C...............If shade is retracted use the adjusted heat transfer coefficient for interior
C...............glass surface
                if(i_shd(icomp,icfctp).eq.0) h_glass_room=HC
              
C...............Determine source term for interior glass surface node.
                q_glass_airnd=h_glass_room
     &             *SNA(icomp,isur)*(tfa(icomp)
     &             -tfc(icomp,isur,(ne-2)*2))

C...............Save for output.
                h_glass_roomS(icomp,isur)=h_glass_room
                q_glass_airndS(icomp,isur)=
     &            q_glass_airnd/SNA(icomp,isur)
              
C...............Sum convection generation terms for air node
C...............(convective gains added to blind and glass from zone air need 
C...............to be subtracted from air node).
                q_airndConv_to_cfc(icomp,2)=
     &            q_airndConv_to_cfc(icomp,2)
     &            +(q_blind_airnd+q_glass_airnd)*(-1.)
   
C...............Assign heat injection terms to shade and glass layers.

C...............SHADE: first node of indoor blind layer.
                inode=nndc(icomp,isur)-nndl(icomp,isur,ne)

C...............Divide heat injection terms by surface area to get [W/m^2].
C...............'W' time-dependent coefficients in MZSETU require [W/m^2].
                q_blind_airnd=q_blind_airnd/SNA(icomp,isur)
              
                nnod=nndl(icomp,isur,ne)
                fnnod=float(nnod)

                qcfc_cv(icomp,isur,inode,2)=
     &            qcfc_cv(icomp,isur,inode,2)+q_blind_airnd/(fnnod*2.)

                do 110 j=2,nnod
                  inode=inode+1
                  qcfc_cv(icomp,isur,inode,2)=
     &              qcfc_cv(icomp,isur,inode,2)+q_blind_airnd/fnnod
  110           continue

                inode=inode+1
                qcfc_cv(icomp,isur,inode,2)=
     &            qcfc_cv(icomp,isur,inode,2)+q_blind_airnd/(fnnod*2.)

C...............GLASS
C...............inside surface node of innermost glass layer (ne-2)
                inode=nndc(icomp,isur)-nndl(icomp,isur,ne)-
     &                nndl(icomp,isur,ne-1)

C...............Divide heat injection terms by surface area to get [W/m^2].
C...............'W' time-dependent coefficients in MZSETU require [W/m^2].
                q_glass_airnd=q_glass_airnd/SNA(icomp,isur)

                qcfc_cv(icomp,isur,inode,2)=
     &            qcfc_cv(icomp,isur,inode,2) + q_glass_airnd

              end if 

            end if !layer type selection

C Save future terms.
            nn=nndc(icomp,isur)
            do 37 j=1,nn
              qcfc_cvF(icomp,isur,j)=qcfc_cv(icomp,isur,j,2)
  37        continue

  100     continue

        END IF

 1001 CONTINUE
 
!if number of nodes exceeds two per layer (ie. NNDL>2) then return error msg
  300 if(IER.eq.1)then
        write(outs,'(a)') 'ERROR in subroutine cfc_convection:'
        CALL EDISP(IUOUT,outs)
        write(outs,'(a)')'   Contiguous shading layers found.'
        CALL EDISP(IUOUT,outs)
        goto 1002
      elseif(IER.eq.2)then
        write(outs,'(a)') 'ERROR in subroutine cfc_convection:'
        CALL EDISP(IUOUT,outs)
        write(outs,'(a,a)')
     &    ' Detected different gas properties on either',
     &    ' side of between-panes shade layer.'
        CALL EDISP(IUOUT,outs)
        goto 1002
      end if

 1002 return

      end


C ********************************************************************
C                        --ConvVertCav--
C
C Calculates convection coefficient between two vertical surfaces in 
C an enclosed cavity based on cavity thickness (L), bounding surface
C temperatures(TA, TB) and fill gas properties (mlr_mass,condA,condB,
C viscA,viscB,spchtA,spchtB). Returns (hc).
C
C Reference:
C Shewen, E., Hollands, K.G.T., Raithby, G.D. (1996) 'Heat Transfer
C by Natural Convection Across a Vertical Cavity of Large Aspect
C Ratio', Journal of Heat Transfer, Vol. 118, pp.993-995.
C
C ********************************************************************
      subroutine ConvVertCav(L,TA,TB,mlr_mass,condA,condB,
     &                          viscA,viscB,spchtA,spchtB,hc)
      IMPLICIT NONE
C gas molar mass, A and B gas property coefficients
      real mlr_mass,condA,condB,viscA,viscB,spchtA,spchtB

      real L,TA,TB,hc
      real g,R,P,Tm,ro,cp,k,mu,dT,Ra,Nu
      real small
      logical close_dT, close_L

C Small real number to check for values close to 0.
      small = 1.0e-6

C if gap is evacuated, set hc to 0.1
C This is a crude idealized treatment.
      if(mlr_mass.lt.0.0001)then
        hc=0.1
        goto 1000
      endif

      g=9.806402                       !gravity [m/s2]
      P=101325.                        !abs pressure [Pa]
      R=8.314472                       !universal gas constant [J/mol.K]

C Convert temperature to Kelvin
      TA=TA+273.15
      TB=TB+273.15

      if(TA.gt.0.0.and.TB.gt.0.0)then
        Tm=0.5*(TA+TB)                   !mean temp

C Calculate gas properties based on linear Temperature fit
        k=condA+condB*Tm                 !conductivity [W/m.K]
        mu=viscA+viscB*Tm                !viscosity[N.sec/m^2]
        cp=spchtA+spchtB*Tm              !specific heat [J/kg.K]

        ro=(P*mlr_mass)/(R*Tm*1000.)     !density [kg/m^3]

        dT=ABS(TA-TB)                    !temp. diff.

C Check if dT = 0 or L = 0, which will cause divide by zero exceptions.
        call eclose(dT,0.0,small,close_dT)     
        call eclose(L,0.0,small,close_L)
  
        if(close_dT .or. close_L) then

C.........Set convection coefficient to a small number
          hc = 0.001

        else

C.........Calculate Rayleigh number
          Ra = ro*ro*cp*dT*L*L*L*g/(mu*k*Tm)

C.........Calculate Nusselt number (Shewen, Hollands and Raithby 1996)
          Nu = SQRT(1.+((0.0665*Ra**(0.33333))/
     &           (1.+(9000./Ra)**1.4))**(2))

C.........Calculate convective heat transfer coefficient.
          hc = Nu*k/L     !(W/m^2.K)

        end if

      end if

 1000 return
      end


C ********************************************************************
C                    --CFC_time_dependent_Rgap--
C 
C Modifies difference equation coefficients VCF(,,1),VCF(,,2),
C VCP(,,1), VCP(,,2) with time dependent gas gap resistances 
C cfcRgap(,,,1) and cfcRgap(,,,2). 
C
C ********************************************************************
      subroutine CFC_time_dependent_Rgap(icomp)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      integer nconst,nelts,ngaps,npgap
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      integer nnds,nndz,nndc,nndl
      COMMON/VTHP15/VCP(MS,MN,2),QCP(MS,MN)
      real vcp,qcp
      COMMON/VTHP16/VCF(MS,MN,2),QCF(MS,MN)
      real vcf,qcf

      integer icomp
C local variables
      integer i,icfctp,ngp,m,il,in,iflg,id

C Consider each multilayered construction within current
C zone.
      DO 10 I=1,NCONST(ICOMP)

C CFC flag
        icfctp=icfcfl(icomp,i)

        IF(icfctp.gt.0)then   ! detect CFC

C For discretized constructions.
          NGP=NGAPS(ICOMP,I) ! number of gaps in surface I
          M=1  !gap index
          IL=1 !layer index
          IN=1 !node index

C Set the air gap flag to off.
          IFLG=0

C Continue with other nodes (homogeneous) in the first layer.
          DO 20 ID=1,NNDL(ICOMP,I,IL)-1
            IN=IN+1
 20       CONTINUE

C Continue with other layers in the construction.
          DO 30 IL=2,NELTS(ICOMP,I)

C The current layer is an air gap.
            IF((M.LE.NGP.AND.NPGAP(ICOMP,I,M).EQ.IL)
     &                .OR. 
     &         icfcltp(ICOMP,icfctp,IL).EQ.0)THEN

C If the previous layer was a solid.
              IF(IFLG.EQ.0)THEN
                IN=IN+1
C Modify present and future difference equation coefficients
C with time dependent gas gap resistances
                VCP(I,IN,1)=VCP(I,IN,1)
                VCP(I,IN,2)=VCP(I,IN,2)/cfcRgap(icomp,i,il,1)
                VCF(I,IN,1)=VCF(I,IN,1)
                VCF(I,IN,2)=VCF(I,IN,2)/cfcRgap(icomp,i,il,2)
              ENDIF
              IFLG=1

C Continue with other nodes in the layer.
              DO 41 ID=1,NNDL(ICOMP,I,IL)-1
                IN=IN+1
C Modify present and future difference equation coefficients
C with time dependent gas gap resistances
                VCP(I,IN,1)=VCP(I,IN,1)/cfcRgap(icomp,i,il,1)
                VCP(I,IN,2)=VCP(I,IN,2)/cfcRgap(icomp,i,il,1)
                VCF(I,IN,1)=VCF(I,IN,1)/cfcRgap(icomp,i,il,2)
                VCF(I,IN,2)=VCF(I,IN,2)/cfcRgap(icomp,i,il,2)

   41         CONTINUE
              M=M+1

C The current layer is solid.
            ELSE

C If the previous layer was solid.
              IF(IFLG.EQ.0)THEN
                IN=IN+1

C If the previous layer was an air gap.
              ELSE
                IN=IN+1
C Modify present and future difference equation coefficients
C with time dependent gas gap resistances.
                VCP(I,IN,1)=VCP(I,IN,1)/cfcRgap(icomp,i,il-1,1)
                VCP(I,IN,2)=VCP(I,IN,2)
                VCF(I,IN,1)=VCF(I,IN,1)/cfcRgap(icomp,i,il-1,2)
                VCF(I,IN,2)=VCF(I,IN,2)

              ENDIF
              IFLG=0

C Continue with other nodes (homogeneous) in the layer.
              DO 51 ID=1,NNDL(ICOMP,I,IL)-1
                IN=IN+1
   51         CONTINUE

            ENDIF

   30     CONTINUE

        END IF
   10 CONTINUE

      return
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
!  SUBROUTINE: PD_LW (Pleated drape layer long wave effective properties)
!
!  Calculates the effective longwave properties of a drapery layer
!  Returns the front-side emittance and transmittance of the drapery layer.

!  If you want the back-side reflectance call the subroutine a second time with the same
!  input data - but interchange forward-facing and backward-facing properties
!****************************************************************************
      SUBROUTINE PD_LW( S, W, OPENNESS_FABRIC, EPSLWF0_FABRIC, 
     & EPSLWB0_FABRIC, TAULW0_FABRIC, EPSLWF_PD, TAULW_PD) 

      IMPLICIT NONE
      REAL S                        ! pleat spacing (> 0)
      REAL W                        ! pleat depth (>=0, same units as S)
      REAL OPENNESS_FABRIC      ! fabric openness, 0-1 (=tausbb at normal incidence)
      REAL EPSLWF0_FABRIC            ! fabric LW front emittance at 0 openness
                                                !    typical (default) = 0.92
      REAL EPSLWB0_FABRIC            ! fabric LW back emittance at 0 openness
                                                !    typical (default) = 0.92
      REAL TAULW0_FABRIC            ! fabric LW transmittance at 0 openness
                                                !    nearly always 0
      REAL EPSLWF_PD                  ! returned: drape front effective LW emittance
      REAL TAULW_PD                  ! returned: drape effective LW transmittance

                  
      REAL RHOLWF_FABRIC, RHOLWB_FABRIC, TAULW_FABRIC
      REAL EPSLWF_FABRIC, EPSLWB_FABRIC, TAULX, RHOLWF_PD

      CALL OPENNESS_LW( OPENNESS_FABRIC, EPSLWF0_FABRIC, 
     & TAULW0_FABRIC, EPSLWF_FABRIC, TAULW_FABRIC)
      
      CALL OPENNESS_LW( OPENNESS_FABRIC, EPSLWB0_FABRIC, 
     & TAULW0_FABRIC, EPSLWB_FABRIC, TAULX)

      !RHOLWF_FABRIC = P01( 1. - EPSLWF_FABRIC - TAULW_FABRIC, "PD_LW RhoLWF")
      !RHOLWB_FABRIC = P01( 1. - EPSLWB_FABRIC - TAULW_FABRIC, "PD_LW RhoLWB")

      RHOLWF_FABRIC = 1. - EPSLWF_FABRIC - TAULW_FABRIC
      RHOLWB_FABRIC = 1. - EPSLWB_FABRIC - TAULW_FABRIC
      
      IF (RHOLWF_FABRIC < 0.) RHOLWF_FABRIC = 0.
      IF (RHOLWB_FABRIC < 0.) RHOLWB_FABRIC = 0.
      
      CALL PD_DIFF( S, W, RHOLWF_FABRIC, RHOLWB_FABRIC, 
     &      TAULW_FABRIC, RHOLWF_PD, TAULW_PD)

      !EPSLWF_PD = P01( 1. - TAULW_PD - RHOLWF_PD, "PD_LW EpsLWF")
      EPSLWF_PD = 1. - TAULW_PD - RHOLWF_PD 
      
      IF (EPSLWF_PD < 0.) EPSLWF_PD = 0.
      
      END SUBROUTINE PD_LW

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
!   F12,    F13,  F15,  F16,   F21,	F25, F26, F31, F35,	F36, F41, F42, F43,	F45, F46 = SHAPE FACTORS 
!   F51, F52, F53, F54,  F61, F62, F63, F78, F79, F710,	F711, F712,	F810, F811,	F812 = SHAPE FACTORS
!   F910, F911,	F912, F108,	F109, F1011, F1012,	F118, F119,	F1110, F128, F129, F1210 = SHAPE FACTORS 
!   Z1_BD, Z2_BD, Z6_BD, Z3_BD,	Z8_BD, Z11_BD, Z12_BD = DIFFUSE SOURCE TERMS DUE TO INCIDENT BEAM RADIATION ON SURFACES 1, 2, 6, 3, 11 AND 12.
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
      
!******************************************************************************
! common models (shared by more than one shade type)
!******************************************************************************
      SUBROUTINE OPENNESS_LW(OPENNESS, EPSLW0, TAULW0, EPSLW, TAULW) 
      ! long wave properties for shade types characterized by openness
      !   insect screen, roller blind, drape fabric
      
      IMPLICIT NONE
      
      REAL OPENNESS            ! shade openness (=tausbb at normal incidence)
      REAL EPSLW0                  ! apparent LW emittance of shade at 0 openness
                                          !   (= wire or thread emittance)
                                          !   typical (default) values
                                          !      dark insect screen = .93
                                          !      metalic insect screen = .32
                                          !      roller blinds = .91
                                          !      drape fabric = .87
      REAL TAULW0                  ! apparent LW transmittance of shade at 0 openness
                                          !   typical (default) values
                                          !      dark insect screen = .02
                                          !      metalic insect screen = .19
                                          !      roller blinds = .05
                                          !      drape fabric = .05
      REAL EPSLW                  ! returned: effective LW emittance of shade
      REAL TAULW                  ! returned: effective LW transmittance of shade

      EPSLW = EPSLW0*(1.-OPENNESS)
      TAULW = TAULW0*(1.-OPENNESS) + OPENNESS
                                                 
      END SUBROUTINE OPENNESS_LW
!------------------------------------------------------------------------------
!      REAL FUNCTION P01(P, WHAT)      ! constrain property to range 0 - 1
!      IMPLICIT NONE
      
!      REAL P                  ! property
!      CHARACTER*(*) WHAT      ! identifier for err msg

!#if defined( _DEBUG)
!      CHARACTER*100 Modifier
!      IF (P < -.05 .OR. P > 1.05)      THEN
!            Modifier = " "
!            IF (P < -.2 .OR. P > 1.2) Modifier = " very"
!            WRITE (*, "(A,A,' out of range ',F0.3)") TRIM( WHAT), 
!     &            TRIM( Modifier), P
!      END IF
!#endif

!      P01 = MAX( 0., MIN( 1., P))
      
!      END FUNCTION P01
!------------------------------------------------------------------------------

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
      
C ********************************************************************
C                             --SETCoef--
C
C Sets coefficients for TDMA solver
C
C INPUT:
C tb = layer back transmittance
C tf = layer front transmittance
C rb = layer back reflectance
C rf = layer front reflectance
C Ir = solar radiation source
C n  = number of layers + 2 for inside/outside
C
C OUTPUT:
C aw = west side coefficient
C ap = pole coefficient
C ae = east side coefficient
C bp = pole source term
C
C Details in:
C Wright, J.L., Kotey, N.A. (2006) 'Solar Absorption by Each Element 
C in a Glazing/Shading Layer Array',ASHRAE Transactions, Vol. 112, 
C Pt. 2. pp. 3-12.
C ********************************************************************

      SUBROUTINE SETCoef(tb,tf,rb,rf,Ir,n,aw,ap,ae,bp)
      IMPLICIT NONE
#include "building.h"

      INTEGER n,i,xx
      real aw,ap,ae,bp,tb,tf,rb,rf
      DIMENSION aw(me*2+2),ap(me*2+2),ae(me*2+2),bp(me*2+2)
      DIMENSION tb(me),tf(me),rb(me),rf(me)
      REAL Ir

      aw(1)=0.
      ap(1)=0.00001
      ae(1)=1.
      bp(1)=0.
      aw(2)=1.
      IF (rb(n-2).LT.0.0001) THEN
        ap(2)=0.00001
      ELSE
        ap(2)=rb(n-2)
      ENDIF
      ae(2)=-tf(n-2)
      bp(2)=0.
      DO i=2,n-2,1
        xx=2*i-1
        aw(xx)=-tb(n-i)
        IF (rf(n-i).LT.0.0001) THEN
          ap(xx)=0.00001
        ELSE
          ap(xx)=rf(n-i)
        ENDIF
        ae(xx)=1.
        bp(xx)=0.
        xx=2*i
        aw(xx)=1.
        IF (rb(n-i-1).LT.0.0001) THEN
          ap(xx)=0.00001
        ELSE
          ap(xx)=rb(n-i-1)
        ENDIF
        ae(xx)=-tf(n-i-1)
        bp(xx)=0.
      ENDDO
      xx=2*(n-2)
      ae(xx)=0.
      bp(xx)=-(tf(1))*Ir
      RETURN
      END


C ********************************************************************
C                             --TDMA Solver --
C
C Tri-Diagonal Matrix Algorithm (TDMA) solver
C
C INPUT:
C aw = west side coefficient
C ap = pole coefficient
C ae = east side coefficient
C bp = pole source term
C n  = number of layers + 2 for inside/outside
C
C OUTPUT:
C solx = solution vector
C
C Details in:
C Wright, J.L., Kotey, N.A. (2006) 'Solar Absorption by Each Element 
C in a Glazing/Shading Layer Array',ASHRAE Transactions, Vol. 112, 
C Pt. 2. pp. 3-12.
C ********************************************************************
      SUBROUTINE TDMAsol(ae,ap,aw,bp,n,solx)
      IMPLICIT NONE
#include "building.h"
      INTEGER n,m,i
      REAL alpha,beta,solx,aw,ap,ae,bp
      DIMENSION alpha(me*2+2),beta(me*2+2),solx(me*2+2)
      DIMENSION aw(me*2+2),ap(me*2+2),ae(me*2+2),bp(me*2+2)

      m=2*n-4
      DO i=1,m,1
        alpha(i)=0.
        beta(i)=0.
      ENDDO
      alpha(1)=ae(1)/ap(1)
      beta(1)=bp(1)/ap(1)
      DO i=2,m,1
        alpha(i)=ae(i)/(ap(i)-alpha(i-1)*aw(i))
        beta(i)=(aw(i)*beta(i-1)+bp(i))/(ap(i)-alpha(i-1)*aw(i))
      ENDDO
      solx(m)=beta(m)
      DO i=m-1,1,-1
        solx(i)=alpha(i)*solx(i+1)+beta(i)
      ENDDO

      RETURN
      END 


C ********************************************************************
C                             --profile_angle --
C
C Calculates horizontal and vertical profile angle based on sun's
C position (sazi = solar azimuth, salt = solar altitude) relative 
C to CFC surface position (pazi = surface azimuth).
C
C Returns: proang [degrees]
C
C Reference: Chapter 4, Section 4.1.1
C Lomanowski, B.A. (2008) 'Implementation of Window Shading Models
C into Dynamic Whole-Building Simulation', MASc Thesis, University
C of Waterloo.
C
C ********************************************************************
      subroutine profile_angle(icomp,isurf,icfctp,
     &      omega_v,omega_h)
      USE h3kmodule
      IMPLICIT NONE

#include "building.h"
#include "CFC_common.h"

      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      real sazi,salt
      integer isunup
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      real pazi,pelv

      integer icomp,isurf,icfctp
      real pi,r, omega_v, omega_h

C.....DECLARATIONS for H3Kreporting object
      CHARACTER*2   cZone_Chars, cSurf_Chars

      pi = 4.0 * ATAN(1.0)
      r=pi/180.

C.....Vertical profile angle
      omega_v = ATAN(TAN(SALT*r)/COS(SAZI*r-PAZI(icomp,isurf)*r))/r
C.....Horizontal profile angle
      omega_h = SAZI-PAZI(ICOMP,ISURF)
      
C.....Pad zone index to 'XX'
      if ( ICOMP .gt. 9 ) then
        write (cZone_Chars,'(I2)') ICOMP
      else
        write (cZone_Chars,'(A,I1)') '0', ICOMP
      endif

C.....Pad surface index to 'XX'
      if ( ISURF .gt. 9 ) then
        write (cSurf_Chars,'(I2)') ISURF
      else
        write (cSurf_Chars,'(A,I1)') '0', ISURF
      endif

c      call AddToReport(
c     &         rvCFCazimuth%identifier,
c     &         SAZI-PAZI(ICOMP,ISURF),
c     &         cZone_Chars,
c     &         cSurf_Chars)

c      call AddToReport(
c     &         rvCFCelevation%identifier,
c     &         SALT-PELV(ICOMP,ISURF),
c     &         cZone_Chars,
c     &         cSurf_Chars)

c      call AddToReport(
c     &         rvCFCvertprofileangle%identifier,
c     &         omega_v,
c     &         cZone_Chars,
c     &         cSurf_Chars)

      return
      end


C ********************************************************************
C                         --set_HR_to_zero --
C
C If an indoor venetian blind is present in a CFC, calculation for 
C longwave exchange of CFC layers with interior surfaces is handled
C in subroutine qlwCFC. To avoid double accounting, this subroutine
C sets those radiant heat transfer coefficients in communication with
C the CFC surface calculated in MZCFGG to zero.
C
C If surface I is CFC, and surface J is a non CFC interior surface
C set HRF(I,J)=HRF(J,I)=0. Then re-sum ZHRF(I).
C
C
C ********************************************************************
      subroutine set_HR_to_zero(icomp)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      integer nconst,nelts,ngaps,npgap
      COMMON/COE31/HRP(MS,MS),ZHRP(MS),HRF(MS,MS),ZHRF(MS)
      real hrp,zhrp,hrf,zhrf
      COMMON/COE31S/HRS(MCOM,MS,MS),ZHRS(MCOM,MS)
      real hrs,zhrs

      integer nc,ne,i,j,icfctp,icomp
      real sum

      nc=nconst(icomp)
C Loop through zone surfaces.
      do 100 i=1,nc
        ne=nelts(icomp,i)
        icfctp=icfcfl(icomp,i) ! cfc type index

C If cfc is detected AND indoor slat blind is present.
        if(icfctp.gt.0)then
          if(icfcltp(icomp,icfctp,ne).ge.2)then
            do 110 j=1,nc
              HRF(i,j)=0.0
              HRF(j,i)=0.0
 110        continue
          endif
        end if
 100  continue

C Since individual HRF coefficients are now altered, need
C to sum HRFs to determine new ZHRF.

C Summate for each surface.
      DO 140 I=1,NC
        SUM=0.
        DO 150 J=1,NC
          SUM=SUM+HRF(J,I)
  150   CONTINUE

C For I receiving.
        ZHRF(I)=SUM
  140 CONTINUE

C Save future time-row values for use as present
C values at next time-step.
      DO 160 I=1,NC
        ZHRS(ICOMP,I)=ZHRF(I)
        DO 170 J=1,NC
          HRS(ICOMP,I,J)=HRF(I,J)
  170   CONTINUE
  160 CONTINUE

      return
      end

C ********************************************************************
C                         --set_QELW_to_zero --
C
C If an outdoor venetian blind is present in a CFC, calculation for 
C longwave exchange of CFC layers with external surroundings is handled
C in subroutine qlwCFC. To avoid double accounting, this subroutine
C sets the external source term calculated by MZELWE to zero is surface
C is CFC. 
C
C If surface I is CFC ,set QELWF(I)=0.
C
C ********************************************************************
      subroutine set_QELW_to_zero(icomp)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      integer nconst,nelts,ngaps,npgap
      COMMON/COE33/QELWP(MS),QELWF(MS)
      real qelwp,qelwf
      COMMON/COE33Z/QELWS(MCOM,MS)
      real qelws

      integer nc,icfctp,i,icomp

      nc=nconst(icomp)
C loop through zone surfaces
      do 100 i=1,nc
        icfctp=icfcfl(icomp,i)  !CFC type index

C if cfc is detected AND indoor slat blind is present
        if(icfctp.gt.0)then

          if(icfcltp(icomp,icfctp,1).ge.2) QELWF(I)=0.

        end if
 100  continue

C Save future time-row values for use as present
C values at next time-step.
      DO 160 I=1,NC
        QELWS(ICOMP,I)=QELWF(I)
  160 CONTINUE

      return
      end

C ********************************************************************
C                         --CFC_output--
C
C Optional CFC output generation invoked in MZNUMA.
C 
C Generates time-step output to three scratch files:
C
C fort.97 : Contains solar-optical properties output for each layer 
C           in CFC
C fort.98 : Contains temperature, longwave and convective source term
C           output
C fort.99 : Contains solar incident, transmitted and absorbed flux
C           output
C
C ********************************************************************
      subroutine CFCoutput(icomp)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      integer nconst,nelts,ngaps,npgap
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      integer nnds,nndz,nndc,nndl
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      integer ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      real sazi,salt
      integer isunup
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      real qfp,qff,tp,tf,qdp,qdf,vp,vf,dp,df,hp,hf
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      real tfa,qfa
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      real pazi,pelv
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      real hcip,hcif,hcop,hcof
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      real TFC,QFC
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      real TFS,QFS
      COMMON/COE32J/QTMCA(MS,MN,2)
      real qtmca

      COMMON/PVANG/fInc_angle(MS)
      real fInc_angle

      common/CFCsolarS/RadIncDirS(mcom,ms),RadIncDifS(mcom,ms),
     &      transBB_S(mcom,ms),transD_S(mcom,ms),refl_S(mcom,ms),
     &      issur_self_shd(mcom,ms),proang_s(mcom,mcfc,ms),
     &      qtmca_ext(mcom,ms,mn),omega_v_s(mcom,mcfc,ms),
     &      omega_h_s(mcom,mcfc,ms)
      real RadIncDirS,RadIncDifS,transBB_S,transD_S,refl_S
      real proang_s,qtmca_ext,omega_v,omega_h,omega_v_s,omega_h_s
      integer issur_self_shd

      integer i,j,k,l,nn,nc,ne,icfctp,icomp
        integer ivblayr,idrplayr,irldlayr,ibuglayr

C**************** Ambient, Solar and CFC descriptors *****************
      write(97,*)
     &'TIMESTEP | ICOMP | ISUNUP |  SAZI  |  SALT  |  Tamb  |  Tzone'
      write(97,9990)NSINC,ICOMP,ISUNUP,SAZI,SALT,TP,TFA(ICOMP)

 9990     format(I6,I10,I7,3x,4F9.3)

      nc=nconst(icomp)

      do 10 i=1,nc
        if (icfcfl(icomp,i).gt.0)then
          icfctp=icfcfl(icomp,i)
          ne=nelts(icomp,i)

          do 11 j=1,ne
            
            if(icfcltp(icomp,icfctp,j).eq.iVenBlind) then
               
               ivblayr=j
               
               write(97,9991)
     &           ' CFC SURFACE |  PAZI  |  ANGI  |  PROANG  | HCIF |',
     &           'HCOF |VBLAYR|   W   |   S   |   PHI   | ORIENT |'
               write(97,9992)i,pazi(icomp,i),fInc_angle(i),
     &            proang_s(icomp,icfctp,i),
     &           hcif(icomp,i),hcof(icomp,i),ivblayr,vb_w(icomp,icfctp),
     &           vb_s(icomp,icfctp),vb_phiNew(icomp,icfctp),
     &         vb_VorH(icomp,icfctp)
     
            elseif(icfcltp(icomp,icfctp,j).eq.iPleatedDrape) then
               
               idrplayr=j
                     
               write(97,9991)
     &         '    CFC SURFACE |  PAZI  | OMEGA_V | OMEGA_H |', 
     &         'HCIF | HCOF | DRPLYR |    W    |    S    |'
               write(97,9992)i,pazi(icomp,i),
     &         omega_v_s(icomp,icfctp,i),omega_h_s(icomp,icfctp,i),
     &         hcif(icomp,i),hcof(icomp,i),idrplayr,
     &         drp_w(icomp,icfctp), drp_s(icomp,icfctp)
 
             elseif(icfcltp(icomp,icfctp,j).eq.iRollerBlind) then
               
               irldlayr=j
                     
               write(97,8880)
     &         '   CFC SURFACE |  PAZI  | ANGI |',
     &         'HCIF  |  HCOF  |RLDLYR|'
     
               write(97,8881)i,pazi(icomp,i),fInc_angle(i),
     &          hcif(icomp,i),hcof(icomp,i),irldlayr,irldlayr
       
             elseif(icfcltp(icomp,icfctp,j).eq.iInsectScreen) then
               
               ibuglayr=j
                     
               write(97,8880)
     &         '   CFC SURFACE |  PAZI  | ANGI |',
     &         'HCIF  |  HCOF  |BUGLYR|'
     
               write(97,8881)i,pazi(icomp,i),fInc_angle(i),
     &          hcif(icomp,i),hcof(icomp,i),ibuglayr
       
            endif
               
 9991     format(A50,A48)
 9992     format(6x,I10,3x,5F9.3,I7,3f9.3,A8)
 
 8880     format(A57)
 8881     format(3x,I11,4F8.3,I7) 
 
 11         continue
 
          write(97,9993)
     &    '     layer |SolRFbb|SolRBbb|SolTFbb|SolTBbb',
     &    '|SolRFbd|SolRBbd|SolTFbd|SolTBbd',
     &    '|SolRFdd|SolRBdd|SolTFdd|SolTBdd',
     &    '| Emisf | Emisb | taul  |'

 9993     format(4x,A48,A32,A31,A26)
            
          do 12 k=1,ne

          write(97,9994)k,SolRFbb(icomp,i,icfctp,k),
     &         SolRBbb(icomp,i,icfctp,k),SolTFbb(icomp,i,icfctp,k),
     &         SolTBbb(icomp,i,icfctp,k),SolRFbd(icomp,i,icfctp,k),
     &         SolRBbd(icomp,i,icfctp,k),SolTFbd(icomp,i,icfctp,k),
     &         SolTBbd(icomp,i,icfctp,k),SolRFdd(icomp,i,icfctp,k),
     &         SolRBdd(icomp,i,icfctp,k),SolTFdd(icomp,i,icfctp,k),
     &         SolTBdd(icomp,i,icfctp,k),
     &         rlwEF(icomp,icfctp,k),rlwEB(icomp,icfctp,k),
     &         rlwT(icomp,icfctp,k)
 9994     format(6x, I12,1x,15F8.3)
       
 12       continue
 
          endif
          
 10   continue
 
C********************************************************************

C**************** CFC Convection/LW exchange*************************
      write(98,*)
     &'TIMESTEP | ICOMP | ISUNUP |  SAZI  |  SALT  |  Tamb  |  Tzone'
      write(98,9995)NSINC,ICOMP,ISUNUP,SAZI,SALT,TP,TFA(ICOMP)

 9995 format(I6,I10,I7,3x,4F9.3)

      nc=nconst(icomp)

      do 13 i=1,nc

      if (icfcfl(icomp,i).gt.0)then
      icfctp=icfcfl(icomp,i)
      ne=nelts(icomp,i)

      do 14 j=1,ne
      
      if(icfcltp(icomp,icfctp,j).eq.iVenBlind) then
        
        ivblayr=j        
        
        write(98,9996)
     &'   CFC SURFACE |  PAZI  |  ANGI  |  PROANG  | HCIF |',
     &'HCOF |VBLAYR|   W   |   S   |   PHI   | ORIENT |'

        write(98,9997)i,pazi(icomp,i),fInc_angle(i),
     & proang_s(icomp,icfctp,i),
     &hcif(icomp,i),hcof(icomp,i),ivblayr,vb_w(icomp,icfctp),
     &vb_s(icomp,icfctp),vb_phiNew(icomp,icfctp),vb_VorH(icomp,icfctp)
       
      elseif(icfcltp(icomp,icfctp,j).eq.iPleatedDrape) then
        
        idrplayr = j      
        
        write(98,9996)
     &'     CFC SURFACE |  PAZI  | OMEGA_V | OMEGA_H |',
     &'HCIF  |  HCOF  | DRPLYR |    W    |    S    |'
       
        write(98,9997)i,pazi(icomp,i),
     & omega_v_s(icomp,icfctp,i),omega_h_s(icomp,icfctp,i),
     & hcif(icomp,i),hcof(icomp,i),idrplayr,
     & drp_w(icomp,icfctp), drp_s(icomp,icfctp)
     
      elseif(icfcltp(icomp,icfctp,j).eq.iRollerBlind) then
        
        irldlayr = j      
        
        write(98,8882)
     &  '  CFC SURFACE |  ANGI  |  HCIF  |  HCOF  |  N/A  |RLDLYR|'

        write(98,8883)i,fInc_angle(i),
     &  hcif(icomp,i),hcof(icomp,i),irldlayr,irldlayr,irldlayr

      elseif(icfcltp(icomp,icfctp,j).eq.iInsectScreen) then
        
        ibuglayr = j      
        
        write(98,8882)
     &  '  CFC SURFACE |  ANGI  |  HCIF  |  HCOF  |  N/A  |BUGLYR|'
       
        write(98,8883)i,fInc_angle(i),
     & hcif(icomp,i),hcof(icomp,i),ibuglayr,ibuglayr
       
        else
        
        !do nothing
        
      endif      

 14   continue        
        
 9996     format(A50,A48)
 9997     format(6x,I10,3x,5F9.3,I7,3f9.3,A8)
 8882     format(A57)
 8883     format(3x,I10,3F9.3,I6,I7) 
 
      write(98,9998)
     &'     layer |tfc(o) |tfc(m) |tfc(i) |CFCRgap',
     &'|Q_lw(o)|Q_lw(m)|Q_lw(i)|',
     &'|QextVB_g_env| h_vb_room  |h_glass_room|',
     &'|QintVB_b_rm | QintVB_g_rm|',
     &'|Q_cv(o)|Q_cv(m)|Q_cv(i)|'

 9998     format(4x,A48,A25,A40,A28,A26)

          do 15 l=1,ne

            if((icfcltp(icomp,icfctp,l).lt.2.and.l.lt.ne).or.
     &         (icfcltp(icomp,icfctp,l).eq.2.and.l.gt.1.and.
     &          l.lt.ne))then

              write(98,9999)l,tfc(icomp,i,(l*2)-1),tfc(icomp,i,l*2),
     &          tfc(icomp,i,(l*2)+1),
     &          cfcRgap(icomp,i,l,2),qcfc_lw(icomp,i,(l*2)-1,2),
     &          qcfc_lw(icomp,i,l*2,2),qcfc_lw(icomp,i,(l*2)+1,2),0.,0.,
     &          0.,0.,0.,qcfc_cv(icomp,i,(l*2)-1,2),
     &          qcfc_cv(icomp,i,l*2,2),qcfc_cv(icomp,i,(l*2)+1,2)

      !outdoor blind
            elseif(icfcltp(icomp,icfctp,l).eq.2.and.l.eq.1)then

              write(98,9999)l,tfc(icomp,i,(l*2)-1),tfc(icomp,i,l*2),
     &          tfc(icomp,i,(l*2)+1),
     &          cfcRgap(icomp,i,l,2),qcfc_lw(icomp,i,(l*2)-1,2),
     &          qcfc_lw(icomp,i,l*2,2),qcfc_lw(icomp,i,(l*2)+1,2),
     &          q_glass_extS(icomp,i),0.,
     &          0.,0.,0.,qcfc_cv(icomp,i,(l*2)-1,2),
     &          qcfc_cv(icomp,i,l*2,2),qcfc_cv(icomp,i,(l*2)+1,2)

      !indoor blind
            elseif(l.eq.ne)then

              if(icfcltp(icomp,icfctp,l).ge.iVenBlind)then
                write(98,9999)l,tfc(icomp,i,(l*2)-1),tfc(icomp,i,l*2),
     &            tfs(icomp,i),
     &            cfcRgap(icomp,i,l,2),qcfc_lw(icomp,i,(l*2)-1,2),
     &            qcfc_lw(icomp,i,l*2,2),qcfc_lw(icomp,i,(l*2)+1,2),0.,
     &            h_blind_roomS(icomp,i),h_glass_roomS(icomp,i),
     &            q_blind_airndS(icomp,i),q_glass_airndS(icomp,i),
     &            qcfc_cv(icomp,i,(l*2)-1,2),qcfc_cv(icomp,i,l*2,2),
     &            qcfc_cv(icomp,i,(l*2)+1,2)
              else
                write(98,9999)l,tfc(icomp,i,(l*2)-1),tfc(icomp,i,l*2),
     &            tfs(icomp,i),
     &            cfcRgap(icomp,i,l,2),qcfc_lw(icomp,i,(l*2)-1,2),
     &            qcfc_lw(icomp,i,l*2,2),qcfc_lw(icomp,i,
     &           (l*2)+1,2),0.,0.,0.,0.,0.,qcfc_cv(icomp,i,(l*2)-1,2),
     &            qcfc_cv(icomp,i,l*2,2),qcfc_cv(icomp,i,(l*2)+1,2)
              end if

            else
              !do nothing
            end if
 9999       format(6x,I12,1x,7F8.3,6F13.3,2F8.3)

 15       continue
        end if
 13   continue
C********************************************************************

C************************ Solar Processing **************************
      IF(ISUNUP.eq.1)then
        write(99,*)
     &'TIMESTEP | ICOMP | ISUNUP |  SAZI  |  SALT  |  Tamb  |  Tzone'
        write(99,1000)NSINC,ICOMP,ISUNUP,SAZI,SALT,TP,TFA(ICOMP)

 1000   format(I6,I10,I7,3x,4F9.3)

        nc=nconst(icomp)

        do 16 i=1,nc

          if (icfcfl(icomp,i).gt.0)then
            icfctp=icfcfl(icomp,i)
            nn=nndc(icomp,i)
            ne=nelts(icomp,i)

          do 17 j=1,ne
        
          if(icfcltp(icomp,icfctp,j).eq.iVenBlind) then
        
          ivblayr=j

          write(99,1001)
     &'   CFC SURFACE |  PAZI  |  ANGI  |  PROANG  | IRadB |',
     &' IRadD |IsShd? |TranBB | TranD | Refl  |',
     &'VBLAYR|   W   |   S   |   PHI   | ORIENT |'

          write(99,1002)i,pazi(icomp,i),fInc_angle(i),
     & proang_s(icomp,icfctp,i),
     & RadIncDirS(icomp,i),RadIncDifS(icomp,i),
     & issur_self_shd(icomp,i),transBB_S(icomp,i),transD_S(icomp,i),
     & refl_S(icomp,i),ivblayr,vb_w(icomp,icfctp),
     & vb_s(icomp,icfctp),vb_phiNew(icomp,icfctp),vb_VorH(icomp,icfctp)
       
          elseif(icfcltp(icomp,icfctp,j).eq.iPleatedDrape) then
       
          idrplayr=j

          write(99,1001)
     &'   CFC SURFACE |  PAZI  | OMEGA_V | OMEGA_H | IRadB |',
     &' IRadD |IsShd? |TranBB | TranD | Refl  |',
     &'DRPLAYR|   W   |   S   |'
          write(99,1002)i,pazi(icomp,i),omega_v_s(icomp,icfctp,i),
     & omega_h_s(icomp,icfctp,i),
     & RadIncDirS(icomp,i),RadIncDifS(icomp,i),
     & issur_self_shd(icomp,i),transBB_S(icomp,i),transD_S(icomp,i),
     & refl_S(icomp,i),idrplayr,drp_w(icomp,icfctp),
     & drp_s(icomp,icfctp)

          elseif(icfcltp(icomp,icfctp,j).eq.iRollerBlind) then
       
          irldlayr=j

          write(99,8884)
     &'    CFC SURFACE |  PAZI  | ANGI | IRadB | IRadD | IsShd?  ',
     &'|TranBB | TranD | Refl  |  N/A  |RLDLAYR|'
          write(99,8885)i,pazi(icomp,i),fInc_angle(i),
     & RadIncDirS(icomp,i),RadIncDifS(icomp,i),
     & issur_self_shd(icomp,i),transBB_S(icomp,i),transD_S(icomp,i),
     & refl_S(icomp,i),irldlayr,irldlayr
       
          elseif(icfcltp(icomp,icfctp,j).eq.iInsectScreen) then
       
          ibuglayr=j

          write(99,8884)
     &'    CFC SURFACE |  PAZI  | ANGI | IRadB | IRadD | IsShd?',
     &'|TranBB | TranD | Refl  |  N/A  |BUGLAYR|'
          write(99,8885)i,pazi(icomp,i),fInc_angle(i),
     & RadIncDirS(icomp,i),RadIncDifS(icomp,i),
     & issur_self_shd(icomp,i),transBB_S(icomp,i),transD_S(icomp,i),
     & refl_S(icomp,i),ibuglayr,ibuglayr
       
          endif
       
  17  continue
       
 1001     format(A57,A41,A44)
 1002     format(6x,I10,3x,5F9.3,I6,3f9.3,I5,2x,3F9.3,A8)
 8884     format(A57,A41)
 8885     format(3x,I10,4F9.3,I10,3F9.3,I5) 
 

            write(99,1003)
     &'     node  | qtmcaExt | qtmcaFin |'

 1003       format(4x,A40)

            do 18 l=1,nn

              write(99,1004)l,qtmca_ext(icomp,i,l),qtmca(i,l,2)

 1004         format(6x,I12,1x,2F11.3)

 18         continue
          end if
 16     continue
      END IF
C********************************************************************

      return
      end

C ********************************************************************
C                         --CFC_control--
C
C Created by: Bart Lomanowski
C Initial Creation Date: April 2009
C
C This subroutine carries out time-step control of CFCs based on
C control data stored in COMMON/CFCCTL. It first collects information
C on sensor/actuator/control type and carries out basic setpoint 
C control for the specified CFC type. 
C
C ********************************************************************
      subroutine CFC_control()
      use h3kmodule
      IMPLICIT NONE
#include "building.h"
#include "control.h"
#include "CFC_common.h"
      COMMON/OUTIN/IUOUT,IUIN
      integer iuout,iuin

      common/c1/ncomp,ncon
      integer ncomp,ncon
      
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      real tfa,qfa
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      real TFC,QFC
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      real TFS,QFS
      
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      real qfp,qff,tp,tf,qdp,qdf,vp,vf,dp,df,hp,hf
      
      COMMON/BTIME/BTIMEP,BTIMEF
      real btimep,btimef
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      integer ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),
     &            ARP(MCOM,MS),ARF(MCOM,MS)
      integer IE
      real ATP,ATF,ARP,ARF

      CHARACTER outs*124
      
C Working variables to temporarily store slat angle for daytype and hour
C      real vb_phiX
C      dimension vb_phiX(mcom,mcfc,mdty,MT)
C      real slat_angleX
 
      integer i,j,k           ! do loop indices
      integer ictlfun         ! control function index
      integer nctldaytypes    ! number of control day types
      integer iday            ! current day
      integer idaytype        ! day type
      integer idayofweek      ! day of week (1 to 7)
      integer nperiods        ! number of periods in control daytype
      integer iperiod         ! control period
      integer icomp           ! zone index
      integer icfctp          ! CFC type index
      integer iact1, iact2, iact3   ! actuator details
      integer isen1, isen2, isen3   ! sensor details
      integer ictype                ! control type
      integer ischedule_shdstateONorOFF
      real shdONsetpoint
      real shdOFFsetpoint
      real shdONslatPOSITION
      real shdOFFslatPOSITION
      real schedule_slat_angle
      real temp                     ! temperature [C] control parameter - can be dbt, dbt/mrt, sol-air
      real solrad                   ! solar radiation [W/m2] control parameter - can be diff hor, dir norm, total incident on surface
      real tmrt                     ! mrt temperature [C]
      real solrad_dir               ! incident direct solar radiation [W/m2] - returned by sub. get_incident_solrad
      real solrad_diff              ! incident diffuse solar radiation [W/m2] - returned by sub. get_incident_solrad
      character msgout*124
      character*2 cZone_Chars, cCFC_Chars
      
C Prior to initiating control logic, set the default slat angle and 
C shading state for the next time step in all zones and all CFC types
C containing a shade layer.    
      do 30 i = 1, ncomp
        if(icfc(i).eq.1)then
          do 31 j = 1, ncfc(i)
            do 32 k = 1, ncfc_el(i,j)
            
              if(icfcltp(i,j,k).eq.iVenBlind)then
              
                vb_phiNew(i,j) = vb_phi_SV(i,j)
                i_shd(i,j)     = i_shd_SV(i,j)
                
                if(NSINC.eq.1)then
                  vb_phiNew(i,j) = vb_phi(i,j)  !if first timestep, use initial slat angle from *.cfc input file
                  i_shd(i,j) = 1                !if first timestep, initialize shading flag to ON
                endif
                
              elseif(icfcltp(i,j,k).gt.iVenBlind)then
               
                i_shd(i,j)     = i_shd_SV(i,j)
                
                if(NSINC.eq.1)then
                  i_shd(i,j) = 1                !if first timestep, initialize shading flag to ON
                endif
                
              endif
              
  32        continue
  31      continue   
        endif
  30  continue
      
      
C -------------------------------------------------------------------
C Control the zone complex fenestration shading process according
C to the sensor location of the associated control function. 
      iday = idyp
      idayofweek = idwp
      
      if(ihrp.eq.24)then
        iday = idyf
        idayofweek = idwf
      endif
      
C Loop through each complex fenestration control function.

      IF (nCFCctlloops.gt.0)THEN
        do 10 i = 1, nCFCctlloops
          ictlfun = i
          nctldaytypes = nCFCctldaytypes(i)
          
C Determine if period validy dates of daytypes in current 
C control loop are valid for current calendar day.
          
          ! for weekday, saturday and sunday daytypes
          IF(nctldaytypes.eq.0)THEN
            if(idayofweek.eq.6)then
              idaytype = 2
            elseif(idayofweek.eq.7)then
              idaytype = 3
            else
              idaytype = 1
            endif
            if(iday.le.iCFCctldatevalid(i,idaytype,2).and.iday
     &      .ge.iCFCctldatevalid(i,idaytype,1))goto 11                    
          ELSE
            ! Loop through each daytype until a valid daytype is found
            do 20 j = 1, nctldaytypes
              idaytype = j
              if(iday.le.iCFCctldatevalid(i,j,2).and.iday
     &               .ge.iCFCctldatevalid(i,j,1))goto 11
  20        continue
          ENDIF

C If we are here then no control for control loop i. Skip to end of loop and 
C continue with next control period.  
          goto 10
      
C Commence control solution.    
  11      if(btimef.gt.24.)btimef = btimef - 24.  
          nperiods = nCFCdayctlperiods(ictlfun,idaytype)
      
          do 40 k = 1, nperiods
            iperiod = k
            if(k.eq.nperiods)then
              if(btimef.gt.
     &          CFCctlperiodstart(ictlfun,idaytype,nperiods))
     &          goto 12
            else
              if(btimef.le.
     &           CFCctlperiodstart(ictlfun,idaytype,k+1))goto 12
            endif   
  40      continue    
 
C If we are here then no control for control function i. Skip to end of loop
C and continue with next control period.   
          goto 10
 
C Now we have found a valid daytype and period. Apply appropriate control law. 
C <<Currently only basic control is avaialble so no need to call separate routine>>

C -------------------------------------------------------------------
C Determine actuator type and copy control data to working variables
C -------------------------------------------------------------------

  12      iact1 = iCFCactuator(ictlfun,1)    ! 0=shade ON/OFF, 1=slat angle, 2=both (for schedule
          iact2 = iCFCactuator(ictlfun,2)    ! zone index
          iact3 = iCFCactuator(ictlfun,3)    ! CFC type

C Actuator controls shade ON/OFF state (ie. shade layer deployed/retracted).     
          if(iact1.eq.0)then
            shdONsetpoint = CFCmiscdata(ictlfun,idaytype,iperiod,2)
            shdOFFsetpoint = CFCmiscdata(ictlfun,idaytype,iperiod,3)      

C Actuator controls slat angle of slat-type shade layer.
          elseif(iact1.eq.1)then
            shdONsetpoint = CFCmiscdata(ictlfun,idaytype,iperiod,2)
            shdOFFsetpoint = CFCmiscdata(ictlfun,idaytype,iperiod,3)
            shdONslatPOSITION = 
     &            CFCmiscdata(ictlfun,idaytype,iperiod,4)
            shdOFFslatPOSITION = 
     &            CFCmiscdata(ictlfun,idaytype,iperiod,5) 
C Schedule
          elseif(iact1.eq.2)then
            ischedule_shdstateONorOFF = 
     &      INT(CFCmiscdata(ictlfun,idaytype,iperiod,2))
            schedule_slat_angle = 
     &            CFCmiscdata(ictlfun,idaytype,iperiod,3)
          endif

C -------------------------------------------------------------------
C Set shade ON/OFF state or slat angle based on sensor and control
C type data
C -------------------------------------------------------------------

          isen1 = iCFCsensor(ictlfun,1)
          isen2 = iCFCsensor(ictlfun,2)
          isen3 = iCFCsensor(ictlfun,3)
          ictype = iCFCctltype(ictlfun,idaytype,iperiod)

C Determine appropriate temperature parameter to use.
          IF(ictype.eq.1.or.ictype.eq.2)then        ! senses temperature
      
            if(isen1.eq.-3.and.isen2.eq.0)then      ! ambient temperature
              temp = TF
            elseif(isen1.eq.-3.and.isen2.eq.1)then  ! sol-air temperature
              !temp = SOLAIR(TF,QFF,QDF)            ! **THIS FUNCTION DOES NOT CURRENTLY WORK**
            elseif(isen1.eq.-2)then                 ! mix of dry bulb and mean radiant temp.
              call MZMIXT(isen2,tmrt,temp)          ! **NOT SURE IF THIS WORKS**
            elseif(isen1.gt.0)then                  ! temperature in zone corresponding to isen1
              if(isen2.eq.0)then
              
                temp = tfa(isen1)
            
              elseif(isen2.gt.0)then
              
                if(isen3.eq.0)then
                  temp = tfs(isen1,isen2)
                elseif(isen3.gt.0)then
                  temp = tfc(isen1,isen2,isen3)
                endif
            
              endif
            endif

C Perform actual control of shade ON/OFF and slat angle here.
            if(temp.gt.shdONsetpoint)then
              if(iact1.eq.0)then                              ! actuate shade ON/OFF
                vb_phiNew(iact2,iact3) = vb_phi(iact2,iact3)  ! default slat angle
                i_shd(iact2,iact3)     = 1                    ! shade state ON
              elseif(iact1.eq.1)then                          ! actuate slat angle
                vb_phiNew(iact2,iact3) = shdONslatPOSITION    ! set new slag angle
                i_shd(iact2,iact3)     = 1                    ! shade state ON
              endif
            elseif(temp.lt.shdOFFsetpoint)then
              if(iact1.eq.0)then                              ! actuate shade ON/OFF
                vb_phiNew(iact2,iact3) = vb_phi(iact2,iact3)  ! default slat angle
                i_shd(iact2,iact3)     = 0                    ! shade state OFF
              elseif(iact1.eq.1)then                          ! actuate slat angle
                vb_phiNew(iact2,iact3) = shdOFFslatPOSITION   ! set new slag angle
                i_shd(iact2,iact3)     = 1                    ! shade state OFF     
              endif
            endif
        
          ELSEIF(ictype.eq.3.or.ictype.eq.4)then    ! senses solar radiation

C Determine appropriate solar radiation parameter to use.
            if(isen1.eq.-3.and.isen2.eq.4)then      ! senses diff. horizontal solar radiation
              solrad = QFF
            elseif(isen1.eq.-3.and.isen2.eq.5)then  ! senses direct normal solar radiation
              solrad = QDF
            elseif(isen1.eq.-4)then                 ! senses total radiation incident on surface
            
C Check that ext. rad sensor is on external surface.
              if(IE(isen2,isen3).eq.0)then
                call get_incident_solrad(isen2,isen3,
     &            solrad_dir,solrad_diff)
                solrad = solrad_dir + solrad_diff   ! total radiation incident on surface
              else
                write(msgout,'(A,i3)')
     &            'In CFC control function: ', ictlfun
                call USRMSG(msgout,
     &          'Surface incident radiation sensor must be external.'
     &          ,'W')
C-----------Kill simulation     
                stop    
              endif
          
            endif

C Perform actual control of shade ON/OFF and slat angle here
            if(solrad.gt.shdONsetpoint)then
              if(iact1.eq.0)then
                vb_phiNew(iact2,iact3) = vb_phi(iact2,iact3)  ! default slat angle
                i_shd(iact2,iact3)     = 1                    ! shade state ON
              elseif(iact1.eq.1)then
                vb_phiNew(iact2,iact3) = shdONslatPOSITION    ! set new slag angle
                i_shd(iact2,iact3)     = 1                    ! shade state ON
              endif
            elseif(solrad.lt.shdOFFsetpoint)then
              if(iact1.eq.0)then
                vb_phiNew(iact2,iact3) = vb_phi(iact2,iact3)  ! default slat angle
                i_shd(iact2,iact3)     = 0                    ! shade state OFF
              elseif(iact1.eq.1)then
                vb_phiNew(iact2,iact3) = shdOFFslatPOSITION   ! set new slag angle
                i_shd(iact2,iact3)     = 1                    ! shade state ON     
              endif
            endif

          ELSEIF(ictype.eq.5)then                   ! senses wind speed, actuates shade ON/OFF
      
            if(isen1.eq.-3.and.isen2.eq.2)then
C Perform actual control of shade ON/OFF and slat angle here
              if(VF.gt.shdONsetpoint)then
                if(iact1.eq.0)then
                  vb_phiNew(iact2,iact3) = vb_phi(iact2,iact3)  ! default slat angle
                  i_shd(iact2,iact3)     = 1                    ! shade state ON
                elseif(iact1.eq.1)then
                  vb_phiNew(iact2,iact3) = shdONslatPOSITION    ! set new slag angle
                  i_shd(iact2,iact3)     = 1                    ! shade state ON
                endif
              elseif(VF.lt.shdOFFsetpoint)then
                if(iact1.eq.0)then
                  vb_phiNew(iact2,iact3) = vb_phi(iact2,iact3)  ! default slat angle
                  i_shd(iact2,iact3)     = 0                    ! shade state OFF
                elseif(iact1.eq.1)then
                  vb_phiNew(iact2,iact3) = shdOFFslatPOSITION   ! set new slag angle
                  i_shd(iact2,iact3)     = 1                    ! shade state OFF     
                endif
              endif        
            endif
        
          ELSEIF(ictype.eq.6)then                   ! senses wind direction, actuates shade ON/OFF
      
            if(isen1.eq.-3.and.isen2.eq.3)then
C Perform actual control of shade ON/OFF and slat angle here
              if(DF.gt.shdONsetpoint)then
                if(iact1.eq.0)then
                  vb_phiNew(iact2,iact3) = vb_phi(iact2,iact3)  ! default slat angle
                  i_shd(iact2,iact3)     = 1                    ! shade state ON
                elseif(iact1.eq.1)then
                  vb_phiNew(iact2,iact3) = shdONslatPOSITION    ! set new slag angle
                  i_shd(iact2,iact3)     = 1                    ! shade state ON
                endif
              elseif(DF.lt.shdOFFsetpoint)then
                if(iact1.eq.0)then
                  vb_phiNew(iact2,iact3) = vb_phi(iact2,iact3)  ! default slat angle
                  i_shd(iact2,iact3)     = 0                    ! shade state OFF
                elseif(iact1.eq.1)then
                  vb_phiNew(iact2,iact3) = shdOFFslatPOSITION   ! set new slag angle
                  i_shd(iact2,iact3)     = 1                    ! shade state ON     
                endif
              endif  
            endif
        
          ELSEIF(ictype.eq.7)then                   ! no sensor, schedule only
            i_shd(iact2,iact3) = ischedule_shdstateONorOFF
            vb_phiNew(iact2,iact3) = schedule_slat_angle
          ENDIF    

C Check that new slat angle is within correct range:
C     -90 (deg.) < vb_phiNew < 90 (deg.)
          icomp=iact2     ! zone index
          icfctp=iact3    ! CFC type

          if(vb_phiNew(icomp,icfctp).lt.-89.9.or.vb_phiNew(icomp,icfctp)
     &      .gt.89.9)then
C Send warning error message and overwrite angle to 89.9 deg.
            write(outs,'(a,a,i2,a,i2,a)')
     &        'WARNING: Calculated slat angle for CFC',
     &        ' type ',icfctp, 'in zone ',icomp, ' is out of range.'
            CALL EDISP(IUOUT,outs)
            write(outs,'(a)')'        Setting slat angle to 89.9 deg. '
            CALL EDISP(IUOUT,outs)
            vb_phiNew(icomp,icfctp)=89.9
          end if
      
  10    continue  
  
      ELSE
        goto 9999
      ENDIF
      
C Save slat angle and shade state for use in future time-step.    
 9999 do 33 i = 1, ncomp
        if(icfc(i).eq.1)then
          do 34 j = 1, ncfc(i)
            do 35 k = 1, ncfc_el(i,j)
              if(icfcltp(i,j,k).eq.iVenBlind)then
                vb_phi_SV(i,j) = vb_phiNew(i,j)
                i_shd_SV(i,j)     = i_shd(i,j)
              elseif(icfcltp(i,j,k).gt.iVenBlind)then
                i_shd_SV(i,j)     = i_shd(i,j)
              endif
  35        continue
  34      continue   
        endif
  33  continue

C Make slat angle and shade state available in H3KReports.
      DO 43 i = 1, ncomp                            ! Examine each zone in turn.
        IF (icfc(i)==1) THEN                        ! Check if there are CFCs in this zone.
          DO 44 j = 1, ncfc(i)                      ! Examine each type of CFC in zone.

            if ( i > 9 ) then                       ! Tag output with zone number (pad to 'XX' format).
              write( cZone_Chars, '(I2)' ) i
            else
              write( cZone_Chars, '(A,I1)' ) '0', i
            end if
            if ( j > 9 ) then                       ! Tag output with CFC type.
              write( cCFC_Chars, '(I2)' ) j
            else
              write( cCFC_Chars, '(A,I1)' ) '0', j
            end if

            call AddToReport (                      ! Report control state.
     &             rvCFCShadeCtl%Identifier,
     &             float( i_shd(i,j) ),
     &             cZone_Chars,
     &             cCFC_Chars )

            call AddToReport (                      ! Report slat angle.
     &             rvCFCSlatAngle%Identifier,
     &             vb_phiNew(i,j),
     &             cZone_Chars,
     &             cCFC_Chars )

  44      CONTINUE
        END IF
  43  CONTINUE


      return
      end

C ********************************************************************
C                      --get_incident_solrad--
C
C Created by: Bart Lomanowski
C Initial Creation Date: May 2009
C
C Calculates direct and diffuse components of solar radiation incident
C on an external surface (including shading from obstructions).
C
C ********************************************************************
      subroutine get_incident_solrad(icomp,isur,solrad_dir, solrad_diff)
#include "building.h"    
      
      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      real SAZI, SALT
      integer ISUNUP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      integer IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/OUTIN/IUOUT,IUIN
      integer iuout,iuin
      COMMON/SHAD2/ISHD(MCOM),IGCS(MCOM,MS),PO(MCOM,MS,MT),
     & POF(MCOM,MS,MT),IGCI(MCOM,MS),insst(mcom,mgt,mt,misur),
     & pinsst(mcom,mgt,mt,misur)
      integer ISHD, IGCS, IGCI, insst
      real PO,POF,pinsst
      
      real QD, QF       ! future row solar intensity values
      real CAI          ! cosine of angle of incidence
      real ANGI         ! angle of incidence
      real PSAZI2       ! solar minus surface normal azimuths
      real PSELV2       ! solar minus surface normal elevations
      real SRADDO       ! direct solar radiation incident on isur (not corrected for shading)
      real SRADF        ! diffuse solar radiation incident on isur (not corrected for shading)
      real skydif       ! sky diffuse component (currently not used here)
      real grddif       ! ground diffuse component (currently not used here)
      real POO
      real POFF
      
      real solrad_dir   ! RETURN: direct solar radiation incident on isur and corrected for shading
      real solrad_diff  ! RETURN: diffuse solar radiation incident on isur and corrected for shading

      character outs*124
            
      integer IND1, IND2, IANGBI1, IANGBI2      ! dummy, not used
      integer ians      ! surface self shading flag, 1=self-shaded, 0=not self-shaded
          
      QD = 0.0
      QF = 0.0
      CAI = 0.0
      ANGI = 0.0
      PSAZI2 = 0.0
      PSELV2 = 0.0
      SRADDO = 0.0
      SRADF = 0.0
      skydif = 0.0
      grddif = 0.0
      POO = 0.0
      POFF = 0.0
      solrad_dir = 0.0
      solrad_diff = 0.0
      
C -------------------------------------------------------------------
C First determine the solar angles at this time.
C -------------------------------------------------------------------
      CALL MZSANG
      
C -------------------------------------------------------------------
C IF sun not up, set incident solar rad to zero and jump to end of 
C routine.
C -------------------------------------------------------------------
      if (ISUNUP.EQ.0) then
        solrad_dir = 0.0
        solrad_diff = 0.0
        goto 1000
      endif

C -------------------------------------------------------------------
C Establish future row solar intensity values: QD & QF.
C -------------------------------------------------------------------
      CALL MZSINT(icomp,QD,QF)


C -------------------------------------------------------------------
C Is surface facing away from the sun so that it can only
C receive diffuse component. IANS=1; yes, self-shaded.
C -------------------------------------------------------------------
      ians=0
      CALL MZSFSH(icomp,isur,ians)

C -------------------------------------------------------------------
C Calculate the angle of incidence between the sun and surface ISRF,
C ANGI; COS of ANGI, CAI;  for this external construction.
C -------------------------------------------------------------------
      IF(ians.NE.1)THEN
        CALL MZSCAI(icomp,isur,CAI,ANGI,IND1,IND2,PSAZI2,PSELV2,
     &              IANGBI1,IANGBI2)
        IF(ANGI.LT.0..OR.ANGI.GT.90.)THEN
          write(outs,999)ANGI
 999      format(' MZSLGN error: incidence angle = ',F10.3,'.')
          call edisp(iuout,outs)
          goto 1000
        ENDIF
      ELSE

C Set default, otherwise undefined.
        CAI=0.
        ANGI=90.
      ENDIF

C -------------------------------------------------------------------
C Calculate direct (SRADDO) and diffuse (SRADF) on surface isur.
C -------------------------------------------------------------------
      CALL MZSRAD(icomp,isur,QD,QF,CAI,SRADDO,SRADF,SKYDIF,GRDDIF)

C Surface is self-shaded; no direct radiation.
      IF(IANS.EQ.1)SRADDO=0.
          
C Replace calculated values by measured values from tdf file (leave
C this alone for now).

C -------------------------------------------------------------------
C Now we have the external radiation on each surface. Apply shading
C factor generated by ish, if shading database exists.
C -------------------------------------------------------------------
      IF(ISHD(ICOMP).EQ.0.OR.ISHD(ICOMP).EQ.2)THEN
         POO=0.0
         POFF=0.0
      ELSE
         POO=PO(ICOMP,isur,IHRF)
         POFF=POF(ICOMP,isur,IHRF)
      ENDIF     

C -------------------------------------------------------------------
C Calculate RETURN values. 
C -------------------------------------------------------------------
      solrad_dir = SRADDO*(1.-POO)
      solrad_diff = SRADF*(1.-POFF)

 1000 return

      end
