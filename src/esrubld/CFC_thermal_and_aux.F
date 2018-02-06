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

C This file contains the following routines of the CFC functionality:
C
C      CFC_thermal_processing
C      qlwCFC
C      FSsolve
C      solar_multilayer
C      cfc_convection
C      ConvVertCav
C      CFC_time_dependent_Rgap
C      PD_LW
C      OPENNESS_LW
C      SETCoef
C      TDMAsol
C      profile_angle
C      set_HR_to_zero
C      set_QELW_to_zero
C      CFCoutput
C      CFC_control
C      get_incident_solrad
C
C The subroutines defined in <complex_fenestration_1.F> are required
C for (some of) the above functions.

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
  
cx Slat angle change implies diff > 0.x, no?? Logic suggested by A. Geissler.
cx Uncomment this line to implement this rather than legacy logic in line 130.
cx          IF((NSINC.eq.1).or.(diff.gt.0.1))then
C Legacy logic for slat angle change is used in the next line.
          IF((NSINC.eq.1).or.(diff.lt.0.1))then

cx << insert check for CFC thin layers here??
cx    PROBLEM: availability of layer (and timestep length?) data ... ???
cx        if (icfcfl(ICOMP,I).ne.0) then
C           Check rho cp s / t - value; thermal layer values from
C           materials.h entries for zone,surface,layer;
C           Check outermost layer (index 1) only.
c            cfclaythrm=cfcdbden(cfcarrayindex)
c     &                *cfcdbsht(cfcarrayindex)*cfcthck()/TIMESEP
c          if (cfclaythrm.lt.4.0) then
C           Issue warning! << output function needs change!! >>
c            CALL USRMSG('*** WARNING : CFC Layer very thin!!',
c     &         'Numerical issues (e.g. MZELWE errors) likely!','W')
c          endif
c        endif

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

C...............Save effective properties (front emissivity and transmittance) into
C...............common variables.
                rlwEF(icomp,icfctp,j) = eff_lwE
                rlwT(icomp,icfctp,j)  = eff_lwT

C...............Determine effective longwave properties for BACK layer surface (facing
C...............the indoors) by setting negative slat angle
                call vb_eff_diff_properties(tmp_vb_w,tmp_vb_s,
     &            tmp_vb_phi*(-1.0),tmp_lwEF,tmp_lwEB,tmp_lwT,
     &            eff_lwE,eff_lwT)

C...............Save efective back emissivity into common variable
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

C Assign nodal temperatures and convert to kelvin
      nn=nndc(icomp,isurf)
      do 30 i=1,nn
C        if(i.lt.nn)then
           tnd(i)=tfc(icomp,isurf,i)+273.15
C        else
C          tsrf=tfs(icomp,isurf)+273.15
C        end if
  30  continue

C Assign emittance to front and back surfaces of cfc type itp
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
C geometry.h for zname.
#include "geometry.h"
#include "CFC_common.h"

      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      real sazi,salt
      integer isunup
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      real pazi,pelv

      integer icomp,isurf,icfctp
      real pi,r, omega_v, omega_h

C.....DECLARATIONS for H3Kreporting object
      CHARACTER*12   cZone_Chars, cSurf_Chars

      pi = 4.0 * ATAN(1.0)
      r=pi/180.

C.....Vertical profile angle
      omega_v = ATAN(TAN(SALT*r)/COS(SAZI*r-PAZI(icomp,isurf)*r))/r
C.....Horizontal profile angle
      omega_h = SAZI-PAZI(ICOMP,ISURF)
      
C.....Switch for zone name output here
      if (ReportBoolConfig("use_zonenames")) then
        write (cZone_Chars,'(A)') zname(icomp)(1:lnzname(icomp))
      else
C.......Pad zone index to 'XXX'
        write (cZone_Chars,'(A,I3.3)') 'zone_',icomp
      endif ! use_zonenames

C << the following would ideally require that surface name lengths
C    are stored in a common like zone name lengths ... filled in
C    georead() (egeometry.F line 1750 ff.)/ egomin() (egeometry.F line 594)? >>

C.....Switch for surface name output here
cx      if (ReportBoolConfig ("use_surfacenames")) then
cx        write (cSurf_Chars,'(A)')
cx     &          sname(iZone,isurf)(1:lnsname(icomp,isurf))
cx      else
C.......Pad surface index to 'XXX'
          write (cSurf_Chars,'(A,I3.3)') 'surface_',isurf
cx      endif ! use_surfacenames

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
     & '        CFC SURFACE |  PAZI  |  ANGI  |  PROANG  | HCIF |',
     &       '  HCOF |VBLAYR|   W     |   S   |   PHI   | ORIENT |'
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
     & '       CFC SURFACE |  PAZI  |  ANGI  |  PROANG  | HCIF   | ',
     & 'HCOF  |VBLAYR|   W   |   S    |   PHI   | ORIENT |'

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
        
 9996     format(2A)
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
     &         (icfcltp(icomp,icfctp,l).ge.2.and.l.gt.1.and.
     &          l.lt.ne))then

              write(98,9999)l,tfc(icomp,i,(l*2)-1),tfc(icomp,i,l*2),
     &          tfc(icomp,i,(l*2)+1),
     &          cfcRgap(icomp,i,l,2),qcfc_lw(icomp,i,(l*2)-1,2),
     &          qcfc_lw(icomp,i,l*2,2),qcfc_lw(icomp,i,(l*2)+1,2),0.,0.,
     &          0.,0.,0.,qcfc_cv(icomp,i,(l*2)-1,2),
     &          qcfc_cv(icomp,i,l*2,2),qcfc_cv(icomp,i,(l*2)+1,2)

      !outdoor blind
            elseif(icfcltp(icomp,icfctp,l).ge.2.and.l.eq.1)then

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
 9999       format(6x,I12,1x,3F8.3,F8.2,3F8.3,6F13.3,2F8.3)

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
C Use geometry.h for zname
#include "geometry.h"
#include "control.h"
#include "CFC_common.h"
C net_flow supplies MNOD
#include "net_flow.h"
#include "tdf2.h"
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

      common/trc/itrc
      integer itrc

      common/dllerr/dllsubr,dllmesg
      character dllsubr*12,dllmesg*124

      logical dll

      CHARACTER outs*124
      
C Working variables to temporarily store slat angle for daytype and hour
C      real vb_phiX
C      dimension vb_phiX(mcom,mcfc,mdty,MT)
C      real slat_angleX

      integer IER,ISD,itdi,IFOC     ! used in context with tdf ...
      integer i,j,k                 ! do loop indices
      integer ictlfun               ! control function index
      integer nctldaytypes          ! number of control day types
      integer iday                  ! current day
      integer idaytype              ! day type
      integer idayofweek            ! day of week (1 to 7)
      integer nperiods              ! number of periods in control daytype
      integer iperiod               ! control period
      integer icomp                 ! zone index
      integer iZone                 ! zone index for zone loops
      integer icfctp                ! CFC type index
      integer iact1, iact2, iact3   ! actuator details
      integer isen1, isen2, isen3   ! sensor details
      integer ictype                ! control type
      integer ischedule_shdstateONorOFF
      real VAL
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
      character*12 cZone_Chars
      character*2  cCFC_Chars

      DIMENSION VAL(MBITS+2)

C Check if running in dll mode.
      call isadll(dll)


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

  12      iact1 = iCFCactuator(ictlfun,1)    ! 0=shade ON/OFF, 1=slat angle, 2=both (for schedule)
                                             ! 9=both (read from tdf)
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

          elseif(iact1.eq.9)then
C Temporal data file (tdf)

            if(ICFCCTL(ictlfun).ne.0) then
              itdi=ICFCCTL(ictlfun)
              IFOC=itdi
C           Data in temporal database (on/off state and slat angle).
              CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
              ischedule_shdstateONorOFF = VAL(ISD)
              schedule_slat_angle       = VAL(ISD+1)

C Debug.
C           write(6,*)'tdf ctl data @',btimef,' is item',itdi,
C     &       ' column ',isd,' value ',ischedule_shdstateONorOFF,
C     &       ' column ',isd+1, value ',schedule_slat_angle,
C     &       ' for ctl loop ',icf

            else
C            Error ... !!
             write(outs,'(a,a,i2,a,i2,a)')
     &       'CFC_control: ICFCCTL does not point to a valid temporal',
     &       ' item for zone ',iact2,', CFC type',iact3,'.'
             if(dll)then
               dllsubr='CFC_control'
               dllmesg=outs
               ier=2
               return
             else
               call edisp(iuout,outs)
               call epwait
               call epagend
               STOP
             endif
            endif

          endif ! check iact1

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

          ELSEIF(ictype.eq.9)then                   ! no sensor, tdf
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
            write(outs,'(a,a,i2,a,i3,a)')
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
      DO 43 iZone = 1, ncomp                        ! Examine each zone in turn.
        IF (icfc(iZone)==1) THEN                    ! Check if there are CFCs in this zone.
C.........Switch for zone name output here
          if (ReportBoolConfig("use_zonenames")) then
            write (cZone_Chars,'(A)') zname(iZone)(1:lnzname(iZone))
          else
C...........Pad zone index to 'XX'
            write (cZone_Chars,'(A,I3.3)') 'zone_',iZone
          endif ! use_zonenames

          DO 44 j = 1, ncfc(iZone)                  ! Examine each type of CFC in zone.

            if ( j > 9 ) then                       ! Tag output with CFC type.
              write( cCFC_Chars, '(I2)' ) j
            else
              write( cCFC_Chars, '(A,I1)' ) '0', j
            end if

            call AddToReport (                      ! Report control state.
     &             rvCFCShadeCtl%Identifier,
     &             float( i_shd(iZone,j) ),
     &             cZone_Chars,
     &             cCFC_Chars )

            call AddToReport (                      ! Report slat angle.
     &             rvCFCSlatAngle%Identifier,
     &             vb_phiNew(iZone,j),
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
      subroutine get_incident_solrad(icomp,isur,solrad_dir,solrad_diff)
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
