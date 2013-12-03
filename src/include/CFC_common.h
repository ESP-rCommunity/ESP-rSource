C This file contains commons related to the implementation
C of Complex Fenestration Consturctions (CFC):

C Created by: Bartosz Lomanowski
C Initial Creation Date: May 7, 2008

C---------------------------------------------------------------------------------
C PARAMETERS
C
C---------------------------------------------------------------------------------

C Flag (manually set for now) for calculation of ground and sky diffuse components
      integer i_ground_sky_diff_calc
      parameter ( i_ground_sky_diff_calc = 0 )

C Flags for solar_multilayer subroutine to indicate which diffuse properties 
C (sky or ground) to use for the venetian blind layer. 
      integer i_ground, i_sky
      parameter ( i_sky = 1 )
      parameter ( i_ground = 2 )

C---------------------------------------------------------------------------------

C Named constants for complex fenestration layer types

      Integer iGasGap
      Parameter ( iGasGap = 0 )
      
      Integer iGlazing
      Parameter ( iGlazing = 1 )
      
      Integer iVenBlind
      Parameter ( iVenBlind = 2 )
      
      Integer iPleatedDrape
      Parameter ( iPleatedDrape = 3 )
      
      Integer iRollerBlind
      Parameter ( iRollerBlind = 4 ) 
      
      Integer iInsectScreen
      Parameter ( iInsectScreen = 5 )
      
C---------------------------------------------------------------------------------
C IMPORT COMMONS
C
C Used in the import process for creation of *.cfc zone file based
C on data contained in *.GSL files created using the GSLedit 
c Glazing/Shading Editor 
C---------------------------------------------------------------------------------

      COMMON/GSLedit/GSLfiles(mcom,mcfc),
     &               iGSLfile(mcom,mcfc)

C Array used to store filenames of .GSL import files for each CFC type
      CHARACTER*72 GSLfiles

C Index for each CFC type: 
C      1: *.GSL import file exists
C      0: *.GSL file does not exist
      INTEGER iGSLfile 
      
C---------------------------------------------------------------------------------

      COMMON/impCFC/im_cfcfl(mcom,ms),
     &              im_ncfc(mcom),
     &              im_ncfc_el(mcom,mcfc),
     &              im_cfcltp(mcom,mcfc,me)

C CFC type index: 
C      0: not-CFC
C    >=1: CFC type for each surface in zone 

      INTEGER im_cfcfl

C Number of CFC types in zone
      INTEGER im_ncfc

C Number of elements (layers) in CFC type
      INTEGER im_ncfc_el

C Layer type index: 
C      0: gas gap
C      1: glazing
C      2: venetian blind
C      3: drapery
C      4: roller blind
C      5: Insect screen (bug)
      INTEGER im_cfcltp
     
C---------------------------------------------------------------------------------
C Normal Solar Optical properties for each layer of CFC imported from *.GSL files.

      COMMON/impCFCsol/rim_SolRf(mcom,mcfc,me),
     &                 rim_SolRb(mcom,mcfc,me),
     &                 rim_SolT(mcom,mcfc,me),
     &                 rim_SolTf_tot(mcom,mcfc,me),
     &                 rim_SolTb_tot(mcom,mcfc,me),
     &                 rim_SolTf_bd(mcom,mcfc,me),
     &                 rim_SolTb_bd(mcom,mcfc,me)
     
C Front reflectance of each element (facing exterior)
      REAL rim_SolRf

C Back reflectance of each element(facing interior)
      REAL rim_SolRb

C Transmittance of each element
      REAL rim_SolT

C Total front transmittance of each element
      REAL rim_SolTf_tot

C Total back transmittance of each element
      REAL rim_SolTb_tot

C Beam-Diffuse front transmittance of each element
      REAL rim_SolTf_bd

C Beam-Diffuse back transmittance of each element
      REAL rim_SolTb_bd

C---------------------------------------------------------------------------------
C Normal Solar Visible properties for each layer of CFC imported from *.GSL files.

      COMMON/impCFCvis/rim_VisRf(mcom,mcfc,me),
     &                 rim_VisRb(mcom,mcfc,me),
     &                 rim_VisT(mcom,mcfc,me)

C Front reflectance of each element (facing exterior)
      REAL rim_VisRf

C Back reflectance of each element(facing interior)
      REAL rim_VisRb

C Transmittance of each element
      REAL rim_VisT

C---------------------------------------------------------------------------------
C Normal Longwave Radiative properties for each layer of CFC imported from
C *.GSL files.

      COMMON/impCFClw/rim_lwEf(mcom,mcfc,me),
     &                rim_lwEb(mcom,mcfc,me),
     &                rim_lwT(mcom,mcfc,me) 

C Front emissivity of each element (facing exterior)
      REAL rim_lwEf

C Back emissivity of each element(facing interior)
      REAL rim_lwEb

C Transmittance of each element
      REAL rim_lwT

C---------------------------------------------------------------------------------
C Venetian blind descriptors for each CFC type (for type 2 CFC layers)

      COMMON/impCFCvb/rim_vb_w(mcom,mcfc),
     &                rim_vb_s(mcom,mcfc),
     &                rim_vb_phi(mcom,mcfc),
     &                rim_vb_crown(mcom,mcfc),
     &                rim_vb_wr(mcom,mcfc),
     &                rim_vb_t(mcom,mcfc),
     &                cim_VorH(mcom,mcfc)

C Slat width (mm)
      REAL rim_vb_w

C Slat spacing (mm). For thick slats, slat spacing is the actual spacing between
C the top and bottom slat surfaces. 
C
C            -----------------------
C           |                       |
C           |      thick slat       |
C            ----------------------- -----
C                                      |
C                                      |
C                                      |
C                                      |  s (mm)
C                                      |
C                                      |
C            ----------------------- -----
C           |                       |
C           |      thick slat       |
C            -----------------------

      REAL rim_vb_s

C Slat angle (deg.)
C                                 Sign Convention
C---------------------------------------------------------------------------------
C            |      /    |                             |    \      |
C            |     /     |                             |     \     |
C            |    /      |                             |      \    |
C   exterior |      /    | interior           exterior |    \      | interior
C            |     /     |                             |     \     |
C            |    /      |                             |      \    |
C 
C           +ve slat angle                             -ve slatangle
C---------------------------------------------------------------------------------
C            |           |                             |     |     |
C            |   -----   |                             |     |     |
C            |           |                             |     |     |
C   exterior |           | interior           exterior |     |     | interior
C            |   -----   |                             |     |     |
C            |           |                             |     |     |
C
C             0 deg (open)                             90 deg (closed)
C---------------------------------------------------------------------------------
      REAL rim_vb_phi

C Slat crown (mm) - for curved slats, it is the apparent projected slat thickness
      REAL rim_vb_crown

C Slat width/curvature radius ratio 
      REAL rim_vb_wr

C Slat max thickness (mm)
      REAL rim_vb_t

C Slat orientaion: VERT or HORZ
      CHARACTER cim_VorH*4
C---------------------------------------------------------------------------------
C Pleated drape descriptors for each CFC type (for type 3 CFC layers)

      COMMON/impCFCdrp/rim_drp_w(mcom,mcfc),
     &                rim_drp_s(mcom,mcfc),
     &                rim_drp_Fr(mcom,mcfc)


C Since pleats are repetitive, an enclosure formed by two consecutive
C pleats will represent the entire drapery. A cross-section below
C describes such an enclosure represented by a pleat width, w, and a
C pleat spacing, s
C
C         w
C         <-------->
C    ^  ----------  ^
C    |  |           |
C    |  |1st pleat  | s
C    |  |               |
C    |  |               |
C  L |  ----------  -
C    |                |
C    |  2nd pleat|
C    |           |
C    |           |
C    -  ----------

C Drape width (mm)
      REAL rim_drp_w
      
C Drape spacing (mm)
      REAL rim_drp_s

C Drape fullness
      REAL rim_drp_Fr
C---------------------------------------------------------------------------------
C Insect screen descriptors for each CFC type (for type 5 CFC layers)

      COMMON/impCFCbug/rim_bug_d(mcom,mcfc),
     &                rim_bug_s(mcom,mcfc),
     &                rim_bug_Emis(mcom,mcfc)


C 
C A cross-section below describes such an enclosure represented by a 
C wire diameter, d, a wire pitch, s, and a wire emissivity, Emis

C           _
C         _   _  ___
C           _     ^
C         <--->   |
C           d     |   s
C                 |
C           _     |
C         _   _  _|_
C           _  



C           _
C         _   _
C           _  


C Insect screen wire diameter (mm)
      REAL rim_bug_d
      
C Insect screen wire pitch (mm)
      REAL rim_bug_s

C Insect screen wire emissivity
      REAL rim_bug_Emis
C---------------------------------------------------------------------------------
C Fill Gas properties:
C Gas mixture as specified in GSLedit is characterized by:
C
C     molecular mass          (g/gmole)
C     gas conductivity        (W/m.K)
C     gas viscosity           (N.s/m2)
C     gas specific heat       (J/kg.K)
C
C     Gas properties are detemined by a linear 
C     Temperature dependency of the form: 
C
C     Pmix = a + bT   
C
C     Pmix is the gas mixture property of interest, 
C     a and b are coefficients imported from GSLedit 

      COMMON/impCFCgas/rim_mlr_mass(mcom,mcfc,me),
     &                 rim_cond_A(mcom,mcfc,me),
     &                 rim_cond_B(mcom,mcfc,me),
     &                 rim_visc_A(mcom,mcfc,me),
     &                 rim_visc_B(mcom,mcfc,me),
     &                 rim_spht_A(mcom,mcfc,me),
     &                 rim_spht_B(mcom,mcfc,me)

C molecular mass of fill gas mixture (g/gmole)
      REAL rim_mlr_mass

C 'a'coefficient  - fill gas conductivity
      REAL rim_cond_A

C 'b' coefficient - fill gas conductivity
      REAL rim_cond_B

C 'a'coefficient  - fill gas viscosity
      REAL rim_visc_A

C 'b' coefficient - fill gas viscosity
      REAL rim_visc_B

C 'a'coefficient  - fill gas specific heat
      REAL rim_spht_A

C 'b' coefficient - fill gas specific heat
      REAL rim_spht_B

C---------------------------------------------------------------------------------
C Intenational Glazing Database ID (for type 0 (glazings) CFC layers)

      COMMON/impCFC_IGDB/cim_IGDB_ID(mcom,mcfc,me)

C IGDB database ID
      CHARACTER cim_IGDB_ID*8
      
C---------------------------------------------------------------------------------
C SIMULATION COMMONS
C
C These are essentially the same set of variables as the IMPORT COMMONS. These 
C common blocks are filled by reading the *.cfc zone file created during
C the import process.
C---------------------------------------------------------------------------------
C SEE IMPORT COMMONS SECTIONS ABOVE FOR VARIABLE DEFINITIONS OF THE FOLLOWING:

      COMMON/CFCindc/icfcfl(mcom,ms),
     &               ncfc(mcom),
     &               ncfc_el(mcom,mcfc),
     &               icfcltp(mcom,mcfc,me)

      INTEGER icfcfl
      INTEGER ncfc
      INTEGER ncfc_el
      INTEGER icfcltp

      COMMON/CFCsol/solRF(mcom,mcfc,me),
     &              solRB(mcom,mcfc,me),
     &              solT(mcom,mcfc,me),
     &              SolTf_tot(mcom,mcfc,me),
     &              SolTb_tot(mcom,mcfc,me),
     &              SolTf_bd(mcom,mcfc,me),
     &              SolTb_bd(mcom,mcfc,me)

      REAL solRF
      REAL solRB
      REAL solT
      REAL SolTf_tot
      REAL SolTb_tot
      REAL SolTf_bd
      REAL SolTb_bd

      COMMON/CFCvis/visRF(mcom,mcfc,me),
     &              visRB(mcom,mcfc,me),
     &              visT(mcom,mcfc,me)

      REAL visRF
      REAL visRB
      REAL visT

      COMMON/CFClw/rlwEF(mcom,mcfc,me),
     &             rlwEB(mcom,mcfc,me),
     &             rlwT(mcom,mcfc,me)

      REAL rlwEF
      REAL rlwEB
      REAL rlwT

      COMMON/CFCvb/vb_w(mcom,mcfc),
     &             vb_s(mcom,mcfc),
     &             vb_phi(mcom,mcfc),
     &             vb_crown(mcom,mcfc),
     &             vb_wr(mcom,mcfc),
     &             vb_t(mcom,mcfc)

      COMMON/CFCvbC/vb_VorH(mcom,mcfc)

      REAL vb_w
      REAL vb_s
      REAL vb_phi
      REAL vb_crown
      REAL vb_wr
      REAL vb_t
      CHARACTER vb_VorH*4

      COMMON/CFCdrp/drp_w(mcom,mcfc),
     &             drp_s(mcom,mcfc),
     &             drp_Fr(mcom,mcfc)

      REAL drp_w
      REAL drp_s
      REAL drp_Fr

      COMMON/CFCbug/bug_d(mcom,mcfc),
     &             bug_s(mcom,mcfc),
     &             bug_Emis(mcom,mcfc)

      REAL bug_d
      REAL bug_s
      REAL bug_Emis

      COMMON/CFCgas/rmlr_mass(mcom,mcfc,me),
     &              cond_A(mcom,mcfc,me),
     &              cond_B(mcom,mcfc,me),
     &              visc_A(mcom,mcfc,me),
     &              visc_B(mcom,mcfc,me),
     &              spht_A(mcom,mcfc,me),
     &              spht_B(mcom,mcfc,me), 
C...................Molar mass % fractions of 1:Air, 2:Ar, 3:Kr, 4:Xe and 4:SF6     
     &              imlr_mass_frac(mcom,mcfc,me,5)

      REAL rmlr_mass
      REAL cond_A
      REAL cond_B
      REAL visc_A
      REAL visc_B
      REAL spht_A
      REAL spht_B
      INTEGER imlr_mass_frac

C---------------------------------------------------------------------------------
C Common to save longwave properties as given in *.cfc file
C     -these are used as input for determining effective layer properties

      COMMON/CFClwS/rlwEF_sv(mcom,mcfc,me),
     &              rlwEB_sv(mcom,mcfc,me),
     &              rlwT_sv(mcom,mcfc,me)

C Front emissivity of each element (facing exterior)
      REAL rlwEF_sv

C Back emissivity of each element(facing interior)
      REAL rlwEB_sv

C Transmittance of each element
      REAL rlwT_sv

C---------------------------------------------------------------------------------
C Common to save effective longwave properties in 

      COMMON/CFClwEff/lwE_eff(mcom,mcfc,me),
     &              lwT_eff(mcom,mcfc,me)

C Effective emissivity of each element
      REAL lwE_eff

C Effective transmittance of each element
      REAL lwT_eff

C---------------------------------------------------------------------------------
      COMMON/CFCin/lcfcin(mcom),
     &             icfc(mcom)

C holds filename of *.cfc zone file
      CHARACTER lcfcin*72

C Index for *.cfc file: 
C      1: file exists
C      0: file does not exist
      INTEGER icfc 

C---------------------------------------------------------------------------------

C---------------------------------------------------------------------------------
C CFC THERMAL MODEL COMMONS
C
C These commons contain:
C Longwave radiative source terms for intraconstructional longwave radiation
C exchange within CFC and between CFC and interior zone and external environment.
C
C Convective gap resistances and convective source terms for indoor/outdoor
C shading layer. 
C---------------------------------------------------------------------------------

      COMMON/ELWE4/TEQV(MS)

C Equivalent surroundings temperature used in venetian blind 
C longwave radiation exchange balance (K).
      REAL TEQV

C---------------------------------------------------------------------------------

      COMMON/CFClwx/qcfc_lw(mcom,ms,mn,2)

C CFC longwave radiation exchange source terms (W/m^2)
      REAL qcfc_lw

C---------------------------------------------------------------------------------

      COMMON/CFCcvx/qcfc_cv(mcom,ms,mn,2)

C CFC convective exchange source terms (W/m^2)
      REAL qcfc_cv

C---------------------------------------------------------------------------------
C This common block saves the future time values of qcfc_lw source terms 
C (held in COMMON/CFClwx/) and qcfc_cv soure terms (held in COMMON/CFCcvx)
C for use as the present values at the next time-step.

      COMMON/CFClwxSV/qcfc_lwF(mcom,ms,mn),
     &                qcfc_lwP(mcom,ms,mn)
      COMMON/CFCcvxSV/qcfc_cvF(mcom,ms,mn),
     &                qcfc_cvP(mcom,ms,mn)

C Future values(W/m^2)
      REAL qcfc_lwF, qcfc_cvF

C Present values(W/m^2)
      REAL qcfc_lwP,qcfc_cvP

C---------------------------------------------------------------------------------
C Common to hold gas gap resistances (m^2.K/W)- gas gap resistance only takes
C CONVECTION into account, longwave radiation is accounted for by the qcfc source
C terms. 

      COMMON/CFCconv/cfcRgap(mcom,ms,me,2)

      REAL cfcRgap

C---------------------------------------------------------------------------------
C This common block saves the future time values of cfcRgap 
C (held in COMMON/CFCconv/) for use as the present values at the next time-step.

      COMMON/CFCconvSV/cfcRgapF(mcom,ms,me),
     &                 cfcRgapP(mcom,ms,me)

C Future value of cfcRgap (m^2.K/W)
      REAL cfcRgapF

C Present value of cfcRgap (m^2.K/W)
      REAL cfcRgapP

C---------------------------------------------------------------------------------
C Common to hold zone air convective gain from CFCs containing indoor 
C shading layer. For and indoor blind, convective heat transfer takes place not
C only on the inside face of the blind, but also the outside face and the innermost
C glass surface.

      COMMON/CFC_znair_gain/q_airndConv_to_cfc(mcom,2),
     &                      q_airndConv_to_cfcF(mcom),
     &                      q_airndConv_to_cfcP(mcom)

C Zone air convective source term (W)
      REAL q_airndConv_to_cfc

C Future value of q_conv_to_cfc (W)
      REAL q_airndConv_to_cfcF

C Presend value of q_conv_to_cfc (W)
      REAL q_airndConv_to_cfcP

C---------------------------------------------------------------------------------
C Common to save convective fluxes for outdoor blind case and
C convective coefficients and fluxes for indoor blind case.
C For outdoor/indoor blinds, the air cavity next to the blind is exposed 
C to outdoor/indoor air, hence convective heat transfer takes place between 
C the exposed inner surfaces and outdoor/indoor environment. 

      common/cfc_conv_sv/q_glass_extS(mcom,ms),
     &       h_blind_roomS(mcom,ms),h_glass_roomS(mcom,ms),
     &       q_blind_airndS(mcom,ms),q_glass_airndS(mcom,ms)

C OUTDOOR BLIND CASE

C Convective flux from exposed glass to external air
      real q_glass_extS

C INDOOR BLIND CASE

C Convective coefficient from the back surface of the blind (facing toward outdoors)
C to the zone air.
      real h_blind_roomS

C Convective coefficient from the interior glass surface behind the blind to the
C zone air. 
      real h_glass_roomS

C Convective flux from the back surface of the blind (facing toward outdoors) to the
C zone air
      real q_blind_airndS

C Convective flux from the interior glass surface behind the blind to the zone air. 
      real q_glass_airndS

C---------------------------------------------------------------------------------
C Common to hold exchange factors (FS - script F notation) for CFC intra-
C construction longwave radiation exchange.

      COMMON/CFC_FS/FSff(mcom,ms,me,me),
     &              FSfb(mcom,ms,me,me),
     &              FSbf(mcom,ms,me,me),
     &              FSbb(mcom,ms,me,me)

C Front surface (facing outdoors) to front surface (facing outdoors) exchange
C factor.
      REAL FSff

C Front surface (facing outdoors) to back surface (facing indoors) exchange
C factor.
      REAL FSfb

C Back surface (facing indoors) to front surface (facing outdoors) exchange
C factor.
      REAL FSbf

C Back surface (facing indoors) to back surface (facing indoors) exchange
C factor.
      REAL FSbb

C---------------------------------------------------------------------------------
C Common to hold exchange factors (FS - script F notation) for CFC
C longwave radiation exchange with external environment.

      COMMON/CFC_FSenv/FSfenv(mcom,ms,me),
     &                 FSbenv(mcom,ms,me)

C Front surface (facing outdoors) to environment -  exchange factor.
      REAL FSfenv

C Back surface (facing indoors) to environment -  exchange factor.
      REAL FSbenv

C---------------------------------------------------------------------------------
C Common to hold exchange factors (FS - script F notation) for CFC
C longwave radiation exchange with internal zone surfaces.

      COMMON/CFC_FSZn/FSfZn(mcom,ms,me,ms),
     &                FSbZn(mcom,ms,me,ms)

C Front surface (facing outdoors) to internal zone surface -  exchange factor.
      REAL FSfZn

C Back surface (facing indoors) to internal zone surface -  exchange factor.
      REAL FSbZn

C---------------------------------------------------------------------------------
C Common to save longwave radiant fluxes for outdoor and indoor blind cases.

      common/cfc_lwrad_sv/q_cfclw_toRoom(mcom,ms),
     &       q_cfclw_toExt(mcom,ms),Bld_fraction(mcom,ms),
     &       Sky_fraction(mcom,ms),Grd_fraction(mcom,ms)

C Longwave radiant flux from CFC layers (including any diathermanous layers) to
C zone surfaces
      real q_cfclw_toRoom

C Longwave radiant flux from CFC layers (including any diathermanous layers) to
C external surroundings.
      real q_cfclw_toExt

C Contribution of surrounding buildings to longwave flux 
      real Bld_fraction

C Contribution of sky to longwave flux
      real Sky_fraction

C Contribution of ground to longwave flux
      real Grd_fraction

C---------------------------------------------------------------------------------

C---------------------------------------------------------------------------------
C CFC SLAT CONTROL COMMONS
C
C These commons contain variables for the dynamic control of slat-type blinds:
C
C---------------------------------------------------------------------------------

      COMMON/CFCslatcontrl/vb_phiOld(mcom,mcfc),vb_phiNew(mcom,mcfc),
     &                     i_shd(mcom,mcfc),vb_phi_SV(mcom,mcfc),
     &                     i_shd_SV(mcom,mcfc)

C Slat angle for previous time-step (deg.)
      REAL vb_phiOld

C Slat angle for future time-step (deg.)
      REAL vb_phiNew

C Time-step shading ON/OFF flag
      INTEGER i_shd
      
C Slat angle for storing previous time step value (deg.)
      REAL vb_phi_SV
      
C For storing value of previous time step shade state
      INTEGER i_shd_SV
     
C---------------------------------------------------------------------------------
C CFC SOLAR AND VISIBLE OPTICAL PROPERTIES COMMONS
C
C These commons store SOLAR and VISIBLE properties of each layer in a CFC
C at each time step as a function of the sun's position.
C
C Each layer is described by 12 optical properties:
C
C     Beam-Beam: Front and Back Reflectance and Transmittance for specular glass
C                layers.
C     Beam-Diffuse: Front and Back Reflectance and Transmittance for scattering
C                   shade layers. (Set to 0 for specular layers)
C     Diffuse-Diffuse: Front and Back Reflectance and Transmittance for scattering
C                   and specular layers.
C---------------------------------------------------------------------------------
C Solar Beam-Beam properties
      COMMON/cfcBB/SolRFbb(mcom,ms,mcfc,me),
     &                SolRBbb(mcom,ms,mcfc,me),
     &                SolTFbb(mcom,ms,mcfc,me),
     &                SolTBbb(mcom,ms,mcfc,me)

C Front beam-beam reflectance
      REAL SolRFbb

C Back beam-beam reflectance
      REAL SolRBbb

C Front beam-beam transmittance
      REAL SolTFbb

C Back beam-beam transmittance
      REAL SolTBbb

C---------------------------------------------------------------------------------
C Solar Beam-Diffuse properties
      COMMON/cfcBD/SolRFbd(mcom,ms,mcfc,me),
     &                SolRBbd(mcom,ms,mcfc,me),
     &                SolTFbd(mcom,ms,mcfc,me),
     &                SolTBbd(mcom,ms,mcfc,me)

C Front beam-diffuse reflectance
      REAL SolRFbd

C Back beam-diffuse reflectance
      REAL SolRBbd

C Front beam-diffuse transmittance
      REAL SolTFbd

C Back beam-diffuse transmittance
      REAL SolTBbd

C---------------------------------------------------------------------------------
C Solar Diffuse-Diffuse properties
      COMMON/cfcDD/SolRFdd(mcom,ms,mcfc,me),
     &                SolRBdd(mcom,ms,mcfc,me),
     &                SolTFdd(mcom,ms,mcfc,me),
     &                SolTBdd(mcom,ms,mcfc,me)

C Front diffuse-diffuse reflectance
      REAL SolRFdd

C Back diffuse-diffuse reflectance
      REAL SolRBdd

C Front diffuse-diffuse transmittance
      REAL SolTFdd

C Back diffuse-diffuse transmittance
      REAL SolTBdd

C---------------------------------------------------------------------------------
C Solar Sky and Ground Diffuse-Diffuse properties
      COMMON/cfc_sky_ground_DD/SolRFskydd(mcom,ms,mcfc,me),
     &                SolRBskydd(mcom,ms,mcfc,me),
     &                SolTFskydd(mcom,ms,mcfc,me),
     &                SolTBskydd(mcom,ms,mcfc,me),
     &                SolRFgrddd(mcom,ms,mcfc,me),
     &                SolRBgrddd(mcom,ms,mcfc,me),
     &                SolTFgrddd(mcom,ms,mcfc,me),
     &                SolTBgrddd(mcom,ms,mcfc,me)

C Front diffuse-diffuse sky reflectance
      REAL SolRFskydd

C Back diffuse-diffuse sky reflectance
      REAL SolRBskydd

C Front diffuse-diffuse sky transmittance
      REAL SolTFskydd

C Back diffuse-diffuse sky transmittance
      REAL SolTBskydd

C Front diffuse-diffuse ground reflectance
      REAL SolRFgrddd

C Back diffuse-diffuse ground reflectance
      REAL SolRBgrddd

C Front diffuse-diffuse ground transmittance
      REAL SolTFgrddd

C Back diffuse-diffuse ground transmittance
      REAL SolTBgrddd

C---------------------------------------------------------------------------------
C Visible Beam-Beam properties
      COMMON/cfcBBvis/VisRFbb(mcom,ms,mcfc,me),
     &                VisRBbb(mcom,ms,mcfc,me),
     &                VisTFbb(mcom,ms,mcfc,me),
     &                VisTBbb(mcom,ms,mcfc,me)

C Front beam-beam reflectance
      REAL VisRFbb

C Back beam-beam reflectance
      REAL VisRBbb

C Front beam-beam transmittance
      REAL VisTFbb

C Back beam-beam transmittance
      REAL VisTBbb

C---------------------------------------------------------------------------------
C Visible Beam-Diffuse properties
      COMMON/cfcBDvis/VisRFbd(mcom,ms,mcfc,me),
     &                VisRBbd(mcom,ms,mcfc,me),
     &                VisTFbd(mcom,ms,mcfc,me),
     &                VisTBbd(mcom,ms,mcfc,me)

C Front beam-diffuse reflectance
      REAL VisRFbd

C Back beam-diffuse reflectance
      REAL VisRBbd

C Front beam-diffuse transmittance
      REAL VisTFbd

C Back beam-diffuse transmittance
      REAL VisTBbd

C---------------------------------------------------------------------------------
C Visible Diffuse-Diffuse properties
      COMMON/cfcDDvis/VisRFdd(mcom,ms,mcfc,me),
     &                VisRBdd(mcom,ms,mcfc,me),
     &                VisTFdd(mcom,ms,mcfc,me),
     &                VisTBdd(mcom,ms,mcfc,me)

C Front diffuse-diffuse reflectance
      REAL VisRFdd

C Back diffuse-diffuse reflectance
      REAL VisRBdd

C Front diffuse-diffuse transmittance
      REAL VisTFdd

C Back diffuse-diffuse transmittance
      REAL VisTBdd

C---------------------------------------------------------------------------------
