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
C Main Reference:
C     Lomanowski, B.A. (2008) 'Implementation of Window Shading Models
C     into Dynamic Whole-Building Simulation', MASc Thesis, University
C     of Waterloo.
C
C ********************************************************************

C it contains the following routines:
C
C      CFCoutputH3K (A. Geissler)

C ********************************************************************
C                         --CFC_output for h3k --
C
C H3K CFC output generation invoked in MZNUMA.
C 
C Generates time-step output to h3k if active
C
C ********************************************************************
      subroutine CFCoutputH3K(icomp)
      USE h3kmodule

      IMPLICIT NONE
#include "building.h"
#include "geometry.h"
#include "CFC_common.h"
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
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
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      real thrmli
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      real tfa,qfa
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      real TFC,QFC
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      real pazi,pelv
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      real hcip,hcif,hcop,hcof

      COMMON/PVANG/fInc_angle(MS)
      real fInc_angle

      real RGap1, RGap2, Rsi, Rse, SBOLTZ, T0
      real RGap1nom ! experimental, 15 K nominal RGap for DGU U-value ...
      real Gap1Width, hc_nom, TA, TB
      
      real solrad_dir       ! incident direct solar radiation [W/m2] - returned by sub. get_incident_solrad
      real solrad_diff      ! incident diffuse solar radiation [W/m2] - returned by sub. get_incident_solrad
      real ang              ! angle of incidence on surface "90" is eq. to "does not see sun"
      real psazi,pselv      ! solar azimuth and elevation angles relativ to surface

      integer i,j,k,l,nn,nc,ne,icfctp,icomp,ivblayr
      integer iH3K_NameLen(2)

c      CHARACTER*128 H3K_rep_NAME(2)
      CHARACTER*3 cConstrName,sIGU
      CHARACTER*3 cLayerName
      CHARACTER*2 cZone,cSurface
c      character*3 cBuildingName ! root name for "building"
c      character*1 cSep ! The separator

      logical TGU,DGU
      
      SBOLTZ=5.6697E-08 ! Stefan Boltzmann Constant
      T0=273.15         ! 0∞C in K

      TA=17.5
      TB=2.5   ! DT = 15 K for approx. U_nom

C Basic names (should be defined in a header, somewhere ...)
c      cBuildingName='bui'
c      cSep='/'

C.......Pad zone index to 'XX'
      if ( icomp .gt. 9 ) then
        write (cZone,'(I2)') icomp
      else
        write (cZone,'(A,I1)') '0', icomp
      endif

C Number of constructions in zone:
      nc=nconst(icomp)

      do 13 i=1,nc

        if (icfcfl(icomp,i).gt.0) then
          icfctp=icfcfl(icomp,i)
          ne=nelts(icomp,i)
          nn=nndc(icomp,i)
          DGU = .false.
          TGU = .false.
          RGap1 = 0.0
          RGap2 = 0.0
          Rsi   = 0.13
          Rse   = 0.04
          sIGU = 'XXX'

C.......Pad surface / construction index to 'XX'
          if ( i .gt. 9 ) then
            write (cSurface,'(I2)') i
          else
            write (cSurface,'(A,I1)') '0', i
          endif

C Output of CFC construction data ***********************************
C CFC venetian blind slat angle
          call AddToReport (
     &           rvCFCvbAngle%Identifier,
     &           vb_phiNew(icomp,icfctp),
     &           zname(icomp)(1:lnzname(icomp)),
     &           cSurface)

C CFC ON/OFF flag
          call AddToReport (
     &           rvCFCvbOnOff%Identifier,
     &           real(i_shd(icomp,icfctp)),
     &           zname(icomp)(1:lnzname(icomp)),
     &           cSurface)

C CFC Direct and diffuse radiation incidence
          call AddToReport (
     &           rvCFCRadTot%Identifier,
     &           RadIncDirS(icomp,i) + RadIncDifS(icomp,i),
     &           zname(icomp)(1:lnzname(icomp)),
     &           cSurface)

          call AddToReport (
     &           rvCFCRadDifSky%Identifier,
     &           RadIncDifSky(icomp,i),
     &           zname(icomp)(1:lnzname(icomp)),
     &           cSurface)

          call AddToReport (
     &           rvCFCRadDifGrd%Identifier,
     &           RadIncDifGrd(icomp,i),
     &           zname(icomp)(1:lnzname(icomp)),
     &           cSurface)


C CFC beam and diffuse transmission
          call AddToReport (
     &           rvCFCTransBB%Identifier,
     &           transBB_S(icomp,i),
     &           zname(icomp)(1:lnzname(icomp)),
     &           cSurface)

          call AddToReport (
     &           rvCFCTransD%Identifier,
     &           transD_S(icomp,i),
     &           zname(icomp)(1:lnzname(icomp)),
     &           cSurface)

C CFC beam and diffuse transmission for light
          call AddToReport (
     &           rvCFCTransBBvis%Identifier,
     &           transBB_V(icomp,i),
     &           zname(icomp)(1:lnzname(icomp)),
     &           cSurface)

          call AddToReport (
     &           rvCFCTransDvis%Identifier,
     &           transD_V(icomp,i),
     &           zname(icomp)(1:lnzname(icomp)),
     &           cSurface)


          do 15 l=1,ne
C Output of CFC layer data ****************************************** 
C Filter output by layer type ...
            if ( (icfcltp(icomp,icfctp,l).lt.2)
     &       .or.(icfcltp(icomp,icfctp,l).eq.2 .and. l.gt.1)) then

C.......Pad layer index to 'XX'
              if ( l .gt. 9 ) then
                write (cLayerName,'(A,I2)') 'l', l
              else
                write (cLayerName,'(A,I1)') 'l0', l
              endif

              if ( icfcltp(icomp,icfctp,l).eq.0 ) then
C CFC gap resistance if layer is gap (convective, only!)
                call AddToReport (
     &             rvCFCRgap%Identifier,
     &             cfcRgap(icomp,i,l,2),
     &             zname(icomp)(1:lnzname(icomp)),
     &             cSurface,
     &             cLayerName)
              endif

C Calculate U-value for IGU in CFC ...
C Layer type index  >>icfcltp<<  : 
C      0: gas gap
C      1: glazing
C      2: venetian blind
C The following logic assumes that there are not more than 3 consecutive 
C glass layers - interspersed with gas layers, of course - in the CFC ...
            if ( icfcltp(icomp,icfctp,l).eq.0 
     &           .and. rmlr_mass(icomp,icfctp,l).gt.0.2905E+02) then
              ! this is a non-air gas layer and no IGU has been identified, yet,
              if (icfcltp(icomp,icfctp,l-1) .eq. 1) then
                ! the previous layer is glass, 
                if ((l.le.(ne-1)) .and. 
     &              (icfcltp(icomp,icfctp,l+1) .eq. 1)) then
                  ! the next layer is also glass, so U-value can be / is calculated ...
                  if (DGU) then
                    DGU = .false.
                    TGU = .true. ! triple glazing unit
                    Rsi = 0.13 ! actual internal convective resistance
                    ! radiative part (heat transfer)
                    RGap2 = SBOLTZ*ABS((T0+tfc(icomp,i,(l*2)-1))**4.0
     &                               - (T0+tfc(icomp,i,(l*2)+1))**4.0)
                    RGap2 = RGap2/ABS(tfc(icomp,i,(l*2)-1)
     &                                 - tfc(icomp,i,(l*2)+1))
                    RGap2 = RGap2/(  1.0/rlwEB_sv(icomp,icfctp,l-1)
     &                             + 1.0/rlwEF_sv(icomp,icfctp,l+1)
     &                             - 1.0 )
                    ! overall gap resistance:
                    RGap2 = RGap2 + 1.0/cfcRgap(icomp,i,l,2)
                    RGap2 = 1.0/RGap2
                    ! add third glass layer resistance (lam = 1.0 W/(m2 K), assume 6 mm glass pane)
                    RGap2 = RGap2 + 0.006/1.0
                  else
                    DGU = .true.
                    Rse = 0.04 ! actual external convective resistance
                    Rsi = 0.13 ! actual internal convective resistance
                    ! radiative part (as heat transfer coefficient)
                    RGap1 = SBOLTZ*ABS((T0+tfc(icomp,i,(l*2)-1))**4.0
     &                               - (T0+tfc(icomp,i,(l*2)+1))**4.0)
                    RGap1 = RGap1/ABS(tfc(icomp,i,(l*2)-1)
     &                               - tfc(icomp,i,(l*2)+1))
                    RGap1 = RGap1/(  1.0/rlwEB_sv(icomp,icfctp,l-1)
     &                             + 1.0/rlwEF_sv(icomp,icfctp,l+1)
     &                             - 1.0 )
                    ! now for "nominal" U-value (experimental, 27.01.10)
                    RGap1nom = SBOLTZ*ABS((T0+TA)**4.0
     &                               - (T0+TB)**4.0)
                    RGap1nom = RGap1nom/ABS(TA-TB)
                    RGap1nom = RGap1nom/
     &                            (  1.0/rlwEB_sv(icomp,icfctp,l-1)
     &                             + 1.0/rlwEF_sv(icomp,icfctp,l+1)
     &                             - 1.0 )
                    ! overall gap resistance:
                    RGap1 = RGap1 + 1.0/cfcRgap(icomp,i,l,2)
                    RGap1 = 1.0/RGap1
C calculate nominal convective gap resistance (code from lines 1836 ff.)
                    Gap1Width=THRMLI(icomp,i,l,4) ! gap width
                    CALL ConvVertCav(PELV(icomp,i),Gap1Width,TA,TB,
     &                    rmlr_mass(icomp,icfctp,l),
     &                    cond_A(icomp,icfctp,l),cond_B(icomp,icfctp,l),
     &                    visc_A(icomp,icfctp,l),visc_B(icomp,icfctp,l),
     &                    spht_A(icomp,icfctp,l),spht_B(icomp,icfctp,l),
     &                    hc_nom)
C end nom. conv. gap res.
                    RGap1nom = RGap1nom + hc_nom
                    RGap1nom = 1.0/RGap1nom
                    ! add first and second glass layer resistances (for simplicity, assume 6 mm glass per pane, lam=1.0 W/(m2 K))
                    RGap1 = RGap1 + 0.012/1.0
                    RGap1nom = RGap1nom + 0.012/1.0
                  endif ! DGU or TGU
                endif ! following layer is glass
              endif ! previous layer is glass
            endif ! current layer is gas
C Debug ...
c          if (l .eq. ne) then
c           write(*,*)'Typ=',icfctp,' RGap1=',RGap1,' RGap2=',RGap2
c          endif

          end if ! filter layer type
 15     continue  ! loop through layers of construction

C Now calculate and output U-value for IGU of this construction
        if (DGU) then
          sIGU = 'DGU'
        else
          sIGU = 'TGU'
        endif
        CFC_ISO_Uvalue(icomp,icfctp)=1.0/(RGap1nom + RGap2 + 0.17)
        CFC_nom_Uvalue(icomp,icfctp)=1.0/(RGap1 + RGap2 + Rsi + Rse)

        call AddToReport (
     &         rvCFCUvalueISO%Identifier,
     &         CFC_ISO_Uvalue(icomp,icfctp),
     &         zname(icomp)(1:lnzname(icomp)),
     &         cSurface,
     &         sIGU)

        call AddToReport (
     &         rvCFCUvalueActual%Identifier,
     &         CFC_nom_Uvalue(icomp,icfctp),
     &         zname(icomp)(1:lnzname(icomp)),
     &         cSurface,
     &         sIGU)

      end if ! icfcfl .gt. 0
 13   continue ! loop through constructions of zone

      return
      end
