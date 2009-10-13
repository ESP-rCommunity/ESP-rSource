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

C This file contains subroutines related to the `adaptive conflation controller'
C which controls how CFD is used by the thermal domain.

C ADAPCONF - Main controlling routine for CFD adaptive conflation controller.
C CFDLENS - Determine length scales for calculating dimensionless groupings.
C GOPHER - Performs preliminary CFD simulation with MIT 0-eqn model.
C cfdDELT - Determine surface-air temp diff for calculating dimensionless groups.
C cfdRA - Calculates Rayleigh number for surface.
C cfdRE - Calculates Reynolds number for surface.
C THERPHYS - Set thermophysical properties for calc of dimensionless groups.
C MAXVEL - Determines velocity to use for calculating Reynolds number.
C VELMAG - Calculates magnitude of velocity for requested CFD cell.
C cfdRA2RE - Determines relative strength of natural- and forced-conv effects.
C setBUOY - Sets buoyancy and relaxation factors.
C setCONV - Sets convergence criteria.
C turnKEon - Activates k-epsilon turbulence model.
C setSHK1 - Sets handshaking and BC for 1-way adaptive conflation control.
C setSHK2 - Sets handshaking and BC for conditional 2-way adaptive conf ctl.
C HOWTURB - Determines "how turbulent" flow is adjacent to a surface.
C CHKSPC - Examines placement of next-to-wall grid points in terms of y+.
C resetHCs - Resets the HCIF array to empirical convection coefficients.
C AIRBCMAP - Maps idealized HVAC model's supply air temp and flow to CFD domain.

C ********************* ADAPCONF *********************
C ADAPCONF - Main controlling routine for `adaptive conflation controller'.

      SUBROUTINE ADAPCONF
#include "building.h"
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      common/CFDlen/rlengW,rlengE,rlengS,rlengN,rlengL,rlengH
      common/CFDdt/DTW,DTE,DTS,DTN,DTL,DTH
      common/CFDvel/VWmax,VEmax,VSmax,VNmax,VLmax,VHmax
      COMMON/ICFNOD/ICFD,ICP
      COMMON/ACCrec/IACC
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      CHARACTER*72 LCFD

C The CFD model possesses a suite of handshaking mechanisms, thermal boundary
C condition treatments, and turbulence and near-wall modelling approaches.
C As each approach has limited applicability (e.g. buoyancy-driven flow over
C vertical surfaces) this `adaptive handshaking algorithm' controls the
C simulation by dynamically selecting an appropriate approach based on an
C appraisal of the flow regime. In this manner the CFD modelling adapts
C with the flow regime.

C Currently two adaptive control schemes are supported:
C   1) one-way adaptive conflation control
C   2) conditional two-way adaptive conflation control

C Record ACC actions (begin).
      IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
        write(IACC,*)
        write(IACC,*) '***************************************'
        write(IACC,*) 'Adaptive conflation controller invoked.'
        write(IACC,*) 'on day ',IDYP,' at time ',BTIMEF
        write(IACC,*) '***************************************'
        write(IACC,*)
      ENDIF
C Record ACC actions (end).

C Map idealized HVAC model supply air temperature and velocity to
C CFD domain. This is a temporary measure for demonstrating the
C model's capabilities. This approach needs to be generalized in the future.
cc      CALL AIRBCMAP(ICP)

C Map radiator output predicted in thermal domain to heat source in CFD domain.
C This is a temporary measure for demonstrating the model's capabilities.
C This approach should be generalized in the future.
      CALL RADMAP(ICP)

C Invoke CFD with the MIT model to get a preliminary flow and temp field.
C This is called the `gopher' run.
      CALL GOPHER(ICP)

C Get surface-air temperature difference (i.e. driving potential for heat
C transfer) for each `physical' surface in the CFD domain.
      CALL cfdDELT(ICP)

C Set thermophysical properties of air (evaluated at zone air-point temp).
C See THERPHYS for variable definitions.
      TA = TFA(ICP)
      CALL THERPHYS(TA,GRAVITY,aBETA,RHO,HEATCAP,VISM,aCOND)

C Calculate Rayleigh number for each `physical' surface. Pass Ra-calculation
C routine the physical properties, length scale, and surface-air temperature
C difference. The length scale was calculated by CFDLENS at the time the
C .geo and .dfd files were read in.
      CALL cfdRA(GRAVITY,aBETA,RHO,HEATCAP,VISM,aCOND,DTW,rlengW,RAYLW)
      CALL cfdRA(GRAVITY,aBETA,RHO,HEATCAP,VISM,aCOND,DTE,rlengE,RAYLE)
      CALL cfdRA(GRAVITY,aBETA,RHO,HEATCAP,VISM,aCOND,DTS,rlengS,RAYLS)
      CALL cfdRA(GRAVITY,aBETA,RHO,HEATCAP,VISM,aCOND,DTN,rlengN,RAYLN)
      CALL cfdRA(GRAVITY,aBETA,RHO,HEATCAP,VISM,aCOND,DTL,rlengL,RAYLL)
      CALL cfdRA(GRAVITY,aBETA,RHO,HEATCAP,VISM,aCOND,DTH,rlengH,RAYLH)

C Determine velocity to use in calculating Reynolds number.
      CALL MAXVEL

C Calculate Reynolds number for each `physical' surface. Pass Re-calculation
C routine the physical properties, length scale, and velocity.
      CALL cfdRE(RHO,VISM,rlengW,VWmax,REYW)
      CALL cfdRE(RHO,VISM,rlengE,VEmax,REYE)
      CALL cfdRE(RHO,VISM,rlengS,VSmax,REYS)
      CALL cfdRE(RHO,VISM,rlengN,VNmax,REYN)
      CALL cfdRE(RHO,VISM,rlengL,VLmax,REYL)
      CALL cfdRE(RHO,VISM,rlengH,VHmax,REYH)

C Determine strengh of natural-convection and forced-convection effects
C at each `physical' surface.
      CALL cfdRA2RE(REYW,RAYLW,ForNW)
      CALL cfdRA2RE(REYE,RAYLE,ForNE)
      CALL cfdRA2RE(REYS,RAYLS,ForNS)
      CALL cfdRA2RE(REYN,RAYLN,ForNN)
      CALL cfdRA2RE(REYL,RAYLL,ForNL)
      CALL cfdRA2RE(REYH,RAYLH,ForNH)

C Turn on the k-epsilon model and initialize k and epsilon fields.
      CALL turnKEon

C If free convection effects are important at any of the surfaces, turn buoyancy
C on in the CFD calculations. Otherwise, turn buoyancy off. Adjust relaxation
C factors to suit buoyancy decision.
      ForNmax=AMAX1(ForNW,ForNE,ForNS,ForNN,ForNL,ForNH)
      CALL setBUOY(ICP,ForNmax)

C Set convergence criteria for final CFD run.
      CALL setCONV

C Choose an appropriate boundary condition for each physical surface.
C Surface numbers: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high. Treatment depends on which adaptive conflation
C controller is active.

      IF( abs(IFCFD(ICP)).eq.4 )THEN
C One-way adaptive conflation control.
        CALL setSHK1(1,ForNW)
        CALL setSHK1(2,ForNE)
        CALL setSHK1(3,ForNS)
        CALL setSHK1(4,ForNN)
        CALL setSHK1(5,ForNL)
        CALL setSHK1(6,ForNH)

      ELSEIF( abs(IFCFD(ICP)).eq.5 )THEN
C Conditional two-way adaptive conflation control. First recalculate convection
C coefficients using the adaptive convection algorithm's ICOR choice from the
C last time-step. These HC values will be used to set boundary conditions.
C This step ensures that empirical HCs (rather than the last time-step's
C CFD-calculated values) are used.
        CALL resetHCs
        CALL setSHK2(1,ForNW)
        CALL setSHK2(2,ForNE)
        CALL setSHK2(3,ForNS)
        CALL setSHK2(4,ForNN)
        CALL setSHK2(5,ForNL)
        CALL setSHK2(6,ForNH)

      ENDIF

C Preparation for final CFD run now complete.
      RETURN
      END


C ********************* CFDLENS *********************
C CFDLENS - Determine length scales to use in calculating non-dimen groupings.
C           Supporting routine for CFD `adaptive conflation controller'.

      SUBROUTINE CFDLENS
#include "cfd.h"
#include "building.h"
      COMMON/CONST/GREAT,small,GRAV
      COMMON/ICFNOD/ICFD,ICP
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      common/CFDlen/rlengW,rlengE,rlengS,rlengN,rlengL,rlengH


C This subroutine calculates length scales which are subsequently used to
C calculate non-dimensional groupings (e.g. Ra, Re) for building surfaces.
C It forms part of the `adaptive conflation controller' which is used to
C control how CFD is utilized by the building simulator. This subroutine is
C called once, at the time of reading the input data, when the geometrical
C data within the `G1' common block are still available. This subroutine
C operates on data contained in the .geo and .dfd files and sets the data
C for common/CFDlen.

C ESP-r geometrical variables used in this subroutine:
c   ISUR : surface number
c   NVER(ISUR) : number of vertices that define surface ISUR
c   X(IVER), Y(IVER), Z(IVER) : X, Y, Z coordinates of vertex IVER
c   JVN(ISUR,I) : vertex number of I'th vertex point in surface ISUR


C Initialize variables used to determine surface length scales.
      ZWmax = small
      ZWmin = GREAT
      ZEmax = small
      ZEmin = GREAT
      ZSmax = small
      ZSmin = GREAT
      ZNmax = small
      ZNmin = GREAT
      XLmax = small
      XLmin = GREAT
      YLmax = small
      YLmin = GREAT
      XHmax = small
      XHmin = GREAT
      YHmax = small
      YHmin = GREAT

C Examine each CFD surface in turn.
      DO 10 L=1,NSB(ICFD)

C Determine which ESP-r surface this CFD surface maps to.
        ISUR=ISUFLC(L,ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

C Determine max and min coordinates for each surface.
        IF(location.EQ.1)THEN
C---------On west face: find highest and lowest vertex points on face.
          do 100 I=1,NVER(ISUR)
            IVER = JVN(ISUR,I)
            ZWmax = AMAX1(ZWmax,Z(IVER))
            ZWmin = AMIN1(ZWmin,Z(IVER))
  100     continue

        ELSEIF(location.EQ.2)THEN
C---------On east face: find highest and lowest vertex points on face.
          do 101 I=1,NVER(ISUR)
            IVER = JVN(ISUR,I)
            ZEmax = AMAX1(ZEmax,Z(IVER))
            ZEmin = AMIN1(ZEmin,Z(IVER))
  101     continue

        ELSEIF(location.EQ.3)THEN
C---------On south face: find highest and lowest vertex points on face.
          do 102 I=1,NVER(ISUR)
            IVER = JVN(ISUR,I)
            ZSmax = AMAX1(ZSmax,Z(IVER))
            ZSmin = AMIN1(ZSmin,Z(IVER))
  102     continue

        ELSEIF(location.EQ.4)THEN
C---------On north face: find highest and lowest vertex points on face.
          do 103 I=1,NVER(ISUR)
            IVER = JVN(ISUR,I)
            ZNmax = AMAX1(ZNmax,Z(IVER))
            ZNmin = AMIN1(ZNmin,Z(IVER))
  103     continue

        ELSEIF(location.EQ.5)THEN
C---------On low face: find largest and smallest X and Y points on face.
          do 104 I=1,NVER(ISUR)
            IVER = JVN(ISUR,I)
            XLmax = AMAX1(XLmax,X(IVER))
            XLmin = AMIN1(XLmin,X(IVER))
            YLmax = AMAX1(YLmax,Y(IVER))
            YLmin = AMIN1(YLmin,Y(IVER))
  104     continue

        ELSEIF(location.EQ.6)THEN
C---------On high face: find largest and smallest X and Y points on face.
          do 105 I=1,NVER(ISUR)
            IVER = JVN(ISUR,I)
            XHmax = AMAX1(XHmax,X(IVER))
            XHmin = AMIN1(XHmin,X(IVER))
            YHmax = AMAX1(YHmax,Y(IVER))
            YHmin = AMIN1(YHmin,Y(IVER))
  105     continue

        ENDIF

   10 CONTINUE

C Determine length scales (= height) for vertical surfaces.
      rlengW = ZWmax - ZWmin
      rlengE = ZEmax - ZEmin
      rlengS = ZSmax - ZSmin
      rlengN = ZNmax - ZNmin

C Determine length scales (= surface area/perimeter) for horizontal surfaces.
C This assumes a rectangular shape: this will have to be modified when blockages
C are fully implemented.
      rlengL = (XLmax-XLmin)*(YLmax-YLmin) /
     &         ( 2.*(XLmax-XLmin) + 2.*(YLmax-YLmin) )
      rlengH = (XHmax-XHmin)*(YHmax-YHmin) /
     &         ( 2.*(XHmax-XHmin) + 2.*(YHmax-YHmin) )

      RETURN
      END


C ********************* GOPHER *********************
C GOPHER - Performs preliminary CFD simulation for current time-step using
C          the MIT 0-eqn model with coarse convergence parameters.

      SUBROUTINE GOPHER(ICOMP)
#include "cfd.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     1             IPPHI(MNZ),SRMAX(MNZ)
      COMMON/RENORM/LRENORM
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      common/fvisgo/gophrun

      COMMON/ACCrec/IACC
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL BUOYA,BOUSSA
      LOGICAL LRENORM
      LOGICAL gophrun
      CHARACTER*72 LCFD


C This subroutine performs a preliminary CFD simulation (known as the
C `gopher' run) to assess the flow field. The results of this simulation
C are used to evaluate dimensionless groupings in order to set up
C handshaking, boundary conditions, and turbulence options for the `final'
C CFD simulation.  This might be modified in future to reduce the gridding
C density to provide more rapid assessments.

C Turn off the k-epsilon model and fixed eddy viscosity model, and turn on
C the MIT zero-equation model.
      MITzero(ICFD) = .TRUE.
      KEMDL(ICFD) = .FALSE.
      ZandKE(ICFD) = .FALSE.
      ZEROT(ICFD) = .FALSE.

C Turn on buoyancy and use the Boussinesq assumption. Make sure that the
C energy equation is also solved. Set the reference temperature for
C buoyancy calcs to the air-point temperature calculated by the thermal domain
C the previous time-step.
      CALLT(ICFD) = .TRUE.
      BUOY(ICFD) = .TRUE.
      BOUSSI(ICFD) = .TRUE.
      TBAR = TFA(ICOMP)

C Set the handshaking flags and boundary condition flags to use the
C Chen and Xu zero-equation turbulence model. Employ one-way surface conflation.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
      DO 20 L=1,NSB(ICFD)
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10
        ITCtype(L,ICFD) = 2
        IWSB(L,ICFD) = location + 50
   20 CONTINUE

C Set convergence criteria for a coarse convergence. This is adequate to get
C an indication of the flow field. If the convergence criteria in the DFS
C file for this domain is greater than 0.02 then use it and if iterations
C less than 1000 set to 1000.

C Debug.
C      write(6,*) 'In GOPHER SRMAX and MAXITR were ',SRMAX(ICFD),
C     &  MAXITR(ICFD)

      if(MAXITR(ICFD).lt.1000)then
        MAXITR(ICFD) = 1000
      endif
      if(SRMAX(ICFD).lt.0.02)then
        SRMAX(ICFD) = 0.02
      endif

C Debug.
C      write(6,*) 'In GOPHER SRMAX and MAXITR are now ',SRMAX(ICFD),
C     &  MAXITR(ICFD)

      LRENORM = .FALSE.

C Set the relaxation factors at safe levels to ensure a converged solution.
C <<Use user specified values if lower. Cannot edit vars in common though>>
      URFCU(ICFD) = 0.1
      URFCV(ICFD) = 0.1
      URFCW(ICFD) = 0.1
      URFCP(ICFD) = 0.1
      URFCT(ICFD) = 0.5
      URFCVS(ICFD) = 0.5

C Record ACC actions (begin).
      IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
        write(IACC,*) 'Initiating preliminary CFD sim to assess flow.'
      ENDIF
      call edisp(IUOUT,'Initiating prelimanary CFD to assess flow.')

C Record ACC actions (end).
C Now do the gopher CFD simulation. 
      gophrun = .true.
      CALL CFMNGE(ICOMP,icfd)
      gophrun = .false.

C Gopher run complete.
      RETURN
      END


C ********************* cfdDELT *********************
C cfdDELT - Determine surface-air temperature differences to use in calculating
C           non-dimensional groupings. Supporting routine for CFD `adaptive
C           handshaking' algorithm.

      SUBROUTINE cfdDELT(ICOMP)
#include "cfd.h"
#include "building.h"
      COMMON/CONST/GREAT,small,GRAV
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      common/CFDdt/DTW,DTE,DTS,DTN,DTL,DTH


C This subroutine calculates the surface-air temperature difference
C (ie. driving potential for heat transfer) for each `physical' surface in
C the CFD domain. A `physical' surface may extend over numerous surfaces
C in the CFD and thermal models. For example, a boundary layer may develop
C over the full height of a south facing surface, but this could be
C represented by 3 surfaces in the thermal model and 10 surfaces in the
C CFD model. This implementation assumes a cuboidal CFD domain. Once blockages
C are fully implemented some changes will be required to this logic.

C This subroutine is called (once) each time step that CFD is active.
C It returns the temperature difference results through common/CFDdt. The
C `future' time-row values from the thermal simulation are used to establish
C the temperature differences. The returned variables are:
c   DTW : temp diff on west physical surface
c   DTE : temp diff on east physical surface
c   DTS : temp diff on south physical surface
c   DTN : temp diff on north physical surface
c   DTL : temp diff on low physical surface
c   DTH : temp diff on high physical surface

C Initialize variables used to determine max & min surface temperatures.
      TWmin = GREAT
      TWmax = -GREAT
      TEmin = GREAT
      TEmax = -GREAT
      TSmin = GREAT
      TSmax = -GREAT
      TNmin = GREAT
      TNmax = -GREAT
      TLmin = GREAT
      TLmax = -GREAT
      THmin = GREAT
      THmax = -GREAT

C Examine each CFD surface in turn.
      DO 10 L=1,NSB(ICFD)

C Determine which ESP-r surface this CFD surface maps to.
        ISUR=ISUFLC(L,ICFD)

C Determine upon which physical surface of the CFD domain the solid boundary
C resides. `location' is equal to the least significant digit of IWSB and has
C the following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

        IF(location.EQ.1)THEN
C---------On west physical surface:
C---------Find coldest and warmest temperature on this physical surface.
          TWmax = AMAX1(TWmax,TFS(ICOMP,ISUR))
          TWmin = AMIN1(TWmin,TFS(ICOMP,ISUR))

        ELSEIF(location.EQ.2)THEN
C---------On east physical surface:
C---------Find coldest and warmest temperature on this physical surface.
          TEmax = AMAX1(TEmax,TFS(ICOMP,ISUR))
          TEmin = AMIN1(TEmin,TFS(ICOMP,ISUR))

        ELSEIF(location.EQ.3)THEN
C---------On south physical surface:
C---------Find coldest and warmest temperature on this physical surface.
          TSmax = AMAX1(TSmax,TFS(ICOMP,ISUR))
          TSmin = AMIN1(TSmin,TFS(ICOMP,ISUR))

        ELSEIF(location.EQ.4)THEN
C---------On north physical surface:
C---------Find coldest and warmest temperature on this physical surface.
          TNmax = AMAX1(TNmax,TFS(ICOMP,ISUR))
          TNmin = AMIN1(TNmin,TFS(ICOMP,ISUR))

        ELSEIF(location.EQ.5)THEN
C---------On low physical surface:
C---------Find coldest and warmest temperature on this physical surface.
          TLmax = AMAX1(TLmax,TFS(ICOMP,ISUR))
          TLmin = AMIN1(TLmin,TFS(ICOMP,ISUR))

        ELSEIF(location.EQ.6)THEN
C---------On high physical surface:
C---------Find coldest and warmest temperature on this physical surface.
          THmax = AMAX1(THmax,TFS(ICOMP,ISUR))
          THmin = AMIN1(THmin,TFS(ICOMP,ISUR))

        ENDIF

   10 CONTINUE

C Determine surface-air temp differences for each physical surface.
      TA = TFA(ICOMP)
      DTW = AMAX1( ABS(TA-TWmax), ABS(TA-TWmin) )
      DTE = AMAX1( ABS(TA-TEmax), ABS(TA-TEmin) )
      DTS = AMAX1( ABS(TA-TSmax), ABS(TA-TSmin) )
      DTN = AMAX1( ABS(TA-TNmax), ABS(TA-TNmin) )
      DTL = AMAX1( ABS(TA-TLmax), ABS(TA-TLmin) )
      DTH = AMAX1( ABS(TA-THmax), ABS(TA-THmin) )

      RETURN
      END


C ********************* cfdRA *********************
C cfdRA - Calculates Rayleigh number for surface. Supporting routine for
C         CFD `adaptive conflation controller'.

      SUBROUTINE cfdRA(GRAVITY,aBETA,RHO,HEATCAP,VISM,aCOND,DT,rleng,RA)

C This subroutine is passed the values of the thermophysical variables,
C the length scale, and the surface-air temperature difference. It calculates
C and returns the Rayleigh number for the surface.

C Variable definitions:
c   GRAVITY : gravitational constant
c   aBETA : thermal expansion coefficient of air
c   RHO : density of air
c   HEATCAP : specific heat of the air
c   VISM : molecular viscosity of air
c   aCOND : thermal conductivity of air
c   DT : surface-air temperature difference
c   rleng : length scale for surface
c   RA : Rayleigh number

      RA = (GRAVITY*aBETA*DT*rleng**3.*RHO**2.*HEATCAP) / (VISM*aCOND)

      RETURN
      END


C ********************* cfdRE *********************
C cfdRE - Calculates Reynolds number for surface. Supporting routine for
C         CFD `adaptive conflation controller'.

      SUBROUTINE cfdRE(RHO,VISM,rleng,VELref,RE)

C This subroutine is passed the values of the thermophysical variables,
C the length scale, and the maximum velocity adjacent to the surface. It
C calculates and returns the Reynolds number for the surface.

C Variable definitions:
c   RHO : density of air
c   VISM : molecular viscosity of air
c   VELref : reference velocity adjacent to surface
c   rleng : length scale for surface
c   RE : Reynolds number

      RE = ( RHO*VELref*rleng ) / VISM

      RETURN
      END


C ********************* THERPHYS *********************
C THERPHYS - Set thermophysical properties for calculation of non-dimensional
C            groupings. Supporting routine for CFD `adaptive conflation controller'.
      SUBROUTINE THERPHYS(TA,GRAVITY,aBETA,RHO,HEATCAP,VISM,aCOND)
#include "cfd.h"
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/CONST/GREAT,small,GRAV

C This subroutine calculates the temperature-dependent physical properties
C of air. The temperature at which the properties are to be evalulated is
C passed to this subroutine in the calling statement. Additionally, this
C subroutine sets the temperature-independent air properties and other physical
C quantities to the appropriate variables

C Variable definitions:
c   GRAVITY : gravitational constant, local variable
c   GRAV : gravitational constant (set in INICNT)
c   aBETA : thermal expansion coefficient of air (calculated by function)
c   RHO : density of air (calculated by function)
c   HEATCAP : specific heat of the air, local variable
c   SH : specific heat of the air (set in INICNT)
c   VISM : molecular viscosity of air, local variable
c   VISCOS : molecular viscosity of air (set in INICNT)
c   PRANDL : Prandtl number (set in INICNT)
c   aCOND : thermal conductivity of air (calculated from other properties)

C Temperature-dependent air properties.
      aBETA = AIRBET(TA)
      RHO = AIRDEN(TA)
      aCOND = SH*VISCOS/PRANDL

C Temperature-independent air properties.
      VISM = VISCOS
      HEATCAP = SH

C Other constants.
      GRAVITY = GRAV

      RETURN
      END


C ********************* MAXVEL *********************
C MAXVEL - Determines velocity to use in calculating Reynolds number.
C          Supporting routine for CFD `adaptive conflation controller'.

      SUBROUTINE MAXVEL
#include "cfd.h"
      COMMON/CONST/GREAT,small,GRAV
      COMMON/ICFNOD/ICFD,ICP
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      common/CFDvel/VWmax,VEmax,VSmax,VNmax,VLmax,VHmax


C This subroutine determines, for each surface in the CFD domain, the appropriate
C velocity to use for calculating the Reynolds number.

C For now, all three velocity components are considered in determining the
C magnitude of the velocity vectors next to each physical (e.g. south) surface.

C Initialize variables used to determine max velocity adjacent to each surface.
      VWmax = -GREAT
      VEmax = -GREAT
      VSmax = -GREAT
      VNmax = -GREAT
      VLmax = -GREAT
      VHmax = -GREAT

C Examine each CFD surface in turn.
      DO 10 L=1,NSB(ICFD)

C Determine upon which physical surface of the CFD domain the solid boundary
C resides. `location' is equal to the least significant digit of IWSB and has
C the following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

        IF(location.EQ.1)THEN
C---------On west physical surface. Examine each CFD cell adjacent to CFD surface:
C---------determine magnitude of cell velocity; assign cell velocity to VWmax if
C---------greater than the largest cell velocity encountered to date.
          I=ISBi(L,ICFD)
          DO 100 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 101 K=KSBi(L,ICFD),KSBf(L,ICFD)
              CALL VELMAG(I,J,K,Vmean)
              VWmax = AMAX1(VWmax,Vmean)
  101       CONTINUE
  100     CONTINUE

        ELSEIF(location.EQ.2)THEN
C---------On east physical surface. See annotations for west surface.
          I=ISBi(L,ICFD)
          DO 110 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 111 K=KSBi(L,ICFD),KSBf(L,ICFD)
              CALL VELMAG(I,J,K,Vmean)
              VEmax = AMAX1(VEmax,Vmean)
  111       CONTINUE
  110     CONTINUE

        ELSEIF(location.EQ.3)THEN
C---------On south physical surface. See annotations for west surface.
          J=JSBi(L,ICFD)
          DO 120 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 121 K=KSBi(L,ICFD),KSBf(L,ICFD)
              CALL VELMAG(I,J,K,Vmean)
              VSmax = AMAX1(VSmax,Vmean)
  121       CONTINUE
  120     CONTINUE

        ELSEIF(location.EQ.4)THEN
C---------On north physical surface. See annotations for west surface.
          J=JSBi(L,ICFD)
          DO 130 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 131 K=KSBi(L,ICFD),KSBf(L,ICFD)
              CALL VELMAG(I,J,K,Vmean)
              VNmax = AMAX1(VNmax,Vmean)
  131       CONTINUE
  130     CONTINUE

        ELSEIF(location.EQ.5)THEN
C---------On low physical surface. See annotations for west surface.
          K=KSBi(L,ICFD)
          DO 140 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 141 J=JSBi(L,ICFD),JSBf(L,ICFD)
              CALL VELMAG(I,J,K,Vmean)
              VLmax = AMAX1(VLmax,Vmean)
  141       CONTINUE
  140     CONTINUE

        ELSEIF(location.EQ.6)THEN
C---------On high physical surface. See annotations for west surface.
C---------On low physical surface. See annotations for west surface.
          K=KSBi(L,ICFD)
          DO 150 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 151 J=JSBi(L,ICFD),JSBf(L,ICFD)
              CALL VELMAG(I,J,K,Vmean)
              VHmax = AMAX1(VHmax,Vmean)
  151       CONTINUE
  150     CONTINUE

        ENDIF

   10 CONTINUE

      RETURN
      END


C ********************* VELMAG *********************
C VELMAG - Calculates magnitude of velocity for requested CFD cell.
C          Supporting routine for CFD `adaptive conflation controller'.

      SUBROUTINE VELMAG(I,J,K,Vmean)
#include "cfd.h"
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)

C Calculate velocity at `P' point. Interpolation required due to staggered grid.
C `P' point located mid-distance between the `U', `V', and `W' points, so
C velocity components at `P' point calculated by averaging velocities at
C neighbouring points. Then get magnitude of velocity at `P' using three
C components.
      UatP = 0.5*Uf(i,j,k) + 0.5*Uf(i+1,j,k)
      VatP = 0.5*Vf(i,j,k) + 0.5*Vf(i,j+1,k)
      WatP = 0.5*Wf(i,j,k) + 0.5*Wf(i,j,k+1)
      Vmean = SQRT(UatP**2 + VatP**2 + WatP**2)

      RETURN
      END


C ********************* cfdRA2RE *********************
C cfdRA2RE - Determines relative strength of natural-convection and
C            forced-convection effects for given surface.
C            Supporting routine for CFD `adaptive conflation controller'.

      SUBROUTINE cfdRA2RE(RE,RA,FORorNAT)
#include "cfd.h"
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN

C This subroutine is passed the Reynolds number and the Rayleigh number for a
C given surface. It uses these non-dimensional groupings to determine the
C relative strength natural-convection and forced-convection effects for the
C surface and returns an indicator. The result is used to determine how to
C apply CFD and how to treat the near-wall region adjacent to the surface.

C Variable definitions:
c   RE : Reynolds number
c   RA : Rayleigh number
c   PRANDL : Prandtl number (set in INICNT)
c   FORorNAT : indicator of relative importance of natural-convection to
c              forced-convection effects. When FORorNAT>>1 then forced convection
c              effects can be neglected; when FORorNAT<<1 then free convection
c              effects can be neglected; when FORorNAT is about 1, then both
c              effects must be considered.

      FORorNAT = ( RA/PRANDL ) / RE**2.

      RETURN
      END


C ********************* setBUOY *********************
C setBUOY - Sets the buoyancy and relaxation factors according to results
C           appraised from `gopher' simulation. Supporting routine for
C           CFD `adaptive conflation controller'.

      SUBROUTINE setBUOY(ICOMP,ForNmax)
#include "cfd.h"
#include "building.h"
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ)
      COMMON/ICFNOD/ICFD,ICP

      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL BUOYA,BOUSSA

C Make sure energy equation is solved.
      CALLT(ICFD) = .TRUE.

C Check whether free convection effects are significant for at least one surface.
      IF( ForNmax .gt. 0.1)THEN

C Buoyancy is significant. Therefore turn buoyancy on. Reduce relaxation
C factors to encourage convergence. Set reference temperature for buoyancy
C term to previous time-step's air-point temperature.
        BUOY(ICFD) = .TRUE.
        BOUSSI(ICFD) = .TRUE.
        TBAR = TFA(ICOMP)
        URFCU(ICFD) = 0.05
        URFCV(ICFD) = 0.05
        URFCW(ICFD) = 0.05
        URFCP(ICFD) = 0.05
        URFCT(ICFD) = 0.25
        URFCK(ICFD) = 0.05
        URFCE(ICFD) = 0.05
        URFCVS(ICFD) = 0.25
      ELSE

C Buoyancy is not significant. Therefore turn buoyancy off and set more
C aggressive relaxation factors.
        BUOY(ICFD) = .FALSE.
        BOUSSI(ICFD) = .FALSE.
        URFCU(ICFD) = 0.5
        URFCV(ICFD) = 0.5
        URFCW(ICFD) = 0.5
        URFCP(ICFD) = 0.7
        URFCT(ICFD) = 1.0
        URFCK(ICFD) = 1.0
        URFCE(ICFD) = 1.0
        URFCVS(ICFD) = 1.0

      ENDIF

C Buoyancy adjustments completed.
      RETURN
      END


C ********************* setCONV *********************
C setCONV - Sets convergence criteria for final CFD run. Drive final CFD run
C           to greater convergence than gopher run. Supporting routine for
C           CFD `adaptive conflation controller'.

      SUBROUTINE setCONV
#include "cfd.h"
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     1             IPPHI(MNZ),SRMAX(MNZ)
      COMMON/RENORM/LRENORM
      COMMON/ICFNOD/ICFD,ICP

      LOGICAL LRENORM

C In future alternate convergence criteria could be devised (e.g. tracking
C surface convection estimates and halting simulation when changes between
C iterations drop below a threshold) and set as a function of the type of
C adaptive conflation control in operation. For now, just set the maximum
C residuals and number of iterations to drive the solution to a well-converged
C state unless the user has defined a loose criteria in the DFS file.

C << why doesnt it just pay attention to what is in the DFS file? >>
      if(MAXITR(ICFD).lt.2000)then
        MAXITR(ICFD) = 2000
      endif

C Relax convergence a bit for IEA-hut case.
c      SRMAX(ICFD) = 0.001
      if(SRMAX(ICFD).lt.0.005)then
        SRMAX(ICFD) = 0.005
      elseif(SRMAX(ICFD).gt.0.02)then
        continue
      else
        SRMAX(ICFD) = 0.02
      endif

C Debug.
C      write(6,*) 'In setConv SRMAX is now ',SRMAX(ICFD)

      LRENORM = .FALSE.

      RETURN
      END


C ********************* turnKEon *********************
C turnKEon - Turns on k-epsilon turbulence model and initializes k and epsilon
C            fields based on preliminary CFD run.

      SUBROUTINE turnKEon
#include "cfd.h"
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     1            TURBIN,ALAMDA,PRTE,PRED
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)

      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero

C Turn on k-epsilon turbulence model.
      KEMDL(ICFD) = .TRUE.

C Make sure Chen and Xu zero-equation model and fixed eddy viscosity
C model are switched off
      MITzero(ICFD) = .FALSE.
      ZandKE(ICFD) = .FALSE.
      ZEROT(ICFD) = .FALSE.

C Velocity and temperature solution from preliminary CFD run using Chen & Xu model
C will be used to initialize variables for final k-e run. However, k and epsilon
C not solved with Chen and Xu model, so initialize them now.
C Set epsilon to an arbitrary value, then calculate k from k-epsilon's standard
C eddy viscosity relation.
      epsilon = 0.001
      DO 200 I=1,NI
        DO 201 J=1,NJ
          DO 202 K=1,NK
            EDf(i,j,k) = epsilon
            TEf(i,j,k) = ( (VIS(i,j,k)*EDf(i,j,k)) /
     &                     (CMU*DENf(i,j,k))         )**0.5
  202     CONTINUE
  201   CONTINUE
  200 CONTINUE

      RETURN
      END


C ********************* setSHK1 *********************
C setSHK1 - Sets a thermal-CFD handshaking mechanism and boundary
C           condition treatment to employ for the final CFD simulation.
C           Decision based on an appraisal of the results from the `gopher'
C           CFD simulation. Used for one-way adaptive conflation control.

      SUBROUTINE setSHK1(isetsurf,FORorNAT)

#include "cfd.h"
#include "building.h"
      COMMON/ICFNOD/ICFD,ICP
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/ACCrec/IACC
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      CHARACTER*72 LCFD

C Record ACC actions (begin).
      write(IACC,*)
      write(IACC,*) 'One-way ACC algorithm invoked.'
      
C Record ACC actions (end).
C Examine each CFD surface in turn.
      DO 10 L=1,NSB(ICFD)

C Determine upon which physical surface of the CFD domain the solid boundary
C resides. `location' is equal to the least significant digit of IWSB and has
C the following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

C This subroutine was called to set the handshaking mechanism for the
C `isetsurf' physical surface. Does this CFD surface correspond to the
C `isetsurf' physical surface? If so, then set the handshaking mechanism.
        IF(location.eq.isetsurf) THEN

C Determine nature of flow at surface.
          if(FORorNAT .ge. 10.)then

C Flow is dominated by buoyancy effects ==> Use one-way surface conflation
C with the Yuan wall functions on vertical surfaces and one-way Neumann
C (with Tair from thermal domain) on horizontal surfaces.
            if(isetsurf.ge.1 .and. isetsurf.le.4)then
              ITCtype(L,ICFD) = 3
              IWSB(L,ICFD) = location + 60
            else
              ITCtype(L,ICFD) = 4
              IWSB(L,ICFD) = location + 20
              icTREF(L,ICFD) = 1
            endif
C Record ACC actions (begin).
            IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
              write(IACC,*) '  CFD surf ',L,' on physical surf ',
     &                isetsurf,' : FORorNAT = ',FORorNAT,' ==>  buoyant'
            ENDIF
C Record ACC actions (end).

          elseif(FORorNAT .le. 0.1)then

C Buoyancy effects are negligible ==> Use one-way surface conflation
C with cooperative Neumann boundary condition.
            ITCtype(L,ICFD) = 5
            IWSB(L,ICFD) = location + 20
            icTREF(L,ICFD) = 2
C Record ACC actions (begin).
            IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
              write(IACC,*) '  CFD surf ',L,' on physical surf ',
     &                    isetsurf,' : FORorNAT = ',FORorNAT,
     &                    ' ==>  forced'
            ENDIF
C Record ACC actions (end).

          else

C Both buoyant and forced effects are important ==> Use one-way conflation
C with the cooperative Robin boundary condition.
            ITCtype(L,ICFD) = 8
            IWSB(L,ICFD) = location + 40
C Record ACC actions (begin).
            IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
              write(IACC,*) '  CFD surf ',L,' on physical surf ',
     &             isetsurf,' : FORorNAT = ',FORorNAT,' ==>  mixed flow'
            ENDIF
C Record ACC actions (end).

          endif

        ENDIF
   10 CONTINUE

C All CFD solid boundaries corresponding to physical surface under consideration
C have been set.
      RETURN
      END


C ********************* setSHK2 *********************
C setSHK2 - Sets a thermal-CFD handshaking mechanism and boundary
C           condition treatment to employ for the final CFD simulation.
C           Decision based on an appraisal of the results from the `gopher'
C           CFD simulation. Used for conditional two-way adaptive conflation
C           control.

      SUBROUTINE setSHK2(isetsurf,FORorNAT)

#include "cfd.h"
#include "building.h"
      COMMON/ICFNOD/ICFD,ICP
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      common/howTB/turbin(6)
      COMMON/ACCrec/IACC
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      CHARACTER*72 LCFD

C Record ACC actions (begin).
      write(IACC,*)
      write(IACC,*) 'Conditional two-way ACC algorithm invoked.'
C Record ACC actions (end).
C Examine each CFD surface in turn.
      DO 10 L=1,NSB(ICFD)

C Determine upon which physical surface of the CFD domain the solid boundary
C resides. `location' is equal to the least significant digit of IWSB and has
C the following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

C This subroutine was called to set the handshaking mechanism for the
C `isetsurf' physical surface. Does this CFD surface correspond to the
C `isetsurf' physical surface? If so, then set the handshaking mechanism.
        IF(location.eq.isetsurf) THEN

C Determine nature of flow at surface.
          if(FORorNAT .ge. 10.)then

C Flow is dominated by buoyancy effects ==> Use two-way surface conflation
C with the Yuan wall functions at vertical surfaces and the Neumann condition
C (air temp from thermal domain) at horizontal surfaces.
            if(isetsurf.ge.1 .and. isetsurf.le.4)then
              ITCtype(L,ICFD) = 11
              IWSB(L,ICFD) = location + 60
            else
              ITCtype(L,ICFD) = 4
              IWSB(L,ICFD) = location + 20
              icTREF(L,ICFD) = 1
            endif
C Record ACC actions (begin).
            IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
              write(IACC,*) 'CFD surf ',L,' on physical surf ',isetsurf,
     &                    ' : FORorNAT = ',FORorNAT,' ==>  buoyant'
            ENDIF
C Record ACC actions (end).

          elseif(FORorNAT .le. 0.1)then

C Buoyancy effects are negligible ==> Examine the eddy viscosity distribution
C next to the physical surface to determine "how turbulent" flow is.
            CALL HOWTURB
            if( turbin(isetsurf).ge.30. )then
C Flow next to wall is turbulent so there is a chance k-eps can calculate the
C surface convection ==> Use two-way Dirichlet condition and allow log-law
C wall functions to calculate surface convection.
              ITCtype(L,ICFD) = 9
              IWSB(L,ICFD) = location
            else
C Flow next to wall is weakly turbulent so k-eps has no chance of resolving
C surface convection ==> Use one-way surface conflation with cooperative
C Neumann boundary condition.
              ITCtype(L,ICFD) = 5
              IWSB(L,ICFD) = location + 20
              icTREF(L,ICFD) = 2
            endif
C Record ACC actions (begin).
            IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
              write(IACC,*) 'CFD surf ',L,' on physical surf ',isetsurf,
     &                    ' : FORorNAT = ',FORorNAT,' ==>  forced'
            ENDIF
C Record ACC actions (end).

          else

C Both buoyant and forced effects are important ==> Use two-way conflation
C with the cooperative Robin boundary condition.
            ITCtype(L,ICFD) = 12
            IWSB(L,ICFD) = location + 40
C Record ACC actions (begin).
            IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
              write(IACC,*) 'CFD surf ',L,' on physical surf ',isetsurf,
     &                    ' : FORorNAT = ',FORorNAT,' ==>  mixed flow'
            ENDIF
C Record ACC actions (end).

          endif

        ENDIF
   10 CONTINUE

C All CFD solid boundaries corresponding to physical surface under consideration
C have been set.
      RETURN
      END


C ********************* HOWTURB *********************
C HOWTURB - Determines "how turbulent" flow is adjacent to a surface by examining
C           the eddy viscosity predictions from the preliminary CFD run.
C           Supporting routine for CFD `adaptive conflation controller'.

      SUBROUTINE HOWTURB
#include "cfd.h"
      COMMON/ICFNOD/ICFD,ICP
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM2/VolP(ntcelx,ntcely,ntcelz),
     &             VolU(ntcelx,ntcely,ntcelz),
     &             VolV(ntcelx,ntcely,ntcelz),
     &             VolW(ntcelx,ntcely,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      common/howTB/turbin(6)

C This routine examines the eddy viscosity distribution in the cells adjacent
C to each physical surface of the CFD domain. It determines the average ratio
C of the eddy viscosity to the molecular viscosity, an indicator of "how turbulent"
C a flow is. It saves these ratios (six, one for each physical surface) to a common
C block for subsequent use by the adaptive conflation controller.

C Initialize variables used to calculate averages.
        rmassW=0.
        totalW=0.
        rmassE=0.
        totalE=0.
        rmassS=0.
        totalS=0.
        rmassN=0.
        totalN=0.
        rmassL=0.
        totalL=0.
        rmassH=0.
        totalH=0.

C Examine each CFD surface in turn.
      DO 10 L=1,NSB(ICFD)

C Determine upon which physical surface of the CFD domain the solid boundary
C resides. `location' is equal to the least significant digit of IWSB and has
C the following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

        IF(location.EQ.1)THEN
C On west physical surface. Examine each CFD cell adjacent to CFD surface.
C Determine the ratio of eddy viscosity to molecular viscosity. Add to running
C totals for the west physical surface. Weight the ratio by the mass of the air
C in the cell.
          I=ISBi(L,ICFD)
          DO 100 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 101 K=KSBi(L,ICFD),KSBf(L,ICFD)
              rmassW = rmassW+DENf(I,J,K)*VolP(I,J,K)
              ratiomus = (VIS(I,J,K)-VISCOS) / VISCOS
              totalW = totalW+ratiomus*DENf(I,J,K)*VolP(I,J,K)
  101       CONTINUE
  100     CONTINUE

        ELSEIF(location.EQ.2)THEN
C On east physical surface. See annotations for west surface.
          I=ISBi(L,ICFD)
          DO 110 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 111 K=KSBi(L,ICFD),KSBf(L,ICFD)
              rmassE = rmassE+DENf(I,J,K)*VolP(I,J,K)
              ratiomus = (VIS(I,J,K)-VISCOS) / VISCOS
              totalE = totalE+ratiomus*DENf(I,J,K)*VolP(I,J,K)
  111       CONTINUE
  110     CONTINUE

        ELSEIF(location.EQ.3)THEN
C On south physical surface. See annotations for west surface.
          J=JSBi(L,ICFD)
          DO 120 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 121 K=KSBi(L,ICFD),KSBf(L,ICFD)
              rmassS = rmassS+DENf(I,J,K)*VolP(I,J,K)
              ratiomus = (VIS(I,J,K)-VISCOS) / VISCOS
              totalS = totalS+ratiomus*DENf(I,J,K)*VolP(I,J,K)
  121       CONTINUE
  120     CONTINUE

        ELSEIF(location.EQ.4)THEN
C On north physical surface. See annotations for west surface.
          J=JSBi(L,ICFD)
          DO 130 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 131 K=KSBi(L,ICFD),KSBf(L,ICFD)
              rmassN = rmassN+DENf(I,J,K)*VolP(I,J,K)
              ratiomus = (VIS(I,J,K)-VISCOS) / VISCOS
              totalN = totalN+ratiomus*DENf(I,J,K)*VolP(I,J,K)
  131       CONTINUE
  130     CONTINUE

        ELSEIF(location.EQ.5)THEN
C On low physical surface. See annotations for west surface.
          K=KSBi(L,ICFD)
          DO 140 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 141 J=JSBi(L,ICFD),JSBf(L,ICFD)
              rmassL = rmassL+DENf(I,J,K)*VolP(I,J,K)
              ratiomus = (VIS(I,J,K)-VISCOS) / VISCOS
              totalL = totalL+ratiomus*DENf(I,J,K)*VolP(I,J,K)
  141       CONTINUE
  140     CONTINUE

        ELSEIF(location.EQ.6)THEN
C On high physical surface. See annotations for west surface.
          K=KSBi(L,ICFD)
          DO 150 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 151 J=JSBi(L,ICFD),JSBf(L,ICFD)
              rmassH = rmassH+DENf(I,J,K)*VolP(I,J,K)
              ratiomus = (VIS(I,J,K)-VISCOS) / VISCOS
              totalH = totalH+ratiomus*DENf(I,J,K)*VolP(I,J,K)
  151       CONTINUE
  150     CONTINUE

        ENDIF
   10 CONTINUE

C All CFD surfaces have been examined. Now calculate average ratios over
C each physical surface. Save results to common block variable then return
C to calling routine.
      turbin(1) = totalW/rmassW
      turbin(2) = totalE/rmassE
      turbin(3) = totalS/rmassS
      turbin(4) = totalN/rmassN
      turbin(5) = totalL/rmassL
      turbin(6) = totalH/rmassH

      RETURN
      END


C ********************* CHKSPC *********************
C CHKSPC - Examines spacing to next-to-wall grid points in terms of dimensionless
C          distance (y+). Operates on solution from final CFD simulation. These
C          results are used by ASSESS to decide whether to use the CFD conv
C          coefficient when conditional two-way adaptive conflation is active.
C          Supporting routine for CFD `adaptive conflation controller'.

      SUBROUTINE CHKSPC
#include "cfd.h"
#include "building.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM2/VolP(ntcelx,ntcely,ntcelz),
     &             VolU(ntcelx,ntcely,ntcelz),
     &             VolV(ntcelx,ntcely,ntcelz),
     &             VolW(ntcelx,ntcely,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/closehow/yplus(ntcelx,ntcely,ntcelz)
      COMMON/ypave/ypavg(MCOM)

      real rmass(MCOM),total(MCOM)

C This routine calculates the average y+ value of the next-to-wall grid points
C for each surface in the thermal model. It saves these values to a common
C block for subsequent use by the adaptive conflation controller.

C Initialize variables used to calculate averages. One set for each thermal surf.
      NSUR = NCONST(ICP)
      DO 5 ISUR=1,NSUR
        rmass(ISUR)=0.
        total(ISUR)=0.
    5 CONTINUE

C Examine each thermal surface in turn. Get y+ values for each CFD surface
C that corresponds to the thermal surface.
      DO 20 ISUR=1,NSUR
        DO 10 L=1,NSB(ICFD)
          IS=ISUFLC(L,ICFD)
          IF(IS.eq.ISUR) THEN

C CFD surface lies on the thermal surface.
C Determine upon which physical surface of the CFD domain the solid boundary
C resides. `location' is equal to the least significant digit of IWSB and has
C the following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
            location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10
            IF(location.EQ.1)THEN
            
C On west physical surface. Examine each CFD cell adjacent to CFD surface.
C Determine y+ and add to running total for the thermal surface. Weight by
C the mass of the air in the cell.
              I=ISBi(L,ICFD)
              DO 100 J=JSBi(L,ICFD),JSBf(L,ICFD)
                DO 101 K=KSBi(L,ICFD),KSBf(L,ICFD)
                  rmass(ISUR) = rmass(ISUR)+DENf(I,J,K)*VolP(I,J,K)
                  total(ISUR) = 
     &                 total(ISUR)+yplus(I,J,K)*DENf(I,J,K)*VolP(I,J,K)
  101           CONTINUE
  100         CONTINUE
            ELSEIF(location.EQ.2)THEN
            
C On east physical surface. See annotations for west surface.
              I=ISBi(L,ICFD)
              DO 110 J=JSBi(L,ICFD),JSBf(L,ICFD)
                DO 111 K=KSBi(L,ICFD),KSBf(L,ICFD)
                  rmass(ISUR) = rmass(ISUR)+DENf(I,J,K)*VolP(I,J,K)
                  total(ISUR) = 
     &                 total(ISUR)+yplus(I,J,K)*DENf(I,J,K)*VolP(I,J,K)
  111           CONTINUE
  110         CONTINUE
            ELSEIF(location.EQ.3)THEN
            
C On south physical surface. See annotations for west surface.
              J=JSBi(L,ICFD)
              DO 120 I=ISBi(L,ICFD),ISBf(L,ICFD)
                DO 121 K=KSBi(L,ICFD),KSBf(L,ICFD)
                  rmass(ISUR) = rmass(ISUR)+DENf(I,J,K)*VolP(I,J,K)
                  total(ISUR) = 
     &                 total(ISUR)+yplus(I,J,K)*DENf(I,J,K)*VolP(I,J,K)
  121           CONTINUE
  120         CONTINUE
            ELSEIF(location.EQ.4)THEN
            
C On north physical surface. See annotations for west surface.
              J=JSBi(L,ICFD)
              DO 130 I=ISBi(L,ICFD),ISBf(L,ICFD)
                DO 131 K=KSBi(L,ICFD),KSBf(L,ICFD)
                  rmass(ISUR) = rmass(ISUR)+DENf(I,J,K)*VolP(I,J,K)
                  total(ISUR) = 
     &                 total(ISUR)+yplus(I,J,K)*DENf(I,J,K)*VolP(I,J,K)
  131           CONTINUE
  130         CONTINUE
            ELSEIF(location.EQ.5)THEN
            
C On low physical surface. See annotations for west surface.
              K=KSBi(L,ICFD)
              DO 140 I=ISBi(L,ICFD),ISBf(L,ICFD)
                DO 141 J=JSBi(L,ICFD),JSBf(L,ICFD)
                  rmass(ISUR) = rmass(ISUR)+DENf(I,J,K)*VolP(I,J,K)
                  total(ISUR) = 
     &                 total(ISUR)+yplus(I,J,K)*DENf(I,J,K)*VolP(I,J,K)
  141           CONTINUE
  140         CONTINUE
            ELSEIF(location.EQ.6)THEN
            
C On high physical surface. See annotations for west surface.
              K=KSBi(L,ICFD)
              DO 150 I=ISBi(L,ICFD),ISBf(L,ICFD)
                DO 151 J=JSBi(L,ICFD),JSBf(L,ICFD)
                  rmass(ISUR) = rmass(ISUR)+DENf(I,J,K)*VolP(I,J,K)
                  total(ISUR) = 
     &                 total(ISUR)+yplus(I,J,K)*DENf(I,J,K)*VolP(I,J,K)
  151           CONTINUE
  150         CONTINUE

C All physical surfaces examined.
            ENDIF

C Examine next CFD surface.
          ENDIF
   10   CONTINUE

C Examine next thermal surface.
   20 CONTINUE


C All thermal surfaces have been examined. Now calculate average y+ value for
C each thermal surface. Save results to common block variable then return
C to calling routine.
      DO 30 ISUR=1,NSUR
        ypavg(ISUR) = total(ISUR)/rmass(ISUR)
   30 CONTINUE

      RETURN
      END


C ********************* resetHCs *********************
C resetHCs - Resets the HCIF array to empirical convection coefficients.
C            This is used when there is conditional two-way adaptive conflation
C            control. Supporting routine for CFD `adaptive conflation controller'.

      SUBROUTINE resetHCs
#include "building.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      common/CORIND/ICORI(MCOM,MS),ICORE(MCOM,MS)
      COMMON/PREC1I/APRAT(MCOM,MS),HEIGHT(MCOM,MS)
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)

C This routine is used to reset the HCIF array, the array that holds the
C current convection coefficients. This is used only when there is conditional
C two-way adaptive conflation control. This action is necessary because HC
C values may be called upon by some of the boundary conditions set by the
C controller. The HCIF array may be holding CFD-calculated HC values from the
C previous time-step. Therefore, this routine replaces these will empirical
C values calculated with the adaptive convection algorithm's choice of ICOR
C from the previous time-step.

C Iterate through each multi-layer construction.
      NC=NCONST(ICP)
      DO 101 ISUR=1,NC

C Calculate the HTC for `inside' surface of the mlc using empirical correlations.
        TA=TFA(ICP)
        TS=TFS(ICP,ISUR)
        DT=ABS(TA-TS)
        ICOR = ICORI(ICP,ISUR)
        HGT = HEIGHT(ICP,ISUR)
        ARHY = APRAT(ICP,ISUR)
        CALL HTBUOY(HC,ICOR,ICP,ISUR,DT,HGT,ARHY)
        HCIF(ICP,ISUR)=HC
  101 CONTINUE

      RETURN
      END


C ********************* AIRBCMAP *********************
C AIRBCMAP - Maps idealized HVAC model's predicted supply air temperature
C            and flow rate to air flow inlet in CFD domain. This is a prototype
C            application that needs to be generalized in the future. It should
C            be integrated more closely with the network air flow conflation
C            mechanism.

      SUBROUTINE AIRBCMAP(ICOMP)
#include "building.h"
#include "cfd.h"
      COMMON/ICFNOD/ICFD,ICP
      COMMON/Fisher/Vdot(MCOM),SAT(MCOM),ADJREF(MCOM,MS),DELTAR(MCOM)
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),ICNACT(MCFND,MNZ)
      COMMON/ACCrec/IACC
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      CHARACTER*72 LCFD

C Set CFD opening to be manipulated.
      Iopen = 1

C Set air flow at CFD opening to supply air flow calculated by idealized
C HVAC model. Vdot from idealized HVAC model expressed in m3/s. FIXM is
C CFD boundary condition expressed in kg/s. Treat air density as a constant
C for now.
      rhoair = 1.2
      FIXM(Iopen,ICFD) = Vdot(ICOMP)*rhoair

C Set temperature at CFD opening to supply air temperature calculated by
C idealized HVAC model.
      FIXT(Iopen,ICFD) = SAT(ICOMP)

C Record ACC actions (begin).
      write(IACC,*)
      write(IACC,*) '  Resetting BCs at CFD air flow openings:'
      write(IACC,*) '    --> FIXM = ',FIXM(Iopen,ICFD),
     &              '    --> FIXT = ',FIXT(Iopen,ICFD)
C Record ACC actions (end).

C Return to calling routine.
      RETURN
      END


C ********************* RADMAP *********************
C RADMAP - Maps radiator output to heat source in CFD domain. Used for IEA
C          test hut demo in thesis. This is a prototype application that needs
C          to be generalized in the future.

      SUBROUTINE RADMAP(ICOMP)
#include "building.h"
#include "cfd.h"
      COMMON/ICFNOD/ICFD,ICP
      COMMON/ACCrec/IACC
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/HSrc/NHS(2,MNZ),IHSi(MNHS,2,MNZ),IHSf(MNHS,2,MNZ),
     &            JHSi(MNHS,2,MNZ),JHSf(MNHS,2,MNZ),KHSi(MNHS,2,MNZ),
     &            KHSf(MNHS,2,MNZ),HSID(MNHS,2,MNZ),SHS(MNHS,2,MNZ),
     &            BHS(MNHS,2,MNZ)
      COMMON/SETUQ/QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)

      CHARACTER*72 LCFD,HSID
      logical BHS

C Set CFD heat source to be manipulated.
      Ihs = 1

C Set plant injection calculated by thermal domain to the heat source in
C the CFD domain. Note that plant injection node is a mix of
C radiant/convective flux (eg radiator) in this case.
      SHS(Ihs,2,ICFD) = (QPLTP(ICOMP)+QPLTF(ICOMP))*.5

C Record ACC actions (begin).
      write(IACC,*)
      write(IACC,*) '  Resetting heat source at radiator:'
      write(IACC,*) '    --> SHS = ',SHS(Ihs,2,ICFD)
C Record ACC actions (end).

C Return to calling routine.
      RETURN
      END
