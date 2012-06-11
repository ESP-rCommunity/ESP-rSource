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
C construction (CFC). The file is a copy of esrubld/complex_fenestration.F
C as quick work-around for testing integration of CFCs in e2r.
C
C ********************************************************************

C it contains the following routines:
C
C      qlwCFC
C      FSsolve
C      cfc_eff_opt_prop
C      groundskydiffcalc
C      solar_multilayer
C      ConvVertCav
C      vb_eff_diff_properties
C      CFC_time_dependent_Rgap
C      SOLMATS
C      VB_SOL46_CURVE
C      VB_SOL46_THICK
C      VB_SOL_DD_THICK
C      VB_SOL4
C      VB_SOL6
C      SETCoef
C      TDMAsol
C      profile_angle
C      set_HR_to_zero
C      set_QELW_to_zero
C      CFCoutput

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
     &NPGAP(MCOM,MS,MGP)
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
      real epsf(ME)                   !front emissivity
      real epsb(ME)                   !back emissivity
      real tnd(MN)                    !nodal temp [K]

C lw heat transfer between pairs of surfaces
      real q(ME,ME)   !heat tsfer between CFC layers [W]
      real qZnsur(ME,MS)   !heat tsfer rate between CFC and zone surfaces [W]
      real qZnsurTot(MS)                   !total heat tsfer rate for internal zone surface due to exchange with CFC [W]
      real qlwtot(ME)                 !total heat tsfer rate for CFC layer [W]
      real qenv(ME)                   !heat tsfer rate between CFC layers and environment[W]
      real sigma                                      !stefan-boltzman constant [W/m^2 K^4]

C infinite plates radiant heat transfer coefficient for TESTING
C      real hr_inf_plates                              ![W/m2 K] 

      !logical VBxst     !true if slat-type blind present in CFC type

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
  2         continue
            do 3 j=1,nc
                  qZnsur(i,j)=0.
  3         continue
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
C            if(icfcltp(icomp,icfctp,i).ge.2)then
C                  VBxst=.true.
C            else
C                  VBxst=.false.
C            end if
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
!      &   (VBxst.and.vb_phiNew(icomp,icfctp)
!      &   .ne.vb_phiOld(icomp,icfctp)))then
!       write(93,*)
!       write(93,*)'SLAT ANGLE IS CHANGING, CALLING FSSOLVE'


C count number of CFC surfaces that exchange LW radiation,skipping airgaps
         do 10 i=1,ncfc_el(icomp,icfctp)
            if(icfcltp(icomp,icfctp,i).gt.0)then
            icount=icount+1
            else
            !air gap, do nothing
            end if
  10     continue

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
     &            nMatSize,nExtPart,nCFCPart)

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
C            if(i.lt.nn)then
            tnd(i)=tfc(icomp,isurf,i)+273.15
C            else
C            tsrf=tfs(icomp,isurf)+273.15
C            end if
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
     &  (epsf(j)*FSff(icomp,isurf,j,k)*epsf(k)+
     &  epsb(j)*FSbf(icomp,isurf,j,k)*epsf(k)+
     &  epsf(j)*FSfb(icomp,isurf,j,k)*epsb(k)+
     &  epsb(j)*FSbb(icomp,isurf,j,k)*epsb(k))*
     &  sigma*((tnd(j*2)**4)-(tnd(k*2)**4))*
     &  (-1.0*SNA(icomp,isurf))


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
  100       continue
            do 110 l=j,1,-2
                  if(l.ne.j)then
                        qlwtot(j)=qlwtot(j)-q(l,j)
                  end if
  110       continue
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
!           qZnsur(j,k)=
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
     &    (epsf(j)*FSfZn(icomp,isurf,j,k)*EI(icomp,k)+
     &    epsb(j)*FSbZn(icomp,isurf,j,k)*EI(icomp,k))*
     &    (SNA(icomp,k)*(-1.0))*sigma*
     &    ((tnd(j*2)**4)-((tfs(icomp,k)+273.15)**4))


C DETERMINE RADIANT HEAT TRANSFER COEFF. FOR INDICES OF MERIT 
C REPORTING.(CURRENTLY NOT USED)
          hr_cfc_to_sur(icomp,isurf,j,k)=
     &    (epsf(j)*FSfZn(icomp,isurf,j,k)*EI(icomp,k)+
     &    epsb(j)*FSbZn(icomp,isurf,j,k)*EI(icomp,k))*
     &    (SNA(icomp,k)/SNA(icomp,isurf))*sigma*
     &    ((tnd(j*2)**2)+((tfs(icomp,k)+273.15)**2))*
     &    (tnd(j*2)+(tfs(icomp,k)+273.15))

         else 
          qZnsur(j,k)=0.
          hr_cfc_to_sur(icomp,isurf,j,k)=0.
         end if

 130     continue
 120   continue

C Initialize output data
       q_cfclw_toRoom(icomp,isurf)=0.0

C Add longwave exchanges with interior zone surfaces to CFC layer
C source terms
       do 140 j=1,nCFCPart,2
        do 150 k=1,nc
         if(k.ne.isurf)then !omit interior blind surface
          qlwtot(j)=qlwtot(j)+qZnsur(j,k)
          q_cfclw_toRoom(icomp,isurf)=
     &    q_cfclw_toRoom(icomp,isurf)+(qZnsur(j,k)/SNA(icomp,isurf)) !save for output and divide by area to get [W/m2}
         end if
 150    continue
 140   continue

C Add longwave exchanges with CFC layers to interior zone surface
C node. 
       do 160 i=1,nc
         do 170 j=1,nCFCPart,2
          if(i.ne.isurf)then
                qZnsurTot(i)=qZnsurTot(i)-qZnsur(j,i)
          else
                qZnsurTot(i)=0.
          end if
 170     continue
 160   continue

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
     &                                  qlwtot(i)/(fnnod*2.)
            do 210 j=2,nnod
            inode=inode+1
            qcfc_lw(icomp,isurf,inode,2)=qcfc_lw(icomp,isurf,inode,2)+
     &                                  qlwtot(i)/fnnod
  210       continue

            inode=inode+1
            qcfc_lw(icomp,isurf,inode,2)=qcfc_lw(icomp,isurf,inode,2)+
     &                                  qlwtot(i)/(fnnod*2.)
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
C then assign source terms to all nodes in the inside layer.
C Otherwise assign heat injection to only the inside surface node
C for non CFC surfaces.
        icfctp=icfcfl(icomp,i)

        IF(icfctp.gt.0)then
         if(icfcltp(icomp,icfctp,ne).ge.2)then
          qcfc_lw(icomp,i,inode,2)=qcfc_lw(icomp,i,inode,2)+
     &                            qZnsurTot(i)/(fnnod*2.)
          do 230 j=nn-1,nn-nnod+1,-1
            inode=inode-1
            qcfc_lw(icomp,i,inode,2)=qcfc_lw(icomp,i,inode,2)+
     &                              qZnsurTot(i)/fnnod
  230     continue
          inode=inode-1
          qcfc_lw(icomp,i,inode,2)=qcfc_lw(icomp,i,inode,2)+
     &                            qZnsurTot(i)/(fnnod*2.)
         else
          qcfc_lw(icomp,i,inode,2)=qcfc_lw(icomp,i,inode,2)+
     &                            qZnsurTot(i)
         endif
        ELSE
          qcfc_lw(icomp,i,inode,2)=qcfc_lw(icomp,i,inode,2)+
     &                            qZnsurTot(i)
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
     &NPGAP(MCOM,MS,MGP)
      integer nconst,nelts,ngaps,npgap
      COMMON/V2/CFB(MCOM,MST,MST)
      real cfb
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      real ei,ee,ai,ae

C misc local variables
      integer icomp,isurf,icfctp,nc,i,j,indx
      integer iDiaIn,iDiaOut,irow,icol,k,indxj,ij,jk
      integer N                            !matrix size
      integer nExtPart,nCFCPart			   !matrix partition sizes
      real B(N,N+2),tmpB(N,N+2)            !radiosity matrix
      real JsolB(N)                        !solution matrix

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
  20        continue
  10  continue


C populate CFC block of 'A' matrix with longwave radiative properties of 
C CFC layers each non-gas gap layer has two radiating surfaces, front 
C (facing outdoors),back (facing indoors)
      do 30 i=1,nCFCPart,2 

            indx=nExtPart+i   !matrix index, account for external partition (ie.if nExtPart>0)

C reflectances and trasmittances
            if(i.eq.1)then
C include LW exchange with environment if iDiaOut=1
                  if(iDiaOut.eq.1)then
                        B(indx,indx-1)=-(1.-rlwEF(icomp,icfctp,i)-
     &                                 rlwT(icomp,icfctp,i))
                        B(indx+1,indx-1)=-rlwT(icomp,icfctp,i)
                  endif
                  B(indx,indx+2)=-rlwT(icomp,icfctp,i)
                  B(indx+1,indx+2)=-(1.-rlwEB(icomp,icfctp,i)-
     &                             rlwT(icomp,icfctp,i))
            elseif(i.eq.nCFCPart-1)then
                  B(indx,indx-1)=-(1.-rlwEF(icomp,icfctp,i)-
     &                           rlwT(icomp,icfctp,i))
                  B(indx+1,indx-1)=-rlwT(icomp,icfctp,i)
            else
                  B(indx,indx-1)=-(1.-rlwEF(icomp,icfctp,i)-
     &                           rlwT(icomp,icfctp,i))
                  B(indx,indx+2)=-rlwT(icomp,icfctp,i)
                  B(indx+1,indx-1)=-rlwT(icomp,icfctp,i)
                  B(indx+1,indx+2)=-(1.-rlwEB(icomp,icfctp,i)-
     &                             rlwT(icomp,icfctp,i))
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
     &                                   rlwT(icomp,icfctp,irow-2))
     &                                   *CFB(icomp,isurf,j)*(-1.)
                          B(irow-2,icol)=rlwT(icomp,icfctp,irow-2)
     &                                   *CFB(icomp,isurf,j)*(-1.)
                        end if

                        if(i.ne.isurf)then

C The following does not fully account for the transparency of another CFC
C indoor shade in the zone.
C Assume reflectance of the 'other' blind is 1-EI. Otherwise lw exchange 
C between both CFC indoor layers is necessary.
                          if(j.eq.1)then
                            B(irow,icol-1)=(1.-EI(icomp,i))
     &                      *CFB(icomp,i,isurf)*(-1.)
                          end if
                          if(irow.ne.icol)then
                            B(irow,icol)=(1.-EI(icomp,i))
     &                      *CFB(icomp,i,j)*(-1.) !fill in bottom block
                          end if
                        end if
                END IF
  51  continue
  41  continue
      end if

C MATRIX SET-UP COMPLETE. NOW TURN ON EACH SURFACE AND DETERMINE SCRIPT F 
C EXCHANGE FACTORS

C Solve for exchange factors
      DO 60 i=1,nCFCPart

            indx=nExtPart+i   !matrix index, account for external partition (ie.if nExtPart>0)

C copy 'B' matrix to 'tmpB' matrix - use tmpB for solving and retain B 
C matrix for next iteration
            do 701 j=1,N
                  do 801 k=1,N+1
                        tmpB(j,k)=B(j,k)
  801             continue
  701       continue

C TURN ON SOURCE TERM
            tmpB(indx,N+1)=1.
            if(i.ne.1)then
                  tmpB(indx-1,N+1)=0.
            end if

C solve matrix with only one surface 'turned on'
            call SOLMATS(N,tmpB,JsolB)

 
C Assign exchange factors based on radiosity solution for 'turned on'
C surface to all other surfaces in enclosure.
C
C Each non gas-gap layer in CFC has two surfaces, front and back, 
C layers are indexed 1,3,5,7.., skipping air gaps which are indexed 2,4,6,8...
C
C Need to distinguish between front and back surfaces.
            IF(mod(i,2).ne.0)then   !source is front surface

C Calculate exchange factor for intra-constructional CFC layers.
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
     &                 FSfb(icomp,isurf,i,j)+
     &                 CFB(icomp,isurf,jk)*JsolB(nExtPart+nCFCPart+jk)
                      end if
 115                  continue
                    else
                      FSfb(icomp,isurf,i,j)=0.
                    end if
                  else
                    FSff(icomp,isurf,i,j)=JsolB(indxj-1)
                    FSfb(icomp,isurf,i,j)=JsolB(indxj+2)
                  end if
100           continue

C Calculate exchange factor from each CFC layer to external
C surroundings.
              if(iDiaOut.eq.1)then
                 FSfenv(icomp,isurf,i)=JsolB(2) !radiosity of outside surface of CFC
              end if

C Calculate exchange factor from each CFC layer to each internal
C zone surface.
              if(iDiaIn.eq.1)then
                  do 111 ij=1,nc
                    FSfZn(icomp,isurf,i,ij)=0.   !initialize
                      if(ij.ne.isurf)then
                        do 112 jk=1,nc
                          if(jk.ne.isurf)then
                            FSfZn(icomp,isurf,i,ij)=
     &                      FSfZn(icomp,isurf,i,ij)+
     &                      CFB(icomp,ij,jk)
     &                      *JsolB(nExtPart+nCFCPart+jk)
                          end if
 112                    continue
                        FSfZn(icomp,isurf,i,ij)=
     &                  FSfZn(icomp,isurf,i,ij)+
     &                  CFB(icomp,ij,isurf)*JsolB(nExtPart+nCFCPart)
                      end if
 111              continue
              end if

            ELSEIF(mod(i,2).eq.0)then  !source is back surface
                
C Calculate exchange factor for intra-constructional CFC layers.
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
     &                      FSbb(icomp,isurf,i-1,j+1)+
     &                      CFB(icomp,isurf,jk)
     &                      *JsolB(nExtPart+nCFCPart+jk)
                          end if
 116                    continue
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
 110          continue

C Calculate exchange factor from each CFC layer to external
C surroundings.
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
     &                  FSbZn(icomp,isurf,i-1,ij)+
     &                  CFB(icomp,ij,jk)*JsolB(nExtPart+nCFCPart+jk)
                      end if
 114                continue
                    FSbZn(icomp,isurf,i-1,ij)=
     &              FSbZn(icomp,isurf,i-1,ij)+
     &              CFB(icomp,ij,isurf)*JsolB(nExtPart+nCFCPart)
                  end if
 113            continue
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
C Lomanowski, B.A. (2008) 'Implementation of Window Shading Models
C into Dynamic Whole-Building Simulation', MASc Thesis, University
C of Waterloo.
C
C Slat-type blind layers (CFC layer type 2) are dependent on profile
C angle. Effective slat-blind solar optical properties are determined
C for both curved slats and flat slats with thickness correction. 
C Refer to Chapter 4 in B.Lomanowski MASc Thesis (2008) for model
C details. 
C
C Visible properties enabled & necessary extensions inserted. A.G., 25.09.2009
C
C isolvis    - Switch for solar (0) or visual (1) data
C
C ********************************************************************
      subroutine cfc_eff_opt_prop(icomp,isurf,icfctp,anginc,
     &                             proang,isolvis)
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

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      real PAZI,PELV

      integer icomp,isurf,icfctp,isolvis
      real proang, anginc, tilt, SolVisT
      real pi,r
      real w,s,phi,crown,wr,t !local slat width, spacing, crown, w/r ratio, thickness
      real sl_rad_overw
      logical Curved
      
C local copies of parameters that should'nt be changed (!!!)
      real proang_rad,anginc_rad

C variables for calculation of sky and ground diffuse transmittance
      real TXskydiff       ! front sky diffuse transmittance (slat blinds with horizontal slats)
      real RXskydiff       ! front sky diffuse reflectance (slat blinds with horizontal slats)
      real TXgrddiff       ! front ground diffuse transmittance (slat blinds with horizontal slats)
      real RXgrddiff       ! front ground diffuse reflectance (slat blinds with horizontal slats)

      integer j ,jj        ! iterators

C temporary INPUT variables to hold solar and visible properties as
C read from *.cfc file
      real Rufs,Rdfs,Tslat
C temporary OUTPUT variables to hold effective solar and visible properties
      real OutRFbd,OutTFbb,OutTFbd,OutRBbd,OutTBbb,OutTBbd
      real OutRFdd,OutRBdd,OutTFdd,OutTBdd
C clear glass Fresnel calcs
      real N2,KL,THETA1,THETA2,TAU_A,RPERP,RPARL
      real TAUPERP,TAUPARL,RHOPERP,RHOPARL
      real RclearON,RclearDN,TclearON,TclearDN 

      CHARACTER outs*124

C misc local variables
      integer ne,i,itype

      pi = 4.0 * ATAN(1.0)
      r=pi/180.
      ne=ncfc_el(icomp,icfctp)

C convert profile and incidence angles to radians
      proang_rad=proang*r
      anginc_rad=anginc*r

C loop through CFC layer types and compute solar optical properties for 
C each layer, set air gap properties to 0 except for bb and dd transmission,
C set these to 1.
      do 100 i=1,ne
       itype=icfcltp(icomp,icfctp,i)

       if(itype.eq.1)then

C Establish extinction coefficient (KL) and refractive index (N2).
C IF normal solar/visual transmittance >0.645, use clear reference glass
C IF normal solar/visual transmittance <= 0.645, use bronze reference glass
        if (isolvis.eq.0) then ! solar
          SolVisT = SolT(icomp,icfctp,i)
        else ! visual
          SolVisT = VisT(icomp,icfctp,i)
        endif

        if(SolVisT.gt.0.645)then
            N2 = 1.526
            KL = 25.7*THRMLI(icomp,isurf,i,4) ![thickness in m]
        else
            N2 = 1.7
            KL = 100.6*THRMLI(icomp,isurf,i,4) ![thickness in m]
        end if

C Calculate beam DIRECT NORMAL incidence transmittance and reflectance
C for reference glass based on extinction coefficient and refractive
C index.
        TAU_A = EXP(-1.*KL)
        RPERP=((N2-1.)/(N2+1.))**2.
        TclearDN=TAU_A*(1.-RPERP)*(1.-RPERP)
     &      /(1.-(RPERP*RPERP*TAU_A*TAU_A))
        RclearDN=RPERP*(1.+(TAU_A*TclearDN))

        THETA1=ABS(anginc_rad)
        IF (THETA1.LT.0.1)THEN
          TclearON=TclearDN
          RclearON=RclearDN
        ELSE
C Calculate beam OFF-NORMAL incidence transmittance and reflectance for
C reference glass based on extinction coefficient and refractive index 
C using Snell's law, Fresnel equations.
          THETA2 = ASIN((SIN(THETA1))/N2)
          TAU_A = EXP(-1.0*KL/COS(THETA2))
          RPERP = ((SIN(THETA2-THETA1))/(SIN(THETA2+THETA1)))**2.
          RPARL = ((TAN(THETA2-THETA1))/(TAN(THETA2+THETA1)))**2.
          TAUPERP = (TAU_A*(1.0-RPERP)*(1.0-RPERP))/
     &            (1.0-(RPERP*RPERP*TAU_A*TAU_A))
          TAUPARL = (TAU_A*(1.0-RPARL)*(1.0-RPARL))/
     &            (1.0-(RPARL*RPARL*TAU_A*TAU_A))
          RHOPERP = RPERP*(1.0+(TAU_A*TAUPERP))
          RHOPARL = RPARL*(1.0+(TAU_A*TAUPARL))
          TclearON = (TAUPERP+TAUPARL)/2.0
          RclearON = (RHOPERP+RHOPARL)/2.0
        END IF

C Calculate glazing layer beam OFF-NORMAL front and back transmittance
C and reflectance based on clear glass off-normal and normal values.
        if (isolvis.eq.0) then ! solar
          SolTFbb(icomp,isurf,icfctp,i)=
     &      (TclearON/TclearDN)*SolT(icomp,icfctp,i)

          SolTBbb(icomp,isurf,icfctp,i)=SolTFbb(icomp,isurf,icfctp,i)

          SolRFbb(icomp,isurf,icfctp,i)=1.-
     &      (((1.-RclearON)/(1.-RclearDN))*(1.-SolRF(icomp,icfctp,i)))

          SolRBbb(icomp,isurf,icfctp,i)=1.-
     &      (((1.-RclearON)/(1.-RclearDN))*(1.-SolRB(icomp,icfctp,i)))

        else ! visual data
C Calculate glazing layer beam OFF-NORMAL front and back visual transmittance
C and reflectance based on clear glass off-normal and normal values.
          VisTFbb(icomp,isurf,icfctp,i)=
     &      (TclearON/TclearDN)*VisT(icomp,icfctp,i)

          VisTBbb(icomp,isurf,icfctp,i)=VisTFbb(icomp,isurf,icfctp,i)

          VisRFbb(icomp,isurf,icfctp,i)=1.-
     &      (((1.-RclearON)/(1.-RclearDN))*(1.-VisRF(icomp,icfctp,i)))

          VisRBbb(icomp,isurf,icfctp,i)=1.-
     &      (((1.-RclearON)/(1.-RclearDN))*(1.-VisRB(icomp,icfctp,i)))
        endif


C Calculate DIFFUSE-DIFFUSE incidence transmittance and reflectance
C for glass template based on extinction coefficient and refractive
C index.
        THETA1 = 51.*r
        THETA2 = ASIN((SIN(THETA1))/N2)
        TAU_A = EXP(-1.0*KL/COS(THETA2))
        RPERP = ((SIN(THETA2-THETA1))/(SIN(THETA2+THETA1)))**2.
        RPARL = ((TAN(THETA2-THETA1))/(TAN(THETA2+THETA1)))**2.
        TAUPERP = (TAU_A*(1.0-RPERP)*(1.0-RPERP))/
     &            (1.0-(RPERP*RPERP*TAU_A*TAU_A))
        TAUPARL = (TAU_A*(1.0-RPARL)*(1.0-RPARL))/
     &            (1.0-(RPARL*RPARL*TAU_A*TAU_A))
        RHOPERP = RPERP*(1.0+(TAU_A*TAUPERP))
        RHOPARL = RPARL*(1.0+(TAU_A*TAUPARL))
        TclearON = (TAUPERP+TAUPARL)/2.0
        RclearON = (RHOPERP+RHOPARL)/2.0

C calculate diffuse-diffuse for given glass layer
        if (isolvis.eq.0) then ! solar
          SolTFdd(icomp,isurf,icfctp,i)=
     &      (TclearON/TclearDN)*SolT(icomp,icfctp,i)

          SolTBdd(icomp,isurf,icfctp,i)=SolTFdd(icomp,isurf,icfctp,i)

          SolRFdd(icomp,isurf,icfctp,i)=1.-
     &     (((1.-RclearON)/(1.-RclearDN))*(1.-SolRF(icomp,icfctp,i)))

          SolRBdd(icomp,isurf,icfctp,i)=1.-
     &     (((1.-RclearON)/(1.-RclearDN))*(1.-SolRB(icomp,icfctp,i)))

        else ! visual data
C calculate visual diffuse-diffuse for given glass layer
          VisTFdd(icomp,isurf,icfctp,i)=
     &      (TclearON/TclearDN)*VisT(icomp,icfctp,i)

          VisTBdd(icomp,isurf,icfctp,i)=VisTFdd(icomp,isurf,icfctp,i)

          VisRFdd(icomp,isurf,icfctp,i)=1.-
     &     (((1.-RclearON)/(1.-RclearDN))*(1.-VisRF(icomp,icfctp,i)))

          VisRBdd(icomp,isurf,icfctp,i)=1.-
     &     (((1.-RclearON)/(1.-RclearDN))*(1.-VisRB(icomp,icfctp,i)))
        endif

C set beam-diffuse eq. 0
        if (isolvis.eq.0) then ! solar
          SolTFbd(icomp,isurf,icfctp,i)=0.0
          SolTBbd(icomp,isurf,icfctp,i)=0.0
          SolRFbd(icomp,isurf,icfctp,i)=0.0
          SolRBbd(icomp,isurf,icfctp,i)=0.0
        else
          VisTFbd(icomp,isurf,icfctp,i)=0.0
          VisTBbd(icomp,isurf,icfctp,i)=0.0
          VisRFbd(icomp,isurf,icfctp,i)=0.0
          VisRBbd(icomp,isurf,icfctp,i)=0.0
        endif

       elseif(itype.eq.2)then   ! currently only slat-type blinds are considered 
       
C If shade is retracted set solar transmittance to 1 and skip to next layer
        
        if (i_shd(icomp,icfctp) .eq.0) then ! blind is retracted, treat as air layer ...
          if (isolvis.eq.0) then ! solar
            SolTFbb(icomp,isurf,icfctp,i)=0.99999999
            SolTBbb(icomp,isurf,icfctp,i)=0.99999999
            SolRFbb(icomp,isurf,icfctp,i)=0.0
            SolRBbb(icomp,isurf,icfctp,i)=0.0
            SolTFbd(icomp,isurf,icfctp,i)=0.0
            SolTBbd(icomp,isurf,icfctp,i)=0.0
            SolRFbd(icomp,isurf,icfctp,i)=0.0
            SolRBbd(icomp,isurf,icfctp,i)=0.0
            SolTFdd(icomp,isurf,icfctp,i)=0.99999999
            SolTBdd(icomp,isurf,icfctp,i)=0.99999999
            SolRFdd(icomp,isurf,icfctp,i)=0.0
            SolRBdd(icomp,isurf,icfctp,i)=0.0
          else
            VisTFbb(icomp,isurf,icfctp,i)=0.99999999
            VisTBbb(icomp,isurf,icfctp,i)=0.99999999
            VisRFbb(icomp,isurf,icfctp,i)=0.0
            VisRBbb(icomp,isurf,icfctp,i)=0.0
            VisTFbd(icomp,isurf,icfctp,i)=0.0
            VisTBbd(icomp,isurf,icfctp,i)=0.0
            VisRFbd(icomp,isurf,icfctp,i)=0.0
            VisRBbd(icomp,isurf,icfctp,i)=0.0
            VisTFdd(icomp,isurf,icfctp,i)=0.99999999
            VisTBdd(icomp,isurf,icfctp,i)=0.99999999
            VisRFdd(icomp,isurf,icfctp,i)=0.0
            VisRBdd(icomp,isurf,icfctp,i)=0.0
          endif

          ! also set the sky and ground diffuse properties 
          ! if the sky/ground calc has been requested.  
          if (i_ground_sky_diff_calc.eq.1) then 
            if (isolvis.eq.0) then ! solar
              SolTFskydd(icomp,isurf,icfctp,i)=0.9999999 
              SolRFskydd(icomp,isurf,icfctp,i)=0.0
              SolTFgrddd(icomp,isurf,icfctp,i)=0.9999999 
              SolRFgrddd(icomp,isurf,icfctp,i)=0.0

              SolTBskydd(icomp,isurf,icfctp,i)=0.9999999 
              SolRBskydd(icomp,isurf,icfctp,i)=0.0 
              SolTBgrddd(icomp,isurf,icfctp,i)=0.9999999 
              SolRBgrddd(icomp,isurf,icfctp,i)=0.0
            else
              ! visual
              VisTFskydd(icomp,isurf,icfctp,i)=0.9999999 
              VisRFskydd(icomp,isurf,icfctp,i)=0.0
              VisTFgrddd(icomp,isurf,icfctp,i)=0.9999999 
              VisRFgrddd(icomp,isurf,icfctp,i)=0.0

              VisTBskydd(icomp,isurf,icfctp,i)=0.9999999 
              VisRBskydd(icomp,isurf,icfctp,i)=0.0
              VisTBgrddd(icomp,isurf,icfctp,i)=0.9999999 
              VisRBgrddd(icomp,isurf,icfctp,i)=0.0
            endif
          end if ! ground / sky

        else ! continue as usual

C         Set beam-beam reflectances to zero
          if (isolvis.eq.0) then ! solar
            SolRFbb(icomp,isurf,icfctp,i)=0.0
            SolRBbb(icomp,isurf,icfctp,i)=0.0
          else
            VisRFbb(icomp,isurf,icfctp,i)=0.0
            VisRBbb(icomp,isurf,icfctp,i)=0.0
          endif

C         Assign direct normal solar/visible properties to local variables
          if (isolvis.eq.0) then ! solar
            Rufs=solRF(icomp,icfctp,i)
            Rdfs=solRB(icomp,icfctp,i)
            Tslat=solT(icomp,icfctp,i)
          else
            Rufs=visRF(icomp,icfctp,i)
            Rdfs=visRB(icomp,icfctp,i)
            Tslat=visT(icomp,icfctp,i)
          endif

C         Assign slat blind descriptors to local variables
          w=vb_w(icomp,icfctp)
          s=vb_s(icomp,icfctp)
          phi=vb_phiNew(icomp,icfctp)*r
          crown=vb_crown(icomp,icfctp)
          wr=vb_wr(icomp,icfctp)
          t=vb_t(icomp,icfctp)
          tilt=PELV(icomp,isurf)
       
          if(crown.gt. 1.0 .and.crown.gt.t)then !slat is CURVED

            sl_rad_overw=1./wr
            Curved=.true.

C           Calculate front effective solar optical properties and assign to commons
            call VB_SOL46_CURVE(s,w,sl_rad_overw,phi,proang_rad,
     &            Rdfs,Rufs,Tslat,OutRFbd,OutTFbb,OutTFbd)

            if (isolvis.eq.0) then ! solar
              SolTFbb(icomp,isurf,icfctp,i)=OutTFbb
              SolRFbd(icomp,isurf,icfctp,i)=OutRFbd
              SolTFbd(icomp,isurf,icfctp,i)=OutTFbd
            else
              VisTFbb(icomp,isurf,icfctp,i)=OutTFbb
              VisRFbd(icomp,isurf,icfctp,i)=OutRFbd
              VisTFbd(icomp,isurf,icfctp,i)=OutTFbd
            endif

C...........CALCULATE GROUND AND SKY DIFFUSE COMPONENTS (FRONT SURFACE)
C           Check whether calculation of ground and sky diffuse
C           components has been requested. 
            
            if (i_ground_sky_diff_calc.eq.1) then

              call GroundSkyDiffCalc(Curved,s,w,sl_rad_overw,phi,
     &                               tilt,Rdfs,Rufs,Tslat,
     &                               TXskydiff,RXskydiff,
     &                               TXgrddiff,RXgrddiff)

              if (isolvis.eq.0) then ! solar
                SolTFskydd(icomp,isurf,icfctp,i)=TXskydiff
                SolRFskydd(icomp,isurf,icfctp,i)=RXskydiff
                SolTFgrddd(icomp,isurf,icfctp,i)=TXgrddiff
                SolRFgrddd(icomp,isurf,icfctp,i)=RXgrddiff
              else
                VisTFskydd(icomp,isurf,icfctp,i)=TXskydiff
                VisRFskydd(icomp,isurf,icfctp,i)=RXskydiff
                VisTFgrddd(icomp,isurf,icfctp,i)=TXgrddiff
                VisRFgrddd(icomp,isurf,icfctp,i)=RXgrddiff
              endif
            end if ! end sky/ground diffuse request for Curved, FRONT

C           Calculate front effective diffuse (hemispherical) 
C           properties and assign to commons
            call vb_eff_diff_properties(w,s,phi/r,Rufs,
     &            Rdfs,Tslat,OutRFdd,OutTFdd)

            if (isolvis.eq.0) then ! solar
              SolTFdd(icomp,isurf,icfctp,i)=OutTFdd
              SolRFdd(icomp,isurf,icfctp,i)=OutRFdd
            else
              VisTFdd(icomp,isurf,icfctp,i)=OutTFdd
              VisRFdd(icomp,isurf,icfctp,i)=OutRFdd
            endif

C...........Complementary angle for BACK
            phi=phi*(-1.0)

C           Calculate back effective optical properties and assign to commons
            call VB_SOL46_CURVE(s,w,sl_rad_overw,phi,proang_rad,
     &            Rdfs,Rufs,Tslat,OutRBbd,OutTBbb,OutTBbd)

            if (isolvis.eq.0) then ! solar
              SolTBbb(icomp,isurf,icfctp,i)=OutTBbb
              SolRBbd(icomp,isurf,icfctp,i)=OutRBbd
              SolTBbd(icomp,isurf,icfctp,i)=OutTBbd
            else
              VisTBbb(icomp,isurf,icfctp,i)=OutTBbb
              VisRBbd(icomp,isurf,icfctp,i)=OutRBbd
              VisTBbd(icomp,isurf,icfctp,i)=OutTBbd
            endif

C...........CALCULATE GROUND AND SKY DIFFUSE COMPONENTS (BACK SURFACE)
C           Check whether calculation of ground and sky diffuse
C           components has been requested. 
            
            if (i_ground_sky_diff_calc.eq.1) then

              call GroundSkyDiffCalc(Curved,s,w,sl_rad_overw,phi,
     &                               tilt,Rdfs,Rufs,Tslat,
     &                               TXskydiff,RXskydiff,
     &                               TXgrddiff,RXgrddiff)

              if (isolvis.eq.0) then ! solar
                SolTBskydd(icomp,isurf,icfctp,i)=TXskydiff
                SolRBskydd(icomp,isurf,icfctp,i)=RXskydiff
                SolTBgrddd(icomp,isurf,icfctp,i)=TXgrddiff
                SolRBgrddd(icomp,isurf,icfctp,i)=RXgrddiff
              else
                VisTBskydd(icomp,isurf,icfctp,i)=TXskydiff
                VisRBskydd(icomp,isurf,icfctp,i)=RXskydiff
                VisTBgrddd(icomp,isurf,icfctp,i)=TXgrddiff
                VisRBgrddd(icomp,isurf,icfctp,i)=RXgrddiff
              endif
            end if ! end sky/ground diffuse request for Curved, BACK

C           Calculate back effective diffuse (hemispherical) 
C           properties and assign to commons
            call vb_eff_diff_properties(w,s,phi/r,Rufs,
     &            Rdfs,Tslat,OutRBdd,OutTBdd)

            if (isolvis.eq.0) then ! solar
              SolTBdd(icomp,isurf,icfctp,i)=OutTBdd
              SolRBdd(icomp,isurf,icfctp,i)=OutRBdd
            else
              VisTBdd(icomp,isurf,icfctp,i)=OutTBdd
              VisRBdd(icomp,isurf,icfctp,i)=OutRBdd
            endif

          else ! slat is considered FLAT with thickness t [mm]
            Curved=.false.

C           Calculate front effective solar optical properties and assign to commons
            call VB_SOL46_THICK(s,w,t,phi,proang_rad,
     &            Rdfs,Rufs,Tslat,OutRFbd,OutTFbb,OutTFbd)

            if (isolvis.eq.0) then ! solar
              SolTFbb(icomp,isurf,icfctp,i)=OutTFbb
              SolRFbd(icomp,isurf,icfctp,i)=OutRFbd
              SolTFbd(icomp,isurf,icfctp,i)=OutTFbd
            else
              VisTFbb(icomp,isurf,icfctp,i)=OutTFbb
              VisRFbd(icomp,isurf,icfctp,i)=OutRFbd
              VisTFbd(icomp,isurf,icfctp,i)=OutTFbd
            endif

C...........CALCULATE GROUND AND SKY DIFFUSE COMPONENTS (FRONT SURFACE)
C           Check whether calculation of ground and sky diffuse
C           components has been requested. 
            
            if (i_ground_sky_diff_calc.eq.1) then

              call GroundSkyDiffCalc(Curved,s,w,sl_rad_overw,phi,
     &                               tilt,Rdfs,Rufs,Tslat,
     &                               TXskydiff,RXskydiff,
     &                               TXgrddiff,RXgrddiff)

              if (isolvis.eq.0) then ! solar
                SolTFskydd(icomp,isurf,icfctp,i)=TXskydiff
                SolRFskydd(icomp,isurf,icfctp,i)=RXskydiff
                SolTFgrddd(icomp,isurf,icfctp,i)=TXgrddiff
                SolRFgrddd(icomp,isurf,icfctp,i)=RXgrddiff
              else
                VisTFskydd(icomp,isurf,icfctp,i)=TXskydiff
                VisRFskydd(icomp,isurf,icfctp,i)=RXskydiff
                VisTFgrddd(icomp,isurf,icfctp,i)=TXgrddiff
                VisRFgrddd(icomp,isurf,icfctp,i)=RXgrddiff
              endif

            end if ! end sky/ground diffuse request for Flat, FRONT

C           Calculate front effective diffuse (hemispherical) 
C           properties and assign to commons
            call VB_SOL_DD_THICK(s,w,t,phi,Rufs,Rdfs,
     &            Tslat,OutRFdd,OutTFdd)

            if (isolvis.eq.0) then ! solar
              SolTFdd(icomp,isurf,icfctp,i)=OutTFdd
              SolRFdd(icomp,isurf,icfctp,i)=OutRFdd
            else
              VisTFdd(icomp,isurf,icfctp,i)=OutTFdd
              VisRFdd(icomp,isurf,icfctp,i)=OutRFdd
            endif

C...........Complementary angle for BACK
            phi=phi*(-1.0)

C           Calculate back effective solar optical properties adn assign to commons
            call VB_SOL46_THICK(s,w,t,phi,proang_rad,
     &            Rdfs,Rufs,Tslat,OutRBbd,OutTBbb,OutTBbd)

            if (isolvis.eq.0) then ! solar
              SolTBbb(icomp,isurf,icfctp,i)=OutTBbb
              SolRBbd(icomp,isurf,icfctp,i)=OutRBbd
              SolTBbd(icomp,isurf,icfctp,i)=OutTBbd
            else
              VisTBbb(icomp,isurf,icfctp,i)=OutTBbb
              VisRBbd(icomp,isurf,icfctp,i)=OutRBbd
              VisTBbd(icomp,isurf,icfctp,i)=OutTBbd
            endif

C...........CALCULATE GROUND AND SKY DIFFUSE COMPONENTS (BACK SURFACE)
C           Check whether calculation of ground and sky diffuse
C           components has been requested. 
            if (i_ground_sky_diff_calc.eq.1) then

              call GroundSkyDiffCalc(Curved,s,w,sl_rad_overw,phi,
     &                               tilt,Rdfs,Rufs,Tslat,
     &                               TXskydiff,RXskydiff,
     &                               TXgrddiff,RXgrddiff)

              if (isolvis.eq.0) then ! solar
                SolTBskydd(icomp,isurf,icfctp,i)=TXskydiff
                SolRBskydd(icomp,isurf,icfctp,i)=RXskydiff
                SolTBgrddd(icomp,isurf,icfctp,i)=TXgrddiff
                SolRBgrddd(icomp,isurf,icfctp,i)=RXgrddiff
              else
                VisTBskydd(icomp,isurf,icfctp,i)=TXskydiff
                VisRBskydd(icomp,isurf,icfctp,i)=RXskydiff
                VisTBgrddd(icomp,isurf,icfctp,i)=TXgrddiff
                VisRBgrddd(icomp,isurf,icfctp,i)=RXgrddiff
              endif

            end if ! end sky/ground diffuse request for Flat, BACK

C           Calculate back effective diffuse (hemispherical) 
C           properties and assign to commonss
            call VB_SOL_DD_THICK(s,w,t,phi,Rufs,Rdfs,
     &            Tslat,OutRBdd,OutTBdd)

            if (isolvis.eq.0) then ! solar
              SolTBdd(icomp,isurf,icfctp,i)=OutTBdd
              SolRBdd(icomp,isurf,icfctp,i)=OutRBdd
            else
              VisTBdd(icomp,isurf,icfctp,i)=OutTBdd
              VisRBdd(icomp,isurf,icfctp,i)=OutRBdd
            endif

          end if ! slat considered flat
        endif !  i_shd() .eq. 0 

       elseif(itype.eq.0)then ! gas filled cavity
C       Set gas gap properties to 0 except for bb and dd transmission, set to 1. 
         if (isolvis.eq.0) then ! solar
           SolTFbb(icomp,isurf,icfctp,i)=0.99999999
           SolTBbb(icomp,isurf,icfctp,i)=0.99999999
           SolRFbb(icomp,isurf,icfctp,i)=0.0
           SolRBbb(icomp,isurf,icfctp,i)=0.0
           SolTFbd(icomp,isurf,icfctp,i)=0.0
           SolTBbd(icomp,isurf,icfctp,i)=0.0
           SolRFbd(icomp,isurf,icfctp,i)=0.0
           SolRBbd(icomp,isurf,icfctp,i)=0.0
           SolTFdd(icomp,isurf,icfctp,i)=0.99999999
           SolTBdd(icomp,isurf,icfctp,i)=0.99999999
           SolRFdd(icomp,isurf,icfctp,i)=0.0
           SolRBdd(icomp,isurf,icfctp,i)=0.0
         else
           VisTFbb(icomp,isurf,icfctp,i)=0.99999999
           VisTBbb(icomp,isurf,icfctp,i)=0.99999999
           VisRFbb(icomp,isurf,icfctp,i)=0.0
           VisRBbb(icomp,isurf,icfctp,i)=0.0
           VisTFbd(icomp,isurf,icfctp,i)=0.0
           VisTBbd(icomp,isurf,icfctp,i)=0.0
           VisRFbd(icomp,isurf,icfctp,i)=0.0
           VisRBbd(icomp,isurf,icfctp,i)=0.0
           VisTFdd(icomp,isurf,icfctp,i)=0.99999999
           VisTBdd(icomp,isurf,icfctp,i)=0.99999999
           VisRFdd(icomp,isurf,icfctp,i)=0.0
           VisRBdd(icomp,isurf,icfctp,i)=0.0
         endif
         ! also set the sky and ground diffuse properties 
         ! if the sky/ground calc has been requested.  
         if (i_ground_sky_diff_calc.eq.1) then 
           if (isolvis.eq.0) then ! solar
             SolTFskydd(icomp,isurf,icfctp,i)=0.9999999 
             SolRFskydd(icomp,isurf,icfctp,i)=0.0
             SolTFgrddd(icomp,isurf,icfctp,i)=0.9999999 
             SolRFgrddd(icomp,isurf,icfctp,i)=0.0

             SolTBskydd(icomp,isurf,icfctp,i)=0.9999999 
             SolRBskydd(icomp,isurf,icfctp,i)=0.0 
             SolTBgrddd(icomp,isurf,icfctp,i)=0.9999999 
             SolRBgrddd(icomp,isurf,icfctp,i)=0.0
           else
             VisTFskydd(icomp,isurf,icfctp,i)=0.9999999 
             VisRFskydd(icomp,isurf,icfctp,i)=0.0
             VisTFgrddd(icomp,isurf,icfctp,i)=0.9999999 
             VisRFgrddd(icomp,isurf,icfctp,i)=0.0

             VisTBskydd(icomp,isurf,icfctp,i)=0.9999999 
             VisRBskydd(icomp,isurf,icfctp,i)=0.0
             VisTBgrddd(icomp,isurf,icfctp,i)=0.9999999 
             VisRBgrddd(icomp,isurf,icfctp,i)=0.0
           endif
        end if 

       else

          write(outs,'(a,i2,a,i2,a,i2,a)')
     &      'ERROR: layer type', itype, ' for layer ',i,' of CFC type',
     &       icfctp,' is not supported.'
          CALL EDISP(IUOUT,outs)
          goto 1000
       end if 

  100 continue

 1000 return

      end

C ********************************************************************
C                        --groundskydiffcalc--
C
C Calculate GROUND and SKY diffuse components. FRONT and BACK are defined
C by parameter "phi" at call time.
C
C For slat type (Venetian) blinds, only!
C
C
      subroutine GroundSkyDiffCalc(Curved,s,w,t,phi,tilt,
     &                             Rdfs,Rufs,Tslat,
     &                             TXskydiff,
     &                             RXskydiff,
     &                             TXgrddiff,
     &                             RXgrddiff)

      IMPLICIT NONE

C.....Parameters
      logical Curved
      real s,w,t,phi,tilt
      real Rdfs,Rufs,Tslat

C     Return values (sky and ground diffuse transmittance)
      real TXskydiff       ! sky diffuse transmittance (slat blinds with horizontal slats)
      real RXskydiff       ! sky diffuse reflectance (slat blinds with horizontal slats)
      real TXgrddiff       ! ground diffuse transmittance (slat blinds with horizontal slats)
      real RXgrddiff       ! ground diffuse reflectance (slat blinds with horizontal slats)

C.....Local variables
C     Temporary OUTPUT variables to hold effective solar properties
      real OutRXbd,OutTXbb,OutTXbd

      real skyang,grdang
      real element_proang_sky     ! profile angle of sky element
      real element_proang_grd     ! profile angle of ground element
      integer nelem_sky,nelem_grd ! number of integration slices
      integer iter
      real pi,r,beta,skyfrac

      pi = 4.0 * ATAN(1.0)
      r=pi/180.

      TXskydiff = 0.0 ! reset the sky diffuse transmittance
      RXskydiff = 0.0 ! reset the sky diffuse reflectance  
      TXgrddiff = 0.0 ! reset the ground diffuse transmittance
      RXgrddiff = 0.0 ! reset the ground diffuse reflectance            

C.....Sky / ground fraction as function of surface >tilt< angle
C     Surface tilt angle (PELV(icomp,isurf)) is defined as
C       0°   for vertical surfaces
C      90°   for horizontal surfaces facing upward ('ceiling') and
C     -90°   for horizontal surfaces facing downward ('floor')
C
C     Local tilt angle beta is defined as
C       0°   is horizontal facing upward, no ground view
C      90°   is vertical, 50/50 sky/ground view
C     180°   is horizontal facing downward, no sky view
      beta    = (90.-tilt)
      skyfrac = (180.-beta)/180.
      skyang  = skyfrac*180.
      if (skyang.lt.0.) skyang=0.
      grdang  = 180.-skyang
      if (grdang.lt.0.) grdang=0.

C     Set number of integration slices. Use a minimum of 5 deg increments and a 
C     maximum of 10 slices
      nelem_sky = int(skyang/5.)
      nelem_sky = min(nelem_sky,10)

      nelem_grd = int(grdang/5.)
      nelem_grd = min(nelem_grd,10)

c      write(*,*)'skyang= ',skyang,'; nelem_sky= ',nelem_sky,
c     &          '; grdang= ',grdang,'; nelem_grd= ',nelem_grd

C SKY CALCULATION
      if (skyang.gt.0.) then
C       Perform integration of sky elements based on the midpoint rule:
C       integral(a to b) f(x)dx = Sum(1 to n) f(a + (i-1/2)(b-a)/n) * ((b-a)/n)
        integrate_sky_elements: do iter = 1, nelem_sky
        
           element_proang_sky = (skyang/float(nelem_sky)) * 
     &                          (float(iter)-0.5) * 1.
           
           element_proang_sky = element_proang_sky * r       ! convert to radians
        
C          Calculate the beam-beam and beam-diffuse transmission
C          at the current sky element profile angle
           if (Curved) then
             call  VB_SOL46_CURVE(s,w,t,phi,
     &                          element_proang_sky,           ! profile angle of sky element
     &                          Rdfs,Rufs,Tslat,              ! slat properties input
     &                          OutRXbd,OutTXbb,OutTXbd)      ! overall effective properties output
           else ! flat / thick
             call  VB_SOL46_THICK(s,w,t,phi,
     &                          element_proang_sky,           ! profile angle of sky element
     &                          Rdfs,Rufs,Tslat,              ! slat properties input
     &                          OutRXbd,OutTXbb,OutTXbd)      ! overall effective properties output
           endif
        
C          Sum up each sky element's contribution to the sky
C          diffuse transmittance and reflectance.
           TXskydiff = TXskydiff + 
     &              ((pi/2.)/float(nelem_sky))*           ! (b-a)/n
     &              ((OutTXbb + OutTXbd) * 
     &              cos(element_proang_sky))              ! f(a + (i-1/2)(b-a)/n)
           RXskydiff = RXskydiff + 
     &              ((pi/2.)/float(nelem_sky))*           ! (b-a)/n
     &              ((OutRXbd) * cos(element_proang_sky)) ! f(a + (i-1/2)(b-a)/n)
        
        end do integrate_sky_elements
      else ! no sky view
C         do nothing, return values for sky are 0.
      endif

C GROUND CALCULATION
      if (grdang.gt.0.) then
C       Perform integration of ground elements based on the midpoint rule:
C       integral(a to b) f(x)dx = Sum(1 to n) f(a + (i-1/2)(b-a)/n) * ((b-a)/n)
        integrate_grd_elements: do iter = 1, nelem_grd
        
           element_proang_grd = (grdang/float(nelem_grd)) * 
     &                          (float(iter)-0.5) * 1.
           
           element_proang_grd = element_proang_grd * r * (-1.0)  ! profile angle for ground in radians
        
C          Calculate the beam-beam and beam-diffuse transmission
C          at the current ground element profile angle
           if (Curved) then
             call  VB_SOL46_CURVE(s,w,t,phi,
     &                          element_proang_grd,           ! profile angle of sky element
     &                          Rdfs,Rufs,Tslat,              ! slat properties input
     &                          OutRXbd,OutTXbb,OutTXbd)      ! overall effective properties output
           else ! flat / thick
             call  VB_SOL46_THICK(s,w,t,phi,
     &                          element_proang_grd,           ! profile angle of sky element
     &                          Rdfs,Rufs,Tslat,              ! slat properties input
     &                          OutRXbd,OutTXbb,OutTXbd)      ! overall effective properties output
           endif
        
C          Sum up each sky element's contribution to the sky
C          diffuse transmittance and reflectance.
           TXgrddiff = TXgrddiff + 
     &               ((pi/2.)/float(nelem_grd))*           ! (b-a)/n
     &               ((OutTXbb + OutTXbd) * 
     &                cos(element_proang_grd))             ! f(a + (i-1/2)(b-a)/n)
           RXgrddiff = RXgrddiff + 
     &               ((pi/2.)/float(nelem_grd))*           ! (b-a)/n
     &               ((OutRXbd) * cos(element_proang_grd)) ! f(a + (i-1/2)(b-a)/n)
        
        end do integrate_grd_elements
      else ! no ground view
C       do nothing, return values for ground are 0.
      endif
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
C icalc_mode - switch for 'traditional' (0) or 'sky' (1) or 'ground' (2)
C icomp      - current zone
C isurf      - current surface in zone
C icfctp     - current cfc type in zone
C GBM        - External beam solar irradiance [W/m2]
C GDF        - External diffuse solar irradiance [W/m2]
C IBM        - Internal beam solar irradiance [W/m2]
C IDF        - Internal diffuse solar irradiance [W/m2]
C isolvis    - Switch for solar (0) or visual (1) data
C 
C OUTPUT:
C TRANSBB_SYS - Total transmitted beam solar flux [W/m2]
C TRANSD_SYS  - Total transmitted diffuse solar flux [W/m2]
C REFL_SYS    - Total reflected solar flux (beam+diffuse) [W/m2]
C AbsSol      - Absorbed solar flux at each layer [W/m2]
C
C ********************************************************************
      subroutine solar_multilayer(icalc_mode,icomp,isurf,icfctp,
     &           GBM,GDF,IBM,IDF,
     &           isolvis,TRANSBB_SYS,TRANSD_SYS,REFL_SYS,AbsSol)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"

      INTEGER LayNo,n,m,i,icomp,isurf,icfctp,isolvis,icalc_mode
      INTEGER itype
      REAL GBM, GDF, IDF, IBM
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

C Initialise return values to zero
      TRANSBB_SYS = 0.0
      TRANSBD_SYS = 0.0
      TRANSDD_SYS = 0.0
      TRANSD_SYS  = 0.0
      REFL_SYS    = 0.0

      do i=1,LayNo,1
        AbsSol(i)=0.0
      enddo

C If no radiation, opt out
      if (GBM.le.0. .and. GDF.le.0.) then
        return
      endif

C Assign optical properties for each layer from commons to temp arrays depending
C on isolvis
      if (isolvis .eq. 0) then
        do 10 i=1,LayNo
C           direct-direct and direct-diffuse
            rfbb(i)=SolRFbb(icomp,isurf,icfctp,i)
            rbbb(i)=SolRBbb(icomp,isurf,icfctp,i)
            tfbb(i)=SolTFbb(icomp,isurf,icfctp,i)
            tbbb(i)=SolTBbb(icomp,isurf,icfctp,i)
            rfbd(i)=SolRFbd(icomp,isurf,icfctp,i)
            rbbd(i)=SolRBbd(icomp,isurf,icfctp,i)
            tfbd(i)=SolTFbd(icomp,isurf,icfctp,i)
            tbbd(i)=SolTBbd(icomp,isurf,icfctp,i)

            itype=icfcltp(icomp,icfctp,i)

C           diffuse properties, sky and ground for slat blinds if requested
            if(itype.eq.2 .and. icalc_mode.eq.i_sky) then
c?            if(icalc_mode.eq.i_sky) then
              rfdd(i)=SolRFskydd(icomp,isurf,icfctp,i)
              rbdd(i)=SolRBskydd(icomp,isurf,icfctp,i)
              tfdd(i)=SolTFskydd(icomp,isurf,icfctp,i)
              tbdd(i)=SolTBskydd(icomp,isurf,icfctp,i)
            elseif(itype.eq.2 .and. icalc_mode.eq.i_ground) then
c?            elseif(icalc_mode.eq.i_ground) then
              rfdd(i)=SolRFgrddd(icomp,isurf,icfctp,i)
              rbdd(i)=SolRBgrddd(icomp,isurf,icfctp,i)
              tfdd(i)=SolTFgrddd(icomp,isurf,icfctp,i)
              tbdd(i)=SolTBgrddd(icomp,isurf,icfctp,i)
            else ! traditional
              rfdd(i)=SolRFdd(icomp,isurf,icfctp,i)
              rbdd(i)=SolRBdd(icomp,isurf,icfctp,i)
              tfdd(i)=SolTFdd(icomp,isurf,icfctp,i)
              tbdd(i)=SolTBdd(icomp,isurf,icfctp,i)
            end if

 10     continue
 
      else ! visual properties
        do 11 i=1,LayNo
C           direct-direct and direct-diffuse
            rfbb(i)=VisRFbb(icomp,isurf,icfctp,i)
            rbbb(i)=VisRBbb(icomp,isurf,icfctp,i)
            tfbb(i)=VisTFbb(icomp,isurf,icfctp,i)
            tbbb(i)=VisTBbb(icomp,isurf,icfctp,i)
            rfbd(i)=VisRFbd(icomp,isurf,icfctp,i)
            rbbd(i)=VisRBbd(icomp,isurf,icfctp,i)
            tfbd(i)=VisTFbd(icomp,isurf,icfctp,i)
            tbbd(i)=VisTBbd(icomp,isurf,icfctp,i)

            itype=icfcltp(icomp,icfctp,i)

C           diffuse properties, sky and ground for slat blinds if requested
            if(itype.eq.2 .and. icalc_mode.eq.i_sky) then
c?            if(icalc_mode.eq.i_sky) then
              rfdd(i)=VisRFskydd(icomp,isurf,icfctp,i)
              rbdd(i)=VisRBskydd(icomp,isurf,icfctp,i)
              tfdd(i)=VisTFskydd(icomp,isurf,icfctp,i)
              tbdd(i)=VisTBskydd(icomp,isurf,icfctp,i)
            elseif(itype.eq.2 .and. icalc_mode.eq.i_ground) then
c?            elseif(icalc_mode.eq.i_ground) then
              rfdd(i)=VisRFgrddd(icomp,isurf,icfctp,i)
              rbdd(i)=VisRBgrddd(icomp,isurf,icfctp,i)
              tfdd(i)=VisTFgrddd(icomp,isurf,icfctp,i)
              tbdd(i)=VisTBgrddd(icomp,isurf,icfctp,i)
            else ! traditional
              rfdd(i)=VisRFdd(icomp,isurf,icfctp,i)
              rbdd(i)=VisRBdd(icomp,isurf,icfctp,i)
              tfdd(i)=VisTFdd(icomp,isurf,icfctp,i)
              tbdd(i)=VisTBdd(icomp,isurf,icfctp,i)
            end if

 11     continue
      endif

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
! Layer Absorptance Calculation
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
        SS(i)=(Bminus(i)-Bplus(i)+Bplus(i+1)-Bminus(i+1)+Dminus(i)
     &                    -Dplus(i)+Dplus(i+1)-Dminus(i+1)) !/ (GBM+GDF)
        AbsSol(i)=SS(i)
      ENDDO

      IF (GBM.lt.0.001) THEN
            GBM=0.0     ! reset back to zero (???)
      ENDIF

      RETURN
      END


C ********************************************************************
C                        --ConvVertCav--
C
C Calculates convection coefficient between two surfaces at tilt 
C angle (ang) in an enclosed cavity based on cavity thickness (L), 
C bounding surface temperatures(TA, TB) and fill gas properties 
C (mlr_mass,condA,condB, viscA,viscB,spchtA,spchtB). Returns
C convection coefficient (hc) in W/(m2 K).
C
C References:
C Shewen, E., Hollands, K.G.T., Raithby, G.D. (1996) 'Heat Transfer
C by Natural Convection Across a Vertical Cavity of Large Aspect
C Ratio', Journal of Heat Transfer, Vol. 118, pp.993-995.
C
C EN 673:1996
C
C ISO 15099:2003
C
C ********************************************************************
      subroutine ConvVertCav(ang,L,TA,TB,mlr_mass,condA,condB,
     &                          viscA,viscB,spchtA,spchtB,hc)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h" 
C provides i_hcond

C gas molar mass, A and B gas property coefficients
      real mlr_mass,condA,condB,viscA,viscB,spchtA,spchtB

      real ang,L,TA,TB,hc
      real g,R,P,Tm,ro,cp,k,mu,dT,Ra,Nu
      real A,n,PI,rad
      real gam,Nu1,Nu2,GG,Agv,XX,YY,Nu_v,Nu_60

      logical closea

C if gap is evacuated, set hc to 0.1
C This is a crude idealized treatment.
      if(mlr_mass.lt.0.0001)then
            hc=0.01
            goto 1000
      endif

      g=9.806402                       ! gravity [m/s2]
      P=101325.                        ! abs pressure [Pa]
      R=8.314472                       ! universal gas constant [J/mol.K]
      PI = 4.0 * ATAN(1.0)
      rad=PI/180.                      ! conversion grad -> rad

C Convert temperature to Kelvin
      TA=TA+273.15 ! "outer" temperature
      TB=TB+273.15 ! "inner" temperature

      Tm=0.5*(TA+TB)                   !mean temp

C Calculate gas properties based on linear Temperature fit
      k=condA+condB*Tm                 !conductivity [W/m.K]
      mu=viscA+viscB*Tm                !viscosity[N.sec/m^2]
      cp=spchtA+spchtB*Tm              !specific heat [J/kg.K]

      ro=(P*mlr_mass)/(R*Tm*1000.)     !density [kg/m^3]

      dT=ABS(TA-TB)                    !temp. diff.

C Calculate Rayleigh number (= Gr*Pr)
      Ra=ro*ro*cp*dT*L*L*L*g/(mu*k*Tm)

      call eclose(Ra,0.00,0.01,closea)
      if (closea) Ra=1.0

      if (i_hconv.eq.0) then
C Calculate Nusselt number (Shewen, Hollands and Raithby 1996)
        Nu=SQRT(1.+((0.0665*Ra**(0.33333))/
     &          (1.+(9000./Ra)**1.4))**(2.0))

      elseif (i_hconv.eq.1) then
C ***  Nusselt number acc. to EN 673:1996 ***
C       Calculate coefficients A and n (EN 673, tilted glass) from tilt angle ang
c        A=0.036 ! (0deg, vert) 0.10 (45deg) 0.16 (90deg, horiz)
        A=0.036667+0.0013778*ang !  R= 0.99983 
c        n=0.38  ! (0deg, vert) 0.31 (45deg) 0.28 (90deg, horiz)
        n=0.38-0.002*ang+(9.8765E-6)*ang**2. ! R=1

C       Angle dependant Nusselt number (EN 673 eqn 6)
c        Nu = A*Ra**(n)
        Nu=1.0
        if (Ra.gt.0.) Nu=A*exp(n*log(Ra)) ! x**y
C
C       'ang' is PELV(icomp,isurf) and defined as
C       0°   for vertical surfaces
C      90°   for horizontal surfaces facing upward ('ceiling') and
C     -90°   for horizontal surfaces facing downward ('floor')

        if ((ang.lt.0.).and.(ang.gt.-45.)) then
C         use vertical value for now
          Nu=0.036*exp(0.38*log(Ra))
C       if heat flux downwards, set Nu=0.1 (very small convective coefficent)
        elseif (ang.le.-45.) then
          Nu=0.1
        endif
C       Now Nu is set according to EN 
C *** END EN 673:1996 ***
      else ! i_hconv.eq.2

C *** Nusselt number acc. to ISO 15099:2003 ***
C       Section 5.3.3
C       gam =   0deg ... horizontal glazing, heat flow upwards
C       gam =  90deg ... vertical glazing, heat flow horizontal
C       gam = 180deg ... horizontal glazing, heat flow downwards
C   
C       It is assumed that TA is "outside" and TB is "inside". Base eqns
C       assume that TA < TB, i.e. heat flow from inside to outside. If 
C       TA > TB, the complement of the tilt angle is to be used, i.e.
C       180deg - gam instead of gam. and calculations done with 180deg - gam!
C   
        if (TA .le. TB) then
          gam = 90. - ang
        else ! TA > TB, use complement angles
          gam = 90. + ang ! 180 - (90. - ang)
        endif

C       Vertical value (eqn. 49) is necessary for more than one angle, always calculate:
        Agv=22. ! Ag,v = H / dg,v ??? height ??? where from ???
c        use max vertice z - min vertice z per surface?
    
        if (Ra .gt. 50000.) then
          Nu1=0.673838*exp(0.333333*log(Ra))
        elseif ((Ra.le. 50000.) .and. (Ra .gt. 10000.)) then
          Nu1=0.028154*exp(0.4134*log(Ra))
        else ! Ra < 10000.
          Nu1=1.+1.7596678E-10*exp(2.2984755*log(Ra))
        endif
        Nu2=0.242*exp(0.272*log(Ra/Agv))
        Nu_v=max(Nu1,Nu2)! Nusselt for vertical cavity
    
C       Value for 60deg is also used for more than one angle, also always calculate:
        GG = 0.5/exp(0.1*log(1.+exp(20.6*log(Ra/3160.))))
        Nu1= exp((1./7.)*log(1.+
     &              exp(7.*log(0.0936*exp(0.314*log(Ra)))/(1.+GG))))
        Nu2= (0.104+0.175/Agv)*exp(0.283*log(Ra))
        Nu_60=max(Nu1,Nu2) ! Nusselt for 60 deg tilt
     
c       Check if vertical
        call eclose(ang,0.00,0.01,closea)
     
        if ((0. .le.gam) .and. (gam.lt. 60.)) then
          XX = (1.-1708./(Ra*cos(gam*rad)))
          XX = (XX + abs(XX))/2.
          YY = (exp(0.333333*log(Ra*cos(gam*rad)/5830.))-1.)
          YY = (YY + abs(YY))/2.
          Nu = 1.+1.44*XX*
     &         (1.-(1708.*exp(1.6*log(sin(1.8*gam*rad))))/
     &                                         (Ra*cos(gam*rad)))+YY
    
        elseif (gam .eq. 60.) then
          Nu = Nu_60
    
        elseif ((60. .lt.gam) .and. (gam.lt. 90.)) then
C       linear interpolation between Nu_60 and Nu_v
          Nu = gam*(Nu_v-Nu_60)/30.+Nu_60-2.*(Nu_v-Nu_60)
          
        elseif (closea) then ! vertical
          Nu = Nu_v 
    
        else ! between 90 and 180 deg
          Nu=1.+(Nu_v-1.)*sin(gam*rad)
    
        endif
C       Now Nu is set according to ISO
C *** END ISO 15099:2003 ***
      endif ! which model?

C Nusselt number Nu is now set, so finally
C calculate convective heat transfer coefficient:
      hc=Nu*k/L     !(W/(m^2.K))

c Debug:
c      write(*,*)'A=',A,'; n=',n,'; Ra=',
c     &        Ra,'; Nu=',Nu,'; hc=',hc

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
     &NPGAP(MCOM,MS,MGP)
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

         IF(icfctp.gt.0)then !detect CFC

C For discretized constructions.
           NGP=NGAPS(ICOMP,I) !number of gaps in surface I
           M=1  !gap index
           IL=1 !layer index
           IN=1 !node index

C Set the air gap flag to off.
           IFLG=0

C Continue with other nodes (homogeneous) in the first layer.
           DO 20 ID=1,NNDL(ICOMP,I,IL)-1
             IN=IN+1
 20        CONTINUE

C Continue with other layers in the construction.
            DO 30 IL=2,NELTS(ICOMP,I)

C The current layer is an air gap.
              IF(M.LE.NGP.AND.NPGAP(ICOMP,I,M).EQ.IL)THEN

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

   41           CONTINUE
                M=M+1

C The current layer is solid.
              ELSE

C If the previous layer was solid.
                IF(IFLG.EQ.0)THEN
                  IN=IN+1

C If the previous layer was an air gap
                ELSE
                  IN=IN+1
C Modify present and future difference equation coefficients
C with time dependent gas gap resistances
                  VCP(I,IN,1)=VCP(I,IN,1)/cfcRgap(icomp,i,il-1,1)
                  VCP(I,IN,2)=VCP(I,IN,2)
                  VCF(I,IN,1)=VCF(I,IN,1)/cfcRgap(icomp,i,il-1,2)
                  VCF(I,IN,2)=VCF(I,IN,2)

                ENDIF
                IFLG=0

C Continue with other nodes (homogeneous) in the layer.
                DO 51 ID=1,NNDL(ICOMP,I,IL)-1
                  IN=IN+1
   51           CONTINUE

              ENDIF

   30      CONTINUE

         END IF
   10  CONTINUE

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
      real F12_4,F13_4, F14_4, F31_4, F41_4, FSS_4	                ! four-surface model shape factors
      real C3, B3, C4, B4, K3, K4, DEN   	                        ! temporaries

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

      tauvb = F12_4 + (F14_4*K3) + (F13_4*K4)	! transmittance of VB (equal front/back)
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
     &       (PHI_RAD.LT.(-90.0*PI/180.0)) ) THEN
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
     &       (OHM_RAD.LT.(-89.5*PI/180.0)) ) THEN
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
            XA=SL_RAD*SIN(-SL_THETA/2.0)	!Glass-side end coordinate
            YA=SL_RAD*COS(-SL_THETA/2.0)
            XB=-XA      !Indoor-side end coordinate
            YB=YA
            YC=SL_RAD*COS(PHI+OMEGA)	!Tangent to slat in irradiance direction
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

            T_CORR_D=SQRT((XC-XD)**2+(YC-YD)**2)	!Slat thickness perpendicular to light direction
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
     &            RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,
     &            RHOSFBD, TAUSFBD)
            ELSE                                ! NO
                  CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &            RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,
     &            RHOSFBD, TAUSFBD)
            ENDIF

      ELSE  ! NO, THERE IS NO DOUBLE BLOCKAGE

            IF(ABS(PHI+OMEGA).LT.(SL_THETA/2.0)) THEN  ! YES, APPLY CURVATURE CORRECTION 

                  XA=SL_RAD*SIN(-SL_THETA/2.0)      !Glass-side end coordinate
                  YA=SL_RAD*COS(-SL_THETA/2.0)
                  XB=-XA                              !Indoor-side end coordinate
                  YB=YA
                  YC=SL_RAD*COS(PHI+OMEGA)	!Tangent to slat in irradiance direction
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
     &                  RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,
     &                  RHOSFBD, TAUSFBD)
                        RHOSFBD=RHOSFBD*T_CORR_D/(S*COS(OMEGA))
                        TAUSFBD=TAUSFBD*T_CORR_D/(S*COS(OMEGA))

                  ELSE  !Slat is lit from below
                        DE=XC-XA
                        CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &                  RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,
     &                  RHOSFBD, TAUSFBD)
                        RHO_TEMP=RHOSFBD*T_CORR_F/(S*COS(OMEGA))
                        TAU_TEMP=TAUSFBD*T_CORR_F/(S*COS(OMEGA))
                        DE=ABS(XB-XF)
                        CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &                  RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,
     &                  RHOSFBD, TAUSFBD)
                        RHOSFBD=RHOSFBD*(T_CORR_D-T_CORR_F)/
     &                  (S*COS(OMEGA))+RHO_TEMP
                        TAUSFBD=TAUSFBD*(T_CORR_D-T_CORR_F)/
     &                  (S*COS(OMEGA))+TAU_TEMP
                  ENDIF


            ELSE ! NO, DO NOT APPLY CURVATURE CORRECTION

                  DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))
                  IF((DE/W).GT.(1.0 - SMALL_ERROR)) THEN          ! YES
                        CALL VB_SOL4(S, W, OMEGA, DE, PHI,
     &                  RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,
     &                  RHOSFBD, TAUSFBD)

                  ELSE       ! NO
                        CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &                  RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,
     &                  RHOSFBD, TAUSFBD)

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
     &            RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)
            ELSE      ! NO
                  TAUSFBB = 0.0
                  CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &            RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,RHOSFBD, TAUSFBD)
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
      PHI = 0.0	        ! INITIALIZE PHI
      OMEGA = 0.0	! INITIALIZE OMEGA


!-----------------------------------------------------------------------------------
!  CHECK TO SEE IF SLAT ANGLE IS EQUAL TO OR GREATER THAN 89.5 DEGREES
!  AND SET THE SLAT ANGLE TO 89.5 DEGREES 
!  (THIS IS DONE FOR BOTH POSITIVE AND NEGATIVE SLAT ANGLES)
      IF((PHI_RAD.ge.(89.5*PI/180.0)).OR.
     &      (PHI_RAD.le.(-89.5*PI/180.0)))THEN		
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
     &      (OHM_RAD.lt.(-89.5*PI/180.0)))THEN
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
      IF(H.LE.0.001)H=0.00 !SLATS ARE TOUCHING

      FEDGE = (T*ABS(COS(PHI+OMEGA)))/
     &  ((H + T/(ABS(COS(PHI))))*(ABS(COS(OMEGA))))

!------------------------------------------------------------------------------------

!  CHECK TO SEE IF BEAM IS ALLIGNED WITH SLATS
      IF(ABS(PHI + OMEGA).LT.SMALL_ERROR) THEN  ! YES
            RHOSFBD = FEDGE*((RHOUFS_SLAT+RHODFS_SLAT)/2.0)
            TAUSFBB =       1.0-FEDGE
            TAUSFBD =       0.0

      ELSE                             ! BEAM NOT ALLIGNED WITH SLATS
            RHOSFBD =       0.0
            TAUSFBB =       0.0
            TAUSFBD =       0.0

            DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))

!  CHECK TO SEE IF THERE IS DIRECT BEAM TRANSMISSION
            IF((DE/W).GT.(1.0 - SMALL_ERROR)) THEN          ! YES
                  TAUSFBB = ((DE-W)/DE) * (1.-FEDGE)
                  IF(TAUSFBB.LT.0.0) TAUSFBB = 0.0
                  CALL VB_SOL4(S, W, OMEGA, DE, PHI,
     &            RHODFS_SLAT, RHOUFS_SLAT, 
     &            TAU_SLAT,RHOSFBD, TAUSFBD)

                  RHOSFBD= RHOSFBD*(1.-FEDGE) +
     &            FEDGE*((RHOUFS_SLAT+RHODFS_SLAT)/2.0)

                  TAUSFBD = TAUSFBD*(1.-FEDGE)

            ELSE     ! NO
                  TAUSFBB = 0.0
                  CALL VB_SOL6(S, W, OMEGA, DE, PHI,
     &            RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,
     &            RHOSFBD, TAUSFBD)

                  RHOSFBD= RHOSFBD*(1.-FEDGE) + 
     &            FEDGE*((RHOUFS_SLAT+RHODFS_SLAT)/2.0)

                  TAUSFBD = TAUSFBD*(1.-FEDGE)

            ENDIF  !  END CHECK FOR DIRECT BEAM TRANSMISSION
      ENDIF  ! END CHECK TO SEE IF BEAM ALLIGNED WITH SLATS

!  FINAL CHECK FOR NEGATIVE VALUES
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
      SUBROUTINE VB_SOL4(S,W,OMEGA,DE,PHI,RHO3,RHO4,TAU,
     &                        RHOSFBD,TAUSFBD)
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
            ELSE  ! NO, THERE ARE NO GAPS IN BETWEEN SLATS
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

      SUBROUTINE VB_SOL6(S,W,OMEGA,DE,PHI,RHO35, RHO46, TAU,
     &                  RHOSFBD, TAUSFBD)
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
     &            RHODFS_SLAT, TAU_SLAT, RHOSFDD, TAUSDD)
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
     &      (PHI_RAD.le.(-89.5*PI/180.0)))THEN
            IF (PHI_RAD.lt.0.0) THEN
                  PHI = -89.5*PI/180.0
            ELSE
                  PHI = 89.5*PI/180.0
            ENDIF
      ELSE
            PHI = PHI_RAD ! USUAL ACTION
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
      subroutine profile_angle(icomp,isurf,icfctp,proang)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"

      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      real sazi,salt
      integer isunup

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      real pazi,pelv

      integer icomp,isurf,icfctp
      real proang,pi,r,pang_vh,pang_ih

      pi = 4.0 * ATAN(1.0)
      r=pi/180.

C Check if slat blind is horizontal or vertical and apply appropriate
C equation (ref. Lomanowski 2008)
      IF(vb_VorH(icomp,icfctp).eq.'HORZ')then

        proang=ATAN(TAN(SALT*r)/COS(SAZI*r-PAZI(icomp,isurf)*r))/r

C Correct for blind assembly tilt angle PELV if not vertical 
C (A. Geissler, 2011)
C In ESP-r, PELV is defined as:
C     0°   for vertical surfaces (accept +- 5° as vertical, here)
C    90°   for horizontal surfaces facing upward ('ceiling') and
C   -90°   for horizontal surfaces facing downward ('floor')
        if (abs(PELV(icomp,isurf)).gt.5.) then
C In ref. C. Chantrasrisalai, PhD Thesis 2007, Oklahoma State University, 
C page 61 eqn. 4-1b the blind assembly tilt angle (Sig) is defined as
C     0°   for blind assembly facing upward,
C    90°   for blind assembly vertical and
C   180°   for blind assembly facing the ground.
C         Eqn. 4-1b for the ESP-r angle reference system becomes
          proang=proang - PELV(icomp,isurf)
        endif

      ELSEIF(vb_VorH(icomp,icfctp).eq.'VERT')then

        proang=SAZI-PAZI(ICOMP,ISURF)

C Correct for blind assembly tilt angle PELV if not vertical 
C (A. Geissler, 2011)
        if (abs(PELV(icomp,isurf)).gt.5.) then
          pang_vh=ATAN(TAN(SALT*r)/COS(SAZI*r-PAZI(icomp,isurf)*r))/r
          pang_ih=pang_vh-PELV(icomp,isurf)
C         Eqn. 4-1d:
          proang=atan(cos(pang_vh)*tan(proang)/cos(pang_ih))
        endif

      ELSE
           proang=90.
      END IF

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
C loop through zone surfaces
      do 100 i=1,nc
            ne=nelts(icomp,i)
            icfctp=icfcfl(icomp,i) ! cfc type index

C if cfc is detected AND indoor slat blind is present
                  if(icfctp.gt.0)then
                   if(icfcltp(icomp,icfctp,ne).ge.2)then
                      do 110 j=1,nc
                         HRF(i,j)=0.0
                         HRF(j,i)=0.0
 110                  continue
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

C if cfc is detected AND outdoor slat blind is present
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

      integer i,j,k,l,nn,nc,ne,icfctp,icomp,ivblayr

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
      if(icfcltp(icomp,icfctp,j).eq.2)ivblayr=j
 11   continue

      write(97,9991)
     &'   CFC SURFACE |  PAZI  |  ANGI  |  PROANG  | HCIF |',
     &'HCOF |VBLAYR|   W   |   S   |   PHI   | ORIENT |'
      write(97,9992)i,pazi(icomp,i),fInc_angle(i),
     & proang_s(icomp,icfctp,i),
     & hcif(icomp,i),hcof(icomp,i),ivblayr,vb_w(icomp,icfctp),
     & vb_s(icomp,icfctp),vb_phiNew(icomp,icfctp),vb_VorH(icomp,icfctp)
 9991     format(A57,A52)
 9992     format(6x,I10,3x,5F9.3,I7,3f9.3,A8)

      write(97,9993)
     &'     layer |SolRFbb|SolRBbb|SolTFbb|SolTBbb',
     &'|SolRFbd|SolRBbd|SolTFbd|SolTBbd',
     &'|SolRFdd|SolRBdd|SolTFdd|SolTBdd',
     &'| Emisf | Emisb | taul  |'

 9993     format(4x,A48,A32,A31,A26)

      do 12 k=1,ne

      write(97,9994)k,SolRFbb(icomp,i,icfctp,k),
     &SolRBbb(icomp,i,icfctp,k),SolTFbb(icomp,i,icfctp,k),
     &SolTBbb(icomp,i,icfctp,k),SolRFbd(icomp,i,icfctp,k),
     &SolRBbd(icomp,i,icfctp,k),SolTFbd(icomp,i,icfctp,k),
     &SolTBbd(icomp,i,icfctp,k),SolRFdd(icomp,i,icfctp,k),
     &SolRBdd(icomp,i,icfctp,k),SolTFdd(icomp,i,icfctp,k),
     &SolTBdd(icomp,i,icfctp,k),
     &rlwEF(icomp,icfctp,k),rlwEB(icomp,icfctp,k),
     &rlwT(icomp,icfctp,k)
 9994       format(6x, I12,1x,15F8.3)
 12   continue
      end if
 10   continue
C********************************************************************

C**************** CFC Convection/LW exchange*************************
      write(98,*)
     & 'TIMESTEP | ICOMP | ISUNUP |  SAZI  |  SALT  |  Tamb  |  Tzone'
        write(98,9995)NSINC,ICOMP,ISUNUP,SAZI,SALT,TP,TFA(ICOMP)

 9995     format(I6,I10,I7,3x,4F9.3)

      nc=nconst(icomp)

      do 13 i=1,nc

      if (icfcfl(icomp,i).gt.0)then
      icfctp=icfcfl(icomp,i)
      ne=nelts(icomp,i)

      do 14 j=1,ne
      if(icfcltp(icomp,icfctp,j).eq.2)ivblayr=j
 14   continue

      write(98,9996)
     & '   CFC SURFACE |  PAZI  |  ANGI  |  PROANG  | HCIF |',
     & 'HCOF |VBLAYR|   W   |   S   |   PHI   | ORIENT |'
      write(98,9997)i,pazi(icomp,i),fInc_angle(i),
     & proang_s(icomp,icfctp,i),
     & hcif(icomp,i),hcof(icomp,i),ivblayr,vb_w(icomp,icfctp),
     & vb_s(icomp,icfctp),vb_phiNew(icomp,icfctp),
     & vb_VorH(icomp,icfctp)
 9996     format(A57,A52)
 9997     format(6x,I10,3x,5F9.3,I7,3f9.3,A8)

      write(98,9998)
     &'     layer |tfc(o) |tfc(m) |tfc(i) |CFCRgap',
     &'|Q_lw(o)|Q_lw(m)|Q_lw(i)|',
     &'|QextVB_g_env| h_vb_room  |h_glass_room|',
     &'|QintVB_b_rm | QintVB_g_rm|',
     &'|Q_cv(o)|Q_cv(m)|Q_cv(i)|'

 9998     format(4x,A48,A25,A40,A28,A26)

      do 15 l=1,ne

      if((icfcltp(icomp,icfctp,l).lt.2.and.l.lt.ne)
     &.or.(icfcltp(icomp,icfctp,l).eq.2.and.l.gt.1.and.l.lt.ne))then

      write(98,9999)l,tfc(icomp,i,(l*2)-1),tfc(icomp,i,l*2),
     & tfc(icomp,i,(l*2)+1),
     &cfcRgap(icomp,i,l,2),qcfc_lw(icomp,i,(l*2)-1,2),
     &qcfc_lw(icomp,i,l*2,2),qcfc_lw(icomp,i,(l*2)+1,2),0.,0.,
     &0.,0.,0.,qcfc_cv(icomp,i,(l*2)-1,2),qcfc_cv(icomp,i,l*2,2),
     &qcfc_cv(icomp,i,(l*2)+1,2)

      !outdoor blind
      elseif(icfcltp(icomp,icfctp,l).eq.2 .and. l.eq.1)then

      write(98,9999)l,tfc(icomp,i,(l*2)-1),tfc(icomp,i,l*2),
     & tfc(icomp,i,(l*2)+1),
     &cfcRgap(icomp,i,l,2),qcfc_lw(icomp,i,(l*2)-1,2),
     &qcfc_lw(icomp,i,l*2,2),qcfc_lw(icomp,i,(l*2)+1,2),
     &q_glass_extS(icomp,i),0.,
     &0.,0.,0.,qcfc_cv(icomp,i,(l*2)-1,2),qcfc_cv(icomp,i,l*2,2),
     &qcfc_cv(icomp,i,(l*2)+1,2)

      !indoor blind
      elseif(l.eq.ne)then

         if(icfcltp(icomp,icfctp,l).eq.2)then
         write(98,9999)l,tfc(icomp,i,(l*2)-1),tfc(icomp,i,l*2),
     &    tfs(icomp,i),
     &   cfcRgap(icomp,i,l,2),qcfc_lw(icomp,i,(l*2)-1,2),
     &   qcfc_lw(icomp,i,l*2,2),qcfc_lw(icomp,i,(l*2)+1,2),0.,
     &   h_blind_roomS(icomp,i),h_glass_roomS(icomp,i),
     &   q_blind_airndS(icomp,i),q_glass_airndS(icomp,i),
     &   qcfc_cv(icomp,i,(l*2)-1,2),qcfc_cv(icomp,i,l*2,2),
     &   qcfc_cv(icomp,i,(l*2)+1,2)
         else
         write(98,9999)l,tfc(icomp,i,(l*2)-1),tfc(icomp,i,l*2),
     &    tfs(icomp,i),
     &   cfcRgap(icomp,i,l,2),qcfc_lw(icomp,i,(l*2)-1,2),
     &   qcfc_lw(icomp,i,l*2,2),qcfc_lw(icomp,i,(l*2)+1,2),0.,0.,
     &   0.,0.,0.,qcfc_cv(icomp,i,(l*2)-1,2),qcfc_cv(icomp,i,l*2,2),
     &   qcfc_cv(icomp,i,(l*2)+1,2)
         end if

      else
      !do nothing
      end if
 9999       format(6x,I12,1x,7F8.3,6F13.3,2F8.3)

 15   continue
      end if
 13   continue
C********************************************************************

C************************ Solar Processing **************************
      IF(ISUNUP.eq.1)then
      write(99,*)
     & 'TIMESTEP | ICOMP | ISUNUP |  SAZI  |  SALT  |  Tamb  |  Tzone'
      write(99,1000)NSINC,ICOMP,ISUNUP,SAZI,SALT,TP,TFA(ICOMP)

 1000     format(I6,I10,I7,3x,4F9.3)

      nc=nconst(icomp)

      do 16 i=1,nc

      if (icfcfl(icomp,i).gt.0)then
      icfctp=icfcfl(icomp,i)
      nn=nndc(icomp,i)
      ne=nelts(icomp,i)

      do 17 j=1,ne
      if(icfcltp(icomp,icfctp,j).eq.2)ivblayr=j
 17   continue

      write(99,1001)
     & '   CFC SURFACE |  PAZI  |  ANGI  |  PROANG  | IRadB |',
     & ' IRadD |IsShd? |TranBB | TranD | Refl  |',
     & 'VBLAYR|   W   |   S   |   PHI   | ORIENT |'
      write(99,1002) i,pazi(icomp,i),fInc_angle(i),
     & proang_s(icomp,icfctp,i),
     & RadIncDirS(icomp,i),RadIncDifS(icomp,i),
     & issur_self_shd(icomp,i),transBB_S(icomp,i),transD_S(icomp,i),
     & refl_S(icomp,i),ivblayr,vb_w(icomp,icfctp),
     & vb_s(icomp,icfctp),vb_phiNew(icomp,icfctp),
     & vb_VorH(icomp,icfctp)
 1001     format(A57,A41,A44)
 1002     format(6x,I10,3x,9f9.3,I5,2x,3F9.3,A8)
c 1002     format(I13,5F9.3,I6,3f9.3,I5,2x,3F9.3,A8)

      write(99,1003)
     &'     node  | qtmcaExt | qtmcaFin |'

 1003     format(4x,A40)

      do 18 l=1,nn

      write(99,1004)l,qtmca_ext(icomp,i,l),qtmca(i,l,2)

 1004       format(6x,I12,1x,2F11.3)

 18   continue
      end if
 16   continue
      END IF
C********************************************************************

      return
      end
