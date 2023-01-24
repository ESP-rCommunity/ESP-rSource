!! This file is part of the ESP-r system.
!! Copyright Energy Systems Research Unit, University of
!! Strathclyde, Glasgow Scotland, 2001-2013.

!! ESP-r is free software.  You can redistribute it and/or
!! modify it under the terms of the GNU General Public
!! License as published by the Free Software Foundation 
!! (version 2 or later).

!! ESP-r is distributed in the hope that it will be useful
!! but WITHOUT ANY WARRANTY; without even the implied
!! warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
!! PURPOSE. See the GNU General Public License for more
!! details.

!! Dynamic multi-segmented human thermal model
!! This code is developed based on the AUB bioheat model 

!! This human thermal model is developed and linked with
!! ESP-r tward the PhD research of Mohamad Rida.

!! This code needs further testing, there are some problems with it.
!! For the time being, the functionality to enable this is disabled in:
!! prjfmk.F, EDCAS, line 3880
!! esruprj.help, item prjfmk dynamicpeople_model

      SUBROUTINE segmentedocc(ICOMP,HMETA,Top,RHOCC,CLO)

include "building.h"
include "cfd.h"

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      real headflux1,trunkflux1,larmflux1,rarmflux1,llegflux1
      real rlegflux1,H2Oexp1,H2Oswt1,Qoccsens1,Qocclat1
      ! real COflux,Tsko_av1,Tco_av1,Tsk_av1
      COMMON/ocflux1/headflux1(MNZ),trunkflux1(MNZ),larmflux1(MNZ),&
        rarmflux1(MNZ),llegflux1(MNZ),rlegflux1(MNZ),H2Oexp1(MNZ),&
        H2Oswt1(MNZ),Qoccsens1(MCOM),Qocclat1(MCOM)
        ! COflux,Tsko_av1,Tco_av1,Tsk_av1
      real tavhead1,tavtrunk1,tavarml1,tavarmr1,tavlegl1,tavlegr1
      real hrhead,hrtrunk,hrarml,hrarmr,hrlegl,hrlegr,HMETA2
      integer cfdcheck,msegflag
      COMMON/segtemp1/tavhead1(MNZ),tavtrunk1(MNZ),tavarml1(MNZ),&
        tavarmr1(MNZ),tavlegl1(MNZ),tavlegr1(MNZ),hrhead(MNZ),&
        hrtrunk(MNZ),hrarml(MNZ),hrarmr(MNZ),hrlegl(MNZ),hrlegr(MNZ),&
        cfdcheck(MNZ),msegflag(MNZ),HMETA2(MNZ)
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF      
      common/stddbpath/standarddbpath
      character standarddbpath*72
      common/filep/ifil
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      character*72 LCFD
      REAL HRexp,Mresp
      REAL Top, RHOCC
      real CLO
      real, parameter :: T_STP=0.02 

! TODO: need to clarify exactly what this is, maybe use simulation time step.      
! time step is taken 0.02 though whatever the simulation time is defined by a user in ESP-r the human thermal model will be iterated on this time step.
      real sim_time
      real, parameter :: PI=3.14159
      character auxFilename*170
      character(len=4000) :: buffer
      integer :: ios = 0 
      integer :: count = 0, line_index = 0 
      Character(len = 30) :: start_identifier, stop_identifier

! The human body is divided into 25 segments, including ten fingers.
      integer, parameter :: L_F1=1
      integer, parameter :: R_F1=2
      integer, parameter :: L_F2=3
      integer, parameter :: R_F2=4
      integer, parameter :: L_F3=5
      integer, parameter :: R_F3=6
      integer, parameter :: L_F4=7
      integer, parameter :: R_F4=8
      integer, parameter :: L_F5=9
      integer, parameter :: R_F5=10
      integer, parameter :: PALM_L=11
      integer, parameter :: PALM_R=12
      integer, parameter :: FOREARM_L=13
      integer, parameter :: FOREARM_R=14
      integer, parameter :: UPPERARM_L=15
      integer, parameter :: UPPERARM_R=16
      integer, parameter :: FOOT_L=17
      integer, parameter :: FOOT_R=18
      integer, parameter :: CALF_L=19
      integer, parameter :: CALF_R=20
      integer, parameter :: THIGH_L=21
      integer, parameter :: THIGH_R=22
      integer, parameter :: HEAD=23
      integer, parameter :: L_CHEST=24
      integer, parameter :: U_CHEST=25

! Blood parameters.
      real, parameter :: Cp_blood=4000.0
      real, parameter :: ro_blood=1.05
      real, parameter :: k_blood=0.51944

      integer, parameter :: WOOL=1
      integer, parameter :: COTTON=2
      integer, parameter :: ACETATE=3
      integer, parameter :: NYLON=4
      integer, parameter :: ORLON=5
      integer, parameter :: POLYESTER=6
      integer, parameter :: OTHER=7
      integer, parameter :: N_FABRIC=11

! The model is structured into types that each has its parameters.
      type clothtype
            real Rth
            real Rev
            real thick
            real air_thick
            real radius
            real density
            real Cp
            real Ri_c
            real Ri1_c
            real Pi_c
            real Pi1_c
            real Ti_c
            real Ti1_c
            character*8 code
            integer material1
            integer material2
            real perc1
            real perc2
      end type
      
      type coretype
            real CoreCap ! capacitance
            real coreMet
            real mechwork ! mechanical work
            real coreTemp(3)
      end type

      type skintype
            real SkinCap ! capacitance
            real skinMet
            real SkinArea
            real skinTemp(3)
            real moistacc
            real OutSkTemp(3)
      end type

      type arterytype
            real radius
            real length
            integer index
      end type

      type arteriestype
            real length
            real radius
            real thick
            real E
            integer isterminal
            integer previous
            complex(8) charact_imp
            complex(8) input_imp
            complex(8) term_imp
            complex(8) gamma
            complex(8) ratio
            real mft
      end type
      
      type veintype
            real radius
            real length
            integer index
      end type

! Every type has unique values for each body segment.
      type bodysegtype
            type (coretype) core
            type (skintype) skin
            type (arterytype) artery(32)
            real Cbl_a
            type (veintype) vein(32)
            type (veintype) supvein(32)
            real Cbl_v
            real Cbl_vs
            integer n
            real mass
            real Kap
            real mperf
            real mskin
            real mskin_basal
            real mskin_maxcon
            real mskin_maxdil
            real SWEAT
            real THERMO
            real WORK
            real COLD
            real Ta(3)
            real Tv(3)
            real Tvs(3)
            real xma
            real xmv
            real xmvs
            real mperfv
            real fs_thick
            type (clothtype) layer(5)
            integer clothed
            integer n_layers
            real inner_radius
            real AVABflow
            real AVAstart
            real t_ava_local
            real sklat
            real sksens
      end type

! 25 body segments.
      type (bodysegtype) bodyseg(25)
      type (arteriestype) arteries(138)
      real, dimension(:,:), allocatable :: velocity
      real fic,ic(25,5)
      real ic_t(5)
      real :: Qress,Qresl,Mshiv,msw,Psk,Psat,CardOut,beatpermin,&
           mskin,sh,Regain,Phi,Sorption,AVAcoef,&
           AVA_control,AVA_cut
      real :: Kmuscle,Kfatskin

! TODO: check this.
! Ambient conditions are taken from ESP-r see later in the loop for each time step.
      real :: Op_T=20+273
      real :: atm_P=1.013e2
      real :: RH=0.30
      real :: METAo=1
      real :: Sat_amb_P
      real :: amb_vap_p
      real :: HR_amb
      real :: hc=3.1
      real :: hr
      real :: he
      integer :: i,j,seg,r,iin
      real :: x(25,16),z(138,7),heartbeat,m,met,cardiac,&
                         period,period1,cardiacout
      integer :: y(25,32)
      integer :: y_t(32)
      real :: x_t(16)
      real :: z_t(7)
      real :: Tskin_av(3)
      real :: Tskino_av(3)
      real :: Tcore_av(3)
      complex(8) :: ratios(138)
      real :: coreTemp,skinTemp,OutSkTemp,Ta,Tv,Tcrav,Tskav,Tvs
      real :: coreMet,shiv,mechwork,alpha,resp,Qcrsk,ha,hv,Aart,&
                          Avein,perfusion,CoreCap,Nu
      real :: Qconva,Qconvv,Qconvvs,shivt(3)
      integer :: k
      real :: Aveins,hvs,skinMet,SkinArea,Pskin,mblsk,SkinCap,&
                          eq_hc,eq_hr
      real :: sksenstotal,expo
      real :: sklattotal
      real :: Ca,xma,Taadj,time,v,arteryradius(138)
      real :: Cv,Cvs,xmv,xmvs,Tvsadj,Tvadj,mperfv
      character*8 :: codes(N_FABRIC)
      real :: fab_prop(N_FABRIC,9)
      character*8 :: codes_t
      real :: fab_prop_t(9)
      real :: air_thick
      real :: coef(6,5),coef_i(6,5)
      real :: rj_c,rj1_c,Re_c,Rd_c,Re1_c,&
              Rd1_c,P_c,T_c,P1_c,T1_c,P2_c,T2_c,&
              ro_c,th_c,phi_c,Cp_c,phi_cc

      real :: m1_R,m1_L,m2,m3,m4
      real :: mv1_R,mv1_L,mv2,mv3,mv4
      real Cotton_coef(5),Orlon_coef(5),Wool_coef(5),&
                       Polyester_coef(5),Acetate_coef(5),&
                       Nylon_coef(5),sorption_coef(7)
      data Cotton_coef /9.1866e-9,-1.2454e-006,4.7084e-005,&
                       0.00069586,0.0079181/
      data Wool_coef /1.6198e-008,-2.3252e-006,9.6392e-005,0.00080984&
                      ,0.016387/
      data Orlon_coef /0.0,1.1246e-007,-1.2451e-005,0.00054236,&
                      -0.0019394/
      data Polyester_coef /0.0,3.1418e-008,-4.5287e-006,0.00026399,&
                      0.0018463/
      data Acetate_coef /5.9288e-009,-8.9196e-007,4.6026e-005,&
                      -3.3354e-005,0.0049316/
      data Nylon_coef /1.9279e-009,-3.0765e-007,1.6808e-005,0.00030396&
                      ,0.0040585/
      real Cotton_coef_i(5),Orlon_coef_i(5),Wool_coef_i(5)&
                       ,Polyester_coef_i(5),Acetate_coef_i(5),&
                        Nylon_coef_i(5)
      data Cotton_coef_i /1.9993e+005,-89557,9444,583.44,-4.8188/
      data Wool_coef_i /31428,-23957,4673.4,230.73,-3.4354/
      data Orlon_coef_i /9.1118e+007,-5.8328e+006,10778,5452.6,2.2022/
      data Polyester_coef_i /2.6903e+009,-1.8793e+008,3.2938e+006,&
                         -10342,10.116 /
      data Acetate_coef_i /1.3023e+005,-29213,-3669.6,1436.6,-3.471/
      data Nylon_coef_i /2.8214e+005,-1.3573e+005,6094.8,1494.2,&
                       -3.6809/
      data sorption_coef /2.5313e-008,-9.1901e-006,0.0012923,&
                      -0.088246,3.0942,-65.827,3706/

      integer :: loop,il,ml

      ! If there is CFD active on this time step, determine which domain this zone is linked with.
      icfd=0
      if (NCONF.gt.0 .and. IFCFD(icomp).ne.0) then
        do iconf=1,NCONF
          if (ICFDND(iconf).eq.ICOMP) then
            icfd=iconf
            exit
          endif
        enddo
      endif
      write(6,*)'icfd',icfd

      iunit = ifil+1
      auxFilename = trim(standarddbpath) // trim('/multi_seg_occup_DB.txt')

      coef(1,:)=Wool_coef(:)
      coef(2,:)=Cotton_coef(:)
      coef(3,:)=Acetate_coef(:)
      coef(4,:)=Nylon_coef(:)
      coef(5,:)=Orlon_coef(:)
      coef(6,:)=Polyester_coef(:)

      coef_i(1,:)=Wool_coef_i(:)
      coef_i(2,:)=Cotton_coef_i(:)
      coef_i(3,:)=Acetate_coef_i(:)
      coef_i(4,:)=Nylon_coef_i(:)
      coef_i(5,:)=Orlon_coef_i(:)
      coef_i(6,:)=Polyester_coef_i(:)
      CLO=0.4
      if (CLO.LE.0.6) then
        do il=1,25
          if(il.le.10)then
            bodyseg(i)%inner_radius=10
            bodyseg(i)%n_layers=0
          elseif(i.eq.11.or.i.eq.12)then
            bodyseg(i)%inner_radius=25
            bodyseg(i)%n_layers=0
          elseif(i.eq.13.or.i.eq.14)then
            bodyseg(i)%inner_radius=37
            bodyseg(i)%n_layers=0
  !          bodyseg(i)%layer(1)%code="EF11"
  !          bodyseg(i)%layer(1)%air_thick=0.3
          elseif(i.eq.15.or.i.eq.16)then
            bodyseg(i)%inner_radius=45
            bodyseg(i)%n_layers=1
            bodyseg(i)%layer(1)%code="EF11"
            bodyseg(i)%layer(1)%air_thick=0.3
          elseif(i.eq.17.or.i.eq.18)then
            bodyseg(i)%inner_radius=36
            bodyseg(i)%n_layers=1
            bodyseg(i)%layer(1)%code="EF11"
            bodyseg(i)%layer(1)%air_thick=0.3
          elseif(i.eq.19.or.i.eq.20)then
            bodyseg(i)%inner_radius=43
            bodyseg(i)%n_layers=0
  !          bodyseg(i)%layer(1)%code="EF11"
  !          bodyseg(i)%layer(1)%air_thick=0.3
          elseif(i.eq.21.or.i.eq.22)then
            bodyseg(i)%inner_radius=67
            bodyseg(i)%n_layers=1
            bodyseg(i)%layer(1)%code="EF11"
            bodyseg(i)%layer(1)%air_thick=0.3
          elseif(i.eq.24.or.i.eq.25)then
            bodyseg(i)%inner_radius=130
            bodyseg(i)%n_layers=1
            bodyseg(i)%layer(1)%code="EF11"
            bodyseg(i)%layer(1)%air_thick=0.3
          elseif(i.eq.23)then
            bodyseg(i)%inner_radius=73
            bodyseg(i)%n_layers=0
          endif
          if (bodyseg(i)%n_layers==0) then
            bodyseg(i)%clothed=0
          else
            bodyseg(i)%clothed=1
          end if
        end do
      elseif (CLO.GT.0.6) then
        do i=1,25
          if(i.le.10)then
            bodyseg(i)%inner_radius=10
            bodyseg(i)%n_layers=0
          elseif(i.eq.11.or.i.eq.12)then
            bodyseg(i)%inner_radius=25
            bodyseg(i)%n_layers=0
          elseif(i.eq.13.or.i.eq.14)then
            bodyseg(i)%inner_radius=37
            bodyseg(i)%n_layers=1
            bodyseg(i)%layer(1)%code="EF11"
!            bodyseg(i)%layer(2)%code="EF11"
            bodyseg(i)%layer(1)%air_thick=0.3
!            bodyseg(i)%layer(2)%air_thick=0.3
          elseif(i.eq.15.or.i.eq.16)then
            bodyseg(i)%inner_radius=45
            bodyseg(i)%n_layers=1
            bodyseg(i)%layer(1)%code="EF11"
!            bodyseg(i)%layer(2)%code="EF11"
            bodyseg(i)%layer(1)%air_thick=0.3
!            bodyseg(i)%layer(2)%air_thick=0.3
          elseif(i.eq.17.or.i.eq.18)then
            bodyseg(i)%inner_radius=36
            bodyseg(i)%n_layers=2
            bodyseg(i)%layer(1)%code="EF11"
            bodyseg(i)%layer(2)%code="EF11"
          elseif(i.eq.19.or.i.eq.20)then
            bodyseg(i)%inner_radius=43
            bodyseg(i)%n_layers=1
            bodyseg(i)%layer(1)%code="EF11"
!            bodyseg(i)%layer(2)%code="EF11"
            bodyseg(i)%layer(1)%air_thick=0.3
!            bodyseg(i)%layer(2)%air_thick=0.3
          elseif(i.eq.21.or.i.eq.22)then
            bodyseg(i)%inner_radius=67
            bodyseg(i)%n_layers=2
            bodyseg(i)%layer(1)%code="EF11"
            bodyseg(i)%layer(2)%code="EF11"
            bodyseg(i)%layer(1)%air_thick=0.3
            bodyseg(i)%layer(2)%air_thick=0.3
          elseif(i.eq.24.or.i.eq.25)then
            bodyseg(i)%inner_radius=130
            bodyseg(i)%n_layers=2
            bodyseg(i)%layer(1)%code="EF11"
            bodyseg(i)%layer(2)%code="EF11"
            bodyseg(i)%layer(1)%air_thick=0.3
            bodyseg(i)%layer(2)%air_thick=0.3
          elseif(i.eq.23)then
            bodyseg(i)%inner_radius=73
            bodyseg(i)%n_layers=0
          endif
          if (bodyseg(i)%n_layers==0) then
            bodyseg(i)%clothed=0
          else
            bodyseg(i)%clothed=1
          end if
        end do
      endif

! Read fabric properties.
      start_identifier = "clothing fabric start"
      stop_identifier  = "clothing fabric end"
      ios = 0
      line_index = 0
      count = 0
      open(iunit, file=auxFilename)
      do while (ios == 0)
        read(iunit, '(A)', iostat=ios) buffer
        if (ios == 0) then
          if(buffer == stop_identifier) then
            count = 0
            EXIT
          endif
          if (count == 1) then
            line_index = line_index + 1
            read (buffer, *) fab_prop_t
            fab_prop(line_index, :) = fab_prop_t 
          endif
          if(buffer == start_identifier) then
            count =  1
          endif
        endif
      enddo
      close(iunit)

      start_identifier = "clothing fabric code start"
      stop_identifier  = "clothing fabric code end"
      ios = 0
      line_index = 0
      count = 0
      open(iunit, file=auxFilename)
      do while (ios == 0)
        read(iunit, '(A)', iostat=ios) buffer
        if (ios == 0) then
          if(buffer == stop_identifier) then
            count = 0
            EXIT
          endif
          if (count == 1) then
            line_index = line_index + 1
            read (buffer, *) codes_t
            codes(line_index) = codes_t 
          endif
          if(buffer == start_identifier) then
            count =  1
          endif
        endif
      enddo
      close(iunit)

      do i=1,25
        if (bodyseg(i)%clothed==1) then
          loop=bodyseg(il)%n_layers
          do j=1,loop
            do k=1,N_FABRIC
              if (bodyseg(i)%layer(j)%code==codes(k)) then
                bodyseg(i)%layer(j)%thick=fab_prop(k,1)
                bodyseg(i)%layer(j)%density=&
                fab_prop(k,2)/bodyseg(i)%layer(j)%thick
                bodyseg(i)%layer(j)%Rth=fab_prop(k,3)
                bodyseg(i)%layer(j)%Rev=fab_prop(k,4)
                bodyseg(i)%layer(j)%material1=&
                floor(fab_prop(k,5))
                bodyseg(i)%layer(j)%material2=&
                floor(fab_prop(k,6))
                bodyseg(i)%layer(j)%perc1=fab_prop(k,7)
                bodyseg(i)%layer(j)%perc2=fab_prop(k,8)
                bodyseg(i)%layer(j)%Cp=fab_prop(k,9)
                exit
              end if
            end do
          end do
        end if
      end do

! Calculate the radius of the outer fabric layer.
      do i=1,25
        if (bodyseg(i)%clothed==1) then
          bodyseg(i)%layer(1)%radius=bodyseg(i)%inner_radius+&
            bodyseg(i)%layer(1)%air_thick+bodyseg(i)%layer(1)%thick
          do j=2,bodyseg(i)%n_layers
            bodyseg(i)%layer(j)%radius=bodyseg(i)%layer(j-1)%radius+&
              bodyseg(i)%layer(j)%air_thick+bodyseg(i)%layer(j)%thick
          end do
        end if
      end do

! Read body parameters.
      start_identifier = "body_parameters start"
      stop_identifier  = "body_parameters end"
      ios = 0
      line_index = 0
      count = 0
      open(iunit, file=auxFilename)
      do while (ios == 0)
        read(iunit, '(A)', iostat=ios) buffer
        if (ios == 0) then
          if(buffer == stop_identifier) then
            count = 0
            EXIT
          endif
          if (count == 1) then
            line_index = line_index + 1
            read (buffer, *) x_t
            x(line_index, :) =x_t 
          endif
          if(buffer == start_identifier) then
            count =  1
          endif
        endif
      enddo
      close(iunit)

      do i=1,25
        bodyseg(i)%mass=x(i,1)
        bodyseg(i)%skin%SkinArea=x(i,2)
        bodyseg(i)%core%coreMet=x(i,3)
        bodyseg(i)%skin%skinMet=x(i,4)
        bodyseg(i)%core%CoreCap=x(i,5)
        bodyseg(i)%skin%SkinCap=x(i,6)
        bodyseg(i)%Cbl_a=x(i,7)
        bodyseg(i)%Cbl_v=x(i,8)
        bodyseg(i)%mskin_basal=x(i,9)
        bodyseg(i)%mskin_maxcon=x(i,10)
        bodyseg(i)%mskin_maxdil=x(i,11)
        bodyseg(i)%SWEAT=x(i,12)
        bodyseg(i)%THERMO=x(i,13)
        bodyseg(i)%WORK=x(i,14)
        bodyseg(i)%COLD=x(i,15)
        bodyseg(i)%fs_thick=x(i,16)
        bodyseg(i)%core%mechwork=bodyseg(i)%core%coreMet*0.05
      end do

! Number of arteries and veins in each body segment.
      bodyseg(HEAD)%n=27
      bodyseg(U_CHEST)%n=32
      bodyseg(L_CHEST)%n=19
      bodyseg(UPPERARM_L)%n=7
      bodyseg(UPPERARM_R)%n=7
      bodyseg(FOREARM_L)%n=5
      bodyseg(FOREARM_R)%n=5
      bodyseg(PALM_L)%n=1
      bodyseg(PALM_R)%n=1
      bodyseg(THIGH_L)%n=4
      bodyseg(THIGH_R)%n=4
      bodyseg(CALF_L)%n=6
      bodyseg(CALF_R)%n=6
      bodyseg(FOOT_L)%n=2
      bodyseg(FOOT_R)%n=2
      bodyseg(L_F1)%n=1
      bodyseg(R_F1)%n=1
      bodyseg(L_F2)%n=1
      bodyseg(R_F2)%n=1
      bodyseg(L_F3)%n=1
      bodyseg(R_F3)%n=1
      bodyseg(L_F4)%n=1
      bodyseg(R_F4)%n=1
      bodyseg(L_F5)%n=1
      bodyseg(R_F5)%n=1

! Read artery data.
      start_identifier = "arteries_index start"
      stop_identifier  = "arteries_index end"
      ios = 0
      line_index = 0
      count = 0
      open(iunit, file=auxFilename)
      do while (ios == 0)
        read(iunit, '(A)', iostat=ios) buffer
        if (ios == 0) then
          if(buffer == stop_identifier) then
            count = 0
            EXIT
          endif
          if (count == 1) then
            line_index = line_index + 1
            read (buffer, *) y_t
            y(line_index, :) = y_t 
          endif
          if(buffer == start_identifier) then
            count =  1
          endif
        endif
      enddo
      close(iunit)
      do i=1,25
        do j=1,bodyseg(i)%n
          bodyseg(i)%artery(j)%index=y(i,j)
          bodyseg(i)%vein(j)%index=y(i,j)
        end do
      end do

      start_identifier = "arteries_param start"
      stop_identifier  = "arteries_param end"
      ios = 0
      line_index = 0
      count = 0
      open(iunit, file=auxFilename)
      do while (ios == 0)
        read(iunit, '(A)', iostat=ios) buffer
        if (ios == 0) then
          if(buffer == stop_identifier) then
            count = 0
            EXIT
          endif
          if (count == 1) then
            line_index = line_index + 1
            read (buffer, *) z_t
            z(line_index, :) =z_t 
          endif
          if(buffer == start_identifier) then
            count =  1
          endif
        endif
      enddo
      close(iunit)
      do i=1,138
        arteries(int(z(i,1)))%length=z(i,2)
        arteries(int(z(i,1)))%radius=z(i,3)
        arteries(int(z(i,1)))%thick=z(i,4)
        arteries(int(z(i,1)))%E=z(i,5)*1e6
        arteries(int(z(i,1)))%isterminal=int(z(i,6))
        arteries(int(z(i,1)))%previous=int(z(i,7))
      end do

! Read initial conditions.
      write(6,*)'msegflag(icfd)',msegflag(icfd)
      if (icfd.gt.0) then
        if (msegflag(icfd)==0) then
          start_identifier = "Initial condition1 start"
          stop_identifier  = "Initial condition1 end"
          ios = 0
          line_index = 0
          count = 0
          open(iunit, file=auxFilename)
          do while (ios == 0)
            read(iunit, '(A)', iostat=ios) buffer
            if (ios == 0) then
              if(buffer == stop_identifier) then
                count = 0
                EXIT
              endif
              if (count == 1) then
                line_index = line_index + 1
                read (buffer, *) ic_t
                  ic(line_index, :) =ic_t 
              endif
              if(buffer == start_identifier) then
                count =  1
              endif
            endif
          enddo
          close(iunit)

          write(6,*)'ic1',ic

          do i=1,25
            bodyseg(i)%core%coreTemp(1)=ic(i,1)
            bodyseg(i)%skin%skinTemp(1)=ic(i,2)
            bodyseg(i)%skin%OutSkTemp(1)=2*ic(i,2)-ic(i,1)
            bodyseg(i)%Ta(1)=ic(i,3)
            bodyseg(i)%Tv(1)=ic(i,4)
            bodyseg(i)%Tvs(1)=ic(i,2)
          end do
        endif

        if (msegflag(icfd)==1) then
          open(unit=iunit,file="initial_condition1.txt",status="UNKNOWN")
          do i=1,25
            read (iunit,*) (ic(i,j),j=1,4)
          end do

          write(6,*)'ic2',ic

          do i=1,25
            bodyseg(i)%core%coreTemp(1)=ic(i,1)
            bodyseg(i)%skin%skinTemp(1)=ic(i,2)
            bodyseg(i)%skin%OutSkTemp(1)=2*ic(i,2)-ic(i,1)
            bodyseg(i)%Ta(1)=ic(i,3)
            bodyseg(i)%Tv(1)=ic(i,4)
            bodyseg(i)%Tvs(1)=ic(i,2)
          end do
          close(unit=iunit)
        endif
      endif

      do j=1,25
        if (bodyseg(j)%clothed==1) then
          do i=1,bodyseg(j)%n_layers
            bodyseg(j)%layer(i)%Ti_c=Op_T-273.0
            bodyseg(j)%layer(i)%Pi_c=amb_vap_p
            bodyseg(j)%layer(i)%Ri_c=Regain(RH*100,&
              coef(bodyseg(j)%layer(i)%material1,:),&
              coef(bodyseg(j)%layer(i)%material2,:),&
              bodyseg(j)%layer(i)%perc1,bodyseg(j)%layer(i)%perc2)
          end do
        end if
      end do
      do i=1,25
        bodyseg(i)%skin%moistacc=0.0
      end do
      Tcore_av(1)=0.0
      Tskin_av(1)=0.0
      do i=1,25
        Tskin_av(1)=Tskin_av(1)+bodyseg(i)%skin%skinTemp(1)*bodyseg(i)%THERMO
        Tcore_av(1)=Tcore_av(1)+bodyseg(i)%core%coreTemp(1)*bodyseg(i)%THERMO
      end do
      cardiac=CardOut(Tcore_av(1),Tskin_av(1))
      met=0.0
      AVA_control = cardiac
      AVA_cut = 320000

      do i=1,25
        met=met+HMETA*bodyseg(i)%core%coreMet+HMETA*bodyseg(i)%skin%skinMet
        t_ava_local=0
        bodyseg(i)%AVABflow = 0;
      end do
      sh=Mshiv(Tcore_av(1),Tskin_av(1),real(1.0))
      shivt(1)=sh
      heartbeat=beatpermin(met,cardiac,sh)
      cardiacout=cardiac*(met)/met

!      print*, cardiac
      if (cardiac <= 427500) then
        arteries(57)%radius=sqrt((0.3726**2-0.2178**2)*(cardiac-&
          270000)/(427500-270000)+0.2178**2)
        arteries(42)%radius=arteries(57)%radius
        arteries(85)%radius=sqrt((0.268**2-0.1977**2)*(cardiac-&
          270000)/(427500-270000)+0.1977**2)
        arteries(81)%radius=arteries(85)%radius
        arteries(108)%radius=sqrt((0.255**2-0.1911**2)*(cardiac-&
          270000)/(427500-270000)+0.1911**2)
        arteries(103)%radius=arteries(108)%radius
        arteries(129)%radius=sqrt((0.085**2-0.0427**2)*&
          (bodyseg(L_F1)%skin%OutSkTemp(1)-12)/(35-12)+0.0427**2)
        if (bodyseg(L_F1)%skin%OutSkTemp(1)>35) then
          arteries(129)%radius=0.085
        else if (bodyseg(L_F1)%skin%OutSkTemp(1)<12) then
          arteries(129)%radius=0.0427
        end if
        arteries(134)%radius=sqrt((0.085**2-0.0427**2)*&
          (bodyseg(R_F1)%skin%OutSkTemp(1)-12)/(35-12)+0.0427**2)
        if (bodyseg(R_F1)%skin%OutSkTemp(1)>35) then
          arteries(134)%radius=0.085
        else if (bodyseg(R_F1)%skin%OutSkTemp(1)<12) then
          arteries(134)%radius=0.0427
        end if
        arteries(130)%radius=sqrt((0.085**2-0.0427**2)*&
          (bodyseg(L_F2)%skin%OutSkTemp(1)-12)/(35-12)+0.0427**2)
        if (bodyseg(L_F2)%skin%OutSkTemp(1)>35) then
          arteries(130)%radius=0.085
        else if (bodyseg(L_F2)%skin%OutSkTemp(1)<12) then
          arteries(130)%radius=0.0427
        end if
        arteries(135)%radius=sqrt((0.085**2-0.0427**2)*&
          (bodyseg(R_F2)%skin%OutSkTemp(1)-12)/(35-12)+0.0427**2)
        if (bodyseg(R_F2)%skin%OutSkTemp(1)>35) then
          arteries(135)%radius=0.085
        else if (bodyseg(R_F2)%skin%OutSkTemp(1)<12) then
          arteries(135)%radius=0.0427
        end if
        arteries(131)%radius=sqrt((0.085**2-0.0427**2)*&
          (bodyseg(L_F3)%skin%OutSkTemp(1)-12)/(35-12)+0.0427**2)
        if (bodyseg(L_F3)%skin%OutSkTemp(1)>35) then
          arteries(131)%radius=0.085
        else if (bodyseg(L_F3)%skin%OutSkTemp(1)<12) then
          arteries(131)%radius=0.0427
        end if
        arteries(136)%radius=sqrt((0.085**2-0.0427**2)*&
          (bodyseg(R_F3)%skin%OutSkTemp(1)-12)/(35-12)+0.0427**2)
        if (bodyseg(R_F3)%skin%OutSkTemp(1)>35) then
          arteries(136)%radius=0.085
        else if (bodyseg(R_F3)%skin%OutSkTemp(1)<12) then
          arteries(136)%radius=0.0427
        end if
        arteries(132)%radius=sqrt((0.085**2-0.0427**2)*&
          (bodyseg(L_F4)%skin%OutSkTemp(1)-12)/(35-12)+0.0427**2)
        if (bodyseg(L_F4)%skin%OutSkTemp(1)>35) then
          arteries(132)%radius=0.085
        else if (bodyseg(L_F4)%skin%OutSkTemp(1)<12) then
          arteries(132)%radius=0.0427
        end if
        arteries(137)%radius=sqrt((0.085**2-0.0427**2)*&
          (bodyseg(R_F4)%skin%OutSkTemp(1)-12)/(35-12)+0.0427**2)
        if (bodyseg(R_F4)%skin%OutSkTemp(1)>35) then
          arteries(137)%radius=0.085
        else if (bodyseg(R_F4)%skin%OutSkTemp(1)<12) then
          arteries(137)%radius=0.0427
        end if
        arteries(133)%radius=sqrt((0.085**2-0.0427**2)*&
          (bodyseg(L_F5)%skin%OutSkTemp(1)-12)/(35-12)+0.0427**2)
        if (bodyseg(L_F5)%skin%OutSkTemp(1)>35) then
          arteries(133)%radius=0.085
        else if (bodyseg(L_F5)%skin%OutSkTemp(1)<12) then
          arteries(133)%radius=0.0427
        end if
        arteries(138)%radius=sqrt((0.085**2-0.0427**2)*&
          (bodyseg(R_F5)%skin%OutSkTemp(1)-12)/(35-12)+0.0427**2)
        if (bodyseg(R_F5)%skin%OutSkTemp(1)>35) then
          arteries(138)%radius=0.08
        else if (bodyseg(R_F5)%skin%OutSkTemp(1)<12) then
          arteries(138)%radius=0.0427
        end if
        arteries(92)%radius=sqrt((0.429**2-0.248**2)*(cardiac-270000)&
          /(427500-270000)+0.248**2)
        arteries(89)%radius=arteries(92)%radius
        arteries(113)%radius=sqrt((0.3**2-0.145**2)*(cardiac-270000)&
          /(427500-270000)+0.145**2)
        arteries(114)%radius=arteries(113)%radius
        arteries(127)%radius=sqrt((0.2308**2-0.1015**2)*(cardiac-270000)&
          /(427500-270000)+0.1015**2)
        arteries(126)%radius=arteries(127)%radius
        arteries(12)%radius=sqrt((0.4**2-0.4625**2)*(cardiac-270000)&
          /(427500-270000)+0.4625**2)
        arteries(4)%radius=arteries(12)%radius
      else
        arteries(57)%radius=0.3726
        arteries(42)%radius=arteries(57)%radius
        arteries(85)%radius=0.268
        arteries(81)%radius=arteries(85)%radius
        arteries(108)%radius=0.255
        arteries(103)%radius=arteries(108)%radius
        do i=129,138
          arteries(i)%radius=0.085
        end do
        arteries(92)%radius=0.429
        arteries(89)%radius=arteries(92)%radius
        arteries(113)%radius=0.3
        arteries(114)%radius=arteries(113)%radius
        arteries(127)%radius=0.2308
        arteries(126)%radius=arteries(127)%radius
        arteries(12)%radius=0.4
        arteries(4)%radius=arteries(12)%radius
      end if

      do i=1,25
        do j=1,bodyseg(i)%n
          bodyseg(i)%artery(j)%radius=arteries&
            (bodyseg(i)%vein(j)%index)%radius
          bodyseg(i)%artery(j)%length=arteries&
            (bodyseg(i)%vein(j)%index)%length
          bodyseg(i)%vein(j)%length=arteries&
            (bodyseg(i)%vein(j)%index)%length
          if (i<15) then
            bodyseg(i)%vein(j)%radius=1.75*arteries&
              (bodyseg(i)%vein(j)%index)%radius
            bodyseg(i)%supvein(j)%radius=1.75*arteries&
              (bodyseg(i)%vein(j)%index)%radius
            bodyseg(i)%supvein(j)%length=arteries&
              (bodyseg(i)%vein(j)%index)%length
          else
            bodyseg(i)%vein(j)%radius=1.75*arteries&
              (bodyseg(i)%vein(j)%index)%radius
            bodyseg(i)%supvein(j)%radius=1.75*arteries&
              (bodyseg(i)%vein(j)%index)%radius
            bodyseg(i)%supvein(j)%length=arteries&
              (bodyseg(i)%vein(j)%index)%length
          end if
        end do
      end do

      do i=1,138
        arteryradius(i)=arteries(i)%radius
      end do

      call bloodtree(heartbeat,ratios,fic,arteryradius,AVA_control)
      allocate(velocity(2,int(60.0/heartbeat/T_STP)+1))

      call four(ratios(42),heartbeat,T_STP,velocity,m1_R,cardiacout)
      mv1_R=m1_R*PI*(arteries(42)%radius)**2
      call four(ratios(57),heartbeat,T_STP,velocity,m1_L,cardiacout)
      mv1_L=m1_L*PI*(arteries(57)%radius)**2
      call four(ratios(4),heartbeat,T_STP,velocity,m2,cardiacout)
      mv2=m2*PI*(arteries(4)%radius)**2
      call four(ratios(12),heartbeat,T_STP,velocity,m3,cardiacout)
      mv3=m3*PI*(arteries(12)%radius)**2
      call four(ratios(34),heartbeat,T_STP,velocity,m4,cardiacout)
      mv4=m4*PI*(arteries(34)%radius)**2

      deallocate(velocity)
      sim_time=3600/(NTSTEP)
      he=16.5*hc
      time=0.0
      period=0.0
      period1=0.0

      do i=1,int(sim_time/T_STP)
        Op_T=Top + 273
        RH=RHOCC
        Sat_amb_P=Psat(Op_T)
        amb_vap_p=RH*Sat_amb_P
        HR_amb=0.622*amb_vap_p/(atm_P-amb_vap_p)
        METAo=HMETA !META(ICOMP,IDAY,IGN,IPER)*58*1.8
        allocate(velocity(2,int(60.0/heartbeat/T_STP)+1))

        do seg=1,25
          if (i.eq.1)then
            bodyseg(seg)%core%coreTemp(2)=bodyseg(seg)%core%coreTemp(1)
            bodyseg(seg)%skin%skinTemp(2)=bodyseg(seg)%skin%skinTemp(1)
            bodyseg(seg)%skin%OutSkTemp(2)=bodyseg(seg)%skin%OutSkTemp(1)
            bodyseg(seg)%Ta(2)=bodyseg(seg)%Ta(1)
            bodyseg(seg)%Tv(2)=bodyseg(seg)%Tv(1)
            bodyseg(seg)%Tvs(2)=bodyseg(seg)%Tvs(1)
            Tcore_av(2)=Tcore_av(1)
            Tskin_av(2)=Tskin_av(1)
          end if
          if (icfd.gt.0) then
            if (cfdcheck(icfd).GT.1) then
              if(seg==HEAD)then
                Op_T=tavhead1(icfd) + 273
              elseif(seg.GT.23)then
                Op_T=tavtrunk1(icfd) + 273
  !                RH=RHOCC
              elseif(seg==1.or.seg==3.or.seg==5.or.seg==7.or.seg==9&
                .or.seg==11.or.seg==13.or.seg==15)then
                Op_T=tavarml1(icfd) + 273
  !                RH=RHOCC
              elseif(seg==2.or.seg==4.or.seg==6.or.seg==8.or.seg==10&
                .or.seg==12.or.seg==14.or.seg==16)then
                Op_T=tavarmr1(icfd) + 273
  !                RH=RHOCC
              elseif(seg==17.or.seg==19.or.seg==21)then
                Op_T=tavlegl1(icfd) + 273
  !                RH=RHOCC
              elseif(seg==18.or.seg==20.or.seg==22)then
                Op_T=tavlegr1(icfd) + 273
  !                RH=RHOCC
              endif
            endif
          endif
          RH=0.5
          Sat_amb_P=Psat(Op_T)
          amb_vap_p=RH*Sat_amb_P
          HR_amb=0.622*amb_vap_p/(atm_P-amb_vap_p)
          METAo=HMETA
          if (seg>23 .or. seg==THIGH_L .or. seg==THIGH_R) then
            expo=1.333
          else
            expo=1.0
          end if
          CoreCap=bodyseg(seg)%core%CoreCap
          coreMet=METAo*bodyseg(seg)%core%coreMet
          Tcrav=Tcore_av(2)
          Tskav=Tskin_av(2)
          shiv=Mshiv(Tcrav,Tskav,bodyseg(seg)%COLD)
          mechwork=bodyseg(seg)%core%mechwork
          if (seg.eq.24) then
            alpha=1.0
          else
            alpha=0.0
          end if
          resp=Qress(met,Op_T-273.0,HR_amb)+Qresl(met,Op_T-273.0,HR_amb)
          Kmuscle=(1.0/0.05)*bodyseg(seg)%skin%SkinArea
          Kfatskin=bodyseg(seg)%skin%SkinArea/&
            ((bodyseg(seg)%fs_thick-2.0)*0.0048+0.0044)
          Kap=1.0/(1.0/Kmuscle+1.0/Kfatskin)
          coreTemp=bodyseg(seg)%core%coreTemp(2)
          skinTemp=bodyseg(seg)%skin%skinTemp(2)
          OutSkTemp=bodyseg(seg)%skin%OutSkTemp(2)
          Ta=bodyseg(seg)%Ta(2)
          Tv=bodyseg(seg)%Tv(2)
          Tvs=bodyseg(seg)%Tvs(2)
          Qcrsk=Kap*(coreTemp-skinTemp)
          Qconva=0.0
          Qconvv=0.0
          Qconvvs=0.0
          do j=1,bodyseg(seg)%n
            Nu=3.66
            ha=Nu*k_blood/bodyseg(seg)%artery(j)%radius/100.0/2.0
            Aart=2*PI*bodyseg(seg)%artery(j)%radius*bodyseg(seg)%artery(j)%length/1e4
            hv=Nu*k_blood/bodyseg(seg)%vein(j)%radius/100.0/2.0
            hvs=Nu*k_blood/bodyseg(seg)%supvein(j)%radius/100.0/2.0
            Avein=2*PI*bodyseg(seg)%vein(j)%radius*bodyseg(seg)%vein(j)%length/1e4
            Aveins=2*PI*bodyseg(seg)%supvein(j)%radius*bodyseg(seg)%supvein(j)%length/1e4
            Qconva=Qconva+ha*Aart*(coreTemp-Ta)
            Qconvv=Qconvv+hv*Avein*(coreTemp-Tv)
            Qconvvs=Qconvvs+hvs*Aveins*(Tvs-skinTemp)
          end do

          if (seg < 13) then
            if (bodyseg(seg)%skin%OutSkTemp(2)<12 .and. &
              bodyseg(seg)%skin%OutSkTemp(1)>12 .and. &
              time>120 .and. Tcore_av(2)<=37.5) then
              bodyseg(seg)%t_ava_local = time
              bodyseg(seg)%AVAstart = bodyseg(seg)%AVABflow
            else if (bodyseg(seg)%skin%OutSkTemp(2)>15 .and. &
              bodyseg(seg)%skin%OutSkTemp(1)<=15 .and. &
              time>120 .and. Tcore_av(2)<=37.5) then
              bodyseg(seg)%t_ava_local = time
              bodyseg(seg)%AVAstart = bodyseg(seg)%AVABflow
            else if (bodyseg(seg)%skin%OutSkTemp(2)<33.2 .and. &
              bodyseg(seg)%skin%OutSkTemp(1)>=33.2 .and. &
              time>120 .and. Tcore_av(2)<=37.5) then
              bodyseg(seg)%t_ava_local = time
              bodyseg(seg)%AVAstart = bodyseg(seg)%AVABflow
            else if (bodyseg(seg)%skin%OutSkTemp(2)>33.2 .and. &
              bodyseg(seg)%skin%OutSkTemp(1)<=33.2 .and. &
              time>120 .and.Tcore_av(2)<=37.5) then
              bodyseg(seg)%t_ava_local = time
              bodyseg(seg)%AVAstart = bodyseg(seg)%AVABflow
            else if (bodyseg(seg)%skin%OutSkTemp(2)<33.2 .and. &
              Tcore_av(2)<37.5 .and. Tcore_av(1)>=37.5) then
              bodyseg(seg)%t_ava_local = time
              bodyseg(seg)%AVAstart = bodyseg(seg)%AVABflow
            else if (time<=120) then
              bodyseg(seg)%t_ava_local = 0
              bodyseg(seg)%AVAstart = bodyseg(seg)%AVABflow
            end if
          end if

          ! Calculate the total perfusion rates and skin blood flows for each member.
          ! Each body part has a unique number of arteries.
          ! The total volumetric flow rate of blood through each artery is calculated in cm3/s.
          Ca=bodyseg(seg)%Cbl_a

          select case (seg)
          case (HEAD)
            call four(ratios(4),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(4)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=xma+v*PI*arteries(4)%radius**2
            if (period1>(60.0/heartbeat)) then
              perfusion=0.0
              perfusion=perfusion+m*PI*arteries(4)%radius**2
            end if
            call four(ratios(12),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(12)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=xma+v*PI*arteries(12)%radius**2
            Taadj=bodyseg(U_CHEST)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=perfusion+m*PI*arteries(12)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(HEAD)%mskin_basal,&
                bodyseg(HEAD)%mskin_maxcon,bodyseg(HEAD)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (U_CHEST)
            call four(ratios(1),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(1)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(1)%radius**2
            Taadj=bodyseg(U_CHEST)%Tv(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(1)%radius**2
              perfusion=perfusion-arteries(42)%mft*PI*arteries(42)%radius**2
              perfusion=perfusion-arteries(57)%mft*PI*arteries(42)%radius**2
              perfusion=perfusion-arteries(4)%mft*PI*arteries(4)%radius**2
              perfusion=perfusion-arteries(12)%mft*PI*arteries(12)%radius**2
              perfusion=perfusion-arteries(34)%mft*PI*arteries(34)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(U_CHEST)%mskin_basal,&
                bodyseg(U_CHEST)%mskin_maxcon,bodyseg(U_CHEST)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (L_CHEST)
            call four(ratios(34),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(34)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(34)%radius**2
            Taadj=bodyseg(U_CHEST)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(34)%radius**2
              perfusion=perfusion-arteries(89)%mft*PI*arteries(89)%radius**2
              perfusion=perfusion-arteries(92)%mft*PI*arteries(89)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(L_CHEST)%mskin_basal,&
                bodyseg(L_CHEST)%mskin_maxcon,bodyseg(L_CHEST)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (UPPERARM_L)
            call four(ratios(57),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(57)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(57)%radius**2
            Taadj=bodyseg(U_CHEST)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(57)%radius**2
              perfusion=perfusion-arteries(85)%mft*PI*arteries(85)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(UPPERARM_L)%mskin_basal,&
                bodyseg(UPPERARM_L)%mskin_maxcon,bodyseg(UPPERARM_L)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (UPPERARM_R)
            call four(ratios(42),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(42)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(42)%radius**2
            Taadj=bodyseg(U_CHEST)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(42)%radius**2
              perfusion=perfusion-arteries(81)%mft*PI*arteries(81)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(UPPERARM_R)%mskin_basal,&
                bodyseg(UPPERARM_R)%mskin_maxcon,bodyseg(UPPERARM_R)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (FOREARM_L)
            call four(ratios(85),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(85)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(85)%radius**2
            Taadj=bodyseg(UPPERARM_L)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(85)%radius**2
              perfusion=perfusion-arteries(108)%mft*PI*arteries(108)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(FOREARM_L)%mskin_basal,&
                bodyseg(FOREARM_L)%mskin_maxcon,bodyseg(FOREARM_L)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%AVABflow = 0
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (FOREARM_R)
            call four(ratios(81),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(81)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(81)%radius**2
            Taadj=bodyseg(UPPERARM_R)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(81)%radius**2
              perfusion=perfusion-arteries(103)%mft*PI*arteries(103)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(FOREARM_R)%mskin_basal,&
                bodyseg(FOREARM_R)%mskin_maxcon,bodyseg(FOREARM_R)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              bodyseg(seg)%AVABflow = 0
            end if
          case (PALM_L)
            call four(ratios(108),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(108)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(108)%radius**2
            Taadj=bodyseg(FOREARM_L)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(108)%radius**2
              perfusion=perfusion-arteries(129)%mft*PI*arteries(129)%radius**2
              perfusion=perfusion-arteries(130)%mft*PI*arteries(130)%radius**2
              perfusion=perfusion-arteries(131)%mft*PI*arteries(131)%radius**2
              perfusion=perfusion-arteries(132)%mft*PI*arteries(132)%radius**2
              perfusion=perfusion-arteries(133)%mft*PI*arteries(133)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(PALM_L)%mskin_basal,&
                bodyseg(PALM_L)%mskin_maxcon,bodyseg(PALM_L)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (AVA_control>=AVA_cut) then
                bodyseg(seg)%AVABflow = 0
              else
                bodyseg(seg)%AVABflow = 0
              end if
            end if
          case (PALM_R)
            call four(ratios(103),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(103)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(103)%radius**2
            Taadj=bodyseg(FOREARM_R)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(103)%radius**2
              perfusion=perfusion-arteries(134)%mft*PI*arteries(134)%radius**2
              perfusion=perfusion-arteries(135)%mft*PI*arteries(135)%radius**2
              perfusion=perfusion-arteries(136)%mft*PI*arteries(136)%radius**2
              perfusion=perfusion-arteries(137)%mft*PI*arteries(137)%radius**2
              perfusion=perfusion-arteries(138)%mft*PI*arteries(138)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(PALM_R)%mskin_basal,&
                bodyseg(PALM_R)%mskin_maxcon,bodyseg(PALM_R)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (AVA_control>=AVA_cut) then
                bodyseg(seg)%AVABflow = 0
              else
                bodyseg(seg)%AVABflow = 0
              end if
            end if
          case (THIGH_L)
            call four(ratios(92),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(92)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(92)%radius**2
            Taadj=bodyseg(L_CHEST)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(92)%radius**2
              perfusion=perfusion-arteries(114)%mft*PI*arteries(114)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(THIGH_L)%mskin_basal,&
                bodyseg(THIGH_L)%mskin_maxcon,bodyseg(THIGH_L)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (THIGH_R)
            call four(ratios(89),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(89)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(89)%radius**2
            Taadj=bodyseg(L_CHEST)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(89)%radius**2
              perfusion=perfusion-arteries(113)%mft*PI*arteries(113)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(THIGH_R)%mskin_basal,&
                bodyseg(THIGH_R)%mskin_maxcon,bodyseg(THIGH_R)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (CALF_L)
            call four(ratios(114),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(114)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(114)%radius**2
            Taadj=bodyseg(THIGH_L)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(114)%radius**2
              perfusion=perfusion-arteries(127)%mft*PI*arteries(127)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(CALF_L)%mskin_basal,&
                bodyseg(CALF_L)%mskin_maxcon,bodyseg(CALF_L)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (CALF_R)
            call four(ratios(113),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(113)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(113)%radius**2
            Taadj=bodyseg(THIGH_R)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(113)%radius**2
              perfusion=perfusion-arteries(126)%mft*PI*arteries(126)%radius**2
              mblsk=ro_blood*Mskin(perfusion,Tcrav,Tskav,bodyseg(CALF_R)%mskin_basal,&
                bodyseg(CALF_R)%mskin_maxcon,bodyseg(CALF_R)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (FOOT_L)
            call four(ratios(127),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(127)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(127)%radius**2
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(127)%radius**2
            end if
            call four(ratios(128),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(128)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=xma+v*PI*arteries(128)%radius**2
            Taadj=bodyseg(CALF_L)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=perfusion+m*PI*arteries(128)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,skinTemp,bodyseg(FOOT_L)%mskin_basal,&
                bodyseg(FOOT_L)%mskin_maxcon,bodyseg(FOOT_L)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (FOOT_R)
            call four(ratios(125),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(125)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(125)%radius**2
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(125)%radius**2
            end if
            call four(ratios(126),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(126)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=xma+v*PI*arteries(126)%radius**2
            Taadj=bodyseg(CALF_R)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=perfusion+m*PI*arteries(126)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,skinTemp,bodyseg(FOOT_R)%mskin_basal,&
                bodyseg(FOOT_R)%mskin_maxcon,bodyseg(FOOT_R)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
            end if
          case (L_F1)
            call four(ratios(129),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(129)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(129)%radius**2
            Taadj=bodyseg(PALM_L)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(129)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,skinTemp,bodyseg(L_F1)%mskin_basal,&
                bodyseg(L_F1)%mskin_maxcon,bodyseg(L_F1)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (Tcore_av(2)>37.5) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .or. (bodyseg(seg)%skin%OutSkTemp(2)<32&
                .and. time<=120)) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if ( Tcore_av(2)<36.7 .or. &
                bodyseg(seg)%skin%OutSkTemp(2)<=32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              end if
            end if
          case (R_F1)
            call four(ratios(134),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(134)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(134)%radius**2
            Taadj=bodyseg(PALM_R)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(134)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(R_F1)%mskin_basal,&
                bodyseg(R_F1)%mskin_maxcon,bodyseg(R_F1)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (Tcore_av(2)>37.5) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .or. (bodyseg(seg)%skin%OutSkTemp(2)<32&
                .and. time<=120)) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if ( Tcore_av(2)<36.7 .or. &
                bodyseg(seg)%skin%OutSkTemp(2)<=32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              end if
            end if
          case (L_F2)
            call four(ratios(130),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(130)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(130)%radius**2
            Taadj=bodyseg(PALM_L)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(130)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(L_F2)%mskin_basal,&
                bodyseg(L_F2)%mskin_maxcon,bodyseg(L_F2)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (Tcore_av(2)>37.5) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .or. (bodyseg(seg)%skin%OutSkTemp(2)<32&
                .and. time<=120)) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if ( Tcore_av(2)<36.7 .or. &
                bodyseg(seg)%skin%OutSkTemp(2)<=32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              end if
            end if
          case (R_F2)
            call four(ratios(135),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(135)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(135)%radius**2
            Taadj=bodyseg(PALM_R)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(135)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(R_F2)%mskin_basal,&
                bodyseg(R_F2)%mskin_maxcon,bodyseg(R_F2)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (Tcore_av(2)>37.5) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .or. (bodyseg(seg)%skin%OutSkTemp(2)<32&
                .and. time<=120)) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if ( Tcore_av(2)<36.7 .or. &
                bodyseg(seg)%skin%OutSkTemp(2)<=32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              end if
            end if
          case (L_F3)
            call four(ratios(131),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(131)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(131)%radius**2
            Taadj=bodyseg(PALM_L)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(131)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(L_F3)%mskin_basal,&
                bodyseg(L_F3)%mskin_maxcon,bodyseg(L_F3)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (Tcore_av(2)>37.5) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .or. (bodyseg(seg)%skin%OutSkTemp(2)<32&
                .and. time<=120)) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if ( Tcore_av(2)<36.7 .or. &
                bodyseg(seg)%skin%OutSkTemp(2)<=32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              end if
            end if
          case (R_F3)
            call four(ratios(136),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(136)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(136)%radius**2
            Taadj=bodyseg(PALM_R)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(136)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(R_F3)%mskin_basal,&
                bodyseg(R_F3)%mskin_maxcon,bodyseg(R_F3)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (Tcore_av(2)>37.5) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .or. (bodyseg(seg)%skin%OutSkTemp(2)<32&
                .and. time<=120)) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if ( Tcore_av(2)<36.7 .or. &
                bodyseg(seg)%skin%OutSkTemp(2)<=32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              end if
            end if
          case (L_F4)
            call four(ratios(132),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(132)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(132)%radius**2
            Taadj=bodyseg(PALM_L)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(132)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(L_F4)%mskin_basal,&
                bodyseg(L_F4)%mskin_maxcon,bodyseg(L_F4)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (Tcore_av(2)>37.5) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .or. (bodyseg(seg)%skin%OutSkTemp(2)<32&
                .and. time<=120)) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if ( Tcore_av(2)<36.7 .or. &
                bodyseg(seg)%skin%OutSkTemp(2)<=32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              end if
            end if
          case (R_F4)
            call four(ratios(137),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(137)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(137)%radius**2
            Taadj=bodyseg(PALM_R)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(137)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(R_F4)%mskin_basal,&
                bodyseg(R_F4)%mskin_maxcon,bodyseg(R_F4)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (Tcore_av(2)>37.5) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .or. (bodyseg(seg)%skin%OutSkTemp(2)<32&
                .and. time<=120)) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if ( Tcore_av(2)<36.7 .or. &
                bodyseg(seg)%skin%OutSkTemp(2)<=32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              end if
            end if
          case (L_F5)
            call four(ratios(133),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(133)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(133)%radius**2
            Taadj=bodyseg(PALM_L)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(133)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(L_F5)%mskin_basal,&
                bodyseg(L_F5)%mskin_maxcon,bodyseg(L_F5)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (Tcore_av(2)>37.5) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .or. (bodyseg(seg)%skin%OutSkTemp(2)<32&
                .and. time<=120)) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if ( Tcore_av(2)<36.7 .or. &
                bodyseg(seg)%skin%OutSkTemp(2)<=32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              end if
            end if
          case (R_F5)
            call four(ratios(138),heartbeat,T_STP,velocity,m,cardiacout)
            arteries(138)%mft=m
            call calc_velocity(velocity,heartbeat,T_STP,time,v)
            xma=v*PI*arteries(138)%radius**2
            Taadj=bodyseg(PALM_R)%Ta(2)
            if (period1>(60.0/heartbeat)) then
              perfusion=m*PI*arteries(138)%radius**2
              mblsk=ro_blood*Mskin(perfusion,coreTemp,OutSkTemp,bodyseg(R_F5)%mskin_basal,&
                bodyseg(R_F5)%mskin_maxcon,bodyseg(R_F5)%mskin_maxdil)/3600.0/1e3
              bodyseg(seg)%mperf=perfusion
              bodyseg(seg)%mskin=mblsk
              if (Tcore_av(2)>37.5) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .or. (bodyseg(seg)%skin%OutSkTemp(2)<32&
                .and. time<=120)) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else if ( Tcore_av(2)<36.7 .or. &
                bodyseg(seg)%skin%OutSkTemp(2)<=32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              else if (bodyseg(seg)%skin%OutSkTemp(2)>32 .and. time>120) then
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow + AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  ((ro_blood*perfusion/1e3 - mblsk)-bodyseg(seg)%AVAstart)*T_STP
              else
                bodyseg(seg)%AVABflow = bodyseg(seg)%AVABflow - AVAcoef(time-bodyseg(seg)%t_ava_local)*&
                  bodyseg(seg)%AVAstart*T_STP
              end if
            end if
          end select
          if (bodyseg(seg)%AVABflow<0) then
            bodyseg(seg)%AVABflow=0
          end if

          xma=ro_blood*xma/1e3
          bodyseg(seg)%xma=xma

          perfusion=bodyseg(seg)%mperf
          mblsk=bodyseg(seg)%mskin
          mperfv=perfusion*ro_blood/1e3
          perfusion = ro_blood*perfusion/1e3-mblsk-bodyseg(seg)%AVABflow

          hr=0.7*4.0*5.67e-8*0.95*(273.0+(OutSkTemp+Op_T-273)/2)**3
          if (seg<13) then
            hc=7.6
          else if (seg==13 .or. seg==14) then
            hc=5.4
          else if (seg==15 .or. seg==16) then
            hc=5.75
          else
            hc=3.1
          end if
          he=16.5*hc

          SkinCap=bodyseg(seg)%skin%SkinCap
          skinMet=METAo*bodyseg(seg)%skin%skinMet
          SkinArea=bodyseg(seg)%skin%SkinArea

          if (bodyseg(seg)%clothed==0) then
            Pskin=Psk(Psat(OutSkTemp+273.0),he,amb_vap_p,msw(Tcrav,Tskav,SkinArea,bodyseg(seg)%SWEAT))
            bodyseg(seg)%skin%moistacc=bodyseg(seg)%skin%moistacc+T_STP*(msw(Tcrav,Tskav,&
              SkinArea,bodyseg(seg)%SWEAT)+(Psat(OutSkTemp+273.0)-Pskin)/0.33/2.43e6-(Pskin-amb_vap_p)*he/2.43e6)
          else
            Pskin=Psk(Psat(OutSkTemp+273.0),1.0/(0.034*(1-exp(-bodyseg(seg)%layer(1)%air_thick/15))),&
              bodyseg(seg)%layer(1)%Pi_c,msw(Tcrav,Tskav,SkinArea,bodyseg(seg)%SWEAT))
            bodyseg(seg)%skin%moistacc=bodyseg(seg)%skin%moistacc+T_STP*&
              (msw(Tcrav,Tskav,SkinArea,bodyseg(seg)%SWEAT)+(Psat(OutSkTemp+273.0)-Pskin)/0.33/2.43e6-&
              (Pskin-bodyseg(seg)%layer(1)%Pi_c)/2.43e6/&
              (0.034*(1-exp(-bodyseg(seg)%layer(1)%air_thick/15))))
          end if

          Cv=bodyseg(seg)%Cbl_v
          select case (seg)
          case (HEAD)
            bodyseg(seg)%xmv=perfusion
            Tvadj=bodyseg(HEAD)%Tv(2)
          case (U_CHEST)
            xmv=bodyseg(L_CHEST)%xmv + bodyseg(UPPERARM_L)%xmv + bodyseg(UPPERARM_R)%xmv + bodyseg(HEAD)%xmv &
              + bodyseg(FOREARM_L)%xmvs + bodyseg(FOREARM_R)%xmvs
            bodyseg(seg)%xmv=xmv+perfusion
            if (xmv>0) then
              Tvadj=(bodyseg(HEAD)%Tv(2)*bodyseg(HEAD)%xmv+(bodyseg(UPPERARM_L)%xmv+ bodyseg(FOREARM_L)%xmvs)*&
                bodyseg(UPPERARM_L)%Tv(2)+(bodyseg(UPPERARM_R)%xmv+ bodyseg(FOREARM_R)%xmvs)*&
                bodyseg(UPPERARM_R)%Tv(2)+bodyseg(L_CHEST)%xmv*bodyseg(L_CHEST)%Tv(2))/xmv
            else
              Tvadj=(bodyseg(HEAD)%Tv(2)+bodyseg(UPPERARM_L)%Tv(2)+bodyseg(UPPERARM_R)%Tv(2)+&
                bodyseg(L_CHEST)%Tv(2))/4.0     
            end if
          case (L_CHEST)
            xmv=bodyseg(THIGH_L)%xmv + bodyseg(THIGH_R)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=(bodyseg(THIGH_L)%Tv(2)+bodyseg(THIGH_R)%Tv(2))/2.0
          case (UPPERARM_L)
            xmv=bodyseg(FOREARM_L)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=bodyseg(FOREARM_L)%Tv(2)
          case (UPPERARM_R)
            xmv=bodyseg(FOREARM_R)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=bodyseg(FOREARM_R)%Tv(2)
          case (FOREARM_L)
            xmv=bodyseg(PALM_L)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=bodyseg(PALM_L)%Tv(2)
          case (FOREARM_R)
            xmv=bodyseg(PALM_R)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=bodyseg(PALM_R)%Tv(2)
          case (PALM_L)
            xmv=bodyseg(L_F1)%xmv+bodyseg(L_F2)%xmv+bodyseg(L_F3)%xmv+bodyseg(L_F4)%xmv+bodyseg(L_F5)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=(bodyseg(L_F1)%Tv(2)+bodyseg(L_F2)%Tv(2)+bodyseg(L_F3)%Tv(2)+bodyseg(L_F4)%Tv(2)+&
              bodyseg(L_F5)%Tv(2))/5
          case (PALM_R)
            xmv=bodyseg(R_F1)%xmv+bodyseg(R_F2)%xmv+bodyseg(R_F3)%xmv+bodyseg(R_F4)%xmv+bodyseg(R_F5)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=(bodyseg(R_F1)%Tv(2)+bodyseg(R_F2)%Tv(2)+bodyseg(R_F3)%Tv(2)+bodyseg(R_F4)%Tv(2)+&
              bodyseg(R_F5)%Tv(2))/5
          case (THIGH_L)
            xmv=bodyseg(CALF_L)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=bodyseg(CALF_L)%Tv(2)
          case (THIGH_R)
            xmv=bodyseg(CALF_R)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=bodyseg(CALF_R)%Tv(2)
          case (CALF_L)
            xmv=bodyseg(FOOT_L)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=bodyseg(FOOT_L)%Tv(2)
          case (CALF_R)
            xmv=bodyseg(FOOT_R)%xmv
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=bodyseg(FOOT_R)%Tv(2)
          case (FOOT_L,FOOT_R,L_F1,R_F1,L_F2,R_F2,L_F3,R_F3,L_F4,R_F4,L_F5,R_F5)
            xmv=0.0
            bodyseg(seg)%xmv=xmv+perfusion
            Tvadj=bodyseg(seg)%Tv(2)
          end select
          Cvs=bodyseg(seg)%Cbl_v
          select case (seg)
          case (HEAD)
            xmvs=0.0
            bodyseg(seg)%xmvs=0.0
            Tvsadj=0.0
          case (U_CHEST)
            xmvs=bodyseg(L_CHEST)%xmvs + bodyseg(UPPERARM_L)%xmvs + bodyseg(UPPERARM_R)%xmvs + bodyseg(HEAD)%xmvs
            bodyseg(seg)%xmvs=0.0
            Tvsadj=0.0
          case (L_CHEST)
            xmvs=bodyseg(THIGH_L)%xmvs + bodyseg(THIGH_R)%xmvs
            bodyseg(seg)%xmvs=0.0
            Tvsadj=0.0
          case (UPPERARM_L)
            xmvs=bodyseg(FOREARM_L)%xmvs
            bodyseg(seg)%xmvs=0.0
            Tvsadj=bodyseg(FOREARM_L)%Tvs(2)
          case (UPPERARM_R)
            xmvs=bodyseg(FOREARM_R)%xmvs
            bodyseg(seg)%xmvs=0.0
            Tvsadj=bodyseg(FOREARM_R)%Tvs(2)
          case (FOREARM_L)
            xmvs=bodyseg(PALM_L)%xmvs
            bodyseg(seg)%xmvs=xmvs+mblsk+bodyseg(seg)%AVABflow
            Tvsadj=bodyseg(PALM_L)%Tvs(2)
          case (FOREARM_R)
            xmvs=bodyseg(PALM_R)%xmvs
            bodyseg(seg)%xmvs=xmvs+mblsk+bodyseg(seg)%AVABflow
            Tvsadj=bodyseg(PALM_R)%Tvs(2)
          case (PALM_L)
            xmvs=bodyseg(L_F1)%xmvs+bodyseg(L_F2)%xmvs+bodyseg(L_F3)%xmvs+bodyseg(L_F4)%xmvs+bodyseg(L_F5)%xmvs
            bodyseg(seg)%xmvs=xmvs+mblsk+bodyseg(seg)%AVABflow
            Tvsadj=(bodyseg(L_F1)%Tvs(2)+bodyseg(L_F2)%Tvs(2)+bodyseg(L_F3)%Tvs(2)+bodyseg(L_F4)%Tvs(2)+&
              bodyseg(L_F5)%Tvs(2))/5
          case (PALM_R)
            xmvs=bodyseg(R_F1)%xmvs+bodyseg(R_F2)%xmvs+bodyseg(R_F3)%xmvs+bodyseg(R_F4)%xmvs+bodyseg(R_F5)%xmvs
            bodyseg(seg)%xmvs=xmvs+mblsk+bodyseg(seg)%AVABflow
            Tvsadj=(bodyseg(R_F1)%Tvs(2)+bodyseg(R_F2)%Tvs(2)+bodyseg(R_F3)%Tvs(2)+bodyseg(R_F4)%Tvs(2)+&
              bodyseg(R_F5)%Tvs(2))/5
          case (THIGH_L)
            xmvs=bodyseg(CALF_L)%xmvs
            bodyseg(seg)%xmvs=0.0
            Tvsadj=bodyseg(CALF_L)%Tvs(2)
          case (THIGH_R)
            xmvs=bodyseg(CALF_R)%xmvs
            bodyseg(seg)%xmvs=0.0
            Tvsadj=bodyseg(CALF_R)%Tvs(2)
          case (CALF_L)
            xmvs=bodyseg(FOOT_L)%xmvs
            bodyseg(seg)%xmvs=0.0
            Tvsadj=bodyseg(FOOT_L)%Tvs(2)
          case (CALF_R)
            xmvs=bodyseg(FOOT_R)%xmvs
            bodyseg(seg)%xmvs=0.0
            Tvsadj=bodyseg(FOOT_R)%Tvs(2)
          case (FOOT_L,FOOT_R)
            xmvs=0.0
            bodyseg(seg)%xmvs=0.0
            Tvsadj=0.0
          case (L_F1,R_F1,L_F2,R_F2,L_F3,R_F3,L_F4,R_F4,L_F5,R_F5)
            xmvs=0.0
            bodyseg(seg)%xmvs=xmvs+mblsk+bodyseg(seg)%AVABflow
            Tvsadj=bodyseg(seg)%Tvs(2)
          end select
          bodyseg(seg)%skin%OutSkTemp(3)=2*bodyseg(seg)%skin%skinTemp(2)-bodyseg(seg)%core%coreTemp(2)

          if (seg>23) then
            if (seg==U_CHEST) then
              bodyseg(seg)%core%coreTemp(3)=(coreMet+shiv-alpha*resp-Qcrsk-Qconva-Qconvv&
                +Cp_blood*((perfusion+bodyseg(U_CHEST)%mskin)*(Ta-coreTemp)+&
              bodyseg(U_CHEST)%mskin*(skinTemp-coreTemp)))*T_STP/CoreCap+coreTemp
              bodyseg(seg)%Ta(3)=(Qconva+xma*Cp_blood*(Taadj-Ta))*T_STP/Ca+Ta
              bodyseg(seg)%Tv(3)=(Qconvv+xmv*Cp_blood*(Tvadj-Tv)+mperfv*Cp_blood*(coreTemp-Tv)+&
                xmvs*Cp_blood*(Tvsadj-Tv))*T_STP/Cv+Tv
            end if
            if (seg==L_CHEST) then
              bodyseg(seg)%core%coreTemp(3)=(coreMet+shiv-alpha*resp-Qcrsk-Qconva-Qconvv&
                +Cp_blood*((perfusion+bodyseg(L_CHEST)%mskin)*(Ta-coreTemp)+&
              bodyseg(L_CHEST)%mskin*(skinTemp-coreTemp)))*T_STP/CoreCap+coreTemp
              bodyseg(seg)%Ta(3)=(Qconva+xma*Cp_blood*(Taadj-Ta))*T_STP/Ca+Ta
              bodyseg(seg)%Tv(3)=(Qconvv+xmv*Cp_blood*(Tvadj-Tv)+mperfv*Cp_blood*(coreTemp-Tv)+&
                xmvs*Cp_blood*(Tvsadj-Tv))*T_STP/Cv+Tv
            end if
            if (bodyseg(seg)%clothed==0) then
              bodyseg(seg)%skin%skinTemp(3)=(skinMet+Kap*(coreTemp-skinTemp)/4&
                -(SkinArea/expo)*(hc*(bodyseg(seg)%skin%OutSkTemp(2)-(Op_T-273))&
                +hr*(bodyseg(seg)%skin%OutSkTemp(2)-(Op_T-273))+he*(Pskin-amb_vap_p))&
                +mblsk*Cp_blood*(coreTemp-skinTemp))*T_STP/SkinCap+skinTemp
            else
              bodyseg(seg)%skin%skinTemp(3)=(skinMet+Kap*(coreTemp-skinTemp)/4-(SkinArea/expo)*&
                ((bodyseg(seg)%skin%OutSkTemp(2)-bodyseg(seg)%layer(1)%Ti_c)&
                /(1.0/(4.9+24.0/bodyseg(seg)%layer(1)%air_thick))+(Pskin-bodyseg(seg)%layer(1)%Pi_c)&
                /(0.034*(1-exp(-bodyseg(seg)%layer(1)%air_thick/15.0))))+&
                mblsk*Cp_blood*(coreTemp-skinTemp))*T_STP/SkinCap+skinTemp
            end if
          else if (seg<15) then
            bodyseg(seg)%core%coreTemp(3)=(coreMet+shiv-alpha*resp-Qcrsk-Qconva-Qconvv&
              +Cp_blood*((perfusion+mblsk+bodyseg(seg)%AVABflow)*(Ta-coreTemp)))*T_STP/CoreCap+coreTemp
            bodyseg(seg)%skin%skinTemp(3)=(skinMet+Qcrsk+Qconvvs-(SkinArea/expo)*&
              ((hc+hr)*(bodyseg(seg)%skin%OutSkTemp(2)-Op_T+273)&
              +he*(Pskin-amb_vap_p))+(mblsk+bodyseg(seg)%AVABflow)*Cp_blood*&
              (coreTemp-skinTemp))*T_STP/SkinCap+skinTemp
            bodyseg(seg)%Ta(3)=(Qconva+xma*Cp_blood*(Taadj-Ta))*T_STP/Ca+Ta
            bodyseg(seg)%Tv(3)=(Qconvv+xmv*Cp_blood*(Tvadj-Tv)+perfusion*Cp_blood*(coreTemp-Tv))*T_STP/Cv+Tv
            bodyseg(seg)%Tvs(3)=(-Qconvvs+xmvs*Cp_blood*(Tvsadj-Tvs)+(mblsk+bodyseg(seg)%AVABflow)&
              *Cp_blood*(skinTemp-Tvs))*T_STP/Cvs+Tvs
          else
            bodyseg(seg)%core%coreTemp(3)=(coreMet+shiv-alpha*resp-Qcrsk-Qconva-Qconvv&
              +Cp_blood*((perfusion+mblsk)*(Ta-coreTemp)+mblsk*(skinTemp-coreTemp)))*T_STP/CoreCap+coreTemp
            if (bodyseg(seg)%clothed==0) then
              bodyseg(seg)%skin%skinTemp(3)=(skinMet+Qcrsk-(SkinArea/expo)*&
                (hc*(bodyseg(seg)%skin%OutSkTemp(2)-(Op_T-273))&
                +hr*(bodyseg(seg)%skin%OutSkTemp(2)-(Op_T-273))+he*(Pskin-amb_vap_p))&
                +mblsk*Cp_blood*(coreTemp-skinTemp))*T_STP/SkinCap+skinTemp
            else
              bodyseg(seg)%skin%skinTemp(3)=(skinMet+Qcrsk-(SkinArea/expo)*&
                ((bodyseg(seg)%skin%OutSkTemp(2)-bodyseg(seg)%layer(1)%Ti_c)&
                /(1.0/(4.9+24.0/bodyseg(seg)%layer(1)%air_thick))+(Pskin-bodyseg(seg)%layer(1)%Pi_c)&
                /(0.034*(1-exp(-bodyseg(seg)%layer(1)%air_thick/15.0))))+&
                mblsk*Cp_blood*(coreTemp-skinTemp))*T_STP/SkinCap+skinTemp
            end if
            bodyseg(seg)%Ta(3)=(Qconva+xma*Cp_blood*(Taadj-Ta))*T_STP/Ca+Ta
            bodyseg(seg)%Tv(3)=(Qconvv+xmv*Cp_blood*(Tvadj-Tv)+mperfv*Cp_blood*(coreTemp-Tv)+&
              xmvs*Cp_blood*(Tvsadj-Tv))*T_STP/Cv+Tv
          end if

          if (bodyseg(seg)%clothed==0) then
            if((Pskin-amb_vap_p).lt.0)then
              bodyseg(seg)%sklat=0
              bodyseg(seg)%sksens=(hr+hc)*(bodyseg(seg)%skin%OutSkTemp(3)-Op_T+273.0)*SkinArea/expo
            else
              bodyseg(seg)%sklat=he*(Pskin-amb_vap_p)*SkinArea/expo
              bodyseg(seg)%sksens=(hr+hc)*(bodyseg(seg)%skin%OutSkTemp(3)-Op_T+273.0)*&
                SkinArea/expo
            end if
          else
            if((bodyseg(seg)%layer(bodyseg(seg)%n_layers)%Pi_c-amb_vap_p).lt.0)then
              bodyseg(seg)%sklat=0
              bodyseg(seg)%sksens=(SkinArea/expo)*(bodyseg(seg)%layer(bodyseg(seg)%n_layers)%Ti_c-Op_T+273.0)/&
                (bodyseg(seg)%layer(bodyseg(seg)%n_layers)%Rth+1.0/(hc+hr))
            else
              bodyseg(seg)%sklat=(SkinArea/expo)*(bodyseg(seg)%layer(bodyseg(seg)%n_layers)%Pi_c-amb_vap_p)/&
                (bodyseg(seg)%layer(bodyseg(seg)%n_layers)%Rev+1.0/hc/16.5)
              bodyseg(seg)%sksens=(SkinArea/expo)*(bodyseg(seg)%layer(bodyseg(seg)%n_layers)%Ti_c-Op_T+273.0)/&
                (bodyseg(seg)%layer(bodyseg(seg)%n_layers)%Rth+1.0/(hc+hr))
            end if
          end if

          if (bodyseg(seg)%clothed==1) then
            do j=1,bodyseg(seg)%n_layers
              phi_cc=Phi(bodyseg(seg)%layer(j)%Ri_c,coef_i(bodyseg(seg)%layer(j)%material1,:),&
                coef_i(bodyseg(seg)%layer(j)%material2,:),real(1.0),real(0.0))
              bodyseg(seg)%layer(j)%Pi_c=Psat(bodyseg(seg)%layer(j)%Ti_c+273.0)*phi_cc/100.0
            end do
          end if

          if (bodyseg(seg)%clothed==1) then
            do j=1,bodyseg(seg)%n_layers
              phi_c=Phi(bodyseg(seg)%layer(j)%Ri_c,coef_i(bodyseg(seg)%layer(j)%material1,:),&
                coef_i(bodyseg(seg)%layer(j)%material2,:),real(1.0),real(0.0))
              if (phi_c<0) then
                phi_c=0
              end if
              Cp_c=bodyseg(seg)%layer(j)%Cp
              ro_c=bodyseg(seg)%layer(j)%density
              th_c=bodyseg(seg)%layer(j)%thick/1000.0
              rj_c=bodyseg(seg)%layer(j)%radius
              P_c=bodyseg(seg)%layer(j)%Pi_c
              T_c=bodyseg(seg)%layer(j)%Ti_c
              if (j==bodyseg(seg)%n_layers) then
                Re_c=bodyseg(seg)%layer(j)%Rev+1.0/hc/16.5
                Rd_c=bodyseg(seg)%layer(j)%Rth+1.0/(hc+hr)
              else
                Re_c=bodyseg(seg)%layer(j)%Rev+0.034*(1-exp(-bodyseg(seg)%layer(j+1)%air_thick/15))
                Rd_c=bodyseg(seg)%layer(j)%Rth+1.0/(4.9+24/bodyseg(seg)%layer(j+1)%air_thick)
              end if
              if (j==1) then
                rj1_c=bodyseg(seg)%inner_radius
                P1_c=Pskin
                T1_c=bodyseg(seg)%skin%skinTemp(2)
                Re1_c=0.034*(1-exp(-bodyseg(seg)%layer(1)%air_thick/15))
                Rd1_c=1.0/(4.9+24/bodyseg(seg)%layer(1)%air_thick)
              else
                rj1_c=bodyseg(seg)%layer(j-1)%radius
                P1_c=bodyseg(seg)%layer(j-1)%Pi_c
                T1_c=bodyseg(seg)%layer(j-1)%Ti_c
                Re1_c=bodyseg(seg)%layer(j-1)%Rev+0.034*(1-exp(-bodyseg(seg)%layer(j)%air_thick/15))
                Rd1_c=bodyseg(seg)%layer(j-1)%Rth+1/(4.9+24/bodyseg(seg)%layer(j)%air_thick)
              end if
              if (j==bodyseg(seg)%n_layers) then
                P2_c=amb_vap_p
                T2_c=Op_T-273.0
              else
                P2_c=bodyseg(seg)%layer(j+1)%Pi_c
                T2_c=bodyseg(seg)%layer(j+1)%Ti_c
              end if
              bodyseg(seg)%layer(j)%Ri1_c=((rj1_c/rj_c)*(P1_c-P_c)/Re1_c-(P_c-P2_c)/Re_c)*T_STP&
                /ro_c/th_c/2.43e6+bodyseg(seg)%layer(j)%Ri_c
              bodyseg(seg)%layer(j)%Ti1_c=((rj1_c/rj_c)*(T1_c-T_c)/Rd1_c-(T_c-T2_c)/Rd_c+th_c*ro_c*&
                Sorption(phi_c,sorption_coef)*(bodyseg(seg)%layer(j)%Ri1_c-&
                bodyseg(seg)%layer(j)%Ri_c)/T_STP)*1/ro_c/th_c/Cp_c+&
                bodyseg(seg)%skin%OUTSKTemp(2)
              bodyseg(seg)%layer(j)%Ti_c=bodyseg(seg)%layer(j)%Ti1_c
              bodyseg(seg)%layer(j)%Ri_c=bodyseg(seg)%layer(j)%Ri1_c
              if (bodyseg(seg)%layer(j)%Ri_c<0) then
                bodyseg(seg)%layer(j)%Ri_c=0
              end if
            end do
          end if

          bodyseg(seg)%core%coreTemp(2)=bodyseg(seg)%core%coreTemp(3)
          bodyseg(seg)%skin%skinTemp(2)=bodyseg(seg)%skin%skinTemp(3)
          bodyseg(seg)%skin%OutSkTemp(2)=bodyseg(seg)%skin%OutSkTemp(3)
          bodyseg(seg)%Ta(2)=bodyseg(seg)%Ta(3)
          bodyseg(seg)%Tv(2)=bodyseg(seg)%Tv(3)
          bodyseg(seg)%Tvs(2)=bodyseg(seg)%Tvs(3)
          if (icfd.gt.0) H2Oswt1(icfd)=H2Oswt1(icfd)+msw(Tcrav,Tskav,SkinArea,bodyseg(seg)%SWEAT)
          Mresp=1.43e-6*METAo
          HRexp=0.2*HR_amb+0.0277+6.5e-5*Op_T
          if (icfd.gt.0) H2Oexp1(icfd)=Mresp*(HRexp-HR_amb)
          if (seg==25 .and. icfd.gt.0) then
            headflux1(icfd)=(bodyseg(23)%sksens/bodyseg(23)%skin%SkinArea)*0.5+Qress(met,Op_T-273.0,HR_amb)
            trunkflux1(icfd)=((bodyseg(24)%sksens+bodyseg(25)%sksens)&
              /(bodyseg(24)%skin%SkinArea+bodyseg(25)%skin%SkinArea))*0.5
            larmflux1(icfd)=((bodyseg(1)%sksens+bodyseg(3)%sksens+&
              bodyseg(5)%sksens+bodyseg(7)%sksens+&
              bodyseg(9)%sksens+bodyseg(11)%sksens+&
              bodyseg(13)%sksens+bodyseg(15)%sksens)&
              /(bodyseg(1)%skin%SkinArea+bodyseg(3)%skin%SkinArea+&
              bodyseg(5)%skin%SkinArea+bodyseg(7)%skin%SkinArea+&
              bodyseg(9)%skin%SkinArea+bodyseg(11)%skin%SkinArea+&
              bodyseg(13)%skin%SkinArea+bodyseg(15)%skin%SkinArea))*0.5
            rarmflux1(icfd)=((bodyseg(2)%sksens+bodyseg(4)%sksens+&
              bodyseg(6)%sksens+bodyseg(8)%sksens+&
              bodyseg(10)%sksens+bodyseg(12)%sksens+&
              bodyseg(14)%sksens+bodyseg(16)%sksens)&
              /(bodyseg(2)%skin%SkinArea+bodyseg(4)%skin%SkinArea+&
              bodyseg(6)%skin%SkinArea+bodyseg(8)%skin%SkinArea+&
              bodyseg(10)%skin%SkinArea+bodyseg(12)%skin%SkinArea+&
              bodyseg(14)%skin%SkinArea+bodyseg(16)%skin%SkinArea))*0.5
            llegflux1(icfd)=((bodyseg(17)%sksens+bodyseg(19)%sksens+&
              bodyseg(21)%sksens)&
              /(bodyseg(17)%skin%SkinArea+bodyseg(19)%skin%SkinArea+&
              bodyseg(21)%skin%SkinArea))*0.5
            rlegflux1(icfd)=((bodyseg(18)%sksens+bodyseg(20)%sksens+&
              bodyseg(22)%sksens)&
              /(bodyseg(18)%skin%SkinArea+bodyseg(20)%skin%SkinArea+&
              bodyseg(22)%skin%SkinArea))*0.5
          end if
        end do

        time=time+T_STP
        period=period+T_STP
        period1=period1+T_STP
        deallocate(velocity)
        Tcore_av(3)=0.0
        Tskin_av(3)=0.0
        Tskino_av(2)=0.0
        sksenstotal=0.0
        sklattotal=0.0
        do j=1,28
          if (j<(L_CHEST+1) .or. j==U_CHEST) then
            Tcore_av(3)=Tcore_av(3)+bodyseg(j)%core%coreTemp(3)*bodyseg(j)%THERMO
            Tskin_av(3)=Tskin_av(3)+bodyseg(j)%skin%skinTemp(3)*bodyseg(j)%THERMO
            Tskino_av(2)=Tskino_av(2)+bodyseg(j)%skin%OutSkTemp(2)*bodyseg(j)%THERMO
            Tskino_av(3)=Tskino_av(3)+bodyseg(j)%skin%OutSkTemp(3)*bodyseg(j)%THERMO
            sksenstotal=sksenstotal+bodyseg(j)%sksens
            sklattotal=sklattotal+bodyseg(j)%sklat
          end if
        end do
        Tskin_av(2)=Tskin_av(3)
        Tcore_av(2)=Tcore_av(3)

        shivt(3)=Mshiv(Tcore_av(3),Tskin_av(3),real(1.0))

        if (period>(60.0/heartbeat)) then
          cardiac=2*CardOut(Tcore_av(3),Tskin_av(3))
          heartbeat=beatpermin(met,cardiac,shivt(3))
          cardiacout=cardiac
          AVA_control = cardiac
        end if
        if (period1>(60.0/heartbeat)) then
          if (cardiac <= 427500) then
            arteries(57)%radius=sqrt((0.3726**2-0.2178**2)*(cardiac-270000)/(427500-270000)+0.2178**2)
            arteries(42)%radius=arteries(57)%radius
            arteries(85)%radius=sqrt((0.268**2-0.1977**2)*(cardiac-270000)/(427500-270000)+0.1977**2)
            arteries(81)%radius=arteries(85)%radius
            arteries(108)%radius=sqrt((0.255**2-0.1911**2)*(cardiac-270000)/(427500-270000)+0.1911**2)
            arteries(103)%radius=arteries(108)%radius
            arteries(129)%radius=sqrt((0.085**2-0.0427**2)*(bodyseg(L_F1)%skin%OutSkTemp(2)-12)/(35-12)+0.0427**2)
            if (bodyseg(L_F1)%skin%OutSkTemp(2)>35) then
              arteries(129)%radius=0.085
            else if (bodyseg(L_F1)%skin%OutSkTemp(2)<12) then
              arteries(129)%radius=0.0427
            end if
            arteries(134)%radius=sqrt((0.085**2-0.0427**2)*(bodyseg(R_F1)%skin%OutSkTemp(2)-12)/(35-12)+0.0427**2)
            if (bodyseg(R_F1)%skin%OutSkTemp(2)>35) then
              arteries(134)%radius=0.085
            else if (bodyseg(R_F1)%skin%OutSkTemp(2)<12) then
              arteries(134)%radius=0.0427
            end if
            arteries(130)%radius=sqrt((0.085**2-0.0427**2)*(bodyseg(L_F2)%skin%OutSkTemp(2)-12)/(35-12)+0.0427**2)
            if (bodyseg(L_F2)%skin%OutSkTemp(2)>35) then
              arteries(130)%radius=0.085
            else if (bodyseg(L_F2)%skin%OutSkTemp(2)<12) then
              arteries(130)%radius=0.0427
            end if
            arteries(135)%radius=sqrt((0.085**2-0.0427**2)*(bodyseg(R_F2)%skin%OutSkTemp(2)-12)/(35-12)+0.0427**2)
            if (bodyseg(R_F2)%skin%OutSkTemp(2)>35) then
              arteries(135)%radius=0.085
            else if (bodyseg(R_F2)%skin%OutSkTemp(2)<12) then
              arteries(135)%radius=0.0427
            end if
            arteries(131)%radius=sqrt((0.085**2-0.0427**2)*(bodyseg(L_F3)%skin%OutSkTemp(2)-12)/(35-12)+0.0427**2)
            if (bodyseg(L_F3)%skin%OutSkTemp(2)>35) then
              arteries(131)%radius=0.085
            else if (bodyseg(L_F3)%skin%OutSkTemp(2)<12) then
              arteries(131)%radius=0.0427
            end if
            arteries(136)%radius=sqrt((0.085**2-0.0427**2)*(bodyseg(R_F3)%skin%OutSkTemp(2)-12)/(35-12)+0.0427**2)
            if (bodyseg(R_F3)%skin%OutSkTemp(2)>35) then
              arteries(136)%radius=0.085
            else if (bodyseg(R_F3)%skin%OutSkTemp(2)<12) then
              arteries(136)%radius=0.0427
            end if
            arteries(132)%radius=sqrt((0.085**2-0.0427**2)*(bodyseg(L_F4)%skin%OutSkTemp(2)-12)/(35-12)+0.0427**2)
            if (bodyseg(L_F4)%skin%OutSkTemp(2)>35) then
              arteries(132)%radius=0.085
            else if (bodyseg(L_F4)%skin%OutSkTemp(2)<12) then
              arteries(132)%radius=0.0427
            end if
            arteries(137)%radius=sqrt((0.085**2-0.0427**2)*(bodyseg(R_F4)%skin%OutSkTemp(2)-12)/(35-12)+0.0427**2)
            if (bodyseg(R_F4)%skin%OutSkTemp(2)>35) then
              arteries(137)%radius=0.085
            else if (bodyseg(R_F4)%skin%OutSkTemp(2)<12) then
              arteries(137)%radius=0.0427
            end if
            arteries(133)%radius=sqrt((0.085**2-0.0427**2)*(bodyseg(L_F5)%skin%OutSkTemp(2)-12)/(35-12)+0.0427**2)
            if (bodyseg(L_F5)%skin%OutSkTemp(2)>35) then
              arteries(133)%radius=0.085
            else if (bodyseg(L_F5)%skin%OutSkTemp(2)<12) then
              arteries(133)%radius=0.0427
            end if
            arteries(138)%radius=sqrt((0.085**2-0.0427**2)*(bodyseg(R_F5)%skin%OutSkTemp(2)-12)/(35-12)+0.0427**2)
            if (bodyseg(R_F5)%skin%OutSkTemp(2)>35) then
              arteries(138)%radius=0.08
            else if (bodyseg(R_F5)%skin%OutSkTemp(2)<12) then
              arteries(138)%radius=0.0427
            end if
            arteries(92)%radius=sqrt((0.429**2-0.248**2)*(cardiac-270000)/(427500-270000)+0.248**2)
            arteries(89)%radius=arteries(92)%radius
            arteries(113)%radius=sqrt((0.3**2-0.145**2)*(cardiac-270000)/(427500-270000)+0.145**2)
            arteries(114)%radius=arteries(113)%radius
            arteries(127)%radius=sqrt((0.2308**2-0.1015**2)*(cardiac-270000)/(427500-270000)+0.1015**2)
            arteries(126)%radius=arteries(127)%radius
            arteries(12)%radius=sqrt((0.4**2-0.4625**2)*(cardiac-270000)/(427500-270000)+0.4625**2)
            arteries(4)%radius=arteries(12)%radius
          else
            arteries(57)%radius=0.3726
            arteries(42)%radius=arteries(57)%radius
            arteries(85)%radius=0.268
            arteries(81)%radius=arteries(85)%radius
            arteries(108)%radius=0.255
            arteries(103)%radius=arteries(108)%radius
            arteries(92)%radius=0.429
            arteries(89)%radius=arteries(92)%radius
            arteries(113)%radius=0.3
            arteries(114)%radius=arteries(113)%radius
            arteries(127)%radius=0.2308
            arteries(126)%radius=arteries(127)%radius
            arteries(12)%radius=0.4
            arteries(4)%radius=arteries(12)%radius
          end if
          do j=1,138
            arteryradius(j)=arteries(j)%radius
          end do
          do ml=1,27
            if (j<(L_CHEST+1) .or. j==U_CHEST) then
              do j=1,bodyseg(ml)%n
                bodyseg(ml)%artery(j)%radius=arteries(bodyseg(ml)%vein(j)%index)%radius
                bodyseg(ml)%vein(j)%radius=1.75*arteries(bodyseg(ml)%vein(j)%index)%radius
                bodyseg(ml)%supvein(j)%radius=1.75*arteries(bodyseg(ml)%vein(j)%index)%radius
              end do
            end if
          end do
        end if

        if (period>(60.0/heartbeat)) then
          call bloodtree(heartbeat,ratios,fic,arteryradius,AVA_control)
          period=0.0
        end if
        if (period1>(60.0/heartbeat)+T_STP) then
          period1=0.0
        end if

!      if (mod(i,100)==0) then
!      print 10,i,Op_T-273,bodyseg(U_CHEST)%skin%OutSkTemp(2),bodyseg(U_CHEST)%layer(1)%Ti_c,&
!                  bodyseg(R_F2)%skin%OutSkTemp(2),bodyseg(R_F2)%core%coreTemp(2)&
!                  ,bodyseg(R_F1)%AVABflow
!      10 format(" ",i6," ",f9.5," ",f9.5," ",f9.5," ",f9.5," ",f9.5," ",f12.9)
!      end if
        if (i.eq.int(sim_time/T_STP)) then
!        if (time.GE.(sim_time-4*T_STP).and.time.LE.(sim_time))then
          open(unit=iunit,file="initial_condition1.txt",status="UNKNOWN")

          write(6,*)'bodyseg',bodyseg

          do iin=1,25
            ic(iin,1)=bodyseg(iin)%core%coreTemp(2)
            ic(iin,2)=bodyseg(iin)%skin%skinTemp(2)
            ic(iin,3)=bodyseg(iin)%Ta(2)
            ic(iin,4)=bodyseg(iin)%Tv(2)
            ic(iin,5)=bodyseg(iin)%Tvs(2)
            write(iunit,50) (ic(iin,r),r=1,5)
50          format(f11.5," ",f11.5," ",f11.5," ",f11.5," ",f11.5)
!              print*,Op_T-273, ic(iin,1), ic(iin,2)
          end do
          close(unit=iunit)
          open(iunit, file='PhysioResults1.csv',status="UNKNOWN",position='append')   
          if (icfd.gt.0) then
            write(iunit,*) ic(1,1),',',ic(2,1),',',ic(3,1),',',ic(4,1),',',ic(5,1),',',ic(6,1),',',&
              ic(7,1),',',ic(8,1),',',ic(9,1),',',ic(10,1),',',ic(11,1),',',ic(12,1),',',&
              ic(13,1),',',ic(14,1),',',ic(15,1),',',ic(16,1),',',ic(17,1),',',ic(18,1),',',&
              ic(19,1),',',ic(20,1),',',ic(21,1),',',ic(22,1),',',ic(23,1),',',ic(24,1),',',&
              ic(25,1),',',ic(1,2),',',ic(2,2),',',ic(3,2),',',ic(4,2),',',ic(5,2),',',ic(6,2),',',&
              ic(7,2),',',ic(8,2),',',ic(9,2),',',ic(10,2),',',ic(11,2),',',ic(12,2),',',&
              ic(13,2),',',ic(14,2),',',ic(15,2),',',ic(16,2),',',ic(17,2),',',ic(18,2),',',&
              ic(19,2),',',ic(20,2),',',ic(21,2),',',ic(22,2),',',ic(23,2),',',ic(24,2),',',&
              ic(25,2),',',tavhead1(icfd),',',tavtrunk1(icfd),',',tavarml1(icfd),',',tavarmr1(icfd),',',tavlegl1(icfd),',',&
              tavlegr1(icfd),',',hrhead(icfd),',',hrtrunk(icfd),',',hrarml(icfd),',',hrarmr(icfd),',',hrlegl(icfd),',',hrlegr(icfd)
          else
            write(iunit,*) ic(1,1),',',ic(2,1),',',ic(3,1),',',ic(4,1),',',ic(5,1),',',ic(6,1),',',&
              ic(7,1),',',ic(8,1),',',ic(9,1),',',ic(10,1),',',ic(11,1),',',ic(12,1),',',&
              ic(13,1),',',ic(14,1),',',ic(15,1),',',ic(16,1),',',ic(17,1),',',ic(18,1),',',&
              ic(19,1),',',ic(20,1),',',ic(21,1),',',ic(22,1),',',ic(23,1),',',ic(24,1),',',&
              ic(25,1),',',ic(1,2),',',ic(2,2),',',ic(3,2),',',ic(4,2),',',ic(5,2),',',ic(6,2),',',&
              ic(7,2),',',ic(8,2),',',ic(9,2),',',ic(10,2),',',ic(11,2),',',ic(12,2),',',&
              ic(13,2),',',ic(14,2),',',ic(15,2),',',ic(16,2),',',ic(17,2),',',ic(18,2),',',&
              ic(19,2),',',ic(20,2),',',ic(21,2),',',ic(22,2),',',ic(23,2),',',ic(24,2),',',&
              ic(25,2)  
          endif
          close(iunit)
        endif
        Qoccsens1(ICOMP)=sksenstotal+Qress(met,Op_T-273.0,HR_amb)
        Qocclat1(ICOMP)=sklattotal+Qresl(met,Op_T-273.0,HR_amb)
        ! Tsko_av1=Tskino_av(2)
        ! Tco_av1=Tcore_av(2)
        ! Tsk_av1=Tskin_av(2)
      end do

      Return
      END SUBROUTINE segmentedocc


