C============================== immersed_PCM ==========================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Deducts the scaled temperature of the tank nodes, assigns it to a 
C     new arrey in order to correspond to the PCM axial discretization.
C     According to the user choice to adopt an explicit or implicit 
C     method, it calls the respective subroutines.
C
C======================================================================

      SUBROUTINE Immersed_PCM(nX,nY,nPCMbot,nPCMtop,dr,
     &                       heightNode,Tstep_prev1,  
     &                       Mdot,nNodes,QPCM,ts,  
     &                       H,TMP,
     &                       Axb,Ax,Ay, V,
     &                       rhos,rhol,cs,cl,heatLat,cndcts,cndctl,Tmi, 
     &                       method,e,
     &                       hPCMcyl,dPCMcyl,hPCMbot,hPCMtop,
     &                       dPCMnode,CrossArea)


      INTEGER mX, mY, mXmY, mPCMsec, max_nNodes
      PARAMETER (mX=100, mY=100, mXmY=1000, mPCMsec=100, 
     &           max_nNodes=100)

C-----COUNTERS
      INTEGER i            ! axial direction counter [-]

C-----SPACE DISCRETIZATION and GEOMETRY
      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      REAL dr              ! grid dimension in radial direction
      REAL ts              ! time step [s]
      REAL V(nY,nX)        ! cells volumes [m^3]
      REAL Ax(nY,nX-1)     ! CV faces in radial direction (east-west) [m^2]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL Ay(nX)          ! CV faces in axial direction  (north-south) [m^2]

C-----PCM CHARACTERISTICS
      REAL rhos            ! density of solid [kg/m^3]
      REAL rhol            ! density of liquid [kg/m^3]
      REAL cs              ! specific heat of solid [J/(kg K)]
      REAL cl              ! specific heat of liquid [J/(kg K)]
      REAL heatLat         ! latent heat [J/kg]
      REAL cndcts          ! conductivity into solid [W/(m K)]
      REAL cndctl          ! conductivity into liquid [W/(m K)]
      REAL Tmi             ! melting temperature  [°C]
      INTEGER method       ! time integration method [-]
      REAL e               ! half phase change range [°C]
      REAL TMP(nY,nX)      ! cylinder cells temperature [°C]                          
      REAL H(nY,nX)        ! cylinder cells enthalpy                                   
      REAL Tm              ! scaling temperature 

C-----TANK CHARACTERISTICS
      REAL alfa            ! convection coefficient [W/(m^2 K)]

      INTEGER nNodes       ! number of nodes which the PCM tank is divided into

      REAL hPCMcyl         ! cylinder height [m]
      REAL dPCMcyl         ! PCM cylinders diameter [m]
      REAL hPCMbot         ! Position of the cylinder bottom [m]
      REAL hPCMtop         ! Position of the cylinder top [m]

      INTEGER nPCMbot      ! tank node corresponding to the lower cylinder node
                           ! tank node which the cylinder bottom is located in
      INTEGER nPCMtop      ! tank node corresponding to the higher cylinder node
                           ! tank node which the cylinder top is located in
      REAL heightNode      ! tank CV height
      REAL dPCMnode(nY-1)  ! distance among nodes - bottom, generic, top          
      REAL Tbound(mY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL Tstep_prev(max_nNodes)    
                           ! tank layer temperature
      REAL Tstep_prev1(nNodes)
                           ! original tank layer temperature
      REAL Tbot            ! DHW temp in contact with the cylinder bottom
      REAL Ttop            ! DHW temp in contact with the cylinder top
      REAL Mdot(nNodes)    ! flow rate at each node (value i is from node i 
                           ! to node i+1)
C-----OTHER
      REAL QPCM(nNodes)    ! haet transfer from immersed PCM [J]
      INTEGER IC           ! tank time step counter
!       INTEGER ICSTARTI     ! tank time step counter for implicit method
!       INTEGER ICSTARTE     ! tank time step counter for explicit method

      SAVE IC


      Tm = 0.

C-----Deducts the scaled temperature of the tank nodes
      do i= 1,nNodes
        Tstep_prev(i) = Tstep_prev1(i)-Tmi
      enddo
C ATTENZIONE!!! CANCELLARE QUESTO E RIPRISTINARE L!ALTRO!!! ^
!       do i= 1,nNodes
!         Tstep_prev(i) = 50.-Tmi
!       enddo
C ATTENZIONE!!! CANCELLARE QUESTO E RIPRISTINARE L!ALTRO!!!




C-----Boundary temperature for PCM section - lateral surface
      DO i=1,nY
        Tbound(i)=Tstep_prev(nPCMbot+i-1)
      ENDDO
C-----Bottom surface
      IF (hPCMbot.EQ.(nPCMbot-1)*heightNode) THEN
        Tbot = Tstep_prev(nPCMbot-1)
      ELSE 
        Tbot = Tstep_prev(nPCMbot)
      ENDIF
C-----Top surface
      IF (hPCMtop.EQ.nPCMtop*heightNode) THEN
        Ttop = Tstep_prev(nPCMtop+1)
      ELSE 
        Ttop = Tstep_prev(nPCMtop)
      ENDIF



C-----Select the method to solve the problem
      IF(method.EQ.1)THEN
        CALL ExplicitPCM(ts,nX,nY,nNodes,nPCMbot,nPCMtop,
     &                   dr,dPCMnode(1),
     &                   V(1,1),Ax(1,1),Axb(1),Ay(1),
     &                   dPCMcyl, hPCMcyl, 
     &                   alfa,Tbound(1),Tmi,method,e,Mdot(1),
     &                   cndcts,cndctl,
     &                   H(1,1),TMP(1,1),
     &                   heatLat,rhos,rhol, cs, cl,QPCM(1),Tm,
     &                   Tbot,Ttop,  
     &                   CrossArea)
      ELSEIF(method.EQ.2)THEN
        CALL ImplicitPCM(ts,nX,nY,nNodes,nPCMbot,nPCMtop,
     &                   dr,dPCMnode(1),
     &                   V(1,1),Ax(1,1),Axb(1),Ay(1),
     &                   dPCMcyl, hPCMcyl, 
     &                   alfa,Tbound(1),Tmi,method,e,Mdot(1),
     &                   cndcts,cndctl,
     &                   H(1,1),TMP(1,1),
     &                   heatLat,rhos,rhol, cs, cl,QPCM(1),Tm,
     &                   Tbot,Ttop,  
     &                   CrossArea)
      ENDIF



      IC = IC + 1 

C-----Print to an output file the following data have been calculated 
C     for all PCM time steps
      OPEN(UNIT=93,FILE='CellsTempFromRoutine.dat', 
     &                           FORM='FORMATTED', STATUS='UNKNOWN',
     &                           ACCESS='APPEND')
      write(93,"(I10,22E18.8)") IC,                !1st column: istant 
     &                          Tstep_prev(1)+Tmi, !2nd column: tank previous temperature at the 1st layer (bottom)
     &                          Tstep_prev(5)+Tmi, !3rd column: tank previous temperature at the 3rd layer 
     &                          Tstep_prev(10)+Tmi, !4th column: tank previous temperature at the 5th layer
     &                          Tstep_prev(15)+Tmi, !5th column: tank previous temperature at the 5th layer
     &                          Tstep_prev(17)+Tmi, !6th column: tank previous temperature at the 7th layer (top) 
     &                          Tstep_prev(19)+Tmi, !7th column: tank previous temperature at the 7th layer (top) 
     &                          QPCM(1),           !8th column: heat flux exchanged between PCM module and tank water (PCM bottom layer)
     &                          QPCM(5),           !9th column: heat flux exchanged between PCM module and tank water 
     &                          QPCM(10),           !10th column: heat flux exchanged between PCM module and tank water (PCM bottom layer)
     &                          QPCM(15),           !11th column: heat flux exchanged between PCM module and tank water 
     &                          QPCM(17),           !12th column: heat flux exchanged between PCM module and tank water 
     &                          QPCM(19),           !13th column: heat flux exchanged between PCM module and tank water (PCM top layer)
     &                          alfa,              !14th column: convection coefficient
     &                          TMP(1,1)+Tmi,      !15th column: PCM temperature at bottom layer for the border cell
     &                          TMP(1,10)+Tmi,     !16th column: PCM temperature at bottom layer for an intermediate cell 
     &                          TMP(1,20)+Tmi,     !17th column: PCM temperature at bottom layer for the inner cell  
     &                          TMP(2,1)+Tmi,      !18th column: PCM temperature at intermediate layer for the border cell
     &                          TMP(2,10)+Tmi,     !19th column: PCM temperature at intermediate layer for an intermediate cell 
     &                          TMP(2,20)+Tmi,     !20th column: PCM temperature at intermediate layer for the inner cell 
     &                          TMP(3,1)+Tmi,      !21th column: PCM temperature at top layer for the border cell 
     &                          TMP(3,10)+Tmi,     !22th column: PCM temperature at top layer for an intermediate cell 
     &                          TMP(3,20)+Tmi      !23th column: PCM temperature at top layer for the inner cell  
      CLOSE(93)


      RETURN
      END









C=============================== ImplicitPCM ==========================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Calculates the heat flux exchanged between PCM modules and tank 
C     water for each layer where the PCM is located in. It solves the 
C     problem using the implicit Euler method.
C     Simplification hypothesis:
C       - PCM modules are considered cylindrical
C       - top and bottom are adiabatic surfaces
C       - no convection inside the modules
C       - no subcooling phenomena considered
C     
C     References:
C     W.J.Minkowycz, E.M.Sparrow, "Advances in Numerical Heat Transfer"
C     chapter 9 - V.R.Voller, "An overview of numerical methods
C     for solving Phase Change problems"
C
C======================================================================

      SUBROUTINE ImplicitPCM(ts,nX,nY,nNodes,nPCMbot,nPCMtop,
     &                       dr,dPCMnode,
     &                       V,Ax,Axb,Ay,
     &                       dPCMcyl,hPCMcyl, 
     &                       alfa,Tbound,Tmi,method,e,Mdot,
     &                       cndcts,cndctl,
     &                       H,TMP,
     &                       heatLat,rhos,rhol,cs,cl,QPCM,Tm,
     &                       Tbot,Ttop,  
     &                       CrossArea)


      INTEGER mX, mY, mXmY, mPCMsec, max_nNodes
      REAL pi
      PARAMETER (mX=100, mY=100, mXmY=1000, mPCMsec=100, 
     &           max_nNodes=100)
      PARAMETER (pi=3.1415926535897932385) 

C-----COUNTERS
      REAL dt              ! time step [s]
      REAL ts              ! time step [s]
      INTEGER i            ! radial direction counter [-]
      INTEGER j            ! axial direction counter [-]
      INTEGER k            ! cell counter [-]

C-----SPACE DISCRETIZATION and GEOMETRY
      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      INTEGER nNodes       ! number of nodes which the PCM tank is divided into
      INTEGER nPCMbot      ! tank node corresponding to the lower cylinder node
                           ! tank node which the cylinder bottom is located in
      INTEGER nPCMtop      ! tank node corresponding to the higher cylinder node
                           ! tank node which the cylinder top is located in
      REAL hPCMcyl         ! cylinder height [m]
      REAL dPCMcyl         ! PCM cylinders diameter [m]
      REAL dr              ! grid dimension in radial direction
      REAL dPCMnode(nY-1)  ! spatial step, cell height = distance among nodes [m]   
      REAL V(nY,nX)        ! cells volumes [m^3]
      REAL Ax(nY,nX-1)     ! CV faces in radial direction (east-west) [m^2]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW [m^2]
      REAL Ay(nX)          ! CV faces in axial direction  (north-south) [m^2]

C-----TANK CHARACTERISTICS
      REAL alfa            ! convection coefficient [W/(m^2 K)]
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section - 
C                            boundary condition
      REAL Mdot(nNodes)    ! flow rate at each node (value i is from node i to node i+1)
      REAL CrossArea

C-----PCM CHARACTERISTICS
      REAL cndcts          ! conductivity into solid [W/(m K)]
      REAL cndctl          ! conductivity into liquid [W/(m K)]
      REAL cndctx(mY,mX-1) ! conductivity on CV faces in radial direction [W/(m K)]
      REAL cndcty(mY-1,mX) ! conductivity on CV faces in axial direction [W/(m K)]
      REAL cndctP(mY,mX)   ! conductivity on CV node [W/(m K)]
      REAL TMP(nY,nX)      ! cylinder cells temperature [°C]
      REAL H(nY,nX)        ! cylinder cells enthalpy
      REAL heatLat         ! latent heat [J/kg]
      REAL rhos            ! density [kg/m^3]
      REAL rhol            ! density [kg/m^3]
      REAL rho(mY,mX)      ! density on CV node [kg/m^3]
      REAL cs              ! specific heat of solid [J/(kg K)]
      REAL cl              ! specific heat of liquid [J/(kg K)]
      REAL Tmi             ! melting temperature  [°C]
      REAL Tm              ! scaling temperature

C-----OTHER
      REAL QPCM(max_nNodes)! haet transfer from immersed PCM [J]

      INTEGER nstpcm       ! number of pcm inner time steps for tank timesteps, 
C                            to verify explicit condition
      INTEGER istpcm       ! iteration value for internal pcm time step
      SAVE IC
      INTEGER method       ! time integration method [-]
      REAL eps             ! constant to calculate conductivity at cells faces
      REAL coeffx(mY,mX)   ! cndctx*Ax/dr coefficient, calculated for all faces:
                           ! diffusive term
      REAL coeffy(mY,mX)   ! cndcty*Ay/dPCMnode coefficient, calculated for all 
                           ! north and south faces: diffusive term 
      INTEGER IC           ! tank time step counter


C-----VALUES FOR IMPLICIT METHOD 
      REAL e
      REAL CA (mY,mX)      ! apparent specific heat 
      REAL Tmatrix(mXmY,mXmY) 
                           ! temperature coefficients matrix to solve the 
                           ! equations system (Ax=B)
      REAL T_SCn(mXmY)     ! temperature matrix self-coupling coefficients, 
                           ! into a vector
      REAL T_EASTn(mXmY)   ! temperature matrix cross-coupling coefficients (diagonals), 
                           ! into vectors
      REAL T_WESTn(mXmY)   ! temperature matrix cross-coupling coefficients (diagonals), 
                           ! into vectors
      REAL acc(mY,mX)      ! rho*V*CA/dt coefficient, calculated for all cells: 
                           ! accumulation term 
      REAL rho_V_dt(mY,mX) ! rho*V/dt coefficient, calculated for all cells
      REAL RHS(mXmY)       ! Right Hand Side to solve the equations system (Ax=B)
      REAL TMPn(mXmY)      ! sub-iteration temperature, corresponding to the solution 
                           ! of the equations system (Ax=B)
                           ! the values are calculated calling LU decomposition subroutines 
                           ! LUDCMS and LUBKSS or SolveTridiag if nY=1.
      REAL Hn(mXmY)        ! sub-iteration entalpy 
      REAL R(mXmY)         ! residual vector
      REAL RES             ! residue
      REAL VETT(mXmY)      ! vector for LU decomposition
      INTEGER L            ! variable for LU decomposition 
                           ! = +/- 1 depending on whether the number of row interchanges 
                           ! was even or odd
      INTEGER MAXIT        ! variable for LU decomposition


! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       REAL TMP_PREV(mY,mX)
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      nXnY = nX*nY

!       big = 5.
      eps = 1E-5

      nstpcm = 1 !if nstpcm=1 adopts the tank time-step 
      dt = ts/nstpcm

      
            

C-----Sets to 0 the heat flux from the module, it is computed (averaged)
C     at the end of the loop
      DO i=1,nY
        QPCM(i+nPCMbot-1) = 0.0
      ENDDO





C-----Calculate the convection coefficient ones per tank time-step
C     The calculation is done once per PCM time step. It takes account 
C     of PCM border cells mean temperature and tank whater mean  
C     temperature at the corresponding nodes.
      CALL convection_coeff(nX,nY,nNodes,nPCMbot,nPCMtop,
     &                      Tbound(1),
     &                      hPCMcyl,dPCMcyl,
     &                      Tmi,
     &                      dt,alfa,CrossArea,
     &                      Mdot(1),TMP(1,1))



! PER RILASSAMENTO
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         DO j=1,nX
!           DO i=1,nY
!             TMP_PREV(i,j) = TMP(i,j) 
!           ENDDO
!         ENDDO
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




C=====TIME LOOP   
      DO istpcm=1,nstpcm   



C-------Calculate nodal conductivity and density 
        DO j = 1, nX
          DO i= 1, nY
            IF (TMP(i,j).LT.0.)  THEN 
               cndctP(i,j) = cndcts
               rho(i,j)    = rhos
            ELSEIF(TMP(i,j).GT.0.)  THEN
               cndctP(i,j) = cndctl
               rho(i,j)    = rhol
            ELSE
               cndctP(i,j) = cndcts !indifferent the value at the moment, 
                                    !after calculates the nodal value 
                                    !using Kirchhoff
               rho(i,j)    = (rhos+rhol)/2
            ENDIF
          ENDDO
        ENDDO


C-------Calculate diffusive terms outside iterations loop because it is 
C       negligible in comparison to the accumulation term, especially 
C       during the Phase Change.
C       Use Kirchhoff approximation, see "Treatment of discontinuous 
C       thermal conductivity in control-volume solutions of phase-change
C       problems, "Voller and Swaminathan, Numerical Heat Transfer, 
C       Vol.24, No.B, pp. 161-180, September 1993

C-------Average conductivity on CV faces - radial direction, from border 
C       to axis (towards the east)
        DO j = 1, nX-1
          DO i= 1, nY
C           conducibility at east face of cell
            cndctx(i,j) = cndctP(i,j)+
     &                    (TMP(i,j+1)*(cndctP(i,j+1)-cndctP(i,j)))/
     &                    (TMP(i,j+1)-TMP(i,j)+eps)
            coeffx(i,j) = cndctx(i,j)*Ax(i,j)/dr 
          ENDDO
        ENDDO

C-------Average conductivity on CV faces - axial direction, from bottom 
C       to top (towards the north)
        IF(NY.NE.1)THEN
          DO j = 1, nX
            DO i= 1, nY-1
C             conducibility at north face of cell
              cndcty(i,j) = cndctP(i,j)+
     &                      (TMP(i+1,j)*(cndctP(i+1,j)-cndctP(i,j)))/
     &                      (TMP(i+1,j)-TMP(i,j)+eps)
              coeffy(i,j) = cndcty(i,j)*Ay(j)/dPCMnode(i)
            ENDDO
          ENDDO
        ENDIF


C-------Calculate accumulation term for all cells
        DO j=1,nX
          DO i=1,nY
            rho_V_dt(i,j) = rho(i,j)*V(i,j)/dt
          ENDDO
        ENDDO


C-------INIZIALIZATIONS BEFORE LOOP
C-------Assign the known temperature value to the iteration-temperature
C       arrey. The known temperature value is calculated for the current
C       time istant. The new temperature values to be calculated refer 
C       to the future time istant
        DO j=1,nX
          DO i=1,nY
            k=(j-1)*nY+i       
            TMPn(k) = TMP(i,j) 
            Hn(k) = H(i,j)     
          ENDDO
        ENDDO


        RES = 1E+1
        N = 0
        MAXIT = 15

C=======CONVERGENCE LOOP (time sub-iterations if residual is > 1E+0)
        DO WHILE ((RES.GT.5E+0).AND.(N.LE.MAXIT))




          IF (N.EQ.MAXIT) THEN
            write(*,*) "failed convergence on PCM loop, " ,
     &                 "residual =",RES
            STOP
          ENDIF

        
          N=N+1

C---------PREDICTION 
C---------Apparent specific heat  is calculated from the 
C         known temperature and enthalpy fields at iteration n-1 
          DO j =1, nX
            DO i = 1, nY
              k=(j-1)*nY+i
              IF (TMPn(k).LE.TM-e) THEN 
                CA(i,j) = cs
              ELSEIF (TMPn(k).GE.TM+e) THEN
                CA(i,j) = cl
              ELSE
                CA(i,j) = (cs+cl)/2 + heatLat/(2*e)
              ENDIF
            ENDDO
          ENDDO 



! C ATTENZIONE!!! CANCELLARE QUESTO E RIPRISTINARE L!ALTRO!!! ^
!           DO j =1, nX
!             DO i = 1, nY
!               CA(i,j)=2100
!             ENDDO
!           ENDDO 
! C ATTENZIONE!!! CANCELLARE QUESTO E RIPRISTINARE L!ALTRO!!! ^


C---------Calculate accumulation term for all cells
          DO j=1,nX
            DO i=1,nY
              acc(i,j) = rho(i,j)*V(i,j)*CA(i,j)/dt
!               rho_V_dt(i,j) = rho(i,j)*V(i,j)/dt
            ENDDO
          ENDDO




!           IF(TMPn(k).LE.TM+e.AND.TMPn(k).LE.TM-e)THEN
!             DO j=1,nXnY
!               DO i=1,nXnY
!                 Tmatrix(k,k) = 0.
!               ENDDO
!             ENDDO
!             DO k=1,nXnY
!               Tmatrix(k,k) = big
!             ENDDO
!           ELSE
C---------Matrix coefficients (using Taylor series expansion) for the 
C         computation of the nodal temperature field at iteration n
            IF(nY.GT.1)THEN
              CALL PentaDiagCoeff(acc(1,1),coeffx(1,1),coeffy(1,1),
     &                         Tmatrix(1,1),Axb(1),alfa,nX,nY,nXnY,n)
            ELSE
              CALL TriDiagCoeff(acc(1,1),coeffx(1,1),
     &                       Tmatrix(1,1),Axb(1),alfa,nX,nY,nXnY,n,
     &                       T_SCn(1),T_EASTn(1),T_WESTn(1))
            ENDIF
!           ENDIF

           



C---------Right Hand Side (for sub-iteration temperature equation system)
          DO j=1,NX
            DO i=1,NY   
              k=(j-1)*nY+i
              IF (j.EQ.1) THEN !general border cells 
                RHS(k) = acc(i,j)*TMPn(k)+
     &                  rho_V_dt(i,j)*(H(i,j)-Hn(k))+
     &                  alfa*Axb(i)*Tbound(i)
              ELSE !general cells 
                RHS(k) = acc(i,j)*TMPn(k)+
     &                  rho_V_dt(i,j)*(H(i,j)-Hn(k))
              ENDIF
            ENDDO
          ENDDO






C---------Call subroutines to solve Ax=B system to predict
C         the nodal temperature field at iteration n 
          IF(NY.GT.1)THEN
            CALL LUDCMS(Tmatrix(1,1),nXnY,mXmY,VETT(1),L)
            CALL LUBKSS(Tmatrix(1,1),nXnY,mXmY,VETT(1),RHS(1))
          ELSE
            CALL SolveTridiag(nX,T_WESTn(1),T_SCn(1),T_EASTn(1),
     &                            RHS(1),IER)
          ENDIF



C---------Update sub-iteration temperature
          DO j=1,NX
            DO i=1,NY   
              k=(j-1)*nY+i
              TMPn(k)=RHS(k) 
            ENDDO
          ENDDO



C---------CORRECTION
C---------Update nodal enthalpy (using Taylor series expansion)
          IF(nY.GT.1)THEN
            CALL IterationEnthalpy2D(coeffx(1,1),coeffy(1,1),
     &                              Tbound(1),TMPn(1),Hn(1),H(1,1),
     &                              Axb(1),alfa,
     &                              nX,nY,nXnY,n,
     &                              rho_V_dt(1,1))
          ELSE
            CALL IterationEnthalpy1D(coeffx(1,1),
     &                              Tbound(1),TMPn(1),Hn(1),H(1,1),
     &                              Axb(1),alfa,nX,nY,nXnY,n,
     &                              rho_V_dt(1,1))
          ENDIF




C---------Correction of temperature field (nth iteration), 
C         in order to ensure consistency with the enthalpy
          DO j=1,NX
            DO i=1,NY   
              k=(j-1)*nY+i
              IF (Hn(k).LT.cs*(Tm-e)) THEN
                TMPn(k) = Hn(k)/cs
              ELSEIF (Hn(k).GT.cl*(Tm+e)+heatLat) THEN
                TMPn(k) = (Hn(k)-(cs-cl)*Tm-heatLat)/cl
              ELSE 
                TMPn(k) = (Hn(k)+((cl-cs)/2+
     &           heatLat/(2*e))*(Tm-e))/
     &             ((cs+cl)/2+heatLat/(2*e))
              ENDIF
            ENDDO
          ENDDO



C---------Calculate residues 
          IF(nY.GT.1)THEN
            CALL Residual2D(coeffx(1,1),coeffy(1,1),
     &                       Tbound(1),TMPn(1),Hn(1),H(1,1),
     &                       Axb(1),alfa,nX,nY,nXnY,n,
     &                       rho_V_dt(1,1),R(1))
          ELSE
            CALL Residual1D(coeffx(1,1),
     &                       Tbound(1),TMPn(1),Hn(1),H(1,1),
     &                       Axb(1),alfa,nX,nY,nXnY,n,
     &                       rho_V_dt(1,1),R(1))
          ENDIF




C---------Mean upon residues
          RES = 0.
          DO k=1,nXnY
            RES = RES+ABS(R(k))
          ENDDO
          RES = RES/FLOAT(nXnY)




! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         j=1
!         DO i=1,nY
!           k=(j-1)*nY+i
!           TMPn(k)=50.-Tmi
!         ENDDO
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




        ENDDO
C=======END CONVERGENCE LOOP


C-------Update nodal temperature and enthalpy for the new time step
        DO j=1,nX
          DO i=1,nY
            k=(j-1)*nY+i ! cells' values are stored in axial direction 
            TMP(i,j) = TMPn(k) 
            H(i,j) = Hn(k) 
          ENDDO
        ENDDO


! C RILASSAMENTO:
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         DO j=1,nX
!           DO i=1,nY
!             TMP(i,j) = (TMP(i,j)+TMP_PREV(i,j))/2.
!           ENDDO
!         ENDDO
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       j=1
!       DO i=1,nY
!         TMP(i,j)=50.-Tmi
!       ENDDO
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




C-------Calculate thermal flux exchanged between DHW and PCM
        DO i=1,nY
          QPCM(i+nPCMbot-1) = Axb(i)*alfa*(TMP(i,1)-Tbound(i))
        ENDDO



      ENDDO
C=====END TIME LOOP



      RETURN      
      END









C=============================== ExplicitPCM ==========================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Calculates the heat flux exchanged between PCM modules and tank 
C     water for each layer where the PCM is located in. It solves the 
C     problem using the explicit Euler method.
C     Simplification hypothesis:
C       - PCM modules are considered cylindrical
C       - top and bottom are adiabatic surfaces
C       - no convection inside the modules
C       - no subcooling phenomena considered
C     
C     References:
C     W.J.Minkowycz, E.M.Sparrow, "Advances in Numerical Heat Transfer"
C     chapter 9 - V.R.Voller, "An overview of numerical methods
C     for solving Phase Change problems"
C
C======================================================================
 
      SUBROUTINE ExplicitPCM(ts,nX,nY,nNodes,nPCMbot,nPCMtop,
     &           dr,dPCMnode,
     &           V,Ax,Axb,Ay,
     &           dPCMcyl, hPCMcyl, 
     &           alfa,Tbound,Tmi,method,e,Mdot,
     &           cndcts,cndctl,
     &           H,TMP,
     &           heatLat,rhos,rhol,cs, cl,QPCM,Tm,
     &           Tbot,Ttop,  
     &           CrossArea)


      INTEGER max_nNodes,mX,mY,mXmY,mPCMsec
      REAL pi
      PARAMETER (max_nNodes=100)
      PARAMETER (mX=100, mY=100, mXmY=1000, mPCMsec=100)
      PARAMETER (pi=3.1415926535897932385)

C-----COUNTERS
      REAL dt              ! time step [s]
      REAL ts              ! time step [s]
      INTEGER i            ! radial direction counter [-]
      INTEGER j            ! axial direction counter [-]
      INTEGER k            ! cell counter [-]

C-----SPACE DISCRETIZATION and GEOMETRY
      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nNodes       ! number of nodes which the PCM tank is divided into
      INTEGER nPCMbot      ! tank node corresponding to the lower cylinder node
                           ! tank node which the cylinder bottom is located in
      INTEGER nPCMtop      ! tank node corresponding to the higher cylinder node
                           ! tank node which the cylinder top is located in
      REAL dPCMcyl         ! PCM cylinders diameter [m]
      REAL hPCMcyl         ! cylinder height [m]
      REAL dr              ! grid dimension in radial direction
      REAL dPCMnode(nY-1)  ! spatial step, cell height = distance among nodes [m]   
      REAL V(nY,nX)        ! cells volumes [m^3]
      REAL Ax(nY,nX-1)     ! CV faces in radial direction (east-west) [m^2]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL Ay(nX)          ! CV faces in axial direction  (north-south) [m^2]

C-----TANK CHARACTERISTICS
      REAL alfa            ! convection coefficient [W/(m^2 K)]
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section - 
                           ! boundary condition
      REAL Mdot(nNodes)    ! flow rate at each node (value i is from node i to node i+1) 
      REAL CrossArea

C-----PCM CHARACTERISTICS
      REAL cndcts          ! conductivity into solid [W/(m K)]
      REAL cndctl          ! conductivity into liquid [W/(m K)]
      REAL cndctx(mY,mX-1) ! conductivity on CV faces in radial direction [W/(m K)]
      REAL cndcty(mY-1,mX) ! conductivity on CV faces in axial direction [W/(m K)]
      REAL cndctP(mY,mX)   ! conductivity on CV's node [W/(m K)]
      REAL TMP(nY,nX)      ! cylinder cells temperature [°C]
      REAL H(nY,nX)        ! cylinder cells enthalpy
      REAL heatLat         ! latent heat [J/kg]
      REAL rhos            ! density into solid [kg/m^3]
      REAL rhol            ! density into liquid [kg/m^3]
      REAL rho(mY,mX)      ! density on CV's node [kg/m^3]
      REAL cs              ! specific heat of solid [J/(kg K)]
      REAL cl              ! specific heat of liquid [J/(kg K)]
      REAL Tmi             ! melting temperature  [°C]
      REAL Tm              ! scaling temperature


! C-----VALUES FOR EXPLICIT METHOD (CONVERGENCE CRITERION)
!       REAL conda 
!       REAL condb 
! !      REAL condc 
! !      REAL condd
!       REAL valAss


C-----VALUES FOR EXPLICIT METHOD (CONVERGENCE CRITERION)
      REAL dts             ! time step for solid phase [s]
      REAL dtl             ! time step for liquid phase [s] 


C-----OTHER
      REAL QPCM(max_nNodes)! haet transfer from immersed PCM [J]
      INTEGER nstpcm       ! number of pcm inner time steps for tank timesteps, 
                           ! to verify explicit condition
      INTEGER istpcm       ! iteration value for internal pcm time step
      INTEGER method       ! time integration method [-]
      REAL eps             ! constant to calculate conductivity at cells faces
      REAL coeffx(mY,mX)   ! cndctx*Ax/dr coefficient, calculated for all faces:
                           ! diffusive term
      REAL coeffy(mY,mX)   ! cndcty*Ay/dPCMnode coefficient, calculated for all 
                           ! north and south faces: diffusive term 
      REAL acc(mY,mX)      ! rho*V/dt coefficient, calculated for all cells: 
                           ! accumulation term 
      INTEGER N            ! variable for LU decomposition


      eps=1e-5



C-----STABILITY CONDITION for explicit method 
C     0.25 comes out from stability limit for central cells, 
C     cylindrical simmetry
      dts = 0.25*rhos*cs*dr**2/cndcts 
      dtl = 0.25*rhol*cl*dr**2/cndctl
      dt=MIN(dts,dtl)





! NoRoundUp:
!       nstpcm = ts/dt
C SCOMMENTARE SE SI VUOLE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C ma se si scommenta il grafico viene leggermente diverso 

! RoundUp:
C-----Round up internal time step so that there is an integral number of time steps
C-----within the system time step
C-----The 0.5 in the formula below is to make sure that the rounding is made upwards
C-----(equivalent to ceil function in C)
      nstpcm = NINT(0.499999+ts/dt)
      dt = ts/nstpcm
C SCOMMENTARE SE SI VUOLE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! C CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       nstpcm=60
!       dt=0.2 !ATTENZIONE! dt=0.2 se tank-time-step=12 secondi!!! quindi era sbagliato comunque!!
! C CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



C-----Sets to 0 the heat flux from the module, it is computed (averaged) 
C     at the end of the loop
      DO i=1,nY
          QPCM(i+nPCMbot-1) = 0.0
      ENDDO




C SCOMMENTARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C-------Calculate the convection coefficient ones per tank time-step
C       The calculation is done once per PCM time step. It takes account 
C       of PCM border cells mean temperature and tank whater mean  
C       temperature at the corresponding nodes.
        CALL convection_coeff(nX,nY,nNodes,nPCMbot,nPCMtop,
     &                      Tbound(1),
     &                      hPCMcyl,dPCMcyl,
     &                      Tmi,
     &                      dt,alfa,CrossArea,
     &                      Mdot(1),TMP(1,1))
C SCOMMENTARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







C=====TIME LOOP
      DO istpcm=1,nstpcm
    


C-------Calculate nodal conductivity and density
        DO j = 1, nX
          DO i= 1, nY
            IF (TMP(i,j).LT.0.)  THEN 
               cndctP(i,j) = cndcts
               rho(i,j)    = rhos 
            ELSEIF(TMP(i,j).GT.0.)  THEN
               cndctP(i,j) = cndctl
               rho(i,j)    = rhol 
            ELSE
               cndctP(i,j) = cndctl 
               rho(i,j)    = (rhos+rhol)/2 
            ENDIF
          ENDDO
        ENDDO


C-------Calculate diffusive terms
C       Use Kirchhoff approximation, see "Treatment of discontinuous 
C       thermal conductivity in control-volume solutions of phase-change
C       problems, "Voller and Swaminathan, Numerical Heat Transfer, 
C       Vol.24, No.B, pp. 161-180, September 1993

C-------Average conductivity on CV faces - radial direction, from border 
C       to axis (towards the east)
        DO j = 1, nX-1
          DO i= 1, nY
C           conducibility at east face of cell
            cndctx(i,j) = cndctP(i,j)+
     &                    (TMP(i,j+1)*(cndctP(i,j+1)-cndctP(i,j)))/
     &                    (TMP(i,j+1)-TMP(i,j)+eps)
            coeffx(i,j) = cndctx(i,j)*Ax(i,j)/dr
          ENDDO
        ENDDO

C-------Average conductivity on CV faces - axial direction, from bottom 
C       to top (towards the north)
        IF(NY.NE.1)THEN
          DO j = 1, nX
            DO i= 1, nY-1
C             conducibility at north face of cell
              cndcty(i,j) = cndctP(i,j)+
     &                      (TMP(i+1,j)*(cndctP(i+1,j)-cndctP(i,j)))/
     &                      (TMP(i+1,j)-TMP(i,j)+eps)
              coeffy(i,j) = cndcty(i,j)*Ay(j)/dPCMnode(i)
            ENDDO
          ENDDO
        ENDIF




C-----Calculate accumulation term for all cells
      DO j=1,nX
        DO i=1,nY
          acc(i,j) = rho(i,j)*V(i,j)/dt
        ENDDO
      ENDDO
  





   
C-------Calculate enthalpy for each cell 
        IF(nY.GT.1)THEN
          CALL Enthalpy2D(acc(1,1),coeffx(1,1),coeffy(1,1),Tbound(1),
     &                   Tbot,Ttop,TMP(1,1),H(1,1),Axb(1),alfa,
     &                   nX,nY,nXnY,n)
        ELSE
          CALL Enthalpy1D(acc(1,1),coeffx(1,1),Tbound(1),
     &                   Tbot,Ttop,TMP(1,1),H(1,1),Axb(1),alfa,
     &                   nX,nY,nXnY,n)
        ENDIF





C-------Calculate TMP for each cell 
        DO j=1,nX
          DO i=1,nY
	    IF (H(i,j).LT.(cs*Tm)) THEN
              TMP(i,j) = h(i,j)/cs
	    ELSEIF (H(i,j).GE.(cs*Tm).AND.h(i,j).LT.cs*       
     &                  Tm+heatLat) THEN
              TMP(i,j) = Tm
	    ELSE 
              TMP(i,j) = (H(i,j)-(cs-cl)*Tm-heatLat)/cl
	    ENDIF
	  ENDDO
        ENDDO






C-------Calculate the energy exchanged between DHW and PCM 
C       for the tank time step as sum of energy exchanged 
C       for all the PCM time steps
        DO i=1,nY
          QPCM(i+nPCMbot-1) = QPCM(i+nPCMbot-1)+
     &             (Axb(i)*alfa*(TMP(i,1)-Tbound(i)))*dt
        ENDDO




      ENDDO ! enddo on intertnal PCM time step
C=====END TIME LOOP


C-----Extract the average heat flux from PCM
      DO i=1,nY
          QPCM(i+nPCMbot-1) = QPCM(i+nPCMbot-1)/ts
      ENDDO



      RETURN      
      END










C======================== convection_coeff ============================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011  
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Calculates the convection coefficient between an immersed PCM 
C     module (border cells) and the tank fluid for the nodes of the 
C     tank where the module is collocated. The subroutine is adapted 
C     from the existing heat transfer calculation (immersed_HX).
C     
C     References:
C     Didier Thevenard, subroutine "immersed_HX_calc"
C
C======================================================================

      SUBROUTINE convection_coeff(nX,nY,nNodes,nPCMbot,nPCMtop,
     &      Tbound,
     &      hPCMcyl,dPCMcyl,
     &      Tmi,
     &      dt,alfa,CrossArea,
     &      Mdot,TMP)


 
      PARAMETER (max_nNodes=100)
      PARAMETER (mX=100, mY=100, mXmY=1000, mPCMsec=100)
      PARAMETER (pi=3.1415926535897932385) 


      REAL dt
      INTEGER i,j,k
      REAL TMP(nY,nX)      ! cylinder cells temperature [°C]
      REAL Tbound(nY)      ! tank layer temperature around PCM
      REAL TWATmed         ! water average temperature at PCM border
      REAL TPCMmed         ! PCM average temperature on cylinder border cells
      REAL TWMfilm         ! non scaled mean film temperature [C]
      REAL TsPCM           ! non scaled PCM wall mean temperature [C]
      REAL CrossArea       ! cross area of tank [m^2]
      REAL Volume          ! tank volume
      REAL Tmi
      REAL k_water         ! water conductivity [W/(m*K)]
      REAL rho_water       ! water density [kg/m^3]
      REAL Beta            ! water expansion coefficient
      REAL gBeta_n         ! water g*beta/ni^2 coeff at PCM border [1/(m^3*K)]
      REAL mu              ! dynamic viscosity [kg/(m*s)]
      REAL Cp_water        ! specific heat ow water
      REAL k_wall          ! water conductivity at wall temperature [W/(m*K)]
      REAL mu_wall         ! dynamic viscosity at wall temperature[kg/(m*s)]
      REAL Cp_wall         ! specific heat ow water at wall temperature
      REAL Vel             ! velocity of fluid in tank node [m/s]
      REAL Mdot(nNodes)    ! flow rate at each node (value i is from node i
                           ! to node i+1) [kg/s]
      REAL Mdot_med        ! average flow rate at cylinder border [kg/s] 
 
      REAL Gr              ! Grashof number 
      REAL Pr              ! Prandtl number []
      REAL Pr_wall         ! Prandtl number at wall temperature []

      REAL Ra              ! Rayleigh coefficient
      REAL Nu_n            ! Nusselt number for natural convection
      REAL Nu_f            ! Nusselt number for forced convection
      REAL Nu              ! Nusselt number for mixed convection
      REAL Re

      REAL C               ! auxiliary coefficient for Nusselt number calculation
      REAL n,m             ! auxiliary exponent for Nusselt number calculation
      REAL alfa            ! convection coefficient [W/(m^2 K)]

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nPCMbot      ! tank node corresponding to the lower cylinder node
                           ! tank node which the cylinder bottom is located in
      INTEGER nPCMtop      ! tank node corresponding to the higher cylinder node
                           ! tank node which the cylinder top is located in
      REAL hPCMcyl


C---- Functions
      INTEGER Bound                ! Bound a number by two others
      REAL glycol_water_density    ! Function for density of glycol-water mixture [kg/m3]
      REAL glycol_water_viscosity  ! Function for dynamic viscosity of glycol-water mixture [Pa s]
      REAL glycol_water_cond       ! Function for thermal conductivity of glycol [W/m/K]
      REAL glycol_water_cp         ! Function for specific heat or glycol-water [J/kg/K]
      REAL glycol_water_thermexp   ! Function for coefficient of thermal expansion of glycol-water [1/K]




     

C-----Water average temperature at PCM border
      TWATmed=0.
      DO i=1,nY
        TWATmed=TWATmed+Tbound(i)
      ENDDO
      TWATmed=TWATmed/nY

C-----Average temperature of PCM border cells
      TPCMmed=0.
      DO i=1, nY
        TPCMmed=TPCMmed+TMP(i,1)
      ENDDO
      TPCMmed=TPCMmed/nY



C-----since TWATmed and TPCMmed are scaled with Tmi the mean film  
C     temperature TWMfilm is introduced without scaling for properties
C     determination only
      TWMfilm = (TWATmed + TPCMmed )*0.5 + Tmi
      Beta = glycol_water_thermexp(0.,TWMfilm)
      rho_water = glycol_water_density(0., TWMfilm)
      k_water = glycol_water_cond(0.,TWMfilm)
      mu = glycol_water_viscosity(0.,TWMfilm)
      Cp_water = glycol_water_cp(0.,TWMfilm)
      gBeta_n = Beta * rho_water**2/mu**2
      Pr = Cp_water * mu / k_water

C-----for property evaluationa at the temperature of the border the non
C     scaled temperature is introduced
      TsPCM = TPCMmed + Tmi
      k_wall = glycol_water_cond(0.,TsPCM)
      mu_wall = glycol_water_viscosity(0.,TsPCM)
      Cp_wall = glycol_water_cp(0.,TsPCM)
      Pr_wall = Cp_wall * mu_wall / k_wall 


C-----Natural convection
      Gr = ABS(gBeta_n*(TWATmed-TPCMmed)*hPCMcyl**3)
      Ra = Gr * Pr
      Nu_n = (0.825+(0.387*Ra**(1./6.))/(1.+(0.492*Pr**(9./16.)))**
     &       (8./27.))**2.     


C-----Forced convection
      Mdot_med = 0.
      DO i=nPCMbot,nPCMtop
        Mdot_med=Mdot_med + Mdot(i)
      ENDDO
      Mdot_med=Mdot_med/nY

      Vel = Mdot_med/rho_water/CrossArea
      Re = ABS(Vel)*dPCMcyl/(mu/rho_water)
      if (Re .LT. 40.) then
        C = 0.75
        m = 0.4
      else if (Re .LT. 1000.) then
        C = 0.51
        m = 0.5
      else 
        C = 0.26
        m = 0.6
      endif
      if (Pr .LT. 10) then
        n = 0.37
      else
        n = 0.36
      endif
      Nu_f = C*(Re**m)*(Pr**n)*((Pr/Pr_wall)**0.25)
    
        
C-----Mixed convection
      Nu = ((Nu_n ** 3.) + (Nu_f ** 3.)) ** (1./3.)


C-----Convection coefficient
      alfa = Nu*k_water/hPCMcyl


! C ATTENZONE!!!!!! VALORE DA CANCELLARE!!!!!
!    alfa =100.
! C ATTENZONE!!!!!! VALORE DA CANCELLARE!!!!!


      RETURN      
      END









C======================== PentaDiagCoeff ==============================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011  
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Calculates and stores the matrix coefficients for the solution of 
C     the equation system to be generated for the implicit method - 
C     case 2D.
C
C======================================================================

      SUBROUTINE PentaDiagCoeff(acc,coeffx,coeffy,
     &                          Tmatrix,Axb,alfa,nX,nY,nXnY,n)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL Tmatrix(mXmY,mXmY) 
                           ! temperature matrix coefficients
      REAL T_SCn(mXmY)     ! temperature self-coupling coefficients, 
                           ! into a vector
      REAL T_EASTn(mXmY)   ! temperature cross-coupling coefficients (diagonals), into vectors
      REAL T_WESTn(mXmY)   ! temperature cross-coupling coefficients (diagonals), into vectors
      REAL T_NORTHn(mXmY)  ! temperature cross-coupling coefficients (diagonals), into vectors
      REAL T_SOUTHn(mXmY)  ! temperature cross-coupling coefficients (diagonals), into vectors
      REAL alfa            ! convection coefficient [W/(m^2 K)]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL coeffx(mY,mX)   ! cndctx*Ax/dr coefficient, calculated for all faces:
                           ! diffusive term
      REAL coeffy(mY,mX)   ! cndcty*Ay/dPCMnode coefficient, calculated for all north and south 
                           ! faces: diffusive term 
      REAL acc(mY,mX)      ! rho*V*CA/dt coefficient, calculated for all cells: accumulation term 




C-----write the sum of the accumulation (refered to the node) 
C     and diffusion (refered to the east, west, north, south faces) 
C     contributions into an arrey that'll be the principal diagonal. 
C-----write the diffusion (refered to the east, west, north, south faces) 
C     contributions into arreys that'll be the other 4 diagonals. 

C-----BOTTOM:
      i=1
C     BOTTOM OUTERMOST
      j=1
      k=(j-1)*nY+i
      T_SCn(k)    = acc(i,j)+coeffx(i,j)+coeffy(i,j)+
     &              alfa*Axb(i)
      T_EASTn(k)  = -coeffx(i,j)
      T_NORTHn(k) = -coeffy(i,j)
C     GENERAL BOTTOM
      DO j=2,nX-1
          k=(j-1)*nY+i
          T_SCn(k)    = acc(i,j)+coeffx(i,j)+coeffx(i,j-1)+
     &                  coeffy(i,j)
          T_EASTn(k)  = -coeffx(i,j)
          T_WESTn(k)  = -coeffx(i,j-1)
          T_NORTHn(k) = -coeffy(i,j)
      ENDDO
C     BOTTOM INNERMOST
      j=nX
      k=(j-1)*nY+i
      T_SCn(k)    = acc(i,j)+coeffx(i,j-1)+coeffy(i,j)
      T_WESTn(k)  = -coeffx(i,j-1)
      T_NORTHn(k) = -coeffy(i,j)
C-----TOP:
      i=nY
C     TOP OUTERMOST
      j=1
      k=(j-1)*nY+i
      T_SCn(k)    = acc(i,j)+coeffx(i,j)+coeffy(i-1,j)+
     &              alfa*Axb(i)
      T_EASTn(k)  = -coeffx(i,j)
      T_SOUTHn(k) = -coeffy(i-1,j)
C     GENERAL TOP
      DO j=2,nX-1
          k=(j-1)*nY+i
          T_SCn(k)    = acc(i,j)+coeffx(i,j)+coeffx(i,j-1)+
     &                  coeffy(i-1,j)
          T_EASTn(k)  = -coeffx(i,j)
          T_WESTn(k)  = -coeffx(i,j-1)
          T_SOUTHn(k) = -coeffy(i-1,j)        
      ENDDO      
C     TOP INNERMOST
      j=nX
      k=(j-1)*nY+i
      T_SCn(k)    = acc(i,j)+coeffx(i,j-1)+coeffy(i-1,j)
      T_WESTn(k)  = -coeffx(i,j-1)
      T_SOUTHn(k) = -coeffy(i-1,j)
      IF (nY.GT.2)THEN
C-------OUTERMOST:
	j=1
	DO i=2,nY-1
	    k=(j-1)*nY+i
	    T_SCn(k)    = acc(i,j)+coeffx(i,j)+
     &                    coeffy(i,j)+coeffy(i-1,j)+
     &                    alfa*Axb(i)
	    T_EASTn(k)  = -coeffx(i,j)
	    T_NORTHn(k) = -coeffy(i,j)
	    T_SOUTHn(k) = -coeffy(i-1,j)
	ENDDO
C-------INNERMOST:
	j=nX
	DO i=2,nY-1
	    k=(j-1)*nY+i
	    T_SCn(k)    = acc(i,j)+coeffx(i,j-1)+
     &                    coeffy(i,j)+coeffy(i-1,j)
	    T_WESTn(k)  = -coeffx(i,j-1)
	    T_NORTHn(k) = -coeffy(i,j)
	    T_SOUTHn(k) = -coeffy(i-1,j)
	ENDDO
C-------GENERAL CELL
	DO j=2,nX-1   
	  DO i=2,nY-1
	    k=(j-1)*nY+i
	    T_SCn(k)    = acc(i,j)+coeffx(i,j)+coeffx(i,j-1)+
     &                    coeffy(i,j)+coeffy(i-1,j)
	    T_EASTn(k)  = -coeffx(i,j)
	    T_WESTn(k)  = -coeffx(i,j-1)
	    T_NORTHn(k) = -coeffy(i,j)
	    T_SOUTHn(k) = -coeffy(i-1,j)
	  ENDDO
	ENDDO
      ENDIF

          


      DO j=1,nXnY
        DO I=1,nXnY
            Tmatrix(I,j)=0.
        ENDDO
      ENDDO


C-----Put these vectors into the matrix diagonals 
      DO k=1,nXnY
        Tmatrix(k,k)=T_SCn(k)
        IF (k.LT.nXnY-nY+1) THEN
          Tmatrix(k+nY,k)=T_WESTn(k+nY)
        ENDIF
        IF (k.GT.nY) THEN
          Tmatrix(k-nY,k)=T_EASTn(k-nY)
        ENDIF
        IF (k.GT.1) THEN
          Tmatrix(k-1,k)=T_NORTHn(k-1)
        ENDIF
        IF (k.LT.nXnY) THEN
          Tmatrix(k+1,k)=T_SOUTHn(k+1)
        ENDIF
      ENDDO


!          IF (n.EQ.1)THEN
!            write(*,*) "matrice Tmatrix"
!             DO j=1,nXnY
!               write(*,*) (Tmatrix(j,i),i=1,nXnY)
!             ENDDO
!          ENDIF

      RETURN
      END








C========================= TriDiagCoeff ===============================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011  
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Calculates and stores the matrix coefficients for the solution of 
C     the equation system to be generated for the implicit method - 
C     case 1D.
C
C======================================================================

      SUBROUTINE TriDiagCoeff(acc,coeffx,
     &                       Tmatrix,Axb,alfa,nX,nY,nXnY,n,
     &                       T_SCn,T_EASTn,T_WESTn)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL Tmatrix(mXmY,mXmY) 
                           ! temperature matrix coefficients
      REAL T_SCn(mXmY)     ! temperature self-coupling coefficients, into a vector
      REAL T_EASTn(mXmY)   ! temperature cross-coupling coefficients (diagonals), into vectors
      REAL T_WESTn(mXmY)   ! temperature cross-coupling coefficients (diagonals), into vectors
      REAL T_NORTHn(mXmY)  ! temperature cross-coupling coefficients (diagonals), into vectors
      REAL T_SOUTHn(mXmY)  ! temperature cross-coupling coefficients (diagonals), into vectors
      REAL alfa            ! convection coefficient [W/(m^2 K)]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL coeffx(mY,mX)   ! cndctx*Ax/dr coefficient, calculated for all faces:
                           ! diffusive term
      REAL acc(mY,mX)      ! rho*V*CA/dt coefficient, calculated for all cells: accumulation term 






C-----write the sum of the accumulation (refered to the node) 
C     and diffusion (refered to the east and west faces) 
C     contributions into an arrey that'll be the principal diagonal. 
C-----write the diffusion (refered to the east and west faces) 
C     contributions into arreys that'll be the other 2 diagonals. 

      i=1
C-----OUTERMOST:
      j=1
      T_SCn(j)    = acc(i,j)+coeffx(i,j)+alfa*Axb(i)
      T_EASTn(j)  = -coeffx(i,j)
C-----INNERMOST:
      j=nX
      T_SCn(j)    = acc(i,j)+coeffx(i,j-1)
      T_WESTn(j)  = -coeffx(i,j-1)
C-----GENERAL CELL
      DO j=2,nX-1   
          T_SCn(j)    = acc(i,j)+coeffx(i,j)+coeffx(i,j-1)
          T_EASTn(j)  = -coeffx(i,j)
          T_WESTn(j)  = -coeffx(i,j-1)
      ENDDO

          

C-----------Put these vectors into the matrix diagonals 
      DO j=1,nXnY
        DO i=1,nXnY
          Tmatrix(i,j)=0.
        ENDDO
      ENDDO

      DO k=1,nXnY
        Tmatrix(k,k)=T_SCn(k)
        IF (k.LT.nXnY-nY+1) THEN
          Tmatrix(k+nY,k)=T_WESTn(k+nY)
        ENDIF
        IF (k.GT.nY) THEN
          Tmatrix(k-nY,k)=T_EASTn(k-nY)
        ENDIF
      ENDDO


!          IF (n.EQ.1)THEN
!            write(*,*) "matrice Tmatrix"
!             DO j=1,nXnY
!               write(*,*) (Tmatrix(j,i),i=1,nXnY)
!             ENDDO
!          ENDIF

      RETURN
      END









C======================== IterationEnthalpy2D ========================= 
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011  
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Updates the nodal enthalpy for the nth iteration using the Taylor 
C     series expansion for the implicit method - case 2D.
C
C======================================================================

      SUBROUTINE IterationEnthalpy2D(coeffx,coeffy,
     &                              Tbound,TMPn,Hn,H,Axb,alfa,
     &                              nX,nY,nXnY,n,
     &                              rho_V_dt)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL alfa            ! convection coefficient [W/(m^2 K)]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL coeffx(mY,mX)   ! cndctx*Ax/dr coefficient, calculated for all faces:
                           ! diffusive term
      REAL coeffy(mY,mX)   ! cndcty*Ay/dPCMnode coefficient, calculated for all 
                           ! north and south faces: diffusive term 
      REAL rho_V_dt(mY,mX) ! rho*V/dt coefficient, calculated for all cells
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL TMPn(mXmY)      ! sub-iteration temperature 
                           ! unknown quantity arrey that shall become Ax=B system solution
      REAL Hn(mXmY)        ! sub-iteration entalpy 
      REAL H(nY,nX)        ! cylinder cells enthalpy




C-----BOTTOM:
      i=1
C     BOTTOM OUTERMOST
      j=1
      k=(j-1)*nY+i
      Hn(k) = (alfa*Axb(i)*(Tbound(i)-TMPn(k))+                 !west 
     &        coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+                 !east
     &        0.+                                               !south
     &        coeffy(i,j)*(TMPn(k+1)-TMPn(k))+                  !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
C     GENERAL BOTTOM
      DO j=2,nX-1
        k=(j-1)*nY+i
        Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+            !west 
     &          coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+               !east
     &          0.+                                             !south
     &          coeffy(i,j)*(TMPn(k+1)-TMPn(k))+                !north
     &          rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
      ENDDO
C     BOTTOM INNERMOST
      j=nX
      k=(j-1)*nY+i
      Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+              !west 
     &        0.+                                               !east
     &        0.+                                               !south
     &        coeffy(i,j)*(TMPn(k+1)-TMPn(k))+                  !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
C-----TOP:
      i=nY
C     TOP OUTERMOST
      j=1
      k=(j-1)*nY+i
      Hn(k) = (alfa*Axb(i)*(Tbound(i)-TMPn(k))+                 !west 
     &        coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+                 !east
     &        coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+                !south
     &        0.+                                               !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
C     GENERAL TOP
      DO j=2,nX-1
        k=(j-1)*nY+i
        Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+            !west 
     &          coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+               !east
     &          coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+              !south
     &          0.+                                             !north
     &          rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
      ENDDO
C     TOP INNERMOST
      j=nX
      k=(j-1)*nY+i
      Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+              !west 
     &        coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+                !south
     &        0.+                                               !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
      IF (nY.GT.2)THEN
C-------OUTERMOST:
      j=1
      DO i=2,nY-1
        k=(j-1)*nY+i
        Hn(k) = (alfa*Axb(i)*(Tbound(i)-TMPn(k))+               !west 
     &          coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+               !east
     &          coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+              !south
     &          coeffy(i,j)*(TMPn(k+1)-TMPn(k))+                !north
     &          rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
      ENDDO
C-------INNERMOST:
      j=nX
      DO i=2,nY-1
        k=(j-1)*nY+i
        Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+            !west 
     &          0.+                                             !east
     &          coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+              !south
     &          coeffy(i,j)*(TMPn(k+1)-TMPn(k))+                !north
     &          rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
      ENDDO
C-------GENERAL CELL
      DO j=2,nX-1
        DO i=2,nY-1   
          k=(j-1)*nY+I 
        Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+            !west 
     &          coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+               !east
     &          coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+              !south
     &          coeffy(i,j)*(TMPn(k+1)-TMPn(k))+                !north
     &          rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
        ENDDO
      ENDDO
      ENDIF


      RETURN
      END





C======================== IterationEnthalpy1D =========================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011  
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Updates the nodal enthalpy for the nth iteration using the Taylor 
C     series expansion for the implicit method - case 1D.
C
C======================================================================

      SUBROUTINE IterationEnthalpy1D(coeffx,
     &                              Tbound,TMPn,Hn,H,
     &                              Axb,alfa,nX,nY,nXnY,n,
     &                              rho_V_dt)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL alfa            ! convection coefficient [W/(m^2 K)]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL coeffx(mY,mX)   ! cndctx*Ax/dr coefficient, calculated for all faces:
                           ! diffusive term
      REAL rho_V_dt(mY,mX) ! rho*V/dt coefficient, calculated for all cells
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL TMPn(mXmY)      ! sub-iteration temperature 
                           ! unknown quantity arrey that shall become Ax=B system solution
      REAL Hn(mXmY)        ! sub-iteration entalpy 
      REAL H(nY,nX)        ! cylinder cells enthalpy





      i=1
C-----OUTERMOST:
      j=1
      Hn(j) = (alfa*Axb(i)*(Tbound(i)-TMPn(j))+       !west 
     &        coeffx(i,j)*(TMPn(j+nY)-TMPn(j))+       !east
     &        0.+                                     !south
     &        0.+                                     !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
C-----INNERMOST:
      j=nX
      Hn(j) = (coeffx(i,j-1)*(TMPn(j-nY)-TMPn(j))+    !west 
     &        0.+                                     !east
     &        0.+                                     !south
     &        0.+                                     !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
C-----GENERAL CELL
      DO j=2,nX-1
      Hn(j) = (coeffx(i,j-1)*(TMPn(j-nY)-TMPn(j))+    !west 
     &        coeffx(i,j)*(TMPn(j+nY)-TMPn(j))+       !east
     &        0.+                                     !south
     &        0.+                                     !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
      ENDDO



      RETURN
      END








C============================ Residual2D ==============================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011  
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Calculates the residuals of the discretized conserved enthalpy 
C     equation for the implicit method - case 2D.
C
C======================================================================

      SUBROUTINE Residual2D(coeffx,coeffy,
     &                          Tbound,TMPn,Hn,H,Axb,alfa,
     &                          nX,nY,nXnY,n,
     &                          rho_V_dt,R)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL alfa            ! convection coefficient [W/(m^2 K)]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL coeffx(mY,mX)   ! cndctx*Ax/dr coefficient, calculated for all faces:
                           ! diffusive term
      REAL coeffy(mY,mX)   ! cndcty*Ay/dPCMnode coefficient, calculated for all 
                           ! north and south faces: diffusive term 
      REAL rho_V_dt(mY,mX) ! rho*V/dt coefficient, calculated for all cells
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL TMPn(mXmY)      ! sub-iteration temperature 
                           ! unknown quantity arrey that shall become Ax=B system solution
      REAL Hn(mXmY)        ! sub-iteration entalpy 
      REAL H(nY,nX)        ! cylinder cells enthalpy
      REAL R(mXmY)         ! residual vector



C-----BOTTOM:
      i=1
C     BOTTOM OUTERMOST
      j=1
      k=(j-1)*nY+i
      R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &       alfa*Axb(i)*(TMPn(k)-Tbound(i))+                   !west 
     &       coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+                  !east
     &       0.+                                                !south
     &       coeffy(i,j)*(TMPn(k)-TMPn(k+1))                    !north
C     GENERAL BOTTOM
      DO j=2,nX-1
        k=(j-1)*nY+i
        R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &         coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+              !west 
     &         coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+                !east
     &         0.+                                              !south
     &         coeffy(i,j)*(TMPn(k)-TMPn(k+1))                  !north
      ENDDO
C     BOTTOM INNERMOST
      j=nX
      k=(j-1)*nY+i
      R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &       coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+                !west 
     &       0.+                                                !east
     &       0.+                                                !south
     &       coeffy(i,j)*(TMPn(k)-TMPn(k+1))                    !north
C-----TOP:
      i=nY
C     TOP OUTERMOST
      j=1
      k=(j-1)*nY+i
      R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &       alfa*Axb(i)*(TMPn(k)-Tbound(i))+                   !west 
     &       coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+                  !east
     &       coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+                 !south
     &       0.                                                 !north                                           
C     GENERAL TOP
      DO j=2,nX-1
        k=(j-1)*nY+i
        R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &         coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+              !west 
     &         coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+                !east
     &         coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+               !south
     &         0.                                               !north                                                
      ENDDO
C     TOP INNERMOST
      j=nX
      k=(j-1)*nY+i
      R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &       coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+                !west 
     &       0.+                                                !east
     &       coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+                 !south
     &       0.                                                 !north
      IF (nY.GT.2)THEN
C-----OUTERMOST:
      j=1
      DO i=2,nY-1
        k=(j-1)*nY+i
        R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &         alfa*Axb(i)*(TMPn(k)-Tbound(i))+                 !west 
     &         coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+                !east
     &         coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+               !south
     &         coeffy(i,j)*(TMPn(k)-TMPn(k+1))                  !north
      ENDDO
C-----INNERMOST:
      j=nX
      DO i=2,nY-1
        k=(j-1)*nY+i
        R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &         coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+              !west 
     &         0.+                                              !east
     &         coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+               !south
     &         coeffy(i,j)*(TMPn(k)-TMPn(k+1))                  !north
      ENDDO
C-----GENERAL CELL
      DO j=2,nX-1
        DO i=2,nY-1   
          k=(j-1)*nY+I 
          R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &           coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+            !west 
     &           coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+              !east
     &           coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+             !south
     &           coeffy(i,j)*(TMPn(k)-TMPn(k+1))                !north
        ENDDO
      ENDDO
      ENDIF


      RETURN
      END








C============================ Residual1D ==============================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011  
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Calculates the residuals of the discretized conserved enthalpy 
C     equation for the implicit method - case 1D.
C
C======================================================================

      SUBROUTINE Residual1D(coeffx,
     &                     Tbound,TMPn,Hn,H,Axb,alfa,
     &                     nX,nY,nXnY,n,
     &                     rho_V_dt,R)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL alfa            ! convection coefficient [W/(m^2 K)]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL coeffx(mY,mX)   ! cndctx*Ax/dr coefficient, calculated for all faces:
                           ! diffusive term
      REAL rho_V_dt(mY,mX) ! rho*V/dt coefficient, calculated for all cells
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL TMPn(mXmY)      ! sub-iteration temperature 
                           ! unknown quantity arrey that shall become Ax=B system solution
      REAL Hn(mXmY)        ! sub-iteration entalpy 
      REAL H(nY,nX)        ! cylinder cells enthalpy
      REAL R(mXmY)         ! residual vector


      i=1
C-----OUTERMOST:
      j=1
      R(j) = rho_V_dt(i,j)*(Hn(j)-H(i,j))+
     &       alfa*Axb(i)*(TMPn(k)-Tbound(i))+                   !west
     &       coeffx(i,j)*(TMPn(j)-TMPn(j+nY))+                  !east
     &       0.+                                                !south
     &       0.                                                 !north
C-----INNERMOST:
      j=nX
      R(j) = rho_V_dt(i,j)*(Hn(j)-H(i,j))+
     &       coeffx(i,j-1)*(TMPn(j)-Tbound(i))+                 !west 
     &       0.+                                                !east
     &       0.+                                                !south
     &       0.                                                 !north
C-----GENERAL CELL
      DO j=2,nX-1
        R(j) = rho_V_dt(i,j)*(Hn(j)-H(i,j))+
     &         coeffx(i,j-1)*(TMPn(j)-TMPn(j-nY))+              !west 
     &         coeffx(i,j)*(TMPn(j)-TMPn(j+nY))+                !east
     &         0.+                                              !south
     &         0.                                               !north
      ENDDO



      RETURN
      END









C============================ Enthalpy2D ==============================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011  
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Solves the explicit Euler numerical equation at each time step 
C     for the nodal enthalpy field (explicit method) - case 2D.
C
C======================================================================

      SUBROUTINE Enthalpy2D(acc,coeffx,coeffy,Tbound,
     &                   Tbot,Ttop,TMP,H,Axb,alfa,
     &                   nX,nY,nXnY,n)

      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)


      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL alfa            ! convection coefficient [W/(m^2 K)]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL TMP(nY,nX)      ! cylinder cells temperature [°C]
      REAL H(nY,nX)        ! cylinder cells enthalpy
      REAL Tbot            ! DHW temp in contact with the cylinder bottom
      REAL Ttop            ! DHW temp in contact with the cylinder top
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL coeffx(mY,mX)   ! cndctx*Ax/dr coefficient, calculated for all faces:
                           ! diffusive term
      REAL coeffy(mY,mX)   ! cndcty*Ay/dPCMnode coefficient, calculated for all north and south faces:
                           ! diffusive term 
      REAL acc(mY,mX)      ! rho*V/dt coefficient, calculated for all cells: accumulation term 





C---- For a cylynder with more than one layer in axial direction 
C---- Cylynder top and bottom are adiabatic surfaces                                                                  |

C---- Outermost bottom cell
      i=1
      j=1
      H(i,j) = (acc(i,j)*H(i,j)-
     &         (alfa*Axb(i)+                 !west,self-coupling
     &         coeffx(i,j)+                  !est,self-coupling
     &         0.+                           !south,self-coupling
     &         coeffy(i,j))                  !north,self-coupling
     &         *TMP(i,j)+
     &         alfa*Axb(i)*Tbound(i)+        !west,cross-coupling
     &         coeffx(i,j)*TMP(i,j+1)+       !est,cross-coupling
     &         0.*Tbot+                      !south,cross-coupling
     &         coeffy(i,j)*TMP(i+1,j))       !north,cross-coupling
     &         /acc(i,j)
               

C---- Innermost bottom cell
      i=1
      j=nX
      H(i,j) = (acc(i,j)*H(i,j)-
     &         (coeffx(i,j-1)+               !west,self-coupling
     &         0.+                           !south,self-coupling
     &         coeffy(i,j))                  !north,self-coupling
     &         *TMP(i,j)+ 
     &         coeffx(i,j-1)*TMP(i,j-1)+     !west,cross-coupling
     &         0.*Tbot+                      !south,cross-coupling
     &         coeffy(i,j)*TMP(i+1,j))       !north,cross-coupling
     &         /acc(i,j)

C---- Outermost top cell
      i=nY
      j=1
      H(i,j) = (acc(i,j)*H(i,j)-
     &         (alfa*Axb(i)+                 !west,self-coupling
     &         coeffx(i,j)+                  !est,self-coupling
     &         coeffy(i-1,j)+                !south,self-coupling
     &         0.)                           !north,self-coupling
     &         *TMP(i,j)+
     &         alfa*Axb(i)*Tbound(i)+        !west,cross-coupling
     &         coeffx(i,j)*TMP(i,j+1)+       !est,cross-coupling
     &         coeffy(i-1,j)*TMP(i-1,j)+     !south,cross-coupling
     &         0.*Ttop)                      !north,cross-coupling
     &         /acc(i,j)

C---- Innermost top cell
      i=nY
      j=nX
      H(i,j) = (acc(i,j)*H(i,j)-
     &         (coeffx(i,j-1)+               !west,self-coupling
     &         coeffy(i-1,j)+                !south,self-coupling
     &         0.)                           !north,self-coupling
     &         *TMP(i,j)+ 
     &         coeffx(i,j-1)*TMP(i,j-1)+     !west,cross-coupling
     &         coeffy(i-1,j)*TMP(i-1,j)+     !south,cross-coupling
     &         0.*Ttop)                      !north,cross-coupling
     &         /acc(i,j)

C---- Generic bottom cell
      i=1
      DO j=2,nX-1
        H(i,j) = (acc(i,j)*H(i,j)-                    
     &           (coeffx(i,j-1)+             !west,self-coupling
     &           coeffx(i,j)+                !est,self-coupling
     &           0.+                         !south,self-coupling
     &           coeffy(i,j))                !north,self-coupling
     &           *TMP(i,j)+ 
     &           coeffx(i,j-1)*TMP(i,j-1)+   !west,cross-coupling
     &           coeffx(i,j)*TMP(i,j+1)+     !est,cross-coupling
     &           0.*Tbot+                    !south,cross-coupling
     &           coeffy(i,j)*TMP(i+1,j))     !north,cross-coupling
     &           /acc(i,j)
      ENDDO

C---- Generic top cell
      i=nY
      DO j=2,nX-1
        H(i,j) = (acc(i,j)*H(i,j)-                        
     &           (coeffx(i,j-1)+             !west,self-coupling
     &           coeffx(i,j)+                !est,self-coupling
     &           coeffy(i-1,j)+              !south,self-coupling
     &           0.)                         !north,self-coupling
     &           *TMP(i,j)
     &           +coeffx(i,j-1)*TMP(i,j-1)+  !west,cross-coupling
     &           coeffx(i,j)*TMP(i,j+1)+     !est,cross-coupling
     &           coeffy(i-1,j)*TMP(i-1,j)+   !south,cross-coupling
     &           0.*Ttop)                    !north,cross-coupling
     &           /acc(i,j)
      ENDDO

C---- Generic outermost cell
      j=1
      DO i=2,nY-1
        H(i,j) = (acc(i,j)*H(i,j)-
     &           (alfa*Axb(i)+               !west,self-coupling
     &           coeffx(i,j)+                !est,self-coupling
     &           coeffy(i-1,j)+              !south,self-coupling
     &           coeffy(i,j))                !north,self-coupling
     &           *TMP(i,j)+
     &           alfa*Axb(i)*Tbound(i)+      !west,cross-coupling
     &           coeffx(i,j)*TMP(i,j+1)+     !est,cross-coupling
     &           coeffy(i-1,j)*TMP(i-1,j)+   !south,cross-coupling
     &           coeffy(i,j)*TMP(i+1,j))     !north,cross-coupling
     &           /acc(i,j)
      ENDDO

C---- Generic innermost cell
      j=nX
      DO i=2,nY-1
        H(i,j) = (acc(i,j)*H(i,j)-
     &           (coeffx(i,j-1)+             !west,self-coupling
     &           coeffy(i-1,j)+              !south,self-coupling
     &           coeffy(i,j))                !north,self-coupling
     &           *TMP(i,j)+                                    
     &           coeffx(i,j-1)*TMP(i,j-1)+   !west,cross-coupling
     &           coeffy(i-1,j)*TMP(i-1,j)+   !south,cross-coupling
     &           coeffy(i,j)*TMP(i+1,j))     !north,cross-coupling
     &           /acc(i,j)
      ENDDO

C---- Generic cell 
      DO j=2,nX-1 !generic cells in radial direction
        DO i=2,nY-1 !generic cells in axial direction
          H(i,j) = (acc(i,j)*H(i,j)-
     &             (coeffx(i,j-1)+           !west,self-coupling
     &             coeffx(i,j)+              !est,self-coupling
     &             coeffy(i-1,j)+            !south,self-coupling
     &             coeffy(i,j))              !north,self-coupling
     &             *TMP(i,j)+ 
     &             coeffx(i,j-1)*TMP(i,j-1)+ !west,cross-coupling
     &             coeffx(i,j)*TMP(i,j+1)+   !est,cross-coupling
     &             coeffy(i-1,j)*TMP(i-1,j)+ !south,cross-coupling
     &             coeffy(i,j)*TMP(i+1,j))   !north,cross-coupling
     &             /acc(i,j)     
        ENDDO  
      ENDDO



      RETURN
      END








C============================ Enthalpy1D ==============================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: October 2011  
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Solves the explicit Euler numerical equation at each time step 
C     for the nodal enthalpy field (explicit method) - case 1D.
C
C======================================================================

      SUBROUTINE Enthalpy1D(acc,coeffx,Tbound,
     &                   Tbot,Ttop,TMP,H,Axb,alfa,
     &                   nX,nY,nXnY,n)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)


      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL alfa            ! convection coefficient [W/(m^2 K)]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL TMP(nY,nX)      ! cylinder cells temperature [°C]
      REAL H(nY,nX)        ! cylinder cells enthalpy
      REAL Tbot            ! DHW temp in contact with the cylinder bottom
      REAL Ttop            ! DHW temp in contact with the cylinder top
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL coeffx(mY,mX)   ! cndctx*Ax/dr coefficient, calculated for all faces:
                           ! diffusive term
      REAL coeffy(mY,mX)   ! cndcty*Ay/dPCMnode coefficient, calculated for all 
                           ! north and south faces: diffusive term 
      REAL acc(mY,mX)      ! rho*V/dt coefficient, calculated for all cells:
                           ! accumulation term 


C---- For a cylynder with only one layer in axial direction, no conductive 
C     contribution top-bottom. Cylynder top and bottom are adiabatic surfaces
      IF (nY.EQ.1) THEN
        i=1 ! i doesn't change
        j=1 ! Outermost 
        H(1,j) = (acc(i,j)*H(i,j)- 
     &           (alfa*Axb(i)+
     &           coeffx(i,j)+
     &           0.+
     &           0.)
     &           *TMP(i,j)+
     &           alfa*Axb(i)*Tbound(i) + 
     &           coeffx(i,j)*TMP(i,j+1)+
     &           0.*Tbot +
     &           0.*Ttop) 
     &           /acc(i,j)
        j=nX ! Innermost 
        H(1,j) = (acc(i,j)*H(i,j)- 
     &           (coeffx(i,j-1)+
     &           0.+
     &           0.)
     &           *TMP(i,j)+ 
     &           coeffx(i,j-1)*TMP(i,j-1)+
     &           0.*Tbot+
     &           0.*Ttop)  
     &           /acc(i,j)
        DO j=2,nX-1 ! Generic
          H(1,j) = (acc(i,j)*H(i,j)-
     &             (coeffx(i,j-1)+ 
     &             coeffx(i,j)+ 
     &             0.+
     &             0.)
     &             *TMP(i,j)+ 
     &             coeffx(i,j-1)*TMP(i,j-1)+ 
     &             coeffx(i,j)*TMP(i,j+1)+
     &             0.*Tbot+
     &             0.*Ttop)
     &             /acc(i,j)
        ENDDO
      ENDIF


      RETURN
      END
