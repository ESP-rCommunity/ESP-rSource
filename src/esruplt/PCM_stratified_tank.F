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

      SUBROUTINE Immersed_PCM(ts,nX,nY,nNodes,nPCMbot,nPCMtop,
     &                       dr,dPCMnode,dPCMcyl,hPCMcyl,
     &                       V,Ax,Axb,Ay,
     &                       heightNode,hPCMbot,hPCMtop,  
     &                       heatLat,rhos,rhol,cs,cl,cndcts,cndctl,  
     &                       TMP,Tmi,Tstep_prev1,
     &                       method,e,Mdot,
     &                       H,CrossArea,QPCM)


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
      REAL alphaLat        ! convection coefficient for cylinder lateral surface [W/(m^2 K)]
      REAL alphaTop        ! convection coefficient for cylinder top surface [W/(m^2 K)]
      REAL alphaBot        ! convection coefficient for cylinder bottom surface [W/(m^2 K)]

      INTEGER nNodes       ! number of nodes what the PCM tank is divided into

      REAL hPCMcyl         ! PCM cylinder height [m]
      REAL dPCMcyl         ! PCM cylinder diameter [m]
      REAL hPCMbot         ! Position of the cylinder bottom [m]
      REAL hPCMtop         ! Position of the cylinder top [m]

      INTEGER nPCMbot      ! tank layer corresponding to the lower cylinder node
                           ! tank layer where the cylinder bottom is located in
      INTEGER nPCMtop      ! tank layer corresponding to the higher cylinder node
                           ! tank layer where the cylinder top is located in
      INTEGER nPCMhalf     ! tank layer corresponding to PCM half height
      REAL heightNode      ! tank CV height
      REAL dPCMnode(nY-1)  ! distance among nodes - bottom, generic, top          
      REAL Tbound(mY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL Tstep_prev(max_nNodes)    
                           ! tank layer temperature
      REAL Tstep_prev1(nNodes)
                           ! original tank layer temperature
      REAL Tbot            ! DHW temp at the layer what the PCM cylinder 
                           ! bottom surface is in contact with
      REAL Ttop            ! DHW temp at the layer what the PCM cylinder 
                           ! top surface is in contact with
      REAL Mdot(nNodes)    ! flow rate at each node (value i is from node i 
                           ! to node i+1)
C-----OTHER
      REAL QPCM(nNodes)    ! haet transfer from immersed PCM [J]
      INTEGER IC           ! tank time step counter

      SAVE IC


      Tm = 0.

C-----Deducts the scaled temperature of the tank nodes
      do i= 1,nNodes
        Tstep_prev(i) = Tstep_prev1(i)-Tmi
      enddo



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
     &                   dr,dPCMnode(1),dPCMcyl, hPCMcyl,
     &                   V(1,1),Ax(1,1),Axb(1),Ay(1),
     &                   heatLat,rhos,rhol,cs,cl,cndcts,cndctl,
     &                   TMP(1,1),Tbound(1),Tbot,Ttop,Tmi,Tm,
     &                   alphaLat,alphaTop,alphaBot,
     &                   method,e,Mdot(1),
     &                   H(1,1),CrossArea,QPCM(1))
      ELSEIF(method.EQ.2)THEN
        CALL ImplicitPCM(ts,nX,nY,nNodes,nPCMbot,nPCMtop,
     &                   dr,dPCMnode(1),dPCMcyl, hPCMcyl,
     &                   V(1,1),Ax(1,1),Axb(1),Ay(1),
     &                   heatLat,rhos,rhol,cs,cl,cndcts,cndctl,
     &                   TMP(1,1),Tbound(1),Tbot,Ttop,Tmi,Tm, 
     &                   alphaLat,alphaTop,alphaBot,
     &                   method,e,Mdot(1),
     &                   H(1,1),CrossArea,QPCM(1))
      ENDIF

      nPCMhalf = INT((nPCMbot+nPCMtop)/2)            !Tank layer corresponding to PCM half height

      IC = IC + 1 

C-----Print to an output file the following data have been calculated 
C     for all PCM time steps
      OPEN(UNIT=93,FILE='CellsTempFromRoutine.dat', 
     &                           FORM='FORMATTED', STATUS='UNKNOWN',
     &                           ACCESS='APPEND')
      write(93,"(I10,24E18.8)") IC,                                     !1st column: istant 
     &                        Tstep_prev(1)+Tmi,                        !2nd column: tank previous temperature at the 1st layer (tank bottom)
     &                        Tstep_prev(INT(nNodes/4))+Tmi,            !3rd column: tank previous temperature at the layer situated at 1/4 of tank height
     &                        Tstep_prev(INT(nNodes/2))+Tmi,            !4th column: tank previous temperature at the layer situated at 1/2 of tank height
     &                        Tstep_prev(nPCMbot)+Tmi,                  !5th column: tank previous temperature at the PCM bottom layer
     &                        Tstep_prev(INT((nPCMbot+nPCMtop)/2))+Tmi, !6th column: tank previous temperature at the PCM half height layer
     &                        Tstep_prev(nPCMtop)+Tmi,                  !7th column: tank previous temperature at the PCM top layer   
     &                        Tstep_prev(nNodes)+Tmi,                   !8th column: tank previous temperature at the top layer (tank top) 
     &                        QPCM(nPCMbot),                            !9th column: heat flux exchanged between PCM module and tank water at PCM bottom layer
     &                        QPCM(nPCMbot+1),                          !10th column: heat flux exchanged between PCM module and tank water layer at one layer heigher
     &                        QPCM(nPCMhalf),                           !11th column: heat flux exchanged between PCM module and tank water layer corresponding to PCM half height
     &                        QPCM(nPCMtop-1),                          !12th column: heat flux exchanged between PCM module and tank water at PCM to -1 layer
     &                        QPCM(nPCMtop),                            !13th column: heat flux exchanged between PCM module and tank water at PCM top layer
     &                        alphaLat,                                 !14th column: convection coefficient at lateral surface
     &                        alphaTop,                                 !15th column: convection coefficient at top surface
     &                        alphaBot,                                 !16th column: convection coefficient at bottom surface 
     &                        TMP(1,1)+Tmi,                             !17th column: PCM temperature at bottom layer for the border cell
     &                        TMP(1,INT(nX/2))+Tmi,                     !18th column: PCM temperature at bottom layer for an intermediate cell 
     &                        TMP(1,nX)+Tmi,                            !19th column: PCM temperature at bottom layer for the inner cell  
     &                        TMP(INT(nY/2),1)+Tmi,                     !20h column: PCM temperature at intermediate layer for the border cell
     &                        TMP(INT(nY/2),INT(nX/2))+Tmi,             !21th column: PCM temperature at intermediate layer for an intermediate cell 
     &                        TMP(INT(nY/2),nX)+Tmi,                    !22th column: PCM temperature at intermediate layer for the inner cell 
     &                        TMP(nY,1)+Tmi,                            !23th column: PCM temperature at top layer for the border cell 
     &                        TMP(nY,INT(nX/2))+Tmi,                    !24th column: PCM temperature at top layer for an intermediate cell 
     &                        TMP(nY,nX)+Tmi                            !25th column: PCM temperature at top layer for the inner cell  
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
     &                       dr,dPCMnode,dPCMcyl,hPCMcyl,
     &                       V,Ax,Axb,Ay,
     &                       heatLat,rhos,rhol,cs,cl,cndcts,cndctl,
     &                       TMP,Tbound,Tbot,Ttop,Tmi,Tm, 
     &                       alphaLat,alphaTop,alphaBot,
     &                       method,e,Mdot,
     &                       H,CrossArea,QPCM)


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
      INTEGER nNodes       ! number of nodes what the PCM tank is divided into
      INTEGER nPCMbot      ! tank node corresponding to the lower cylinder node
                           ! tank node where the cylinder bottom is located in
      INTEGER nPCMtop      ! tank node corresponding to the higher cylinder node
                           ! tank node where the cylinder top is located in
      REAL hPCMcyl         ! cylinder height [m]
      REAL dPCMcyl         ! PCM cylinders diameter [m]
      REAL dr              ! grid dimension in radial direction
      REAL dPCMnode(nY-1)  ! spatial step, cell height = distance among nodes [m]   
      REAL V(nY,nX)        ! cells volumes [m^3]
      REAL Ax(nY,nX-1)     ! CV faces in radial direction (east-west) [m^2]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW [m^2]
      REAL Ay(nX)          ! CV faces in axial direction  (north-south) [m^2]

C-----TANK CHARACTERISTICS
      REAL alphaLat        ! convection coefficient [W/(m^2 K)]
      REAL alphaTop        ! convection coefficient for cylinder top surface [W/(m^2 K)]
      REAL alphaBot        ! convection coefficient for cylinder bottom surface [W/(m^2 K)]
      REAL Tbound(nY)      ! DHW layer temperature in contact with each PCM section - 
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
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL coeffy(mY,mX)   ! = cndcty(?,?)*Ay(?)/dPCMnode(?) 
                           ! coefficient calculated for all north and south faces between cells; 
                           ! diffusive term
      REAL convLat(mY)     ! alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(mX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(mX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
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
      REAL acc(mY,mX)      ! = rho(?,?)*V(?,?)*CA(?,?)/dt 
                           ! coefficient calculated for all cells; accumulation term 
      REAL rho_V_dt(mY,mX) ! = rho(?,?)*V(?,?)/dt 
                           ! coefficient calculated for all cells
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




      nXnY = nX*nY


      eps = 1E-5


      dt = ts

      
            

C-----Sets to 0 the heat flux from the module, it is computed (averaged)
C     at the end of the loop

      DO i=1,nNodes
        QPCM(i)=0.0
      ENDDO



C-----Calculate the convection coefficient ones per tank time-step
C     The calculation is done once per PCM time step. It takes account 
C     of PCM border cells mean temperature and tank whater mean  
C     temperature at the corresponding nodes.
      CALL convection_coeff(nX,nY,nNodes,nPCMbot,nPCMtop,
     &                      Tbound(1),
     &                      hPCMcyl,dPCMcyl,
     &                      Tmi,dt,
     &                      alphaLat,alphaTop,alphaBot,
     &                      CrossArea,Mdot(1),TMP(1,1))





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

C-------Coefficients for the border cells convective thermal exchange 
C-------Lateral surface cells
        DO i=1,nY
          convLat(i) = alphaLat*Axb(i)
        ENDDO
C-------Top and bottom surface cells
        DO j=1,nX
          convBot(j) = alphaBot*Ay(j)
          convTop(j) = alphaTop*Ay(j)
        ENDDO


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



C---------Calculate accumulation term for all cells
          DO j=1,nX
            DO i=1,nY
              acc(i,j) = rho(i,j)*V(i,j)*CA(i,j)/dt
            ENDDO
          ENDDO




C---------Matrix coefficients (using Taylor series expansion) for the 
C         computation of the nodal temperature field at iteration n
          IF(nY.GT.1)THEN
            CALL PentaDiagCoeff(acc(1,1),coeffx(1,1),coeffy(1,1),
     &                         Tmatrix(1,1),
     &                         convLat(1),convTop(1),convBot(1),
     &                         nX,nY,nXnY,n)
          ELSE
            CALL TriDiagCoeff(acc(1,1),coeffx(1,1),Tmatrix(1,1),
     &                       convLat(1),convTop(1),convBot(1),
     &                       nX,nY,nXnY,n,
     &                       T_SCn(1),T_EASTn(1),T_WESTn(1))
          ENDIF





C---------Calculate right hand side (known values)      
          CALL PCM_RHS(Tbound(1),Ttop,Tbot,TMPn(1),Hn(1),H(1,1),
     &                 convLat(1),convTop(1),convBot(1),
     &                 nX,nY,nXnY,acc(1,1),rho_V_dt(1,1),RHS(1))






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
     &                              Tbound(1),Ttop,Tbot,TMPn(1),
     &                              Hn(1),H(1,1),
     &                              convLat(1),convTop(1),convBot(1),
     &                              nX,nY,nXnY,rho_V_dt(1,1))
          ELSE
            CALL IterationEnthalpy1D(coeffx(1,1),
     &                              Tbound(1),Ttop,Tbot,TMPn(1),
     &                              Hn(1),H(1,1),
     &                              convLat(1),convTop(1),convBot(1),
     &                              nX,nY,nXnY,rho_V_dt(1,1))
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
     &                       Tbound(1),Ttop,Tbot,TMPn(1),Hn(1),H(1,1),
     &                       convLat,convTop,convBot,
     &                       nX,nY,nXnY,
     &                       rho_V_dt(1,1),R(1))
          ELSE
            CALL Residual1D(coeffx(1,1),
     &                       Tbound(1),Ttop,Tbot,TMPn(1),Hn(1),H(1,1),
     &                       convLat,convTop,convBot,
     &                       nX,nY,nXnY,
     &                       rho_V_dt(1,1),R(1))
          ENDIF




C---------Mean upon residues
          RES = 0.
          DO k=1,nXnY
            RES = RES+ABS(R(k))
          ENDDO
          RES = RES/FLOAT(nXnY)




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



C-------Calculate thermal flux exchanged between DHW and PCM

C-------PCM module lateral surface
        DO i=1,nY
          QPCM(i+nPCMbot-1) = Axb(i)*alphaLat*(TMP(i,1)-Tbound(i))
        ENDDO
C-------PCM module bottom and top surface
        DO j=1,nX
          QPCM(nPCMbot) = QPCM(nPCMbot)+
     &                    (Ay(j)*alphaBot*(TMP(1,j)-Tbot))
          QPCM(nPCMtop) = QPCM(nPCMtop)+
     &                    (Ay(j)*alphaTop*(TMP(nY,j)-Ttop))
        ENDDO





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
     &                      dr,dPCMnode,dPCMcyl, hPCMcyl,
     &                      V,Ax,Axb,Ay,
     &                      heatLat,rhos,rhol,cs, cl,cndcts,cndctl,
     &                      TMP,Tbound,Tbot,Ttop,Tmi,Tm, 
     &                      alphaLat,alphaTop,alphaBot,
     &                      method,e,Mdot,
     &                      H,CrossArea,QPCM)


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
      INTEGER nNodes       ! number of nodes what the PCM tank is divided into
      INTEGER nPCMbot      ! tank node corresponding to the lower cylinder node
                           ! tank node where the cylinder bottom is located in
      INTEGER nPCMtop      ! tank node corresponding to the higher cylinder node
                           ! tank node where the cylinder top is located in
      REAL dPCMcyl         ! PCM cylinders diameter [m]
      REAL hPCMcyl         ! cylinder height [m]
      REAL dr              ! grid dimension in radial direction
      REAL dPCMnode(nY-1)  ! spatial step, cell height = distance among nodes [m]   
      REAL V(nY,nX)        ! cells volumes [m^3]
      REAL Ax(nY,nX-1)     ! CV faces in radial direction (east-west) [m^2]
      REAL Axb(nY)         ! border face of external CV, in contact with DHW
      REAL Ay(nX)          ! CV faces in axial direction  (north-south) [m^2]

C-----TANK CHARACTERISTICS
      REAL alphaLat           ! convection coefficient [W/(m^2 K)]
      REAL alphaTop        ! convection coefficient for cylinder top surface [W/(m^2 K)]
      REAL alphaBot        ! convection coefficient for cylinder bottom surface [W/(m^2 K)]
      REAL Tbound(nY)      ! DHW layer temperature in contact with each PCM section - 
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



C-----VALUES FOR EXPLICIT METHOD CONVERGENCE CRITERION
      REAL dts             ! time step for solid phase [s]
      REAL dtl             ! time step for liquid phase [s] 


C-----OTHER
      REAL QPCM(max_nNodes)! haet transfer from immersed PCM [J]
      INTEGER nstpcm       ! number of pcm inner time steps for tank timesteps, 
                           ! to verify explicit condition
      INTEGER istpcm       ! iteration value for internal pcm time step
      INTEGER method       ! time integration method [-]
      REAL eps             ! constant to calculate conductivity at cells faces
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL coeffy(mY,mX)   ! = cndcty(?,?)*Ay(?)/dPCMnode(?) 
                           ! coefficient calculated for all north and south faces between cells; 
                           ! diffusive term
      REAL convLat(mY)     ! alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(mX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(mX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
      REAL acc(mY,mX)      ! = rho(?,?)*V(?,?)/dt  
                           ! coefficient calculated for all cells; accumulation term 
      INTEGER N            ! variable for LU decomposition


      eps=1e-5



C-----STABILITY CONDITION for explicit method 
C     0.25 comes out from stability limit for central cells, 
C     cylindrical simmetry
      dts = 0.25*rhos*cs*dr**2/cndcts 
      dtl = 0.25*rhol*cl*dr**2/cndctl
      dt=MIN(dts,dtl)






C-----Round up internal time step so that there is an integral number 
C     of time steps within the system time step
C     The 0.5 is to make sure that the rounding is made upwards
      nstpcm = NINT(0.499999+ts/dt)
      dt = ts/nstpcm




C-----Sets to 0 the heat flux from the module, it is computed (averaged) 
C     at the end of the loop

C-----PCM module lateral surface
      DO i=1,nY
        QPCM(i+nPCMbot-1) = 0.0
      ENDDO
C-----PCM module bottom and top surface
      QPCM(nPCMbot) = 0.0
      QPCM(nPCMtop) = 0.0




C-------Calculate the convection coefficient ones per tank time-step
C       The calculation is done once per PCM time step. It takes account 
C       of PCM border cells mean temperature and tank whater mean  
C       temperature at the corresponding nodes.
        CALL convection_coeff(nX,nY,nNodes,nPCMbot,nPCMtop,
     &                      Tbound(1),
     &                      hPCMcyl,dPCMcyl,
     &                      Tmi,dt,
     &                      alphaLat,alphaTop,alphaBot,
     &                      CrossArea,Mdot(1),TMP(1,1))








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


C-------Coefficients for the border cells convective thermal exchange 
C-------Lateral surface cells
        DO i=1,nY
          convLat(i) = alphaLat*Axb(i)
        ENDDO
C-------Top and bottom surface cells
        DO j=1,nX
          convBot(j) = alphaBot*Ay(j)
          convTop(j) = alphaTop*Ay(j)
        ENDDO



C-----Calculate accumulation term for all cells
      DO j=1,nX
        DO i=1,nY
          acc(i,j) = rho(i,j)*V(i,j)/dt
        ENDDO
      ENDDO
  





   
C-------Calculate enthalpy for each cell 
        IF(nY.GT.1)THEN
          CALL Enthalpy2D(acc(1,1),coeffx(1,1),coeffy(1,1),Tbound(1),
     &                   Ttop,Tbot,TMP(1,1),H(1,1),
     &                   convLat(1),convTop(1),convBot(1),
     &                   nX,nY,nXnY,n)
        ELSE
          CALL Enthalpy1D(acc(1,1),coeffx(1,1),
     &                   Tbound(1),Ttop,Tbot,TMP(1,1),H(1,1),
     &                   convLat(1),convTop(1),convBot(1),
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

C-------PCM module lateral surface
        DO i=1,nY
          QPCM(i+nPCMbot-1) = QPCM(i+nPCMbot-1)+
     &             (Axb(i)*alphaLat*(TMP(i,1)-Tbound(i)))*dt
        ENDDO
C-------PCM module bottom and top surface
        DO j=1,nX
          QPCM(nPCMbot) = QPCM(nPCMbot)+
     &             (Ay(j)*alphaBot*(TMP(1,j)-Tbot))*dt
          QPCM(nPCMtop) = QPCM(nPCMtop)+
     &             (Ay(j)*alphaTop*(TMP(nY,j)-Ttop))*dt
        ENDDO



      ENDDO ! enddo on intertnal PCM time step
C=====END TIME LOOP


C-----Extract the average heat flux from PCM

C-----PCM module lateral surface
      DO i=1,nY
          QPCM(i+nPCMbot-1) = QPCM(i+nPCMbot-1)/ts
      ENDDO
C-----PCM module bottom and top surface
      QPCM(nPCMbot) = QPCM(nPCMbot)/ts
      QPCM(nPCMtop) = QPCM(nPCMtop)/ts



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
     &      Tmi,dt,
     &      alphaLat,alphaTop,alphaBot,
     &      CrossArea,Mdot,TMP)


 
      PARAMETER (max_nNodes=100)
      PARAMETER (mX=100, mY=100, mXmY=1000, mPCMsec=100)
      PARAMETER (pi=3.1415926535897932385) 


      REAL dt
      INTEGER i,j,k
      REAL TMP(nY,nX)      ! cylinder cells temperature [°C]


      REAL CrossArea       ! cross area of tank [m^2]
      REAL Volume          ! tank volume
      REAL Tmi

C---- Calculation on the lateral cylinder surface:
      REAL Tbound(nY)      ! DHW layer temperature around PCM [°C]
      REAL TWATmed         ! DHW average temperature at PCM lateral surface [°C]
      REAL TPCMmedLat      ! PCM average temperature on cylinder lateral border cells [°C]
      REAL TWMfilmLat      ! non scaled mean film temperature for lateral cylinder surface [°C]
      REAL TsPCMLat        ! non scaled PCM wall mean temperature [°C]
      REAL k_waterLat      ! water conductivity [W/(m*K)]
      REAL rho_waterLat    ! water density [kg/m^3]
      REAL BetaLat         ! water expansion coefficient 
      REAL gBeta_nLat      ! water g*beta/ni^2 coeff [1/(m^3*K)]
      REAL muLat           ! dynamic viscosity [kg/(m*s)]
      REAL Cp_waterLat     ! specific heat ow water [J/(kg*K)]
      REAL PrLat           ! Prandtl number []
      REAL k_wallLat       ! water conductivity at wall temperature [W/(m*K)]
      REAL mu_wallLat      ! dynamic viscosity at wall temperature [kg/(m*s)]
      REAL Cp_wallLat      ! specific heat ow water at wall temperature [J/(kg*K)]
      REAL Pr_wallLat      ! Prandtl number at wall temperature []
      REAL GrLat           ! Grashof number for vertical flat surface []
      REAL alphaLat           ! convection coefficient on PCM vertical surface [W/(m^2 K)]

C---- Calculation on the top cylinder surface:
      REAL Ttop            ! DHW temp at the layer what the PCM cylinder 
                           ! top surface is in contact with [°C]
      REAL TPCMmedTop      ! PCM average temperature on cylinder top border cells [°C]
      REAL TWMfilmTop      ! non scaled mean film temperature for top cylinder surface [°C] 
      REAL TsPCMTop        ! non scaled PCM wall mean temperature [°C]
      REAL k_waterTop      ! water conductivity [W/(m*K)]
      REAL rho_waterTop    ! water density [kg/m^3]
      REAL BetaTop         ! water expansion coefficient 
      REAL gBeta_nTop      ! water g*beta/ni^2 coeff [1/(m^3*K)]
      REAL muTop           ! dynamic viscosity [kg/(m*s)]
      REAL Cp_waterTop     ! specific heat ow water [J/(kg*K)]
      REAL PrTop           ! Prandtl number []
      REAL k_wallTop       ! water conductivity at wall temperature [W/(m*K)]
      REAL mu_wallTop      ! dynamic viscosity at wall temperature [kg/(m*s)]
      REAL Cp_wallTop      ! specific heat ow water at wall temperature [J/(kg*K)]
      REAL Pr_wallTop      ! Prandtl number at wall temperature []
      REAL GrTop           ! Grashof number for horizontal (top) flat surface []
      REAL alphaTop        ! convection coefficient on PCM top (horizontal) surface [W/(m^2 K)]

C---- Calculation on the bottom cylinder surface 
      REAL Tbot            ! DHW temp at the layer what the PCM cylinder 
                           ! bottom surface is in contact with [°C]
      REAL TPCMmedBot      ! PCM average temperature on cylinder top border cells [°C]
      REAL TWMfilmBot      ! non scaled mean film temperature for bottom cylinder surface [°C] 
      REAL TsPCMBot        ! non scaled PCM wall mean temperature [°C]
      REAL k_waterBot      ! water conductivity [W/(m*K)]
      REAL rho_waterBot    ! water density [kg/m^3] 
      REAL BetaBot         ! water expansion coefficient 
      REAL gBeta_nBot      ! water g*beta/ni^2 coeff [1/(m^3*K)]
      REAL muBot           ! dynamic viscosity [kg/(m*s)]
      REAL Cp_waterBot     ! specific heat ow water [J/(kg*K)]
      REAL PrBot           ! Prandtl number []
      REAL k_wallBot       ! water conductivity at wall temperature [W/(m*K)]
      REAL mu_wallBot      ! dynamic viscosity at wall temperature [kg/(m*s)]
      REAL Cp_wallBot      ! specific heat ow water at wall temperature [J/(kg*K)]
      REAL Pr_wallBot      ! Prandtl number at wall temperature []
      REAL GrBot           ! Grashof number for horizontal (bottom) flat surface []
      REAL alphaBot        ! convection coefficient on PCM bottom (horizontal) surface [W/(m^2 K)]



      REAL Vel             ! velocity of fluid in tank node [m/s]
      REAL Mdot(nNodes)    ! flow rate at each node (value i is from node i
                           ! to node i+1) [kg/s]
      REAL Mdot_med        ! average flow rate at cylinder border [kg/s] 
 




      REAL RaLat           ! Rayleigh coefficient for lateral (vertical) flat surface
      REAL RaTop           ! Rayleigh coefficient for top (horizontal) flat surface
      REAL RaBot           ! Rayleigh coefficient for bottom (horizontal) flat surface
      REAL Nu_nLat         ! Nusselt number for natural convection for lateral (vertical) flat surface
      REAL Nu_fLat         ! Nusselt number for forced convection (only for lateral flat surface)
      REAL NuLat           ! Nusselt number for mixed convection for lateral (vertical) flat surface
      REAL NuTop           ! Nusselt number for natural convection for top (horizontal) flat surface
      REAL Re

      REAL C               ! auxiliary coefficient for Nusselt number calculation
      REAL n,m             ! auxiliary exponent for Nusselt number calculation
      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nPCMbot      ! tank node corresponding to the lower cylinder node
                           ! tank node where the cylinder bottom is located in
      INTEGER nPCMtop      ! tank node corresponding to the higher cylinder node
                           ! tank node where the cylinder top is located in
      REAL hPCMcyl         ! PCM cylinder height [m]
      REAL dPCMcyl         ! PCM cylinder diameter [m]
      REAL geomRatio       ! Characteristic lenght for the top/bottom cylinder surface


C---- Functions
      INTEGER Bound                ! Bound a number by two others
      REAL glycol_water_density    ! Function for density of glycol-water mixture [kg/m3]
      REAL glycol_water_viscosity  ! Function for dynamic viscosity of glycol-water mixture [Pa s]
      REAL glycol_water_cond       ! Function for thermal conductivity of glycol [W/m/K]
      REAL glycol_water_cp         ! Function for specific heat or glycol-water [J/kg/K]
      REAL glycol_water_thermexp   ! Function for coefficient of thermal expansion of glycol-water [1/K]




     

C-----Water average temperature at PCM lateral border
      TWATmed=0.
      DO i=1,nY
        TWATmed=TWATmed+Tbound(i)
      ENDDO
      TWATmed=TWATmed/nY

C-----Average temperature of PCM border cells
      TPCMmedLat=0.
      DO i=1, nY
        TPCMmedLat=TPCMmedLat+TMP(i,1)
      ENDDO
      TPCMmedLat=TPCMmedLat/nY

C-----Average temperature of PCM top cells
      TPCMmedTop=0.
      DO j=1, nX
        TPCMmedTop=TPCMmedTop+TMP(nY,j)
      ENDDO
      TPCMmedTop=TPCMmedTop/nX

C-----Average temperature of PCM bottom cells
      TPCMmedBot=0.
      DO j=1, nX
        TPCMmedBot=TPCMmedBot+TMP(1,j)
      ENDDO
      TPCMmedBot=TPCMmedBot/nX 



C-----Since TWATmed (or Ttop or Ttop) and TPCMmed- 
C     are scaled with Tmi, the mean film temperature TWMfilm- 
C     is introduced without scaling for properties
C     determination only

C-----Calculation next to the lateral cylinder surface
      TWMfilmLat = (TWATmed + TPCMmedLat )*0.5 + Tmi
      BetaLat = glycol_water_thermexp(0.,TWMfilmLat)
      rho_waterLat = glycol_water_density(0., TWMfilmLat)
      k_waterLat = glycol_water_cond(0.,TWMfilmLat)
      muLat = glycol_water_viscosity(0.,TWMfilmLat)
      Cp_waterLat = glycol_water_cp(0.,TWMfilmLat)
      gBeta_nLat = BetaLat * rho_waterLat**2/muLat**2
      PrLat = Cp_waterLat * muLat / k_waterLat

C-----Calculation next to the top cylinder surface
      TWMfilmTop = (Ttop + TPCMmedTop )*0.5 + Tmi
      BetaTop = glycol_water_thermexp(0.,TWMfilmTop)
      rho_waterTop = glycol_water_density(0., TWMfilmTop)
      k_waterTop = glycol_water_cond(0.,TWMfilmTop)
      mu = glycol_water_viscosity(0.,TWMfilmTop)
      Cp_waterTop = glycol_water_cp(0.,TWMfilmTop)
      gBeta_nTop = BetaTop * rho_waterTop**2/mu**2
      PrTop = Cp_waterTop * mu / k_waterTop

C-----Calculation next to the bottom cylinder surface
      TWMfilmBot = (Ttop + TPCMmedTop )*0.5 + Tmi
      BetaBot = glycol_water_thermexp(0.,TWMfilmBot)
      rho_waterBot = glycol_water_density(0., TWMfilmBot)
      k_waterBot = glycol_water_cond(0.,TWMfilmBot)
      mu = glycol_water_viscosity(0.,TWMfilmBot)
      Cp_waterBot = glycol_water_cp(0.,TWMfilmBot)
      gBeta_nBot = BetaBot * rho_waterBot**2/mu**2
      PrBot = Cp_waterBot * mu / k_waterBot




C-----For property evaluation at the temperature of the border 
C     the non scaled temperature is introduced

C-----Calculation next to the lateral cylinder surface
      TsPCMLat = TPCMmedLat + Tmi
      k_wallLat = glycol_water_cond(0.,TsPCMLat)
      mu_wallLat = glycol_water_viscosity(0.,TsPCMLat)
      Cp_wallLat = glycol_water_cp(0.,TsPCMLat)
      Pr_wallLat = Cp_wallLat * mu_wallLat / k_wallLat 

C-----Calculation next to the top cylinder surface
      TsPCMTop = Ttop + Tmi
      k_wallTop = glycol_water_cond(0.,TsPCMTop)
      mu_wallTop = glycol_water_viscosity(0.,TsPCMTop)
      Cp_wallTop = glycol_water_cp(0.,TsPCMTop)
      Pr_wallTop = Cp_wallTop * mu_wallTop / k_wallTop 

C-----Calculation next to the bottom cylinder surface
      TsPCMBot = Tbot + Tmi
      k_wallBot = glycol_water_cond(0.,TsPCMBot)
      mu_wallBot = glycol_water_viscosity(0.,TsPCMBot)
      Cp_wallBot = glycol_water_cp(0.,TsPCMBot)
      Pr_wallBot = Cp_wallBot * mu_wallBot / k_wallBot 




C-----Characteristic lenght for the top/bottom cylinder surface 
C     =(dPCMcyl**2/4*pi)/(dPCMcyl*pi)
      geomRatio = dPCMcyl/4



C-----Natural convection on the lateral cylinder surface
      GrLat = ABS(gBeta_nLat*(TWATmed-TPCMmedLat)*hPCMcyl**3)
      RaLat = GrLat * PrLat
      Nu_nLat = (0.825+(0.387*RaLat**(1./6.))/(1.+(0.492*
     &          Pr**(9./16.)))**(8./27.))**2.     

C-----Natural convection on the top cylinder surface
      GrTop = ABS(gBeta_nTop*(Ttop-TPCMmedTop)*geomRatio**3)
      RaTop = GrTop * PrTop
      NuTop = 0.27*RaTop**(1/4)

C-----Natural convection on the bottom cylinder surface
      GrBot = ABS(gBeta_nBot*(Tbot-TPCMmedBot)*geomRatio**3)
      RaBot = GrBot * PrBot
      NuBot = 0.27*RaBot**(1/4)




C-----Forced convection
      Mdot_med = 0.
      DO i=nPCMbot,nPCMtop
        Mdot_med=Mdot_med + Mdot(i)
      ENDDO
      Mdot_med=Mdot_med/nY

      Vel = Mdot_med/rho_waterLat/CrossArea
      Re = ABS(Vel)*dPCMcyl/(muLat/rho_waterLat)
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
      Nu_fLat = C*(Re**m)*(PrLat**n)*((PrLat/Pr_wallLat)**0.25)
    
        
C-----Mixed convection on the lateral cylinder surface
      NuLat = ((Nu_nLat ** 3.) + (Nu_fLat ** 3.)) ** (1./3.)

C-----Convection coefficients
      alphaLat = NuLat*k_waterLat/hPCMcyl
      alphaTop = NuTop*k_waterTop/geomRatio
      alphaBot = NuBot*k_waterBot/geomRatio



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

      SUBROUTINE PentaDiagCoeff(acc,coeffx,coeffy,Tmatrix,
     &                          convLat,convTop,convBot,
     &                          nX,nY,nXnY,n)


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
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL coeffy(mY,mX)   ! = cndcty(?,?)*Ay(?)/dPCMnode(?) 
                           ! coefficient calculated for all north and south faces between cells; 
                           ! diffusive term
      REAL convLat(nY)     ! alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(nX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(nX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
      REAL acc(mY,mX)      ! =rho(?,?)*V(?,?)*CA(?,?)/dt 
                           ! coefficient calculated for all cells; accumulation term 




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
      T_SCn(k)        = acc(i,j)+
     &                  convLat(i)+                   !west,self-coupling
     &                  coeffx(i,j)+                  !east,self-coupling
     &                  convBot(j)+                   !south,self-coupling
     &                  coeffy(i,j)                   !north,self-coupling
      T_EASTn(k)      = -coeffx(i,j)                  !east,cross-coupling
      T_NORTHn(k)     = -coeffy(i,j)                  !north,cross-coupling
C     GENERAL BOTTOM
      DO j=2,nX-1
          k=(j-1)*nY+i
          T_SCn(k)    = acc(i,j)+
     &                  coeffx(i,j-1)+                !west,self-coupling
     &                  coeffx(i,j)+                  !east,self-coupling
     &                  convBot(j)+                   !south,self-coupling
     &                  coeffy(i,j)                   !north,self-coupling
          T_EASTn(k)  = -coeffx(i,j)
          T_WESTn(k)  = -coeffx(i,j-1)
          T_NORTHn(k) = -coeffy(i,j)
      ENDDO
C     BOTTOM INNERMOST
      j=nX
      k=(j-1)*nY+i
      T_SCn(k)        = acc(i,j)+
     &                  coeffx(i,j-1)+                !west,self-coupling
     &                  convBot(j)+                   !south,self-coupling
     &                  coeffy(i,j)                   !north,self-coupling
      T_WESTn(k)      = -coeffx(i,j-1)
      T_NORTHn(k)     = -coeffy(i,j)
C-----TOP:
      i=nY
C     TOP OUTERMOST
      j=1
      k=(j-1)*nY+i
      T_SCn(k)        = acc(i,j)+
     &                  convLat(i)+                   !west,self-coupling
     &                  coeffx(i,j)+                  !east,self-coupling
     &                  coeffy(i-1,j)+                !south,self-coupling
     &                  convTop(j)                    !north,self-coupling
      T_EASTn(k)      = -coeffx(i,j)
      T_SOUTHn(k)     = -coeffy(i-1,j)
C     GENERAL TOP
      DO j=2,nX-1
          k=(j-1)*nY+i
          T_SCn(k)    = acc(i,j)+
     &                  coeffx(i,j-1)+                !west,self-coupling
     &                  coeffx(i,j)+                  !east,self-coupling
     &                  coeffy(i-1,j)+                !south,self-coupling
     &                  convTop(j)                    !north,self-coupling
          T_EASTn(k)  = -coeffx(i,j)
          T_WESTn(k)  = -coeffx(i,j-1)
          T_SOUTHn(k) = -coeffy(i-1,j)        
      ENDDO      
C     TOP INNERMOST
      j=nX
      k=(j-1)*nY+i
      T_SCn(k)        = acc(i,j)+
     &                  coeffx(i,j-1)+                !west,self-coupling
     &                  coeffy(i-1,j)+                !south,self-coupling
     &                  convTop(j)                    !north,self-coupling
      T_WESTn(k)      = -coeffx(i,j-1)
      T_SOUTHn(k)     = -coeffy(i-1,j)
      IF (nY.GT.2)THEN
C-------OUTERMOST:
	j=1
	DO i=2,nY-1
	    k=(j-1)*nY+i
	    T_SCn(k)    = acc(i,j)+
     &                    convLat(i)+                 !west,self-coupling
     &                    coeffx(i,j)+                !east,self-coupling
     &                    coeffy(i-1,j)+              !south,self-coupling
     &                    coeffy(i,j)                 !north,self-coupling
	    T_EASTn(k)  = -coeffx(i,j)
	    T_NORTHn(k) = -coeffy(i,j)
	    T_SOUTHn(k) = -coeffy(i-1,j)
	ENDDO
C-------INNERMOST:
	j=nX
	DO i=2,nY-1
	    k=(j-1)*nY+i
	    T_SCn(k)    = acc(i,j)+
     &                    coeffx(i,j-1)+              !west,self-coupling
     &                    coeffy(i-1,j)+              !south,self-coupling
     &                    coeffy(i,j)                 !north,self-coupling
	    T_WESTn(k)  = -coeffx(i,j-1)
	    T_NORTHn(k) = -coeffy(i,j)
	    T_SOUTHn(k) = -coeffy(i-1,j)
	ENDDO
C-------GENERAL CELL
	DO j=2,nX-1   
	  DO i=2,nY-1
	    k=(j-1)*nY+i
	    T_SCn(k)    = acc(i,j)+
     &                    coeffx(i,j-1)+              !west,self-coupling
     &                    coeffx(i,j)+                !east,self-coupling
     &                    coeffy(i-1,j)+              !south,self-coupling
     &                    coeffy(i,j)                !north,self-coupling
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
     &                       Tmatrix,convLat,convTop,convBot,
     &                       nX,nY,nXnY,n,T_SCn,T_EASTn,T_WESTn)


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
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL convLat(nY)     ! = alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(nX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(nX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
      REAL acc(mY,mX)      ! rho(?,?)*V(?,?)*CA(?,?)/dt 
                           ! coefficient calculated for all cells; accumulation term 





C-----write the sum of the accumulation (refered to the node) 
C     and diffusion (refered to the east and west faces) 
C     contributions into an arrey that'll be the principal diagonal. 
C-----write the diffusion (refered to the east and west faces) 
C     contributions into arreys that'll be the other 2 diagonals. 

      i=1
C-----OUTERMOST:
      j=1
      T_SCn(j)        = acc(i,j)+
     &                  convLat(i)+                   !west,self-coupling
     &                  coeffx(i,j)+                  !east,self-coupling
     &                  convBot(j)+                   !south,self-coupling
     &                  convTop(j)                    !north,self-coupling
      T_EASTn(j)      = -coeffx(i,j)
C-----INNERMOST:
      j=nX
      T_SCn(j)        = acc(i,j)+
     &                  coeffx(i,j-1)+                !west,self-coupling
     &                  convBot(j)+                   !south,self-coupling
     &                  convTop(j)                    !north,self-coupling
      T_WESTn(j)      = -coeffx(i,j-1)
C-----GENERAL CELL:
      DO j=2,nX-1   
          T_SCn(j)    = acc(i,j)+
     &                  coeffx(i,j-1)+                !west,self-coupling
     &                  coeffx(i,j)+                  !east,self-coupling
     &                  convBot(j)+                   !south,self-coupling
     &                  convTop(j)                    !north,self-coupling
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






C============================== PCM_RHS ===============================
C     Created by: Roberta Padovan, Marco Manzan
C     Created on: Dicember 2011  
C     Copyright:  Università di Trieste
C----------------------------------------------------------------------
C     ABSTRACT:
C     Calculates the right hand side for the nth iteration for the  
C     implicit method - cases 1D/2D.
C
C======================================================================



      SUBROUTINE PCM_RHS(Tbound,Ttop,Tbot,TMPn,Hn,H,
     &                   convLat,convTop,convBot,
     &                   nX,nY,nXnY,acc,rho_V_dt,RHS)

      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL coeffy(mY,mX)   ! = cndcty(?,?)*Ay(?)/dPCMnode(?) 
                           ! coefficient calculated for all north and south faces between cells; 
                           ! diffusive term
      REAL convLat(nY)     ! alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(nX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(nX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
      REAL rho_V_dt(mY,mX) ! = rho(?,?)*V(?,?)/dt 
                           ! coefficient calculated for all cells
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL Tbot            ! DHW temp at the layer what the PCM cylinder 
                           ! bottom surface is in contact with
      REAL Ttop            ! DHW temp at the layer what the PCM cylinder 
                           ! top surface is in contact with
      REAL TMPn(nXnY)      ! sub-iteration temperature 
                           ! unknown quantity arrey that shall become Ax=B system solution
      REAL Hn(nXnY)        ! sub-iteration entalpy 
      REAL H(nY,nX)        ! cylinder cells enthalpy
      REAL acc(mY,mX)      ! = rho(?,?)*V(?,?)*CA(?,?)/dt 
                           ! coefficient calculated for all cells; accumulation term 
      REAL RHS(nXnY)       ! Right Hand Side to solve the equations system (Ax=B)



C-----BOTTOM:
      i=1
C     BOTTOM OUTERMOST
      j=1
      k=(j-1)*nY+i
      RHS(k) = acc(i,j)*TMPn(k) + rho_V_dt(i,j)*(H(i,j)-Hn(k))+
     &        convLat(i)*Tbound(i)+                                   !west
     &        convBot(j)*Tbot                                         !south
C     GENERAL BOTTOM and BOTTOM INNERMOST
      DO j=2,nX
        k=(j-1)*nY+i
        RHS(k) = acc(i,j)*TMPn(k) + rho_V_dt(i,j)*(H(i,j)-Hn(k))+
     &          convBot(j)*(Tbot-TMPn(k))                             !south
      ENDDO
C-----TOP:
      i=nY
C     TOP OUTERMOST
      j=1
      k=(j-1)*nY+i
      RHS(k) = acc(i,j)*TMPn(k) + rho_V_dt(i,j)*(H(i,j)-Hn(k))+
     &        convLat(i)*Tbound(i)+                                   !west
     &        convTop(j)*Ttop                                         !north
C     GENERAL TOP and TOP INNERMOST
      DO j=2,nX
        k=(j-1)*nY+i
        RHS(k) = acc(i,j)*TMPn(k) + rho_V_dt(i,j)*(H(i,j)-Hn(k))+
     &           convTop(j)*Ttop                                      !north
      ENDDO
      IF (nY.GT.2)THEN
C-------OUTERMOST
        j=1
        DO i=2,nY-1
          k=(j-1)*nY+i
          RHS(k) = acc(i,j)*TMPn(k) + rho_V_dt(i,j)*(H(i,j)-Hn(k))+
     &             convLat(i)*Tbound(i)                               !west
        ENDDO
C-------INNERMOST and GENERAL CELL
        DO j=2,nX
          DO i=2,nY-1   
            k=(j-1)*nY+I 
            RHS(k) = acc(i,j)*TMPn(k) + rho_V_dt(i,j)*(H(i,j)-Hn(k))
          ENDDO
        ENDDO
      ENDIF


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
     &                              Tbound,Ttop,Tbot,TMPn,
     &                              Hn,H,
     &                              convLat,convTop,convBot,
     &                              nX,nY,nXnY,rho_V_dt)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL coeffy(mY,mX)   ! = cndcty(?,?)*Ay(?)/dPCMnode(?) 
                           ! coefficient calculated for all north and south faces between cells; 
                           ! diffusive term
      REAL convLat(nY)     ! alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(nX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(nX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
      REAL rho_V_dt(mY,mX) ! = rho(?,?)*V(?,?)/dt 
                           ! coefficient calculated for all cells
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL Tbot            ! DHW temp at the layer what the PCM cylinder 
                           ! bottom surface is in contact with
      REAL Ttop            ! DHW temp at the layer what the PCM cylinder 
                           ! top surface is in contact with
      REAL TMPn(nXnY)      ! sub-iteration temperature 
                           ! unknown quantity arrey that shall become Ax=B system solution
      REAL Hn(nXnY)        ! sub-iteration entalpy 
      REAL H(nY,nX)        ! cylinder cells enthalpy




C-----BOTTOM:
      i=1
C     BOTTOM OUTERMOST
      j=1
      k=(j-1)*nY+i
      Hn(k) = (convLat(i)*(Tbound(i)-TMPn(k))+                  !west 
     &        coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+                 !east
     &        convBot(j)*(Tbot-TMPn(k))+                        !south
     &        coeffy(i,j)*(TMPn(k+1)-TMPn(k))+                  !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
C     GENERAL BOTTOM
      DO j=2,nX-1
        k=(j-1)*nY+i
        Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+            !west 
     &          coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+               !east
     &          convBot(j)*(Tbot-TMPn(k))+                      !south
     &          coeffy(i,j)*(TMPn(k+1)-TMPn(k))+                !north
     &          rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
      ENDDO
C     BOTTOM INNERMOST
      j=nX
      k=(j-1)*nY+i
      Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+              !west 
     &        convBot(j)*(Tbot-TMPn(k))+                        !south
     &        coeffy(i,j)*(TMPn(k+1)-TMPn(k))+                  !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
C-----TOP:
      i=nY
C     TOP OUTERMOST
      j=1
      k=(j-1)*nY+i
      Hn(k) = (convLat(i)*(Tbound(i)-TMPn(k))+                  !west 
     &        coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+                 !east
     &        coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+                !south
     &        convTop(j)*(Ttop-TMPn(k))+                        !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
C     GENERAL TOP
      DO j=2,nX-1
        k=(j-1)*nY+i
        Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+            !west 
     &          coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+               !east
     &          coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+              !south
     &          convTop(j)*(Ttop-TMPn(k))+                      !north
     &          rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
      ENDDO
C     TOP INNERMOST
      j=nX
      k=(j-1)*nY+i
      Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+              !west 
     &        coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+                !south
     &        convTop(j)*(Ttop-TMPn(k))+                        !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
      IF (nY.GT.2)THEN
C-------OUTERMOST
        j=1
        DO i=2,nY-1
          k=(j-1)*nY+i
          Hn(k) = (convLat(i)*(Tbound(i)-TMPn(k))+              !west 
     &            coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+             !east
     &            coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+            !south
     &            coeffy(i,j)*(TMPn(k+1)-TMPn(k))+              !north
     &            rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
        ENDDO
C-------INNERMOST
        j=nX
        DO i=2,nY-1
          k=(j-1)*nY+i
          Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+          !west 
     &            coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+            !south
     &            coeffy(i,j)*(TMPn(k+1)-TMPn(k))+              !north
     &            rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
        ENDDO
C-------GENERAL CELL
        DO j=2,nX-1
          DO i=2,nY-1   
            k=(j-1)*nY+I 
            Hn(k) = (coeffx(i,j-1)*(TMPn(k-nY)-TMPn(k))+        !west 
     &              coeffx(i,j)*(TMPn(k+nY)-TMPn(k))+           !east
     &              coeffy(i-1,j)*(TMPn(k-1)-TMPn(k))+          !south
     &              coeffy(i,j)*(TMPn(k+1)-TMPn(k))+            !north
     &              rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
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
     &                              Tbound,Ttop,Tbot,TMPn,
     &                              Hn,H,
     &                              convLat,convTop,convBot,
     &                              nX,nY,nXnY,rho_V_dt)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL convLat(nY)     ! = alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(nX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(nX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
      REAL rho_V_dt(mY,mX) ! = rho(?,?)*V(?,?)/dt 
                           ! coefficient calculated for all cells
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL Tbot            ! DHW temp at the layer what the PCM cylinder 
                           ! bottom surface is in contact with
      REAL Ttop            ! DHW temp at the layer what the PCM cylinder 
                           ! top surface is in contact with
      REAL TMPn(nXnY)      ! sub-iteration temperature 
                           ! unknown quantity arrey that shall become Ax=B system solution
      REAL Hn(nXnY)        ! sub-iteration entalpy 
      REAL H(nY,nX)        ! cylinder cells enthalpy





      i=1
C-----OUTERMOST:
      j=1
      Hn(j) = (convLat(i)*(Tbound(i)-TMPn(j))+        !west 
     &        coeffx(i,j)*(TMPn(j+nY)-TMPn(j))+       !east
     &        convBot(j)*(Tbot-TMPn(j))+              !south
     &        convTop(j)*(Ttop-TMPn(j))+              !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
C-----INNERMOST:
      j=nX
      Hn(j) = (coeffx(i,j-1)*(TMPn(j-nY)-TMPn(j))+    !west 
     &        convBot(j)*(Tbot-TMPn(j))+              !south
     &        convTop(j)*(Ttop-TMPn(j))+              !north
     &        rho_V_dt(i,j)*H(i,j))/(rho_V_dt(i,j))
C-----GENERAL CELL
      DO j=2,nX-1
      Hn(j) = (coeffx(i,j-1)*(TMPn(j-nY)-TMPn(j))+    !west 
     &        coeffx(i,j)*(TMPn(j+nY)-TMPn(j))+       !east
     &        convBot(j)*(Tbot-TMPn(j))+              !south
     &        convTop(j)*(Ttop-TMPn(j))+              !north
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
     &                          Tbound,Ttop,Tbot,TMPn,Hn,H,
     &                          convLat,convTop,convBot,
     &                          nX,nY,nXnY,
     &                          rho_V_dt,R)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL coeffy(mY,mX)   ! = cndcty(?,?)*Ay(?)/dPCMnode(?) 
                           ! coefficient calculated for all north and south faces between cells; 
                           ! diffusive term
      REAL convLat(nY)     ! = alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(nX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(nX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
      REAL rho_V_dt(mY,mX) ! rho*V/dt coefficient, calculated for all cells
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL Tbot            ! DHW temp at the layer what the PCM cylinder 
                           ! bottom surface is in contact with
      REAL Ttop            ! DHW temp at the layer what the PCM cylinder 
                           ! top surface is in contact with
      REAL TMPn(nXnY)      ! sub-iteration temperature 
                           ! unknown quantity arrey that shall become Ax=B system solution
      REAL Hn(nXnY)        ! sub-iteration entalpy 
      REAL H(nY,nX)        ! cylinder cells enthalpy
      REAL R(mXmY)         ! residual vector



C-----BOTTOM: 
      i=1
C     BOTTOM OUTERMOST
      j=1
      k=(j-1)*nY+i
      R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &       convLat(i)*(TMPn(k)-Tbound(i))+                    !west 
     &       coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+                  !east
     &       convBot(j)*(TMPn(k)-Tbot)+                         !south
     &       coeffy(i,j)*(TMPn(k)-TMPn(k+1))                    !north
C     GENERAL BOTTOM
      DO j=2,nX-1
        k=(j-1)*nY+i
        R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &         coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+              !west 
     &         coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+                !east
     &         convBot(j)*(TMPn(k)-Tbot)+                       !south
     &         coeffy(i,j)*(TMPn(k)-TMPn(k+1))                  !north
      ENDDO
C     BOTTOM INNERMOST
      j=nX
      k=(j-1)*nY+i
      R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &       coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+                !west 
     &       convBot(j)*(TMPn(k)-Tbot)+                         !south
     &       coeffy(i,j)*(TMPn(k)-TMPn(k+1))                    !north
C-----TOP:
      i=nY
C     TOP OUTERMOST
      j=1
      k=(j-1)*nY+i
      R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &       convLat(i)*(TMPn(k)-Tbound(i))+                    !west 
     &       coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+                  !east
     &       coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+                 !south
     &       convTop(j)*(TMPn(k)-Ttop)                          !north                                           
C     GENERAL TOP
      DO j=2,nX-1
        k=(j-1)*nY+i
        R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &         coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+              !west 
     &         coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+                !east
     &         coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+               !south
     &         convTop(j)*(TMPn(k)-Ttop)                        !north                                                
      ENDDO
C     TOP INNERMOST
      j=nX
      k=(j-1)*nY+i
      R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &       coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+                !west 
     &       coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+                 !south
     &       convTop(j)*(TMPn(k)-Ttop)                          !north
      IF (nY.GT.2)THEN
C-------OUTERMOST:
        j=1
        DO i=2,nY-1
          k=(j-1)*nY+i
          R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &           convLat(i)*(TMPn(k)-Tbound(i))+                !west 
     &           coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+              !east
     &           coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+             !south
     &           coeffy(i,j)*(TMPn(k)-TMPn(k+1))                !north
        ENDDO
C-------INNERMOST:
        j=nX
        DO i=2,nY-1
          k=(j-1)*nY+i
          R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &           coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+            !west 
     &           coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+             !south
     &           coeffy(i,j)*(TMPn(k)-TMPn(k+1))                !north
        ENDDO
C-------GENERAL CELL
        DO j=2,nX-1
          DO i=2,nY-1   
            k=(j-1)*nY+I 
            R(k) = rho_V_dt(i,j)*(Hn(k)-H(i,j))+
     &             coeffx(i,j-1)*(TMPn(k)-TMPn(k-nY))+          !west 
     &             coeffx(i,j)*(TMPn(k)-TMPn(k+nY))+            !east
     &             coeffy(i-1,j)*(TMPn(k)-TMPn(k-1))+           !south
     &             coeffy(i,j)*(TMPn(k)-TMPn(k+1))              !north
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
     &                     Tbound,Ttop,Tbot,TMPn,Hn,H,
     &                     convLat,convTop,convBot,
     &                     nX,nY,nXnY,
     &                     rho_V_dt,R)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL convLat(nY)     ! = alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(nX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(nX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
      REAL rho_V_dt(mY,mX) ! = rho(?,?)*V(?,?)/dt 
                           ! coefficient calculated for all cells
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL Tbot            ! DHW temp at the layer what the PCM cylinder 
                           ! bottom surface is in contact with
      REAL Ttop            ! DHW temp at the layer what the PCM cylinder 
                           ! top surface is in contact with
      REAL TMPn(nXnY)      ! sub-iteration temperature 
                           ! unknown quantity arrey that shall become Ax=B system solution
      REAL Hn(nXnY)        ! sub-iteration entalpy 
      REAL H(nY,nX)        ! cylinder cells enthalpy
      REAL R(mXmY)         ! residual vector


      i=1
C-----OUTERMOST:
      j=1
      R(j) = rho_V_dt(i,j)*(Hn(j)-H(i,j))+
     &       convLat(i)*(TMPn(j)-Tbound(i))+                    !west
     &       coeffx(i,j)*(TMPn(j)-TMPn(j+nY))+                  !east
     &       convBot(j)*(TMPn(j)-Tbot)+                         !south
     &       convTop(j)*(TMPn(j)-Ttop)                          !north
C-----INNERMOST:
      j=nX
      R(j) = rho_V_dt(i,j)*(Hn(j)-H(i,j))+
     &       coeffx(i,j-1)*(TMPn(j)-Tbound(i))+                 !west 
     &       convBot(j)*(TMPn(j)-Tbot)+                         !south
     &       convTop(j)*(TMPn(j)-Ttop)                          !north
C-----GENERAL CELL
      DO j=2,nX-1
        R(j) = rho_V_dt(i,j)*(Hn(j)-H(i,j))+
     &         coeffx(i,j-1)*(TMPn(j)-TMPn(j-nY))+              !west 
     &         coeffx(i,j)*(TMPn(j)-TMPn(j+nY))+                !east
     &         convBot(j)*(TMPn(j)-Tbot)+                       !south
     &         convTop(j)*(TMPn(j)-Ttop)                        !north
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

      SUBROUTINE Enthalpy2D(acc,coeffx,coeffy,
     &                   Tbound,Ttop,Tbot,TMP,H,
     &                   convLat,convTop,convBot,
     &                   nX,nY,nXnY,n)

      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)


      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL coeffy(mY,mX)   ! = cndcty(?,?)*Ay(?)/dPCMnode(?) 
                           ! coefficient calculated for all north and south faces between cells; 
                           ! diffusive term
      REAL convLat(nY)     ! alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(nX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(nX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
      REAL acc(mY,mX)      ! = rho(?,?)*V(?,?)/dt 
                           ! coefficient calculated for all cells, accumulation term 
      REAL Tbound(nY)      ! tank layer temperature in contact with each PCM section
                           ! boundary condition
      REAL Tbot            ! DHW temp at the layer what the PCM cylinder 
                           ! bottom surface is in contact with
      REAL Ttop            ! DHW temp at the layer what the PCM cylinder 
                           ! top surface is in contact with
      REAL TMP(nY,nX)      ! cylinder cells temperature [°C]
      REAL H(nY,nX)        ! cylinder cells enthalpy





C---- For a cylynder with more than one layer in axial direction 
C---- Cylynder top and bottom are adiabatic surfaces                                                                  |

C-----BOTTOM:
      i=1
C     BOTTOM OUTERMOST
      j=1
      H(i,j) = (acc(i,j)*H(i,j)-
     &         (convLat(i)+                  !west,self-coupling
     &         coeffx(i,j)+                  !east,self-coupling
     &         convBot(j)+                   !south,self-coupling
     &         coeffy(i,j))                  !north,self-coupling
     &         *TMP(i,j)+
     &         convLat(i)*Tbound(i)+         !west,cross-coupling
     &         coeffx(i,j)*TMP(i,j+1)+       !east,cross-coupling
     &         convBot(j)*Tbot+              !south,cross-coupling
     &         coeffy(i,j)*TMP(i+1,j))       !north,cross-coupling
     &         /acc(i,j)
C     GENERAL BOTTOM
      DO j=2,nX-1
        H(i,j) = (acc(i,j)*H(i,j)-                    
     &           (coeffx(i,j-1)+             !west,self-coupling
     &           coeffx(i,j)+                !east,self-coupling
     &           convBot(j)+                 !south,self-coupling
     &           coeffy(i,j))                !north,self-coupling
     &           *TMP(i,j)+ 
     &           coeffx(i,j-1)*TMP(i,j-1)+   !west,cross-coupling
     &           coeffx(i,j)*TMP(i,j+1)+     !east,cross-coupling
     &           convBot(j)*Tbot+            !south,cross-coupling
     &           coeffy(i,j)*TMP(i+1,j))     !north,cross-coupling
     &           /acc(i,j)
      ENDDO
C     BOTTOM INNERMOST
      j=nX
      H(i,j) = (acc(i,j)*H(i,j)-
     &         (coeffx(i,j-1)+               !west,self-coupling
     &         convBot(j)+                   !south,self-coupling
     &         coeffy(i,j))                  !north,self-coupling
     &         *TMP(i,j)+ 
     &         coeffx(i,j-1)*TMP(i,j-1)+     !west,cross-coupling
     &         convBot(j)*Tbot+              !south,cross-coupling
     &         coeffy(i,j)*TMP(i+1,j))       !north,cross-coupling
     &         /acc(i,j)
C-----TOP:
      i=nY
C     TOP OUTERMOST
      j=1
      H(i,j) = (acc(i,j)*H(i,j)-
     &         (convLat(i)+                  !west,self-coupling
     &         coeffx(i,j)+                  !east,self-coupling
     &         coeffy(i-1,j)+                !south,self-coupling
     &         convTop(j))                   !north,self-coupling
     &         *TMP(i,j)+
     &         convLat(i)*Tbound(i)+         !west,cross-coupling
     &         coeffx(i,j)*TMP(i,j+1)+       !east,cross-coupling
     &         coeffy(i-1,j)*TMP(i-1,j)+     !south,cross-coupling
     &         convTop(j)*Ttop)              !north,cross-coupling
     &         /acc(i,j)
C     GENERAL TOP
      DO j=2,nX-1
        H(i,j) = (acc(i,j)*H(i,j)-                        
     &           (coeffx(i,j-1)+             !west,self-coupling
     &           coeffx(i,j)+                !east,self-coupling
     &           coeffy(i-1,j)+              !south,self-coupling
     &           convTop(j))                 !north,self-coupling
     &           *TMP(i,j)
     &           +coeffx(i,j-1)*TMP(i,j-1)+  !west,cross-coupling
     &           coeffx(i,j)*TMP(i,j+1)+     !east,cross-coupling
     &           coeffy(i-1,j)*TMP(i-1,j)+   !south,cross-coupling
     &           convTop(j)*Ttop)            !north,cross-coupling
     &           /acc(i,j)
      ENDDO
C     TOP INNERMOST
      j=nX
      H(i,j) = (acc(i,j)*H(i,j)-
     &         (coeffx(i,j-1)+               !west,self-coupling
     &         coeffy(i-1,j)+                !south,self-coupling
     &         convTop(j))                   !north,self-coupling
     &         *TMP(i,j)+ 
     &         coeffx(i,j-1)*TMP(i,j-1)+     !west,cross-coupling
     &         coeffy(i-1,j)*TMP(i-1,j)+     !south,cross-coupling
     &         convTop(j)*Ttop)              !north,cross-coupling
     &         /acc(i,j)
      IF (nY.GT.2)THEN
C-------OUTERMOST:
        j=1
        DO i=2,nY-1
          H(i,j) = (acc(i,j)*H(i,j)-
     &             (convLat(i)+                !west,self-coupling
     &             coeffx(i,j)+                !east,self-coupling
     &             coeffy(i-1,j)+              !south,self-coupling
     &             coeffy(i,j))                !north,self-coupling
     &             *TMP(i,j)+
     &             convLat(i)*Tbound(i)+       !west,cross-coupling
     &             coeffx(i,j)*TMP(i,j+1)+     !east,cross-coupling
     &             coeffy(i-1,j)*TMP(i-1,j)+   !south,cross-coupling
     &             coeffy(i,j)*TMP(i+1,j))     !north,cross-coupling
     &             /acc(i,j)
        ENDDO
C-------INNERMOST:
        j=nX
        DO i=2,nY-1
          H(i,j) = (acc(i,j)*H(i,j)-
     &             (coeffx(i,j-1)+             !west,self-coupling
     &             coeffy(i-1,j)+              !south,self-coupling
     &             coeffy(i,j))                !north,self-coupling
     &             *TMP(i,j)+                                    
     &             coeffx(i,j-1)*TMP(i,j-1)+   !west,cross-coupling
     &             coeffy(i-1,j)*TMP(i-1,j)+   !south,cross-coupling
     &             coeffy(i,j)*TMP(i+1,j))     !north,cross-coupling
     &             /acc(i,j)
        ENDDO
C-------GENERAL CELL
        DO j=2,nX-1 !generic cells in radial direction
          DO i=2,nY-1 !generic cells in axial direction
            H(i,j) = (acc(i,j)*H(i,j)-
     &               (coeffx(i,j-1)+           !west,self-coupling
     &               coeffx(i,j)+              !east,self-coupling
     &               coeffy(i-1,j)+            !south,self-coupling
     &               coeffy(i,j))              !north,self-coupling
     &               *TMP(i,j)+ 
     &               coeffx(i,j-1)*TMP(i,j-1)+ !west,cross-coupling
     &               coeffx(i,j)*TMP(i,j+1)+   !east,cross-coupling
     &               coeffy(i-1,j)*TMP(i-1,j)+ !south,cross-coupling
     &               coeffy(i,j)*TMP(i+1,j))   !north,cross-coupling
     &               /acc(i,j)     
          ENDDO  
        ENDDO
      ENDIF


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

      SUBROUTINE Enthalpy1D(acc,coeffx,
     &                   Tbound,Ttop,Tbot,TMP,H,
     &                   convLat,convTop,convBot,
     &                   nX,nY,nXnY,n)


      INTEGER mX,mY,mXmY
      PARAMETER (mX=100,mY=100,mXmY=1000)


      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER nXnY         ! number of total cells
      REAL coeffx(mY,mX)   ! = cndctx(?,?)*Ax(?,?)/dr  
                           ! coefficient calculated for all east and west faces between cells;
                           ! diffusive term
      REAL convLat(nY)     ! = alphaLat*Axb(?) 
                           ! coefficient for convective thermal exchange at lateral border cells
      REAL convTop(nX)     ! = alphaTop*Ay(?) 
                           ! coefficient for convective thermal exchange at top border cells
      REAL convBot(nX)     ! = alphaBot*Ay(?) 
                           ! coefficient for convective thermal exchange at bottom border cells
      REAL acc(mY,mX)      ! = rho(?,?)*V(?,?)/dt 
                           ! coefficient calculated for all cells; accumulation term 
      REAL Tbound(nY)      ! DHW layer temperature in contact with each PCM section
                           ! boundary condition
      REAL Tbot            ! DHW temp at the layer what the PCM cylinder 
                           ! bottom surface is in contact with
      REAL Ttop            ! DHW temp at the layer what the PCM cylinder 
                           ! top surface is in contact with
      REAL TMP(nY,nX)      ! cylinder cells temperature [°C]
      REAL H(nY,nX)        ! cylinder cells enthalpy


C---- For a cylynder with only one layer in axial direction, no conductive 
C     contribution top-bottom. Cylynder top and bottom are adiabatic surfaces
      i=1
C-----OUTERMOST:
      j=1
      H(1,j) = (acc(i,j)*H(i,j)- 
     &         (convLat(i)+                !west,self-coupling
     &         coeffx(i,j)+                !east,self-coupling
     &         convBot(j)+                 !south,self-coupling
     &         convTop(j))                 !north,self-coupling
     &         *TMP(i,j)+
     &         convLat(i)*Tbound(i) +      !west,cross-coupling
     &         coeffx(i,j)*TMP(i,j+1)+     !east,cross-coupling
     &         convBot(j)*Tbot +           !south,cross-coupling
     &         convTop(j)*Ttop)            !north,cross-coupling
     &         /acc(i,j)
C-----INNERMOST:
      j=nX
      H(1,j) = (acc(i,j)*H(i,j)-
     &         (coeffx(i,j-1)+             !west,self-coupling
     &         convBot(j)+                 !south,self-coupling
     &         convTop(j))                 !north,self-coupling
     &         *TMP(i,j)+ 
     &         coeffx(i,j-1)*TMP(i,j-1)+   !west,cross-coupling
     &         convBot(j)*Tbot +           !south,cross-coupling
     &         convTop(j)*Ttop)            !north,cross-coupling  
     &         /acc(i,j)
C-----GENERAL CELL:
      DO j=2,nX-1 
        H(1,j) = (acc(i,j)*H(i,j)-
     &           (coeffx(i,j-1)+           !west,self-coupling
     &           coeffx(i,j)+              !east,self-coupling
     &           convBot(j)+               !south,self-coupling
     &           convTop(j))               !north,self-coupling
     &           *TMP(i,j)+ 
     &           coeffx(i,j-1)*TMP(i,j-1)+ !west,cross-coupling
     &           coeffx(i,j)*TMP(i,j+1)+   !east,cross-coupling
     &           convBot(j)*Tbot+          !south,cross-coupling
     &           convTop(j)*Ttop)          !north,cross-coupling
     &           /acc(i,j)
        ENDDO


      RETURN
      END
