C*****************************************************************
C Special Materials PCM properties calculation
C
C Created by: Achim Geissler
C Created on: July 11, 2008
C Copyright: GaCH
C ________
C Contains 
C Function LatentHeat()
C Subroutine PCMSTORE()
C ________
C ABSTRACT:
C This code can be used to enable modelling of PCM materials which show 
C significant sub cooling properties. I.e., such materials which - once melted - 
C must be subcooled by a few degrees K below the "conversion temperature" at
C which they were completely liquified.
C
C The routine is designed to be called in a "governing" PCM routine, which
C could (probably) be coded along the lines of SPMCPM53() for use in ESP-r.
C
C*****************************************************************
      function LatentHeat(k,T1,T2)
      implicit none
C Calculate integral of stored / released latent heat between T1 and T2
C k   ... "normal" (k=1) or "subcooled" (k=0) range
C T1 ... start temperature in °C
C T2 ... end temperature in °C
C
C Return value:
C LH ... stored / released latent heat in J/kg. Thereby, the energy balance 
C is formulated viewed from the surrounding space:
C         > 0 when T1 > T2, i.e. heat released
C         < 0 when T1 < T2, i.e. heat stored
C
C Equation and parameters taken from PhD. thesis by S. Hoffmann, Weimar 2006
C
      real T1,T2
      integer k

C.... Common blocks
      common /PCMPARM/TSOLI,TMELT,sub,Lges
      real TSOLI,TMELT,sub,Lges ! Lges is total latent heat for 
                                             ! specific PCM used in J/kg

      common/CH/a1,a2,b1,b2
      real a1,a2,b1,b2 ! Equation parameters for non-subcooling (a1, b1)
                              ! and subcooling (a2, b2) states for specific PCM

      real aa,bb

      if (k.eq.0) THEN ! function defaults to "normal" mode
         aa=a2
         bb=b2
      else
         aa=a1
         bb=b1
      endif

      LatentHeat = EXP(aa)/bb*(EXP(bb*T1)-EXP(bb*T2))
      
C     More than Lges is not possible:
      LatentHeat = min(Lges,LatentHeat)

      End


C**********************************************************************************************
      subroutine PCMSTORE(TnS,TnS1,k,storedLH,AppCon,AppSHC)
      implicit none
C This routine keeps book of latent heat stored / released by a PCM material
C The PCM material is described by the parameters in Common /PCMPARM/ and must
C be set at calling time.
C
C The routine is designed to be called once each simulation time step. It must be called for
C each PCM node, if a PCM layer is split into several nodes.
C 
C**********************************************************************************************

C.... Parameter
      real TnS        ! present temperature TPC(MCOM,MS,MN) ?
      real TnS1      ! future temperature TFC(MCOM,MS,MN) ?
      integer k       ! "sub-cooling switch", value of preceding time step must be passed.
                           ! k must be initialised befor first call (typically k_init=1)
      real storedLH  ! stored latent heat ("h" in thesis of S.H.), value of preceeding
                           ! time step must be passed when called. Typically, this parameter
                           ! is initialised with "0" befor first call.
      real AppSHC   ! apparent specific heat capacity - used for ESP-r coupling
      real AppCon   ! apparent thermal conductivity - used for ESP-r coupling

C.... Common blocks
      common /PCMPARM/TSOLI,TMELT,sub,Lges
      real TSOLI,TMELT,sub,Lges

C.... Local variables
      real TSOLI2,TMELT2,storeThisTS
      
      TSOLI2=TSOLI-sub
      TMELT2=TMELT-sub

C**********************************************************************************************
C      Calculate the heat storage according to the temperature change between present (TnS) and 
C      future (TnS1) temperatures.
C      Also calculate apparent heat capacity and apparent thermal conductivity based on percentage
C      of liquidification of PCM material.
C
C      Following cases are checked / differentiated
C      Heating: cases 1, 2, 3, 4, 5, 6 and 7
C      Cooling: cases 1, 2, 3, 5, 6, 7, and 8
C
C                ^
C                ¦ C [J/(kg K)]
C                ¦                       6         5              (5)
C                ¦                        /------<-->------/-----<-->-----
C                ¦                       /                / 4
C                ¦                      /                /
C                ¦                     /                /
C                ¦                  7 /                / 3
C                ¦                   /                /
C                ¦                  /                /
C                ¦                 /                /
C                ¦      (1)     8 /      1       2 /
C          C_liq ¦-----<-->------/----------------/        ----------------> T [°C]
C                             TMELT2  TSOLI2    TMELT   TSOLI
C
C  Possibly, checking temperatures against each other should use eclose(). 
C  However, then an "alternative action" must be defined if eclose() fails ... 
C  what would this "alternative action" be? "Do nothing"?
C
      if (TnS.lt.TnS1) THEN
C *** Temperature increase in PCM  ***
         IF (k.eq.1) Then
         ! PCM behaves according to "non-subcooling" properties
            if (TnS1.le.TMELT)
               ! PCM is solid (case 1), StoredLH = 0
                  AppCon=CONSOL
                  AppSHC=SHCSOL

            elseif (TnS.le.TMELT.and.TnS1.gt.TMELT)
            ! PCM begins to melt (case 2)
               storedLH = -1.0E0*enthalp(1,TMELT,TnS1) ! "storedLH" must be >0 !
               AppCon=(1-storedLH/Lges)*CONSOL+
     &                        storedLH/Lges*CONLIQ
               AppSHC=SHCLIQ+storedLH/(TnS1-TMELT) ! to be verified

            elseif (TnS.gt.TMELT.and.TnS1.le.TSOLI)
            ! PCM is partially liquid, latent heat is stored in the "normal" 
            ! range (k=1, case 3).
               storeThisTS = enthalp(1,TnS,TnS1)
               storedLH = storedLH - storeThisTS
               AppCon=(1-storedLH/Lges)*CONSOL+
     &                        storedLH/Lges*CONLIQ
               AppSHC=SHCLIQ+storeThisTS/(TnS1-TnS) ! to be verified

            elseif (TnS.gt.TMELT.and.TnS1.gt.TSOLI)
            ! Rest of solid PCM is completely melted in this timestep and also heated 
            ! as liquid (case 4)
               storedLH = Lges ! crude
               AppCon=CONLIQ
               AppSHC=SHCLIQ ! crude, actually, here we should take "rest" of latent heat into acc.
C         *** set new k ***
               k=0
C         ****************
            endif ! <- temperature bracketing for k = 1

         else ! (k=0)
         ! PCM behaves according to subcooling properties
            if (TnS.gt.TSOLI2)
            ! PCM is completely liquified and heated further (case 5)
               storedLH=Lges
               AppCon=CONLIQ
               AppSHC=SHCLIQ

            elseif (TnS1.gt.TSOLI2)
            ! PCM is subcooled partial liquid and is completely (re-)melted 
            ! in this timestep (case 6)
               storedLH = Lges ! crude
               AppCon=CONLIQ
               AppSHC=SHCLIQ ! crude, actually, again we should take "rest" of latent heat into acc.

            ELSEif (TnS1.le.TSOLI2) then
            ! PCM is subcooled partial liquid and not completely melted in 
            ! this timestep (k=0, case 7)
               storeThisTS = enthalp(0,TnS,TnS1)
               storedLH = storedLH - storeThisTS
               AppCon=(1-storedLH/Lges)*CONSOL+
     &                        storedLH/Lges*CONLIQ
               AppSHC=SHCLIQ+storeThisTS/(TnS1-TnS) ! to be verified
            endif ! <- temperature bracketing for k=0

         endif ! <- k = ?

      elseif (TnS.gt.TnS1) Then
C *** Temperature decrease in PCM ***
         IF (k.eq.1) Then
         ! PCM behaves according to "non-subcooling" properties
            if (TnS.le.TMELT)
               ! PCM is solid (case 1), StoredLH = 0
               AppCon=CONSOL
               AppSHC=SHCSOL
            elseif (TnS.gt.TMELT.and.TnS1.le.TMELT)
            ! PCM non-subcooled solidification is completed (case 2)               
               AppCon=(1-storedLH/Lges)*CONSOL+
     &                        storedLH/Lges*CONLIQ
               AppSHC=SHCLIQ+storedLH/(TnS-TMELT)
               storedLH = 0 ! now all PCM is solid

            elseif (TnS.le.TSOLI.and.TnS1.ge.TMELT)
            ! PCM is partially liquid, and is now cooled (non-subcooling, case 3)
               storedThisTS = enthalp(1,TnS,TnS1)
               storedLH = storedLH - storedThisTS
               AppCon=(1-storedLH/Lges)*CONSOL+
     &                        storedLH/Lges*CONLIQ
               AppSHC=SHCLIQ+storedThisTS/(TnS-TnS1) ! to be verified
            endif ! <- temperature bracketing for k=1

         else ! (k=0)
         ! PCM behaves according to subcooling properties
            IF(TnS1.ge.TSOLI2) then
            ! PCM is completely melted and does not reach sub-cooled 
            ! solidification temp. in this timestep (case 5)
               storedLH = Lges
               AppCon=CONLIQ
               AppSHC=SHCLIQ

             ELSEif (TnS.gt.TSOLI2.and.TnS1.lt.TSOLI2) then
            ! PCM begins to solidify in sub-cooled mode 
            ! this timestep (k=0, case 6)
               storeThisTS = enthalp(0,TSOLI2,TnS1)
               storedLH = storeThisTS
               AppCon=CONSOL
               AppSHC=SHCSOL ! crude

            ELSEif (TnS.le.TSOLI2.and.TnS1.gt.TMELT2) then
            ! PCM is subcooled partial liquid and not completely solidified in 
            ! this timestep (k=0, case 7)
               storeThisTS = enthalp(0,TnS,TnS1)
               storedLH = storedLH - storeThisTS
               AppCon=(1-storedLH/Lges)*CONSOL+
     &                        storedLH/Lges*CONLIQ
               AppSHC=SHCSOL+storedThisTS/(TnS-TnS1) ! to be verified

             ELSEif (TnS.gt.TMELT2.and.TnS1.le.TMELT2) then
            ! PCM completely solidifies in this timestep (k=0, case 8)
               storeThisTS = enthalp(0,TnS,TMELT2)
               storedLH = 0
               AppCon=CONSOL
               AppSHC=SHCSOL ! crude
C         *** set new k ***
               k=1
C         ****************
            endif ! <- temperature bracketing for k=0
         endif ! <- k = ?
      else
         ! what happens here ???? (TnS = TnS1)
      endif ! <- end calculation of heat storage at this time step

      return
      end ! <- Subroutine PCMSTORE()
