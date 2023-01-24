C chemical_properties.h
C
C This file contains definitions for various chemical properties.
C
C References:
C
C  Van Wylen, Sonntag & Borgnakke, "Fundamentals of Classical
C  Thermodynamics", John Wiley & Sons, 1994.
C

C----------------------------------------------------------------------
C     Named Constants.
C----------------------------------------------------------------------
C.....Number of elements in the periodic table
      integer iElement_count
      parameter ( iElement_count = 118 )

C-----------------------------------------------------------------------
C     Element atomic numbers. Add more as required
C-----------------------------------------------------------------------
      integer iElement_Carbon, iElement_Hydrogen
      integer iElement_Oxygen, iElement_Nitrogen
      parameter (iElement_Hydrogen = 1  )
      parameter (iElement_Carbon   = 6  )
      parameter (iElement_Nitrogen = 7  )
      parameter (iElement_Oxygen   = 8  )

C.....Number of active elements
      integer iActive_El_count
      parameter ( iActive_El_count  = 4 )

C.....Indicies of active elements
      integer iActive_elements(iActive_El_count)
      integer iElc
      data (iActive_elements(iElc), iElc=1,iActive_El_count)
     &                     / iElement_Hydrogen,
     &                       iElement_Carbon,
     &                       iElement_Nitrogen,
     &                       iElement_Oxygen     /

      save iActive_elements
      
C-----------------------------------------------------------------------
C     Elemental Molar masses(kg/kmol). Source: Van Wylen et al. 1994
C-----------------------------------------------------------------------
C.....Molar masses of all ('active') elements
      real fElement_Molar_mass(115)
      data fElement_Molar_mass(iElement_Hydrogen)   /  1.00794  /
      data fElement_Molar_mass(iElement_Carbon)     / 12.011    /
      data fElement_Molar_mass(iElement_Nitrogen)   / 14.0067   /
      data fElement_Molar_mass(iElement_Oxygen)     / 15.994    /

      save fElement_Molar_mass


C-----------------------------------------------------------------------
C     Named constants for compounds
C-----------------------------------------------------------------------      
C.....Number of supported compounds
      integer iCompound_count
      parameter ( iCompound_count = 15 )

C.....H2
      integer iHydrogen
      parameter ( iHydrogen = 1 )
C.....CH4
      integer iMethane
      parameter ( iMethane = 2 )
C.....C2H6
      integer iEthane
      parameter ( iEthane = 3 )
C.....C3H8
      integer iPropane
      parameter ( iPropane = 4 )
C.....C4H10
      integer iButane
      parameter ( iButane = 5 )
C.....C5H12
      integer iPentane
      parameter ( iPentane = 6 )
C.....C6H14
      integer iHexane
      parameter ( iHexane = 7 )
C.....CO
      integer iC_Monoxide
      parameter ( iC_Monoxide = 8 )
C.....CO2
      integer iC_Dioxide
      parameter ( iC_Dioxide = 9 )
C.....O2
      integer iOxygen
      parameter ( iOxygen = 10 )
C.....H2O (v)
      integer iWater_V
      parameter ( iWater_V = 11 )
C.....H2O (l)
      integer iWater_l
      parameter ( iWater_l = 12 )
C.....N2
      integer iNitrogen
      parameter ( iNitrogen = 13 )

C.....Ethanol (C2H5OH)
      integer iEthanol
      parameter ( iEthanol = 14 )

C.....Methanol (CH3OH)
      integer iMethanol
      parameter ( iMethanol = 15 )            


C-----------------------------------------------------------------------
C     Composition of each compound
C-----------------------------------------------------------------------
      integer iCompound_Composition(iCompound_count,iElement_count)
      integer iCPc
      integer iVector_length
      parameter ( iVector_length = iCompound_count * iElement_count )
      
      common/Chem_compounds/iCompound_Composition


C-----------------------------------------------------------------------
C     Compound enthalpies of formation (J / kmol), @ 25oC, 100 kPa
C     Source: Van Wylen et al. 1994
C-----------------------------------------------------------------------
      real fEnthalpy_Formation ( iCompound_count )

      data fEnthalpy_Formation ( iWater_v    )  / -241.826e06 /
      data fEnthalpy_Formation ( iWater_l    )  / -285.830e06 /
      data fEnthalpy_Formation ( iHydrogen   )  /    0.       /
      data fEnthalpy_Formation ( iMethane    )  / - 74.873e06 /
      data fEnthalpy_Formation ( iEthane     )  / - 84.740e06 /
      data fEnthalpy_Formation ( iPropane    )  / -103.900e06 /
      data fEnthalpy_Formation ( iButane     )  / -126.200e06 /
      data fEnthalpy_Formation ( iPentane    )  / -146.500e06 /
      data fEnthalpy_Formation ( iHexane     )  / -167.300e06 /
      data fEnthalpy_Formation ( iC_Monoxide )  / -110.527e06 /
      data fEnthalpy_Formation ( iC_Dioxide  )  / -393.522e06 /
      data fEnthalpy_Formation ( iOxygen     )  /    0.       /
      data fEnthalpy_Formation ( iNitrogen   )  /    0.       /
      data fEnthalpy_Formation ( iMethanol   )  / -201.300e06 /
      data fEnthalpy_Formation ( iEthanol    )  / -235.000e06 /
          
      save fEnthalpy_Formation


C---------------------------------------------------------------------
C     Flag indicating if compound can be oxidized (easily...)
C---------------------------------------------------------------------
      logical bIs_Oxidizable(iCompound_count)

      data bIs_Oxidizable( iWater_v    )  / .false. /
      data bIs_Oxidizable( iWater_l    )  / .false. /
      data bIs_Oxidizable( iHydrogen   )  / .true.  /
      data bIs_Oxidizable( iMethane    )  / .true.  /
      data bIs_Oxidizable( iEthane     )  / .true.  /
      data bIs_Oxidizable( iPropane    )  / .true.  /
      data bIs_Oxidizable( iButane     )  / .true.  /
      data bIs_Oxidizable( iPentane    )  / .true.  /
      data bIs_Oxidizable( iHexane     )  / .true.  /
      data bIs_Oxidizable( iC_Monoxide )  / .true.  /
      data bIs_Oxidizable( iC_Dioxide  )  / .false. /
      data bIs_Oxidizable( iOxygen     )  / .false. /
      data bIs_Oxidizable( iNitrogen   )  / .false. /
      data bIs_Oxidizable( iMethanol   )  / .true.  /
      data bIs_Oxidizable( iEthanol    )  / .true.  /

      save bIs_Oxidizable

C---------------------------------------------------------------------
C     Universal Ideal gas constant (J/kMol K)
C---------------------------------------------------------------------
      real fR_universal
      parameter (fR_universal = 8314.0 )


C---------------------------------------------------------------------
C     Specific heat correlation methods:
C---------------------------------------------------------------------
      integer iCp_Kyle_Corr           ! Kyle 1994 correlations
      parameter (iCp_Kyle_Corr = 1)
      
C.....Associate correlations with compounds (presently, only Kyle's
C.....correlations are supported.
      integer iCp_Corr_method ( iCompound_count )
      save iCp_Corr_method
      data iCp_Corr_method / iCompound_count * iCp_Kyle_Corr /
      
C---------------------------------------------------------------------
C     Kyle Specific heat correlation coefficients.
C
C     Source: Kyle, B. Chemical and Process Thermodynamics, Prentice
C                Hall, 1984
C
C        via Cengel, Y. and Boles, M. Thermodynamics, an engineering
C                approach. McGraw Hill 1989
C
C---------------------------------------------------------------------
      
C.....Correlation coefficients
      real fCp_Kyle_Coeff(iCompound_count, 4)
      save fCp_Kyle_Coeff

      
      data (fCp_Kyle_Coeff ( iNitrogen, iCPc ),iCPc=1,4)
     &        / 28.90, -0.1571e-02, 0.8081e-05, -2.873e-09   /

      data (fCp_Kyle_Coeff ( iOxygen, iCPc ),iCPc=1,4)
     &        / 25.48, 1.520e-02, -0.7155e-05, 1.312e-09  /
     
      data (fCp_Kyle_Coeff ( iHydrogen, iCPc ),iCPc=1,4)
     &        /  29.11, -0.1916e-02, 0.4003e-05, -0.8704e-09 /

      data (fCp_Kyle_Coeff ( iC_Monoxide, iCPc ),iCPc=1,4)
     &        /  28.16, 0.1652e-02, 0.5372e-05, -2.222e-09  /
      
      data (fCp_Kyle_Coeff ( iC_Dioxide, iCPc ),iCPc=1,4)
     &        /  22.26, 5.981e-02, -3.501e-05, 7.469e-09  /

      data (fCp_Kyle_Coeff( iWater_v, iCPc ), iCPc=1,4 )
     &        / 32.24, 0.1923e-02, 1.055e-05, -3.595e-09    /
     
      data (fCp_Kyle_Coeff ( iMethane, iCPc ),iCPc=1,4)
     &        /  19.89, 5.024e-02, 1.269e-05, -11.01e-09  /

      data (fCp_Kyle_Coeff ( iEthane, iCPc ),iCPc=1,4)
     &        /  6.900, 17.27e-02, -6.406e-05, 7.285e-09  /

      data (fCp_Kyle_Coeff ( iPropane, iCPc ),iCPc=1,4)
     &        /  -4.04, 30.48e-02, -15.72e-05, 31.74e-09  /

      data (fCp_Kyle_Coeff ( iButane, iCPc ),iCPc=1,4)
     &        /  3.96, 37.15e-02, -18.34e-05, 35.00e-09  /

      data (fCp_Kyle_Coeff ( iPentane, iCPc ),iCPc=1,4)
     &        /  6.774, 45.43e-02, -22.46e-05, 42.29e-09  /

      data (fCp_Kyle_Coeff ( iHexane, iCPc ),iCPc=1,4)
     &        /  6.938, 55.22e-02, -28.65e-05, 57.69e-09  /

      data (fCp_Kyle_Coeff ( iMethanol, iCPc ),iCPc=1,4)
     &        /  19.0, 9.152e-02, -1.22e-05, -8.039e-09   /

      data (fCp_Kyle_Coeff ( iEthanol, iCPc ),iCPc=1,4)
     &        /  19.9 , 20.96e-02, -10.38e-05, 20.05e-09  /
     

C.....Kyle Correlation ranges
      real fCp_Kyle_Ranges (iCompound_count,2)
      save fCp_Kyle_Ranges
      integer iCp_Range_MAX, iCp_Range_MIN
      parameter (iCp_Range_MAX = 2,
     &           iCp_Range_MIN = 1)

      data fCp_Kyle_Ranges ( iNitrogen, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iNitrogen, iCp_Range_MAX )
     &     / 260.0, 1800.0 /

      data fCp_Kyle_Ranges ( iOxygen, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iOxygen, iCp_Range_MAX )
     &     / 260.0, 1800.0 /

      data fCp_Kyle_Ranges ( iHydrogen, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iHydrogen, iCp_Range_MAX )
     &     / 260.0, 1800.0 /
     
      data fCp_Kyle_Ranges ( iC_Monoxide, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iC_Monoxide, iCp_Range_MAX )
     &     / 260.0, 1800.0 /

      data fCp_Kyle_Ranges ( iC_Dioxide, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iC_Dioxide, iCp_Range_MAX )
     &     / 260.0, 1800.0 /

      data fCp_Kyle_Ranges ( iMethane, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iMethane, iCp_Range_MAX )
     &     / 260.0, 1500.0 /

      data fCp_Kyle_Ranges ( iEthane, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iEthane, iCp_Range_MAX )
     &     / 260.0, 1500.0 /
     
      data fCp_Kyle_Ranges ( iPropane, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iPropane, iCp_Range_MAX )
     &     / 260.0, 1500.0 /

      data fCp_Kyle_Ranges ( iButane, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iButane, iCp_Range_MAX )
     &     / 260.0, 1500.0 /
     
      data fCp_Kyle_Ranges ( iPentane, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iPentane, iCp_Range_MAX )
     &     / 260.0, 1500.0 /

      data fCp_Kyle_Ranges ( iHexane, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iHexane, iCp_Range_MAX )
     &     / 260.0, 1500.0 /

      data fCp_Kyle_Ranges ( iMethanol, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iMethanol, iCp_Range_MAX )
     &     / 273.0, 1000.0 /

      data fCp_Kyle_Ranges ( iEthanol, iCp_Range_MIN ),
     &     fCp_Kyle_Ranges ( iEthanol, iCp_Range_MAX )
     &     / 273.0, 1500.0 /
          
C---------------------------------------------------------------------
C     Miscellaneous named constants
C---------------------------------------------------------------------
C.....Heating value
      integer iLHV, iHHV
      parameter (iLHV = 1, iHHV = 2)
     
C---------------------------------------------------------------------
C     Flags for warning / error messages
C---------------------------------------------------------------------
      logical bWarning, bError
      parameter ( bWarning = .false.,
     &            bError   = .true. )
C.....Context buffer
      common / Chem_prop_msg / cContext
      character*256 cContext
C.....Message passed to Chemical_properties_msg
      character*256 cMessage

C---------------------------------------------------------------------
C    Tolerance for close-to-zero comparisons.
C---------------------------------------------------------------------
      real fTolerance
      parameter (fTolerance = 1.0E-06)

C Setup parameters For fuel gases as designated by EN 437 and BS 4947
C These standards specify composition of fuel gases in terms of
C proportions of chemical compounds present in each fuel gas
C Wobbe numbers and calorific values are in J/m^3
C P,Qgas and R are inputs to Siegart's formula
C pChemicalFormula is the % proportion of each chemical in the standard
C fuel gas
      integer maxFuel
      parameter (maxFuel = 280)
      character*5 fuelName (maxFuel)
      real r_netWobbeNo (maxFuel),r_netCalorific (maxFuel)
      real grossWobbe (maxFuel),grossCalorific (maxFuel)
      real relDensity (maxFuel),VA (maxFuel),C (maxFuel),D (maxFuel)
      real P (maxFuel),Qgas (maxFuel),R (maxFuel),N (maxFuel)

C Fuel name to compound proportion in %
      real fuel2ChemProp(maxFuel,iCompound_count)
      real pCH4(maxFuel),pH2(maxFuel),pN2(maxFuel),pC3H8(maxFuel)
      real pC4H10(maxFuel)

      data fuelName(20),r_netWobbeNo(20),r_netCalorific(20),
     &grossWobbe(20),grossCalorific(20),relDensity(20),VA(20),
     &C(20),D(20),P(20),Qgas(20),R(20),N(20),
     &fuel2ChemProp(20,iMethane),
     &pCH4(20),pH2(20),pN2(20),pC3H8(20),pC4H10(20)
     &/'G20',45.67e6,34.02e6,50.72e6,37.78e6,0.555,9.67,
     &0.1105,10.374,100.,0.330,0.0154,11.7,
     &100.,
     &100.,0.,0.,0.,0.
     &/

      data fuelName(21),r_netWobbeNo(21),r_netCalorific(21),
     &grossWobbe(21),grossCalorific(21),relDensity(21),VA(21),
     &C(21),D(21),P(21),Qgas(21),R(21),N(21),
     &fuel2ChemProp(21,iMethane),fuel2ChemProp(21,iPropane),
     &pCH4(21),pH2(21),pN2(21),pC3H8(21),pC4H10(21)
     &/'G21',49.60e6,41.01e6,54.76e6,45.28e6,0.684,11.6,
     &0.1050,10.942,100,0.347,0.0145,12.2,
     &87.,13.,
     &87.,0.,0.,13.,0.
     &/

      data fuelName(222),r_netWobbeNo(222),r_netCalorific(222),
     &grossWobbe(222),grossCalorific(222),relDensity(222),VA(222),
     &C(222),D(222),P(222),Qgas(222),R(222),N(222),
     &fuel2ChemProp(222,iMethane),fuel2ChemProp(222,iHydrogen),
     &pCH4(222),pH2(222),pN2(222),pC3H8(222),pC4H10(222)
     &/'G222',42.87e6,28.53e6,47.87e6,31.86e6,0.443,8.00,
     &0.1173,9.6610,100,0.303,0.0163,10.9,
     &77.,23.,
     &77.,23.,0.,0.,0.
     &/

      data fuelName(23),r_netWobbeNo(23),r_netCalorific(23),
     &grossWobbe(23),grossCalorific(23),relDensity(23),VA(23),
     &C(23),D(23),P(23),Qgas(23),R(23),N(23),
     &fuel2ChemProp(23,iMethane),fuel2ChemProp(23,iNitrogen),
     &pCH4(23),pH2(23),pN2(23),pC3H8(23),pC4H10(23)
     &/'G23',41.11e6,31.46e6,45.66e6,34.95e6,0.586,8.95,
     &0.1020,10.374,100,0.330,0.0154,11.6,
     &92.5,7.5,
     &92.5,0.,7.5,0.,0.
     &/

      data fuelName(231),r_netWobbeNo(231),r_netCalorific(231),
     &grossWobbe(231),grossCalorific(231),relDensity(231),VA(231),
     &C(231),D(231),P(231),Qgas(231),R(231),N(231),
     &fuel2ChemProp(231,iMethane),fuel2ChemProp(231,iNitrogen),
     &pCH4(231),pH2(231),pN2(231),pC3H8(231),pC4H10(231)
     &/'G231',36.82e6,28.91e6,40.90e6,32.11e6,0.617,8.22,
     &0.0921,10.375,100,0.330,0.0154,11.4,
     &85.,15.,
     &85.,0.,15.,0.,0.
     &/

      data fuelName(25),r_netWobbeNo(25),r_netCalorific(25),
     &grossWobbe(25),grossCalorific(25),relDensity(25),VA(25),
     &C(25),D(25),P(25),Qgas(25),R(25),N(25),
     &fuel2ChemProp(25,iMethane),fuel2ChemProp(25,iNitrogen),
     &pCH4(25),pH2(25),pN2(25),pC3H8(25),pC4H10(25)
     &/'G25',37.38e6,29.25e6,41.52e6,32.49e6,0.612,8.32,
     &0.0935,10.374,100,0.330,0.0154,11.5,
     &86.,14.,
     &86.,0.,14.,0.,0.
     &/

      data fuelName(26),r_netWobbeNo(26),r_netCalorific(26),
     &grossWobbe(26),grossCalorific(26),relDensity(26),VA(26),
     &C(26),D(26),P(26),Qgas(26),R(26),N(26),
     &fuel2ChemProp(26,iMethane),fuel2ChemProp(26,iNitrogen),
     &fuel2ChemProp(26,iPropane),
     &pCH4(26),pH2(26),pN2(26),pC3H8(26),pC4H10(26)
     &/'G26',40.52e6,33.36e6,44.83e6,36.91e6,0.678,9.46,
     &0.0935,10.750,100,0.341,0.0148,11.9,
     &80.,13.,7.,
     &80.,0.,13.,7.,0.
     &/

      data fuelName(27),r_netWobbeNo(27),r_netCalorific(27),
     &grossWobbe(27),grossCalorific(27),relDensity(27),VA(27),
     &C(27),D(27),P(27),Qgas(27),R(27),N(27),
     &fuel2ChemProp(27,iMethane),fuel2ChemProp(27,iNitrogen),
     &pCH4(27),pH2(27),pN2(27),pC3H8(27),pC4H10(27)
     &/'G27',35.17e6,27.89e6,39.06e6,30.98e6,0.629,7.93,
     &0.0876,10.375,100,0.330,0.0154,11.4,
     &82.,18.,
     &82.,0.,18.,0.,0.
     &/

      data fuelName(271),r_netWobbeNo(271),r_netCalorific(271),
     &grossWobbe(271),grossCalorific(271),relDensity(271),VA(271),
     &C(271),D(271),P(271),Qgas(271),R(271),N(271),
     &fuel2ChemProp(271,iMethane),fuel2ChemProp(271,iNitrogen),
     &pCH4(271),pH2(271),pN2(271),pC3H8(271),pC4H10(271)
     &/'G271',30.94e6,25.17e6,34.36e6,27.96e6,0.662,7.16,
     &0.0738,10.375,100,0.330,0.0154,11.2,
     &74.,26.,
     &74.,0.,26.,0.,0.
     &/

      data fuelName(30),r_netWobbeNo(30),r_netCalorific(30),
     &grossWobbe(30),grossCalorific(30),relDensity(30),VA(30),
     &C(30),D(30),P(30),Qgas(30),R(30),N(30),
     &fuel2ChemProp(30,iButane),
     &pCH4(30),pH2(30),pN2(30),pC3H8(30),pC4H10(30)
     &/'G30',80.58e6,116.1e6,87.33e6,125.8e6,2.075,32.52,
     &0.0875,12.749,100,0.399,0.0117,14.0,
     &100.,
     &0.,0.,0.,0.,100.
     &/

      data fuelName(31),r_netWobbeNo(31),r_netCalorific(31),
     &grossWobbe(31),grossCalorific(31),relDensity(31),VA(31),
     &C(31),D(31),P(31),Qgas(31),R(31),N(31),
     &fuel2ChemProp(31,iPropane),
     &pCH4(31),pH2(31),pN2(31),pC3H8(31),pC4H10(31)
     &/'G31',70.69e6,88.00e6,76.84e6,95.65e6,1.550,24.65,
     &0.0906,12.432,100,0.389,0.0121,13.7,
     &100.,
     &0.,0.,0.,100.,0.
     &/

     
