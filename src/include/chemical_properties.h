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
C.....First zero all arrays      
      data ( ( iCompound_Composition(iCPc, iElc ),
     &         iELc=1,iElement_count ),
     &       iCPc = 1, iCompound_count
     &     )
     &     / iVector_length * 0 /

C.....Now add elemental compositions     
      data iCompound_Composition(iHydrogen,   iElement_Hydrogen ) /  2 /

      data iCompound_Composition(iMethane,    iElement_Carbon   ) /  1 /
      data iCompound_Composition(iMethane,    iElement_Hydrogen ) /  4 /

      data iCompound_Composition(iEthane,     iElement_Carbon   ) /  2 /
      data iCompound_Composition(iEthane,     iElement_Hydrogen ) /  6 /

      data iCompound_Composition(iPropane,    iElement_Carbon   ) /  3 /
      data iCompound_Composition(iPropane,    iElement_Hydrogen ) /  8 /

      data iCompound_Composition(iButane,     iElement_Carbon   ) /  4 /
      data iCompound_Composition(iButane,     iElement_Hydrogen ) / 10 /

      data iCompound_Composition(iHexane,     iElement_Carbon   ) /  6 /
      data iCompound_Composition(iHexane,     iElement_Hydrogen ) / 14 /

      data iCompound_Composition(iPentane,    iElement_Carbon   ) /  5 /
      data iCompound_Composition(iPentane,    iElement_Hydrogen ) / 12 /

      data iCompound_Composition(iC_Monoxide, iElement_Carbon   ) /  1 /
      data iCompound_Composition(iC_Monoxide, iElement_Oxygen   ) /  1 /

      data iCompound_Composition(iC_Dioxide,  iElement_Carbon   ) /  1 /
      data iCompound_Composition(iC_Dioxide,  iElement_Oxygen   ) /  2 /

      data iCompound_Composition(iOxygen,     iElement_Oxygen   ) /  2 /

      data iCompound_Composition(iNitrogen,   iElement_Nitrogen ) /  2 /

      data iCompound_Composition(iWater_l,    iElement_Hydrogen ) /  2 /
      data iCompound_Composition(iWater_l,    iElement_Oxygen   ) /  1 /
      
      data iCompound_Composition(iWater_v,    iElement_Hydrogen ) /  2 /
      data iCompound_Composition(iWater_v,    iElement_Oxygen   ) /  1 /

      data iCompound_Composition(iMethanol,   iElement_Carbon   ) /  1 /
      data iCompound_Composition(iMethanol,   iElement_Hydrogen ) /  4 /
      data iCompound_Composition(iMethanol,   iElement_Oxygen   ) /  1 /

      data iCompound_Composition(iEthanol,    iElement_Carbon   ) /  2 /
      data iCompound_Composition(iEthanol,    iElement_Hydrogen ) /  6 /
      data iCompound_Composition(iEthanol,    iElement_Oxygen   ) /  1 /

      save iCompound_Composition
      
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
C     Universial Ideal gas constant (J/kMol K)
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

      
