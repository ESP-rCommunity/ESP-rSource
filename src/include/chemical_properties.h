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
C.....Number of known compounds
      integer iCompound_count
      parameter ( iCompound_count = 13 )

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

C.....Heating value
      integer iLHV, iHHV
      parameter (iLHV = 1, iHHV = 2)

C-----------------------------------------------------------------------
C     Elemental Molar masses(kg/kmol). Source: Van Wylen et al. 1994
C-----------------------------------------------------------------------
C.....(hydrogen, carbon, oxygen)
      real fMM_H, fMM_C, fMM_O, fMM_N
      parameter ( fMM_H = 1.00794  )
      parameter ( fMM_C = 12.011   )
      parameter ( fMM_O = 15.9994  )
      parameter ( fMM_N = 14.0067  )

C-----------------------------------------------------------------------
C     Compound enthalpies of formation (J / kmol), @ 25oC, 100 kPa
C     Source: Van Wylen et al. 1994
C-----------------------------------------------------------------------
      real fEF_Water_l, fEF_Water_v
      real fEF_Hydrogen, fEF_Methane, fEF_Oxygen, fEF_Propane
      real fEF_Ethane, fEF_Hexane, fEF_Pentane, fEF_C_Monoxide
      real fEF_C_Dioxide, fEF_Butane, fEF_Nitrogen

      parameter ( fEF_Water_v = -241.826E06 )
      parameter ( fEF_Water_l = -285.830E06 )
      parameter ( fEF_Hydrogen = 0.000 )
      parameter ( fEF_Methane = - 74.873E06 )
      parameter ( fEF_Ethane = - 84.740E06 )
      parameter ( fEF_Propane = -103.900E06 )
      parameter ( fEF_Butane = -126.200E06 )
      parameter ( fEF_Pentane = -146.500E06 )
      parameter ( fEF_Hexane = -167.300E06 )
      parameter ( fEF_C_Monoxide = -110.527E06 )
      parameter ( fEF_C_Dioxide = -393.522E06 )
      parameter ( fEF_Oxygen = 0.000 )
      parameter ( fEF_Nitrogen = 0.000 )


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
      integer iCPc
      
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

      
