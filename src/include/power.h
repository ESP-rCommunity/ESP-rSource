C Network parameters:
C Number of electrical network types 1-d.c. 2-1-ph ... 4-3ph 5-balanced
      integer MENTYP
      PARAMETER (MENTYP=5)

C The number of phase types 1-d.c. 2-1-ph ... 4-3-ph 5-balanced
      integer MPHATY
      PARAMETER (MPHATY=5)

C The maximum mumber of phases
      INTEGER MPHAS
      PARAMETER (MPHAS=3)

C The maximum number of electrical nodes
      INTEGER MENOD
      PARAMETER (MENOD=100)

C The maximum number of connected hybrid nodes and data items
      INTEGER MHYCOM, MHYDAT
      PARAMETER (MHYCOM=100)
      PARAMETER (MHYDAT=30)

C The maximum number of connected power components and data items
      INTEGER MPOWCOM,MPWDAT,MPCDS
      PARAMETER (MPOWCOM=30)
      PARAMETER (MPWDAT=75)
      PARAMETER (MPCDS=30)

C The maximum number of connecting components and data items
      INTEGER MCONECOM, MCCDAT
      PARAMETER (MCONECOM=100)
      PARAMETER (MCCDAT=30)

C The maximum number of connections
      INTEGER MECON
      PARAMETER (MECON=100)

C Maximim number of actual and PU Z array elements
      INTEGER MZARRY
      PARAMETER (MZARRY=18)

C Maximim number of primitive Y array elements
      INTEGER MYARRY
      PARAMETER (MYARRY=18)

C Solution parameters:
C Matrix parameters equals 2XMENOD.
      INTEGER MJROW
      PARAMETER (MJROW=200)
      INTEGER MJCOL
      PARAMETER (MJCOL=200)

C Iteration and convergence criteria
      INTEGER MENRITER
      PARAMETER (MENRITER=30)

C Library parameter, electrical library number (IFIL+MADFIL) and width.
      INTEGER MADFIL
      PARAMETER (MADFIL=21)

C The library width should be an even number.
      INTEGER MELIBW
      PARAMETER (MELIBW=20)

C The electrical results library header.
      INTEGER MELIBH
      PARAMETER (MELIBH=3)

C The maximum number of recovered data items associated with one electrical
C network entity.
      INTEGER MERPOS
      PARAMETER (MERPOS=6)

C The 1-D revover array width.
      INTEGER MERAWD
      PARAMETER (MERAWD=200)

C Maximum number of additional variables requiring time-row transport that
C can be assocaited with a power-only component.
      INTEGER MEnet_Time_Row_MAX
      PARAMETER ( MEnet_Time_Row_MAX = 30 )
