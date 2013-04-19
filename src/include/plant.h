C plant.h

C ============= Array Dimensions =================================
C
C The following parameters are used to dimension arrays
C used in the plant domain
C
C ================================================================

C Type declarations:
      integer MADATA, MAXPC,  MBDATA, MCOEFG, MCONVR, MPCRES
      integer MMISCD, MNODEC, MPCDAT, MPP, MPPC, MDLY
      integer   MPCOE, MPCOM, MPCON, MPCONC, MPNODE, MPVAR
      integer NPRECL, MPSEN, MPDLAY 

C Maximum number of component ADATA (=ESPpdb:MAXMSC)
      PARAMETER (MADATA=140)
C Maximum number of plant database entries (=ESPpdb:MAXPC)
      PARAMETER (MAXPC=460)
C Maximum number of component BDATA
      PARAMETER (MBDATA=140)
C Maximum number of plant network matrix coefficients (<= MPCOM * MPCOE)
      PARAMETER (MCOEFG=2000)
C Maximum number of data per component inter-connection (held by CONVAR)
      PARAMETER (MCONVR=4)
C Maximum number plant component additional output variables
      PARAMETER (MPCRES=15)
C Maximum number of plant component CDATA
      PARAMETER (MMISCD=5)
C Maximum number of nodes per plant component (=ESPpdb:MAXNOD)
      PARAMETER (MNODEC=20)
C Maximum number of plant component generator 'storage data'
CHOW
      PARAMETER (MPCDAT=20)
C  Maximum number of primitive parts
      PARAMETER (MPP=110)
C  Maximum number of coefficients in each primitive part
      PARAMETER (MPPC=15)
C Maximum number of sections in transport delay simulation
      PARAMETER (MDLY=100)
CHOWEND
C Maximum number of plant matrix coefficients for a component model
      PARAMETER (MPCOE=50)
C Maximum number of plant components in overall system
      PARAMETER (MPCOM=105)
C Maximum number of plant component inter-connections
      PARAMETER (MPCON=120)
C Maximum number of plant component inter-connections per plant node
      PARAMETER (MPCONC=4)
C Maximum number of plant nodes in overall system
      PARAMETER (MPNODE=140)
C Maximum number of plant component state variable types
      PARAMETER (MPVAR=4)
C Record length of plant results binary file.
      PARAMETER (NPRECL=21)
C Maximum number of auxiliary sensors.
      PARAMETER (MPSEN=2)
C Maximum number of control system delay time-steps.
      PARAMETER (MPDLAY=4)

C ============= Named constants =================================
C The following named constants are used strictly to clarify
C code.
C ===============================================================

C----------------------------------------------------------------------
C     Flags for convective/radiant/embedded gains 
C----------------------------------------------------------------------
C.....Types of gains      
      integer iConductive, iConvective, iRadiant 
      parameter ( iConvective=1, iRadiant=2, iConductive=3)

C ============ Commons ==========================================
C Standard common variables
C ===============================================================

C----------------------------------------------------------------------
C This array holds the length of the character names for all plant
C comonents.
C----------------------------------------------------------------------
      integer iPltNameLen(MPCOM)
      common/GlobalPlantData/iPltNameLen
