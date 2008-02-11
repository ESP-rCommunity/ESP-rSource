C The following is a parameter file for building related facilites.
C Note that if this is changed then both the esru libraries and ESRU
C applications must be recompiled.
C August 95 version.
C Parameters misur and maxwin added 1.11.91
C Parameters mcub, mgc,mpatch added 9.05.92
C Parameters mgrt, mgtv added 31.03.95
C 32 Surface version 07.00
C 42 Surface version 05.03
C 52 Surface version 08.03

C Used in the ESP suite of programs written by staff of the
C Energy Simulation Research Unit of the University of Strathclyde.

C Type declarations
      integer MCOM,MCON,MTV,MV,ME,MGP,MN,MP,MDY,MT,MA,MC,MBP,MTMS
      integer MTS,MGT,MCT,MTRACE,MGRDP,MLCOM,MDF,MDCFP,MDCFS,MDCFV
      integer MCF,MBCDT,MBCDP,MTMC,MGAL,MANG,MGOPT,MISC,MSEN,MO,MF
      integer MGRT,MGTV,MST, MS,MSM,MZS,MZRL,MEQ,MTR,MMLC,MCVT1
      integer MCVT2,MSCH,IRWMAX,MB,MOX,MOZ,misur,MCUB,MGC,MPATCH
      integer MTHF,MTHEQ,MDATA,MHCV,MFP,MSPMNOD,MSPMDAT,MVS,MGV,MLS
      integer MLBS,MCVS,MEZ,MZE,MCZ,MSSZ,MZSS,MYSS,MSEZ,MZSE,MYSE
      integer MEEZ,MXEE,MYEE,MECZ,MXEC,MYEC,MCVV,MNOD3,MCFF3,MNOD3T
      integer MGXYZ,MBTYP,MNM,MNRS,MNFA,MROW,MCOL,MLAY,MLEV,MGRID
      integer MMTR,MBND,MSNOD,MMAT,MIPVM,MIPVA,MSPS
      integer MDTY,MGTY,MGPER

C For the building:
      PARAMETER (MCOM=28)
      PARAMETER (MCON=900)
      PARAMETER (MTV=250)
      PARAMETER (MV=24)
      PARAMETER (ME=8)
      PARAMETER (MGP=3)
      PARAMETER (MN=35)
      PARAMETER (MP=7)
      PARAMETER (MDY=3)
      PARAMETER (MT=24)
      PARAMETER (MA=24)
      PARAMETER (MBP=8)
      PARAMETER (MTMS=1440)
      PARAMETER (MTS=1440)

C 'mgt' is the maximum number of insolated glazings and
C this should be set to the same as MS. 
      PARAMETER (MGT=52)
      PARAMETER (MCT=3)
      PARAMETER (MTRACE=60)
      PARAMETER (MGRDP=10)

C Casual gains limits.
      PARAMETER (MDTY=3)
      PARAMETER (MGTY=7)
      PARAMETER (MGPER=72)

C Allow for 24 (one per hour) * 3 (day types) * 3 (casual gain types)
      PARAMETER (MC=72)

C Casual gain control settings:
      PARAMETER (MLCOM=4)
      PARAMETER (MDF=9)
      PARAMETER (MDCFP=5)
      PARAMETER (MDCFS=2)
      PARAMETER (MDCFV=145)

C Zone control functions, day types, periods in day.
      PARAMETER (MCF=28,MBCDT=10,MBCDP=8)

C MTMC is number of different glazing systems per zone, MGAL is number
C of optical sets per optics db item and TMC and MANG is the number of
C angles at which optical data is held. MGOPT is the maximum size of
C optic sets in optics database.
      PARAMETER (MTMC=7,MGAL=2,MANG=9,MGOPT=90)

C MISC is the maximum number of miscellaneous data
C items for control laws.
C Should be equal to 9 + 8*MSEN with a minimum of 16
      PARAMETER (MISC=41)
      PARAMETER (MSEN=4)
      PARAMETER (MO=24)

      PARAMETER (MF=100)

C Ground topology
      PARAMETER (MGRT=400)
      PARAMETER (MGTV=500)

C Change next 2 settings to increase/ reduce
C number of surfaces. MSM is MS+6 for viewfactor
C calculations. Also set MNSBZ in cfd.h to equal MS.
      PARAMETER (MST=52)
      PARAMETER (MS=52)
      PARAMETER (MSM=58)

C MZS is the number of items to report/select in res.
      PARAMETER (MZS=90)

C MZRL is associated with data taking up all fields of a zone
C results library (i.e. is = nzrl). Should be larger of either MCOM or MS+12 or 36.
      PARAMETER (MZRL=64)

C If MS changes, so should MEQ (MS+1) and MTR (MS+4).
      PARAMETER (MEQ=53)
      PARAMETER (MTR=56)

C Multilayered construction limit
      PARAMETER (MMLC=80)

C For the time-step controller:
      PARAMETER (MCVT1=6)
      PARAMETER (MCVT2=2)
      PARAMETER (MSCH=24)
      
C For the time-step controller type 6:
      PARAMETER (IRWMAX=20)

C For shading/ insolation:
      PARAMETER (MB=80)
      PARAMETER (MOX=50)
      PARAMETER (MOZ=50)

C 'misur' is the maximum number of surfaces insolated from one source.
      parameter (misur=10)

C For mrt calculations:
C 'mcub' is the number of MRT sensors in a zone.
      PARAMETER (MCUB=4,MGC=20000,MPATCH=200)

C For variable thermophysical properties:
      PARAMETER (MTHF=12,MTHEQ=3,MDATA=12)

C Maximum number of homogeneous control volumes per construction.
      PARAMETER (MHCV=MN+ME)

C For max no. of fuzzy control periods:
      PARAMETER (MFP=4)

C special materials parameters.
      PARAMETER (MSPMNOD=40,MSPMDAT=16)

C Multi-gridding:

C Maximum number of local vertices per surface.
      PARAMETER (MVS=50)

C Maximum number gridding vertices per surface.
      PARAMETER (MGV=100)

C Maximum number of gridding lines per surface.
      PARAMETER (MLS=100)

C Maximum number of bounary lines per surface.
      PARAMETER (MLBS=20)

C Maximum number of control volumes per surface.
      PARAMETER (MCVS=50)

C Maximum number of edges per zone.
      PARAMETER (MEZ=50)

C Maximum number of levels in edge.
      PARAMETER (MZE=ME+2)

C Maximum number of corners per zone.
      PARAMETER (MCZ=10)

C Maximum number of surface-surface connections.
      PARAMETER (MSSZ=20)

C Maximum number of z-intervals in surface-surface connections.
      PARAMETER (MZSS=20)

C Maximum number of y-intervals in surface-surface connections.
      PARAMETER (MYSS=6*ME)

C Maximum number of surface-edge connections.
      PARAMETER (MSEZ=70)

C Maximum number of z-intervals in surface-edge connections.
      PARAMETER (MZSE=20)

C Maximum number of y-intervals in surface-edge connections.
      PARAMETER (MYSE=6*ME)

C Maximum number of co-linear edge-edge connection.
      PARAMETER (MEEZ=30)

C Maximum number of x-intervals in co-linear edge-edge connection.
      PARAMETER (MXEE=6*ME)

C Maximum number of y-intervals in co-linear edge-edge connection.
      PARAMETER (MYEE=6*ME)

C Maximum number of edge-corner connections.
      PARAMETER (MECZ=3*MCZ)

C Maximum number of x-intervals in edge-corner connection.
      PARAMETER (MXEC=6*ME)

C Maximum number of y-intervals in edge-corner connection.
      PARAMETER (MYEC=6*ME)

C Maximum number of intersection vertices per control volume.
      PARAMETER (MCVV=10)

C Maximum number of nodes in a 3D zone model.
      PARAMETER (MNOD3=1000)

C Maximum number of zone matrix coefficients (linked to
C MNOD3 - pragmatic suggestion is MNOD3 times 20).
      PARAMETER (MCFF3=20000)

C Maximum number of cofficients in the temporary full storage array.
      PARAMETER (MNOD3T=MNOD3+2)

C Maximum number of X,Y,and Z - interval for ground.
      PARAMETER (MGXYZ=15)

C Maximum number of boundary types for ground.
      PARAMETER (MBTYP=10)

C Maximum number of moisture nodes.
      PARAMETER (MNM=50)

C For results analysis:
C   Maximum number of result sets - should either 
C   be 30 (default) or 100 for sensitivity analysis:
      PARAMETER (MNRS=30)

C For factorial analysis: MNRS >= 2^MNFA
      PARAMETER (MNFA=4)

C Structured-gridding:
C Maximum number of rows, columns, layers.
      PARAMETER (MROW=12,MCOL=12,MLAY=12)

C Maximum number of rows, columns, and layers.
      PARAMETER (MLEV=12)

C Maximum number of control volumes ( should be less than MNOD3).
      PARAMETER (MGRID=10*MROW*MCOL*MLAY)

C Maximum number of material geometries.
      PARAMETER (MMTR=30)

C Maximum number of surface boundary boxes.
      PARAMETER (MBND=30)

C Maximum number of surface nodes.
      PARAMETER (MSNOD=MNOD3/10)

C Maximum number of materials
      PARAMETER (MMAT=MCON*ME)

C Maximum number of IPV metrics and assessments to be run (should be
C less than or equal to MSPS.
      PARAMETER (MIPVM=12,MIPVA=10)

C Maximum number of simulation parameter sets.
      PARAMETER (MSPS=10)
