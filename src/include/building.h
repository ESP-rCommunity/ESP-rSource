C ESP-r building-related definition file setting maximum
C parameter values.
C 72 zone 62 surface version 12.08 updated

C Type declarations.
      integer MCOM,MCON,MTV,MV,ME,MGP,MN,MP,MDY,MT,MA,MC,MBP,MTMS
      integer MTS,MGT,MCT,MTRACE,MGRDP,MLCOM,MDF,MDCFP,MDCFS,MDCFV
      integer MTMC,MGAL,MANG,MGOPT,MISC,MSEN,MO,MF,MGRT,MGTV,MST
      integer MS,MSM,MZS,MZRL,MEQ,MTR,MMLC,MCVT1,MCVT2,MSCH,IRWMAX
      integer MB,MOX,MOZ,MISUR,MCUB,MGC,MPATCH,MTHF,MTHEQ,MDATA,MHCV
      integer MFP,MSPMNOD,MSPMDAT,MVS,MGV,MLS,MLBS,MCVS,MEZ,MZE,MCZ
      integer MSSZ,MZSS,MYSS,MSEZ,MZSE,MYSE,MEEZ,MXEE,MYEE,MECZ
      integer MXEC,MYEC,MCVV,MNOD3,MCFF3,MNOD3T,MGXYZ,MBTYP,MNM
      integer MNRS,MNFA,MROW,MCOL,MLAY,MLEV,MGRID,MMTR,MBND,MSNOD
      integer MMAT,MIPVM,MIPVA,MSPS,MDTY,MGTY,MGPER
      integer MCNDV,MCNDC,MPICK,MVAR,MIMG,MSPMRES
      integer MCFC,MILDB,MNSCN

C Geometry.
      PARAMETER (MCOM=72)	!- Zones.
      PARAMETER (MS=62)		!- Surfaces/zone (set MNSBZ in cfd.h to at least 2*MS).
      PARAMETER (MCON=2200)	!- Edges/zone.
      PARAMETER (MTV=200)	!- Vertices/zone.
      PARAMETER (MV=42)		!- Vertices/surface.
      PARAMETER (MST=MS)	!- Used with view factors.
      PARAMETER (MSM=MS+6)	!- Used with view factors.

C Ground topology.
      PARAMETER (MGRT=400)	!- Surfaces.
      PARAMETER (MGTV=500)	!- Vertices.

C Construction.
      PARAMETER (ME=30)		!- Elements/construction. - Increased from 8 to 20 to support CFCs - (Bart Lomanowski)
      PARAMETER (MN=50)		!- Nodes/construction.    - Increased from 35 to 50 to support CFCs - (Bart Lomanowski)
      PARAMETER (MGP=10)	!- Air gaps/construction. - Increased from 3 to 10 to support CFCs - (Bart Lomanowski)
      PARAMETER (MMLC=200)	!- Multilayered constructions in model.
      PARAMETER (MHCV=MN+ME)	!- Control volumes per construction.

C Glazing.
      PARAMETER (MGT=MS)	!- Insolated glazings in zone
      PARAMETER (MTMC=7)	!- Glazing systems/zone,
      PARAMETER (MGAL=2)	!- Optical sets per optics db item.
      PARAMETER (MANG=9)	!- Angles at which optical data is held.
      PARAMETER (MGOPT=150)	!- Set size in optics database.

C Shading/insolation.
      PARAMETER (MB=180)	!- Site obstruction blocks.
      PARAMETER (MOX=210)	!- Grid lines in x-direction.
      PARAMETER (MOZ=120)	!- Grid lines in z-direction.
      PARAMETER (MISUR=10)	!- Surfaces insolated from one source.
      PARAMETER (MBP=8)		!- TMC blind control periods.

C Variable thermo-physical properties.
      PARAMETER (MTHF=12)	!- Thermal functions.
      PARAMETER (MTHEQ=3)	!- Thermal equations.
      PARAMETER (MDATA=12)	!- Defining data items.

C Special materials.
      PARAMETER (MSPMNOD=40)	!- Special materials.
      PARAMETER (MSPMDAT=21)	!- Defining data items.
      PARAMETER (MSPMRES=10)	!- Number of result data.

C Casual gains.
      PARAMETER (MCT=7,MGTY=7)	!- Casual gain types.
      PARAMETER (MDTY=10)	!- Day types.
      PARAMETER (MGPER=72)	!- Periods.
      PARAMETER (MC=24*MCT*MDTY)!- Total casual gains.

C Mean radiant temperature.
      PARAMETER (MCUB=4)	!- Sensors in zone.
      PARAMETER (MGC=26000)	!- Surface grid cells.
      PARAMETER (MPATCH=200)	!- Hemispherical patches.

C Time.
      PARAMETER (MP=7)		!- Days in week.
      PARAMETER (MT=24)		!- Hours in day.
      PARAMETER (MDY=3)		!- Day types.
      PARAMETER (MA=24)		!- Air change periods.

C Casual gain control.
      PARAMETER (MLCOM=4)	!- Periods.
      PARAMETER (MDF=9)		!- Daylight factor sets.
      PARAMETER (MDCFP=5)	!- Daylight coefficient sets.
      PARAMETER (MDCFS=2)
      PARAMETER (MDCFV=145)	!- Sky patches.
      PARAMETER (MILDB=40000)	!- Radiance illuminance data sets (?)

C Systems control.
      PARAMETER (MSEN=4)
      PARAMETER (MISC=9+8*MSEN)	!- Miscellaneous data items per control law
      PARAMETER (MO=24)
      PARAMETER (MF=100)
      PARAMETER (MFP=4)		!- Fuzzy control periods.

C Simulation control.
      PARAMETER (MTRACE=60)	!- Trace parameters.
      PARAMETER (MTMS=1440)
      PARAMETER (MTS=1440)
      PARAMETER (MGRDP=43)	!- Ground temperature profiles.

C Time-step control.
      PARAMETER (MCVT1=6)
      PARAMETER (MCVT2=2)
      PARAMETER (MSCH=24)
      PARAMETER (IRWMAX=20)	!- For type 6.

C Results analysis.
      PARAMETER (MSPS=10)	!- Simulation parameter sets.
      PARAMETER (MNFA=4)	!- Factorial analyses.
      PARAMETER (MNRS=2**MNFA)	!- Result sets (set to 100 for sensitivity analysis).
      PARAMETER (MIPVA=MSPS)	!- IPV assessments.
      PARAMETER (MIPVM=12)	!- IPV metrics.
      PARAMETER (MZS=90)	!- Number of items to report in res.
      PARAMETER (MZRL=MS+12)	!- Fields in a zone results library record
                                !  (minimum value is 36; if MCOM>MS use MCOM
                                !  in formula).
C Mathematical model.
      PARAMETER (MEQ=MS+1)	!- Equations.
      PARAMETER (MTR=MS+4)	!- Equation terms.

C Multi-gridding.
      PARAMETER (MVS=50)	!- Local vertices per surface.
      PARAMETER (MGV=100)	!- Gridding vertices per surface.
      PARAMETER (MLS=100)	!- Gridding lines per surface.
      PARAMETER (MLBS=20)	!- Bounary lines per surface.
      PARAMETER (MCVS=50)	!- Control volumes per surface.
      PARAMETER (MEZ=50)	!- Edges per zone.
      PARAMETER (MZE=ME+2)	!- Levels in edge.
      PARAMETER (MCZ=10)	!- Corners per zone.
      PARAMETER (MECZ=3*MCZ)	!- Edge-corner connections.
      PARAMETER (MSSZ=20)	!- Surface-surface connections.
      PARAMETER (MZSS=20)	!- z-intervals in surface-surface connections.
      PARAMETER (MYSS=6*ME)	!- y-intervals in surface-surface connections.
      PARAMETER (MSEZ=70)	!- Surface-edge connections.
      PARAMETER (MZSE=20)	!- z-intervals in surface-edge connections.
      PARAMETER (MYSE=6*ME)	!- y-intervals in surface-edge connections.
      PARAMETER (MEEZ=30)	!- Co-linear edge-edge connection.
      PARAMETER (MXEE=6*ME)	!- x-intervals in co-linear edge-edge connection.
      PARAMETER (MYEE=6*ME)	!- y-intervals in co-linear edge-edge connection.
      PARAMETER (MXEC=6*ME)	!- x-intervals in edge-corner connection.
      PARAMETER (MYEC=6*ME)	!- y-intervals in edge-corner connection.
      PARAMETER (MCVV=10)	!- Intersection vertices per control volume.
      PARAMETER (MNOD3=1000)	!- Nodes in a 3D zone model.
      PARAMETER (MCFF3=20*MNOD3)!- Zone matrix coefficients.
      PARAMETER (MNOD3T=MNOD3+2)!- Cofficients in the temporary storage array.
      PARAMETER (MGXYZ=15)	!- x, y and z intervals for ground.
      PARAMETER (MBTYP=10)	!- Boundary types for ground.
      PARAMETER (MNM=50)	!- Moisture nodes.

C Structured-gridding.
      PARAMETER (MROW=12)	!- Rows.
      PARAMETER (MCOL=12)	!- Columns.
      PARAMETER (MLAY=12)	!- Layers.
      PARAMETER (MGRID=10*MROW*MCOL*MLAY)	!- Control volumes (should be less than MNOD3).
      PARAMETER (MLEV=12)
      PARAMETER (MMTR=30)	!- Material geometries.
      PARAMETER (MBND=30)	!- Surface boundary boxes.
      PARAMETER (MSNOD=MNOD3/10)!- Surface nodes.
      PARAMETER (MMAT=MCON*ME)	!- Materials.

C Miscellaneous.
      PARAMETER (MCNDV=20)
      PARAMETER (MCNDC=100)
      PARAMETER (MPICK=30)
      PARAMETER (MVAR=5)
      PARAMETER (MIMG=10)	!- Images associated with model.
      PARAMETER (MCFC=12)	! Maximum number of different CFC types within a zone.
      PARAMETER (MNSCN=10) ! Maximum number of Radiance scenes (now per .rcf, should be per zone)
