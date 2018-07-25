C ESP-r building-related definition file setting maximum
C parameter values.
C 72 zone 62 surface version 12.08 updated
C 82 zone 62 surface version 08.2011 updated
C 82 zone 80 surface version 10.2011 updated
C 82 zone 96 surface version 02.2016 updated
C 3300 surfaces for MCON 3.2012 updated
C 8 air gaps MGP and 16 layers ME 7.2012 updated
C 82 zone 90 surface version 02.2017 updated - supports all exemplars
C and thus is considered the default.
C 82 edges per polygon version 08.2017 updated

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
      integer MCNDV,MCNDC,MPICK,MVAR,MIMG
      integer MCFC,MSPMRES,MBL

C Geometry.
      PARAMETER (MCOM=82)      !- Zones.
      PARAMETER (MS=120)       !- Surfaces/zone (set MNSBZ in cfd.h to at least 2*MS).
      PARAMETER (MCON=3300)    !- Surfaces in model.
      PARAMETER (MTV=360)      !- Vertices/zone.
      PARAMETER (MV=96)        !- Vertices/surface.
      PARAMETER (MST=MS)       !- Used with view factors.
      PARAMETER (MSM=MS+6)     !- Used with view factors.
      PARAMETER (MBL=15)       !- Base surfaces/zone.

C Ground topology.
      PARAMETER (MGRT=400)     !- Surfaces.
      PARAMETER (MGTV=500)     !- Vertices.

C Construction.
      PARAMETER (ME=16)        !- Elements/construction. - 8 to 16 to support CFCs (B Lomanowski)
      PARAMETER (MN=50)        !- Nodes/construction.    - 35 to 50 to support CFCs (B Lomanowski)
      PARAMETER (MGP=8)        !- Air gaps/construction. - 3 to 8 to support CFCs (B Lomanowski)
      PARAMETER (MMLC=200)     !- Multilayered constructions in model.
      PARAMETER (MHCV=MN+ME)   !- Control volumes per construction.

C Glazing.
      PARAMETER (MGT=MS)       !- Insolated glazings in zone
      PARAMETER (MTMC=20)      !- Glazing systems/zone,
      PARAMETER (MGAL=2)       !- Optical sets per optics db item.
      PARAMETER (MANG=9)       !- Angles at which optical data is held.
      PARAMETER (MGOPT=150)    !- Set size in optics database.

C Shading/insolation.
      PARAMETER (MB=200)       !- Site obstruction blocks.
      PARAMETER (MOX=120)      !- Grid lines in x-direction.
      PARAMETER (MOZ=120)      !- Grid lines in z-direction.
      PARAMETER (MISUR=10)     !- Surfaces insolated from one source.
      PARAMETER (MBP=8)        !- TMC blind control periods.

C Variable thermo-physical properties.
      PARAMETER (MTHF=12)      !- Thermal functions.
      PARAMETER (MTHEQ=3)      !- Thermal equations.
      PARAMETER (MDATA=12)     !- Defining data items.

C Special materials.
      PARAMETER (MSPMNOD=40)   !- Special materials.
      PARAMETER (MSPMDAT=21)   !- Defining data items.
      PARAMETER (MSPMRES=12)   !- Output data items

C Casual gains.
      PARAMETER (MCT=7,MGTY=7) !- Casual gain types.
      PARAMETER (MDTY=15)      !- Day types.
      PARAMETER (MGPER=72)     !- Periods.
      PARAMETER (MC=24*MCT*MDTY)!- Total casual gains.

C Mean radiant temperature.
      PARAMETER (MCUB=16)        !- Sensors in zone.
      PARAMETER (MGC=42000)     !- Surface grid cells (rougly associated with MS).
      PARAMETER (MPATCH=200)    !- Hemispherical patches.

C Time.
      PARAMETER (MP=7)          !- Days in week.
      PARAMETER (MT=24)         !- Hours in day.
      PARAMETER (MDY=3)         !- Day types.
      PARAMETER (MA=24)         !- Air change periods.

C Casual gain control.
      PARAMETER (MLCOM=4)       !- Periods.
      PARAMETER (MDF=9)         !- Daylight factor sets.
      PARAMETER (MDCFP=5)       !- Daylight coefficient sets.
      PARAMETER (MDCFS=2)
      PARAMETER (MDCFV=145)     !- Sky patches.

C Systems control.
      PARAMETER (MSEN=4)
      PARAMETER (MISC=9+8*MSEN) !- Miscellaneous data items per control law
      PARAMETER (MO=24)
      PARAMETER (MF=100)
      PARAMETER (MFP=4)         !- Fuzzy control periods.

C Simulation control.
      PARAMETER (MTRACE=60)     !- Trace parameters.
      PARAMETER (MTMS=1440)
      PARAMETER (MTS=1440)
      PARAMETER (MGRDP=43)      !- Ground temperature profiles.

C Time-step control.
      PARAMETER (MCVT1=6)
      PARAMETER (MCVT2=2)
      PARAMETER (MSCH=24)
      PARAMETER (IRWMAX=20)     !- For type 6.

C Results analysis.
      PARAMETER (MSPS=30)       !- Simulation parameter sets.
      PARAMETER (MNFA=4)        !- Factorial analyses.
C      PARAMETER (MNRS=2**MNFA)  !- Result sets (set to 100 for sensitivity analysis).
      PARAMETER (MNRS=100)      !- Result sets (set to 100 for sensitivity analysis).
      PARAMETER (MIPVA=MSPS)    !- IPV assessments.
      PARAMETER (MIPVM=12)      !- IPV metrics.
      PARAMETER (MZS=120)       !- Number of items to report in res (biggest of MCOM or MS).
      PARAMETER (MZRL=MS+12)    !- Fields in a zone results library record. It should
                                !  be largest of (40 for file names or MCON+12 for zones
                                !  or MS+12 or MGP * 5). See reslib.F for logic.
C Mathematical model.
      PARAMETER (MEQ=MS+1)      !- Equations.
      PARAMETER (MTR=MS+4)      !- Equation terms.

C Multi-gridding.
      PARAMETER (MVS=50)        !- Local vertices per surface.
      PARAMETER (MGV=100)       !- Gridding vertices per surface.
      PARAMETER (MLS=100)       !- Gridding lines per surface.
      PARAMETER (MLBS=20)       !- Bounary lines per surface.
      PARAMETER (MCVS=50)       !- Control volumes per surface.
      PARAMETER (MEZ=50)        !- Edges per zone.
      PARAMETER (MZE=ME+2)      !- Levels in edge.
      PARAMETER (MCZ=10)        !- Corners per zone.
      PARAMETER (MECZ=3*MCZ)    !- Edge-corner connections.
      PARAMETER (MSSZ=20)       !- Surface-surface connections.
      PARAMETER (MZSS=20)       !- z-intervals in surface-surface connections.
      PARAMETER (MYSS=6*ME)     !- y-intervals in surface-surface connections.
      PARAMETER (MSEZ=70)       !- Surface-edge connections.
      PARAMETER (MZSE=20)       !- z-intervals in surface-edge connections.
      PARAMETER (MYSE=6*ME)     !- y-intervals in surface-edge connections.
      PARAMETER (MEEZ=30)       !- Co-linear edge-edge connection.
      PARAMETER (MXEE=6*ME)     !- x-intervals in co-linear edge-edge connection.
      PARAMETER (MYEE=6*ME)     !- y-intervals in co-linear edge-edge connection.
      PARAMETER (MXEC=6*ME)     !- x-intervals in edge-corner connection.
      PARAMETER (MYEC=6*ME)     !- y-intervals in edge-corner connection.
      PARAMETER (MCVV=10)       !- Intersection vertices per control volume.
      PARAMETER (MNOD3=1000)    !- Nodes in a 3D zone model.
      PARAMETER (MCFF3=20*MNOD3)!- Zone matrix coefficients.
      PARAMETER (MNOD3T=MNOD3+2)!- Cofficients in the temporary storage array.
      PARAMETER (MGXYZ=15)      !- x, y and z intervals for ground.
      PARAMETER (MBTYP=10)      !- Boundary types for ground.
      PARAMETER (MNM=50)        !- Moisture nodes.

C Structured-gridding.
      PARAMETER (MROW=12)       !- Rows.
      PARAMETER (MCOL=12)       !- Columns.
      PARAMETER (MLAY=12)       !- Layers.
      PARAMETER (MGRID=10*MROW*MCOL*MLAY)    !- Control volumes (should be less than MNOD3).
      PARAMETER (MLEV=12)
      PARAMETER (MMTR=30)       !- Material geometries.
      PARAMETER (MBND=30)       !- Surface boundary boxes.
      PARAMETER (MSNOD=MNOD3/10)!- Surface nodes.
      PARAMETER (MMAT=MCON*ME)  !- Materials.

C Miscellaneous.
      PARAMETER (MCNDV=20)
      PARAMETER (MCNDC=100)
      PARAMETER (MPICK=30)
      PARAMETER (MVAR=5)
      PARAMETER (MIMG=10)       !- Images associated with model.
      PARAMETER (MCFC=7)        ! Maximum number of different CFC types within a zone.
