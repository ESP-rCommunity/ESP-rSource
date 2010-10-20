C This header relates to summary information derived from model entities in ESP-r.
C It is dependant on building.h and should follow building.h so that
C parameters will have been defined.

C Average U values.
      real uavgtran   ! average U value of windows (per zone)
      real uavwall    ! average U value of walls (per zone)
      real uavslproof ! average U value of sloped roofs (per zone)
      real uavfltroof ! average U value of flat roof (per zone)
      real uavgsky    ! average U value of skylights (per zone)
      real uavground  ! average U value at ground (per zone)
      real xuavgtran    ! average U value of windows (transparent surfaces)
      real xuavwall     ! average U value of walls
      real xuavground   ! average U value of surfaces connected to ground
      real xuavslproof  ! average U value of sloped roofs
      real xuavfltroof  ! average U value of flat roof
      real xuavgsky     ! average U value of skylights
      common/znuvals/uavgtran(MCOM),uavwall(MCOM),uavslproof(MCOM),
     &  uavfltroof(MCOM),uavgsky(MCOM),uavground(MCOM),xuavgtran,
     &  xuavwall,xuavground,xuavslproof,xuavfltroof,xuavgsky

C Aggregate areas (m^2).
      real areatran    ! m2 of windows (per zone)
      real areawall    ! m2 of walls (per zone)
      real areaslproof ! m2 of sloped roofs (per zone)
      real areafltroof ! m2 of flat roofs (per zone)
      real areaskylt   ! m2 of skylights (per zone)
      real areaground  ! m2 of ground contact (per zone)
      real exposed     ! m2 of outside surface area (per zone)
      real vexposed    ! m2 of outside vertical surface (per zone)
      real xfloor      ! m2 project floor area based on basearea
      real xexposed    ! m2 project outside surface area (any orientation)
      real xvexposed   ! m2 project outside vertical surfaces
      real xtoground   ! m2 project ground connected surfaces
      real xareatran   ! m2 project windows (transparent surfaces)
      real xareawall   ! m2 project opaque walls vertical and downward facing to outside
      real xareaslproof ! m2 project sloped roofs
      real xareafltroof ! m2 project flat roofs
      real xareaskylt  ! m2 project skylights
      common/agrearea/areatran(MCOM),areawall(MCOM),areaslproof(MCOM),
     &  areafltroof(MCOM),areaskylt(MCOM),areaground(MCOM),
     &  exposed(MCOM),vexposed(MCOM),xfloor,xexposed,xvexposed,
     &  xtoground,xareatran,xareawall,xareaslproof,xareafltroof,
     &  xareaskylt

C Facade and foundation thickness and perimeter (foundation) length.
      real xvthk        ! max thickness of facade (vertical walls)
      real xfndthkg     ! max thickness of MLC connected to ground
      real xperimlength ! length of edges at wall - foundation joins (perimeter)
      common/overallthick/xvthk,xfndthkg,xperimlength
