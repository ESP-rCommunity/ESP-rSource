C This header relates to Radiance model entities and attributes in ESP-r. It is
C dependant on building.h and should follow building.h so that
C parameters will have been defined. Based on an initial idea by Achim Geissler.

C Radiance based file for picture processing:
      character rambfil*72  ! Radiance abient file name used if bounces > 0
      character optnfil*72  ! Radiance options file name for use with rpict and rview
                            ! to shorten command lines - root.opt
      common/e2rf/rambfil,optnfil

C Radiance model descriptive files:
      character rofil*72    ! outside facing objects file - root-out.rad
      character rzfil*72    ! zone (inside) composition - root-in.rad
      character rskyfil*72  ! the sky definition file - root.sky
      character octfil*72   ! radiance octree file - root.oct
      character picfil*72   ! picture file to be produced
      character rmfil*72    ! miscel geometries - root-misc.rad
      character glzfil*72   ! transparent materials definitions - root-glz.rad
      common/rad1/rofil,rzfil,rskyfil,octfil,picfil,rmfil,glzfil

      character aglzfil*72  ! transparent ? definitions - root-glz_a.rad
      common/rad1a/aglzfil

      character matfil*72   ! opaque materials definitions
      character rmmfil*72   ! miscel materials descriptions
      common/rad1m/matfil,rmmfil

C File unit numbers for radiance model files
      integer irofil,irzfil,imatfil,iglzfil
      common/radif/irofil,irzfil,imatfil,iglzfil
