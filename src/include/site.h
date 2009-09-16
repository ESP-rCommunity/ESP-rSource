C This header relates to site-related model entities in ESP-r. It is
C dependant on building.h and should follow building.h so that
C parameters will have been defined.

      integer IXPOS  ! site exposure index :
                     ! 1= city centre, normal case; 2= urban site, normal case
                     ! 3= rural site, normal case;
                     ! 4= city centre, equal sky, ground & building view factors
                     ! 5= city centre, below mean height of surrounding buildings
                     ! 6= rural site, isolated; 7= totally enclosed building
                     ! 8= user defined
      real GREF      ! current ground reflectivity (for the site if no snow data or
                     ! from GREF12 or actual hourly snow covered ground reflectivity)
      integer ITGREF ! flag for ground reflectivity model used
                     ! 1= constant albedo
                     ! 2= simple model (monthly albedo, number of days with snow on ground
                     ! 3= advanced model (monthly albedo, snow depth read from file)
      real GREF12    ! no snow ground reflectivity for each month
      real SREF      ! uncorrected snow covered ground reflectivity
      integer NSNOW  ! monthly number of days with snow on the ground
      character*72 SNFNAM ! file containing hourly snow depth information
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
