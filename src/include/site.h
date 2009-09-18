C This header relates to site-related model entities in ESP-r. It is
C dependant on building.h and should follow building.h so that
C parameters will have been defined.

C Site position.
      real  sitelat  ! site latitude (degrees) 
      real  sitelongdif  ! longitudinal difference between the site and the
                  ! reference time meridian (east +ve)
      COMMON/C4/sitelat,sitelongdif

C Exposure of the building on the site as well as ground and snow reflection data.
      integer siteexposureindex  ! site exposure index :
                     ! 1= city centre, normal case; 2= urban site, normal case
                     ! 3= rural site, normal case;
                     ! 4= city centre, equal sky, ground & building view factors
                     ! 5= city centre, below mean height of surrounding buildings
                     ! 6= rural site, isolated; 7= totally enclosed building
                     ! 8= user defined
      real groundrefl ! current ground reflectivity (for the site if no snow data or
                     ! from GREF12 or actual hourly snow covered ground reflectivity)
      integer groundreflmodel ! flag for ground reflectivity model used
                     ! 1= constant albedo
                     ! 2= simple model (monthly albedo, number of days with snow on ground
                     ! 3= advanced model (monthly albedo, snow depth read from file)
      real groundreflmonth ! no snow ground reflectivity for each month
      real snowgroundrefl  ! uncorrected snow covered ground reflectivity
      integer dayswithsnow  ! monthly number of days with snow on the ground
      COMMON/C5/siteexposureindex,groundrefl,groundreflmodel,
     &  groundreflmonth(12),snowgroundrefl,dayswithsnow(12)

      character*72 SNFNAM ! file containing hourly snow depth information
      COMMON/C5SD/SNFNAM

C Site viewfactors from building surfaces to the site.
      real skyview      ! viewfactor to the sky vault (0.0 - 1.0) 
      real groundview   ! viewfactor to the ground (0.0 - 1.0)
      real buildingview ! viewfactor to buildings (0.0 - 1.0)
      COMMON/C5R/skyview,groundview,buildingview

C Monthly profiles of temperatures defined by the user and standard profiles.
      real UGRDTP    ! user defined monthly temperature profile
      integer NGRDP  ! numer of user defined monthly profiles
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP

      real grdtmp ! standard (compiled in) monthly temperature profiles
      common/ground/grdtmp(12,mgrdp)
