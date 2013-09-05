C tdf2.h Parameters for TDF program, May 2010 version.

      INTEGER MIT,MTABC,MHEL,MBITS,MGAT,MGAR,MGAI,MALT,MHTS

C Allowed number of tabular entries in the TDF file. For large
C models there may be one item per zone so this should reflect
C the number of zones in building.h
      PARAMETER (MIT=82)

C Allowed columns of tabular data in TAB file.
      PARAMETER (MTABC=80)

C Maximum number of help lines associated with a generic item.
      PARAMETER (MHEL=5)

C Maximum number of non-static bits in a generic item period data set.
      PARAMETER (MBITS=18)

C Maximum number of text fields.
      PARAMETER (MGAT=12)

C Maximum number of real fields.
      PARAMETER (MGAR=18)

C Maximum number of integer fields.
      PARAMETER (MGAI=12)

C Maximum number of text alternatives in a text field.
      PARAMETER (MALT=6)

C Maximum number of timesteps in an hour.
      PARAMETER (MHTS=60)
      
C Related to TDF header.
      integer NWPR   ! Number of words per record in the TAB file.
      integer NITDF  ! Total number of instances in the TDF db.
      integer NTSPH  ! Number of time steps per hour, 1 to 60 allowed.
      integer NEXTCL ! Next column available in tabular file.
      integer NEXTRC ! Next record available in << >>
      integer LASTHD ! Last record used by the TDF header section.
      integer LSTREC ! The last record used by the TDF scratch file
      integer NDBSTP ! Total number of timesteps during the db tabular period.
      integer NUWPR  ! Current number of data columns used (not counting time column)
                     ! i.e. this is the sum of ntbits() for current contents.
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR

      integer itdyear  ! the year assumed in the temporal file
      integer itdbdoy  ! beginning julian day of temporal file
      integer itdedoy  ! ending julian day of temporal file
      common/tdset/itdyear,itdbdoy,itdedoy

      integer ITEMSTD  ! number of temporal items associated with model
      COMMON/TDFFLG/ITEMSTD

C Related to each temporal item instance.
      character TAG*12   ! unique itentity for the item, outside code may reference.
      character TTYPE*8  ! generic type tag for type of processing and information.
      character TMENU*32 ! descriptive text for use in selection menus.
      character TAIDE*36 ! Aide memoire, especially useful with similar items.
      COMMON/TDFIH1/TAG(MIT),TTYPE(MIT),TMENU(MIT),TAIDE(MIT)

C Layout of a temporal item.
      integer NTSTAT     ! number of static data fields (not time-based)
      integer NTBITS     ! number of static and time-based data fields
      integer NTMAR      ! number of real data fields
      integer NTMAT      ! number of text data fields
      integer NTMAI      ! number of integer data fields
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)

C For each data field.
      integer itdftr     ! if 1 then data slot is real, 2 is text 3 is integer
      integer iatdf      ! position of data within the item
      integer itcol      ! column position for each data in the scratch file
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)

      character tdfdes*32  ! Description of temporal item data field
      COMMON/TDFLD1/TDFDES(MIT,MBITS)
      
C TDFTEX is the chosen alternative text parameter for text fields.
      character TMAT*16  ! temporal item data type text attributes - zone, sensor, control names.
      COMMON/TDFTEX/TMAT(MIT,MGAT)

C TDFREL are ranges allowed for real fields in the instance.
      real TMAR      ! real attributes to be passed to outside code.
      real TMIN,TMAX ! acceptable minimum and maximum for range checking.
      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)

C TDFINIT are range allowed for integer fields
      integer itmar  ! integer attributes to be passed to code
      integer itmin,itmax ! acceptable minimum and maximum for range checking
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)

C For each kind of temporal database item a zero indicates the item is
C not used, a non-zero value points to the index of the item in the
C temporal file. Some variables are an array(mcom) form in which case
C a zero indicates that that particular zone is not associated and
C a non-zero indicates the index of the item. For example icasual(3)=6
C says that the 3rd zone in the model uses a casual gains item which
C is the 6th item in the temporal file.
      integer IALLCLM   ! if non-zero a item holding all climate data used
      integer ICASUAL   ! if non-zero lumped casual gains are used 
      integer IZIVENT   ! if non-zero ventilation & infiltration used
      integer IRAIRVL   ! if non-zero room air velocity (not yet used)
      integer ISETPTT   ! if non-zero single set point temperature used
      integer ICTLSTA   ! if non-zero control state (not yet used)
      integer ISKYLUX   ! if non-zero measured sky lux (not yet used)
      integer IDBTEXT   ! if non-zero ambient dry bulb (climate) used
      integer IWINDVL   ! if non-zero ambient wind velocity (climate) used
      integer IWINDDR   ! if non-zero ambient wind direction (climate) used
      integer IRELHUM   ! if non-zero ambient relative humidity used
      integer IDIFHSL   ! if non-zero ambient diffuse solar used
      integer IDIRSOL   ! if non-zero ambient direct solar used
      integer IGLOHSL   ! if non-zero ambient global horiz solar used
      integer IOBJVEL   ! if non-zero model has velocity ( )
      integer IOBJDIR   ! if non-zero model has direction ( )
      integer ISKYTMP   ! if non-zero measured sky temperature (not yet used)
      integer IGLOVRT   ! if non-zero vertical global solar is used
      integer ICASUAL3  ! if non-zero separate casual gains are used
      integer IELECPWR  ! if non-zero real & reactive power available (one per zn)
      integer IACTIV    ! if non-zero a zone activity pattern is used
      integer IBIDIRS   ! if non-zero bidirectional data available
      integer IVERSOL   ! if non-zero surface vertical global solar used
      integer IHTCLSETP ! if non-zero heating and cooling setpoints used
      integer IDBTZNM   ! if non-zero measured room db T available for viewing
      integer ISURTMEAS ! if non-zero measured surface T available for viewing
      integer IMFNTDFP  ! if non-zero measured fluid mass flow node node pressure
      integer IMFNTDFF  ! flag set to 1 if measured pressure data is present
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IELECPWR(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS),IVERSOL(MCOM,MS),IHTCLSETP(MCOM),
     &       IDBTZNM(MCOM),ISURTMEAS(MCOM,MS),IMFNTDFP(MNOD),IMFNTDFF
