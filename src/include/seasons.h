C This header relates to climate season entities in ESP-r. It is
C dependant on header file building.h and include statements to
C seasons.h should follow building.h.

C Typical assessment period definitions.
      integer ia1wins,ia1winf ! early jan-feb period start and finish
      integer ia1sprs,ia1sprf ! spring period start and finish
      integer iasums,iasumf   ! summer period start and finish
      integer ia2sprs,ia2sprf ! autumn prriod start and finish
      integer ia2wins,ia2winf ! late winter nov-dec period start and finish
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf

C Season definitions. 2 periods for winter (i.e. jan-feb & nov-dec)
C transition (i.e. mar-may & sep-oct) and one period for summer.
      integer is1wins,is1winf ! early jan-feb season start and finish
      integer is2wins,is2winf ! late winter nov-dec season start and finish
      integer is1sprs,is1sprf ! spring season start and finish
      integer is2sprs,is2sprf ! autumn season start and finish
      integer is1sums,is1sumf ! summer season start and finish
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf

C When scanning climate data the average and total degree days for each
C season are held in block ddseasonavg.
      real seahddwk  ! hdd avg/week (for each season)
      real seacddwk  ! cdd avg/week (for each season)
      real seahddtot ! total hdd during each season
      real seacddtot ! total cdd during each season
      common/ddseasonavg/seahddwk(MSPS),seacddwk(MSPS),seahddtot(MSPS),
     &                   seacddtot(MSPS)


C For each of the seasons: 
      real wkdiff   ! difference in the performance parameters for the
                    ! assessment period and the season
      integer iwkbest  ! index of the best week (1-52)
      integer iwkbstrt ! the julian day start of the best week
      real wkheatdd ! heating degree days in the seasons best week
      real wkcooldd ! cooling degree days in the seasons best week.
      common/wkseasondiff/wkdiff(MIPVA),iwkbest(MIPVA),iwkbstrt(MIPVA),
     &                    wkheatdd(MIPVA),wkcooldd(MIPVA)

C When calculating best fit periods user supplied weightings and
C base temperatures.    
      real hddw   ! weighting for heating dd
      real cddw   ! weighting for cooling dd
      real radw   ! weighting for solar radiation.
      real hddbaset ! base heating temperature for dd calcs
      real cddbaset ! base cooling temperature for dd calcs
      common/seaddweighting/hddw,cddw,radw,hddbaset,cddbaset
