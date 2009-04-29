C This header relates to climate season entities in ESP-r. It is
C not dependant on other header files and include statements to
C seasons.h can are not sensitive to placement.

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
