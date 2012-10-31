*Geometry 1.1,GEN,crawl # tag version, format, zone name
*date Mon Mar 19 09:56:13 2012  # latest file modification 
crawl describes the crawl space below dwelling
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.50000,0.20000,0.20000  #   1
*vertex,4.50000,2.40000,0.20000  #   2
*vertex,4.70000,2.40000,0.20000  #   3
*vertex,5.50000,2.40000,0.20000  #   4
*vertex,6.50000,2.40000,0.20000  #   5
*vertex,6.50000,1.80000,0.20000  #   6
*vertex,6.50000,1.20000,0.20000  #   7
*vertex,6.50000,0.20000,0.20000  #   8
*vertex,4.50000,3.20000,0.20000  #   9
*vertex,4.50000,3.40000,0.20000  #  10
*vertex,4.50000,4.20000,0.20000  #  11
*vertex,4.50000,4.90000,0.20000  #  12
*vertex,5.50000,4.90000,0.20000  #  13
*vertex,6.50000,4.90000,0.20000  #  14
*vertex,6.50000,4.40000,0.20000  #  15
*vertex,6.50000,3.60000,0.20000  #  16
*vertex,4.50000,7.00000,0.20000  #  17
*vertex,4.50000,6.10000,0.20000  #  18
*vertex,3.80000,0.20000,0.20000  #  19
*vertex,3.10000,0.20000,0.20000  #  20
*vertex,2.60000,0.20000,0.20000  #  21
*vertex,2.00000,0.20000,0.20000  #  22
*vertex,1.30000,0.20000,0.20000  #  23
*vertex,0.10000,0.20000,0.20000  #  24
*vertex,0.10000,2.80000,0.20000  #  25
*vertex,0.10000,5.00000,0.20000  #  26
*vertex,0.10000,7.00000,0.20000  #  27
*vertex,1.20000,7.00000,0.20000  #  28
*vertex,2.80000,7.00000,0.20000  #  29
*vertex,6.50000,7.00000,0.20000  #  30
*vertex,6.50000,6.10000,0.20000  #  31
*vertex,0.10000,0.20000,-0.20000  #  32
*vertex,6.50000,0.20000,-0.20000  #  33
*vertex,6.50000,7.00000,-0.80000  #  34
*vertex,0.10000,7.00000,-0.80000  #  35
# 
# tag, number of vertices followed by list of associated vert
*edges,8,2,1,8,7,6,5,4,3  #  1
*edges,12,9,2,3,4,5,16,15,14,13,12,11,10  #  2
*edges,19,18,17,29,28,27,26,25,24,23,22,21,20,19,1,2,9,10,11,12  #  3
*edges,7,18,12,13,14,31,30,17  #  4
*edges,10,32,33,8,1,19,20,21,22,23,24  #  5
*edges,11,33,34,30,31,14,15,16,5,6,7,8  #  6
*edges,7,34,35,27,28,29,17,30  #  7
*edges,6,35,32,24,25,26,27  #  8
*edges,4,33,32,35,34  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,lbath_floor,CEIL,-,-,-,Ceilti_crl,OPAQUE,ANOTHER,01,10  #   1 ||< lbath_floor:lower_bath
*surf,entry_floor,CEIL,-,-,-,Ceilti_crlc,OPAQUE,ANOTHER,02,13  #   2 ||< entry_floor:entry
*surf,liv_floor,CEIL,-,-,-,Ceilti_crlc,OPAQUE,ANOTHER,03,19  #   3 ||< liv_floor:living
*surf,Base-9,CEIL,-,-,-,Ceilti_crl,OPAQUE,ANOTHER,09,07  #   4 ||< Base-9:below_str
*surf,front,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,right,VERT,-,-,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,back,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,left,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,ground,SLOP,-,-,-,ground,OPAQUE,GROUND,01,0  #   9 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,0.00,0  # zone base
