*Geometry 1.1,GEN,crawl # tag version, format, zone name
*date Wed Apr 18 18:09:06 2012  # latest file modification 
crawl describes crawl space under bungalow
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,2.00000,0.20000  #   1
*vertex,0.90000,2.00000,0.20000  #   2
*vertex,2.00000,2.00000,0.20000  #   3
*vertex,2.90000,2.00000,0.20000  #   4
*vertex,3.10000,2.00000,0.20000  #   5
*vertex,3.10000,0.00000,0.20000  #   6
*vertex,2.90000,0.00000,0.20000  #   7
*vertex,2.10000,0.00000,0.20000  #   8
*vertex,0.00000,0.00000,0.20000  #   9
*vertex,2.00000,2.70000,0.20000  #  10
*vertex,2.00000,3.70000,0.20000  #  11
*vertex,2.00000,3.80000,0.20000  #  12
*vertex,2.40000,3.80000,0.20000  #  13
*vertex,3.20000,3.80000,0.20000  #  14
*vertex,3.40000,3.80000,0.20000  #  15
*vertex,4.10000,3.80000,0.20000  #  16
*vertex,4.10000,3.60000,0.20000  #  17
*vertex,4.10000,2.30000,0.20000  #  18
*vertex,4.10000,2.00000,0.20000  #  19
*vertex,0.00000,2.70000,0.20000  #  20
*vertex,0.00000,2.90000,0.20000  #  21
*vertex,0.00000,3.70000,0.20000  #  22
*vertex,0.00000,3.80000,0.20000  #  23
*vertex,0.00000,4.70000,0.20000  #  24
*vertex,0.00000,6.60000,0.20000  #  25
*vertex,2.50000,6.60000,0.20000  #  26
*vertex,3.20000,6.60000,0.20000  #  27
*vertex,3.40000,6.60000,0.20000  #  28
*vertex,3.40000,5.10000,0.20000  #  29
*vertex,4.10000,6.60000,0.20000  #  30
*vertex,4.10000,5.10000,0.20000  #  31
*vertex,5.40000,6.60000,0.20000  #  32
*vertex,6.80000,6.60000,0.20000  #  33
*vertex,8.10000,6.60000,0.20000  #  34
*vertex,8.10000,3.70000,0.20000  #  35
*vertex,8.10000,2.00000,0.20000  #  36
*vertex,7.10000,2.00000,0.20000  #  37
*vertex,6.00000,2.00000,0.20000  #  38
*vertex,5.10000,2.00000,0.20000  #  39
*vertex,5.10000,0.00000,0.20000  #  40
*vertex,4.00000,0.00000,0.20000  #  41
*vertex,3.30000,0.00000,0.20000  #  42
*vertex,8.10000,0.00000,0.20000  #  43
*vertex,7.00000,0.00000,0.20000  #  44
*vertex,6.20000,0.00000,0.20000  #  45
*vertex,0.00000,0.00000,-0.20000  #  46
*vertex,8.10000,0.00000,-0.20000  #  47
*vertex,8.10000,6.60000,-0.50000  #  48
*vertex,0.00000,6.60000,-0.50000  #  49
# 
# tag, number of vertices followed by list of associated vert
*edges,9,2,1,9,8,7,6,5,4,3  #  1
*edges,13,10,3,4,5,19,18,17,16,15,14,13,12,11  #  2
*edges,10,1,2,3,10,11,12,23,22,21,20  #  3
*edges,11,24,23,12,13,14,15,29,28,27,26,25  #  4
*edges,6,29,15,16,31,30,28  #  5
*edges,14,18,19,39,38,37,36,35,34,33,32,30,31,16,17  #  6
*edges,7,5,6,42,41,40,39,19  #  7
*edges,8,39,40,45,44,43,36,37,38  #  8
*edges,10,49,46,9,1,20,21,22,23,24,25  #  9
*edges,12,46,47,43,44,45,40,41,42,6,7,8,9  # 10
*edges,6,47,48,34,35,36,43  # 11
*edges,10,48,49,25,26,27,28,30,32,33,34  # 12
*edges,4,47,46,49,48  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,br2_floor,CEIL,-,-,-,Ceilti_crlc,OPAQUE,ANOTHER,01,11  #   1 ||< br2_floor:BR_two
*surf,hall_floor,CEIL,-,-,-,Ceilti_crlc,OPAQUE,ANOTHER,02,15  #   2 ||< hall_floor:hall
*surf,vest_floor,CEIL,-,-,-,Ceilti_crl,OPAQUE,ANOTHER,03,12  #   3 ||< vest_floor:lobby_stor
*surf,br1_floor,CEIL,-,-,-,Ceilti_crlc,OPAQUE,ANOTHER,04,13  #   4 ||< br1_floor:BR_one
*surf,stor_floor,CEIL,-,-,-,Ceilti_crl,OPAQUE,ANOTHER,05,08  #   5 ||< stor_floor:storage
*surf,liv_floor,CEIL,-,-,-,Ceilti_crlc,OPAQUE,ANOTHER,06,16  #   6 ||< liv_floor:livingdin
*surf,bath_floor,CEIL,-,-,-,Ceilti_crl,OPAQUE,ANOTHER,07,09  #   7 ||< bath_floor:bath
*surf,kit_floor,CEIL,-,-,-,Ceilti_crl,OPAQUE,ANOTHER,08,10  #   8 ||< kit_floor:kitchen
*surf,left,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,front,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,right,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,back,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,ground,SLOP,-,FLOOR,EXTGRND,ground,OPAQUE,GROUND,01,0  #  13 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,0.00,0  # zone base
