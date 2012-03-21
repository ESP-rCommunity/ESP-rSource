*Geometry 1.1,GEN,crawl # tag version, format, zone name
*date Wed Mar 21 11:00:40 2012  # latest file modification 
crawl describes crawl space under 4-in-block
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.50000,1.90000,0.20000  #   1
*vertex,0.50000,5.70000,0.20000  #   2
*vertex,1.50000,5.70000,0.20000  #   3
*vertex,2.60000,5.70000,0.20000  #   4
*vertex,2.60000,4.40000,0.20000  #   5
*vertex,2.60000,3.50000,0.20000  #   6
*vertex,2.60000,2.70000,0.20000  #   7
*vertex,2.60000,1.90000,0.20000  #   8
*vertex,2.50000,1.90000,0.20000  #   9
*vertex,2.40000,1.90000,0.20000  #  10
*vertex,1.60000,1.90000,0.20000  #  11
*vertex,1.50000,1.90000,0.20000  #  12
*vertex,1.40000,1.90000,0.20000  #  13
*vertex,0.60000,1.90000,0.20000  #  14
*vertex,2.80000,0.30000,0.20000  #  15
*vertex,2.80000,1.90000,0.20000  #  16
*vertex,2.80000,2.70000,0.20000  #  17
*vertex,2.80000,2.60000,0.20000  #  18
*vertex,3.50000,2.60000,0.20000  #  19
*vertex,4.30000,2.60000,0.20000  #  20
*vertex,4.50000,2.60000,0.20000  #  21
*vertex,4.50000,0.30000,0.20000  #  22
*vertex,4.70000,2.60000,0.20000  #  23
*vertex,5.50000,2.60000,0.20000  #  24
*vertex,6.10000,2.60000,0.20000  #  25
*vertex,6.50000,2.60000,0.20000  #  26
*vertex,6.50000,0.30000,0.20000  #  27
*vertex,5.40000,0.30000,0.20000  #  28
*vertex,4.70000,0.30000,0.20000  #  29
*vertex,7.40000,2.60000,0.20000  #  30
*vertex,8.60000,2.60000,0.20000  #  31
*vertex,9.60000,2.60000,0.20000  #  32
*vertex,9.60000,0.30000,0.20000  #  33
*vertex,8.40000,0.30000,0.20000  #  34
*vertex,7.80000,0.30000,0.20000  #  35
*vertex,4.10000,3.50000,0.20000  #  36
*vertex,4.10000,4.40000,0.20000  #  37
*vertex,4.60000,4.40000,0.20000  #  38
*vertex,5.50000,4.40000,0.20000  #  39
*vertex,6.10000,4.40000,0.20000  #  40
*vertex,2.80000,5.70000,0.20000  #  41
*vertex,2.80000,7.30000,0.20000  #  42
*vertex,4.70000,7.30000,0.20000  #  43
*vertex,5.40000,7.30000,0.20000  #  44
*vertex,6.10000,7.30000,0.20000  #  45
*vertex,7.00000,7.30000,0.20000  #  46
*vertex,8.60000,7.30000,0.20000  #  47
*vertex,9.60000,7.30000,0.20000  #  48
*vertex,0.50000,1.90000,-0.20000  #  49
*vertex,2.80000,1.90000,-0.20000  #  50
*vertex,2.80000,0.30000,-0.20000  #  51
*vertex,9.60000,0.30000,-0.20000  #  52
*vertex,9.60000,7.30000,-0.20000  #  53
*vertex,2.80000,7.30000,-0.20000  #  54
*vertex,2.80000,5.70000,-0.20000  #  55
*vertex,0.50000,5.70000,-0.20000  #  56
# 
# tag, number of vertices followed by list of associated vert
*edges,14,2,1,14,13,12,11,10,9,8,7,6,5,4,3  #  1
*edges,10,16,15,22,21,20,19,18,17,7,8  #  2
*edges,9,21,22,29,28,27,26,25,24,23  #  3
*edges,8,26,27,35,34,33,32,31,30  #  4
*edges,15,23,24,25,40,39,38,37,36,6,7,17,18,19,20,21  #  5
*edges,13,38,39,40,45,44,43,42,41,4,5,6,36,37  #  6
*edges,10,30,31,32,48,47,46,45,40,25,26  #  7
*edges,10,51,52,33,34,35,27,28,29,22,15  #  8
*edges,5,52,53,48,32,33  #  9
*edges,9,53,54,42,43,44,45,46,47,48  # 10
*edges,4,54,55,41,42  # 11
*edges,6,55,56,2,3,4,41  # 12
*edges,4,56,49,1,2  # 13
*edges,11,49,50,16,8,9,10,11,12,13,14,1  # 14
*edges,4,50,51,15,16  # 15
*edges,8,52,51,50,49,56,55,54,53  # 16
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,floor,CEIL,-,-,-,Ceilti_crl,OPAQUE,ANOTHER,01,09  #   1 ||< ceiling:stairs
*surf,Base-12,CEIL,-,-,-,Ceilti_crl,OPAQUE,ANOTHER,02,12  #   2 ||< store2_floor:store2
*surf,bath_floor,CEIL,-,-,-,Ceilti_crl,OPAQUE,ANOTHER,03,11  #   3 ||< bath_floor:bathroom
*surf,kitch_floor,CEIL,-,-,-,Ceilti_crl,OPAQUE,ANOTHER,04,10  #   4 ||< kitch_floor:low_kitchen
*surf,hall_floor,CEIL,-,-,-,Ceilti_crlc,OPAQUE,ANOTHER,05,17  #   5 ||< hall_floor:low_hall
*surf,br_floor,CEIL,-,-,-,Ceilti_crlc,OPAQUE,ANOTHER,06,15  #   6 ||< br_floor:low_bedroom
*surf,liv_floor,CEIL,-,-,-,Ceilti_crlc,OPAQUE,ANOTHER,07,12  #   7 ||< liv_floor:lowliving
*surf,front_a,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,right,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,back,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,left_a,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,back_b,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,left_b,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,front_b,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,left_c,VERT,-,WALL,-,Crawl_wall,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,ground,FLOR,-,FLOOR,EXTGRND,ground,OPAQUE,GROUND,01,0  #  16 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,56.34,0  # zone base
