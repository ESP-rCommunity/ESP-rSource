*Geometry 1.1,GEN,ceiling_2 # tag version, format, zone name
*date Mon Apr  2 09:26:11 2012  # latest file modification 
ceiling_2 describes the ceiling over level 2
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,25.00000,8.00000,7.00000  #   1
*vertex,40.00000,8.00000,7.00000  #   2
*vertex,40.00000,26.00000,7.00000  #   3
*vertex,25.00000,26.00000,7.00000  #   4
*vertex,29.00000,14.00000,7.00000  #   5
*vertex,25.00000,14.00000,7.00000  #   6
*vertex,25.00000,8.00000,8.05000  #   7
*vertex,40.00000,8.00000,8.05000  #   8
*vertex,40.00000,26.00000,8.05000  #   9
*vertex,25.00000,26.00000,8.05000  #  10
*vertex,29.00000,14.00000,8.05000  #  11
*vertex,25.00000,14.00000,8.05000  #  12
*vertex,10.00000,8.00000,7.00000  #  13
*vertex,10.00000,8.00000,8.05000  #  14
*vertex,10.00000,26.00000,7.00000  #  15
*vertex,10.00000,26.00000,8.05000  #  16
*vertex,21.00000,20.00000,7.00000  #  17
*vertex,21.00000,20.00000,8.05000  #  18
*vertex,21.00000,14.00000,7.00000  #  19
*vertex,21.00000,14.00000,8.05000  #  20
*vertex,25.00000,20.00000,7.00000  #  21
*vertex,29.00000,20.00000,7.00000  #  22
*vertex,25.00000,20.00000,8.05000  #  23
*vertex,29.00000,20.00000,8.05000  #  24
*vertex,10.00000,16.00000,8.05000  #  25
*vertex,15.00000,21.00000,8.05000  #  26
*vertex,15.00000,13.00000,8.05000  #  27
*vertex,35.00000,13.00000,8.05000  #  28
*vertex,35.00000,21.00000,8.05000  #  29
*vertex,40.00000,16.00000,8.05000  #  30
*vertex,25.00000,13.00000,8.05000  #  31
*vertex,25.00000,21.00000,8.05000  #  32
*vertex,16.00000,20.00000,8.05000  #  33
*vertex,16.00000,14.00000,8.05000  #  34
*vertex,34.00000,14.00000,8.05000  #  35
*vertex,34.00000,20.00000,8.05000  #  36
*vertex,26.00000,8.00000,7.00000  #  37
*vertex,26.00000,8.00000,8.05000  #  38
*vertex,24.00000,8.00000,7.00000  #  39
*vertex,24.00000,8.00000,8.05000  #  40
*vertex,16.00000,19.00000,8.05000  #  41
*vertex,35.00000,14.00000,8.05000  #  42
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,37,2,8,38,7  #  1
*edges,5,2,3,9,30,8  #  2
*edges,4,3,4,10,9  #  3
*edges,4,5,6,12,11  #  4
*edges,9,1,6,5,22,21,4,3,2,37  #  5
*edges,6,13,39,1,7,40,14  #  6
*edges,5,15,13,14,25,16  #  7
*edges,4,4,15,16,10  #  8
*edges,4,19,17,18,20  #  9
*edges,4,6,19,20,12  # 10
*edges,9,6,1,39,13,15,4,21,17,19  # 11
*edges,4,21,22,24,23  # 12
*edges,4,22,5,11,24  # 13
*edges,4,17,21,23,18  # 14
*edges,5,25,14,27,26,16  # 15
*edges,6,29,42,28,8,30,9  # 16
*edges,5,7,31,27,14,40  # 17
*edges,5,7,38,8,28,31  # 18
*edges,6,10,16,26,32,29,9  # 19
*edges,11,27,31,12,20,34,41,33,18,23,32,26  # 20
*edges,11,28,42,29,32,23,24,36,35,11,12,31  # 21
*edges,5,34,20,18,33,41  # 22
*edges,4,11,35,36,24  # 23
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south_e,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,north_e,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,north_c2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,08,12  #   4 ||< north_c2:core_two
*surf,bottom_e,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,03,06  #   5 ||< top:office_e_1
*surf,south_w,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,west,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,north_w,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,east_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,08,10  #   9 ||< east_c1:core_two
*surf,north_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,08,11  #  10 ||< north_c1:core_two
*surf,bottom_w,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,04,06  #  11 ||< top:office_w_1
*surf,core_c1_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,08,07  #  12 ||< core_c1_a:core_two
*surf,core_c1_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,08,08  #  13 ||< core_c1_b:core_two
*surf,core_c1w_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,08,09  #  14 ||< core_c1w_a:core_two
*surf,bottom_wo,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,14,03  #  15 ||< bottom:office_w2w
*surf,bottom_eo,CEIL,-,-,-,inter_flr,OPAQUE,ANOTHER,13,03  #  16 ||< bottom:office_e2e
*surf,bottom_s,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,12,02  #  17 ||< bottom:office_2_sth
*surf,base_east,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,12,20  #  18 ||< base_east:office_2_sth
*surf,bottom_n,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,11,03  #  19 ||< bottom:office_2_nor
*surf,bottom_wp2,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,15,16  #  20 ||< bottom_w:passage_2
*surf,bottom_ep2,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,15,03  #  21 ||< bottom:passage_2
*surf,base_w,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,09,16  #  22 ||< base_w:core_three
*surf,core3_be,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,09,13  #  23 ||< base_e:core_three
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,5,11,   492.00 0  # zone base list
