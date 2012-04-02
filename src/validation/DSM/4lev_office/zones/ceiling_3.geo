*Geometry 1.1,GEN,ceiling_3 # tag version, format, zone name
*date Mon Apr  2 09:26:37 2012  # latest file modification 
ceiling_3 is the ceiling of the 3rd level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,25.00000,8.00000,10.85000  #   1
*vertex,40.00000,8.00000,10.85000  #   2
*vertex,40.00000,26.00000,10.85000  #   3
*vertex,25.00000,26.00000,10.85000  #   4
*vertex,25.00000,14.00000,10.85000  #   5
*vertex,25.00000,8.00000,11.90000  #   6
*vertex,40.00000,8.00000,11.90000  #   7
*vertex,40.00000,26.00000,11.90000  #   8
*vertex,25.00000,26.00000,11.90000  #   9
*vertex,29.00000,14.00000,11.90000  #  10
*vertex,25.00000,14.00000,11.90000  #  11
*vertex,10.00000,8.00000,10.85000  #  12
*vertex,10.00000,8.00000,11.90000  #  13
*vertex,10.00000,26.00000,10.85000  #  14
*vertex,10.00000,26.00000,11.90000  #  15
*vertex,21.00000,20.00000,11.90000  #  16
*vertex,21.00000,14.00000,11.90000  #  17
*vertex,25.00000,20.00000,10.85000  #  18
*vertex,25.00000,20.00000,11.90000  #  19
*vertex,29.00000,20.00000,11.90000  #  20
*vertex,34.00000,14.00000,11.90000  #  21
*vertex,34.00000,20.00000,11.90000  #  22
*vertex,16.00000,14.00000,11.90000  #  23
*vertex,16.00000,20.00000,11.90000  #  24
*vertex,16.00000,14.00000,10.85000  #  25
*vertex,34.00000,14.00000,10.85000  #  26
*vertex,16.00000,20.00000,10.85000  #  27
*vertex,34.00000,20.00000,10.85000  #  28
*vertex,15.00000,13.00000,10.85000  #  29
*vertex,15.00000,21.00000,10.85000  #  30
*vertex,10.00000,16.00000,10.85000  #  31
*vertex,40.00000,16.00000,10.85000  #  32
*vertex,35.00000,21.00000,10.85000  #  33
*vertex,35.00000,13.00000,10.85000  #  34
*vertex,25.00000,13.00000,10.85000  #  35
*vertex,25.00000,21.00000,10.85000  #  36
*vertex,27.00000,8.00000,10.85000  #  37
*vertex,27.00000,8.00000,11.90000  #  38
*vertex,34.00000,16.00000,10.85000  #  39
*vertex,16.00000,16.00000,10.85000  #  40
*vertex,23.00000,8.00000,11.90000  #  41
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,37,2,7,38,6  #  1
*edges,5,2,32,3,8,7  #  2
*edges,4,3,4,9,8  #  3
*edges,11,6,38,7,8,9,19,20,22,21,10,11  #  4
*edges,5,12,1,6,41,13  #  5
*edges,5,14,31,12,13,15  #  6
*edges,4,4,14,15,9  #  7
*edges,5,25,40,27,24,23  #  8
*edges,11,6,11,17,23,24,16,19,9,15,13,41  #  9
*edges,5,28,39,26,21,22  # 10
*edges,5,5,25,23,17,11  # 11
*edges,5,26,5,11,10,21  # 12
*edges,5,27,18,19,16,24  # 13
*edges,5,18,28,22,20,19  # 14
*edges,5,29,12,31,14,30  # 15
*edges,5,32,2,34,33,3  # 16
*edges,5,2,37,1,35,34  # 17
*edges,4,1,12,29,35  # 18
*edges,6,36,30,14,4,3,33  # 19
*edges,9,36,33,34,35,5,26,39,28,18  # 20
*edges,9,35,29,30,36,18,27,40,25,5  # 21
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south_e,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,north_e,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,top_e,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,05,07  #   4 ||< bottom:office_e_3
*surf,south_w,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,west,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,north_w,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,east_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,10  #   8 ||< east_c1:core_three
*surf,top_w,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,06,07  #   9 ||< bottom:office_w_3
*surf,core_c1_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,08  #  10 ||< core_c1_b:core_three
*surf,north_c1p,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,11  #  11 ||< north_c1:core_three
*surf,north_c2p,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,12  #  12 ||< north_c2:core_three
*surf,core_c1w_ap,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,09  #  13 ||< core_c1w_a:core_three
*surf,core_c1_ap,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,07  #  14 ||< core_c1_a:core_three
*surf,top_w2w,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,14,02  #  15 ||< top:office_w2w
*surf,top_e2e,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,13,02  #  16 ||< top:office_e2e
*surf,top_se,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,12,19  #  17 ||< top_e:office_2_sth
*surf,top_sw,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,12,01  #  18 ||< top:office_2_sth
*surf,top_north,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,11,02  #  19 ||< top:office_2_nor
*surf,top_epag,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,15,02  #  20 ||< top:passage_2
*surf,top_wpag,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,15,15  #  21 ||< top_w:passage_2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,6,15,16,17,18,19,20,   406.00 0  # zone base list
