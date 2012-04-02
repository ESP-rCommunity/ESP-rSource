*Geometry 1.1,GEN,ceiling_1 # tag version, format, zone name
*date Mon Apr  2 09:25:44 2012  # latest file modification 
ceiling_1 ceiling over the ground level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,25.00000,8.00000,3.15000  #   1
*vertex,40.00000,8.00000,3.15000  #   2
*vertex,40.00000,26.00000,3.15000  #   3
*vertex,25.00000,26.00000,3.15000  #   4
*vertex,29.00000,14.00000,3.15000  #   5
*vertex,25.00000,14.00000,3.15000  #   6
*vertex,25.00000,8.00000,4.20000  #   7
*vertex,40.00000,8.00000,4.20000  #   8
*vertex,40.00000,26.00000,4.20000  #   9
*vertex,25.00000,26.00000,4.20000  #  10
*vertex,29.00000,14.00000,4.20000  #  11
*vertex,25.00000,14.00000,4.20000  #  12
*vertex,10.00000,8.00000,3.15000  #  13
*vertex,10.00000,8.00000,4.20000  #  14
*vertex,10.00000,26.00000,3.15000  #  15
*vertex,10.00000,26.00000,4.20000  #  16
*vertex,21.00000,20.00000,3.15000  #  17
*vertex,21.00000,20.00000,4.20000  #  18
*vertex,21.00000,14.00000,3.15000  #  19
*vertex,21.00000,14.00000,4.20000  #  20
*vertex,25.00000,20.00000,3.15000  #  21
*vertex,29.00000,20.00000,3.15000  #  22
*vertex,25.00000,20.00000,4.20000  #  23
*vertex,29.00000,20.00000,4.20000  #  24
*vertex,40.00000,16.00000,3.15000  #  25
*vertex,26.00000,26.00000,3.15000  #  26
*vertex,26.00000,20.00000,3.15000  #  27
*vertex,24.00000,20.00000,3.15000  #  28
*vertex,24.00000,26.00000,3.15000  #  29
*vertex,10.00000,16.00000,3.15000  #  30
*vertex,27.00000,8.00000,4.20000  #  31
*vertex,23.00000,8.00000,4.20000  #  32
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,2,8,31,7  #  1
*edges,5,2,25,3,9,8  #  2
*edges,5,3,26,4,10,9  #  3
*edges,4,5,6,12,11  #  4
*edges,9,7,31,8,9,10,23,24,11,12  #  5
*edges,5,13,1,7,32,14  #  6
*edges,5,15,30,13,14,16  #  7
*edges,5,4,29,15,16,10  #  8
*edges,4,19,17,18,20  #  9
*edges,4,6,19,20,12  # 10
*edges,9,7,12,20,18,23,10,16,14,32  # 11
*edges,5,21,27,22,24,23  # 12
*edges,4,22,5,11,24  # 13
*edges,5,17,28,21,23,18  # 14
*edges,9,2,1,6,5,22,27,26,3,25  # 15
*edges,9,1,13,30,15,29,28,17,19,6  # 16
*edges,6,27,21,28,29,4,26  # 17
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south_e,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,north_e,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,north_c2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,07,11  #   4 ||< north_c2:entry_core
*surf,top_e,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,03,07  #   5 ||< bottom:office_e_1
*surf,south_w,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,west,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,north_w,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,east_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,07,10  #   9 ||< east_c1:entry_core
*surf,north_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,07,09  #  10 ||< north_c1:entry_core
*surf,top_w,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,04,07  #  11 ||< bottom:office_w_1
*surf,core_c1_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,07,05  #  12 ||< core_c1_a:entry_core
*surf,core_c1_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,07,06  #  13 ||< core_c1_b:entry_core
*surf,core_c1w_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,07,07  #  14 ||< core_c1w_a:entry_core
*surf,top,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,01,06  #  15 ||< top:office_e_0
*surf,bottom_w,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,02,06  #  16 ||< top:office_w_0
*surf,over_entr,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,07,08  #  17 ||< over_entr:entry_core
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,312.00,1  # zone base
