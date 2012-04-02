*Geometry 1.1,GEN,roof # tag version, format, zone name
*date Mon Apr  2 13:01:06 2012  # latest file modification 
roof describes a low slope roof space
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,8.00000,14.70000  #   1
*vertex,40.00000,8.00000,14.70000  #   2
*vertex,40.00000,26.00000,14.70000  #   3
*vertex,10.00000,26.00000,14.70000  #   4
*vertex,12.40000,16.00000,16.00000  #   5
*vertex,37.60000,16.00000,16.00000  #   6
*vertex,25.00000,8.00000,14.70000  #   7
*vertex,9.40000,7.40000,14.70000  #   8
*vertex,9.40000,26.60000,14.70000  #   9
*vertex,40.60000,26.60000,14.70000  #  10
*vertex,40.60000,7.40000,14.70000  #  11
*vertex,40.00000,16.00000,14.70000  #  12
*vertex,29.00000,20.00000,14.70000  #  13
*vertex,29.00000,14.00000,14.70000  #  14
*vertex,25.00000,14.00000,14.70000  #  15
*vertex,21.00000,14.00000,14.70000  #  16
*vertex,21.00000,20.00000,14.70000  #  17
*vertex,25.00000,20.00000,14.70000  #  18
*vertex,25.00000,26.00000,14.70000  #  19
*vertex,10.00000,16.00000,14.70000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,8,11,6,5  #  1
*edges,3,11,10,6  #  2
*edges,4,10,9,5,6  #  3
*edges,3,9,8,5  #  4
*edges,8,11,8,9,4,20,1,7,2  #  5
*edges,8,3,19,4,9,10,11,2,12  #  6
*edges,9,2,7,15,14,13,18,19,3,12  #  7
*edges,9,7,1,20,4,19,18,17,16,15  #  8
*edges,6,14,15,16,17,18,13  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,north,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,soffet_sw,FLOR,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,soffet_ne,FLOR,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,top,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,05,06  #   7 ||< top:office_e_3
*surf,top_w,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,06,06  #   8 ||< top:office_w_3
*surf,core_top,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,10,07  #   9 ||< core_top:core_top
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,5,5,9,6,8,7,   599.04 0  # zone base list
