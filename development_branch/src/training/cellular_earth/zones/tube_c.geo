*Geometry 1.1,GEN,tube_c # tag version, format, zone name
*date Thu Aug 23 17:43:01 2007  # latest file modification 
tube_c is thye c section of the earth tube
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,-4.00000,-1.00000,-1.00000  #   1
*vertex,-3.50000,-1.00000,-1.00000  #   2
*vertex,-3.50000,-0.50000,-1.00000  #   3
*vertex,7.50000,-0.50000,-1.00000  #   4
*vertex,7.50000,1.50000,-1.00000  #   5
*vertex,3.50000,1.50000,-1.00000  #   6
*vertex,3.50000,1.00000,-1.00000  #   7
*vertex,7.00000,1.00000,-1.00000  #   8
*vertex,7.00000,0.00000,-1.00000  #   9
*vertex,-4.00000,0.00000,-1.00000  #  10
*vertex,-4.00000,-1.00000,-0.80000  #  11
*vertex,-3.50000,-1.00000,-0.80000  #  12
*vertex,-3.50000,-0.50000,-0.80000  #  13
*vertex,7.50000,-0.50000,-0.80000  #  14
*vertex,7.50000,1.50000,-0.80000  #  15
*vertex,3.50000,1.50000,-0.80000  #  16
*vertex,3.50000,1.00000,-0.80000  #  17
*vertex,7.00000,1.00000,-0.80000  #  18
*vertex,7.00000,0.00000,-0.80000  #  19
*vertex,-4.00000,0.00000,-0.80000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,12,11  #  1
*edges,4,2,3,13,12  #  2
*edges,4,3,4,14,13  #  3
*edges,4,4,5,15,14  #  4
*edges,4,5,6,16,15  #  5
*edges,4,6,7,17,16  #  6
*edges,4,7,8,18,17  #  7
*edges,4,8,9,19,18  #  8
*edges,4,9,10,20,19  #  9
*edges,4,10,1,11,20  # 10
*edges,10,11,12,13,14,15,16,17,18,19,20  # 11
*edges,10,1,10,9,8,7,6,5,4,3,2  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,inlet,VERT,-,-,-,insul_frame,OPAQUE,ANOTHER,05,07  #   1 ||< side_f:tube_b
*surf,side_a,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   2 ||< user def grnd profile  1
*surf,side_b,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   3 ||< user def grnd profile  1
*surf,side_c,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   4 ||< user def grnd profile  1
*surf,side_d,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   5 ||< user def grnd profile  1
*surf,outlet,VERT,-,-,-,insul_frame,OPAQUE,GROUND,00,01  #   6 ||< user def grnd profile  1
*surf,side_e,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   7 ||< user def grnd profile  1
*surf,side_f,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   8 ||< user def grnd profile  1
*surf,side_g,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   9 ||< user def grnd profile  1
*surf,side_h,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #  10 ||< user def grnd profile  1
*surf,top,CEIL,-,-,-,earth_top,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,base,FLOR,-,-,-,below,OPAQUE,GROUND,00,01  #  12 ||< user def grnd profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,12,     8.50 0  # zone base list
