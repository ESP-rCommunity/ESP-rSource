*Geometry 1.1,GEN,rad_600 # tag version, format, zone name
*date Tue Aug 10 11:41:24 2010  # latest file modification 
rad_600 describes 600mm radiant panel
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.60000,0.00000,2.70000  #   1
*vertex,7.20000,0.00000,2.70000  #   2
*vertex,7.20000,0.60000,2.70000  #   3
*vertex,3.60000,0.60000,2.70000  #   4
*vertex,3.60000,0.40000,2.70000  #   5
*vertex,3.60000,0.00000,2.75000  #   6
*vertex,7.20000,0.00000,2.75000  #   7
*vertex,7.20000,0.60000,2.75000  #   8
*vertex,3.60000,0.60000,2.75000  #   9
*vertex,3.60000,0.40000,2.75000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,7,6  #  1
*edges,4,2,3,8,7  #  2
*edges,4,3,4,9,8  #  3
*edges,4,4,5,10,9  #  4
*edges,4,5,1,6,10  #  5
*edges,5,6,7,8,9,10  #  6
*edges,5,1,5,4,3,2  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,front_edge,VERT,-,-,-,alum_insu_i,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,right_edge,VERT,-,-,-,alum_insu_i,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,back_edge,VERT,-,-,-,alum_insu_i,OPAQUE,ANOTHER,02,11  #   3 ||< back_edge:plenum
*surf,left_edge,VERT,-,-,-,alum_insu_i,OPAQUE,ANOTHER,02,12  #   4 ||< left_edge:plenum
*surf,join_400,VERT,-,-,-,alum_insu,OPAQUE,ANOTHER,05,02  #   5 ||< right_edge:rad_400
*surf,upper,CEIL,-,-,-,alum_insu_i,OPAQUE,ANOTHER,02,13  #   6 ||< upper:plenum
*surf,base,FLOR,-,-,-,alum_panel,OPAQUE,ANOTHER,04,11  #   7 ||< rad_600:one_bed_600
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,7,     2.16 0  # zone base list
