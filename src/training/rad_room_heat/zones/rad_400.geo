*Geometry 1.1,GEN,rad_400 # tag version, format, zone name
*date Tue Aug 10 11:41:24 2010  # latest file modification 
rad_400 describes the 400mm radiant panel
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,2.70000  #   1
*vertex,3.60000,0.00000,2.70000  #   2
*vertex,3.60000,0.40000,2.70000  #   3
*vertex,0.40000,0.40000,2.70000  #   4
*vertex,0.40000,4.30000,2.70000  #   5
*vertex,0.00000,4.30000,2.70000  #   6
*vertex,0.00000,0.00000,2.75000  #   7
*vertex,3.60000,0.00000,2.75000  #   8
*vertex,3.60000,0.40000,2.75000  #   9
*vertex,0.40000,0.40000,2.75000  #  10
*vertex,0.40000,4.30000,2.75000  #  11
*vertex,0.00000,4.30000,2.75000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,1,6,5,4,3,2  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,front_edge,VERT,-,-,-,alum_insu_i,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,right_edge,VERT,-,-,-,alum_insu_i,OPAQUE,ANOTHER,06,05  #   2 ||< join_400:rad_600
*surf,back_a_edge,VERT,-,-,-,alum_insu_i,OPAQUE,ANOTHER,02,08  #   3 ||< back_a_edge:plenum
*surf,back_b_edge,VERT,-,-,-,alum_insu_i,OPAQUE,ANOTHER,02,09  #   4 ||< back_b_edge:plenum
*surf,back_edge,VERT,-,-,-,alum_insu_i,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,left_edge,VERT,-,-,-,alum_insu_i,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,upper,CEIL,-,-,-,alum_insu_i,OPAQUE,ANOTHER,02,10  #   7 ||< upper:plenum
*surf,base,FLOR,-,-,-,alum_panel,OPAQUE,ANOTHER,01,12  #   8 ||< rad_pnl:one_bed_400
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,     3.00 0  # zone base list
