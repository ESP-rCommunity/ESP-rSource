*Geometry 1.1,GEN,boundary_up # tag version, format, zone name
*date Wed Aug 22 15:39:57 2007  # latest file modification 
boundary_up is a boundary zone above suspended ceiling
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,3.50000  #   1
*vertex,0.00000,5.55000,3.50000  #   2
*vertex,6.10000,0.00000,3.50000  #   3
*vertex,6.10000,5.55000,3.50000  #   4
*vertex,0.00000,0.00000,4.00000  #   5
*vertex,0.00000,5.55000,4.00000  #   6
*vertex,6.10000,5.55000,4.00000  #   7
*vertex,6.10000,0.00000,4.00000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,4,3  #  1
*edges,4,8,7,6,5  #  2
*edges,4,3,4,7,8  #  3
*edges,4,4,2,6,7  #  4
*edges,4,2,1,5,6  #  5
*edges,4,1,3,8,5  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,base,FLOR,-,-,-,structure,OPAQUE,ANOTHER,05,05  #   1 ||< upper:ceiling_abv
*surf,t_upper,CEIL,-,-,-,ceiling,OPAQUE,CONSTANT,20,00  #   2 ||< constant @ 20dC &   0W rad
*surf,edg_2upper,VERT,-,-,-,insul_frame,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,edg_3upper,VERT,-,-,-,insul_frame,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,edg_4upper,VERT,-,-,-,insul_frame,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,edg_5upper,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,1,    33.85 0  # zone base list
