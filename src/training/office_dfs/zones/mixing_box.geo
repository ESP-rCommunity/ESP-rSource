*Geometry 1.1,GEN,mixing_box # tag version, format, zone name
*date Fri Jul 10 10:47:16 2009  # latest file modification 
mixing_box is a zonal representation of ahu
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,6.00000,3.00000  #   1
*vertex,3.00000,6.00000,3.00000  #   2
*vertex,3.00000,12.00000,3.00000  #   3
*vertex,0.00000,12.00000,3.00000  #   4
*vertex,0.00000,6.00000,3.50000  #   5
*vertex,3.00000,6.00000,3.50000  #   6
*vertex,3.00000,12.00000,3.50000  #   7
*vertex,0.00000,12.00000,3.50000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,05,10  #   1 ||< core_a:ceil_void
*surf,Surf-2,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ADIABATIC,0,0  #   2 ||< adiabatic
*surf,Surf-3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ADIABATIC,0,0  #   3 ||< adiabatic
*surf,Surf-4,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ADIABATIC,0,0  #   4 ||< adiabatic
*surf,Surf-5,CEIL,-,-,-,gyp_blk_ptn,OPAQUE,ADIABATIC,0,0  #   5 ||< adiabatic
*surf,Surf-6,FLOR,-,-,-,gyp_blk_ptn,OPAQUE,ADIABATIC,0,0  #   6 ||< adiabatic
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    18.00 0  # zone base list
