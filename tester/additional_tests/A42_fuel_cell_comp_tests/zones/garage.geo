*Geometry 1.1,GEN,garage # tag version, format, zone name
*date Sat Jan  4 17:31:21 2014  # latest file modification 
garage describes the garage with slab on grade floor, exterior s
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.97000,6.79000,2.60000  #   1
*vertex,12.00000,6.79000,2.60000  #   2
*vertex,12.00000,12.20000,2.60000  #   3
*vertex,4.97000,12.20000,2.60000  #   4
*vertex,4.97000,6.79000,5.62000  #   5
*vertex,12.00000,6.79000,5.62000  #   6
*vertex,12.00000,12.20000,5.62000  #   7
*vertex,4.97000,12.20000,5.62000  #   8
*vertex,12.00000,7.07000,2.60000  #   9
*vertex,12.00000,11.95000,2.60000  #  10
*vertex,12.00000,11.95000,4.60000  #  11
*vertex,12.00000,7.07000,4.60000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,8,2,9,12,11,10,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,6,1,4,3,10,9,2  #  6
*edges,4,9,10,11,12  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,main_share,VERT,-,-,-,ccht_wall_r,OPAQUE,ANOTHER,02,16  #   1 ||< garage_in:main_second
*surf,front_garage,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,side_wall,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,garage_back,VERT,-,-,-,ccht_wall_r,OPAQUE,ANOTHER,02,17  #   4 ||< garage_back:main_second
*surf,garage_ceiln,CEIL,-,-,-,exp_flr_r,OPAQUE,ANOTHER,02,18  #   5 ||< garage_ceil:main_second
*surf,slab-on-grad,FLOR,-,-,-,slab_floor,OPAQUE,BASESIMP,28,**  #   6 ||< BASESIMP config type  28
*surf,garage_door,VERT,-,-,-,ext_doors,OPAQUE,EXTERIOR,0,0  #   7 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    38.03 0  # zone base list
