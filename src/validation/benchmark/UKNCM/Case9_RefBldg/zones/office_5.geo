*Geometry 1.1,GEN,office_5 # tag version, format, zone name
*date Fri May  8 11:23:57 2009  # latest file modification 
office_5 is a ground level open plan office in core
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.50000,6.00000,0.00000  #   1
*vertex,29.50000,6.00000,0.00000  #   2
*vertex,29.50000,12.00000,0.00000  #   3
*vertex,10.50000,12.00000,0.00000  #   4
*vertex,10.50000,6.00000,3.50000  #   5
*vertex,29.50000,6.00000,3.50000  #   6
*vertex,29.50000,12.00000,3.50000  #   7
*vertex,10.50000,12.00000,3.50000  #   8
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
*surf,Wall-1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,02,07  #   1 ||< Wall-3b:office_2
*surf,Wall-2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,04  #   2 ||< Wall-4:circ_area2
*surf,Wall-3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,04,07  #   3 ||< Wall-1b:office_4
*surf,Wall-4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,05,02  #   4 ||< Wall-2:circ_area1
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,14,06  #   5 ||< floor:off5_st2
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   114.00 2  # zone base list
# 
*bridge_start,0.100,2.805,2.805,28.055  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
