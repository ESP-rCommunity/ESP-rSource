*Geometry 1.1,GEN,circ_area1 # tag version, format, zone name
*date Fri May  8 11:23:44 2009  # latest file modification 
circ_area1 is ground level adjacent to office_1
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,6.00000,0.00000  #   1
*vertex,10.50000,6.00000,0.00000  #   2
*vertex,10.50000,12.00000,0.00000  #   3
*vertex,6.00000,12.00000,0.00000  #   4
*vertex,6.00000,6.00000,3.50000  #   5
*vertex,10.50000,6.00000,3.50000  #   6
*vertex,10.50000,12.00000,3.50000  #   7
*vertex,6.00000,12.00000,3.50000  #   8
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
*surf,Wall-1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,02,06  #   1 ||< Wall-3a:office_2
*surf,Wall-2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,07,04  #   2 ||< Wall-4:office_5
*surf,Wall-3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,04,06  #   3 ||< Wall-1a:office_4
*surf,Wall-4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,01,07  #   4 ||< Wall-2b:office_1
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,12,06  #   5 ||< floor:circ1_st2
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,27.00,1  # zone base
# 
*bridge_start,0.100,0.664,0.664,6.645  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
