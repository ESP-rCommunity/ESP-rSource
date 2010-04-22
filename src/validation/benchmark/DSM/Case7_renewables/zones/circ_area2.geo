*Geometry 1.1,GEN,circ_area2 # tag version, format, zone name
*date Thu Jan 22 17:49:23 2009  # latest file modification 
circ_area2 is ground level circulation adjacent to office_4
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,29.50000,6.00000,0.00000  #   1
*vertex,34.00000,6.00000,0.00000  #   2
*vertex,34.00000,12.00000,0.00000  #   3
*vertex,29.50000,12.00000,0.00000  #   4
*vertex,29.50000,6.00000,3.50000  #   5
*vertex,34.00000,6.00000,3.50000  #   6
*vertex,34.00000,12.00000,3.50000  #   7
*vertex,29.50000,12.00000,3.50000  #   8
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
*surf,Wall-1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,02,08  #   1 ||< Wall-3c:office_2
*surf,Wall-2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,03,07  #   2 ||< Wall-4b:office_3
*surf,Wall-3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,04,08  #   3 ||< Wall-1c:office_4
*surf,Wall-4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,07,02  #   4 ||< Wall-2:office_5
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,13,06  #   5 ||< floor:circ2_st2
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 0  # zone base list
# 
*bridge_start,0.100,0.664,0.664,6.645  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
