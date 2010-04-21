*Geometry 1.1,GEN,meet_room2 # tag version, format, zone name
*date Fri May  8 10:45:31 2009  # latest file modification 
meet_room2 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,30.00000,20.00000,0.00000  #   1
*vertex,35.00000,20.00000,0.00000  #   2
*vertex,35.00000,30.00000,0.00000  #   3
*vertex,30.00000,30.00000,0.00000  #   4
*vertex,30.00000,20.00000,4.00000  #   5
*vertex,35.00000,20.00000,4.00000  #   6
*vertex,35.00000,30.00000,4.00000  #   7
*vertex,30.00000,30.00000,4.00000  #   8
*vertex,35.00000,25.00000,0.00000  #   9
*vertex,35.00000,25.00000,4.00000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,3,4,8,7  #  2
*edges,4,4,1,5,8  #  3
*edges,5,5,6,10,7,8  #  4
*edges,5,1,4,3,9,2  #  5
*edges,4,2,9,10,6  #  6
*edges,4,9,3,7,10  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_circ1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,09,07  #   1 ||< ptn_m2:circ_area1
*surf,ptn_circ2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,10,07  #   2 ||< ptn_m2:circ_area2
*surf,ptn_atr,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,71,06  #   3 ||< Wall-4:atrium
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,26,05  #   4 ||< floor:meetR2_st2
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   5 ||< ground profile  1
*surf,ptn_o9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,13,12  #   6 ||< ptn_meet2:office_9
*surf,ptn_o10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,14,12  #   7 ||< ptn_meet2:office_10
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    50.00 2  # zone base list
# 
*bridge_start,0.100,1.230,1.230,12.305  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
