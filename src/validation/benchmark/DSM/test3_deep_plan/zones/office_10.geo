*Geometry 1.1,GEN,office_10 # tag version, format, zone name
*date Fri May  8 10:37:41 2009  # latest file modification 
office_10 is on ground level internal
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,25.00000,0.00000  #   1
*vertex,15.00000,25.00000,0.00000  #   2
*vertex,15.00000,35.00000,0.00000  #   3
*vertex,35.00000,35.00000,0.00000  #   4
*vertex,35.00000,25.00000,0.00000  #   5
*vertex,44.00000,25.00000,0.00000  #   6
*vertex,44.00000,44.00000,0.00000  #   7
*vertex,6.00000,44.00000,0.00000  #   8
*vertex,6.00000,25.00000,4.00000  #   9
*vertex,15.00000,25.00000,4.00000  #  10
*vertex,15.00000,35.00000,4.00000  #  11
*vertex,35.00000,35.00000,4.00000  #  12
*vertex,35.00000,25.00000,4.00000  #  13
*vertex,44.00000,25.00000,4.00000  #  14
*vertex,44.00000,44.00000,4.00000  #  15
*vertex,6.00000,44.00000,4.00000  #  16
*vertex,15.00000,30.00000,0.00000  #  17
*vertex,15.00000,30.00000,4.00000  #  18
*vertex,35.00000,30.00000,0.00000  #  19
*vertex,35.00000,30.00000,4.00000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,10,9  #  1
*edges,4,3,4,12,11  #  2
*edges,4,5,6,14,13  #  3
*edges,4,6,7,15,14  #  4
*edges,4,7,8,16,15  #  5
*edges,4,8,1,9,16  #  6
*edges,10,9,10,18,11,12,20,13,14,15,16  #  7
*edges,10,1,8,7,6,5,19,4,3,17,2  #  8
*edges,4,17,3,11,18  #  9
*edges,4,4,19,20,12  # 10
*edges,4,2,17,18,10  # 11
*edges,4,19,5,13,20  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_o9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,13,05  #   1 ||< ptn_o10a:office_9
*surf,ptn_circa,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,10,02  #   2 ||< ptn_o10b:circ_area2
*surf,ptn_o9a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,13,03  #   3 ||< ptn_o10:office_9
*surf,ptn_o4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,04,08  #   4 ||< ptn_o10:office_4
*surf,ptn_o6,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,01  #   5 ||< ptn_o10:office_6
*surf,ptn_o8,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,08,07  #   6 ||< ptn_o10:office_8
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,28,08  #   7 ||< floor:off10_st2
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   8 ||< ground profile  1
*surf,ptn_circb,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,10,03  #   9 ||< ptn_o10c:circ_area2
*surf,ptn_circc,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,10,01  #  10 ||< ptn_o10a:circ_area2
*surf,ptn_meet1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,11,07  #  11 ||< ptn_o10:meet_room1
*surf,ptn_meet2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,12,07  #  12 ||< ptn_o10:meet_room2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,   522.00 2  # zone base list
# 
*bridge_start,0.100,12.846,12.846,128.461  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
