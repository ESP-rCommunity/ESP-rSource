*Geometry 1.1,GEN,storage # tag version, format, zone name
*date Tue Jan 20 17:46:44 2009  # latest file modification 
storage describes a storage area in retail unit
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,33.00000,0.00000,0.00000  #   1
*vertex,50.00000,0.00000,0.00000  #   2
*vertex,50.00000,20.00000,0.00000  #   3
*vertex,33.00000,20.00000,0.00000  #   4
*vertex,33.00000,0.00000,7.50000  #   5
*vertex,50.00000,0.00000,7.50000  #   6
*vertex,50.00000,20.00000,7.50000  #   7
*vertex,33.00000,20.00000,7.50000  #   8
*vertex,37.69868,5.52786,7.50000  #   9
*vertex,45.30132,5.52786,7.50000  #  10
*vertex,45.30132,14.47214,7.50000  #  11
*vertex,37.69868,14.47214,7.50000  #  12
*vertex,50.00000,4.00000,0.00000  #  13
*vertex,50.00000,9.00000,0.00000  #  14
*vertex,50.00000,9.00000,6.00000  #  15
*vertex,50.00000,4.00000,6.00000  #  16
*vertex,50.00000,11.00000,0.00000  #  17
*vertex,50.00000,16.00000,0.00000  #  18
*vertex,50.00000,16.00000,6.00000  #  19
*vertex,50.00000,11.00000,6.00000  #  20
*vertex,38.31959,6.25834,7.50000  #  21
*vertex,44.68041,6.25834,7.50000  #  22
*vertex,44.68041,13.74166,7.50000  #  23
*vertex,38.31959,13.74166,7.50000  #  24
*vertex,50.00000,10.00000,0.00000  #  25
*vertex,50.00000,10.00000,7.50000  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,8,25,17,20,19,18,3,7,26  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,11,5,6,26,7,8,5,9,12,11,10,9  #  5
*edges,9,1,4,3,18,17,25,14,13,2  #  6
*edges,4,13,14,15,16  #  7
*edges,4,17,18,19,20  #  8
*edges,8,25,26,6,2,13,16,15,14  #  9
*edges,4,21,22,23,24  # 10
*edges,10,9,10,11,12,9,21,24,23,22,21  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_adab_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ADIABATIC,0,0  #   1 ||< adiabatic
*surf,east_2,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_adab_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ADIABATIC,0,0  #   3 ||< adiabatic
*surf,ptn_store,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,01,02  #   4 ||< ptn_stor:sales_area
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,door_1,VERT,east_1,V-DOOR,-,Vehicle_door,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,door_2,VERT,east_2,V-DOOR,-,Vehicle_door,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,east_1,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,Storskylight,CEIL,RL_frame,S-WINDOW,-,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
*surf,RL_frame,CEIL,ceiling,F-FRAME,-,frame_notion,OPAQUE,EXTERIOR,0,0  #  11 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   340.00 0  # zone base list
# 
*bridge_start,0.100,43.237,43.237,432.374  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
