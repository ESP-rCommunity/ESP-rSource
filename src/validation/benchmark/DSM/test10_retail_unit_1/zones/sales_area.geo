*Geometry 1.1,GEN,sales_area # tag version, format, zone name
*date Tue Jan 20 17:45:41 2009  # latest file modification 
sales_area describes a sales area
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,33.00000,0.00000,0.00000  #   2
*vertex,33.00000,20.00000,0.00000  #   3
*vertex,0.00000,20.00000,0.00000  #   4
*vertex,0.00000,0.00000,7.50000  #   5
*vertex,33.00000,0.00000,7.50000  #   6
*vertex,33.00000,20.00000,7.50000  #   7
*vertex,0.00000,20.00000,7.50000  #   8
*vertex,9.12098,5.52786,7.50000  #   9
*vertex,23.87902,5.52786,7.50000  #  10
*vertex,23.87902,14.47214,7.50000  #  11
*vertex,9.12098,14.47214,7.50000  #  12
*vertex,0.00000,19.94987,0.00752  #  13
*vertex,0.00000,0.05013,0.00752  #  14
*vertex,0.00000,0.05013,3.02300  #  15
*vertex,0.00000,19.94987,3.02300  #  16
*vertex,10.32627,6.25834,7.50000  #  17
*vertex,22.67373,6.25834,7.50000  #  18
*vertex,22.67373,13.74166,7.50000  #  19
*vertex,10.32627,13.74166,7.50000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,10,4,1,5,8,4,13,16,15,14,13  #  4
*edges,10,5,6,7,8,5,9,12,11,10,9  #  5
*edges,4,1,4,3,2  #  6
*edges,4,17,18,19,20  #  7
*edges,10,9,10,11,12,9,17,20,19,18,17  #  8
*edges,4,13,14,15,16  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_adb_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ADIABATIC,0,0  #   1 ||< adiabatic
*surf,ptn_stor,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,02,04  #   2 ||< ptn_store:storage
*surf,ptn_adb_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ADIABATIC,0,0  #   3 ||< adiabatic
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,skylight,CEIL,sky_frame,S-WINDOW,-,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   7 ||< external
*surf,sky_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,Displ_glz,VERT,west,D-WINDOW,CLOSED,Not_DisplayG,dispGlz_not,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   660.00 0  # zone base list
# 
*bridge_start,0.100,98.867,98.867,988.669  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
