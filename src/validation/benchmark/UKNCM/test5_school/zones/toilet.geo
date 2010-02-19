*Geometry 1.1,GEN,toilet # tag version, format, zone name
*date Tue Jan 20 19:09:21 2009  # latest file modification 
toilet describes a toilet
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,40.00000,0.00000,0.00000  #   1
*vertex,48.00000,0.00000,0.00000  #   2
*vertex,48.00000,6.00000,0.00000  #   3
*vertex,40.00000,6.00000,0.00000  #   4
*vertex,40.00000,0.00000,3.50000  #   5
*vertex,48.00000,0.00000,3.50000  #   6
*vertex,48.00000,6.00000,3.50000  #   7
*vertex,40.00000,6.00000,3.50000  #   8
*vertex,42.21115,1.65836,3.50000  #   9
*vertex,45.78885,1.65836,3.50000  #  10
*vertex,45.78885,4.34164,3.50000  #  11
*vertex,42.21115,4.34164,3.50000  #  12
*vertex,48.00000,1.10263,0.64320  #  13
*vertex,48.00000,4.89737,0.64320  #  14
*vertex,48.00000,4.89737,2.85680  #  15
*vertex,48.00000,1.10263,2.85680  #  16
*vertex,41.47018,0.00000,0.64320  #  17
*vertex,46.52982,0.00000,0.64320  #  18
*vertex,46.52982,0.00000,2.85680  #  19
*vertex,41.47018,0.00000,2.85680  #  20
*vertex,42.50334,1.87750,3.50000  #  21
*vertex,45.49666,1.87750,3.50000  #  22
*vertex,45.49666,4.12250,3.50000  #  23
*vertex,42.50334,4.12250,3.50000  #  24
*vertex,41.60000,0.00000,0.70000  #  25
*vertex,46.40000,0.00000,0.70000  #  26
*vertex,46.40000,0.00000,2.80000  #  27
*vertex,41.60000,0.00000,2.80000  #  28
*vertex,48.00000,1.20000,0.70000  #  29
*vertex,48.00000,4.80000,0.70000  #  30
*vertex,48.00000,4.80000,2.80000  #  31
*vertex,48.00000,1.20000,2.80000  #  32
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,17,20,19,18,17  #  1
*edges,10,2,3,7,6,2,13,16,15,14,13  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,10,5,6,7,8,5,9,12,11,10,9  #  5
*edges,4,1,4,3,2  #  6
*edges,10,9,10,11,12,9,21,24,23,22,21  #  7
*edges,10,13,14,15,16,13,29,32,31,30,29  #  8
*edges,10,17,18,19,20,17,25,28,27,26,25  #  9
*edges,4,21,22,23,24  # 10
*edges,4,25,26,27,28  # 11
*edges,4,29,30,31,32  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,07,15  #   3 ||< ptn_f:circ_area
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,05,02  #   4 ||< ptn_a:class_4
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,win_toiFrame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,win2_toiFram,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,rooflight,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
*surf,window2,VERT,win2_toiFram,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  11 ||< external
*surf,window,VERT,win_toiFrame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    48.00 0  # zone base list
# 
*bridge_start,0.100,9.729,9.729,97.290  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
