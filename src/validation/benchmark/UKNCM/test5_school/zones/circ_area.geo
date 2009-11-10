*Geometry 1.1,GEN,circ_area # tag version, format, zone name
*date Thu Jan 15 18:04:38 2009  # latest file modification 
circ_area describes a circulation area
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,6.00000,0.00000  #   1
*vertex,48.00000,6.00000,0.00000  #   2
*vertex,48.00000,8.00000,0.00000  #   3
*vertex,0.00000,8.00000,0.00000  #   4
*vertex,0.00000,6.00000,3.50000  #   5
*vertex,48.00000,6.00000,3.50000  #   6
*vertex,48.00000,8.00000,3.50000  #   7
*vertex,0.00000,8.00000,3.50000  #   8
*vertex,13.26687,6.55279,3.50000  #   9
*vertex,34.73313,6.55279,3.50000  #  10
*vertex,34.73313,7.44721,3.50000  #  11
*vertex,13.26687,7.44721,3.50000  #  12
*vertex,0.00000,7.63246,0.64320  #  13
*vertex,0.00000,6.36754,0.64320  #  14
*vertex,0.00000,6.36754,2.85680  #  15
*vertex,0.00000,7.63246,2.85680  #  16
*vertex,39.17893,8.00000,0.64320  #  17
*vertex,8.82107,8.00000,0.64320  #  18
*vertex,8.82107,8.00000,2.85680  #  19
*vertex,39.17893,8.00000,2.85680  #  20
*vertex,48.00000,6.36754,0.64320  #  21
*vertex,48.00000,7.63246,0.64320  #  22
*vertex,48.00000,7.63246,2.85680  #  23
*vertex,48.00000,6.36754,2.85680  #  24
*vertex,8.00000,6.00000,0.00000  #  25
*vertex,8.00000,6.00000,3.50000  #  26
*vertex,16.00000,6.00000,0.00000  #  27
*vertex,16.00000,6.00000,3.50000  #  28
*vertex,24.00000,6.00000,0.00000  #  29
*vertex,24.00000,6.00000,3.50000  #  30
*vertex,32.00000,6.00000,0.00000  #  31
*vertex,32.00000,6.00000,3.50000  #  32
*vertex,40.00000,6.00000,0.00000  #  33
*vertex,40.00000,6.00000,3.50000  #  34
*vertex,15.02002,6.62584,3.50000  #  35
*vertex,32.97998,6.62584,3.50000  #  36
*vertex,32.97998,7.37416,3.50000  #  37
*vertex,15.02002,7.37416,3.50000  #  38
*vertex,0.00000,7.60000,0.70000  #  39
*vertex,0.00000,6.40000,0.70000  #  40
*vertex,0.00000,6.40000,2.80000  #  41
*vertex,0.00000,7.60000,2.80000  #  42
*vertex,38.39999,8.00000,0.70000  #  43
*vertex,9.60000,8.00000,0.70000  #  44
*vertex,9.60000,8.00000,2.80000  #  45
*vertex,38.39999,8.00000,2.80000  #  46
*vertex,48.00000,6.40000,0.70000  #  47
*vertex,48.00000,7.60000,0.70000  #  48
*vertex,48.00000,7.60000,2.80000  #  49
*vertex,48.00000,6.40000,2.80000  #  50
# 
# tag, number of vertices followed by list of associated vert
*edges,10,2,3,7,6,2,21,24,23,22,21  #  1
*edges,10,3,4,8,7,3,17,20,19,18,17  #  2
*edges,10,4,1,5,8,4,13,16,15,14,13  #  3
*edges,15,5,26,28,30,32,34,6,7,8,5,9,12,11,10,9  #  4
*edges,9,1,4,3,2,33,31,29,27,25  #  5
*edges,10,9,10,11,12,9,35,38,37,36,35  #  6
*edges,10,13,14,15,16,13,39,42,41,40,39  #  7
*edges,10,17,18,19,20,17,43,46,45,44,43  #  8
*edges,10,21,22,23,24,21,47,50,49,48,47  #  9
*edges,4,1,25,26,5  # 10
*edges,4,25,27,28,26  # 11
*edges,4,27,29,30,28  # 12
*edges,4,29,31,32,30  # 13
*edges,4,31,33,34,32  # 14
*edges,4,33,2,6,34  # 15
*edges,4,35,36,37,38  # 16
*edges,4,39,40,41,42  # 17
*edges,4,43,44,45,46  # 18
*edges,4,47,48,49,50  # 19
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   5 ||< ground profile  1
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,win1_frame,VERT,west,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,win2_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,win3_Frame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,01,03  #  10 ||< ptn_b:op_plan_off
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,02,03  #  11 ||< ptn_b:class_1
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,03,03  #  12 ||< ptn_b:class_2
*surf,ptn_d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,04,03  #  13 ||< ptn_b:class_3
*surf,ptn_e,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,05,03  #  14 ||< ptn_b:class_4
*surf,ptn_f,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,03  #  15 ||< ptn_a:toilet
*surf,rooflight,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  16 ||< external
*surf,window1,VERT,win1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  17 ||< external
*surf,window2,VERT,win2_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  18 ||< external
*surf,window3,VERT,win3_Frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  19 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    96.00 0  # zone base list
# 
*bridge_start,0.100,28.595,28.595,285.949  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
