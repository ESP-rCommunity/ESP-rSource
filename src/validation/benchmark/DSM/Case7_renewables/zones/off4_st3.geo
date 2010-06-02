*Geometry 1.1,GEN,off4_st3 # tag version, format, zone name
*date Fri May  8 11:25:15 2009  # latest file modification 
off4_st3 is a 3rd level open plan office facing north
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,12.00000,7.00000  #   1
*vertex,34.00000,12.00000,7.00000  #   2
*vertex,34.00000,18.00000,7.00000  #   3
*vertex,6.00000,18.00000,7.00000  #   4
*vertex,6.00000,12.00000,10.50000  #   5
*vertex,34.00000,12.00000,10.50000  #   6
*vertex,34.00000,18.00000,10.50000  #   7
*vertex,6.00000,18.00000,10.50000  #   8
*vertex,10.50000,12.00000,7.00000  #   9
*vertex,10.50000,12.00000,10.50000  #  10
*vertex,29.50000,12.00000,7.00000  #  11
*vertex,29.50000,12.00000,10.50000  #  12
*vertex,28.85438,18.00000,7.64320  #  13
*vertex,11.14562,18.00000,7.64320  #  14
*vertex,11.14562,18.00000,9.85680  #  15
*vertex,28.85438,18.00000,9.85680  #  16
*vertex,13.73901,13.65836,10.50000  #  17
*vertex,26.26099,13.65836,10.50000  #  18
*vertex,26.26099,16.34164,10.50000  #  19
*vertex,13.73901,16.34164,10.50000  #  20
*vertex,28.40000,18.00000,7.70000  #  21
*vertex,11.60000,18.00000,7.70000  #  22
*vertex,11.60000,18.00000,9.80000  #  23
*vertex,28.40000,18.00000,9.80000  #  24
*vertex,14.76168,13.87750,10.50000  #  25
*vertex,25.23832,13.87750,10.50000  #  26
*vertex,25.23832,16.12250,10.50000  #  27
*vertex,14.76168,16.12250,10.50000  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,3,7,6  #  1
*edges,10,3,4,8,7,3,13,16,15,14,13  #  2
*edges,4,4,1,5,8  #  3
*edges,12,5,10,12,6,7,8,5,17,20,19,18,17  #  4
*edges,6,1,4,3,2,11,9  #  5
*edges,4,1,9,10,5  #  6
*edges,4,9,11,12,10  #  7
*edges,4,11,2,6,12  #  8
*edges,10,13,14,15,16,13,21,24,23,22,21  #  9
*edges,10,17,18,19,20,17,25,28,27,26,25  # 10
*edges,4,21,22,23,24  # 11
*edges,4,25,26,27,28  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,17,06  #   1 ||< Wall-4a:off3_st3
*surf,Wall-3,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Wall-4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,15,06  #   3 ||< Wall-2a:off1_st3
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,11,04  #   5 ||< ceiling:off4_st2
*surf,Wall-1a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,19,03  #   6 ||< Wall-3:circ1_st3
*surf,Wall-1b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,21,03  #   7 ||< Wall-3:off5_st3
*surf,Wall-1c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,20,03  #   8 ||< Wall-3:circ2_st3
*surf,win_frame,VERT,Wall-3,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,win_glz,VERT,win_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  11 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,   168.00 2  # zone base list
# 
*bridge_start,0.100,21.922,21.922,219.224  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
