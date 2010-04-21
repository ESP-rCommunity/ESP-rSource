*Geometry 1.1,GEN,off1_st2 # tag version, format, zone name
*date Fri May  8 11:24:05 2009  # latest file modification 
off1_st2 is level two office facing west
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,3.50000  #   1
*vertex,6.00000,0.00000,3.50000  #   2
*vertex,6.00000,18.00000,3.50000  #   3
*vertex,0.00000,18.00000,3.50000  #   4
*vertex,0.00000,0.00000,7.00000  #   5
*vertex,6.00000,0.00000,7.00000  #   6
*vertex,6.00000,18.00000,7.00000  #   7
*vertex,0.00000,18.00000,7.00000  #   8
*vertex,6.00000,12.00000,3.50000  #   9
*vertex,6.00000,12.00000,7.00000  #  10
*vertex,6.00000,6.00000,3.50000  #  11
*vertex,6.00000,6.00000,7.00000  #  12
*vertex,1.10263,0.00000,4.14320  #  13
*vertex,4.89737,0.00000,4.14320  #  14
*vertex,4.89737,0.00000,6.35680  #  15
*vertex,1.10263,0.00000,6.35680  #  16
*vertex,0.00000,14.69210,4.14320  #  17
*vertex,0.00000,3.30790,4.14320  #  18
*vertex,0.00000,3.30790,6.35680  #  19
*vertex,0.00000,14.69210,6.35680  #  20
*vertex,4.89737,18.00000,4.14320  #  21
*vertex,1.10263,18.00000,4.14320  #  22
*vertex,1.10263,18.00000,6.35680  #  23
*vertex,4.89737,18.00000,6.35680  #  24
*vertex,1.20000,0.00000,4.20000  #  25
*vertex,4.80000,0.00000,4.20000  #  26
*vertex,4.80000,0.00000,6.30000  #  27
*vertex,1.20000,0.00000,6.30000  #  28
*vertex,0.00000,14.40000,4.20000  #  29
*vertex,0.00000,3.60000,4.20000  #  30
*vertex,0.00000,3.60000,6.30000  #  31
*vertex,0.00000,14.40000,6.30000  #  32
*vertex,4.80000,18.00000,4.20000  #  33
*vertex,1.20000,18.00000,4.20000  #  34
*vertex,1.20000,18.00000,6.30000  #  35
*vertex,4.80000,18.00000,6.30000  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,13,16,15,14,13  #  1
*edges,10,3,4,8,7,3,21,24,23,22,21  #  2
*edges,10,4,1,5,8,4,17,20,19,18,17  #  3
*edges,6,5,6,12,10,7,8  #  4
*edges,6,1,4,3,9,11,2  #  5
*edges,4,9,3,7,10  #  6
*edges,4,11,9,10,12  #  7
*edges,4,2,11,12,6  #  8
*edges,10,13,14,15,16,13,25,28,27,26,25  #  9
*edges,10,17,18,19,20,17,29,32,31,30,29  # 10
*edges,10,21,22,23,24,21,33,36,35,34,33  # 11
*edges,4,25,26,27,28  # 12
*edges,4,29,30,31,32  # 13
*edges,4,33,34,35,36  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Wall-3,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Wall-4,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,15,05  #   4 ||< floor:off1_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,01,04  #   5 ||< ceiling:office_1
*surf,Wall-2a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,11,03  #   6 ||< Wall-4:off4_st2
*surf,Wall-2b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,12,04  #   7 ||< Wall-4:circ1_st2
*surf,Wall-2c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,09,03  #   8 ||< Wall-4:off2_st2
*surf,win1_frame,VERT,Wall-1,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,win2_frame,VERT,Wall-4,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,win3_frame,VERT,Wall-3,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,win1_glz,VERT,win1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
*surf,win2_glz,VERT,win2_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  13 ||< external
*surf,win3_glz,VERT,win3_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  14 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,   108.00 2  # zone base list
# 
*bridge_start,0.100,11.421,11.421,114.212  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
