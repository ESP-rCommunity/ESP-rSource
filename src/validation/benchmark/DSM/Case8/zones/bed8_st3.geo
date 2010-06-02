*Geometry 1.1,GEN,bed8_st3 # tag version, format, zone name
*date Thu Dec 11 14:11:27 2008  # latest file modification 
bed8_st3 is guest room eight on top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,28.00000,0.00000,7.00000  #   1
*vertex,31.50000,0.00000,7.00000  #   2
*vertex,31.50000,5.00000,7.00000  #   3
*vertex,30.00000,5.00000,7.00000  #   4
*vertex,30.00000,3.50000,7.00000  #   5
*vertex,28.00000,3.50000,7.00000  #   6
*vertex,28.00000,0.00000,10.50000  #   7
*vertex,31.50000,0.00000,10.50000  #   8
*vertex,31.50000,5.00000,10.50000  #   9
*vertex,30.00000,5.00000,10.50000  #  10
*vertex,30.00000,3.50000,10.50000  #  11
*vertex,28.00000,3.50000,10.50000  #  12
*vertex,28.79148,0.00000,7.79149  #  13
*vertex,30.70852,0.00000,7.79149  #  14
*vertex,30.70852,0.00000,9.70851  #  15
*vertex,28.79148,0.00000,9.70851  #  16
*vertex,31.50000,1.13069,7.79149  #  17
*vertex,31.50000,3.86931,7.79149  #  18
*vertex,31.50000,3.86931,9.70851  #  19
*vertex,31.50000,1.13069,9.70851  #  20
*vertex,29.00000,0.70000,10.50000  #  21
*vertex,30.50000,0.70000,10.50000  #  22
*vertex,30.50000,2.64000,10.50000  #  23
*vertex,29.00000,2.64000,10.50000  #  24
*vertex,28.84067,0.00000,7.84068  #  25
*vertex,30.65933,0.00000,7.84068  #  26
*vertex,30.65933,0.00000,9.65932  #  27
*vertex,28.84067,0.00000,9.65932  #  28
*vertex,31.50000,1.20096,7.84068  #  29
*vertex,31.50000,3.79904,7.84068  #  30
*vertex,31.50000,3.79904,9.65932  #  31
*vertex,31.50000,1.20096,9.65932  #  32
*vertex,29.12251,0.85844,10.50000  #  33
*vertex,30.37749,0.85844,10.50000  #  34
*vertex,30.37749,2.48156,10.50000  #  35
*vertex,29.12251,2.48156,10.50000  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,8,7,1,13,16,15,14,13  #  1
*edges,10,2,3,9,8,2,17,20,19,18,17  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,12,7,8,9,10,11,12,7,21,24,23,22,21  #  7
*edges,6,1,6,5,4,3,2  #  8
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
*surf,south,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,56,28  #   3 ||< ptn_am:cirArea_st3
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,36,01  #   4 ||< ptn_a:bath78_st3
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,36,07  #   5 ||< ptn_e:bath78_st3
*surf,ptn_d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,46,02  #   6 ||< ptn_a:bed7_st3
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,15,07  #   8 ||< ceiling:bed8_st2
*surf,wins_frame,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,wine_frame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,wins_glz,VERT,wins_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
*surf,wine_glz,VERT,wine_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  13 ||< external
*surf,RL_glz,CEIL,RL_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  14 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    14.50 0  # zone base list
# 
*bridge_start,0.100,3.659,3.659,36.592  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
