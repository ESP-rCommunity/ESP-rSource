*Geometry 1.1,GEN,atrium # tag version, format, zone name
*date Fri May  8 10:42:07 2009  # latest file modification 
atrium describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,20.00000,20.00000,0.00000  #   1
*vertex,30.00000,20.00000,0.00000  #   2
*vertex,30.00000,30.00000,0.00000  #   3
*vertex,20.00000,30.00000,0.00000  #   4
*vertex,20.00000,20.00000,20.00000  #   5
*vertex,30.00000,20.00000,20.00000  #   6
*vertex,30.00000,30.00000,20.00000  #   7
*vertex,20.00000,30.00000,20.00000  #   8
*vertex,20.00000,20.00000,4.00000  #   9
*vertex,30.00000,20.00000,4.00000  #  10
*vertex,30.00000,30.00000,4.00000  #  11
*vertex,20.00000,30.00000,4.00000  #  12
*vertex,20.00000,20.00000,8.00000  #  13
*vertex,30.00000,20.00000,8.00000  #  14
*vertex,30.00000,30.00000,8.00000  #  15
*vertex,20.00000,30.00000,8.00000  #  16
*vertex,20.00000,20.00000,12.00000  #  17
*vertex,30.00000,20.00000,12.00000  #  18
*vertex,30.00000,30.00000,12.00000  #  19
*vertex,20.00000,30.00000,12.00000  #  20
*vertex,20.00000,20.00000,16.00000  #  21
*vertex,30.00000,20.00000,16.00000  #  22
*vertex,30.00000,30.00000,16.00000  #  23
*vertex,20.00000,30.00000,16.00000  #  24
*vertex,22.76393,22.76393,20.00000  #  25
*vertex,27.23607,22.76393,20.00000  #  26
*vertex,27.23607,27.23607,20.00000  #  27
*vertex,22.76393,27.23607,20.00000  #  28
*vertex,23.12917,23.12917,20.00000  #  29
*vertex,26.87083,23.12917,20.00000  #  30
*vertex,26.87083,26.87083,20.00000  #  31
*vertex,23.12917,26.87083,20.00000  #  32
# 
# tag, number of vertices followed by list of associated vert
*edges,10,5,6,7,8,5,25,28,27,26,25  #  1
*edges,4,1,4,3,2  #  2
*edges,4,1,2,10,9  #  3
*edges,4,3,4,12,11  #  4
*edges,4,4,1,9,12  #  5
*edges,4,2,3,11,10  #  6
*edges,4,9,10,14,13  #  7
*edges,4,11,12,16,15  #  8
*edges,4,12,9,13,16  #  9
*edges,4,10,11,15,14  # 10
*edges,4,13,14,18,17  # 11
*edges,4,15,16,20,19  # 12
*edges,4,16,13,17,20  # 13
*edges,4,14,15,19,18  # 14
*edges,4,17,18,22,21  # 15
*edges,4,19,20,24,23  # 16
*edges,4,20,17,21,24  # 17
*edges,4,18,19,23,22  # 18
*edges,4,21,22,6,5  # 19
*edges,4,23,24,8,7  # 20
*edges,4,24,21,5,8  # 21
*edges,4,22,23,7,6  # 22
*edges,10,25,26,27,28,25,29,32,31,30,29  # 23
*edges,4,29,30,31,32  # 24
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Top-5,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Base-6,FLOR,-,PARTN,-,Sol_grnd_not,OPAQUE,GROUND,01,00  #   2 ||< ground profile  1
*surf,Wall-3b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,09,08  #   3 ||< ptn_atr:circ_area1
*surf,Wall-1b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,10,08  #   4 ||< ptn_atr:circ_area2
*surf,Wall-2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,11,02  #   5 ||< ptn_atr:meet_room1
*surf,Wall-4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,12,03  #   6 ||< ptn_atr:meet_room2
*surf,Wall-3b1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,23,08  #   7 ||< ptn_atr:circ1_st2
*surf,Wall-1bst2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,24,08  #   8 ||< ptn_atr:circ2_st2
*surf,Wall-2st2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,25,02  #   9 ||< ptn_atr:meetR1_st2
*surf,Wall-4st2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,26,03  #  10 ||< ptn_atr:meetR2_st2
*surf,Wall-3bst3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,37,08  #  11 ||< ptn_atr:circ1_st3
*surf,Wall-1bst3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,38,08  #  12 ||< ptn_atr:circ2_st3
*surf,Wall-2st3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,39,02  #  13 ||< ptn_atr:meetR1_st3
*surf,Wall-4st3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,40,03  #  14 ||< ptn_atrium:meetR2_st3
*surf,Wall-3bst4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,51,08  #  15 ||< ptn_atr:circ1_st4
*surf,Wall-1bst4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,52,08  #  16 ||< ptn_atr:circ2_st4
*surf,Wall-2st4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,53,02  #  17 ||< ptn_atrium:meetR1_st4
*surf,Wall-4st4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,54,03  #  18 ||< ptn_atrium:meetR2_st4
*surf,Wall-3bst5,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,65,08  #  19 ||< ptn_atr:circ1_st5
*surf,Wall-1bst5,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,66,08  #  20 ||< ptn_atr:circ2_st5
*surf,Wall-2st5,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,67,02  #  21 ||< ptn_atrium:meetR1_st5
*surf,Wall-4st5,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,68,03  #  22 ||< ptn_atr:meetR2_st5
*surf,roof_frame,CEIL,Top-5,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  23 ||< external
*surf,skylight,CEIL,roof_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  24 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,2,   100.00 2  # zone base list
# 
*bridge_start,0.100,9.165,9.165,91.649  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
