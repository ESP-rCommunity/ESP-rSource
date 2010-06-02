*Geometry 1.1,GEN,off3_st3 # tag version, format, zone name
*date Fri May  8 11:25:08 2009  # latest file modification 
off3_st3 is 3rd level open plan office facing east
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.00000,0.00000,7.00000  #   1
*vertex,40.00000,0.00000,7.00000  #   2
*vertex,40.00000,18.00000,7.00000  #   3
*vertex,34.00000,18.00000,7.00000  #   4
*vertex,34.00000,0.00000,10.50000  #   5
*vertex,40.00000,0.00000,10.50000  #   6
*vertex,40.00000,18.00000,10.50000  #   7
*vertex,34.00000,18.00000,10.50000  #   8
*vertex,34.00000,12.00000,7.00000  #   9
*vertex,34.00000,12.00000,10.50000  #  10
*vertex,34.00000,6.00000,7.00000  #  11
*vertex,34.00000,6.00000,10.50000  #  12
*vertex,35.10263,0.00000,7.64320  #  13
*vertex,38.89737,0.00000,7.64320  #  14
*vertex,38.89737,0.00000,9.85680  #  15
*vertex,35.10263,0.00000,9.85680  #  16
*vertex,40.00000,3.30790,7.64320  #  17
*vertex,40.00000,14.69210,7.64320  #  18
*vertex,40.00000,14.69210,9.85680  #  19
*vertex,40.00000,3.30790,9.85680  #  20
*vertex,38.89737,18.00000,7.64320  #  21
*vertex,35.10263,18.00000,7.64320  #  22
*vertex,35.10263,18.00000,9.85680  #  23
*vertex,38.89737,18.00000,9.85680  #  24
*vertex,35.65836,4.97508,10.50000  #  25
*vertex,38.34164,4.97508,10.50000  #  26
*vertex,38.34164,13.02492,10.50000  #  27
*vertex,35.65836,13.02492,10.50000  #  28
*vertex,35.20000,0.00000,7.70000  #  29
*vertex,38.80000,0.00000,7.70000  #  30
*vertex,38.80000,0.00000,9.80000  #  31
*vertex,35.20000,0.00000,9.80000  #  32
*vertex,40.00000,3.60000,7.70000  #  33
*vertex,40.00000,14.40000,7.70000  #  34
*vertex,40.00000,14.40000,9.80000  #  35
*vertex,40.00000,3.60000,9.80000  #  36
*vertex,38.80000,18.00000,7.70000  #  37
*vertex,35.20000,18.00000,7.70000  #  38
*vertex,35.20000,18.00000,9.80000  #  39
*vertex,38.80000,18.00000,9.80000  #  40
*vertex,35.87750,5.63251,10.50000  #  41
*vertex,38.12250,5.63251,10.50000  #  42
*vertex,38.12250,12.36749,10.50000  #  43
*vertex,35.87750,12.36749,10.50000  #  44
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,13,16,15,14,13  #  1
*edges,10,2,3,7,6,2,17,20,19,18,17  #  2
*edges,10,3,4,8,7,3,21,24,23,22,21  #  3
*edges,12,5,6,7,8,10,12,5,25,28,27,26,25  #  4
*edges,6,1,11,9,4,3,2  #  5
*edges,4,4,9,10,8  #  6
*edges,4,9,11,12,10  #  7
*edges,4,11,1,5,12  #  8
*edges,10,13,14,15,16,13,29,32,31,30,29  #  9
*edges,10,17,18,19,20,17,33,36,35,34,33  # 10
*edges,10,21,22,23,24,21,37,40,39,38,37  # 11
*edges,10,25,26,27,28,25,41,44,43,42,41  # 12
*edges,4,29,30,31,32  # 13
*edges,4,33,34,35,36  # 14
*edges,4,37,38,39,40  # 15
*edges,4,41,42,43,44  # 16
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Wall-2,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Wall-3,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,10,04  #   5 ||< ceiling:off3_st2
*surf,Wall-4a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,18,01  #   6 ||< Wall-2:off4_st3
*surf,Wall-4b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,20,02  #   7 ||< Wall-2:circ2_st3
*surf,Wall-4c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,16,02  #   8 ||< Wall-2:off2_st3
*surf,win1_frame,VERT,Wall-1,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,win2_frame,VERT,Wall-2,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,win3_frame,VERT,Wall-3,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,win1_glz,VERT,win1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  13 ||< external
*surf,win2_glz,VERT,win2_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  14 ||< external
*surf,win3_glz,VERT,win3_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  15 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  16 ||< external
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
*bridge_start,0.100,18.661,18.661,186.614  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
