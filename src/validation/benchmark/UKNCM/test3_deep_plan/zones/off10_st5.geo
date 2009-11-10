*Geometry 1.1,GEN,off10_st5 # tag version, format, zone name
*date Fri May  8 10:51:10 2009  # latest file modification 
off10_st5 is the other inner office of the top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,25.00000,16.00000  #   1
*vertex,15.00000,25.00000,16.00000  #   2
*vertex,15.00000,35.00000,16.00000  #   3
*vertex,35.00000,35.00000,16.00000  #   4
*vertex,35.00000,25.00000,16.00000  #   5
*vertex,44.00000,25.00000,16.00000  #   6
*vertex,44.00000,44.00000,16.00000  #   7
*vertex,6.00000,44.00000,16.00000  #   8
*vertex,6.00000,25.00000,20.00000  #   9
*vertex,15.00000,25.00000,20.00000  #  10
*vertex,15.00000,35.00000,20.00000  #  11
*vertex,35.00000,35.00000,20.00000  #  12
*vertex,35.00000,25.00000,20.00000  #  13
*vertex,44.00000,25.00000,20.00000  #  14
*vertex,44.00000,44.00000,20.00000  #  15
*vertex,6.00000,44.00000,20.00000  #  16
*vertex,15.00000,30.00000,16.00000  #  17
*vertex,15.00000,30.00000,20.00000  #  18
*vertex,35.00000,30.00000,16.00000  #  19
*vertex,35.00000,30.00000,20.00000  #  20
*vertex,6.00000,35.00000,20.00000  #  21
*vertex,44.00000,35.00000,20.00000  #  22
*vertex,16.50294,37.48754,20.00000  #  23
*vertex,33.49706,37.48754,20.00000  #  24
*vertex,33.49706,41.51246,20.00000  #  25
*vertex,16.50294,41.51246,20.00000  #  26
*vertex,8.48754,27.76393,20.00000  #  27
*vertex,12.51246,27.76393,20.00000  #  28
*vertex,12.51246,32.23607,20.00000  #  29
*vertex,8.48754,32.23607,20.00000  #  30
*vertex,37.48754,27.76393,20.00000  #  31
*vertex,41.51246,27.76393,20.00000  #  32
*vertex,41.51246,32.23607,20.00000  #  33
*vertex,37.48754,32.23607,20.00000  #  34
*vertex,17.89085,37.81626,20.00000  #  35
*vertex,32.10915,37.81626,20.00000  #  36
*vertex,32.10915,41.18374,20.00000  #  37
*vertex,17.89085,41.18374,20.00000  #  38
*vertex,8.81626,28.12917,20.00000  #  39
*vertex,12.18374,28.12917,20.00000  #  40
*vertex,12.18374,31.87083,20.00000  #  41
*vertex,8.81626,31.87083,20.00000  #  42
*vertex,37.81626,28.12917,20.00000  #  43
*vertex,41.18374,28.12917,20.00000  #  44
*vertex,41.18374,31.87083,20.00000  #  45
*vertex,37.81626,31.87083,20.00000  #  46
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,10,9  #  1
*edges,4,3,4,12,11  #  2
*edges,4,5,6,14,13  #  3
*edges,5,6,7,15,22,14  #  4
*edges,4,7,8,16,15  #  5
*edges,5,8,1,9,21,16  #  6
*edges,12,21,11,12,22,15,16,21,23,26,25,24,23  #  7
*edges,10,1,8,7,6,5,19,4,3,17,2  #  8
*edges,4,17,3,11,18  #  9
*edges,4,4,19,20,12  # 10
*edges,4,2,17,18,10  # 11
*edges,4,19,5,13,20  # 12
*edges,11,9,10,18,11,21,9,27,30,29,28,27  # 13
*edges,11,13,14,22,12,20,13,31,34,33,32,31  # 14
*edges,10,23,24,25,26,23,35,38,37,36,35  # 15
*edges,10,27,28,29,30,27,39,42,41,40,39  # 16
*edges,10,31,32,33,34,31,43,46,45,44,43  # 17
*edges,4,35,36,37,38  # 18
*edges,4,39,40,41,42  # 19
*edges,4,43,44,45,46  # 20
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_of9a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,69,05  #   1 ||< ptn_of10b:office9_st5
*surf,ptn_circ2a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,66,02  #   2 ||< ptn_of10b:circ2_st5
*surf,ptn_of9b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,69,03  #   3 ||< ptn_of10a:office9_st5
*surf,ptn_of4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,60,08  #   4 ||< ptn_of10:office4_st5
*surf,ptn_of6,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,62,01  #   5 ||< ptn_of10:office6_st5
*surf,ptn_of8,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,64,07  #   6 ||< ptn_of10:office8_st5
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,56,07  #   8 ||< ceiling:off10_st4
*surf,ptn_circ2b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,66,03  #   9 ||< ptn_of10c:circ2_st5
*surf,ptn_circ2c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,66,01  #  10 ||< ptn_of10a:circ2_st5
*surf,ptn_m1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,67,07  #  11 ||< ptn_of10:meetR1_st5
*surf,ptn_m2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,68,07  #  12 ||< ptn_of10:meetR2_st5
*surf,ceiling_b,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,ceiling_c,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,RL_frame_a,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,RL_frame_b,CEIL,ceiling_b,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,RL_frame_c,CEIL,ceiling_c,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,RL_glz_a,CEIL,RL_frame_a,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  18 ||< external
*surf,RL_glz_b,CEIL,RL_frame_b,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  19 ||< external
*surf,RL_glz_c,CEIL,RL_frame_c,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  20 ||< external
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
*bridge_start,0.100,34.995,34.995,349.947  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
