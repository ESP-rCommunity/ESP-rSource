*Geometry 1.1,GEN,office9_st5 # tag version, format, zone name
*date Fri May  8 10:50:52 2009  # latest file modification 
office9_st5 is one core office 9 of top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,6.00000,16.00000  #   1
*vertex,44.00000,6.00000,16.00000  #   2
*vertex,44.00000,25.00000,16.00000  #   3
*vertex,35.00000,25.00000,16.00000  #   4
*vertex,35.00000,15.00000,16.00000  #   5
*vertex,15.00000,15.00000,16.00000  #   6
*vertex,15.00000,25.00000,16.00000  #   7
*vertex,6.00000,25.00000,16.00000  #   8
*vertex,6.00000,6.00000,20.00000  #   9
*vertex,44.00000,6.00000,20.00000  #  10
*vertex,44.00000,25.00000,20.00000  #  11
*vertex,35.00000,25.00000,20.00000  #  12
*vertex,35.00000,15.00000,20.00000  #  13
*vertex,15.00000,15.00000,20.00000  #  14
*vertex,15.00000,25.00000,20.00000  #  15
*vertex,6.00000,25.00000,20.00000  #  16
*vertex,15.00000,20.00000,16.00000  #  17
*vertex,15.00000,20.00000,20.00000  #  18
*vertex,35.00000,20.00000,16.00000  #  19
*vertex,35.00000,20.00000,20.00000  #  20
*vertex,6.00000,15.00000,20.00000  #  21
*vertex,15.00000,6.00000,20.00000  #  22
*vertex,35.00000,6.00000,20.00000  #  23
*vertex,44.00000,15.00000,20.00000  #  24
*vertex,16.50294,8.48754,20.00000  #  25
*vertex,33.49706,8.48754,20.00000  #  26
*vertex,33.49706,12.51246,20.00000  #  27
*vertex,16.50294,12.51246,20.00000  #  28
*vertex,8.48754,17.76393,20.00000  #  29
*vertex,12.51246,17.76393,20.00000  #  30
*vertex,12.51246,22.23607,20.00000  #  31
*vertex,8.48754,22.23607,20.00000  #  32
*vertex,37.48754,17.76393,20.00000  #  33
*vertex,41.51246,17.76393,20.00000  #  34
*vertex,41.51246,22.23607,20.00000  #  35
*vertex,37.48754,22.23607,20.00000  #  36
*vertex,17.89085,8.81626,20.00000  #  37
*vertex,32.10915,8.81626,20.00000  #  38
*vertex,32.10915,12.18374,20.00000  #  39
*vertex,17.89085,12.18374,20.00000  #  40
*vertex,8.81626,18.12917,20.00000  #  41
*vertex,12.18374,18.12917,20.00000  #  42
*vertex,12.18374,21.87083,20.00000  #  43
*vertex,8.81626,21.87083,20.00000  #  44
*vertex,37.81626,18.12917,20.00000  #  45
*vertex,41.18374,18.12917,20.00000  #  46
*vertex,41.18374,21.87083,20.00000  #  47
*vertex,37.81626,21.87083,20.00000  #  48
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,10,23,22,9  #  1
*edges,5,2,3,11,24,10  #  2
*edges,4,3,4,12,11  #  3
*edges,4,5,6,14,13  #  4
*edges,4,7,8,16,15  #  5
*edges,5,8,1,9,21,16  #  6
*edges,14,9,22,23,10,24,13,14,21,9,25,28,27,26,25  #  7
*edges,10,1,8,7,17,6,5,19,4,3,2  #  8
*edges,4,6,17,18,14  #  9
*edges,4,19,5,13,20  # 10
*edges,4,17,7,15,18  # 11
*edges,4,4,19,20,12  # 12
*edges,11,21,14,18,15,16,21,29,32,31,30,29  # 13
*edges,11,13,24,11,12,20,13,33,36,35,34,33  # 14
*edges,10,25,26,27,28,25,37,40,39,38,37  # 15
*edges,10,29,30,31,32,29,41,44,43,42,41  # 16
*edges,10,33,34,35,36,33,45,48,47,46,45  # 17
*edges,4,37,38,39,40  # 18
*edges,4,41,42,43,44  # 19
*edges,4,45,46,47,48  # 20
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_of2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,58,03  #   1 ||< ptn_of9:office2_st5
*surf,ptn_of4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,60,07  #   2 ||< ptn_of9:office4_st5
*surf,ptn_of10a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,70,03  #   3 ||< ptn_of9b:off10_st5
*surf,ptn_circ1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,65,01  #   4 ||< ptn_of9:circ1_st5
*surf,ptn_of10b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,70,01  #   5 ||< ptn_of9a:off10_st5
*surf,ptn_of8,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,64,06  #   6 ||< ptn_of9:office8_st5
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,55,07  #   8 ||< ceiling:office9_st4
*surf,ptn_circ1a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,65,03  #   9 ||< ptn_of9b:circ1_st5
*surf,ptn_circ1b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,65,02  #  10 ||< ptn_of9a:circ1_st5
*surf,ptn_m1a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,67,06  #  11 ||< ptn_of9:meetR1_st5
*surf,ptn_m2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,68,06  #  12 ||< ptn_of9:meetR2_st5
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
