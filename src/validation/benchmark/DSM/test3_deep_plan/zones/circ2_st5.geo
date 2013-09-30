*Geometry 1.1,GEN,circ2_st5 # tag version, format, zone name
*date Fri May  8 10:48:51 2009  # latest file modification 
circ2_st5 is other circulation space on top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,15.00000,30.00000,16.00000  #   1
*vertex,35.00000,30.00000,16.00000  #   2
*vertex,35.00000,35.00000,16.00000  #   3
*vertex,15.00000,35.00000,16.00000  #   4
*vertex,15.00000,30.00000,20.00000  #   5
*vertex,35.00000,30.00000,20.00000  #   6
*vertex,35.00000,35.00000,20.00000  #   7
*vertex,15.00000,35.00000,20.00000  #   8
*vertex,20.00000,30.00000,16.00000  #   9
*vertex,20.00000,30.00000,20.00000  #  10
*vertex,30.00000,30.00000,16.00000  #  11
*vertex,30.00000,30.00000,20.00000  #  12
*vertex,20.52786,31.38197,20.00000  #  13
*vertex,29.47214,31.38197,20.00000  #  14
*vertex,29.47214,33.61803,20.00000  #  15
*vertex,20.52786,33.61803,20.00000  #  16
*vertex,21.25834,31.56459,20.00000  #  17
*vertex,28.74166,31.56459,20.00000  #  18
*vertex,28.74166,33.43541,20.00000  #  19
*vertex,21.25834,33.43541,20.00000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,3,7,6  #  1
*edges,4,3,4,8,7  #  2
*edges,4,4,1,5,8  #  3
*edges,12,5,10,12,6,7,8,5,13,16,15,14,13  #  4
*edges,6,1,4,3,2,11,9  #  5
*edges,4,1,9,10,5  #  6
*edges,4,11,2,6,12  #  7
*edges,4,9,11,12,10  #  8
*edges,10,13,14,15,16,13,17,20,19,18,17  #  9
*edges,4,17,18,19,20  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_of10a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,70,10  #   1 ||< ptn_circ2c:off10_st5
*surf,ptn_of10b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,70,02  #   2 ||< ptn_circ2a:off10_st5
*surf,ptn_of10c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,70,09  #   3 ||< ptn_circ2b:off10_st5
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,52,04  #   5 ||< ceiling:circ2_st4
*surf,ptn_m1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,67,03  #   6 ||< ptn_circ2:meetR1_st5
*surf,ptn_m2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,68,02  #   7 ||< ptn_circ2:meetR2_st5
*surf,ptn_atr,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,71,20  #   8 ||< Wall-1bst5:atrium
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,   100.00 2  # zone base list
# 
*bridge_start,0.100,6.704,6.704,67.040  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
