*Geometry 1.1,GEN,meetR1_st5 # tag version, format, zone name
*date Fri May  8 10:49:36 2009  # latest file modification 
meetR1_st5 is meeting room one on top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,15.00000,20.00000,16.00000  #   1
*vertex,20.00000,20.00000,16.00000  #   2
*vertex,20.00000,30.00000,16.00000  #   3
*vertex,15.00000,30.00000,16.00000  #   4
*vertex,15.00000,20.00000,20.00000  #   5
*vertex,20.00000,20.00000,20.00000  #   6
*vertex,20.00000,30.00000,20.00000  #   7
*vertex,15.00000,30.00000,20.00000  #   8
*vertex,15.00000,25.00000,16.00000  #   9
*vertex,15.00000,25.00000,20.00000  #  10
*vertex,16.38197,22.76393,20.00000  #  11
*vertex,18.61803,22.76393,20.00000  #  12
*vertex,18.61803,27.23607,20.00000  #  13
*vertex,16.38197,27.23607,20.00000  #  14
*vertex,16.56459,23.12917,20.00000  #  15
*vertex,18.43541,23.12917,20.00000  #  16
*vertex,18.43541,26.87083,20.00000  #  17
*vertex,16.56459,26.87083,20.00000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,11,5,6,7,8,10,5,11,14,13,12,11  #  4
*edges,5,1,9,4,3,2  #  5
*edges,4,9,1,5,10  #  6
*edges,4,4,9,10,8  #  7
*edges,10,11,12,13,14,11,15,18,17,16,15  #  8
*edges,4,15,16,17,18  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_circ1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,65,06  #   1 ||< ptn_m1:circ1_st5
*surf,ptn_atrium,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,71,21  #   2 ||< Wall-2st5:atrium
*surf,ptn_circ2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,66,06  #   3 ||< ptn_m1:circ2_st5
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,53,04  #   5 ||< ceiling:meetR1_st4
*surf,ptn_of9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,69,11  #   6 ||< ptn_m1a:office9_st5
*surf,ptn_of10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,70,11  #   7 ||< ptn_m1:off10_st5
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    50.00 2  # zone base list
# 
*bridge_start,0.100,3.352,3.352,33.520  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
