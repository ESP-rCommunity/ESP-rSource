*Geometry 1.1,GEN,office2_st5 # tag version, format, zone name
*date Mon Dec 22 14:04:19 2008  # latest file modification 
office2_st5 is south perimeter office on top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,0.00000,16.00000  #   1
*vertex,44.00000,0.00000,16.00000  #   2
*vertex,44.00000,6.00000,16.00000  #   3
*vertex,6.00000,6.00000,16.00000  #   4
*vertex,6.00000,0.00000,20.00000  #   5
*vertex,44.00000,0.00000,20.00000  #   6
*vertex,44.00000,6.00000,20.00000  #   7
*vertex,6.00000,6.00000,20.00000  #   8
*vertex,12.98335,0.00000,16.73509  #   9
*vertex,37.01665,0.00000,16.73509  #  10
*vertex,37.01665,0.00000,19.26491  #  11
*vertex,12.98335,0.00000,19.26491  #  12
*vertex,16.50294,1.65836,20.00000  #  13
*vertex,33.49706,1.65836,20.00000  #  14
*vertex,33.49706,4.34164,20.00000  #  15
*vertex,16.50294,4.34164,20.00000  #  16
*vertex,13.60001,0.00000,16.80000  #  17
*vertex,36.40000,0.00000,16.80000  #  18
*vertex,36.40000,0.00000,19.20000  #  19
*vertex,13.60001,0.00000,19.20000  #  20
*vertex,17.89085,1.87750,20.00000  #  21
*vertex,32.10915,1.87750,20.00000  #  22
*vertex,32.10915,4.12250,20.00000  #  23
*vertex,17.89085,4.12250,20.00000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,9,12,11,10,9  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,10,5,6,7,8,5,13,16,15,14,13  #  5
*edges,4,1,4,3,2  #  6
*edges,10,9,10,11,12,9,17,20,19,18,17  #  7
*edges,10,13,14,15,16,13,21,24,23,22,21  #  8
*edges,4,17,18,19,20  #  9
*edges,4,21,22,23,24  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ptn_of3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,59,04  #   2 ||< Wall-4:office3_st5
*surf,ptn_of9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,69,01  #   3 ||< Wall-1:office9_st5
*surf,ptn_of1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,57,02  #   4 ||< ptn_of2:office1_st5
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,44,05  #   6 ||< ceiling:office2_st4
*surf,wins_frame,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,wins_glz,VERT,wins_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   228.00 0  # zone base list
# 
*bridge_start,0.100,31.818,31.818,318.185  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
