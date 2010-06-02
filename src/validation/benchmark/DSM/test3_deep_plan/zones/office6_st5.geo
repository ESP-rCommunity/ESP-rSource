*Geometry 1.1,GEN,office6_st5 # tag version, format, zone name
*date Mon Dec 22 14:26:27 2008  # latest file modification 
office6_st5 is the north perimeter office of top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,44.00000,16.00000  #   1
*vertex,44.00000,44.00000,16.00000  #   2
*vertex,44.00000,50.00000,16.00000  #   3
*vertex,6.00000,50.00000,16.00000  #   4
*vertex,6.00000,44.00000,20.00000  #   5
*vertex,44.00000,44.00000,20.00000  #   6
*vertex,44.00000,50.00000,20.00000  #   7
*vertex,6.00000,50.00000,20.00000  #   8
*vertex,37.01665,50.00000,16.73509  #   9
*vertex,12.98335,50.00000,16.73509  #  10
*vertex,12.98335,50.00000,19.26491  #  11
*vertex,37.01665,50.00000,19.26491  #  12
*vertex,16.50294,45.65836,20.00000  #  13
*vertex,33.49706,45.65836,20.00000  #  14
*vertex,33.49706,48.34164,20.00000  #  15
*vertex,16.50294,48.34164,20.00000  #  16
*vertex,36.39999,50.00000,16.80000  #  17
*vertex,13.60000,50.00000,16.80000  #  18
*vertex,13.60000,50.00000,19.20000  #  19
*vertex,36.39999,50.00000,19.20000  #  20
*vertex,17.89085,45.87750,20.00000  #  21
*vertex,32.10915,45.87750,20.00000  #  22
*vertex,32.10915,48.12250,20.00000  #  23
*vertex,17.89085,48.12250,20.00000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,10,3,4,8,7,3,9,12,11,10,9  #  3
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
*surf,ptn_of10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,70,05  #   1 ||< Wall-7:off10_st5
*surf,ptn_of5,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,61,04  #   2 ||< ptn_of6:office5_st5
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ptn_of7,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,63,02  #   4 ||< Wall-2:office7_st5
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,48,05  #   6 ||< ceiling:office6_st4
*surf,winn_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,winn_glz,VERT,winn_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
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
