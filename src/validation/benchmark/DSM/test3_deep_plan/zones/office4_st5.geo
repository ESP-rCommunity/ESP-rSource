*Geometry 1.1,GEN,office4_st5 # tag version, format, zone name
*date Mon Dec 22 14:15:26 2008  # latest file modification 
office4_st5 is the east facing perimeter office on top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,44.00000,6.00000,16.00000  #   1
*vertex,50.00000,6.00000,16.00000  #   2
*vertex,50.00000,44.00000,16.00000  #   3
*vertex,44.00000,44.00000,16.00000  #   4
*vertex,44.00000,6.00000,20.00000  #   5
*vertex,50.00000,6.00000,20.00000  #   6
*vertex,50.00000,44.00000,20.00000  #   7
*vertex,44.00000,44.00000,20.00000  #   8
*vertex,50.00000,12.98335,16.73509  #   9
*vertex,50.00000,37.01665,16.73509  #  10
*vertex,50.00000,37.01665,19.26491  #  11
*vertex,50.00000,12.98335,19.26491  #  12
*vertex,44.00000,25.00000,16.00000  #  13
*vertex,44.00000,25.00000,20.00000  #  14
*vertex,45.65836,16.50294,20.00000  #  15
*vertex,48.34164,16.50294,20.00000  #  16
*vertex,48.34164,33.49706,20.00000  #  17
*vertex,45.65836,33.49706,20.00000  #  18
*vertex,50.00000,13.60001,16.80000  #  19
*vertex,50.00000,36.40000,16.80000  #  20
*vertex,50.00000,36.40000,19.20000  #  21
*vertex,50.00000,13.60001,19.20000  #  22
*vertex,45.87750,17.89085,20.00000  #  23
*vertex,48.12250,17.89085,20.00000  #  24
*vertex,48.12250,32.10915,20.00000  #  25
*vertex,45.87750,32.10915,20.00000  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,10,2,3,7,6,2,9,12,11,10,9  #  2
*edges,4,3,4,8,7  #  3
*edges,11,5,6,7,8,14,5,15,18,17,16,15  #  4
*edges,5,1,13,4,3,2  #  5
*edges,10,9,10,11,12,9,19,22,21,20,19  #  6
*edges,4,13,1,5,14  #  7
*edges,4,4,13,14,8  #  8
*edges,10,15,16,17,18,15,23,26,25,24,23  #  9
*edges,4,19,20,21,22  # 10
*edges,4,23,24,25,26  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_of3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,59,03  #   1 ||< ptn_of4:office3_st5
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_of5,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,61,01  #   3 ||< Wall-1:office5_st5
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,46,04  #   5 ||< ceiling:office4_st4
*surf,wine_frame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,ptn_of9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,69,02  #   7 ||< Wall-2:office9_st5
*surf,ptn_of10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,70,04  #   8 ||< Wall-6:off10_st5
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,wine_glz,VERT,wine_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  11 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,   228.00 0  # zone base list
# 
*bridge_start,0.100,31.818,31.818,318.185  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
