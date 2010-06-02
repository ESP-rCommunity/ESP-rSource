*Geometry 1.1,GEN,office1_st5 # tag version, format, zone name
*date Mon Dec 22 13:59:04 2008  # latest file modification 
office1_st5 is the SW corner office of top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,16.00000  #   1
*vertex,6.00000,0.00000,16.00000  #   2
*vertex,6.00000,6.00000,16.00000  #   3
*vertex,0.00000,6.00000,16.00000  #   4
*vertex,0.00000,0.00000,20.00000  #   5
*vertex,6.00000,0.00000,20.00000  #   6
*vertex,6.00000,6.00000,20.00000  #   7
*vertex,0.00000,6.00000,20.00000  #   8
*vertex,1.10263,0.00000,16.73509  #   9
*vertex,4.89737,0.00000,16.73509  #  10
*vertex,4.89737,0.00000,19.26491  #  11
*vertex,1.10263,0.00000,19.26491  #  12
*vertex,0.00000,4.89737,16.73509  #  13
*vertex,0.00000,1.10263,16.73509  #  14
*vertex,0.00000,1.10263,19.26491  #  15
*vertex,0.00000,4.89737,19.26491  #  16
*vertex,1.65836,1.65836,20.00000  #  17
*vertex,4.34164,1.65836,20.00000  #  18
*vertex,4.34164,4.34164,20.00000  #  19
*vertex,1.65836,4.34164,20.00000  #  20
*vertex,1.20000,0.00000,16.80000  #  21
*vertex,4.80000,0.00000,16.80000  #  22
*vertex,4.80000,0.00000,19.20000  #  23
*vertex,1.20000,0.00000,19.20000  #  24
*vertex,0.00000,4.80000,16.80000  #  25
*vertex,0.00000,1.20000,16.80000  #  26
*vertex,0.00000,1.20000,19.20000  #  27
*vertex,0.00000,4.80000,19.20000  #  28
*vertex,1.87750,1.87750,20.00000  #  29
*vertex,4.12250,1.87750,20.00000  #  30
*vertex,4.12250,4.12250,20.00000  #  31
*vertex,1.87750,4.12250,20.00000  #  32
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,9,12,11,10,9  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,10,4,1,5,8,4,13,16,15,14,13  #  4
*edges,10,5,6,7,8,5,17,20,19,18,17  #  5
*edges,4,1,4,3,2  #  6
*edges,10,9,10,11,12,9,21,24,23,22,21  #  7
*edges,10,13,14,15,16,13,25,28,27,26,25  #  8
*edges,10,17,18,19,20,17,29,32,31,30,29  #  9
*edges,4,21,22,23,24  # 10
*edges,4,25,26,27,28  # 11
*edges,4,29,30,31,32  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ptn_of2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,58,04  #   2 ||< Wall-4:office2_st5
*surf,ptn_of8,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,64,08  #   3 ||< Wall-1:office8_st5
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,43,05  #   6 ||< ceiling:office1_st4
*surf,wins_frame,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,winw_frame,VERT,west,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,wins_glz,VERT,wins_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
*surf,winw_glz,VERT,winw_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  11 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    36.00 0  # zone base list
# 
*bridge_start,0.100,7.635,7.635,76.345  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
