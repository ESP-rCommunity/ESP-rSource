*Geometry 1.1,GEN,office5_st4 # tag version, format, zone name
*date Fri Dec 19 08:48:35 2008  # latest file modification 
office5_st4 is the NE corner of the 4th level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,44.00000,44.00000,12.00000  #   1
*vertex,50.00000,44.00000,12.00000  #   2
*vertex,50.00000,50.00000,12.00000  #   3
*vertex,44.00000,50.00000,12.00000  #   4
*vertex,44.00000,44.00000,16.00000  #   5
*vertex,50.00000,44.00000,16.00000  #   6
*vertex,50.00000,50.00000,16.00000  #   7
*vertex,44.00000,50.00000,16.00000  #   8
*vertex,50.00000,45.10263,12.73509  #   9
*vertex,50.00000,48.89737,12.73509  #  10
*vertex,50.00000,48.89737,15.26491  #  11
*vertex,50.00000,45.10263,15.26491  #  12
*vertex,48.89737,50.00000,12.73509  #  13
*vertex,45.10263,50.00000,12.73509  #  14
*vertex,45.10263,50.00000,15.26491  #  15
*vertex,48.89737,50.00000,15.26491  #  16
*vertex,50.00000,45.20000,12.80000  #  17
*vertex,50.00000,48.80000,12.80000  #  18
*vertex,50.00000,48.80000,15.20000  #  19
*vertex,50.00000,45.20000,15.20000  #  20
*vertex,48.80000,50.00000,12.80000  #  21
*vertex,45.20000,50.00000,12.80000  #  22
*vertex,45.20000,50.00000,15.20000  #  23
*vertex,48.80000,50.00000,15.20000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,10,2,3,7,6,2,9,12,11,10,9  #  2
*edges,10,3,4,8,7,3,13,16,15,14,13  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
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
*surf,ptn_of4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,46,03  #   1 ||< ptn_of5:office4_st4
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ptn_of6,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,48,02  #   4 ||< Wall-2:office6_st4
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,61,06  #   5 ||< Base-6:office5_st5
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,33,05  #   6 ||< ceiling:office5_st3
*surf,wine_frame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,winn_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,wine_glz,VERT,wine_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
*surf,winn_glz,VERT,winn_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
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
*bridge_start,0.100,5.221,5.221,52.211  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
