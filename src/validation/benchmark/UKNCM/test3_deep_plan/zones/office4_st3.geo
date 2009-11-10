*Geometry 1.1,GEN,office4_st3 # tag version, format, zone name
*date Tue Jan 20 15:53:57 2009  # latest file modification 
office4_st3 is east facing perimeter office on 3rd level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,44.00000,6.00000,8.00000  #   1
*vertex,50.00000,6.00000,8.00000  #   2
*vertex,50.00000,44.00000,8.00000  #   3
*vertex,44.00000,44.00000,8.00000  #   4
*vertex,44.00000,6.00000,12.00000  #   5
*vertex,50.00000,6.00000,12.00000  #   6
*vertex,50.00000,44.00000,12.00000  #   7
*vertex,44.00000,44.00000,12.00000  #   8
*vertex,50.00000,12.98335,8.73509  #   9
*vertex,50.00000,37.01665,8.73509  #  10
*vertex,50.00000,37.01665,11.26491  #  11
*vertex,50.00000,12.98335,11.26491  #  12
*vertex,44.00000,25.00000,8.00000  #  13
*vertex,44.00000,25.00000,12.00000  #  14
*vertex,50.00000,13.60001,8.80000  #  15
*vertex,50.00000,36.40000,8.80000  #  16
*vertex,50.00000,36.40000,11.20000  #  17
*vertex,50.00000,13.60001,11.20000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,10,2,3,7,6,2,9,12,11,10,9  #  2
*edges,4,3,4,8,7  #  3
*edges,5,5,6,7,8,14  #  4
*edges,5,1,13,4,3,2  #  5
*edges,10,9,10,11,12,9,15,18,17,16,15  #  6
*edges,4,13,1,5,14  #  7
*edges,4,4,13,14,8  #  8
*edges,4,15,16,17,18  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_of3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,31,03  #   1 ||< ptn_of4:office3_st3
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_of5,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,33,01  #   3 ||< ptn_of4:office5_st3
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,46,05  #   4 ||< floor:office4_st4
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,18,04  #   5 ||< ceiling:office4_st2
*surf,win_frame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,ptn_of9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,41,02  #   7 ||< ptn_of4:office9_st3
*surf,ptn_of10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,42,04  #   8 ||< ptn_of4:off10_st3
*surf,win_glz,VERT,win_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
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
*bridge_start,0.100,16.533,16.533,165.334  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
