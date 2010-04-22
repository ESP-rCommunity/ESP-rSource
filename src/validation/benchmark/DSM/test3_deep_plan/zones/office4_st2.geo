*Geometry 1.1,GEN,office4_st2 # tag version, format, zone name
*date Mon Dec 15 09:03:40 2008  # latest file modification 
office4_st2 is east facing perimeter on level 2
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,44.00000,6.00000,4.00000  #   1
*vertex,50.00000,6.00000,4.00000  #   2
*vertex,50.00000,44.00000,4.00000  #   3
*vertex,44.00000,44.00000,4.00000  #   4
*vertex,44.00000,6.00000,8.00000  #   5
*vertex,50.00000,6.00000,8.00000  #   6
*vertex,50.00000,44.00000,8.00000  #   7
*vertex,44.00000,44.00000,8.00000  #   8
*vertex,50.00000,12.98335,4.73509  #   9
*vertex,50.00000,37.01665,4.73509  #  10
*vertex,50.00000,37.01665,7.26491  #  11
*vertex,50.00000,12.98335,7.26491  #  12
*vertex,44.00000,25.00000,4.00000  #  13
*vertex,44.00000,25.00000,8.00000  #  14
*vertex,50.00000,13.60001,4.80000  #  15
*vertex,50.00000,36.40000,4.80000  #  16
*vertex,50.00000,36.40000,7.20000  #  17
*vertex,50.00000,13.60001,7.20000  #  18
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
*surf,ptn_o3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,17,03  #   1 ||< Wall-3:office3_st2
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_o5,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,19,01  #   3 ||< Wall-1:office5_st2
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,32,05  #   4 ||< Base-6:office4_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,04,04  #   5 ||< ceiling:office_4
*surf,win_frame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,ptn_o9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,27,02  #   7 ||< Wall-2:office9_st2
*surf,ptn_o10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,28,04  #   8 ||< Wall-6:off10_st2
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
