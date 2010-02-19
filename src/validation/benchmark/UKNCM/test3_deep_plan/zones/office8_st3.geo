*Geometry 1.1,GEN,office8_st3 # tag version, format, zone name
*date Tue Jan 20 15:55:39 2009  # latest file modification 
office8_st3 is the west facing perimeter office of 3rd level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,6.00000,8.00000  #   1
*vertex,6.00000,6.00000,8.00000  #   2
*vertex,6.00000,44.00000,8.00000  #   3
*vertex,0.00000,44.00000,8.00000  #   4
*vertex,0.00000,6.00000,12.00000  #   5
*vertex,6.00000,6.00000,12.00000  #   6
*vertex,6.00000,44.00000,12.00000  #   7
*vertex,0.00000,44.00000,12.00000  #   8
*vertex,0.00000,37.01665,8.73509  #   9
*vertex,0.00000,12.98335,8.73509  #  10
*vertex,0.00000,12.98335,11.26491  #  11
*vertex,0.00000,37.01665,11.26491  #  12
*vertex,6.00000,25.00000,8.00000  #  13
*vertex,6.00000,25.00000,12.00000  #  14
*vertex,0.00000,36.39999,8.80000  #  15
*vertex,0.00000,13.60000,8.80000  #  16
*vertex,0.00000,13.60000,11.20000  #  17
*vertex,0.00000,36.39999,11.20000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,4,3,4,8,7  #  1
*edges,10,4,1,5,8,4,9,12,11,10,9  #  2
*edges,5,5,6,14,7,8  #  3
*edges,5,1,4,3,13,2  #  4
*edges,10,9,10,11,12,9,15,18,17,16,15  #  5
*edges,4,2,13,14,6  #  6
*edges,4,13,3,7,14  #  7
*edges,4,1,2,6,5  #  8
*edges,4,15,16,17,18  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_of7,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,35,01  #   1 ||< ptn_of8:office7_st3
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,50,04  #   3 ||< floor:office8_st4
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,22,03  #   4 ||< ceiling:office8_st2
*surf,winw_frame,VERT,west,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_of9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,41,06  #   6 ||< ptn_of8:office9_st3
*surf,ptn_of10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,42,06  #   7 ||< ptn_of8:off10_st3
*surf,ptn_of1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,29,03  #   8 ||< ptn_of8:office1_st3
*surf,winw_glz,VERT,winw_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,4,   228.00 0  # zone base list
# 
*bridge_start,0.100,16.533,16.533,165.334  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
