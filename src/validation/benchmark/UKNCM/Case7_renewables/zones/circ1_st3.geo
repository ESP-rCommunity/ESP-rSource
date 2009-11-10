*Geometry 1.1,GEN,circ1_st3 # tag version, format, zone name
*date Fri May  8 11:25:21 2009  # latest file modification 
circ1_st3 is circulation in level 3 adjacent to office_1
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,6.00000,7.00000  #   1
*vertex,10.50000,6.00000,7.00000  #   2
*vertex,10.50000,12.00000,7.00000  #   3
*vertex,6.00000,12.00000,7.00000  #   4
*vertex,6.00000,6.00000,10.50000  #   5
*vertex,10.50000,6.00000,10.50000  #   6
*vertex,10.50000,12.00000,10.50000  #   7
*vertex,6.00000,12.00000,10.50000  #   8
*vertex,7.24377,7.65836,10.50000  #   9
*vertex,9.25623,7.65836,10.50000  #  10
*vertex,9.25623,10.34164,10.50000  #  11
*vertex,7.24377,10.34164,10.50000  #  12
*vertex,7.40813,7.87750,10.50000  #  13
*vertex,9.09187,7.87750,10.50000  #  14
*vertex,9.09187,10.12250,10.50000  #  15
*vertex,7.40813,10.12250,10.50000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,10,5,6,7,8,5,9,12,11,10,9  #  5
*edges,4,1,4,3,2  #  6
*edges,10,9,10,11,12,9,13,16,15,14,13  #  7
*edges,4,13,14,15,16  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,16,06  #   1 ||< Wall-3a:off2_st3
*surf,Wall-2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,21,04  #   2 ||< Wall-4:off5_st3
*surf,Wall-3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,18,06  #   3 ||< Wall-1a:off4_st3
*surf,Wall-4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,15,07  #   4 ||< Wall-2b:off1_st3
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,12,05  #   6 ||< ceiling:circ1_st2
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   8 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 2  # zone base list
# 
*bridge_start,0.100,1.810,1.810,18.101  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
