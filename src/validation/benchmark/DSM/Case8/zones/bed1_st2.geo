*Geometry 1.1,GEN,bed1_st2 # tag version, format, zone name
*date Thu May  7 11:34:31 2009  # latest file modification 
bed1_st2 describes a guest room one on 2nd level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.50000,0.00000,3.50000  #   1
*vertex,7.00000,0.00000,3.50000  #   2
*vertex,7.00000,3.50000,3.50000  #   3
*vertex,5.00000,3.50000,3.50000  #   4
*vertex,5.00000,5.00000,3.50000  #   5
*vertex,3.50000,5.00000,3.50000  #   6
*vertex,3.50000,0.00000,7.00000  #   7
*vertex,7.00000,0.00000,7.00000  #   8
*vertex,7.00000,3.50000,7.00000  #   9
*vertex,5.00000,3.50000,7.00000  #  10
*vertex,5.00000,5.00000,7.00000  #  11
*vertex,3.50000,5.00000,7.00000  #  12
*vertex,4.29149,0.00000,4.29149  #  13
*vertex,6.20851,0.00000,4.29149  #  14
*vertex,6.20851,0.00000,6.20851  #  15
*vertex,4.29149,0.00000,6.20851  #  16
*vertex,4.34068,0.00000,4.34068  #  17
*vertex,6.15932,0.00000,4.34068  #  18
*vertex,6.15932,0.00000,6.15932  #  19
*vertex,4.34068,0.00000,6.15932  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,8,7,1,13,16,15,14,13  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,1,6,5,4,3,2  #  8
*edges,10,13,14,15,16,13,17,20,19,18,17  #  9
*edges,4,17,18,19,20  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,09,06  #   2 ||< ptn_e:bed2_st2
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,24,06  #   3 ||< ptn_e:bath1-2
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,24,03  #   4 ||< ptn_c:bath1-2
*surf,ptn_d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,32,17  #   5 ||< ptn_ba:cirArea_St2
*surf,ptn_e,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,32,02  #   6 ||< ptn_a:cirArea_St2
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,42,08  #   7 ||< floor:bed1_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,06,14  #   8 ||< Base-8:circ_area
*surf,Win-1_frame,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,win_glz,VERT,Win-1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,14.50,1  # zone base
# 
*bridge_start,0.100,1.106,1.106,11.056  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
