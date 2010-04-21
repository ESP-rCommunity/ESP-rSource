*Geometry 1.1,GEN,bed16_st3 # tag version, format, zone name
*date Thu Dec 11 17:19:16 2008  # latest file modification 
bed16_st3 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,28.00000,8.00000,7.00000  #   1
*vertex,30.00000,8.00000,7.00000  #   2
*vertex,30.00000,6.50000,7.00000  #   3
*vertex,31.50000,6.50000,7.00000  #   4
*vertex,31.50000,11.50000,7.00000  #   5
*vertex,28.00000,11.50000,7.00000  #   6
*vertex,28.00000,8.00000,10.50000  #   7
*vertex,30.00000,8.00000,10.50000  #   8
*vertex,30.00000,6.50000,10.50000  #   9
*vertex,31.50000,6.50000,10.50000  #  10
*vertex,31.50000,11.50000,10.50000  #  11
*vertex,28.00000,11.50000,10.50000  #  12
*vertex,30.70851,11.50000,7.79149  #  13
*vertex,28.79149,11.50000,7.79149  #  14
*vertex,28.79149,11.50000,9.70851  #  15
*vertex,30.70851,11.50000,9.70851  #  16
*vertex,31.50000,7.63069,7.79149  #  17
*vertex,31.50000,10.36931,7.79149  #  18
*vertex,31.50000,10.36931,9.70851  #  19
*vertex,31.50000,7.63069,9.70851  #  20
*vertex,29.00000,8.86000,10.50000  #  21
*vertex,30.50000,8.86000,10.50000  #  22
*vertex,30.50000,10.80000,10.50000  #  23
*vertex,29.00000,10.80000,10.50000  #  24
*vertex,30.65932,11.50000,7.84068  #  25
*vertex,28.84068,11.50000,7.84068  #  26
*vertex,28.84068,11.50000,9.65932  #  27
*vertex,30.65932,11.50000,9.65932  #  28
*vertex,31.50000,7.70096,7.84068  #  29
*vertex,31.50000,10.29904,7.84068  #  30
*vertex,31.50000,10.29904,9.65932  #  31
*vertex,31.50000,7.70096,9.65932  #  32
*vertex,29.12251,9.01844,10.50000  #  33
*vertex,30.37749,9.01844,10.50000  #  34
*vertex,30.37749,10.64156,10.50000  #  35
*vertex,29.12251,10.64156,10.50000  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,10,4,5,11,10,4,17,20,19,18,17  #  4
*edges,10,5,6,12,11,5,13,16,15,14,13  #  5
*edges,4,6,1,7,12  #  6
*edges,12,7,8,9,10,11,12,7,21,24,23,22,21  #  7
*edges,6,1,6,5,4,3,2  #  8
*edges,10,13,14,15,16,13,25,28,27,26,25  #  9
*edges,10,17,18,19,20,17,29,32,31,30,29  # 10
*edges,10,21,22,23,24,21,33,36,35,34,33  # 11
*edges,4,25,26,27,28  # 12
*edges,4,29,30,31,32  # 13
*edges,4,33,34,35,36  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,40,07  #   1 ||< ptn_e:bath15-16S3
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,40,02  #   2 ||< ptn_b:bath15-16S3
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,56,40  #   3 ||< ptn_bo:cirArea_st3
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,54,04  #   6 ||< ptn_d:bed15_st3
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,23,07  #   8 ||< ceiling:bed16_st2
*surf,winn_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,wine_frame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,winn_glz,VERT,winn_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
*surf,wine_glz,VERT,wine_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  13 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  14 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    14.50 0  # zone base list
# 
*bridge_start,0.100,3.659,3.659,36.591  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
