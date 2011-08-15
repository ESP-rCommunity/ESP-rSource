*Geometry 1.1,GEN,L5_kitch2 # tag version, format, zone name
*date Mon Aug 15 17:01:40 2011  # latest file modification 
L5_kitch2 describes kitchen adjacent to conference level five
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.75000,17.75000,64.15000  #   1
*vertex,10.00000,17.75000,64.15000  #   2
*vertex,10.00000,21.25000,64.15000  #   3
*vertex,6.50000,21.25000,64.15000  #   4
*vertex,4.05000,21.25000,64.15000  #   5
*vertex,4.05000,24.00000,64.15000  #   6
*vertex,3.50000,24.00000,64.15000  #   7
*vertex,0.00000,24.00000,64.15000  #   8
*vertex,0.00000,21.25000,64.15000  #   9
*vertex,0.00000,17.75000,64.15000  #  10
*vertex,6.75000,17.75000,67.37500  #  11
*vertex,10.00000,17.75000,67.37500  #  12
*vertex,10.00000,21.25000,67.37500  #  13
*vertex,6.50000,21.25000,67.37500  #  14
*vertex,4.05000,21.25000,67.37500  #  15
*vertex,4.05000,24.00000,67.37500  #  16
*vertex,3.50000,24.00000,67.37500  #  17
*vertex,0.00000,24.00000,67.37500  #  18
*vertex,0.00000,21.25000,67.37500  #  19
*vertex,0.00000,17.75000,67.37500  #  20
*vertex,3.75000,17.75000,64.15000  #  21
*vertex,3.75000,17.75000,67.37500  #  22
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,12,11  #  1
*edges,4,2,3,13,12  #  2
*edges,4,3,4,14,13  #  3
*edges,4,4,5,15,14  #  4
*edges,4,5,6,16,15  #  5
*edges,4,6,7,17,16  #  6
*edges,4,7,8,18,17  #  7
*edges,4,8,9,19,18  #  8
*edges,4,9,10,20,19  #  9
*edges,11,11,12,13,14,15,16,17,18,19,20,22  # 10
*edges,11,1,21,10,9,8,7,6,5,4,3,2  # 11
*edges,4,21,1,11,22  # 12
*edges,4,10,21,22,20  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,pnt_cnf,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   1 ||< not yet defined
*surf,ptn_lby_a,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   2 ||< not yet defined
*surf,ptn_lby_b,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   3 ||< not yet defined
*surf,ptn_lby_c,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   4 ||< not yet defined
*surf,ptn_lby_d,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   5 ||< not yet defined
*surf,ptn_lby_e,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   6 ||< not yet defined
*surf,ptn_elev,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,ptn_open_a,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,ptn_open_b,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,roof,CEIL,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,floor_kitch2,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,ptn_kitb,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  12 ||< external
*surf,ptn_kitc,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,-,-  #  13 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,11,    46.14 0  # zone base list
