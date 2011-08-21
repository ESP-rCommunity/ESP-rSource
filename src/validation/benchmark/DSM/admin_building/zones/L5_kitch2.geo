*Geometry 1.1,GEN,L5_kitch2 # tag version, format, zone name
*date Sun Aug 21 11:13:02 2011  # latest file modification 
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
*surf,pnt_cnf,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,65,09  #   1 ||< ptn_kita:L5_confer
*surf,ptn_lby_a,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,67,12  #   2 ||< ptn_k1:L5_lobby
*surf,ptn_lby_b,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,67,11  #   3 ||< ptn_k1a:L5_lobby
*surf,ptn_lby_c,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,67,10  #   4 ||< ptn_k1b:L5_lobby
*surf,ptn_lby_d,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,67,09  #   5 ||< ptn_k1_c:L5_lobby
*surf,ptn_lby_e,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,67,08  #   6 ||< ptn_k1_d:L5_lobby
*surf,ptn_elev,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,17  #   7 ||< left_5:lifts
*surf,ptn_open_a,VERT,-,-,-,glaz_ptn,OPAQUE,ANOTHER,77,11  #   8 ||< gptn_g:L5_sw_void
*surf,ptn_open_b,VERT,-,-,-,glaz_ptn,OPAQUE,ANOTHER,77,10  #   9 ||< gptn_f:L5_sw_void
*surf,roof,CEIL,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,floor_kitch2,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,ptn_kitb,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,65,10  #  12 ||< ptn_kitb:L5_confer
*surf,ptn_kitc,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,65,11  #  13 ||< ptn_kitc:L5_confer
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,11,    46.14 0  # zone base list
