*Geometry 1.1,GEN,livdinkit # tag version, format, zone name
*date Tue Jun 11 10:48:00 2013  # latest file modification 
livdinkit is an open plan living dining kitchen
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,1.20000,0.00000,0.00000  #   1
*vertex,7.50000,0.00000,0.00000  #   2
*vertex,7.50000,3.30000,0.00000  #   3
*vertex,5.70000,3.30000,0.00000  #   4
*vertex,5.70000,4.30000,0.00000  #   5
*vertex,4.70000,4.30000,0.00000  #   6
*vertex,4.70000,3.30000,0.00000  #   7
*vertex,1.20000,3.30000,0.00000  #   8
*vertex,1.20000,2.10000,0.00000  #   9
*vertex,1.20000,0.30000,0.00000  #  10
*vertex,1.20000,0.00000,2.50000  #  11
*vertex,7.50000,0.00000,2.50000  #  12
*vertex,7.50000,3.30000,2.50000  #  13
*vertex,5.70000,3.30000,2.50000  #  14
*vertex,5.70000,4.30000,2.50000  #  15
*vertex,4.70000,4.30000,2.50000  #  16
*vertex,4.70000,3.30000,2.50000  #  17
*vertex,1.20000,3.30000,2.50000  #  18
*vertex,1.20000,2.10000,2.50000  #  19
*vertex,1.20000,0.30000,2.50000  #  20
*vertex,1.20000,2.00000,0.10000  #  21
*vertex,1.20000,0.40000,0.10000  #  22
*vertex,1.20000,0.40000,2.10000  #  23
*vertex,1.20000,2.00000,2.10000  #  24
*vertex,1.20000,3.20000,0.90000  #  25
*vertex,1.20000,2.20000,0.90000  #  26
*vertex,1.20000,2.20000,2.10000  #  27
*vertex,1.20000,3.20000,2.10000  #  28
*vertex,5.60000,4.30000,0.00000  #  29
*vertex,4.80000,4.30000,0.00000  #  30
*vertex,4.80000,4.30000,2.10000  #  31
*vertex,5.60000,4.30000,2.10000  #  32
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,12,11  #  1
*edges,4,2,3,13,12  #  2
*edges,4,3,4,14,13  #  3
*edges,4,4,5,15,14  #  4
*edges,8,5,29,32,31,30,6,16,15  #  5
*edges,4,6,7,17,16  #  6
*edges,4,7,8,18,17  #  7
*edges,10,8,9,19,18,8,25,28,27,26,25  #  8
*edges,10,9,10,20,19,9,21,24,23,22,21  #  9
*edges,4,10,1,11,20  # 10
*edges,10,11,12,13,14,15,16,17,18,19,20  # 11
*edges,12,1,10,9,8,7,6,30,29,5,4,3,2  # 12
*edges,4,21,22,23,24  # 13
*edges,4,25,26,27,28  # 14
*edges,4,29,30,31,32  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,right_facade,VERT,-,-,-,Wall_EW_1990,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,other,VERT,-,-,-,gyp_blk_ptn,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,ptn_bath_a,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,06  #   3 ||< ptn_liv_e:br12bthhall
*surf,ptn_bath_b,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,05  #   4 ||< ptn_liv_d:br12bthhall
*surf,ptn_hall,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,04  #   5 ||< ptn_liv_c:br12bthhall
*surf,ptn_br_a,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,03  #   6 ||< ptn_liv_b:br12bthhall
*surf,ptn_br_b,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,02  #   7 ||< ptn_liv_a:br12bthhall
*surf,fr_facade_a,VERT,-,-,-,Wall_EW_1990,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,fr_facade_b,VERT,-,-,-,Doorfrm_ext,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,fr_facade_c,VERT,-,-,-,Wall_EW_1990,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,SIMILAR,00,00  #  11 ||< identical environment
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #  12 ||< identical environment
*surf,operable,VERT,fr_facade_b,C-WINDOW,CLOSED,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  13 ||< external
*surf,fixed,VERT,fr_facade_a,C-WINDOW,CLOSED,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  14 ||< external
*surf,door_nonliv,VERT,ptn_hall,DOOR,CLOSED,door,OPAQUE,ANOTHER,02,19  #  15 ||< door_liv:br12bthhall
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   6 # list of surfs
  1  8  9 10 13 14
# 
*insol_calc,all_applicable   2 # insolation sources
 13 14
# 
*base_list,0,21.79,0  # zone base
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,0.000,0.000,-0.200,1.200,3.300,0.200,0.000,1.00,balc_base,extern_wall  # block   1
*obs,0.000,0.000,2.500,1.200,3.300,0.200,0.000,1.00,balc_top,extern_wall  # block   2
*obs,0.000,-0.200,-0.200,1.200,0.200,2.900,0.000,1.00,corner,extern_wall  # block   3
*end_block
