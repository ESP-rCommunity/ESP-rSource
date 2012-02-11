*Geometry 1.1,GEN,L4_toil_psg # tag version, format, zone name
*date Sat Feb 11 17:39:52 2012  # latest file modification 
L4_toil_psg describes toilet and passage on level four
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,51.60000,23.17941,60.25000  #   1
*vertex,58.30000,23.08441,60.25000  #   2
*vertex,58.30000,28.59427,60.25000  #   3
*vertex,58.30000,29.50000,60.25000  #   4
*vertex,58.30000,30.10000,60.25000  #   5
*vertex,58.30000,30.50000,60.25000  #   6
*vertex,56.04857,30.50000,60.25000  #   7
*vertex,53.10365,30.50000,60.25000  #   8
*vertex,51.00000,28.49928,60.25000  #   9
*vertex,51.00000,23.17941,60.25000  #  10
*vertex,51.60000,23.17941,63.39300  #  11
*vertex,58.30000,23.08441,63.39300  #  12
*vertex,58.30000,28.59427,63.39300  #  13
*vertex,58.30000,29.50000,63.39300  #  14
*vertex,58.30000,30.10000,63.39300  #  15
*vertex,58.30000,30.50000,63.39300  #  16
*vertex,56.04857,30.50000,63.39300  #  17
*vertex,53.10365,30.50000,63.39300  #  18
*vertex,51.00000,28.49928,63.39300  #  19
*vertex,51.00000,23.17941,63.39300  #  20
*vertex,51.00000,30.50000,60.25000  #  21
*vertex,51.00000,30.50000,63.39300  #  22
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,12,11  #  1
*edges,4,2,3,13,12  #  2
*edges,4,4,5,15,14  #  3
*edges,4,5,6,16,15  #  4
*edges,4,6,7,17,16  #  5
*edges,4,21,9,19,22  #  6
*edges,4,9,10,20,19  #  7
*edges,4,10,1,11,20  #  8
*edges,11,11,12,13,14,15,16,17,18,22,19,20  #  9
*edges,11,1,10,9,21,8,7,6,5,4,3,2  # 10
*edges,4,3,4,14,13  # 11
*edges,6,8,21,22,18,17,7  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,pt_meeting,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,54,09  #   1 ||< ptn_toilet:L4_meeting
*surf,pta_fac_l4,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,24  #   2 ||< pta_fac_l4:ne_facade
*surf,gl_facade_l4,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   3 ||< external
*surf,ptnc_fac_l4,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,27  #   4 ||< ptnc_fac_l4:ne_facade
*surf,ptna_str_l4,VERT,-,-,-,door,OPAQUE,ANOTHER,02,13  #   5 ||< ptna_str_l4:stair_2
*surf,ptn_cel_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,58,18  #   6 ||< ptn_cel_b:L4_open_plr
*surf,ptna_opn_l4,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,58,17  #   7 ||< ptna_opn_l4:L4_open_plr
*surf,ptnb_opn_l4,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,58,16  #   8 ||< ptnb_opn_l4:L4_open_plr
*surf,ceil_l4_tp,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,81,10  #   9 ||< top_l4_toilc:L5_uf_plen
*surf,floor,FLOR,-,-,-,top_pln,OPAQUE,ANOTHER,78,36  #  10 ||< flool4tp:L4_flrvoid
*surf,ptnb_fac_l4,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,25  #  11 ||< ptnb_fac_l4:ne_facade
*surf,ptnb_str_l4,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,14  #  12 ||< ptnb_str_l4:stair_2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   1 # list of surfs
  3
# 
*insol_calc,all_applicable   1 # insolation sources
  3
# 
*base_list,2,15,10,    53.76 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,59.105,29.490,60.240,0.620,0.800,0.010,90.000,0.00,gl_facads,stone_eldon  # block   1
*obs,59.105,29.490,63.393,0.620,0.800,0.010,90.000,0.00,gl_facadh,stone_eldon  # block   2
*obs,59.105,29.490,60.240,0.010,0.800,3.163,90.000,0.00,gl_facadl,stone_eldon  # block   3
*obs,59.105,30.100,60.240,0.010,0.800,3.163,90.000,0.00,gl_facadr,stone_eldon  # block   4
*end_block
