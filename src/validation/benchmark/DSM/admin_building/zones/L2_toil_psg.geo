*Geometry 1.1,GEN,L2_toil_psg # tag version, format, zone name
*date Sat Feb 11 17:23:16 2012  # latest file modification 
L2_toil_psg describes L2 toilet and passage to stair_2
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,51.60000,23.17941,52.45000  #   1
*vertex,58.30000,23.08441,52.45000  #   2
*vertex,58.30000,28.59427,52.45000  #   3
*vertex,58.30000,29.50000,52.45000  #   4
*vertex,58.30000,30.10000,52.45000  #   5
*vertex,58.30000,30.50000,52.45000  #   6
*vertex,56.04857,30.50000,52.45000  #   7
*vertex,53.10365,30.50000,52.45000  #   8
*vertex,51.00000,28.49928,52.45000  #   9
*vertex,51.00000,23.17941,52.45000  #  10
*vertex,51.60000,23.17941,55.59300  #  11
*vertex,58.30000,23.08441,55.59300  #  12
*vertex,58.30000,28.59427,55.59300  #  13
*vertex,58.30000,29.50000,55.59300  #  14
*vertex,58.30000,30.10000,55.59300  #  15
*vertex,58.30000,30.50000,55.59300  #  16
*vertex,56.04857,30.50000,55.59300  #  17
*vertex,53.10365,30.50000,55.59300  #  18
*vertex,51.00000,28.49928,55.59300  #  19
*vertex,51.00000,23.17941,55.59300  #  20
*vertex,51.00000,30.50000,52.45000  #  21
*vertex,51.00000,30.50000,55.59300  #  22
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,12,11  #  1
*edges,4,2,3,13,12  #  2
*edges,4,3,4,14,13  #  3
*edges,4,4,5,15,14  #  4
*edges,4,5,6,16,15  #  5
*edges,4,6,7,17,16  #  6
*edges,4,21,9,19,22  #  7
*edges,4,9,10,20,19  #  8
*edges,4,10,1,11,20  #  9
*edges,11,11,12,13,14,15,16,17,18,22,19,20  # 10
*edges,11,1,10,9,21,8,7,6,5,4,3,2  # 11
*edges,6,8,21,22,18,17,7  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,pt_meeting,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,21,13  #   1 ||< ptn_toilet:L2_meeting
*surf,pta_fac_l2,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,18  #   2 ||< pta_fac_l2:ne_facade
*surf,ptb_fac_l2,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,19  #   3 ||< ptb_fac_l2:ne_facade
*surf,gl_facade_l2,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   4 ||< external
*surf,ptnc_fac_l2,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,20  #   5 ||< ptnc_fac_l2:ne_facade
*surf,ptna_str_l2,VERT,-,-,-,door,OPAQUE,ANOTHER,02,09  #   6 ||< ptna_str_l2:stair_2
*surf,ptn_cel_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,26,13  #   7 ||< ptn_cel_b:L2_core_cel
*surf,ptna_opn_l2,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,22,13  #   8 ||< ptn_toil_b:L2_open_pln
*surf,ptnb_opn_l2,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,22,12  #   9 ||< ptn_toil_a:L2_open_pln
*surf,top,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,50,27  #  10 ||< top:L3_ufloor
*surf,floor,FLOR,-,-,-,top_pln,OPAQUE,ANOTHER,17,59  #  11 ||< floor_tpsg:L2_uf_plen
*surf,ptnb_str_l2,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,10  #  12 ||< ptnb_str_l2:stair_2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   1 # list of surfs
  4
# 
*insol_calc,all_applicable   1 # insolation sources
  4
# 
*base_list,2,15,11,    53.76 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,59.105,29.490,52.440,0.620,0.800,0.010,90.000,0.00,gl_facads,stone_eldon  # block   1
*obs,59.105,29.490,55.593,0.620,0.800,0.010,90.000,0.00,gl_facadh,stone_eldon  # block   2
*obs,59.105,29.490,52.440,0.010,0.800,3.163,90.000,0.00,gl_facadl,stone_eldon  # block   3
*obs,59.105,30.100,52.440,0.010,0.800,3.163,90.000,0.00,gl_facadr,stone_eldon  # block   4
*end_block
