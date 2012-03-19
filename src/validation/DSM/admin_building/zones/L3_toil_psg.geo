*Geometry 1.1,GEN,L3_toil_psg # tag version, format, zone name
*date Sun Feb 12 08:13:49 2012  # latest file modification 
L3_toil_psg describes toilet and passage at level three
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,51.60000,23.17941,56.35000  #   1
*vertex,58.30000,23.08441,56.35000  #   2
*vertex,58.30000,28.59427,56.35000  #   3
*vertex,58.30000,29.50000,56.35000  #   4
*vertex,58.30000,30.10000,56.35000  #   5
*vertex,58.30000,30.50000,56.35000  #   6
*vertex,56.04857,30.50000,56.35000  #   7
*vertex,53.10365,30.50000,56.35000  #   8
*vertex,51.00000,28.49928,56.35000  #   9
*vertex,51.00000,23.17941,56.35000  #  10
*vertex,51.60000,23.17941,59.49300  #  11
*vertex,58.30000,23.08441,59.49300  #  12
*vertex,58.30000,28.59427,59.49300  #  13
*vertex,58.30000,29.50000,59.49300  #  14
*vertex,58.30000,30.10000,59.49300  #  15
*vertex,58.30000,30.50000,59.49300  #  16
*vertex,56.04857,30.50000,59.49300  #  17
*vertex,53.10365,30.50000,59.49300  #  18
*vertex,51.00000,28.49928,59.49300  #  19
*vertex,51.00000,23.17941,59.49300  #  20
*vertex,51.00000,30.50000,56.35000  #  21
*vertex,51.00000,30.50000,59.49300  #  22
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
*surf,pt_meeting,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,39,09  #   1 ||< ptn_toilet:L3_meeting
*surf,pta_fac_l3,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,28  #   2 ||< pta_fac_l3:ne_facade
*surf,ptb_fac_l3,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,29  #   3 ||< ptb_fac_l3:ne_facade
*surf,gl_facade_l3,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   4 ||< external
*surf,ptnc_fac_l3,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,30  #   5 ||< ptnc_fac_l3:ne_facade
*surf,ptna_str_l3,VERT,-,-,-,door,OPAQUE,ANOTHER,02,11  #   6 ||< ptna_str_l3:stair_2
*surf,ptn_cel_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,49,23  #   7 ||< ptn_cel_b:L3_open_rig
*surf,ptna_opn_l3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,49,21  #   8 ||< ptna_opn_l3:L3_open_rig
*surf,ptnb_opn_l3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,49,22  #   9 ||< ptnb_opn_l3:L3_open_rig
*surf,top,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,78,18  #  10 ||< top:L4_flrvoid
*surf,floor,FLOR,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,50,36  #  11 ||< floor_tpsg:L3_ufloor
*surf,ptnb_str_l3,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,12  #  12 ||< ptnb_str_l3:stair_2
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
*obs,59.105,29.490,56.340,0.620,0.800,0.010,90.000,0.00,gl_facads,stone_eldon  # block   1
*obs,59.105,29.490,59.493,0.620,0.800,0.010,90.000,0.00,gl_facadh,stone_eldon  # block   2
*obs,59.105,29.490,56.340,0.010,0.800,3.163,90.000,0.00,gl_facadl,stone_eldon  # block   3
*obs,59.105,30.100,56.340,0.010,0.800,3.163,90.000,0.00,gl_facadr,stone_eldon  # block   4
*end_block
