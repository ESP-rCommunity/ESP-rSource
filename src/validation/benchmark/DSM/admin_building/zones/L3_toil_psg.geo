*Geometry 1.1,GEN,L3_toil_psg # tag version, format, zone name
*date Thu Aug 11 07:43:12 2011  # latest file modification 
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
*surf,pt_meeting,VERT,-,-,-,gyp_blk_ptn,OPAQUE,UNKNOWN,-,-  #   1 ||< not yet defined
*surf,pta_fac_l3,VERT,-,-,-,masny_at_st,OPAQUE,UNKNOWN,-,-  #   2 ||< not yet defined
*surf,ptb_fac_l3,VERT,-,-,-,masny_at_st,OPAQUE,UNKNOWN,-,-  #   3 ||< not yet defined
*surf,gl_facade_l3,VERT,-,-,-,db_lpasol,DSF4554_06nb,UNKNOWN,-,-  #   4 ||< not yet defined
*surf,ptnc_fac_l3,VERT,-,-,-,masny_at_st,OPAQUE,UNKNOWN,-,-  #   5 ||< not yet defined
*surf,ptna_str_l3,VERT,-,-,-,door,OPAQUE,UNKNOWN,-,-  #   6 ||< not yet defined
*surf,ptn_cel_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,ptna_opn_l3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,ptnb_opn_l3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,top,CEIL,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,floor,FLOR,-,-,-,flr_ov_pln,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,ptnb_str_l3,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,-,-  #  12 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,15,11,    53.76 0  # zone base list
