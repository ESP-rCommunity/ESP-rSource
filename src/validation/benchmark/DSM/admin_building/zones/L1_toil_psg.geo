*Geometry 1.1,GEN,L1_toil_psg # tag version, format, zone name
*date Sat Aug  6 12:27:50 2011  # latest file modification 
L1_toil_psg describes toilet and passage on level one
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,51.60000,23.17941,47.95000  #   1
*vertex,58.30000,23.08441,47.95000  #   2
*vertex,58.30000,28.59427,47.95000  #   3
*vertex,58.30000,29.50000,47.95000  #   4
*vertex,58.30000,30.10000,47.95000  #   5
*vertex,58.30000,30.50000,47.95000  #   6
*vertex,56.04857,30.50000,47.95000  #   7
*vertex,53.10365,30.50000,47.95000  #   8
*vertex,51.00000,28.49928,47.95000  #   9
*vertex,51.00000,23.17941,47.95000  #  10
*vertex,51.60000,23.17941,51.69300  #  11
*vertex,58.30000,23.08441,51.69300  #  12
*vertex,58.30000,28.59427,51.69300  #  13
*vertex,58.30000,29.50000,51.69300  #  14
*vertex,58.30000,30.10000,51.69300  #  15
*vertex,58.30000,30.50000,51.69300  #  16
*vertex,56.04857,30.50000,51.69300  #  17
*vertex,53.10365,30.50000,51.69300  #  18
*vertex,51.00000,28.49928,51.69300  #  19
*vertex,51.00000,23.17941,51.69300  #  20
*vertex,51.00000,30.50000,47.95000  #  21
*vertex,51.00000,30.50000,51.69300  #  22
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
*surf,pt_meeting,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,09,13  #   1 ||< ptn_toilet:L1_meeting
*surf,faca_l1,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,facb_l1,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,gl_facade,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   4 ||< external
*surf,facc_l1,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptna_str_l1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,07  #   6 ||< ptna_str_l1:stair_2
*surf,ptn_cel_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,24  #   7 ||< ptn_toil:L1_support
*surf,ptna_opn_l1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,21  #   8 ||< ptna_opn_l1:L1_open_off
*surf,ptnb_opn_l1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,22  #   9 ||< ptnb_opn_l1:L1_open_off
*surf,top,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,17,31  #  10 ||< ceil_tpsg:L2_uf_plen
*surf,floor,FLOR,-,-,-,foundation,OPAQUE,GROUND,01,00  #  11 ||< ground profile  1
*surf,ptnb_str_l1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,08  #  12 ||< ptnb_str_l1:stair_2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,15,11,    53.76 0  # zone base list
