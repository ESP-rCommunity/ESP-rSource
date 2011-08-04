*Geometry 1.1,GEN,L1_toilet # tag version, format, zone name
*date Fri Aug  5 05:23:05 2011  # latest file modification 
L1_toilet describes toilets on level one
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,35.70000,47.95000  #   1
*vertex,8.35979,35.70000,47.95000  #   2
*vertex,12.00000,35.70000,47.95000  #   3
*vertex,12.00000,40.70000,47.95000  #   4
*vertex,7.83730,40.70000,47.95000  #   5
*vertex,3.56241,40.70000,47.95000  #   6
*vertex,0.00000,40.70000,47.95000  #   7
*vertex,0.00000,36.28000,47.95000  #   8
*vertex,0.00000,35.70000,51.69300  #   9
*vertex,8.35979,35.70000,51.69300  #  10
*vertex,12.00000,35.70000,51.69300  #  11
*vertex,12.00000,40.70000,51.69300  #  12
*vertex,7.83730,40.70000,51.69300  #  13
*vertex,3.56241,40.70000,51.69300  #  14
*vertex,0.00000,40.70000,51.69300  #  15
*vertex,0.00000,36.28000,51.69300  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,10,9  #  1
*edges,4,2,3,11,10  #  2
*edges,4,3,4,12,11  #  3
*edges,4,4,5,13,12  #  4
*edges,4,5,6,14,13  #  5
*edges,4,6,7,15,14  #  6
*edges,4,7,8,16,15  #  7
*edges,4,8,1,9,16  #  8
*edges,8,9,10,11,12,13,14,15,16  #  9
*edges,8,1,8,7,6,5,4,3,2  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_cora_l1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,11  #   1 ||< ptn_cora_l1:stair_1
*surf,ptn_corb_l1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,12  #   2 ||< ptn_corb_l1:stair_1
*surf,ptn_corc_l1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,14,19  #   3 ||< ptn_toilsw:L1_support
*surf,fac_a_l1,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,fac_b_l1,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,fac_c_l1,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,fac_d_l1,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ptn_vd_l1,VERT,-,-,-,stone_cast,OPAQUE,ANOTHER,15,13  #   8 ||< ptn_toilet:L1_sw_void
*surf,ceiling,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,17,30  #   9 ||< ceiling_toil:L2_uf_plen
*surf,floor,FLOR,-,-,-,foundation,OPAQUE,GROUND,01,00  #  10 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,    60.00 0  # zone base list
