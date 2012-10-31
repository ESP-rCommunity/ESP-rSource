*Geometry 1.1,GEN,L4_toilet # tag version, format, zone name
*date Tue Jan 31 09:01:30 2012  # latest file modification 
L4_toilet describes toilets on level four srv rd
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,35.70000,60.25000  #   1
*vertex,8.35979,35.70000,60.25000  #   2
*vertex,12.00000,35.70000,60.25000  #   3
*vertex,12.00000,40.70000,60.25000  #   4
*vertex,7.83730,40.70000,60.25000  #   5
*vertex,3.56241,40.70000,60.25000  #   6
*vertex,0.00000,40.70000,60.25000  #   7
*vertex,0.00000,36.28000,60.25000  #   8
*vertex,0.00000,35.70000,63.39300  #   9
*vertex,8.35979,35.70000,63.39300  #  10
*vertex,12.00000,35.70000,63.39300  #  11
*vertex,12.00000,40.70000,63.39300  #  12
*vertex,7.83730,40.70000,63.39300  #  13
*vertex,3.56241,40.70000,63.39300  #  14
*vertex,0.00000,40.70000,63.39300  #  15
*vertex,0.00000,36.28000,63.39300  #  16
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
*surf,ptn_cora_l3,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,09  #   1 ||< ptnb_toil_l4:stair_1
*surf,ptn_corb_l3,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,10  #   2 ||< ptna_toil_l4:stair_1
*surf,ptn_corc_l3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,63,12  #   3 ||< ptn_toilet:L4_admin
*surf,fac_a_l3,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,fac_b_l3,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,fac_c_l3,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,fac_d_l3,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ptn_vd_l3,VERT,-,-,-,stn_stair,OPAQUE,ANOTHER,51,15  #   8 ||< Wall-15:L4_sw_void
*surf,ceiling,CEIL,-,ROOF,FLAT,roof_0.2,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,floor,FLOR,-,-,-,slab_275,OPAQUE,ANOTHER,80,01  #  10 ||< floor_l4toi:L4_srv_void
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,    60.00 0  # zone base list
