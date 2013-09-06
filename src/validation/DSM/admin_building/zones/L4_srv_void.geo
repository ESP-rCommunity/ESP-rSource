*Geometry 1.1,GEN,L4_srv_void # tag version, format, zone name
*date Tue Jan 31 09:27:41 2012  # latest file modification 
L4_srv_void describes floor void at service facade
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,35.70000,60.25000  #   1
*vertex,0.00000,36.28000,60.25000  #   2
*vertex,0.00000,40.70000,60.25000  #   3
*vertex,12.00000,40.70000,60.25000  #   4
*vertex,12.00000,35.70000,60.25000  #   5
*vertex,24.08189,40.70000,60.25000  #   6
*vertex,24.03439,35.70000,60.25000  #   7
*vertex,27.69180,40.70000,60.25000  #   8
*vertex,51.00000,40.70000,60.25000  #   9
*vertex,51.00000,36.00000,60.25000  #  10
*vertex,50.00000,36.00000,60.25000  #  11
*vertex,50.00000,35.70000,60.25000  #  12
*vertex,12.00000,35.70000,59.49300  #  13
*vertex,27.50000,35.70000,60.25000  #  14
*vertex,50.00000,35.70000,59.49300  #  15
*vertex,27.69180,35.70000,59.49300  #  16
*vertex,24.03439,35.70000,59.49300  #  17
*vertex,50.00000,36.00000,59.49300  #  18
*vertex,51.00000,36.00000,59.49300  #  19
*vertex,51.00000,40.70000,59.49300  #  20
*vertex,27.69180,40.70000,59.49300  #  21
*vertex,24.08189,40.70000,59.49300  #  22
*vertex,12.00000,40.70000,59.49300  #  23
*vertex,0.00000,35.70000,59.49300  #  24
*vertex,0.00000,40.70000,59.49300  #  25
*vertex,0.00000,36.28000,59.49300  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,5,2,1,5,4,3  #  1
*edges,4,4,5,7,6  #  2
*edges,4,6,7,14,8  #  3
*edges,6,8,14,12,11,10,9  #  4
*edges,8,13,17,16,15,12,14,7,5  #  5
*edges,6,16,21,20,19,18,15  #  6
*edges,4,16,17,22,21  #  7
*edges,4,13,23,22,17  #  8
*edges,5,24,26,25,23,13  #  9
*edges,6,25,26,24,1,2,3  # 10
*edges,4,15,18,11,12  # 11
*edges,4,18,19,10,11  # 12
*edges,4,19,20,9,10  # 13
*edges,10,23,25,3,4,6,8,9,20,21,22  # 14
*edges,4,24,13,5,1  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,floor_l4toi,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,60,10  #   1 ||< floor:L4_toilet
*surf,Base-14,CEIL,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,62,14  #   2 ||< Base-14:L4_admin
*surf,floor_pz,CEIL,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,63,08  #   3 ||< floor_pz:L4_printz
*surf,Base-30,CEIL,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,54,27  #   4 ||< Base-30:L4_cell_srv
*surf,srv_facade,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,78,50  #   5 ||< srv_facade:L4_flrvoid
*surf,Top-29,FLOR,-,-,-,slab_275,OPAQUE,ANOTHER,40,26  #   6 ||< Top-29:L3_cell_srv
*surf,ceiling_pz,FLOR,-,-,-,slab_275,OPAQUE,ANOTHER,41,07  #   7 ||< ceiling_pz:L3_print_zn
*surf,Top-13,FLOR,-,-,-,slab_275,OPAQUE,ANOTHER,42,13  #   8 ||< Top-13:L3_social
*surf,ceiling,FLOR,-,-,-,slab_275,OPAQUE,ANOTHER,46,09  #   9 ||< ceiling:L3_toilet
*surf,right_end,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,fill_a,VERT,-,-,-,conc_250,OPAQUE,SIMILAR,00,00  #  11 ||< identical environment
*surf,fill_b,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,58  #  12 ||< l4_flrvdc:stair_2
*surf,left_facade,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,srv_edge,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,to_stair_1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,13  #  15 ||< l4_flrva:stair_1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,254.70,0  # zone base
