*Geometry 1.1,GEN,ceil_void # tag version, format, zone name
*date Fri Jul 10 10:46:59 2009  # latest file modification 
ceil_void is a 0.5m ceiling void over offices
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,3.00000  #   1
*vertex,3.00000,0.00000,3.00000  #   2
*vertex,3.00000,4.50000,3.00000  #   3
*vertex,1.00000,4.50000,3.00000  #   4
*vertex,0.00000,4.50000,3.00000  #   5
*vertex,6.00000,0.00000,3.00000  #   6
*vertex,9.00000,0.00000,3.00000  #   7
*vertex,12.00000,0.00000,3.00000  #   8
*vertex,12.00000,3.00000,3.00000  #   9
*vertex,12.00000,6.00000,3.00000  #  10
*vertex,9.00000,6.00000,3.00000  #  11
*vertex,3.00000,6.00000,3.00000  #  12
*vertex,0.00000,6.00000,3.00000  #  13
*vertex,6.00000,13.50000,3.00000  #  14
*vertex,9.00000,12.00000,3.00000  #  15
*vertex,12.00000,12.00000,3.00000  #  16
*vertex,12.00000,15.00000,3.00000  #  17
*vertex,12.00000,18.00000,3.00000  #  18
*vertex,9.00000,18.00000,3.00000  #  19
*vertex,6.00000,18.00000,3.00000  #  20
*vertex,3.00000,18.00000,3.00000  #  21
*vertex,3.00000,13.50000,3.00000  #  22
*vertex,12.00000,9.00000,3.00000  #  23
*vertex,0.00000,0.00000,3.50000  #  24
*vertex,3.00000,0.00000,3.50000  #  25
*vertex,12.00000,0.00000,3.50000  #  26
*vertex,12.00000,6.00000,3.50000  #  27
*vertex,12.00000,12.00000,3.50000  #  28
*vertex,12.00000,18.00000,3.50000  #  29
*vertex,3.00000,18.00000,3.50000  #  30
*vertex,3.00000,13.50000,3.50000  #  31
*vertex,3.00000,6.00000,3.50000  #  32
*vertex,0.00000,6.00000,3.50000  #  33
# 
# tag, number of vertices followed by list of associated vert
*edges,5,2,1,5,4,3  #  1
*edges,12,4,5,13,12,11,10,9,8,7,6,2,3  #  2
*edges,9,15,14,22,21,20,19,18,17,16  #  3
*edges,8,11,12,22,14,15,16,23,10  #  4
*edges,10,24,25,26,27,28,29,30,31,32,33  #  5
*edges,8,1,2,6,7,8,26,25,24  #  6
*edges,11,8,9,10,23,16,17,18,29,28,27,26  #  7
*edges,6,18,19,20,21,30,29  #  8
*edges,6,21,22,12,32,31,30  #  9
*edges,4,12,13,33,32  # 10
*edges,5,13,5,1,24,33  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-6,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,01,05  #   1 ||< ceiling:manager
*surf,ceiling,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,02,07  #   2 ||< ceiling:general
*surf,ceil_conff,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,03,05  #   3 ||< ceiling:conference
*surf,ceil_rec,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,04,17  #   4 ||< ceiling:reception
*surf,structure,CEIL,-,-,-,susp_floor,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,south_edge,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,east_edge,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,north_edge,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,core_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,SIMILAR,00,00  #   9 ||< identical environment
*surf,core_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,01  #  10 ||< Surf-1:mixing_box
*surf,core_c,VERT,-,-,-,gyp_blk_ptn,OPAQUE,SIMILAR,00,00  #  11 ||< identical environment
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,4,1,2,3,4,   180.00 0  # zone base list
