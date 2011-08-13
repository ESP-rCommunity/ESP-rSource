*Geometry 1.1,GEN,L4_light_w2 # tag version, format, zone name
*date Sat Aug 13 16:40:58 2011  # latest file modification 
L4_light_w2 describes light well two at level four
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.25000,21.30000,60.25000  #   1
*vertex,38.00000,21.30000,60.25000  #   2
*vertex,38.00000,28.50000,60.25000  #   3
*vertex,34.25000,28.50000,60.25000  #   4
*vertex,34.25000,21.30000,63.39300  #   5
*vertex,38.00000,21.30000,63.39300  #   6
*vertex,38.00000,28.50000,63.39300  #   7
*vertex,34.25000,28.50000,63.39300  #   8
*vertex,34.25000,21.30000,61.25000  #   9
*vertex,38.00000,21.30000,61.25000  #  10
*vertex,38.00000,28.50000,61.25000  #  11
*vertex,34.25000,28.50000,61.25000  #  12
*vertex,34.25000,21.30000,59.49300  #  13
*vertex,38.00000,21.30000,59.49300  #  14
*vertex,38.00000,28.50000,59.49300  #  15
*vertex,34.25000,28.50000,59.49300  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,10,6,5,9  #  1
*edges,4,11,7,6,10  #  2
*edges,6,3,4,12,8,7,11  #  3
*edges,4,9,5,8,12  #  4
*edges,4,5,6,7,8  #  5
*edges,4,13,16,15,14  #  6
*edges,4,4,1,9,12  #  7
*edges,4,1,2,10,9  #  8
*edges,4,2,3,11,10  #  9
*edges,4,14,15,3,2  # 10
*edges,4,13,14,2,1  # 11
*edges,4,16,13,1,4  # 12
*edges,4,15,16,4,3  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,l3_lw2_fr,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,59,28  #   1 ||< not yet defined
*surf,l3_lw2_ri,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,59,29  #   2 ||< not yet defined
*surf,l3_lw2_bk,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,59,30  #   3 ||< not yet defined
*surf,l3_lw2_lef,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,59,31  #   4 ||< not yet defined
*surf,up_l3_lw2,CEIL,-,-,-,fictitious,SC_fictit,UNKNOWN,49,06  #   5 ||< not yet defined
*surf,l3_lw2_base,FLOR,-,-,-,fictitious,SC_fictit,UNKNOWN,23,05  #   6 ||< not yet defined
*surf,left_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,59,25  #   7 ||< not yet defined
*surf,front_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,59,26  #   8 ||< not yet defined
*surf,right_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,59,27  #   9 ||< not yet defined
*surf,l3_flrv_r,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,l3_flrv_f,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,l3_flrv_l,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,l3_flrv_b,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  13 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 0  # zone base list
