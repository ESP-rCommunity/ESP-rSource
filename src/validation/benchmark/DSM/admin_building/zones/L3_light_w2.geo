*Geometry 1.1,GEN,L3_light_w2 # tag version, format, zone name
*date Sat Aug 13 15:01:29 2011  # latest file modification 
L3_light_w2 describes void under light well 2
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.25000,21.30000,56.35000  #   1
*vertex,38.00000,21.30000,56.35000  #   2
*vertex,38.00000,28.50000,56.35000  #   3
*vertex,34.25000,28.50000,56.35000  #   4
*vertex,34.25000,21.30000,59.49300  #   5
*vertex,38.00000,21.30000,59.49300  #   6
*vertex,38.00000,28.50000,59.49300  #   7
*vertex,34.25000,28.50000,59.49300  #   8
*vertex,34.25000,21.30000,57.35000  #   9
*vertex,38.00000,21.30000,57.35000  #  10
*vertex,38.00000,28.50000,57.35000  #  11
*vertex,34.25000,28.50000,57.35000  #  12
*vertex,34.25000,21.30000,55.59300  #  13
*vertex,38.00000,21.30000,55.59300  #  14
*vertex,38.00000,28.50000,55.59300  #  15
*vertex,34.25000,28.50000,55.59300  #  16
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
*surf,l3_lw2_fr,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,49,18  #   1 ||< l3_lw2_fr:L3_open_rig
*surf,l3_lw2_ri,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,49,19  #   2 ||< l3_lw2_ri:L3_open_rig
*surf,l3_lw2_bk,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,49,20  #   3 ||< l3_lw2_bk:L3_open_rig
*surf,l3_lw2_lef,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,48,46  #   4 ||< l3_lw2_left:L3_open_lft
*surf,up_l3_lw2,CEIL,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #   5 ||< not yet defined
*surf,l3_lw2_base,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,31,05  #   6 ||< up_l3_lw2:L2_light_w2
*surf,left_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,48,47  #   7 ||< lw2left_rail:L3_open_lft
*surf,front_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,16  #   8 ||< front_rail:L3_open_rig
*surf,right_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,17  #   9 ||< right_rail:L3_open_rig
*surf,l3_flrv_r,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,50,45  #  10 ||< l3_flrv_rr:L3_ufloor
*surf,l3_flrv_f,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,50,46  #  11 ||< l3_flrv_ff:L3_ufloor
*surf,l3_flrv_l,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,50,47  #  12 ||< l3_flrv_ll:L3_ufloor
*surf,l3_flrv_b,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,50,48  #  13 ||< l3_flrv_bb:L3_ufloor
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 0  # zone base list
