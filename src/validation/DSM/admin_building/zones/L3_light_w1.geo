*Geometry 1.1,GEN,L3_lw1 # tag version, format, zone name
*date Sun Aug 21 11:49:04 2011  # latest file modification 
L3_lw1 describes light well one at level three
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,13.70000,21.30000,56.35000  #   1
*vertex,17.45000,21.30000,56.35000  #   2
*vertex,17.45000,28.50000,56.35000  #   3
*vertex,13.70000,28.50000,56.35000  #   4
*vertex,13.70000,21.30000,59.49300  #   5
*vertex,17.45000,21.30000,59.49300  #   6
*vertex,17.45000,28.50000,59.49300  #   7
*vertex,13.70000,28.50000,59.49300  #   8
*vertex,13.70000,21.30000,57.35000  #   9
*vertex,17.45000,21.30000,57.35000  #  10
*vertex,17.45000,28.50000,57.35000  #  11
*vertex,13.70000,28.50000,57.35000  #  12
*vertex,13.70000,21.30000,55.59300  #  13
*vertex,17.45000,21.30000,55.59300  #  14
*vertex,17.45000,28.50000,55.59300  #  15
*vertex,13.70000,28.50000,55.59300  #  16
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
*surf,l3_lw2_fr,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,48,39  #   1 ||< l3_lw2_fr:L3_open_lft
*surf,l3_lw2_ri,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,48,40  #   2 ||< l3_lw2_ri:L3_open_lft
*surf,l3_lw2_bk,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,48,41  #   3 ||< l3_lw2_bk:L3_open_lft
*surf,l3_lw2_lef,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,48,42  #   4 ||< l3_lw2_lef:L3_open_lft
*surf,up_l3_lw2,CEIL,-,-,-,fictitious,SC_fictit,ANOTHER,59,06  #   5 ||< l3_lw2_base:L4_light_w1
*surf,l3_lw2_base,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,23,34  #   6 ||< l3_lw2_base:L2_cust_srv
*surf,left_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,48,43  #   7 ||< left_rail:L3_open_lft
*surf,front_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,48,44  #   8 ||< front_rail:L3_open_lft
*surf,right_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,48,45  #   9 ||< right_rail:L3_open_lft
*surf,l3_flrv_r,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,50,32  #  10 ||< l3_flrv_r:L3_ufloor
*surf,l3_flrv_f,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,50,33  #  11 ||< l3_flrv_f:L3_ufloor
*surf,l3_flrv_l,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,50,34  #  12 ||< l3_flrv_l:L3_ufloor
*surf,l3_flrv_b,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,50,35  #  13 ||< l3_flrv_b:L3_ufloor
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 0  # zone base list
