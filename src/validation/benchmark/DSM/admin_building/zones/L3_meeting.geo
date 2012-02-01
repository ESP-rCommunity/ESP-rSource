*Geometry 1.1,GEN,L3_meeting # tag version, format, zone name
*date Mon Jan 30 17:55:58 2012  # latest file modification 
L3_meeting describes corner meeting room in L3
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,51.60000,17.00457,56.35000  #   1
*vertex,52.01118,17.14706,56.35000  #   2
*vertex,52.53366,17.28956,56.35000  #   3
*vertex,54.10112,17.66955,56.35000  #   4
*vertex,56.57106,18.23954,56.35000  #   5
*vertex,58.30000,18.66702,56.35000  #   6
*vertex,58.30000,19.90000,56.35000  #   7
*vertex,58.30000,20.50000,56.35000  #   8
*vertex,58.30000,23.08441,56.35000  #   9
*vertex,51.60000,23.17941,56.35000  #  10
*vertex,51.60000,20.42448,56.35000  #  11
*vertex,51.60000,19.23701,56.35000  #  12
*vertex,51.60000,18.19204,56.35000  #  13
*vertex,51.60000,17.28956,56.35000  #  14
*vertex,51.60000,17.00457,59.49300  #  15
*vertex,52.01118,17.14706,59.49300  #  16
*vertex,52.53366,17.28956,59.49300  #  17
*vertex,54.10112,17.66955,59.49300  #  18
*vertex,56.57106,18.23954,59.49300  #  19
*vertex,58.30000,18.66702,59.49300  #  20
*vertex,58.30000,19.90000,59.49300  #  21
*vertex,58.30000,20.50000,59.49300  #  22
*vertex,58.30000,23.08441,59.49300  #  23
*vertex,51.60000,23.17941,59.49300  #  24
*vertex,51.60000,20.42448,59.49300  #  25
*vertex,51.60000,19.23701,59.49300  #  26
*vertex,51.60000,18.19204,59.49300  #  27
*vertex,51.60000,17.28956,59.49300  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,16,15  #  1
*edges,4,2,3,17,16  #  2
*edges,4,3,4,18,17  #  3
*edges,4,4,5,19,18  #  4
*edges,4,5,6,20,19  #  5
*edges,4,6,7,21,20  #  6
*edges,4,7,8,22,21  #  7
*edges,4,8,9,23,22  #  8
*edges,4,9,10,24,23  #  9
*edges,4,10,11,25,24  # 10
*edges,4,11,12,26,25  # 11
*edges,4,12,13,27,26  # 12
*edges,4,13,14,28,27  # 13
*edges,4,14,1,15,28  # 14
*edges,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28  # 15
*edges,14,1,14,13,12,11,10,9,8,7,6,5,4,3,2  # 16
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,fac_frame,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,fac_glaz_a,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   2 ||< external
*surf,fac_glaz_b,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   3 ||< external
*surf,fac_glaz_c,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   4 ||< external
*surf,fac_glaz_d,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   5 ||< external
*surf,pa_fac_l3,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,16  #   6 ||< pa_fac_l3:ne_facade
*surf,gl_fac_l3,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   7 ||< external
*surf,pb_fac_l3,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,17  #   8 ||< pb_fac_l3:ne_facade
*surf,ptn_toilet,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,45,01  #   9 ||< pt_meeting:L3_toil_psg
*surf,ptn_open_a,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,49,07  #  10 ||< ptn_open_a:L3_open_rig
*surf,ptn_open_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,08  #  11 ||< ptn_open_b:L3_open_rig
*surf,door,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,09  #  12 ||< door:L3_open_rig
*surf,ptn_open_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,10  #  13 ||< ptn_open_c:L3_open_rig
*surf,ptn_frame,VERT,-,-,-,mull_90,OPAQUE,ANOTHER,38,08  #  14 ||< side_fr:L3_void_b
*surf,ceiling,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,78,17  #  15 ||< ceiling:L4_flrvoid
*surf,floor,FLOR,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,50,28  #  16 ||< floor:L3_ufloor
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,16,    35.32 0  # zone base list
