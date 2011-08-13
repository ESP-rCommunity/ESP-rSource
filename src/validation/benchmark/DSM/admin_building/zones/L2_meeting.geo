*Geometry 1.1,GEN,L2_meeting # tag version, format, zone name
*date Sat Aug 13 14:47:08 2011  # latest file modification 
L2_meeting describes corner meeting room wutg angled wall
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,51.60000,23.17941,52.45000  #   1
*vertex,51.60000,20.42448,52.45000  #   2
*vertex,51.60000,19.23701,52.45000  #   3
*vertex,51.60000,18.19204,52.45000  #   4
*vertex,51.60000,17.28956,52.45000  #   5
*vertex,51.60000,17.00457,52.45000  #   6
*vertex,52.01118,17.14706,52.45000  #   7
*vertex,54.10112,17.66955,52.45000  #   8
*vertex,56.57106,18.23954,52.45000  #   9
*vertex,58.30000,18.66702,52.45000  #  10
*vertex,58.30000,19.90000,52.45000  #  11
*vertex,58.30000,20.50000,52.45000  #  12
*vertex,58.30000,23.08441,52.45000  #  13
*vertex,51.60000,23.17941,55.59300  #  14
*vertex,51.60000,20.42448,55.59300  #  15
*vertex,51.60000,19.23701,55.59300  #  16
*vertex,51.60000,18.19204,55.59300  #  17
*vertex,51.60000,17.28956,55.59300  #  18
*vertex,51.60000,17.00457,55.59300  #  19
*vertex,52.01118,17.14706,55.59300  #  20
*vertex,54.10112,17.66955,55.59300  #  21
*vertex,56.57106,18.23954,55.59300  #  22
*vertex,58.30000,18.66702,55.59300  #  23
*vertex,58.30000,19.90000,55.59300  #  24
*vertex,58.30000,20.50000,55.59300  #  25
*vertex,58.30000,23.08441,55.59300  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,15,14  #  1
*edges,4,2,3,16,15  #  2
*edges,4,3,4,17,16  #  3
*edges,4,4,5,18,17  #  4
*edges,4,5,6,19,18  #  5
*edges,4,6,7,20,19  #  6
*edges,4,7,8,21,20  #  7
*edges,4,8,9,22,21  #  8
*edges,4,9,10,23,22  #  9
*edges,4,10,11,24,23  # 10
*edges,4,11,12,25,24  # 11
*edges,4,12,13,26,25  # 12
*edges,4,13,1,14,26  # 13
*edges,13,14,15,16,17,18,19,20,21,22,23,24,25,26  # 14
*edges,13,1,13,12,11,10,9,8,7,6,5,4,3,2  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_open_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,22,11  #   1 ||< ptn_meet_a:L2_open_pln
*surf,ptn_open_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,22,10  #   2 ||< ptn_meet_b:L2_open_pln
*surf,door,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,22,09  #   3 ||< ptn_meet_c:L2_open_pln
*surf,ptn_open_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,22,08  #   4 ||< ptn_frame:L2_open_pln
*surf,frame_open,VERT,-,-,-,mull_90,OPAQUE,UNKNOWN,0,0  #   5 ||< not yet defined
*surf,fac_frame,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,fac_glaz_a,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   7 ||< external
*surf,fac_glaz_b,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   8 ||< external
*surf,fac_glaz_c,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,pa_fac_l2,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,14  #  10 ||< pa_fac_l2:ne_facade
*surf,gl_fac_l2,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  11 ||< external
*surf,pb_fac_l2,VERT,-,-,-,masny_at_st,OPAQUE,ANOTHER,05,15  #  12 ||< pb_fac_l2:ne_facade
*surf,ptn_toilet,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,33,01  #  13 ||< pt_meeting:L2_toil_psg
*surf,ceiling_mt,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,50,16  #  14 ||< ceiling_mt:L3_ufloor
*surf,floor_mt,FLOR,-,-,-,top_pln,OPAQUE,ANOTHER,17,47  #  15 ||< floor_mt:L2_uf_plen
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,15,    35.33 0  # zone base list
