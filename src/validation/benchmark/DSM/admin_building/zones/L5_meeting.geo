*Geometry 1.1,GEN,L5_meeting # tag version, format, zone name
*date Thu Jul 14 19:46:37 2011  # latest file modification 
L5_meeting describes corner meeting room on top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,51.60000,17.00457,64.15000  #   1
*vertex,52.01118,17.14706,64.15000  #   2
*vertex,52.53366,17.28956,64.15000  #   3
*vertex,54.10112,17.66955,64.15000  #   4
*vertex,56.57106,18.23954,64.15000  #   5
*vertex,58.30000,18.66702,64.15000  #   6
*vertex,58.30000,19.90000,64.15000  #   7
*vertex,58.30000,20.50000,64.15000  #   8
*vertex,58.30000,23.08441,64.15000  #   9
*vertex,51.60000,23.17941,64.15000  #  10
*vertex,51.60000,20.42448,64.15000  #  11
*vertex,51.60000,19.23701,64.15000  #  12
*vertex,51.60000,18.19204,64.15000  #  13
*vertex,51.60000,17.28956,64.15000  #  14
*vertex,51.60000,17.00457,67.37500  #  15
*vertex,52.01118,17.14706,67.37500  #  16
*vertex,52.53366,17.28956,67.37500  #  17
*vertex,54.10112,17.66955,67.37500  #  18
*vertex,56.57106,18.23954,67.37500  #  19
*vertex,58.30000,18.66702,67.37500  #  20
*vertex,58.30000,19.90000,67.37500  #  21
*vertex,58.30000,20.50000,67.37500  #  22
*vertex,58.30000,23.08441,67.37500  #  23
*vertex,51.60000,23.17941,67.37500  #  24
*vertex,51.60000,20.42448,67.37500  #  25
*vertex,51.60000,19.23701,67.37500  #  26
*vertex,51.60000,18.19204,67.37500  #  27
*vertex,51.60000,17.28956,67.37500  #  28
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
*surf,fac_frame,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,fac_glaz_a,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,fac_glaz_b,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,fac_glaz_c,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,fac_glaz_d,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,pa_fac_l4,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,14,43  #   6 ||< pa_fac_l5:L2_ne_facad
*surf,gl_fac_l4,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,14,44  #   7 ||< gl_fac_l5:L2_ne_facad
*surf,pb_fac_l4,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,14,45  #   8 ||< pb_fac_l5:L2_ne_facad
*surf,ptn_toilet,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,34,01  #   9 ||< pt_meeting:L5_toil_psg
*surf,ptn_open_a,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,ptn_open_b,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,door,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,ptn_open_c,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  13 ||< not yet defined
*surf,ptn_frame,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  14 ||< not yet defined
*surf,ceiling,CEIL,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  15 ||< not yet defined
*surf,floor,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  16 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,16,    35.32 0  # zone base list
