*Geometry 1.1,GEN,L2_careers # tag version, format, zone name
*date Thu Jul 14 22:59:12 2011  # latest file modification 
L2_careers describes open space over the careers service area
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.25000,13.06217,52.45000  #   1
*vertex,34.50000,12.11219,52.45000  #   2
*vertex,34.60000,10.97222,52.45000  #   3
*vertex,34.38913,9.92725,52.45000  #   4
*vertex,33.86664,8.78728,52.45000  #   5
*vertex,34.25000,8.75000,52.45000  #   6
*vertex,36.43250,9.21500,52.45000  #   7
*vertex,46.17000,11.54250,52.45000  #   8
*vertex,48.49750,12.16000,52.45000  #   9
*vertex,51.06250,12.77750,52.45000  #  10
*vertex,51.60000,13.00000,52.45000  #  11
*vertex,51.60000,17.28956,52.45000  #  12
*vertex,34.50000,12.11219,56.35000  #  13
*vertex,34.25000,13.06217,56.35000  #  14
*vertex,51.60000,17.28956,56.35000  #  15
*vertex,51.60000,13.00000,56.35000  #  16
*vertex,51.06250,12.77750,56.35000  #  17
*vertex,48.49750,12.16000,56.35000  #  18
*vertex,46.17000,11.54250,56.35000  #  19
*vertex,36.43250,9.21500,56.35000  #  20
*vertex,34.25000,8.75000,56.35000  #  21
*vertex,33.86664,8.78728,56.35000  #  22
*vertex,34.38913,9.92725,56.35000  #  23
*vertex,34.60000,10.97222,56.35000  #  24
*vertex,34.25000,13.06217,55.59300  #  25
*vertex,51.60000,17.28956,55.59300  #  26
*vertex,34.25000,13.06217,53.45000  #  27
*vertex,51.60000,17.28956,53.45000  #  28
*vertex,35.91461,13.46776,56.35000  #  29
*vertex,36.76406,9.49976,56.35000  #  30
*vertex,36.24158,9.35726,56.35000  #  31
# 
# tag, number of vertices followed by list of associated vert
*edges,12,2,1,12,11,10,9,8,7,6,5,4,3  #  1
*edges,13,14,13,24,23,22,21,20,19,18,17,16,15,29  #  2
*edges,4,5,6,21,22  #  3
*edges,4,6,7,20,21  #  4
*edges,4,7,8,19,20  #  5
*edges,4,8,9,18,19  #  6
*edges,4,9,10,17,18  #  7
*edges,4,10,11,16,17  #  8
*edges,6,11,12,28,26,15,16  #  9
*edges,4,12,1,27,28  # 10
*edges,5,26,25,14,29,15  # 11
*edges,4,28,27,25,26  # 12
*edges,6,1,2,13,14,25,27  # 13
*edges,4,2,3,24,13  # 14
*edges,4,3,4,23,24  # 15
*edges,4,4,5,22,23  # 16
*edges,8,29,14,13,24,23,22,31,30  # 17
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,lower_fict,FLOR,-,-,-,UNKNOWN,OPAQUE,ANOTHER,56,40  #   1 ||< upper_fict:L1_careers
*surf,upper_fict,CEIL,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   2 ||< not yet defined
*surf,fac_frame_a,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,fac_glaz_a,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,stone_wall,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,fac_glaz_b,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,fac_glaz_c,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,fac_frame_b,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,end_glazing,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,balcony,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,struc,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,fict_opening,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,seg_a,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,41,12  #  13 ||< seg_l:L2_circul
*surf,seg_b,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,41,13  #  14 ||< seg_m:L2_circul
*surf,seg_c,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,41,14  #  15 ||< seg_n:L2_circul
*surf,seg_d,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,41,15  #  16 ||< seg_o:L2_circul
*surf,L3_vc_low,CEIL,-,-,-,UNKNOWN,OPAQUE,ANOTHER,22,10  #  17 ||< L3_vc_low:L3_void_c
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,75.57,0  # zone base
