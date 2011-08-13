*Geometry 1.1,GEN,L4_void_cir # tag version, format, zone name
*date Sat Aug 13 15:51:40 2011  # latest file modification 
L4_void_cir describes void adjacent to circular level four
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.25000,13.06200,60.25000  #   1
*vertex,34.50000,12.11219,60.25000  #   2
*vertex,34.60000,10.97222,60.25000  #   3
*vertex,34.38913,9.92725,60.25000  #   4
*vertex,33.86664,8.78728,60.25000  #   5
*vertex,36.24158,9.35726,60.25000  #   6
*vertex,36.76406,9.49976,60.25000  #   7
*vertex,35.91460,13.46780,60.25000  #   8
*vertex,34.25000,13.06200,64.15000  #   9
*vertex,34.50000,12.11219,64.15000  #  10
*vertex,34.60000,10.97222,64.15000  #  11
*vertex,34.38913,9.92725,64.15000  #  12
*vertex,33.86664,8.78728,64.15000  #  13
*vertex,36.24158,9.35726,64.15000  #  14
*vertex,36.76406,9.49976,64.15000  #  15
*vertex,35.91460,13.46780,64.15000  #  16
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
*surf,Wall-1,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,43,12  #   1 ||< not yet defined
*surf,Wall-2,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,43,13  #   2 ||< not yet defined
*surf,Wall-3,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,43,14  #   3 ||< not yet defined
*surf,Wall-4,VERT,-,-,-,gyp_ins_gyp,OPAQUE,UNKNOWN,43,15  #   4 ||< not yet defined
*surf,Wall-5,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   5 ||< external
*surf,Wall-6,VERT,-,-,-,stone_m_ell,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,Wall-7,VERT,-,-,-,stone_atr_pt,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,Wall-8,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,Top-9,CEIL,-,-,-,roof_0.2,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,Base-10,FLOR,-,-,-,fictitious,SC_fictit,UNKNOWN,22,09  #  10 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,     8.16 0  # zone base list
