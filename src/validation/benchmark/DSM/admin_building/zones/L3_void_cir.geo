*Geometry 1.1,GEN,L3_void_cir # tag version, format, zone name
*date Sat Aug 13 14:57:23 2011  # latest file modification 
L3_void_cir describes a void adjacent to circular meeting at L3
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.25000,13.06200,56.35000  #   1
*vertex,34.50000,12.11219,56.35000  #   2
*vertex,34.60000,10.97222,56.35000  #   3
*vertex,34.38913,9.92725,56.35000  #   4
*vertex,33.86664,8.78728,56.35000  #   5
*vertex,36.24158,9.35726,56.35000  #   6
*vertex,36.76406,9.49976,56.35000  #   7
*vertex,35.91460,13.46780,56.35000  #   8
*vertex,34.25000,13.06200,60.25000  #   9
*vertex,34.50000,12.11219,60.25000  #  10
*vertex,34.60000,10.97222,60.25000  #  11
*vertex,34.38913,9.92725,60.25000  #  12
*vertex,33.86664,8.78728,60.25000  #  13
*vertex,36.24158,9.35726,60.25000  #  14
*vertex,36.76406,9.49976,60.25000  #  15
*vertex,35.91460,13.46780,60.25000  #  16
*vertex,34.25000,13.06200,57.35000  #  17
*vertex,35.91460,13.46780,57.35000  #  18
*vertex,36.76406,9.49976,57.35000  #  19
*vertex,34.25000,13.06200,59.49300  #  20
*vertex,36.76406,9.49976,59.49300  #  21
*vertex,35.91460,13.46780,59.49300  #  22
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,10,9,20,17  #  1
*edges,4,2,3,11,10  #  2
*edges,4,3,4,12,11  #  3
*edges,4,4,5,13,12  #  4
*edges,4,5,6,14,13  #  5
*edges,6,6,7,19,21,15,14  #  6
*edges,4,19,18,22,21  #  7
*edges,4,18,17,20,22  #  8
*edges,8,9,10,11,12,13,14,15,16  #  9
*edges,8,1,8,7,6,5,4,3,2  # 10
*edges,4,7,8,18,19  # 11
*edges,4,21,22,16,15  # 12
*edges,4,8,1,17,18  # 13
*edges,4,22,20,9,16  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,seg_ac,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,36,12  #   1 ||< seg_l:L3_circular
*surf,seg_bc,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,36,13  #   2 ||< seg_m:L3_circular
*surf,seg_cc,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,36,14  #   3 ||< seg_n:L3_circular
*surf,seg_dc,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,36,15  #   4 ||< seg_o:L3_circular
*surf,fac_glaz,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   5 ||< external
*surf,fac_stone,VERT,-,-,-,stone_m_ell,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,fict_open,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,49,04  #   7 ||< fict_open:L3_open_rig
*surf,fict_open_b,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,48,32  #   8 ||< fict_open_b:L3_open_lft
*surf,l3_vc_upper,CEIL,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,L3_vc_low,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,20,17  #  10 ||< L3_vc_low:L2_careers
*surf,balcony_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,03  #  11 ||< balcony_a:L3_open_rig
*surf,struc_a,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,balcony_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,48,33  #  13 ||< balcony_b:L3_open_lft
*surf,struc_b,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  14 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,     8.16 0  # zone base list
