*Geometry 1.1,GEN,reception # tag version, format, zone name
*date Thu Aug 23 18:21:04 2007  # latest file modification 
reception of office group adj to conference room
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.00000,6.00000,0.00000  #   1
*vertex,3.00000,6.00000,3.00000  #   2
*vertex,9.00000,6.00000,0.00000  #   3
*vertex,9.00000,6.00000,3.00000  #   4
*vertex,12.00000,6.00000,0.00000  #   5
*vertex,12.00000,6.00000,3.00000  #   6
*vertex,12.00000,6.00000,0.90000  #   7
*vertex,3.00000,13.50000,0.00000  #   8
*vertex,6.00000,13.50000,0.00000  #   9
*vertex,6.00000,13.50000,3.00000  #  10
*vertex,3.00000,13.50000,3.00000  #  11
*vertex,9.00000,12.00000,0.00000  #  12
*vertex,9.00000,12.00000,3.00000  #  13
*vertex,12.00000,12.00000,0.00000  #  14
*vertex,12.00000,12.00000,0.90000  #  15
*vertex,12.00000,12.00000,3.00000  #  16
*vertex,12.00000,9.00000,0.00000  #  17
*vertex,12.00000,9.00000,0.90000  #  18
*vertex,12.00000,8.00000,0.90000  #  19
*vertex,12.00000,8.00000,0.50000  #  20
*vertex,12.00000,7.00000,0.50000  #  21
*vertex,12.00000,7.00000,0.90000  #  22
*vertex,12.00000,9.00000,3.00000  #  23
*vertex,12.00000,6.10000,1.00000  #  24
*vertex,12.00000,6.10000,2.90000  #  25
*vertex,12.00000,8.90000,2.90000  #  26
*vertex,12.00000,8.90000,1.00000  #  27
*vertex,12.00000,11.00000,0.90000  #  28
*vertex,12.00000,11.00000,0.50000  #  29
*vertex,12.00000,10.00000,0.50000  #  30
*vertex,12.00000,10.00000,0.90000  #  31
*vertex,12.00000,9.10000,1.00000  #  32
*vertex,12.00000,9.10000,2.90000  #  33
*vertex,12.00000,11.90000,2.90000  #  34
*vertex,12.00000,11.90000,1.00000  #  35
*vertex,9.00000,6.10000,0.90000  #  36
*vertex,11.90000,6.10000,0.90000  #  37
*vertex,11.90000,11.90000,0.90000  #  38
*vertex,9.00000,11.90000,0.90000  #  39
*vertex,9.00000,11.10000,0.90000  #  40
*vertex,11.10000,11.10000,0.90000  #  41
*vertex,11.10000,9.10000,0.90000  #  42
*vertex,9.00000,9.10000,0.90000  #  43
*vertex,9.00000,8.10000,0.90000  #  44
*vertex,11.10000,8.10000,0.90000  #  45
*vertex,11.10000,6.90000,0.90000  #  46
*vertex,9.00000,6.90000,0.90000  #  47
# 
# tag, number of vertices followed by list of associated vert
*edges,4,8,1,2,11  #  1
*edges,4,1,3,4,2  #  2
*edges,5,3,5,7,6,4  #  3
*edges,4,9,8,11,10  #  4
*edges,4,12,9,10,13  #  5
*edges,5,14,12,13,16,15  #  6
*edges,8,5,17,18,19,20,21,22,7  #  7
*edges,4,21,20,19,22  #  8
*edges,12,7,22,19,18,23,6,7,24,25,26,27,24  #  9
*edges,4,24,27,26,25  # 10
*edges,8,17,14,15,28,29,30,31,18  # 11
*edges,4,30,29,28,31  # 12
*edges,12,18,31,28,15,16,23,18,32,33,34,35,32  # 13
*edges,4,32,35,34,33  # 14
*edges,12,36,37,38,39,40,41,42,43,44,45,46,47  # 15
*edges,12,37,36,47,46,45,44,43,42,41,40,39,38  # 16
*edges,8,2,4,6,23,16,13,10,11  # 17
*edges,8,5,3,1,8,9,12,14,17  # 18
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,pt_core_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,SIMILAR,00,00  #   1 ||< identical environment
*surf,fict_rec,VERT,-,-,-,fict,SC_fictit,ANOTHER,02,04  #   2 ||< fict_rec:general
*surf,pt_rec_a,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,03  #   3 ||< pt_rec_a:general
*surf,pt_recp_a,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,03,04  #   4 ||< pt_recp_a:conference
*surf,fict_open,VERT,-,-,-,fict,SC_fictit,ANOTHER,03,03  #   5 ||< fict_open:conference
*surf,pt_recp_b,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,03,02  #   6 ||< pt_recp_b:conference
*surf,spandral,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,vent,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,frame,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,glazing,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  10 ||< external
*surf,spandral_a,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,vent_a,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,frame_a,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,glazing_a,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  14 ||< external
*surf,desk_top,CEIL,-,-,-,door,OPAQUE,ADIABATIC,0,0  #  15 ||< adiabatic
*surf,desk_base,FLOR,-,-,-,door,OPAQUE,ADIABATIC,0,0  #  16 ||< adiabatic
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,05,04  #  17 ||< ceil_rec:ceil_void
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #  18 ||< identical environment
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   8 # list of surfs
  7  8  9 10 11 12 13 14
# 
*insol_calc,all_applicable   2 # insolation sources
 10 14
# 
*base_list,1,16,    10.10 0  # zone base list
