*Geometry 1.1,GEN,guest_room # tag version, format, zone name
*date Mon Mar 19 14:37:35 2012  # latest file modification 
guest room describes rarely used room for guests
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.00000,7.60000,0.00000  #   1
*vertex,7.00000,7.60000,0.00000  #   2
*vertex,7.60000,7.60000,0.00000  #   3
*vertex,7.60000,9.20000,0.00000  #   4
*vertex,7.00000,9.20000,0.00000  #   5
*vertex,7.00000,11.00000,0.00000  #   6
*vertex,7.60000,11.00000,0.00000  #   7
*vertex,7.60000,12.60000,0.00000  #   8
*vertex,7.00000,12.60000,0.00000  #   9
*vertex,4.00000,12.60000,0.00000  #  10
*vertex,4.00000,11.00000,0.00000  #  11
*vertex,4.00000,10.00000,0.00000  #  12
*vertex,4.00000,7.60000,3.20000  #  13
*vertex,7.00000,7.60000,3.20000  #  14
*vertex,7.60000,7.60000,3.20000  #  15
*vertex,7.60000,9.20000,3.20000  #  16
*vertex,7.00000,9.20000,3.20000  #  17
*vertex,7.00000,11.00000,3.20000  #  18
*vertex,7.60000,11.00000,3.20000  #  19
*vertex,7.60000,12.60000,3.20000  #  20
*vertex,7.00000,12.60000,3.20000  #  21
*vertex,4.00000,12.60000,3.20000  #  22
*vertex,4.00000,11.00000,3.20000  #  23
*vertex,4.00000,10.00000,3.20000  #  24
*vertex,7.00000,12.60000,0.20000  #  25
*vertex,4.00000,12.60000,0.20000  #  26
*vertex,7.60000,12.60000,0.20000  #  27
*vertex,7.60000,11.00000,0.20000  #  28
*vertex,7.00000,11.00000,0.20000  #  29
*vertex,7.00000,9.20000,0.20000  #  30
*vertex,7.60000,9.20000,0.20000  #  31
*vertex,7.60000,7.60000,0.20000  #  32
*vertex,7.00000,7.60000,0.20000  #  33
*vertex,4.00000,7.60000,0.20000  #  34
*vertex,4.00000,10.00000,0.20000  #  35
*vertex,4.00000,9.90000,0.86000  #  36
*vertex,4.00000,8.70000,0.86000  #  37
*vertex,4.00000,8.70000,2.93000  #  38
*vertex,4.00000,9.90000,2.93000  #  39
*vertex,4.00000,8.40000,0.20000  #  40
*vertex,4.00000,8.40000,3.20000  #  41
*vertex,4.00000,8.40000,0.00000  #  42
*vertex,4.00000,9.83000,0.93000  #  43
*vertex,4.00000,8.77000,0.93000  #  44
*vertex,4.00000,8.77000,2.86000  #  45
*vertex,4.00000,9.83000,2.86000  #  46
# 
# tag, number of vertices followed by list of associated vert
*edges,13,13,14,15,16,17,18,19,20,21,22,23,24,41  #  1
*edges,13,1,42,12,11,10,9,8,7,6,5,4,3,2  #  2
*edges,4,9,10,26,25  #  3
*edges,4,25,26,22,21  #  4
*edges,4,8,9,25,27  #  5
*edges,4,27,25,21,20  #  6
*edges,4,28,27,20,19  #  7
*edges,4,29,28,19,18  #  8
*edges,4,30,29,18,17  #  9
*edges,4,31,30,17,16  # 10
*edges,4,32,31,16,15  # 11
*edges,4,7,8,27,28  # 12
*edges,4,6,7,28,29  # 13
*edges,4,5,6,29,30  # 14
*edges,4,4,5,30,31  # 15
*edges,4,3,4,31,32  # 16
*edges,4,33,32,15,14  # 17
*edges,4,34,33,14,13  # 18
*edges,10,35,40,41,24,35,36,39,38,37,36  # 19
*edges,4,2,3,32,33  # 20
*edges,4,1,2,33,34  # 21
*edges,4,12,42,40,35  # 22
*edges,5,11,12,35,26,10  # 23
*edges,10,36,37,38,39,36,43,46,45,44,43  # 24
*edges,5,26,35,24,23,22  # 25
*edges,4,40,34,13,41  # 26
*edges,4,42,1,34,40  # 27
*edges,4,43,44,45,46  # 28
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ceiling,CEIL,-,-,-,ceil_to_roof,OPAQUE,ANOTHER,12,01  #   1 ||< ceiling_str:upper
*surf,floor_str,FLOR,-,-,-,floor_to_cli,OPAQUE,ANOTHER,10,01  #   2 ||< floor_str:lower
*surf,skirt_g,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,09,21  #   3 ||< skirt_g:family
*surf,fam_ptn_g,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,09,01  #   4 ||< f_ptn_g:family
*surf,skirt_h,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,09,22  #   5 ||< skirt_h:family
*surf,fa_ptn_h,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,09,02  #   6 ||< f_ptn_h:family
*surf,ptn_a,VERT,-,-,-,door,OPAQUE,ANOTHER,07,11  #   7 ||< ptn_a:hall
*surf,ptn_b,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,07,12  #   8 ||< ptn_b:hall
*surf,ptn_c,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,07,13  #   9 ||< ptn_c:hall
*surf,ptn_d,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,07,14  #  10 ||< ptn_d:hall
*surf,ptn_e,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,07,15  #  11 ||< ptn_e:hall
*surf,skirt_a,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,07,18  #  12 ||< skirt_a:hall
*surf,skirt_b,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,07,19  #  13 ||< skirt_b:hall
*surf,skirt_c,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,07,20  #  14 ||< skirt_c:hall
*surf,skirt_d,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,07,21  #  15 ||< skirt_d:hall
*surf,skirt_e,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,07,22  #  16 ||< skirt_e:hall
*surf,k_ptn_b,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,02,06  #  17 ||< k_ptn_b:kiten_util
*surf,k_ptn_c,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,02,07  #  18 ||< k_ptn_c:kiten_util
*surf,wall_at_glz,VERT,-,-,-,masonry0.2,OPAQUE,EXTERIOR,0,0  #  19 ||< external
*surf,k_skirt_b,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,02,24  #  20 ||< skirt_b:kiten_util
*surf,k_skirt_c,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,02,25  #  21 ||< skirt_c:kiten_util
*surf,k_skirt_d,VERT,-,-,-,masonry0.2,OPAQUE,EXTERIOR,0,0  #  22 ||< external
*surf,skirt_out,VERT,-,-,-,stone_0.9m,OPAQUE,EXTERIOR,0,0  #  23 ||< external
*surf,br3_glz_frm,VERT,wall_at_glz,F-FRAME,CLOSED,sash_frame,OPAQUE,EXTERIOR,0,0  #  24 ||< external
*surf,ext_wall,VERT,-,WALL,-,stone_0.9m,OPAQUE,EXTERIOR,0,0  #  25 ||< external
*surf,k_ptn_gues,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,02,08  #  26 ||< k_ptn_d:kiten_util
*surf,skirt_d-gues,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,02,26  #  27 ||< skirt_d:kiten_util
*surf,glazing,VERT,br3_glz_frm,C-WINDOW,CLOSED,single_glz,SCF8783_06nb,EXTERIOR,0,0  #  28 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,4,2,    25.92 0  # zone base list
