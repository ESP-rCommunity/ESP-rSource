*Geometry 1.1,GEN,L3_void_b # tag version, format, zone name
*date Sat Aug 13 14:58:09 2011  # latest file modification 
L3_void_b describes void above the careers services area
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,45.64634,11.68470,56.35000  #   1
*vertex,45.97883,11.73220,56.35000  #   2
*vertex,48.44877,12.30219,56.35000  #   3
*vertex,50.82371,12.87217,56.35000  #   4
*vertex,51.63119,13.10967,56.35000  #   5
*vertex,51.58369,13.77465,56.35000  #   6
*vertex,51.58369,16.29209,56.35000  #   7
*vertex,51.63119,17.00457,56.35000  #   8
*vertex,51.58369,17.28956,56.35000  #   9
*vertex,44.69636,15.62710,56.35000  #  10
*vertex,45.64634,11.68470,59.49300  #  11
*vertex,45.97883,11.73220,60.25000  #  12
*vertex,48.44877,12.30219,60.25000  #  13
*vertex,50.82371,12.87217,60.25000  #  14
*vertex,51.63119,13.10967,60.25000  #  15
*vertex,51.58369,13.77465,60.25000  #  16
*vertex,51.58369,16.29209,60.25000  #  17
*vertex,51.63119,17.00457,60.25000  #  18
*vertex,51.58369,17.28956,59.49300  #  19
*vertex,44.69636,15.62710,59.49300  #  20
*vertex,51.58369,17.28956,60.25000  #  21
*vertex,44.69636,15.62710,60.25000  #  22
*vertex,45.64634,11.68470,60.25000  #  23
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,2,12,23,11  #  1
*edges,4,2,3,13,12  #  2
*edges,4,3,4,14,13  #  3
*edges,4,4,5,15,14  #  4
*edges,4,5,6,16,15  #  5
*edges,4,6,7,17,16  #  6
*edges,4,7,8,18,17  #  7
*edges,5,8,9,19,21,18  #  8
*edges,4,9,10,20,19  #  9
*edges,4,10,1,11,20  # 10
*edges,10,23,12,13,14,15,16,17,18,21,22  # 11
*edges,10,1,10,9,8,7,6,5,4,3,2  # 12
*edges,4,19,20,22,21  # 13
*edges,4,20,11,23,22  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,stone,VERT,-,-,-,stone_m_ell,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,fac_gla,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   2 ||< external
*surf,fac_glb,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   3 ||< external
*surf,fac_frame,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Wall-5,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Wall-6,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,Wall-7,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,Wall-8,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,int_glaz,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,06  #   9 ||< int_glaz:L3_open_rig
*surf,int_glazb,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,05  #  10 ||< int_glazb:L3_open_rig
*surf,Top-11,CEIL,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,fict_low,FLOR,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,struc_b,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  13 ||< not yet defined
*surf,struc_a,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  14 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,12,    27.08 0  # zone base list
