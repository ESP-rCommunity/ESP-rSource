*Geometry 1.1,GEN,passage # tag version, format, zone name
*date Mon Feb 13 19:49:38 2012  # latest file modification 
passage connects office and staff room across the foyer
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,32.00000,11.00000,3.00000  #   1
*vertex,68.00000,11.00000,3.00000  #   2
*vertex,68.00000,13.50000,3.00000  #   3
*vertex,32.00000,13.50000,3.00000  #   4
*vertex,32.00000,11.00000,6.00000  #   5
*vertex,68.00000,11.00000,6.00000  #   6
*vertex,68.00000,13.50000,6.00000  #   7
*vertex,32.00000,13.50000,6.00000  #   8
*vertex,37.27208,11.00000,3.43934  #   9
*vertex,62.72792,11.00000,3.43934  #  10
*vertex,62.72792,11.00000,5.56066  #  11
*vertex,37.27208,11.00000,5.56066  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,9,12,11,10,9  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,4,9,10,11,12  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_foy,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,03,10  #   1 ||< ptn_corid:Foyer
*surf,ptn_staff,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,08,08  #   2 ||< ptn_staff:Staff_traing
*surf,ptn_store,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,01,06  #   3 ||< ptn_psg:Sales
*surf,ptn_off,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,06,08  #   4 ||< ptn_off:Office3
*surf,roof,CEIL,-,ROOF,-,SM_roof2008,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,ANOTHER,03,08  #   6 ||< floor:Foyer
*surf,inner_glaz,VERT,ptn_foy,-,-,dbl_glz,DCF7671_06nb,ANOTHER,03,09  #   7 ||< inner_glaz:Foyer
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    90.00 0  # zone base list
