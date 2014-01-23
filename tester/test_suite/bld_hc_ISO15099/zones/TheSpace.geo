*Geometry 1.1,GEN,TheSpace # tag version, format, zone name
*date Wed Jan 15 08:34:35 2014  # latest file modification 
TheSpace describes a dummy office space behind a double facade
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,4.50000,0.00000,0.00000  #   2
*vertex,4.50000,6.00000,0.00000  #   3
*vertex,0.00000,6.00000,0.00000  #   4
*vertex,0.00000,0.00000,3.50000  #   5
*vertex,4.50000,0.00000,3.50000  #   6
*vertex,4.50000,6.00000,3.50000  #   7
*vertex,0.00000,6.00000,3.50000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,iCW,VERT,-,-,-,dbl_glz,DCF7671_06nb,ANOTHER,03,03  #   1 ||< iCW:TheChannel
*surf,Wall-2,VERT,-,-,-,mass_part,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,Wall-3,VERT,-,-,-,mass_part,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,Wall-4,VERT,-,-,-,mass_part,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,hungCeil,CEIL,-,-,-,susp_ceil,OPAQUE,ANOTHER,02,06  #   5 ||< hungCeil:hungCeiling
*surf,Base-6,FLOR,-,-,-,susp_flr_re,OPAQUE,ANOTHER,02,05  #   6 ||< Ceil:hungCeiling
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 0  # zone base list
