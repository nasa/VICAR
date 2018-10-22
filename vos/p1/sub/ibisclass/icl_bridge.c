/*********************************************************************
 *  FORTRAN Bridges for ICL routines
 *
 *  Don't even bother looking here; there is nothing but
 *  symantic sugar for the icl_new and icl_get modules.
 *
 *********************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include "ibisfile.h"

#define MAX_GRP_SIZE 32

void FTN_NAME2_(icl_get_root, ICL_GET_ROOT) (int *ibis,char *group,
		char *inst, ZFORSTR_PARAM)
#if 0
char *group;  /* name of group */
#endif
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,group,&ibis,3,2,1, inst);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,3,3,2, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetROOT(*ibis,grp,in);
}

void FTN_NAME2_(icl_get_position, ICL_GET_POSITION) (int *ibis, char *group,
		char *inst,ZFORSTR_PARAM)
#if 0
char *group;  /* name of group */
#endif
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,group,&ibis,3,2,1, inst);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,3,3,2, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetPOSITION(*ibis,grp,in);
}

void FTN_NAME2_(icl_get_pos_image, ICL_GET_POS_IMAGE) (int *ibis, char *gline,
		char *gsamp, char *gband, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char gline_c[MAX_GRP_SIZE+1],*gl=(char *)0;
	char gsamp_c[MAX_GRP_SIZE+1],*gs=(char *)0;
	char gband_c[MAX_GRP_SIZE+1],*gb=(char *)0;
	
	zsfor2c(gline_c,MAX_GRP_SIZE,gline,&ibis,5,2,1, inst);
	if (strlen(gline_c)) gl=gline_c;
	zsfor2c(gsamp_c,MAX_GRP_SIZE,gsamp,&ibis,5,3,2, inst);
	if (strlen(gsamp_c)) gs=gsamp_c;
	zsfor2c(gband_c,MAX_GRP_SIZE,gband,&ibis,5,4,3, inst);
	if (strlen(gband_c)) gb=gband_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,5,4, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetPOS_IMAGE(*ibis,gl,gs,gb,in);
}

void FTN_NAME2_(icl_get_pos_geographic, ICL_GET_POS_GEOGRAPHIC) (int *ibis,
		char *glat, char *glong, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char glat_c[MAX_GRP_SIZE+1],*gla=(char *)0;
	char glong_c[MAX_GRP_SIZE+1],*glo=(char *)0;
	
	zsfor2c(glat_c,MAX_GRP_SIZE,glat,&ibis,4,2,1, inst);
	if (strlen(glat_c)) gla=glat_c;
	zsfor2c(glong_c,MAX_GRP_SIZE,glong,&ibis,4,3,2, inst);
	if (strlen(glong_c)) glo=glong_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,4,4,3, inst);
	if (strlen(inst_c)) in=inst_c;
		
	ICLGetPOS_GEOGRAPHIC(*ibis,gla,glo,in);
}

void FTN_NAME2_(icl_get_value, ICL_GET_VALUE) (int *ibis, char *group,
		char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,group,&ibis,3,2,1, inst);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,3,3,2, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetVALUE(*ibis,grp,in);
}


void FTN_NAME2_(icl_get_rgb, ICL_GET_RGB) (int *ibis, char *gred,
		char *ggreen, char *gblue, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	char ggrp3_c[MAX_GRP_SIZE+1],*gg3=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gred,&ibis,5,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,ggreen,&ibis,5,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(ggrp3_c,MAX_GRP_SIZE,gblue,&ibis,5,4,3, inst);
	if (strlen(ggrp3_c)) gg3=ggrp3_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,5,4, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetRGB(*ibis,gg1,gg2,gg3,in);
}

void FTN_NAME2_(icl_get_pixel, ICL_GET_PIXEL) (int *ibis, char *gpos,
		char *gval, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gpos,&ibis,4,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gval,&ibis,4,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,4,4,3, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetPIXEL(*ibis,gg1,gg2,in);
}

void FTN_NAME2_(icl_get_direction, ICL_GET_DIRECTION) (int *ibis, char *group,
		char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,group,&ibis,3,2,1, inst);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,3,3,2, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetDIRECTION(*ibis,grp,in);
}


void FTN_NAME2_(icl_get_matrix, ICL_GET_MATRIX) (int *ibis, char *gmat,
		char *gind, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gmat,&ibis,4,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gind,&ibis,4,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,4,4,3, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetMATRIX(*ibis,gg1,gg2,in);
}

void FTN_NAME2_(icl_get_quality, ICL_GET_QUALITY) (int *ibis, char *group,
		char *inst,ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,group,&ibis,3,2,1, inst);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,3,3,2, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetQUALITY(*ibis,grp,in);
}

void FTN_NAME2_(icl_get_histogram, ICL_GET_HISTOGRAM) (int *ibis, char *gmat,
		char *gind, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gmat,&ibis,4,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gind,&ibis,4,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,4,4,3, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetHISTOGRAM(*ibis,gg1,gg2,in);
}

void FTN_NAME2_(icl_get_lookup_table, ICL_GET_LOOKUP_TABLE) (int *ibis,
		char *gps, char *gind, char *memname, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	char ggrp3_c[MAX_GRP_SIZE+1],*gg3=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gps,&ibis,5,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gind,&ibis,5,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(ggrp3_c,MAX_GRP_SIZE,memname,&ibis,5,4,3, inst);
	if (strlen(ggrp3_c)) gg3=ggrp3_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,5,4, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetLOOKUP_TABLE(*ibis,gg1,gg2,gg3,in); /* gg3 is actually a member */
}

void FTN_NAME2_(icl_get_statistics, ICL_GET_STATISTICS) (int *ibis, char *gmat,
		char *gind, char *memname, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	char ggrp3_c[MAX_GRP_SIZE+1],*gg3=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gmat,&ibis,5,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gind,&ibis,5,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(ggrp3_c,MAX_GRP_SIZE,memname,&ibis,5,4,3, inst);
	if (strlen(ggrp3_c)) gg3=ggrp3_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,5,4, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetSTATISTICS(*ibis,gg1,gg2,gg3,in); /* gg3 is actually a member */
}

void FTN_NAME2_(icl_get_point, ICL_GET_POINT) (int *ibis, char *gpos,
		char *gval, char *memname, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	char ggrp3_c[MAX_GRP_SIZE+1],*gg3=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gpos,&ibis,5,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gval,&ibis,5,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(ggrp3_c,MAX_GRP_SIZE,memname,&ibis,5,4,3, inst);
	if (strlen(ggrp3_c)) gg3=ggrp3_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,5,4, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetPOINT(*ibis,gg1,gg2,gg3,in); /* gg3 is actually a member */
}


/*********************************************************************
 *  FORTRAN Bridges for ICLNew
 *********************************************************************/


void FTN_NAME2_(icl_new_root, ICL_NEW_ROOT) (int *ibis, int *cols,
		int *ncols, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status = ICLNewROOT(*ibis,cols,*ncols,in);
}

void FTN_NAME2_(icl_new_position, ICL_NEW_POSITION) (int *ibis, int *cols,
		int *ncols, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewPOSITION(*ibis,cols,*ncols,in);
}

void FTN_NAME2_(icl_new_pos_image, ICL_NEW_POS_IMAGE) (int *ibis, int *line,
		int *samp, int *band, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,6,5,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewPOS_IMAGE(*ibis,*line,*samp,*band,in);
}

void FTN_NAME2_(icl_new_pos_geographic, ICL_NEW_POS_GEOGRAPHIC) (int *ibis,
		int *lat, int *longv, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewPOS_GEOGRAPHIC(*ibis,*lat,*longv,in);
}

void FTN_NAME2_(icl_new_value, ICL_NEW_VALUE) (int *ibis, int *cols,
		int *ncols, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewVALUE(*ibis,cols,*ncols,in);
}

void FTN_NAME2_(icl_new_rgb, ICL_NEW_RGB) (int *ibis,
		int *red, int *green, int *blue,
		char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,6,5,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewRGB(*ibis,*red,*green,*blue,in);
}

void FTN_NAME2_(icl_new_pixel, ICL_NEW_PIXEL) (int *ibis, int *pos, int *npos,
		int *val, int *nval, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,7,6,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewPIXEL(*ibis,pos,*npos,val,*nval,in);
}

void FTN_NAME2_(icl_new_direction, ICL_NEW_DIRECTION) (int *ibis, int *cols,
		int *ncols, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewDIRECTION(*ibis,cols,*ncols,in);
}


void FTN_NAME2_(icl_new_matrix, ICL_NEW_MATRIX) (int *ibis, int *mat,
		int *nmat, int *ind, int *nind, char *inst,
		int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,7,6,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewMATRIX(*ibis,mat,*nmat,ind,*nind,in);
}

void FTN_NAME2_(icl_new_quality, ICL_NEW_QUALITY) (int *ibis, int *qual,
		int *nqual, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewQUALITY(*ibis,qual,*nqual,in);
}

void FTN_NAME2_(icl_new_histogram, ICL_NEW_HISTOGRAM) (int *ibis, int *mat,
	int *nmat, int *ind, int *nind, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,7,6,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewHISTOGRAM(*ibis,mat,*nmat,ind,*nind,in);
}


void FTN_NAME2_(icl_new_lookup_table, ICL_NEW_LOOKUP_TABLE) (int *ibis,
		int *ps, int *nps, int *ind, int *nind, char *memname,
		char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,memname,&ibis,8,6,1, status);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,8,7,2, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewLOOKUP_TABLE(*ibis,ps,*nps,ind,*nind,grp,in);
}

void FTN_NAME2_(icl_new_statistics, ICL_NEW_STATISTICS) (int *ibis, int *mat,
		int *nmat, int *ind, int *nind, char *memname, char *inst,
		int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,memname,&ibis,8,6,1, status);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,8,7,2, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewSTATISTICS(*ibis,mat,*nmat,ind,*nind,grp,in);
}


void FTN_NAME2_(icl_new_point, ICL_NEW_POINT) (int *ibis, int *pos, int *npos,
		int *val, int *nval, char *memname, char *inst,
		int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,memname,&ibis,8,6,1, status);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,8,7,2, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewPOINT(*ibis,pos,*npos,val,*nval,grp,in);
}

