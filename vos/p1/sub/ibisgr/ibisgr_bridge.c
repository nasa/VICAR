/*
 *  ibisgr_bridge.c -- FORTRAN bridges to IBIS Graphics-1 code
 *
 *  NOTE!!!: Ported C code should call the "z" routines rather
 *   than the fortran bridges below. Only FORTRAN programs can
 *   call these routines.
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "ibisfile.h"

int FTN_NAME2(rdgr, RDGR) (int *instance,int *num,int *dimension)
{
	return zrdgr(*instance,*num,*dimension);
}

int FTN_NAME2_(rdgr_unit, RDGR_UNIT) (int *unit,int *num,int *dimension)
{
	return zrdgr_unit(*unit,*num,*dimension);
}

int FTN_NAME2(wrgr, WRGR) (int *instance,int *num,int *dimension)
{
	return zwrgr(*instance,*num,*dimension);
}

int FTN_NAME2_(wrgr_unit, WRGR_UNIT) (int *unit,int *num,int *dimension)
{
	return zwrgr_unit(*unit,*num,*dimension);
}

int FTN_NAME2(updategr, UPDATEGR) (int *unit,int *num,int *dimension)
{
	return zupdategr(*unit,*num,*dimension);
}

int FTN_NAME2(getgr, GETGR) (int *num,int *zero,int *eof,
			float *first_c,float *second_c,float *other_c)
{
	return zgetgr(*num,zero,eof,first_c,second_c,other_c);
}

int FTN_NAME2(nextgr, NEXTGR) (int *num,int *eof,
		float *first_c,float *second_c,float *other_c)
{
	return znextgr(*num,eof,first_c,second_c,other_c);
}

int FTN_NAME2(putgr, PUTGR) (int *num,
		float *first_c,float *second_c,float *other_c)
{
	return zputgr(*num,*first_c,*second_c,other_c);
}

int FTN_NAME2(endgr, ENDGR) (int *num)
{
	return zendgr(*num);
}

int FTN_NAME2(setgr, SETGR) (int *num, int *row)
{
	return zsetgr(*num, *row);
}

int FTN_NAME2(clgr, CLGR) (int *num)
{
	return zclgr(*num);
}

void FTN_NAME2(signalgr, SIGNALGR) (int *num,int *status,int *abortflag)
{
	(void) zsignalgr(*num,*status,*abortflag);
}



