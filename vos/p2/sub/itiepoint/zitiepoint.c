#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: ziwrite_tiepoints - IBIS write tiepoints           */
/************************************************************************/

void ziwrite_tiepoints( unit,nah,nav,npts, tiepoints, colcount)
int unit, nah, nav, npts, colcount;
float *tiepoints;
{
FTN_NAME2_(iwrite_tiepoints, IWRITE_TIEPOINTS) ( &unit,&nah,&nav,&npts,
						tiepoints, &colcount);
}

/************************************************************************/
/* C-Callable Version: ziread_tiepoints - IBIS read tiepoints           */
/************************************************************************/

void ziread_tiepoints( unit,nah,nav,maxpts, tiepoints, colcount)
int unit, *nah, *nav, maxpts, colcount;
float *tiepoints;
{
FTN_NAME2_(iread_tiepoints, IREAD_TIEPOINTS) ( &unit,nah,nav,&maxpts,
						tiepoints, &colcount);
}
