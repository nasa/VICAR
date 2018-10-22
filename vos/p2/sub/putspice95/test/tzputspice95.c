#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spc.h"
#include "spiceinc.h"

/************************************************************************/
/* Test Program For FORTRAN AND C Callable Subroutine PUTSPICE95.F      */
/************************************************************************/

void FTN_NAME(tzputspice95)(project, buf, mode, ind)
 int	*project;
 void	*buf;
 int	*mode;
 int	*ind;
{
 char		src[5];
 buf_union_typ	cbuf;
 memset(src, '\0', 5);
 memcpy((char *) &cbuf, (char *) buf,
		sizeof(buf_union_typ));

 *ind = zputspice95(*project, &cbuf, *mode);
}
