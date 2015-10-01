/* stub city - ain't it pretty? */
#include	"taeconf.inp"	/* TAE configuration definitions */
#include	"tminc.inc"
#include	"terminc.inc"
#include	"forstr.inp"
#include "taeintproto.h"
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

/* originally from zinit.c */
#ifdef LARGE_PARBLK_FIX
FILE *z_init(struct LARGE_PARBLK *UNUSED(p), FUNINT UNUSED(m)) { return NULL; }
#else
FILE *z_init(struct PARBLK *UNUSED(p), FUNINT UNUSED(m)) { return NULL; }
#endif

/* originally from qparmtask.c */
int q_cmd(TEXT *UNUSED(c)){return 0; }
int q_cmdwait(TEXT *UNUSED(c)){return 0; }
int q_dynp(struct PARBLK *UNUSED(X1), char* UNUSED(X2), FUNINT UNUSED(X3)) 
{return 0; }
int q_out(struct PARBLK *UNUSED(X1)){return 0; }
int q_sndp(struct PARBLK *UNUSED(X1), FUNINT UNUSED(X2)) 
{return 0; }

/* originally from parmtask.c */
int p_inim(struct PARBLK *UNUSED(X1), FUNINT UNUSED(X2), FUNINT UNUSED(X3)) 
{ return FAIL; }

/* originally from xqtask.c */
int BRIDGE1_NAME(xqout)(void){return 0; }

/* originally from zexit.c */
FUNCTION VOID z_exit(FUNINT sfi, TEXT *skey)
{
   exit(sfi);
}

/* originally from terminal.np.c */
CODE t_pinit(COUNT* lines, COUNT* columns, CODE *type)
{
   return t_init(lines, columns, type);
}

/* originally from terminal.np.c */
CODE t_init(COUNT* lines, COUNT* columns, CODE* type)
{
#if !defined(vms) && !defined(__VMS)
   if (isatty(1))
      setbuf(stdout, NULL);			/* unbuffer */
   else
      setvbuf(stdout, NULL, _IOLBF, 0);		/* set line buffering */
#endif

   *lines = 0;
   *columns = 0;
   *type = T_NOTTERM;

   return SUCCESS;
}


/* copied out of taeutil.c */
/*
 *  Raise num to exp (an integer)
 */
FUNCTION DOUBLE power 
(
    DOUBLE	num,		/* input: the number to raise		*/
    FUNINT	exp		/* input: the exponent			*/

 )
    {
    DOUBLE	temp;
    TAEINT	i;


    temp = 1;
    if (exp>=0)
    	{
        for(i=1; i<=exp; i++)
       	    if (fl_mult(temp, num, &temp) != SUCCESS)   /* floating mult routine */
    		return(0);		/* failure			*/
        return(temp);
        }
    else
    	{
    	for (i=1; i <= -exp; i++)
    	    if (fl_div(temp, num, &temp) != SUCCESS)
    		return(0);
    	return(temp);
    	}
    }
