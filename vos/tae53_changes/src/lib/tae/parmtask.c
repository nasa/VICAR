/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/



/* TPL CHECKOUT FILE_TIME=27-MAY-1987 11:45 DUA1:[TAEV2.OLB.GENLIB]PARMTASK.C;3 */
/* TDB CHECKOUT FILE_TIME=29-JUL-1985 16:02 DUA1:[TAEV1.OLB]PARMTASK.C;2 */
/* TDM CHECKOUT FILE_TIME= 4-MAY-1984 15:46 DUA0:[TAEV1.OLB]PARMTASK.C;3 */
/* Parameter receipt subroutine package.
 * Written in C, functions in this package should be called (probably via
 * bridge routines) by application programs to receive parameters
 * from the terminal monitor or parent task.
 *
 * A caller supplied parameter block is initialized either with
 *
 *	p_inim - receives parameters from the terminal monitor (TM), or
 *	p_rdb - reads parameters from a specially formatted disk file.
 *
 * Note that p_rdb is in another source file.
 *
 * The caller may then retrieve individual parameters and their attributes
 * using calls in other source files.
 *
 *
 * CHANGE LOG:
 *
 *	11-oct-83	Fix unix compilation errors...palm
 *	03-may-84	Move IMPORT into function...lim
 *	24-aug-84	Save runtype and session id for later access...dm
 *	22-jul-85	Change names $session and $runtype to avoid
 *			Gould/UTX (UNIX) compilation errors...dm
 *	26-aug-85	PR 806: Check for exit from dynamic tutor...dab
 *      06-jul-87       Added restrict or dynamic mode bit...tpl
 *
 */

#include	"stdh.inp"
#include	"taeconf.inp"
#include	"symtab.inc"
#include	"parblk.inc"
#include "syninc.inc"
#include "taeintproto.h"

    GLOBAL TEXT     tae_runtyp[STRINGSIZ+1];	/* run type of process      */
    GLOBAL TEXT	    tae_sess[STRINGSIZ+1];	/* for sharing session ID...*/




/* p_inim - receive parameter block from TM.
 * p_inim or p_rdb must be called before any other call to the p_ package
 * using the same block structure.
 *
 * In addition to receiving the parameter block, p_inim performs other
 * initialization steps on block.
 *
 * If the mode argument passed in is P_ABORT, this and subsequent calls to
 * the p_ package using the same block will only return if SUCCESSful.
 *
 * Return codes:
 *
 *	SUCCESS
 *	P_NOROOM	If user parameter block not large enough
 *	P_KILLED	if the user exited tutor.  This can only occur
 *			if parameters are being received from a dynamic
 *			tutor session.  The application should terminate
 *			itself after performing any necessary clean-up
 *			operations.
 *	FAIL		if the parameters could not be received.
 */

FUNCTION CODE p_inim 
(
 struct PARBLK	*block,		/* in/out: parm block to be initialized	*/
 FUNINT		blksiz,		/* in:  size of block in bytes		*/
 FUNINT		mode		/* in:  P_ABORT or P_CONT		*/
 )
    {
    IMPORT TEXT		pm_killed[], pk_killed[];
    CODE		code;
    COUNT		bytes;
    struct  VARIABLE    *v;


    code = c_rcvp((GENPTR)block, blksiz);
    (*block).hostcode = code;			/* save host error code	*/
    if (code != SUCCESS )
 	{
	x_error(mode,"Unable to receive parameters from TAE monitor. Code %d.",
		"TAE-PRMRCV", code, 0, 0);
    	return (FAIL);
        }
    if (blksiz < (*block).blksiz)		/* if buffer not large enough	*/
	{
	x_error(mode, "Block to receive parameters from TM too small.",
		"TAE-PRMSIZ", 0, 0, 0);
	return (P_NOROOM);
	}
    if ( mode == P_ABORT )
        (*block).mode = P_MODE_ABORT | P_MODE_RESTRICT;
    else if ( mode == P_CONT )
        (*block).mode = P_MODE_CONT | P_MODE_RESTRICT;
    else
        {
        (*block).mode = ( mode & (P_MODE_ABORT | P_MODE_CONT) )
                        | P_MODE_RESTRICT;
        }
    /*
     *	The following function converts all pointers in PARBLK from relative
     *	to absolute pointers so that the block may be easily manipulated.
     */
    makeabs(&(*block).symtab, (*block).pool);

    bytes = blksiz - ((GENPTR)(*block).pool - (GENPTR)block);  /* size of pool		*/
    r_newsiz((*block).pool, bytes);			       /* set new allocation	*/

    if ( ((*block).msgtyp == M_KILLED) || ((*block).msgtyp == M_DYNEXIT) )
	{
	  x_error((*block).mode, pm_killed, pk_killed, 0,0,0);
        return (P_KILLED);
	}
    else
        {
	if ((*block).msgtyp == M_INIPAR) 	/* is this first PARBLK?    */
    	    {
	    v = p_fvar (block, "$RUNTYPE");	/* find $RUNTYPE global	    */
	    if (v != NULL)
	        s_copy (SVAL(*v,0), tae_runtyp);  /* save for later use	    */
	    v = p_fvar (block, "$SESSION");	/* find $SESSION global	    */
	    if (v != NULL)
	        s_copy (SVAL(*v,0), tae_sess);	/* save for later use	    */
	    }
        }
        return (SUCCESS);
    }
