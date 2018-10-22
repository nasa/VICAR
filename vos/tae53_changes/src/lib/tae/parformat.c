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



/*TDM         CHECKOUT FILE_TIME=22-AUG-1983 14:57 DUA0:[TAEV1.OLB]PARFORMAT.C;24 */
/*TDM         CHECKOUT FILE_TIME=13-JUL-1983 15:41 DUA0:[TAEV1.OLB]PARFORMAT.C;22 */

/*
 * Parameter formatting routines.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	22-aug-83	Updated for NULLABLES...dm
 *	25-aug-83	Return code FP_NULL for truncated values...dm
 *	07-sep-83	Updated for formatting function name changes...dm
 *
 ***********************************************************************
 */

#include	"stdh.inp"		/* system standard */
#include	"taeconf.inp"		/* TAE configuration */
#include	"parblk.inc"		/* parameter block definitions */
#include	"resinc.inc"		/* restricted allocation package */
#include	"tminc.inc"		/* TM  related definitions */
#include "taeintproto.h"

/*
 *	 p_inifor:   Initialise for parameter block formatting
 */
FUNCTION  VOID  p_inifor
(
 struct   PARBLK *parblk	/* IN: parameter block */
)
    {
    struct  FORCTX	*forctx; /* pointer to pool */
  
    forctx = (struct FORCTX *) r_alloc((*parblk).pool, sizeof (struct FORCTX));
				/* allocate storage in pool */
    if (forctx != NULL)		/* successful allocation */
    	{
    	(*forctx).nxtind = -1;	/* set indexes for first call*/
	(*forctx).nxtvar = (*parblk).symtab.link;
	}
    (*parblk).ctxptr = (GENPTR) forctx ; /* save ptr in parblk */
    return;
    }

/*
 * 	p_forp :  Format parameters in the parameter block.
 */

FUNCTION  CODE  p_forp
(
 struct  PARBLK	*parblk,		/* IN: parameter block */
 TEXT		line[],			/* OUT: formatted parameter rec */
 COUNT		length			/* IN: line length */ 
)
    {
    CODE 		code;

    code = m_forp((struct FORCTX * )(*parblk).ctxptr, line, length);
    return(code);
    }

/*
 * m_forp .  format the input variable parameters (in a linked list).
 *
 * TBD: elemination of proc name returned twice.
 */

FUNCTION  CODE  m_forp
(
 struct  FORCTX	*forctx,		/* IN: pointer format context block */
 TEXT		line[],			/* OUT: formatted parameter rec */
 COUNT		length			/* IN: line length	     */
)
    {
    struct  VARIABLE	*vv;			/* pointer to a variable     */
    COUNT		index;			/* index pointer 	     */
    COUNT		max_index;		/* max value of index ptr    */
    COUNT		nchar;			/* number of characters in line */
    CODE 		code = 0;

    if (forctx == NULL) 			/* FORCTX allocated failed   */
	return(FP_EOP);	
    if ((*forctx).nxtvar == NULL)
	return(FP_EOP);				/* end of parameter list     */

    line[0] = EOS;
    index = (*forctx).nxtind;
    for (vv = (*forctx).nxtvar; vv != NULL; vv = (*vv).v_link)
	{
	if (index == -1) 			/* stuff name into line	     */
	    {					
	    code = m_fpname((*vv).v_name, line, length);
 	    if (code == FAIL) break;			/* no more room      */
	    index++;				
	    }
	max_index = ((*vv).v_count >= 1) ? ((*vv).v_count-1) : 0;
	for ( ; index <= max_index; index++)
	    {
	    code = m_fpval(vv, index, line, length, length);  /* format next value */
	    if (code != SUCCESS) break;		/* no more room 	     */
	    }
	if (code != SUCCESS) break;		/* exit outer loop also	     */
	index = -1;				/* reinit for next variable  */
	}
    if (code == TRUNCATE)			/* couldnt fit in a new line */
	code = FP_NULL;
    else if (code == FAIL)			/* no more space in line     */
	{
	if (s_length(line) == 0) 
	    code = FP_NULL;			/* line too small 	     */
        else								
   	    {
            s_append("+", line);		/* add continuation    	     */
	    code = SUCCESS;			/* regard as success 	     */
	    }
	}
    else
	{					/* end reached 		     */
	nchar = s_length(line);
	line[nchar-2] = EOS;			/* delete hanging ", " 	     */
	}
    (*forctx).nxtind = index;			/* save new values 	     */
    (*forctx).nxtvar = vv;
    return(code);
    }
