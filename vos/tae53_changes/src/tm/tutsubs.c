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



/* TPEB CHECKOUT FILE_TIME= 7-DEC-1984 15:14 DUA0:[TAEV1.TM]TUTSUBS.C;14 */
/* TPEB CHECKOUT FILE_TIME=10-OCT-1983 20:05 DUA0:[TAEV1.TM]TUTSUBS.C;13 */
/*TPEB        CHECKOUT FILE_TIME=11-JUL-1983 20:37 DUA0:[TAEV1.TM]TUTSUBS.C;11 */

/* Tutor specific utility functions.
 * Functions are in alphabetical order.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	10-oct-83	Fix unix lint errors...palm
 *	06-dec-84	TCL 67: Add a few functions from tutor breakup...peb
 *	08-dec-84	TCL 67: rsetini to set parm qual defaults...peb
 *	13-dec-84	TCL 67: dispflab to handle parm qual display...peb
 */

#include	"stdh.inp"		/* standard C definitions		*/
#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"tmhost.inp"		/* host-dependent defs			*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"syninc.inc"		/* syntax package includes		*/
#include	"dirinc.inc"		/* directive package defines & structs	*/

#include "taeintproto.h"

    static TEXT		msg_mmov[] = "Terminal Monitor internal memory overflow.";
    static TEXT		key_mmov[] = "TAE-MEMOVR";	/* dyn mem overflow in tutor*/


/* addcomp - add a value component's text to a TXTSTOR structure.
 */

FUNCTION CODE addcomp 
(
    struct TXTSTOR	*txt,		/* in/out: struct to add text to	*/
    struct VARIABLE	*v,		/* in:  variable containing the value	*/
    FUNINT		compnum	/* in:  component number		*/

 )
    {
    CODE		type;
    TEXT		buf[STRINGSIZ+1];
    struct VARIABLE	*vref;

    if ((*v).v_type != V_NAME  &&  (*v).v_count == 0   &&  compnum == 1)
	{
	addtxt (txt, "--   (null value)");
	return (SUCCESS);
	}
    if ((*v).v_type != V_NAME  &&  compnum > (*v).v_count)
	return(SUCCESS);
    type = (*v).v_type;
    if (type == V_NAME)
	{
	vref = (*v).v_ref;
	if (vref != NULL)
            {
	    buf[0] = EOS;
	    if ((*v).v_default)
	        s_copy("+", buf);			/* ref local to PDF */
	    s_append ((*vref).v_name, buf);
	    if (addtxt(txt, buf) != SUCCESS)
		return(FAIL);
	    }
	}
    else if (type == V_INTEGER)
	{
	s_i2s(IVAL(*v, compnum-1), buf);		/* convert ot string*/
	if (addtxt(txt, buf) != SUCCESS)		/* add to dyn stor block*/
	    return(FAIL);
	}
    else if (type == V_REAL)
	{
	s_r2s(RVAL(*v, compnum-1), buf);		/* convert ot string*/
	if (addtxt(txt, buf) != SUCCESS)		/* add to dyn stor block*/
	    return(FAIL);
	}
    else if (type == V_STRING)
	{
	s_copy(SVAL(*v, compnum-1), buf);
	addqu(buf);			/* surround with quotes			*/
	if (addtxt(txt, buf) != SUCCESS)	/* add to dyn stor block	*/
	    return(FAIL);
	if (brktxt(txt, TUTVALSIZ) != SUCCESS)	/* break strings up for field width*/
	    return(FAIL);
	}
    return(SUCCESS);
    }

/*
 *	clrmlin - clear multi-lines on display.
 */

FUNCTION VOID clrmlin 
(
    FUNINT		line,		/* in:  display line # at which to start clear*/
    FUNINT		col,		/* in:  display column # at which to start clear*/
    FUNINT		numlin		/* in:  number of lines to clear	*/

 )
    {
    COUNT		i;

    for (i = 0; i < numlin; i++, line++)
	t_lclear(line, col);
    return;
    }

/* dispflab - display tutor field labels.
 */

FUNCTION VOID dispflab 
(
    FUNINT		line,		/* in:  screen line number		*/
    FUNINT		subtut,		/* in:  TRUE if tutoring on subc FALSE if on parms*/
    FUNINT		qualtut	/* in:  TRUE if tutoring on parm quals FALSE otherwise*/

 )
    {
    COUNT		column;

    if (subtut)
	{
	t_output(line, TUTNAMECHDCOL, "subcommand");
	t_output(line+1, TUTNAMECHDCOL, "----------");
	column = TUTSUBDESCCHDCOL;
	}
    else
	{
	if (qualtut)
	    t_output(line, TUTNAMECHDCOL, "qual");
	else
	    t_output(line, TUTNAMECHDCOL, "parm");
	t_output(line+1, TUTNAMECHDCOL, "----");
	t_output(line, TUTVALCHDCOL, "value");
	t_output(line+1, TUTVALCHDCOL, "-----");
	column = TUTVARDESCCHDCOL;
	}
    t_output(line, column, "description");
    t_output(line+1, column, "-----------");
    return;
    }

/* parenval - enclose a set of values in parentheses.
 * Starts at the current position in a syntax block,
 * inserts a '(' and appends a ')' to the syntax stream.
 */

FUNCTION CODE parenval 
(
    struct SYNBLK	*sb		/* in/out: syntax block			*/

 )
    {
    TEXT		buf[CMDLINSIZ+3];

    if (s_length((*sb).inichr) >= CMDLINSIZ-1)
	return(FAIL);
    s_copy("(", buf);
    s_append((*sb).curchr, buf);
    s_append(")", buf);
    s_copy(buf, (*sb).curchr);
    return(SUCCESS);
    }

/* rsetini - reset initial values for one symbol table.
 * The initial values become the same as the current values.
 * Note that the initial values may be different from the 
 * default values specified in the pdf.
 *
 * Returns SUCCESS unless memory overflow.
 *
 *	TBD: the v_dcount and v_dvp should be in TUTEXT since
 *	TBD+: tutor is the only user.  Makes VARIABLE smaller.
 */

FUNCTION  CODE rsetini 
(
    struct SYMTAB	*st		/* in/out: symbol table.*/

 )
    {
    struct VARIABLE	*v;
    CODE		type;
    COUNT		i;
    CODE		code;

    for (v = (*st).link; v != NULL; v = (*v).v_link)
	{
	if ((*v).v_class != V_PARM)		/* skip unless parm	*/
	    continue;
	type = (*v).v_type;
	if (type == V_NAME)
	    {
	    (*v).v_dref = (*v).v_ref;		/* note: this may be NULL */
	    continue;				/* but okay		  */
	    }
	if ((*v).v_count <= 0)		
	    (*v).v_dcount = (*v).v_count;	/* null or no value	*/
	else
            {					/* substantial value	*/
	    if ((*v).v_dvp == NULL)		/* if no def value vec	*/
		{
		(*v).v_dvp = (GENPTR) allval(v);
		if ((*v).v_dvp == NULL) goto over_err;
		}
	    (*v).v_dcount = 0;			/* in case we must dealloc before thru*/
	    for (i = 0; i < (*v).v_count; i++)	/* copy current values	*/
		{
		if (type == V_INTEGER)
		    DIVAL(*v, i) = IVAL(*v, i);
		else if (type == V_REAL)
		    DRVAL(*v, i) = RVAL(*v, i);
		else if (type == V_STRING)
		    {
		    if ((DSVAL(*v, i) = s_save(SVAL(*v, i))) == NULL)
			goto over_err;
		    }
		(*v).v_dcount++;
		}
	    }
	if ((*v).v_pv12)
	    {					/* recursive - set parm...	*/
	    code = rsetini(&(*v).v_qualst);	/* qualifier sym tab defaults	*/
	    if (code != SUCCESS)
		return(FAIL);
	    }
	}
    return(SUCCESS);

over_err:
    tutmsg(msg_mmov, key_mmov, 0, 0, 0, 0, 0);		/* report memory overflow	*/
    return(FAIL);
    }

/* txtread - text read for tutor.
 * Does a d_text call & saves the position afterwards.
 */

FUNCTION CODE txtread 
(
    struct DIRBLK	*db,		/* in/out: directive block (for d_ pkg)	*/
    TEXT		str[],		/* out: text string read		*/
    struct POSCTX	*posctx	/* out: the saved position context	*/

 )
    {
    CODE		code;

    code = d_text(db, str);
    f_movpos(&(*((*db).sfileptr)).posctx, posctx);
    return(code);
    }
