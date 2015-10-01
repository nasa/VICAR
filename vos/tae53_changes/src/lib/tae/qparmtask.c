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



/* TDM CHECKOUT FILE_TIME= 9-SEP-1986 14:15 DUA1:[TAEV1.OLB]QPARMTASK.C;2 */
/* TDM CHECKOUT FILE_TIME=27-JUN-1984 10:13 DUA0:[TAEV1.OLB]QPARMTASK.C;5 */
/*
 *	q_ functions to send a V-block using inter-task communication.
 *	The V-block can be built using q_functions in other source files.
 *
 *	CHANGE LOG:
 *
 *	04-may-84	Move IMPORT declarations...lim
 *	04-may-84	VALUE_x to xVAL ... ces
 *	27-jun-84	save_string name change to q_save...palm
 *			change alloc_var to q_alloc...palm
 *	24-aug-84	Abort process if called in batch mode...dm
 *	22-jul-85	Change $runtype to tae_runtyp to aviod UNIX
 *			(GOULD/UTX) compilation error...dm
 *	14-sep-86	Fix PR1138/1139 (x_error loop). Check application 
 *			type and write message to stdout from q_sndp...dm
 *      04-apr-88       Added string size in call to q_alloc...tpl
 *	05-mar-89	new q_cmd and q_cmdwait...palm	
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"resinc.inc"	/* restricted allocation package	*/
#include	"terminc.inc"
#include "syninc.inc"
#include "taeintproto.h"


/*  Standard XQ/XR  error messages and error keys:		*/

    IMPORT TEXT	   pm_room[], pk_room[];

#define	NOROOM	{x_error((*p).mode, pm_room, pk_room, (uintptr_t) name, 0, 0); \
    		 return (P_NOROOM);  }	


/*
 *	q_dynp.  Send V-block to TM for dynamic parameters.
 */

FUNCTION CODE q_dynp
(
 struct PARBLK	*p,		/* V-block to send		*/
 TEXT		pdfspec[],	/* pdf file spec		*/
 FUNINT		mode		/* M_SUBPDF or M_FULLPDF	*/
)
    {
    IMPORT TEXT	 	tae_runtyp[];	/* INTER/BATCH/ASYNC		*/
    TEXT		*pstr;
    struct VARIABLE	*v;
    TEXT		name[6];
    CODE		code;


    if (s_equal(tae_runtyp, "BATCH"))	/* if invoked from a batch job  */
 	{
	x_error((*p).mode,
        "Dynamic parameteres from TAE are not available in batch mode",
		"TAE-DYNBATCH", 0, 0, 0);
	}

    /* The PDF file spec is set to the string value of the variable named
       _PROC.
    */

    s_copy ("_PROC", name);
    v = p_fvar(p, name);
    if (v == NULL)			/* if not already there, make it*/
	{
	v = q_alloc(&(*p).symtab, (*p).pool, name, V_STRING, 1, 0);
	if (v == NULL)
	    NOROOM
	(*v).v_class = V_LOCAL;
	}
    pstr = q_save((*p).pool, pdfspec);
    if (pstr == NULL)
        NOROOM
    SVAL(*v, 0) = pstr;
    code = q_sndp(p, mode);		/* send to TM			*/
    return (code);
    }

/*	q_cmd.  Send a command string to TM to be executed.
 *
 *	The command may be any TCL command and the command runs
 *	in the context of the process PDF and it can use the
 *	context of the process PDF.  When the command is complete
 *	the new process PDF context  must be received via
 *	p_inim.  You can examine the received parblk for $SFI/$SKEY
 *	to see if it worked ok or you may grab command output values
 *	from the parblk.
 *
 *	The command may not be a synchronous process (or a procedure
 *	that runs a synchronous proess).  You may however, submit
 *	procs that run asynchronously with commands like:
 *
 *		"xyz |runtype=asyc| x=36"
 *
 *	When p_inim completes, the proc has been started.
 *	If you then want to wait on termination of the async proc, 
 *	send the command "wait-async".
 *	
 */

FUNCTION CODE q_cmd (TEXT* command)
{
int	code;
struct  PARBLK parblk;
TEXT	*vv[1];

q_init (&parblk, sizeof parblk.pool, P_ABORT);		/* build a parblk */
vv[0] = command;
q_string (&parblk, "_COMMAND", 1, vv, P_ADD);		/* add _command */
code = q_sndp (&parblk, M_COMMAND);			/* send to TM	*/
return (code);
}


/*	q_cmdwait.   Do a q_cmd and wait for completion.
 *
 *	This is for callers who don't need the full parblk output
 *	of a command.  The return code is the $SFI of the
 *	executed command.
 */

FUNCTION CODE q_cmdwait (TEXT *command)
{
int	code;
struct PARBLK  parblk;
struct VARIABLE *sfi;

code = q_cmd (command);				/* execute command */
if (code != SUCCESS)
    return (code);
p_inim (&parblk, P_BYTES, P_ABORT);		/* get result		*/
sfi = p_find (&parblk, "$SFI");			/* completion code	*/
if (sfi)
    return (IVAL(*sfi,0));
else
    return (P_BADNAME);
}



/*
 * 	q_out. Send output V-block to TAE monitor.
 *	
 *	TBD:	How to determine P_BADNAME  error code in q_out	
 */

FUNCTION  CODE  q_out
(
 struct  PARBLK	*p		/* V-block			*/
)

    {
    CODE		code;

    code = q_sndp(p, M_OUTPUT);		/* send to TM			*/
    return(code);
    }

/*
 *	q_sndp.  Send V-block to parent task.
 *
 *	TBD: q_sndp has an exit() which should be more formal (e.g., $SFI).
 *	NOTE: q_sndp should not call x_error to avoid infilte loop.
 */

FUNCTION CODE q_sndp
(
 struct PARBLK	*p,		/* PARBLK to send		*/
 FUNINT		msgtype	        /* P_OUTPUT, P_SUBPDF, P_FULLPDF*/
 )
    {
    IMPORT CODE		applic_type;	/* Application type: 'c'/FORTRAN */
    CODE		code;
    CODE		termtype, lines, cols;		/* terminal info */
    TEXT		msgbuf[STRINGSIZ+1];

    makerel(&(*p).symtab, (*p).pool);	/* make pointers relative	*/
    (*p).msgtyp = msgtype;		/* set message type		*/
    (*p).blksiz = r_top((*p).pool) - (GENPTR)p;
    code = c_sndp((GENPTR)p, (*p).blksiz);
    (*p).hostcode = code;		/* save host error code		*/
    if ((code != SUCCESS) && ((*p).mode == P_ABORT))
	{
	t_init(&lines, &cols, &termtype);	/* get terminal type    */
	if (termtype == T_CRT)			/* we have got a terminal */
	    {
	    sprintf(msgbuf, 
    "[TAE-NOCOMM] TAE failure communicating from application to TM. Code %d.",
		code);
	    t_write(msgbuf, T_STDCC);		/* write error msg */
	    }
/*  	Also write the msg to standard output file. (NOTE: the p_/m_msgout
 *	routines do the necessary checking.)
 */
	sprintf(msgbuf,
	"TAE failure communicating from application to TM. Code %d.", code);
	if (applic_type == C_TYPE)			/* application in 'c' */
	    m_msgout(msgbuf, "TAE-NOCOMM");
	else if (applic_type == FORTRAN_TYPE) 
	    p_msgout(msgbuf, "TAE-NOCOMM");		/* FORTN  application */
	else 	
	    ;					/* unknown type; do nothing */	
	procexit(code);				/* exit the process	    */
	}
    makeabs(&(*p).symtab, (*p).pool);	/* make pool absolute again	*/
    return (code);
    }	
