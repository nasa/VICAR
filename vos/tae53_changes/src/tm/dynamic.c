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



/*
		Dynamic parameter handling
*/

/*	Change log:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *
 *	05-aug-83	Fix parmst deallocation bugin dynget()...dm
 *	23-aug-83	Add installation exit call to dyncom()...peb
 *	02-sep-83	PR 360 fix; PR 464 fix...palm
 *	08-sep-83	Fix length of prompt string...palm
 *	08-sep-83	PR 495: dynamic parm mode in BATCH...palm
 *	12-sep-83	Re-structure for new tutor interface;
 *			fix PR 464 (easy, fast request for current proc)...palm
 *	22-sep-83	Fix bug in dynget call to dp_ins()...peb
 *	26-sep-83	New dp_ins call for dynamic subcommands...palm
 *	05-oct-83	Change getint() to cmd_noscreen()...dm
 *	11-oct-83	R for RUN; unix compilation errors...palm
 *	30-oct-83	Dyn tutor activation change & PR fixes...peb
 *	25-jan-84	New cmd_noscreen call; archive commands...palm
 *	05-feb-84	Fix bug: stdout from user goes to proc's stdout...nhe
 *	07-feb-84	Add Y_ASYNC for Y_RUN...nhe
 *	14-feb-84	Runtyp made CODE run_type...palm
 *	19-feb-84	New dynamic tutor rules...palm
 *	09-mar-84	Fix double close...palm
 *	11-mar-84	Pass 'dynamic' flag to opn_tutor as argument...dm
 *	15-mar-84	Print parcmp error message here...dm
 *	04-may-84	VALUE_x to xVAL ... ces
 *	07-may-84	Clean-up the IMPORT declarations...lim
 *	24-aug-84	Change dynamic tutor request to NOFORCE...lia
 *	15-oct-84	Created bld_pdf_ctx and moved logic from dynget...jtm
 *	16-oct-84	New call to opnpdf...nhe
 *	25-oct-84	Handle DO_HOLD from in dynget from tutor...nhe
 *
 *************************************************************************
 * CHANGES MADE IN THE RCJM TREE
 *
 *	18-apr-85	Support dynamic parameter req from remote jobs...dm
 *	19-apr-85	Update comments in bld_pdf_ctx...dm
 *	25-apr-85	Errmsg if bld_pdf_ctx fails for remote jobs...dm
 *	27-apr-85	Update get_remote_file calling seq...dm
 *
 *********************************************************************
 *	
 *	24-jul-85	Fix UNIX lint compilation errors...dm
 *	26-AUG-85	PR 806: Return M_DYNEXIT when exit from dyn tut...dab
 *	09-sep-85	PR 961: Argument to 'dynget' tells if tutor for	sync or
 *			async job, passed to 'tutor' to reject HOLD if sync...dab
 *	23-apr-87	Tell parmrg to merge VALIDs...palm
 *	22-jul-87	Add get_dyncmd () as part of effort to force TM 
 *			into libraries...ljn
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm	
 *      08-sep-87       Add pool size in calls to pack_parm and dynget
 *                      calling sequence...tpl
 *	24-feb-88	PR 1504: Change label dimension to LABELSIZ+1...ljn
 *	24-mar-88	Added braces to structure initializer...ljn
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	01-may-89	new dyncmd for dynamic command from process...palm 
 *	23-may-90	No more RCJM...ljn
 *	28-jun-90	Removed get_dyncmd()...ljn
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"syninc.inc"	/* syntax package			*/
#include	"terminc.inc"	/* terminal package			*/
#include	"tminc.inc"	/* TM definitions			*/
#include	"parblk.inc"	/* For definition of M_CONTINUE		*/
#include "taeintproto.h"

FUNCTION  CODE  get_dynpname
(
    struct CONTXT	*ctx,		/* in: context of executing proc     */
    struct PARBLK	*parblk,	/* in/out: parameter block	     */
    TEXT		dynspec[],	/* out: dynamic tutor pdf spec	     */
    TEXT		subcmd[]	/* out: subcommand for dynamic tutor */

 );

/*
 *	Currently, there are no commands here;  the table is referenced
 *	from INTRINSIC.C and must have at least the terminator entry.
 */

    GLOBAL struct ITRCMD dyncmd[] =
	{
	{0, ""}	/* TERMINATOR ENTRY: REQUIRED AT END */
	};


/*	bld_pdf_ctx - Build a context block for dynamic paramter mode
 *
 *	This function is called prior to entering dynamic parameter mode to
 *	obtain parameters for:
 *
 *	- a process, executed from an interactive TM, who has issued an
 *	  xqdynp request
 * 	
 *	- an asynchronous process who has issued an xqdynp request
 *	
 *	- an asynchronous procedure who has issued a GETPAR request
 *
 *	It's purpose is to initialize a context block for eventual use by
 *	tutor. This function does not build the symbol tables.
 *
 *** NOTE **
 *	In case of local asynchronous jobs the original proc
 *	context was no longer retained. Therefore, the input context
 *	block was a madeup context block created from primctx, but
 *	with executing proc's pdf spec etc.
 *
 *	RETURN CODES:
 *	
 *	FAIL - 		Could not build context block, or user is trying to
 *	       		do dynamic tutor from BATCH. Error message has been
 *	       		issued, PDF file closed, and context block cleaned up.
 *
 *	SUCCESS - 	The context block was successfully initialized, and
 *			the appropriate PDF was opened.
 */
	
FUNCTION CODE bld_pdf_ctx 
(
    struct CONTXT	*ctx,		/* in: context of executing process  */
    struct PARBLK	*parblk,	/* in/out: parameter block	     */
    struct CONTXT	*dctx,		/* out: context block for tutor      */
    struct SFILE	*dynpdf,	/* out: address of dynamic pdf SFILE */
    BOOL		*dash_present	/* out: TRUE if subcommand indicated */

 )
    {
    IMPORT struct CONTXT *curproc;	/* current context		*/
    IMPORT CODE		run_type;	/* INTER, BATCH, ASYCH		*/

    CODE		code;
    TEXT		fspec[FSPECSIZ+1];


    if (run_type != INTER)			/* must be interactive 	*/
	goto not_inter;				
    (*dctx).backlink = curproc;			/* for NAME resolution	*/
    (*dctx).prclevel = (*curproc).prclevel + 1;
    if (s_equal((*ctx).pdf.libr, "/LOCAL/"))
    	goto intern_try;
    code = inictx(dctx);			/* init context		*/
    if (code != SUCCESS)
	goto over_err;
    code = get_dynpname(ctx, parblk, fspec,
		(*dctx).subcmd);		/* get dynamic pdf name */
    if (code != SUCCESS)
	goto dynpdf_err;
    *dash_present = !NULLSTR((*dctx).subcmd) || (*dctx).subcmd[1] == '-';
    (*dctx).subsiz = s_length((*dctx).subcmd);
    (*dctx).intrinsic = FALSE;			/* only PDFs dynamically  */
    code = opnpdf(ctx, dctx, fspec, dynpdf);	/* open the PDF		  */
    if (code != SUCCESS)			
	goto preopn_err;	
    return (SUCCESS);				/* Return ins. exit status */

not_inter:
    tmmsg(PROCFAIL, "Attempt to enter dynamic tutor from BATCH.",
	  "TAE-DYNBATCH",0,0,0,0,0);
    return (FAIL);

intern_try:
    tmmsg(PROCFAIL, "Attempt to enter dynamic tutor for an internal proc.",
    	  "TAE-DYNINTERN",0,0,0,0,0);
    return (FAIL);

over_err:
    overr();				/* report memory overflow	*/
    goto clsctx_kill;

dynpdf_err:
    tmmsg(PROCFAIL,
	"Could not get user pdf '%s' for dynamic tutoring.",
	  "TAE-NODYNPDF", (uintptr_t) fspec,0,0,0,0);
    goto clsctx_kill;

preopn_err:
clsctx_kill:
    clsctx(dctx);
    return (FAIL);
    }


/*	get_dynpname. Get name of pdf for dynamic tutoring.
 */

FUNCTION  CODE  get_dynpname
(
    struct CONTXT	*ctx,		/* in: context of executing proc     */
    struct PARBLK	*parblk,	/* in/out: parameter block	     */
    TEXT		dynspec[],	/* out: dynamic tutor pdf spec	     */
    TEXT		subcmd[]	/* out: subcommand for dynamic tutor */

 )
    {

    struct SYNBLK	sb;		/* syntax block			*/
    TEXT	label[LABELSIZ+1], cmdstr[CMDLINSIZ+1];

    CODE		code;
    struct VARIABLE	*v;


    v = lookex(&(*parblk).symtab, "_PROC");	/* find proc name	*/
    if (v == NULL)				/* avoid crash		*/
	cmdstr[0] = EOS;
    else
        s_copy(SVAL(*v, 0), cmdstr);
    code = cmd_parse(&sb, cmdstr, label, dynspec, subcmd);
    if (code == FAIL)
	return (FAIL);
    if (NULLSTR(dynspec))
	f_spec (&(*ctx).pdf, dynspec);		/* if _PROC null, use curr*/

    return (SUCCESS);
    }


/*	dynget - initiate dynamic tutor or dynamic parameter mode
 *
 *	This is called when a process wants dynamic parameters.
 *	The output is an updated parblk with relative pointers or,
 *	if the process is to be killed, a parblk with no parameters
 *	and the message type set to M_KILLED.  If the user decided not
 *	to do anything after all, the message type is set to M_HOLD.
 *	If the user entered exit from tutor, return a parblk with
 *	M_DYNEXIT to indicate this, with no parameters; differs from
 *	M_KILLED in that process does not get aborted by parent TM.
 */

FUNCTION VOID dynget 
(
    struct CONTXT	*ctx,		/* in: context of executing process */
    struct PARBLK	*parblk,	/* in/out: parameter block	    */
    FUNINT              pool_size,      /* in: size of parblk.pool      */
    BOOL		asytut_req	/* in: async or sync tutoring ? */

 )
    {
    struct CONTXT	dctx;		/* dynamic context		*/
    CODE		code, mode;
    static TEXT 	notused_key[] = "TAE-NOTUSED";
    static struct SYMTAB nullst = {NULL}; /* null symbol table		*/
    struct VARIABLE	*v;
    struct SFILE	dynpdf;		
    TEXT		held_msg[STRINGSIZ+1];
    BOOL		dash_present;


    code = bld_pdf_ctx (ctx, parblk, &dctx, &dynpdf,
    					&dash_present);	/* Build tutor CONTXT */
    if (code == FAIL)
	goto kill;
    code = dp_ins (parblk, &dynpdf, &dctx,
    				    dash_present);	/* Installation exit */

    if (code == DP_NOTDONE)			/* Work not done by Inst. exit */
	{
	code = plcini (&dctx);			/* Make standard locals */
	if (code != SUCCESS)
	    goto postopn_err;
	code = pdftab (&dctx, &dynpdf);		/* Build symbol tables */
	if (code != SUCCESS)
	    goto postopn_err;
	held_msg[0] = EOS;
	code = parcmp(&(*parblk).symtab, &dctx.parmst, held_msg);
	if (code != SUCCESS)
	  tmmsg(PROCFAIL, held_msg, notused_key,0,0,0,0,0);
				/* print err msg */
	mode = ((*parblk).msgtyp == M_FULLPDF) ? FULLSET : SUBSET;
	code = parmrg(&(*parblk).symtab, &dctx.parmst, mode|VM_VALID);
	if (code != SUCCESS)
	    goto postopn_err;
        v = lookex(&(*parblk).symtab, "_PREFACE");
	code = opn_tutor (&dynpdf, &dctx, NOFORCE, v, TRUE);
	if (code != SUCCESS)
	    goto postopn_err;
	dctx.asydyntut = asytut_req;		/* tutor for async or sync ? */
	code = tutor (&dynpdf, &dctx);
	cls_tutor (&dynpdf, &dctx);		/* close tutor		   */
        }
    (*parblk).symtab.link = NULL;		/* initialize parblk tab   */
    if (code == DO_RUN)
	{					/* continue the process	   */
	if (pack_parm(&dctx.parmst, parblk, pool_size) != SUCCESS)
	    goto pack_err;
	(*parblk).msgtyp = M_CONTINUE;
	}
    else if (code == DO_HOLD)
    	(*parblk).msgtyp = M_HOLD;		/* tell caller to hold on  */
    else
	{					/* tell process of exit	*/
	if (pack_parm(&nullst, parblk, pool_size) != SUCCESS)
	    goto pack_err;
	(*parblk).msgtyp = M_DYNEXIT;
	}
    clsctx(&dctx);
    f_close (&dynpdf, F_KEEP);
    return;

pack_err:
    tmmsg(PROCFAIL, "Parameter values overflow process message capacity.",
	  "TAE-PSETOVER",0,0,0,0,0);
    goto postopn_err;

postopn_err:
    f_close(&dynpdf, F_KEEP);
    goto clsctx_kill;

clsctx_kill:
    clsctx(&dctx);
    goto kill;

kill:
    pack_parm(&nullst, parblk, pool_size);
    (*parblk).msgtyp = M_KILLED;
    return;
    }

/*	dyncommand - execute a TAE (i.e, TCL) command from process. 
 *
 *	The command is assumed to be the string named "_COMMAND"
 *	in the parblk.  The executing process can get hold of
 *	output values from the command by examining the returned
 *	parblk.  See notes below.
 */
FUNCTION VOID dyncommand 
(
    struct CONTXT	*ctx,		/* in/out: parameter context	*/
    struct PARBLK	*parblk,	/* in/out: absolute parblk */
    int			pool_size

 )
    {
    IMPORT struct VARIABLE *sfi_gbl, *skey_gbl;
    IMPORT struct CONTXT *curproc;		/* pointer to cur proc	*/
    IMPORT CODE		 cmd_mode;		/* for prccmd loop	*/
    struct VARIABLE	*cmd;
    struct CONTXT	*saved_cur;
    CODE		saved_mode;
    TEXT		cmdstr[CMDLINSIZ+1];
    CODE		code;

    saved_mode = cmd_mode;			/* save old command mode*/
    saved_cur  = curproc;			/* save the 'current' 	*/
    cmd_mode   = DYNMODE;			/* dynamic cmd mode	*/	
    curproc    = ctx;				
    (*ctx).inbody = TRUE;			/* so commands work okay */
    cmd = lookex (&(*parblk).symtab, "_COMMAND");
    if (!cmd || (*cmd).v_type != V_STRING || (*cmd).v_count < 1)
	{
  	IVAL(*sfi_gbl, 0) = -1;	
	set_string (skey_gbl, "TAE-DYN_COMMAND_BAD");
	code = FAIL;
	}
    else
	{
	s_copy (SVAL(*cmd,0), cmdstr);		/* get it wide enough for  */
        code = prccmd(cmdstr, ctx);		/* process the command	*/
	}
    cmd_mode = saved_mode;			/* restore old mode	*/
    curproc = saved_cur;			/* restore the cur proc	*/

    /*
     *	Here, we package the context of the process PDF for return to
     *  the executing process.   Since the dynamic command runs
     *  in the context of the process PDF, the  dynamic command
     *  can set output variables and the executing process can
     *  get hold of the values.   $SFI/$SKEY may be of most interest.
     */

    code = package (ctx, parblk, pool_size, 0); /* re-build parblk	*/
    if (code != SUCCESS)
	{
        r_init ((*parblk).pool, pool_size);	/* make a good null block */
	(*parblk).symtab.link = NULL;
	}
    (*parblk).msgtyp = M_CONTINUE;
    return;
    }

#ifdef XXXXXXXX		/* following code no longer used	*/
/* The following are the data structures required by commands that are
   exclusive to dynamic parameter mode.  Additional commands used in dynamic
   parameter mode are described elsewhere if they are also used in other
   command modes.

    Note that the structures for the RUN command are unusual in that they
    must be built dynamically based on the parameters requested.
*/



    CODE	run_do();		/* RUN execution routine	*/

#define Y_RUN Y_BODY|Y_CLEAR|Y_DYNCMD|Y_ASYNC|Y_INTER

    GLOBAL struct ITRCMD dyncmd[] =
	{
{1, "RUN",	"",  Y_RUN,	 	    0,	 NULL,  run_do	   },

{0, ""}	/* TERMINATOR ENTRY: REQUIRED AT END */
	};


/*	dyncom - Get parameters through dynamic parameter mode
 *
 *	returns DO_EXIT or DO_RUN
 */
FUNCTION CODE dyncom 
(
    struct SFILE	*pdf,		/* in/out: opened pdf		*/
    struct CONTXT	*ctx		/* in/out: parameter context	*/

 )
    {
    IMPORT struct SFILE	 prcfil;	/* context of proc that requested parms	*/
    IMPORT struct VARIABLE *sfi_gbl, *skey_gbl;
    IMPORT struct CONTXT *curproc;		/* pointer to cur proc	*/
    IMPORT struct CONTXT primctx;		/* primary context	*/
    IMPORT struct SFILE	*pstdo_fil, *saved_pstd;/* ptr to stdout SFILE	*/
    IMPORT CODE		 cmd_mode;	/* for prccmd loop		*/
    IMPORT TEXT		 next_cmd[];

    struct SFILE	saved_fil;
    struct CONTXT	*saved_cur;
    BOOL		fil_saved;	/* TRUE if we saved file struct	*/
    CODE		saved_mode;
    TEXT		cmdstr[CMDLINSIZ+1];
    CODE		code,scode;
    TEXT		new_prompt[STRINGSIZ+1];

    fil_saved  = FALSE;
    saved_mode = cmd_mode;			/* save old command mode*/
    saved_cur  = curproc;			/* save the 'current' 	*/
    cmd_mode   = DYNMODE;
    curproc    = ctx;				/* for err msgs		*/
    saved_pstd = pstdo_fil;
    pstdo_fil = NULL;				/* implies term'l, by convention */
    (*ctx).inbody = TRUE;			/* so commands work okay*/
    (*ctx).prclevel = 0;			/* so error msgs okay	*/
    if (pdf != &prcfil)				/* if parms req from diff proc	*/
	{
	fil_saved = TRUE;
	MOVE_STRUCT(prcfil, saved_fil);		/* save the calling file*/
	MOVE_STRUCT(*pdf, prcfil);		/* so tutor_do picks up	*/
	}
    putlist(&((*ctx).parmst));			/* give user the list	*/
    build_prompt((*ctx).pdf.name, new_prompt);
    do
    	{
    	if (NULLSTR(next_cmd))			/* no saved HELP cmd?	*/
	    cmd_noscreen (A_LASTCMD, new_prompt, cmdstr);
	else
    	    {					/* HELP supplied the lin*/
    	    s_copy(next_cmd, cmdstr);
    	    next_cmd[0] = EOS;
    	    }
	scode = substitute (cmdstr, CMDLINSIZ, &primctx, TRUE);
	history_save (cmdstr);
	if (scode != SUCCESS)			/* bad substitution	*/
	    continue;				/* get another command	*/
    	code = prccmd(cmdstr, ctx);		/* process the command	*/
    	}
    while ( (code != DO_RUN) && (code != DO_EXIT));
    if (fil_saved)
	MOVE_STRUCT(saved_fil, prcfil);		/* restore prcfil	*/
    if (code == DO_EXIT)
	{
    	IVAL(*sfi_gbl,0) = -1;			/* force error on 'exit'*/
	set_string (skey_gbl, "TAE_EXIT"); 	/* set $skey 	*/
	}
    cmd_mode = saved_mode;			/* restore old mode	*/
    curproc = saved_cur;			/* restore the cur proc	*/
    pstdo_fil = saved_pstd;			/* go back to old stdout */
    return(code);
    }

/* build_prompt - Build a new prompt string
 *
 */
FUNCTION static VOID build_prompt
(
    TEXT 		name[],		/* in: proc name		*/
    TEXT		prompt[]	/* out: the prompt built	*/

 )
    {
    IMPORT  struct  VARIABLE    *prompt_gbl;	/* pointer to $prompt	*/

    COUNT		i;

    i = s_copy(SVAL(*prompt_gbl, 0), prompt); /* get current prompt	*/
    prompt[i++] = '-';
    i += s_copy(name, &prompt[i]);		/* add proc name	*/
    prompt[i++] = '>';
    prompt[i] = EOS;
    return;
    }

/*	move_locals.   Move locals from parblk into symbol table.
 */

FUNCTION CODE move_locals 
(
    struct PARBLK	*p,		/* in: parblk		*/
    struct SYMTAB	*locst		/* out: symbol table	*/

 )
    {
    struct VARIABLE	*vin, *vout, *allvar();
    CODE		code;

    for (vin=(*p).symtab.link; vin != NULL; vin=(*vin).v_link)
        {
	if ((*vin).v_class != V_LOCAL)	/* only locals get moved */
	    continue;
	vout = allvar (locst);		/* allocate variable	 */	
        if (vout == NULL)
	    return (FAIL);
	code = vcopy (vin, vout);	/* copy variable	 */
	if (code != SUCCESS)
	    return (FAIL);
	}
    return (SUCCESS);
    }

/* putlist - Put the list of parameter names from the parmst onto the terminal
 *
 */
FUNCTION static VOID putlist
(
    struct SYMTAB	*parmst		/* in: paramters to list	*/

 )
    {
    TEXT		request[STRINGSIZ+1];
    struct VARIABLE	*v;
    COUNT		msglen;

    msglen = s_copy("Parameters requested: ", request);	
    for (v=(struct VARIABLE *)(*parmst).link; v!=NULL; v=(*v).v_link )
    	{
    	if (msglen+s_length((*v).v_name)+2 > STRINGSIZ-3)
    	    {				/* names don't fit in the buffer*/
    	    s_append("...", request);
    	    break;
    	    }
        msglen = s_append((*v).v_name, request);
    	if ((*v).v_link != NULL) s_append(", ", request);
    	}
    t_write(request, T_STDCC);
    return;
    }

/* run_do - Execute the RUN command
 *
 * Not treated by TM as a "proc syntax" intrinsic command because we
 * want dynamically created parms and because intrinsics don't allow
 * FILE or NAME variables.
 *
 * returns DO_RUN or DO_RETURN (i.e., error)
 *
 */
FUNCTION CODE run_do
(
    struct CONTXT	*proc_ctx,		/* in/out: proc context		*/
    struct CONTXT	*cmd_ctx		/* in: context of run cmd	*/

 )
    {
    IMPORT CODE		cmd_mode;	/* INTMODE, DYNMODE, or NORMODE		*/

/* The idea here is to update the proc context using the values supplied in the
   command context
 */


    CODE		code;

    if (cmd_mode != DYNMODE)
	{
	tmmsg(PROCFAIL, "'RUN' only available in dynamic parameter mode.",
		"TAE-BADCMD");
	return(DO_CHECK);
	}
    code = updtab(&((*proc_ctx).parmst), (*cmd_ctx).sb);
    if (code == SUCCESS)
    	return(DO_RUN);
    return(DO_CHECK);
    }
#endif
