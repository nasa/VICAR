/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* Process PDF for terminal monitor.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	12-jul-83	Rewrote process()...dm
 *	14-jul-83	Display host error code in subprocess related
 *			messages...dm
 *	29-jul-83	Clear "receive" event flag in process()...dm
 *	15-aug-83	Fix comment to align with delgbl_do fix...peb
 *	02-sep-83	Fix error msg in opnpdf (PR 432)...palm
 *			Also, new dynget calling sequence...palm
 *	08-sep-83	Set $status into $skey if DCL command...palm
 *	12-sep-83	Build global symbol table in alphabetic order...dm
 *	12-sep-83	Condititional compilation on function 'process'
 *			preparing for UNIX (where it will be cnp)...palm
 *	25-sep-83	Accept output variables from exit handlers of
 *			an aborted task...palm
 *	10-oct-83	Fix unix compilation errors...palm
 *	21-oct-83	Fix stdout reset bug...dm
 *	03-feb-84	Update for full-blown proc interrupt mode...ne
 *	22-feb-84	In pdftab(): save curproc explicitly...peb
 *	27-feb-84	In process():  Add message handshake...peb
 *	03-mar-84	In process(): Handle param request while in async...nhe
 *	11-mar-84	Make parblk relative before sending back to process
 *			for handsake...dm
 *	12-mar-84	Make outputs and interrupt EXTERN for UNIX...palm
 *	13-mar-84	Update to accomodate bldcmd changes...dm
 *	15-mar-84	Move logging of dyn pars to elsewhere...nhe
 *	04-may-84	VALUE_x to xVAL ... ces
 *	07-may-84	Clean-up the IMPORT declarations...lim
 *			'toprefs'-->'numrefs'...nhe
 *	17-mar-84	New hierarchy and f_crack calling sequence...palm
 *	8-jun-84	TCL-99: avoid global conflicts with
 *			level 0 locals...palm
 *	12-jun-84	TCL-21: CONTROL/C handling in procedure...palm
 *	21-jun-84	pr706: get errmsgs from process into stdout for async...nhe
 *	09-jul-84	pr664: move call to pt_ins(), in run_proc...peb
 *	13-jul-84	errmsgs-->process(),errmsg-->CODE_ERR in tminc...nhe
 *	01-aug-84	file_name-->file_spec in struct MON_MSG...nhe
 *	03-aug-84	pr396: give proc name on set_outval failure...peb
 *	16-aug-84	pr396: TAE-COMOPN --> TAE-COMOPEN...peb
 *	22-aug-84	Pass proc context to logmsg...lia
 *	08-oct-84	Update opnpdf to handle remote case...nhe
 *	11-oct-84	Add conditional compilation for c_snpa/c_sndp call
 *			as implemented for UNIX 1.2...dm
 *	12-oct-84	Conditional compilation for get_pblk_remote()...dm
 *	14-oct-84	Changes for internal procs...nhe
 *	17-oct-84	TCL 117: pdftab logic for compiled PDFs...peb
 *	18-oct-84	Enhancements to internal procs...nhe
 *	23-oct-84	TCL 117: compilation of SUBCMDs...peb
 *	07-dec-84	PR 908: incomplete reinit of VARIABLE struct in
 *			converting the parm to a global in make_gbl...lia
 *	11-dec-84	TCL 97: maintain flag for executing onfail cmd...lia
 *	18-dec-84	Fix infinite loop on bad onfail cmd...lia
 *
 ***************************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	06-feb-85	Check for link-abort event for remote jobs...dm
 *	09-feb-85	At CTL/C  abort_remote if processing an rpd...dm
 *	13-mar-85	Update for new member names of MON_MSG...dm
 *	06-apr-85	Update get_pblk_remote() for remote jobs; delete
 *			reference to ASYNCINC...dm
 *	17-apr-85	Add _DYNPDF for remote process's dynpar block...dm
 *
 ***********************************************************************
 *
 *	25-jul-85	Fix UNIX lint compilation errors...dm
 *	01-aug-85	Delete ecb_link reference due to link-abort 
 *			redesign...dm
 *	26-aug-85	PR 806: recognize user exit from dynamic tutor after
 *			call to get_parm_locpar...dab
 *	09-sep-85	PR 961: Call to 'dynget' by 'process' indicates that 
 *			will be tutoring for sync proc (to prevent HOLD)...dab
 *	03-oct-85	PR 946: Added "not compiling" (FALSE) parameter to
 *			calling sequence of 'chk_vector'...dab
 *	17-feb-86	Honor OPTIONS=INTERRUPT for procedures; Honor
 *			int_enabled flag (as set by DISABLE-INTERRUPT and
 *			ENABLE-INTERRUPT).
 *	24-mar-86	Pass current contxt to interrupt from procedure and
 *			process...palm
 *	28-may-86	Fix bug in make_gbl whereby adding a variable to
 *			BEGINNING of global table caused crash (we forgot
 *			to unlink the VARIABLE from the parmst before linking
 *			into glbtab)...palm
 *	16-jul-86	Honor ACTION= on parmset execution...palm
 *	05-sep-86	PR 1129 fix.  PDF line number now correct in
 *			error messages when comments, null lines precede
 *			erroroneous statement...peb
 *	05-sep-86	Remove anachronous null str
 *			check after getpst calls...peb
 *	11-sep-86	PR 1100 fix.  Modified process() to check for entry via
 *			menu.  If so, request for prompt before repaint...lia
 *	28-mar-87	New calling sequence for package in process. Also
 *			we use LARGE_PARBLK for sending to subprocess...palm
 *	19-jul-87	Fix make_gbl to pick up qualifiers...palm
 *	02-aug-87	Fix missing key in deep_value_copy tmmmsg...palm
 *	19-aug-87	Maintain consistency with old executables wrt
 *			the M_KILLED flag:  The V1.3 outboard library checks for
 *			M_KILLED PARBLK.msgtyp to determine that the tutor
 *			user typed "EXIT".  The V1.4 library and beyond check
 *			for either M_KILLED or M_DYNEXIT, and dynget sets
 *			msgtyp to M_DYNEXIT.   Here, we convert from
 *			M_DYNEXIT to M_KILLED before sending a parblk to an
 *			application process so that old executables work.
 *			This mess started with PR806...palm
 *	26-aug-87	Fix qualptr casting and definition in opnpdf...palm
 *      08-sep-87       Add size in call to dynget...tpl
 *	17-dec-87	Executable PAR files...palm
 *	18-dec-87	Add CheckXEvent call to run_proc...palm
 *	24-feb-88	PR 1504: Change label dimension to LABELSIZ+1...ljn
 *	26-feb-88	Reset EventProcCtx when coming out of a procedure 
 *			that has an event loop (using WPT-EVENT)...palm
 *	17-mar-88	PR 1499: Add call to isTerm()...ljn
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	02-may-88	Remove restriction that only files with type .PAR
 *			can be executed as PAR files.  See call to IsParFile.
 *			...palm
 *	05-may-88	isTerm() -> f_isterm()...ljn
 *	01-dec-88	package valids for SELFTUTOR...palm
 *	13-feb-89	Zero parblk header before sending to process...palm
 *	01-may-89	disallow process execution while process already
 *			running...palm 
 *	12-sep-89	Port PM's 1-may-89 fix on UNIX...ljn
 *      20-dec-89       Fixed handling of M_COMMAND for VMS...tpl
 *	23-may-90	Remove RCJM stuff referring to old TAE_RCJM...ljn
 *	27-jun-90	Remove Facelift code...ljn
 *	11-may-92	Used P_BIGSENTINEL instead of P_SENTINEL...tpl
 *	11-sep-92 PR875 In getpst, changed inbuf size to CMDLINSIZ instead of
 *			STRINGSIZ and use f_readstring instead of f_read...tpl
 *	22-oct-92	Prototype of tae_alloc unnecessary and Ultrix 4.3 does
 *			 not like it...rt
 *	13-may-93	LARGE_PARBLK is larger then max VMS record size.
 *			Use Vm_parbwrite and Vm_parbread instead of f_bwrite
 *			and f_bread to work-around this limitation...cew
 */

#include	"taeconf.inp"	/* TAE configuration definitions	*/
#include	"terminc.inc"	/* terminal package definitions		*/
#include	"syninc.inc"	/* syntax package structure and defs	*/
#include	"eventinc.inp"	/* e_ package defs & structs		*/
#include	"taskinc.inp"	/* c_ package defs & structs		*/
#include	"symtab.inc"
#include	"tmhost.inp"
#include	"parblk.inc"
#include	"tminc.inc"	/* TM-only host-independent definitions	*/


    GLOBAL	v202ppc = 0;		/* source version		*/



    BOOL		s_equal();	/* strings equal (case insignificant)	*/
    BOOL		s_lseq();	/* 1st string left substring of 2nd?	*/
    BOOL		bldcmd();	/* append 1 cmd line to whole cmd string*/
    BOOL		f_libr();	/* TRUE if verb has explicit libr spec	*/
    struct VARIABLE	*srchrf();	/* search for variab among list of ref'd glbls*/
    struct VARIABLE	*lookex();	/* look for variab in sym tab by exact name match*/

    static  CODE open_try();
    static  CODE chk_onfail();
    static  CODE get_attn();
    static  CODE make_gbl();
    static  VOID procedure();


#define	BC_PARMST	1
#define	BC_LOCST	2
#define	BC_GLOBREFS	3




/* build_prompt - Build a new prompt string, "<old prompt>-INTERRUPT>"
 *
 */
    FUNCTION static VOID build_prompt(prompt)

    TEXT		prompt[];	/* out: the prompt built	*/

    {
    IMPORT  struct  VARIABLE    *prompt_gbl;	/* pointer to $prompt	*/

    COUNT		i;

    i = s_copy(SVAL(*prompt_gbl, 0), prompt); /* get current prompt	*/
    prompt[i++] = '-';
    i += s_copy("INTERRUPT", &prompt[i]);	/* add 'INTERRUPT'	*/
    prompt[i++] = '>';
    prompt[i] = EOS;
    return;
    }

/*
 *	chk_onfail - check a command to make sure it is an
 *	allowed _ONFAIL command.
 *
 */

    FUNCTION static CODE chk_onfail (cmdstr)

    TEXT		cmdstr[];	/* in:  command string			*/

    {
    struct SYNBLK	sb;		/* syntax block				*/
    TEXT		verb[TOKESIZ+1];
    struct ITRCMD	*itrcmd;	/* pointer to intrinsic table		*/
    struct ITRCMD	*intrin();	/* get ITRCMD entry for command		*/

    initok(&sb, cmdstr);		/* init syntax block for syntax pkg	*/
    if (getvrb(&sb, verb) == S_SYNERR) 	/* get verb from cmd str		*/
	{
        tmmsg(PROCFAIL, "Incorrect command format in _ONFAIL command: %s.",
    	      "TAE-ONFAIL", sb.errmsg);
	return(FAIL);			/* DO_RETURN avoids _ONFAIL loop	*/
	}
    itrcmd = intrin(verb);		/* get pointer to ITRCMD entry		*/
    if (itrcmd != NULL)
	if (((*itrcmd).flags & Y_ONFAIL)  != 0)
	    return (SUCCESS);
    tmmsg(PROCFAIL, "'%s' not allowed as _ONFAIL command.", "TAE-ONFAIL", verb);
    return (FAIL);
    }

/*
 *	clssav - close a proc and save the position.
 */

    FUNCTION VOID clssav (pctx)

    struct CONTXT	*pctx;		/* in/out: proc context			*/

    {
    IMPORT struct SFILE	prcfil;		/* proc file context			*/

    f_movpos(&(prcfil.posctx), &((*pctx).prcpos));
    f_close(&prcfil, F_KEEP);	
    return;
    }
 
#ifdef TAE_ASYNC
/*  get_pblk_remote - Get parameter values for a parblk from parent monitor
 *
 *  Input is an 'absolute' parblk and output is a 'relative' parblk (imitates
 *  dynget in this).
 *
 *  Returns SUCCESS/FAIL
 */
    FUNCTION CODE get_pblk_remote (proc_ctx, parblk, errmsg)

    struct CONTXT	*proc_ctx;	/* in: Context of executing proc    */
    struct PARBLK	*parblk;	/* in/out: parameter block	*/
    struct CODE_ERR	*(*errmsg);	/* out: pointer to err msg	*/

    {
    IMPORT TEXT		comm_file[];	/* name of mon-to-mon comm'n file    */
    IMPORT CODE		parent_runtype; /* run_type of parent monitor	     */
    TEXT		recv_file[FSPECSIZ+1];	/* file received from parent */
    struct SFILE	sf;
    struct PARHDR	ph;
    CODE		code;
    CODE		send_stat, recv_stat;
    COUNT		recsize;

    static struct CODE_ERR er_asyparm=
    	{"Async jobs may request parms only if invoked interactively.",
    	 "TAE-ASYPARM", 0};

    static struct CODE_ERR er_comopn=
    	{"Error opening async communication file.",
    	 "TAE-COMOPEN", 0};

    static struct CODE_ERR er_comwrt=
    	{"Error writing to async communication file.",
    	 "TAE-COMWRT", 0};

    static struct CODE_ERR er_comread=
    	{"Error reading async communication file.",
    	 "TAE-COMREAD", 0};

    static struct CODE_ERR er_fmtcomm=
    	{"Async communication file not properly formatted.",
	 "TAE-FMTCOMM", 0};

    static struct CODE_ERR er_asysnd=
    	{"Error sending message to parent monitor.  Host error code = %d.",
    	 "TAE-ASYSND", 0};

    static struct CODE_ERR er_asyrcv=
    	{"Error receiving message from parent monitor.  Host error code = %d.",
    	 "TAE-ASYRCV", 0};


    if (parent_runtype == ASYNC || parent_runtype == BATCH)
    	{
    	*errmsg = &er_asyparm;
    	return (FAIL);
    	}

/* Save the parblk in the communication file				*/

    code = f_opnspc (&sf, SAVELUN, comm_file, "", "", ASY_TYPE, F_WRITE);
    if (code != SUCCESS) goto open_err;
    code = save_pfile (&sf, (*parblk).msgtyp, NULL, 0, 1, &(*parblk).symtab);	/* write parmst to file */
    if (code != SUCCESS) goto write_err;
    f_close (&sf, F_KEEP);

/* Send the message to the parent monitor and receive return reply	*/

    code = get_parm_locpar(comm_file, recv_file, TRUE,
	    &send_stat, &recv_stat);		/* get parm from local parent */
    if (code == KILLED)				/* user wants to exit the job */
	{
	(*parblk).msgtyp = M_KILLED;
	return (SUCCESS);
	}
    else if (code == DO_EXIT)			/* user exited from dyn tutor */
	{
	(*parblk).msgtyp = M_DYNEXIT;
	return (SUCCESS);
	}
    if (code != SUCCESS) goto comm_err;		/* communication error      */

    code = f_opnspc(&sf, SAVELUN, recv_file, "", "",
		    ASY_TYPE, F_READ);		/* open for read	    */
    if (code != SUCCESS)  goto open_err;	/* report error		    */
    code = f_bread(&sf, (GENPTR) &ph, sizeof(struct PARHDR), &recsize);	
    if (code != SUCCESS)  goto read_err;
    if (!s_equal(ph.sentinel, P_BIGSENTINEL)) goto format_err;	/* not proper format	    */
#if defined(vms) || defined(__VMS)
    code = Vm_parbread(&sf, (GENPTR) parblk, sizeof(struct PARBLK), &recsize);
#else
    code = f_bread(&sf, (GENPTR) parblk, sizeof(struct PARBLK), &recsize);
#endif
    if (code != SUCCESS) goto read_err;
    f_close (&sf, F_DELETE);
    return(SUCCESS);


/*  on errors we point to an error msg in front of this module		*/
open_err:
    *errmsg = &er_comopn;
    return(FAIL);

write_err:
    *errmsg = &er_comwrt;
    f_close(&sf, F_DELETE);
    return(FAIL);

read_err:
    *errmsg = &er_comread;
    f_close(&sf, F_KEEP);
    return(FAIL);

format_err:
    *errmsg = &er_fmtcomm;
    f_close(&sf, F_KEEP);
    return (FAIL);

comm_err:
    if (send_stat != SUCCESS)		/* msg could not be sent	*/
    	{
	*errmsg = &er_asysnd;
    	er_asysnd.code = send_stat;
	}
    else				/* error in receiving reply	*/
	{
	*errmsg = &er_asyrcv;
	er_asyrcv.code = recv_stat;
	}
    return(FAIL);
    }
#endif


/*	getpst - get one PDF statement.
 *	Skips comments & null lines.
 *	The statement may result from multiple single PDF lines, all
 *	but the last ending in "+".  As these multiple lines are concatenated,
 *	each continuation "+" is replaced with a blank.
 */

    FUNCTION CODE getpst (pdffil, sttmt, records, num_prec, first_rec)

    struct SFILE	*pdffil;	/* in/out: PDF file context	*/
    TEXT		sttmt[];	/* out: PDF statement		*/
    COUNT		*records;	/* out: number of records read	*/
    COUNT		*num_prec;	/* out: # comments, null lines	*/
					/*      preceding 1st real sttmt rec*/
    struct POSCTX	*first_rec;	/* out: position of 1st record	*/

    {
    CODE		code;
    CODE		cmdcode;	/* SUCCESS, CONTINUE, or FAIL	*/
    COUNT		nrec;		/* number of records flushed	*/
    BOOL		cont;
    BOOL		in_sttmt;	/* TRUE if actual statement started*/
    TEXT		inbuf[CMDLINSIZ+1];
    COUNT		recsize;

    *records  = 0;
    *num_prec = 0;
    in_sttmt  = FALSE;
    sttmt[0]  = EOS;
    cont = FALSE;
    do
	{
	if ((code = f_readstring (pdffil,CMDLINSIZ, inbuf)) != SUCCESS)	/* get single PDF line	*/
	    goto gp_rderr;
	(*records)++;
	if (!in_sttmt) (*num_prec)++;		/* assume comment or null line*/
    	if (!cont) f_movpos (&(*pdffil).posctx, first_rec);  /* save pos'n of 1st rec */
	if (NULLSTR(inbuf))
	    {
	    cont = TRUE;
	    continue;
	    }
	cmdcode = bldcmd(inbuf, sttmt);		/* append to any previous line	*/
	if (!in_sttmt  &&  !NULLSTR(sttmt))
	    {
	    in_sttmt = TRUE;			/* no more preceding comments	*/
	    (*num_prec)--;			/* this line wasn't comment	*/
	    }
	if (cmdcode == FAIL)			/* no more space		*/
	    {
	    flush_cmd(pdffil, inbuf, &nrec);	/* flush the rest of statement	*/
	    *records = *records+nrec;
	    cont = FALSE;
	    }
	else if (cmdcode == CONTINUE)
	    cont = TRUE;
	else					/* if SUCCESS return		*/
	    cont = NULLSTR(sttmt);		/* continue if comment recs	*/
	}
    while (cont);
    return (SUCCESS);

gp_rderr:
    if (code == F_EOF)
	tmmsg(PROCFAIL, "Premature end of proc definition file.", "TAE-PREEOF");
    else
	tmmsg(PROCFAIL, "Error reading proc definition file. %s",
		"TAE-RDERR", (*pdffil).errmsg);
    sttmt[0] = EOS;
    return(FAIL);
    }

/*	
 *	ini_status- initialize $SFI and $SKEY
 */

    FUNCTION VOID ini_status ()

    {
    IMPORT struct VARIABLE *sfi_gbl;	/* pointer to global $SFI	*/
    IMPORT struct VARIABLE *skey_gbl;	/* pointer to global $SKEY	*/

    static TAEINT	one   = 1;	/* value for $SFI		*/


    IVAL(*sfi_gbl,0) = one;
    set_string (skey_gbl, "");
    return;
    }

/*	interrupt - handle TAE interrupt mode processing.
 *
 */

    FUNCTION CODE interrupt (ctx)

	struct CONTXT	*ctx;		/* in: context of executing proc  */

    {
    IMPORT CODE		cmd_mode;	/* dynamic vs interrupt vs norm	*/
    IMPORT CODE		usermode;	/* for prccmd loop; menu or cmd	*/
    IMPORT struct SFILE	prcfil;		/* context of proc that requested parms	*/
    IMPORT struct CONTXT *curproc;		/* pointer to cur proc	*/
    IMPORT TEXT		next_cmd[];
    IMPORT struct CONTXT primctx;		/* primary context	*/
    IMPORT struct SFILE	*pstdo_fil, *saved_pstd;/* ptr to stdout SFILE	*/
    IMPORT struct VARIABLE  *skey_gbl, *sfi_gbl;
    IMPORT	  BOOL  int_enabled;		/* TRUE if ENABLE-INT	  */


    struct SFILE	saved_fil;
    CODE		saved_mode;
    CODE		saved_user;
    TEXT		cmdstr[CMDLINSIZ+1];
    CODE		code,scode;
    TEXT		new_prompt[STRINGSIZ+1];
    TEXT		skey_save[STRINGSIZ+1];
    TAEINT		sfi_save;
    struct CONTXT	*saved_cur;

    if (!(*ctx).interrupt)			/* proc inhibits intrpt mode? */
	return (DO_CONTINUE);
    if (!int_enabled)				/* intrpt mode not allowed? */
	return (DO_ABORT);			/* if not, do an abort	    */
    s_copy (SVAL(*skey_gbl,0), skey_save);	/* save status globals	    */
    sfi_save = IVAL (*sfi_gbl,0);
    saved_mode = cmd_mode;			/* save old command mode    */
    cmd_mode   = INTMODE;			/* proc interr mode	    */
    saved_user = usermode;			/* saved old user mode	    */
    usermode   = CMDMODE;			/* stay here until 'MENU'   */
    saved_cur  = curproc;			/* save ptr to 'current' ctx*/
    curproc    = &primctx;			/* for err msgs		    */
    MOVE_STRUCT(prcfil, saved_fil);		/* save the calling file    */
    saved_pstd = pstdo_fil;
    pstdo_fil = NULL;				/* implies term'l, by convention */
    build_prompt(new_prompt);			/* build interrupt prompt */
    next_cmd[0] = EOS;				/* null help cmd to start */
    do
    	{
    	if (NULLSTR(next_cmd))			/* no saved HELP cmd?	*/
    	    {
	    if (usermode == CMDMODE)
		cmd_noscreen (A_LASTCMD, new_prompt, cmdstr);
	    else				/* assume menu mode	*/
		menmod(cmdstr);			/* display menu 	*/
    	    }
	else
    	    {					/* HELP supplied the lin*/
    	    s_copy(next_cmd, cmdstr);
    	    next_cmd[0] = EOS;
    	    }
	if (NULLSTR(cmdstr))			/* if null input...	*/
	    continue;				/* get command again	*/
	history_save (cmdstr);
    	code = prccmd(cmdstr, &primctx);	/* process the command	*/
    	}
    while ( (code != DO_ABORT) && (code != DO_CONTINUE));
    MOVE_STRUCT(saved_fil, prcfil);		/* restore prcfil	*/
    cmd_mode = saved_mode;			/* restore old mode	*/
    usermode = saved_user;
    curproc = saved_cur;			/* restore the cur proc	*/
    pstdo_fil = saved_pstd;			/* go back to old stdout */
    set_string (skey_gbl, skey_save);		/* restore status	*/
    IVAL(*sfi_gbl,0) = sfi_save;
    return(code);
    }

/*	int_find - Find an internal proc the specified name
 *
 *	returns the address of its ipblock or NULL
 */
    FUNCTION struct IPBLOCK *int_find(ctx, name)

    struct CONTXT	*ctx;		/* in: proc context 		    */
    TEXT 		name[];		/* in: name of proc to find	    */

    {
    struct IPBLOCK	*ipbpt;
    struct CONTXT	*tmpctx;

/* keep looking in outer nested contexts until we're not in an internal procs */
    tmpctx=ctx;
    for (tmpctx=ctx;(s_equal((*tmpctx).pdf.libr, "/LOCAL/")); tmpctx=(*tmpctx).backlink)
    	{
	if ((*tmpctx).int_procs == NULL)
	    continue;
	for (ipbpt = (struct IPBLOCK *) (*tmpctx).int_procs; ipbpt != NULL; ipbpt = (*ipbpt).flink)
	    if (s_equal ((*ipbpt).procname, name))
		return(ipbpt);
    	}

    if ((*tmpctx).int_procs == NULL)		/* check the first non-local */
	return(NULL);
    for (ipbpt = (struct IPBLOCK *) (*tmpctx).int_procs; ipbpt != NULL; ipbpt = (*ipbpt).flink)
	if (s_equal ((*ipbpt).procname, name))
	    return(ipbpt);
    return (NULL);
    }

/*	make_gbl. Move values of all PARMs to corresponding globals.
 *		  This is "execution" of a GLOBAL PDF.
 *
 *	NOTE: 	New variables are inserted to the global symbol table
 *		in alphabetically ascending order.
 *
 *	We don't have to chk_vector here: the declaration of
 *	a PARM in a GLOBAL PDF makes sure that the PARM and
 *	the corresponding global are compatible.  (See gbl_defaults
 *	in DECLARE.C.)
 */
    FUNCTION static CODE make_gbl (pctx)

    struct CONTXT	*pctx;		/* in/out: global proc context	*/
    {
    IMPORT struct SYMTAB glbtab;	/* global symbol table		*/
    IMPORT struct CONTXT primctx;	/* level 0 context		*/

    struct  VARIABLE	*pv;		/* current PARM			*/
    struct  VARIABLE	*ppv;		/* previous PARM in parmst	*/
    struct  VARIABLE	*lgv;		/* last global in glbtab	*/
    struct  VARIABLE	*gv;		/* current global		*/
    struct  DEFPDF	*defpdf;
    CODE		code;
    struct  VARIABLE	*find_slot(), *lookex();

    defpdf = NULL;
    ppv = (struct VARIABLE *) &(*pctx).parmst.link;	/* previous pointer */
    for (pv=(*pctx).parmst.link; pv != NULL; )
	{
	gv = lookex (&glbtab, (*pv).v_name);
	if (gv == NULL)				/* define new global	*/
	    {
	    if (lookex (&primctx.locst, (*pv).v_name) != NULL)
		{
		tmmsg (PROCFAIL,
		"Global variable name '%s' conflicts with level 0 variable.",
		"TAE-LOCCONFLICT", (*pv).v_name);
		ppv = pv;				/* previous PARM  */
		pv = (*pv).v_link;			/* current PARM	  */
		continue;
		}
            (*ppv).v_link = (*pv).v_link;	/* unlink from parmst	*/
	    lgv = find_slot((*pv).v_name, &glbtab); /* find slot in tbl */
    	    if (lgv == NULL)
    		{
    		(*pv).v_link = glbtab.link;
    		glbtab.link = pv;		/* link at beginning	*/
    		}
    	    else
    		{
    		(*pv).v_link = (*lgv).v_link;	
		(*lgv).v_link = pv;		/* link into glbtab	*/
    		}
	    deltut(pv);				/* delete tutor extent	*/
	    (*pv).v_class = V_GLOBAL;		
	    (*pv).v_protect = FALSE;
	    (*pv).v_intrinsic = FALSE;
	    (*pv).v_implicit = FALSE;
	    (*pv).v_refcnt = 0;			
	    if (defpdf == NULL)			/* build DEFPDF		*/
	        {
		defpdf =
		    (struct DEFPDF *) tae_alloc (1, sizeof(struct DEFPDF));
		(*defpdf).refcount = 0;
		MOVE_STRUCT ((*pctx).pdf, (*defpdf).pdf);
		}		
	    (*defpdf).refcount ++;
	    (*pv).v_pdf = defpdf;	        	
	    pv = (*ppv).v_link;			/* next parm; ppv same	*/
	    }
	else
	    {
	    deep_value_copy (pv, gv, (*pv).v_name, (*gv).v_name);
	    ppv = pv;				/* previous PARM	*/
	    pv = (*pv).v_link;			/* current PARM		*/
	    }
	}
    return (DO_SUCCESS);
    }  	    	

/*	deep_value_copy
 *
 *	Does a value copy from source variable to 
 *	destination variable, including qualifiers
 *	to any level.
 *
 *	The return code here is a chk_vector return code
 *	or SUCCESS.
 *	
 */
    FUNCTION CODE deep_value_copy (sv, dv, sname, dname) 

    	struct VARIABLE	*sv;			/* in: source variable	*/
        struct VARIABLE *dv;			/* in: dest variable	*/
        TEXT		sname[];		/* in: source name	*/
        TEXT		dname[];		/* in: dest name	*/

    {
    CODE		code, worst_code;
    struct VARIABLE	*dvq;			/* dest qualifier	*/
    struct VARIABLE	*svq;			/* source qualifier	*/
    TEXT		qsname[STRINGSIZ+1];
    TEXT		qdname[STRINGSIZ+1];

    dv = RESOLVE(dv);
    code = chk_vector (dv, (*sv).v_type, (*sv).v_cvp, (*sv).v_count, FALSE);
    if (code == SUCCESS)
        set_value (dv, (*sv).v_cvp, (*sv).v_count);
    else
	{
	tmmsg (PROCFAIL, 
		"Variable '%s' not compatible with variable '%s'.",
    		"TAE-MISMATCH", dname, sname);
        return (code);
	}

    /*  Now do qualifiers (the "deep" part)				*/

    worst_code = SUCCESS;
    for (dvq = (*dv).v_qualst.link; dvq != NULL;  dvq = (*dvq).v_link)
    	{
    	svq = lookex (&(*sv).v_qualst, (*dvq).v_name);
	if (svq == NULL)
	    continue;		/* exact structure match not required	*/

	/* build variable names for recursive call to deep_value_copy	*/

	s_copy (sname, qsname);
	s_append (".", qsname);
	s_append ((*svq).v_name, qsname);
	s_copy (dname, qdname);
	s_append (".", qdname);
	s_append ((*dvq).v_name, qdname);

	code = deep_value_copy 	(svq, dvq, qsname, qdname); 
	if (code != SUCCESS)
            worst_code = code;	
    	}
    return (worst_code);
    }

/*
 *	outputs.   Set output values from a process.
 *
 *	A process has sent a parblk containing output values.
 *
 */

    FUNCTION CODE outputs(procctx, parblk)

    struct CONTXT	*procctx;	/* in: proc context		*/
    struct PARBLK	*parblk;	/* in: PARBLK from process	*/

    {
    COUNT		i;
    struct VARIABLE	*v;
    CODE		code;

    /* Strategy: the 'candidates' for output are global refs and parameters
     * of TYPE=NAME.  For each candidate, check to see if the parblk
     * contains a corresponding value.
     */

    for (i=0; i < (*procctx).numrefs; i++)
	{
	v = (*procctx).refs[i];
        code = set_outval(v, parblk);
	if (code != SUCCESS)
	    goto outval_error;
	}
    for (v=(*procctx).parmst.link; v != NULL; v=(*v).v_link)
        {
	if ((*v).v_type == V_NAME)
	    {
	    code = set_outval(v, parblk);
	    if (code != SUCCESS)
		goto outval_error;
	    }
	}
    return (SUCCESS);

outval_error:
    tmmsg(PROCFAIL, "Invalid output value for '%s' in proc '%s'.",
	"TAE-INVOUT", (*v).v_name, (*procctx).pdf.name);
    return(FAIL);
    }

/*
 *	 prsstt - parse statement and build symbol table for this statement.
 *
 *	For context reasons, we don't do substitution here; the impact
 *	is that introductory commands cannot have substitution if
 *	HELP is to work.
 *
 */

    FUNCTION CODE prsstt (sttmt, itrcmd, pdfstb)

    TEXT		sttmt[CMDLINSIZ+1];	/* in/out:  PDF statement*/
    struct ITRCMD	**itrcmd;		/* out: ITRCMD for cmd	 */
    struct SYMTAB	*pdfstb;		/* out: cmd symbol table */

    {
    struct SYNBLK	sb;			/* syntax block 	*/
    TEXT		cmd[FSPECSIZ+1];
    TEXT		subcmd[SUBCMDSIZ+1];
    TEXT		label[LABELSIZ+1];
    struct ITRCMD	*intrin();		/* get ITRCMD pointer	*/
    struct ITRCMD 	*itrsub();		/* get subcmd pointer	*/
    CODE		code;

    code = cmd_parse(&sb, sttmt, label, cmd, subcmd);
    if (code != SUCCESS)
        goto pd_synerr;
    *itrcmd = intrin(cmd);			/* find command in tables*/
    if (*itrcmd == NULL)	
        goto pd_niter;
    *itrcmd = itrsub(subcmd, *itrcmd);		/* find subcommand 	*/
    if (*itrcmd == NULL)
        goto pd_niter;
    if (memtab((**itrcmd).partab, (**itrcmd).numprm, pdfstb) != SUCCESS)
        return(FAIL);
    if (updtab(pdfstb, &sb) != SUCCESS)		/* update from cmd string*/
        return(FAIL);	
    return(SUCCESS);

pd_synerr:
    tmmsg(PROCFAIL, sb.errmsg, "TAE-FMTERR");
    return(FAIL);

pd_niter:
    tmmsg(PROCFAIL, "Unrecognized command '%s'.",
	"TAE-UNRECCMD", cmd);
    return(FAIL);
    }

/*
 *	set_outval.    Set output variable if a corresponding
 *	value exists in the PARBLK.
 */

    FUNCTION static CODE set_outval(v, parblk)

    struct VARIABLE	*v;		/* in/out: VARIABLE to set	*/
    struct PARBLK	*parblk;	/* in: PARBLK from process	*/

    {
    struct VARIABLE	*pv, *rv;
    CODE		code;

    pv = lookex(&(*parblk).symtab, (*v).v_name);
    if (pv == NULL)
        return (SUCCESS);	
    rv = RESOLVE(v);	
    code = chk_vector(rv, (*pv).v_type, (*pv).v_cvp, (*pv).v_count, FALSE);
    if (code != SUCCESS)
	return (FAIL);
    code = set_value(rv, (*pv).v_cvp, (*pv).v_count);
    if (code != SUCCESS)
	return (FAIL);
    return (SUCCESS);
    }

/*
 *	 opnpdf - find PDF and open it.
 *
 *	Return code:
 *
 *	SUCCESS -- pdf opened successfully.
 *	FAIL --  pdf cannot be opened
 *
 *	Upon successful return, (*pctx).pdf is the FSBLOCK of the PDF.
 */

    FUNCTION CODE opnpdf (parctx, pctx, cmdstring, file)

    struct CONTXT	*parctx;	/* in: parent proc context		*/
    struct CONTXT	*pctx;		/* in/out: proc context			*/
    TEXT		cmdstring[];	/* in: command string: may be a file spec */
    struct SFILE	*file;		/* out: the file control block		*/

    {
    CODE		code;		
    struct CONTXT	*ctx;
    TEXT		errstr[STRINGSIZ+1];
    struct IPBLOCK	*ipbpt, *int_find();

/* before hierarchy search, see if the proc is internal		*/
    if ((ipbpt = int_find (parctx, cmdstring)) != NULL)
    	{		/* first find the file spec ("/LOCAL/" --> internal ) */
        for (ctx = parctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); ctx = (*ctx).backlink);
	code = f_opnblk (file, PDFLUN, &(*ctx).pdf, F_READ);  	/* open it   */
	if (code != SUCCESS)
	    {
	    tmmsg (PROCFAIL, "Error finding internal proc. Code '%s'.",
			"TAE-INTPDF", (*file).errmsg);
	    return (FAIL);
	    }
    	f_setpos (file, &(*ipbpt).position);	/* position to int'l proc   */
    	(*file).posctx.possav = TRUE;
    	s_copy ((*ipbpt).procname, (*pctx).pdf.name); /* retain file name	    */
    	s_copy ("/LOCAL/", (*pctx).pdf.libr); 	/* flag for local	    */
    	(*pctx).pdf_line = 0;			/* line # relative to int'l proc */
    	return (SUCCESS);
    	}
    code = hierarchy(file, cmdstring, PDF_TYPE, PDFLUN, &(*pctx).pdf, errstr);
    if (code == SUCCESS)
	{
	if (f_isterm (file)) 			/* if terminal 	           */
	    code = FAIL;			/* request TAE-PROCSYN err */
	if (code == SUCCESS)			/* TBD: fix this kludge	*/
	    {
 	    (*pctx).parfile = IsParFile (file);
            return(SUCCESS);
	    }
    	}
    if (code == FAIL)	/* syntax error in file spec: */
	{
	tmmsg (PROCFAIL, "Error in proc name at or near '%s'.",
	       "TAE-PROCSYN", errstr);
	return (FAIL);
	}
    else if (code == F_FILERR)
	{
	tmmsg(PROCFAIL, "Proc '%s' in '%s':  %s.",
	     "TAE-PDFRD", (*pctx).pdf.name, (*pctx).pdf.libr,
	     (*file).errmsg);
	return (FAIL);
	}
    else
	{
	tmmsg(PROCFAIL, "Unable to locate proc '%s'.", "TAE-NOPROC",
	      cmdstring);
	return (FAIL);
	}
    }


/*
 *	opnsav - open (actually, reopen) a proc and position to the previously
 *	saved position.
 *
 *	If pdf.libr is "\LOCAL\", the proc is internal, and we backlink
 */

    FUNCTION CODE opnsav (pctx)

    struct CONTXT	*pctx;		/* in/out: proc context			*/

    {
    IMPORT struct SFILE	prcfil;		/* proc file context			*/

    CODE		code;
    TEXT		buf[STRINGSIZ+1];
    struct CONTXT	*ctx;


/* first find the pdf spec */
    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); ctx = (*ctx).backlink);
    code = f_opnblk(&prcfil, PDFLUN, &(*ctx).pdf, F_READ);
    if (code != SUCCESS)		/* reopen the proc		*/
	return(code);
/* now use the file position from the original, possibly internal, proc	*/
    f_setpos(&prcfil, &((*pctx).prcpos));	/* posit to where we left off		*/
    prcfil.posctx.possav = TRUE;	/* mark file position known		*/
    (*pctx).prcpos.possav = FALSE;	/* posit no longer saved		*/
    code = f_read(&prcfil, buf);	/* & then to next record		*/
    return(code);
    }

/*
 *	pdftab - build symbol table from PDF.
 *	If a GLOBAL PDF, the parm_do function will define the globals along
 *	the way.
 */

    FUNCTION CODE pdftab (pctx, file)

    struct CONTXT	*pctx;		/* in:  proc context			*/
    struct SFILE	*file;		/* in/out: PDF file control block	*/

    {
    IMPORT struct CONTXT *curproc;
    IMPORT struct VARIABLE *sfi_gbl;

    TEXT		sttmt[CMDLINSIZ+1];	/* PDF statement		*/
    CODE		cmdcod;		/* command code				*/
    COUNT		records;	/* number records read for statement	*/
    COUNT		num_prec;	/* # comment, null lines preceding sttmt*/
    struct CONTXT	*save_curproc;
    CODE		startmode;	/* init search mode: SUB_SEARCH or NOT_SPECIAL*/
    CODE		code;

    startmode = (*pctx).special;
    code      = SUCCESS;
    save_curproc = curproc;
    curproc   = pctx;			/* so errors reported correctly	*/
    while (FOREVER)
	{
	if ((*pctx).parfile)  		/* executing PAR file? */
	    {
            code = prc_par_file (file, pctx);
            if (code != SUCCESS) 
    		goto fail;
            break;
	    }
	if (getpst(file, sttmt, &records, &num_prec, &(*pctx).prcpos) 
		    != SUCCESS)			/* get statement	*/
	    goto fail;
	(*pctx).pdf_line += num_prec;		/* adj for comment,null lines */
	cmdcod = prccmd(sttmt, pctx);		/* process the statement*/
	if ((*pctx).compiled)			/* if PROCESS-COMPILED, etc.	*/
	    {
	    code = prc_compiled(file, pctx);	/* PDF is compiled -- process it*/
	    if (code != SUCCESS)
		goto fail;
	    break;
	    }
 	(*pctx).pdf_line += records - num_prec;
	if (IVAL(*sfi_gbl,0) < 0)
	    goto fail;
        if ((*pctx).inbody || cmdcod == DO_RETURN)
    	    break;					/* end of declarations	*/
        if (cmdcod != DO_SUCCESS)
	    goto fail;
	}
    (*pctx).body_line = (*pctx).pdf_line;
    curproc = save_curproc;				/* back to prev context	*/
    if (startmode != SUB_SEARCH  &&  startmode != TUTOR_ATTEMPT  &&
	startmode != COMPILING)
	{
	if (NULLSTR((*pctx).subcmd)  &&  (*pctx).special != SUB_SEARCH)	/* subcommand present?	*/
	    {
	    if (!(*pctx).subfnd && (*pctx).subs)
		{
		tmmsg(PROCFAIL, "Subcommand required.", "TAE-SUBREQ");
		code = FAIL;	
		}
	    }
	else
	    if ((*pctx).special == SUB_SEARCH)		/* if mode changed by subcmd_do*/
		{
		(*pctx).special = NOT_SPECIAL;
		code = FAIL;
		}
	    else if (!(*pctx).subfnd)			/* and not found...	*/
		{
		tmmsg(PROCFAIL, "Unrecognized subcommand '%s'.", "TAE-UNRECSUB",
		    (*pctx).subcmd);
		code = FAIL;
		}
	if (code == FAIL)
	    msg_subs(pctx);				/* disp avail subcmds	*/
	}
    else if (startmode == TUTOR_ATTEMPT)
	if (NULLSTR((*pctx).subcmd)  &&  (*pctx).special != SUB_SEARCH)	/* subcommand present?	*/
	    {
	    if (!(*pctx).subfnd && (*pctx).subs)
		(*pctx).special = SUB_SEARCH;		/* force tutor subc display*/
	    }
    return(code);

fail:
    curproc = save_curproc;				/* back to prev context	*/
    return (FAIL);
    }

/* msg_subs - display a list of available subcommands.
 */

    FUNCTION static VOID msg_subs (ctx)

    struct CONTXT	*ctx;		/* in:  proc context			*/

    {
    struct SUBCMD	*s;
    TEXT		buf[STRINGSIZ+1];
    BOOL		still_adding;
    COUNT		len;

    static TEXT		inibuf[] = "Valid subcommands:  ";

    still_adding = TRUE;
    s_copy(inibuf, buf);
    for (s = (*ctx).subptr; s != NULL; s = (*s).link)
	if (still_adding)
	    if (s_length((*s).name) + s_length(buf) + 5 > STRINGSIZ)
		{
		s_append("...", buf);
		still_adding = FALSE;
		}
	    else
		{
		s_append((*s).name, buf);
		s_append(", ", buf);
		}
    if (still_adding  &&  (*ctx).subptr != NULL)
	{
	len = s_length(buf);			/* strip trailing ", "		*/
	buf[len-2] = EOS;
	}
    if (!s_equal(buf, inibuf))
	tmmsg(PROCFAIL, buf, "TAE-SUBAVL");
    return;
    }

/*
 *	procedure - interpret procedure.
 */

    FUNCTION static VOID procedure (pctx, ecbi)

    struct CONTXT	*pctx;		/* in:  context for this procedure	*/
    struct ECB		*ecbi;		/* in:  event blk for operator interr.	*/

    {
    IMPORT struct SFILE	prcfil;			/* proc file context		*/
    IMPORT struct VARIABLE *sfi_gbl, *skey_gbl;
    TEXT		cmdstr[CMDLINSIZ+1];	/* command string		*/
    struct VARIABLE	*on;			/* onfail VARIABLE		*/
    CODE		cmdcod;			/* command return code		*/
    COUNT		records;
    COUNT		num_prec;	/* # comment, null lines preced sttmt	*/
    COUNT		onfail;			/* _ONFAIL index to execute	*/
    CODE		code;

    on = lookex(&((*pctx).locst), "_ONFAIL");	/* _ONFAIL VARIABLE	*/
    code = cmdcod = DO_SUCCESS;
    while (FOREVER)
	{
	if ((e_occur(ecbi) && interrupt (pctx) == DO_ABORT)
		|| cmdcod == DO_ABORT)
	    {
    	    e_clear (ecbi);
	    if ((*on).v_count > 1)	/* this proc handles ABORT? */
		onfail = 1;			
 	    else
    	        {
	        IVAL (*sfi_gbl, 0) = -10;		/* set abort indicator	*/
	        set_string (skey_gbl, "TAE-ABORT");	/* set abort status	*/
	        return (DO_ABORT);		        /* pass up 		*/
		}
	    }					/* proc handle it	    */
	else if (cmdcod != DO_SUCCESS &&  IVAL(*sfi_gbl,0) < 0)	
    	    onfail = 0;					/* proc failed 		*/
        else
    	    onfail = -1;				/* command executed ok	*/
        if (onfail >= 0)				/* execute _ONFAIL?	*/
	    {
	    s_copy (SVAL(*on, onfail), cmdstr);	
	    if (chk_onfail(cmdstr) != SUCCESS)		/* check command	*/
		return (DO_CHECK);			/* quit if bad command	*/
	    (*pctx).onfailcmd = TRUE;			/* set executing onfail	*/
	    cmdcod = prccmd(cmdstr, pctx);		/* process the command	*/
	    if (cmdcod == DO_CHECK && IVAL(*sfi_gbl,0) < 0)
		return (DO_CHECK);
	    }
	else						/* execute next command	*/
	    {
	    (*pctx).onfailcmd = FALSE;			/* clear onfailcmd indicator */
	    if (getpst(&prcfil, cmdstr, &records, &num_prec, &(*pctx).prcpos) 
			!= SUCCESS)			/* get cmd line */
    		return (DO_CHECK);			/* terminate if error	*/
	    (*pctx).pdf_line += num_prec;	/* adj for comment, null lines*/
	    cmdcod = prccmd(cmdstr, pctx);		/* process the command	*/
	    (*pctx).pdf_line += records - num_prec;
	    }
        if (cmdcod == DO_STOP)
    	    return (DO_STOP);
        else if (cmdcod == DO_RETURN)			/* convert for invoker	*/
    	    return (DO_CHECK);

	/* NOTE: all other return codes are handled as
	   DO_CHECK on next interation	*/
	}
    }

#ifndef PROCESS_CNP
/*
 *	process - run a TAE process.
 *
 *	This function sends a message received handshake to the subprocess
 *	if the message received was of type M_HLOGMSG.  This allows application
 *	software linked before TAE V1.2 to run without the handshake, since
 *	the older software uses the message code M_LOGMSG.
 *
 *	(The compilation of this function may be suppressed by
 *	defining PROCESS_CNP, in which case a host-dependent version
 *	may be written in a process.cnp module, for example, UNIX.)
 */

    FUNCTION CODE process (pctx, ecbi, errmsg)

    struct CONTXT	*pctx;		/* in:  proc context			*/
    struct ECB		*ecbi;		/* in/out: event blk for interrupts	*/
    struct CODE_ERR	*(*errmsg);	/* out: error msg if CODE is FAIL, else "" */

    {
    IMPORT CODE		run_type;	/* ASYNC or BATCH or INTER		*/
    IMPORT CODE		menu_screen;	/* menu screen status indicator		*/
    IMPORT CODE		usermode;	/* user mode: CMDMODE or MENUMODE	*/
    IMPORT struct VARIABLE *skey_gbl, *sfi_gbl;
#ifdef VAX_VMS
    IMPORT TEXT 	*dcl_string;	/* ptr to dcl string to run		*/
    TEXT		*value[1];	/* scrach value vector			*/
#endif
    struct LARGE_PARBLK	parblk;		/* message block to send to subprocess	*/
    struct TCB		tcb;		/* task control block (for c_ package)	*/
    CODE		comcod;		/* task wait completion code		*/
    CODE		status;
    TEXT		exespec[FSPECSIZ+1];
    struct FSBLOCK	fsblock;		
    CODE		code, code2, ret_code;
    TEXT		errstr[STRINGSIZ+1];
    TEXT		deflibe[FLIBRSIZ+1];
    struct CONTXT	*ctx;
    BOOL		asytut_req;	/* tutor for async job ? (false here) */
    struct VARIABLE	*_tutor;
    CODE		packageFlag;	/* used to package valid lists for    */
					/* SELFTUTOR			      */	

/* error messages from function process().  We have them here because
   process cannot output them, having no standard output to output them to
 */

    static struct CODE_ERR er_over=
	{"Parameter values overflow capacity of message to block.",
	"TAE-PSETOVER", 0};

    static struct CODE_ERR er_ini=
    	{"Unable to initiate process; host code = %d.",
	"TAE-PRCSINI", 0};

    static struct CODE_ERR er_notfnd =
	{"Process cannot be found or activated; host code = %d.",
	"TAE-PRCSACT", 0};

    static struct CODE_ERR er_term=
    	{"Abnormal process termination; host code = %d.",
	"TAE-PRCSTRM", 0};

    static struct CODE_ERR er_imsg=
    	{"Error sending initiation message to process; host code = %d.",
	"TAE-IMSGERR", 0};

    static struct CODE_ERR er_rcv=
    	{"Error receiving message from process; host code = %d.",
	 "TAE-RCVERR", 0};

    static struct CODE_ERR er_par=
    	{"Message from process incorrectly formatted.",
	 "TAE-PARBLK", 0};

    static struct CODE_ERR er_snd=
    	{"Error sending message to process, host code = %d.",
	 "TAE-SNDERR", 0};


/* first make sure we get a good library name by finding the parent pdf */
    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); ctx = (*ctx).backlink);
    s_copy ((*ctx).pdf.libr, deflibe);
    f_crack((*pctx).exe_spec, deflibe, (*pctx).pdf.name,
             EXE_TYPE, &fsblock, errstr);	/* apply defaults...	   */
    f_spec(&fsblock, exespec);			/* build file spec to run  */
    zero_block ((GENPTR) &parblk, (GENPTR) parblk.pool - (GENPTR) &parblk);
    parblk.last = FALSE;
    parblk.msgtyp = M_INIPAR;			/* initial parameter block */
    _tutor = lookex (&(*pctx).locst, "_TUTOR");
    packageFlag = (_tutor && IVAL(*_tutor,0)) ? VM_VALID : 0; 
    if (package(pctx, &parblk, sizeof parblk.pool, packageFlag) != SUCCESS)
	goto rp_overr;				
    code = c_act(&tcb, exespec);		/* activate task	   */
    if (code !=SUCCESS) goto rp_tiner;		/* error in queing for activation */
    code = c_snds(&tcb, (GENPTR) &parblk, parblk.blksiz);  /* send init msg blk*/
    if (code != SUCCESS) goto rp_msger;
    e_clear(&tcb.ecbrcv);			/* clear receive efn       */



/* Task started.  Now wait on completion, event, or message.
 * NOTE: We queue to receive the next message from subtask only
 * after a send is complete, or after a message is received from it.
 * So a subtask can not send a message to tm until it has read the
 * block sent to it from tm. (An attempt to do so will hang the subtask).
 */

    *errmsg = NULL;				/* no err msg yet		*/
    ret_code = DO_CHECK;
    while (FOREVER)
	{
    	e_wait(4, &tcb.ecbsnd, &tcb.ecbrcv, &tcb.ecbtsk, ecbi);
			/* wait on send, receive, completion, or event		*/

	if (e_occur(&(tcb.ecbtsk)))		/* if task completion, no more loop */
	    {
	    if (usermode == MENUMODE)		/* if process activate by menu */
		menu_screen = PROMPT_PAINT;	/* prompt before repaint menu  */
#ifdef VAX_VMS
	    if (dcl_string != NULL)		
		{				/* DCL command finished		*/
		value[0] = &(tcb.statmsg[2]);   /* (ignore the %x on front)	*/
		set_value(skey_gbl, (GENPTR)value, 1);	/* move $STATUS to $SKEY	*/
		IVAL(*sfi_gbl,0) = (tcb.compcd == C_NORMAL) ? 1 : -1;
		break;
		}
#endif
	    if (ret_code == DO_ABORT || tcb.compcd == C_NORMAL)
	        break;			/* normal or expected abnormal     */
	    goto rp_wterr;		/* unexpected abnormal termination */
	    }
	else if (e_occur(ecbi))			/* operator attention */
	    {
	    e_clear (ecbi);			/* clear the event    */
	    if ((*pctx).interrupt)		/* interrupt allowed? */
    		{
		c_suspend(&tcb);
		code = interrupt (pctx);	/* do interrupt commands */
		if (code == DO_ABORT)
		    {
		    /* here we initiate an abort but continue the while loop
		       so that messages from exit handlers in the process
		       will be received (especially, output variables).
		    */
		    if (c_iabort(&tcb) != SUCCESS)		/* initiate abort   */
			m_put("Unable to abort process; host code = %d.",
			      "TAE-PRCSABT", tcb.hostcode);
		    e_clear(ecbi);	/* so nested levels won't hit proc int	*/
		    IVAL (*sfi_gbl, 0) = -10;
		    set_string (skey_gbl, "TAE-ABORT");
		    ret_code = DO_ABORT;			/* return code	*/
		    }
		else
		    c_resume(&tcb);
    		}
	    }
	else if (e_occur(&tcb.ecbsnd))			/* if a send 	    */
	    {
	    e_clear(&tcb.ecbsnd);			/* clear  ECB       */
	    code = c_recs(&tcb, (GENPTR)&parblk, sizeof parblk);
	    if (code != SUCCESS) goto rp_rcver;
	    }
	else if (e_occur(&tcb.ecbrcv))			/* a message received */
	    {
	    e_clear(&tcb.ecbrcv);
	    makeabs(&parblk.symtab, parblk.pool);	/* make pointers absolute*/
	    if (chk_parblk (&parblk) != SUCCESS)
	    	goto rp_badparblk;
	    if (parblk.msgtyp == M_FULLPDF  ||  parblk.msgtyp == M_SUBPDF)
		{
    		if (run_type == ASYNC)
    		    {
#ifdef TAE_ASYNC
    		    code = get_pblk_remote (pctx, &parblk, errmsg);
						/* get parms from parent mon */
    		    if (code != SUCCESS) goto rp_rmerr;
#endif
    		    }
    		else
		    {
		    asytut_req = FALSE;			/* tutor for sync proc */
		    dynget(pctx, &parblk, sizeof ( parblk.pool ), 
                                    asytut_req);	/* dynamic parameter 	    */
		    }
		if (parblk.msgtyp == M_DYNEXIT)		/* see change log   */
		    parblk.msgtyp = M_KILLED;		/* for comments     */
		code = c_snds(&tcb, (GENPTR)&parblk, parblk.blksiz);
		if (code != SUCCESS) goto rp_dmser;
		}
	    else if (parblk.msgtyp == M_COMMAND)	/* execute a command line */
		{
		dyncommand (pctx, &parblk, sizeof parblk.pool);
		code = c_snds(&tcb, (GENPTR)&parblk, parblk.blksiz);
		if (code != SUCCESS) goto rp_msger;
		}
	    else if (parblk.msgtyp == M_LOGMSG  ||  parblk.msgtyp == M_HLOGMSG)
	  	{
		logmsg(pctx, &parblk);		/* log the message	*/
		if (parblk.msgtyp == M_HLOGMSG)
		    {				/* handshake		*/
		    parblk.msgtyp = M_CONTINUE;
    		    makerel(&parblk.symtab, parblk.pool);   /* make relative */
		    code = c_snds(&tcb, (GENPTR)&parblk, parblk.blksiz);
		    if (code != SUCCESS) goto rp_msger;
		    }
		else				/* for old applic compatibility	*/
		    {
		    code = c_recs(&tcb, (GENPTR)&parblk, sizeof parblk);
		    if (code != SUCCESS) goto rp_rcver;
		    }
		}
	    else if (parblk.msgtyp == M_OUTPUT)
	        {
	        outputs(pctx, &parblk);		/* apply output values	    */
		code = c_recs(&tcb, (GENPTR)&parblk, sizeof parblk);
		if (code != SUCCESS) goto rp_rcver;
		}
	    else
		{
		dynamic_ins (pctx, &parblk);	/* installation exit	    */
		code = c_recs(&tcb, (GENPTR)&parblk, sizeof parblk);
		if (code != SUCCESS) goto rp_rcver;
		}
	    }
	}
    return (ret_code);	

rp_overr:
    *errmsg = &er_over;
    return (FAIL);

rp_tiner:
    *errmsg = &er_ini;
    er_ini.code = tcb.hostcode;
    return(FAIL);

rp_wterr:
    if (tcb.compcd == C_ACTERR)
    	{
    	*errmsg = &er_notfnd;
    	er_notfnd.code = tcb.hostcode;
    	}
    else
    	{
    	*errmsg = &er_term;
    	er_term.code = tcb.hostcode;
    	}
    return(FAIL);

rp_msger:
    *errmsg = &er_imsg;
    er_imsg.code = tcb.hostcode;
    c_abort(&tcb);
    return(FAIL);

rp_rcver:
    *errmsg = &er_rcv;
    er_rcv.code = tcb.hostcode;
    c_abort(&tcb);
    return(FAIL);

rp_badparblk:
    *errmsg = &er_par;
    c_abort(&tcb);
    return(FAIL);

rp_dmser:
    *errmsg = &er_snd;
    er_snd.code = tcb.hostcode;
    c_abort(&tcb);
    return(FAIL);


rp_rmerr:		/* error on getting parms from remote monitor */
    c_abort(&tcb);
    return(FAIL);
    }
#endif

/*	run_proc - run a proc.
 *
 *	Note that if tutor runs from deep level, run_proc will not
 *	produce a session log of the parameters supplied -- this is
 *	because we only log primary level.
 */

    FUNCTION CODE run_proc (pctx)

    struct CONTXT	*pctx;		/* in:  proc context			*/

    {
    IMPORT struct CONTXT  *curproc;	/* current proc context	(for errs)	*/
    IMPORT struct ECB	ecbi;		/* event block for operator interrupts	*/
    struct CODE_ERR	*errmsg;	/* ptr to an errmsg defined statically */
    CODE		code;
    CODE		code1, code2;
    static	processRunning = FALSE;
    IMPORT CODE         run_type;       /* ASYNC or BATCH or INTER             */

    code1 = code2 = SUCCESS;    /* in case batch mode: they don't get set */

#ifdef SESSION_LOG
    if ((*pctx).prclevel <= 1)			/* if activating from level 0	*/
	slproc(pctx);				/* log the activation		*/
#endif
    pa_ins (pctx);				/* installation exit	*/
    code = DO_CHECK;

#if defined(VICAR_BATCH_LOG) && defined(VAX_VMS)
    if (run_type != BATCH)
    {
        code1 = chg_stdout(pctx);                       /* set new stdout   */
        if (code1 != SUCCESS)
            goto reset;                         /* error exit           */
    }
#else
    code1 = chg_stdout(pctx);                   /* set new stdout       */
    if (code1 != SUCCESS)
	goto reset;				/* error exit		*/
#endif

    if ((*pctx).proctype == Y_PROCEDURE)	/* if a procedure	*/
	{
	curproc = pctx;				/* for error messages	*/
	code = procedure(pctx, &ecbi);		/* interpret procedure	*/
	curproc = (*pctx).backlink;		/* nest up		*/
	}
    else if ((*pctx).proctype == Y_PROCESS)	/* proc is a process	*/
	{
	if (processRunning)
	    {
	    tmmsg (PROCFAIL, 
		"Attempt to nest process execution.", "TAE-NOSYNC"); 
	    return (DO_CHECK);	
	    }
	processRunning = TRUE;
	code = process(pctx, &ecbi, &errmsg);	/* run the process	*/
	processRunning = FALSE;
	}
    else if ((*pctx).proctype == Y_GLOBAL)	/* proc is GLOBAL	*/
   	code = make_gbl (pctx);			/* make parms globals	*/
    else if ((*pctx).proctype == Y_PARMSET)
	code = action_run (pctx);		/* honor ACTION=	*/

reset:
    code2 = re_stdout(pctx);			/* reset stdout		*/
    pt_ins (pctx);				/* installation exit	*/
#ifdef SESSION_LOG
    if ((*pctx).prclevel <= 1)			/* if activated from level 0	*/
	slterm();				/* log the termination		*/
#endif
    if (code1 != SUCCESS || code2 != SUCCESS)	/* stdout related error	*/
	return(DO_CHECK);    	
    if ((*pctx).proctype == Y_PROCESS && code ==FAIL)	/* if process was run, saved errmsg */
    	tmmsg(PROCFAIL, (*errmsg).text, (*errmsg).key, (*errmsg).code);

    /*	
     *	A proc only returns one of the following codes:
     *
     *	   DO_STOP means "return to primary level".
     *	   DO_CHECK means invoker must check the $SFI/$SKEY.
     *	   DO_ABORT means pi mode ABORT has been done.
     *
     *	The following code is for safety to enforce this rule.
     *
     *	Note that a proc does not have the capability to return DO_RETURN
     *	or DO_SUCCESS -- these are special codes reserved for intrinsics.
     */

    if (code == DO_STOP || code == DO_ABORT)
        return (code);
    return (DO_CHECK);
    }
