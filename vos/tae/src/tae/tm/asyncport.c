/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*	Async Processing Routines
 *	-------------------------
 *
 * CHANGE LOG:
 *
 *	10-feb-84	initial code...ne
 *	01-mar-84	split out non-port part...ne
 *	15-mar-84	fix REPLY returns...ne
 *			re-write SHOW
 *	17-apr-84	Update for UNIX implementation...dm
 *	03-may-84	correct Y_ and do_ returns...ne
 *	04-may-84	new _gbl and IVAL, etc...cs
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	07-may-84	Clean-up the usage of IMPORT...lim
 *	17-may-84	Add code to showasy function for SHOW-ASYNC and
 *			SHOW-WAITING to honor control/c...lim
 *	17-may-84	New hierarchy and f_crack calling sequences...palm
 *	17-may-84	Fix for loop in showasy...palm
 *	06-jun-84	Change err msg on bad job creation...nhe
 *	14-jun-84	Return DO_CHECK on WAIT detection of a failure...nhe
 *	24-jun-84	Fix  T B D  (move globals to globals.c)...palm
 *	09-jul-84	Portability bug fixes as implemented for UNIX...dm
 *	01-aug-84	Add file v#'s to standard output name...nhe
 *	06-aug-84	New async/batch activation strategy...palm
 *	14-aug-84	Set sfi to -1 on ABORT or TUTOR EXIT...nhe
 *	16-aug-84	pr396: I/O error key consolidation...peb
 *	09-sep-84	pr810: sho job name in REPLY...nhe
 *	11-sep-84	Accept JOB qualifier and look at NODE...nhe
 *	27-sep-84	pr846: add emit, change async_exit to not emit...nhe
 *	01-oct-84	Add get_remote_pdf and other RCJM handling...nhe
 *	17-oct-84	PR715: async getpar on locals/globals...jtm
 *	25-oct-84	Handle HOLD; do new SHOW-ASY format...nhe
 *	02-nov-84	Add other updates for RCJM implementation...dm
 *	03-nov-84	Add conditional compilation flag for remote agent...dm
 *	06-nov-84	copy parfile library name in run_remote()...dm
 *	16-nov-84	Looping parent agent, other rcjm stage-2 changes...dm
 *	08-dec-84	PR898: change $TUTOR(2) reference to $DYNTUT...lia
 *	09-dec-84	Put back emit_do() for non-RCJM async processing...dm
 *	10-dec-84	Add processing for ASYNCEND qualifier...nhe
 *	11-dec-84	Conditionalize code w.r.t. ASYNCEND in RCJM agent,
 *			other RCJM related changes...dm
 *	02-jan-85	RCJM related bug fixes and cleanups...dm
 *	14-jan-85	Move send_exit_msg() etc to TAE_RCJM defined area...dm
 *
 **********   CHANGES MADE IN THE RCJM TREE ****************
 *
 *	15-jan-85	Other minor rcjm related changes...dm
 *	28-jan-85	Update for remote job abort...dm
 *	06-feb-85	Incorporate changes due to deletion of the remote
 *			agent, put remote node communication into modules
 *			such as emit_do...dm
 *	09-feb-85	Fix bugs reported in rcjm PR #012...dm
 *	14-feb-85	Re-order listing alphabetically...dm
 *	15-feb-85	Add remote help...nhe
 *	22-feb-85	Clean-up and more bug fixes...dm
 *	04-mar-85	Implement enable-/disable-/show-path commands...dm
 *	07-mar-85	Add get_rem_tuthelp to get remote help for tutor...nhe
 *	08-mar-85	Breakup to asyncport.c and asyncrem.c...dm
 *	15-mar-85	Fix bug related to abnormal term of remote job...dm
 *	17-mar-85	Update for new rel_rsrc calling argument...dm
 *	20-mar-85	Remove 'static' declaration of get_jobname()...dm
 *	31-mar-85	Add get_parm_locpar() for getting parameters from
 *			parent tm running locally...dm
 *	09-apr-85	Update reply_do to abort remote jobs...dm
 *	19-apr-85	Update reply_do to specify acb in context block...dm
 *	26-apr-85	Update for new mark_path_del calling seq...dm
 *	29-apr-85	Update for new remote job abort strategy...dm
 *	29-may-85	Update message for aborting remote job...dm
 *
 ********  MERGE FOLLOWING TAE-V1.3 CHANGES ***********...dm (23-may-85)
 *
 *	27-jan-85	Fix crash on RUNT=(AYSNC,very long name)...nhe
 *	31-jan-84	NOWAIT key for wait during reply to REPWAIT...nhe
 *	06-may-85	Fix dimension with SIZ to SIZ+1...lia
 ***************************************************************************
 *
 *	14-jun-85	Extra args to create_mon for UNIX compatibility...dm
 *	01-jul-85	Fix UNIX compilation errors...dm
 *	23-jul-85	Fix UNIX lint errors...dm
 *	22-aug-85	Add emit_stat() for sending status code to 
 *			parent TM...la
 *	26-aug-85	PR 806: Check for exit from dyn tut and send msg back to
 *			spawned proc (asy_getp,get_parm_locpar,reply_do)...dab
 *	29-aug-85	PR 967: Update upsymb() to include parm qualifiers if
 *			available...lia
 *	09-sep-85	PR 851: Distinguish between 'user interrupt'
 *			or 'error sending msg to child' in reply_do...dab
 *	12-sep-85	PR 995: Remove async job completion message code in
 *			in reply_do (done in check_asy_done now). asy_rcv_done
 *			no longer sets async_request when async job done...dab
 *	23-sep-85	PR 1050: Change find_oldest to check that async job is
 *			is "waiting" AND "active" to prevent inactive jobs
 *			waiting job copletion msgs. from being "found"...dab
 *	03-oct-85	PR 946: Added "not compiling" (FALSE) parameter to
 *			calling sequence of 'chk_vector'...dab
 *	15-jul-86	Not checked out; changes made to 
 *			8-OCT-1985 08:30 version for ASYNC-PROCESS...palm
 *	16-jul-86	Fix  T B D  on ACB.aprocess...palm
 *	16-jul-86	set/use new m_parent_job and m_child_job in MON_MSG
 *			...palm
 *	12-sep-86	Move additional changes made during integration of
 *			Interprocess Communication feature to SUN UNIX:
 *			- add call to async_stdout in run_local_process for UNIX
 *			- correct code generating compilation error...lia
 *	17-sep-86	Set new $PARENT global in get_asyctx; use
 *			$JOB for _PARENT to children...palm
 *
 **********  1.4 MERGE: LATE CHANGES MADE IN THE RCJM TREE (30oct86 - ne) ***
 *
 *	29-aug-85	Update find_id() for RCJM implementation...dm
 *	30-Oct-86	Add conditionals for non-VMS RCJM...nhe
 *	28-mar-87	New call sequence to package usign LARGE_PARBLK...palm
 *	22-jul-87	As part of attempt to force TM modules into libraries
 *			added get_asynccmd ()...ljn
 *	07-aug-87	PR1223: zero blocks before using so spare field can 
 *			really be used in future...palm
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *      08-sep-87       Add size argument in call to dynget...tpl
 *	02-dec-87	Bring asy_rec_s outside of VAX_VMS conditional
 *			in rcv_msg_w...dm
 *	24-mar-88	Added braces to structure initializer...ljn
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	01-dec-88	New package call sequence...palm
 *	26-jan-89	MAXVAL -> INT_MAXVAL for RESIDVAR...palm	
 *	23-may-90	Removed RCJM stuff by referring to old TAE_RCJM...ljn
 *	28-jun-90	Removed get_asynccmd()...ljn
 *	01-aug-91	Braces for string initializers...ljn
 *	11-may-92       Used P_BIGSENTINEL instead of P_SENTINEL...tpl
 *	22-oct-92	Prototype of tae_alloc unecessary and Ultrix 4.3
 *			does not like it...rt
 */

#include	"taeconf.inp"	/* TAE configuration definitions	    */
#include	"comminc.inp"	/* Interprocess communication		    */
#include	"tminc.inc"	/* TM-only host-independent definitions	    */
#include 	"terminc.inc"	/* terminal package definitions		    */
#include 	"tmhost.inp"	
#include 	"parblk.inc"
#include 	"taskinc.inp"	/* task manipulation definitions	    */
#include	"asyncinc.inc"	/* async related definitions		    */
#include	"syninc.inc"	/* syntax package			    */


    GLOBAL int	v182async = 0;	/* source version			    */

/**************** source globals ********************************************/

#define ACB_NAME_C      3       /* column positions for SHOW, start at 0    */
#define ACB_PROC_C      19
#define ACB_STATE_C     29
#define ACB_SFI_C       39
#define ACB_SKEY_C      51

/***********/
    static TEXT show_hdr[] =
"\n      Name           Proc     State          sfi    skey\n";


    static struct RESIDVAR em_parms[] =	/* EMIT			*/
        {
/* name	    type     k minc  max   size	  dc  val        dvp */
"$SFI",   V_INTEGER, 0, 0, 1,        0,   0, NULL,    NULL,
"$SKEY",  V_STRING , 0, 0, 1,  STRINGSIZ, 0, NULL,    NULL
        };


    static  struct RESIDVAR  reply_parms[] =	
	{
	/* name		type	k minc  maxc	 size	dc	val   dvp */
	
    	"JOB", 	    V_STRING,   0, 0,   1, JOBNAMESIZ, 0,  NULL, NULL
	};

    static  struct RESIDVAR  rm_parms[] =
	{
	/* name		type	k minc  maxc	 size	dc	val   dvp */
	
    	"JOB", 	    V_STRING,   0, 1,  INT_MAXVAL, JOBNAMESIZ, 0,  NULL, NULL
	};

    static  struct RESIDVAR  wait_parms[] =
	{
	/* name		type	k minc  maxc	 size	dc	val   dvp */
	
    	"JOB", 	    V_STRING,   0, 1,  INT_MAXVAL, JOBNAMESIZ, 0,  NULL, NULL
	};

    CODE		emit_do(), remove_do(), reply_do(), waitasy_do();

#define Y_EMIT	Y_PRIM | Y_PROC | Y_BODY | Y_PROCSYN | Y_CMD | Y_ASYNC	/* async only */

    GLOBAL struct ITRCMD asynccmd[] =
    {
{0, "EMIT",  "",       Y_EMIT,     I_NPM(em_parms), em_parms, emit_do },
{4, "REMOVE","",       Y_GENERAL,     1,   rm_parms,     remove_do },
{4, "REPLY", "",       Y_GENERAL,     1,   reply_parms,  reply_do },
{0, "WAIT", "ASYNC",   Y_GENERAL,     1,   wait_parms,   waitasy_do},
{0, ""}		/* TERMINATOR ENTRY */
     };


/*
 *	asy_getp - Get parameter values for an async procedure
 *
 *	This function is called by reply_do when it is determined
 *	that the requesting asynchronous proc issued a getpar TCL
 *	command. This processing is different from the processing
 *	performed for xqdynp, since a getpar request may request values
 *	for locals and globals, while xqdynp may request only parameters.
 *
 */
    FUNCTION VOID asy_getp (ctx, parblk, pool_size)

    struct CONTXT	*ctx;		/* context of executing proc */
    struct PARBLK	*parblk;	/* in/out: parameter block   */
    FUNINT              pool_size;      /* in: pool size             */

    {
    struct VARIABLE	*v, *new_var;
    struct VARIABLE	*allvar(), *lookex();	
    static struct SYMTAB nullst = {NULL};	/* Null symbol table */
    struct CONTXT	paramctx;	/* Context block for tutor */
    struct SFILE	dynpdf;		/* SFILE for dynamic tutor PDF */
    BOOL		dash_present;	/* TRUE if dash present */
    FAST CODE		code;

/*	Now, build a context block for use by dynamic tutor */

    code = bld_pdf_ctx (ctx, parblk, &paramctx,
	   	&dynpdf, &dash_present);/* Build a context block from pdf */
    if (code == FAIL)
	goto kill;

    code = plcini (&paramctx);				/* Make standard locals */
    if (code != SUCCESS)
	goto postopn_err;
    code = pdftab (&paramctx, &dynpdf);			/* Get remaining context
 *							   data (e.g., help file */
    if (code != SUCCESS)
	goto postopn_err;
    deltab (&paramctx.parmst);				/* Don't need parm st */

/*	Move into the dynamic tutor ctx (paramctx) the variables
 *	that are requested. Note that the requesting TM changed the types to
 *	PARM, regardless of their original type. Therefore, those that are
 *	of class local should be ignored (they were put in the PARBLK by
 *	the remote TM to help find the pdf and to identify the preface).
 */

    for (v = (*parblk).symtab.link; v != NULL; v = (*v).v_link)
	{
	if ((*v).v_class == V_PARM)
	    {
	    new_var = allvar (&paramctx.parmst);	/* Allocate new space */
	    if (new_var == NULL)
		{
		overr();
		goto postopn_err;
		}
	    code = vcopy (v, new_var);			/* Copy parameter */
	    if (code != SUCCESS)
		{
		overr();
		goto postopn_err;
		}
	    }
	}

    code = gp_ins (&dynpdf, &paramctx);
    if (code == DP_NOTDONE)
	{
	v = lookex (&(*parblk).symtab, "_PREFACE");		/* Get preface */
	code = opn_tutor (&dynpdf, &paramctx, NOFORCE, v, TRUE);
	if (code != SUCCESS)
	    goto postopn_err;
	paramctx.asydyntut = TRUE;		/* Tutoring for async job */
	code = tutor (&dynpdf, &paramctx);			/* Do tutor */
	cls_tutor (&dynpdf, &paramctx);
	}
    (*parblk).symtab.link = NULL;
    if (code == DO_RUN)
	{
	if (pack_parm (&paramctx.parmst, parblk, pool_size) != SUCCESS)
	    goto pack_err;
	(*parblk).msgtyp = M_CONTINUE;
	}
    else if (code == DO_HOLD)
    	(*parblk).msgtyp = M_HOLD;    		/* user changed his mind    */
    else
	{
	if (pack_parm (&nullst, parblk, pool_size) != SUCCESS)
	    goto pack_err;
	(*parblk).msgtyp = M_DYNEXIT;		/* user exited from dyn tut */
	}
    clsctx (&paramctx);
    f_close (&dynpdf, F_KEEP);
    return;

pack_err:
    tmmsg (PROCFAIL, "Parameter values overflow process message capacity.",
    	"TAE-PSETOVER");
    goto postopn_err;

postopn_err:
    f_close (&dynpdf, F_KEEP);
    goto clsctx_kill;

clsctx_kill:
    clsctx (&paramctx);
    goto kill;

kill:
    pack_parm (&nullst, parblk, pool_size);	
    (*parblk).msgtyp = M_KILLED;
    return;
    }


/*****************************************************************/
/*	asy_rcv_done - asynchronous routine
 *		       triggered when receive from a mon mbx is complete
 *
 *  NOTE: this function must not touch globals.
 *
 *	Also note that the termination of the async job is handled
 *	diffrently for local tm, RCJM tm and the remote agent.
 *	In the later two cases, the async process (which means the parent
 *	agent or the remote tm) is looping to get the next job request
 *	from its parent process. So we must not release resources such as
 *	the event flags or the mailbox channel. These resources are
 *	released upon a MO_EXIT, which implies exit for the looping child.
 */


    FUNCTION VOID asy_rcv_done (acb)

    struct ACB		*acb;		/* in: pointer to acb		    */

    {
    IMPORT BOOL		async_request;
    IMPORT CODE		run_type;	/* INTER, ASYNC, or BATCH	    */

    struct MON_MSG	*msg;
    struct TCB		*tc;

    static CODE		ticket_num = 0; /* counts number of async requests  */
    CODE		code;
    BOOL		re_queue;	/* true if queue for next message   */

    if (!(*acb).active) return;		/* assume this is msg from aborted job */
    msg = (struct MON_MSG *) (*(*acb).atcb).inmsg;
    tc = (*(*acb).atcb).tcb;
    re_queue = FALSE;
#ifdef VAX_VMS
    (*tc).mb_idle = TRUE;
#endif
    if ((*msg).type == MO_TERM)				/* termination msg   */
	{
	(*acb).active = FALSE;			/* mark job complete */
	rel_rsrc((*acb).atcb);			/* release resources */
	if ((*acb).notify && run_type==INTER)		/* notify on end?    */
    	    {
    	    (*acb).waiting = TRUE;
    	    (*acb).wait_num = ticket_num++;
    	    }
	}
    else if ((*msg).type == MO_STATUS)			/* status msg	    */
	{
	(*acb).sfi = (*msg).m_sfi;
	s_copy ((*msg).m_skey, (*acb).skey);
	re_queue = TRUE;
	}
    else if (((*msg).type == MO_REQUEST) || ((*msg).type == MO_GETPAR))
	{
	s_copy ((*msg).m_filespec, (*acb).requ_file);	/* save name of par file */
    	async_request = TRUE;			/* to flag menu user	*/
	(*acb).waiting = TRUE;
    	(*acb).wait_num = ticket_num++;		/* assign a number	*/
	}
    if (re_queue)				/* should queue for next msg */
	{
#ifdef VAX_VMS
	code = asy_recs(tc, msg, sizeof(struct MON_MSG),
		asy_rcv_done, acb);			/* start another read */
	if (code != SUCCESS)
	    tmmsg (PROCFAIL,
		"Error receiving async monitor message. Host code = %d.",
		"TAE-ASYRCV", (*tc).hostcode);
#endif
	}
    return;
    }

/*  async_abo - Execute ABORT-JOB
 *
 *  returns DO_CHECK
 *
 *  Note that the ITRCMD structure for this is with ABORT (no sub)
 */
    FUNCTION CODE async_abo (procctx, cmdctx)

    struct CONTXT	*procctx;		/* in: proc context	*/
    struct CONTXT	*cmdctx;		/* in: command context  */

    {
    IMPORT struct ACB	*acb_head;		/* acb list-head	*/
    IMPORT COUNT	acbcnt;			/* list count		*/

    struct VARIABLE	*v, *lookex();
    BOOL		all, found;
    struct ACB		*acb;
    COUNT		vnum;
    CODE		code;

    if (acbcnt == 0)
    	{
    	tmmsg (SUCCESS, "No jobs in async job list.", "TAE-EMPTYASY");
    	return(DO_CHECK);
    	}
    v = lookex(&(*cmdctx).parmst, "JOB");
    all = (s_equal(SVAL(*v,0), "ALL"));
    for (vnum = 0; vnum < (*v).v_count; vnum++)	     /* search JOB  */
    	{
    	found = FALSE;
	for (acb = acb_head; acb != NULL; acb = (*acb).link)
	    {
	    if (all || s_equal((*acb).name, SVAL(*v,vnum)))
    		{
		if ((*acb).active)
		    {
		    found = TRUE;
		    abort_acb (acb);	/* abort the job	*/
		    s_copy ("ABORTED", (*acb).skey);
		    (*acb).sfi = -1;
		    tmmsg (SUCCESS, "Job '%s' aborted.", "TAE-ABORTOK",
		        (*acb).name);
		    }
		else if (!all)
		    {
		    found = TRUE;
		    tmmsg (PROCFAIL, "Job '%s' not active.",
			    "TAE-NOABO", (*acb).name);
		    }
    		}
	    }
	if (!found && !all)
	    tmmsg (PROCFAIL, "Job '%s' not in job list.", "TAE-NOJOB",
		    SVAL(*v, vnum));
    	if (!found && all)
    	    tmmsg (PROCFAIL, "No jobs active.", "TAE-ZEROJOB");
    	}
    return(DO_CHECK);
  }

/* 	async_exit - Exit processing for async job
 *
 */
    FUNCTION VOID async_exit ()

    {
    struct MON_MSG		msg;
    CODE			code;

    msg.type = MO_TERM;				/* indicate termination msg */
#ifdef UNIX
    msg.m_childpid = getpid();
#endif
    code = c_snpa ((GENPTR) &msg, sizeof(struct MON_MSG));
    put_stdout ("\nJob terminated.");
    return;
    }

/*  emit_do - Execute the emit command, i.e., send a status message to
 *	      the parent TM
 *
 */
    FUNCTION CODE emit_do (procctx, cmdctx)

    FAST struct CONTXT *procctx;	/* in: context of invoking proc	     */
    FAST struct CONTXT *cmdctx;		/* in: context of cmd line	     */

    {
    IMPORT  struct  VARIABLE    *sfi_gbl;	/* pointer to $sfi	*/
    IMPORT  struct  VARIABLE 	*skey_gbl;	/* pointer to $skey	*/
    IMPORT  CODE		run_type;

    struct VARIABLE 		*v, *lookex();
    struct MON_MSG		msg;
    CODE			code;

    if (run_type != ASYNC)
    	{
    	tmmsg(PROCFAIL, "'EMIT' available only in async jobs.", "TAE-ASYONLY");
    	return(DO_CHECK);
    	}
    v = lookex(&(*cmdctx).parmst, "$SFI");
    if ((*v).v_count != 0)
    	msg.m_sfi = IVAL(*v, 0);		/* use supplied value       */
    else
    	msg.m_sfi = IVAL (*sfi_gbl,0);		/* default is current sfi   */
    v = lookex(&(*cmdctx).parmst, "$SKEY");
    if ((*v).v_count != 0)
    	s_copy (SVAL(*v, 0), msg.m_skey);	/* use supplied value       */
    else
        s_copy (SVAL(*skey_gbl,0), msg.m_skey);	/* default is current skey  */
    msg.type = MO_STATUS;			/* indicate status msg 	    */
#ifdef UNIX
    msg.m_childpid = getpid();
#endif
    code = c_snpa ((GENPTR) &msg, sizeof(struct MON_MSG));
    return(DO_CHECK);
    }


/* 
 *	emit_stat - Sends the status code to the parent TM.
 */
    FUNCTION VOID emit_stat ()

    {
    IMPORT struct VARIABLE	*sfi_gbl;	/* pointer to $sfi	*/
    IMPORT struct VARIABLE	*skey_gbl;	/* pointer to $skey	*/

    struct MON_MSG		msg;
    CODE			code;

    s_copy (SVAL(*skey_gbl,0), msg.m_skey);
    msg.m_sfi = IVAL (*sfi_gbl,0);
    msg.type = MO_STATUS;			/* indicate status msg 	    */
#ifdef UNIX
    msg.m_childpid = getpid();
#endif
    code = c_snpa ((GENPTR) &msg, sizeof(struct MON_MSG));
    return;
    }


/*
 *	get_parm_locpar. Get parameters from local async parent
 */

    FUNCTION  CODE  get_parm_locpar(send_file, recv_file, req_process,
			send_stat, recv_stat)

    TEXT		send_file[];	/* in: name of parfile to send to tm  */
    TEXT		recv_file[];	/* out: name of pafile recved from tm */
    FUNINT		req_process;	/* in: TRUE if request from a process */
    CODE		*send_stat;	/* out: host_code if error in sending */
    CODE		*recv_stat;	/* out: host_code if error in receive */

    {

    struct MON_MSG	msg;		/* monitor-to-monitor message	*/

    msg.type = (req_process) ? MO_REQUEST : MO_GETPAR;
    s_copy (send_file, msg.m_filespec);	/* name of comm'n file		*/
#ifdef UNIX
    msg.m_childpid = getpid();
    *send_stat = c_snpa ((GENPTR)&msg, sizeof (struct MON_MSG));
#else
    *send_stat = c_sndp ((GENPTR)&msg, sizeof (struct MON_MSG));
#endif
    if (*send_stat != SUCCESS) return (FAIL);

/* Get the return message						*/

    *recv_stat = c_rcvp((GENPTR)&msg, sizeof(struct MON_MSG));	/* get the monitor msg */
    if (*recv_stat != SUCCESS) return (FAIL);

    if (msg.type == MO_DYNEXIT) return (DO_EXIT);	/* user exited dyn tut */

    s_copy(msg.m_filespec, recv_file);		/* get received file name  */
    return (SUCCESS);
    }



#ifdef UNIX
/*  find_id - Returns the acb with the specified process id, or NULL
 *
 */
    FUNCTION struct ACB *find_id (pid)

    FUNINT		pid;	/* in: job id			*/

    {
    IMPORT struct ACB *acb_head;

    struct ACB		*acb;

    for (acb = acb_head; acb != NULL; acb=(*acb).link)
    	if ( (*(*acb).atcb).pid == pid && (*acb).active)
    	    return(acb);
    return(acb);			/* last is NULL			*/
    }
#endif


/*  find_job - Returns the acb with the specified job name, or NULL
 *
 */
    FUNCTION struct ACB *find_job (name)

    TEXT		name[];		/* in: job name			*/

    {
    IMPORT struct ACB *acb_head;

    struct ACB		*acb;

    for (acb = acb_head; acb != NULL; acb=(*acb).link)
    	if (s_equal((*acb).name, name))
    	    return(acb);
    return(acb);			/* last is NULL			*/
    }


/* find_oldest - Returns a pointer to the 'oldest' waiting ACB, or NULL
 *		 if none waiting.
 *
 * The oldest is defined to be the one with the lowest acb.wait_num
 */
    FUNCTION struct ACB *find_oldest()

    {
    IMPORT struct ACB		*acb_head;	/* points to acb list  */

    struct ACB			*acb, *oldest_acb = NULL;
    COUNT			found = 0;

    for (acb=acb_head; acb!= NULL; acb=(*acb).link)
    	if ((*acb).waiting && (*acb).active)
    	    {
       	    if (found++ == 0)       /* use if/else-if instead of || because */
    		oldest_acb = acb;		/* oldest_acb may be NULL */
    	    else if ((*acb).wait_num < (*oldest_acb).wait_num)
    		oldest_acb = acb;
    	    }
    return(oldest_acb);
    }


/* find_seq - Find a sequence number for a given proc name
 *
 * returns FAIL if no sequence numbers available
 */
    FUNCTION CODE find_seq (proc, seq)

    TEXT		proc[];		/* in: the proc name		*/
    COUNT		*seq;		/* out: the sequence number	*/

    {
    IMPORT struct ACB	*acb_head;	/* points to first ACB	 	*/

    BOOL		seqhit[MAXACB]; /* each element TRUE if seq no used */
    COUNT		i;
    struct ACB		*acb;


/* Build an array of seq numbers used for this proc; use lowest	unused number */

    for (i = 0; i < MAXACB; i++)
    	seqhit[i] = FALSE;

    for (acb = acb_head; acb != NULL; acb=(*acb).link)
    	if (s_equal(proc, (*acb).pdf.name)) seqhit[(*acb).seq] = TRUE;

    for (i = 0; i < MAXACB; i++)
    	if (!seqhit[i])			/* use index of first FALSE	*/
    	    {
       	    *seq = i;
    	    return(SUCCESS);
    	    }
    return(FAIL);
    }


/* free_acb - free the dynamic storage used by an acb:
 *
 */
    FUNCTION VOID free_acb (acb)

    struct ACB		*acb;		/* in: the acb			*/

    {
    free_atcb((*acb).atcb);		/* first free the atcb		*/
    tae_free ((GENPTR) acb);		/* do this one last		*/
    return;
    }


/* free_atcb - free the dynamic storage used by an atcb:
 *
 */
    FUNCTION VOID free_atcb (atcb)

    struct ATCB		*atcb;		/* in: the atcb			*/

    {
    if ((*atcb).tcb != NULL)		/* if not already released	*/
	tae_free ((*atcb).tcb);		
    tae_free ((*atcb).inmsg);
    tae_free (atcb);
    }


/*	get_asyctx - Get the rest of the monitor context for an async job.
 *		     Note that we get most of it from the RESTORE command in
 *		     the JOB file.
 *
 *	Under UNIX, it also blocks operator interrupt from reaching 
 *	the async monitor (performed as a part of initialization).
 *
 *  returns SUCCESS/FAIL
 */
    FUNCTION CODE get_asyctx ()

    {
    IMPORT CODE		parent_runtype;		/* run_type of parent mon    */
    IMPORT TEXT		comm_file[];
    IMPORT TEXT		job_name[];
    IMPORT TEXT		parent_job[];
    IMPORT struct VARIABLE *job_gbl;		/* ptr to $JOB global	 */
    IMPORT struct VARIABLE *parent_gbl;		/* ptr to $PARENT global */

    CODE		code;
    struct MON_MSG	msg;
    struct FSBLOCK	asy_block;
    TEXT		errstr[STRINGSIZ+1];


#ifdef UNIX
    block_opint();				/* block interrupt from oper */
#endif
    code = c_rcvp((GENPTR) &msg, sizeof(struct MON_MSG));  /* get the monitor msg */
    if (code != SUCCESS)
    	return(FAIL);				 /* we have no stdout yet */
    parent_runtype = msg.m_parmode;		/* save run_type of parent */
    s_bcopy (msg.m_parent_job, parent_job, JOBNAMESIZ);
    s_bcopy (msg.m_child_job, job_name, JOBNAMESIZ);
    set_string (parent_gbl, parent_job);
    set_string (job_gbl, job_name);
    f_crack (msg.m_filespec, "", "", "", &asy_block, errstr);
    s_copy (ASY_TYPE, asy_block.type);
    f_spec (&asy_block, comm_file);	/* save name of file to use for asy comm */
    return(SUCCESS);
    }


/*
 *  get_jobname - Get the user-readable name of the job
 *
 *  The job name is the user specified job name, or, if none
 *  is specified, we take the proc name and, if it's already in
 *  use we append an underscore and a sequence number.  The sequence
 *  numbers are maintained in the acb.
 *
 *  The job name can be specified by the user in the JOB qualifier or as
 *  the second component of the RUNTYPE qualifier.  JOB prevails in a conflict.
 */
    FUNCTION CODE get_jobname (procname, qualst, outname, seq)

    TEXT		procname[];	/* in: proc name		    */
    struct SYMTAB	*qualst;	/* in: pointer to qualifier sym tab */
    TEXT		outname[];	/* out: the name we come up with    */
    COUNT		*seq;		/* out: new sequence number	    */

    {
    struct VARIABLE	*v, *vj, *lookex();		
    TEXT		asc_seq[10];
    TEXT		jobname[STRINGSIZ+1];  /* big because user may foul up */
    struct ACB		*find_job();	/* finds an acb given a job name    */

    jobname[0] = EOS;
    vj = lookex (qualst, "JOB");
    v = lookex (qualst, "RUNTYPE");
    if ((*v).v_count == 2)			/* job name specified? 	    */
    	{
    	if ((*vj).v_count != 0)			/* job specified 2 ways	    */
    	    tmmsg(SUCCESS,
    		"Both JOB and RUNTYPE(2) specified. RUNTYPE(2) ignored.",
    		"TAE-JOBREDUN");
    	else
    	    s_copy(SVAL(*v,1), jobname);
    	}
    if ((*vj).v_count != 0)			/* JOB specified	    */
    	s_copy(SVAL(*vj,0), jobname);
    if (!NULLSTR(jobname))			/* job name specified? 	    */
    	{
    	if (s_equal(jobname, "ALL"))
    	    {
    	    tmmsg(PROCFAIL, "'ALL' is a reserved job name; use another.",
    				 "TAE-RESALL");
    	    return(FAIL);
    	    }
    	if (s_length (jobname) > JOBNAMESIZ)
    	    {
    	    tmmsg(PROCFAIL, "Job name '%s' too long; maximum is %d.",
    			"TAE-LONGJNAME", jobname, JOBNAMESIZ);
    	    return (FAIL);
    	    }
    	if (find_job(jobname) != NULL)
    	    {
    	    tmmsg(PROCFAIL, "Name '%s' already in use.", "TAE-USEDNAME",
    			jobname);
    	    return(FAIL);
    	    }
        s_copy (jobname, outname);
    	return (SUCCESS);
    	}
    s_copy (procname, outname);			/* user no specify   	     */
    if (find_seq (outname, seq) != SUCCESS)	/* find lowest seq for this proc */
    	{
    	tmmsg (PROCFAIL, "Too many async procs with same name.", "TAE-TOOASYNC");
    	return(FAIL);
    	}
    if (*seq != 0)				/* don't use 0		     */
    	{
    	s_append ("_", outname);
    	s_i2s (*seq, asc_seq);
    	s_append (asc_seq, outname);
    	}
    if (find_job(outname) != NULL)
	{
	tmmsg(PROCFAIL, "Name '%s' already in use.", "TAE-USEDNAME", outname);
	return(FAIL);
	}
    return(SUCCESS);
    }

/*	get_syms.	Restores variables after a dynamic encounter
 *			with the parent.
 *
 *  returns SUCCESS/FAIL; aborts if any variable is 'bad'
 *
 */
    FUNCTION CODE get_syms (save_file, ctx)

    TEXT		save_file[];		/* in: name of file to restore */
    struct  CONTXT	*ctx;			/* in/out: context with symbols	*/

    {
    CODE		code;
    struct  SFILE	f;
    struct  VARIABLE	*v1;
    COUNT		recsize;
    struct  PARBLK	p;
    struct  PARHDR	ph;


    code = f_opnspc(&f, SAVELUN, save_file, "", "",
		    ASY_TYPE, F_READ);		/* open for read	*/
    if (code != SUCCESS)  goto open_err;	/* report error		*/

/*  read and check the header record.					*/

    code = f_bread(&f, (GENPTR) &ph, sizeof(struct PARHDR), &recsize);	
    if (code != SUCCESS)  goto read_err;
    if (!s_equal(ph.sentinel, P_BIGSENTINEL))
	goto format_err;			/* not proper format	*/

/*  read the parameter records and restore locals/globals/parms		*/

    while (FOREVER)
	{					/* read each record and process the variables therein */
	code = f_bread(&f, (GENPTR) &p, sizeof(struct PARBLK), &recsize);
   	if (code != SUCCESS) break;
	makeabs(&p.symtab, p.pool);		/* make pointers absolute   */
	if (chk_parblk(&p) != SUCCESS)
	    goto format_err;			/* check internal integrity */

/*  update symbol tables 						    */

    	for (v1 = p.symtab.link; v1 != NULL; v1 = (*v1).v_link)
	    {				
	    code = upsymb(ctx, v1);
	    if (code != SUCCESS) goto up_err;
	    }
	}
    if (code != F_EOF)	goto read_err;	
    f_close(&f, F_DELETE);
    return (SUCCESS);


format_err:
    f_close(&f, F_KEEP);
    tmmsg(PROCFAIL, "Parameter file not properly formatted.",
	     "TAE-FMTRESTR");
    return (FAIL);

open_err:
    tmmsg(PROCFAIL, "Unable to open the restore file, %s",
	    "TAE-OPNRD", f.errmsg);
    return (FAIL);

read_err:
    f_close(&f, F_KEEP);
    tmmsg(PROCFAIL, "Unable to read the restore file, %s",
	    "TAE-RDERR", f.errmsg);
    return (FAIL);

up_err:
    f_close(&f, F_KEEP);
    return(FAIL);
    }

/*	init_acb - Initialize an ACB
 *
 *  returns a pointer to the new ACB or NULL. 
 *
 */
    FUNCTION struct ACB *init_acb (atcb)

    struct ATCB		*atcb;		/* in/out: ptr to job's atcb	    */

    {
    struct ACB		*acbptr;
    struct TCB		*tc;

    acbptr = (struct ACB *)tae_alloc(1, ACBSIZ);	/* allocate ACB	    */
    if (acbptr == NULL)
	goto acb_err;    	
    (*acbptr).sfi = 0;				/* init status      	    */
    (*acbptr).done = FALSE;
    if (atcb == NULL)				/* not yet allocated        */
	{
	(*acbptr).atcb = atcb = (struct ATCB *) tae_alloc(1, ATCBSIZ);
    	if (atcb == NULL)
	    goto atcb_err;
	(*atcb).tcb = tc = (struct TCB *) tae_alloc (1, sizeof(struct TCB));
    	if (tc == NULL)
    	    goto tcb_err;
	(*atcb).inmsg = NULL;
	}
    else					/* for remote jobs only	    */
 	(*acbptr).atcb = atcb;			/* save the atcb pointer    */
    return(acbptr);

tcb_err:
    tae_free((GENPTR) atcb);
atcb_err:
    tae_free((GENPTR) acbptr);
acb_err:
    overr();
    free_acb(acbptr);
    return(NULL);
    }

/*  insert - insert a string at a specified position on the provided record
 *
 */
    FUNCTION VOID insert (s, rec, width)

    TEXT		*s;		/* in: string to insert		*/
    TEXT		*rec;		/* in/out: record		*/
    FUNINT		width;		/* in: max field width		*/

    {
    COUNT		i;

    for (i=0; i < width && *s != EOS; i++, *rec++ = *s++)
    		;
    return;
    }

/*
 *  int2aschex - Integer to ascii/hex, with deletion of leading zeroes.
 *
 */
    FUNCTION VOID int2aschex (i, txt)

    FUNINT		i;		/* in: the integer	*/
    TEXT		txt[];		/* out: the ascii	*/

    {
    sprintf (txt, "%X", i);			/* pid to hex ascii	*/
    while (txt[0] == '0')
        s_shift (txt, 1);			/* delete leading zeros	*/
    return;
    }


/*  link_acb - Link new acb into linked list, keeping list at MAXACB
 *
 *  returns SUCCESS or, if no room, FAIL
 */
    FUNCTION CODE link_acb (acb)

    FAST struct ACB	*acb;		/* in: acb to link in 		*/

    {
    IMPORT struct ACB	*acb_head;
    IMPORT COUNT	acbcnt;

    struct ACB		*ac, *ac2;


    if (acbcnt == MAXACB)		/* too many			*/
    	{				/* remove first inactive job	*/
    	ac2 = acb_head;
    	for (ac = acb_head; (*ac).active; ac = (*ac).link)
    	    {
    	    ac2 = ac;			/* save old acb for thread	*/
    	    if ((*ac).link == NULL)
    		{
    		tmmsg (PROCFAIL, "No room for new job,  MAXACB jobs active.",
    		"TAE-NOASYROOM");
    		return(FAIL);
    		}
    	    }
    	tmmsg(SUCCESS, "No room.  Removing job '%s' from async job list.",
    		"TAE-REMOVE", (*ac).name);    	    	
    	(*ac2).link = acb;
    	(*acb).link = (*ac).link;
    	free_acb (ac);			/* free acb's dynamic stoarage  */
    	}
    else if (acbcnt == 0)		/* first time			*/
	{
	acb_head = acb;			/* srart of chain		*/
	(*acb).link = NULL;
	acbcnt = 1;
	}
    else				/* link to end			*/
    	{
        for (ac = acb_head; (*ac).link != NULL; ac = (*ac).link);
    	acbcnt++;
    	(*ac).link = acb;
    	(*acb).link = NULL;		/* make sure we terminate	*/
    	}
    return(SUCCESS);
    }


/*  rcv_msg_w - Receive a message and wait
 *
 *  returns SUCCESS or FAIL; host error code in tcb
 */

    FUNCTION CODE rcv_msg_w(acb, bufadr, buflen )

    struct  ACB		*acb;    	/* in: acb for the job to recv the msg	*/
    GENPTR		bufadr[];	/* in: the buffer address		*/
    TAEINT		buflen;		/* in: length				*/

    {
    IMPORT struct ECB	ecbi;		/* interrupt ecb			*/

    struct TCB		*tcb;		/* the tcb to send the msg through	*/
    CODE		code;

    tcb = (*(*acb).atcb).tcb;
    code  = asy_recs(tcb, bufadr, buflen, NULL, NULL);
    if (code != SUCCESS)
    	return(FAIL);
#ifdef 	VAX_VMS
    code  = e_wait(2, &(*tcb).ecbrcv, &ecbi);
    if (code != SUCCESS)
    	return(FAIL);
    (*tcb).mb_idle = TRUE;		/* free up the channel. 	    */
    if (e_occur(&(*tcb).ecbrcv))
    	{
    	e_clear(&(*tcb).ecbrcv);
    	return(SUCCESS);		/* msg received ok		    */
    	}
#endif
    if (e_occur(&ecbi))
    	{
    	e_clear(&ecbi);
    	return(KILLED);		/* return 'no more'? Caller can just exit   */
    	}
    return (SUCCESS); 
    }

/*  remove_do - Process the REMOVE command
 *
 *  returns DO_CHECK
 */
    FUNCTION CODE remove_do (procctx, cmdctx)

    FAST struct CONTXT *procctx;	/* in: context of invoking proc	     */
    FAST struct CONTXT *cmdctx;		/* in: context of cmd line for REMOVE */

    {
    IMPORT COUNT	acbcnt;		/* count of acb's		    */
    IMPORT struct ACB	*acb_head;	/* ACB list header		    */

    struct VARIABLE	*v, *lookex();
    BOOL		all, found;
    struct ACB		*acb, *last_acb, *acb1;
    COUNT		job_count;	/* # of jobs on a disabled  path */
    COUNT		vnum;

    if (acbcnt == 0)
    	{
    	tmmsg (SUCCESS, "No jobs in async job list.", "TAE-EMPTYASY");
    	return(DO_CHECK);
    	}
    v = lookex(&(*cmdctx).parmst, "JOB");
    all = (s_equal(SVAL(*v,0), "ALL"));
    for (vnum = 0; vnum < (*v).v_count; vnum++)	     /* search for each job in JOB  */
    	{
    	found = FALSE;
        last_acb = acb_head;
	for (acb = acb_head; acb != NULL; acb = (*acb).link)
	    {
	    if (all || s_equal((*acb).name, SVAL(*v,vnum)))
    		{
    		found = TRUE;
		if (!(*acb).active)
		    {
		    if (acb == acb_head)
			acb_head = (*acb).link;
		    else
			(*last_acb).link = (*acb).link;	    /* remove it      */
		    free_acb (acb);			/* deallocate acb     */
		    acbcnt--;
		    }
		else
		    {
		    tmmsg (PROCFAIL, "Job '%s' not removed because active.",
			    "TAE-NOABO", (*acb).name);
		    last_acb = acb;
		    }
    		}
	    else
		last_acb = acb;
	    }
	if (!found && !all)
	    tmmsg (PROCFAIL, "Job '%s' not on job list.", "TAE-NOJOB",
		    SVAL(*v, vnum));
    	}
    return(DO_CHECK);
    }


/*  reply_do - Process the REPLY command
 *
 *  T.B.D.  HOW TO REPLY TO REMOTE JOBS
 *
 *  returns DO_CHECK
 */
    FUNCTION CODE reply_do (procctx, cmdctx)

    FAST struct CONTXT *procctx;	/* in: context of invoking proc	     */
    FAST struct CONTXT *cmdctx;		/* in: context of cmd line for REPLY */

    {
    IMPORT struct CONTXT *curproc;	/* ptr to current context	    */
    IMPORT struct CONTXT primctx;	/* primary level context	    */
    IMPORT BOOL		 async_request;	/* TRUE if a request is pending	    */
    IMPORT CODE		 usermode;	/* CMDMODE or MENUMODE		    */
    IMPORT CODE		 termtype;	/* terminal type		    */
    IMPORT struct VARIABLE *dyntut_gbl;	/* pointer to $DYNTUT variable	    */

    struct VARIABLE	*v, *lookex();
    struct ACB		*find_oldest(), *acb = NULL;
    struct CONTXT	*saved_cur;
    struct CONTXT	tempctx;	/* temporary context block	    */
    struct SFILE	f;
    TAEINT		recsize;
    struct PARHDR	ph;
    struct LARGE_PARBLK	parblk;
    struct TCB		*t;
    CODE		code;
    struct MON_MSG	mon_msg;	/* monitor-to-monitor msg	   */
    struct MON_MSG	*request_msg;
    TEXT		jobmsg[STRINGSIZ+1];	/* TAE job name		   */
    BOOL		asytut_req;	/* async tutor request (true here) */


/*  Note on async_request flag: it's set TRUE by the async receiving routine;
 *  below, we set it false initially and TRUE if any ACBs are waiting; because
 *  a new request may have come in after we tested a particular ACB, we only
 *  set TRUE upon testing for more waiting, and only if menu mode (we don't
 *  want to repeat the request message in TCL mode)
 */


/* 	Find the acb 		*/

    async_request = FALSE;			/* assume there are no more */
    v = lookex (&(*cmdctx).parmst, "JOB");
    if ((*v).v_count == 0)			/* if no JOB value	    */
    	acb = find_oldest();
    else
    	acb = find_job(SVAL(*v,0));		/* find specified JOB	    */
    if ((acb == NULL) || (*acb).waiting == FALSE || (*acb).active == FALSE)
	{
	tmmsg (PROCFAIL, "No jobs or specified job not waiting.",
	   "TAE-NOWAIT");
	return (DO_CHECK);
	}
    (*acb).waiting = FALSE;
    if ((find_oldest() != NULL) && usermode == MENUMODE)
    	async_request = TRUE;			/* there is still more      */

/*      show the job name if not full screen tutor (full screen wipes us out) */
    if ((termtype != T_CRT) ||		/* want $DYNTUT .ne. "screen"*/
	(!s_equal(SVAL(*dyntut_gbl, 0), "SCREEN"))  )
    	{
    	s_copy ("Job \"", jobmsg);
    	s_append ((*acb).name, jobmsg);
    	s_append ("\"", jobmsg);
    	t_write (jobmsg, T_STDCC);
    	}

/* 	Read the parblk from the save file			 	    */

    code = f_opnspc(&f, SAVELUN, (*acb).requ_file, "", "",
		    ASY_TYPE, F_READ);		/* open for read	    */
    if (code != SUCCESS)  goto open_err;	/* report error		    */
    code = f_bread(&f, (GENPTR) &ph, sizeof(struct PARHDR), &recsize);	
    if (code != SUCCESS)  goto read_err;
    if (!s_equal(ph.sentinel, P_BIGSENTINEL))
	goto format_err;			/* not proper format	    */
    code = f_bread(&f, (GENPTR) &parblk, sizeof(struct PARBLK), &recsize);
    if (code != SUCCESS) goto read_err;
    makeabs(&parblk.symtab, parblk.pool);	/* make pointers absolute   */
    if (chk_parblk(&parblk) != SUCCESS)
	goto format_err;			/* check internal integrity */

/*	Tutor on the parameters						    */

    saved_cur = curproc;
    curproc = &primctx;				/* for NAMEs		    */
    request_msg = (struct MON_MSG *) (*(*acb).atcb).inmsg;
    MOVE_STRUCT(primctx, tempctx);
    MOVE_STRUCT((*acb).pdf, tempctx.pdf);
    tempctx.acbptr = (GENPTR) acb;		/* for 'remote'  flag	    */
    if ((*request_msg).type == MO_GETPAR)
	asy_getp (&tempctx, &parblk, sizeof ( parblk.pool ) );	/* Do async getpar proc.    */
    else
	{
	asytut_req = TRUE;			/* dynget for async job     */
                                         	/* Do dynamic tutor proc.   */
	dynget (&tempctx, &parblk, sizeof ( parblk.pool ), asytut_req );
	}
    curproc = saved_cur;
    	
    if ( (parblk.msgtyp == M_CONTINUE) || (parblk.msgtyp == M_DYNEXIT) )
    	{					/* must send msg to child   */
     	f_close (&f, F_DELETE);
        if (parblk.msgtyp == M_CONTINUE)
    	    /*  save the results and tell the async monitor 		    */

	    {
	    code = f_opnspc(&f, SAVELUN, (*acb).requ_file, "", "",
			    ASY_TYPE, F_WRITE);	/* open for write   	    */
	    if (code != SUCCESS)  goto open_err;	/* report error		    */
	    zero_block (&ph, sizeof (ph));
	    s_copy(P_BIGSENTINEL, ph.sentinel);	/* Write the header	    */
	    ph.recsize = parblk.blksiz;		/* not really known now     */
	    s_copy("TIME", ph.datetime);		/* dummy date/time for now  */
	    code = f_bwrite(&f, (GENPTR) &ph, sizeof(struct PARHDR));	
	    if (code != SUCCESS) goto write_err;
	    code = f_bwrite(&f, (GENPTR) &parblk, sizeof(struct PARBLK));
	    if (code != SUCCESS) goto write_err;
	    f_close (&f, F_KEEP);
	    mon_msg.type = MO_REPLY;
    	    s_copy ((*acb).requ_file, mon_msg.m_filespec);
            }
        else
	    /* no results but tell async monitor */
            mon_msg.type = MO_DYNEXIT;

	t = (*(*acb).atcb).tcb;				/* ptr to tcb	    */
	code = send_msg_w(acb, (GENPTR) &mon_msg,
		sizeof(struct MON_MSG));  		/* send the parblk  */
	if (code != SUCCESS )
	    {
	    if (code == KILLED)
		tmmsg (PROCFAIL, "User interrupt entered.", "TAE-ASYINTRPT");
	    else
	        tmmsg(PROCFAIL,
                "Error sending message to async monitor. Host error code = %d.",
	         "TAE-ASYSND", (*t).hostcode);
    	    return(DO_CHECK);
    	    }
#ifdef VAX_VMS
	code = asy_recs(t, (*(*acb).atcb).inmsg, sizeof(struct MON_MSG),
		asy_rcv_done, acb);				/* start read */
	if (code != SUCCESS)
	    {
	    tmmsg (PROCFAIL,
	        "Error receiving async monitor message. Host code = %d.",
	        "TAE-ASYRCV", (*t).hostcode);
    	    return(DO_CHECK);
	    }
#endif
    	}
    else if (parblk.msgtyp == M_HOLD)
	{
    	(*acb).waiting = TRUE;			/* HOLD means do it later */
    	f_close (&f, F_KEEP);
    	}
    else if (parblk.msgtyp == M_KILLED)
    	{
    	f_close (&f, F_DELETE);			/* close the ASY file	  */
	abort_acb (acb);
	s_copy ("ABORTED", (*acb).skey);
	(*acb).sfi = -1;
	tmmsg (SUCCESS, "Job '%s' aborted.", "TAE-ABORTOK", (*acb).name); 
    	}
    return(DO_CHECK);


format_err:
    f_close(&f, F_KEEP);
    tmmsg(PROCFAIL, "Async comm file not properly formatted.",
	     "TAE-FMTCOMM");
    return (DO_CHECK);

open_err:
    tmmsg(PROCFAIL, "Unable to open the async comm file, %s",
	    "TAE-COMOPEN", f.errmsg);
    return (DO_CHECK);

read_err:
    f_close(&f, F_KEEP);
    tmmsg(PROCFAIL, "Unable to read the async comm file, %s",
	    "TAE-COMREAD", f.errmsg);
    return (DO_CHECK);

write_err:
    f_close(&f, F_DELETE);
    tmmsg(PROCFAIL, "Unable to write the async comm file, %s",
	    "TAE-COMWRITE", f.errmsg);
    return (DO_CHECK);
    }


/*
 *  run_async - Initiate an asynchronous proc
 *
 *  returns SUCCESS if successful initiation; otherwise FAIL
 *
 *  Spawn a monitor or a process, send it a message.
 *  Under VAX_VMS we also initiate a read of reply from the task.
 *  Under UNIX, we only enable the signal from the receiving pipe
 *  (in create_async function), actual read is done later.
 */
    FUNCTION CODE run_async (procctx, cmdctx)

    FAST struct CONTXT *procctx;	/* in: context of invoking proc		*/
    FAST struct CONTXT *cmdctx;		/* in: context of cmd line for new proc	*/

    {
    IMPORT struct SYMTAB glbtab;	/* TCL global symbols		*/
    IMPORT struct VARIABLE *skey_gbl;	/* permanent pointer to $SKEY	*/

    struct ACB		*acb, *init_acb();
    CODE		code;
    struct ATCB		*atcb;			/* ptr to async TCB	    */
    struct SYMTAB	*qualptr;
    struct VARIABLE	*nq, *nfpt, *lookex();
    VOID		asy_rcv_done();		/* action routine	     */
    struct VARIABLE 	*rt;

    qualptr = &((*cmdctx).qualst);		/* pt to symtab of qualifiers */
    acb = init_acb(NULL);			/* alloc & init acb  */
    if (acb == NULL)
        return(DO_CHECK);	
    MOVE_STRUCT((*cmdctx).pdf, (*acb).pdf);	/* save pdf in acb for dyntut */
    code = get_jobname((*cmdctx).pdf.name, qualptr, (*acb).name,
    		&(*acb).seq);  			
    if (code != SUCCESS)
	goto undo_acb;
    s_copy ((*acb).name, (*acb).skey);			/* initial skey	     */
    atcb = (*acb).atcb;					/* point to atcb     */
    rt = lookex (qualptr, "RUNTYPE");
    if (s_lseq (SVAL(*rt,0), "ASYNCHRONOUS"))
	code = run_local (acb, cmdctx);		/* runtype = ASYNC */
    else
	code = run_local_process (acb, cmdctx);  
    (*atcb).inmsg = tae_alloc (1, sizeof(struct MON_MSG));  /* buffer to receive msgs */
    if ((*atcb).inmsg == NULL)
	{
	overr();
	goto undo_spawn;
	}
    if (code != SUCCESS) goto undo_acb;
    (*acb).active = TRUE;
#ifdef VAX_VMS
    code = asy_recs((*atcb).tcb, (*atcb).inmsg, sizeof(struct MON_MSG), asy_rcv_done, acb);		/* start read */
    if (code != SUCCESS)
    	{
    	tmmsg (PROCFAIL, "Error receiving async monitor message. Host code = %d.",
    		"TAE-ASYRCV", (*(*atcb).tcb).hostcode);
    	tae_free((GENPTR) (*atcb).inmsg);
    	goto undo_spawn;
    	}
#endif
    nfpt = lookex (qualptr, "ASYNCEND");	/* look at ASYNCEND qualifier */
    (*acb).notify = (s_equal (SVAL(*nfpt,0), "NOTIFY"));

    code = link_acb(acb);		/* add the acb to the acb linked-list	*/
    if (code != SUCCESS) goto undo_spawn;
    tmmsg(SUCCESS, "Asynchronous job '%s' initiated.", "TAE-ASYNCJOB",
    		(*acb).name);
    set_string (skey_gbl, (*acb).name);	/* set $SKEY to job name   */
    return (SUCCESS);


undo_spawn:
    abort_acb (acb);			/* kills monitor, releases resources */

undo_acb:
    free_acb(acb);
    return (DO_CHECK);
    }

/*
 *	run_local - run an async job local to the requesting node
 *
 *	return SUCCESS/FAIL
 */
    FUNCTION CODE run_local (acb, cmdctx)

    struct ACB		*acb;		/* in: ACB pointer		   */
    struct CONTXT	*cmdctx;	/* in: context of the command	   */

    {
    struct SFILE	job_file;	/* .JOB file			*/
    TEXT		jen_file[FSPECSIZ+1];
    struct FSBLOCK	jen_block;
    struct FSBLOCK      out_block;
    struct ATCB		*atcb;
    TEXT		log_name[FSPECSIZ+1];
    TEXT		errstr[STRINGSIZ+1];
    TEXT		record[STRINGSIZ+1];
    struct VARIABLE 	*v, *lookex();
    CODE		code;

    (*acb).aprocess = FALSE;				/* a full TM	*/
    /* build .job file	*/
    code = host_job (&job_file, cmdctx, TRUE);		/* host part of .job */
    if (code != SUCCESS)
    	return(FAIL);
    code = job_ins (&job_file, cmdctx);			/* installation exit */
    if (code != SUCCESS)
	{
	f_close (&job_file, F_DELETE);
    	return(FAIL);
	}
    code = bld_tcl (cmdctx, &job_file, ASYNC);		/* write TCL commands*/
    if (code != SUCCESS)
	{
	f_close (&job_file, F_DELETE);
    	return(FAIL);
	}
    f_close (&job_file, F_KEEP);

    /*  start the async job  */

    if (s_equal((*acb).name, "NORUN"))
	{
        s_copy ("Job file ", record);
	s_append (job_file.full_spec, record);
	s_append (" created. ", record);
	put_stdout (record);
    	return (FAIL);
	}

    v = lookex(&(*cmdctx).qualst, "STDOUT");	/* Get the STDOUT qualifier */
    if (v == NULL)
	{
	tmierr (1001);				/* Can't find STDOUT qualifier in submit */
	return (FAIL);
	}
    if ((*v).v_count != 0)			/* User specify log filename? */
	code = f_crack (SVAL (*v, 0), "",
    			"", ALOG_TYPE, &out_block, errstr);
    else
	code = f_crack ("", "", (*cmdctx).pdf.name,
			ALOG_TYPE, &out_block, errstr);	
    f_spec(&out_block, log_name);

    atcb = (*acb).atcb;
    code = create_async (atcb, sizeof (struct MON_MSG), (*cmdctx).pdf.name, ASYNC,
		job_file.full_spec, log_name);
    if (code != SUCCESS)
	{
    	tmmsg (PROCFAIL, "Error creating async job.  Host error code = %d.",
    	    "TAE-ASYCRE",(*(*atcb).tcb).hostcode);
    	return(FAIL);
    	}
    f_crack (job_file.full_spec, "", "", "", &jen_block, errstr);
    s_copy (JEN_TYPE, jen_block.type);
    f_spec (&jen_block, jen_file);	/* save name of file to use for asy comm */
    code = send_ctx_msg (acb, jen_file, MO_INIT);  /* init message	*/
    if (code != SUCCESS)
        abort_acb(acb);
    return(code);
    }

/*	run_local_process.  This starts an ASYNC job initiated
 *	with the RUNTYPE=ASYNC-PROCESS qualifier.
 *
 *	Under UNIX, we need to update the local variable _STDOUT.
 *
 *	return SUCCESS/FAIL
 */
    FUNCTION CODE run_local_process (acb, cmdctx)
    
    struct ACB		*acb;		/* in: ACB pointer		   */
    struct CONTXT	*cmdctx;	/* in: context of the command	   */

    {
    IMPORT struct VARIABLE *job_gbl;	/* ptr to $JOB TCL global	*/

    struct CONTXT	*ctx;
    struct ATCB		*atcb;
    struct VARIABLE 	*vq, *lookex();
    CODE		code;
    static TEXT		*null_string[] = {""};	/* value vector 	*/
    TEXT		errstr[STRINGSIZ+1];
    struct FSBLOCK 	fsblock;		/* file spec block	*/
    TEXT   		def_library[FLIBRSIZ+1];/* default exe library	*/
    TEXT		exespec[FSPECSIZ+1];	/* exe to execute	*/
    struct LARGE_PARBLK	parblk;			/* initial parblk	*/
    TEXT 		*targetvv[1]; 
    TEXT		*standout;
    struct TCB		*t;

    if ((*cmdctx).proctype != Y_PROCESS)
        {
        tmmsg (PROCFAIL, 
		"Proc type must be PROCESS for RUNTYPE=ASYNC-PROCESS.", 
    		"TAE-NOTPROCESS");
        return (FAIL);
        }
    (*acb).aprocess = TRUE;	
    if (s_equal((*acb).name, "NORUN"))
        return (FAIL);		
    vq = lookex (&(*cmdctx).qualst, "STDOUT");	/* get qualifier	     */
    if ((*vq).v_count > 0)			/* if something explicit     */
        standout = SVAL(*vq,0);
    else
        standout = NULL;				/* default: no output	     */
    targetvv[0] = (*acb).name;			/* build value vector	     */
    addstr (&(*cmdctx).locst, "_PARENT", 1, 1, (*job_gbl).v_cvp, V_LOCAL);
    addstr (&(*cmdctx).locst, "_CHILD", 1, 1, targetvv, V_LOCAL);
#ifdef UNIX
    async_stdout (cmdctx, standout);
#endif
    for (ctx = cmdctx; 
    	    s_equal ((*ctx).pdf.libr, "/LOCAL/");  
    					    ctx = (*ctx).backlink)
    	;					/* find parent if local      */
    s_copy ((*ctx).pdf.libr, def_library);	/* get default library	     */
    f_crack ((*cmdctx).exe_spec, def_library, (*cmdctx).pdf.name, 
    	EXE_TYPE, &fsblock, errstr);   		/* apply defaults to exe      */
    f_spec (&fsblock, exespec);			/* build exe file spec	      */
    parblk.last = TRUE;
    parblk.msgtyp = M_INIPAR;	
    code = package (cmdctx, &parblk, sizeof parblk.pool, 0);
    if (code != SUCCESS)
        {
        tmmsg (PROCFAIL, "Parameter values overflow VBLOCK.", "TAE-PSETOVER");
        return (FAIL);
        }
   atcb = (*acb).atcb;
   code = create_async (atcb, parblk.blksiz, (*cmdctx).pdf.name, 
					ASYNC_PROCESS, exespec, standout);
   if (code != SUCCESS)
	{
	tmmsg (PROCFAIL, "Error creating async process.  Host error code %d.",
		"TAE-ASYCRE", (*(*atcb).tcb).hostcode);
	return (FAIL);
	}
   code = send_msg_w (acb, (GENPTR)&parblk, parblk.blksiz);   
   if (code != SUCCESS)
	{
	t = (*(*acb).atcb).tcb;
	tmmsg (PROCFAIL, 
	    "Error sending message to async process. Host error code = %d.",
	    "TAE-ASYSND", (*t).hostcode);
        return (FAIL);
	}
   return (SUCCESS);
   }

/*
 *	send_ctx_msg - Send initiation message to async monitor
 *
 */
    FUNCTION CODE send_ctx_msg (acb, file_spec, msg_type)

    struct ACB		*acb;		/* in: ACB pointer		*/
    TEXT		file_spec[];	/* in: file spec for msg	*/
    FUNINT		msg_type;	/* in: message type		*/

    {
    IMPORT CODE			run_type;	/* TM run_type		*/
    IMPORT struct VARIABLE	*job_gbl;	/* ptr to $JOB		*/

    struct TCB			*t;
    CODE			code;
    struct MON_MSG		mon_msg;

    s_copy (file_spec, mon_msg.m_filespec);
    mon_msg.type = msg_type;
    mon_msg.m_parmode = run_type;
    s_copy ((*acb).name, mon_msg.m_child_job);
    s_copy (SVAL(*job_gbl,0), mon_msg.m_parent_job);
    t = (*(*acb).atcb).tcb;			/* ptr to tcb		   */
    code = send_msg_w(acb, (GENPTR) &mon_msg, sizeof(struct MON_MSG));
    if (code == FAIL) 
    	tmmsg(PROCFAIL,
	"Error sending message to async monitor.  Host error code = %d.",
    	"TAE-ASYSND", (*t).hostcode);
    return(code);
    }

/*
 *  send_msg_w - Send a message to a spawned task and wait on one of message
 *		 I/O completion or user interrupt
 *
 *	NOTE: Under UNIX  message writing is synchronous, so we do not
 *	wait on I/O completion.
 *
 *  returns SUCCESS or FAIL; host error code in tcb
 *
 */
    FUNCTION CODE send_msg_w (acb, msg, msglen)

    struct  ACB		*acb;    	/* in: acb for the job to send the msg	*/
    GENPTR		msg;		/* in: the message			*/
    TAEINT		msglen;		/* in: length				*/

    {
    IMPORT struct ECB	ecbi;		/* interrupt ecb			*/

    struct TCB		*tcb;		/* the tcb to send the msg through	*/
    CODE		code;

    tcb = (*(*acb).atcb).tcb;
    code  = asy_snds(tcb, msg, msglen);
    if (code != SUCCESS)
    	return(FAIL);
#ifdef 	VAX_VMS
    code  = e_wait(2, &(*tcb).ecbsnd, &ecbi);
    if (code != SUCCESS)
    	return(FAIL);
    (*tcb).mb_idle = TRUE;		/* free up the channel. 	    */
    if (e_occur(&(*tcb).ecbsnd))
    	{
    	e_clear(&(*tcb).ecbsnd);
    	return(SUCCESS);		/* msg received ok		    */
    	}
#endif
    if (e_occur(&ecbi))
    	{
    	e_clear(&ecbi);
    	return(KILLED);		/* return 'no more'? Caller can just exit   */
    	}
    return (SUCCESS);
    }

/*  show_acb_brief -  Display an acb, brief form
 *
 */
    FUNCTION VOID show_acb_brief (acb)

    struct ACB		*acb;			/* in: acb to display	     */

    {
    IMPORT COUNT	termcols;		/* number of columns on terminal */

    TEXT		line[STRINGSIZ+1];
    TEXT		tmpline[STRINGSIZ+1];
    TEXT		ssfi[20];		/* sfi converted to string   */
    COUNT		i;


    for (i = 0; i < STRINGSIZ; line[i++] = ' ');	   /* clear line     */
    line[termcols] = EOS;				   /* set line width */
    insert ((*acb).name, &line[ACB_NAME_C], ACB_PROC_C-ACB_NAME_C);
    insert ((*acb).pdf.name, &line[ACB_PROC_C], ACB_STATE_C-ACB_PROC_C);
    if ((*acb).waiting && (*acb).active)
    	insert ("WAITING", &line[ACB_STATE_C], ACB_SFI_C-ACB_STATE_C);
    else if ((*acb).active)
        insert ("ACTIVE", &line[ACB_STATE_C], ACB_SFI_C-ACB_STATE_C);
    else
    	insert ("COMPLETE", &line[ACB_STATE_C], ACB_SFI_C-ACB_STATE_C);
    s_i2s ((*acb).sfi, ssfi);
    i = ACB_SKEY_C - 2 - s_length(ssfi);	/* right justify	*/
    insert (ssfi, &line[i], ACB_SKEY_C-ACB_SFI_C);
    insert ((*acb).skey, &line[ACB_SKEY_C], termcols-ACB_SKEY_C);
    put_stdout (line);
    return;
    }

/*  show_acb_full  -  Display an acb, full form
 *
 */
    FUNCTION VOID show_acb_full (acb)

    struct ACB		*acb;			/* in: acb to display	     */

    {
    IMPORT COUNT	termcols;		/* number of columns on terminal */

    TEXT		line[STRINGSIZ+1];
    TEXT		ssfi[20];		/* sfi converted to string   */

    put_stdout("");
    s_copy ("JOB: ", line);
    s_append ((*acb).name, line);
    put_stdout (line);

    s_copy (" PROC: ", line);
    s_append ((*acb).pdf.name, line);
    put_stdout (line);

    s_copy (" SESSION ID: ", line);
    s_append ((*(*acb).atcb).session_id, line);
    put_stdout (line);

    s_copy (" STATE:  ", line);
    if ((*acb).waiting && (*acb).active)
    	s_append ("WAITING", line);
    else if ((*acb).active)
        s_append ("ACTIVE", line);
    else
    	s_append ("COMPLETE", line);
    put_stdout (line);

    s_i2s ((*acb).sfi, ssfi);
    s_copy (" SFI: ", line);
    s_append (ssfi, line);
    put_stdout (line);

    s_copy (" SKEY: ", line);
    s_bcopy ((*acb).skey, &line[7], (sizeof(line)-7));
    put_stdout (line);
    return;
    }

/*  showasy - for SHOW-ASYNC and SHOW-WAITING
 *
 *  returns DO_CHECK
 */
    FUNCTION CODE showasy (procctx, cmdctx)

    struct CONTXT	*procctx;		/* current context	*/
    struct CONTXT 	*cmdctx;		/* context of SHOW	*/

    {
    IMPORT struct ACB	   *acb_head;		/* acb list header	*/
    IMPORT COUNT	   acbcnt;		/* list count		*/
    IMPORT struct SYMTAB   glbtab;		/* global TCL symbols	*/
    IMPORT struct VARIABLE *skey_gbl;		/* pointer to $skey	*/
    IMPORT struct ECB	   ecbi;		/* operator attn ecb    */

    struct VARIABLE	*v, *frmpt, *lookex();
    struct ACB		*a, *find_job();
    BOOL		e_occur();
    BOOL		wait_req, hit = FALSE;
    COUNT		vnum;
    static TEXT		none[] = "TAE-NONE";

    v = lookex(&(*cmdctx).parmst, "JOB");
    frmpt = lookex(&(*cmdctx).parmst, "FORM");	/* FORM is BRIEF or FULL     */
    if (s_equal(SVAL(*frmpt,0), "BRIEF"))
        put_stdout(show_hdr);			/* see start of source for hdr */
    else
    	put_stdout("");
    if (acbcnt == 0 && s_equal(SVAL(*v, 0), "ALL"))
	{
	put_stdout("\n...none...\n");
	set_string (skey_gbl, none);		/* set $skey 		     */
	return(DO_CHECK);
	}
    wait_req = s_equal (&(*cmdctx).subcmd[0], "WAITING");
    if (!s_equal(SVAL(*v, 0), "ALL"))
    	{
	for (vnum = 0; vnum < (*v).v_count; vnum++)	     /* search JOB  list */
	    {
	    if (e_occur(&ecbi)) return(DO_SUCCESS);
	    a = find_job (SVAL(*v, vnum));
	    if (a == NULL)
		tmmsg (PROCFAIL, "Job '%s' not in job list.", "TAE-NOJOB",
			SVAL(*v, vnum));
	    else if ((wait_req && (*a).waiting) || !wait_req)
		{
		hit = TRUE;			/* for "none" report	    */
		s_equal(SVAL(*frmpt,0),"BRIEF")? show_acb_brief(a):show_acb_full(a);
    		}
	    }
    	}

    else				/* ALL */
    	{
    	for (a = acb_head; a != NULL; a = (*a).link)
	    {
	    if (e_occur(&ecbi)) return(DO_SUCCESS);
	    if ((wait_req && (*a).waiting) || !wait_req)
		{
		s_equal(SVAL(*frmpt,0),"BRIEF")? show_acb_brief(a):show_acb_full(a);
    		hit = TRUE;		/* not "none" anymore		    */
    		}
	    }
    	}
    if (!hit)				/* we never saw a one		    */
	{
	set_string (skey_gbl, none);		/* set $skey 		    */
	put_stdout("\n...none...\n");
	return(DO_CHECK);
	}
    return(DO_CHECK);
    }


/*
 *  trunc_left - Truncate left-hand characters
 *
 */
    FUNCTION VOID trunc_left(in, num, out)

    TEXT		in[];		/* in: input string	            */
    FUNINT		num;		/* in: max characters in out string */
    					/*  (not including EOS)		    */
    TEXT		out[];		/* out: output string		    */

    {
    COUNT		i,j,k;

    k = s_length(in);
    if (k <= num)
    	{
    	s_copy(in,out);
    	return;
    	}
    for (i=k-num, j=0; i<=k; i++, j++)		/* '=' catches EOS	    */
    	out[j] = in[i];
    return;
    }


/*
 *	upsymb.  Update a symbol table with provided variable
 *
 *  returns SUCCESS or FAIL; outputs err msg if FAIL
 */

    FUNCTION  CODE  upsymb (ctx, var)

    struct  CONTXT 	*ctx;		/* in: symbol table		*/
    struct  VARIABLE	*var;		/* in: variable 		*/

    {
    IMPORT  struct SYMTAB glbtab;

    struct  VARIABLE	*tv, *tv1;	/* target VARIABLE		*/
    struct  VARIABLE	*vp;
    CODE		code;
    struct  VARIABLE	*usearch(), *lookex();

    tv = usearch ((*var).v_name, ctx);
    if (tv == NULL)
	{
	tmmsg (PROCFAIL, "Error restoring '%s'.", "TAE-ARESTERR",
	       (*var).v_name);
	return (FAIL);
	}
    if (exclude(tv))			/* protect _PROC, etc.	*/
	return (SUCCESS);
    code = chk_vector(tv, (*var).v_type, (*var).v_cvp,
		      (*var).v_count, FALSE);
    if (code != SUCCESS)
        {
        tmmsg(PROCFAIL, "Mismatch restoring '%s'.", "TAE-MISMATCH",
       	      (*var).v_name);
        return (FAIL);
        }
    code = set_value(tv, (*var).v_cvp, (*var).v_count);
    if (code != SUCCESS)
        {
        overr();
        return (FAIL);			
    	}

/* update with possible new parm qualifiers */

    for (vp=(*var).v_qualst.link; vp!=NULL; vp=(*vp).v_link)
	{
	tv1 = lookex(&(*tv).v_qualst, (*vp).v_name); /* find qualifier in parms symbol table */
	if (tv1 == NULL)
	    {
	    tmmsg (PROCFAIL, "Error restoring '%s.%s'.", "TAE-ARESTERR",
	    		(*var).v_name, (*vp).v_name);
	    return (FAIL);
	    }
	code = chk_vector(tv1, (*vp).v_type, (*vp).v_cvp,
			  (*vp).v_count, FALSE);
	if (code != SUCCESS)
	    {
	    tmmsg(PROCFAIL, "Mismatch restoring '%s.%s'.", "TAE-MISMATCH",
	    		(*var).v_name, (*vp).v_name);
	    return (FAIL);
	    }
	code = set_value(tv1, (*vp).v_cvp, (*vp).v_count);
	if (code != SUCCESS)
	    {
	    overr();
	    return (FAIL);
	    }
	}
    return (SUCCESS);
    }

/*  waitasy_do	- WAIT-ASYNC TCL command
 *
 *  wait on the jobs in the specified list of jobs, or on attention sequence
 *
 *  $SFI and $SKEY are set to the values of sfi and skey for the last job
 *  waited on.
 *
 *  return DO_SUCCESS/DO_CHECK
 */
    FUNCTION CODE waitasy_do (procctx, cmdctx)

    struct CONTXT	*procctx;		/* in: proc context	    */
    struct CONTXT	*cmdctx;		/* in: WAIT cmd context	    */

    {
    IMPORT struct SYMTAB	glbtab;		/* globals table	    */
    IMPORT struct ACB		*acb_head;	/* acb header		    */
    IMPORT struct ECB		ecbi;		/* att seq ecb		    */
    IMPORT struct VARIABLE      *sfi_gbl;	/* pointer to $sfi	*/
    IMPORT struct VARIABLE 	*skey_gbl;	/* pointer to $skey	*/
    IMPORT COUNT		acbcnt;

    struct VARIABLE		*job, *lookex();
    struct ACB			*acb, *find_job();
    TEXT			*pt;
    COUNT			vnum;
    TAEINT			mi1 = -1;

    job = lookex(&(*cmdctx).parmst, "JOB");

/* Scan the list to see if any jobs are non-existent; if so return	    */

    if (acbcnt == 0)
    	{
    	tmmsg (SUCCESS, "No jobs in async job list.", "TAE-EMPTYASY");
    	return(DO_CHECK);
    	}
    if (!s_equal(SVAL(*job, 0), "ALL"))
	for (vnum = 0; vnum < (*job).v_count; vnum++)	 /* search JOB  list */
	    if (find_job(SVAL(*job,vnum)) == NULL)
		{
		tmmsg (PROCFAIL, "Job '%s' not in job list.", "TAE-NOJOB",
			SVAL(*job, vnum));
		return(DO_CHECK);
		}

/* Go through each job in the acb list; wait on the ones named in JOB	    */

    for (vnum = 0; vnum < (*job).v_count; vnum++)        /* search JOB  list */
    	{
        for (acb = acb_head; acb != NULL; acb = (*acb).link)
    	    {
    	    if (s_equal((*acb).name, SVAL(*job, vnum))	/* got one  */
    		|| s_equal(SVAL(*job, 0), "ALL") )		/* or all   */
    		{
    		if ((*acb).active)
    		    while (FOREVER)				/* poll	    */
			{
			if (!(*acb).active) break;		/* done	    */
			if (e_occur(&ecbi)) 			/* attn     */
    			    {
			    set_string (skey_gbl, "TAE-ABOWAIT");
			    IVAL(*sfi_gbl, 0) = mi1;
			    return(DO_SUCCESS);
    			    }
			if ((*acb).waiting) break;
    		 	wait_hold (1000);	/* hold for 1000 ms.	     */
			}
    		if ((*acb).waiting)
    		    {
    		    tmmsg (SUCCESS,
"Cannot wait on '%s'; job is waiting for REPLY.", "TAE-REPWAIT", (*acb).name);
    		    if ( (s_equal(SVAL(*job, 0), "ALL")) )
    		        continue;
    		    else
    			break;
    		    }
		IVAL(*sfi_gbl, 0) = (*acb).sfi;		/* set $sfi, $skey   */
    		pt = (*acb).skey;
		set_string (skey_gbl, pt);
    		if ( !(s_equal(SVAL(*job, 0), "ALL")) )
    		    break;			       /* get out if not all */
    		}
    	    }
    	}
    return(DO_CHECK);
    }
