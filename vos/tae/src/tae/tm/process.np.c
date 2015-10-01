/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

 /*<<<<<<<<<<<<    PROCESS ACTIVATION UNDER UNIX    >>>>>>>>>>>>>
 *
 *	Open issues:
 *
 *	Does the interrupt program change any statics, like
 *	cmd_mode, etc.?  If so, the longjump could screw things
 *	up.
 *
 *
 *	When pipe_sig gets an EXIT return from dyncmd, it must
 *		flag primary level (via longjmp?).
 *
 *	Need more work here so that when the Abort/continue question
 *		is active, packet traffic discontinues.
 *
 *
 *	
 *	HOW THE TASK PACKAGE WORKS UNDER UNIX:
 *
 *	The coding here is very delicate w.r.t. missing signals, race
 *	conditions, etc: be careful when making changes.
 *	
 *	Two pipes are established: one to send to the subprocess and
 *	one to receive from the subprocess.  The parent starts the
 *	ball rolling by sending an initial message to the subprocess.
 *	When the process wants to send a message to parent, it hits
 *	signal 16 for the parent.  The parent then reads the "from"
 *	pipe.
 *
 *	Messages from the subprocess are received and processed in the
 *	signal catcher for the pipe signal (16).  We do it in the
 *	catcher to avoid race problems with re-signalling on the primary
 *	level.
 *
 *	When an EXIT is recieved from a message processor (like dyncmd),
 *	the signal catcher forces back to primary level with a longjmp
 *	so a proper return to run_proc can be made.
 *
 */

/*
 *	process - run a TAE process.
 *
 *	(The compilation of this function may be suppressed by
 *	defining PROCESS_CNP, in which case a host-dependent version
 *	may be written in a process.cnp module, for example, UNIX.)
 */


/*
 *	CHANGE LOG:
 *
 *	10-nov-83 	Make arguments to execl strings...dm
 *	12-dec-83	Return host code for TAE-PRCSCOM...dm
 *			Also close open pipes before return...dm
 *	13-dec-83	Ignore signal for broken pipe...dm
 *	23-apr-84	Put pipe numbers in environment list,
 *			implement debugging...dm
 *	29-apr-84	Change for 4.2 implementation...dm
 *	18-jun-84	Move BSD_4P2 symbol defition to TAECONF...dm
 *	03-jul-84	Make the default debugger "dbx" for BSD 4.2...dm
 *	01-aug-84	Make get_pblk_remote call conditional...dm
 *	16-jun-85	Allow M_HLOGMSG  msg type under non-BSD_4.2 version,
 *			(bug fix), update for TAE V1.3 implementation...dm
 *	01-jul-85	Fix UNIX compilation errors...dm
 *	10-jul-85	Update calling seq for logmsg() and get_pblk_remote()
 *			functions...dm
 *	25-jul-85	Fix UNIX lint errors...dm
 *	15-aug-85	Save child_pid so that child can be aborted when
 *			parent dies (via a SIGTERM or SIGQUIT signal)...dm
 *	20-sep-85	Return FAIL if error return in process()...dm
 *	04-dec-86	PR 961: Indicate to 'dynget' that we're tutoring for
 *			sync proc (to prevent HOLD)...peb
 *	04-dec-86	Pass current context to interrupt from process...peb
 *	04-dec-86	PR 1100: Before returning, check for entry via menu.  
 *			If so, request prompt before repaint...peb
 *	12-dec-86	Check return code from outputs()...peb
 *	21-jan-87	Check block size for write to pipe...lia
 *      24-nov-87       Add size in call to dynget...tpl
 *      25-nov-87       Add size in call to package...tpl
 *	03-mar-88	Fix race condition where first SIGREAD comes
 *			in right after the initial write of the initial
 *			PARBLK before setjmp is called.  The signal processor
 *			uses the context block left over from the
 *			previous execution of process.  The fix has been
 *			implemented for the BSD 4.2 code only...palm
 *
 *			The fix consists of writing the first message AFTER 
 *			setjmp has been done.    Then if the message comes
 *			in immediately, we are ready.
 *      08-may-88       Removed #ifndef PROCESS_CNP since it does nothing..tpl
 *	26-jun-88	Added remark to act_task() on socket creation...ljn
 *	07-oct-88	Use sizeof(PARBLK) rather than P_BYTES on read...nhe
 *	08-nov-88	-vfork() defined for SYSV in taeconf.inp
 *			-#ifdef BSD_UNIX -> #ifndef SYSV...ljn
 *	09-nov-88	The application debugging problems appear to occur
 *			only on the Sun. Ergo, #ifdef sun...ljn
 *	10-nov-88	-Cut out BSD 4.1 version; removed all BSD_4P2 defs
 *			-#ifdef SV_BSDSIG and sigvector() for hpux...ljn
 *	01-dec-88	new package calling sequence; package valids for
 *			SELFTUTOR...palm
 *	09-feb-89	Zero parblk header before sending to process...palm
 *	01-may-89	new M_COMMAND msg...palm
 *	27-jun-89	-Restored BSD 4.1 code, but use #ifndef NOSIGVEC.
 *			-Applied PAM's timing bug fix to NOSIGVEC version...ljn
 *	24-aug-89	#include vfork.h to cure sparc problems debugging
 *			applications...ljn
 *	17-jan-90	Made a FUNINT a void to quell compiler warning...ljn
 *	27-sep-91	Removed special handling of hpux signals...ljn
 *	04-sep-92 PR 871,444 Added multiple read/write for large par block...tpl
 *	06-oct-92	SGI port.  Conditionally defined the simple macro
 *			_BSD_SIGNALS before the <signal.h> header file is
 *			referenced, so that sigvec is defined...bth
 */


#include "stdh.inp"
#ifdef sgi
#define _BSD_SIGNALS
#endif
#include <signal.h>
#ifdef S_NONE
#undef S_NONE		/* prevent errors on Solaris */
#endif
#include <errno.h>
#include <setjmp.h>
#if defined(sparc)
#if (OSMajorVersion >= 5)
#include <unistd.h>
#else
#include <vfork.h>
#endif
#endif 
#if !defined(NOSIGVEC)
#include	<sys/types.h>
#include	<sys/socket.h>
#endif
#include "taeconf.inp"
#include "tminc.inc"
#include "parblk.inc"
#include "taskinc.inp"
#include "eventinc.inp"

#define WRITECHUNK min(PIPESIZ, parblk.blksiz-already_wrote)

/* error messages from function process(). We have them here because
   process can not output them, having no standard output to output them to.
 */

    static struct CODE_ERR er_over=
	{"Parameter values overflow capacity of message to block.",
	"TAE-PSETOVER", 0};

    static struct CODE_ERR er_size=
        {"Initiation message to process too large for pipe.", "TAE-SIZERR", 0};

    static struct CODE_ERR er_ini=
	{"Unable to initiate  the process, host code = %d.",
	    "TAE-PRCSINI", 0};

    static struct CODE_ERR er_comini=
	{"Cannot establish communication with the process, host code = %d.",
	    "TAE-PRCSCOM", 0};

    static struct CODE_ERR er_term=
	{"Abnormal process termination; process status code = %d.",
	"TAE-PRCSTRM", 0};

    static struct CODE_ERR er_imsg=
	{"Error sending initiation message to process; host code = %d.",
	"TAE-IMSGERR", 0};

    static struct CODE_ERR er_out=
	{"Error in process output values.", "TAE-INVOUT", 0};

    static struct CODE_ERR er_rcv=
	{"Error receiving message from process; host code = %d.",
	 "TAE-RCVERR", 0};

    static struct CODE_ERR er_par=
	{"Message from process incorrectly formatted.",
	  "TAE-PARBLK", 0};

    static struct CODE_ERR er_toolg=
        {"Message to process too large for pipe.", "TAE-SIZERR", 0};

    static struct CODE_ERR er_snd=
	{"Error sending message to process, host code = %d.",
	 "TAE-SNDERR", 0};

    static struct CODE_ERR er_acknsnd=
	{"Error sending handshake message to process; host code =  %d.",
	"TAE-SNDERR", 0};

    GLOBAL    TAEINT  sync_child = 0;	/* pid of child running synchronously, 
					 * zero implies no such child active */


#if !defined(NOSIGVEC)
#include	<sys/types.h>
#include	<sys/socket.h>

    GLOBAL  v023proc = 0;		/* version number		*/

    static    jmp_buf	context;
    static    BOOL	opwait;		/* true if operator int waiting	    */
    static    int	pipeint;	/* true if pipe int being processed */


    VOID	int_sig(); 		/* handle interrupt signals 	    */
    VOID	pipe_sig();		/* handle pope io signals	    */


    FUNCTION CODE process (pctx, ecbi, errmsg)

    struct CONTXT	*pctx;		/* in:  proc context		     */
    struct ECB		*ecbi;		/* in/out: event blk for interrupts  */
    struct CODE_ERR	*(*errmsg);	/* out: errmsg if code FAIL, else "" */

    {
    IMPORT  TEXT	**environ;	/* shell environment list	     */
    IMPORT  CODE	menu_screen;	/* menu screen status indicator	*/
    IMPORT  CODE	usermode;	/* user mode: CMDMODE or MENUMODE*/
    IMPORT  CODE 	run_type;	/* INTER, ASYNC or BATCH	     */
    IMPORT  int		errno;
    struct LARGE_PARBLK	parblk;		/* message block to send to task     */
    CODE		status;
    TEXT		exespec[FSPECSIZ+1];
    struct FSBLOCK	fsblock;
    TAEINT		pid, child_pid;	
    TAEINT		read_chan, write_chan;	/* read/write channels	    */
    CODE		code, ret_code;
    COUNT		count;
    BOOL		asytut_req;
    LONG		ackn_msg;		/* max 4 bytes of ackn msg */
    CODE		pr_stat;		/* process status	   */
    CODE		code1;
    TEXT		errstr[STRINGSIZ+1];
    struct CONTXT	*ctx;
    struct VARIABLE	*_tutor;
    struct VARIABLE	*lookex();
    CODE		packageFlag;		/* used to request that valids*/
						/* be packaged for SELFTUTOR  */
    COUNT		already_read;
    COUNT		already_wrote;
    GENPTR		p;


/* first make sure we get a good library name by finding the parent pdf */
    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); 
	ctx = (*ctx).backlink);
    f_crack((*pctx).exe_spec, (*ctx).pdf.libr, (*pctx).pdf.name,
             EXE_TYPE, &fsblock, errstr);	/* apply defaults...	   */
    f_spec(&fsblock, exespec);			/* build file spec to run  */
    s_lower(exespec);				/* convert to lower case   */
    zero_block ((GENPTR) &parblk, (GENPTR) parblk.pool - (GENPTR) &parblk);
    parblk.last = FALSE;
    parblk.msgtyp = M_INIPAR;			/* initial parameter block */
    _tutor = lookex (&(*pctx).locst, "_TUTOR");
    packageFlag = (_tutor && IVAL(*_tutor,0)) ? VM_VALID : 0;
/* build the parblk        */
    if (package(pctx, &parblk, sizeof parblk.pool, packageFlag) != SUCCESS)		goto rp_overr;	

    read_chan = -1;				/* initialize to -ve value */
    write_chan = -1;			
    pid = getpid();				/* process id of tm	   */
    code = act_task(exespec, pid, environ,
	&read_chan, &write_chan, &child_pid);
    if (code != SUCCESS)
	goto rp_tiner;				/* task initiation error   */
     sync_child = child_pid;			/* save child's pid	   */

    p = (GENPTR)&parblk;
    set_signal (SIGPIPE, SIG_IGN);		/* dont quit on broken pipe*/


/* Task started.  Now wait on signal from task for a message read,
 * signal from operator or task completion.
 * NOTE: We enable to receive the next signal from the child only
 * after the previous message is read and an ackowldgement is sent.
 * Also, if an operator interrupt arrives during the message processing,
 * the opwait flag is set and the interrupt is processed after the
 * message processing is complete.
 */
    *errmsg = NULL;			/* no error msg yet		*/
    ret_code = DO_CHECK;		/* error return			*/
    ackn_msg = ACKN_CODE;		/* acknowledgement code for subtask*/
    code1 = setjmp(context);		/* remember this place		*/

/*
 *  code1 = 0 only happen in the initial setjmp call
 *  code1 = 1 for normal pipe i/o
 *  code1 = 2 for interrupt
 *  Once a read/write started, it will loop till all of the par  had been read/
 *  written.  Be very careful when making changes in this loop.
 *  Make sure functions in $TAESRSTAE/lib/tae/taskpro.np.c are changed
 *  accordingly if you made any changes here.
 */
    if (code1 == 0)			/* first time			*/
	{
	pipeint = 0;
	opwait = FALSE;
        set_signal (SIGREAD, pipe_sig);   /* enable signal from task	*/
	set_signal (SIGINT, int_sig);	/* enable operator interrupt	*/
/*
 *	write first chunk
 */
	already_wrote = 0;
	already_read = 0;
        count = WRITECHUNK;
        count = write(write_chan, &p[already_wrote], count );
        if (count <= 0 ) 
		goto rp_msger;
	already_wrote += count;		/* increment counter		*/
	if ( already_wrote == parblk.blksiz )	/* all done		*/ 
		already_wrote = 0;
	else if (already_wrote > parblk.blksiz ) 
		goto rp_msger;

	goto WAITLOOP;				/* continue to  wait for msg */
	}
    else if (code1 == 1)			/* message arrived	     */
	{
	pipeint--;

	if ( already_wrote > 0 )	/* write some more of the parblk */
	    {
            count = WRITECHUNK;
	    count = write(write_chan, &p[already_wrote], count );
	    if ( count <= 0 )
		goto rp_msger;
	    already_wrote += count;

	    if ( already_wrote == parblk.blksiz )	/* all done? */
		already_wrote = 0;
	    else if ( already_wrote > parblk.blksiz )
		goto rp_msger;
	    
	    set_signal (SIGREAD, pipe_sig);	/* reenable signal from task */
	    goto WAITLOOP;			/* continue to  wait for msg */
	    }
	else				        /* read msg from proc, 
						   possibly a continuation */
	    {
	    count = read(read_chan, &p[already_read], 
				sizeof(parblk)-already_read ); 
	    already_read += count;
	    if (count <= 0) goto rp_rcver;	
	    if ( already_read < parblk.blksiz )  
	 	{
	        set_signal (SIGREAD, pipe_sig);	/* reenable signal from task */
		goto WAITLOOP;			/* continue to  wait for msg */
		}
	    if ( already_read > parblk.blksiz ) 
		goto rp_rcver;
	    if ( already_read ==  parblk.blksiz ) /* got it all, process it */
		already_read = 0;
	    }

/*******   now process the message    ************/

    	makeabs(&parblk.symtab, parblk.pool);	/* make pointers absolute    */
    	if (chk_parblk (&parblk) != SUCCESS)
	    goto rp_badparblk;
	if (parblk.msgtyp == M_FULLPDF  ||  parblk.msgtyp == M_SUBPDF)
	    {
	    if (run_type == ASYNC)
		{
#ifdef TAE_ASYNC
		code = get_pblk_remote(pctx, &parblk, errmsg);
						/* get parms from parent mon */
		if (code != SUCCESS) goto rp_rmerr;
#endif
		}
	    else
		{
		asytut_req = FALSE;		/* tutor for sync proc	*/
						/* dynamic parameter    */
		dynget(pctx, &parblk, sizeof ( parblk.pool), asytut_req);
		logdyn(&parblk);		/* log dynamic parameters   */
		}
	    set_signal (SIGREAD, pipe_sig);	/* reenable signal from task */
	    count = write(write_chan, &ackn_msg, ACKN_SIZE);	/* send ackn */
            if ( count <= 0 )
                goto rp_dmser;
						/* now send parameter block  */
	    already_wrote = 0;
            count = WRITECHUNK;
            count = write(write_chan, &p[already_wrote], count );
            if ( count <= 0 )
                goto rp_dmser;
            already_wrote += count;
            if ( already_wrote == parblk.blksiz )
                already_wrote = 0;
	    }
	else if (parblk.msgtyp == M_COMMAND)	/* execute a command line */
	    {
	    dyncommand (pctx, &parblk, sizeof parblk.pool);
	    set_signal (SIGREAD, pipe_sig);	/* reenable signal from task */
	    count = write(write_chan, &ackn_msg, ACKN_SIZE);	/* send ackn */
						/* now send parameter block  */
            already_wrote = 0;
            count = WRITECHUNK;
            count = write(write_chan, &p[already_wrote], count );
            if ( count <= 0 )
                goto rp_dmser;
            already_wrote += count;
            if ( already_wrote == parblk.blksiz )
                already_wrote = 0;
	    }
	else if (parblk.msgtyp == M_LOGMSG || parblk.msgtyp == M_HLOGMSG)
	    {
	    logmsg(pctx, &parblk);		/* log the message           */
	    set_signal (SIGREAD, pipe_sig);	/* reenable signal from task */
	    count = write(write_chan, &ackn_msg, ACKN_SIZE);	/* send ackn */
	    if (parblk.msgtyp == M_HLOGMSG)
		{
		parblk.msgtyp = M_CONTINUE;	/* handshake		     */
		makerel(&parblk.symtab, parblk.pool);
		already_wrote = 0;
                count = WRITECHUNK;
                count = write(write_chan, &p[already_wrote], count );
                if ( count <= 0 )
                    goto rp_mgser;
                already_wrote += count;
                if ( already_wrote == parblk.blksiz )
                	already_wrote = 0;
		}
	    }
	else if (parblk.msgtyp == M_OUTPUT)
	    {
	    code = outputs(pctx, &parblk);	/* apply output values	     */
	    if (code != SUCCESS) goto rp_outerr;
	    set_signal (SIGREAD, pipe_sig);	/* reenable signal from task */
	    count = write(write_chan, &ackn_msg, ACKN_SIZE);	/* send ackn */
	    }
	else
	    {
	    dynamic_ins (pctx, &parblk);	/* installation exit	     */
	    set_signal (SIGREAD, pipe_sig);	/* reenable signal from task */
	    count = write(write_chan, &ackn_msg, ACKN_SIZE);	/* send ackn */
	    }
	set_signal(SIGINT, int_sig);		/* enable signal handler     */
	}

     if (code1 == 2 || opwait)		/* if ^C occured	 	 */
	{
	opwait = FALSE;			/*  no more waiting	 	 */
	if ((*pctx).interrupt)		/* interrupt allowed ?		 */
	    {
	    c_suspend(child_pid);	/* suspend the task	 	 */
	    code = interrupt(pctx);	/* do interrupt processing 	 */
            if (code == DO_ABORT)	/* this dialog will be aborted   */
   					/* if signal comes in the	 */
    					/* the middle			 */
	        {
	        kill(child_pid, SIGKILL);	/* abort the task	 */
	        ret_code = DO_ABORT;
	        goto close_ret;
	        }
	    else
	        c_resume(child_pid);	/* resume suspended task 	*/
	    }
	set_signal (SIGINT, int_sig);	/* enable operator intrpt	*/
	set_signal (SIGREAD, pipe_sig);	
        }

/*  now wait for the process to terminate	*/

WAITLOOP:

    while (FOREVER)	
	{
	while (!opwait &&
		(code = wait(&status)) != child_pid  && code != -1)
	    ;				/* loop if some other child...	*/
				    	/* has terminated		*/
    	if (code == child_pid)
            {
	    sync_child = 0;		/* mark that no child active	*/
	    pr_stat = (status & 0177);	/*low byte is process status    */
	    if (pr_stat == 0177)	/* if process stopped (interrupted) */
		continue;		/* ignore the signal		*/
	    if (usermode == MENUMODE)	/* if process activated by menu	*/
		menu_screen = PROMPT_PAINT;	/* prompt before repaint menu*/
	    if (pr_stat != 0)		/* if not normal termination	*/
	        goto rp_wterr;
	    break;
	    }
	}
    close (read_chan);				/* close the pipes	*/
    close (write_chan);
    t_attn(ecbi);				/* reset oper int handler */
    set_signal (SIGREAD, SIG_IGN);		/* disable signal	  */
    return (DO_CHECK);


rp_overr:
    *errmsg = &er_over;
    return (FAIL);

rp_sizerr:
    *errmsg = &er_size;
    return (FAIL);

rp_tiner:
    if (child_pid == -1)
	{
	*errmsg = &er_ini;
	er_ini.code = errno;
	}
    else
	{
        *errmsg = &er_comini;
	er_comini.code = errno;
	}
    ret_code = FAIL;
    goto close_ret;

rp_wterr:
    *errmsg = &er_term;
    er_term.code = (status&0177);
    ret_code = FAIL;
    goto close_ret;

rp_msger:
    *errmsg = &er_imsg;
    er_imsg.code = errno;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_outerr:
    *errmsg = &er_out;
    kill (child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_rcver:
    *errmsg = &er_rcv;
    er_rcv.code = errno;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_badparblk:
    *errmsg = &er_par;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_toolarge:
    *errmsg = &er_toolg;
    kill(child_pid, SIGKILL);
    ret_code =FAIL;
    goto close_ret;

rp_dmser:
    *errmsg = &er_snd;
    er_snd.code = errno;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_mgser:
    *errmsg = &er_acknsnd;
    er_acknsnd.code = errno;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_rmerr:
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

close_ret:
    sync_child = 0;			/* no child active	*/
    if (read_chan != -1)		/* channel open		*/
	close(read_chan);		/* close it		*/
    if (write_chan != -1)
	close(write_chan);		/* close write channel  */
    t_attn(ecbi);			/* reset ^C handler	*/
    set_signal (SIGREAD, SIG_IGN);	/* ignore child signals */
    return (ret_code);
    }

/* 	signal catchers.
 */

    int_sig()			/* here for operator attn	*/
        {
	set_signal (SIGINT, SIG_IGN);	/* disable operator intrpt 	 */
    if (pipeint > 0 )		/* if pipe signal being processed	*/
	{
	opwait = TRUE;		/* set the flag and return		*/
	return;
	}
    else
	{
        longjmp(context, 2);	/* branch to  process it		*/
	}
    }

/*
 *	pipe_sig.  Signal from child to read from pipe.
 */
/* 	Note on re-enabling the pipe signal:  one of them will not
 *	be lost by re-enabling on the primary level because another
 *	pipe signal may not come in until we send out a write
 *	message.
 */

    pipe_sig()			/* here when child wants to send */

    {
    pipeint++;
    longjmp(context, 1);	/* branch to main level		 */
    }

/*
 *	set_signal. Set the signal handler (under BSD 4.2)
 */

    FUNCTION  CODE  set_signal(signum, sig_hand)

    FUNINT		signum;		/* signal number	   */
    void		(*sig_hand)();	/* signal handler function */

    {

#if defined(sun) && OSMajorVersion >= 5
    signal(signum, sig_hand);
    return SUCCESS;
#else
    CODE		code;
    struct	sigvec  isv;
    struct	sigvec  osv;
    isv.sv_handler = sig_hand;
    isv.sv_mask = 0;			/* no signal masking	   */
    isv.sv_onstack = 0;			/* no special stack	   */
    code = sigvec(signum, &isv, &osv);	/* assign handler to signum */
    return (code);
#endif
    }


/*++
*  FUNCTION:	act_task
*
*  DESCRIPTION:  This function activates a child process and start up
*  the specified image file.  It can also start the debugger and ask it
*  to start up the image file.  It also creates a socket for use in
*  passing V-blocks to/from the child task.
*
*  CONTROL:
*	Decide if debugger is to be used and what its name is.
*	Create socket for communications with child:
*	    Create socket.
*	    Bind name to socket.
*	    Listen for connection requests.
*	Add parameters to environment to tell child pid and socket name.
*	Fork new process.
*	Exec requested task or debugger with requested task.
*	Accept connection to socket.
*
--*/
FUNCTION  CODE  act_task(task_name, parent_pid,  envtbl,
                         read_pipe, write_pipe, child_pid)

TEXT	task_name[];		/* in:  task name	  	     */
FUNINT	parent_pid;		/* in:  parent pid		     */
TEXT 	*envtbl[];		/* in: environment table	     */
FUNINT	*read_pipe;		/* out: read end of up-pipe	     */
FUNINT	*write_pipe;		/* out: write end of down-pipe	     */
FUNINT	*child_pid;		/* out: process id of activated task */
{

    IMPORT  struct VARIABLE *switch_gbl;	/* TAE switch bits	*/
    IMPORT  CODE	run_type;	/* INTER, ASYNC, or BATCH	*/
    IMPORT  TAEINT	errno;
    int	  	code, ns = -1, nfds, readfds;
    int		chld_sig();
    int	  	down_pipe[2], up_pipe[2];
    static TEXT	snd_pipe0[20], snd_pipe1[20], rcv_pipe0[20], rcv_pipe1[20];
    static TEXT parent_name[40];
    static TEXT	snd_skname[20];
    static TEXT dbxflagname[20];
    static TEXT	*parent_env[]={parent_name, snd_pipe0, snd_pipe1,
			rcv_pipe0, rcv_pipe1};
    static BOOL 	sockflag = 0;
    static int	sd = 0;
    static struct sockaddr  sockname;
    struct sockaddr	acname;
    int		socknamelen, acnamelen;
    CODE	status;
    BOOL	debug;			/* TRUE if debug switch set	*/
    TEXT	dbg_name[FSPECSIZ+1];	/* debugger name		*/
#define	MAXENV 1000
    TEXT	*envlist[MAXENV+1];	/* final environment list	*/
    COUNT	i, k;
    TEXT	*dbgptr;		/* pointer to debugger symbol	*/
    TEXT	*getenv();
/*----------------------------------------------------------------------*/
/*  EXECUTABLE STATEMENTS.						*/
/*									*/
/*  Initialize.								*/

#include	<stdio.h>

*child_pid = -2;				/* initialize		*/

/*  Decide if debugger is to be used and what its name is.		*/
/*									*/
/*  Debugger is used iff SW_DEBUG bit is set in $SWITCH TAE global	*/
/*  (i.e. $SWITCH = 1) and RUNTYPE is INTERACTIVE. The standard		*/
/*  4.2BSD debugger, dbx, will be used unless another name is		*/
/*  passed in via the environment variable DEBUUGGER.			*/


debug = (run_type == INTER) && (IVAL(*switch_gbl, 0) & SW_DEBUG);

/* Pipes work just fine for Apollo for debugging purposes. Sockets
 * of family AF_UNIX in fact do not. (Apollo has only AF_INET.)
 */

if( debug )
    {
    if (debug)   /* Redundant. For debug of debug. */
	{
	dbgptr = getenv( "DEBUGGER" );
	if ( dbgptr == NULL)
	    s_copy("/usr/ucb/dbx", dbg_name);
	else
	    s_bcopy(dbgptr, dbg_name, FSPECSIZ);
	}
/*  Create socket for communications with child.			*/
/*  Create the socket, bind a name to it, listen for connections.	*/
/*  This is done only once.						*/

/***	This section of code ought to be done at a higher level.      ***/
    if( !sockflag )
    {
	sockflag = 1;
        sd = socket( AF_UNIX, SOCK_STREAM, 0 );
        sockname.sa_family = AF_UNIX;
        sprintf( sockname.sa_data, "%d.TM", parent_pid );
        socknamelen = sizeof( sockname.sa_family ) + strlen( sockname.sa_data );
        status = bind( sd, &sockname, socknamelen );
        status = listen( sd, 1 );
    }
/***	That way, the socket could be properly UNLINKed when TM exits.***/

/*  Add parameters to environment to tell child pid and socket name.	*/

    sprintf(parent_name, "PARENT=%d", parent_pid);

    sprintf( snd_skname, "SOCKNAME=%s", sockname.sa_data );

    sprintf( dbxflagname, "DBXFLAG=%d", 1 );

    i = 0;  k = 0;
    envlist[i++] = parent_name;
    envlist[i++] = snd_skname;
    envlist[i++] = dbxflagname;
    while( (envlist[i++] = envtbl[k++]) != NULL && i < MAXENV ) ;

    envlist[MAXENV] = 0;

/*  Fork new process.							*/

    *child_pid = vfork();

/*  Exec requested task or debugger with requested task.		*/

    if (*child_pid == -1)
        return (FAIL);
    else if (*child_pid == 0)
        {
	if (debug)
            execle(dbg_name, dbg_name, task_name, 0, envlist);
	else
	    execle(task_name, task_name, 0, envlist);
	_exit(1);
        }

/*  Accept connection to socket.					*/

    code = set_signal( SIGCHLD, chld_sig );

    readfds = 1<<sd;
    nfds = select( 32, &readfds, 0, 0, 0 );

    code = set_signal( SIGCHLD, SIG_DFL );

    if( nfds > 0 )
        {
        acnamelen = sizeof( acname );
        ns = accept( sd, &acname, &acnamelen );
        }

    *read_pipe = ns;
    *write_pipe = ns;
    }
else
    {
    status = pipe(down_pipe);			/* create pipe	#1	*/
    if (status == -1) return (FAIL);
    status = pipe(up_pipe);			/* create pipe #2	*/
    if (status == -1)
    	{
	close(down_pipe[0]);			/* close the open pipes	*/
	close(down_pipe[1]);
	return (FAIL);
	}

    sprintf(parent_name, "PARENT=%d", parent_pid);
    sprintf(snd_pipe0, "DOWN_PIPE0=%d", down_pipe[0]);
    sprintf(snd_pipe1, "DOWN_PIPE1=%d", down_pipe[1]);
    sprintf(rcv_pipe0, "UP_PIPE0=%d", up_pipe[0]);
    sprintf(rcv_pipe1, "UP_PIPE1=%d", up_pipe[1]);

    debug = FALSE;
    if (run_type == INTER)			/* debug only if interactive */
	debug = (IVAL(*switch_gbl, 0) & SW_DEBUG);
    if (debug)
	{
	dbgptr = getenv("DEBUGGER");		/* get debugger name	*/
	if (dbgptr == NULL)			/* not defined		*/
#ifdef SYSV
	    s_copy("/bin/adb", dbg_name);	/* default for others 	*/
#else						
	    s_copy("/usr/ucb/dbx", dbg_name);	/* default for 4.2	*/
#endif
	else
	    s_bcopy(dbgptr, dbg_name, FSPECSIZ);  /* copy debugger name	*/
	}

    for (i=0; i<5; i++)
	envlist[i] = parent_env[i];		/* first copy locals   	*/
    for (i=5, k=0; i < MAXENV; i++, k++)	/* copy shell  exports	*/
	{
	envlist[i] = envtbl[k];
	if (envlist[i] == NULL)
	    break;				/* last one copied	*/
	}
    envlist[MAXENV] = 0;

    *child_pid = vfork();			/* use 'vfork' for fastness  */

    if (*child_pid == 0)
        {
	if (debug)
            execle(dbg_name, dbg_name, task_name, 0, envlist);
	else
	    execle(task_name, task_name, 0, envlist);
	_exit(1);
        }
    if (*child_pid == -1) return (FAIL);
    *read_pipe = up_pipe[0];		/* end to read from		*/
    *write_pipe = down_pipe[1];		/* end to write to		*/
    close (down_pipe[0]);		/* close unused ends		*/
    close (up_pipe[1]);
    }
return (SUCCESS);
}

/*	Catch SIGCHLD and do nothing. */
FUNCTION static VOID chld_sig()
{
}

/*
 *	c_suspend.  Suspend the executing task	.
 *	Note: under standard UNIX, this is a NOOP.
 */

    FUNCTION  static VOID  c_suspend(child_pid)

    int		child_pid;		/* process id of task to suspend */

    {
#ifndef SYSV
    kill(child_pid, SIGSTOP);		/* send signal to stop		 */
#endif
    return;
    }

/*
 *	c_resume.  Resume the suspended task.
 *	Note: under standard UNIX, this is a NOOP.
 */

    FUNCTION  static VOID  c_resume(child_pid)

    int		child_pid;		/* process id of suspended task */

    {
#ifndef SYSV
    kill(child_pid, SIGCONT);		/* send signal to continue 	*/
#endif
    return;
    }
#else

/*****  For 	BSD-4.1 and other similar versions		******/


    GLOBAL  v12proc = 0;		/* version number		*/

    static    BOOL	op_int;
    static    jmp_buf	context;

    VOID	int_sig();
    VOID	pipe_sig();


    FUNCTION CODE process (pctx, ecbi, errmsg)
    struct CONTXT	*pctx;		/* in:  proc context		     */
    struct ECB		*ecbi;		/* in/out: event blk for interrupts  */
    struct CODE_ERR	*(*errmsg);	/* out: errmsg if code FAIL, else "" */

    {
    IMPORT   CODE	menu_screen;
    IMPORT   CODE	usermode;
    IMPORT   TAEINT	errno;
    IMPORT   TEXT	**environ;
#ifdef LARGE_PARBLK_FIX
    struct LARGE_PARBLK parblk;         /* message block to send to task     */
#else
    struct PARBLK	parblk;		/* message block to send to task     */
#endif
    CODE		status;
    TEXT		exespec[FSPECSIZ+1];
    struct FSBLOCK	fsblock;
    TAEINT		pid, child_pid;	
    TAEINT		read_chan, write_chan;	/* read/write channels	    */
    CODE		code, ret_code;
    BOOL		active;
    BOOL		asytut_req;
    COUNT		count;
    LONG		ackn_msg;		/* max 4 bytes of ackn msg */
    CODE		pr_stat;		/* process status	   */
    TEXT		errstr[STRINGSIZ+1];
    struct  CONTXT	*ctx;
    struct VARIABLE	*_tutor;
    struct VARIABLE	*lookex();
    CODE		packageFlag;		/* used to request that valids*/

/* first make sure we get a good library name by finding the parent pdf */
    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); 
	ctx = (*ctx).backlink);
    f_crack((*pctx).exe_spec, (*ctx).pdf.libr, (*pctx).pdf.name,
             EXE_TYPE, &fsblock, errstr);		/* apply defaults...	   */
    f_spec(&fsblock, exespec);			/* build file spec to run  */
    s_lower(exespec);				/* convert to lower case   */
    zero_block ((GENPTR) &parblk, (GENPTR) parblk.pool - (GENPTR) &parblk);
    parblk.last = FALSE;
    parblk.msgtyp = M_INIPAR;			/* initial parameter block */
    _tutor = lookex (&(*pctx).locst, "_TUTOR");
    packageFlag = (_tutor && IVAL(*_tutor,0)) ? VM_VALID : 0;
    if (package(pctx, &parblk, sizeof parblk.pool, packageFlag) != SUCCESS)	/* build the parblk	   */
	goto rp_overr;	

    if (parblk.blksiz >= PIPESIZ)		/* block size not too large */
        goto rp_sizerr;

    read_chan = -1;				/* initialize to -ve value */
    write_chan = -1;			
    pid = getpid();				/* process id of tm	   */
    if  ((*pctx).interrupt)			/* TAE interrupt allowed ? */
        signal (SIGINT, SIG_IGN);		/* ignore in subprocess	   */
    code = act_task(exespec, pid, environ, &read_chan,
		&write_chan, &child_pid);
    if (code != SUCCESS)
	goto rp_tiner;				/* task initiation error   */

    sync_child = child_pid;			/* save child's pid	   */
    signal (SIGPIPE, SIG_IGN);			/* dont quit on broken pipe*/


/* Task started.  Now wait on signal from task for a message read,
 * signal from operator or task completion.
 * NOTE: We enable to receive the next signal from subtask only
 * after the previous message is read and an ackowldgement is sent.
 */
    ackn_msg = ACKN_CODE;		/* acknowledgement code for subtask*/
    code = setjmp(context);		/* remember this place		*/
    if (code == 0)			/* first time			*/
	{
	op_int = FALSE;
        signal (SIGINT, int_sig);	/* enable operator signal	*/
        signal (SIGREAD, pipe_sig);	/* enable signal from task	*/
        count = write(write_chan, (GENPTR)&parblk, parblk.blksiz);
        if (count != parblk.blksiz) goto rp_msger;
	}
    if (code != 0)
        {				/* here from pipe signal longmp	*/
        active = 1;			/* tell catcher we're active	*/
	ret_code = DO_CHECK;
        count = read(read_chan, &parblk, sizeof(parblk)); /* read message  */
	if (count <= 0) goto rp_rcver;	

/*******   now process the message    ************/
        active = 0;
    	makeabs(&parblk.symtab, parblk.pool);	/* make pointers absolute    */
    	if (chk_parblk (&parblk) != SUCCESS)
	    goto rp_badparblk;
	if (parblk.msgtyp == M_FULLPDF  ||  parblk.msgtyp == M_SUBPDF)
	    {
	    asytut_req = FALSE;			/* tutoring for sync proc*/
	    dynget(pctx, &parblk, sizeof ( parblk.pool), asytut_req);	/* dynamic parameter	    */
	    logdyn(&parblk);			/* log dynamic parameters   */

	    signal (SIGREAD, pipe_sig);		/* reenable signal from task */
	    count = write(write_chan, &ackn_msg, ACKN_SIZE);	/* send ackn */
						/* now send parameter block  */
            if (parblk.blksiz >= PIPESIZ) goto rp_toolarge;
	    count = write(write_chan, (GENPTR)&parblk, parblk.blksiz);
	    if (count != parblk.blksiz) goto rp_dmser;
	    }

	else if (parblk.msgtyp == M_COMMAND)	/* execute a command line */
	    {
	    dyncommand (pctx, &parblk, sizeof parblk.pool);
	    signal (SIGREAD, pipe_sig);		/* reenable signal from task */
	    count = write(write_chan, &ackn_msg, ACKN_SIZE);	/* send ackn */
						/* now send parameter block  */
            if (parblk.blksiz >= PIPESIZ) goto rp_toolarge;
	    count = write(write_chan, (GENPTR)&parblk, parblk.blksiz);
	    if (count != parblk.blksiz) goto rp_dmser;
	    }
	else if (parblk.msgtyp == M_LOGMSG || parblk.msgtyp == M_HLOGMSG)
	    {
	    logmsg(pctx, &parblk);		/* log the message           */
	    signal (SIGREAD, pipe_sig);		/* reenable signal from task */
	    count = write(write_chan, &ackn_msg, ACKN_SIZE);	/* send ackn */
	    if (parblk.msgtyp == M_HLOGMSG)
		{
		parblk.msgtyp = M_CONTINUE;	/* handshake		     */
		makerel(&parblk.symtab, parblk.pool);
                if (parblk.blksiz >= PIPESIZ) goto rp_toolarge;
		count = write(write_chan, (GENPTR)&parblk, parblk.blksiz);
		if (count != parblk.blksiz) goto rp_mgser;
		}
	    }
	else if (parblk.msgtyp == M_OUTPUT)
	    {
	    code = outputs(pctx, &parblk);	/* apply output values	     */
            if (code != SUCCESS) goto rp_outerr;	
	    signal (SIGREAD, pipe_sig);		/* reenable signal from task */
	    count = write(write_chan, &ackn_msg, ACKN_SIZE);	/* send ackn */
	    }
	else
	    {
	    dynamic_ins (pctx, &parblk);	/* installation exit	     */
	    signal (SIGREAD, pipe_sig);		/* reenable signal from task */
	    count = write(write_chan, &ackn_msg, ACKN_SIZE);	/* send ackn */
	    }
	}
    while (FOREVER)				/* one loop for each SIGINT  */
	{
	while ((code = wait(&status)) != child_pid  && code != -1)
	    ;				/* loop if some other child...	*/
				    	/* has terminated		*/
    	if (code == child_pid)
            {
	    sync_child = 0;		/* mark that no child is active */
	    pr_stat = (status & 0177);	/*low byte is process status    */
	    if (pr_stat == 0177)	/* if process stopped (interrupted) */
		;			/* ignore the signal		*/
	    if (usermode == MENUMODE)	/* if process activated by menu	*/
		menu_screen = PROMPT_PAINT;	/* prompt before repaint menu*/
	    if (pr_stat != 0)		/* if not normal termination	*/
	        goto rp_wterr;
	    break;
	    }
    	if (code == -1  &&  errno == EINTR) /* signal came during wait	 */
	    {
	    if (op_int && (*pctx).interrupt)	/* interrupt allowed ?	 */	
	    	{
		c_suspend(child_pid);		/* suspend the task	 */
		code = interrupt(pctx);	/* do interrupt processing 	 */
	        if (code == ABORT)	/* this dialog will be aborted   */
    					/* if pipe signal comes in the	 */
    					/* the middle			 */
		    {
		    kill(child_pid, SIGKILL);	/* abort the task	 */
		    goto close_ret;
		    }
		else
		    c_resume(child_pid);	/* resume suspended task */
	        op_int = FALSE;			/* for next detection    */
		signal (SIGINT, int_sig);	/* enable operator intrpt*/
	        }
	    }
	}
    close (read_chan);				/* close the pipes	*/
    close (write_chan);
    return (DO_CHECK);


rp_overr:
    *errmsg = &er_over;
    return (FAIL);

rp_sizerr:
    *errmsg = &er_size;
    return (FAIL);

rp_tiner:
    if (child_pid == -1)
	{
	*errmsg = &er_ini;
	er_ini.code = errno;
	}
    else
	{
        *errmsg = &er_comini;
	er_comini.code = errno;
	}
    ret_code = FAIL;
    goto close_ret;

rp_wterr:
    *errmsg = &er_term;
    er_term.code = (status&0177);
    ret_code = FAIL;
    goto close_ret;

rp_msger:
    *errmsg = &er_imsg;
    er_imsg.code = errno;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_outerr:
    *errmsg = &er_out;
    kill (child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_rcver:
    *errmsg = &er_rcv;
    er_rcv.code = errno;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_badparblk:
    *errmsg = &er_par;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_toolarge:
    *errmsg = &er_toolg;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_dmser:
    *errmsg = &er_snd;
    er_snd.code = errno;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

rp_mgser:
    *errmsg = &er_acknsnd;
    er_acknsnd.code = errno;
    kill(child_pid, SIGKILL);
    ret_code = FAIL;
    goto close_ret;

close_ret:
    sync_child = 0;
    if (read_chan != -1)		/* channel open		*/
	close(read_chan);		/* close it		*/
    if (write_chan != -1)
	close(write_chan);		/* close write channel  */
    t_attn(ecbi);			/* reset ^C handler	*/
    return (ret_code);
    }


/* 	signal catchers.
 */

    int_sig()			/* here for operator attn	*/
    {
    op_int = TRUE;
    signal (SIGINT, int_sig);	/* re-enable opertor interrupt	*/
    return;
    }

/*
 *	pipe_sig.  Signal from child to read from pipe.
 */
/* 	Note on re-enabling the pipe signal:  one of them will not
 *	be lost by re-enabling on the primary level because another
 *	pipe signal may not come in until we send out a write
 *	message.
 */

    pipe_sig()			/* here when child wants to send */

    {
    longjmp(context, 1);		/* branch to main level		 */
    }    		


    FUNCTION  CODE  act_task(task_name, parent_pid,  envtbl,
			read_pipe, write_pipe, child_pid)

    TEXT	task_name[];		/* in:  task name	  	     */
    FUNINT	parent_pid;		/* in:  parent pid		     */
    TEXT 	*envtbl[];		/* in: environment table	     */
    FUNINT	*read_pipe;		/* out: read end of up-pipe	     */
    FUNINT	*write_pipe;		/* out: write end of down-pipe	     */
    FUNINT	*child_pid;		/* out: process id of activated task */

    {

    IMPORT  struct VARIABLE *switch_gbl;	/* TAE switch bits	*/
    IMPORT  CODE	run_type;	/* INTER, ASYNC, or BATCH	*/
    int	  	down_pipe[2], up_pipe[2];
    static TEXT parent_name[40];
    static TEXT	snd_pipe0[20], snd_pipe1[20], rcv_pipe0[20], rcv_pipe1[20];
    static TEXT	*parent_env[]={parent_name, snd_pipe0, snd_pipe1,
			rcv_pipe0, rcv_pipe1};
    CODE	status;
    BOOL	debug;			/* TRUE if debug switch set	*/
    TEXT	dbg_name[STRINGSIZ+1];	/* debugger name		*/
#define	MAXENV 1000
    TEXT	*envlist[MAXENV+1];	/* final environment list	*/
    COUNT	i, k;
    TEXT	*dbgptr;		/* pointer to debugger symbol	*/
    TEXT	*getenv();

		
    *child_pid = -2;				/* initialize		*/
    status = pipe(down_pipe);			/* create pipe	#1	*/
    if (status == -1) return (FAIL);
    status = pipe(up_pipe);			/* create pipe #2	*/
    if (status == -1)
    	{
	close(down_pipe[0]);			/* close the open pipes	*/
	close(down_pipe[1]);
	return (FAIL);
	}

    sprintf(parent_name, "PARENT=%d", parent_pid);
    sprintf(snd_pipe0, "DOWN_PIPE0=%d", down_pipe[0]);
    sprintf(snd_pipe1, "DOWN_PIPE1=%d", down_pipe[1]);
    sprintf(rcv_pipe0, "UP_PIPE0=%d", up_pipe[0]);
    sprintf(rcv_pipe1, "UP_PIPE1=%d", up_pipe[1]);

    debug = FALSE;
    if (run_type == INTER)			/* debug only if interactive */
	debug = (IVAL(*switch_gbl, 0) & SW_DEBUG);
    if (debug)
	{
	dbgptr = getenv("DEBUGGER");		/* get debugger name	*/
	if (dbgptr == NULL)			/* not defined		*/
	    s_copy("/bin/adb", dbg_name);	/* default for others 	*/
	else
	    s_bcopy(dbgptr, dbg_name, FSPECSIZ);  /* copy debugger name	*/
	}

    for (i=0; i<5; i++)
	envlist[i] = parent_env[i];		/* first copy locals   	*/
    for (i=5, k=0; i < MAXENV; i++, k++)	/* copy shell  exports	*/
	{
	envlist[i] = envtbl[k];
	if (envlist[i] == NULL)
	    break;				/* last one copied	*/
	}
    envlist[MAXENV] = 0;

    *child_pid = vfork();			/* use 'vfork' for fastness  */

    if (*child_pid == 0)
        {
	if (debug)
            execle(dbg_name, dbg_name, task_name, 0, envlist);
	else
	    execle(task_name, task_name, 0, envlist);
	_exit(1);
        }
    if (*child_pid == -1) return (FAIL);
    *read_pipe = up_pipe[0];		/* end to read from		*/
    *write_pipe = down_pipe[1];		/* end to write to		*/
    close (down_pipe[0]);		/* close unused ends		*/
    close (up_pipe[1]);
    return (SUCCESS);
    }

/*
 *	c_suspend.  Suspend the executing task	.
 *	Note: under standard UNIX, this is a NOOP.
 */

    FUNCTION  static VOID  c_suspend(child_pid)

    int		child_pid;		/* process id of task to suspend */

    {
#ifndef  SYSV
    kill(child_pid, SIGSTOP);		/* send signal to stop		 */
#endif
    return;
    }

/*
 *	c_resume.  Resume the suspended task.
 *	Note: under standard UNIX, this is a NOOP.
 */

    FUNCTION  static VOID  c_resume(child_pid)

    int		child_pid;		/* process id of suspended task */

    {
#ifndef  SYSV
    kill(child_pid, SIGCONT);		/* send signal to continue 	*/
#endif
    return;
    }
#endif
