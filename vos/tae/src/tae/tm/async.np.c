/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* >>  UNIX  << */
/*      Non-portable Async Processing Routines
 *      --------------------------------------
 *
 * CHANGE LOG:
 *
 *      21-may-84       Initial release...dm
 *      12-jun-85       Update for TAE-V1.3 features...dm
 *      18-jun-85       Do not assume tm from TSYSLIB library...dm
 *      05-jul-85       Add block_opint(), other minor changes...dm
 *      23-jul-85       Fix lint errors...dm
 *      15-aug-85       Use SIGTERM instead of SIGKILL to abort the async
 *                      monitor...dm
 *      16-aug-85       Fix compilation errors for RCJM case...dm
 *      19-aug-85       Add function asy_recs...dm
 *      28-aug-85       Add handshake logic to asy_recs() and sig_io()...dm
 *      08-sep-85       Return SUCCESS on operator interrupt in asy_recs...dm
 *      01-aug-86       Make changes for ASYNC-PROCESS...lia
 *      12-aug-86       Cleanup definition of parent_pid and parent_env...lia
 *      13-aug-86       Fix PARENV define bug...lia
 *      15-aug-86       Check for SYS$OUTPUT set to NULL and reset it...lia
 *      19-aug-86       Add async_stdout() to update _STDOUT for ASYNC-PROCESS
 *                      ...lia
 *	11-dec-86	Fix lint warnings...peb
 *	21-jan-87	Check block size for output via pipe...lia
 ************************** Merge TCP changes...gc, 5 Mar 87 ****************
 *	02-jan-87	Call n_wait() rather than sleep() for TCPIP...dm 
 *	03-jan-87	Ignore operator interrupt before activating child...dm
 *	05-jan-87	Add asy_rcv_init() to initialize async msg recv...dm
 *	07-jan-87	Check for interrupt before n_wait in asy_recs...dm
 *      02-Apr-87	Add parameters to sig_io for test use...gc
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	25-oct-88	Added #ifdef BSD_UNIX around SIGCONT to match other...ln
 *	08-nov-88	-vfork() defined for SYSV in taeconf.inp
 *			-#ifdef BSD_UNIX -> #ifndef SYSV...ljn
 *	10-nov-88	If SYSV, #include fcntl.h...ljn
 *	27-jun-89	Temporary fix to enable TAE to build on pure
 *			SYSV systems: #ifdef out set_signal calls...ljn
 *			(True fix is to get #ifdef TAE_ASYNC to work again.)
 *	01-jul-89	types.h not included by file.h on SYSV machines...ljn
 *	11-sep-89	Added NOSIGVEC...ljn
 *	23-may-90	Removed RCJM stuff by referring to old TAE_RCJM...ljn
 *			n_wait() is now sleep(), disable_intp()->signal()...ljn
 *	13-dec-90	TMIMAGE is now taetm...ljn
 *	17-jun-91	PR1099: use $TAEBIN/$TAEPLAT/taetm...ljn
 *
 ****************************************************************************
 */

#include        "taeconf.inp"       /* TAE configuration definitions            */
#include        "tminc.inc"         /* TM-only host-independent definitions     */
#include        "asyncinc.inc"      /* async processing definitions             */
#include        "terminc.inc"       /* terminal package definitions             */
#include        "tmhost.inp"        
#include        "taskinc.inp"       /* task manipulation definitions            */
#include        <signal.h>
#ifdef macII
#include	<sys/types.h>
#else
#ifdef SYSV
#include	<fcntl.h>
#include	<sys/types.h>
#endif
#endif
#include        <sys/file.h>
#include	<errno.h>

    GLOBAL      v31async = 0;   /* source version                           */


    static TAEINT	parent_pid;	/* pid of parent tm		    */
    static COUNT        asyact_num = 0; /* number of asyncs active          */
    static int          rcv_pipe[2];    /* channel to receive all async msg */
    static struct  MON_MSG   saved_msg; 
    static struct  TCB  *waiting_tcb = NULL;    
                                        /* tcb of child waiting for msg     */
    static  BOOL        msg_read;       /* msg read by asy_recs before exit */
    static BOOL         msg_exist=FALSE; /* msg already there               */
    static  BOOL        ignore_int=FALSE;       /* ignore operator interrupt*/
    static LONG  ackn_msg = ACKN_CODE;   /* max 4 bytes of acknl. to child  */
    VOID                sig_io();       /* I/O signal catcher               */

    static TAEINT	async_client_id; /* Used to create client flag	*/
    static TAEINT	*async_client = &async_client_id;

/*
 *      abort_acb. Abort the monitor associated with an acb and
 *      release associated resources
 *              
 *              We also reset the async_request flag if necessary.  Note that
 *              there's a race condition here (a new request comes in after
 *              the call to find_oldest but before async_request is reset)
 *              that we're ignoring.
 */
    FUNCTION VOID abort_acb(acb)

    struct ACB          *acb;                   /* in: the acb              */

    {
    IMPORT BOOL         async_request;          /* TRUE if requests waiting */
    CODE                code;
    struct ACB          *find_oldest();
    struct SFILE        f;


    if ((*acb).waiting)                         /* delete the comm file     */
        {
        code = f_opnspc(&f, SAVELUN, (*acb).requ_file, "", "",
                    ASY_TYPE, F_READ);
        if (code == SUCCESS) f_close (&f, F_DELETE);
        }
    (*acb).waiting = (*acb).active = FALSE;
    if (find_oldest() == NULL)                  /* reset if no others waiting */
        async_request = FALSE;                  /* otherwise leave it in current state */
    if ((*acb).remote)                          /* all done if a remote job   */
        return;                                 
#ifndef SYSV
    kill ((*(*acb).atcb).pid, SIGCONT);         /* resume in case suspended   */
#endif
    kill ((*(*acb).atcb).pid, SIGTERM);         /* force exit                 */
    rel_rsrc ((*acb).atcb);                     /* release resources          */
    return;
    }


/*
 *  act_mon - activate a copy of the monitor
 *
 *  returns SUCESS or FAIL; failure code in tcb
 *
 */
    FUNCTION static CODE act_mon (atcb, task_name, job_type, parent_pid,
                 envtbl, job_spec, log_spec, down_pipe, up_pipe)

    FAST struct ATCB    *atcb;          /* in/out: address of an atcb   */
                                        /* (*atcb).pid is an output*/
    TEXT        task_name[];            /* in: name of task to activate */
    FUNINT      job_type;               /* in:  async or remote job     */
    FUNINT      parent_pid;             /* in: parent process id        */
    TEXT        *envtbl[];              /* in: environment table        */
    TEXT        job_spec[];             /* in: file spec of .job file   */
    TEXT        log_spec[];             /* in: name of output log file  */
    FUNINT      down_pipe[];            /* in:channel id of send pipe   */
    FUNINT      up_pipe[];              /* in:channel id of receive pipe*/


    {
    IMPORT TAEINT  errno;               /* error number                 */

#define MAXENV 1000

    static TEXT runtype[] = "RUNTYPE=ASYNC";
    static TEXT act_mode[] = "TAE_ACTMODE=LOCAL";
    static TEXT infile[FSPECSIZ+20] = "CHILD_CMD_INPUT=";
    static TEXT outfile[FSPECSIZ+20] = "CHILD_JOB_LOG=";
    static TEXT path_id[FNAMESIZ+10] = "PATHID="; 
    static TEXT parent_name[40];
    static TEXT snd_pipe0[20], snd_pipe1[20], rcv_pipe0[20], rcv_pipe1[20];
    static TEXT *parent_env[]={runtype, act_mode, parent_name, 
                snd_pipe0, snd_pipe1, rcv_pipe0, rcv_pipe1};
    TEXT        *envlist[MAXENV+1];     /* final environment list       */
    int         child_pid;              /* async monitor's pid          */
    COUNT       i, k;
                
#define PARENV  sizeof(parent_env) / sizeof(TEXT *)

    child_pid = -2;                             /* initialize, not 0 or -1  */
    sprintf(parent_name, "PARENT=%d", parent_pid);
    sprintf(snd_pipe0, "DOWN_PIPE0=%d", down_pipe[0]);
    sprintf(snd_pipe1, "DOWN_PIPE1=%d", down_pipe[1]);
    sprintf(rcv_pipe0, "UP_PIPE0=%d", up_pipe[0]);
    sprintf(rcv_pipe1, "UP_PIPE1=%d", up_pipe[1]);


    for (i=0; i<PARENV; i++)
        envlist[i] = parent_env[i];             /* first copy locals    */
    if (job_type == ASYNC)                      /* activating async tm  */
        {
        s_copy(job_spec, &infile[16]);
        s_copy(log_spec, &outfile[14]);
        envlist[i++] = infile;                  /* job file name in environ */
        envlist[i++] = outfile;                 /* log file name in environ */
        }
    for (k=0; i < MAXENV; i++, k++)     /* copy shell  exports  */
        {
        envlist[i] = envtbl[k];
        if (envlist[i] == NULL)
            break;                              /* last one copied      */
        }
    envlist[MAXENV] = 0;

    child_pid = vfork();                    /* use 'vfork' for fastness  */
    if (child_pid == 0)
        {
        execle(task_name, task_name, 0, envlist);
        _exit(1);                               /* only if execle failed     */
        }
    if (child_pid == -1)
        {
        (*(*atcb).tcb).hostcode = errno;
        return (FAIL);
        }
    (*atcb).pid = child_pid;
    (*(*atcb).tcb).child_pid = child_pid;
    (*(*atcb).tcb).hostcode = 0;
    return (SUCCESS);
    }

/* 	asy_rcv_init.  Initialize for async msg reception from child.
 *
 *	This routine throws away any old message from the asynchronous
 *	task - used for starting a new job after a control c.
 */
	
    FUNCTION  VOID  asy_rcv_init(tcb)

    struct	TCB	*tcb;		/* in: task control block	     */
    {

    if (msg_exist && (saved_msg.m_childpid == (*tcb).child_pid))
	{
	write((*tcb).send_chan, &ackn_msg, 
			ACKN_SIZE);		/* send ackn to waiting child */
	msg_exist = FALSE;			/* Throw away old message    */
	}
    return;
    }

/*
 *  act_process - activate an ASYNC-PROCESS process
 *
 *  returns SUCCESS or FAIL
 */
    FUNCTION static CODE act_process (atcb, task_name, parent_pid, envtbl,
                                      log_spec, down_pipe, up_pipe)

    FAST struct ATCB    *atcb;          /* in/out: address of an atcb   */
                                        /* (*atcb).child_pid is an output*/
    TEXT        task_name[];            /* in: name of task to activate */
    FUNINT      parent_pid;             /* in: parent process id        */
    TEXT        *envtbl[];              /* in: environment table        */
    TEXT        log_spec[];             /* in: name of output log file  */
    FUNINT      down_pipe[];            /* in:channel id of send pipe   */
    FUNINT      up_pipe[];              /* in:channel id of receive pipe*/


    {
    IMPORT TAEINT  errno;               /* error number                 */

#define MAXENV 1000

    static TEXT outfile[FSPECSIZ+20] = "PROCESS_JOB_LOG=";
    static TEXT parent_name[40];
    static TEXT snd_pipe0[20], snd_pipe1[20], rcv_pipe0[20], rcv_pipe1[20];
    static TEXT *parent_env[]={parent_name, outfile, 
                                    snd_pipe0, snd_pipe1, rcv_pipe0, rcv_pipe1};
    TEXT        *envlist[MAXENV+1];     /* final environment list       */
    int         async_proc_pid;         /* async process's pid          */
    COUNT       i, k;
                
#define TASKENV  sizeof(parent_env) / sizeof(TEXT *)
                                        /* # of env parameters from parent */

    async_proc_pid = -2;                        /* initialize, not 0 or -1  */
    if (log_spec == NULL)
        s_copy("/dev/null", &outfile[16]);      /* no log file specified  */
    else
        s_copy(log_spec, &outfile[16]);         /* log file name in environ */
    sprintf(parent_name, "PARENT=%d", parent_pid);
    sprintf(snd_pipe0, "DOWN_PIPE0=%d", down_pipe[0]);
    sprintf(snd_pipe1, "DOWN_PIPE1=%d", down_pipe[1]);
    sprintf(rcv_pipe0, "UP_PIPE0=%d", up_pipe[0]);
    sprintf(rcv_pipe1, "UP_PIPE1=%d", up_pipe[1]);


    for (i=0; i<TASKENV; i++)
        envlist[i] = parent_env[i];             /* first copy locals    */
    for (i=TASKENV, k=0; i < MAXENV; i++, k++)  /* copy shell  exports  */
        {
        envlist[i] = envtbl[k];
        if (envlist[i] == NULL)
            break;                              /* last one copied      */
        }
    envlist[MAXENV] = 0;

    async_proc_pid = vfork();               /* use 'vfork' for fastness  */
    if (async_proc_pid == 0)
        {
        execle(task_name, task_name, 0, envlist);
        _exit(1);                               /* only if execle failed     */
        }
    if (async_proc_pid == -1)
        {
        (*(*atcb).tcb).hostcode = errno;
        return (FAIL);
        }
    (*atcb).pid = async_proc_pid;
    (*(*atcb).tcb).hostcode = 0;
    return (SUCCESS);
    }

/*      asy_rcv_unint. Receive the async msg ignoring the operator interrupt.
 *
 *      Note: This routine is usually called instead of  an asy_recv call
 *      to get the message anyway.
 */

    FUNCTION  CODE  asy_rcv_unint (tcb, block, blksize)

    struct      TCB     *tcb;           /* in: task control block            */
    GENPTR              block;          /* in: address of receive block      */
    FUNINT              blksize;        /* in: number of bytes expected      */
    
    {
    CODE                code;

    ignore_int = TRUE;                  /* don't quit on operator interrupt */
    code = asy_recs(tcb, block, blksize, NULL, NULL);
    ignore_int = FALSE;
    return (code);
    }


/*	recv_occur. Check if message reception has occured.
 *			If TRUE, but not already read by asy_recs, the
 *			message is retrieved by this routine.
 */

    FUNCTION  BOOL  recv_occur(tcb, block, blksize)

    struct	TCB	*tcb;		/* in: task control block	     */
    GENPTR		block;		/* in: address of receive block      */
    FUNINT		blksize;	/* in: number of bytes expected      */
    
    {
    if (msg_read)			/* asy_recs returned after msg  */
	return (TRUE);
    if (!msg_exist)			/* no msg arrived after that    */
	return (FALSE);
    if (saved_msg.m_childpid != (*tcb).child_pid)
	return(FALSE);			/* from some other child	*/
    asy_rcv_unint(tcb, block, blksize);	/* transfer the message to user */
	return(TRUE);
    }

/*
 *      asy_recs.  Receive a message from async task.   
 *
 *	NOTE 1: the action routine and action parameter are ignored.
 *	The signal handler uses its own action routine.
 *
 *	Also, note that we check for child pid of the saved message 
 *	because an unsolicited msg might have sent by another 
 *	terminated or aborted child.
 *
 *	NOTE 2:  Do not clear ecbi here as that is checked afterwards
 *	to determine if operator killed the job.
 */

    FUNCTION  CODE  asy_recs(tcb, block, blksize, actadr, actparm) 

    struct      TCB     *tcb;           /* in: task control block */
    GENPTR              block;          /* in: address of receive block      */
    FUNINT              blksize;        /* in: number of bytes expected      */
    VOID                (*actadr)();    /* in: address of an action routine  */
    GENPTR              actparm;        /* in: action routine parm (ignored) */

    {
    IMPORT  struct ECB  ecbi;           /* operator interrupt ecb            */
/** int                 sig_mask;       * signal to wait for        * 
  * int                 old_mask;
  */

    if (actadr != NULL)                 /* don't wait for msg arrival  */
        return (SUCCESS);
    else                                /* wait until a message arrives  */
        {
        msg_read = FALSE;
        waiting_tcb = tcb;              /* save child's tcb address     */
        while (FOREVER)
            {
            if (msg_exist && ((*tcb).child_pid == 
                saved_msg.m_childpid))   /* msg has arrived from the child   */
                break;
/*****  Recheck validity of the following code using sigblock, sigpause ******
 *          sig_mask = 1 << (SIGIO-1);          * right most bit is bit 1    *
 *          old_mask = sigblock(sig_mask);      * block SIGIO temporarily    *
 *          sigpause(0);                        * wait till a signal arrives *
 *******/
            if ((!ignore_int)  && e_occur(&ecbi))       /*  interrupt signal  */
                {
/*******        sigblock(old_mask);             * restore signal SIGIO ********/
                return(SUCCESS);                /* msg will be received later */
                }
	    sleep(1);				/* wait for 1 second	      */
            }
        waiting_tcb = NULL; 
        bytmov((GENPTR) &saved_msg, block, blksize); 
        msg_read = TRUE;
        msg_exist = FALSE;
        write((*tcb).send_chan, &ackn_msg, 
                        ACKN_SIZE);             /* send confirmation to child */
        }
    return(SUCCESS);
    }


/*
 * asy_snds. Send a message in an asynchronous environment.
 *
 *      Return code :
 *              SUCCESS - Message sent successfully
 *              FAIL - Message could not be sent thru the pipe
 */

    FUNCTION  CODE  asy_snds(tcb, block, blksize)

    struct TCB          *tcb;           /* in: task control block       */
    GENPTR              block;          /* in: data block to send       */
    FUNINT              blksize;        /* in: number of bytes to send  */

    {
    IMPORT  int         errno;
    int                 count;

    if (blksize >= PIPESIZ)  
        {
        (*tcb).hostcode = EMSGSIZE;
        return (FAIL);
        }
    count = write((*tcb).send_chan, block, blksize);
    if (count != blksize)				/* error             */
        {
        (*tcb).hostcode = errno;                        /* save error code   */
        return (FAIL);
        }
    return (SUCCESS);
    }

/*
 *      async_stdout - Update _STDOUT local variable for UNIX
 *                     ASYNC-PROCESS.
 *
 *
 *      By default, the _STDOUT is set to the UNIX null device so
 *      we can have fast ASYNC-PROCESS startup.  If the process 
 *      has an explicit STDOUT qualifier, then we set _STDOUT
 *      to the qualifier value; otherwise use "/dev/null" device.
 */

    FUNCTION VOID async_stdout (cmdctx, stdout_string)

    struct CONTXT       *cmdctx;        /* context block for the process */
    TEXT                *stdout_string; /* in: STDOUT value or NULL if   */
                                        /* no STDOUT qualifier present   */

    {
    IMPORT struct VARIABLE *lookex();
    struct VARIABLE        *v;
    TEXT                   *svv[1];

    if (stdout_string == NULL)
        stdout_string = "/dev/null" ;
    svv[0] = stdout_string;
    v = lookex (&(*cmdctx).locst, "_STDOUT");
    set_component (v, svv, 1);          /* index of 1 will give 1st component */
    return;
    }

/*
 *      block_opint. Block operator interrupt.
 */
 
    FUNCTION  VOID  block_opint()

    {
    int                 int_mask;

        
    int_mask = 1 << (SIGINT-1);         /* right most bit is bit 1      */
#if !defined(NOSIGVEC) && !(defined(sun) && OSMajorVersion >= 5)
    sigblock(int_mask);                 /* block operator interrrupt    */
#endif
    return;
    }


/*      build_name.  Build UNIX process name for ASYNC job.
 *
 *      The name must be unique in the group and we would like
 *      to see the proc name in there also, but we only get
 *      14 characters.  Also, we must allow several jobs
 *      with same proc name to be active at once.
 *
 *      The process name generated here is not the job name
 *      that the TAE user knows about.  The TAE user can't
 *      see ASYNCs from other TMs, so he doesn't want the
 *      TM "pid" field, nor does he want the restriction that
 *      only a part of the proc name is in the job name.
 *
 */
    FUNCTION static VOID build_name (pid, proc, seq, name)

    TEXT        pid[];          /* in: process ID of TM         */
    TEXT        proc[];         /* in: proc name (not file spec)*/
                                /* (we use as much of this as possible) */
    FUNINT      seq;            /* in: 1-99 sequence number     */
    TEXT        name[];         /* out: process name            */

    {
    COUNT       i, j;
    TEXT        seqstr[10];     /* TBD: [seqstrSIZ+1]           */

    s_copy (pid, name);
    i = s_append (proc, name);
    name [PNAMESIZ-2] = EOS;            /* clip if too long     */
    sprintf (seqstr, "%02x", seq);      /* get sequence as ascii*/
    for (i = 0, j = s_length(seqstr)-1; i < j; i++)
        {
        if (seqstr[0] != '0' ) break;
        s_shift (seqstr, 1);            /* delete leading zeros */
        }
    s_append (seqstr, name);            /* add in sequence nr   */
    return;
    }

/*  create_async - Create the async TAE monitor (or a ASYNC-PROCESS process)
 *                 and initialize tcb.
 *
 *  returns SUCCESS or FAIL, and a host-dependent error in tcb
 *
 *      Establish a single pipe to receive message from all
 *      asynchronous processes. In addition, for each activated
 *      async process, create a down pipe to send messages.
 *      No up-pipes are necessary here because all real data
 *      exchanges between the parent tm and the async tm
 *      take place in the form of disc files.
 *
 *      The message sent to tm had the message type and file name
 *      if appropriate. The received message contains the child_pid
 *      to identify the sender.
 *
 */

    FUNCTION CODE create_async (atcb, mb_size, procname, job_type, job_spec, log_spec)

    FAST struct ATCB    *atcb;          /* in/out: address of an atcb      */
    FUNINT              mb_size;        /* in: size to use for mailbox     */
    TEXT                procname[];     /* in: name of proc to be run      */
    FUNINT              job_type;       /* in: ASYNC, ASYNC_PROCESS, REMOTE */
    TEXT                job_spec[];     /* in: ASYNC or REMOTE: .job filespec */
                                        /*     ASYNC_PROCESS: filespec of executable */
    TEXT                log_spec[];     /* in: ASYNC: output log file name */
                                        /*     ASYNC_PROCESS: SYS$OUTPUT file */

    {
    IMPORT      TEXT    **environ;              /* enviromnent  table         */
    IMPORT  int         errno;
    IMPORT  struct ECB  ecbi;                   /* ecb for ^C                 */
    CODE                code;
    TEXT        task_name[FSPECSIZ+1];          /* name of task to create     */
#ifdef BUG_FIXES
    TEXT        txtname[STRINGSIZ+1];           /* new process name           */
/* Must be bigger than PNAMESIZ because it's strcat'ed to and THEN truncated */
#else
    TEXT        txtname[PNAMESIZ+1];            /* new process name           */
#endif
    TEXT        pid_name[PNAMESIZ+1];   
    int         send_pipe[2];
    int         status;
    TAEINT      parent_pid;
    char        *ptr;
    char        *getenv();
    static COUNT        seq_num = 0;            /* for build_name             */

/*   generate UNIX name for new monitor process   */

    parent_pid = getpid();                              /* process id of tm     */
    sprintf(pid_name, "%d", parent_pid);                /* convert to ascii     */
    build_name(pid_name, procname, seq_num, txtname);   /* build new process name       */
    s_copy (txtname, (*atcb).session_id);               /* is also session_id           */

/*  Create a common receive pipe to receive messages from all active
 *  async jobs. Note that this pipe is created only when there are no
 *  async jobs already active and closed when all have terminated.
 */
    if (asyact_num == 0)                                /* if none active    */
        {
        status = pipe(rcv_pipe);                        /* open the up_pipe  */
        if (status == -1)       
            {
            (*(*atcb).tcb).hostcode = errno;    /* save error code   */
            return (FAIL);
            }
/*      The FASYNC flag was deleted from the following call due to     */
/*      conflict with SunView.                                         */
        fcntl(rcv_pipe[0], F_SETFL, FNDELAY);    /* non_block,   */
#if !defined(NOSIGVEC) && !(defined(sun) && OSMajorVersion >= 5)
        set_signal(SIGIO, sig_io);          /* signal on msg arrival */
#endif
        }

/*   Create a down pipe to send data to the process */

    status = pipe(send_pipe);                           /* create the link   */
    if (status == -1)
        goto undo_pipes;                                /* could not create  */
    (*atcb).send_chan = send_pipe[1];                   /* for asy_ protocol */
    (*(*atcb).tcb).send_chan = (*atcb).send_chan;       /* save in tcb too   */


/*  activate a copy of the monitor/parent agent or an ASYNC-PROCESS process.
 *  (NOTE: the task is run from the same library in which the parent tae
 *  monitor resides, except for the async process which should have the library
 *  name specified thru input job_spec.
 *
 *    NOTE: f_libr() call returns the last '/' as part of library name
 */

    if (job_type == ASYNC_PROCESS)
        {
        s_copy(job_spec, task_name);
        code = act_process (atcb, task_name, parent_pid, environ, 
                log_spec, send_pipe, rcv_pipe);  /* activate ASYNC-PROCESS proc */
        }
    else	/* job_type == ASYNC  ... local async */
        {
      	task_name[0] = EOS;
        if ((ptr = getenv("TAEBIN")) != NULL)  
            s_copy(ptr, task_name);
        s_append("/", task_name);
        if ((ptr = getenv("TAEPLAT")) != NULL)
            s_append(ptr, task_name);
        s_append("/taetm", task_name);
        signal(SIGINT, SIG_IGN);	/* ignore ^C at child creation 	*/
	code = act_mon(atcb, task_name, job_type, parent_pid, environ, job_spec,
	    log_spec, send_pipe, rcv_pipe);          /* activate monitor/agent    */
        }
    if (code != SUCCESS)
        goto undo_pipes;                                

    t_attn(&ecbi);                              /* reset ^C handler          */
#if !defined(NOSIGVEC) && !(defined(sun) && OSMajorVersion >= 5)
    set_signal (SIGPIPE, SIG_IGN);              /* dont quit on broken pipe  */
#endif
    asyact_num++;
    if (++seq_num == MAXACB) seq_num = 0;

    close (send_pipe[0]);                       /* close the unused end      */
    return(SUCCESS);

undo_pipes:
    if (asyact_num == 0)
        {
        close(rcv_pipe[0]);                     /* close open pipes         */
        close(rcv_pipe[1]);     
        }
    (*(*atcb).tcb).hostcode = errno;
    close(send_pipe[0]);                        /* close open pipes         */
    close(send_pipe[1]);        
    return(FAIL);
    }


/*      host_job.     Write the host part of an ASYNC .job file
 *
 *      Note that for a remote job we run an AGENT rather than TM.
 */
    FUNCTION host_job (job, cmdctx, user_proc)

    struct SFILE        *job;           /* in/out: SFILE opened to job file   */
    struct CONTXT       *cmdctx;        /* in: command context                */
    FUNINT              user_proc;      /* in: TRUE if job file for user proc */

    {
    CODE                code;
    struct FSBLOCK      job_block;
    TEXT                file_spec[FSPECSIZ+1];
    COUNT               lun;            /* not used in VMS or UNIX      */

    s_copy (DEF_DIR, job_block.libr);           /* File created in def dir   */

    s_copy ((*cmdctx).pdf.name, job_block.name);/* Use proc name as filename */
    s_copy (JOB_TYPE, job_block.type);          /* Extension is "JOB"       */
    job_block.attr[0] = EOS;
    code = f_opnblk (job, lun, &job_block, F_WRITE);
    if (code != SUCCESS)
        {
        f_spec (&job_block, file_spec);
        tmmsg (PROCFAIL, "Unable to create job file '%s'. %s",
               "TAE-JOBOPN", file_spec, (*job).errmsg);
        return (FAIL);
        }
    return (SUCCESS);
    }


/* rel_rsrc - Release system resources
 *
 */
    FUNCTION VOID rel_rsrc (atcb)

    struct ATCB         *atcb;

    {

    close ((*atcb).send_chan);                  /* close the write pipe     */
    asyact_num--;                               /* decrement asy jobs active*/
    if (asyact_num == 0)                        /* if no more active        */
        {
        close (rcv_pipe[0]);                    /* also close receive pipe  */
        close (rcv_pipe[1]);
        }       
     return;
    }


/*
 *      sig_io. signal catcher for SIGIO .
 */

    FUNCTION  VOID sig_io( sig, code, scp )
    int		sig;	/* Signal number */
    int		code; 	/* Signal detail */
    struct sigcontext  *scp;  /* Pointer to pre-signal context */
    {
    IMPORT  int         errno;
    struct  ACB         *acbptr;
    int                 count;
    struct MON_MSG      async_buf;
    struct ACB          *find_id();


    count = read(rcv_pipe[0], (GENPTR) &async_buf,
                        sizeof(struct MON_MSG));        /* read the msg      */
    if (count != sizeof (struct MON_MSG))               /* none there, error */
        {
        tmmsg (PROCFAIL,
                "Error receiving async monitor message. Host code = %d.",
                "TAE-ASYRCV", errno);                   /* internal error    */
        return;                                         
        }
    if (waiting_tcb != NULL)                    /* parent waiting for reply  */
        {
        if ((*waiting_tcb).child_pid == async_buf.m_childpid)
                                                /* msg came from that child   */
            {
            waiting_tcb = NULL;                         /* reset for next msg */
            bytmov((GENPTR)&async_buf, (GENPTR)&saved_msg,
                        sizeof (struct MON_MSG));
            msg_exist = TRUE;                           
            return;
            }
        }
    acbptr = find_id(async_buf.m_childpid);             /* get the right acb  */
    if (acbptr != NULL)
        {
        bytmov((GENPTR)&async_buf, (*(*acbptr).atcb).inmsg,
                        sizeof (struct MON_MSG));
        write((*(*(*acbptr).atcb).tcb).send_chan, &ackn_msg, 
                        ACKN_SIZE);             /* send confirmation to child */
        asy_rcv_done(acbptr);
        }
    else
        {
        bytmov((GENPTR)&async_buf, (GENPTR)&saved_msg,
                        sizeof (struct MON_MSG));
        msg_exist = TRUE;                       /* save for next requst       */
        }
    return;
    }
