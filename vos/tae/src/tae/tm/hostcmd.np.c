/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*<<BSD/UNIX>>*/
/*
 *	hostcmd.   Host-dependent TCL intrinsic commands.
 *
 *	CHANGE LOG:
 *
 *	21-dec-83	Set $SFI, $SKEY after shell termination..dm
 *	10-may-84	set menu_screen (for TAE V1.2)...dm
 *	16-may-84	get cmd interprter name as a defined symbol...dm
 *	01-jul-85	Change VALUE_x to xVAL, Y_BATCH to Y_ABI...dm
 *	15-aug-85	Add exit handler to abort children when tm dies...dm
 *	12-sep-85	Add set_exit_sig for exit time cleanup...dm
 *	03-dec-86	V1.4: Honor host_enabled flag in shell_do...peb
 *	15-dec-86	Fix call seq in abort_remote call...peb
 *	04-jan-87	Add new HI009 intrinsic for FDs...palm
 *	19-jan-87	Make interactive and non-interactive shells to
 *			be consistent (PR #1104)...dm
 *      12-aug-87       Added get_hostcmd()...tpl
 *	18-feb-88	Bring HI002 from VMS to unix...dm
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	10-nov-88	SYSV needs to #include fcntl.h...ljn
 *	31-jan-89	from hi002, call delete_ivp_pool to clean
 *			things up for dynamic memory monitoring...palm
 *	01-may-89	alter check on cmd_mode...palm 
 *	01-jul-89	types.h not included by file.h on SYSV machines...ljn
 *	23-may-90	Remove RCJM stuff by referring to old TAE_RCJM...ljn
 *	28-jun-90	Removed get_hostcmd()...ljn
 *	06-feb-91	dm_bytes stuff ifdef'd out for FAST_ALLOC...ljn
 */

#include	"taeconf.inp"	/* TAE configuration definitions	*/
#include	"syninc.inc"	/* syntax block				*/
#include	"tminc.inc"	/* TM-only host-independent definitions	*/
#include	<signal.h>
#ifdef TAE_ASYNC
#include 	"asyncinc.inc"
#endif
#ifdef macII
#include	<compat.h>
#include 	<sys/types.h>
#else
#ifdef SYSV
#include 	<fcntl.h>
#include 	<sys/types.h>
#endif
#endif
#include        <sys/file.h>	/* for open/close HI009 use		*/

    GLOBAL	v17host = 0;	/* source verison			*/
    VOID	term_sig();


/* Table of intrinsic commands in this source module: 	*/

    GLOBAL CODE shell_do() , fd_do();	
#ifndef FAST_ALLOC
    GLOBAL VOID dm_bytes_do();
#endif

    GLOBAL struct ITRCMD hostcmd[] = 	
	{

/*  name        sub     flags	                  pcount    pdf    do-fun   */

#ifdef SYNTAX_CHECK
{0, "USH",      "",  Y_ABI|Y_BODY|Y_CLEAR|Y_CMD|Y_SYNTAX|Y_DEFSUB,
#else
{0, "USH",	"",  Y_ABI|Y_BODY|Y_CLEAR|Y_CMD|Y_DEFSUB,
#endif
						   0,     NULL,   shell_do }, 
#ifndef FAST_ALLOC
{0, "HI002",    "",  Y_ABI|Y_BODY|Y_CLEAR|Y_CMD,   0,     NULL,   dm_bytes_do},
#endif

{0, "HI009",    "",  Y_ABI|Y_BODY|Y_CLEAR|Y_CMD|Y_DEFSUB,
						   0,     NULL,    fd_do},
{0, ""}   /* TERMINATOR ENTRY: required */ 
	};



/*
 *	shell_do.   Shell command.  Everything past the command field is sent
 *	to the shell as the command to be executed.
 *
 *	While the shell command is executing, to TM it is like a
 *	process is running. TM waits until shell execution is complete.
 *	However no message communication exists between the shell and
 *	TM and proc interrupt mode is not available.
 *	Note that if no parameter follows the shell command when invoked
 *	by the user, then shell is run interactively.
 */

    FUNCTION CODE shell_do(cpctx, npctx)

    	struct CONTXT	*cpctx;		/* in: current proc context	*/
	struct CONTXT	*npctx;		/* in: context of DCL command	*/

	{
	IMPORT  struct VARIABLE *skey_gbl;	/* prt to $skey		*/
	IMPORT  struct VARIABLE *sfi_gbl;	/* ptr to $sfi		*/	
	IMPORT CODE 	menu_screen;
	IMPORT CODE	cmd_mode;		/* intmode or normmode  */
	IMPORT BOOL	host_enabled;		/* TRUE if USH command OK*/

	struct SYNBLK	*sb;			/* ptr to syntax block	*/
        TEXT		*i;
	CODE		status;
	TEXT		*sh_string;
	TEXT		skey_val[KEYSIZ+1];
	TEXT		*value[1];		/* scratch value vector */	
	TEXT		shell_string[CMDLINSIZ+1];
	TEXT		*ptr;
	COUNT		len;			/* length of string 	*/
	TEXT		*getenv();

	if (menu_screen != UNDEFINE)		/* if not first time    */ 
	    menu_screen = PROMPT_PAINT;		/* force menu prompt	*/
	if (!host_enabled)
	    {
	    tmmsg (PROCFAIL, "'USH' command has been disabled.",
		"TAE-DISHOST");
	    return (DO_CHECK);
	    }
	if (cmd_mode != NORMCMD)	/* only from normal cmd mode! */
	    {
	    tmmsg(PROCFAIL, "'USH' only available in normal command mode.", 
		"TAE-BADCMD");
	    return (DO_CHECK);
	    }
	sb = (*npctx).sb;
	for (i=(*sb).curchr; *i != EOS; i++);	/* go to end of string	*/
	for(--i; *i == ' ' || *i == '\t'; i--);	/* ignore trail white space  */
	if (*i == '\\')				/* last character is '\'     */
	    {
	    tmmsg(PROCFAIL, "'\' invalid as last character.", "TAE-INVSHL");
	    return (DO_CHECK);
	    }
	ptr = getenv("SHELL");			/* get value of symbol SHELL */
	if (ptr == NULL)
    	    s_copy("sh" , shell_string);	/* default name		     */
	else
   	    s_bcopy(ptr, shell_string, FSPECSIZ);

	sh_string = (*sb).curchr;		/* ptr to rest of TCL cmd    */
	if (chkend(sb) == SUCCESS)		/* if command is shell only  */
	    {
	    s_append(" -i", shell_string);	/* make shell interactive    */
	    }
	else
	    {
	    len = s_append(" -c \"", shell_string);	/* command follows  */
  	    while (*sh_string == ' ' ||
		   *sh_string == '\t')
		sh_string++;			/* strip leading white space */
	    s_bcopy(sh_string, &shell_string[len], 
		CMDLINSIZ-len-2);		/* add user command 	     */ 
	    s_append("\"", shell_string);	/* Put within quotes	     */
	    }
  	status = system(shell_string);      /* issue the shell command   */
	if (status == 0177)			/* could not run shell	     */
	    tmmsg(PROCFAIL, "Could not invoke shell.", "TAE-RUNSHELL");
	else
	    {
	    if (status != 0)
	        tmmsg(PROCFAIL, "Abnormal shell termination.", 
			"TAE-ABNSHELL");
	    }
	IVAL(*sfi_gbl, 0) = (status == 0) ? 1 : -1;	/* set $skey 	     */ 
	sprintf(skey_val, "%o", status);		/* convert to string */
	value[0] = &skey_val[0];			/* skey value poiner */
	set_value(skey_gbl, value, 1);			/* set $skey	     */
	return(DO_CHECK);
	}

/*	host_init.   Host-dependent initialization.
 *
 *	For UNIX, we specify exit handlers (signal catchers for SIGTERM
 *	and SIGQUIT) for killing all active children when the parent
 *	monitor dies. Note that we don't catch  other signals such as 
 *	bus error because they are not  part of normal scenario.
 *
 */

    FUNCTION VOID host_init ()

    {

/* set up an exit handler					*/
#ifdef macII
    setcompat (COMPAT_BSDSIGNALS | COMPAT_EXEC | COMPAT_BSDTTY);
#endif
    signal(SIGTERM, term_sig);		/* specify exit handler		   */
    signal(SIGQUIT, term_sig);
    return;
    }


/*  The exit handler							    */

    FUNCTION VOID term_sig()

    {

    IMPORT TAEINT	sync_child;		/* pid of running child */

    if (sync_child > 0)				/* if child running	*/
	kill (sync_child, SIGKILL);		/* kill it		*/
 
#ifdef TAE_ASYNC
	term_async();				/* kill all async jobs	*/
#endif
	exit(-1);				/* died due to signal   */
     }


#ifdef TAE_ASYNC

    FUNCTION  VOID  term_async()
 
    {
    IMPORT struct ACB	*acb_head;		/* acb list-head	*/
    struct ACB		*acb;


/* kill async processes							    */
    for (acb = acb_head; acb != NULL; acb = (*acb).link)
	{
	if ((*acb).active)
	    {
	    abort_acb (acb);		/* abort the local process  */
	    tmmsg (SUCCESS, "Job '%s' aborted.", "TAE-ABORTOK",(*acb).name);
	    }
    	}
    return;				/* continue with next exit handler */
    }
#endif


#ifndef FAST_ALLOC
/*	dm_bytes_do.	Return dm_bytes as $SFI and display dm_bytes
 *
 *	If you are chasing down memory alloation problems,
 *	remember to do "let $lastcmd=--" to turn off
 *	command archive.
 *
 */
    FUNCTION dm_bytes_do ()
    {
    IMPORT struct VARIABLE *sfi_gbl;

    /* globalref int dm_bytes; */
    IMPORT  int dm_bytes;
    TEXT	string[STRINGSIZ+1];

    delete_ivp_pool();			/* clean up dm_bytes. This pool */
					/* get re-created automatically	*/
    IVAL (*sfi_gbl, 0) = dm_bytes;
    s_i2s (dm_bytes, string);
    put_stdout (string);
    return (DO_CHECK);
    }
#endif


/*	determine how many file descriptors are available
 */

    FUNCTION CODE fd_do()

    {
    IMPORT struct VARIABLE *sfi_gbl;
#define N 100
    int fd[N];
    int i, n;

    for (n=0, i=0; i < N; i++)			/* open as many as possible */
        {
	fd[i] = open ("/dev/null", O_WRONLY);
	if (fd[i] >= 0)  
	    n++;				/* tally if good open       */
	}
    for (i=0; i < N; i++)			/* close all that we opened */
	if (fd[i] >= 0)
	    close (fd[i]);    
    tmmsg (SUCCESS, "%d", "TAE-FDSAVAIL", n);
    IVAL (*sfi_gbl, 0) = n;			/* return n as $SFI         */
    return (SUCCESS);
    }
