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



/* <<UNIX>> */
/*
 *	Host dependent utility functions for UNIX system.
 *
 *	
 *	CHANGE LOG:
 *	
 *	14-may-84	Add wait_hold()...dm
 *	19-may-84	Bring e_clear here...dm
 *	18-jun-85	Cleanup wait_hold, new utility routines for
 *			TAE-v1.3 implementation...dm
 *	15-aug-85	Add routine get_pid()...dm
 *	16-aug-85	Add routine e_set for RCJM use...dm
 *	10-sep-85	Update e_clear from dummy to real...dm	
 *	23-jun-88	Use s_table[] instead of UPPER...tp
 *	13-may-94	IRIX 5.2 port: undef get_pid before get_pid()...dag
 *  
 */

#include	<signal.h>
#include	"stdh.inp"
#include	"taeconf.inp"
#include	"eventinc.inp"
#include <time.h>
#include	<pwd.h>
#include "taeintproto.h"
#if defined(__linux) || defined(linux) || defined(__APPLE__) /* for strerror */
#include <string.h>
#endif
#include <sys/types.h>
#include <unistd.h>



/*
 *	e_set. Set event control flag
 */

FUNCTION  VOID  e_set
(
    struct  ECB	  *ecb		/* ptr to event control block	*/
)
    {
    (*ecb).flag = TRUE;		/* set the flag			*/
    return;
    }



/*	get_host_msg. Get the text corresponding to a host code.
 */

FUNCTION CODE get_host_msg
(
 FUNINT		msg_code,	/* in: host message code	    */
 TEXT		message[],	/* out: corresponding message	    */
 FUNINT		max_length	/* in: max chars caller can handle  */
)
    {
#if !(defined(linux) || defined(__linux) || defined(__APPLE__)) /* sys_nerr gone */
    IMPORT	int	sys_nerr;		/* highest UNIX error number  */
    IMPORT	char	*sys_errlist[];		/* system error messages      */
#endif
    IMPORT	char	s_table[];		/* lower to upper case table  */
    TEXT		tempbuf[257];		/* buffer to get message      */

    tempbuf[0] = EOS;
#if defined(linux) || defined(__linux) || defined(__APPLE__)
    s_copy(strerror(msg_code), tempbuf);
#else
    if ((0 < msg_code) && (msg_code <= sys_nerr))
						/* if error & message exist   */
	s_copy (sys_errlist[msg_code], tempbuf);
#endif
    tempbuf[0] = s_table[(int) tempbuf[0]];	/* make sure 1st character looks good */
    s_bcopy (tempbuf, message, max_length);
    return (SUCCESS);
    }



/*
 *	get_pid. Get process id.
 */

#ifdef irix5
/* get_pid is defined by IRIX 5.2 in /usr/include/task.h */
#undef get_pid
#endif

FUNCTION VOID  get_pid(long *pid)
{
  *pid = getpid();			/* call system routine	*/
  return;
}

  

/*
 */
FUNCTION  VOID get_time
(
    TEXT	atime[]			/* output: current asci time */
)
    {
    time_t	clock;				/* clock time in seconds     */
    TEXT	loc_time[27];			/* local buf for ascii time  */

    time(&clock);
    s_bcopy(ctime(&clock), loc_time, 26);	/* convert to ascii	     */
    s_bcopy(&loc_time[4], atime, 20);		/* copy to user		     */
    return;
    }


/*	get_user.    Get (login) name of the current user.
 *
 *	NOTE: We can not call getlogin because that will fail if invoked
 *	from a non-interactive process (not attached to a terminal).
 */


FUNCTION VOID get_user 
(
 TEXT 	name[USERNAMESIZ+1]		/* out: name of current user  */
 )
    {
    int 	uid;				/* user id of current process */
    struct 	passwd	*pw_ptr;
    TEXT	*name_ptr;			/* pointer to login name      */

    uid = getuid();				/* get current user id	      */
    pw_ptr = getpwuid(uid);			/* get ptr to pw file entry   */
    name_ptr = (*pw_ptr).pw_name;
    s_bcopy(name_ptr, name, USERNAMESIZ);	/* return login name to user  */
    return;
    }

/*
 *	getsession. get session id (same as process id) for tm.
 */


FUNCTION  VOID  getsession
(
    TEXT	session[]			/* output: session id	*/
 )
    {
    int		pid;				/* process id 		*/

    pid = getpid();				/* get process id	*/
    sprintf(session, "%d", pid);		/* convert to string	*/
    return;
    }


/*
 * 	getulib.  Get user library.
 */


FUNCTION  VOID  getulib
(
 TEXT	userlib[]		/* output: user library */
 )
    {
    TEXT	*ptr;

    userlib[0] = EOS;
    if ((ptr = getenv("ULIB")) != NULL)	 /* translate $ULIB	*/
        s_copy(ptr, userlib);		/* copy the string	*/
    return;	
    }



/*
 *	e_clear.  Clear an event control flag.	
 */

FUNCTION  VOID  e_clear
(
 struct  ECB	  *ecb		/* ptr to event control block	*/
)
    {
    (*ecb).flag = FALSE;	/* reset the flag		*/
    return;
    }


/*  inter_proc - Return TRUE if current process interactive, else FALSE
 *
 */
FUNCTION BOOL inter_proc (void)

    {
    char	*ptr;

    ptr = getenv("RUNTYPE");
    if (ptr == NULL)			/* not defined in environ	*/
	return (TRUE);			/* assume it is interactive	*/
    else if (s_equal(ptr, "INTERACTIVE"))	/* explicitely defined  */
 	return (TRUE);
    return (FALSE);			/* non-interactive otherwise    */
    }



/*
 *	wait_hold. Wait for a specified amount of time.
 *
 *	Note: If the specified time is < 1 second, it is changed
 *	to 1 second.
 */

FUNCTION  VOID  wait_hold
(
 FUNINT	msec		/* time in milli-seconds  	*/
)
    {
    unsigned	seconds;	/* time in seconds		*/

    seconds = msec/1000;	/* resolution of alarm clock	*/
    if (seconds < 1)
	seconds = 1;		/* must be positive		*/
    sleep(seconds);		/* wait for the interval	*/
    return;
    }
