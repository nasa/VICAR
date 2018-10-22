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
#include "taeintproto.h"
#include "syninc.inc"
#include <string.h>
#include <unistd.h>
/*
 * The functions in this module are used by the subprocess to 
 * communicate with the parent task.
 *
 *
 *	CHANGE LOG:
 *
 *	23-jul-85	Add c_reqintp() for RCJM implementation...dm
 *	28-aug-85	Add handshake logic t c_snpa()...dm
 *	10-sep-85	Ignore operator interrupt in child (default)...dm
 *	10-dec-86	Add missing '#endif' after opint_sig()...peb
 *	19-jan-87	PR1036: c_close_all new function...palm
 *	21-jan-87	Check block size for write to pipe...lia
 *	03-feb-87	Make _NFILE reference portable...peb
 *	21-feb-88	Eliminate conditional compile on TAE_RCJM...ljn
 *	02-aug-88	Add new function c_child_proc() ...dm 
 *	12-sep-88	Apollo uses pipes for debugging app's under TM...ljn
 *	07-oct-88	Use sizeof(parblk) rather than P_BYTES on read...nhe
 *	21-oct-88	Fixed fix of 7-oct...ljn
 *	09-nov-88	Use of sockets for application debugging only seems
 *			necessary on the Sun; thus '#ifdef sun'...ljn
 *	22-nov-88	Added #ifdef SV_BSDSIG and sigvector() for hpux...ljn
 *	27-jan-88	Use void instead of VOID as required by SunOS 4.0..ljn
 *	27-jun-89	#ifdef out c_reqintp() and opint_sig() for systems
 *			without sigvec capability...ljn
 *	19-jan-89	opint_sig() is void for everyone...ljn
 *	23-may-90	Remove RCJM stuff by referring to old TAE_RCJM...ljn
 *	04-sep-92 PR871, PR444 Added multiple read/write for large par blk...tpl
 * 10-may-94	SCO port...swd
 *      26-jan-96       Made fixes in debugging for Solaris platforms
 */

#include <stdio.h> 		/* C standard codes			*/ 
#include <signal.h>		/*  signal processing			*/
#include <errno.h>		/* UNIX system error codes		*/
#include "taeconf.inp"		/* TAE standard, configuration defs	*/
#include "parblk.inc"		/* Parameter block definitions		*/
#include "taskinc.inp"		/* task related definitions		*/
#include "taeintproto.h"

#define WRITECHUNK min(blksize - already_wrote, PIPESIZ )



/*  Module-wide variables and definitions				*/
   
    long		c_code	= 0;	/* latest sys-service return	*/
    static	int	parent_pid = 0; /* parent process id		*/
    static	int	read_chan;	/* pipe number to read from	*/
    static	int	write_chan;	/* pipe number to write to	*/


/*
 *	c_subini	C_ package initialization for an initiated subtask
 *
 *	return codes:	SUCCESS or FAIL
 *
 *	c_subini initializes to receive messages from a parent task.
 *
 *	Note: The operator interrupt is ignored as the default.
 *	However, the child may specify its own handler later if it
 *	wishes to and is allowed to process the operator interrupt.
 *
 */

#include	<sys/types.h>
#include	<sys/socket.h>

FUNCTION CODE c_subini(void)

{
    COUNT  	i;
    int		argint[5];
    TEXT	*argptr;
    static TEXT *env_list[] = {"PARENT", "DOWN_PIPE0", "DOWN_PIPE1", 
			"UP_PIPE0", "UP_PIPE1"};
    int		status, dbxflag;
    int		sd;
    struct sockaddr	sockname;
    int		socknamelen;

    argptr = getenv( "DBXFLAG" );
    if( argptr != NULL && (dbxflag = atoi(argptr)) == 1 )
    {
    if( (argptr = getenv( "PARENT" )) == NULL ) return( FAIL );
    parent_pid = atoi( argptr );
    sockname.sa_family = AF_UNIX;
    if( (argptr = getenv( "SOCKNAME" )) == NULL ) return( FAIL );
    strcpy( sockname.sa_data, argptr );
    sd = socket( AF_UNIX, SOCK_STREAM, 0 );
    socknamelen = sizeof( sockname.sa_family ) + strlen( sockname.sa_data );
    status = connect( sd, &sockname, socknamelen );
    read_chan = write_chan = sd;
    }
    else
    {
    for (i=0; i < 5; i++)
	{
	argptr = getenv(env_list[i]);	/* get parent id & pipe #	   */ 
	if (argptr == NULL)		/* if it does not exist		   */
	    return (FAIL);
	argint[i] = atoi(argptr);	/* convert to integer	           */
	}
    parent_pid = argint[0];		
    read_chan = argint[1];		/* down_pipe: read end		   */
    write_chan = argint[4];		/* up_pipe: write end		   */

    close(argint[2]);			/* close unused ends		   */
    close(argint[3]);
    }
    signal(SIGINT, SIG_IGN);		/* don't die on oper interrupt	   */
    return (SUCCESS);
}


/*	c_close_all	Close all FDs.
 *	This closes all FDs except the big three (STDOUT, STDIN,
 *	and STDERR) and the two pipes needed for TM communication
 *	(read_chan and write_chan).   This is used to save
 *	FD entries. 
 *
 *	The following is a portable attempt at obtaining the size of the
 *	file descriptor table.  _NFILE is not necessarily correct but,
 *	on systems that do not have getdtablesize(), it is an approximation
 *	and will not generate a bug here if it is wrong.
 */
	FUNCTION VOID c_close_all (void)

	{
#ifndef	_NFILE
#define	_NFILE	getdtablesize()
#endif
	int	fd;

	if (parent_pid ==0)
	    c_subini();
	for (fd=3; fd < _NFILE; fd++)
	    if (fd != read_chan  &&  fd != write_chan)
		close (fd);
	return;
	}


/*
 *	c_rcvp		Receive a data block from a parent task.
 *
 *	return codes:
 *
 *		SUCCESS - Data successfully received from the parent.
 *		c_code(host error code) - Data could not be received
 *
 *	c_rcvp gets a data block from the parent task.  
 *	It does not return until a message is available from the down
 *	pipe, (i.e. the parent writes a message thru that pipe). 
 *	
 */

    FUNCTION CODE c_rcvp(

    GENPTR		block,		/* in: pointer to receive buffer   */
    FUNINT		blksize		/* in: number of characters 	   */
    					/*     dimensioned by 'block'	   */
    )
    {
    CODE		code;
    COUNT		count;
    struct LARGE_PARBLK	*parblkptr;
    COUNT		already_read;

    if (parent_pid == 0)		/* have we been this way before?    */
    	{
    	code = c_subini();		/* 1st time; get arguments to child */
    	if (code == FAIL)
	    {
	    if (errno != SUCCESS)	/* this happened on a mac... */
		return(errno);
	    else
	        return(errno+1);
	    }
    	}

    parblkptr = (struct LARGE_PARBLK *)block;
    already_read = 0;
/*
 *  blksize is the size of the receive buffer
 *  (*parblkptr).blksiz comes from the sender to indicate how big a
 *  block is transferred
 */
    while ( FOREVER )
	{
	count = read ( read_chan, &block[already_read], blksize-already_read );
 	if ( count <= 0 ) return (errno);
	already_read += count;
	if ( already_read > blksize )		    /* too big 		     */
	    {
	    return (FAIL);
	    }
	if ( already_read < (*parblkptr).blksiz )   /* check with input size */
	    {
	    count = kill ( parent_pid, SIGREAD);	/* sig tm to write */
	    }
	else
	    {
	    break;				        /* got them all	   */
	    }
	}
    if ( already_read != (*parblkptr).blksiz )
	{
	return (FAIL);
	}
    return ( SUCCESS );
    }


/*
 *	c_sndp		Send a data block to a parent task.
 *
 *	return codes:
 *
 *		SUCCESS - Data successfully sent to the parent.
 *		c_code(host error code) - Data could not be sent
 *
 *	c_sndp sends the specified data block to the parent task over
 *	the up-pipe. It waits until the parent task receives the block
 *	and sends an acknowledge message over the down-pipe. 
 *	
 *	If data is to be sent before any data (parameter block) has been
 *	received, a dummy read is performed to discard the waiting block
 *	so that acknowledgement message could be received from the
 *	parent over the down-pipe.
 */

    FUNCTION CODE c_sndp(

    GENPTR		block,		/* in: data block to send	    */
    FUNINT		blksize		/* in: number of characters	    */
    )
    {
    char		dummyblk[sizeof(struct LARGE_PARBLK)];
    CODE		code;
    COUNT		count;
    LONG		ackn_msg;	/* maximum 4 bytes of acknowledgement*/
    CODE 		already_read;
    CODE 		already_wrote;
struct PARBLK       *parblkptr;

    parblkptr = (struct PARBLK *)block;
    if (parent_pid == 0)                /* have we been this way before?    */
        {				/* we should have; if not dummy read*/
        code = c_subini();              /* 1st time; get arguments to child */
        if (code == FAIL)
	    {
	    if (errno != SUCCESS)	/* this happened on a mac... */
		return(errno);
	    else
                return(errno+1);
	    }

    	already_read = 0;
    	while ( FOREVER )
            {
            count = read( read_chan, &dummyblk[already_read], 
					sizeof(dummyblk)-already_read );
            if ( count <= 0 ) 
		return (errno);
            already_read += count;
            if ( already_read < sizeof(dummyblk) )
		{
            	count = kill ( parent_pid, SIGREAD);     /* sig tm to write */
		}
            else
		{
            	break;                                   /* got them all    */
		}
            }
        }

    already_wrote = 0;
    while ( already_wrote < blksize )
	{
	count = write ( write_chan, &block[already_wrote], WRITECHUNK);/*to TM*/
	already_wrote += count;
	count = kill ( parent_pid, SIGREAD );	      /* signal parent to read*/
	if ( count < 0 )
	    {
	    return (errno);
	    }
	}
        

    ackn_msg = 0;
    count = read(read_chan, &ackn_msg, ACKN_SIZE);	
    if (count != ACKN_SIZE || ackn_msg != ACKN_CODE)
    	{
	return (FAIL);
	}
    return (SUCCESS);
    }

#ifdef TAE_ASYNC 
/*
 *	c_snpa.	Send a data block to a parent task from an async monitor.
 *
 *	return codes:
 *
 *		SUCCESS - Data successfully sent to the parent.
 *		c_code(host error code) - Data could not be sent
 *
 *	c_snpa sends the specified data block to the parent task 
 *	over the up-pipe. It waits for the parent task to 
 *	receive (i.e. read) the message. (This is required for 
 *	thew parent agent which might send  more than one
 *	message asynchronousy.) 
 *
 *	Note:
 *	    1)	This design works under the  assumption that 
 *		the  parent monitor does not send any unsolicited
 *		asynchronous messages to the child through 
 *		the write channel.
 *	    2)  This design does not hangup the child too long because 
 *		the message is received and acknowledgement is sent by 
 *		the parent monitor  at the signal handler level.
 * 
 *	If data is to be sent before any data (parameter block) has been
 *	received, a dummy read is performed to discard the waiting block
 *	so that acknowledgement message could be received from the
 *	parent over the down-pipe.
 */

    FUNCTION CODE c_snpa(

    GENPTR		block,		/* in: data block to send	    */
    FUNINT		blksize		/* in: number of characters	    */
    )

    {
    char		dummyblk[sizeof(struct PARBLK)];
    CODE		code;
    COUNT		count;
    LONG		ackn_msg;	/* maximum 4 bytes of acknowledgement*/


    if (parent_pid == 0)		/* have we been this way before?     */
    	{				/* we should have; if not dummy read */
    	code = c_subini();		/* 1st time; get pipe numbers etc.   */
    	if (code == FAIL)
	    {
	    if (errno != SUCCESS)	/* this happened on a mac... */
		return(errno);
	    else
	        return(errno+1);
	    }
    	count = read(read_chan, dummyblk, sizeof(dummyblk));
	if (count < 0) 
	    return(errno);		/* read failure			    */
	}

    count = write(write_chan, block, blksize);	  /* send message to parent */
    if (count <= 0)
	    return(errno);		/* write failure		    */
#if defined(sco)
#define SIGIO SIGPOLL
#endif
    kill(parent_pid, SIGIO);		/* inform parent		    */
    ackn_msg = 0;
    count = read(read_chan, &ackn_msg, ACKN_SIZE);	
    if (count != ACKN_SIZE || ackn_msg != ACKN_CODE)
	return (FAIL);
    return (SUCCESS);
    }
#endif



/****
 *
 * c_child_proc.  Check if the process (task) is a child of TM.
 *
 *  Note that the envirnomental symbol
 *  "PARENT" is set by TM at task activation time.
 *
 ****/


     FUNCTION  BOOL  c_child_proc(void)

    {
    TEXT	*parent;
    
    parent = getenv("PARENT");
    if (parent != NULL &&  *parent != EOS)	/* TM is the parent	*/
	return (TRUE);
    else
	return (FALSE);
    }
