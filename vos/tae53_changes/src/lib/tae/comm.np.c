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



/*  >> UNIX 4.2 BSD <<  */
/***************************************************************************/
/*                                                                         */
/*	Communications Interface Package for Catalog Manager               */
/*                                                                         */
/* This package provides interface routines for communication among        */
/* processes.  The following functions are provided:                       */
/*                                                                         */
/*		c_crepath  -  create a path                                */
/*		c_delpath  -  delete a path				   */
/*		c_conpath  -  connect to a path				   */
/*		c_discpath -  disconnect from a path			   */
/*		c_putmsg   -  transmit message over specified path	   */
/*		c_pmwcd    -  put message with connect/disconnect	   */
/*		c_getmsg   -  receive a message from the specified path	   */
/*		c_read/c_write						   */
/*		c_errtxt   -  generate an error message from errno	   */
/*	        traname    -  get value of environment variable		   */
/*                                                                         */
/***************************************************************************/
/*   CHANGE LOG:						     	   */
/*	26-Jul-1985  	Initial Release...njf				   */
/*	15-aug-1985	make getmsg smart enough to pick up split     	   */
/*			messages...nci					   */
/*	25-Jul-1986	Change m_ to c_; delete print statements;	   */
/*			delete use of host_stat;			   */
/*			fix "FUNINT *size" in c_getmsg...lia		   */
/*	12-Aug-1986	Save path.size in c_crepath()...lia		   */
/*	13-Aug-1986	Move maintenance of enable path indicator to the   */
/*			c_ functions...lia				   */
/*	21-Aug-1986	Append ".sk" to name of path for socket name...lia */
/*	28-Aug-1986	Changes for new path name element in PATH struct   */
/*			...lia						   */
/*	26-Sep-1986	Add call to shutdown() in deleting a path...lia	   */
/*	12-Dec-1986	Fix c_getmsg: "COUNT size" --> "COUNT *size"...peb */ 
/*	12-dec-1986	Make c_errtxt() VOID...peb		           */	
/*	05-jan-87	Add TBD notes; added close in c_delpath to avoid   */
/*			losing FDs; collapsed an if/then/else into ?:      */
/*			added DONTLINGER technology from SUN primer	   */
/*			....palm.					   */
/*	05-jan-87	New EINTR_LOOP macro to re-start interrupted       */
/*			system calls....palm				   */
/*	06-jan-87	Avoid system crash by accepting all outstanding    */
/*			connection requests in c_delpath...palm		   */
/*	21-jan-87	Uncondnally set PATH.name[0] to EOS in c_delpath   */
/*			...palm						   */
/*	18-may-87	Add c_accept, c_read, c_close...palm		   */
/*	29-jan-88	Conditionalize DONTLINGER for portability...ljn    */
/*	08-may-88	new c_write ...palm 				   */
/*	25-may-88	Do an unlink() before a bind()...ljn		   */
/*	25-oct-88	Added _NFILE #define trick from taskpro.np.c...ljn */
/*	10-nov-88	SYSV #include change...ljn			   */
/*	19-jan-90	Fixed send() and recv() termination checks in      */
/*			c_write(), c_read(), c_getmsg()...ljn		   */
/*	24-jan-90	changed all recv calls to read, because APOLLO BSD */
/*			recv does not appear to work properly...krw	   */
/*	07-may-90	Fixed "fix" of 19-may...ljn			   */
/*	07-may-90	Use latest UNIX technology in c_delpath()...ljn	   */
/*	06-aug-91	SO_LINGER more portable than SO_DONTLINGER...ljn   */
/*	21-aug-91	time.h on NEC...ljn				   */
/*	05-oct-92	IBM RS/6000 port...rt				   */
/*	06-oct-92	SGI needs to use the BSD time header file...bth/rt */
/*	11-dec-92	Solaris wants <sys/time.h>...dag		   */
/*      23-mar-94       PR2658: SCO Unix doesn't support UNIX domain sockets so */
/*                      add new functions c_gethostaddr, c_writeport, and  */
/*                      c_getport to support INET domain sockets; update   */
/*                      existing functions c_crepath and c_conpath for INET */
/*                      domain sockets...ws
 *  04-apr-94	SCO port...swd
 *  09-may-94	pr-2708: Use more general method to calculate socket 
 *		address length in c_crepath() and c_conpath()...swd
 *  20-may-94	pr-2736:  Disabled retries in c_conpath() because SCO hangs in
 *		calls to connect() after the first.  This may cause future
 *		problems.  See pr-2736...swd
 *  23-may-94	pr-2745: pr-2708 fix wasn't quite right.  Need to add one to
 *		address length to accommodate terminal null...swd
 *  16-jun-94	Intergraph Port: Use <sys/time.h> instead of <time.h>...bth/rt
 */
/*	TBD: this package is not FIFO and limits to 5 messages
 *	TBD: the name c_pmwcd is not so good
 *	TBD: why is size by ref in c_getmsg--it's not an output parm     
 *	TBD: need explanation of the scenario, motivation, etc.
 *	TBD: the way c_getmsg blocks (with the EINTR_LOOP), there's no
 *		way to abort a TCL RECVAR command.
 *	TBD: c_pmwcd is blocking?   It should not be.
 */
/***************************************************************************/

#include <stdio.h>

#ifdef macII
#include <sys/time.h>
#else
#ifdef SYSV
#if defined(mipseb) || defined(sgi) || (defined(sun) && OSMajorVersion >= 5) || defined(sco) || defined (__clipper__) || defined(__linux) || defined(linux)
#include <sys/time.h>
#else
#include <time.h>
#endif
#else
#include <sys/time.h>
#endif /* SYSV */
#endif /* macII */

#if defined(__linux) || defined(linux) || defined(__APPLE__) /* for strerror */
#include <string.h>
#endif
#include "taeintproto.h"
#include  "taeconf.inp"
#include  "comminc.inp"
#include <unistd.h>
struct linger lg = {0,0};

#ifdef AIX
#include <sys/select.h>
#endif

#ifdef INET_SOCKETS
#include <netinet/in.h>
#include <netdb.h>
#include <sys/param.h>
#include <sys/utsname.h>        /* for c_gethostaddr */
extern int h_errno;
#endif

FUNCTION CODE traname (
    TEXT lognam[],			/* In:  Environment variable name */
    TEXT name[],			/* Out: Value of lognam		  */
    BOOL *device);			/* Out: Not used in UNIX version  */


/*	This macro keeps executing system services while
	they are interrupted by signals:
*/
#define EINTR_LOOP(code, syscall)  \
	do {code=syscall;} while (code<0 && errno==EINTR)


/***********   usage notes *****************

The traditional use (which is flawed) is as follows:

	1. A server creates a globally-known path with c_crepath

	2. The server calls c_getmsg (blocking) to get the next message

	3. A client sends to a server with c_pwmcd

	4. If the server needs to send to the client, then the
	   client would have its own c_crepath and the server
	   would do c_pwmcd, i.e., the client behaves like a 
	   server and vice versa. 

	5. Both server and client call c_delpath when finished.


The traditional scheme is very awkward and not in the UNIX IPC
spirit.

The modified approach (18-may-87) is more in the IPC spirit, but
is not consistent with VMS.  More work is needed on portability.
Either the traidional or modified approach may be used with
this package.  The modified approach is:

	1. A server creates a globally-known path with c_crepath.

	2. The server waits for a connection with c_accept.
	   In this scheme, the server only serves one client,
	   which is not too exciting, but it gets us through
	   the DCF demo.

	3. The server receives messages via c_read.

	4. The server closes the connection with c_close and
	   deletes the path with c_delpath.

	5. The client calls c_conpath to attach to the server.
	   This connection corresponds to the server's c_accept.

	6. The client sends messages with c_putmsg. 

	7. The client calls c_discpath to disconnect from the
	   server.  TBD: make sure c_discpath does not delete
	   the path!
*************  END usage notes *****************/


#ifdef INET_SOCKETS
/**************************************************************************/
/*                                                                        */
/* c_gethostaddr - Get the local host's IP address and store it in        */
/*                 the socket                                             */
/*                                                                        */
/**************************************************************************/
FUNCTION  CODE  c_gethostaddr ( struct sockaddr_in *socket_info )
{
char   hostname[MAXHOSTNAMELEN]; /* from sys/param.h */
struct hostent *hostinfo;
int    status;
char  *nodename;
struct utsname sys_info;

  status = gethostname(hostname, MAXHOSTNAMELEN);
  if (status != 0)
  {
     perror("c_gethostaddr: Can't get local host's name via gethostname");
     return -1;
  }
      
  hostinfo = gethostbyname(hostname); 
  if (hostinfo==NULL)
  {  
    /* couldn't get the host's info using the hostname, so try getting the 
       info with the nodename retrieved from a different source */

    if (uname(&sys_info) < 0)
    {
#ifdef sco
      herror("c_gethostaddr: Can't get IP info for local host");
#else
      errno = h_errno;
      perror("c_gethostaddr: Can't get IP info for local host");
#endif
      return -1;
    }
    else
    { 
      hostinfo = gethostbyname(sys_info.nodename);
      if (hostinfo==NULL)
      {
#ifdef sco
        herror("c_gethostaddr: Can't get IP info for local host");
#else
        errno = h_errno;
        perror("c_gethostaddr: Can't get IP info for local host");
#endif
        return -1;
      }
    } /* end if strstr */
  } 
  bcopy((char *)hostinfo->h_addr, (char *)&socket_info->sin_addr, 
        hostinfo->h_length);

  return 1;

} /* end c_gethostaddr */
#endif /* INET_SOCKETS */

#ifdef INET_SOCKETS
/**************************************************************************/
/*                                                                        */
/*   c_writeport  -- write the given port number to a file                */
/*                   for the specified socket                             */
/*                                                                        */
/**************************************************************************/
FUNCTION  CODE  c_writeport 
(
 char  *socketname, /* in */
 short  portnum    /* in; assumed to be in network format */
)
{
char   sockfilename[108];  /* same size as sun_path in sockaddr_un struct */
FILE  *sockfile;

  /* create the file with the same name as the socket's name */
  GET_SOCKFILENAME(sockfilename,socketname);

  sockfile = fopen(sockfilename, "w");          /* create file for writing */
  if (sockfile==NULL)
  {
     perror("c_writeport: Can't open socket file");
     return -1;
  }

  /* convert # from net to host format & write out to the file */
  if (fprintf(sockfile, "%hd", ntohs(portnum)) <= 0)
  {  
     perror("c_writeport: Can't write to socket file");
     fclose(sockfile);
     return -1;
  }
  fclose(sockfile);
  return 1;

} /* end c_writeport */
#endif /* INET_SOCKETS */

#ifdef INET_SOCKETS
/**************************************************************************/
/*                                                                        */
/*   c_getport -- get the port number for the specified socket            */
/*                                                                        */
/**************************************************************************/
FUNCTION  CODE  c_getport
(
 char   *sockname,   /* in; socket filename */
 short  *portnum    /* out; port number in network format */
)
{
FILE  *sockfile;
char   sockfname[108]; /* same size as sun_path in sockaddr_un struct */
short  port;

    GET_SOCKFILENAME(sockfname, sockname);
    sockfile = fopen(sockfname, "r");
    if (sockfile==NULL)
    {
       perror("c_getport: Can't open socket file");
       return -1;
    }
	
    if (fscanf(sockfile, "%hd", &port) <= 0)
    {  
       perror("c_getport: Can't read port # from socket file");
       fclose(sockfile);
       return -1; /* unable to get the port number */
    }
    *portnum = htons(port);  /* convert from host to net format */
    fclose(sockfile);

    return 1;
}
#endif /* INET_SOCKETS */

/**************************************************************************/
/*									  */
/*   c_crepath  -- create a path					  */
/*									  */
/**************************************************************************/

FUNCTION  CODE  c_crepath 
(
 struct PATH *path,			/* Out: Path control block        */
 FUNINT path_size,			/* In: Path size                  */
 TEXT   path_name[],			/* In: Path name		  */
 FUNINT type			/* In: temporary (TMPMBX) or      */
					/*     permanent (PRMMBX)	  */
 )
    {

    CODE   retcode = SUCCESS;		/* Return status		  */
    FUNINT size;			/* Size of path address		  */
    CODE   status;			/* Local status code		  */
    FUNINT s;				/* Socket id 			  */

#ifdef INET_SOCKETS
    struct sockaddr_in loc_sock_info;   /* local socket info */

    bzero(&loc_sock_info, sizeof loc_sock_info);  /* initialize */
    loc_sock_info.sin_family  =  AF_INET;         /* Address family */
    (*path).type          =  SOCK_STREAM;         /* socket type  */

    s = socket (loc_sock_info.sin_family, (*path).type, 0); /* create socket */
    if (s < 0)
    {   c_errtxt (path, errno);
        return  FAIL;
    }

    setsockopt (s, SOL_SOCKET, SO_REUSEADDR, (GENPTR)0, 0);
    setsockopt (s, SOL_SOCKET, SO_LINGER, &lg, sizeof(lg));
    
    if (c_gethostaddr ( &loc_sock_info) < 0 )
        goto errReturn;
    
    loc_sock_info.sin_port = 0;  /* let the system pick a port # */
    size = sizeof (loc_sock_info);
    
    status  =  bind (s, (struct sockaddr *)&loc_sock_info, size);
    if (status < 0)
        goto errReturn;

    /* Name successfully bound to socket,         */ 
    /* get the port number assigned by the system */
    if (getsockname(s, &loc_sock_info, &size) != 0)
        goto errReturn;
    
    /* create file and write socket's port number for clients to use */
    if ( c_writeport(path_name, loc_sock_info.sin_port) <= 0 )
        goto errReturn;
                        
    s_bcopy (path_name, (*path).name, PNAMESIZ); /* path name */
    bcopy(loc_sock_info, (*path).sock_info, sizeof loc_sock_info);

#else
    TEXT   pname[PNAMESIZ];             /* Socket name                    */

    (*path).sock_info.sa_family  =  AF_UNIX;	 /* Address family	  */
    (*path).type		 =  SOCK_STREAM; /* socket type		  */

    s = socket ((*path).sock_info.sa_family, (*path).type, 0);
    if (s < 0)
        {
	c_errtxt (path, errno);
	return FAIL;
	}

    setsockopt (s, SOL_SOCKET, SO_REUSEADDR, (GENPTR)0, 0);
    setsockopt (s, SOL_SOCKET, SO_LINGER, &lg, sizeof(lg));
    
    s_bcopy (path_name, (*path).name, PNAMESIZ); /* path name	          */
    s_bcopy (path_name, pname, PNAMESIZ-4);	 /* build socket name	  */
    GET_SOCKFILENAME(pname,pname);

    s_bcopy (pname, (*path).sock_info.sa_data, PNAMESIZ-1);

    size = sizeof ( (*path).sock_info) - sizeof ( (*path).sock_info.sa_data )
	+ strlen ( (*path).sock_info.sa_data ) + 1;
    unlink ((*path).sock_info.sa_data);     /* clean up any debris   */

    /* socket created, bind name to it */
    status  =  bind (s, &((*path).sock_info), size);
    if (status < 0)
        goto errReturn;  /* Error binding name to socket           */

#endif

    /* Set accept queue size to 5 (unix max=5)*/
    status  =  listen (s, 5);
    if (status < 0)
        goto errReturn;

    (*path).chnl  =  s;
    (*path).size = path_size;
    (*path).del_on_disc = (type == TMPMBX);

    return (retcode);

 errReturn:
    c_errtxt (path, errno); /* do this first so that use the correct errno */
    close(s);
    (*path).name[0] = 0;
    return FAIL;

    }  /* end c_crepath */


/**************************************************************************/
/*									  */
/*  c_delpath -- Delete a path						  */
/*									  */
/**************************************************************************/


FUNCTION CODE c_delpath 
(
    struct PATH *path			/* In: Path Control Block
					   */
)

    {

    CODE  retcode  =  SUCCESS;		/* Return status		  */
    CODE  status;			/* Local status code		  */
    static struct timeval no_timeout = {0};
    struct sockaddr from;
    int snew;
    socklen_t fromlen;
    fd_set path_select;
#ifdef INET_SOCKETS
    char sockfilename[108];  /* same size as sun_path in sockaddr_un struct */
#endif


#ifndef _NFILE
#define _NFILE getdtablesize()
#endif

    if ((*path).name[0] != 0)
	{
        FD_ZERO (&path_select);		
	FD_SET ((*path).chnl, &path_select);	/* mask for select        */ 
        while (select (_NFILE, &path_select, 0, 0, &no_timeout))
	    {
	    fromlen = sizeof (from);
	    snew = accept ((*path).chnl, &from, &fromlen);
	    close (snew);		/* cleanup outstanding connects   */
            FD_ZERO (&path_select);	/* reset mask		  */	
	    FD_SET ((*path).chnl, &path_select);
	    }
        shutdown ((*path).chnl, 2);	/* Discard any data queued	  */
					/* Delete socket file		  */
#ifdef INET_SOCKETS
        GET_SOCKFILENAME(sockfilename,(*path).name);
        status  =  unlink (sockfilename);
#else
        status  =  unlink ((*path).sock_info.sa_data);
#endif
	(*path).name[0] = EOS;		/* set no longer active		  */
        close ((*path).chnl);
        if (status < 0)
	    {
	    c_errtxt (path, errno);
	    retcode = FAIL;
	    }
	}
    return (retcode);
    }



/**************************************************************************/
/* 									  */
/*  	c_conpath --  Connect to a path					  */
/*									  */
/**************************************************************************/

FUNCTION CODE c_conpath 
(
 struct PATH *path,			/* Out: Path Control Block	  */
 TEXT	path_name[]		/* In:  Path name		  */
)

    {
/* ----- Set retry limit.  SCO connect() seems to have a problem: hangs
 * -----    on retries, so we disable retries for SCO.  This may lead to
 * -----    future problems:  see pr-2736.
 */
#if defined(sco)
#define retry 0
#else
#define retry 200
#endif

#ifdef INET_SOCKETS

    IMPORT  int   errno;		/* Unix error number		  */

    CODE    retcode  = SUCCESS;		/* Return status		  */
    CODE    status;			/* Local status code		  */
    FUNINT 	s;			/* Socket ID			  */
    COUNT   size;			/* Path address size		  */
    TEXT    pname[PNAMESIZ];		/* Final path name (after traname)*/
    BOOL    unused;			/* Place holder (unix version)	  */
    COUNT   r;				/* retry index 		    	  */
struct sockaddr_in inet_sock;  

char sockfilename[108]; /* same size as sun_path in sockaddr_un struct */
FILE  *sockfile;
short portnum;

    (*path).del_on_disc = FALSE;		/* just close on c_disc   */

    bzero(&inet_sock, sizeof inet_sock);  /* initialize */
    inet_sock.sin_family  =  AF_INET;	        /* Address family        */
    (*path).type	  =  SOCK_STREAM;       /* Socket type           */

					/* Create socket	    	  */
    s = socket ( inet_sock.sin_family, (*path).type, 0 );
    if (s < 0)
	{				/* Socket not created		  */
	c_errtxt(path, errno);
	retcode = FAIL;
	}
    else
	{

        traname ( path_name, pname, &unused );	 /* Translate path name	  */
        s_bcopy ( pname, pname, PNAMESIZ-4);	 /* Truncate if necessary */

        if (c_getport(pname,&portnum) < 0)
        {
            c_errtxt (path, errno);
            return FAIL;
        }

	inet_sock.sin_port = portnum;

        if (c_gethostaddr(&inet_sock) < 0 )
        {
            c_errtxt (path, errno);
            return FAIL;
        }

	size = sizeof inet_sock;
	(*path).chnl = s;

	for (r=0; r <= retry; r++)
	    {
	    status = connect ( (*path).chnl, &(inet_sock), size );
	    if (status < 0)
		{			/* Not connected		  */
		if ((errno == ECONNREFUSED) && (r < retry))
		    continue;		/* Keep trying to connect	  */
		else
		    {			/* Connect failed, return error	  */
		    c_errtxt (path, errno);
		    retcode = FAIL;
		    break;
		    }
		}
	    else
		break;			/* Exit for-loop, successful 	  */
	    }
	}

    bcopy(inet_sock, (*path).sock_info, sizeof inet_sock);

    return (retcode);
#else
    CODE    retcode  = SUCCESS;		/* Return status		  */
    CODE    status;			/* Local status code		  */
    FUNINT 	s;			/* Socket ID			  */
    COUNT   size;			/* Path address size		  */
    TEXT    pname[PNAMESIZ];		/* Final path name (after traname)*/
    BOOL    unused;			/* Place holder (unix version)	  */
    COUNT   r;				/* retry index 		    	  */

    (*path).del_on_disc = FALSE;		/* just close on c_disc   */
    (*path).sock_info.sa_family  =  AF_UNIX;	 /* Address family 	  */
    (*path).type		 =  SOCK_STREAM; /* Socket type		  */
    traname ( path_name, pname, &unused );	 /* Translate path name	  */
    s_bcopy ( pname, pname, PNAMESIZ-4);	 /* Truncate if necessary */
    GET_SOCKFILENAME(pname,pname);

    s_bcopy ( pname, (*path).sock_info.sa_data, PNAMESIZ-1 );

					/* Create socket	    	  */
    s = socket ( (*path).sock_info.sa_family, (*path).type, 0 );
    if (s < 0)
	{				/* Socket not created		  */
	c_errtxt(path, errno);
	retcode = FAIL;
	}
    else
	{
	(*path).chnl = s;
        size = sizeof ( (*path).sock_info) - 
	    sizeof ( (*path).sock_info.sa_data ) +
	    strlen ( (*path).sock_info.sa_data ) + 1;
	for (r=0; r <= retry; r++)
	    {
	    status = connect ( (*path).chnl, &((*path).sock_info), size );
	    if (status < 0)
		{			/* Not connected		  */
		if ((errno == ECONNREFUSED) && (r < retry))
		    continue;		/* Keep trying to connect	  */
		else
		    {			/* Connect failed, return error	  */
		    c_errtxt (path, errno);
		    retcode = FAIL;
		    break;
		    }
		}
	    else
		break;			/* Exit for-loop, successful 	  */
	    }
	}
    return (retcode);
#endif
    }


/**************************************************************************/
/*									  */
/*	c_discpath --  Disconnect from a path				  */
/*									  */
/**************************************************************************/

FUNCTION CODE  c_discpath 
(
 struct PATH *path			/* In: Path Control Block	  */
)
    {

    CODE  retcode  =  SUCCESS;

    if ( (*path).del_on_disc )		/* Delete on disconnect?	  */
	retcode  =  c_delpath (path);
    else
	close ( (*path).chnl );
    return (retcode);
    }


/**************************************************************************/
/*									  */ 
/*	c_putmsg -- Transmit a message over the specified path		  */
/*									  */ 
/**************************************************************************/


FUNCTION  CODE  c_putmsg
(
 struct PATH *path,			/* In: Path Control Block	  */
 GENPTR	    buffer,		/* In: Message to be output	  */
 FUNINT	    size		/* In: Message size		  */
 )
    {
    CODE  retcode  =  SUCCESS;		/* Return status		  */
    CODE  status;			/* Local status code		  */


    EINTR_LOOP(status,send ( (*path).chnl, buffer, size, 0 ));
    if (status < 0)
	{
	c_errtxt (path, errno);
	retcode = FAIL;
	}
    return (retcode);
    }


/**************************************************************************/
/*									  */
/*  	c_pmwcd	 -- (Put message with connect/disconnect)		  */
/*		    Transmit a message over a path created by		  */
/*		    a process other than the calling process.		  */
/*									  */
/*			o Connect to the path. 				  */
/*		 	o Transmit the message.				  */
/*			o Disconnect the path connection.		  */
/*									  */
/**************************************************************************/

FUNCTION CODE  c_pmwcd 
(
 TEXT	path_name[],		/* In:  Path name		  */
 GENPTR	buffer,			/* In:  Message to be output	  */
 FUNINT	size,			/* In:  Message size		  */
 TEXT	errmsg[]		/* Out: Error message		  */
)
    {
    struct PATH path;			/* Path control block		  */
    CODE        code;			/* Local code			  */

    code = c_conpath (&path, path_name);
    if (code == SUCCESS)
	{
	path.del_on_disc = FALSE;	/* Do NOT delete upon disconnect  */
	code = c_putmsg (&path, buffer, size);
	if (code == SUCCESS)
	    {
	    code = c_discpath (&path);
	    if (code != SUCCESS) s_copy (path.errmsg, errmsg);
	    }
	else
	    {
	    s_copy(path.errmsg, errmsg);
	    c_discpath (&path);
	    }
	}
    else
	{
	s_copy (path.errmsg, errmsg);
	}
    return (code);
    }


/**************************************************************************/
/*									  */
/*  	c_getmsg -- Receive a message from the specified path.		  */
/*									  */
/**************************************************************************/

FUNCTION CODE c_getmsg
(
 struct PATH     *path,		/* In:  Path Control Block	  */
 GENPTR	    buffer,		/* Out: Buffer to store msg in    */
 COUNT	    *size		/* In:  Buffer size		  */
 )
    {
    GENPTR	bufptr;			/* extra buffer ptr to handle    */
					/*   split messages.		  */
    FUNINT  bufsiz;
    CODE   retcode = SUCCESS;		/* Return status		  */
    COUNT  len;				/* Length of msg read		  */
    struct sockaddr from;		/* Socket address of sender	  */
    socklen_t  fromlen;			/* Length of socket address	  */
    COUNT  snew;			/* Socket ID 			  */


    fromlen = sizeof (from);
    EINTR_LOOP(snew, accept ((*path).chnl, &from, &fromlen));
    if (snew < 0)
	{
	c_errtxt (path, errno);
	retcode = FAIL;
	}
    else
	{			/* Connection accepted, read message	*/
	bufptr = buffer;
	bufsiz = *size;
	do  
	    {  			/* until all of the message is in or error. */
#ifdef apollo
	    EINTR_LOOP(len, read (snew, bufptr, bufsiz));
#else
	    EINTR_LOOP(len, recv (snew, bufptr, bufsiz, 0));
#endif
            if (len <= 0) break;
	    bufptr += len;
	    bufsiz -= len;
	    }
	while (bufsiz > 0);
        close (snew);
	if (len < 0)
	    {
	    c_errtxt (path, errno);
	    retcode = FAIL;
	    }
	}
    return (retcode);
    }


/*
 *
 *
 *	c_accept.   Accept a connection.
 *
 *	The returned value is a FD that can be passed to
 *	c_read and c_close.  A negative return means
 *	error--see path.errmsg for the info.
 *
 */

FUNCTION COUNT c_accept 
(
 struct PATH	*path		/* in: path name	*/
 )
    {
    struct sockaddr from;		/* Socket address of sender	  */
    socklen_t  fromlen;			/* Length of socket address	  */
    COUNT  socketFd;

    fromlen = sizeof (from);
    EINTR_LOOP(socketFd, accept ((*path).chnl, &from, &fromlen));
    if (socketFd < 0)
	{
	c_errtxt (path, errno);
        return (-1);
	}
    return (socketFd);
    }
    

/*	c_read.   read next message, fixed length.
 *
 *	The function value is the message size or
 *	zero indicating a problem.
 *
 */

FUNCTION COUNT c_read
(
 struct PATH *path,	/* in: path structure		        */
 FUNINT  fd,		/* in: file desc from c_accept		*/
 GENPTR	buffer,		/* in: buffer to receive next message	*/
 FUNINT size		/* in: buffer size			*/
 )
    {
    GENPTR	bufptr;			/* extra buffer ptr to handle    */
    COUNT	bufsiz;
    COUNT	len;			/* Length of msg read		  */
    COUNT	totalBytes = 0;	

    bufptr = buffer;
    bufsiz = size;
    do  			/* until all of the message is in or error. */
	{  			
#ifdef apollo
	EINTR_LOOP(len, read (fd, bufptr, bufsiz));
#else
	EINTR_LOOP(len, recv (fd, bufptr, bufsiz, 0));
#endif
        if (len <= 0) break;
	bufptr += len;
	bufsiz -= len;
        totalBytes += len;
	}
    while (bufsiz > 0);

    if (len < 0)
	{
	c_errtxt (path, errno);
        return (0);
	}
    return (totalBytes);
    }



/*	c_write.   write message.
 *
 *
 */

FUNCTION COUNT c_write 
(
 struct PATH *path,	/* in: path structure		        */
 FUNINT  fd,		/* in: file desc from c_accept		*/
 GENPTR	buffer,		/* in: buffer to receive next message	*/
 FUNINT size		/* in: buffer size			*/
 )
    {
    GENPTR	bufptr;			/* extra buffer ptr to handle    */
    COUNT	bufsiz;
    COUNT	len;			/* Length of msg read		  */
    COUNT	totalBytes = 0;	

    bufptr = buffer;
    bufsiz = size;
    do  			/* until all of the message is in or error. */
	{  			
	EINTR_LOOP(len, send (fd, bufptr, bufsiz, 0));
        if (len <= 0) break;
	bufptr += len;
	bufsiz -= len;
        totalBytes += len;
	}
    while (bufsiz > 0);

    if (len < 0)
	{
	c_errtxt (path, errno);
        return (0);
	}
    return (totalBytes);
    }


/*	c_close.	Close the data FD.  TBD: a bad name?
 *	
 */

FUNCTION void c_close 
(
 struct PATH *path,		/* in: path struct 	*/
 FUNINT fd			/* in: FD from c_accept */	
 )
    {
    close (fd);
    }	
	
/**************************************************************************/
/*									  */
/* 	c_errtxt  --  Generate an error message based on errno		  */
/*									  */
/**************************************************************************/

FUNCTION VOID c_errtxt
( 
 struct PATH *path,			/* In/Out: Path for error string  */
 CODE	    err_code		/* In:	   errno value		  */
)
    {
#if defined(__linux) || defined(linux) || defined(__APPLE__) /* sys_nerr gone */
    s_bcopy(strerror(errno), (*path).errmsg, PMSGLEN);
#else

    IMPORT  TEXT  *sys_errlist[];	/* UNIX error messages		  */
    IMPORT  int   sys_nerr;		/* No. of messages in sys_errlist */
    IMPORT  int   errno;		/* Unix error number		  */

    if (err_code <= sys_nerr)
	{					/* Message is in table 		  */
	s_bcopy (sys_errlist[errno], (*path).errmsg, PMSGLEN);
	}
#endif
    return;
    }

/**************************************************************************/
/*									  */
/*  traname  -  get value of environment variable			  */
/*									  */
/**************************************************************************/

FUNCTION CODE traname 
(
 TEXT lognam[],			/* In:  Environment variable name */
 TEXT name[],			/* Out: Value of lognam		  */
 BOOL *device			/* Out: Not used in UNIX version  */
)
    {
    char *nmptr;

    *device = FALSE;
    if ( (nmptr = getenv(lognam)) == NULL)
	s_copy (lognam, name);
    else
	s_copy (nmptr, name);
    return (SUCCESS);
    }




