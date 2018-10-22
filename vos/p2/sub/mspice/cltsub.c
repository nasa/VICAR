/*--------------------------------------------------------------------------
 * Apr.     1998  ...S.Le......   Initial release.
 *
 * Apr. 29, 1998  ...T.Huang...   Ported onto Alpha/VMS.
 *                                Cleaned the list of includes.
 *                                Modified to generate same results as the
 *                                 original SPICE routines.
 *                                Correct socket and file I/O calls.
 *                                Corrected illegal memory access problem.
 *                                Corrected C Kernel loading sequence.
 *                                Corrected to use source search when
 *                                 an invalid ck_id is passed.
 *
 * Jul. 24, 1998  ...T.Huang...   Removed the 'free' subroutine call in 
 *                                subroutine msclt_write_client_name.  This 
 *                                caused some programs on SGI to crash.
 *
 * Oct. 08, 1998  ...T.Huang...   SPICE_CONFIG_FILE was never closed.  Client 
 *                                will run out of file descriptors when 
 *                                performing lots of transactions.  'fclose'
 *                                calls have been added to correct this 
 *                                problem.
 *
 * Feb. 08, 1999  ...T.Huang...   Added check inside msclt_readn for the case
 *                                when the Socket Descriptor is valid, but the
 *                                client no longer exist.  This check will 
 *                                exit the while loop and return an error
 *                                status of -1.
 *
 * Aug.  5, 1999  ...M.Brady...   Cleaned up things which were causing
 *                                compiler errors:
 *                                --Removed trailing '1' parameter from
 *                                two fprintf calls in msclt_read_req_struct.
 *                                --Removed two extra %s symbols from
 *                                format string of last fprintf in
 *                                msclt_put_kernel. 
 *                                  
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>

#include "ms_defines.h"
#include "cltsub.h"
#include <rpc/types.h>
#include <rpc/xdr.h>


/*--------------------------------------------------------------------------
 * retrieve the config file name from SPICE_CONFIG_FILE environment
 * variable. Search for an entry key SPICE_LOG and copy the config
 * file name into the given parameter.
 *
 * Caller must make sure that enough memory is passed into <fname>
 *--------------------------------------------------------------------------*/
int msclt_getLogFileName (char *fname)
{
 FILE	*cfile;
 char	*cfname;
 char	dataline[256];
 char	key[256];
 char	value[256];

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s\n%s\n",
	"msclt_getLogFileName:ERROR!!!!",
	"SPICE_CONFIG_FILE is not defined");
    return -1;
    }
 
 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
	"msclt_getLogFileName:Cannot open Config file\n",
	"Sys mesg", strerror(errno),
	"Config file name", cfname);
    return -1;
    }

 while (fgets(dataline, 256, cfile)) {
     sscanf (dataline, "%s%s", key, fname);     
     if (!(strcmp(key, CONFIG_FILE_LOG_KEY))) {
	fclose (cfile);
	return 0;
	}
     }
 fprintf (stderr, "%s\n%s:%s\n",
	"msclt_getLogFileName:ERROR getting log file name",
	"Config file name", cfname);
 fclose (cfile);
 return -1;
}
/*--------------------------------------------------------------------------
 * retrieve the config file name from SPICE_CONFIG_FILE environment
 * variable. Search for an entry name SPICE_TCP_PORT and return
 * the port number to caller.
 *--------------------------------------------------------------------------*/
short msclt_getPortno ()
{
 short portno;
 FILE   *cfile;
 char   *cfname;
 char   dataline[256];
 char   key[256];
 char	value[256];

 const char *cm = "msclt_getPortno";

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s:%s\n",
        cm, "SPICE_CONFIG_FILE is not defined");
    return -1;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s:%s\n%s:%s\n%s:%s\n",
        cm, "Cannot open Config file\n",
        "Sys mesg", strerror(errno),
        "Config file name", cfname);
    return -1;
    }

  while (fgets(dataline, 256, cfile)) {
     sscanf (dataline, "%s%s", key, value);
     if (!(strcmp(key, CONFIG_FILE_PORTNO_KEY))) {
	portno = atoi (value);
        fclose (cfile);
	return portno;
	}
     }

 fprintf (stderr, "%s\n%s:%s\n",
        "msclt_getPortno:ERROR getting portno",
	"Config file name", cfname);
 fclose (cfile);
 return -1;
}
/*--------------------------------------------------------------------------*
 * retrive the spice log file name from SPICE_CONFIG_FILE. Then write the
 * message to end of file along with the current time.
 *--------------------------------------------------------------------------*/
int msclt_log (const char *sub, const char *mesg)
{
 FILE	*lfile;
 char	lfname[256];
 char	lcl_mesg[512];
 char	tstr[30];
 int	length;

 time_t	bintime;
 struct tm *curtime;

 if (msclt_getLogFileName(lfname)) {
    fprintf (stderr, "%s\n",
	"msclt_log:ERROR getting log filename");
    return -1;
    }

 time (&bintime);
 curtime = (struct tm*) localtime (&bintime);
 strcpy (tstr, (char*) asctime(curtime));
 tstr[strlen(tstr) - 1] = '\0';

 sprintf (lcl_mesg, "%s-->%s:%s\n", tstr, sub, mesg);
 fprintf (stderr, "%s", lcl_mesg);
 length = strlen (lcl_mesg);

 if (!(lfile = fopen(lfname, "a"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s",
        "msclt_log:ERROR opening file",
        "Sys mesg", strerror(errno),
	"File name", lfname);
    return -1;
    }

 if ( (write(fileno(lfile), lcl_mesg, length)) != length ) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
	"msclt_log:ERROR writing to log file",
	"Sys mesg", strerror(errno),
	"Log file", lfname);
    fclose (lfile);
    return -1;
    }
 else fclose (lfile);

 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_short (int sd, short *data)
{
 XDR	xdrs;
 char	buf[REQ_CODE_LEN];

 const char *cm = "msclt_read_short";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 if (msclt_readn(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:ERROR reading from socket\n", cm);
    fprintf (stderr, "%s:%s\n",
	"Sys mesg", strerror(errno));
    return -1;
    }

 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_DECODE);
 if (!xdr_short (&xdrs, data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR deconding socket data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_short (int sd, short data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_write_short";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_ENCODE);
 if (!xdr_short (&xdrs, &data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding user data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing data to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }
 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_u_short (int sd, u_short *data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_read_u_short";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 if (msclt_readn(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR reading from socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_DECODE);
 if (!xdr_u_short (&xdrs, data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR decoding server data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_u_short (int sd, u_short data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_write_u_short";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_ENCODE);
 if (!xdr_u_short (&xdrs, &data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding user data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing data to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }
 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_long (int sd, XDR_long *data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_read_long";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 if (msclt_readn(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR reading from socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_DECODE);
 if (!xdr_long (&xdrs, data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR deconding socket data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_long (int sd, XDR_long data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_write_long";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_ENCODE);
 if (!xdr_long (&xdrs, &data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding user data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing data to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }
 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_u_long (int sd, XDR_u_long *data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_read_u_long";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 if (msclt_readn(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR reading from socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_DECODE);
 if (!xdr_u_long (&xdrs, data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR deconding socket data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_u_long (int sd, XDR_u_long data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_write_u_long";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_ENCODE);
 if (!xdr_u_long (&xdrs, &data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding user data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing data to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*
 * read a string from <sd> and put it in <str>. The max length of <str>
 * is specified in <max_str_len>. Notice that this function will fill up
 * the length of the string from the socket. It's up to the caller to
 * null out the string if you want.
 *
 * For all string the protocol is:
 *		- XDR_u_long for string len
 *		- followed by a buffer of SVR_MESG_BUF_LEN (256Bytes).
 *		- you can only decode up to <string len> bytes of
 *			real data from socket.
 *
 * RETURN: the length of the string read off the socket.
 *	   -1 if ERROR occurred.
 *--------------------------------------------------------------------------*/
int msclt_read_string (int sd, char *str, int max_str_len)
{
 XDR	xdrs;
 char	buf[SVR_MESG_BUF_LEN];
 XDR_u_long	length;

 const char *cm = "msclt_read_string";

 if (msclt_read_u_long(sd, &length)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading string length from socket");
    return -1;
    }

 if (length >= max_str_len) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR:not enough mem passed in");
    return -1;
    }

 memset ((void*) buf, '\0', SVR_MESG_BUF_LEN);
 if (msclt_readn(sd, buf, SVR_MESG_BUF_LEN) != SVR_MESG_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading message off socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 xdrmem_create (&xdrs, buf, SVR_MESG_BUF_LEN, XDR_DECODE);
 if (!xdr_vector (&xdrs, str, length,
		sizeof(char), (xdrproc_t) xdr_char)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR decoding message from socket");
    fprintf (stderr, "%s:%s\n",
	"Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*
 * protocol for writing strings:
 * - write the string len
 * - write the actual string upto SVR_MESG_BUF_LEN
 *
 * NOTICE that these are any form of string you want. It does not interpret
 *	the string. That is, if a server want to send an informational
 *	message to the client, it has to first write a SVR_INFO_MESG
 *	code to the socket, so that the client will know to interpret
 *	the incomming string as an informational message.
 *
 *	Similarly, when reading the string, you have to first read the
 *		preceeding response code to know that it's an
 *		informational string. This is why I don't read/write the
 *		response codes in here.
 *--------------------------------------------------------------------------*/
int msclt_write_string (int sd, char *str, int max_str_len)
{
 XDR	xdrs;
 char	buf[SVR_MESG_BUF_LEN];
 XDR_u_long	length;

 const char *cm = "msclt_write_string";

 length = strlen (str);
 if (length >= SVR_MESG_BUF_LEN) {
    fprintf (stderr, "%s:%s\n",
	cm, "ERROR:string is too long");
    return -1;
    }

 if (msclt_write_u_long (sd, length)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to string to socket");
    return -1;
    }

 memset ((void*) buf, '\0', SVR_MESG_BUF_LEN);
 xdrmem_create (&xdrs, buf, SVR_MESG_BUF_LEN, XDR_ENCODE);
 if (!xdr_vector (&xdrs, str, length,
		sizeof(char), (xdrproc_t) xdr_char)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding message string");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, SVR_MESG_BUF_LEN) != SVR_MESG_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*
 * WARNING: This call will block the socket indefinitely until
 *		the socket is available for reading.
 *
 * check to see if the given socket is readable.
 * return 1: ready for read
 *	 -1: error
 *	  0: not ready for read
 *--------------------------------------------------------------------------*/
int msclt_is_readable (int sd)
{
 fd_set	fds;
 int	status;
 struct timeval *tm = (struct timeval*) NULL;

 const char *cm = "msclt_is_readable";

 FD_ZERO ((fd_set*) &fds);
 FD_SET  ((int) sd, &fds);
 status = select ( ((int) sd + 1), (fd_set*) &fds,
		(fd_set*) 0, (fd_set*) 0, tm);
 if ((status > 0) && (FD_ISSET((int)sd, &fds))) return 1;
 else if (status < 0) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR select() failed");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*
 * WARNING: This call will block the socket indefinitely until
 *		the socket is available for writing.
 *
 * check to see if the given socket is readable.
 * return 1: ready for write
 *	 -1: error
 *	  0: not ready for write
 *--------------------------------------------------------------------------*/
int msclt_is_writeable (int sd)
{
 fd_set fds;
 int    status;
 struct timeval *tm = (struct timeval*) NULL;

 const char *cm = "msclt_is_writeable";

 FD_ZERO ((fd_set*) &fds);
 FD_SET  ((int) sd, &fds);
 status = select ( ((int) sd + 1), (fd_set*) 0,
		(fd_set*) &fds, (fd_set*) 0, tm);
 if ((status > 0) && (FD_ISSET((int)sd, &fds))) return 1;
 else if (status < 0) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR select() failed");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_ck_struct (int sd, msCkStruct *ckdata)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];

 const char *cm = "msclt_write_ck_struct";

 memset ((void*)buf, '\0', SVR_DATA_BUF_LEN);
 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_ENCODE);
 if (!xdr_msCkStruct(&xdrs, ckdata)) {
    fprintf (stderr, "%s:%s\n%s:%s\n",
	cm, "ERROR encoding ck info",
	"Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing ckdata to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 /* txh::added */
 xdr_destroy (&xdrs);
 return 0; 
}
/*--------------------------------------------------------------------------*/
int msclt_write_spk_struct (int sd, msSpkStruct *spkdata)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];

 const char *cm = "msclt_write_spk_struct";
 
 memset ((void*)buf, '\0', SVR_DATA_BUF_LEN);
 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_ENCODE);
 if (!xdr_msSpkStruct(&xdrs, spkdata)) {
    fprintf (stderr, "%s:%s\n%s:%s\n",
	cm, "ERROR encoding spk info",
	"Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing spkdata to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 /* txh::added */
 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_req_struct (int sd, msUserRequestStruct *req)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];
 
 const char *cm = "mssvr_write_req_struct";
 
 memset ((void*) buf, '\0', SVR_DATA_BUF_LEN);
 if (   msclt_is_readable(sd) &&
        msclt_readn(sd, buf, SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm,
        "ERROR reading client request from socket");
    return -1;
    }
 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_DECODE);
 if (!xdr_msUserRequestStruct(&xdrs, req)) {
    fprintf (stderr, "%s:%s\n", cm,
        "ERROR decoding client request from socket");

    /* txh::added */
    xdr_destroy (&xdrs);
    return -1;
    }

 /* txh::added */
 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_req_struct (int sd, msUserRequestStruct *req)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];

 const char *cm = "msclt_write_req_struct";

 memset ((void*) buf, '\0', SVR_DATA_BUF_LEN);
 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_ENCODE);
 if (!xdr_msUserRequestStruct(&xdrs, req)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding user request");
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf,  SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "Cannot send user request to socket");
    fprintf (stderr, "%s:%s\n", cm, "... Returning with no data ....");
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*
 * reading response code from server.
 * return 0 ==> ERROR.
 *--------------------------------------------------------------------------*/
XDR_u_long msclt_read_resp_code (int sd)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];
 XDR_u_long resp = 0;

 const char *cm = "msclt_read_resp_code";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 if (msclt_readn(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading response code from server");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return 0;
    }

 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_DECODE);
 if (!xdr_u_long (&xdrs, &resp)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR decoding server response");
    fprintf (stderr, "%s:%s\n", cm, "... Returning with no data ...");
    xdr_destroy (&xdrs);
    return 0;
    }

 /* txh::added */
 xdr_destroy (&xdrs);
 return resp;
}
/*--------------------------------------------------------------------------*/
int msclt_read_ck_struct (int sd, msCkStruct *ckdata)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];

 const char *cm = "msclt_read_ck_struct";

 memset ((void*) buf, '\0', SVR_DATA_BUF_LEN);
 if (msclt_readn(sd, buf, SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n%s:%s\n", cm,
	"ERROR reading ckdata from server",
	"Sys mesg", strerror(errno));
    fprintf (stderr, "%s:%s\n", cm,
	"....Returning with no data....");
    return -1;
    }

 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_DECODE);
 if (!xdr_msCkStruct(&xdrs, ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR decoding ckdata from server");
    fprintf (stderr, "%s:%s\n", cm,
	"....Returning with no data....");
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_spk_struct (int sd, msSpkStruct *spkdata)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];

 const char *cm = "msclt_read_spk_struct";

 memset ((void*) buf, '\0', SVR_DATA_BUF_LEN);
 if (msclt_readn (sd, buf, SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading spkdata from server");
    fprintf (stderr, "%s:%s\n", cm,
	"....Returning with no data....");
    return -1;
    }

 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_DECODE);
 if (!xdr_msSpkStruct(&xdrs, spkdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR decoding spkdata from server");
    fprintf (stderr, "%s:%s\n", cm,
	"....Returning with no data....");
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_connectToSvr (int *sd)
{
 char			*head,
			*tail;
 char			hname[1024],
			err_mesg[1024];
 short			portno;
 struct hostent		*hp;
 struct sockaddr_in	host_addr;
 int                    On=1;

 const char *cm = "msclt_connectToSvr";

 if ((portno = msclt_getPortno()) == (-1) ) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR getting port no");
    fprintf (stderr, "%s:%s\n", cm, "Cannot connect to spice server");
    return -1;
    }

 memset ((void*)hname, '\0', 1024);
 if (msclt_getSvrHostNames(hname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR getting server host names");
    fprintf (stderr, "%s:%s\n", cm,
	"Cannot connect to spice server");
    return -1;
    } 

 head = strtok (hname, ":");
 while (head) {
    if (!(hp = (struct hostent*) gethostbyname (head))) {
       fprintf (stderr, "%s:%s\n", cm, "ERROR getting host info");
       fprintf (stderr, "%s:%s\n", "Host name: ", head);
       continue;
       }

    memset ((void*) &host_addr, '\0', sizeof(host_addr));
    host_addr.sin_family	= AF_INET;
    host_addr.sin_port		= htons(portno);
    host_addr.sin_addr.s_addr	=
		((struct in_addr *)(hp->h_addr))->s_addr;

    if ((*sd = socket(AF_INET, SOCK_STREAM, 0)) == (-1)) {
       fprintf (stderr, "%s:%s\n", cm,
		"ERROR initializing connection socket");
       fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
       continue;
       }

    if (setsockopt(*sd, SOL_SOCKET, SO_KEEPALIVE, (char*)&On, sizeof(On)) < 0)
    {
       fprintf (stderr, "%s:%s\n", "Warning: Could not set keepalive option.",
                strerror(errno));
       continue;
    }

    fprintf (stdout, "Connecting: msserver@%s.....", head);
    if (connect(*sd, (struct sockaddr*) &host_addr,
			sizeof(host_addr)) < 0) {
       fprintf (stdout, "FAILED\n");
       head = strtok(NULL, ":");
       close (*sd);
       continue;
       }

    fprintf (stdout, "SUCCESS\n");
    if (msclt_write_client_name(sd)) {
       fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing client's name to server");
       fprintf (stderr, "%s:%s\n",
	"Sys mesg", strerror(errno));
       close (*sd);
       return -1;
       }

    return 0;
    }

 fprintf (stderr, "%s:%s\n", cm,
	"Cannot find an active spice server");
 return -1;
}
/*--------------------------------------------------------------------------*/
/* txh::modified to use 'cuserid' function to eliminate pwd.h and unistd.h  */
/*      function calls.                                                     */

int msclt_write_client_name (int *sd)
{
 char *clt_ptr, *cuserid_p2();

 const char *cm = "msclt_write_client_name";

 clt_ptr = cuserid_p2 ();

 if (msclt_write_string(*sd, clt_ptr, strlen(clt_ptr))) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing client name to socket");
    return -1;
    }

 return 0;
}
/*--------------------------------------------------------------------------*
 * read the config file and return the svr hostnames separated by * ":" 
 *--------------------------------------------------------------------------*/
int msclt_getSvrHostNames (char *hname)
{
 FILE	*cfile;
 char	*cfname;
 char	dataline[256];
 char	key[256];
 char	value[256];

 const char *cm = "msclt_getSvrHostName";

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s:%s\n",
	cm, "SPICE_CONFIG_FILE not define");
    return -1;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s:%s\n%s:%s\n%s: %s\n",
	cm, "ERROR openning config file",
	"Sys mesg", strerror(errno),
	"File name", cfname);
    return -1;
    }

 while (fgets(dataline, 256, cfile)) {
     sscanf (dataline, "%s%s", key, value);
     if (!strcmp (key, CONFIG_FILE_SVR_HOST_KEY)) {
	strcat (hname, value);
	strcat (hname, ":");
	}
     } 

 fclose (cfile);
 return 0;
}
/*--------------------------------------------------------------------------*
 * - initialize socket
 * - send request code to server
 * - send msUserRequest struct to server
 * - listen for return data.
 * 	- server informational message
 *	- server error message
 *	- ckdata struct
 *	- spkdata struct
 *	- server completion code
 *
 * RETURN: 0 on SUCCESS
 *	  -1 on FAILURE
 *
 * code		: input
 * req		: input
 * ckdata	: output
 * spkdata	: output
 *
 *--------------------------------------------------------------------------*/
int msclt_getspice (XDR_u_long req_code, msUserRequestStruct *req,
		msCkStruct *ckdata, msSpkStruct *spkdata)
{
 int	sd;
 XDR_u_long resp_code;
 char	svr_mesg[SVR_MESG_BUF_LEN];

 const char *cm = "msclt_getspice";

 if (msclt_connectToSvr(&sd)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR connecting to server");
    fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
    return -1;
    }

 if (msclt_write_u_long (sd, req_code)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing request code to server");
    fprintf (stderr, "%s:%s\n", cm,
	"Returning with no data");
    socket_close (sd);
    return -1;
    }
 else if (msclt_write_req_struct(sd, req)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing request info to server");
    fprintf (stderr, "%s:%s\n", cm,
	"Returning with no data");
    socket_close (sd);
    return -1;
    }

 while (msclt_is_readable(sd)) {
    if (!(resp_code = msclt_read_resp_code(sd))) { 
       fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading response code from server");
       fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
       socket_close (sd);
       return -1;
       }

    switch (resp_code) {
	case SVR_INFO_MESG:
	   memset ((void*) svr_mesg, '\0', SVR_MESG_BUF_LEN);
	   if (	msclt_is_readable(sd) &&
		msclt_read_string(sd, svr_mesg, SVR_MESG_BUF_LEN)) {
	      fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading server's info mesg");
	      fprintf (stderr, "%s:%s\n", cm,
		"Returning with no data");
	      socket_close (sd);
	      return -1;
	      }     
	   else fprintf (stdout, "%s\n", svr_mesg);
	   break;

	case SVR_ERR_MESG:
	   memset ((void*) svr_mesg, '\0', SVR_MESG_BUF_LEN);
	   if (	msclt_is_readable(sd) &&
		msclt_read_string(sd, svr_mesg, SVR_MESG_BUF_LEN)) {
	      fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading err mesg from server");
	      fprintf (stderr, "%s:%s\n", cm,
		"Terminating server connection");
	      }
	   else fprintf (stderr, "%s\n", svr_mesg);
	   socket_close (sd);
	   return -1;

	case SVR_CK_DATA:
	   if (	msclt_is_readable(sd) &&
		msclt_read_ck_struct(sd, ckdata)) {
	      fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading server ckdata");
	      fprintf (stderr, "%s:%s\n", cm,
		"Terminating server connection");
	      socket_close (sd);
	      return -1;
	      }
	   break;

	case SVR_SPK_DATA:
	   if (	msclt_is_readable(sd) &&
		msclt_read_spk_struct(sd, spkdata)) {
	      fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading SPK data from server");
	      fprintf (stderr, "%s:%s\n", cm,
		"Terminating server connection");
	      socket_close (sd);
	      return -1;
	      }
	   break;
	
	case OPS_COMPLETED:
	   fprintf (stderr, "%s:%s\n", cm,
		"Done reading data from server");
	   fprintf (stderr, "%s:%s\n", cm,
		"Request completed SUCCESSFULLY");
	   socket_close (sd);
	   return 0;

	default:
	   sprintf (svr_mesg, "%s\n%s: %d",
		"Unknown response code from server",
		"Response code", resp_code);
	   fprintf (stderr, "%s:%s\n", cm, svr_mesg);
	   socket_close (sd);
	   return -1;
        }
    }

 fprintf (stderr, "%s:%s\n", cm, "ERROR polling client socket");
 fprintf (stderr, "%s:%s\n", cm, "Returning with no data from server");

 return -1;
}
/*--------------------------------------------------------------------------*/
int msclt_gllgetspice (msUserRequestStruct *req,
	msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice(GLL_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"msclt_gllgetspice",
	"ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casgetspice (msUserRequestStruct *req,
	msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice(CAS_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"msclt_casgetspice",
	"ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice(SIM_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_simgetspice",
        "ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice(VGR1_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vgr1getspice",
        "ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice(VGR2_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vgr2getspice",
        "ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1getspice (msUserRequestStruct *req,
	msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice (VO1_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"msclt_vo1getspice", "ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2getspice (msUserRequestStruct *req,
	msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice (VO2_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vo2getspice", "ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*
 * - initialize socket
 * - send request code to server
 * - send msCkStruct to server
 * - listen for return data
 *	- server informational message
 *	- server error message
 *	- server completion code
 *
 * RETURN: 0 on SUCCESS
 *	  -1 on FAILURE
 *
 * req_code	: input
 * ckdata	: input
 *
 *--------------------------------------------------------------------------*/
int msclt_putspice (XDR_u_long req_code, msCkStruct *ckdata)
{
 int	sd;
 XDR_u_long	resp_code;
 char	svr_mesg[SVR_MESG_BUF_LEN];

 const char *cm = "msclt_putspice";

 if (msclt_connectToSvr(&sd)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR connecting to server");
    fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
    return -1;
    }
 
 if (msclt_write_u_long (sd, req_code)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing request code to server");
    fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
    socket_close (sd);
    return -1;
    }
 else if (msclt_write_ck_struct(sd, ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing ckdata to server");
    socket_close (sd);
    return -1;
    }

  while (msclt_is_readable(sd)) {
    if (!(resp_code = msclt_read_resp_code(sd))) {
       fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading response code from server");
       fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
       socket_close (sd);
       return -1;
       }
 
    switch (resp_code) {
        case SVR_INFO_MESG:
           memset ((void*) svr_mesg, '\0', SVR_MESG_BUF_LEN);
           if (msclt_read_string(sd, svr_mesg, SVR_MESG_BUF_LEN)) {
              fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading server's info mesg");
              fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
              socket_close (sd);
              return -1;
              }
           else fprintf (stdout, "%s\n", svr_mesg);
           break;
 
        case SVR_ERR_MESG:
           memset ((void*) svr_mesg, '\0', SVR_MESG_BUF_LEN);
           if (msclt_read_string(sd, svr_mesg, SVR_MESG_BUF_LEN)) {
              fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading err mesg from server");
              fprintf (stderr, "%s:%s\n", cm,
		"Terminating server connection");
              }
           else fprintf (stderr, "%s\n", svr_mesg);
           socket_close (sd);
           return -1;
 
        case OPS_COMPLETED:
           fprintf (stderr, "%s:%s\n", cm, "Done reading data from server");
           fprintf (stderr, "%s:%s\n", cm, "Request completed SUCCESSFULLY");
           socket_close (sd);
           return 0;
 
        default:
           sprintf (svr_mesg, "%s\n%s: %d",
                "Unknown response code from server",
                "Response code", resp_code);
           fprintf (stderr, "%s:%s\n", cm, svr_mesg);
           socket_close (sd);
           return -1;
        }
    }

 fprintf (stderr, "%s:%s\n", cm, "ERROR polling client socket");
 fprintf (stderr, "%s:%s\n", cm, "Returning with no data");

 return -1;
}
/*--------------------------------------------------------------------------*/
int msclt_gllputspice (msCkStruct *ckdata)
{
 if (msclt_putspice(GLL_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
	"msclt_gllputspice",
	"ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casputspice (msCkStruct *ckdata)
{
 if (msclt_putspice(CAS_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_casputspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simputspice (msCkStruct *ckdata)
{
 if (msclt_putspice(SIM_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_simputspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1putspice (msCkStruct *ckdata)
{
 if (msclt_putspice(VGR1_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vgr1putspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2putspice (msCkStruct *ckdata)
{
 if (msclt_putspice(VGR2_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vgr2putspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1putspice (msCkStruct *ckdata)
{
 if (msclt_putspice(VO1_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vo1putspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2putspice (msCkStruct *ckdata)
{
 if (msclt_putspice(VO2_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vo2putspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*
 * both bname & tname are input. Caller must select a temp text file before
 * calling this.
 *--------------------------------------------------------------------------*/
int msclt_bin2text_file (char *bname, char *tname)
{
 struct stat buf;
 const char *cm = "msclt_bin2text_file";
 if (stat(bname, &buf) == (-1)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR getting binary file info");
    fprintf (stderr, "%s:%s\n", "Bin file", bname);
    fprintf (stderr, "%s:%s\n", "sys mesg", strerror(errno));
    return -1;
    }

 if (stat(tname, &buf) != (-1)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR:temp text file already exist");
    fprintf (stderr, "%s:%s", "Temp file", tname);
    return -1;
    }

 zms_spcb2a (bname, tname);
 if (zms_failed()) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR converting binary to text file");
    return -1;
    }

 return 0; 
}
/*--------------------------------------------------------------------------*/
int msclt_text2bin_file (char *tname, char *bname)
{
 struct stat buf;
 const char *cm = "msclt_text2bin_file";
 if (stat(tname, &buf) == (-1)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR:input text file does not exist");
    return -1;
    }
 
 if (stat(bname, &buf) != (-1)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR:output bin file already exist");
    return -1;
    }

 zms_spca2b (tname, bname);
 if (zms_failed()) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR converting text to binary file");
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*
 * Caller should already have sent the request code and the resulting
 *	file name before calling this. Caller is responsible for removing
 *	the temp text file when done!!!!
 *
 * 1/ send file size to server.
 * 2/ start sending text file content (1K at a time)
 * 3/ ask for ACK every 3pkts
 * 4/ ask for OPS_COMPLETED ack when done
 *--------------------------------------------------------------------------*/
int msclt_send_text_file (int sd, char *tname)
{
 XDR_u_long ack,
	nleft,
        filesize;
 int	pkg_count = 0;
 char   databuf[1024];
 struct stat statbuf;

 const char *cm = "msclt_send_text_file";

 FILE *infile;

 if ((stat(tname, &statbuf)) == (-1)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR getting file info");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }
 else filesize = (XDR_u_long) statbuf.st_size;

 if (msclt_write_u_long (sd, filesize)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing file size to socket");
    return -1;
    }

 if ((infile = fopen(tname, "r")) == (FILE*) NULL) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR openning text file");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }
 while (filesize) {
    nleft = (filesize < 1024) ? filesize : 1024;
    if (fread((void*) databuf, 1, nleft, infile) != nleft) {
       fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading text data from file");
       fprintf (stderr, "%s:%s\n", "sys mesg", strerror(errno));
       fclose (infile);
       return -1;
       }
    if (msclt_writen(sd, databuf, nleft) != nleft) {
       fprintf (stderr, "%s:%s\n", cm,
		"ERROR writing text data to socket");
       fprintf (stderr, "%s:%s\n", "sys mesg", strerror(errno));
       fclose (infile);
       return -1;
       }

    if (pkg_count == 3) {
       if (	msclt_is_readable(sd) &&
		msclt_read_u_long (sd, &ack)) {
	  fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading ACK from socket");
	  fclose (infile);
	  return -1;
	  }
       else pkg_count = 0;

       if (ack != SVR_PKG_ACK) {
	  fprintf (stderr, "%s:%s\n", cm,
		"Un-expected positive ACK from socket");
	  fclose (infile);
	  return -1;
	  }

       fprintf (stderr, "Bytes left to send....%d\n", filesize);

       }
    else pkg_count++;

    filesize -= nleft;
    }

 fclose (infile);
 return 0;
}
/*--------------------------------------------------------------------------*
 * Caller should already have sent the appropriate request code and
 *	the resulting file name before calling this. Caller is responsible
 *	for deleting any temp text file when done!!!!
 *
 * NOTICE that the file name that you are passing should be an empty
 *	temp file. Anything in it will be over-written.
 *--------------------------------------------------------------------------*/
int msclt_receive_text_file (int sd, char *tname)
{
 XDR_u_long nleft,
	filesize,
	ack = SVR_PKG_ACK;
 int	pkg_count = 0;
 char	databuf[1024];
 struct	stat statbuf;

 FILE	*infile;

 const char *cm = "msclt_receive_text_file";

 if (!(infile = fopen(tname, "w"))) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR openning text file");
    fprintf (stderr, "%s:%s\n", "sys mesg", tname);
    return -1; 
    }

 if (msclt_read_u_long(sd, &filesize)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading file size off socket");
    fclose (infile);
    return -1;
    }

 while (filesize) {
    nleft = (filesize < 1024) ? filesize : 1024;
    if (msclt_is_readable(sd) &&
	msclt_readn(sd, databuf, nleft) != nleft) {
       fprintf (stderr, "%s:%s\n", cm, "ERROR reading data from socket");
       fprintf (stderr, "%s:%s\n", "sys mesg", strerror(errno));
       fclose (infile);
       return -1;	
       }
    if (fwrite((void*) databuf, 1, nleft, infile) != nleft) {
       fprintf (stderr, "%s:%s\n", cm, "ERROR writing data to file");
       fprintf (stderr, "%s:%s\n", "sys mesg", strerror(errno));
       fclose (infile);
       return -1;
       }
    if (pkg_count == 3) {
       if (msclt_write_u_long(sd, ack)) {
	  fprintf (stderr, "%s:%s\n", cm, "ERROR writing ACK to sender");
	  fclose (infile);
	  return -1;
	  }
       else pkg_count = 0;
       fprintf (stderr, "Bytes left to receive....%d\n", filesize);
       }
    else pkg_count++;

    filesize -= nleft;
    }

 fclose (infile);
 return 0;
}
/*--------------------------------------------------------------------------*
 * the request code and the actual kernel name should have been sent
 * before calling this.
 *--------------------------------------------------------------------------*/
int msclt_send_bin_kernel (int sd, char *kname)
{
 char	tname[256];
 const char *cm = "msclt_send_bin_kernel";

 memset ((void*) tname, '\0', 256);
 tmpnam (tname);

 if (msclt_bin2text_file(kname, tname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR creating temp text file");
    remove (tname);
    return -1;
    }
 if (msclt_write_u_long (sd, OPS_COMPLETED)) {

    }

 if (msclt_send_text_file (sd, tname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR sending text file to socket");
    remove (tname);
    return -1;
    }
 remove (tname);
 return 0;
}
/*--------------------------------------------------------------------------*
 * - get a temp file name.
 * - call "msclt_receive_text_file" to put the data in temp file
 * - when done, convert the text file to bin kernel.
 * - remove temp file.
 *--------------------------------------------------------------------------*/
int msclt_receive_bin_kernel (int sd, char *kname)
{
 char 	tname[256];
 const char *cm = "msclt_receive_bin_kernel";

 memset ((void*) tname, '\0', 256);
 tmpnam (tname);

 if (msclt_receive_text_file (sd, tname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR receiving text file from socket");
    remove (tname);
    return -1;
    }

 if (msclt_text2bin_file (tname, kname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR converting text file to binary kernel");
    remove (tname);
    return -1;
    }

 remove (tname);
 return 0;
}
/*--------------------------------------------------------------------------*
 * - initialize connection to server
 * - send request code to server
 * - send requested kernel name to server.
 * - listen for server's initial response.
 * - call function to receive bin file from server.
 *--------------------------------------------------------------------------*/
int msclt_receive_kernel (XDR_u_long req_code, char *kname, char *local_dir)
{
 int sd;
 XDR_u_long resp_code;
 char	new_kname[SVR_MESG_BUF_LEN];

 const char *cm = "msclt_get_kernel";

 if (msclt_connectToSvr (&sd)) {			/* open connection */
    fprintf (stderr, "%s:%s\n", cm,			/* to server.	   */
	"ERROR connecting to spice server\n");
    return -1;
    }
 if (msclt_write_u_long (sd, req_code)) {		/* send request    */
    fprintf (stderr, "%s:%s\n", cm,			/* code to server. */
	"ERROR writing request code to server");
    close (sd);
    return -1;
    }
 if (msclt_write_string (sd, kname, strlen(kname))) {	/* send requested  */
    fprintf (stderr, "%s:%s\n", cm,			/* file name to    */
	"ERROR writing kernel name to socket");		/* server.	   */
    close (sd);
    return -1;
    }

 if (!(resp_code = msclt_read_resp_code(sd))) {			/* read	   */
    fprintf (stderr, "%s:%s\n", cm,				/* resp	   */
        	"ERROR reading response code from server");	/* code	   */
       fprintf (stderr, "%s:%s\n", cm,				/* from	   */
		"Returning with no data");			/* server. */
       close (sd);
       return -1;
       }

 if (resp_code == SVR_ERR_MESG) {				/* cannot  */
    char svr_mesg[SVR_MESG_BUF_LEN];				/* get     */
    memset ((void*) svr_mesg, '\0', SVR_MESG_BUF_LEN);		/* kernel  */

    if ( msclt_is_readable(sd) &&				/* file    */
         msclt_read_string(sd, svr_mesg, SVR_MESG_BUF_LEN)) {	/* from    */
       fprintf (stderr, "%s:%s\n", cm,				/* server. */
            "ERROR reading err mesg from server");
       fprintf (stderr, "%s:%s\n", cm,
            "Terminating server connection");
       }
    else fprintf (stderr, "%s\n", svr_mesg);

    close (sd);
    return -1;
    }

 sprintf (new_kname, "%s/%s", local_dir, kname);
 if (msclt_receive_bin_kernel(sd, new_kname)) {		/* cool, ready to  */
    fprintf (stderr, "%s:%s\n%s:%s", cm,		/* receive bin     */
	"ERROR sending binary kernel to socket",	/* kernel from	   */
	"Kernel name", kname);				/* socket.	   */
    close (sd);
    return -1;
    }

 close (sd);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_put_kernel (XDR_u_long req_code, char *kname)
{
 int sd;
 const char *cm = "msclt_put_kernel";

 if (msclt_connectToSvr(&sd)) {
    fprintf (stderr, "%s:%s\n", cm, 
	"ERROR connecting to spice server");
    return -1;
    }
 if (msclt_write_u_long(sd, req_code)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing request code to server");
    close (sd);
    return -1;
    }
 if (msclt_write_string (sd, kname, strlen(kname))) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing kernel name to socket");
    close (sd);
    return -1;
    }
 if (msclt_receive_bin_kernel(sd, kname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR receiving binary kernel from server");
    close (sd);
    return -1;
    }

 close (sd);
 return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
int msclt_gllgetck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (GLL_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_gllgetck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casgetck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (CAS_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_casgetck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simgetck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (SIM_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_simgetck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1getck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (VGR1_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_vgr1getck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2getck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (VGR2_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_vgr2getck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1getck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (VO1_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_vo1getck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2getck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (VO2_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_vo2getck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
int msclt_gllgetspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (GLL_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_gllgetspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casgetspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (CAS_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_casgetspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simgetspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (SIM_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_simgetspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1getspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (VGR1_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_vgr1getspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2getspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (VGR2_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_vgr2getspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1getspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (VO1_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_vo1getspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2getspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (VO2_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_vo2getspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
int msclt_gllputck (char *ckname)
{
 if (msclt_put_kernel (GLL_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_gllputck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casputck (char *ckname)
{
 if (msclt_put_kernel (CAS_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_casputck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simputck (char *ckname)
{
 if (msclt_put_kernel (SIM_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_simputck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1putck (char *ckname)
{
 if (msclt_put_kernel (VGR1_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_vgr1putck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2putck (char *ckname)
{
 if (msclt_put_kernel (VGR2_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_vgr2putck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1putck (char *ckname)
{
 if (msclt_put_kernel (VO1_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_vo1putck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2putck (char *ckname)
{
 if (msclt_put_kernel (VO2_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_vo2putck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
int msclt_gllputspk (char *spk)
{
 if (msclt_put_kernel (GLL_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_gllputspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casputspk (char *spk)
{
 if (msclt_put_kernel (CAS_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_casputspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simputspk (char *spk)
{
 if (msclt_put_kernel (SIM_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_simputspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1putspk (char *spk)
{
 if (msclt_put_kernel (VGR1_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_vgr1putspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2putspk (char *spk)
{
 if (msclt_put_kernel (VGR2_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_vgr2putspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1putspk (char *spk)
{
 if (msclt_put_kernel (VO1_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_vo1putspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2putspk (char *spk)
{
 if (msclt_put_kernel (VO2_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_vo2putspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*
 * returns the number of bytes actually read from the socket.
 *--------------------------------------------------------------------------*/
int msclt_readn (int sd, char *ptr, int nbytes)
{
 int            nleft,
                nread,
                status,
                count = 0,
                try = 0;  /* txh::added for socket read try */
 fd_set         readfds;
 struct timeval timeout;
 struct stat    f_stat;
 
 nleft = nbytes;                                /* init the total number */
 FD_ZERO(&readfds);                             /* of bytes and zero and */
 FD_SET(sd, &readfds);                          /* set readfds.          */
 
 memset ((char *) &timeout, '\0',               /* init the timeout      */
        sizeof (struct timeval));               /* count                 */
 timeout.tv_sec = (long) 5;

 fstat(sd, &f_stat);
 
 while ((nleft > 0) && (count < 10) && (try < 10)) {       /* keep reading  */
    /* txh::modified to distinguish between file and socket read for VMS */
    if ((f_stat.st_mode & S_IFMT) != S_IFREG)
       status = select (sd+1, (fd_set *) &readfds,         /* make sure sd  */
           (fd_set *) 0, (fd_set *) 0,                     /* is ready to   */
           (struct timeval *) &timeout);                   /* be read       */
    else status = 1;  /* a file descriptor */

    if ((status > 0) && (FD_ISSET(sd, &readfds))) {
       if ((f_stat.st_mode & S_IFMT) == S_IFREG)
          nread = read(sd, ptr, nleft);         /* read from a file */
       else
       {
          nread = socket_read(sd, ptr, nleft);  /* read from socket */

          /* txh::added to detect for case where the client goes away but 
                  select on socket descriptor returns no error. */
          if (nread == 0)
          {
             /* txh::detecting for continuous no data socket. */
             sleep (1);
             ++try;
          }
          else try = 0;
       }
       if (nread < 0) return -1;
       nleft -= nread;
       ptr += nread;
       }
    else if (status == 0) count++;
    else if (status < 0) return -1;
    }
 if (nleft == 0) return (nbytes);
 else return -1;
}
/*--------------------------------------------------------------------------*
 * returns the number of bytes actually written to the socket.
 *--------------------------------------------------------------------------*/
int msclt_writen (int sd, char *ptr, int nbytes)
{
 int            nleft,                       
                nwritten,
                status,
                count = 0;
 fd_set         writefds;
 struct timeval timeout;
 struct stat    f_stat;
 
 nleft = nbytes;
 FD_ZERO(&writefds);
 FD_SET(sd, &writefds);
 memset ((char *) &timeout, '\0',
        sizeof (struct timeval));
 timeout.tv_sec = (long) 5;

 fstat(sd, &f_stat);
 
 while (nleft > 0) {
    /* txh::modified to distinguish socket and file write for VMS */
    if ((f_stat.st_mode & S_IFMT) != S_IFREG)
       status = select (sd+1, (fd_set *) 0, (fd_set *) &writefds,
           (fd_set *) 0, (struct timeval *) &timeout);
    else status = 1;  /* a file descriptor */

    if (status > 0) {
       if ((f_stat.st_mode & S_IFMT) == S_IFREG)
          nwritten = write (sd, ptr, nleft); /* write to a file descriptor */
       else
          nwritten = socket_write(sd, ptr, nleft); /* write to a socket */
       if (nwritten <= 0) return -1;
       nleft -= nwritten;
       ptr += nwritten;
       }
    else if (status == 0) count ++;
    else if (status < 0) return -1;
    }
 if (nleft == 0) return (nbytes);
 else return -1;
}
/*--------------------------------------------------------------------------*
 * print out content of user request struct.
 *--------------------------------------------------------------------------*/
void msclt_printuserrequeststruct (msUserRequestStruct req)
{
 printf ("sc_id:\t\t\t%d\n", req.sc_id);
 printf ("system:\t\t\t%d\n", req.system);
 printf ("scet[0]:\t\t%d %d %d %d %d %d\n",
        req.scet[0], req.scet[1], req.scet[2],
        req.scet[3], req.scet[4], req.scet[5]);
 printf ("instrument_name:\t%s\n", req.instrument_name);
 printf ("target_name:\t\t%s\n\n", req.target_name);
 printf ("ck_id:\t\t%s\n", req.ck_id);
 printf ("ck_name:\t\t\t%s\n", req.ck_name);
 printf ("ck_source:\t\t%s\n", req.ck_source);
 printf ("spk_id:\t\t\t%s\n", req.spk_id);
 printf ("spk_name:\t\t\t%s\n", req.spk_name);
 printf ("seg_id:\t\t\t%s\n", req.provInfo.seg_id);
}
/*--------------------------------------------------------------------------*
 * print out content of CK struct.
 *--------------------------------------------------------------------------*/
void msclt_printckstruct (msCkStruct ck)
{
 printf ("sc_id:\t\t\t%d\n", ck.sc_id);
 printf ("instrument:\t\t%d\n", ck.instrument);
 printf ("system:\t\t\t%d\n", ck.system);
 printf ("scet[0]:\t\t%d %d %d %d %d %d\n\n",
        ck.scet[0], ck.scet[1], ck.scet[2],
        ck.scet[3], ck.scet[2], ck.scet[3]);

 printf ("av:\t\t\t%g %g %g\n\n",
	ck.av[0], ck.av[1], ck.av[2]);
 printf ("c_matrix:\t\t%g\t%g\t%g\n",
	ck.c_matrix[0], ck.c_matrix[1],
	ck.c_matrix[2]);
 printf ("\t\t\t%g\t%g\t%g\n",
	ck.c_matrix[3], ck.c_matrix[4],
	ck.c_matrix[5]);
 printf ("\t\t\t%g\t%g\t%g\n\n", 
	ck.c_matrix[6], ck.c_matrix[7], 
	ck.c_matrix[8]);
 
 printf ("ck_id:\t\t\t%s\n", ck.ck_id);
 printf ("ck_source:\t\t%s\n", ck.ck_source);
 printf ("segid:\t\t\t%s\n", ck.seg_id);
 printf ("ck_name:\t\t%s\n", ck.ck_name);
}
/*--------------------------------------------------------------------------*
 * print out content of SPK struct.
 *--------------------------------------------------------------------------*/
void msclt_printspkstruct (msSpkStruct spk)
{
 printf ("sc_id:\t\t\t\t%d\n", spk.sc_id);
 printf ("tgt_radius_l_axis:\t\t%g\n", spk.tgt_radius_l_axis);
 printf ("tgt_radius_s_axis:\t\t%g\n", spk.tgt_radius_s_axis);
 printf ("tgt_polar_radius:\t\t%g\n", spk.tgt_polar_radius);
 printf ("sc_pos_bd_centered:\t\t%g\t%g\t%g\n",
                spk.sc_pos_bd_centered[0],
                spk.sc_pos_bd_centered[1],
                spk.sc_pos_bd_centered[2]);
 printf ("pic_pos_sc_centered:\t\t%g\t%g\t%g\n",
                spk.pic_pos_sc_centered[0],
                spk.pic_pos_sc_centered[1],
                spk.pic_pos_sc_centered[2]);
 printf ("rs_vector:\t\t\t%g\t%g\t%g\n", spk.rs_vector[0],
                spk.rs_vector[1], spk.rs_vector[2]);
 
 printf ("range_pic_bd_2_sun:\t\t%g\n", spk.range_pic_bd_2_sun);
 printf ("range_sc_2_central_bd_cntr:\t%g\n", spk.range_sc_2_central_bd_cntr);
 printf ("range_sc_2_picture_bd_cntr:\t%g\n", spk.range_sc_2_picture_bd_cntr);
 printf ("tgt_bd_cntr_2_sun_lat:\t\t%g\n", spk.tgt_bd_cntr_2_sun_lat);
 printf ("tgt_bd_cntr_2_sun_lon:\t\t%g\n", spk.tgt_bd_cntr_2_sun_lon);
 printf ("tgt_bd_cntr_2_sc_lat:\t\t%g\n", spk.tgt_bd_cntr_2_sc_lat);
 printf ("tgt_bd_cntr_2_sc_lon:\t\t%g\n", spk.tgt_bd_cntr_2_sc_lon);
 
 printf ("\nme_matrix\t\t\t%g\t%g\t%g\n", spk.me_matrix[0],
                spk.me_matrix[1], spk.me_matrix[2]);
 printf ("\t\t\t\t%g\t%g\t%g\n", spk.me_matrix[3],
                spk.me_matrix[4], spk.me_matrix[5]);
 printf ("\t\t\t\t%g\t%g\t%g\n", spk.me_matrix[6],
                spk.me_matrix[7], spk.me_matrix[8]);
 printf ("om_matrix\t\t\t%g\t%g\t%g\n", spk.om_matrix[0],
                spk.om_matrix[1], spk.om_matrix[2]);
 printf ("\t\t\t\t%g\t%g\t%g\n", spk.om_matrix[3],
                spk.om_matrix[4], spk.om_matrix[5]);
 printf ("\t\t\t\t%g\t%g\t%g\n\n", spk.om_matrix[6],
                spk.om_matrix[7], spk.om_matrix[8]);
 
 printf ("north_angle:\t\t\t%g\n", spk.north_angle);
 printf ("sub_sc_line:\t\t\t%g\n", spk.sub_sc_line);
 printf ("sub_sc_samp:\t\t\t%g\n", spk.sub_sc_samp);
 printf ("p5_lat:\t\t\t\t%g\n", spk.p5_lat);
 printf ("p5_lon:\t\t\t\t%g\n", spk.p5_lon);
 printf ("p5_incidence_angle:\t\t%g\n", spk.p5_incidence_angle);
 printf ("p5_emission_angle:\t\t%g\n", spk.p5_emission_angle);
 printf ("p5_phase_angle:\t\t\t%g\n", spk.p5_phase_angle);
 printf ("p5_phase_angle:\t\t\t%g\n", spk.p5_phase_angle);
 printf ("p5_vert_pix_size:\t\t%g\n", spk.p5_vert_pix_size);
 printf ("p5_horiz_pix_size:\t\t%g\n", spk.p5_horiz_pix_size);
 printf ("range_sc2p5_intercept_pt:\t%g\n\n\n", spk.p5_horiz_pix_size);
}
