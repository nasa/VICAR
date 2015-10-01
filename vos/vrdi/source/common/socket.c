/*
  AF_UNIX socket routines.
 
  Mark S. Mann
  
  Routines:

    socket_open(lstn, caller_name, caller_hdr, callee_name, callee_hdr, wfc)

         Opens either a client or a server socket.  Returns fd of the
	 socket open to the server, or fd of the server to be listened on,
	 or -1 on error.

         int lstn (in) 
            lstn==0 --> create a client socket
            lstn >0 --> create a server socket with a queue length
                        of lstn
         char *caller_name (in)
            name of the socket to be created
         SOCKADDR *caller_header (out)
            socket structure containing info on your socket being created
         char *callee_name (in) CLIENT ONLY
            name of the server socket to which to connect 
         char *callee_hdr  (out) CLIENT ONLY
            socket structure containing info on the socket being connected to
         int wfc (in) CLIENT ONLY
            wfc==0 --> If server is non-blocking,
	               wait for it to answer your connect
            wfc!=0 --> If server is non-blocking,
	               do not wait for it to answer your connect

    socket_connect(fd, callee_name, callee_header, wfc)

         Connect to an open socket.  Should only be called by socket_open.
	 Returns -1 on error, 0 on success.
    
         int fd (in)
	    file descriptor of the open to the server socket
         char *callee_name (in)
            name of the server socket to which to connect 
         char *callee_hdr  (out)
            socket structure containing info on the socket being connected to
         int wfc (in)
            wfc==0 --> If server is non-blocking,
	               wait for it to answer your connect
            wfc!=0 --> If server is non-blocking,
	               do not wait for it to answer your connect


    socket_accept(fd, callee_header, wfa, slow)

         Accept a request to be connected to. Should be called by
	 the server only. Returns the fd of the client socket on success,
	 -1 on error. 

	 int fd (in)
	    file desc of the server socket
	 char *callee_header (in)
	    socket info on the server (set by socket_open)
	 int wfa (in)
            If true, wait for client to connect, otherwise return if no
                       connection
         int slow (in)
            If true, wait a short time for connection (see socket_read_waiting),
                       else return immediately (if !wfa).

    socket_recv(fd, msg, len)

         Read len bytes into msg from the socket fd.  Returns number of
	 bytes read from socket, 0 on error.

	 int fd (in)
	    file desc of the client (if server) or of own socket (if client)
	 unsigned char *msg (out)
	    buffer to hold data read
	 int len (in)
	    length (in bytes) of data to be read


    socket_send(fd, msg, len)

         Send len bytes from msg buffer into the socket fd.  Returns number
	 of bytes written to socket, 0 on error.

	 int fd (in)
	    file desc of the client (if server) or of own socket (if client)
	 unsigned char *msg (out)
	    buffer holding data to be written
	 int len (in)
	    length (in bytes) of data to be written


    socket_close(fd)

         Close a socket.

	 int fd (in)
	    file desc of socket.


    socket_free(fd)

         Close a socket and remove its associated file.  BUG: only allows
	 one open socket at a time.

	 int fd (in)
	    file desc of socket.


    socket_read_waiting(num, fds, index, slow)

         Checks "num" sockets contained in the array fds to see if any
         data is waiting to be read.  Returns non-zero (actually the number
         of sockets with data waiting) if any is available, with *index
         set to the index of the first socket with data.  Returns 0 if
         no data is ready, or -1 if something broke.  If slow is set, then
         do not instantaneously poll, but allow for a small delay.

         int num (in)
            number of active sockets in fds array
         int fds[] (in)
            array of file descriptors for each active socket
         int *index (out)
            returned index into fds of socket with data waiting
	 int slow (in)
	    1/0  'wait a bit'/poll on socket for incoming data

    socket_write_waiting(fd)

         Return non-zero status if data is ready to be written to.

	 int fd (in)
	    file desc of socket which is interesting for writing.

    socket_error(buf)

         Print a fatal socket error message. EXITS

	 char *buf (in)
            infomative info.
*/    

#include "xvmaininc.h"

#include <stdio.h>
#include <errno.h>
#if VMS_OS
#include <file.h>
#include <types.h>
#include <socket.h>
#include "sock_emulate.h"	/* definitions for socket emulation routines */
#include <time.h>
#else
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#endif
#include <string.h>
#include "xdintproto.h"

#define FALSE     0
#define TRUE      1

#define MAXRETRY  10

typedef struct sockaddr SOCKADDR;

#define MAXFD    256

static int MAXPASSTHRU = 4096;  /* 4 Kbytes */



int socket_open(lstn, name, header, wfc)

     int   lstn;         /* if lstn > 0, create the listen socket (queue len
			    of lstn), otherwise, connect to the socket */
     char *name;	 /* name of socket to create                   */
     SOCKADDR   *header;
     int   wfc;          /* wait for connect? 1-yes 0-no               */
{
  int fd;
  char buffer[80];

  if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
    socket_error("Couldn't open socket");
    return(fd);
  }

/* The strcpy() below is bogus because the string length is 14, and the	*/
/* buffer size is 14, leaving no room for the terminator.  Fix this	*/
/* at some point.							*/

  strcpy(header->sa_data, name);
  header->sa_family = AF_UNIX;

  if (lstn > 0) {

#if UNIX_OS
    if (unlink(header->sa_data) == -1) {
      if (errno != ENOENT) {
        sprintf(buffer, "Couldn't unlink %s\n", header->sa_data);
        socket_error(buffer);
      }
    }
#endif

    if (bind(fd, header, sizeof( SOCKADDR )) == -1) {
      socket_error("Couldn't bind socket");
    }
    if (listen(fd, lstn) == -1) {
      socket_error("Couldn't start listen");
    }
  }
  else {
    if (socket_connect(fd, name, header, wfc) == -1) {
      fd = -1;
    }
  }

  return(fd);
}


int socket_connect(fd, callee_name, callee_header, wfc)
     int       fd;
     char     *callee_name;
     SOCKADDR *callee_header;
     int       wfc;
{
  int status;

/* The strcpy() below is bogus because the string length is 14, and the	*/
/* buffer size is 14, leaving no room for the terminator.  Fix this	*/
/* at some point.							*/

  strcpy(callee_header->sa_data, callee_name);
  callee_header->sa_family = AF_UNIX;

  do {

    status = connect(fd, callee_header, sizeof( SOCKADDR ));

  } while (status == -1  &&  wfc);

  if (status == -1)
    socket_error("Couldn't connect socket");

  return(status);
}


int socket_accept(slave_fd, callee_header, wfa, slow)
     int       slave_fd;       /* slave fd */
     SOCKADDR *callee_header;
     int       wfa;            /* wait for accept: 1-yes 0-no */
     int       slow;	       /* Delay a short time for connect if true */
{
  int  fd;     /* master fd */
  socklen_t  length;
  int  status;
  int  index;

  length = sizeof(SOCKADDR);

  fd = -1;
  do {
    if ((status = socket_read_waiting(1, &slave_fd, &index, slow)) == -1)
      break;
    if (status > 0) {
      if ((fd = accept(slave_fd, callee_header, &length)) == -1) {
	socket_error("Accept error");
      }
    }
  } while (wfa && fd == -1);

  return(fd);
}



int socket_recv(fd, msg, msglen)
     int fd;
     unsigned char *msg;
     int msglen;
{
  int i;
  int len;
  int count;
  int status;
  int size, total_size;
  int npasses;
  unsigned char *ptr;

  npasses = msglen/MAXPASSTHRU;

  count = MAXRETRY;
  total_size = 0;
  for (i=0; i<npasses+1 && count; i++) {
    size = (i == npasses) ? (msglen % MAXPASSTHRU) : MAXPASSTHRU;
    if (size > 0) {
      ptr  = &(msg[i*MAXPASSTHRU]);

      count = MAXRETRY;
      do {
	count--;

	len = recv(fd, ptr, size, 0);
	status = send(fd, &len, sizeof(int), 0);

	if (len == -1  ||  status == -1) {
	  return 0;
	}

      } while (count  &&  !(len == size));

      if (!count) {
	socket_error("Recv error, exceeded max retry limit");
      }
    }
    total_size += size;
  }

  return(total_size);
}


int socket_send(fd, msg, msglen)
     int fd;
     unsigned char *msg;
     int msglen;
{
  int i;
  int len;
  int count;
  int status;
  int size, total_size;
  int npasses;
  unsigned char *ptr;

  npasses = msglen / MAXPASSTHRU;

  count = MAXRETRY;
  total_size = 0; 
  for (i=0; i<npasses+1 && count; i++) {
    size = (i == npasses) ? (msglen % MAXPASSTHRU) : MAXPASSTHRU;
    if (size > 0) {

      ptr = &(msg[i*MAXPASSTHRU]);
      count = MAXRETRY;
      do {
	count--;

	len = send(fd, ptr, size, 0);
	status = recv(fd, &len, sizeof(int), 0);

	if (len == -1  ||  status == -1) {
	  return 0;
	}

      } while (count  &&  !(len == size));

      if (!count) {
	socket_error("Send error, exceeded max retry limit");
      }
    }
    total_size += size;
  }

  return(total_size);
}


void socket_close(fd)
     int fd;
{
#if VMS_OS
  vmsockclose(fd);
#else
  close(fd);
#endif
}


void socket_free(fd, header)
     int fd;
     struct sockaddr *header;
{
#if VMS_OS
  vmsockclose(fd);
#else
  close(fd);
  if (header != NULL)
    unlink(header->sa_data);
#endif
}


int socket_read_waiting(num, fds, index, slow)
     int num;
     int *fds;
     int *index;
     int slow;
{
  int i;
  int width, status;
  struct timeval timeout;
  fd_set         readfds;

  *index = -1;
  width           = get_dtablesize();

  if (slow) {
    timeout.tv_sec  = 0;
    timeout.tv_usec = 100000;
  }
  else {
    timeout.tv_sec  = 0;
    timeout.tv_usec = 0;
  }
  
  FD_ZERO(&readfds);
  for (i=0; i<num; i++)
    FD_SET(fds[i], &readfds);

  status = select(width, &readfds, 0, 0, &timeout);

  if (status > 0) {
    for (i=0; i<num; i++)
      if (FD_ISSET(fds[i], &readfds)) {
        *index = i;
        break;
      }
    if (*index == -1)		/* huh?  fd not found?? */
      status = 0;
  }

  return(status);
}  


int socket_write_waiting(fd)
     int fd;
{
  int width, status;
  struct timeval timeout;
  fd_set         writefds;

  width           = get_dtablesize();
  timeout.tv_sec  = 0;
  timeout.tv_usec = 100000;
  
  FD_ZERO(&writefds);
  FD_SET(fd, &writefds);

  status = select(width, 0, &writefds, 0, &timeout);

  if (status > 0) {
    status = FD_ISSET(fd, &writefds);
  }

  return(status);
}  


void socket_error(buf)
     char *buf;
{
  char msg[81];

  sprintf(msg, "SOCKET:  %s (%d)", buf, errno);
  error(msg);
}

#if GETDTABLESIZE_OS
int get_dtablesize()
{
   return getdtablesize();	/* emulated */
}
#else

/* Some Unixes have getdtablesize(), and some don't, so we just emulate */
/* the routine using calls that they do have.				*/

int get_dtablesize()
{
   struct rlimit rlp;

   rlp.rlim_cur = 32;	/* in case of error: same as MAXFDSWIDTH on VMS emul */
   getrlimit(RLIMIT_NOFILE, &rlp);
   return rlp.rlim_cur;
}
#endif

