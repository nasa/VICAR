/* VMS-specific pseudo-Eunuchs(tm) routines for imitating sockets */
/* VICAR-specific also */
/* initial effort btcarcich dec'1989 */

#include "xvmaininc.h"

#include "sock_emulate.h"

#define accept dummydummy	/* to avoid bogus accept definition in socket.h */
#include <socket.h>	/* to get timeval structure */
#undef accept

int accept( int sd, struct sockaddr *S_addr, int *addrlen);

#include <time.h>
#include <types.h>
#include <lckdef.h>
#include <libdef.h>
#include <rmsdef.h>
#include <dvidef.h>
#include <iodef.h>
#include <psldef.h>
#include <stsdef.h>
#include <file.h>
#include <timeb.h>

/* the Eunuchs(tm) select READ or WRITE check mean check for when a fd can
      ***BE*** READ from or WRITTEN to without waiting, respectively, 
      by the CALLER.
   the VMS equivalent of this READ or WRITE check is to check for when
      the mailbox has ***BEEN*** WRITTEN to or READ from, respectively,
      by a COOPERATING PROCESS.
   So, i present the following #define's to hide this potential confusion
      of read/write selection
*/
#define SELECT$WRITE_CHECK IO$M_READATTN
#define SELECT$READ_CHECK IO$M_WRTATTN


#include <ssdef.h>
#include <stddef.h>
#include <errno.h>	/* to force additional errno's */
#include <stdio.h>
#include <descrip.h>

#define BREAKUP(A,B) A/(B); A=A%(B)

#ifdef DEBUG
#define DPR(A) printf A
#else
#define DPR(A) 
#endif

static fd_set opened_fds, unblocked_fds, bound_fds, connected_fds;
static fd_set readsel_fds, writesel_fds, exceptsel_fds; /* who gets selected */
static fd_set readqio_fds, writeqio_fds, exceptqio_fds; /* act qio/ast's */
static long nsel_fds, nqio_fds;		/* number of selected * qio'ed fd's */

long sys$qiow(), sys$hiber(), sys$setast(), sys$cancel(), sys$dassgn();
long sys$setimr(), sys$cantim(), sys$bintim(), sys$exit(), sys$wake();
long sys$assign(), sys$crembx();

#define ACCMXMSG 4200		/* accepted/connected mailbox message max len */
#define ACCBUFQUO 2*ACCMXMSG	/* accepted/connected mailbox buffer quota */
#define MAXMBXNAMLEN 255	/* maximum mailbox name length */

static struct {
   ushort mbxchan;		/* mailbox channel */
   char mbxname[ MAXMBXNAMLEN];	/* mailbox name */
   int mbxnamlen;		/* mailbox name length */
   int efn;			/* event flag number for sock_efn() */
} all_fds[MAXFDSWIDTH];

static long nfds;			/* number of fd's open */

static long selecttime;		/* type of select timing */
#define NULLTIME 0		/* -block indefinitely ('til 1st select) 
					-$HIBER
					-1st AST routine delivers $WAKE */
#define ZEROTIME 1		/* -poll all fd's once 
					-no $HIBER
					-no $WAKE's from AST */
#define WAITTIME 2		/* -wait for selects 
					-$HIBER
					-no $WAKE's from AST 
					-$WAKE from $SETIMR */

static long timrastdlvrd;	/* select time out NULL=>did not timeout */
static long selectastdlvrd;	/* 1st *selast routine delivered wake */

typedef struct IOSBstruct {
  short cndval;			/* condition value */
  short nxfer;			/* transfer count */
  long  dsi;			/* device-specific info */
  } IOSB;

static struct { long timelong[2]; } VMSbintim;
static long status;

#define ERRNRET(A) { errno=(A); return(-1);}

/* The event flags for XtAppAddInput() must be in cluster 0!  Due to a	*/
/* stupidity in VMS, lib$get_ef() only allocates from cluser 1, so it	*/
/* cannot be used.  We must arbitrarily use a chunk of flags, HOPING	*/
/* that nobody else decides to use them!  We free them on startup, then	*/
/* allocate/deallocate as needed.					*/

#define EF_START 10
#define EF_END 15

/**********************************************************************/
void init_fds()
/**********************************************************************/
{
long i;
static firsttime = 1;

  if ( firsttime) {

DPR( ("init_fds - inside firstime\n"));

    FD_ZERO( &opened_fds);
    FD_ZERO( &unblocked_fds);
    FD_ZERO( &bound_fds);
    FD_ZERO( &connected_fds);
    for(i=0;i<MAXFDSWIDTH; i++) {
        all_fds[i].mbxchan = -1;
        all_fds[i].efn = 0;
    }
    nfds=0;
    firsttime=FALSE;
  }
}

/**********************************************************************/
long getdtablesize()
/**********************************************************************/

{ return( MAXFDSWIDTH);} /* end of getdtablsize() */

/**********************************************************************/
int recv( s, msg, len, flags)	/* send a message from a socket */
/**********************************************************************/
int s;
char *msg;
int len, flags;
{
  ushort iofunc;
  IOSB iosb;

#ifdef DEBUG
  char dvibuf[65];	/* for $getdviw */
  int *msgint;
#endif

  s-=SOCKOFFS;

/* test inputs */


  if ( s < 0 || s >= MAXFDSWIDTH ) ERRNRET( EBADF)
  if ( !msg) ERRNRET( EFAULT)
  if ( !FD_ISSET( s, &opened_fds)) ERRNRET( EBADF)
  if ( !FD_ISSET( s, &connected_fds)) ERRNRET( EBADF)

  iofunc = IO$_READVBLK;
  if ( FD_ISSET( s, &unblocked_fds)) iofunc |= IO$M_NOW;

#ifdef DEBUG
  {
  struct {
    ushort abuflen;	/* available dvibuf length */
    ushort itmcod;
    char   *bufaddr;
    long   *pretlen;	/* pointer to returned length */
    } itmlst[2];
  long lretlen;		/* returned length */

  itmlst[1].abuflen=0; itmlst[1].itmcod=0; /* terminate itmlst */

  dvibuf[64]='\0';
  itmlst[0].abuflen=sizeof dvibuf;
  itmlst[0].itmcod=DVI$_DEVNAM;
  itmlst[0].bufaddr=dvibuf;
  itmlst[0].pretlen=(&lretlen);

  status=sys$getdviw(NULL,all_fds[s].mbxchan,NULL,&itmlst,&iosb,NULL,NULL,NULL);

  if ( status!=SS$_NORMAL) printf("$getdvi()=%d\n",status);
/*  printf("...chan(%d) name = >%s<\n", all_fds[s].mbxchan, dvibuf); */
  }
msgint=(int *)msg;
#endif /* DEBUG */

DPR(("about to recv(%d,,%d,%xx) - chan=%d/%s\n"
,s,len,flags,all_fds[s].mbxchan,dvibuf));

  iosb.cndval = 0;
  status = sys$qiow( NULL, all_fds[s].mbxchan, iofunc, &iosb, NULL, NULL, 
             msg, len, NULL,NULL,NULL,NULL);

DPR(("...recd(%d,[0:4]=%d/%xx,%d,)=%d - $qiow(%d/%s)=%d\n"
,s,*msgint,*msgint,len,iosb.nxfer,
all_fds[s].mbxchan,dvibuf,status));

/* special case:  unblocked and no write qio's queued */
  if ( FD_ISSET( s, &unblocked_fds)
      && iosb.cndval == SS$_ENDOFFILE) ERRNRET( EWOULDBLOCK)

  if ( !( status & STS$M_SUCCESS)) ERRNRET( status)	/* errno from $qiow */
  return( iosb.nxfer);
}

/**********************************************************************/
int send( s, msg, len, flags)	/* send a message from a socket */
/**********************************************************************/
int s;
char *msg;
int len, flags;

{
  ushort iofunc;
  IOSB iosb;

#ifdef DEBUG
  char dvibuf[65];	/* for $getdviw */
  int *msgint;
#endif

  s-=SOCKOFFS;

/* test inputs */

  if ( s < 0 || s >= MAXFDSWIDTH ) ERRNRET( EBADF)
  if ( !msg) ERRNRET( EFAULT)
  if ( !FD_ISSET( s, &opened_fds)) ERRNRET( EBADF)
  if ( !FD_ISSET( s, &connected_fds)) ERRNRET( EBADF)

/* DPR(("...starting send(s=%d,len=%d,flags=%08x)...\n",s,len,flags)); */

  iofunc = IO$_WRITEVBLK;
  if ( FD_ISSET( s, &unblocked_fds)) iofunc |= IO$M_NOW;

#ifdef DEBUG
  {
  struct {
    ushort abuflen;	/* available dvibuf length */
    ushort itmcod;
    char   *bufaddr;
    long   *pretlen;	/* pointer to returned length */
    } itmlst[2];
  long lretlen;		/* returned length */

  itmlst[1].abuflen=0; itmlst[1].itmcod=0; /* terminate itmlst */

  itmlst[0].abuflen=sizeof dvibuf;
  itmlst[0].itmcod=DVI$_DEVNAM;
  itmlst[0].bufaddr=dvibuf;
  itmlst[0].pretlen=(&lretlen);

  status=sys$getdviw(NULL,all_fds[s].mbxchan,NULL,&itmlst,&iosb,NULL,NULL,NULL);

  if ( status!=SS$_NORMAL) printf("$getdvi()=%d",status);
/*  printf("...chan(%d) name = >%s<\n", all_fds[s].mbxchan, dvibuf); */
  }
msgint=(int *)msg;
#endif /* DEBUG */

  iosb.cndval = 0;
DPR(("about to send(%d,[0:4]=%d/%xx,%d,%xx) - chan=%d/%s\n"
,s,*msgint,*msgint,len,flags,all_fds[s].mbxchan,dvibuf));

  status = sys$qiow( NULL, all_fds[s].mbxchan, iofunc, &iosb, NULL, NULL, 
             msg, len, NULL,NULL,NULL,NULL);

DPR(("...sent(%d,[0:4]=%d,%d,)=%d - $qiow(%d/%s)=%d\n"
,s,*msgint,len,iosb.nxfer,all_fds[s].mbxchan,dvibuf,status));

  if ( !( status & STS$M_SUCCESS)) ERRNRET( status)	/* errno from $qiow */
  return( iosb.nxfer);
}

/**********************************************************************/
int connect( s, addr, addrlen) /* connect to accepting socket s, 
                                  re-point s to addr
/**********************************************************************/
int s;
struct sockaddr *addr;
int addrlen;
{
  struct sockaddr localaddr;	/* local sockaddr */
  ushort mbxchan;		/* local mailbox channel # */
  ushort iofunc;
  IOSB iosb;
  struct dsc$descriptor_s mbxnamdescr;
  int getpid();
  struct timeb timestamp;

  s-=SOCKOFFS;

/* test inputs */

  if ( s < 0 || s >= MAXFDSWIDTH ) ERRNRET( EBADF)
  if ( !addr) ERRNRET( EFAULT)
  if ( addrlen < sizeof( struct sockaddr)) ERRNRET( EFAULT)
  if ( !FD_ISSET( s, &opened_fds)) ERRNRET( ENOTSOCK)
  if ( FD_ISSET( s, &connected_fds)) ERRNRET( EISCONN)

/* check if s is bound - if so, it is bound to the name of the mailbox
                         through which communication will go after 
                         accept/connect hookup 
                       - if s is NOT bound, then create a dummy name based
                         on a timestamp (so it's unique) through which
			 communication will go after the accept/connect. */
DPR(("in connect ...\n"));

  if ( !FD_ISSET( s, &bound_fds)) {

    ftime(&timestamp);
    sprintf(localaddr.sa_data, "VS_%06.6x%04.4x", timestamp.time&0x00FFFFFF,
						 timestamp.millitm);
					/* DPR(("localaddr.sa_data=>%s<..."
					, localaddr.sa_data)); */
    status=bind( (s+SOCKOFFS), &localaddr, sizeof( struct sockaddr));
					/* DPR(("bind()=%d ...\n", status));*/
    if ( status == -1) return( -1);	/* errno from bind */
  }

/* put the server mailbox name in a descriptor: */

  mbxnamdescr.dsc$w_length = strlen( addr->sa_data);
  mbxnamdescr.dsc$a_pointer = addr->sa_data;
  mbxnamdescr.dsc$b_class = DSC$K_CLASS_S;
  mbxnamdescr.dsc$b_dtype = DSC$K_DTYPE_T;

/* $assign to server mailbox */
/* DPR(("about to $assign to mbx %s ...\n",addr->sa_data)); */

  { int i;
    for ( i=0; i<20 && !(status & STS$M_SUCCESS); i++) {
      status = sys$assign( &mbxnamdescr, &mbxchan, PSL$C_USER, 
                       &mbxnamdescr);
					DPR(("#%d $assign=%d ...\n",i,status));

  if ( !( status & STS$M_SUCCESS)) 
#ifdef DEBUG
      printf("sleep(1) = %d \n",
#endif
      sleep( 1)
#ifdef DEBUG
      )
#endif
      ;
    }
  }

  if ( !( status & STS$M_SUCCESS)) ERRNRET( status)	/* errno from $assign */

/* qio write to server mailbox - send mailbox name */

  iofunc = IO$_WRITEVBLK;
/* no unblocked writes:  if ( FD_ISSET( s, &unblocked_fds)) iofunc |= IO$M_NOW;
  (arbitrary decision) */

  iosb.cndval = 0;
  status = sys$qiow( NULL, mbxchan, iofunc, &iosb, NULL, NULL, 
           all_fds[s].mbxname, all_fds[s].mbxnamlen,NULL,NULL,NULL,NULL);
					/* DPR(("$qiow=%d ...\n",status)); */
  if ( !(status & STS$M_SUCCESS)) ERRNRET( status)	/* errno from $qiow */

/* qiow was successful:  close the connection to the server mailbox 
   and create a data mailbox */

  status = sys$dassgn( mbxchan);
					/* DPR(("$dassgn=%d ...\n",status)); */
  if ( !(status & STS$M_SUCCESS)) ERRNRET( status)	/* errno from $dassgn */

/* ...put the mailbox name in a descriptor... */

  mbxnamdescr.dsc$w_length = all_fds[s].mbxnamlen;
  mbxnamdescr.dsc$a_pointer = all_fds[s].mbxname;
  mbxnamdescr.dsc$b_class = DSC$K_CLASS_S;
  mbxnamdescr.dsc$b_dtype = DSC$K_DTYPE_T;

/* ...and create the mailbox... */

  status = sys$crembx( NULL, &(all_fds[s].mbxchan), 
             ACCMXMSG, ACCBUFQUO,
             NULL, PSL$C_USER, &mbxnamdescr);

DPR(("about to leave connect; $crembx=%d...mbxchan chosen = %d\
...mbname =>%s<\n", status, all_fds[s].mbxchan, all_fds[s].mbxname));

/* set connected bit and return */

  if ( status & STS$M_SUCCESS) {
    FD_SET( s, &connected_fds);
    return( 0);
  }
  else ERRNRET( status) /* errno by $crembx */
}

/**********************************************************************/
int accept( s, addr, addrlen) /* accept connections */
/**********************************************************************/
int s;
struct sockaddr *addr;
int *addrlen;
{
  ushort iofunc;
  IOSB iosb;
  int newsocket;
  struct dsc$descriptor_s mbxnamdescr;
  char readit[14];		/* size of sockaddr.sa_data */

  s-=SOCKOFFS;

/* test inputs */

  if ( s < 0 || s >= MAXFDSWIDTH ) ERRNRET( EBADF)
  if ( !FD_ISSET( s, &opened_fds)) ERRNRET( EBADF)
  if ( !FD_ISSET( s, &bound_fds)) ERRNRET( ENOTSOCK)
  if ( !addr) ERRNRET( EFAULT)
  if ( *addrlen < sizeof( struct sockaddr)) ERRNRET( EFAULT)

/* qio read to mailbox */

  iofunc = IO$_READVBLK;

  if ( FD_ISSET( s, &unblocked_fds)) iofunc |= IO$M_NOW;

  iosb.cndval = 0;
  status = sys$qiow( NULL, all_fds[s].mbxchan, 
             iofunc, &iosb, NULL, NULL, &readit, sizeof(readit)
           , NULL,NULL,NULL,NULL);

  readit[iosb.nxfer]='\0';
DPR(("accept $qiow complete - read >%s< - status=%d\n", readit, status));

/* special case:  unblocked and no write qio's queued */
  if ( FD_ISSET( s, &unblocked_fds)
      && iosb.cndval == SS$_ENDOFFILE) ERRNRET( EWOULDBLOCK)

/* if qiow was successful... */

  if ( status & STS$M_SUCCESS) {

/* ...get a new socket... */

    newsocket = socket( NULL, NULL, NULL);
			DPR(("accept - newsocket = %d\n", newsocket));
    if ( newsocket == -1) return( -1);	/* errno by socket() */

/* ...copy name of mailbox to accept into addr address parameter
      and bind it to the newsocket... */

    else {

      newsocket-=SOCKOFFS;

      strncpy( addr->sa_data, readit, iosb.nxfer+1);
      *addrlen = sizeof( struct sockaddr);
      status = bind( ( newsocket+SOCKOFFS), addr, *addrlen);
		/* DPR(("accept - bind newsocket = %d\n", status)); */
      if ( status == -1) return( -1);		/* errno by bind() */

/* ...put the mailbox name in a descriptor... */
      mbxnamdescr.dsc$w_length = all_fds[newsocket].mbxnamlen;
      mbxnamdescr.dsc$a_pointer = all_fds[newsocket].mbxname;
      mbxnamdescr.dsc$b_class = DSC$K_CLASS_S;
      mbxnamdescr.dsc$b_dtype = DSC$K_DTYPE_T;

/* ...and create the mailbox... */

      status = sys$crembx( NULL, &(all_fds[newsocket].mbxchan), 
             ACCMXMSG, ACCBUFQUO,
             NULL, PSL$C_USER, &mbxnamdescr);
		DPR(("accept - chan=%d - $crembx = %d\n", 
			all_fds[newsocket].mbxchan, status));

/* and finally return the new mailbox/socket's fd */

      if ( status & STS$M_SUCCESS) {
        FD_SET( newsocket, &connected_fds);
        return( ( newsocket + SOCKOFFS) );
      }

      else ERRNRET( status) /* errno by $crembx */
    }
  }
  else ERRNRET( status)
}

/**********************************************************************/
int listen( s, backlog) /* create mailbox for listening for connections */
/**********************************************************************/
int s, backlog;
{
#define LISTENSIZE 256				/* max message size */

  long listenbufquo = backlog * 2 * LISTENSIZE;  /* buffer quota */

  struct dsc$descriptor_s mbxnamdescr;

  s-=SOCKOFFS;

  if ( s < 0 || s >= MAXFDSWIDTH ) ERRNRET( EBADF)
  if ( !FD_ISSET( s, &opened_fds)) ERRNRET( EBADF)
  if ( !FD_ISSET( s, &bound_fds)) ERRNRET( ENOTSOCK)

  mbxnamdescr.dsc$w_length = all_fds[s].mbxnamlen;
  mbxnamdescr.dsc$a_pointer = all_fds[s].mbxname;
  mbxnamdescr.dsc$b_class = DSC$K_CLASS_S;
  mbxnamdescr.dsc$b_dtype = DSC$K_DTYPE_T;

  status = sys$crembx( NULL, &(all_fds[s].mbxchan), 
             LISTENSIZE, listenbufquo,
             NULL, PSL$C_USER, &mbxnamdescr);
  if ( status & STS$M_SUCCESS) {
    FD_SET( s, &connected_fds);
    return( NULL);
  }
  else ERRNRET( status)
}

/**********************************************************************/
int bind( s, name, namelen) /* add name to socket */
/**********************************************************************/
int s;
struct sockaddr *name;
int namelen;
{
  DPR(("bind(): %d to %s\n", s, name->sa_data));
  if ( !name) ERRNRET( EFAULT)
  if ( namelen != sizeof( struct sockaddr)) ERRNRET( EINVAL)

  s-=SOCKOFFS;

  if ( s < 0 || s >= MAXFDSWIDTH ) ERRNRET( EBADF)
  if ( !FD_ISSET( s, &opened_fds)) ERRNRET( EBADF)
  if ( FD_ISSET( s, &bound_fds)) ERRNRET( EADDRINUSE)

  strcpy( all_fds[s].mbxname, name->sa_data);
  all_fds[s].mbxnamlen = strlen( all_fds[s].mbxname);

  FD_SET( s, &bound_fds);
  return( NULL);
}

/**********************************************************************/
int socket( domain, socktype, sockproto) /* allocate a socket bit */
/**********************************************************************/
int domain, socktype, sockproto; /* note - these arguments are ignored */
{
int socket_fd;

  init_fds();

  for ( socket_fd=0;
        ( socket_fd < MAXFDSWIDTH) && FD_ISSET( socket_fd, &opened_fds);
        socket_fd++) ;

  if ( socket_fd<MAXFDSWIDTH) {
    FD_SET( socket_fd, &opened_fds);
    FD_CLR( socket_fd, &bound_fds);
    FD_CLR( socket_fd, &unblocked_fds);
    FD_CLR( socket_fd, &connected_fds);
    return( socket_fd+SOCKOFFS);
  }

  else ERRNRET( -1)
}

/**********************************************************************/
int vmsockclose( des) /* close a socket descriptor */
/**********************************************************************/
int des;
{

  des-=SOCKOFFS;

  if ( ( 0 <= des) && (des < MAXFDSWIDTH)) {
    if (all_fds[des].efn != 0) {		/* free event flag if present */
      lib$free_ef(&all_fds[des].efn);
      all_fds[des].efn = 0;
    }
    if ( FD_ISSET( des, &opened_fds)) {
      FD_CLR( des, &opened_fds);
      if ( FD_ISSET( des, &connected_fds)) {
        status = sys$cancel(all_fds[des].mbxchan);  /* to kill sock_efn's qio */
        status = sys$dassgn(all_fds[des].mbxchan);
      }
      return( NULL);
    }
    else ERRNRET( EBADF)	/* not opened */
  }
  else ERRNRET( EBADF)		/* invalid socket descriptor, des */
}

/**********************************************************************/
void timrast() /* handle timr expiration as AST */
/**********************************************************************/
{
						/* DPR(("timrast - ")); */
  status = sys$wake( NULL,NULL);
					/* DPR(("$wake=%d\n", status)); */
  timrastdlvrd = (!NULL);
} /* end of timrast() */

/**********************************************************************/
void selast( i) /* handle general qio AST */
/**********************************************************************/
int i;		/* fd number */
{
  nsel_fds++;
			DPR(("selast, nsel_fds=%d after ++ ...\n",nsel_fds));
  if ( ( selecttime == NULLTIME)	/* first AST, unhibernate */
       || ( nsel_fds >= nqio_fds) )	/* or all qio's are in */
    status = sys$wake( NULL,NULL);
					/* DPR(("$wake=%d\n", status)); */
}

/**********************************************************************/
void excselast( i) /* handle exception qio AST */
/**********************************************************************/
int i;		/* fd number */
{
		DPR(("excselast, i=%d, nsel_fds=%d\n", i, nsel_fds));
  FD_SET( i, &exceptsel_fds);
  selast( i);
}

/**********************************************************************/
void readselast( i) /* handle read qio AST */
/**********************************************************************/
int i;		/* fd number */
{
		DPR(("readselast, i=%d, nsel_fds=%d\n", i, nsel_fds));
  FD_SET( i, &readsel_fds);
  selast( i);
}

/**********************************************************************/
void wrtselast( i) /* handle write qio AST */
/**********************************************************************/
int i;		/* fd number */
{
		DPR(("wrtselast, i=%d, nsel_fds=%d\n", i, nsel_fds));
  FD_SET( i, &writesel_fds);
  selast( i);
}

/**********************************************************************/
long select( width, readfds, writefds, exceptfds, timeout)
/**********************************************************************/

long width;
fd_set *readfds, *writefds, *exceptfds; /* original lists of who to query */
struct timeval *timeout;

{
  long deldy, delhr, delmin, delsec;
  unsigned long timrreqidt=0;

  char ascdeltim[] = {"dddd hh:mm:ss.cc"};
  struct dsc$descriptor_s ascdeltimdescr;

  long a;
  IOSB iosb;
  long i, localtimeout;
/**********************************************************************/
/* start execution */
#ifdef DEBUG
/*
  printf("inside select() ...");
  if ( readfds) printf(" *readfds=%08x", *readfds);
  if ( writefds) printf(" *writefds=%08x", *writefds);
  if ( exceptfds) printf(" *exceptfds=%08x", *exceptfds);
  printf( "\n");
*/
#endif
  nsel_fds = 0;			/* reset number of selected fd's */
  nqio_fds = 0;			/* reset number of qio'ed fd's */

  FD_ZERO( &readsel_fds);	/* ready fd_set's for who answers qio/ast */
  FD_ZERO( &writesel_fds);
  FD_ZERO( &exceptsel_fds);

  FD_ZERO( &readqio_fds);	/* ready fd_set's for who gets qio'ed */
  FD_ZERO( &writeqio_fds);
  FD_ZERO( &exceptqio_fds);

  sys$setast( (char) NULL);

  if ( readfds) 
  for ( i=0; i<(width<MAXFDSWIDTH ? width : MAXFDSWIDTH); i++) {

    if ( FD_ISSET( i, readfds) && FD_ISSET( i, &connected_fds)) {

/* DPR(("about to $qiow a read check on fd %d ... ", i)); */

      status = sys$qiow( 0, all_fds[i].mbxchan,
                           IO$_SETMODE|SELECT$READ_CHECK,
                           &iosb, NULL, NULL, 
                           readselast, i , NULL,NULL,NULL,NULL);
/* DPR(("$qiow($read_check)=%d\n", status)); */
      FD_SET( i, &readqio_fds);				/* record qio/ast */
      nqio_fds++;
    }
  } /* for i=0; i<width... */

  if ( writefds) 
  for ( i=0; i<(width<MAXFDSWIDTH ? width : MAXFDSWIDTH); i++) {

    if ( FD_ISSET( i, writefds) && FD_ISSET( i, &connected_fds)) {

/* DPR(("about to $qiow a write check on fd %d ... ", i)); */

      status = sys$qiow( 0, all_fds[i].mbxchan,
                           IO$_SETMODE|SELECT$WRITE_CHECK,
                           &iosb, NULL, NULL, 
                           wrtselast, i , NULL,NULL,NULL,NULL);
/* DPR(("$qiow(WRITE_CHECK)=%d\n", status)); */
      FD_SET( i, &writeqio_fds);			/* record qio/ast */
      nqio_fds++;
    }
  } /* for i=0; i<width... */

/* exception selection not implemented

  if ( exceptfds) 
  for ( i=0; i<(width<MAXFDSWIDTH ? width : MAXFDSWIDTH); i++) {
    if ( FD_ISSET( i, exceptfds) && FD_ISSET( i, &connected_fds)) {

      status = sys$qiow( 0, all_fds[i].mbxchan,
                           IO$_SETMODE|IO$_?ATTN,
                           &iosb, NULL, NULL, 
                           excselast, i , NULL,NULL,NULL,NULL);
      FD_SET( i, &exceptqio_fds);
      nqio_fds++;
    }
  } /* for i=0; i<width... */
/**/

/* enable ast's */

  sys$setast( (char) 1);

/* set selecttime switch for wait/nowait behavior from timeout */

  if ( !timeout) selecttime=NULLTIME;
  else
    if ( timeout->tv_sec || timeout->tv_usec) {
      selecttime=WAITTIME;
      localtimeout = timeout->tv_sec;
      if ( timeout->tv_usec) localtimeout++;	/* round fractional seconds */
    }
    else selecttime=ZEROTIME;

/* DPR(("selecttime = %d\n", selecttime)); */

/* act (or not) according to selecttime */

  switch( selecttime)
  {
    case WAITTIME:

/* DPR(("just after case WAITTIME:\n")); */

      /* set up ast to be delivered after localtimeout seconds */

      /* (1) convert delta seconds to ascii delta time to binary time */

      deldy = BREAKUP( localtimeout, 86400);
      delhr = BREAKUP( localtimeout, 3600);
      delmin = BREAKUP( localtimeout, 60);
      delsec = localtimeout;
      sprintf( ascdeltim, "%d %d:%d:%d.00", 
               deldy, delhr, delmin, delsec);

/* DPR(("ascdeltim=>%s<\n", ascdeltim)); */

      ascdeltimdescr.dsc$w_length = strlen( ascdeltim);
      ascdeltimdescr.dsc$a_pointer = ascdeltim;
      ascdeltimdescr.dsc$b_class = DSC$K_CLASS_S;
      ascdeltimdescr.dsc$b_dtype = DSC$K_DTYPE_T;

      status = sys$bintim( &ascdeltimdescr, &VMSbintim);
					/* DPR(("$bintim = %08x\n", status)); */
      /* clear timr ast delivered flag and set timr */

      timrastdlvrd=NULL;
      status = sys$setimr( NULL, &VMSbintim, timrast, timrreqidt);
			if ( status != SS$_NORMAL) {
				DPR(("VMSbintim= %08x - ", VMSbintim));
				DPR(("$setimr = %08x\n", status));
			}
      /* drop through to hibernation */

    case NULLTIME:
				/* DPR(("just after case NULLTIME:\n")); */
      /* hibernate until woken up by select AST or wake AST */

      sys$hiber();

				/* DPR(("after $hiber in NULLTIME:\n")); */

      /* if timr may not have expired then cancel it and flush any
         $WAKE's with our own non-stacking $WAKE and $HIBER */

      if ( selecttime = WAITTIME) 
        if ( !timrastdlvrd) {

#ifdef DEBUG
          /* printf( "$cantim()=%d", */
#endif
          sys$cantim( timrreqidt, PSL$C_USER)
#ifdef DEBUG
          /* ) */
#endif
          ;

#ifdef DEBUG
          /* printf( "$wake()=%d", */
#endif
          sys$wake(NULL,NULL)
#ifdef DEBUG
          /* ) */
#endif
          ;
				/* DPR(("before $hiber2 NULLTIME:\n")); */
#ifdef DEBUG
          /* printf( "$hiber2()=%d", */
#endif
          sys$hiber()
#ifdef DEBUG
          /* ) */
#endif
          ;
				/* DPR(("after $hiber2 NULLTIME:\n")); */
        }

      /* drop through to final handling */

    case ZEROTIME:
					/* DPR(("after case ZEROTIME:\n")); */
      /* disable AST's */

      sys$setast( (char) NULL);

      /* increment selected fd's
      or cancel pending AST-generating qio's */

      for ( i=0; 
            i<(width<MAXFDSWIDTH ? width : MAXFDSWIDTH);
            i++) {

        if ( FD_ISSET( i, &readqio_fds))
          if ( !FD_ISSET( i, &readsel_fds)) {
		/* DPR(("$cancelling $qio for read-check fd %d\n", i));*/
            status = sys$cancel( all_fds[i].mbxchan);
          }
        if ( FD_ISSET( i, &writeqio_fds))
          if ( !FD_ISSET( i, &writesel_fds)) {
		/* DPR(("$cancelling $qio for write-check fd %d\n", i)); */
            status = sys$cancel( all_fds[i].mbxchan);
          }
/* exception select not implemented
        if ( FD_ISSET( i, &exceptqio_fds))
          if ( !FD_ISSET( i, &exceptsel_fds))
            status = sys$cancel( all_fds[i].mbxchan);
*/
      } /* for i=0; i<width... */

      if ( readfds)
        *readfds = readsel_fds;	/* load answered fds's into passed args */

      if ( writefds)
        *writefds = writesel_fds;

      if ( exceptfds)
        *exceptfds = exceptsel_fds;

      sys$setast( (char) 1);	/* renable ast's - they should 
                                   happen because of sys$cancel()'s 
                                   and harmlessly load up *sel_fds's */
      break;

    default:
      /* huh? how did we get here? */
      printf( "switch(selectime) logic error in routine select");
      printf( "selecttime = %d\n", selecttime);
      sys$exit( SS$_NORMAL);

  } /* case (selectime) */
  return( nsel_fds);
} /* select() */


/**********************************************************************/
void sockefnast(efn)		/* handle socket efn AST */
/**********************************************************************/
int efn;			/* efn number */
{
  sys$setef(efn);
}

/**********************************************************************/
int sock_efn(s)		/* Activate and return an event flag for reading mbx */
			/* Return event flag on success; -1, errno on error  */
/**********************************************************************/
int s;
{
  IOSB iosb;
  static int firsttime=1;
  int i;

  if (firsttime) {		/* Free some event flags for our use */
    for (i=EF_START; i<=EF_END; i++)
      lib$free_ef(&i);
    firsttime = 0;
  }

  s-=SOCKOFFS;

/* test inputs */

  if ( s < 0 || s >= MAXFDSWIDTH ) ERRNRET( EBADF)
  if ( !FD_ISSET( s, &opened_fds)) ERRNRET( EBADF)
  if ( !FD_ISSET( s, &connected_fds)) ERRNRET( EBADF)

/* Allocate an event flag if we don't already have one */

  if (all_fds[s].efn == 0) {
    for (i=EF_START; i<=EF_END; i++) {
      status = lib$reserve_ef(&i); 
      if (status == SS$_NORMAL) {
        all_fds[s].efn = i;
        break;
      }
    }
    if (all_fds[s].efn == 0)
      ERRNRET(EBADF);
  }
  sys$clref(all_fds[s].efn);		/* Clear event flag */
  sys$setast(1);			/* enable ASTs */

/* Now queue a read check to the mailbox.  When data becomes available,	*/
/* the AST will fire which will set the given event flag.		*/

  status = sys$qiow(0, all_fds[s].mbxchan, IO$_SETMODE|SELECT$READ_CHECK,
                    &iosb, NULL, NULL, 
                    sockefnast, all_fds[s].efn, NULL,NULL,NULL,NULL);
  if (status != SS$_NORMAL) {
    ERRNRET(EBADF);
  }

  return all_fds[s].efn;
}

