/* These definitions are needed for the emulation of the socket routines */
/* in VMS.								 */

#ifndef _EMULATE_H
#define _EMULATE_H

/* Typedefs (why aren't these in <types.h>??? */

typedef unsigned short ushort;

/* Emulation for FD family of macros */

#define SOCKOFFS 10000	/* offset to keep socket fd's != system fd's */
#define MAXFDSWIDTH 32

typedef int fd_set;

/* make FD_SET/_CLR/_ISSET/_ZERO suitable for both forms of fd */

#define FD_SET(n,p) ( n<SOCKOFFS? (*(p)|=(1<<n)): (*(p)|=(1<<(n-SOCKOFFS))) )
#define FD_CLR(n,p) ( n<SOCKOFFS? (*(p)&=~(1<<n)): (*(p)&=~(1<<(n-SOCKOFFS))) )
#define FD_ISSET(n,p) ( n<SOCKOFFS? (*(p)&(1<<n)): (*(p)&(1<<(n-SOCKOFFS))) )
#define FD_ZERO( p) { *(p) = 0;}

/* The following is copied from a Unix types.h for reference */

/*
 * These macros are used for select().  select() uses bit masks of file
 * descriptors in longs.  These macros manipulate such bit fields (the
 * file sysrem macros uses chars).  FD_SETSIZE may be defined by the user,
 * but must be >= u.u_highestfd + 1.  Since we know the absolute limit on
 * number of per process open files is 2048, we need to define FD_SETSIZE
 * to be large enough to accomodate this many file descriptors.  Unless the
 * user has this many files opened, he should redefine FD_SETSIZE to a
 * smaller number.
 */

#if 0

#define MAXFUPLIM       2048
#define FD_SETSIZE MAXFUPLIM

typedef long fd_mask;

#ifndef howmany
#define howmany(x,y)  (((x)+((y)-1))/(y))
#endif

typedef struct fd_set {
   fd_mask fds_bits[howmany(FD_SETSIZE, NFDBITS)];
} fd_set;

#define FD_SET(n,p)  ((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define FD_CLR(n,p) ((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define FD_ISSET(n,p) ((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))

#define FD_ZERO(p)     memset((char *)(p), (char) 0, sizeof(*(p)))

#endif /* 0 */

#endif /* _EMULATE_H */
