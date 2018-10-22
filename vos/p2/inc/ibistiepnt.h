/*****************************************************************************
 ibistiepnt.h   Routines for I/O to IBIS tiepoint files containing
 general qualifiers and image point qualifiers.

 Revision History:
  2-14-97  S. Pohorsky    Original version

 Changes:
  20-06-97 by F. Wewel, DLR

 References:   Mars 94/96 image point collection program design document,
               Juergen Oberst, DLR, 31-Oct-1996, Appendix 3
               
*****************************************************************************/

#ifndef IBISTIEPNT_H
#define IBISTIEPNT_H

#include "ibisfile.h"
#include "ibiserrs.h"

#ifdef __cplusplus
extern "C" {
#endif

#define  ERR            -1                         
#define  OK              1
#define  FNAMLEN       120
#define  STRING_32      32

// MAX_noimgs was 12; increased to match tp (2010-01-25 rgd by request from DLR)
#define  MAX_noimgs     25
#define  MAX_nogenqlf   20
#define  MAX_noimgqlf   20

#define MISSING_POINT_VALUE -1

#ifndef _NO_PROTO
/*
 *=============================================================================
 * Function prototypes
 *=============================================================================
 */
int zitiepnt_openw(int, int *, int, char [][FNAMLEN], 
		   int, char [][STRING_32], 
		   char [][IFMT_SIZE], char [][STRING_32], int, 
		   char [][STRING_32], char [][IFMT_SIZE], 
		   char [][STRING_32], int);

int zitiepnt_openr(int, int *, int *, char [][FNAMLEN], int *, 
		   char [][STRING_32], char [][IFMT_SIZE], 
		   char [][STRING_32], int *, char [][STRING_32], 
		   char [][IFMT_SIZE], char [][STRING_32], int *);

int zitiepnt_openu(int, int *, int *, char [][FNAMLEN], int *, 
		   char [][STRING_32], char [][IFMT_SIZE], 
		   char [][STRING_32], int *, char [][STRING_32], 
		   char [][IFMT_SIZE], char [][STRING_32], int *);

int zitiepnt_read(int, int, float *, float *,
		  float *, int *, char *,
		  float *, int *, char *);

int zitiepnt_write(int, int, float *,float *,
		   float *, int *, char *,
		   float *, int *, char *);

int zitiepnt_close(int);

#else /* _NO_PROTO */

int     zitiepnt_openw();
int     zitiepnt_openr();
int     zitiepnt_openu();
int     zitiepnt_read();
int     zitiepnt_write();
int     zitiepnt_close();

#endif /* _NO_PROTO */

#ifdef __cplusplus
}
#endif

#endif
