/*	XD_Get_Segments - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = XD_Get_Segments( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */
#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"


#define MAX_LINES 3
#define ABOVE (top->y) - 1
#define BELOW (top->y) + 1

struct RUNS_STRUCT {
  int leftx;
  int rightx;
  int y;
  int popped;
};
struct READ_BUF {
  int line;
  unsigned char *buf;
};

#if VMS_OS
#if ALPHA_ARCH
static struct RUNS_STRUCT *top;
static struct RUNS_STRUCT *run;
static int running;
#else
globaldef noshare struct RUNS_STRUCT *top;
globaldef noshare struct RUNS_STRUCT *run;
globaldef noshare int running;
#endif
#endif /* VMS_OS */

#if UNIX_OS
static struct RUNS_STRUCT *top;
static struct RUNS_STRUCT *run;
static int running;
#endif /* UNIX_OS */

/**/
FUNCTION int XD_Get_Segments( Unit, Imp, X, Y, Boundary, bottom, max_runs )
INTEGER Unit, Imp, X, Y;
BYTE Boundary;
struct RUNS_STRUCT *bottom;
INTEGER max_runs;
{
  struct READ_BUF lines[3], *first_line, *next;
  unsigned char *buff;
  unsigned char *which_buff();
  int status, length, i;

  first_line = &lines[0];
  run = top = bottom;
  length = ((AW_RIGHT( *Imp ) - AW_LEFT( *Imp ) + 1)+3)/4*4;
  status = SUCCESS;

  for ( next = first_line; next < (first_line + MAX_LINES); next++ ) {
    next->line = 0;
    next->buf = (unsigned char *) malloc( length );
    if ( next->buf == 0 ) {
      status = MEMORY_ERROR;
      break;
    }
  }
  if (status == SUCCESS ) {
    buff = which_buff( *Y, first_line );
    status = XD_Device_Interface( Unit, READ_LINE,
				 *Imp, AW_LEFT(*Imp), *Y, length, buff );
    if (status == SUCCESS ) {
      find_left( AW(*Imp), *X, buff, *Boundary );
      find_right( AW(*Imp), *X, buff, *Boundary );
      run->y = *Y;
      run->popped = run->rightx - run->leftx + 1;
      running = 1;
      run++;
/**/    
      while ( top >= bottom ) {
	if ( top->y > AW_TOP(*Imp) ) {

	  buff = which_buff( ABOVE, first_line );
	  status = XD_Device_Interface( Unit, READ_LINE,
				       *Imp, AW_LEFT(*Imp), ABOVE, length, buff );
	  if (status != SUCCESS ) break;

	  status = stack_runs( AW(*Imp), ABOVE, bottom, buff, max_runs, Boundary );
	  if (status != SUCCESS ) break;
	}
	if ( top->y < AW_BOTTOM(*Imp) ) {

	  buff = which_buff( BELOW, first_line );
	  status = XD_Device_Interface( Unit, READ_LINE,
				    *Imp, AW_LEFT(*Imp), BELOW, length, buff );
	  if (status != SUCCESS ) break;

	  status = stack_runs( AW(*Imp), BELOW, bottom, buff, max_runs, Boundary );
	  if (status != SUCCESS ) break;
	}
	for( top = run - 1; (top >= bottom) && (top->popped); top-- );
	if ( top >= bottom ) top->popped = top->rightx - top->leftx + 1;
      }
      for ( run = bottom; run < ( bottom + running ); run++ ) run->leftx++;
      *max_runs = running;
    }
  }
  return (status);
}
/**/
FUNCTION unsigned char *which_buff( y, first )
int y;
struct READ_BUF *first;
{
  struct READ_BUF *check;
  unsigned char *temp;

  static struct READ_BUF *next;
  static int init = TRUE;

  if ( init ) {
    next = first;
    init = FALSE;
  }
  for ( check = first; check  < ( first + MAX_LINES); check++ ) {
    if ( y == check->line ) return( (unsigned char *) check->buf );
  }
  next->line = y;
  temp = next->buf;
  next++;
  if ( next > ( first + MAX_LINES - 1 ) ) next = first;
  return ( (unsigned char *) temp );
}

FUNCTION int stack_runs( window, y, bottom, buff, max_runs, Boundary )
INTEGER window, max_runs;
int y;
struct RUNS_STRUCT *bottom;
BYTE *buff, Boundary;
{
  struct RUNS_STRUCT *prev;
  int temp_leftx, status;

  status = SUCCESS;
  prev = run;
  find_left( window, top->leftx, buff, *Boundary );

  if (( run->leftx <= top->rightx ) && ( run->leftx <= window[RIGHT] )) {
    find_right( window, run->leftx, buff, *Boundary );
    run->y = y;
    status = stack_it( bottom, *max_runs );
    if (status == SUCCESS ) {

      if ( prev->rightx < top->rightx ) {

	temp_leftx = prev->rightx + 1;
	while ( temp_leftx < top->rightx ) {

	  find_left( window, temp_leftx, buff, *Boundary );
	  if ( run->leftx <= top->rightx ) {

	    find_right( window, run->leftx, buff, *Boundary );
	    run->y = y;
	    prev = run;
	    temp_leftx = prev->rightx + 1;
	    status = stack_it( bottom, *max_runs );
	    if (status != SUCCESS ) break;
	  } else break;
	}
      }
    }
  }
  return( status );
}
/**/
FUNCTION int find_left( window, x, buff, Boundary )
INTEGER window;
int x;
unsigned char *buff, Boundary;
{
  if ( buff[x-window[LEFT]+1] == Boundary ) {
    for (run->leftx = x;
	 (run->leftx <= window[RIGHT]) && 
	  (buff[run->leftx-window[LEFT]+1] == Boundary) &&
	  (run->leftx <= top->rightx);
	 run->leftx++ );
  } else {
    for (run->leftx = x;
	 (run->leftx >= window[LEFT]) && 
	  (buff[run->leftx-window[LEFT]+1] != Boundary);
	 run->leftx-- );
    if ( buff[run->leftx-window[LEFT]+1] == Boundary ) run->leftx++;
  }
}

FUNCTION int find_right( window, x, buff, Boundary )
INTEGER window;
int x;
unsigned char *buff, Boundary;
{
  for (run->rightx = x;
       (run->rightx < window[RIGHT]) && 
	(buff[run->rightx-window[LEFT]+1] != Boundary);
       run->rightx++ );
  run->rightx--;
}

FUNCTION int stack_it( bottom, max_runs )
struct RUNS_STRUCT *bottom;
int max_runs;
{
  int status;
  struct RUNS_STRUCT *search;

  status = SUCCESS;
  for (search = run - 1; search >= bottom; search-- ) {
    if (( search->y == run->y) && ( search->leftx == run->leftx )) break;
  }
  if ( search < bottom ) {
    run->popped = 0;
    if ( running > max_runs ) status = MEMORY_ERROR;
    running++;
    run++;
  }
  return( status );
}
