/*++
*
* FACILITY:	TM - (TAE TERMINAL MONITOR)
* MODULE:	RM - Resource Usage Monitoring Package
* 
* ABSTRACT:
* 
*	This file contains the procedures that comprise the resource monitoring
*	package.
*
* CHANGE LOG:
*
*	27-jun-83 	Initial Field Release...cb
*	08-dec-83	Remove greetings...peb
* 		JPL modifications
*
*  13-MAR-84 (PEB)	Indent rm display for batch, add trailer line of "-"s
*  15-MAR-84 (PEB)	Extend trailer line clear accross
*
--*/



#include "stdh.inp"		/* standard C definitions		*/

#include "taeconf.inp"		/* TAE configuration definitions	*/
#include "tminc.inc"		/* TM-only host-independent definitions	*/
#include "taskinc.inp"		/* c_ definitions and structures  	*/
#include "symtab.inc"		/* TM symbol table			*/
#include "taeintproto.h"
#include "vicartae.inc"		/* VICAR-specific definitions		*/

/* include files I need for my adaptations -Steve Hwan */
#include <stdio.h>

/* needed for time() */
#include <sys/types.h>

/* needed for gettimeofday() */
#include <sys/time.h>
#include <errno.h>

#if GETRUSAGE_AVAIL
/* the following is needed for getrusage */
#include <sys/resource.h>
#else
/* the following is needed for times on Solaris */
#include <sys/times.h>
#include <limits.h>
#include <time.h>
#endif
#include <string.h>
#include <time.h>

#define USAGE_DEBUG	0

	GLOBAL  int v08rm = 0;			/* source version number */

GLOBAL struct TCB	initcb;		/* global tcb for subprocess */


/* Data Structure Definitions */

typedef struct				/* Define Usage Vector */	
    {
    long		usertim0;	/* real time (usec) */
    long		usertim1;	/* real time (sec) */
    long		systim0;	/* cpu time (usec) */
    long		systim1;	/* cpu time (sec) */
    long		elapse0;	/* elapsed/wall-clock time(usec) */
    long		elapse1;	/* elapsed/wall-clock time(sec) */
#if GETRUSAGE_AVAIL
    int			inblock;	/* block input operations */
    int			outblock;	/* block output operations */
    int			minpageflts;	/* page faults not req. phys I/O */
    int			majpageflts;	/* page faults reqiring phys. I/O */
#endif
    } t_use_vec;

#define	PROC_MAX	9		/* max length for TAE proc name */
#define LEVEL_MAX	20		/* max proc level available for RM */
#define	RM_INDENT_TABS	4		/* no. tabs to indent for batch log*/
#define RM_TRAIL_CHARS	130		/* no dashes in trailer line	*/

/* Global variables and definitions */
   
GLOBAL CODE		run_type;		/* run mode (BATCH etc) */

/* Module-wide variables and definitions */

static long		g_shallow;	/* bottom proc level for accounting */

static long		g_sstart_qt[2];	/* session start time - quadword */
static long		g_pstart_qt[LEVEL_MAX][2];/* proc start time */

static t_use_vec	g_sstart_uv;	/* session start usage vector */
static t_use_vec	g_stotal_uv;	/* session total usage vector */

static t_use_vec	g_pstart_uv[LEVEL_MAX];/* proc start usage vector */
static t_use_vec	g_ptotal_uv[LEVEL_MAX];	/* total proc usage (stop-start) */
static char		g_pname[LEVEL_MAX][PROC_MAX+1];	/* proc name (with EOS) */
/* I changed this one from 21 to 26 - Steve */
static char		g_pstart_datetime[LEVEL_MAX][26]; /* date and time of proc start */

static int		update_child=0;
/* I removed the exit handler.  -Steve */


FUNCTION VOID rm_get_datetime(char datetime[26]);
FUNCTION VOID rm_display_usage(COUNT);
FUNCTION VOID rm_getcum(t_use_vec* p_uv);
FUNCTION VOID rm_time_fields(long, long, long*, long*, long*, long*);
FUNCTION VOID rm_sub_uv(t_use_vec*, t_use_vec*, t_use_vec*);
FUNCTION VOID rm_cpy_uv(t_use_vec*, t_use_vec*);
FUNCTION VOID rm_clear_uv(t_use_vec*);
FUNCTION VOID rm_print_status(char*);
FUNCTION VOID rm_printf(char*, uintptr_t, uintptr_t,uintptr_t,uintptr_t,
			uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t,
			uintptr_t);
FUNCTION CODE rm_autodisplay(COUNT);
FUNCTION VOID rm_fail(char routine[], char action[], long status);

FUNCTION VOID sys_gettim
(/* DESCRIPTION:
*
*	This routine gets the system time in units of seconds and
*	microsecs past Jan 1, 1970.  seconds are stored in start[1] and
*	microsecs are stored in start[0]
*	Note that the VMS system uses the 2 longs to store 1 value
*	representing number of 100 nsec blocks past a different date.
*
*/
long start[2]
 )
{
    struct timeval temp_systime;

    gettimeofday(&temp_systime, NULL);
    start[1] = temp_systime.tv_sec;
    start[0] = temp_systime.tv_usec;
    return;
}


FUNCTION VOID rm_init 
(/*
* DESCRIPTION:
*
*	This routine initializes the RM package.  The following steps are
*	performed:
*
*	- determine if BATCH
*	- set flags based on mode (auto-display, acc-log, acc-level)
*	- save session start values
*	- display initialization message
*/

struct CONTXT		*p_ctx		/* level-0 context block */

 )
{
long		i;
    
if (run_type == BATCH)			/* check for BATCH */
    g_shallow=2;		/* set the shallowest level of proc for */
else				/* which to keep usage statistics, 	*/
    g_shallow=1;

sys_gettim (g_sstart_qt);	/* session start time */
for (i = 0; i < LEVEL_MAX; i++)
    rm_clear_uv (&g_ptotal_uv[i]);	/* zero last-proc uvs */
rm_clear_uv (&g_sstart_uv);		/* clear session start uv for next call */
rm_getcum (&g_sstart_uv);		/* get session start usage vector */
					/* tricky, was 0, now not */
/*** Nominal messages commented out.
 * rm_printf ("\nResource Monitor Initialized");
 * rm_printf ("- Auto Display %s", (g_autodis ? "Enabled" : "Disabled"));
 ****/
return;
}



FUNCTION VOID rm_term (void)
/*
* DESCRIPTION:
*
*	This routine performs session termination processing for the
*	RM package.
*	-I'm only leaving this here so I can avoid making changes to 
*	exits.c if I can avoid it.
*
*/
{

return;
}


FUNCTION VOID rm_pa 
(/*
* DESCRIPTION:
*
*	This routine is called prior to activation of each proc.
*/

struct CONTXT		*p_ctx		/* proc context block */

 )
{
COUNT			level;		/* nesting level of current proc */

level = (*p_ctx).prclevel;
sys_gettim (g_pstart_qt[level]);	/* session start time */
if ((level < LEVEL_MAX) && (level >= g_shallow))
    {
    rm_get_datetime (&g_pstart_datetime[level][0]);/* save proc start date/time string */
    rm_getcum (&g_pstart_uv[level]);		   /* save proc start usage */
    }
}


FUNCTION VOID rm_pt 
(/*
* DESCRIPTION:
*
*	This routine is called after termination of each proc.
*/

struct CONTXT		*p_ctx		/* proc context block */

 )
{
COUNT			level;		/* nested level of proc */
long			quadtime[2];

level = (*p_ctx).prclevel;
if ((level < LEVEL_MAX) && (level >= g_shallow))
    {
    update_child = 1; /* True */
    rm_getcum (&g_stotal_uv);				/* total session use */
/*    update_child = 0;  False */
    rm_sub_uv (&g_stotal_uv,&g_pstart_uv[level],
               &g_ptotal_uv[level]);			/* total proc use */
/* now process real-time field specially */
sys_gettim (quadtime);					/* current time */
if (quadtime[0] < g_pstart_qt[level][0]) {	/* carry from the seconds */
  g_ptotal_uv[level].elapse1 = quadtime[1] - g_pstart_qt[level][1]-1;	/* seconds */
  g_ptotal_uv[level].elapse0 = quadtime[0] +1000000 - g_pstart_qt[level][0];	/* us */
} else {
  g_ptotal_uv[level].elapse1 = quadtime[1] - g_pstart_qt[level][1];	/* seconds */
  g_ptotal_uv[level].elapse0 = quadtime[0] - g_pstart_qt[level][0];	/* useconds */
}
    strcpy (&g_pname[level][0],(*p_ctx).pdf.name);	/* copy proc name */
    if (rm_autodisplay(level))
	rm_display_usage(level);	/* display usage */
    }

}


FUNCTION VOID rm_get_datetime 
(/*
* DESCRIPTION:
*
*	This routine returns the current date and time as a 
*	readable string for display
*/
    char datetime[26]		/* out: date-time string	*/

 )
{
    time_t temp_time;
    int i;

    temp_time = time(NULL);
    strcpy(datetime, ctime( &temp_time));
    for (i=0; i<26; i++)
      if (datetime[i]=='\n')
        datetime[i]='\0';
    datetime[25] = '\0';		/* Guarantee Null terminator */
    return;
}

FUNCTION CODE rm_do_usage 
(/*
* DESCRIPTION:
*
*	This routine is invoked by the intrinsic commands "USAGE"
*	and "USAGE-SHOW".
*
*/

struct CONTXT	*procctx,	/* in: proc context	*/
struct CONTXT	*cmdctx	/* in: cmd context	*/
 )
{
COUNT		level;		/* level or depth of current proc */

level = (*cmdctx).prclevel;
if ((level < LEVEL_MAX) && (level >= g_shallow))
    {
    rm_getcum (&g_stotal_uv);		/* total sessions usage */
    rm_display_usage(level);		/* display usage */
    }
else
    {
      rm_printf("\nProcs nested too deep for USAGE command.", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    }
return 0;
}
    


FUNCTION VOID rm_display_usage 
(/*
* DESCRIPTION:
*
*	This routine displays the usage arrays for the entire session and
*	the last proc executed.
*
*/
COUNT		prclevel		/* nesting level of last proc */

 )
{
t_use_vec	uv1,uv2;
long		hours1,mins1,secs1,cents1;
long		hours2,mins2,secs2,cents2;
char		buf[STRINGSIZ+1];
COUNT		i;

rm_cpy_uv(&g_stotal_uv,&uv1);				/* copy session uv */
rm_cpy_uv(&g_ptotal_uv[prclevel],&uv2);			/* copy proc uv */

 rm_printf ("", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
 rm_printf ("Statistic\t\tSession\t\tLast Proc (%s)",
	    (uintptr_t) &g_pname[prclevel][0],
	    0, 0, 0, 0, 0, 0, 0, 0, 0);
 rm_printf ("---------\t\t-------\t\t---------", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
 rm_printf ("Proc Start Time\t\t\t\t%s", 
	    (uintptr_t) &g_pstart_datetime[prclevel][0],
	    0, 0, 0, 0, 0, 0, 0, 0, 0);
#if GETRUSAGE_AVAIL
 rm_printf ("Block input operations\t%-11d\t%-11d",uv1.inblock,uv2.inblock,
	    0, 0, 0, 0, 0, 0, 0, 0);
 rm_printf ("Block output operations\t%-11d\t%-11d",uv1.outblock,uv2.outblock,
	    0, 0, 0, 0, 0, 0, 0, 0);
rm_printf ("Page Faults(no ph I/O)\t%-11d\t%-11d",uv1.minpageflts,
	   uv2.minpageflts,0, 0, 0, 0, 0, 0, 0, 0);
rm_printf ("Page Faults(phys I/O)\t%-11d\t%-11d",uv1.majpageflts,
	   uv2.majpageflts,0, 0, 0, 0, 0, 0, 0, 0);
#endif

rm_time_fields (uv1.systim0,uv1.systim1,&hours1,&mins1,&secs1,&cents1);
rm_time_fields (uv2.systim0,uv2.systim1,&hours2,&mins2,&secs2,&cents2);
rm_printf ("System CPU Time\t\t%02d:%02d:%02d.%02d\t%02d:%02d:%02d.%02d",
	   (uintptr_t) hours1,(uintptr_t) mins1,(uintptr_t) secs1,
	   (uintptr_t) cents1,
	   (uintptr_t) hours2, (uintptr_t) mins2, (uintptr_t) secs2,
	   (uintptr_t) cents2, 0, 0);

rm_time_fields (uv1.usertim0,uv1.usertim1,&hours1,&mins1,&secs1,&cents1);
rm_time_fields (uv2.usertim0,uv2.usertim1,&hours2,&mins2,&secs2,&cents2);
rm_printf ("User CPU Time\t\t%02d:%02d:%02d.%02d\t%02d:%02d:%02d.%02d",
	   (uintptr_t) hours1,(uintptr_t) mins1,(uintptr_t) secs1,
	   (uintptr_t) cents1,
	   (uintptr_t) hours2, (uintptr_t) mins2, (uintptr_t) secs2,
	   (uintptr_t) cents2, 0, 0);

rm_time_fields (uv1.elapse0,uv1.elapse1,&hours1,&mins1,&secs1,&cents1);
rm_time_fields (uv2.elapse0,uv2.elapse1,&hours2,&mins2,&secs2,&cents2);
rm_printf ("Connect Time\t\t%02d:%02d:%02d.%02d\t%02d:%02d:%02d.%02d",
	   (uintptr_t) hours1,(uintptr_t) mins1,(uintptr_t) secs1,
	   (uintptr_t) cents1,
	   (uintptr_t) hours2, (uintptr_t) mins2, (uintptr_t) secs2,
	   (uintptr_t) cents2, 0, 0);

if (run_type != INTER)
    {
    buf[0] = EOS;
    for (i = 0; i < RM_TRAIL_CHARS; i++)
	s_append("-", buf);
    put_stdout(buf);    			/* trailer of "-"s	*/
    }
 rm_printf ("", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

}



FUNCTION VOID rm_getcum 
(/*
* DESCRIPTION:
*
*	This routine returns the sum of the cumulative resource usages for
* 	both the current process and the current sub-process, from the
*	start of the current session.
*
* NOTES:
*
*  1)	g_sstart_uv and g_sstart_qt must be setup prior to the first call
*	to this routine.
*
*/

t_use_vec	*p_uv		/* pointer to returned uv	*/

 )
{
struct		item			/* define item for $GETJPI call	*/
    {
    unsigned short	len;		/* length of buffer (bytes)	*/
    unsigned short	code;		/* item code			*/
    unsigned long	*addr;		/* buffer address		*/
    unsigned long	*retlenaddr;	/* addr of returned len longword */
    };

long			quadtime[2];
#if GETRUSAGE_AVAIL
int			errors;
static struct rusage	timeself, timechild;
#else
clock_t			errors;
struct tms			usage_tms;
#endif

static t_use_vec	uv1,uv2;	/* local uv's		*/

/* pid = initcb.pid; */
/* printf("pid is %d\n",initcb.pid); */

#if GETRUSAGE_AVAIL
errors = getrusage(RUSAGE_SELF,&timeself);
/* need to convert from timeself to uv and load uv vector. */
uv1.usertim1 = timeself.ru_utime.tv_sec;
uv1.usertim0 = timeself.ru_utime.tv_usec;
uv1.systim1 = timeself.ru_stime.tv_sec;
uv1.systim0 = timeself.ru_stime.tv_usec;
uv1.inblock = timeself.ru_inblock;
uv1.outblock = timeself.ru_oublock;
uv1.minpageflts = timeself.ru_minflt;
uv1.majpageflts = timeself.ru_majflt;
if (errors) rm_fail ("rm_getcum","getrusage(self)",(long)errors);

if (update_child) {
  errors = getrusage(RUSAGE_CHILDREN,&timechild);
  /* need to convert from timechild to uv and load uv vector. */
  uv2.usertim1 = timechild.ru_utime.tv_sec;
  uv2.usertim0 = timechild.ru_utime.tv_usec;
  uv2.systim1 = timechild.ru_stime.tv_sec;
  uv2.systim0 = timechild.ru_stime.tv_usec;
  uv2.inblock = timechild.ru_inblock;
  uv2.outblock = timechild.ru_oublock;
  uv2.minpageflts = timechild.ru_minflt;
  uv2.majpageflts = timechild.ru_majflt;
  if (errors) rm_fail ("rm_getcum","getrusage(child)",(long)errors);
}
#else

errors= times(&usage_tms);
uv1.usertim1 = (long) (usage_tms.tms_utime*1000/CLOCKS_PER_SEC) / 1000;
uv1.usertim0 = (long) (usage_tms.tms_utime*1000/CLOCKS_PER_SEC) % 1000;
uv1.systim1 = (long) (usage_tms.tms_stime*1000/CLOCKS_PER_SEC) / 1000;
uv1.systim0 = (long) (usage_tms.tms_stime*1000/CLOCKS_PER_SEC) % 1000;
if (update_child) {
  uv2.usertim1 = (long) ((usage_tms.tms_utime-usage_tms.tms_cutime)
				*1000/CLOCKS_PER_SEC) / 1000;
  uv2.usertim0 = (long) ((usage_tms.tms_utime-usage_tms.tms_cutime)
				*1000/CLOCKS_PER_SEC) % 1000;
  uv2.systim1 = (long) ((usage_tms.tms_stime-usage_tms.tms_cstime)
				*1000/CLOCKS_PER_SEC) / 1000;
  uv2.systim0 = (long) ((usage_tms.tms_stime-usage_tms.tms_cstime)
				*1000/CLOCKS_PER_SEC) % 1000;
}

#endif

rm_sub_uv (&uv1,&g_sstart_uv,p_uv);		/* subtract initial */

/* now process real-time field specially */
sys_gettim (quadtime);					/* current time */
/* delta real time - not sure why I'm not just using rm_sub_uv */
if (quadtime[0] < g_sstart_qt[0]) {	/* carry from the seconds */
  p_uv->elapse1 = quadtime[1] - g_sstart_qt[1]-1;	/* seconds */
  p_uv->elapse0 = quadtime[0] +1000000 - g_sstart_qt[0];	/* useconds */
} else {
  p_uv->elapse1 = quadtime[1] - g_sstart_qt[1];	/* seconds */
  p_uv->elapse0 = quadtime[0] - g_sstart_qt[0];	/* microseconds */
}

return;
}

FUNCTION VOID rm_time_fields 
(/*
* DESCRIPTION:
*
*	This routine returns the time fields given a time
*	an array containing time[0] = microsecs, time[1]=seconds
*
*/

long	time0,				/* time microsec part */
long	time1,				/* time sec part */
long	*p_hours,	/* time fields */
long    *p_mins,
long    *p_secs,
long    *p_cents
 )
{
*p_cents = time0 / 10000;		/* get number of centiseconds */
*p_secs = time1 % 60;
time1 /= 60;				/* put time in units of minutes */
*p_mins = time1 % 60;
*p_hours = time1 / 60;

return;
}


FUNCTION VOID rm_add_uv 
(/*
* DESCRIPTION:
*
*	This routine adds two usage vectors.
*	Note that elapsed time is only handled by getcum and rm_pt, and
*	we don't need to worry about it in this routine.
*
*/

t_use_vec	*uv1,
t_use_vec       *uv2,
t_use_vec       *result

 )
{

#if USAGE_DEBUG
printf("rm_add_uv\n");
printf("uv1 user : [%ld , %ld]\n",uv1->usertim1,uv1->usertim0);
printf("uv2 user : [%ld , %ld]\n",uv2->usertim1,uv2->usertim0);
printf("uv1 sys : [%ld , %ld]\n",uv1->systim1,uv1->systim0);
printf("uv2 sys : [%ld , %ld]\n",uv2->systim1,uv2->systim0);
#endif
result->usertim0 = uv1->usertim0 + uv2->usertim0;
result->usertim1 = uv1->usertim1 + uv2->usertim1;
#if USAGE_DEBUG
printf("result user : [%ld , %ld]\n",result->usertim1,result->usertim0);
printf("result sys : [%ld , %ld]\n",result->systim1,result->systim0);
#endif
if (result->usertim0 > 1000000) {
    result->usertim0 -= 1000000;
    result->usertim1 ++;
  }
result->systim0 = uv1->systim0 + uv2->systim0;
result->systim1 = uv1->systim1 + uv2->systim1;
if (result->systim0 > 1000000) {
  result->systim0 -= 1000000;
  result->systim1 ++;
  }
#if GETRUSAGE_AVAIL
result->inblock = uv1->inblock + uv2->inblock;
result->outblock = uv1->outblock + uv2->outblock;
result->minpageflts = uv1->minpageflts + uv2->minpageflts;
result->majpageflts = uv1->majpageflts + uv2->majpageflts;
#endif

return;
}


FUNCTION VOID rm_sub_uv 
(/*
* DESCRIPTION:
*
*	This routine subtracts two usage vectors, so that
*	result = uv1 - uv2.
*	Note that elapsed time is only handled by getcum and rm_pt, and
*	we don't need to worry about it in this routine.
*
*/

t_use_vec	*uv1,
t_use_vec *uv2,
t_use_vec *result

 )
{
#if USAGE_DEBUG
printf("rm_sub_uv\n");
printf("uv1 user : [%ld , %ld]\n",uv1->usertim1,uv1->usertim0);
printf("uv2 user : [%ld , %ld]\n",uv2->usertim1,uv2->usertim0);
printf("uv1 sys : [%ld , %ld]\n",uv1->systim1,uv1->systim0);
printf("uv2 sys : [%ld , %ld]\n",uv2->systim1,uv2->systim0);
#endif
if (uv1->usertim0 < uv2->usertim0) {	/* carry from the seconds */
  result->usertim0 = uv1->usertim0 + 1000000 - uv2->usertim0;
  result->usertim1 = uv1->usertim1 - uv2->usertim1 -1;
} else {
  result->usertim0 = uv1->usertim0 - uv2->usertim0;
  result->usertim1 = uv1->usertim1 - uv2->usertim1;
}
if (uv1->systim0 < uv2->systim0) {	/* carry from the seconds */
  result->systim0 = uv1->systim0 +1000000 - uv2->systim0;
  result->systim1 = uv1->systim1 - uv2->systim1 -1;
} else {
  result->systim0 = uv1->systim0 - uv2->systim0;
  result->systim1 = uv1->systim1 - uv2->systim1;
}
#if USAGE_DEBUG
printf("result user : [%ld , %ld]\n",result->usertim1,result->usertim0);
printf("result sys : [%ld , %ld]\n",result->systim1,result->systim0);
#endif
#if GETRUSAGE_AVAIL
result->inblock = uv1->inblock - uv2->inblock;
result->outblock = uv1->outblock - uv2->outblock;
result->minpageflts = uv1->minpageflts - uv2->minpageflts;
result->majpageflts = uv1->majpageflts - uv2->majpageflts;
#endif

return;
}

FUNCTION VOID rm_cpy_uv 
(/*
* DESCRIPTION:
*
*	This routine copies vector uv1 into uv2.
*
*/

t_use_vec	*uv1,
t_use_vec *uv2

 )
{
uv2->usertim0 = uv1->usertim0;
uv2->usertim1 = uv1->usertim1;
uv2->systim0 = uv1->systim0;
uv2->systim1 = uv1->systim1;
uv2->elapse0 = uv1->elapse0;
uv2->elapse1 = uv1->elapse1;
#if GETRUSAGE_AVAIL
uv2->inblock = uv1->inblock;
uv2->outblock = uv1->outblock;
uv2->minpageflts = uv1->minpageflts;
uv2->majpageflts = uv1->majpageflts;
#endif

return;
}


FUNCTION VOID rm_clear_uv 
(/*
* DESCRIPTION:
*
*	This routine clears a usage vector.
*
*/

t_use_vec	*uv

 )
{
uv->usertim0 = 0;
uv->usertim1 = 0;
uv->systim0 = 0;
uv->systim1 = 0;
uv->elapse0 = 0;
uv->elapse1 = 0;
#if GETRUSAGE_AVAIL
uv->inblock = 0;
uv->outblock = 0;
uv->minpageflts = 0;
uv->majpageflts = 0;
#endif

return;
}



FUNCTION VOID rm_fail 
(/*
* DESCRIPTION:
*
*	This routine is called to report a fatal error.
*
*/

char	routine[],
char    action[],
long	status
 )
{
  rm_printf ("", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  rm_printf ("*** RESOURCE MONITOR INTERNAL ERROR ***", 0, 0, 0, 0, 0, 
	     0, 0, 0, 0, 0);
  rm_printf ("- IN ROUTINE %s, DURING %s",(uintptr_t) routine, 
	     (uintptr_t) action,
	     0, 0, 0, 0, 0, 0, 0, 0);
  rm_print_status ("- ");
  rm_printf ("",0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  exit (status);
  return;
}



FUNCTION VOID rm_print_status 
(/*
* DESCRIPTION:
*
*	This routine decodes and displays a VMS error code.
*
*/

char		*pstr
 )
{
 rm_printf ("Error %X (hex) encountered",(uintptr_t) errno, 0, 0, 0, 0, 0, 
	    0, 0, 0, 0);
 return;
}



FUNCTION VOID rm_printf 
(		
/*
* DESCRIPTION:
*
*	This routine formats a record using "PRINTF" conventions, then
*	sends it to the "standard output" device.  The record is
*	written to the standard output using "single space" carriage control.
*
*/

char		*p_fmt,
uintptr_t		arg1,
uintptr_t arg2, uintptr_t arg3, uintptr_t arg4,uintptr_t arg5, uintptr_t arg6, uintptr_t arg7,
uintptr_t arg8, uintptr_t arg9, uintptr_t arg10
)
{
char		record[200];			/* local record buffer */
char		record2[200+RM_INDENT_TABS];
COUNT		i;

    sprintf (record,p_fmt,arg1,arg2,arg3,arg4,arg5,
			arg6,arg7,arg8,arg9,arg10);	/* format record */
    if (run_type != INTER)				/* for BATCH...	*/
	{						/* indent	*/
	for (i = 0; i < RM_INDENT_TABS; i++)
	    record2[i] = '	';			/* TAB char	*/
	record2[i] = EOS;
	s_append(record, record2);
	put_stdout(record2);
	}
    else
	put_stdout (record);				/* output record */
    return;
}



FUNCTION CODE rm_autodisplay
(/* rm_autodisplay
 *
 * Determines whether to autodisplay the usage stats for a given
 * level of proc nesting.  Returns TRUE if stats should be displayed.
 *
 * Stats are displayed based on the value of the $AUTOUSAGE global variable.
 * If $AUTOUSAGE=="ALL", then stats are always displayed.  If it is "BATCH",
 * then stats are displayed if we are running in batch AND the current
 * proc level is being echoed to the batch log.  If $AUTOUSAGE is "NONE",
 * stats are never automatically displayed.  The default is "BATCH".
 */
COUNT prclevel		/* level for which autodisplay is to be determined */

 )
{
IMPORT struct VARIABLE  *becho_gbl, *autousage_gbl;
TEXT			flag, autoflag;
COUNT			index;

if (autousage_gbl == NULL)
    autoflag = 'B';
else
    autoflag = *(SVAL(*autousage_gbl, 0));

if (autoflag == 'N') return (FALSE);		/* don't autodisp */
if (autoflag == 'A') return (TRUE);		/* always autodisp */

if (run_type != BATCH) return (FALSE);		/* autodisp only for BATCH */

prclevel--;		/* adjust prclevel to correspond to becho index */

if (prclevel > (*becho_gbl).v_count)		/* if echo short... */
    index = (*becho_gbl).v_count - 1;		/* use last	    */
else
    index = prclevel  - 1;
						/* use first character  */
flag = *(SVAL(*becho_gbl, index));		/* of echo global.  if  */
if (flag == 'F' || flag == 'Y' ||		/* echoing, give usage  */
    flag == 'B' || flag == 'T')			/* stats		*/
    return (TRUE);				

return (FALSE);
}
