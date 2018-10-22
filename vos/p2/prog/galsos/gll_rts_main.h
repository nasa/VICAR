#ifndef MIPS_GLL_RTS_MAIN_INCLUDED
#define MIPS_GLL_RTS_MAIN_INCLUDED 1

/*				GLL_RTS_MAIN.H
 ******************************************************************************
 *	This file includes the basic data structures for the Galileo
 *	Phase II telemetry records.
 *
 * History:
 * 
 * Date		Reference	Description
 * -----------  --------------	------------------------------------------------
 *  20- 6-1989	N/A		Damon Knight - Original Delivery
 ******************************************************************************
 */

/*
 *=============================================================================
 *	Definitions
 *=============================================================================

#define	BOOM_FLAG_YES		0
#define	BOOM_FLAG_MAYBE		1
#define	BOOM_FLAG_NO		2
#define BOOM_FLAG_UNKNOWN	3
*/

/* SPACECRAFT_CLOCK
 *=============================================================================
 *
 *	    15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *	 1 ! RIM							   | 0
 *         +-------------------------------+				   +
 *	 3 | MOD 91			   |				   | 2
 *	   +-------------------------------+-------------------------------+
 *	 5 | MOD 8			   | MOD 10			   | 4
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *
 *	RIM		Real_time Image Counter. 24 bits, increments every
 *			60-2/3 seconds.	Ranges from 0 to 1677215.
 *
 *	MOD 91		8 bit counter, increments every 2/3 seconds. Ranges
 *			from 0 to 90.
 *
 *	MOD 10		8 bit counter, increments every 66-2/3 milliseconds.
 *			Ranges from 0 to 9.
 *
 *	MOD 8		8 bit counter, increments every 8-1/3 milliseconds.
 *			Ranges from 0 to 7.
 *
 * ============================================================================
 */

typedef	struct
	{
	UINT	rim;				/* Real time image count */
	UBYTE	mod91;				/* mod 91 counter	 */
	UBYTE	mod10;				/* mod 10 counter	 */
	UBYTE	mod8;				/* mod 8 counter	 */
	}
	sclk_typ;

/* EARTH_RECEIVED_TIME & SPACECRAFT EVENT TIME
 *=============================================================================
 *
 *	    15  14  13  12  11  10  9   8   7   6   5   4   3   2   1   0
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *	 1 | day			       | year			   | 0
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *	 3 | millsec			   | minute			   | 2
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *
 *	year			(year - 1900)
 *
 *	day			day of the year
 *
 *	minute			minute of day
 *
 *	millsec			millisecond of minute
 *
 *=============================================================================
 */

typedef	struct
	{
	UWORD			year;		/* YEAR 		     */
	UWORD			day;		/* DAY OF YEAR		     */
	UBYTE			hour;		/* HOUR OF DAY		     */
	UBYTE			minute;		/* MINUTES OF HOUR	     */
	UBYTE			second;		/* SECONDS OF MINUTE	     */
	UWORD			msecond;	/* MILLISECOND OF SECOND     */
	}
	ert_typ;

typedef	ert_typ		scet_typ;

typedef	ert_typ		rct_typ;


typedef	struct
	{
	UINT	rim;				/* Real time image count */
	UBYTE	mod91;				/* mod 91 counter	 */
	UBYTE	mod10;				/* mod 10 counter	 */
	}
	pws_sclk_typ;

typedef pws_sclk_typ    nims_ph2_sclk_typ;

typedef	struct
	{
	UWORD			year;		/* YEAR 		     */
	UBYTE                   month;          /* MONTH OF YEAR             */
        UBYTE                   day;		/* DAY OF MONTH              */
	UBYTE			hour;		/* HOUR OF DAY		     */
	UBYTE			minute;		/* MINUTES OF HOUR	     */
	UBYTE			second;		/* SECONDS OF MINUTE	     */
	UWORD			msecond;	/* MILLISECOND OF SECOND     */
	}
	pws_ert_typ;

typedef	pws_ert_typ		pws_scet_typ;
typedef	pws_ert_typ		nims_ph2_ert_typ;
typedef	pws_ert_typ		nims_ph2_scet_typ;

#define JAN  0
#define FEB  31
#define MAR  28
#define MAR1 29
#define APR  31
#define MAY  30
#define JUN  31
#define JUL  30
#define AUG  31
#define SEP  31
#define OCT  30
#define NOV  31
#define DEC  30

static short int day_tab[2][12] =
{{JAN,
  JAN+FEB,
  JAN+FEB+MAR,
  JAN+FEB+MAR+APR,
  JAN+FEB+MAR+APR+MAY,
  JAN+FEB+MAR+APR+MAY+JUN,
  JAN+FEB+MAR+APR+MAY+JUN+JUL,
  JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG,
  JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP,
  JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT,
  JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV,
  JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV+DEC},
 {JAN,
  JAN+FEB,
  JAN+FEB+MAR1,
  JAN+FEB+MAR1+APR,
  JAN+FEB+MAR1+APR+MAY,
  JAN+FEB+MAR1+APR+MAY+JUN,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP+OCT,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV+DEC},
};

#endif

