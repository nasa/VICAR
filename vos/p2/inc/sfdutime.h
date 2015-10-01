/*===========================================================================*
 |  SFDUTIME.H								     |
 |	Standard SDR and SFDU data structures for time and spacecraft clock  |
 |                                                                           |
 |  Cognizant Programmer: Damon D. Knight                                    |
 |                                                                           |
 |  Revision    History                                                      |
 |    Date        FR#        Description                                     |
 |  --------    -------      ---------------------------------------------   |
 |  08/17/93      N/A        DDK - Ported to Unix (Didn't Change Anything)   |
 |                                                                           |
 *===========================================================================*/

struct binary_date
	{
	short year;		/* year of century (0-99)	*/
	short day;		/* day of year (1-366) 		*/
	short hour;		/* hour of day (0-23)		*/
	short minute;		/* minute of hour (0-59)	*/
	short second;		/* second of minute (0-59)	*/
	short msec;		/* millisecond (0-999)		*/
	};

struct rct_time
	{
	char day;
	char month;
	char minute;
	char hour;
	};

struct rct_time_plus
	{
	char year;
	char second;
	char day;
	char month;
	char minute;
	char hour;
  	short filler;
	};

struct sfdu_time		/* 1950 binary epoch time format */
	{
	unsigned short secs_since_1950_lsb;	/* seconds-since-1950 */
	unsigned short secs_since_1950_msb;	/* seconds-since-1950 */
	unsigned short fractional_seconds;	/* fractional-seconds */
	};

struct sfdu_time_int	/* same as sfdu_time, except seconds are 32 bit */ 
	{
	unsigned int seconds_since_1950;	/* seconds-since-1950 */
	unsigned short fractional_seconds;	/* fractional-seconds */
	};

struct sdr_time			/* SDR time format */
	{
	short hour_of_year;	/* hour-of-year */
	short second_of_hour;	/* seconds-into-hour */
	short msec_of_second;	/* milliseconds-into-second */
	};

struct sc_clock		/*** VGR Flight Data System Counter ***/
	{
	unsigned short mod16;	/* Mod16 counter (0-65535)		     */
	unsigned line	:10;	/* Line count (1-800), overflows into mod60  */
	unsigned mod60	:6;	/* Mod60 counter (0-59), overflows into mod16*/
	short unused;
	};
