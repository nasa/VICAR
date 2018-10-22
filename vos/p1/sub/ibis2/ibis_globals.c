#include "ibis.h"
#include <string.h>
#include <stdio.h>

/* Global variables for XIBIS module */

static List	_ibis_list;
List* 		x_ibis = &_ibis_list;
int 		default_format=FMT_REAL;
int		format_size[FMT_LAST+1];
static char	format_nameX[FMT_LAST+1][IFMT_SIZE];
char		*format_name[FMT_LAST+2];
char		*_ibis_current_module="NO MODULE";
char *format_label[] = {
		 "FMT_*ERROR*",
		 "FMT_NONE",
		 "FMT_BYTE",
		 "FMT_HALF",
		 "FMT_FULL",
		 "FMT_REAL",
		 "FMT_DOUB",
		 "FMT_COMP",
		 "FMT_ASCII",
		 (char *)0L
};

/* ID tables for external interface.  See ibisdefines.h. */

#if POINTERS_64_BITS_OS
XIBIS *_ibis_id_table[MAX_NUM_IBIS_IDS];
XREC *_ibis_rec_id_table[MAX_NUM_IBIS_REC_IDS];
#endif


int _init_ibis_globals(void)
{
	static int first_time=1;
	int i;

	if (!first_time) return 1;

	/* initialize ibis list */
	x_ibis->destruct = (void(*)(list_value))_i_free_ibis;
	
	/* initialize the format name converters */

	for (i = FMT_BASE; i<=FMT_ASCII; i++)
	{
		strcpy(format_nameX[i],(format_label[i] + 4));
		format_name[i] = (char *)format_nameX[i];
	}

	/* initialize format size array and ASCII names */

	format_size[FMT_BASE] = 	0;
	format_size[FMT_NONE] = 	0;
	format_size[FMT_BYTE] =		sizeof( char );
	format_size[FMT_HALF] =		sizeof( short );
	format_size[FMT_FULL] =		sizeof( int );
	format_size[FMT_REAL] =		sizeof( float );
	format_size[FMT_DOUB] =		sizeof( double );
	format_size[FMT_COMP] =		2*sizeof( float );
	format_size[FMT_ASCII] =	8*sizeof( char ); /* just in case ... */
	for (i=FMT_ASCII+1; i<=FMT_LAST; i++)
	{
		format_size[i] = i+1-FMT_ASCII;
		sprintf(format_nameX[i],"A%-d",i-FMT_ASCII);
		format_name[i] = (char *)format_nameX[i];
	}
        format_name[FMT_LAST+1] = (char *)0;

	
	/* Initialize the ID tables for 64-bit OS's. */

#if POINTERS_64_BITS_OS
	memset(_ibis_id_table, 0, sizeof(_ibis_id_table));
	memset(_ibis_rec_id_table, 0, sizeof(_ibis_id_table));
#endif

	first_time=0; /* dont do this again ! */
	
	return (1);
}
