#include "stdh.inp"		/* system standard  (REQUIRED)		*/
#include "taeconf.inp"		/* TAE configuration (REQUIRED)		*/
#include "symtab.inc"		/* TM symbol table			*/
#include "tmhost.inp"		/* TM host-dependent definitions	*/
#include "tminc.inc"		/* TM definitions			*/

#include "vicartae.inc"		/* VICAR-specific definitions		*/

/*	flag_do.   Intrinsic flag processing.
 *
 */
/* These are the bits in the global $SWITCH that aren't in taeconf.inp	*/

#define SW_NOMESSAGE 0x200	/* Suppress "Beginning VICAR task..." message */

#define SW_P0 0x01000000  	/* Programmer's Byte: Bit 0 */
#define SW_P1 0x02000000  	/* Programmer's Byte: Bit 1 */
#define SW_P2 0x04000000  	/* Programmer's Byte: Bit 2 */
#define SW_P3 0x08000000  	/* Programmer's Byte: Bit 3 */
#define SW_P4 0x10000000  	/* Programmer's Byte: Bit 4 */
#define SW_P5 0x20000000  	/* Programmer's Byte: Bit 5 */
#define SW_P6 0x40000000  	/* Programmer's Byte: Bit 6 */
#define SW_P7 0x80000000  	/* Programmer's Byte: Bit 7 */

    IMPORT struct VARIABLE *switch_gbl;
    struct VARIABLE *lookex();

    FUNCTION CODE flag_do (procctx, cmdctx)
    
    struct CONTXT *procctx;	/* in/out: proc context		*/
    struct CONTXT *cmdctx;	/* in/out: command context	*/

    {
    int i, mask, flag_count;
    char *flags;
    struct VARIABLE *flags_var, *v;

    if (s_equal((*cmdctx).subcmd, "SHOW"))	/* Special routine to	  */
	{					/* handle the SHOW subcmd */
	show_flags(procctx, cmdctx);
	return(DO_CHECK);
	}

    flags_var = lookex(&(*cmdctx).parmst, "FLAGS");
    flag_count = (*flags_var).v_count;

    mask = 0;
    for (i = 0; i < flag_count; i++)
	{
	flags = SVAL(*flags_var, i);
	switch (*flags)				/* Convert the symbolically */
	    {					/* requested flags to a bit */
	    case 'D' : mask |= SW_DEBUG; break;	/* representation	    */
	    case 'L' : mask |= SW_SYSOUTS; break;
	    case 'A' : mask |= SW_ASYNC_CMD; break;
	    case 'T' : mask |= SW_DYN_LIB; break;
	    case 'S' : mask |= SW_SYNTAX; break;
	    case 'N' : mask |= SW_NOMESSAGE; break;

    	    case 'P' : 
		switch (*(flags+1))
		    {
		    case '0' : mask |= SW_P0; break;
		    case '1' : mask |= SW_P1; break;
		    case '2' : mask |= SW_P2; break;
		    case '3' : mask |= SW_P3; break;
		    case '4' : mask |= SW_P4; break;
		    case '5' : mask |= SW_P5; break;
		    case '6' : mask |= SW_P6; break;
		    case '7' : mask |= SW_P7; break;
		    }; break;
	    }
	}

/* Now perform the desired action.  Three subcommands are possible,
 * ADD, DELETE, and SET.  The actions performed are:
 *
 *	ADD	-- Add the reqested bits to the existing value of
 *		   $SWITCH (bitwise OR).
 *	DELETE	-- Clear the reqested bits from $SWITCH.
 *	SET	-- Set $SWITCH to only the requested bits, ignoring
 *		   any previous settings.
 */
    switch (*(*cmdctx).subcmd)
	{
	case 'A' : IVAL(*switch_gbl, 0) |= mask; break;		/* ADD	   */
	case 'D' : IVAL(*switch_gbl, 0) &= ~mask; break;	/* DELETE  */
	default : IVAL(*switch_gbl, 0) = mask; break;		/* SET	   */
	}
    return (DO_CHECK);
    }

/* show_flags -- Display the current settings of the $SWITCH global
 */
    FUNCTION CODE show_flags (procctx, cmdctx)
    
    struct CONTXT *procctx;	/* in/out: proc context		*/
    struct CONTXT *cmdctx;	/* in/out: command context	*/

    {
    int switch_val;

    switch_val = IVAL(*switch_gbl, 0);

    if (switch_val == 0)
	{
	put_stdout("You have no flags set at this time.");
	return(DO_CHECK);
	}

    put_stdout("Current flags set in $SWITCH global:");

    if (switch_val & SW_DEBUG)
	put_stdout("\tDEBUG\t\t-- Run application programs in debug mode");
    if (switch_val & SW_SYSOUTS)
	put_stdout("\tLOGGING\t\t-- Provide full logging for async jobs");
    if (switch_val & SW_ASYNC_CMD)
	put_stdout("\tADEBUG\t\t-- Run async jobs in a special debug mode");
    if (switch_val & SW_DYN_LIB)
	put_stdout("\tTUTLIB\t\t-- Display library names when in TUTOR");
    if (switch_val & SW_SYNTAX)
	put_stdout("\tSYNTAX\t\t-- Run in syntax check mode");
    if (switch_val & SW_NOMESSAGE)
	put_stdout("\tNOMESSAGE\t-- Suppress \"Beginning VICAR task...\" message");
    if (switch_val & SW_P0)
	put_stdout("\tP0\t\t-- See application reference manual for effect");
    if (switch_val & SW_P1)
	put_stdout("\tP1\t\t-- See application reference manual for effect");
    if (switch_val & SW_P2)
	put_stdout("\tP2\t\t-- See application reference manual for effect");
    if (switch_val & SW_P3)
	put_stdout("\tP3\t\t-- See application reference manual for effect");
    if (switch_val & SW_P4)
	put_stdout("\tP4\t\t-- See application reference manual for effect");
    if (switch_val & SW_P5)
	put_stdout("\tP5\t\t-- See application reference manual for effect");
    if (switch_val & SW_P6)
	put_stdout("\tP6\t\t-- See application reference manual for effect");
    if (switch_val & SW_P7)
	put_stdout("\tP7\t\t-- See application reference manual for effect");

    return(DO_CHECK);
    }
