/**
 **  ibisglobals.h
 **
 **  Global variables for IBIS subroutine library.
 **/
 
#ifndef _H_IBISGLOBALS
#define _H_IBISGLOBALS

/**
 **  Global Variables
 **/

extern List     *x_ibis;
extern char     *format_name[];
extern int	format_size[];
extern char	*format_label[];
extern char	*_ibis_current_module;
extern int 	default_format;

/* ID tables for external interface.  See ibisdefines.h. */

#if POINTERS_64_BITS_OS
extern XIBIS *_ibis_id_table[];
extern XREC *_ibis_rec_id_table[];
#endif

#endif /* _H_IBISGLOBALS */

