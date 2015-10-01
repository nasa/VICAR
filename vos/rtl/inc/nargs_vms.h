/************************************************************************/
/* THIS FILE MUST NEVER BE INCLUDED IN APPLICATION CODE!		*/
/*									*/
/* It is used only by the RTL routines that accepted pure optional	*/
/* arguments before they were ported.  They must still accept pure	*/
/* optionals for backwards compatibility, until everything is ported.	*/
/* This file is used for the Alpha to set up n_args() to work properly	*/
/* (the real n_args() function is VAX-specific).			*/
/************************************************************************/

#if ALPHA_ARCH
#include <varargs.h>
#define n_args() (__VA_COUNT_BUILTIN())
#endif

