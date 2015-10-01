/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/



/*TDM         CHECKOUT FILE_TIME=11-JUL-1983 20:39 DUA0:[TAEV1.TM]UTIL.C;3 */
/*	IF Stack Manipulation Routines
 *
 *
 *		"TINY" stack manipulation routines. A tiny
 *		stack is an array where stack[STACK_TOP] contains the
 *		index into the array of the current top of stack,
 *		and stack[STACK_SIZE] contains the number of entries allowed in
 *		the stack plus 2.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	14-jul-83	Deleted reference to PRIMINC...dm
 *	25-jul-85	Fix UNIX lint error...dm
 *
 */

#include	"taeconf.inp"	/* TAE configuration definitions		*/

#include	"tmhost.inp"
#include	"tminc.inc"	/* TM-only host-independent definitions		*/
#include "taeintproto.h"


#define	STACK_SIZE	1	/* Offset into stack array of the length of
				   stack */
#define	STACK_TOP	0	/* Offset into stack array of the current
				   top of the stack */

/*	poptin - Pop an element from from a stack, where stack is made of
 *		 "TINY" elements.
 *
 *			
 *	Function return codes:
 *		
 *		(value popped from stack)
 *		STACK_FAULT - (stack is empty)
 *
 */

FUNCTION 	CODE	poptin 
(
    TINY	stack[]	/* IN/OUT: Stack to access */

 )
    {
    FAST TINY sindex;
    FUNINT     value;

    sindex = stack[STACK_TOP];
    if (sindex == STACK_EMPTY)
	return (STACK_FAULT);
    else
	{
	  value = stack[(int) sindex--];	/* Get value from stack */
	stack[STACK_TOP] = sindex;		/* Update stack index pointer in stack */
	return (value);
	}
    }

/*	pushti - Push element from from a stack, where stack is made of
 *		 "TINY" elements.
 *
 *			
 *	Function return codes:
 *		
 *		SUCCESS
 *		STACK_FAULT - (stack is full)
 *
 */

FUNCTION 	CODE	pushti 
(
    TINY	stack[],	/* IN/OUT: Stack to access */
    FUNINT	value	  	/* IN: Value to be pushed onto stack */

 )
    {
    FAST TINY sindex;			

    sindex = stack[STACK_TOP];			/* Get current top of stack */
    if (sindex == (stack[STACK_SIZE] -1))		/* Second element in stack is the length */
	return (STACK_FAULT);
    else
	{
	  stack[(int) ++sindex] = value ;		/* Increment top of stack ...
						   index and push value */
	stack[STACK_TOP] = sindex;		/* Now put updated top of stack ...
					   pointer into stack */
	return (SUCCESS);
	}
    }

/*	stk_in - Initialize an IF Stack of
 *		 "TINY" elements.
 *
 *			
 *	Function return codes:
 *		
 *			None.
 *
 */

FUNCTION 	VOID	stk_in 
(
    TINY	stack[],	/* IN/OUT: Stack to access */
    TINY	s_length,		/* IN: Allowable number of entries on stack */
    TINY	inival		/* IN: Initial value to pushed on the stack */

 )
    {
    FAST TINY status;			

    stack[STACK_TOP] = STACK_EMPTY;		/* Initialize top of stack ptr */
    stack[STACK_SIZE] = s_length + STACK_OVRHD;		/* Initialize size of array,
 *						   where MAXIF is the max # of
 *						   nested IF's. */
    status = pushti (stack, inival);		/* Finally, initialize to true */
    }

/*	toptin - Get value on the top of the specified stack containing
 *		 "TINY" elements.
 *
 *			
 *	Function return codes:
 *		
 *		(value of top of stack)
 *		STACK_FAULT - (stack is empty)
 *
 */

FUNCTION 	CODE	toptin 
(
    TINY	stack[]	/* IN: Stack to access */

 )
    {

    if (stack[STACK_TOP] == STACK_EMPTY)
	return (STACK_FAULT);
    else
      return (stack[(int) stack[STACK_TOP]]);		/* Get value from stack */
    }
