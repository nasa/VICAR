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

#include "taeconf.inp"
#include "parblk.inc"
#include "syninc.inc"
#include "taeintproto.h"

#ifdef ASSEMBLE
#ifdef sun
	.data
	.even
	.text

	.data
	.text
	.globl	_s_append
_s_append:
	link	a6,#-8		| initialization
	moveml	a4-a5,sp@	| save the a4, a5 registers
	movl	a6@(8),a5	|s
	movl	a6@(12),a4	|t
NEXTBYTE:
	tstb	a4@+		| look for end of 1st string
	bne	NEXTBYTE
	subql	#1,a4		| dont count the EOS
NEXTCHAR:
	movb	a5@+,a4@+	| Move each byte from the source string (s)
	bne	NEXTCHAR
	movl	a4,d0		| Calculate new length
	subl	a6@(12),d0
	subql	#1,d0
	moveml	a6@(-8),a4-a5	| restore registers
	unlk	a6
	rts

	.data
	.text
	.globl	_s_bcopy
_s_bcopy:
	link	a6,#-8		| initialization
	moveml	a4-a5,sp@	| Push reg onto stack
	movl	a6@(12),a4	| outstr
	movl	a6@(8),a5	| instr
	movl	a6@(16),d0	| siz
	bra	CHECK_COUNT
MOVENEXTB:
	movb	a5@+,a4@+
	beq	DONE
CHECK_COUNT:
	dbra	d0,MOVENEXTB
	clrb	a4@		| force EOS
DONE:
	extl	d0		| calc actual number copied
	subl	a6@(16),d0
	negl	d0
	subql	#1,d0
	moveml	a6@(-8),a4-a5
	unlk	a6
	rts

	.data
	.text
	.globl	_s__copy
_s__copy:
	link	a6,#-8		| Initialization
	moveml	a4-a5,sp@	| save regs to stack
	movl	a6@(8),a5	| s
	movl	a6@(12),a4	| t
NEXTB:
	movb	a5@+,a4@+
	bne	NEXTB
	movl	a5,d0		| Calc length
	subl	a6@(8),d0
	subql	#1,d0
	moveml	a6@(-8),a4-a5	| restore regs
	unlk	a6
	rts

	.data
	.text
	.globl	_s_length
_s_length:
	link	a6,#-4		| initialize
	movl	a5,sp@		| save reg on stack
	movl	a6@(8),a5	| s
NEXTBL:
	tstb	a5@+
	bne	NEXTBL
	movl	a5,d0
	subl	a6@(8),d0
	subql	#1,d0
	movl	sp@,a5		| restore regs
	unlk	a6
	rts

	.data
	.text
	.globl	_bytmov
_bytmov:
	link	a6,#-12		| Initialize
	moveml	d0/a4-a5,sp@	| save d0,a4,a5 registers
	movl	a6@(8),a5	| from
	movl	a6@(12),a4	| to
	movl	a6@(16),d0	| bytes
	bra	LOOP
MOVENEXT:
	movb	a5@+,a4@+
LOOP:
	dbra	d0,MOVENEXT
	moveml	a6@(-12),d0/a4-a5 | restore d0,a4,a5 registers
	unlk	a6
	rts

	.data
	.text
	.globl	_zero_block
_zero_block:
	link	a6,#-8		| Initialize
	moveml	d0/a5,sp@	| save d0,a5 registers
	movl	a6@(8),a5	| block
	movl	a6@(12),d0	| bytes
	bra	CHECKCNT
CLRNEXT:
	clrb	a5@+
CHECKCNT:
	dbra	d0,CLRNEXT
	moveml	a6@(-8),d0/a5	| restore d0,a5
	unlk	a6
	rts
#endif

#ifdef vax
.text
/*****************************
.title	stringmar
.ident	/103/
;	C callable string package subroutines that are performance critical.
;
;
;	s_append - Subroutine to append a C string (s) to another C string (t).
;
;	Example call from C program:
;
;	len = s_append (s, t)
;		      4	 8
;	where
;
;	char	*s;		in:  string to append
;	char	*t;		out: string to append to
;
;	and where "len" is a COUNT variable.
;
;
**********************/
    .globl	_s_append
_s_append:
    .word	0x7c
    locc	$0,$32767,*8(ap)	#FIND END OF T
    movl	r1,r6			#SAVE ADDR OF Ts eos
    locc	$0,$32767,*4(ap)	#FIND END OF S
    subl2	4(ap),r1		#r1 = LENGTH OF S
    incl	r1			#SO WE MOVE eos ALSO
    movc3	r1,*4(ap),(r6)		#APPEND S TO T
    subl3	8(ap),r3,r0		#r0 = LENGTH + 1 OF DESTINATION
    decl	r0			#RETURN LENGTH OF RESULT TO CALLER
    ret
/*   */
/*********************
;
;	s_bcopy - Subroutine to copy (bounded) a C string from
;	source to destination.
;
;	Example call from C program:
;
;	len = s_bcopy (source, dest, siz)
;			4	8	12
;	where
;
;	char	*source;	in:  source string		
;	char	*dest;		out: destination string		
;	FUNINT	siz;		in:  max length of output string
;
;	and where "len" is a COUNT variable.
;
;
**********************/
    .globl	_s_bcopy
_s_bcopy:
    .word	0x7c
    locc	$0,12(ap),*4(ap)	#FIND THE eos
    subl2	4(ap),r1		#CALCULATE THE LENGTH
    movl	r1,r6			#SAVE LENGTH FOR RETURN
    movc3	r1,*4(ap),*8(ap)	#COPY THE STRING
    clrb	(r3)			#eos ON END OF STRING
    movl	r6,r0			#RETURN THE LENGTH
    ret
/*  */
/*************************
;
;	s_copy - Subroutine to copy a C string from source to destination.
;
;	Example call from C program:
;
;	len = s_copy (source, dest)
;			4	8
;	where
;
;	char	*source;	in:  source string	
;	char	*dest;		out: destination string	
;
;	and where "len" is a COUNT variable.
;
;
********************/
    .globl	_s__copy
_s__copy:
    .word	0x7c
    locc	$0,$32767,*4(ap)	#FIND THE eos
    subl2	4(ap),r1		#CALCULATE THE LENGTH
    movl	r1,r6
    incl	r1			#COPY THE EOS ALSO
    movc3	r1,*4(ap),*8(ap)	#COPY THE STRING
    movl	r6,r0			#RETURN THE LENGTH
    ret
/*  */
/**********************
;
;	s_length - Subroutine to return the length of a C string.
;
;	Example call from C program:
;
;	len = s_length (str);
;
;	where
;
;	char	*str;		in:  string to find length of
;
;	and where "len" is a COUNT variable.
;
;
************************/
    .globl	_s_length
_s_length:
    .word	0x3c
    locc	$0,$32767,*4(ap)	#FIND THE eos
    subl2	4(ap),r1		#CALCULATE THE LENGTH
    movl	r1,r0
    ret

/*  */
/**************************
;
;	Subroutine to move a contiguous block of bytes from source to
;	destination.
;
;	Example call from C program:
;
;	bytmov (source, dest, numbyt);
;		4	8	12
;	where
;
;	char	*source;	in:  start of source for bytes	
;	char	*dest;		out: start of destination for bytes
;	FUNINT	numbyt;		in:  number of bytes to move
;
;
***************************/
    .globl	_bytmov
_bytmov:
    .word	0x3c
    movc3	12(ap),*4(ap),*8(ap)
    ret
/*  */
/*************************
;
;	zero_block. Subroutine to zero-fill the supplied block.
;
;	Example call from C program:
;
;	zero_block(block, numbyt)
;		   4      8
;	where
;
;	GENPTR	block;		in:  start of block to zero fill  
;	FUNINT	numbyt;		in:  number of bytes to zero fill 
;
******************************/    
    .globl	_zero_block
_zero_block:
    .word	0x3c
    movc5	$0,_zero_block,$0,8(ap),*4(ap)
    ret
#endif

#else

/*	String functions which are optimized in
	assembly language on some hosts
*/
#include	"stdh.inp"
#include	"taeconf.inp"
#include	"expinc.inc"		/* expression evaluation support	*/
#include	"symtab.inc"		/* symbol table definitions		*/
#include	"syninc.inc"		/* syntax support			*/
    
/*
 *  CHANGE LOG:
 *
 *	11/15/82  	add s_s2dr...nhe
 *	12/10/82  	add s_string...palm
 *	16-Dec-82  	New include strategy...palm
 *	14-Dec-82  	s_dr2s new....palm
 *	20-Feb-83  	use tae_alloc, tae_dealloc...palm
 *	28-Feb-83	change s_s2dr function to use getprim...nhe
 *			update s_s2i, add s_s2i1
 *	07-Mar-83	Correct s_s2i, s_s2dr to handle signs...nhe
 *	15-mar-83	New s_bcopy...palm
 *	31-mar-83	s_dr2s --> s_r2ws, s_r2s outputs g format...peb
 *	31-mar-83	Eliminate s_s2r, s_s2dr --> s_s2r...peb
 *	02-apr-83	Fix comment in s_s2i1...peb
 *	04-apr-83	Fix zero display in s_r2s...peb
 *	11-may-83	Remove conversion functions to a different module...dm
 *	06-feb-84	Remove definition of UPPER...dm
 *	06-feb-84	Remove functions which are standardly in C;
 *			Write bytmov and zero_block in C...palm
 *	25-jun-84	Conditional compilation of source ...dm
 *	01-apr-88	Merged nooptstr.c and optstr.c...ljn
 */

/*      TAE string package for C callers.      */


/*
 *   s_append.  Append s to t.   Returns count of t.
 */

    FUNCTION COUNT s_append(FAST TEXT *s, FAST TEXT *t)

    {
    FAST TEXT *p;

    p = t;			/* remember t's starting address */
    while (*t)			/* find end of t		 */
	t++;			
    while ((*t++ = *s++))	/* copy in s			 */
	;			
    return (t-p-1);		/* new length of t		 */
    }

/*
 *	s_bcopy.    Bounded copy.
 *
 *	Returns length of output string.  Output string is never
 *	longer than 'siz' characters (not including EOS).
 *	(Typically, you pass a SIZ constant like STRINGSIZ.)
 */
    FUNCTION COUNT s_bcopy(

    TEXT	instr[],		/* in: input string		*/
    TEXT	outstr[],		/* out: output string		*/
    FUNINT	siz			/* in: max size of outstr	*/
    )

    {
    COUNT	i;
    
    for (i=0; i < siz; i++)
        if ((outstr[i] = instr[i]) == EOS)
             return (i);
    outstr[siz] = EOS;			/* clip at max siz	*/
    return (siz);
    }    

/*
 *   s_copy.  Copy s to t and return length of s.
 */

    FUNCTION COUNT s_copy(FAST TEXT *s, FAST TEXT *t)
    {
    FAST TEXT *p;

    p = s;			/* remember start of s	*/
    while ((*t++ = *s++))
        ;
    return (s-p-1);
    }
 
/*
 *   s_length.  Return length of string.
 */

    FUNCTION COUNT s_length(FAST TEXT *s)

    {
    FAST TEXT *p;

    p = s;			/* remember s address   */
    while (*s++)	
        ;
    return(s-p-1);
    }

/*	bytmov.   Move bytes.
 */

    FUNCTION VOID bytmov (

	FAST GENPTR	from,		/* in: from pointer	*/
	FAST GENPTR	to,		/* in: to pointer	*/
	FAST FUNINT	bytes		/* in: number of bytes	*/
    )

	{
	FAST COUNT	i;

	for (i=0; i<bytes; i++)
	    *to++ = *from++;
	return;
	}

/*	zero_block.    Zero a block of memory.
 */

	FUNCTION VOID zero_block (

	FAST GENPTR	block,		/* in: ptr to memory block	*/
	FAST FUNINT	bytes		/* in: bytes to zero		*/
	)

	{
	FAST COUNT	i;

	for (i=0; i<bytes; i++)
	    *block++ = 0;
	return;
	}
#endif
