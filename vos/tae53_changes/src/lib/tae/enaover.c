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



#ifdef FOOBAR
/*
 * This code does not work. It is perhaps, though, not "foobar".
 * Maybe someday someone will clean it up. (Not holding my breath.) 
 */
#ifdef vax
.text
/**************************

	_ena_under: enable floating point underflow trap for caller.
    		    (This is done by setting the FU bit of the caller's
    		     PSW, which is on the stack.)

	_ena_over: enable integer overflow trap for caller.
		   (This is done by setting the IV bit of the caller's
		    PSW, which is on the stack.)

***************************/
    .globl	_ena_under
_ena_under:
    .word	0
    insv	$1,$6,$1,4(sp)
    ret

    .globl	_ena_over
_ena_over:
    .word	0
    insv	$1,$5,$1,4(sp)
    ret
#endif

#ifdef   sun
	.text
|#PROC# 04
	.globl	_ena_under
_ena_under:
	link	a6,#-LF55
	trapv
	unlk	a6
	rts
   LF55 = 0
	LS55 = 0x0
	LP55 =	8
|#PROC# 04
	.globl	_ena_over
_ena_over:
	link	a6,#-LF58
	trapv
	unlk	a6
	rts
   LF58 = 0
	LS58 = 0x0
	LP58 =	8
	.data
#endif

#else

/*	dummy enable of overflow conditions
*/
	void ena_under(void)
	{
	return;
	}
	void ena_over(void)
	{
	return;
	}
#endif
