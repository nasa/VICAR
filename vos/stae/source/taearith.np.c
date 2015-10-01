/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*<<UNIX>>*/

/*
 *	Integer and floating point arithmetic packages
 *
 *
 *	Change log:
 *	23-sep-83	Make function 'handler' static...palm
 *	23-sep-83	Conversion from VMS to UNIX...palm
 *
 */
#include 	"stdh.inp"
#include	<signal.h>
#include	"taeconf.inp"
#include 	<setjmp.h>
#include "taeintproto.h"

#ifndef testmain

    GLOBAL COUNT v50flar = 0;		/* source version number	*/

    static jmp_buf	context;	/* context to be restored by    */
    					/* trap handler			*/

FUNCTION CODE fl_add
(
 TAEFLOAT		in1,		/* in: first number to multiply	*/
 TAEFLOAT		in2,		/* in: second number		*/
 TAEFLOAT		*out		/* out: result			*/
 );
FUNCTION CODE fl_div
(
 TAEFLOAT		in1,		/* in: first number to multiply	*/
 TAEFLOAT		in2,		/* in: second number		*/
 TAEFLOAT		*out		/* out: result			*/
 );
FUNCTION CODE fl_mult
(
 TAEFLOAT		in1,		/* in: first number to multiply	*/
 TAEFLOAT		in2,		/* in: second number		*/
 TAEFLOAT		*out		/* out: result			*/
 );
FUNCTION CODE fl_sub
(
 TAEFLOAT		in1,		/* in: first number to multiply	*/
 TAEFLOAT		in2,		/* in: second number		*/
 TAEFLOAT		*out		/* out: result			*/
 );
FUNCTION CODE int_add
(
 TAEINT		in1,		/* in: first number to multiply	*/
 TAEINT		in2,		/* in: second number		*/
 TAEINT		*out		/* out: result			*/
 );
FUNCTION CODE int_div
(
 TAEINT		in1,		/* in: first number to multiply	*/
 TAEINT		in2,		/* in: second number		*/
 TAEINT		*out		/* out: result			*/
 );
FUNCTION CODE int_mult
(
 TAEINT		in1,		/* in: first number to multiply	*/
 TAEINT		in2,		/* in: second number		*/
 TAEINT		*out		/* out: result			*/
 );
FUNCTION CODE int_sub
(
 TAEINT		in1,		/* in: first number to multiply	*/
 TAEINT		in2,		/* in: second number		*/
 TAEINT		*out		/* out: result			*/
 );

/* fl_add - add two floating point numbers
 *
 * return SUCCESS/FAIL
 */
    FUNCTION CODE fl_add(

    TAEFLOAT		in1,		/* in: first number to multiply	*/
    TAEFLOAT		in2,		/* in: second number		*/
    TAEFLOAT		*out		/* out: result			*/
    )

    {
    return(fl_arith(in1, in2, '+', out));	/* trap handler may set  */
    }

FUNCTION static VOID handler(int);

/* fl_arith - General arithmetic on two floating point numbers
 *
 * return SUCCESS/FAIL
 */
    FUNCTION CODE fl_arith(

    TAEFLOAT		in1,		/* in: first number to multiply	*/
    TAEFLOAT		in2,		/* in: second number		*/
    FAST TEXT		op,		/* in: operator			*/
    TAEFLOAT		*out		/* out: result			*/
    )

    {
    CODE		code;
    TAEFLOAT		temp = 0;

    signal (SIGFPE, handler);		/* enable trap catcher		*/
    code = setjmp (context);		/* save present context		*/
    if (code != 0)
        {
	*out = 0.0;			/* handler brought us back here */
	return (FAIL);			/* from float operation		*/
	}
    if (op == '*')
    	temp = in1 * in2;		/* use local data to keep trap local */
    else if (op == '+')
    	temp = in1 + in2;
    else if (op == '-')
    	temp = in1 - in2;
    else if (op == '/')
    	temp = in1/in2;
    *out = temp;
    signal (SIGFPE, SIG_DFL);		/* disable trap catcher		*/
    return(SUCCESS);	
    }


/* fl_div - divide two floating point numbers
 *
 * return SUCCESS/FAIL
 */
    FUNCTION CODE fl_div(

    TAEFLOAT		in1,		/* in: first number to multiply	*/
    TAEFLOAT		in2,		/* in: second number		*/
    TAEFLOAT		*out		/* out: result			*/
    )

    {
    return(fl_arith(in1, in2, '/', out));	
    }


/* fl_mult - multiply two floating point numbers
 *
 * return SUCCESS/FAIL
 */
    FUNCTION CODE fl_mult(

    TAEFLOAT		in1,		/* in: first number to multiply	*/
    TAEFLOAT		in2,		/* in: second number		*/
    TAEFLOAT		*out		/* out: result			*/
    )

    {
    return(fl_arith(in1, in2, '*', out));	
    }


/* fl_sub - subtract two floating point numbers
 *
 * return SUCCESS/FAIL
 */
    FUNCTION CODE fl_sub(

    TAEFLOAT		in1,		/* in: first number to multiply	*/
    TAEFLOAT		in2,		/* in: second number		*/
    TAEFLOAT		*out		/* out: result			*/
    )

    {
    return(fl_arith(in1, in2, '-', out));	
    }

/* int_add - add two integer numbers
 *
 * return SUCCESS/FAIL
 */
    FUNCTION CODE int_add(

    TAEINT		in1,		/* in: first number to multiply	*/
    TAEINT		in2,		/* in: second number		*/
    TAEINT		*out		/* out: result			*/
    )

    {
    return(int_arith(in1, in2, '+', out));	
    }


/* int_arith - General arithmetic on two integer numbers
 *
 * return SUCCESS/FAIL
 */
    FUNCTION CODE int_arith(

    TAEINT		in1,		/* in: first number to multiply	*/
    TAEINT		in2,		/* in: second number		*/
    TEXT		op,		/* in: operator			*/
    TAEINT		*out		/* out: result			*/
    )

    {
    CODE		code;
    TAEINT		temp = 0;

    signal (SIGFPE, handler);		/* enable trap handler		*/
    code = setjmp (context);		/* save present context		*/
    if (code != 0)
        {
	*out = 0;			/* handler brought us back here */
	return (FAIL);			/* from float operation		*/
	}
    if (op == '*')
    	temp = in1 * in2;		/* use local data to keep trap local */
    else if (op == '+')
    	temp = in1 + in2;
    else if (op == '-')
    	temp = in1 - in2;
    else if (op == '/')
    	temp = in1/in2;
    *out = temp;
    signal (SIGFPE, SIG_DFL);		/* reset trap to default	 */
    return(SUCCESS);			
    }


/* int_div - divide two integer numbers
 *
 * return SUCCESS/FAIL
 */
    FUNCTION CODE int_div(

    TAEINT		in1,		/* in: first number to multiply	*/
    TAEINT		in2,		/* in: second number		*/
    TAEINT		*out		/* out: result			*/
    )

    {
    return(int_arith(in1, in2, '/', out));	
    }


/* int_fl2i - convert a floating point to an integer
 *
 * We need a fn to do this because we watch out for overflow
 *
 * return SUCCESS/FAIL
 */
FUNCTION CODE int_fl2i
(
    TAEFLOAT		fl,		/* in: the floater		*/
    TAEINT		*intgr		/* out: the integer		*/

 )
    {
    CODE 		code;
    TAEINT		temp;

    signal(SIGFPE, handler);		/* announce handler		*/
    code = setjmp (context);
    if (code != 0)
	{
	*intgr = 0;
	return (FAIL);
	}
    temp = fl;
    *intgr = temp;
    signal (SIGFPE, SIG_DFL);
    return(SUCCESS);
    }    

/* int_mult - multiply two integer numbers
 *
 * return SUCCESS/FAIL
 */
    FUNCTION CODE int_mult(

    TAEINT		in1,		/* in: first number to multiply	*/
    TAEINT		in2,		/* in: second number		*/
    TAEINT		*out		/* out: result			*/
    )

    {
    return(int_arith(in1, in2, '*', out));	
    }


/* int_sub - subtract two integer numbers
 *
 * return SUCCESS/FAIL
 */
    FUNCTION CODE int_sub(

    TAEINT		in1,		/* in: first number to multiply	*/
    TAEINT		in2,		/* in: second number		*/
    TAEINT		*out		/* out: result			*/
    )

    {
    return(int_arith(in1, in2, '-', out));	
    }



/* handler - trap/fault handler
 *
 *	The handler restores context to the function generating the
 *	trap.
 *
 *	We do not attempt to return to the point of the trap because
 *	that will just execute the bad instruction again.  We
 *	do a long jump to the offending function.
 *
 */
FUNCTION static void handler(int UNUSED(x1))


    {

    longjmp (context, 1);		/* reset to context	*/
    }


#else

    main(void)
    {
    float r1, r2, out;
    CODE code;
    int i1, i2, i3;    

    printf("Floating point multiply trap test, version 1\n");
    printf(" Enter two numbers> ");
    scanf("%f , %f", &r1, &r2);
    while ( r1 != -1)	/* exit on -1				*/
    	{
    	printf("inputs: %e,  %e\n", r1, r2);
        code = fl_mult(r1, r2, &out);    
    	printf("code = %d, output = %e\n", code, out);
    	printf(" Enter two numbers> ");
    	scanf("%f , %f", &r1, &r2);
    	}
    printf("Integer multiply test.\n");
    while (1)
	{
	printf("Enter two numbers> ");
	scanf("%d, %d", &i1, &i2);
	code = int_mult(i1, i2, &i3);
	printf("code = %d, output = %d\n", code, i3);    
        }
    }
#endif    
