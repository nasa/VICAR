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



/* Functions to manipulate SUBCMD structure chains.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	10-oct-83	Fixed lint errors...palm
 *	07-may-84	Move the IMPORT declaration...lim
 *	22-oct-92	Prototype of tae_alloc is unnecessary and Ultrix 4.3
 *			does not like it...rt
 */

#include "stdh.inp"
#include "taeconf.inp"
#include "tminc.inc"
#include "taeintproto.h"



/*
 *	allsub.  Build SUBCMD structure.
 *
 *	Allocates structure and places it at end of SUBCMD chain.
 */

FUNCTION struct SUBCMD *allsub 
(
    struct SUBCMD	**subptr	/* in/out: ptr to 1st SUBCMD struct	*/

 )
    {
    FAST struct SUBCMD	*s;		/* pointer to allocated structure	*/
    FAST struct SUBCMD	*sc;		/* current pointer			*/
    FAST GENPTR		z;
    FAST COUNT		i;

    s = (struct SUBCMD *) tae_alloc(1, sizeof(struct SUBCMD));
    if (s == NULL)
	return(NULL);
    z = (GENPTR) s;
    for (i = 0; i < sizeof(struct SUBCMD); i++)
	z[i] = 0;
    if (*subptr == NULL)
	*subptr = s;
    else
	{
	for (sc = *subptr; (*sc).link != NULL; sc = (*sc).link)
	    ;				/* find end of chain...		*/
	(*sc).link = s;			/* link in new struct		*/
	}
    return(s);
    }

/*
 *	delsub.  Delete SUBCMD chain.
 */

FUNCTION VOID delsub 
(
    FAST struct SUBCMD **subptr	/* in/out: ptr to 1st SUBCMD struct	*/

 )
    {
    struct SUBCMD	*s;

    for (s = *subptr; s != NULL; s = *subptr)	/* delete 1st till chain empty	*/
	{
	*subptr = (*s).link;		/* de-link				*/
	fretxt(&(*s).ext.l1help);	/* free level 1 help text if any	*/
	tae_free((GENPTR) s);
	}
    return;
    }

/*
 *	subex.  Find name in a SUBCMD chain using exact match.
 */

FUNCTION struct SUBCMD *subex 
(
    struct SUBCMD	*subptr,	/* in:  1st struct in chain to search	*/
    TEXT		name[]		/* in:  name to find			*/

 )
    {
    FAST struct SUBCMD	*s;		/* current place in table		*/

    for (s = subptr; s != NULL; s = (*s).link)
	if (s_equal(name, (*s).name))
	    break;			/* if found				*/
    return(s);				/* return found or NULL			*/
    }

/*
 *	subab.  Lookup name in SUBCMD chain allowing abbreviations.
 *	Function returns:
 *
 *	SUCCESS if name found successfully (1 instance)
 *	FAIL	if name cannot be found in chain
 *	AMBIG	if name is an ambiguous abbreviation within chain
 */

FUNCTION CODE subab 
(
    struct SUBCMD	*subptr,	/* in:  1st struct in SUBCMD chain	*/
    TEXT		name[],		/* in:  name of subcommand to find	*/
    FAST struct SUBCMD	**subc		/* out: SUBCMD found (NULL unless SUCCESS)*/

 )
    {
    COUNT		nmatch;		/* number of matches found		*/
    FAST struct SUBCMD	*s, *ssave = 0;

    *subc = subex(subptr, name);	/* try 1st by exact match		*/
    if (*subc != NULL)
	return(SUCCESS);
    nmatch = 0;
    for (s = subptr; s != NULL; s = (*s).link)
	if (s_lseq(name, (*s).name))
	    {
	    ssave = s;
	    nmatch++;
	    }
    if (nmatch == 1)
	{
	*subc = ssave;
	return(SUCCESS);
	}
    else if (nmatch == 0)		/* subcommand not in chain	*/
	{
	*subc = NULL;
	return(FAIL);
	}
    else 				/* ambig abbreviation		*/
	{
	*subc = NULL;
	return(AMBIG);
	}
    }
