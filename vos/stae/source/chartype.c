/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*
 *	chartype.c
 *	Defines a character type table to provide a fast classification 
 *	of a character.
 *
 * 	Change Log:
 *
 *	17-jun-88	Initial release...tp
 *	07-feb-89	Conditionalize between GLOBAL and globaldef...ljn
 */

#include "taeconf.inp"
#include "chartype.inc"
#include "taeintproto.h"

#define DG	CH_DIGIT	/* redefine for a easier table setup */
#define UP	CH_UPPER
#define LO	CH_LOWER
#define LT	CH_LETTER
#define GR	CH_GRAPH
#define WH	CH_WHITE
#define CT	CH_CONTROL
#define XL	CH_EXTLTR

#ifdef ASCII
#ifdef VAX_VMS
globaldef unsigned char char_type[128] =
#else
GLOBAL unsigned char char_type[128] =
#endif
{
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /*  0-  9*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /* 10- 19*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /* 20- 29*/
CT|WH, CT|WH,    WH,    GR,    GR,    GR, GR|XL,    GR,    GR,    GR, /* $ (36)*/
   GR,    GR,    GR,    GR,    GR,    GR,    GR,    GR,    DG,    DG, /* 40- 49*/
   DG,    DG,    DG,    DG,    DG,    DG,    DG,    DG,    GR,    GR, /* 50- 59*/
   GR,    GR,    GR,    GR,    GR, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, /* 60- 69*/
UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, /* 70- 79*/
UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, /* 80- 89*/
UP|LT,    GR,    GR,    GR,    GR, GR|XL,    GR, LO|LT, LO|LT, LO|LT, /* _ (95)*/
LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, /*100-109*/
LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, /*110-119*/
LO|LT, LO|LT, LO|LT,    GR,    GR,    GR,    GR, CT|WH		      /*120-127*/
};
#else
#ifdef EBCDIC
GLOBAL unsigned char char_type[256] =
{
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /*  0-  9*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /* 10- 19*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /* 20- 29*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /* 30- 39*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /* 40- 49*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /* 50- 59*/
CT|WH, CT|WH, CT|WH, CT|WH,    WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /* 60- 69*/
CT|WH, CT|WH, CT|WH, CT|WH,    GR,    GR,    GR,    GR,    GR,    GR, /* 70- 79*/
   GR, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /* 80- 89*/
   GR, GR|XL,    GR,    GR,    GR,    GR,    GR,    GR, CT|WH, CT|WH, /* $(91)*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH,    GR,    GR,    GR, GR|XL, /* _(109)*/
   GR,    GR, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /*110-119*/
CT|WH,    GR,    GR,    GR,    GR,    GR,    GR,    GR, CT|WH, LO|LT, /*120-129*/
LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, CT|WH, CT|WH, /*130-139*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, /*140-149*/
LO|LT, LO|LT, LO|LT, LO|LT, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /*150-159*/
CT|WH,    GR, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, LO|LT, /*160-169*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /*170-179*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /*180-189*/
CT|WH, CT|WH,    GR, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, /*190-199*/
UP|LT, UP|LT, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH,    GR, UP|LT, /*200-209*/
UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, UP|LT, CT|WH, CT|WH, /*210-219*/
CT|WH, CT|WH, CT|WH, CT|WH,    GR, CT|WH, UP|LT, UP|LT, UP|LT, UP|LT, /*220-229*/
UP|LT, UP|LT, UP|LT, UP|LT, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, /*230-239*/
   DG,    DG,    DG,    DG,    DG,    DG,    DG,    DG,    DG,    DG, /*240-249*/
CT|WH, CT|WH, CT|WH, CT|WH, CT|WH, CT|WH			      /*250-255*/
};
#else
	compilation error: only ASCII and EBCDIC tables are coded.
#endif
#endif
