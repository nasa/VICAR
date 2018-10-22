#ifndef CAT_GEN_UTIL_H
#define CAT_GEN_UTIL_H
/******************************************************************************

gen_cat_util.h

In this file are variables that are used by the MIPS software that provides
a software TAE interface with SYABSE. 

  Original Programmer: Megan O'Shaughnessy, 16 June 1994
  Source: C
 
  Current Programmer:  Thuy Truong, 07 October 1994
  Revisions:
 
  10-07-94  TLT Removed function prototypes for catInitMessageDesc,
			catTerminate, catInitQueryDesc, catFree, mallocFnPtr.
		Revised function prototypes for catExecuteCmd, retvalsFnPtr,
			catSetNulls and catGetRets
		Added function prototypes for catFillQiDescCmd, catLogin,
			and catLogout.
		Revised CAT_NULL_DATETIME
  12-10-94  TLT Revised function prototype for catExecuteCmd from 
                        void *dataStruct to char *dataStruct 
			(FR 85853).
  03-13-95  TLT Added return status: CAT_NOT_FOUND and CAT_NOT_AFFECTED.
  05-11-95  TLT Revised the definition of CAT_BADTRANSLATION_STRING from "nul"
                to "?" (FR 87222)
  04-14-97  TLT Ported to HP by adding function prototypes for 
                cat_test_null_int,
		cat_test_null_short,
		cat_test_null_float,
		cat_test_null_char,
		cat_test_null_sybase,
		catquote,
		catcomma.
  05-08-97  TLT Revised function pointer prototypes to remove duplicate
                symbol warnings on SGI and HP (DFR).
  06-05-97  SP  Added include of <ctype.h> for routine cat_strcpy_toupper.
******************************************************************************/
#include <string.h>
#include <ctype.h>
#include <sybfront.h>
#include <sybdb.h>
#include <mdms_qi.h>
#include <mdms_query.h> 

/**************************************************************************/
/* Generic defines                                                        */
/**************************************************************************/
#define MDMS_MAIN 1
#define ROUTINE /* */          /* Blank - indicates start of new subroutine. */

#define CAT_NULL_SYBASE     "NULL"
#define CAT_NULL_BYTE       255    /* 0 to 254 are valid: translation tables */
#define CAT_NULL_SHORT      -32768
#define CAT_NULL_INT        (-2147483647-1)	/* avoid compiler warning */
#define CAT_NULL_FLOAT      -999.0
#define CAT_NULL_CHAR       "?"
#define CAT_NULL_DATETIME   "Jan  1 1900 12:00:00:000AM"

#define MIPS_FILEPATH_MAX_LEN  255
#define MIPS_FILENAME_MAX_LEN  120
#define MIPS_FILETYPE_MAX_LEN  10
#define MIPS_FILESPEC_MAX_LEN  MIPS_FILEPATH_MAX_LEN + MIPS_FILENAME_MAX_LEN \
                               + MIPS_FILETYPE_MAX_LEN
#define MIPS_DATETIME_MAX_LEN  MDMS_DATETIME_LEN

/**************************************************************************/
/* (hopefully temporary) macros                                           */
/**************************************************************************/
#define MIPS_ATTRNAME(x,y)    (x)->attrName[y]
#define MIPS_VALUELENGTH(x,y) (x)->valLength[y] 
#define MIPS_BIND(x,y,z,v,w)  dbbind((x)->dbproc,y,z,v,w)

/**************************************************************************/
/* function pointer prototyping                                           */
/**************************************************************************/
#ifdef FILL_CMD_FN_PTR
void (*fillCmdFnPtr)();
#else
extern void (*fillCmdFnPtr)();
#endif
 
#ifdef FN_PTR
int (*fnPtr)();
#else
extern int (*fnPtr)();
#endif
 
#ifdef PRINTOUT_FN_PTR
void (*printoutFnPtr)();
#else
extern void (*printoutFnPtr)();
#endif
 
#ifdef RETVALS_FN_PTR
void (*retvalsFnPtr)();
#else
extern void (*retvalsFnPtr)();
#endif
 
/**************************************************************************/
/* Dummy structure and function pointer.                                  */
/**************************************************************************/
typedef struct {
  int bogusint;
} cat_dummy_struct_typ;

#ifdef CAT_DUMMY_STRUCT_DEF
cat_dummy_struct_typ cat_dummy_struct;
#else
extern cat_dummy_struct_typ cat_dummy_struct;
#endif
 
 
#ifdef CAT_DUMMY_FN
void (*cat_dummyFn)(MDMS_QI_DESC_OBJ *qiDesc, void *s);
#else
extern void (*cat_dummyFn)(MDMS_QI_DESC_OBJ *qiDesc, void *s);
#endif

/**************************************************************************/
/* Query structure                                                        */
/* Defines for the string lengths are in mdms_message.h and in mdms_qi.h  */
/**************************************************************************/
typedef struct {
  char progname [HOST_LEN + NAME_LEN + 3];   /* 43 chars */
  char server [MDMS_NAME_LEN + 1];           /* 31 chars */
  char db [MDMS_NAME_LEN + 1];               /* 31 chars */ 
  char user [MDMS_NAME_LEN + 1];             /* 31 chars */
  char passwd [MDMS_NAME_LEN + 1];           /* 31 chars */
  int printflag;
} cat_user_struct_typ;

/**************************************************************************/
/* routine prototyping                                                    */
/**************************************************************************/
char *cat_test_null_int (int i);
char *cat_test_null_short (short ii);
char *cat_test_null_float (double n);
char *cat_test_null_char (char *s);
char *cat_test_null_sybase (char *s);

int catcomma(char *cmd, char *s);
int catquote(char *cmd, char *s);

char *zcatGetMsg (int err);
char *zcatGetSybaseMsg (int err);

int checkSlashes (char *path);
int checkDots (char *s);
int zcatGetUserData (char *server, char *db, char *user, char *passwd);
void catGetRets (void (*catRetvalsFnPtr)(),
                int printflag,
                void *catRetvalsDataStruct);
void cat_strcpy_toupper( char *tostr, char *fromstr);
int zsplitFilespec (char *filespec, char *path, char *filen, char *ext);
int zcombineFilespec (char *path, char *filen, char *ext, char *filespec);
int catLogin (cat_user_struct_typ *userInfo);
void catFillQiDescCmd( void (*fillCmdFnPtr)(), 
                      void *queryDataStruct);
void catSetNulls (void);
int catLogout (void);
int catExecuteCmd (int (*getRowsFnPtr)(), 
		   char *dataStruct, 
		   int record_length, 
                   int *nrows, 
                   int maxrows,
		   int printflag, 
                   void (*printoutFnPtr)(),
		   void *retvalsDataStruct, 
                   void (*retvalsFnPtr)());

/**************************************************************************/
/* Error messages and status from Sybase                                  */
/* The MDMS errors are #defined in mdms_qi.h                              */
/**************************************************************************/
#define MAX_SYB_ERR          3

/* error messages are in zcatGetSybaseMsg() */

/**************************************************************************/
/* Error messages and status from the catalog.                            */
/**************************************************************************/

#define CAT_BADTRANSLATION  -1
#define CAT_BADTRANSLATION_STRING "?"

#define MAX_ERR              12
#define CAT_SUCCESS          1
#define CAT_ERROR_ERR        2  /* error using catalog_status buffer */
#define CAT_SYBASE_ERR       3
#define CAT_GEN_ERR          4 /* this should probably never be used */
#define CAT_GEN_WARNING      5 /* this should probably never be used */
#define CAT_PATH_TOOBIG      6
#define CAT_FILENAME_TOOBIG  7
#define CAT_FILESPEC_TOOBIG  8
#define CAT_NOSLASHINPATH    9
#define CAT_INSUFFICIENT_MEM 10
#define CAT_NOT_FOUND        11 /* TLT: added 03-13-95 */
#define CAT_NOT_AFFECTED     12 /* TLT: added 03-13-95 */

/* error messages are in zcatGetMsg() */
/* end module */
#endif

