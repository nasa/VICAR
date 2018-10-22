#ifndef IBISHELPER
#define IBISHELPER

#include <stdio.h>

#include "cartoLinkedList.h"

#ifndef MAXCOLS
#define MAXCOLS 500
#endif

/* IBIS formatting and printing was copied over from ibis_list.c */
#define IBISPRINT( ptr, fmtchar, fmtStr ) \
	switch (tolower(fmtchar)) { \
		case 'b': printf( fmtStr,    *(char *)  (ptr));  break; \
		case 'h': printf( fmtStr,    *(short *) (ptr));  break; \
		case 'f': printf( fmtStr,    *(long *)  (ptr));  break; \
		case 'r': printf( fmtStr,    *(float *)  (ptr));  break; \
		case 'd': printf( fmtStr,    *(double *)(ptr));  break; \
		case 'c': printf( fmtStr,    ((float*)ptr)[0],((float*)ptr)[1] );break; \
		case 'a': printf( fmtStr,    ptr );break; \
		default:  printf( fmtStr,   "BAD FORMAT!" );break; \
	}

#define IBISFORMAT( str, fmtchar, colsize ) \
	switch (tolower(fmtchar)) { \
		case 'b': \
		case 'h': \
		case 'f': sprintf( str, "%%%dd",  (colsize));  break; \
		case 'r': sprintf( str, "%%%d.2f",  (colsize));  break; \
		case 'd': sprintf( str, "%%%d.2lf", (colsize));  break; \
		case 'c': sprintf( str, " (%%-%d.2f,%%%d.2f)", \
			((colsize)-4)/2, colsize - (4 + ((colsize)-4)/2) );\
			 break; \
		case 'a': sprintf( str, "%%%ds", (colsize));break; \
		default:  sprintf( str, "%%%ds", (colsize));break; \
	}

///////////////////////////////////////////////////
typedef struct{
   int unit, handle;
   int nr, nc;
   char mode[8], org[7];
   char formats[MAXCOLS][6];
   int colLens[MAXCOLS];
   int totRecSize;   // size of each record in bytes
   int totDataSize;  // size of row * col in bytes
   char **data;

} IBISStruct;

///////////////////////////////////////////////////
typedef struct{
   int unit, handle;
   int nr, nc;
   char mode[8], org[7];
   LINKEDLIST *formats;
   LINKEDLIST *colLens;
} IBISPrep;

/* errors when an unknown format is specified    */
void IBISHELPER_wrongFormatError(IBISStruct *ibis, int col);

/* opens the ibis file for read or update        */
IBISStruct* IBISHELPER_openIBIS(char *name, int instance, char *mode);

/* DEPRECATED - USE IBISHELPER_openIBIS_out2     */
/* creates an output ibis file based on the out  */
/* parameter and returns an ibis struct          */
/* creates a new file but does not IBISFileOpen  */
/* until the IBISHELPER_closeIBIS is called      */
/* Data is written out at the end of program     */
IBISStruct* IBISHELPER_openIBIS_out(char **format, int inst, int nr, int nc);

/* writes out data if mode is read or update,    */
/* closes the file and deletes the ibis struct   */
void IBISHELPER_closeIBIS(IBISStruct **ibis);

/* returns the greatest column size              */
int IBISHELPER_getMaxColLen(IBISStruct *ibis);

/* casts the IBIS data into an integer data type */
/* and returns the integer                       */
/*                                               */
/* col and index start offset at 0 not 1         */
int IBISHELPER_getInt(IBISStruct *ibis, int col, int index);

/* casts the IBIS data into a float data type    */
/* and return the float                          */
/*                                               */
/* col and index start offset at 0 not 1         */
float IBISHELPER_getFloat(IBISStruct *ibis, int col, int index);

/* casts the IBIS data into a float data type    */
/* and return the double                         */
/*                                               */
/* col and index start offset at 0 not 1         */
double IBISHELPER_getDouble(IBISStruct *ibis, int col, int index);

/* copies a strint into the parameter variable   */
/*                                               */
/* col and index start offset at 0 not 1         */
void IBISHELPER_getString(IBISStruct *ibis, char *buf, int col, int index);

/* returns an actual pointer to the data in ibis */
/*                                               */
/* col and index start offset at 0 not 1         */
char* IBISHELPER_getBufPtr(IBISStruct *ibis, int col, int index);

/* gets the formats                              */
void IBISHELPER_getFormats(IBISStruct *ibis, char formats[MAXCOLS][30]);

/* sets the IBIS file buffer at col, index with  */
/* data                                          */
/* data gets written out when                    */
/* IBISHELPER_closeIBIS is called                */
/*                                               */
/* col and index start offset at 0 not 1         */
void IBISHELPER_setDouble(IBISStruct *ibis, int col, int index, double data);

/* sets the IBIS file buffer at col, index with  */
/* data                                          */
/* data gets written out when                    */
/* IBISHELPER_closeIBIS is called                */
/*                                               */
/* col and index start offset at 0 not 1         */
void IBISHELPER_setString(IBISStruct *ibis, int col, int index, char *str);

/* creates an output ibis file based on the out  */
/* parameter and returns an ibis struct          */
/* creates a new file but does not IBISFileOpen  */
/* until the IBISHELPER_closeIBIS is called      */
/* Data is written out at the end of program     */
IBISPrep* IBISHELPER_openIBIS_out2(char *name, int inst, int nr);

/* Since IBISPrep does not require all the       */
/* columns to be declared at start we need to    */
/* set it up using this call.                    */
/* The column number will be in order of the     */
/* call to this function.                        */
void IBISHELPER_addColumn(IBISPrep *ibis2, char *format);

/* Returns an IBISStruct* so that the data can   */
/* be written out                                */
IBISStruct* IBISHELPER_getIBISStruct(IBISPrep **prep);

#endif
