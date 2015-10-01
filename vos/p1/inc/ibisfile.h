/**
 **  ibisfile.h
 **
 **   Useful definitions for IBIS subroutine library.
 **/
 
#ifndef _H_IBISFILE
#define _H_IBISFILE

#include "xvmaininc.h"

/* Array size definitions (Added per FR#85751) String limits include NULL */
#define I2_MAX_COL        1024 /* Current somewhat arbitrary Max # columns   */
#define I2_MAX_COL_OLD    100  /* Limit for IBIS-1 files                     */
#define I2_MAX_GRP_NAME   33   /* Maximum Group name string                  */
#define I2_MAX_TYPE_NAME  7    /* Maximum Type name string                   */
#define I2_MAX_VALUE_NAME 17   /* Maximum Value name string for IBIS Get/set */

/* Size of input format string to IBISFileOpen */
#define IFMT_SIZE 6

/* IBISFile  modes */
#define IMODE_READ     "READ"
#define IMODE_WRITE    "WRITE"
#define IMODE_OWRITE   "OWRITE"
#define IMODE_UPDATE   "UPDATE"

/* IBISFileClose options */
#define ICLOSE_UKEEP	"UKEEP"
#define ICLOSE_UDELETE	"UDELETE"

/* IBISGroup modify options */
#define IGROUP_APPEND   "APPEND"
#define IGROUP_INSERT   "INSERT"
#define IGROUP_REMOVE   "REMOVE"

/* IBISFile organizations */
#define IORG_ROW       "ROW"
#define IORG_COLUMN    "COLUMN"

/* group types */
#define ITYPE_FORMAT    "FORMAT"
#define ITYPE_UNIT      "UNIT"
#define ITYPE_GROUP     "GROUP"
#define ITYPE_LOCAL     "LOCAL"
#define ITYPE_ANY       "ANY"

/* file descriptor attributes */
#define IFILE_NR        "NR"
#define IFILE_NC        "NC"
#define IFILE_ORG       "ORG"
#define IFILE_MODE      "MODE"
#define IFILE_FORMATS   "FORMATS"
#define IFILE_UNITS     "UNITS"
#define IFILE_GROUPS    "GROUPS"
#define IFILE_LOCALS    "LOCALS"
#define IFILE_VUNIT     "VUNIT"
#define IFILE_U_SIZE    "U_SIZE"
#define IFILE_U_FORMAT  "U_FORMAT"
#define IFILE_HOST      "HOST"
#define IFILE_INTFMT    "INTFMT"
#define IFILE_REALFMT   "REALFMT"
#define IFILE_FMT_DEFAULT "FMT_DEFAULT"
#define IFILE_TYPE      "TYPE"
#define IFILE_VERSION   "VERSION"
#define IFILE_AUTO_INIT "AUTO_INIT"
#define IFILE_PIX_NL    "PIX_NL"
#define IFILE_PIX_NS    "PIX_NS"
#define IFILE_PIX_HOST  "PIX_HOST"
#define IFILE_PIX_FORMAT "PIX_FORMAT"
#define IFILE_PIX_RECSIZE "PIX_RECSIZE"

/* ibis versions */
#define IVERSION_1		"IBIS-1"
#define IVERSION_2		"IBIS-2"

/* Auto-Initialize flag */
#define IINIT_ON		"ON"
#define IINIT_OFF		"OFF"

/* column properties */
#define ICOLUMN_U_FORMAT  "U_FORMAT"
#define ICOLUMN_U_SIZE    "U_SIZE"
#define ICOLUMN_FORMAT    "FORMAT"
#define ICOLUMN_SIZE      "SIZE"

/* record properties */
#define IRECORD_U_FORMAT  "U_FORMAT"
#define IRECORD_U_SIZE    "U_SIZE"
#define IRECORD_COLUMNS   "COLUMNS"
#define IRECORD_IUNIT     "IUNIT"
#define IRECORD_NR		  "NR"
#define IRECORD_NC		  "NC"
#define IRECORD_ROW		  "ROW"
#define IRECORD_REC_SIZE   "REC_SIZE"

/* formats */
#define IFMT_BYTE      "BYTE"
#define IFMT_HALF      "HALF"
#define IFMT_FULL      "FULL"
#define IFMT_REAL      "REAL"
#define IFMT_DOUB      "DOUB"
#define IFMT_COMP      "COMP"
#define IFMT_ASCII     "ASCII"

/* special "no trans" format */
#define IFMT_NONE      "NONE"

/* C++ requires prototypes */
#ifdef __cplusplus
extern "C" {
#endif

#ifndef _NO_PROTO

int IBISFileOpen(int, int*, char*, int, int, char*, char *);
int IBISFileUnit(int, int *, char *, int, int, char *, char *);
int IBISFileUnitOpen(int);
int IBISFileGet(int, char *, void *, int, int, int);
int IBISFileSet(int, char*, char*, int);
int IBISFileClear(int);
int IBISFileClose(int, char*);

int IBISSignalU(int, int, int);
int IBISSignal(int, int, int);
int IBISColumnFind(int, char *, char *, int *, int, int);
int IBISColumnRead(int, char *, int, int, int);
int IBISColumnGet(int ibis_id, char *name, void *value, int column);
int IBISColumnSet(int ibis_id, char *name, void *value, int column);
int IBISColumnWrite(int ibis_id, char *buffer, int column, int srow, int nrows);
int IBISColumnNew(int ibis_id, int column, int ncols, char *fmt);
int IBISColumnClear(int ibis_id, int column, int ncols);
int IBISColumnDelete(int ibis_id, int column, int ncols);
int IBISColumnMove(int ibis_id, int sourcecol, int destcol, int ncols);
int IBISColumnRead(int, char *, int, int, int);

int IBISRecordOpen(int, int *, char *, int *, int, char *);
int IBISRecordRead(int, char *, int);
int IBISRecordWrite(int, char *, int);

int IBISRecordClose(int record_id);
int IBISRecordGet(int record_id, char *name, char *value, int sval, int nvals);
int IBISRecordSet(int record_id, char *name, int value);
int IBISRecordClear(int record_id, int row, int nrows);

int IBISGroupNew(int ibis_id, char *type, char *name, int *cols, int ncols, char *expr);
int IBISGroupDelete(int ibis_id, char *type, char *name);
int IBISGroupModify(int ibs_id, char *type, char *name, char *mod, int *cols, int ncol);
int IBISGroupFind(int ibis_id, char *type, int column, char *namelist, int sgroup, int maxgroups, int length);
int IBISGroupTransfer(int ibis_in, int ibis_out, char *type, int *incols, int *outcols, int numcols);

int IBISRowClear(int ibis_id, int srow, int nrows);
int IBISRowNew(int ibis_id, int srow, int nrows);
int IBISRowDelete(int ibis_id, int srow, int nrows);

int IBISLabelRemove(int unit);

int ICLNewROOT(int, int *, int, char *);
int ICLNewPOSITION(int, int *, int, char *);
int ICLNewPOS_IMAGE(int, int, int, int, char *);
int ICLNewPOS_GEOGRAPHIC(int, int, int, char *);
int ICLNewVALUE(int, int *, int, char *);
int ICLNewRGB(int, int, int, int, char *);
int ICLNewPIXEL(int, int *, int, int *, int, char *);
int ICLNewDIRECTION(int, int *, int, char *);
int ICLNewMATRIX(int, int *, int, int *, int, char *);
int ICLNewQUALITY(int, int *, int, char *);
int ICLNewHISTOGRAM(int, int *, int, int *, int, char *);
int ICLNewLOOKUP_TABLE(int, int *, int, int *, int, char *, char *);
int ICLNewSTATISTICS(int, int *, int, int *, int, char *, char *);
int ICLNewPOINT(int, int *, int, int *, int, char *, char *);

void ICLGetROOT(int , char *, char *);
void ICLGetPOSITION(int, char *, char *);
void ICLGetPOS_IMAGE(int, char *, char *, char *, char *);
void ICLGetPOS_GEOGRAPHIC(int, char *, char *, char *);
void ICLGetVALUE(int, char *, char *);
void ICLGetRGB(int, char *, char *, char *, char *);
void ICLGetPIXEL(int, char *, char *, char *);
void ICLGetDIRECTION(int, char *, char *);
void ICLGetMATRIX(int, char *, char *, char *);
void ICLGetQUALITY(int, char *, char *);
void ICLGetHISTOGRAM(int, char *, char *, char *);
void ICLGetLOOKUP_TABLE(int, char *, char *, char *, char *);
void ICLGetSTATISTICS(int, char *, char *, char *, char *);
void ICLGetPOINT(int, char *, char *, char *, char *);

/* Graphics routines */

int zrdgr(int instance,int num,int dimension);
int zrdgr_unit(int unit,int num,int dimension);
int zwrgr(int instance,int num,int dimension);
int zwrgr_unit(int unit,int num,int dimension);
int zupdategr(int unit,int num,int dimension);
int zgetgr(int num,int *zero1,int *eof,float* first_c,float* second_c,float* other_c);
int znextgr(int num,int *eof,float* first_c,float* second_c,float* other_c);
int zputgr(int num,double first_c,double second_c,float* other_c);
int zendgr(int num);
int zsetgr(int num, int row);
int zclgr(int num);
void zsignalgr(int num,int status,int abendflag);

#endif

#ifdef __cplusplus
}
#endif


#endif /* _H_IBISFILE */

