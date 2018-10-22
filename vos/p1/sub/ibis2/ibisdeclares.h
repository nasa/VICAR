/**
 **  ibisdeclares.h
 **
 **   Forward definitions for IBIS subroutine library.
 **/
 
#ifndef _H_IBISDECLARES
#define _H_IBISDECLARES
#include "zvproto.h"
/**
 **  Forward references:
 **/

struct XIBIS;
typedef struct XIBIS XIBIS;
struct XREC;
typedef struct XREC XREC;
struct XGROUP;
typedef struct XGROUP XGROUP;
struct XGAP;
typedef struct XGAP XGAP;
struct XCOL;
typedef struct XCOL XCOL;
struct t_recmethod;
typedef struct t_recmethod t_recmethod;
struct t_rowmethod;
typedef struct t_rowmethod t_rowmethod;
struct t_colmethod;
typedef struct t_colmethod t_colmethod;
struct t_filemethod;
typedef struct t_filemethod t_filemethod;
struct t_labmethod;
typedef struct t_labmethod t_labmethod;
struct trans_type;
typedef struct trans_type trans_type;

/* generic function pointer - arg list actually can vary */
typedef int (*XMethod)();

/**
 ** Public (protected) Function declarations
 **/

XIBIS* _i_new_ibis(void);
XREC*  _i_new_record(int ncols);
XGROUP* _i_new_group(char *group);
XGROUP* _i_request_group(List *list, char *group);
XGROUP* _i_find_group(List *list, char *group);
XGROUP* _i__i_find_group_all(XIBIS *ibis, char *group);
XGROUP* _i_group_construct(XIBIS *ibis, char *expression);
char *_i_parse_group_name(XIBIS *ibis, char *name, List **list);
void *_i_mem_dup(char* data, int size );
void _i_free_ibis(XIBIS *ibis);
void _i_free_group(XGROUP *group);
void _i_free_record(XREC *rec);
int _i_null_method(void);
int _i_strcmp_nocase(char* str1,char* str2);
int _i_keymatch(char *keystr, char **keys);
int _i_IBISFormatSize(XIBIS *ibis, int format, int *size);
int _i_IBISFormatCode(char *fmtstr );
int _i_detach_col_from_format(XIBIS* ibis, int col);
int _i_attach_format_to_column(XIBIS *ibis,int fmt, int col);
XCOL *_i_new_column(void);
void _i_free_column(XCOL *col);
int _i_init_column_array(XIBIS *ibis );
int _i_insert_column_pointers(XIBIS *ibis, int col, int ncol );
int _i_delete_column_pointers(XIBIS *ibis, int col, int ncol );
int _i_move_column_pointers(XIBIS * ibis, int srccol, int destcol, int ncol);
void _i_free_col_array(XIBIS *ibis );
int _i_attach_column_to_group(List* list,char *grp, XCOL *col);
int _i_detach_column_from_list(XIBIS *ibis,List* list,int col);
int IBISGroupTransfer(int in_id, int out_id, char *type, int *incols, 
		      int* outcols, int nc );
int IBISGroupNew(int ibis_id,char *type, char *name, int *cols, int ncols, 
		 char *expr );
int _IBISGroupNew(XIBIS* ibis, char* type, char* name, int* cols, int ncols, 
		  char* expr);
int IBISGroupDelete(int ibis_id,char *type, char *name );
int IBISGroupModify(int ibis_id,char *type, char* name, char* mod, int* cols, 
		    int ncols );
int IBISGroupFind(int, char*, int, char*, int, int, int);
int IBISColumnRead(int ibis_id,char* buffer,int column,int srow,int nrows);
int IBISColumnWrite(int ibis_id,char *buffer,int column,int srow,int nrows);
int IBISColumnClear(int ibis_id,int column,int ncols);
int IBISColumnNew(int ibis_id,int column,int ncols,char *fmt);
int IBISColumnDelete(int ibis_id,int column,int ncols);
int IBISColumnMove(int ibis_id,int sourcecol,int destcol,int ncols);
int IBISColumnFind(int, char*, char*, int*, int, int);
int _i_set_trans(XIBIS* ibis,trans_type *trans,int buffmt,int filefmt);
int _i_trans_init(XIBIS *ibis );
void _i_trans_reset(XIBIS* ibis );
int _i_trans_buf_to_local(trans_type *trans, XCOL *col, char* inbuf, 
			  char *outbuf, int num );
int _i_trans_buf_from_local(trans_type* trans, XCOL* col, char* inbuf, 
			    char* outbuf, int num );
XREC *_i_new_record(int ncols );
void _i_free_record(XREC *rec);
int _i_notify_records(XIBIS* ibis, notice_type notice, int sval, int endval);
void _i_init_space(XIBIS* ibis);
void _i_allocate_space(XIBIS *ibis, int offset, int size );
void _i_deallocate_space(XIBIS* ibis, int offset, int size );
int _i_find_space(XIBIS* ibis, int* offset, int size);
List* _i_new_list(void (*kill_method)());
void _i_free_list(List *list );
List* _i_find_value(List *list, list_value value);
int _i_count_list(List *list);
void _i_insert_value(List* list, list_value value);
void _i_append_value(List* list, list_value value);
void _i_delete_value(List* list, list_value value);
int _init_ibis_globals(void);
void _i_ibis1_install_methods(XIBIS* ibis);
void _i_ibis2_install_methods(XIBIS* ibis );
void _i_purge_ibis(XIBIS* ibis);
int IBISRowClear(int ibis_id,int srow,int nrows);
int IBISRowNew(int ibis_id,int srow,int nrows);
int IBISRowDelete(int ibis_id,int srow,int nrows);
int _i_compute_blocksize(XIBIS* ibis, int lower_bound );
int _i_reset_recsize(XIBIS* ibis);
int _i_useful_segment(int lower_bound );
void _i_compute_new_offsets(XIBIS* ibis,int scol,int ncol);
int _i_process_contiguous(int unit, int (*function)(int, void*, ...), char *buffer, 
			  int offset, int nbytes, int blksize, int recsize, 
			  int inc);
int _i_install_ofile_methods(XIBIS* ibis );
void _i_install_cfile_methods(XIBIS* ibis );
void _i_install_rfile_methods(XIBIS* ibis );
int _i_install_grfile_methods(XIBIS *ibis );
int _i_clear_ibis_file(XIBIS* ibis, int sblock);
int _i_install_numblocks(XIBIS* ibis, char* label,int numblocks);
int _i_install_nlabel_methods(XIBIS * ibis );
int _i_install_olabel_methods(XIBIS *ibis );
int _i_install_grlabel_methods(XIBIS * ibis );
int _i_strcmp_nocase(char* str1,char* str2);
void _i_make_uppercase(char* str);
int _i_strncmp_nocase(char* str1,char* str2, int n);
int _i_keymatch(char* keystr, char **keys);
void *_i_mem_dup(char* data, int size );
int _i_check_name(char* namestr );
XGROUP* _i_new_group(char* group);
XGROUP* _i_find_group(List *list, char* grp );
char *_i_parse_group_name(XIBIS *ibis, char *name, List **list);
XGROUP* _i_find_group_all(XIBIS* ibis, char *grp );
XGROUP* _i_request_group(List* list, char *grp );
void _i_free_group(XGROUP* group );
XGROUP* _i_group_construct(XIBIS* ibis, char* expression);
int _HandleNotice(XREC* record, notice_type notice,void * sval, int endval );
int IBISRecordOpen(int ibis_id, int *record_id,  char *group, int *columns, 
		   int ncols, char* u_format);
int IBISRecordClose(int record_id );
int IBISRecordSet(int record_id, char *name, int value );
int IBISRecordGet(int record_id, char* name, char* value, int sval, 
		  int nvals );
int IBISRecordRead(int record_id, char* buffer, int row);
int IBISRecordWrite(int record_id,char* buffer,int row);
int IBISRecordClear(int record_id,int row,int nrows);
void ICLGetROOT(int ibis,char* group,char* inst);
void ICLGetPrimitive(int ibis,char *group,char* expression);
void ICLGetPOSITION(int ibis,char* group,char* inst);
void ICLGetPOS_IMAGE(int ibis,char* gline,char* gsamp,char* gband,char *inst);
void ICLGetPOS_GEOGRAPHIC(int ibis,char* glat,char* glong,char* inst);
void ICLGetVALUE(int ibis,char* group,char* inst);
void ICLGetRGB(int ibis,char* gred,char* ggreen,char* gblue,char* inst);
void ICLGetPIXEL(int ibis,char* gpos,char* gval,char* inst);
void ICLGetDIRECTION(int ibis,char* group,char* inst);
void ICLGetMATRIX(int ibis,char* gmat,char* gind,char* inst);
void ICLGetQUALITY(int ibis,char* gqual,char* inst);
void ICLGetHISTOGRAM(int ibis,char* gmat,char* gind,char* inst);
void ICLGetLOOKUP_TABLE(int ibis,char* gps,char* gind,char* memname,char* inst);void ICLGetSTATISTICS(int ibis,char* gmat,char* gind,char* memname,char *inst);
void ICLGetPOINT(int ibis,char* gpos,char* gval,char* memname,char* inst);
int icl_keymatch(char *keystr, char **keys);


#endif /* _H_IBISDECLARES */

