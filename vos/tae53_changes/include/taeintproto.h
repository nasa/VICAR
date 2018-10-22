// Prototypes internal to the tae library
#ifndef TAEINTPROTO_H
#define TAEINTPROTO_H
#include "stdh.inp"
#include "taeconf.inp"
#include "comminc.inp"
#include "expinc.inc"
#include "syninc.inc"
#include "dirinc.inc"
#include "helpinc.inc"
#include "forstr.inp"
#include "asyncinc.inc"
#include <stdint.h>
#include <sys/types.h>

#include "taeextproto.h"

/* Silence warning in gcc about unused arguments. This does nothing
   for other compilers */

#ifdef UNUSED
#elif defined(__GNUC__)
# define UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
# define UNUSED(x) /*@unused@*/ x
#else
# define UNUSED(x) x
#endif
#define OUTMSGSIZ   STRINGSIZ		/* size of output message */ 
#define ALIGN ALIGN2
struct	FORCTX 
{
  struct  VARIABLE  *nxtvar;	/* variable to work with next */
  COUNT   nxtind;			/* next index to value vector */
};
typedef CODE (*FUNCTION_PTR) (char* record, int size, GENPTR d_context, int);

FUNCTION COUNT s_append(FAST TEXT* s, FAST TEXT* t);
FUNCTION COUNT s_bcopy(TEXT instr[], TEXT outstr[], FUNINT siz);
FUNCTION COUNT s_copy(FAST TEXT *s, FAST TEXT *t);
FUNCTION COUNT s_strip(TEXT s[]);
FUNCTION COUNT s_length(FAST TEXT* s);
FUNCTION TEXT *s_save(FAST TEXT *s);
FUNCTION VOID s_blank(FAST TEXT* s, FUNINT n);
FUNCTION BOOL s_lseq(FAST TEXT* s, FAST TEXT* t);
FUNCTION VOID m_msgout(TEXT msgbuf[OUTMSGSIZ+1], TEXT msgkey[KEYSIZ+1]);
FUNCTION VOID p_msgout(TEXT msgbuf[OUTMSGSIZ+1], TEXT msgkey[KEYSIZ+1]);
FUNCTION VOID bytmov(FAST GENPTR from, FAST GENPTR to, FAST FUNINT bytes);
FUNCTION VOID zero_block (FAST GENPTR block, FAST FUNINT bytes);
FUNCTION CODE q_sndp(struct PARBLK* p, FUNINT msgtype);
FUNCTION CODE c_subini(void);
FUNCTION VOID c_close_all(void);
FUNCTION CODE c_rcvp(GENPTR block, FUNINT blksize);
FUNCTION CODE c_sndp(GENPTR block, FUNINT blksize);
FUNCTION CODE c_snpa(GENPTR block, FUNINT blksize);
FUNCTION CODE t_init(COUNT *lines, COUNT *columns, CODE *type);
FUNCTION CODE t_write(TEXT string[], FUNINT cc);
FUNCTION BOOL c_child_proc(void);
FUNCTION VOID procexit(CODE exit_code);
FUNCTION VOID x_error(FUNINT mode, TEXT message[], TEXT key[], uintptr_t A1, 
		      uintptr_t A2, uintptr_t A3);
FUNCTION VOID parm_err(TEXT control[], TEXT key[], uintptr_t A1,
		       uintptr_t A2, uintptr_t A3);
FUNCTION COUNT s_lower(FAST TEXT* s);
FUNCTION VOID makeabs(struct SYMTAB *symtab, ALIGN *s);
FUNCTION VOID makerel(struct SYMTAB *symtab, ALIGN *s);
FUNCTION VOID r_newsiz(ALIGN *s, COUNT bytes);
FUNCTION VOID r_init(ALIGN *storage, FUNINT bytes);
FUNCTION GENPTR r_alloc(ALIGN *s, FUNINT bytes);
FUNCTION VOID r_dealloc(ALIGN *s, GENPTR p);
FUNCTION GENPTR r_top(ALIGN* s);
FUNCTION VOID r_setrel(ALIGN* s, GENPTR pp, GENPTR p);
FUNCTION GENPTR r_setabs(ALIGN* s, GENPTR pp);
FUNCTION CODE p_attr(struct PARBLK* block, TEXT name[], CODE* type, COUNT* n, 
		     BOOL* dflt, CODE* access);
FUNCTION VOID p_get_root (TEXT name[], TEXT root[NAMESIZ+1], 
			  TEXT remainder[STRINGSIZ+1]);
FUNCTION VOID p_get_leaf (TEXT name[], TEXT leaf[NAMESIZ+1], 
			  TEXT remainder[STRINGSIZ+1]);
FUNCTION VOID z_call(struct PARBLK* parblk);
UNSUPPORTED VOID Vm_FreeValue(TEXT* pv, FUNINT type, FUNINT count);
UNSUPPORTED char *Vm_AllocValue(struct VARIABLE* v);
UNSUPPORTED struct VARIABLE *Vm_Alloc(struct SYMTAB* symtab, TEXT name[], 
				      FUNINT type, FUNINT count, FUNINT strsiz);
FUNCTION VOID getlun_tae(int* lun);
FUNCTION VOID s_d2s(GENPTR d, TEXT *s);
FUNCTION CODE s_i2s(TAEINT i, TEXT* s);
FUNCTION CODE s_r2s(TAEFLOAT r, TEXT* s);
FUNCTION CODE s_s2i(TEXT* s, TAEINT* i);
FUNCTION CODE s_s2i1(TEXT* s, TAEINT* i);
FUNCTION CODE s_r2ws(TAEFLOAT taefloat, TEXT string[]);
FUNCTION CODE s_s2r(TEXT* s, TAEFLOAT* r);
FUNCTION CODE s_s2r1(TEXT* s, TAEFLOAT* r);
FUNCTION COUNT s_index(TEXT s[], FUNINT c);
FUNCTION VOID dblquote(TEXT s[]);
FUNCTION VOID addqu(TEXT s[]);
FUNCTION VOID initok(struct SYNBLK* sb, TEXT* usrstr);
FUNCTION CODE getprim(struct SYNBLK* sb, struct VALUE* value);
FUNCTION CODE fl_add(TAEFLOAT in1, TAEFLOAT in2, TAEFLOAT* out);
FUNCTION CODE fl_mult(TAEFLOAT in1, TAEFLOAT in2, TAEFLOAT* out);
FUNCTION CODE fl_div(TAEFLOAT in1, TAEFLOAT in2, TAEFLOAT* out);
FUNCTION CODE int_mult(TAEINT in1, TAEINT in2, TAEINT* out);
FUNCTION CODE int_add(TAEINT in1, TAEINT in2, TAEINT* out);
FUNCTION VOID strpqu(TEXT s[]);
FUNCTION VOID deal_pval(struct VALUE *value, CODE class);
FUNCTION CODE gettok (struct SYNBLK* sb, TEXT* token);
FUNCTION CODE getfld (struct SYNBLK* sb, TEXT* field);
FUNCTION VOID s_free(TEXT *s);
FUNCTION COUNT s_shift(TEXT s[], FUNINT n);
FUNCTION CODE fl_arith(TAEFLOAT in1, TAEFLOAT in2, TEXT op, TAEFLOAT* out);
FUNCTION CODE int_arith(TAEINT in1, TAEINT in2, TEXT op, TAEINT* out);

FUNCTION CODE q_one_string(struct PARBLK* p, TEXT name[], COUNT idx, 
			   TEXT onestr[], FUNINT mode);
FUNCTION CODE q_shortstring(struct PARBLK* p, TEXT name[], FUNINT count, 
			    FUNINT strsiz, TEXT* vector[], FUNINT mode);
UNSUPPORTED CODE Vm_SpCopyVar(struct VARIABLE* vin, struct VARIABLE* vout);
UNSUPPORTED CODE Vm_CopyValue(FUNINT type, GENPTR in, GENPTR out, FUNINT count);
UNSUPPORTED CODE Vm_MoveSymtab (struct SYMTAB* st1, struct SYMTAB* st2);
FUNCTION VOID bytmov (GENPTR from, GENPTR to, FUNINT bytes);
UNSUPPORTED VOID  Vm_FreeTable(struct SYMTAB *head);
UNSUPPORTED COUNT Vm_ValidSize (FUNINT type, FUNINT count);
FUNCTION CODE q_min (struct PARBLK* p, TEXT name[], FUNINT count);
FUNCTION CODE q_max (struct PARBLK* p, TEXT name[], FUNINT count);
FUNCTION CODE q_stringlength (struct PARBLK* p, TEXT name[], FUNINT count);
FUNCTION CODE q_parmpage (struct PARBLK* p, TEXT name[], BOOL flag);
FUNCTION CODE q_validintg(struct PARBLK* p, TEXT name[], FUNINT count, 
			  TAEINT ilow[], TAEINT ihigh[]);
FUNCTION CODE q_validreal(struct PARBLK* p, TEXT name[], FUNINT count, 
			  TAEFLOAT rlow[], TAEFLOAT rhigh[]);
FUNCTION CODE q_validstr(struct PARBLK* p, TEXT name[], FUNINT count, 
			 TEXT *vector[]);
FUNCTION CODE q_validonestr(struct PARBLK* p, TEXT name[], FUNINT idx, 
			 TEXT onestr[]);
FUNCTION CODE q_validprep(struct PARBLK* p, TEXT name[], FUNINT count, 
			  FUNINT type, struct VARIABLE** vv);
FUNCTION CODE t_read (TEXT string[STRINGSIZ+1], CODE *term);

FUNCTION CODE q_listproc
(
 struct    PARBLK    *inblk,
 TEXT      name[],
struct    PARBLK    *outblk
);

FUNCTION CODE f_opnspc
(

 struct SFILE	*sfile,		/* in/out: SFILE struct		*/
 FUNINT		lun,		/* in: lun to use		*/
 TEXT		fspec[],	/* in: host file spec		*/
 TEXT		deflibr[],	/* in: default library		*/
 TEXT		defname[],	/* in: default file name	*/
 TEXT		deftype[],	/* in: default file type	*/
 FUNINT		access		/* in: F_READ, F_WRITE...	*/
 );
FUNCTION CODE f_read
(
 struct SFILE 	*f,		/* in/out: SFILE for an opened file  */
 TEXT string[]	/* out: record with EOS
					   terminator   */
 );
FUNCTION VOID xzper
(
    FORSTR 		*tae_spec,	/* in: tae spec FORTRAN string	*/
    FORSTR 		*host_spec,	/* out: host spec FORTRAN string*/
    TAEINT		*filemode,	/* in: filemode			*/
    TAEINT		*status	/* out: status			*/

 );
FUNCTION VOID xzdash 
(
    FORSTR		*tae_spec,	/* in: FORTRAN string		*/
    FORSTR		*host_spec,	/* out: FORTRAN string		*/
    TAEINT		*filemode,	/* in: filemode			*/
    TAEINT		*status	/* out: status			*/

 );
FUNCTION VOID xzstd
(
    FORSTR 		*tae_spec,	/* in: tae spec FORTRAN string	*/
    FORSTR 		*host_spec,	/* out: host spec FORTRAN string*/
    TAEINT		*filemode,	/* in: filemode			*/
    TAEINT		*status	/* out: status			*/

 );
FUNCTION CODE q_prep
(
 struct PARBLK	*p,		/* in/out: PARBLK		*/
 TEXT		name[],		/* in: paramter name		*/
 FUNINT		count,		/* in: proposed parm count	*/
 FUNINT		type,		/* in: parm type		*/
 FUNINT		mode,		/* in: update or add		*/
 struct VARIABLE **vv,		/* out: VARIABLE ptr		*/
 FUNINT         strsiz         /* in: string size              */ 
 );
FUNCTION VOID i_clse
(
 struct IFCB		*ifcb,		/* in/out: image file control block	*/
 FUNINT		disp		/* in:  I_DEL or I_SAVE			*/
 );
FUNCTION CODE i_herr 
(
 struct IFCB		*ifcb		/* in: image file control block	*/
 );
FUNCTION CODE i_opin
(
 struct IFCB	*ifcb,		/* out:	image file control block	*/
 FUNINT		lun,		/* in:  log. unit no. (ignored for UNIX)	*/
 TEXT		hostname[],	/* in:  host filespec of file to open	*/
 FUNINT		type		/* in:  open type (I_INPUT or I_INOUT)	*/
 );
FUNCTION CODE i_opou 
(
 struct IFCB	*ifcb,		/* out: image file control block	*/
 FUNINT		lun,		/* in:  log. unit no. (ignored for VMS)	*/
 TEXT		hostname[],	/* in:  host filespec of file to open	*/
 FUNINT		org,		/* in:  file organization I_CI or I_CS	*/
 FUNINT		chans,		/* in:  number of channels		*/
 FUNINT		lines,		/* in:  number of lines per channel	*/
 FUNINT		linsiz,		/* in:  bytes per line			*/
 FUNINT		labels,		/* in:  number of label records		*/
 FUNINT		labsiz		/* in:  bytes per label record		*/
 );
FUNCTION CODE i_rdlb
(
 struct IFCB	*ifcb,		/* in/out: image file control block	*/
 GENPTR		buffer,		/* out: user's receive data buffer	*/
 FUNINT		labnum		/* in:  label record number (1-n)...	*/
    				/* where n is number of label records	*/
 );
FUNCTION CODE i_read
(
 struct IFCB    *ifcb,		/* in/out: image file control block	*/
 GENPTR		buffer,		/* out: user's data receive buffer	*/
 FUNINT		channel,	/* in:  channel number of line		*/
 FUNINT		line,		/* in:  line number within channel	*/
 FUNINT		lines		/* in:  number of lines to read		*/
 );
FUNCTION CODE i_wait
(
 struct IFCB	*ifcb		/* in/out: image file control block	*/
 );
FUNCTION CODE i_write
(
 struct IFCB	*ifcb,		/* in/out: image file control block	*/
 GENPTR		buffer,		/* out: user's data buffer		*/
 FUNINT		channel,	/* in:  channel number of line		*/
 FUNINT		line,		/* in:  line number within channel	*/
 FUNINT		lines		/* in:  number of lines to write	*/
 );
FUNCTION CODE i_wrlb
(
 struct IFCB	*ifcb,		/* in/out: image file control block	*/
 GENPTR		buffer,		/* out: user's data buffer		*/
 FUNINT		labnum		/* in:  label record number (1-n)...	*/
				/* where n is number of label records	*/
 );
FUNCTION CODE q_wrtb
(
 TEXT	filespec[],	/* in: file specification		*/
 FUNINT	lun,		/* in: lun to use			*/
 struct PARBLK *p		/* in: V-block to write			*/
 );
FUNCTION  VOID  p_inifor
(
 struct   PARBLK *parblk	/* IN: parameter block */
 );
FUNCTION  CODE  p_forp
(
 struct  PARBLK	*parblk,		/* IN: parameter block */
 TEXT		line[],			/* OUT: formatted parameter rec */
 COUNT		length			/* IN: line length */ 
 );
FUNCTION VOID xzhost
(
    TEXT		tae_spec[],	/* in: tae spec, C string	*/
    TEXT		host_spec[FSPECSIZ+1],
    					/* out: host spec, C string 	*/
    TAEINT		*filemode,	/* in: filemode			*/
    TAEINT		*status	/* out: status			*/

 );
FUNCTION  VOID  p_herr
(
 struct  PARBLK *block,		/* in: parameter block  */
 CODE		*hcode		/* out: host error code	*/
 );
FUNCTION CODE p_rdb
(
 TEXT		filespec[],	/* in: file to read		*/
 COUNT		lun,		/* in: lun to use		*/
 struct PARBLK	*p,		/* out: PARBLK to read		*/
 FUNINT         bytes,		/* in: bytes in PARBLK		*/
 FUNINT		mode		/* in: P_CONT, P_ABORT		*/
 );
FUNCTION void oini_ins
(
 struct PARBLK	*parblk	/* in: initial parblk tm	*/
 );
FUNCTION CODE Vm_GetIntg 
(
    GENPTR              h,              /* in:  no-pool parblk handle   */
    TEXT		name[],		/* in:  parameter name		*/
    FUNINT		dimen,		/* in:  dimension of intg	*/
    TAEINT		intg[],		/* out: receives the values	*/
    COUNT		*count		/* out: value count		*/

 );
FUNCTION CODE Vm_GetReal 
(
    GENPTR              h,              /* in:  no-pool parblk handle   */
    TEXT		name[],		/* in:  parameter name		*/
    FUNINT		dimen,		/* in:  dimension of intg	*/
    TAEFLOAT		real[],		/* out: receives the values	*/
    COUNT		*count		/* out: value count		*/

 );
FUNCTION  TEXT  * s_fortxt
(
 FORSTR	*for_string
 );
FUNCTION CODE i_xtnd
(
 struct IFCB	*ifcb,		/* in/out: image file control block	*/
 FUNINT		channels,	/* in:  number of channels to add	*/
 FUNINT		lines		/* in:  number of lines to add per channel*/
 );
FUNCTION CODE f_rewind
(

 FAST  struct SFILE 	*f			/* in/out: opened SFILE structure */
 );
FUNCTION CODE f_close
(
 struct SFILE 	*f,		/* in/out: SFILE for open file	  */
 FUNINT		disp		/* in: F_KEEP or F_DELETE
					   disposition */
 );
FUNCTION CODE f_crack
(
 TEXT		fspec[],	/* in: host file specification	*/
 TEXT		deflibr[],	/* in: default library		*/
 TEXT		defname[],	/* in: default name		*/
 TEXT		deftype[],	/* in: default type		*/
 struct FSBLOCK	*fsblock,	/* out: constructed FSBLOCK	*/
 TEXT		errstr[]	/* out: 'at or near' string     */
					/* (if code is fail)		*/
 );
FUNCTION CODE f_bread
(

 struct SFILE 	*f,		/* in/out: SFILE for an opened file */
 GENPTR		buffer,		/* out: record read from file	    */
 FUNINT		bufsize,	/* in: size of user buffer 	    */
 COUNT		*recsize	/* out: Number of data bytes read   */
 );
UNSUPPORTED CODE Vm_ReadVm 
(

 struct 	VM_STRUCT	*vm,		/* in: existing vm 	    */
 struct SFILE		*f		/* in: opened file
						   context  */
 );
FUNCTION CODE f_write
(
 struct SFILE	*f,		/* in/out: SFILE for the file	*/
 TEXT 		string[]	/* in: record to be written	*/
);
FUNCTION VOID f_movpos
(
 struct POSCTX *from,	/* in: source of position context	*/
 struct POSCTX *to		/* out: destination			*/
 );
FUNCTION CODE f_bwrite(
     struct SFILE	*f,		/* in/out: SFILE for the file	*/
     GENPTR		buffer,		/* in: record to be written	*/
     COUNT		size);		/* in: size of buffer in bytes  */
FUNCTION VOID f_setpos
(
 struct SFILE 	*f,		/* in/out: opened SFILE		    */
 struct POSCTX	*posctx	/* in: postion context 		    */
);
UNSUPPORTED CODE Vm_WriteVm 
(
 struct	VM_STRUCT	*vm,		/* vm object to write	*/
 struct	SFILE		*sf		/* opened file		*/
);
FUNCTION VOID c_errtxt
( 
 struct PATH *path,			/* In/Out: Path for error string  */
 CODE	    err_code		/* In:	   errno value		  */
);
FUNCTION  VOID  parserec
(
 TEXT  	*recptr,       /* IN: ptr to a DIRECTIVE record */
 TEXT	*nameptr,	/* OUT: ptr to name */
 TEXT	field[]	/* OUT: associated field */ 
 );
FUNCTION CODE app_cmd 
(
 TEXT	inbuf[],		/* in/out: single input line buffer  */
 TEXT	cmdstr[],		/* in/out: accumulated cmd string    */
 BOOL	inquote		/* in/out: TRUE if quote in effect   */
 );
FUNCTION  VOID  flush_cmd
(
 struct SFILE 	*sptr,		/* IN: sfile of the command file    */
 TEXT		inbuf[],	/* IN/OUT: last line read from file */
 TAEINT		*numrec	/* OUT: number of records read	    */
 );
FUNCTION  VOID  d_incclose
(
    struct   DIRBLK	*db  			/* directive block	*/
);
FUNCTION CODE f_opnblk
(
 struct SFILE	*sfile,		/* in/out: sfile structure	*/
 FUNINT		lun,		/* in: lun to use		*/
 struct FSBLOCK	*fsblock,	/* in: fsblock 			*/
 FUNINT		access		/* in: F_READ, F_WRITE, ...	*/
 );
FUNCTION  VOID  getulib
(
 TEXT	userlib[]		/* output: user library */
 );
UNSUPPORTED struct VARIABLE *Vm_AllocVar
(
 struct SYMTAB *head	/* in/out: pointer to symbol table header 	*/
 );
FUNCTION  CODE  f_brewrite
(
 struct SFILE	*f,		/* in/out: SFILE for the file	*/
 GENPTR		buffer,		/* in: record to be written	*/
 COUNT		size		/* in: size of buffer in bytes  */
 );
FUNCTION CODE chk_parblk 
(
 struct PARBLK	*p		/* in/out: PARBLK pointer	*/
 );
FUNCTION  VOID get_time
(
    TEXT	atime[]			/* output: current asci time */
 );
UNSUPPORTED CODE Vm_ParblkOut
(
 struct SFILE	*sf,		/* in: file context		*/
 struct LARGE_PARBLK	*par,		/* in/out: parblk		*/
 struct VARIABLE	*var		/* in: the variable to add	*/
);
FUNCTION  VOID  getsession
(
    TEXT	session[]			/* output: session id	*/
 );
UNSUPPORTED CODE Vm_st2blk
(
 GENPTR              h,              /* in: handle to nopool parblk  */
 struct LARGE_PARBLK	*p,		/* out: PARBLK			*/
 FUNINT              pool_size      /* in: pool size of parblk      */
 );
FUNCTION  CODE  f_subst
(
 TEXT        *fspec,         /* in: file spec                        */
 TEXT        *subspec       /* out: file spec after substitution    */
 );
FUNCTION  CODE  m_fpname
(
 TEXT		name[],			/* in: variable name	*/
 TEXT  		line[],			/* IN/OUT: formatted parameter rec */
 COUNT		length			/* IN: line length */
 );
FUNCTION COUNT f_spec
(
 struct FSBLOCK	*fsblock,		/* in: file spec block	*/
 TEXT		fspec[FSPECSIZ+1]	/* out: VMS file spec	*/
 );
struct VARIABLE  *p_find
(
 struct PARBLK	*block,		/* in: parblk			*/
 TEXT		name[]		/* in: name of parameter	*/
 );
FUNCTION TEXT *q_save
(
 ALIGN	pool[],			/* restrcted storage		*/
 TEXT	string[]		/* string to copy		*/
 );
UNSUPPORTED TEXT *Vm_Save 
(
 TEXT	string[]		/* string to copy		*/
);
FUNCTION struct VARIABLE *q_alloc
( 
 struct SYMTAB	*symtab,	/* in: symtab to link to	*/
 ALIGN		pool[],		/* in: restricted storgae block	*/
 TEXT		name[],		/* in: name of variable		*/
 FUNINT		type,		/* in: variable type		*/
 FUNINT		count,		/* in: number of values		*/
 FUNINT		strsiz		/* in: string size		*/
  );
UNSUPPORTED GENPTR Vm_AllocValid
(
 FAST struct VARIABLE *v,	/* in/out: ptr to var adding valid struct*/
 FUNINT lcount		/* in:  number of valid ranges	     */
 );
FUNCTION CODE getval
(
 FAST struct SYNBLK *sb,	/* in/out: syntax block		*/
 FAST TEXT	*value[],	/* out: array of pointers to strings	*/
 FUNINT		maxval,		/* in:  dimension of value	*/
 COUNT		*count		/* out: actual number of values:*/
				/* -1 = no value; 0 = --;	*/
 );
FUNCTION VOID free_val
(
 FAST TEXT	*value[],	/* in/out: array of pointers to strings	*/
 FUNINT 	count		/* in/out: actual number of values	*/
 );
FUNCTION CODE irange
(
 TEXT	string[],		/* integer range string		*/
 TAEINT	*ilow,			/* out: low value		*/
 TAEINT	*ihigh			/* out: high value		*/
);
FUNCTION CODE rrange
(
 TEXT	string[],		/* integer range string		*/
 TAEFLOAT	*rlow,			/* out: lower value		*/
 TAEFLOAT	*rhigh			/* out: high value		*/
 );
UNSUPPORTED CODE  Vm_CopyVar
(
    FAST struct VARIABLE *vin,	/* in: VARIABLE struct to move		*/
    FAST struct VARIABLE *vout	/* in: VARIABLE struct to move to	*/

 );
FUNCTION  CODE  m_forp
(
    struct  FORCTX	*forctx, /* IN: pointer format context block */
    TEXT		line[],	 /* OUT: formatted parameter rec */
    COUNT		length	 /* IN: line length	     */
);
UNSUPPORTED CODE Vm_CopyValid
(
    FUNINT		type,		/* in: variable type		*/
    GENPTR		in,		/* in: valid structure		*/
    GENPTR		out,		/* out: valid structure 	*/
    GENPTR		pool		/* in: NULL for tae_allocation  */
					/* or pool for r_allocation     */

 );
FUNCTION CODE q_wrtblk
(
    struct SFILE	*f,		/* in: opened file		*/
    struct PARBLK 	*p		/* in: PARBLK to write		*/
);
UNSUPPORTED COUNT Vm_ValueSize
(
    FUNINT	type		/* V_STRING, V_REAL, V_INTEGER		*/

 );
FUNCTION CODE p_real
(
 struct PARBLK	*block,		/* in:  parameter block			*/
 TEXT		name[],		/* in:  parameter name			*/
 FUNINT		dimen,		/* in:  dimension of intg		*/
 TAEFLOAT	real[],		/* out: receives the values		*/
 COUNT		*count		/* out: value count			*/
 );
FUNCTION CODE p_intg 
(
 struct PARBLK	*block,		/* in:  parameter block			*/
 TEXT		name[],		/* in:  parameter name			*/
 FUNINT		dimen,		/* in:  dimension of intg		*/
 TAEINT		intg[],		/* out: receives the values		*/
 COUNT		*count		/* out: value count			*/
 );
FUNCTION CODE p_string
(
 struct PARBLK	*block,		/* in:  parameter block			*/
 TEXT		name[],		/* in:  parameter name			*/
 TEXT 		***sptr,	/* out: pointer to string vector	*/
 COUNT		*count		/* out: number of strings		*/
 );
FUNCTION CODE s_for2c
(
 FORSTR		*for_string,		/* in: FORTRAN string		*/
 TEXT		c_string[STRINGSIZ+1],	/* out: C string		*/
 FUNINT		index			/* in: index in fortran string	*/
						/* (zero if for_string scalar)	*/
 );
FUNCTION CODE hierarchy
(
    struct SFILE	*sfile,		/* out: SFILE to use for open		*/
    TEXT		verbstring[],	/* in: command string: may be filespec	*/
    TEXT		type[],		/* in: default file type		*/
    FUNINT		lun,		/* in: lun to use for file open		*/
    struct FSBLOCK	*fsblock,	/* out: FSBLOCK for PDF where found	*/
    TEXT	errstr[STRINGSIZ+1]	/* out: 'at or near' string		*/

 );
FUNCTION struct VARIABLE *lookex
(
    struct SYMTAB *head,		/* in: symbol tab to search	*/
    TEXT name[]			/* in: name to find		*/

);
FUNCTION VOID put_stdout
(
    TEXT		record[]	/* in: record to put		*/

);

FUNCTION VOID tmmsg
(
    FUNINT		scode,		/* in:  status code to set		*/
    TEXT		msg[],		/* in:  message control text		*/
    TEXT		key[],		/* in:  message key			*/
    uintptr_t		a1,	/* in:  ints or str ptrs	*/
    uintptr_t           a2,
    uintptr_t           a3,
    uintptr_t           a4,
    uintptr_t           a5
 );
void make_upper_case(char *out,char *in);
int i_search_name
(
 char *tape[],			/* In: pointers to "name=tape" strings	*/
 int count,			/* In: size of tape table		*/
char *name			/* In: name to find
				   */
 );
int i_search_device
(
 char *tape[],			/* In: pointers to "name=tape" strings	*/
 int count,			/* In: size of tape table		*/
char *device			/* In: device string to find
				   */
 );
void i_crack
(
 char *entry,		/* In: entry of form "name=device"	*/
 char *name,		/* Out: symbolic name			*/
char *device		/* Out: device name in entry		*/
 );

FUNCTION CODE set_value
(
    struct VARIABLE	*v,		/* structure to be updated	*/
    GENPTR		value,		/* in: pointer to value vector	*/
    FUNINT		count		/* in: vector count		*/

 );
FUNCTION CODE itr_lookup
(
    TEXT		cmd[],		/* in: command 		*/
    TEXT		subcmd[],	/* in: subcommand	*/
    struct ITRCMD	**itrcmd	/* out: entry pointer	*/

 );
FUNCTION COUNT t_highlight
(
 TEXT	string[]		/* in/out: string to highlight	*/
 );
FUNCTION VOID cmd_noscreen
(
    CODE 	archive,		/* in: A_MENU, A_TUTOR, A_LASTCMD*/
    FAST TEXT	pmtbuf[],		/* in:  prompt buffer		*/
    FAST TEXT	cmdstr[]		/* out: command string		*/

 );
FUNCTION  CODE  line_noscreen
(
    CODE 	archive,		/* in: A_MENU, A_TUTOR, A_LASTCMD    */
    TEXT	prompt[],		/* IN: prompt string		     */
    TEXT  	line[],			/* OUT: line from interactive source */
    CODE	*term			/* OUT: line terminator 	     */

 );
FUNCTION VOID m_put
(
    TEXT 	control[],		/* in: control string		*/
    TEXT	key[],			/* in: message key		*/
    uintptr_t	a1,		/* in: integers or string ptrs	*/
    uintptr_t   a2,
    uintptr_t   a3,
    uintptr_t   a4,
    uintptr_t   a5
 );
FUNCTION CODE bldcmd
(
    TEXT	inbuf[],		/* in/out: single input line buffer	*/
    TEXT	cmdstr[]		/* in/out: accumulated cmd string	*/

 );
FUNCTION VOID history_save
(
    TEXT	cmdstr[]		/* in: string at least MAXSTRSIZ */

 );
FUNCTION CODE sl2write
(
    TEXT record[],	/* input: record to be written */
    BOOL breakup	/* input: TRUE if rec needs breaking up w/cmdindex[] */

 );
FUNCTION VOID overr (void);
FUNCTION CODE help_do 
(
    struct CONTXT	*cpctx,		/* in:  containing proc context	   */
    struct CONTXT	*npctx		/* in:  proc ctx for HELP command  */

 );
FUNCTION CODE f_open
(
 struct SFILE 	*f,		/* out: file context block	*/
 FUNINT 	lun,		/* in: lun to use for file	*/
 TEXT 		library[],	/* in: name of library		*/
 TEXT 		name[],		/* in: name of file		*/
 TEXT 		type[],		/* in: type of file. no period.	*/
 FUNINT 	access		/* in: F_READ, F_WRITE, 	*/
					/* F_EXTEND, F_OVER		*/
 );
FUNCTION CODE proc_help
(    			    
 struct   SFILE	*fctx,		/* In: file context block for opened file			*/
 TEXT  		curlib[],	/* In: library name of opened file	*/
 TEXT		libname[],	/* In: library name for LHS of header	 */
 TEXT		procname[],	/* In: proc name for LHS of help display */
 TEXT		subname[],	/* In: name of subcommand, or NULL if none	*/
 TEXT		parmname[],	/* In/out: name of parm, or NULL if none	*/
 TEXT		type[],		/* In: proc/menu/command/global	*/
 struct  HELPBLK	*helpblk	/* out: help output control block    */
 
);
FUNCTION VOID initab
(
    struct SYMTAB *head		/* header to init		*/

 );
FUNCTION CODE movest 
(
    struct SYMTAB	*st1,		/* in:  symbol table to move from	*/
    struct SYMTAB	*st2		/* out: symbol table to move to		*/

 );
FUNCTION CODE substitute 
(
    char*		cmdstr,  /* [siz] */	/* in/out: command string*/
    int		        siz,			/* in:  dim-1 of cmdstr  */
    GENPTR	        pctx,			/* in:  proc context	 */
    int		        report			/* in:  true if err msg report	*/
						/*      is desired		*/
 );
FUNCTION CODE updtab 
(
    FAST struct SYMTAB	*symtab,	/* in/out: symbol table			*/
    FAST struct SYNBLK	*sb		/* in/out:  syntax block		*/

 );
FUNCTION VOID  deltab
(
    FAST struct SYMTAB *head		/* in/out: symbol table header		*/

 );
FUNCTION CODE inictx 
(
    FAST struct CONTXT	*pctx		/* in/out: proc context block		*/

 );
FUNCTION CODE cmd_parse 
(
    struct SYNBLK	*sb,			/* out: syntax block	*/
    TEXT		cmdstr[CMDLINSIZ+1],	/* in/out: command str	*/
    TEXT		*label,			/* out: command label	*/
    TEXT		cmd[FSPECSIZ+1],	/* out: command		*/
    TEXT		subcmd[SUBCMDSIZ+1]	/* out: subcommand	*/

 );
FUNCTION VOID clr_latch 
(
    struct CONTXT	*pctx		/* in:  proc context			*/

 );
FUNCTION CODE memtab 
(
    struct RESIDVAR 	*resvar,	/* table of resident variables		*/
    FUNINT		numres,		/* number of resident variables		*/
    struct SYMTAB	*s		/* out: symbol table header		*/

 );
FUNCTION VOID ini_status (void);
FUNCTION VOID clsctx 
(
    struct CONTXT	*pctx		/* in:  proc context		*/

 );
FUNCTION VOID clssav 
(
    struct CONTXT	*pctx		/* in/out: proc context			*/

 );
FUNCTION CODE get_nowhite 
(
    struct SYNBLK	*sb,			/* in/out: syntax block	*/
    TEXT		token[TOKESIZ+1]	/* out: token		*/

 );
FUNCTION CODE subab 
(
    struct SUBCMD	*subptr,	/* in:  1st struct in SUBCMD chain	*/
    TEXT		name[],		/* in:  name of subcommand to find	*/
    FAST struct SUBCMD	**subc		/* out: SUBCMD found (NULL unless SUCCESS)*/

 );
FUNCTION BOOL is_passthru(char *cmd);
FUNCTION CODE prccmd 
(
    TEXT   cmdstr[CMDLINSIZ+1],		/* in/out:  command string		*/
    FAST struct CONTXT	*procctx	/* in:  current proc context		*/

 );
FUNCTION struct SUBCMD *subex 
(
    struct SUBCMD	*subptr,	/* in:  1st struct in chain to search	*/
    TEXT		name[]		/* in:  name to find			*/

 );
FUNCTION CODE opnsav 
(
    struct CONTXT	*pctx		/* in/out: proc context			*/

 );
FUNCTION CODE opn_tutor 
(
    struct SFILE	*pdf,		/* in/out: sfile opened to the pdf  */
    struct CONTXT	*context,	/* in: context as set by opnpdf	    */
    FUNINT		screen_select,	/* in: FORCE_SCREEN, FORCE_NOSCREEN,*/
					/* or NOFORCE			    */
    struct VARIABLE	*vpreface,	/* in: preface strings (or NULL)    */
    FUNINT		dynamic	/* TRUE if dynamic tutor	    */

 );
FUNCTION CODE sub_tutor 
(
    struct SFILE	*pdf,		/* in/out: sfile of opened pdf	*/
    struct CONTXT	*context	/* in/out: proc context	with...	*/
    					/* subcommand chain built	*/

 );
FUNCTION VOID cls_tutor 
(
    struct SFILE	*pdf,		/* in/out: sfile of pdf	*/
    struct CONTXT	*context	/* in/out: proc context	*/

 );
FUNCTION CODE tutor 
(
    struct SFILE	*pdf,		/* in/out: opened pdf		*/
    struct CONTXT	*context	/* in/out: context block	*/

 );
FUNCTION CODE plcini 
(
    struct CONTXT	*pctx		/* in/out:  proc context	*/

 );
FUNCTION CODE move_locals
(
    struct PARBLK	*p,		/* in: parblk		*/
    struct SYMTAB	*locst		/* out: symbol table	*/

 );
FUNCTION CODE pssub
(
    struct PARBLK	*parblk,	/* in:  parameter block from process */
    struct SFILE	*pdf,		/* in/out: PDF			*/
    struct CONTXT	*ctx,		/* in/out: context to get subc for */
    FUNINT		dash_present, /* in: TRUE if "-" in process's cmd line*/
    TEXT		sub_str[]	/* out: user's cmd line following subc*/

 );
FUNCTION CODE parmrg 
(
    struct SYMTAB	*st1,		/* in:  1st symbol table		*/
    struct SYMTAB	*st2,		/* in/out: sym tab to merge into	*/
    FUNINT		mode		/* in:  FULLSET or SUBSET	*/
					/* VM_VALID may be or'd here to */
					/* request VALID merging	*/
 );
FUNCTION CODE parcmp 
(
    struct SYMTAB	*st1,		/* in:  1st symbol table		*/
    struct SYMTAB	*st2,		/* in:  2st symbol table		*/
    TEXT		errmsg[STRINGSIZ+1]	/* out: error msg string	*/

 );
FUNCTION CODE reini_ctx 
(
    struct CONTXT	*ctx		/* in/out: proc context to reset	*/

 );
FUNCTION CODE psparm
(
    struct SFILE	*pdf,		/* in/out: PDF for the proc	*/
    struct CONTXT	*ctx,		/* in/out: parameter context	*/
    TEXT		held_msg[],	/* in: possible previously gen'd EM */
    TEXT		held_key[]	/* in: possible prev gen'd error key */

 );
FUNCTION CODE  vcopy
(
    FAST struct VARIABLE *vin,	/* in: VARIABLE struct to move		*/
    FAST struct VARIABLE *vout	/* in: VARIABLE struct to move to	*/

 );
FUNCTION struct VARIABLE *allvar
(
    struct SYMTAB *head	/* in/out: pointer to symbol table header 	*/

 );
FUNCTION CODE pdftab 
(
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*file		/* in/out: PDF file control block	*/

 );
FUNCTION CODE parenval 
(
    struct SYNBLK	*sb		/* in/out: syntax block			*/

 );
FUNCTION CODE repval 
(
    FAST struct VARIABLE *v,		/* in/out: symbol table entry		*/
    FAST TEXT		*value[],	/* in:  val str ptrs (vals to repl with)*/
    FUNINT		count,		/* in:  count of values for this parm	*/
    FUNINT		by_name	/* in: TRUE if value[0] is the name of	*/
					/* a referenced variable		*/
					
 );
FUNCTION CODE set_string 
(
    struct VARIABLE	*v,		/* in/out:  variable to set		*/
    TEXT		string[]	/* in:  value to set variable to		*/

 );
FUNCTION struct ACB *find_oldest(void);
FUNCTION  CODE  asy_rcv_unint 
(
    struct      TCB     *tcb,           /* in: task control block            */
    GENPTR              block,          /* in: address of receive block      */
    FUNINT              blksize        /* in: number of bytes expected      */
    
 );
FUNCTION  CODE  asy_recs
(
    struct      TCB     *tcb,           /* in: task control block */
    GENPTR              block,          /* in: address of receive block      */
    FUNINT              blksize,        /* in: number of bytes expected      */
    VOID                (*actadr)(void),    /* in: address of an action routine  */
    GENPTR              actparm        /* in: action routine parm (ignored) */

 );
FUNCTION BOOL e_occur
(
 struct ECB *ecb
 );
FUNCTION CODE set_component
(
    struct VARIABLE	*v,		/* structure to be updated	*/
    GENPTR		value,		/* in: pointer to value vector	*/
    FUNINT		index		/* in: TCL index (value of 1 to  count)	*/

 );
FUNCTION  CODE  set_signal
(
    FUNINT		signum,		/* signal number	   */
    void		(*sig_hand)(int)
				/* signal handler function */

 );
FUNCTION  VOID sig_io
( 
 int		sig	/* Signal number */
  );
FUNCTION CODE t_attn
(
 struct ECB *ecb		/* in/out: ecb to enable	*/
 );
FUNCTION struct ACB *find_id 
(
    FUNINT		pid	/* in: job id			*/

 );
FUNCTION VOID asy_rcv_done 
(
    struct ACB		*acb		/* in: pointer to acb		    */

 );
FUNCTION CODE bld_pdf_ctx 
(
    struct CONTXT	*ctx,		/* in: context of executing process  */
    struct PARBLK	*parblk,	/* in/out: parameter block	     */
    struct CONTXT	*dctx,		/* out: context block for tutor      */
    struct SFILE	*dynpdf,	/* out: address of dynamic pdf SFILE */
    BOOL		*dash_present	/* out: TRUE if subcommand indicated */

 );
FUNCTION CODE gp_ins 
(
    struct SFILE	*pdf,			/* in: SFILE for opened pdf*/
    struct CONTXT	*ctx			/* in/out: dynamic context */

 );
FUNCTION CODE pack_parm
(
    struct SYMTAB	*symtab,	/* in: symbol table to use	*/
    struct PARBLK	*p,		/* out: PARBLK			*/
    FUNINT              pool_size      /* in: pool space size          */
 );
FUNCTION VOID rel_rsrc 
(
    struct ATCB         *atcb

 );
FUNCTION VOID abort_acb
(
    struct ACB          *acb                   /* in: the acb              */

 );
FUNCTION  VOID  block_opint(void);
FUNCTION  CODE  upsymb 
(
    struct  CONTXT 	*ctx,		/* in: symbol table		*/
    struct  VARIABLE	*var		/* in: variable 		*/

 );
FUNCTION  VOID  e_clear
(
 struct  ECB	  *ecb		/* ptr to event control block	*/
 );
FUNCTION CODE send_msg_w 
(
    struct  ACB		*acb,    	/* in: acb for the job to send the msg	*/
    GENPTR		msg,		/* in: the message			*/
    TAEINT		msglen		/* in: length				*/

 );
FUNCTION VOID dynget 
(
    struct CONTXT	*ctx,		/* in: context of executing process */
    struct PARBLK	*parblk,	/* in/out: parameter block	    */
    FUNINT              pool_size,      /* in: size of parblk.pool      */
    BOOL		asytut_req	/* in: async or sync tutoring ? */

 );
FUNCTION CODE run_local 
(
    struct ACB		*acb,		/* in: ACB pointer		   */
    struct CONTXT	*cmdctx	/* in: context of the command	   */

 );
FUNCTION CODE run_local_process 
(    
    struct ACB		*acb,		/* in: ACB pointer		   */
    struct CONTXT	*cmdctx	/* in: context of the command	   */

);
FUNCTION CODE host_job 
(
    struct SFILE        *job,           /* in/out: SFILE opened to job file   */
    struct CONTXT       *cmdctx,        /* in: command context                */
    FUNINT              user_proc      /* in: TRUE if job file for user proc */

 );
FUNCTION CODE job_ins 
(
    struct SFILE	*jobfile,		/* in/out: already open job file*/
    struct CONTXT	*cmdctx		/* in:  command context		*/

 );
FUNCTION CODE bld_tcl 
(
    struct CONTXT	*cmdctx,	/* in: command context 		*/
    struct SFILE	*job,		/* in/out: SFILE for job	*/
    FUNINT		run_type	/* in: type of job:		*/
					/*     ASYNC or BATCH		*/

 );
FUNCTION VOID tmierr 
(
    FUNINT		number		/* in:  error number		*/

 );
FUNCTION CODE create_async 
(
    FAST struct ATCB    *atcb,          /* in/out: address of an atcb      */
    FUNINT              mb_size,        /* in: size to use for mailbox     */
    TEXT                procname[],     /* in: name of proc to be run      */
    FUNINT              job_type,       /* in: ASYNC, ASYNC_PROCESS, REMOTE */
    TEXT                job_spec[],     /* in: ASYNC or REMOTE: .job filespec */
                                        /*     ASYNC_PROCESS: filespec of executable */
    TEXT                log_spec[]     /* in: ASYNC: output log file name */
                                        /*     ASYNC_PROCESS: SYS$OUTPUT file */

 );
FUNCTION CODE send_ctx_msg 
(
    struct ACB		*acb,		/* in: ACB pointer		*/
    TEXT		file_spec[],	/* in: file spec for msg	*/
    FUNINT		msg_type	/* in: message type		*/

 );
FUNCTION CODE addstr2 
(
    struct SYMTAB	*st,		/* in/out: symbol table		*/
    TEXT		name[],		/* in:  name of variable to add	*/
    FUNINT		maxc,		/* in: max value count		*/
    FUNINT		count,		/* in: current value count	*/
    TEXT		*val[],		/* in:  string value vector	*/
    FUNINT		class		/* in: class (for v_class)	*/

);
FUNCTION VOID async_stdout 
(
    struct CONTXT       *cmdctx,        /* context block for the process */
    TEXT                *stdout_string /* in: STDOUT value or NULL if   */
                                        /* no STDOUT qualifier present   */

 );
FUNCTION CODE package
(
   	struct CONTXT	*ctx,		/* input: current context block	*/
    	struct PARBLK 	*par,		/* output: parameter block	*/
	FUNINT	        pool_size,	/* input: size of PARBLK.pool   */
	FUNINT		scope		/* input: VM_VALID or zero      */

 );
FUNCTION  CODE  asy_snds
(
    struct TCB          *tcb,           /* in: task control block       */
    GENPTR              block,          /* in: data block to send       */
    FUNINT              blksize        /* in: number of bytes to send  */

 );
FUNCTION  BOOL exclude
(
    struct  VARIABLE 	*v		/* in: pointer to global variable */

 );
FUNCTION CODE chk_vector
(
    struct VARIABLE	*v,	/* in: VARIABLE to check against	*/
    FUNINT		type,	/* in: type of value			*/
    GENPTR		value,	/* in: pointer to value vector to check	*/
    FUNINT		count,	/* in: count of value vector		*/
    BOOL		compiling  /* in: compiling a proc ? */

 );
FUNCTION  VOID  wait_hold
(
 FUNINT	msec		/* time in milli-seconds  	*/
 );
FUNCTION struct VARIABLE *usearch 
(
    TEXT		name[],		/* in:  variable name		*/
    FAST struct CONTXT	*pctx		/* in:  proc context		*/

 );
FUNCTION VOID  get_pid(long *pid);
FUNCTION CODE save_pfile 
  (
    struct SFILE	*sf,		/* in: pointer to opened file context*/
    FUNINT		msgtype,	/* in: message type for p.msgtyp     */
    struct VARIABLE	*vector[],	/* in: array of variables to save    */
    FUNINT		vcnt,		/* in: number of vars in vector      */
    struct SYMTAB 	*symtab1,	/* in: ptr to array of symtab
					   ptrs   */
    struct SYMTAB 	*symtab2,	/* in: ptr to array of symtab
					   ptrs   */
    struct SYMTAB 	*symtab3	/* in: ptr to array of symtab
					   ptrs   */
   );
FUNCTION CODE job_ins1 
(
    struct SFILE	*jobfile,		/* in/out: already open job file*/
    struct CONTXT	*cmdctx		/* in:  command context		*/

 );
FUNCTION  CODE  cmdmatch 
(

    TEXT	incmd[],		/* in: input command		    */
    struct VARIABLE **defv,		/* out: $DEFCMD VARIABLE of match   */
    COUNT	*index			/* out: index to matching string    */


 );
FUNCTION VOID synerr
(
 FAST struct SYNBLK	*sb,		/* in/out: syntax block			*/
 FAST TEXT		*es		/* in:  error message string		*/
 );
FUNCTION struct VARIABLE *search 
(
    TEXT		name[],		/* in:  variable name		*/
    struct CONTXT	*pctx		/* in:  proc context		*/

 );
FUNCTION CODE f_name
(
 TEXT	fspec[],		/* in: host file specifiation	*/
 TEXT	name[FNAMESIZ+1]	/* out: file name field		*/
 );
FUNCTION CODE f_libr
(
 TEXT	fspec[],		/* in: host file specification	    */
 TEXT	library[FLIBRSIZ+1]	/* out: library field if any	    */
					/* if this address is NULL, then ok */
 );
FUNCTION CODE f_type
(
 TEXT	fspec[],		/* in: host file specifiation	*/
 TEXT	type[FTYPESIZ+1]	/* out: file type field		*/
 );
FUNCTION CODE f_attr
(
 TEXT	fspec[],		/* in: host file spec		*/
 TEXT	attr[]			/* out: attribute field		*/
 );
FUNCTION struct SUBCMD *allsub 
(
    struct SUBCMD	**subptr	/* in/out: ptr to 1st SUBCMD struct	*/

 );
FUNCTION CODE parhdrRead 
(
	struct SFILE	*sfile,		/* in: opened SFILE	*/
	struct PARHDR	*parhdr,	/* out: buffer to receive hdr */
	TEXT		errmsg[STRINGSIZ+1],
	TEXT		errkey[STRINGSIZ+1]

 );
FUNCTION CODE gettitle 
(
    struct DIRBLK	*db,		/* in/out: directive block for help file*/
    struct TXTSTOR	*title,		/* out: title text in dynamic storage	*/
    TEXT		errmsg[],	/* out: error message			*/
    TEXT		key[]		/* out: error key			*/

 );
FUNCTION VOID addref 
(
    struct CONTXT	*pctx,		/* in/out: proc ocntext			*/
    TEXT		name[]		/* in:  name of global variable		*/

 );
FUNCTION CODE  specvcopy
(
    FAST struct VARIABLE *vin,	/* input: VARIABLE struct to move	*/
    FAST struct VARIABLE *vout	/* input: VARIABLE struct to move to	*/

 );
FUNCTION TEXT *allval
(
    FAST struct VARIABLE *v	/* in/out: variable to have values allocated  */

 );
FUNCTION CODE deep_value_copy 
(
    	struct VARIABLE	*sv,			/* in: source variable	*/
        struct VARIABLE *dv,			/* in: dest variable	*/
        TEXT		sname[],		/* in: source name	*/
        TEXT		dname[]		/* in: dest name	*/

 );
FUNCTION CODE pblk_out 
(
    struct SFILE	*sf,		/* in: file context		*/
    struct PARBLK	*par,		/* in/out: parblk		*/
    FUNINT		poolsize,	/* bytes in parblk.pool		*/
    struct VARIABLE	*var		/* in: the variable to add	*/

 );
FUNCTION CODE name_check
(
 TEXT		name[]		/* in: candidate name		*/
 );
FUNCTION struct VARIABLE *int_search 
(
    FAST TEXT		name[],		/* in:  variable name		*/
    FAST struct CONTXT	*pctx		/* in:  proc context		*/

 );
FUNCTION  struct VARIABLE * alpha_var
(
    TEXT	  name[],	/* in: name of variable to be added	  */
    struct SYMTAB *head	/* in/out: pointer to symbol table header */

 );
FUNCTION CODE bld_valid
(
    struct SYMTAB	*st,		/* in: symbol table of var	*/
					/* (for keyword validation)	*/
    struct VARIABLE	*v,		/* in/out: VARIABLE structure 	*/
    TEXT		*value[],	/* in: ranges in string format	*/
    FUNINT		count		/* in: count of value vector	*/

 );
FUNCTION  VOID delvar
(
    struct SYMTAB *head,		/* in/out: symbol table		*/
    FAST struct VARIABLE *v		/* in:  entry to delete		*/

 );
FUNCTION CODE trans_valid
(
    struct VARIABLE	*v		/* VARIABLE to transform	*/

 );
FUNCTION CODE opnpdf 
(
    struct CONTXT	*parctx,	/* in: parent proc context		*/
    struct CONTXT	*pctx,		/* in/out: proc context			*/
    TEXT		cmdstring[],	/* in: command string: may be a file spec */
    struct SFILE	*file		/* out: the file control block		*/

 );
FUNCTION CODE dp_ins 
(
    struct PARBLK	*parblk,		/* in: parblk from process */
    struct SFILE	*pdf,			/* in: opened PDF	   */
    struct CONTXT	*ctx,			/* in/out: dynamic context */
    FUNINT		dash_present		/* in: TRUE if '-' present */
 );
FUNCTION CODE t_output 
(
    FUNINT	line,			/* in: line number (start at 1)   */
    FUNINT	column,			/* in: column number (start at 1) */
    TEXT	string[]		/* in: string to write		  */

 );
FUNCTION CODE t_input 
(
    FUNINT	line,			/* in: line number (start at 1)   */
    FUNINT	column,			/* in: column number (start at 1) */
    TEXT	string[STRINGSIZ+1],	/* out: string typed		*/
    CODE	*term			/* out: terminator: 		*/
    					/* T_CR or T_ESCAPE		*/
 );
FUNCTION CODE t_1gettok (void);
FUNCTION VOID t_getpos 
(
    COUNT	*line,		/* out: line number	*/
    COUNT	*column	/* out: column number	*/

 );
FUNCTION CODE t_gettok (void);
FUNCTION CODE t_pos 
(    
    FUNINT	line,			/* in: line number (start at 1)   */
					/*  -1 means use current line	  */
    FUNINT	column			/* in: column number (start at 1) */

     );
FUNCTION CODE t_lclear
(
    FUNINT	line,		/* in: line to clear (-1 = current line)*/
    FUNINT	col		/* in: and start clearing at this col	*/
 );
FUNCTION CODE t_bell (void);
FUNCTION VOID high_value 
(
    struct VARIABLE	*v,		/* in: parameter to highlight	*/
    FUNINT		index,		/* in: index to highlight	*/
    BOOL		highlight	/* in: TRUE to highlight	*/
					/*     FALSE to turn off	*/

 );
FUNCTION void parser_init 
(
    struct PSTATE *statelist[],		/* in: vector of state ptrs	*/
    FUNINT	   n			/* in: entries in vector	*/

 );
FUNCTION 	VOID	stk_in 
(
    TINY	stack[],	/* IN/OUT: Stack to access */
    TINY	s_length,		/* IN: Allowable number of entries on stack */
    TINY	inival		/* IN: Initial value to pushed on the stack */

 );
FUNCTION CODE parser
(
    struct SYNBLK	*sb,		/* in: syntax block		*/
    struct PSTATE	*statelist[],	/* in: list of state pointers	*/
    FUNINT		state0,		/* in: index of pointer to start state	*/
    					/* (W.R.T. the statelist)	*/
    struct ERRMSG	*(*errmsg)	/* out: ptr to error msg struct	*/

 );
FUNCTION CODE popvstack
(
    struct	VALUE	*value		/* out: the value popped	*/

 );
FUNCTION 	CODE	toptin 
(
    TINY	stack[]	/* IN: Stack to access */

 );
FUNCTION CODE int_fl2i
(
 TAEFLOAT	fl,		/* in: the floater		*/
 TAEINT		*intgr		/* out: the integer		*/
 );
FUNCTION CODE pushvstack
(
    struct	VALUE	*value		/* in: the structure to push	*/

 );
FUNCTION CODE int_div
(
 TAEINT		in1,		/* in: first number to multiply	*/
 TAEINT		in2,		/* in: second number		*/
 TAEINT		*out		/* out: result			*/
 );
FUNCTION 	CODE	poptin 
(
    TINY	stack[]	/* IN/OUT: Stack to access */

 );
FUNCTION 	CODE	pushti 
(
    TINY	stack[],	/* IN/OUT: Stack to access */
    FUNINT	value	  	/* IN: Value to be pushed onto stack */

 );
FUNCTION struct TCLFUNC *fn_search
(
    TEXT		name[]		/* in: name of the function to find	*/

 );
FUNCTION CODE fl_sub
(
 TAEFLOAT		in1,		/* in: first number to multiply	*/
 TAEFLOAT		in2,		/* in: second number		*/
 TAEFLOAT		*out		/* out: result			*/
 );
FUNCTION CODE int_sub
(
 TAEINT		in1,		/* in: first number to multiply	*/
 TAEINT		in2,		/* in: second number		*/
 TAEINT		*out		/* out: result			*/
 );
FUNCTION struct VARIABLE  *defglobal 
(
    TEXT	name[],		/* in: name of global		*/
    FUNINT	type,		/* in: type 			*/
    FUNINT	size,		/* in: size if string		*/
    FUNINT	minc,		/* in: minimum count		*/
    FUNINT	maxc,		/* in: max count		*/
    FUNINT	count,		/* in: current count		*/
    GENPTR	vp,		/* in: current value vector	*/
    TEXT	*valid[],	/* in: valid strings		*/
    FUNINT	count_v	/* in: count of valid vector	*/
 );
FUNCTION VOID rm_init 
(
struct CONTXT		*p_ctx		/* level-0 context block */

 );
FUNCTION VOID rm_pa 
(
struct CONTXT		*p_ctx		/* proc context block */
 );
FUNCTION VOID rm_pt 
(
struct CONTXT		*p_ctx		/* proc context block */
 );
FUNCTION CODE psdyn
(
    struct PARBLK	*parblk,	/* in:  parameter block	*/
    struct SFILE	*pdf,
    struct CONTXT	*ctx,	/* in/out: context containing requested params*/
    FUNINT		dash_present	/* in:  TRUE if '-' in proc's cmd */

 );
FUNCTION VOID rm_term (void);
FUNCTION CODE mount_do
(
    struct CONTXT *procctx,	/* in/out: enclosing proc contxt	*/
    struct CONTXT *cmdctx	/* in/out: command contxt		*/

 );
FUNCTION CODE dismount_do
(
 struct CONTXT *procctx,	/* in/out: enclosing proc contxt	*/
 struct CONTXT *cmdctx	/* in/out: command contxt		*/
 );
FUNCTION CODE passthru_do(struct CONTXT *UNUSED(X1), struct CONTXT *UNUSED(X2));
FUNCTION CODE nopassthru_do(struct CONTXT *UNUSED(X1), struct CONTXT *UNUSED(X2));
FUNCTION CODE flag_do
(
    struct CONTXT *procctx,	/* in/out: proc context		*/
    struct CONTXT *cmdctx	/* in/out: command context	*/

);
FUNCTION CODE rm_do_usage 
(struct CONTXT	*procctx,	/* in: proc context	*/
struct CONTXT	*cmdctx	/* in: cmd context	*/
 );

FUNCTION CODE listproc_do
(
    struct CONTXT	*cpctx,		/* in:  current proc context	*/
    struct CONTXT	*npctx		/* in:  context of listproc cmd	*/

 );
FUNCTION CODE f_ophf
(
 struct SFILE 	*f,		/* out: file context block	*/
 FUNINT 	lun,		/* in: lun to use for file	*/
 TEXT		fspec[],	/* in: host file name		*/
 FUNINT 	access		/* in: F_READ, F_WRITE,F_EXTEND	*/
				 	/* F_OVER, or'ed with F_NOLOWER   */
 );
FUNCTION  VOID  d_init
(
 struct  DIRBLK	*db,		/* OUT: pointer to directive block */
 struct  SFILE	*sf,		/* IN:	pointer to file's SFILE    */
 TEXT libr[],		/* IN:  file library name	   */
 TEXT **sys_char,	/* IN:  ptr to system characteristics*/
 FUNINT char_cnt	/* IN:  count of characteristic strings	   */
 );
FUNCTION CODE prep_c_help 
(
    struct DIRBLK	*dirblk,	/* in/out: directive block		*/
    struct TXTSTOR	*title,		/* out: title text			*/
    TEXT		msg[],		/* error message (if any)		*/
    TEXT		key[]		/* error key (if any)			*/

 );
FUNCTION  CODE  d_dirctv
(
 struct  DIRBLK	*db,		/* IN: pointer to directive block */
 TEXT dirctv[],	/* OUT: directive type (including '.') */ 
 TEXT field[]	/* OUT: associated text */
 );
FUNCTION VOID fretxt 
(
    struct TXTSTOR	*block

 );
FUNCTION  CODE  d_search
(
 struct DIRBLK  *db,	 	/* IN: directive block		     */
 TEXT dirctv[],		/* IN: directive name  to search for */
 TEXT field[]		/* OUT: associated text 	     */
 );
FUNCTION  CODE  d_text
(
 struct	DIRBLK	*db,		/* IN: pointer to directive block */
 TEXT		record[]	/* OUT: text record */
 );
FUNCTION BOOL subchar
(
    FAST TEXT	*s			/* in:  string				*/
 );
FUNCTION CODE opnhlp 
(
    struct SFILE	*fctx,			/* out: opened help file context */
    TEXT		procstr[],		/* in/out: proc(-sub) name	*/
    TEXT		proc[],			/* out: proc name to help on	*/
    TEXT		sub[],			/* out: sub name to help on	*/
    TEXT		library[],		/* out: library name		*/
    FUNINT		*itrinsic,		/* out: TRUE if intrinsic cmd	*/
    struct  HELPBLK	*helpblk		/* out: help output control blk	*/

 );
FUNCTION CODE hard_help
(
    struct   SFILE	*fctx,		/* In: file context block for opened file	*/
    TEXT  		curlib[],	/* In: library name of opened file		*/
    TEXT		procname[],	/* In: proc name for LHS of help display	*/
    TEXT		outname[],	/* In: name of output file			*/
    struct  HELPBLK	*helpblk	/* out: help output control block		*/

 );
FUNCTION  CODE msg_help
(
    struct  SFILE	*fctx,		/* In: file context block for opened file			*/
    TEXT		msgkey[],	/* In: message key 		*/
    struct  HELPBLK	*helpblk	/* Out: help output control blk	*/

 );
FUNCTION  CODE genhelp
(
    struct SFILE	*fctx,		/* In: file ctx block for opened file */
    TEXT		libname[FLIBRSIZ+1],	/* library where file exists  */
    TEXT		type[8],	/* In: menu/gen  		      */
    TEXT		lefthead[],	/* In: left side string of help disp  */
    struct  HELPBLK	*helpblk	/* In/out: help output control block  */

 );
FUNCTION  VOID  wrterr
(
    TEXT	msg[],			/* IN: error message to output */
    TEXT	key[],			/* IN: message key */
    uintptr_t	A1,		        /* IN: integers or string
					   pointers */
    uintptr_t   A2,
    uintptr_t   A3,
    uintptr_t   A4,
    uintptr_t   A5

 );
FUNCTION CODE t_clear(void);
FUNCTION VOID disptxt 
(
    FUNINT		line,		/* in:  display line # to start		*/
    FUNINT		col,		/* in:  column number to start each line*/
    struct TXTSTOR	*block		/* in:  storage block containing dynam stored text*/

 );
FUNCTION VOID dsphdr 
(
    TEXT	dspname[],		/* in:  display name		*/
    TEXT	left_str[],		/* in:  left string for header	*/
    FUNINT	pagenum,		/* in:  page number 		*/
					/* (.le. 0  means no page nrs)	*/
    FUNINT	lastflag		/* in:  TRUE if last page 	*/

 );
FUNCTION  VOID  pos_scroll(void);
FUNCTION  CODE  	cmd_screen 
(
    CODE 	archive,		/* in: A_MENU, A_TUTOR, A_LASTCMD */
    TEXT	cmdstr[],		/* out: command string		  */
    CODE	*terminat		/* command line terminator        */

 );
FUNCTION CODE getvrb
(
 FAST struct SYNBLK *sb,    /* in/out: syntax block			*/
 TEXT		*verb	    /*out: verb				*/
 );
FUNCTION  CODE  intprm 
(
    FUNINT		maxparm,	/* in: max number of parameters (0/1) */
    TEXT		cmdnm[],	/* in: command name */
    TEXT		cmdstr[],	/* in:  the command string	*/
    TEXT		cmdprm[],	/* out: the command parameter	*/
    TEXT		msg[],		/* out: error message		*/
    TEXT		key[]		/* out: error key		*/

 );
FUNCTION BOOL f_isterm
(
 struct SFILE    *file
 );
FUNCTION CODE getpst 
(
    struct SFILE	*pdffil,	/* in/out: PDF file context	*/
    TEXT		sttmt[],	/* out: PDF statement		*/
    COUNT		*records,	/* out: number of records read	*/
    COUNT		*num_prec,	/* out: # comments, null lines	*/
					/*      preceding 1st real sttmt rec*/
    struct POSCTX	*first_rec	/* out: position of 1st record	*/

 );
FUNCTION CODE prsstt 
(
    TEXT		sttmt[CMDLINSIZ+1],	/* in/out:  PDF statement*/
    struct ITRCMD	**itrcmd,		/* out: ITRCMD for cmd	 */
    struct SYMTAB	*pdfstb		/* out: cmd symbol table */

 );
FUNCTION struct ITRCMD * intrin 
(
    FAST TEXT		*cmd		/* in:  command 			*/

 );
FUNCTION struct ITRCMD * itrsub
(
    TEXT		subcmd[],		/* in: subcommand	*/
    struct ITRCMD    	*p			/* in: first ITRCMD	*/

 );
FUNCTION  CODE  d_incopen
(
 struct  DIRBLK  	*db,		/* IN: directive block	*/
 struct  POSCTX	*curinc	/* IN: posctx of .include record */
 );
FUNCTION  VOID  left_fbld
(
    TEXT	libname[],		/* IN: library name		*/
    TEXT	fname[],		/* IN: proc/mdf  name		*/
    TEXT	subc[],			/* IN: subcmd name (ignor if null)*/
    TEXT	type[],			/* IN: mdf/proc/command	*/
    TEXT	lefthead[]		/* OUT: string to be displayed	*/

 );
FUNCTION VOID left_gbld
(
    TEXT		pdfname[],	/* in: the name of the pdf file		*/
    TEXT		varname[],	/* in: the name of the global variable		*/
    TEXT		type[],		/* in: global				*/
    TEXT		lefthead[]	/* out: left side of the header		*/

/* Actually we only build the part of the header that goes after the display name 	*/

 );
FUNCTION VOID left_pbld
(
    TEXT		procname[],	/* in: the name of the proc		*/
    TEXT		subcname[],	/* in: the name of the subcommand		*/
    TEXT		parmname[],	/* in: the name of the parm		*/
    TEXT		type[],		/* in: proc/command			*/
    TEXT		lefthead[]	/* out: left side of the header		*/

/* Actually we only build the part of the header that goes after the display name 	*/

 );
FUNCTION VOID left_sbld
(
    TEXT		procname[],	/* in: the name of the proc		*/
    TEXT		subcname[],	/* in: the name of the subcommand	*/
    TEXT		type[],		/* in: proc/command			*/
    TEXT		lefthead[]	/* out: left side of the header		*/

/* Actually we only build the part of the header that goes after the display name 	*/

 );
FUNCTION CODE chkend
(
 FAST struct SYNBLK	*sb	/*  in/out: syntax block		*/
 );
FUNCTION CODE evalexp
(
    struct SYNBLK	*synblk,	/* in: syntax block		*/
    struct CONTXT	*contxt,	/* in: proc context block	*/
    CODE		type,		/* in: integer, string, or real */
    GENPTR		*value,		/* out: pointer to the result, NULL if fail 	*/    
    COUNT		*count,		/* out: number of outputs	*/
    CODE		*term		/* out: terminator: EOS or S_COMSEP */

 );
FUNCTION CODE cmd_editor 
(
    CODE	archive,	/* in: A_NONE or A_LASTCMD		*/
    TEXT	line[],		/* out: user-typed line			*/
    TEXT 	prompt[],	/* in: prompt string			*/
    COUNT	line_num,	/* in: line number on screen		*/
				/*     (-1 if line number not known)	*/
    FUNINT	screen		/* TRUE if in screen mode		*/

 );
FUNCTION CODE tut_editor 
(
    TEXT	line[],		/* out: user-typed line			*/
    TEXT 	prompt[],	/* in: characters in prompt string	*/
    COUNT	line_num	/* in: line number			*/

 );
FUNCTION CODE menu_editor 
(
    TEXT	line[],		/* out: user-typed line			*/
    TEXT 	prompt[],	/* in: characters in prompt string	*/
    COUNT	line_num	/* in: line number			*/

 );
FUNCTION 	CODE	endfor_do 
(
    struct 	CONTXT	*proctx,	/* IN/OUT: Current context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: New context block */

 );
FUNCTION CODE endgbl_do 
(
    struct CONTXT	*cpctx,		/* in:  context of containing proc	*/
    struct CONTXT	*npctx		/* in:  proc ctx for END-GLOBAL command	*/

 );
FUNCTION CODE endif_do 
(
    struct	CONTXT	*proctx,	/* IN/OUT: Stack to access */
    struct	CONTXT	*cmdctx	/* IF command line context (w/ syntax block) */
 );
FUNCTION 	CODE	endloop_do 
(
    struct 	CONTXT	*proctx,	/* IN/OUT: Current context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: New context block (not used by endloop_do) */

 );
FUNCTION CODE endproc_do 
(
    struct CONTXT	*cpctx,		/* in/out: context of containing proc	*/
    struct CONTXT	*npctx		/* in:  ctx from END-PROC cmd (null)	*/

 );
FUNCTION CODE endsub_do 
(
    struct CONTXT	*cpctx,		/* in/out: context of containing proc	*/
    struct CONTXT	*npctx		/* in:  ctx from END-SUB cmd (null)	*/

 );
FUNCTION CODE version_do 
(
    struct CONTXT	*procctx,		/* in: proc context	*/
    struct CONTXT	*cmdctx		/* in: cmd context	*/

 );
FUNCTION CODE listcmd_do 
(
    struct CONTXT	*pc,		/* in: proc context	*/
    struct CONTXT	*c		/* in: command context	*/

 );
FUNCTION CODE fndsep
(
  FAST struct SYNBLK	*sb		/* in/out: syntax block		*/
 );
FUNCTION VOID delval
(
    TEXT	*pv,				/*in: pointer to values	*/
    FUNINT	type,				/*in: type of variable	*/
    FUNINT	count				/*in: current value cnt	*/

 );
FUNCTION CODE chk_component
(
    struct VARIABLE	*v,	/* in: VARIABLE to check against	*/
    FUNINT		type,	/* in: type of value			*/
    GENPTR		value	/* in: pointer to value vector to check	*/

 );
FUNCTION CODE t_distol (void);
FUNCTION CODE t_entol 
(
    TEXT	*fspec,		/* in: file spec for file to log to 	*/
    TEXT	**errmsg	/* out: pointer to errmsg		*/

 );
FUNCTION  VOID  sltime(void);
FUNCTION BOOL f_force_lower
(
 BOOL	flag		/* TRUE for force_lower	*/
 );
FUNCTION  VOID  addproc
(
    struct  CONTXT  *pctx,		/* IN: proc context */
    TEXT	    record[]		/* IN/OUT: formatted line */

 );
FUNCTION  CODE  put_outmsg
(
    TEXT	string[]		/* in: text string  to output	 */

 );
FUNCTION  VOID  slwrite
(
    TEXT		prefix[],		/* IN: prefix to line 	*/
    TEXT		line[]			/* IN: line to log    	*/


 );
FUNCTION VOID  m_pfmt
(
 TEXT 	control[],		/* in: control string		*/
 TEXT	key[],			/* in: message key		*/
 TEXT	mputrec[],		/* out: output string		*/
 uintptr_t a1,			/* in: integers or string ptrs	*/
 uintptr_t a2,
 uintptr_t a3,
 uintptr_t a4,
 uintptr_t a5
 );
FUNCTION  CODE  m_frname
(
 struct  VARIABLE *vv,			/* IN: pointer to a variable */
 TEXT  		line[],			/* IN/OUT: formatted parameter rec */
 COUNT		stdlen,		 	/* IN: standard line length  */
 COUNT		longlen		        /* IN: maximum line length   */
 );
FUNCTION  LONG  t_count(void);
FUNCTION BOOL search_vector 
(
    	TEXT	*string_vv[],		/* in: string value vector */
    	FUNINT	count,			/* in: number of strings   */
    	TEXT	search_string[]	/* in: string to search for */

 );
FUNCTION  CODE  entr_help
(
    struct   SFILE	*fctx,		/* IN: SFILE of file with entries   */
    TEXT		libname[],	/* IN: library name for file	    */
    TEXT     		filename[],	/* IN: name of file with entries    */
    struct   HELPBLK	*helpblk	/* IN/OUT: help control block	    */

 );
FUNCTION  VOID  wrttxt
(
    FUNINT	line,			/* IN: line number to write to */
    FUNINT	col,			/* IN: column number to write to */
    TEXT	text[],			/* IN: message text */
    FUNINT	blank			/* IN: True if rest to be blanked */

 );
FUNCTION VOID initxt 
(
    struct TXTSTOR	*block		/* out:  block to initialize		*/

 );
FUNCTION  CODE  d_cdirctv
(
 struct  DIRBLK	*db,		/* IN: pointer to dirctive block     */
 TEXT dirctv[],	/* OUT: directive type (including .) */
 TEXT field[]	/* OUT: associated text 	     */
 );
FUNCTION CODE addtxt 
(
    struct TXTSTOR	*block,		/* in/out: bloct to add a line of text to*/
    TEXT		str[]		/* in:  string to add			*/

 );
FUNCTION CODE async_abo 
(
    struct CONTXT	*procctx,		/* in: proc context	*/
    struct CONTXT	*cmdctx		/* in: command context  */

 );
FUNCTION CODE fill_value
(
    struct VARIABLE	*v,	/* in/out: VARIABLE structure	*/
    FUNINT		count	/* in: new value count		*/

 );
FUNCTION VOID async_exit (void);
FUNCTION  VOID  term_async(void);
FUNCTION CODE disable_recvar(struct CONTXT* ,struct CONTXT*);
FUNCTION VOID term_ins(void);
FUNCTION  CODE  get_parm_locpar
(			

    TEXT		send_file[],	/* in: name of parfile to send to tm  */
    TEXT		recv_file[],	/* out: name of pafile recved from tm */
    FUNINT		req_process,	/* in: TRUE if request from a process */
    CODE		*send_stat,	/* out: host_code if error in sending */
    CODE		*recv_stat	/* out: host_code if error in receive */

			);
FUNCTION CODE get_syms 
(
    TEXT		save_file[],		/* in: name of file to restore */
    struct  CONTXT	*ctx			/* in/out: context with symbols	*/

 );
FUNCTION CODE s_sh2i
(
 TEXT	s[],			/* hexadecimal numeric string	*/
 LONG	*i			/* out: the integer		*/
 );
FUNCTION CODE cpy_val
(
    FUNINT		type,		/* in: variable type		*/
    GENPTR		in,		/* in value(s)			*/
    GENPTR		out,		/* out value(s)			*/
    FUNINT		count		/* in: number of in values	*/

 );
FUNCTION BOOL true_part 
(
    struct CONTXT	*pctx		/* in: proc context			*/

 );
FUNCTION COUNT valid_size 
(    
    FAST  FUNINT type,		/* in: V_INTEGER, V_REAL, V_STRING	*/
    FAST  FUNINT count		/* in: number of ranges			*/

     );
FUNCTION COUNT valsize
(
    FUNINT	type		/* V_STRING, V_REAL, V_INTEGER		*/

 );
FUNCTION CODE f_readstring
(
 struct SFILE 	*f,		/* in/out: SFILE for an opened file  */
 COUNT	        maxsiz,	/* in: max string siz */
 TEXT  		string[]	/* out: record with EOS terminator   */
 );
FUNCTION  VOID  menmod 
(
    TEXT 	tmcmd[]		/* OUT: command string returned to TM */

 );
FUNCTION  struct  VARIABLE  *find_slot
(
    TEXT		name[],		/* in: variable name to match	 */
    struct  SYMTAB	*symtab	/* in: symbol table to look thru */

 );
FUNCTION VOID deltut 
(
    struct VARIABLE	*v		/* in/out: variable to deallocate from	*/

 );
FUNCTION BOOL IsParFile 
(
	struct SFILE *sfile
 );
FUNCTION CODE prc_par_file 
(
    	struct SFILE	*file,		/* opened PAR file	*/
        struct CONTXT	*ctx		/* contxt to build	*/

 );
FUNCTION CODE prc_compiled 
(
    struct SFILE	*file,		/* in/out: compiled PDF control block	*/
    struct CONTXT	*ctx		/* in/out: context for compiled PDF	*/

 );
FUNCTION  VOID  slproc
(
    struct  CONTXT   *context		/* in: context block for the proc */

 );
FUNCTION VOID pa_ins 
(
    struct CONTXT *pctx	/* in/out: context of proc	*/
 );
FUNCTION CODE chg_stdout
	(
    struct CONTXT	*procctx		/* in: current proc context	*/

	 );
FUNCTION CODE process 
(
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct ECB		*ecbi,		/* in/out: event blk for interrupts	*/
    struct CODE_ERR	*(*errmsg)	/* out: error msg if CODE is FAIL, else "" */

 );
FUNCTION CODE action_run 
(
    struct CONTXT *ctx		/* in/out: current proc context	*/

 );
FUNCTION CODE re_stdout
(
    struct CONTXT	*cmdctx	/* in: just completed 'new' context	*/

 );
FUNCTION VOID pt_ins 
(
    struct CONTXT *pctx	/* in/out: context of proc	*/
 );
FUNCTION  VOID  slterm(void);
FUNCTION CODE package
(
   	struct CONTXT	*ctx,		/* input: current context block	*/
    	struct PARBLK 	*par,		/* output: parameter block	*/
	FUNINT	        pool_size,	/* input: size of PARBLK.pool   */
	FUNINT		scope		/* input: VM_VALID or zero      */

 );
FUNCTION  CODE  act_task
(

TEXT	task_name[],		/* in:  task name	  	     */
FUNINT	parent_pid,		/* in:  parent pid		     */
TEXT 	*envtbl[],		/* in: environment table	     */
TAEINT	*read_pipe,		/* out: read end of up-pipe	     */
TAEINT	*write_pipe,		/* out: write end of down-pipe	     */
TAEINT	*child_pid		/* out: process id of activated task */
 );
VOID pipe_sig(int);
VOID int_sig(int);
FUNCTION CODE get_pblk_remote 
(
    struct CONTXT	*proc_ctx,	/* in: Context of executing proc    */
    struct PARBLK	*parblk,	/* in/out: parameter block	*/
    struct CODE_ERR	*(*errmsg)	/* out: pointer to err msg	*/

 );
FUNCTION   VOID  logdyn
(
    struct PARBLK	*parblk	/* in: parblk from subprocess	*/
 );
FUNCTION VOID dyncommand 
(
    struct CONTXT	*ctx,		/* in/out: parameter context	*/
    struct PARBLK	*parblk,	/* in/out: absolute parblk */
    int			pool_size

 );
FUNCTION   VOID  logmsg
(
    struct CONTXT	*ctx,		/* in: context of executing process */
    struct PARBLK	*parblk	/* in: parblk from subprocess	*/

 );
FUNCTION CODE outputs
(
    struct CONTXT	*procctx,	/* in: proc context		*/
    struct PARBLK	*parblk	/* in: PARBLK from process	*/

 );
FUNCTION VOID dynamic_ins 
(
    struct CONTXT	*pctx,		/* in/out: process CONTXT */
    struct PARBLK	*parblk	/* in: PARBLK from process*/

 );
FUNCTION CODE interrupt 
(
	struct CONTXT	*ctx		/* in: context of executing proc  */

 );
FUNCTION CODE getqlf
(
 struct SYNBLK	*sb,	/* in/out: syntax block			*/
 TEXT		qualstr[] /* out:  qualifier string		*/
 );
FUNCTION  VOID  disp_var
(
    struct  VARIABLE 	*v,		/* in: variable being displayed		*/
    TEXT		var_name[]	/* in: name to use in var display	*/

 );
FUNCTION VOID disp_qual 
(	
	struct VARIABLE *v,	/* in: variable with (maybe) qualifiers */
	TEXT		name[]	/* in: qualified name of variable 	*/

	);
FUNCTION struct VARIABLE * vmove
(
    struct VARIABLE *p,		/* input: VARIABLE struct to move	*/
    ALIGN	    *s,		/* input: restricted storage block	*/
    FUNINT          specials	/* input: bits indicating special...	*/
    				/*	  parts of VARIABLE to move	*/

 );
FUNCTION  CODE  c_crepath 
(
 struct PATH *path,			/* Out: Path control block        */
 FUNINT path_size,			/* In: Path size                  */
 TEXT   path_name[],			/* In: Path name		  */
 FUNINT type			/* In: temporary (TMPMBX) or      */
					/*     permanent (PRMMBX)	  */
 );
FUNCTION CODE c_delpath 
(
    struct PATH *path			/* In: Path Control Block
					   */
 );
FUNCTION CODE  c_pmwcd 
(
 TEXT	path_name[],		/* In:  Path name		  */
 GENPTR	buffer,			/* In:  Message to be output	  */
 FUNINT	size,			/* In:  Message size		  */
 TEXT	errmsg[]		/* Out: Error message		  */
 );
FUNCTION CODE c_getmsg
(
 struct PATH     *path,		/* In:  Path Control Block	  */
 GENPTR	    buffer,		/* Out: Buffer to store msg in    */
 COUNT	    *size		/* In:  Buffer size		  */
 );
FUNCTION CODE chk_valid
(
    struct VARIABLE	*v,		/* VARIABLE to check		*/
    GENPTR		value,		/* genptr to value vector	*/
    FUNINT		count		/* count of value		*/

 );
FUNCTION CODE file_ins 
(
    struct VARIABLE *v,	  	  /* in: VARIABLE definition	*/
    GENPTR 		vector,	  /* in: value vector		*/
    FUNINT		count,	  /* in: value count		*/
    TEXT		msgkey[], /* out: message key (if error)*/
    TEXT		msg[]	  /* out: message (if error)	*/

 );
FUNCTION CODE showasy 
(
    struct CONTXT	*procctx,		/* current context	*/
    struct CONTXT 	*cmdctx		/* context of SHOW	*/

 );
FUNCTION GENPTR allleg
(
    FAST struct VARIABLE *v,	/* in/out: ptr to var being validized	*/
    FUNINT lcount		/* in:  number of valid ranges	     */

 );
FUNCTION VOID cpy_vld
(
    FUNINT		type,		/* in: variable type		*/
    GENPTR		in,		/* in: valid structure		*/
    GENPTR		out		/* out: valid structure 	*/

 );
FUNCTION struct ACB *find_job 
(
    TEXT		name[]		/* in: job name			*/

 );
FUNCTION CODE addint 
(
    struct SYMTAB	*st,		/* in/out: symbol table			*/
    TEXT		name[],		/* in:  name of variable to add		*/
    FUNINT		maxc,		/* in:  maximum count			*/
    FUNINT		count,		/* in:  count				*/
    FUNINT		valvec[],	/* in:  values of integer		*/
    FUNINT		class		/* in: class (V_LOCAL, etc.)		*/

 );
FUNCTION CODE s_c2for
(
 TEXT		*c_string,	/* in: source string		*/
 FORSTR		*for_string,	/* out: FORTRAN-77 string	*/
 FUNINT		index		/* in: index in for_string	*/
				/* (zero if for_string scalar)	*/
 );
ADA_FUNCTION VOID Co_Free_Nocall 
(
    struct COLLECTION *c			/* IN: ptr to collection */
 );
FUNCTION  FUNCTION_PTR d_setsub 
(
 FUNCTION_PTR new_function	/* in: pointer to substitute function */
 );
FUNCTION GENPTR d_setctx 
(
 GENPTR new_context	/* in: new context for substitute */
 );
FUNCTION VOID host_init (void);
FUNCTION CODE intrin_init (void);
FUNCTION CODE ini_globals(void);
FUNCTION BOOL inter_proc (void);
FUNCTION VOID greet 
(
    FAST TEXT	vrsion[]		/* in:  TM version string	*/

 );
FUNCTION   CODE  c_init(void);
FUNCTION VOID init_ins 
(
    struct CONTXT *primctx	/* in/out: primary level context  */
 );
FUNCTION VOID delsub 
(
    FAST struct SUBCMD **subptr	/* in/out: ptr to 1st SUBCMD struct	*/

 );
FUNCTION VOID free_acb 
(
    struct ACB		*acb		/* in: the acb			*/

 );
FUNCTION 	VOID	cleanlp 
(
    GENPTR	ptr			   /*IN:OUT: Pointer to ICB link list*/

 );
FUNCTION VOID refs_ins
(
    struct CONTXT	*pctx		/* in/out: context block	*/

 );
FUNCTION VOID set_process_name 
(
     CODE	runtype,				/* BATCH or ASYNC */
     TEXT	name[]					/* job name	*/

 );
FUNCTION CODE get_asyctx (void);
FUNCTION VOID emit_stat (void);
FUNCTION  struct VARIABLE * get_ivp
(
    struct  SYMTAB  *head		/* in/out: symbol table header	*/

);
FUNCTION CODE logoff_do 
(
    struct CONTXT	*cpctx,		/* in:  containing proc context		*/
    struct CONTXT	*npctx		/* in:  proc ctx for LOGOFF command	*/
 );
FUNCTION 	VOID	chk_label 
(
    struct 	CONTXT	*proctx	/* IN/OUT: Current context block */

 );
FUNCTION CODE setqlf 
(
    FUNINT		intrin,		/* true if intrinsic		*/
    struct SYNBLK	*sb,		/* in/out: syntax block (command stream)*/
    struct SYMTAB	*st		/* out:  qualifier symbol table		*/

 );
FUNCTION  CODE  list_missing 
(
    struct  SYMTAB  *symtab		/* in: pointer to symbol table	*/

 );
FUNCTION CODE action_setup 
(
    struct CONTXT *ctx		/* in/out: current proc context	*/

 );
FUNCTION CODE run_batch 
(
    struct	CONTXT	*procctx,	/* IN: Invoking proc context	  */
    struct	CONTXT	*cmdctx	/* IN: proc with batch qualifier  */

 );
FUNCTION CODE run_async 
(
    FAST struct CONTXT *procctx,	/* in: context of invoking proc		*/
    FAST struct CONTXT *cmdctx		/* in: context of cmd line for new proc	*/

 );
FUNCTION CODE set_stdout 
(
    struct CONTXT	*procctx,	/* in: outgoing proc context		*/
    struct CONTXT	*cmdctx	/* in/out: new proc context		*/

 );
FUNCTION CODE run_proc 
(
    struct CONTXT	*pctx		/* in:  proc context			*/

 );
FUNCTION CODE trs_value
(
    struct VARIABLE	*v,		/* structure to be updated	*/
    GENPTR		value,		/* in: pointer to value vector	*/
    FUNINT		count		/* in: vector count		*/

 );
FUNCTION CODE rest_parm
(
    TEXT		restspec[],	/* in:  restore file spec or null	*/
    struct CONTXT	*pctx,		/* in/out:  proc context		*/
    TEXT		errmsg[]	/* out: returned error message if fail	*/

 );
FUNCTION CODE save_parm 
(
    TEXT		savespec[],	/* in:  save file spec		*/
    struct CONTXT	*pctx,		/* in:  proc context		*/
    CODE		access,		/* in:  F_WRITE or F_OVER	*/
					/* (F_OVER used for SAVE LAST)	*/
    TEXT		errmsg[]	/* out: error message if FAIL	*/

 );
FUNCTION CODE upd_p_qlf 
(
    struct SYNBLK	*sb,		/* in/out: command line syntax block	*/
    struct VARIABLE	*v		/* in/out: parm which the quals apply to*/

 );
FUNCTION CODE key_trans
(
    struct SYMTAB	*parmst,	/* in: parameter symbol table	*/
    TEXT		key[],		/* in: keyword value		*/
    struct VARIABLE	**v		/* out: VARIABLE pointer	*/

 );
FUNCTION CODE lookab 
(
    struct SYMTAB	*head,		/* in:  header to symbol table		*/
    TEXT		name[],		/* in:  name of variable (parm) to find	*/
    FAST struct VARIABLE **v		/* out: variable found (NULL unless SUCCESS)*/

 );
FUNCTION CODE getkey
(
 FAST struct SYNBLK	*sb,		/* in/out: symbol table		*/
 TEXT		key[TOKESIZ+1]	/* out: parameter name		*/
 );
FUNCTION COUNT misprm 
(
    struct SYMTAB	*symtab,	/* in:  symbol table		*/
    FAST TEXT		mislst[STRINGSIZ+1]	/* out: list of missing */

 );
FUNCTION VOID tutmsg 
(
    TEXT		msg[],
    TEXT		key[],
    uintptr_t		a1, 
    uintptr_t		a2, 
    uintptr_t		a3, 
    uintptr_t		a4, 
    uintptr_t		a5 
 );
FUNCTION VOID hold_msg 
(
    TEXT	msg[],		/* in: control string		*/
    TEXT	key[],		/* in: message key		*/
    uintptr_t	a1,		/* in: edit parameters		*/
    uintptr_t a2,
    uintptr_t a3,
    uintptr_t a4,
    uintptr_t a5

 );
FUNCTION CODE gettvnam 
(
    TEXT		cmdstr[],	/* in:  command string incl verb	*/
    TEXT		*varname[MAXVAL],	/* out: variable name list	*/
    COUNT		*numvar	/* out: number of variable names in list*/

 );
FUNCTION VOID list_subc 
(
    struct SUBCMD	*s		/* in:  subcommand to list		*/

 );
FUNCTION CODE h1dsp_subc 
(
    struct CONTXT	*ctx,		/* in:  proc context			*/
    struct SUBCMD	*s,		/* subcommand to display help on		*/
    struct SFILE	*hf		/* in:  help file			*/

 );
FUNCTION VOID list_parm 
(
    struct VARIABLE	*v		/* in:  variable to list		*/

 );
FUNCTION VOID h1dsp_parm 
(
    struct CONTXT	*ctx,		/* in:  proc context			*/
    struct VARIABLE	*v,		/* variable to display help on		*/
    struct SFILE	*hf		/* in:  help file			*/

 );
FUNCTION CODE helper
(
    struct DIRBLK	*dirblk,	/* in: directive control block pointing	*/
					/*     to start of help text		*/
    TEXT		type[],		/* in: menu/proc/command/parm/msg/gen	*/
    TEXT		lefthead[],	/* in: string to follow "HELP DISPLAY: " */
    					/*     in the help display header	*/
    struct TXTSTOR	*titlept,	/* in: pointer to title string. 		*/
    					/*     null if no title or null title	*/
    struct HELPBLK	*helpblk	/* out: help control block		*/

 );
FUNCTION CODE inlevel2 
(
    struct CONTXT	*pctx,		/* in/out: proc context w/ tab to update.*/
    struct SFILE	*hf,		/* in/out: help file control block	*/
    TEXT		type[],		/* in: subcmd/parm			*/
    TEXT		subcmd[]	/* in: subcommand qualifier to search	*/

 );
FUNCTION CODE parm_help
(
    struct VARIABLE     *v,		/* in: VARIABLE struct for the parameter	*/
    FUNINT		infile,		/* in: TRUE if parm help text is in file	*/
    struct DIRBLK	*dirblk,	/* in: directive control block pointing	*/
					/*     to start of help text		*/
    TEXT		lefthead[],	/* in: string to follow "HELP DISPLAY: " */
    					/*     in the help display header	*/
    struct TXTSTOR	*titlept,	/* in: pointer to title string. 		*/
    					/*     null if no title or null title	*/
    struct HELPBLK	*helpblk	/* out: help control block		*/

 );
FUNCTION CODE getspnum 
(
    struct CONTXT	*pctx,		/* in/out: proc context to find subc in	*/
    TEXT		subname[],	/* in:  subcmd name			*/
    COUNT		*pagnum,	/* out: display page number		*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE getpgnum 
(
    struct CONTXT	*pctx,		/* in/out: proc context to find parm in	*/
    TEXT		parmname[],	/* in:  proc parameter name		*/
    COUNT		*pagnum,	/* out: display page number		*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE gettpnam 
(
    struct SYNBLK	*sb,		/* in/out: syntax block			*/
    TEXT		name[NAMESIZ+1],/* out: parm name			*/
    COUNT		*subscr	/* out: subscript on parm		*/

 );
FUNCTION BOOL dash_dash
(
    FAST struct SYNBLK	*sb
 );
FUNCTION VOID dispcomp 
(
    struct VARIABLE	*v,		/* in:  the variable to display		*/
    FUNINT		compnum,	/* in:  subscr (starting with 1) of the component*/
    FUNINT		fillcomp	/* in:  lowest #d comp that was filled (0 = no fill)*/

 );
FUNCTION CODE inipan 
(
    struct VARIABLE	*v		/* in/out: variable containing the panel*/

 );
FUNCTION VOID adjval 
(
    struct VARIABLE	*v,		/* in/out: var containing panel to be adjusted*/
    FUNINT		compnum	/* in:  component # of new panel top	*/

 );
FUNCTION VOID dispval 
(
    struct VARIABLE	*v		/* in:  variable to display value of	*/

 );
FUNCTION VOID listqnam 
(
    struct SYMTAB	*symtab,	/* in:  symbol table containing quals	*/
    TEXT		parm_name[]	/* in:  name of parm being qualified	*/

 );
FUNCTION CODE dispbld 
(
    struct CONTXT	*pctx,		/* in/out: proc context			*/
    struct TXTSTOR	*title,		/* in:  .TITLE text			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE q_get_exec 
(
    struct CONTXT	*ctx			/* in/out: child proc context	*/

 );
FUNCTION CODE runtype 
(
    struct SYMTAB 	*qualst		/* in: qualifier symtab	*/

 );
FUNCTION CODE gettsnam 
(
    struct SYNBLK	*sb,			/* in/out: syntax block		*/
    TEXT		name[SUBCMDSIZ+1]	/* out: subcommand name		*/

 );
FUNCTION struct VARIABLE *get_parm1 
(
    struct CONTXT *pctx,	/* in: proc context		*/
    FAST FUNINT	  page,		/* in: page number		*/
    struct SFILE  *hf		/* in: help file		*/

 );
FUNCTION CODE chk_do
(
    struct CONTXT	*ctx,		/* in: current proc context	*/
    struct ITRCMD	*itrcmd	/* in: ptr to ITRCMD entry	*/

 );
FUNCTION CODE bldvhlp 
(
    struct CONTXT	*pctx,		/* in/out: all the parameters		*/
    struct VARIABLE	*v,		/* in/out: the 1st variable of this page*/
    FUNINT		lvarpg,		/* in:  page # of last page built	*/
    struct TXTSTOR	*title,		/* in:  title text (just to know # lines)*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE bldshlp 
(
    struct CONTXT	*pctx,		/* in/out: all the parameters		*/
    struct SUBCMD	*s,		/* in/out: the 1st subcommand of this page*/
    FUNINT		lsubpg,		/* in:  page # of last page built	*/
    struct TXTSTOR	*title,		/* in:  title text (just to know # lines)*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION VOID high_lfield 
(
    TEXT	input[],	/* in: input string		*/
    FUNINT	width,		/* in: width of field		*/
    TEXT	output[]	/* out: string with high sequence */

 );
FUNCTION CODE addcomp 
(
    struct TXTSTOR	*txt,		/* in/out: struct to add text to	*/
    struct VARIABLE	*v,		/* in:  variable containing the value	*/
    FUNINT		compnum	/* in:  component number		*/

 );
FUNCTION VOID hdisptxt 
(
    FUNINT		line,		/* in:  display line # to start		*/
    FUNINT		col,		/* in:  column number to start each line*/
    struct TXTSTOR	*block,		/* in:  storage block containing dynam stored text*/
    FUNINT		width,		/* in: width of field			*/
    BOOL		highlight	/* in: TRUE to highlight FALSE		*/
					/*     to turn highlight off		*/

 );
FUNCTION VOID hdisprtxt 
(
    FUNINT		line,		/* in:  display line # to start		*/
    FUNINT		rtcol,		/* in:  column number to right-justify against*/
    struct TXTSTOR	*block,		/* in:  storage block containing dynam stored text*/
    FUNINT		width,		/* in: width of field			*/
    BOOL		highlight	/* in: TRUE to highlight FALSE to	*/
	    				/*     turn highlight off		*/

 );
FUNCTION VOID clrmlin 
(
    FUNINT		line,		/* in:  display line # at which to start clear*/
    FUNINT		col,		/* in:  display column # at which to start clear*/
    FUNINT		numlin		/* in:  number of lines to clear	*/

 );
FUNCTION VOID disprtxt 
(
    FUNINT		line,		/* in:  display line # to start		*/
    FUNINT		rtcol,		/* in:  column number to right-justify against*/
    struct TXTSTOR	*block		/* in:  storage block containing dynam stored text*/

 );
FUNCTION VOID dispflab 
(
    FUNINT		line,		/* in:  screen line number		*/
    FUNINT		subtut,		/* in:  TRUE if tutoring on subc FALSE if on parms*/
    FUNINT		qualtut	/* in:  TRUE if tutoring on parm quals FALSE otherwise*/

 );
FUNCTION COUNT nlinval 
(
    struct VARIABLE	*v		/* in:  variable to fin # lines for	*/

 );
FUNCTION CODE txtread 
(
    struct DIRBLK	*db,		/* in/out: directive block (for d_ pkg)	*/
    TEXT		str[],		/* out: text string read		*/
    struct POSCTX	*posctx	/* out: the saved position context	*/

 );
FUNCTION CODE brktxt 
(
    struct TXTSTOR	*block,		/* in/out: text storage block		*/
    FUNINT		width		/* in:  field width			*/

 );
FUNCTION CODE tutsubeq 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutprmeq 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutmsghlp 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tuthlp 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );

FUNCTION CODE tutpage 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutscrol 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutrun 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutexit 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutsav 
(
    TEXT		cmdstr[],	/* in:  command string		*/
    TEXT		cmdprm[],	/* in:  the command parameter 	*/
    struct CONTXT	*pctx,		/* in:  proc context		*/

    struct SFILE	*hf		/* in/out: help file control block	*/
 );
FUNCTION CODE tutrstr 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutlist 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutdisplay 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutscreen 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutnoscreen 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutset 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/
	
 );
FUNCTION CODE tutshow 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in/out:  the command parameter if any*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutqual 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in/out:  the command parameter if any*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutaccept 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tuthold 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 );
FUNCTION CODE tutselect
(
TEXT		cmdstr[],	/* in:  command string			*/
TEXT		cmdprm[],	/* in:  the command parameter if any	*/
struct CONTXT	*pctx,		/* in:  proc context			*/
struct SFILE	*hf		/* in/out: help file control block	*/
 );
FUNCTION CODE get_parms_remote 
(
    struct CONTXT	*proc_ctx,	/* in: Context of executing proc    */
    TEXT		**preface,	/* in: preface for tutoring 	    */
    struct CONTXT	*parctx,	/* in/out: parameter context	    */
    FUNINT		vcount		/* in: number of preface lines	    */

 );
FUNCTION CODE tutohlp 
(
    struct SFILE	*pdf,		/* in:  PDF				*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct TXTSTOR	*title,		/* out: block of .TITLE text (in dyn mem)*/
    struct SFILE	**ohf		/* out: help file control block	*/

 );
FUNCTION VOID listpnam 
(
    struct SYMTAB	*symtab	/* in:  symbol table containing parms	*/

 );
FUNCTION VOID list_preface 
(
    struct VARIABLE	*p	/* in: strings to display	*/

 );
FUNCTION  CODE rsetini 
(
    struct SYMTAB	*st		/* in/out: symbol table.*/

 );
FUNCTION  VOID  slparm
(
    TEXT		prefix[],		/* record prefix 	   */
    struct  SYMTAB      *symtab		/* symbol table address    */

 );
FUNCTION VOID listsnam 
(
    struct SUBCMD	*subptr	/* in:  subcommand chain		*/

 );
FUNCTION CODE restore 
(
    struct CONTXT	*ctx	/* in/out: command context	*/

 );
FUNCTION CODE getscr
(
    FAST TEXT	line[],				/* OUT: record from script file*/
    CODE	*term,				/* OUT: terminator character */
    FUNINT	screen				/* IN: true if screen mode   */

 );
FUNCTION VOID m_cput 
(
    FUNINT	line,			/* in: line number to write message  */
    TEXT 	control[],		/* in: control string		*/
    TEXT	key[],			/* in: message key		*/
    uintptr_t	a1,		/* in: integers or string ptrs	*/
    uintptr_t   a2,
    uintptr_t   a3,
    uintptr_t   a4,
    uintptr_t   a5
 );

FUNCTION void ena_over(void);
FUNCTION void ena_under(void);

#endif
