// Prototypes internal to the tae library
#ifndef TAEINTPROTO_H
#define TAEINTPROTO_H
#include "stdh.inp"
#include "taeconf.inp"
#include "expinc.inc"
#include "syninc.inc"
#include "forstr.inp"
#include "parblk.inc"
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
struct	FORCTX 
{
  struct  VARIABLE  *nxtvar;	/* variable to work with next */
  COUNT   nxtind;			/* next index to value vector */
};

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
FUNCTION TEXT *s_save(TEXT* s);
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
FUNCTION CODE q_dynp
(
 struct PARBLK	*p,		/* V-block to send		*/
 TEXT		pdfspec[],	/* pdf file spec		*/
 FUNINT		mode		/* M_SUBPDF or M_FULLPDF	*/
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
    FAST TEXT		cmdstr[],  /* [siz] */	/* in/out: command string*/
    FUNINT		siz,			/* in:  dim-1 of cmdstr  */
    struct CONTXT	*pctx,			/* in:  proc context	 */
    BOOL		report			/* in:  true if err msg report	*/
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
FUNCTION DOUBLE power 
(
    DOUBLE	num,		/* input: the number to raise		*/
    FUNINT	exp		/* input: the exponent			*/

 );
FUNCTION CODE q_prep 
(
    struct PARBLK	*p,		/* in/out: PARBLK		*/
    TEXT		name[],		/* in: paramter name		*/
    FUNINT		count,		/* in: proposed parm count	*/
    FUNINT		type,		/* in: parm type		*/
    FUNINT		mode,		/* in: update or add		*/
    struct VARIABLE	**vv,		/* out: VARIABLE ptr		*/
    FUNINT              strsiz         /* in: string size              */ 
 );

#endif
