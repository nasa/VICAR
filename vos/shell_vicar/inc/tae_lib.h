/* Include file for shell tae library */

#include "xvmaininc.h"
#include "ftnbridge.h"
#ifdef MAX
#undef MAX
#endif
#include "taeconf.inp"
#include "pgminc.inc"
#include "parblk.inc"
#include <stdio.h>
#ifndef MIN
#define MIN(x,y) (((x)>(y)) ? (y) : (x))
#endif

/* these are for the initialize_variable subroutine */
#define IS_KEYWORD TRUE
#define NOT_KEYWORD  FALSE

/* this is for the string parser */
#define NAME_VALUE_TYPE 1
#define KEYWORD_TYPE 2
#define VALUE_TYPE 3

/* function declararations */
char *new_data(int, int);
char *new_valid(int, int);
struct VARIABLE *p_fvar(struct PARBLK *, char *);
struct VARIABLE *p_fvar_abbrev(struct PARBLK *parblk, char *key);
char *find_parm(char*, char*, char**, int*);
FILE *open_pdf(char *pdf_file);
void process_pdf_filename(char *proc_file,char *pdf_file, char *proc);
void tae_abort(int code, char *val);
int initialize_parblk(struct PARBLK *parblk,char *proc,FILE *fp);
int set_sub_command(struct PARBLK *parblk, char *cmd);
int add_subcmd_vars_to_parblk(struct PARBLK *parblk,FILE *fp);
int fgetstr(char **lptr, FILE *fp);
int parse_parm (char *cmd, int len, char **sk, char **ek, char **sv,
		char **ev);
void add_one_pdf_variable_to_parblk(struct PARBLK *parblk,char line[]);
int parse_pdf_statement(char* line, char* parm,int *type,int *keyflag,
			int vcount[2],char** vvals,int *vlen,char **dvals,
			int* dlen);
int count_values(char *value,int vallen,int valid_flag);
int initialize_variable(struct VARIABLE *v, char *name, int type, int class,	
			int keyflag, int minc, int maxc, int nvalids,  
			int ndefs);
int get_parm_values(char *vals, char *value, int vallen, int type, int size, 
		    int valid_flag);
int add_variable_to_parblk(struct PARBLK *parblk, struct VARIABLE *var);
int read_parm_string(struct VARIABLE* v, char* value, int vallen);
int add_subcmd_to_parblk(struct PARBLK *parblk, FILE *fp);
int set_keyword(struct PARBLK *parblk,char *key, int len, 
		struct VARIABLE* specific_v);
void check_values(struct VARIABLE *v, int replace);
int add_pdf_variables_to_parblk(struct PARBLK *parblk,char* proc_file, 
				char*cmd_line);
int put_pdf_values_in_parblk(struct PARBLK *parblk,char *cmd_line);
void shvic_make_upper_case(char *out, char *in);
void shvic_make_upper_case_max(char *out, char *in, int max);

/* TAE EMULATION ERROR CODES */
#define TAE_ERR_BASE -98
#define AMBIGPAR  	(TAE_ERR_BASE  -  0)	
#define AMBIGSUB  	(TAE_ERR_BASE  -  1)	
#define AMBIGVAL  	(TAE_ERR_BASE  -  2)	
#define BADFILE  	(TAE_ERR_BASE  -  3)	
#define BADPAR  	(TAE_ERR_BASE  -  4)	
#define BADPFILE  	(TAE_ERR_BASE  -  5)	
#define BADREF  	(TAE_ERR_BASE  -  6)	
#define BADVALCT  	(TAE_ERR_BASE  -  7)	
#define CNTERR  	(TAE_ERR_BASE  -  8)	
#define DFTERR  	(TAE_ERR_BASE  -  9)	
#define EXPR  		(TAE_ERR_BASE  -  10)	
#define FMTRESTR  	(TAE_ERR_BASE  -  11)	
#define INVINT  	(TAE_ERR_BASE  -  12)	
#define INVNAME  	(TAE_ERR_BASE  -  13)	
#define INVPNAME  	(TAE_ERR_BASE  -  14)	
#define INVPVAL  	(TAE_ERR_BASE  -  15)	
#define INVREAL  	(TAE_ERR_BASE  -  16)	
#define INVSTR  	(TAE_ERR_BASE  -  17)	
#define INVSUBNAME  (TAE_ERR_BASE  -  18)	
#define INVVAL  	(TAE_ERR_BASE  -  19)	
#define KEYSYNTAX  	(TAE_ERR_BASE  -  20)	
#define KEYWORD  	(TAE_ERR_BASE  -  21)	
#define MISINTRO  	(TAE_ERR_BASE  -  22)	
#define MISPAR  	(TAE_ERR_BASE  -  23)	
#define MIXTYPE  	(TAE_ERR_BASE  -  24)	
#define MIXVAR  	(TAE_ERR_BASE  -  25)	
#define MULVAR  	(TAE_ERR_BASE  -  26)	
#define NAMERR  	(TAE_ERR_BASE  -  27)	
#define NESTMUL  	(TAE_ERR_BASE  -  28)	
#define NOGLB  		(TAE_ERR_BASE  -  29)	
#define NOPARM  	(TAE_ERR_BASE  -  30)	
#define NOTINPDF  	(TAE_ERR_BASE  -  31)	
#define NOTNULL  	(TAE_ERR_BASE  -  32)	
#define NOTUSED  	(TAE_ERR_BASE  -  33)	
#define NOVAL  		(TAE_ERR_BASE  -  34)	
#define NOVALUE  	(TAE_ERR_BASE  -  35)	
#define NULLARG  	(TAE_ERR_BASE  -  36)	
#define NULLELEM  	(TAE_ERR_BASE  -  37)	
#define NUMNULL  	(TAE_ERR_BASE  -  38)	
#define PARCREATE  	(TAE_ERR_BASE  -  39)	
#define PARS  		(TAE_ERR_BASE  -  40)	
#define POSERR  	(TAE_ERR_BASE  -  41)	
#define PSETOVER  	(TAE_ERR_BASE  -  42)	
#define RANGE  		(TAE_ERR_BASE  -  43)	
#define REFEX  		(TAE_ERR_BASE  -  44)	
#define SCRCNT  	(TAE_ERR_BASE  -  45)	
#define STRSIZ  	(TAE_ERR_BASE  -  46)	
#define SUBREQ  	(TAE_ERR_BASE  -  47)	
#define SUBTWC  	(TAE_ERR_BASE  -  48)	
#define TOOFEWVAL  	(TAE_ERR_BASE  -  49)	
#define TOOMANYVAL  (TAE_ERR_BASE  -  50)	
#define TYPERR  	(TAE_ERR_BASE  -  51)	
#define UNDEFSUB  	(TAE_ERR_BASE  -  52)	
#define UNDEFVAR  	(TAE_ERR_BASE  -  53)	
#define UNEXEOL  	(TAE_ERR_BASE  -  54)	
#define VALIDREQ  	(TAE_ERR_BASE  -  55)	
#define VARCNFLCT  	(TAE_ERR_BASE  -  56)	
#define LAST_TAE_ERR  	(TAE_ERR_BASE  -  56)	

