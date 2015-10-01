/************************************************************************/
/* ANSI C Prototypes for VICAR Run-Time Library				*/
/************************************************************************/

#ifndef _ZVPROTO_H
#define _ZVPROTO_H

#ifndef NULL
#ifdef __cplusplus
#define NULL 0
#else
#define NULL (void*) 0
#endif
#endif

#include "xvmaininc.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _NO_PROTO

/* User-defined main subroutine */

void main44();

/* Run-Time Library routines */

void  zabend();
void  zqprint();
void  sc2for(), v2_sc2for();
void  sc2for_array(), v2_sc2for_array();
void  sfor2c(), v2_sfor2c();
int   sfor2c_array(), v2_sfor2c_array();
int   sfor2len(), v2_sfor2len();
char *sfor2ptr(), *v2_sfor2ptr();
int   zlgetlabel();
void  zmove();
void  zvbands();
int   zvcmdout();
int   zvcommand();
int   zveaction();
void  zvend();
int   zvfilename();
int   zvfilpos();
int   zvhost();
void  zvintract();
int   zvip();
int   zviparm();
int   zviparmd();
int   zvipcnt();
int   zvipone();
int   zvipstat();
int   zviptst();
void  zvmessage();
int   zvp();
int   zvparm();
int   zvparmd();
void  zvpblk();
int   zvpclose();
int   zvpcnt();
int   zvpixsize();
int   zvpixsizeb();
int   zvpixsizeu();
int   zvplabel();
int   zvplabel2();
int   zvpone();
int   zvpopen();
int   zvpout();
int   zvpstat();
int   zvptst();
int   zvq_out();
int   zvselpi();
int   zvselpiu();
int   zvsfile();
int   zvsignal();
void  zvsize();
void  zvsptr();
int   zvtpinfo();
int   zvtpmode();
int   zvtpset();
void  zvtrans();
int   zvtrans_in();
int   zvtrans_inb();
int   zvtrans_inu();
int   zvtrans_out();
int   zvtrans_set();
int   zvpinit();		/* DON'T CALL FROM APPLICATION CODE */
int   zv_rtl_init();		/* DON'T CALL FROM APPLICATION CODE */
int   zladd();
int   zldel();
int   zlget();
int   zlhinfo();
int   zlinfo();
int   zlninfo();
int   zlpinfo();
int   zvadd();
int   zvclose();
int   zvget();
int   zvopen();
int   zvread();
int   zvunit();
int   zvwrit();
int   zvzinit();		/* DON'T CALL FROM APPLICATION CODE */

#ifdef RTL_USE_SHELL_VIC

/* Shell-VICAR output variable support routines */

int v2param_count_elements();
char *v2param_find_entry();
char *v2param_get_file();
char *v2param_get_one_value();
void v2param_remove_file();

#endif

#else /* _NO_PROTO */

#include <stdarg.h>	/* for va_list, sigh */

/* User-defined main subroutine */

void main44(void);

/* Run-Time Library routines */

void zabend(void);
void zqprint(
		char *message,
		int length);
void sc2for(
		char *c_string,
		int max_length,
		char *for_string,
		void *argptr,
		int nargs,
		int argno,
		int strno);
void sc2for_array(
		char *c_string,
		int len,
		int nelements,
		char *for_string,
		int *max_length,
		void *argptr,
		int nargs,
		int argno,
		int strno);
void sfor2c(
		char *c_string,
		int len,
		char *for_string,
		void *argptr,
		int nargs,
		int argno,
		int strno);
int sfor2c_array(
		char **c_string,
		int *max_length,
		int nelements,
		char *for_string,
		void *argptr,
		int nargs,
		int argno,
		int strno);
int sfor2len(
		char *for_string,
		void *argptr,
		int nargs,
		int argno,
		int strno);
char *sfor2ptr(
		char *for_string);
void v2_sc2for(
		char *c_string,
		int max_length,
		char *for_string,
		void *argptr,
		int nargs,
		int argno,
		int strno,
		va_list *param,
		int *which);
void v2_sc2for_array(
		char *c_string,
		int len,
		int nelements,
		char *for_string,
		int *max_length,
		void *argptr,
		int nargs,
		int argno,
		int strno,
		va_list *param,
		int *which);
void v2_sfor2c(
		char *c_string,
		int len,
		char *for_string,
		void *argptr,
		int nargs,
		int argno,
		int strno,
		va_list *param,
		int *which);
int v2_sfor2c_array(
		char **c_string,
		int *max_length,
		int nelements,
		char *for_string,
		void *argptr,
		int nargs,
		int argno,
		int strno,
		va_list *param,
		int *which);
int v2_sfor2len(
		char *for_string,
		void *argptr,
		int nargs,
		int argno,
		int strno,
		va_list *param,
		int *which);
char *v2_sfor2ptr(
		char *for_string);
int zlgetlabel(
		int unit,
		char *buf,
		int *bufsize);
void zmove(
		void *from,
		void *to,
		int len);
void zvbands(
		int *sb,
		int *nb,
		int *nbi);
int zvcmdout(
		char *command);
int zvcommand(
		char *command);
int zveaction(
		char *action,
		char *message);
void zvend(
		int status);
int zvfilename(
		char *in_name,
		char *out_name,
		int out_len);
int zvfilpos(
		int unit);
int zvhost(
		char *host,
		char *intfmt,
		char *realfmt);
void zvintract(
		char *subcmd,
		char *prompt);
int zvip(
		char *name,
		void *value,
		int *count);
int zviparm(
		char *name,
		void *value,
		int *count,
		int *def,
		int maxcnt,
		int length);
int zviparmd(
		char *name,
		void *value,
		int *count,
		int *def,
		int maxcnt,
		int length);
int zvipcnt(
		char *name,
		int *count);
int zvipone(
		char *name,
		void *value,
		int instance,
		int maxlen);
int zvipstat(
		char *name,
		int *count,
		int *def,
		int *maxlen,
		char *type);
int zviptst(
		char *name);
void zvmessage(
		char *message,
		char *key);
int zvp(
		char *name,
		void *value,
		int *count);
int zvparm(
		char *name,
		void *value,
		int *count,
		int *def,
		int maxcnt,
		int length);
int zvparmd(
		char *name,
		void *value,
		int *count,
		int *def,
		int maxcnt,
		int length);
void zvpblk(
		void **parblk);
int zvpclose(void);
int zvpcnt(
		char *name,
		int *count);
int zvpixsize(
		int *pixsize,
		char *type,
		char *ihost,
		char *rhost);
int zvpixsizeb(
		int *pixsize,
		char *type,
		int unit);
int zvpixsizeu(
		int *pixsize,
		char *type,
		int unit);
				
int zvplabel(	int unit,
		int mvlimit,
		int doubleflag);
				
int zvplabel2(	int unit,
		int mvlimit,
		int doubleflag,
		int incl_def);

int zvpone(
		char *name,
		void *value,
		int instance,
		int maxlen);
int zvpopen(
		char *filename,
		char *error_act,
		int *unit);
int zvpout(
		char *name,
		void *value,
		char *format,
		int count,
		int length);
int zvpstat(
		char *name,
		int *count,
		int *def,
		int *maxlen,
		char *type);
int zvptst(
		char *name);
int zvq_out(
		void *parblk);
int zvselpi(
		int instance);
int zvselpiu(
		int unit);
int zvsfile(
		int unit,
		int file);
void zvsignal(
		int unit,
		int status,
		int abend_flat);
void zvsize(
		int *sl,
		int *ss,
		int *nl,
		int *ns,
		int *nli,
		int *nsi);
void zvsptr(
		char *value,
		int count,
		int *offsets,
		int *lengths);
int zvtpinfo(
		char *sym_name,
		char *dev_name,
		int *tfile,
		int *trec);
int zvtpmode(
		int unit);
int zvtpset(
		char *name,
		int tfile,
		int trec);
void zvtrans(
		int *buf,
		void *source,
		void *dest,
		int npix);
int zvtrans_in(
		int *buf,
		char *stype,
		char *dtype,
		char *sihost,
		char *srhost);
int zvtrans_inb(
		int *buf,
		char *stype,
		char *dtype,
		int unit);
int zvtrans_inu(
		int *buf,
		char *stype,
		char *dtype,
		int unit);
int zvtrans_out(
		int *buf,
		char *stype,
		char *dtype,
		char *dihist,
		char *drhost);
int zvtrans_set(
		int *buf,
		char *stype,
		char *dtype);
int zvpinit(			/* DON'T CALL FROM APPLICATION CODE */
		void *parb);
int zv_rtl_init(void);		/* DON'T CALL FROM APPLICATION CODE */
int zvzinit(			/* DON'T CALL FROM APPLICATION CODE */
		int argc,
		char *argv[]);
void zmove(void *from, void *to, int len);


#ifdef RTL_USE_SHELL_VIC

/* Shell-VICAR output variable support routines */

int v2param_count_elements(
		char *param_value,
		char *param_name);
char *v2param_find_entry(
		char *filename,
		char *param_name);
char *v2param_get_file(void);
char *v2param_get_one_value(
		char **p,
		char *param_name);
void v2param_remove_file(
		char *filename);

#endif


/* Variadic routines */

/* This is a useful extension in GCC that lets us have the compiler	*/
/* check that we end the variable length argument with a NULL.		*/
/* However, it is only available in GCC version 4.0 or later.		*/

#if __GNUC__ > 3
#define SENTINEL __attribute__((__sentinel__(0)))
#else
#define SENTINEL
#endif
 
int zladd(
		int unit,
		char *type,
		char *key,
		void *value,
		...) SENTINEL;
int zldel(
		int unit,
		char *type,
		char *key,
		...) SENTINEL;
int zlget(
		int unit,
		char *type,
		char *key,
		void *value,
		...) SENTINEL;
int zlhinfo(
		int unit,
		char *tasks,
		int *instances,
		int *nhist,
		...) SENTINEL;
int zlinfo(
		int unit,
		char *type,
		char *key,
		char *format,
		int *maxlen,
		int *nelement,
		...) SENTINEL;
int zlninfo(
		int unit,
		char *key,
		char *format,
		int *maxlength,
		int *nelement,
		...) SENTINEL;
int zlpinfo(
		int unit,
		char *properties,
		int *nprop,
		...) SENTINEL;
int zvadd(
		int unit,
		...) SENTINEL;
int zvclose(
		int unit,
		...) SENTINEL;
int zvget(
		int unit,
		...) SENTINEL;
int zvopen(
		int unit,
		...) SENTINEL;
int zvread(
		int unit,
		void *buffer,
		...) SENTINEL;
int zvunit(
		int *unit,
		char *name,
		int instance,
		...) SENTINEL;
int zvwrit(
		int unit,
		void *buffer,
		...) SENTINEL;

#endif /* _NO_PROTO */

#ifdef __cplusplus
}	/* end extern "C" */
#endif

#endif /* _XVPROTO_H */

