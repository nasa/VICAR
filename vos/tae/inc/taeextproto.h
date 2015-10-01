/* Prototypes for TAE library methods called externally */
#ifndef TAEEXTPROTO_H
#define TAEEXTPROTO_H

#include "stdh.inp"
#include "taeconf.inp"

struct VARIABLE;
struct PARBLK;
struct LARGE_PARBLK;

FUNCTION struct VARIABLE *p_fvar(struct PARBLK *block, TEXT name[]);
FUNCTION BOOL s_equal(FAST TEXT *s, FAST TEXT *t);
FUNCTION CODE q_cmd (TEXT* command);
FUNCTION CODE p_inim (struct PARBLK* block, FUNINT blksiz, FUNINT mode);
FUNCTION CODE q_cmdwait (TEXT *command);
FUNCTION VOID q_init(struct PARBLK *p, FUNINT pool_size, FUNINT mode);
FUNCTION CODE q_string(struct PARBLK *p, TEXT name[], FUNINT count,
                       TEXT* vector[], FUNINT mode);
FUNCTION CODE q_real(struct PARBLK* p, TEXT name[], FUNINT count,
                     TAEFLOAT real[], FUNINT mode);
FUNCTION CODE q_intg(struct PARBLK* p, TEXT name[], FUNINT count,
                     TAEINT intg[], FUNINT mode);
FUNCTION CODE q_dynp(struct PARBLK* p, TEXT pdfspec[], FUNINT mode);
FUNCTION CODE m_msg(TEXT message[], TEXT key []);
FUNCTION CODE p_mput(TEXT message[], TEXT key[]);
FUNCTION CODE q_out(struct PARBLK* p);
FUNCTION CODE t_pinit(COUNT* lines, COUNT* columns, CODE* type);
#ifdef LARGE_PARBLK_FIX
FUNCTION FILE *z_init (struct LARGE_PARBLK *block, FUNINT mode);
#else
FUNCTION FILE *z_init (struct PARBLK *block, FUNINT mode);
#endif
FUNCTION VOID z_exit(FUNINT sfi, TEXT* skey);
FUNCTION CODE m_fpval(struct VARIABLE *vv, COUNT index, TEXT line[],
			COUNT stdlen, COUNT longlen);

#endif

