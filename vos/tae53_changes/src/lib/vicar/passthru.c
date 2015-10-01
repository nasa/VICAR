/* This module is used to turn passthru mode on and off.  Passthru
 * mode is the ability to execute or pass-through a vicar command
 * (either intrinsic or a procedure, not a process) from within
 * an interactive prompt-style program.  See also, PRSTYLE.C.
 */

#include "taeconf.inp"
#include "tminc.inc"
#include "taeintproto.h"
#include "vicartae.inc"		/* VICAR-specific definitions */

GLOBAL BOOL	procs_allowed=TRUE;	/* true if external procs allowed */

FUNCTION CODE passthru_do(struct CONTXT *UNUSED(X1), struct CONTXT *UNUSED(X2))
    
    {
    procs_allowed = TRUE;
    return (SUCCESS);
    }

FUNCTION CODE nopassthru_do(struct CONTXT *UNUSED(X1), struct CONTXT *UNUSED(X2))
    {
    procs_allowed = FALSE;
    return (SUCCESS);
    }

FUNCTION BOOL is_passthru(char *cmd)
    {
    TEXT	subcmd[STRINGSIZ+1];
    struct ITRCMD *itrcmd;
    
    subcmd[0] = '\0';
    if (itr_lookup(cmd, subcmd, &itrcmd) != SUCCESS) return (FALSE);
    if (s_equal((*itrcmd).cmd, "PASSTHRU")) return (TRUE);
    if (s_equal((*itrcmd).cmd, "NOPASSTHRU")) return (TRUE);
    return (FALSE);
    }
