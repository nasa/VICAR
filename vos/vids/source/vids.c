/*	Main routine for the VIDS server process
 */

#include	"xvmaininc.h"
#include	"VIDSdefs.h"		/* VIDS definitions	  */
#include	<stdio.h>			/* UNIX-like standard I/O */
#include        "asyncinc.inc"

struct PARBLK	InParblk, OutParblk;	/* GLOBAL variables */

static FILE     *sysout = NULL;
static COUNT    get_string();
static TAEINT   get_integer();
static CODE     send_parblk();
struct VARIABLE *p_find();

static TEXT this_job[STRINGSIZ+1], 
            parent_job[STRINGSIZ+1];

void terminate();
CODE	emit();
#define TERM    0x1000
#define STATUS  0x1001

main ()
{
struct PATH in_path;				/* input path control block */
struct VARIABLE *v;
struct PARBLK *in_parblk;
static struct PARBLK *out_parblk;		/* VBLOCKs */
CODE        code;
COUNT       i, l, parblk_size;
TEXT        *s;
TEXT        sender_job[STRINGSIZ+1], 
	    errmsg[STRINGSIZ+1],
	    key[STRINGSIZ+1];
COUNT       mb_size;				/* size of mailbox to create */
FILE        *z_init();
static VIDSEnvironment *current_env;	/* Ptr to current device environment */
VIDSEnvironment *env, *empty_env;	/* temp. environment pointers */
int         exit_routine();
char        dev_name[MAXDEVNAMESIZE+1];
Boolean     found;

in_parblk = &InParblk;
out_parblk = &OutParblk;

sysout = z_init (in_parblk, P_CONT);		/* get initial parameters */
if (sysout == NULL)
    terminate (in_parblk->hostcode, "z_init failure");
emit (0, "Alive");
get_string (in_parblk, "_CHILD",  this_job);
get_string (in_parblk, "_PARENT", parent_job);        
/* fprintf (sysout, "\nVIDS job initiated.\n"); */	/* redundant */

current_env = malloc(sizeof(VIDSEnvironment));
if (current_env == NULL)
{
    fprintf (sysout, "Cannot allocate starting environment\n");
    terminate (-1, "malloc error");
}

InitFileList();
InitEnvironment(current_env);		/* init the primary environment */
current_env->next = current_env;	/* init the circular chain */
current_env->prev = current_env;
mb_size = sizeof(struct PARBLK);
code = c_crepath (&in_path, mb_size, this_job, TMPMBX);
if (code != SUCCESS)
{
    fprintf (sysout, "Cannot create input mailbox. %s\n", in_path.errmsg);
    terminate (in_path.host_code, "c_crepath error");
}
q_init (out_parblk, P_BYTES, P_CONT);
send_parblk (out_parblk, this_job, parent_job);	/* announce ready    */

code = SetExitHandler(exit_routine, 3, out_parblk, this_job, parent_job);
if (code != SUCCESS)
{
    fprintf (sysout, "Cannot declare an exit handler.\n");
    terminate (-1, "exit handler error");
}

zdfregister(1);		/* tell the VRDI flag routines that this is VIDS */

while (FOREVER)        
{
    q_init (out_parblk, P_BYTES, P_CONT);
    parblk_size = mb_size;
    emit(0, "Ready");
    code = c_getmsg (&in_path, (GENPTR)in_parblk, &parblk_size);
    emit(0, "Executing");
    makeabs (&in_parblk->symtab, in_parblk->pool);	/* make ptrs absolute */
    in_parblk->mode = P_CONT;				/* continue on errors */
    get_string (in_parblk, "_JOB", parent_job);

    v = p_find (&InParblk, "DEVICE");		/* look for device change */
    if (v != NULL && v->v_count==1 && strlen(SVAL(*v,0)) != 0)
    {						/* device name given */
	strcpy(dev_name, SVAL(*v,0));
	UpperCase(dev_name, dev_name);

	/* loop through envs, looking for this device */

	env = current_env;
	found = False;
	do
	{
	    if (env->devUnit == noUnit)
		empty_env = env;
	    else if (EQUAL(dev_name, env->devName))
	    {
		found = True;
		break;
	    }
	    env = env->next;
	} while (env != current_env);

	if (found)
	    current_env = env;		/* found the device */
	else
	    current_env = empty_env;	/* didn't find it, so use empty slot */
    }

    code = doVIDS(current_env);
    PeekChar(True);		/* If PeekChar was called, terminate any i/o's*/

    q_intg (out_parblk, "STAT", 1, &code, P_ADD);
    s = current_env->message;				/* to pass **char */
    q_string (out_parblk, "MESSAGE", 1, &s, P_ADD);
    s = current_env->key;				/* to pass **char */
    q_string (out_parblk, "KEY", 1, &s, P_ADD);
    s = "REPLY";
    q_string (out_parblk, "TYPE", 1, &s, P_ADD);
    send_parblk (out_parblk, this_job, parent_job);	/* Send status out */
}

}
/******************************************************************************/

FUNCTION static TAEINT get_integer (parblk, name)

struct PARBLK *parblk;        /* in: VBLOCK                        */
TEXT          name[];         /* in: name of variable              */

{
    struct VARIABLE *v;

    v = p_find (parblk, name);
    if (v == NULL)
    {
	fprintf (sysout, "Cannot find variable '%s'.", name);
	terminate (-1, "No variable.");
    }
    return (IVAL(*v,0));
}
/******************************************************************************/




FUNCTION static COUNT get_string (parblk, name, output)

struct PARBLK *parblk;        /* in: VBLOCK                        */
TEXT          name[];         /* in: name of variable              */
TEXT        output[];         /* out: string value                 */

{
    struct VARIABLE *v;

    v = p_find (parblk, name);
    if (v == NULL)
    {
	fprintf (sysout, "Cannot find variable '%s'.", name);
        terminate (-1, "No variable.");
    }
    return (s_copy (SVAL(*v,0), output));     /* return string length */
}

/******************************************************************************/

FUNCTION static CODE send_parblk (parblk, this_job, target_job)

struct PARBLK        *parblk;        /* in: parblk to send            */
TEXT                this_job[];      /* in: name of current job       */
TEXT                target_job[];    /* in: name of job to send it to */

{
    IMPORT GENPTR        r_top();        /* current top of allocated area */
    TEXT                *valvec[1];      /* string value vector for _JOB  */
    TEXT                errmsg[STRINGSIZ+1];
    CODE                code;

    valvec[0] = this_job;
    q_string (parblk, "_JOB", 1, valvec, P_ADD);  /* tell target who we are */
    (*parblk).blksiz = r_top ((*parblk).pool) - (GENPTR)parblk;    
    makerel (&(*parblk).symtab, (*parblk).pool);  /* make ptrs relative     */
    code = c_pmwcd (target_job, (GENPTR)parblk, (*parblk).blksiz, errmsg);
    if (code != SUCCESS)
        fprintf (sysout, "Cannot send to '%s'.  %s.\n", target_job, errmsg);
    return (code);
}

/******************************************************************************/

/*        emit.   Emit status to parent.  This is process analogy of
 *        the TCL EMIT command.  
 *
 *        Currently, under UNIX, there
 *	  appears to be a bug whereby back-to-back emits
 *	  causes one or both of the emits to be lost.
 *
 *	This disabled for Unix because it doesn't seem to work.
 *	Is it really needed at all???
 */

FUNCTION CODE emit (sfi, skey) 

FUNINT        sfi;                  /* in: $SFI to report                */
TEXT        skey[];                 /* in: $SKEY to report               */

{
#if VMS_OS
    struct MON_MSG        msg;
    CODE                code;

    msg.type = MO_STATUS;
    msg.m_sfi = sfi;
    s_bcopy (skey, msg.m_skey, sizeof(msg.m_skey) - 1);
    code = c_snpa ((GENPTR)&msg, sizeof (msg));
    return (code);
#else
    return (SUCCESS);
#endif
}


/******************************************************************************/

FUNCTION void terminate (sfi, skey)

FUNINT        sfi;                 /* in: final $SFI status        */
TEXT        skey[];                /* in: final $SKEY string       */

{
    struct MON_MSG msg;	

    emit (sfi, skey);                  /* emit final status            */
    msg.type = MO_TERM;                /* tell parent we're terminating*/
    c_snpa ((GENPTR)&msg, sizeof(msg));    
    exit (1);
}


/*****************************************************************************/
/* Send a message back to TAE saying we aborted so the RECVAR doesn't hang.
 */

FUNCTION CODE exit_routine (exit_status, out_parblk,
					this_job, parent_job)

int *exit_status;			/* mandatory argument */
struct PARBLK *out_parblk;		/* VBLOCK for reply */
TEXT *this_job, *parent_job;

{
    TEXT        *s;
    int val;

    val = FAIL;
    q_intg (out_parblk, "STAT", 1, &val, P_ADD);
    s = "VIDS has unexpectedly aborted!  Please tell the VIDS programmer.";
    q_string (out_parblk, "MESSAGE", 1, &s, P_ADD);
    s = "VIDS-DIE";
    q_string (out_parblk, "KEY", 1, &s, P_ADD);
    s = "DIE";
    q_string (out_parblk, "TYPE", 1, &s, P_ADD);
    send_parblk (out_parblk, this_job, parent_job);	/* Send status out */

    return (SUCCESS);

}



/*****************************************************************************/
/* Send a message back to TAE with the VIDS output text to print.
 * We must send a message for printing so the output will go in the logs.
 */

FUNCTION CODE send_message (message, key)
TEXT *message, *key;
{
    static struct PARBLK message_parblk;	/* VBLOCK for reply */
    TEXT *s;
    int val;

    q_init (&message_parblk, P_BYTES, P_CONT);
    val = SUCCESS;
    q_intg (&message_parblk, "STAT", 1, &val, P_ADD);
    q_string (&message_parblk, "MESSAGE", 1, &message, P_ADD);
    q_string (&message_parblk, "KEY", 1, &key, P_ADD);
    s = "MESSAGE";
    q_string (&message_parblk, "TYPE", 1, &s, P_ADD);
    send_parblk (&message_parblk, this_job, parent_job);  /* Send status out */

    return (SUCCESS);

}
