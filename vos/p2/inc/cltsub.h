#ifndef CLT_SUB_H
#define CLT_SUB_H

/**** txh::modified includes for VMS compilation ***/
#include "xvmaininc.h"

#if VMS_OS
#include <unixlib.h>
#include <stat.h>
#include <types.h>
#include <socket.h>
#include <netdb.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netdb.h>
#define  socket_write write     /* txh::VMS has different functions */
#define  socket_read  read      /*      for socket read, write and  */
#define  socket_close close     /*      close.                      */
#endif

#define CLIENT_NAME_LEN			32
#define CONFIG_FILE_LOG_KEY		"SPICE_LOG"
#define CONFIG_FILE_PORTNO_KEY		"SPICE_TCP_PORT"
#define CONFIG_FILE_SVR_HOST_KEY	"SERVER_HOST"

#ifdef __cplusplus
extern "C"
{
#endif

/* The XDR routines dealing with longs are defined poorly... they are	*/
/* defined to pass 32 bits on the wire but use "long" as the type, which*/
/* of course is not always 32 bits.  Linux and mac use __LP64__ to	*/
/* redefine these to int * rather than long * for this reason.  Solaris	*/
/* does not seem to do this.  To "fix" this, we define our own XDR_long	*/
/* type which is defined here the same way as xdr_long() is declared.	*/
/* Use it exclusively for these routines rather than the bare "long"	*/
/* type.  So much for portability of XDR!!  rgd 2010/10/08		*/

#ifdef __LP64__
#define XDR_long int
#define XDR_u_long u_int
#else
#define XDR_long long
#define XDR_u_long u_long
#endif

int msclt_getLogFileName (char *fname);
short msclt_getPortno ();
int msclt_log (const char *sub, const char *mesg);
int msclt_read_short (int sd, short *data);
int msclt_write_short (int sd, short data);
int msclt_read_u_short (int sd, u_short *data);
int msclt_write_u_short (int sd, u_short data);
int msclt_read_long (int sd, XDR_long *data);
int msclt_write_long (int sd, XDR_long data);
int msclt_read_u_long (int sd, XDR_u_long *data);
int msclt_write_u_long (int sd, XDR_u_long data);
int msclt_read_string (int sd, char *str, int max_str_len);
int msclt_write_string (int sd, char *str, int max_str_len);
int msclt_is_readable (int sd);
int msclt_is_writeable (int sd);
int msclt_write_ck_struct (int sd, msCkStruct *ckdata);
int msclt_write_spk_struct (int sd, msSpkStruct *spkdata);
int msclt_read_req_struct (int sd, msUserRequestStruct *req);
int msclt_write_req_struct (int sd, msUserRequestStruct *req);
XDR_u_long msclt_read_resp_code (int sd);
int msclt_read_ck_struct (int sd, msCkStruct *ckdata);
int msclt_read_spk_struct (int sd, msSpkStruct *spkdata);
int msclt_connectToSvr (int *sd);
int msclt_write_client_name (int *sd);
int msclt_getSvrHostNames (char *hname);

int msclt_getspice (XDR_u_long req_code, msUserRequestStruct *req,
                msCkStruct *ckdata, msSpkStruct *spkdata);

int msclt_gllgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_casgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_simgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_vgr1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_vgr2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_vo1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_vo2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);

int msclt_putspice (XDR_u_long req_code, msCkStruct *ckdata);

int msclt_gllputspice (msCkStruct *ckdata);
int msclt_casputspice (msCkStruct *ckdata);
int msclt_simputspice (msCkStruct *ckdata);
int msclt_vgr1putspice (msCkStruct *ckdata);
int msclt_vgr2putspice (msCkStruct *ckdata);
int msclt_vo1putspice (msCkStruct *ckdata);
int msclt_vo2putspice (msCkStruct *ckdata);

int msclt_bin2text_file (char *bname, char *tname);
int msclt_text2bin_file (char *tname, char *bname);
int msclt_send_text_file (int sd, char *tname);
int msclt_receive_text_file (int sd, char *tname);
int msclt_send_bin_kernel (int sd, char *kname);
int msclt_receive_bin_kernel (int sd, char *kname);
int msclt_receive_kernel (XDR_u_long req_code, char *kname, char *local_dir);
int msclt_put_kernel (XDR_u_long req_code, char *kname);

int msclt_gllgetck (char *ckname, char *local_dir);
int msclt_casgetck (char *ckname, char *local_dir);
int msclt_simgetck (char *ckname, char *local_dir);
int msclt_vgr1getck (char *ckname, char *local_dir);
int msclt_vgr2getck (char *ckname, char *local_dir);
int msclt_vo1getck (char *ckname, char *local_dir);
int msclt_vo2getck (char *ckname, char *local_dir);

int msclt_gllgetspk (char *spk, char *local_dir);
int msclt_casgetspk (char *spk, char *local_dir);
int msclt_simgetspk (char *spk, char *local_dir);
int msclt_vgr1getspk (char *spk, char *local_dir);
int msclt_vgr2getspk (char *spk, char *local_dir);
int msclt_vo2getspk (char *spk, char *local_dir);
int msclt_vo2getspk (char *spk, char *local_dir);

int msclt_gllputck (char *ckname);
int msclt_casputck (char *ckname);
int msclt_simputck (char *ckname);
int msclt_vgr1putck (char *ckname);
int msclt_vgr2putck (char *ckname);
int msclt_vo1putck (char *ckname);
int msclt_vo2putck (char *ckname);

int msclt_gllputspk (char *spk);
int msclt_casputspk (char *spk);
int msclt_simputspk (char *spk);
int msclt_vgr1putspk (char *spk);
int msclt_vgr2putspk (char *spk);
int msclt_vo1putspk (char *spk);
int msclt_vo2putspk (char *spk);

int msclt_readn (int sd, char *ptr, int nbytes);
int msclt_writen (int sd, char *ptr, int nbytes);
void msclt_printspkstruct (msSpkStruct spk);
void msclt_printckstruct (msCkStruct ck);
void msclt_printuserrequeststruct (msUserRequestStruct req);
#ifdef __cplusplus
}
#endif

#endif
