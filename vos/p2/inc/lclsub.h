#ifndef LCL_SUB_H
#define LCL_SUB_H

#ifdef __cplusplus
extern "C"
{
#endif

int mslcl_getgllenv (msEnvStruct *env);
int mslcl_getcasenv (msEnvStruct *env);
int mslcl_getsimenv (msEnvStruct *env);
int mslcl_getvgr1env (msEnvStruct *env);
int mslcl_getvgr2env (msEnvStruct *env);
int mslcl_getvo1env (msEnvStruct *env);
int mslcl_getvo2env (msEnvStruct *env);

int mslcl_load_support_kernels (msEnvStruct *env);
int mslcl_scet2sclk (int sc_id, int *scet, double *sclk);
int mslcl_strcmp (char *usr_str, char *ker_str, char *def_str);
int mslcl_identical_segid(char *usr_segid, char *ker_segid);
int mslcl_kid_2_kinfo (msEnvStruct *env,
        char *kid, kernelInfoStruct *kinfo);
int mslcl_kname_2_kinfo (msEnvStruct *env,
        char *kname, kernelInfoStruct *kinfo);
int mslcl_ck_info_from_kdb (msEnvStruct *env, kernelInfoStruct *info);
int mslcl_spk_info_from_kdb (msEnvStruct *env, kernelInfoStruct *info);
int mslcl_read_kernel_info (msEnvStruct *env,
                kernelInfoStruct *info, int type);
int mslcl_use_prov_info (char *segid);
int mslcl_lowerSegidPriority (char *segid);
int mslcl_loadSpk (int sd, double et, char *spk_ref,
        char *segid, kernelInfoStruct *kinfo, int kernel_count);
int mslcl_loadCK (int sd, msUserRequestStruct *req,
	kernelInfoStruct *kinfo, int kernel_count);
int mslcl_unloadSpk (kernelInfoStruct *kinfo, int count);
void mslcl_getDateTime (char *date_time);

int mslcl_gllgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_casgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_simgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_vgr1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_vgr2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_vo1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_vo2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_getspice (msEnvStruct *env, msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);

int mslcl_gllputspice  (msCkStruct *ckdata);
int mslcl_casputspice  (msCkStruct *ckdata);
int mslcl_simputspice  (msCkStruct *ckdata);
int mslcl_vgr1putspice (msCkStruct *ckdata);
int mslcl_vgr2putspice (msCkStruct *ckdata);
int mslcl_vo1putspice  (msCkStruct *ckdata);
int mslcl_vo2putspice  (msCkStruct *ckdata);
int mslcl_putspice (msEnvStruct *env, msCkStruct *ckdata);


int mssvr_getNewClient (int ld);
int mssvr_initAcceptor (int *sd);
int mssvr_readClientName (int sd, char *clt_name);
XDR_u_long mssvr_readRequestCode (int sd);
int mssvr_handleRequest (int sd);

void mssvr_gllgetspice (int sd);
void mssvr_gllputspice (int sd);
void mssvr_gllsendck(int sd);
void mssvr_gllsendspk(int sd);
void mssvr_gllreceiveck(int sd);
void mssvr_gllreceivespk(int sd);

void mssvr_casgetspice (int sd);
void mssvr_casputspice (int sd);
void mssvr_cassendck(int sd);
void mssvr_cassendspk(int sd);
void mssvr_casreceiveck(int sd);
void mssvr_casreceivespk(int sd);

void mssvr_simgetspice (int sd);
void mssvr_simputspice (int sd);
void mssvr_simsendck(int sd);
void mssvr_simsendspk(int sd);
void mssvr_simreceiveck(int sd);
void mssvr_simreceivespk(int sd);

void mssvr_vgr1getspice (int sd);
void mssvr_vgr1putspice (int sd);
void mssvr_vgr1sendck(int sd);
void mssvr_vgr1sendspk(int sd);
void mssvr_vgr1receiveck(int sd);
void mssvr_vgr1receivespk(int sd);

void mssvr_vgr2getspice (int sd);
void mssvr_vgr2putspice (int sd);
void mssvr_vgr2sendck(int sd);
void mssvr_vgr2sendspk(int sd);
void mssvr_vgr2receiveck(int sd);
void mssvr_vgr2receivespk(int sd);

void mssvr_vo1getspice (int sd);
void mssvr_vo1putspice (int sd);
void mssvr_vo1sendck(int sd);
void mssvr_vo1sendspk(int sd);
void mssvr_vo1receiveck(int sd);
void mssvr_vo1receivespk(int sd);

void mssvr_vo2getspice (int sd);
void mssvr_vo2putspice (int sd);
void mssvr_vo2sendck(int sd);
void mssvr_vo2sendspk(int sd);
void mssvr_vo2receiveck(int sd);
void mssvr_vo2receivespk(int sd);

void mssvr_getspice (int sd, msEnvStruct *env);
int mssvr_readckdata(int sd, msEnvStruct *env,
	msUserRequestStruct *req, msCkStruct *ckdata);
int t_mssvr_readckdata(int sd, msEnvStruct *env,
        msUserRequestStruct *req, msCkStruct *ckdata);
int mssvr_read_ckrecord (int sd, msEnvStruct *env,
        msUserRequestStruct *req, msCkStruct *ckdata, double tol);
int mssvr_readck_by_source (int sd, msEnvStruct *env,
        msUserRequestStruct *req, msCkStruct *ckdata);
int mssvr_readck_by_id (int sd, msEnvStruct *env,
        msUserRequestStruct *req, msCkStruct *ckdata);
int mssvr_readck_by_name (int sd, msEnvStruct *env,
        msUserRequestStruct *req, msCkStruct *ckdata);
int mssvr_readckfromfile (int sd, msUserRequestStruct *req,
        msCkStruct *ckdata, double tol, char *ckfname);
int mssvr_readspkdata (int sd, msEnvStruct *env,
        msUserRequestStruct *request, msCkStruct *ckdata,
        msSpkStruct *spkdata);
void mssvr_putspice (int sd, msEnvStruct *env);
int mssvr_write_to_ckid (int sd, char *ckid,
                msCkStruct *ckdata, msEnvStruct *env);
int mssvr_write_to_cksource (int sd, char *cksource,
                msCkStruct *ckdata, msEnvStruct *env);
int mssvr_write_to_ckname (int sd, char *fname,
                msCkStruct *ckdata, msEnvStruct *env);
void mssvr_receive_kernel (int sd, msEnvStruct *env);
void mssvr_send_kernel (int sd, msEnvStruct *env);
int mssvr_write_err_log (int sd, const char *sub, const char *mesg);
int mssvr_write_info_log (int sd, const char *sub,
                const char *mesg, int logmesg);
int mssvr_load_spk (int sd, double et, char *spk_ref,
        char *segid, msEnvStruct *env);

#ifdef __cplusplus
}
#endif

#endif
