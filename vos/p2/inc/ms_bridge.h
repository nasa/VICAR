#ifndef MS_BRIDGE_H
#define MS_BRIDGE_H

#ifdef __cplusplus
extern "C" {
#endif
void	zcorcav		(int*, float*, float*, double*, double*,
			float*, float*, float*, float*, float*,
			float*, float*, float*);
void	zms_erract	(char *x,char *y);
void	zms_vadd_mat	(double*, double*, double*);
void	zms_bodeul	(int, double, double*, double*, double*, double*);
void	zms_bodn2c	(char*, int*, int*);
void	zms_bodvar	(int, char*, int*, double*);
void	zms_ckbss	(int, double, double, int);
void	zms_ckcls	(int);
void	zms_ckpfs	(int, double*, double, double, int,
				double*, double*, double*, int*);
void	zms_cksns	(int*, double*, char*, int*);
void	zms_cklpf	(char*, int*);
void	zms_ckupf	(int);
void	zms_ckw01	(int, double, double, int, char*, int, char*,
				int, double, double*, double*);
void	zms_clpool	();
void	zms_dafada	(void*, int);
void	zms_dafbna	(int, void*, char*);
void	zms_dafcls	(int);
void	zms_dafena	();
void	zms_dafgh	(int*);
void	zms_dafopr	(char*, int*);
void	zms_dafopw	(char*, int*);
void	zms_dafps	(int, int, void*, void*, void*);
double	zms_dpr		();
void	zms_dafus	(void*, int, int, void*, void*);
void	zms_eul2m	(double, double, double, int, int, int, double*);
int	zms_failed	();
double	zms_halfpi	();
void    zms_invert      (double[3][3], double[3][3]);
void	zms_irfrot	(int, int, void*);
void	zms_ldpool	(char*);
void	zms_m2q		(void*, void*);
void	zms_mtxv	(void*, void*, void*);
void	zms_mxm		(void*, void*, void*);
void	zms_mxmt	(void*, void*, void*);
void	zms_mxv		(void*, void*, void*);
double	zms_pi	        ();
void	zms_reclat	(void*, double*, double*, double*);
void	zms_reset	();
void    zms_rotate      (double, int, void*);
void	zms_sce2t 	(int, double, double*);
void	zms_sctiks 	(int, char*, double*);
void	zms_spkapp	(int, double, char*, void*, char*, void*, double*);
void	zms_spklef	(char*, int*);
void	zms_spkssb	(int, double, char*, void*);
void	zms_spkcls	(int);
void	zms_spkuef	(int);
void	zms_spcec	(int*, int*);
void	zms_spcac	(int*, int*, char*, char*);
void	zms_spcb2a	(char*, char*);
void	zms_spca2b	(char*, char*);
void	zms_surfnm	(double, double, double, double*, double*);
void	zms_surfpt	(double*, double*, double, double,
			double, double*, int*);
void    zms_tkfram      (int, double[3][3], int*, int*);
void	zms_txtcls	(int*);
void	zms_txtopn	(char*, int*);
void	zms_txtopr	(char*, int*);
void	zms_utc2et	(char*, double*);
void	zms_vminus	(void*, void*);
double	zms_vsep	(double*, double*);
void	zms_vsub	(double*, double*, double*);
double	zms_vnorm	(double*);
void	zms_xpose	(void*, void*);

#ifdef __cplusplus
}
#endif

#endif
