#include "vicmain_c"
#include <string.h>
#include <stdlib.h>

#define NUM_SPK_SUM     5
/**********************************************************************
 
TSUBS: Test Program for additional C Bridges

Revision history:  

	28may00		lwk	replaced dafopr of SPK with spklef, and
				spklef of CK with cklpf (!)

        24may00         lwk     changed ET for SPKSSB lookup, as the old
				date was not in GLL_LONG_2.BSP (!)

	06mar96		lwk	replaced zreset() with zreset1()

	14dec94		lwk	added ckgp,mtxv,pi,scencd,twopi
		  
	06-24-94	JFM	Print statement (ZPRNT) after DAFGS removed.
				SUMMARY array must be unpacked by DAFUS
				prior to printing. (FR 85089)

	03-15-94 	TLT	Original test for bridges:dmpool_1,halfpi,
                                rotate,rotmat,mxv,mxm,mxmt,sct2e,bodeul,
                                m2eul,failed,irfrot,rspool_1,dafgs,dpr,sce2t,
                                dafus,dafbfs,dafhsf,daffna,errprt,spkssb,
                                spkapp,ckgpav,dafopr,dafcls,reset,vminus,
                                xpose,m2q,dafopw,dafps,dafbna,dafada,dafena.

***********************************************************************/


void main44()

{
  double        timout;
  double        spk_dbls[2];
  double        spk_ints[6];
  double	summary[NUM_SPK_SUM];
  double        x_pi;
  double        zpi();
  double        half_pi;
  double        zhalfpi();
  double        two_pi;
  double        ztwopi();
  double        zdpr();
  double	zrpd();
  double	sclk_tol;
  double	sclkdp;
  double        val;
  double        val4;
  double        val5;
  double        val6;
  double        val7;
  double        mout[3][3];
  double        mout2[3][3];
  double        matrix[3][3];
  double        matrix2[3][3];
  double        vin[3];
  double        vout[3];
  double        c_ra,c_dec,c_twist;
  double	etime;
  double	et;
  double	et_lt;
  double	craft_target_lt;
  double        c_matrix[3][3];
  double        target_craft[3];
  double        c_av[3];
  double	ssb_craft[6];
  double	ssb_target[6];
  double	craft_target[6];
  double        craft_target_pos[3]; 
  int           handle;
  int           found;
  int           count;
  int		num_doubles;
  int	        ck_id;
  int		num_ints;	
  int		daf_found;
  int           zfailed();
  char          sclkc[18];
  char          CKfile[200];
  char          LONGfile[200];
  char          SCLKfile[200];
  char          CONSTfile[200];
  char          LEAPSfile[200];
  char		*system = "J2000";
  char		*lighttime = "LT";
  char          *env;
  char          binpool[1024];

  zvmessage("/*************begin tsubs********************/","");
  zreset1();
  initspice();
  zvp("LONG_PATH",LONGfile,&count);
  zdafopr(LONGfile, &handle);
  if (zfailed()) {
    zvmessage(" DAFOPR failed","");
    zreset1();
  }
  else zprnt(4,1,&handle,"after dafopr ...handle = ");

  zdafcls(&handle);
  zvmessage("after dafcls ...","");
  if (zfailed()) zreset1();

  zspklef(LONGfile, &handle);
  zvmessage("after spklef ...","");
  if (zfailed()) zreset1();

  zdafbfs(handle);
  zvmessage("after dafbfs ...","");
  if (zfailed()) zreset1();

  zdafhsf(handle,&num_doubles,&num_ints);
  if (zfailed()) {
    zvmessage(" DAFHSF failed","");
    zreset1();
  }
  else {
    zprnt(4,1,&num_doubles,"after dafhsf ...num_doubles = ");
    zprnt(4,1,&num_ints,"after dafhsf ...num_ints = ");
  }

  zdaffna(&daf_found);
  if (zfailed()) {
    zvmessage(" DAFFNA failed","");
    zreset1();
  }
  else zprnt(4,1,&daf_found,"after daffna ...daf_found = ");

  memset(summary,0,8*NUM_SPK_SUM);
  zdafgs(summary);
  if (zfailed()) zreset1();

  zdafus(summary, num_doubles, num_ints, spk_dbls, spk_ints);
  if (zfailed()) {
    zvmessage(" DAFUS failed","");
    zreset1();
  }
  else {
    zprnt(8,2,spk_dbls,"after dafus ...spk_dbls = ");
    zprnt(4,2,spk_ints,"after dafus ...spk_ints = ");
  }

  et = -3.139E+08;
  zspkssb(299, et, system, ssb_craft);
  if (zfailed()) {
    zvmessage(" SPKSSB failed","");
    zreset1();
  }
  else {
    zprnt(8,6,ssb_craft,"after spkssb ...ssb_craft = ");
    /*  this requests the state of 299 relative to the observer at
     *  ssb_craft, which is also 299, so result must be 0 ... */
    zspkapp(299, et, system, ssb_craft,
          lighttime, craft_target_pos,&craft_target_lt);
    if (zfailed()) {
      zvmessage(" SPKAPP failed","");
      zreset1();
    }
    else {
      zprnt(8,3,craft_target_pos,"after spkapp ...target_pos = ");
      zprnt(8,1,&craft_target_lt,"after spkapp ...craft_target_lt = ");
    }
  }

  zdafcls(&handle);
  zvmessage("after dafcls ...","");
  if (zfailed()) zreset1();

  zvp("CK_PATH",CKfile,&count);
  zdafopr(CKfile,&handle);
  if (zfailed()) {
    zvmessage(" DAFOPR failed","");
    zreset1();
  }
  else zprnt(4,1,&handle,"after dafopr ...handle = ");

  zdafcls(&handle);
  zvmessage("after dafcls ...","");
  if (zfailed()) zreset1();

  zcklpf(CKfile,&handle);
  zvmessage("after cklpf ...","");
  if (zfailed()) zreset1();

  et_lt = et - craft_target_lt;
  zbodeul(299, et_lt, &val4,&val5,&val6,&val7);
  if (zfailed()) {
    zvmessage(" BODEUL failed","");
    zreset1();
  }
  else {
    zprnt(8,1,&val4,"after bodeul ...val4 = ");
    zprnt(8,1,&val5,"after bodeul ...val5 = ");
    zprnt(8,1,&val6,"after bodeul ...val6 = ");
    zprnt(8,1,&val7,"after bodeul ...val7 = ");
  }

  memset(c_matrix,0,8*9);
  memset(c_av,0,8*3);
  timout = 0.0E0;
  sclkdp = 1.383E+10;
  zckgp(-77001,sclkdp,3000.0E0,system,c_matrix,&timout,&found);
  if (zfailed()) {
    zvmessage(" CKGP failed","");
    zreset1();
  }
  else {
    zprnt(8,9,c_matrix,"after ckgp ...cmatrix = ");
    zprnt(8,1,&timout,"after ckgp ...timout = ");
    zprnt(4,1,&found,"after ckgp ...found = ");
  }

  memset(c_matrix,0,8*9);
  memset(c_av,0,8*3);
  timout = 0.0E0;
  sclkdp = 1.383E+10;
  zckgpav(-77001,sclkdp,3000.0E0,system,c_matrix,c_av,&timout,&found);
  if (zfailed()) {
    zvmessage(" CKGPAV failed","");
    zreset1();
  }
  else {
    zprnt(8,9,c_matrix,"after ckgpav ...cmatrix = ");
    zprnt(8,3,c_av,"after ckgpav ...c_av = ");
    zprnt(8,1,&timout,"after ckgpav ...timout = ");
    zprnt(4,1,&found,"after ckgpav ...found = ");
  }

  matrix[0][0]= 0.0E0;
  matrix[1][0]= 1.0E0;
  matrix[2][0]= 0.0E0;
  matrix[0][1]= -1.0E0;
  matrix[1][1]= 0.0E0;
  matrix[2][1]= 0.0E0;
  matrix[0][2]= 0.0E0;
  matrix[1][2]= 0.0E0;
  matrix[2][2]= 1.0E0;
  zm2eul(matrix,3,2,3,&c_twist,&c_dec,&c_ra);
  if (zfailed()) {
    zvmessage(" M2EUL failed","");
    zreset1();
  }
  else {
    zprnt(8,1,&c_twist,"after m2eul ...c_twist = ");
    zprnt(8,1,&c_dec,"after m2eul ...c_dec = ");
    zprnt(8,1,&c_ra,"after m2eul ...c_ra = ");
  }

  vin[0] = 1.0E0;
  vin[1] = -2.0E0;
  vin[2] = 3.0E0;
  zvminus(vin,target_craft);
  if (zfailed()) {
    zvmessage(" VMINUS failed","");
    zreset1();
  }
  else zprnt(8,3,target_craft,"after vminus ...target_craft = ");

  zvp("SCLK_PATH",SCLKfile,&count);
  zldpool(SCLKfile);
  
  zsct2e(-77,sclkdp,&etime);
  if (zfailed()) {
    zvmessage(" SCT2E failed","");
    zreset1();
  }
  else zprnt(8,1,&etime,"after sct2e...etime = -2.078E+08 =? ");

  strcpy(sclkc,"1651033.47.4.1");
  zvmessage(" SCLK (RIM.MOD91.MOD10.MOD8) = 1651033.47.4.0","");
  zscencd(-77,sclkc,&sclkdp);
  if (zfailed()) {
    zvmessage(" SCNCD failed","");
    zreset1();
  }
  else zprnt(8,1,&sclkdp,"after scencd, sclkdp = 12019524032.0 =? ");

  sclkdp = 0.0E0;
  zsce2t(-77,etime,&sclkdp);
  if (zfailed()) {
    zvmessage(" SCE2T failed","");
    zreset1();
  }
  else zprnt(8,1,&sclkdp,"after sce2t...sclkdp = 1.383E+10 =? ");

  val = zdpr();
  if (zfailed()) {
    zvmessage(" DPR failed","");
    zreset1();
  }
  else zprnt(8,1,&val,"degrees per radian is....");

  val = zrpd();
  if (zfailed()) {
    zvmessage(" RPD failed","");
    zreset1();
  }
  else zprnt(8,1,&val,"radians per degree is....");

  x_pi = zpi();
  if (zfailed()) {
    zvmessage(" PI failed","");
    zreset1();
  }
  else zprnt(8,1,&x_pi,"pi is....");

  half_pi = zhalfpi();
  if (zfailed()) {
    zvmessage(" HALFPI failed","");
    zreset1();
  }
  else zprnt(8,1,&half_pi,"halfpi is....");

  two_pi = ztwopi();
  if (zfailed()) {
    zvmessage(" TWOPI failed","");
    zreset1();
  }
  else zprnt(8,1,&two_pi,"twopi is....");

  val = half_pi/2;
  zrotate(val,3,mout);
  if (zfailed()) {
    zvmessage(" ROTATE failed","");
    zreset1();
  }
  else zprnt(8,9,mout,"after rotate  ...mout = ");

  memset(mout2,0,8*9);
  zrotmat(mout,half_pi,1,mout2);
  if (zfailed()) {
    zvmessage(" ROTMAT failed","");
    zreset1();
  }
  else zprnt(8,9,mout2,"after rotmat  ...mout2 = ");

  zmxv(matrix,vin,vout);
  if (zfailed()) {
    zvmessage(" MXV failed","");
    zreset1();
  }
  else zprnt(8,3,vout,"after mxv   ... vout = ");

  zmtxv(matrix,vin,vout);
  if (zfailed()) {
    zvmessage(" MTXV failed","");
    zreset1();
  }
  else zprnt(8,3,vout,"after mtxv   ... vout = ");

  zmxmt(matrix,matrix,mout);
  if (zfailed()) {
    zvmessage(" MXMT failed","");
    zreset1();
  }
  else zprnt(8,9,mout,"after mxmt  ...mout = ");

  matrix2[0][0]= 1.0E0;
  matrix2[1][0]= 0.0E0;
  matrix2[2][0]= 0.0E0;
  matrix2[0][1]= 0.0E0;
  matrix2[1][1]= 1.0E0;
  matrix2[2][1]= 1.0E0;
  matrix2[0][2]= 0.0E0;
  matrix2[1][2]= -1.0E0;
  matrix2[2][2]= 1.0E0;
  zmxm(matrix,matrix2,mout);
  if (zfailed()) {
    zvmessage(" MXM failed","");
    zreset1();
  }
  else zprnt(8,9,mout,"after mxm  ...mout = ");


  memset(mout,0,8*9);
  zirfrot(4,1,mout);
  if (zfailed()) {
    zvmessage(" IRFROT failed","");
    zreset1();
  }
  else zprnt(8,9,mout,"after irfrot  ...mout = ");


/* Thu Oct 31 13:24:50 PST 1996				*/
/* need to take out this test because the new version	*/
/* spice toolkit does not support dmpool anymore	*/
/*  zdmpool_1("dump.out");				*/
  zvmessage("No need to test for this anymore with ","");
  zvmessage("The new SPICE toolkit !!!!","");

  zvmessage("/*************end tsubs********************/","");
}
void zvwait()
{
}
void vwait()
{
}
