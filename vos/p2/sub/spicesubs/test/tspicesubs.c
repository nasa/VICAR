/************************************************************

PROGRAM TO TEST SPICESUBS

*************************************************************/
#include	"spiceinc.h"
#include        "vicmain_c"
#include <stdlib.h>

#define		LEN_TEST	7

kernel_db_typ	kernel_db[MAX_KERNELS];
int		kernel_count;

void main44()
{
  extern kernel_db_typ	kernel_db[MAX_KERNELS];
  extern int		kernel_count;
  body_id_typ	ids;
  basics_typ	basics;
  sp_summary_typ	sp_summary;
  c_summary_typ		c_summary;
  radii_typ	radius;
  double	et;
  double	et_lt;
  double	sclk;
  double	sclk_tol;
  double	craft_target_lt;
  double	tcb_lon,tcb_lat;
  double	ssb_craft[6];
  double	ssb_target[6];
  double	craft_target[6];
  double	target_craft[3];
  double	target_craft_bodyfixed[3];
  double	me_matrix[9];
  double	c_matrix[9];
  double	c_av[3];
  double	ra, dec, twist, lambda;
  int	i;
  int	status;
  int	target_id;
  int	sc_id;
  int	dim_radii;
  int	handle;
  int	ck_id;
  int   count;
  short	year = 1989;
  short	day = 237;
  short	hour = 6;
  short	minute = 2;
  short	second = 21;
  short	milli = 0;
  short	c_year, c_day, c_hour, c_minute, c_second, c_milli;
  char	mess[80];
  char	in_test_str[LEN_TEST];
  char	test_str[LEN_TEST+1];
  char	craft[LEN_CRAFT+1];
  char	target[LEN_TARGET+1];
  char	center[LEN_TARGET+1];
  char	sol[LEN_TARGET+1];
  char	scet[LEN_SCET+1];
  char	scet1[LEN_SCET+1];
  char	scet2[LEN_SCET+1];
  char  SPfile[132];
  char  CKfile[132];
  char *env;
  char binpool[1024];

/*  zvwait(1); */         /* vwait not available on alpha-vms */
  zvmessage("Let's test MASSAGE_STRING first (know your ascii) ...","");
  zvmessage("First print the 7 input characters of the string to massage:","");
  zvmessage(" ","");
  strcpy(in_test_str, "abcde");
  in_test_str[5] = ' ';
  in_test_str[6] = ' ';
  sprintf(mess, " 1st char: %x; 2nd char: %x; 3rd char: %x; 4th char: %x",
          in_test_str[0], in_test_str[1], in_test_str[2], in_test_str[3]);
  zvmessage(mess,"");
  sprintf(mess, " 5th char: %x; 6th char: %x; 7th char: %x;",
          in_test_str[4], in_test_str[5], in_test_str[6]);
  zvmessage(mess,"");

  zvmessage(" ","");
  zvmessage("Then the 8 output characters - trailing spaces removed, null",""); 
  zvmessage("terminated, and changed to upper case.","");
  zvmessage(" ","");

  massage_string(test_str, in_test_str, LEN_TEST);
  sprintf(mess, " 1st char: %x; 2nd char: %x; 3rd char: %x; 4th char: %x",
           test_str[0], test_str[1], test_str[2], test_str[3]);
  zvmessage(mess,"");
  sprintf(mess, " 5th char: %x; 6th char: %x; 7th char: %x; 8th char: %x",
           test_str[4], test_str[5], test_str[6], test_str[7]);
  zvmessage(mess,"");
  zvmessage(" ","");
  zvmessage("Wasn't that fun. Now testing SUMMARIZE_SPK:","");
  zvmessage(" ","");

/*****init_spice******/
  zerrprt("SET","NONE");
  zerract("SET","RETURN");

/*  zvwait(1);*/         /* called so that linkage editor can find it */
                         /** vwait not available on alpha-vms **/
  zclpool();

#if UNIX_OS
{
  env = getenv("BINPOOL");
  if( env != NULL)
    strcpy(binpool,env);
  else {
    zvmessage("Environment name: BINPOOL not defined for binpool.ker"," ");
    zmabend("");}
  zrspool_1(binpool);
}
#else
  zrspool_1("BINPOOL");
#endif
/*****end init_spice******/

  zvp("SP_KERNEL",SPfile,&count);
  zdafopr(SPfile, &handle);
  if (zfailed())
  {
    zvmessage("dafopr failed","");
    exit();
  }
  zvmessage("If you got here, INIT_SPICE worked","");
  zvmessage(" ","");

  summarize_spk(handle, &sp_summary);
  zdafcls(handle);
  if (zfailed())
  {
    zvmessage("SP Kernel closing failed","");
    exit();
  }
  zvmessage("The following segments (SUMMARIZE_SPK) are in the venus.spk kernel:","");
  zvmessage(" ","");

  for (i = 0; i < sp_summary.sc_cnt; i++)
  {
    sprintf(mess, "spacecraft segment for: %d", sp_summary.sc_summ[i].spacecraft);
    zvmessage (mess,"");
  }
  zvmessage(" ","");
  for (i = 0; i < sp_summary.body_cnt; i++)
  {
    sprintf(mess, "body segment for: %d", sp_summary.body_summ[i].body);
    zvmessage (mess,"");
  }
  zvmessage(" ","");
  zvmessage("Now testing SUMMARIZE_CK:","");
  zvmessage(" ","");

  zvp("CK_KERNEL",CKfile,&count);
  zdafopr(CKfile,&handle);
  if (zfailed())
  {
    zvmessage("dafopr failed","");
    exit();
  }

  summarize_ck(handle, &c_summary);
  zdafcls(handle);
  if (zfailed())
  {
    zvmessage("C Kernel closing failed","");
    exit();
  }
  zvmessage("The following arrays (SUMMARIZE_CK) are in the venus.ck kernel:","");
  zvmessage(" ","");

  for (i = 0; i < c_summary.count; i++)
  {
    sprintf(mess,
        "array for: spacecraft: %d, instrument: %d,", 
         c_summary.point[i].spacecraft, c_summary.point[i].instrument);
    zvmessage (mess,"");
    sprintf(mess,
        "            begin_et: %f, end_et: %f", 
         c_summary.point[i].begin_et,   c_summary.point[i].end_et);
    zvmessage (mess,"");
  }

  zvmessage(" ","");
  status = load_spice(GLL);
  if (status == SUCCESS)  zvmessage("LOAD_SPICE worked","");
  else
  {
    zvmessage("Oops, LOAD_SPICE failed","");
    exit();
  }
  zvmessage("Time to work the GET_BODY versus GET_BODY_IDS subroutines...","");
  zvmessage(" ","");
  strcpy(craft, "GLL","");
  strcpy(target, "VENUS","");
  status = get_body_ids(craft, target, &ids);
  if (status == SUCCESS)
  {
    sprintf(mess, " Given craft %s, we get sc_id %d", craft, ids.sc);
    zvmessage (mess,"");
    sprintf(mess, " Given target %s, we get target_id %d", target, ids.target);
    zvmessage (mess,"");
    sprintf(mess, " We also get a center_id of %d and a sun_id of %d",
                     ids.center, ids.sol);
    zvmessage (mess,"");
  }
  else
  {
    zvmessage("Oops, get_body_ids failed","");
    exit();
  }
  zvmessage(" ","");
  zvmessage("Can GET_BODY return what we want?","");
  zvmessage(" ","");

  status = get_body(BODY_NAME, &ids.sol, sol);
  if (status == SUCCESS)
  {
    sprintf(mess, " Given sun_id: %d, we get body name: %s", ids.sol, sol);
    zvmessage (mess,"");
  }
  else
  {
    zvmessage("Oops, GET_BODY failed","");
    exit();
  }
  status = get_body(BODY_NAME, &ids.center, center);
  status = get_body(BODY_ID, &ids.center, center);
  if (status == SUCCESS)
  {
    sprintf(mess, " Given center: %s, we get center_id: %d", center,
                     ids.center);
    zvmessage (mess,"");
  }
  else
  {
    zvmessage("Oops, GET_BODY failed","");
    exit();
  }
  zvmessage(" ","");
  zvmessage("Time to work the two SCET SUBROUTINES...","");
  zvmessage(" ","");

  sprintf(mess, " Given catalog scets: %d, %d, %d, %d, %d, and %d...",
                   year, day, hour, minute, second, milli);
  zvmessage (mess,"");
  combine_scet(year, day, hour, minute, second, milli, scet1);
  sprintf(mess, " combine_scet returns scet:  %s",scet1);
  zvmessage (mess,"");
  zvmessage(" ","");
  strcpy(scet2, "1990-44 // 05:58:16:962","");
  sprintf(mess, " Given scet: %s...", scet2);
  zvmessage (mess,"");
  split_scet(scet2, &c_year, &c_day, &c_hour, &c_minute, &c_second, &c_milli);
  sprintf(mess, " split_scet returns: %d, %d, %d, %d, %d, and %d",
                   c_year, c_day, c_hour, c_minute, c_second, c_milli);
  zvmessage (mess,"");
  zvmessage(" ","");

  zvmessage("Now it's alot of work to get to GET_LATLON but here goes (using get_basics)...","");
  zvmessage(" ","");

  zbodvar(ids.target,"RADII",&dim_radii,&radius);

  zutc2et(scet2,&et); /* Ephemeris time in secs from J2000 */
  zsce2t(ids.sc,et,&sclk);  /* Convert ET to spacecraft clock */
  sclk_tol = 3000.;
  ck_id = -77001;	     /* GLL SSI instrument ID */
  status = get_basics(ids,ck_id,et,sclk,sclk_tol,"J2000",&basics,
	&c_matrix,&c_av);

  if (status == INSUFF_EPHEMERIS)
  {
    zvmessage("Oops, GET_BASICS failed","");
    exit();
  }
  zreset1();				/* Reset if INSUFF_POINTING	*/
  zvminus(basics.craft_target_pos,target_craft);
  eul2mat(basics.me_ra, basics.me_dec, basics.me_twist, me_matrix);
  zmxv(me_matrix, target_craft, target_craft_bodyfixed);
  get_latlon(radius, target_craft_bodyfixed, &tcb_lon, &tcb_lat);

  zvmessage("If it all worked, lat should be -2.97 and lon should be 176.30","");
  sprintf(mess, " Calculated lat = %6.2f, calculated lon = %6.2f",
                  tcb_lat, tcb_lon);
  zvmessage(mess,"");

  zvmessage(" ","");
  zvmessage("Let's get Venus' euler angles at our scet to test EUL2MAT...","");
  zvmessage(" ","");

  zbodeul(ids.target, et, &ra, &dec, &twist, &lambda);
  eul2mat(ra, dec, twist, me_matrix);
  zvmessage("If it all worked, the matrix should be: -0.331","");
  zvmessage("                                        -0.943","");
  zvmessage("                                         0.018","");
  zvmessage("                                         0.867","");
  zvmessage("                                        -0.312","");
  zvmessage("                                        -0.388","");
  zvmessage("                                         0.371","");
  zvmessage("                                        -0.113","");
  zvmessage("                                         0.922","");
  sprintf(mess, " Calculated matrix = %5.3f", me_matrix[0]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[1]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[2]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[3]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[4]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[5]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[6]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[7]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[8]);	
  zvmessage(mess,"");

  zvmessage(" ","");
  zvmessage("Now let's test CHGMAT and change said matrix from J2000 to B1950...","");
  zvmessage(" ","");
  chgmat(J2000, B1950, &me_matrix);
  zvmessage("If it worked, the matrix should be:     -0.320","");
  zvmessage("                                        -0.947","");
  zvmessage("                                         0.018","");
  zvmessage("                                         0.871","");
  zvmessage("                                        -0.302","");
  zvmessage("                                        -0.388","");
  zvmessage("                                         0.373","");
  zvmessage("                                        -0.108","");
  zvmessage("                                         0.922","");
  sprintf(mess, " Matrix in B1950   = %5.3f", me_matrix[0]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[1]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[2]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[3]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[4]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[5]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[6]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[7]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", me_matrix[8]);	
  zvmessage(mess,"");
  zvmessage(" ","");
  zvmessage("Now let's test CHGVEC and change craft_target from J2000 to B1950...","");
  zvmessage(" ","");
  chgvec(basics.craft_target_pos, J2000, B1950, &craft_target);
  zvmessage("If it worked, the vector should be:       -617472.680","");
  zvmessage("                                          1350399.125","");
  zvmessage("                                           672263.099","");
  sprintf(mess, " Vector in B1950   = %5.3f", craft_target[0]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", craft_target[1]);	
  zvmessage(mess,"");
  sprintf(mess, "                   = %5.3f", craft_target[2]);	
  zvmessage(mess,"");            
  zvmessage("That's all folks","");      
}
