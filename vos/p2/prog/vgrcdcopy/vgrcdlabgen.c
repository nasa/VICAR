/*										*/
/*  vgrcdlabgen.c								*/
/*										*/
/*  This routine takes a (native format) Engineering Data Record from the 	*/
/*  compressed PDS image CD-ROM and parses out the necessary information to	*/
/*  create the Voyager VICAR label.  This binary header information is stored	*/
/*  on the PDS CD's in VAX format.  For this routine to work correctly, 	*/
/*  this header MUST be translated into a format understandable by the native	*/
/*  machine.  This translation is performed by the trans_eng_hdr() routine.	*/
/*										*/
/*  The original code for vgrcdlabgen() was derived from the VICAR subroutine	*/
/*  vgrlabgen() which worked under VMS only.  This package is designed to allow	*/
/*  for the creation of VGR VICAR images from the PDS CD-ROMs on other 		*/
/*  platforms: namely UN*X and VMS.  Additionally, I should note that all of 	*/
/*  the 'unnecessary' routines were replaced by sprintf() and strncpy().  These	*/
/*  routines were: AUTCON, OUTCON, MVL, BSWAP, ITLA, HEXCON, and TIMEOUT.	*/
/*  The above routines simply formatted numbers for textual representation 	*/
/*  now done by sprintf) and copied bytes around (now done by strncpy and 	*/
/*  memcpy).									*/
/*  										*/
/*  As a last note:  since this is designed to get its input data from the PDS	*/
/*  CD-ROMs, I took advantage of the fact that the fds counts and target name	*/
/*  dont have to be found using sources other than the binary header.  In the	*/
/*  'old' days, the fds counts were retrieved from the binary *prefix* and the	*/
/*  target name was derived from the binary *header*.  Since the target name	*/
/*  is available in ASCII in the PDS label, I just get it and pass it into this	*/
/*  routine as 'targname'.  This should be fine assuming that the PDS label is 	*/
/*  correct.  The advantage in doing this is that I do not have to use zpbname()*/
/*  to lookup the target name.  As for the fds counts, I would have to grok a 	*/
/*  VGR binary line prefix.  I dont want to bother with passing a pointer to a 	*/
/*  structure that is mostly empty anyway.  I have the two numbers from earlier	*/
/*  processing, so I'll just pass them in here.					*/
/*										*/
/*  I guess it should be obvious that this is not you fathers' Oldsmobile ;-)	*/
/*										*/
#include <stdio.h>
#include "vgrimcod.h"   	/* telemery codes */
#include <string.h>
#include "xvmaininc.h"	/* for SUN_SOLARIS_ARCH */

/*#ifdef __unix		/* wrong flag */
#if SUN_SOLARIS_ARCH
#include "uengrhdr.h"
#else
#include "engrhdr.h"
#endif


int check_ind ( int location, int ind) {
char alpha_buf[80];
  if ( ind != 1) {
    sprintf ( alpha_buf," VGRCDLABGEN -- ERROR %d AT LOCATION %d \n",ind,location);
    printf  ( alpha_buf);
    return 1;			/*  keep the compilers happy with a return value*/
  }
}

vgrcdlabgen ( ounit, edr, mod16, mod60, targname, sedrflg, print, exposure)

  int ounit;			/*  VICAR file unit number			*/
  struct edrhdr *edr;		/*  pointer to native format binary header	*/
  unsigned short mod16, mod60;	/*  FDS counts found in previous processing	*/
  char *targname;		/*  obtained from the PDS label on the CD	*/
  int sedrflg, print;		/*  flags to use SEDR (usually NOT) and prints	*/
  float exposure;		/*  usually set to -1.0 to force a table lookup	*/

{
/*                       1         2         3         4         5         6         7
               0123456789012345678901234567890123456789012345678901234567890123456789012 */ 
char v01[73]= "                     800     800 800 800 L 1                          SC";
char v02[73]= "VGR-*   FDS *****.**   PICNO **********   SCET **.*** **:**:**         C";
char v03[73]= "xA CAMERA  EXP *******.* MSEC FILT x(xxxxxx)  xx GAIN  SCAN RATE **:1  C";
char v04[73]= "ERT **.*** **:**:**   */** ******* RES   VIDICON TEMP ****.** DEG C    C";
char v05[73]= "IN/xxxxxx/xx OUT/xxxxxx/xx     GANYMEDE    DSS #**   BIT SNR ****.***  C";
char v06[73]= " xxxxx A/xxxxxxxx B/xxxx C/xxxx D/xxxxxxxx ETLM/xxxxxxxxxxxxxxxxxxxxS AC";
char v07[73]= "NA OPCAL xx(*******.*MSEC)PIXAVG ***/* OPERATIONAL MODE *(******)     AC";
char v08[73]= "CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC";
char v09[73]= "NA   xxx  xxxx  xxx   xxx   xxx   xxx   xxx   xxx   x x  x xxxxxxxxxx AC";
char v10[73]= "WA   xxx  xxxx  xxx   xxx   xxx   xxx   xxx   xxx   x x  x xxxxxxxxxx AC";
char v11[73]= "LSB_TRUNC=XXX  TLM_MODE=XXXX  COMPRESSION=XXX                          C";
char v12[73]= "INCIDENCE ANGLE ***.*     EMISSION ANGLE ***.*    PHASE ANGLE ***.*    C";
char v13[73]= "NORTH AZIMUTH ANGLE ***.*     KM/LINE *****.**      KM/SAMP *****.**   C";
char v14[73]= "ALT ******** KM SL.RANGE ******** KM VFOV ******* KM HFOV ******* KM   C";
char v15[73]= "LAT  ***(UL) ***(UR) ***(LL) ***(LR) ***(C) ***(SUB S/C) ***(SUBSOL)   C";
char v16[73]= "LONG ***(UL) ***(UR) ***(LL) ***(LR) ***(C) ***(SUB S/C) ***(SUBSOL)   C";
/*                       1         2         3         4         5         6         7
               0123456789012345678901234567890123456789012345678901234567890123456789012 */ 
/*******************************************************************/
/* NOTE: The last label record output should have an "L" in cc 72  */
/*******************************************************************/

float expo[24] = {    5.0,    7.5,   12.5,   15.0,   22.5,   30.0,
		     45.0,   60.0,   90.0,  120.0,  180.0,  240.0,
		    360.0,  480.0,  720.0,  960.0, 1440.0, 1920.0,
		   2880.0, 3840.0, 5760.0, 7680.0,11520.0,15360.0};

float expo_new[31] =   {      5.0,    15.0,    90.0,   120.0,   180.0,
			    240.0,   360.0,   480.0,   720.0,   960.0,
			   1440.0,  1920.0,  2880.0,  3840.0,  5760.0,
			   7680.0, 11520.0, 15360.0,   960.0,  1440.0,
			   1920.0,  2880.0,  3840.0,  5760.0,  7680.0,
			  11520.0, 15360.0, 23040.0, 30720.0, 46080.0,
			  61440.0};

char fpname[2][8][7] = {
	"CH4/JS ","BLUE   ","CLEAR  ","VIOLET ",	/* WA FILTERS */
	"SODIUM ","GREEN  ","CH4/U  ","ORANGE ",

	"CLEAR  ","VIOLET ","BLUE   ","ORANGE ",	/* NA FILTERS */
	"CLEAR  ","GREEN  ","GREEN  ","UV     " };

char mtbl[8][6] = {
	"NOSHUT","NOSHUT","NAONLY","WAONLY",		/* CAMERA MODES */
	"BOTALT","BSIMAN","BODARK","BOTSIM" };

char shut_mode[4][11] = {"NORMAL    ","LONG START",
			"LONG END  ","LONG OPEN "};

char yesno[2][3]={ "YES","NO " };

char ascii[16] = {'0','1','2','3','4','5','6','7',
		  '8','9','0','A','B','C','D','F'};

short edit[32] = {					/* CAMERA EDIT & RESOLUTION*/
  	101,0,0,0,101,0,305,0,101,101,0,203,101,0,0,101,0,101,
   /*   ------ JUN 7 94   IM7 & 9 replaces GS4B & 4C      */
   /*   101,0,0,0,101,0,305,0,101,101,0,101,101,0,0,101,0,101,  */
	-110,0,102,101,-105,0,103,0,102,101,304,101,101,101 };
		/* e.g. -105 is 1/5 EDIT PARTIAL RES */

union parword_ind parword_ind;

short date[6];
short ss[10],ns[10];
int nlab;
int imcode,icamera;
int expcd,filter,gain,scan_rate,res;
int year,day,minute,msec;
int source,mrk3_id,mrk4_id;
int i,l,n,ind,mode,pixel_average,g1_voltage;
char station_name[13];
float exp,noise;
float vidicon_temp = -80.0;	/* Dummy value for vidicon temp */
char format[6],buf[8], temp[80];
int lsb_trunc;                          /* lsb truncation flag */

  /* Spacecraft ID: VGR-1 or VGR-2     */
  if ( edr->sds.fid.sc_id ) v02[4]='1';
    else v02[4]='2';

  /* Spacecraft Clock		     */
  sprintf ( temp, "%d.%02d", mod16, mod60);
  strncpy ( &v02[12], temp, 8);

  /* Picture No,e.g."1234U-234" */
  if (edr->sds.picno[0] != 0) strncpy ( &v02[29], &edr->sds.picno[0], 10);

  /* Spacecraft Event Time	     */
  if ((sedrflg) || (edr->scet.day_year.year != 0) || (edr->scet.day_year.day != 0) || (edr->scet_min != 0)  || (edr->scet_msec != 0))  {
    date[0] = edr->scet.day_year.year;
    date[1] = edr->scet.day_year.day;
    minute = edr->scet_min;
    date[2] = minute/60;
    date[3] = minute%60;
    msec = edr->scet_msec;
    date[4] = msec/1000;
    date[5] = msec%1000;
  sprintf ( temp, "%02d.%03d %02d:%02d:%02d", date[0], date[1], date[2], date[3], date[4]);
  strncpy ( &v02[47], temp, 15);
  }

  /* Camera ID */
  imcode = edr->sds.fid.imcode;
  icamera = edr->subcom.subword1.camera_id;
  if ( imcode == GS4 )  strncpy ( &v03[0], "PWS      ", 9);
  else if ( imcode == GS2 )  strncpy ( &v03[0], "PRA      ", 9);
  else if ( icamera == 0 )  strncpy ( &v03[0], "WA CAMERA", 9);
  else strncpy ( &v03[0], "NA CAMERA", 9);

  expcd = edr->subcom.subword3.exposure;	/* Exposure time */
  if ( exposure < 0.0 )  {			/*  Force a table lookup  */
    if ( edr->subcom.subword3.exposure_table == 1 )  {
      exp = expo_new[expcd-1];			/* in milliseconds */
      if ( expcd > 0 && expcd < 32 )  {
        sprintf ( temp, "%9.1f", exp);
        strncpy ( &v03[15], temp, 9);
      }
    }
    else  {
      exp = expo[expcd-1];			/* in milliseconds */
      if ( expcd > 0 && expcd < 25 )  {
        sprintf ( temp, "%9.1f", exp);
        strncpy ( &v03[15], temp, 9);
      }
    }
  }
  else  {					/*  Use given exposure value  */
    sprintf ( temp, "%9.1f", exposure);
    strncpy ( &v03[15], temp, 9);
  }

  filter = edr->subcom.subword3.filter;		/* Filter position */
  v03[35] = ascii[filter];
  strncpy ( &v03[37], &fpname[icamera][filter][0], 6);

  gain = edr->subcom.word20.bits2.gain_state;	/* Gain state */
  if (gain == 1)  strncpy ( &v03[46], "HI", 2);
  else  strncpy ( &v03[46], "LO", 2);

  zvgrimfmt ( imcode, format, &scan_rate, ss, ns);     /* Scan rate */
  if (imcode == IM26) scan_rate = 2;
  else if (imcode == OC3) scan_rate = 3;
  sprintf ( temp, "%2d", scan_rate);
  strncpy ( &v03[65], temp, 2);

  date[0] = edr->ert.day_year.year;		/* Earth Received Time */
  date[1] = edr->ert.day_year.day;
  minute = edr->ert_min;
  date[2] = minute/60;
  date[3] = minute%60;
  msec = edr->ert_msec;
  date[4] = msec/1000;
  date[5] = msec%1000;
  sprintf ( temp, "%02d.%03d %02d:%02d:%02d", date[0], date[1], date[2], date[3], date[4]);
  strncpy ( &v04[4], temp, 15);

  res = edit[imcode];		/* Pixel editing and resolution */
  if (res < 0)  {
    strncpy    ( &v04[27], "PARTIAL", 7);
    res = -res;
  }
  else strncpy ( &v04[27], "FULL   ", 7);
  if (imcode == IMS)  strncpy ( &v04[22], ".843", 4);
  else  {
    sprintf ( temp, "%c/%2d", ascii[res/100], res%100);
    strncpy ( &v04[22], temp, 4);
  }

  sprintf ( temp, "%7.2f", vidicon_temp);	/* Vidicon temperature */
  strncpy ( &v04[54], temp, 7);	

  if (edr->output_volume[0] != 0)  {		/* EDR tape id & file number */
    strncpy ( &v05[3], &edr->output_volume[0], 6);
    sprintf ( temp, "%2d", edr->fileno);
    strncpy ( &v05[10], temp, 2);
  }

  if ( targname)  sprintf ( temp, "%-10s", targname);      /* Target body obtained from PDS label on CD */
  else  sprintf ( temp, "%10s", "          ");
  strncpy ( &v05[31], temp, 10);

  source = edr->gcf[1].source_station;		/* DSN Source Station ID */
  ind = zdsnid ( &source, &mrk3_id, &mrk4_id, station_name);
  if ( ind == 0)                              sprintf ( temp, "**");
  else if ( edr->gcf[1].s2.dsn_mark_no == 1)  sprintf ( temp, "%02d", mrk4_id);
  else                                        sprintf ( temp, "%02d", mrk3_id);
  strncpy ( &v05[48], temp, 2);

  noise = (float)edr->sds.system_noise_temp_max/128.0; /* System Noise Temperature */
  sprintf ( temp, "%8.3f", noise);
  strncpy ( &v05[61], temp, 8);

/*	This code from VGRLOG not fully understood    */
/*	Currently, I'm not even using it
  i = 2*edr->subcom.subword3.na_elect_cal + edr->subcom.subword3.wa_elect_cal + edr->subcom.word20.word;
  bswap(&i,1);
  hexcon(&i,buf,4);
  mvl(&buf[3],&v06[1],5);

  bswap(&edr->subcom.parword_a,15);
  hexcon(&edr->subcom.parword_a,&v06[9],4);
  hexcon(&edr->subcom.parword_b,&v06[20],2);
  hexcon(&edr->subcom.parword_c,&v06[27],2);
  hexcon(&edr->subcom.parword_d,&v06[34],4);
  hexcon(&edr->subcom.na_sample1,&v06[48],10);
  bswap(&edr->subcom.parword_a,15);
*/

  sprintf ( temp, "%08.1f", exp);
  strncpy ( &v07[12], temp, 8);

					     /* Pixel average */
  pixel_average = 8 * edr->subcom.word20.bits2.pixel_average;
  sprintf ( temp, "%03d", pixel_average);
  strncpy ( &v07[33], temp, 3);
  ind = edr->subcom.word20.bits2.pixel_avg_ind;
  if (ind == 0) v07[37] = '0';
  else          v07[37] = '1';

  mode = edr->subcom.parword_a.mode;	     /* Camera mode */
  v07[56] = ascii[mode];
  strncpy ( &v07[58], &mtbl[mode][0], 6);

  parword_ind.word = edr->subcom.parword_a_ind | edr->subcom.parword_d_ind;
  sprintf ( temp, "%s", yesno[1 - edr->subcom.subword3.na_elect_cal]);
  strncpy ( &v09[5], temp, 3);
  if ( !parword_ind.bits.na_cycle_ind)  strncpy ( &v09[10], "PREP", 4);
  else                                  strncpy ( &v09[10], "READ", 4);
  sprintf ( temp, "%s", yesno[parword_ind.bits.na_beam_ind]);
  strncpy ( &v09[16], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.na_shutter_reset]);
  strncpy ( &v09[22], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.na_shutter_open]);
  strncpy ( &v09[28], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.na_shutter_close]);
  strncpy ( &v09[34], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.na_light_flood]);
  strncpy ( &v09[40], temp, 3);
  if ( edr->subcom.parword_b.na_exposure == 0)  strncpy ( &v09[46], "YES", 3);
  else                                          strncpy ( &v09[46], "NO ", 3);
  v09[52] = ascii[edr->subcom.parword_b.na_filt];
  if ( edr->subcom.parword_b.na_filt_step_mode == 0) v09[54] = 'P';
  else                                               v09[54] = 'S';
  strncpy ( &v09[59], shut_mode[edr->subcom.parword_d.na_shutter_select], 10);

  sprintf ( temp, "%s", yesno[1 - edr->subcom.subword3.wa_elect_cal]);
  strncpy ( &v10[5], temp, 3);
  if ( !parword_ind.bits.wa_cycle_ind)  strncpy ( &v10[10], "PREP", 4);
  else                                  strncpy ( &v10[10], "READ", 4);
  sprintf ( temp, "%s", yesno[parword_ind.bits.wa_beam_ind]);
  strncpy ( &v10[16], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.wa_shutter_reset]);
  strncpy ( &v10[22], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.wa_shutter_open]);
  strncpy ( &v10[28], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.wa_shutter_close]);
  strncpy ( &v10[34], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.wa_light_flood]);
  strncpy ( &v10[40], temp, 3);
  if ( edr->subcom.parword_c.wa_exposure == 0)  strncpy ( &v10[46], "YES", 3);
  else                                          strncpy ( &v10[46], "NO ", 3);
  v10[52] = ascii[edr->subcom.parword_c.wa_filt];
  if ( edr->subcom.parword_c.wa_filt_step_mode == 0) v10[54] = 'P';
  else                                               v10[54] = 'S';
  strncpy ( &v10[59], shut_mode[edr->subcom.parword_d.wa_shutter_select], 10);

  i = edr->subcom.word20.bits2.fds_code;
  g1_voltage = edr->subcom.word20.bits2.g1_voltage;
  v09[57] = '*';
  v10[57] = '*';
  if (i == 5) v09[57] = ascii[g1_voltage];	
  else v10[57] = ascii[g1_voltage];	

  strncpy ( &v11[24], format, 5);		/* TLM FORMAT	*/

  /* process truncation code */
  if (imcode == GS2 || imcode == GS4)   {	/* restored Jun 10 1994   */
    strncpy ( &v11[10], "N/A", 3);
    strncpy ( &v11[42], "N/A", 3);
  }
  else  {
    if ( icamera == 0) lsb_trunc = edr->subcom.word20.bits.wa_lsb_trunc;
    else               lsb_trunc = edr->subcom.word20.bits.na_lsb_trunc;
    if ((edr->nept_byte.compression == 1) && (lsb_trunc == 0))  
      strncpy ( &v11[10], "ON ", 3);
    else  
      strncpy ( &v11[10], "OFF", 3);
  }

  if ( edr->nept_byte.compression == 1)
    strncpy ( &v11[42], "ON ", 3);
  else  
    strncpy ( &v11[42], "OFF", 3);

  if ( !sedrflg)  {		        /*If SEDR not present*/
    v11[71]='L';  	    		/* Put an 'L' in cc 72 of last rec*/
    nlab = 11;
  }

  if ( print == 1)  {
    printf ( "%s\n", v01);
    printf ( "%s\n", v02);
    printf ( "%s\n", v03);
    printf ( "%s\n", v04);
    printf ( "%s\n", v05);
    printf ( "%s\n", v06);
    printf ( "%s\n", v07);
    printf ( "%s\n", v08);
    printf ( "%s\n", v09);
    printf ( "%s\n", v10);
    printf ( "%s\n", v11);
  }

  ind = zladd ( ounit, "HISTORY", "LAB01", v01, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(1, ind);
  ind = zladd ( ounit, "HISTORY", "LAB02", v02, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(2, ind);
  ind = zladd ( ounit, "HISTORY", "LAB03", v03, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(3, ind);
  ind = zladd ( ounit, "HISTORY", "LAB04", v04, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(4, ind);
  ind = zladd ( ounit, "HISTORY", "LAB05", v05, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(5, ind);
  ind = zladd ( ounit, "HISTORY", "LAB06", v06, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(6, ind);
  ind = zladd ( ounit, "HISTORY", "LAB07", v07, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(7, ind);
  ind = zladd ( ounit, "HISTORY", "LAB08", v08, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(8, ind);
  ind = zladd ( ounit, "HISTORY", "LAB09", v09, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(9, ind);
  ind = zladd ( ounit, "HISTORY", "LAB10", v10, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(10, ind);
  ind = zladd ( ounit, "HISTORY", "LAB11", v11, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(11, ind);

  if ( sedrflg)  {			    	/*If SEDR present*/
    v16[71] = 'L';         			/* Put an 'L' in cc 72 of last rec*/
    nlab    = 16;

    if ( print == 1)  {
      printf ( "%s\n", v12);
      printf ( "%s\n", v13);
      printf ( "%s\n", v14);
      printf ( "%s\n", v15);
      printf ( "%s\n", v16);
    }

    ind = zladd ( ounit, "HISTORY", "LAB12", v12, "FORMAT", "STRING", "ULEN", 72, 0);
    check_ind(12, ind);
    ind = zladd ( ounit, "HISTORY", "LAB13", v13, "FORMAT", "STRING", "ULEN", 72, 0);
    check_ind(13, ind);
    ind = zladd ( ounit, "HISTORY", "LAB14", v14, "FORMAT", "STRING", "ULEN", 72, 0);
    check_ind(14, ind);
    ind = zladd ( ounit, "HISTORY", "LAB15", v15, "FORMAT", "STRING", "ULEN", 72, 0);
    check_ind(15, ind);
    ind = zladd ( ounit, "HISTORY", "LAB16", v16, "FORMAT", "STRING", "ULEN", 72, 0);
    check_ind(16, ind);
  }

  ind = zladd ( ounit, "HISTORY", "NLABS", &nlab, "FORMAT", "INT", 0);
  check_ind(17, ind);

}
