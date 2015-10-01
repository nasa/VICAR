$!****************************************************************************
$!
$! Build proc for MIPL module summary
$! VPACK Version 1.9, Monday, August 28, 2000, 14:32:00
$!
$! Execute by entering:		$ @summary
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module summary ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to summary.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("summary.imake") .nes. ""
$   then
$      vimake summary
$      purge summary.bld
$   else
$      if F$SEARCH("summary.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake summary
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @summary.bld "STD"
$   else
$      @summary.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create summary.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack summary.com -
	-s summary.c -
	-i summary.imake -
	-t tstsummary.pdf -
	-o summary.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create summary.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*===========================================================================*
 |  summary.c -- Routine to print Image File summary	
 |
 |   06-Mar-95		Made portable for unix			
 |    4-Jan-90          WPL - Minor Modifications for GS4B/4C PWS Formats	
 |   26-Apr-89  41469   PXZ-lie about the scan rate of OC3 and IM26 as specified
 |                      by the FR
 |   19-Aug-88   n/a    pxz - added support for NSPP formats
 |    8-Aug-88  	scp B776-5, get the exposure value from a passed
 |			parameter instead of the table.  Deleted table.
 |   19-Jul-88  38147   The SNR parameters were changed to signed short
 |   18-Jul-88  37489   Computation of nmissing for OC3 is based on 260 lines
 |    8-Jul-88  38667   Inserted missing value into the new exposure table
 |   24-Mar-88	36608	pxz - added support for NDPP formats
 *===========================================================================*/
#include "xvmaininc.h" 
#include "vgrimcod.h"
#include "vgredrfmt.h"
#include <ctype.h>
#include <string.h>
#include <stdio.h>

struct edrhdr edrh;
struct edrlin ldr;
 
summary(edrbyt,ldrbyt,ix,exposure)
unsigned char  *edrbyt[1024];
unsigned char  *ldrbyt[1024];
	int	ix;
	float	exposure;
{
union parword_ind parword_ind;
static float   expo[24] = {   5.0,    7.5,   12.5,   15.0,   22.5,   30.0,
			     45.0,   60.0,   90.0,  120.0,  180.0,  240.0,
			    360.0,  480.0,  720.0,  960.0, 1440.0, 1920.0,
			   2880.0, 3840.0, 5760.0, 7680.0,11520.0,15360.0};

static float   expo_new[31] =   {     5.0,    15.0,    90.0,   120.0,   180.0,
				    240.0,   360.0,   480.0,   720.0,   960.0,
				   1440.0,  1920.0,  2880.0,  3840.0,  5760.0,
				   7680.0, 11520.0, 15360.0,   960.0,  1440.0,
				   1920.0,  2880.0,  3840.0,  5760.0,  7680.0,
				  11520.0, 15360.0, 23040.0, 30720.0, 46080.0,
				  61440.0};

static char fpname [2][8][7] = {
	"CH4/JS ","BLUE   ","CLEAR  ","VIOLET ",	/* WA FILTERS */
	"SODIUM ","GREEN  ","CH4/U  ","ORANGE ",

	"CLEAR  ","VIOLET ","BLUE   ","ORANGE ",	/* NA FILTERS */
	"CLEAR  ","GREEN  ","GREEN  ","UV     " };
static char mtbl[8][6] = {
	"NOSHUT","NOSHUT","NAONLY","WAONLY",		/* CAMERA MODES */
	"BOTALT","BSIMAN","BODARK","BOTSIM" };
static struct {
	short number;
	char type[3];
	} spacecraft[8] = {
		32,"MOS",
		31,"MOS",
		 0,"PTM",
		 0,"UNK",
		42,"SIM",
		41,"SIM",
		 0,"TST",
		 0,"TST" };

static char ascii[17] = {'0','1','2','3','4','5','6','7',
			 '8','9','0','A','B','C','D','F','\0'};

/*
1234567890123456789012345678901234567890123456789012345678901234567890123456789
---------------------------------MIPL 80.314.22.42.25--------------------------
VGR-1   FDS=34850.55   PICNO=1619S1-004   TARGET=GANYMEDE        EDR=123456/01
SCET=80.314 21:12:34.923   CAM=ISSNA   FILT=3(VIOLET)      FL=800  COUNT=    14
FERT=80.314 22:37:44.336   GAIN=LO     EXP=  .0125         PL=  0  TYPE=MOS
LERT=80.314 22:37:48.336   MODE=IM-12  10:1 WAONLY         ML=  0  SOURCE=SDR

# OF MINOR FRAMES     UNREADABLE RECORDS=1234                     MIN      MAX
    FROM WBDL=1234    SEQUENCE BREAKS   =1234   SYS NOISE TEMP  123456 - 123456
    FROM IDR =1234    SYNC CODE ERRORS  =1234   SYMBOL SNR      123456 - 123456
    FROM SDR =1234    FDS COUNT ERRORS  =1234   AGC             123456 - 123456
    MISSING  =1234    BIT-ERR TOLERANCE =3
1234567890123456789012345678901234567890123456789012345678901234567890123456789
*/

char msg[120];   	/* message to be build and send to zvmessage */
int sc,imcode,icamera,mod16,mod60,count;
int expcd,filter,gain,scan_rate;
int year,day,minute,msec;
int i,l,n,ind,mode;
int sc_number;
int nfull,npartial,nmissing;
int n_idr,n_wbdl,n_sdr,n_miss;
short int spix[20],npix[20];
float exp,exp_val,noise_min,noise_max,snr_min,snr_max,agc_min,agc_max;
short date[6];
char no_date[20] = "**.*** **:**:**.***";
char date_string[22];
char format[6];
char picno_msg[11] = "****X*-***";
char target_body[11] = "          "; /* ten spaces */
char out_vol[6];
char output_volmsg[10] = "******/**"; 
char camera_strng[6] = "     ";           /* five blanks */
char filter_strng[10] = "*(      )";
char fltr_strng[7] = "      ";      /* six blanks */
char spcrft[4] = "   ";             /* three blanks */
char gain_strng[3] = "  ";          /* two blanks */
char source_strng[6] = "WBDL";
char mtbl_mode[8] = "      ";       /* six blanks */
char frmt_strng[7] = "     " ;      /* five blanks */
 
getedrdata(edrbyt,ldrbyt);
date[0] = edrh.irt_year;		/* MIPL Internally Received Time */
date[1] = edrh.irt_day;
minute = edrh.irt_min;
date[2] = minute/60;
date[3] = minute%60;
msec = edrh.irt_msec;
date[4] = msec/1000;
date[5] = msec%1000;
sprintf(date_string, "%02d.%03d %02d:%02d:%02d.%03d", date[0],date[1],
     date[2],date[3],date[4], date[5]);
	
sprintf( msg,
"------- SUMMARY Version 06 Mar 1995 ---- Date:%s ------------",
  date_string);

zvmessage(msg," ");

mod16 = ldr.fds_mod16;			/* Spacecraft Clock		     */
mod60 = ldr.fds_mod60;

if (edrh.sds.picno[0] != 0)
	strncpy(picno_msg, edrh.sds.picno, 10);
if (edrh.sds.target_body[0] != 0)
	strncpy(target_body, edrh.sds.target_body, 10);

if (edrh.output_volume[0] != 0)		/* EDR tape id & file number */
	{
	strncpy(out_vol, edrh.output_volume, 6);
	n = edrh.fileno + 1;
	sprintf(output_volmsg, "%s/%02d", out_vol, n);
	}

sprintf(msg,
 "VGR-*   FDS=%05d.%02d   PICNO=%s   TARGET=%s      EDR=%s",
	mod16,mod60,picno_msg,target_body,output_volmsg);

sc = edrh.sds.fid.sc_id;		/* Spacecraft ID: VGR-1 or VGR-2     */
if (sc==1) msg[4]='1';
else msg[4]='2';

zvmessage(msg," ");


if ((edrh.scet_year != 0) || (edrh.scet_day != 0) ||	/* Spacecraft Event Time */
    (edrh.scet_min != 0)  || (edrh.scet_msec != 0))
	{
	date[0] = edrh.scet_year;
	date[1] = edrh.scet_day;
	minute = edrh.scet_min;
	date[2] = minute/60;
	date[3] = minute%60;
	msec = edrh.scet_msec;
	date[4] = msec/1000;
	date[5] = msec%1000;
	sprintf(date_string, "%02d.%03d %02d:%02d:%02d.%03d", date[0],date[1],
		date[2],date[3],date[4], date[5]);
	}

imcode = edrh.sds.fid.imcode;		/* Camera ID */
icamera = edrh.subcom.camera_id;
/*  ------------------              Add GS4B/4C     Oct. 26, 89         */    
if ((imcode==GS4) || (imcode == IM2WP) ||
    (imcode==GS4B) || (imcode == GS4C) )    /* Oct. 26, 89              */
    strncpy(camera_strng, "PWS  ",5);

else
    if (imcode==GS2) 
    	strncpy(camera_strng, "PRA  ",5);
    else
	if (icamera==0)
	   strncpy(camera_strng, "ISSWA", 5);
	else
	   strncpy(camera_strng, "ISSNA", 5);

if ( (imcode!=GS4) && (imcode!=IM2WP) && (imcode!=GS2) &&
    (imcode!= GS4B) &&  (imcode!= GS4C) )  /*  Oct. 26, 89              */
     
	{
	filter = edrh.subcom.filter;	/* Filter position */
	strncpy(fltr_strng, fpname[icamera][filter],6);
	sprintf(filter_strng, "%c(%s)",ascii[filter],fltr_strng);
	}

nfull = edrh.sds.nfull;
npartial = edrh.sds.npartial;
switch(imcode)
{ case IMQ:  nmissing = 480 - nfull - npartial;
	     break;
  case IM24: nmissing = 195 - nfull;
	     break;
  case IM2D:
  case IM25: nmissing = 585 - nfull;
	     break;
  case IM26: nmissing = 266 - nfull - npartial;
	     break;
  case OC3:  nmissing = 210 - nfull - npartial;
	     break;
  case IM2W: nmissing = 720 - nfull - npartial;
	     break;
  case IM2WP: nmissing = 80 - nfull - npartial;
	     break;
  default :  nmissing = 800 - nfull - npartial;
}
count = edrh.subcom.picture_count;

sprintf(msg,
    "SCET=%s   CAM=%s   FILT=%s      FL=%03d   COUNT=%05d",    
     date_string,camera_strng,filter_strng,nfull,count );

zvmessage(msg," ");

date[0] = edrh.ert_year;		/* Earth Received Time */
date[1] = edrh.ert_day;
minute = edrh.ert_min;
date[2] = minute/60;
date[3] = minute%60;
msec = edrh.ert_msec;
date[4] = msec/1000;
date[5] = msec%1000;
sprintf(date_string, "%02d.%03d %02d:%02d:%02d.%03d", date[0],date[1],
	date[2],date[3],date[4], date[5]);
if ( (imcode!=GS4) && (imcode != IM2WP) && (imcode!=GS2) &&
     (imcode!=GS4B) && (imcode != GS4C) )    /* Oct. 26, 89    */

{	gain = edrh.subcom.word20.bits.gain_state;	/* Gain state */
	if (gain == 1)
	     strncpy(gain_strng, "HI",2);
	else
	     strncpy(gain_strng, "LO",2);

	if (exposure < 0.0)				/* use the table */
	{   expcd = edrh.subcom.exposure;		/* Exposure time */
	    if (edrh.subcom.exposure_table == 1) 	/* new table */
	    {	exp = expo_new[expcd-1]/1000.;	/* in seconds */
		if (expcd>0 && expcd<32) 
		     exp_val = exp;
	    }
	    else
	    {	exp = expo[expcd-1]/1000.;		/* in seconds */
		if (expcd>0 && expcd<25)
		    exp_val = exp;
	    }
	}
	else
	{   exposure /= 1000.0;
	    exp_val = exposure ; 
	}
}
sc_number = edrh.gcf[0].sc_number;
for (i=0; i<8; i++)
	{
	if (spacecraft[i].number == sc_number)
		{
		strncpy(spcrft, spacecraft[i].type, 3);
		break;
		}
	}
sprintf(msg,"FERT=%s   GAIN=%s     EXP=%8.4f        PL=%3d   TYPE=%s",
      date_string, gain_strng, exp_val, npartial, spcrft ); 

zvmessage(msg," ");

zvgrimfmt(imcode,format,&scan_rate,spix,npix);	/* Scan rate */
strncpy(frmt_strng, format, 5);
if (imcode == OC3)
   scan_rate = 3;
if (imcode == IM26)
   scan_rate = 2;

date[0] = edrh.lert_year;		/* Earth Received Time */
date[1] = edrh.lert_day;
minute = edrh.lert_min;
date[2] = minute/60;
date[3] = minute%60;
msec = edrh.lert_msec;
date[4] = msec/1000;
date[5] = msec%1000;
sprintf(date_string, "%02d.%03d %02d:%02d:%02d.%03d", date[0],date[1],
     date[2],date[3],date[4], date[5]);

mode = edrh.subcom.parword_a.mode;      /* Camera mode */

n_idr  = edrh.sds.nmf_from_idr;
n_wbdl = edrh.sds.nmf_from_wbdl;
n_sdr  = edrh.sds.nmf_from_sdr;
n_miss = edrh.sds.nmf_missing;
if (n_idr > n_wbdl)
	strncpy(source_strng, "IDR ",4);
if ((n_sdr > n_wbdl) && (n_sdr > n_idr))
	strncpy(source_strng, "SDR ",4);
strncpy(mtbl_mode, mtbl[mode],6);
sprintf(msg,
 "LERT=%s   MODE=%s  %2d:1  %s        ML=%3d   SOURCE=%s", date_string ,
 frmt_strng, scan_rate, mtbl_mode, nmissing, source_strng);
zvmessage(msg," ");

n = edrh.sds.nbadrec;
sprintf(msg,
"# OF MINOR FRAMES:    UNREADABLE RECORDS=%4d                     MIN      MAX"
,n);

zvmessage(msg," ");

n = edrh.sds.nlog_seq_breaks;
noise_min = (float) edrh.sds.system_noise_temp_min/128.0; /* Sys Noise Temp */
noise_max = (float) edrh.sds.system_noise_temp_max/128.0;
sprintf(msg,
"    FROM WBDL=%4d    SEQUENCE  BREAKS  =%4d   SYS NOISE TEMP  %6.3f - %6.3f",
n_wbdl, n, noise_min, noise_max);
zvmessage(msg," ");

n = edrh.sds.pn_errs;
snr_min = edrh.sds.symbol_snr_min / 128.0;		 /* Symbol SNR */
snr_max = edrh.sds.symbol_snr_max/128.0;
sprintf(msg,
"    FROM IDR =%4d    SYNC CODE ERRORS  =%4d   SYMBOL SNR      %6.3f - %6.3f",
   n_idr, n, snr_min, snr_max);  
zvmessage(msg," ");

n = edrh.sds.fds_count_errs;
agc_min = (float) edrh.sds.agc_min/128.0; 			/* AGC */
agc_max = (float) edrh.sds.agc_max/128.0;
sprintf(msg,
"    FROM SDR =%4d    FDS COUNT ERRORS  =%4d   AGC            %6.3f -%6.3f",
   n_sdr, n, agc_min, agc_max );
zvmessage(msg," ");

n = ldr.sds.allowed_pn_errs;

sprintf(msg,
"    MISSING  =%4d    BIT-ERR TOLERANCE =%1d                           ",
  n_miss, n);
zvmessage(msg," ");

return(1);
}
getedrdata(ebyt,lbyt) 
unsigned char ebyt[1024];
unsigned char lbyt[1024];

/*  the following moves data from a byte sequence into the edr data      */
/*  structure since this processing is called by the 'native'machine,    */
/*  each machine will create this data structure from the input byte     */
/*  sequence making the code portable                                    */

{
int i,ii;
unsigned short tempi;
const short byt = 256;

i=0;
edrh.recid = ebyt[i++];                       
edrh.fileno= ebyt[i++];                       

i = 6;
tempi = ebyt[i]+(ebyt[i+1]*byt);               /* 6, 7  */
i += 2;
edrh.ert_day = (tempi & 0x01ff);
edrh.ert_year= (tempi >> 9);
edrh.ert_min = ebyt[i]+(ebyt[i+1]*byt);		/* 8, 9  */  
i += 2;
edrh.ert_msec= ebyt[i]+(ebyt[i+1]*byt);		/* 10,11 */
i += 2;
tempi = ebyt[i]+(ebyt[i+1]*byt); 		/* 12,13 */
i += 2;
edrh.lert_day = (tempi & 0x01ff);
edrh.lert_year= (tempi >> 9);
edrh.lert_min = ebyt[i]+(ebyt[i+1]*byt);       /* 14,15 */
i += 2;
edrh.lert_msec= ebyt[i]+(ebyt[i+1]*byt);       /* 16,17 */

i = 30;
tempi = ebyt[i]+(ebyt[i+1]*byt);              /* 30,31 */
i += 2;
edrh.scet_day = (tempi & 0x01ff);
edrh.scet_year= (tempi >> 9);
edrh.scet_min = ebyt[i]+(ebyt[i+1]*byt);       /* 32,33 */
i += 2;
edrh.scet_msec= ebyt[i]+(ebyt[i+1]*byt);       /* 34,35 */

i=54;
for (ii=0; ii<= 5; ii++) 
	edrh.output_volume[ii]=ebyt[i++];       /* 54,55,56,57,58,59 */
i=76;
tempi = ebyt[i]+(ebyt[i+1]*byt);
edrh.gcf[0].sc_number=(tempi >> 8);

i=108;
tempi = ebyt[i]+(ebyt[i+1]*byt);           /* 108,109 */
i += 2;
edrh.irt_day = tempi & 0x01ff;
edrh.irt_year= tempi >> 9;
edrh.irt_min = ebyt[i]+(ebyt[i+1]*byt);    /* 110,111 */
i += 2;
edrh.irt_msec= ebyt[i]+(ebyt[i+1]*byt);       /* 112,113 */

i=118;
tempi = ebyt[i]+(ebyt[i+1]*byt);              /* 118,119 */
edrh.sds.fid.sc_id=(tempi & 0x0001);
edrh.sds.fid.imcode=((tempi >> 1) & 0x001f);
edrh.sds.fid.major_data_type=((tempi >> 6)& 0x0003);
i +=2;
edrh.sds.system_noise_temp_min= ebyt[i]+(ebyt[i+1]*byt);  /* 120,121 */
i += 2;
edrh.sds.system_noise_temp_max= ebyt[i]+(ebyt[i+1]*byt);  /* 122,123 */
i = 124;
edrh.sds.symbol_snr_min= ebyt[i]+(ebyt[i+1]*byt); 	/* 124,125 */
i += 2;
edrh.sds.symbol_snr_max= ebyt[i]+(ebyt[i+1]*byt); 	/* 126,127 */
i += 2;
edrh.sds.agc_min= ebyt[i]+(ebyt[i+1]*byt);       	/* 128,129 */
i += 2;
edrh.sds.agc_max= ebyt[i]+(ebyt[i+1]*byt);      	/* 130,131 */
i += 2;
edrh.sds.pn_errs= ebyt[i]+(ebyt[i+1]*byt);      	/* 132,133 */

i = 144;
edrh.sds.nfull= ebyt[i]+(ebyt[i+1]*byt);      		/* 144,145 */
i += 2;
edrh.sds.npartial= ebyt[i]+(ebyt[i+1]*byt);       	/* 146,147 */
i += 2;
edrh.sds.nbadrec= ebyt[i]+(ebyt[i+1]*byt);       	/* 148,149 */
i += 2;
edrh.sds.nlog_seq_breaks= ebyt[i]+(ebyt[i+1]*byt);	/* 150,151 */

i=160;
edrh.sds.nmf_from_idr= ebyt[i]+(ebyt[i+1]*byt); 	/* 160,161 */
i += 2;
edrh.sds.nmf_from_wbdl= ebyt[i]+(ebyt[i+1]*byt); 	/* 162,163 */
i += 2;
edrh.sds.nmf_from_sdr= ebyt[i]+(ebyt[i+1]*byt); 	/* 164,165 */
i += 2;
edrh.sds.nmf_missing= ebyt[i]+(ebyt[i+1]*byt); 		/* 166,167 */

i=170;
for (ii=0;ii<=9;ii++)
	edrh.sds.picno[ii]= ebyt[i++];			/* 170..,179 */
for (ii=0;ii<=9;ii++)
	edrh.sds.target_body[ii]= ebyt[i++];		/* 180..,189 */
i=192;
tempi = ebyt[i]+(ebyt[i+1]*byt); 			/* 192,193 */
i += 2;
edrh.subcom.shuttered_pic=(tempi & 0x007f);
if (tempi & 0x8000) 
	edrh.subcom.camera_id= 1;
    else
	edrh.subcom.camera_id=0;
tempi = ebyt[i]+(ebyt[i+1]*byt);			/* 194,194 */
i += 2;
/* subcom.segment_number, subcom.line_number   */
tempi = ebyt[i]+(ebyt[i+1]*byt);			/* 196,197 */
i += 2;
edrh.subcom.filter=((tempi >> 1) & 0x0007);
edrh.subcom.exposure=((tempi >> 4) & 0x001f);
edrh.subcom.exposure_table=((tempi >> 11) & 0x0001);
edrh.subcom.picture_count= ebyt[i]+(ebyt[i+1]*byt);	/* 198,199 */
i += 2;
tempi = ebyt[i]+(ebyt[i+1]*byt);			/* 200,201 */
i += 2;
edrh.subcom.parword_a.mode=( (tempi >> 13) & 0x0007);
i=232;
tempi = ebyt[i]+(ebyt[i+1]*byt);			/* 232,233 */
edrh.subcom.word20.bits.gain_state=((tempi >> 4) &0x0001);
ldr.fds_mod16=lbyt[22]+(lbyt[23]*byt);
ldr.fds_mod60=lbyt[24]+(lbyt[25]*byt);
ldr.sds.allowed_pn_errs=lbyt[142]+(lbyt[143]*byt);
return(1);
}
 
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create summary.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY summary

   To Create the build file give the command:

	$ vimake summary                     (VMS)
   or
	% vimake summary                     (Unix)


*************************************************************************/

#define SUBROUTINE summary

#define MODULE_LIST summary.c

#define FTN_STRING
#define P2_SUBLIB

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$Test_File:
$ create tstsummary.pdf
! tstsummary.pdf:  Summary is tested via the  program imlist
!                mipl:[mipl.vgr]e2575228.1 2575228.2 
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
local path type=string init="wms_test_work:[testdata.mipl.vgr]"
body
let _onfail="continue"
let $autousage="none"
let $echo="yes"
if ($syschar(1)="UNIX")
   let path="/project/test_work/testdata/mipl/vgr/"
end-if

imlist inp=&"path"e2575228.1 'FULL
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create summary.hlp
1 SUMMARY

  Routine to print out an Image File summary.

  Calling Sequence:  CALL SUMMARY(EDR,LDR)

  Arguments:  All arguments are inputs:

	INTEGER*2 EDR(128)	EDR header record.
	INTEGER*2 LDR(512)	EDR record for line 1.

	The EDR header and line records are input from the binary label
	portion of the Image File.  See program EDRVAL for an example.

2 History

  Original Programmer: Gary Yagi, December 4, 1984
  Made Portable for Unix: C Randy Schenk (CRI) March 6, 1995
  Current Cognizant Programmer: Gary Yag
  Source Language: C
  Revisions:
    25 Aug 00  GMY  Add reference to gcfdata.h and vgrsubcom.h (AR-104738)

2 Operation

  The summary information is extracted from the EDR header and printed
  out.  The first line record is needed to get the FDS count since the
  count in the EDR header does not necessarilly correspond to the start
  of the frame.

$ Return
$!#############################################################################
