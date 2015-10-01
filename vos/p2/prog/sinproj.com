$!****************************************************************************
$!
$! Build proc for MIPL module sinproj
$! VPACK Version 1.9, Monday, December 07, 2009, 17:02:49
$!
$! Execute by entering:		$ @sinproj
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module sinproj ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to sinproj.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("sinproj.imake") .nes. ""
$   then
$      vimake sinproj
$      purge sinproj.bld
$   else
$      if F$SEARCH("sinproj.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sinproj
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sinproj.bld "STD"
$   else
$      @sinproj.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sinproj.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sinproj.com -mixed -
	-s sinproj.c -
	-i sinproj.imake -
	-p sinproj.pdf -
	-t tstsinproj.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sinproj.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "vicmain_c"

#define PI 	       3.14159265358979
#define	MAXTASKS       200	/* Number of label levels to search */
#define TRUE           1
void labelvalue(int unit, char *key, char *type, void *value, int size);

float    degppix, fprojsamp;
float    lat_uc_i, lat_lc_i, lon_cl_i, lon_cr_i, proj_lon_i;
float    lat_uc,   lat_lc, lon_cl, lon_cr, proj_lon;

int      nli, nsi, projsampi, inp, out, pix_size,  wrap;
int      nl,  ns,  projsamp,   sl, el, ss, es;
int      fl_lat_uc, fl_lat_lc, fl_lon_cl, fl_lon_cr, fl_proj_lon;
int      fl_sl, fl_nl, fl_ss, fl_ns, fl_projsamp;
void calc_output_file_parms(void);
void calc_and_write_output(void);
void initialize_variables(void);
void get_label_values(void);
void get_input_parms(void);
void add_label_items(void);

void main44(void)
{
   initialize_variables();

   zifmessage ("SINPROJ version 31-OCT-94");

   zveaction("SA", "");

   zvunit(&inp, "INP", 1, NULL);
   zvopen(inp, "OP", "READ", NULL);

   get_label_values();
   get_input_parms();
   calc_output_file_parms();

   zvunit(&out, "OUT", 1, NULL);
   zvopen(out, "OP","WRITE","U_NL",nl,"U_NS",ns, NULL);

   calc_and_write_output();

   add_label_items();

   zvclose(inp, NULL);
   zvclose(out, NULL);
}

void initialize_variables(void)
{

   degppix = fprojsamp = 0.0;
   lat_uc_i = lat_lc_i = lon_cl_i = lon_cr_i = proj_lon_i = 0.0;
   lat_uc =  lat_lc =  lon_cl =  lon_cr =  proj_lon = 0.0;

   nli =nsi =projsampi =inp =out =pix_size = wrap = 0;
   nl = ns = projsamp = sl = el = ss =es = 0;

   fl_lat_uc =fl_lat_lc =fl_lon_cl =fl_lon_cr =fl_proj_lon = 0;
   fl_sl =    fl_nl =    fl_ss =    fl_ns =fl_projsamp = 0;

   return;
}


/*---------------------------------------------------------------------------*/
/* get values from input file vicar label                                    */

void get_label_values(void)
{
   float diff;

   labelvalue(inp, "PROJSAMP", "INT",  &projsampi, sizeof(projsampi));
   labelvalue(inp, "PROJ_LON", "REAL", &proj_lon_i, sizeof(proj_lon_i));
   labelvalue(inp, "LON_CL",   "REAL", &lon_cl_i, sizeof(lon_cl_i));
   labelvalue(inp, "LON_CR",   "REAL", &lon_cr_i, sizeof(lon_cr_i));
   labelvalue(inp, "LAT_UC",   "REAL", &lat_uc_i, sizeof(lat_uc_i));
   labelvalue(inp, "LAT_LC",   "REAL", &lat_lc_i, sizeof(lat_lc_i));

   zvget( inp, "NL", &nli, "NS", &nsi, "PIX_SIZE", &pix_size, NULL);

   diff = lon_cr_i - lon_cl_i;
   if (diff < 0) diff += 360.0;
   degppix = diff / nsi;

   if (fabs(degppix + diff - 360) < degppix)
      wrap = TRUE;

   /*----- adjust latitudes and longitudes from the edges of the pixels      */
   /*      to the centers,  which the program calculates from                */

   lat_uc_i -= degppix/2;
   lat_lc_i += degppix/2;
   lon_cl_i += degppix/2;
   lon_cr_i -= degppix/2;

   return;
}

/*---------------------------------------------------------------------------*/
/* get parameters from pdf and set flags                                     */

void get_input_parms(void)
{

   zvp("LAT_UC",   &lat_uc,     &fl_lat_uc);
   zvp("LAT_LC",   &lat_lc,     &fl_lat_lc);
   zvp("LON_CL",   &lon_cl,     &fl_lon_cl);
   zvp("LON_CR",   &lon_cr,     &fl_lon_cr);
   zvp("PROJ_LON", &proj_lon,   &fl_proj_lon);
   zvp("SL",       &sl,         &fl_sl);
   zvp("NL",       &nl,         &fl_nl);
   zvp("SS",       &ss,         &fl_ss);
   zvp("NS",       &ns,         &fl_ns);
   zvp("PROJSAMP", &projsamp,   &fl_projsamp);

   /*----- adjust latitudes and longitudes from the edges of the pixels      */
   /*      to the centers,  which the program calculates from                */

   lat_uc -= degppix/2;
   lat_lc += degppix/2;
   lon_cl += degppix/2;
   lon_cr -= degppix/2;


   return;
}


/*---------------------------------------------------------------------------*/
/* add label values to output files                                         */

void add_label_items(void)
{
   int status;

   /*----- adjust latitudes and longitudes to be at the edges of the pixels  */
   /*      instead of the centers,  which the program calculates from        */

   lon_cl -= degppix/2;
   lon_cr += degppix/2;
   lat_uc += degppix/2;
   lat_lc -= degppix/2;

   status = zladd(out,"HISTORY", "PROJ_LON", &proj_lon, "FORMAT","REAL", NULL);
   status = zladd(out,"HISTORY", "LON_CL",   &lon_cl,   "FORMAT","REAL", NULL);
   status = zladd(out,"HISTORY", "LON_CR",   &lon_cr,   "FORMAT","REAL", NULL);
   status = zladd(out,"HISTORY", "LAT_UC",   &lat_uc,   "FORMAT","REAL", NULL);
   status = zladd(out,"HISTORY", "LAT_LC",   &lat_lc,   "FORMAT","REAL", NULL);
   status = zladd(out,"HISTORY", "PROJSAMP", &projsamp, "FORMAT","INT", NULL);

   return;
}

/*---------------------------------------------------------------------------*/
/* function to get values from vicar label                                   */

void labelvalue(int unit, char *key, char *type, void *value, int size)
{
   int   instance[MAXTASKS],j;
   char  task[MAXTASKS][8];
   int   length;
   int   status;
   int   numtask = MAXTASKS;
   char  message[81];

   /* placed in the value buffer (assuming string length < MAXLABELSIZE.     */

   memset(value,0,size);
   zlhinfo(unit,task[0],instance,&numtask, NULL);
   for (j = numtask-1; j >= 0 ; j--) {
      status = zlget (unit,"HISTORY",key,value,"ERR_ACT","","HIST",task[j],
             "INSTANCE",instance[j],"FORMAT",type,"LENGTH",&length,NULL);
      if (status == 1) break;
      if (status != CANNOT_FIND_KEY) {
         zvsignal(unit,status,1);
      }
   }

   /* It the keyword value not successfully found, indicate with an error.   */

   if (status != 1) {
      sprintf(message,"Error in getting label item %s\n",key);
      zvmessage(message,0);
      zvsignal(unit,status,1);
   }
   return;
}


/*---------------------------------------------------------------------------*/
/* sort of a messy way to do this .... however ....                          */

void calc_output_file_parms(void)
{
   int bad = 0;


   if (fl_lat_uc && fl_sl) {
      zvmessage("both lat_uc and sl can't be specified",0);
      bad = TRUE;
   }
   if (fl_lat_lc && fl_nl) {
      zvmessage("both lat_lc and nl can't be specified",0);
      bad = TRUE;
   }
   if (fl_lon_cl && fl_ss) {
      zvmessage("both lon_cl and ss can't be specified",0);
      bad = TRUE;
   }
   if (fl_lon_cr && fl_ns) {
      zvmessage("both lon_cr and ns can't be specified",0);
      bad = TRUE;
   }

   if (bad == TRUE)
      zabend();


   if (fl_lat_uc) {
      sl = 1 + 0.5 - (lat_uc - lat_uc_i) / degppix;
      lat_uc = lat_uc_i + (degppix * (1 - sl));
   }
   else if (fl_sl)
      lat_uc = lat_uc_i + (degppix * (1 - sl));
   else {
      lat_uc = lat_uc_i;
      sl = 1;
   }

   if (fl_lat_lc) {
      el = 1 + 0.5 - (lat_lc - lat_uc_i) / degppix;
      nl = el - sl + 1;
      lat_lc = lat_uc_i + (degppix * (1 - el));
   }
   else if (fl_nl) {
      el = sl + nl - 1;
      lat_lc = lat_uc_i + (degppix * (1 - el));
   }
   else {
      lat_lc = lat_lc_i;
      el = nli;
      nl = el - sl + 1;
   }


   if (fl_lon_cl) {
      ss = projsampi + 1.0 + (lon_cl - proj_lon_i) / degppix;
      lon_cl = proj_lon_i + (ss - projsampi - 0.5) * degppix;
   }
   else if (fl_ss) {
      if (ss < 1 || ss > nsi){
         zvmessage("ss must be within sample range of input file",0);
         zabend();
      }
      lon_cl = proj_lon_i + (ss - projsampi - 0.5) * degppix;
   }
   else if (!fl_proj_lon && !fl_projsamp) {
      lon_cl = lon_cl_i;
      ss = 1;
   }
   while (lon_cl <    0.0) lon_cl += 360;
   while (lon_cl >= 360.0) lon_cl -= 360;


   if (fl_lon_cr) {
      es = projsampi + 1.0 + (lon_cr - proj_lon_i) / degppix;
      ns = es - ss + 1;
      lon_cr = proj_lon_i + (es - projsampi - 0.5) * degppix;
   }
   else if (fl_ns) {
      es = ss + ns - 1;
      lon_cr = proj_lon_i + (es - projsampi - 0.5) * degppix;
   }
   else if (!fl_proj_lon && !fl_projsamp) {
      lon_cr = lon_cr_i;
      es = nsi;
      ns = es - ss + 1;
   }
   while (lon_cr <    0.0) lon_cr += 360;
   while (lon_cr >= 360.0) lon_cr -= 360;


   if (fl_proj_lon) {
      while (proj_lon <    0.0) proj_lon += 360;
      fprojsamp = projsampi + (proj_lon - proj_lon_i) / degppix;
      if (fprojsamp >= 0)
         projsamp = (int) floor (fprojsamp + 0.5);
      else
         projsamp = (int) ceil (fprojsamp - 0.5);
      if (!fl_lon_cr && !fl_lon_cl && !fl_ss && !fl_ns) {
         if (wrap){
            ss = projsamp - nsi/2;
            while (ss < 1){
               ss += nsi;
               projsamp += nsi;
            }
            ns = nsi;
            es = ss + ns - 1;
            while (es > nsi) es -= nsi;         
            lon_cl = proj_lon_i + (ss - projsampi) * degppix;
            lon_cr = proj_lon_i + (es - projsampi) * degppix;
            es = ss + ns - 1;
	 }
         else {
            lon_cl = lon_cl_i;
            lon_cr = lon_cr_i;
            ss = 1;
            es = nsi;
            ns = es - ss + 1;
            if (proj_lon < lon_cl || proj_lon > lon_cr){
               zvmessage("projsamp must be in image longitude range",0);
               zabend();
	    }
         } 
      }
      while (proj_lon >= 360.0) proj_lon -= 360;
   }
   else if (fl_projsamp) {
      if (projsamp < 1){
         zvmessage("projsamp must be positive",0);
         zabend();
      }
      proj_lon = proj_lon_i + (projsamp - projsampi) * degppix;
      if (!fl_lon_cr && !fl_lon_cl && !fl_ss && !fl_ns) {
         if (wrap){
            ss = projsamp - nsi/2;
            while (ss < 1){
               ss += nsi;
               projsamp += nsi;
	    }
            ns = nsi;
            es = ss + ns - 1;
            while (es > nsi) es -= nsi;         
            lon_cl = proj_lon_i + (ss - projsampi) * degppix;
            lon_cr = proj_lon_i + (es - projsampi) * degppix;
            es = ss + ns - 1;
            while (lon_cl <    0.0) lon_cl += 360;
            while (lon_cr >= 360.0) lon_cr -= 360;
	 }
         else{
            lon_cl = lon_cl_i;
            lon_cr = lon_cr_i;
            ss = 1;
            es = nsi;
            ns = es - ss + 1;
            if (proj_lon < lon_cl || proj_lon > lon_cr){
               zvmessage("projsamp must be in image longitude range",0);
               zabend();
            }
	 }
      }
   }
   /* default proj_lon is halfway between starting and ending longitudes   */
   /* of the reprojection                                                    */
   else {
      projsamp = ss + ns / 2;
      proj_lon = proj_lon_i + (projsamp - projsampi) * degppix;
      while (proj_lon <    0.0) proj_lon += 360;
      while (proj_lon >= 360.0) proj_lon -= 360;
   }
   projsamp -= ss + 1;
   return;
}





void calc_and_write_output(void)
{


float    latitude, cos_lat;
float    d_lo_po, d_ro_lo, d_li_pi, d_ri_li;
float    offset_pixels, offset_degrees;

int      offset_pix, buf_size, w_pixels;
int      line, samp, nsamps;
int      ss_oi, es_oi, ss_o, es_o, ns_o;
int      ss_i, es_i;

char     *outbuffer, *to;

   /*------------------------------------------------------------------------*/
   /* allocate space for buffer lines are read into and written out of       */
   /*    get space for 4 extra pixels just in case of pixel calc errors      */

   buf_size = ns * pix_size;
   outbuffer = (char *)malloc(buf_size + 4 * pix_size);
   if (!outbuffer) {
      zvmessage("Failed trying to allocate outbuffer",0);
      zabend();
   }
   outbuffer += 2 * pix_size;

   /*------------------------------------------------------------------------*/
   /* get offset in degrees between input and output projection longitudes   */

   offset_degrees = proj_lon - proj_lon_i;
   if (offset_degrees >  180) offset_degrees -= 360.;
   if (offset_degrees < -180) offset_degrees += 360.;

   d_li_pi = lon_cl_i - proj_lon_i;
     while (d_li_pi >  180.0) d_li_pi -= 360.0;
     while (d_li_pi < -180.0) d_li_pi += 360.0;
   d_lo_po = lon_cl - proj_lon;
     while (d_lo_po >  180.0) d_lo_po -= 360.0;
     while (d_lo_po < -180.0) d_lo_po += 360.0;
   d_ro_lo = lon_cr - lon_cl;
     while (d_ro_lo < 0.0) d_ro_lo += 360.0;
   d_ri_li = lon_cr_i - lon_cl_i;
     while (d_ri_li < 0.0) d_ri_li += 360.0;

   for (line = sl; line <= el; line++) {
      memset(outbuffer, 0, buf_size);

      latitude = lat_uc_i + (degppix * (1 - line));
      cos_lat = cos (PI * latitude / 180);
      w_pixels = 360 * cos_lat / degppix;
      if (w_pixels == 0) w_pixels = 1;
      offset_pixels = offset_degrees * cos_lat / degppix;

      /*---------------------------------------------------------------------*/
      /*--- round-off differently for positive and negative offsets ---------*/

      if (offset_pixels >= 0)
         offset_pix = (int) floor (offset_pixels + 0.5);
      else
         offset_pix = (int) ceil (offset_pixels - 0.5);

      /*---------------------------------------------------------------------*/
      /*--- find edges of imagery in output file numbering  -----------------*/

      ss_o = projsamp + d_lo_po * cos_lat / degppix + 1.0;
      es_o = ss_o     + d_ro_lo * cos_lat / degppix + 0.5;
      if (w_pixels == 1) es_o = ss_o;
      ns_o = es_o - ss_o + 1;

      if ((ss_o < 1 && es_o < 1) || (ss_o > ns && es_o > ns)) {
            ss_o = 1;  es_o = 1,  ns_o = 0;
      }

      if (ss_o < 1) {
          ss_o = 1;  ns_o = es_o - ss_o + 1;
      }

      if (es_o > ns) {
          es_o = ns;  ns_o = es_o - ss_o + 1;
      }

      /*---------------------------------------------------------------------*/
      /*--- ss_oi = left edge of output imagery in sample numbering of input */

      ss_oi = projsampi + offset_pix - projsamp + ss_o;
      es_oi = ss_oi + ns_o - 1;

      /*---------------------------------------------------------------------*/
      /*--- find edges of input imagery in input file numbering  ------------*/

      ss_i = projsampi + d_li_pi * cos_lat / degppix + 1.0;
      es_i = ss_i      + d_ri_li * cos_lat / degppix + 0.5;
      if (w_pixels == 1) es_i = ss_i;


      if (ns_o == 0) {
         es_oi = ss_oi; /* don't write out anything */
      }
      else if (ss_oi >= ss_i && ss_oi <= es_i && es_oi >= ss_i && es_oi <= es_i) {
         samp = ss_oi;
         nsamps = ns_o;
         to = outbuffer + (ss_o - 1) * pix_size;
         if (nsamps != 0) {
            zvread(inp, to, "LINE",line,"SAMP",samp,"NSAMPS",nsamps, NULL);
         }
      }

      else if (ss_oi <= ss_i && es_oi >= es_i) {
         samp = ss_i;
         nsamps = es_i - ss_i +1;
         to = outbuffer + (ss_o + (ss_i - ss_oi)) * pix_size;
         if (nsamps != 0) {
            zvread(inp, to, "LINE",line,"SAMP",samp,"NSAMPS",nsamps, NULL);
         }
      }

      else if (es_oi > es_i) {
         if (ss_oi <= es_i) {
            samp = ss_oi;
            nsamps = es_i - ss_oi + 1;
            to = outbuffer + (ss_o - 1) * pix_size;
            if (nsamps != 0) {
               zvread(inp, to, "LINE",line,"SAMP",samp,"NSAMPS",nsamps, NULL);
            }
            ss_oi = es_i + 1;
	 }
         ss_oi -= w_pixels;
         es_oi -= w_pixels;
         if (es_oi >= ss_i && es_oi < es_i) {
            if (ss_oi < ss_i) ss_oi = ss_i;
            samp = ss_oi;
            nsamps = es_oi - ss_oi + 1;
            to = outbuffer + (es_o - nsamps) * pix_size;
            if (nsamps != 0) {
               zvread(inp, to, "LINE",line,"SAMP",samp,"NSAMPS",nsamps, NULL);
            }
	 }
      }

      else if (ss_oi < ss_i) {
         if (es_oi >= ss_i) {
            samp  = ss_i;
            nsamps = es_oi - ss_i + 1;
            to = outbuffer + (es_o - nsamps) * pix_size;
            if (nsamps != 0) {
               zvread(inp, to, "LINE",line,"SAMP",samp,"NSAMPS",nsamps, NULL);
            }
            es_oi = ss_i - 1;
         }
         ss_oi += w_pixels;
         es_oi += w_pixels;
         if (ss_oi <= es_i && ss_oi > ss_i) {
            if (es_oi > es_i) es_oi = es_i;
            samp = ss_oi;
            nsamps = es_oi - ss_oi + 1;
            to = outbuffer + (ss_o - 1) * pix_size;
            if (nsamps != 0) {
               zvread(inp, to, "LINE",line,"SAMP",samp,"NSAMPS",nsamps, NULL);
            }
	 }
      }
      zvwrit(out, outbuffer, NULL);
   }
   return;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sinproj.imake
#define PROGRAM  sinproj

#define MODULE_LIST sinproj.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create sinproj.pdf
process help=*
parm INP	TYPE=STRING
parm OUT	TYPE=STRING   DEFAULT=a
parm LON_CL     TYPE=REAL     DEFAULT=--  COUNT=0:1
parm LON_CR     TYPE=REAL     DEFAULT=--  COUNT=0:1
parm LAT_UC     TYPE=REAL     DEFAULT=--  COUNT=0:1
parm LAT_LC     TYPE=REAL     DEFAULT=--  COUNT=0:1
parm PROJ_LON	TYPE=REAL     DEFAULT=--  COUNT=0:1
parm SIZE       TYPE=INTEGER  DEFAULT=--  COUNT=0:4
parm SL  	TYPE=INTEGER  DEFAULT=--  COUNT=0:1
parm SS  	TYPE=INTEGER  DEFAULT=--  COUNT=0:1
parm NL  	TYPE=INTEGER  DEFAULT=--  COUNT=0:1
parm NS 	TYPE=INTEGER  DEFAULT=--  COUNT=0:1
parm PROJSAMP	TYPE=INTEGER  DEFAULT=--  COUNT=0:1
PARM DBUG       TYPE=KEYWORD  COUNT=0:1   DEFAULT=--    VALID=(DBUG,NODBUG)
end-proc

.title
SINPROJ
.help
PURPOSE:
SINPROJ reprojects a vicar image file of a sinusoidal projection of a given
projection longitude into an image file with a different projection longitude,
it also can produce an output file with the same projection longitude but of a
different size, i.e. sinproj will not change the pixel size of the image but
will change the width of the image by only reprojecting a fraction of the 
original image.
SINPROJ expects the projection longitude (PROJ_LON),  the starting (LON_CL) and
ending (LON_CR) longitudes, and the starting (LAT_UC) and ending (LAT_LC)
latitudes  for the output file to be specified, although all these values can
be defaulted or specified by line and sample instead, with SINPROJ converting
them from one to another internally. The input image file should also have
these keys in its vicar label because they are needed to recalculate the new
projection and are are used as the default values if none are specified in the
parameters.
.page
When SINPROJ is run with LON_CL and LON_CR specifying the size of the output
image and PROJ_LON not specified the projection longitude will default to the
longitude midway between LON_CL and LON_CR; similarly if only SS and NS are
specified PROJSAMP will be set to SS + NS/2. SINPROJ cannot reproject an image
into an oblique sinusoidal projection. If LAT_UC and LAT_LC are specified to be
smaller in the output image than they are in original image, the output image
will simply cut off at those latitudes. The equator is always taken to be the
latitude from which the projection is calculated.
.page
examples
if the input image file has in its label:
                            PROJ_LON = 180.0
                            LON_CL = 0.0  LON_CR = 360.0
                            LAT_UC = 90.0 LAT_LC = -90.0

SINPROJ inp out PROJ_LON=0
   output image is very lopsided with all of the image to the right
   of the projection longitude (you probably don't want this) and
   contains all of the input image
SINPROJ inp out PROJ_LON=0 LON_CL=180 LON_CR = 179
   output image is centered around the projection longitude      
SINPROJ inp out  LON_CL=180 LON_CR = 220
   output image is centered on a projection longitude of 200
   and is only 1/9th as wide as the input image
SINPROJ inp out  SS = 360 NS = 80
   this will do the same as the previous example if there are
   two samples per degree
.page
HISTORY

Original Author: ??? 
Cognizant Programmer: m. jentoft-nilsen

history:
    Mar 6, 1992:  MJ-N - mcr 1290 allow projections with lon_cl
                  or lon_cr outside of original longitude range of an
                  image
    22 Feb 1990 - FR 52797 mj-n
    02 Jan 1995   Made portable for UNIX ... J. Turner (CRI) 


.level1
.variable INP
input image file
.variable OUT
output image file
.variable LON_CL
output left longitude
.variable LON_CR
output right longitude
.variable LAT_UC
output upper latitude
.variable LAT_LC
output lower latitude
.variable PROJ_LON
output projection longitude
.variable SIZE
standard vicar size field
.variable SL
starting line
.variable SS
starting sample
.variable NL
number of lines
.variable NS
number of samples
.variable PROJSAMP
sample number of the 
output projection longitude
in input file sample
numbering
.level2
.variable INP
input image file, it needs to contain the label items: proj_lon, lon_cl,
lon_cr, lat_uc, and lat_lc for sinproj to work
.variable OUT
output image file
.variable LON_CL
the longitude of the leftmost pixel at the equator for the output
file. if not specified and if the size field is not specified the
default is the input file's value. should be between 0.0 and 360.0
.variable LON_CR
the longitude of the rightmost pixel at the equator for the output
file if not specified and if the size field is not specified the
default is the input file's value. LON_CR is expected to be greater
than LON_CL but less than 360.0, if it isn't then SINPROJ interprets
it as a wraparound, e.g. specify a reprojection from 320.0 to 20.0
with a projection longitude of 350.0 as
  LON_CL = 320.0 LON_CR = 20.0 PROJ_LON = 350.
.variable LAT_UC
the latitude of the northernmost pixel at the projection longitude
for the output file. should be between 90.0 and -90.0
.variable LAT_LC
the latitude of the southernmost pixel at the projection longitude
for the output file. should be between 90.0 and -90.0 and should be 
less than LAT_UC
.variable PROJ_LON
the projection longitude of the sinusoidal projection for the output
file, it usually passes through the center of the image frame. PROJ_LON
defaults to the longitude halfway between LON_CL and LON_CR. If samples
are specified instead of longitudes PROJ_LON is calculated from PROJSAMP
.variable SL
starting line for the output file, corresponds to LAT_UC. 
.variable SS
starting sample for the output file, corresponds to LON_CL
.variable NL
number of lines for the output file
.variable NS
number of samples for the output file
.variable PROJSAMP
sample number of the output projection longitude for the output file 
it needs to be specified in terms of the input file sample numbering.
ie if the input image is 2000 samples wide and the output image is going
to be 500 samples wide and centered on the input image :

   SINPROJ inp=tst.in out=tst.out ss=750 ns=500  
   SINPROJ inp=tst.in out=tst.out ss=750 ns=500  projsamp=1000

will both give the same result

   SINPROJ inp=tst.in out=tst.out ss=750 ns=500  projsamp=250

will not work
If not specified it is calculated from PROJ_LON
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstsinproj.pdf
!------------------------------------------------------------------------------
!
!      TSTSINPROJ.PDF
!------------------------------------------------------------------------------
procedure
parm data string default = gendata  valid=(gendata,nodata)
refgbl $syschar  !! Glogal variable for VAX_VMS or UNIX test
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="no"
write " The Test Data are handeled for both VMS and UNIX in this PDF."
write " At present (October 1994), in order to run this program, the"
write " following data files MUST be created on the VAX-VMS system"
write " and then copied to the local directory on the UNIX system "
write " where the program resides.  The switch GENDATA must be set"
write " when running on the VAX-VMS system to generate the data. On" 
write " the UNIX system, the NODATA switch musr be set. The program"
write " BIN-INIT which generates the test files has not been ported "
write " for the UNIX system."
write " "
write " "
write " terr.out  "
write " wei.out"
write " "
write " "
let $echo="yes"
if (data = "gendata")
!---------------------------------------------------------------------------
let $echo="no"
write " Generation of GEN terrain"
let $echo="yes"
IF ($syschar(1) = "UNIX")
   bin-init out=( terrain, weight )  +
         filetype=gtei  oformat=half  +
         lat_uc=90.0  lat_lc=-90.0  +
         nl=128   ns=256   +
         proj_lon=180.0  projsamp=128  

   gen out=gen8x16.out +
      nl=8     ns=16  format=half+
      linc=1.0    sinc=1.0     binc=0.0     ival=1.0

   size inp=gen8x16.out out=size.out noin=noin zoom=16 

   label-switch inp=(terrain, size.out) +
                out=terr.out

   ush rm gen8x16.out
   ush rm size.out
   ush rm terrain

   gen out=gen8x16.out +
       nl=8     ns=16  format=real+
       linc=1.0    sinc=1.0     binc=0.0     ival=1.0

   size inp=gen8x16.out out=size.out noin=noin zoom=16 

   label-switch inp=(weight, size.out) +
                out=wei.out

   ush rm gen8x16.out
   ush rm size.out
   ush rm weight
!
ELSE !! Must be VAX_VMS
!   
   bin-init out=( v2$scratch:terrain, v2$scratch:weight )  +
         filetype=gtei  oformat=half  +
         lat_uc=90.0  lat_lc=-90.0  +
         nl=128   ns=256   +
         proj_lon=180.0  projsamp=128  

   gen out=v2$scratch:gen8x16.out +
      nl=8     ns=16  format=half+
      linc=1.0    sinc=1.0     binc=0.0     ival=1.0

   size inp=v2$scratch:gen8x16.out out=v2$scratch:size.out noin=noin zoom=16 

   label-switch inp=(v2$scratch:terrain, v2$scratch:size.out) +
                out=v2$scratch:terr.out

   dcl delete v2$scratch:gen8x16.out;*
   dcl delete v2$scratch:size.out;*
   dcl delete v2$scratch:terrain.*;*

   gen out=v2$scratch:gen8x16.out +
       nl=8     ns=16  format=real+
       linc=1.0    sinc=1.0     binc=0.0     ival=1.0

   size inp=v2$scratch:gen8x16.out out=v2$scratch:size.out noin=noin zoom=16

   label-switch inp=(v2$scratch:weight, v2$scratch:size.out) +
                out=v2$scratch:wei.out

   dcl delete v2$scratch:gen8x16.out;*
   dcl delete v2$scratch:size.out;*
   dcl delete v2$scratch:weight.*;*
!
END-IF !! End Test for VAX_VMS or UNIX for data initialization
!
   let _onfail="continue"
let $echo="no"
write " End generation of GEN terrain "
end-if !! End generation of GEN terrain
let $echo="no"
!------------------------------------------------------------------------------
write " Begin SINPROJ test PDF"
write " Test that default reprojecion of whole image works ... display tsta."
write " - it should look the same as terrain"
let $echo="yes"
let _onfail="continue"


IF ($syschar(1) = "UNIX")

   label-l terr.out
   sinproj inp = terr.out out = tsta.out 
   label-l tsta.out

   !----------------------------------------------------------------------------
   let $echo="no"

   write " Test that default new projsamp and proj_lon are correct ... "
   write " projsamp should be 128 and proj_lon should be about 180.0"
   let $echo="yes"

   sinproj inp = terr.out out = tstb.out  +
           ss = 75 ns = 100 
   label-l tstb.out

   !----------------------------------------------------------------------------
   let $echo="no"
   write " This should produce an image similar to tstb.out,"
   write " there may be small differences if lon_cl, lon_cr, proj_lon, "
   write " ss, ns, and projsamp are compared between the two images because "
   write " tstb.out is specified by sample and tstc.out"
   write " is specified by longitude"
   let $echo="yes"

   sinproj inp = terr.out out = tstc.out  +
           lon_cl = 45  lon_cr = 90 
   label-l tstc.out

   sinproj inp = terr.out out = tstc2.out  +
           lon_cl = 90  lon_cr = 135 
   label-l tstc.out

   !----------------------------------------------------------------------------
   let $echo="no"
   write " Some other tests"
   let $echo="yes"

   sinproj inp = tstc.out out = tstd.out  +
                 sl = 25 nl = 100 
   label-l tstd.out
   
   sinproj inp = tstc.out out = tste.out  +
                 lat_uc=45 lat_lc=15 
   label-l tste.out

   sinproj inp = tstc.out out = tstg.out projsamp=15 
   label-l tstg.out

   sinproj inp = tstc.out out = tsth.out proj_lon=80 
   label-l tsth.out

   !----------------------------------------------------------------------------
   let $echo="no"
   write " Test that some of the error messages work"
   let $echo="yes"

   sinproj inp = tstc.out out = tsth.out +
                 proj_lon=150 
   sinproj inp = terr.out out = tstx.out +
                 ss=50 lon_cl=45 
   sinproj inp = terr.out out = tstx.out +
                 ns=100 lon_cr=90 
   sinproj inp = terr.out out = tstx.out +
                 sl=50 lat_uc=45 
   sinproj inp = terr.out out = tstx.out +
                 nl=50 lat_lc=0 
   sinproj inp = terr.out out = tstx.out ss=0 
   sinproj inp = terr.out out = tstx.out projsamp=0 
   sinproj inp = terr.out out = tstx.out projsamp=-10 

   ush rm tsta.out
   ush rm tstb.out
   ush rm tstc.out
   ush rm tstc2.out
   ush rm tstd.out
   ush rm tste.out
   ush rm tstg.out
   ush rm tsth.out
!
ELSE !! Else must be VAX_VMS
!   
   label-l v2$scratch:terr.out
   sinproj inp = v2$scratch:terr.out out = v2$scratch:tsta.out 
   label-l v2$scratch:tsta.out

   !----------------------------------------------------------------------------
   let $echo="no"

   write " Test that default new projsamp and proj_lon are correct ... "
   write " projsamp should be 128 and proj_lon should be about 180.0"
   let $echo="yes"

   sinproj inp = v2$scratch:terr.out out = v2$scratch:tstb.out  +
           ss = 75 ns = 100 
   label-l v2$scratch:tstb.out

   !----------------------------------------------------------------------------
   let $echo="no"
   write " This should produce an image similar to v2$scratch:tstb.out,"
   write " there may be small differences if lon_cl, lon_cr, proj_lon, "
   write " ss, ns, and projsamp are compared between the two images because "
   write " v2$scratch:tstb.out is specified by sample and v2$scratch:tstc.out"
   write " is specified by longitude"
   let $echo="yes"

   sinproj inp = v2$scratch:terr.out out = v2$scratch:tstc.out  +
           lon_cl = 45  lon_cr = 90 
   label-l v2$scratch:tstc.out

   sinproj inp = v2$scratch:terr.out out = v2$scratch:tstc2.out  +
           lon_cl = 90  lon_cr = 135 
   label-l v2$scratch:tstc.out

   !----------------------------------------------------------------------------
   let $echo="no"
   write " Some other tests"
   let $echo="yes"

   sinproj inp = v2$scratch:tstc.out out = v2$scratch:tstd.out  +
                 sl = 25 nl = 100 
   label-l v2$scratch:tstd.out

   sinproj inp = v2$scratch:tstc.out out = v2$scratch:tste.out  +
                 lat_uc=45 lat_lc=15 
   label-l v2$scratch:tste.out

   sinproj inp = v2$scratch:tstc.out out = v2$scratch:tstg.out +
           projsamp=15 
   label-l v2$scratch:tstg.out

   sinproj inp = v2$scratch:tstc.out out = v2$scratch:tsth.out +
           proj_lon=80 
   label-l v2$scratch:tsth.out

   !----------------------------------------------------------------------------
   let $echo="no"
   write " Test that some of the error messages work"
   let $echo="yes"

   sinproj inp = v2$scratch:tstc.out out = v2$scratch:tsth.out +
                 proj_lon=150 
   sinproj inp = v2$scratch:terr.out out = v2$scratch:tstx.out +
                 ss=50 lon_cl=45 
   sinproj inp = v2$scratch:terr.out out = v2$scratch:tstx.out +
                 ns=100 lon_cr=90 
   sinproj inp = v2$scratch:terr.out out = v2$scratch:tstx.out +
                 sl=50 lat_uc=45 
   sinproj inp = v2$scratch:terr.out out = v2$scratch:tstx.out +
                 nl=50 lat_lc=0 
   sinproj inp = v2$scratch:terr.out out = v2$scratch:tstx.out +
                 ss=0 
   sinproj inp = v2$scratch:terr.out out = v2$scratch:tstx.out +
                 projsamp=0 
   sinproj inp = v2$scratch:terr.out out = v2$scratch:tstx.out +
                 projsamp=-10 

   dcl delete v2$scratch:tsta.out;*
   dcl delete v2$scratch:tstb.out;*
   dcl delete v2$scratch:tstc.out;*
   dcl delete v2$scratch:tstc2.out;*
   dcl delete v2$scratch:tstd.out;*
   dcl delete v2$scratch:tste.out;*
   dcl delete v2$scratch:tstg.out;*
   dcl delete v2$scratch:tsth.out;*
!
END-IF !! Test for VAX_VMS or UNIX
!
let $echo="no"
write " End SINPROJ test PDF"
end-proc
$ Return
$!#############################################################################
