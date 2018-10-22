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

