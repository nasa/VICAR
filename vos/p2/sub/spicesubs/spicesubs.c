/******************************************************************************
  Original programmer: Sheila Tews, 17 Aug 89
  Cognizant programmer: Gary Yagi, 7 Nov 90
  Ported to UNIX: Thuy Truong, 17 Nov 93
  Revisions:  -TLT-  06/27/94  Ported to ALPHA-VMS: added length variable in
                               massage_string.
                                                   
  SPICESUBS contains common subroutines which provide basic functions
  helpful in using NAIF SPICE. The following subroutines are provided:

    get_body_ids, get_body,
    eul2mat, chgvec, chgmat,
    combine_scet, split_scet,
    get_latlon,
    load_spice,
    massage_string,
    summarize_spk, summarize_ck,
    get_basics, get_kdb

******************************************************************************/
#include "xvmaininc.h"
#include <stdio.h>
#include <string.h>
#include "spiceinc.h"
#include <stdlib.h>

#define	BODY_SIZE	   36
#define	NAME_POS	   16
#define LINE_LENGTH        80
#define SCLK_BEGIN_FIELD   89      /* field positions in ascii file KERNELDB */
#define SCLK_END_FIELD    111
#define ET_BEGIN_FIELD     89
#define ET_END_FIELD      111
#define FALSE           0

FILE	                *body_file;     /* Note: "BODY_IDS" must be declared
					   as a logical name on VAX/VMS
                                           and as an environment variable
                                           on UNIX pointing to the 
					   BODY-ID file */

FILE	                *kdb_file;     /* Note: "KERNELDB" must be declared
					   as a logical name on VAX/VMS
                                           and as an environment variable
                                           on UNIX pointing to the 
					   kernel data file */


extern	kernel_db_typ	kernel_db[MAX_KERNELS];
extern	int		kernel_count;

/*
============================================================================
  GET_BODY_IDS:

  Given spacecraft and target names, returns SPICE spacecraft, target-body,
  and central-body integer identifiers (the central-body identifier is
  extracted from target-body identifier).

  NOTE:  A good idea might be to have the following two routines just load a
         table the first call and search it on subsequent calls.  Some
         programs call them TOO many times for all this file opening and
         closing.  Actually, NAIF should be providing this function.
  ==========================================================================

  WARNING:  The BODY_IDS environment variable is no longer supported by VICAR,
            so this routine should not be used.  Use subroutine PBID() instead.
            See nimscmm2.com for an example.
            11-Jan-2012 -- LWK
  ==========================================================================*/
int	get_body_ids(craft,target,ids)
  char		*craft;	  /* input spacecraft name */
  char		*target;  /* input target name */
  body_id_typ	*ids;	  /* output S/C, target, central-body, and sun IDs */
{
  int	body_id;
  int	i;
  int	body_len;
  char	*s_quote = "\'\r";
  char	body_name[BODY_SIZE];
  char	body_line[LINE_LENGTH];
  char  *env;
  char  file_name[256];
  int	found = 0;

  ids->sc = 0;
  ids->target = 0;
  ids->sol = 10;

#if UNIX_OS       /* translate BODY_IDS */
{
  env = getenv("BODY_IDS");
  if( env != NULL)
    strcpy(file_name,env);
  else {
    zvmessage("Environment name: BODY_IDS not defined"," ");
    zmabend("");}
  if ((body_file=fopen(file_name,"r")) == 0) return FAILURE;
}
#else
  if ((body_file=fopen("BODY_IDS","r")) == 0) return FAILURE;
#endif             /* end translation of BODY_IDS */

  while ((found<2) && (fgets(body_line,LINE_LENGTH,body_file) != NULL))
     {
     sscanf(body_line,"%d",&body_id);
     body_len = strcspn(&body_line[NAME_POS],s_quote);
     if (body_len == strlen(&body_line[NAME_POS])) return FAILURE;
     strncpy(body_name, &body_line[NAME_POS],body_len);

     if (!strncmp(body_name,craft,LEN_CRAFT))
        {
        found++;
        ids->sc = body_id;
        }
     if (!strncmp(body_name,target,LEN_TARGET))
        {
        found++;
        ids->target = body_id;
        if (ids->target < 10 || ids->target>9999) ids->center = ids->target;
        else ids->center = ((ids->target/100)*100) + 99;
        }
     for (i=0; i<BODY_SIZE; body_name[i++]=0);
     }

  if (found != 2) return FAILURE;
  fclose(body_file);
  return SUCCESS;
}
/*
 ==============================================================================
 GET_BODY:  Returns body name or body id given the other
 ==============================================================================
*/
int	get_body(witch,body_id,body)
  int	witch;
  int	*body_id;
  char	*body;
{
  int	id_number;
  int	i;
  int	body_len;
  char	*s_quote = "\'\r";
  char	body_name[BODY_SIZE];
  char	body_line[LINE_LENGTH];
  char  *env;
  char  file_name[256];
  int	found = FALSE;

#if UNIX_OS           /* translate BODY_IDS */
{
  env = getenv("BODY_IDS");
  if( env != NULL)
    strcpy(file_name,env);
  else {
    zvmessage("Environment name: BODY_IDS not defined"," ");
    zmabend("");}
  if ((body_file=fopen(file_name,"r")) == 0) return FAILURE;
}
#else
  if ((body_file=fopen("BODY_IDS","r")) == 0) return FAILURE;
#endif                 /* end translation of BODY_IDS */

  while (fgets(body_line,LINE_LENGTH,body_file) != NULL)
     {
     if (witch == BODY_ID)
        {
        sscanf(body_line, "%d", &id_number);
        body_len = strcspn(&body_line[NAME_POS], s_quote);
        if (body_len == strlen(&body_line[NAME_POS])) return FAILURE;
        strncpy(body_name,&body_line[NAME_POS],body_len);

        if (!strncmp(body_name,body,body_len))
           {
           *body_id = id_number;
           break;
           }
        for (i=0; i<BODY_SIZE; body_name[i++]=0);
        }
     else
        {
        sscanf(body_line,"%d",&id_number);
        if (id_number == *body_id)
           {
           body_len = strcspn(&body_line[NAME_POS], s_quote);
           if (body_len == strlen(&body_line[NAME_POS])) return FAILURE;
           strncpy(body, &body_line[NAME_POS], body_len);
           body[body_len] = 0;
           break;
           }      
        }
     }

  if (body_file == NULL) return FAILURE;
  else fclose(body_file);
  return SUCCESS;
}
/*
 ==============================================================================
   EUL2MAT:

   Converts ra, dec, and twist to matrix (delete and use NAIF routine when
   it arrives in mid-May...still haven't arrived - March 14, 1994 --TLT--).
 ==============================================================================
*/
void eul2mat(ra,dec,twist,c_matrix)
  double	ra;
  double	dec;
  double	twist;
  double	*c_matrix;
{
  double        zhalfpi();
  double	ra_halfpi,halfpi_dec;

  ra_halfpi = ra + zhalfpi();
  halfpi_dec = zhalfpi() - dec ;
 
  zrotate(ra_halfpi,           3,c_matrix);

  zrotmat(c_matrix, halfpi_dec,1, c_matrix);
  zrotmat(c_matrix, twist,     3, c_matrix);
}
/*
 =============================================================================
 CHGVEC:

 Rotates a double precision vector between two standard reference frames.
 ============================================================================*/

void chgvec (in_vector,in_ref,out_ref,out_vector)
  double	*in_vector;
  double	*out_vector;
  int		in_ref;
  int		out_ref;
{
  double	rot_matrix[9];

  zirfrot(in_ref, out_ref, rot_matrix);
  zmxv(rot_matrix, in_vector, out_vector);
}
/*
=============================================================================
   CHGMAT:

   Rotates a double precision 3*3 matrix between J2000 and B1950 (should 
   probably be replaced with a NAIF routine as soon as it is identified - 
   around mid-MAY, even though THEY don't use published precision)
=============================================================================*/

void chgmat(from,to,matrix)
  int		from;
  int		to;
  double	*matrix;
{
/* This is the matrix expressing the rotation of the coordinate
   reference frame from EME of B1950 to EME of J2000, complete to
   the published precision. */

  static double refmat[9] = { 0.9999256794956877,
                              0.0111814832391717,
                              0.0048590037723143,
                             -0.0111814832204662,
                              0.9999374848933135,
                             -0.0000271702937440,
                             -0.0048590038153592,
                             -0.0000271625947142,
                              0.9999881946023742 };

  if (from==J2000 && to==B1950)  zmxm(matrix,refmat,matrix);
  if (from==B1950 && to==J2000)  zmxmt(matrix,refmat,matrix);
}
/*
==============================================================================
  COMBINE_SCET:

  Combines scet from the six word integers (usually found in the catalog) and
  formats them into one character string necessary for Spice

      Spice format: YYYY-DDD // HH:MM:SS.MMM

=============================================================================*/

void combine_scet(year,day,hour,minute,second,milli,scet)
  short	year;
  short	day;
  short	hour;
  short	minute;
  short	second;
  short	milli;
  char	*scet;
{
  sprintf(scet, "%04d-%03d // %02d:%02d:%02d.%03d", year, day, hour, 
           minute, second, milli);
}
/*
==============================================================================
  SPLIT_SCET:

  Splits scet up from a character string (necessary for Spice) and
  formats them into the six word integers (usually found in the catalog) 

      Spice format: YYYY-DDD // HH:MM:SS.MMM

=============================================================================*/

void split_scet(scet,year,day,hour,minute,second,milli)
  char	*scet;
  short	*year;
  short	*day;
  short	*hour;
  short	*minute;
  short	*second;
  short	*milli;
{
  sscanf(scet,             " %4hd", year);
  sscanf(&scet[SCET_DAY],  " %3hd", day );
  sscanf(&scet[SCET_HOUR], " %2hd", hour);
  sscanf(&scet[SCET_MIN],  " %2hd", minute);
  sscanf(&scet[SCET_SEC],  " %2hd", second);
  sscanf(&scet[SCET_MILL], " %3hd", milli);
}
/*
=============================================================================
  GET_LATLON:

  Get latitude/longitude from position vector
=============================================================================*/

void	get_latlon(radius,from_to_bodyfixed,lon,lat)
  radii_typ	radius;
  double	from_to_bodyfixed[3];
  double	*lon;
  double	*lat;
{
  static	double	rad;

  zreclat(from_to_bodyfixed,&rad,&(*lon),&(*lat));
  *lon = 360 - (*lon * zdpr());		/* West longitude */
  if (*lon > 360) *lon -= 360;
  *lat = *lat * zdpr();
}
/*
==============================================================================
  LOAD_SPICE:

  Loads ALLthe NAIF (not MIPS) kernels we have relevant to GALILEO spacecraft.

  When operational procedures are clarified, (e.g. how many kernels
  containing what will be provided) the rudimentary file management system 
  currently implemented (get_kdb) for determining which kernels to load should
  be expanded. All is premature.
=============================================================================*/

int	load_spice(sc_id)
  int   sc_id;
{
  int	handle;
  int	i;
  int	status;
  char	filename[LEN_FILENAME+1];

  status = get_kdb();				/* Load kernel database */
  if (status != SUCCESS) return FAILURE;

  for (i=0; i<kernel_count; i++)
      {
      if (!strncmp(kernel_db[i].source,"NAIF",LEN_SOURCE))
         {
         if (kernel_db[i].type == SPK ||
             kernel_db[i].type == SK ||
             kernel_db[i].type == PK)
            {
	      zspklef(kernel_db[i].filename,&handle);
            if (zfailed()) return FAILURE;
            }
         else if (kernel_db[i].type == CK)
            {
	      zcklpf(kernel_db[i].filename,&handle);
            if (zfailed()) return FAILURE;
            }
         }
     }
  return SUCCESS;
}
/*
===========================================================================
  MASSAGE_STRING:

  Delete trailing blanks, add null terminator, and convert string to
  upper-case.
==========================================================================*/

void massage_string(out_string,in_string,in_len)
  char	*out_string;
  char	*in_string;
  int	in_len;
{
  int	i;
  int   length;
	/* Add a null terminator */
  strncpy(out_string,in_string,in_len);
  out_string[in_len] = 0;
	/* Remove trailing spaces */
  for (i=strlen(out_string)-1; out_string[i]==' '; out_string[i--]=0);
	/* Change to uppercase */
  length = strlen(out_string);
  for (i=0; i < length; i++)
	out_string[i] = toupper(out_string[i]);
}
/*
 ==============================================================================
  SUMMARIZE_SPK:

  Summarizes SPICE SP kernel
   - we need project cooperation on file identifier definition
 ============================================================================*/
#define	NUM_SPK_SUM	5

int	summarize_spk(handle,summ)
  int			handle;
  sp_summary_typ	*summ;
{
  int		num_doubles;	/* 2 expected */
  int		num_ints;	/* 6 expected*/
  int		daf_found;
  int		loop_count;
  double	summary[NUM_SPK_SUM];
  struct
  {
    double	begin_et;
    double	end_et;
  }	spk_dbls;
  struct
  {
    int		body;
    int		conter;
    int		rest[4];
  }	spk_ints;

  summ->sc_cnt = 0;
  summ->body_cnt = 0;

  zdafbfs(handle);
  zdafhsf(handle,&num_doubles,&num_ints);
  zdaffna(&daf_found);

  for (;daf_found && summ->sc_cnt < MAX_SP_SC
                   && summ->body_cnt < MAX_SP_BODY;)
      {
      zdafgs(summary);
      zdafus(summary, num_doubles, num_ints, &spk_dbls, &spk_ints);
      if (spk_ints.body < 0)
         {
         summ->sc_summ[summ->sc_cnt].spacecraft = spk_ints.body;
         summ->sc_summ[summ->sc_cnt].begin_et   = spk_dbls.begin_et;
         summ->sc_summ[summ->sc_cnt++].end_et   = spk_dbls.end_et;
         }
      else
         {
         summ->body_summ[summ->body_cnt].body     = spk_ints.body;
         summ->body_summ[summ->body_cnt].begin_et = spk_dbls.begin_et;
         summ->body_summ[summ->body_cnt++].end_et = spk_dbls.end_et;
      }

    zdaffna(&daf_found);
  }

	/* time to do something with the spacecrafts */
  if (daf_found != SUCCESS) return FAILURE;
  return SUCCESS;
}
/*
=============================================================================
  SUMMARIZE_CK:

  Summarizes SPICE C kernel
   - we need project cooperation on file identifier definition
=============================================================================*/
#define	NUM_CK_SUM   10		/* CK segment decriptor length in INT */

int	summarize_ck(handle,summ)
  int		handle;
  c_summary_typ	*summ;
{
  int		num_doubles;	/* # double-precision items in descriptor */
  int		num_ints;	/* # integer items in descriptor */
  int		sc_id;		/* Spacecraft ID = -77 for GLL */
  int		loop_count;
  int		daf_found;
  double	summary[NUM_CK_SUM];	/* segment descriptor buffer */
  struct
  {
    double	beg_encsclk;		/* beginning and ending spacecraft */
    double	end_encsclk;		/* clock (encoded) */
  }	ck_dbls;
  struct
  {
    int		ck_code;		/* instrument code = -77001 for SSI */ 
    int		rest[5];
  }	ck_ints;

  zdafbfs(handle);
  zdafhsf(handle, &num_doubles, &num_ints);
  zdaffna(&daf_found);

  for (summ->count=0; daf_found && summ->count<MAX_C_SUMM; summ->count++)
  {
	/* Get CK segment descriptor and unpack */
    zdafgs(summary);
    zdafus(summary, num_doubles, num_ints, &ck_dbls, &ck_ints);
	/* Extract spacecraft and camera id from CK-code */
    sc_id = ck_ints.ck_code/1000;
    summ->point[summ->count].spacecraft = sc_id;
    summ->point[summ->count].instrument = -(ck_ints.ck_code-1000*sc_id);
	/* Convert encoded spacecraft clock to ephemeris time */
    zsct2e(sc_id,ck_dbls.beg_encsclk,&summ->point[summ->count].begin_et);
    zsct2e(sc_id,ck_dbls.end_encsclk,&summ->point[summ->count].end_et);
    zdaffna(&daf_found);
  }
  if (daf_found != SUCCESS) return FAILURE;
  return SUCCESS;
}
/*
=============================================================================
  GET_BASICS:

  Get the SPICE basics that you will need for most further calculations
============================================================================*/

int	get_basics(ids,ck_id,etime,sclk,sclk_tol,system,basics,c_matrix,c_av)
  body_id_typ	ids;
  int		ck_id;
  double	etime;
  double	sclk;
  double	sclk_tol;
  char		system[5];
  basics_typ	*basics;                /* output */
  double	*c_matrix;		/* output C-matrix */
  double	*c_av;			/* output C-angular velocity vector */
{
  char		*lighttime = "LT";
  timout_typ	timout;
  int		found;
  double	lambda;
  double	c_ra,c_dec,c_twist;

  double	ssb_craft[6];
  double	ssb_target[6];
  double	ssb_center[6];

  double	etime_lt;
  double	center_craft_lt;
  double	craft_target_lt;
  double	craft_sun_lt;
  double        zhalfpi();

        /* Get spacecraft position relative to SSB */
  zspkssb(ids.sc, etime, system, &ssb_craft);
        /* Get target body position relative to SSB */
  zspkssb(ids.target, etime, system, ssb_target);
	/* Get central body position relative to SSB */
  zspkssb(ids.center, etime, system, ssb_center);
	/* Get target body position relative to spacecraft */
  zspkapp(ids.target, etime, system, ssb_craft,
          lighttime, &basics->craft_target_pos,&craft_target_lt);
  if (zfailed()) return INSUFF_EPHEMERIS;
	/* Get spacecraft position relative to central body */
  zspkapp(ids.sc, etime, system, ssb_center,
          lighttime, &basics->center_craft_pos, &center_craft_lt);
  if (zfailed()) return INSUFF_EPHEMERIS;
	/* Get sun state vectors (pos/vel) relative to spacecraft */
  zspkapp(ids.sol, etime, system, ssb_craft,
          lighttime, &basics->craft_sun_pos, &craft_sun_lt);
  if (zfailed()) return INSUFF_EPHEMERIS;
	/* Get euler angles for ME-matrix */
  etime_lt = etime - craft_target_lt;
  zbodeul(ids.target, etime_lt, &basics->me_ra, &basics->me_dec,
            &basics->me_twist, &lambda);
  if (zfailed()) return INSUFF_EPHEMERIS;
	/* Get euler angles for C-matrix */
  zckgpav(ck_id,sclk,sclk_tol,system,c_matrix,c_av,&timout,&found);
  if (zfailed()) return INSUFF_POINTING;
  if (!found)
     {
     zckgpav(ck_id,sclk,3000.,system,c_matrix,c_av,&timout,&found);
     if (zfailed() || (!found)) return INSUFF_POINTING;
     zvmessage(" ***C-matrix may be very inaccurate");
     }  
  zm2eul(c_matrix,3,2,3,&c_twist,&c_dec,&c_ra);
  basics->c_ra = c_ra;
  basics->c_dec = zhalfpi() - c_dec;
  basics->c_twist = c_twist;
  return SUCCESS;
}
/*
=============================================================================
  GET_KDB:  Load the ASCII kernel database.
============================================================================*/

int get_kdb ()
{
  int read_stat;
  int i;
  int year_b, year_e;
  int day_b, day_e;
  char kdb_line[256];
  char month_b[5], month_e[5];
  char time_b[10], time_e[10];
  char  *env;
  char  file_name[256];
  char  kernel_path[50];
  char  real_path[256];
  char  kernel_file[132];
  char  msg[132];

#if UNIX_OS
{
  env = getenv("KERNELDB");
  if( env != NULL)
    strcpy(file_name,env);
  else {
    zvmessage("Environment name: KERNELDB not defined for kdb.gll"," ");
    zmabend("");}
  if ((kdb_file=fopen(file_name,"r")) == 0) return FAILURE;
}
#else
  if ((kdb_file=fopen("KERNELDB","r")) == 0) return FAILURE;
#endif

  kernel_count = 0;  
/* for each line in KERNELDB, read kernel data */
  for (i=0; i < MAX_KERNELS; i++)
    {
    if(fgets(kdb_line,256,kdb_file) == NULL) break;

    /* reads into kernel data buffer all but the last two fields */
    sscanf(kdb_line,"%s%d%s%s%s%d%s%d%s%d%s%d%s",kernel_db[i].id,
	   &kernel_db[i].type,
	   kernel_db[i].source,
	   kernel_path,kernel_file,
	   &year_b,month_b,&day_b,time_b,
	   &year_e,month_e,&day_e,time_e);

    /* reads last two fields according to kernel type */
    if(kernel_db[i].type == CK)           /* C kernel */ 
      {
	sscanf(kdb_line+SCLK_BEGIN_FIELD,"%s%s",
               kernel_db[i].range_begin.sclk_begin,
	       kernel_db[i].range_end.sclk_end);
      }
    else if(kernel_db[i].type == SPK)      /*SP kernel*/
      {
	sscanf(kdb_line+ET_BEGIN_FIELD,"%lE",
	       &kernel_db[i].range_begin.et_begin);
	sscanf(kdb_line+ET_END_FIELD,"%lE",
	       &kernel_db[i].range_end.et_end);
      }
    else                  /* kernel type not supported */
      {
	zvmessage("incorrect data in KERNELDB","");
	return FAILURE;
      }

    /* process directory path and filename for appropriate systems */
#if UNIX_OS
	{
	env = getenv(kernel_path);
	if( env != NULL)
	  strcpy(real_path,env);
	else {
	  sprintf(msg,"Environment name: %s not defined for path to kernel filenames",real_path);
	  zvmessage(msg,"");
	  zmabend("");
	  }
	sprintf(kernel_db[i].filename,"%s/%s",real_path,kernel_file);
	}
#else
	{
	strcpy(real_path,kernel_path);
	sprintf(kernel_db[i].filename,"%s:%s",real_path,kernel_file);
	}
#endif          /* end filename process*/
    
    /* write scet string to kernel data buffer */
    sprintf(kernel_db[i].scet_begin,"%d %s %02d %s",
	    year_b,month_b,day_b,time_b);
    sprintf(kernel_db[i].scet_end,"%d %s %02d %s",year_e,month_e,day_e,time_e);

    /* update kernel_count */
    kernel_count++;
  }

/* close the KERNELDB database */

  fclose(kdb_file);
  return SUCCESS;
}
