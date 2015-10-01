$!****************************************************************************
$!
$! Build proc for MIPL module spicesubs
$! VPACK Version 1.8, Wednesday, June 12, 1996, 14:15:46
$!
$! Execute by entering:		$ @spicesubs
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
$ write sys$output "*** module spicesubs ***"
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
$ write sys$output "Invalid argument given to spicesubs.com file -- ", primary
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
$   if F$SEARCH("spicesubs.imake") .nes. ""
$   then
$      vimake spicesubs
$      purge spicesubs.bld
$   else
$      if F$SEARCH("spicesubs.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake spicesubs
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @spicesubs.bld "STD"
$   else
$      @spicesubs.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create spicesubs.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack spicesubs.com -
	-s spicesubs.c -
	-i spicesubs.imake -
	-t tspicesubs.c tspicesubs.imake tspicesubs.pdf tstspicesubs.pdf -
	-o spicesubs.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create spicesubs.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create spicesubs.imake
#define SUBROUTINE spicesubs

#define MODULE_LIST spicesubs.c 

#define P2_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create tspicesubs.c
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
$!-----------------------------------------------------------------------------
$ create tspicesubs.imake
/* Imake file for Test of VICAR subroutine spicesubs */

#define PROGRAM tspicesubs

#define MODULE_LIST tspicesubs.c

#define MAIN_LANG_C
#define TEST

#define USES_ANSI_C

#define LIB_SPICE
#define LIB_RTL
#define LIB_FORTRAN
#define LIB_TAE
#define LIB_P2SUB
/* #define LIB_LOCAL must be removed before delivery, but used during */
/* developer testing on VAX and SUN.                                  */ 
/*#define LIB_LOCAL*/
$!-----------------------------------------------------------------------------
$ create tspicesubs.pdf
PROCESS
PARM SP_KERNEL		STRING 	COUNT=1
PARM CK_KERNEL		STRING 	COUNT=1

END-PROC
$!-----------------------------------------------------------------------------
$ create tstspicesubs.pdf
!****************************************************************************
! TSTSPICESUBS.PDF, unit test procedure for subroutine SPICESUBS.F
!
!   Testers: please read the unit test for information!
!   This pdf assumes that if you are not on a unix host, then you are on
!   a vms host.
!
! Revision History:
!   June  27, 1994  T. Truong  revised NOTE TO TESTER
!   March 21, 1994  T. Truong  PORTED TO UNIX
!*****************************************************************************
procedure help=*
refgbl $echo
body
let _onfail="continue"
let $echo="no"
write "**************************************"
write "		NOTE TO TESTER:"
write "**************************************"
Write  " "
Write  " The Following Test Data are handled separately for VMS and UNIX. "
Write  " Currently for the UNIX, in order to run the program, the data"
Write  " files MUST be copied from $SPICEKER/* to the LOCAL directory "
Write  " where the program resides."
write " "
refgbl $syschar
local bsp       type=string             !...gll_long_2.bsp
local ssi_ck    type=string             !...pred_ev6.ssi_ck

if ($syschar(1) = "UNIX")
  let bsp = "gll_long_2.bsp"
  let ssi_ck = "pred_ev6.ssi_ck"

else ! VMS format
  let bsp = "SPICEKER:gll_long_2.bsp"
  let ssi_ck = "SPICEKER:pred_ev6.ssi_ck"
end-if
let  $echo="yes"
!
! BEGIN testing
!

tspicesubs +
	SP_KERNEL=@bsp +
	CK_KERNEL=@ssi_ck

end-proc

$ Return
$!#############################################################################
$Other_File:
$ create spicesubs.hlp
1 spicesubs

  SPICESUBS contains common subroutines which provide basic functions
  helpful in using NAIF SPICE. The following subroutines are provided:

         get_body_ids           - returns s/c, target, and central body
                                  integer identifiers given s/c and
                                  target names

         get_body               - returns body name or body identifier
                                  given the other

         eul2mat                - converts RA, DEC, and TWIST to matrix
                                  
         chgvec                 - rotates a double precision vector
                                  between two reference frames

         chgmat                 - rotates a double precision 3*3 matrix
                                  from between J2000 and B1950 reference
                                  frames

         combine_scet           - combines scet from 6 short integers
                                  used by catalog to format used by SPICE

         split_scet             - splits scet from format used by SPICE
                                  to six short integers used by catalog

         get_latlon             - returns latitude and longitude given
                                  body radius and vector in bodyfixed
                                  coordinates

         load_spice             - loads current kernels

         massage_string         - reformats string adding null terminator
                                  and removing trailing spaces

         summarize_spk          - creates structure containing summary
                                  of an SP kernel

         summarize_spk          - creates structure containing summary
                                  of a C kernel

         get_basics             - gets the basics from SPICE

         get_kdb                - loads kernel database

2 get_body_ids

  Purpose:   This subroutine returns spacecraft, target, and central
             body integer identifiers given spacecraft and target names.

  C Calling sequence:   #include "spices89.h"
                      integer status;
                      char    		craft[LEN_CRAFT+1];
                      char     		target[LEN_TARGET+1];
                      body_id_typ	ids;

                      status = get_body_ids(craft, target, &ids);

  Input:    craft  - null terminated ascii string
            target - null terminated ascii string 

  Output:   ids:
              sc_id      - NAIF/SPICE integer identifier for spacecraft
              target_id  - NAIF/SPICE integer identifier for target
              center_id  - NAIF/SPICE integer identifier for central body
              sunid_id   - NAIF/SPICE integer identifier for the sun

  Status returns:   1 = all is OK
                    0 = invalid spacecraft or target name or
                        file containing all is inaccessible

2 get_body

  Purpose:   This subroutine returns the body name or NAIF/SPICE 
             integer identifier for a body, given the other.

  C Calling sequence:   #include "spices89.h"
                      int      status;
                      char     target[LEN_TARGET+1];
                      int      target_id;

                      status = get_body(BODY_ID, &target_id, target);

  Input:    int       - set to BODY_ID if you want the identifier
                         or to BODY_NAME if you want the ascii name

            target     - null terminated ascii string 
              or
            target_id  - target integer identifier

  Output:   target     - null terminated ascii string 
              or
            target_id  - target integer identifier

  Status returns:   1 = all is OK
                    0 = invalid body name or body identifier or
                        file containing all is inaccessible

2 eul2mat

  Purpose:   This subroutine converts input right ascension, declination,
             and twist into a matrix.

  C Calling sequence:   #include "spices89.h"
                      	double   ra;
                      	double   dec;
                      	double   twist;
                      	double   c_matrix[9];

                      	zeul2mat(ra, dec, twist, &c_matrix);

  Input:    ra    - right ascension
            dec   - declination
            twist - twist angle

  Output:   c_matrix - output C matrix

  Status returns:   none

2 chgvec

  Purpose:   Rotates a double precision vector between two
             reference frames.

  C Calling sequence:	#include "spices89.h"
                     	double   in_vec[3];
                     	double   out_vec[3];
                     	int      in_ref;
                     	int      out_ref;

                     	chgvec (in_vec, in_ref, out_ref, &out_vec);

  Input:     in_vec  -  vector to rotate
             in_ref  - reference frame of vector to rotate
             out_ref - reference frame of rotated vector

  Output:    out_vec - rotated vector

  Status returns:   none

2 chgmat

  Purpose:   Rotates a double precision 3*3 matrix between J2000 and
             EME1950 reference frames.
  C Calling sequence:  	#include "spices89.h"
			int to, from;
                     	double   matrix[9];

                     	chgmat (from, to, &matrix);

  Input:     from - 	reference frame you have
             to   - 	reference frame desired
	            	matrix in J2000 reference frame

  Output:    matrix in B1950 reference frame

  Status returns:   none

2 combine_scet

  Purpose:   This subroutine combines the scet from six input short
             integers found in the catalog into a format acceptable 
             to NAIF/SPICE routines.

  C Calling sequence:	#include "spices89.h"
                      	short    year;
                      	short    day;
                      	short    hour;
                      	short    minute;
                      	short    second;
                      	short    milli;
                      	char     scet[LEN_SCET+1];

                      	combine_scet(year, day, hour, minute, second,
                        	milli, scet);

  Input:    short integers of year, day, hour, minute, second, millisec
            of scet found in catalog

  Output:   scet - in format acceptable to NAIF/SPICE

  Status returns:   none

2 split_scet

  Purpose:   This subroutine splits the scet in a format acceptable 
             to NAIF/SPICE routines into six input short integers found
             in the catalog.

  C Calling sequence:	#include "spices89.h"
                      	short    year;
                      	short    day;
                      	short    hour;
                      	short    minute;
                      	short    second;
                      	short    milli;
                      	char     scet[LEN_SCET+1];

                      	split_scet(scet, &year, &day, &hour, &minute, 
                        	&second, &milli);

  Input:    scet - in format acceptable to NAIF/SPICE

  Output:   short integers of year, day, hour, minute, second, millisec
            of scet found in catalog

  Status returns:   none

2 get_latlon

  Purpose:   This subroutine returns the planetocentric latitiude and 
             west longitude of the intersection of an input vector in 
             bodyfixed coordinates with the body.

  C Calling sequence:	#include "spices89.h"
                      	radii_typ radius;
                      	double	from_to_bodyfixed[3];
                      	double	lat;
                      	double	lon;

                      	get_latlon(radius, from_to_bodyfixed,
                        	&lat, &lon);

  Input:    radius - semi major, semi minor, and polar radii of body
            from_to_bodyfixed - vector

  Output:   lat - planetocentric latitude of intersection
            lon - west longitude of intersection

  Status returns:   none

2 load_spice

  Purpose:   This subroutine loads available kernels.

  C Calling sequence:	#include "spices89.h"
                      	int status;

                      	status = load_spice();

  Input:    spacecraft id - NAIF's own version of spacecraft number

  Status returns:   1 = all is OK
                    0 = SPICE routine failure

2 massage_string

  Purpose:   This subroutine reformats an input string by adding
             a null terminator, removing trailing spaces and changing
             text to uppercase.

  C Calling sequence:	#include "spices89.h"
                      	char     in_target[LEN_TARGET+1];
                      	char     target[LEN_TARGET+1];

                      	massage_string(target, in_target, LEN_TARGET);

  Input:    in_target  - character string of fortran or catalog origin
                         which is missing some important attributes

            LEN_TARGET - length of in_target

  Output:   target     - null terminated, uppercase, and 'trailing spaces
                         removed' string of length 1 greater than LEN_TARGET
                         (need room for that null character)

  Status returns:   none

2 summarize_spk

  Purpose:   This subroutine loops through an open (with dafopn) SPK
             kernel extracting the body and/or spacecraft name along
             with beginning ephemeris time and ending ephemeris time
             for each segment.

  C Calling sequence:	#include "spices89.h"
                      	int      handle;
                      	spk_summary_typ	summ;

                      	summarize_spk(handle, &summ);

  Input:    handle    - handle of kernel file returned by DAFOPN
                        call

  Output:   summ      - a structure containing spacecraft and body
                        counts as well as ephemeris time periods
                        for each body and spacecraft (see spiceinc.h)

  Status returns:   none

2 summarize_ck

  Purpose:   This subroutine loops through an open (with DAFOPN) CK
             kernel extracting the spacecraft and instrument name along
             with beginning ephemeris time and ending ephemeris time
             for each array.

  C Calling sequence:	#include "spices89.h"
                      	int      handle;
                      	ck_summary_typ	summ;

                      	summarize_ck(handle, &summ);

  Input:    handle    - handle of kernel file returned by DAFOPN
                        call

  Output:   summ      - a structure containing an array count and
                        spacecraft and instrument data for each array
                        as well as ephemeris time periods 

  Status returns:   none

2 get_basics

  Purpose:   This subroutine calculates basic vectors and angles
             which can be used for further calculations.

  C Calling sequence:	#include "spices89.h"
                      	body_id_typ     ids;
                      	int             instr;
                      	double          etime;
		      	double	      	sclk;
		      	double	      	sclk_tol;
                      	char            system;
                      	basics_typ      basics;
		      	double	      	c_matrix[9];
		      	double	      	c_av[3];

            		status = get_basics(ids, instr, etime, sclk, 
				sclk_tol, system, &basics, &c_matrix, &c_av);

  Input:    ids - body ids returned from get_body_ids
            instr - instrument id as defined in spiceinc.h
            etime - ephemeris time
	    sclk - encoded spacecraft-clock
	    sclk_tol - encoded spacecraft-clock tolerance
            system - system of interest

  Output:   basics - a structure containing vectors necessary
                     and angles for further calculations
	    c_matrix - camera pointing matrix
	    c_av - camera angular velocity vector

  Status returns:   INSUFF_EPHEMERIS
                    INSUFF_POINTING
                    SUCCESS

2 get_kdb

  Purpose:   This subroutine retrieves data from kernel database KERNELDB
             to a kernel buffer.

  C Calling sequence:	#include "spices89.h"
                      	extern	kernel_db_typ	kernel_db[MAX_KERNELS];
                      	extern	int		kernel_count;

                      	status = get_kdb();

  Input:    none

  Output:   kernel_db    - a structure containing currently defined data
                           about available SPICE kernels

            kernel_count - the number of kernels in kernel_db

  Status returns:   1 = all is OK
                    0 = incorrect data in kernel database

2 History

Original programmer: Sheila Tews, 17 Aug 89
Cognizant programmer: Gary Yagi, 7 Nov 90
Source language:     C
Revisions:
  27 Jun 94  T.Truong Ported to ALPHA-VMS:
                      comment out vwait
                      changed #define USES_C to #define USES_ANSI_C
                      added length variable in massage_string
  18 Nov 93  T.Truong Ported to UNIX:
		      removed descr1,descr2,descr3,init_spice.
		      rewrote get_kdb from fortran to c.
  01 Apr 92  G.Yagi   Modify GET_BODY_IDS for GASPRA
  07 Nov 90  G.Yagi   Modify for changes in C-kernel format and Sept
		      NAIF SPICE delivery.  Changed test file to test
		      Galileo rather than Voyager (because of the
		      C-kernel changes, GET_BASICS no longer works
		      on the obsolete Voyager C-kernels).
  08 Nov 90  G.Yagi   Restored basics data structure to original form
			(since it is used to define catalog record).
  10 Nov 90  G.Yagi   Modified entry point SUMMARIZE_CK for new C-kernel
		      format.

  12 Jun 96  S. Le    Made the following changes to "tspicesubs.c" to
		      get it to compile correctly:
			1/ main44() ---changed to---> void main44()
			2/ zreset() ---changed to---> void zreset1()
$ Return
$!#############################################################################
