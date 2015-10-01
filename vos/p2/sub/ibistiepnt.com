$!****************************************************************************
$!
$! Build proc for MIPL module ibistiepnt
$! VPACK Version 1.9, Monday, December 07, 2009, 16:23:20
$!
$! Execute by entering:		$ @ibistiepnt
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
$ write sys$output "*** module ibistiepnt ***"
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
$ write sys$output "Invalid argument given to ibistiepnt.com file -- ", primary
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
$   if F$SEARCH("ibistiepnt.imake") .nes. ""
$   then
$      vimake ibistiepnt
$      purge ibistiepnt.bld
$   else
$      if F$SEARCH("ibistiepnt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibistiepnt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibistiepnt.bld "STD"
$   else
$      @ibistiepnt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibistiepnt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibistiepnt.com -mixed -
	-s ibistiepnt.c -
	-i ibistiepnt.imake -
	-o ibistiepnt.hlp -
	-t tibistiepnt.pdf tibistiepnt.imake tibistiepnt.c
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibistiepnt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 ibistiepnt.c  
 
  Routines for I/O to IBIS tiepoint files containing general qualifiers and 
  image point qualifiers.

 Revision History:
  Feb-14-97  S. Pohorsky    Original version

 Changes:
  20-Jun-97 by F. Wewel, DLR  
  09-Dec-97 by F. Wewel, DLR  
  
  - tpibis() version of hwibis() (by Dr. T. Cook, DLR)

 References:   Mars 94/96 image point collection program design document,
               Juergen Oberst, DLR, 31-Oct-1996, Appendix 3
               
*/
#include <stdio.h>

#include "ibistiepnt.h"
#include "file_no_path.h"
#include "zvproto.h"
#include <string.h>

#define MAXCOL 600  /* Defines maximum No. of columns expected */
#define MAXSTR 100  /* Maximum No. of characters in temp. string */

#define  LINESIZ 132

static char pbuf[LINESIZ+1];

#define MAX_TFILES 20           /*  Max number of tiepoint files at any time */
#define MAX_gq     100       	/*  max number of general qualifiers  */
#define MAX_iqcols 500   	/*  max num. of image qualifier columns  */
				/*  Nimages*Niqual <= MAX_iqcols  */

static int ga_ncols,gi_ncols, gr_ncols, ia_ncols, ii_ncols, ir_ncols;
static int record1,record2,record3,record4,record5,record6,record7,record8;
static int nimages, ibisn;
 
static int max_index  = -1;  /* Highest index encountered for tfiles array.  */
static int curr_index = -1;  /* tfiles array index of current tiepoint file. */
 

/*  The following typedef shows the data kept for each tiepoint file.  */
typedef struct {int  unit;  
                int  opened;
		int  ga_ncols; 
		int  gi_ncols; 
		int  gr_ncols; 
		int  ia_ncols; 
		int  ii_ncols; 
		int  ir_ncols;
		int  record1; 
		int  record2; 
		int  record3; 
		int  record4; 
		int  record5; 
		int  record6; 
		int  record7; 
		int  record8;
		int  nimages; 
		int  ibisn;
           	}  tdata;

static tdata tfiles[MAX_TFILES]; /*  space for data for up to MAX files.  */

#define MAX(x,y) (((x)<(y)) ? (y) : (x))



/*  ---------------  */

/*  declarations (prototypes) for internal routines  */


static int  zitiepnt_records (int       ibis,
                              int       images, 
                              int       ngqual, 
                              int       niqual, 
                              char      gqualfmt[][IFMT_SIZE], 
                              char      iqualfmt[][IFMT_SIZE],
                              int      *gincols,
                              int      *grncols,
                              int      *gancols,
                              int      *iincols,
                              int      *irncols,
                              int      *iancols,
                              int      *rec1,
                              int      *rec2,
                              int      *rec3,
                              int      *rec4,
                              int      *rec5,
                              int      *rec6,
                              int      *rec7,
                              int      *rec8);
                              
static int find_index (int);
static int assign_index (int);
static int read_tstruct (int);
static int write_tstruct (int);


static int tpibis (int, int *, int, int, int, 
                   char [][STRING_32], char [][IFMT_SIZE], char [][STRING_32], int, 
                   char [][STRING_32], char [][IFMT_SIZE], char [][STRING_32]);


int zitiepnt_openw ( int      unit,
                     int     *ibis,
                     int      no_imgs, 
                     char     imgsnam[][FNAMLEN],
                     int      no_genqlf, 
                     char     genqlf_nam [][STRING_32], 
                     char     genqlf_frmt[][IFMT_SIZE], 
                     char     genqlf_unit[][STRING_32], 
                     int      no_imgqlf, 
                     char     imgqlf_nam [][STRING_32], 
                     char     imgqlf_frmt[][IFMT_SIZE], 
                     char     imgqlf_unit[][STRING_32], 
                     int      no_rows)
   {
   int    status;  
   int    i;
   char   string[FNAMLEN], picnam[FNAMLEN];
   float  mp = MISSING_POINT_VALUE;
   
/* ----------------------------------------------------------------
                             initialise the IBIS - tiepoint file header */
   status = tpibis (unit, ibis, 
                    no_rows,   no_imgs,
                    no_imgqlf, imgqlf_nam, imgqlf_frmt, imgqlf_unit,
                    no_genqlf, genqlf_nam, genqlf_frmt, genqlf_unit);
   if (status != 1) return(ERR);


/*   Now put pertinent parameters in Property label  */

    status = zladd( unit, "PROPERTY","NUMBER_OF_IMAGES",&no_imgs,
                          "FORMAT", "INT", "PROPERTY","TIEPOINT",NULL);
    if (status != 1){
           zvsignal(unit, status, 0);
           return status;
    }
    status = zladd( unit, "PROPERTY","NUMBER_OF_GENERAL_QUALIFIERS",&no_genqlf,
                    "FORMAT","INT", "PROPERTY","TIEPOINT",NULL);
    if (status != 1){
           zvsignal(unit, status, 0);
           return status;
    }
    status = zladd( unit, "PROPERTY","NUMBER_OF_IMAGE_QUALIFIERS",&no_imgqlf,
                    "FORMAT","INT", "PROPERTY","TIEPOINT",NULL);
    if (status != 1){
           zvsignal(unit, status, 0);
           return status;
    }

   status = zladd(unit, "HISTORY", "IMAGES",  &no_imgs, 
                                   "FORMAT",  "INT", NULL);
   if (status != 1) return(ERR);

   for (i = 0; i < no_imgs; i++) {
      
       sprintf(string, "IMAGE_%1d_NAME", i+1);
       sprintf(picnam, "%s", imgsnam[i]);

       file_no_path(picnam);

       status = zladd(unit, "HISTORY",  string,   picnam, 
                              "FORMAT",   "STRING", NULL);
       if (status != 1) return(ERR);
       }

   status = zladd(unit, "HISTORY", "MISSING_POINT_VALUE", &mp, 
                                     "FORMAT",              "REAL", NULL);
   if (status != 1) return(ERR);


/* ----------------------------------------------------------
        define IBIS 'records' to handle variety of data types possible */

   status =  zitiepnt_records (*ibis, no_imgs, no_genqlf, no_imgqlf, 
                               genqlf_frmt, imgqlf_frmt,
                               &gi_ncols, &gr_ncols, &ga_ncols,
                               &ii_ncols, &ir_ncols, &ia_ncols,
                               &record1, &record2, &record3,
                               &record4, &record5, &record6,
                               &record7, &record8);
   if (status != 1) return(ERR);

/*  ------------------------------------------------------------
                               Save data needed for zitiepnt_write.  */    
    nimages =  no_imgs;
    ibisn   = *ibis;
    
    i = assign_index(unit);  /*  Get an available structure and store  */
    if (i < 0) return (ERR);
    
    status = write_tstruct(i);                /*  data for this file.  */
    if (status == ERR)  return (ERR);

   return(OK);
   }


/*
 zitiepnt_openr
 ===============================================================================
  Open an exisiting IBIS tiepoint file for READ
 ===============================================================================
*/

int zitiepnt_openr ( int      unit,
                     int     *ibis,
                     int     *Nimages, 
                     char     imgsnam[][FNAMLEN],
                     int     *Ngqual, 
                     char     gqual_nam [][STRING_32], 
                     char     gqual_fmt [][IFMT_SIZE], 
                     char     gqual_unit[][STRING_32], 
                     int     *Niqual, 
                     char     iqual_nam [][STRING_32], 
                     char     iqual_fmt [][IFMT_SIZE], 
                     char     iqual_unit[][STRING_32], 
                     int     *nrow)

   {
   int     i, j, k, ncol, status, index, no_unit, u_list[MAXCOL], cnt;
   char    unit_nam[MAXCOL][STRING_32], type[STRING_32];
/*  ==================================================================  */

/*    Open IBIS file for Read  */

    status = IBISFileOpen(unit, ibis, "READ", 0,0, 0,0);
    if (status != 1){
       IBISSignalU(unit, status, 0);
       return status;
    }
    status = IBISFileGet(*ibis, "NC", &ncol, 1,1, 0);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
    }
    status = IBISFileGet(*ibis, "NR", nrow, 1,1, 0);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
    }

/*   Now get pertinent parameters in Property label  */

    status = zlget( unit, "PROPERTY", "NUMBER_OF_IMAGES", Nimages,
                          "FORMAT", "INT", "PROPERTY","TIEPOINT", NULL);
    if (status != 1){
     /* --------------------------------------------------
                                                   old style (hwibis()) */
        status = zlget(unit, "HISTORY", "IMAGES", Nimages,
                             "FORMAT",  "INT", NULL);

        if (status != 1){
           zvsignal(unit, status, 0);
           return status;
           }
        else {
           *Ngqual = 1;
  /* -------------------------------------------------------- Change Aug. 97 
           *Niqual = 1  -- old --                             by F. Wewel, DLR */
           
            status = zlget( unit, "HISTORY", "IMAGE_POINT_QUALIFIERS", Niqual,
                                  "FORMAT",  "INT", NULL);
                                  
            if (status != 1) *Niqual = 0;
           }
        }
    else {
       status = zlget( unit, "PROPERTY","NUMBER_OF_GENERAL_QUALIFIERS", Ngqual,
                             "FORMAT","INT", "PROPERTY","TIEPOINT",NULL);
       if (status != 1){
           zvsignal(unit, status, 0);
           return status;
           }
       status = zlget( unit, "PROPERTY","NUMBER_OF_IMAGE_QUALIFIERS", Niqual,
                             "FORMAT","INT", "PROPERTY","TIEPOINT",NULL);
       if (status != 1){
           zvsignal(unit, status, 0);
           return status;
           }
       }
/*  Check that this file has at least the original columns. More OK.  */

    if (ncol < *Ngqual + *Nimages * ( *Niqual + 2) ){
           zvmessage("Error:  Too few columns in tiepoint file.","");
           return ERR;
    }

/*  Get the formats of the qualifiers  */

    index  = *Nimages * ( *Niqual + 2) + 1;
    status = IBISFileGet(*ibis, "FORMATS", gqual_fmt, index, *Ngqual, IFMT_SIZE);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
    }
    
    
    index  = 3;
    status = IBISFileGet(*ibis,"FORMATS",iqual_fmt, index, *Niqual, IFMT_SIZE);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
       }
 /*  Get the name of the qualifiers  */

    index  = 3;
    status = IBISFileGet(*ibis, "GROUPS", iqual_nam, index, *Niqual, STRING_32);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
       }       

/* ************************************************** Change 8.Dez.97 by F.Wewel */    
    index  = *Niqual + 3;
/* ************************************************** Change 8.Dez.97 by F.Wewel */    
    status = IBISFileGet(*ibis, "GROUPS", gqual_nam, index, *Ngqual, STRING_32);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
    }
    
    
 /*  Get the unit of the qualifiers  */

    index  = 1;
    no_unit = IBISFileGet(*ibis, "UNITS", unit_nam, index,
                                                    2+*Niqual+*Ngqual, STRING_32);
    if (no_unit < 0){
       IBISSignal(*ibis, no_unit, 0);
       return no_unit;
       }       


    for (i = 0; i < no_unit; i++) {

/* ************************************************** Change 8.Dez.97 by F.Wewel */    
        cnt = IBISColumnFind (*ibis, ITYPE_UNIT,  unit_nam[i], u_list, 1, ncol);


        if (cnt < 0){
            IBISSignal(*ibis, cnt, 0);
            return cnt;
            }       

        for (j = 0 ; j < cnt; j++) {
               
             for (k = 0; k < *Niqual; k++) {
       
                  index = k + 3;
                  if (u_list[j] == index) {
                      sprintf (iqual_unit[k], "%s", unit_nam[i]);
                      }
                  }
/* ************************************************** Change 8.Dez.97 by F.Wewel */    
             for (k = 0; k < *Ngqual; k++) {
       
                  index = *Nimages * (2 + *Niqual) + k + 1;
                  if (u_list[j] == index) {
                      sprintf (gqual_unit[k], "%s", unit_nam[i]);
                      }
                  }
             }
            
       }


/* ------------------------------------ get tne names of the image files */
   for (i = 0; i < *Nimages; i++) {
   
       sprintf(type, "IMAGE_%1d_NAME", i+1);
       status = zlget(unit, "HISTORY", type, imgsnam[i], "",NULL);
       if (status < 0) {
       
       /* ##############################
          IBISSignalU(unit , status, 0);
          return status;
          ############################### */
          
          break;
          }

       }


/*  define IBIS 'records' to handle variety of data types possible.  */

    status = zitiepnt_records(*ibis, *Nimages, *Ngqual, *Niqual, 
              gqual_fmt, iqual_fmt, 
              &gi_ncols, &gr_ncols, &ga_ncols, 
              &ii_ncols, &ir_ncols, &ia_ncols, 
              &record1, &record2, &record3, &record4, 
              &record5, &record6, &record7, &record8);
    if (status != 1){
       return status;
    }

/*  Save data needed for zitiepnt_read.  */    

    nimages = *Nimages;
    ibisn   = *ibis;
    i = assign_index(unit);    /*  Get an available structure and store  */
    if (i < 0) return ERR;
    status = write_tstruct(i);  /*  data for this file.  */
    if (status == ERR)  return status;

   return (OK);
   }
   
   
   
/*
 zitiepnt_openu
 ===============================================================================
  Open an exisiting IBIS tiepoint file for UPDATE
 ===============================================================================
*/
int zitiepnt_openu (int      unit,
                     int     *ibis,
                     int     *Nimages, 
                     char     imgsnam[][FNAMLEN],
                     int     *Ngqual, 
                     char     gqual_nam [][STRING_32], 
                     char     gqual_fmt [][IFMT_SIZE], 
                     char     gqual_unit[][STRING_32], 
                     int     *Niqual, 
                     char     iqual_nam [][STRING_32], 
                     char     iqual_fmt [][IFMT_SIZE], 
                     char     iqual_unit[][STRING_32], 
                     int     *nrow)
   {
   int i, j, k, ncol, status, index, no_unit, u_list[MAXCOL], cnt;
   char    unit_nam[MAXCOL][STRING_32], type[STRING_32];
/*  ==================================================================  */
 
/*    Open IBIS file for Update  */
 
    status = IBISFileOpen(unit, ibis, "UPDATE", 0,0, 0,0);
    if (status != 1){
       IBISSignalU(unit, status, 0);
       return status;
    }
    status = IBISFileGet(*ibis, "NC", &ncol, 1,1, 0);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
    }
    status = IBISFileGet(*ibis, "NR", nrow, 1,1, 0);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
    }
 
/*   Now get pertinent parameters in Property label  */
 
    status = zlget( unit, "PROPERTY","NUMBER_OF_IMAGES",Nimages,
                    "FORMAT","INT", "PROPERTY","TIEPOINT",NULL);
    if (status != 1){
           zvsignal(unit, status, 0);
           return status;
    }
    status = zlget( unit, "PROPERTY","NUMBER_OF_GENERAL_QUALIFIERS",Ngqual,
                    "FORMAT","INT", "PROPERTY","TIEPOINT",NULL);
    if (status != 1){
           zvsignal(unit, status, 0);
           return status;
    }
    status = zlget( unit, "PROPERTY","NUMBER_OF_IMAGE_QUALIFIERS",Niqual,
                    "FORMAT","INT", "PROPERTY","TIEPOINT",NULL);
    if (status != 1){
           zvsignal(unit, status, 0);
           return status;
    }
 
/*  Check that this file has at least the original columns. More OK.  */
 
    if (ncol < *Ngqual + *Nimages * ( *Niqual + 2) ){
           zvmessage("Error:  Too few columns in tiepoint file.","");
           return ERR;
    }
 
/*  Get the formats of the qualifiers  */
 
    index  = *Nimages * ( *Niqual + 2) + 1;
    status = IBISFileGet(*ibis, "FORMATS", gqual_fmt, index,*Ngqual, IFMT_SIZE);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
    }
    
    index = 3;
    status = IBISFileGet(*ibis,"FORMATS",iqual_fmt, index, *Niqual,IFMT_SIZE);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
    }
 
  /*  Get the name of the qualifiers  */

    index  = 3;
    status = IBISFileGet(*ibis, "GROUPS", iqual_nam, index, *Niqual, STRING_32);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
       }       

/* ************************************************** Change 8.Dez.97 by F.Wewel */    
    index  =  *Niqual + 3;
/* ************************************************** Change 8.Dez.97 by F.Wewel */    
    status = IBISFileGet(*ibis, "GROUPS", gqual_nam, index, *Ngqual, STRING_32);
    if (status < 0){
       IBISSignal(*ibis, status, 0);
       return status;
    }
    
    
 /*  Get the unit of the qualifiers  */

    index  = 1;
    no_unit = IBISFileGet(*ibis, "UNITS", unit_nam, index,
                                                    2+*Niqual+*Ngqual, STRING_32);
    if (no_unit < 0){
       IBISSignal(*ibis, no_unit, 0);
       return no_unit;
       }       


    for (i = 0; i < no_unit; i++) {

/* ************************************************** Change 8.Dez.97 by F.Wewel */    
        cnt = IBISColumnFind (*ibis, ITYPE_UNIT,  unit_nam[i], u_list, 1, ncol);

        if (cnt < 0){
            IBISSignal(*ibis, cnt, 0);
            return cnt;
            }       

        for (j = 0 ; j < cnt; j++) {
               
             for (k = 0; k < *Niqual; k++) {
       
                  index = k + 3;
                  if (u_list[j] == index) {
                      sprintf (iqual_unit[k], "%s", unit_nam[i]);
                      }
                  }
                  
/* ************************************************** Change 8.Dez.97 by F.Wewel */    
             for (k = 0; k < *Ngqual; k++) {
       
                  index = *Nimages * (2 + *Niqual) + k + 1;
                  if (u_list[j] == index) {
                      sprintf (gqual_unit[k], "%s", unit_nam[i]);
                      }
                  }
             }
       }


/* ------------------------------------ get tne names of the image files */
   for (i = 0; i < *Nimages; i++) {
   
       sprintf(type, "IMAGE_%1d_NAME", i+1);
       status = zlget(unit, "HISTORY", type, imgsnam[i], "",NULL);
       if (status < 0) {
       
       /* ##############################
          IBISSignalU(unit , status, 0);
          return status;
          ############################### */
          
          break;
          }

       }


 
 
/*  define IBIS 'records' to handle variety of data types possible.  */
 
    status = zitiepnt_records(*ibis, *Nimages, *Ngqual, *Niqual,
              gqual_fmt, iqual_fmt,
              &gi_ncols, &gr_ncols, &ga_ncols,
              &ii_ncols, &ir_ncols, &ia_ncols,
              &record1, &record2, &record3, &record4,
              &record5, &record6, &record7, &record8);
    if (status != 1){
       return status;
    }
 
/*  Save data needed for zitiepnt_read and zitiepnt_write.  */
 
    nimages = *Nimages;
    ibisn   = *ibis;
    i = assign_index(unit);    /*  Get an available structure and store  */
    if (i < 0) return ERR;
    status = write_tstruct(i);  /*  data for this file.  */
    if (status == ERR)  return status;

    return OK;
   }





/*
 zitiepnt_records
 ===============================================================================
  Internal function for zitiepnt_open routines to define up to 8 IBIS records
  according to the general and image qualifier formats.
 ===============================================================================
*/
int  zitiepnt_records (int       ibis,
                       int       images, 
                       int       ngqual, 
                       int       niqual, 
                       char      gqualfmt[][IFMT_SIZE], 
                       char      iqualfmt[][IFMT_SIZE],
                       int      *gincols,
                       int      *grncols,
                       int      *gancols,
                       int      *iincols,
                       int      *irncols,
                       int      *iancols,
                       int      *rec1,
                       int      *rec2,
                       int      *rec3,
                       int      *rec4,
                       int      *rec5,
                       int      *rec6,
                       int      *rec7,
                       int      *rec8)
  {
  char   qual_fmt[IFMT_SIZE];
  int    lcols[MAX_iqcols], scols[MAX_iqcols];
  int    ga_cols[MAX_gq], gi_cols[MAX_gq], gr_cols[MAX_gq];
  int    ia_cols[MAX_iqcols], ii_cols[MAX_iqcols], ir_cols[MAX_iqcols];
  int    i, j, k, status;
/*  ==================================================================  */

/*    Scan the qualifier formats to obtain data type information  */

    *gincols=0;        /*  Count columns of type FULL,REAL,ASCII  */
    *grncols=0;
    *gancols=0;
    for (j = 0; j < ngqual; ++j){
        strcpy(qual_fmt, gqualfmt[j]);
        
        zccase(qual_fmt, 1, IFMT_SIZE-1);	/*  convert to upper case.  */
        if (strncmp(qual_fmt,"FULL",4) == 0){
            gi_cols[*gincols] = images * (2 + niqual) + j+1;
            ++(*gincols);
            }
        else if (strncmp(qual_fmt,"REAL",4) == 0){
            gr_cols[*grncols] = images * (2 + niqual) + j+1;
            ++(*grncols);
            }
        else if (qual_fmt[0] == 'A'){
            ga_cols[*gancols] = images * (2 + niqual) + j+1;
            ++(*gancols);
            }
        else {
           sprintf( pbuf,"Error: General qualifier format %5.5s not supported.",
                    qual_fmt);
           zvmessage(pbuf,"");
           return ERR;
           }
        }
        
/*      Now on to the image qualifiers.  */
     *iincols=0;        /*  Count columns of type FULL,REAL,ASCII  */
     *irncols=0;
     *iancols=0;
     for (j = 0; j < niqual; ++j){
         strcpy( qual_fmt, iqualfmt[j]);
         zccase( qual_fmt, 1, IFMT_SIZE-1);
         if (strncmp (qual_fmt,"FULL",4) == 0){
            for (k = 0; k < images; ++k){
                 ii_cols[*iincols] = j + 3 + k*(2+niqual);
                 ++(*iincols);
                 }
            }
        else if (strncmp(qual_fmt,"REAL",4) == 0){
             for (k = 0; k < images; ++k){
                  ir_cols[*irncols] = j + 3 + k*(2+niqual);
                  ++(*irncols);
                  }
             }
        else if (qual_fmt[0] == 'A'){
             for (k = 0; k<images; ++k){
                  ia_cols[*iancols] = j + 3 + k*(2+niqual);
                  ++(*iancols);
                  }
             }
        else {
           sprintf( pbuf,"Error: Image qualifier format %5.5s not supported.",
                    qual_fmt);
           zvmessage(pbuf,"");
           return ERR;
           }
        }    /* ---------------------------------------- end of for loop  */
    
/*       Sort image qualifier arrays in ascending order  */

    if (*iincols > 0)  zsortin( ii_cols, *iincols);
    if (*irncols > 0)  zsortin( ir_cols, *irncols);
    if (*iancols > 0)  zsortin( ia_cols, *iancols);

/*    Define up to 8 records to cover all of the columns in the file
       1. all of the columns containing LINE values.
       2. all of the columns containing SAMPLE values.
       3. all of the columns containing General qualifiers of type FULL.
       4. all of the columns containing General qualifiers of type REAL.
       5. all of the columns containing General qualifiers of type ASCII.
       6. all of the columns containing Image qualifiers of type FULL.
       7. all of the columns containing Image qualifiers of type REAL.
       8. all of the columns containing Image qualifiers of type ASCII.  */

    for (j=0; j<images; ++j){     /*  columns for first two records  */
         lcols[j] = 1 + j * (2+niqual);     /*    Line for image j.  */
         scols[j] = 2 + j * (2+niqual);     /*  Sample for image j.  */
         }

    if (images > 0){
        status = IBISRecordOpen(ibis, rec1, 0, lcols,images,"REAL");
        if (status != 1){
           IBISSignal(ibis, status, 0);
           return status;
        }
        status = IBISRecordOpen(ibis, rec2, 0, scols,images,"REAL");
        if (status != 1){
           IBISSignal(ibis, status, 0);
           return status;
        }
    }
    if (*gincols > 0){
        status = IBISRecordOpen(ibis, rec3, 0, gi_cols,*gincols,"FULL");
        if (status != 1){
           IBISSignal(ibis, status, 0);
           return status;
        }
    }
    if (*grncols > 0){
        status = IBISRecordOpen(ibis, rec4, 0, gr_cols,*grncols,"REAL");
        if (status != 1){
           IBISSignal(ibis, status, 0);
           return status;
        }
    }
    if (*gancols > 0){
        status = IBISRecordOpen(ibis, rec5, 0, ga_cols,*gancols,"NONE");
        if (status != 1){
           IBISSignal(ibis, status, 0);
           return status;
        }
    }
    if (*iincols > 0){
        status = IBISRecordOpen(ibis, rec6, 0, ii_cols,*iincols,"FULL");
        if (status != 1){
           IBISSignal(ibis, status, 0);
           return status;
        }
    }
    if (*irncols > 0){
        status = IBISRecordOpen(ibis, rec7, 0, ir_cols,*irncols,"REAL");
        if (status != 1){
           IBISSignal(ibis, status, 0);
           return status;
        }
    }
    if (*iancols > 0){
        status = IBISRecordOpen(ibis, rec8, 0, ia_cols,*iancols,"NONE");
        if (status != 1){
           IBISSignal(ibis, status, 0);
           return status;
        }
    }
    return OK;
  }


/*
 zitiepnt_write
 ===============================================================================
  Write a row to an IBIS tiepoint file.
 ===============================================================================
*/
int	zitiepnt_write(
  int			unit,
  int 			irow,  
  float 		*lbuf,		/*  LINES  */
  float 		*sbuf,		/*  SAMPLES  */
  float 		*gr_qbuf, 	/*  general qualifiers, real,int,ASCII*/
  int 			*gi_qbuf, 
  char 			*ga_qbuf,
  float 		*ir_qbuf, 	/*  image qualifiers, real,int,ASCII*/
  int 			*ii_qbuf, 
  char 			*ia_qbuf)
{
 int status,index;
/*  ==================================================================  */

    index = find_index(unit);        /*  find the structure where the data */
    if (index < 0)  return ERR;
    status = read_tstruct(index);     /*  for this file is stored.  */
    if (status == ERR)  return status;

/* Each row involves up to 8 records to cover all of the columns.
       1. all of the columns containing LINE values.
       2. all of the columns containing SAMPLE values.
       3. all of the columns containing General qualifiers of type FULL.
       4. all of the columns containing General qualifiers of type REAL.
       5. all of the columns containing General qualifiers of type ASCII.
       6. all of the columns containing Image qualifiers of type FULL.
       7. all of the columns containing Image qualifiers of type REAL.
       8. all of the columns containing Image qualifiers of type ASCII.  */

    if (nimages > 0){
        status = IBISRecordWrite( record1, (char *)lbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
        status = IBISRecordWrite( record2, (char *)sbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (gi_ncols > 0){
        status =  IBISRecordWrite( record3, (char *)gi_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (gr_ncols > 0){
        status =  IBISRecordWrite( record4, (char *)gr_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (ga_ncols > 0){
        status =  IBISRecordWrite( record5, ga_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (ii_ncols > 0){
        status =  IBISRecordWrite( record6, (char *)ii_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (ir_ncols > 0){
        status =  IBISRecordWrite( record7, (char *)ir_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (ia_ncols > 0){
        status =  IBISRecordWrite( record8, ia_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    return OK;
}

/*
 zitiepnt_read
 ===============================================================================
  Read a row from an IBIS tiepoint file.
 ===============================================================================
*/
int	zitiepnt_read(
  int			unit,
  int 			irow,  
  float 		*lbuf,		/*  LINES  */
  float 		*sbuf,		/*  SAMPLES  */
  float 		*gr_qbuf, 	/*  general qualifiers, real,int,ASCII*/
  int 			*gi_qbuf, 
  char 			*ga_qbuf,
  float 		*ir_qbuf, 	/*  image qualifiers, real,int,ASCII*/
  int 			*ii_qbuf, 
  char 			*ia_qbuf)
{
 int status,index;
/*  ==================================================================  */

    index = find_index(unit);        /*  find the structure where the data */
    if (index < 0)  return ERR;
    status = read_tstruct(index);     /*  for this file is stored.  */
    if (status == ERR)  return status;

/* Each row involves up to 8 records to cover all of the columns.
       1. all of the columns containing LINE values.
       2. all of the columns containing SAMPLE values.
       3. all of the columns containing General qualifiers of type FULL.
       4. all of the columns containing General qualifiers of type REAL.
       5. all of the columns containing General qualifiers of type ASCII.
       6. all of the columns containing Image qualifiers of type FULL.
       7. all of the columns containing Image qualifiers of type REAL.
       8. all of the columns containing Image qualifiers of type ASCII.  */

    if (nimages > 0){
        status = IBISRecordRead( record1, (char *)lbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
        status = IBISRecordRead( record2, (char *)sbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (gi_ncols > 0){
        status =  IBISRecordRead( record3, (char *)gi_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (gr_ncols > 0){
        status =  IBISRecordRead( record4, (char *)gr_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (ga_ncols > 0){
        status =  IBISRecordRead( record5, ga_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (ii_ncols > 0){
        status =  IBISRecordRead( record6, (char *)ii_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (ir_ncols > 0){
        status =  IBISRecordRead( record7, (char *)ir_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    if (ia_ncols > 0){
        status =  IBISRecordRead( record8, ia_qbuf, irow);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    }
    return OK;
}

/*
 zitiepnt_close
 ===============================================================================
  Close an IBIS tiepoint file.
 ===============================================================================
*/
int	zitiepnt_close(
  int			unit)
{
  int status,i;
/*  ==================================================================  */

    i = find_index(unit);             /*  find the structure where the data */
    if (i < 0)  return ERR;
    status = read_tstruct(i);         /*  for this file is stored.  */
    if (status == ERR)  return status;

    tfiles[i].opened = 0;		/*  Mark file as closed.  */
    tfiles[i].unit   = -1;              /*  This structure now not in use.*/
    curr_index = -1;                    /*  No file is current after close.  */
    status =  IBISFileClose( ibisn, 0);
        if (status != 1){
           IBISSignal(ibisn, status, 0);
           return status;
        }
    return OK;
    }
    
/*
 ===============================================================================
  Internal routines to support having > 1 tiepoint file open.
 ===============================================================================
*/
int	find_index( int unit)  /*  find index of opened file.  */
{
  int i;
/*  ==================================================================  */
    if (max_index < 0)  return ERR;

    for ( i=0; i <= max_index; ++i) {   
        if (tfiles[i].opened && unit == tfiles[i].unit)
           return i;   /*  return index if this is the right file (unit).  */
    }
    return ERR;        /*  not found  */
}








/*  ******************************************************************  */

int assign_index (int unit)  /*  assign index when opening file.  */
   {
   int i;
/*  ==================================================================  */

    for (i = 0; i <= max_index; ++i) { 
      
         if (tfiles[i].opened && unit == tfiles[i].unit)
            return (ERR);            /*  Trying to open file twice.*/
            
         }

    for ( i=0; i < MAX_TFILES; ++i) { 
      
        if (i > max_index || tfiles[i].opened == 0){  /*  if struct not in use*/

            tfiles[i].unit = unit;
            tfiles[i].opened = 1;              /*  1 = TRUE = OPENED  */
            max_index = MAX( max_index, i );
            
            return (i);                        /* Return index that is 
                                                  assigned for this file (unit).*/
            } 
        }
    return (ERR);        /*  not free indexes  */
    }

/*
 ===============================================================================
*/
int read_tstruct (int index)  /*  read data saved for specified file */
   {
/*  ==================================================================  */
    if (tfiles[index].opened == 0)  return ERR; /*  mistake if not open  */
    if (index == curr_index) return OK;         /*  Skip if done.  */


    ga_ncols = tfiles[index].ga_ncols;
    gi_ncols = tfiles[index].gi_ncols;
    gr_ncols = tfiles[index].gr_ncols;
    ia_ncols = tfiles[index].ia_ncols;
    ii_ncols = tfiles[index].ii_ncols;
    ir_ncols = tfiles[index].ir_ncols;
    record1 = tfiles[index].record1;
    record2 = tfiles[index].record2;
    record3 = tfiles[index].record3;
    record4 = tfiles[index].record4;
    record5 = tfiles[index].record5;
    record6 = tfiles[index].record6;
    record7 = tfiles[index].record7;
    record8 = tfiles[index].record8;
    nimages = tfiles[index].nimages;
    ibisn = tfiles[index].ibisn;
    curr_index = index;
    
    return OK;
   }



/*  ******************************************************************  */
int write_tstruct( int index)  /*  write data for specified file */
   {
/*  ==================================================================  */
    if (index > max_index) {
    
        return (ERR);  /*  too big.*/
        }
        
     tfiles[index].ga_ncols = ga_ncols;
     tfiles[index].gi_ncols = gi_ncols;
     tfiles[index].gr_ncols = gr_ncols;
     tfiles[index].ia_ncols = ia_ncols;
     tfiles[index].ii_ncols = ii_ncols;
     tfiles[index].ir_ncols = ir_ncols;
     tfiles[index].record1 = record1;
     tfiles[index].record2 = record2;
     tfiles[index].record3 = record3;
     tfiles[index].record4 = record4;
     tfiles[index].record5 = record5;
     tfiles[index].record6 = record6;
     tfiles[index].record7 = record7;
     tfiles[index].record8 = record8;
     tfiles[index].nimages = nimages;
     tfiles[index].ibisn = ibisn;
     
     curr_index = index;
    
     return (OK);
     }



/********************************************************************

   Subroutine Description
   ======================
   The purpose of this routine is to open an IBIS2 file for a
   variety of point types (to be used by the Mars 96 project).
   It opens a column orientated IBIS2 file and sets up group
   names for each column and column sets. The general structure of
   this file is as follows (its effective DB fields)...

   line samp ipqlf1 ipqlf2... ipqlf`n' for IMAGE_1
   line samp ipqlf1 ipqlf2... ipqlf`n' for IMAGE_2
      "   "   "      "    ...
      "   "   "      "    ...
   line samp ipqlf1 ipqlf2... ipqlf`n' for IMAGE_`n'

   gqlf1 gqlf2            ...  gqlf`n' for GENERAL_QLF


   where there are:
   n_imgrps                    `n' image groups
   n_ipqlf                     `n' image point qualifiers
   n_gqlf                      `n' general qualifiers for the points

   Groups "IMAGE_1..IMAGE_`N'",refer to common points between
          ------------------   images, and associated information
                               pertaining to "each" image e.g.
                               line, sample, quality of match, DN
                               value etc. So these are unique
                               characteristics to each image.

   Group "OBJECT_TYPE", refers to the type of point data held
         --------------        in the IBIS file, and is set
                               internally in the routine according
                               to the following rules:
   if optype="no_op" & image groups      IBIS file type set to: "tiepoint"

   Group "GENERAL_QLF", pertains to common features between the
         -------------         the points e.g. a common point
                               between images will have the
                               same lon-lat-albedo on the
                               surface of the planet. So its
                               really a "general characteristic".

   N.B. In addition, by default, the unit types for line and samp,
   are "pixels", for X,Y,Z are "m", and Lon.Lat are "degrees".
   All other qualifiers can have their units entered via unit
   qualifier parameters - should you so desire.


   Example use:
   ============
   For example use, see the test program "ttpibis.c", which sets
   up some group names, units, formats etc, and then closes the
   file. You can check out the contents of an IBIS label by
   doing a "label-list filename" on the IBIS file.


   History:
   ========
   Subroutine specified  15th July 1994 by:

   Dr B.Giese, DLR Berlin-Adlershof,
   Institute for Planetary Exploration, 12489 Berlin, Germany.


   Subroutine  written      Aug 1994    by ACC
              Modified 25th Aug 1994    by ACC
              Modified 18th May 1995    by ACC  - Added extra comments
              Modified 29th Aug 1996    by ACC  - Define sub-groups "before"
                                                  main groups: suggested
                                                  by F.Oschutz Aug 1996
              Modified 24th Sep 1996    by ACC  - Now all object point
                                                  units are REAL - this
                                                  agrees with the specification
                                                  of 25th Nov 1994.

 
   Dr A.C.Cook, DLR Berlin-Adlershof,
   Institute for Planetary Exploration, 12489 Berlin, Germany.  
**********************************************************************/

tpibis (              /*            P-A-R-A-M-E-T-E-R-S                      */
                      /*            ===================                      */
int   vunit,          /* VICAR Unit No. - for IBIS error messages            */
int  *iunit,          /* IBIS unit No. - for IBIS error messages etc         */
int   nr,             /* No. of rows                                         */
int   n_imgrps,       /* No. of image groups                                 */
int   n_ipqlf,        /* No. of qualifiers per "image" group                 */
char  ipqlf_na[][STRING_32],       /* "Image" group qualifiers - names       */
char  ipqlf_fo[][IFMT_SIZE],       /* "Image" group qualifiers - formats      */
char  ipqlf_un[][STRING_32],       /* "Image" group qualifiers - units        */
int   n_gqlf,                      /* No. of qualifiers per "general" group   */
char  gqlf_na[][STRING_32],        /* "General" group qualifier - names       */
char  gqlf_fo[][IFMT_SIZE],        /* "General" group qualifier - formats     */
char  gqlf_un[][STRING_32])        /* "General" group qualifier - units       */

{
int nc;                             /* No. of columns in the file */
int i;                              /* Index variable */
int j;                              /* Index variable */
int count;                          /* A counter of the No. of columns set */
int col[MAXCOL];                    /* Contains column numbers for each group */
int status;                         /* Vicar status */
int ibis_unit;                      /* IBIS unit - internal use */

char format_buf[MAXCOL][IFMT_SIZE]; /* Currently assume MAXCOL columns max */
char *fmt_ptr=(char *)0;            /* Pointer to this array */
char type[MAXSTR];                  /* Temporary string variable */
char name[MAXSTR];                  /* Temporary string variable */
char number[MAXSTR];                /* Temporary string variable */
char msgbuf[80];                    /* Message buffer */




                       /************************/
                       /* Initialise variables */
                       /************************/
nc=(2+n_ipqlf)*n_imgrps + n_gqlf;        /* No. of columns */
status=0;                                    /* Status is O.K. */

if (nc>=MAXCOL)
  {
  zvmessage(msgbuf, "[tpibis] Exceeded maximum allowed number of columns");
  zabend();
  }





                    /******************************/
                    /* The formats of the columns */
                    /******************************/
/* IMAGE_1.. */     
       for (i=0;i<n_imgrps;i++) {
            strcpy(format_buf[i*(2+n_ipqlf)],  "REAL");
            strcpy(format_buf[1+i*(2+n_ipqlf)],"REAL");
                         
            for(j=0;j<n_ipqlf;j++) {
                strcpy(format_buf[j+2+i*(2+n_ipqlf)], ipqlf_fo[j]);
                }
            }

/* GENERAL_QLF */   
       for(i=0;i<n_gqlf;i++) {
           strcpy(format_buf[i + n_imgrps*(2+n_ipqlf)], gqlf_fo[i]);
           }





                       /*************************/
                       /* Opening the IBIS file */
                       /*************************/
/* Format pntr */   fmt_ptr=format_buf[0];

/* IBIS unit */     status=IBISFileOpen(vunit,&ibis_unit,IMODE_WRITE,nc,
                                        nr,fmt_ptr,IORG_COLUMN);

/* Abort if req. */ if (status!=0) IBISSignal(ibis_unit,status,0);





                      /*****************************/
                      /* Define the IBIS file type */
                      /*****************************/
     strcpy(type,"tiepoint");

     status=IBISFileSet(ibis_unit,IFILE_TYPE,type,0);
     if (status < 0) IBISSignalU(vunit,status,0);





                    /******************************/
                    /* Define the sub-group names */
                    /******************************/
/* IMAGE_N: line */    
    if (n_imgrps > 0) {
    
        for (i=0;i<n_imgrps;i++) {
        
             col[i]=1+i*(2+n_ipqlf);
             }
        count=IBISGroupNew(ibis_unit,"group", "line",col,n_imgrps,0);
        }

/* IMAGE_N: samp */ 
   if (n_imgrps > 0) {
         
       for (i=0;i<n_imgrps;i++) {
            col[i]=2+i*(2+n_ipqlf);
            }
       count=IBISGroupNew(ibis_unit,"group", "samp",col,n_imgrps,0);
       }

/* IMAGE_N: qualif*/  
   if (n_ipqlf > 0 && n_imgrps > 0) {
   
       for (i=0;i<n_ipqlf;i++) {
            for (j=0;j<n_imgrps;j++) {
            
                 col[j]=3+i+j*(2+n_ipqlf);
                 }

            count=IBISGroupNew(ibis_unit,"group", ipqlf_na[i], col, n_imgrps,0);
            
            if (count==IBIS_GROUP_ALREADY_EXISTS) {
            
                status=IBISGroupModify (ibis_unit,"group",
                                        ipqlf_na[i],"append",col,n_imgrps);
                if (status < 0) IBISSignal(ibis_unit,status,0);
                }
            }
      }


/* GENERAL_QLF: qualif*/ 

   for (i = 0;i < n_gqlf; i++) {
   
        col[0]=i+1 + n_imgrps*(2+n_ipqlf);
        
        count = IBISGroupNew (ibis_unit,"group", gqlf_na[i], col, 1, 0);
        if (count==IBIS_GROUP_ALREADY_EXISTS) {
            status=IBISGroupModify(ibis_unit,"group",
                                   gqlf_na[i], "append", col, 1);
            if (status < 0) IBISSignal(ibis_unit,status,0);
            }
       }





                    /******************************/
                    /* Define the sub-group units */
                    /******************************/

/* IMAGE_N: line & samp */   
    if (n_imgrps > 0) {
        for (i=0;i<n_imgrps;i++) {
             col[2*i]=1+i*(2+n_ipqlf);
             col[2*i+1]=2+i*(2+n_ipqlf);
             }
        count=IBISGroupNew(ibis_unit,"unit",
                           "pixels",col,2*n_imgrps,0);
        if (count==IBIS_GROUP_ALREADY_EXISTS) {
        
            status=IBISGroupModify (ibis_unit,"unit",
                                    "pixels","append",col,2*n_imgrps);
            if (status < 0) IBISSignal(ibis_unit,status,0);
            }
        }


/* IMAGE_N: qualif*/     
    if (n_ipqlf > 0 && n_imgrps > 0) {
    
        for (i=0;i<n_ipqlf;i++) {
        
             for (j=0;j<n_imgrps;j++) {
             
                  col[j]=3+i+j*(2+n_ipqlf);
                  }
             count=IBISGroupNew(ibis_unit,"unit",
                                ipqlf_un[i], col, n_imgrps, 0);
             if (count==IBIS_GROUP_ALREADY_EXISTS) {
                 status=IBISGroupModify(ibis_unit,"unit",
                                        ipqlf_un[i],"append",col,n_imgrps);
                 if (status < 0) IBISSignal(ibis_unit,status,0);
                 }
            }
        }

/* GENERAL_QLF: qualifiers */ 
    for (i=0;i<n_gqlf;i++) {
                                
         col[0]=i+1+ n_imgrps*(2+n_ipqlf);
         count=IBISGroupNew(ibis_unit,"unit", gqlf_un[i], col, 1, 0);
          if (count==IBIS_GROUP_ALREADY_EXISTS) {
              status=IBISGroupModify(ibis_unit,"unit",
                                     gqlf_un[i], "append", col,1);
              if (status < 0) IBISSignal(ibis_unit,  status,0);
              }
          }





                       /**************************/
                       /* Define the main groups */
                       /**************************/
/* IMAGE_N */     
    for (i=0;i<n_imgrps;i++) {
    
          col[0]=1+i*(2+n_ipqlf);
          col[1]=2+i*(2+n_ipqlf);
          for (j=0;j<n_ipqlf;j++) {
          
              col[j+2]=j+3+i*(2+n_ipqlf);
              }
          strcpy(name,"IMAGE_");
          j=i+1;
          sprintf(number,"%d",j);
          strcat(name,number);
          count=IBISGroupNew(ibis_unit,"group",name,col, (2+n_ipqlf),0);
          }


/* GENERAL_QLF */     
    if (n_gqlf > 0) { 
    
        for (i=0;i<n_gqlf;i++) {
        
             col[i]=n_imgrps*(2+n_ipqlf)+1+i;
             }
             
        count=IBISGroupNew(ibis_unit,"group", "GENERAL_QLF",col,n_gqlf,0);
        }

/* Then return the IBIS unit */  


   *iunit=ibis_unit;



   return status;

   }


    
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ibistiepnt.imake
/* Imake file for VICAR subroutine ibistiepnt */

#define SUBROUTINE   ibistiepnt

#define MODULE_LIST  ibistiepnt.c

#define P2_SUBLIB

#define USES_ANSI_C
 
#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$Other_File:
$ create ibistiepnt.hlp
1 IBISTIEPNT

  Module IBISTIEPNT contains routines for opening and closing and
  reading and writing IBIS-2 tiepoint files with general qualifiers
  and image point qualifiers:

    zitiepnt_openw opens a new IBIS tiepoint file for writing.

    zitiepnt_openr opens an existing IBIS tiepoint file for reading.

    zitiepnt_openu opens an existing IBIS tiepoint file for updating.

    zitiepnt_write writes a row to an IBIS tiepoint file.

    zitiepnt_read reads a row from an IBIS tiepoint file.

    zitiepnt_close closes an IBIS tiepoint file.

 The type of tiepoint file used in these routines is described in the reference
 given below.  For convenience, some of the information from the reference
 will be quoted here.

 A tiepoint file with qualifiers is a tabular file (with rows and columns)
 containing data for tiepoints from a user-specified set of n images (pictures).
 Each row (record) contains data items for the set of n images for one tiepoint.
 Each row has the same format (same set of items in the same order).  
 Each column contains the values for a given record item for the set of
 records in the file. 

 The order of items in each row is as follows (! order changes via the first spec!):

  L1 S1 q11 q12 ... q1m  L2 S2 q21 q22 ... q2m ... Ln Sn qn1 qn2 ...qnm Q0 Q1 ... 

The Li and Si, i=1 to n, are the line and sample coordinates of a given
"conjugate point" or "tiepoint" measured in images i=1 to n.
"The (optional) qij items are " 'image point qualifiers' containing information
about particular coordinate line/sample pairs.  While the line/sample 
coordinates are always real, the qualifiers may be integer, real, or text;
however, for a given m, this qualifier type is always constant."
"The last columns (Q0, Q1...) of each row contain (optional) 'general
qualifiers' pertaining to the 'complete tiepoint'.  The general qualifiers
may be of type integer, real, or text."

      
2  CALLING SEQUENCE

  C Calling Sequences:        

#include "ibistiepnt.h"
#define OK    1

   status = zitiepnt_openw (unit, &ibis, 
                            Nimages, imgs_fnam,
                            Ngqual, gqual_nam, gqual_fmt, gqual_unit, 
                            Niqual, iqual_nam, iqual_fmt, iqual_unit, nrow);

   status = zitiepnt_openr (unit, &ibis,
                           &Nimages, imgs_fnam,
                           &Ngqual, gqual_nam, gqual_fmt, gqual_unit, 
                           &Niqual, iqual_nam, iqual_fmt, iqual_unit, &nrow);


   status = zitiepnt_openu (unit, &ibis,
                            &Nimages, imgs_fnam,
                            &Ngqual, gqual_nam, gqual_fmt, gqual_unit, 
                            &Niqual, iqual_nam, iqual_fmt, iqual_unit, &nrow);

   status = zitiepnt_write (unit,irow,lbuf,sbuf,gr_qbuf, gi_qbuf,ga_qbuf,
                                                 ir_qbuf, ii_qbuf,ia_qbuf);

   status = zitiepnt_read (unit,irow,lbuf,sbuf,gr_qbuf, gi_qbuf,ga_qbuf,
                                                 ir_qbuf, ii_qbuf,ia_qbuf);
   status = zitiepnt_close(unit);

    Note the & for scalar variables passed by address.


  FORTRAN Calling Sequences:  not implemented yet.

  For all of the above routines, the returned status is the (defined) value
  OK if the routine is successful.  If the routine is not successful, the
  routine will print an error message and return a different value.
  For any of the "open" routines, the calling application 
  should call XVUNIT first, but the file should be closed
  when the "open" routine is called.

2  ARGUMENTS

   zitiepnt_openw 

      unit        VICAR unit number of tiepoint file.  (input, integer)
                  (unit,  Nimages, Ngqual, Niqual, and nrow are passed by value
                  for this routine.)
      ibis        IBIS file descriptor of tiepoint file.  (output, integer)
                   (This is returned just in case you need it.)
      Nimages     Number of images for which tiepoint file contains tiepoints.
                  (input, integer)   A tiepoint file with qualifiers contains
                  data for tiepoints from a user-specified set of n images.
      imgs_fnam   Name of the image files.
                  The declaration is:   char imgs_fnam[][FNAMLEN]
                  (FNAMLEN is defined in ibistiepnt.h) 
      Ngqual      Number of General qualifiers.     (input, integer)
      gqual_nam   Name of the general qualifier.
                  (input, an array of Ngqual strings)
                  The declaration is:   char gqual_nam[][STRING_32]
                  (STRING_32 is defined in ibistiepnt.h) 
      gqual_fmt   General qualifier data format specifications.
                  (input, an array of Ngqual strings)
                  The declaration is:   char gqual_fmt[][IFMT_SIZE]
                  (IFMT_SIZE is defined in ibisfile.h, which is included
                  by itiepointq.h.)  The valid format specifications are
                  "REAL" (for float), "FULL" (for integer), or for text,
                  "A1","A2",..., "A256".  Text columns must be assigned
                  a fixed size (field width).  For example, "A256" means
                  text with a field width of 256 characters.  The whole field
                  can be filled with characters; the last position is NOT
                  reserved for a null terminator.  The routines
                  zitiepnt_read and zitiepnt_write  provide an exta space
                  for a null terminator for each text field.  Thus the user
                  only needs to include a null terminator in the field if
                  they want to store a string shorter than the field width.
                  Thus, the calling application
                  may store null-terminated strings in such text fields
                  if desired, but the application should still reflect the
                  fact that zitiepnt_read and zitiepnt_write transfer
                  the fixed number of characters.
      gqual_unit  Unit of the general qualifier (e.g. pixels, m, cm ...).
                  (input, an array of Ngqual strings)
                  The declaration is:   char gqual_unit[][STRING_32]
      Niqual      Number of image point qualifiers per image. (input, integer)
      iqual_nam   Name of the image point qualifier.
                  (input, an array of Niqual strings)
                  The declaration is:   char iqual_nam[][STRING_32]
      iqual_fmt   Image point qualifier data format specifications.
                  (input, an array of Niqual strings)
                  The declaration is:   char iqual_fmt[][IFMT_SIZE]
                  (See under gqual_fmt for more information.)
      iqual_unit  Unit of the image point qualifier (e.g. pixels, m, cm ...).
                  (input, an array of Niqual strings)
                  The declaration is:   char iqual_unit[][STRING_32]
      nrow        Number of rows to be allocated.   (input, integer)
                  Make this somewhat greater than the largest you expect the
                  file to get.
                  
   zitiepnt_openr 
      unit        VICAR unit number of tiepoint file.  (input, integer)
                  (unit is passed by value for this routine.)
      ibis        IBIS file descriptor of tiepoint file.  (output, integer)
                   (This is returned just in case you need it.)
      Nimages     Number of images for which tiepoint file contains tiepoints.
                  (output, integer)   A tiepoint file with qualifiers contains
                  data for tiepoints from a user-specified set of n images.
      imgs_fnam   Name of the image files.
                  The declaration is:   char imgs_fnam[][FNAMLEN]
                  (FNAMLEN is defined in ibistiepnt.h) 
      Ngqual      Number of General qualifiers.     (output, integer)
      gqual_nam   Name of the general qualifier.
                  (input, an array of Ngqual strings)
                  The declaration is:   char gqual_nam[][STRING_32]
                  (STRING_32 is defined in ibistiepnt.h) 
      gqual_fmt   General qualifier data format specifications.
                  (output, an array of Ngqual strings)
                  The declaration is:   char gqual_fmt[][IFMT_SIZE]
                  (IFMT_SIZE is defined in ibisfile.h, which is included
                  by itiepointq.h.)  The valid format specifications are
                  "REAL" (for float), "FULL" (for integer), or for text,
                  "A1","A2",..., "A256".  Text columns must be assigned
                  a fixed size (field width).  For example, "A256" means
                  text with a field width of 256 characters.  The whole field
                  can be filled with characters; the last position is NOT
                  reserved for a null terminator.  The routines
                  zitiepnt_read and zitiepnt_write  provide an extra space
                  for a null terminator for each text field.  Thus the user
                  only needs to include a null terminator in the field if
                  they want to store a string shorter than the field width.
                  Thus, the calling application
                  may store null-terminated strings in such text fields
                  if desired, but the application should still reflect the
                  fact that zitiepnt_read and zitiepnt_write transfer
                  the fixed number of characters.
      gqual_unit  Unit of the general qualifier (e.g. pixels, m, cm ...).
                  (input, an array of Ngqual strings)
                  The declaration is:   char gqual_unit[][STRING_32]
      Niqual      Number of image point qualifiers per image. (output, integer)
      iqual_nam   Name of the image point qualifier.
                  (input, an array of Niqual strings)
                  The declaration is:   char iqual_nam[][STRING_32]
      iqual_fmt   Image point qualifier data format specifications.
                  (output, an array of Niqual strings)
                  The declaration is:   char iqual_fmt[][IFMT_SIZE]
                  (See under gqual_fmt for more information.)
      iqual_unit  Unit of the image point qualifier (e.g. pixels, m, cm ...).
                  (input, an array of Niqual strings)
                  The declaration is:   char iqual_unit[][STRING_32]
      nrow        Number of rows allocated.   (output, integer)

   zitiepnt_openu 
                  The arguments are the same as for zitiepnt_openr.

   zitiepnt_write 
                  For both zitiepnt_write and zitiepnt_read, it is helpful
                  to relate the arguments to the row diagram that was explained
                  above.  It is repeated here for the reader.
L1 S1 q11 q12 ... q1m  L2 S2 q21 q22 ... q2m ... Ln Sn qn1 qn2 ...qnm  Q0 Q1 ... 
                  The row contains several different types of values.  These
                  routines utilize a separate argument for each of the different
                  types of values.

      unit        VICAR unit number of tiepoint file.  (input, integer)
                  (unit and irow are passed by value for this routine.)
      irow        the row number (ordinal) at which to write data.  The
                  numbering starts with irow=1 for the first row in the file.
                  (input, integer)
      lbuf        the line coordinates, L1,L2,...Ln.  (input, array of floats
                  of size Nimages)
      sbuf        the sample coordinates, S1,S2,...Sn (input, array of floats
                  of size Nimages)
      gr_qbuf     the general qualifiers of type REAL, ordered left to right
                  as in row diagram.  The size of this array is the number of
                  corresponding (REAL) formats in the gqual_fmt argument to the
                  "open" routine.  (input, array of floats)
      gi_qbuf     the general qualifiers of type FULL, ordered left to right
                  as in row diagram.  The size of this array is the number of
                  corresponding (FULL) formats in the gqual_fmt argument to the
                  "open" routine.  (input, array of integers)
      ga_qbuf     the general qualifiers of type ASCII, ordered left to right
                  as in row diagram.  These are packed in adjacent elements
                  of one character array.  Each ASCII qualifier is allocated
                  space in this array according to the number of characters
                  (field width) specified in the corresponding (A..) formats 
                  in the gqual_fmt argument to the "open" routine.  
                  In addition this buffer must include one extra space
                  for a null terminator for each text field.  For example,
                  to store an A3 field and an A2 field, the buffer should
                  contain the three characters (A3), then a null terminator,
                  then the two characters (A2), and then a null terminator.
                  (input, array of characters)
      ir_qbuf     the image point qualifiers of type REAL, ordered left to right
                  as in row diagram.  The size of this array is Nimages times
                  the number of corresponding (REAL) formats in the 
                  iqual_fmt argument to the
                  "open" routine.  (input, array of floats)
      ii_qbuf     the image point qualifiers of type FULL, ordered left to right
                  as in row diagram.  The size of this array is Nimages times
                  the number of corresponding (FULL) formats in the 
                  iqual_fmt argument to the
                  "open" routine.  (input, array of integers)
      ia_qbuf     the general qualifiers of type ASCII, ordered left to right
                  as in row diagram.  These are packed in adjacent elements
                  of one character array.  Each ASCII qualifier is allocated
                  space in this array according to the number of characters
                  (field width) specified in the corresponding (A..) formats 
                  in the gqual_fmt argument to the "open" routine.  
                  In addition this buffer must include one extra space
                  for a null terminator for each text field.  For example,
                  to store an A3 field and an A2 field, the buffer should
                  contain the three characters (A3), then a null terminator,
                  then the two characters (A2), and then a null terminator.
                  (input, array of characters)

   zitiepnt_read 
                  uses the same set of arguments as zitiepnt_write, except
                  that all of the array arguments are outputs of this routine.

   zitiepnt_close
      unit        VICAR unit number of tiepoint file.  (input, integer)
                  (unit is passed by value for this routine.)

2  HISTORY

      Original Programmer: Steve Pohorsky     April 1997
      Current Cognizant Programmer: Steve Pohorsky
      Source Language: C


 Revision History
  4-97  ...SP.... Initial release is portable (VMS and UNIX).
  6-97  ...adapt to the DLR hwibis() specification (F. Wewel, DLR)
  
2  OPERATION

 zitiepnt_openw sets up an IBIS file with NCOL (number of columns) =
 Ngqual + Nimages * (2 + Niqual).   This routine stores Nimages, Ngqual, and
 Niqual in the VICAR property label.  These values are read when either
 zitiepnt_openr or zitiepnt_openu are called and are used to recompute
 the number of columns to be accessed.  (The latter two routines allow extra
 columns to be added by the user following the original columns.  This gives the
 user the option of storing additional data in the file.  Any extra columns
 in the file will not be altered by this module.)

 These "open" routines store information about the column formats so that
 zitiepnt_read and zitiepnt_write know the size of their array arguments.

2 REFERENCES

 Data Description: Mars 94/96 image point collection program design document,
                   Dr. Juergen Oberst, DLR, 31-Oct-1996, Appendix 3

 IBIS Software:    "IBIS Subroutine Library Programmers' Guide", 
                   Dr. Niles Ritter, JPL Document D-11944.
$ Return
$!#############################################################################
$Test_File:
$ create tibistiepnt.pdf
PROCESS
PARM OUT TYPE=STRING COUNT=2
END-PROC
$!-----------------------------------------------------------------------------
$ create tibistiepnt.imake
/* IMAKE file for Test of VICAR subroutine tibistiepnt */

#define PROGRAM tibistiepnt

#define MODULE_LIST tibistiepnt.c 

#define TEST

#define MAIN_LANG_C
#define USES_ANSI_C
 
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/* #define LIB_LOCAL */

$!-----------------------------------------------------------------------------
$ create tibistiepnt.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "vicmain_c"
#include "ibistiepnt.h"


/* Program to test routines in ibistiepnt  */
void main44 ()
   {
   int       vic_fd, vic_fd2;
   int       ibis_fd, ibis_fd2;
   int       no_imgs;
   char      imgs_fnam[MAX_noimgs][FNAMLEN];
   int       no_genqlf;
   char      genqlf_nam [MAX_nogenqlf][STRING_32]; 
   char      genqlf_frmt[MAX_nogenqlf][IFMT_SIZE]; 
   char      genqlf_unit[MAX_nogenqlf][STRING_32]; 
   int       no_imgqlf;
   char      imgqlf_nam [MAX_noimgqlf][STRING_32]; 
   char      imgqlf_frmt[MAX_noimgqlf][IFMT_SIZE]; 
   char      imgqlf_unit[MAX_noimgqlf][STRING_32]; 
   int       no_rows;
   int       i, j, k, index;
   int       status;
   int       slen0, slen1;
   short     c;
   int       no_irqlf;   
   float      lne_buf[5], smp_buf[5];
   /*  -------------------------------------------------- general qualifiers*/
   int        gi_qbuf[2]; 
   char       ga_qbuf[13];
   /*  ---------------------------------------------------- image qualifiers*/
   float      ir_qbuf[10]; 
   int 	      ii_qbuf[5]; 
   char       ia_qbuf[25];

   float      lne_buf2[5], smp_buf2[5];
   /*  -------------------------------------------------- general qualifiers*/
   int        gi_qbuf2[2]; 
   char       ga_qbuf2[13];
   /*  ---------------------------------------------------- image qualifiers*/
   float      ir_qbuf2[10]; 
   int 	      ii_qbuf2[5]; 
   char       ia_qbuf2[25];
   
   
   
   no_imgs   = 5;
   no_genqlf = 4;
   no_imgqlf = 4;
   
   no_rows   = 50;
   
   
/* ------------------------------------------------
                                           general qualifier */
   sprintf (genqlf_nam [0], "id-number");
   sprintf (genqlf_frmt[0], "FULL");
   sprintf (genqlf_unit[0], "none");
   
   sprintf (genqlf_nam [1], "specification");
   sprintf (genqlf_frmt[1], "A10");
   sprintf (genqlf_unit[1], "XX");
  
   slen0 = atoi(genqlf_frmt[1] + 1) + 1;


   sprintf (genqlf_nam [2], "quality level");
   sprintf (genqlf_frmt[2], "FULL");
   sprintf (genqlf_unit[2], "Pixel");
  

   sprintf (genqlf_nam [3], "class");
   sprintf (genqlf_frmt[3], "A1");
   sprintf (genqlf_unit[3], "none");
  

/* ------------------------------------------------
                                           image qualifier */
   sprintf (imgqlf_nam [0], "cross correlation");
   sprintf (imgqlf_frmt[0], "REAL");
   sprintf (imgqlf_unit[0], "none");
   
   sprintf (imgqlf_nam [1], "lsm correlation");
   sprintf (imgqlf_frmt[1], "REAL");
   sprintf (imgqlf_unit[1], "0");
  
   no_irqlf = 2;

   sprintf (imgqlf_nam [2], "point class");
   sprintf (imgqlf_frmt[2], "FULL");
   sprintf (imgqlf_unit[2], "none");

   sprintf (imgqlf_nam [3], "descriptor");
   sprintf (imgqlf_frmt[3], "A4");
   sprintf (imgqlf_unit[3], "abc");

   slen1 = atoi(imgqlf_frmt[3] + 1) + 1;


/* ------------------------------------------------
                                              any image name */
   for (i = 0; i < no_imgs; i++) {
        sprintf (&imgs_fnam[i][0], "image_%3.3d.byte", i+100);
        }
        
/* ---------------------------------------------------------------
                                                 get vicar file unit */
   status = zvunit (&vic_fd, "OUT", 1, 0);
   if (status != OK) {
      zmabend("Error opening a file for Write.");
      }

/* ---------------------------------------------------------------
                                         open ibis file for writing */
   status = zitiepnt_openw (vic_fd, &ibis_fd,
                            no_imgs, imgs_fnam,
                            no_genqlf, genqlf_nam, genqlf_frmt, genqlf_unit, 
                            no_imgqlf, imgqlf_nam, imgqlf_frmt, imgqlf_unit, 
                            no_rows);


   if (status != OK) {
      zmabend("Error opening a file for Write.");
      }

/*  Now set up and write data for 100 rows.  */

   for (i = 1; i <= no_rows; ++i){
   
         for (j = 0; j < no_imgs; j++) {
           /* -----------------------------------------
                                        line, sample for each images */
  
             lne_buf[j] = (float) j; 
             smp_buf[j] = (float) j + 10.0;

                         
        /*  -------------------------------------------
                            the image qualifier                      */
        /*  -------------------------------------------
                                            cross correlation (real) */
            index = j * no_irqlf; 
            ir_qbuf[index]     = (float) i / 100.0 - 0.5;
        /*  -------------------------------------------
                                               lsm corelation (real) */
            ir_qbuf[index + 1] = (float) i / 100.0;
        /*  -------------------------------------------
                                                   point class (int) */
            index = j;
            ii_qbuf[index] = j * 100 + i;
            
        /*  -------------------------------------------
                                               descriptor (char [4]) */
            index = j * slen1;
            sprintf (ia_qbuf + index, "%c%3.3d", (short)(65+j), i+100 * j);  
            
            }
            
    /*  -------------------------------------------
                          the general qualifier                      */
                          
       /*  -------------------------------------------
                                                      id-number (int) */
           gi_qbuf[0] = i;
       /*  -------------------------------------------
                                                  quality level (int) */
           gi_qbuf[1] = i % 3 ? 0 : 1;
   
       /*  -------------------------------------------
                                            specification (char [10]) */
           sprintf (ga_qbuf, "crater%4.4d", i + 200);
       /*  -------------------------------------------
                                            specification (char [10]) */
           index = slen0;          /* !!! index = strlen(Axx) + 1 !!! */
           sprintf (ga_qbuf + index, "%c", i % 4 ? 'B' : 'A');

        /* -------------------------------------------
                                    write one record to the IBIS file */
           status = zitiepnt_write (vic_fd, i, 
                                      lne_buf, smp_buf,
                                      0,       gi_qbuf, ga_qbuf,
                                      ir_qbuf, ii_qbuf, ia_qbuf);
                                       
           if (status != OK)
               zmabend("Error on write.");

           }   /*  end of for loop  */


   status = zitiepnt_close(vic_fd);
   if (status != OK)
       zmabend("Error on close.","");

/*  ----------------------------------------------------------
             Now read the tiepoint data from the file into different arrays */


/* ------------------------------------------------
                                           general qualifier */
   sprintf (genqlf_nam [0], "");
   sprintf (genqlf_frmt[0], "");
   sprintf (genqlf_unit[0], "");
   
   sprintf (genqlf_nam [1], "");
   sprintf (genqlf_frmt[1], "");
   sprintf (genqlf_unit[1], "");
  
   sprintf (genqlf_nam [2], "");
   sprintf (genqlf_frmt[2], "");
   sprintf (genqlf_unit[2], "");
  

   sprintf (genqlf_nam [3], "");
   sprintf (genqlf_frmt[3], "");
   sprintf (genqlf_unit[3], "");

/* ------------------------------------------------
                                           image qualifier */
   sprintf (imgqlf_nam [0], "");
   sprintf (imgqlf_frmt[0], "");
   sprintf (imgqlf_unit[0], "");
   
   sprintf (imgqlf_nam [1], "");
   sprintf (imgqlf_frmt[1], "");
   sprintf (imgqlf_unit[1], "");
  
   sprintf (imgqlf_nam [2], "");
   sprintf (imgqlf_frmt[2], "");
   sprintf (imgqlf_unit[2], "");

   sprintf (imgqlf_nam [3], "");
   sprintf (imgqlf_frmt[3], "");
   sprintf (imgqlf_unit[3], "");

   no_imgs   = 0;
   no_genqlf = 0;
   no_imgqlf = 0;   
   no_rows   = 0;
  
    status = zitiepnt_openr (vic_fd,  &ibis_fd,
                             &no_imgs, imgs_fnam,
                             &no_genqlf, genqlf_nam, genqlf_frmt, genqlf_unit, 
                             &no_imgqlf, imgqlf_nam, imgqlf_frmt, imgqlf_unit, 
                             &no_rows);
    if (status != OK)
       zmabend("Error opening a file for Read.");


   /* -----------------------------------------------
                                               type label of tiepoint file */
      printf ("\n number of images: %d\n", no_imgs);
      for (i = 0; i < no_imgs; i++) {
      
           printf (" %d %s\n", i+1, imgs_fnam[i]);
      
           }
      printf ("\n number of image qualifier: %d\n", no_imgqlf);
      for (i = 0; i < no_imgqlf; i++) {
      
           printf (" %d %20s  %6s  %6s\n", i+1, imgqlf_nam[i],
                                                imgqlf_frmt[i], imgqlf_unit[i]);
      
           }
           
      printf ("\n number of general qualifier: %d\n", no_genqlf);
      for (i = 0; i < no_genqlf; i++) {
      
           printf (" %d %20s  %6s  %6s\n", i+1, genqlf_nam[i],
                                                genqlf_frmt[i], genqlf_unit[i]);
           }
           



/*  Read the 100 rows, checking that no data was lost in the move.  */
   k = 0;                        /* sollte es auch bleiben (A.d.S.) */
   for (i = 1; i <= no_rows; ++i){
   
        status = zitiepnt_read (vic_fd, i,  
                                lne_buf2, smp_buf2, 
                                0,        gi_qbuf2, ga_qbuf2,
                                ir_qbuf2, ii_qbuf2, ia_qbuf2);
        if (status != OK)
            zmabend("Error on read.");

   
         for (j = 0; j < no_imgs; j++) {
           /* -----------------------------------------
                                        line, sample for each images */
  
             lne_buf[j] = (float) j; 
             smp_buf[j] = (float) j + 10.0;

                         
        /*  -------------------------------------------
                            the image qualifier                      */
        /*  -------------------------------------------
                                            cross correlation (real) */
            index = j * no_irqlf; 
            ir_qbuf[index]     = (float) i / 100.0 - 0.5;
        /*  -------------------------------------------
                                               lsm corelation (real) */
            ir_qbuf[index + 1] = (float) i / 100.0;
        /*  -------------------------------------------
                                                   point class (int) */
            index = j;
            ii_qbuf[index] = j * 100 + i;
            
        /*  -------------------------------------------
                                               descriptor (char [4]) */
            index = j * slen1;
            sprintf (ia_qbuf + index, "%c%3.3d", (short)(65+j), i+100 * j);  
            
            }
            
    /*  -------------------------------------------
                          the general qualifier                      */
                          
       /*  -------------------------------------------
                                                      id-number (int) */
           gi_qbuf[0] = i;
       /*  -------------------------------------------
                                                  quality level (int) */
           gi_qbuf[1] = i % 3 ? 0 : 1;
   
       /*  -------------------------------------------
                                            specification (char [10]) */
           sprintf (ga_qbuf, "crater%4.4d", i + 200);
       /*  -------------------------------------------
                                            specification (char [10]) */
           index = slen0;          /* !!! index = strlen(Axx) + 1 !!! */
           sprintf (ga_qbuf + index, "%c", i % 4 ? 'B' : 'A');



/*  check for differences between what was written and what was read  */

   for (j = 0; j < no_imgs; j++) {
       if (lne_buf[j] != lne_buf2[j])		                   ++k;
       if (smp_buf[j] != smp_buf2[j])		                   ++k;
       
       index = j * no_irqlf;
       
       if (ir_qbuf[index]   != ir_qbuf2[index])            ++k;
       if (ir_qbuf[index+1] != ir_qbuf2[index+1])          ++k;      
       
       
       if (ii_qbuf[j] !=  ii_qbuf2[j])                     ++k;

       if (strncmp(ia_qbuf+j*slen1, ia_qbuf2+j*slen1, 4)) ++k;
       
       }
       
   if (gi_qbuf[0] != gi_qbuf2[0])                          ++k;
   if (gi_qbuf[1] != gi_qbuf2[1])                          ++k;
      
   if (strncmp(ga_qbuf,       ga_qbuf2, 10))               ++k;
   if (strncmp(ga_qbuf+slen0, ga_qbuf2+slen0, 1))          ++k;




           }   /* ------------------------------------ end of for loop  */


   status = zitiepnt_close(vic_fd);
   if (status != OK)
       zmabend("Error on close.");

   if (k == 0) 
       zvmessage("Success on test 1.","");
   else
       zvmessage("Failure on test 1.","");


/* -------------------------------------------- Next test updates a row.  */

   status = zitiepnt_openu (vic_fd,  &ibis_fd,
                             &no_imgs, imgs_fnam,
                             &no_genqlf, genqlf_nam, genqlf_frmt, genqlf_unit, 
                             &no_imgqlf, imgqlf_nam, imgqlf_frmt, imgqlf_unit, 
                             &no_rows);
   if (status != OK)
      zmabend("Error opening a file for Update.");


/* --------------------------------------- 1. changing row 5 */
   for (j = 0; j < no_imgs; j++) {
   /* -----------------------------------------
                                line, sample for each images */
  
        lne_buf[j] = (float)  55.55; 
        smp_buf[j] = (float) 555.55;

    /*  -------------------------------------------
                         the image qualifier                      */
    /*  -------------------------------------------
                                         cross correlation (real) */
         index = j * no_irqlf; 
         ir_qbuf[index]     =  50.0 + (float) j / 10.0;
    /*  -------------------------------------------
                                            lsm corelation (real) */
         ir_qbuf[index + 1] = 500.0 + (float) j / 10.0;
    /*  -------------------------------------------
                                                point class (int) */
         index = j;
         ii_qbuf[index] = 50 + j;
            
     /*  -------------------------------------------
                                            descriptor (char [4]) */
         index = j * slen1;
         sprintf (ia_qbuf + index, "%c%3.3d", (short)(97+j), j);  
            
         }
            
/*  -------------------------------------------
                     the general qualifier                      */
                          
/*  -------------------------------------------
                                                id-number (int) */
     gi_qbuf[0] = 5555;
/*  -------------------------------------------
                                            quality level (int) */
      gi_qbuf[1] = -455;
   
/*  -------------------------------------------
                                      specification (char [10]) */
    sprintf (ga_qbuf, "crater%4.4d", 5005);
/*  -------------------------------------------
                                      specification (char [10]) */
    index = slen0;           /* !!! index = strlen(Axx) + 1 !!! */
    sprintf (ga_qbuf + index, "%c", '5');

/* -------------------------------------------
                              write one record to the IBIS file */
    status = zitiepnt_write (vic_fd, 5, 
                             lne_buf, smp_buf,
                             0,       gi_qbuf, ga_qbuf,
                             ir_qbuf, ii_qbuf, ia_qbuf);
                                       
    if (status != OK)
        zmabend ("Error on write.");


/* ------------------------------------- 2. appending 10 rows */
   status = IBISFileSet (ibis_fd, IFILE_NR, (char *)no_rows + 10, 0);

   if (status != OK)
        zmabend ("Error on append.");
        
        
/* ------------------------------------ copy the values from row 5
                                        to the ten new rows */
   for (j = 1; j <= 10; j++) {
     
      /* ------------------------------------------- 
                              write one record to the IBIS file */
                              
         gi_qbuf[0] = j * 10 + j;
         status = zitiepnt_write (vic_fd, no_rows + j, 
                                  lne_buf, smp_buf,
                                  0,       gi_qbuf, ga_qbuf,
                                  ir_qbuf, ii_qbuf, ia_qbuf);
         if (status != OK)
             zmabend ("Error on write.");
         }
         
   status = zitiepnt_close(vic_fd);
   if (status != OK)
       zmabend("Error on close.");
       
     
 
/*  Now for a two-file test.  Tests code for handling multiple files.  */

/*   step 1: open first file for read  */


    status = zitiepnt_openr (vic_fd,  &ibis_fd,
                            &no_imgs, imgs_fnam,
                            &no_genqlf, genqlf_nam, genqlf_frmt, genqlf_unit, 
                            &no_imgqlf, imgqlf_nam, imgqlf_frmt, imgqlf_unit, 
                            &no_rows);

    if (status != OK)
        zmabend("Error opening a file for Read.");

/*   step 2: open second file for write with no qualifiers.  */

   status = zvunit(&vic_fd2, "OUT", 2, 0);
   zvsignal(vic_fd2, status, 1);               /* abort if error */

/* ----------------------- open a new tipepoint file for writing */
   status = zitiepnt_openw(vic_fd2, &ibis_fd2,
                           no_imgs, imgs_fnam,
                           1, genqlf_nam, genqlf_frmt, genqlf_unit, 
                           1, imgqlf_nam, imgqlf_frmt, imgqlf_unit, 
                           no_rows);

   if (status != OK)
      zmabend("Error opening a file for Write.");

/* -------------------------------------------------
                                copy line sample of file 1 to file 2 
                                with 1 image and 1 general qualifier */

    for ( i = 1; i <= no_rows;  i++) {
    
         status = zitiepnt_read (vic_fd, i,  
                                 lne_buf, smp_buf, 
                                 0,       gi_qbuf, ga_qbuf,
                                 ir_qbuf, ii_qbuf, ia_qbuf);


         if (status != OK)
             zmabend ("Error on read.");

         for (j = 0; j < no_imgs; j++) {
              index = j * no_irqlf;
              ir_qbuf2[j] = ir_qbuf[index];
              }

         status = zitiepnt_write (vic_fd2,  i, 
                                  lne_buf,  smp_buf,
                                  0,        gi_qbuf, 0,
                                  ir_qbuf2, 0,       0);

         if (status != OK)
             zmabend ("Error on write.");
         }  


   status = zitiepnt_close(vic_fd);
   if (status != OK)
       zmabend("Error on close.");
       
   status = zitiepnt_close(vic_fd2);
   if (status != OK)
       zmabend("Error on close.");
       
/* ---------------------------------------------------------
                                                 check for differences */
   status = zitiepnt_openr (vic_fd,  &ibis_fd,
                           &no_imgs, imgs_fnam,
                           &no_genqlf, genqlf_nam, genqlf_frmt, genqlf_unit, 
                           &no_imgqlf, imgqlf_nam, imgqlf_frmt, imgqlf_unit, 
                           &no_rows);


    if (status != OK)
        zmabend("Error on open (read).");

    status = zitiepnt_openr (vic_fd2,  &ibis_fd2,
                            &no_imgs, imgs_fnam,
                            &no_genqlf, genqlf_nam, genqlf_frmt, genqlf_unit, 
                            &no_imgqlf, imgqlf_nam, imgqlf_frmt, imgqlf_unit, 
                            &no_rows);

    if (status != OK)
        zmabend("Error on open (read).");



   /* -----------------------------------------------
                                               type label of tiepoint file */
      printf ("\n number of images: %d\n", no_imgs);
      for (i = 0; i < no_imgs; i++) {
      
           printf (" %d %s\n", i+1, imgs_fnam[i]);
      
           }
      printf ("\n number of image qualifier: %d\n", no_imgqlf);
      for (i = 0; i < no_imgqlf; i++) {
      
           printf (" %d %20s  %6s  %6s\n", i+1, imgqlf_nam[i],
                                                imgqlf_frmt[i], imgqlf_unit[i]);
      
           }
           
      printf ("\n number of general qualifier: %d\n", no_genqlf);
      for (i = 0; i < no_genqlf; i++) {
      
           printf (" %d %20s  %6s  %6s\n", i+1, genqlf_nam[i],
                                                genqlf_frmt[i], genqlf_unit[i]);
           }
           





    k = 0;

    for ( i = 1; i <= no_rows;  i++) {
    
         status = zitiepnt_read (vic_fd, i,  
                                 lne_buf, smp_buf, 
                                 0,       gi_qbuf, ga_qbuf,
                                 ir_qbuf, ii_qbuf, ia_qbuf);
         if (status != OK)
             zmabend ("Error on read.");

         status = zitiepnt_read (vic_fd2, i,  
                                 lne_buf2, smp_buf2, 
                                 0,        gi_qbuf2,   0,
                                 ir_qbuf2,  0,         0);
         if (status != OK)
             zmabend ("Error on read.");

         for (j = 0; j < no_imgs; j++) {
              if (lne_buf[j] != lne_buf2[j])         ++k;
              if (smp_buf[j] != smp_buf2[j])         ++k;
       
              index = j * no_irqlf;
       
              if (ir_qbuf[index]   != ir_qbuf2[j])   ++k;       
              }
       
         if (gi_qbuf[0] != gi_qbuf2[0])              ++k;
 
         }

   status = zitiepnt_close(vic_fd);
   if (status != OK)
       zmabend("Error on close.");
       
   status = zitiepnt_close(vic_fd2);
   if (status != OK)
       zmabend("Error on close.");

   if (k == 0) 
       zvmessage("Success on test 3.","");
   else
       zvmessage("Failure on test 3.","");

   }
$ Return
$!#############################################################################
