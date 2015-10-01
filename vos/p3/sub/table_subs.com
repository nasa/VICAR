$!****************************************************************************
$!
$! Build proc for MIPL module table_subs
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:57
$!
$! Execute by entering:		$ @table_subs
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
$ write sys$output "*** module table_subs ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("table_subs.imake") .nes. ""
$   then
$      vimake table_subs
$      purge table_subs.bld
$   else
$      if F$SEARCH("table_subs.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake table_subs
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @table_subs.bld "STD"
$   else
$      @table_subs.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create table_subs.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack table_subs.com -
	-s table_subs.c -
	-i table_subs.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create table_subs.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <string.h>

#include "const.h"
#include "parm.h"

char *strtok ();
double *nr_dvector ();
float **nr_matrix ();
void l_load_table ();
struct table *l_init_table ();
void l_strip_tail ();
void l_zero_temps ();
void l_list_table ();


#ifndef LLength
#define LLength 192
#endif
#ifndef TempRows
#define TempRows 3000
#endif
#ifndef TempCols
#define TempCols 100
#endif
#ifndef TempStrLen
#define TempStrLen 32
#endif

struct table *l_read_table(tablefile,n_tab,starttag,endtag,readnames,readunits)
     char *tablefile;
     int *n_tab;
     char *starttag;
     char *endtag;
     int *readnames;
     int *readunits;
{
  char   line[LLength], *svalue, stag[MAX_PAR_STRING], etag[MAX_PAR_STRING],
         names[TempCols][TempStrLen], units[TempCols][TempStrLen];

  int    eof, start, i, j, n, index, status, rows, columns, first, found,
         get_units, get_names, get_data, width[TempCols], exponent[TempCols];
  float  **tmatrix, temp;

  struct table *tbl, *firsttable;

  FILE *rfile;


  first = TRUE;
  tmatrix = nr_matrix(0,TempRows-1,0,TempCols-1);
  l_zero_temps (TempCols,TempRows,width,exponent,units,names,tmatrix);
  start = get_names = get_units = get_data = FALSE;

  rfile = fopen(tablefile,"r");

  while (getline_vicar(line, LLength, rfile, &eof) >= 0 && !eof) {
    if (line != 0) {
      l_strip_tail (line);

      if ((index = l_equal_array(starttag, line, *n_tab, "STRING")) != 0) {
         if (start) {
            if (first) {
               tbl = l_init_table (rows, columns, stag, tmatrix,
                               names , units, width, exponent);
               firsttable = tbl;
               first = FALSE;
               found = 1;
	    }
            else {
               tbl->nexttbl = l_init_table (rows, columns, stag, tmatrix,
                               names , units, width, exponent);
               tbl = tbl->nexttbl;
               found += 1;
	    }
            l_zero_temps(TempCols,TempRows,width,exponent,units,names,tmatrix);
	 }
         rows = columns = 0;
         start = TRUE;
         n = index-1;
         strcpy (stag,starttag + n*MAX_PAR_STRING);
         strcpy (etag,endtag + n*MAX_PAR_STRING);
         if (readnames[n])      get_names = TRUE;
         else if (readunits[n]) get_units = TRUE;
         else                   get_data = TRUE;
      }

      if (start && strlen(etag) && EQUALN(line,etag)) {
         if (first) {
            tbl = l_init_table (rows, columns, stag, tmatrix,
                               names , units, width, exponent);
            firsttable = tbl;
            first = FALSE;
            found = 1;
         }
         else {
            tbl->nexttbl = l_init_table (rows, columns, stag, tmatrix,
                            names , units, width, exponent);
            tbl = tbl->nexttbl;
            found +=1;
         }
         l_zero_temps (TempCols,TempRows,width,exponent,units,names,tmatrix);
         start = get_names = get_units = get_data = FALSE;
      }

      if (get_names) {
          i=l_string_parse(line,names);
          columns = columns > i ? columns : i;
          get_names = FALSE;
          if (readunits[n]) get_units = TRUE;
          else              get_data = TRUE;
      }
      else if (get_units) {
          i=l_string_parse(line,units);
          if (columns != i && columns != 0)
             l_message (" read_table error: columns names and units problems");
          columns = columns > i ? columns : i;
          get_units = FALSE;
          get_data = TRUE;
      }
      else if (get_data && (status = sscanf(line,"%f",&temp)) == 1) {
          i=0;
          svalue = strtok (line," \t\n");
          while (svalue != 0) {
             if ((status=sscanf(svalue,"%f",&tmatrix[rows][i])) !=1)
                tmatrix[rows][i] = 0.0;
             else {
                width[i] = strlen(svalue)>width[i] ? strlen(svalue):width[i];
                exponent[i] = strchr (svalue,'E') == 0 ? 0:1;
                if (strchr (svalue,'e') != 0) exponent[i] = 1;
	     }
             i+=1;
             svalue = strtok (0," \t\n");
          }
          rows += 1;
          if (columns != i && columns != 0)
             l_message ("read_table error: problem with number of columns ");
          columns = columns > i ? columns : i;
      }
    }
  }

  if (start) {
     if (first) {
        tbl = l_init_table (rows, columns, stag, tmatrix,
                               names , units, width, exponent);
        firsttable = tbl;
        first = FALSE;
        found = 1;
     }
     else {
        tbl->nexttbl = l_init_table (rows, columns, stag, tmatrix,
                            names , units, width, exponent);
        tbl = tbl->nexttbl;
        found +=1;
     }
  }
  *n_tab = found;

  fclose(rfile);

  nr_free_matrix(tmatrix,0,TempRows-1,0,TempCols-1);

  return firsttable;
}



void l_strip_tail (line)
   char *line;
{
   int j;

   j = strlen(line) - 1;
   while ((line[j]=='\n' || line[j]==' ' || line[j]=='\t') && j >= 0)
     line[j--] = '\0';
}



int l_equal_array (in_array, match, count, arr_type)
   void *in_array;
   void *match;
   int count;
   char *arr_type;
{
   char *first, *second;
   int i, mem = 0;

   for (i=0; i<count; i++) {
      if (EQUAL(arr_type,"CHAR") || EQUAL(arr_type,"STRING")) {
         if (EQUALN((char *)match, ((char *)in_array) + i*MAX_PAR_STRING ))
            mem = i+1;
      }
      else if (EQUAL(arr_type,"INT")) {
         if ((int)((int *)in_array)[i] == *((int *)match))
            mem = i+1;
      }
      else if (EQUAL(arr_type,"REAL") || EQUAL(arr_type,"FLOAT")) {
         if ((float)((float *)in_array)[i] == *((float *)match))
            mem = i+1;
      }
      else if (EQUAL(arr_type,"DOUB")) {
         if ((double)((double *)in_array)[i] == *((double *)match))
            mem = i+1;
      }
   }
   return mem;
}



int l_string_parse (line, string_array)
  char *line;
  char string_array[TempCols][TempStrLen];
{
   char *svalue;
   int column = 0;

   svalue = strtok (line," \t\n");
   while (svalue != 0) {
      sscanf(svalue,"%s",string_array[column]);
      column+=1;
      svalue = strtok (0," \t\n");
   }

   return column;
}



struct table *l_init_table (rows, columns, title, tmatrix,
                               names , units, width, exponent)
   float **tmatrix;
   char  *names, *units;
   int    *width,*exponent;
   int rows, columns;
   char *title;
{
   char *tunits, *tnames;
   int j, i;
   struct table *tbl;

   tbl = (struct table *)get_space(sizeof(struct table));

   tbl->rows = rows;
   tbl->columns = columns;

   tbl->title = (char *)get_space(strlen(title)+1);
   strcpy(tbl->title,title);

   tbl->col = 
        (struct column *)get_space(tbl->columns*sizeof(struct column));

   if (tbl->rows > 0) {
      for (j=0; j<tbl->columns; j++) {
         tnames = names + j*TempStrLen;
         tbl->col[j].name = (char *)get_space(strlen(tnames)+1);
         strcpy(tbl->col[j].name,tnames);

         tunits = units + j*TempStrLen;
         tbl->col[j].units = (char *)get_space(strlen(tunits)+1);
         strcpy(tbl->col[j].units,tunits);

         tbl->col[j].exponent = exponent[j];
         tbl->col[j].width = width[j];

         tbl->col[j].rows = tbl->rows;
         tbl->col[j].row = nr_dvector (0, tbl->col[j].rows);
         for (i=0; i<tbl->rows; i++)
            tbl->col[j].row[i] = (double)tmatrix[i][j];
      }
   }
   tbl->nexttbl = 0;

   return tbl;
}


void l_zero_temps (columns, rows, width, exponent,
                   units, names, tmatrix)
   int columns, rows;
   int *width, *exponent;
   char *units, *names;
   float **tmatrix;
{
   int i,j;
   char *tunits, *tnames;

   for (i=0; i<columns; i++) {
      width[i] = 0;
      exponent[i] = 0;
      tunits = units + i*TempStrLen;
      tnames = names + i*TempStrLen;
      memset (tunits,'\0',TempStrLen);
      memset (tnames,'\0',TempStrLen);
      for (j=0; j<rows; j++) {
         tmatrix[j][i] = 0;
      }
   }
}




void l_list_table (tbl)
  struct table *tbl;
{
  int i,j;
  char msg[LLength], svalue[TempStrLen], temp[TempStrLen], form[TempStrLen];


  memset (msg,'\0',LLength);
  sprintf (msg,"\n ");
  for (j=0; j<tbl->columns; j++) {
      if (strlen(tbl->col[j].name) > 0)
         strncat (msg, tbl->col[j].name, tbl->col[j].width+1);
      for (i=strlen(tbl->col[j].name); i<tbl->col[j].width+1; i++)
         strcat (msg," ");           
      strcat (msg," ");           
  }
  l_message (msg);

  memset (msg,'\0',LLength);
  sprintf (msg," ");
  for (j=0; j<tbl->columns; j++) {
      strncat (msg, tbl->col[j].units, tbl->col[j].width+1);
      for (i=strlen(tbl->col[j].units); i<tbl->col[j].width+1; i++)
         strcat (msg," ");           
      strcat (msg," ");
  }
  l_message (msg);
  l_message ("");

  for (i=0; i<tbl->rows; i++) {
     memset (msg,'\0',LLength);
     sprintf (msg,"  ");
     for (j=0; j<tbl->columns; j++) {
        memset (svalue,'\0',TempStrLen);
        memset (temp,'\0',TempStrLen);
        memset (form,'\0',TempStrLen);
        strcat (form,"%.");
        if (tbl->col[j].exponent) {
           sprintf (temp,"%d",tbl->col[j].width-6);
           strcat (form,temp);
           strcat (form,"e  ");
        }
        else {
           sprintf (temp,"%d",tbl->col[j].width);
           strcat (form,temp);
           strcat (form,"f  ");
	}
        sprintf (svalue,form,tbl->col[j].row[i]);

        strncat (msg,svalue,tbl->col[j].width);
        strcat (msg,"  ");
     }
     l_message (msg);
  }
}




void l_write_table (tbl,tablefile)
  struct table *tbl;
  char *tablefile;
{
  int i,j;
  char msg[LLength], svalue[TempStrLen], temp[TempStrLen], form[TempStrLen];

  FILE *wfile;


  wfile = fopen(tablefile,"w");


  memset (msg,'\0',LLength);
  sprintf (msg,"\n ");
  for (j=0; j<tbl->columns; j++) {
      if (strlen(tbl->col[j].name) > 0)
         strncat (msg, tbl->col[j].name, tbl->col[j].width+1);
      for (i=strlen(tbl->col[j].name); i<tbl->col[j].width+1; i++)
         strcat (msg," ");           
      strcat (msg," ");           
  }
  strcat (msg,"\n");           
  fprintf(wfile,msg);

  memset (msg,'\0',LLength);
  sprintf (msg," ");
  for (j=0; j<tbl->columns; j++) {
      strncat (msg, tbl->col[j].units, tbl->col[j].width+1);
      for (i=strlen(tbl->col[j].units); i<tbl->col[j].width+1; i++)
         strcat (msg," ");           
      strcat (msg," ");
  }
  strcat (msg,"\n");           
  fprintf(wfile,msg);
  fprintf(wfile,"   \n");

  for (i=0; i<tbl->rows; i++) {
     memset (msg,'\0',LLength);
     sprintf (msg,"  ");
     for (j=0; j<tbl->columns; j++) {
        memset (svalue,'\0',TempStrLen);
        memset (temp,'\0',TempStrLen);
        memset (form,'\0',TempStrLen);
        strcat (form,"%.");
        if (tbl->col[j].exponent) {
           sprintf (temp,"%d",tbl->col[j].width-6);
           strcat (form,temp);
           strcat (form,"e  ");
        }
        else {
           sprintf (temp,"%d",tbl->col[j].width);
           strcat (form,temp);
           strcat (form,"f  ");
	}
        sprintf (svalue,form,tbl->col[j].row[i]);

        strncat (msg,svalue,tbl->col[j].width);
        strcat (msg,"  ");
     }
     strcat (msg,"\n");           
     fprintf(wfile,msg);
  }
  fclose(wfile);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create table_subs.imake

#define SUBROUTINE table_subs

#define MODULE_LIST table_subs.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
