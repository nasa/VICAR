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
