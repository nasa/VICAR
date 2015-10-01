/*	xdgcolor - description
 *
 *	Purpose:
 *              Returns the DN value to use on the graphics plane to get
 *              the named color.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        July 26, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdgcolor( Unit, Name )
 *
 *	Parameter List:
 *
 *		Unit:     Display device unit number
 *              Name:     String of color names
 *
 *	Possible Error Codes:
 *
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdfuncs.h"

#define  BLACK		(colors & 0x80)

FUNCTION FTN_NAME(xdgcolor)( INTEGER Unit, STRING Name, ZFORSTR_PARAM )
   {
   ZFORSTR_BLOCK
   char		CString[256];

   zsfor2c(CString, 255, Name, &Unit, 2, 2, 1, Name);
   return( zdgcolor( *Unit, CString ) );
   }

FUNCTION zdgcolor( unit, name )
int	unit;
char	name[];
   {
   int	dn, R=0, G=0, B=0, maxval, legal_string;
   int	colors=0, transparent=0;

   if (!ZCHECK_UNIT_NUMBER) {
      dn = 0;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      dn = 0;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      dn = 0;
      }
   else if (!ZOVERLAY_AVAILABLE) {
      dn = 0;
      }
   else {
      legal_string = parse_name(name, &colors, &R, &G, &B, &transparent);
      if (!legal_string || transparent) {
         dn = 0;
         }
      else {
         if (!ZDEFAULT_OVERLAY_LUT) {
            maxval = MAX(R, MAX(G, B));
            if (maxval > 255) {
               R *= (255.0 / (double)maxval);
               G *= (255.0 / (double)maxval);
               B *= (255.0 / (double)maxval);
               }
            if (BLACK) {    /* if black selected, cut intensity in half */
               R /= 2;
               G /= 2;
               B /= 2;
               }
            dn = zdgrgb(unit, R, G, B);
            }
         else {             /* default overlay LUT in effect */
            if (ZBITS_IN_OVERLAY >= 8)
               dn = colors;
            else if (ZBITS_IN_OVERLAY >= 4)
               dn = ((R != 0) * 0x01) + ((G != 0) * 0x02) +
                    ((B != 0) * 0x04) + ((BLACK != 0) * 0x08);
            }
         }
      }
   return (dn);
   }

int parse_name(name, colors, R, G, B, transparent)
char	name[];
int	*colors, *R, *G, *B, *transparent;
   {
   int	i, stringptr=0, badstring=FALSE;
   char	c, tempstr[80];

   strcpy(tempstr, name);

   /*  Convert color name(s) to lower case  */
   for (i=0, c=tempstr[0]; c != '\0'; c=tempstr[++i])
      tempstr[i] = tolower(tempstr[i]);

   /*  Parse string until:                                           */
   /*  a)  end of string is reached or                               */
   /*  b)  invalid color name is read or                             */
   /*  c)  "transparent" color read.                                 */

   /*  (If "transparent" is passed in, the other color names become  */
   /*  irrelevant.)                                                  */

   c = tempstr[0];
   while ((c != '\0') && !badstring && !(*transparent)) {

      /*  If the default 8-bit graphics LUT is in effect, then we use  */
      /*  the "colors" mask to set the DN.  If the default LUT is not  */
      /*  in effect, or if the LUT is only four bits, then we use the  */
      /*  RGB values to set the DN.  This routines sets up both.       */

      if (strncmp(tempstr+stringptr,"red",3) == 0) {
         *colors = *colors | 0x01;
         *R += 255;
         }
      else if (strncmp(tempstr+stringptr,"green",5) == 0) {
         *colors = *colors | 0x02;
         *G += 255;
         }
      else if (strncmp(tempstr+stringptr,"blue",4) == 0) {
         *colors = *colors | 0x04;
         *B += 255;
         }
      else if (strncmp(tempstr+stringptr,"white",5) == 0) {
         *colors = *colors | 0x08;
         *R += 255;
         *G += 255;
         *B += 255;
         }
      else if (strncmp(tempstr+stringptr,"magenta",7) == 0) {
         *colors = *colors | 0x10;
         *R += 255;
         *B += 255;
         }
      else if (strncmp(tempstr+stringptr,"yellow",6) == 0) {
         *colors = *colors | 0x20;
         *R += 255;
         *G += 255;
         }
      else if (strncmp(tempstr+stringptr,"cyan",4) == 0) {
         *colors = *colors | 0x40;
         *B += 255;
         *G += 255;
         }
      else if (strncmp(tempstr+stringptr,"black",5) == 0) {
         *colors = *colors | 0x80;
         }
      else if (strncmp(tempstr+stringptr,"transparent",11)) {
         *transparent = TRUE;
         }
      else {
         badstring = TRUE;
         }

      if (!badstring && !(*transparent)) {
         /*  Parse the string until the next comma or space is located.  */
         for (c=tempstr[stringptr]; ((c != ',') && (c != ' ') && (c != '\0')); )
            c = tempstr[++stringptr];

         if (c != '\0') {
             /*  Parse the string to remove any additional white space.  */
            for (c=tempstr[stringptr]; ((c == ',') || (c == ' ')); )
               c = tempstr[++stringptr];

            }  /* end if */
         }  /* end if */
      }  /* end while */

   return(!badstring);
   }
