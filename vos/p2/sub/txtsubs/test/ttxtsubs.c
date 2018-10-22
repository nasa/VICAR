#include <stdio.h>
#include "vicmain_c"
#include "ftnbridge.h"

#define TEXT_HEIGHT		25
#define TEXT_LENGTH		79
#define DN			35

/*  Program to test subroutines in TXTSUBS.                                   */

main44()
{
   int   lc, flag, def, status, temp, cnt;
   short char_height, line, sample, text_loc, dn;
   short text_height, text_length, lth, msg_len, font;
   char  image_block[TEXT_LENGTH*TEXT_HEIGHT], buf[256], out_msg[16];
   float scale, angle;

   zvparm("FONT", &temp, &cnt, &def, 1, 1);
   font = (short) temp;
   zvparm("MSG", out_msg, &cnt, &def, 1, 15);
   zvparm("HEIGHT", &temp, &cnt, &def, 1, 1);
   char_height = (short) temp;
   zvparm("SCALE", &scale, &cnt, &def, 1, 1);
   zvparm("ANGLE", &angle, &cnt, &def, 1, 1);
   zvparm("X", &temp, &cnt, &def, 1, 1);
   sample = (short) temp;
   zvparm("Y", &temp, &cnt, &def, 1, 1);
   line = (short) temp;
   zvparm("LOC", &temp, &cnt, &def, 1, 1);
   text_loc = (short) temp;
   msg_len = (short) strlen(out_msg);

   text_height = TEXT_HEIGHT;
   text_length = TEXT_LENGTH;
   dn = DN;

   for (lc=0; lc < text_length*text_height; lc++)
      image_block[lc] = 32;

   zvmessage("C Interface:", "");
   status = ztxtfont(font);
   if (status != 1) {
      sprintf(buf, "Error (%d) reading font file %03d.FON.", status, font);
      zvmessage(buf, "");
   }
   if (status == 1) {
      status = ztxtsize(char_height, scale);
      if (status != 1) {
         sprintf(buf, "Error (%d) setting text size.", status);
         zvmessage(buf, "");
      }
   }
   if (status == 1) {
      status = ztxtrotate(angle);
      if (status != 1) {
         sprintf(buf, "Error (%d) setting text angle.", status);
         zvmessage(buf, "");
      }
   }
   if (status == 1) {
      status = ztxtcolor(dn);
      if (status != 1) {
         sprintf(buf, "Error (%d) setting text color.", status);
         zvmessage(buf, "");
      }
   }
   if (status == 1) {
      status = ztxtlength(&lth, msg_len, out_msg);
      if (status != 1) {
         sprintf(buf, "Error (%d) getting length of text string.", status);
         zvmessage(buf, "");
      }
   }
   if (status == 1) {
      status = ztxttext(image_block, text_height, text_length, sample, line,
           text_loc, msg_len, out_msg, &flag);
      if (status != 1) {
         sprintf(buf, "Error (%d) writing text to image array.", status);
         zvmessage(buf, "");
      }
   }

   if (status == 1) {
      for (lc=0; lc < text_height; lc++) {
         memcpy(buf, &image_block[text_length*lc], text_length);
         buf[text_length] = '\0';
         zvmessage(buf, "");
      }
   }

   FTN_NAME(ttxtsubsf)(&text_height, &text_length, &char_height, &sample,
        &line, &scale, &dn, &angle, &text_loc, &font, out_msg, &msg_len,
        image_block);

   exit(0);
}
