#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"
#include "rtlintproto.h"
#include <math.h>
#include <unistd.h>
#include <errno.h>

#if RTL_USE_COMPRESSION

/* masking array used in the algorithm to take out bits in memory */
const unsigned int cod1mask[25] = {0x0,0x1,0x3,0x7,0xf,0x1f,
                                   0x3f,0x7f,0xff,0x1ff,0x3ff,
                                   0x7ff,0xfff,0x1fff,0x3fff,
                                   0x7fff,0xffff,0x1ffff,
                                   0x3ffff,0x7ffff,0xfffff,
                                   0x1fffff,0x3fffff,0x7fffff,
                                   0xffffff};
/* pointer to the beginning of the buffer passed in to the algorithm */
unsigned char *pcode1;
/* bit position in the current byte of encrypted or decrypted buffer*/
int bit1ptr;

/*****************************************************/
/* This function gets the eoci (End Of Compressed    */
/* Image) from the unit table.                       */
/*****************************************************/
void getEOCI(int unit, unsigned long *eoci)
{
   *eoci = 0;
   *eoci += (unsigned int)(CURRENT_I_VALUE(EOCI1));
   *eoci += pow(2, 32) * (unsigned int)(CURRENT_I_VALUE(EOCI2));
}

/*****************************************************/
/* This function sets the eoci (End Of Compressed    */
/* Image) in the unit table.                         */
/*****************************************************/
void setEOCI(int unit, unsigned long eoci)
{
   CURRENT_I_VALUE(EOCI1) = (unsigned int)(eoci%(unsigned long)pow(2,32));
   CURRENT_I_VALUE(EOCI2) = (unsigned int)(eoci/pow(2,32));
}

/*****************************************************/
/* This function gets the size of eol labels from    */
/* a BASIC compressed file.                          */
/*****************************************************/
int v2_basic_get_eol_size(int unit)
{
   struct bufstate *bufstate;
   char *value, *ip, loc_buf[20];
   int vallen;
   unsigned long eoci;
   int eol_size;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   /* obtain the eoci (end of compressed image) byte offset */
   getEOCI(unit, &eoci);

   /* read 20 bytes just to get the LBLSIZE */
   V2_LSEEK(bufstate->devstate.dev.disk.channel, eoci, SEEK_SET);
   read(bufstate->devstate.dev.disk.channel, loc_buf, 20);

   loc_buf[19] = '\0' ; /* Make 'loc_buf' a c string.     */

   if (v2_find_entry(loc_buf, SYSTEM_KEY, &value, &vallen, &ip) != 0)
      eol_size = atoi(v2_string(value,vallen));
   else
      eol_size = 0;

   CURRENT_I_VALUE(EOL_SIZE) = eol_size;

   return SUCCESS;
}

/*****************************************************/
/* This function performs the necessary operations   */
/* on a BASIC compressed file label before closing.  */
/*****************************************************/
int basic_check_out_lbl_on_close(int unit)
{
   int lblsize, new_lblsize, eol_size = 0;
   int left;
   long lzind;
   char *labels, *eol_labels, *p;
   struct bufstate *state;
   /*   char tstbuf[600];*/

   eol_labels = NULL;
   lblsize = CURRENT_I_VALUE(LBLSIZE);
   labels = CURRENT_S_VALUE(LABELS);
   lzind = LAZY_INDEX(CURRENT_I_VALUE(NL));
   new_lblsize = strlen(labels);

   /* If more labels than initially allocated memory for, 
      eol needs to be written out as well.  Create eol by
      allocating memory and adding LBLSIZE, then blank out
      the eol section in the header labels.*/
   if(new_lblsize >= lblsize)	/* Must split off an EOL group */
   {
      p = v2_place_cut(labels, lblsize);	/* Get start of EOL section */

      if (strlen(p) != 0)	/* make sure there's something there! */
      {
         eol_labels = malloc(strlen(p) + LABEL_SIZE_ITEM + LABEL_BUFFER_EXTRA);
         if (eol_labels == NULL)		/* Create EOL label space */
            return NO_MEMORY_FOR_LABEL_PROCESS;

         memset(eol_labels, 0, LABEL_SIZE_ITEM);
         strcpy(eol_labels + LABEL_SIZE_ITEM, p); /* put labels there */
	 eol_size = strlen(p) + LABEL_SIZE_ITEM;
	 CURRENT_I_VALUE(EOL_SIZE) = eol_size;
         v2_add_label_size_item(eol_labels, eol_size);	/* put placeholder in */
         strcat(eol_labels, "  ");		/* leave space after last one */

         memset(p, 0, strlen(p));     /* blank out EOL labels in main section */
      }
   }

   if (eol_labels != NULL) 
      v2_set_eol(labels);
   else
      v2_clear_eol(labels);	/* make sure it's unset if the label shrunk! */

   new_lblsize = strlen(labels);

   /*   v2_add_label_size_item(labels, new_lblsize);*/

   state = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   /* Write out main labels */

   left = lblsize - new_lblsize;
   if (left > 0)    /* Zero out the part of the label that contains no data */
      memset(labels+new_lblsize, 0, left);

   V2_LSEEK(state->devstate.dev.disk.channel, 0, SEEK_SET);
   write(state->devstate.dev.disk.channel, labels, lblsize);

   /* Write out eol labels */
   if (eol_labels != NULL)
   {
      V2_LSEEK(state->devstate.dev.disk.channel, lzind, SEEK_SET);
      write(state->devstate.dev.disk.channel, eol_labels, eol_size);
   }

   if (eol_labels != NULL)
      free(eol_labels);

   return SUCCESS;
}

/*****************************************************/
/* This function gets the eol label from a BASIC     */
/* compressed file.                                  */
/*****************************************************/
int v2_basic_read_in_eol(int unit, char *p, int eol_size)
{
   struct bufstate *state;
   unsigned long eoci;

   state = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   /* read in eol labels */
   if(CURRENT_I_VALUE(EOL))
   {

      getEOCI(unit, &eoci);

      V2_LSEEK(state->devstate.dev.disk.channel, eoci, SEEK_SET);
      read(state->devstate.dev.disk.channel, p, eol_size);
   }

   return SUCCESS;
}

/*****************************************************/
/* This function  truncates the file if the file     */
/* was opened for WRITE or UPDATE.                   */
/* This function performs the functionalities of     */
/* v2_finish_file function but for BASIC compressed  */
/* files.                                            */
/*****************************************************/
int basic_finish_file(int unit)
{
   int status;
   V2_OFFSET j;
   unsigned long eoci;

   struct bufstate *bufstate;
   struct devstate *devstate;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   devstate = &bufstate->devstate;

   status = SUCCESS;
   if(!EQUAL(CURRENT_S_VALUE(OP), "READ"))
   {
#if FTRUNCATE_AVAIL_OS

      getEOCI(unit, &eoci);
      j = eoci + CURRENT_I_VALUE(EOL_SIZE);

      if (V2_FTRUNCATE(devstate->dev.disk.channel, j) != 0)
         status = errno;

#endif
   }

   return status;
}

/*****************************************************/
/* This function truncates the BASIC compressed      */
/* file if necessary.                                */
/* This function is similar to v2_close_file         */
/* function.                                         */
/*****************************************************/
int v2_basic_close_file(int unit)
{
   int status;
   struct bufstate *bufstate;
   struct devstate *devstate;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   devstate = &bufstate->devstate;

   status = basic_finish_file(unit);
   if(status != SUCCESS) return status;

   status = v2_close_os_file(unit);
   if(status != SUCCESS) return status;

   if(bufstate->buffer != NULL)
      free(bufstate->buffer);

   if(CURRENT_IP_VALUE(LABELS) != 0)
      free(CURRENT_IP_VALUE(LABELS));
   CURRENT_I_VALUE(LBLALLOC) = 0;

   CURRENT_I_VALUE(FLAGS) &= ~OPEN;

   return SUCCESS;
}

/*****************************************************/
/* This function performs all the functions that are */
/* performed by c_xvclose but for BASIC compressed   */
/* files.                                            */
/*****************************************************/
int v2_basic_close(int unit)
{
   int free_unit, status;
   struct bufstate *bufstate;
   struct devstate *devstate;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   devstate = &bufstate->devstate;

   /* On BASIC2, all the indices are stored right after the starting label.       */
   /* If the compression type is BASIC2 and the file was opened for modification  */
   /* then we need to write out the line indices into the file.                   */
   if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC2") && !EQUAL(CURRENT_S_VALUE(OP), "READ"))
   {
      unsigned char *lengths;
      int i;

      lengths = calloc(sizeof(int) * CURRENT_I_VALUE(NL), sizeof(int));
      for(i = 0; i < CURRENT_I_VALUE(NL); i++)
      {
	 unsigned int nBytes;

	 nBytes =  LAZY_INDEX(i+1) - LAZY_INDEX(i);
	 v2_set32BitInt(nBytes, lengths+i*sizeof(int));
      }

      V2_LSEEK(bufstate->devstate.dev.disk.channel, CURRENT_I_VALUE(LBLSIZE), SEEK_SET);
      write(bufstate->devstate.dev.disk.channel, lengths, sizeof(int) * CURRENT_I_VALUE(NL));

      free(lengths);
   }

   /* Calls for final modifications to the label before closing. */
   /* Similar to c_xvclose: line 25                              */
   if(EQUAL(CURRENT_S_VALUE(OP), "WRITE") || EQUAL(CURRENT_S_VALUE(OP), "UPDATE"))
   {
      status = v2_set_basic_label_params(unit);
      if(status != SUCCESS) return status;

      status = basic_check_out_lbl_on_close(unit);
      if(status != SUCCESS) return status;
   }

   /* Similar to c_xvclose: line 35 */
   status = v2_basic_close_file(unit);
   if(status != SUCCESS) return status;

   /* free BASIC specific global variables */
   free(CURRENT_IP_VALUE(LAZYINDEX));
   free(CURRENT_IP_VALUE(ENCODED_BUF));
   free(CURRENT_IP_VALUE(DECODED_BUF));

   /* Similar to c_xvclose: line 39 */
   if (v2_substr(CURRENT_S_VALUE(CLOS_ACT),"DELETE"))
      v2_delete_file(unit);

   free_unit = v2_substr(CURRENT_S_VALUE(CLOS_ACT),"FREE"); /* Save for later */

   v2_close_unit(unit);		/* Clear current table and active unit table. */
   v2_final_cleanup(unit);	/* To clear up all details. */

   if (free_unit)
      v2_deactivate_a_unit(unit);

   return SUCCESS;
}

/*****************************************************/
/* This function contains all the preconditions that */
/* the file must meet before allowing BASIC          */
/* compression.                                      */
/*****************************************************/
int v2_basic_precheck(int unit)
{
   struct devstate *devstate;

   devstate = &((struct bufstate*)CURRENT_IP_VALUE(BUFSTATE))->devstate;
   if(!EQUAL(CURRENT_S_VALUE(FORMAT), "BYTE") &&
      !EQUAL(CURRENT_S_VALUE(FORMAT), "HALF") &&
      !EQUAL(CURRENT_S_VALUE(FORMAT), "FULL"))
      return UNSUPPORTED_FMT_BY_COMPRESSION;

   if(!EQUAL(CURRENT_S_VALUE(ORG), "BSQ"))
      return UNSUPPORTED_ORG_BY_COMPRESSION;

   if(devstate->device_type != DEV_DISK)
      return UNSUPPORTED_DEVICE_BY_COMPRESSION;

   if(CURRENT_I_VALUE(NBB) != 0 || CURRENT_I_VALUE(NLB) != 0)
      return UNSUPPORTED_BIN_HDR_OR_PREFIX_BY_COMPRESSION;

   return SUCCESS;
}

/*****************************************************/
/* This function is a helper function for the BASIC  */
/* compression algorithm to acquire the value of a   */
/* 32 bit integer.  This was implemented so          */
/* that the number representation would be immune to */
/* endian differences.                               */
/*****************************************************/
int get32BitInt(unsigned char *ptr, unsigned int *value)
{
   *value = ptr[0];
   *value += pow(2,8)  * ptr[1];
   *value += pow(2,16) * ptr[2];
   *value += pow(2,24) * ptr[3];

   return SUCCESS;
}

/*****************************************************/
/* This function is a helper function for the BASIC  */
/* compression algorithm to acquire the value of a   */
/* 24 bit integer.  This was implemented so          */
/* that the number representation would be immune to */
/* endian differences.                               */
/*****************************************************/
int get24BitInt(unsigned char ptr[], unsigned int *val)
{
   *val = ptr[0];
   *val += ptr[1] * pow(2, 8);
   *val += ptr[2] * pow(2, 16);

   return SUCCESS;
}

/*****************************************************/
/* This function is a helper function for the BASIC  */
/* compression algorithm to set the value of a 32    */
/* bit integer to write out.  This was implemented so*/
/* that the number representation would be immune to */
/* endian differences.                               */
/*****************************************************/
int v2_set32BitInt(unsigned int len, unsigned char *ptr)
{
   int mod;

   ptr[3] = len/pow(2,24);
   mod = pow(2, 24);
   len = len % mod;
   ptr[2] = len/pow(2,16);
   mod = pow(2, 16);
   len = len % mod;
   ptr[1] = len/pow(2,8);
   mod = pow(2, 8);
   len = len % mod;
   ptr[0] = len;

   return SUCCESS;
}

/*****************************************************/
/* This function is a helper function for the BASIC  */
/* compression algorithm to set the value of a 24    */
/* bit integer to write out.  This was implemented so*/
/* that the number representation would be immune to */
/* endian differences.                               */
/*****************************************************/
int set24BitInt(unsigned int val, unsigned char *ptr)
{
   int mod;

   ptr[2] = val/pow(2,16);
   mod = pow(2, 16);
   val = val % mod;
   ptr[1] = val/pow(2, 8);
   mod = pow(2, 8);
   val = val % mod;
   ptr[0] = val;

   return SUCCESS;
}

/*****************************************************/
/* This function is a helper function for the BASIC  */
/* compression algorithm to get a specified number   */
/* of bits from pcode1(global pointer that points    */
/* to the encoded or decoded buf), convert it to a   */
/* number and return it inside val.                  */
/*****************************************************/
int grab1(unsigned char *val, int nbit)
{
   int shift;
   unsigned int v1,v2;
   shift = 8-nbit-(bit1ptr);

   if (shift>0)
   {
      *val = ((*(pcode1))>>shift)&cod1mask[nbit];
      bit1ptr += nbit;

      return SUCCESS;
   }
   if (shift<0)
   {
      v1 = (*((pcode1)++))&cod1mask[nbit+shift];
      v2 = ((*(pcode1))>>(8+shift))&cod1mask[-shift];

      *val = (v1<<(-shift))+v2;

      bit1ptr = -shift;

      return SUCCESS;
   }
   *val = (*((pcode1)++))&cod1mask[nbit];
   bit1ptr = 0;

   return SUCCESS;
}

/*****************************************************/
/* This function is the decoding algorithm for BASIC */
/* compression.  The encoded buffer is passed into   */
/* code and the decoded buffer is passed out in buf. */
/*****************************************************/
int basic_decode(unsigned char* code, unsigned char* buf, int ns, int wid)
{
   int iw,ip,ptop,i,old;
   unsigned char val;
   int runInt;
   unsigned char runChar;
   unsigned int nval;
   int cmprtrns1[7];

   nval = 999999;
   val = code[0];

   runInt = -3;

   pcode1 = code;
   bit1ptr = 0;

   old = 0;
   ptop = ns*wid;

   for (i=0;i<7;i++) cmprtrns1[i] = i-3;

   for(iw = 0; iw < wid; iw++)
   {
      for (ip=iw;ip<ptop;ip+=wid)
      {
	 if (runInt>(-3))
         {
            buf[ip] = (unsigned char)nval;
            runInt--;
            continue; 
         }
         grab1(&val,3);

         if (val<7)
         {
            nval = old+cmprtrns1[val];
            buf[ip] = (unsigned char)nval;
            old = nval;
            continue;
         }
         grab1(&val,1);

         if (val)
         {
            grab1(&runChar,4);
            if (runChar==15)
            {
               grab1(&runChar,8);

               if ((unsigned char)runChar==255)
               {
                  unsigned char ptr[3];
		  grab1(&runChar, 8);
		  ptr[0] = runChar;
		  grab1(&runChar, 8);
		  ptr[1] = runChar;
		  grab1(&runChar, 8);
		  ptr[2] = runChar;
		  get24BitInt(ptr, (unsigned int*) &runInt);
	       }
               else runInt = runChar + 15;
            }
	    else runInt = (unsigned char)runChar;

            grab1(&val,3);
            if (val<7) nval = old+cmprtrns1[val];
            else grab1((unsigned char*)(&nval), 8);
            buf[ip] = (unsigned char)nval;
            old = nval;
         }
         else
         {
            grab1(&val,8);
            buf[ip] = (unsigned char)val;
            old = val;
         }
      }
   }

   return SUCCESS;
}

/*****************************************************/
/* This function is a helper function for the BASIC  */
/* encoding operation.  It puts the value in val into*/
/* memory location pointed by pcode1+reg1 into nbit  */
/* number of bits.                                   */
/*****************************************************/
int emit1(unsigned char val, int nbit, unsigned char *reg1)
{
   int shift;

   shift = 8-nbit-bit1ptr;
   if (shift>0)
   {
      (*reg1) = (*reg1|(val<<shift));
      bit1ptr += nbit;

      return bit1ptr;
   }
   if (shift<0)
   {
      *(pcode1++) = (*reg1|(val>>(-shift)));
      *reg1 = (val<<(8+shift));
      bit1ptr = -shift;

      return bit1ptr;
   }
   *(pcode1++) = (*reg1|val);

   *reg1 = 0;
   bit1ptr = 0;

   return SUCCESS;
}

/*****************************************************/
/* This function is meat of the BASIC encoding       */
/* algorithm.  This function is called repeatedly by */
/* the basic_encode function to compress the data    */
/* according to its run length (run), last 2         */
/* different values (vold and old), and the current  */
/* value (val), into the memory location pointed by  */
/* pcode1+reg1.                                      */
/*****************************************************/
int basic_encrypt(int *run, int *old, int *vold, int val, unsigned char *reg1)
{
   if(*run<4)
   {
      if(abs(*old-*vold)<4)
         emit1((char)(*old-*vold+3),3,reg1);
      else
      {
         emit1((char)14,4,reg1);
	 emit1((char)(*old),8,reg1);
      }

      while(*run>1)
      {
         emit1((char)3,3,reg1);
         (*run)--;
      }

      *vold = *old; *old = val;
   }
   else
   {
      emit1((char)15,4,reg1);
      if (*run<19)
      {
	     emit1((char)(*run-4),4,reg1);
      }
      else
      {
         emit1((char)15,4,reg1);
	 if(*run<274)
	 {
	    emit1((char)(*run-19),8,reg1);
	 }
	 else
	 {
	    unsigned char ptr[3]; // added by pkim to account for endian change

	    emit1((char)255,8,reg1);

	    set24BitInt(*run-4, ptr); // added by pkim to account for endian change
	    emit1(ptr[0], 8, reg1); // added by pkim to account for endian change
	    emit1(ptr[1], 8, reg1); // added by pkim to account for endian change
            emit1(ptr[2], 8, reg1); // added by pkim to account for endian change
         }
      }
      if (abs(*old-*vold)<4)
      {
         emit1((char)(*old-*vold+3),3,reg1);
      }
      else
      {
         emit1((char)7,3,reg1);
         emit1((char)(*old),8,reg1);
      }
      *vold = *old; *old = val; *run = 1;
   }

   return SUCCESS;
}

/*****************************************************/
/* This function loops through the data given by     */
/* unencodedBuf, keeping track of run length.  When  */
/* the value of the data changes, it passes the run  */
/* length, last 2 differing values, the current      */
/* value, and the pointer in pcode1 buffer to encode */
/* the data into, to basic_encrypt function.         */
/*****************************************************/
int basic_encode(unsigned char *unencodedBuf, unsigned char *encodedBuf,
                 int ns, int wid, int *totBytes)
{
   int ip,ptop,run,iw,old,vold,i;
   int val = 0;
   unsigned char reg1;
   int bytecor1[32];

   bit1ptr=0;
   ptop = ns*wid;
   reg1 = 0;
   run = 0;
   old = unencodedBuf[0];

   vold = 999999;

   pcode1 = encodedBuf+4;
   bytecor1[0] = 0;
   for (i=1;i<32;i++) bytecor1[i] = i/8+1;

   for (iw=0;iw<wid;iw++)
      for (ip=iw;ip<ptop;ip+=wid)
      {
	  val = unencodedBuf[ip];

          if (val==old) run++;
	  else
	     basic_encrypt(&run, &old, &vold, val, &reg1);
      }

   /* purge of last code */
   basic_encrypt(&run, &old, &vold, val, &reg1);

   *pcode1 = reg1;

   *totBytes = pcode1 - encodedBuf;
   if(bit1ptr > 0) (*totBytes)++;

   v2_set32BitInt(*totBytes, encodedBuf);

   return SUCCESS;
}

/*****************************************************/
/* This function is called by basic_close and adds   */
/* required labels into the compressed image.        */
/* COMPRESS and EOCI (end of compressed image)       */
/* labels are added.                                 */
/*****************************************************/
int v2_set_basic_label_params(int unit)
{
   int status;
   char* label;
   unsigned long eoci;

   /* if label is not in local memory, get it */
   if (CURRENT_S_VALUE(LABELS) == NULL)
   {
      label = calloc(CURRENT_I_VALUE(LBLSIZE), sizeof(char));
      status = v2_read_in_labels(unit, &label, &CURRENT_I_VALUE(LBLALLOC));
      if (status != SUCCESS)
         return status;

      CURRENT_S_VALUE(LABELS) = label;
   }

   label = CURRENT_S_VALUE(LABELS);

   /* if compress label is not set, add it */
   if(strstr(label, "COMPRESS") == NULL)
      zladd(unit, "SYSTEM", "COMPRESS", &(CURRENT_S_VALUE(COMPRESS)), "FORMAT", "STRING", NULL);

   /* Get the last index for the EOCI (end of compressed image) label */
   /* The last index would only be -1 if the compression was BASIC    */
   /* and it had performed a read or update operation on the file.    */
   if(LAZY_INDEX(CURRENT_I_VALUE(NL)) == -1)
   {
      CURRENT_I_VALUE(IMG_REC) = CURRENT_I_VALUE(NL);
      v2_lazy_search(unit);
   }
   eoci = (unsigned long)(LAZY_INDEX(CURRENT_I_VALUE(NL)));

   /* delete EOCI labels if they exist and add a new ones */
   if((strstr(label, "EOCI1") != NULL))
   {
      status = zldel(unit, "SYSTEM", "EOCI1", NULL);
      if (status != SUCCESS)
         return status;
   }
   if((strstr(label, "EOCI2") != NULL))
   {
      status = zldel(unit, "SYSTEM", "EOCI2", NULL);
      if (status != SUCCESS)
         return status;
   }
   
   setEOCI(unit, eoci);
   zladd(unit, "SYSTEM", "EOCI1", &CURRENT_I_VALUE(EOCI1), "FORMAT", "INT", NULL);
   zladd(unit, "SYSTEM", "EOCI2", &CURRENT_I_VALUE(EOCI2), "FORMAT", "INT", NULL);
   CURRENT_S_VALUE(LABELS) = label;

   return SUCCESS;
}

/*****************************************************/
/* This function performs the necessary preprocessing*/
/* that is required by a BASIC compressed image.     */
/* It allocates the lazy index, encoded and decoded  */
/* buffers.                                          */
/*****************************************************/
int v2_basic_preprocess(int unit)
{
   int i, status;
   char *label;
   struct bufstate *state;

   state = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   CURRENT_IP_VALUE(LAZYINDEX) = malloc(sizeof(long)*(CURRENT_I_VALUE(NL)+1));
   if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC"))
      LAZY_INDEX(0) = (unsigned long)(CURRENT_I_VALUE(LBLSIZE));
   else if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC2"))
      LAZY_INDEX(0) = (unsigned long)(CURRENT_I_VALUE(LBLSIZE)+sizeof(int)*CURRENT_I_VALUE(NL));

   if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC2") && 
      (EQUAL(CURRENT_S_VALUE(OP), "READ") || EQUAL(CURRENT_S_VALUE(OP), "UPDATE")))
   {
      unsigned char *lengths;

      lengths = calloc(sizeof(int)*CURRENT_I_VALUE(NL), sizeof(int));
      V2_LSEEK(state->devstate.dev.disk.channel, CURRENT_I_VALUE(LBLSIZE), SEEK_SET);
      read(state->devstate.dev.disk.channel, lengths, sizeof(int)*CURRENT_I_VALUE(NL));
      for(i = 1; i <= CURRENT_I_VALUE(NL); i++)
      {
 	 unsigned int nBytes;

         get32BitInt(lengths+(i-1)*sizeof(int), &nBytes);
         LAZY_INDEX(i) = LAZY_INDEX(i-1) + nBytes;
      }

      free(lengths);
   }
   else
      for(i = 1; i <= CURRENT_I_VALUE(NL); i++) LAZY_INDEX(i) = -1;

   CURRENT_IP_VALUE(DECODED_BUF) = calloc(CURRENT_I_VALUE(PIX_SIZE) * CURRENT_I_VALUE(NS), sizeof(char));
   CURRENT_IP_VALUE(ENCODED_BUF) = calloc(CURRENT_I_VALUE(PIX_SIZE) * CURRENT_I_VALUE(NS) * 3/2 + 11, sizeof(char));

   status = v2_read_in_labels(unit, &label, &CURRENT_I_VALUE(LBLALLOC));
   if (status != SUCCESS)
      return status;

   CURRENT_S_VALUE(LABELS) = label;

   return SUCCESS;
}

/*****************************************************/
/* This function searches for the byte offset        */
/* location of the line number LINE in current_table */
/* Due to the file layout of BASIC compression       */
/* it searches line by line through the file and     */
/* stores the byte offset in lazy_index buffer.      */
/*****************************************************/
int v2_lazy_search(int unit)
{
   int i, status;
   unsigned char buf[4];
   struct bufstate *state;

   state = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   for(i = CURRENT_I_VALUE(IMG_REC); i > 0 && LAZY_INDEX(i) == -1; i--);
   for(; i < CURRENT_I_VALUE(IMG_REC) && i < CURRENT_I_VALUE(NL); i++)
   {
      unsigned int index;

      V2_LSEEK(state->devstate.dev.disk.channel, LAZY_INDEX(i), SEEK_SET);
      read(state->devstate.dev.disk.channel, buf, 4);
      status = get32BitInt(buf, &index);
      if(status != SUCCESS) return status;
      LAZY_INDEX(i+1) = index + LAZY_INDEX(i);
   }

   return SUCCESS;
}

/*****************************************************/
/* This function is called by basic read and write   */
/* routines to set the IMG_REC variable in the unit  */
/* table.  This function checks to see which line to */
/* read and to see if it is a valid line.            */
/*****************************************************/
int set_IMG_REC(int unit)
{
   if(CURRENT_I_VALUE(LINE) < 0) return IMPROPER_LINE_SIZE_PARAM;

   if(CURRENT_I_VALUE(LINE) == 0)
      ++CURRENT_I_VALUE(IMG_REC);
   else
      CURRENT_I_VALUE(IMG_REC) = CURRENT_I_VALUE(LINE);

   if(CURRENT_I_VALUE(IMG_REC) > CURRENT_I_VALUE(NL)) return END_OF_FILE;

   return SUCCESS;
}

/*****************************************************/
/* This function is called by c_xvread ->            */
/* compress_read_rec if a file is compressed with    */
/* BASIC algorithm.                                  */
/* This function reads a compressed line off the     */
/* disk, decompresses, and returns the decompressed  */
/* line in DECODED_BUF in the unit table.            */
/*****************************************************/
int v2_basic_read_rec(int unit, struct bufstate *state)
{
   unsigned char *encodedBuf, nBuf[4];
   unsigned char *decodedBuf;
   int status;
   unsigned int nBytes;
   int ns, wid;

   status = set_IMG_REC(unit);
   if(status != SUCCESS) return status;

   ns = CURRENT_I_VALUE(NS);
   wid = CURRENT_I_VALUE(PIX_SIZE);
 
   encodedBuf = (unsigned char*)CURRENT_IP_VALUE(ENCODED_BUF);
   decodedBuf = (unsigned char*)CURRENT_IP_VALUE(DECODED_BUF);

   /* if byte offset of the line to read is not indexed, search for it */
   if (LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)-1) == -1)
      status = v2_lazy_search(unit);
   V2_LSEEK(state->devstate.dev.disk.channel, LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)-1), SEEK_SET);

   /* read from file and decode */
   read(state->devstate.dev.disk.channel, nBuf, 4);
   status = get32BitInt(nBuf, &nBytes);
   if(status != SUCCESS) return status;

   read(state->devstate.dev.disk.channel, encodedBuf, nBytes);
   basic_decode(encodedBuf, decodedBuf, ns, wid);

   return SUCCESS;
}

/*****************************************************/
/* This function is called by c_xvread ->            */
/* compress_read_rec if a file is compressed with    */
/* BASIC2 algorithm.                                 */
/* This function reads a compressed line off the     */
/* disk, decompresses, and returns the decompressed  */
/* line in DECODED_BUF in the unit table.            */
/*****************************************************/
int v2_basic2_read_rec(int unit, struct bufstate *state)
{
  unsigned char *encodedBuf;
   unsigned char *decodedBuf;
   int status, nBytes;
   int ns, wid, lzind;

   status = set_IMG_REC(unit);
   if(status != SUCCESS) return status;

   ns = CURRENT_I_VALUE(NS);
   wid = CURRENT_I_VALUE(PIX_SIZE);
   lzind = LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)-1);

   encodedBuf = (unsigned char*)CURRENT_IP_VALUE(ENCODED_BUF);
   decodedBuf = (unsigned char*)CURRENT_IP_VALUE(DECODED_BUF);

   /* if BASIC2 compression format then index should never be -1 since */
   /* the indices are all initialized at the beginning                 */
   /*if(LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)-1) == -1) status = v2_lazy_search(unit);*/
   V2_LSEEK(state->devstate.dev.disk.channel, LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)-1), SEEK_SET);

   nBytes = LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)) - LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)-1);

   read(state->devstate.dev.disk.channel, encodedBuf, nBytes);
   basic_decode(encodedBuf, decodedBuf, ns, wid);

   return SUCCESS;
}

/*****************************************************/
/* This function checks the precondition for basic   */
/* write.                                            */
/*****************************************************/
int basic_writ_precondition(int unit)
{
   int samp, nsamps, ns;

   /* check to see requesting write in sequential order */
   /* We need to make sure the request is not for a line
      greater than the last unwritten line */
   if(CURRENT_I_VALUE(LINE) != 0 && CURRENT_I_VALUE(LINE) - CURRENT_I_VALUE(IMG_REC) > 1)
      return NON_SEQUENTIAL_WRITE;
   /* Check to ensure the request is not for a line that
      has already been written.*/
   if(CURRENT_I_VALUE(LINE) != 0 && CURRENT_I_VALUE(LINE) - CURRENT_I_VALUE(IMG_REC) < 0)
      return NON_SEQUENTIAL_WRITE;

   /* return error if trying to write in update mode */
   if(EQUAL(CURRENT_S_VALUE(OP), "UPDATE")) return NON_SEQUENTIAL_WRITE;

   /* return error if trying to compress and write only part of a line */
   samp = CURRENT_I_VALUE(SAMP);
   nsamps = CURRENT_I_VALUE(NSAMPS);
   ns = CURRENT_I_VALUE(NS);

   if(samp != 0 && samp != 1 &&
      nsamps != 0 && nsamps != ns) return NON_SEQUENTIAL_WRITE;

   return SUCCESS;
}

/*****************************************************/
/* This function is called by c_xvwrit ->            */
/* compress_writ_rec if a file is compressed with    */
/* BASIC algorithm.                                  */
/* This function compresses the given line and       */
/* writes the compressed line onto the disk.         */
/*****************************************************/
int v2_basic_writ_rec(int unit, struct bufstate *state)
{
   unsigned char* encodedBuf;
   unsigned char* decodedBuf;
   int status;
   int nBytes;
   unsigned long lzind;

   status = basic_writ_precondition(unit);
   if(status != SUCCESS)
      return status;

   status = set_IMG_REC(unit);
   if(status != SUCCESS) return status;

   /* get the byte offset location of the current line to read */
   lzind = LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)-1);

   /* encode and write to file */
   encodedBuf = (unsigned char*)CURRENT_IP_VALUE(ENCODED_BUF);
   decodedBuf = (unsigned char*)CURRENT_IP_VALUE(DECODED_BUF);

   basic_encode(decodedBuf, encodedBuf, CURRENT_I_VALUE(NS), CURRENT_I_VALUE(PIX_SIZE), &nBytes);
   LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)) = lzind + nBytes;

   V2_LSEEK(state->devstate.dev.disk.channel, lzind, SEEK_SET);
   write(state->devstate.dev.disk.channel, encodedBuf, nBytes);

   if(!(CURRENT_I_VALUE(FLAGS) & DATA_WRITTEN))
      CURRENT_I_VALUE(FLAGS) |= DATA_WRITTEN;

   return SUCCESS;
}

/*****************************************************/
/* This function is called by c_xvwrit ->            */
/* compress_writ_rec if a file is compressed with    */
/* BASIC2 algorithm.                                 */
/* This function compresses the given line and       */
/* writes the compressed line onto the disk.         */
/*****************************************************/
int v2_basic2_writ_rec(int unit, struct bufstate *state)
{
   unsigned char* encodedBuf;
   unsigned char* decodedBuf;
   int status;
   int nBytes;
   unsigned long lzind;

   status = basic_writ_precondition(unit);
   if(status != SUCCESS)
      return status;

   status = set_IMG_REC(unit);
   if(status != SUCCESS) return status;

   /* get the byte offset location of the current line to read */
   lzind = LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)-1);

   /* encode and write to file */
   encodedBuf = (unsigned char*)CURRENT_IP_VALUE(ENCODED_BUF);
   decodedBuf = (unsigned char*)CURRENT_IP_VALUE(DECODED_BUF);

   basic_encode(decodedBuf, encodedBuf, CURRENT_I_VALUE(NS), CURRENT_I_VALUE(PIX_SIZE), &nBytes);
   LAZY_INDEX(CURRENT_I_VALUE(IMG_REC)) = lzind + nBytes - sizeof(int);

   /* We add sizeof(int) to encodedBuf because we don't want to */
   /* write the length of the compressed line.  The BASIC2      */
   /* compression writes all the line lengths right after the   */
   /* file header.                                              */
   V2_LSEEK(state->devstate.dev.disk.channel, lzind, SEEK_SET);
   write(state->devstate.dev.disk.channel, encodedBuf + sizeof(int), nBytes);

   if(!(CURRENT_I_VALUE(FLAGS) & DATA_WRITTEN))
      CURRENT_I_VALUE(FLAGS) |= DATA_WRITTEN;

   return SUCCESS;
}

#else

int v2_set_basic_label_params(int UNUSED(unit))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_basic_precheck(int UNUSED(unit))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_basic_preprocess(int UNUSED(unit))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_basic_close(int UNUSED(unit))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_basic_read_rec(int UNUSED(unit), struct bufstate * UNUSED(state))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_basic2_read_rec(int UNUSED(unit), struct bufstate * UNUSED(state))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_basic_writ_rec(int UNUSED(unit), struct bufstate * UNUSED(state))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_basic2_writ_rec(int UNUSED(unit), struct bufstate * UNUSED(state))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_basic_get_eol_size(int UNUSED(unit))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_basic_read_in_eol(int UNUSED(unit), char * UNUSED(p),
							int UNUSED(eol_size))
{
   return INVALID_COMPRESSION_TYPE;
}

#endif

