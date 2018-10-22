/****************************************************************************
 * Program GETLL (Get LINESAMP/LATLON)
 * 
 * This program will output the (LINE, SAMP) if input (LAT, LON) or
 *   output (LAT, LON) if input (LINE, SAMP); the 2 modes are distinguished
 *   by the presence of keyword 'TOLS (To LineSamp) meaning the first case.
 *
 * Hint: set window size to at least 90 column size to best view this file
 ****************************************************************************/

#include "vicmain_c"
#include "taeconf.inp"
#include "parblk.inc"
#include "pgminc.inc"
#include "spiceinc.h"
#include <string.h>

#define  SUCCESS  1

void main44(void)
{
  float MAP[40], CoordIn[2], CoordOut[2];
  int   Mode, Cnt, Def, Status, Dummy;

  switch (FillMAPBuffer(MAP))
  {
    case SUCCESS: break;
    case -1: case -2: case -3: case -4: case -5: case -6:
      zmabend("GETLL: Unable to determine MAP buffer for ZCONVEV, cannot continue!", "");
    default: 
      zvmessage ("GETLL: WARNING, unhandled return code for FillMAPBuffer", "");
      break;
  }

  zvparm ("COORDIN", CoordIn, &Cnt, &Def, 2, 0);

  if (zvptst("TOLS"))
  {
    Mode = 1;   /* CONVEV is to convert Lat/Lon to Line/Samp */
    zconvev(&Status, MAP, MAP, &CoordOut[0], &CoordOut[1],
                               &CoordIn[0],  &CoordIn[1], Mode, &Dummy);
  }
  else
  {
    Mode = 2;   /* CONVEV is to convert Line/Samp to Lat/Lon */
    zconvev(&Status, MAP, MAP, &CoordIn[0],  &CoordIn[1], 
                               &CoordOut[0], &CoordOut[1], Mode, &Dummy);
  }
  if (Status != 0)
    if (Mode == 1)
      zvmessage (" This LAT,LON coordinate is on other side of the planet","");
    else
      zvmessage (" This LINE,SAMP is off the planet", "");

  OutputCoord (Mode, CoordIn, CoordOut);
}

/*****************************************************************************
 * Function FillMAPBuffer
 * Purpose: Fill the input buffer with that of the standard float [40] MAP
 *          data buffer by obtaining information from GETSPICE2 and 
 *          GETCAMCON for CONVEV to be executed on geometrically corrected
 *          images (project type 8).  The function will also informs the user
 *          if it detects ZGETSPICE2 using different CKNAME than specified.
 *
 * Requirement: INP parameter from TAE must be a valid image file
 *              specification.
 *
 * Input  : None
 * Output : MAP data buffer for CONVEV to convert geometrically corrected
 *          images.  Please consult to CONVEV.HLP for the elements filled.
 * Return : Function execution status with the following codes:
 *             1  => Success
 *            -1  => File unit obtaining failure for INP
 *            -2  => File unit opening failure
 *            -3  => GETSPICE2 failed
 *            -4  => GETPROJ failed
 *            -5  => GETCAMCON failed
 *            -6  -> Project is not one of the type: GLL, VGR-1, VGR-2.
 *                   Theoretically, the program should work for all project
 *                     CONVEV can handle.  But such responsibility will not
 *                     be claimed unless needed, and unless other projects 
 *                     are tested.
 ****************************************************************************/
int FillMAPBuffer(
                  float MAP[40]   /* output */
                 )
{
  buf_union_typ SPICE;    /* Spice Buffer */
  char     Project[6];
  int      InUnit, Camera, Dum1, Dum2, Status, i;
  static int NO_ABEND = 0;
  char     MsgBuf[128], ActualCKName[LEN_SOURCE+1], 
           RequestedCKName[LEN_SOURCE+1];

  Status = zvunit(&InUnit, "INP", 1, NULL);
  zvsignal (InUnit,Status,NO_ABEND);
  if  (Status != SUCCESS) return -1;

  /* Get SPICE buffer */
  Status = zvopen(InUnit, "OP", "READ", NULL);
  zvsignal (InUnit, Status, NO_ABEND);
  if (Status != SUCCESS) return -2;

  if (zgetspice2(InUnit, 1, &SPICE) != SUCCESS)
  {
    zvmessage ("GETLL:ZGETSPICE2 failed.", "");
    return -3; 
  }

  /* Get Project and Camera S/N, also validates Project */
  zgetproj (InUnit, Project, &Camera, &Dum1, &Status);
  zvclose (InUnit,NULL);
  if (Status != 0) 
  {
    zvmessage ("GETLL:ZGETPROJ failed.", "");
    return -4;
  }
  else if (strncmp("GLL",Project,3) && strncmp("VGR-1",Project,5) && 
           strncmp("VGR-2", Project,5))
  {
    sprintf(MsgBuf,"GETLL: Input file is of invalid project type %s.",Project);
    zvmessage (MsgBuf, "");
    return -6;
  }

  /* Check requested and actual CKName used, if different inform the user */
  zvparm("CKNAME", RequestedCKName, &Dum1, &Dum2, 1, sizeof(RequestedCKName));
  for (i=0; RequestedCKName[i]; i++)
    RequestedCKName[i] = toupper(RequestedCKName[i]);

  if (!strncmp("GLL",Project,3))
  { /* for project "GLL", getspice2 only gives the Actual SOURCE in form of
     * CKID in intbuf[171], intbuf[10] contains the RequestedCKName only
     */ 
    if (CKID2CKName(SPICE.intbuf[0],&SPICE.intbuf[171], ActualCKName) < 0)
    { /* Fill with '?" if unknown */
      memset (ActualCKName, '?', LEN_SOURCE-1);
      ActualCKName[LEN_SOURCE-1] = '\0';
    }
  }
  else
  { /* for project Voyager, getspice will return the actual CKName used in
     * intbuf[10] exactly correct except SEDR is returned as "    "
     */
    memcpy (ActualCKName, &SPICE.intbuf[10],4);
    ActualCKName[4] = '\0';
    if (!strcmp(ActualCKName, "    ")) strcpy(ActualCKName, "SEDR");
  }
  if (strcmp(RequestedCKName, ActualCKName))
  {
    sprintf (MsgBuf, "CKName %s not available, %s used.", RequestedCKName,
             ActualCKName);
    zvmessage (MsgBuf, "");
  }

  /* Start Filling the MAP Buffer */
  zmve (8,9,&SPICE.doublebuf[58],MAP,1,1);         /* OM Matrix */
  zmve (8,3,&SPICE.doublebuf[21],&MAP[18],1,1);    /* RS Vector */

  MAP[24] = SPICE.doublebuf[14];            /* Polar Radius */
  MAP[25] = SPICE.doublebuf[12];            /* equatorial radius, */

  zgetcamcon (Project, Camera, &MAP[26], &MAP[27], &MAP[28],
                               &MAP[29], &Status);
  if (Status != 0)
  {
    zvmessage ("GETLL:ZGETCAMCON failed.", "");
    return -5;
  }

  MAP[37] = SPICE.doublebuf[26];      /* space to picture body center (KM) */
  Dum1 = 8;
  zmve(4, 1, &Dum1, &MAP[38], 1, 1); /* indicate Geometrically corrected */

  return SUCCESS; 
} 

/******************************************************************************
 * Function OutputCoord
 * Purpose: Output the processed coordinates to the standard console, and then
 *          the .PDF defined 2 dimensional real variable. 
 * Input: CONVEVMode => int, (1, or 2) refers to the mode that CONVEV was 
 *                      executed: LATLON to LINESAMP or LINESAMP to LATLON 
 *                      respectively.
 *        CoordIn    => float x 2, The input coordinate from the user.
 *        CoordOut   => float x 2, The output coordinate calculated by CONVEV
 * Output: none.
 * Return: none.
 *****************************************************************************/

OutputCoord (int CONVEVMode,    /* Input */
             float CoordIn[2],  /* Input */
             float CoordOut[2]  /* Input */
              )
{
  char MsgBuf[256];
  struct PARBLK ParBlk;
  double doubleout[2];

  /* Output to the standard console */
  zvmessage ("", "");
  if (CONVEVMode == 1)
    sprintf (MsgBuf, "(%f, %f) {LAT,LON} = (%f, %f) {LINE,SAMP}\n", 
             CoordIn[0], CoordIn[1], CoordOut[0], CoordOut[1]);
  else
    sprintf (MsgBuf, "(%f, %f) {LINE,SAMP} = (%f, %f) {LAT,LON}\n",
             CoordIn[0], CoordIn[1], CoordOut[0], CoordOut[1]);
  zvmessage (MsgBuf, "");

  /* Output to the TAE procedure's variables */
  doubleout[0] = CoordOut[0];
  doubleout[1] = CoordOut[1];
  q_init(&ParBlk,  P_BYTES, P_ABORT);
  q_real (&ParBlk, "COORDOUT", 2, doubleout, P_ADD);
  zvq_out (&ParBlk);
}

/*****************************************************************************
 * Function CKID2CKName
 * Purpose: Converts a C Kernel ID (CKID) to its common recognized name
 *          format (CKName)
 *
 * Method: This function works by opening the SPICE Kernel Database file
 *         for parsing.  If it finds CKID on any row's first column, then
 *         the 3rd column of the row is returned as the CKName
 *         
 * Usage : This function is useful in converting the 172nd element of the 4x200
 *         SPICE buffer returned from getspice95 to the common recognized SOURCE 
 *         name (DAVI, NAV, NAIF, etc)
 *
 * Input : SpacecraftID => (int), Spacecraft ID as returned by the 1st element
 *                         of the 4x200 SPICE buffer.
 *         CKID => (char []), C Kernel ID as returned by the 172nd element of
 *                 the 4x200 SPICE buffer.  The length of this "char array" is
 *                 defined by LEN_SPICE_ID in spiceinc.h. NOTE: this input is
 *                 not expected to be a "standard string" with NULL termination.
 *                 Thus one could simply pass the address of the 172nd element
 *                 of the SPICE buffer as getspice95 doesn't NULL terminate
 *                 any of its strings.
 * Output: CKName => (char []), Common recognized SPICE source name such as:
 *                     NAIF, AMOS, NEAR, NAV2, FAR, NAV, DAVI, etc.
 *                   the calling function should make sure to allocate the
 *                   string with length LEN_SOURCE+1 as defined in spiceinc.h
 *                   (+1 for NULL char)
 *                   This ouput will be NULL if function fails
 * Return: (int)  1 -> Success
 *               -1 -> Unable to obtain full path of the Kernel Database file
 *               -2 -> Unable to open Kernel Database file for read access
 *               -3 -> Kernel Database file format unknown
 *               -4 -> CKID not found in Kernel Database file
 *****************************************************************************/
int CKID2CKName (int  SpacecraftID,    /* Input  */
                 char CKID[],          /* Input  */
                 char CKName[]         /* Output */
                )
{
  FILE *KDbFILE;
  char KDbFname[256], KDbLine[256], ID[LEN_SPICE_ID+1], 
       Source[LEN_SOURCE+1];
  int  Dummy;

  CKName[0] = '\0';

  if (GetKernelDbFname(KDbFname, SpacecraftID) != SUCCESS) return -1;
  if ((KDbFILE=fopen(KDbFname, "r")) == NULL) return -2;

  while (!feof(KDbFILE))
  {
    if (fgets(KDbLine, 256, KDbFILE)==NULL) return -3;
    if (sscanf(KDbLine, "%s%i%s", ID, &Dummy, Source)!= 3) return -3;
    if (!strncmp(CKID,ID,LEN_SPICE_ID))
    {
      strcpy (&CKName[0], Source);
      break;
    }
  }

  fclose (KDbFILE);
  if (CKName)
    return SUCCESS;
  else
    return -4;
}
