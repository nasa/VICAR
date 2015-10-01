#include "VIDSdefs.h"
/* This file contains the source code for all the structure initialization
 * routines.
 */
/************************************************************************/
InitEnvironment(env)
    
  VIDSEnvironment	*env;
    
{
  int i;
  
  *env->message = '\0';		/* make sure message is an empty string	*/
  *env->key =  '\0';		/* make sure key is an empty string	*/
  env->devUnit = noUnit;
  env->isColor = FALSE;
  env->isBW = TRUE;
  env->isPseudo = FALSE;
  env->nimps = 0;
  env->nlMax = 0;
  env->nsMax = 0;
  env->nButtons = 0;

  env->bwIMP = 1;		/* set plane for bw display to first	*/
  env->redIMP = 1;		/* set IMP for red display		*/
  env->greenIMP = 2;		/* set IMP for green display		*/
  env->blueIMP = 3;		/* set IMP for blue display		*/
  env->grafIMP = 4;		/* set IMP for graphics display		*/
  env->isOn.red = env->isOn.green = 		/* all planes are on	*/
  env->isOn.blue = env->isOn.graf = True;
  
  for (i = 0; i < MAXPLANES; i++)  /* initialize image plane structures	*/
  {
    InitPlane(&env->planes[i]);
    env->planes[i].imp = i;
  }

  env->cursor.number = 1;	/* defaults of 0 for cursor attributes	*/
  env->cursor.form = 0;
  env->cursor.blink = 0;
  env->buffer = NULL;

  env->default_slope = 1.0;
  env->default_offset = 0.0;

  env->listData.oldvalid = False;

  env->profile.savednmin = 0;
  env->profile.savednmax = 0;
  env->profile.savexmax = 0;

  InitRegions(env);		/* init the region list */

  InitNames(env);		/* init the logical plane name table */

  InitMovie(env);		/* init JMOVIE state */

  InitTextAttr(&env->UserAttr);	/* initialize the text attributes */
  InitTextAttr(&env->SystemAttr);

  return;
}
/************************************************************************/
InitPlane(plane)		/* Initialize a PlaneInfo structure	*/

  PlaneInfo	*plane;
  
{
  plane->file = NULL;
  plane->band = 1;		/* default to first band		*/
  plane->imageWindow.sl = plane->imageWindow.ss = 1;
  plane->imageWindow.nl = plane->imageWindow.ns = 0;
  plane->subPixel.left = plane->subPixel.top = 0;
  plane->accessWindow.sl = plane->accessWindow.ss = 1;
  plane->accessWindow.nl = plane->accessWindow.ns = 0;
  plane->softZoom = 1;
  plane->hist.array = NULL;
  plane->hist.rgnseed = 0;
  plane->lut = NULL;
  plane->pstable = NULL;
  return;
}
/************************************************************************/
InitFile(file)

  FileInfo		*file;	/* file info structure to clear out	*/
  
{
  file->filename[0] = '\0';	/* Make sure filename is null string	*/
  file->fileUnit = noUnit;	/* No file unit yet			*/
  file->isOpen = FALSE;
  file->addr = NULL;
  file->nl = 0;
  file->ns = 0;
  file->nb = 0;
  file->org = BSQ;
  file->format = ByteFormat;
  file->scale.slope = 0.0;	/* use default range */
  file->scale.offset = 0.0;
}
