/** Source code for:      vtiff.c
** The library of TIFF file routines used in vtiff is based on a public-
** domain suite of software, with additional enhancements for JPL use.
** The original source contains the following copyright notice:
**
** Tag Image File Format Library
**
** Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
** Copyright (c) 1991, 1992 Silicon Graphics, Inc.
** 
** Permission to use, copy, modify, distribute, and sell this software and 
** its documentation for any purpose is hereby granted without fee, provided
** that (i) the above copyright notices and this permission notice appear in
** all copies of the software and related documentation, and (ii) the names
** of Sam Leffler and Silicon Graphics may not be used in any advertising or
** publicity relating to the software without the specific, prior written
** permission of Stanford and Silicon Graphics.
** 
** THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
** EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
** WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
** 
** IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
** ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
** OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
** WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
** LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
** OF THIS SOFTWARE.
**
** Library-wide configuration defines:
**    MMAP_SUPPORT	add support for memory mapping read-only files
**    COLORIMETRY_SUPPORT add support for 6.0 colorimetry tags
**    JPEG_SUPPORT	add support for 6.0 JPEG tags & JPEG algorithms
**    YCBCR_SUPPORT	add support for 6.0 YCbCr tags
**    CMYK_SUPPORT	add support for 6.0 CMYK tags
**
** Compression configuration defines:
**    CCITT_SUPPORT	add support for CCITT Group 3 & 4 algorithms
**    PACKBITS_SUPPORT	add support for Macintosh PackBits algorithm
**    LZW_SUPPORT	add support for LZW algorithm
**    THUNDER_SUPPORT	add support for ThunderScan 4-bit RLE algorithm
**    NEXT_SUPPORT	add support for NeXT 2-bit RLE algorithm
**    JPEG_SUPPORT	add support for JPEG DCT algorithm
**
**/

/* 03oct2011 -lwk- changed some of the variables returned by TIFFGetField
		from int to tiff_u_long as that's what it expects (they were
		overrunning their buffer) */

#include "tiffio.h"
#include "vicmain_c"
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>

#ifndef MIN
#define MIN(x,y) ((x) < (y) ? (x) : (y))
#endif

void tiff_to_vic(void);
void vic_to_tiff(void);
void WriteTIFFClut(TIFF *tif);
void WriteFileFromTIFF(char *parm,int instance,TIFF *tif);
void SetUpVicarFile(char *parm,int instance,TIFF *in,int outunit[],
			int *nl,int *ns,int *nb);
int WriteVicarData(int unit[3],TIFF *tif,int nl,int ns,int nb);

void main44(void)
{   
    int count,def;
    char command[12];
    
    zvparm("_SUBCMD",command,&count,&def,0,0);
    switch (toupper(command[0]))
    {
      case 'F' : vic_to_tiff();
               break;
      case 'T' : tiff_to_vic();
               break;
    }
}

static int first=1;

PrintHandler(char *fd,char *fmt,va_list ap)
{
	static char message[132];
	char txt[132];
	int len;

	if (first)
	{
	   first=0;
           message[0]='\0';
	}

        vsprintf(txt, fmt, ap);
	strcat(message,txt);
	len=strlen(message);
	if (message[len-1]=='\n')
	{
	  message[len-1]='\0'; 
          zvmessage(message, " ");
	  message[0]='\0';
        }
}

AbortMsg(message)
char *message;
{
      zvmessage(message,"");
      zabend();
}

/*********************************************************************/
/*********************** tiff_to_vic        **************************/
/*********************************************************************/

void tiff_to_vic(void)
{
      int i,count;
      TIFF  *in;
      char inname[133];

/* open input TIFF file */
      zvselpi(0);
      zvpone("INP", inname, 1, 132);
      in = TIFFOpen(inname, "r");
      if (in == NULL) AbortMsg("Error opening TIFF file");

      if (zvptst("DUMP"))
      {
        TIFFSetPrintHandler( (TIFFPrintHandler)PrintHandler );
	TIFFPrintDirectory(in,0L,TIFFPRINT_NONE);
      }

      GetTIFFGeoreference(in);

/* If desired, convert to lookup table and write out to file */
      zvpcnt("OUT",&count);
      if (count==2) WriteTIFFClut(in);

/* convert TIFF to output file */
      zveaction("SA", "");
      WriteFileFromTIFF("OUT",1,in);      


/* add auxilliary files, if any */
      zvpcnt("AUXIL",&count);
      for (i=0; i<count; i++)
      {
            if (!TIFFReadDirectory(in))
                  AbortMsg("Not enough images in TIFF file for auxilliaries");
            WriteFileFromTIFF("AUXIL",i+1,in);
      }
      
/* close up shop */
      TIFFClose(in);


}

void WriteFileFromTIFF(char *parm,int instance,TIFF *tif)
{
      int  unit[3],nl,ns,nb,i;

      SetUpVicarFile(parm,instance,tif,unit,&nl,&ns,&nb);
      if (!WriteVicarData(unit,tif,nl,ns,nb))
            AbortMsg("Error in Copying data to TIFF ");

      for (i=0; i<nb; i++)
	      zvclose(unit[i], NULL);
}


void SetUpVicarFile(char *parm,int instance,TIFF *in,int outunit[],
			int *nl,int *ns,int *nb)
{
      int i;
      uint16_t nbs;
      char name[132];
      int numout;
      long tifinfo;

      zvpcnt("OUT", &numout);
     
/* get info from TIFF file */
      TIFFGetField(in,TIFFTAG_IMAGEWIDTH,&tifinfo);
      *ns = tifinfo;
      TIFFGetField(in,TIFFTAG_IMAGELENGTH,&tifinfo);
      *nl = tifinfo;
      if (!TIFFGetField(in, TIFFTAG_SAMPLESPERPIXEL, &nbs))
        nbs=1;
      *nb = nbs;
      if (*nb < 1) *nb=1;
      if (*nb > 3) *nb=3;
      if (*nb==3 && numout<3)
      {
	   zvmessage("Cannot compress RGB TIFF to 8-bit"," ");
	   zabend();
      }

      zvselpi(0);  /* INP is not a VICAR file */

      for (i=0;i<*nb;i++)
      {
	      zvpone(parm, name, instance+i, 132);
	      zvunit(outunit+i, "xxx", instance+i,"U_NAME",name, NULL);
	      
		/* Open up VICAR file with proper size */
	
	      zvopen(outunit[i],"OP","WRITE","U_NL",*nl,"U_NS",*ns, NULL);
	      
		/* Reopen in UPDATE for random access */
	      zvclose(outunit[i], NULL);
	      zvopen(outunit[i],"OP","UPDATE", NULL);
      }
}


int WriteVicarData(int unit[3],TIFF *tif,int nl,int ns,int nb)
{
      uint8_t *buf,*lbuf[3],*bptr[3],*ptr;
      uint32_t x,y,y1,y2,z;
      uint16_t planar;
      int nsamps;
      int tilebufsize;
      int tilepix;
      tiff_u_long tile_width,tile_height;
      int scanbytes;
      int band;
      int nrows;
      int chunky;
      int i;
      int plane_inc;
      int tile,tile1;
      long (*_tiff_read)();

      TIFFGetField( tif, TIFFTAG_PLANARCONFIG, &planar);
      chunky = (planar == PLANARCONFIG_CONTIG);

	
	if (TIFFIsTiled(tif))
	{
		plane_inc = TIFFComputeTile(tif, 0,0,0,1)
				- TIFFComputeTile(tif, 0,0,0,0);
		TIFFGetField(tif, TIFFTAG_TILEWIDTH, &tile_width);
		TIFFGetField(tif, TIFFTAG_TILELENGTH, &tile_height);
		tilebufsize = TIFFTileSize(tif);
		_tiff_read = TIFFReadEncodedTile;
	}
	else
	{
		plane_inc = TIFFComputeStrip(tif, 0, 1)
				- TIFFComputeStrip(tif, 0, 0);
		TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &tile_width);
		TIFFGetField(tif, TIFFTAG_ROWSPERSTRIP, &tile_height);
		tilebufsize = TIFFStripSize( tif );
		_tiff_read = TIFFReadEncodedStrip;
	}
	tilepix = tile_width*tile_height;
	buf = (uint8_t *)malloc(tilebufsize);
	if (chunky)
		lbuf[0] = (uint8_t *)malloc(tilebufsize);
	else
		lbuf[0] = (uint8_t *)malloc(tilebufsize*nb);
	for (i=1;i<nb;i++) 
		lbuf[i] = lbuf[0]+ i*tile_width*tile_height;
	if (!buf || !lbuf[0]) 
		AbortMsg("failed to allocate buffer for tile/strip");
	
	tile=0;
	for (y = 0; y < nl; y+=tile_height)
	{
		nrows = MIN(tile_height, nl - y);
		for (x = 0; x < ns; x+=tile_width)
		{
			memset(buf,0,(size_t)tilebufsize);
			if (chunky)
			{
				if (_tiff_read(tif, tile++, buf, tilebufsize) < 0)
					goto bad;
				/* reshuffle pixels into chunky: rgbrgb...*/
				ptr=buf;
				for (i=0;i<nb;i++) bptr[i] = lbuf[i];
				for (z=0; z<tilepix; z++)
					for (band=0;band<nb;band++)
						*bptr[band]++ = *ptr++;
			}
			else /* interleave by tile */
			{
				tile1=tile++;
				for (band=0; band<nb; band++)
				{
					if (_tiff_read(tif, tile1, lbuf[band], tilebufsize) < 0)
						goto bad;
					tile1 += plane_inc;
				}
			}

			/* write out a single tile */
			nsamps = MIN(tile_width, ns - x);
			for (y1=0,y2=0; y1< nrows; y1++)
			{
				for (band=0;band<nb;band++)
				{
					zvwrit(unit[band],lbuf[band]+y2,
					"line",y+y1+1, "samp",x+1,"nsamps", nsamps, NULL);
				}
				y2 += tile_width;
			}
		}
	}
      
      free(buf);
      free(lbuf[0]);
      return (1);
bad:
      free(buf);
      free(lbuf[0]);
      return (0);

}


/*********************************************************************/
/*********************** vic_to_tiff        **************************/
/*********************************************************************/


void vic_to_tiff(void)
{
      int i,count;
      TIFF  *out;
      char outname[133];
      char tiffmode[10];
      
/* open output TIFF file */
      zvpone("out", outname, 1, 132);
      out = TIFFOpen(outname, "w");
      if (out == NULL) AbortMsg("Error opening TIFF file");

/* Put application-specific Resource data in description tag: */
      AddTIFFGeoreference(out);

/* convert input file to TIFF */
      zveaction("SA", "");
      zvp("TIFFMODE",tiffmode,&count);
      AddFiletoTIFF("INP",out,tiffmode); 

/* add auxilliary files, if any, using strips */
      zvpcnt("AUXIL",&count);
      if (count > 0)
      {
            TIFFWriteDirectory(out);
            AddFiletoTIFF("AUXIL",out,"strips");
      }
      
/* close up shop */
      TIFFClose(out);
}


GetTIFFGeoreference(in)
TIFF *in;
{
	long Lat,Long;
	long vPix[2];
	long hPix[2];
	long xPos;
	long yPos;
	int count;
	int projType;
	float hres,vres;

	  if (TIFFhasCartoTags(in))
	  {
		  /*
		   * new stuff -- These are "CARTTAG" because they
		   * are placed only in a private TIFF IFD and are not
		   * registered (though the file offset to IFD is).
		   */

		  /* Should be PROJECTIONTYPE_UTM, :*/
		  TIFFGetField(in, CARTTAG_PROJECTIONTYPE,&projType);
		  TIFFGetField(in, CARTTAG_PROJ_XPOS,&xPos);
		  TIFFGetField(in, CARTTAG_PROJ_YPOS,&yPos);
		  TIFFGetField(in, CARTTAG_LATITUDE,&Lat);
		  TIFFGetField(in, CARTTAG_LONGITUDE,&Long);
		  TIFFGetField(in, CARTTAG_XPIXPERANGLE,&hres);
		  TIFFGetField(in, CARTTAG_YPIXPERANGLE,&vres);
		  hres *= 36e5;
		  vres *= 36e5;
		  hPix[0] = hres;
		  hPix[1] = (hres - (double)hPix[0]) * (uint32_t)0xFFFFFFFF;
		  vPix[0] = vres;
		  vPix[1] = (vres - (double)vPix[0]) * (uint32_t)0xFFFFFFFF;
	  }

	/** should write out to PROPERTY label **/

}

/* convert deg-min-sec to thousanths of second */

double DMSToSec1000(angl,count)
float angl[3];
int count;
{
	register double s1000=0.0;
	enum {deg,min,sec};
	
	switch (count-1)
	{
		case sec:
			s1000+=angl[sec]*1e3;  /* fall through */
		case min:
			s1000+=angl[min]*6e4;  /* fall through */
		case deg:
			s1000+=angl[deg]*36e5;
			break;
		default: /* default value is 1.0 */
			s1000=1.0;
			break;
	}
	
	return s1000;
}

#define EQUATOR 90L*60*60*1000
#define PRIME_MERIDIAN 180L*60*60*1000

AddTIFFGeoreference(out)
TIFF *out;
{
	float theLat[3],theLong[3];
	float hDegPix[3],vDegPix[3];
	uint32_t lat1000sec,long1000sec;
	uint32_t vPix[2];
	uint32_t hPix[2];
	uint32_t xPos,yPos;
	char cart_string[500];
	int def,count,location_set;
	double hres,vres;

	/** look for PROPERTY label, else get from parms **/
	
	  zvparm("LAT",theLat,&count,&def,0,0);
	  location_set = (!def);
	  if (location_set)
	  {
		  lat1000sec=DMSToSec1000(theLat,count);
		  if (zvptst("NORTH"))
			lat1000sec=EQUATOR+lat1000sec;
		  else
			lat1000sec=EQUATOR-lat1000sec;
			
		  zvp("LONG",theLong,&count);
		  long1000sec=DMSToSec1000(theLong,count);
		  if (zvptst("EAST"))
			long1000sec=PRIME_MERIDIAN+long1000sec;
		  else
			long1000sec=PRIME_MERIDIAN-long1000sec;
			
		  /* hres is in pixels per thousandths of second*/
		  zvp("HDEGPIX",hDegPix,&count);
		  hres = (double) 1.0 / DMSToSec1000(hDegPix,count);
		  zvp("VDEGPIX",vDegPix,&count);
		  vres = (double) 1.0 / DMSToSec1000(vDegPix,count);
	
		  /* hpix are in pixels per degree */
		  hPix[0] = hres*36e5;
		  hPix[1] = (hres*36e5 - (double)hPix[0]) * (uint32_t)0xFFFFFFFF;
		  vPix[0] = vres*36e5;
		  vPix[1] = (vres*36e5 - (double)vPix[0]) * (uint32_t)0xFFFFFFFF;
	
		  zvp("XPIXPOS",&xPos,&count);
		  zvp("YPIXPOS",&yPos,&count);

		  
		  /*
		   * Old method of storing info - for backward compatibility
		   */
		  sprintf(cart_string,
				"CART_RESOURCE(%d,%ld,%ld,%ld,%lu,%ld,%lu,%ld,%ld)",
				 0,long1000sec,lat1000sec,hPix[0],hPix[1],
				   vPix[0],vPix[1],xPos,yPos);
		  TIFFSetField(out, TIFFTAG_IMAGEDESCRIPTION,cart_string);
		  
		  /*
		   * new stuff -- These are "CARTTAG" because they
		   * are placed only in a private TIFF IFD and are not
		   * registered.
		   */
		  TIFFSetField(out, CARTTAG_PROJECTIONTYPE,PROJECTIONTYPE_UTM);
		  TIFFSetField(out, CARTTAG_PROJ_XPOS,xPos);
		  TIFFSetField(out, CARTTAG_PROJ_YPOS,yPos);
		  TIFFSetField(out, CARTTAG_LONGITUDE,long1000sec);
		  TIFFSetField(out, CARTTAG_LATITUDE,lat1000sec);
		  TIFFSetField(out, CARTTAG_XPIXPERANGLE,hres);
		  TIFFSetField(out, CARTTAG_YPIXPERANGLE,vres);
	  }
}

AddFiletoTIFF(parm,tif,mode)
char *parm;
TIFF *tif;
char *mode;      /* "tiled" or "Strips" */
{
      int  unit[3];
      int i,nl,ns,nb,numinp,status;
      int bit8;
      char name[201];

      zvpcnt(parm, &numinp);
      bit8 = zvptst("bit8");
     
   
      for (i=0; i<numinp; i++)
      {
          zvpone(parm, name, i+1, 200);
          zvunit(unit+i, parm, i+1,"U_NAME",name, NULL);
          status = zvopen(unit[i],"OP","READ","U_FORMAT","BYTE", NULL);
      }

      nb = (numinp==3) ? 3 : 1;
      
      if (numinp==3 && bit8)
      {
      		ConvertToLut( unit );
      		nb = 1;
      		numinp=2;
      }
      
      switch (*parm)
      {
	      case 'i':case 'I':
	         TIFFSetField(tif, TIFFTAG_SUBFILETYPE, 0);
	         break;
	      default:
	         TIFFSetField(tif, TIFFTAG_SUBFILETYPE, FILETYPE_REDUCEDIMAGE);
	      break;
      }

      SetUpTiffDirectory(unit,tif,mode,&nl,&ns,nb,numinp);
      if (!WriteTIFFData(unit,tif,nl,ns,nb))
            AbortMsg("Error in Copying data to TIFF ");
	    
      for (i=0; i<nb; i++)
          zvclose(unit[i], NULL);
}

#define MAX_DIM 512

int ConvertToLut( unit )
int unit[];
{
	int tempunit[2];
	int i;
	int line,samp=0;
	int status=1;
	int lineinc,sampinc;
	int nl,ns,numcolors,size;
	unsigned char *lbuffer[3];
	unsigned char *index;
	unsigned char *red,*grn,*blu,*iptr;
	int *table=(int *)0;
	
     	zvget(unit[0],"NL",&nl,"NS",&ns, NULL);
     	lineinc = (nl + MAX_DIM-1)/MAX_DIM;
     	sampinc = (ns + MAX_DIM-1)/MAX_DIM;
	while (!(ns % sampinc)) sampinc++;
	
	/* Create a temporary index and LUT file */
	
	status = zvunit( tempunit+0, "xxx",5,"u_name","vtiff_temp_img", NULL);
	status = zvopen( tempunit[0], "op", "write","u_nl",nl,"u_ns",ns, NULL);

	status = zvunit( tempunit+1, "xxx",6,"u_name","vtiff_temp_lut", NULL);
	status = zvopen( tempunit[1], "op", "write","u_nl",1,"u_ns",1024, NULL);
	
	/* allocate memory */
	
	if (!table_new( &table, 254 ))
	{
		zvmessage("Memory error creating color table"," ");
		zabend();
	}
	size = (ns<256) ? 256 : ns;
	for (i=0;i<3;i++)
	{
		lbuffer[i] = (unsigned char *)malloc(size);
		if (!lbuffer[i])
		{
			zvmessage("Memory error allocating LUT buffer"," ");
			zabend();
		}
	}
	index = (unsigned char *)malloc(size < 1024 ? 1024 : size);
	if (!index) 
	{
		zvmessage("Memory error creating index table"," ");
		zabend();
	}

	
	/* run through image & collect colors */
	
	for (line=1; line<=nl; line+=lineinc)
	{
		for (i=0;i<3;i++)
			zvread(unit[i],lbuffer[i],"line",line, NULL);
		
		red = lbuffer[0]; grn=lbuffer[1]; blu=lbuffer[2];
		numcolors=0;
		for (samp=samp%ns; samp<ns; samp+=sampinc,numcolors++)
		{
			red[numcolors] = red[samp];
			grn[numcolors] = grn[samp];
			blu[numcolors] = blu[samp];
		}
		
	        table_add_colors(table, red, grn, blu, numcolors );

	}
	
	table_build( table );  /* computes optimal color table */
	
	/* write out INDEX image */
	red = lbuffer[0]; grn=lbuffer[1]; blu=lbuffer[2];
	for (line=1; line<=nl; line++)
	{
		for (i=0;i<3;i++)
			zvread(unit[i],lbuffer[i],"line",line, NULL);

      		table_rgb_to_index( table, red, grn, blu, ns, index );
      		zvwrit( tempunit[0], index, NULL);
	}
	
	/* write out LUT */
	table_extract_lut( table, red, grn, blu);
	iptr = index;
	memset(index, 0, 1024);
	for (i=0;i<256;i++)
	{
		*iptr++ = *red++;
		*iptr++ = *grn++;
		*iptr++ = *blu++;
		iptr++;
	}
	zvwrit( tempunit[1], index, NULL);

	/* close old files * free up memory */
	for (i=0;i<3;i++)
	{
		zvclose( unit[i], NULL);
		free(lbuffer[i]);
	}
	free (index);
    	table_free( table );
	
	/* reset units */
	status = zvclose( tempunit[0], NULL);
	status = zvopen( tempunit[0], "op", "read","clos_act", "delete", NULL);
	status = zvclose( tempunit[1], NULL);
	status = zvopen( tempunit[1], "op", "read","clos_act", "delete", NULL);
	unit[0] = tempunit[0];
	unit[1] = tempunit[1];
	
	return status;
}

/* This will probably do for initializing most TIFF files */

SetUpTiffDirectory(inunit,out,mode,nl,ns,nb,numinp)
int inunit[];
TIFF *out;
char *mode;      /* "tiled" or "Strips" */
int *nl;
int *ns;	 /* output: sizes */
int nb;		 /* input : number of bands */
int numinp;	/* number of VICAR units in inunit */
{
      int count,def,tiled,status,i;
      tiff_u_long tile_width,tile_height;
      int resunit;
      float xresolution,yresolution;
      char *tiff_time();
      uint16_t clut[3][256];
      uint16_t compression;

      tiled = (toupper(*mode) == 'T');

      resunit = zvptst( "INCH") ? RESUNIT_INCH : RESUNIT_CENTIMETER;
      zvp( "XRES", &xresolution, &def); 
      zvp( "YRES", &yresolution, &def); 
      TIFFSetField(out, TIFFTAG_RESOLUTIONUNIT, resunit);
      TIFFSetField(out, TIFFTAG_XRESOLUTION, (double)xresolution);
      TIFFSetField(out, TIFFTAG_YRESOLUTION, (double)yresolution);

/* Initial tags which are always the same for our applications */
      TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, 8);

 
/* Compression */
      if (zvptst("LZW"))
      		compression = COMPRESSION_LZW;
      else if (zvptst("PACKBITS"))
      		compression = COMPRESSION_PACKBITS;
      else               
      		compression = COMPRESSION_NONE;

      TIFFSetField(out, TIFFTAG_COMPRESSION, compression);

/* Tiling parameters */
	zvp("TLINES",&tile_height,&count);
	zvp("TSAMPS",&tile_width,&count);
      if (tiled)
      {
	    if (tile_height%8 || tile_width%8)
	    {
	    	zvmessage("Tile size must be a multiple of 8"," ");
		zabend();
	    }
            TIFFSetField(out, TIFFTAG_TILEWIDTH, tile_width);
            TIFFSetField(out, TIFFTAG_TILELENGTH, tile_height);
      }
	else
	  {
      		TIFFSetField(out, TIFFTAG_ROWSPERSTRIP, tile_height);
	  }      

      
/* Set width,length, and bands (called "samples" in TIFF) */
      zvget(inunit[0],"NL",nl,"NS",ns, NULL);

      TIFFSetField(out, TIFFTAG_IMAGEWIDTH,(uint32_t) *ns);
      TIFFSetField(out, TIFFTAG_IMAGELENGTH,(uint32_t) *nl);


/* Name of the program creating this tiff file: */
      TIFFSetField(out, TIFFTAG_SOFTWARE, "VICAR Program VTIFF");

/* Set the TIFF standard date-time */
      TIFFSetField(out, TIFFTAG_DATETIME, tiff_time());

/* set the color type and color map, if any */
      if (zvptst( "chunky" ))
          TIFFSetField(out, TIFFTAG_PLANARCONFIG,PLANARCONFIG_CONTIG);
      else
          TIFFSetField(out, TIFFTAG_PLANARCONFIG,PLANARCONFIG_SEPARATE);
	  
      TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, nb);

      if (nb==1)
      {
      	  if (numinp == 2)
	  {
      	      TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_PALETTE);
	      SetTIFFClut(clut,inunit[1],"DiskFile");
	      TIFFSetField(out, TIFFTAG_COLORMAP, clut[0],clut[1],clut[2]);
	  }
	  else TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);

      }
      else
      {
      	  TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
      }
}

SetTIFFClut(clut,lutunit,type)
unsigned short clut[3][256];
int lutunit;
char *type; /* "Gray" or "Disk" */
{
      uint16_t i,j,val;
      enum {red,green,blue};
      unsigned char idx_lut[256][4];
      
      switch (toupper(*type))
      {
            case 'G':
                  for (i=0;i<256;i++)             /* default gray clut */
                  {
                        val = i + (i<<8);
                        clut[red][i] = val;
                        clut[green][i] = val;
                        clut[blue][i] = val;
                  }
                  break;
	    case 'D':				/* Disk File (IDX) clut */
		zvread( lutunit, idx_lut, NULL);
		for (j=0; j<3; j++)
			for (i=0;i<256;i++)
				 clut[j][i] = (int)257*idx_lut[i][j];
	
		zvclose( lutunit, NULL);
		  break;
	    default:
		  AbortMsg("Unknown lookup table type");
      }

}


void WriteTIFFClut(TIFF *tif)
{
      int i,j;
      enum {red,green,blue};
      uint16_t* clut[3];
      unsigned char idx_lut[256][4];
      int lutunit;
      uint16_t photo;

	TIFFGetField( tif, TIFFTAG_PHOTOMETRIC, &photo );
	if (photo==PHOTOMETRIC_PALETTE)
	{
		zvunit(&lutunit,"OUT",2, NULL);
		zvopen( lutunit, "op", "write", "u_nl", 1,
			 "u_ns", 1024, "u_format", "byte", NULL);
		
		TIFFGetField(tif, TIFFTAG_COLORMAP,
			 clut,clut+1,clut+2);
	
		/** create IDX format lut rgb0rgb0...**/
	
		memset( idx_lut, 0, 1024L );
		for (i=0; i<3; i++)
			for (j=0;j<256;j++)
				idx_lut[j][i] = clut[i][j]>>8;
		
		zvwrit( lutunit, idx_lut, NULL);
		zvclose( lutunit, NULL);
	}
	else zvmessage( "*** no lookup table in TIFF ***", " ");
}


char *tiff_time()
{
      static char time_string[20];
      time_t cur_time;
      struct tm timv;

      cur_time = time(0L);
      timv = *localtime(&cur_time);
      sprintf(time_string,"%4d:%02d:%02d %02d:%02d:%02d",
            1900+(timv.tm_year), 1+(timv.tm_mon), (timv.tm_mday),
                 (timv.tm_hour),   (timv.tm_min), (timv.tm_sec));
      
      return (time_string);
}



WriteTIFFData(unit, tif,nl,ns,nb)
int unit[];
TIFF *tif;
int nl,ns,nb;
{
      uint8_t *buf,*lbuf[3],*bptr[3],*ptr;
      uint32_t x,y,y1,y2,z;
      uint16_t planar;
      int nsamps;
      int tilebufsize;
      int tilepix;
      tiff_u_long tile_width,tile_height;
      int scanbytes;
      int band;
      int nrows;
      int chunky;
      int i;
      int plane_inc;
      int tile,tile1;
      long (*_tiff_write)();

      TIFFGetField( tif, TIFFTAG_PLANARCONFIG, &planar);
      chunky = (planar == PLANARCONFIG_CONTIG);

	
	if (TIFFIsTiled(tif))
	{
		TIFFWriteRawTile(tif, 0, (unsigned char *)&i, 0); /* forces setup */
		plane_inc = TIFFComputeTile(tif, 0,0,0,1)
				- TIFFComputeTile(tif, 0,0,0,0);
		TIFFGetField(tif, TIFFTAG_TILEWIDTH, &tile_width);
		TIFFGetField(tif, TIFFTAG_TILELENGTH, &tile_height);
		tilebufsize = TIFFTileSize(tif);
		_tiff_write = TIFFWriteEncodedTile;
	}
	else
	{
		TIFFWriteRawStrip(tif, 0, (unsigned char *)&i, 0); /* forces setup */
		plane_inc = TIFFComputeStrip(tif, 0, 1)
				- TIFFComputeStrip(tif, 0, 0);
		TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &tile_width);
		TIFFGetField(tif, TIFFTAG_ROWSPERSTRIP, &tile_height);
		tilebufsize = TIFFStripSize( tif );
		_tiff_write = TIFFWriteEncodedStrip;
	}
	tilepix = tile_width*tile_height;
	buf = (uint8_t *)malloc(tilebufsize);
	if (chunky)
		lbuf[0] = (uint8_t *)malloc(tilebufsize);
	else
		lbuf[0] = (uint8_t *)malloc(tilebufsize*nb);
	for (i=1;i<nb;i++) 
		lbuf[i] = lbuf[0]+ i*tilepix;
	if (!buf || !lbuf[0]) 
		AbortMsg("failed to allocate buffer for tile/strip");
	
	tile=0;
	for (y = 0; y < nl; y+=tile_height)
	{
		nrows = MIN(tile_height, nl - y);
		for (x = 0; x < ns; x+=tile_width)
		{
			memset(buf,0,(size_t)tilebufsize);

			/* read in a single tile */
			nsamps = MIN(tile_width, ns - x);
			for (y1=0,y2=0; y1< nrows; y1++)
			{
				for (band=0;band<nb;band++)
				{
					zvread(unit[band],lbuf[band]+y2,
					"line",y+y1+1, "samp",x+1,"nsamps",
                                         nsamps, NULL);
				}
				y2+=tile_width;
			}

			if (chunky)
			{
				/* reshuffle pixels from chunky: rgbrgb...*/
				ptr=buf;
				for (i=0;i<nb;i++) bptr[i] = lbuf[i];
				for (z=0; z<tilepix; z++)
					for (band=0;band<nb;band++)
						*ptr++ = *bptr[band]++;

				if (_tiff_write(tif, tile++, buf, tilebufsize) < 0)
					goto bad;
			}
			else /* interleave by tile */
			{
				tile1=tile++;
				for (band=0; band<nb; band++)
				{
					if (_tiff_write(tif, tile1, lbuf[band], tilebufsize) < 0)
						goto bad;
					tile1 += plane_inc;
				}
			}

		}
	}
      
      free(buf);
      free(lbuf[0]);
      return (1);
bad:
      free(buf);
      free(lbuf[0]);
      return (0);
}

