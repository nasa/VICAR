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

#include "tiffio.h"
#include "vicmain_c"
#include <time.h>
#include <string.h>

typedef unsigned char u_char;
typedef unsigned long u_long;
typedef unsigned short u_short;

#ifndef MIN
#define MIN(x,y) ((x) < (y) ? (x) : (y))
#endif

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

PrintHandler(fd,fmt,ap)
char* fd;
char* fmt;
char* ap;
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

tiff_to_vic()
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

WriteFileFromTIFF(parm,instance,tif)
char *parm;
int instance;
TIFF *tif;
{
      int  unit[3],nl,ns,nb,i;

      SetUpVicarFile(parm,instance,tif,unit,&nl,&ns,&nb);
      if (!WriteVicarData(unit,tif,nl,ns,nb))
            AbortMsg("Error in Copying data to TIFF ");

      for (i=0; i<nb; i++)
	      zvclose(unit[i],NULL);
}


SetUpVicarFile(parm,instance,in,outunit,nl,ns,nb)
char *parm;
int instance;
TIFF *in;
int outunit[];
int *nl,*ns,*nb;
{
      int i;
      u_short nbs;
      char name[132];
      int numout;

      zvpcnt("OUT", &numout);
     
/* get info from TIFF file */
      TIFFGetField(in,TIFFTAG_IMAGEWIDTH,ns);
      TIFFGetField(in,TIFFTAG_IMAGELENGTH,nl);
      TIFFGetField(in, TIFFTAG_SAMPLESPERPIXEL, &nbs);
      *nb = nbs;
      if (*nb < 1) *nb=1;
      if (*nb > 3) *nb=3;
      if (*nb==3 && numout<3)
      {
	   zvmessage("Cannot yet compress RGB to 8-bit"," ");
	   zabend();
      }

      zvselpi(0);  /* INP is not a VICAR file */

      for (i=0;i<*nb;i++)
      {
	      zvpone(parm, name, instance+i, 132);
	      zvunit(outunit+i, "xxx", instance+i,"U_NAME",name, NULL);
	      
		/* Open up VICAR file with proper size */
	
	      zvopen(outunit[i],"OP","WRITE","U_NL",*nl,"U_NS",*ns,NULL);
	      
		/* Reopen in UPDATE for random access */
	      zvclose(outunit[i],NULL);
	      zvopen(outunit[i],"OP","UPDATE",NULL);
      }
}


WriteVicarData(outunit,in,nl,ns,nb)
int outunit[];
TIFF *in;
int nl,ns,nb;
{
      u_char *buf,*lbuf[3],*ptr;
      u_long x,y,y1,z;
      u_short planar;
      int nlines,nsamps;
      int tilebufsize;
      int tile_width,tile_height;
      int scanbytes;
      int band;
      int chunky;
      int i;

      TIFFGetField( in, TIFFTAG_PLANARCONFIG, &planar);
      chunky = (planar == PLANARCONFIG_CONTIG);

      if (TIFFIsTiled(in))            /* TIFF IMAGE ORGANIZED IN TILES */
      {
            TIFFGetField(in, TIFFTAG_TILEWIDTH, &tile_width);
            TIFFGetField(in, TIFFTAG_TILELENGTH, &tile_height);
            tilebufsize = TIFFTileSize(in);
	    buf = (u_char *)malloc(tilebufsize);
	    if (chunky)
	    	lbuf[0] = (u_char *)malloc(tilebufsize);
	    else
	    	lbuf[0] = (u_char *)malloc(tilebufsize*nb);
	    for (i=1;i<nb;i++) 
	    	lbuf[i] = lbuf[0]+ i*tile_width*tile_height;
            if (!buf || !lbuf[0]) 
		AbortMsg("failed to allocate buffer for tiles");
 
 	    for (band=0; band < nb; band++)
	    {
		    for (y = 0; y < nl; y+=tile_height)
		    {
			  nlines = MIN(tile_height, nl - y);
			  for (x = 0; x < ns; x+=tile_width)
			  {
				/* Read tile in to buffer */
				
				nsamps = MIN(tile_width, ns - x);
				if (chunky)
				{
				    if (TIFFReadTile(in, buf, x,y,0,0) < 0)
					goto bad;
				    /* reshuffle pixels from chunky: rgbrgb...*/
				    ptr=buf;
				    for (z=0; z<tile_width*tile_height; z++)
					for (band=0;band<nb;band++)
					     lbuf[band][z] = *ptr++;
				}
				else
				{
				   for (band=0; band<nb; band++)
			      	      if (TIFFReadTile(in, lbuf[band], x,y,0,band) < 0)
			           	  goto bad;
				}
	      
				/* Write out a single tile to VICAR */
				for (y1=0; y1< nlines; y1++)
				{
				    for (band=0;band<nb;band++)
				    {
				       zvwrit(outunit[band],
				          lbuf[band]+y1*tile_width,"line",y+y1+1,
				         "samp",x+1,"nsamps", nsamps,NULL);
				    }
				}
			  } /* samp loop */
		    } /* line loop */
	    } /* band loop */
      }
      else                              /* TIFF IMAGE ORGANIZED IN STRIPS */
      {
            scanbytes = TIFFScanlineSize(in);
            buf = (u_char *)malloc(scanbytes);
	    if (chunky)
            	lbuf[0] = (u_char *)malloc(scanbytes);
	    else
	    	lbuf[0] = (u_char *)malloc(scanbytes * nb);

	    for (i=1;i<nb;i++) 
	    	lbuf[i] = lbuf[0]+ i*ns;
      
	    for (y = 0; y < nl; y++)
	    {
	    	if (chunky)
		{
		    if (TIFFReadScanline(in, buf,y,0) < 0)
			goto bad;
		    /* reshuffle pixels from chunky: rgbrgb...*/
		    ptr=buf;
		    for (z=0; z<ns; z++)
		       for (band=0;band<nb;band++)
			    lbuf[band][z] = *ptr++;
		}
		else  /* band-interleaved by line */
		{
		    for (band=0; band<nb; band++)
 	                if (TIFFReadScanline(in, lbuf[band],y,band) < 0)
          	            goto bad;
		}
		
	    	for (band=0; band<nb; band++)
		    zvwrit(outunit[band],lbuf[band],"LINE",y+1,NULL);
		  
	    } /* line loop */
		    
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


vic_to_tiff()
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
      AddFiletoTIFF("INP",1,out,tiffmode); 

/* add auxilliary files, if any, using strips */
      zvpcnt("AUXIL",&count);
      for (i=0; i<count; i++)
      {
            TIFFWriteDirectory(out);
            AddFiletoTIFF("AUXIL",i+1,out,"strips");
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
		  hPix[1] = (hres - (double)hPix[0]) * (u_long)0xFFFFFFFF;
		  vPix[0] = vres;
		  vPix[1] = (vres - (double)vPix[0]) * (u_long)0xFFFFFFFF;
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
	u_long lat1000sec,long1000sec;
	u_long vPix[2];
	u_long hPix[2];
	u_long xPos,yPos;
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
		  hPix[1] = (hres*36e5 - (double)hPix[0]) * (u_long)0xFFFFFFFF;
		  vPix[0] = vres*36e5;
		  vPix[1] = (vres*36e5 - (double)vPix[0]) * (u_long)0xFFFFFFFF;
	
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

AddFiletoTIFF(parm,instance,tif,mode)
char *parm;
int instance;
TIFF *tif;
char *mode;      /* "tiled" or "Strips" */
{
      int  unit[3];
      int i,nl,ns,nb,numinp,status;
      char name[132];

      zvpcnt("INP", &numinp);
      nb = (numinp==3) ? 3 : 1;
   
      for (i=0; i<nb; i++)
      {
          zvpone(parm, name, instance+i, 132);
          zvunit(unit+i, "xxx", instance+i,"U_NAME",name, NULL);
          status = zvopen(unit[i],"OP","READ", NULL);
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


      SetUpTiffDirectory(unit,tif,mode,&nl,&ns,nb);
      if (!WriteTIFFData(unit,tif,nl,ns,nb))
            AbortMsg("Error in Copying data to TIFF ");
	    
      for (i=0; i<nb; i++)
          zvclose(unit[i],NULL);
}


/* This will probably do for initializing most TIFF files */

SetUpTiffDirectory(inunit,out,mode,nl,ns,nb)
int inunit[];
TIFF *out;
char *mode;      /* "tiled" or "Strips" */
int *nl;
int *ns;	 /* output: sizes */
int nb;		 /* input : number of bands */
{
      int count,def,tiled,status,i;
      int tile_width,tile_height;
      int numinp;
      char *tiff_time();
      u_short clut[3][256];
      u_short compression;

      zvpcnt("INP", &numinp);

      tiled = (toupper(*mode) == 'T');

/* Initial tags which are always the same for our applications */
      TIFFSetField(out, TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH);
      TIFFSetField(out, TIFFTAG_XRESOLUTION, 72.0);
      TIFFSetField(out, TIFFTAG_YRESOLUTION, 72.0);
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
      if (tiled)
      {
            zvp("TLINES",&tile_height,&count);
            zvp("TSAMPS",&tile_width,&count);
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
      		/* Random access for compressed requires this: */
      		TIFFSetField(out, TIFFTAG_ROWSPERSTRIP,1);
	  }      

      
/* Set width,length, and bands (called "samples" in TIFF) */
      zvget(inunit[0],"NL",nl,"NS",ns,NULL);

      TIFFSetField(out, TIFFTAG_IMAGEWIDTH,(u_long) *ns);
      TIFFSetField(out, TIFFTAG_IMAGELENGTH,(u_long) *nl);


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
	      SetTIFFClut(clut,"DiskFile");
	      TIFFSetField(out, TIFFTAG_COLORMAP, clut[0],clut[1],clut[2]);
	  }
	  else TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);

      }
      else
      {
      	  TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
      }
}

SetTIFFClut(clut,type)
unsigned short clut[3][256];
char *type; /* "Gray" or "Disk" */
{
      u_short i,j,val;
      enum {red,green,blue};
      unsigned char idx_lut[256][4];
      int lutunit;
      
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
		zvunit(&lutunit,"inp",2, NULL );
		zvopen( lutunit, "op", "read", NULL);
	
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


WriteTIFFClut(tif)
TIFF *tif;
{
      int i,j;
      enum {red,green,blue};
      u_short* clut[3];
      unsigned char idx_lut[256][4];
      int lutunit;
      u_short photo;

	TIFFGetField( tif, TIFFTAG_PHOTOMETRIC, &photo );
	if (photo==PHOTOMETRIC_PALETTE)
	{
		zvunit(&lutunit,"OUT",2, NULL );
		zvopen( lutunit, "op", "write", "u_nl", 1,
			 "u_ns", 1024, "u_format", "byte", NULL );
		
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



WriteTIFFData(inunit, out,nl,ns,nb)
int inunit[];
TIFF *out;
int nl,ns,nb;
{
      u_char *buf,*ptr,*endptr,*lbuf[3];
      u_long x,y,y1,z;
      int nsamps,nrows;
      int tilebufsize;
      int tile_width,tile_height;
      int scanbytes;
      int band;
      int chunky;
      int i;

      chunky = zvptst( "chunky" );

      if (TIFFIsTiled(out))
      {
            TIFFGetField(out, TIFFTAG_TILEWIDTH, &tile_width);
            TIFFGetField(out, TIFFTAG_TILELENGTH, &tile_height);
            tilebufsize = TIFFTileSize(out);
            buf = (u_char *)malloc(tilebufsize);
	    if (chunky)
	    	lbuf[0] = (u_char *)malloc(tilebufsize);
	    else
	    	lbuf[0] = (u_char *)malloc(tilebufsize*nb);
	    for (i=1;i<nb;i++) 
	    	lbuf[i] = lbuf[0]+ i*tile_width*tile_height;
            if (!buf || !lbuf[0]) 
                AbortMsg("failed to allocate buffer for tiles");
            
            for (y = 0; y < nl; y+=tile_height)
            {
                  nrows = MIN(tile_height, nl - y);
                  for (x = 0; x < ns; x+=tile_width)
                  {
                        /* read in a single tile */
                        memset(buf,0,(size_t)tilebufsize);
                        nsamps = MIN(tile_width, ns - x);
                        for (y1=0; y1< nrows; y1++)
			{
			    for (band=0;band<nb;band++)
			    {
                                zvread(inunit[band],lbuf[band]+y1*tile_width,
				  "line",y+y1+1, "samp",x+1,"nsamps", nsamps,NULL);
			    }
			    
 			}
			
			if (chunky)
			{
			    /* reshuffle pixels into chunky: rgbrgb...*/
			    ptr=buf;
			    for (z=0; z<tile_width*tile_height; z++)
			    	for (band=0;band<nb;band++)
			             *ptr++ = lbuf[band][z];
			    if (TIFFWriteTile(out, buf, x,y,0,0) < 0)
			         goto bad;
			}
			else /* interleave by tile */
			{
			   for (band=0; band<nb; band++)
			      if (TIFFWriteTile(out, lbuf[band], x,y,0,band) < 0)
			           goto bad;
			}
                  }
            }
      }
      else                              /* TIFF IMAGE ORGANIZED IN STRIPS */
      {
            scanbytes = TIFFScanlineSize(out);
            buf = (u_char *)malloc(scanbytes);
	    if (chunky)
            	lbuf[0] = (u_char *)malloc(scanbytes);
	    else
	    	lbuf[0] = (u_char *)malloc(scanbytes * nb);

	    for (i=1;i<nb;i++) 
	    	lbuf[i] = lbuf[0]+ i*ns;

            if (!buf || !lbuf[0]) AbortMsg("failed to allocate buffer for tiles");
            
            for (y = 0; y < nl; y++)
            {
	    	  for (band=0; band<nb; band++)
                  	zvread(inunit[band],lbuf[band],"LINE",y+1,NULL);
		  if (chunky)
		  {
		         /* reshuffle pixels into chunky: rgbrgb...*/
			 ptr=buf;
			 for (z=0; z<ns; z++)
			    for (band=0;band<nb;band++)
			        *ptr++ = lbuf[band][z];
 	                 if (TIFFWriteScanline(out, buf,y,0) < 0)
          	              goto bad;
		  }
		  else   /* band-interleaved by line */
		  {
		  	for (band=0; band<nb; band++)
 	                    if (TIFFWriteScanline(out, lbuf[band],y,band) < 0)
          	              goto bad;
		  }
	
            }
      }
      

done:
      free(buf);
      free(lbuf[0]);
      return (1);
bad:
      free(buf);
      free(lbuf[0]);
      return (0);
}

