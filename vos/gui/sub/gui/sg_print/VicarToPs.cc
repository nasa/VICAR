///////////////////////////////////////////////////////////////////////////////
// VicarToPostScript - function which converts a VICAR-File
// into a PostScript-File in order to print the main widget
// either into a file or send it imidiatly to the printer
///////////////////////////////////////////////////////////////////////////////
#include "VicarToPs.h"
#include "zvproto.h"    // prototypes of RTL (zvopen ...)

#include <iostream>
using namespace std;
#include <string.h>
#include <time.h> 
#include <stdlib.h>

/*-------------------------------------------------------------------------*/
/*       Writes a title above and a frame around the VICAR image           */
/*-------------------------------------------------------------------------*/
void WriteFrameAndTitle(FILE *psFile, char *title)
{
    fprintf(psFile, "%s\n", "%-------------- Image Frame ---------------");
    fprintf(psFile, "gsave\n");
    fprintf(psFile, "XPos YPos translate\n");
    fprintf(psFile, "width height\n");
    fprintf(psFile, "printFrame\n");
    fprintf(psFile, "%s\n", "%-------------- Image Title ---------------");
    fprintf(psFile, "height 5 add\n");
    fprintf(psFile, "(%s) printTitle\n", title);
    fprintf(psFile, "grestore\n\n");
}

/*-------------------------------------------------------------------------*/
/*            Writes the header into the PostScript-File                   */
/*-------------------------------------------------------------------------*/
void WriteHeader(FILE *psFile)
{
    char dateStr[80];
    char currDate[50];
    struct tm *newtime;
    time_t aclock;
    
    // Get the curent system date

    time(&aclock);                      // get the time in seconds
    newtime = localtime( &aclock);      // converts time to struct tm form
    strcpy(currDate, asctime(newtime));
    sprintf(dateStr, "%%CreationDate:   %s", currDate);
    
    // Set up the ADOBE POSTSCRIPT (R)  Page description Comments
    
    fprintf( psFile, "%s\n",       "%!PS-Adobe-2.0");
    fprintf( psFile, "%s\n",       "%%Creator:       TP Tiepoint Program");
    fprintf( psFile, "%s\n",       "%%Title:         TP Printout");
    fprintf( psFile, "%s",         dateStr);
    fprintf( psFile, "%s\n",       "%%Pages:         1");
    fprintf( psFile, "%s\n",       "%%DocumentFonts: Times-Roman");
    fprintf( psFile, "%s\n\n",     "%%EndComments");
}

/*-------------------------------------------------------------------------*/
/*        Writes the variable definitions into the PostScript-File         */
/*-------------------------------------------------------------------------*/
void WriteVariableDefinitions(FILE *psFile, int nl, int ns, PaperSize psize)
{
    int h, w, x, y;           // used page size and position
    float lsq, slq;           // Ratios nl/ns and ns/nl
    float hwq;                // Ratio height/width
    
    lsq = (float)nl / (float)ns;
    slq = (float)ns / (float)nl;
    hwq = (float)psize.height / (float)psize.width;

    if(lsq > hwq) { 
	w = (int) ( ((float)psize.height * (float)ns) / (float)nl );
	h = psize.height;
	x = (int)( (float)(psize.width - w) / 2.0 ) + psize.xpos;
	y = psize.ypos;
    }
    else if(lsq < hwq) {
	w = psize.width;
	h = (int) ( ((float)psize.width * (float)nl) / (float)ns );
	x = psize.xpos;
	y = (int)( (float)(psize.height - h) / 2.0 ) + psize.ypos;
    }
    else { 
	w = psize.width;
	h = psize.height;
	x = psize.xpos;
	y = psize.ypos;
    }
    
    /*  Define Variables */
    
    fprintf(psFile, "%s\n", "%--------- Define Variables ----------------");
    fprintf(psFile, "/width    %3d def  %s\n", w, "%Page Width");
    fprintf(psFile, "/height   %3d def  %s\n", h, "%page Height");
    fprintf(psFile, "/XPos     %3d def  %s\n", x, "%left start X-Position");
    fprintf(psFile, "/YPos     %3d def  %s\n", y, "%bottom start Y-Position");
    fprintf(psFile, "/Margin    10 def  %s\n", "%margin beetwin plots");
    fprintf(psFile, "/basefont  /Times-Roman findfont def\n\n");
}

/*-------------------------------------------------------------------------*/
/*            Writes the PostScript procedures into the file               */
/*-------------------------------------------------------------------------*/
void WriteProcedureDefinitions(FILE *psFile)
{
    /* Define Procedures */
    
    fprintf(psFile, "%s\n", "%--------- Define Procedures ---------------");
    fprintf(psFile, "/printFrame       %s\n", "%stack -> height, width");
    fprintf(psFile, "  {  newpath\n");
    fprintf(psFile, "     2 copy\n");
    fprintf(psFile, "     0 0 moveto\n");
    fprintf(psFile, "     0 exch rlineto\n");
    fprintf(psFile, "     0 rlineto\n");
    fprintf(psFile, "     0 exch neg rlineto\n");
    fprintf(psFile, "     neg 0 rlineto\n");
    fprintf(psFile, "     closepath\n");
    fprintf(psFile, "     stroke  } def\n\n");
    
    fprintf(psFile, "/printTitle       %s\n", 
	    "%stack -> title string, height");
    fprintf(psFile, "  { /title exch def\n");
    fprintf(psFile, "    /hoch exch def\n");
    fprintf(psFile, "    basefont findfont 15 scalefont setfont\n");
    fprintf(psFile, "    .2 setgray\n");
    fprintf(psFile, "    width\n");
    fprintf(psFile, "    title stringwidth pop sub 2 div  %s\n",
	    "%centers string, x for moveto");
    fprintf(psFile, "    hoch moveto                      %s\n",
	    "%y for moveto (height)");
    fprintf(psFile, "    title show } def\n\n");
}

/*-------------------------------------------------------------------------*/
/*            Writes the VICAR image into the PostScript-File              */
/*-------------------------------------------------------------------------*/
void WriteImage(FILE *psFile, int InUnit, 
                unsigned char *byteBuff, unsigned short *halfBuff,
                int nl, int ns, int ps)
{ 
    int i,s;
    unsigned char *bytePtr;
    unsigned short *halfPtr;
    
    fprintf(psFile, "%s\n", "%----------------- Image -----------------");
    fprintf(psFile, "gsave\n");
    fprintf(psFile, "XPos YPos translate\n");
    fprintf( psFile, "width height scale\n");
    fprintf( psFile, "%s%d%s\n",
	     " /picstr ", ns, " string def");
    fprintf( psFile, "%s\n",       " /vicarimage");
    fprintf( psFile, " { %d %d %d [ %d 0 0 %d 0 %d ] \n",
	     ns,nl,ps*8, ns,-nl,nl);
    fprintf( psFile, "     {currentfile picstr readhexstring pop}\n");
    fprintf( psFile, "     image\n");
    fprintf( psFile, "     }  def\n");
    fprintf( psFile, " vicarimage\n");
    if(byteBuff) { 
	for(i=0; i<nl; i++) { 
	    zvread(InUnit, byteBuff, "LINE", i+1, NULL);
	    bytePtr=byteBuff;
	    for(s=0; s<ns; s++, bytePtr++) { 
		fprintf( psFile, "%02x", *bytePtr );
		if(((s+1)%30) == 0)
		    fprintf( psFile, "\n" );
	    }
	    fprintf( psFile, "\n" );
	}
    }
    else { 
	for(i=0; i<nl; i++) { 
	    zvread(InUnit, halfBuff, "LINE", i+1, NULL);
	    halfPtr=halfBuff;
	    for(s=0; s<ns; s++, halfPtr++) { 
		fprintf( psFile, "%02x", *halfPtr );
		if(((s+1)%30) == 0)
		    fprintf( psFile, "\n" );
	    }
	    fprintf( psFile, "\n" );
	}
    }
    fprintf( psFile, "\n" );
    fprintf(psFile, "grestore\n\n");
}

/*-------------------------------------------------------------------------*/
/*    Converts a given VICAR-File into the PostScript-File                 */
/*-------------------------------------------------------------------------*/
int vicImageToPostScript(char *vicFile, char *psFile, char *title, 
			 PaperSize pSize)
{
    int vicUnit;
    int nl, ns;
    static int instance = 1;
    char format[8];
    int ps;                          /* pixel size in bytes      */
    FILE *psFilePtr;
    unsigned char  *byteBuff;        /* pointer to image data of type BYTE*/
    unsigned short *halfBuff;        /* buffer for image data of type HALF */
    
    byteBuff = NULL;
    halfBuff = NULL;
    
    // Open the VICAR image file for reading

    zvunit(&vicUnit, (char *)"VicarToPs", instance++, "u_name", vicFile, NULL);
    zvopen(vicUnit, NULL);
    zvget(vicUnit, 
	  "NL",       &nl,
	  "NS",       &ns,
	  "PIX_SIZE", &ps,
	  "FORMAT",   format,
	  NULL);

    // Open the PostScript-File for writing

    psFilePtr = fopen(psFile, "w");
    if (!psFilePtr) {
	cerr << "Error opening output file" << endl;
	return -1;
    }

    // Allocation of memory for one image line 

    if(!strcmp(format, "BYTE")) { 
	byteBuff=(unsigned char *)malloc(ns*sizeof(char));
	if(byteBuff==NULL) { 
	    cerr << "Sorry, memory problems !!" << endl;
	    return -2;
	}
    }
    else if(!strcmp(format, "HALF")) { 
	halfBuff=(unsigned short *)malloc(ns*sizeof(short));
	if(halfBuff==NULL) { 
	    cerr << "Sorry, memory problems !!" << endl;
	    return -2;
	}
    }
    else { 
	cerr << "Sorry, only BYTE and HALF supported for PostScript !!" 
	     << endl;
	return -3;
    } 

    // Write the header, variables and PostScript procedures 

    WriteHeader(psFilePtr);
    WriteVariableDefinitions(psFilePtr, nl, ns, pSize);
    WriteProcedureDefinitions(psFilePtr);
    
    // Write the VICAR image into the PS-file

    WriteImage(psFilePtr, vicUnit, byteBuff, halfBuff, nl, ns, ps);

    // Write a title above and a frame around the VICAR-image

    WriteFrameAndTitle(psFilePtr, title);

    // Insert the showpage command to print out the page

    fprintf(psFilePtr, "showpage   %s\n\n", "%prints the current page");
  
    // Close the VICAR file and the PostsScript-File

    zvclose(vicUnit, NULL);
    if(byteBuff) free((char *)byteBuff);
    if(halfBuff) free((char *)halfBuff);
    fclose(psFilePtr);  
    
    return(0);
}

