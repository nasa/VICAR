#include <ctype.h>
#include "xvmaininc.h"
#include "tae_lib.h"
#include <string.h>
#include <stdlib.h>

/* architecture-specific manner of parsing search-path */

#if MAC_MPW_ARCH
#define DIRECTORY_END 	':'
#define SEARCH_PATH 	"COMMANDS"
#define PATH_SEPARATOR 	","
#define PATH_END 		""

#else
#if VMS_OS
#define DIRECTORY_END 	']'
#define SEARCH_PATH 	"PATH"
#define PATH_SEPARATOR 	":"
#define PATH_END 		""

#else
#if PC_DOS_ARCH
#define DIRECTORY_END 	'\\'
#define SEARCH_PATH 	"PATH"
#define PATH_SEPARATOR 	";"
#define PATH_END 		"\\"

#else /* UNIX_ARCH */
#define DIRECTORY_END 	'/'
#define SEARCH_PATH 	"PATH"
#define PATH_SEPARATOR 	":"
#define PATH_END 		"/"
#endif /* PC_DOS   */
#endif /* VAX_ARCH */
#endif /* MAC_ARCH */



void process_pdf_filename(char *proc_file,char *pdf_file, char *proc)
{
	int i;

	/* Remove the file suffix, if any:   */
	strcpy(pdf_file,proc_file);	
	if ((i=strlen(pdf_file))>4)
		if (pdf_file[i-4] == '.') pdf_file[i-4]='\0';

#if VMS_OS
	/* get rid of .exe;version part */
	for (j=strlen(pdf_file); (j>1) && (pdf_file[j-1] != '.'); j--);
	if (pdf_file[j-1] == '.')
		pdf_file[j-1] = 0;
#endif
	/*-- filter out the pathname & Tack on the pdf suffix */
	for (i=strlen(pdf_file); (i>1) && (pdf_file[i-1]!= DIRECTORY_END); i--);
	if ((i==1)&&(pdf_file[0] != DIRECTORY_END)) i=0;
	strcpy(proc,pdf_file + i);

	strcat(pdf_file,".pdf");
		
}


/* Open the pdf file, using the PATH search path, if needed */

FILE *open_pdf(char *pdf_file)
{
	FILE * fp;
	char test_file[80];
	char *path,*dir,*fil;
	int i,len;
	
	fp = fopen( pdf_file, "r" );
#if UNIX_OS
	if (fp == NULL)
	{
		/* Then we need to search through the PATH directories */
	
		for (i=strlen(pdf_file); (i>1) && (pdf_file[i-1]!= DIRECTORY_END); i--);
		if ((i==1)&&(pdf_file[0] != DIRECTORY_END)) i=0;
		fil = pdf_file + i;
		len = 0;
		
		if ((path = getenv(SEARCH_PATH)) != NULL)
		{
			for (dir=path; (*dir!='\0' && fp==NULL); dir+=len)
			{
				if (*dir != '\0')	/* found a good directory */
				{
					dir += strspn(dir,PATH_SEPARATOR);
					len = strcspn(dir,PATH_SEPARATOR);
					strncpy(test_file,dir,len);
					strcpy(test_file+len,PATH_END);
					strcat(test_file,fil);
					fp = fopen( test_file, "r");	/* And Test it	!		*/
				}
			}
		}
	}
#endif
	
	return fp;

}

/* fgetstr reads in a line, and concatenates any additional */
/* continuation lines if it hits a '+' character at the end */

int fgetstr(char **lptr, FILE *fp)
{
	int i,done,num=1000;
	static char line[1000];

	*lptr = line;

	do
	{
		/* skip to first non-whitespace char */
		
		while (isspace(line[0] = fgetc(fp)));
		done = feof(fp);
		i=1;
	
		/* get line plus continuation lines */
		
		while (!done)
		{
			for (;( i<(num-1) && line[i-1]!=13 &&line[i-1]!=10 && !feof(fp));i++)
				line[i] = fgetc(fp);
			done = (line[i-2] != '+');
			if (!done)  i -= 2;
		}
		
		/* get rid of end-of-line control characters */	
		
		if ((line[i-1] == 13)||(line[i-1] == 10)) line[i-1] = 0;
		else	line[i]=0;
	
		if (feof(fp)) return (EOF);
		
	} while ((line[0] == '!') || (strlen(line) ==0));
		
	return (FALSE);
}

