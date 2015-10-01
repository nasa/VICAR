#include "tae_lib.h"
#include <stdio.h>
#include <string.h>

void process_command_line(struct PARBLK *parblk,char *command, 
			  char *command_line);

/*------------------------------------------------------*/
/*------------------------------------------------------*/
/*---------------- explicit TAE calls ------------------*/
/*------------------------------------------------------*/
/*------------------------------------------------------*/

/* Stand-alone C-Interface Initialization */
int zzinit(struct PARBLK *parblk,int argc,char *argv[])
{
    int i;
    int len;
    char *command_line;

    /* get the total length of command-line args */

    len = 5;
    for (i=1; i<argc; i++)
        len += strlen(argv[i]) + 5;	/* only 1 needed, more for safety */

    command_line = (char *)malloc(len);
    if (command_line == NULL) {		/* shouldn't happen, it's startup! */
        fprintf(stderr, "malloc failure in zzinit(); contact system programmer\n");
        return 0;
    }

    /* concatenate the args into a single command-line */

    command_line[0] = '\0';
    for (i=1;i<argc;i++)
    {
	strcat(command_line,argv[i]);
	if ((i+1)<argc) strcat(command_line," "); /* space may be significant */
    }

    /* call the common subroutine */
    process_command_line(parblk,argv[0],command_line);

    free(command_line);

    return SUCCESS;

}

/* FORTRAN Interface initialization -- Called by */
/* the FORTRAN stand-alone version of XZINIT     */

void FTN_NAME(xzparbinit)(struct PARBLK *parblk, char *command, 
			  char *command_line, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char c_cmd[1025],c_cmdline[8193];

	zsfor2c(c_cmd,1024,command,&parblk,3,2,1, command_line);
	zsfor2c(c_cmdline,8192,command_line,&parblk,3,3,2, command_line);

	/* call the common subroutine */
	process_command_line(parblk,c_cmd,c_cmdline);

}

/* common subroutine for both C and FORTRAN initialization */

void process_command_line(struct PARBLK *parblk,char *command, 
			  char *command_line)
{
	add_pdf_variables_to_parblk(parblk,command,command_line);

	put_pdf_values_in_parblk(parblk,command_line);
}
