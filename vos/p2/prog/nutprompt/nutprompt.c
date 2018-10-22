#include        "vicmain_c"
#include	"taeconf.inp"
#define prompt "NUT>"

    void main44 (void)
    {
    COUNT	line_num;		/* IN: input line number	     */
    COUNT	col_num;		/* IN: input column number	     */
    TEXT	loc_prompt[STRINGSIZ+1];
    int         count, lfcr;
    char         msg[5];

     zvp ("line",&line_num,&count);
     zvp ("col",&col_num,&count);
     zvp ("lfcr",&lfcr,&count);
     s_copy (prompt, loc_prompt);
     t_highlight (loc_prompt);
     t_output( line_num, col_num, loc_prompt);	/* write prompt              */
     if (lfcr) {
       line_num++;
       t_output( line_num, 1, "");
     }
     return;
    }
