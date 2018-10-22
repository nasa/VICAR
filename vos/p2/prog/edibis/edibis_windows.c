/*
 * Portable "Curses" window screen management for EDIBIS
 *   12-08-2012 - RJB - Converted char to const chtype
 *                  in void FTN_NAME(show_char)(const chtype *thechar);
 *                  on LINUX and MacOSX 10.6.8 
 */

#include "xvmaininc.h"
#include <curses.h>
#include <string.h>
#include "ftnbridge.h"
#define MAX_STRING 200

/*prototypes*/
void FTN_NAME(initscreen)(int batch);
void FTN_NAME(endscreen)(void);
void FTN_NAME(emit_beep)(void);
void FTN_NAME(high_write)(int *y, int *x, char *string, ZFORSTR_PARAM);
void FTN_NAME(gotoposition)(int *vert,int *horiz);
void FTN_NAME(gotoy)(int *vert);
void FTN_NAME(eraseline)(void);
void FTN_NAME(delete_char)(void);
void FTN_NAME(show_char)(const chtype *thechar);        //was int *thechar
void FTN_NAME(newscreen)(void);
void FTN_NAME(show_help)(void);
void FTN_NAME(show_nokeypad_help)(void);
void FTN_NAME(show_formats)(void);
void FTN_NAME(show_data)(void);
void FTN_NAME(clear_data)(void);
void FTN_NAME(show_header)(void);
void FTN_NAME(show_rownum)(void);
void FTN_NAME(show_colnum)(void);
void FTN_NAME(row_write)(char *string, int *row, ZFORSTR_PARAM);
void FTN_NAME(rownum_write)(char *string, ZFORSTR_PARAM);
void FTN_NAME(colnum_write)(char *string, ZFORSTR_PARAM);
void FTN_NAME(screen_write)(char *string, ZFORSTR_PARAM);
void FTN_NAME(display_prompt)(char *string, ZFORSTR_PARAM);
void FTN_NAME(display_status)(char *string, ZFORSTR_PARAM);

#if VMS_OS
#  define 	STANDOUT(win) wsetattr(win,_REVERSE)
#  define 	STANDEND(win) wclrattr(win,_REVERSE)
#else
#  ifdef A_REVERSE /* Then attributes work */
#    define 	STANDOUT(win) wattron(win,A_REVERSE)
#    define 	STANDEND(win) wattroff(win,A_REVERSE)
#  else /* try the only thing left */
#    define  STANDOUT(win) wstandout(win)
#    define  STANDEND(win) wstandend(win)
#  endif
#endif


#  define beep() printf("%c",7); 

#define PROMPTPOS 22
#define STATUSPOS 22
#define DATAPOS 4
#define DATASIZE 18
#define RCPOS 1
#define ROWPOS 5
#define COLPOS 22
#define FORMATPOS 2

static WINDOW *WEdit=(WINDOW *)0;
static WINDOW *WHeader=(WINDOW *)0;
static WINDOW *WFormats=(WINDOW *)0;
static WINDOW *WColNum=(WINDOW *)0;
static WINDOW *WRowNum=(WINDOW *)0;
static WINDOW *WData=(WINDOW *)0;
//static WINDOW *WPrompt=(WINDOW *)0;
//static WINDOW *WStatus=(WINDOW *)0;
static char keypad_mode[20];

void FTN_NAME(initscreen)(batch)
int batch;
{

#if VMS_OS
	strcpy(keypad_mode,"\033=");
#endif
#if UNIX_OS
	strcpy(keypad_mode,"\033[?1h\033=");
#endif

	printf(keypad_mode);  /* use "keypad()" for SYSV */

	initscr();
	noecho(); /* don't echo chars on terminal */
	crmode(); /* immediately transmit chars; no filter */
	nonl();   /* don't convert nl->CR/LF */

	WEdit = newwin(0,0,0,0);
		WHeader = subwin( WEdit,   2,0,0,0);
			WRowNum = subwin( WEdit, 1,6, RCPOS,  ROWPOS);
			WColNum = subwin( WEdit, 1,5, RCPOS,  COLPOS);
		WFormats = subwin( WEdit,  2,0,FORMATPOS,0);
		WData = subwin( WEdit,DATASIZE ,0,DATAPOS,0);

}

void FTN_NAME(endscreen)(void)
{
	endwin();
}

void FTN_NAME(emit_beep)(void)
{
	beep();
}

void FTN_NAME(high_write)(int *y, int *x, char *string, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
        char c_string[MAX_STRING+1];
	
        zsfor2c(c_string, MAX_STRING, string, &y, 3, 3, 1, string);
	wmove(WEdit,(*y)-1,(*x)-1);
	STANDOUT(WEdit);
	waddstr(WEdit,c_string);
	touchwin(WEdit);
	wrefresh(WEdit);
	STANDEND(WEdit);
}

void FTN_NAME(gotoposition)(vert,horiz)
int *vert;
int *horiz;
{
	wmove(WEdit,(*vert)-1,(*horiz)-1);
}

void FTN_NAME(gotoy)(vert)
int *vert;
{
	wmove(WEdit,(*vert)-1,0);
}

void FTN_NAME(eraseline)(void)
{
	wclrtoeol(WEdit);
 }


void FTN_NAME(delete_char)(void)
{
	int y,x;
	
	getyx(WEdit,y,x);
	wmove(WEdit,y,x-1);
	wclrtoeol(WEdit);
	touchwin(WEdit);
	wrefresh(WEdit);
}

void FTN_NAME(show_char)(thechar)
const chtype *thechar;                      // was int *thechar  12/8/2012
{
	waddch(WEdit,*thechar);
	touchwin(WEdit);
	wrefresh(WEdit);
}


void FTN_NAME(newscreen)(void)
{
	clear(); 
}


void FTN_NAME(show_help)(void)
{
	int line=0;

	wclear(WEdit);

#define WP(str) mvwaddstr(WEdit,line++,0,(str));
WP("          EDIBIS COMMANDS                 +-----------------------------------+")
WP("    (For NOKEYPAD help, hit return)       |        |        | FNDNXT | DELETE |")
WP("Arrow keys move current cell one place.   |  GOLD  |  HELP  |        |  (ROW) |")
WP("ADVANCE and BACKUP set direction.         |        |        |  FIND  | INSERT |")
WP("                                          |--------+--------+--------+--------|")
WP("FIND Search Strings:                      |        | PAGE   |        |  CUT   |")
WP("----------------------                    |   --   | UP/DWN |   --   | (CELL) |")
WP("  VALUE     -Exact Match                  |        |        |        | PASTE  |")
WP("  MIN:MAX   -Between values               |--------+--------+--------+--------|")
WP("  MIN:      -Value and above              | ADVANCE| BACKUP |        |        |")
WP("  :MAX      -Value and below              |        |        |   --   |   --   |")
WP("                                          | BOTTOM |  TOP   |        |        |")
WP("Other Commands:                           |--------+--------+--------+--------|")
WP("---------------                           |        | PAGE   |        | GO TO  |")
WP("CTRL/R      Refresh screen                |   --   | RT/LFT |   --   |  ROW   |")
WP("CTRL/F      Display values using format   |        |        |        |        |")
WP("QUIT        Quit and exit to VICAR        |--------+--------+--------|        |")
WP("EXIT        Save and exit to VICAR        |                 |        | GO TO  |")
WP("<RETURN>    Change cell value to input    |      --         |   --   |  COL   |")
WP("/ <command> Use NOKEYPAD command          +-----------------+--------+--------+")
WP("                                           To exit, press the spacebar.        ")

	touchwin( WEdit );
	wrefresh(WEdit);
}


void FTN_NAME(show_nokeypad_help)(void)
{
	int line=0;

	wclear(WEdit);

#define WP(str) mvwaddstr(WEdit,line++,0,(str));
WP("NOKEYPAD: The no-keypad commands may  be used interactively by placing the     ")
WP("command on the EDIBIS command-line, prefaced by a '/' character, e.g.          ")
WP("                                                                               ")             
WP("    ]  /(1,2) set 5.1                                                          ") 
WP("                                                                               ")             
WP("The (row,col) parameter is optional and defaults to current cursor position.   ")
WP("                                                                               ")             
WP("Command (may be abbreviated)                     Function                      ")
WP("-----------------------------   ---------------------------------------------  ")
WP("(row,col) SET <value>           Change the value of cell (row,col) to <value>  ")
WP("(row,col) DEL*ETE <numrows>     Delete <numrows> rows, starting at <row>       ")
WP("(row,col) INS*ERT <numrows>     Delete <numrows> rows, starting at <row>       ")
WP("(row,col) JUM*P                 Jump to position (row,col) in file.            ")
WP("(row,col) CUT                   Copy current cell value into buffer, and clear ")
WP("(row,col) PAS*TE                Paste buffer into current cell                 ")
WP("(row,col) SEA*RCH <string>      Search fwd/backwd in column for range <string>.")
WP("(row,col) FOR*MAT (formt)       (IBIS-1 only) Set the column FORTRAN FORMAT.   ")
WP("          FWD/BAC*KWARD         Set search direction ForWarD(down)/BACkwd(Up).")
WP("          TOP/BOTTOM            Go to top/bottom of file                       ")
WP("          LEF*T/RIG*HT          Go one page left/right in file                 ")
WP("          ROW <rownum>          Go to row <rownum>, same column                ")
WP("          COL*UMN <colnum>      Go to column <colnum>, same row                ")
WP("To exit, press any key.                                                        ")

	touchwin( WEdit );
	wrefresh(WEdit);
}

void FTN_NAME(show_formats)(void)
{
	touchwin( WEdit );
	wrefresh( WFormats );
}


void FTN_NAME(show_data)(void)
{
	touchwin( WEdit );
}

void FTN_NAME(clear_data)(void)
{
   wclear(WData);
   touchwin(WData);
}

void FTN_NAME(show_header)(void)
{
	touchwin( WEdit );
	wrefresh( WHeader );
}

void FTN_NAME(show_rownum)(void)
{
	touchwin( WRowNum );
}

void FTN_NAME(show_colnum)(void)
{
	touchwin( WColNum );
}


/*
 *  Append a string of characters, with no
 *  end-of-line return. This avoids the VAX-VMS prompting
 *  '+','$' carriage-control extension, which is non-portable.
 */


void FTN_NAME(row_write)(char *string, int *row, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 2, 1, 1, row);
   wmove(WEdit,DATAPOS + (*row)-1,0);
   waddstr(WEdit,c_string);
}

void FTN_NAME(rownum_write)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   wclear(WRowNum);
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,RCPOS,ROWPOS);
   waddstr(WEdit,c_string);
}

void FTN_NAME(colnum_write)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   wclear(WColNum);
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,RCPOS,COLPOS);
   waddstr(WEdit,c_string);
}

void FTN_NAME(screen_write)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   waddstr(WEdit,c_string);
}

void FTN_NAME(display_prompt)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,PROMPTPOS,0);
   wclrtoeol(WEdit);
   wmove(WEdit,PROMPTPOS,0);
   waddstr(WEdit,c_string);
   wrefresh(WEdit);
}

void FTN_NAME(display_status)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,STATUSPOS,0);
   wclrtoeol(WEdit);
   wmove(WEdit,STATUSPOS,0);
   waddstr(WEdit,c_string);
   waddstr(WEdit," (hit <return> to continue)");
   wrefresh(WEdit);
   wgetch(WEdit);
   wmove(WEdit,STATUSPOS,0);
   wclrtoeol(WEdit);
   wrefresh(WEdit);
}


