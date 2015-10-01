/* Internal prototypes for VRDI functions */
/* These are not full ANSI prototypes.  Too much work and not enough	*/
/* users of the package.  It's also incomplete. rgd 2/2010		*/

#ifndef VRDI_XDINTPROTO_H
#define VRDI_XDINTPROTO_H

int IP85LO_Interface(int *, int, int *, int *, int *, int *,
		     int *, int *, int *, int *, int *, int *);
int IP85HI_Interface(int *, int, int *, int *, int *, int *,
		     int *, int *, int *, int *, int *, int *);
int RAMTEK_Interface(int *, int, int *, int *, int *, int *,
		     int *, int *, int *, int *, int *, int *);
int IVAS_Interface(int *, int, int *, int *, int *, int *,
		   int *, int *, int *, int *, int *, int *);
int ADAGE_Interface(int *, int, int *, int *, int *, int *,
		    int *, int *, int *, int *, int *, int *);
int JUP_Interface(int *, int, int *, int *, int *, int *,
		  int *, int *, int *, int *, int *, int *);
int TEK_Interface(int *, int, int *, int *, int *, int *,
		  int *, int *, int *, int *, int *, int *);
int Dummy_Interface(int *, int, int *, int *, int *, int *,
		    int *, int *, int *, int *, int *, int *);
int X_Interface(int *, int, int *, int *, int *, int *,
		int *, int *, int *, int *, int *, int *);

int error();
int soft_error();

int XD_Allocate_Device();
int XD_Get_Device();
int XD_Free_Device();
int XD_Get_Devices_Alloc();

int socket_open();
int socket_connect();
int socket_accept();
int socket_recv();
int socket_send();
void socket_close();
void socket_free();
int socket_read_waiting();
int socket_write_waiting();
void socket_error();
int get_dtablesize();

int XD_Circle();
int XD_Circle_Points();

int XD_Clip();
int XD_Out_Codes();

int XD_Draw_Char();

void xd_error_handler();

int XD_Get_Segments();
unsigned char *which_buff();
int stack_runs();
int find_left();
int find_right();
int stack_it();

int XD_Initialize();

int XD_Polyline();
int XD_Draw_Vector();

int XD_Read_DIBs();
int dib_substr();

int XD_Read_Font();

int FindDefault();
int FindGeneric();
int FindOwnedGeneric();
int GetMasterPID();
int GetUserName();
int GetUserTerminal();
int GetUserVAX();
int GetDeviceInfo();




#endif
