#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spc.h"
#include "spiceinc.h"

/************************************************************************/
/* Test Program For FORTRAN AND C Callable Subroutine GETSPICE95.F      */
/************************************************************************/

void FTN_NAME(tzgetspice95)(int *mode, int *sc_id, char *camera, int * scet,
		char *target_name, int *system, char *source,
		void *usr_info[10], void *buf, int *ind, ZFORSTR_PARAM)
{
 ZFORSTR_BLOCK
 int            maxlen,
                nelement;
 char           ccamera[15],
                ctarget_name[30],
                csource[15],
                *temp;
 prov_info_typ  cusr_info;
 static buf_union_typ buffer;

 maxlen = 5;
 zsfor2c(ccamera, maxlen, camera, &mode, 10, 3, 1, ind);

 maxlen = 15;
 zsfor2c(ctarget_name, maxlen, target_name, &mode, 10, 5, 2, ind);

 maxlen = 5;
 zsfor2c(csource, maxlen, source, &mode, 10, 7, 3, ind);

 maxlen = 10;
 nelement = 10;
 zsfor2c_array(&temp, &maxlen, nelement, usr_info, &mode, 10, 8, 4, ind);
 memset((char *) &cusr_info, '\0', sizeof(prov_info_typ));

 memcpy(cusr_info.inst, temp+(0*maxlen), 4);
 memcpy(cusr_info.purpose, temp+(1*maxlen), 4);
 memcpy(cusr_info.prog_name, temp+(2*maxlen), 6);
 memcpy(cusr_info.sp_ref, temp+(3*maxlen), 4);
 memcpy(cusr_info.req_no, temp+(4*maxlen), 4);
 memcpy(cusr_info.year, temp+(5*maxlen), 4);
 memcpy(cusr_info.month_day, temp+(6*maxlen), 4);
 memcpy(cusr_info.hour_min, temp+(7*maxlen), 4);
 memcpy(cusr_info.file_id, temp+(8*maxlen), 4);
 memcpy(cusr_info.usr_grp_id, temp+(9*maxlen), 6);
 free (temp);


 memset((char*) &buffer, '\0', sizeof(buf_union_typ));

 *ind = zgetspice95(*mode, *sc_id, ccamera, scet,
                ctarget_name, *system, csource,
                &cusr_info, (buf_union_typ*) &buffer);

 if (*ind == SUCCESS) {
    printf("Test From C::SUCCESS\n");
    printBuffer(&buffer);
    memcpy((char *) buf, (char *) &buffer,
		sizeof(buf_union_typ));
    }
 else
    printf("Test From C::FAILURE\n"); 
}
/*========================================================*
 * int printBuffer(): Print the content of the MIPS	  *
 *			buffer				  *
 *========================================================*/
int printBuffer(buf) 
 buf_union_typ *buf;
{
 char str[200];

 printf("SC_ID                          : %d\n", buf->intbuf[0]);

 memcpy((char *) str, (char *) &buf->intbuf[1], 4);
 str[4] = '\0';
 printf("Instrument                     : %s\n", str);

 printf("measurement time year          : %d\n", buf->intbuf[2]);
 printf("measurement time day           : %d\n", buf->intbuf[3]);
 printf("measurement time hour          : %d\n", buf->intbuf[4]);
 printf("measurement time minute        : %d\n", buf->intbuf[5]);
 printf("measurement time second        : %d\n", buf->intbuf[6]);
 printf("measurement time millisec      : %d\n", buf->intbuf[7]);
 printf("Target body code (target_id)   : %d\n\n", buf->intbuf[8]);

 if (buf->intbuf[9] == 1)
    strcpy(str, "J2000");
 else if (buf->intbuf[9] == 2)
    strcpy(str, "B1950");
 printf("Coordinate System              : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[10], 4);
 str[4] = '\0';
 printf("Source file to search          : %s\n\n", str);

 printf("XYZ of SC relative central body: %g %g %g\n\n",
                        buf->doublebuf[15],
                        buf->doublebuf[16],
                        buf->doublebuf[17]);
 printf("XYZ of picture body relative SC: %g %g %g\n\n",
                        buf->doublebuf[18],
                        buf->doublebuf[19],
                        buf->doublebuf[20]);
 printf("SC range from sun              : %g\n", buf->doublebuf[24]);
 printf("SC range from central body     : %g\n", buf->doublebuf[25]);
 printf("SC range from picture body     : %g\n\n", buf->doublebuf[26]);

 printf("lat & lon of sun rel pic body : %g %g\n",
                buf->doublebuf[27], buf->doublebuf[28]);
 printf("lat & lon of sc rel pic body  : %g %g\n\n",
                buf->doublebuf[29], buf->doublebuf[30]);

 printf("C_MATRIX:\n%g %g %g\n%g %g %g\n%g %g %g\n\n",
        buf->doublebuf[40], buf->doublebuf[41], buf->doublebuf[42],
        buf->doublebuf[43], buf->doublebuf[44], buf->doublebuf[45],
        buf->doublebuf[46], buf->doublebuf[47], buf->doublebuf[48]);

 printf("lat & lon of P5 point: %g %g\n\n",
        buf->doublebuf[76], buf->doublebuf[77]);
 printf("incidence angle at P5 point: %g\n",
                        buf->doublebuf[78]);
 printf("emission  angle at P5 point: %g\n",
                        buf->doublebuf[79]);
 printf("phase     angle at P5 point: %g\n",
                        buf->doublebuf[80]);
 printf("Hor & Vert pix size at P5: %g %g\n\n",
        buf->doublebuf[81], buf->doublebuf[82]);
 printf("ME_MATRIX:\n%g %g %g\n%g %g %g\n%g %g %g\n\n",
        buf->doublebuf[49], buf->doublebuf[50], buf->doublebuf[51],
        buf->doublebuf[52], buf->doublebuf[53], buf->doublebuf[54],
        buf->doublebuf[55], buf->doublebuf[56], buf->doublebuf[57]);
 printf("SC Range to P5 : %g\n", buf->doublebuf[83]);
 printf("North Angle    : %g\n\n", buf->doublebuf[67]);

 printf("Picture body equat radius, long : %g\n", buf->doublebuf[12]);
 printf("Picture body equat radius, short: %g\n", buf->doublebuf[13]);
 printf("Picture body polar radius       : %g\n\n", buf->doublebuf[14]);
 printf("OM_MATRIX:\n%g %g %g\n%g %g %g\n%g %g %g\n\n",
        buf->doublebuf[58], buf->doublebuf[59], buf->doublebuf[60],
        buf->doublebuf[61], buf->doublebuf[62], buf->doublebuf[63],
        buf->doublebuf[64], buf->doublebuf[65], buf->doublebuf[66]);

 printf("RS-Vector: %g %g %g\n", buf->doublebuf[21],
                buf->doublebuf[22], buf->doublebuf[23]);

 printf("line   sub-s/c-point: %g\n", buf->doublebuf[68]);
 printf("sampel sub-s/c-point: %g\n", buf->doublebuf[69]);

 printf("\n******** Provenance Information *******\n");
 memcpy((char*) str, (char*) &buf->intbuf[188], 4);
 str[4] = '\0';
 printf("Institute             : %s\n", str);
 memcpy((char *) str, (char *)&buf->intbuf[168], 4);
 str[4] = '\0';
 printf("Year                  : %s\n", str);
 memcpy((char *) str, (char *) &buf->intbuf[169], 4);
 str[4] = '\0';
 printf("Month_day             : %s\n", str);
 memcpy((char *) str, (char *) &buf->intbuf[170], 4);
 str[4] = '\0';
 printf("Hour_min              : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[13], 4);
 str[4] = '\0';
 printf("SPK_ref               : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[171], 4);
 str[4] = '\0';
 printf("CK_ref                : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[172], 4);
 str[4] = '\0';
 printf("Purpose               : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[173], 7);
 str[7] = '\0';
 printf("Prog_name             : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[175], 4);
 str[4] = '\0';
 printf("Job_req_no            : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[176], 6);
 str[6] = '\0';
 printf("Usr_grp_id            : %s\n", str);
 return SUCCESS;
}
