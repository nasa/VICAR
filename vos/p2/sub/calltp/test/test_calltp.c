/*******************************************************************
 * Test main routine.  It shows how to use display_points() and how to
 * process data that came back from the subroutine.
 * vxp 4/97 - Initial delivery
 * vxp 5/97 - Fixed Line and Sample being printed in reverse
 * 	    - Added parameters to tp
 *******************************************************************/
#include "calltp.h"
#include <stdio.h>
#include <stdlib.h>

int main()
{
    int i, j;
    int n = 3;             /* number of points */
    char file1[256];	   /* VICAR file */
    char file2[256];	   /* VICAR file */
    int status;            /* Status returned from tiepoint program */
    double array[3][4] = { {25.50, 75.75, 75.20, 175}, 
			   {50.40, 100.1, 100.3, 200.938}, 
			   {75,    125,   125,   225} };
    double **out_array;

#ifdef __VMS
    sprintf(file1, "%s", "images:io.red");
    sprintf(file2, "%s", "images:io.blu");
#else
    sprintf(file1, "%s", "/usr/local/images/io.red");
    sprintf(file2, "%s", "/usr/local/images/io.blu");
#endif

    out_array = display_points(file1, file2, array, &n, NULL, &status);

    printf("Interactive program finished with exit status = %d\n", status);
    printf("%d tiepoints collected\n", n);
    printf("S1\t\tL1\t\t|\tS2\t\tL2\n");
    printf("-----------------------------------------------------------------\n");
    for (i = 0; i < n; i++) {
        for (j = 0; j < 4; j++) {
            if (j == 2) 
		printf("|\t");
            printf("%f\t", out_array[i][j]);
        }
        printf("\n");
    }
    return 0;
}
