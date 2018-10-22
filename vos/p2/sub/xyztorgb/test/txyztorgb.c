/*This program tests function XYZTORGB*/
#include   "vicmain_c"
main44()
{
  int device,i;
  double rgb[3],out[3][3],offset[3];
  							/*chromaticity coords.*/
  double white[3] = {.3324,.3475,.3201};		  /*D55*/

  double red[3] = {.62,.33,.05};			  /*Conrac 7211 LP*/
  double green[3] = {.21,.675,.115};
  double blue[3] = {.15,.06,.79};

  xyztorgb("TV",out,offset);
  printf("RESULTS FOR \"TV\":\n");
  printf("matrix = %6.6f %6.6f %6.6f\n",out[0][0],out[0][1],out[0][2]);
  printf("         %6.6f %6.6f %6.6f\n",out[1][0],out[1][1],out[1][2]);
  printf("         %6.6f %6.6f %6.6f\n",out[2][0],out[2][1],out[2][2]);
  printf("offset=  %6.6f %6.6f %6.6f\n",offset[0],offset[1],offset[2]);
  printf("\n");
  
  for (i = 0; i < 3; i++) {
    rgb[i] = out[i][0] * white[0] +
  	     out[i][1] * white[1] +
  	     out[i][2] * white[2] +
  	     offset[i];
  }


  printf("When returned matrix is multiplied by the chromaticity \n");
  printf("coordinates of sunlight (D55) and of the three phosphors, \n");
  printf("the results are the following phosphor intensities:\n");
  printf("\n");
  printf("reproduced       red      green     blue\n");
  printf("  color       intensity intensity intensity\n");


  printf("white       %10.6f %10.6f %10.6f\n",rgb[0],rgb[1],rgb[2]);

  for (i = 0; i < 3; i++) {
    rgb[i] = out[i][0] * red[0] + out[i][1] * red[1] + out[i][2] * red[2];
  }
  printf("red         %10.6f %10.6f %10.6f\n",rgb[0],rgb[1],rgb[2]);

  for (i = 0; i < 3; i++) {
    rgb[i] = out[i][0] * green[0] + out[i][1] * green[1] + out[i][2] * green[2];
  }
  printf("green       %10.6f %10.6f %10.6f\n",rgb[0],rgb[1],rgb[2]);

  for (i = 0; i < 3; i++) {
    rgb[i] = out[i][0] * blue[0] + out[i][1] * blue[1] + out[i][2] * blue[2];
  }
  printf("blue        %10.6f %10.6f %10.6f\n",rgb[0],rgb[1],rgb[2]);

  printf("\n");
  printf("The values for white should be approximately equal; those for \n");
  printf("red, green, and blue should be non-zero only in the red, green, \n");
  printf("and blue, respectively\n");
}

