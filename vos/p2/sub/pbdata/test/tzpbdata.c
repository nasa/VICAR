#include "xvmaininc.h"
#include "ftnbridge.h"

#define SUCCESS 1
#define FAILURE 0

void FTN_NAME(tzpbdata)()
{
  char buf[132];
  float d[20];
  int datastat,idstat,id,i;
  int ntargets = 65;
  char name[65][13];
  char binpool[132];

  /* initialize planetary body names array */

  strcpy(name[0],  "MERCURY") ; 
  strcpy(name[1],  "VENUS")   ;
  strcpy(name[2],  "EARTH")   ;
  strcpy(name[3],  "MOON")   ;
  strcpy(name[4],  "MARS")   ;
  strcpy(name[5],  "PHOBOS")   ;
  strcpy(name[6],  "DEIMOS")   ;
  strcpy(name[7],  "JUPITER")   ;
  strcpy(name[8],  "IO")   ;
  strcpy(name[9],  "EUROPA")   ;
  strcpy(name[10], "GANYMEDE")   ;
  strcpy(name[11], "CALLISTO")   ;
  strcpy(name[12], "AMALTHEA")   ;
  strcpy(name[13], "HIMALIA")   ;
  strcpy(name[14], "ELARA")   ;
  strcpy(name[15], "PASIPHAE")   ;
  strcpy(name[16], "SINOPE")   ;
  strcpy(name[17], "LYSITHEA")   ;
  strcpy(name[18], "CARME")   ;
  strcpy(name[19], "ANANKE")   ;
  strcpy(name[20], "LEDA")   ;
  strcpy(name[21], "THEBE")   ;
  strcpy(name[22], "ADRASTEA")   ;
  strcpy(name[23], "METIS")   ;
  strcpy(name[24], "SATURN")   ;
  strcpy(name[25], "MIMAS")   ;
  strcpy(name[26], "ENCELADUS")   ;
  strcpy(name[27], "TETHYS")   ;
  strcpy(name[28], "DIONE")   ;
  strcpy(name[29], "RHEA")   ;
  strcpy(name[30], "TITAN")   ;
  strcpy(name[31], "HYPERION")   ;
  strcpy(name[32], "IAPETUS")   ;
  strcpy(name[33], "PHOEBE")   ;
  strcpy(name[34], "JANUS")   ;
  strcpy(name[35], "EPIMETHEUS")   ;
  strcpy(name[36], "HELENE")   ;
  strcpy(name[37], "TELESTO")   ;
  strcpy(name[38], "CALYPSO")   ;
  strcpy(name[39], "ATLAS")   ;
  strcpy(name[40], "PROMETHEUS")   ;
  strcpy(name[41], "PANDORA")   ;
  strcpy(name[42], "URANUS")   ;
  strcpy(name[43], "ARIEL")   ;
  strcpy(name[44], "UMBRIEL")   ;
  strcpy(name[45], "TITANIA")   ;
  strcpy(name[46], "OBERON")   ;
  strcpy(name[47], "MIRANDA")   ;
  strcpy(name[48], "CORDELIA")   ;
  strcpy(name[49], "OPHELIA")   ;
  strcpy(name[50], "BIANCA")   ;
  strcpy(name[51], "CRESSIDA")   ;
  strcpy(name[52], "DESDEMONA")   ;
  strcpy(name[53], "JULIET")   ;
  strcpy(name[54], "PORTIA")   ;
  strcpy(name[55], "ROSALIND")   ;
  strcpy(name[56], "BELINDA")   ;
  strcpy(name[57], "PUCK")   ;
  strcpy(name[58], "NEPTUNE")   ;
  strcpy(name[59], "TRITON")   ;
  strcpy(name[60], "NEREID")   ;
  strcpy(name[61], "PLUTO")   ;
  strcpy(name[62], "CHARON")   ;
  strcpy(name[63], "GASPRA")   ;
  strcpy(name[64], "IDA")   ;


  zvmessage("********C CALLABLE VERSION****","");
  zvmessage("1       TARGET  TARGET                              ROTATION     SOLAR","");
  zvmessage("        NUMBER   NAME       A       B       C       PERIOD      RANGE","");

  for (i=0;i<65;i++)
    {
    datastat = zpbdata(name[i],d);
    /*zprnt(4,1,&datastat," ZPBDATA return:");*/
    if(datastat == FAILURE) zmabend("error in pbdata","");
    idstat = zpbid(name[i],&id);		/*SEDR ID*/
    /*zprnt(4,1,&datastat," ZPBID return:");*/
    if(idstat == FAILURE) zmabend("error in pbid","");

    sprintf(buf,"%11d    %-9s %7.1f %7.1f %7.1f %12.7f %16.10E",id,
	    name[i],d[0],d[1],d[2],d[4],d[5]);
    zvmessage(buf,"");
    }

    return;
}
