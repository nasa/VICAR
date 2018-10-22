#include "xvmaininc.h"
#include <string.h>
#define  N  65

zpbname(id, par)
int        id   ;
char       par[12]  ;
{
 /*  Routine to return PLANET name given ID#   */
 /*   char   p8[12] ;      */
 char   pname[65][12] ;
 int    look[65] ;
 int    j, stat ;

 /*   ID numbering scheme is consistent with GLL SPICE
      LOOKUP table for PLANET I.D.  FROM SEDR WORD # 9   */

   /*   MERCURY  */
  look[0] = 199 ;  

   /*  VENUS     */   
  look[1] = 299 ;

   /*  EARTH     */
  look[2] = 399 ;  look[3] = 301 ;

   /*  MARS      */
  look[4] = 499 ;  look[5] = 401 ;  look[6] = 402 ;

   /*  JUPITER   */
  look[7] = 599 ;  
  for (j=0; j < 16; j++)  look[8+j] = 501 + j;

   /*  SATURN    */
  look[24] = 699 ;
  for (j=0; j < 17; j++)  look[25+j] = 601 + j ;

   /*  URANUS    */
  look[42] = 799 ;
  for (j=0; j < 15; j++)  look[43+j] = 701 + j ;

   /*  NEPTUNE    */
  look[58] = 899 ;   look[59] = 801 ;  look[60] = 802 ;

   /*  PLUTO      */
  look[61] = 999 ;    look[62] = 901 ;

   /*  GASPRA      */
  look[63] = 9511010  ;

   /*  IDA   */
  look[64] = 2431010;

  /*   Now the PLANETARY Body name  */

  strcpy(pname[0],  "MERCURY") ; 
  strcpy(pname[1],  "VENUS")   ;
  strcpy(pname[2],  "EARTH")   ;
  strcpy(pname[3],  "MOON")   ;
  strcpy(pname[4],  "MARS")   ;
  strcpy(pname[5],  "PHOBOS")   ;
  strcpy(pname[6],  "DEIMOS")   ;
  strcpy(pname[7],  "JUPITER")   ;
  strcpy(pname[8],  "IO")   ;
  strcpy(pname[9],  "EUROPA")   ;
  strcpy(pname[10], "GANYMEDE")   ;
  strcpy(pname[11], "CALLISTO")   ;
  strcpy(pname[12], "AMALTHEA")   ;
  strcpy(pname[13], "HIMALIA")   ;
  strcpy(pname[14], "ELARA")   ;
  strcpy(pname[15], "PASIPHAE")   ;
  strcpy(pname[16], "SINOPE")   ;
  strcpy(pname[17], "LYSITHEA")   ;
  strcpy(pname[18], "CARME")   ;
  strcpy(pname[19], "ANANKE")   ;
  strcpy(pname[20], "LEDA")   ;
  strcpy(pname[21], "THEBE")   ;
  strcpy(pname[22], "ADRASTEA")   ;
  strcpy(pname[23], "METIS")   ;
  strcpy(pname[24], "SATURN")   ;
  strcpy(pname[25], "MIMAS")   ;
  strcpy(pname[26], "ENCELADUS")   ;
  strcpy(pname[27], "TETHYS")   ;
  strcpy(pname[28], "DIONE")   ;
  strcpy(pname[29], "RHEA")   ;
  strcpy(pname[30], "TITAN")   ;
  strcpy(pname[31], "HYPERION")   ;
  strcpy(pname[32], "IAPETUS")   ;
  strcpy(pname[33], "PHOEBE")   ;
  strcpy(pname[34], "JANUS")   ;
  strcpy(pname[35], "EPIMETHEUS")   ;
  strcpy(pname[36], "HELENE")   ;
  strcpy(pname[37], "TELESTO")   ;
  strcpy(pname[38], "CALYPSO")   ;
  strcpy(pname[39], "ATLAS")   ;
  strcpy(pname[40], "PROMETHEUS")   ;
  strcpy(pname[41], "PANDORA")   ;
  strcpy(pname[42], "URANUS")   ;
  strcpy(pname[43], "ARIEL")   ;
  strcpy(pname[44], "UMBRIEL")   ;
  strcpy(pname[45], "TITANIA")   ;
  strcpy(pname[46], "OBERON")   ;
  strcpy(pname[47], "MIRANDA")   ;
  strcpy(pname[48], "CORDELIA")   ;
  strcpy(pname[49], "OPHELIA")   ;
  strcpy(pname[50], "BIANCA")   ;
  strcpy(pname[51], "CRESSIDA")   ;
  strcpy(pname[52], "DESDEMONA")   ;
  strcpy(pname[53], "JULIET")   ;
  strcpy(pname[54], "PORTIA")   ;
  strcpy(pname[55], "ROSALIND")   ;
  strcpy(pname[56], "BELINDA")   ;
  strcpy(pname[57], "PUCK")   ;
  strcpy(pname[58], "NEPTUNE")   ;
  strcpy(pname[59], "TRITON")   ;
  strcpy(pname[60], "NEREID")   ;
  strcpy(pname[61], "PLUTO")   ;
  strcpy(pname[62], "CHARON")   ;
  strcpy(pname[63], "GASPRA")   ;
  strcpy(pname[64], "IDA")   ;

 /*  Constants are now loaded ....  */ 

  stat = 0;  
  for (j=0; j < N; j++)
  {
   if (id == look[j])
   {
    stat = 1; 
    strcpy(par, pname[j]) ;
    return stat ;
   }
  }

}



/*  include "xvmaininc.h"
    #define  N  64          */

int  zpbid(par, id)
char       par[12]  ;
int        *id   ;
{
 /*  Routine to return ID# given PLANET Name  */
 char   p8[12] ;  
 char   pname[65][12] ;
 int    look[65] ;
 int    j, stat ;

 /*   ID numbering scheme is consistent with GLL SPICE
      LOOKUP table for PLANET I.D.  FROM SEDR WORD # 9   */
   /*   MERCURY  */
  look[0] = 199 ;  

   /*  VENUS     */   
  look[1] = 299 ;

   /*  EARTH     */
  look[2] = 399 ;  look[3] = 301 ;

   /*  MARS      */
  look[4] = 499 ;  look[5] = 401 ;  look[6] = 402 ;

   /*  JUPITER   */
  look[7] = 599 ;  
  for (j=0; j < 16; j++)  look[8+j] = 501 + j;

   /*  SATURN    */
  look[24] = 699 ;
  for (j=0; j < 17; j++)  look[25+j] = 601 + j ;

   /*  URANUS    */
  look[42] = 799 ;
  for (j=0; j < 15; j++)  look[43+j] = 701 + j ;

   /*  NEPTUNE    */
  look[58] = 899 ;   look[59] = 801 ;  look[60] = 802 ;

   /*  PLUTO      */
  look[61] = 999 ;    look[62] = 901 ;

   /*  GASPRA      */
  look[63] = 9511010  ;

   /*  IDA        */
  look[64] = 2431010  ;

  /*   Now the PLANETARY Body name  */

  strcpy(pname[0],  "MERCURY") ; 
  strcpy(pname[1],  "VENUS")   ;
  strcpy(pname[2],  "EARTH")   ;
  strcpy(pname[3],  "MOON")   ;
  strcpy(pname[4],  "MARS")   ;
  strcpy(pname[5],  "PHOBOS")   ;
  strcpy(pname[6],  "DEIMOS")   ;
  strcpy(pname[7],  "JUPITER")   ;
  strcpy(pname[8],  "IO")   ;
  strcpy(pname[9],  "EUROPA")   ;
  strcpy(pname[10], "GANYMEDE")   ;
  strcpy(pname[11], "CALLISTO")   ;
  strcpy(pname[12], "AMALTHEA")   ;
  strcpy(pname[13], "HIMALIA")   ;
  strcpy(pname[14], "ELARA")   ;
  strcpy(pname[15], "PASIPHAE")   ;
  strcpy(pname[16], "SINOPE")   ;
  strcpy(pname[17], "LYSITHEA")   ;
  strcpy(pname[18], "CARME")   ;
  strcpy(pname[19], "ANANKE")   ;
  strcpy(pname[20], "LEDA")   ;
  strcpy(pname[21], "THEBE")   ;
  strcpy(pname[22], "ADRASTEA")   ;
  strcpy(pname[23], "METIS")   ;
  strcpy(pname[24], "SATURN")   ;
  strcpy(pname[25], "MIMAS")   ;
  strcpy(pname[26], "ENCELADUS")   ;
  strcpy(pname[27], "TETHYS")   ;
  strcpy(pname[28], "DIONE")   ;
  strcpy(pname[29], "RHEA")   ;
  strcpy(pname[30], "TITAN")   ;
  strcpy(pname[31], "HYPERION")   ;
  strcpy(pname[32], "IAPETUS")   ;
  strcpy(pname[33], "PHOEBE")   ;
  strcpy(pname[34], "JANUS")   ;
  strcpy(pname[35], "EPIMETHEUS")   ;
  strcpy(pname[36], "HELENE")   ;
  strcpy(pname[37], "TELESTO")   ;
  strcpy(pname[38], "CALYPSO")   ;
  strcpy(pname[39], "ATLAS")   ;
  strcpy(pname[40], "PROMETHEUS")   ;
  strcpy(pname[41], "PANDORA")   ;
  strcpy(pname[42], "URANUS")   ;
  strcpy(pname[43], "ARIEL")   ;
  strcpy(pname[44], "UMBRIEL")   ;
  strcpy(pname[45], "TITANIA")   ;
  strcpy(pname[46], "OBERON")   ;
  strcpy(pname[47], "MIRANDA")   ;
  strcpy(pname[48], "CORDELIA")   ;
  strcpy(pname[49], "OPHELIA")   ;
  strcpy(pname[50], "BIANCA")   ;
  strcpy(pname[51], "CRESSIDA")   ;
  strcpy(pname[52], "DESDEMONA")   ;
  strcpy(pname[53], "JULIET")   ;
  strcpy(pname[54], "PORTIA")   ;
  strcpy(pname[55], "ROSALIND")   ;
  strcpy(pname[56], "BELINDA")   ;
  strcpy(pname[57], "PUCK")   ;
  strcpy(pname[58], "NEPTUNE")   ;
  strcpy(pname[59], "TRITON")   ;
  strcpy(pname[60], "NEREID")   ;
  strcpy(pname[61], "PLUTO")   ;
  strcpy(pname[62], "CHARON")   ;
  strcpy(pname[63], "GASPRA")   ;
  strcpy(pname[64], "IDA")   ;

  strcpy(p8, par) ; 
  
  stat = 0 ;
  for (j=0; j < N; j++)
  {
   if ( strcmp(p8, pname[j]) == 0)
   {
    stat = 1 ;
    *id = look[j] ;
    return  stat ;
   }
  }
}
