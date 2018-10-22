/**  Copyright (c) 1999, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>

#include "return_status.h"

main()
{ int	code,
	subsys,
	number;

  printf("\nStatic display options\n");
  for (number=0; number<30; number++)
  { code = number % 8;
    subsys = number % 10;

    printf("%s\n",
           err_msg(RTN_STAT_VALUE(number,subsys,code),
                   RTN_M_STRING,RTN_M_NMEMONIC,(RTN_M_STRING|RTN_M_VALUE)));
  }

  printf("\n\nMixing display options\n");

  for (number=0; number<30; number++)
  { code = number % 8;
    subsys = number % 10;

    printf("%s\n",err_msg(RTN_STAT_VALUE(number,subsys,code),
			((number+3)%5),((number+1)%5),(number%8)));
/***
			((number+3)%5),((number+1)%5),RTN_M_STRING));
			RTN_M_STRING,RTN_M_STRING,RTN_M_STRING));
			RTN_M_STRING,0,RTN_M_STRING));
			RTN_M_NMEMONIC,0,RTN_M_STRING));
***/
  }

  return number;
}
