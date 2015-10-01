#include "tae_lib.h"

void show_parblk(struct PARBLK *parb)
{
	int i=1,j,count;
	struct VARIABLE *vptr;
	
	
	vptr=parb->symtab.link;
	
	printf("**** Parameter Block *****\n");
	if (vptr != NULL)
		for (; vptr != NULL; vptr=vptr->v_link)
		{
			printf("\nVARIABLE #%d:\n",i++);
			printf("\tv_name = \"%s\"\n",vptr->v_name);
			switch (vptr->v_type)
			{
				case V_INTEGER:
					printf("\tv_type = V_INTEGER\n");
					break;
				case V_REAL:
					printf("\tv_type = V_REAL\n");
					break;
				case V_STRING:
					printf("\tv_type = V_STRING\n");
					break;
				default:
					printf("\tv_type = ** UNDEFINED **\n");
					break;
			}
			switch (vptr->v_class)
			{
				case V_GLOBAL:
					printf("\tv_class = V_GLOBAL\n");
					break;
				case V_LOCAL:
					printf("\tv_class = V_LOCAL\n");
					break;
				case V_PARM:
					printf("\tv_class = V_PARM\n");
					break;
				default:
					printf("\tv_class = ** UNDEFINED **\n");
					break;
			}
			printf("\tv_keyword = %d\n",vptr->v_keyword);
			printf("\tv_default = %d\n",vptr->v_default);

			printf("\tv_minc = %d\n",vptr->v_minc);
			printf("\tv_maxc = %d\n",vptr->v_maxc);
			printf("\tv_size = %d\n",vptr->v_size);
			printf("\tv_nullable = %d\n",vptr->v_nullable);
			
			printf("%d DEFAULT VALUES:\n",vptr->v_dcount);
			switch (vptr->v_type)
			{
				case V_INTEGER:
					for (j=0;j<vptr->v_dcount;j++)
						printf("\t\t%d: %d\n",j,DIVAL(*vptr,j));
					break;
				case V_REAL:
					for (j=0;j<vptr->v_dcount;j++)
						printf("\t\t%d: %f\n",j,DRVAL(*vptr,j));
					break;
				case V_STRING:
					for (j=0;j<vptr->v_dcount;j++)
						printf("\t\t%d: \"%s\"\n",j,DSVAL(*vptr,j));
					break;
			}

			if (vptr->v_valid == NULL)
				printf("0 VALID VALUES\n");
			else switch (vptr->v_type)
			{
				case V_INTEGER:
					count = ((struct I_VALID*)vptr->v_valid)->count;
					printf("%d VALID VALUES:\n",count);
					if (count >0)
					printf("\t\tlow: %d\n",((struct I_VALID*)vptr->v_valid)->range[0].low);
					if (count >1)
					printf("\t\thigh: %d\n",((struct I_VALID*)vptr->v_valid)->range[0].high);
					break;
				case V_REAL:
					count = ((struct R_VALID*)vptr->v_valid)->count;
					printf("%d VALID VALUES:\n",count);
					if (count > 0)
					printf("\t\tlow: %f\n",((struct R_VALID*)vptr->v_valid)->range[0].low);
					if (count > 1)
					printf("\t\thigh: %f\n",((struct R_VALID*)vptr->v_valid)->range[0].high);
					break;
				case V_STRING:
					count = ((struct S_VALID*)vptr->v_valid)->count;
					printf("%d VALID VALUES:\n",count);
					for (j=0;j<count;j++)
						printf("\t\t%d: \"%s\"\n",j,
						  ((struct S_VALID*)vptr->v_valid)->slist[j].string);
					break;
			}
			
			printf("%d ACTUAL VALUES:\n",vptr->v_count);
			switch (vptr->v_type)
			{
				case V_INTEGER:
					for (j=0;j<vptr->v_count;j++)
						printf("\t\t%d: %d\n",j,IVAL(*vptr,j));
					break;
				case V_REAL:
					for (j=0;j<vptr->v_count;j++)
						printf("\t\t%d: %f\n",j,RVAL(*vptr,j));
					break;
				case V_STRING:
					for (j=0;j<vptr->v_count;j++)
						printf("\t\t%d: \"%s\"\n",j,SVAL(*vptr,j));
					break;
			}
		}
	
	
}

