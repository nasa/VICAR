/*
 *  struct_xrec.c  - manipulates XREC structs.
 */

#include "ibis.h"

#define NEW_TYPE( type, num ) ((type *)calloc(1L, sizeof(type)*(num)))

XREC *_i_new_record(int ncols )
{
	XREC *rec;
	rec = NEW_TYPE( XREC, 1 );
	if (!rec) goto failure;
	
	rec->trans = NEW_TYPE( trans_type, (FMT_ASCII+1) );
	if (!rec->trans) goto failure;

	rec->column = NEW_TYPE( XCOL*, ncols );
	if (!rec->column) goto failure;

	rec->inbuffer = NEW_TYPE( char*, ncols );
	if (!rec->inbuffer) goto failure;

	rec->outbuffer = NEW_TYPE( char*, ncols );
	if (!rec->outbuffer) goto failure;

	rec->method = NEW_TYPE( t_recmethod, ncols );
	if (!rec->outbuffer) goto failure;

	rec->collaborators = _i_new_list(0);
	if (!rec->collaborators) goto failure;

	rec->num_cols = ncols;
	rec->cur_row = 1;
	rec->top_row = 1;
	rec->flags = FLAG_REC_REFRESH;

	return (rec);
	
failure:
	_i_free_record( rec );
	return (XREC *)0;
}


#define FREE_MEMBER( mname ) if (rec->mname) do \
	{free( rec->mname ); rec->mname=(void *)0;} while (0)
#define FREE_LIST( mname ) if (rec->mname) do \
	{_i_free_list( rec->mname ); rec->mname=(void *)0;} while (0)

void _i_free_record(XREC *rec)
{
	if (!rec) return;
	FREE_MEMBER( trans );
	FREE_MEMBER( column );
	FREE_MEMBER( inspace );
	FREE_MEMBER( outspace );
	FREE_MEMBER( inbuffer );
	FREE_MEMBER( outbuffer );
	FREE_MEMBER( method );
	FREE_LIST( collaborators );
	free( rec );
}

/*
 * General Purpose record-notification utility
 * (used by row and column ops )
 */

int _i_notify_records(XIBIS* ibis, notice_type notice, int sval, int endval)
{
	List *ent;
	XREC *rec;
	int status=1;
	
	if (!ibis->record) return 1;
	
	for (ent=ibis->record->next; ent; ent=ent->next)
	{
		rec = (XREC *)ent->value;
		status = rec->method->notice( rec, notice, sval, endval);
		if (status != 1) return status;
	}
	
	return status;
}
