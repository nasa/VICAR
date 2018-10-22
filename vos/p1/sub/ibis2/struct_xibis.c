#include "ibis.h"

/* XIBIS Structure/method Manipulation Routines */


#define NEW_TYPE( type ) ((type *)calloc(1L, sizeof(type)))

/*
 *  Creation -- the methods are separate structs to allow overrides.
 */

XIBIS* _i_new_ibis(void)
{
	XIBIS *newibis;
	
	newibis = NEW_TYPE( XIBIS );
	if (!newibis) goto failure;
	
	newibis->rowmethod = NEW_TYPE(t_rowmethod);
	if (!newibis->rowmethod) goto failure;
	
	newibis->colmethod = NEW_TYPE(t_colmethod);
	if (!newibis->colmethod) goto failure;
	
	newibis->recmethod = NEW_TYPE(t_recmethod);
	if (!newibis->recmethod) goto failure;
	
	newibis->filemethod = NEW_TYPE(t_filemethod);
	if (!newibis->filemethod) goto failure;
	
	newibis->labmethod = NEW_TYPE(t_labmethod);
	if (!newibis->labmethod) goto failure;

	newibis->trans = ((trans_type *)calloc(1L, sizeof(trans_type)*(FMT_ASCII+1) ));
	if (!newibis->trans) goto failure;
	
	newibis->format_size = ((int *)calloc(1L, sizeof(int)*(FMT_LAST+1) ));
	if (!newibis->format_size) goto failure;
	
	newibis->column = ((XCOL **)calloc(1L, sizeof(XCOL*)*(MAX_COL+EXTRA_COL) ));
	if (!newibis->column) goto failure;
	
	return (newibis);

failure:
	_i_free_ibis( newibis );
	return( (XIBIS*)0);
}


#define FREE_MEMBER( mname ) if (ibis->mname) do \
	{free( ibis->mname ); ibis->mname=(void *)0;} while (0)
	
#define FREE_LIST( mname ) if (ibis->mname) do \
	{_i_free_list( ibis->mname ); ibis->mname=(void *)0;} while (0)

/*
 *  Destruction
 */

void _i_purge_ibis(XIBIS* ibis )
{
	if ( ibis )
	{
		/* free and clear members not created by "_i_new_ibis" */

		_i_free_col_array( ibis );
		FREE_LIST( formats );		
		FREE_LIST( record );
		FREE_LIST( groups );
		FREE_LIST( units );
		FREE_LIST( locals );
		FREE_LIST( gaps );
		FREE_MEMBER( fmt );
		ibis->fmt_len = IFMT_SIZE;
		
		/* clear all transient flags */
	
		ibis->flags &= (MASK_ORG | MASK_MODE | FLAG_FILE_OLD);
	}
}



void _i_free_ibis( XIBIS *ibis )
{
	if ( ibis )
	{
		/* free data created by FileUnitOpen */
	
		_i_purge_ibis( ibis );

		/* free members allocated by _i_new_ibis */

		FREE_MEMBER( rowmethod );
		FREE_MEMBER( colmethod );
		FREE_MEMBER( recmethod );
		FREE_MEMBER( filemethod );
		FREE_MEMBER( labmethod );
		FREE_MEMBER( column );
		FREE_MEMBER( trans );
		FREE_MEMBER( format_size );

		/* free the struct itself */
				
		free( ibis );
	}
}

