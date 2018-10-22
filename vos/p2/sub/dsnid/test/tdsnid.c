/*===========================================================================*
 |  TDSNID.C -- Routine to test GCFDCODE.C				     |
 *===========================================================================*/
main()
{
	int icode,mrk3_station_id,mrk4_station_id,ind;
	char name[13];

	name[12] = 0;
	for (icode=0; icode<256; icode++)
		{
		ind = zdsnid(&icode, &mrk3_station_id, &mrk4_station_id, name);
		if (ind == 1) printf(" %d   %s   %d   %d\n",icode,
			name,mrk3_station_id,mrk4_station_id);
		}
	icode = 1;
	zdsnid(&icode, &mrk3_station_id, &mrk4_station_id, name);
	printf(" %d   %s   %d   %d\n",icode, name,mrk3_station_id,mrk4_station_id);
	icode = 0;
	zdsnid(&icode, &mrk3_station_id, &mrk4_station_id, name);
	printf(" %d   %s   %d   %d\n",icode, name,mrk3_station_id,mrk4_station_id);
	icode = 257;
	zdsnid(&icode, &mrk3_station_id, &mrk4_station_id, name);
	printf(" %d   %s   %d   %d\n",icode, name,mrk3_station_id,mrk4_station_id);
}
