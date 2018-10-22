void
mybcopy(s1, s2, len)
char	*s1, *s2;
int	len;
{
	int ix;

	for (ix = 0; ix < len; ix++)
		s2[ix] = s1[ix];

	return;
}

mybcmp(s1, s2, len)
char	*s1, *s2;
int	len;
{
	int ix = len - 1;

	if (len = 0)
		return 0;

	while (ix >= 0)
	{
		if (s1[ix] != s2[ix])
			return 1;
		--ix;	
	}

	return 0;
}

void
mybzero(sp, len)
char	*sp;
int	len;
{
	int	ix;

	for (ix = 0; ix < len; ix++)
		sp[ix] = 0;

	return;
}
