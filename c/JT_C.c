#include <math.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct complex complex;
struct complex
{
	double re;
	double img;
}

complex fromReal (double r)
{
	complex c = {r, 0};
	return c;
}

//////

void fxshft (complex* p, int n)
{

}

//////

int main ()
{
	int i;
	double coeffs[4] = {1, -6, 11, -6};
	complex poly [4];
	for (i = 0; i < 4; i ++)
	{
		poly[i] = fromReal(coeffs[i]);
		printf("%f + i * %f\n", poly[i].re, poly[i].img);
	};

	return 0
}