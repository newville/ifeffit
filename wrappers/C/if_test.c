/*     sample use of ifeffit   */

#include "ifeffit.h"

int main(){
  int ret, i, npts;
  double x, xout, *array;
  double ax[200], ay[200];
  char cxx[256];

  /* basic execution of ifeffit commands */
  iff_exec(" my.x = indarr(40)");
  iff_exec(" my.y = sin(my.x)");
  ret = iff_exec(" my.z = cos(my.x)");

  ret = iff_exec(" show @arrays");

  /* scalars */
  x   = 1.0023;
  ret = iff_put_scalar("xx", &x);
  ret = iff_get_scalar("etok", &xout);
  printf(" etok =  %12.7g \n", xout);

  ret = iff_exec(" show @scalars");

  /* strings */
  ret = iff_put_string("str1", "Test String");
  ret = iff_exec(" show @strings");
  ret = iff_get_string("str1", cxx);
  printf("  out_string = %i %s\n", ret, cxx);


  /* arrays */ 
  npts = iff_get_array("my.y", ax);
  for (i = 0;i<npts; i++) {
    printf("  %12.7g", ax[i]);
    if ((i+1) % 6 == 0) { printf("\n");}
    ay[i] = ax[i] - 0.2 * i    ;
  }
  printf ("\n");

  ret = iff_put_array("my.u", &npts, ay);
  iff_exec(" show @arrays");

  /*  iff_exec(" plot(my.x, my.u)");*/

}







