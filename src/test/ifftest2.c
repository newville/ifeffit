/* ifeffit test program 

   This provides a simple teset mechanism for using ifeffit in C

   Copyright (c) 1998--2000 Matthew Newville, The University of Chicago
  
   This software is distributed under the terms of the GNU General 
   Public License as published by the Free Software Foundation.  It 
   is distributed in the hope that it will be useful, but WITHOUT 
   ANY WARRANTY; without even the implied warranty of 
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

   See the GNU General Public License (in the file COPYING) for 
   more details.

   This file is based on and borrows heavily from the file fileman.c
   distributed with the GNU readline library.

   Matt Newville newville@cars.uchicago.edu
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#if defined (HAVE_STRING_H)
#  include <string.h>
#else 
#  include <strings.h>
#endif

#include "../lib/ifeffit.h"

int main(){
  int ret, i, npts;
  double x, xout, *array, *px;
  double ax[2048], ay[2048];
  char  *cp;
  px  = calloc(1,sizeof(double));

  i = 0;
  printf("=Test %i: Starting Ifeffit\n",i);
  /* start with a blank string to initialize ifeffit  */
  if (ifeffit (" ")) {
    printf("=Test %i failed: library did not load correctly\n",i) ;
    exit(1);
  } else {
    printf("=Test %i passed.\n", i) ;
  }

  i++;
  printf("=Test %i: simple math\n",i);

  ret = ifeffit(" x = 1+1");
  ret = ifeffit(" print x");
  ret = ifeffit(" my.x = indarr(40)");
  ret = ifeffit(" my.y = sin(my.x / 21)");
  ret = ifeffit(" show @arrays");

  /* scalars */
  x   = 1.0023;
  ret = iff_put_scalar("xx", &x);

  ret = ifeffit(" show @scalars");
  printf(" test of iff_scaval (getting etok):\n");
  ret  = iff_get_scalar("etok",px);
  *px = iff_scaval("etok");
  printf(" etok =  %12.7g\n", *px);

  /* strings */

  ret = iff_put_string("str2", "A Second Test String");
  ret = iff_put_string("str1", "Test String X#1");
  ret = iff_put_string("str3", "String 3");
  ret = ifeffit(" show @strings");

  cp  = iff_strval("str1");
  printf("out_string 1 = %s\n", cp);

  cp  = iff_strval("str2");
  printf("out_string 2 = %s\n", cp);

  cp  = iff_strval("str3");
  printf("out_string 3 = %s\n", cp);

  /* arrays */ 
  ret = ifeffit("set nn = npts(my.x)");
  npts = *px  = iff_scaval("nn");
  printf(" array size:=  %d\n", npts);

  array = calloc(npts,sizeof(double));
  npts  = iff_get_array("my.x", array);
  for (i = 0;i<npts; i++) {
    printf("  %12.7g", array[i]);
    if ((i+1) % 6 == 0) { printf("\n");}
    ay[i] = array[i] - 0.2 * i    ;
  }
  printf ("\n");

  ret = iff_put_array("my.u", &npts, ay);

  printf(" ..... \n");
  ifeffit(" show @arrays");

  printf(" ..... \n");
  /*  ifeffit(" plot(my.x, my.u)");*/
  free(px);
}







