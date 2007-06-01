/* iff_wrap.c : C interface to  IFeffit XAFS Analysis library

 This module provides a simple set of functions to interface with
 the IFEFFIT XAFS Analysis library. The functions provided are:

   iff_exec(cmd)               execute an ifeffit command
   iff_put_scalar(name,val)    put a double to a named ifeffit scalar
   iff_get_scalar(name)        get a named ifeffit scalar
   iff_put_string(name,val)    put a string as a named ifeffit string
   iff_get_string(name)        get a named ifeffit string
   iff_put_array(name,len,arr) set an array as a name ifeffit array
   iff_get_array(name,arr)     get a named ifeffit array as an array
   iff_get_echo(char)          get line from ifeffit's echo cache
 */

#include "ifeffit.h"

static char iff_str[1024];

/* execute ifeffit command */
int iff_exec(char *cmd) { 
  sprintf(iff_str, "%s\n\n\0", cmd);
  return (ifeffit_(iff_str, strlen(iff_str)));
}

/* put double into a named ifeffit scalar */
int iff_put_scalar(char *name, double *val) { 
  sprintf(iff_str, "%s = %19.12g \n\n\0", name, *val);
  return (iff_exec(iff_str));
}

/* get double from a named ifeffit scalar */
int iff_get_scalar(char *name, double *val) { 
  return (iffgetsca_(name, val, strlen(name)));
}

/* put string into a named ifeffit string */
int iff_put_string(char *name, char *val) { 
  sprintf(iff_str, "$%s = %s \n\n\0", name, val);
  return (iff_exec(iff_str));
}

/* get string from a named ifeffit string */
int iff_get_string(char *name, char *val) { 
  int i;
  i =  iffgetstr_(name,  iff_str, strlen(name), strlen(iff_str));
  strncpy(val,iff_str,i+1);
  return i;
}

/* put array of length n into a named ifeffit array */
int iff_put_array(char *name, int *n, double *arr) { 
  return ( iffputarr_(name,  n, arr, strlen(name)) );
}

/* get array from named ifeffit array */
int iff_get_array(char *name, double *arr) { 
  return ( iffgetarr_(name,  arr, strlen(name)) );
}

int iff_get_echo(char *val) { 
  int i;
  i =  iffgetecho_(iff_str,  strlen(iff_str));
  strncpy(val,iff_str,i+1);
  return i;
}
