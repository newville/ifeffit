/* ifeffitc.c : C interface to  IFeffit XAFS Analysis library

 This module provides a simple set of functions to interface with
 the IFEFFIT XAFS Analysis library. The functions provided are:

   iff_exec(cmd)               execute an ifeffit command
   iff_put_scalar(name,val)    put a double to a named ifeffit scalar
   iff_get_scalar(name)        get a named ifeffit scalar
   iff_put_string(name,val)    put a string as a named ifeffit string
   iff_get_string(name)        get a named ifeffit string
   iff_put_array(name,len,arr) set an array as a named ifeffit array
   iff_get_array(name,arr)     get a named ifeffit array as an array
   iff_get_echo(char)          get line from ifeffit's echo cache
   iff_strval(char)            return pointer to ifeffit string

c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1997--2000 Matthew Newville, The University of Chicago
c Copyright (c) 1992--1996 Matthew Newville, University of Washington
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, The University of Washington, or the authors
c appear in advertising or endorsement of works derived from this
c software without specific prior written permission from all parties.
c
c THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
c EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
c MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
c IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
c CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
c TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
c SOFTWARE OR THE USE OR OTHER DEALINGS IN THIS SOFTWARE.
c//////////////////////////////////////////////////////////////////////
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ifeffit.h"

/* execute ifeffit command */
IFF_EXPORT(int) iff_exec(char *cmd) {
  char iff_str[1027];
  sprintf(iff_str, "%s\n\n\0", cmd);
  return (ifeffit_(iff_str, strlen(iff_str)));
}

IFF_EXPORT(int) ifeffit(char *cmd) {
  char iff_str[1027];
  sprintf(iff_str, "%s\n\n\0", cmd);
  return (ifeffit_(iff_str, strlen(iff_str)));
}

/* put double into a named ifeffit scalar */
IFF_EXPORT(int) iff_put_scalar(char *name, double *val) {
  char cmd[1027];
  sprintf(cmd, "%s = %19.12g", name, *val);
  return (iff_exec(cmd));
}

/* get double from a named ifeffit scalar */
IFF_EXPORT(int) iff_get_scalar(char *name, double *val) {
  return (iffgetsca_(name, val, strlen(name) )); 
}

/* put string into a named ifeffit string */
IFF_EXPORT(int) iff_put_string(char *name, char *val) {
  char cmd[1027];
  sprintf(cmd, "$%s = '%s'", name, val);
  return (iff_exec(cmd));
}

/* get string from a named ifeffit string */
IFF_EXPORT(int) iff_get_string(char *name, char *val) {
  int  i;
  char tmp[256];
  i   = iffgetstr_(name, tmp, strlen(name), 256);
  strncpy(val,tmp,i+1);
  return i;
}

/* put array of length n into a named ifeffit array */
IFF_EXPORT(int) iff_put_array(char *name, int *n, double *arr) {
  return (iffputarr_(name,  n, arr, strlen(name)) );
}

/* get array from named ifeffit array */
IFF_EXPORT(int) iff_get_array(char *name, double *arr) {
  return (iffgetarr_(name,  arr, strlen(name)) );
}

IFF_EXPORT(int) iff_get_echo(char *val) {
  int  i;
  char tmp[512];
  i =  iffgetecho_(tmp, 512);
  strncpy(val, tmp, i+1);
  return i;
}

IFF_EXPORT(char*) iff_strval(char *name) {
  int  i;
  char *t, *c;
  t = calloc(256,sizeof(char));
  i = iff_get_string(name, t);
  c = calloc(i+1,sizeof(char));
  strncpy(c,t,i+1);
  free(t);
  return c;
}

IFF_EXPORT (double) iff_scaval(char *name) {
  int i; double x=0;
  i = iff_get_scalar(name, &x);
  return x;
}



