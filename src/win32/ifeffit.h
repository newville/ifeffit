/*************************
 * C header file Ifeffit *
 *************************/

/* main interface routines */
#define IFF_EXPORT(a) __declspec(dllexport) a _stdcall


IFF_EXPORT(int)     iff_exec(char *);
IFF_EXPORT(int)     ifeffit(char *);
IFF_EXPORT(int)     iff_get_string(char *, char *);
IFF_EXPORT(int)     iff_put_string(char *, char *);
IFF_EXPORT(int)     iff_get_scalar(char *, double *);
IFF_EXPORT(int)     iff_put_scalar(char *, double *);
IFF_EXPORT(int)     iff_get_array(char *, double *);
IFF_EXPORT(int)     iff_put_array(char *, int *, double *);
IFF_EXPORT(int)     iff_get_echo(char *);
IFF_EXPORT(char*)   iff_strval(char *);
IFF_EXPORT(double)  iff_scaval(char *);



