#ifdef __cplusplus
extern "C" {
#endif

#if defined (sun)
int MAIN_ () { return 0; }
#elif defined (linux) && defined(__ELF__)
int MAIN__ () { return 0; }
#endif

#ifdef __cplusplus
}
#endif
