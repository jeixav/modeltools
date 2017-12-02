void RanSetSeed_generic_stream(generic_state *stream, unsigned int *piSeed, int cSeed)  ;
unsigned int IRan_generic_stream(generic_state *stream)       ;
double DRan_generic_stream(generic_state *stream)       ;
double DRanS_generic_stream(generic_state *stream)       ;
void VecIRan_generic_stream(generic_state *stream, unsigned int *ranbuf, int n)       ;
void VecDRan_generic_stream(generic_state *stream, double *ranbuf, int n)       ;
void VecDRanS_generic_stream(generic_state *stream, double *ranbuf, int n)       ;
void RanSetSeed_R250_static(unsigned int *piSeed, int cSeed)  ;
unsigned int IRan_R250_static()	  ;
void VecIRan_R250_static(unsigned int *ranbuf, int n)  ;
void RanSetSeed_R250_stream(void *stream, unsigned int *piSeed, int cSeed)  ;
void *Ran_R250_new_stream(void *clone_in, unsigned int *piSeed, int cSeed)   ;
unsigned int IRan_R250_stream(void *stream)       ;
double DRan_R250_stream(void *stream)	  ;
double DRanS_R250_stream(void *stream)	  ;
void VecIRan_R250_stream(void *stream, unsigned int *ranbuf, int n)  ;
void VecDRan_R250_stream(void *stream, double *ranbuf, int n)  ;
void VecDRanS_R250_stream(void *stream, double *ranbuf, int n)  ;
void *Ran_SHR3_new_stream(void *clone_in, unsigned int *piSeed, int cSeed)   ;
void RanSetSeed_SHR3_stream(generic_state *stream, unsigned int *piSeed, int cSeed)  ;
unsigned int IRan_SHR3_stream(generic_state *stream)	  ;
double DRan_SHR3_stream(generic_state *stream)	  ;
double DRanS_SHR3_stream(generic_state *stream)	  ;
void VecIRan_SHR3_stream(generic_state *stream, unsigned int *ranbuf, int n)  ;
void VecDRan_SHR3_stream(generic_state *stream, double *ranbuf, int n)  ;
void VecDRanS_SHR3_stream(generic_state *stream, double *ranbuf, int n)  ;
unsigned int IRan_MT19937_stream(generic_state *stream)       ;
double DRan_MT19937_stream(generic_state *stream)       ;
double DRanS_MT19937_stream(generic_state *stream)     ;
void RanNormalZigSetSeed(void *stream, void *values, int nvalues)  ;
double DRan_NormalZig_stream(void *stream)  ;
double D64Ran_NormalZig_stream(void *stream)  ;
double D64RanNormalFun(void *stream)  ;
