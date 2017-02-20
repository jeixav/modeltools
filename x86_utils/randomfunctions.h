void Ran_SetInitialSeeds(unsigned int auiSeed[], int cSeed, unsigned int uiSeed, unsigned int uiMin)  ;
void RanSetSeed_SHR3(void *SHR3, int *piSeed, int cSeed)  ;
unsigned int IRan_SHR3(void *SHR3)	  ;
double DRan_SHR3(void *SHR3)	  ;
double DRanS_SHR3(void *SHR3)	  ;
void VecIRan_SHR3(void *SHR3, unsigned int *ranbuf, int n)  ;
void VecDRan_SHR3(void *SHR3, double *ranbuf, int n)  ;
void VecDRanS_SHR3(void *SHR3, double *ranbuf, int n)  ;
void RanSetSeed_R250_stream(void *stream, int *piSeed, int cSeed)  ;
void *Ran_R250_new_stream(void *clone_in, int *piSeed, int cSeed)   ;
unsigned int IRan_R250_stream(void *stream)	  ;
double DRan_R250_stream(void *stream)	  ;
double DRanS_R250_stream(void *stream)	  ;
void VecIRan_R250_stream(void *stream, unsigned int *ranbuf, int n)  ;
void VecDRan_R250_stream(void *stream, double *ranbuf, int n)  ;
void VecDRanS_R250_stream(void *stream, double *ranbuf, int n)  ;
void RanSetSeed_MWC8222(void *MWC8222, int *piSeed, int cSeed)  ;
unsigned int IRan_MWC8222(void *MWC8222)  ;
double DRan_MWC8222(void *MWC8222)         ;
double DRanS_MWC8222(void *MWC8222)        ;
void VecIRan_MWC8222(void *MWC8222, unsigned int *auiRan, int cRan)  ;
void VecDRan_MWC8222(void *MWC8222, double *adRan, int cRan)  ;
double  DRanNormalZigVec(void *stream)  ;
void  RanNormalSetSeedZig(void *stream, int *piSeed, int cSeed)  ;
void  RanNormalSetSeedZigVec(void *stream, int *piSeed, int cSeed)  ;
int NormalFromUniform(double gaussian[], int *ngauss, int uniform[], int nuni)  ;