CC=s.cc
FC=s.f90

objects = c_baseio.o dummy.o f_baseio.o

f_baseio_test : f_baseio_test.F90 $(objects)
	$(FC) -o f_baseio_test $< $(objects)

f_baseio.o : f_baseio.F90
	$(FC) $(FFLAGS) -c $<

.PHONY : clean
clean :
	rm f_baseio_test $(objects)
