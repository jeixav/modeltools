!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!
program self_test
  use ISO_C_BINDING
  implicit none
  integer :: status, iun1, iun2, iun3, errors, nw
  integer*8 :: nblks, fsize
  integer :: i
  integer*8 :: wa8
  integer, external :: fnom, fclos, existe, wasize, numblks, waread2, wawrit2
  integer*8, external :: numblks64, wasize64
  integer, dimension(4) :: buf
  integer, dimension(512) :: buf512

  iun1 = 0
  status = fnom(iun1,'./fortran_formatted','SEQ+FMT+FTN+APPEND',0)
  ! FIXME Abort if non-zero status
  print '("iun1 = ",I0,", fnom status = ",I0)', iun1,status
  write(iun1,*)'ceci est un test'
  status = fclos(iun1)
  print '("fclos(iun1) = ",I0)',status

  iun2 = 0
  status = fnom(iun2,'./fortran_unformatted','SEQ+UNF+FTN+APPEND',0)
  print '("iun2 = ",I0,", fnom status = ",I0)', iun1,status
  write(iun2)(i,i=0,3)
  status = fclos(iun2)
  print '("fclos(iun2) = ",I0)',status

  iun3 = 0
  status = fnom(iun3,'fortran_d77','UNF+FTN+D77+SCRATCH',5)
  print '("iun3 = ",I0,", fnom status = ",I0)', iun3,status
  buf = 1
  write(iun3,rec=1)buf
  buf = 2
  write(iun3,rec=2)buf
  call system("ls -l $TMPDIR")
  status = fclos(iun3)
  print '("fclos(iun3) = ",I0)',status

  iun1 = 0
  status = fnom(iun1,'./fortran_wa','RND+SPARSE',0)
  print '("iun1 = ",I0,", fnom status(wa) = ",I0)',iun1,status
  call waopen(iun1)
  buf = 1
  call wawrit(iun1,buf,1,4)
  wa8 = 1024*1024
  wa8 = 1024*wa8*4
  call wawrit64(iun1,buf,wa8,4,0)  ! sparse write at very high address
  buf = 2
  nw = wawrit2(iun1,buf,6,4)
  if(nw == 4) print *,'wawrit2 OK at 4'
  call waclos(iun1)
  call waopen(iun1)
  nblks = numblks64(iun1)
  fsize = wasize64(iun1)
  print '("iun1(fortran_wa) = ",I0,", numblks = ",I0,", fsize = ",I0)',iun1, nblks, fsize
  call waclos(iun1)
  ! FIXME Is fclose required after waclos?
  status = fclos(iun1)
  print '("fclos(iun1) = ",I0)',status

  iun2 = 0
  status = fnom(iun2,'./fortran_da','RND',0)
  print '("iun2 = ",I0,", fnom status(da) = ",I0)',iun2,status
  call openda(iun2)
  buf512 = 1
  call writda(iun2,buf512,1,1)
  buf512 = 2
  call writda(iun2,buf512,1,2)
  call closda(iun2)
  status = fclos(iun2)
  print '("fclos(iun2) = ",I0)',status

  iun3 = 0
  status = fnom(iun3,'./fortran_da','RND',0)
  print '("iun3 = ",I0,", fnom status(da) = ",I0)',iun3,status
  call waopen(iun3)
  errors = 0
  buf512 = 0
  call waread(iun3,buf512,1,512)
  do i=1,512
     if(buf512(i) .ne. 1) errors = errors + 1
  enddo
  buf512 = 0
  nw = waread2(iun3,buf512,513,512)
  if(nw == 512) print *,'waread2 OK at 512'
  do i=1,512
     if(buf512(i) .ne. 2) errors = errors + 1
  enddo
  print '("iun3 = ",I0,", errors(waread) = ",I0)',iun3,errors
  call waclos(iun3)
  status = fclos(iun3)
  print '("fclos(iun3) = ",I0)',status

  iun3 = 0
  status = fnom(iun3,'./fortran_da','RND+OLD+R/O',0)
  print '("iun3 = ",I0,", fnom status(da) = ",I0)',iun3,status
  call openda(iun3)
  errors = 0
  buf512 = 0
  call readda(iun3,buf512,1,1)
  do i=1,512
     if(buf512(i) .ne. 1) errors = errors + 1
  enddo
  buf512 = 0
  call readda(iun3,buf512,1,2)
  do i=1,512
     if(buf512(i) .ne. 2) errors = errors + 1
  enddo
  nblks = numblks(iun3)
  fsize = wasize(iun3)
  print '("iun3 = ",I0,", errors(readda) = ",I0,", numblks = ",I0,", fsize = ",I0)',iun3,errors,nblks,fsize
  call closda(iun3)
  ! FIXME Abort if non-zero return value

  iun3 = 0
  status = fnom(iun3,'./fortran_da','RND',0)
  print '("iun3 = ",I0,", fnom status(da) = ",I0)',iun3,status
  call waopen(iun3)
  errors = 0
  buf512 = 0
  call waread(iun3,buf512,1,512)
  do i=1,512
     if(buf512(i) .ne. 1) errors = errors + 1
  enddo
  buf512 = 0
  call waread(iun3,buf512,513,512)
  do i=1,512
     if(buf512(i) .ne. 2) errors = errors + 1
  enddo
  print '("errors = ",I0)',errors
  call waclos(iun3)
  status = fclos(iun3)
  print '("fclos(iun3) = ",I0)',status

  status = existe('./fortran_formatted')
  print '("existe(./fortran_formatted) = ",I0)',status
  status = existe('./fortran_unformatted')
  print '("existe(./fortran_unformatted) = ",I0)',status
  status = existe('./fortran_da')
  print '("existe(./fortran_da) = ",I0)',status
  status = existe('./fortran_wa')
  print '("existe(./fortran_wa) = ",I0)',status
  status = existe('./iepala')
  print '("existe(./iepala) = ",I0)',status

end program self_test
!
! iun        : if zero upon entry, fnom will find an appropriate unit number
! path       : file name (character string)
!              some_name@file_path refers to file some_name inside CMCARC archive file_path
! options    : list of + separated options (upper case or lower case)
!              STD         RPN "standard" file (implies WA+RND)
!              FTN         Fortran file (UNF, D77 may be used as sub attributes)
!              D77         Fortran direct access file (lrec must be non zero)
!              UNF         Fortran sequential unformatted file (default is formatted)
!              RND         random access file (normally used with STD)
!              WA          Word Addressable file (Big Endian) (implies RND)
!              STREAM      stream file (non Fortran, no record markers, Big Endian
!              BURP        Meteorological reports file
!              OLD         file must exist (applies to all files)
!              R/O         file is Read Only (default is Read/Write) (applies to all files) (implies OLD)
!              R/W         file is Read Write (default) (applies to all files)
!              SCRATCH     File will be removed when closed (applies to all files)
!              SPARSE      Unix WA sparse file (may be written into far beyond end of file)
!              PAGED       special type of WA file (not implemented yet)
!              REMOTE      file is on another system and accessed with a ssh (applies to all files)
!              ex.   STD+RND+OLD+R/W open existing random standard file for reading and writing
!                    FTN+UNF         open Fortran sequential file for reading and writing , create it if it does not exist
! lrec       : record length in 4 byte integers for Fortran D77 file records (should be zero otherwise)
!
