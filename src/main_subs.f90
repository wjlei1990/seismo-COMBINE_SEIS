module main_subs

  implicit none

contains

subroutine read_main_parfile_mpi(rank, comm, ierr)

  use var_main

  use mpi 
  integer :: rank, comm, ierr
  
  if(rank.eq.0)then
    print *, "Read in master node:"
    call read_main_parfile(ierr)
  endif

  print *,"Bcast the par..."
  call MPI_Bcast(DEBUG, 1, MPI_LOGICAL, 0, comm, ierr)
  call MPI_Bcast(ASDF_FILE_1,150,MPI_CHARACTER,0,comm,ierr)
  call MPI_Bcast(ASDF_FILE_2,150,MPI_CHARACTER,0,comm,ierr)
  call MPI_Bcast(OUTDIR,150,MPI_CHARACTER,0,comm,ierr)

	!if(rank.eq.1) then
	!	print *, "MPI_staff"
	!	print *, RUN_FLEXWIN, RUN_MEASURE_ADJ, WRITE_ADJ_ASDF,&
	!			ROTATE_COMP, WRITE_NORMAL_OUTPUT
	!		print *, trim(OBSD_FILE), 	
	!   PRINT *, trim(MEASURE_ADJ_OUTDIR)
	!endif

end subroutine read_main_parfile_mpi


subroutine read_main_parfile(ierr)

  !read the parfile for the main(some flags)
  use var_main

  integer :: dummy_row
  integer :: ierr
  integer :: IIN=21
  integer :: i

  character(len=30) :: dummy_string

  !print *,"Read main par"
  dummy_row = 8
  
  open(UNIT=IIN,FILE="PAR_FILE_MAIN",iostat=ierr)
  if(ierr.ne.0)then
    print *,"Can't find PAR_FILE_MAIN. Stop! "
    stop
  endif

  do i=1,dummy_row
    read(IIN,*)
  enddo

  !print *,"HERE"

	read(IIN,*)
  read(IIN,3) dummy_string, DEBUG

	read(IIN,*)
	read(IIN,*)
	read(IIN,2) dummy_string, ASDF_FILE_1
	read(IIN,2) dummy_string, ASDF_FILE_2
  read(IIN, *)
  read(IIN, *)
	read(IIN,2) dummy_string, OUTDIR

	print *, "OBSD_FILE: ", trim(ASDF_FILE_1)
	print *, "SYNT_FILE: ", trim(ASDF_FILE_2)
	print *, "OUTDIR: ", trim(OUTDIR)

2 format(a,a)
3 format(a,l20)
4 format(a,i)

  close(IIN)
	!stop

end subroutine read_main_parfile

end module main_subs
