!> @file
!! Program: Global_Tomography_Data_Processing
!! Developer: Princeton Global Tomography Group(PGTG)
!! Group Member: Wenjie Lei(lei@princeton.edu), Ebru Bozdag(bozdag@princeton.edu),
!! James A. Smith(jas11@princeton.edu)
!! Bug Report: lei@princeton.edu

program main

  use asdf_data
  use var_main

  use asdf_subs

	use main_subs
  use combine_seis_subs

  use mpi
  implicit none

  type(asdf_event)        :: my_asdf_1, my_asdf_2 
  type(asdf_event)        :: my_asdf_comb

  !mpi_var
  integer                 :: nproc,comm,rank
  integer                 :: ierr,adios_err

  integer :: i
	real    :: t1, t2, t3, t4

	call CPU_TIME(t1)
	!----------.
  !init mpi  !
	!----------'
  call mpi_init(ierr)
  call mpi_comm_dup(mpi_comm_world,comm,ierr)
  call mpi_comm_rank(comm,rank,ierr)
  call mpi_comm_size(comm,nproc,ierr)
  if(rank.eq.0) print *, "Start Combine..."

  !--------------------------.
  !read main parfile         !
  !--------------------------'
  if(rank.eq.0) print *,"Read in main Parfile..."
  call read_main_parfile_mpi(rank,comm,ierr)
	!stop

  !--------------------------.
  !read in asdf data         !
  !--------------------------'
	if(rank.eq.0) then
   	print *,"-----------------"
   	print *,"Read ASDF file"
   	print *,"-----------------"
	endif
	if(rank.eq.0) then
  	print *, "ASDF FILE 1: ",trim(ASDF_FILE_1)
		print *, "ASDF FILE 2: ",trim(ASDF_FILE_2)
	endif
  call read_asdf_file(ASDF_FILE_1,my_asdf_1,rank,nproc,comm,ierr)
  print *, "read obsd finished!"
  call read_asdf_file(ASDF_FILE_2,my_asdf_2,rank,nproc,comm,ierr)
  print *, "read synt finished!"
	if(rank.eq.0) then
  	print *, "/event:", trim(my_asdf_1%event)
	endif
  !stop

	call MPI_Barrier(comm,ierr)

  !--------------------------.
  !combine                   !
  !--------------------------'
	if(rank.eq.0) then
   	print *,"-----------------"
   	print *,"Begin Combine"
   	print *,"-----------------"
	endif

  !!if(my_asdf_1%nrecords.eq.my_asdf_2%nrecords) then
  !  call init_asdf_data(my_asdf_comb, my_asdf_1%nrecords)
  !else
  !  print *,"Records number inconsistent"
  !  print *,"nrecords 1 and 2:", my_asdf_1%nrecords, my_asdf_2%nrecords
  !  stop
  !endif

  !do i=1, my_asdf_1%nrecords
   !call combie_seis subroutine
  call combine_seis(my_asdf_1, my_asdf_2, my_asdf_comb)
  !enddo

  !--------------------------.
  !Write out
  !--------------------------.
  call system('mkdir -p '//trim(OUTDIR)//'')
  ASDF_FILE_COMB=trim(OUTDIR)//'/'//trim(my_asdf_comb%event)//"_adj_comb.bp"
  print *, "Combined file name: ", trim(ASDF_FILE_COMB)
  call write_asdf_file(ASDF_FILE_COMB, my_asdf_comb, rank, nproc, comm, ierr)

  !--------------------------.
  !finalize mpi              !
  !--------------------------'
  call MPI_Barrier(comm,ierr)
  call mpi_finalize(ierr)

	call CPU_TIME(t2)

	open(unit=22, file='cpu_time')
	write(22, *) "rank, time:", rank, t2-t1
	close(22)

end program main
