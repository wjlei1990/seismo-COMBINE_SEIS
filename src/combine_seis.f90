module combine_seis_subs
  
  implicit none

  double precision :: TOL=0.00001

contains

subroutine combine_seis(asdf_1, asdf_2, asdf_comb)

  use asdf_data
  use asdf_subs
  
  type(asdf_event) :: asdf_1, asdf_2, asdf_comb

  integer :: i
  integer :: ierr
  real :: dt, b
  integer :: nrecords, npts

  character(len=200) :: path1, path2

  if(asdf_1%nrecords.eq.asdf_2%nrecords) then
    nrecords=asdf_1%nrecords
    call init_asdf_data(asdf_comb, nrecords)
  else
    print *,"Records number inconsistent"
    print *,"nrecords 1 and 2:", asdf_1%nrecords, asdf_2%nrecords
    stop
  endif

  do i=1, nrecords
    ierr=0
    
    path1=trim(asdf_1%receiver_name_array(i))//'.'//&
      trim(asdf_1%network_array(i))//'.'//&
      trim(asdf_1%component_array(i))
    path2=trim(asdf_2%receiver_name_array(i))//'.'//&
      trim(asdf_2%network_array(i))//'.'//&
      trim(asdf_2%component_array(i))
    
    !check the data consistency
    if(trim(path1).ne.trim(path2)) then
      ierr=1
    endif
    asdf_comb%receiver_name_array(i)=asdf_1%receiver_name_array(i)
    asdf_comb%network_array(i)=asdf_1%network_array(i)
    asdf_comb%component_array(i)=asdf_1%component_array(i)
    asdf_comb%receiver_id_array(i)=asdf_1%receiver_id_array(i)
    !dt
    if(abs(asdf_1%sample_rate(i)-asdf_2%sample_rate(i)).gt.TOL)then
      ierr=1
    endif
    dt=asdf_1%sample_rate(i)
    asdf_comb%sample_rate(i) = dt
    !npts
    if(asdf_1%npoints(i).ne.asdf_2%npoints(i))then
      ierr=1
    endif
    npts=asdf_1%npoints(i)
    asdf_comb%npoints(i) = npts
    !begin_value
    if(abs(asdf_1%begin_value(i)-asdf_2%begin_value(i)).gt.(dt/2.))then
      ierr=1
    endif
    b=asdf_1%begin_value(i)
    asdf_comb%begin_value(i) = b

    !records
    allocate(asdf_comb%records(i)%record(npts))
    if(ierr.ne.0)then
      asdf_comb%records(i)%record(:)=0.0
    else
      asdf_comb%records(i)%record=asdf_1%records(i)%record+asdf_2%records(i)%record
    endif
  enddo

  asdf_comb%receiver_name=""
  asdf_comb%network=""
  asdf_comb%component=""
  asdf_comb%receiver_id=""
  asdf_comb%receiver_name=trim(asdf_1%receiver_name)
  asdf_comb%network=trim(asdf_1%network)
  asdf_comb%component=trim(asdf_1%component)
  asdf_comb%receiver_id=trim(asdf_1%receiver_id)

  asdf_comb%receiver_name_len=len_trim(asdf_comb%receiver_name)
  asdf_comb%network_len=len_trim(asdf_comb%network)
  asdf_comb%receiver_id_len=len_trim(asdf_comb%receiver_id)
  asdf_comb%component_len=len_trim(asdf_comb%component)

  asdf_comb%event = asdf_1%event
  asdf_comb%event_lat = asdf_1%event_lat
  asdf_comb%event_lo = asdf_1%event_lo
  asdf_comb%event_dpt = asdf_1%event_dpt
  
  asdf_comb%nreceivers = asdf_1%nreceivers

  asdf_comb%min_period = min(asdf_1%min_period,asdf_2%min_period)
  asdf_comb%max_period = max(asdf_1%max_period, asdf_2%max_period)
  
  asdf_comb%gmt_year = asdf_1%gmt_year
  asdf_comb%gmt_day = asdf_1%gmt_day
  asdf_comb%gmt_hour =asdf_1%gmt_hour
  asdf_comb%gmt_min = asdf_1%gmt_min
  asdf_comb%gmt_sec = asdf_1%gmt_sec
  asdf_comb%gmt_msec = asdf_1%gmt_msec

  !asdf_comb%npoints=asdf_1%npoints

  asdf_comb%receiver_lat = asdf_1%receiver_lat
  asdf_comb%receiver_lo = asdf_1%receiver_lo
  asdf_comb%receiver_el = asdf_1%receiver_el
  asdf_comb%receiver_dpt = asdf_1%receiver_dpt

  !asdf_comb%begin_value = asdf_1%begin_value
  asdf_comb%end_value = asdf_1%end_value
  asdf_comb%cmp_azimuth = asdf_1%cmp_azimuth
  asdf_comb%cmp_incident_ang = asdf_1%cmp_incident_ang
  !asdf_comb%sample_rate = asdf_1%sample_rate
  asdf_comb%scale_factor = asdf_1%scale_factor

  !asdf_comb% = asdf_1%
  !asdf_comb% = asdf_1%

  return

end subroutine combine_seis

end module combine_seis_subs
