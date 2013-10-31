module main_subs
!> Defines adios data structure
!! \param adios_group adios group
!! \param my_group_size Stores the adios group size

contains

subroutine read_parameter_file()

	use main_parameter
  implicit none

  integer, parameter :: IIN = 11
  integer, parameter :: NHEAD = 8
  integer :: ios

  integer :: idummy,i
  character(len=28) :: junk
	character :: dummy

  open(unit=IIN,file='PAR_FILE',status='old')

  ! ignore header
  do idummy=1,NHEAD
    read(IIN,*)
  enddo


	read(IIN,3) junk, DEBUG
	read(IIN,3) junk, CHECK_STATION
	read(IIN,3) junk, CHECK_CMT
	read(IIN,3) junk, CHECK_VAR
	read(IIN,*)
	read(IIN,*)
	read(IIN,2) junk, CMT_DIR
	read(IIN,2) junk, STATION_DIR
	read(IIN,2) junk, OBSD_DIR
	read(IIN,2) junk, SYNT_DIR
	read(IIN,2) junk, OBSD_FILE_PREFIX
	read(IIN,2) junk, SYNT_FILE_PREFIX
	read(IIN,*) 
	read(IIN,*)  
	read(IIN,4) junk, min_period 
	read(IIN,4) junk, max_period
	read(IIN,*) 
	read(IIN,*)  
	read(IIN,2) junk, comp_all(1)
	read(IIN,2) junk, comp_all(2)
	read(IIN,2) junk, comp_all(3)

  close(IIN)
	
	print *,"DEBUG:", DEBUG
	if(DEBUG) then
    print *, "CHECK_STATION: ", CHECK_STATION
    print *, "CHECK_CMT: ", CHECK_CMT
    print *, "CHECK_VAR: ", CHECK_VAR
		print *, "CMT_DIR:", trim(CMT_DIR), " STATION_DIR:", trim(STATION_DIR)
		print *, "OBSD_DIR:", trim(OBSD_DIR), " SYNT_DIR:", trim(SYNT_DIR)
		print *, "OBSD_FILE_PREFIX:", trim(OBSD_FILE_PREFIX)
		print *, "SYNT_FILE_PREFIX:", trim(SYNT_FILE_PREFIX)
		print *, "MIN and MAX PERIOD:", min_period, max_period
		print *, "Three Components:  ", trim(COMP_ALL(1)),"  ", &
							trim(COMP_ALL(2)), "  ", &
							trim(COMP_ALL(3))
	endif

2 format(a,a)
3 format(a,l20)
4 format(a,F)


end subroutine read_parameter_file

subroutine read_event_list(event_list, n_events)

  implicit none

	character(len=13), allocatable :: event_list(:)
	integer :: n_events

  integer, parameter :: IIN = 11
!  integer, parameter :: NHEAD = 12
!  integer ios

  integer :: i

	open(unit=IIN, file="event_list")

  read(IIN,*) n_events
	print *,"n_events:", n_events

	allocate(event_list(n_events))

  do i = 1, n_events
    read(IIN,'(a)') event_list(i)
		print *,"i, event_name:",i, trim(event_list(i))
  enddo
  close(IIN)

end subroutine

!> Gets the number of receivers from the STATIONS file
!! \param nreceivers The number of receivers.

!subroutine get_nreceivers ()
!
!  use seismo_variables
!  implicit none
!
!  integer :: stat
!
!  nreceivers = 0
!  open(10, file=STATIONS_FILE, status='old')
!  do
!    read(10, *, iostat=stat)
!    if (stat /= 0) exit
!    nreceivers = nreceivers + 1
!  end do
!  close(10)
!
!end subroutine get_nreceivers

subroutine generate_station_list(station_list, nw_list, lat_list, &
						lon_list, elevation_list, depth_list, &
						exist_list, station_exist_list, event_name)

	use main_parameter
	use seismo_variables, only : n_stations, n_records
	implicit none

	character(len=8), allocatable :: station_list(:), nw_list(:)
	integer, allocatable :: exist_list(:), station_exist_list(:)
	real, allocatable :: lat_list(:), lon_list(:)
	real, allocatable :: elevation_list(:), depth_list(:)
	character(len=13) :: event_name

	integer :: IIN=20
	character(len=150) :: station_file
	integer :: stat

	integer :: i,j
  character(len=150) :: synt_file, obsd_file
	logical :: obsd_exist, synt_exist

	
	station_file=trim(STATION_DIR)//"/STATIONS_"//trim(event_name)
	!print *,trim(STATION_DIR)
	!print *,trim(station_file)

	open(unit=IIN, file=trim(station_file), status='old')
	n_stations=0
	do
		read(IIN, *, iostat=stat)
		!print *,"n_stations"
		if(stat /= 0) exit
		n_stations=n_stations+1
	enddo
	close(IIN)

	!print *, "success"

	allocate(station_list(n_stations))
	allocate(nw_list(n_stations))
	allocate(lat_list(n_stations))
	allocate(lon_list(n_stations))
	allocate(depth_list(n_stations))
	allocate(elevation_list(n_stations))
	!3 components
	allocate(exist_list(3*n_stations))
  allocate(station_exist_list(n_stations))

	open(unit=IIN, file=station_file, status='old')
	do i=1,n_stations
		read(IIN,*)station_list(i), nw_list(i), lat_list(i), lon_list(i),&
								elevation_list(i), depth_list(i)
	enddo

	if(DEBUG) then
		print *, "All Station info:"
		print *, "n_stations:", n_stations
		do i=1, n_stations
			print *, trim(station_list(i)), "  ",trim(nw_list(i)), &
					lat_list(i), lon_list(i)
		enddo
	endif

	exist_list(:)=0
  station_exist_list(:)=0

	do i=1,n_stations
		do j=1,3
			synt_file=trim(station_list(i))//"."//trim(nw_list(i))//"."&
					//trim(comp_all(j))//"."//trim(synt_file_prefix)
			obsd_file=trim(station_list(i))//"."//trim(nw_list(i))//"."&
					//trim(comp_all(j))//"."//trim(obsd_file_prefix)
			
			synt_file=trim(SYNT_DIR)//"/"//trim(event_name)//"/"//trim(synt_file)
			obsd_file=trim(OBSD_DIR)//"/"//trim(event_name)//"/"//trim(obsd_file)

			inquire(file=synt_file, exist=synt_exist)
			inquire(file=obsd_file, exist=obsd_exist)

			if(synt_exist.and.obsd_exist)then
				exist_list(3*(i-1)+j)=1
        station_exist_list(i)=1
			endif

			if(DEBUG)then
				print *,"obsd_file:", trim(obsd_file)," ---exist:", obsd_exist
				print *,"synt_file:", trim(synt_file)," ---exist:", synt_exist
			endif

		enddo
	enddo

	n_records=sum(exist_list(1:(3*n_stations)))
  !n_stations=sum(station_exist_list(1:n_stations))

	print *, "n_records available:", n_records
  print *, "n_stations available:", n_stations

end subroutine generate_station_list


subroutine read_cmt_file(event_name, cmt)
		
	use main_parameter, only : CMT_DIR
	use source_info

	character(len=*), intent(in) :: event_name
	type(cmt_struct) :: cmt

	integer :: IIN=20, ierr
	character(len=256) :: cmt_file, string

	cmt_file=trim(CMT_DIR)//"/CMTSOLUTION_"//trim(event_name)
	write(*,*)"CMT File: ",trim(cmt_file)

	open(unit=IIN, file=cmt_file, status='old', iostat=ierr)
	if(ierr.ne.0)then
		write(*,*) "CMT file not found!"
		stop
	endif

  read(IIN,"(a256)") string
  ! skips empty lines
  do while( len_trim(string) == 0 )
  	read(IIN,"(a256)") string
  enddo
  ! read header with event information
  read(string,"(a4,i5,i3,i3,i3,i3,f6.2)") cmt%datasource, cmt%gmt_year, &
      cmt%gmt_month, cmt%gmt_day, cmt%gmt_hour, cmt%gmt_min, cmt%gmt_sec

  ! read event name 
  read(IIN,"(a)") string
  read(string(12:len_trim(string)),*) cmt%event_name
  ! read time shift
  read(IIN,"(a)") string
  read(string(12:len_trim(string)),*) cmt%time_shift
  ! read half duration
  read(IIN,"(a)") string
  read(string(15:len_trim(string)),*) cmt%half_duration
  ! read latitude
  read(IIN,"(a)") string
  read(string(10:len_trim(string)),*) cmt%latitude
  ! read longitude
  read(IIN,"(a)") string
  read(string(11:len_trim(string)),*) cmt%longitude
  ! read depth
  read(IIN,"(a)") string
  read(string(7:len_trim(string)),*) cmt%depth
  ! read Mrr
  read(IIN,"(a)") string
  read(string(5:len_trim(string)),*) cmt%Mrr
  ! read Mtt
  read(IIN,"(a)") string
  read(string(5:len_trim(string)),*) cmt%Mtt
  ! read Mpp
  read(IIN,"(a)") string
  read(string(5:len_trim(string)),*) cmt%Mpp
  ! read Mrt
  read(IIN,"(a)") string
  read(string(5:len_trim(string)),*) cmt%Mrt
  ! read Mrp
  read(IIN,"(a)") string
  read(string(5:len_trim(string)),*) cmt%Mrp
  ! read Mtp
  read(IIN,"(a)") string
  read(string(5:len_trim(string)),*) cmt%Mtp
  close(IIN)

	print *,"======================================"
	print *,"CMTSOLUTION file info:"
	print *,"Time info: ", cmt%gmt_year, cmt%gmt_month, cmt%gmt_day,&
		cmt%gmt_hour, cmt%gmt_min, cmt%gmt_sec
	print *, "event_name: ", trim(cmt%event_name)
	print *, "latitude and longitude:", cmt%latitude, cmt%longitude
	print *, "depth: ", cmt%depth
	print *, "PARA: ", cmt%Mrr, cmt%Mtt, cmt%Mpp, cmt%Mrt, cmt%Mrp, cmt%Mtp
	print *,"======================================"

end subroutine read_cmt_file


subroutine copy_sac_to_asdf_data(my_asdf, station_list, nw_list,&
							exist_list, station_exist_list,&
              sta_lat, sta_lon, sta_ele, sta_dep, &
							event_name, data_dir, prefix)

	use asdf_data
	use seismo_variables
	use main_parameter
	use asdf_subs
	
	type(asdf_event) :: my_asdf
	character(len=*) :: station_list(:)
	character(len=*) :: nw_list(:)

	integer :: exist_list(:), station_exist_list(:)
	real :: sta_lat(:), sta_lon(:)
	real :: sta_ele(:), sta_dep(:)
	
	character(len=*) :: event_name, data_dir, prefix

	character(len=200) :: fn

	integer :: i, j, loc, asdf_index, dim_info

	!character(len=3) :: comp_all(3)

	!container var
	real, dimension(NDATAMAX) :: displ
	character(len=:), allocatable :: my_receiver_name, my_network
	character(len=:), allocatable :: my_component, my_receiver_id 
	integer :: m

	integer :: nerr
  integer :: nerr_files=0

	allocate(character(len=6*3*n_records)::my_receiver_name)
	allocate(character(len=6*3*n_records)::my_network)
	allocate(character(len=6*3*n_records)::my_component)
	allocate(character(len=6*3*n_records)::my_receiver_id)
	my_receiver_name=""
	my_network=""
	my_component=""
	my_receiver_id=""

  my_asdf%nreceivers = sum(station_exist_list(:))

	asdf_index=0
	do i=1, n_stations
	!i is station name
		do j=1, 3
		!j is comp name
			loc=3*(i-1)+j
			if(exist_list(loc).eq.1)then
				asdf_index=asdf_index+1
				fn=trim(data_dir)//"/"//trim(event_name)//"/"//trim(station_list(i))&
						//"."//trim(nw_list(i))//"."//trim(comp_all(j))//"."//trim(prefix)

				print *,"Read in fn:", trim(fn)
        print *,"asdf_index: ", asdf_index
    		call rsac1(trim(adjustl(fn)), displ, npoints, B, DELTA, &
      			   NDATAMAX, nerr)

        if(nerr.ne.0)then
          !Read in sac file fails...
          print *, "Error read in sac file: ", trim(fn)
				  allocate(my_asdf%records(asdf_index)%record(1))
    		  my_asdf%npoints(asdf_index) = 0
          nerr_files=nerr_files+1
          cycle
        else
				  allocate(my_asdf%records(asdf_index)%record(npoints))
    		  my_asdf%npoints(asdf_index) = npoints
				  my_asdf%records(asdf_index)%record(1:npoints) = displ(1:npoints)
          my_asdf%sample_rate(asdf_index) = DELTA
          my_asdf%begin_value(asdf_index) = B

!    		  if (i == 1) then
      	  call getnhv('NZYEAR', my_asdf%gmt_year(asdf_index), nerr)
      	  call getnhv('NZJDAY', my_asdf%gmt_day(asdf_index), nerr)
      	  call getnhv('NZHOUR', my_asdf%gmt_hour(asdf_index), nerr)
      	  call getnhv('NZMIN', my_asdf%gmt_min(asdf_index), nerr)
      	  call getnhv('NZSEC', my_asdf%gmt_sec(asdf_index), nerr)
      	  call getnhv('NZMSEC', my_asdf%gmt_msec(asdf_index), nerr)
!    		  endif

          !call getfhv('EVLA',my_asdf%event_lat(asdf_index), nerr)
          !call getfhv('EVLO',my_asdf%event_lo(asdf_index), nerr)
          !call getfhv('EVDP',my_asdf%event_dpt(asdf_index), nerr)

          !call getfhv('',my_asdf%, nerr)
          !call getfhv('',my_asdf%, nerr)

    		  call getfhv('STLA', my_asdf%receiver_lat(asdf_index), nerr)
    		  call getfhv('STLO', my_asdf%receiver_lo(asdf_index), nerr)
   			  call getfhv('STEL', my_asdf%receiver_el(asdf_index), nerr)
    		  call getfhv('STDP', my_asdf%receiver_dpt(asdf_index), nerr)

    		  call getkhv('kstnm', my_asdf%receiver_name_array(asdf_index), nerr)
    		  call getkhv('kcmpnm', my_asdf%component_array(asdf_index), nerr)
    		  call getkhv('knetwk', my_asdf%network_array(asdf_index), nerr)
          !print *, "my_asdf%component", trim(my_asdf%component_array(asdf_index))
      	  call getkhv('khole', my_asdf%receiver_id_array(asdf_index), nerr)

!    		  if (sac_type == 1) then
          !call getfhv('B',my_asdf%begin_value(asdf_index), nerr)
          call getfhv('E', my_asdf%end_value(asdf_index), nerr)

      	  call getfhv('t1',my_asdf%P_pick(asdf_index),nerr)
      	  call getfhv('t2',my_asdf%S_pick(asdf_index),nerr)
!    		  elseif (sac_type == 2) then
!      		  E = 6000.0
!    		  endif
    		  call getfhv('CMPAZ', my_asdf%cmp_azimuth(asdf_index), nerr)
    		  call getfhv('CMPINC', my_asdf%cmp_incident_ang(asdf_index), nerr)
    		  call getfhv('SCALE', my_asdf%scale_factor(asdf_index), nerr)

    		  call getfhv('AZ',my_asdf%ev_to_sta_AZ(asdf_index),nerr)
    		  call getfhv('BAZ',my_asdf%sta_to_ev_AZ(asdf_index),nerr)
    		  call getfhv('GCARC',my_asdf%great_circle_arc(asdf_index),nerr)
          call getfhv('DIST',my_asdf%dist(:),nerr)

        endif

				!station info
    		my_receiver_name = trim(my_receiver_name)//&
          trim(my_asdf%receiver_name_array(asdf_index))//'.'
    		my_network = trim(my_network)//&
          trim(my_asdf%network_array(asdf_index))//'.'
    		my_component = trim(my_component)//&
          trim(my_asdf%component_array(asdf_index))//'.'
    		my_receiver_id = trim(my_receiver_id)//&
          trim(my_asdf%receiver_id_array(asdf_index))//'.'

			endif
		enddo
  enddo

  !print *,"nerr_files: ", nerr_files
  !print *,"check:", trim(my_receiver_name)
  !print *,"check:", trim(my_component)
  !stop

  ! get the length of the strings
  my_asdf%receiver_name_len = len_trim(my_receiver_name)
  my_asdf%network_len = len_trim(my_network)
  my_asdf%component_len = len_trim(my_component)
  my_asdf%receiver_id_len = len_trim(my_receiver_id)

  my_asdf%receiver_name = ""
  my_asdf%network = ""
  my_asdf%component = ""
  my_asdf%receiver_id = ""

	my_asdf%receiver_name = my_receiver_name
	my_asdf%network = my_network
	my_asdf%component = my_component
	my_asdf%receiver_id = my_receiver_id

  !print *, "my_asdf%component+all:",trim(my_asdf%component)

	call split_string(my_asdf%receiver_name, my_asdf%receiver_name_len, &
					my_asdf%receiver_name_array, dim_info, '.')
	call split_string(my_asdf%network, my_asdf%network_len, &
					my_asdf%network_array, dim_info, '.')
	call split_string(my_asdf%component, my_asdf%component_len, &
					my_asdf%component_array, dim_info, '.')
	call split_string(my_asdf%receiver_id, my_asdf%receiver_id_len, &
					my_asdf%receiver_id_array, dim_info, '.')

	my_asdf%event=event_name

	!print *,"my_receiver_name",trim(my_receiver_name)
	!print *,"my_network", trim(my_network)
	!print *,"my_receiver_id:", trim(my_receiver_id)

end subroutine copy_sac_to_asdf_data

subroutine copy_cmt_to_asdf_data(my_asdf, cmt)

	use source_info
	use asdf_data

	type(cmt_struct), intent(in) :: cmt 
	type(asdf_event) :: my_asdf

!	my_asdf%gmt_year 	= cmt%gmt_year
!	my_asdf%gmt_month = cmt%gmt_month
!	my_asdf%gmt_day		= cmt%gmt_day
!	my_asdf%gmt_hour 	= cmt%gmt_hour
!	my_asdf%gmt_min 	= cmt%gmt_min
!	my_asdf%gmt_sec   = int(cmt%gmt_sec)
!
	my_asdf%event_lat = cmt%latitude
	my_asdf%event_lo  = cmt%longitude
	my_asdf%event_dpt = cmt%depth

	!print *, "check"
	!print *, cmt%gmt_year, my_asdf%gmt_year

	!stop

end subroutine copy_cmt_to_asdf_data

end module main_subs
