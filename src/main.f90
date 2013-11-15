!> \file main.f90
!! \Generates adios bp files for observed and synthetic seismograms.
!! \author WL & JAS

program sac_to_asdf

    use adios_write_mod
    use seismo_variables
		use main_parameter
		use asdf_data
		use main_subs
		use asdf_subs
		use source_info

    implicit none
    include 'mpif.h'

		type(asdf_event) :: obsd_all, synt_all

		type(cmt_struct) :: cmtsolution

    integer                 :: irec=1, pmin, pmax
    integer                 :: comm, rank, i, ierr, nproc, adios_err
    !integer(kind=8)         :: adios_groupsize, adios_totalsize, varid
    !integer(kind=8)         :: adios_handle, adios_group
    integer(kind=8)         :: adios_group
    real                    :: nerr

		!character(len=13), allocatable :: event_list(:)
		character(len=8), allocatable :: station_list(:), nw_list(:)
		integer, allocatable :: exist_list(:), station_exist_list(:)
		real, allocatable :: lat_list(:), lon_list(:)
		real, allocatable :: depth_list(:), elevation_list(:)
		!integer :: n_events

		character(len=120) :: OBSD_FILE, SYNT_FILE
		character(len=10) :: MAX_PERIOD_STRING, MIN_PERIOD_STRING

    call MPI_Init (ierr)
    call MPI_Comm_dup (MPI_COMM_WORLD, comm, ierr)
    call MPI_Comm_rank (comm, rank, ierr)
    call MPI_Comm_size (comm, nproc, ierr)

    call adios_init_noxml (comm, adios_err)
    call adios_allocate_buffer (600, adios_err)
    call adios_declare_group (adios_group, "EVENTS", "iter", 1, adios_err)
    call adios_select_method (adios_group, "MPI", "", "", adios_err)

    call read_parameter_file()

    !call read_event_list(event_list, n_events)

    !do earthquake = 1, n_events
      !generate the station list
			call generate_station_list(station_list, nw_list, lat_list, &
						lon_list, elevation_list, depth_list, &
					 	exist_list, station_exist_list, event_name)
      !read in cmtsolution file
			!@call read_cmt_file(event_list(earthquake), cmtsolution)
      !init asdf data structure
			call init_asdf_data(obsd_all, n_records)
			call init_asdf_data(synt_all, n_records)

      !obsd_all
			call copy_sac_to_asdf_data(obsd_all, station_list, nw_list, &
            exist_list, station_exist_list, &
					 	lat_list, lon_list, elevation_list, depth_list, & 
						event_name, obsd_dir, obsd_file_prefix)
			!call copy_cmt_to_asdf_data(obsd_all, cmtsolution)
			obsd_all%min_period = MIN_PERIOD
			obsd_all%max_period = MAX_PERIOD

      !synt_all
			call copy_sac_to_asdf_data(synt_all, station_list, nw_list, &
            exist_list, station_exist_list, &
						lat_list, lon_list, elevation_list, depth_list, &
						event_name, synt_dir, synt_file_prefix)
			!call copy_cmt_to_asdf_data(synt_all, cmtsolution)
			synt_all%min_period = MIN_PERIOD
			synt_all%max_period = MAX_PERIOD

			write(MIN_PERIOD_STRING, '(I6)') int(MIN_PERIOD)
			write(MAX_PERIOD_STRING, '(I6)') int(MAX_PERIOD)
			MIN_PERIOD_STRING = (adjustl(MIN_PERIOD_STRING))
			MAX_PERIOD_STRING = (adjustl(MAX_PERIOD_STRING))

			print *,"===================="
			print *,"Begin write out"
			!Write OBSD_FILE
      !adios_groupsize = 0
			OBSD_FILE=trim(event_name)//"_"//trim(MIN_PERIOD_STRING)//&
									"_"//trim(MAX_PERIOD_STRING)//"_obsd.bp"
			!print *,"Write out file: ", trim(OBSD_FILE)
      call write_asdf_file (OBSD_FILE, obsd_all, adios_group,&
              rank, nproc, comm, ierr)

			!Write SYNT_FILE
      !adios_groupsize = 0
			SYNT_FILE=trim(event_name)//"_"//trim(MIN_PERIOD_STRING)//&
									"_"//trim(MAX_PERIOD_STRING)//"_synt.bp"
			!print *,"Write out file: ", trim(SYNT_FILE)
      !write (SYNT_FILE,'(a,"_synt.bp")') trim (event_list(earthquake))
      call write_asdf_file (SYNT_FILE, synt_all, adios_group, &
              rank, nproc, comm, ierr)
			print *,"Finish writing"
			print *,"===================="

    !enddo

    call MPI_Barrier (comm, ierr)
    call adios_finalize (rank, adios_err)
    call MPI_Finalize (ierr)

end program
