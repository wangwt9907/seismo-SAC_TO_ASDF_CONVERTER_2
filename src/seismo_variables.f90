module seismo_variables

  ! -------------------------------------------------------------
  !logical :: DEBUG
  ! directories
  !character(len=100) :: WORKING_DIR
  !character(len=100) :: OBSD_FILE, SYNT_FILE
  !character(len=100) :: CMT_FILE, STATIONS_FILE
  integer            :: NDATAMAX=300000
  ! -------------------------------------------------------------

  integer                 :: n_stations, n_records


  character(len=5)        :: receiver_name, component
  character(len=5)        :: network, receiver_id
  integer                 :: receiver_name_len, component_len, network_len
  integer                 :: receiver_id_len, earthquake

  real                    :: event_lat, event_lo, event_dpt

  integer                 :: gmt_year, gmt_day, gmt_hour, gmt_min, gmt_sec, gmt_msec 

  real                    :: sta_la, sta_lo, sta_el, sta_dp

  integer                 :: npoints
  real                    :: B, E, DELTA, sac_scale

  real                    :: P_pick, S_pick
  real                    :: cmp_az, cmp_inc
  real                    :: gcarc, azimuth, baz

end module seismo_variables

module main_parameter

	logical :: DEBUG, CHECK_STATION, CHECK_CMT, CHECK_VAR

	character(len=150) :: CMT_DIR, STATION_DIR, OBSD_DIR, SYNT_DIR
	character(len=32)	 :: OBSD_FILE_PREFIX, SYNT_FILE_PREFIX
	real :: MIN_PERIOD, MAX_PERIOD

	character(len=3), dimension(3) :: comp_all

end module main_parameter

module source_info

	type cmt_struct
		integer :: gmt_year, gmt_month, gmt_day
		integer :: gmt_hour, gmt_min
		real :: gmt_sec
		real :: Mb, Ms
		real :: time_shift, half_duration
		real :: latitude, longitude, depth
		real :: Mrr, Mtt, Mpp, Mrt, Mrp, Mtp
		character(len=100) :: PDE_event_name
		character(len=13)  :: event_name
		character(len=20)  :: datasource
	end type cmt_struct

end module source_info
