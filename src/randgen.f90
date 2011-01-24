PROGRAM randgen
  IMPLICIT NONE
  REAL(KIND=8) :: rn, rmin=0., rmax=1., swap
  !rn: random number
  !rmin, rmax: range of random number
  !swap: temp variable for swapping rmin and rmax
  INTEGER :: i, num_rn
  !num_rn: number of random numbers to be generated
  
  call get_argument()
  write(*,*) "# Number of random numbers: ", num_rn
  write(*,*) "# Range: ", rmin, rmax
  write(*,*)
  
  call init_random_seed()
  do i = 1, num_rn
     call RANDOM_NUMBER(rn)
     rn = rn * (rmax - rmin) + rmin
     write(*,*) rn     
  end do

CONTAINS
  SUBROUTINE get_argument()
    IMPLICIT NONE
    INTEGER :: stat, i, n
    INTEGER, PARAMETER :: LEAST_REQUIRED_NUM_ARG = 2
    CHARACTER(LEN=128) :: usage, arg
    

    n = COMMAND_ARGUMENT_COUNT()
    call GET_COMMAND_ARGUMENT(NUMBER=0, VALUE=arg)
    usage = "Usage: " // TRIM(ADJUSTL(arg)) // " -n <num> [-r <min> <max>]"

    if (n < LEAST_REQUIRED_NUM_ARG) then
       write(*,*) "Insufficient arguments!"
       write(*,*) usage
       call EXIT(1)
    end if

    i = 1
    do while (i <= n)
       call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)
       i = i + 1       
       select case (arg)
       case ('-n')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)
          i = i + 1          
          if (stat /= 0) then
             write(*,*) "Unable to read the value of argument -n"
             write(*,*) usage
             call EXIT(1)
          end if
          read(arg, *, IOSTAT=stat) num_rn
          if (stat /= 0) then
             write(*,*) "Unable to parse the value of argument -n, an&
                  & integer is needed!"
             call EXIT(1)
          end if
          
       case ('-r')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)          
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the first value of argument -r"
             write(*,*) usage
             call EXIT(1)
          end if
          read(arg, *, IOSTAT=stat) rmin
          if (stat /= 0) then
             write(*,*) "Unable to parse the first value of argument -r, &
                  &two real numbers are needed!"
             call EXIT(1)
          end if

          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)          
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the second value of argument -r"
             write(*,*) usage
             call EXIT(1)
          end if
          read(arg, *, IOSTAT=stat) rmax
          if (stat /= 0) then
             write(*,*) "Unable to parse the second value of argument -r, &
                  &two real numbers are needed!"
             call EXIT(1)
          end if

          if (rmin == rmax) then
             write(*,*) "<min> and <max> should not be the same!"
             write(*,*) usage
             call EXIT(1)
          end if
          
          !swap rmin and rmax if rmin > rmax
          if (rmin > rmax) then
             swap = rmin
             rmin = rmax
             rmax = swap
          end if
          
       case default
          write(*,*) "Unknown argument: ", arg
          call EXIT(1)
       end select
    end do
  END SUBROUTINE get_argument
  
  SUBROUTINE init_random_seed()
    INTEGER :: i, n, clock
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))
    CALL SYSTEM_CLOCK(COUNT=clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    CALL RANDOM_SEED(PUT = seed)
    DEALLOCATE(seed)
  END SUBROUTINE init_random_seed

END PROGRAM randgen
