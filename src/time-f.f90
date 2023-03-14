module time_f
    use iso_c_binding
    use iso_fortran_env
    implicit none
    private

    integer, parameter, public :: time_t = c_long_long
    integer, parameter, public :: clock_t = c_long

    integer, parameter, public :: CLOCKS_PER_SEC = 1000**(c_long/c_int)

    type, public, bind(c) :: tm
        integer(c_int) :: tm_sec
        integer(c_int) :: tm_min
        integer(c_int) :: tm_hour
        integer(c_int) :: tm_mday
        integer(c_int) :: tm_mon
        integer(c_int) :: tm_year
        integer(c_int) :: tm_wday
        integer(c_int) :: tm_yday
        integer(c_int) :: tm_isdst
    end type

    public :: asctime
    public :: clock
    public :: ctime
    public :: difftime
    public :: gmtime
    public :: localtime
    public :: mktime
    public :: strftime
    public :: time

    interface asctime
        module procedure asctime_f
    end interface

    interface clock
        procedure clock_c
    end interface

    interface ctime
        module procedure ctime_f
    end interface

    interface difftime
        procedure difftime_c
    end interface

    interface gmtime
        module procedure gmtime_f
    end interface

    interface localtime
        module procedure localtime_f
    end interface

    interface mktime
        procedure mktime_c
    end interface

    interface strftime
        module procedure strftime_f
    end interface

    interface time
        module procedure time_ff
    end interface

    interface
        function asctime_c(timeptr) bind(c, name="asctime")
            import :: tm, c_ptr
            type(tm), intent(in) :: timeptr
            type(c_ptr) :: asctime_c
        end function

        function clock_c() bind(c, name="clock")
            import :: clock_t
            integer(clock_t) :: clock_c
        end function

        function ctime_c(timer) bind(c, name="ctime")
            import :: time_t, c_ptr
            integer(time_t), intent(in) :: timer
            type(c_ptr) :: ctime_c
        end function

        function difftime_c(time1, time2) bind(c, name="difftime")
            import :: time_t, c_double
            integer(time_t), intent(in), value :: time1, time2
            real(c_double) :: difftime_c
        end function

        function gmtime_c(timer) bind(c, name="gmtime")
            import :: time_t, c_ptr
            integer(time_t), intent(in) :: timer
            type(c_ptr) :: gmtime_c
        end function

        function localtime_c(timer) bind(c, name="localtime")
            import :: time_t, c_ptr
            integer(time_t), intent(in) :: timer
            type(c_ptr) :: localtime_c
        end function

        function mktime_c(timeptr) bind(c, name="mktime")
            import :: tm, time_t
            type(tm), intent(inout) :: timeptr
            integer(time_t) :: mktime_c
        end function

        function strftime_c(str, maxsize, format, timeptr) bind(c, name="strftime")
            import :: c_ptr, c_size_t, tm
            type(c_ptr), intent(in), value :: str
            integer(c_size_t) :: maxsize
            type(c_ptr), intent(in), value :: format
            type(tm), intent(in) :: timeptr
            integer(c_size_t) :: strftime_c
        end function

        function time_c(timer) bind(c, name="time")
            import :: time_t, c_ptr
            type(c_ptr), intent(in), value :: timer
            integer(time_t) :: time_c
        end function
    end interface
contains
    function ctime_str_to_f(ptr) result(ans)
        type(c_ptr), intent(in), value :: ptr
        character(len=24) :: ans
        character(kind=c_char), dimension(:), pointer :: temp
        integer :: i
        call c_f_pointer(ptr, temp, [24])
        do i = 1, 24
            ans(i:i) = temp(i)
        end do
    end function

    function asctime_f(timeptr) result(ans)
        type(tm), intent(in) :: timeptr
        character(len=24) :: ans
        type(c_ptr) :: ret
        ret = asctime_c(timeptr)
        ans = ctime_str_to_f(ret)
    end function

    function ctime_f(timer) result(ans)
        integer(time_t), intent(in) :: timer
        character(len=24) :: ans
        type(c_ptr) :: ret
        ret = ctime_c(timer)
        ans = ctime_str_to_f(ret)
    end function

    function gmtime_f(timer) result(ans)
        integer(time_t), intent(in) :: timer
        type(tm), target :: ans
        type(tm), pointer :: pans
        type(c_ptr) :: ret
        ret = gmtime_c(timer)
        call c_f_pointer(ret, pans)
        ans = pans
    end function

    function localtime_f(timer) result(ans)
        integer(time_t), intent(in) :: timer
        type(tm), target :: ans
        type(tm), pointer :: pans
        type(c_ptr) :: ret
        ret = localtime_c(timer)
        call c_f_pointer(ret, pans)
        ans = pans
    end function

    function strftime_f(str, maxsize, format, timeptr) result(ans)
        character(len=*), intent(inout), target :: str
        integer, intent(in) :: maxsize
        character(len=*), intent(in) :: format
        type(tm), intent(in) :: timeptr
        integer :: ans
        character(len=:), allocatable, target :: c_format
        integer(c_size_t) :: c_maxsize, c_ans
        c_maxsize = int(maxsize, kind=c_size_t)
        c_format = (format)//c_null_char
        c_ans = strftime_c(c_loc(str), c_maxsize, c_loc(c_format), timeptr)
        ans = int(c_ans)
    end function

    function time_ff() result(ans)
        integer(time_t) :: ans
        ans = time_c(c_null_ptr)
    end function
end module time_f

