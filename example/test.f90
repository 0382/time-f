program test
    use iso_c_binding
    use time_f
    implicit none
    integer(time_t) :: rawtime
    integer(clock_t) :: t1, t2
    integer :: str_size, i
    real :: sum, x
    type(tm) :: info
    character(len=:), allocatable :: buffer
    t1 = clock()
    rawtime = time()
    info = localtime(rawtime)
    buffer = repeat(' ', 40)
    str_size = strftime(buffer, 40, "time is %Y-%m-%d %H:%M:%S", info)
    if (str_size == 0) then
        print '(A)', "buffer size is not enough"
    else
        print '(A)', buffer(1:str_size)//"."
    end if
    sum = 0.0
    do i = 1, 1000000
        call random_number(x)
        sum = sum + x
    end do
    t2 = clock()
    print '(A,F6.4,"s")', "time used: ", (t2 - t1)/real(CLOCKS_PER_SEC)
    print *, sum
end program test