program lab7

    implicit none

    ! Declare variables
    integer :: i
    real*4 :: x, y, m, b, Sxx, Sxy, x_, y_

    !Initialize math variables
    x_ = 0.0
    y_ = 0.0
    Sxx = 0.0
    Sxy = 0.0

    !Open the input and output files.
    open(42,file='LineData.txt')

    !Read in Data and take running computations
    do i = 1, 200
        read(42,*) x, y
        !Update Sxx and Syy with running computation
        Sxx = Sxx + ((float(i) - 1) / float(i)) * (x - x_)**2 !update Sxx
        Sxy = Sxy + ((float(i) - 1) / float(i)) * (x - x_) * (y - y_) !update Sxy

        !Update x and y with running averages
        x_ = x_ + (x - x_) / float(i)
        y_ = y_ + (y - y_) / float(i)

    end do

    !compute slope and intercept
    m = Sxy / Sxx
    b = y_ - m * x_

    print*, "m: ", m, " b: ", b

end program lab7