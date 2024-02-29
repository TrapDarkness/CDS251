program hw7EX

    implicit none

    ! Declare variables
    integer :: i
    real*4 :: x, y, a, b, c, Sxx, Sxy, Sxx2, Sx2x2, Sx2y, x_, y_, x2_

    !Initialize math variables
    x_ = 0.0
    y_ = 0.0
    x2_ = 0.0
    Sxx = 0.0
    Sxy = 0.0
    Sxx2 = 0.0
    Sx2x2 = 0.0
    Sx2y = 0.0

    !Open the input and output files.
    open(42,file='QuadData.txt')

    !Read in Data and take running computations
    do i = 1, 200
        read(42,*) x, y

        !Update Sums of Squares with running computation
        Sxx = Sxx + ((float(i) - 1) / float(i)) * (x - x_)**2 
        Sxy = Sxy + ((float(i) - 1) / float(i)) * (x - x_) * (y - y_) 
        Sxx2 = Sxx2 + ((float(i) - 1) / float(i)) * (x - x_) * (x**2 - x2_) 
        Sx2x2 = Sx2x2 + ((float(i) - 1) / float(i)) * (x**2 - x2_)**2 
        Sx2y = Sx2y + ((float(i) - 1) / float(i)) * (y - y_) * (x**2 - x2_)

        !Update x and y with running averages
        x_ = x_ + (x - x_) / float(i)
        y_ = y_ + (y - y_) / float(i)
        x2_ = x2_ + (x**2 - x2_) / float(i) 

    end do

    !compute a, b, and c
    a = (Sx2y*Sxx - Sxy*Sxx2) / (Sxx*Sx2x2 - (Sxx2)**2)
    b = (Sxy*Sx2x2 - Sx2y*Sxx2) / (Sxx*Sx2x2 - (Sxx2)**2)
    c = y_ - (b * x_) - (a * x2_)

    print*, "x^2 coefficent a = ", a 
    print*, "x coefficent b = ", b
    print*, "Constant c = ", c

end program hw7EX