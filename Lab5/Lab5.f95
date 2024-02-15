program Lab5

    implicit none
    
    !Declare variables
    integer :: n, i, j ! size of array, and iterator variables for do loops
    real*4, allocatable :: A(:) !array to be sorted
    real*4 :: temp !temp variable for swapping
    character*50 :: file_name !name of file to take numbers from 
    
    !Get input file
    print*, "Please enter filename: "
    read(*,*) file_name

    !open file, and read first integer in that file
    open(42,file=file_name)
    read(42,*)
    read(42,*) n

    !Bubble sort loop
    allocate(A(n))

    !Read in values
    do i = 1, n 
        read(42,*) A(i) !read in data value
    end do

    !Bubble sort loop
    do i = 1, n
        do j = 1, n - 1
            if (A(j) .gt. A(j+1)) then
                temp = A(j)
                A(j) = A(j+1)
                A(j+1) = temp
            endif
        enddo
    enddo

    !Write sorted array to output file
    open(43,file='BubbleOut'//file_name)
    write(43,*) A

    !Clean-up
    deallocate(A)
    close(42)
    close(43)

end program Lab5   