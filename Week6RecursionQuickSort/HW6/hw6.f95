program HW6

    implicit none
   
    ! Declare variables
    integer :: n, i
    real*4, allocatable :: A(:)
    character*50 :: filename
   
    !Request filename
    print*, "Please input filename"
    read(*,*) filename
   
    ! Open the input and output files.
    open(42,file=filename)
    open(43,file='Output.txt')
    read(42,*)
    read(42,*) n
    
    !allocate memory
    allocate(A(n))

    !read in data
    do i = 1, n
        read(42,*) A(i)
    enddo   
   
    !Sort Data
    call QuickSort(A,n, 1, n)

    !Write output file
    write(43,*) "Sorted Data" !Title file
    write(43,*) n !Size of data
    do i = 1, n
        write(43,*) A(i) !Loop to write data
    enddo
   
    ! Clean-up
    deallocate(A)
    close(42)
    close(43)
    
    !Done
    print*, "Done!"

end program HW6

recursive subroutine QuickSort(A, n, lo, hi)

    implicit none
    
    !Initialize working variables
    integer :: n, lo, hi, p, Partition
    real*4 :: A(n) 

    !Set up Partition and recurision quicksort
    if (lo < hi) then
        p = Partition(A, n, lo, hi)
        call QuickSort(A, n, lo, p-1)
        call QuickSort(A, n, p+1, hi)
    endif

end subroutine QuickSort

function Partition(A, n, lo, hi) result(P)

    implicit none
    
    !Initialize working variables
    integer :: n, i, P, lo, hi 
    real*4 :: A(n), Temp, Pivot

    !Set pivot and low
    Pivot = A(hi)
    P = lo

    !Swap values in Partition
    do i = lo, hi - 1
        if (A(i) .le. Pivot) then
            Temp = A(P)
            A(P) = A(i)
            A(i) = Temp
            P = P + 1
        endif
    enddo
    
    !Swap high and P 
    Temp = A(P)
    A(P) = A(hi)
    A(hi) = Temp

end function Partition
