program HW5

    implicit none
   
    ! Declare variables
    integer :: n, i, j
    integer, allocatable :: index(:)
    real*4, allocatable :: A(:)
    real*4 :: Temp, start_time, end_time
    logical :: Done
    character*50 :: filename

    call cpu_time(start_time)
   
    !Request filename
    print*, "Please input filename"
    read(*,*) filename
   
    ! Open the input and output files.
    open(42,file=filename)
    open(43,file='Output.txt')
    read(42,*)
    read(42,*) n
    
    !allocate memory
    allocate(A(n),index(n))

    !read in data
    do i = 1, n
        read(42,*) A(i)
    enddo
   
   !Set up index
    do i = 1, n
        index(i) = i
    enddo
   
    !Sort Data
    do i = 1, n - 1
        Done = .true.
        do j = 1, n - i
            if (A(index(j)) .gt. A(index(j+1))) then
                Temp = index(j)
                index(j) = index(j+1)
                index(j+1) = Temp
                Done = .false.
            endif
        enddo
        if (Done) exit
    enddo

    !Write output file
    write(43,*) "Sorted Data" !Title file
    write(43,*) n !Size of data
    do i = 1, n
        write(43,*) A(index(i)) !Loop to write data
    enddo
   
    ! Clean-up
    deallocate(A,index)
    close(42)
    close(43)
    
    !Done
    print*, "Done!"
    
    call cpu_time(end_time)
    
    write(*, '(A, F8.6)') 'Elapsed time, s : ',  (end_time - start_time)


end program HW5
