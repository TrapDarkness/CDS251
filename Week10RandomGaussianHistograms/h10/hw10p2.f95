program hw10p2
    ! Alexander Shumway
    ! CDS251
    ! HW10Program2
    ! 03/28/2024
    
    ! Generate Histogram

    implicit none

    !declare variables here
    integer :: i, HistStart, HistEnd, BadCount, nBoxes
    integer, allocatable :: Hist(:)
    real*8 :: BoxWidth, iHist, Num, HistRange
    
    !initialize files
    open(42,file='Bumps.txt')
    open(43,file='BumpsHist.txt')

    !Initialize other variables
    nBoxes = 100
    HistStart = 10
    HistEnd = 30
    HistRange = HistEnd - HistStart
    BoxWidth = HistRange / nBoxes
    allocate(Hist(nBoxes))

    !Generate Histogram
    
    do i = 1, nBoxes
        read(42,*) Num
        iHist = Int((Num-HistStart) * float(NBoxes) / HistRange)
        if (iHist .ge. 1 .and. iHist .le. nBoxes) then
            Hist(iHist) = Hist(iHist) + 1
        else
            BadCount = BadCount + 1
        endif
    enddo
   
   !Create plot
    open(44, file = 'Histogram.txt')
    do i = 1, nBoxes
        x = (float(i) - 0.5) * BoxWidth + HistStart
        write(42,*) x, Hist(i)
    enddo
   
    !clean-up
    close(42)
    close(43)
    close(44)
    deallocate(nBoxes)

end program hw10p2
