program exercise_7_5a
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0
   real(R_), allocatable   :: A(:), AbsA(:)

   open (file=input_file, newunit=In)
      read (In, *) M
      allocate (A(M))
      read (In, *) A
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "("//M//"f6.2)") A
   close (Out)
 
   allocate(AbsA(M))
   
   call SortA(A, AbsA)
  
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "(/"//M//"f6.2)") AbsA
      write (Out, "(/"//M//"f6.2)") A
   close (Out)

contains

    subroutine SortA(A, AbsA)
      real(R_), intent(inout) :: A(:), AbsA(:)

      integer  :: i, MaxInd

      AbsA = abs(A)
      
      do i = 1, Size(A)-1
         MaxInd = maxLoc(AbsA(i:), 1) + i -1
         if (AbsA(i) <= AbsA(MaxInd)) &
            A([i, MaxInd])  = A([MaxInd, i])
            AbsA([i, MaxInd])  = AbsA([MaxInd, i])
      end do
   end subroutine SortA
end program exercise_7_5a
