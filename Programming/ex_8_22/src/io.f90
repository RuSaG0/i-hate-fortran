module IO
   use Environment

   implicit none
contains
   subroutine ReadP(input_file, N, M, A)
      character(*), intent(in)                  :: input_file
      real(R_), intent(out), allocatable        :: A(:, :)
      integer(I_), intent(out)                  :: N, M
      integer                                   :: In = 0, i = 0
   
      open (file = input_file, newunit = In)
        read (In, *) N, M
        allocate (A(M, N))  ! Change by default
        read (In, *) (A(:, i), i = 1, N)
      close (In)
   end subroutine ReadP
  
   subroutine OutputP(output_file, A)
      character(*), intent(in):: output_file
      real(R_), intent(in)     :: A(:, :)
      integer(I_)              :: x1, x2, j = 0
      
      integer:: Out = 0
   
      x1 = Ubound(A, 1)
      x2 = Ubound(A, 2)
   
      open (file = output_file, encoding = E_, newunit = Out)
        write (Out, '('//x1//'f6.2)') (A(:, j), j = 1, x2)   
      close (Out)
   end subroutine OutputP
   
   subroutine OutputMatrix(output_file, A)
      character(*), intent(in)      :: output_file
      real(R_), intent(in)          :: A(:, :)
      integer(I_)                   :: x1, x2, Out = 0, j = 0
      
      x1 = Ubound(A, 1)
      x2 = Ubound(A, 2)
   
      open (file = output_file, encoding = E_, newunit = Out, position='append')
        write(Out, *) '===================================='
        write (Out, '('//x1//'f6.2)') (A(:, j), j = 1, x2)   
      close (Out)
   end subroutine OutputMatrix
end module IO
