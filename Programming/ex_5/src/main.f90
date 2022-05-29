program exercise_5
   use Environment
   
   implicit none
   character(*), parameter:: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   real(R_)                :: S = 0
   real(R_), allocatable   :: Z(:)
   integer(I_)             :: Pos

   open (file = input_file, newunit = In)
      read (In, *) N
      allocate (Z(N))
      read (In, *) Z
   close (In)
   
   call Positive(Z, Pos, S)

   open (file = output_file, encoding = E_, newunit = Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"(f6.2, 1x))") Z
      write (Out, '(/2(a, T12, "= ", f6.2/))') "Sum", S
   close (Out)

contains
    subroutine Positive(Z, Pos, S)
      real(R_)     Z(:), S
      integer(I_) Pos
      intent(inout)  Z
      intent(out) Pos, S

      Pos = FINDLOC(ARRAY = Z > 0 .and. Z < 1, VALUE=.true., DIM = 1)
      print *, "pos: " ,Pos
      
      if(Pos == 0) &
          S = Sum(Z)

      S = Sum(Z(1: Pos))

      print *, "sum: ", S

   end subroutine Positive
end program exercise_5
