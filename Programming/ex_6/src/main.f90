program exercise_6
   use Environment
   
   implicit none
   character(*), parameter:: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: aSinXImplemented = 0, aSinXImbedded = 0, x = 0

   open (file = input_file, newunit = In)
      read (In, *) x
   close (In)
   
  
   aSinXImplemented = aSinXImp(x)
   aSinXImbedded = aSin(x)
   
   open (file = output_file, encoding = E_, newunit = Out)
      write (Out, '(4(a, T16, "= ", f13.6/))') &
          'x ', x, &
          "aSin(x) ", aSinXImplemented, &
          "Fortran aSin(x) ", aSinXImbedded, &
          "Error ", aSinXImplemented-aSinXImbedded
    close (Out)

contains
    pure real(R_) function aSinXImp(x) result(aSinX)
        real(R_), intent(in)               :: x
        real(R_)                           :: r, q, x_2, oldaSinX
        integer(I_)                        :: n

        x_2 = x*x

        n = -1
        r = x
        aSinX = r

        do
            n = n+1
            q = x_2 * (((n+0.5)**2) / ((n+1)*(n+1.5)))
            r = r*q
            oldaSinX = aSinx
            aSinX = aSinX+r
            if(oldaSinX == aSinx) exit
        end do
    end function aSinXImp
end program exercise_6
