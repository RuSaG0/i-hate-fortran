program lab_1_4_1
   use Environment

   implicit none
   integer(I_), parameter                                       :: CANDIDATES_AMOUNT = 5, SURNAME_LENGTH = 15, &
                                                                   DATE_LENGTH = 4, MILITARY_LENGTH = 3, &
                                                                   REGISTRATION_LENGTH = 1
   
   character(MILITARY_LENGTH, kind = CH_), parameter            :: NOT_SERVED_CHARSET = Char(1053, CH_) // &
                                                                                Char(1077, CH_) // &
                                                                                Char(1090, CH_)
   
   character(REGISTRATION_LENGTH, kind = CH_), parameter        :: P_CHARSET = Char(1055, CH_)

   character(:), allocatable                                    :: input_file, output_file, format, fmt

   character(SURNAME_LENGTH, kind = CH_)                        :: surnameList(CANDIDATES_AMOUNT) = ""
   character(SURNAME_LENGTH, kind = CH_), allocatable           :: SPBMilitarySurnameList(:), guestMilitarySurnameList(:)
   character(DATE_LENGTH, kind = CH_)                           :: birthdayList(CANDIDATES_AMOUNT) = ""
   character(DATE_LENGTH, kind = CH_), allocatable              :: SPBMilitaryBirthdayList(:), guestMilitaryBirthdayList(:)
   character(kind = CH_)                                        :: genderList(CANDIDATES_AMOUNT) = ""
   character(REGISTRATION_LENGTH, kind = CH_), allocatable      :: SPBMilitaryGenderList(:), guestMilitaryGenderList(:)
   
   character(MILITARY_LENGTH, kind = CH_)                       :: isServedList(CANDIDATES_AMOUNT) = ""

   character(kind = CH_)                                        :: registrationList(CANDIDATES_AMOUNT) = ""

   logical, allocatable                                         :: SPBMilitaryMask(:), guestMilitaryMask(:)
   integer(I_)                                                  :: SPBMilitaryAmount = 0, guestMilitaryAmount = 0
    
   integer, allocatable                                         :: SPBMilitaryIndexesList(:), guestMilitaryIndexesList(:)

   integer                                                      :: In, Out, IO, i, j
   integer, parameter                                           :: indexes(*) = [(i, i = 1, CANDIDATES_AMOUNT)]

   input_file = "../data/class.txt"
   output_file = "output.txt"
   
    open (file = input_file, encoding = E_, newunit = In)
      format = '(5(a, 1x))'
      read (In, format, iostat = IO) & 
          (surnameList(i), birthdayList(i), isServedList(i), registrationList(i), genderList(i), i = 1, CANDIDATES_AMOUNT)
   close (In)

   Out = OUTPUT_UNIT
   open (Out, encoding = E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading class list."
      case(1:)
         write (Out, '(a)') "Error while reading class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading class list: ", io
   end select

   open (file = output_file, encoding = E_, newunit = Out)
      write (out, '(a)') "Исходный список:"
      write (Out, format, iostat = IO) &
          (surnameList(i), birthdayList(i), isServedList(i), registrationList(i), genderList(i), i = 1, CANDIDATES_AMOUNT)
   close (Out)

   Out = OUTPUT_UNIT
   open (Out, encoding = E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing class list."
      case(1:)
         write (Out, '(a)') "Error while writing class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing class list: ", io
   end select

   SPBMilitaryMask = registrationList == P_CHARSEt  .and. &
                     isServedList /= NOT_SERVED_CHARSET

   SPBMilitaryAmount = Count(SPBMilitaryMask)
   SPBMilitaryIndexesList = Pack(indexes, SPBMilitaryMask)
   
   allocate (SPBMilitarySurnameList(SPBMilitaryAmount), &
             SPBMilitaryBirthdayList(SPBMilitaryAmount), &
             SPBMilitaryGenderList(SPBMilitaryAmount))
   
   do concurrent (i = 1:SPBMilitaryAmount)
      SPBMilitarySurnameList(i) = surnameList(SPBMilitaryIndexesList(i))
      SPBMilitaryBirthdayList(i) = birthdayList(SPBMilitaryIndexesList(i))
      SPBMilitaryGenderList(i) = genderList(SPBMilitaryIndexesList(i))
   end do

   guestMilitaryMask = registrationList /= P_CHARSET  .and. & 
                       isServedList /= NOT_SERVED_CHARSET 
   
   guestMilitaryAmount = Count (guestMilitaryMask)
   guestMilitaryIndexesList = Pack(indexes, guestMilitaryMask)
   
   allocate (guestMilitarySurnameList(guestMilitaryAmount), &
             guestMilitaryBirthdayList(guestMilitaryAmount), &
             guestMilitaryGenderList(guestMilitaryAmount))

   do concurrent (i = 1:guestMilitaryAmount)
      guestMilitarySurnameList(i)  = surnameList(guestMilitaryIndexesList(i))
      guestMilitaryBirthdayList(i)  = birthdayList(guestMilitaryIndexesList(i))
      guestMilitaryGenderList = genderList(guestMilitaryIndexesList(i))
   end do
      
   do i = SPBMilitaryAmount, 2, -1
      do j = 1, i-1
         if (SPBMilitarySurnameList(j) > SPBMilitarySurnameList(j+1)) &
          SPBMilitarySurnameList(j+1:j:-1) =  SPBMilitarySurnameList(j:j+1)
          SPBMilitaryBirthdayList(j+1:j:-1) = SPBMilitaryBirthdayList(j:j+1) 
          SPBMilitaryGenderList(j+1:j:-1) = SPBMilitaryGenderList(j:j+1) 
      end do
   end do


   do i = guestMilitaryAmount, 2, -1
      do j = 1, i-1
         if (guestMilitarySurnameList(j) < guestMilitarySurnameList(j+1)) &
          guestMilitarySurnameList(j+1:j:-1) = guestMilitarySurnameList(j:j+1) 
          guestMilitaryBirthdayList(j+1:j:-1) = guestMilitaryBirthdayList(j:j+1)
          guestMilitaryGenderList(j+1:j:-1) = guestMilitaryGenderList(j:j+1) 
      end do
   end do

  open (file = output_file, encoding = E_, position='append', newunit = Out)
      fmt ='(3(a, 1x))'
      write (out, '(/a)') "Служившие. Питер:"
      write (Out, fmt, iostat = IO) &
        (SPBMilitarySurnameList(i), SPBMilitaryBirthdayList(i), SPBMilitaryGenderList(i), i = 1, SPBMilitaryAmount)
   close (Out)
   

   Out = OUTPUT_UNIT
   open (Out, encoding = E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted boys list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted boys list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
   end select
   
   open (file = output_file, encoding = E_, position='append', newunit = Out)
      write (out, '(/a)') "Служившие. Гости:"
      write (Out, fmt, iostat = IO) &
        (guestMilitarySurnameList(i), guestMilitaryBirthdayList(i), guestMilitaryGenderList(i), i = 1, guestMilitaryAmount)
   close (Out)

   Out = OUTPUT_UNIT
   open (Out, encoding = E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted boys list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted boys list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
   end select

end program lab_1_4_1
