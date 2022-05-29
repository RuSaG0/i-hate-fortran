program reference_lab_1_4_2
   use Environment

   implicit none
   integer(I_), parameter              :: CANDIDATES_AMOUNT = 5, SURNAME_LENGTH = 15, DATE_LENGTH = 4, MILITARY_LENGTH = 3
   character(kind = CH_), parameter    :: NOT_SERVED_CHARSET(*) = [Char(1053, CH_), Char(1077, CH_), Char(1090, CH_)]
   character(kind = CH_), parameter    :: P_CHARSET = Char(1055, CH_)
   character(kind = CH_), parameter    :: G_CHARSET = Char(1057, CH_)
   character(:), allocatable           :: input_file, output_file

   character(kind = CH_)               :: surnameList(SURNAME_LENGTH, CANDIDATES_AMOUNT)  = "", &
                                          birthdayList(DATE_LENGTH,CANDIDATES_AMOUNT) = "", &
                                          isServedList(MILITARY_LENGTH,CANDIDATES_AMOUNT)  = "",&
                                          registrationList(CANDIDATES_AMOUNT) = "", &
                                          genderList(CANDIDATES_AMOUNT) = ""
                           
   character(kind = CH_), allocatable  :: SPBSurnames(:, :), guestSurnames(:, :)
   character(kind = CH_), allocatable  :: SPBBirthdays(:, :), guestBirthdays(:, :)
   character(kind = CH_), allocatable  :: SPBGenders(:), guestGenders(:)

   integer(I_)                         :: i

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   call readData(input_file, surnameList, birthdayList, isServedList, registrationList, genderList)
   
   call outputData(output_file, surnameList, birthdayList, isServedList, registrationList, genderList, &
      "Исходный список:", "rewind")
   
   call fillMilitary(surnameList, birthdayList, isServedList, registrationList, genderList, &
        SPBSurnames, SPBBirthdays, SPBGenders, NOT_SERVED_CHARSET, P_CHARSET)  
   call fillMilitary(surnameList, birthdayList, isServedList, registrationList, genderList, &
        guestSurnames, guestBirthdays, guestGenders, NOT_SERVED_CHARSET, G_CHARSET) 
          
   call sortBySurname(SPBSurnames, SPBGenders)
   call sortBySurname(guestSurnames, guestGenders)

   call outputData(output_file, SPBSurnames, SPBBirthdays, isServedList, &
    [(P_CHARSET, i = 1, Size(SPBGenders))], SPBGenders,  "Служившие Питер:", "append")
   call outputData(output_file, guestSurnames, guestBirthdays, isServedList, &
   [(G_CHARSET, i = 1, Size(guestGenders))], guestGenders, "Служившие Гости:", "append")
      
contains

   subroutine readData(Input_File, surnameList, birthdayList, isServedList, registrationList, genderList)
      character(*)                     :: Input_File
      character(kind = CH_)            :: surnameList(:, :), birthdayList(:, :), isServedList(:, :), &
                                          registrationList(:), genderList(:)

      intent (in)                      :: Input_File
      intent (out)                     :: surnameList, birthdayList, isServedList, registrationList, genderList

      integer In, IO, i
      character(:), allocatable        :: format
      
      open (file = Input_File, encoding = E_, newunit = In)
         format = '(' // SURNAME_LENGTH // 'a1, 1x, ' // DATE_LENGTH // 'a1, 1x, ' // &
            MILITARY_LENGTH // 'a1, 1x, '//'a1, 1x, '//'a1, 1x)'

         read (In, format, iostat = IO) &
              (surnameList(:, i), birthdayList(:, i), isServedList(:, i), registrationList(i), &
              genderList(i), i = 1, CANDIDATES_AMOUNT)
         
         call Handle_IO_status(IO, "reading class list")
      close (In)
   end subroutine readData


   subroutine outputData(outputFile, surnameList, birthdayList, isServedList, registrationList, &
      genderList, opDescription, writeType)
      character(*)                     :: outputFile, writeType, opDescription
      character(kind = CH_)            :: surnameList(:, :), birthdayList(:, :), isServedList(:, :), &
                                          registrationList(:), genderList(:)

      intent (in)                      :: outputFile, surnameList, birthdayList, isServedList, registrationList, &
                                          genderList, opDescription, writeType

      integer(I_)                      :: Out, i, IO
      character(:), allocatable        :: format
   
      open (file = outputFile, encoding = E_, position = writeType, newunit = Out)
         write (out, '(/a)') opDescription
         format = '(' // SURNAME_LENGTH // 'a1, 1x, ' // DATE_LENGTH // 'a1, 1x, ' // &
            MILITARY_LENGTH // 'a1, 1x'//'a1, 1x'//'a1, 1x)'
         write (Out, format, iostat = IO) (surnameList(:, i), birthdayList(:, i), isServedList(:, i), &
         registrationList(i), genderList(i), i = 1, Size(genderList))
         call Handle_IO_status(IO, "writing " // opDescription)
    close (Out)
   end subroutine outputData

 pure subroutine fillMilitary(surnameList, birthdayList, isServedList, registrationList, genderList, &
                              filteredSurnameList, filteredBirthdayList, filteredGenderList, NOT_SERVED_CHARSET, registerChar)
    
      character(kind = CH_)               :: surnameList(:, :), birthdayList(:, :), isServedList(:,:), &
                                             registrationList(:), genderList(:)

      character(kind = CH_)               :: filteredSurnameList(:, :), filteredBirthdayList(:, :), & 
                                             filteredGenderList(:), registerChar

      character(kind = CH_)               :: NOT_SERVED_CHARSET(:)

      intent(in)                          :: surnameList, birthdayList, genderList, NOT_SERVED_CHARSET, &
                                             isServedList, registrationList, registerChar

      intent(out)                         :: filteredSurnameList, filteredBirthdayList, filteredGenderList
      allocatable                         :: filteredSurnameList, filteredBirthdayList, filteredGenderList 

      logical, allocatable                :: militaryList(:)
      integer, allocatable                :: militaryIndexes(:)
      integer(I_)                         :: militaryAmount, i

      integer, parameter                  :: indexes(*) = [(i, i = 1, CANDIDATES_AMOUNT)]

      militaryList    =  [(ALL(isServedList(:, i) /= NOT_SERVED_CHARSET), i = 1, CANDIDATES_AMOUNT)] & 
                        .and. registrationList == registerChar

      militaryAmount  = Count(militaryList)

      militaryIndexes  = Pack(indexes, militaryList)

      allocate (filteredSurnameList(SURNAME_LENGTH,militaryAmount), &
               filteredBirthdayList(DATE_LENGTH,militaryAmount), &
               filteredGenderList(militaryAmount))

      do concurrent (i = 1:militaryAmount)
         filteredSurnameList(:, i)  = surnameList(:,militaryIndexes(i))
         filteredBirthdayList(:, i)  = birthdayList(:,militaryIndexes(i))
         filteredGenderList(i)= genderList(militaryIndexes(i))
      end do
   end subroutine fillMilitary

   pure subroutine sortBySurname(surnameList, genderList)
      character(kind = CH_)   :: surnameList(:, :), genderList(:)
      intent (inout)          :: surnameList, genderList

      integer(I_)             :: i, j
      
      do i = Size(genderList), 2, -1
         do j = 1, i-1
            if (lexComparer(surnameList(:, j), surnameList(:,j+1))) &
                surnameList(:, j+1:j:-1) = surnameList(:,j:j+1)
         end do
      end do
   end subroutine sortBySurname

   pure logical function lexComparer(arr1, arr2)
      character(kind = CH_), intent(in)         :: arr1(:), arr2(:)
      integer                                   :: i 

      do i = 1, Min(Size(arr1), Size(arr2)) - 1
         if (arr1(i) /= arr2(i)) &
            exit
      end do
      lexComparer = arr1(i) > arr2(i)  
   end function lexComparer
   
end program reference_lab_1_4_2
