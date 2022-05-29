module Group_IO
   use Environment

   implicit none
   integer, parameter :: CANDIDATES_AMOUNT   = 5
   integer, parameter :: SURNAME_LENGTH   = 15
   integer, parameter :: DATE_LENGTH  = 4
   integer, parameter :: MILITARY_LENGTH  = 3

   type CandidateType
      character(SURNAME_LENGTH, kind=CH_)       :: surname = ""
      character(DATE_LENGTH, kind=CH_)          :: birthDate = ""
      character(MILITARY_LENGTH, kind=CH_)      :: isServedCharset = ""
      character(kind=CH_)                       :: registrationCharset = ""
      character(kind=CH_)                       :: genderChar = ""
   end type CandidateType
   
contains

   subroutine createDataFile(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file
      
      type(CandidateType)        :: candidate
      integer                    :: In, Out, IO, i, recl
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
      recl = (SURNAME_LENGTH + DATE_LENGTH + MILITARY_LENGTH  + 1 + 1)*CH_
      open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
         format = '('//CANDIDATES_AMOUNT//'(a, 1x))'
         do i = 1, CANDIDATES_AMOUNT
            read (In, format, iostat=IO) candidate
            call Handle_IO_status(IO, "reading formatted class list, line " // i)
            write (Out, iostat=IO, rec=i) candidate
            call Handle_IO_status(IO, "creating unformatted file with class list, record " // i)
         end do
      close (In)
      close (Out)
   end subroutine createDataFile

   function readClassList(Data_File) result(candidateList)
      type(CandidateType)             :: candidateList(CANDIDATES_AMOUNT)
      character(*), intent(in)        :: Data_File

      integer In, IO, recl
      
      recl = ((SURNAME_LENGTH + DATE_LENGTH + MILITARY_LENGTH+ 1 + 1)*CH_) * CANDIDATES_AMOUNT
      open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, iostat=IO, rec=1) candidateList
         call Handle_IO_status(IO, "reading unformatted class list")
      close (In)
   end function readClassList
 

   subroutine outputData(Output_File, candidateList, opDescription, writeType)
      character(*), intent(in)         :: Output_File, writeType, opDescription
      type(CandidateType), intent(in)  :: candidateList(:)

      integer                          :: Out, IO
      character(:), allocatable        :: format
      
      open (file=Output_File, encoding=E_, position=writeType, newunit=Out)
         write (out, '(/a)') opDescription
         format = '(5(a, 1x))'
         write (Out, format, iostat=IO) candidateList
         call Handle_IO_status(IO, "writing " // opDescription)
      close (Out)
   end subroutine outputData
end module Group_IO 
