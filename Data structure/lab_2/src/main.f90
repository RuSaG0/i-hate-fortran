program reference_lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable        :: stringSource, modifiedStringSource

   type(Text), pointer              :: string => Null()
   type(Text), pointer              :: modifiedString => Null()
   
   character(7, CH_), parameter     :: modifiedSubstring = "@RuSaG0" 
   character(4, CH_), parameter     :: substring = "quia"

   stringSource = "../data/source.f90"
   modifiedStringSource = "source.f90.diff"

   string => readString(stringSource)
    call output(modifiedStringSource, string, "rewind")

   if (Associated(string)) &
         modifiedString => changeSubscring(string, substring, modifiedSubstring) 

   if (Associated(modifiedString)) &
      call output(modifiedStringSource, modifiedString, "append")

end program reference_lab_2


