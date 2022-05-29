module Source_Process

   use Environment
   use Source_IO

   implicit none

contains

pure recursive function changeSubscring(string, substring, modifiedSubstring) result (modifiedString)
    type(Text), pointer           :: modifiedString
    type(Text), intent(in)        :: string
      
    character(*), intent(in)      :: modifiedSubstring, substring    
    integer(I_)                   :: i
      
    allocate (modifiedString)
    modifiedString%Str = string%Str
      
    !https://stackoverflow.com/questions/54328622/i-am-looking-for-suggestions-on-speeding-up-my-boyer-moore-horspool-code
    i = index(modifiedString%Str, substring)
    do while(i > 0) 
        modifiedString%Str = modifiedString%Str(:i-1) // modifiedSubstring // modifiedString%Str(i+Len(substring):) 
        i = index(modifiedString%Str, substring)
    end do

    if (Associated(string%next)) then
        modifiedString%next => changeSubscring(string%next, substring, modifiedSubstring)
    endif


end function changeSubscring
  
end module Source_process
