!< Convert ICEM multi-block info grid to acustic format.
program info2acustic
!< Convert ICEM multi-block info grid to acustic format.
use, intrinsic :: iso_fortran_env, only : stdout => output_unit, &
                                          stderr => error_unit,  &
                                          I4P    => int32,       &
                                          R8P    => real64
use info2acustic_file_object, only : file_object
implicit none

type(file_object) :: a_file                        !< Acoustic file.
integer(I4P)      :: command_line_arguments_number !< Number of command line arguments.
character(999)    :: file_name_info                !< Info file name.
character(999)    :: file_name_acustic             !< Acoustic file name.
integer(I4P)      :: c                             !< Counter.

command_line_arguments_number = command_argument_count()
if (command_line_arguments_number<2) then
   write(stderr,'(A)')' Error: at least 2 arguments must be passed to command line'
   ! call print_usage
   stop
else
   call get_command_argument(command_line_arguments_number, file_name_acustic)
   c = 0
   do while (c < command_line_arguments_number - 1)
     c = c + 1
     call get_command_argument(c, file_name_info)
     call a_file%initialize(file_name=file_name_info)
     call a_file%write_to_file(file_name=file_name_acustic)
   enddo
endif
endprogram info2acustic
