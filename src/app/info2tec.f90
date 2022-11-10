program info2tec
!-----------------------------------------------------------------------------------------------------------------------------------
USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout => OUTPUT_UNIT, stderr => ERROR_UNIT ! Standard output/error logical units.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
integer::            c,Nca,uin,uou,Ni,Nj,Nk
character(500)::     fin,fou,line,zone
!---------------------------------------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------------------------------------
Nca = command_argument_count()
if (Nca<2) then
  write(stderr,'(A)')' Error: at least 2 arguments must be passed to command line'
  call print_usage
  stop
else
  call get_command_argument(Nca,fou) ; open(unit=Get_Unit(uou),file=trim(adjustl(fou)),action='WRITE')
  write(uou,'(A)')'VARIABLES="x" "y" "z"'
  c = 0
  do while (c<Nca-1)
    c = c + 1
    call get_command_argument(c,fin) ; open(unit=Get_Unit(uin),file=trim(adjustl(fin)),action='READ')
    do
      read(uin,'(A)',end=1,err=1)line
      if (index(line,"domain")>0) then
        zone = line(:index(line," ")) ; read(line(index(line," "):),*)Ni,Nj,Nk
        write(uou,'(A,I4,A,I4,A,I4)')'ZONE T="'//trim(zone)//'" I=',Ni,' J=',Nj,' K=',Nk
      else
        write(uou,'(A)')trim(line)
      endif
    enddo
    close(uin)
    1 write(stdout,'(A)')' Parsed file '//trim(adjustl(fin))
  enddo
  close(uou)
endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine print_usage()
  !---------------------------------------------------------------------------------------------------------------------------------
  ! Subroutine for printing the correct use of the program.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' info2tec'
  write(stdout,'(A)')' Convert info file to tecplot format'
  write(stdout,'(A)')' Usage:'
  write(stdout,'(A)')'   info2tec info_files_list output_tec_file'
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_usage

  !> @brief The Get_Unit function returns a free logic unit for opening a file. The unit value is returned by the function, and also
  !> by the optional argument "Free_Unit". This allows the function to be used directly in an open statement like:
  !> open(unit=Get_Unit(myunit),...) ; read(myunit)...
  !> If no units are available, -1 is returned.
  integer function Get_Unit(Free_Unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer, intent(OUT), optional:: Free_Unit !< Free logic unit.
  integer::                        n1        !< Counter.
  integer::                        ios       !< Inquiring flag.
  logical::                        lopen     !< Inquiring flag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  Get_Unit = -1
  n1=1
  do
    if ((n1/=stdout).AND.(n1/=stderr)) then
      inquire (unit=n1,opened=lopen,iostat=ios)
      if (ios==0) then
        if (.NOT.lopen) then
          Get_Unit = n1 ; if (present(Free_Unit)) Free_Unit = Get_Unit
          return
        endif
      endif
    endif
    n1=n1+1
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction Get_Unit
endprogram info2tec
