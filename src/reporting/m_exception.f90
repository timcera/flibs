module m_exception

  implicit none

  private

  !---------------------------
  ! Contenu public du module !
  !---------------------------

  public :: EXCEPT_raiseFatalError
  public :: EXCEPT_raiseError
  public :: EXCEPT_raiseWarning
  public :: EXCEPT_raiseInformation
  public :: EXCEPT_raiseFailure
  public :: EXCEPT_assertFatalError
  public :: EXCEPT_assertError
  public :: EXCEPT_assertWarning
  public :: EXCEPT_assertInformation
  public :: EXCEPT_assertFailure

  integer , parameter , public :: EXCEPT_MSG_LENGTH = 200

  !---------------------------
  ! Contenu priv� du module  !
  !---------------------------
  integer, parameter :: EXCEPT_INFORMATION = 0
  integer, parameter :: EXCEPT_WARNING = 1
  integer, parameter :: EXCEPT_ERROR = 2
  integer, parameter :: EXCEPT_FATAL_ERROR = 3
  integer, parameter :: EXCEPT_FAILURE = 4

contains

  ! Proc�dure publique qui l�ve une exception FATAL_ERROR:
  ! - affiche un message d'information suivant un format standard
  !   en indiquant l'origine de l'appel � la proc�dure;
  ! - stoppe l'ex�cution.
  subroutine EXCEPT_raiseFatalError(origin, message)
    implicit none
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message

    call EXCEPT_print(EXCEPT_FATAL_ERROR, origin, message)
    call EXCEPT_stop ()
  end subroutine EXCEPT_raiseFatalError

  ! Proc�dure publique qui l�ve une exception ERROR:
  ! - affiche un message d'information suivant un format standard
  !   en indiquant l'origine de l'appel � la proc�dure;
  ! - stoppe l'ex�cution.
  subroutine EXCEPT_raiseError(origin, message)
    implicit none
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message

    call EXCEPT_print(EXCEPT_ERROR, origin, message)
    call EXCEPT_stop ()
  end subroutine EXCEPT_raiseError

  ! Proc�dure publique qui l�ve une exception WARNING:
  ! - affiche un message d'information suivant un format standard
  !   en indiquant l'origine de l'appel � la proc�dure;
  subroutine EXCEPT_raiseWarning(origin, message)
    implicit none
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message

    call EXCEPT_print(EXCEPT_WARNING, origin, message)

  end subroutine EXCEPT_raiseWarning

  ! Proc�dure publique qui l�ve une exception INFORMATION:
  ! - affiche un message d'information suivant un format standard
  !   en indiquant l'origine de l'appel � la proc�dure.
  subroutine EXCEPT_raiseInformation(origin, message)
    implicit none
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message

    call EXCEPT_print(EXCEPT_INFORMATION, origin, message)

  end subroutine EXCEPT_raiseInformation

  ! Proc�dure publique qui l�ve une exception FAILURE:
  ! - affiche un message d'information suivant un format standard
  !   en indiquant l'origine de l'appel � la proc�dure;
  ! - stoppe l'ex�cution.
  subroutine EXCEPT_raiseFailure(origin, message)
    implicit none
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message

    call EXCEPT_print(EXCEPT_FAILURE, origin, message)
    call EXCEPT_stop ()
  end subroutine EXCEPT_raiseFailure

  ! Proc�dure publique qui l�ve une exception FATAL_ERROR:
  ! - affiche un message d'information suivant un format standard
  !   en indiquant l'origine de l'appel � la proc�dure;
  ! - stoppe l'ex�cution.
  subroutine EXCEPT_assertFatalError(test, origin, message)
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message

#ifdef _DEBUG
    if (.not.test) call EXCEPT_raiseFatalError(origin, message)
#else
    continue
#endif

  end subroutine EXCEPT_assertFatalError

  ! Proc�dure publique qui l�ve une exception ERROR:
  ! - affiche un message d'information suivant un format standard
  !   en indiquant l'origine de l'appel � la proc�dure;
  ! - stoppe l'ex�cution.
  subroutine EXCEPT_assertError(test, origin, message)
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message

#ifdef _DEBUG
    if (.not.test) call EXCEPT_raiseError(origin, message)
#else
    continue
#endif

  end subroutine EXCEPT_assertError

  ! Proc�dure publique qui l�ve une exception WARNING:
  ! - affiche un message d'information suivant un format standard
  !   en indiquant l'origine de l'appel � la proc�dure;
  subroutine EXCEPT_assertWarning(test, origin, message)
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message

#ifdef _DEBUG
    if (.not.test) call EXCEPT_raiseWarning(origin, message)
#else
    continue
#endif

  end subroutine EXCEPT_assertWarning

  ! Proc�dure publique qui l�ve une exception INFORMATION:
  ! - affiche un message d'information suivant un format standard
  !   en indiquant l'origine de l'appel � la proc�dure.
  subroutine EXCEPT_assertInformation(test, origin, message)
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message

#ifdef _DEBUG
    if (.not.test) call EXCEPT_raiseInformation(origin, message)
#else
    continue
#endif

  end subroutine EXCEPT_assertInformation

  ! Proc�dure publique qui l�ve une exception FAILURE:
  ! - affiche un message d'information suivant un format standard
  !   en indiquant l'origine de l'appel � la proc�dure;
  ! - stoppe l'ex�cution.
  subroutine EXCEPT_assertFailure(test, origin, message)
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message

#ifdef _DEBUG
    if (.not.test) call EXCEPT_raiseFailure(origin, message)
#else
    continue
#endif

  end subroutine EXCEPT_assertFailure
  !
  ! Printing subroutine for the module.
  ! exceptionCode : an integer exception code between : EXCEPT_INFORMATION,
  ! EXCEPT_WARNING, EXCEPT_ERROR, EXCEPT_FATAL_ERROR, EXCEPT_FAILURE
  ! origin : the name of the element generating the exception
  ! message : a message to display
  !
  subroutine EXCEPT_print(exceptionCode, origin, message)
    implicit none
    integer, intent(in) :: exceptionCode
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message
    
    select case (exceptionCode)
    case (EXCEPT_INFORMATION)
       write(6,*) "Information from: ", origin
       write(6,*) "Information: ", trim(adjustl(message))
    case (EXCEPT_WARNING)
       write(6,*) "Warning from: ", origin
       write(6,*) "Warning: ", trim(adjustl(message))
    case (EXCEPT_ERROR, EXCEPT_FATAL_ERROR, EXCEPT_FAILURE)
       write(6,*) "Internal error from: ", origin
       write(6,*) "Error: ", trim(adjustl(message))
    case default
       write(6,*) "Bad value for the exception code."
    end select
  end subroutine EXCEPT_print
  !
  ! Unique stopping subroutine for the module.
  ! In debug mode, put a breakpoint here allows to see the exception
  ! before stopping.
  !
  subroutine EXCEPT_stop ()
    implicit none
    STOP
  end subroutine EXCEPT_stop
end module m_exception
