module bmp_conversions
    use integral_types

    contains
    pure integer(four_bytes) function RGB(r,g,b)
        integer(one_byte),intent(in):: r,g,b
        RGB=UnsignedChar2Int4(r)+UnsignedChar2Int4(g)*256+UnsignedChar2Int4(b)*256*256
    end function RGB
    pure integer(four_bytes) function UnsignedChar2Int4(arg)
        integer(one_byte),intent(in):: arg
        if (arg .ge. 0) then
            UnsignedChar2Int4=arg
            return
        endif
        UnsignedChar2Int4=256+arg
    end function UnsignedChar2Int4
    pure integer(one_byte) function GetColorValue(arg,clr)
        integer(four_bytes),intent(in):: arg
        character*1,intent(in):: clr
        select case (clr)
        case('r')
            GetColorValue=iand(int(255,four_bytes),arg)
        case('g')
            GetColorValue=ISHFTC(iand(int(65280,four_bytes),arg),-8)
        case('b')
            GetColorValue=ISHFTC(iand(int(16711680,four_bytes),arg),-16)
        case default
!           write (*,*) "Ку"
        endselect
    end function GetColorValue
end module bmp_conversions

module bmpfile
    use bmp_conversions
    use integral_types

    !Заголовок битмапа
    !Bitmap header
    type t_bmp
        character(2) bfType
        integer(four_bytes) bfSize
        integer(four_bytes) bfReserved
        integer(four_bytes) bfOffs
        integer(four_bytes) biSize
        integer(four_bytes) biWidth
        integer(four_bytes) biHeight
        integer(two_bytes)  biPlanes
        integer(two_bytes)  biBitCnt
        integer(four_bytes) biCompr
        integer(four_bytes) biSizeIm
        integer(four_bytes) biXPels
        integer(four_bytes) biYPels
        integer(four_bytes) biClrUsed
        integer(four_bytes) biClrImp
    end type t_bmp

    integer(four_bytes),parameter :: BadFile = 1
    integer(four_bytes),parameter :: IOError = 2
    integer(four_bytes),parameter :: BadFrmt = 3
!   integer(four_bytes), allocatable :: ImageForOutput(:,:)

    contains
        !Читает заголовок битмапа. Если (вроде) нет ошибок, возвращает 0.
        !Reads a bitmap header. If no errors returns 0.
        integer(four_bytes) function GetBitmap(bitmap, unitnum)
            type (t_bmp) bitmap
            integer(four_bytes) unitnum
            integer(four_bytes) status
            character(256) msg

            read(unitnum,iostat=status) bitmap%bfType
            if (status.ne.0) then
                msg = ''; write(msg, *) status
                !msg = 'Ошибка чтения из файла # ' // trim(adjustl(msg))
                msg = 'Error reading file # ' // trim(adjustl(msg))
                write (*,*) msg
                GetBitmap = IOError
                return
            endif
            !Неправильный файл подсунули
            !Incorrect file type
            if (bitmap%bfType.ne.'BM') then
                GetBitmap = BadFile
                return
            endif

            read(unitnum,iostat=status) bitmap%bfSize, &
                &       bitmap%bfReserved,      &
                &       bitmap%bfOffs,          &
                &       bitmap%biSize,          &
                &       bitmap%biWidth,         &
                &       bitmap%biHeight,        &
                &       bitmap%biPlanes,        &
                &       bitmap%biBitCnt,        &
                &       bitmap%biCompr,         &
                &       bitmap%biSizeIm,        &
                &       bitmap%biXPels,         &
                &       bitmap%biYPels,         &
                &       bitmap%biClrUsed,       &
                &       bitmap%biClrImp
            if (status.ne.0) then
                msg = ''; write(msg, *) status
                !msg = 'Ошибка чтения из файла # ' // trim(adjustl(msg))
                msg = 'Error reading file # ' // trim(adjustl(msg))
                write (*,*) msg
                GetBitmap = IOError
                return
            endif
            !Не умеем читать жатый битмап
            !Unable to read rle-compressed bitmap
            if (bitmap%biCompr.ne.0) then
                GetBitmap = BadFrmt
                return
            endif
            !Вроде бы и RGB, а вроде бы и с палитрой -- фигня
            !Unable to read rgb bitmap with palette
            if (bitmap%biBitCnt.eq.24.and.bitmap%biClrUsed.ne.0) then
                GetBitmap = BadFrmt
                return
            endif
            GetBitmap = 0
        end function GetBitmap

        !Читает палитру, если получается - возвращает 0.
        !Reads palette, returns 0 if success
        integer(four_bytes) function ReadPalette(array)
            integer(four_bytes) array(0:), status
            integer(two_bytes) i
            integer(one_byte) b,g,r,rgbres
            character(256) msg
            do i = 0, size(array)-1
                read(1, iostat = status) b,g,r,rgbres
                if (status.ne.0) then
                    msg = ''; write(msg, *) status
                    msg = 'Ошибка чтения из файла # ' // trim(adjustl(msg))
                    write (*,*) status
                    ReadPalette = IOError
                    return
                endif
                array(i) = RGB(r,g,b)
            enddo
            ReadPalette = 0
        end function ReadPalette

        !В 1 и 4-хбитных файлах порядок следования точек обратный (от старшего к младшему)
        !Эта функция его "восстанавливает": в случае 1-битного изображения меняет порядок
        !на обратный(?), в случае 4-хбитного - меняет местами четвёрки бит
        !1- and 4-bit bitmaps use a reverse order of bits in data block
        !This function changes (makes direct) the bit sequence
        integer(four_bytes) function TransposeBits(var,bitcount)
            integer(four_bytes) var,swp
            integer(two_bytes) bitcount
            integer(one_byte) i,j
            if (bitcount.eq.1) then
                do i=0,3
                    do j=0,7
                        call mvbits(var,i*8+j,1,swp,i*8+7-j)
                    enddo
                enddo
            elseif (bitcount.eq.4) then
                do i=0,3
                    call mvbits(var,i*8,4,swp,i*8+4)
                    call mvbits(var,i*8+4,4,swp,i*8)
                enddo
            endif
            TransposeBits=swp
        end function TransposeBits

!Эта функция создаёт структуру "палитра виндовс" и возвращает на неё ссылку.
!Не используем пока.
!This function creates "windows palette" structure and returns corresponding pointer.
!Not used at this moment.

!       integer(four_bytes) function GetWinPalette(array)
!           integer(four_bytes) array(0:)
!           integer(four_bytes) nColors, i, status
!           type(T_PALETTEENTRY) PalEntry(0:256)
!           type(T_LOGPALETTE) LogPal
!           equivalence(LogPal,PalEntry)
!
!           nColors = size(array)
!           LogPal%palVersion = #300
!           LogPal%palNumEntries = nColors
!
!           do i = 0, nColors-1
!               PalEntry(i+1)%peRed = GetRedValue(array(i))
!               PalEntry(i+1)%peGreen = GetGreenValue(array(i))
!               PalEntry(i+1)%peBlue = GetBlueValue(array(i))
!               PalEntry(i+1)%peFlags = 0
!           end do
!
!           GetWinPalette = CreatePalette(LogPal)
!       end function GetWinPalette

!Сохраненяет битмапа (только rgb-типа)
!Saves the bitmap (rgb-type only)
integer(four_bytes) function SaveBitmap(arg,fname)
    integer(four_bytes) arg(1:,1:),unitnum,i,status,j,strlen
!   integer(one_byte),parameter :: chr255=Z'FF'
    integer(one_byte),parameter :: chr255=-1
    character(*) fname
!   If (fname(LEN_TRIM(fname)-3:LEN_TRIM(fname))) .ne. '.bmp') fname = TRIM(fname)//'.bmp'
    unitnum=2
    !Если (if) gfortran (Fortran 2003)
    open (unitnum,file=fname,access='stream',iostat=status) !,status='replace')
    !Если (if) openf95 (Fortran 95)
    !open (unitnum,file=fname,form='binary',iostat=status) !,status='replace')
    write(unitnum) "BM"                                    !bfType As Integer
    write(unitnum) int(54 + ubound(arg,1)*ScanAlign(ubound(arg,2)),four_bytes) !bfSize As Long
!   write(*,*) int(54 + ubound(arg,1)*ScanAlign(ubound(arg,2)),four_bytes) !bfSize As Long
    write(unitnum) int(0,four_bytes)                                !bfReserved1 As Integer + bfReserved2 As Integer
    write(unitnum) int(54,four_bytes)                               !bfOffBits As Long

    write(unitnum) int(40,four_bytes)                             !biSize As Long
    write(unitnum) int(ubound(arg,2))                    !biWidth As Long
    write(unitnum) int(ubound(arg,1))                    !biHeight As Long
    write(unitnum) int(1,two_bytes)                              !biPlanes As Integer
    write(unitnum) int(24,two_bytes)                             !biBitCount As Integer
    write(unitnum) int(0,four_bytes)                              !biCompression As Long
    write(unitnum) int(ubound(arg,1)*ubound(arg,2),four_bytes)    !biSizeImage As Long
    write(unitnum) int(0,four_bytes) !biXPelsPerMeter As Long
    write(unitnum) int(0,four_bytes) !biYPelsPerMeter As Long
    write(unitnum) int(0,four_bytes) !biClrUsed As Long
    write(unitnum) int(0,four_bytes) !biClrImportant As Long
    strlen=ScanAlign(ubound(arg,2))-ubound(arg,2)*3
!   write(*,*) ScanAlign(ubound(arg,2)), ubound(arg,2)
    do i=ubound(arg,1),1,-1
        do j=1,ubound(arg,2)
            write(unitnum) GetColorValue(arg(i,j),'b'),GetColorValue(arg(i,j),'g'),GetColorValue(arg(i,j),'r')
        enddo
        write(unitnum) (chr255,j=1,strlen)
    enddo
    close(unitnum)
    SaveBitmap=0
end function SaveBitmap

integer(four_bytes) function ScanAlign(pwidth)
    integer(four_bytes) pwidth
    ScanAlign = iand((pwidth*3 + 3), Z'FFFFFFFC')
!    if (real(pwidth*3)/real(4) .gt. aint(real(pwidth * 3)/real(4))) then
!       ScanAlign = int( aint( real(pwidth*3) / real(4) ) )*4 + 4
!    else
!       ScanAlign=pwidth
!    endif
end function ScanAlign

integer(four_bytes) function OpenBitmap(ImageForOutput,fname)
!   use conversions

    type (t_bmp) bitmap

    integer(one_byte), allocatable :: BmpTripleData(:)
    integer(four_bytes) status,i,j,k,pos,step,offset
    integer(four_bytes), allocatable :: ImageForOutput(:,:),BmpData(:),palette(:)
!   integer(four_bytes), allocatable :: BmpData(:),palette(:)
    character(*) fname
    character(100) msg

    !Открытие файла
    !Opening file
    !Если (if) gfortran (Fortran 2003)
    open (1,file=fname,access='stream',form='unformatted',iostat=status, status='old')
    !Если (if) openf95 (Fortran 95)
    !open (1,file=fname,form='binary',iostat=status, status='old')

    !Проверка успешности открытия файла
    !Verifying file open
    if (status.ne.0) then
        msg = ''; write(msg, *) status
        msg = 'Ошибка доступа к файлу # ' // trim(adjustl(msg))
        write (*,*) status
        OpenBitmap=0
        return
    endif

    !Считывание заголовка
    !Reading the header
    status = GetBitmap(bitmap, 1)

    !Проверка правильности считывания заголовка
    !Verifying the header
    if (status.ne.0) then
        msg = ''; write(msg, *) status
        msg = 'Ошибка функции GetBitmap # ' // trim(adjustl(msg))
        write (*,*) status
        close(1)
        OpenBitmap=0
        return
    endif

    allocate(ImageForOutput(bitmap%biHeight,bitmap%biWidth))

    select case (bitmap%biBitCnt)
        case(1,4,8) !Растр с палитрой !Raster with palette
            allocate(BmpData((bitmap%bfSize-bitmap%bfOffs)/4))
            !Считывание палитры !Reading the palette
            if (bitmap%biClrUsed.eq.0) then
                allocate(palette(0:2**bitmap%biBitCnt-1))
                status = ReadPalette(palette)
            else
                allocate(palette(0:bitmap%biClrUsed-1))
                status = ReadPalette(palette)
            endif

            !Проверка успешности считывания палитры
            !Verifying the palette
            if (status.ne.0) then
                msg = ''; write(msg, *) status
                !msg = 'Ошибка функции ReadPalette # ' // trim(adjustl(msg))
                msg = 'Error in function ReadPalette # ' // trim(adjustl(msg))
                write (*,*) status
                deallocate(BmpData,ImageForOutput,palette)
                close(1)
                OpenBitmap=0
                return
            endif

            !Считывание области данных
            !Reading the data block
            read(1, iostat=status) BmpData
            close(1)

            !Проверка успешности считывания области данных
            !Verifying the data block
            if (status.ne.0) then
                msg = ''; write(msg, *) status
                !msg = 'Ошибка чтения данных # ' // trim(adjustl(msg))
                msg = 'Error reading data block # ' // trim(adjustl(msg))
                write (*,*) status
                deallocate(BmpData,ImageForOutput,palette)
                OpenBitmap=0
                return
            endif

            !"Переворачивание" байтов в одно- и четырехбитных растрах
            !Transposing the data bits
            if ((bitmap%biBitCnt.eq.1).or.(bitmap%biBitCnt.eq.4)) then
                do i=1,ubound(BmpData,1)
                    BmpData(i)=TransposeBits(BmpData(i),bitmap%biBitCnt)
                enddo
            endif

            offset=0
            step = 32/bitmap%biBitCnt
            do i=1,bitmap%biHeight
                do j=0,bitmap%biWidth-1,step
                    offset=offset+1
                    do k=1,step
                        pos=j+k
                        if (pos.gt.bitmap%biWidth) exit
                        ImageForOutput(bitmap%biHeight-i+1,pos)=palette(        &
                    &   int(ibits(BmpData(offset),((k-1)*bitmap%biBitCnt),bitmap%biBitCnt),two_bytes))
                    enddo
                enddo
            enddo
            deallocate(BmpData,palette)
            OpenBitmap=status

        case(24) !RGB data
            allocate(BmpTripleData(bitmap%bfSize-bitmap%bfOffs))
            !Проверка размерности созданного массива
            !Verifying the array dim-s
            !write(*,*) bitmap%bfSize,bitmap%bfOffs

            !Считывание области данных
            !Reading data block
            read(1, iostat=status) BmpTripleData
            close(1)

            !Проверка правильности считывания области данных
            !Verifying the data block
            if (status.ne.0) then
                msg = ''; write(msg, *) status
                msg = 'Ошибка чтения данных # ' // trim(adjustl(msg))
                write (*,*) status
                deallocate(BmpTripleData,ImageForOutput)
                OpenBitmap=0
                return
            endif

            offset=-2
            k=ubound(BmpTripleData,1)/bitmap%biHeight-bitmap%biWidth*3
            !!Проверка правильности расчёта числа байт, необходимых для выравнивания строки по двойному слову
            !!Можно использовать функцию ScanAlign
            !!Verifying the number of bits needed to align the lines at a double word. Function ScanAlign can be used
            !!write(*,*) k,ubound(BmpTripleData,1),bitmap%biHeight,bitmap%biWidth
            do i=1,bitmap%biHeight
                do j=1,bitmap%biWidth
                    offset=offset+3
                    ImageForOutput(bitmap%biHeight-i+1, j) = &
                        RGB(BmpTripleData(offset+2), BmpTripleData(offset+1),BmpTripleData(offset))
                enddo
                offset=offset+k
            enddo
            deallocate(BmpTripleData)
            OpenBitmap=status
        case default
            !write (*,*) 'Unknown file format'
            write (*,*) 'Неизвестный формат файла'
            close(1)
            OpenBitmap=0
    endselect

!   write (*,*) ubound(ImageForOutput,1),ubound(ImageForOutput,2)
!   write (*,*) ImageForOutput(170,702)
!   write (*,*) GetColorValue(ImageForOutput(170,702),'r')
!   write (*,*) GetColorValue(ImageForOutput(170,702),'g')
!   write (*,*) GetColorValue(ImageForOutput(170,702),'b')

!   status=SaveBitmap(ImageForOutput,'./grrr.bmp')
!   deallocate(ImageForOutput)
end function OpenBitmap

end module bmpfile
