program matrix

  implicit none
  integer, parameter :: n = 5
  integer :: i, j, k
  real(8) :: a(n, n), b(n, n), c(n, n), total, t1, t2

  do i = 1, n
    do j = 1, n
      a(i, j) = (i-1) * n + (j-1)
      b(i, j) = (j-1) * n + (i-1)
      c(i, j) = 0.0d0
    end do
  end do

  call cpu_time(t1)
  do i = 1, n
    do j = 1, n
      do k = 1, n
        c(i, j) = c(i, j) + a(i, k) * b(k, j)
      end do
    end do
  end do
  call cpu_time(t2)
  print *, "cpu time:", t2-t1, "sec"

  total = 0.0d0
  do i = 1, n
    do j = 1, n
      total = total + c(i, j)
    end do
  end do

  write(*,*) 'sum:', total
  !write(*,*) c(1,1)
end program matrix
