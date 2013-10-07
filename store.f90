subroutine storedata(idx,N,fmeantmp,fvartmp,fmeanprimetmp,fvarprimetmp,fmeandbleprimetmp,fvardbleprimetmp,DAT)

  implicit none
  integer,intent(in)::idx,N

  real*8,intent(in)::fmeantmp,fvartmp,fmeanprimetmp(n),fvarprimetmp(n)
  real*8::fmeandbleprimetmp(n,n),fvardbleprimetmp(n,n)

  real*8,intent(out)::DAT(*)


  integer::cnt,i,j

  cnt=idx
  dat(cnt)=fmeantmp

  do i=1,n
     cnt=cnt+1     
     dat(cnt)=fmeanprimetmp(cnt-1)
  end do

  do i=1,n
     do j=1,n
        if (i.le.j) then
           cnt=cnt+1
           dat(cnt)=fmeandbleprimetmp(i,j)
        end if
     end do
  end do

  cnt=cnt+1

  dat(cnt)=fvartmp

  do i=1,n
     cnt=cnt+1     
     dat(cnt)=fvarprimetmp(cnt-1)
  end do

  do i=1,n
     do j=1,n
        if (i.le.j) then
           cnt=cnt+1
           dat(cnt)=fvardbleprimetmp(i,j)
        endif
     end do
  end do

  cnt=idx
  dat(cnt)=fmeantmp

  do i=1,n
     cnt=cnt+1     
     dat(cnt)=fmeanprimetmp(cnt-1)
  end do

  do i=1,n
     do j=1,n
        if (i.le.j) then
           cnt=cnt+1
           dat(cnt)=fmeandbleprimetmp(i,j)
        end if
     end do
  end do

  cnt=cnt+1

  dat(cnt)=fvartmp

  do i=1,n
     cnt=cnt+1     
     dat(cnt)=fvarprimetmp(cnt-1)
  end do

  do i=1,n
     do j=1,n
        if (i.le.j) then
           cnt=cnt+1
           dat(cnt)=fvardbleprimetmp(i,j)
        endif
     end do
  end do

  return
end subroutine storedata
