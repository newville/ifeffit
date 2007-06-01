      integer function nxtunt(iunit)
c  return next available unit number, greater than or equal to iunit.
c  will not return unit number less than 1, or equal to 5 or 6.
      integer iunit
      logical open

      nxtunt = max(1, iunit)
 10   continue
      inquire (unit=nxtunt, opened=open)
      if (open) then
          nxtunt = nxtunt + 1
          if ((nxtunt.eq.5).or.(nxtunt.eq.6)) nxtunt = 7
          goto 10
      endif
      return
c  end integer function nxtunt
      end
