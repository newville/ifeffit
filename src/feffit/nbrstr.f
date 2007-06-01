      integer function nbrstr(string)
c
c  find a number in a string
c  given a string that is known to begin with a digit or sign.
c  return the position of the end of the number.
c  nbrstr : position of end of number
      integer   istrln, i, ilen, iback
      character*(*)  string
      character*1    digit*10, plus, minus, d, e, decml, s, sp
      logical     lexp, ldecml
      data digit  /'1234567890'/
      data minus,plus,d,e,decml /'-','+','d','e','.'/
c------
      ldecml = .false.
      lexp   = .false.
      ilen   = istrln(string)
      nbrstr = ilen
      if (ilen.gt.1) then
         iback  = 1
c find end of number :  digits are always ok.
c stop at second d, e, decml, or sign that's not preceded by (d,e)
         do 200 i = 2, ilen
            sp = string(i-1:i-1)
            s  = string(i:i)
            if (index(digit,s).eq.0) then
               if ( (((s.ne.plus).and.(s.ne.minus).and.(s.ne.d)
     $                 .and.(s.ne.e).and.(s.ne.decml)))
     $          .or.((lexp.and.((s.eq.d).or.(s.eq.e))))
     $          .or.((ldecml.and.(s.eq.decml)))
     $          .or.((((s.eq.plus).or.(s.eq.minus)).and.
     $                (sp.ne.d).and.(sp.ne.e))) )     go to 210
               lexp   = lexp.or.(s.eq.d).or.(s.eq.e)
               ldecml = ldecml.or.(s.eq.decml)
            end if
 200     continue
         iback = 0
 210     continue
         nbrstr = i - 1 - iback
      end if
      return
c  end function nbrstr
      end
