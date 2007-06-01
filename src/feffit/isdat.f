       logical function isdat(string)
c  tests if string contains numerical data
c    returns true if the first (up to eight) words in string can
c    all be numbers. requires at least two words, and tests only
c    the first eight columns
       integer nwords, mwords, i
       parameter (mwords = 8)
       character*(30)  string*(*), words(mwords), line*(256)
       logical isnum
       external isnum
c
       isdat = .false.
       do 10 i = 1, mwords
          words(i) = 'no'
 10    continue
c
       nwords = mwords
       line   = string
       call triml(line)
       call untab(line)
       call bwords(line, nwords, words)
       if (nwords.ge.2) then
          isdat = .true.
          do 50 i = 1, nwords
             if (.not. ( isnum( words(i) ) ) ) isdat = .false.
 50       continue
       end if
c
       return
       end
