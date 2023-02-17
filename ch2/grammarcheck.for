      program circle
        DOUBLE PRECISION r, area
        integer i,j

c      This program reads a real number r and prints
c      the area of a circle with radius r.
        do 100 i=1,3
          do 12 j=1,3
   12       write (*,*) 'Give radius r'
  100       write (*,*) 'Give radius r:'

            read  (*,*) r
            area = DSQRT(r)
            write (*,10) area
   10       format (/,1X,'Area =',g13.5,/,1X,/)
   

        stop
        end