kilo: kilo.c
		$(CC) kilo.c -o kilo -Wall -Wextra -pedantic -std=c99

# "kilo: kilo.c" --> I want to build "kilo" by "kilo.c"
# "$(CC)" ---------> CC by default (C compiler)
# "-Wall" ---------> all warnings
# "-Wextra" -------> extra warnings
# "-pedantic" -----> more warnings
# "-std=c99" ------> C standard version

# To compile: "$ make".