
//////
#include <unistd.h>
#include <sys/io.h>

#define I8042_COMMAND_REG 0x64

int main(int argc, char *argv[]) {
	char data = 0xae; // enable keyboard

	ioperm(I8042_COMMAND_REG, 1, 1);
	if (argc == 2 && argv[1][0] == '0')
		data = 0xad; // disable keyboard
	outb(data, I8042_COMMAND_REG);

	return 0;
}

//////
#include <time.h>
#include <stdio.h>

/**                                                                             
 * sleep for `sec' seconds, without relying on the wall clock of time(2)        
 * or gettimeofday(2).                                                          
 *                                                                              
 * under ideal conditions is accurate to one microsecond. To get nanosecond     
 * accuracy, replace sleep()/usleep() with something with higher resolution     
 * like nanosleep() or ppoll().                                          
 */
void
true_sleep(int sec)
{
        struct timespec ts_start;
        struct timespec ts_end;

	clock_gettime(CLOCK_MONOTONIC, &ts_start);

        ts_end = ts_start;
        ts_end.tv_sec += sec;

        for(;;) {
                struct timespec ts_current;
                struct timespec ts_remaining;

                clock_gettime(CLOCK_MONOTONIC, &ts_current);

		ts_remaining.tv_sec = ts_end.tv_sec - ts_current.tv_sec;
                ts_remaining.tv_nsec = ts_end.tv_nsec - ts_current.tv_nsec;
                while (ts_remaining.tv_nsec > 1000000000) {
                        ts_remaining.tv_sec++;
                        ts_remaining.tv_nsec -= 1000000000;
                }
		while (ts_remaining.tv_nsec < 0) {
			ts_remaining.tv_sec--;
                        ts_remaining.tv_nsec += 1000000000;
                }

                if (ts_remaining.tv_sec < 0) {
                        break;
                }

                if (ts_remaining.tv_sec > 0) {
                        sleep(ts_remaining.tv_sec);
                } else {
                        usleep(ts_remaining.tv_nsec / 1000);
                }
        }
}

int
main()
{
        true_sleep(10);
}

//////
//git show 'HEAD@{1}..HEAD'

/* before the 'git pull', which is shown in the output of 'git pull': the first hex number in a range that might look something like this: "950a966..92b793d  stable-2.0 -> origin/stable-2.0" and then there are a couple of options.  you could run "git diff <hex>..92b793d" to see the diff. or you could run "git log -p" to see each commit as a separate diff, until you get to the commit that starts with that <hex>  number. */

/* if you use emacs, I recommend the 'magit' package, which makes working with git much more pleasant.  you can run M-x magit-log  to see the list of commits and hit RET on any line to see the  diff for that commit. */

//////
extern void base64_encode (const char *restrict in, size_t inlen, char *restrict out, size_t outlen);"
restrict is a type qualifier, which may only be used with a pointer type, and which requires that objects referenced through such a pointer must be made through a single pointer value, i.e., no aliases / pointers into other parts of the object are allowed

//////
main()
{
	int a = ({int b; b = 10; b +=1; b;});
	return a;
}

//$ tcc -run test.c 
//$ echo $?
//11
