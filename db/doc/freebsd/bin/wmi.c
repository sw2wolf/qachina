<<<<<<< HEAD
#!/home/***/tcc/bin/tcc -run
=======
#!/home/sw2wolf/tcc/bin/tcc -run
>>>>>>> d793b5a536546fcfd71feb7a6d5cbd1cb7f1307d
#define __aligned(N) __attribute__ ((aligned (N)))
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>

void got_alarm(int sig) {
	/* printf("ok\n",sig); */
	/* fflush(stdout); */
	execvp("xinit", (char *[]){"xinit", NULL});
}

int main() {
	char c;

    printf("Which WM to you want(1/2/9):\n");
	printf("1:stumpwm-clisp\n");
	printf("2:dwm-dbg\n");
	printf("9:console\n");

	signal(SIGALRM, got_alarm);
	alarm(5);

	c = getchar();
	switch(c) {
		case '1':
			execvp("xinit", (char *[]){"xinit", "clisp", 0});
			break;
		case '2':
			execvp("xinit", (char *[]){"xinit", "dwm", 0});
			break;
		case '9': break;
		default:
			execvp("xinit", (char *[]){"xinit", 0});
			break;
	}

    return 0;
}
