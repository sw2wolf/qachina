//#!/home/sw2wolf/tcc/bin/tcc -run
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>

void got_alarm(int sig) {
	printf("ok~d",sig);
	fflush(stdout);
//execvp("xinit", (char *[]){"xinit", NULL});
}


int main()
{
	char c;

    printf("Which WM to you want(1/2/9):\n");
	printf("1:stumpwm-clisp\n");
	printf("2:dwm-dbg\n");
	printf("9:console\n");


	alarm(5);
	signal(SIGALRM, got_alarm);

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
