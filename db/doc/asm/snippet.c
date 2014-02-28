
//////
main()
{
	int a = ({int b; b = 10; b +=1; b;});
	return a;
}

//$ tcc -run test.c 
//$ echo $?
//11
