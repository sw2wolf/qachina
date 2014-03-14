
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
