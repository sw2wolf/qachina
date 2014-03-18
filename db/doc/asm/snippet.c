
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
