int foo(void)
{
    int array;
    ;
    return array;
}

int foo2(int array)
{
    int a[10];
    a[foo()];
    return 2-array;
}
