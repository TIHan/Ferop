namespace Native
{
    using Ferop;

    [Ferop]
    [Header("#include <stdio.h>")]
    class Native
    {
        [Import]
        public static void PrintHelloWorld()
        {
            Ferop.C (@"printf(""Hello World!\n"");");
        }

        static void Main(string[] args)
        {
            PrintHelloWorld();
            System.Console.Read();
        }
    }
}
