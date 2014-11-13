namespace Native
{
    using Ferop;

    [Ferop]
    class Native
    {
        [Import]
        public static void PrintHelloWorld()
        {
            // 'object' is just a placeholder here for void return types.
            Ferop.C<object>("printf(\"Hello World!\\n\");");
        }

        static void Main(string[] args)
        {
            Native.PrintHelloWorld();
            System.Console.Read();
        }
    }
}
