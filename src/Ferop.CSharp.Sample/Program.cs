using Ferop;

namespace Ferop.CSharp.Sample
{
    [Ferop]
    public class Program
    {
        [Import]
        public static int AddOne (int x)
        {
            return Ferop.C<int> ("return x + 1;");
        }

        static void Main(string[] args)
        {
            System.Console.WriteLine(AddOne(5));
            System.Console.Read();
        }
    }
}
