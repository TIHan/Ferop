namespace Native
{
    using Ferop;

    public struct Vector2
    {
        float _x;
        public float X
        { 
            get 
            {
                return _x;
            }
        }
        public float Y { get; set; }
    }

    [Ferop]
    [Header("#include <stdio.h>")]
    class Native
    {
        [Import]
        public static void PrintVector2(Vector2 v)
        {
            Ferop.C (@"printf(""Vector2: %f %f\n"", v.X, v.Y);");
        }

        static void Main(string[] args)
        {
            PrintVector2(new Vector2());
            System.Console.Read();
        }
    }
}
