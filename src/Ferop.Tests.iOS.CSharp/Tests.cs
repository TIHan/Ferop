using System;
using NUnit.Framework;
using System.Runtime.InteropServices;

namespace Ferop.Tests.iOS.CSharp
{
	[TestFixture]
	public class Tests
	{
		[DllImport("__Internal")]
		public static extern byte Tests_testByte (byte x);

		[Test]
		public void TestByte ()
		{
			Assert.AreEqual (Byte.MaxValue, Tests_testByte (Byte.MaxValue));
		}
	}
}

