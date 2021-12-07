using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay3
  {
    private IDay<int> day;
    private string[] data;

    public TestDay3()
    {
      day = new Day3();
      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
      var expected = 198;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 230;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}