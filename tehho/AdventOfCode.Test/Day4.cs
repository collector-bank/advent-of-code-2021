using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay4
  {
    private IDay<int> day;
    private string[] data;

    public TestDay4()
    {
      day = new Day4();
      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
      var expected = 4512;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 1924;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}