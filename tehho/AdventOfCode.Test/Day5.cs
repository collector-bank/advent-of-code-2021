using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay5
  {
    private IDay<int> day;
    private string[] data;

    public TestDay5()
    {
      day = new Day5();
      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
      var expected = 5;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 12;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}