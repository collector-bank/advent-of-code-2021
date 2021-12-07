using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay6
  {
    private IDay<Int64> day;
    private string[] data;

    public TestDay6()
    {
      day = new Day6();
      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
      var expected = 5934;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 26984457539;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}