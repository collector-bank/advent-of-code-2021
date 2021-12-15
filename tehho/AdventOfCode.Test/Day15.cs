using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay15
  {
    private IDay<int> day;
    private string[] data;

    public TestDay15()
    {
      day = new Day15();
      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
      var expected = 40;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 315;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}