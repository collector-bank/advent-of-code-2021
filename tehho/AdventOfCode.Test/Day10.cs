using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay10
  {
    private IDay<long> day;
    private string[] data;

    public TestDay10()
    {
      day = new Day10();
      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
      var expected = 26397;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 288957;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}