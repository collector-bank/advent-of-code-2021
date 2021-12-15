using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay14
  {
    private IDay<long> day;
    private string[] data;

    public TestDay14()
    {
      day = new Day14();
      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
      var expected = 1588;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 2188189693529;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}