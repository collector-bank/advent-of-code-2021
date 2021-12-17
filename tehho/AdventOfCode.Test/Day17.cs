using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;
using System.Collections.Generic;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay17
  {
    private IDay<int> day;
    private string[] data;

    public TestDay17()
    {
      day = new Day17();

      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
        var expected = 45;
        var result = day.Logic(data);
        Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 112;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}