using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay8
  {
    private IDay<int> day;
    private string[] data;

    public TestDay8()
    {
      day = new Day8();
      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
      var expected = 26;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 61229;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}