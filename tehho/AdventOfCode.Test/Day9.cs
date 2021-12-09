using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay9
  {
    private IDay<int> day;
    private string[] data;

    public TestDay9()
    {
      day = new Day9();
      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
      var expected = 15;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 1134;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}