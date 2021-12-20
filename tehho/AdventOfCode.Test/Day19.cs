using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;
using System.Collections.Generic;
using System.Linq;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay19
  {
    private IDay<int> day;
    private string[] data;

    public TestDay19()
    {
      day = new Day19();

      data = File.ReadAllLines(day.Folder + "/data.test");
    }
  
    [TestMethod]
    public void Logic1()
    {
      var expected = 79;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 3621;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}