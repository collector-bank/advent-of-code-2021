using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;
using System.Collections.Generic;
using System.Linq;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay20
  {
    private IDay<int> day;
    private string[] data;

    public TestDay20()
    {
      day = new Day20();

      data = File.ReadAllLines(day.Folder + "/data.test");
    }
  
    [TestMethod]
    public void Logic1()
    {
      var expected = 35;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 3351;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}