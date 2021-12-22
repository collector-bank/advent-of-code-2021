using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;
using System.Collections.Generic;
using System.Linq;

namespace adventofcode2121.Test
{
  [TestClass]
  public class TestDay21
  {
    private IDay<long> day;
    private string[] data;

    public TestDay21()
    {
      day = new Day21();

      data = File.ReadAllLines(day.Folder + "/data.test");
    }
  
    [TestMethod]
    public void Logic1()
    {
      var expected = 739785;
      var result = day.Logic(data);
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = 444356092776315;
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}