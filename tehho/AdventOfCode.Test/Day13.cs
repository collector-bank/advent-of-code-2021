using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay13
  {
    private IDay<string> day;
    private string[] data;

    public TestDay13()
    {
      day = new Day13();
      data = File.ReadAllLines(day.Folder + "/data.test");
    }

    [TestMethod]
    public void Logic1()
    {
      var expected = 17;
      var result = int.Parse(day.Logic(data));
      Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void Logic2()
    {
      var expected = "\n#####\n#   #\n#   #\n#   #\n#####\n     \n     ";
      var result = day.Logic2(data);
      Assert.AreEqual(expected, result);
    }
  }
}