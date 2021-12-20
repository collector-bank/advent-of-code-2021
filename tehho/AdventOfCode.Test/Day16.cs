using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;
using System.Collections.Generic;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay16
  {
    private IDay<long> day;
    private string[] data;

    public TestDay16()
    {
      day = new Day16();

      data = File.ReadAllLines(day.Folder + "/data.test");


    }

    [TestMethod]
    public void Logic1()
    {
      for (int i=0; i < 4; i++)
      {
        var temp = data[i].Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
        var expected = int.Parse(temp[1]);
        var result = day.Logic(new string[] {temp[0]});
        Assert.AreEqual(expected, result);
      }
    }

    [TestMethod]
    public void Logic2()
    {
      for (int i=4; i < data.Length; i++)
      {
        var temp = data[i].Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
        var expected = long.Parse(temp[1]);
        var result = day.Logic2(new string[] {temp[0]});
        Assert.AreEqual(expected, result);
      }
    }
  }
}