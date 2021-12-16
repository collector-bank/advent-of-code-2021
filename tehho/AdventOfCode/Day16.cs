using System.Linq;
using System;
using System.Text;
using System.Collections;

namespace AdventOfCode
{
  public class Day16 : Day<long>
  {
    public override string Folder => "16";

    private abstract class Package
    {
      public int Version;
      public int Type;
      public int BitsRead;
      public abstract long Number {get;}

      protected Package(int version, int type, int bitsRead)
      {
        Version = version;
        Type = type;
        BitsRead = bitsRead;
      }
    }

    private class NumberPackage : Package
    {
      private long _number;


      public NumberPackage(long number, int bitsRead, int version) 
        : base(version, 4, bitsRead)
      {
        _number = number;
      }
      public override long Number => _number;
    }

    private class CommandPackage : Package
    {
      
      public List<Package> Packages = new List<Package>();

      public override long Number => 
        Type == 0 ? Packages.Sum(p => p.Number) :
        Type == 1 ? Packages.Aggregate(1, (acc, p) => acc * (int)p.Number) :
        Type == 2 ? Packages.Min(p => p.Number) :
        Type == 3 ? Packages.Max(p => p.Number) :
        Type == 5 ? Packages[0].Number > Packages[1].Number ? 1 : 0 :
        Type == 6 ? Packages[0].Number < Packages[1].Number ? 1 : 0 :
        Type == 7 ? Packages[0].Number == Packages[1].Number ? 1 : 0 :
        -1;

      public CommandPackage(List<Package> packages, int bitsRead, int version, int type) 
        : base(version, type, bitsRead)
      {
        Packages = packages;
      }
    }

    public override long Logic(string[] data)
    {
      tries = 0;

      var bytes = Enumerable.Range(0, (data[0].Length + 1)/2)
        .Select(i => byte.Parse(data[0].Substring(i * 2, 2), System.Globalization.NumberStyles.HexNumber))
        .Reverse()
        .ToArray();

      var bits = new BitArray(bytes)
        .Cast<bool>()
        .Reverse()
        .ToArray();

      var package = ParsePackage(bits);

      return SumPackageVersion(package);
    }

    public override long Logic2(string[] data)
    {
      tries = 0;

      var bytes = Enumerable.Range(0, (data[0].Length + 1)/2)
        .Select(i => byte.Parse(data[0].Substring(i * 2, 2), System.Globalization.NumberStyles.HexNumber))
        .Reverse()
        .ToArray();

      var bits = new BitArray(bytes)
        .Cast<bool>()
        .Reverse()
        .ToArray();

      var package = ParsePackage(bits);

      return package.Number;
    }

    private Package ParsePackage(bool[] bits)
    {
      int packageStart = 0;
      var length = 3;

      IEnumerable<bool> temp;
      temp = bits[packageStart..(packageStart + length)];
      var version = ConvertBoolArrayToInt(temp);
      packageStart += length;

      temp = bits[packageStart..(packageStart + length)];
      var type = ConvertBoolArrayToInt(temp);
      packageStart += length;

      if (type == 4)
      {
        bool continueLoop;
        var values = new List<bool>();
        length = 5;
        do
        {
          continueLoop = bits[packageStart];
          
          temp = bits[(packageStart + 1)..(packageStart + length)];
          values.AddRange(temp);

          packageStart += length;
        } while (continueLoop);

        return new NumberPackage(ConvertBoolArrayToLong(values), packageStart, version);
      }
      else
      {
        var id = bits[packageStart];
        packageStart++;

        var packages = new List<Package>();

        if (id)
        {
          length = 11;
          temp = bits[packageStart..(packageStart + length)];
          var nrOfPackages = ConvertBoolArrayToInt(temp);
          packageStart += length;

          for (int i = 0; i < nrOfPackages; i++)
          {
            var p = ParsePackage(bits[packageStart..]);
            packages.Add(p);
            packageStart += p.BitsRead;
          }
        }
        else
        {
          length = 15;
          temp = bits[packageStart..(packageStart + length)];
          var packageBits = ConvertBoolArrayToInt(temp);
          packageStart += length;

          for (int i = 0; i < packageBits;)
          {
            var p = ParsePackage(bits[packageStart..]);
            packages.Add(p);
            i += p.BitsRead;
            packageStart += p.BitsRead;
          }
        }

        return new CommandPackage(packages, packageStart, version, type);
      }
    }

    private int SumPackageVersion(Package package)
    {
      var result = package.Version;

      if (package is CommandPackage)
      {
        var comPackage = package as CommandPackage;
        result += comPackage.Packages.Select(SumPackageVersion).Sum();
      }

      return result;
    }


    private long ConvertBoolArrayToLong(IEnumerable<bool> bits)
    {
      var sb = new StringBuilder();
      foreach (var bit in bits)
      {
        sb.Append(bit ? '1' : '0');
      }
      return Convert.ToInt64(sb.ToString(), 2);
    }

    private int ConvertBoolArrayToInt(IEnumerable<bool> bits)
    {
      if (bits.Count() > 32)
      {
        throw new Exception("Too many bits");
      }

      var sb = new StringBuilder();
      foreach (var bit in bits)
      {
        sb.Append(bit ? '1' : '0');
      }
      return Convert.ToInt32(sb.ToString(), 2);
    }
  }
}