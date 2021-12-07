using System.Diagnostics;

namespace AdventOfCode
{
  public abstract class Day<T> : IDay<T>
  {
    protected int tries = 0;
    public int Tries => tries;

    public abstract string Folder {get;}

    public abstract T Logic(string[] data);

    public abstract T Logic2(string[] data);
  }
}