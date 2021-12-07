namespace AdventOfCode
{
    public interface IDay<T>
    {
        T Logic(string[] data);
        T Logic2(string[] data);

        string Folder {get;}
        int Tries {get;}
    }
}