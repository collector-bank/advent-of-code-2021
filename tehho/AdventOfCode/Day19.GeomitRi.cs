using GeometRi;

namespace AdventOfCode
{
  public static class Vector3dExtensions
  {
    public static Vector3d Forward => new Vector3d(0, 0, 1);
    public static Vector3d Backward => new Vector3d(0, 0, -1);
    public static Vector3d Up => new Vector3d(0, 1, 0);
    public static Vector3d Down => new Vector3d(0, -1, 0);
    public static Vector3d Left => new Vector3d(-1, 0, 0);
    public static Vector3d Right => new Vector3d(1, 0, 0);
    public static Vector3d Zero => new Vector3d(0,0,0);

    public static IEnumerable<Vector3d> AllDirections => new[] { Forward, Backward, Up, Down, Left, Right };

    public static IEnumerable<Vector3d> AllUpGivenForward(Vector3d direction)
    {
      if (direction == Forward || direction == Backward)
      {
        return new[] { Up, Down, Left, Right };
      }
      else if (direction == Up || direction == Down)
      {
        return new[] { Forward, Backward, Left, Right };
      }
      else
      {
        return new[] { Forward, Backward, Up, Down };
      }
    }

  }

  public static class Plane3dExtensions
  {
    public static Plane3d XY => new Plane3d(Vector3dExtensions.Zero.ToPoint, Vector3dExtensions.Forward);
    public static Plane3d XZ => new Plane3d(Vector3dExtensions.Zero.ToPoint, Vector3dExtensions.Up);
    public static Plane3d YZ => new Plane3d(Vector3dExtensions.Zero.ToPoint, Vector3dExtensions.Right);
  }

  public static class Matrix3dExtensions
  {
    public static IEnumerable<Func<Vector3d, Vector3d>> AllReflections => new Func<Vector3d, Vector3d>[]
    {
      p => p,
      p => p.ReflectIn(Plane3dExtensions.XY),
      p => p.ReflectIn(Plane3dExtensions.XZ),
      p => p.ReflectIn(Plane3dExtensions.YZ),
      p => p.ReflectIn(Plane3dExtensions.XY).ReflectIn(Plane3dExtensions.XZ),
      p => p.ReflectIn(Plane3dExtensions.XY).ReflectIn(Plane3dExtensions.YZ),
      p => p.ReflectIn(Plane3dExtensions.XZ).ReflectIn(Plane3dExtensions.YZ),
      p => p.ReflectIn(Plane3dExtensions.XY).ReflectIn(Plane3dExtensions.XZ).ReflectIn(Plane3dExtensions.YZ),
    };
  }
}