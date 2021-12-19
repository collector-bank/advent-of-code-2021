namespace AdventOfCode
{
  public partial class Day18
  {
    private class SnailNumber
    {
      public SnailNumber(SnailNumber? parent, SnailNumber left, SnailNumber right)
      {
        Parent = parent;
        Left = left;
        Left.Parent = this;
        Right = right;
        Right.Parent = this;
        Value = -1;
      }

      public SnailNumber(int value)
      {
        Parent = null;
        Left = null;
        Right = null;
        Value = value;
      }

      public int Value;
      public SnailNumber? Left;
      public SnailNumber? Right;
      public SnailNumber? Parent;

      public int ParentCount => Parent == null ? 0 : Parent.ParentCount + 1;

      public static SnailNumber operator +(SnailNumber left, SnailNumber right)
      {
        
        return new SnailNumber(null, left, right);
      }

      public void Reduce()
      {
        bool changed;
        do
        {
          changed = Explode();
          if (!changed)
            changed = Split();
        }
        while (changed);
      }

      public bool Explode()
      {
        if (ParentCount >= 4)
        {
          // Leaf node
          if (Left == null || Right == null)
          {
            return false;
          }
          else 
          {
            if (Parent!.Right == this)
            {
              var current = NextLeft();

              if (current != null)
                current.Value += Left.Value;
              
              current = NextRight();
              
              if (current != null)
                current.Value += Right.Value;

              Parent.Right = new SnailNumber(0);
              Parent.Right.Parent = Parent;
            }
            if (Parent!.Left == this)
            {
              var current = NextLeft();
              if (current != null)
                current.Value += Left.Value;

              current = NextRight();

              if (current != null)
                current.Value += Right.Value;
              
              Parent.Left = new SnailNumber(0);
              Parent.Left.Parent = Parent;
            }
            return true;
          }
        }
        else
        {
          bool changed = false;
          if (Left != null)
          {
            changed = Left.Explode();
          }
          if (Right != null && !changed)
          {
            changed = Right.Explode();
          }
          return changed;
        }
      }

      public bool Split()
      {
        if (Left == null && Right == null)
        {
          if (Value >= 10)
          {
            var value = Value / 2;
            
            var left = new SnailNumber(value);
            left.Parent = this;
            var right = new SnailNumber(value + Value % 2);
            right.Parent = this;

            Left = left;
            Right = right;
            Value = -1;

            return true;
          }
          return false;
        }

        var changed = Left!.Split();
        return !changed ? Right!.Split() : true;
      }

      public int Magnitude()
      {
        if (Value != -1)
        {
          return Value;
        }

        return (Left!.Magnitude() * 3) + (Right!.Magnitude() * 2);
      }

      private SnailNumber? NextLeft()
      {
        var current = this;
        if (current.Parent == null)
        {
          return null;
        }

        while (current.Parent!.Left == current)
        {
          current = current.Parent;
          if (current.Parent == null)
          {
            return null;
          }
        }

        current = current.Parent.Left;

        while (current!.Right != null)
        {
          current = current.Right;
        }

        return current;
      }

      private SnailNumber? NextRight()
      {
        var current = this;
        if (current.Parent == null)
        {
          return null;
        }

        while (current.Parent.Right == current)
        {
          current = current.Parent;
          if (current.Parent == null)
          {
            return null;
          }
        }

        current = current.Parent.Right;

        while (current!.Left != null)
        {
          current = current.Left;
        }

        return current;
      }

      private static (SnailNumber, int) ParseWithNumber(string str)
      {
        if (str[0] == '[')
        {
          var (left, leftRead) = ParseWithNumber(str.Substring(1));
          var (right, rightRead) = ParseWithNumber(str.Substring(leftRead + 1));
          var parent = new SnailNumber(null, left, right);
          parent.Left!.Parent = parent;
          parent.Right!.Parent = parent;
          return (parent, leftRead + rightRead + 2);
        }
        else
        {
          var indexOfComma = str.IndexOf(',');
          var indexOfBracket = str.IndexOf(']');
          var index = indexOfComma == -1 ? indexOfBracket : Math.Min(indexOfComma, indexOfBracket);
          return (new SnailNumber(int.Parse(str.Substring(0, index))), index + 1);
        }
      }

      public static SnailNumber Parse(string str)
      {
        var (number, _) = ParseWithNumber(str);

        return number;
      }
      
      public override string ToString()
      {
        if (Left == null && Right == null)
        {
          return Value.ToString();
        }
        else
        {
          return $"[{Left},{Right}]";
        }
      }

    }
  }
}