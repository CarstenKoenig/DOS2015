using System;
using System.Collections.Generic;
using System.Linq;

using ZahlenListe = System.Collections.Generic.IEnumerable<int>;

namespace CountdownCS
{
    public enum Operator
    {
        Add,
        Sub,
        Mul,
        Div
    }

    public static class Operators
    {
        public static bool IsValid (this Operator op, int left, int right)
        {
            switch (op)
            {
                case Operator.Add:
                    return true;
                case Operator.Sub:
                    return left > right;
                case Operator.Mul:
                    return true;
                case Operator.Div:
                    return left % right == 0;
                default:
                    return false;
            }
        }

        public static int Apply(this Operator op, int left, int right)
        {
            switch (op)
            {
                case Operator.Add:
                    return left+right;
                case Operator.Sub:
                    return left-right;
                case Operator.Mul:
                    return left*right;
                case Operator.Div:
                    return left/right;
                default:
                    throw new ArgumentOutOfRangeException("op");
            }
        }

        public static string Zeichen(this Operator op)
        {
            switch (op)
            {
                case Operator.Add:
                    return "+";
                case Operator.Sub:
                    return "-";
                case Operator.Mul:
                    return "*";
                case Operator.Div:
                    return "/";
                default:
                    throw new ArgumentOutOfRangeException("op");
            }
        }
    }

    abstract class Expression
    {
        public abstract ZahlenListe Eval();

        public static IEnumerable<Expression> Lösungen(ZahlenListe zahlen, int zielZahl)
        {
            return
                from ns in zahlen.Subbags()
                from e in Expressions(ns)
                where e.Eval().FirstOrDefault() == zielZahl
                select e;
        }

        public static IEnumerable<Expression> Expressions(ZahlenListe zahlen)
        {
            if (!zahlen.Any())
                return new Expression[] { };

            if (zahlen.Count() == 1)
                return new[] { new Value(zahlen.First()) };

            return
                from t in zahlen.NichtLeereSplits()
                from l in Expressions(t.Item1)
                from r in Expressions(t.Item2)
                from op in Operatoren
                select new Apply(op, l, r);

        }

        static IEnumerable<Operator> Operatoren = new[] { Operator.Add, Operator.Sub, Operator.Mul, Operator.Div };
    }

    sealed class Value : Expression
    {
        readonly int _value;
        public Value(int value)
        {
            _value = value;
        }

        public override ZahlenListe Eval()
        {
            return _value > 0 ? new[] { _value } : new int[] { };
        }

        public override string ToString()
        {
            return _value.ToString();
        }
    }

    sealed class Apply : Expression
    {
        readonly Expression _left;
        readonly Expression _right;
        readonly Operator _operator;

        public Apply(Operator op, Expression left, Expression right)
        {
            _left = left;
            _right = right;
            _operator = op;
        }

        public override ZahlenListe Eval()
        {
                return
                    from left in _left.Eval()
                    from right in _right.Eval()
                    where _operator.IsValid(left, right)
                    select _operator.Apply(left, right);
        }

        public override string ToString()
        {
            return string.Format("({0}{1}{2})", _left.ToString(), _operator.Zeichen(), _right.ToString());
        }
    }

    public static class Listen
    {
        static ZahlenListe leereListe = new int[] { };

        public static IEnumerable<Tuple<ZahlenListe, ZahlenListe>> NichtLeereSplits(this ZahlenListe list)
        {
            return list
                .Splits()
                .Where(t => t.Item1.Any() && t.Item2.Any());
        }

        public static IEnumerable<Tuple<ZahlenListe, ZahlenListe>> Splits(this ZahlenListe list)
        {
            if (!list.Any())
                return new[] { Tuple.Create(leereListe,leereListe) };

            var x = list.First();
            var xs = list.Skip(1);

            var kopf = Tuple.Create(leereListe, list);
            var rest =
                from t in xs.Splits()
                select Tuple.Create(Cons(x, t.Item1), t.Item2);
            return Cons(kopf, rest);
                

        }

        public static IEnumerable<ZahlenListe> Subbags(this ZahlenListe list)
        {
            return
                from ys in list.Sublists()
                from zs in ys.Permutationen()
                select zs;
        }

        public static IEnumerable<ZahlenListe> Sublists(this ZahlenListe list)
        {
            if (!list.Any())
                yield return leereListe;
            else
            {
                var x = list.First();
                var xs = list.Skip(1);
                foreach (var ys in xs.Sublists())
                    yield return Cons(x, ys);
                foreach (var ys in xs.Sublists())
                    yield return ys;
            }
        }

        public static IEnumerable<ZahlenListe> Permutationen(this ZahlenListe list)
        {
            if (!list.Any())
                return new[] { leereListe };
            return
                from t in list.Picks()
                from ys in t.Item2.Permutationen()
                select Cons(t.Item1, ys);
        }

        public static IEnumerable<Tuple<int, ZahlenListe>> Picks(this ZahlenListe list)
        {
            if (!list.Any())
                yield break;
            else
            {
                var x = list.First();
                var xs = list.Skip(1);
                yield return Tuple.Create(x, xs);
                foreach(var t in xs.Picks())
                    yield return Tuple.Create(t.Item1, Cons(x, t.Item2));
            }
        }

        static IEnumerable<T> Cons<T>(T neuerKoppf, IEnumerable<T> liste)
        {
            yield return neuerKoppf;
            foreach (var x in liste)
                yield return x;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            foreach (var lsg in Expression.Lösungen(new[] { 1, 3, 7, 10, 25, 50 }, 765))
                Console.WriteLine(lsg);

        }
    }

}
