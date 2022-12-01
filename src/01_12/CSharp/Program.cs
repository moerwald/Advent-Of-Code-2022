var caloriesPerElf =
 File.ReadAllText("./input.txt")
            .Split("\r\n\r\n")
            .Select(x => x.Split("\r\n").Select(int.Parse))
            .Select(x => x.Sum());

Console.WriteLine(caloriesPerElf.Max());
Console.WriteLine(caloriesPerElf.OrderDescending().Take(3).Sum());