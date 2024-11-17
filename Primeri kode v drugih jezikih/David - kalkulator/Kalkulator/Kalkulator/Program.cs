using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Kalkulator
{
    class Program
    {
        static void Main(string[] args)
        {
            int ooo = 0;
            while (ooo !=5)
            {  
                Console.WriteLine("Izberi računsko operacijo:");
                Console.WriteLine("1. seštevanje");
                double vsota = 0;
                Console.WriteLine("2. odštevanje");
                double odštevanje = 0;
                Console.WriteLine("3. množenje");
                double množenje = 1;
                Console.WriteLine("4. deljenje");
                double deljenje = 0;
                Console.WriteLine("5. se konča");
                double z = double.Parse(Console.ReadLine());
                double konec = 0;
                switch (z)
                {
                    case 1:
                        Sestevanje(vsota);
                        break;
                    case 2:
                        Odstevanje(odštevanje);
                        break;
                    case 3:
                        Mnozenje(množenje);
                        break;
                    case 4:
                        Deljenje(deljenje);
                        break;
                    case 5:
                        Console.WriteLine("Program se je končal.");
                        Environment.Exit(0);
                        break;
                    default:
                        Console.WriteLine("Konec");
                        ooo = 1;
                        break;
                }
            }
           Console.ReadKey();
        }

        static double Sestevanje(double vsota)
        {
            Console.Write("Koliko števil želiš sešteti: ");
            int št1 = int.Parse(Console.ReadLine());
            for (int i = 0; i < št1; i++)
            {
                Console.Write("Vpiši število: ");
                double x = double.Parse(Console.ReadLine());
                vsota = vsota + x;
            }
            Console.WriteLine("Vsota števil je: {0}.", vsota);
            return vsota;
        }

        static double Odstevanje(double odštevanje)
        {
            Console.Write("Koliko števil želiš odšteti: ");
            int št1 = int.Parse(Console.ReadLine());
            double x;
            bool st1 = false;
            for (int i = 1; i <= št1; i+=1)
            {
                if (st1 == false)
                {
                    Console.Write("Vpiši število: ");
                    x = double.Parse(Console.ReadLine());
                    odštevanje = x;
                    st1= true;
                }
                else
                {
                    Console.Write("Vpiši število: ");
                    x = double.Parse(Console.ReadLine());
                    odštevanje = odštevanje - x;
                }
            }
            Console.WriteLine("Razlika števil je: {0}.", odštevanje);
            return odštevanje;
        }

        static double Mnozenje(double množenje)
        {
            Console.Write("Koliko števil želiš zmnožiti: ");
            int št1 = int.Parse(Console.ReadLine());
            for (int i = 0; i < št1; i++)
            {
                Console.Write("Vpiši število: ");
                double x = double.Parse(Console.ReadLine());
                množenje =  množenje * x;
            }
            Console.WriteLine("Zmnožek števil je: " + množenje);
            return množenje;
        }

        static double Deljenje(double deljenje)
        {
            Console.Write("Koliko števil želiš deliti: ");
            int št1 = int.Parse(Console.ReadLine());
            double x;
            bool deljenje1 = false;
            for (int i = 1; i <= št1; i += 1)
            {
                if (deljenje1 == false)
                {
                    Console.Write("Vpiši število: ");
                    x = double.Parse(Console.ReadLine());
                    deljenje = x;
                    deljenje1 = true;
                }
                else
                {
                    Console.Write("Vpiši število: ");
                    x = double.Parse(Console.ReadLine());
                    deljenje = deljenje / x;
                }
            }
            Console.WriteLine("Razlika števil je: {0}.", deljenje);
            return deljenje;
        }
    }
}
