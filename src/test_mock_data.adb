---------------------------------------------------------------------------
-- FILE          : test_mock_data.adb
-- LAST REVISION : 2008-07-27
-- SUBJECT       : Test program that exercises the mock data generator.
--
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Ada.Calendar;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

with Data_Source;
with Database;
with Support.Types;

use Ada.Calendar;
use Ada.Text_IO;
use Ada.Integer_Text_IO;
use Support.Types;

procedure Test_Mock_Data is
   package Speed_IO is new Ada.Text_IO.Float_IO(Speed_Type);
   use Speed_IO;

   package Direction_IO is new Ada.Text_IO.Float_IO(Direction_Type);
   use Direction_IO;

   package Duration_IO is new Ada.Text_IO.Fixed_IO(Duration);
   use Duration_IO;

begin
   Data_Source.Generate_Mock_Data.Do_Startup;
   Put_Line("Generating Mock Data (105 seconds)...");
   Data_Source.Generate_Mock_Data.Connect;
   delay 105.0;  -- Between 100.0 and 110.0. This should create exactly 10 mock data points.
   Data_Source.Generate_Mock_Data.Disconnect;
   Put_Line("Done!");
   New_Line;

   -- Display the results of the mock data generation.
   declare
      Wind_Speed_Data : Speed_Results :=
        Database.Get_Wind_Speed(Database.Get_First_Date, Database.Get_Last_Date);

      Wind_Direction_Data : Direction_Results :=
        Database.Get_Wind_Direction(Database.Get_First_Date, Database.Get_Last_Date);

   begin
      Put_Line("Wind Speed Data");
      Put_Line("===============");
      for I in Wind_Speed_Data'Range loop
         Put( Year   (Wind_Speed_Data(I).The_Date) );
         Put( Month  (Wind_Speed_Data(I).The_Date) );
         Put( Day    (Wind_Speed_Data(I).The_Date) );
         Put( Seconds(Wind_Speed_Data(I).The_Date) );
         Put( Wind_Speed_Data(I).Speed );
         New_Line;
      end loop;
      New_Line;

      Put_Line("Wind Direction Data");
      Put_Line("===================");
      for I in Wind_Direction_Data'Range loop
         Put( Year   (Wind_Direction_Data(I).The_Date) );
         Put( Month  (Wind_Direction_Data(I).The_Date) );
         Put( Day    (Wind_Direction_Data(I).The_Date) );
         Put( Seconds(Wind_Direction_Data(I).The_Date) );
         Put(Wind_Direction_Data(I).Direction);
         New_Line;
      end loop;
   end;

   Data_Source.Generate_Mock_Data.Do_Shutdown;
   Data_Source.Generate_Data.Do_Shutdown;
   Database.Do_Shutdown;
end Test_Mock_Data;
