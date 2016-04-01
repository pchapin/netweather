---------------------------------------------------------------------------
-- FILE          : database.adb
-- LAST REVISION : 2008-07-27
-- SUBJECT       : NetWeather database package
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

with APQ;
with APQ.PostgreSQL.Client;
with Support;

use Ada.Calendar;
use Ada.Calendar.Formatting;
use Ada.Text_IO;
use Ada.Integer_Text_IO;
use APQ.PostgreSQL.Client;

package body Database is

   package Speed_IO is new Ada.Text_IO.Float_IO(Speed_Type);
   use Speed_IO;

   package Direction_IO is new Ada.Text_IO.Float_IO(Direction_Type);
   use Direction_IO;

   package Duration_IO is new Ada.Text_IO.Fixed_IO(Duration);
   use Duration_IO;

   -- Types
   type Speed_Results_Pointer is access Speed_Results;
   type Direction_Results_Pointer is access Direction_Results;

   -- Subprograms
   procedure Deallocate is new
     Ada.Unchecked_Deallocation(Speed_Results, Speed_Results_Pointer);
   procedure Deallocate is new
     Ada.Unchecked_Deallocation(Direction_Results, Direction_Results_Pointer);


   function Atoi(Letter: Character) return Integer is
      Value : Integer;
   begin
      Value := Character'Pos(Letter)- Character'Pos('0');
   return Value;
   end Atoi;


   procedure Do_Tests is
   begin
      raise Support.Test_Failure;
   end Do_Tests;


   -- Internal task for synchronizing all access to the database. Note that the database
   -- itself might be able to handle concurrent access (for example by other programs),
   -- but this program needs to protect its own internal database access structures from
   -- corruption by its own tasks.
   --
   task Database_Handler is
      entry Add_Wind_Speed(Speed : Speed_Type; Date : Ada.Calendar.Time);
      entry Add_Wind_Direction(Direction : Direction_Type; Date : Ada.Calendar.Time);
      entry Get_Wind_Speed
        (Start_Date     : Ada.Calendar.Time;
         End_Date       : Ada.Calendar.Time;
         Result_Pointer : out Speed_Results_Pointer);
      entry Get_Wind_Direction
        (Start_Date     : Ada.Calendar.Time;
         End_Date       : Ada.Calendar.Time;
         Result_Pointer : out Direction_Results_Pointer);
      entry Get_First_Date(First_Date : out Ada.Calendar.Time);
      entry Get_Last_Date(Last_Date : out Ada.Calendar.Time);
      entry Do_Shutdown;
   end Database_Handler;


   task body Database_Handler is
      Wind_Speed : Speed_Type;
      Speed_Date : Ada.Calendar.Time;

      Wind_Direction : Direction_Type;
      Direction_Date : Ada.Calendar.Time;

      C : Connection_Type;
      Q : Query_Type;

   begin -- Database Handler
      Set_Host_Name(C, "localhost");                -- Host name where database is located.
      Set_User_Password(C,"postgres", "frenchfry"); -- Userid and password.
      Set_DB_Name(C, "postgres");                   -- Database name
      Connect(C);

      loop
         select
            accept Add_Wind_Speed(Speed: Speed_Type; Date : Ada.Calendar.Time) do
               Wind_Speed := Speed;
               Speed_Date := Date;
            end Add_Wind_Speed;

            Prepare(Q,"INSERT INTO WindSpeed values(");
            Append_Line(Q, Image(Speed_Date));
            Append_Line(Q, Speed_Type'Image(Wind_Speed));
            Append_Line(Q,")");
            Execute_Checked(Q,C);
         or
            accept Add_Wind_Direction(Direction : Direction_Type; Date : Ada.Calendar.Time) do
               Wind_Direction := Direction;
               Direction_Date := Date;
            end Add_Wind_Direction;

            Prepare(Q,"INSERT INTO WindDirection values(");
            Append_Line(Q, Image(Direction_Date));
            Append_Line(Q, Direction_Type'Image(Wind_Direction));
            Append_Line(Q,")");
            Execute_Checked(Q,C);
         or
            accept Get_Wind_Speed
              (Start_Date     : in  Ada.Calendar.Time;
               End_Date       : in  Ada.Calendar.Time;
               Result_Pointer : out Speed_Results_Pointer) do

               Prepare(Q,"SELECT date, speed from WindSpeed where date >=");
               Append_Line(Q, Image(Start_Date));
               Append_Line(Q, "AND date <=");
               Append_Line(Q, Image(End_Date));
               Append_Line(Q, ")");
               Execute_Checked(Q,C);

               -- Bug? What if Tuples() returns zero?
               Result_Pointer := new Speed_Results(1 .. Positive(Tuples(Q)));
               for I in Result_Pointer'Range  loop
                  Fetch(Q);
                  declare
                     Date_Text  : String := Value(Q,1);
                     Speed_Text : String := Value(Q,2);

                     -- There's got to be a better way to do this. (Also see below).
                     Year : Integer :=
                       (Atoi(Date_Text(1)) * 1000) +
                       (Atoi(Date_Text(2)) *  100) +
                       (Atoi(Date_Text(3)) *   10) +
                       (Atoi(Date_Text(4)));

                     Month : Integer :=
                       (Atoi(Date_Text(6)) *   10) +
                       (Atoi(Date_Text(7)));

                     Day : Integer :=
                       (Atoi(Date_Text(9)) *   10) +
                       (Atoi(Date_Text(10)));

                     Seconds : Integer :=
                       (3600 * ((Atoi(Date_Text(12)) * 10) + (Atoi(Date_Text(13))))) +
                       (  60 * ((Atoi(Date_Text(15)) * 10) + (Atoi(Date_Text(16))))) +
                               ((Atoi(Date_Text(18)) * 10) + (Atoi(Date_Text(19))));
                  begin
                     Result_Pointer(I).The_Date :=
                       Ada.Calendar.Time_Of(Year, Month, Day, Day_Duration(Seconds));

                     Result_Pointer(I).Speed := Speed_Type'Value(Speed_Text);
                  end;
               end loop;
            end Get_Wind_Speed;
         or
            accept Get_Wind_Direction
              (Start_Date     : in  Ada.Calendar.Time;
               End_Date       : in  Ada.Calendar.Time;
               Result_Pointer : out Direction_Results_Pointer) do

               Prepare(Q,"SELECT date, speed from WindDirection where date >=");
               Append_Line(Q, Image(Start_Date));
               Append_Line(Q, "AND date <=");
               Append_Line(Q, Image(End_Date));
               Append_Line(Q, ")");
               Execute_Checked(Q,C);

               -- Bug? What if Tuples() returns zero?
               Result_Pointer := new Direction_Results(1 .. Positive(Tuples(Q)));
               for I in Result_Pointer'Range loop
                  Fetch(Q);
                  declare
                     Date_Text      : String := Value(Q, 1);
                     Direction_Text : String := Value(Q, 2);

                     Year : Integer :=
                       (Atoi(Date_Text(1)) * 1000) +
                       (Atoi(Date_Text(2)) *  100) +
                       (Atoi(Date_Text(3)) *   10) +
                       (Atoi(Date_Text(4)));

                     Month : Integer :=
                       (Atoi(Date_Text(6)) *   10) +
                       (Atoi(Date_Text(7)));

                     Day : Integer :=
                       (Atoi(Date_Text(9)) *   10) +
                       (Atoi(Date_Text(10)));

                     Seconds : Integer :=
                       (3600 * ((Atoi(Date_Text(12)) * 10) + (Atoi(Date_Text(13))))) +
                       (  60 * ((Atoi(Date_Text(15)) * 10) + (Atoi(Date_Text(16))))) +
                               ((Atoi(Date_Text(18)) * 10) + (Atoi(Date_Text(19))));
                  begin
                     Result_Pointer(I).The_Date :=
                       Ada.Calendar.Time_Of(Year, Month, Day, Day_Duration(Seconds));

                     Result_Pointer(I).Direction := Direction_Type'Value(Direction_Text);
                  end;
               end loop;
            end Get_Wind_Direction;
         or
            accept Do_Shutdown;
            exit;
         end select;
      end loop;

   exception
      when APQ.Not_Connected =>
         null;
   end Database_Handler;


   -- The visible procedures just call entries in the internal task.

   procedure Add_Wind_Speed(Speed : Speed_Type; Date : Ada.Calendar.Time) is
   begin
      Database_Handler.Add_Wind_Speed(Speed, Date);
   end Add_Wind_Speed;


   procedure Add_Wind_Direction(Direction : Direction_Type; Date : Ada.Calendar.Time) is
   begin
      Database_Handler.Add_Wind_Direction(Direction, Date);
   end Add_Wind_Direction;


   function Get_Wind_Speed
     (Start_Date, End_Date : Ada.Calendar.Time) return Speed_Results is

      Result_Pointer: Speed_Results_Pointer;
   begin
      Database_Handler.Get_Wind_Speed(Start_Date, End_Date, Result_Pointer);
      declare
         Result : Speed_Results(1 .. Result_Pointer'Last);
      begin
         Result := Result_Pointer.all;
         Deallocate(Result_Pointer);
         return Result;
      end;
   end Get_Wind_Speed;


   function Get_Wind_Direction
     (Start_Date, End_Date : Ada.Calendar.Time) return Direction_Results is

      Result_Pointer : Direction_Results_Pointer;
   begin
      Database_Handler.Get_Wind_Direction(Start_Date, End_Date, Result_Pointer);
      declare
         Result: Direction_Results(1..Result_Pointer'last);

      begin
         Result:= Result_Pointer.all;
         Deallocate(Result_Pointer);
         return Result;
      end;
   end Get_Wind_Direction;


   function Get_First_Date return Ada.Calendar.Time is
      First_Date : Ada.Calendar.Time;
   begin
      Database_Handler.Get_First_Date(First_Date);
      return First_Date;
   end Get_First_Date;


   function Get_Last_Date return Ada.Calendar.Time is
      Last_Date : Ada.Calendar.Time;
   begin
      Database_Handler.Get_Last_Date(Last_Date);
      return Last_Date;
   end Get_Last_Date;


   procedure Do_Shutdown is
   begin
      Database_Handler.Do_Shutdown;
   end Do_Shutdown;

end Database;
