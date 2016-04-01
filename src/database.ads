---------------------------------------------------------------------------
-- FILE          : database.ads
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
with Ada.Calendar;
with Support.Types;

use Support.Types;

package Database is

   procedure Do_Tests;

   procedure Add_Wind_Speed
     (Speed : Speed_Type; Date : Ada.Calendar.Time);

   procedure Add_Wind_Direction
     (Direction : Direction_Type; Date : Ada.Calendar.Time);

   function Get_Wind_Speed
     (Start_Date, End_Date : Ada.Calendar.Time) return Speed_Results;

   function Get_Wind_Direction
     (Start_Date, End_Date : Ada.Calendar.Time) return Direction_Results;

   function Get_First_Date return Ada.Calendar.Time;
   function Get_Last_Date return Ada.Calendar.Time;

   procedure Do_Shutdown;

end Database;

