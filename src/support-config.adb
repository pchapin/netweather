---------------------------------------------------------------------------
-- FILE          : support-config.adb
-- LAST REVISION : 2008-07-27
-- SUBJECT       : Configuration file support.
--
-- This package reads the NetWeather configuration file. It is very simplistic at the moment
-- and does not allow for any variation in configuration file format. This package should allow
-- a more general syntax in the configuration file.
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Ada.Text_IO;
with Support.Types;

use Ada.Text_IO;
use Support.Types;

pragma Elaborate_All(Ada.Text_IO);

package body Support.Config is

   package Speed_IO is new Ada.Text_IO.Float_IO(Speed_Type);
   use Speed_IO;

   Calibration_Speed     : Speed_Type     := 1.0;
   Calibration_Direction : Direction_Type := 1.0;

   -- Read the configuration file. This procedure needs major elaboration.
   procedure Get_File is
      File : File_Type;
      Item : String(1..5);
   begin
      Open(File, In_File, "Config.txt");
      while not End_Of_File(File) loop
         Get(File, Item);
         if Item /= "Speed" then
            Skip_Line(File);
         else
            Get(File, Calibration_Speed);
            Skip_Line(File);
         end if;
      end loop;
      Close(File);
   end Get_File;


   function Speed_Calibration_Factor return Speed_Type is
   begin
      return Calibration_Speed;
   end Speed_Calibration_Factor;


   function Direction_Calibration_Factor return Direction_Type is
   begin
      return Calibration_Direction;
   end Direction_Calibration_Factor;


-- Elaboration of this package causes the configuration file to be read.
begin
   Get_File;
exception
   when others =>
      Ada.Text_IO.Put_Line("Unhandled exception while initializing Support.Config");
end Support.Config;
