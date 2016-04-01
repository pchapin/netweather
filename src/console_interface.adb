---------------------------------------------------------------------------
-- FILE          : console_interface.adb
-- LAST REVISION : 2008-03-29
-- SUBJECT       : NetWeather console interface package.
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
with Ada.Characters.Handling;

with Database;
with Data_Source;
with Support;

package body Console_Interface is

   procedure Do_Tests is
   begin
      raise Support.Test_Failure;
   end Do_Tests;


   procedure Main_Loop is
      Menu_Selection  : Character;
      Not_Done        : Boolean := True;

      function Get_Menu_Selection return Character is
         Selected_Option : Character;
      begin
         Ada.Text_IO.Put_Line("VTC Netweather");
         Ada.Text_IO.Put_Line("");
         Ada.Text_IO.Put_Line(" 1) Mock data source");
         Ada.Text_IO.Put_Line(" 2) Real data source");
         Ada.Text_IO.Put_Line(" q) Quit");
         Ada.Text_IO.Put_Line("");
         Ada.Text_IO.Put("=> ");
         Ada.Text_IO.Get(Selected_Option);
         Ada.Text_IO.Skip_Line;
         return Selected_Option;
      end Get_Menu_Selection;

   begin  -- Main_Loop
      Data_Source.Generate_Mock_Data.Do_Startup;
      Data_Source.Generate_Data.Do_Startup;

      while Not_Done loop
         Menu_Selection := Ada.Characters.Handling.To_Lower(Get_Menu_Selection);

         case Menu_Selection is
            when '1' =>
               Data_Source.Generate_Data.Disconnect;
               Data_Source.Generate_Mock_Data.Connect;

            when '2' =>
               Data_Source.Generate_Mock_Data.Disconnect;
               Data_Source.Generate_Data.Connect;

            when 'q' =>
               Not_Done := False;

            when others =>
               Ada.Text_IO.Put_Line("Invalid Entry");
         end case;
      end loop;

      Data_Source.Generate_Mock_Data.Disconnect;
      Data_Source.Generate_Data.Disconnect;
      Data_Source.Generate_Mock_Data.Do_Shutdown;
      Data_Source.Generate_Data.Do_Shutdown;
      Database.Do_Shutdown;
   end Main_Loop;

end Console_Interface;
