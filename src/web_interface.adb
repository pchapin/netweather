---------------------------------------------------------------------------
-- FILE          : web_interface.adb
-- LAST REVISION : 2008-02-23
-- SUBJECT       : NetWeather web interface package.
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Support;
with Database; use Database;

with Ada.Text_IO;

with Ada.Calendar.Formatting;

with AWS;
with AWS.Config;
with AWS.Response;
with AWS.Server;
with AWS.Status;

with Templates_Parser;


package body Web_Interface is



   procedure Start_NW is
      NW : AWS.Server.HTTP;



   begin


      AWS.Server.Start(NW, "NW_Webserver", NW_Data'Unrestricted_Access);


   end Start_NW;



   function NW_Data(NW_Status : AWS.Status.Data) return AWS.Response.Data is

        use type Templates_Parser.Vector_Tag;

        Times : Templates_Parser.Vector_Tag := +"Time";
        Speeds  : Templates_Parser.Vector_Tag := +"Speed";
        Directions : Templates_Parser.Vector_Tag := +"Direction";

        Translations : Templates_Parser.Translate_Table:= Templates_Parser.No_Translation;

        WS_Result : Speed_Results := Get_Wind_Speed(Get_First_Date, Get_Last_Date);
        WD_Result : Direction_Results := Get_Wind_Direction(Get_First_Date, Get_Last_Date);
   begin

      for I in WS_Result'Range loop
        	 Times      := Templates_Parser."&"(Ada.Calendar.Formatting.Image(WS_Result(I).The_Date), Times);
                 Speeds     := Templates_Parser."&"(Speed_Type'Image(WS_Result(I).Speed), Speeds);
                 Directions := Templates_Parser."&"(Direction_Type'Image(WD_Result(I).Direction), Directions);


        end loop;

        Translations := (1 => Templates_Parser.Assoc ("TIME", Times),
                         2 => Templates_Parser.Assoc ("SPEED", Speeds),
                         3 => Templates_Parser.Assoc ("DIRECTION", Directions));



        return AWS.Response.Build("HTML", Message_Body => Templates_Parser.Parse ("NW.tmplt", Translations));
   end NW_Data;



   procedure Do_Tests is
   begin
      raise Support.Test_Failure;
   end Do_Tests;

end Web_Interface;

