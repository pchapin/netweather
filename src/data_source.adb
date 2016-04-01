---------------------------------------------------------------------------
-- FILE          : data_source.adb
-- LAST REVISION : 2008-07-27
-- SUBJECT       : Package that listens to the weather station hardware.
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
with Ada.Numerics;
with Ada.Numerics.Float_Random;
with Interfaces;

with Database;
with Support;
with Support.Config;
with Support.Types;
with Serial_Port;

use Ada.Numerics.Float_Random;
use Database;
use Support.Config;
use Support.Types;

package body Data_Source is
   use type Interfaces.Unsigned_8;

   procedure Do_Tests is
   begin
      raise Support.Test_Failure;
   end Do_Tests;

   -- A simple task that generates mock data periodically.
   task body Generate_Mock_Data is
      Shutdown_Now   : Boolean := False;
      Connect_Flag   : Boolean := False;
      Today          : Ada.Calendar.Time;
      Rand           : Generator;
      Result         : Uniformly_Distributed;
      Base_Speed     : Speed_Type     := 10.0;
      Speed          : Speed_Type     := 0.0;
      Base_Direction : Direction_Type := (2.0 * Ada.Numerics.Pi) / 3.0;
      Direction      : Direction_Type := 0.0;
   begin
      select
         accept Do_Startup do
            Shutdown_Now := False;
         end Do_Startup;
      or
         accept Do_Shutdown do
            Shutdown_Now := True;
         end Do_Shutdown;
      end select;

      while not Shutdown_Now loop
         select
            accept Connect do
               Connect_Flag := True;
            end Connect;
         or
            accept Disconnect do
               Connect_Flag := False;
            end Disconnect;
         or
            accept Do_Shutdown do
               Connect_Flag := False;
               Shutdown_Now := True;
            end Do_Shutdown;
         or
            when Connect_Flag =>
               delay 10.0;
               Result    := Random(Rand);
               Speed     := Speed_Type((5.0 * Result - 2.5) + Float(Base_Speed));
               Result    := Random(Rand);
               Direction := Direction_Type((0.33 * Result - 0.165) + Float(Base_Direction));
               Today     := Ada.Calendar.Clock;
               Database.Add_Wind_Speed(Speed, Today);
               Database.Add_Wind_Direction(Direction, Today);
         end select;
      end loop;
   end Generate_Mock_Data;


   task body Generate_Data is
      Shutdown_Now   : Boolean := False;
      Connect_Flag   : Boolean := False;
      Today          : Ada.Calendar.Time;
      Speed          : Speed_Type := 0.0;
      Direction      : Direction_Type := 0.0;
      Tag            : Interfaces.Unsigned_8;
      Op_Code        : Interfaces.Unsigned_8;
   begin
      select
         accept Do_Startup do
            Serial_Port.Open
              (Serial_Port.COM1,
               Serial_Port.B9600,
               Serial_Port.None,
               Serial_Port.Eight,
               Serial_Port.One);
            Shutdown_Now := False;
         end Do_Startup;
      or
         accept Do_Shutdown do
            Shutdown_Now := True;
         end Do_Shutdown;
      end select;

      while not Shutdown_Now loop
         select
	    accept Connect do
               Connect_Flag := True;
            end Connect;
         or
            accept Disconnect do
               Connect_Flag := False;
            end Disconnect;
         or
            accept Do_Shutdown do
               Serial_Port.Close;
               Connect_Flag := False;
               Shutdown_Now := True;
            end Do_Shutdown;
         or
            when Connect_Flag =>
               delay 0.0;

               -- The task won't accept any more control entries until it has read a value.
               loop
                  Tag := Serial_Port.Read;
                  exit when Tag = 16#FF#;
               end loop;
               loop
                  Op_Code := Serial_Port.Read;
                  case Op_Code is
                     when 16#01# =>
                        Speed :=
                          Speed_Calibration_Factor * Speed_Type(Serial_Port.Read);

                     when 16#02# =>
                        Direction :=
                          Direction_Calibration_Factor * Direction_Type(Serial_Port.Read);

                     when 16#FF# =>
                        -- Received end marker. Assume we've received both speed and direction.
                        Today := Ada.Calendar.Clock;
                        Add_Wind_Speed(Speed, Today);
                        Add_Wind_Direction(Direction, Today);
                        exit;

                     when others =>
                        -- Received unknown op code.
                        Op_Code := Serial_Port.Read;   -- Read associated value and discard.
                        Op_Code := Serial_Port.Read;   -- Get next op code.
                  end case;
               end loop;
         end select;
     end loop;
   end Generate_Data;

end Data_Source;
