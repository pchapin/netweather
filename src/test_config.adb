---------------------------------------------------------------------------
-- FILE          : test_config.adb
-- LAST REVISION : 2008-07-27
-- SUBJECT       : Test program that exercises the configuration file handling.
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
with Support.Config;
with Support.Types;
with Database;

use Ada.Text_IO;
use Support.Config;
use Support.Types;

procedure Test_Config is
   package Speed_IO is new Ada.Text_IO.Float_IO(Speed_Type);
   use Speed_IO;

   package Direction_IO is new Ada.Text_IO.Float_IO(Direction_Type);
   use Direction_IO;

   Calibration_Speed     : Speed_Type     := 0.0;
   Calibration_Direction : Direction_Type := 0.0;
begin
   Calibration_Speed := Speed_Calibration_Factor;
   Put("Speed Calibration Factor     = ");
   Put(Calibration_Speed);
   New_Line;
   Put("Direction Calibration Factor = ");
   Put(Calibration_Direction);
   New_Line;
end Test_Config;
