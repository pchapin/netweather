---------------------------------------------------------------------------
-- FILE          : support-config.adb
-- LAST REVISION : 2008-07-27
-- SUBJECT       : Configuration file support.
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Support.Types;

package Support.Config is

   -- The speed calibration factor is a multiplier that converts the raw 8 bit integers read
   -- from the hardware into actual speed values in meters/sec. It is a configurable item
   -- because its value will depend on the operation of the hardware.
   --
   function Speed_Calibration_Factor return Types.Speed_Type;

   -- This calibration factor plays the same role as the speed calibration factor except for
   -- wind direction.
   function Direction_Calibration_Factor return Types.Direction_Type;

end Support.Config;
