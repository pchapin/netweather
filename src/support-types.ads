---------------------------------------------------------------------------
-- FILE          : support-types.ads
-- LAST REVISION : 2008-03-22
-- SUBJECT       : Types needed by various parts of the NetWeather program.
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

package Support.Types is
   type Speed_Type     is digits 4 range 0.0 .. 100.0;  -- meters/sec
   type Direction_Type is digits 3 range 0.0 .. 2.0 * Ada.Numerics.Pi; -- radians from north.

   type Speed_Information is
      record
         The_Date : Ada.Calendar.Time;
         Speed    : Speed_Type;
      end record;

   type Direction_Information is
      record
         The_Date  : Ada.Calendar.Time;
         Direction : Direction_Type;
      end record;

   type Speed_Results     is array(Positive range <>) of Speed_Information;
   type Direction_Results is array(Positive range <>) of Direction_Information;
end Support.Types;
