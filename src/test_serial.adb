---------------------------------------------------------------------------
-- FILE          : data_source.adb
-- LAST REVISION : 2008-07-27
-- SUBJECT       : Test program that exercises the serial port.
--
-- This test program is intended to be used with a terminal program. It is currently very
-- minimal and should be extended.
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Ada.Integer_Text_IO;
with Serial_Port;

use Ada.Integer_Text_IO;
use Serial_Port;

procedure test_serial is
   Num : Integer;

begin
   -- Open port using the same parameters as in the main application.
   Open(COM1, B4800, None, Seven, One);

   -- Write the ASCII codes of a message. Verify on a terminal program.
   Write(16#40#);
   Write(16#41#);
   Write(13);     -- CR
   Write(10);     -- LF

   -- Read and display a value.
   Num := Integer(Read);
   Put(Num);
   Close;
end test_serial;
