---------------------------------------------------------------------------
-- FILE          : serial_port.ads
-- LAST REVISION : 2008-02-23
-- SUBJECT       : Specification of package for simple serial port access.
--
-- This is intended to be a very easy to use package for reading from and
-- writing to the serial port. Only one port can be open at a time so the
-- user of this package does not need to track a serial port "object" or
-- handle of any sort. Time out and flow control settings are hard coded
-- so the user of this package does not need to worry about configuring
-- them.
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Interfaces;

package Serial_Port is

   -- Type defining possible ports
   type Port_Type is (COM1, COM2);

   -- Types for configuring serial parameters.
   type Baud_Type      is (B2400, B4800, B9600, B19200);
   type Parity_Type    is (None, Even, Odd);
   type Data_Size_Type is (Seven, Eight);
   type Stop_Type      is (One, Two);

   Open_Failure : exception;
   IO_Failure   : exception;


   -- Raises Open_Failure if a port is already open or if the underlying open fails.
   procedure Open
      (Port      : in Port_Type;
       Baud      : in Baud_Type;
       Parity    : in Parity_Type;
       Data_Size : in Data_Size_Type;
       Stop      : in Stop_Type);

   -- Raises IO_Failure if no port is open or if the underlying I/O fails.
   function Read return Interfaces.Unsigned_8;
   procedure Write(Byte : Interfaces.Unsigned_8);

   -- Has no effect if no port is open.
   procedure Close;
end Serial_Port;
