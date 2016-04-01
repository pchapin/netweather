---------------------------------------------------------------------------
-- FILE          : data_source.ads
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

package Data_Source is

   procedure Do_Tests;

   -- Provides fake data for testing.
   task Generate_Mock_Data is
      entry Do_Startup;    -- Invoke before calling any other entries (except Do_Shutdown).
      entry Do_Shutdown;   -- Must call this to terminate task... even without Do_Startup.
      entry Connect;       -- Start generating mock data.
      entry Disconnect;    -- Stop generating mock data.
   end Generate_Mock_Data;

   -- Provides data from real hardware.
   task Generate_Data is
      entry Do_Startup;    -- Invoke before calling any other entires (except Do_Shutdown).
      entry Do_Shutdown;   -- Must call this to terminate task... even without Do_Startup.
      entry Connect;       -- Start reading data from the serial port.
      entry Disconnect;    -- Stop reading data from the serial port.
   end Generate_Data;
   -- Note that Do_Startup opens the serial port and Do_Shutdown closes it (if it has been
   -- opened). Once Connect has been called, the task will not respond to any other control
   -- entries while it is trying to read a value from the hardware. This should be fixed.

end Data_Source;
