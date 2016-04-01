---------------------------------------------------------------------------
-- FILE          : web_interface.ads
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
with AWS;
with AWS.Status;
with AWS.Response;

package Web_Interface is

   procedure Start_NW;
   function NW_Data(NW_Status : AWS.Status.Data) return AWS.Response.Data;

   procedure Do_Tests;

end Web_Interface;

