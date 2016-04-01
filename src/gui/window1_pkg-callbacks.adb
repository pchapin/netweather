with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Ada.Text_IO;

package body Window1_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------------------
   -- On_Hardwarebutton_Released --
   --------------------------------

   procedure On_Hardwarebutton_Released
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      -- Clean up Old Hardware Connections Here!

      Ada.Text_IO.Put_Line("Reconnecting To Hardware...");
      --Gtk.Label.Set_Text(Object.Hardware_Status_Label, "Reconnecting To Hardware...");

   end On_Hardwarebutton_Released;

   --------------------------
   -- On_Startweb_Released --
   --------------------------

   procedure On_Startweb_Released
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      -- Check for old Web Interfaces!
      Ada.Text_IO.Put_Line("Starting Web Interface...");
   end On_Startweb_Released;

   -------------------------
   -- On_Stopweb_Released --
   -------------------------

   procedure On_Stopweb_Released
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Ada.Text_IO.Put_Line("Stopping Web Interface...");
   end On_Stopweb_Released;

   ---------------------------------
   -- On_Databaseconnect_Released --
   ---------------------------------

   procedure On_Databaseconnect_Released
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      -- Check for existing Connections
      Ada.Text_IO.Put_Line("Connecting To Database...");
   end On_Databaseconnect_Released;

   ------------------------------------
   -- On_Databasedisconnect_Released --
   ------------------------------------

   procedure On_Databasedisconnect_Released
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Ada.Text_IO.Put_Line("Disconnecting From Database...");
   end On_Databasedisconnect_Released;

end Window1_Pkg.Callbacks;
