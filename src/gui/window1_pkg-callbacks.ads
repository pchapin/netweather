with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Window1_Pkg.Callbacks is
   procedure On_Hardwarebutton_Released
     (Object : access Gtk_Button_Record'Class);

   procedure On_Startweb_Released
     (Object : access Gtk_Button_Record'Class);

   procedure On_Stopweb_Released
     (Object : access Gtk_Button_Record'Class);

   procedure On_Databaseconnect_Released
     (Object : access Gtk_Button_Record'Class);

   procedure On_Databasedisconnect_Released
     (Object : access Gtk_Button_Record'Class);

end Window1_Pkg.Callbacks;
