with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Button; use Gtk.Button;

package Callbacks_Layout is

   package Button_Callback is new
     Gtk.Handlers.Callback (Gtk_Button_Record);

end Callbacks_Layout;
