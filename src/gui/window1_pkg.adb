with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Layout; use Callbacks_Layout;
with Layout_Intl; use Layout_Intl;
with Window1_Pkg.Callbacks; use Window1_Pkg.Callbacks;

package body Window1_Pkg is

procedure Gtk_New (Window1 : out Window1_Access) is
begin
   Window1 := new Window1_Record;
   Window1_Pkg.Initialize (Window1);
end Gtk_New;

procedure Initialize (Window1 : access Window1_Record'Class) is
   pragma Suppress (All_Checks);
   Pixmaps_Dir : constant String := "";
   Tooltips : Gtk_Tooltips;

begin
   Gtk.Window.Initialize (Window1, Window_Toplevel);
   Set_Title (Window1, -"window1");
   Set_Position (Window1, Win_Pos_None);
   Set_Modal (Window1, False);
   Set_Resizable (Window1, True);

   Gtk_New (Window1.Frame1);
   Set_Label_Align (Window1.Frame1, 0.15, 0.5);
   Set_Shadow_Type (Window1.Frame1, Shadow_Etched_In);

   Gtk_New (Window1.Notebook1);
   Set_Scrollable (Window1.Notebook1, False);
   Set_Show_Border (Window1.Notebook1, True);
   Set_Show_Tabs (Window1.Notebook1, True);
   Set_Tab_Pos (Window1.Notebook1, Pos_Top);

   Gtk_New_Vbox (Window1.Vbox1, False, 0);

   Gtk_New (Window1.Frame2);
   Set_Border_Width (Window1.Frame2, 3);
   Set_Label_Align (Window1.Frame2, 0.05, 0.5);
   Set_Shadow_Type (Window1.Frame2, Shadow_Etched_In);

   Gtk_New_Vbox (Window1.Vbox2, False, 0);

   Gtk_New (Window1.Hardware_Status_Label, -("Hardware Status Here"));
   Set_Alignment (Window1.Hardware_Status_Label, 0.5, 0.5);
   Set_Padding (Window1.Hardware_Status_Label, 0, 0);
   Set_Justify (Window1.Hardware_Status_Label, Justify_Left);
   Set_Line_Wrap (Window1.Hardware_Status_Label, False);
   Set_Selectable (Window1.Hardware_Status_Label, False);
   Set_Use_Markup (Window1.Hardware_Status_Label, False);
   Set_Use_Underline (Window1.Hardware_Status_Label, False);

   Pack_Start
     (Window1.Vbox2,
      Window1.Hardware_Status_Label,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Window1.Hardware_Button);
   Set_Border_Width (Window1.Hardware_Button, 10);
   Set_Relief (Window1.Hardware_Button, Relief_Normal);

   Gtk_New
     (Window1.Alignment1, 0.5, 0.5, 0.0,
      0.0);

   Gtk_New_Hbox (Window1.Hbox2, False, 2);

   Gtk_New (Window1.Image1 , "gtk-connect", Gtk_Icon_Size'Val (4));
   Set_Alignment (Window1.Image1, 0.5, 0.5);
   Set_Padding (Window1.Image1, 0, 0);

   Pack_Start
     (Window1.Hbox2,
      Window1.Image1,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Window1.Label8, -("Reconnect"));
   Set_Alignment (Window1.Label8, 0.5, 0.5);
   Set_Padding (Window1.Label8, 0, 0);
   Set_Justify (Window1.Label8, Justify_Left);
   Set_Line_Wrap (Window1.Label8, False);
   Set_Selectable (Window1.Label8, False);
   Set_Use_Markup (Window1.Label8, False);
   Set_Use_Underline (Window1.Label8, True);

   Pack_Start
     (Window1.Hbox2,
      Window1.Label8,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Window1.Alignment1, Window1.Hbox2);
   Add (Window1.Hardware_Button, Window1.Alignment1);
   Pack_Start
     (Window1.Vbox2,
      Window1.Hardware_Button,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Gtk_New (Tooltips);
   Set_Tip (Tooltips, Window1.Hardware_Button, -"Reconnect to the NetWeather Hardware");
   Button_Callback.Object_Connect
     (Window1.Hardware_Button, "released",
      Button_Callback.To_Marshaller (On_Hardwarebutton_Released'Access), Window1.Hardware_Button);
   Add (Window1.Frame2, Window1.Vbox2);
   Gtk_New (Window1.Label6, -("Hardware"));
   Set_Alignment (Window1.Label6, 0.5, 0.5);
   Set_Padding (Window1.Label6, 0, 0);
   Set_Justify (Window1.Label6, Justify_Left);
   Set_Line_Wrap (Window1.Label6, False);
   Set_Selectable (Window1.Label6, False);
   Set_Use_Markup (Window1.Label6, False);
   Set_Use_Underline (Window1.Label6, False);
   Set_Label_Widget (Window1.Frame2,Window1.Label6);

   Pack_Start
     (Window1.Vbox1,
      Window1.Frame2,
      Expand  => False,
      Fill    => True,
      Padding => 0);
   Gtk_New (Window1.Frame3);
   Set_Border_Width (Window1.Frame3, 3);
   Set_Label_Align (Window1.Frame3, 0.05, 0.5);
   Set_Shadow_Type (Window1.Frame3, Shadow_Etched_In);

   Gtk_New_Vbox (Window1.Vbox5, False, 0);

   Gtk_New (Window1.Web_Status_Label, -("Web Status Here"));
   Set_Alignment (Window1.Web_Status_Label, 0.5, 0.5);
   Set_Padding (Window1.Web_Status_Label, 0, 0);
   Set_Justify (Window1.Web_Status_Label, Justify_Left);
   Set_Line_Wrap (Window1.Web_Status_Label, False);
   Set_Selectable (Window1.Web_Status_Label, False);
   Set_Use_Markup (Window1.Web_Status_Label, False);
   Set_Use_Underline (Window1.Web_Status_Label, False);

   Pack_Start
     (Window1.Vbox5,
      Window1.Web_Status_Label,
      Expand  => False,
      Fill    => False,
      Padding => 5);
   Gtk_New_Hbox (Window1.Hbox4, False, 0);

   Gtk_New (Window1.Start_Web);
   Set_Border_Width (Window1.Start_Web, 10);
   Set_Relief (Window1.Start_Web, Relief_Normal);

   Gtk_New
     (Window1.Alignment2, 0.5, 0.5, 0.0,
      0.0);

   Gtk_New_Hbox (Window1.Hbox5, False, 2);

   Gtk_New (Window1.Image2 , "gtk-ok", Gtk_Icon_Size'Val (4));
   Set_Alignment (Window1.Image2, 0.5, 0.5);
   Set_Padding (Window1.Image2, 0, 0);

   Pack_Start
     (Window1.Hbox5,
      Window1.Image2,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Window1.Label13, -("Start"));
   Set_Alignment (Window1.Label13, 0.5, 0.5);
   Set_Padding (Window1.Label13, 0, 0);
   Set_Justify (Window1.Label13, Justify_Left);
   Set_Line_Wrap (Window1.Label13, False);
   Set_Selectable (Window1.Label13, False);
   Set_Use_Markup (Window1.Label13, False);
   Set_Use_Underline (Window1.Label13, True);

   Pack_Start
     (Window1.Hbox5,
      Window1.Label13,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Window1.Alignment2, Window1.Hbox5);
   Add (Window1.Start_Web, Window1.Alignment2);
   Pack_Start
     (Window1.Hbox4,
      Window1.Start_Web,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Set_Tip (Tooltips, Window1.Start_Web, -"Start the NetWeather Web Interface");
   Button_Callback.Object_Connect
     (Window1.Start_Web, "released",
      Button_Callback.To_Marshaller (On_Startweb_Released'Access), Window1.Start_Web);
   Gtk_New (Window1.Stop_Web);
   Set_Border_Width (Window1.Stop_Web, 10);
   Set_Relief (Window1.Stop_Web, Relief_Normal);

   Gtk_New
     (Window1.Alignment3, 0.5, 0.5, 0.0,
      0.0);

   Gtk_New_Hbox (Window1.Hbox6, False, 2);

   Gtk_New (Window1.Image3 , "gtk-cancel", Gtk_Icon_Size'Val (4));
   Set_Alignment (Window1.Image3, 0.5, 0.5);
   Set_Padding (Window1.Image3, 0, 0);

   Pack_Start
     (Window1.Hbox6,
      Window1.Image3,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Window1.Label14, -("Stop"));
   Set_Alignment (Window1.Label14, 0.5, 0.5);
   Set_Padding (Window1.Label14, 0, 0);
   Set_Justify (Window1.Label14, Justify_Left);
   Set_Line_Wrap (Window1.Label14, False);
   Set_Selectable (Window1.Label14, False);
   Set_Use_Markup (Window1.Label14, False);
   Set_Use_Underline (Window1.Label14, True);

   Pack_Start
     (Window1.Hbox6,
      Window1.Label14,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Window1.Alignment3, Window1.Hbox6);
   Add (Window1.Stop_Web, Window1.Alignment3);
   Pack_Start
     (Window1.Hbox4,
      Window1.Stop_Web,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Set_Tip (Tooltips, Window1.Stop_Web, -"Stop the NetWeather Web Interface");
   Button_Callback.Object_Connect
     (Window1.Stop_Web, "released",
      Button_Callback.To_Marshaller (On_Stopweb_Released'Access), Window1.Stop_Web);
   Pack_Start
     (Window1.Vbox5,
      Window1.Hbox4,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Add (Window1.Frame3, Window1.Vbox5);
   Gtk_New (Window1.Label10, -("Web Interface"));
   Set_Alignment (Window1.Label10, 0.5, 0.5);
   Set_Padding (Window1.Label10, 0, 0);
   Set_Justify (Window1.Label10, Justify_Left);
   Set_Line_Wrap (Window1.Label10, False);
   Set_Selectable (Window1.Label10, False);
   Set_Use_Markup (Window1.Label10, False);
   Set_Use_Underline (Window1.Label10, False);
   Set_Label_Widget (Window1.Frame3,Window1.Label10);

   Pack_Start
     (Window1.Vbox1,
      Window1.Frame3,
      Expand  => False,
      Fill    => True,
      Padding => 5);
   Gtk_New (Window1.Frame4);
   Set_Border_Width (Window1.Frame4, 3);
   Set_Label_Align (Window1.Frame4, 0.05, 0.5);
   Set_Shadow_Type (Window1.Frame4, Shadow_Etched_In);

   Gtk_New_Vbox (Window1.Vbox6, False, 0);

   Gtk_New (Window1.Database_Status_Label, -("Database Status Here"));
   Set_Alignment (Window1.Database_Status_Label, 0.5, 0.5);
   Set_Padding (Window1.Database_Status_Label, 0, 0);
   Set_Justify (Window1.Database_Status_Label, Justify_Left);
   Set_Line_Wrap (Window1.Database_Status_Label, False);
   Set_Selectable (Window1.Database_Status_Label, False);
   Set_Use_Markup (Window1.Database_Status_Label, False);
   Set_Use_Underline (Window1.Database_Status_Label, False);

   Pack_Start
     (Window1.Vbox6,
      Window1.Database_Status_Label,
      Expand  => False,
      Fill    => False,
      Padding => 5);
   Gtk_New_Hbox (Window1.Hbox7, False, 0);

   Gtk_New (Window1.Database_Connect);
   Set_Border_Width (Window1.Database_Connect, 10);
   Set_Relief (Window1.Database_Connect, Relief_Normal);

   Gtk_New
     (Window1.Alignment4, 0.5, 0.5, 0.0,
      0.0);

   Gtk_New_Hbox (Window1.Hbox8, False, 2);

   Gtk_New (Window1.Image4 , "gtk-connect", Gtk_Icon_Size'Val (4));
   Set_Alignment (Window1.Image4, 0.5, 0.5);
   Set_Padding (Window1.Image4, 0, 0);

   Pack_Start
     (Window1.Hbox8,
      Window1.Image4,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Window1.Label19, -("Connect"));
   Set_Alignment (Window1.Label19, 0.5, 0.5);
   Set_Padding (Window1.Label19, 0, 0);
   Set_Justify (Window1.Label19, Justify_Left);
   Set_Line_Wrap (Window1.Label19, False);
   Set_Selectable (Window1.Label19, False);
   Set_Use_Markup (Window1.Label19, False);
   Set_Use_Underline (Window1.Label19, True);

   Pack_Start
     (Window1.Hbox8,
      Window1.Label19,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Window1.Alignment4, Window1.Hbox8);
   Add (Window1.Database_Connect, Window1.Alignment4);
   Pack_Start
     (Window1.Hbox7,
      Window1.Database_Connect,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Set_Tip (Tooltips, Window1.Database_Connect, -"Connect to the NetWeather Database");
   Button_Callback.Object_Connect
     (Window1.Database_Connect, "released",
      Button_Callback.To_Marshaller (On_Databaseconnect_Released'Access), Window1.Database_Connect);
   Gtk_New (Window1.Database_Disconnect);
   Set_Border_Width (Window1.Database_Disconnect, 10);
   Set_Relief (Window1.Database_Disconnect, Relief_Normal);

   Gtk_New
     (Window1.Alignment5, 0.5, 0.5, 0.0,
      0.0);

   Gtk_New_Hbox (Window1.Hbox9, False, 2);

   Gtk_New (Window1.Image5 , "gtk-disconnect", Gtk_Icon_Size'Val (4));
   Set_Alignment (Window1.Image5, 0.5, 0.5);
   Set_Padding (Window1.Image5, 0, 0);

   Pack_Start
     (Window1.Hbox9,
      Window1.Image5,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Window1.Label20, -("Disconnect"));
   Set_Alignment (Window1.Label20, 0.5, 0.5);
   Set_Padding (Window1.Label20, 0, 0);
   Set_Justify (Window1.Label20, Justify_Left);
   Set_Line_Wrap (Window1.Label20, False);
   Set_Selectable (Window1.Label20, False);
   Set_Use_Markup (Window1.Label20, False);
   Set_Use_Underline (Window1.Label20, True);

   Pack_Start
     (Window1.Hbox9,
      Window1.Label20,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Window1.Alignment5, Window1.Hbox9);
   Add (Window1.Database_Disconnect, Window1.Alignment5);
   Pack_Start
     (Window1.Hbox7,
      Window1.Database_Disconnect,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Set_Tip (Tooltips, Window1.Database_Disconnect, -"Disconnect from the NetWeather Database");
   Button_Callback.Object_Connect
     (Window1.Database_Disconnect, "released",
      Button_Callback.To_Marshaller (On_Databasedisconnect_Released'Access), Window1.Database_Disconnect);
   Pack_Start
     (Window1.Vbox6,
      Window1.Hbox7,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Add (Window1.Frame4, Window1.Vbox6);
   Gtk_New (Window1.Label15, -("Database"));
   Set_Alignment (Window1.Label15, 0.5, 0.5);
   Set_Padding (Window1.Label15, 0, 0);
   Set_Justify (Window1.Label15, Justify_Left);
   Set_Line_Wrap (Window1.Label15, False);
   Set_Selectable (Window1.Label15, False);
   Set_Use_Markup (Window1.Label15, False);
   Set_Use_Underline (Window1.Label15, False);
   Set_Label_Widget (Window1.Frame4,Window1.Label15);

   Pack_Start
     (Window1.Vbox1,
      Window1.Frame4,
      Expand  => False,
      Fill    => True,
      Padding => 0);
   Append_Page (Window1.Notebook1, Window1.Vbox1);
   Set_Tab_Label_Packing (Window1.Notebook1, Window1.Vbox1, False, True, Pack_Start);
   Gtk_New (Window1.Label3, -("Status and Controls"));
   Set_Alignment (Window1.Label3, 0.5, 0.5);
   Set_Padding (Window1.Label3, 0, 0);
   Set_Justify (Window1.Label3, Justify_Left);
   Set_Line_Wrap (Window1.Label3, False);
   Set_Selectable (Window1.Label3, False);
   Set_Use_Markup (Window1.Label3, False);
   Set_Use_Underline (Window1.Label3, False);
   Set_Tab (Window1.Notebook1, 0, Window1.Label3);

   Gtk_New (Window1.Label21, -("Coming Soon"));
   Set_Alignment (Window1.Label21, 0.5, 0.5);
   Set_Padding (Window1.Label21, 0, 0);
   Set_Justify (Window1.Label21, Justify_Left);
   Set_Line_Wrap (Window1.Label21, False);
   Set_Selectable (Window1.Label21, False);
   Set_Use_Markup (Window1.Label21, False);
   Set_Use_Underline (Window1.Label21, False);

   Append_Page (Window1.Notebook1, Window1.Label21);
   Set_Tab_Label_Packing (Window1.Notebook1, Window1.Label21, False, True, Pack_Start);
   Gtk_New (Window1.Label4, -("Current Weather"));
   Set_Alignment (Window1.Label4, 0.5, 0.5);
   Set_Padding (Window1.Label4, 0, 0);
   Set_Justify (Window1.Label4, Justify_Left);
   Set_Line_Wrap (Window1.Label4, False);
   Set_Selectable (Window1.Label4, False);
   Set_Use_Markup (Window1.Label4, False);
   Set_Use_Underline (Window1.Label4, False);
   Set_Tab (Window1.Notebook1, 1, Window1.Label4);

   Gtk_New (Window1.Label23, -("Coming Soon"));
   Set_Alignment (Window1.Label23, 0.5, 0.5);
   Set_Padding (Window1.Label23, 0, 0);
   Set_Justify (Window1.Label23, Justify_Left);
   Set_Line_Wrap (Window1.Label23, False);
   Set_Selectable (Window1.Label23, False);
   Set_Use_Markup (Window1.Label23, False);
   Set_Use_Underline (Window1.Label23, False);

   Append_Page (Window1.Notebook1, Window1.Label23);
   Set_Tab_Label_Packing (Window1.Notebook1, Window1.Label23, False, True, Pack_Start);
   Gtk_New (Window1.Label5, -("Configuration"));
   Set_Alignment (Window1.Label5, 0.5, 0.5);
   Set_Padding (Window1.Label5, 0, 0);
   Set_Justify (Window1.Label5, Justify_Left);
   Set_Line_Wrap (Window1.Label5, False);
   Set_Selectable (Window1.Label5, False);
   Set_Use_Markup (Window1.Label5, False);
   Set_Use_Underline (Window1.Label5, False);
   Set_Tab (Window1.Notebook1, 2, Window1.Label5);

   Gtk_New (Window1.Image6 , Pixmaps_Dir & "vtc.gif");
   Set_Alignment (Window1.Image6, 0.5, 0.5);
   Set_Padding (Window1.Image6, 0, 0);

   Append_Page (Window1.Notebook1, Window1.Image6);
   Set_Tab_Label_Packing (Window1.Notebook1, Window1.Image6, False, True, Pack_Start);
   Gtk_New (Window1.Label22, -("About"));
   Set_Alignment (Window1.Label22, 0.5, 0.5);
   Set_Padding (Window1.Label22, 0, 0);
   Set_Justify (Window1.Label22, Justify_Left);
   Set_Line_Wrap (Window1.Label22, False);
   Set_Selectable (Window1.Label22, False);
   Set_Use_Markup (Window1.Label22, False);
   Set_Use_Underline (Window1.Label22, False);
   Set_Tab (Window1.Notebook1, 3, Window1.Label22);

   Add (Window1.Frame1, Window1.Notebook1);
   Gtk_New (Window1.Frame_Label, -("VTC NetWeather"));
   Set_Alignment (Window1.Frame_Label, 0.5, 0.5);
   Set_Padding (Window1.Frame_Label, 0, 0);
   Set_Justify (Window1.Frame_Label, Justify_Left);
   Set_Line_Wrap (Window1.Frame_Label, False);
   Set_Selectable (Window1.Frame_Label, False);
   Set_Use_Markup (Window1.Frame_Label, False);
   Set_Use_Underline (Window1.Frame_Label, False);
   Set_Label_Widget (Window1.Frame1,Window1.Frame_Label);

   Add (Window1, Window1.Frame1);
end Initialize;

end Window1_Pkg;
