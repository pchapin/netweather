with Gtkada.Intl; use Gtkada.Intl;

package body Layout_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Layout", Msg);
   end "-";

end Layout_Intl;
