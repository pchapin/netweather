---------------------------------------------------------------------------
-- FILE          : support-simd.adb
-- LAST REVISION : 2008-02-23
-- SUBJECT       : Implementation of package for manipulating SIMD arrays.
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Deallocation;

package body Support.SIMD is

   package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions(Numeric_Type);

   procedure Deallocate is
      new Ada.Unchecked_Deallocation(Object => SIMD_Representation, Name => SIMD_Pointer);

   -- Constructor.
   function Make(Size : SIMD_Index) return SIMD_Array is
      Result : SIMD_Array;
   begin
      Result.Representation := new SIMD_Representation(0 .. Size - 1);
      for I in Result.Representation'Range loop
         Result.Representation(I) := 0.0;
      end loop;
      return Result;
   end Make;

   -- Access.
   procedure Set_Element
      (Vector : in out SIMD_Array; Index : in SIMD_Index; Value : in Numeric_Type) is
   begin
      Vector.Representation(Index) := Value;
   end Set_Element;

   function Get_Element
      (Vector : SIMD_Array; Index : SIMD_Index) return Numeric_Type is
   begin
      return Vector.Representation(Index);
   end Get_Element;

   -- Binary operations.
   generic
      with function Operation(Left, Right : Numeric_Type) return Numeric_Type;
   function SIMD_Binary_Operation(Left, Right : SIMD_Array) return SIMD_Array;

   function SIMD_Binary_Operation(Left, Right : SIMD_Array) return SIMD_Array is
     Result : SIMD_Array;
     J      : SIMD_Index := 0;
   begin
      if Left.Representation'Length /= Right.Representation'Length then
         raise Invalid_Length;
      end if;
      Result.Representation := new SIMD_Representation(0 .. Left.Representation'Length - 1);
      for I in Result.Representation'Range loop
         Result.Representation(J) := Operation(Left.Representation(I), Right.Representation(I));
      end loop;
      return Result;
   end SIMD_Binary_Operation;

   function Internal_Add is new SIMD_Binary_Operation(Operation => "+");
   function Internal_Sub is new SIMD_Binary_Operation(Operation => "-");
   function Internal_Mul is new SIMD_Binary_Operation(Operation => "*");
   function Internal_Div is new SIMD_Binary_Operation(Operation => "/");

   function "+"(Left, Right : SIMD_Array) return SIMD_Array renames Internal_Add;
   function "-"(Left, Right : SIMD_Array) return SIMD_Array renames Internal_Add;
   function "*"(Left, Right : SIMD_Array) return SIMD_Array renames Internal_Add;
   function "/"(Left, Right : SIMD_Array) return SIMD_Array renames Internal_Add;

   -- Unary operations.
   generic
      with function Operation(Value : Numeric_Type) return Numeric_Type;
   function SIMD_Unary_Operation(Vector : SIMD_Array) return SIMD_Array;

   function SIMD_Unary_Operation(Vector : SIMD_Array) return SIMD_Array is
      Result : SIMD_Array;
   begin
      Result.Representation := new SIMD_Representation(0 .. Vector.Representation'Length - 1);
      for I in Result.Representation'Range loop
         Result.Representation(I) := Operation(Vector.Representation(I));
      end loop;
      return Result;
   end SIMD_Unary_Operation;

   function Internal_Sqrt is new SIMD_Unary_Operation(Operation => Elementary_Functions.Sqrt);

   function Sqrt(Vector : SIMD_Array) return SIMD_Array renames Internal_Sqrt;

   -- Overriding subprograms for Controlled types.

   procedure Finalize(Vector : in out SIMD_Array) is
   begin
      Deallocate(Vector.Representation);
   end Finalize;

   procedure Adjust(Vector : in out SIMD_Array) is
      New_Representation : SIMD_Pointer;
   begin
      New_Representation := new SIMD_Representation(Vector.Representation'Range);
      New_Representation.all := Vector.Representation.all;
      Vector.Representation := New_Representation;
   exception
      when Storage_Error =>
         Vector.Representation := null;
         raise;
   end Adjust;

end Support.SIMD;

