---------------------------------------------------------------------------
-- FILE          : support-simd.ads
-- LAST REVISION : 2008-02-23
-- SUBJECT       : Specification of package for manipulating SIMD arrays.
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Ada.Finalization;

generic
   type Numeric_Type is digits <>;
package Support.SIMD is
   type SIMD_Index is new Natural;
   type SIMD_Array is private;

   Invalid_Length : exception;

   -- Constructor.
   function Make(Size : SIMD_Index) return SIMD_Array;

   -- Access.
   procedure Set_Element
      (Vector : in out SIMD_Array; Index : in SIMD_Index; Value : in Numeric_Type);
      -- Raises Constraint_Error if index is out of bounds.

   function Get_Element
      (Vector : SIMD_Array; Index : SIMD_Index) return Numeric_Type;
      -- Raises Constraint_Error if index is out of bounds.

   -- Basic arithmetic.
   function "+"(Left, Right : SIMD_Array) return SIMD_Array;
   function "-"(Left, Right : SIMD_Array) return SIMD_Array;
   function "*"(Left, Right : SIMD_Array) return SIMD_Array;
   function "/"(Left, Right : SIMD_Array) return SIMD_Array;

   -- Elementary functions.
   function Sqrt(Vector : SIMD_Array) return SIMD_Array;

private
   type SIMD_Representation is array(SIMD_Index range <>) of Numeric_Type;
   type SIMD_Pointer is access SIMD_Representation;
   type SIMD_Array is new Ada.Finalization.Controlled with
      record
         Representation : SIMD_Pointer := null;
      end record;

   procedure Finalize(Vector : in out SIMD_Array);
   procedure Adjust(Vector : in out SIMD_Array);

end Support.SIMD;

