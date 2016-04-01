---------------------------------------------------------------------------
-- FILE          : support-signal_processing.adb
-- LAST REVISION : 2008-02-23
-- SUBJECT       : Several signal processing primitives
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

package body Support.Signal_Processing is
   package Numeric_IO is
      new Ada.Text_IO.Float_IO(Numeric_Type);
   package Waveform_Index_IO is
      new Ada.Text_IO.Integer_IO(Waveform_Index);
   package Numeric_Functions is
      new Ada.Numerics.Generic_Elementary_Functions(Numeric_Type);
   package Complex_Numeric_Functions is
      new Ada.Numerics.Generic_Complex_Elementary_Functions(Complex_Numeric);

   use Complex_Numeric;
   use Numeric_IO;
   use Waveform_Index_IO;
   use Numeric_Functions;
   use Complex_Numeric_Functions;


   function Read_Waveform(File_Name : in String) return Waveform is
      Input_File : File_Type;
      Value      : Numeric_Type;
      Count      : Waveform_Index;
   begin
      Open(File => Input_File, Mode => In_File, Name => File_Name);
      Get(Input_File, Count);
      declare
         Result : Waveform(0 .. Count - 1);
      begin
         for T in 0 .. Count - 1 loop
            Get(Input_File, Value);
            Result(T) := Value;
         end loop;
         Close(Input_File);
         return Result;
      end;
   end Read_Waveform;


   function Average_Value(Signal : Waveform) return Numeric_Type is
      Accumulator : Numeric_Type := 0.0;
   begin
      for I in Signal'Range loop
         Accumulator := Accumulator + Signal(I);
      end loop;
      return Accumulator / Numeric_Type(Signal'Length);
   end Average_Value;


   function RMS_Value(Signal : Waveform) return Numeric_Type is
      Accumulator : Numeric_Type := 0.0;
   begin
      for I in Signal'Range loop
         Accumulator := Accumulator + (Signal(I) * Signal(I));
      end loop;
      return Sqrt(Accumulator / Numeric_Type(Signal'Length));
   end RMS_Value;


   procedure Min_Max(Signal : in Waveform; Min_Value, Max_Value : out Numeric_Type) is
      Min : Numeric_Type := Numeric_Type'Last;
      Max : Numeric_Type := Numeric_Type'First;
   begin
      -- FIXME: We should probably raise an exception if Signal is empty.
      for I in Signal'Range loop
         if Signal(I) < Min then Min := Signal(I); end if;
         if Signal(I) > Max then Max := Signal(I); end if;
      end loop;
      Min_Value := Min;
      Max_Value := Max;
   end Min_Max;


   function Discrete_Fourier_Transform(Signal : Waveform) return Spectrum is
      Harmonics : Spectrum(0 .. Spectrum_Index(Signal'Last/2));
      N     : Natural := Signal'Length;
      Omega : Numeric_Type := (2.0 * Ada.Numerics.Pi) / Numeric_Type(N);
      Phase : Numeric_Type;
   begin
      for K in Harmonics'Range loop
         Harmonics(K) := (0.0, 0.0);
         for T in Signal'Range loop
            Phase := Omega * Numeric_Type(T);
            Harmonics(K) := Harmonics(K) + Signal(T) * exp(j * Numeric_Type(K) * Phase);
         end loop;
         Harmonics(K) := 2.0 * Harmonics(K) / Numeric_Type(N);
      end loop;
      return Harmonics;
   end Discrete_Fourier_Transform;

end Support.Signal_Processing;

