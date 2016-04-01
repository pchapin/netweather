---------------------------------------------------------------------------
-- FILE          : support-signal_processing.ads
-- LAST REVISION : 2008-02-23
-- SUBJECT       : Specification of several signal processing primitives
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Ada.Numerics.Generic_Complex_Types;

package Support.Signal_Processing is
   type Numeric_Type is digits 12;
   package Complex_Numeric is new Ada.Numerics.Generic_Complex_Types(Numeric_Type);

   type Waveform_Index is new Natural;
   type Spectrum_Index is new Natural;
   type Waveform is array(Waveform_Index range <>) of Numeric_Type;
   type Spectrum is array(Spectrum_Index range <>) of Complex_Numeric.Complex;

   function Read_Waveform(File_Name : String) return Waveform;
   function Average_Value(Signal : Waveform) return Numeric_Type;
   function RMS_Value(Signal : Waveform) return Numeric_Type;
   procedure Min_Max(Signal : in Waveform; Min_Value, Max_Value : out Numeric_Type);
   function Discrete_Fourier_Transform(Signal : Waveform) return Spectrum;

end Support.Signal_Processing;
