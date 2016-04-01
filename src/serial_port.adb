---------------------------------------------------------------------------
-- FILE          : serial_port.adb
-- LAST REVISION : 2008-02-23
-- SUBJECT       : Body of package for simple serial port access.
--
-- Please send comments or bug reports to
--
--      c/o Peter C. Chapin
--      Electrical and Computer Engineering Technology
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;
with Win32;
with Win32.Winnt;
with Win32.Winbase;

package body Serial_Port is

   use type System.Address;
   use type Win32.BOOL;
   use type Interfaces.C.unsigned_long;

   Current_Port  : Win32.Winnt.HANDLE := System.Null_Address;
   Old_Settings, New_Settings : aliased Win32.Winbase.DCB;
   Old_Timeouts, New_Timeouts : aliased Win32.Winbase.COMMTIMEOUTS;

   Input_Byte    : aliased Interfaces.Unsigned_8;  -- Data byte from ReadFile.
   Output_Byte   : aliased Interfaces.Unsigned_8;  -- Data byte to WriteFile.
   Bytes_Read    : aliased Win32.DWORD;            -- # of bytes read by ReadFile.
   Bytes_Written : aliased Win32.DWORD;            -- # of bytes written by WriteFile.

   type Byte_Pointer is access all Interfaces.Unsigned_8;
   function Convert_Address is
      new Ada.Unchecked_Conversion(Source => Byte_Pointer, Target => System.Address);

   -- Look up tables for easy access to raw settings values.

   Port_Table : array(Port_Type) of String(1..5) :=
     ("COM1" & Ada.Characters.Latin_1.NUL, "COM2" & Ada.Characters.Latin_1.NUL);

   Baud_Table : array(Baud_Type) of Win32.DWORD :=
     (2400, 4800, 9600, 19200);

   Parity_Table : array(Parity_Type) of Win32.BYTE :=
     (Win32.Winbase.NOPARITY, Win32.Winbase.EVENPARITY, Win32.Winbase.ODDPARITY);

   Data_Size_Table : array(Data_Size_Type) of Win32.BYTE :=
     (7, 8);

   Stop_Table : array(Stop_Type) of Win32.BYTE :=
     (Win32.Winbase.ONESTOPBIT, Win32.Winbase.TWOSTOPBITS);


   procedure Open
      (Port      : in Port_Type;
       Baud      : in Baud_Type;
       Parity    : in Parity_Type;
       Data_Size : in Data_Size_Type;
       Stop      : in Stop_Type) is

      Dummy_Result : Win32.BOOL;
   begin
      if Current_Port /= System.Null_Address then
         raise Open_Failure;
      end if;

      Current_Port := Win32.Winbase.CreateFile
        (lpFileName            => Win32.Addr(Port_Table(Port)),
         dwDesiredAccess       => Win32.Winnt.GENERIC_READ or Win32.Winnt.GENERIC_WRITE,
         dwShareMode           => 0,
         lpSecurityAttributes  => null,
         dwCreationDisposition => Win32.Winbase.OPEN_EXISTING,
         dwFlagsAndAttributes  => 0,
         hTemplateFile         => System.Null_Address);

      -- Check for success.
      if Current_Port = Win32.Winbase.INVALID_HANDLE_VALUE then
         Current_Port := System.Null_Address;
         raise Open_Failure;
      end if;

      -- Get the current port settings.
      if Win32.Winbase.GetCommState(Current_Port, Old_Settings'Access) = Win32.FALSE then
         Dummy_Result := Win32.Winbase.CloseHandle(Current_Port);
         Current_Port := System.Null_Address;
         raise Open_Failure;
      end if;

      -- Get the current port timeouts.
      if Win32.Winbase.GetCommTimeouts(Current_Port, Old_Timeouts'Access) = Win32.FALSE then
         Dummy_Result := Win32.Winbase.CloseHandle(Current_Port);
         Current_Port := System.Null_Address;
         raise Open_Failure;
      end if;

      New_Settings := Old_Settings;
      New_Timeouts := Old_Timeouts;

      -- Adjust the settings.
      New_Settings.BaudRate := Baud_Table(Baud);
      New_Settings.fBinary  := Win32.TRUE;
      if Parity = None then
         New_Settings.fParity := Win32.FALSE;
      else
         New_Settings.fParity := Win32.TRUE;
      end if;
      New_Settings.fOutxCtsFlow    := Win32.FALSE;
      New_Settings.fOutxDsrFlow    := Win32.FALSE;
      New_Settings.fDtrControl     := Win32.Winbase.DTR_CONTROL_ENABLE;
      New_Settings.fDsrSensitivity := Win32.FALSE;
      New_Settings.fOutX           := Win32.FALSE;
      New_Settings.fInX            := Win32.FALSE;
      New_Settings.fErrorChar      := Win32.FALSE;
      New_Settings.fNull           := Win32.FALSE;
      New_Settings.fRtsControl     := Win32.Winbase.RTS_CONTROL_ENABLE;
      New_Settings.fAbortOnError   := Win32.FALSE;
      New_Settings.ByteSize        := Data_Size_Table(Data_Size);
      New_Settings.Parity          := Parity_Table(Parity);
      New_Settings.StopBits        := Stop_Table(Stop);

      if Win32.Winbase.SetCommState(Current_Port, New_Settings'Access) = Win32.FALSE then
         Dummy_Result := Win32.Winbase.CloseHandle(Current_Port);
         Current_Port := System.Null_Address;
         raise Open_Failure;
      end if;

   end Open;


   function Read return Interfaces.Unsigned_8 is
   begin
      if Current_Port = System.Null_Address then
         raise IO_Failure;
      end if;

      if Win32.Winbase.ReadFile
         (hFile                => Current_Port,
          lpBuffer             => Convert_Address(Input_Byte'Access),
          nNumberOfBytesToRead => 1,
          lpNumberOfBytesRead  => Bytes_Read'Access,
          lpOverlapped         => null) = Win32.FALSE then
         raise IO_Failure;
      end if;
      return Input_Byte;
   end Read;


   procedure Write(Byte : Interfaces.Unsigned_8) is
   begin
      if Current_Port = System.Null_Address then
         raise IO_Failure;
      end if;

      Output_Byte := Byte;
      if Win32.Winbase.WriteFile
         (hFile                  => Current_Port,
          lpBuffer               => Convert_Address(Output_Byte'Access),
          nNumberOfBytesToWrite  => 1,
          lpNumberOfBytesWritten => Bytes_Written'Access,
          lpOverlapped           => null) = Win32.FALSE then
         raise IO_Failure;
      end if;
   end Write;


   procedure Close is
      Dummy_Result : Win32.BOOL;
   begin
      if Current_Port = System.Null_Address then
         return;
      end if;

      Dummy_Result := Win32.Winbase.SetCommState(Current_Port, Old_Settings'Access);
      Dummy_Result := Win32.Winbase.SetCommTimeouts(Current_Port, Old_Timeouts'Access);
      Dummy_Result := Win32.Winbase.CloseHandle(Current_Port);
      Current_Port := System.Null_Address;
   end Close;


end Serial_Port;
