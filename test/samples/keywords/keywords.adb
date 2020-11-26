with Ada.Integer_Text_Io;
with Ada.Text_Io;

procedure Main is
   type Console is
      -- may conflict in c#
      record
         A      : Integer; -- the first letter of the alphabet
         Static : Integer; -- an integer
      end record;

   type T_Void is array (1 .. 3) of Integer;
   type System is
      -- may conflict in c#
      record
         Return_0 : Integer; -- not the end of the function
         Void     : T_Void;  -- not nothing
      end record;

   type Main is
      -- not the main function
      record
         Int    : System;  -- not an integer
         IfTrue : Integer; -- should not cause conflict
      end record;

   type T_For_0 is array (Positive range <>) of Integer;
   type T_Words is array (1 .. 2) of Main;

   procedure Keywords (If_0  : Integer;   -- not a condition
                       Class : Character; -- not a class
                       I     : String;    -- just a string
                       In_0  : Console;   -- not in
                       For_0 : T_For_0;   -- not a loop
                       Words : T_Words    -- contains lots of things
                      ) is
   begin
      -- TODO If this compiles, it is already a good step!
      null;
   end Keywords;

   If_0       : Integer;
   Class      : Character;
   I          : String (1 .. 8);
   In_0       : Console;
   Words      : T_Words;
   String_End : Natural;
begin
   Ada.Integer_Text_Io.Get(If_0);
   Ada.Text_Io.Skip_Line;
   Ada.Text_Io.Get(Class);
   Ada.Text_Io.Skip_Line;
   Ada.Text_Io.Get_Line(I, String_End);
   for J in String_End + 1 .. I'Last loop
       I(J) := Character'Val(0);
   end loop;
   if String_End = 0 then
      Ada.Text_Io.Skip_Line;
   end if;
   Ada.Integer_Text_Io.Get(In_0.A);
   Ada.Integer_Text_Io.Get(In_0.Static);
   Ada.Text_Io.Skip_Line;
   declare
      For_0 : T_For_0 (1 .. If_0);
   begin
      for J in For_0'Range loop
         Ada.Integer_Text_Io.Get(For_0(J));
      end loop;
      Ada.Text_Io.Skip_Line;
      for J in Words'Range loop
         Ada.Integer_Text_Io.Get(Words(J).Int.Return_0);
         Ada.Text_Io.Skip_Line;
         for K in Words(J).Int.Void'Range loop
            Ada.Integer_Text_Io.Get(Words(J).Int.Void(K));
         end loop;
         Ada.Text_Io.Skip_Line;
         Ada.Integer_Text_Io.Get(Words(J).IfTrue);
         Ada.Text_Io.Skip_Line;
      end loop;
      Keywords(If_0, Class, I, In_0, For_0, Words);
   end;
end Main;
