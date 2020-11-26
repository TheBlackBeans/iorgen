with Ada.Integer_Text_Io;
with Ada.Text_Io;

procedure Main is
   type AStruct is
      -- A struct for the example
      record
         Integer_0   : Integer;   -- an integer
         Character_0 : Character; -- a char
      end record;

   type T_List is array (Positive range <>) of AStruct;

   procedure Example (N    : Integer; -- a number, used as a size
                      List : T_List   -- a list of structs
                     ) is
   begin
      -- TODO In a real life scenario, you will describe here what you want the
      -- end user to do with this generated code
      null;
   end Example;

   N : Integer;
begin
   Ada.Integer_Text_Io.Get(N);
   Ada.Text_Io.Skip_Line;
   declare
      List : T_List (1 .. N);
   begin
      for I in List'Range loop
         Ada.Integer_Text_Io.Get(List(I).Integer_0);
         Ada.Text_Io.Get(List(I).Character_0);
         Ada.Text_Io.Get(List(I).Character_0);
         Ada.Text_Io.Skip_Line;
      end loop;
      Example(N, List);
   end;
end Main;
