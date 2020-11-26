with Ada.Integer_Text_Io;
with Ada.Text_Io;

procedure Main is
   procedure Simple (N           : Integer; -- the first number
                     OtherNumber : Integer  -- the second number
                    ) is
   begin
      -- TODO Just do what you want with these numbers, like sum them.
      null;
   end Simple;

   N           : Integer;
   OtherNumber : Integer;
begin
   Ada.Integer_Text_Io.Get(N);
   Ada.Text_Io.Skip_Line;
   Ada.Integer_Text_Io.Get(OtherNumber);
   Ada.Text_Io.Skip_Line;
   Simple(N, OtherNumber);
end Main;
