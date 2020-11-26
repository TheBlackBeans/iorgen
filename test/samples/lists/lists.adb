with Ada.Integer_Text_Io;
with Ada.Text_Io;

procedure Main is
   type T_ListInt is array (Positive range <>) of Integer;
   type T_ListString4 is array (Positive range <>, Positive range <>) of Character;
   type T_Matrix is array (Positive range <>, Positive range <>) of Integer;

   procedure Lists (N           : Integer;       -- the first list's size
                    ListInt     : T_ListInt;     -- a list containing ints
                    Size        : Integer;       -- an other size
                    ListChar    : String;        -- a list of char
                    String_0    : String;        -- a string
                    ListString4 : T_ListString4; -- a list of strings of size 4
                    Matrix      : T_Matrix       -- a matrix of int
                   ) is
   begin
      -- TODO Aren't these lists beautifull?
      null;
   end Lists;

   N          : Integer;
   Size       : Integer;
   String_0   : String (1 .. 20);
   String_End : Natural;
begin
   Ada.Integer_Text_Io.Get(N);
   Ada.Text_Io.Skip_Line;
   declare
      ListInt : T_ListInt (1 .. N);
   begin
      for I in ListInt'Range loop
         Ada.Integer_Text_Io.Get(ListInt(I));
      end loop;
      Ada.Text_Io.Skip_Line;
      Ada.Integer_Text_Io.Get(Size);
      Ada.Text_Io.Skip_Line;
      declare
         ListChar : String (1 .. Size);
      begin
         Ada.Text_Io.Get(ListChar);
         Ada.Text_Io.Skip_Line;
         Ada.Text_Io.Get_Line(String_0, String_End);
         for I in String_End + 1 .. String_0'Last loop
             String_0(I) := Character'Val(0);
         end loop;
         if String_End = 0 then
            Ada.Text_Io.Skip_Line;
         end if;
         declare
            ListString4 : T_ListString4 (1 .. Size, 1 .. 4);
         begin
            for I in ListString4'Range loop
               for J in ListString4'Range(2) loop
                  if Ada.Text_IO.End_Of_Line then
                     ListString4(I, J) := Character'Val(0);
                  else
                     Ada.Text_Io.Get(ListString4(I, J));
                  end if;
               end loop;
               Ada.Text_Io.Skip_Line;
            end loop;
            declare
               Matrix : T_Matrix (1 .. Size, 1 .. Size);
            begin
               for I in Matrix'Range loop
                  for J in Matrix'Range(2) loop
                     Ada.Integer_Text_Io.Get(Matrix(I, J));
                  end loop;
                  Ada.Text_Io.Skip_Line;
               end loop;
               Lists(N, ListInt, Size, ListChar, String_0, ListString4, Matrix);
            end;
         end;
      end;
   end;
end Main;
