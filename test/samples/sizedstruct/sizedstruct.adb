with Ada.Containers.Indefinite_Vectors;
with Ada.Integer_Text_Io;
with Ada.Text_Io;

procedure Main is
   type T_IntList is array (Positive range <>) of Integer;
   type List (Size_1 : Natural) is
      -- contains a list
      record
         Size1   : Integer;                 -- the list's size
         IntList : T_IntList (1 .. Size_1); -- the integer list
      end record;

   type String_0 (Size_1 : Natural) is
      -- contains a string
      record
         Size2      : Integer;              -- the list's size
         StringList : String (1 .. Size_1); -- the string list
      end record;

   type T_ListList is array (Positive range <>, Positive range <>) of Integer;
   type Matrix (Size_1 : Natural) is
      -- contains a matrix
      record
         Size3    : Integer;                          -- the list's size
         ListList : T_ListList (1 .. Size_1, 1 .. 2); -- the list list
      end record;

   type T_IntListN is array (Positive range <>) of Integer;
   type NotASizedStruct (Size_1 : Natural) is
      -- this is not a 'sized struct', but a regular one!
      record
         Size4    : Integer;                  -- not the list's size
         IntListN : T_IntListN (1 .. Size_1); -- the integer list
      end record;

   package V_Lists is new Ada.Containers.Indefinite_Vectors (Positive, List);
   package V_Strings is new Ada.Containers.Indefinite_Vectors (Positive, String_0);
   package V_Matrices is new Ada.Containers.Indefinite_Vectors (Positive, Matrix);
   package V_Same is new Ada.Containers.Indefinite_Vectors (Positive, NotASizedStruct);

   procedure SizedStruct (N        : Integer;           -- the size of the lists
                          Lists    : V_Lists.Vector;    -- a list of list of different sizes
                          Strings  : V_Strings.Vector;  -- a list of strings of different sizes
                          Matrices : V_Matrices.Vector; -- a list of matrices of different sizes
                          Same     : V_Same.Vector      -- a list of list of same sizes
                         ) is
   begin
      -- TODO The is a special case.
      null;
   end SizedStruct;

   N          : Integer;
   Lists      : V_Lists.Vector;
   Strings    : V_Strings.Vector;
   Matrices   : V_Matrices.Vector;
   Same       : V_Same.Vector;
   Size_1     : Natural;
   String_End : Natural;
begin
   Ada.Integer_Text_Io.Get(N);
   Ada.Text_Io.Skip_Line;
   Lists.Reserve_capacity(Ada.Containers.Count_Type(N));
   for I in 1 .. N loop
      Ada.Integer_Text_Io.Get(Size_1);
      Ada.Text_Io.Skip_Line;
      declare
         Lists_E : List (Size_1);
      begin
         Lists_E.Size1 := Size_1;
         for J in Lists_E.IntList'Range loop
            Ada.Integer_Text_Io.Get(Lists_E.IntList(J));
         end loop;
         Ada.Text_Io.Skip_Line;
         Lists.Append(Lists_E);
      end;
   end loop;
   Strings.Reserve_capacity(Ada.Containers.Count_Type(N));
   for I in 1 .. N loop
      Ada.Integer_Text_Io.Get(Size_1);
      Ada.Text_Io.Skip_Line;
      declare
         Strings_E : String_0 (Size_1);
      begin
         Strings_E.Size2 := Size_1;
         Ada.Text_Io.Get_Line(Strings_E.StringList, String_End);
         for J in String_End + 1 .. Strings_E.StringList'Last loop
             Strings_E.StringList(J) := Character'Val(0);
         end loop;
         if String_End = 0 then
            Ada.Text_Io.Skip_Line;
         end if;
         Strings.Append(Strings_E);
      end;
   end loop;
   for I in 1 .. 2 loop
      Ada.Integer_Text_Io.Get(Size_1);
      Ada.Text_Io.Skip_Line;
      declare
         Matrices_E : Matrix (Size_1);
      begin
         Matrices_E.Size3 := Size_1;
         for J in Matrices_E.ListList'Range loop
            for K in Matrices_E.ListList'Range(2) loop
               Ada.Integer_Text_Io.Get(Matrices_E.ListList(J, K));
            end loop;
            Ada.Text_Io.Skip_Line;
         end loop;
         Matrices.Append(Matrices_E);
      end;
   end loop;
   Same.Reserve_capacity(Ada.Containers.Count_Type(N));
   for I in 1 .. N loop
      declare
         Same_E : NotASizedStruct (N);
      begin
         Ada.Integer_Text_Io.Get(Same_E.Size4);
         Ada.Text_Io.Skip_Line;
         for J in Same_E.IntListN'Range loop
            Ada.Integer_Text_Io.Get(Same_E.IntListN(J));
         end loop;
         Ada.Text_Io.Skip_Line;
         Same.Append(Same_E);
      end;
   end loop;
   SizedStruct(N, Lists, Strings, Matrices, Same);
end Main;
