with Ada.Integer_Text_Io;
with Ada.Text_Io;

procedure Main is
   type Struct1 is
      -- A simple struct
      record
         Foo : Integer; -- a field
         Bar : Integer; -- a field
      end record;

   type Position is
      -- Represents a position
      record
         X : Integer; -- X
         Y : Integer; -- Y
         Z : Integer; -- Z
      end record;

   type Point is
      -- A point's name and position
      record
         Name        : Character;        -- the point's name (single character)
         Description : String (1 .. 12); -- the point's description
         Pos         : Position;         -- the point's position
      end record;

   type Chars is
      -- a struct of chars
      record
         FirstChar  : Character; -- a first char
         SecondChar : Character; -- a second char
         ThirdChar  : Character; -- a third char
      end record;

   type T_StructList is array (Positive range <>) of Struct1;
   type T_Triangle is array (1 .. 3) of Point;

   procedure Structs (Struct      : Struct1;      -- a struct 1 instance
                      N           : Integer;      -- a number
                      StructList  : T_StructList; -- a list a struct 1
                      Triangle    : T_Triangle;   -- a triangle
                      StructChars : Chars         -- a struct of chars
                     ) is
   begin
      -- TODO Look at them structs.
      null;
   end Structs;

   Struct      : Struct1;
   N           : Integer;
   Triangle    : T_Triangle;
   StructChars : Chars;
   String_End  : Natural;
begin
   Ada.Integer_Text_Io.Get(Struct.Foo);
   Ada.Integer_Text_Io.Get(Struct.Bar);
   Ada.Text_Io.Skip_Line;
   Ada.Integer_Text_Io.Get(N);
   Ada.Text_Io.Skip_Line;
   declare
      StructList : T_StructList (1 .. N);
   begin
      for I in StructList'Range loop
         Ada.Integer_Text_Io.Get(StructList(I).Foo);
         Ada.Integer_Text_Io.Get(StructList(I).Bar);
         Ada.Text_Io.Skip_Line;
      end loop;
      for I in Triangle'Range loop
         Ada.Text_Io.Get(Triangle(I).Name);
         Ada.Text_Io.Skip_Line;
         Ada.Text_Io.Get_Line(Triangle(I).Description, String_End);
         for J in String_End + 1 .. Triangle(I).Description'Last loop
             Triangle(I).Description(J) := Character'Val(0);
         end loop;
         if String_End = 0 then
            Ada.Text_Io.Skip_Line;
         end if;
         Ada.Integer_Text_Io.Get(Triangle(I).Pos.X);
         Ada.Integer_Text_Io.Get(Triangle(I).Pos.Y);
         Ada.Integer_Text_Io.Get(Triangle(I).Pos.Z);
         Ada.Text_Io.Skip_Line;
      end loop;
      Ada.Text_Io.Get(StructChars.FirstChar);
      Ada.Text_Io.Get(StructChars.SecondChar);
      Ada.Text_Io.Get(StructChars.SecondChar);
      Ada.Text_Io.Get(StructChars.ThirdChar);
      Ada.Text_Io.Get(StructChars.ThirdChar);
      Ada.Text_Io.Skip_Line;
      Structs(Struct, N, StructList, Triangle, StructChars);
   end;
end Main;
