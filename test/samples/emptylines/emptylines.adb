with Ada.Integer_Text_Io;
with Ada.Text_Io;

procedure Main is
   type StructWithAChar is
      -- a char struct
      record
         Char1 : Character; -- a char
         Int2  : Integer;   -- an integer
      end record;

   type T_ListInStruct is array (Positive range <>) of Integer;
   type A (Size_1 : Natural) is
      -- a struct
      record
         ListInStruct   : T_ListInStruct (1 .. Size_1); -- a list in a struct
         StructInStruct : StructWithAChar;              -- a struct in a struct
      end record;

   type SizedStruct (Size_1 : Natural) is
      -- a sized struct
      record
         Size           : Integer;              -- the size
         StringInStruct : String (1 .. Size_1); -- the string
      end record;

   type T_EmptyList is array (1 .. 0) of Integer;
   type T_EmptyInSample is array (Positive range <>) of Integer;

   procedure EmptyLines (EmptyList           : T_EmptyList;     -- an empty list
                         BufferString        : String;          -- here to check correct parsing of empty line above
                         N                   : Integer;         -- an integer, will be 0 in the sample input
                         EmptyInSample       : T_EmptyInSample; -- an empty list (only in the sample)
                         EmptyString         : String;          -- an empty string
                         Main                : String;          -- an other buffer string
                         EmptyCharList       : String;          -- an empty char list
                         NonEmptyCharList    : String;          -- an char list, non empty
                         StructWithEmptyLine : A;               -- a struct containing an empty line, then a struct
                         ASizedStruct        : SizedStruct;     -- a sized struct containing an empty line
                         Finish              : String           -- a string to finish
                        ) is
   begin
      -- TODO Wow, lots of empty lines!
      null;
   end EmptyLines;

   EmptyList        : T_EmptyList;
   BufferString     : String (1 .. 3);
   N                : Integer;
   EmptyString      : String (1 .. 0);
   Main             : String (1 .. 4);
   EmptyCharList    : String (1 .. 0);
   NonEmptyCharList : String (1 .. 5);
   Finish           : String (1 .. 6);
   Size_1           : Natural;
   String_End       : Natural;
begin
   for I in EmptyList'Range loop
      Ada.Integer_Text_Io.Get(EmptyList(I));
   end loop;
   Ada.Text_Io.Skip_Line;
   Ada.Text_Io.Get_Line(BufferString, String_End);
   for I in String_End + 1 .. BufferString'Last loop
       BufferString(I) := Character'Val(0);
   end loop;
   if String_End = 0 then
      Ada.Text_Io.Skip_Line;
   end if;
   Ada.Integer_Text_Io.Get(N);
   Ada.Text_Io.Skip_Line;
   declare
      EmptyInSample : T_EmptyInSample (1 .. N);
   begin
      for I in EmptyInSample'Range loop
         Ada.Integer_Text_Io.Get(EmptyInSample(I));
      end loop;
      Ada.Text_Io.Skip_Line;
      Ada.Text_Io.Get_Line(EmptyString, String_End);
      for I in String_End + 1 .. EmptyString'Last loop
          EmptyString(I) := Character'Val(0);
      end loop;
      if String_End = 0 then
         Ada.Text_Io.Skip_Line;
      end if;
      Ada.Text_Io.Get_Line(Main, String_End);
      for I in String_End + 1 .. Main'Last loop
          Main(I) := Character'Val(0);
      end loop;
      if String_End = 0 then
         Ada.Text_Io.Skip_Line;
      end if;
      Ada.Text_Io.Get(EmptyCharList);
      Ada.Text_Io.Skip_Line;
      Ada.Text_Io.Get(NonEmptyCharList);
      Ada.Text_Io.Skip_Line;
      declare
         StructWithEmptyLine : A (N);
      begin
         for I in StructWithEmptyLine.ListInStruct'Range loop
            Ada.Integer_Text_Io.Get(StructWithEmptyLine.ListInStruct(I));
         end loop;
         Ada.Text_Io.Skip_Line;
         Ada.Text_Io.Get(StructWithEmptyLine.StructInStruct.Char1);
         Ada.Integer_Text_Io.Get(StructWithEmptyLine.StructInStruct.Int2);
         Ada.Text_Io.Skip_Line;
         Ada.Integer_Text_Io.Get(Size_1);
         Ada.Text_Io.Skip_Line;
         declare
            ASizedStruct : SizedStruct (Size_1);
         begin
            ASizedStruct.Size := Size_1;
            Ada.Text_Io.Get_Line(ASizedStruct.StringInStruct, String_End);
            for I in String_End + 1 .. ASizedStruct.StringInStruct'Last loop
                ASizedStruct.StringInStruct(I) := Character'Val(0);
            end loop;
            if String_End = 0 then
               Ada.Text_Io.Skip_Line;
            end if;
            Ada.Text_Io.Get_Line(Finish, String_End);
            for I in String_End + 1 .. Finish'Last loop
                Finish(I) := Character'Val(0);
            end loop;
            if String_End = 0 then
               Ada.Text_Io.Skip_Line;
            end if;
            EmptyLines(EmptyList, BufferString, N, EmptyInSample, EmptyString, Main, EmptyCharList, NonEmptyCharList, StructWithEmptyLine, ASizedStruct, Finish);
         end;
      end;
   end;
end Main;
