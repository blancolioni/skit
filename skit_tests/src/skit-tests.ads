package Skit.Tests is

   procedure Initialize;

   procedure Test
     (Source   : String;
      Expected : Object);

   procedure Test
     (Source   : String;
      Expected : Integer);

   procedure Test
     (Source   : String;
      Expected : String);

   procedure Report;

end Skit.Tests;
