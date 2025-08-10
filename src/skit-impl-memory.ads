with Skit.Marks;
with Skit.Allocator;

private package Skit.Impl.Memory is

   function Create
     (Core_Size  : Positive;
      Marks      : not null access Skit.Marks.Abstraction'Class)
      return Skit.Allocator.Reference;

end Skit.Impl.Memory;
