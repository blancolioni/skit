with Skit.Allocator;

private package Skit.Impl.Memory is

   function Create
     (Core_Size  : Positive)
      return Skit.Allocator.Reference;

end Skit.Impl.Memory;
