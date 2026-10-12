with Ada.Streams;
with Skit_Representation;

package Skit
  with SPARK_Mode
is

   --  An Object is one word, in whichever representation this build of
   --  skit was configured with (ADR 0001, option D): a 32-bit tagged word
   --  by default, or a NaN-boxed 64-bit word. See alire.toml.

   Word_Size : constant := Skit_Representation.Word_Size;

   Max_Integer : constant := Skit_Representation.Max_Integer;
   Min_Integer : constant := Skit_Representation.Min_Integer;
   --  The integers an Object holds exactly: 30-bit in the 32-bit word,
   --  every Integer in the 64-bit one.

   type Object is private;
   type Object_Array is array (Positive range <>) of Object;

   function Is_Integer (X : Object) return Boolean;
   function To_Integer (X : Object) return Integer
     with Pre => Is_Integer (X);

   --  In the 64-bit word, Min_Integer .. Max_Integer is all of Integer,
   --  and the compiler warns that the test below is always true.
   pragma Warnings (Off, "range test optimized away");
   pragma Warnings (Off, "value is known to be in range");
   function To_Object (X : Integer) return Object
     with Post => Is_Integer (To_Object'Result)
                  and then (if Long_Long_Integer (X)
                                 in Min_Integer .. Max_Integer
                            then To_Integer (To_Object'Result) = X);
   pragma Warnings (On, "range test optimized away");
   pragma Warnings (On, "value is known to be in range");
   --  An integer outside Min_Integer .. Max_Integer wraps to the payload
   --  width, as Int arithmetic does; inside it, the value round-trips.

   --  The 64-bit word holds NaNs and infinities, which validity checking
   --  (-gnatVa) treats as invalid data, so it is off for the float
   --  conversions.
   pragma Validity_Checks (Off);

   function Is_Representable (X : Long_Float) return Boolean;
   --  Whether a float object can hold X. The 32-bit word holds a Float,
   --  so X has to fit in one; the 64-bit word holds any Long_Float.

   function To_Object (X : Float) return Object
     with Post => Is_Float (To_Object'Result);

   function To_Object (X : Long_Float) return Object
     with Pre  => Is_Representable (X),
          Post => Is_Float (To_Object'Result);

   function Is_Float (X : Object) return Boolean;
   function To_Float (X : Object) return Long_Float
     with Pre => Is_Float (X);

   pragma Validity_Checks (On);

   function Is_Application (X : Object) return Boolean;
   function Is_Symbol (X : Object) return Boolean;
   function Is_Undefined (X : Object) return Boolean;
   function Is_Foreign_Object (X : Object) return Boolean;
   function Is_Primitive_Function (X : Object) return Boolean;
   --  True for a bare reference to a host-bound primitive (a "foreign
   --  import ... #name" wrapper), before it is embedded in any application.
   --  Such an object is build-specific and can never be baked into an image
   --  directly (see skit/docs/adr/0002-external-skit-representation.md,
   --  decision B) -- a host that binds a name directly to one (rather than
   --  to an application built from it) should not export that name.

   type User_Data_Interface is limited interface;

   type Argument_Mode is (Strict, Lazy);
   type Argument_Mode_Array is array (Positive range <>) of Argument_Mode;

   type Primitive_Evaluator_Interface is interface;

   function Argument_Count
     (This : Primitive_Evaluator_Interface)
      return Natural
      is abstract;

   function Argument_Modes
     (This : Primitive_Evaluator_Interface)
      return Argument_Mode_Array
      is abstract
     with Post'Class => Argument_Modes'Result'First = 1
                        and then Argument_Modes'Result'Last
                                   = This.Argument_Count;
   --  One mode per argument, indexed from 1 like the arguments themselves.
   pragma Annotate
     (GNATprove, False_Positive,
      "contract of function might not be feasible",
      "an array 1 .. N exists for every Natural N; the prover cannot see"
      & " that through the dispatching call to Argument_Count");

   function Evaluate
     (This      : Primitive_Evaluator_Interface;
      User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
      is abstract
     with Pre'Class => Arguments'Length = Argument_Count (This);

   --  A foreign object is a host-owned value that lives in the Skit heap as a
   --  first-class node: it may hold Object children, is traced by the garbage
   --  collector, and can be serialized into a module image.  The host
   --  implements this interface, registers a deserializer factory under the
   --  object's Class_Name, and binds instances into a machine (which returns a
   --  Primitive-tagged Object referencing the instance).  See
   --  skit/docs/adr/0002-external-skit-representation.md.

   type Foreign_Object_Interface is limited interface;

   function Class_Name (This : Foreign_Object_Interface) return String
      is abstract;
   --  Stable type tag.  Written to the image so the object can be
   --  deserialized, and the key its deserializer factory is registered under.

   function Serialize (This : Foreign_Object_Interface)
      return Ada.Streams.Stream_Element_Array
      is abstract;
   --  The object's own opaque leaf bytes (may be empty).  Object children are
   --  NOT encoded here -- they are machine-local references that travel
   --  separately and relocate on load.

   procedure Visit
     (This    : in out Foreign_Object_Interface;
      Process : not null access procedure (Child : in out Object))
      is abstract;
   --  Call Process on every Object child, in a fixed, deterministic order.
   --  One traversal, two callers: the collector passes a Process that forwards
   --  the child and rewrites it in place (hence Child is in out); the image
   --  writer passes one that collects the children.  The order is a
   --  contract -- deserialization consumes the children positionally.
   --  Process is an anonymous access parameter so the caller may pass a nested
   --  forwarding closure (the collector's forward routine is local).

   procedure Free (This : in out Foreign_Object_Interface)
      is abstract;
   --  Called exactly once, when no live reference to the object remains;
   --  release any host resources here.

   function Image (This : Foreign_Object_Interface) return String
      is abstract;
   --  Per-instance rendering, used by the machine's debug printer.

   --  Reference and factory types for the host side.  The machine owns a bound
   --  reference: it calls Free and reclaims the object when no live reference
   --  remains.  A Deserializer reconstructs an object from its opaque bytes
   --  and its (already relocated) Object children.

   type Foreign_Reference is access all Foreign_Object_Interface'Class;

   type Deserializer is access function
     (Bytes    : Ada.Streams.Stream_Element_Array;
      Children : Object_Array)
      return Foreign_Reference;

private

   --  Object is the representation's word, and inherits its operations:
   --  Tag_Of, Word_Payload, Make_Word and the integer and float encodings.
   --  Those are the only code that knows the layout. Everything below is
   --  written in terms of them, and is the same for every representation.

   use all type Skit_Representation.Object_Tag;

   type Object_Payload is new Skit_Representation.Object_Payload;
   subtype Cell_Address is Object_Payload;

   Tag_Size   : constant := Skit_Representation.Tag_Size;
   Tag_Layout : constant := Skit_Representation.Tag_Layout;
   --  Recorded in a module image's header, with Word_Size; an image loads
   --  only into a build with the same representation.

   type Counter is mod 2 ** 64;
   --  For statistics only. Being modular, a counter cannot overflow and
   --  raise on a long run, and proof never has to bound it.

   type Object is new Skit_Representation.Word;

   --  Payload and Is_Application are all Skit.Memory knows of an Object,
   --  and the collector is proved knowing only what Application's
   --  postcondition says about them: that an application built from an
   --  address is an application with that payload. Their definitions,
   --  which are the representation's bit layout, are hidden from proof
   --  except where one of root Skit's own contracts is proved from them.
   --  Unfolding them inside the collector's quantified invariants makes
   --  its proof time out, and in the NaN-boxed word they are 64-bit
   --  division and modulus.

   function Payload (X : Object) return Object_Payload
   is (Object_Payload (Word_Payload (X)));
   pragma Annotate
     (GNATprove, Hide_Info, "Expression_Function_Body", Payload);
   --  The payload of a primitive or an application.

   function Make_Object
     (Tag     : Skit_Representation.Object_Tag;
      Payload : Object_Payload)
      return Object
   is (Make_Word (Tag, Skit_Representation.Object_Payload (Payload)))
     with Pre  => Tag in Primitive_Object | Application_Object,
          Post => Tag_Of (Make_Object'Result) = Tag
                  and then Skit.Payload (Make_Object'Result) = Payload;
   pragma Annotate
     (GNATprove, Unhide_Info, "Expression_Function_Body", Payload);

   Payload_Nil        : constant Object_Payload := 0;
   Payload_S          : constant Object_Payload := 1;
   Payload_K          : constant Object_Payload := 2;
   Payload_I          : constant Object_Payload := 3;
   Payload_C          : constant Object_Payload := 4;
   Payload_B          : constant Object_Payload := 5;
   Payload_S_Prime    : constant Object_Payload := 6;
   Payload_B_Star     : constant Object_Payload := 7;
   Payload_C_Prime    : constant Object_Payload := 8;
   Payload_Y          : constant Object_Payload := 9;
   Payload_Undefined  : constant Object_Payload := 10;
   Payload_Suspension : constant Object_Payload := 11;

   subtype Combinator_Payload is
     Object_Payload range Payload_S .. Payload_Y;

   function Is_Primitive (O : Object) return Boolean
   is (Tag_Of (O) = Primitive_Object);

   function Primitive (Payload : Object_Payload) return Object
   is (Make_Object (Primitive_Object, Payload))
     with Post => Is_Primitive (Primitive'Result)
                  and then Skit.Payload (Primitive'Result) = Payload;

   Nil        : constant Object := Primitive (Payload_Nil);
   S          : constant Object := Primitive (Payload_S);
   K          : constant Object := Primitive (Payload_K);
   I          : constant Object := Primitive (Payload_I);
   C          : constant Object := Primitive (Payload_C);
   B          : constant Object := Primitive (Payload_B);
   S_Prime    : constant Object := Primitive (Payload_S_Prime);
   B_Star     : constant Object := Primitive (Payload_B_Star);
   C_Prime    : constant Object := Primitive (Payload_C_Prime);
   Y          : constant Object := Primitive (Payload_Y);
   Undefined  : constant Object := Primitive (Payload_Undefined);
   Suspension : constant Object := Primitive (Payload_Suspension);

   subtype Primitive_Function_Payload is
     Object_Payload range 64 .. 4095;

   subtype Primitive_Variable_Payload is
     Object_Payload range 4096 .. 65535;

   --  Foreign objects occupy a distinct, higher payload band; the gap between
   --  the symbol range above and this one is reserved (room for symbols to
   --  grow).  ~1M slots, well within the 30-bit payload.

   subtype Foreign_Object_Payload is
     Object_Payload range 2 ** 20 .. 2 ** 21 - 1;

   function Is_Integer (X : Object) return Boolean
   is (Tag_Of (X) = Integer_Object);

   function To_Integer (X : Object) return Integer
   is (Decode_Integer (X));

   function To_Object (X : Integer) return Object
   is (Encode_Integer (X));

   pragma Validity_Checks (Off);  --  as for the declarations above

   function Is_Representable (X : Long_Float) return Boolean
   is (Skit_Representation.Is_Representable (X));

   function To_Object (X : Float) return Object
   is (Encode_Float (X));

   function To_Object (X : Long_Float) return Object
   is (Encode_Long_Float (X));

   function Is_Float (X : Object) return Boolean
   is (Tag_Of (X) = Float_Object);

   function To_Float (X : Object) return Long_Float
   is (Decode_Float (X));

   pragma Validity_Checks (On);

   function Is_Application (X : Object) return Boolean
   is (Tag_Of (X) = Application_Object);
   pragma Annotate
     (GNATprove, Hide_Info, "Expression_Function_Body", Is_Application);
   --  Hidden from proof, as Payload is (above).

   function Is_Symbol (X : Object) return Boolean
   is (Is_Primitive (X)
       and then Payload (X) in Primitive_Variable_Payload);

   function Is_Undefined (X : Object) return Boolean
   is (X = Undefined);

   function Is_Foreign_Object (X : Object) return Boolean
   is (Is_Primitive (X)
       and then Payload (X) in Foreign_Object_Payload);

   function Is_Combinator (X : Object) return Boolean
   is (Is_Primitive (X) and then Payload (X) in Combinator_Payload);

   --  The combinators, independently of how the representation encodes
   --  them: code outside the representation names a combinator by this
   --  type, never by its payload. (Nil, Undefined and Suspension are
   --  special objects, compared for equality.) The combinator payloads are
   --  Payload_S .. Payload_Y in this same order, so both directions are
   --  arithmetic.

   type Combinator_Kind is
     (Comb_S, Comb_K, Comb_I, Comb_C, Comb_B,
      Comb_S_Prime, Comb_B_Star, Comb_C_Prime, Comb_Y);

   function Combinator_Of (X : Object) return Combinator_Kind
   is (Combinator_Kind'Val (Payload (X) - Payload_S))
     with Pre => Is_Combinator (X);

   function To_Object (C : Combinator_Kind) return Object
   is (Primitive (Payload_S + Combinator_Kind'Pos (C)))
     with Post => Is_Combinator (To_Object'Result)
                  and then Combinator_Of (To_Object'Result) = C;

   function Is_Primitive_Function (X : Object) return Boolean
   is (Is_Primitive (X) and then Payload (X) in Primitive_Function_Payload);

   --  Constructors: the only sanctioned way to build an Object outside the
   --  top-level Skit package, so the tag/payload layout stays private here.

   function Application (Address : Object_Payload) return Object
   is (Make_Object (Application_Object, Address))
     with Post => Is_Application (Application'Result)
                  and then Payload (Application'Result) = Address;
   pragma Annotate
     (GNATprove, Unhide_Info, "Expression_Function_Body", Is_Application);

   function Address (X : Object) return Cell_Address
   is (Payload (X))
     with Pre => Is_Application (X);
   --  The cell an application refers to. Outside root Skit and Skit.Memory,
   --  use this rather than Payload.

   --  Each kind of reference below is a base payload plus an index. The
   --  arithmetic is modular, so an index past the end of its band would
   --  land silently in the next one; the preconditions rule that out.

   --  A primitive function is stored as base + its zero-based slot in the
   --  machine's primitive table; the index round-trips through these two.

   Max_Primitive_Function_Index : constant :=
     Primitive_Function_Payload'Last - Primitive_Function_Payload'First;

   function Primitive_Function (Index : Natural) return Object
   is (Primitive (Primitive_Function_Payload'First + Object_Payload (Index)))
     with Pre  => Index <= Max_Primitive_Function_Index,
          Post => Is_Primitive_Function (Primitive_Function'Result);

   function Primitive_Function_Index (X : Object) return Natural
   is (Natural (Payload (X) - Primitive_Function_Payload'First))
     with Pre => Is_Primitive_Function (X);

   --  A symbol is base + its index in the handle's symbol vector.

   Max_Symbol_Index : constant :=
     Primitive_Variable_Payload'Last - Primitive_Variable_Payload'First;

   function Symbol (Index : Natural) return Object
   is (Primitive (Primitive_Variable_Payload'First + Object_Payload (Index)))
     with Pre  => Index <= Max_Symbol_Index,
          Post => Is_Symbol (Symbol'Result);

   function Symbol_Index (X : Object) return Natural
   is (Natural (Payload (X) - Primitive_Variable_Payload'First))
     with Pre => Is_Symbol (X);

   --  A foreign object is base + its slot in the machine's foreign-object
   --  registry; the slot round-trips through these two.

   Max_Foreign_Object_Index : constant :=
     Foreign_Object_Payload'Last - Foreign_Object_Payload'First;

   function Foreign_Object (Index : Natural) return Object
   is (Primitive (Foreign_Object_Payload'First + Object_Payload (Index)))
     with Pre  => Index <= Max_Foreign_Object_Index,
          Post => Is_Foreign_Object (Foreign_Object'Result);

   function Foreign_Object_Index (X : Object) return Natural
   is (Natural (Payload (X) - Foreign_Object_Payload'First))
     with Pre => Is_Foreign_Object (X);

end Skit;
