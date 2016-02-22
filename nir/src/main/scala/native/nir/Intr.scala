package native
package nir

import Type._

object Intr {
  private def value(id: String, ty: Type) =
    Val.Global(Global.intrinsic(id), Type.Ptr(ty))
  private def intrinsic(id: String, args: Seq[Type], ret: Type) =
    Val.Global(Global.intrinsic(id.split("_"): _*), Type.Ptr(Type.Function(args, ret)))
  private def nullary(id: String, to: Type) =
    intrinsic(id, Seq(), to)
  private def unary(id: String, from: Type, to: Type) =
    intrinsic(id, Seq(from), to)
  private def binary(id: String, from1: Type, from2: Type, to: Type) =
    intrinsic(id, Seq(from1, from2), to)
  private def ternary(id: String, from1: Type, from2: Type, from3: Type, to: Type) =
    intrinsic(id, Seq(from1, from2, from3), to)
  private def cls(id: String) =
    Type.Class(Global.intrinsic(id))

  def call(intr: Val.Global, args: Val*): Op = {
    val Val.Global(_, Type.Ptr(ty)) = intr
    Op.Call(ty, intr, args)
  }

  lazy val object_          = cls   ("object")
  lazy val object_init      = unary ("object_init"    , object_,          Unit   )
  lazy val object_equals    = binary("object_equals"  , object_, object_, Bool   )
  lazy val object_toString  = unary ("object_toString", object_,          string )
  lazy val object_hashCode  = unary ("object_hashCode", object_,          I32    )
  lazy val object_getType   = unary ("object_getType" , object_,          type_  )

  lazy val monitor_enter     = unary  ("monitor_enter"    , object_,           Unit  )
  lazy val monitor_exit      = unary  ("monitor_exit"     , object_,           Unit  )
  lazy val monitor_notify    = unary  ("monitor_notify"   , object_,           Unit  )
  lazy val monitor_notifyAll = unary  ("monitor_notifyAll", object_,           Unit  )
  lazy val monitor_wait      = ternary("monitor_wait"     , object_, I64, I32, Unit  )

  lazy val bool_array   = cls("bool_array")
  lazy val char_array   = cls("char_array")
  lazy val byte_array   = cls("byte_array")
  lazy val short_array  = cls("short_array")
  lazy val int_array    = cls("int_array")
  lazy val long_array   = cls("long_array")
  lazy val float_array  = cls("float_array")
  lazy val double_array = cls("double_array")
  lazy val object_array = cls("object_array")

  lazy val type_     = cls  ("type")
  lazy val type_name = unary("type_name", Type.Ptr(type_), string)
  lazy val type_size = unary("type_size", Type.Ptr(type_), Size  )

  lazy val null_type    = value ("null_type"   , type_)
  lazy val nothing_type = value ("nothing_type", type_)
  lazy val object_type  = value ("object_type" , type_)
  lazy val type_type    = value ("type_type"   , type_)
  lazy val bool_type    = value ("bool_type"   , type_)
  lazy val char_type    = value ("char_type"   , type_)
  lazy val byte_type    = value ("byte_type"   , type_)
  lazy val short_type   = value ("short_type"  , type_)
  lazy val int_type     = value ("int_type"    , type_)
  lazy val long_type    = value ("long_type"   , type_)
  lazy val float_type   = value ("float_type"  , type_)
  lazy val double_type  = value ("double_type" , type_)
  lazy val string_type  = value ("string_typr" , type_)

  lazy val bool_array_type   = value("bool_array_type"  , type_)
  lazy val char_array_type   = value("char_array_type"  , type_)
  lazy val byte_array_type   = value("byte_array_type"  , type_)
  lazy val short_array_type  = value("short_array_type" , type_)
  lazy val int_array_type    = value("int_array_type"   , type_)
  lazy val long_array_type   = value("long_array_type"  , type_)
  lazy val float_array_type  = value("float_array_type" , type_)
  lazy val double_array_type = value("double_array_type", type_)
  lazy val object_array_type = value("object_array_type", type_)

  lazy val bool          = cls  ("bool")
  lazy val bool_box      = unary("bool_box"     , Bool, bool  )
  lazy val bool_unbox    = unary("bool_unbox"   , bool, Bool  )
  lazy val bool_toString = unary("bool_toString", Bool, string)
  lazy val bool_parse    = unary("bool_parse"   , Bool, string)
  lazy val bool_hashCode = unary("bool_hashCode", Bool, I32   )

  lazy val char          = cls  ("char")
  lazy val char_box      = unary("char_box"      , I16 , char  )
  lazy val char_unbox    = unary("char_unbox"    , char, I16   )
  lazy val char_toString = unary("char_toString" , I16 , string)
  lazy val char_hashCode = unary("char_hashCode" , I16 , I32   )

  lazy val byte           = cls   ("byte")
  lazy val byte_box       = unary ("byte_box"      , I8  ,      byte  )
  lazy val byte_unbox     = unary ("byte_unbox"    , byte,      I8    )
  lazy val byte_toString  = unary ("byte_toString" , I8  ,      string)
  lazy val byte_parse     = unary ("byte_parse"    , I8  ,      string)
  lazy val byte_parse_rdx = binary("byte_parse_rdx", I8  , I32, string)
  lazy val byte_hashCode  = unary ("byte_hashCode" , I8  ,      I32   )

  lazy val short           = cls   ("short")
  lazy val short_box       = unary ("short_box"      , I16  ,      short )
  lazy val short_unbox     = unary ("short_unbox"    , short,      I16   )
  lazy val short_toString  = unary ("short_toString" , I16  ,      string)
  lazy val short_parse     = unary ("short_parse"    , I16  ,      string)
  lazy val short_parse_rdx = binary("short_parse_rdx", I16  , I32, string)
  lazy val short_hashCode  = unary ("short_hashCode" , I16  ,      I32   )

  lazy val int                      = cls   ("int")
  lazy val int_box                  = unary ("int_box"                 , I32,      int   )
  lazy val int_unbox                = unary ("int_unbox"               , int,      I32   )
  lazy val int_toString             = unary ("int_toString"            , I32,      string)
  lazy val int_toUnsignedString     = unary ("int_toUnsignedString"    , I32,      string)
  lazy val int_toString_rdx         = binary("int_toString_rdx"        , I32, I32, string)
  lazy val int_toUnsignedString_rdx = binary("int_toUnsignedString_rdx", I32, I32, string)
  lazy val int_parse                = unary ("int_parse"               , I32,      string)
  lazy val int_parse_rdx            = binary("int_parse_rdx"           , I32, I32, string)
  lazy val int_parseUnsigned        = unary ("int_parseUnsigned"       , I32,      string)
  lazy val int_parseUnsigned_rdx    = binary("int_parseUnsigned_rdx"   , I32, I32, string)
  lazy val int_hashCode             = unary ("int_hashCode"            , I32,      I32   )

  lazy val long                      = cls   ("long")
  lazy val long_box                  = unary ("long_box"                 , I64,      int   )
  lazy val long_unbox                = unary ("long_unbox"               , int,      I64   )
  lazy val long_toString             = unary ("long_toString"            , I64,      string)
  lazy val long_toUnsignedString     = unary ("long_toUnsignedString"    , I64,      string)
  lazy val long_toString_rdx         = binary("long_toString_rdx"        , I64, I32, string)
  lazy val long_toUnsignedString_rdx = binary("long_toUnsignedString_rdx", I64, I32, string)
  lazy val long_parse                = unary ("long_parse"               , I64,      string)
  lazy val long_parse_rdx            = binary("long_parse_rdx"           , I64, I32, string)
  lazy val long_parseUnsigned        = unary ("long_parseUnsigned"       , I64,      string)
  lazy val long_parseUnsigned_rdx    = binary("long_parseUnsigned_rdx"   , I64, I32, string)
  lazy val long_hashCode             = unary ("long_hashCode"            , I64,      I32   )

  lazy val float          = cls  ("float")
  lazy val float_box      = unary("float_box"     , F32  , float )
  lazy val float_unbox    = unary("float_unbox"   , float, F32   )
  lazy val float_toString = unary("float_toString", F32  , string)
  lazy val float_parse    = unary("float_parse"   , F32  , string)
  lazy val float_hashCode = unary("float_hashCode", F32  , I32   )

  lazy val double          = cls  ("double")
  lazy val double_box      = unary("double_box"     , F64   , double)
  lazy val double_unbox    = unary("double_unbox"   , double, F64   )
  lazy val double_toString = unary("double_toString", F64   , string)
  lazy val double_parse    = unary("double_parse"   , F64   , string)
  lazy val double_hashCode = unary("double_hashCode", F64   , I32   )

  lazy val string         = cls   ("string")
  lazy val string_charAt  = binary("string_charAt",  string,  I32,      I16   )
  lazy val string_concat  = binary("string_concat",  string,  string,   string)
  lazy val string_length  = unary ("string_length",  string,            I32   )

  lazy val alloc  = binary ("alloc", Type.Ptr(type_), Size, object_)
  lazy val init   = binary ("init" , Type.I32, Type.Ptr(Type.Ptr(Type.I8)), ArrayClass(string))
  lazy val yield_ = nullary("yield", Unit)
  lazy val throw_ = unary  ("throw", object_, Type.Nothing)

  lazy val array = Map[Type, Type](
    bool    -> bool_array  ,
    char    -> char_array  ,
    byte    -> byte_array  ,
    short   -> short_array ,
    int     -> int_array 	 ,
    long    -> long_array  ,
    float   -> float_array ,
    double  -> double_array,
    object_ -> object_array
  )

  lazy val box = Map[Type, Val.Global](
    bool   -> bool_box  ,
    char   -> char_box  ,
    byte   -> byte_box  ,
    short  -> short_box ,
    int    -> int_box   ,
    long   -> long_box  ,
    float  -> float_box ,
    double -> double_box
  )

  lazy val unbox = Map[Type, Val.Global](
    bool   -> bool_unbox  ,
    char   -> char_unbox  ,
    byte   -> byte_unbox  ,
    short  -> short_unbox ,
    int    -> int_unbox   ,
    long   -> long_unbox  ,
    float  -> float_unbox ,
    double -> double_unbox
  )

  lazy val toString_ = Map[Type, Val.Global](
    object_ -> object_toString,
    bool    -> bool_toString  ,
    char    -> char_toString  ,
    byte    -> byte_toString  ,
    short   -> short_toString ,
    int     -> int_toString   ,
    long    -> long_toString  ,
    float   -> float_toString ,
    double  -> double_toString
  )

  lazy val toUnsignedString = Map[Type, Val.Global](
    int  -> int_toUnsignedString ,
    long -> long_toUnsignedString
  )

  lazy val toString_rdx = Map[Type, Val.Global](
    int  -> int_toString_rdx ,
    long -> long_toString_rdx
  )

  lazy val toUnsignedString_rdx = Map[Type, Val.Global](
    int  -> int_toUnsignedString_rdx ,
    long -> long_toUnsignedString_rdx
  )

  lazy val parse = Map[Type, Val.Global](
    bool   -> bool_parse  ,
    byte   -> byte_parse  ,
    short  -> short_parse ,
    int    -> int_parse   ,
    long   -> long_parse  ,
    float  -> float_parse ,
    double -> double_parse
  )

  lazy val parseUnsigned = Map[Type, Val.Global](
    int  -> int_parseUnsigned ,
    long -> long_parseUnsigned
  )

  lazy val parse_rdx = Map[Type, Val.Global](
    byte  -> byte_parse_rdx ,
    short -> short_parse_rdx,
    int   -> int_parse_rdx  ,
    long  -> long_parse_rdx
  )

  lazy val parseUnsigned_rdx = Map[Type, Val.Global](
    int  -> int_parseUnsigned_rdx,
    long -> long_parseUnsigned_rdx
  )

  lazy val hashCode_ = Map[Type, Val.Global](
    bool   -> bool_hashCode  ,
    char   -> char_hashCode  ,
    byte   -> byte_hashCode  ,
    short  -> short_hashCode ,
    int    -> int_hashCode   ,
    long   -> long_hashCode  ,
    float  -> float_hashCode ,
    double -> double_hashCode
  )

  lazy val intrinsic_type = Map[Type, Val.Global](
    Type.Null    -> null_type   ,
    Type.Nothing -> nothing_type,
    object_      -> object_type ,
    type_        -> type_type   ,
    bool         -> bool_type   ,
    char         -> char_type   ,
    byte         -> byte_type   ,
    short        -> short_type  ,
    int          -> int_type    ,
    long         -> long_type   ,
    float        -> float_type  ,
    double       -> double_type
  )

  lazy val array_type = Map[Type, Val.Global](
    bool    -> bool_array_type  ,
    char    -> char_array_type  ,
    byte    -> byte_array_type  ,
    short   -> short_array_type ,
    int     -> int_array_type   ,
    long    -> long_array_type  ,
    float   -> float_array_type ,
    double  -> double_array_type,
    object_ -> object_array_type
  )

  lazy val layout = Map[Global, Seq[Type]](
    bool_array.name   -> Seq(Ptr(I8), I32, Array(Bool, 0))   ,
    char_array.name   -> Seq(Ptr(I8), I32, Array(I16, 0))    ,
    byte_array.name   -> Seq(Ptr(I8), I32, Array(I8, 0))     ,
    short_array.name  -> Seq(Ptr(I8), I32, Array(I16, 0))    ,
    int_array.name    -> Seq(Ptr(I8), I32, Array(I32, 0))    ,
    long_array.name   -> Seq(Ptr(I8), I32, Array(I64, 0))    ,
    float_array.name  -> Seq(Ptr(I8), I32, Array(F32, 0))    ,
    double_array.name -> Seq(Ptr(I8), I32, Array(F64, 0))    ,
    object_array.name -> Seq(Ptr(I8), I32, Array(Ptr(I8), 0)),
    type_.name        -> Seq(Ptr(I8))                        ,
    string.name       -> Seq(Ptr(I8), I32, Ptr(I8))
  )
}
