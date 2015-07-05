module internal Ferop.CConversion

open System.Reflection
open CTypedAST

type CConvInfo = 
    { 
        Name: string
        ImportedFunctions: MethodInfo list
        ExportedFunctions: MethodInfo list
        IsCpp: bool
    }


val makeCEnv : CConvInfo -> CEnv
