#!/bin/sh

# combine dependencies into one javascript file for each category

uhc="
/usr/local/lib//uhc-1.1.3/lib/js/libEH-RTS.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Base.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_BoxArray.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Char.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Enum.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Float.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_MutVar.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Read.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Real.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_ST.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_STRef.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Show.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_StackTrace.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Types.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Ptr.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_ByteArray.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_IOBase.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_OldException.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_OldIO.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Run.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/Generics/UHC_Generics_Tuple.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Generics.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Bounded.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Eq.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Ord.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Ix.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/UHC/UHC_Array.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/Data/Data_Maybe.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/Data/Data_Char.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/Data/Data_List.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/uhcbase-1.1.3/uhc-1.1.3/js/plain/Control/Control_Monad.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/base-3.0.0.0/uhc-1.1.3/js/plain/Prelude.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/base-3.0.0.0/uhc-1.1.3/js/plain/Control/Control_Category.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/base-3.0.0.0/uhc-1.1.3/js/plain/Control/Monad/Control_Monad_Instances.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/base-3.0.0.0/uhc-1.1.3/js/plain/Data/Data_Function.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/base-3.0.0.0/uhc-1.1.3/js/plain/Control/Monad/Control_Monad_Fix.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/base-3.0.0.0/uhc-1.1.3/js/plain/Control/Control_Arrow.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/base-3.0.0.0/uhc-1.1.3/js/plain/Data/Data_Monoid.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/base-3.0.0.0/uhc-1.1.3/js/plain/Control/Control_Applicative.mjs
/usr/local/lib//uhc-1.1.3/lib/pkg/base-3.0.0.0/uhc-1.1.3/js/plain/Data/Data_Foldable.mjs
"

uhcjs="
lib/uhc-js/uhc-js/src/Language/UHC/JScript/Language_UHC_JScript_Types.mjs
lib/uhc-js/uhc-js/src/Language/UHC/JScript/ECMA/Language_UHC_JScript_ECMA_String.mjs
lib/uhc-js/uhc-js/src/Language/UHC/JScript/Language_UHC_JScript_Primitives.mjs
lib/uhc-js/uhc-js/src/Language/UHC/JScript/Language_UHC_JScript_Assorted.mjs
lib/uhc-js/uhc-js/src/Language/UHC/JScript/Language_UHC_JScript_Prelude.mjs
lib/uhc-js/uhc-js/src/Language/UHC/JScript/ECMA/Language_UHC_JScript_ECMA_Array.mjs
lib/uhc-js/uhc-js/src/Language/UHC/JScript/ECMA/Language_UHC_JScript_ECMA_Expressions.mjs
lib/uhc-js/uhc-js/src/Language/UHC/JScript/W3C/Language_UHC_JScript_W3C_HTML5.mjs
"

app="
src/Data/Data_Map.mjs
src/Data/Data_Set.mjs
src/PropositionalLogic/PropositionalLogic_Logic.mjs
src/PropositionalLogic/PropositionalLogic_Parser.mjs
src/PropositionalLogic.mjs
src/App.mjs
"


# > uhc.js
# for f in $uhc ; do
#   cat $f >> uhc.js
# done

> uhcjs.js
for f in $uhcjs ; do
  cat $f >> uhcjs.js
done

> app.js
for f in $app ; do
  cat $f >> app.js
done