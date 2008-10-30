module HSP.HJScript where

import HSP.Monad
import HSP.XML
import HSP.XMLGenerator
import HJScript -- hiding (( := )(..), genElement, genEElement, asAttr, asChild)

import qualified HSX.XMLGenerator as HSX

instance Monad m => EmbedAsChild (HSPT' m) (Block t) where
  asChild b = asChild $
    <script type="text/javascript">
      <% show b %>
    </script>

instance Monad m => EmbedAsChild (HSPT' m) (HJScript ()) where
  asChild script = asChild . snd $ evalHJScript script

instance Monad m => IsAttrValue m (Block t) where
  toAttrValue block = return . attrVal $ "javascript:" ++ show block

instance Monad m => IsAttrValue m (HJScript ()) where
  toAttrValue script = toAttrValue . snd $ evalHJScript script

instance Monad m => IsAttrValue m (HJScript (Exp t)) where
  toAttrValue script = toAttrValue $ evaluateHJScript script

newGlobalVar :: HSP (Var t, Block ())
newGlobalVar = do
  varName <- newGlobVarName
  let block = toBlock $ VarDecl varName
  return (JVar varName, block)

newGlobalVarWith :: Exp t -> HSP (Var t, Block ())
newGlobalVarWith e = do
  varName <- newGlobVarName
  let block = toBlock $ VarDeclAssign varName e
  return (JVar varName, block)

newGlobVarName :: HSP String
newGlobVarName = do
  r <- getIncNumber
  return $ "global_" ++ (show r)


type ElemRef = String
{- 
-- This should be done Soon (TM), but we 
-- need to look over things like getElementById,
-- so it'll have to wait until the next version.
   
newtype ElemRef = ElemRef String

instance IsAttrValue ElemRef where
 toAttrValue (ElemRef s) = toAttrValue s

instance ToAttrNodeValue ElemRef where
 toAttrNodeValue (ElemRef s) = toAttrValue s
-}

genId :: HSP ElemRef
genId = do
  r <- getIncNumber
  return $ "elemId_" ++ (show r)

ref :: HSP XML -> HSP (ElemRef, XML)
ref xml = do
  i    <- genId
  xml' <- xml `setId` i
  return (i, xml') 


-------------------------------------------------
-- Settting properties
-------------------------------------------------

setId :: (HSX.SetAttr m xml, HSX.EmbedAsAttr m (Attr String v)) 
        => xml -> v -> HSX.XMLGenT m (HSX.XML m)
setId e v = e `set` ("id" := v)

-- Generel method for adding 'onEvent' attributes to XML elements.
onEvent :: (HSX.SetAttr m xml, HSX.EmbedAsAttr m (Attr String (HJScript t)))
        => Event -> xml -> HJScript t -> HSX.XMLGenT m (HSX.XML m)
onEvent event xml script = xml `set` (showEvent event := script)

onAbort, onBlur, onChange, onClick, onDblClick, onError,
  onFocus, onKeyDown, onKeyPress, onKeyUp, onLoad, onMouseDown,
  onMouseMove, onMouseOut, onMouseOver, onMouseUp, onReset,
  onResize, onSelect, onSubmit, onUnload :: 
    (HSX.SetAttr m xml, HSX.EmbedAsAttr m (Attr String (HJScript t)))
        => xml -> HJScript t -> HSX.XMLGenT m (HSX.XML m)

onAbort     = onEvent OnAbort
onBlur      = onEvent OnBlur
onChange    = onEvent OnChange
onClick     = onEvent OnClick
onDblClick  = onEvent OnDblclick
onError     = onEvent OnError
onFocus     = onEvent OnFocus
onKeyDown   = onEvent OnKeyDown
onKeyPress  = onEvent OnKeyPress
onKeyUp     = onEvent OnKeyUp
onLoad      = onEvent OnLoad
onMouseDown = onEvent OnMouseDown
onMouseMove = onEvent OnMouseMove
onMouseOut  = onEvent OnMouseOut
onMouseOver = onEvent OnMouseOver
onMouseUp   = onEvent OnMouseUp
onReset     = onEvent OnReset
onResize    = onEvent OnResize
onSelect    = onEvent OnSelect
onSubmit    = onEvent OnSubmit
onUnload    = onEvent OnUnload
