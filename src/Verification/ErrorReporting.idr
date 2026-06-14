||| Tools to recollect validation errors on a HTML element
module Verification.ErrorReporting

import Util.Application
import Core.Condition
import Core.Tags
import Core.Specification
import Verification.Restrictions
import Verification.Attributes
import Verification.ContentModel
import Core.Node
import Util.Tree
import Data.String
import Core.Attributes

||| Type for validation errors
public export
data Error =
      ViolatedRestriction ConditionError 
    | InvalidAttribute Attribute
    | FailedContentModel ContentModel

||| A Tree of (node,errors) pairs
||| These trees are the output of the validation algorithm
public export
ErrorTree : Type
ErrorTree = Tree (NodePosition, List Error)

||| Returns True if the node can't be validated by the HTML Spec
||| (SVG and MathML elements)
public export
skipValidation: NodePosition -> Bool
skipValidation node = (node `is` SVG) || (node `is` Math)

||| Given a node, it returns the list of validation errors it contains.
||| It doesn't verify its children
public export
errors' : NodePosition -> List Error
errors' node =
    if skipValidation node then [] else
        
    (
        case validate node of
            Nothing => []
            Just model => [FailedContentModel model]
    )
    ++
    map ViolatedRestriction (Restrictions.validate node)
    ++  
    map InvalidAttribute (Attributes.validate (focus node))

public export
errors: Node -> List Error
errors = fromTree .> errors'