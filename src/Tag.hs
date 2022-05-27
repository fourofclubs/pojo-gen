module Tag where

import Gen

tagClss :: Class
tagClss = Class "Tag" [tagIdFld, tagTextFld] "A toolset tag"

tagIdFld = fld "id" "BigInteger" "The tag id"
tagTextFld = fld "text" "String" "The tag text"