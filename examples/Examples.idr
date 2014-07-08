module Examples

import Edda.Model
import Edda.Utils

mdata : Maybe Properties
mdata = Just [(Title, "I am a title"), (Author, "Anne Other")]

text : Inline
text = Serif "I am a sentence."

text1 : Inline
text1 = Serif "I am a second sentence."

para : Block
para = Plain [text, text1]

sect : Block
sect = MkSection (Just "sect1") (Just [Serif "Introduction"]) [para]

myFirstEdda : Edda
myFirstEdda = MkEdda mdata [sect, para]
