import Data.List

findNextLetter curry letter table = let letterIndex = (elemIndices letter table)
                                        in last (take (curry + head letterIndex + 1) table)

encryptOneLetter key letter
        | letter >= 'a' && letter <= 'z' = findNextLetter key letter (cycle ['a'..'z'])
        | letter >= 'A' && letter <= 'Z' = findNextLetter key letter (cycle ['A'..'Z'])
      	| otherwise = letter

encrypt key text = map (encryptOneLetter key) text
 
decrypt key text = encrypt ((length (['a'..'z']++['A'..'Z'])) - key) text
