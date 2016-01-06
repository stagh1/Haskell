data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Eq)

isBinaryTree Empty = True
isBinaryTree tree@(Node val left right)
       | left == Empty && right == Empty = True
       | left == Empty = val < valueOf right && isBinaryTree right
       | right == Empty = val > valueOf left && isBinaryTree left
       | left /= Empty && right /= Empty = (val < (valueOf right)) && (val > (valueOf left)) && isBinaryTree left && isBinaryTree right

insert el Empty = Node el Empty Empty
insert el tree@(Node val left right)
       | val == el = tree
       | val > el = Node val (insert el left) right
       | val < el = Node val left (insert el right)

search el Empty = False
search el tree@(Node val left right)
       | val == el = True
       | val > el = search el left
       | val < el = search el right

remove el Empty = Empty
remove el tree@(Node val left right)
       | el == val = deleteElement el (Node val left right)
       | el < val = Node val (remove el left) right
       | el > val = Node val left (remove el right)

fromList [] tree = tree
fromList (first : rest) tree = fromList rest (insert first tree)
