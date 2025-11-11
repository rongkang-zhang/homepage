-- CategoryDemo.hs: Haskell中的函子实例示例
import Data.Functor (Functor(..))

-- 自定义数据类型模拟"树"结构
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

-- 为Tree实现Functor实例（函子）
instance Functor Tree where
    -- fmap: (a→b) → Tree a → Tree b（态射映射）
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- 测试：函子保持态射复合
main :: IO ()
main = do
    let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
        f = (+1)    -- 态射f: Int→Int
        g = (*2)    -- 态射g: Int→Int
    -- 验证 fmap (g . f) = fmap g . fmap f（函子性质）
    print $ fmap (g . f) tree  -- Node (Leaf 4) (Node (Leaf 6) (Leaf 8))
    print $ (fmap g . fmap f) tree  -- 与上一行结果相同