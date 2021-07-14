
```haskell
data Tree = Leaf Int | Node Tree Tree

VertexCount :: Tree -> Int
VertexCount (Leaf _)   = 0
VertexCount (Node r l) = 1 + VertexCount r + VertexCount l

LeafCount :: Tree -> Int
LeafCount (Leaf _)   = 0
LeafCount (Node r l) = LeafCount r + LeafCount l
```


Prove that the identity A is valid for all x -> Tree
```haskell
LeafCount x = VertexCount x + 1 -- (A)
```

(Proof)
We will give a proof by structural induction on x.

- Base case:

When x = Leaf _,

```haskell
VertexCount x = 0
LeafCount x = 1
            = VertexCount x + 1
```

- Inductive step:

Let x = Node r l and suppose statement (A) holds for r and l.

```haskell
LeafCount x = LeafCount (Node r l)
            = LeafCount r + LeafCount l
            = VertexCount r + 1 + VertexCount l + 1
            = VertexCount (Node r l) + 1
            = VertexCount x + 1
```

Therefore proposition A stands for all x -> Tree.
QED
