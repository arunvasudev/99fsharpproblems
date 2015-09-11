module fsharp99

type Tree<'a> = 
    | Empty 
    | Branch of 'a * Tree<'a> * Tree<'a>
