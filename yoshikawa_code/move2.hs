import ListMaker2

main = do
	putStrLn "LHS of n = 1\n"
	print $ left 1
	putStrLn "\n\n"
	putStrLn "RHS of n = 1\n"
	print $ right 1
	putStrLn "\n\n"
	putStrLn "LHS of n = 2\n"
	print $ left 2
	putStrLn "\n\n"
	putStrLn "RHS of n = 2\n"
	print $ right 2
	putStrLn "\n\n"
	putStrLn "LHS of n = 3\n"
	print $ left 3
	putStrLn "\n\n"
	putStrLn "RHS of n = 3\n"
	print $ right 3
	putStrLn "\n\n"
	putStrLn "LHS of n = 4\n"
	print $ left 4
	putStrLn "\n\n"
	putStrLn "RHS of n = 4\n"
	print $ right 4




left n = move2 n
right n = map (\x -> tail x) $ move2 n

