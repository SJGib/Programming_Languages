merge a b i j = 
	if ((length a) == 0) || ((length b) == 0)
	then a ++ b
	else 
		if ((length a) == i) && ((length b) == j)
		then []
		else
			if ((length a) == i)
			then
				(b!!j):(merge a b i (j+1))
			else
				if ((length b) == j)
				then
					(a!!i):(merge a b (i+1) j)
				else
					if (a!!i) < (b!!j)
					then
						(a!!i):(merge a b (i+1) j)
					else
						(b!!j):(merge a b i (j+1))

mergeSort l = 
	if (length l) > 2
	then merge (
		mergeSort (take (div (length l) 2) l)) (
		mergeSort (drop (div (length l) 2) l)) 0 0
	else
		if (length l) < 2
		then l
		else
			if (head l) > l!!1
			then [l!!1,(head l)]
			else l