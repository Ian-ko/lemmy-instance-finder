import Parser

content_extractor_core :: [Char] -> [Char] -> [[Char]]
content_extractor_core word [] = [word]
content_extractor_core word (x:xs) | x=='\n' = word:(content_extractor_core [] xs)
                                   | True = content_extractor_core (word++[x]) xs

content_extractor :: [Char] -> [[Char]] 
content_extractor input = (content_extractor_core [] input) -- >>= no_space

get_title :: Int -> [[Char]] -> [[Char]]
get_title _ [] = []
get_title count (x:xs) | count == 3 = x:(get_title 1 xs)
                       | count == 0 = x:(get_title (count+1) xs)
                       | True = get_title (count+1) xs

get_url :: Int -> [[Char]] -> [[Char]]
get_url _ [] = []
get_url count (x:xs) | count == 4 = x:(get_url 2 xs)
                     | count == 1 = x:(get_url (count+1) xs)
                     | True = get_url (count+1) xs

get_desc :: Int -> [[Char]] -> [[Char]]
get_desc _ [] = []
get_desc count (x:xs) | count == 5 = x:(get_desc 3 xs)
                      | count == 2 = x:(get_desc (count+1) xs)
                      | True = get_desc (count+1) xs

add_index :: Int -> [[a]] -> [(Int,[a])]
add_index _ [] = []
add_index acc (x:xs) = (acc,x) : (add_index (acc+1) xs)

index_finder :: [Char] -> [(Int,[Char])] -> [Int]
index_finder pat input = input >>= (index_finder_util pat) 

index_finder_util :: [Char] -> (Int,[Char]) -> [Int]
index_finder_util pat input | multi_word_finder_lowercase pat (snd input) = [fst input]
                            | True = []

get_data :: [(Int,[Char])] -> Int -> [Char]
get_data input index = input >>= comp
 where
 comp input | (fst input) == index = (snd input) ++ ['\n']
            | True = []

main :: IO ()
main = do
 putStr "input file name\n"
 file_name <- getLine
 putStr "reading file\n"
 file_data  <- readFile file_name
 loop file_data


loop :: [Char] -> IO () 
loop file_data = do 
 putStr "input keywords\n"
 keywords <- getLine 
 putStr "parsing data\n"
 let contents = content_extractor file_data
 putStr "sorting data\n"
 let title = get_title 0 contents
 let url = get_url 0 contents
 let desc = get_desc 0 contents
 putStr "adding markers\n"
 let index_title = add_index 0 title
 let index_url = add_index 0 url
 let index_desc = add_index 0 desc
 putStr "parsing content\n"
 let matches = index_finder  keywords index_desc
 putStr "fectching names\n"
 putStr "--------instance names-----\n"
 putStr (matches >>= (get_data index_title))
 putStr "---------links----------\n"
 putStr (matches >>= (get_data index_url))
 
 putStr "quit [y/n]\n"
 quit <- (fmap head (getLine))
 if quit == 'y' then
  return ()
 else
  loop file_data
