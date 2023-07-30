module Parser where
import Data.Char
{-# LANGUAGE ParallelListComp #-}

text_match :: Eq a => [a] -> [a] -> Bool
text_match pat input | (length pat) == (length input) = ls_and char_match
                     | True = False
                     where
                     char_match = [x==y | x<-pat | y<-input]

ls_and :: [Bool] -> Bool
ls_and [] = True
ls_and (x:xs) = x && (ls_and xs)

smart_new_line_char :: [Char]
smart_new_line_char = ['.','-','_','!','?','"',',','(','[','{',')',']','}','\\']

word_extractor_core :: [Char] -> [Char] -> [[Char]]
word_extractor_core word [] = [word]
word_extractor_core word (x:xs) | (elem x smart_new_line_char) && ((safe_head xs) == ' ') = word:(word_extractor_core [] xs)
                                | x == ' ' = word:(word_extractor_core [] xs)
                                | True = word_extractor_core (word++[x]) xs
				where
				safe_head [] = ' '
				safe_head a = head a

word_extractor :: [Char] -> [[Char]] 
word_extractor input = (word_extractor_core [] input) -- >>= no_space

ls_or :: [Bool] -> Bool
ls_or [] = False
ls_or (x:xs) = x || (ls_or xs)

word_finder :: [Char] -> [Char] -> Bool
word_finder pat input = ls_or matched_words
 where
 matched_words = fmap (text_match pat) (word_extractor input)

--give in multiple keywords as string with spaces and give input
multi_word_finder :: [Char] -> [Char] -> Bool
multi_word_finder pat input = ls_and (fmap (\x -> word_finder x input) (word_extractor pat))

multi_word_finder_lowercase :: [Char] -> [Char] -> Bool
multi_word_finder_lowercase pat input = multi_word_finder (fmap toLower pat) (fmap toLower input)
