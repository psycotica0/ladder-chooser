import qualified Data.Sequence as S
import System.IO

main = do
	input <- getContents
	let ladder = S.fromList $ lines input
	withFile "/dev/tty" ReadWriteMode $ resolve_match ladder

-- If there's only one item, it's the victor
-- Spit it out to stdout, not the terminal
resolve_match ladder tty | (S.length ladder == 1) = putStrLn $ S.index ladder 0
resolve_match ladder tty | (S.length ladder == 0) = return ()

resolve_match ladder tty = do
	let (current, rest) = S.splitAt 2 ladder
	hPutStrLn tty $ (show $ S.index current 0) ++ " vs " ++ (show $ S.index current 1)
	hPutStr tty "(L/R/N)? "
	hFlush tty
	input <- hGetLine tty
	resolve_match (process_inputs (current, rest) input) tty

-- This function takes the 
process_inputs (current, rest) "L" = rest S.|> (S.index current 0)
process_inputs (current, rest) "R" = rest S.|> (S.index current 1)
process_inputs (current, rest) "N" = rest
-- If we don't know what they said, just send them the same thing again
process_inputs (current, rest) _ = current S.>< rest
