#if INTERACTIVE

open System.IO

let file = Path.Combine([| __SOURCE_DIRECTORY__; __SOURCE_FILE__ |])
let contents = File.ReadAllText(file)
printf "%s" <| contents

#endif
