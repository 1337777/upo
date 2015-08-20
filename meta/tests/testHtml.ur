open Parse
open Html

val parser = format (b, i, a)

fun parse r =
    case parser r.Source of
        Failure s => error <xml>Bad HTML: {[s]}</xml>
      | Success x => return <xml><body>
        <h1>Here it is:</h1>

        {x}
      </body></xml>

fun main () = return <xml><body>
  <form> <textarea{#Source}/> <submit action={parse}/> </form>
</body></xml>
