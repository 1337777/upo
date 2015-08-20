open Json

(* Example from http://en.wikipedia.org/wiki/JSON *)

type address = {StreetAddress : string,
                City : string,
                State : string,
                PostalCode : string}

type phoneType = variant [Mobile = string,
                          LandLine = unit,
                          Secret = unit]

type phoneNumber = {Type_ : phoneType,
                    Number : string}

type person = {FirstName : string,
               LastName : string,
               Age : int,
               Address : address,
               PhoneNumber : list phoneNumber}

val sample : person = {FirstName = "Larry",
                       LastName = "Lambda",
                       Age = 42,
                       Address = {StreetAddress = "99 Confluence Circle",
                                  City = "Hoserville",
                                  State = "QQ",
                                  PostalCode = "66666"},
                       PhoneNumber = {Type_ = make [#Mobile] "Verizon", Number = "1234"}
                                         :: {Type_ = make [#Secret] (), Number = "ssssh"}
                                         :: []}

val json_address : json address = json_record {StreetAddress = "streetAddress",
                                               City = "city",
                                               State = "state",
                                               PostalCode = "postalCode"}

val json_phoneType : json phoneType = json_variant {Mobile = "mobile",
                                                    LandLine = "landline",
                                                    Secret = "secret"}

val json_phoneNumber : json phoneNumber = json_record {Type_ = "type",
                                                       Number = "number"}

val json_person : json person = json_record {FirstName = "firstName",
                                             LastName = "lastName",
                                             Age = "age",
                                             Address = "address",
                                             PhoneNumber = "phoneNumber"}

fun renderPerson (p : person) = <xml>
  <b>First name:</b> {[p.FirstName]}<br/>
  <b>Last name:</b> {[p.LastName]}<br/>
  <b>Age:</b> {[p.Age]}<br/>
  <b>Street address:</b> {[p.Address.StreetAddress]}<br/>
  <b>City:</b> {[p.Address.City]}<br/>
  <b>State:</b> {[p.Address.State]}<br/>
  <b>Postal code:</b> {[p.Address.PostalCode]}<br/>
  <!-- <b>Phone numbers:</b> {List.mapX (fn pn => <xml>{[pn.Number]}; </xml>) p.PhoneNumber}<br/> -->
</xml>

fun parse r = return <xml><body>
  {renderPerson (fromJson r.Text)}
</body></xml>

fun main () = return <xml><body>
  <h1>Json parsing adventure!</h1>

  <hr/>
  <h2>Free sample</h2>

  {[toJson sample]}
  <hr/>

  <h2>Parse your own</h2>
  <form>
    <textarea{#Text} rows={10} cols={80}/><br/>
    <submit value="Parse" action={parse}/>
  </form>
</body></xml>

structure God = Json.Recursive(struct
                                   con t a = variant [Fun = string * list a,
                                                      Var = string]

                                   fun json_t [a] (_ : json a) : json (t a) =
                                       let
                                           val json_fun : json (string * list a) = json_record ("1", "2")
                                       in
                                           json_variant {Fun = "Fun", Var = "Var"}
                                       end
                               end)

fun renderGod (God.Rec g) =
    match g
          {Fun = fn (s, gs) => <xml>
            <b>Main god:</b> {[s]}<br/>
            <b>Subgods:</b> <ul>
              {List.mapX (fn g' => <xml><li>{renderGod g'}</li></xml>) gs}
            </ul>
          </xml>,
          Var = fn s => <xml>
            <b>Var:</b> {[s]}
          </xml>}

fun parseGod r = return <xml><body>
  <h2>Beautified</h2>
  {renderGod (fromJson r.Text)}

  <h2>Round-tripped</h2>
  {[toJson (fromJson r.Text : God.r)]}
</body></xml>

fun godMain () = return <xml><body>
  <h1>Parse ye gods</h1>

  <form>
    <textarea{#Text} rows={10} cols={80}/><br/>
    <submit value="Parse" action={parseGod}/>
  </form>
</body></xml>
