(* 
Määritellään funktio, joka saa kolme parametria. Kahta ensimmäistä käytetään fibonacci-luvun laskemiseen, jonka on oltava suurempi kuin kolmas parametri. Rekursiivinen funktio on erikseen määriteltävä rec-avainsanalla 

Sisennykset ovat semanttisesti merkitseviä ja ilmaisevat lohkorakenteen
*)
let rec fibonacci first second min_value =
    if first + second <= min_value then
// rekursiivinen kutsu
        fibonacci second (first + second) min_value     
else
            first + second // lohkon viimeinen arvo palautetaan
(*
Edellisenkaltainen funktio voidaan määritellä myös ilman rekursiota käyttäen laiskaa evaluaatiota ja korkeamman kertaluvun funktiota. Tässä funktion fibonacci 2 paluuarvon tyyppi ja parametrin n tyyppi on eksplisiittisesti määritelty aritmeettisten ylivuotojen välttämiseksi. Funktio Seq.unfold ottaa parametreinaan generaattorifunktion ja laskennan alkutilan ja palauttaa sequence:n johon on poimittu toistuvasti generaattorifunktion paluuarvoparin toinen jäsen. Paluuarvon ensimmäinen jäsen toimii “tilana” josta generaattori generoi seuraavan “arvon” ja “tilan” . Sequence evaluoidaan laiskasti, mikä mahdollistaa äärettömän “listan” luomisen. Tässä sitä on käytetty fibonaccin lukujonon määrittelemiseen. Seq.find puolestaan ottaa predikaatin ja sequence:n ja palauttaa sequencen ensimmäisen jäsenen, jolle predikaatti pätee. Binäärioperaattoreita voidaan käyttää prefix-notaatiolla suluttamalla ne, kuten tässä on tehty < -operaattorille. Lauseke ((<) n) on (<)-funktion osittainen sovellus, ekvivalentti anonyymin funktion (fun x -> n < x) kanssa. I-päätteiset literaalit ovat bigint-literaaleja.
*)

let fibonacci2 (n: bigint): bigint =
    let fibs: seq<bigint> = 
        //invariantti: (a,b) = (fib(n), fib(n+1))
Seq.unfold (fun (a,b) -> Some(a, (b, a + b))) (0I, 1I)
    Seq.find ( (<) n ) fibs

(*    
 kutsutaan fibonacci-funktiota ja tulostetaan arvo. Näillä argumenteilla tulostaa 34. “%A” formatoi automaattisesti annetun syötteen. [<EntryPoint>] määrittää ohjelman aloituspisteen (vrt. public static void main() Javassa tai int main() C/C++:ssa)
*)
[<EntryPoint>]
let main argv =
    printfn "%A" (fibonacci 0 1 27)
    0 // ohjelman paluuarvo

