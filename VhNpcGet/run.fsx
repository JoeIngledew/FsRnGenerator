#r "System.Net.Http"
#r "Newtonsoft.Json"

open System
open System.Net
open System.Net.Http
open Newtonsoft.Json

let defaultMin = 0
let defaultMax = 3 

type Npc = {
    name: string
    descriptors: string []
    quirks: string []
    asTier: int
    genderId: string
    sampleDesc: string
}

let logM (log : TraceWriter option) (s : string) =
    match log with
    | Some l -> l.Info(s)
    | None -> ()

let wrapDupes (xs : string []) =
    xs
    |> Array.mapi (fun ix x -> if xs.[0..ix] |> Array.filter (fun s -> s = x) |> Array.length > 1 then sprintf "\"%s\"" x else x)

let packStrings (xs : string[]) =
    let len = xs.Length
    let modXs = wrapDupes xs
    if len = 1 then modXs.[0]
    elif len = 2 then sprintf "%s and %s" modXs.[0] modXs.[1]
    else 
        let commaSep = System.String.Join(", ", modXs.[0..len-3])
        sprintf "%s, %s and %s" commaSep modXs.[len-2] modXs.[len-1]

let getQuirkString (qs  : string []) =
    if qs.Length > 0 then
        sprintf " who %s" (packStrings qs)
    else ""

let getDescString (ds : string []) =
    if ds.Length > 0 then
        sprintf " %s " (packStrings ds)
    else " "

let getSampleDesc (name: string) (desc: string[]) (quirks: string[]) (tier : int) (gender: string) = 
    sprintf "%s, a%s%s%s. (Tier %i)" name (getDescString desc) gender (getQuirkString quirks) tier

let mutable rng = new Random()

let getGenderIdentifier (log : TraceWriter option) = 
    sprintf "RNG is %A" rng |> logM log
    let percent = rng.Next(1, 101)
    sprintf "gender percent is %i" percent |> logM log
    if percent > 80 then "woman"
    elif percent > 60 then "man"
    else "person"

let getTier =
    let percent = rng.Next(101)
    if percent > 99 then 2
    elif percent > 94 then 1
    else 0

let names = "Anton,Gaston,Brooke,Arrav,Gretchen,Red,Greed,Valkyr,Seren,Cross,Alec,Alice,Saskia,Aoife,Saoirse,Olga,Helga,Sveta,Svetlana,Jessika,Rolf,Werner,Verne,Albrecht,Madchen,Zeke,Horkenfriend,Arkenfriend,Tanya,Nena,Vasken,Tetra,Zeera,Hirgen,Ursula,Vivienne,Vivi,Myra,Orchard,Rus,Bower,Usher,Balto,Sara,Greta,Basil,Roth,Eli,Skulk,Croon,Mourn,Eben,Brazz,Child,Jest,Ash,Eel,Mesmer,Oanken,Nadja,Najka,Art,Min,Vale,Weaver,Mac,Stone,Farrant,River,Alex,Yargle,Luc,Marne,Morne,Mary,Velka,Vorsht,Hork,Messer,Tristan,Tristain,Rey,Pollo,Pollok,Wallace,Tyr,Baxter,Bax,Ida,Ella,Carrie,Lillian,Pearl,Catherine,Ada,Blanche,May,Effie,Mae,Alma,Irene,Irina,Addie,Esther,Kat,Ora,Osha,Iva,Eva,Evelyn,Hilda,Till,Genevive,Irving,Wilson,Curtis,Cecil,Ellis,Manuel,Virgil,Silas,Elijah,Felix,Cornell,Levi,Ross,Maurice,Amos,Wes,Lester,Leon,Warren,Aldo,Xander,Ray,Luther,Francis,Tom,Thom,Earl,Will,Walther,von Haus,Abel,Acacia,Ace,Adalee,Adalia,Aderyn,Aldia,Adler,Adrienne,Aegle,Adriel,Aerith,Aerin,Erin,Afsha,Aidric,Edric,Aksel,Aletha,Allegra,Aloy,Aloysius,Alwyn,Amachi,Amado,Amara,Amarantha,Amethyst,Garnet,Amos,Arlan,Anders,Anise,Andro,Aquillina,Ara,Arcadia,Arden,Ari,Asher,Ashlyn,Ashok,Asriel,Astrid,Atilio,Atticus,Auberon,Aura,Aurelia,Azure,Baird,Baker,Balthazar,Bambi,Bannon,Barker,Baron,Bayard,Beck,Beckett,Benedicta,Berry,Birch,Bishop,Blaer,Bo,Bogdan,Bond,Bram,Bryce,Bronwen,Bubba,Burgess,Burke,Cade,Cabot,Caelan,Caesar,Cainan,Calandra,Calder,Calhoun,Calix,Callahan,Calliope,Camden,Canei,Canon,Canton,Case,Cassius,Cass,Cedar,Cherish,Chloris,Cicero,Cinnamon,Coco,Colden,Corban,Coulter,Cowan,Crisin,Cullen,Dagny,Damarion,Damaris,Danae,Dante,Dason,Dax,Daya,Dedric,Demetra,Deon,Destin,Viper,Python,Diamondback,Winder,Devyn,Dirk,Dryden,Duke,Desden,Echo,Edrea,Eldred,Eldora,Elgin,Elmer,Elmore,Emlyn,Emrys,Enoch,Eoghan,Erasmo,Errol,Eudora,Evadne,Fallon,Faron,Fawn,Faye,Forbes,Fyfe,Galla,Gannon,Gaspar,Gemini,Gideon,Gower,Greer,Gudrun,Gunther,Harmon,Hart,Hesper,Hester,Hodge,Hope,Hubbel,Hux,Hyacinth,Iago,Ianthe,Ignis,Ignacia,Igor,Ilaria,Ilene,Iliana,Ilya,Iolanthe,Isolde,Istas,Jagger,Jager,Jan,Janus,Jarrel,Jeriah,Jericho,Jethro,Jolyon,Jewel,Juniper,Jurgen,Kafka,Kenna,Kinsey,Kyllion,Lachlan,Lainey,Lark,Lars,Larsen,Lasse,Lorelei,Ludmilla,Macsen,Maeve,Marek,Melody,Melton,Merrick,Merrill,Merton,Milena,Monseratte,Mordecai,Morton,Myra,Myron,Myrtle,Nairn,Nemi,Mixie,Nona,Norton,Norval,Nuala,Nysa,Octavia,Oda,Odelia,Odessa,Odette,Oleg,Onyx,Opal,Ora,Orchid,Orlean,Orrick,Oswald,Otten,Paprika,Parthenia,Pavel,Pearl,Peony,Petros,Peyton,Philemon,Philo,Finn,Pia,Phillys,Precious,Prim,Proctor,Prospero,Quillan,Quillon,Dillion,Quinn,Raven,Raz,Reid,Remy,Ridley,Riona,River,Rogue,Ruslan,Rusty,Rye,Ryder,Ryker,Sabina,Saffron,Sage,Saoirse,Sapphire,Scout,Selah,Seneca,Seraphina,Shoshana,Shura,Sigmund,Silvanus,Sipho,Sypha,Siren,Sirrus,Sloane,Solveig,Sona,Soren,Spike,Stamos,Star,Stellan,Stoney,Storm,Svea,Syon,Tad,Tolbot,Tanis,Tarek,Tarian,Tarun,Taurean,Tave,Tawny,Teal,Tea,Temperance,Thalia,Thane,Tibor,Torin,Trevelyan,Trilby,Trix,Tullia,Ty,Ulf,Ulrich,Uma,Una,Uriel,Vaclav,Van,Vance,Varun,Vaughan,Vega,Vella,Vellis,Verna,Vester,Vibol,Vidal,Vitus,Vivek,Vlad,Vonda,Walden,Waleska,Walker,Ward,Wardell,Wassily,Waylon,Wendell,Whisper,Wiley,Wilhelm,Willow,Wilmot,Wilton,Windell,Winona,Winter,Summer,Autumn,Wolf,Wolfrom,Wolter,Wren,Worth,Wyatt,Xoey,Yael,Yanis,Yelena,Yerik,Yetta,Ylva,Ynyr,Yona,Ysanne,Yusia,Zak,Zaid,Zephyr,Zia,Zoya"
let quirks = "has a dog,carries a silver-inlaid cane,smells like death,is wraped in far too many furs,smells vagualy of candle wax,walks with a limp,snorts when they laugh,is missing an arm,is missing a limb,carries a small snakeskim tome,carries a bag of books,is armed with a rapier,is armed with a knife,wears tattered finery,has no shoes,wears pristine clothes,has a strange accent,has an unusual vocal pattern,emotes wildly when they talk,only speaks Vornheim Sign Language,is quick to anger,has prominent tattoos,has distinct tattoos,has small tattoos,has high-quality tattoos,has low-quality tattoos,smells like fish,wears old-style military uniform,smells of lavender,smells of cumin,smells of nutmeg,smells of tobacco,smells like petrichor,looks distant,will not look you in the eye,wears gloves,has no shoes,has faint ligature marks on their wrists,has faint ligature marks on their neck,has a prominent scar,has lots of scars,has a fresh wound, has lots of fresh wounds,has a broken rib,walks on crutches,chews their nails compulsively,has an inviting smile,has piercing eyes,fidgets with whatever they're holding,speaks with a lisp,wears shiny armour,wears rusted armour,is not wearing much,is a hull,lacks common sense,is a compulsive liar,has a lot of cash on hand,is out to get you,wears glasses,wears a monocle,is very unwell,is a gang member,is a railjack,is a snakereader,is a servant,is from the Foundation,is lost,is a sailor,is a Leviathan Hunter,is a Sparkcrafter,is a shoeshiner,is unemployed,is a courtesan,moonlights as an actor,is an alcoholic,seems familiar,is using a fake name,is a ghost,looks like a southander,is an inspector,is a bluecoat"
let descriptions = "beautiful,attractive,deathly,pretty,ugly,tall,short,average,fat,thin,lanky,smart,scruffy,well-presented,sharp,clever,knowledgable,manipulative,vindictive,witty,funny,dour,sensible,common,noble,high-class,middle-class,working-class,dangerous,shadowy,shady,up-front,matter-of-fact,straightforward,softly-spoken,harsh,cruel,kind,caring,detached,radiant,truthful,conceited,healthy,unhealthy,pale,dessicated,malnourished,well-fed,wealthy,poor,happy,unhappy,snappy,angry,enthusiastic,boring,grey,old,young,childish,juvenile,ancient,wise,surprised,shocked,shifty,forthcoming,divine,strong,weak,hardy,robust,ghostly,severe,calm,calming,tranquil"

let getRandomInCommaSepSet (log : TraceWriter option) (xs : string) =
    sprintf "RNG is %A" rng |> logM log 
    let split = xs.Split(',')
    split.[rng.Next(split.Length)]

let getXFromSet (log : TraceWriter option) (x : int) (xs : string) = 
    sprintf "Getting %i from set of length %i" x (xs.Length) |> logM log
    [ for _ in 1..x do yield getRandomInCommaSepSet log xs ] |> List.toArray

let getSomeFromCommaSepSet (numToGet : int option) (min : int option) (max : int option) (xs : string) (log: TraceWriter option) =
    match (numToGet, min, max) with 
    | (Some n, _, _) ->
        sprintf "Getting fixed amount %i" n |> logM log
        if n < 0 then Choice2Of2 ("Number to generate must be greater than or equal to zero")
        else
            logM log "did not throw"
            getXFromSet log n xs |> Choice1Of2
    | (_, Some n, None) ->
        sprintf "Getting a min of amount %i" n |> logM log
        if n > defaultMax then Choice2Of2 ("Minimum must be less than default max of 3 if specifying only minimum")
        else 
            let countSome = rng.Next(n, (defaultMax + 1))
            getXFromSet log countSome xs |> Choice1Of2
    | (_, None, Some n) ->
        sprintf "Getting a max of amount %i" n |> logM log
        if n <= defaultMin then Choice2Of2 ("Maximum must be greater than default minimum of 1 is specifying only maximum")
        else
            let countSome = rng.Next(defaultMin, (n + 1))
            getXFromSet log countSome xs |> Choice1Of2
    | (_, Some n, Some m) -> 
        sprintf "Getting between %i and %i" n m |> logM log
        if m <= n then Choice2Of2 ("Minimum must be less than maximum")
        else 
            let countSome = rng.Next(n, (m + 1))
            getXFromSet log countSome xs |> Choice1Of2
    | _ ->
        logM log "Getting a random amt"
        let countSome = rng.Next(defaultMin, (defaultMax + 1))
        getXFromSet log countSome xs |> Choice1Of2

let toIntOption (o : KeyValuePair<string,string> option) =
    match o with
    | Some x -> Some (int x.Value)
    | None -> None

let getStringParam (req : HttpRequestMessage) (key : string) =
    req.GetQueryNameValuePairs()
    |> Seq.tryFind (fun q -> q.Key = key)
    |> toIntOption

let Run(req: HttpRequestMessage, log: TraceWriter) = 
    async {
        rng <- new System.Random()
        let l : (TraceWriter option) = None
        log.Info(sprintf "F# HTTP Trigger function began processing a request")

        log.Info(sprintf "getting num quirks")
        let numQuirks = getStringParam req "numQuirks"
        log.Info(sprintf "Got Num Quirks %A, getting min quirks" numQuirks)
        let minQuirks = getStringParam req "minQuirks"
        log.Info(sprintf "Got min quirks %A, getting max quirks" minQuirks)
        let maxQuirks = getStringParam req "maxQuirks"
        log.Info(sprintf "Got max quirks %A, getting num desc" maxQuirks)
        let numDesc = getStringParam req "numDesc"
        log.Info(sprintf "Got num descs %A, getting min desc" numDesc)
        let minDesc = getStringParam req "minDesc"
        log.Info(sprintf "Got min desc %A, getting max desc" minDesc)
        let maxDesc = getStringParam req "maxDesc"
        log.Info(sprintf "got max desc %A" maxDesc)

        log.Info(sprintf "Getting quirk set")
        let quirksSet = getSomeFromCommaSepSet numQuirks minQuirks maxQuirks quirks l
        log.Info(sprintf "Got quirk set %A" quirksSet)

        match quirksSet with
        | Choice2Of2 s -> return req.CreateResponse(HttpStatusCode.BadRequest, s)
        | Choice1Of2 qs ->
            log.Info(sprintf "Getting descriptor set")
            let descs = getSomeFromCommaSepSet numDesc minDesc maxDesc descriptions l
            log.Info(sprintf "Got descriptor set %A" descs)
            match descs with 
            | Choice2Of2 s -> return req.CreateResponse(HttpStatusCode.BadRequest, s)
            | Choice1Of2 ds ->
                let retName = getRandomInCommaSepSet l names
                let retQuirks = qs
                let retDesc = ds
                let tier = getTier
                let gender = getGenderIdentifier l
                let sample = getSampleDesc retName retDesc retQuirks tier gender

                return req.CreateResponse(HttpStatusCode.OK, { name = retName; descriptors = retDesc; quirks = retQuirks; asTier = tier; genderId = gender; sampleDesc = sample })
    } |> Async.RunSynchronously