#r "System.Net.Http"
#r "Newtonsoft.Json"
#r "System.Xml.dll"
#r "System.Runtime.Serialization.dll"

open System
open System.Net
open System.Net.Http
open Newtonsoft.Json
open Microsoft.FSharp.Reflection
open System.IO
open System.Reflection
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary
open System.Runtime.Serialization.Json
open System.Text
open System.Xml
open System.Xml.Serialization

let defaultMin = 0
let defaultMax = 3 

[<DataContract>]
type Npc = {
    [<field: DataMember>]
    name: string
    [<field: DataMember>]
    descriptors: string []
    [<field: DataMember>]
    quirks: string []
    [<field: DataMember>]
    asTier: int
    [<field: DataMember>]
    genderId: string
    [<field: DataMember>]
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

let addQuotes (s : string) =
    sprintf "\"%s\"" s

let maybeWrapInQuotes (s : string) =
    let p = rng.Next(1, 101)
    if p > 96 then addQuotes s else s

let getGenderIdentifier (log : TraceWriter option) = 
    sprintf "RNG is %A" rng |> logM log
    let percent = rng.Next(1, 101)
    sprintf "gender percent is %i" percent |> logM log
    if percent > 80 then "woman"
    elif percent > 60 then "man"
    else "person" |> maybeWrapInQuotes

let getTier =
    let percent = rng.Next(101)
    if percent > 99 then 2
    elif percent > 94 then 1
    else 0

let names = "Anton,Gaston,Brooke,Arrav,Gretchen,Red,Greed,Valkyr,Seren,Cross,Alec,Alice,Saskia,Aoife,Saoirse,Olga,Helga,Sveta,Svetlana,Jessika,Rolf,Werner,Verne,Albrecht,Madchen,Zeke,Horkenfriend,Arkenfriend,Tanya,Nena,Vasken,Tetra,Zeera,Hirgen,Ursula,Vivienne,Vivi,Myra,Orchard,Rus,Bower,Usher,Balto,Sara,Greta,Basil,Roth,Eli,Skulk,Croon,Mourn,Eben,Brazz,Child,Jest,Ash,Eel,Mesmer,Oanken,Nadja,Najka,Art,Min,Vale,Weaver,Mac,Stone,Farrant,River,Alex,Yargle,Luc,Marne,Morne,Mary,Velka,Vorsht,Hork,Messer,Tristan,Tristain,Rey,Pollo,Pollok,Wallace,Tyr,Baxter,Bax,Ida,Ella,Carrie,Lillian,Pearl,Catherine,Ada,Blanche,May,Effie,Mae,Alma,Irene,Irina,Addie,Esther,Kat,Ora,Osha,Iva,Eva,Evelyn,Hilda,Till,Genevive,Irving,Wilson,Curtis,Cecil,Ellis,Manuel,Virgil,Silas,Elijah,Felix,Cornell,Levi,Ross,Maurice,Amos,Wes,Lester,Leon,Warren,Aldo,Xander,Ray,Luther,Francis,Tom,Thom,Earl,Will,Walther,von Haus,Abel,Acacia,Ace,Adalee,Adalia,Aderyn,Aldia,Adler,Adrienne,Aegle,Adriel,Aerith,Aerin,Erin,Afsha,Aidric,Edric,Aksel,Aletha,Allegra,Aloy,Aloysius,Alwyn,Amachi,Amado,Amara,Amarantha,Amethyst,Garnet,Amos,Arlan,Anders,Anise,Andro,Aquillina,Ara,Arcadia,Arden,Ari,Asher,Ashlyn,Ashok,Asriel,Astrid,Atilio,Atticus,Auberon,Aura,Aurelia,Azure,Baird,Baker,Balthazar,Bambi,Bannon,Barker,Baron,Bayard,Beck,Beckett,Benedicta,Berry,Birch,Bishop,Blaer,Bo,Bogdan,Bond,Bram,Bryce,Bronwen,Bubba,Burgess,Burke,Cade,Cabot,Caelan,Caesar,Cainan,Calandra,Calder,Calhoun,Calix,Callahan,Calliope,Camden,Canei,Canon,Canton,Case,Cassius,Cass,Cedar,Cherish,Chloris,Cicero,Cinnamon,Coco,Colden,Corban,Coulter,Cowan,Crisin,Cullen,Dagny,Damarion,Damaris,Danae,Dante,Dason,Dax,Daya,Dedric,Demetra,Deon,Destin,Viper,Python,Diamondback,Winder,Devyn,Dirk,Dryden,Duke,Desden,Echo,Edrea,Eldred,Eldora,Elgin,Elmer,Elmore,Emlyn,Emrys,Enoch,Eoghan,Erasmo,Errol,Eudora,Evadne,Fallon,Faron,Fawn,Faye,Forbes,Fyfe,Galla,Gannon,Gaspar,Gemini,Gideon,Gower,Greer,Gudrun,Gunther,Harmon,Hart,Hesper,Hester,Hodge,Hope,Hubbel,Hux,Hyacinth,Iago,Ianthe,Ignis,Ignacia,Igor,Ilaria,Ilene,Iliana,Ilya,Iolanthe,Isolde,Istas,Jagger,Jager,Jan,Janus,Jarrel,Jeriah,Jericho,Jethro,Jolyon,Jewel,Juniper,Jurgen,Kafka,Kenna,Kinsey,Kyllion,Lachlan,Lainey,Lark,Lars,Larsen,Lasse,Lorelei,Ludmilla,Macsen,Maeve,Marek,Melody,Melton,Merrick,Merrill,Merton,Milena,Monseratte,Mordecai,Morton,Myra,Myron,Myrtle,Nairn,Nemi,Mixie,Nona,Norton,Norval,Nuala,Nysa,Octavia,Oda,Odelia,Odessa,Odette,Oleg,Onyx,Opal,Ora,Orchid,Orlean,Orrick,Oswald,Otten,Paprika,Parthenia,Pavel,Pearl,Peony,Petros,Peyton,Philemon,Philo,Finn,Pia,Phillys,Precious,Prim,Proctor,Prospero,Quillan,Quillon,Dillion,Quinn,Raven,Raz,Reid,Remy,Ridley,Riona,River,Rogue,Ruslan,Rusty,Rye,Ryder,Ryker,Sabina,Saffron,Sage,Saoirse,Sapphire,Scout,Selah,Seneca,Seraphina,Shoshana,Shura,Sigmund,Silvanus,Sipho,Sypha,Siren,Sirrus,Sloane,Solveig,Sona,Soren,Spike,Stamos,Star,Stellan,Stoney,Storm,Svea,Syon,Tad,Tolbot,Tanis,Tarek,Tarian,Tarun,Taurean,Tave,Tawny,Teal,Tea,Temperance,Thalia,Thane,Tibor,Torin,Trevelyan,Trilby,Trix,Tullia,Ty,Ulf,Ulrich,Uma,Una,Uriel,Vaclav,Van,Vance,Varun,Vaughan,Vega,Vella,Vellis,Verna,Vester,Vibol,Vidal,Vitus,Vivek,Vlad,Vonda,Walden,Waleska,Walker,Ward,Wardell,Wassily,Waylon,Wendell,Whisper,Wiley,Wilhelm,Willow,Wilmot,Wilton,Windell,Winona,Winter,Summer,Autumn,Wolf,Wolfrom,Wolter,Wren,Worth,Wyatt,Xoey,Yael,Yanis,Yelena,Yerik,Yetta,Ylva,Ynyr,Yona,Ysanne,Yusia,Zak,Zaid,Zephyr,Zia,Zoya"
let quirks = "has a dog,carries a silver-inlaid cane,smells like death,is wraped in far too many furs,smells vagualy of candle wax,walks with a limp,snorts when they laugh,is missing an arm,is missing a limb,carries a small snakeskim tome,carries a bag of books,is armed with a rapier,is armed with a knife,wears tattered finery,has no shoes,wears pristine clothes,has a strange accent,has an unusual vocal pattern,emotes wildly when they talk,only speaks Vornheim Sign Language,is quick to anger,has prominent tattoos,has distinct tattoos,has small tattoos,has high-quality tattoos,has low-quality tattoos,smells like fish,wears old-style military uniform,smells of lavender,smells of cumin,smells of nutmeg,smells of tobacco,smells like petrichor,looks distant,will not look you in the eye,wears gloves,has no shoes,has faint ligature marks on their wrists,has faint ligature marks on their neck,has a prominent scar,has lots of scars,has a fresh wound, has lots of fresh wounds,has a broken rib,walks on crutches,chews their nails compulsively,has an inviting smile,has piercing eyes,fidgets with whatever they're holding,speaks with a lisp,wears shiny armour,wears rusted armour,is not wearing much,is a hull,lacks common sense,is a compulsive liar,has a lot of cash on hand,is out to get you,wears glasses,wears a monocle,is very unwell,is a gang member,is a railjack,is a snakereader,is a servant,is from the Foundation,is lost,is a sailor,is a Leviathan Hunter,is a Sparkcrafter,is a shoeshiner,is unemployed,is a courtesan,moonlights as an actor,is an alcoholic,seems familiar,is using a fake name,is a ghost,looks like a southander,is an inspector,is a bluecoat,has a great big bushy beard,rubs their hands together a lot,clips their nails with a long knife,wears odd clothing,smells like goats,has complex braided hair,has a braided beard,has neat stubble,uses long words incorrectly,is always eating,is always drinking,has mismatched eye colours,picks their ears,picks their nose,smells like wet dog,smells like cheap perfume,smells like expensive perfume,smells of spirits,smells of cedar wood,smells of woodsmoke,scratches their chin,sniffs often,weaves flowers in their hair,weaves flowers in their clothing,is a braggart,is highly superstitious,wears unusual jewellry,has freckes,doesn't blink,is holier than thou,rubs their palms on their thighs,seems very cold,seems overly warm,asks often about their appearance,yawns a lot,has a squeaky voice,has a very deep voice,belly-laughs often,makes bad jokes,is addicted to puns,twirls coins on their fingers,belches loud and often,picks their teeth with a knife,coughs a lot,blinks a lot,has very oily skin,is very sweaty,sneezes a lot,stretches a lot,has clothes a few sizes too large,has ill-fitting (small) clothes,offers food,offers drink,offers exotic things,rubs their eyes often,has deep bags under their eyes,taps their foot all the time,drums their fingers all the time,crask their knuckes,has a black eye,picks lint off their clothes (and others'!),has their arm in a sling,has no teeth,has bruised fingers,has frostbite,has a bandaged hand,has a bandaged leg,has a bandaged arm,is dressed very formally,has extraordinarily colourful clothing,makes up words,carries crumbs to feed birds,speaks to animals like people,covers their mouth when speaking,touches people when they're talking to them a lot,is paint-stained,is fascinated by fire,hands coppers to beggars,has bulging pockets,has archaic speech patterns,has unusually clean shoes,wears a toupe,has fake facial hair,has dyed hair,writes everything people tell them,has a nosebleed,wears rune-inlaid armour,avoids crowds,rubs a knee injury,has arthiritis,wears a well-tended blade,refers to good old days,flirts with one of the PCs,has ink-stained hands and papercuts,is a clerk,has a burn scar,wears a ring on every finger,has a lot of piercings,sharpens a knife with a whetstone,rubs palm calluses with their thumb,is hard of hearing,gives a fake name every meeting,regularly blows stray hair out of their eyes,bows before and after speaking,has crumbs all over them,has a food-stained face,has a wine-stained face,carries a mouse,speaks through sign language,uses foul language often,is always late,pouts,mixes others' names up,giggles (innappropriately),combs their hair with their fingers,jingles coins in their pouch,is intoxicated,plays with a necklace,says \"hmm...\" often,asks rhetorical questions,gossips a lot,claims to be a member of a gang,greatly exaggerates,nods often,carries a lot of junk,has a prominent animal bite scar,is sleepy,is always out of breath,is an avid artist and sketches characters,is a hyperchondriac,is a vegetarian,only eats meat,has no fingernails,keeps losing their stuff,often forgets what they are saying,likes to play games,is an avid gambler,talks too loudly,attracts birds,attracts cats,consistently invades personal space"
let descriptions = "beautiful,attractive,deathly,pretty,ugly,tall,short,average,fat,thin,lanky,smart,scruffy,well-presented,sharp,clever,knowledgable,manipulative,vindictive,witty,funny,dour,sensible,common,noble,high-class,middle-class,working-class,dangerous,shadowy,shady,up-front,matter-of-fact,straightforward,softly-spoken,harsh,cruel,kind,caring,detached,radiant,truthful,conceited,healthy,unhealthy,pale,dessicated,malnourished,well-fed,wealthy,poor,happy,unhappy,snappy,angry,enthusiastic,boring,grey,old,young,childish,juvenile,ancient,wise,surprised,shocked,shifty,forthcoming,divine,strong,weak,hardy,robust,ghostly,severe,calm,calming,tranquil,fretful,able,frantic,possessive,abrasive,fresh,practical,funky,absent,precise,predictable,abusive,frightened,preoccupied,accepting,frigid,pretentious,accident-prone,frugal,accommodating,prim,fun,productive,adaptable,professional,furtive,proper,adorable,fussy,protective,adventurous,gabby,proud,affable,garrulous,prudent,affected,gaudy,affectionate,generous,puckish,genial,punctilious,punctual,giddy,purposeful,agnostic,giggly,pushy,agreeable,puzzled,alert,quarrelsome,alluring,glamorous,queer,aloof,gloomy,quick,altruistic,glorious,quick-tempered,hungry,glum,quiet,ambiguous,good,quixotic,ambitious,goofy,rembunctious,amiable,graceful,random,amused,gracious,rash,amusing,grandiose,rational,grateful,rawboned,animated,realistic,annoyed,gregarious,reasonable,annoying,grieving,rebellious,anti-social,grouchy,recalcitrant,anxious,growly,receptive,apathetic,gruesome,reckless,apologetic,gruff,reclusive,appreciative,grumpy,refined,apprehensive,guarded,reflective,approachable,regretful,argumentative,Guilty,Aristocratic,Gullible,Relaxed,Arrogant,Haggling,Relents,Artistic,Handsome,Reliable,Ashamed,Happy,Relieved,Aspiring,Hard,Religious,Assertive,Reluctant,Astonished,Hardy,Remorseful,Attentive,Harmonious,Repugnant,Audacious,Harried,Repulsive,Austere,Harsh,Resentful,Authoritarian,Hateful,Reserved,Authoritative,Haughty,Resilient,Available,Healthy,Resolute,Average,Resourceful,Awful,Heartless,Respectful,Awkward,Responsible,Babbling,Hedonistic,Responsive,Babyish,Helpful,Restless,Bad,Helpless,Retiring,Bashful,Hesitant,Rhetorical,Beautiful,High,Rich,Belligerent,HighRight,Bewildered,Hilarious,Righteous,Biter,Homeless,Rigid,Honest,Risk-Taking,Blasé,Romantic,Blowhard,Honorable,Rough,Boastful,Hopeful,Rowdy,Boisterous,Hopeless,Rude,Bold,Hormonal,Rugged,Boorish,Horrible,Ruthless,Bored,Hospitable,Sacrificing,Boring,Hostile,Sad,Bossy,Sadistic,Boundless,Huffy,Safe,Brainy,Humble,Sagely,Brash,Humorous,Saintly,Bratty,Hurt,Salient,Brave,Hysterical,Sanctimonious,Brazen,Ignorant,Sanguine,Bright,Ill,Sarcastic,Brilliant,Ill-Bred,Sassy,Brotherly,Imaginative,Satisfied,Brutish,Immaculate,Saucy,Bubbly,Immature,Savage,Busy,Immobile,Scared,Calculating,Immodest,Scarred,Callous,Impartial,Scary,Calm,Impatient,Scattered,Candid,Imperial,Scheming,Capable,Impolite,Scornful,Capricious,Impotent,Scrawny,Carefree,Impractical,Scruffy,Careful,Impudent,Secretive,Careless,Impulsive,Secure,Caring,Inactive,Sedate,Caustic,Incoherent,Seductive,Cautious,Incompetent,Selective,Changeable,Inconsiderate,Self-Centered,Charismatic,Inconsistent,Self-Confident,Charming,Indecisive,Self-Conscious,Chaste,Independent,Self-Controlling,Cheerful,Indifferent,Self-Directed,Cheerless,Indiscrete,Self-Disciplined,Childish,Indiscriminate,Self-Giving,Chivalrous,Indolent,Self-Reliant,Civilised,Indulgent,Self-Serving,Classy,Industrious,Selfish,Clean,Inefficient,Selfless,Clever,Inept,Senile,Close,Inflexible,Sensitive,Closed,Inimitable,Sensual,Clumsy,Innocent,Sentimental,Coarse,Inquisitive,Serene,Cocky,Insecure,Serious,Coherent,Insensitive,Sexual,Cold,Insightful,Sexy,Insincere,Shallow,Combative,Insipid,Shameless,Comfortable,Insistent,Sharp,Committed,Insolent,Sharp-Tongued,Communicative,Instinctive,Sharp-Witted,Compassionate,Insulting,Sheepish,Competent,Intellectual,Shiftless,Complacent,Intelligent,Shifty,Compliant,Intense,Short,Composed,Interested,Shrewd,Compulsive,Interrupting,Shy,Conceited,Intimidating,Silent,Concerned,Intolerant,Silky,Condescending,Intrepid,Silly,Confident,Introspective,Simian,Confused,Introverted,Simple,Congenial,Intuitive,Sincere,Conscientious,Inventive,Sisterly,Considerate,Involved,Skillful,Consistent,Irresolute,Sleazy,Constricting,Irresponsible,Sloppy,Content,Irreverent,Slovenly,Contented,Irritable,Contrarian,Irritating,Slutty,Contrite,Jackass,Sly,Controlling,Jaded,Small-Minded,Conversational,Jealous,Smart,Cooperative,Jittery,Smiling,Coquettish,Joking,Smooth,Courageous,Jolly,Sneaky,Courteous,Jovial,Snob,Covetous,Joyful,Sociable,Cowardly,Joyous,Soft-Hearted,Cowering,Judgmental,Soft-Spoken,Coy,Keen,Solitary,Crabby,Kenderish,Sore,Crafty,Sorry,Cranky,Kittenish,Sour,Crazy,Knowledgeable,Spendthrift,Creative,Lackadaisical,Spiteful,Credible,Lacking,Splendid,Creepy,Languid,Spoiled,Critical,Lascivious,Spontaneous,Cross,Late,Spunky,Crude,Lazy,Squeamish,Cruel,Leader,Stately,Cuddly,Lean,Static,Cultured,Lethargic,Steadfast,Curious,Level,Sterile,Cutthroat,Lewd,Stern,Cynical,Liar,Stimulating,Dainty,Licentious,Stingy,Dangerous,Light-Hearted,Stoical,Daring,Likeable,Stolid,Dark,Limited,Dashing,Lineat,Strange,Dauntless,Lingering,Strict,Dazzling,Lively,Strident,Debonair,Logical,Strong,Deceitful,Lonely,Deceiving,Loquacious,Stubborn,Decent,Lordly,Studious,Decisive,Loud,Stupid,Decorous,Loudmouth,Suave,Deep,Lovable,Submissive,Defeated,Lovely,Successful,Defective,Succinct,Deferential,Loving,Sulky,Defiant,Sullen,Deliberate,Lowly,Sultry,Delicate,Loyal,Supercilious,Delightful,Lucky,Superstitious,Demanding,Lunatic,Supportive,Demonic,Lying,Surly,Dependable,Macho,Suspicious,Dependent,Mad,Sweet,Depressed,Malice,Sympathetic,Deranged,Malicious,Systematic,Despicable,Manipulative,Taciturn,Despondent,Mannerly,Tacky,Detached,Materialistic,Tactful,Detailed,Matronly,Tactless,Determined,Matter-Of-Fact,Talented,Devilish,Mature,Talkative,Devious,Mean,Tall,Devoted,Meek,Tardy,Dignified,Melancholy,Tasteful,Diligent,Melodramatic,Temperamental,Direct,Temperate,Disaffected,Merciful,Tenacious,Disagreeable,Mercurial,Tense,Discerning,Messy,Tentative,Disciplined,Meticulous,Terrible,Discontented,Mild,Terrified,Discouraged,Mischievous,Testy,Discreet,Miserable,Thankful,Disgusting,Miserly,Thankless,Dishonest,Mistrusting,Disillusioned,Modern,Thorough,Disinterested,Modest,Thoughtful,Disloyal,Moody,Thoughtless,Dismayed,Moping,Threatening,Disorderly,Moralistic,Thrifty,Disorganized,Motherly,Thrilled,Disparaging,Motivated,Tight,Disrespectful,Mysterious,Timid,Dissatisfied,Nagging,Tired,Dissolute,Naive,Tireless,Distant,Narcissistic,Tiresome,Distraught,Narrow-Minded,Tolerant,Distressed,Nasty,Touchy,Disturbed,Naughty,Tough,Dogmatic,Neat,Trivial,Domineering,NeedsTroubled,Dorky,Needy,Truculent,Doubtful,Negative,Trusting,Downtrodden,Negligent,Trustworthy,Draconian,Nervous,Truthful,Dramatic,Neurotic,Typical,Dreamer,Ugly,Dreamy,Nibbler,Unappreciative,Dreary,Nice,Unassuming,Dubious,Unbending,Dull,Nihilistic,Unbiased,Dumb,Nimble,Uncaring,Dutiful,Uncommitted,Dynamic,Unconcerned,Eager,NoUncontrolled,Easygoing,Noble,Unconventional,Eccentric,Noisy,Uncooperative,Educated,Nonchalant,Uncoordinated,Effervescent,Nosy,Uncouth,Efficient,Undependable,Egocentric,Nuanced,Understanding,Egotistic,Nuisance,Undesirable,Elated,Nurturing,Undisciplined,Eloquent,Nut,Unenthusiastic,Embarrassed,Obedient,Unfeeling,Embittered,Obese,Unfocused,Obliging,Unforgiving,Eminent,Obnoxious,Unfriendly,Emotional,Obscene,Ungrateful,Empathetic,Obsequious,Unhappy,Enchanting,Observant,Unhelpful,Encouraging,Obstinate,Uninhibited,Enduring,Odd,Unkind,Energetic,Odious,Unmotivated,Engaging,Open,Unpredictable,Enigmatic,Open-Minded,Unreasonable,Entertaining,Opinionated,Unreceptive,Enthusiastic,Opportunistic,Unreliable,Envious,Optimistic,Unresponsive,Equable,Orcish,Unrestrained,Erratic,Orderly,Unruly,Ethical,Organized,Unscrupulous,Evasive,Ornery,Unselfish,Evil,Ossified,Unsure,Exacting,Ostentatious,Unsympathetic,Excellent,Outgoing,Unsystematic,Excessive,Outrageous,Unusual,Excitable,Outspoken,Unwilling,Excited,Overbearing,Upbeat,Exclusive,Overweight,Upset,Expansive,Overwhelmed,Uptight,Expert,Overwhelming,Useful,Extravagant,Paces,Vacant,Extreme,Pacifistic,Vague,Exuberant,Painstaking,Vain,Fabulous,Panicky,Valiant,Facetious,Paranoid,Vengeful,Faded,Particular,Venomous,Fair,Passionate,Verbose,FaithPassive,Versatile,Faithful,Passive-Aggressive,Vigorous,Faithless,Pathetic,Vindictive,Fake,Patient,Violent,Fanatical,Patriotic,Virtuous,Fanciful,Peaceful,Visual,Fantastic,Penitent,Vivacious,Fatalistic,Pensive,Volatile,Fearful,Perfect,Voracious,Fearless,Perfectionist,Vulgar,Feisty,Performer,Vulnerable,Ferocious,Perserverant,Warlike,Fidgety,Perseveres,Fierce,Persevering,Wary,Fiery,Persistent,Wasteful,Fighter,Persuasive,Weak,Filthy,Pert,Weary,Fine,Perverse,Weird,Finicky,Pessimistic,Flagging,Petty,Whimsical,Flakey,Petulant,Wholesome,Flamboyant,Philanthropic,Wicked,Flashy,Picky,Wild,Fleeting,Pious,Willing,Flexible,Pitiful,Wise,Flighty,Placid,Flippant,Plain,Withdrawn,Flirty,Playful,Witty,Flustered,Pleasant,Worldly,Focused,Pleasing,Worried,Foolish,Plotting,Worthless,Forceful,Plucky,Wretched,Forgetful,Polite,Xenophobic,Forgiving,Pompous,Young,Formal,Poor,Youthful,Fortunate,Popular,Zany,Foul,Positive,Zealot,Frank"

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
                let retName = getRandomInCommaSepSet l names |> maybeWrapInQuotes
                let retQuirks = qs
                let retDesc = ds
                let tier = getTier
                let gender = getGenderIdentifier l
                let sample = getSampleDesc retName retDesc retQuirks tier gender

                return req.CreateResponse(HttpStatusCode.OK, { name = retName; descriptors = retDesc; quirks = retQuirks; asTier = tier; genderId = gender; sampleDesc = sample })
    } |> Async.RunSynchronously