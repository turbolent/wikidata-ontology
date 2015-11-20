package com.turbolent.wikidataOntology


case class Item(id: Int, name: String)

package object Q {
  val human = Item(5, "human")
  val city = Item(515, "city")
  val book = Item(571, "book")
  val year = Item(577, "year")
  val planet = Item(634, "planet")
  val country = Item(6256, "country")
  val mountain = Item(8502, "mountain")
  val movie = Item(11424, "movie")
  val president = Item(30461, "president")
  val musicalInstrument = Item(34379, "musical instrument")
  val language = Item(34770, "language")
  val musicGenre = Item(189991, "music genre")
  val album = Item(482994, "album")
  val male = Item(6581097, "male")
  val female = Item(6581072, "female")

  // occupations
  val politician = Item(82955, "politician")
  val actor = Item(33999, "actor")
  val painter = Item(1028181, "painter")
  val writer = Item(36180, "writer")
  val journalist = Item(1930187, "journalist")
  val singer = Item(177220, "singer")
  val composer = Item(36834, "composer")
  val priest = Item(42603, "priest")
  val baseballPlayer = Item(10871364, "baseball player")
  val poet = Item(49757, "poet")
  val lawyer = Item(40348, "lawyer")
  val athleticsCompetitor = Item(11513337, "athletics competitor")
  val historian = Item(201788, "historian")
  val filmDirector = Item(2526255, "film director")
  val screenwriter = Item(28389, "screenwriter")
  val iceHockeyPlayer = Item(11774891, "ice hockey player")
  val architect = Item(42973, "architect")
  val author = Item(482980, "author")
  val diplomat = Item(193391, "diplomat")
  val cricketer = Item(12299841, "cricketer")
  val musician = Item(639669, "musician")
  val engineer = Item(81096, "engineer")
  val basketballPlayer = Item(3665646, "basketball player")
  val sculptor = Item(1281618, "sculptor")
  val bicycleRacer = Item(2309784, "bicycle racer")
  val officer = Item(189290, "officer")
  val novelist = Item(6625963, "novelist")
  val rugbyUnionPlayer = Item(14089670, "rugby union player")
  val judge = Item(16533, "judge")
  val botanist = Item(2374149, "botanist")
  val photographer = Item(33231, "photographer")
  val theologian = Item(1234713, "theologian")
  val physician = Item(39631, "physician")
  val sportsperson = Item(2066131, "sportsperson")
  val economist = Item(188094, "economist")
  val mathematician = Item(170790, "mathematician")
  val artist = Item(483501, "artist")
  val conductor = Item(158852, "conductor")
  val philosopher = Item(4964182, "philosopher")
  val jurist = Item(185351, "jurist")
  val singerSongwriter = Item(488205, "singer-songwriter")
  val translator = Item(333634, "translator")
  val filmProducer = Item(3282637, "film producer")
  val entrepreneur = Item(131524, "entrepreneur")
  val physicist = Item(169470, "physicist")
  val jazzMusician = Item(15981151, "jazz musician")
  val televisionPresenter = Item(947873, "television presenter")
  val linguist = Item(14467526, "linguist")
  val rugbyLeaguePlayer = Item(14373094, "rugby league player")
  val seiyu = Item(622807, "seiyū")
  val operaSinger = Item(2865819, "opera singer")
  val chemist = Item(593644, "chemist")
  val musicEducator = Item(16145150, "music educator")
  val tennisPlayer = Item(10833314, "tennis player")
  val educationist = Item(1231865, "educationist")
  val rower = Item(13382576, "rower")
  val entomologist = Item(3055126, "entomologist")
  val teacher = Item(37226, "teacher")
  val model = Item(4610556, "model")
  val pianist = Item(486748, "pianist")
  val soldier = Item(4991371, "soldier")
  val musicologist = Item(14915627, "musicologist")
  val boxer = Item(11338576, "boxer")
  val voiceActor = Item(2405480, "voice actor")
  val anthropologist = Item(4773904, "anthropologist")
  val archaeologist = Item(3621491, "archaeologist")
  val illustrator = Item(644687, "illustrator")
  val handballPlayer = Item(13365117, "handball player")
  val mangaka = Item(191633, "mangaka")
  val playwright = Item(214917, "playwright")
  val artHistorian = Item(1792450, "art historian")
  val songwriter = Item(753110, "songwriter")
  val chessPlayer = Item(10873124, "chess player")
  val filmActor = Item(10800557, "film actor")
  val astronomer = Item(11063, "astronomer")
  val announcer = Item(1371925, "announcer")
  val basketballCoach = Item(5137571, "basketball coach")
  val badmintonPlayer = Item(13141064, "badminton player")
  val explorer = Item(11900058, "explorer")
  val universityProfessor = Item(1622272, "university professor")
  val fencer = Item(13381863, "fencer")
  val racingDriver = Item(378622, "racing driver")
  val golfer = Item(13156709, "golfer")
  val businessperson = Item(43845, "businessperson")
  val professor = Item(121594, "professor")
  val publisher = Item(2516866, "publisher")
  val radioHost = Item(2722764, "radio host")
  val psychologist = Item(212980, "psychologist")
  val guitarist = Item(855091, "guitarist")
  val recordProducer = Item(183945, "record producer")
  val swimmer = Item(10843402, "swimmer")
  val volleyballPlayer = Item(15117302, "volleyball player")
  val privateBanker = Item(806798, "private banker")
  val librarian = Item(182436, "librarian")
  val amateurWrestler = Item(19595175, "amateur wrestler")
  val esperantist = Item(860918, "esperantist")
  val sociologist = Item(2306091, "sociologist")
  val zoologist = Item(350979, "zoologist")
  val biologist = Item(864503, "biologist")
  val televisionActor = Item(10798782, "television actor")
  val cinematographer = Item(222344, "cinematographer")
  val fieldHockeyPlayer = Item(10843263, "field hockey player")
  val computerScientist = Item(82594, "computer scientist")
  val rabbi = Item(133485, "rabbi")
  val militaryPersonnel = Item(47064, "military personnel")
  val motorcycleRacer = Item(3014296, "motorcycle racer")
  val sprinter = Item(4009406, "sprinter")
  val theatreDirector = Item(3387717, "theatre director")
  val choreographer = Item(2490358, "choreographer")
  val sportShooter = Item(17486376, "sport shooter")
  val pornographicActor = Item(488111, "pornographic actor")
  val psychiatrist = Item(211346, "psychiatrist")
  val alpineSkier = Item(4144610, "alpine skier")
  val surgeon = Item(774306, "surgeon")
  val dancer = Item(5716684, "dancer")
  val rapper = Item(2252262, "rapper")
  val politicalScientist = Item(1238570, "political scientist")
  val comedian = Item(245068, "comedian")
  val biathlete = Item(16029547, "biathlete")
  val discJockey = Item(130857, "disc jockey")
  val associationFootballManager = Item(628099, "association football manager")
  val filmEditor = Item(7042855, "film editor")
  val judoka = Item(6665249, "judoka")
  val autobiographer = Item(18814623, "autobiographer")
  val skiJumper = Item(13382603, "ski jumper")
  val organist = Item(765778, "organist")
  val missionary = Item(219477, "missionary")
  val aviator = Item(2095549, "aviator")
  val scientist = Item(901, "scientist")
  val hurler = Item(18199024, "hurler")
  val classicalPhilologist = Item(16267607, "classical philologist")
  val philologist = Item(13418253, "philologist")
  val musher = Item(500097, "musher")
  val memberOfParliament = Item(486839, "member of parliament")
  val paleontologist = Item(1662561, "paleontologist")
  val director = Item(3455803, "director")
  val speedSkater = Item(10866633, "speed skater")
  val geologist = Item(520549, "geologist")
  val jockey = Item(846750, "jockey")
  val ornithologist = Item(1225716, "ornithologist")
  val civilEngineer = Item(13582652, "civil engineer")
  val comicsArtist = Item(715301, "comics artist")
  val presenter = Item(13590141, "presenter")
  val graphicDesigner = Item(627325, "graphic designer")
  val professionalWrestler = Item(13474373, "professional wrestler")
  val televisionDirector = Item(2059704, "television director")
  val rikishi = Item(2727289, "rikishi")
  val catholicPriest = Item(250867, "Catholic priest")
  val gaelicFootballPlayer = Item(17351861, "Gaelic football player")
  val essayist = Item(11774202, "essayist")
  val mixedMartialArtist = Item(11607585, "mixed martial artist")
  val farmer = Item(131512, "farmer")
  val tableTennisPlayer = Item(13382519, "table tennis player")
  val inventor = Item(205375, "inventor")
  val merchant = Item(215536, "merchant")
  val archivist = Item(635734, "archivist")
  val animator = Item(266569, "animator")
  val geographer = Item(901402, "geographer")
  val policeOfficer = Item(384593, "police officer")
  val headCoach = Item(3246315, "head coach")
  val crossCountrySkier = Item(13382608, "cross-country skier")
  val editor = Item(1607826, "editor")
  val marathonRunner = Item(13382460, "marathon runner")
  val sportsCoach = Item(41583, "sports coach")
  val biochemist = Item(2919046, "biochemist")
  val samurai = Item(38142, "samurai")
  val designer = Item(5322166, "designer")
  val violinist = Item(1259917, "violinist")
  val televisionProducer = Item(578109, "television producer")
  val balletDancer = Item(805221, "ballet dancer")
  val naturalist = Item(18805, "naturalist")
  val herpetologist = Item(16271064, "herpetologist")
  val saxophonist = Item(12800682, "saxophonist")
  val engraver = Item(329439, "engraver")
  val chef = Item(3499072, "chef")
  val drummer = Item(386854, "drummer")
  val humanRightsActivist = Item(1476215, "human rights activist")
  val audioEngineer = Item(128124, "audio engineer")
  val cartoonist = Item(1114448, "cartoonist")
  val ichthyologist = Item(4205432, "ichthyologist")
  val historianOfModernAge = Item(17489339, "historian of modern age")
  val fashionDesigner = Item(3501317, "fashion designer")
  val presbyter = Item(831474, "presbyter")
  val mycologist = Item(2487799, "mycologist")
  val statistician = Item(2732142, "statistician")
  val longDistanceRunner = Item(4439155, "long-distance runner")
  val bishop = Item(29182, "bishop")
  val curler = Item(17516936, "curler")
  val nurse = Item(186360, "nurse")
  val mountaineer = Item(9149093, "mountaineer")
  val cleric = Item(2259532, "cleric")
  val bandleader = Item(806349, "bandleader")
  val contributingEditor = Item(876864, "contributing editor")
  val librettist = Item(8178443, "librettist")
  val lichenologist = Item(15924544, "lichenologist")
  val opinionJournalist = Item(16287483, "opinion journalist")
  val monk = Item(733786, "monk")
  val medievalist = Item(3332711, "medievalist")
  val artCollector = Item(10732476, "art collector")
  val civilServant = Item(212238, "civil servant")
  val cricketUmpire = Item(2143894, "cricket umpire")
  val dubActor = Item(11481802, "dub actor")
  val manager = Item(2462658, "manager")
  val middleDistanceRunner = Item(13381753, "middle-distance runner")
  val draughtsperson = Item(15296811, "draughtsperson")
  val literaryCritic = Item(4263842, "literary critic")
  val sailor = Item(45199, "sailor")
  val racecarDriver = Item(15958185, "racecar driver")
  val magistrate = Item(4594605, "magistrate")
  val rakugoka = Item(11620676, "Rakugoka")
  val condottiero = Item(273108, "condottiero")
  val weightlifter = Item(13381376, "weightlifter")
  val specialistInLiterature = Item(17167049, "specialist in literature")
  val trumpeter = Item(17093672, "trumpeter")
  val choirDirector = Item(1076502, "choir director")
  val lexicographer = Item(14972848, "lexicographer")
  val cartographer = Item(1734662, "cartographer")
  val aerospaceEngineer = Item(15895020, "aerospace engineer")
  val pharmacist = Item(105186, "pharmacist")
  val churchHistorian = Item(1743122, "church historian")
  val astronaut = Item(11631, "astronaut")
  val organMaker = Item(1937431, "organ maker")
  val columnist = Item(1086863, "columnist")
  val pastor = Item(152002, "pastor")
  val militaryPhysician = Item(4002666, "military physician")
  val mineralogist = Item(13416354, "mineralogist")
  val lithographer = Item(16947657, "lithographer")
  val bassist = Item(584301, "bassist")
  val artisticGymnast = Item(13381572, "artistic gymnast")
  val newsPresenter = Item(270389, "news presenter")
  val dentist = Item(27349, "dentist")
  val meteorologist = Item(2310145, "meteorologist")
  val photojournalist = Item(957729, "photojournalist")
  val prehistorian = Item(17488316, "prehistorian")
  val neuroscientist = Item(6337803, "neuroscientist")
  val mayor = Item(30185, "mayor")
  val veterinarian = Item(202883, "veterinarian")
  val metallurgist = Item(18576582, "metallurgist")
  val beachVolleyballPlayer = Item(17361156, "beach volleyball player")
  val futsalPlayer = Item(18515558, "futsal player")
  val geneticist = Item(3126128, "geneticist")
  val circusPerformer = Item(17307272, "circus performer")
  val caricaturist = Item(3658608, "caricaturist")
  val sovereign = Item(2304859, "sovereign")
  val literaryHistorian = Item(13570226, "literary historian")
  val formulaOneDriver = Item(10841764, "Formula One driver")
  val regionalHistorian = Item(17488465, "regional historian")
  val ophthalmologist = Item(15059856, "ophthalmologist")
  val diarist = Item(18939491, "diarist")
  val abbot = Item(103163, "abbot")
  val goPlayer = Item(12039558, "go player")
  val cook = Item(156839, "cook")
  val squashPlayer = Item(16278103, "squash player")
  val curator = Item(674426, "curator")
  val ceramist = Item(7541856, "ceramist")
  val minister = Item(1423891, "minister")
  val pokerPlayer = Item(15295720, "poker player")
  val waterPoloPlayer = Item(17524364, "water polo player")
  val psychotherapist = Item(1900167, "psychotherapist")
  val archbishop = Item(49476, "archbishop")
  val trackCyclist = Item(15117395, "track cyclist")
  val horseTrainer = Item(466640, "horse trainer")
  val internist = Item(15924224, "internist")
  val financier = Item(1979607, "financier")
  val politicalCommissar = Item(168559, "political commissar")
  val heimatforscher = Item(1595570, "Heimatforscher")
  val archer = Item(13382355, "archer")
  val germanist = Item(2599593, "germanist")
  val lacrossePlayer = Item(17682262, "lacrosse player")
  val magician = Item(148401, "magician")
  val bandyPlayer = Item(18702210, "bandy player")
  val cameraOperator = Item(1208175, "camera operator")
  val musicTheorist = Item(16031530, "music theorist")
  val barrister = Item(808967, "barrister")
  val javelinThrower = Item(18510502, "javelin thrower")
  val arachnologist = Item(17344952, "arachnologist")
  val taekwondoAthlete = Item(13382533, "taekwondo athlete")
  val printer = Item(175151, "printer")
  val scenographer = Item(2707485, "scenographer")
  val clarinetist = Item(118865, "clarinetist")
  val proGamer = Item(4379701, "pro gamer")
  val snookerPlayer = Item(17165321, "snooker player")
  val legalHistorian = Item(2135538, "legal historian")
  val cabaretArtist = Item(15214752, "cabaret artist")
  val socialWorker = Item(12455619, "social worker")
  val nordicCombinedSkier = Item(13382605, "Nordic combined skier")
  val equestrian = Item(2730732, "equestrian")
  val civilLawNotary = Item(189010, "civil law notary")
  val freestyleSkier = Item(18617021, "freestyle skier")
  val pharmacologist = Item(2114605, "pharmacologist")
  val dartsPlayer = Item(18574233, "darts player")
  val karateka = Item(9017214, "karateka")
  val banjoist = Item(9648008, "banjoist")
  val drugTrafficker = Item(10384029, "drug trafficker")
  val physiologist = Item(14946079, "physiologist")
  val bobsledder = Item(13383011, "bobsledder")
  val lyricist = Item(822146, "lyricist")
  val neurologist = Item(783906, "neurologist")
  val anatomist = Item(10872101, "anatomist")
  val costumeDesigner = Item(1323191, "costume designer")
  val filmCritic = Item(15809665, "film critic")
  val submariner = Item(3492027, "submariner")
  val stageActor = Item(2259451, "stage actor")
  val restaurateur = Item(3427922, "restaurateur")
  val cellist = Item(13219637, "cellist")
  val pathologist = Item(15814242, "pathologist")
  val educator = Item(974144, "educator")
  val egyptologist = Item(1350189, "egyptologist")
  val ethnologist = Item(1371378, "ethnologist")
  val bridgePlayer = Item(18437198, "bridge player")
  val programmer = Item(5482740, "programmer")
  val astrophysicist = Item(752129, "astrophysicist")
  val performanceArtist = Item(10774753, "performance artist")
  val testPilot = Item(730242, "test pilot")
  val fiddler = Item(19754019, "fiddler")
  val artCritic = Item(4164507, "art critic")
  val restorer = Item(2145981, "restorer")
  val religiousServant = Item(15995941, "religious servant")
  val docent = Item(462390, "docent")
  val triathlete = Item(15306067, "triathlete")
  val rallyDriver = Item(10842936, "rally driver")
  val spy = Item(9352089, "spy")
  val activist = Item(15253558, "activist")
  val blogger = Item(8246794, "blogger")
  val militaryHistorian = Item(1493121, "military historian")
  val salonHolder = Item(3068305, "salon-holder")
  val gynaecologist = Item(2640827, "gynaecologist")
  val motivationalSpeaker = Item(15982858, "motivational speaker")
  val genealogist = Item(8963721, "genealogist")
  val hornist = Item(19729565, "hornist")
  val malacologist = Item(16271261, "malacologist")
  val fighterPilot = Item(618694, "fighter pilot")
  val microbiologist = Item(3779582, "microbiologist")
  val draughtsman = Item(683754, "draughtsman")
  val horticulturist = Item(3140857, "horticulturist")
  val etcher = Item(10862983, "etcher")
  val printmaker = Item(15966904, "printmaker")
  val visualArtist = Item(3391743, "visual artist")
  val carcinologist = Item(16868721, "carcinologist")
  val musicCritic = Item(1350157, "music critic")
  val schoolTeacher = Item(2251335, "school teacher")
  val poleVaulter = Item(13464497, "pole vaulter")
  val beautyPageantContestant = Item(18581305, "beauty pageant contestant")
  val musicHistorian = Item(20198542, "music historian")
  val artDirector = Item(706364, "art director")
  val calligrapher = Item(3303330, "calligrapher")
  val romanist = Item(2504617, "romanist")
  val vocalist = Item(2643890, "vocalist")
  val copperplateEngraver = Item(13365770, "copperplate engraver")
  val orientalist = Item(1731155, "orientalist")
  val sportsJournalist = Item(13219447, "sports journalist")
  val academic = Item(16631371, "academic")
  val oboist = Item(16003954, "oboist")
  val gardener = Item(758780, "gardener")
  val historianOfMathematics = Item(17486330, "historian of mathematics")
  val flyingAce = Item(222982, "flying ace")
  val rancher = Item(18972771, "rancher")
  val philanthropist = Item(13472585, "philanthropist")
  val researcher = Item(1650915, "researcher")
  val numismatist = Item(2004963, "numismatist")
  val puppeteer = Item(2629392, "puppeteer")
  val surfer = Item(13561328, "surfer")
  val cycloCrossCyclist = Item(15117415, "cyclo-cross cyclist")
  val landscapeArchitect = Item(2815948, "landscape architect")
  val urbanPlanner = Item(131062, "urban planner")
  val molecularBiologist = Item(15839206, "molecular biologist")
  val reporter = Item(42909, "reporter")
  val poloPlayer = Item(13218361, "polo player")
  val goldsmith = Item(211423, "goldsmith")
  val newspaperEditor = Item(17351648, "newspaper editor")
  val chiefExecutiveOfficer = Item(484876, "chief executive officer")
  val nuclearScientist = Item(16742096, "nuclear scientist")
  val preacher = Item(432386, "preacher")
  val cardiologist = Item(15618389, "cardiologist")
  val lobbyist = Item(11986654, "lobbyist")
  val clubDj = Item(10816969, "club DJ")
  val stockbroker = Item(4182927, "stockbroker")
  val netballer = Item(17619498, "netballer")
  val chessComposer = Item(2627699, "chess composer")
  val characterActor = Item(948329, "character actor")
  val immunologist = Item(15634285, "immunologist")
  val illuminator = Item(998628, "illuminator")
  val referee = Item(202648, "referee")
  val agronomist = Item(1781198, "agronomist")
  val playboyPlaymate = Item(728711, "Playboy Playmate")
  val youtuber = Item(17125263, "YouTuber")
  val americanFootballPlayer = Item(19204627, "American football player")
  val king = Item(12097, "king")
  val lecturer = Item(1569495, "lecturer")
  val troubadour = Item(186370, "troubadour")
  val ambassador = Item(121998, "ambassador")
  val criminologist = Item(8142883, "criminologist")
  val bookseller = Item(998550, "bookseller")
  val pediatrician = Item(1919436, "pediatrician")
  val luthPlayer = Item(21166956, "luth player")
  val ultramarathonRunner = Item(19827218, "ultramarathon runner")
  val bartender = Item(808266, "bartender")
  val astrologer = Item(155647, "astrologer")
  val streetArtist = Item(7622988, "street artist")
  val hammerThrower = Item(13856320, "hammer thrower")
  val theatreCritic = Item(17337766, "theatre critic")
  val sportsCommentator = Item(2986228, "sports commentator")
  val bacteriologist = Item(15816836, "bacteriologist")
  val epidemiologist = Item(13416803, "epidemiologist")
  val manufacturer = Item(15637082, "manufacturer")
  val builder = Item(1340643, "builder")
  val warCorrespondent = Item(164236, "war correspondent")
  val statesman = Item(372436, "statesman")
  val makeUpArtist = Item(935666, "make-up artist")
  val architecturalHistorian = Item(17486326, "architectural historian")
  val luthier = Item(762707, "luthier")
  val mandolinist = Item(19723482, "mandolinist")
  val parson = Item(955464, "parson")
  val botanicalIllustrator = Item(3148760, "botanical illustrator")
  val dermatologist = Item(16031394, "dermatologist")
  val nun = Item(191808, "nun")
  val oncologist = Item(16062369, "oncologist")
  val dramaturge = Item(487596, "dramaturge")
  val henchman = Item(247297, "henchman")
  val economicHistorian = Item(17488363, "economic historian")
  val nascarTeamOwner = Item(17614049, "NASCAR team owner")
  val ladyInWaiting = Item(715222, "Lady-in-waiting")
  val miningEngineer = Item(18524075, "mining engineer")
  val psychoanalyst = Item(3410028, "psychoanalyst")
  val ethnomusicologist = Item(17484288, "ethnomusicologist")
  val tradeUnionist = Item(15627169, "trade unionist")
  val chamberlain = Item(264323, "chamberlain")
  val luger = Item(13382981, "luger")
  val skeletonRacer = Item(13388442, "skeleton racer")
  val topologist = Item(18663426, "topologist")
  val academician = Item(414528, "academician")
  val winemaker = Item(897317, "winemaker")
  val figureSkater = Item(13219587, "figure skater")
  val radioDj = Item(10730252, "radio DJ")
  val playerOfBasquePelota = Item(19746576, "player of Basque pelota")
  val medicalHistorian = Item(15985128, "medical historian")
  val churchMusician = Item(355493, "church musician")
  val watchmaker = Item(157798, "watchmaker")
  val bowler = Item(4951095, "bowler")
  val matador = Item(2412523, "matador")
  val typographer = Item(1229025, "typographer")
  val accountant = Item(326653, "accountant")
  val diver = Item(16004431, "diver")
  val childActor = Item(970153, "child actor")
  val handballCoach = Item(13365201, "handball coach")
  val cryptographer = Item(15442776, "cryptographer")
  val musicExecutive = Item(3089940, "music executive")
  val canoer = Item(13382566, "canoer")
  val literaryEditor = Item(2516852, "literary editor")
  val bryologist = Item(16334507, "bryologist")
  val obstetrician = Item(13638192, "obstetrician")
  val gymnast = Item(16947675, "gymnast")
  val mechanicalEngineer = Item(1906857, "mechanical engineer")
  val baseballUmpire = Item(1856798, "baseball umpire")
  val catholicBishop = Item(611644, "Catholic bishop")
  val affichiste = Item(739437, "affichiste")
  val faculty = Item(5428874, "faculty")
  val nutritionist = Item(2576499, "nutritionist")
  val medallist = Item(1708232, "medallist")
  val hurdler = Item(13724897, "hurdler")
  val neurosurgeon = Item(9385011, "neurosurgeon")
  val poolPlayer = Item(20540007, "pool player")
  val warPhotographer = Item(11496048, "war photographer")
  val mechanic = Item(327029, "mechanic")
  val corvetteCaptain = Item(279704, "corvette captain")
  val revolutionary = Item(3242115, "revolutionary")
  val speleologist = Item(16742175, "speleologist")
  val navigator = Item(254651, "navigator")
  val shortStoryWriter = Item(15949613, "short story writer")
  val artDealer = Item(173950, "art dealer")
  val midwife = Item(185196, "midwife")
  val aristocrat = Item(2478141, "aristocrat")
  val ostracodologist = Item(21484199, "ostracodologist")
  val rector = Item(212071, "rector")
  val acarologist = Item(19798999, "acarologist")
  val religious = Item(2566598, "religious")
  val literary = Item(18195617, "literary")
  val textileArtist = Item(10694573, "textile artist")
  val torturer = Item(20725072, "torturer")
  val chansonnier = Item(13391399, "chansonnier")
  val ufologist = Item(18921227, "ufologist")
  val mammalogist = Item(16831394, "mammalogist")
  val geodesist = Item(11699606, "geodesist")
  val pteridologist = Item(16334509, "pteridologist")
  val biographer = Item(864380, "biographer")
  val stuntPerformer = Item(465501, "stunt performer")
  val seismologist = Item(12051314, "seismologist")
  val secretary = Item(80687, "secretary")
  val filmmaker = Item(15212951, "filmmaker")
  val muhaddith = Item(1172458, "muhaddith")
  val industrialist = Item(2193183, "industrialist")
  val mountainGuide = Item(819677, "mountain guide")
  val deacon = Item(161944, "deacon")
  val skipper = Item(1897112, "skipper")
  val woodCarver = Item(6138343, "wood carver")
  val musicDirector = Item(1198887, "music director")
  val consultant = Item(15978655, "consultant")
  val juristConsultant = Item(2664701, "jurist-consultant")
  val shipowner = Item(500251, "shipowner")
  val privateInvestigator = Item(1058617, "private investigator")
  val highJumper = Item(13382122, "high jumper")
  val longJumper = Item(13381428, "long jumper")
  val iconographer = Item(4199058, "iconographer")
  val virologist = Item(15634281, "virologist")
  val intendant = Item(6491450, "intendant")
  val bibliographer = Item(10429346, "bibliographer")
  val impresario = Item(943995, "impresario")
  val functionary = Item(1328323, "functionary")
  val editorInChief = Item(589298, "editor-in-chief")
  val radioProducer = Item(3406651, "radio producer")
  val motocrossRider = Item(12327806, "motocross rider")
  val assessor = Item(335757, "assessor")
  val geheimrat = Item(11165895, "Geheimrat")
  val geopolitician = Item(16947320, "geopolitician")
  val chronicler = Item(3330547, "chronicler")
  val anglicanPriest = Item(3409375, "Anglican priest")
  val solicitor = Item(14284, "solicitor")
  val directorOfChurchMusic = Item(1294133, "director of church music")
  val culturalHistorian = Item(15462162, "cultural historian")
  val videoGameDeveloper = Item(210167, "video game developer")
  val akhoond = Item(2632248, "Akhoond")
  val soundArtist = Item(19850998, "sound artist")
  val trombonist = Item(544972, "trombonist")
  val theoreticalPhysicist = Item(19350898, "theoretical physicist")
  val sinologist = Item(15255771, "sinologist")
  val prosecutor = Item(600751, "prosecutor")
  val talentAgent = Item(1344174, "talent agent")
  val kickboxer = Item(11296761, "kickboxer")
  val rugbyPlayer = Item(13415036, "rugby player")
  val hagiographer = Item(17166634, "hagiographer")
  val beekeeper = Item(852389, "beekeeper")
  val chaplain = Item(208762, "chaplain")
  val oceanographer = Item(3546255, "oceanographer")
  val curate = Item(1753370, "curate")
  val bioinformatician = Item(15956275, "bioinformatician")
  val balloonist = Item(728425, "balloonist")
  val programMaker = Item(2548714, "program maker")
  val resistanceFighter = Item(1397808, "resistance fighter")
  val musicArranger = Item(1643514, "music arranger")
  val businessManager = Item(832136, "business manager")
  val rowingCoach = Item(21121588, "Rowing coach")
  val liedermacher = Item(956365, "Liedermacher")
  val draughtsPlayer = Item(18536353, "draughts player")
  val provost = Item(756215, "provost")
  val joiner = Item(326358, "joiner")
  val prelate = Item(725440, "prelate")
  val businessExecutive = Item(5001872, "business executive")
  val electricalEngineer = Item(1326886, "electrical engineer")
  val faqih = Item(1999841, "faqih")
  val stainedGlassArtist = Item(2205972, "stained-glass artist")
  val socialScientist = Item(15319501, "social scientist")
  val iceHockeyCoach = Item(5137576, "ice hockey coach")
  val patron = Item(15472169, "patron")
  val miner = Item(820037, "miner")
  val parasitologist = Item(20751419, "parasitologist")
  val privateer = Item(201559, "privateer")
  val militaryLeader = Item(1402561, "military leader")
  val lightingDesigner = Item(1823479, "lighting designer")
  val musicProducer = Item(3922505, "music producer")
  val discusThrower = Item(13381689, "discus thrower")
  val medicalWriter = Item(18533509, "medical writer")
  val conArtist = Item(14948222, "con artist")
  val historianOfReligion = Item(17488357, "historian of religion")
  val torero = Item(549322, "torero")
  val harpsichordist = Item(5371902, "harpsichordist")
  val advocate = Item(3611891, "advocate")
}