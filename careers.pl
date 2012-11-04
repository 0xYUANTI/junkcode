:- module(careers, []).

%%%%%%%%%%%%%%%
%%% Queries %%%
%%%%%%%%%%%%%%%

type(Career, Type)         :- career(Type, Career, _, _, _, _).
statline(Career, StatLine) :- career(_, Career, StatLine, _, _, _).
skills(Career, Skills)     :- career(_, Career, _, Skills, _, _).
talents(Career, Talents)   :- career(_, Career, _, _, Talents, _).
exits(Career, Exits)       :- career(_, Career, _, _, _, Exits).

career(C)                  :- career(_, C, _, _, _, _).
careers(Cs)                :- findall(C, career(C), Cs).

skill(Career, Skill)       :- occurs(Career, skills, Skill).
talent(Career, Talent)     :- occurs(Career, talents, Talent).

skills(Skills)             :- collected(skills, Skills).
talents(Talents)           :- collected(talents, Talents).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stored Procedures %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

occurs(Career, Field, Val) :-
        untangled(Career, Field, Vals),
        member(Val, Vals).

collected(Field, Vals) :-
        findall(Vals0, untangled(_, Field, Vals0), Vals1),
        flatten(Vals1, Vals2),
        list_to_set(Vals2, Vals).

untangled(Career, Field, Untangled) :-
        call(Field, Career, Tangled),
        maplist(untangled, Tangled, Untangled0),
        flatten(Untangled0, Untangled1),
        list_to_set(Untangled1, Untangled).

untangled(A,   A)     :- atom(A).
untangled(M:S, M:S)   :- atom(S).
untangled(M:S, M:any) :- var(S).
untangled(A,   As)    :- alts(A, As0), maplist(untangled, As0, As).

alts(oneof(Xs), Xs).
alts(twoof(Xs), Xs).

valid_career(N) :-
        career(T, N, SL, Ss, Ts, Es),
        is_type(T), is_name(N),
        is_statline(SL),
        is_abilities(Ss), is_abilities(Ts),
        is_exits(Es).

%%%%%%%%%%%%%
%%% Types %%%
%%%%%%%%%%%%%

is_name(N) :- atom(N).

is_type(basic).
is_type(advanced).

is_statline(Main-Secondary) :-
         Main = [_WS,_BS,_S,_T,_Ag,_Int,_WP,_Fel],
         Secondary = [A,W,Mag],
         maplist(between(0, 40), Main),
         between(0, 2, A), between(0, 8, W), between(0, 4, Mag),
         forall(member(Stat, Main), 0 is Stat mod 5).

is_abilities(As) :- maplist(is_ability, As).
is_ability(A)    :- atom(A).
is_ability(M:S)  :- atom(M), ( atom(S) ; var(S) ).
is_ability(A)    :- alts(A, As), is_abilities(As).

is_exits(Es) :- maplist(atom, Es).

%%%%%%%%%%%%%%%%
%%% Database %%%
%%%%%%%%%%%%%%%%

career(basic,
       agitator,
       [5, 5, 0, 0, 5, 10, 0, 10]-[0, 2, 0],
       [oneof(['academic knowledge':history, gossip]), oneof(['academic knowledge':law, 'common knowledge':'the empire']), concealment, charm, perception, 'read/write', oneof(['speak language':breton, 'speak language':tilean]), 'speak language':reikspiel],
       [oneof([coolheaded, 'street fighting']), 'flee!', 'public speaking'],
       [charlatan, demagogue, outlaw, politician, rogue, zealot]).

career(basic,
       apprentice_wizard,
       [0, 0, 0, 0, 5, 10, 15, 5]-[0, 2, 1],
       ['academic knowledge':magic, channelling, 'magical sense', perception, 'read/write', search, 'speak arcane language':magick, 'speak language':classical],
       [oneof(['aethyric attunement', 'fast hands']), 'petty magic':arcane, oneof([savvy, 'very resilient'])],
       [journeyman_wizard, scholar, scribe]).

career(basic,
       bailiff,
       [5, 5, 5, 0, 0, 10, 5, 10]-[0, 2, 0],
       ['academic knowledge':law, oneof(['animal care', gossip]), charm, oneof([command, navigation]), oneof([intimidate, 'common knowledge':'the empire']), perception, 'read/write', ride],
       [oneof([etiquette, 'super numerate']), 'public speaking'],
       [militiaman, politician, protagonist, racketeer, smuggler, toll_keeper]).

career(basic,
       barber_surgeon,
       [5, 0, 0, 0, 10, 10, 10, 5]-[0, 2, 0],
       [charm, oneof([drive, swim]), haggle, heal, perception, 'read/write', oneof(['speak language':breton, 'speak language':reikspiel, 'speak language':tilean]), trade:apothecary],
       [oneof(['resistance to disease', savvy]), oneof([suave, 'very resilient']), surgery],
       [interrogator, grave_robber, physician, tradesman, vagabond]).

career(basic,
       boatman,
       [10, 5, 5, 5, 10, 5, 0, 0]-[0, 2, 0],
       [oneof(['common knowledge':'the empire', 'common knowledge':kislev]), oneof(['consume alcohol', gossip]), navigation, 'outdoor survival', perception, row, sail, oneof(['secret language':'ranger tongue', 'speak language':kislevian]), swim],
       [orientation, 'seasoned traveller'],
       [fisherman, marine, navigator, seaman, smuggler]).

career(basic,
       bodyguard,
       [10, 0, 5, 5, 5, 0, 0, 0]-[1, 3, 0],
       ['dodge blow', heal, intimidate, perception],
       [oneof([disarm, 'quick draw']), 'specialist weapon group':parrying, 'specialist weapon group':throwing, 'street fighting', 'strike to stun', oneof(['very strong', 'very resilient'])],
       [bailiff, bounty_hunter, interrogator, jailer, mercenary, protagonist, racketeer]).

career(basic,
       bone_picker,
       [5, 0, 5, 10, 5, 0, 5, 5]-[0, 2, 0],
       ['animal care', oneof([charm, gossip]), drive, 'common knowledge':'the empire', evaluate, haggle, perception, search],
       [oneof([coolheaded, streetwise]), oneof([hardy, 'resistance to disease'])],
       [camp_follower, cat_burglar, fence, grave_robber, smuggler]).

career(basic,
       bounty_hunter,
       [5, 10, 5, 0, 10, 0, 5, 0]-[0, 2, 0],
       ['follow trail', intimidate, 'outdoor survival', perception, search, shadowing, 'silent move'],
       [oneof([marksman, 'strike to stun']), rover, 'specialist weapon group':entangling, oneof([sharpshooter, 'strike mighty blow'])],
       [mercenary, protagonist, scout, targeteer, vampire_hunter]).

career(basic,
       burgher,
       [5, 0, 0, 0, 5, 10, 5, 5]-[0, 2, 0],
       [oneof(['common knowledge':'the empire', 'consume alcohol']), drive, evaluate, oneof([gossip, 'read/write']), haggle, perception, search, oneof(['speak language':breton, 'speak language':kislevian, 'speak language':tilean]), 'speak language':reikspiel],
       [dealmaker, oneof([savvy, suave])],
       [agitator, fence, innkeeper, merchant, militiaman, tradesman, valet]).

career(basic,
       camp_follower,
       [0, 0, 0, 5, 10, 5, 5, 10]-[0, 2, 0],
       [oneof(['animal care', drive]), oneof([charm, evaluate]), gossip, haggle, perception, search, oneof([trade:armourer, trade:bowyer, trade:cartographer, trade:cook, trade:gunsmith, trade:herbalist, trade:merchant, trade:smith, trade:tailor, trade:weaponsmith, 'speak language':breton, 'speak language':kislevian, 'speak language':tilean, 'sleight of hand'])], % XXX parsed correctly?
       [oneof([dealmaker, 'street fighting']), 'flee!', oneof([hardy, suave]), oneof(['resistance to disease', 'seasoned traveller'])],
       [charcoal_burner, charlatan, servant, smuggler, spy, tradesman, vagabond]).

career(basic,
       charcoal_burner,
       [5, 0, 5, 5, 5, 5, 5, 5]-[0, 2, 0],
       [oneof(['common knowledge':'the empire', concealment]), oneof([drive, gossip]), haggle, 'outdoor survival', perception, 'scale sheer surface', search, 'secret signs':ranger],
       ['flee!', oneof([savvy, 'very strong'])],
       [hunter, miner, scout, vagabond, woodsman]).

career(basic,
       coachman,
       [5, 10, 0, 0, 10, 0, 5, 5]-[0, 2, 0],
       ['animal care', drive, oneof([gossip, haggle]), oneof([heal, ride]), navigation, perception, 'secret signs':ranger, oneof(['speak language':breton, 'speak language':kislevian, 'speak language':tilean])],
       [oneof(['quick draw', 'seasoned traveller']), 'specialist weapon group':gunpowder],
       [ferryman, highwayman, outlaw, roadwarden, scout, smuggler, toll_keeper]).

career(basic,
       entertainer,
       [5, 10, 0, 0, 10, 0, 5, 10]-[0, 2, 0],
       [oneof(['animal care', swim]), charm, 'common knowledge':'the empire', oneof([evaluate, gossip]), perception, performer:_, performer:_, 'speak language':reikspiel, oneof(['animal training', blather, 'charm animal', hypnotism, ride, 'scale sheer surface', 'sleight of hand', ventriloquism])],
       [twoof(['lightning reflexes', mimic, 'public speaking', 'quick draw', sharpshooter, 'specialist weapon group':throwing, 'trick riding', 'very strong', wrestling])],
       [charlatan, minstrel, rogue, thief, vagabond]).

career(basic,
       envoy,
       [5, 5, 0, 0, 5, 10, 5, 10]-[0, 2, 0],
       [charm, oneof(['common knowledge':'the empire', 'common knowledge':'the wasteland']), evaluate, gossip, haggle, perception, 'read/write', 'secret language':'guild tongue', swim, trade:merchant],
       [oneof([dealmaker, 'seasoned traveller'])],
       [charlatan, merchant, rogue, seaman, student, vagabond]).

career(basic,
       estalian_diestro,
       [15, 0, 5, 5, 10, 5, 0, 0]-[1, 2, 0],
       ['academic knowledge':science, 'common knowledge':estalia, 'dodge blow', 'read/write', 'speak language':estalian],
       [oneof(['lightning reflexes', swashbuckler]), oneof(['quick draw', 'strike to injure']), 'specialist weapon group':fencing, 'strike mighty blow'],
       [bodyguard, duellist, highwayman, protagonist, rogue]).

career(basic,
       ferryman,
       [5, 5, 10, 5, 5, 5, 0, 0]-[0, 2, 0],
       [charm, 'common knowledge':'the empire', oneof([evaluate, 'secret language':'ranger tongue']), oneof([gossip, intimidate]), haggle, perception, row, swim],
       [oneof([marksman, suave]), oneof(['specialist weapon group':gunpowder, 'street fighting'])],
       [boatman, highwayman, roadwarden, seaman, smuggler]).

career(basic,
       fieldwarden,
       [5, 10, 0, 5, 10, 0, 10, 0]-[0, 2, 0],
       [oneof(['academic knowledge':necromancy, 'common knowledge':'the empire']), concealment, 'follow trail', 'outdoor survival', perception, search, 'silent move'],
       [oneof(['fleet footed', savvy]), oneof(['mighty shot', 'rapid reload']), oneof([rover, 'quick draw'])],
       [bounty_hunter, mercenary, scout, vagabond, vampire_hunter]).

career(basic,
       fisherman,
       [0, 5, 10, 5, 10, 5, 0, 0]-[0, 2, 0],
       [oneof(['common knowledge':'the empire', 'common knowledge':'the wasteland']), oneof(['consume alcohol', haggle]), oneof([navigation, trade:merchant]), 'outdoor survival', perception, row, sail, oneof(['speak language':reikspiel, 'speak language':norse]), swim],
       [oneof([hardy, savvy]), oneof([orientation, 'street fighting'])],
       [marine, merchant, militiaman, navigator, seaman]).

career(basic,
       grave_robber,
       [5, 5, 5, 0, 10, 0, 10, 0]-[0, 2, 0],
       [drive, oneof([gossip, haggle]), perception, 'scale sheer surface', search, 'secret signs':thief, 'silent move'],
       ['flee!', 'resistance to disease', oneof([streetwise, 'strong-minded'])],
       [cat_burglar, fence, rat_catcher, student, thief]).

career(basic,
       hedge_wizard,
       [0, 0, 0, 5, 5, 5, 10, 10]-[0, 2, 1],
       [oneof(['animal care', haggle]), oneof([charm, intimidate]), channelling, oneof(['charm animal', trade:apothecary]), oneof([heal, hypnotism]), 'magical sense', perception, search],
       ['hedge magic', 'petty magic':hedge],
       [apprentice_wizard, charlatan, initiate, outlaw, vagabond]).

career(basic,
       hunter,
       [0, 15, 0, 5, 10, 5, 0, 0]-[0, 3, 0],
       [concealment, 'follow trail', 'outdoor survival', perception, oneof([search, swim]), 'secret signs':ranger, oneof(['silent move', 'set trap'])],
       [oneof([hardy, 'specialist weapon group':longbow]), oneof(['lightning reflexes', 'very resilient']), oneof([marksman, rover]), 'rapid reload'],
       [bounty_hunter, charcoal_burner, fieldwarden, kithband_warrior, miner, scout, soldier, targeteer]).

career(basic,
       initiate,
       [5, 5, 0, 5, 0, 10, 10, 10]-[0, 2, 0],
       [oneof(['academic knowledge':astronomy, 'academic knowledge':history]), 'academic knowledge':theology, charm, heal, perception, 'read/write', 'speak language':classical, 'speak language':reikspiel],
       [oneof(['lightning reflexes', 'very strong']), 'public speaking', oneof([suave, 'warrior born'])],
       [barber_surgeon, demagogue, friar, priest, scribe, zealot]).

career(basic,
       jailer,
       [10, 0, 10, 10, 0, 0, 5, 0]-[0, 3, 0],
       [command, 'consume alcohol', 'dodge blow', oneof([heal, 'sleight of hand']), intimidate, perception, search],
       ['resistance to disease', 'resistance to poison', 'specialist weapon group':entangling, wrestling],
       [bailiff, bodyguard, interrogator, rat_catcher, watchman]).

career(basic,
       kislevite_kossar,
       [10, 10, 0, 10, 0, 0, 10, 0]-[0, 2, 0],
       ['common knowledge':kislev, 'consume alcohol', 'dodge blow', oneof([gamble, gossip]), 'outdoor survival', perception, search, 'speak language':kislevian],
       ['specialist weapon group':'two-handed', 'strike to injure'],
       [bounty_hunter, mercenary, sergeant, shieldbreaker, veteran]).

career(basic,
       kithband_warrior,
       [5, 5, 0, 0, 10, 10, 5, 0]-[0, 2, 0],
       [concealment, 'dodge blow', 'follow trail', oneof([heal, search]), 'outdoor survival', perception, 'scale sheer surface', 'silent move'],
       [oneof([marksman, rover]), oneof(['rapid reload', 'warrior born'])],
       [hunter, outrider, scout, vagabond, veteran]).

career(basic,
       marine,
       [10, 10, 10, 0, 5, 0, 5, 0]-[1, 3, 0],
       [oneof(['common knowledge':'the wasteland', gamble]), 'consume alcohol', 'dodge blow', oneof([gossip, 'secret language':'battle tongue']), intimidate, row, swim],
       [oneof([disarm, 'quick draw']), 'strike mighty blow', 'strike to stun'],
       [mate, outlaw, sergeant, smuggler, thug]).

career(basic,
       mercenary,
       [10, 10, 5, 5, 5, 0, 5, 0]-[1, 2, 0],
       [oneof(['animal care', gamble]), oneof(['common knowledge':bretonnia, 'common knowledge':kislev, 'common knowledge':tilea]), 'dodge blow', oneof([drive, ride]), oneof([gossip, haggle]), oneof([perception, search]), 'secret language':'battle tongue', oneof(['speak language':tilean, swim])],
       [oneof([disarm, 'quick draw']), oneof(['rapid reload', 'strike mighty blow']), oneof([sharpshooter, 'strike to stun'])],
       [bodyguard, bounty_hunter, outlaw, sergeant, shieldbreaker, veteran]).

career(basic,
       messenger,
       [5, 5, 0, 5, 10, 5, 5, 0]-[0, 2, 0],
       ['animal care', oneof(['common knowledge':'the empire', 'common knowledge':'the wasteland', gossip]), navigation, 'outdoor survival', 'secret signs':scout, perception, ride, 'speak language':reikspiel, swim],
       [orientation, 'seasoned traveller'],
       [coachman, herald, kithband_warrior, outrider, roadwarden, scout, soldier]).

career(basic,
       militiaman,
       [10, 5, 5, 5, 10, 0, 0, 0]-[0, 2, 0],
       ['animal care', 'dodge blow', oneof([drive, swim]), oneof([gamble, gossip]), 'outdoor survival', perception, search, trade:_],
       [oneof(['specialist weapon group':'two-handed', 'rapid reload']), 'strike mighty blow'],
       [artisan, fieldwarden, mercenary, messenger, outlaw, sergeant, thief]).

career(basic,
       miner,
       [5, 5, 10, 5, 0, 5, 5, 0]-[0, 2, 0],
       ['animal care', oneof([concealment, drive]), oneof([evaluate, 'outdoor survival']), navigation, perception, 'scale sheer surface', oneof([trade:miner, trade:prospector])],
       [orientation, 'specialist weapon group':'two-handed', oneof(['very resilient', 'warrior born'])],
       [charcoal_burner, engineer, mercenary, scout, shieldbreaker, smuggler]).

career(basic,
       noble,
       [10, 5, 0, 0, 5, 5, 5, 10]-[0, 2, 0],
       [oneof([blather, command]), 'common knowledge':'the empire', oneof(['consume alcohol', performer:musician]), charm, oneof([gamble, gossip]), 'read/write', ride, 'speak language':reikspiel],
       [etiquette, oneof([luck, 'public speaking']), oneof([savvy, 'specialist weapon group':fencing]), oneof([schemer, 'specialist weapon group':parrying])],
       [courtier, pistolier, politician, rogue, squire, student]).

career(basic,
       norse_berserker,
       [15, 0, 10, 10, 0, 0, 10, 0]-[0, 2, 0],
       ['common knowledge':norsca, 'consume alcohol', intimidate, performer:storyteller, 'speak language':norse, swim],
       [frenzy, menacing, 'quick draw', 'specialist weapon group':'two-handed'],
       [mercenary, pit_fighter, seaman, sergeant, veteran]).

career(basic,
       outlaw,
       [10, 10, 0, 0, 10, 5, 0, 0]-[1, 2, 0],
       [oneof(['animal care', 'common knowledge':'the empire']), concealment, 'dodge blow', oneof([drive, ride]), oneof([gossip, 'secret signs':thief]), perception, 'scale sheer surface', oneof(['set trap', swim]), 'silent move'],
       [oneof([rover, streetwise]), oneof([sharpshooter, 'strike to stun'])],
       [demagogue, highwayman, thief, vagabond, veteran]).

career(basic,
       outrider,
       [5, 10, 0, 0, 10, 10, 5, 0]-[0, 2, 0],
       ['animal care', 'follow trail', navigation, 'outdoor survival', perception, ride, search, 'silent move'],
       [oneof([coolheaded, 'very strong']), orientation, 'specialist weapon group':entangling],
       [coachman, highwayman, mercenary, roadwarden, scout]).

career(basic,
       peasant,
       [5, 5, 5, 10, 5, 0, 5, 0]-[0, 2, 0],
       [oneof(['animal care', charm]), oneof(['animal training', swim]), oneof(['charm animal', trade:cook]), concealment, oneof([drive, trade:bowyer]), oneof([gamble, performer:dancer, performer:singer]), oneof(['outdoor survival', trade:farmer]), oneof([row, 'set trap']), oneof(['scale sheer surface', 'silent move'])],
       [oneof([hardy, rover]), oneof(['flee!', 'specialist weapon group':sling])],
       [bone_picker, charcoal_burner, fisherman, militiaman, outlaw, politician, servant, tradesman, zealot]).

career(basic,
       pit_fighter,
       [15, 0, 0, 10, 10, 0, 10, 0]-[0, 2, 0],
       ['dodge blow', intimidate],
       [oneof([disarm, wrestling]), oneof(['quick draw', 'strike to injure']), 'specialist weapon group':flail, 'specialist weapon group':parrying, 'specialist weapon group':'two-handed', 'strike mighty blow', oneof(['very strong', 'strong-minded'])],
       [bounty_hunter, mercenary, protagonist, troll_slayer, veteran]).

career(basic,
       protagonist,
       [10, 0, 10, 0, 10, 0, 10, 0]-[1, 2, 0],
       ['dodge blow', oneof([gossip, haggle]), intimidate, ride],
       [oneof([disarm, 'quick draw']), oneof([menacing, suave]), 'street fighting', 'strike mighty blow', 'strike to injure', 'strike to stun'],
       [duellist, pit_fighter, racketeer, thief, thug]).

career(basic,
       rat_catcher,
       [5, 10, 0, 5, 10, 0, 10, 0]-[0, 2, 0],
       ['animal care', 'animal training', concealment, perception, search, 'set trap', 'silent move'],
       ['resistance to disease', 'resistance to poison', 'specialist weapon group':sling, 'tunnel rat'],
       [bone_picker, cat_burglar, grave_robber, jailer, shieldbreaker, thief]).

career(basic,
       roadwarden,
       [10, 10, 5, 0, 10, 5, 5, 0]-[0, 2, 0],
       ['animal care', oneof(['common knowledge':'the empire', gossip]), drive, oneof(['follow trail', 'secret signs':scout]), navigation, 'outdoor survival', perception, ride, search],
       [oneof(['quick draw', 'rapid reload']), 'specialist weapon group':gunpowder],
       [highwayman, messenger, outlaw, outrider, scout, sergeant, toll_keeper]).

career(basic,
       rogue,
       [5, 5, 0, 0, 10, 5, 5, 10]-[0, 2, 0],
       [blather, charm, evaluate, oneof([gamble, 'secret signs':thief]), oneof([gossip, haggle]), perception, oneof([performer:actor, performer:storyteller]), oneof([search, 'secret language':'thieves tongue']), 'speak language':reikspiel],
       [oneof(['flee!', streetwise]), oneof([luck, 'sixth sense']), 'public speaking'],
       [charlatan, demagogue, entertainer, outlaw, servant, thief]).

career(basic,
       runebearer,
       [10, 0, 5, 5, 10, 5, 5, 0]-[0, 2, 0], % XXX arg, movement +1
       ['dodge blow', navigation, 'outdoor survival', 'secret signs':scout, perception, swim],
       ['flee!', oneof(['fleet footed', 'sixth sense']), orientation, 'rapid reload', oneof(['very resilient', 'very strong'])],
       [rat_catcher, scout, shieldbreaker, tomb_robber, veteran]).

career(basic,
       scribe,
       [0, 0, 0, 0, 10, 10, 10, 5]-[0, 2, 0],
       ['academic knowledge':_, oneof(['common knowledge':'the empire', gossip]), perception, 'read/write', 'secret language':'guild tongue', 'speak language':breton, 'speak language':classical, oneof(['speak language':reikspiel, 'speak language':tilean]), trade:calligrapher],
       [linguistics],
       [agitator, apprentice_wizard, initiate, navigator, scholar]).

career(basic,
       seaman,
       [10, 5, 10, 0, 10, 0, 0, 0]-[1, 2, 0],
       [oneof(['common knowledge':bretonnia, 'common knowledge':norsca, 'common knowledge':tilea, 'common knowledge':'the wasteland']), oneof(['consume alcohol', perception]), 'dodge blow', row, sail, 'scale sheer surface', oneof(['speak language':breton, 'speak language':norse, 'speak language':tilean]), swim],
       [oneof([hardy, 'street fighting']), 'seasoned traveller', oneof(['strike mighty blow', swashbuckler])],
       [marine, mate, navigator, rogue, smuggler]).

career(basic,
       servant,
       [5, 0, 5, 0, 10, 5, 10, 5]-[0, 2, 0],
       [oneof(['animal care', trade:cook]), blather, 'dodge blow', oneof([drive, search]), oneof([evaluate, haggle]), gossip, perception, oneof(['read/write', 'sleight of hand'])],
       [oneof(['acute hearing', 'flee!']), oneof([etiquette, hardy]), oneof(['lightning reflexes', 'very resilient'])],
       [agitator, burgher, camp_follower, innkeeper, messenger, spy, thief, valet]).

career(basic,
       shieldbreaker,
       [10, 0, 5, 5, 10, 0, 5, 0]-[1, 2, 0],
       ['dodge blow', navigation, perception, 'scale sheer surface', shadowing],
       [oneof(['acute hearing', coolheaded]), orientation, 'strike mighty blow', 'strike to injure', 'strike to stun'],
       [pit_fighter, runebearer, sergeant, smuggler, tomb_robber, veteran]).

career(basic,
       smuggler,
       [5, 5, 0, 0, 10, 10, 0, 10]-[0, 2, 0],
       [drive, evaluate, oneof([gossip, 'secret language':'thieves tongue']), haggle, perception, row, search, 'silent move', oneof(['speak language':breton, 'speak language':kislevian, 'secret signs':thief]), swim],
       [oneof([dealmaker, streetwise])],
       [boatman, charlatan, fence, ferryman, seaman, shieldbreaker, thief]).

career(basic,
       soldier,
       [10, 10, 0, 0, 10, 0, 5, 0]-[1, 2, 0],
       [oneof(['animal care', heal]), oneof(['common knowledge':'the empire', perception]), 'dodge blow', oneof([drive, ride]), oneof([gamble, gossip]), intimidate],
       [oneof([disarm, 'quick draw']), oneof([sharpshooter, 'strike mighty blow']), oneof(['specialist weapon group':gunpowder, 'specialist weapon group':'two-handed']), oneof(['strike to injure', 'rapid reload']), oneof(['strike to stun', 'mighty shot'])],
       [mercenary, outrider, sergeant, vagabond, veteran, watchman]).

career(basic,
       squire,
       [10, 5, 5, 5, 5, 0, 0, 5]-[1, 2, 0],
       [oneof(['academic knowledge':'genealogy/heraldry', 'common knowledge':bretonnia]), 'animal care', 'animal training', oneof([charm, gossip]), 'dodge blow', ride, oneof(['speak language':breton, 'speak language':reikspiel])],
       [etiquette, 'specialist weapon group':cavalry, 'strike mighty blow'],
       [knight, noble, outlaw, sergeant, veteran]).

career(basic,
       student,
       [0, 0, 0, 0, 10, 10, 5, 10]-[0, 2, 0],
       ['academic knowledge':_, oneof(['academic knowledge':_, gossip]), oneof([charm, 'consume alcohol']), oneof([heal, search]), perception, 'read/write', 'speak language':classical, 'speak language':reikspiel],
       [oneof([etiquette, linguistics]), oneof([savvy, suave]), oneof(['seasoned traveller', 'super numerate'])],
       [agitator, apprentice_wizard, barber_surgeon, engineer, envoy, initiate, physician, scholar]).

career(basic,
       thief,
       [5, 5, 0, 0, 15, 5, 0, 10]-[0, 2, 0],
       [oneof([charm, 'scale sheer surface']), concealment, oneof([evaluate, disguise]), oneof([gamble, 'pick locks']), perception, oneof(['read/write', 'sleight of hand']), search, oneof(['secret language':'thieves tongue', 'secret signs':thief]), 'silent move'],
       [oneof(['alley cat', streetwise]), oneof(['super numerate', trapfinder])],
       [cat_burglar, charlatan, entertainer, fence, rogue, tomb_robber]).

career(basic,
       thug,
       [10, 0, 5, 5, 0, 0, 5, 5]-[1, 2, 0],
       ['consume alcohol', 'dodge blow', gamble, intimidate, 'secret language':'thieves tongue'],
       [oneof([coolheaded, 'lightning reflexes']), disarm, oneof(['resistance to poison', 'quick draw']), oneof(['strike to injure', wrestling]), 'strike to stun'],
       [bodyguard, interrogator, mercenary, pit_fighter, racketeer]).

career(basic,
       toll_keeper,
       [10, 5, 5, 10, 5, 0, 5, 0]-[0, 2, 0],
       ['dodge blow', evaluate, oneof([gossip, haggle]), perception, 'read/write', search, oneof(['speak language':breton, 'speak language':kislevian, 'speak language':tilean])],
       [oneof(['lightning reflexes', marksman])],
       [ferryman, fieldwarden, highwayman, outlaw, soldier, politician, thief]).

career(basic,
       tomb_robber,
       [10, 0, 0, 0, 10, 10, 10, 5]-[0, 2, 0],
       [oneof(['common knowledge':'the empire', 'secret signs':thief]), oneof([concealment, 'outdoor survival']), evaluate, perception, oneof(['pick locks', 'silent move']), 'read/write', 'scale sheer surface', search, oneof(['speak language':classical, 'speak language':khazalid, 'speak language':eltharin])],
       [oneof([luck, 'sixth sense']), oneof([trapfinder, 'tunnel rat'])],
       [fence, rat_catcher, shieldbreaker, thief, vampire_hunter]).

career(basic,
       tradesman,
       [0, 0, 5, 5, 10, 5, 10, 0]-[0, 2, 0],
       [oneof(['animal care', gossip]), drive, haggle, evaluate, perception, 'read/write', 'secret language':'guild tongue', trade:_, trade:_],
       [oneof([dealmaker, savvy])],
       [artisan, engineer, envoy, merchant, militiaman, zealot]).

career(basic,
       troll_slayer,
       [10, 0, 5, 5, 5, 0, 10, 0]-[1, 3, 0],
       ['consume alcohol', 'dodge blow', intimidate],
       [oneof([disarm, 'quick draw']), hardy, oneof(['lightning reflexes', 'very resilient']), 'specialist weapon group':'two-handed', 'street fighting', 'strike mighty blow'],
       [giant_slayer]).

career(basic,
       vagabond,
       [5, 10, 0, 0, 10, 5, 0, 5]-[0, 2, 0],
       [oneof(['common knowledge':bretonnia, 'common knowledge':estalia, 'common knowledge':kislev, 'common knowledge':tilea]), oneof([gossip, 'secret language':'ranger tongue', 'secret language':'thieves tongue']), oneof([haggle, swim]), oneof([heal, perception]), navigation, 'outdoor survival', oneof([performer:dancer, performer:singer, performer:storyteller, 'secret signs':ranger, 'secret signs':thief]), 'silent move'],
       [oneof(['fleet footed', rover]), oneof([marksman, orientation]), 'seasoned traveller'],
       [bone_picker, entertainer, friar, scout, thief, woodsman]).

career(basic,
       valet,
       [0, 0, 0, 0, 10, 10, 5, 10]-[0, 2, 0],
       ['academic knowledge':'genealogy/heraldry', blather, evaluate, oneof([gossip, 'speak language':breton, 'speak language':reikspiel]), haggle, perception, 'read/write', search],
       [oneof([coolheaded, suave]), oneof([dealmaker, 'seasoned traveller']), etiquette],
       [herald, rogue, squire, steward, student]).

career(basic,
       watchman,
       [10, 5, 5, 0, 5, 10, 0, 5]-[0, 2, 0],
       ['academic knowledge':law, 'dodge blow', 'follow trail', gossip, intimidate, perception, search],
       [oneof([coolheaded, savvy]), oneof([disarm, 'street fighting']), 'strike mighty blow', 'strike to stun'],
       [mercenary, racketeer, roadwarden, sergeant, soldier, tradesman]).

career(basic,
       woodsman,
       [10, 0, 10, 0, 5, 0, 10, 0]-[0, 3, 0],
       [concealment, oneof(['follow trail', 'set trap']), perception, 'scale sheer surface', 'secret language':'ranger tongue', 'secret signs':ranger, 'silent move'],
       [oneof(['fleet footed', 'very resilient']), rover, 'specialist weapon group':'two-handed'],
       [hunter, militiaman, outlaw, scout, vagabond]).

career(basic,
       zealot,
       [10, 0, 5, 10, 0, 0, 10, 5]-[0, 2, 0],
       ['academic knowledge':theology, charm, 'common knowledge':'the empire', intimidate, 'read/write'],
       [oneof([coolheaded, 'very strong']), oneof([hardy, suave]), 'public speaking', 'specialist weapon group':flail],
       [agitator, initiate, flagellant, friar, outlaw]).

%%=============================================================================

career(advanced,
       anointed_priest,
       [15, 15, 10, 10, 10, 15, 25, 20]-[1, 5, 2],
       ['academic knowledge':_, 'academic knowledge':_, 'academic knowledge':theology, channelling, charm, 'common knowledge':_, 'common knowledge':_, gossip, heal, 'magical sense', oneof([ride, swim]), 'speak arcane language':magick, 'speak language':_, 'speak language':_],
       [oneof(['aethyric attunement', meditation]), oneof(['armoured casting', 'fast hands']), 'divine lore':_, 'lesser magic':_, 'lesser magic':_, oneof(['seasoned traveller', 'strike mighty blow'])],
       [demagogue, flagellant, high_priest, scholar, witch_hunter]).

career(advanced,
       artisan,
       [0, 0, 10, 10, 20, 10, 10, 10]-[0, 3, 0],
       [drive, evaluate, gossip, haggle, perception, 'secret language':'guild tongue', oneof(['speak language':khazalid, 'speak language':breton, 'speak language':tilean]), trade:_, trade:_, trade:_],
       [oneof([artistic, etiquette])],
       [demagogue, engineer, guild_master, merchant, militiaman]).

career(advanced,
       assassin,
       [25, 25, 10, 10, 30, 20, 10, 20]-[2, 4, 0],
       [concealment, disguise, gossip, perception, 'prepare poison', 'scale sheer surface', 'secret signs':thief, shadowing, 'silent move'],
       ['quick draw', 'lightning parry', sharpshooter, 'specialist weapon group':entangling, 'specialist weapon group':parrying, 'specialist weapon group':throwing, 'street fighting', streetwise, swashbuckler],
       [champion, outlaw_chief, rogue, sergeant, witch_hunter]).

career(advanced,
       captain,
       [30, 20, 20, 20, 20, 15, 15, 25]-[2, 7, 0],
       ['academic knowledge':'strategy/tactics', 'animal care', command, 'common knowledge':_, 'common knowledge':_, 'common knowledge':_, 'dodge blow', gossip, 'read/write', ride, 'secret language':'battle tongue', oneof(['speak language':kislevian, 'speak language':tilean])],
       [oneof([disarm, 'quick draw']), 'lightning parry', oneof(['specialist weapon group':cavalry, 'specialist weapon group':'two-handed']), oneof(['specialist weapon group':flail, 'specialist weapon group':parrying])],
       [agitator, explorer, merchant, outlaw_chief, politician]).

career(advanced,
       cat_burglar,
       [10, 10, 5, 5, 25, 10, 10, 0]-[0, 4, 0],
       [concealment, evaluate, gossip, haggle, perception, 'pick locks', 'scale sheer surface', search, 'secret language':'thieves tongue', 'secret signs':thief, 'silent move'],
       ['alley cat', 'street fighting', streetwise, trapfinder],
       [crime_lord, fence, master_thief, racketeer, vagabond]).

career(advanced,
       champion,
       [40, 40, 25, 25, 30, 0, 20, 0]-[2, 8, 0],
       ['dodge blow', evaluate, intimidate, perception],
       [oneof(['fleet footed', 'lightning reflexes']), 'lightning parry', 'master gunner', 'mighty shot', 'quick draw', 'rapid reload', 'specialist weapon group':_, 'specialist weapon group':_, 'specialist weapon group':_, wrestling],
       [assassin, scout, sergeant, witch_hunter]).

career(advanced,
       charlatan,
       [10, 10, 5, 10, 15, 15, 15, 25]-[0, 4, 0],
       [blather, charm, oneof(['common knowledge':bretonnia, 'common knowledge':tilea]), disguise, evaluate, gamble, gossip, haggle, perception, 'secret language':'thieves tongue', 'sleight of hand', oneof(['speak language':breton, 'speak language':tilean]), 'speak language':reikspiel],
       ['flee!', mimic, 'public speaking', oneof([schemer, streetwise]), 'seasoned traveller'],
       [cat_burglar, demagogue, outlaw, politician, spy]).

career(advanced,
       courtier,
       [5, 5, 0, 0, 10, 20, 20, 20]-[0, 4, 0],
       [oneof(['academic knowledge':'the arts', 'academic knowledge':history, gamble]), blather, charm, oneof([command, performer:_]), oneof(['common knowledge':bretonnia, 'common knowledge':tilea]), evaluate, gossip, perception, 'read/write', ride, oneof(['speak language':breton, 'speak language':tilean]), 'speak language':reikspiel],
       [oneof([dealmaker, etiquette]), 'public speaking', oneof([savvy, suave]), oneof([schemer, 'specialist weapon group':fencing])],
       [charlatan, duellist, noble_lord, politician, steward, spy]).

career(advanced,
       crime_lord,
       [20, 20, 15, 15, 20, 25, 20, 30]-[1, 6, 0],
       [charm, command, 'common knowledge':'the empire', 'dodge blow', evaluate, gossip, haggle, intimidate, perception, 'secret language':'thieves tongue', 'secret signs':thief, torture],
       [oneof([dealmaker, schemer]), menacing, 'public speaking', 'resistance to poison', 'sixth sense', oneof(['specialist weapon group':crossbow, 'specialist weapon group':parrying]), streetwise],
       [demagogue, master_thief, outlaw_chief, politician]).

career(advanced,
       daemon_slayer,
       [40, 0, 30, 30, 20, 0, 30, 0]-[2, 8, 0],
       ['common knowledge':_, 'common knowledge':_, 'consume alcohol', 'dodge blow', intimidate, 'scale sheer surface'],
       ['lightning parry', unsettling],
       []).

career(advanced,
       demagogue,
       [10, 10, 0, 10, 15, 20, 15, 30]-[1, 4, 0],
       ['academic knowledge':history, 'academic knowledge':law, blather, charm, command, 'common knowledge':'the empire', concealment, disguise, 'dodge blow', gossip, intimidate, perception, 'speak language':reikspiel],
       [oneof([etiquette, streetwise]), 'master orator', 'public speaking', 'street fighting'],
       [crime_lord, friar, mercenary, outlaw_chief, politician]).

career(advanced,
       duellist,
       [20, 20, 10, 20, 20, 15, 15, 10]-[1, 4, 0],
       [charm, 'dodge blow', gamble, gossip, intimidate, perception, 'sleight of hand'],
       [oneof([ambidextrous, disarm]), etiquette, 'master gunner', 'mighty shot', 'quick draw', sharpshooter, 'specialist weapon group':fencing, 'specialist weapon group':gunpowder, 'specialist weapon group':parrying, 'strike mighty blow', 'strike to injure', swashbuckler],
       [assassin, champion, highwayman, rogue, sergeant]).

career(advanced,
       engineer,
       [10, 15, 5, 5, 10, 20, 10, 0]-[0, 4, 0],
       ['academic knowledge':engineering, 'academic knowledge':science, oneof(['common knowledge':dwarfs, 'common knowledge':tilea]), oneof([drive, ride]), perception, 'read/write', oneof(['speak language':khazalid, 'speak language':tilean]), trade:gunsmith],
       ['master gunner', oneof(['specialist weapon group':engineer, 'specialist weapon group':gunpowder])],
       [artisan, explorer, guild_master, pistolier, smuggler]).

career(advanced,
       explorer,
       [20, 20, 10, 15, 15, 25, 20, 15]-[1, 6, 0],
       [oneof(['academic knowledge':history, 'academic knowledge':law]), command, 'common knowledge':_, 'common knowledge':_, 'common knowledge':_, drive, evaluate, 'follow trail', navigation, 'outdoor survival', perception, 'read/write', ride, 'scale sheer surface', 'secret language':'ranger tongue', 'secret signs':scout, 'speak language':_, 'speak language':_, 'speak language':_, swim, trade:cartographer],
       [oneof([orientation, linguistics]), 'seasoned traveller'],
       [captain, merchant, sea_captain, spy]).

career(advanced,
       fence,
       [15, 10, 10, 5, 10, 5, 10, 10]-[1, 4, 0],
       [evaluate, gamble, gossip, haggle, intimidate, perception, 'sleight of hand'],
       [oneof([dealmaker, streetwise]), 'strike to stun', 'super numerate'],
       [charlatan, crime_lord, master_thief, racketeer]).

career(advanced,
       flagellant,
       [15, 0, 10, 15, 5, 0, 20, 10]-[1, 6, 0],
       ['academic knowledge':theology, charm, heal, intimidate, 'speak language':classical],
       [fearless, oneof(['specialist weapon group':flail, 'specialist weapon group':'two-handed']), 'strike mighty blow'],
       [demagogue, interrogator, priest, soldier, veteran]).

career(advanced,
       friar,
       [10, 0, 5, 10, 0, 15, 15, 15]-[0, 4, 0],
       ['academic knowledge':theology, 'animal care', 'common knowledge':_, 'common knowledge':_, heal, 'outdoor survival', perception, oneof(['speak language':breton, 'speak language':estalian, 'speak language':kislevian, 'speak language':tilean]), 'speak language':classical, 'speak language':reikspiel],
       ['seasoned traveller'],
       [demagogue, flagellant, priest, scholar]).

career(advanced,
       ghost_strider,
       [20, 30, 15, 15, 25, 20, 20, 0]-[2, 6, 0],
       [concealment, 'dodge blow', 'follow trail', intimidate, 'lip reading', navigation, 'outdoor survival', perception, 'secret language':'ranger tongue', 'secret signs':ranger, 'set trap', shadowing, 'silent move'],
       [oneof([hardy, 'fleet footed']), 'lightning parry', 'mighty shot', 'quick draw', 'rapid reload', 'sure shot'],
       [captain, outlaw_chief, targeteer, vampire_hunter]).

career(advanced,
       giant_slayer,
       [25, 0, 15, 15, 10, 0, 20, 0]-[1, 6, 0],
       ['common knowledge':_, 'consume alcohol', 'dodge blow', intimidate, perception],
       [fearless, 'resistance to poison', 'specialist weapon group':flail, 'strike to injure'],
       [daemon_slayer]).

career(advanced,
       guild_master,
       [10, 10, 0, 10, 15, 30, 20, 35]-[1, 5, 0],
       ['academic knowledge':history, charm, command, 'common knowledge':'the empire', evaluate, gossip, haggle, perception, 'secret language':'guild tongue', oneof(['speak language':breton, 'speak language':estalian, 'speak language':kislevian, 'speak language':norse]), 'speak language':reikspiel, trade:_, trade:_],
       [dealmaker, etiquette, linguistics],
       [crime_lord, politician, racketeer]).

career(advanced,
       herald,
       [10, 10, 5, 5, 15, 15, 10, 20]-[0, 4, 0],
       ['academic knowledge':'genealogy/heraldry', 'academic knowledge':history, blather, charm, oneof(['common knowledge':bretonnia, 'common knowledge':kislev, 'common knowledge':tilea]), 'common knowledge':'the empire', evaluate, gossip, haggle, perception, 'read/write', ride, oneof(['speak language':breton, 'speak language':kislevian, 'speak language':tilean]), 'speak language':reikspiel],
       [etiquette, 'master orator', 'public speaking'],
       [agitator, courtier, entertainer, explorer, politician, squire]).

career(advanced,
       high_priest,
       [20, 20, 15, 15, 15, 20, 30, 25]-[1, 6, 3],
       ['academic knowledge':_, 'academic knowledge':_, 'academic knowledge':_, 'academic knowledge':theology, channelling, charm, 'common knowledge':_, 'common knowledge':_, gossip, heal, intimidate, 'magical sense', oneof([ride, swim]), 'speak arcane language':magick, 'speak language':_, 'speak language':_, 'speak language':_],
       [oneof(['aethyric attunement', meditation]), oneof(['armoured casting', 'mighty missile']), etiquette, oneof(['fast hands', 'strong-minded']), 'lesser magic':_, 'lesser magic':_],
       [politician, scholar, witch_hunter]).

career(advanced,
       highwayman,
       [20, 20, 10, 10, 30, 20, 15, 25]-[1, 4, 0],
       ['animal care', 'animal training', charm, 'common knowledge':'the empire', evaluate, gossip, ride, 'silent move'],
       [ambidextrous, etiquette, 'master gunner', 'mighty shot', sharpshooter, 'specialist weapon group':fencing, 'specialist weapon group':gunpowder, swashbuckler, 'trick riding'],
       [agitator, duellist, master_thief, outlaw_chief, sergeant]).

career(advanced,
       innkeeper,
       [10, 5, 5, 10, 20, 10, 10, 20]-[0, 4, 0],
       [oneof([blather, 'lip reading']), charm, 'common knowledge':'the empire', 'consume alcohol', evaluate, gossip, haggle, perception, oneof(['read/write', 'sleight of hand']), oneof(['speak language':breton, 'speak language':kislevian, 'speak language':reikspiel, 'speak language':tilean]), trade:cook],
       [oneof([etiquette, streetwise]), oneof([dealmaker, 'street fighting']), 'strike to stun'],
       [burgher, fence, merchant, outlaw, smuggler]).

career(advanced,
       interrogator,
       [15, 0, 20, 10, 10, 10, 20, 15]-[0, 4, 0],
       [charm, heal, intimidate, perception, torture],
       [menacing, 'specialist weapon group':flail, wrestling],
       [physician, racketeer, thief]).

career(advanced,
       journeyman_wizard,
       [5, 5, 0, 5, 10, 20, 25, 10]-[0, 3, 2],
       ['academic knowledge':magic, 'academic knowledge':_, channelling, oneof([charm, intimidate]), 'common knowledge':_, 'common knowledge':_, gossip, 'magical sense', 'read/write', oneof([ride, swim]), 'speak arcane language':magick, 'speak language':_, 'speak language':_],
       [oneof(['arcane lore':_, 'dark lore':_]), oneof(['aethyric attunement', 'dark magic']), oneof(['fast hands', 'very resilient']), 'lesser magic':_, 'lesser magic':_, oneof([meditation, 'mighty missile'])],
       [charlatan, master_wizard, scholar]).

career(advanced,
       judicial_champion,
       [35, 0, 15, 15, 20, 10, 15, 0]-[2, 6, 0],
       ['dodge blow', perception],
       ['lightning parry', 'specialist weapon group':fencing, 'specialist weapon group':flail, 'specialist weapon group':parrying, 'specialist weapon group':'two-handed'],
       [assassin, champion, sergeant, witch_hunter, zealot]).

career(advanced,
       knight,
       [25, 0, 15, 15, 15, 5, 15, 5]-[1, 4, 0],
       [oneof(['academic knowledge':'genealogy/heraldry', 'academic knowledge':theology]), 'academic knowledge':'strategy/tactics', 'dodge blow', perception, ride, 'secret language':'battle tongue', 'speak language':_, 'speak language':_],
       ['specialist weapon group':cavalry, 'specialist weapon group':flail, 'specialist weapon group':'two-handed', 'strike mighty blow'],
       [captain, initiate, knight_of_the_inner_circle, noble_lord, vampire_hunter]).

career(advanced,
       knight_of_the_inner_circle,
       [35, 10, 20, 20, 20, 15, 25, 15]-[2, 8, 0],
       [oneof(['academic knowledge':'genealogy/heraldry', 'academic knowledge':theology]), 'academic knowledge':'strategy/tactics', 'animal training', charm, command, 'common knowledge':_, 'common knowledge':_, 'common knowledge':_, 'dodge blow', perception, 'read/write', ride, 'secret language':'battle tongue', oneof(['secret signs':scout, 'secret signs':templar]), oneof(['speak language':breton, 'speak language':estalian, 'speak language':kislevian, 'speak language':tilean])],
       [etiquette, 'lightning parry', 'seasoned traveller', 'specialist weapon group':fencing, 'specialist weapon group':parrying, 'stout-hearted', 'strike to injure', 'strike to stun'],
       [captain, champion, noble_lord, witch_hunter]).

career(advanced,
       master_thief,
       [20, 20, 10, 10, 40, 25, 20, 25]-[1, 6, 0],
       [charm, concealment, disguise, 'dodge blow', evaluate, oneof([gamble, 'lip reading']), gossip, perception, 'pick locks', 'read/write', 'scale sheer surface', 'secret language':'thieves tongue', 'secret signs':thief, search, 'silent move', 'sleight of hand', swim],
       ['specialist weapon group':crossbow, 'specialist weapon group':throwing, oneof(['street fighting', swashbuckler]), streetwise, trapfinder],
       [crime_lord, explorer, outlaw_chief, targeteer]).

career(advanced,
       master_wizard,
       [10, 10, 0, 10, 15, 30, 35, 15]-[0, 4, 3],
       ['academic knowledge':magic, 'academic knowledge':_, 'academic knowledge':_, channelling, oneof([charm, intimidate]), 'common knowledge':_, 'common knowledge':_, oneof([gossip, ride]), 'magical sense', 'read/write', 'speak arcane language':magick, oneof(['speak arcane language':daemonic, 'speak arcane language':'arcane elf']), 'speak language':_, 'speak language':_, 'speak language':_],
       [oneof(['aethyric attunement', meditation]), oneof(['dark magic', 'strong-minded']), oneof(['fast hands', 'mighty missile']), 'lesser magic':_, 'lesser magic':_],
       [explorer, scholar, wizard_lord]).

career(advanced,
       mate,
       [15, 15, 10, 15, 10, 10, 10, 10]-[1, 4, 0],
       [command, 'common knowledge':_, 'common knowledge':_, 'consume alcohol', 'dodge blow', gamble, gossip, intimidate, row, sail, oneof(['speak language':breton, 'speak language':kislevian, 'speak language':norse]), swim, trade:shipwright],
       ['resistance to disease', 'seasoned traveller', 'street fighting'],
       [explorer, merchant, navigator, sea_captain]).

career(advanced,
       merchant,
       [10, 10, 5, 5, 10, 25, 20, 20]-[0, 4, 0],
       [charm, 'common knowledge':_, 'common knowledge':_, drive, evaluate, gossip, haggle, 'read/write', ride, 'secret language':'guild tongue', oneof(['speak language':breton, 'speak language':estalian, 'speak language':kislevian, 'speak language':norse]), 'speak language':reikspiel, trade:merchant],
       [oneof([dealmaker, streetwise]), 'super numerate'],
       [guild_master, militiaman, politician, racketeer, spy]).

career(advanced,
       minstrel,
       [10, 10, 0, 0, 15, 10, 5, 25]-[0, 4, 0],
       [charm, 'common knowledge':_, 'common knowledge':_, gossip, perception, performer:musician, performer:singer, 'read/write', oneof(['speak language':breton, 'speak language':eltharin, 'speak language':tilean])],
       [etiquette, 'public speaking'],
       [charlatan, demagogue, highwayman, spy, student]).

career(advanced,
       navigator,
       [10, 10, 5, 5, 10, 25, 10, 5]-[0, 4, 0],
       ['academic knowledge':astronomy, 'common knowledge':_, 'common knowledge':_, navigation, perception, 'read/write', 'speak language':classical, swim, trade:cartographer],
       [orientation],
       [artisan, explorer, scholar, sea_captain]).

career(advanced,
       noble_lord,
       [25, 15, 10, 10, 10, 20, 20, 30]-[1, 6, 0],
       [oneof(['academic knowledge':history, 'academic knowledge':'strategy/tactics']), 'academic knowledge':'genealogy/heraldry', charm, command, 'common knowledge':'the empire', evaluate, gossip, perception, 'read/write', ride, 'speak language':classical, 'speak language':reikspiel],
       ['master orator', 'public speaking', 'specialist weapon group':fencing],
       [captain, knight, scholar, sea_captain]).

career(advanced,
       outlaw_chief,
       [20, 30, 10, 20, 10, 10, 10, 20]-[2, 6, 0],
       ['academic knowledge':'strategy/tactics', command, 'common knowledge':'the empire', concealment, 'follow trail', perception, ride, 'scale sheer surface', 'secret language':'battle tongue', 'secret language':'thieves tongue', oneof(['secret signs':scout, 'secret signs':thief]), 'silent move'],
       ['lightning parry', 'mighty shot', 'quick draw', 'rapid reload', 'sure shot'],
       [assassin, captain, crime_lord, demagogue]).

career(advanced,
       physician,
       [0, 0, 10, 10, 15, 30, 20, 15]-[0, 4, 0],
       ['academic knowledge':science, heal, gossip, perception, 'prepare poison', 'read/write', 'speak language':classical, trade:apothecary],
       ['resistance to disease', 'strike to stun', surgery],
       [friar, guild_master, scholar, spy]).

career(advanced,
       pistolier,
       [20, 20, 10, 10, 15, 0, 15, 15]-[1, 4, 0],
       ['animal care', 'dodge blow', oneof([evaluate, gossip]), perception, ride, 'secret signs':scout],
       ['master gunner', 'quick draw', 'rapid reload', sharpshooter, 'specialist weapon group':gunpowder, 'strike mighty blow', 'sure shot'],
       [courtier, duellist, knight, sergeant, veteran]).

career(advanced,
       politician,
       [5, 5, 5, 10, 0, 20, 10, 20]-[0, 4, 0],
       [oneof(['academic knowledge':history, 'academic knowledge':'genealogy/heraldry']), 'academic knowledge':law, blather, charm, command, 'common knowledge':'the empire', evaluate, gossip, haggle, perception, performer:actor, 'read/write', 'speak language':reikspiel],
       [oneof([dealmaker, schemer]), oneof([etiquette, streetwise]), 'master orator', 'public speaking'],
       [courtier, crime_lord, demagogue, noble_lord, racketeer, steward]).

career(advanced,
       priest,
       [10, 10, 5, 10, 5, 10, 20, 15]-[0, 4, 1],
       ['academic knowledge':_, 'academic knowledge':theology, channelling, charm, 'common knowledge':_, 'common knowledge':_, gossip, heal, 'magical sense', perception, 'read/write', oneof([ride, swim]), 'speak arcane language':magick, 'speak language':_, 'speak language':_],
       [oneof(['armoured casting', 'master orator']), 'petty magic':divine, oneof(['strike to injure', 'strike to stun'])],
       [anointed_priest, flagellant, scholar, steward]).

career(advanced,
       racketeer,
       [20, 15, 15, 10, 10, 0, 15, 10]-[1, 5, 0],
       [command, 'common knowledge':'the empire', 'dodge blow', evaluate, gossip, haggle, intimidate, perception, shadowing],
       [menacing, 'street fighting', streetwise, 'strike mighty blow', 'strike to stun'],
       [fence, master_thief, politician, outlaw_chief]).

career(advanced,
       scholar,
       [5, 5, 5, 5, 10, 30, 15, 15]-[0, 4, 0],
       ['academic knowledge':_, 'academic knowledge':_, 'academic knowledge':_, 'common knowledge':_, 'common knowledge':_, 'common knowledge':_, oneof([evaluate, trade:cartographer]), perception, 'read/write', 'speak language':_, 'speak language':_, 'speak language':_, 'speak language':classical],
       [linguistics],
       [apprentice_wizard, explorer, friar, merchant, physician, steward]).

career(advanced,
       scout,
       [20, 20, 10, 10, 15, 20, 15, 0]-[1, 6, 0],
       ['animal care', 'charm animal', 'common knowledge':_, 'common knowledge':_, concealment, 'dodge blow', 'follow trail', navigation, perception, ride, 'secret language':'ranger tongue', 'secret signs':scout, 'silent move', 'speak language':_, 'speak language':_],
       [oneof(['mighty shot', 'sure shot']), orientation, 'rapid reload', oneof(['specialist weapon group':crossbow, 'specialist weapon group':longbow])],
       [explorer, outlaw_chief, sergeant, vampire_hunter, ghost_strider]).

career(advanced,
       sea_captain,
       [25, 20, 15, 20, 20, 20, 25, 30]-[2, 6, 0],
       ['academic knowledge':'strategy/tactics', 'animal training', command, 'common knowledge':_, 'common knowledge':_, 'common knowledge':_, 'dodge blow', perception, sail, 'speak language':_, 'speak language':_, 'speak language':_, swim, oneof([trade:cartographer, trade:shipwright])],
       [disarm, oneof(['lightning parry', swashbuckler]), 'seasoned traveller', 'specialist weapon group':fencing, 'strike mighty blow'],
       [explorer, noble_lord, scholar, spy]).

career(advanced,
       sergeant,
       [20, 15, 10, 10, 10, 10, 10, 20]-[1, 4, 0],
       ['academic knowledge':'strategy/tactics', command, 'common knowledge':_, 'common knowledge':_, 'dodge blow', gossip, intimidate, perception, oneof([ride, swim]), 'secret language':'battle tongue', 'speak language':tilean],
       [oneof([menacing, 'seasoned traveller']), oneof(['street fighting', wrestling]), 'strike mighty blow', 'strike to stun'],
       [captain, duellist, judicial_champion, knight]).

career(advanced,
       spy,
       [15, 15, 5, 10, 20, 20, 35, 20]-[1, 4, 0],
       [charm, 'common knowledge':_, 'common knowledge':_, concealment, disguise, gossip, 'lip reading', performer:actor, 'pick locks', shadowing, 'sleight of hand', 'secret language':_, 'silent move', 'speak language':_, 'speak language':_, 'speak language':_],
       ['flee!', linguistics, schemer, oneof([suave, 'sixth sense'])],
       [assassin, explorer, master_thief, racketeer]).

career(advanced,
       steward,
       [10, 10, 10, 10, 0, 30, 20, 25]-[0, 4, 0],
       ['academic knowledge':law, charm, command, 'common knowledge':'the empire', evaluate, gossip, haggle, intimidate, perception, 'read/write', ride, search, 'speak language':reikspiel, trade:merchant],
       ['public speaking', 'super numerate'],
       [crime_lord, fence, merchant, noble]).

career(advanced,
       targeteer,
       [0, 35, 10, 10, 25, 10, 20, 15]-[1, 4, 0],
       ['common knowledge':'the empire', gossip, 'outdoor survival', perception, search, 'sleight of hand'],
       ['mighty shot', 'rapid reload', sharpshooter, 'specialist weapon group':longbow, oneof(['specialist weapon group':crossbow, 'specialist weapon group':throwing]), 'sure shot'],
       [assassin, champion, duellist, sergeant]).

career(advanced,
       vampire_hunter,
       [20, 20, 10, 20, 15, 15, 20, 0]-[1, 4, 0],
       [oneof(['academic knowledge':history, 'academic knowledge':necromancy]), 'common knowledge':'the empire', concealment, 'dodge blow', 'follow trail', perception, 'scale sheer surface', shadowing, search, 'silent move', 'speak language':classical],
       [oneof(['mighty shot', 'rapid reload']), 'specialist weapon group':crossbow, 'stout-hearted', 'strike mighty blow', 'strike to injure', 'tunnel rat'],
       [demagogue, initiate, knight, targeteer, witch_hunter]).

career(advanced,
       veteran,
       [20, 20, 10, 10, 15, 0, 15, 0]-[1, 6, 0],
       ['common knowledge':'the empire', 'consume alcohol', 'dodge blow', gamble, gossip, intimidate, perception, 'secret language':'battle tongue'],
       [oneof(['mighty shot', 'strike mighty blow']), oneof(['rapid reload', 'strike to injure']), 'specialist weapon group':_, 'specialist weapon group':_, oneof(['very resilient', 'very strong'])],
       [champion, judicial_champion, outlaw_chief, sergeant, targeteer]).

career(advanced,
       witch_hunter,
       [30, 30, 15, 15, 15, 15, 35, 20]-[2, 6, 0],
       ['academic knowledge':magic, 'academic knowledge':necromancy, 'academic knowledge':theology, charm, command, 'common knowledge':'the empire', gossip, intimidate, perception, ride, search, 'silent move', 'speak language':_],
       ['lightning parry', oneof(['lightning reflexes', marksman]), menacing, 'public speaking', 'sixth sense', 'specialist weapon group':crossbow, 'specialist weapon group':entangling, 'specialist weapon group':throwing, 'stout-hearted', 'strike mighty blow'],
       [captain, demagogue, initiate, knight_of_the_inner_circle]).

career(advanced,
       wizard_lord,
       [15, 15, 5, 15, 20, 35, 40, 20]-[0, 5, 4],
       ['academic knowledge':magic, 'academic knowledge':_, 'academic knowledge':_, 'academic knowledge':_, channelling, oneof([charm, intimidate]), 'common knowledge':_, 'common knowledge':_, 'common knowledge':_, 'magical sense', 'read/write', 'speak arcane language':magick, oneof(['speak arcane language':daemonic, 'speak arcane language':'arcane elf']), 'speak language':_, 'speak language':_, 'speak language':_, 'speak language':_],
       [oneof(['aethyric attunement', 'mighty missile']), oneof(['dark magic', meditation]), oneof(['fast hands', hardy]), 'lesser magic':_, 'lesser magic':_],
       [explorer, guild_master]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Integrity Constraints %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- careers(Cs), is_set(Cs).
:- careers(Cs), \+ (exits(_, Es), member(E, Es), \+ member(E, Cs)).
:- skills(Ss), talents(Ts), \+ (member(X, Ss), member(X, Ts)).
:- \+ (\+ valid_career(C)).

%%% eof
