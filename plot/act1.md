# Possible commands:
 - DESCRIBE
 - GO [SOMWERE]
 - EXAMINE [SOMETHING]
 - TAKE [SOMETHING]
 - DROP [SOMETHING]
 - USE [SOMETHING]
 - TALK TO [SOMEONE]
 - HINT
 - TELL [NUMBER]


# Act 1: Departure from the Edge of the World

## (command: DESCRIBE, executes automaticly)
You awaken to a stark view from your window at an Antarctic base camp in New Swabia. A desolate expanse of ice and snow stretches endlessly under a pale, gray sky. You step outside your BARRACK. Nearby, a sturdy twin-engine plane rests on a makeshift RUNWAY, its metal hull glinting faintly in the weak sunlight. To the side, there’s a fuel DEPOT and a supply TENT. The air is frigid, the wind howls intermittently, and the isolation weighs heavily. By the plane, you spot your partner, Lt. CLARA Voss, a pragmatic military pilot assigned to join you on this mission.

## CURRENT POSITION: START

(posible commands: DESCRIBE, GO RUNWAY, GO DEPOT, GO TENT, GO BARRACK, HINT, TALK TO CLARA)
HINT:
"I think I should talk with Clara."

TALK TO CLARA:
"I’m not going to shout; I should go to her."

GO [LOCATION]:
*You head to another location.*

## GO BARRACK:
DESCRIBE (automaticly)
This is your resting place during the mission—small but convenient. Your bed is neatly made, and a PHOTO of your late wife sits on the dresser beside it. Across the room, your working desk holds mission documents, a small lamp, and a LIGHTER. A CALENDAR hangs above the desk.

HINT:
"I should gather something useful."

EXAMINE PHOTO:
"I’ll never forget you, my love."

TAKE PHOTO:
"Sorry, my love, but I can’t take you with me."
*Does not take photo.*

EXAMINE LIGHTER:
"I really should quit smoking."

TAKE LIGHTER:
"This might come in handy."
*takes lighter*

DROP LIGHTER:
"I might need something else instead."
*Drops lighter.*

EXAMINE CALENDAR:
"August 26, 1946"

TAKE CALENDAR:
"I doubt this will be useful."
*Does not take calendar.*

TALK TO CLARA:
"I’m not going to shout; I should go to her."

## GO RUNWAY:

DESCRIBE (automaticly):
The sunlight, reflected off the steel plates, blinds you as you approach the aircraft—a Douglas A-20 Havoc. It’s not the newest PLANE, but it’s reliable. CLARA is tinkering with one of the engines.

HINT:
"I should talk to Clara."

EXAMINE PLANE:
"Your type served well in the war."

TALK TO CLARA:
Clara: "Morning, doc. We need to get this bird fueled up. Could you check the TANKS for me?"
    Your choises:
    1. "Okay, I’ll handle it now."
        Unlocks EXAMINE TANKS and new HINT: "I should check the fuel tanks."
    2. "I think it’s good enough, but I could double-check."
        Clara: "Good enough doesn’t cut it out here. Antarctica doesn’t forgive mistakes. Check it properly."
        Unlocks EXAMINE TANKS and new HINT: "I should check the fuel tanks."
    3. "Why don’t you take care of it?"
        Clara: (frowning) "Oh, you’re lazy, aren’t you? Fine, I’ll handle it after I finish checking the oil, but you’re not off the hook, doc. Go gather mandatory supplies and drop them near the plane."
        (Skips fueling the plane task; starts supplies task)

EXAMINE TANKS:
*You crouch beside the aircraft and open the fuel hatch.*
"We’re running low. We need at least one more drum of fuel."
Clara: "Told you. Go grab one from the depot."
(Starts fuel task)

## GO DEPOT
DESCRIBE(automaticly)
You step into the depot, a rough but functional structure shielding fuel CANISTERs from the Antarctic cold.

HINT:
(If on fuel task) "I should gather some fuel."
(If not talked to Clara) "I should talk to Clara first."

EXAMINE CANISTER:
"Heavy, but necessary."

TAKE CANISTER
*takes canister*

DROP CANISTER
"I should get it to clara"
*does not drop cansiter*

## GO RUNWAY (after getting canister)
DESCRIBE (automaticly):
The sunlight, reflected off the steel plates, blinds you as you approach the Douglas A-20 Havoc—a reliable, if not modern, PLANE. Clara is still tinkering with one of the engines.

HINT:
"I should talk to Clara."

EXAMINE PLANE:
"Your type served well on the war"

TALK TO CLARA:
"I have it!"
Clara: "Nice, hand it over—our bird’s thirsty."
*starts fueling the plane*
Clara: "Why don’t you gather some supplies while I finish fueling?"
    Your choises:
    1. "On my way."
        (Starts supplies task)
    2. "Are you sure you can handle it by yourself?"
        Clara: "Don’t worry, doc—I’m not a kid. Go grab those supplies."
        (Starts supplies task)

HINT:
"I should gather supplies in the supply TENT."

## GO TENT
DESCRIBE (automatically):
You enter the supply tent, a cramped space cluttered with gear. Boxes and crates are labeled with essentials: food, water, scientific tools, and survival equipment. A LIST of stock hangs on the wall.

HINT:
"I should take only the most necessary items for the mission."

EXAMINE LIST:
- FOOD Rations
- WATER
- GEIGER Counter
- First aid KIT
- RADIO
- Climbing GEAR
- Navigation TOOLS
"The plane has a capacity of only 5 items, I must choose intelligently."


EXAMINE FOOD:
"Canned goods and dried meals. Enough to last two weeks, but not exactly gourmet."

EXAMINE WATER:
"Several canisters of fresh water. No telling what we might find out there, better to have our own supply."

EXAMINE GEIGER:
"A standard radiation detector. If we stumble upon something unnatural, this could be crucial."

EXAMINE KIT:
"Bandages, antiseptic, morphine... Everything needed for basic field medical care."

EXAMINE RADIO:
"A shortwave field radio. Not the best range, but it should work if we’re within contact distance of the base."

EXAMINE CLIMBING GEAR:
"Ropes, pitons, carabiners—if we need to descend into something deep or climb out of trouble, this will help."

EXAMINE NAVIGATION TOOLS:
"A compass, maps, and a sextant. Old-school but reliable."

TAKING ITEMS:
TAKE FOOD:
"Essential for survival. I don’t plan on starving out there." takes food

TAKE WATER:
"Dehydration is just as dangerous as the cold." takes water

TAKE GEIGER:
"If we’re dealing with something unnatural, this might be useful." takes Geiger counter

TAKE FIRST AID KIT (change to MEDKIT):
"Better to be safe than sorry." takes first aid kit

TAKE RADIO:
"If we lose contact, this might be our only way to call for help." takes radio

TAKE CLIMBING GEAR:
"If we have to scale ice walls or descend into caves, we’ll need this." takes climbing gear

TAKE NAVIGATION TOOLS:
"We can’t afford to get lost." takes navigation tools

DROPPING ITEMS:
DROP FOOD:
"I hope we won’t regret this." drops food

DROP WATER:
"Maybe there’s another source where we’re headed." drops water

DROP GEIGER:
"If there’s nothing radioactive, it’s just extra weight." drops Geiger counter

DROP FIRST AID KIT:
"Risky move, but I might need something else more." drops first aid kit

DROP RADIO:
"We’ll just have to rely on good old-fashioned shouting." drops radio

DROP CLIMBING GEAR:
"Hopefully, no steep cliffs on this trip." drops climbing gear

DROP NAVIGATION TOOLS:
"If Clara can fly straight, maybe we won’t need these." drops navigation tools


## GO RUNWAY

DESCRIPTION ( automaticly)
Clara has finished fueling and has something waiting for you. You pack the supplies into the plane. The reason for your journey—Admiral Byrd’s diary—lies open on a box in front of you, its cryptic coordinates circled in red ink: 70°S, 10°E. Clara hands you a cup of lukewarm coffee.

HINT:
"I should thank her for the coffee."

TALK TO CLARA:
"Thank you!"
*a moment of silence*
Clara: "So, tell me again why we’re risking our necks for this? A diary from some explorer doesn’t scream ‘top priority’ to me."

Your choices:
    1."Because it could be the discovery of the century."
    2."Orders are orders. The government wants answers."
    3."I’ve got a feeling there’s something big waiting for us."

    Choice 1:
        Clara: "Discovery of the century? I hope it’s not just a pile of ice and a frostbite bill."
        Your choices:
            a. "Byrd wasn’t a dreamer. Those coordinates mean something."
            b. "Even if it’s nothing, the science alone is worth it."
            Clara (to a): "Maybe. But I’d rather not die proving him right."
    Choice 2:
        Clara: "Yeah, and Uncle Sam loves sending us into the freezer for kicks. What’s their angle?"
        You: "Cold War jitters, probably. They don’t want the Soviets sniffing around first."
    Choice 3:
        Clara: "Feelings don’t keep us warm, doc. What’s in that diary that’s got you hooked?"
        You: "Hints of a hidden land—geological oddities, maybe more."



You: "What do you think we’ll find out there?"

Clara: "Best case? A rock formation worth naming. Worst case? A grave with our names on it. I don’t buy the unearthly land garbage."

You: "Neither do I, but the government does."

Clara: "I think it's time we have a good wheater"

Preparations complete, you and Clara climb into the plane’s hatch. Clara starts the engines, ready to challenge the icy wilderness. The plane roars to life, cutting through swirling snow as it lifts off. Inside, you study the diary while Clara grips the yoke. The horizon swallows the base camp, leaving you with a mix of anticipation—and a hint of lurking danger.

## UWAGI
- podawał bym tytuły aktów w gierce - będą cool.
- trzeba obsłużyć sytuacje ponownego użycia TELL TO np. powtórzenie ostatniej odpowiedzi.