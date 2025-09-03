;;; These really can't be trusted, they need to be retrieved from
;;;   https://api.wynncraft.com/v2/recipe/get/Dagger-80-83 (e.g.)
;;; The list of all of them is at https://api.wynncraft.com/v2/recipe/list
;;; (And need to change all level to level and minimum and maximum)

(setf *recipes*
      '(
	(kind "wynncraft/recipe/get/Boots-3-5" code 200 timestamp 1610855726478 version "2.1.0" 
	 data ((level (3 5) type "BOOTS" skill "TAILORING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Wheat String" amount 2)) healthOrDamage (14 20) durability (182 189) id "Boots-3-5")))
	(kind "wynncraft/recipe/get/Boots-5-7" code 200 timestamp 1610855726683 version "2.1.0" 
	 data ((level (5 7) type "BOOTS" skill "TAILORING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Wheat String" amount 2)) healthOrDamage (21 27) durability (189 196) id "Boots-5-7")))
	(kind "wynncraft/recipe/get/Bow-1-3" code 200 timestamp 1610855726910 version "2.1.0" 
	 data ((level (1 3) type "BOW" skill "WOODWORKING" materials ((item "Refined Wheat String" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (7 10) durability (175 182) id "Bow-1-3")))
	(kind "wynncraft/recipe/get/Boots-7-9" code 200 timestamp 1610855727214 version "2.1.0" 
	 data ((level (7 9) type "BOOTS" skill "TAILORING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Wheat String" amount 2)) healthOrDamage (26 39) durability (196 203) id "Boots-7-9")))
	(kind "wynncraft/recipe/get/Bow-3-5" code 200 timestamp 1610855727409 version "2.1.0" 
	 data ((level (3 5) type "BOW" skill "WOODWORKING" materials ((item "Refined Wheat String" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (12 15) durability (182 189) id "Bow-3-5")))
	(kind "wynncraft/recipe/get/Bow-5-7" code 200 timestamp 1610855727609 version "2.1.0" 
	 data ((level (5 7) type "BOW" skill "WOODWORKING" materials ((item "Refined Wheat String" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (15 17) durability (189 196) id "Bow-5-7")))
	(kind "wynncraft/recipe/get/Bow-7-9" code 200 timestamp 1610855727888 version "2.1.0" 
	 data ((level (7 9) type "BOW" skill "WOODWORKING" materials ((item "Refined Wheat String" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (17 22) durability (196 203) id "Bow-7-9")))
	(kind "wynncraft/recipe/get/Bracelet-3-5" code 200 timestamp 1610855728205 version "2.1.0" 
	 data ((level (3 5) type "BRACELET" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 2)) healthOrDamage (0 0) durability (182 189) id "Bracelet-3-5")))
	(kind "wynncraft/recipe/get/Bracelet-5-7" code 200 timestamp 1610855728489 version "2.1.0" 
	 data ((level (5 7) type "BRACELET" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 2)) healthOrDamage (0 0) durability (189 196) id "Bracelet-5-7")))
	(kind "wynncraft/recipe/get/Bracelet-1-3" code 200 timestamp 1610855728783 version "2.1.0" 
	 data ((level (1 3) type "BRACELET" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 2)) healthOrDamage (0 0) durability (175 182) id "Bracelet-1-3")))
	(kind "wynncraft/recipe/get/Chestplate-1-3" code 200 timestamp 1610855729026 version "2.1.0" 
	 data ((level (1 3) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Oak Paper" amount 1)) healthOrDamage (11 14) durability (175 182) id "Chestplate-1-3")))
	(kind "wynncraft/recipe/get/Chestplate-3-5" code 200 timestamp 1610855729221 version "2.1.0" 
	 data ((level (3 5) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Oak Paper" amount 1)) healthOrDamage (14 21) durability (182 189) id "Chestplate-3-5")))
	(kind "wynncraft/recipe/get/Chestplate-7-9" code 200 timestamp 1610855729513 version "2.1.0" 
	 data ((level (7 9) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Oak Paper" amount 1)) healthOrDamage (26 39) durability (196 203) id "Chestplate-7-9")))
	(kind "wynncraft/recipe/get/Chestplate-5-7" code 200 timestamp 1610855729715 version "2.1.0" 
	 data ((level (5 7) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Oak Paper" amount 1)) healthOrDamage (21 27) durability (189 196) id "Chestplate-5-7")))
	(kind "wynncraft/recipe/get/Dagger-1-3" code 200 timestamp 1610855730010 version "2.1.0" 
	 data ((level (1 3) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (6 8) durability (175 182) id "Dagger-1-3")))
	(kind "wynncraft/recipe/get/Dagger-3-5" code 200 timestamp 1610855730220 version "2.1.0" 
	 data ((level (3 5) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (8 12) durability (182 189) id "Dagger-3-5")))
	(kind "wynncraft/recipe/get/Dagger-5-7" code 200 timestamp 1610855730442 version "2.1.0" 
	 data ((level (5 7) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (12 15) durability (189 196) id "Dagger-5-7")))
	(kind "wynncraft/recipe/get/Bracelet-7-9" code 200 timestamp 1610855730760 version "2.1.0" 
	 data ((level (7 9) type "BRACELET" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 2)) healthOrDamage (0 0) durability (196 203) id "Bracelet-7-9")))
	(kind "wynncraft/recipe/get/Dagger-7-9" code 200 timestamp 1610855730962 version "2.1.0" 
	 data ((level (7 9) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (15 17) durability (196 203) id "Dagger-7-9")))
	(kind "wynncraft/recipe/get/Food-3-5" code 200 timestamp 1610855731165 version "2.1.0" 
	 data ((level (3 5) type "FOOD" skill "COOKING" materials ((item "Refined Gudgeon Meat" amount 2) (item "Refined Wheat Grains" amount 1)) healthOrDamage (33 42) duration (1116 1152) basicDuration (60 60) id "Food-3-5")))
	(kind "wynncraft/recipe/get/Food-1-3" code 200 timestamp 1610855731413 version "2.1.0" 
	 data ((level (1 3) type "FOOD" skill "COOKING" materials ((item "Refined Gudgeon Meat" amount 2) (item "Refined Wheat Grains" amount 1)) healthOrDamage (24 32) duration (1080 1116) basicDuration (60 60) id "Food-1-3")))
	(kind "wynncraft/recipe/get/Food-5-7" code 200 timestamp 1610855731636 version "2.1.0" 
	 data ((level (5 7) type "FOOD" skill "COOKING" materials ((item "Refined Gudgeon Meat" amount 2) (item "Refined Wheat Grains" amount 1)) healthOrDamage (43 54) duration (1152 1188) basicDuration (60 60) id "Food-5-7")))
	(kind "wynncraft/recipe/get/Food-7-9" code 200 timestamp 1610855731931 version "2.1.0" 
	 data ((level (7 9) type "FOOD" skill "COOKING" materials ((item "Refined Gudgeon Meat" amount 2) (item "Refined Wheat Grains" amount 1)) healthOrDamage (54 59) duration (1188 1224) basicDuration (60 60) id "Food-7-9")))
	(kind "wynncraft/recipe/get/Helmet-1-3" code 200 timestamp 1610855732126 version "2.1.0" 
	 data ((level (1 3) type "HELMET" skill "ARMOURING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Oak Paper" amount 2)) healthOrDamage (11 14) durability (175 182) id "Helmet-1-3")))
	(kind "wynncraft/recipe/get/Helmet-3-5" code 200 timestamp 1610855732318 version "2.1.0" 
	 data ((level (3 5) type "HELMET" skill "ARMOURING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Oak Paper" amount 2)) healthOrDamage (14 21) durability (182 189) id "Helmet-3-5")))
	(kind "wynncraft/recipe/get/Helmet-7-9" code 200 timestamp 1610855732507 version "2.1.0" 
	 data ((level (7 9) type "HELMET" skill "ARMOURING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Oak Paper" amount 2)) healthOrDamage (26 38) durability (196 203) id "Helmet-7-9")))
	(kind "wynncraft/recipe/get/Helmet-5-7" code 200 timestamp 1610855732770 version "2.1.0" 
	 data ((level (5 7) type "HELMET" skill "ARMOURING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Oak Paper" amount 2)) healthOrDamage (21 27) durability (189 196) id "Helmet-5-7")))
	(kind "wynncraft/recipe/get/Necklace-3-5" code 200 timestamp 1610855732976 version "2.1.0" 
	 data ((level (3 5) type "NECKLACE" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 3)) healthOrDamage (0 0) durability (182 189) id "Necklace-3-5")))
	(kind "wynncraft/recipe/get/Necklace-1-3" code 200 timestamp 1610855733217 version "2.1.0" 
	 data ((level (1 3) type "NECKLACE" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 3)) healthOrDamage (0 0) durability (175 182) id "Necklace-1-3")))
	(kind "wynncraft/recipe/get/Necklace-7-9" code 200 timestamp 1610855733453 version "2.1.0" 
	 data ((level (7 9) type "NECKLACE" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 3)) healthOrDamage (0 0) durability (196 203) id "Necklace-7-9")))
	(kind "wynncraft/recipe/get/Necklace-5-7" code 200 timestamp 1610855733628 version "2.1.0" 
	 data ((level (5 7) type "NECKLACE" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 3)) healthOrDamage (0 0) durability (189 196) id "Necklace-5-7")))
	(kind "wynncraft/recipe/get/Leggings-1-3" code 200 timestamp 1610855733831 version "2.1.0" 
	 data ((level (1 3) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Wheat String" amount 1)) healthOrDamage (11 14) durability (175 182) id "Leggings-1-3")))
	(kind "wynncraft/recipe/get/Leggings-3-5" code 200 timestamp 1610855734076 version "2.1.0" 
	 data ((level (3 5) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Wheat String" amount 1)) healthOrDamage (14 21) durability (182 189) id "Leggings-3-5")))
	(kind "wynncraft/recipe/get/Leggings-5-7" code 200 timestamp 1610855734339 version "2.1.0" 
	 data ((level (5 7) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Wheat String" amount 1)) healthOrDamage (21 27) durability (189 196) id "Leggings-5-7")))
	(kind "wynncraft/recipe/get/Leggings-7-9" code 200 timestamp 1610855734550 version "2.1.0" 
	 data ((level (7 9) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Copper Ingot" amount 2) (item "Refined Wheat String" amount 1)) healthOrDamage (27 38) durability (196 203) id "Leggings-7-9")))
	(kind "wynncraft/recipe/get/Potion-1-3" code 200 timestamp 1610855734762 version "2.1.0" 
	 data ((level (1 3) type "POTION" skill "ALCHEMISM" materials ((item "Refined Gudgeon Oil" amount 1) (item "Refined Wheat Grains" amount 2)) healthOrDamage (24 33) duration (360 372) basicDuration (3 3) id "Potion-1-3")))
	(kind "wynncraft/recipe/get/Potion-3-5" code 200 timestamp 1610855735031 version "2.1.0" 
	 data ((level (3 5) type "POTION" skill "ALCHEMISM" materials ((item "Refined Gudgeon Oil" amount 1) (item "Refined Wheat Grains" amount 2)) healthOrDamage (33 42) duration (372 384) basicDuration (3 3) id "Potion-3-5")))
	(kind "wynncraft/recipe/get/Potion-5-7" code 200 timestamp 1610855735267 version "2.1.0" 
	 data ((level (5 7) type "POTION" skill "ALCHEMISM" materials ((item "Refined Gudgeon Oil" amount 1) (item "Refined Wheat Grains" amount 2)) healthOrDamage (43 54) duration (384 396) basicDuration (3 3) id "Potion-5-7")))
	(kind "wynncraft/recipe/get/Relik-1-3" code 200 timestamp 1610855735550 version "2.1.0" 
	 data ((level (1 3) type "RELIK" skill "WOODWORKING" materials ((item "Refined Gudgeon Oil" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (7 10) durability (175 182) id "Relik-1-3")))
	(kind "wynncraft/recipe/get/Potion-7-9" code 200 timestamp 1610855735848 version "2.1.0" 
	 data ((level (7 9) type "POTION" skill "ALCHEMISM" materials ((item "Refined Gudgeon Oil" amount 1) (item "Refined Wheat Grains" amount 2)) healthOrDamage (54 59) duration (396 408) basicDuration (3 3) id "Potion-7-9")))
	(kind "wynncraft/recipe/get/Relik-5-7" code 200 timestamp 1610855736132 version "2.1.0" 
	 data ((level (5 7) type "RELIK" skill "WOODWORKING" materials ((item "Refined Gudgeon Oil" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (15 17) durability (189 196) id "Relik-5-7")))
	(kind "wynncraft/recipe/get/Relik-3-5" code 200 timestamp 1610855736401 version "2.1.0" 
	 data ((level (3 5) type "RELIK" skill "WOODWORKING" materials ((item "Refined Gudgeon Oil" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (12 15) durability (182 189) id "Relik-3-5")))
	(kind "wynncraft/recipe/get/Relik-7-9" code 200 timestamp 1610855736591 version "2.1.0" 
	 data ((level (7 9) type "RELIK" skill "WOODWORKING" materials ((item "Refined Gudgeon Oil" amount 2) (item "Refined Oak Wood" amount 1)) healthOrDamage (17 22) durability (196 203) id "Relik-7-9")))
	(kind "wynncraft/recipe/get/Boots-1-3" code 200 timestamp 1610855736822 version "2.1.0" 
	 data ((level (1 3) type "BOOTS" skill "TAILORING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Wheat String" amount 2)) healthOrDamage (11 14) durability (175 182) id "Boots-1-3")))
	(kind "wynncraft/recipe/get/Ring-3-5" code 200 timestamp 1610855737052 version "2.1.0" 
	 data ((level (3 5) type "RING" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 1)) healthOrDamage (0 0) durability (182 189) id "Ring-3-5")))
	(kind "wynncraft/recipe/get/Ring-5-7" code 200 timestamp 1610855737240 version "2.1.0" 
	 data ((level (5 7) type "RING" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 1)) healthOrDamage (0 0) durability (189 196) id "Ring-5-7")))
	(kind "wynncraft/recipe/get/Ring-7-9" code 200 timestamp 1610855737438 version "2.1.0" 
	 data ((level (7 9) type "RING" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 1)) healthOrDamage (0 0) durability (196 203) id "Ring-7-9")))
	(kind "wynncraft/recipe/get/Scroll-1-3" code 200 timestamp 1610855737735 version "2.1.0" 
	 data ((level (1 3) type "SCROLL" skill "SCRIBING" materials ((item "Refined Gudgeon Oil" amount 1) (item "Refined Oak Paper" amount 1)) healthOrDamage (21 26) duration (360 372) basicDuration (3 3) id "Scroll-1-3")))
	(kind "wynncraft/recipe/get/Scroll-3-5" code 200 timestamp 1610855738017 version "2.1.0" 
	 data ((level (3 5) type "SCROLL" skill "SCRIBING" materials ((item "Refined Gudgeon Oil" amount 1) (item "Refined Oak Paper" amount 1)) healthOrDamage (27 34) duration (372 384) basicDuration (3 3) id "Scroll-3-5")))
	(kind "wynncraft/recipe/get/Scroll-5-7" code 200 timestamp 1610855738220 version "2.1.0" 
	 data ((level (5 7) type "SCROLL" skill "SCRIBING" materials ((item "Refined Gudgeon Oil" amount 1) (item "Refined Oak Paper" amount 1)) healthOrDamage (35 41) duration (384 396) basicDuration (3 3) id "Scroll-5-7")))
	(kind "wynncraft/recipe/get/Scroll-7-9" code 200 timestamp 1610855738515 version "2.1.0" 
	 data ((level (7 9) type "SCROLL" skill "SCRIBING" materials ((item "Refined Gudgeon Oil" amount 1) (item "Refined Oak Paper" amount 1)) healthOrDamage (41 45) duration (396 408) basicDuration (3 3) id "Scroll-7-9")))
	(kind "wynncraft/recipe/get/Spear-1-3" code 200 timestamp 1610855738731 version "2.1.0" 
	 data ((level (1 3) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Oak Wood" amount 2)) healthOrDamage (4 6) durability (175 182) id "Spear-1-3")))
	(kind "wynncraft/recipe/get/Spear-3-5" code 200 timestamp 1610855738991 version "2.1.0" 
	 data ((level (3 5) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Oak Wood" amount 2)) healthOrDamage (7 9) durability (182 189) id "Spear-3-5")))
	(kind "wynncraft/recipe/get/Spear-5-7" code 200 timestamp 1610855739254 version "2.1.0" 
	 data ((level (5 7) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Oak Wood" amount 2)) healthOrDamage (10 13) durability (189 196) id "Spear-5-7")))
	(kind "wynncraft/recipe/get/Spear-7-9" code 200 timestamp 1610855739472 version "2.1.0" 
	 data ((level (7 9) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Copper Ingot" amount 1) (item "Refined Oak Wood" amount 2)) healthOrDamage (13 15) durability (196 203) id "Spear-7-9")))
	(kind "wynncraft/recipe/get/Wand-1-3" code 200 timestamp 1610855739765 version "2.1.0" 
	 data ((level (1 3) type "WAND" skill "WOODWORKING" materials ((item "Refined Wheat String" amount 1) (item "Refined Oak Wood" amount 2)) healthOrDamage (3 5) durability (175 182) id "Wand-1-3")))
	(kind "wynncraft/recipe/get/Wand-3-5" code 200 timestamp 1610855740010 version "2.1.0" 
	 data ((level (3 5) type "WAND" skill "WOODWORKING" materials ((item "Refined Wheat String" amount 1) (item "Refined Oak Wood" amount 2)) healthOrDamage (5 7) durability (182 189) id "Wand-3-5")))
	(kind "wynncraft/recipe/get/Wand-5-7" code 200 timestamp 1610855740328 version "2.1.0" 
	 data ((level (5 7) type "WAND" skill "WOODWORKING" materials ((item "Refined Wheat String" amount 1) (item "Refined Oak Wood" amount 2)) healthOrDamage (7 10) durability (189 196) id "Wand-5-7")))
	(kind "wynncraft/recipe/get/Wand-7-9" code 200 timestamp 1610855741678 version "2.1.0" 
	 data ((level (7 9) type "WAND" skill "WOODWORKING" materials ((item "Refined Wheat String" amount 1) (item "Refined Oak Wood" amount 2)) healthOrDamage (10 12) durability (196 203) id "Wand-7-9")))
	(kind "wynncraft/recipe/get/Boots-10-13" code 200 timestamp 1610855742003 version "2.1.0" 
	 data ((level (10 13) type "BOOTS" skill "TAILORING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Barley String" amount 4)) healthOrDamage (36 47) durability (207 217) id "Boots-10-13")))
	(kind "wynncraft/recipe/get/Boots-13-15" code 200 timestamp 1610855742303 version "2.1.0" 
	 data ((level (13 15) type "BOOTS" skill "TAILORING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Barley String" amount 4)) healthOrDamage (47 67) durability (217 224) id "Boots-13-15")))
	(kind "wynncraft/recipe/get/Boots-15-17" code 200 timestamp 1610855742510 version "2.1.0" 
	 data ((level (15 17) type "BOOTS" skill "TAILORING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Barley String" amount 4)) healthOrDamage (67 79) durability (224 231) id "Boots-15-17")))
	(kind "wynncraft/recipe/get/Boots-17-19" code 200 timestamp 1610855742798 version "2.1.0" 
	 data ((level (17 19) type "BOOTS" skill "TAILORING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Barley String" amount 4)) healthOrDamage (79 92) durability (231 238) id "Boots-17-19")))
	(kind "wynncraft/recipe/get/Bow-10-13" code 200 timestamp 1610855742992 version "2.1.0" 
	 data ((level (10 13) type "BOW" skill "WOODWORKING" materials ((item "Refined Barley String" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (23 25) durability (207 217) id "Bow-10-13")))
	(kind "wynncraft/recipe/get/Bow-13-15" code 200 timestamp 1610855743212 version "2.1.0" 
	 data ((level (13 15) type "BOW" skill "WOODWORKING" materials ((item "Refined Barley String" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (25 30) durability (217 224) id "Bow-13-15")))
	(kind "wynncraft/recipe/get/Bracelet-10-13" code 200 timestamp 1610855743445 version "2.1.0" 
	 data ((level (10 13) type "BRACELET" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 4)) healthOrDamage (0 0) durability (207 217) id "Bracelet-10-13")))
	(kind "wynncraft/recipe/get/Bracelet-13-15" code 200 timestamp 1610855743644 version "2.1.0" 
	 data ((level (13 15) type "BRACELET" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 4)) healthOrDamage (0 0) durability (217 224) id "Bracelet-13-15")))
	(kind "wynncraft/recipe/get/Bracelet-15-17" code 200 timestamp 1610855743960 version "2.1.0" 
	 data ((level (15 17) type "BRACELET" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 4)) healthOrDamage (0 0) durability (224 231) id "Bracelet-15-17")))
	(kind "wynncraft/recipe/get/Bow-17-19" code 200 timestamp 1610855744223 version "2.1.0" 
	 data ((level (17 19) type "BOW" skill "WOODWORKING" materials ((item "Refined Barley String" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (34 37) durability (231 238) id "Bow-17-19")))
	(kind "wynncraft/recipe/get/Bracelet-17-19" code 200 timestamp 1610855744553 version "2.1.0" 
	 data ((level (17 19) type "BRACELET" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 4)) healthOrDamage (0 0) durability (231 238) id "Bracelet-17-19")))
	(kind "wynncraft/recipe/get/Chestplate-10-13" code 200 timestamp 1610855744826 version "2.1.0" 
	 data ((level (10 13) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Birch Paper" amount 2)) healthOrDamage (36 47) durability (207 217) id "Chestplate-10-13")))
	(kind "wynncraft/recipe/get/Chestplate-13-15" code 200 timestamp 1610855745096 version "2.1.0" 
	 data ((level (13 15) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Birch Paper" amount 2)) healthOrDamage (47 67) durability (217 224) id "Chestplate-13-15")))
	(kind "wynncraft/recipe/get/Chestplate-15-17" code 200 timestamp 1610855745342 version "2.1.0" 
	 data ((level (15 17) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Birch Paper" amount 2)) healthOrDamage (67 79) durability (224 231) id "Chestplate-15-17")))
	(kind "wynncraft/recipe/get/Bow-15-17" code 200 timestamp 1610855745628 version "2.1.0" 
	 data ((level (15 17) type "BOW" skill "WOODWORKING" materials ((item "Refined Barley String" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (30 34) durability (224 231) id "Bow-15-17")))
	(kind "wynncraft/recipe/get/Dagger-10-13" code 200 timestamp 1610855745891 version "2.1.0" 
	 data ((level (10 13) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (18 22) durability (207 217) id "Dagger-10-13")))
	(kind "wynncraft/recipe/get/Dagger-13-15" code 200 timestamp 1610855746079 version "2.1.0" 
	 data ((level (13 15) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (22 24) durability (217 224) id "Dagger-13-15")))
	(kind "wynncraft/recipe/get/Chestplate-17-19" code 200 timestamp 1610855746367 version "2.1.0" 
	 data ((level (17 19) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Birch Paper" amount 2)) healthOrDamage (79 92) durability (231 238) id "Chestplate-17-19")))
	(kind "wynncraft/recipe/get/Dagger-17-19" code 200 timestamp 1610855746573 version "2.1.0" 
	 data ((level (17 19) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (27 30) durability (231 238) id "Dagger-17-19")))
	(kind "wynncraft/recipe/get/Food-13-15" code 200 timestamp 1610855746830 version "2.1.0" 
	 data ((level (13 15) type "FOOD" skill "COOKING" materials ((item "Refined Trout Meat" amount 4) (item "Refined Barley Grains" amount 2)) healthOrDamage (79 99) duration (1296 1332) basicDuration (60 60) id "Food-13-15")))
	(kind "wynncraft/recipe/get/Food-15-17" code 200 timestamp 1610855747115 version "2.1.0" 
	 data ((level (15 17) type "FOOD" skill "COOKING" materials ((item "Refined Trout Meat" amount 4) (item "Refined Barley Grains" amount 2)) healthOrDamage (99 113) duration (1332 1368) basicDuration (60 60) id "Food-15-17")))
	(kind "wynncraft/recipe/get/Food-10-13" code 200 timestamp 1610855747296 version "2.1.0" 
	 data ((level (10 13) type "FOOD" skill "COOKING" materials ((item "Refined Trout Meat" amount 4) (item "Refined Barley Grains" amount 2)) healthOrDamage (60 79) duration (1242 1296) basicDuration (60 60) id "Food-10-13")))
	(kind "wynncraft/recipe/get/Food-17-19" code 200 timestamp 1610855747483 version "2.1.0" 
	 data ((level (17 19) type "FOOD" skill "COOKING" materials ((item "Refined Trout Meat" amount 4) (item "Refined Barley Grains" amount 2)) healthOrDamage (113 128) duration (1368 1404) basicDuration (60 60) id "Food-17-19")))
	(kind "wynncraft/recipe/get/Helmet-10-13" code 200 timestamp 1610855747767 version "2.1.0" 
	 data ((level (10 13) type "HELMET" skill "ARMOURING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Birch Paper" amount 4)) healthOrDamage (35 47) durability (207 217) id "Helmet-10-13")))
	(kind "wynncraft/recipe/get/Helmet-13-15" code 200 timestamp 1610855747955 version "2.1.0" 
	 data ((level (13 15) type "HELMET" skill "ARMOURING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Birch Paper" amount 4)) healthOrDamage (47 67) durability (217 224) id "Helmet-13-15")))
	(kind "wynncraft/recipe/get/Helmet-15-17" code 200 timestamp 1610855748139 version "2.1.0" 
	 data ((level (15 17) type "HELMET" skill "ARMOURING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Birch Paper" amount 4)) healthOrDamage (67 79) durability (224 231) id "Helmet-15-17")))
	(kind "wynncraft/recipe/get/Helmet-17-19" code 200 timestamp 1610855748335 version "2.1.0" 
	 data ((level (17 19) type "HELMET" skill "ARMOURING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Birch Paper" amount 4)) healthOrDamage (79 92) durability (231 238) id "Helmet-17-19")))
	(kind "wynncraft/recipe/get/Necklace-10-13" code 200 timestamp 1610855748530 version "2.1.0" 
	 data ((level (10 13) type "NECKLACE" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 6)) healthOrDamage (0 0) durability (207 217) id "Necklace-10-13")))
	(kind "wynncraft/recipe/get/Dagger-15-17" code 200 timestamp 1610855748726 version "2.1.0" 
	 data ((level (15 17) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (24 27) durability (224 231) id "Dagger-15-17")))
	(kind "wynncraft/recipe/get/Necklace-15-17" code 200 timestamp 1610855748932 version "2.1.0" 
	 data ((level (15 17) type "NECKLACE" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 6)) healthOrDamage (0 0) durability (224 231) id "Necklace-15-17")))
	(kind "wynncraft/recipe/get/Necklace-17-19" code 200 timestamp 1610855749139 version "2.1.0" 
	 data ((level (17 19) type "NECKLACE" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 6)) healthOrDamage (0 0) durability (231 238) id "Necklace-17-19")))
	(kind "wynncraft/recipe/get/Leggings-10-13" code 200 timestamp 1610855749399 version "2.1.0" 
	 data ((level (10 13) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Barley String" amount 2)) healthOrDamage (35 47) durability (207 200) id "Leggings-10-13")))
	(kind "wynncraft/recipe/get/Leggings-13-15" code 200 timestamp 1610855749605 version "2.1.0" 
	 data ((level (13 15) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Barley String" amount 2)) healthOrDamage (47 67) durability (217 224) id "Leggings-13-15")))
	(kind "wynncraft/recipe/get/Leggings-15-17" code 200 timestamp 1610855749896 version "2.1.0" 
	 data ((level (15 17) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Barley String" amount 2)) healthOrDamage (67 79) durability (224 231) id "Leggings-15-17")))
	(kind "wynncraft/recipe/get/Leggings-17-19" code 200 timestamp 1610855750174 version "2.1.0" 
	 data ((level (17 19) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Granite Ingot" amount 4) (item "Refined Barley String" amount 2)) healthOrDamage (79 92) durability (231 238) id "Leggings-17-19")))
	(kind "wynncraft/recipe/get/Potion-10-13" code 200 timestamp 1610855750431 version "2.1.0" 
	 data ((level (10 13) type "POTION" skill "ALCHEMISM" materials ((item "Refined Trout Oil" amount 2) (item "Refined Barley Grains" amount 4)) healthOrDamage (60 79) duration (414 432) basicDuration (3 3) id "Potion-10-13")))
	(kind "wynncraft/recipe/get/Potion-13-15" code 200 timestamp 1610855750619 version "2.1.0" 
	 data ((level (13 15) type "POTION" skill "ALCHEMISM" materials ((item "Refined Trout Oil" amount 2) (item "Refined Barley Grains" amount 4)) healthOrDamage (79 99) duration (432 444) basicDuration (3 3) id "Potion-13-15")))
	(kind "wynncraft/recipe/get/Potion-15-17" code 200 timestamp 1610855750905 version "2.1.0" 
	 data ((level (15 17) type "POTION" skill "ALCHEMISM" materials ((item "Refined Trout Oil" amount 2) (item "Refined Barley Grains" amount 4)) healthOrDamage (99 113) duration (444 456) basicDuration (3 3) id "Potion-15-17")))
	(kind "wynncraft/recipe/get/Relik-10-13" code 200 timestamp 1610855751096 version "2.1.0" 
	 data ((level (10 13) type "RELIK" skill "WOODWORKING" materials ((item "Refined Trout Oil" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (23 25) durability (207 217) id "Relik-10-13")))
	(kind "wynncraft/recipe/get/Relik-13-15" code 200 timestamp 1610855751405 version "2.1.0" 
	 data ((level (13 15) type "RELIK" skill "WOODWORKING" materials ((item "Refined Trout Oil" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (25 30) durability (217 224) id "Relik-13-15")))
	(kind "wynncraft/recipe/get/Relik-15-17" code 200 timestamp 1610855751603 version "2.1.0" 
	 data ((level (15 17) type "RELIK" skill "WOODWORKING" materials ((item "Refined Trout Oil" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (30 34) durability (224 231) id "Relik-15-17")))
	(kind "wynncraft/recipe/get/Relik-17-19" code 200 timestamp 1610855751871 version "2.1.0" 
	 data ((level (17 19) type "RELIK" skill "WOODWORKING" materials ((item "Refined Trout Oil" amount 4) (item "Refined Birch Wood" amount 2)) healthOrDamage (34 37) durability (231 238) id "Relik-17-19")))
	(kind "wynncraft/recipe/get/Ring-10-13" code 200 timestamp 1610855752090 version "2.1.0" 
	 data ((level (10 13) type "RING" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 2)) healthOrDamage (0 0) durability (207 217) id "Ring-10-13")))
	(kind "wynncraft/recipe/get/Ring-13-15" code 200 timestamp 1610855752330 version "2.1.0" 
	 data ((level (13 15) type "RING" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 2)) healthOrDamage (0 0) durability (217 224) id "Ring-13-15")))
	(kind "wynncraft/recipe/get/Ring-15-17" code 200 timestamp 1610855752512 version "2.1.0" 
	 data ((level (15 17) type "RING" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 2)) healthOrDamage (0 0) durability (224 231) id "Ring-15-17")))
	(kind "wynncraft/recipe/get/Ring-17-19" code 200 timestamp 1610855752691 version "2.1.0" 
	 data ((level (17 19) type "RING" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 2)) healthOrDamage (0 0) durability (231 238) id "Ring-17-19")))
	(kind "wynncraft/recipe/get/Scroll-10-13" code 200 timestamp 1610855752955 version "2.1.0" 
	 data ((level (10 13) type "SCROLL" skill "SCRIBING" materials ((item "Refined Trout Oil" amount 2) (item "Refined Birch Paper" amount 2)) healthOrDamage (46 59) duration (414 432) basicDuration (3 3) id "Scroll-10-13")))
	(kind "wynncraft/recipe/get/Potion-17-19" code 200 timestamp 1610855753200 version "2.1.0" 
	 data ((level (17 19) type "POTION" skill "ALCHEMISM" materials ((item "Refined Trout Oil" amount 2) (item "Refined Barley Grains" amount 4)) healthOrDamage (113 128) duration (456 468) basicDuration (3 3) id "Potion-17-19")))
	(kind "wynncraft/recipe/get/Scroll-13-15" code 200 timestamp 1610855753464 version "2.1.0" 
	 data ((level (13 15) type "SCROLL" skill "SCRIBING" materials ((item "Refined Trout Oil" amount 2) (item "Refined Birch Paper" amount 2)) healthOrDamage (60 73) duration (432 444) basicDuration (3 3) id "Scroll-13-15")))
	(kind "wynncraft/recipe/get/Scroll-15-17" code 200 timestamp 1610855753641 version "2.1.0" 
	 data ((level (15 17) type "SCROLL" skill "SCRIBING" materials ((item "Refined Trout Oil" amount 2) (item "Refined Birch Paper" amount 2)) healthOrDamage (73 84) duration (444 456) basicDuration (3 3) id "Scroll-15-17")))
	(kind "wynncraft/recipe/get/Spear-10-13" code 200 timestamp 1610855753855 version "2.1.0" 
	 data ((level (10 13) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Birch Wood" amount 4)) healthOrDamage (14 17) durability (207 217) id "Spear-10-13")))
	(kind "wynncraft/recipe/get/Scroll-17-19" code 200 timestamp 1610855754063 version "2.1.0" 
	 data ((level (17 19) type "SCROLL" skill "SCRIBING" materials ((item "Refined Trout Oil" amount 2) (item "Refined Birch Paper" amount 2)) healthOrDamage (84 92) duration (456 468) basicDuration (3 3) id "Scroll-17-19")))
	(kind "wynncraft/recipe/get/Spear-13-15" code 200 timestamp 1610855754254 version "2.1.0" 
	 data ((level (13 15) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Birch Wood" amount 4)) healthOrDamage (17 20) durability (217 224) id "Spear-13-15")))
	(kind "wynncraft/recipe/get/Spear-15-17" code 200 timestamp 1610855754515 version "2.1.0" 
	 data ((level (15 17) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Birch Wood" amount 4)) healthOrDamage (20 24) durability (224 231) id "Spear-15-17")))
	(kind "wynncraft/recipe/get/Spear-17-19" code 200 timestamp 1610855754695 version "2.1.0" 
	 data ((level (17 19) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Granite Ingot" amount 2) (item "Refined Birch Wood" amount 4)) healthOrDamage (24 26) durability (231 238) id "Spear-17-19")))
	(kind "wynncraft/recipe/get/Wand-10-13" code 200 timestamp 1610855754941 version "2.1.0" 
	 data ((level (10 13) type "WAND" skill "WOODWORKING" materials ((item "Refined Barley String" amount 2) (item "Refined Birch Wood" amount 4)) healthOrDamage (11 13) durability (207 217) id "Wand-10-13")))
	(kind "wynncraft/recipe/get/Wand-13-15" code 200 timestamp 1610855755134 version "2.1.0" 
	 data ((level (13 15) type "WAND" skill "WOODWORKING" materials ((item "Refined Barley String" amount 2) (item "Refined Birch Wood" amount 4)) healthOrDamage (13 15) durability (217 224) id "Wand-13-15")))
	(kind "wynncraft/recipe/get/Wand-15-17" code 200 timestamp 1610855755392 version "2.1.0" 
	 data ((level (15 17) type "WAND" skill "WOODWORKING" materials ((item "Refined Barley String" amount 2) (item "Refined Birch Wood" amount 4)) healthOrDamage (15 17) durability (224 231) id "Wand-15-17")))
	(kind "wynncraft/recipe/get/Wand-17-19" code 200 timestamp 1610855755571 version "2.1.0" 
	 data ((level (17 19) type "WAND" skill "WOODWORKING" materials ((item "Refined Barley String" amount 2) (item "Refined Birch Wood" amount 4)) healthOrDamage (16 19) durability (231 238) id "Wand-17-19")))
	(kind "wynncraft/recipe/get/Boots-20-23" code 200 timestamp 1610855755762 version "2.1.0" 
	 data ((level (20 23) type "BOOTS" skill "TAILORING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Oat String" amount 4)) healthOrDamage (88 104) durability (238 252) id "Boots-20-23")))
	(kind "wynncraft/recipe/get/Boots-23-25" code 200 timestamp 1610855755950 version "2.1.0" 
	 data ((level (23 25) type "BOOTS" skill "TAILORING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Oat String" amount 4)) healthOrDamage (99 121) durability (252 259) id "Boots-23-25")))
	(kind "wynncraft/recipe/get/Boots-25-27" code 200 timestamp 1610855756150 version "2.1.0" 
	 data ((level (25 27) type "BOOTS" skill "TAILORING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Oat String" amount 4)) healthOrDamage (121 148) durability (259 266) id "Boots-25-27")))
	(kind "wynncraft/recipe/get/Bow-23-25" code 200 timestamp 1610855756352 version "2.1.0" 
	 data ((level (23 25) type "BOW" skill "WOODWORKING" materials ((item "Refined Oat String" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (39 42) durability (252 259) id "Bow-23-25")))
	(kind "wynncraft/recipe/get/Bow-20-23" code 200 timestamp 1610855756532 version "2.1.0" 
	 data ((level (20 23) type "BOW" skill "WOODWORKING" materials ((item "Refined Oat String" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (36 39) durability (242 252) id "Bow-20-23")))
	(kind "wynncraft/recipe/get/Boots-27-29" code 200 timestamp 1610855756718 version "2.1.0" 
	 data ((level (27 29) type "BOOTS" skill "TAILORING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Oat String" amount 4)) healthOrDamage (148 176) durability (266 273) id "Boots-27-29")))
	(kind "wynncraft/recipe/get/Bow-25-27" code 200 timestamp 1610855756919 version "2.1.0" 
	 data ((level (25 27) type "BOW" skill "WOODWORKING" materials ((item "Refined Oat String" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (41 43) durability (259 266) id "Bow-25-27")))
	(kind "wynncraft/recipe/get/Bow-27-29" code 200 timestamp 1610855757180 version "2.1.0" 
	 data ((level (27 29) type "BOW" skill "WOODWORKING" materials ((item "Refined Oat String" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (43 48) durability (266 273) id "Bow-27-29")))
	(kind "wynncraft/recipe/get/Bracelet-20-23" code 200 timestamp 1610855757398 version "2.1.0" 
	 data ((level (20 23) type "BRACELET" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 4)) healthOrDamage (0 0) durability (242 252) id "Bracelet-20-23")))
	(kind "wynncraft/recipe/get/Bracelet-23-25" code 200 timestamp 1610855757581 version "2.1.0" 
	 data ((level (23 25) type "BRACELET" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 4)) healthOrDamage (0 0) durability (252 259) id "Bracelet-23-25")))
	(kind "wynncraft/recipe/get/Bracelet-25-27" code 200 timestamp 1610855757774 version "2.1.0" 
	 data ((level (25 27) type "BRACELET" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 4)) healthOrDamage (0 0) durability (259 266) id "Bracelet-25-27")))
	(kind "wynncraft/recipe/get/Bracelet-27-29" code 200 timestamp 1610855758041 version "2.1.0" 
	 data ((level (27 29) type "BRACELET" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 4)) healthOrDamage (0 0) durability (266 273) id "Bracelet-27-29")))
	(kind "wynncraft/recipe/get/Chestplate-23-25" code 200 timestamp 1610855758258 version "2.1.0" 
	 data ((level (23 25) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Willow Paper" amount 2)) healthOrDamage (99 121) durability (252 259) id "Chestplate-23-25")))
	(kind "wynncraft/recipe/get/Chestplate-25-27" code 200 timestamp 1610855758539 version "2.1.0" 
	 data ((level (25 27) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Willow Paper" amount 2)) healthOrDamage (121 148) durability (259 266) id "Chestplate-25-27")))
	(kind "wynncraft/recipe/get/Chestplate-20-23" code 200 timestamp 1610855758728 version "2.1.0" 
	 data ((level (20 23) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Willow Paper" amount 2)) healthOrDamage (87 104) durability (242 252) id "Chestplate-20-23")))
	(kind "wynncraft/recipe/get/Chestplate-27-29" code 200 timestamp 1610855759000 version "2.1.0" 
	 data ((level (27 29) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Willow Paper" amount 2)) healthOrDamage (148 176) durability (266 273) id "Chestplate-27-29")))
	(kind "wynncraft/recipe/get/Dagger-20-23" code 200 timestamp 1610855759263 version "2.1.0" 
	 data ((level (20 23) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (30 33) durability (242 252) id "Dagger-20-23")))
	(kind "wynncraft/recipe/get/Dagger-23-25" code 200 timestamp 1610855759522 version "2.1.0" 
	 data ((level (23 25) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (33 36) durability (252 259) id "Dagger-23-25")))
	(kind "wynncraft/recipe/get/Dagger-25-27" code 200 timestamp 1610855759714 version "2.1.0" 
	 data ((level (25 27) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (33 37) durability (259 266) id "Dagger-25-27")))
	(kind "wynncraft/recipe/get/Dagger-27-29" code 200 timestamp 1610855759906 version "2.1.0" 
	 data ((level (27 29) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (36 40) durability (266 273) id "Dagger-27-29")))
	(kind "wynncraft/recipe/get/Food-20-23" code 200 timestamp 1610855760102 version "2.1.0" 
	 data ((level (20 23) type "FOOD" skill "COOKING" materials ((item "Refined Salmon Meat" amount 4) (item "Refined Oat Grains" amount 2)) healthOrDamage (122 150) duration (1422 1476) basicDuration (60 60) id "Food-20-23")))
	(kind "wynncraft/recipe/get/Food-25-27" code 200 timestamp 1610855760276 version "2.1.0" 
	 data ((level (25 27) type "FOOD" skill "COOKING" materials ((item "Refined Salmon Meat" amount 4) (item "Refined Oat Grains" amount 2)) healthOrDamage (177 210) duration (1512 1548) basicDuration (60 60) id "Food-25-27")))
	(kind "wynncraft/recipe/get/Food-27-29" code 200 timestamp 1610855760475 version "2.1.0" 
	 data ((level (27 29) type "FOOD" skill "COOKING" materials ((item "Refined Salmon Meat" amount 4) (item "Refined Oat Grains" amount 2)) healthOrDamage (210 243) duration (1548 1584) basicDuration (60 60) id "Food-27-29")))
	(kind "wynncraft/recipe/get/Helmet-20-23" code 200 timestamp 1610855760665 version "2.1.0" 
	 data ((level (20 23) type "HELMET" skill "ARMOURING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Willow Paper" amount 4)) healthOrDamage (87 104) durability (238 252) id "Helmet-20-23")))
	(kind "wynncraft/recipe/get/Food-23-25" code 200 timestamp 1610855760886 version "2.1.0" 
	 data ((level (23 25) type "FOOD" skill "COOKING" materials ((item "Refined Salmon Meat" amount 4) (item "Refined Oat Grains" amount 2)) healthOrDamage (150 177) duration (1476 1512) basicDuration (60 60) id "Food-23-25")))
	(kind "wynncraft/recipe/get/Helmet-23-25" code 200 timestamp 1610855761111 version "2.1.0" 
	 data ((level (23 25) type "HELMET" skill "ARMOURING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Willow Paper" amount 4)) healthOrDamage (99 121) durability (252 259) id "Helmet-23-25")))
	(kind "wynncraft/recipe/get/Helmet-27-29" code 200 timestamp 1610855761344 version "2.1.0" 
	 data ((level (27 29) type "HELMET" skill "ARMOURING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Willow Paper" amount 4)) healthOrDamage (148 176) durability (266 273) id "Helmet-27-29")))
	(kind "wynncraft/recipe/get/Helmet-25-27" code 200 timestamp 1610855761583 version "2.1.0" 
	 data ((level (25 27) type "HELMET" skill "ARMOURING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Willow Paper" amount 4)) healthOrDamage (121 148) durability (259 266) id "Helmet-25-27")))
	(kind "wynncraft/recipe/get/Necklace-20-23" code 200 timestamp 1610855761795 version "2.1.0" 
	 data ((level (20 23) type "NECKLACE" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 6)) healthOrDamage (0 0) durability (242 252) id "Necklace-20-23")))
	(kind "wynncraft/recipe/get/Necklace-23-25" code 200 timestamp 1610855761985 version "2.1.0" 
	 data ((level (23 25) type "NECKLACE" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 6)) healthOrDamage (0 0) durability (252 259) id "Necklace-23-25")))
	(kind "wynncraft/recipe/get/Necklace-27-29" code 200 timestamp 1610855762270 version "2.1.0" 
	 data ((level (27 29) type "NECKLACE" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 6)) healthOrDamage (0 0) durability (266 273) id "Necklace-27-29")))
	(kind "wynncraft/recipe/get/Leggings-20-23" code 200 timestamp 1610855762545 version "2.1.0" 
	 data ((level (20 23) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Oat String" amount 2)) healthOrDamage (87 104) durability (242 252) id "Leggings-20-23")))
	(kind "wynncraft/recipe/get/Necklace-25-27" code 200 timestamp 1610855762752 version "2.1.0" 
	 data ((level (25 27) type "NECKLACE" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 6)) healthOrDamage (0 0) durability (259 266) id "Necklace-25-27")))
	(kind "wynncraft/recipe/get/Leggings-23-25" code 200 timestamp 1610855762954 version "2.1.0" 
	 data ((level (23 25) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Oat String" amount 2)) healthOrDamage (99 121) durability (252 259) id "Leggings-23-25")))
	(kind "wynncraft/recipe/get/Leggings-25-27" code 200 timestamp 1610855763248 version "2.1.0" 
	 data ((level (25 27) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Oat String" amount 2)) healthOrDamage (121 148) durability (259 266) id "Leggings-25-27")))
	(kind "wynncraft/recipe/get/Leggings-27-29" code 200 timestamp 1610855763456 version "2.1.0" 
	 data ((level (27 29) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Gold Ingot" amount 4) (item "Refined Oat String" amount 2)) healthOrDamage (148 176) durability (266 273) id "Leggings-27-29")))
	(kind "wynncraft/recipe/get/Potion-20-23" code 200 timestamp 1610855763703 version "2.1.0" 
	 data ((level (20 23) type "POTION" skill "ALCHEMISM" materials ((item "Refined Salmon Oil" amount 2) (item "Refined Oat Grains" amount 4)) healthOrDamage (122 150) duration (474 492) basicDuration (3 3) id "Potion-20-23")))
	(kind "wynncraft/recipe/get/Potion-23-25" code 200 timestamp 1610855763948 version "2.1.0" 
	 data ((level (23 25) type "POTION" skill "ALCHEMISM" materials ((item "Refined Salmon Oil" amount 2) (item "Refined Oat Grains" amount 4)) healthOrDamage (150 177) duration (492 504) basicDuration (3 3) id "Potion-23-25")))
	(kind "wynncraft/recipe/get/Potion-27-29" code 200 timestamp 1610855764190 version "2.1.0" 
	 data ((level (27 29) type "POTION" skill "ALCHEMISM" materials ((item "Refined Salmon Oil" amount 2) (item "Refined Oat Grains" amount 4)) healthOrDamage (210 243) duration (516 528) basicDuration (3 3) id "Potion-27-29")))
	(kind "wynncraft/recipe/get/Potion-25-27" code 200 timestamp 1610855764416 version "2.1.0" 
	 data ((level (25 27) type "POTION" skill "ALCHEMISM" materials ((item "Refined Salmon Oil" amount 2) (item "Refined Oat Grains" amount 4)) healthOrDamage (177 210) duration (504 516) basicDuration (3 3) id "Potion-25-27")))
	(kind "wynncraft/recipe/get/Relik-20-23" code 200 timestamp 1610855764619 version "2.1.0" 
	 data ((level (20 23) type "RELIK" skill "WOODWORKING" materials ((item "Refined Salmon Oil" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (36 39) durability (242 252) id "Relik-20-23")))
	(kind "wynncraft/recipe/get/Relik-23-25" code 200 timestamp 1610855764860 version "2.1.0" 
	 data ((level (23 25) type "RELIK" skill "WOODWORKING" materials ((item "Refined Salmon Oil" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (39 42) durability (252 259) id "Relik-23-25")))
	(kind "wynncraft/recipe/get/Relik-25-27" code 200 timestamp 1610855765052 version "2.1.0" 
	 data ((level (25 27) type "RELIK" skill "WOODWORKING" materials ((item "Refined Salmon Oil" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (41 43) durability (259 266) id "Relik-25-27")))
	(kind "wynncraft/recipe/get/Relik-27-29" code 200 timestamp 1610855765327 version "2.1.0" 
	 data ((level (27 29) type "RELIK" skill "WOODWORKING" materials ((item "Refined Salmon Oil" amount 4) (item "Refined Willow Wood" amount 2)) healthOrDamage (43 48) durability (266 273) id "Relik-27-29")))
	(kind "wynncraft/recipe/get/Ring-20-23" code 200 timestamp 1610855765519 version "2.1.0" 
	 data ((level (20 23) type "RING" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 2)) healthOrDamage (0 0) durability (242 252) id "Ring-20-23")))
	(kind "wynncraft/recipe/get/Ring-23-25" code 200 timestamp 1610855765712 version "2.1.0" 
	 data ((level (23 25) type "RING" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 2)) healthOrDamage (0 0) durability (252 259) id "Ring-23-25")))
	(kind "wynncraft/recipe/get/Ring-25-27" code 200 timestamp 1610855765920 version "2.1.0" 
	 data ((level (25 27) type "RING" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 2)) healthOrDamage (0 0) durability (259 266) id "Ring-25-27")))
	(kind "wynncraft/recipe/get/Ring-27-29" code 200 timestamp 1610855766105 version "2.1.0" 
	 data ((level (27 29) type "RING" skill "JEWELING" materials ((item "Refined Gold Gem" amount 2) (item "Refined Salmon Oil" amount 2)) healthOrDamage (0 0) durability (266 273) id "Ring-27-29")))
	(kind "wynncraft/recipe/get/Scroll-20-23" code 200 timestamp 1610855766333 version "2.1.0" 
	 data ((level (20 23) type "SCROLL" skill "SCRIBING" materials ((item "Refined Salmon Oil" amount 2) (item "Refined Willow Paper" amount 2)) healthOrDamage (89 109) duration (474 492) basicDuration (3 3) id "Scroll-20-23")))
	(kind "wynncraft/recipe/get/Scroll-23-25" code 200 timestamp 1610855766524 version "2.1.0" 
	 data ((level (23 25) type "SCROLL" skill "SCRIBING" materials ((item "Refined Salmon Oil" amount 2) (item "Refined Willow Paper" amount 2)) healthOrDamage (109 128) duration (492 504) basicDuration (3 3) id "Scroll-23-25")))
	(kind "wynncraft/recipe/get/Scroll-25-27" code 200 timestamp 1610855766729 version "2.1.0" 
	 data ((level (25 27) type "SCROLL" skill "SCRIBING" materials ((item "Refined Salmon Oil" amount 2) (item "Refined Willow Paper" amount 2)) healthOrDamage (128 151) duration (504 516) basicDuration (3 3) id "Scroll-25-27")))
	(kind "wynncraft/recipe/get/Scroll-27-29" code 200 timestamp 1610855766928 version "2.1.0" 
	 data ((level (27 29) type "SCROLL" skill "SCRIBING" materials ((item "Refined Salmon Oil" amount 2) (item "Refined Willow Paper" amount 2)) healthOrDamage (151 174) duration (516 528) basicDuration (3 3) id "Scroll-27-29")))
	(kind "wynncraft/recipe/get/Spear-20-23" code 200 timestamp 1610855767231 version "2.1.0" 
	 data ((level (20 23) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Willow Wood" amount 4)) healthOrDamage (26 28) durability (242 252) id "Spear-20-23")))
	(kind "wynncraft/recipe/get/Spear-23-25" code 200 timestamp 1610855767431 version "2.1.0" 
	 data ((level (23 25) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Willow Wood" amount 4)) healthOrDamage (27 29) durability (252 259) id "Spear-23-25")))
	(kind "wynncraft/recipe/get/Spear-25-27" code 200 timestamp 1610855767629 version "2.1.0" 
	 data ((level (25 27) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Willow Wood" amount 4)) healthOrDamage (28 30) durability (259 266) id "Spear-25-27")))
	(kind "wynncraft/recipe/get/Spear-27-29" code 200 timestamp 1610855768029 version "2.1.0" 
	 data ((level (27 29) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Gold Ingot" amount 2) (item "Refined Willow Wood" amount 4)) healthOrDamage (29 32) durability (266 273) id "Spear-27-29")))
	(kind "wynncraft/recipe/get/Wand-20-23" code 200 timestamp 1610855768228 version "2.1.0" 
	 data ((level (20 23) type "WAND" skill "WOODWORKING" materials ((item "Refined Oat String" amount 2) (item "Refined Willow Wood" amount 4)) healthOrDamage (19 21) durability (242 252) id "Wand-20-23")))
	(kind "wynncraft/recipe/get/Wand-23-25" code 200 timestamp 1610855768460 version "2.1.0" 
	 data ((level (23 25) type "WAND" skill "WOODWORKING" materials ((item "Refined Oat String" amount 2) (item "Refined Willow Wood" amount 4)) healthOrDamage (20 23) durability (252 259) id "Wand-23-25")))
	(kind "wynncraft/recipe/get/Wand-25-27" code 200 timestamp 1610855768647 version "2.1.0" 
	 data ((level (25 27) type "WAND" skill "WOODWORKING" materials ((item "Refined Oat String" amount 2) (item "Refined Willow Wood" amount 4)) healthOrDamage (22 24) durability (259 266) id "Wand-25-27")))
	(kind "wynncraft/recipe/get/Wand-27-29" code 200 timestamp 1610855768855 version "2.1.0" 
	 data ((level (27 29) type "WAND" skill "WOODWORKING" materials ((item "Refined Oat String" amount 2) (item "Refined Willow Wood" amount 4)) healthOrDamage (22 25) durability (266 273) id "Wand-27-29")))
	(kind "wynncraft/recipe/get/Boots-30-33" code 200 timestamp 1610855769036 version "2.1.0" 
	 data ((level (30 33) type "BOOTS" skill "TAILORING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Malt String" amount 6)) healthOrDamage (160 190) durability (277 287) id "Boots-30-33")))
	(kind "wynncraft/recipe/get/Boots-33-35" code 200 timestamp 1610855769232 version "2.1.0" 
	 data ((level (33 35) type "BOOTS" skill "TAILORING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Malt String" amount 6)) healthOrDamage (190 225) durability (287 294) id "Boots-33-35")))
	(kind "wynncraft/recipe/get/Boots-35-37" code 200 timestamp 1610857270402 version "2.1.0" 
	 data ((level (35 37) type "BOOTS" skill "TAILORING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Malt String" amount 6)) healthOrDamage (225 260) durability (294 301) id "Boots-35-37")))
	(kind "wynncraft/recipe/get/Boots-37-39" code 200 timestamp 1610857270584 version "2.1.0" 
	 data ((level (37 39) type "BOOTS" skill "TAILORING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Malt String" amount 6)) healthOrDamage (260 300) durability (301 308) id "Boots-37-39")))
	(kind "wynncraft/recipe/get/Bow-30-33" code 200 timestamp 1610857270798 version "2.1.0" 
	 data ((level (30 33) type "BOW" skill "WOODWORKING" materials ((item "Refined Malt String" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (49 55) durability (277 287) id "Bow-30-33")))
	(kind "wynncraft/recipe/get/Bow-33-35" code 200 timestamp 1610857270994 version "2.1.0" 
	 data ((level (33 35) type "BOW" skill "WOODWORKING" materials ((item "Refined Malt String" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (55 61) durability (287 294) id "Bow-33-35")))
	(kind "wynncraft/recipe/get/Bow-35-37" code 200 timestamp 1610857271219 version "2.1.0" 
	 data ((level (35 37) type "BOW" skill "WOODWORKING" materials ((item "Refined Malt String" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (61 66) durability (294 301) id "Bow-35-37")))
	(kind "wynncraft/recipe/get/Necklace-13-15" code 200 timestamp 1610857271419 version "2.1.0" 
	 data ((level (13 15) type "NECKLACE" skill "JEWELING" materials ((item "Refined Granite Gem" amount 2) (item "Refined Trout Oil" amount 6)) healthOrDamage (0 0) durability (217 224) id "Necklace-13-15")))
	(kind "wynncraft/recipe/get/Bow-37-39" code 200 timestamp 1610857271619 version "2.1.0" 
	 data ((level (37 39) type "BOW" skill "WOODWORKING" materials ((item "Refined Malt String" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (66 72) durability (301 308) id "Bow-37-39")))
	(kind "wynncraft/recipe/get/Bracelet-30-33" code 200 timestamp 1610857271809 version "2.1.0" 
	 data ((level (30 33) type "BRACELET" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 6)) healthOrDamage (0 0) durability (277 287) id "Bracelet-30-33")))
	(kind "wynncraft/recipe/get/Bracelet-33-35" code 200 timestamp 1610857272018 version "2.1.0" 
	 data ((level (33 35) type "BRACELET" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 6)) healthOrDamage (0 0) durability (287 294) id "Bracelet-33-35")))
	(kind "wynncraft/recipe/get/Bracelet-35-37" code 200 timestamp 1610857272219 version "2.1.0" 
	 data ((level (35 37) type "BRACELET" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 6)) healthOrDamage (0 0) durability (294 301) id "Bracelet-35-37")))
	(kind "wynncraft/recipe/get/Bracelet-37-39" code 200 timestamp 1610857272397 version "2.1.0" 
	 data ((level (37 39) type "BRACELET" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 6)) healthOrDamage (0 0) durability (301 308) id "Bracelet-37-39")))
	(kind "wynncraft/recipe/get/Chestplate-30-33" code 200 timestamp 1610857272630 version "2.1.0" 
	 data ((level (30 33) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Acacia Paper" amount 3)) healthOrDamage (160 190) durability (277 287) id "Chestplate-30-33")))
	(kind "wynncraft/recipe/get/Chestplate-35-37" code 200 timestamp 1610857272837 version "2.1.0" 
	 data ((level (35 37) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Acacia Paper" amount 3)) healthOrDamage (225 260) durability (294 301) id "Chestplate-35-37")))
	(kind "wynncraft/recipe/get/Chestplate-33-35" code 200 timestamp 1610857273015 version "2.1.0" 
	 data ((level (33 35) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Acacia Paper" amount 3)) healthOrDamage (190 225) durability (287 294) id "Chestplate-33-35")))
	(kind "wynncraft/recipe/get/Dagger-30-33" code 200 timestamp 1610857273227 version "2.1.0" 
	 data ((level (30 33) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (41 46) durability (277 287) id "Dagger-30-33")))
	(kind "wynncraft/recipe/get/Chestplate-37-39" code 200 timestamp 1610857273415 version "2.1.0" 
	 data ((level (37 39) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Acacia Paper" amount 3)) healthOrDamage (260 300) durability (301 308) id "Chestplate-37-39")))
	(kind "wynncraft/recipe/get/Dagger-35-37" code 200 timestamp 1610857273629 version "2.1.0" 
	 data ((level (35 37) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (51 55) durability (294 301) id "Dagger-35-37")))
	(kind "wynncraft/recipe/get/Dagger-37-39" code 200 timestamp 1610857273815 version "2.1.0" 
	 data ((level (37 39) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (55 60) durability (301 308) id "Dagger-37-39")))
	(kind "wynncraft/recipe/get/Food-30-33" code 200 timestamp 1610857274024 version "2.1.0" 
	 data ((level (30 33) type "FOOD" skill "COOKING" materials ((item "Refined Carp Meat" amount 6) (item "Refined Malt Grains" amount 3)) healthOrDamage (254 298) duration (1602 1656) basicDuration (60 60) id "Food-30-33")))
	(kind "wynncraft/recipe/get/Dagger-33-35" code 200 timestamp 1610855769418 version "2.1.0" 
	 data ((level (33 35) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (46 51) durability (287 294) id "Dagger-33-35")))
	(kind "wynncraft/recipe/get/Food-33-35" code 200 timestamp 1610855769611 version "2.1.0" 
	 data ((level (33 35) type "FOOD" skill "COOKING" materials ((item "Refined Carp Meat" amount 6) (item "Refined Malt Grains" amount 3)) healthOrDamage (298 342) duration (1656 1692) basicDuration (60 60) id "Food-33-35")))
	(kind "wynncraft/recipe/get/Food-35-37" code 200 timestamp 1610855769829 version "2.1.0" 
	 data ((level (35 37) type "FOOD" skill "COOKING" materials ((item "Refined Carp Meat" amount 6) (item "Refined Malt Grains" amount 3)) healthOrDamage (342 403) duration (1692 1728) basicDuration (60 60) id "Food-35-37")))
	(kind "wynncraft/recipe/get/Food-37-39" code 200 timestamp 1610855770047 version "2.1.0" 
	 data ((level (37 39) type "FOOD" skill "COOKING" materials ((item "Refined Carp Meat" amount 6) (item "Refined Malt Grains" amount 3)) healthOrDamage (403 463) duration (1728 1764) basicDuration (60 60) id "Food-37-39")))
	(kind "wynncraft/recipe/get/Helmet-30-33" code 200 timestamp 1610855770225 version "2.1.0" 
	 data ((level (30 33) type "HELMET" skill "ARMOURING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Acacia Paper" amount 6)) healthOrDamage (160 190) durability (277 287) id "Helmet-30-33")))
	(kind "wynncraft/recipe/get/Helmet-33-35" code 200 timestamp 1610855770431 version "2.1.0" 
	 data ((level (33 35) type "HELMET" skill "ARMOURING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Acacia Paper" amount 6)) healthOrDamage (190 225) durability (287 294) id "Helmet-33-35")))
	(kind "wynncraft/recipe/get/Helmet-35-37" code 200 timestamp 1610855770686 version "2.1.0" 
	 data ((level (35 37) type "HELMET" skill "ARMOURING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Acacia Paper" amount 6)) healthOrDamage (225 260) durability (294 301) id "Helmet-35-37")))
	(kind "wynncraft/recipe/get/Helmet-37-39" code 200 timestamp 1610855770900 version "2.1.0" 
	 data ((level (37 39) type "HELMET" skill "ARMOURING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Acacia Paper" amount 6)) healthOrDamage (260 300) durability (301 308) id "Helmet-37-39")))
	(kind "wynncraft/recipe/get/Necklace-30-33" code 200 timestamp 1610855771104 version "2.1.0" 
	 data ((level (30 33) type "NECKLACE" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 9)) healthOrDamage (0 0) durability (277 287) id "Necklace-30-33")))
	(kind "wynncraft/recipe/get/Necklace-33-35" code 200 timestamp 1610855771332 version "2.1.0" 
	 data ((level (33 35) type "NECKLACE" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 9)) healthOrDamage (0 0) durability (287 294) id "Necklace-33-35")))
	(kind "wynncraft/recipe/get/Necklace-35-37" code 200 timestamp 1610855771570 version "2.1.0" 
	 data ((level (35 37) type "NECKLACE" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 9)) healthOrDamage (0 0) durability (294 301) id "Necklace-35-37")))
	(kind "wynncraft/recipe/get/Necklace-37-39" code 200 timestamp 1610855771790 version "2.1.0" 
	 data ((level (37 39) type "NECKLACE" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 9)) healthOrDamage (0 0) durability (301 308) id "Necklace-37-39")))
	(kind "wynncraft/recipe/get/Leggings-33-35" code 200 timestamp 1610855771977 version "2.1.0" 
	 data ((level (33 35) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Malt String" amount 3)) healthOrDamage (190 225) durability (287 294) id "Leggings-33-35")))
	(kind "wynncraft/recipe/get/Leggings-30-33" code 200 timestamp 1610855772164 version "2.1.0" 
	 data ((level (30 33) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Malt String" amount 3)) healthOrDamage (160 190) durability (277 287) id "Leggings-30-33")))
	(kind "wynncraft/recipe/get/Leggings-35-37" code 200 timestamp 1610855772352 version "2.1.0" 
	 data ((level (35 37) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Malt String" amount 3)) healthOrDamage (225 260) durability (294 301) id "Leggings-35-37")))
	(kind "wynncraft/recipe/get/Leggings-37-39" code 200 timestamp 1610855772581 version "2.1.0" 
	 data ((level (37 39) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Sandstone Ingot" amount 6) (item "Refined Malt String" amount 3)) healthOrDamage (260 300) durability (301 308) id "Leggings-37-39")))
	(kind "wynncraft/recipe/get/Potion-30-33" code 200 timestamp 1610855772863 version "2.1.0" 
	 data ((level (30 33) type "POTION" skill "ALCHEMISM" materials ((item "Refined Carp Oil" amount 3) (item "Refined Malt Grains" amount 6)) healthOrDamage (254 298) duration (534 552) basicDuration (3 3) id "Potion-30-33")))
	(kind "wynncraft/recipe/get/Potion-33-35" code 200 timestamp 1610855773071 version "2.1.0" 
	 data ((level (33 35) type "POTION" skill "ALCHEMISM" materials ((item "Refined Carp Oil" amount 3) (item "Refined Malt Grains" amount 6)) healthOrDamage (298 342) duration (552 564) basicDuration (3 3) id "Potion-33-35")))
	(kind "wynncraft/recipe/get/Potion-35-37" code 200 timestamp 1610855773267 version "2.1.0" 
	 data ((level (35 37) type "POTION" skill "ALCHEMISM" materials ((item "Refined Carp Oil" amount 3) (item "Refined Malt Grains" amount 6)) healthOrDamage (342 403) duration (564 576) basicDuration (3 3) id "Potion-35-37")))
	(kind "wynncraft/recipe/get/Potion-37-39" code 200 timestamp 1610855773561 version "2.1.0" 
	 data ((level (37 39) type "POTION" skill "ALCHEMISM" materials ((item "Refined Carp Oil" amount 3) (item "Refined Malt Grains" amount 6)) healthOrDamage (403 463) duration (576 588) basicDuration (3 3) id "Potion-37-39")))
	(kind "wynncraft/recipe/get/Relik-30-33" code 200 timestamp 1610855773850 version "2.1.0" 
	 data ((level (30 33) type "RELIK" skill "WOODWORKING" materials ((item "Refined Carp Oil" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (49 55) durability (277 287) id "Relik-30-33")))
	(kind "wynncraft/recipe/get/Relik-33-35" code 200 timestamp 1610855774061 version "2.1.0" 
	 data ((level (33 35) type "RELIK" skill "WOODWORKING" materials ((item "Refined Carp Oil" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (55 61) durability (287 294) id "Relik-33-35")))
	(kind "wynncraft/recipe/get/Relik-35-37" code 200 timestamp 1610855774254 version "2.1.0" 
	 data ((level (35 37) type "RELIK" skill "WOODWORKING" materials ((item "Refined Carp Oil" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (61 66) durability (294 301) id "Relik-35-37")))
	(kind "wynncraft/recipe/get/Ring-30-33" code 200 timestamp 1610855774445 version "2.1.0" 
	 data ((level (30 33) type "RING" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 3)) healthOrDamage (0 0) durability (277 287) id "Ring-30-33")))
	(kind "wynncraft/recipe/get/Relik-37-39" code 200 timestamp 1610855774635 version "2.1.0" 
	 data ((level (37 39) type "RELIK" skill "WOODWORKING" materials ((item "Refined Carp Oil" amount 6) (item "Refined Acacia Wood" amount 3)) healthOrDamage (66 72) durability (301 308) id "Relik-37-39")))
	(kind "wynncraft/recipe/get/Ring-33-35" code 200 timestamp 1610855774874 version "2.1.0" 
	 data ((level (33 35) type "RING" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 3)) healthOrDamage (0 0) durability (287 294) id "Ring-33-35")))
	(kind "wynncraft/recipe/get/Ring-35-37" code 200 timestamp 1610855775083 version "2.1.0" 
	 data ((level (35 37) type "RING" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 3)) healthOrDamage (0 0) durability (294 301) id "Ring-35-37")))
	(kind "wynncraft/recipe/get/Ring-37-39" code 200 timestamp 1610855775326 version "2.1.0" 
	 data ((level (37 39) type "RING" skill "JEWELING" materials ((item "Refined Sandstone Gem" amount 3) (item "Refined Carp Oil" amount 3)) healthOrDamage (0 0) durability (301 308) id "Ring-37-39")))
	(kind "wynncraft/recipe/get/Scroll-30-33" code 200 timestamp 1610855775506 version "2.1.0" 
	 data ((level (30 33) type "SCROLL" skill "SCRIBING" materials ((item "Refined Carp Oil" amount 3) (item "Refined Acacia Paper" amount 3)) healthOrDamage (181 212) duration (534 552) basicDuration (3 3) id "Scroll-30-33")))
	(kind "wynncraft/recipe/get/Scroll-33-35" code 200 timestamp 1610855775693 version "2.1.0" 
	 data ((level (33 35) type "SCROLL" skill "SCRIBING" materials ((item "Refined Carp Oil" amount 3) (item "Refined Acacia Paper" amount 3)) healthOrDamage (212 243) duration (552 564) basicDuration (3 3) id "Scroll-33-35")))
	(kind "wynncraft/recipe/get/Scroll-35-37" code 200 timestamp 1610855775986 version "2.1.0" 
	 data ((level (35 37) type "SCROLL" skill "SCRIBING" materials ((item "Refined Carp Oil" amount 3) (item "Refined Acacia Paper" amount 3)) healthOrDamage (243 286) duration (564 576) basicDuration (3 3) id "Scroll-35-37")))
	(kind "wynncraft/recipe/get/Scroll-37-39" code 200 timestamp 1610855776182 version "2.1.0" 
	 data ((level (37 39) type "SCROLL" skill "SCRIBING" materials ((item "Refined Carp Oil" amount 3) (item "Refined Acacia Paper" amount 3)) healthOrDamage (286 328) duration (576 588) basicDuration (3 3) id "Scroll-37-39")))
	(kind "wynncraft/recipe/get/Spear-30-33" code 200 timestamp 1610855776390 version "2.1.0" 
	 data ((level (30 33) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Acacia Wood" amount 6)) healthOrDamage (32 37) durability (277 287) id "Spear-30-33")))
	(kind "wynncraft/recipe/get/Spear-33-35" code 200 timestamp 1610855776575 version "2.1.0" 
	 data ((level (33 35) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Acacia Wood" amount 6)) healthOrDamage (37 41) durability (287 294) id "Spear-33-35")))
	(kind "wynncraft/recipe/get/Spear-35-37" code 200 timestamp 1610855776793 version "2.1.0" 
	 data ((level (35 37) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Acacia Wood" amount 6)) healthOrDamage (41 44) durability (294 301) id "Spear-35-37")))
	(kind "wynncraft/recipe/get/Spear-37-39" code 200 timestamp 1610855776982 version "2.1.0" 
	 data ((level (37 39) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Sandstone Ingot" amount 3) (item "Refined Acacia Wood" amount 6)) healthOrDamage (44 48) durability (301 308) id "Spear-37-39")))
	(kind "wynncraft/recipe/get/Wand-30-33" code 200 timestamp 1610855777169 version "2.1.0" 
	 data ((level (30 33) type "WAND" skill "WOODWORKING" materials ((item "Refined Malt String" amount 3) (item "Refined Acacia Wood" amount 6)) healthOrDamage (25 28) durability (277 287) id "Wand-30-33")))
	(kind "wynncraft/recipe/get/Wand-33-35" code 200 timestamp 1610855777364 version "2.1.0" 
	 data ((level (33 35) type "WAND" skill "WOODWORKING" materials ((item "Refined Malt String" amount 3) (item "Refined Acacia Wood" amount 6)) healthOrDamage (28 31) durability (287 294) id "Wand-33-35")))
	(kind "wynncraft/recipe/get/Wand-35-37" code 200 timestamp 1610855777563 version "2.1.0" 
	 data ((level (35 37) type "WAND" skill "WOODWORKING" materials ((item "Refined Malt String" amount 3) (item "Refined Acacia Wood" amount 6)) healthOrDamage (31 33) durability (294 301) id "Wand-35-37")))
	(kind "wynncraft/recipe/get/Boots-40-43" code 200 timestamp 1610855777748 version "2.1.0" 
	 data ((level (40 43) type "BOOTS" skill "TAILORING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Hops String" amount 6)) healthOrDamage (310 365) durability (312 322) id "Boots-40-43")))
	(kind "wynncraft/recipe/get/Wand-37-39" code 200 timestamp 1610855777942 version "2.1.0" 
	 data ((level (37 39) type "WAND" skill "WOODWORKING" materials ((item "Refined Malt String" amount 3) (item "Refined Acacia Wood" amount 6)) healthOrDamage (33 36) durability (301 308) id "Wand-37-39")))
	(kind "wynncraft/recipe/get/Boots-45-47" code 200 timestamp 1610855778154 version "2.1.0" 
	 data ((level (45 47) type "BOOTS" skill "TAILORING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Hops String" amount 6)) healthOrDamage (420 480) durability (329 336) id "Boots-45-47")))
	(kind "wynncraft/recipe/get/Boots-47-49" code 200 timestamp 1610855778338 version "2.1.0" 
	 data ((level (47 49) type "BOOTS" skill "TAILORING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Hops String" amount 6)) healthOrDamage (480 540) durability (336 343) id "Boots-47-49")))
	(kind "wynncraft/recipe/get/Boots-43-45" code 200 timestamp 1610855778516 version "2.1.0" 
	 data ((level (43 45) type "BOOTS" skill "TAILORING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Hops String" amount 6)) healthOrDamage (365 420) durability (322 329) id "Boots-43-45")))
	(kind "wynncraft/recipe/get/Bow-40-43" code 200 timestamp 1610855778734 version "2.1.0" 
	 data ((level (40 43) type "BOW" skill "WOODWORKING" materials ((item "Refined Hops String" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (73 80) durability (312 322) id "Bow-40-43")))
	(kind "wynncraft/recipe/get/Bow-47-49" code 200 timestamp 1610855779006 version "2.1.0" 
	 data ((level (47 49) type "BOW" skill "WOODWORKING" materials ((item "Refined Hops String" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (97 106) durability (336 343) id "Bow-47-49")))
	(kind "wynncraft/recipe/get/Bow-43-45" code 200 timestamp 1610855779208 version "2.1.0" 
	 data ((level (43 45) type "BOW" skill "WOODWORKING" materials ((item "Refined Hops String" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (80 89) durability (322 329) id "Bow-43-45")))
	(kind "wynncraft/recipe/get/Bracelet-40-43" code 200 timestamp 1610855779404 version "2.1.0" 
	 data ((level (40 43) type "BRACELET" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 6)) healthOrDamage (0 0) durability (312 322) id "Bracelet-40-43")))
	(kind "wynncraft/recipe/get/Bow-45-47" code 200 timestamp 1610855779595 version "2.1.0" 
	 data ((level (45 47) type "BOW" skill "WOODWORKING" materials ((item "Refined Hops String" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (89 97) durability (329 336) id "Bow-45-47")))
	(kind "wynncraft/recipe/get/Bracelet-45-47" code 200 timestamp 1610855779812 version "2.1.0" 
	 data ((level (45 47) type "BRACELET" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 6)) healthOrDamage (0 0) durability (329 336) id "Bracelet-45-47")))
	(kind "wynncraft/recipe/get/Bracelet-47-49" code 200 timestamp 1610855780003 version "2.1.0" 
	 data ((level (47 49) type "BRACELET" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 6)) healthOrDamage (0 0) durability (336 343) id "Bracelet-47-49")))
	(kind "wynncraft/recipe/get/Chestplate-43-45" code 200 timestamp 1610855780182 version "2.1.0" 
	 data ((level (43 45) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Spruce Paper" amount 3)) healthOrDamage (365 420) durability (322 329) id "Chestplate-43-45")))
	(kind "wynncraft/recipe/get/Bracelet-43-45" code 200 timestamp 1610855780347 version "2.1.0" 
	 data ((level (43 45) type "BRACELET" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 6)) healthOrDamage (0 0) durability (322 329) id "Bracelet-43-45")))
	(kind "wynncraft/recipe/get/Chestplate-40-43" code 200 timestamp 1610855780529 version "2.1.0" 
	 data ((level (40 43) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Spruce Paper" amount 3)) healthOrDamage (310 365) durability (312 322) id "Chestplate-40-43")))
	(kind "wynncraft/recipe/get/Chestplate-45-47" code 200 timestamp 1610855780704 version "2.1.0" 
	 data ((level (45 47) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Spruce Paper" amount 3)) healthOrDamage (420 480) durability (329 336) id "Chestplate-45-47")))
	(kind "wynncraft/recipe/get/Dagger-40-43" code 200 timestamp 1610855780982 version "2.1.0" 
	 data ((level (40 43) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (61 67) durability (312 322) id "Dagger-40-43")))
	(kind "wynncraft/recipe/get/Chestplate-47-49" code 200 timestamp 1610855781174 version "2.1.0" 
	 data ((level (47 49) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Spruce Paper" amount 3)) healthOrDamage (480 540) durability (336 343) id "Chestplate-47-49")))
	(kind "wynncraft/recipe/get/Dagger-43-45" code 200 timestamp 1610855781450 version "2.1.0" 
	 data ((level (43 45) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (67 74) durability (322 329) id "Dagger-43-45")))
	(kind "wynncraft/recipe/get/Dagger-47-49" code 200 timestamp 1610855781763 version "2.1.0" 
	 data ((level (47 49) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (81 88) durability (336 343) id "Dagger-47-49")))
	(kind "wynncraft/recipe/get/Dagger-45-47" code 200 timestamp 1610855781968 version "2.1.0" 
	 data ((level (45 47) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (74 81) durability (329 336) id "Dagger-45-47")))
	(kind "wynncraft/recipe/get/Food-40-43" code 200 timestamp 1610855782153 version "2.1.0" 
	 data ((level (40 43) type "FOOD" skill "COOKING" materials ((item "Refined Icefish Meat" amount 6) (item "Refined Hops Grains" amount 3)) healthOrDamage (474 551) duration (1782 1836) basicDuration (60 60) id "Food-40-43")))
	(kind "wynncraft/recipe/get/Food-43-45" code 200 timestamp 1610855782355 version "2.1.0" 
	 data ((level (43 45) type "FOOD" skill "COOKING" materials ((item "Refined Icefish Meat" amount 6) (item "Refined Hops Grains" amount 3)) healthOrDamage (551 628) duration (1836 1872) basicDuration (60 60) id "Food-43-45")))
	(kind "wynncraft/recipe/get/Food-45-47" code 200 timestamp 1610855782559 version "2.1.0" 
	 data ((level (45 47) type "FOOD" skill "COOKING" materials ((item "Refined Icefish Meat" amount 6) (item "Refined Hops Grains" amount 3)) healthOrDamage (628 722) duration (1872 1908) basicDuration (60 60) id "Food-45-47")))
	(kind "wynncraft/recipe/get/Food-47-49" code 200 timestamp 1610855782751 version "2.1.0" 
	 data ((level (47 49) type "FOOD" skill "COOKING" materials ((item "Refined Icefish Meat" amount 6) (item "Refined Hops Grains" amount 3)) healthOrDamage (722 815) duration (1908 1944) basicDuration (60 60) id "Food-47-49")))
	(kind "wynncraft/recipe/get/Helmet-40-43" code 200 timestamp 1610855782956 version "2.1.0" 
	 data ((level (40 43) type "HELMET" skill "ARMOURING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Spruce Paper" amount 6)) healthOrDamage (310 365) durability (312 322) id "Helmet-40-43")))
	(kind "wynncraft/recipe/get/Helmet-43-45" code 200 timestamp 1610855783246 version "2.1.0" 
	 data ((level (43 45) type "HELMET" skill "ARMOURING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Spruce Paper" amount 6)) healthOrDamage (365 420) durability (322 329) id "Helmet-43-45")))
	(kind "wynncraft/recipe/get/Helmet-45-47" code 200 timestamp 1610855783470 version "2.1.0" 
	 data ((level (45 47) type "HELMET" skill "ARMOURING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Spruce Paper" amount 6)) healthOrDamage (420 480) durability (329 336) id "Helmet-45-47")))
	(kind "wynncraft/recipe/get/Necklace-40-43" code 200 timestamp 1610855783666 version "2.1.0" 
	 data ((level (40 43) type "NECKLACE" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 9)) healthOrDamage (0 0) durability (312 322) id "Necklace-40-43")))
	(kind "wynncraft/recipe/get/Necklace-43-45" code 200 timestamp 1610855783870 version "2.1.0" 
	 data ((level (43 45) type "NECKLACE" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 9)) healthOrDamage (0 0) durability (322 329) id "Necklace-43-45")))
	(kind "wynncraft/recipe/get/Necklace-45-47" code 200 timestamp 1610855784095 version "2.1.0" 
	 data ((level (45 47) type "NECKLACE" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 9)) healthOrDamage (0 0) durability (329 336) id "Necklace-45-47")))
	(kind "wynncraft/recipe/get/Helmet-47-49" code 200 timestamp 1610855784305 version "2.1.0" 
	 data ((level (47 49) type "HELMET" skill "ARMOURING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Spruce Paper" amount 6)) healthOrDamage (480 540) durability (336 343) id "Helmet-47-49")))
	(kind "wynncraft/recipe/get/Necklace-47-49" code 200 timestamp 1610855785518 version "2.1.0" 
	 data ((level (47 49) type "NECKLACE" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 9)) healthOrDamage (0 0) durability (336 343) id "Necklace-47-49")))
	(kind "wynncraft/recipe/get/Leggings-40-43" code 200 timestamp 1610855785749 version "2.1.0" 
	 data ((level (40 43) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Hops String" amount 3)) healthOrDamage (310 365) durability (312 322) id "Leggings-40-43")))
	(kind "wynncraft/recipe/get/Leggings-43-45" code 200 timestamp 1610855785933 version "2.1.0" 
	 data ((level (43 45) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Hops String" amount 3)) healthOrDamage (365 420) durability (322 329) id "Leggings-43-45")))
	(kind "wynncraft/recipe/get/Leggings-45-47" code 200 timestamp 1610855786106 version "2.1.0" 
	 data ((level (45 47) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Hops String" amount 3)) healthOrDamage (420 480) durability (329 336) id "Leggings-45-47")))
	(kind "wynncraft/recipe/get/Leggings-47-49" code 200 timestamp 1610855786397 version "2.1.0" 
	 data ((level (47 49) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Iron Ingot" amount 6) (item "Refined Hops String" amount 3)) healthOrDamage (480 540) durability (336 343) id "Leggings-47-49")))
	(kind "wynncraft/recipe/get/Potion-43-45" code 200 timestamp 1610855786616 version "2.1.0" 
	 data ((level (43 45) type "POTION" skill "ALCHEMISM" materials ((item "Refined Icefish Oil" amount 3) (item "Refined Hops Grains" amount 6)) healthOrDamage (551 628) duration (612 624) basicDuration (3 3) id "Potion-43-45")))
	(kind "wynncraft/recipe/get/Potion-45-47" code 200 timestamp 1610855786850 version "2.1.0" 
	 data ((level (45 47) type "POTION" skill "ALCHEMISM" materials ((item "Refined Icefish Oil" amount 3) (item "Refined Hops Grains" amount 6)) healthOrDamage (628 722) duration (624 636) basicDuration (3 3) id "Potion-45-47")))
	(kind "wynncraft/recipe/get/Relik-40-43" code 200 timestamp 1610855787133 version "2.1.0" 
	 data ((level (40 43) type "RELIK" skill "WOODWORKING" materials ((item "Refined Icefish Oil" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (73 80) durability (312 322) id "Relik-40-43")))
	(kind "wynncraft/recipe/get/Potion-47-49" code 200 timestamp 1610855787343 version "2.1.0" 
	 data ((level (47 49) type "POTION" skill "ALCHEMISM" materials ((item "Refined Icefish Oil" amount 3) (item "Refined Hops Grains" amount 6)) healthOrDamage (722 815) duration (636 648) basicDuration (3 3) id "Potion-47-49")))
	(kind "wynncraft/recipe/get/Potion-40-43" code 200 timestamp 1610855787545 version "2.1.0" 
	 data ((level (40 43) type "POTION" skill "ALCHEMISM" materials ((item "Refined Icefish Oil" amount 3) (item "Refined Hops Grains" amount 6)) healthOrDamage (474 551) duration (594 612) basicDuration (3 3) id "Potion-40-43")))
	(kind "wynncraft/recipe/get/Relik-45-47" code 200 timestamp 1610855787785 version "2.1.0" 
	 data ((level (45 47) type "RELIK" skill "WOODWORKING" materials ((item "Refined Icefish Oil" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (89 97) durability (329 336) id "Relik-45-47")))
	(kind "wynncraft/recipe/get/Ring-40-43" code 200 timestamp 1610855788009 version "2.1.0" 
	 data ((level (40 43) type "RING" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 3)) healthOrDamage (0 0) durability (312 322) id "Ring-40-43")))
	(kind "wynncraft/recipe/get/Relik-43-45" code 200 timestamp 1610855788210 version "2.1.0" 
	 data ((level (43 45) type "RELIK" skill "WOODWORKING" materials ((item "Refined Icefish Oil" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (80 89) durability (322 329) id "Relik-43-45")))
	(kind "wynncraft/recipe/get/Ring-45-47" code 200 timestamp 1610855788433 version "2.1.0" 
	 data ((level (45 47) type "RING" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 3)) healthOrDamage (0 0) durability (329 336) id "Ring-45-47")))
	(kind "wynncraft/recipe/get/Relik-47-49" code 200 timestamp 1610855788642 version "2.1.0" 
	 data ((level (47 49) type "RELIK" skill "WOODWORKING" materials ((item "Refined Icefish Oil" amount 6) (item "Refined Spruce Wood" amount 3)) healthOrDamage (97 106) durability (336 343) id "Relik-47-49")))
	(kind "wynncraft/recipe/get/Ring-43-45" code 200 timestamp 1610855788875 version "2.1.0" 
	 data ((level (43 45) type "RING" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 3)) healthOrDamage (0 0) durability (322 329) id "Ring-43-45")))
	(kind "wynncraft/recipe/get/Ring-47-49" code 200 timestamp 1610855789067 version "2.1.0" 
	 data ((level (47 49) type "RING" skill "JEWELING" materials ((item "Refined Iron Gem" amount 3) (item "Refined Icefish Oil" amount 3)) healthOrDamage (0 0) durability (336 343) id "Ring-47-49")))
	(kind "wynncraft/recipe/get/Scroll-40-43" code 200 timestamp 1610855789249 version "2.1.0" 
	 data ((level (40 43) type "SCROLL" skill "SCRIBING" materials ((item "Refined Icefish Oil" amount 3) (item "Refined Spruce Paper" amount 3)) healthOrDamage (335 389) duration (594 612) basicDuration (3 3) id "Scroll-40-43")))
	(kind "wynncraft/recipe/get/Scroll-43-45" code 200 timestamp 1610855789454 version "2.1.0" 
	 data ((level (43 45) type "SCROLL" skill "SCRIBING" materials ((item "Refined Icefish Oil" amount 3) (item "Refined Spruce Paper" amount 3)) healthOrDamage (389 443) duration (612 624) basicDuration (3 3) id "Scroll-43-45")))
	(kind "wynncraft/recipe/get/Scroll-45-47" code 200 timestamp 1610855789673 version "2.1.0" 
	 data ((level (45 47) type "SCROLL" skill "SCRIBING" materials ((item "Refined Icefish Oil" amount 3) (item "Refined Spruce Paper" amount 3)) healthOrDamage (443 509) duration (624 636) basicDuration (3 3) id "Scroll-45-47")))
	(kind "wynncraft/recipe/get/Scroll-47-49" code 200 timestamp 1610855789861 version "2.1.0" 
	 data ((level (47 49) type "SCROLL" skill "SCRIBING" materials ((item "Refined Icefish Oil" amount 3) (item "Refined Spruce Paper" amount 3)) healthOrDamage (509 574) duration (636 648) basicDuration (3 3) id "Scroll-47-49")))
	(kind "wynncraft/recipe/get/Spear-43-45" code 200 timestamp 1610855790050 version "2.1.0" 
	 data ((level (43 45) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Spruce Wood" amount 6)) healthOrDamage (54 59) durability (322 329) id "Spear-43-45")))
	(kind "wynncraft/recipe/get/Spear-40-43" code 200 timestamp 1610855790317 version "2.1.0" 
	 data ((level (40 43) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Spruce Wood" amount 6)) healthOrDamage (49 54) durability (312 322) id "Spear-40-43")))
	(kind "wynncraft/recipe/get/Spear-45-47" code 200 timestamp 1610855790510 version "2.1.0" 
	 data ((level (45 47) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Spruce Wood" amount 6)) healthOrDamage (59 65) durability (329 336) id "Spear-45-47")))
	(kind "wynncraft/recipe/get/Spear-47-49" code 200 timestamp 1610855790707 version "2.1.0" 
	 data ((level (47 49) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Iron Ingot" amount 3) (item "Refined Spruce Wood" amount 6)) healthOrDamage (65 70) durability (336 343) id "Spear-47-49")))
	(kind "wynncraft/recipe/get/Wand-40-43" code 200 timestamp 1610855790909 version "2.1.0" 
	 data ((level (40 43) type "WAND" skill "WOODWORKING" materials ((item "Refined Hops String" amount 3) (item "Refined Spruce Wood" amount 6)) healthOrDamage (37 40) durability (312 322) id "Wand-40-43")))
	(kind "wynncraft/recipe/get/Wand-43-45" code 200 timestamp 1610855791118 version "2.1.0" 
	 data ((level (43 45) type "WAND" skill "WOODWORKING" materials ((item "Refined Hops String" amount 3) (item "Refined Spruce Wood" amount 6)) healthOrDamage (40 44) durability (322 329) id "Wand-43-45")))
	(kind "wynncraft/recipe/get/Wand-45-47" code 200 timestamp 1610855791351 version "2.1.0" 
	 data ((level (45 47) type "WAND" skill "WOODWORKING" materials ((item "Refined Hops String" amount 3) (item "Refined Spruce Wood" amount 6)) healthOrDamage (44 49) durability (329 336) id "Wand-45-47")))
	(kind "wynncraft/recipe/get/Wand-47-49" code 200 timestamp 1610855791549 version "2.1.0" 
	 data ((level (47 49) type "WAND" skill "WOODWORKING" materials ((item "Refined Hops String" amount 3) (item "Refined Spruce Wood" amount 6)) healthOrDamage (49 53) durability (336 343) id "Wand-47-49")))
	(kind "wynncraft/recipe/get/Boots-50-53" code 200 timestamp 1610855791768 version "2.1.0" 
	 data ((level (50 53) type "BOOTS" skill "TAILORING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Rye String" amount 8)) healthOrDamage (550 630) durability (347 357) id "Boots-50-53")))
	(kind "wynncraft/recipe/get/Boots-53-55" code 200 timestamp 1610855791949 version "2.1.0" 
	 data ((level (53 55) type "BOOTS" skill "TAILORING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Rye String" amount 8)) healthOrDamage (630 740) durability (357 364) id "Boots-53-55")))
	(kind "wynncraft/recipe/get/Boots-55-57" code 200 timestamp 1610855792178 version "2.1.0" 
	 data ((level (55 57) type "BOOTS" skill "TAILORING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Rye String" amount 8)) healthOrDamage (740 850) durability (364 371) id "Boots-55-57")))
	(kind "wynncraft/recipe/get/Boots-57-59" code 200 timestamp 1610855792433 version "2.1.0" 
	 data ((level (57 59) type "BOOTS" skill "TAILORING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Rye String" amount 8)) healthOrDamage (850 960) durability (371 378) id "Boots-57-59")))
	(kind "wynncraft/recipe/get/Bow-50-53" code 200 timestamp 1610855792625 version "2.1.0" 
	 data ((level (50 53) type "BOW" skill "WOODWORKING" materials ((item "Refined Rye String" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (107 116) durability (347 357) id "Bow-50-53")))
	(kind "wynncraft/recipe/get/Bow-55-57" code 200 timestamp 1610855792820 version "2.1.0" 
	 data ((level (55 57) type "BOW" skill "WOODWORKING" materials ((item "Refined Rye String" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (127 138) durability (364 371) id "Bow-55-57")))
	(kind "wynncraft/recipe/get/Bow-53-55" code 200 timestamp 1610855793024 version "2.1.0" 
	 data ((level (53 55) type "BOW" skill "WOODWORKING" materials ((item "Refined Rye String" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (116 127) durability (357 364) id "Bow-53-55")))
	(kind "wynncraft/recipe/get/Bow-57-59" code 200 timestamp 1610855793216 version "2.1.0" 
	 data ((level (57 59) type "BOW" skill "WOODWORKING" materials ((item "Refined Rye String" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (138 149) durability (371 378) id "Bow-57-59")))
	(kind "wynncraft/recipe/get/Bracelet-50-53" code 200 timestamp 1610855793428 version "2.1.0" 
	 data ((level (50 53) type "BRACELET" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 8)) healthOrDamage (0 0) durability (347 357) id "Bracelet-50-53")))
	(kind "wynncraft/recipe/get/Bracelet-53-55" code 200 timestamp 1610855793663 version "2.1.0" 
	 data ((level (53 55) type "BRACELET" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 8)) healthOrDamage (0 0) durability (357 364) id "Bracelet-53-55")))
	(kind "wynncraft/recipe/get/Bracelet-55-57" code 200 timestamp 1610855793857 version "2.1.0" 
	 data ((level (55 57) type "BRACELET" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 8)) healthOrDamage (0 0) durability (364 371) id "Bracelet-55-57")))
	(kind "wynncraft/recipe/get/Bracelet-57-59" code 200 timestamp 1610855794056 version "2.1.0" 
	 data ((level (57 59) type "BRACELET" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 8)) healthOrDamage (0 0) durability (371 378) id "Bracelet-57-59")))
	(kind "wynncraft/recipe/get/Chestplate-50-53" code 200 timestamp 1610855794241 version "2.1.0" 
	 data ((level (50 53) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Jungle Paper" amount 4)) healthOrDamage (550 630) durability (347 357) id "Chestplate-50-53")))
	(kind "wynncraft/recipe/get/Chestplate-53-55" code 200 timestamp 1610855794420 version "2.1.0" 
	 data ((level (53 55) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Jungle Paper" amount 4)) healthOrDamage (630 740) durability (357 364) id "Chestplate-53-55")))
	(kind "wynncraft/recipe/get/Chestplate-55-57" code 200 timestamp 1610855794587 version "2.1.0" 
	 data ((level (55 57) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Jungle Paper" amount 4)) healthOrDamage (740 850) durability (364 371) id "Chestplate-55-57")))
	(kind "wynncraft/recipe/get/Chestplate-57-59" code 200 timestamp 1610855794799 version "2.1.0" 
	 data ((level (57 59) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Jungle Paper" amount 4)) healthOrDamage (850 960) durability (371 378) id "Chestplate-57-59")))
	(kind "wynncraft/recipe/get/Dagger-50-53" code 200 timestamp 1610855795060 version "2.1.0" 
	 data ((level (50 53) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (89 97) durability (347 357) id "Dagger-50-53")))
	(kind "wynncraft/recipe/get/Dagger-53-55" code 200 timestamp 1610855795244 version "2.1.0" 
	 data ((level (53 55) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (97 106) durability (357 364) id "Dagger-53-55")))
	(kind "wynncraft/recipe/get/Dagger-55-57" code 200 timestamp 1610855795445 version "2.1.0" 
	 data ((level (55 57) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (106 115) durability (364 371) id "Dagger-55-57")))
	(kind "wynncraft/recipe/get/Food-50-53" code 200 timestamp 1610855795718 version "2.1.0" 
	 data ((level (50 53) type "FOOD" skill "COOKING" materials ((item "Refined Piranha Meat" amount 8) (item "Refined Rye Grains" amount 4)) healthOrDamage (837 997) duration (1962 2016) basicDuration (60 60) id "Food-50-53")))
	(kind "wynncraft/recipe/get/Food-53-55" code 200 timestamp 1610855795905 version "2.1.0" 
	 data ((level (53 55) type "FOOD" skill "COOKING" materials ((item "Refined Piranha Meat" amount 8) (item "Refined Rye Grains" amount 4)) healthOrDamage (997 1156) duration (2022 2052) basicDuration (60 60) id "Food-53-55")))
	(kind "wynncraft/recipe/get/Food-55-57" code 200 timestamp 1610855796123 version "2.1.0" 
	 data ((level (55 57) type "FOOD" skill "COOKING" materials ((item "Refined Piranha Meat" amount 8) (item "Refined Rye Grains" amount 4)) healthOrDamage (1156 1277) duration (2052 2088) basicDuration (60 60) id "Food-55-57")))
	(kind "wynncraft/recipe/get/Dagger-57-59" code 200 timestamp 1610855796312 version "2.1.0" 
	 data ((level (57 59) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (115 124) durability (371 378) id "Dagger-57-59")))
	(kind "wynncraft/recipe/get/Food-57-59" code 200 timestamp 1610855796535 version "2.1.0" 
	 data ((level (57 59) type "FOOD" skill "COOKING" materials ((item "Refined Piranha Meat" amount 8) (item "Refined Rye Grains" amount 4)) healthOrDamage (1277 1398) duration (2088 2124) basicDuration (60 60) id "Food-57-59")))
	(kind "wynncraft/recipe/get/Helmet-50-53" code 200 timestamp 1610855796718 version "2.1.0" 
	 data ((level (50 53) type "HELMET" skill "ARMOURING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Jungle Paper" amount 8)) healthOrDamage (550 630) durability (347 357) id "Helmet-50-53")))
	(kind "wynncraft/recipe/get/Helmet-53-55" code 200 timestamp 1610855796896 version "2.1.0" 
	 data ((level (53 55) type "HELMET" skill "ARMOURING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Jungle Paper" amount 8)) healthOrDamage (630 740) durability (357 364) id "Helmet-53-55")))
	(kind "wynncraft/recipe/get/Helmet-55-57" code 200 timestamp 1610855797078 version "2.1.0" 
	 data ((level (55 57) type "HELMET" skill "ARMOURING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Jungle Paper" amount 8)) healthOrDamage (740 850) durability (364 371) id "Helmet-55-57")))
	(kind "wynncraft/recipe/get/Helmet-57-59" code 200 timestamp 1610855797272 version "2.1.0" 
	 data ((level (57 59) type "HELMET" skill "ARMOURING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Jungle Paper" amount 8)) healthOrDamage (850 960) durability (371 378) id "Helmet-57-59")))
	(kind "wynncraft/recipe/get/Necklace-50-53" code 200 timestamp 1610855797452 version "2.1.0" 
	 data ((level (50 53) type "NECKLACE" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 12)) healthOrDamage (0 0) durability (347 357) id "Necklace-50-53")))
	(kind "wynncraft/recipe/get/Necklace-57-59" code 200 timestamp 1610855797638 version "2.1.0" 
	 data ((level (57 59) type "NECKLACE" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 12)) healthOrDamage (0 0) durability (371 378) id "Necklace-57-59")))
	(kind "wynncraft/recipe/get/Necklace-53-55" code 200 timestamp 1610855797837 version "2.1.0" 
	 data ((level (53 55) type "NECKLACE" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 12)) healthOrDamage (0 0) durability (357 364) id "Necklace-53-55")))
	(kind "wynncraft/recipe/get/Necklace-55-57" code 200 timestamp 1610855798067 version "2.1.0" 
	 data ((level (55 57) type "NECKLACE" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 12)) healthOrDamage (0 0) durability (364 371) id "Necklace-55-57")))
	(kind "wynncraft/recipe/get/Leggings-53-55" code 200 timestamp 1610855798237 version "2.1.0" 
	 data ((level (53 55) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Rye String" amount 4)) healthOrDamage (630 740) durability (357 364) id "Leggings-53-55")))
	(kind "wynncraft/recipe/get/Leggings-55-57" code 200 timestamp 1610855798431 version "2.1.0" 
	 data ((level (55 57) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Rye String" amount 4)) healthOrDamage (740 850) durability (364 371) id "Leggings-55-57")))
	(kind "wynncraft/recipe/get/Leggings-57-59" code 200 timestamp 1610855798652 version "2.1.0" 
	 data ((level (57 59) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Rye String" amount 4)) healthOrDamage (850 960) durability (371 378) id "Leggings-57-59")))
	(kind "wynncraft/recipe/get/Potion-53-55" code 200 timestamp 1610855798844 version "2.1.0" 
	 data ((level (53 55) type "POTION" skill "ALCHEMISM" materials ((item "Refined Piranha Oil" amount 4) (item "Refined Rye Grains" amount 8)) healthOrDamage (997 1156) duration (672 684) basicDuration (3 3) id "Potion-53-55")))
	(kind "wynncraft/recipe/get/Potion-50-53" code 200 timestamp 1610855799039 version "2.1.0" 
	 data ((level (50 53) type "POTION" skill "ALCHEMISM" materials ((item "Refined Piranha Oil" amount 4) (item "Refined Rye Grains" amount 8)) healthOrDamage (837 997) duration (654 672) basicDuration (3 3) id "Potion-50-53")))
	(kind "wynncraft/recipe/get/Potion-55-57" code 200 timestamp 1610855799312 version "2.1.0" 
	 data ((level (55 57) type "POTION" skill "ALCHEMISM" materials ((item "Refined Piranha Oil" amount 4) (item "Refined Rye Grains" amount 8)) healthOrDamage (1156 1277) duration (684 696) basicDuration (3 3) id "Potion-55-57")))
	(kind "wynncraft/recipe/get/Relik-50-53" code 200 timestamp 1610856921662 version "2.1.0" 
	 data ((level (50 53) type "RELIK" skill "WOODWORKING" materials ((item "Refined Piranha Oil" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (107 116) durability (347 357) id "Relik-50-53")))
	(kind "wynncraft/recipe/get/Leggings-50-53" code 200 timestamp 1610856921834 version "2.1.0" 
	 data ((level (50 53) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Silver Ingot" amount 8) (item "Refined Rye String" amount 4)) healthOrDamage (550 630) durability (347 357) id "Leggings-50-53")))
	(kind "wynncraft/recipe/get/Relik-53-55" code 200 timestamp 1610856922003 version "2.1.0" 
	 data ((level (53 55) type "RELIK" skill "WOODWORKING" materials ((item "Refined Piranha Oil" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (116 127) durability (357 364) id "Relik-53-55")))
	(kind "wynncraft/recipe/get/Potion-57-59" code 200 timestamp 1610856922195 version "2.1.0" 
	 data ((level (57 59) type "POTION" skill "ALCHEMISM" materials ((item "Refined Piranha Oil" amount 4) (item "Refined Rye Grains" amount 8)) healthOrDamage (1277 1398) duration (696 708) basicDuration (3 3) id "Potion-57-59")))
	(kind "wynncraft/recipe/get/Relik-55-57" code 200 timestamp 1610856922367 version "2.1.0" 
	 data ((level (55 57) type "RELIK" skill "WOODWORKING" materials ((item "Refined Piranha Oil" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (127 138) durability (364 371) id "Relik-55-57")))
	(kind "wynncraft/recipe/get/Relik-57-59" code 200 timestamp 1610856922566 version "2.1.0" 
	 data ((level (57 59) type "RELIK" skill "WOODWORKING" materials ((item "Refined Piranha Oil" amount 8) (item "Refined Jungle Wood" amount 4)) healthOrDamage (138 149) durability (371 378) id "Relik-57-59")))
	(kind "wynncraft/recipe/get/Ring-50-53" code 200 timestamp 1610856922745 version "2.1.0" 
	 data ((level (50 53) type "RING" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 4)) healthOrDamage (0 0) durability (347 357) id "Ring-50-53")))
	(kind "wynncraft/recipe/get/Ring-53-55" code 200 timestamp 1610856922948 version "2.1.0" 
	 data ((level (53 55) type "RING" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 4)) healthOrDamage (0 0) durability (357 364) id "Ring-53-55")))
	(kind "wynncraft/recipe/get/Ring-55-57" code 200 timestamp 1610856923132 version "2.1.0" 
	 data ((level (55 57) type "RING" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 4)) healthOrDamage (0 0) durability (364 371) id "Ring-55-57")))
	(kind "wynncraft/recipe/get/Ring-57-59" code 200 timestamp 1610856923324 version "2.1.0" 
	 data ((level (57 59) type "RING" skill "JEWELING" materials ((item "Refined Silver Gem" amount 4) (item "Refined Piranha Oil" amount 4)) healthOrDamage (0 0) durability (371 378) id "Ring-57-59")))
	(kind "wynncraft/recipe/get/Scroll-50-53" code 200 timestamp 1610856923521 version "2.1.0" 
	 data ((level (50 53) type "SCROLL" skill "SCRIBING" materials ((item "Refined Piranha Oil" amount 4) (item "Refined Jungle Paper" amount 4)) healthOrDamage (590 702) duration (654 672) basicDuration (3 3) id "Scroll-50-53")))
	(kind "wynncraft/recipe/get/Scroll-53-55" code 200 timestamp 1610856923702 version "2.1.0" 
	 data ((level (53 55) type "SCROLL" skill "SCRIBING" materials ((item "Refined Piranha Oil" amount 4) (item "Refined Jungle Paper" amount 4)) healthOrDamage (702 813) duration (672 684) basicDuration (3 3) id "Scroll-53-55")))
	(kind "wynncraft/recipe/get/Scroll-57-59" code 200 timestamp 1610856923904 version "2.1.0" 
	 data ((level (57 59) type "SCROLL" skill "SCRIBING" materials ((item "Refined Piranha Oil" amount 4) (item "Refined Jungle Paper" amount 4)) healthOrDamage (898 982) duration (696 708) basicDuration (3 3) id "Scroll-57-59")))
	(kind "wynncraft/recipe/get/Scroll-55-57" code 200 timestamp 1610856924097 version "2.1.0" 
	 data ((level (55 57) type "SCROLL" skill "SCRIBING" materials ((item "Refined Piranha Oil" amount 4) (item "Refined Jungle Paper" amount 4)) healthOrDamage (813 898) duration (684 696) basicDuration (3 3) id "Scroll-55-57")))
	(kind "wynncraft/recipe/get/Spear-50-53" code 200 timestamp 1610856924328 version "2.1.0" 
	 data ((level (50 53) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Jungle Wood" amount 8)) healthOrDamage (71 78) durability (347 357) id "Spear-50-53")))
	(kind "wynncraft/recipe/get/Spear-53-55" code 200 timestamp 1610856924510 version "2.1.0" 
	 data ((level (53 55) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Jungle Wood" amount 8)) healthOrDamage (78 85) durability (357 364) id "Spear-53-55")))
	(kind "wynncraft/recipe/get/Spear-55-57" code 200 timestamp 1610856924706 version "2.1.0" 
	 data ((level (55 57) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Jungle Wood" amount 8)) healthOrDamage (85 92) durability (364 371) id "Spear-55-57")))
	(kind "wynncraft/recipe/get/Wand-50-53" code 200 timestamp 1610856924939 version "2.1.0" 
	 data ((level (50 53) type "WAND" skill "WOODWORKING" materials ((item "Refined Rye String" amount 4) (item "Refined Jungle Wood" amount 8)) healthOrDamage (53 58) durability (347 357) id "Wand-50-53")))
	(kind "wynncraft/recipe/get/Spear-57-59" code 200 timestamp 1610856925128 version "2.1.0" 
	 data ((level (57 59) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Silver Ingot" amount 4) (item "Refined Jungle Wood" amount 8)) healthOrDamage (92 99) durability (371 378) id "Spear-57-59")))
	(kind "wynncraft/recipe/get/Wand-55-57" code 200 timestamp 1610856925344 version "2.1.0" 
	 data ((level (55 57) type "WAND" skill "WOODWORKING" materials ((item "Refined Rye String" amount 4) (item "Refined Jungle Wood" amount 8)) healthOrDamage (64 69) durability (364 371) id "Wand-55-57")))
	(kind "wynncraft/recipe/get/Wand-53-55" code 200 timestamp 1610856925533 version "2.1.0" 
	 data ((level (53 55) type "WAND" skill "WOODWORKING" materials ((item "Refined Rye String" amount 4) (item "Refined Jungle Wood" amount 8)) healthOrDamage (58 64) durability (357 364) id "Wand-53-55")))
	(kind "wynncraft/recipe/get/Wand-57-59" code 200 timestamp 1610856925744 version "2.1.0" 
	 data ((level (57 59) type "WAND" skill "WOODWORKING" materials ((item "Refined Rye String" amount 4) (item "Refined Jungle Wood" amount 8)) healthOrDamage (69 74) durability (371 378) id "Wand-57-59")))
	(kind "wynncraft/recipe/get/Boots-60-63" code 200 timestamp 1610856925939 version "2.1.0" 
	 data ((level (60 63) type "BOOTS" skill "TAILORING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Millet String" amount 8)) healthOrDamage (970 1070) durability (382 392) id "Boots-60-63")))
	(kind "wynncraft/recipe/get/Boots-65-67" code 200 timestamp 1610856926145 version "2.1.0" 
	 data ((level (65 67) type "BOOTS" skill "TAILORING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Millet String" amount 8)) healthOrDamage (1160 1260) durability (399 406) id "Boots-65-67")))
	(kind "wynncraft/recipe/get/Boots-63-65" code 200 timestamp 1610856926357 version "2.1.0" 
	 data ((level (63 65) type "BOOTS" skill "TAILORING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Millet String" amount 8)) healthOrDamage (1070 1160) durability (392 399) id "Boots-63-65")))
	(kind "wynncraft/recipe/get/Boots-67-69" code 200 timestamp 1610856926559 version "2.1.0" 
	 data ((level (67 69) type "BOOTS" skill "TAILORING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Millet String" amount 8)) healthOrDamage (1260 1350) durability (406 413) id "Boots-67-69")))
	(kind "wynncraft/recipe/get/Bow-60-63" code 200 timestamp 1610856926758 version "2.1.0" 
	 data ((level (60 63) type "BOW" skill "WOODWORKING" materials ((item "Refined Millet String" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (150 162) durability (382 392) id "Bow-60-63")))
	(kind "wynncraft/recipe/get/Bow-63-65" code 200 timestamp 1610856926958 version "2.1.0" 
	 data ((level (63 65) type "BOW" skill "WOODWORKING" materials ((item "Refined Millet String" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (162 174) durability (392 399) id "Bow-63-65")))
	(kind "wynncraft/recipe/get/Bow-67-69" code 200 timestamp 1610856927148 version "2.1.0" 
	 data ((level (67 69) type "BOW" skill "WOODWORKING" materials ((item "Refined Millet String" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (186 198) durability (406 413) id "Bow-67-69")))
	(kind "wynncraft/recipe/get/Bow-65-67" code 200 timestamp 1610856927346 version "2.1.0" 
	 data ((level (65 67) type "BOW" skill "WOODWORKING" materials ((item "Refined Millet String" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (174 186) durability (399 406) id "Bow-65-67")))
	(kind "wynncraft/recipe/get/Bracelet-63-65" code 200 timestamp 1610856927579 version "2.1.0" 
	 data ((level (63 65) type "BRACELET" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 8)) healthOrDamage (0 0) durability (392 399) id "Bracelet-63-65")))
	(kind "wynncraft/recipe/get/Bracelet-60-63" code 200 timestamp 1610856928778 version "2.1.0" 
	 data ((level (60 63) type "BRACELET" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 8)) healthOrDamage (0 0) durability (382 392) id "Bracelet-60-63")))
	(kind "wynncraft/recipe/get/Bracelet-65-67" code 200 timestamp 1610856928985 version "2.1.0" 
	 data ((level (65 67) type "BRACELET" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 8)) healthOrDamage (0 0) durability (399 406) id "Bracelet-65-67")))
	(kind "wynncraft/recipe/get/Bracelet-67-69" code 200 timestamp 1610856929185 version "2.1.0" 
	 data ((level (67 69) type "BRACELET" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 8)) healthOrDamage (0 0) durability (406 413) id "Bracelet-67-69")))
	(kind "wynncraft/recipe/get/Chestplate-63-65" code 200 timestamp 1610856929378 version "2.1.0" 
	 data ((level (63 65) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Dark Paper" amount 4)) healthOrDamage (1070 1160) durability (392 399) id "Chestplate-63-65")))
	(kind "wynncraft/recipe/get/Chestplate-60-63" code 200 timestamp 1610856929630 version "2.1.0" 
	 data ((level (60 63) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Dark Paper" amount 4)) healthOrDamage (970 1070) durability (382 392) id "Chestplate-60-63")))
	(kind "wynncraft/recipe/get/Chestplate-65-67" code 200 timestamp 1610856929884 version "2.1.0" 
	 data ((level (65 67) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Dark Paper" amount 4)) healthOrDamage (1160 1260) durability (399 406) id "Chestplate-65-67")))
	(kind "wynncraft/recipe/get/Chestplate-67-69" code 200 timestamp 1610856930167 version "2.1.0" 
	 data ((level (67 69) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Dark Paper" amount 4)) healthOrDamage (1260 1350) durability (406 413) id "Chestplate-67-69")))
	(kind "wynncraft/recipe/get/Dagger-60-63" code 200 timestamp 1610856930384 version "2.1.0" 
	 data ((level (60 63) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (125 135) durability (382 392) id "Dagger-60-63")))
	(kind "wynncraft/recipe/get/Dagger-63-65" code 200 timestamp 1610856930589 version "2.1.0" 
	 data ((level (63 65) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (135 145) durability (392 399) id "Dagger-63-65")))
	(kind "wynncraft/recipe/get/Dagger-65-67" code 200 timestamp 1610856930807 version "2.1.0" 
	 data ((level (65 67) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (145 155) durability (399 406) id "Dagger-65-67")))
	(kind "wynncraft/recipe/get/Dagger-67-69" code 200 timestamp 1610856931010 version "2.1.0" 
	 data ((level (67 69) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (155 165) durability (406 413) id "Dagger-67-69")))
	(kind "wynncraft/recipe/get/Food-60-63" code 200 timestamp 1610856931179 version "2.1.0" 
	 data ((level (60 63) type "FOOD" skill "COOKING" materials ((item "Refined Koi Meat" amount 8) (item "Refined Millet Grains" amount 4)) healthOrDamage (1420 1541) duration (2142 2196) basicDuration (60 60) id "Food-60-63")))
	(kind "wynncraft/recipe/get/Food-63-65" code 200 timestamp 1610856931399 version "2.1.0" 
	 data ((level (63 65) type "FOOD" skill "COOKING" materials ((item "Refined Koi Meat" amount 8) (item "Refined Millet Grains" amount 4)) healthOrDamage (1541 1662) duration (2196 2232) basicDuration (60 60) id "Food-63-65")))
	(kind "wynncraft/recipe/get/Food-67-69" code 200 timestamp 1610856931620 version "2.1.0" 
	 data ((level (67 69) type "FOOD" skill "COOKING" materials ((item "Refined Koi Meat" amount 8) (item "Refined Millet Grains" amount 4)) healthOrDamage (1816 1882) duration (2268 2304) basicDuration (60 60) id "Food-67-69")))
	(kind "wynncraft/recipe/get/Helmet-60-63" code 200 timestamp 1610856931812 version "2.1.0" 
	 data ((level (60 63) type "HELMET" skill "ARMOURING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Dark Paper" amount 8)) healthOrDamage (970 1070) durability (382 392) id "Helmet-60-63")))
	(kind "wynncraft/recipe/get/Helmet-63-65" code 200 timestamp 1610856932001 version "2.1.0" 
	 data ((level (63 65) type "HELMET" skill "ARMOURING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Dark Paper" amount 8)) healthOrDamage (1070 1160) durability (392 399) id "Helmet-63-65")))
	(kind "wynncraft/recipe/get/Food-65-67" code 200 timestamp 1610856932205 version "2.1.0" 
	 data ((level (65 67) type "FOOD" skill "COOKING" materials ((item "Refined Koi Meat" amount 8) (item "Refined Millet Grains" amount 4)) healthOrDamage (1662 1816) duration (2232 2268) basicDuration (60 60) id "Food-65-67")))
	(kind "wynncraft/recipe/get/Helmet-65-67" code 200 timestamp 1610856932382 version "2.1.0" 
	 data ((level (65 67) type "HELMET" skill "ARMOURING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Dark Paper" amount 8)) healthOrDamage (1160 1260) durability (399 406) id "Helmet-65-67")))
	(kind "wynncraft/recipe/get/Helmet-67-69" code 200 timestamp 1610856932560 version "2.1.0" 
	 data ((level (67 69) type "HELMET" skill "ARMOURING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Dark Paper" amount 8)) healthOrDamage (1260 1350) durability (406 413) id "Helmet-67-69")))
	(kind "wynncraft/recipe/get/Necklace-60-63" code 200 timestamp 1610856932734 version "2.1.0" 
	 data ((level (60 63) type "NECKLACE" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 12)) healthOrDamage (0 0) durability (382 392) id "Necklace-60-63")))
	(kind "wynncraft/recipe/get/Necklace-63-65" code 200 timestamp 1610856932923 version "2.1.0" 
	 data ((level (63 65) type "NECKLACE" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 12)) healthOrDamage (0 0) durability (392 399) id "Necklace-63-65")))
	(kind "wynncraft/recipe/get/Necklace-65-67" code 200 timestamp 1610856933122 version "2.1.0" 
	 data ((level (65 67) type "NECKLACE" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 12)) healthOrDamage (0 0) durability (399 406) id "Necklace-65-67")))
	(kind "wynncraft/recipe/get/Necklace-67-69" code 200 timestamp 1610856933296 version "2.1.0" 
	 data ((level (67 69) type "NECKLACE" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 12)) healthOrDamage (0 0) durability (406 413) id "Necklace-67-69")))
	(kind "wynncraft/recipe/get/Leggings-63-65" code 200 timestamp 1610856933491 version "2.1.0" 
	 data ((level (63 65) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Millet String" amount 4)) healthOrDamage (1070 1160) durability (392 399) id "Leggings-63-65")))
	(kind "wynncraft/recipe/get/Leggings-60-63" code 200 timestamp 1610856933735 version "2.1.0" 
	 data ((level (60 63) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Millet String" amount 4)) healthOrDamage (970 1070) durability (382 392) id "Leggings-60-63")))
	(kind "wynncraft/recipe/get/Leggings-65-67" code 200 timestamp 1610856933931 version "2.1.0" 
	 data ((level (65 67) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Millet String" amount 4)) healthOrDamage (1160 1260) durability (399 406) id "Leggings-65-67")))
	(kind "wynncraft/recipe/get/Leggings-67-69" code 200 timestamp 1610856934121 version "2.1.0" 
	 data ((level (67 69) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Cobalt Ingot" amount 8) (item "Refined Millet String" amount 4)) healthOrDamage (1260 1350) durability (406 413) id "Leggings-67-69")))
	(kind "wynncraft/recipe/get/Potion-60-63" code 200 timestamp 1610856934337 version "2.1.0" 
	 data ((level (60 63) type "POTION" skill "ALCHEMISM" materials ((item "Refined Koi Oil" amount 4) (item "Refined Millet Grains" amount 8)) healthOrDamage (1420 1541) duration (714 732) basicDuration (3 3) id "Potion-60-63")))
	(kind "wynncraft/recipe/get/Potion-63-65" code 200 timestamp 1610856934549 version "2.1.0" 
	 data ((level (63 65) type "POTION" skill "ALCHEMISM" materials ((item "Refined Koi Oil" amount 4) (item "Refined Millet Grains" amount 8)) healthOrDamage (1541 1662) duration (732 744) basicDuration (3 3) id "Potion-63-65")))
	(kind "wynncraft/recipe/get/Potion-67-69" code 200 timestamp 1610856934829 version "2.1.0" 
	 data ((level (67 69) type "POTION" skill "ALCHEMISM" materials ((item "Refined Koi Oil" amount 4) (item "Refined Millet Grains" amount 8)) healthOrDamage (1816 1970) duration (756 768) basicDuration (3 3) id "Potion-67-69")))
	(kind "wynncraft/recipe/get/Relik-63-65" code 200 timestamp 1610856935035 version "2.1.0" 
	 data ((level (63 65) type "RELIK" skill "WOODWORKING" materials ((item "Refined Koi Oil" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (162 174) durability (392 399) id "Relik-63-65")))
	(kind "wynncraft/recipe/get/Relik-65-67" code 200 timestamp 1610856935216 version "2.1.0" 
	 data ((level (65 67) type "RELIK" skill "WOODWORKING" materials ((item "Refined Koi Oil" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (174 186) durability (399 406) id "Relik-65-67")))
	(kind "wynncraft/recipe/get/Potion-65-67" code 200 timestamp 1610856935425 version "2.1.0" 
	 data ((level (65 67) type "POTION" skill "ALCHEMISM" materials ((item "Refined Koi Oil" amount 4) (item "Refined Millet Grains" amount 8)) healthOrDamage (1662 1816) duration (744 756) basicDuration (3 3) id "Potion-65-67")))
	(kind "wynncraft/recipe/get/Relik-67-69" code 200 timestamp 1610856935622 version "2.1.0" 
	 data ((level (67 69) type "RELIK" skill "WOODWORKING" materials ((item "Refined Koi Oil" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (186 198) durability (406 413) id "Relik-67-69")))
	(kind "wynncraft/recipe/get/Relik-60-63" code 200 timestamp 1610856935809 version "2.1.0" 
	 data ((level (60 63) type "RELIK" skill "WOODWORKING" materials ((item "Refined Koi Oil" amount 8) (item "Refined Dark Wood" amount 4)) healthOrDamage (150 162) durability (382 392) id "Relik-60-63")))
	(kind "wynncraft/recipe/get/Ring-60-63" code 200 timestamp 1610856936014 version "2.1.0" 
	 data ((level (60 63) type "RING" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 4)) healthOrDamage (0 0) durability (382 392) id "Ring-60-63")))
	(kind "wynncraft/recipe/get/Ring-65-67" code 200 timestamp 1610856936206 version "2.1.0" 
	 data ((level (65 67) type "RING" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 4)) healthOrDamage (0 0) durability (399 406) id "Ring-65-67")))
	(kind "wynncraft/recipe/get/Ring-63-65" code 200 timestamp 1610856936393 version "2.1.0" 
	 data ((level (63 65) type "RING" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 4)) healthOrDamage (0 0) durability (392 399) id "Ring-63-65")))
	(kind "wynncraft/recipe/get/Ring-67-69" code 200 timestamp 1610856936608 version "2.1.0" 
	 data ((level (67 69) type "RING" skill "JEWELING" materials ((item "Refined Cobalt Gem" amount 4) (item "Refined Koi Oil" amount 4)) healthOrDamage (0 0) durability (406 413) id "Ring-67-69")))
	(kind "wynncraft/recipe/get/Scroll-60-63" code 200 timestamp 1610856936797 version "2.1.0" 
	 data ((level (60 63) type "SCROLL" skill "SCRIBING" materials ((item "Refined Koi Oil" amount 4) (item "Refined Dark Paper" amount 4)) healthOrDamage (998 1082) duration (714 732) basicDuration (3 3) id "Scroll-60-63")))
	(kind "wynncraft/recipe/get/Scroll-63-65" code 200 timestamp 1610856937022 version "2.1.0" 
	 data ((level (63 65) type "SCROLL" skill "SCRIBING" materials ((item "Refined Koi Oil" amount 4) (item "Refined Dark Paper" amount 4)) healthOrDamage (1082 1167) duration (732 744) basicDuration (3 3) id "Scroll-63-65")))
	(kind "wynncraft/recipe/get/Scroll-67-69" code 200 timestamp 1610856937206 version "2.1.0" 
	 data ((level (67 69) type "SCROLL" skill "SCRIBING" materials ((item "Refined Koi Oil" amount 4) (item "Refined Dark Paper" amount 4)) healthOrDamage (1275 1383) duration (756 768) basicDuration (3 3) id "Scroll-67-69")))
	(kind "wynncraft/recipe/get/Scroll-65-67" code 200 timestamp 1610856937403 version "2.1.0" 
	 data ((level (65 67) type "SCROLL" skill "SCRIBING" materials ((item "Refined Koi Oil" amount 4) (item "Refined Dark Paper" amount 4)) healthOrDamage (1167 1275) duration (744 756) basicDuration (3 3) id "Scroll-65-67")))
	(kind "wynncraft/recipe/get/Spear-63-65" code 200 timestamp 1610856937619 version "2.1.0" 
	 data ((level (63 65) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Dark Wood" amount 8)) healthOrDamage (108 116) durability (392 399) id "Spear-63-65")))
	(kind "wynncraft/recipe/get/Spear-60-63" code 200 timestamp 1610856937804 version "2.1.0" 
	 data ((level (60 63) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Dark Wood" amount 8)) healthOrDamage (100 108) durability (382 392) id "Spear-60-63")))
	(kind "wynncraft/recipe/get/Spear-67-69" code 200 timestamp 1610856938000 version "2.1.0" 
	 data ((level (67 69) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Dark Wood" amount 8)) healthOrDamage (124 132) durability (406 413) id "Spear-67-69")))
	(kind "wynncraft/recipe/get/Wand-60-63" code 200 timestamp 1610856938193 version "2.1.0" 
	 data ((level (60 63) type "WAND" skill "WOODWORKING" materials ((item "Refined Millet String" amount 4) (item "Refined Dark Wood" amount 8)) healthOrDamage (75 81) durability (382 392) id "Wand-60-63")))
	(kind "wynncraft/recipe/get/Wand-63-65" code 200 timestamp 1610858056446 version "2.1.0" 
	 data ((level (63 65) type "WAND" skill "WOODWORKING" materials ((item "Refined Millet String" amount 4) (item "Refined Dark Wood" amount 8)) healthOrDamage (81 87) durability (392 399) id "Wand-63-65")))
	(kind "wynncraft/recipe/get/Spear-65-67" code 200 timestamp 1610858056852 version "2.1.0" 
	 data ((level (65 67) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Cobalt Ingot" amount 4) (item "Refined Dark Wood" amount 8)) healthOrDamage (116 124) durability (399 406) id "Spear-65-67")))
	(kind "wynncraft/recipe/get/Wand-65-67" code 200 timestamp 1610858057291 version "2.1.0" 
	 data ((level (65 67) type "WAND" skill "WOODWORKING" materials ((item "Refined Millet String" amount 4) (item "Refined Dark Wood" amount 8)) healthOrDamage (87 93) durability (399 406) id "Wand-65-67")))
	(kind "wynncraft/recipe/get/Wand-67-69" code 200 timestamp 1610858057777 version "2.1.0" 
	 data ((level (67 69) type "WAND" skill "WOODWORKING" materials ((item "Refined Millet String" amount 4) (item "Refined Dark Wood" amount 8)) healthOrDamage (93 99) durability (406 413) id "Wand-67-69")))
	(kind "wynncraft/recipe/get/Boots-70-73" code 200 timestamp 1610858058218 version "2.1.0" 
	 data ((level (70 73) type "BOOTS" skill "TAILORING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Decay String" amount 10)) healthOrDamage (1360 1480) durability (417 427) id "Boots-70-73")))
	(kind "wynncraft/recipe/get/Boots-73-75" code 200 timestamp 1610858058683 version "2.1.0" 
	 data ((level (73 75) type "BOOTS" skill "TAILORING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Decay String" amount 10)) healthOrDamage (1480 1600) durability (427 434) id "Boots-73-75")))
	(kind "wynncraft/recipe/get/Boots-77-79" code 200 timestamp 1610858059110 version "2.1.0" 
	 data ((level (77 79) type "BOOTS" skill "TAILORING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Decay String" amount 10)) healthOrDamage (1730 1850) durability (441 448) id "Boots-77-79")))
	(kind "wynncraft/recipe/get/Boots-75-77" code 200 timestamp 1610858059588 version "2.1.0" 
	 data ((level (75 77) type "BOOTS" skill "TAILORING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Decay String" amount 10)) healthOrDamage (1600 1730) durability (434 441) id "Boots-75-77")))
	(kind "wynncraft/recipe/get/Bow-75-77" code 200 timestamp 1610858059996 version "2.1.0" 
	 data ((level (75 77) type "BOW" skill "WOODWORKING" materials ((item "Refined Decay String" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (224 238) durability (434 441) id "Bow-75-77")))
	(kind "wynncraft/recipe/get/Bow-73-75" code 200 timestamp 1610858060381 version "2.1.0" 
	 data ((level (73 75) type "BOW" skill "WOODWORKING" materials ((item "Refined Decay String" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (212 224) durability (427 434) id "Bow-73-75")))
	(kind "wynncraft/recipe/get/Bracelet-70-73" code 200 timestamp 1610858060781 version "2.1.0" 
	 data ((level (70 73) type "BRACELET" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 10)) healthOrDamage (0 0) durability (417 427) id "Bracelet-70-73")))
	(kind "wynncraft/recipe/get/Bow-77-79" code 200 timestamp 1610858061187 version "2.1.0" 
	 data ((level (77 79) type "BOW" skill "WOODWORKING" materials ((item "Refined Decay String" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (238 250) durability (441 448) id "Bow-77-79")))
	(kind "wynncraft/recipe/get/Bracelet-73-75" code 200 timestamp 1610858061627 version "2.1.0" 
	 data ((level (73 75) type "BRACELET" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 10)) healthOrDamage (0 0) durability (427 434) id "Bracelet-73-75")))
	(kind "wynncraft/recipe/get/Bow-70-73" code 200 timestamp 1610858062051 version "2.1.0" 
	 data ((level (70 73) type "BOW" skill "WOODWORKING" materials ((item "Refined Decay String" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (200 212) durability (417 427) id "Bow-70-73")))
	(kind "wynncraft/recipe/get/Bracelet-75-77" code 200 timestamp 1610858062507 version "2.1.0" 
	 data ((level (75 77) type "BRACELET" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 10)) healthOrDamage (0 0) durability (434 441) id "Bracelet-75-77")))
	(kind "wynncraft/recipe/get/Chestplate-70-73" code 200 timestamp 1610858063013 version "2.1.0" 
	 data ((level (70 73) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Light Paper" amount 5)) healthOrDamage (1360 1480) durability (417 427) id "Chestplate-70-73")))
	(kind "wynncraft/recipe/get/Bracelet-77-79" code 200 timestamp 1610858063542 version "2.1.0" 
	 data ((level (77 79) type "BRACELET" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 10)) healthOrDamage (0 0) durability (441 448) id "Bracelet-77-79")))
	(kind "wynncraft/recipe/get/Chestplate-73-75" code 200 timestamp 1610858063926 version "2.1.0" 
	 data ((level (73 75) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Light Paper" amount 5)) healthOrDamage (1480 1600) durability (427 434) id "Chestplate-73-75")))
	(kind "wynncraft/recipe/get/Chestplate-75-77" code 200 timestamp 1610858064346 version "2.1.0" 
	 data ((level (75 77) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Light Paper" amount 5)) healthOrDamage (1600 1730) durability (434 441) id "Chestplate-75-77")))
	(kind "wynncraft/recipe/get/Chestplate-77-79" code 200 timestamp 1610858064776 version "2.1.0" 
	 data ((level (77 79) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Light Paper" amount 5)) healthOrDamage (1730 1850) durability (441 448) id "Chestplate-77-79")))
	(kind "wynncraft/recipe/get/Dagger-70-73" code 200 timestamp 1610858065210 version "2.1.0" 
	 data ((level (70 73) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (167 177) durability (417 427) id "Dagger-70-73")))
	(kind "wynncraft/recipe/get/Dagger-73-75" code 200 timestamp 1610858065642 version "2.1.0" 
	 data ((level (73 75) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (177 187) durability (427 434) id "Dagger-73-75")))
	(kind "wynncraft/recipe/get/Dagger-75-77" code 200 timestamp 1610858066104 version "2.1.0" 
	 data ((level (75 77) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (187 198) durability (434 441) id "Dagger-75-77")))
	(kind "wynncraft/recipe/get/Food-70-73" code 200 timestamp 1610858066491 version "2.1.0" 
	 data ((level (70 73) type "FOOD" skill "COOKING" materials ((item "Refined Gylia Meat" amount 10) (item "Refined Decay Grains" amount 5)) healthOrDamage (1992 2212) duration (2322 2376) basicDuration (60 60) id "Food-70-73")))
	(kind "wynncraft/recipe/get/Dagger-77-79" code 200 timestamp 1610858066922 version "2.1.0" 
	 data ((level (77 79) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (198 208) durability (441 448) id "Dagger-77-79")))
	(kind "wynncraft/recipe/get/Food-73-75" code 200 timestamp 1610858067399 version "2.1.0" 
	 data ((level (73 75) type "FOOD" skill "COOKING" materials ((item "Refined Gylia Meat" amount 10) (item "Refined Decay Grains" amount 5)) healthOrDamage (2212 2432) duration (2376 2412) basicDuration (60 60) id "Food-73-75")))
	(kind "wynncraft/recipe/get/Food-75-77" code 200 timestamp 1610858067870 version "2.1.0" 
	 data ((level (75 77) type "FOOD" skill "COOKING" materials ((item "Refined Gylia Meat" amount 10) (item "Refined Decay Grains" amount 5)) healthOrDamage (2432 2575) duration (2412 2448) basicDuration (60 60) id "Food-75-77")))
	(kind "wynncraft/recipe/get/Food-77-79" code 200 timestamp 1610858068394 version "2.1.0" 
	 data ((level (77 79) type "FOOD" skill "COOKING" materials ((item "Refined Gylia Meat" amount 10) (item "Refined Decay Grains" amount 5)) healthOrDamage (2575 2718) duration (2448 2484) basicDuration (60 60) id "Food-77-79")))
	(kind "wynncraft/recipe/get/Helmet-70-73" code 200 timestamp 1610858068940 version "2.1.0" 
	 data ((level (70 73) type "HELMET" skill "ARMOURING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Light Paper" amount 10)) healthOrDamage (1360 1480) durability (417 427) id "Helmet-70-73")))
	(kind "wynncraft/recipe/get/Helmet-73-75" code 200 timestamp 1610858069431 version "2.1.0" 
	 data ((level (73 75) type "HELMET" skill "ARMOURING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Light Paper" amount 10)) healthOrDamage (1480 1600) durability (427 434) id "Helmet-73-75")))
	(kind "wynncraft/recipe/get/Helmet-75-77" code 200 timestamp 1610858069864 version "2.1.0" 
	 data ((level (75 77) type "HELMET" skill "ARMOURING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Light Paper" amount 10)) healthOrDamage (1600 1730) durability (434 441) id "Helmet-75-77")))
	(kind "wynncraft/recipe/get/Helmet-77-79" code 200 timestamp 1610858071295 version "2.1.0" 
	 data ((level (77 79) type "HELMET" skill "ARMOURING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Light Paper" amount 10)) healthOrDamage (1730 1850) durability (441 448) id "Helmet-77-79")))
	(kind "wynncraft/recipe/get/Necklace-70-73" code 200 timestamp 1610858071738 version "2.1.0" 
	 data ((level (70 73) type "NECKLACE" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 15)) healthOrDamage (0 0) durability (417 427) id "Necklace-70-73")))
	(kind "wynncraft/recipe/get/Necklace-73-75" code 200 timestamp 1610858072153 version "2.1.0" 
	 data ((level (73 75) type "NECKLACE" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 15)) healthOrDamage (0 0) durability (427 434) id "Necklace-73-75")))
	(kind "wynncraft/recipe/get/Necklace-75-77" code 200 timestamp 1610858072582 version "2.1.0" 
	 data ((level (75 77) type "NECKLACE" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 15)) healthOrDamage (0 0) durability (434 441) id "Necklace-75-77")))
	(kind "wynncraft/recipe/get/Necklace-77-79" code 200 timestamp 1610858073030 version "2.1.0" 
	 data ((level (77 79) type "NECKLACE" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 15)) healthOrDamage (0 0) durability (441 448) id "Necklace-77-79")))
	(kind "wynncraft/recipe/get/Leggings-73-75" code 200 timestamp 1610858073516 version "2.1.0" 
	 data ((level (73 75) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Decay String" amount 5)) healthOrDamage (1480 1600) durability (427 434) id "Leggings-73-75")))
	(kind "wynncraft/recipe/get/Leggings-75-77" code 200 timestamp 1610858073942 version "2.1.0" 
	 data ((level (75 77) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Decay String" amount 5)) healthOrDamage (1600 1730) durability (434 441) id "Leggings-75-77")))
	(kind "wynncraft/recipe/get/Leggings-77-79" code 200 timestamp 1610858074361 version "2.1.0" 
	 data ((level (77 79) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Decay String" amount 5)) healthOrDamage (1730 1850) durability (441 448) id "Leggings-77-79")))
	(kind "wynncraft/recipe/get/Leggings-70-73" code 200 timestamp 1610858074808 version "2.1.0" 
	 data ((level (70 73) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Kanderstone Ingot" amount 10) (item "Refined Decay String" amount 5)) healthOrDamage (1360 1480) durability (417 427) id "Leggings-70-73")))
	(kind "wynncraft/recipe/get/Potion-70-73" code 200 timestamp 1610858075207 version "2.1.0" 
	 data ((level (70 73) type "POTION" skill "ALCHEMISM" materials ((item "Refined Gylia Oil" amount 5) (item "Refined Decay Grains" amount 10)) healthOrDamage (1992 2212) duration (774 792) basicDuration (3 3) id "Potion-70-73")))
	(kind "wynncraft/recipe/get/Potion-73-75" code 200 timestamp 1610858075639 version "2.1.0" 
	 data ((level (73 75) type "POTION" skill "ALCHEMISM" materials ((item "Refined Gylia Oil" amount 5) (item "Refined Decay Grains" amount 10)) healthOrDamage (2212 2432) duration (792 804) basicDuration (3 3) id "Potion-73-75")))
	(kind "wynncraft/recipe/get/Potion-75-77" code 200 timestamp 1610858076052 version "2.1.0" 
	 data ((level (75 77) type "POTION" skill "ALCHEMISM" materials ((item "Refined Gylia Oil" amount 5) (item "Refined Decay Grains" amount 10)) healthOrDamage (2432 2575) duration (804 816) basicDuration (3 3) id "Potion-75-77")))
	(kind "wynncraft/recipe/get/Potion-77-79" code 200 timestamp 1610858076455 version "2.1.0" 
	 data ((level (77 79) type "POTION" skill "ALCHEMISM" materials ((item "Refined Gylia Oil" amount 5) (item "Refined Decay Grains" amount 10)) healthOrDamage (2575 2718) duration (816 828) basicDuration (3 3) id "Potion-77-79")))
	(kind "wynncraft/recipe/get/Relik-70-73" code 200 timestamp 1610858076873 version "2.1.0" 
	 data ((level (70 73) type "RELIK" skill "WOODWORKING" materials ((item "Refined Gylia Oil" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (200 212) durability (417 427) id "Relik-70-73")))
	(kind "wynncraft/recipe/get/Relik-73-75" code 200 timestamp 1610858077285 version "2.1.0" 
	 data ((level (73 75) type "RELIK" skill "WOODWORKING" materials ((item "Refined Gylia Oil" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (212 224) durability (427 434) id "Relik-73-75")))
	(kind "wynncraft/recipe/get/Relik-75-77" code 200 timestamp 1610858077737 version "2.1.0" 
	 data ((level (75 77) type "RELIK" skill "WOODWORKING" materials ((item "Refined Gylia Oil" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (224 238) durability (434 441) id "Relik-75-77")))
	(kind "wynncraft/recipe/get/Relik-77-79" code 200 timestamp 1610858078153 version "2.1.0" 
	 data ((level (77 79) type "RELIK" skill "WOODWORKING" materials ((item "Refined Gylia Oil" amount 10) (item "Refined Light Wood" amount 5)) healthOrDamage (238 250) durability (441 448) id "Relik-77-79")))
	(kind "wynncraft/recipe/get/Ring-70-73" code 200 timestamp 1610858078578 version "2.1.0" 
	 data ((level (70 73) type "RING" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 5)) healthOrDamage (0 0) durability (417 427) id "Ring-70-73")))
	(kind "wynncraft/recipe/get/Ring-73-75" code 200 timestamp 1610858078979 version "2.1.0" 
	 data ((level (73 75) type "RING" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 5)) healthOrDamage (0 0) durability (427 434) id "Ring-73-75")))
	(kind "wynncraft/recipe/get/Ring-75-77" code 200 timestamp 1610858079394 version "2.1.0" 
	 data ((level (75 77) type "RING" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 5)) healthOrDamage (0 0) durability (434 441) id "Ring-75-77")))
	(kind "wynncraft/recipe/get/Ring-77-79" code 200 timestamp 1610858079878 version "2.1.0" 
	 data ((level (77 79) type "RING" skill "JEWELING" materials ((item "Refined Kanderstone Gem" amount 5) (item "Refined Gylia Oil" amount 5)) healthOrDamage (0 0) durability (441 448) id "Ring-77-79")))
	(kind "wynncraft/recipe/get/Scroll-70-73" code 200 timestamp 1610858080353 version "2.1.0" 
	 data ((level (70 73) type "SCROLL" skill "SCRIBING" materials ((item "Refined Gylia Oil" amount 5) (item "Refined Light Paper" amount 5)) healthOrDamage (1398 1552) duration (774 792) basicDuration (3 3) id "Scroll-70-73")))
	(kind "wynncraft/recipe/get/Scroll-73-75" code 200 timestamp 1610858080767 version "2.1.0" 
	 data ((level (73 75) type "SCROLL" skill "SCRIBING" materials ((item "Refined Gylia Oil" amount 5) (item "Refined Light Paper" amount 5)) healthOrDamage (1552 1706) duration (792 804) basicDuration (3 3) id "Scroll-73-75")))
	(kind "wynncraft/recipe/get/Scroll-75-77" code 200 timestamp 1610858081202 version "2.1.0" 
	 data ((level (75 77) type "SCROLL" skill "SCRIBING" materials ((item "Refined Gylia Oil" amount 5) (item "Refined Light Paper" amount 5)) healthOrDamage (1706 1806) duration (804 816) basicDuration (3 3) id "Scroll-75-77")))
	(kind "wynncraft/recipe/get/Scroll-77-79" code 200 timestamp 1610858081633 version "2.1.0" 
	 data ((level (77 79) type "SCROLL" skill "SCRIBING" materials ((item "Refined Gylia Oil" amount 5) (item "Refined Light Paper" amount 5)) healthOrDamage (1806 1906) duration (816 828) basicDuration (3 3) id "Scroll-77-79")))
	(kind "wynncraft/recipe/get/Spear-70-73" code 200 timestamp 1610858082044 version "2.1.0" 
	 data ((level (70 73) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Light Wood" amount 10)) healthOrDamage (134 142) durability (417 427) id "Spear-70-73")))
	(kind "wynncraft/recipe/get/Spear-73-75" code 200 timestamp 1610858082463 version "2.1.0" 
	 data ((level (73 75) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Light Wood" amount 10)) healthOrDamage (142 150) durability (427 434) id "Spear-73-75")))
	(kind "wynncraft/recipe/get/Spear-75-77" code 200 timestamp 1610858082920 version "2.1.0" 
	 data ((level (75 77) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Light Wood" amount 10)) healthOrDamage (150 158) durability (434 441) id "Spear-75-77")))
	(kind "wynncraft/recipe/get/Spear-77-79" code 200 timestamp 1610855820000 version "2.1.0" 
	 data ((level (77 79) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Kanderstone Ingot" amount 5) (item "Refined Light Wood" amount 10)) healthOrDamage (158 166) durability (441 448) id "Spear-77-79")))
	(kind "wynncraft/recipe/get/Wand-70-73" code 200 timestamp 1610855820194 version "2.1.0" 
	 data ((level (70 73) type "WAND" skill "WOODWORKING" materials ((item "Refined Decay String" amount 5) (item "Refined Light Wood" amount 10)) healthOrDamage (100 106) durability (417 427) id "Wand-70-73")))
	(kind "wynncraft/recipe/get/Wand-73-75" code 200 timestamp 1610855820381 version "2.1.0" 
	 data ((level (73 75) type "WAND" skill "WOODWORKING" materials ((item "Refined Decay String" amount 5) (item "Refined Light Wood" amount 10)) healthOrDamage (106 112) durability (427 434) id "Wand-73-75")))
	(kind "wynncraft/recipe/get/Wand-75-77" code 200 timestamp 1610855820584 version "2.1.0" 
	 data ((level (75 77) type "WAND" skill "WOODWORKING" materials ((item "Refined Decay String" amount 5) (item "Refined Light Wood" amount 10)) healthOrDamage (112 119) durability (434 441) id "Wand-75-77")))
	(kind "wynncraft/recipe/get/Wand-77-79" code 200 timestamp 1610855820768 version "2.1.0" 
	 data ((level (77 79) type "WAND" skill "WOODWORKING" materials ((item "Refined Decay String" amount 5) (item "Refined Light Wood" amount 10)) healthOrDamage (119 125) durability (441 448) id "Wand-77-79")))
	(kind "wynncraft/recipe/get/Boots-80-83" code 200 timestamp 1610855820957 version "2.1.0" 
	 data ((level (80 83) type "BOOTS" skill "TAILORING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Rice String" amount 10)) healthOrDamage (1870 2010) durability (452 462) id "Boots-80-83")))
	(kind "wynncraft/recipe/get/Boots-83-85" code 200 timestamp 1610855821144 version "2.1.0" 
	 data ((level (83 85) type "BOOTS" skill "TAILORING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Rice String" amount 10)) healthOrDamage (2010 2160) durability (462 469) id "Boots-83-85")))
	(kind "wynncraft/recipe/get/Boots-85-87" code 200 timestamp 1610855821392 version "2.1.0" 
	 data ((level (85 87) type "BOOTS" skill "TAILORING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Rice String" amount 10)) healthOrDamage (2160 2320) durability (469 476) id "Boots-85-87")))
	(kind "wynncraft/recipe/get/Boots-87-89" code 200 timestamp 1610855821595 version "2.1.0" 
	 data ((level (87 89) type "BOOTS" skill "TAILORING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Rice String" amount 10)) healthOrDamage (2320 2460) durability (476 483) id "Boots-87-89")))
	(kind "wynncraft/recipe/get/Bow-83-85" code 200 timestamp 1610855821840 version "2.1.0" 
	 data ((level (83 85) type "BOW" skill "WOODWORKING" materials ((item "Refined Rice String" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (268 282) durability (462 469) id "Bow-83-85")))
	(kind "wynncraft/recipe/get/Bow-85-87" code 200 timestamp 1610855822072 version "2.1.0" 
	 data ((level (85 87) type "BOW" skill "WOODWORKING" materials ((item "Refined Rice String" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (282 296) durability (469 476) id "Bow-85-87")))
	(kind "wynncraft/recipe/get/Bow-80-83" code 200 timestamp 1610855822284 version "2.1.0" 
	 data ((level (80 83) type "BOW" skill "WOODWORKING" materials ((item "Refined Rice String" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (253 268) durability (452 462) id "Bow-80-83")))
	(kind "wynncraft/recipe/get/Bracelet-83-85" code 200 timestamp 1610855822562 version "2.1.0" 
	 data ((level (83 85) type "BRACELET" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 10)) healthOrDamage (0 0) durability (462 469) id "Bracelet-83-85")))
	(kind "wynncraft/recipe/get/Bracelet-80-83" code 200 timestamp 1610855822755 version "2.1.0" 
	 data ((level (80 83) type "BRACELET" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 10)) healthOrDamage (0 0) durability (452 462) id "Bracelet-80-83")))
	(kind "wynncraft/recipe/get/Bracelet-85-87" code 200 timestamp 1610855822971 version "2.1.0" 
	 data ((level (85 87) type "BRACELET" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 10)) healthOrDamage (0 0) durability (469 476) id "Bracelet-85-87")))
	(kind "wynncraft/recipe/get/Bow-87-89" code 200 timestamp 1610855823188 version "2.1.0" 
	 data ((level (87 89) type "BOW" skill "WOODWORKING" materials ((item "Refined Rice String" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (296 311) durability (476 483) id "Bow-87-89")))
	(kind "wynncraft/recipe/get/Bracelet-87-89" code 200 timestamp 1610855823395 version "2.1.0" 
	 data ((level (87 89) type "BRACELET" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 10)) healthOrDamage (0 0) durability (476 483) id "Bracelet-87-89")))
	(kind "wynncraft/recipe/get/Chestplate-80-83" code 200 timestamp 1610855823633 version "2.1.0" 
	 data ((level (80 83) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Pine Paper" amount 5)) healthOrDamage (1870 2010) durability (452 462) id "Chestplate-80-83")))
	(kind "wynncraft/recipe/get/Chestplate-83-85" code 200 timestamp 1610855823934 version "2.1.0" 
	 data ((level (83 85) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Pine Paper" amount 5)) healthOrDamage (2010 2160) durability (462 469) id "Chestplate-83-85")))
	(kind "wynncraft/recipe/get/Chestplate-85-87" code 200 timestamp 1610855824137 version "2.1.0" 
	 data ((level (85 87) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Pine Paper" amount 5)) healthOrDamage (2160 2320) durability (469 476) id "Chestplate-85-87")))
	(kind "wynncraft/recipe/get/Chestplate-87-89" code 200 timestamp 1610855824326 version "2.1.0" 
	 data ((level (87 89) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Pine Paper" amount 5)) healthOrDamage (2320 2460) durability (476 483) id "Chestplate-87-89")))
	(kind "wynncraft/recipe/get/Dagger-83-85" code 200 timestamp 1610855824520 version "2.1.0" 
	 data ((level (83 85) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (223 235) durability (462 469) id "Dagger-83-85")))
	(kind "wynncraft/recipe/get/Dagger-80-83" code 200 timestamp 1610855824716 version "2.1.0" 
	 data ((level (80 83) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (211 223) durability (452 462) id "Dagger-80-83")))
	(kind "wynncraft/recipe/get/Dagger-85-87" code 200 timestamp 1610855824898 version "2.1.0" 
	 data ((level (85 87) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (235 247) durability (469 476) id "Dagger-85-87")))
	(kind "wynncraft/recipe/get/Dagger-87-89" code 200 timestamp 1610855825090 version "2.1.0" 
	 data ((level (87 89) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (247 259) durability (476 483) id "Dagger-87-89")))
	(kind "wynncraft/recipe/get/Food-83-85" code 200 timestamp 1610855825302 version "2.1.0" 
	 data ((level (83 85) type "FOOD" skill "COOKING" materials ((item "Refined Bass Meat" amount 10) (item "Refined Rice Grains" amount 5)) healthOrDamage (2927 3092) duration (2556 2592) basicDuration (60 60) id "Food-83-85")))
	(kind "wynncraft/recipe/get/Food-80-83" code 200 timestamp 1610855825524 version "2.1.0" 
	 data ((level (80 83) type "FOOD" skill "COOKING" materials ((item "Refined Bass Meat" amount 10) (item "Refined Rice Grains" amount 5)) healthOrDamage (2762 2927) duration (2502 2556) basicDuration (60 60) id "Food-80-83")))
	(kind "wynncraft/recipe/get/Food-85-87" code 200 timestamp 1610855825737 version "2.1.0" 
	 data ((level (85 87) type "FOOD" skill "COOKING" materials ((item "Refined Bass Meat" amount 10) (item "Refined Rice Grains" amount 5)) healthOrDamage (3092 3323) duration (2592 2628) basicDuration (60 60) id "Food-85-87")))
	(kind "wynncraft/recipe/get/Food-87-89" code 200 timestamp 1610855825950 version "2.1.0" 
	 data ((level (87 89) type "FOOD" skill "COOKING" materials ((item "Refined Bass Meat" amount 10) (item "Refined Rice Grains" amount 5)) healthOrDamage (3323 3554) duration (2628 2664) basicDuration (60 60) id "Food-87-89")))
	(kind "wynncraft/recipe/get/Helmet-80-83" code 200 timestamp 1610855826136 version "2.1.0" 
	 data ((level (80 83) type "HELMET" skill "ARMOURING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Pine Paper" amount 10)) healthOrDamage (1870 2010) durability (452 462) id "Helmet-80-83")))
	(kind "wynncraft/recipe/get/Helmet-83-85" code 200 timestamp 1610855826367 version "2.1.0" 
	 data ((level (83 85) type "HELMET" skill "ARMOURING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Pine Paper" amount 10)) healthOrDamage (2010 2160) durability (462 469) id "Helmet-83-85")))
	(kind "wynncraft/recipe/get/Helmet-85-87" code 200 timestamp 1610855826568 version "2.1.0" 
	 data ((level (85 87) type "HELMET" skill "ARMOURING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Pine Paper" amount 10)) healthOrDamage (2160 2320) durability (469 476) id "Helmet-85-87")))
	(kind "wynncraft/recipe/get/Helmet-87-89" code 200 timestamp 1610855826754 version "2.1.0" 
	 data ((level (87 89) type "HELMET" skill "ARMOURING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Pine Paper" amount 10)) healthOrDamage (2320 2460) durability (476 483) id "Helmet-87-89")))
	(kind "wynncraft/recipe/get/Necklace-80-83" code 200 timestamp 1610855826978 version "2.1.0" 
	 data ((level (80 83) type "NECKLACE" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 15)) healthOrDamage (0 0) durability (452 462) id "Necklace-80-83")))
	(kind "wynncraft/recipe/get/Necklace-83-85" code 200 timestamp 1610855827157 version "2.1.0" 
	 data ((level (83 85) type "NECKLACE" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 15)) healthOrDamage (0 0) durability (462 469) id "Necklace-83-85")))
	(kind "wynncraft/recipe/get/Necklace-85-87" code 200 timestamp 1610855827372 version "2.1.0" 
	 data ((level (85 87) type "NECKLACE" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 15)) healthOrDamage (0 0) durability (469 476) id "Necklace-85-87")))
	(kind "wynncraft/recipe/get/Necklace-87-89" code 200 timestamp 1610855827551 version "2.1.0" 
	 data ((level (87 89) type "NECKLACE" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 15)) healthOrDamage (0 0) durability (476 483) id "Necklace-87-89")))
	(kind "wynncraft/recipe/get/Leggings-80-83" code 200 timestamp 1610855827758 version "2.1.0" 
	 data ((level (80 83) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Rice String" amount 5)) healthOrDamage (1870 2010) durability (452 462) id "Leggings-80-83")))
	(kind "wynncraft/recipe/get/Leggings-83-85" code 200 timestamp 1610855827957 version "2.1.0" 
	 data ((level (83 85) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Rice String" amount 5)) healthOrDamage (2010 2160) durability (462 469) id "Leggings-83-85")))
	(kind "wynncraft/recipe/get/Leggings-85-87" code 200 timestamp 1610855828137 version "2.1.0" 
	 data ((level (85 87) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Rice String" amount 5)) healthOrDamage (2160 2320) durability (469 476) id "Leggings-85-87")))
	(kind "wynncraft/recipe/get/Leggings-87-89" code 200 timestamp 1610855828319 version "2.1.0" 
	 data ((level (87 89) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Diamond Ingot" amount 10) (item "Refined Rice String" amount 5)) healthOrDamage (2320 2460) durability (476 483) id "Leggings-87-89")))
	(kind "wynncraft/recipe/get/Potion-80-83" code 200 timestamp 1610855828531 version "2.1.0" 
	 data ((level (80 83) type "POTION" skill "ALCHEMISM" materials ((item "Refined Bass Oil" amount 5) (item "Refined Rice Grains" amount 10)) healthOrDamage (2762 2927) duration (834 852) basicDuration (3 3) id "Potion-80-83")))
	(kind "wynncraft/recipe/get/Potion-83-85" code 200 timestamp 1610855828768 version "2.1.0" 
	 data ((level (83 85) type "POTION" skill "ALCHEMISM" materials ((item "Refined Bass Oil" amount 5) (item "Refined Rice Grains" amount 10)) healthOrDamage (2927 3092) duration (852 864) basicDuration (3 3) id "Potion-83-85")))
	(kind "wynncraft/recipe/get/Potion-85-87" code 200 timestamp 1610855828982 version "2.1.0" 
	 data ((level (85 87) type "POTION" skill "ALCHEMISM" materials ((item "Refined Bass Oil" amount 5) (item "Refined Rice Grains" amount 10)) healthOrDamage (3092 3323) duration (864 876) basicDuration (3 3) id "Potion-85-87")))
	(kind "wynncraft/recipe/get/Potion-87-89" code 200 timestamp 1610855829189 version "2.1.0" 
	 data ((level (87 89) type "POTION" skill "ALCHEMISM" materials ((item "Refined Bass Oil" amount 5) (item "Refined Rice Grains" amount 10)) healthOrDamage (3323 3554) duration (876 888) basicDuration (3 3) id "Potion-87-89")))
	(kind "wynncraft/recipe/get/Relik-80-83" code 200 timestamp 1610855829391 version "2.1.0" 
	 data ((level (80 83) type "RELIK" skill "WOODWORKING" materials ((item "Refined Bass Oil" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (253 268) durability (452 462) id "Relik-80-83")))
	(kind "wynncraft/recipe/get/Relik-83-85" code 200 timestamp 1610855829594 version "2.1.0" 
	 data ((level (83 85) type "RELIK" skill "WOODWORKING" materials ((item "Refined Bass Oil" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (268 282) durability (462 469) id "Relik-83-85")))
	(kind "wynncraft/recipe/get/Relik-85-87" code 200 timestamp 1610855829785 version "2.1.0" 
	 data ((level (85 87) type "RELIK" skill "WOODWORKING" materials ((item "Refined Bass Oil" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (282 296) durability (469 476) id "Relik-85-87")))
	(kind "wynncraft/recipe/get/Ring-83-85" code 200 timestamp 1610855829955 version "2.1.0" 
	 data ((level (83 85) type "RING" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 5)) healthOrDamage (0 0) durability (462 469) id "Ring-83-85")))
	(kind "wynncraft/recipe/get/Relik-87-89" code 200 timestamp 1610855830170 version "2.1.0" 
	 data ((level (87 89) type "RELIK" skill "WOODWORKING" materials ((item "Refined Bass Oil" amount 10) (item "Refined Pine Wood" amount 5)) healthOrDamage (296 311) durability (476 483) id "Relik-87-89")))
	(kind "wynncraft/recipe/get/Ring-85-87" code 200 timestamp 1610855830374 version "2.1.0" 
	 data ((level (85 87) type "RING" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 5)) healthOrDamage (0 0) durability (469 476) id "Ring-85-87")))
	(kind "wynncraft/recipe/get/Ring-80-83" code 200 timestamp 1610855830594 version "2.1.0" 
	 data ((level (80 83) type "RING" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 5)) healthOrDamage (0 0) durability (452 462) id "Ring-80-83")))
	(kind "wynncraft/recipe/get/Ring-87-89" code 200 timestamp 1610855830797 version "2.1.0" 
	 data ((level (87 89) type "RING" skill "JEWELING" materials ((item "Refined Diamond Gem" amount 5) (item "Refined Bass Oil" amount 5)) healthOrDamage (0 0) durability (476 483) id "Ring-87-89")))
	(kind "wynncraft/recipe/get/Scroll-80-83" code 200 timestamp 1610855832023 version "2.1.0" 
	 data ((level (80 83) type "SCROLL" skill "SCRIBING" materials ((item "Refined Bass Oil" amount 5) (item "Refined Pine Paper" amount 5)) healthOrDamage (1937 2053) duration (834 852) basicDuration (3 3) id "Scroll-80-83")))
	(kind "wynncraft/recipe/get/Scroll-85-87" code 200 timestamp 1610855832246 version "2.1.0" 
	 data ((level (85 87) type "SCROLL" skill "SCRIBING" materials ((item "Refined Bass Oil" amount 5) (item "Refined Pine Paper" amount 5)) healthOrDamage (2168 2330) duration (864 876) basicDuration (3 3) id "Scroll-85-87")))
	(kind "wynncraft/recipe/get/Scroll-83-85" code 200 timestamp 1610855832444 version "2.1.0" 
	 data ((level (83 85) type "SCROLL" skill "SCRIBING" materials ((item "Refined Bass Oil" amount 5) (item "Refined Pine Paper" amount 5)) healthOrDamage (2053 2168) duration (852 864) basicDuration (3 3) id "Scroll-83-85")))
	(kind "wynncraft/recipe/get/Spear-80-83" code 200 timestamp 1610855832699 version "2.1.0" 
	 data ((level (80 83) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Pine Wood" amount 10)) healthOrDamage (169 178) durability (452 462) id "Spear-80-83")))
	(kind "wynncraft/recipe/get/Scroll-87-89" code 200 timestamp 1610855832907 version "2.1.0" 
	 data ((level (87 89) type "SCROLL" skill "SCRIBING" materials ((item "Refined Bass Oil" amount 5) (item "Refined Pine Paper" amount 5)) healthOrDamage (2330 2491) duration (876 888) basicDuration (3 3) id "Scroll-87-89")))
	(kind "wynncraft/recipe/get/Spear-85-87" code 200 timestamp 1610855833175 version "2.1.0" 
	 data ((level (85 87) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Pine Wood" amount 10)) healthOrDamage (188 198) durability (469 476) id "Spear-85-87")))
	(kind "wynncraft/recipe/get/Wand-80-83" code 200 timestamp 1610855833384 version "2.1.0" 
	 data ((level (80 83) type "WAND" skill "WOODWORKING" materials ((item "Refined Rice String" amount 5) (item "Refined Pine Wood" amount 10)) healthOrDamage (127 134) durability (452 462) id "Wand-80-83")))
	(kind "wynncraft/recipe/get/Spear-87-89" code 200 timestamp 1610855833628 version "2.1.0" 
	 data ((level (87 89) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Pine Wood" amount 10)) healthOrDamage (198 207) durability (476 483) id "Spear-87-89")))
	(kind "wynncraft/recipe/get/Wand-83-85" code 200 timestamp 1610855833829 version "2.1.0" 
	 data ((level (83 85) type "WAND" skill "WOODWORKING" materials ((item "Refined Rice String" amount 5) (item "Refined Pine Wood" amount 10)) healthOrDamage (134 141) durability (462 469) id "Wand-83-85")))
	(kind "wynncraft/recipe/get/Wand-85-87" code 200 timestamp 1610855834044 version "2.1.0" 
	 data ((level (85 87) type "WAND" skill "WOODWORKING" materials ((item "Refined Rice String" amount 5) (item "Refined Pine Wood" amount 10)) healthOrDamage (141 148) durability (469 476) id "Wand-85-87")))
	(kind "wynncraft/recipe/get/Wand-87-89" code 200 timestamp 1610855835248 version "2.1.0" 
	 data ((level (87 89) type "WAND" skill "WOODWORKING" materials ((item "Refined Rice String" amount 5) (item "Refined Pine Wood" amount 10)) healthOrDamage (148 155) durability (476 483) id "Wand-87-89")))
	(kind "wynncraft/recipe/get/Spear-83-85" code 200 timestamp 1610855835456 version "2.1.0" 
	 data ((level (83 85) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Diamond Ingot" amount 5) (item "Refined Pine Wood" amount 10)) healthOrDamage (178 188) durability (462 469) id "Spear-83-85")))
	(kind "wynncraft/recipe/get/Boots-93-95" code 200 timestamp 1610855835674 version "2.1.0" 
	 data ((level (93 95) type "BOOTS" skill "TAILORING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Sorghum String" amount 12)) healthOrDamage (2640 2840) durability (497 504) id "Boots-93-95")))
	(kind "wynncraft/recipe/get/Boots-95-97" code 200 timestamp 1610855835885 version "2.1.0" 
	 data ((level (95 97) type "BOOTS" skill "TAILORING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Sorghum String" amount 12)) healthOrDamage (2840 3020) durability (504 511) id "Boots-95-97")))
	(kind "wynncraft/recipe/get/Boots-90-93" code 200 timestamp 1610855836082 version "2.1.0" 
	 data ((level (90 93) type "BOOTS" skill "TAILORING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Sorghum String" amount 12)) healthOrDamage (2460 2640) durability (487 497) id "Boots-90-93")))
	(kind "wynncraft/recipe/get/Boots-97-99" code 200 timestamp 1610855836292 version "2.1.0" 
	 data ((level (97 99) type "BOOTS" skill "TAILORING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Sorghum String" amount 12)) healthOrDamage (3020 3200) durability (511 518) id "Boots-97-99")))
	(kind "wynncraft/recipe/get/Bow-90-93" code 200 timestamp 1610855836509 version "2.1.0" 
	 data ((level (90 93) type "BOW" skill "WOODWORKING" materials ((item "Refined Sorghum String" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (314 330) durability (487 497) id "Bow-90-93")))
	(kind "wynncraft/recipe/get/Bow-93-95" code 200 timestamp 1610855836731 version "2.1.0" 
	 data ((level (93 95) type "BOW" skill "WOODWORKING" materials ((item "Refined Sorghum String" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (330 346) durability (497 504) id "Bow-93-95")))
	(kind "wynncraft/recipe/get/Bow-95-97" code 200 timestamp 1610855836989 version "2.1.0" 
	 data ((level (95 97) type "BOW" skill "WOODWORKING" materials ((item "Refined Sorghum String" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (346 361) durability (504 511) id "Bow-95-97")))
	(kind "wynncraft/recipe/get/Bow-97-99" code 200 timestamp 1610855837172 version "2.1.0" 
	 data ((level (97 99) type "BOW" skill "WOODWORKING" materials ((item "Refined Sorghum String" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (361 377) durability (511 518) id "Bow-97-99")))
	(kind "wynncraft/recipe/get/Bracelet-90-93" code 200 timestamp 1610855837369 version "2.1.0" 
	 data ((level (90 93) type "BRACELET" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 12)) healthOrDamage (0 0) durability (487 497) id "Bracelet-90-93")))
	(kind "wynncraft/recipe/get/Bracelet-93-95" code 200 timestamp 1610855837556 version "2.1.0" 
	 data ((level (93 95) type "BRACELET" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 12)) healthOrDamage (0 0) durability (497 504) id "Bracelet-93-95")))
	(kind "wynncraft/recipe/get/Bracelet-97-99" code 200 timestamp 1610855837741 version "2.1.0" 
	 data ((level (97 99) type "BRACELET" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 12)) healthOrDamage (0 0) durability (511 518) id "Bracelet-97-99")))
	(kind "wynncraft/recipe/get/Chestplate-90-93" code 200 timestamp 1610855837956 version "2.1.0" 
	 data ((level (90 93) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Avo Paper" amount 6)) healthOrDamage (2480 2640) durability (487 497) id "Chestplate-90-93")))
	(kind "wynncraft/recipe/get/Chestplate-93-95" code 200 timestamp 1610855838156 version "2.1.0" 
	 data ((level (93 95) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Avo Paper" amount 6)) healthOrDamage (2640 2840) durability (497 504) id "Chestplate-93-95")))
	(kind "wynncraft/recipe/get/Bracelet-95-97" code 200 timestamp 1610855838348 version "2.1.0" 
	 data ((level (95 97) type "BRACELET" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 12)) healthOrDamage (0 0) durability (504 511) id "Bracelet-95-97")))
	(kind "wynncraft/recipe/get/Chestplate-95-97" code 200 timestamp 1610855838546 version "2.1.0" 
	 data ((level (95 97) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Avo Paper" amount 6)) healthOrDamage (2840 3020) durability (504 511) id "Chestplate-95-97")))
	(kind "wynncraft/recipe/get/Ring-1-3" code 200 timestamp 1610855838746 version "2.1.0" 
	 data ((level (1 3) type "RING" skill "JEWELING" materials ((item "Refined Copper Gem" amount 1) (item "Refined Gudgeon Oil" amount 1)) healthOrDamage (0 0) durability (175 182) id "Ring-1-3")))
	(kind "wynncraft/recipe/get/Chestplate-97-99" code 200 timestamp 1610855838942 version "2.1.0" 
	 data ((level (97 99) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Avo Paper" amount 6)) healthOrDamage (3020 3200) durability (511 518) id "Chestplate-97-99")))
	(kind "wynncraft/recipe/get/Dagger-93-95" code 200 timestamp 1610855839134 version "2.1.0" 
	 data ((level (93 95) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (275 288) durability (497 504) id "Dagger-93-95")))
	(kind "wynncraft/recipe/get/Dagger-90-93" code 200 timestamp 1610855839327 version "2.1.0" 
	 data ((level (90 93) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (262 275) durability (487 497) id "Dagger-90-93")))
	(kind "wynncraft/recipe/get/Dagger-95-97" code 200 timestamp 1610855839519 version "2.1.0" 
	 data ((level (95 97) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (288 301) durability (504 511) id "Dagger-95-97")))
	(kind "wynncraft/recipe/get/Dagger-97-99" code 200 timestamp 1610855839727 version "2.1.0" 
	 data ((level (97 99) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (301 314) durability (511 518) id "Dagger-97-99")))
	(kind "wynncraft/recipe/get/Food-90-93" code 200 timestamp 1610855839933 version "2.1.0" 
	 data ((level (90 93) type "FOOD" skill "COOKING" materials ((item "Refined Molten Meat" amount 12) (item "Refined Sorghum Grains" amount 6)) healthOrDamage (3587 3835) duration (2682 2736) basicDuration (60 60) id "Food-90-93")))
	(kind "wynncraft/recipe/get/Food-93-95" code 200 timestamp 1610855840119 version "2.1.0" 
	 data ((level (93 95) type "FOOD" skill "COOKING" materials ((item "Refined Molten Meat" amount 12) (item "Refined Sorghum Grains" amount 6)) healthOrDamage (3835 4082) duration (2736 2772) basicDuration (60 60) id "Food-93-95")))
	(kind "wynncraft/recipe/get/Food-95-97" code 200 timestamp 1610855840301 version "2.1.0" 
	 data ((level (95 97) type "FOOD" skill "COOKING" materials ((item "Refined Molten Meat" amount 12) (item "Refined Sorghum Grains" amount 6)) healthOrDamage (4082 4467) duration (2772 2808) basicDuration (60 60) id "Food-95-97")))
	(kind "wynncraft/recipe/get/Food-97-99" code 200 timestamp 1610855840483 version "2.1.0" 
	 data ((level (97 99) type "FOOD" skill "COOKING" materials ((item "Refined Molten Meat" amount 12) (item "Refined Sorghum Grains" amount 6)) healthOrDamage (4467 4852) duration (2808 2844) basicDuration (60 60) id "Food-97-99")))
	(kind "wynncraft/recipe/get/Helmet-90-93" code 200 timestamp 1610855840681 version "2.1.0" 
	 data ((level (90 93) type "HELMET" skill "ARMOURING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Avo Paper" amount 12)) healthOrDamage (2480 2640) durability (483 497) id "Helmet-90-93")))
	(kind "wynncraft/recipe/get/Helmet-93-95" code 200 timestamp 1610855840878 version "2.1.0" 
	 data ((level (93 95) type "HELMET" skill "ARMOURING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Avo Paper" amount 12)) healthOrDamage (2640 2840) durability (497 504) id "Helmet-93-95")))
	(kind "wynncraft/recipe/get/Helmet-95-97" code 200 timestamp 1610855841062 version "2.1.0" 
	 data ((level (95 97) type "HELMET" skill "ARMOURING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Avo Paper" amount 12)) healthOrDamage (2840 3020) durability (504 511) id "Helmet-95-97")))
	(kind "wynncraft/recipe/get/Helmet-97-99" code 200 timestamp 1610855841275 version "2.1.0" 
	 data ((level (97 99) type "HELMET" skill "ARMOURING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Avo Paper" amount 12)) healthOrDamage (3020 3200) durability (511 518) id "Helmet-97-99")))
	(kind "wynncraft/recipe/get/Necklace-93-95" code 200 timestamp 1610855841475 version "2.1.0" 
	 data ((level (93 95) type "NECKLACE" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 18)) healthOrDamage (0 0) durability (497 504) id "Necklace-93-95")))
	(kind "wynncraft/recipe/get/Necklace-95-97" code 200 timestamp 1610855841691 version "2.1.0" 
	 data ((level (95 97) type "NECKLACE" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 18)) healthOrDamage (0 0) durability (504 511) id "Necklace-95-97")))
	(kind "wynncraft/recipe/get/Necklace-97-99" code 200 timestamp 1610855841894 version "2.1.0" 
	 data ((level (97 99) type "NECKLACE" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 18)) healthOrDamage (0 0) durability (511 518) id "Necklace-97-99")))
	(kind "wynncraft/recipe/get/Leggings-90-93" code 200 timestamp 1610855842077 version "2.1.0" 
	 data ((level (90 93) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Sorghum String" amount 6)) healthOrDamage (2480 2640) durability (487 497) id "Leggings-90-93")))
	(kind "wynncraft/recipe/get/Necklace-90-93" code 200 timestamp 1610855842272 version "2.1.0" 
	 data ((level (90 93) type "NECKLACE" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 18)) healthOrDamage (0 0) durability (487 497) id "Necklace-90-93")))
	(kind "wynncraft/recipe/get/Leggings-93-95" code 200 timestamp 1610855842462 version "2.1.0" 
	 data ((level (93 95) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Sorghum String" amount 6)) healthOrDamage (2640 2840) durability (497 504) id "Leggings-93-95")))
	(kind "wynncraft/recipe/get/Leggings-97-99" code 200 timestamp 1610855842656 version "2.1.0" 
	 data ((level (97 99) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Sorghum String" amount 6)) healthOrDamage (3020 3200) durability (511 518) id "Leggings-97-99")))
	(kind "wynncraft/recipe/get/Potion-90-93" code 200 timestamp 1610855842848 version "2.1.0" 
	 data ((level (90 93) type "POTION" skill "ALCHEMISM" materials ((item "Refined Molten Oil" amount 6) (item "Refined Sorghum Grains" amount 12)) healthOrDamage (3587 3835) duration (894 912) basicDuration (3 3) id "Potion-90-93")))
	(kind "wynncraft/recipe/get/Potion-93-95" code 200 timestamp 1610855843050 version "2.1.0" 
	 data ((level (93 95) type "POTION" skill "ALCHEMISM" materials ((item "Refined Molten Oil" amount 6) (item "Refined Sorghum Grains" amount 12)) healthOrDamage (3835 4082) duration (912 924) basicDuration (3 3) id "Potion-93-95")))
	(kind "wynncraft/recipe/get/Potion-95-97" code 200 timestamp 1610855843245 version "2.1.0" 
	 data ((level (95 97) type "POTION" skill "ALCHEMISM" materials ((item "Refined Molten Oil" amount 6) (item "Refined Sorghum Grains" amount 12)) healthOrDamage (4082 4467) duration (924 936) basicDuration (3 3) id "Potion-95-97")))
	(kind "wynncraft/recipe/get/Relik-90-93" code 200 timestamp 1610855843448 version "2.1.0" 
	 data ((level (90 93) type "RELIK" skill "WOODWORKING" materials ((item "Refined Molten Oil" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (314 330) durability (487 497) id "Relik-90-93")))
	(kind "wynncraft/recipe/get/Potion-97-99" code 200 timestamp 1610855843646 version "2.1.0" 
	 data ((level (97 99) type "POTION" skill "ALCHEMISM" materials ((item "Refined Molten Oil" amount 6) (item "Refined Sorghum Grains" amount 12)) healthOrDamage (4467 4852) duration (936 948) basicDuration (3 3) id "Potion-97-99")))
	(kind "wynncraft/recipe/get/Relik-93-95" code 200 timestamp 1610855843837 version "2.1.0" 
	 data ((level (93 95) type "RELIK" skill "WOODWORKING" materials ((item "Refined Molten Oil" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (330 346) durability (497 504) id "Relik-93-95")))
	(kind "wynncraft/recipe/get/Relik-95-97" code 200 timestamp 1610855844022 version "2.1.0" 
	 data ((level (95 97) type "RELIK" skill "WOODWORKING" materials ((item "Refined Molten Oil" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (346 361) durability (504 511) id "Relik-95-97")))
	(kind "wynncraft/recipe/get/Ring-90-93" code 200 timestamp 1610855844222 version "2.1.0" 
	 data ((level (90 93) type "RING" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 6)) healthOrDamage (0 0) durability (487 497) id "Ring-90-93")))
	(kind "wynncraft/recipe/get/Ring-95-97" code 200 timestamp 1610855844406 version "2.1.0" 
	 data ((level (95 97) type "RING" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 6)) healthOrDamage (0 0) durability (504 511) id "Ring-95-97")))
	(kind "wynncraft/recipe/get/Ring-93-95" code 200 timestamp 1610855844617 version "2.1.0" 
	 data ((level (93 95) type "RING" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 6)) healthOrDamage (0 0) durability (497 504) id "Ring-93-95")))
	(kind "wynncraft/recipe/get/Scroll-90-93" code 200 timestamp 1610855844855 version "2.1.0" 
	 data ((level (90 93) type "SCROLL" skill "SCRIBING" materials ((item "Refined Molten Oil" amount 6) (item "Refined Avo Paper" amount 6)) healthOrDamage (2515 2688) duration (894 912) basicDuration (3 3) id "Scroll-90-93")))
	(kind "wynncraft/recipe/get/Scroll-93-95" code 200 timestamp 1610855845046 version "2.1.0" 
	 data ((level (93 95) type "SCROLL" skill "SCRIBING" materials ((item "Refined Molten Oil" amount 6) (item "Refined Avo Paper" amount 6)) healthOrDamage (2688 2861) duration (912 924) basicDuration (3 3) id "Scroll-93-95")))
	(kind "wynncraft/recipe/get/Ring-97-99" code 200 timestamp 1610855845274 version "2.1.0" 
	 data ((level (97 99) type "RING" skill "JEWELING" materials ((item "Refined Molten Gem" amount 6) (item "Refined Molten Oil" amount 6)) healthOrDamage (0 0) durability (511 518) id "Ring-97-99")))
	(kind "wynncraft/recipe/get/Scroll-95-97" code 200 timestamp 1610855845494 version "2.1.0" 
	 data ((level (95 97) type "SCROLL" skill "SCRIBING" materials ((item "Refined Molten Oil" amount 6) (item "Refined Avo Paper" amount 6)) healthOrDamage (2861 3131) duration (924 936) basicDuration (3 3) id "Scroll-95-97")))
	(kind "wynncraft/recipe/get/Spear-90-93" code 200 timestamp 1610855845690 version "2.1.0" 
	 data ((level (90 93) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Avo Wood" amount 12)) healthOrDamage (210 220) durability (487 497) id "Spear-90-93")))
	(kind "wynncraft/recipe/get/Spear-93-95" code 200 timestamp 1610855845950 version "2.1.0" 
	 data ((level (93 95) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Avo Wood" amount 12)) healthOrDamage (220 230) durability (497 504) id "Spear-93-95")))
	(kind "wynncraft/recipe/get/Spear-95-97" code 200 timestamp 1610855846149 version "2.1.0" 
	 data ((level (95 97) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Avo Wood" amount 12)) healthOrDamage (230 241) durability (504 511) id "Spear-95-97")))
	(kind "wynncraft/recipe/get/Spear-97-99" code 200 timestamp 1610855846370 version "2.1.0" 
	 data ((level (97 99) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Molten Ingot" amount 6) (item "Refined Avo Wood" amount 12)) healthOrDamage (241 251) durability (511 518) id "Spear-97-99")))
	(kind "wynncraft/recipe/get/Relik-97-99" code 200 timestamp 1610855846584 version "2.1.0" 
	 data ((level (97 99) type "RELIK" skill "WOODWORKING" materials ((item "Refined Molten Oil" amount 12) (item "Refined Avo Wood" amount 6)) healthOrDamage (361 377) durability (511 518) id "Relik-97-99")))
	(kind "wynncraft/recipe/get/Leggings-95-97" code 200 timestamp 1610855846904 version "2.1.0" 
	 data ((level (95 97) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Molten Ingot" amount 12) (item "Refined Sorghum String" amount 6)) healthOrDamage (2840 3020) durability (504 511) id "Leggings-95-97")))
	(kind "wynncraft/recipe/get/Wand-90-93" code 200 timestamp 1610855847139 version "2.1.0" 
	 data ((level (90 93) type "WAND" skill "WOODWORKING" materials ((item "Refined Sorghum String" amount 6) (item "Refined Avo Wood" amount 12)) healthOrDamage (157 165) durability (487 497) id "Wand-90-93")))
	(kind "wynncraft/recipe/get/Scroll-97-99" code 200 timestamp 1610855847418 version "2.1.0" 
	 data ((level (97 99) type "SCROLL" skill "SCRIBING" materials ((item "Refined Molten Oil" amount 6) (item "Refined Avo Paper" amount 6)) healthOrDamage (3131 3400) duration (936 948) basicDuration (3 3) id "Scroll-97-99")))
	(kind "wynncraft/recipe/get/Wand-93-95" code 200 timestamp 1610855847660 version "2.1.0" 
	 data ((level (93 95) type "WAND" skill "WOODWORKING" materials ((item "Refined Sorghum String" amount 6) (item "Refined Avo Wood" amount 12)) healthOrDamage (165 173) durability (497 504) id "Wand-93-95")))
	(kind "wynncraft/recipe/get/Wand-95-97" code 200 timestamp 1610855847879 version "2.1.0" 
	 data ((level (95 97) type "WAND" skill "WOODWORKING" materials ((item "Refined Sorghum String" amount 6) (item "Refined Avo Wood" amount 12)) healthOrDamage (173 181) durability (504 511) id "Wand-95-97")))
	(kind "wynncraft/recipe/get/Wand-97-99" code 200 timestamp 1610855848066 version "2.1.0" 
	 data ((level (97 99) type "WAND" skill "WOODWORKING" materials ((item "Refined Sorghum String" amount 6) (item "Refined Avo Wood" amount 12)) healthOrDamage (181 188) durability (511 518) id "Wand-97-99")))
	(kind "wynncraft/recipe/get/Boots-100-103" code 200 timestamp 1610855848343 version "2.1.0" 
	 data ((level (100 103) type "BOOTS" skill "TAILORING" materials ((item "Refined Voidstone Ingot" amount 6) (item "Refined Hemp String" amount 12)) healthOrDamage (3250 3300) durability (522 525) id "Boots-100-103")))
	(kind "wynncraft/recipe/get/Bow-100-103" code 200 timestamp 1610855848537 version "2.1.0" 
	 data ((level (100 103) type "BOW" skill "WOODWORKING" materials ((item "Refined Hemp String" amount 12) (item "Refined Sky Wood" amount 6)) healthOrDamage (379 382) durability (522 525) id "Bow-100-103")))
	(kind "wynncraft/recipe/get/Bracelet-100-103" code 200 timestamp 1610855848712 version "2.1.0" 
	 data ((level (100 103) type "BRACELET" skill "JEWELING" materials ((item "Refined Voidstone Gem" amount 6) (item "Refined Starfish Oil" amount 12)) healthOrDamage (0 0) durability (522 525) id "Bracelet-100-103")))
	(kind "wynncraft/recipe/get/Chestplate-100-103" code 200 timestamp 1610855848911 version "2.1.0" 
	 data ((level (100 103) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Voidstone Ingot" amount 12) (item "Refined Sky Paper" amount 6)) healthOrDamage (3250 3300) durability (522 525) id "Chestplate-100-103")))
	(kind "wynncraft/recipe/get/Dagger-100-103" code 200 timestamp 1610855849107 version "2.1.0" 
	 data ((level (100 103) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Voidstone Ingot" amount 12) (item "Refined Sky Wood" amount 6)) healthOrDamage (316 318) durability (522 525) id "Dagger-100-103")))
	(kind "wynncraft/recipe/get/Necklace-100-103" code 200 timestamp 1610855849298 version "2.1.0" 
	 data ((level (100 103) type "NECKLACE" skill "JEWELING" materials ((item "Refined Voidstone Gem" amount 6) (item "Refined Starfish Oil" amount 18)) healthOrDamage (0 0) durability (522 525) id "Necklace-100-103")))
	(kind "wynncraft/recipe/get/Potion-100-103" code 200 timestamp 1610855849497 version "2.1.0" 
	 data ((level (100 103) type "POTION" skill "ALCHEMISM" materials ((item "Refined Starfish Oil" amount 6) (item "Refined Hemp Grains" amount 12)) healthOrDamage (4962 5072) duration (954 960) basicDuration (3 3) id "Potion-100-103")))
	(kind "wynncraft/recipe/get/Relik-100-103" code 200 timestamp 1610855849672 version "2.1.0" 
	 data ((level (100 103) type "RELIK" skill "WOODWORKING" materials ((item "Refined Starfish Oil" amount 12) (item "Refined Sky Wood" amount 6)) healthOrDamage (379 382) durability (522 525) id "Relik-100-103")))
	(kind "wynncraft/recipe/get/Ring-100-103" code 200 timestamp 1610855849867 version "2.1.0" 
	 data ((level (100 103) type "RING" skill "JEWELING" materials ((item "Refined Voidstone Gem" amount 6) (item "Refined Starfish Oil" amount 6)) healthOrDamage (0 0) durability (522 525) id "Ring-100-103")))
	(kind "wynncraft/recipe/get/Scroll-100-103" code 200 timestamp 1610855850062 version "2.1.0" 
	 data ((level (100 103) type "SCROLL" skill "SCRIBING" materials ((item "Refined Starfish Oil" amount 6) (item "Refined Sky Paper" amount 6)) healthOrDamage (3477 3532) duration (954 960) basicDuration (3 3) id "Scroll-100-103")))
	(kind "wynncraft/recipe/get/Spear-100-103" code 200 timestamp 1610855850339 version "2.1.0" 
	 data ((level (100 103) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Voidstone Ingot" amount 6) (item "Refined Sky Wood" amount 12)) healthOrDamage (253 255) durability (522 529) id "Spear-100-103")))
	(kind "wynncraft/recipe/get/Leggings-100-103" code 200 timestamp 1610855850540 version "2.1.0" 
	 data ((level (100 103) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Voidstone Ingot" amount 12) (item "Refined Hemp String" amount 6)) healthOrDamage (3250 3300) durability (522 525) id "Leggings-100-103")))
	(kind "wynncraft/recipe/get/Boots-103-105" code 200 timestamp 1610855850740 version "2.1.0" 
	 data ((level (103 105) type "BOOTS" skill "TAILORING" materials ((item "Refined Dernic Ingot" amount 6) (item "Refined Dernic String" amount 12)) healthOrDamage (3300 3350) durability (525 527) id "Boots-103-105")))
	(kind "wynncraft/recipe/get/Bow-103-105" code 200 timestamp 1610855850940 version "2.1.0" 
	 data ((level (103 105) type "BOW" skill "WOODWORKING" materials ((item "Refined Dernic String" amount 12) (item "Refined Dernic Wood" amount 6)) healthOrDamage (382 385) durability (525 527) id "Bow-103-105")))
	(kind "wynncraft/recipe/get/Bracelet-103-105" code 200 timestamp 1610855851166 version "2.1.0" 
	 data ((level (103 105) type "BRACELET" skill "JEWELING" materials ((item "Refined Dernic Gem" amount 6) (item "Refined Dernic Oil" amount 12)) healthOrDamage (0 0) durability (525 527) id "Bracelet-103-105")))
	(kind "wynncraft/recipe/get/Chestplate-103-105" code 200 timestamp 1610855851383 version "2.1.0" 
	 data ((level (103 105) type "CHESTPLATE" skill "ARMOURING" materials ((item "Refined Dernic Ingot" amount 12) (item "Refined Dernic Paper" amount 6)) healthOrDamage (3300 3350) durability (525 527) id "Chestplate-103-105")))
	(kind "wynncraft/recipe/get/Dagger-103-105" code 200 timestamp 1610855851592 version "2.1.0" 
	 data ((level (103 105) type "DAGGER" skill "WEAPONSMITHING" materials ((item "Refined Dernic Ingot" amount 12) (item "Refined Dernic Wood" amount 6)) healthOrDamage (318 320) durability (525 527) id "Dagger-103-105")))
	(kind "wynncraft/recipe/get/Food-103-105" code 200 timestamp 1610855851827 version "2.1.0" 
	 data ((level (103 105) type "FOOD" skill "COOKING" materials ((item "Refined Dernic Meat" amount 12) (item "Refined Dernic Grains" amount 6)) healthOrDamage (5072 5170) duration (2880 2900) basicDuration (60 60) id "Food-103-105")))
	(kind "wynncraft/recipe/get/Helmet-103-105" code 200 timestamp 1610855852016 version "2.1.0" 
	 data ((level (103 105) type "HELMET" skill "ARMOURING" materials ((item "Refined Dernic Ingot" amount 6) (item "Refined Dernic Paper" amount 12)) healthOrDamage (3300 3350) durability (525 527) id "Helmet-103-105")))
	(kind "wynncraft/recipe/get/Food-100-103" code 200 timestamp 1610855852245 version "2.1.0" 
	 data ((level (100 103) type "FOOD" skill "COOKING" materials ((item "Refined Starfish Meat" amount 12) (item "Refined Hemp Grains" amount 6)) healthOrDamage (4962 5072) duration (2862 2880) basicDuration (60 60) id "Food-100-103")))
	(kind "wynncraft/recipe/get/Leggings-103-105" code 200 timestamp 1610855852432 version "2.1.0" 
	 data ((level (103 105) type "LEGGINGS" skill "TAILORING" materials ((item "Refined Dernic Ingot" amount 12) (item "Refined Dernic String" amount 6)) healthOrDamage (3300 3350) durability (525 527) id "Leggings-103-105")))
	(kind "wynncraft/recipe/get/Potion-103-105" code 200 timestamp 1610855852615 version "2.1.0" 
	 data ((level (103 105) type "POTION" skill "ALCHEMISM" materials ((item "Refined Dernic Oil" amount 6) (item "Refined Dernic Grains" amount 12)) healthOrDamage (5072 5170) duration (960 964) basicDuration (3 3) id "Potion-103-105")))
	(kind "wynncraft/recipe/get/Relik-103-105" code 200 timestamp 1610855852818 version "2.1.0" 
	 data ((level (103 105) type "RELIK" skill "WOODWORKING" materials ((item "Refined Dernic Oil" amount 12) (item "Refined Dernic Wood" amount 6)) healthOrDamage (382 385) durability (525 527) id "Relik-103-105")))
	(kind "wynncraft/recipe/get/Wand-100-103" code 200 timestamp 1610855853033 version "2.1.0" 
	 data ((level (100 103) type "WAND" skill "WOODWORKING" materials ((item "Refined Hemp String" amount 6) (item "Refined Sky Wood" amount 12)) healthOrDamage (190 192) durability (522 525) id "Wand-100-103")))
	(kind "wynncraft/recipe/get/Ring-103-105" code 200 timestamp 1610855853292 version "2.1.0" 
	 data ((level (103 105) type "RING" skill "JEWELING" materials ((item "Refined Dernic Gem" amount 6) (item "Refined Dernic Oil" amount 6)) healthOrDamage (0 0) durability (525 527) id "Ring-103-105")))
	(kind "wynncraft/recipe/get/Necklace-103-105" code 200 timestamp 1610855853533 version "2.1.0" 
	 data ((level (103 105) type "NECKLACE" skill "JEWELING" materials ((item "Refined Dernic Gem" amount 6) (item "Refined Dernic Oil" amount 18)) healthOrDamage (0 0) durability (525 527) id "Necklace-103-105")))
	(kind "wynncraft/recipe/get/Scroll-103-105" code 200 timestamp 1610855853735 version "2.1.0" 
	 data ((level (103 105) type "SCROLL" skill "SCRIBING" materials ((item "Refined Dernic Oil" amount 6) (item "Refined Dernic Paper" amount 6)) healthOrDamage (3540 3600) duration (960 964) basicDuration (3 3) id "Scroll-103-105")))
	(kind "wynncraft/recipe/get/Spear-103-105" code 200 timestamp 1610855853943 version "2.1.0" 
	 data ((level (103 105) type "SPEAR" skill "WEAPONSMITHING" materials ((item "Refined Dernic Ingot" amount 6) (item "Refined Dernic Wood" amount 12)) healthOrDamage (256 259) durability (529 532) id "Spear-103-105")))
	(kind "wynncraft/recipe/get/Wand-103-105" code 200 timestamp 1610855854132 version "2.1.0" 
	 data ((level (103 105) type "WAND" skill "WOODWORKING" materials ((item "Refined Dernic String" amount 6) (item "Refined Dernic Wood" amount 12)) healthOrDamage (193 196) durability (525 527) id "Wand-103-105")))
	(kind "wynncraft/recipe/get/Helmet-100-103" code 200 timestamp 1610855854386 version "2.1.0" 
	 data ((level (100 103) type "HELMET" skill "ARMOURING" materials ((item "Refined Voidstone Ingot" amount 6) (item "Refined Sky Paper" amount 12)) healthOrDamage (3250 3300) durability (522 525) id "Helmet-100-103")))
	))
