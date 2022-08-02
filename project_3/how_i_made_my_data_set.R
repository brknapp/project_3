library(httr) #this package will help use use the URL we built to get information from the OMDb API
library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
library(tidyverse) #this package will help us work with our nicely formatted data.
library(lubridate) #this package will help us create dates 
library(ggplot2) #this package will help us make graphs

mykey <- "5c7f9206"

search_by_title <- function(mykey,title,type="movie"){
  #build URL:
  base_url <- paste0("http://www.omdbapi.com/?apikey=",mykey)
  info_url <- paste0("&t=",title,"&type=",type) 
  full_url <- paste0(base_url, info_url)
  full_url <- gsub(full_url, pattern = " ", replacement = "%20")
  
  #use URL to get data from the OMDb API:
  movie_api_call <- GET(full_url)
  movie_api_call_char <- rawToChar(movie_api_call$content)
  movie_JSON <- jsonlite::fromJSON(movie_api_call_char, flatten = TRUE) 
  movie_JSON <- as.data.frame(movie_JSON)
  tibble_movie_JSON <- as_tibble(movie_JSON)
  return(tibble_movie_JSON)
}

search_by_title_and_date <- function(mykey,title,type="movie",date){
  #build URL:
  base_url <- paste0("http://www.omdbapi.com/?apikey=",mykey)
  info_url <- paste0("&t=",title,"&type=",type,"&y=",date) 
  full_url <- paste0(base_url, info_url)
  full_url <- gsub(full_url, pattern = " ", replacement = "%20")
  
  #use URL to get data from the OMDb API:
  movie_api_call <- GET(full_url)
  movie_api_call_char <- rawToChar(movie_api_call$content)
  movie_JSON <- jsonlite::fromJSON(movie_api_call_char, flatten = TRUE) 
  movie_JSON <- as.data.frame(movie_JSON)
  tibble_movie_JSON <- as_tibble(movie_JSON)
  return(tibble_movie_JSON)
}

search_by_IMDb_ID <- function(mykey,IMDb_ID,type="movie"){
  #build URL:
  base_url <- paste0("http://www.omdbapi.com/?apikey=",mykey)
  info_url <- paste0("&i=",IMDb_ID,"&type=",type) 
  full_url <- paste0(base_url, info_url)
  full_url <- gsub(full_url, pattern = " ", replacement = "%20")
  
  #use URL to get data from the OMDb API:
  movie_api_call <- GET(full_url)
  movie_api_call_char <- rawToChar(movie_api_call$content)
  movie_JSON <- jsonlite::fromJSON(movie_api_call_char, flatten = TRUE) 
  movie_JSON <- as.data.frame(movie_JSON)
  tibble_movie_JSON <- as_tibble(movie_JSON)
  return(tibble_movie_JSON)
}

by_search_series <- function(mykey,title,type="movie"){
  #build URL:
  base_url <- paste0("http://www.omdbapi.com/?apikey=",mykey)
  info_url <- paste0("&s=",title,"&type=",type) 
  full_url <- paste0(base_url, info_url)
  full_url <- gsub(full_url, pattern = " ", replacement = "%20")
  
  #use URL to get a data frame with a list of titles from the OMDb API:
  movie_api_call <- GET(full_url)
  movie_api_call_char <- rawToChar(movie_api_call$content)
  movie_JSON <- jsonlite::fromJSON(movie_api_call_char, flatten = TRUE) 
  movie_JSON <- as.data.frame(movie_JSON)
  tibble_movie_JSON <- as_tibble(movie_JSON)
  return(tibble_movie_JSON)
}

mat=NULL
by_search_one_or_more_titles <- function(mykey,title,type="movie"){
  #if you only give one title, this part will run:
  if(length(title)<=1){ 
    #build URL:
    base_url <- paste0("http://www.omdbapi.com/?apikey=",mykey)
    info_url <- paste0("&s=",title,"&type=",type) 
    full_url <- paste0(base_url, info_url)
    full_url <- gsub(full_url, pattern = " ", replacement = "%20")
    
    #use URL to get a data frame with a list of titles from the OMDb API:
    movie_api_call <- GET(full_url)
    movie_api_call_char <- rawToChar(movie_api_call$content)
    movie_JSON <- jsonlite::fromJSON(movie_api_call_char, flatten = TRUE) 
    movie_JSON <- as.data.frame(movie_JSON)
    movie_JSON <- as_tibble(movie_JSON)
    return(movie_JSON)
  }
  if(length(title)>1){
    #if you give more than one title, this part will run:
    for(i in title){
      #build URL:
      base_url <- paste0("http://www.omdbapi.com/?apikey=",mykey)
      info_url <- paste0("&s=",i,"&type=",type) 
      full_url <- paste0(base_url, info_url)
      full_url <- gsub(full_url, pattern = " ", replacement = "%20")
      
      #use URL to get a data frame with a list of titles from the OMDb API:
      movie_api_call <- GET(full_url)
      movie_api_call_char <- rawToChar(movie_api_call$content)
      movie_JSON <- jsonlite::fromJSON(movie_api_call_char, flatten = TRUE) 
      movie_JSON <- as.data.frame(movie_JSON)
      mat=rbind(mat,movie_JSON)
      mat=as_tibble(mat)
    }
  }
  return(mat)
}

mat=NULL
get_data_series <- function(mykey,title){
  #this part gets the titles in the series given:
  temp_table <- by_search_series(mykey,title,type="movie")
  list_of_titles <- unique(temp_table$Search.Title)
  
  #this part cycles through each title and gets the data:
  for(movie_title in list_of_titles){
    table <- search_by_title(mykey,movie_title,type="movie")
    mat=rbind(mat,table)
  }
  return(mat)
}

mat=NULL
get_data_one_or_more_titles <- function(mykey,title){
  #this part gets the titles in all series given:
  temp_table <- by_search_one_or_more_titles(mykey,title,type="movie")
  list_of_titles <- unique(temp_table$Search.Title)
  
  #this part cycles through each title and gets the data:
  for(movie_title in list_of_titles){
    table <- search_by_title(mykey,movie_title,type="movie")
    mat=rbind(mat,table)
  }
  return(mat)
}

titles <- c("Harry Potter and the Sorcerer's Stone",
            "Harry Potter and the Chamber of Secrets",
            "Harry Potter and the Prisoner of Azkaban",
            "Harry Potter and the Goblet of Fire",
            "Harry Potter and the Order of the Phoenix",
            "Harry Potter and the Half-Blood Prince",
            "Harry Potter and the Deathly Hallows: Part 1",
            "Harry Potter and the Deathly Hallows: Part 2" ,
            "Rear_Window",
            "Dial_M_for_Murder",
            "Mrs._Miniver",
            "Now,_Voyager",
            "Gone_with_the_Wind",
            "Goodbye,_Mr._Chips",
            "Marty",
            "E.T._the_Extra-Terrestrial",
            "Jaws",
            "Kitty_Foyle",
            "Penny_Serenade",
            "Room_for_One_More",
            "Holiday_Affair",
            "The_More_the_Merrier",
            "Stella_Dallas",
            "Mildred_Pierce",
            "North_by_Northwest",
            "The_Birds",
            "Sleepless_in_Seattle",
            "Lars_and_the_Real_Girl",
            "You've Got Mail",
            "Meet_Me_in_St._Louis",
            "The_Man_Who_Came_to_Dinner",
            "On_the_Waterfront",
            "The_Bridge_on_the_River_Kwai",
            "Our_Vines_Have_Tender_Grapes",
            "Splendor_in_the_Grass",
            "This_Property_Is_Condemned",
            "Miracle_on_34th_Street",
            "Rebecca",
            "Indiscreet",
            "Dirty_Dancing",
            "Footloose",
            "Grease",
            "A_Christmas_Story",
            "Mr._Skeffington",
            "Random_Harvest",
            "Mrs. Parkington",
            "Blossoms in the Dust",
            "Iron_Man",
            "The_Incredible_Hulk",
            "Iron_Man_2",
            "Thor",
            "Captain_America:_The_First_Avenger",
            "The_Avengers",
            "Iron Man 3",
            "Thor:_The_Dark_World",
            "Captain_America:_The_Winter_Soldier",
            "Guardians_of_the_Galaxy",
            "Avengers:_Age_of_Ultron",
            "Ant-Man",
            "Captain_America:_Civil_War",
            "Doctor_Strange",
            "Guardians_of_the_Galaxy_Vol._2",
            "Spider-Man:_Homecoming",
            "Thor:_Ragnarok",
            "Black_Panther",
            "Avengers:_Infinity_War",
            "Ant-Man_and_the_Wasp",
            "Captain_Marvel",
            "Avengers:_Endgame",
            "Spider-Man:_Far_From_Home",
            "Black_Widow",
            "Shang-Chi_and_the_Legend_of_the_Ten_Rings",
            "Eternals",
            "Doctor_Strange_in_the_Multiverse_of_Madness",
            "casablanca",
            "the_wizard_of_oz",
            "it's_a_wonderful_life",
            "goodfellas",
            "taxi_driver",
            "psycho",
            "singin_in_the_rain",
            "2001:_a_space_odyssey",
            "vertigo",
            "fantastic_beasts_and_where_to_find_them",
            "baywatch",
            "the_bourne_identity",
            "the_bourne_supremacy",
            "the_bourne_ultimatum",
            "the_bourne_legacy",
            "willy_wonka_&_the_chocolate_factory",
            "dr._no",
            "from_russia_with_love",
            "goldfinger",
            "thunderball",
            "you_only_live_twice",
            "on_her_majesty's_secret_service",
            "diamonds_are_forever",
            "live_and_let_die",
            "the_man_with_the_golden_gun",
            "the_spy_who_loved_me",
            "moonraker",
            "for_your_eyes_only",
            "octopussy",
            "a_view_to_a_kill",
            "the_living_daylights",
            "licence_to_kill",
            "goldeneye",
            "tomorrow_never_dies",
            "the_world_is_not_enough",
            "die_another_day",
            "casino_royale",
            "quantum_of_solace",
            "skyfall",
            "spectre",
            "no_time_to_die",
            #start of additions:
            "Mission: Impossible",
            "Mission: Impossible II",
            "Mission: Impossible III",
            "Mission: Impossible - Ghost Protocol",
            "Mission: Impossible - Rogue Nation",
            "Mission: Impossible - Fallout",
            "Nomadland",
            "Parasite",
            "Green Book",
            "The Shape of Water",
            "Moonlight",
            "Spotlight",
            "Birdman or (The Unexpected Virtue of Ignorance)",
            "12 Years a Slave",
            "Argo",
            "The Artist",
            "The King's Speech",
            "The Hurt Locker",
            "Slumdog Millionaire",
            "No Country for Old Men",
            "The Departed",
            "Crash",
            "Million Dollar Baby",
            "The Lord of the Rings: The Return of the King",
            "Chicago",
            "A Beautiful Mind",
            "Gladiator",
            "American Beauty",
            "Shakespeare in Love",
            "Titanic",
            "The English Patient",
            "Braveheart",
            "Forrest Gump",
            "Schindler's List",
            "Unforgiven",
            "The Silence of the Lambs",
            "Dances with Wolves",
            "Driving Miss Daisy",
            "Rain Man",
            "The Last Emperor",
            "Platoon",
            "Out of Africa",
            "Amadeus",
            "Terms of Endearment",
            "Gandhi",
            "Chariots of Fire",
            "Ordinary People",
            "Kramer vs. Kramer",
            "The Deer Hunter",
            "Annie Hall",
            "Rocky",
            "One Flew Over the Cuckoo's Nest",
            "The Godfather Part II",
            "The Sting",
            "The Godfather",
            "The French Connection",
            "Patton",
            "Oliver!",
            "In the Heat of the Night",
            "A Man for All Seasons",
            "The Sound of Music",
            "My Fair Lady",
            "Tom Jones",
            "Lawrence of Arabia",
            "West Side Story",
            "The Apartment",
            "Ben-Hur",
            "Gigi",
            "The Bridge on the River Kwai",
            "Marty",
            "On the Waterfront",
            "From Here to Eternity",
            "The Greatest Show on Earth",
            "An American in Paris",
            "All About Eve",
            "Gentleman's Agreement",
            "The Best Years of Our Lives",
            "The Lost Weekend",
            "Going My Way",
            "Casablanca",
            "Mrs. Miniver",
            "How Green Was My Valley",
            "Rebecca",
            "Gone with the Wind",
            "You Can't Take It with You",
            "The Life of Emile Zola",
            "The Great Ziegfeld",
            "Mutiny on the Bounty",
            "It Happened One Night",
            "Cavalcade",
            "Grand Hotel",
            "Cimarron",
            "All Quiet on the Western Front",
            "The Broadway Melody",
            "Wings",
            "Snow White and the Seven Dwarfs",
            "Pinocchio",
            "Fantasia",
            "The Reluctant Dragon",
            "Dumbo",
            "Bambi",
            "Saludos Amigos",
            "Victory Through Air Power",
            "The Three Caballeros",
            "Make Mine Music",
            "Song of the South",
            "Fun and Fancy Free",
            "Melody Time",
            "So Dear to My Heart",
            "The Adventures of Ichabod and Mr. Toad",
            "Treasure Island",
            "Alice in Wonderland",
            "The Story of Robin Hood and His Merrie Men",
            "Peter Pan",
            "The Sword and the Rose",
            "The Living Desert",
            "Rob Roy, the Highland Rogue",
            "The Vanishing Prairie",
            "20,000 Leagues Under the Sea",
            "Davy Crockett, King of the Wild Frontier",
            "Lady and the Tramp",
            "The African Lion",
            "The Littlest Outlaw",
            "The Great Locomotive Chase",
            "Davy Crockett and the River Pirates",
            "Secrets of Life",
            "Westward Ho the Wagons!",
            "Johnny Tremain",
            "Perri",
            "Old Yeller",
            "The Light in the Forest",
            "White Wilderness",
            "Tonka",
            "Sleeping Beauty",
            "The Shaggy Dog",
            "Third Man on the Mountain",
            "Toby Tyler, or Ten Weeks with a Circus",
            "Kidnapped",
            "Pollyanna",
            "The Sign of Zorro",
            "Jungle Cat",
            "Ten Who Dared",
            "Swiss Family Robinson",
            "One Hundred and One Dalmatians",
            "The Parent Trap",
            "Nikki, Wild Dog of the North",
            "Greyfriars Bobby",
            "Babes in Toyland",
            "Moon Pilot",
            "Bon Voyage",
            "Big Red",
            "Almost Angels",
            "The Legend of Lobo",
            "In Search of the Castaways",
            "Son of Flubber",
            "Miracle of the White Stallions",
            "Savage Sam",
            "Summer Magic",
            "The Incredible Journey",
            "The Sword in the Stone",
            "The Three Lives of Thomasina",
            "The Misadventures of Merlin Jones",
            "A Tiger Walks",
            "The Moon-Spinners",
            "Mary Poppins",
            "Emil and the Detectives",
            "Those Calloways",
            "The Monkey’s Uncle",
            "That Darn Cat!",
            "The Ugly Dachshund",
            "Lt. Robin Crusoe U.S.N.",
            "The Fighting Prince of Donegal",
            "Follow Me, Boys!",
            "Monkeys, Go Home!",
            "The Adventures of Bullwhip Griffin",
            "The Happiest Millionaire",
            "The Gnome-Mobile",
            "The Jungle Book",
            "Charlie, The Lonesome Cougar",
            "Blackbeard’s Ghost",
            "The One and Only, Genuine, Original Family Band",
            "Never a Dull Moment",
            "The Horse in the Gray Flannel Suit",
            "The Love Bug",
            "Smith!",
            "Rascal",
            "The Computer Wore Tennis Shoes",
            "King of the Grizzlies",
            "The Boatniks",
            "The Aristocats",
            "The Wild Country",
            "The Barefoot Executive",
            "Scandalous John",
            "Bedknobs and Broomsticks",
            "The Biscuit Eater",
            "Napoleon and Samantha",
            "Run, Cougar, Run",
            "Snowball Express",
            "The World’s Greatest Athlete",
            "Big and Little Wong Tin Bar",
            "The Love Eterne",
            "Come Drink With Me",
            "The Blade Spares None",
            "The Angry River",
            "A Touch of Zen",
            "Fist of Fury",
            "Hapkido",
            "Game of Death",
            "Stranger from Hong Kong",
            "Enter the Dragon",
            "Facets of Love",
            "Not Scared to Die",
            "Ambush",
            "Fist of Unicorn",
            "Fist to Fist",
            "Supermen Against the Orient",
            "The Young Dragons",
            "All in the Family",
            "New Fist of Fury",
            "Shaolin Wooden Men",
            "Hand of Death",
            "Killer Meteors",
            "The Private Eyes",
            "The 36 Crazy Fists",
            "To Kill with Intrigue",
            "Snake Eyes",
            "Magnificent Bodyguards",
            "Snake in the Eagle's Shadow",
            "Drunken Master",
            "Spiritual Kung Fu",
            "Half a Loaf of Kung Fu",
            "The Fearless Hyena",
            "Dragon Fist",
            "Dance of Death",
            "The Young Master",
            "The Cannonball Run",
            "Dragon Lord",
            "Fantasy Mission Force",
            "Project A",
            "Wheels on Meals",
            "Cannonball Run II",
            "Pom Pom",
            "My Lucky Stars",
            "The Protector",
            "Twinkle, Twinkle, Lucky Stars",
            "Heart of Dragon",
            "Police Story",
            "Armour of God",
            "Naughty Boys",
            "Police Story 2",
            "Dragons Forever",
            "Rouge",
            "Painted Faces",
            "Miracles",
            "I Am Sorry",
            "Who Am I?",
            "Twin Dragons",
            "Thunderbolt",
            "The Shootout",
            "Tempting Heart",
            "Story of Kennedy Town",
            "Rush Hour",
            "Rumble in the Bronx",
            "Mulan",
            "Mr. Nice Guy",
            "Martin",
            "King of Comedy",
            "Polar Bear",
            #start of second addition:
            "Doctor Strange in the Multiverse of Madness",
            "Polar Bear",
            "Better Nate Than Ever",
            "Cheaper by the Dozen",
            "Turning Red",
            "The Ice Age Adventures of Buck Wild",
            "The Last Warrior: A Messenger of Darkness",
            "Diary of a Wimpy Kid",
            "Encanto",
            "Eternals",
            "Shang-Chi and the Legend of the Ten Rings",
            "Jungle Cruise",
            "Black Widow",
            "Luca",
            "Cruella",
            "Raya and the Last Dragon",
            "Flora",
            "The Last Warrior: Root of Evil",
            "Soul",
            "Safety",
            "Godmothered",
            "The One and Only Ivan",
            "Magic Camp",
            "Hamilton",
            "Artemis Fowl",
            "Elephant",
            "Dolphin Reef",
            "Stargirl",
            "Onward",
            "Timmy Failure: Mistakes Were Made",
            "Togo",
            "Noelle",
            "Maleficent: Mistress of Evil",
            "The Lion King",
            "Toy Story 4",
            "Aladdin",
            "Avengers: Endgame",
            "Penguins",
            "Mary Poppins Returns",
            "Ralph Breaks the Internet",
            "The Nutcracker and the Four Realms",
            "Christopher Robin",
            "Incredibles 2",
            "Solo: A Star Wars Story",
            "A Wrinkle in Time",
            "Coco",
            "Cars 3",
            "Pirates of the Caribbean: Dead Men Tell No Tales",
            "Born in China",
            "Dangal",
            "Moana",
            "Queen of Katwe",
            "The Light Between Oceans",
            "Pete’s Dragon",
            "The BFG",
            "Finding Dory",
            "Alice Through the Looking Glass",
            "Zootopia",
            "The Finest Hours",
            "The Good Dinosaur",
            "Bridge of Spies",
            "Schuks! Pay Back the Money!",
            "Inside Out",
            "Tomorrowland",
            "Monkey Kingdom",
            "McFarland, USA",
            "Tinker Bell and the Legend of the NeverBeast",
            "Strange Magic",
            "Into The Woods",
            "Big Hero 6",
            "Alexander and the Terrible, Horrible, No Good, Very Bad Day",
            "The Hundred-Foot Journey",
            "Maleficent",
            "Million Dollar Arm",
            "Bears",
            "Muppets Most Wanted",
            "Need for Speed",
            "The Pirate Fairy",
            "Schuks! Your Country Needs You",
            "Saving Mr. Banks",
            "Frozen",
            "Delivery Man",
            "The Wind Rises",
            "The Fifth Estate",
            "Planes",
            "The Lone Ranger",
            "Monsters University",
            "Wings of Life",
            "Oz the Great and Powerful",
            "Lincoln",
            "Wreck-It Ralph",
            "Frankenweenie")




#series <- c("star_wars")



# 
# #for these movies, I want all of them in each series:
series <- c("the_godfather",
            "star_wars",
            "alien",
            "fast_and_furious",
            "final_destination",
            "friday_the_13th",
            "indiana_jones",
            "the_hunger_games",
            "spider-man",
            "harry_potter",
            "lord_of_the_rings",
            "star_trek",
            "home_alone")

mat1=NULL
mat2=NULL
mat3=NULL
get_data_titles_and_series <- function(mykey,titles,series){
  #this part gets the data for all of my stand-alone titles provided:
  for(i in titles){
    temp_table <- search_by_title(mykey,i,type="movie")
    mat1=rbind(mat1,temp_table)
  }
  #this part gets the data by cycling through each movie from each series provided:
  for(j in series){
    temp_table <- by_search_series(mykey,j,type="movie")
    list_of_titles <- unique(temp_table$Search.Title)
    for(movie_title in list_of_titles){
      table2 <- search_by_title(mykey,movie_title,type="movie")
      mat2=rbind(mat2,table2)
    }
  }
  #this part combines data for both the results for the stand-alone titles and series
  mat3=rbind(mat3,mat1,mat2)
  return(mat3)
}

library(readr)
test<-get_data_titles_and_series(mykey,titles,series)
write_csv(x = test, "movie_data_8_01_2022_v3.csv")



