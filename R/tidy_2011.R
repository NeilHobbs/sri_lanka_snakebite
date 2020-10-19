library(readxl)
library(dplyr)
library(tidyr)

#Import datasets

Snakes2011 = read_excel("Snake_Bite_Data_name_update.xlsx", sheet = "2011")


# These are the rows with data values in them (excluding totals in the final row)
snakes2011_A = Snakes2011[7:478, ]



extract_function = function(row, age, quarter, sex, outcome){
  X = data.frame(snakes$...3, row)%>% #takes the row of the dataframe (colum of cases: dead or discharge) and the hospital (...3)
    mutate(Age = age)%>% #which has an age
    mutate(Quarter = quarter)%>% # in the quarter (1, 2, 3, or 4)
    mutate(Sex = sex)%>% #Male or Female
    mutate(Outcome = outcome) #deaths or live.discharge
  
  return(X) #returns the new dataframe in the tidier format
}

snakes = snakes2011_A  #update year for each extraction. Should really be done as a loop


df1 = extract_function(row = snakes$...4,
                       age = "<1", 
                       quarter = 1,
                       sex = "Male", 
                       outcome = "live.discharge")

df2 = extract_function(row = snakes$...5,
                       age = "1-4", 
                       quarter = 1,
                       sex = "Male", 
                       outcome = "live.discharge")

df3 = extract_function(row = snakes$...6,
                       age = "5-16", 
                       quarter = 1,
                       sex = "Male", 
                       outcome = "live.discharge")

df4 = extract_function(row = snakes$...7,
                       age = "17-49", 
                       quarter = 1,
                       sex = "Male", 
                       outcome = "live.discharge")

df5 = extract_function(row = snakes$...8,
                       age = "50-69", 
                       quarter = 1,
                       sex = "Male", 
                       outcome = "live.discharge")

df6 = extract_function(row = snakes$...9,
                       age = "70+", 
                       quarter = 1,
                       sex = "Male", 
                       outcome = "live.discharge")

df7  = extract_function(row = snakes$...10,
                        age = "NAv", 
                        quarter = 1,
                        sex = "Male", 
                        outcome = "live.discharge")

#____________________________________________________________#
df8 = extract_function(row = snakes$...11,
                       age = "<1", 
                       quarter = 1,
                       sex = "Female", 
                       outcome = "live.discharge")

df9 = extract_function(row = snakes$...12,
                       age = "1-4", 
                       quarter = 1,
                       sex = "Female", 
                       outcome = "live.discharge")

df10 = extract_function(row = snakes$...13,
                        age = "5-16", 
                        quarter = 1,
                        sex = "Female", 
                        outcome = "live.discharge")

df11 = extract_function(row = snakes$...14,
                        age = "17-49", 
                        quarter = 1,
                        sex = "Female", 
                        outcome = "live.discharge")

df12 = extract_function(row = snakes$...15,
                        age = "50-69", 
                        quarter = 1,
                        sex = "Female", 
                        outcome = "live.discharge")

df13 = extract_function(row = snakes$...16,
                        age = "70+", 
                        quarter = 1,
                        sex = "Female", 
                        outcome = "live.discharge")

df14  = extract_function(row = snakes$...17,
                         age = "NAv", 
                         quarter = 1,
                         sex = "Female", 
                         outcome = "live.discharge")
#_________________________________________________________#
df15 = extract_function(row = snakes$...18,
                        age = "<1", 
                        quarter = 1,
                        sex = "Male", 
                        outcome = "death")

df16 = extract_function(row = snakes$...19,
                        age = "1-4", 
                        quarter = 1,
                        sex = "Male", 
                        outcome = "death")

df17 = extract_function(row = snakes$...20,
                        age = "5-16", 
                        quarter = 1,
                        sex = "Male", 
                        outcome = "death")

df18 = extract_function(row = snakes$...21,
                        age = "17-49", 
                        quarter = 1,
                        sex = "Male", 
                        outcome = "death")

df19 = extract_function(row = snakes$...22,
                        age = "50-69", 
                        quarter = 1,
                        sex = "Male", 
                        outcome = "death")

df20 = extract_function(row = snakes$...23,
                        age = "70+", 
                        quarter = 1,
                        sex = "Male", 
                        outcome = "death")

df21  = extract_function(row = snakes$...24,
                         age = "NAv", 
                         quarter = 1,
                         sex = "Male", 
                         outcome = "death")

#____________________________________________________________#
df22 = extract_function(row = snakes$...25,
                        age = "<1", 
                        quarter = 1,
                        sex = "Female", 
                        outcome = "death")

df23 = extract_function(row = snakes$...26,
                        age = "1-4", 
                        quarter = 1,
                        sex = "Female", 
                        outcome = "death")

df24 = extract_function(row = snakes$...27,
                        age = "5-16", 
                        quarter = 1,
                        sex = "Female", 
                        outcome = "death")

df25 = extract_function(row = snakes$...28,
                        age = "17-49", 
                        quarter = 1,
                        sex = "Female", 
                        outcome = "death")

df26= extract_function(row = snakes$...29,
                       age = "50-69", 
                       quarter = 1,
                       sex = "Female", 
                       outcome = "death")

df27 = extract_function(row = snakes$...30,
                        age = "70+", 
                        quarter = 1,
                        sex = "Female", 
                        outcome = "death")

df28  = extract_function(row = snakes$...31,
                         age = "NAv", 
                         quarter = 1,
                         sex = "Female", 
                         outcome = "death")
#_______________________________________________#
df29 = extract_function(row = snakes$...32,
                        age = "<1", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "live.discharge")

df30 = extract_function(row = snakes$...33,
                        age = "1-4", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "live.discharge")

df31 = extract_function(row = snakes$...34,
                        age = "5-16", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "live.discharge")

df32 = extract_function(row = snakes$...35,
                        age = "17-49", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "live.discharge")

df33 = extract_function(row = snakes$...36,
                        age = "50-69", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "live.discharge")

df34 = extract_function(row = snakes$...37,
                        age = "70+", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "live.discharge")

df35  = extract_function(row = snakes$...38,
                         age = "NAv", 
                         quarter = 2,
                         sex = "Male", 
                         outcome = "live.discharge")

#____________________________________________________________#
df36 = extract_function(row = snakes$...39,
                        age = "<1", 
                        quarter = 2,
                        sex = "Female", 
                        outcome = "live.discharge")

df37 = extract_function(row = snakes$...40,
                        age = "1-4", 
                        quarter = 2,
                        sex = "Female", 
                        outcome = "live.discharge")

df38 = extract_function(row = snakes$...41,
                        age = "5-16", 
                        quarter = 2,
                        sex = "Female", 
                        outcome = "live.discharge")

df39 = extract_function(row = snakes$...42,
                        age = "17-49", 
                        quarter = 2,
                        sex = "Female", 
                        outcome = "live.discharge")

df40 = extract_function(row = snakes$...43,
                        age = "50-69", 
                        quarter = 2,
                        sex = "Female", 
                        outcome = "live.discharge")

df41 = extract_function(row = snakes$...44,
                        age = "70+", 
                        quarter = 2,
                        sex = "Female", 
                        outcome = "live.discharge")

df42  = extract_function(row = snakes$...45,
                         age = "NAv", 
                         quarter = 2,
                         sex = "Female", 
                         outcome = "live.discharge")
#_________________________________________________________#
df43 = extract_function(row = snakes$...46,
                        age = "<1", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "death")

df44 = extract_function(row = snakes$...47,
                        age = "1-4", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "death")

df45 = extract_function(row = snakes$...48,
                        age = "5-16", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "death")

df46 = extract_function(row = snakes$...49,
                        age = "17-49", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "death")

df47 = extract_function(row = snakes$...50,
                        age = "50-69", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "death")

df48 = extract_function(row = snakes$...51,
                        age = "70+", 
                        quarter = 2,
                        sex = "Male", 
                        outcome = "death")

df49  = extract_function(row = snakes$...52,
                         age = "NAv", 
                         quarter = 2,
                         sex = "Male", 
                         outcome = "death")

#____________________________________________________________#
df50 = extract_function(row = snakes$...53,
                        age = "<1", 
                        quarter = 2,
                        sex = "Female", 
                        outcome = "death")

df51 = extract_function(row = snakes$...54,
                        age = "1-4", 
                        quarter = 2,
                        sex = "Female", 
                        outcome = "death")

df52 = extract_function(row = snakes$...55,
                        age = "5-16", 
                        quarter = 2,
                        sex = "Female", 
                        outcome = "death")

df53= extract_function(row = snakes$...56,
                       age = "17-49", 
                       quarter = 2,
                       sex = "Female", 
                       outcome = "death")

df54= extract_function(row = snakes$...57,
                       age = "50-69", 
                       quarter = 2,
                       sex = "Female", 
                       outcome = "death")

df55 = extract_function(row = snakes$...58,
                        age = "70+", 
                        quarter = 2,
                        sex = "Female", 
                        outcome = "death")

df56  = extract_function(row = snakes$...59,
                         age = "NAv", 
                         quarter = 2,
                         sex = "Female", 
                         outcome = "death")
#__________________________________________________#


df57 = extract_function(row = snakes$...60,
                        age = "<1", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "live.discharge")

df58 = extract_function(row = snakes$...61,
                        age = "1-4", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "live.discharge")

df59 = extract_function(row = snakes$...62,
                        age = "5-16", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "live.discharge")

df60 = extract_function(row = snakes$...63,
                        age = "17-49", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "live.discharge")

df61 = extract_function(row = snakes$...64,
                        age = "50-69", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "live.discharge")

df62 = extract_function(row = snakes$...65,
                        age = "70+", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "live.discharge")

df63  = extract_function(row = snakes$...66,
                         age = "NAv", 
                         quarter = 3,
                         sex = "Male", 
                         outcome = "live.discharge")

#____________________________________________________________#
df64 = extract_function(row = snakes$...67,
                        age = "<1", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "live.discharge")

df65 = extract_function(row = snakes$...68,
                        age = "1-4", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "live.discharge")

df66 = extract_function(row = snakes$...69,
                        age = "5-16", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "live.discharge")

df67 = extract_function(row = snakes$...70,
                        age = "17-49", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "live.discharge")

df68 = extract_function(row = snakes$...71,
                        age = "50-69", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "live.discharge")

df69 = extract_function(row = snakes$...72,
                        age = "70+", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "live.discharge")

df70  = extract_function(row = snakes$...73,
                         age = "NAv", 
                         quarter = 3,
                         sex = "Female", 
                         outcome = "live.discharge")
#_________________________________________________________#
df71 = extract_function(row = snakes$...74,
                        age = "<1", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "death")

df72 = extract_function(row = snakes$...75,
                        age = "1-4", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "death")

df73 = extract_function(row = snakes$...76,
                        age = "5-16", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "death")

df74 = extract_function(row = snakes$...77,
                        age = "17-49", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "death")

df75 = extract_function(row = snakes$...78,
                        age = "50-69", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "death")

df76 = extract_function(row = snakes$...79,
                        age = "70+", 
                        quarter = 3,
                        sex = "Male", 
                        outcome = "death")

df77  = extract_function(row = snakes$...80,
                         age = "NAv", 
                         quarter = 3,
                         sex = "Male", 
                         outcome = "death")

#____________________________________________________________#
df78 = extract_function(row = snakes$...81,
                        age = "<1", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "death")

df79 = extract_function(row = snakes$...82,
                        age = "1-4", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "death")

df80 = extract_function(row = snakes$...83,
                        age = "5-16", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "death")

df81 = extract_function(row = snakes$...84,
                        age = "17-49", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "death")

df82= extract_function(row = snakes$...85,
                       age = "50-69", 
                       quarter = 3,
                       sex = "Female", 
                       outcome = "death")

df83 = extract_function(row = snakes$...86,
                        age = "70+", 
                        quarter = 3,
                        sex = "Female", 
                        outcome = "death")

df84  = extract_function(row = snakes$...87,
                         age = "NAv", 
                         quarter = 3,
                         sex = "Female", 
                         outcome = "death")
#_______________________________________________#
df85 = extract_function(row = snakes$...88,
                        age = "<1", 
                        quarter = 4,
                        sex = "Male", 
                        outcome = "live.discharge")

df86 = extract_function(row = snakes$...89,
                        age = "1-4", 
                        quarter = 4,
                        sex = "Male", 
                        outcome = "live.discharge")

df87 = extract_function(row = snakes$...90,
                        age = "5-16", 
                        quarter = 4,
                        sex = "Male", 
                        outcome = "live.discharge")

df88 = extract_function(row = snakes$...91,
                        age = "17-49", 
                        quarter = 4,
                        sex = "Male", 
                        outcome = "live.discharge")

df89 = extract_function(row = snakes$...92,
                        age = "50-69", 
                        quarter = 4,
                        sex = "Male", 
                        outcome = "live.discharge")

df90 = extract_function(row = snakes$...93,
                        age = "70+", 
                        quarter = 4,
                        sex = "Male", 
                        outcome = "live.discharge")

df91  = extract_function(row = snakes$...94,
                         age = "NAv", 
                         quarter = 4,
                         sex = "Male", 
                         outcome = "live.discharge")

#____________________________________________________________#
df92 = extract_function(row = snakes$...95,
                        age = "<1", 
                        quarter = 4,
                        sex = "Female", 
                        outcome = "live.discharge")

df93 = extract_function(row = snakes$...96,
                        age = "1-4", 
                        quarter = 4,
                        sex = "Female", 
                        outcome = "live.discharge")

df94 = extract_function(row = snakes$...97,
                        age = "5-16", 
                        quarter = 4,
                        sex = "Female", 
                        outcome = "live.discharge")

df95 = extract_function(row = snakes$...98,
                        age = "17-49", 
                        quarter = 4,
                        sex = "Female", 
                        outcome = "live.discharge")

df96 = extract_function(row = snakes$...99,
                        age = "50-69", 
                        quarter = 4,
                        sex = "Female", 
                        outcome = "live.discharge")

df97 = extract_function(row = snakes$...100,
                        age = "70+", 
                        quarter = 4,
                        sex = "Female", 
                        outcome = "live.discharge")

df98  = extract_function(row = snakes$...101,
                         age = "NAv", 
                         quarter = 4,
                         sex = "Female", 
                         outcome = "live.discharge")
#_________________________________________________________#
df99 = extract_function(row = snakes$...102,
                        age = "<1", 
                        quarter = 4,
                        sex = "Male", 
                        outcome = "death")

df100 = extract_function(row = snakes$...103,
                         age = "1-4", 
                         quarter = 4,
                         sex = "Male", 
                         outcome = "death")

df101 = extract_function(row = snakes$...104,
                         age = "5-16", 
                         quarter = 4,
                         sex = "Male", 
                         outcome = "death")

df102 = extract_function(row = snakes$...105,
                         age = "17-49", 
                         quarter = 4,
                         sex = "Male", 
                         outcome = "death")

df103 = extract_function(row = snakes$...106,
                         age = "50-69", 
                         quarter = 4,
                         sex = "Male", 
                         outcome = "death")

df104 = extract_function(row = snakes$...107,
                         age = "70+", 
                         quarter = 4,
                         sex = "Male", 
                         outcome = "death")

df105  = extract_function(row = snakes$...108,
                          age = "NAv", 
                          quarter = 4,
                          sex = "Male", 
                          outcome = "death")

#____________________________________________________________#
df106 = extract_function(row = snakes$...109,
                         age = "<1", 
                         quarter = 4,
                         sex = "Female", 
                         outcome = "death")

df107 = extract_function(row = snakes$...110,
                         age = "1-4", 
                         quarter = 4,
                         sex = "Female", 
                         outcome = "death")

df108 = extract_function(row = snakes$...111,
                         age = "5-16", 
                         quarter = 4,
                         sex = "Female", 
                         outcome = "death")

df109= extract_function(row = snakes$...112,
                        age = "17-49", 
                        quarter = 4,
                        sex = "Female", 
                        outcome = "death")

df110= extract_function(row = snakes$...113,
                        age = "50-69", 
                        quarter = 4,
                        sex = "Female", 
                        outcome = "death")

df111 = extract_function(row = snakes$...114,
                         age = "70+", 
                         quarter = 4,
                         sex = "Female", 
                         outcome = "death")

df112  = extract_function(row = snakes$...115,
                          age = "NAv", 
                          quarter = 4,
                          sex = "Female", 
                          outcome = "death")

df = rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
           df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
           df21, df22, df23, df24, df25, df26, df27, df28, df29, df30,
           df31, df32, df33, df34, df35, df36, df37, df38, df39, df40,
           df41, df42, df43, df44, df45, df46, df47, df48, df49, df50,
           df51, df52, df53, df54, df55, df56, df57, df58, df59, df60,
           df61, df62, df63, df64, df65, df66, df67, df68, df69, df70,
           df71, df72, df73, df74, df75, df76, df77, df78, df79, df80,
           df81, df82, df83, df84, df85, df86, df87, df88, df89, df90,
           df91, df92, df93, df94, df95, df96, df97, df98, df99, df100,
           df101, df102, df103, df104, df105, df106, df107, df108, df109, df110,
           df111, df112)


snakes_tidy = df%>% #update snakes year
  mutate(Year = 2011)%>% #update year
  rename(Count = row)%>% 
  rename(Hospital = snakes....3)

write.csv(snakes_tidy, ".\\snakes2011.csv") #update year





















