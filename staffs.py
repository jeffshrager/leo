import random

staffs=[
    {"Name":"Beginner_Staff","Rarity":"1","Spellcasting_Level_Required":"0","Max_Spell_Lvl":"1","Casting_Speed_Multiplier":"0.5","Casting_Damage_Multiplier":"0.5","Casting_Effect_Multiplier":"0.5","Casting_Area_Multiplier":"0.5","Mana_Consumption_Multiplier":"2"},
    {"Name":"Staff","Rarity":"1","Spellcasting_Level_Required":"1","Max_Spell_Lvl":"1","Casting_Speed_Multiplier":"0.8","Casting_Damage_Multiplier":"0.8","Casting_Effect_Multiplier":"0.8","Casting_Area_Multiplier":"0.8","Mana_Consumption_Multiplier":"1.5"},
    {"Name":"Advanced_Staff","Rarity":"2","Spellcasting_Level_Required":"3","Max_Spell_Lvl":"2","Casting_Speed_Multiplier":"1","Casting_Damage_Multiplier":"1","Casting_Effect_Multiplier":"1","Casting_Area_Multiplier":"1","Mana_Consumption_Multiplier":"1"},
    {"Name":"Energy_Staff","Rarity":"2","Spellcasting_Level_Required":"5","Max_Spell_Lvl":"2","Casting_Speed_Multiplier":"3","Casting_Damage_Multiplier":"0.2","Casting_Effect_Multiplier":"0.5","Casting_Area_Multiplier":"1","Mana_Consumption_Multiplier":"0.5"},
    {"Name":"Staff_of_Regeneration","Rarity":"2","Spellcasting_Level_Required":"5","Max_Spell_Lvl":"2","Casting_Speed_Multiplier":"1","Casting_Damage_Multiplier":"1","Casting_Effect_Multiplier":"2.5","Casting_Area_Multiplier":"1","Mana_Consumption_Multiplier":"1.5"},
    {"Name":"Staff_of_Power","Rarity":"2","Spellcasting_Level_Required":"5","Max_Spell_Lvl":"2","Casting_Speed_Multiplier":"0.3","Casting_Damage_Multiplier":"3","Casting_Effect_Multiplier":"1","Casting_Area_Multiplier":"0.5","Mana_Consumption_Multiplier":"2"},
    {"Name":"Staff_of_the_Ocean","Rarity":"2","Spellcasting_Level_Required":"5","Max_Spell_Lvl":"2","Casting_Speed_Multiplier":"0.5","Casting_Damage_Multiplier":"1","Casting_Effect_Multiplier":"1.5","Casting_Area_Multiplier":"3","Mana_Consumption_Multiplier":"2"},
    {"Name":"_Master's_Staff","Rarity":"3","Spellcasting_Level_Required":"8","Max_Spell_Lvl":"3","Casting_Speed_Multiplier":"1.5","Casting_Damage_Multiplier":"1.5","Casting_Effect_Multiplier":"1.5","Casting_Area_Multiplier":"1.5","Mana_Consumption_Multiplier":"0.7"},
    {"Name":"Staff_of_Criticals","Rarity":"3","Spellcasting_Level_Required":"10","Max_Spell_Lvl":"3","Casting_Speed_Multiplier":"0.8","Casting_Damage_Multiplier":"4","Casting_Effect_Multiplier":"1","Casting_Area_Multiplier":"0.5","Mana_Consumption_Multiplier":"1"},
    {"Name":"Crystalline_Staff","Rarity":"3","Spellcasting_Level_Required":"10","Max_Spell_Lvl":"3","Casting_Speed_Multiplier":"5","Casting_Damage_Multiplier":"0","Casting_Effect_Multiplier":"1","Casting_Area_Multiplier":"1","Mana_Consumption_Multiplier":"0.1"},
    {"Name":"King's_Staff","Rarity":"3","Spellcasting_Level_Required":"10","Max_Spell_Lvl":"3","Casting_Speed_Multiplier":"1","Casting_Damage_Multiplier":"2","Casting_Effect_Multiplier":"2","Casting_Area_Multiplier":"1","Mana_Consumption_Multiplier":"0.8"},
    {"Name":"Elemental_Staff","Rarity":"3","Spellcasting_Level_Required":"10","Max_Spell_Lvl":"3","Casting_Speed_Multiplier":"1","Casting_Damage_Multiplier":"1","Casting_Effect_Multiplier":"3","Casting_Area_Multiplier":"1","Mana_Consumption_Multiplier":"1"},
    {"Name":"Wizard's_Staff","Rarity":"4","Spellcasting_Level_Required":"12","Max_Spell_Lvl":"4","Casting_Speed_Multiplier":"2","Casting_Damage_Multiplier":"2","Casting_Effect_Multiplier":"2","Casting_Area_Multiplier":"2","Mana_Consumption_Multiplier":"0.4"},
    {"Name":"Emerald_Staff","Rarity":"4","Spellcasting_Level_Required":"15","Max_Spell_Lvl":"4","Casting_Speed_Multiplier":"1","Casting_Damage_Multiplier":"6","Casting_Effect_Multiplier":"1","Casting_Area_Multiplier":"1","Mana_Consumption_Multiplier":"1"},
    {"Name":"Staff_of_Shielding","Rarity":"4","Spellcasting_Level_Required":"15","Max_Spell_Lvl":"4","Casting_Speed_Multiplier":"0.3","Casting_Damage_Multiplier":"2","Casting_Effect_Multiplier":"6","Casting_Area_Multiplier":"2","Mana_Consumption_Multiplier":"0.5"},
    {"Name":"Staff_of_the_Wolf","Rarity":"4","Spellcasting_Level_Required":"15","SLR_Mod":"W4","Max_Spell_Lvl":"4","Casting_Speed_Multiplier":"2","Casting_Damage_Multiplier":"2","Casting_Effect_Multiplier":"1.5","Casting_Area_Multiplier":"1.5","Mana_Consumption_Multiplier":"0.5"},
    {"Name":"Staff_of_the_Spider","Rarity":"4","Spellcasting_Level_Required":"15","SLR_Mod":"S4","Max_Spell_Lvl":"4","Casting_Speed_Multiplier":"2","Casting_Damage_Multiplier":"2","Casting_Effect_Multiplier":"1.5","Casting_Area_Multiplier":"1.5","Mana_Consumption_Multiplier":"0.5"},
    {"Name":"Staff_of_the_Zombie","Rarity":"4","Spellcasting_Level_Required":"15","SLR_Mod":"Z4","Max_Spell_Lvl":"4","Casting_Speed_Multiplier":"2","Casting_Damage_Multiplier":"2","Casting_Effect_Multiplier":"1.5","Casting_Area_Multiplier":"1.5","Mana_Consumption_Multiplier":"0.5"},
    {"Name":"Staff_of_Entanglement","Rarity":"4","Spellcasting_Level_Required":"15","Max_Spell_Lvl":"4","Casting_Speed_Multiplier":"1","Casting_Damage_Multiplier":"1","Casting_Effect_Multiplier":"2","Casting_Area_Multiplier":"5","Mana_Consumption_Multiplier":"2"},
    {"Name":"Godly_Staff","Rarity":"5","Spellcasting_Level_Required":"20","Max_Spell_Lvl":"5","Casting_Speed_Multiplier":"3","Casting_Damage_Multiplier":"3","Casting_Effect_Multiplier":"3","Casting_Area_Multiplier":"3","Mana_Consumption_Multiplier":"0.2"},
    {"Name":"Necron's_Staff","Rarity":"5","Spellcasting_Level_Required":"22","Max_Spell_Lvl":"5","Casting_Speed_Multiplier":"0.5","Casting_Damage_Multiplier":"15","Casting_Effect_Multiplier":"0.5","Casting_Area_Multiplier":"0.5","Mana_Consumption_Multiplier":"2"},
    {"Name":"Staff_of_Purity","Rarity":"5","Spellcasting_Level_Required":"22","Max_Spell_Lvl":"5","Casting_Speed_Multiplier":"1","Casting_Damage_Multiplier":"1","Casting_Effect_Multiplier":"1","Casting_Area_Multiplier":"1","Mana_Consumption_Multiplier":"1"},
    {"Name":"Void_Staff","Rarity":"5","Spellcasting_Level_Required":"22","Max_Spell_Lvl":"5","Casting_Speed_Multiplier":"5","Casting_Damage_Multiplier":"1.5","Casting_Effect_Multiplier":"3","Casting_Area_Multiplier":"2","Mana_Consumption_Multiplier":"0.4"}
]


spells=[
    {"Name":"Reincarnate","Level":"3","Casting_Speed":"60","Casting_damage":"0","C_d_lmod":"0","Casting_Effect":"100","Casting_Area":"1","Mana":"150","Type":"Life"},
    {"Name":"Snare","Level":"1","Casting_Speed":"10","Casting_damage":"250","C_d_lmod":"25","Casting_Effect":"40","Casting_Area":"3","Mana":"50","Type":"Life"},
    {"Name":"Stone_Pillars","Level":"4","Casting_Speed":"20","Casting_damage":"1000","C_d_lmod":"0","Casting_Effect":"40","Casting_Area":"5","Mana":"200","Type":"Earth"},
    {"Name":"Thunder_Step","Level":"2","Casting_Speed":"45","Casting_damage":"100","C_d_lmod":"0","Casting_Effect":"50","Casting_Area":"2","Mana":"100","Type":"Air"},
    {"Name":"Blinding_Sound","Level":"4","Casting_Speed":"60","Casting_damage":"3000","C_d_lmod":"500","Casting_Effect":"150","Casting_Area":"1","Mana":"250","Type":"Dark"},
    {"Name":"Recreation","Level":"5","Casting_Speed":"600","Casting_damage":"0","C_d_lmod":"0","Casting_Effect":"400","Casting_Area":"1","Mana":"All","Type":"Life"},
    {"Name":"Whirlwind","Level":"4","Casting_Speed":"30","Casting_damage":"700","C_d_lmod":"0","Casting_Effect":"100","Casting_Area":"4","Mana":"120","Type":"Air"},
    {"Name":"Create_Fire","Level":"1","Casting_Speed":"3","Casting_damage":"50","C_d_lmod":"10","Casting_Effect":"20","Casting_Area":"1","Mana":"20","Type":"Fire"},
    {"Name":"Skill_Empowerment","Level":"3","Casting_Speed":"50","Casting_damage":"0","C_d_lmod":"0","Casting_Effect":"150","Casting_Area":"1","Mana":"100","Type":"Energy"},
    {"Name":"Greater_Barracade","Level":"4","Casting_Speed":"60","Casting_damage":"2000","C_d_lmod":"0","Casting_Effect":"200","Casting_Area":"4","Mana":"300","Type":"Earth"},
    {"Name":"Bug_Defenders","Level":"1","Casting_Speed":"20","Casting_damage":"20","C_d_lmod":"0","Casting_Effect":"20","Casting_Area":"1","Mana":"40","Type":"Life"},
    {"Name":"Change_Gravity","Level":"4","Casting_Speed":"120","Casting_damage":"0","C_d_lmod":"0","Casting_Effect":"250","Casting_Area":"5","Mana":"250","Type":"Energy"},
    {"Name":"Barracade","Level":"2","Casting_Speed":"30","Casting_damage":"0","C_d_lmod":"0","Casting_Effect":"75","Casting_Area":"3","Mana":"100","Type":"Earth"},
    {"Name":"Ray_of_Weakening","Level":"2","Casting_Speed":"20","Casting_damage":"500","C_d_lmod":"0","Casting_Effect":"100","Casting_Area":"1","Mana":"50","Type":"Dark"}
]

r = []

def run():
    global r
    for staff in staffs:
        for spell in spells:
            r = r + [{"spell":spell['Name'],"staff":staff['Name'],"score":modulate(spell,staff)}]
    for entry in sorted(r, key=lambda entry: entry['score']) :
        print('{spelln} x {staffn} = {score}'.format(spelln=entry['spell'],staffn=entry['staff'],score=entry['score']))
    
def modulate(spell,staff):
    return float(spell["Casting_Speed"])*float(staff["Casting_Speed_Multiplier"])+float(spell["Casting_damage"])*float(staff["Casting_Damage_Multiplier"])+float(spell["Casting_Effect"])*float(staff["Casting_Effect_Multiplier"])+float(spell["Casting_Area"])*float(staff["Casting_Area_Multiplier"])

verbs = ["Dancing", "Flying", "Winning", "Dodging", "Master", "Walking", "Super", "Running", "Leaping", "King", "Sleeping",
         "Cute", "Screaming","Whining","Angry"]
nouns = ["Sheep", "Dog", "Cat", "Snuffy", "Cow", "Alligator", "Ada", "Pig", "Eel", "Squid", "Bacteria", "Dragon", "Ghast", "Leo"]

def namegen():
    verb = verbs[random.randint(0,len(verbs)-1)]
    noun = nouns[random.randint(0,len(nouns)-1)]
    if 0!=random.randint(0,4):
        return verb+noun+str(random.randint(0,999)+1)
    else:
        return verb+noun

# run()

for i in range(100):
    print(namegen())
    
