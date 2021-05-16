open System
open System.Collections.Generic
open System.Text.RegularExpressions


let inline tryGetValue (d : ^T) key =
    let mutable output = null
    let parsed = ( ^T : (member TryGetValue  : string * byref< _ > -> bool ) (d, key, &output) )
    match parsed with
    | true -> Some output
    | _ -> None

module GameFiles =
    let (/) path1 path2 = IO.Path.Combine(path1,path2)
    let modBasePath =   __SOURCE_DIRECTORY__ / "STA3_UPRISING"
    let gameInfoPath = IO.DirectoryInfo <| modBasePath  / "Gameinfo"
    let englishPath = IO.FileInfo <| modBasePath  / "String" / "English.str"
    let galaxyScenarioDef = IO.FileInfo <| modBasePath / "Gameinfo" / "GalaxyScenarioDef.galaxyScenarioDef"

    let gameplayConstants =  IO.FileInfo <| gameInfoPath.FullName / "Gameplay.constants"

    let englishDict =
        let stringInfoParser = ".*ID\s+\"(?<ID>.*)\"(\n|\r|\r\n).*Value\s+\"(?<Value>.*)\""
        let englishText = englishPath.FullName |> IO.File.ReadAllText

        let matches = Regex.Matches(englishText, stringInfoParser,RegexOptions.Multiline)

        matches
        |> Seq.map(fun m -> 
            m.Groups.["ID"].Value, m.Groups.["Value"].Value)
        |> dict
        |> fun d -> Dictionary<string,string>(d,StringComparer.OrdinalIgnoreCase)

    
    let tryGetEnglishValue key = tryGetValue englishDict key

    let planetItemTypeDict =
        let infoParser = "planetItemType\s+designName\s+\"(?<designName>.*)\"\s+entityDefName\s+\"(?<entityDefName>.*)\""
        let galaxyScenarioDefText = galaxyScenarioDef.FullName |> IO.File.ReadAllText

        let matches = Regex.Matches(galaxyScenarioDefText, infoParser)

        matches
        |> Seq.map(fun m -> 
            m.Groups.["designName"].Value, m.Groups.["entityDefName"].Value )
        |> dict
        |> fun d -> Dictionary<string,string>(d,StringComparer.OrdinalIgnoreCase)


    let tryGetplanetItemType entityDefName =
        planetItemTypeDict
        |> Seq.map (|KeyValue|)
        |> Seq.filter(fun  (k,v) -> v = entityDefName)

let damagePercentageBonus =
    let damagePercentageParser = "DamagePercentBonus\:(?<AttackType>\w+)\:(?<ArmorType>\w+)\s(?<Percentage>\d*\.?\d*)"
    let text = GameFiles.gameplayConstants.FullName |> IO.File.ReadAllText

    let matches =  Regex.Matches(text, damagePercentageParser ,RegexOptions.Multiline)
    let dict = new Dictionary<string, Dictionary<string,decimal>>()
    matches
    |> Seq.map(fun m -> 
        m.Groups.["AttackType"].Value, m.Groups.["ArmorType"].Value, decimal m.Groups.["Percentage"].Value )
    |> Seq.iter(fun (atk, armor, percentage) ->
        match dict.TryGetValue(atk) with
        | (true, v) -> 
           v.[armor] <- percentage
        | _ ->
            dict.[atk] <- new Dictionary<string,decimal>()
            dict.[atk].[armor] <- percentage
    )
    dict

let percentageIncrease (increase :float) (original : float) =
    (increase * original) + original

let percentageDecrease (increase :float) (original : float) =
    original - (increase * original)

let damagePerSecond weaponDamageIncreasePerc (damagePerBank : float list) weaponCooldownDecreasePerc (preBuffCooldownTime : float) =
    printfn $"weaponDamageIncreasePerc : {weaponDamageIncreasePerc}, damagePerBank {damagePerBank}, weaponCooldownDecreasePerc {weaponCooldownDecreasePerc}, preBuffCooldownTime {preBuffCooldownTime}"
    let damageSum = percentageIncrease weaponDamageIncreasePerc  (damagePerBank |> List.sum) 
    damageSum / (percentageDecrease weaponCooldownDecreasePerc preBuffCooldownTime)

let selectOne (key : string) (lines : string array) =
    lines
    |> Array.tryFind(fun s -> s.Trim().StartsWith(key,StringComparison.InvariantCultureIgnoreCase))
    |> Option.map(fun s -> s.Replace(key,"",StringComparison.InvariantCultureIgnoreCase).Trim().Trim('\"').Trim())

let tryFloat (s : string) =
    match System.Double.TryParse(s) with
    |(true,v) -> Some v
    | _ -> None

let selectOneFloat (key : string) (lines : string array) =
    selectOne key lines
    |> Option.bind tryFloat

let selectMany (key : string) (lines : string array) =
    lines
    |> Array.filter(fun s -> s.Trim().StartsWith(key,StringComparison.InvariantCultureIgnoreCase))
    |> Array.map(fun s -> s.Replace(key,"",StringComparison.InvariantCultureIgnoreCase).Trim().Trim('\"').Trim())
    |> Array.toList

type GameInfoFile = {
    fileName : IO.FileInfo
    lines : string array
    entityType : string option
    nameString : string option
    descString : string option
    armorType : string option
    attackTypes : string list
} with
    static member Create(file, lines) =
        {
            fileName = file
            lines = lines
            entityType = selectOne "entityType" lines
            nameString = selectOne "NameStringID" lines
            descString = selectOne "descStringID" lines            
            armorType = selectOne "armorType" lines
            attackTypes = selectMany "AttackType" lines
        }
    member x.GetEnglishName = 
        x.nameString 
        |> Option.bind (GameFiles.tryGetEnglishValue) 
        |> Option.defaultValue x.fileName.FullName
    member x.GetEnglishDesc = 
        x.descString 
        |> Option.bind (GameFiles.tryGetEnglishValue)  
        |> Option.defaultValue x.fileName.FullName
    member x.isBorg = x.fileName.Name.StartsWith("B_")
    member x.isKlingon = x.fileName.Name.StartsWith("K_")
    member x.isFederation = x.fileName.Name.StartsWith("F_")
    member x.isCardassian = x.fileName.Name.StartsWith("C_")
    member x.isDominion = x.fileName.Name.StartsWith("D_")
    member x.isRomulan  = x.fileName.Name.StartsWith("R_")



let readFile (f : IO.FileInfo) =
    GameInfoFile.Create(f, IO.File.ReadAllLines f.FullName)


let shipEntityTypes = [
    "Frigate"
    "CapitalShip"
    "Titan"
    "Fighter"
    // "EntryVehicle"
]

let isShip(entityType : string) =
    shipEntityTypes |> Seq.contains entityType

let isShipO(entityType : string option) =
    entityType
    |> Option.exists(isShip)

let gameInfos = 
    GameFiles.gameInfoPath.EnumerateFiles("*.entity")
    |> Seq.map(fun f -> (readFile f))
    |> Seq.toList



let attackTypesToArmorType = dict [
    "CAPITALSHIP", "CapitalShip"
    "ANTIHEAVY", "Heavy"
    "ANTIMEDIUM", "Medium"
    "COMPOSITE", ""
    "ANTIMODULE",  "Module"
    "ANTIVERYLIGHT", "VeryLight"
    "ANTIVERYHEAVY", "VeryHeavy"
    "CORVETTE", ""
    "ANTILIGHT", "Light"
    "TITAN", "Titan"
]


let groupByWeakShips ships defendingShipsPredicate (attackingShip : GameInfoFile)  =

    let strongAgainst =
        attackingShip.attackTypes
        |> List.collect(fun at -> 
            let armorType = attackTypesToArmorType.[at]
            ships
            |> List.filter(defendingShipsPredicate)
            |> List.filter(fun s -> s.armorType = Some armorType) 
        )
        |> List.distinctBy(fun s -> s.fileName.FullName)
    attackingShip, strongAgainst

let strongAgainstReport attackingShipsPredicate defendingShipsPredicate (ships : GameInfoFile list) =
    ships
    |> List.filter(attackingShipsPredicate)
    |> List.map(groupByWeakShips ships defendingShipsPredicate)
    |> List.sortBy(fun (ship,_) -> ship.GetEnglishName)
    
    |> Seq.iter(fun (ship, strongAgainst) ->
        try
            let dmgTypes = ship.attackTypes |> List.distinct |> String.concat ","
            printfn $"%A{ship.GetEnglishName}, dmgTypes {dmgTypes} --> strong against" 
            strongAgainst 
            |>  List.sortBy(fun ship -> ship.GetEnglishName) 
            |> List.iter(fun weakShip -> printfn $"--> %A{weakShip.GetEnglishName}" )
        with e -> 
            eprintfn "%A" e
    )

strongAgainstReport (fun atk -> atk.fileName.Name = "F_Cap_Galaxy.entity") (fun def -> def.isBorg)


[<RequireQualifiedAccess>]
type Faction = // File beginning character
| Federation
| Klingon
| Romulan
| Cardassian
| Dominion
| Borg
| Other of string




type BasicInformation = {
    Name : string
    Faction : Faction
    HullType : string option
    FleetRole : string option
}

let parseFaction (g : GameInfoFile) =
    if g.isFederation then Faction.Federation
    elif g.isKlingon then Faction.Klingon
    elif g.isRomulan then Faction.Romulan
    elif g.isCardassian then Faction.Cardassian
    elif g.isDominion then Faction.Dominion
    elif g.isBorg then Faction.Borg
    else Faction.Other (g.fileName.Name.Substring(0,1))

let parseHullType (g : GameInfoFile) = 
    let designName = GameFiles.tryGetplanetItemType (g.fileName.Name.Replace(".entity",""))
    designName
    |> Seq.tryHead
    |> Option.bind(fun (key,_) -> key.Split(':') |> Array.tryItem 2)

let parseFleetRole (g : GameInfoFile) =
    selectOne "statCountType" g.lines
    |> Option.bind(fun sct -> GameFiles.tryGetEnglishValue $"IDS_GameStat_{sct}Built")
    |> Option.bind(fun fleetRole -> (fleetRole.Split('-') |> Array.tryItem 1))
    |> Option.map(fun s -> s.Trim())


let parseBasicInfo (g : GameInfoFile) = 
    let faction = parseFaction g
    let hullType = parseHullType g
    let fleetRole = parseFleetRole g

    {
        Name = g.GetEnglishName
        Faction = faction
        HullType = hullType
        FleetRole = fleetRole
    }

type WeaponBank = Front | Left | Right | Back

type Weapon = {
    Name : string
    AttackType : string
    Range : float
    DPS : float*float
    WeaponBank : WeaponBank list
    RequiredReseach : string list
}

type BasePrice = {
    Credits : float option
    Metal : float option
    Crystal : float option
    BuildTime : float option
}
    with 
        static member Empty = {
            Credits = None
            Metal = None
            Crystal = None
            BuildTime = None
        }

let parseBasePrice (g : GameInfoFile) =
        let linesJoined = g.lines |> String.concat Environment.NewLine
        let stringInfoParser = $"basePrice(\n|\r|\r\n)\s+credits\s+(?<credits>\d+.\d+)(\n|\r|\r\n)\s+metal\s+(?<metal>\d+.\d+)(\n|\r|\r\n)\s+crystal\s+(?<crystal>\d+.\d+)"
        let bp = {BasePrice.Empty with BuildTime = selectOneFloat "BuildTime" g.lines }
        Regex.Matches(linesJoined, stringInfoParser,RegexOptions.Multiline) 
        |> Seq.tryHead
        |> Option.map(fun m ->
            let credits = tryGetValue m.Groups "credits" |> Option.bind (fun g -> g.Value |> tryFloat)
            let metal = tryGetValue m.Groups "metal" |> Option.bind (fun g -> g.Value |> tryFloat)
            let crystal = tryGetValue m.Groups "crystal" |> Option.bind (fun g -> g.Value |> tryFloat)
            { bp with Credits = credits; Metal = metal; Crystal = crystal}
        )
        |> Option.defaultValue bp




type ShipValue = {
    StartValue : float
    ValueIncreasePerLevel : float
}

let parseShipValue  (lines : string array) (attribute : string) =
    let linesJoined = lines |> String.concat Environment.NewLine
    let stringInfoParser = $"{attribute}\s*StartValue\s+(?<StartValue>\d+.\d+)\s+ValueIncreasePerLevel\s+(?<ValueIncreasePerLevel>\d+.\d+)"
    let matches = Regex.Matches(linesJoined, stringInfoParser,RegexOptions.Multiline)
    if matches.Count > 0 then
        matches
        |> Seq.map(fun m ->
            {
                StartValue = m.Groups.["StartValue"].Value |> float
                ValueIncreasePerLevel = m.Groups.["ValueIncreasePerLevel"].Value |> float
            }
        )
        |> Seq.tryHead
    else
        Some { StartValue = selectOneFloat attribute lines  |> Option.defaultValue 0.0; ValueIncreasePerLevel = 0.0 }

let maxAdditionalLevels = 9.0

let parseWeapon (g : GameInfoFile) =
    let lines = ResizeArray<ResizeArray<string>>()
    let mutable weapon = null
    let mutable record = false
    g.lines
    |> Array.iter(fun l ->
        if l = "Weapon" then 
            if weapon <> null then
                lines.Add weapon
            weapon <-  ResizeArray<string>()
            record <- true
            weapon.Add l
        elif record && not <| l.StartsWith ("\t") && not <| l.StartsWith(" ") then
            record <- false
            if weapon <> null then
                lines.Add weapon
        elif record then
            weapon.Add l
    )
    let parseName (lines : string array) =
        lines
        |> selectOne "WeaponClassType"
        |> Option.bind(fun wct -> GameFiles.tryGetEnglishValue $"IDS_WEAPONCLASSTYPE_{wct}" )
        |> Option.defaultValue "Unknown"
    let parseAttackType (lines : string array) =
        lines
        |> selectOne "AttackType"
        |> Option.defaultValue "Unknown"
    let parseRange (lines : string array) =
        lines
        |> selectOneFloat "Range"
        |> Option.defaultValue 1.0
    let parseDps (lines : string array) =
        let cooldown = lines |> selectOneFloat "PreBuffCooldownTime" |> Option.defaultValue 1.0
        let front = lines |> selectOneFloat "DamagePerBank:FRONT"  |> Option.defaultValue 0.0
        let back = lines |> selectOneFloat "DamagePerBank:BACK" |> Option.defaultValue 0.0
        let left = lines |> selectOneFloat "DamagePerBank:LEFT" |> Option.defaultValue 0.0
        let right = lines |> selectOneFloat "DamagePerBank:RIGHT" |> Option.defaultValue 0.0

        let weaponCooldownDecreasePerc = parseShipValue g.lines "weaponCooldownDecreasePerc"
        let weaponDamageIncreasePerc = parseShipValue g.lines "weaponDamageIncreasePerc"

        printfn $"parseDps --> weaponCooldownDecreasePerc :{weaponCooldownDecreasePerc}, weaponDamageIncreasePerc: {weaponDamageIncreasePerc}"

        let init = damagePerSecond 0.0 [front;back;left;right] 0.0 cooldown
        let max =
            let getMax (sv : ShipValue) =
                sv.StartValue + (sv.ValueIncreasePerLevel * maxAdditionalLevels)
            //galaxy 131/45/40
            match weaponCooldownDecreasePerc, weaponDamageIncreasePerc with
            | Some coolDec, Some dmgInc ->
                let coolDec = getMax coolDec
                let dmgInc = getMax dmgInc
                damagePerSecond dmgInc [front;back;left;right] coolDec cooldown
            | Some coolDec, None ->
                let coolDec = getMax coolDec
                damagePerSecond 0.0 [front;back;left;right] coolDec cooldown
            | None, Some dmgInc ->
                let dmgInc = getMax dmgInc
                damagePerSecond dmgInc [front;back;left;right] 0.0 cooldown
            | None, None -> 
                init

        init,max

        
    let parseWeaponBanks (lines : string array) =
        let front = lines |> selectOneFloat "DamagePerBank:FRONT"  |> Option.filter(fun dmg -> dmg > 0.0)
        let back = lines |> selectOneFloat "DamagePerBank:BACK" |> Option.filter(fun dmg -> dmg > 0.0)
        let left = lines |> selectOneFloat "DamagePerBank:LEFT" |> Option.filter(fun dmg -> dmg > 0.0)
        let right = lines |> selectOneFloat "DamagePerBank:RIGHT" |> Option.filter(fun dmg -> dmg > 0.0)
        [
            if front.IsSome then WeaponBank.Front
            if back.IsSome then WeaponBank.Back
            if left.IsSome then WeaponBank.Left
            if right.IsSome then WeaponBank.Right
        ]
    let parseRequiredResearch (lines : string array) =
        lines
        |> selectMany "Subject"
        |> List.choose(fun researchId -> 
            gameInfos 
            |> Seq.tryFind(fun gi -> gi.fileName.Name = $"{researchId}.entity")
            |> Option.map(fun g -> g.GetEnglishName )
        )

    let parseWeapon (lines : string array) =
        let name = parseName lines
        let attackType = parseAttackType lines
        let range = parseRange lines
        let dps = parseDps lines
        let banks = parseWeaponBanks lines
        let requiredReseach = parseRequiredResearch lines
        {
            Name = name
            AttackType = attackType
            Range = range
            DPS = dps
            WeaponBank =  banks
            RequiredReseach = requiredReseach
        }
        
    lines
    |> Seq.map(Seq.toArray)
    |> Seq.map parseWeapon
    |> Seq.toList
    


type ShipSpecifications = {
    HullStrength : ShipValue option
    HullRestoreRate : ShipValue option
    ArmorType : string
    ArmorRating : ShipValue option
    ShieldStrength : ShipValue option
    ShieldMitigation : ShipValue option
    ShieldRestoreRate : ShipValue option
    Antimatter : ShipValue option
    AntimatterRestoreRate : ShipValue option
    CultureProtectRate : ShipValue option
    Weapons : Weapon list
    Shuttlebay : ShipValue option
}




let parseShipSpecifications (g : GameInfoFile) =

    let parseHullStrength (g : GameInfoFile) =  
        parseShipValue g.lines "MaxHullPoints"
    let parseHullRestoreRate (g : GameInfoFile) =  
        parseShipValue g.lines "HullPointRestoreRate"
    let parseArmorRating (g : GameInfoFile) =  
        let armorPointsFromExperience =
            parseShipValue g.lines "ArmorPointsFromExperience"
        let baseArmor () =
             Some { StartValue =  selectOneFloat "BaseArmorPoints" g.lines |> Option.defaultValue 0.0; ValueIncreasePerLevel = 0.0 } 

        armorPointsFromExperience
        |> Option.orElseWith baseArmor
    
    let parseShieldStrength (g : GameInfoFile) =  
        parseShipValue g.lines "MaxShieldPoints"
    let parseShieldMitigation (g : GameInfoFile) =  
        parseShipValue g.lines "maxMitigation"
    let parseShieldRestoreRate (g : GameInfoFile) =  
        parseShipValue g.lines "ShieldPointRestoreRate"
    let parseAntimatter (g : GameInfoFile) =  
        parseShipValue g.lines "MaxAntiMatter"
    let parseAntimatterRestoreRate (g : GameInfoFile) =  
        parseShipValue g.lines "AntiMatterRestoreRate"
    let parseCultureProtectRate (g : GameInfoFile) =  
        parseShipValue g.lines "CultureProtectRate"
    let parseShuttlebay  (g : GameInfoFile) =
        
        let ifNoneThunk () =
            g.lines
            |> selectOneFloat "maxNumCommandPoints"
            |> Option.map(fun v -> { StartValue = v ;
                                     ValueIncreasePerLevel = 0.0 })
            
        parseShipValue g.lines "CommandPoints"
        |> Option.orElseWith ifNoneThunk
    
    let hullStrength = parseHullStrength g
    let hullRestoreRate = parseHullRestoreRate g
    let armorRating = parseArmorRating g
    let shieldStrength = parseShieldStrength g
    let shieldMitigation = parseShieldMitigation g
    let shieldRestoreRate = parseShieldRestoreRate g
    let antimatter = parseAntimatter g
    let antimatterRestoreRate = parseAntimatterRestoreRate g
    let cultureProtectRate = parseCultureProtectRate g
    let weapons = parseWeapon g
    let shuttles = parseShuttlebay g
    {
        HullStrength = hullStrength 
        HullRestoreRate = hullRestoreRate
        ArmorType = g.armorType |> Option.defaultValue ""
        ArmorRating = armorRating
        ShieldStrength =  shieldStrength
        ShieldMitigation =  shieldMitigation
        ShieldRestoreRate = shieldRestoreRate
        Antimatter = antimatter
        AntimatterRestoreRate = antimatterRestoreRate
        CultureProtectRate =  cultureProtectRate
        Weapons = weapons
        Shuttlebay = shuttles
    }


type Ability = {
    Name : string
    Description : string
}

let isNotNullOrWhiteSpace = String.IsNullOrWhiteSpace >> not

let parseAbility (g : GameInfoFile) = 
    let ability0 = selectOne "ability:0" g.lines |> Option.filter isNotNullOrWhiteSpace
    let ability1 = selectOne "ability:1" g.lines |> Option.filter isNotNullOrWhiteSpace
    let ability2 = selectOne "ability:2" g.lines |> Option.filter isNotNullOrWhiteSpace
    let ability3 = selectOne "ability:3" g.lines |> Option.filter isNotNullOrWhiteSpace
    let ability4 = selectOne "ability:4" g.lines |> Option.filter isNotNullOrWhiteSpace
    [
        ability0
        ability1
        ability2
        ability3
        ability4
    ]
    |> List.choose id
    |> List.choose(fun abString -> 
        gameInfos 
        |> List.tryFind(fun gi -> gi.fileName.Name = $"{abString}.entity")
        |> Option.map(fun g -> {Name = g.GetEnglishName; Description = g.GetEnglishDesc })
    )

type Information = {
    GameInfoFile : GameInfoFile
    BasicInformation : BasicInformation
    ShipSpecifications : ShipSpecifications
    Abilities : Ability list
    BasePrice : BasePrice
}

let parseInformation (g : GameInfoFile) =
    let basicInfo = parseBasicInfo g
    let shipSpecs = parseShipSpecifications g
    let abilities = parseAbility g
    let basePrice = parseBasePrice g
    {
        GameInfoFile = g
        BasicInformation = basicInfo
        ShipSpecifications = shipSpecs
        Abilities= abilities
        BasePrice = basePrice
    }



#r "nuget: Giraffe.ViewEngine"

open Giraffe.ViewEngine


let _align = attr "align"

// <table class="wikitable">
//   <tr>
//     <td> [[File:Galaxy class.png|280px|thumbnail|center]]
//     </td>
//   </tr>
// </table>

let th attrs body = th (_scope "row" :: attrs) body
let td attrs body = td (_scope "row" :: attrs) body


let keyValueRow (maxColumns : int) key value =
    let keyColspan, valueColspan =
        if maxColumns % 2 = 0 then
            maxColumns / 2,  maxColumns / 2
        else
            maxColumns / 2,  (maxColumns / 2) + (maxColumns % 2)
    tr [] [
        
        td [_colspan (string keyColspan)] [
            str key
        ]
        td [_colspan (string valueColspan)] [
            str value
        ]
    ]



let factionOutput (info : Information) =
    match info.BasicInformation.Faction with
    | Faction.Federation -> "[[United Federation of Planets|Federation]]"
    | Faction.Klingon -> "[[Klingon Empire|Klingon]]"
    | Faction.Romulan -> "[[Romulan Star Empire|Romulan]]"
    | Faction.Cardassian -> "[[Cardassian Union|Cardassian]]"
    | Faction.Dominion -> "[[Dominion Alliance|Dominion]]"
    | Faction.Borg ->  "[[Borg Collective|Borg]]"
    | Faction.Other(other) -> other

let createBasicInfo (maxColumns : int) (info : Information) = [
    tr [] [
        th [ _colspan (string maxColumns)] [ 
            str "BasicInformation"
        ]
    ]
    keyValueRow maxColumns "Faction" (factionOutput info)
    keyValueRow maxColumns "Hull Classification" ( sprintf "%s" (info.BasicInformation.HullType |> Option.defaultValue "N/A"))
    keyValueRow maxColumns "Fleet Role" ( sprintf "%s" (info.BasicInformation.FleetRole |> Option.defaultValue "N/A"))

]


let createCost (maxColumns : int) (info : BasePrice) = [
    tr [] [
        th [ _colspan (string maxColumns)] [ 
            str "Costs"
        ]
    ]
    
    keyValueRow maxColumns "Credits" (info.Credits |> Option.map string |> Option.defaultValue "N/A" )
    keyValueRow maxColumns "Metal (Tritanium)" (info.Metal |> Option.map string |> Option.defaultValue "N/A" )
    keyValueRow maxColumns "Crystal (Dilithium)" (info.Crystal |> Option.map string |> Option.defaultValue "N/A" )
    keyValueRow maxColumns "Build Time (seconds)"  (info.BuildTime |> Option.map string |> Option.defaultValue "N/A" )
]


let initValueAndMax (sv : ShipValue) =
    let init = sv.StartValue
    let max = init + (sv.ValueIncreasePerLevel * maxAdditionalLevels)
    init, max

let createDefenses (maxColumns : int) (info : ShipSpecifications) = [
    tr [] [
        th [ _colspan (string maxColumns)] [ 
            str "Defenses"
        ]
    ]
    


    
    if info.HullStrength.IsSome then
        let init,max = initValueAndMax info.HullStrength.Value
        keyValueRow maxColumns "Hull Strength (max level)" $"{init} ({max})"
    if info.HullRestoreRate.IsSome then
        let init,max = initValueAndMax info.HullRestoreRate.Value
        keyValueRow maxColumns "Hull Restore Rate (max level)" $"{init} ({max})"
        
    keyValueRow maxColumns "Armor Type" (info.ArmorType)
    if info.ArmorRating.IsSome then
        let init,max = initValueAndMax info.ArmorRating.Value
        keyValueRow maxColumns "Armor Rating (max level)" $"{init} ({max})"
    if info.ShieldStrength.IsSome then
        let init,max = initValueAndMax info.ShieldStrength.Value
        keyValueRow maxColumns "Shield Strength (max level)" $"{init} ({max})"
    if info.ShieldRestoreRate.IsSome then
        let init,max = initValueAndMax info.ShieldRestoreRate.Value
        keyValueRow maxColumns "Shield Restore Rate (max level)" $"{init} ({max})"
    if info.ShieldMitigation.IsSome then
        let init,max = initValueAndMax info.ShieldMitigation.Value
        keyValueRow maxColumns "Shield Mitigation (max level)" $"{init * 100.0  |> int}%% ({max* 100.0 |> int}%%)"

]

let createAuxiliary (maxColumns : int) (info : ShipSpecifications) = [
    tr [] [
        th [ _colspan (string maxColumns)] [ 
            str "Auxiliary"
        ]
    ]
    
    if info.Antimatter.IsSome then
        let init,max = initValueAndMax info.Antimatter.Value
        keyValueRow maxColumns "Antimatter (max level)" $"{init} ({max})"
    if info.AntimatterRestoreRate.IsSome then
        let init,max = initValueAndMax info.AntimatterRestoreRate.Value
        keyValueRow maxColumns "Antimatter Restore Rate (max level)" $"{init} ({max})"
    if info.CultureProtectRate.IsSome then
        let init,max = initValueAndMax info.CultureProtectRate.Value
        keyValueRow maxColumns "Culture Repel Rate (max level)" $"{init} ({max})"
]

let createWeapons (maxColumns : int) (info : ShipSpecifications) = [
    tr [] [
        th [] [ 
            str "Weapons"
        ]
        th [] [ 
            str "Damage Per Second (max level)"
        ]
        th [] [ 
            str "Range"
        ]
        th [] [ 
            str "Attack Type"
        ]
        th [] [ 
            str "Banks"
        ]
    ]
    for weapon in info.Weapons do
        tr [] [
            td [] [
                let research =
                    if weapon.RequiredReseach |> Seq.isEmpty |> not then
                        weapon.RequiredReseach
                        |> String.concat ", "
                        |> sprintf "(Requires %s research)"
                    else ""
                str <| $"{weapon.Name} {research}"
                
            ]
            td [] [
                str <| $"%.1f{fst weapon.DPS} (%.1f{snd weapon.DPS})"
            ]
            td [] [
                str <| $"{weapon.Range}"
            ]
            td [] [
                str <| $"{weapon.AttackType}"
            ]
            td [] [
                weapon.WeaponBank
                |> Seq.map(string) 
                |> String.concat ", "
                |> str
            ]
        ]
]

let createShuttleBay (maxColumns : int) (info : ShipSpecifications) = [
    tr [] [
        th [ _colspan (string maxColumns)] [ 
            str "Shuttlebay"
        ]
    ]
    let getMax (sv : ShipValue) =
        sv.StartValue + (sv.ValueIncreasePerLevel * maxAdditionalLevels)
    let display =
        match info.Shuttlebay with
        | Some sb -> 
            let init = sb.StartValue |> Math.Floor
            let max = getMax sb |> Math.Floor
            $"{init} ({max})"
        | None ->
            "0"
    keyValueRow maxColumns "Shuttles (max level)" display
    
]

let createInfoTable (maxColumns : int) (info : Information) =
    table [_class "wikitable"; _align "right" ] [
            caption [] [
                str "Information"
            ]
            tr [] [
                th [_colspan (string maxColumns)] [
                 str info.BasicInformation.Name
                ]
            ]
            tr [] [
                td [ _colspan (string maxColumns)] [
                    rawText $"[[File:{info.BasicInformation.Name} class.png|280px|thumbnail|center]]"
                ]
            ]
            yield! createBasicInfo maxColumns info
            yield! createCost maxColumns info.BasePrice
            yield! createDefenses   maxColumns info.ShipSpecifications
            yield! createAuxiliary   maxColumns info.ShipSpecifications
            yield! createWeapons maxColumns info.ShipSpecifications
            yield! createShuttleBay maxColumns info.ShipSpecifications
        
    ]


let writeHtmls (wikiInfos : Information list) =

    let wikiTablesPath = IO.Path.Join(__SOURCE_DIRECTORY__,"wiki-tables")
    if IO.Directory.Exists(wikiTablesPath) then
        IO.Directory.Delete(wikiTablesPath,true)
    IO.Directory.CreateDirectory(wikiTablesPath) |> ignore

    wikiInfos
    |> Seq.iter(fun wikiInfo ->
        let formattedHtml =
            createInfoTable 5 wikiInfo
            |> RenderView.AsString.htmlNode
            |> System.Xml.Linq.XElement.Parse
            |> string
        let outputPath = IO.Path.Join(wikiTablesPath, wikiInfo.GameInfoFile.fileName.Name.Replace(".entity", "-infotable.html"))
        IO.File.WriteAllText(outputPath, formattedHtml)
        
    )
    printfn $"Wrote to {wikiTablesPath}"


let ships =
    gameInfos
    |> List.filter(fun f -> f.fileName.Name.Contains("F_Cap_Galaxy.entity"))
    |> List.filter(fun f -> f.entityType |> isShipO)



let wikiInfos =
    ships
    |> List.map parseInformation

writeHtmls wikiInfos