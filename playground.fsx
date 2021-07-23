open System
open System.Collections.Generic
open System.Text.RegularExpressions


let inline tryGetValue (d : ^T) key =
    let mutable output = null
    let parsed = ( ^T : (member TryGetValue  : string * byref< _ > -> bool ) (d, key, &output) )
    match parsed with
    | true -> Some output
    | _ -> None

let inline tryParse str : ^a option =
    let mutable value = Unchecked.defaultof< ^a>
    let result = (^a: (static member TryParse: string * byref< ^a> -> bool) str, &value)
    if result then Some value
    else None

module GameFiles =
    let (/) path1 path2 = IO.Path.Combine(path1,path2)
    let modBasePath =   __SOURCE_DIRECTORY__ / ".." / "STA3NEMESIS"
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
    // printfn $"weaponDamageIncreasePerc : {weaponDamageIncreasePerc}, damagePerBank {damagePerBank}, weaponCooldownDecreasePerc {weaponCooldownDecreasePerc}, preBuffCooldownTime {preBuffCooldownTime}"
    let damageSum = percentageIncrease weaponDamageIncreasePerc  (damagePerBank |> List.sum) 
    damageSum / (percentageDecrease weaponCooldownDecreasePerc preBuffCooldownTime)

let selectOne (key : string) (lines : string array) =
    lines
    |> Array.tryFind(fun s -> s.Trim().StartsWith(key,StringComparison.InvariantCultureIgnoreCase))
    |> Option.map(fun s -> s.Replace(key,"",StringComparison.InvariantCultureIgnoreCase).Trim().Trim('\"').Trim())

let tryFloat (s : string) : float option =
    tryParse s

let tryBool (s : string) : bool option =
    tryParse s

let selectOneFloat (key : string) (lines : string array) =
    selectOne key lines
    |> Option.bind tryFloat

let selectOneBool (key : string) (lines : string array) =
    selectOne key lines
    |> Option.bind tryBool

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
            descString = selectOne "descStringID" lines |> Option.orElse (selectOne "DescriptionStringID" lines)          
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
    member x.isMainFaction = x.isBorg || x.isCardassian || x.isFederation || x.isKlingon || x.isDominion || x.isRomulan



let readFile (f : IO.FileInfo) =
    GameInfoFile.Create(f, IO.File.ReadAllLines f.FullName)

type SearchType =
| Equals of string
| EqualsI of string
| StartsWith of String

let shipEntityTypes = [
    Equals "Frigate"
    Equals"CapitalShip"
    Equals "Titan"
    Equals "Fighter"
    // "EntryVehicle"
]

let isShip(entityType : string) =
    shipEntityTypes |> Seq.exists (fun t ->
        match t with
        | Equals eq -> eq = entityType
        | EqualsI eq -> eq.Equals(entityType, StringComparison.InvariantCultureIgnoreCase)
        | StartsWith sw -> entityType.StartsWith sw
    )


let isShipO(entityType : string option) =
    entityType
    |> Option.exists(isShip)

let buildingTypes = [
    StartsWith "PlanetModule"
    EqualsI "StarBase"
]


let isBuilding(entityType : string) =
    buildingTypes |> Seq.exists (fun t ->
        match t with
        | Equals eq -> eq = entityType
        | EqualsI eq -> eq.Equals(entityType, StringComparison.InvariantCultureIgnoreCase)
        | StartsWith sw -> entityType.StartsWith sw
    )


let isBuildingO(entityType : string option) =
    entityType
    |> Option.exists(isBuilding)

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
    Description : string
    Faction : Faction
    HullType : string option
    FleetRole : string option
}


type WeaponBank = Front | Left | Right | Back
[<RequireQualifiedAccessAttribute>]
type ResearchRequirement =
| Research of string list
| Assimilation 

type Weapon = {
    Name : string
    AttackType : string
    Range : float
    DPS : float*float
    WeaponBank : WeaponBank list
    RequiredResearch : ResearchRequirement
}

type BasePrice = {
    Credits : float option
    Metal : float option
    Crystal : float option
    BuildTime : float option
    SupplyCost : float option
}
    with 
        static member Empty = {
            Credits = None
            Metal = None
            Crystal = None
            BuildTime = None
            SupplyCost = None
        }



type ShipValue = {
    StartValue : float
    ValueIncreasePerLevel : float
}


[<RequireQualifiedAccess>]
type BombingValue = 
| NoBombingAbility
| SimpleBombing of float


type Ability = {
    Name : string
    Description : string
}



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
    Bombing : BombingValue
    MaxAccelerationLinear : float option
    MaxAccelerationStrafe : float option
    MaxDecelerationLinear: float option
    MaxAccelerationAngular: float option
    MaxDecelerationAngular : float option
    MaxSpeedLinear : float option
    MaxRollRate : float option
    MaxRollAngle : float option
}


type Information = {
    GameInfoFile : GameInfoFile
    BasicInformation : BasicInformation
    ShipSpecifications : ShipSpecifications
    Abilities : Ability list
    BasePrice : BasePrice
}


type CounteringInfo = {
    Ship : Information
    StrongAgainst : Information list
    WeakAgainst : Information list
}



let generateCounteringInfoForShip (ships : Information list) (attackingShip : Information)  =
    let getAttackTypes ship =
        ship.ShipSpecifications.Weapons
        |> List.filter(fun w -> w.RequiredResearch <> ResearchRequirement.Assimilation )
        |> List.map(fun w -> w.AttackType)
    let strongAgainst =
        getAttackTypes attackingShip
        |> List.collect(fun at -> 
            let armorType = attackTypesToArmorType.[at]
            ships
            |> List.filter(fun s -> s.ShipSpecifications.ArmorType = armorType) 
        )
        |> List.distinctBy(fun s -> s.GameInfoFile.fileName.FullName)
    let weakAgainst =
        ships
        |> List.filter(fun s ->
            getAttackTypes s 
            |> List.map (fun at -> attackTypesToArmorType.[at])
            |> List.exists(fun armorType -> armorType = attackingShip.ShipSpecifications.ArmorType)

        )
    { Ship = attackingShip; StrongAgainst = strongAgainst; WeakAgainst = weakAgainst }

let generateCounteringInfo (ships : Information list) =
    ships
    |> List.map(generateCounteringInfoForShip ships )
    |> List.sortBy(fun ci -> ci.Ship.GameInfoFile.GetEnglishName)
    



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
        Description = g.GetEnglishDesc
        Faction = faction
        HullType = hullType
        FleetRole = fleetRole
    }


let parseBasePrice (g : GameInfoFile) =
        let linesJoined = g.lines |> String.concat Environment.NewLine
        let stringInfoParser = $"basePrice(\n|\r|\r\n)\s+credits\s+(?<credits>\d+.\d+)(\n|\r|\r\n)\s+metal\s+(?<metal>\d+.\d+)(\n|\r|\r\n)\s+crystal\s+(?<crystal>\d+.\d+)"
        let bp = { BasePrice.Empty 
                    with 
                        BuildTime = selectOneFloat "BuildTime" g.lines |> Option.orElse(selectOneFloat "baseBuildTime" g.lines) |> Option.orElse(selectOneFloat "fighterConstructionTime" g.lines)
                        SupplyCost = selectOneFloat "slotCount" g.lines }
        Regex.Matches(linesJoined, stringInfoParser,RegexOptions.Multiline) 
        |> Seq.tryHead
        |> Option.map(fun m ->
            let credits = tryGetValue m.Groups "credits" |> Option.bind (fun g -> g.Value |> tryFloat)
            let metal = tryGetValue m.Groups "metal" |> Option.bind (fun g -> g.Value |> tryFloat)
            let crystal = tryGetValue m.Groups "crystal" |> Option.bind (fun g -> g.Value |> tryFloat)
            { bp with Credits = credits; Metal = metal; Crystal = crystal}
        )
        |> Option.defaultValue bp




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
        selectOneFloat attribute lines
        |> Option.map(fun v ->
            { StartValue = v; ValueIncreasePerLevel = 0.0 }
        )

let maxAdditionalLevels = 9.0



let parseWeapon (g : GameInfoFile) =
    let lines = ResizeArray<ResizeArray<string>>()
    let mutable weapon = null
    let mutable record = false
    g.lines
    |> Array.iter(fun l ->
        if l.Trim().Equals("Weapon", StringComparison.InvariantCultureIgnoreCase) then 
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
        |> Option.bind(fun wct -> 
            GameFiles.tryGetEnglishValue $"IDS_WEAPONCLASSTYPE_{wct}" )
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

        // printfn $"parseDps --> weaponCooldownDecreasePerc :{weaponCooldownDecreasePerc}, weaponDamageIncreasePerc: {weaponDamageIncreasePerc}"

        let init = damagePerSecond 0.0 [front;back;left;right] 0.0 cooldown
        let max =
            let getMax (sv : ShipValue) =
                sv.StartValue + (sv.ValueIncreasePerLevel * maxAdditionalLevels)

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
        let assimilation =
            lines 
            |> selectOne "RequiredFactionNameID"
            |> Option.exists(fun v -> v.Trim() <> "")
        if assimilation then
            ResearchRequirement.Assimilation
        else
            lines
            |> selectMany "Subject"
            |> List.choose(fun researchId -> 
                gameInfos 
                |> Seq.tryFind(fun gi -> gi.fileName.Name = $"{researchId}.entity")
                |> Option.map(fun g -> g.GetEnglishName )
            )
            |> ResearchRequirement.Research

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
            RequiredResearch = requiredReseach
        }
        
    lines
    |> Seq.map(Seq.toArray)
    |> Seq.map parseWeapon
    |> Seq.toList
    



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
    let parseBombing (g : GameInfoFile) =
        let canBomb = selectOneBool "canBomb" g.lines |> Option.defaultValue false
        if canBomb then
            match selectOneFloat "baseDamage" g.lines with
            | Some v ->  BombingValue.SimpleBombing v
            | None -> BombingValue.NoBombingAbility
        else
            BombingValue.NoBombingAbility
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
    let bombing = parseBombing g
    
    let maxAccelerationLinear = selectOneFloat "maxAccelerationLinear" g.lines
    let maxAccelerationStrafe = selectOneFloat "maxAccelerationStrafe" g.lines
    let maxDecelerationLinear = selectOneFloat "maxDecelerationLinear" g.lines
    let maxAccelerationAngular = selectOneFloat "maxAccelerationAngular" g.lines
    let maxDecelerationAngular = selectOneFloat "maxDecelerationAngular" g.lines
    let maxSpeedLinear = selectOneFloat "maxSpeedLinear"  g.lines
    let maxRollRate = selectOneFloat "maxRollRate" g.lines
    let maxRollAngle =selectOneFloat "maxRollRate" g.lines
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
        Bombing = bombing
        MaxAccelerationLinear = maxAccelerationLinear
        MaxAccelerationStrafe = maxAccelerationStrafe
        MaxDecelerationLinear = maxDecelerationLinear
        MaxAccelerationAngular = maxAccelerationAngular
        MaxDecelerationAngular = maxDecelerationAngular
        MaxSpeedLinear = maxSpeedLinear
        MaxRollRate = maxRollRate
        MaxRollAngle = maxRollAngle
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

let keyValueRowFloatOpt (maxColumns : int) key value =
    value
    |> Option.map string
    |> Option.defaultValue "N/A"
    |> keyValueRow maxColumns key


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
    
    keyValueRowFloatOpt maxColumns "Credits (Latinum)" info.Credits 
    keyValueRowFloatOpt maxColumns "Metal (Tritanium)" info.Metal 
    keyValueRowFloatOpt maxColumns "Crystal (Dilithium)" info.Crystal
    keyValueRowFloatOpt maxColumns "Build Time (seconds)" info.BuildTime 
    keyValueRowFloatOpt maxColumns "Supply cost"  info.SupplyCost 
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
            str "Damage (max level)"
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
                    match weapon.RequiredResearch with
                    | ResearchRequirement.Assimilation -> 
                        "(Assimilated)"
                    | ResearchRequirement.Research r ->
                        if r |> Seq.isEmpty |> not then
                            r
                            |> String.concat ", "
                            |> sprintf "(%s)"
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
                |> String.concat " "
                |> str
            ]
        ]
]

let createSublightSpeeds (maxColumns : int) (info : ShipSpecifications) = [
    tr [] [
        th [ _colspan (string maxColumns)] [ 
            str "Sublight Speed"
        ]
    ]
    keyValueRowFloatOpt maxColumns "Max Speed" info.MaxSpeedLinear  
    keyValueRowFloatOpt maxColumns "Angular Acceleration" info.MaxAccelerationAngular  
    keyValueRowFloatOpt maxColumns "Angular Deceleration" info.MaxDecelerationAngular  
    keyValueRowFloatOpt maxColumns "Linear Acceleration" info.MaxAccelerationLinear  
    keyValueRowFloatOpt maxColumns "Linear Deceleration" info.MaxDecelerationLinear
    keyValueRowFloatOpt maxColumns "Strafe Acceleration" info.MaxAccelerationStrafe
    keyValueRowFloatOpt maxColumns "Roll Rate" info.MaxRollRate
    keyValueRowFloatOpt maxColumns "Roll Angle" info.MaxRollAngle
]

let createOther (maxColumns : int) (info : ShipSpecifications) = [
    tr [] [
        th [ _colspan (string maxColumns)] [ 
            str "Other"
        ]
    ]
    let getMax (sv : ShipValue) =
        sv.StartValue + (sv.ValueIncreasePerLevel * maxAdditionalLevels)
    let display = 
        match info.Shuttlebay with
        | Some sb -> 
            let init, max = initValueAndMax sb
            $"{init} ({max})"
        | None ->
            "0"
    keyValueRow maxColumns "Shuttles (max level)" display
    match info.Bombing with
    | BombingValue.SimpleBombing v->  v |> string |>  keyValueRow maxColumns "Bombing Damage" 
    | BombingValue.NoBombingAbility -> ()
    
]

let createInfoTable (maxColumns : int) (info : Information) =
    div [_id "info-table"] [
        table [_class "wikitable"; _align "right"; _style "margin: 2em" ] [
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
                yield! createSublightSpeeds maxColumns info.ShipSpecifications
                yield! createOther maxColumns info.ShipSpecifications
            
        ]
    ]

let wikiTablesPath = IO.Path.Join(__SOURCE_DIRECTORY__,"wiki-tables")
let writeInfoTableHtmls (wikiInfos : Information list) =

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
    printfn $"Wrote info tables to {wikiTablesPath}"

let renderNodesO node =
    node
    |> Option.map(
        RenderView.AsString.htmlNode
        >> System.Xml.Linq.XElement.Parse
        >> string
    )
    |> Option.defaultValue ""

let abilitiesTablePath = IO.Path.Join(__SOURCE_DIRECTORY__,"entity-ability-tables")
let writeEntityAbilitiesTableHtmls (infos : Information list) =

    
    if IO.Directory.Exists(abilitiesTablePath) then
        IO.Directory.Delete(abilitiesTablePath,true)
    IO.Directory.CreateDirectory(abilitiesTablePath) |> ignore
    
    let writeAbilities (abilities : Ability list) =
        if List.length abilities > 0 then
            table [_class "wikitable"] [
                tr [] [
                    th [] [ str "Name" ]
                    th [] [ str "Description" ]
                ]
                for a in abilities do
                    tr [] [
                        td [] [
                            str $"[[{a.Name}]]"
                        ]
                        td [] [
                            str a.Description
                        ]
                ]
            ]
            |> Some
        else 
            None

    infos
    |> List.iter(fun i ->
        let htmlTable =
            match writeAbilities i.Abilities with
            | Some node ->
                let output =
                    node
                    |> RenderView.AsString.htmlNode
                    |> System.Xml.Linq.XElement.Parse
                    |> string 
                sprintf """
==Abilities==
%s
"""             
                    output
            | None -> ""
        let outputPath = IO.Path.Join(abilitiesTablePath, i.GameInfoFile.fileName.Name.Replace(".entity", "-entity-ability-table.html"))
        IO.File.WriteAllText(outputPath, htmlTable)
    )



let counterTablesPath = IO.Path.Join(__SOURCE_DIRECTORY__,"counter-tables")
let writeCounterHtmlTables (counterShipReport : CounteringInfo list) =
    
    let generateJaggedTable (infos : Information list) =
        infos
        |> List.map(fun i ->
            i.BasicInformation.Faction,
                td [] [
                    if i.BasicInformation.Faction = Faction.Klingon && i.BasicInformation.Name.Contains("Raptor") then
                        str <| sprintf "[[%s(K)]]" i.BasicInformation.Name
                    else str <| sprintf "[[%s]]" i.BasicInformation.Name
                ]
        )
        |>(fun nodes -> 
            nodes
            |> List.groupBy(fst)
            |> List.map(fun (faction, nodes) ->
                let headers = th [] [ str <| sprintf "%A" faction]
                let nodes = nodes |> List.map snd |> List.distinct
                (headers, nodes)
            )
        )|> fun nodes ->
            let headers = nodes |> List.map fst |> tr []
            let dataRows = nodes |> List.map snd 
            let maxJaggedRow = 
                try
                    dataRows |> List.map List.length |> List.max
                with _ -> 0
            let rows = [
                for i=0 to maxJaggedRow - 1 do 
                    [
                        for dataRow in dataRows do
                            match dataRow |> List.tryItem i with
                            | Some node -> node
                            | None -> td [] []
                    ] |> tr []
            ]
            if List.length rows > 0 then
                table [_class "wikitable"] [
                    headers
                    yield! rows
                ]
                |> Some
            else 
                None

    
    if IO.Directory.Exists(counterTablesPath) then
        IO.Directory.Delete(counterTablesPath,true)
    IO.Directory.CreateDirectory(counterTablesPath) |> ignore
    counterShipReport
    |> List.iter(fun ci ->
        let strongAgainstNodes = ci.StrongAgainst |> generateJaggedTable
        let strongAgainstFormattedHtml =  
            let html = 
                strongAgainstNodes |> renderNodesO
            if html |> isNotNullOrWhiteSpace then
                sprintf """
===Strong Against===

This does extra damage against these:

%s
""" 
                    html
            else
                ""
        let weakAgainstNodes = ci.WeakAgainst |> generateJaggedTable
        let weakAgainstFormattedHtml =
            let html =  weakAgainstNodes |> renderNodesO

            if html |> isNotNullOrWhiteSpace then
                sprintf """
===Weak Against===

This list does extra damage against this:

%s
""" 
                    html
            else
                ""
        let finalOutput = $"""
==Countering==

Each ship has a [[Damage Types|attack type]] and an [[Damage Types|armor type]] (some have 2 attack types) which determine what ships it deals a lot of damage against and which ships it takes a lot of damage from.

{strongAgainstFormattedHtml}

{weakAgainstFormattedHtml}
"""
        let outputPath = IO.Path.Join(counterTablesPath, ci.Ship.GameInfoFile.fileName.Name.Replace(".entity", "-counter-table.html"))
        IO.File.WriteAllText(outputPath, finalOutput)
    )
    printfn $"Wrote counter tables to {counterTablesPath}"


let ships =
    gameInfos
    |> List.filter(fun f -> not <| f.fileName.Name.Contains("Tutorial.entity")) // Tutorial
    |> List.filter(fun f -> not <| f.fileName.Name.Contains("_flagship")) //unsure
    |> List.filter(fun f -> not <| f.fileName.Name.Contains("F_Support_Polaris")) // Ship ability clone
    |> List.filter(fun f -> not <| f.fileName.Name.Contains("C_Support_DomDread")) // Ship ability clone
    |> List.filter(fun f -> not <| f.fileName.Name.Contains("C_Support_Hydra")) // Ship ability clone
    |> List.filter(fun f -> not <| f.fileName.Name.Contains("AI.entity")) // AI ships
    |> List.filter(fun f -> not <| f.fileName.Name.Contains("_Coop_")) //unsure
    |> List.filter(fun f -> not <| f.fileName.Name.Contains("_Holo_")) //holo ships
    |> List.filter(fun f -> not <| f.fileName.Name.Contains("C_Cruiser_SonaBattleship")) //replaced by C_Cap_JalakSu 
    |> List.filter(fun f -> not <| f.fileName.Name.Contains("F_Frigate_Nova")) //replaced by C_Cap_JalakSu 
    |> List.filter(fun f -> f.entityType |> isShipO || f.entityType |> isBuildingO)


// Get all entity types
gameInfos
|> Seq.map(fun g -> g.entityType)
|> Seq.distinct
|> Seq.iter(printfn "%A")

let wikiInfos =
    ships
    |> List.map parseInformation

writeInfoTableHtmls wikiInfos

writeEntityAbilitiesTableHtmls wikiInfos

let counterShipReport =
    wikiInfos
    |> List.filter(fun s -> s.GameInfoFile.isMainFaction)
    |> generateCounteringInfo 

writeCounterHtmlTables counterShipReport

let writeNewTemplate (infos : Information list) =
    let getFileByInfo path info =
        IO.DirectoryInfo(path).EnumerateFiles() 
        |> Seq.tryFind(fun f -> f.Name.Contains(info.GameInfoFile.fileName.Name.Replace(".entity", "")))
        |> Option.map(fun fi -> IO.File.ReadAllText fi.FullName)
        |> Option.defaultValue ""

    let newTemplatesPath =  IO.Path.Join(__SOURCE_DIRECTORY__,"new-templates")
    if IO.Directory.Exists(newTemplatesPath) then
        IO.Directory.Delete(newTemplatesPath,true)
    IO.Directory.CreateDirectory(newTemplatesPath) |> ignore
    for info in infos do
        let faction = factionOutput info
        let hullType = info.BasicInformation.HullType
        let fleetRole =
            info.BasicInformation.FleetRole
            |> Option.orElse hullType
            |> Option.defaultValue ""
        let infoTable = getFileByInfo wikiTablesPath info
        let abilities =  getFileByInfo abilitiesTablePath info
        let countering = getFileByInfo counterTablesPath info
        let categories = 
            let mainCategory = 
                if info.GameInfoFile.entityType |> isBuildingO then
                    "[[Category:Building Types]]"
                elif info.GameInfoFile.entityType |> isShipO then
                    "[[Category:Ship classes]]"
                else 
                    "[[Category:Other]]"
            let subCategory =
                info.BasicInformation.FleetRole
                |> Option.orElse info.BasicInformation.HullType
                |> Option.map(sprintf "[[Category:%s]]")
                |> Option.defaultValue ""
            $"""
{mainCategory}
{subCategory}
"""
        let finalOutput = $"""
The '''{{{{PAGENAME}}}}''' is a {faction} {fleetRole}.

{infoTable}


==Description==

{info.BasicInformation.Description}

{abilities}

{countering}

==Gallery==

{categories}

"""
        
        let outputPath = IO.Path.Join(newTemplatesPath, info.GameInfoFile.fileName.Name.Replace(".entity", "-new-template.html"))
        IO.File.WriteAllText(outputPath, finalOutput)
    
    printfn $"Wrote counter tables to {newTemplatesPath}"

writeNewTemplate wikiInfos

let pad (amt: int) (elem: 'a) (list: 'a list) : 'a list =
    if amt >=0 then list @ (List.replicate amt elem)
    else list
let fill (total:int) (elem: 'a) (list: 'a list) =
    if List.length list >= total then
        list
    else
        pad (total - List.length list) elem list
let armorWeaponCSVReport (infos : Information list) =
    let newTemplatesPath =  IO.Path.Join(__SOURCE_DIRECTORY__,"armor-weapon-report")
    if IO.Directory.Exists(newTemplatesPath) then
        IO.Directory.Delete(newTemplatesPath,true)
    IO.Directory.CreateDirectory(newTemplatesPath) |> ignore
    let csvFile = IO.Path.Join(newTemplatesPath, "armor-weapon-report.csv")
    IO.File.AppendAllLines(csvFile, ["Entity, Armor Type, Weapon Type 1, Weapon Type 2, Weapon Type 3"])
    for i in infos do
        let entityFile = i.GameInfoFile.fileName.Name
        let armorType = i.ShipSpecifications.ArmorType
        let weapons =
            i.ShipSpecifications.Weapons
            |> List.map(fun w -> w.AttackType)
            |> List.distinct
            |> fill 3 ""
            |> String.concat ", "
        let output = 
            [entityFile; armorType; weapons]             
            |> String.concat ", "
        IO.File.AppendAllLines(csvFile,[output])
        
armorWeaponCSVReport wikiInfos