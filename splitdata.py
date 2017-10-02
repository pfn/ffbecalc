#!/usr/bin/python

import json
import os
import itertools

units   = json.load(file("units.json"))
equip   = json.load(file("equipment.json"))
enhance = json.load(file("enhancements.json"))
lb      = json.load(file("limitbursts.json"))
espers  = json.load(file("summons.json"))
esperbs = json.load(file("summons_boards.json"))
skills  = json.load(file("skills.json"))
materia = json.load(file("materia.json"))

materiadex = {}
unitdex    = {}
esperdex   = {}
equipdex   = {}
equipnames = set()

def mkdir(name):
  if not os.access(name, os.X_OK):
    os.makedirs(name)

def hasLB(x):
  return "limitburst_id" in x and x["limitburst_id"] is not None

mkdir("json/unit")
mkdir("json/equip")
mkdir("json/enhance")
mkdir("json/esper")
mkdir("json/esperboard")
mkdir("json/lb")
mkdir("json/skill")
mkdir("json/materia")

for x in units.keys():
  if units[x]["name"] != "<na>" and units[x]["job"] is not None:
    if all(itertools.imap(hasLB, units[x]["entries"].values())):
      unitdex[units[x]['name']] = {
        "id": x,
        "min": units[x]["rarity_min"],
        "max": units[x]["rarity_max"]
      }
      units[x]["id"] = x
      json.dump(units[x], file("json/unit/%s.json" % x, "w"))

for x in equip.keys():
  name = equip[x]["name"]
  name2 = name
  compid = equip[x]["compendium_id"]
  if name in equipnames:
    name2 = "%s (%s)" % (name2, compid)
    if name in equipdex:
      cid = equipdex[name]["compid"]
      equipdex["%s (%s)" % (name, cid)] = equipdex[name]
      del equipdex[name]
  skillns = {}
  skillr = []
  eff = []
  effr = []
  reqs = []
  if "requirements" in equip[x]:
    reqs = equip[x]["requirements"]
  if "skills" in equip[x] and equip[x]["skills"] is not None:
    for skillid in equip[x]["skills"]:
      restriction = None
      if "active" not in skills[str(skillid)] or not skills[str(skillid)]["active"]:
        if "unit_restriction" in skills[str(skillid)]:
          restriction = skills[str(skillid)]["unit_restriction"]
        effr += [{
          "unit_restriction": restriction,
          "effects": skills[str(skillid)]["effects_raw"]
        }]
      skillns[(skills[str(skillid)]["name"])] = skills[str(skillid)]["effects"]
  equipdex[name2] = {
    "id": x,
    "compid": compid,
    "type_id": equip[x]["type_id"],
    "slot_id": equip[x]["slot_id"],
    "stats": equip[x]["stats"],
    "skills": equip[x]["skills"],
    "skill_restriction": skillr,
    "reqs": reqs,
    "effects_raw": effr,
    "skill_effects": skillns
  }
  equipnames.add(name)
  json.dump(equip[x], file("json/equip/%s.json" % x, "w"))

for x in materia.keys():
  dex = {
    "id": x,
  }
  materiadex[materia[x]['name']] = dex
  if "skills" in materia[x]:
    eff = []
    effr = []
    skillns = []
    for skillid in materia[x]["skills"]:
      skid = str(skillid)
      eff += skills[skid]["effects"]
      if "active" not in skills[str(skillid)] or not skills[str(skillid)]["active"]:
        restriction = None
        if "unit_restriction" in skills[str(skid)]:
          restriction = skills[str(skid)]["unit_restriction"]
        effr += [{
          "unit_restriction": restriction,
          "effects": skills[skid]["effects_raw"]
        }]
      skillns += [skills[skid]["name"]]
      dex["rarity"] = skills[skid]["rarity"]
      if "magic_type" in skills[skid]:
        dex["magic_type"] = skills[skid]["magic_type"]

    dex["effects"] = eff
    dex["effects_raw"] = effr
    dex["skill_names"] = skillns
  json.dump(materia[x], file("json/materia/%s.json" % x, "w"))

for x in enhance.keys():
  json.dump(enhance[x], file("json/enhance/%s.json" % x, "w"))

for x in lb.keys():
  json.dump(lb[x], file("json/lb/%s.json" % x, "w"))

for x in skills.keys():
  skills[x]["id"] = x
  json.dump(skills[x], file("json/skill/%s.json" % x, "w"))

for x in espers.keys():
  if "names" in espers[x] and x in esperbs and espers[x]["names"] is not None:
    esperdex[espers[x]['names'][0]] = x
    json.dump(espers[x], file("json/esper/%s.json" % x, "w"))
    json.dump(esperbs[x], file("json/esperboard/%s.json" % x, "w"))
  
json.dump(unitdex,    file("json/unit/index.json",    "w"), indent=2)
json.dump(equipdex,   file("json/equip/index.json",   "w"), indent=2)
json.dump(esperdex,   file("json/esper/index.json",   "w"), indent=2)
json.dump(materiadex, file("json/materia/index.json", "w"), indent=2)
