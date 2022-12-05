def is_valid(pp):
    try:
        if len(pp["byr"]) != 4:
            return False
        if int(pp["byr"]) > 2002 or int(pp["byr"]) < 1920:
            return False
        if len(pp["iyr"]) != 4:
            return False
        if int(pp["iyr"]) > 2020 or int(pp["iyr"]) < 2010:
            return False
        if len(pp["eyr"]) != 4:
            return False
        if int(pp["eyr"]) > 2030 or int(pp["eyr"]) < 2020:
            return False
        if pp["hgt"][-2:] not in ("in", "cm"):
            return False
        if pp["hgt"][-2:] == "cm" and not (150 <= int(pp["hgt"][:-2]) <= 193):
            return False
        if pp["hgt"][-2:] == "in" and not (59 <= int(pp["hgt"][:-2]) <= 76):
            return False
        if (
            pp["hcl"][0] != "#"
            or len(pp["hcl"]) != 7
            or not set(pp["hcl"][1:]).issubset(set("0123456789abcdef"))
        ):
            return False
        if pp["ecl"] not in ("amb", "blu", "brn", "gry", "grn", "hzl", "oth"):
            return False
        if len(pp["pid"]) != 9 or int(pp["pid"]) != int(pp["pid"]):
            return False
    except (KeyError, ValueError) as _:
        return False
    return True
