import json

def json_to_plist(json_data):
    plist = []
    for key, value in json_data.items():
        plist.append(key)
        if isinstance(value, dict):
            plist.append(json_to_plist(value))
        elif isinstance(value, list):
            plist.append('({})'.format(' '.join([json_to_plist(v) if isinstance(v, dict) else '"' + str(v) + '"' if isinstance(v, str) else str(v) for v in value])))
        else:
            plist.append('"' + str(value).encode('utf-8').decode() + '"' if isinstance(value, str) else str(value))  # convert value to string, encode as UTF-8
    return '({})'.format(' '.join(plist))

def main():
    with open('recipes.json', 'r', encoding='utf-8') as file:  # ensure the file is read as UTF-8
        data = json.load(file)

    plists = [json_to_plist(item) for item in data]

    # print the plists
    for plist in plists:
        print(plist)

if __name__ == "__main__":
    main()
