from os import listdir
from os.path import isfile, join

test_src_path = "./tests/all/good"
test_entries = [(f.replace(".lat", "").replace("_", " "), join(test_src_path, f), join(test_src_path, f).replace(".lat", ".output")) for f in listdir(test_src_path) if isfile(join(test_src_path, f)) and isfile(join(test_src_path, f.replace(".lat", ".output"))) and f.endswith(".lat") and "core" not in f]

def format_lines(text):
    return '\n'.join(['    '+line for line in text.split("\n") if len(line.strip()) > 0])

for (name, lat, out) in test_entries:
    with open(out, "r") as out_in:
        with open(lat, "r") as lat_in:
            lat_str = lat_in.read()
            out_str = out_in.read()
            print(f"""
it "{name}" $ \h -> expectProgramSuccess [r|
{format_lines(lat_str)}
|] [r|
{format_lines(out_str)}
|]""")