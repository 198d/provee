import re
import sys
import subprocess


def translate_type(type_, output):
    match = re.search(r'typedef (.*) ' + type_ + ';', output)
    if match:
        return translate_type(match.group(1).strip(), output)
    else:
        return type_


def get_members(include, struct_name):
    output = re.sub(r'\#.*\n', '', subprocess.check_output(
        "echo | gcc -E -xc -include '{}' -".format(include),
        shell=True).decode('utf-8'))

    match = re.search(
        r'struct\s+' + struct_name + r'\s+\{[\w\s\[\];*]+\}', output)
    struct_match = match.group(0)

    return [(match.group(2).strip(), match.group(3).strip())
            for match in re.finditer(
                r'(([\w\s\[\]*]+) ([\w\[\]*]+))+;', struct_match)], output


def print_members(include, *struct_names):
    for struct_name in struct_names:
        print('struct {}:'.format(struct_name))
        members, output = get_members(include, struct_name)
        for type_, member in members:
            print('    ' + translate_type(type_, output) + ' ' + member)


print_members('pwd.h', 'passwd')
print_members('grp.h', 'group')
print_members('sys/stat.h', 'stat')
print_members('time.h', 'timespec')
