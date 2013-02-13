#!/usr/bin/env python
# Python script for analyzing ELF object files
# See http://www.muppetlabs.com/~breadbox/software/ELF.txt
# for information about the Executable and Linkable Format (ELF)

import sys, os, string, types

elf_file = { }
lookupThings = [ ]

# The kinds of fields we see in elf file structs.
(INTEGER, STRINGP, STRING) = range(3)

R = open(sys.argv[1]).read()
bytes = [ ]
for x in list(R):
    y = ord(x)
    if y < 0: y = y + 256
    bytes.append(y)

if R[:4] != "\177ELF":
    raise Exception("This doesn't look like an ELF file!")

def get_struct(pointer, struct_descriptor, namespace=None):
    if namespace == None:
        namespace = { }
    namespace['struct_type'] = struct_descriptor
    for tup in struct_descriptor:
        fieldType, fieldName, numBytes = tup[:3]
        if fieldType == INTEGER or fieldType == STRINGP:
            shift = 0
            value = 0
            for i in range(numBytes):
                # assume little-endian
                value = (bytes[pointer] << shift) + value
                pointer = pointer + 1
                shift = shift + 8
            namespace[fieldName] = value
        elif fieldType == STRING:
            expval = ""
            value = ""
            for i in range(numBytes):
                value = value + chr(bytes[pointer])
                pointer = pointer + 1
            namespace[fieldName] = value
        else:
            raise Exception('bad field type')
    return (pointer, namespace)

def get_string(offset, strtab_index):
    z = offset
    # Find the address of the string table (a). e_shoff is
    # where the the section headers start, 40 is the size of a
    # section header, 16 is the offset into the section header
    # for the offset of the string table.
    a = elf_file['e_shoff'] + 40 * strtab_index + 16
    z = z + bytes[a] + (bytes[a+1] << 8) + \
        (bytes[a+2] << 16) + (bytes[a+3] << 24)
    str = ""
    while 1:
        ch = bytes[z]
        if ch == 0:
            return str
        str = str + chr(ch)
        z = z + 1

def interpret_field(fieldName, fieldType, value, strtab_index=None):
    def printable(str):
        r = ""
        for i in range(len(str)):
            c = ord(str[i])
            if c >= 32 and c < 127:
                c = chr(c)
            else:
                c = '\\%03o' % c
            r = r + c
        return r
    if strtab_index == None:
        strtab_index = elf_file['e_shstrndx']
    myLookup = None
    for lookup in lookupThings:
        if lookup['fieldName'] == fieldName:
            myLookup = lookup
            break
    if fieldType == INTEGER:
        r = '0x%X %d' % (value, value)
        if myLookup != None:
            try:
                r = r + ': ' + myLookup['func'](value)
            except KeyError:
                try:
                    r = r + ': ' + myLookup[value]
                except KeyError:
                    pass
        return r
    elif fieldType == STRINGP:
        return ('0x%X %d: "%s"' %
                (value, value,
                 printable(get_string(value, strtab_index))))
    elif fieldType == STRING:
        return '"' + printable(value) + '"'

def show_block(offset, size, alpha=0):
    j = 0
    while j < size:
        print '%05X %04X:' % (offset, j),
        while j < size:
            ch = bytes[offset]
            offset = offset + 1
            j = j + 1
            if alpha and ch > 0x20 and ch < 0x7F:
                ch = chr(ch) + ' '
            else:
                ch = '%02X' % ch
            print ch,
            if (j & 15) == 0:
                break
        print

def dump_struct(name, structdict, indentlevel=0, strtab_index=None):
    struct_type = structdict['struct_type']
    if struct_type == None:
        strtab_index = elf_file['e_shstrndx']
    indent = indentlevel * '  '
    print indent + name
    for k in structdict.keys():
        if k != 'struct_type':
            for tup in struct_type:
                if tup[1] == k:
                    fieldType = tup[0]
                    break
            print indent + '  ' + k + ': ' + \
                  interpret_field(k, fieldType,
                                  structdict[k], strtab_index)

#################################################

elf_file_header = [
    # field_type, field_name, number_of_bytes, [expected_value]
    (STRING, 'e_ident', 16),
    (INTEGER, 'e_type', 2),
    (INTEGER, 'e_machine', 2),
    (INTEGER, 'e_version', 4),
    (INTEGER, 'e_entry', 4),
    (INTEGER, 'e_phoff', 4),
    (INTEGER, 'e_shoff', 4),
    (INTEGER, 'e_flags', 4),
    (INTEGER, 'e_ehsize', 2),
    (INTEGER, 'e_phentsize', 2),
    (INTEGER, 'e_phnum', 2),
    (INTEGER, 'e_shentsize', 2),
    (INTEGER, 'e_shnum', 2),
    (INTEGER, 'e_shstrndx', 2),
    ]

# Oops, this appears to be architecture-dependent!
# Better skip it for now.
#
# def e_flags_interpret(flags):
#     r = ''
#     if flags & 1: r = r + 'EF_MIPS_NOREORDER '
#     if flags & 2: r = r + 'EF_MIPS_PIC '
#     if flags & 4: r = r + 'EF_MIPS_CPIC '
#     if flags & 8: r = r + 'EF_MIPS_XGOT '
#     if flags & 16: r = r + 'EF_MIPS_64BIT_WHIRL '
#     if flags & 32: r = r + 'EF_MIPS_ABI2 '
#     if flags & 64: r = r + 'EF_MIPS_ABI_ON32 '
#     # if flags & 0xf0000000: r = r + 'EF_MIPS_ARCH '
#     return r[:-1]
# lookupThings.append({
#     'fieldName': 'e_flags',
#     'func': e_flags_interpret
#     })

e_type_types = {
    'fieldName': 'e_type',
    0: 'ET_NONE',
    1: 'ET_REL',
    2: 'ET_EXEC',
    3: 'ET_DYN',
    4: 'ET_CORE',
    5: 'ET_NUM',
    0xff00: 'ET_LOPROC',
    0xffff: 'ET_HIPROC'
    }
lookupThings.append(e_type_types)

e_machine_types = {
    'fieldName': 'e_machine',
    0: 'EM_NONE',
    1: 'EM_M32',
    2: 'EM_SPARC',
    3: 'EM_386',
    4: 'EM_68K',
    5: 'EM_88K',
    6: 'EM_486',
    7: 'EM_860',
    8: 'EM_MIPS',
    9: 'EM_S370',
    10: 'EM_MIPS_RS4_BE',
    11: 'EM_RS6000',
    0xE813: 'Equator'}
lookupThings.append(e_machine_types)

######################################################
# Program Header

program_header = [
    (INTEGER, 'p_type', 4),
    (INTEGER, 'p_offset', 4),
    (INTEGER, 'p_vaddr', 4),
    (INTEGER, 'p_paddr', 4),
    (INTEGER, 'p_filesz', 4),
    (INTEGER, 'p_memsz', 4),
    (INTEGER, 'p_flags', 4),
    (INTEGER, 'p_align', 4)
    ]

p_header_types = {
    'fieldName': 'p_type',
    0: 'PT_NULL',
    1: 'PT_LOAD',
    2: 'PT_DYNAMIC',
    3: 'PT_INTERP',
    4: 'PT_NOTE',
    5: 'PT_SHLIB',
    6: 'PT_PHDR',
    7: 'PT_NUM',
    0x60000000: 'PT_LOOS',
    0x6fffffff: 'PT_HIOS',
    0x70000000: 'PT_LOPROC',
    0x7fffffff: 'PT_HIPROC',
    }
lookupThings.append(p_header_types)

##############################################
# Section header

section_header = [
    (STRINGP, 'sh_name', 4),
    (INTEGER, 'sh_type', 4),
    (INTEGER, 'sh_flags', 4),
    (INTEGER, 'sh_addr', 4),
    (INTEGER, 'sh_offset', 4),
    (INTEGER, 'sh_size', 4),
    (INTEGER, 'sh_link', 4),
    (INTEGER, 'sh_info', 4),
    (INTEGER, 'sh_addralign', 4),
    (INTEGER, 'sh_entsize', 4)
    ]

section_header_types = {
    'fieldName': 'sh_type',
    0: 'SHT_NULL',
    1: 'SHT_PROGBITS',
    2: 'SHT_SYMTAB',
    3: 'SHT_STRTAB',
    4: 'SHT_RELA',
    5: 'SHT_HASH',
    6: 'SHT_DYNAMIC',
    7: 'SHT_NOTE',
    8: 'SHT_NOBITS',
    9: 'SHT_REL',
    10: 'SHT_SHLIB',
    11: 'SHT_DYNSYM',
    12: 'SHT_NUM',
    0x60000000: 'SHT_LOOS',
    0x6ffffffb: 'SHT_LOSUNW',
    # 0x6ffffffb: SHT_SUNW_COMDAT ??
    0x6ffffffc: 'SHT_SUNW_syminfo',
    0x6ffffffd: 'SHT_GNU_verdef',
    0x6ffffffe: 'SHT_GNU_verneed',
    0x6fffffff: 'SHT_GNU_versym',
    0x6fffffff: 'SHT_HISUNW',
    0x6fffffff: 'SHT_HIOS',
    0x70000000: 'SHT_LOPROC',
    0x7fffffff: 'SHT_HIPROC',
    0x80000000: 'SHT_LOUSER',
    0x8fffffff: 'SHT_HIUSER',
    }
lookupThings.append(section_header_types)

def sh_flags_interpret(flags):
    r = ''
    if flags & 0x1: r = r + 'SHF_WRITE '
    if flags & 0x2: r = r + 'SHF_ALLOC '
    if flags & 0x4: r = r + 'SHF_EXECINSTR '
    # if flags & 0xf0000000: r = r + 'SHF_MASKPROC '
    return r[:-1]
lookupThings.append({
    'fieldName': 'sh_flags',
    'func': sh_flags_interpret
    })

##############################################
# Symbol table

elf_symbol = [
    (STRINGP, 'st_name', 4),
    (INTEGER, 'st_value', 4),
    (INTEGER, 'st_size', 4),
    (INTEGER, 'st_info', 1),
    (INTEGER, 'st_other', 1),
    (INTEGER, 'st_shndx', 2)
    ]

symbol_bindings = {
    0: 'STB_LOCAL',
    1: 'STB_GLOBAL',
    2: 'STB_WEAK',
    3: 'STB_NUM',
    10: 'STB_LOOS',
    12: 'STB_HIOS',
    13: 'STB_LOPROC',
    15: 'STB_HIPROC',
    }

symbol_types = {
    0: 'STT_NOTYPE',
    1: 'STT_OBJECT',
    2: 'STT_FUNC',
    3: 'STT_SECTION',
    4: 'STT_FILE',
    5: 'STT_NUM',
    11: 'STT_LOOS',
    12: 'STT_HIOS',
    13: 'STT_LOPROC',
    15: 'STT_HIPROC',
    }

def st_info_interpret(info):
    return (symbol_bindings[info >> 4] + ' ' +
            symbol_types[info & 0x0F])
lookupThings.append({
    'fieldName': 'st_info',
    'func': st_info_interpret
    })

##############################################
# Relocation entries

elf32_rel = [
    (INTEGER, 'r_offset', 4),
    (INTEGER, 'r_info', 4),
    ]

elf32_rela = [
    (INTEGER, 'r_offset', 4),
    (INTEGER, 'r_info', 4),
    (INTEGER, 'r_addend', 4),   # signed integer
    ]

def r_info_interpret(info):
    return ('sym=%d type=%d' %
            (info >> 8, info & 0xFF))
lookupThings.append({
    'fieldName': 'r_info',
    'func': r_info_interpret
    })

##############################################
# Expand out the more interesting sections

def ascii_block(d):
    show_block(d['sh_offset'], d['sh_size'], 1)

def number_block(d):
    show_block(d['sh_offset'], d['sh_size'])

def struct_table(d, struct_name, struct_type, struct_size):
    for i in range(d['sh_size'] / struct_size):
        dummy, e = get_struct(d['sh_offset'] + i * struct_size, struct_type)
        dump_struct(struct_name + ' %d' % i,
                    e, 3, d['sh_link'])

expansions = (
    (".strtab", ascii_block),
    (".rodata", number_block),
    (".data", number_block),
    (".symtab", struct_table, 'Sym', elf_symbol, 16),
    (".rela.", struct_table, 'Rela', elf32_rela, 12),
    (".rel.", struct_table, 'Rel', elf32_rel, 8),
    )

##############################################

pointer, dummy = get_struct(0, elf_file_header, elf_file)
dump_struct(sys.argv[1], elf_file, 0)

pointer = elf_file['e_phoff']
for i in range(elf_file['e_phnum']):
    pointer, d = get_struct(pointer, program_header)
    dump_struct('Program Section %d' % i, d, 1)

pointer = elf_file['e_shoff']
for i in range(elf_file['e_shnum']):
    pointer, d = get_struct(pointer, section_header)
    dump_struct('Section %d' % i, d, 1)
    section_name = get_string(d['sh_name'],
                              elf_file['e_shstrndx'])
    for e in expansions:
        if len(section_name) >= len(e) and \
           e[0] == section_name[:len(e[0])]:
            apply(e[1], (d,) + e[2:])
