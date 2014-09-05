/*
 * Copyright 2008 Andrew Riedi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA
 */

#include <windows.h>
#include <wine/unicode.h>
#include <wine/debug.h>
#include "reg.h"

#define ARRAY_SIZE(A) (sizeof(A)/sizeof(*A))

WINE_DEFAULT_DEBUG_CHANNEL(reg);

static const WCHAR empty_wstr[] = {0};

static const WCHAR short_hklm[] = {'H','K','L','M',0};
static const WCHAR short_hkcu[] = {'H','K','C','U',0};
static const WCHAR short_hkcr[] = {'H','K','C','R',0};
static const WCHAR short_hku[] = {'H','K','U',0};
static const WCHAR short_hkcc[] = {'H','K','C','C',0};
static const WCHAR long_hklm[] = {'H','K','E','Y','_','L','O','C','A','L','_','M','A','C','H','I','N','E',0};
static const WCHAR long_hkcu[] = {'H','K','E','Y','_','C','U','R','R','E','N','T','_','U','S','E','R',0};
static const WCHAR long_hkcr[] = {'H','K','E','Y','_','C','L','A','S','S','E','S','_','R','O','O','T',0};
static const WCHAR long_hku[] = {'H','K','E','Y','_','U','S','E','R','S',0};
static const WCHAR long_hkcc[] = {'H','K','E','Y','_','C','U','R','R','E','N','T','_','C','O','N','F','I','G',0};

static const struct
{
    HKEY key;
    const WCHAR *short_name;
    const WCHAR *long_name;
}
root_rels[] =
{
    {HKEY_LOCAL_MACHINE, short_hklm, long_hklm},
    {HKEY_CURRENT_USER, short_hkcu, long_hkcu},
    {HKEY_CLASSES_ROOT, short_hkcr, long_hkcr},
    {HKEY_USERS, short_hku, long_hku},
    {HKEY_CURRENT_CONFIG, short_hkcc, long_hkcc},
};

static const WCHAR type_none[] = {'R','E','G','_','N','O','N','E',0};
static const WCHAR type_sz[] = {'R','E','G','_','S','Z',0};
static const WCHAR type_expand_sz[] = {'R','E','G','_','E','X','P','A','N','D','_','S','Z',0};
static const WCHAR type_binary[] = {'R','E','G','_','B','I','N','A','R','Y',0};
static const WCHAR type_dword[] = {'R','E','G','_','D','W','O','R','D',0};
static const WCHAR type_dword_le[] = {'R','E','G','_','D','W','O','R','D','_','L','I','T','T','L','E','_','E','N','D','I','A','N',0};
static const WCHAR type_dword_be[] = {'R','E','G','_','D','W','O','R','D','_','B','I','G','_','E','N','D','I','A','N',0};
static const WCHAR type_multi_sz[] = {'R','E','G','_','M','U','L','T','I','_','S','Z',0};

static const struct
{
    DWORD type;
    const WCHAR *name;
}
type_rels[] =
{
    {REG_NONE, type_none},
    {REG_SZ, type_sz},
    {REG_EXPAND_SZ, type_expand_sz},
    {REG_BINARY, type_binary},
    {REG_DWORD, type_dword},
    {REG_DWORD_LITTLE_ENDIAN, type_dword_le},
    {REG_DWORD_BIG_ENDIAN, type_dword_be},
    {REG_MULTI_SZ, type_multi_sz},
};

static int reg_printfW(const WCHAR *msg, ...)
{
    va_list va_args;
    int wlen;
    DWORD count, ret;
    WCHAR msg_buffer[8192];

    va_start(va_args, msg);
    vsnprintfW(msg_buffer, 8192, msg, va_args);
    va_end(va_args);

    wlen = lstrlenW(msg_buffer);
    ret = WriteConsoleW(GetStdHandle(STD_OUTPUT_HANDLE), msg_buffer, wlen, &count, NULL);
    if (!ret)
    {
        DWORD len;
        char  *msgA;

        /* On Windows WriteConsoleW() fails if the output is redirected. So fall
         * back to WriteFile(), assuming the console encoding is still the right
         * one in that case.
         */
        len = WideCharToMultiByte(GetConsoleOutputCP(), 0, msg_buffer, wlen,
            NULL, 0, NULL, NULL);
        msgA = HeapAlloc(GetProcessHeap(), 0, len * sizeof(char));
        if (!msgA)
            return 0;

        WideCharToMultiByte(GetConsoleOutputCP(), 0, msg_buffer, wlen, msgA, len,
            NULL, NULL);
        WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), msgA, len, &count, FALSE);
        HeapFree(GetProcessHeap(), 0, msgA);
    }

    return count;
}

static int reg_message(int msg)
{
    static const WCHAR formatW[] = {'%','s',0};
    WCHAR msg_buffer[8192];

    LoadStringW(GetModuleHandleW(NULL), msg, msg_buffer,
        sizeof(msg_buffer)/sizeof(WCHAR));
    return reg_printfW(formatW, msg_buffer);
}

static inline BOOL path_rootname_cmp(const WCHAR *input_path, const WCHAR *rootkey_name)
{
    DWORD length = strlenW(rootkey_name);

    return (!strncmpiW(input_path, rootkey_name, length) &&
            (input_path[length] == 0 || input_path[length] == '\\'));
}

static HKEY path_get_rootkey(const WCHAR *path)
{
    DWORD i;

    for (i = 0; i < ARRAY_SIZE(root_rels); i++)
    {
        if (path_rootname_cmp(path, root_rels[i].short_name) ||
            path_rootname_cmp(path, root_rels[i].long_name))
            return root_rels[i].key;
    }

    return NULL;
}

static HKEY path_open(const WCHAR *path, BOOL create)
{
    LONG err;
    HKEY k;

    k = path_get_rootkey(path);
    if (!k)
    {
        reg_message(STRING_INVALID_KEY);
        return NULL;
    }

    path = strchrW(path, '\\');
    if (path)
        path++;

    if (create)
        err = RegCreateKeyW(k, path, &k);
    else
        err = RegOpenKeyW(k, path, &k);

    if (err != ERROR_SUCCESS)
    {
        reg_message(STRING_CANNOT_FIND);
        return NULL;
    }

    return k;
}

static DWORD wchar_get_type(const WCHAR *type_name)
{
    DWORD i;

    if (!type_name)
        return REG_SZ;

    for (i = 0; i < ARRAY_SIZE(type_rels); i++)
    {
        if (!strcmpiW(type_rels[i].name, type_name))
            return type_rels[i].type;
    }

    return ~0u;
}

static BYTE *wchar_get_data(const WCHAR *input, const DWORD type, const WCHAR separator,
    DWORD *size_out)
{
    DWORD i;

    if (!input)
        input = empty_wstr;

    switch (type)
    {
        case REG_NONE:
        case REG_SZ:
        case REG_EXPAND_SZ:
        {
            BYTE *data;

            i = (strlenW(input) + 1) * sizeof(WCHAR);
            *size_out = i;
            data = HeapAlloc(GetProcessHeap(), 0, i);
            memcpy(data, input, i);
            return data;
        }
        case REG_DWORD:
        case REG_DWORD_BIG_ENDIAN:
        {
            BYTE *data;
            WCHAR *temp;

            if (input[0] == '0' && (input[1] == 'x' || input[1] == 'X'))
                i = strtoulW(input, &temp, 16);
            else
                i = strtoulW(input, &temp, 10);

            if (input[0] == '-' || temp[0] || temp == input)
            {
                reg_message(STRING_INVALID_DWORD);
                return NULL;
            }

            if (i == 0xffffffff)
                WINE_FIXME("Check for integer overflow.\n");

            data = HeapAlloc(GetProcessHeap(), 0, sizeof(DWORD));
            *(DWORD *) data = i;
            *size_out = sizeof(DWORD);
            return data;
        }
        case REG_MULTI_SZ:
        {
            WCHAR *data = HeapAlloc(GetProcessHeap(), 0, (strlenW(input) + 1) * sizeof(WCHAR));
            DWORD p;

            for (i = 0, p = 0; i <= strlenW(input); i++, p++)
            {
                /* If this character is the separator, or no separator has been given and these
                 * characters are "\\0", then add a 0 indicating the end of this string */
                if ( (separator && input[i] == separator) ||
                     (!separator && input[i] == '\\' && input[i + 1] == '0') )
                {
                    /* If it's the first character or the previous one was a separator */
                    if (!p || data[p - 1] == 0)
                    {
                        HeapFree(GetProcessHeap(), 0, data);
                        reg_message(STRING_INVALID_CMDLINE);
                        return NULL;
                    }
                    data[p] = 0;

                    if (!separator)
                        i++;
                }
                else
                    data[p] = input[i];
            }

            /* Add a 0 to the end if the string wasn't "", and it wasn't
             * double-0-terminated already (In the case of a trailing separator) */
            if (p > 1 && data[p - 2])
                data[p++] = 0;

            *size_out = p * sizeof(WCHAR);
            return (BYTE *) data;
        }
        case REG_BINARY:
        {
            BYTE *data = HeapAlloc(GetProcessHeap(), 0, strlenW(input));
            DWORD p, odd;

            for (i = 0; i < strlenW(input); i++)
            {
                if (input[i] >= '0' && input[i] <= '9')
                    data[i] = input[i] - '0';
                else if (input[i] >= 'a' && input[i] <= 'f')
                    data[i] = input[i] - 'a' + 10;
                else if (input[i] >= 'A' && input[i] <= 'F')
                    data[i] = input[i] - 'A' + 10;
                else
                {
                    HeapFree(GetProcessHeap(), 0, data);
                    reg_message(STRING_INVALID_CMDLINE);
                    return NULL;
                }
            }

            odd = i & 1;
            p = i >> 1;
            data += odd;

            for (i = 0; i < p; i++)
                data[i] = (data[i * 2] << 4) | data[i * 2 + 1];

            *size_out = p + odd;
            return data - odd;
        }
        default:
        {
            WINE_FIXME("Add support for registry type: %u\n", type);
            return NULL;
        }
    }
}

static BOOL sane_path(const WCHAR *key)
{
    unsigned int i = strlenW(key);

    if (i < 3 || (key[i - 1] == '\\' && key[i - 2] == '\\'))
    {
        reg_message(STRING_INVALID_KEY);
        return FALSE;
    }

    if (key[0] == '\\' && key[1] == '\\' && key[2] != '\\')
    {
        reg_message(STRING_NO_REMOTE);
        return FALSE;
    }

    return TRUE;
}

static int reg_add(const WCHAR *key_name, const WCHAR *value_name, const BOOL value_empty,
    const WCHAR *type, const WCHAR separator, const WCHAR *data, const BOOL force)
{
    HKEY key = NULL;
    LONG err;

    if (!sane_path(key_name))
        return 1;

    if (value_name && value_empty)
    {
        reg_message(STRING_INVALID_CMDLINE);
        return 1;
    }

    key = path_open(key_name, TRUE);
    if (!key)
        return 1;

    if (value_name || data)
    {
        DWORD size, reg_type;
        BYTE *data_out;

        if (value_name && !value_name[0])
            value_name = NULL;

        if (type && !type[0])
        {
            data = NULL;
            type = NULL;
        }

        if (!force && RegQueryValueExW(key, value_name, NULL, NULL, NULL, NULL) == ERROR_SUCCESS)
        {
            WINE_FIXME("Prompt for overwrite\n");
        }

        reg_type = wchar_get_type(type);
        if (reg_type == ~0u)
        {
            reg_message(STRING_UNSUPPORTED_TYPE);
            RegCloseKey(key);
            return 1;
        }

        data_out = wchar_get_data(data, reg_type, separator, &size);
        if (!data_out)
        {
            RegCloseKey(key);
            return 1;
        }

        err = RegSetValueExW(key, value_name, 0, reg_type, data_out, size);
        HeapFree(GetProcessHeap(), 0, data_out);

        if (err != ERROR_SUCCESS){
            RegCloseKey(key);
            reg_message(STRING_ERROR);
            return 1;
        }
    }

    RegCloseKey(key);
    reg_message(STRING_SUCCESS);
    return 0;
}

static int reg_delete(const WCHAR *key_name, const WCHAR *value_name, const BOOL value_empty,
    const BOOL value_all, const BOOL force)
{
    HKEY key = NULL;
    LONG err;

    if (!sane_path(key_name))
        return 1;

    /* Mutually exclusive options */
    if ((!!value_name + !!value_empty + !!value_all) > 1)
    {
        reg_message(STRING_INVALID_CMDLINE);
        return 1;
    }

    key = path_open(key_name, FALSE);
    if (!key)
        return 1;

    if (!force)
    {
        WINE_FIXME("Prompt for delete\n");
    }

    if (value_empty || value_name)
    {
        if (value_name && value_name[0])
            err = RegDeleteValueW(key, value_name);
        else
            err = RegDeleteValueW(key, NULL);

        if (err != ERROR_SUCCESS)
        {
            RegCloseKey(key);
            reg_message(STRING_ERROR);
            return 1;
        }
    }
    else if (value_all)
    {
        WCHAR *enum_v_name;
        DWORD count, max_size, this_size, i = 0, errors = 0;

        err = RegQueryInfoKeyW(key, NULL, NULL, NULL, NULL, NULL, NULL,
            &count, &max_size, NULL, NULL, NULL);
        if (err != ERROR_SUCCESS)
        {
            RegCloseKey(key);
            reg_message(STRING_ERROR);
            return 1;
        }

        enum_v_name = HeapAlloc(GetProcessHeap(), 0, ++max_size * sizeof(WCHAR));

        while (i < count)
        {
            this_size = max_size;

            err = RegEnumValueW(key, i, enum_v_name, &this_size, NULL, NULL, NULL, NULL);
            if (err != ERROR_SUCCESS)
            {
                i++;
                errors++;
                continue;
            }

            err = RegDeleteValueW(key, enum_v_name);
            if (err != ERROR_SUCCESS)
            {
                i++;
                errors++;
                continue;
            }

            count--;
        }

        HeapFree(GetProcessHeap(), 0, enum_v_name);

        if (errors)
        {
            RegCloseKey(key);
            reg_message(STRING_ERROR);
            return 1;
        }
    }
    /* Delete subtree only if no /v* option is given */
    else
    {
        if (key == path_get_rootkey(key_name))
        {
            /* "This works well enough on native to make you regret you pressed enter" - stefand */
            WINE_FIXME("Deleting a root key is not implemented.\n");
            RegCloseKey(key);
            return 1;
        }

        if (RegDeleteTreeW(key, NULL) != ERROR_SUCCESS || RegDeleteKeyW(key, empty_wstr))
        {
            RegCloseKey(key);
            reg_message(STRING_ERROR);
            return 1;
        }
    }

    RegCloseKey(key);
    reg_message(STRING_SUCCESS);
    return 0;
}

static int reg_query(WCHAR *key_name, WCHAR *value_name, BOOL value_empty,
    BOOL subkey)
{
    static const WCHAR stubW[] = {'S','T','U','B',' ','Q','U','E','R','Y',' ',
        '-',' ','%','s',' ','%','s',' ','%','d',' ','%','d','\n',0};
    reg_printfW(stubW, key_name, value_name, value_empty, subkey);

    return 1;
}

int wmain(int argc, WCHAR *argvW[])
{
    int i;

    static const WCHAR addW[] = {'a','d','d',0};
    static const WCHAR deleteW[] = {'d','e','l','e','t','e',0};
    static const WCHAR queryW[] = {'q','u','e','r','y',0};
    static const WCHAR slashDW[] = {'/','d',0};
    static const WCHAR slashFW[] = {'/','f',0};
    static const WCHAR slashHW[] = {'/','h',0};
    static const WCHAR slashSW[] = {'/','s',0};
    static const WCHAR slashTW[] = {'/','t',0};
    static const WCHAR slashVW[] = {'/','v',0};
    static const WCHAR slashVAW[] = {'/','v','a',0};
    static const WCHAR slashVEW[] = {'/','v','e',0};
    static const WCHAR slashHelpW[] = {'/','?',0};

    if (argc < 2 || !lstrcmpW(argvW[1], slashHelpW)
                 || !lstrcmpiW(argvW[1], slashHW))
    {
        reg_message(STRING_USAGE);
        return 0;
    }

    if (!lstrcmpiW(argvW[1], addW))
    {
        WCHAR *key_name, *value_name = NULL, *type = NULL, separator = '\0', *data = NULL;
        BOOL value_empty = FALSE, force = FALSE;

        if (argc < 3)
        {
            reg_message(STRING_INVALID_CMDLINE);
            return 1;
        }
        else if (argc == 3 && (!lstrcmpW(argvW[2], slashHelpW) ||
                               !lstrcmpiW(argvW[2], slashHW)))
        {
            reg_message(STRING_ADD_USAGE);
            return 0;
        }

        key_name = argvW[2];
        for (i = 1; i < argc; i++)
        {
            if (!lstrcmpiW(argvW[i], slashVW))
                value_name = argvW[++i];
            else if (!lstrcmpiW(argvW[i], slashVEW))
                value_empty = TRUE;
            else if (!lstrcmpiW(argvW[i], slashTW))
                type = argvW[++i];
            else if (!lstrcmpiW(argvW[i], slashSW))
            {
                if (!argvW[++i][0] || argvW[i][1])
                {
                    reg_message(STRING_INVALID_CMDLINE);
                    return 1;
                }
                separator = argvW[i][0];
            }
            else if (!lstrcmpiW(argvW[i], slashDW))
                data = argvW[++i];
            else if (!lstrcmpiW(argvW[i], slashFW))
                force = TRUE;
        }
        return reg_add(key_name, value_name, value_empty, type, separator,
            data, force);
    }
    else if (!lstrcmpiW(argvW[1], deleteW))
    {
        WCHAR *key_name, *value_name = NULL;
        BOOL value_empty = FALSE, value_all = FALSE, force = FALSE;

        if (argc < 3)
        {
            reg_message(STRING_INVALID_CMDLINE);
            return 1;
        }
        else if (argc == 3 && (!lstrcmpW(argvW[2], slashHelpW) ||
                               !lstrcmpiW(argvW[2], slashHW)))
        {
            reg_message(STRING_DELETE_USAGE);
            return 0;
        }

        key_name = argvW[2];
        for (i = 1; i < argc; i++)
        {
            if (!lstrcmpiW(argvW[i], slashVW))
                value_name = argvW[++i];
            else if (!lstrcmpiW(argvW[i], slashVEW))
                value_empty = TRUE;
            else if (!lstrcmpiW(argvW[i], slashVAW))
                value_all = TRUE;
            else if (!lstrcmpiW(argvW[i], slashFW))
                force = TRUE;
        }
        return reg_delete(key_name, value_name, value_empty, value_all, force);
    }
    else if (!lstrcmpiW(argvW[1], queryW))
    {
        WCHAR *key_name, *value_name = NULL;
        BOOL value_empty = FALSE, subkey = FALSE;

        if (argc < 3)
        {
            reg_message(STRING_INVALID_CMDLINE);
            return 1;
        }
        else if (argc == 3 && (!lstrcmpW(argvW[2], slashHelpW) ||
                               !lstrcmpiW(argvW[2], slashHW)))
        {
            reg_message(STRING_QUERY_USAGE);
            return 0;
        }

        key_name = argvW[2];
        for (i = 1; i < argc; i++)
        {
            if (!lstrcmpiW(argvW[i], slashVW))
                value_name = argvW[++i];
            else if (!lstrcmpiW(argvW[i], slashVEW))
                value_empty = TRUE;
            else if (!lstrcmpiW(argvW[i], slashSW))
                subkey = TRUE;
        }
        return reg_query(key_name, value_name, value_empty, subkey);
    }
    else
    {
        reg_message(STRING_INVALID_CMDLINE);
        return 1;
    }
}
