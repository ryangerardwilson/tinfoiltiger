#!/usr/bin/env python3
import os
import sys
import re
import shutil
import yaml


def generate_template_files_hs_module(file_paths, output_path, project_name):
    """
    file_paths: a list of tuples. Each tuple can have either:
       (path, comment) or (path, comment, file_content)
    output_path: where to write the Templates.hs file.
    project_name: project name, used for naming the module and constructing embed paths.
    """

    def to_camel(s, capitalize_first=True):
        """Convert a string to CamelCase."""
        parts = re.split(r'[\s_-]+', s)
        if not parts:
            return ""
        if capitalize_first:
            return ''.join(word.capitalize() for word in parts)
        else:
            return parts[0].lower() + ''.join(word.capitalize() for word in parts[1:])

    def build_variable_name(root_relative_path):
        """
        Build a Haskell variable name from a file path.
        Examples:
          "main.ml" → "fileMainExtMl"
          ".env" → "extEnv"
          "dbs/logs/schema.sql" → "dbsLogsSchema"
        """
        parts = root_relative_path.split(os.sep)
        if len(parts) == 1:
            filename = parts[0]
            if filename.startswith('.'):
                base = filename[1:]
                return "ext" + to_camel(base)
            else:
                if '.' in filename:
                    name, ext = filename.rsplit('.', 1)
                    return "file" + to_camel(name) + "Ext" + to_camel(ext)
                else:
                    return "file" + to_camel(filename)
        else:
            dirs = parts[:-1]
            filename = parts[-1]
            first_dir = dirs[0].lower()
            remaining_dirs = "".join(to_camel(d, capitalize_first=True) for d in dirs[1:])
            dir_part = first_dir + remaining_dirs
            if filename.startswith('.'):
                base = filename[1:]
                return "ext" + to_camel(base)
            else:
                if '.' in filename:
                    name, ext = filename.rsplit('.', 1)
                    # For subdirectory files, we omit the extension for the variable name.
                    return dir_part + to_camel(name)
                else:
                    return dir_part + to_camel(filename)

    # Create the module header.
    header = (
        "{-# LANGUAGE TemplateHaskell #-}\n"
        "{-# LANGUAGE CPP #-}\n"
        f"module Templates.{project_name}.TemplateFiles\n"
        "  ( "    # export list; we will list the individual variables and aggregated templates value.
    )

    # Lists to hold variable names and also the aggregated tuples.
    var_names = []
    aggregated_tuples = []
    embed_lines = []

    for entry in file_paths:
        # Unpack each tuple.
        if len(entry) == 2:
            file_path, comment = entry
        elif len(entry) >= 3:
            file_path, comment, _ = entry   # ignore file_content if present
        else:
            raise ValueError("Each tuple in file_paths must have at least 2 elements.")

        var_name = build_variable_name(file_path)
        var_names.append(var_name)

        # IMPORTANT: All files are now relative to the templates folder.
        # Embed path is always "templates/<project_name>/<file_path>"
        embed_path = f"templates/{project_name}/{file_path}"

        aggregated_tuples.append((embed_path, var_name))

        # Construct the TH embedding line.
        embed_line = f'{var_name} :: ByteString\n'
        embed_line += f'{var_name} = $(embedFile "{embed_path}")'
        if comment:
            embed_line += f'  -- {comment}'
        embed_lines.append(embed_line)

    export_list = ", ".join(var_names) + ", templates"
    header += export_list + "\n  ) where\n\n"

    imports = (
        "import Data.ByteString (ByteString)\n"
        "import Data.FileEmbed (embedFile)\n\n"
    )

    body = "\n\n".join(embed_lines)

    # Create the aggregated 'templates' list.
    template_list_lines = []
    template_list_lines.append("templates :: [(FilePath, ByteString)]")
    template_list_lines.append("templates =")
    template_list_lines.append("  [")
    for (embed_path, var_name) in aggregated_tuples:
        template_list_lines.append(f'    ("{embed_path}", {var_name}),')
    # Remove trailing comma from the last entry if any entries exist.
    if len(aggregated_tuples) > 0:
        template_list_lines[-1] = template_list_lines[-1].rstrip(',')
    template_list_lines.append("  ]")

    aggregated_block = "\n".join(template_list_lines)

    module_text = header + imports + body + "\n\n" + aggregated_block + "\n"
    # module_text = header + imports + body

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(module_text)

    print(f"[INFO] STEP I - Generated {output_path}")
