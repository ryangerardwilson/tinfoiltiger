#!/usr/bin/env python3
import os
import sys
import re
import shutil
import yaml
from modules.generate_template_files_hs_service import generate_template_files_hs_module
from modules.generate_scaffolder_hs_service import generate_scaffolder_hs_module
from modules.generate_template_mappings_hs_service import generate_template_mappings_hs_module


def generate_lib_files_for_project(project_name, project_src_dir, parent_dir):

    def gather_files_from_src(src_dir):
        """
        Recursively gather files in src_dir.
        Returns a list of tuples: (relative_path, variable_name, file_content).
        """

        def build_variable_name(root_relative_path):
            """
            Given a path relative to your resource base, build a Haskell-style variable name.
            Example rules:
              - "main.ml" → "fileMainExtMl"
              - ".env" → "extEnv"
              - "dbs/logs/schema.sql" → "dbsLogsSchema"
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
                        name, _ = filename.rsplit('.', 1)
                        return dir_part + to_camel(name)
                    else:
                        return dir_part + to_camel(filename)

        collected = []
        for root, dirs, files in os.walk(src_dir):
            for f in files:
                try:
                    full_path = os.path.join(root, f)
                    rel_path = os.path.relpath(full_path, src_dir)
                    # Skip unwanted files (you can customize these filters)
                    if rel_path == "app" and '.' not in rel_path:
                        continue
                    if '.db' in rel_path or '.stack-work' in rel_path or 'stack.yaml.lock' in rel_path or 'YourApp.cabal' in rel_path:
                        continue
                    var_name = build_variable_name(rel_path)
                    with open(full_path, 'r', encoding='utf-8') as infile:
                        content = infile.read()
                    collected.append((rel_path, var_name, content))
                except Exception as e:
                    print(f"[ERROR] Could not process file {full_path}: {e}")
        return collected

    print(f"[INFO] Processing project: {project_name}")

    all_files = gather_files_from_src(project_src_dir)
    if not all_files:
        print(f"[WARNING] STEP I - No files found in {project_src_dir}.")
        return

    project_lib_dir = os.path.join(parent_dir, "lib/Templates", project_name)
    os.makedirs(project_lib_dir, exist_ok=True)

    templates_hs_path = os.path.join(project_lib_dir, "TemplateFiles.hs")
    scaffolder_hs_path = os.path.join(project_lib_dir, "Scaffolder.hs")

    generate_template_files_hs_module(all_files, templates_hs_path, project_name)
    generate_scaffolder_hs_module(all_files, scaffolder_hs_path, project_name)
    print(f"[INFO] Finished processing project: {project_name}")


def main():
    print("[INFO] Step I - Preprocessing")

    # First, generate the TemplateMappings.hs module dynamically.
    generate_template_mappings_hs_module()

    # Determine the script directory and the project root (parent directory).
    script_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.abspath(os.path.join(script_dir, os.pardir))

    # CLEAR ALL DIRECTORIES IN THE LIB DIR OF THE PARENT
    # print("[DEBUG] Script directory:", script_dir)
    # print("[DEBUG] Parent directory:", parent_dir)

    # Define the lib directory as a subdirectory of the parent directory.
    lib_dir = os.path.join(parent_dir, "lib/Templates")
    # print("[DEBUG] Lib directory:", lib_dir)

    # Check if the lib directory exists.
    if os.path.exists(lib_dir):
        print(f"[INFO] Clearing directories in: {lib_dir}")
        for item in os.listdir(lib_dir):
            item_path = os.path.join(lib_dir, item)
            # Only remove directories
            if os.path.isdir(item_path):
                try:
                    print(f"[INFO] Removing directory: {item_path}")
                    shutil.rmtree(item_path)
                except Exception as e:
                    print(f"[ERROR] Could not remove {item_path}: {e}")
        print("[INFO] All directories in 'lib' have been cleared.")
    else:
        print(f"[INFO] 'lib' directory not found at {lib_dir}")

    # Since compile.py is running from automations, the templates directory resides at the project root.
    templates_dir = os.path.join(parent_dir, "templates")

    if not os.path.isdir(templates_dir):
        print(f"[ERROR] STEP I - Directory not found at {os.path.abspath(templates_dir)}")
        sys.exit(1)

    # List all subdirectories (projects) within the templates directory.
    projects = [d for d in os.listdir(templates_dir)
                if os.path.isdir(os.path.join(templates_dir, d))]
    if not projects:
        print("[WARNING] STEP I - No projects found in templates/.")
        sys.exit(1)

    # Process each project using the refactored function.
    for project in projects:
        project_src_dir = os.path.join(templates_dir, project)
        generate_lib_files_for_project(project, project_src_dir, parent_dir)

    print("[INFO] Step I - Preprocessing complete")


if __name__ == "__main__":
    main()
