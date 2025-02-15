#!/usr/bin/env python3
import os
import sys
import re
import shutil


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


def generate_templates_hs_module(file_paths, output_path, project_name):
    """
    Given a list of file paths (relative to some base such as "src_project/"),
    generate a Haskell module called Templates (in a project-specific namespace)
    that declares variables for each file using file embedding.

    file_paths: a list of tuples. Each tuple can have either:
       (path, comment) or (path, comment, file_content)
    This function will use the first element (the file path) and the second element as a comment.

    output_path: the file system path where the Templates.hs file should be written.
    project_name: the project name to use as module name prefix (e.g. "QuickStartUpA").
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
        Given a path relative to your resource base, build a Haskell variable name.
        Example rules:
          - "main.ml" → "fileMainExtMl"
          - ".env" → "extEnv"
          - "dbs/logs/schema.sql" → "dbsLogsSchema"
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
                    # For subdirectory files, we omit the extension.
                    return dir_part + to_camel(name)
                else:
                    return dir_part + to_camel(filename)

    # Create a module header using the project name.
    header = (
        "{-# LANGUAGE TemplateHaskell #-}\n"
        "{-# LANGUAGE CPP #-}\n"
        f"module {project_name}.Templates\n"
        "  ( "  # we could list exports here
    )
    # Collect all variable names for the export list.
    var_names = []
    embed_lines = []
    for entry in file_paths:
        # Depending on tuple length, unpack appropriately.
        if len(entry) == 2:
            file_path, comment = entry
        elif len(entry) >= 3:
            file_path, comment, _ = entry   # ignore the third element even if present
        else:
            raise ValueError("Each tuple in file_paths must have at least 2 elements.")
        var_name = build_variable_name(file_path)
        var_names.append(var_name)
        # Create an embedding line:
        embed_line = f'{var_name} :: ByteString\n'
        embed_line += f'{var_name} = $(embedFile "templates/{project_name}/{file_path}")'
        if comment:
            embed_line += f'  -- {comment}'
        embed_lines.append(embed_line)

    export_list = ", ".join(var_names)
    header += export_list + "\n  ) where\n\n"

    imports = (
        "import Data.ByteString (ByteString)\n"
        "import Data.FileEmbed (embedFile)\n\n"
    )

    body = "\n\n".join(embed_lines)

    module_text = header + imports + body + "\n"

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(module_text)

    print(f"[INFO] STEP I - Generated {output_path}")


def generate_scaffolder_hs_module(gathered_files, output_path, project_name):
    """
    Generates the Haskell module for Scaffolder, dynamically building a
    list of templates from the gathered files.

    gathered_files: A list of tuples (rel_path, variable_name, file_content).
      (The file_content is not embedded here; we assume that the variables are
       defined in the Templates module; we simply reference them by variable_name.)
    output_path: The path where the Scaffolder.hs file will be written
                 (e.g., "lib/ProjectA/Scaffolder.hs").
    project_name: The name of the project, so that the module is declared as
                  "<ProjectName>.Scaffolder"
    """

    module_name = f"{project_name}.Scaffolder"
    templates_module_name = f"{project_name}.Templates"

    header = f"""{{-# LANGUAGE CPP #-}}
module {module_name}
  ( scaffold
  , writeFileWithInfo
  , ensureDir
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
-- Note: we now import doesDirectoryExist and removeDirectoryRecursive.
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive )
import System.FilePath ((</>), takeDirectory, takeBaseName)
import Control.Monad ( when )
import System.Process ( callCommand )
import System.IO (stdout, hFlush)
import Data.List ( isPrefixOf )
import Data.Time ( getCurrentTime, utctDay )
import Data.Time.Calendar ( toGregorian )


import {templates_module_name}

-- | Update the package.yaml file with the values provided by the user.
-- For lines starting with:
--
--   name:         --> replaced with the app name (inferred from the target directory)
--   author:       --> replaced with the author name (with escaped quotes)
--   maintainer:   --> replaced with the maintainer's email (with escaped quotes)
--   copyright:    --> replaced with "<currentYear> <author>" (with escaped quotes)
--   github:       --> replaced with the git remote URL (with escaped quotes)
--   description:  --> replaced with the short project description
--
-- Finally, if any dependency line contains the token "YourApp",
-- that token is replaced with the new app name.
updatePackageYaml :: BS.ByteString  -- original package.yaml contents
                  -> String         -- appName (project name, inferred)
                  -> String         -- Author name
                  -> String         -- Git remote URL
                  -> String         -- Maintainer email
                  -> String         -- Project description
                  -> String         -- Current year
                  -> BS.ByteString  -- updated package.yaml contents
updatePackageYaml content appName author gitRemote maintainer description year =
  let s = BS8.unpack content
      ls = lines s
      ls' = map (\\line ->
                if "name:" `isPrefixOf` line
                  then "name:                " ++ appName
                else if "author:" `isPrefixOf` line
                  then "author:              \\\"" ++ author ++ "\\\""
                else if "maintainer:" `isPrefixOf` line
                  then "maintainer:          \\\"" ++ maintainer ++ "\\\""
                else if "copyright:" `isPrefixOf` line
                  then "copyright:           \\\"" ++ year ++ " " ++ author ++ "\\\""
                else if "github:" `isPrefixOf` line
                  then "github:              \\\"" ++ gitRemote ++ "\\\""
                else if "description:" `isPrefixOf` line
                  then "description:         " ++ description
                else line
              ) ls
      ls'' = map (\\l ->
                let trimmed = dropWhile (== ' ') l in
                if trimmed == "- YourApp"
                  then let prefix = takeWhile (== ' ') l in prefix ++ "- " ++ appName
                  else l
              ) ls'
  in BS8.pack (unlines ls'')
"""
    # Build the templates list using each tuple’s relative path and variable name.
    templates_lines = []
    for rel_path, var_name, _ in gathered_files:
        templates_lines.append(f'  ("{rel_path}", {var_name})')
    templates_list = ("templates :: [(FilePath, BS.ByteString)]\n"
                      "templates =\n  [\n" +
                      ",\n".join(templates_lines) +
                      "\n  ]\n\n")

    main_function = '''{-
  scaffold performs the following steps:

  STEP I -
    · Prompt the user for:
         o Author name
         o Git remote URL (or repository identifier, e.g., https://github.com/username/repo)
         o Maintainer's email address
         o A short project description
    · The app name is automatically inferred from the target directory name.

  STEP II -
    · Remove any existing directory with the project name.
    · Execute the command:
         stack new <project_dir_name> --resolver=lts-23.8
       (The output of this command is suppressed.)

  STEP III -
    · Overwrite selected project files (with our custom templates)

  STEP IV -
    · In package.yaml, update fields using the provided information,
      including setting the copyright year to the current year and
      replacing any occurrence of "YourApp" with the new app name.
-}
scaffold :: FilePath -> IO ()
scaffold targetDir = do
  -- The app (project) name is inferred from the targetDir using takeBaseName.
  let appName = takeBaseName targetDir
  putStrLn ("[INFO] Inferred application name: " ++ appName)

  -- STEP I: Prompt user for remaining project information.
  putStrLn "======================================"
  putStrLn "          PROJECT INFORMATION         "
  putStrLn "======================================"
  putStrLn ""
  putStr "Author Name               : "
  hFlush stdout
  author <- getLine
  putStr "Owner/Repo (owner/repo)   : "
  hFlush stdout
  gitRemote <- getLine
  putStr "Maintainer Email          : "
  hFlush stdout
  maintainer <- getLine
  putStr "Project Description       : "
  hFlush stdout
  projDescription <- getLine


  -- Get the current year.
  currentTime <- getCurrentTime
  let (year,_,_) = toGregorian (utctDay currentTime)
      currentYear = show year

  -- STEP II: Remove existing project directory if it exists.
  dirExists <- doesDirectoryExist targetDir
  when dirExists $ do
    putStrLn ("[INFO] Removing existing directory " ++ targetDir)
    removeDirectoryRecursive targetDir

  -- STEP II (continued): Run the 'stack new' command to create the new project.
  -- NOTE: We append shell redirection to suppress its output.
  let stackNewCmd = "stack new " ++ targetDir ++ " --resolver=lts-23.8 > /dev/null 2>&1"
  putStrLn $ "[INFO] Running: " ++ stackNewCmd
  callCommand stackNewCmd

  -- STEP III/IV: Overwrite the generated project files with our templates.
  let fullPath sub = targetDir </> sub
  mapM_ (\\(rel, fileData) -> do
           let dir = takeDirectory rel
           when (not (null dir)) $ ensureDir (fullPath dir)
           let finalContent = if rel == "package.yaml"
                                then updatePackageYaml fileData appName author gitRemote maintainer projDescription currentYear
                                else fileData
           writeFileWithInfo (fullPath rel) finalContent
        ) templates

  putStrLn "[INFO] tinfoiltiger - Scaffolding complete. You can now edit your files or compile."

-- | Write a file and print an informational message.
writeFileWithInfo :: FilePath -> BS.ByteString -> IO ()
writeFileWithInfo path content = do
  BS.writeFile path content
  putStrLn $ "[INFO] tinfoiltiger - Created or updated file: " ++ path

-- | Ensure that a directory exists.
ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing True
'''

    module_text = header + templates_list + main_function

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(module_text)
    print(f"[INFO] STEP I - Generated {output_path}")


def generate_lib_files_for_project(project_name, project_src_dir, parent_dir):
    print(f"[INFO] Processing project: {project_name}")

    all_files = gather_files_from_src(project_src_dir)
    if not all_files:
        print(f"[WARNING] STEP I - No files found in {project_src_dir}.")
        return

    project_lib_dir = os.path.join(parent_dir, "lib", project_name)
    os.makedirs(project_lib_dir, exist_ok=True)

    templates_hs_path = os.path.join(project_lib_dir, "Templates.hs")
    scaffolder_hs_path = os.path.join(project_lib_dir, "Scaffolder.hs")

    generate_templates_hs_module(all_files, templates_hs_path, project_name)
    generate_scaffolder_hs_module(all_files, scaffolder_hs_path, project_name)
    print(f"[INFO] Finished processing project: {project_name}")


def generate_new_template_controller(projects, parent_dir):
    """
    Dynamically generates the NewTemplateController.hs file which processes
    the command-line arguments:
      --new <target_directory> --template <template_name>

    This version reads each project's description from its package.yaml file located in
    parent_dir/templates/<project>/package.yaml. If no description is found, it will fall back
    to a default value.

    The generated file is placed at lib/NewTemplateController.hs.

    Args:
      projects: List of available project names (e.g., ["QuickStartUpA", "QuickStartUpB"]).
                Each project is expected to have its corresponding directory inside the templates
                folder (e.g., parent_dir/templates/QuickStartUpA) containing a package.yaml file.
      parent_dir: The parent directory where the lib and templates subdirectories reside.
    """
    import os
    import yaml  # Ensure PyYAML is installed: pip install pyyaml

    # Helper function to retrieve description from a project's package.yaml located in templates.
    def get_project_description(proj, parent_dir):
        pkg_yaml_path = os.path.join(parent_dir, "templates", proj, "package.yaml")
        default_desc = f"Template for {proj}"
        if os.path.exists(pkg_yaml_path):
            try:
                with open(pkg_yaml_path, "r") as f:
                    pkg_data = yaml.safe_load(f)
                # Uncomment the line below to debug the loaded YAML data.
                # print(f"[DEBUG] {proj} package.yaml data: {pkg_data}")
                desc = pkg_data.get("description")
                if desc is None:
                    print(f"[WARN] 'description' key missing in {pkg_yaml_path}; using default description.")
                    return default_desc
                return desc.strip()
            except Exception as e:
                print(f"[WARN] Could not read description from {pkg_yaml_path}: {e}")
                return default_desc
        else:
            print(f"[WARN] package.yaml not found for project {proj} at {pkg_yaml_path}")
            return default_desc

    # Ensure the "lib" directory exists.
    lib_dir = os.path.join(parent_dir, "lib")
    os.makedirs(lib_dir, exist_ok=True)

    # Path for the generated Haskell controller.
    controller_path = os.path.join(lib_dir, "NewTemplateController.hs")

    lines = []
    lines.append("{-# LANGUAGE OverloadedStrings #-}")
    lines.append("module NewTemplateController (handleNewTemplate) where")
    lines.append("")
    lines.append("import System.Environment (getArgs)")
    lines.append("import System.Exit (exitFailure)")
    lines.append("import System.IO (hPutStrLn, stderr)")
    lines.append("import Data.List (elemIndex)")
    lines.append("")

    # For each project, add an import statement.
    # We assume that each project's Scaffolder module is declared as "module <Project>.Scaffolder where"
    for proj in projects:
        lines.append(f"import qualified {proj}.Scaffolder as S_{proj}")
    lines.append("")

    # Available templates list, with description extracted from package.yaml.
    lines.append("availableTemplates :: [(String, String)]")
    lines.append("availableTemplates =")
    if projects:
        lines.append("  [")
        for i, proj in enumerate(projects):
            desc = get_project_description(proj, parent_dir)
            # Escape any backslashes or quotes for proper Haskell string literal formatting.
            formatted_desc = desc.replace("\\", "\\\\").replace("\"", "\\\"")
            comma = "," if i < len(projects) - 1 else ""
            lines.append(f'    ("{proj}", "{formatted_desc}"){comma}')
        lines.append("  ]")
    else:
        lines.append("  []")
    lines.append("")

    # Write the handleNewTemplate function.
    lines.append("handleNewTemplate :: IO ()")
    lines.append("handleNewTemplate = do")
    lines.append("  args <- getArgs")
    lines.append("  let newIndex = elemIndex \"--new\" args")
    lines.append("      templateIndex = elemIndex \"--template\" args")
    lines.append("  case (newIndex, templateIndex) of")
    lines.append("    (_, Nothing) -> do")
    lines.append("      putStrLn \"------------------------------------\"")
    lines.append("      putStrLn \"          AVAILABLE TEMPLATES       \"")
    lines.append("      putStrLn \"------------------------------------\"")
    lines.append("      mapM_ (\\(name, desc) -> putStrLn $ \"  \" ++ name ++ \" - \" ++ desc) availableTemplates")
    lines.append("      putStrLn \"------------------------------------\"")
    lines.append("      putStrLn \"Usage: tinfoiltiger --new <project-name> --template <template-name>\"")
    lines.append("      putStrLn \"\"")
    lines.append("      exitFailure")
    lines.append("    (Nothing, _) -> do")
    lines.append("      hPutStrLn stderr \"Error: Missing --new flag.\"")
    lines.append("      exitFailure")
    lines.append("    (Just nIdx, Just tIdx) -> do")
    lines.append("      let maybeTarget = if length args > nIdx+1 then Just (args !! (nIdx+1)) else Nothing")
    lines.append("          maybeTemplate = if length args > tIdx+1 then Just (args !! (tIdx+1)) else Nothing")
    lines.append("      case (maybeTarget, maybeTemplate) of")
    lines.append("        (Nothing, _) -> do")
    lines.append("          hPutStrLn stderr \"Error: No target directory specified after --new.\"")
    lines.append("          exitFailure")
    lines.append("        (_, Nothing) -> do")
    lines.append("          putStrLn \"No template specified.\"")
    lines.append("          putStrLn \"\"")
    lines.append("          putStrLn \"------------------------------------\"")
    lines.append("          putStrLn \"Available Templates:\"")
    lines.append("          mapM_ (\\(name, desc) -> putStrLn $ \"  \" ++ name ++ \" - \" ++ desc) availableTemplates")
    lines.append("          putStrLn \"------------------------------------\"")
    lines.append("          exitFailure")
    lines.append("        (Just targetDir, Just tmpl) ->")
    lines.append("          case tmpl of")
    for proj in projects:
        lines.append(f'            "{proj}" -> S_{proj}.scaffold targetDir')
    lines.append("            _ -> do")
    lines.append('              hPutStrLn stderr ("Error: Unrecognized template " ++ tmpl)')
    lines.append("              putStrLn \"\"")
    lines.append("              putStrLn \"------------------------------------\"")
    lines.append("              putStrLn \"Available Templates:\"")
    lines.append("              mapM_ (\\(name, desc) -> putStrLn $ \"  \" ++ name ++ \" - \" ++ desc) availableTemplates")
    lines.append("              putStrLn \"------------------------------------\"")
    lines.append("              exitFailure")

    controller_content = "\n".join(lines)
    with open(controller_path, "w") as f:
        f.write(controller_content)

    print(f"[INFO] Generated {controller_path}")


def main():
    print("[INFO] Step I - Preprocessing")

    # Determine the script directory and the project root (parent directory).
    script_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.abspath(os.path.join(script_dir, os.pardir))

    # CLEAR ALL DIRECTORIES IN THE LIB DIR OF THE PARENT
    print("[DEBUG] Script directory:", script_dir)
    print("[DEBUG] Parent directory:", parent_dir)

    # Define the lib directory as a subdirectory of the parent directory.
    lib_dir = os.path.join(parent_dir, "lib")
    print("[DEBUG] Lib directory:", lib_dir)

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
        print(f"[WARNING] 'lib' directory not found at {lib_dir}")

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

    # Generate the NewTemplateController.hs file that handles the --new flag.
    generate_new_template_controller(projects, parent_dir)

    print("[INFO] Step I - Preprocessing complete")


if __name__ == "__main__":
    main()
