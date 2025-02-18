import os
import sys


def generate_template_mappings_hs_module():
    """
    Dynamically generates the Haskell module lib/TemplateMappings.hs.
    It scans the lib/Templates directory for subdirectories (each corresponding to a template)
    and creates mappings for:
      • templateMappings (using embedDir)
      • scaffolderMappings (using qualified module names for Scaffolder)
      • templateFilesMapping (using qualified module names for TemplateFiles)
    """
    templates_dir = os.path.join("lib", "Templates")
    if not os.path.isdir(templates_dir):
        print(f"[ERROR] Directory '{templates_dir}' not found.", file=sys.stderr)
        sys.exit(1)

    # Get template names (subdirectory names)
    templates = sorted(
        [d for d in os.listdir(templates_dir)
         if os.path.isdir(os.path.join(templates_dir, d))]
    )

    if not templates:
        print(f"[WARNING] No templates found in '{templates_dir}'.", file=sys.stderr)
        sys.exit(1)

    # Build mapping entries for each category.
    template_mappings_entries = []
    scaffolder_mappings_entries = []
    template_files_mapping_entries = []

    for t in templates:
        template_mappings_entries.append(
            f'  ("{t}", $(embedDir "lib/Templates/{t}"))'
        )
        scaffolder_mappings_entries.append(
            f'  ("{t}",  Templates.{t}.Scaffolder.scaffold)'
        )
        template_files_mapping_entries.append(
            f'  ("{t}",  Templates.{t}.TemplateFiles.templates)'
        )

    template_mappings_str = ",\n".join(template_mappings_entries)
    scaffolder_mappings_str = ",\n".join(scaffolder_mappings_entries)
    template_files_mapping_str = ",\n".join(template_files_mapping_entries)

    # Build import lines for the scaffolder and TemplateFiles modules.
    scaffolder_imports = "\n".join(
        [f"import qualified Templates.{t}.Scaffolder" for t in templates]
    )
    templatefiles_imports = "\n".join(
        [f"import qualified Templates.{t}.TemplateFiles" for t in templates]
    )

    hs_content = f'''{{-# LANGUAGE TemplateHaskell #-}}
{{-# LANGUAGE OverloadedStrings #-}}

module TemplateMappings
  ( templateMappings,
    templateMappingsLookup,
    scaffolderMappings,
    templateFilesMapping
  ) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- Import the scaffolder modules from your templates.
{scaffolder_imports}

-- Import the TemplateFiles modules from your templates.
{templatefiles_imports}

-- | A mapping from template name to its embedded (source) files.
-- Each entry is a list of (relative file path, file content) pairs.
templateMappings :: Map String [(FilePath, BS.ByteString)]
templateMappings = Map.fromList
  [ {template_mappings_str}
  ]

-- | Lookup helper to find a template’s embedded files.
templateMappingsLookup :: String -> Maybe [(FilePath, BS.ByteString)]
templateMappingsLookup = flip Map.lookup templateMappings

-- | A mapping from template name to its scaffolder function.
scaffolderMappings :: Map String (FilePath -> IO ())
scaffolderMappings = Map.fromList
  [ {scaffolder_mappings_str}
  ]

-- | A mapping for template-specific files exported from each TemplateFiles module.
-- Here we map a template name to its aggregated embedded files.
templateFilesMapping :: Map String [(FilePath, BS.ByteString)]
templateFilesMapping = Map.fromList
  [ {template_files_mapping_str}
  ]
'''

    # Write the generated Haskell module.
    output_path = os.path.join("lib", "TemplateMappings.hs")
    try:
        with open(output_path, "w", encoding="utf-8") as out_file:
            out_file.write(hs_content)
        print(f"[INFO] Generated {output_path} with templates: {', '.join(templates)}")
    except Exception as e:
        print(f"[ERROR] Failed to write {output_path}: {e}", file=sys.stderr)
        sys.exit(1)
