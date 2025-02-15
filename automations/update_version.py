#!/usr/bin/env python3
import os
import subprocess
import json
import re


MAJOR_RELEASE_NUMBER = 0


###############################################################################
# STEP I: Preprocessing – Generate lib/templates_lib.ml and lib/scaffolder_lib.ml
###############################################################################

def get_versions(MAJOR_RELEASE_NUMBER=None):
    """
    Retrieves the current version number from the server and computes a new version.

    Steps:
      1) Reads ~/.rgwfuncsrc to obtain SSH credentials for the preset 'icdattcwsm'.
      2) SSH into the server and list the .deb files in:
         /home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/tinfoiltiger/debian/
         dists/stable/main/binary-amd64
      3) Takes the last .deb filename (expected to be of the form:
         tinfoiltiger_<MAJOR>.<MINOR>.<PATCH>-<REV>.deb) and extracts the version string.
      4) If no .deb files are found:
           - Assumes a baseline version for the (possibly new) major release
             using {major}.0.1-1 where if MAJOR_RELEASE_NUMBER is None we default to major 0.
         Otherwise, if a file is found and MAJOR_RELEASE_NUMBER is None (or matches the current major):
           - Returns a new version having the same MAJOR and MINOR with PATCH incremented by 1.
         Otherwise:
           - Returns a new version of the form: {MAJOR_RELEASE_NUMBER}.0.1-1.
    Returns:
         (current_version, new_version)
         where current_version follows "X.Y.Z-R" when available (or None if no file existed) and new_version similarly.
    """
    # Step 1: Read the config file for SSH credentials.
    config_path = os.path.expanduser("~/.rgwfuncsrc")
    if not os.path.exists(config_path):
        raise FileNotFoundError(f"Cannot find config file: {config_path}")

    with open(config_path, "r", encoding='utf-8') as f:
        data = json.load(f)

    vm_presets = data.get("vm_presets", [])
    preset = next((p for p in vm_presets if p.get("name") == "icdattcwsm"), None)
    if not preset:
        raise ValueError("No preset named 'icdattcwsm' found in ~/.rgwfuncsrc")

    host = preset["host"]
    ssh_user = preset["ssh_user"]
    ssh_key_path = preset["ssh_key_path"]

    # Step 2: List .deb files on the server.
    remote_deb_dir = (
        "/home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/tinfoiltiger/debian/"
        "dists/stable/main/binary-amd64"
    )
    ssh_cmd = (
        f"ssh -i {ssh_key_path} {ssh_user}@{host} "
        f"\"ls -1 {remote_deb_dir}/*.deb 2>/dev/null || true\""
    )
    output = subprocess.check_output(ssh_cmd, shell=True).decode("utf-8").strip()

    if not output:
        # No .deb files found. Use a default baseline version.
        if MAJOR_RELEASE_NUMBER is None:
            new_major = 0  # Default to 0 if no major release number is specified.
        else:
            new_major = int(MAJOR_RELEASE_NUMBER)
        new_version = f"{new_major}.0.1-1"
        current_version = None  # No current version exists.
        return current_version, new_version

    # Assume the last line is the relevant .deb file.
    deb_file_path = output.split("\n")[-1].strip()

    # Step 3: Parse the filename for version info.
    # Expected filename format: tinfoiltiger_<MAJOR>.<MINOR>.<PATCH>-<REV>.deb
    filename = os.path.basename(deb_file_path)
    match = re.match(r"^tinfoiltiger_(\d+\.\d+\.\d+)-(\d+)\.deb$", filename)
    if not match:
        raise ValueError(f"Could not parse version from deb file name: {filename}")

    version_str = match.group(1)    # e.g. "1.0.3"
    revision_str = match.group(2)   # e.g. "1"
    current_version = f"{version_str}-{revision_str}"

    # Split version_str into major, minor, patch components.
    major_str, minor_str, patch_str = version_str.split(".")
    server_major = int(major_str)
    server_minor = int(minor_str)
    server_patch = int(patch_str)
    server_revision = int(revision_str)

    # Step 4: Compute new version string.
    if MAJOR_RELEASE_NUMBER is None:
        # No new major specified; assume we want same major/minor with patch +1.
        new_major = server_major
        new_minor = server_minor
        new_patch = server_patch + 1
        new_revision = server_revision  # Keeping the same revision, but adjust if necessary.
    else:
        user_major = int(MAJOR_RELEASE_NUMBER)
        if user_major == server_major:
            # Same major: increment patch.
            new_major = server_major
            new_minor = server_minor
            new_patch = server_patch + 1
            new_revision = server_revision
        else:
            # New major version requested - use the base version for a major release.
            new_major = user_major
            new_minor = 0
            new_patch = 1
            new_revision = 1

    new_version = f"{new_major}.{new_minor}.{new_patch}-{new_revision}"
    return current_version, new_version


def get_new_version_number(MAJOR_RELEASE_NUMBER=None):
    """
    1) Reads ~/.rgwfuncsrc JSON to obtain the server credentials preset named 'icdattcwsm'.
    2) SSH into the server and find the current .deb file in:
       /home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/tinfoiltiger/debian/dists/stable/main/binary-amd64
    3) Parse the .deb file name to extract the current version string (MAJOR.MINOR.PATCH-REV).
    4) If MAJOR_RELEASE_NUMBER is None or matches the current MAJOR, return a version
       with PATCH incremented by 1. Otherwise, return a version of the form:
       {MAJOR_RELEASE_NUMBER}.0.1-1
    """

    # -------------------------------------------------------------------------
    # Step 1) Read ~/.rgwfuncsrc and find the 'icdattcwsm' preset
    # -------------------------------------------------------------------------
    funcs_path = os.path.expanduser("~/.rgwfuncsrc")
    if not os.path.exists(funcs_path):
        raise FileNotFoundError(f"Cannot find config file: {funcs_path}")

    with open(funcs_path, "r") as f:
        data = json.load(f)

    vm_presets = data.get("vm_presets", [])
    preset = next((p for p in vm_presets if p.get("name") == "icdattcwsm"), None)
    if not preset:
        raise ValueError("No preset named 'icdattcwsm' found in ~/.rgwfuncs")

    host = preset["host"]
    ssh_user = preset["ssh_user"]
    ssh_key_path = preset["ssh_key_path"]

    # -------------------------------------------------------------------------
    # Step 2) SSH to the server to locate the current .deb file
    # -------------------------------------------------------------------------
    remote_deb_dir = (
        "/home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/tinfoiltiger/debian/"
        "dists/stable/main/binary-amd64"
    )

    # List all .deb files
    ssh_cmd = (
        f"ssh -i {ssh_key_path} {ssh_user}@{host} "
        f"\"ls -1 {remote_deb_dir}/*.deb 2>/dev/null || true\""
    )
    output = subprocess.check_output(ssh_cmd, shell=True).decode().strip()

    # If no .deb files exist, raise an error (or handle how you prefer)
    if not output:
        raise FileNotFoundError(
            f"No .deb files found in {remote_deb_dir}"
        )

    # For simplicity, assume there's only one relevant .deb file, or
    # take the last line if multiple. Adjust logic as needed.
    deb_file_path = output.split("\n")[-1].strip()
    # Example: /home/rgw/.../binary-amd64/tinfoiltiger_1.0.3-1.deb

    # -------------------------------------------------------------------------
    # Step 3) Parse the .deb file name to extract the version
    #         We expect a name of the form: tinfoiltiger_X.Y.Z-REV.deb
    # -------------------------------------------------------------------------
    filename = os.path.basename(deb_file_path)  # tinfoiltiger_1.0.3-1.deb
    match = re.match(r"^tinfoiltiger_(\d+\.\d+\.\d+)-(\d+)\.deb$", filename)
    if not match:
        raise ValueError(
            f"Could not parse version from deb file name: {filename}"
        )

    version_str = match.group(1)  # "1.0.3"
    revision_str = match.group(2)  # "1"

    # Split the version_str into major, minor, patch
    major_str, minor_str, patch_str = version_str.split(".")
    server_major = int(major_str)    # 1
    server_minor = int(minor_str)    # 0
    server_patch = int(patch_str)    # 3
    server_revision = int(revision_str)  # 1

    # -------------------------------------------------------------------------
    # Step 4) Compute new version based on MAJOR_RELEASE_NUMBER
    # -------------------------------------------------------------------------
    if MAJOR_RELEASE_NUMBER is None:
        # If user didn't specify a new major, we assume they
        # want to increment the patch of the existing major.
        new_major = server_major
        new_minor = server_minor
        new_patch = server_patch + 1
        new_revision = server_revision  # keep the same revision
    else:
        # Convert user input to int
        user_major = int(MAJOR_RELEASE_NUMBER)

        if user_major == server_major:
            # If the requested major is the same as the server major,
            # just increment patch
            new_major = server_major
            new_minor = server_minor
            new_patch = server_patch + 1
            new_revision = server_revision
        else:
            # Otherwise, create a new major version: MAJOR.0.1-1
            new_major = user_major
            new_minor = 0
            new_patch = 1
            new_revision = 1

    # Construct the new version string
    new_version_str = f"{new_major}.{new_minor}.{new_patch}-{new_revision}"

    return new_version_str


def replace_version(main_hs_path, package_yaml_path, new_version):
    """
    Replaces version strings in both the Haskell source file and package.yaml file with new_version.

    For main_hs_path, it assumes a version line like:
         let version = "2.0.26-1"
    For package_yaml_path, it assumes a version line like:
         version: "0.1.0.0"
    and replaces it so that the new version is always quoted.

    In particular, if new_version is "2.0.27-1", then main.hs gets "2.0.27-1"
    while package.yaml gets "2.0.27.1".
    """

    # --- Update the Haskell source file ---
    with open(main_hs_path, 'r', encoding='utf-8') as f:
        main_content = f.read()

    # Regular expression for Haskell version definition:
    hs_pattern = r'(let\s+version\s*=\s*")[^"]*(")'

    def hs_replacer(match):
        return match.group(1) + new_version + match.group(2)
    new_main_content = re.sub(hs_pattern, hs_replacer, main_content)

    with open(main_hs_path, 'w', encoding='utf-8') as f:
        f.write(new_main_content)
    print(f"[INFO] Replaced version with {new_version} in {main_hs_path}")

    # --- Convert new_version for package.yaml ---
    # Replace hyphens with dots for package.yaml
    pkg_new_version = new_version.replace("-", ".")

    # --- Update the package.yaml file ---
    with open(package_yaml_path, 'r', encoding='utf-8') as f:
        pkg_content = f.read()

    # Regular expression for YAML version field at the beginning of a line.
    # It will match versions with or without quotes.
    pkg_pattern = r'^(version:\s*)["\']?([^"\n]+)["\']?'

    def pkg_replacer(match):
        # Always force double quotes around the new version.
        return match.group(1) + '"' + pkg_new_version + '"'

    new_pkg_content = re.sub(pkg_pattern, pkg_replacer, pkg_content, flags=re.MULTILINE)

    with open(package_yaml_path, 'w', encoding='utf-8') as f:
        f.write(new_pkg_content)
    print(f"[INFO] Replaced version with {pkg_new_version} in {package_yaml_path}")


def main():

    current_version, new_version = get_versions()
    script_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.abspath(os.path.join(script_dir, os.pardir))
    main_hs_path = os.path.join(parent_dir, 'app/Main.hs')
    package_yaml_path = os.path.join(parent_dir, 'package.yaml')

    replace_version(main_hs_path, package_yaml_path, new_version)


if __name__ == "__main__":
    main()
