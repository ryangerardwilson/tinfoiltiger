#!/usr/bin/env python3
import os
# import sys
import subprocess
import shutil
import json
import re
from packaging.version import parse as parse_version


MAJOR_RELEASE_NUMBER = 0


def get_new_version(MAJOR_RELEASE_NUMBER=None):
    """
    Retrieves the current version number from the server and computes a new version.

    Steps:
      1) Reads ~/.rgwfuncsrc to obtain SSH credentials for the preset 'icdattcwsm'.
      2) SSH into the server and list the .deb files in:
         /home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/tinfoiltiger/debian/dists/stable/main/binary-amd64
      3) Takes the last .deb filename (expected to be of the form:
         tinfoiltiger_<MAJOR>.<MINOR>.<PATCH>-<REV>.deb) and extracts the version string.
      4) If no .deb files are found:
           - Assumes a baseline version for the (possibly new) major release with
             a default major of 0 (if MAJOR_RELEASE_NUMBER is None) or using the provided major,
             forming {major}.0.1-1.
         Otherwise, if MAJOR_RELEASE_NUMBER is None (or matches the current major):
           - Returns a new version having the same MAJOR and MINOR with PATCH incremented by 1.
         Otherwise:
           - Returns a new version of the form: {MAJOR_RELEASE_NUMBER}.0.1-1.
    Returns:
         new_version as a string in the form "X.Y.Z-R"
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
        # No .deb files found on the server.
        if MAJOR_RELEASE_NUMBER is None:
            new_major = 0  # Default to 0 if no major release number is specified.
        else:
            new_major = int(MAJOR_RELEASE_NUMBER)
        new_version = f"{new_major}.0.1-1"
        return new_version

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
        new_revision = server_revision  # keeping the same revision, adjust if needed.
    else:
        user_major = int(MAJOR_RELEASE_NUMBER)
        if user_major == server_major:
            # Same major: increment patch.
            new_major = server_major
            new_minor = server_minor
            new_patch = server_patch + 1
            new_revision = server_revision
        else:
            # New major version requested, format the version as: {MAJOR_RELEASE_NUMBER}.0.1-1.
            new_major = user_major
            new_minor = 0
            new_patch = 1
            new_revision = 1

    new_version = f"{new_major}.{new_minor}.{new_patch}-{new_revision}"
    return new_version


def publish_release(version):

    def build_deb(version):
        # 2) Define paths/naming
        build_root = f"debian/version_build_folders/tinfoiltiger_{version}"
        build_debian = os.path.join(build_root, "DEBIAN")
        build_bin_dir = os.path.join(build_root, "usr/local/bin")
        out_debs_dir = "debian/version_debs"

        # 3) Ensure directories exist
        os.makedirs(build_debian, exist_ok=True)
        os.makedirs(build_bin_dir, exist_ok=True)
        os.makedirs(out_debs_dir, exist_ok=True)

        # 4) Copy your existing tinfoiltiger binary into the package structure
        shutil.copy2("tinfoiltiger", os.path.join(build_bin_dir, "tinfoiltiger"))

        # 5) Create the control file
        control_content = f"""Package: tinfoiltiger
Version: {version}
Section: utils
Priority: optional
Architecture: amd64
Maintainer: Ryan Gerard Wilson <ryangerardwilson@gmail.com>
Description: A memory-only and zero-persistence web application framework, written in Haskell
"""
        control_path = os.path.join(build_debian, "control")
        with open(control_path, "w") as f:
            f.write(control_content)

        # 6) Build the package with dpkg-deb
        output_deb = os.path.join(out_debs_dir, f"tinfoiltiger_{version}.deb")
        cmd = ["dpkg-deb", "--build", build_root, output_deb]
        print(f"[INFO] STEP III - Running: {' '.join(cmd)}")
        subprocess.check_call(cmd)
        print(f"[INFO] STEP III - Finished building {output_deb}")

    def prepare_deb_for_distribution(version):
        """
        1) Remove any old 'debian/dists/stable' to avoid stale files.
        2) Re-create 'debian/dists/stable/main/binary-amd64'.
        3) Ensure an overrides.txt with 'tinfoiltiger optional utils'.
        4) Copy tinfoiltiger_{version}.deb into binary-amd64.
        5) Run dpkg-scanpackages to create Packages -> Packages.gz.
        6) Use apt-ftparchive to generate a Release file with checksums.
        7) Sign Release with GPG (optional).
        """
        # 1) Remove old 'stable' directory entirely to avoid hash mismatches
        stable_dir = "debian/dists/stable"
        if os.path.exists(stable_dir):
            print("[INFO] STEP III - Removing previous debian/dists/stable folder to ensure a clean slate.")
            shutil.rmtree(stable_dir)

        # 2) Re-create apt_binary_dir
        apt_binary_dir = "debian/dists/stable/main/binary-amd64"
        os.makedirs(apt_binary_dir, exist_ok=True)

        # 3) Ensure an overrides.txt file is present
        overrides_path = os.path.join(apt_binary_dir, "overrides.txt")
        if not os.path.exists(overrides_path):
            with open(overrides_path, "w") as f:
                f.write("tinfoiltiger optional utils\n")
        print(f"[INFO] STEP III - overrides.txt verified at {overrides_path}")

        # 4) Copy the newly built .deb into the apt_binary_dir
        deb_source = os.path.join("debian/version_debs", f"tinfoiltiger_{version}.deb")
        print(f"[INFO] STEP III - Copying {deb_source} to {apt_binary_dir}")
        shutil.copy2(deb_source, apt_binary_dir)

        # 5) Generate Packages and Packages.gz with dpkg-scanpackages
        pkg_cmd = [
            "dpkg-scanpackages",
            "--multiversion",
            ".",
            "overrides.txt"
        ]
        packages_path = os.path.join(apt_binary_dir, "Packages")
        print("[INFO] STEP III - Generating Packages via dpkg-scanpackages...")
        with open(packages_path, "w") as pf:
            subprocess.check_call(pkg_cmd, cwd=apt_binary_dir, stdout=pf)
        print(f"[INFO] STEP III - Created {packages_path}")

        prefix = "dists/stable/main/binary-amd64/"
        print("[INFO] STEP III - Adjusting 'Filename:' entries to remove './'...")
        with open(packages_path, "r") as f:
            lines = f.readlines()
        with open(packages_path, "w") as f:
            for line in lines:
                # If line starts with "Filename: ./", replace that portion with the desired prefix
                if line.startswith("Filename: ./"):
                    line = line.replace("Filename: ./", f"Filename: {prefix}")
                f.write(line)
        print("[INFO] STEP III - Updated Filename paths in Packages file.")

        packages_gz_path = os.path.join(apt_binary_dir, "Packages.gz")
        print("[INFO] STEP III - Compressing Packages to Packages.gz...")
        with open(packages_gz_path, "wb") as f_out:
            subprocess.check_call(["gzip", "-9c", "Packages"], cwd=apt_binary_dir, stdout=f_out)
        print(f"[INFO] STEP III - Created {packages_gz_path}")

        # 6) Generate a proper Release file with apt-ftparchive in debian/dists/stable
        os.makedirs(stable_dir, exist_ok=True)
        apt_ftparchive_conf_path = os.path.join(stable_dir, "apt-ftparchive.conf")
        conf_content = """APT::FTPArchive::Release {
Origin "TinFoilTigerRepo";
Label "TinFoilTigerRepo";
Suite "stable";
Codename "stable";
Architectures "amd64";
Components "main";
};
        """
        with open(apt_ftparchive_conf_path, "w") as f:
            f.write(conf_content)

        release_path = os.path.join(stable_dir, "Release")
        apt_ftparchive_cmd = ["apt-ftparchive", "-c", "apt-ftparchive.conf", "release", "."]
        print(f"[INFO] STEP III - Running apt-ftparchive to generate Release (cwd={stable_dir})...")
        with open(release_path, "w") as release_file:
            subprocess.check_call(apt_ftparchive_cmd, cwd=stable_dir, stdout=release_file)
        print(f"[INFO] STEP III - Generated Release at {release_path}")

        # 7) Optionally sign Release with GPG
        print("[INFO] Signing Release file with GPG...")
        sign_cmd = [
            "gpg",
            "--local-user", "172E2D67FB733C7EB47DEA047FE8FD47C68DC85A",
            "--detach-sign",
            "--armor",
            "--output", "Release.gpg",
            "Release"
        ]
        subprocess.check_call(sign_cmd, cwd=stable_dir)
        print("[INFO] STEP III - Release file signed (Release.gpg created).")

        print("[INFO] STEP III - prepare_deb_for_distribution completed successfully.")

    def push_to_server():
        """
        1) Parse ~/.rgwfuncs JSON to find the 'icdattcwsm' preset for ssh details.
        2) SSH into the server and remove the existing 'debian' dir under
           /home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/tinfoiltiger
        3) SCP (or rsync) the local 'debian' folder to that server path.
        """
        # Step 1) Read ~/.rgwfuncs and find icdattcwsm preset
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

        # Step 2) SSH remove existing 'debian' on server
        remote_path = "/home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/tinfoiltiger"
        ssh_cmd = (
            f"ssh -i {ssh_key_path} {ssh_user}@{host} "
            f"'rm -rf {remote_path}/debian'"
        )
        print(f"[INFO] STEP III - Removing existing debian directory on server: {remote_path}/debian")
        subprocess.check_call(ssh_cmd, shell=True)

        # Step 3) Rsync local 'debian' folder to server, excluding certain directories
        rsync_cmd = (
            f"rsync -avz -e 'ssh -i {ssh_key_path}' "
            f"--exclude 'version_build_folders' --exclude 'version_debs' "
            f"debian {ssh_user}@{host}:{remote_path}"
        )
        print(f"[INFO] STEP III - Uploading local debian folder to {remote_path} on server (excluding version_build_folders and version_debs)")
        subprocess.check_call(rsync_cmd, shell=True)
        print("[INFO] STEP III - push_to_server completed successfully.")

    def delete_all_but_last_version_build_folders():
        build_folders_path = "debian/version_build_folders"

        # List all the folders in the build_folders_path
        version_folders = [
            f for f in os.listdir(build_folders_path)
            if os.path.isdir(os.path.join(build_folders_path, f)) and f.startswith("tinfoiltiger_")
        ]

        # Sort the folders based on version
        version_folders.sort(key=lambda x: parse_version(x.split('_')[1]), reverse=True)

        # Keep the last version only
        for folder in version_folders[1:]:
            full_path = os.path.join(build_folders_path, folder)
            print(f"[INFO] STEP III - Deleting build folder: {full_path}")
            shutil.rmtree(full_path)

    def delete_all_but_last_version_debs():
        debs_dir = "debian/version_debs"

        # List all the .deb files
        deb_files = [
            f for f in os.listdir(debs_dir)
            if os.path.isfile(os.path.join(debs_dir, f)) and f.endswith(".deb")
        ]

        # Sort them based on version
        deb_files.sort(key=lambda x: parse_version(x.split('_')[1][:-4]), reverse=True)

        # Keep the last version only
        for deb in deb_files[1:]:
            full_path = os.path.join(debs_dir, deb)
            print(f"[INFO] STEP III - Deleting deb file: {full_path}")
            os.remove(full_path)

    build_deb(version)
    prepare_deb_for_distribution(version)
    push_to_server()
    delete_all_but_last_version_build_folders()
    delete_all_but_last_version_debs()


################################################################################
# MAIN
################################################################################

def main():

    new_version = get_new_version()
    publish_release(new_version)
    print()


if __name__ == "__main__":
    main()
