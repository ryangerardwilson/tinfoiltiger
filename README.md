# TinFoilTiger

A memory-only and zero-persistence web application framework, written in Haskell.

At its core, the framework uses a pure in-memory architecture to execute application logic with minimal disk I/O overhead. It features a special HTML templating engine that integrates Tailwind CSS for dynamically generating responsive, utility-first UI components at build time. Notably, the framework employs Redis as the primary, volatile data store—eschewing traditional data persistence to maximize throughput and reduce latency. This architecture is ideal for applications where rapid, transient data processing is paramount.

## 1. Prerequisites

- Ubuntu 24.04 LTS

## 2. Installation

    curl -fsSL https://files.ryangerardwilson.com/tinfoiltiger/debian/pubkey.gpg | sudo gpg --dearmor -o /usr/share/keyrings/tinfoiltiger.gpg
    echo "deb [arch=amd64 signed-by=/usr/share/keyrings/tinfoiltiger.gpg] https://files.ryangerardwilson.com/tinfoiltiger/debian stable main" | sudo tee /etc/apt/sources.list.d/tinfoiltiger.list
    sudo apt update
    sudo apt-get install tinfoiltiger

This ensures the following are installed: Stack, Redis, a custom Tailwind CSS utility tool (required by the Setup.hs of your project)

    tinfoiltiger --init

## 3. Subsequent Updates

    sudo apt update
    sudo apt install --only-upgrade tinfoiltiger

## 4. Quick Start

Once initialized, run the application, execute it with the `--new` flag followed by the name of your project.

    tinfoiltiger --new <PROJECT_NAME>

The above command will display a list of templates to choose from. You can then re-run with the indicated template name.

    tinfoiltiger --new <PROJECT_NAME> --template <TEMPLAE_NAME>

## 5. Upgrading Your TinFoilTiger Project

TinFoilTiger scaffolds projects with a lib/TinFoilTiger directory. This way, when you run the below upgrade command, the contents of this directory will be replaced, leaving the rest of your project unaffected.

    cd your/tinFoilTiger/project/root/dir
    tinfoiltiger --upgrade

## 6. License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


