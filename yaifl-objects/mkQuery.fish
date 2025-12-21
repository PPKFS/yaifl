#!/usr/bin/env fish

# Check if a name was provided
if set -q argv[1]
    set r $argv[1]
    # Store the first argument in a variable for clarity
    set folder_name "src/Yaifl/$r"

    # Create the directory with the provided name
    mkdir -p $folder_name

    # Define the file name using string formatting and create it inside the directory
    touch "$folder_name/Query.hs"
    touch "$folder_name/Kind.hs"

    echo "Created directory '$folder_name'"
else
    echo "Error: No name provided. Usage: $argv[0] <name>"
end