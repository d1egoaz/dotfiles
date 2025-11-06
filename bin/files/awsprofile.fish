#!/usr/bin/env fish

function awsprofile -d "Select and set AWS profile with fzf"
    # Check if AWS config exists
    if not test -f ~/.aws/config
        echo "AWS config file not found at ~/.aws/config"
        return 1
    end

    # Use fzf to select profile directly from command pipeline
    set selected_profile (grep '\[profile' ~/.aws/config | awk '{print $2}' | tr -d '[]' | fzf --prompt 'AWS Profile > ')

    # Check if a profile was selected
    if test -z "$selected_profile"
        echo "No profile selected."
        return 1
    end

    # Set the AWS_PROFILE environment variable for the current session
    set -gx AWS_PROFILE $selected_profile
    echo "set AWS_PROFILE $selected_profile"

    # Run aws sso login
    echo "Running 'aws sso login'..."
    aws sso login
    aws sts get-caller-identity
end
