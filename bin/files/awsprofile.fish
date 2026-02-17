#!/usr/bin/env fish

function awsprofile -d "Select and set AWS profile with fzf"
    # Check if AWS config exists
    if not test -f ~/.aws/config
        echo "AWS config file not found at ~/.aws/config"
        return 1
    end

    set -l history_file ~/.aws/.awsprofile_history
    set -l all_profiles (grep '\[profile' ~/.aws/config | awk '{print $2}' | tr -d '[]')
    set -l recent (test -f $history_file; and tac $history_file)
    set -l sorted (printf '%s\n' $recent $all_profiles | awk '!seen[$0]++')

    set selected_profile (printf '%s\n' $sorted | fzf --prompt 'AWS Profile > ')

    # Check if a profile was selected
    if test -z "$selected_profile"
        echo "No profile selected."
        return 1
    end

    echo $selected_profile >> $history_file

    # Set the AWS_PROFILE environment variable for the current session
    set -gx AWS_PROFILE $selected_profile
    echo "AWS_PROFILE set to $selected_profile"

    # Ask if SSO login is needed (skip if already authenticated via another profile)
    read -P "Run 'aws sso login'? [y/N] " -l do_login
    if test "$do_login" = y -o "$do_login" = Y
        echo "Running 'aws sso login'..."
        aws sso login
    end

    aws sts get-caller-identity
end
