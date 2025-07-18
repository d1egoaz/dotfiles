#!/usr/bin/env fish

function awsprofile-claude -d "Select AWS profile used for Claude Code"
    set AWS_PROFILE ChimeSandbox-Administrator
    echo "Running 'aws sso login' for $AWS_PROFILE..."
    aws sso login
    aws sts get-caller-identity
end
