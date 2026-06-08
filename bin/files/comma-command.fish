#!/usr/bin/env fish

function ,, -d "Generate a Fish command with pi and insert it into the current prompt"
    if not command -q ,,
        echo ',,: backend command not found. Run just switch to install ~/.local/bin/,,' >&2
        return 127
    end

    if not isatty stdin; or not isatty stdout
        command ,, $argv
        return $status
    end

    set -l original_argv $argv
    set -l generator_args
    set -l description
    set -l target_shell fish
    set -l shell_arg_seen 0

    while test (count $argv) -gt 0
        switch $argv[1]
            case -m --model --provider --thinking --shell
                if test (count $argv) -lt 2
                    command ,, $original_argv
                    return $status
                end
                if test "$argv[1]" = --shell
                    set target_shell $argv[2]
                    set shell_arg_seen 1
                end
                set -a generator_args $argv[1] $argv[2]
                set -e argv[1]
                set -e argv[1]
            case '--model=*' '--provider=*' '--thinking=*'
                set -a generator_args $argv[1]
                set -e argv[1]
            case '--shell=*'
                set target_shell (string replace -- '--shell=' '' $argv[1])
                set shell_arg_seen 1
                set -a generator_args $argv[1]
                set -e argv[1]
            case -h --help
                command ,, --help
                return $status
            case --
                set -e argv[1]
                set description $argv
                break
            case '-*'
                command ,, $original_argv
                return $status
            case '*'
                set description $argv
                break
        end
    end

    if test (count $description) -eq 0
        command ,, --help
        return 2
    end

    if test $shell_arg_seen -eq 0
        set generator_args --shell $target_shell $generator_args
    end

    set -l original_prompt (string join ' ' -- $description)
    set -l command_text (command ,, $generator_args -- $original_prompt)
    if test $status -ne 0
        return $status
    end
    if test -z "$command_text"
        echo ',,: pi returned an empty command' >&2
        return 1
    end

    while true
        printf '\nCommand:\n%s\n\n' "$command_text"
        read -l -P 'Action: [i]nsert, [f]eedback, [d]one: ' action
        or return 0

        switch (string lower -- "$action")
            case i insert
                commandline --replace -- "$command_text"
                commandline --cursor (string length -- "$command_text")
                return 0
            case f feedback
                read -l -P 'Feedback: ' feedback
                or return 0
                if test -z "$feedback"
                    continue
                end

                set -l refinement_prompt (printf 'Original request:\n%s\n\nCurrent command:\n%s\n\nUser feedback:\n%s\n\nReturn one improved %s shell command.' "$original_prompt" "$command_text" "$feedback" "$target_shell")
                set command_text (command ,, $generator_args -- "$refinement_prompt")
                if test $status -ne 0
                    return $status
                end
                if test -z "$command_text"
                    echo ',,: pi returned an empty command' >&2
                    return 1
                end
            case "" d done q quit
                return 0
            case '*'
                echo 'Choose i, f, or d.'
        end
    end
end
