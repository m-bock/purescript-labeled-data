gen-readme:
    purs-to-md test/GenReadme.purs README.md
    md-magic

format:
    purs-tidy format-in-place 'src/**/*.purs'

test:
    spago --config test.dhall test