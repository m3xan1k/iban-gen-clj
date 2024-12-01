## On mac

```
brew install borkdude/brew/babashka
bb iban-gen.clj
```

## On linux

```
bash < <(curl -s https://raw.githubusercontent.com/babashka/babashka/master/install)
bb iban-gen.clj
```

## Move to local bin
```
chmod +x iban-gen.clj
mv iban-gen.clj ~/.local/bin/iban-gen
```
