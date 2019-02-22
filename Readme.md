## Описание

эта папка создана для внутреннего обмена полезным кодом, используемым для обработки данных 

### настройка кнопки gitignore в GitGui
```
git config --global guitool."Add to .gitignore".cmd $'echo "\n$FILENAME" >> .gitignore & git add .gitignore'
git config --global guitool."Add to .gitignore".needsfile yes
git config --global guitool."Add to .gitignore".confirm yes
```