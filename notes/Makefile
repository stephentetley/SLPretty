
all: opts-notes

opts-notes:
	pandoc --from=markdown --to=html --metadata=pagetitle:"Command Opts Notes" --standalone --output=output/cmdopts_notes.html cmdopts_notes.md
	pandoc --from=markdown --to=latex --standalone --output=output/cmdopts_notes.pdf cmdopts_notes.md
	pandoc --from=markdown --to=docx --standalone --output=output/cmdopts_notes.docx cmdopts_notes.md
