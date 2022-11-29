# Public Order Emergency Commission transcript parser

The Public Order Emergency Commission (POEC) has been holding public hearings into the invocation of the _Emergencies Act_ in February 2022. These hearings have included substantial witness testimony—an invaluable contribution to Canada’s public record on not only the conditions surrounding the state of emergency, but national security and how government works.

This testimony, transcribed by a court reporter, is posted after each day’s hearings as a PDF. You can [see the PDF of the transcript of the first day’s hearings as an example](https://publicorderemergencycommission.ca/files/documents/Transcripts/POEC-Public-Hearings-Volume-1-October-13-2022.pdf).

This project aims to increase the accessibility and usability of that testimony. It:

- extracts testimony text from the PDF, converting it to a structured format (one row per line of text), assigning IDs to all lines
- extracts speaker names, headings, and other page and line metadata
- standardizes speaker names and corrects misattributions (according to a set of documented rules, with notes provided where the correction reasoning may be unclear)
- merges testimony split across lines into one, providing “one row per time intervention (a person spoke”), instead of “one row per line of text” (where one intervention usually contains multiple lines of text)
- computes summary statistics about each proceeding and speaker

The parsed data is used to power a [“POEC Explorer”, providing an accessible, user-friendly, web-based version of the testimony](https://poec-explorer.labs.lucascherkewski.com/). The [source code for the explorer is available separately](https://github.com/lchski/poec-explorer). Data updates in the explorer are linked (generally) to specific commits in this repository.

The code in this repository is licensed under the [MIT license](https://github.com/lchski/poec-analysis/blob/main/LICENSE.md).
