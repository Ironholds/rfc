##rfc
A package for retrieving metadata associated with IETF RfCs, intended as an rvest demonstrator. Extracts:

1. RfC number;
2. Unique identifier;
3. Authors;
4. Title;
5. Release date;
6. Abstract;
7. Linked-to RfCs;
8. Section headings;
9. Page counts;
10. Current RfC status;
11. Which RfCs obsoleted it (if applicable);
12. Which RfCs updated it (if applicable).

__Author:__ Oliver Keyes<br/>
__License:__ [MIT](http://opensource.org/licenses/MIT)<br/>
__Status:__ In development

###Installation

    library(devtools)
    install_github("ironholds/rfc")