In June 2019, the State of Illinois enacted a package of legislation to fund transportation capital projects, water infrastructure, natural resources and open space projects, and the construction of schools, affordable housing, and other facilities. Appropriations for the capital package are found in multiple pieces of legislation that were approved together during the General Assembly’s spring
session. The majority of the funded projects and programs are located within one piece of legislation, Public Act 101-0029, which is
publicly available as a PDF. Within the document there are more than 1,400 line-item appropriations written in paragraph. Though the document is searchable, the text format makes viewing more than one appropriation at a time difficult.  

Download the .xlsx file above if you'd like to view the final, clean product.

This  R script "enrolled public act 10100029.R" takes the PDF of the enrolled public act and converts it into a data table that can be explored, grouped, and summarized. 

After running the code, for the purposes of data scrubbing please note the following errors and anomalies that need to be fixed (either
in R or export as a CSV and do the following in Excel): 

1.	Filter  the appropriation column to find errors and multiple appropriations that need to be broken out. 
  
  - Filter by "character(0" . The appropriations in Article 6 are written differently and have more than one appropriation per section
  which don't get caught  by the script. Manually split and enter the appropriations for this article. For some of them, like sections  
  75, 80 and 85 you’ll need to add them up, or break out into multiple rows. After taking care of the line items in Article 6, note the
  remaining rows either have missing dollar signs, or a typo in them, such as article 16, section 2030, where there is a space between
  the dollar sign and the amount so the code didn’t pick it up. Manually copy the appropriation amount into the appropriation column. 

2. Additional sections that have multiple appropriations

- Filter by the selections at the bottom of the appropriation list that have c(“number”, “number”. These have multiple numbers in
the text, either because there are multiple appropriations within that paragraph or there was a typo like a missing tab that made the
code combine two appropriations. Break these out. 

3.	Notice the last outstanding error at the bottom of the appropriation filter is a typo on article 16, section 5250: is this $100,000
or $1,000,000? Here, we assume it is the larger to be on the conservative side. 

The OPTIONAL code at the bottom creates the "grantee" and "purpose" columns that are available in the downloadable Excel document. The resulting columns are very messy and need significant cleaning. If these columns are of use to you, I recommend just using the article and section number from the downloadable Excel doc and using a lookup to pull in the columns from the .xlsx version already scrubbed by CMAP. 

IMPORTANT TO REMEMBER: The projects and programs funded in Public Act 101-0029 do not represent all capital projects funded in the 
package passed by the General Assembly, nor are they guaranteed to move forward. Appropriations, particularly for bond-funded projects
backed by new sources of revenues, are contingent on the state issuing the bonds and realizing the projected level of new revenues. 
Additionally, while many specific projects are included as line items, other line items are for broad categories of project types and
specific projects to be funded will be decided at a later date; the list is therefore not exhaustive of all projects to be funded by 
the capital package. With this in mind, we encourage our partners to download the data, run the R code, explore the projects and
programs in the legislation, and let us know what you find @onto2050. 



