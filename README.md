# IncubationMarker
This is an R package that contains a Shiny app to annotate time series of bird incubation data (egg, nest and ambient sensors) from HOBO data loggers made for the Londoño lab at the Icesi University, Cali, Colombia

## Installation

```
library(devtools)
install_github("JWB1990/IncubationMarker")
```

## Export HOBO data using classical settings

By that I mean...

### header

screenshot, do this not that

### header

Explain what these tests test and why

## Run the App

To run the app, first make sure your exported HOBO .txt files are in the correct folder where the app will find them. Run the code below to return the folder path.

```
library(IncubationMarker)
whereHOBO()
```
e.g. of running that.
copy paste hobo txt into there. There should be two example files in there.

Then run the app with the following.
```
runIncubationMarker()
```
Your files will appear in the list of files to choose from. 

## Use the App

```
show what it does
```

## Built With

* [Shiny](https://shiny.rstudio.com/)

## Authors

* **Justin Baldwin** - [RG](https://www.researchgate.net/profile/Justin_Baldwin)
* **Mario Loaiza-Muñoz** - [RG](https://www.researchgate.net/profile/Mario_Loaiza2)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Inspiration: Cooper, C. and Mills, H. 2005. New software for quantifying incubation behavior from time-series recordings. Journal of Field Ornithology 76(4):352-356.

* Shoutout to the [thermPerf](https://github.com/mdjbru-R-packages/thermPerf) package
