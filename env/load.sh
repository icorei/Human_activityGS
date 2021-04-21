export MIHOST=$(hostname -s)

case $MIHOST in
  roraima)
    export RSTUDIO_PANDOC=/Applications/RStudio.app/Contents/MacOS/pandoc
    ;;
esac

export PROJECT=Human_activityGS
export SCRIPTDIR=$HOME/proyectos/IVIC/$PROJECT
export WORKDIR=$HOME/tmp/$PROJECT


mkdir -p $WORKDIR
