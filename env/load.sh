export MIHOST=$(hostname -s)

case $MIHOST in
terra)
  export GISDATA=/opt/gisdata
  export GISDB=/opt/gisdb
  export GISOUT=/opt/gisout
  ;;
roraima)
  export GISDATA=$HOME/gisdata
  export GISDB=$HOME/gisdb
  export GISOUT=$HOME/gisout
  export RSTUDIO_PANDOC=/Applications/RStudio.app/Contents/MacOS/pandoc
  ;;
katana1)
  export GISDATA=/srv/scratch/$(whoami)/gisdata
  export GISDB=/srv/scratch/$(whoami)/gisdb
  export GISOUT=/srv/scratch/$(whoami)/gisout
  ;;
katana2)
  export GISDATA=/srv/scratch/$(whoami)/gisdata
  export GISDB=/srv/scratch/$(whoami)/gisdb
  export GISOUT=/srv/scratch/$(whoami)/gisout
  ;;
esac

export SCRIPTDIR=$HOME/proyectos/IVIC/Hunting_in_GS
export WORKDIR=$HOME/tmp/Hunting_in_GS
mkdir -p $WORKDIR
