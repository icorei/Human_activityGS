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
  ;;
esac

export SCRIPTDIR=$HOME/proyectos/IVIC/Hunting_in_GS
export WORKDIR=$HOME/tmp/Hunting_in_GS
mkdir -p $WORKDIR
