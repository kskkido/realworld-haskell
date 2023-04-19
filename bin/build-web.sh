function main {
  SCRIPT_DIR=$(cd "$(dirname $0)"; pwd)
  PROJECT_DIR=$(dirname $SCRIPT_DIR)
  cd ${PROJECT_DIR}
	cd ./packages/honduit-web
	make
}

main $*

