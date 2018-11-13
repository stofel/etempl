## Main Makefile

include ./project.mk


DEV_PATHS= \
  ./dev/bin \
  ./dev/lib \
  ./dev/erts* \
  ./dev/releases \
  ./dev/project.mk \
  ./dev/Makefile \
  ./dev/Version
PROD_PATHS= \
  ./prod/bin \
  ./prod/lib \
  ./prod/erts* \
  ./prod/releases \
  ./prod/project.mk \
  ./prod/Makefile \
  ./prod/Version

DEV_RSYNC_FLAGS= -a -e "ssh" --rsync-path="sudo -u $(DEV_USER) rsync" --delete-after
PROD_RSYNC_FLAGS= -a -e "ssh" --rsync-path="sudo -u $(PROD_USER) rsync" --delete-after


# rebar should be in PATH env
REBAR := ../rebar3/rebar3
ROOT := ./

.PHONY: dev prod


init:
	mkdir db
	mkdir files
	git clone https://github.com/erlang/rebar3.git
	cd rebar3 && ./bootstrap && cd -
	./rebar3/rebar3 new release $(PROJECT)

prod:
	cd $(PROJECT); $(REBAR) as prod release; cd -
	ln -sf ../../../../../db _build/prod/rel/$(PROJECT)/
	ln -sf ../../../../../files _build/prod/rel/$(PROJECT)/
	cp project.mk _build/prod/rel/$(PROJECT)/
	cp Makefile.run _build/prod/rel/$(PROJECT)/Makefile

dev:
	cd $(PROJECT); $(REBAR) as dev release; cd -
	ln -sf ../../../../../db $(PROJECT)/_build/dev/rel/$(PROJECT)/
	ln -sf ../../../../../files $(PROJECT)/_build/dev/rel/$(PROJECT)/
	cp project.mk $(PROJECT)/_build/dev/rel/$(PROJECT)/
	cp Makefile.run $(PROJECT)/_build/dev/rel/$(PROJECT)/Makefile

all:
	cd $(PROJECT)
	$(REBAR) as dev compile
	$(REBAR) as prod compile

clean_prod:
	cd $(PROJECT); $(REBAR) as prod clean; cd -
clean_dev:
	cd $(PROJECT); $(REBAR) as dev clean; cd -

aclean_prod:
	cd $(PROJECT); $(REBAR) as prod clean -a; cd -
aclean_dev:
	cd $(PROJECT); $(REBAR) as dev clean -a; cd -

fprod: aclean_prod prod
fdev: aclean_dev dev

##############################################################
## Binary update
send_prod:
	cd $(PROJECT); $(REBAR) as prod tar; cd -
	rm -Rf ./prod
	mkdir ./prod
	cp project.mk ./prod/
	cp Makefile.run ./prod/Makefile
	cd $(PROJECT); \
	date > Version; cd -
	#date > Version; \
	git status >> Version; \
	git log --pretty=format:"%h %s" HEAD~10..HEAD >> Version; cd -
	cp $(PROJECT)/Version ./prod/
	tar -xf $(PROJECT)/_build/prod/rel/$(PROJECT)/$(PROJECT)-0.1.0.tar.gz -C ./prod
	cp $(PROJECT)/config/* ./prod/releases/0.1.0/
	rsync $(PROD_RSYNC_FLAGS) $(PROD_PATHS) $(PROD_CRED):~$(PROD_USER)/$(PROJECT)/

send_dev:
	cd $(PROJECT); $(REBAR) as dev tar; cd -
	rm -Rf ./dev
	mkdir ./dev
	cp project.mk ./dev/
	cp Makefile.run ./dev/Makefile
	cd $(PROJECT); \
	date > Version; cd -
	#date > Version; \
	git status >> Version; \
	git log --pretty=format:"%h %s" HEAD~10..HEAD >> Version; cd -
	cp $(PROJECT)/Version ./dev/
	tar -xf $(PROJECT)/_build/dev/rel/$(PROJECT)/$(PROJECT)-0.1.0.tar.gz -C ./dev
	cp $(PROJECT)/config/* ./dev/releases/0.1.0/
	rsync $(DEV_RSYNC_FLAGS) $(DEV_PATHS) $(DEV_CRED):~$(DEV_USER)/$(PROJECT)/


fast_dev:
	cd $(PROJECT); \
	date > Version; \
	git status >> Version; \
	git log --pretty=format:"%h %s" HEAD~10..HEAD >> Version; cd -
	cp $(PROJECT)/Version $(PROJECT)/_build/dev/rel/$(PROJECT)/
	rsync $(DEV_RSYNC_FLAGS) $(PROJECT)/_build/dev/rel/$(PROJECT)/bin $(PROJECT)/_build/dev/rel/$(PROJECT)/lib $(PROJECT)/_build/dev/rel/$(PROJECT)/releases $(PROJECT)/_build/dev/rel/$(PROJECT)/Version $(DEV_CRED):~$(DEV_USER)/$(PROJECT)/
##############################################################
##############################################################
