DEPS_FILE:=deps.mk
SOURCE_DIR:=src
TEST_SOURCE_DIR:=test/src
EBIN_DIR:=ebin

SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TEST_SOURCES=$(wildcard $(TEST_SOURCE_DIR)/*.erl)
BEAM_TARGETS=$(patsubst %.erl, $(EBIN_DIR)/%.beam, $(notdir $(SOURCES) $(TEST_SOURCES)))

ERLC_OPTS=-o $(EBIN_DIR) -Wall -v +debug_info

all: $(BEAM_TARGETS)

$(DEPS_FILE): $(SOURCES) $(TEST_SOURCES) $(INCLUDES)
	rm -f $@
	echo $(subst : ,:,$(foreach FILE,$^,$(FILE):)) | escript generate_deps $@ $(EBIN_DIR)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl | $(DEPS_FILE) $(EBIN_DIR)
	erlc $(ERLC_OPTS) -pa $(EBIN_DIR) $<

$(EBIN_DIR)/%.beam: $(TEST_SOURCE_DIR)/%.erl | $(DEPS_FILE) $(EBIN_DIR)
	erlc $(ERLC_OPTS) -pa $(EBIN_DIR) $<

$(EBIN_DIR):
	mkdir $@

clean:
	rm -f $(EBIN_DIR)/*.beam
	rm -f $(DEPS_FILE)

run: all
	erl -pa $(EBIN_DIR)

# Note that all targets which depend on clean must have clean in their
# name.  Also any target that doesn't depend on clean should not have
# clean in its name, unless you know that you don't need any of the
# automatic dependency generation for that target (eg cleandb).

# We want to load the dep file if *any* target *doesn't* contain
# "clean" - i.e. if removing all clean-like targets leaves something

ifeq "$(MAKECMDGOALS)" ""
TESTABLEGOALS:=$(.DEFAULT_GOAL)
else
TESTABLEGOALS:=$(MAKECMDGOALS)
endif

ifneq "$(strip $(patsubst clean%,,$(patsubst %clean,,$(TESTABLEGOALS))))" ""
-include $(DEPS_FILE)
endif
