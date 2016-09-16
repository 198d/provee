BUILD_DIR=build
OUT_DIR=$(BUILD_DIR)/out
DIST_DIR=$(BUILD_DIR)/dist


SRCS=$(wildcard *.rkt)
EXES=$(wildcard $(OUT_DIR)/*)


PROGS=$(patsubst %.rkt,$(OUT_DIR)/%,$(SRCS))


.PHONY: clean clean_dist


dist: clean_dist $(BUILD_DIR) $(OUT_DIR) $(DIST_DIR) $(PROGS)
	raco dist $(DIST_DIR) $(PROGS)


clean:
	rm -rf $(BUILD_DIR)


clean_dist:
	rm -rf $(DIST_DIR)


$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)


$(OUT_DIR):
	mkdir -p $(OUT_DIR)


$(DIST_DIR):
	mkdir -p $(DIST_DIR)


$(OUT_DIR)/%: %.rkt
	raco exe -o $@ $<
