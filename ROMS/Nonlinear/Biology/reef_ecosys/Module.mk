local_sub  := ROMS/Nonlinear/Biology/reef_ecosys

local_lib  := libNLM_mymod.a
local_src  := $(wildcard $(local_sub)/*.F)

$(eval $(call make-library,$(local_lib),$(local_src)))

$(eval $(compile-rules))
