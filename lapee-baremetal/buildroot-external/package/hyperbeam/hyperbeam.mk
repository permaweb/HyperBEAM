################################################################################
#
# hyperbeam — HyperBEAM release for LapEE
#
# Builds HyperBEAM from the enclosing worktree and installs a release into
# the target rootfs under /usr/lib/hyperbeam, with a wrapper at /usr/bin/hyperbeam.
#
################################################################################

HYPERBEAM_VERSION = lapee-dev
HYPERBEAM_SITE = $(BR2_EXTERNAL_LAPEE_PATH)/../../..
HYPERBEAM_SITE_METHOD = local
HYPERBEAM_LICENSE = Apache-2.0
HYPERBEAM_DEPENDENCIES = erlang openssl tpm2-tools tpm2-tss

# Build the release using rebar3 in the host toolchain, then stage into target.
define HYPERBEAM_BUILD_CMDS
	cd $(@D) && rebar3 as prod release
endef

define HYPERBEAM_INSTALL_TARGET_CMDS
	mkdir -p $(TARGET_DIR)/usr/lib/hyperbeam
	cp -a $(@D)/_build/prod/rel/hb/. $(TARGET_DIR)/usr/lib/hyperbeam/
	$(INSTALL) -D -m 0755 $(BR2_EXTERNAL_LAPEE_PATH)/package/hyperbeam/hyperbeam.sh \
		$(TARGET_DIR)/usr/bin/hyperbeam
endef

$(eval $(generic-package))
