################################################################################
#
# lapee-init — LapEE PID 1
#
################################################################################

LAPEE_INIT_VERSION = 1.0
LAPEE_INIT_SITE = $(BR2_EXTERNAL_LAPEE_PATH)/package/lapee-init
LAPEE_INIT_SITE_METHOD = local
LAPEE_INIT_LICENSE = Apache-2.0

define LAPEE_INIT_BUILD_CMDS
	$(TARGET_CC) $(TARGET_CFLAGS) -O2 -static -Wall -Wextra \
		$(@D)/lapee-init.c -o $(@D)/lapee-init
endef

define LAPEE_INIT_INSTALL_TARGET_CMDS
	$(INSTALL) -D -m 0755 $(@D)/lapee-init $(TARGET_DIR)/sbin/init
endef

$(eval $(generic-package))
