MODULES_DOCUMENTATION = $(TOPDIR)/scheme/documentation/combine-markdown.scm
MODULES_DOCUMENTATION += $(TOPDIR)/scheme/documentation/module.scm
MODULES_DOCUMENTATION += $(TOPDIR)/scheme/documentation/module/constants.scm
MODULES_DOCUMENTATION += $(TOPDIR)/scheme/documentation/module/generic.scm
MODULES_DOCUMENTATION += $(TOPDIR)/scheme/documentation/more.scm
MODULES_DOCUMENTATION += $(TOPDIR)/scheme/documentation/render-markdown.scm

MODULES_GENIPC = $(TOPDIR)/scheme/genipc/stage-0.scm
MODULES_GENIPC += $(TOPDIR)/scheme/genipc/stage-1.scm
MODULES_GENIPC += $(TOPDIR)/scheme/genipc/stage-2.scm
MODULES_GENIPC += $(TOPDIR)/scheme/genipc/stage-3.scm
MODULES_GENIPC += $(TOPDIR)/scheme/genipc/utilities.scm

MODULES_TEST = $(TOPDIR)/scheme/test/payload.scm
MODULES_TEST += $(TOPDIR)/scheme/test/setup.scm

MODULES_GENERATED_CONSTANTS = $(TOPDIR)/scheme/xmms2/constants/binary-data.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/collection.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/collection-sync.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/configuration.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/courier.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/ipc-manager.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/main.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/media-info-reader.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/media-library.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/meta.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/playback.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/playlist.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/visualization.scm
MODULES_GENERATED_CONSTANTS += $(TOPDIR)/scheme/xmms2/constants/xform.scm

MODULES_GENERATED_IPC = $(TOPDIR)/scheme/xmms2/ipc/binary-data.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/collection.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/collection-sync.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/configuration.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/courier.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/ipc-manager.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/main.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/media-info-reader.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/media-library.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/playback.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/playlist.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/visualization.scm
MODULES_GENERATED_IPC += $(TOPDIR)/scheme/xmms2/ipc/xform.scm

MODULES_CORE = $(TOPDIR)/scheme/xmms2/constants.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/client.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/client/synchronous.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/data-conversion.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/enumeration.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/fetch-spec.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/header.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/io.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/ipc.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/jump-table.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/payload.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/types.scm
MODULES_CORE += $(TOPDIR)/scheme/xmms2/values.scm

MODULES = $(MODULES_CORE) $(MODULES_GENERATED_CONSTANTS)
MODULES += $(MODULES_GENERATED_IPC) $(MODULES_DOCUMENTATION)
MODULES += $(MODULES_GENIPC) $(MODULES_TEST)
