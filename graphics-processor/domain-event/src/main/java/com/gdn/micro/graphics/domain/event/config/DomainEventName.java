package com.gdn.micro.graphics.domain.event.config;

public interface DomainEventName {
  String GRAPHIC_DETAIL_STATUS_EVENT_NAME =
      "com.gdn.micro.graphics.imageresultdetail.status";

  String GRAPHIC_IMAGE_DETAIL_STATUS_EVENT_NAME =
      "com.gdn.micro.graphics.image.process.status";

  String GRAPHIC_IMAGE_DETAIL_STATUS_EVENT_NAME_PRIORITY_1 = "com.gdn.micro.graphics.image.process.status.priority.1";

  String GRAPHIC_IMAGE_DETAIL_STATUS_EVENT_NAME_PRIORITY_2 = "com.gdn.micro.graphics.image.process.status.priority.2";

  String GRAPHIC_RESIZE_IMAGE_STATUS_EVENT = "com.gdn.micro.graphics.image.resize.status";

  String GRAPHIC_RESIZE_IMAGE_STATUS_PRIORITY_1_EVENT = "com.gdn.micro.graphics.image.resize.status.priority.1";

  String GRAPHIC_RESIZE_IMAGE_STATUS_PRIORITY_2_EVENT = "com.gdn.micro.graphics.image.resize.status.priority.2";

  String GRAPHIC_EDITED_RESIZE_IMAGE_STATUS_EVENT = "com.gdn.micro.graphics.edited.image.resize.status";

  String GRAPHIC_EDITED_IMAGE_SCALE_STATUS_EVENT = "com.gdn.micro.graphics.edited.image.scale.status";

  String GRAPHIC_EDITED_REVISED_IMAGE_STATUS_EVENT = "com.gdn.micro.graphics.revised.image.resize.status";

  String GRAPHIC_REVISED_IMAGE_SCALE_STATUS_EVENT = "com.gdn.micro.graphics.revised.image.scale.status";

  String PROCESS_BULK_IMAGE_SCALING = "com.gdn.micro.graphics.process.image.scaling";

  String PROCESS_BULK_IMAGE_SCALING_PRIORITY_1 = "com.gdn.micro.graphics.process.image.scaling.priority.1";

  String PROCESS_BULK_IMAGE_SCALING_PRIORITY_2 = "com.gdn.micro.graphics.process.image.scaling.priority.2";

  String PROCESS_BULK_IMAGE_RESIZE = "com.gdn.micro.graphics.process.image.resize";

  String PROCESS_BULK_IMAGE_EDIT_RESIZE = "com.gdn.micro.graphics.process.image.edit.resize";

  String PROCESS_BULK_IMAGE_REVISED_RESIZE = "com.gdn.micro.graphics.process.image.revised.resize";

  String PROCESS_BULK_IMAGE_EDIT_SCALING= "com.gdn.micro.graphics.process.image.edit.scaling";

  String PROCESS_BULK_IMAGE_RESIZE_PRIORITY_1_EVENT = "com.gdn.micro.graphics.process.image.resize.priority.1";

  String PROCESS_BULK_IMAGE_RESIZE_PRIORITY_2_EVENT = "com.gdn.micro.graphics.process.image.resize.priority.2";

  String PROCESS_IMAGE_SCALING_AND_UPLOAD = "com.gdn.micro.graphics.process.image.scaling.and.upload";
}
