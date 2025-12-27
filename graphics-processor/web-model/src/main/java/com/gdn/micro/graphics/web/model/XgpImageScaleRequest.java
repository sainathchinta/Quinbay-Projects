package com.gdn.micro.graphics.web.model;

import lombok.Data;

@Data
public class XgpImageScaleRequest {
  private FullImageUploadRequest fullImageUploadRequest;
  private MediumImageUploadRequest mediumImageUploadRequest;
  private ThumbNailImageUploadRequest thumbNailImageUploadRequest;
  private boolean active;
  private byte[] imageBytes;
}
