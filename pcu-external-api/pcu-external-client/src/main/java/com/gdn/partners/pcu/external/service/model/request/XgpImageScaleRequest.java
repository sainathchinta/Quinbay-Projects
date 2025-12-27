package com.gdn.partners.pcu.external.service.model.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class XgpImageScaleRequest {
  private FullImageUploadRequest fullImageUploadRequest;
  private MediumImageUploadRequest mediumImageUploadRequest;
  private ThumbNailImageUploadRequest thumbNailImageUploadRequest;
  private byte[] imageBytes;
}
