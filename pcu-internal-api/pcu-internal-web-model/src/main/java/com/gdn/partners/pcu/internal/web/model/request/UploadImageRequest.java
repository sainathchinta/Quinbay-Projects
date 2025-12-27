package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UploadImageRequest {

  private String imageFileName;
  private String productCode;
  private byte[] bytes;
  private boolean active;
  private boolean retryRequest;
  private String originalFileType;
}