package com.gdn.partners.pcu.external.service.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UploadAttributeImageRequest {
  private String imageFileName;
  private byte[] bytes;
  private String originalFileType;
}