package com.gdn.partners.pcu.external.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class VideoSignedUrlResponse {
  private String videoSignedUrl;
  private String videoId;
  private String videoUploadUrl;
  private String imageSignedUrl;
  private String imageUploadUrl;
  private long expiresAt;
}
