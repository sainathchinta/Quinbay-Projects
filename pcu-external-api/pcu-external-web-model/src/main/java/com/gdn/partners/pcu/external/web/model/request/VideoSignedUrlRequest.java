package com.gdn.partners.pcu.external.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class VideoSignedUrlRequest {

  private String clientId;
  private String source;
  private String ownerCode;
  private String videoName;
  private boolean compression;
}