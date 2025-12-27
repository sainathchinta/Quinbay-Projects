package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class SignedUrlResponse extends BaseResponse {
  private static final long serialVersionUID = 7391215382969622954L;
  private String signedUrl;
  private String uploadedPath;
  private String bulkProcessCode;
  private long expiresAt;
}
