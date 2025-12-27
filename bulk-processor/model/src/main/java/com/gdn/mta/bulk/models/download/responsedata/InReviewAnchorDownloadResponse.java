package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@Data
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class InReviewAnchorDownloadResponse extends BaseResponse {
  private static final long serialVersionUID = 4400335666382829276L;
  private String firstAnchorSku;
  private String secondAnchorSku;
  private String firstAnchorSkuName;
  private String secondAnchorSkuName;
}
