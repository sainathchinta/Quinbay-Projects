package com.gdn.mta.bulk.models.download.responsedata;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkDownloadProductBasicInfoResponse extends BaseResponse {

  private List<ProductBasicInfoResponse> productBasicInfoResponseList = new ArrayList<>();
  private Map<String, String> exceptionMap = new HashMap();
}
