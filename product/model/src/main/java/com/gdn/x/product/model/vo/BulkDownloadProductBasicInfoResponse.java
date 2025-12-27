package com.gdn.x.product.model.vo;

import java.io.Serializable;
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
public class BulkDownloadProductBasicInfoResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = -2580000059508817443L;

  private List<ProductBasicInfoResponse> productBasicInfoResponseList = new ArrayList<>();
  private Map<String, String> exceptionMap = new HashMap<>();
}
