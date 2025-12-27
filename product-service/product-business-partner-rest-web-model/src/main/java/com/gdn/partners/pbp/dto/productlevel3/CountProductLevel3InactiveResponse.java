package com.gdn.partners.pbp.dto.productlevel3;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.commons.constant.ProductLevel3InactiveSummaryCriteria;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CountProductLevel3InactiveResponse extends BaseResponse {

  private static final long serialVersionUID = 4529665961797962892L;
  private Map<ProductLevel3InactiveSummaryCriteria, Long> totalItemsByCriterias = new HashMap<>();
  private Long totalItems;
}
