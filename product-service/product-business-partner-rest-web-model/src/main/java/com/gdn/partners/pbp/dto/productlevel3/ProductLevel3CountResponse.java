package com.gdn.partners.pbp.dto.productlevel3;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.commons.constant.ProductLevel3SummaryCriteria;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3CountResponse extends BaseResponse {

  private Map<ProductLevel3SummaryCriteria, Long> totalItemsByCriterias = new HashMap<>();
  private Long totalItems;
}
