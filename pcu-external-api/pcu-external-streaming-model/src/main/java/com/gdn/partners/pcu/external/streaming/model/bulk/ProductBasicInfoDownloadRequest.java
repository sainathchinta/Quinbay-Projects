package com.gdn.partners.pcu.external.streaming.model.bulk;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonTypeName("PRODUCT_BASIC_INFO")
public class ProductBasicInfoDownloadRequest extends BulkDownloadRequest {

  private static final long serialVersionUID = 7569275207293550012L;

  private Map<String, Boolean> privilegedMap = new HashMap<>();
  private ProductSummaryRequest productSummaryRequest;
}
