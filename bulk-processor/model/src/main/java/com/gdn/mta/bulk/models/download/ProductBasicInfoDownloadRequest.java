package com.gdn.mta.bulk.models.download;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
public class ProductBasicInfoDownloadRequest extends BulkDownloadRequest {

  private static final long serialVersionUID = -2692519636385704416L;

  private Map<String, Boolean> privilegedMap = new HashMap<>();
  private ProductSummaryRequest productSummaryRequest;
}
