package com.gdn.mta.bulk.models.download;

import java.io.Serial;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
public class ProductUPCDownloadRequest extends BulkDownloadRequest {

  @Serial
  private static final long serialVersionUID = 24623764L;
  private Integer productSize;
  private ProductSummaryRequest productSummaryRequest;
}
