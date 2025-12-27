package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class StoreCopyProductResponse extends BulkDataResponse implements Serializable {

  private List<ProductLevel3SummaryResponse> productLevel3SummaryResponses;
}
