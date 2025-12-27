package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties
@Data
@AllArgsConstructor
@NoArgsConstructor
public class BulkCampaignProductResponse extends BulkDataResponse implements Serializable {

  private static final long serialVersionUID = -5272702825105564072L;
  List<ProductLevel3SummaryResponse> productLevel3SummaryResponses;
  private String promoType;
  private String recommendedWeek;

  public BulkCampaignProductResponse(List<ProductLevel3SummaryResponse> productLevel3SummaryResponses) {
    this.productLevel3SummaryResponses = productLevel3SummaryResponses;
  }
}
