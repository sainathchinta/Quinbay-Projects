package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by keshashah on 25/10/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class BulkDataResponse {

  private BulkProcessEntity bulkProcessEntity;
  private String businessPartnerCode;
  private ShippingTypeEligibility shippingTypeEligibility;
  private boolean partialDownload;
}
