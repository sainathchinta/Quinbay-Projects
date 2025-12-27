package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class StoreCopyUploadTemplateResponse extends BulkDataResponse implements Serializable {

  private List<PickupPointModel> pickupPoints;
  private ShippingTypeEligibility shippingTypeEligibility;
}
