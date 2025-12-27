package com.gdn.mta.bulk.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ShippingTypeEligibility {
  boolean isEligibleForBigProduct = true;
  boolean isEligibleForBopisProduct = true;
}
