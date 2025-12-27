package com.gdn.mta.bulk.dto;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointDeleteProcessDTO implements Serializable {

  private static final long serialVersionUID = -8047670951433899933L;
  private String businessPartnerCode;
  private String pickupPointCode;
  private String storeId;
  private String createdBy;
  private String updatedBy;
}
