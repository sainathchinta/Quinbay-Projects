package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by parvej on 10/10/2019 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ActiveProductWebRequest {
  private String categoryCode;
  private String searchKey;
  private Boolean discoverable;
  private Boolean buyable;
  private int page;
  private int size;
  private String businessPartnerCode;
  private String inventoryFilter;
  private List<String> categoryCodes;
  private List<String> pickupPointCodes;
  private Integer stock;
  private Boolean displayable;
}
