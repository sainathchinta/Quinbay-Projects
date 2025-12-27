package com.gdn.partners.pcu.internal.client.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Map;
import java.util.List;
import java.util.Set;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserFilter implements Serializable {

  private String categoryCode;
  private String email;
  private Map<String, List<String>> exclusions;
  private String keyword;
  private String mode;
  private String name;
  private String phoneNumber;
  private List<String> pickupPointCodes;
  private String roleCode;
  private String salesLevel;
  private String sortDirection;
  private String sortedBy;
  private String state;
  private String storeCode;
  private String username;
  private String vendorCode;
  private Set<String> roleCodes;

}
