package com.gdn.partners.pcu.internal.client.model.response;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.partners.core.web.dto.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserResponse extends BaseResponse {

  private String storeId;
  private String createdBy;
  private Date createdDate;
  private String updatedBy;
  private Date updatedDate;
  private String username;
  private String email;
  private String name;
  private String phoneNumber;
  private String salesLevel;
  private String state;

  @Builder.Default
  private Set<String> roleCodes = new HashSet<>();

  @Builder.Default
  private Set<String> storeCodes = new HashSet<>();

  @Builder.Default
  private Set<String> vendorCodes = new HashSet<>();

  @Builder.Default
  private Set<String> categoryCodes = new HashSet<>();

}
