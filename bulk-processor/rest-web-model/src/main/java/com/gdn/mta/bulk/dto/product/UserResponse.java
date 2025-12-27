package com.gdn.mta.bulk.dto.product;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
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
  private Set<String> roleCodes = new HashSet<>();
  private Set<String> storeCodes = new HashSet<>();
  private Set<String> vendorCodes = new HashSet<>();
  private Set<String> categoryCodes = new HashSet<>();
}