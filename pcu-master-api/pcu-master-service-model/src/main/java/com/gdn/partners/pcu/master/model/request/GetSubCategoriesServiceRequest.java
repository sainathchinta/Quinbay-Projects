package com.gdn.partners.pcu.master.model.request;

import org.hibernate.validator.constraints.NotBlank;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class GetSubCategoriesServiceRequest {

  @NotBlank(message = "catalogType should not be blank")
  private String catalogType;
  @NotBlank(message = "catalogId should not be blank")
  private String catalogId;
  private String parentId;
  private Boolean hideNonInventory;
  private String filterType;
  private String documentFilterType;
  private boolean ignoreB2bExclusive;
  private boolean filterHalalCategory;
}