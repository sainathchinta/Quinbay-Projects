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
public class CategoryHierarchyServiceRequest {

  @NotBlank(message = "categoryName should not be null")
  private String categoryName;
  @NotBlank(message = "catalogId should not be null")
  private String catalogId;
  private String filterType;
  private String documentFilterType;
}
