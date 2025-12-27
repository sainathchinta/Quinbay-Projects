package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 27/11/2018 AD.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class CategoryDetailRequest extends BaseDTORequest {

  private static final long serialVersionUID = -7982731876642902915L;
  private String catalogId;
  private CategoryMappingsUpdateRequest categoryMappingsDetail;
  private CategoryInfoUpdateRequest categoryInfoDetail;
}
