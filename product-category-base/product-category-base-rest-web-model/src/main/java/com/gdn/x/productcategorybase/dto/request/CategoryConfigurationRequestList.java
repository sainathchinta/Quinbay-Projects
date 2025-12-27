package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryConfigurationRequestList implements Serializable {

  private static final long serialVersionUID = 1821360454483870361L;
  private List<CategoryConfigurationRequest> categoryConfigurationRequestList = new ArrayList<>();
}
