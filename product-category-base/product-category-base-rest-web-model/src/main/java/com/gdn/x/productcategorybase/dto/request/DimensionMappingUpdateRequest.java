package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class DimensionMappingUpdateRequest implements Serializable {
  private static final long serialVersionUID = 4227543461823860311L;

  private List<DimensionMappingRequest> addedDimensionMapping = new ArrayList<>();
  private List<DimensionMappingRequest> updateDimensionMapping = new ArrayList<>();
  private List<DimensionMappingRequest> deletedDimensionMapping = new ArrayList<>();
}
