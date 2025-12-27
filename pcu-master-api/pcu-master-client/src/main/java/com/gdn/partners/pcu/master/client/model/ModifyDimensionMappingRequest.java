package com.gdn.partners.pcu.master.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ModifyDimensionMappingRequest {
  private List<DimensionMappingDTO> addedDimensionMapping = new ArrayList<>();
  private List<DimensionMappingDTO> updateDimensionMapping = new ArrayList<>();
  private List<DimensionMappingDTO> deletedDimensionMapping = new ArrayList<>();
}
