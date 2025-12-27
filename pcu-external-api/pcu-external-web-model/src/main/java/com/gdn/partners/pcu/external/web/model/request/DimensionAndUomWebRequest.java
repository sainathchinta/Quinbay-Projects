package com.gdn.partners.pcu.external.web.model.request;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class DimensionAndUomWebRequest {

  private String uomCode;
  private String uomType;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double conversion;
  private Set<String> upcEanList = new HashSet();
}
