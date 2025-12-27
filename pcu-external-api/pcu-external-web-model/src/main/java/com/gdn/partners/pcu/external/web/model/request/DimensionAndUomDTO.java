package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class DimensionAndUomDTO implements Serializable {

  private String uomCode;
  private String uomType;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double conversion;
  private Set<String> upcEanList = new HashSet<>();
}