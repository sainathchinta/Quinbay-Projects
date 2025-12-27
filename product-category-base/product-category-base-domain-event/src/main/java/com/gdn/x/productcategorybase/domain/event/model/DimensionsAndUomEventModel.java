package com.gdn.x.productcategorybase.domain.event.model;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class DimensionsAndUomEventModel {

  private String uomCode;
  private String uomName;
  private String uomType;
  private double length;
  private double width;
  private double height;
  private double weight;
  private double conversion;
  private List<String> upcEan = new ArrayList<>();
}
